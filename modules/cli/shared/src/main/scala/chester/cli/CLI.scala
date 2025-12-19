package chester.cli

import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.error.*
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.backend.TypeScriptBackend
import chester.tyck.{CoreTypeChecker, ElabConstraint, ElabContext, ElabHandlerConf, ElabProblem, substituteSolutions}
import chester.error.VectorReporter
import chester.interop.typescript.TypeScriptToChester
import chester.interop.golang.GoToChester
import chester.syntax.TypeScriptDeclParser
import chester.tyck.{JSImportSignature, JSImportSignature as JSImportSupport, GoImportSignature}
import chester.transform.EffectCPS
import chester.utils.doc.DocConf
import chester.utils.elab.ProceduralSolverModule
import chester.utils.io.*
import chester.utils.term.*
import chester.utils.term.InputStatus.Complete
import chester.utils.term.{EndOfFile, LineRead, ReadLineResult, StatusError, UserInterrupted}

class CLI[F[_]](using runner: Runner[F], terminal: Terminal[F], io: IO[F]) {
  private given DocConf = DocConf.Default
  private val TypeCommand = "^:(t|type)(?:\\s+(.*))?$".r
  private val LoadCommand = "^:(l|load)(?:\\s+(.*))?$".r

  private case class ReplState(loaded: Vector[String]) {
    def withLoaded(code: String): ReplState = copy(loaded = loaded :+ code)
    def combineInput(input: String): String = {
      if loaded.isEmpty then input
      else s"{\n${(loaded :+ input).mkString("\n")}\n}"
    }
  }

  private val EmptyState = ReplState(Vector.empty)

  private object ReplInfo extends TerminalInfo {
    override def checkInputStatus(input: String): InputStatus = Complete
    override val defaultPrompt: fansi.Str = fansi.Str("chester> ")
    override val continuationPrompt: fansi.Str = fansi.Str("... ")
  }

  private def renderProblems(problems: Seq[Problem], source: Source): Seq[String] = {
    val reader = source.readContent.toOption
      .map(content => SourceReader.fromFileContent(FileContent(content, source.offset)))
      .getOrElse(SourceReader.empty)
    problems.map(p => p.renderDoc(using summon[DocConf], reader).toString)
  }

  private def analyze(
      source: Source,
      jsImports: Map[String, JSImportSignature] = Map.empty,
      goImports: Map[String, GoImportSignature] = Map.empty
  ): Either[Seq[String], (AST, Option[AST])] = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val parsed = for {
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    } yield Parser.parseFile(tokens)

    val parseProblems = parseReporter.getReports
    if (parseProblems.nonEmpty) {
      Left(renderProblems(parseProblems, source))
    } else {
      parsed match {
        case Left(err) =>
          Left(renderProblems(Seq(err), source))
        case Right(cst) =>
          val module = ProceduralSolverModule
          val solver = module.makeSolver[ElabConstraint](new ElabHandlerConf(module))
          val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
          val tyCell = module.newOnceCell[ElabConstraint, AST](solver)
          // Merge Go imports into jsImports for now (both use similar Param-based signatures)
          val allImports = jsImports ++ goImports.map { case (k, v) => (k, JSImportSignature(v.fields)) }
          val ctx = ElabContext(bindings = Map.empty, types = Map.empty, jsImports = allImports, reporter = elabReporter)

          module.addConstraint(solver, ElabConstraint.InferTopLevel(cst, resultCell, tyCell, ctx))
          module.run(solver)

          val elabProblems = elabReporter.getReports
          if (elabProblems.nonEmpty) {
            Left(renderProblems(elabProblems, source))
          } else {
            val maybeAst = module.readStable(solver, resultCell).map(ast => substituteSolutions(ast)(using module, solver))
            val maybeTy = module.readStable(solver, tyCell).map(t => substituteSolutions(t)(using module, solver))
            maybeAst match {
              case Some(ast) => Right((ast, maybeTy))
              case None      => Left(Seq("Elaboration did not produce a result"))
            }
          }
      }
    }
  }

  private def printLines(lines: Seq[String], toStderr: Boolean): F[Unit] =
    lines.foldLeft(Runner.pure[F, Unit](()))((acc, line) => acc.flatMap(_ => IO.println(line, toStderr)))

  private def formatAst(ast: AST, ty: Option[AST]): String = {
    val astDoc = ast.toDoc.toString
    ty match {
      case Some(tpe) =>
        val tyDoc = tpe.toDoc.toString
        s"Type: $tyDoc\nAST:\n$astDoc"
      case None =>
        s"AST:\n$astDoc"
    }
  }

  private def showAnalysis(result: Either[Seq[String], (AST, Option[AST])], output: Option[String]): F[Unit] = {
    result match {
      case Left(errors) =>
        printLines(errors, toStderr = true)
      case Right((ast, ty)) =>
        given coreReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()
        CoreTypeChecker.typeCheck(ast)
        val coreProblems = coreReporter.getReports
        val coreOk = coreProblems.isEmpty
        val coreCheckAction = {
          if coreOk then Runner.pure[F, Unit](())
          else {
            val rendered = renderProblems(coreProblems, source = Source(FileNameAndContent("<core>", "")))
            IO.println("Core type checker failed; skipping evaluation.", toStderr = true)
              .flatMap(_ => printLines(rendered, toStderr = true))
          }
        }

        coreCheckAction.flatMap { _ =>
          if !coreOk && output.isEmpty then Runner.pure[F, Unit](())
          else {
            output match
              case Some(pathStr) =>
                val rendered = formatAst(ast, ty)
                val path = io.pathOps.of(pathStr)
                IO.writeString(path, rendered, writeMode = WriteMode.Overwrite)
                  .flatMap(_ => IO.println(s"Wrote elaborated AST to '${io.pathOps.asString(path)}'."))
              case None =>
                val evalAst = ty match
                  case Some(tpe) =>
                    val (cpsAst, _) = EffectCPS.transformExpr(ast, tpe, EffectCPS.Config(transformIO = true))
                    cpsAst
                  case None => ast
                Evaluator.eval(evalAst).flatMap(value => IO.println(s"=> ${Evaluator.valueToString(value)}"))
          }
        }
    }
  }

  private def showType(expr: String, state: ReplState): F[Unit] = {
    val combined = state.combineInput(expr)
    val source = Source(FileNameAndContent("<stdin>", combined))
    resolveJSImportSignatures(source, combined).flatMap { jsImports =>
      analyze(source, jsImports = jsImports) match
        case Left(errors) =>
          printLines(errors, toStderr = true)
        case Right((ast, maybeTy)) =>
          given coreReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()
          CoreTypeChecker.typeCheck(ast)
          val coreProblems = coreReporter.getReports
          if coreProblems.nonEmpty then
            val rendered = renderProblems(coreProblems, source = Source(FileNameAndContent("<core>", "")))
            IO.println("Core type checker failed; skipping type display.", toStderr = true)
              .flatMap(_ => printLines(rendered, toStderr = true))
          else {
            maybeTy match
              case Some(tpe) =>
                IO.println(s"Type: ${tpe.toDoc.toString}")
              case None =>
                IO.println("No type information available.", toStderr = true)
          }
    }
  }

  private def runRepl(): F[Unit] = {
    Terminal.runTerminal(TerminalInit.Default) {
      def loop(state: ReplState)(using term: InTerminal[F]): F[Unit] = {
        InTerminal.readline(ReplInfo).flatMap {
          case LineRead(line) =>
            val trimmed = line.trim
            trimmed match
              case ":quit" | ":q" =>
                IO.println("Goodbye.")
              case TypeCommand(_, expr) =>
                val exprStr = Option(expr).map(_.trim).getOrElse("")
                val action = {
                  if exprStr.isEmpty then IO.println("Usage: :t <expression>", toStderr = true)
                  else showType(exprStr, state)
                }
                action.flatMap(_ => loop(state))
              case LoadCommand(_, pathStr) =>
                val fileStr = Option(pathStr).map(_.trim).getOrElse("")
                val action: F[ReplState] = {
                  if fileStr.isEmpty then IO.println("Usage: :l <file>", toStderr = true).map(_ => state)
                  else {
                    val path = io.pathOps.of(fileStr)
                    IO.exists(path).flatMap { exists =>
                      if !exists then IO.println(s"Input file '$fileStr' does not exist.", toStderr = true).map(_ => state)
                      else {
                        IO.readString(path).flatMap { content =>
                          val source = Source(FileNameAndContent(fileStr, content))
                          resolveJSImportSignatures(source, content).flatMap { jsImports =>
                            analyze(source, jsImports = jsImports) match
                              case Left(errors) =>
                                printLines(errors, toStderr = true).map(_ => state)
                              case Right((ast, _)) =>
                                given coreReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()
                                CoreTypeChecker.typeCheck(ast)
                                val coreProblems = coreReporter.getReports
                                if coreProblems.nonEmpty then
                                  val rendered = renderProblems(coreProblems, source = Source(FileNameAndContent("<core>", "")))
                                  IO.println("Core type checker failed; not loading file.", toStderr = true)
                                    .flatMap(_ => printLines(rendered, toStderr = true))
                                    .map(_ => state)
                                else IO.println(s"Loaded $fileStr.").map(_ => state.withLoaded(content))
                          }
                        }
                      }
                    }
                  }
                }
                action.flatMap(loop)
              case _ =>
                val combined = state.combineInput(line)
                val src = Source(FileNameAndContent("<stdin>", combined))
                resolveJSImportSignatures(src, combined)
                  .flatMap(jsImports => showAnalysis(analyze(src, jsImports = jsImports), None))
                  .flatMap(_ => loop(state))
          case StatusError(message) =>
            IO.println(s"Input error: $message", toStderr = true).flatMap(_ => loop(state))
          case UserInterrupted =>
            IO.println("Interrupted.")
          case EndOfFile =>
            IO.println("Goodbye.")
        }
      }

      loop(EmptyState)
    }
  }

  private def runFile(pathStr: String): F[Unit] = {
    val path = io.pathOps.of(pathStr)
    for {
      exists <- IO.exists(path)
      _ <-
        if !exists then IO.println(s"Input file '$pathStr' does not exist.", toStderr = true)
        else {
          for {
            content <- IO.readString(path)
            src = Source(FileNameAndContent(pathStr, content))
            jsImports <- resolveJSImportSignatures(src, content)
            _ <- showAnalysis(analyze(src, jsImports = jsImports), None)
          } yield ()
        }
    } yield ()
  }

  private def compileFile(input: String, output: Option[String]): F[Unit] = {
    val path = io.pathOps.of(input)
    for {
      exists <- IO.exists(path)
      _ <-
        if !exists then IO.println(s"Input file '$input' does not exist.", toStderr = true)
        else {
          for {
            content <- IO.readString(path)
            src = Source(FileNameAndContent(input, content))
            jsImports <- resolveJSImportSignatures(src, content)
            _ <- showAnalysis(analyze(src, jsImports = jsImports), output)
          } yield ()
        }
    } yield ()
  }

  private def formatFile(input: String): F[Unit] = {
    val path = io.pathOps.of(input)
    for {
      exists <- IO.exists(path)
      _ <-
        if !exists then IO.println(s"Input file '$input' does not exist.", toStderr = true)
        else {
          IO.readString(path).flatMap { content =>
            given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
            val source = Source(FileNameAndContent(input, content))
            val parsed = for {
              chars <- CharReader.read(source)
              tokens <- Tokenizer.tokenize(chars)
            } yield Parser.parseFile(tokens, preserveComments = true)

            val parseProblems = parseReporter.getReports
            if parseProblems.nonEmpty then printLines(renderProblems(parseProblems, source), toStderr = true)
            else {
              parsed match
                case Left(err) =>
                  printLines(renderProblems(Seq(err), source), toStderr = true)
                case Right(cst) =>
                  val rendered = cst.toDoc.toString
                  IO.writeString(path, rendered, writeMode = WriteMode.Overwrite)
                    .flatMap(_ => IO.println(s"Wrote formatted code to '${io.pathOps.asString(path)}'."))
            }
          }
        }
    } yield ()
  }

  private def ensureDir(pathStr: String): F[io.Path] = {
    val p = io.pathOps.of(pathStr)
    for {
      isDir <- IO.isDirectory(p)
      _ <- if !isDir then IO.createDirRecursiveIfNotExists(p) else Runner.pure(())
    } yield p
  }

  private def targetTsPath(inputPath: io.Path, outDir: io.Path): io.Path = {
    val name = io.pathOps.baseName(inputPath)
    val tsName =
      if name.contains(".") then name.replaceAll("\\.[^.]+$", ".ts") else s"$name.ts"
    io.pathOps.join(outDir, tsName)
  }

  private def compileToTypeScript(input: String, output: Option[String]): F[Unit] = {
    val inPath = io.pathOps.of(input)
    for {
      exists <- IO.exists(inPath)
      _ <-
        if !exists then IO.println(s"Input path '$input' does not exist.", toStderr = true)
        else {
          IO.isDirectory(inPath).flatMap { isDir =>
            val defaultOut = {
              if isDir then io.pathOps.join(inPath, "ts-out")
              else {
                val inStr = io.pathOps.asString(inPath)
                val idx = inStr.lastIndexOf('/')
                val tsName = if inStr.contains(".") then inStr.replaceAll("\\.[^.]+$", ".ts") else s"$inStr.ts"
                if idx >= 0 then io.pathOps.of(inStr.take(idx + 1) + tsName.split('/').last) else io.pathOps.of(tsName)
              }
            }
            val outBaseStr = output.getOrElse(io.pathOps.asString(defaultOut))
            ensureDir(outBaseStr).flatMap { outDir =>
              val inputsF: F[Seq[io.Path]] = {
                if isDir then IO.listFiles(inPath).map(_.filter(p => io.pathOps.baseName(p).endsWith(".chester")))
                else Runner.pure(Seq(inPath))
              }

              inputsF.flatMap { paths =>
                paths.foldLeft(Runner.pure[F, Unit](())) { (acc, p) =>
                  acc.flatMap { _ =>
                    IO.readString(p).flatMap { content =>
                      val src = Source(FileNameAndContent(io.pathOps.asString(p), content))
                      resolveJSImportSignatures(src, content).flatMap { jsImports =>
                        val result = analyze(src, jsImports = jsImports)
                        result match
                          case Left(errs) =>
                            printLines(errs, toStderr = true)
                          case Right((ast, _)) =>
                            val tsProg = TypeScriptBackend.lowerProgram(ast)
                            val rendered = tsProg.toDoc.toString
                            val outPath = targetTsPath(p, outDir)
                            IO.writeString(outPath, rendered, writeMode = WriteMode.Overwrite)
                              .flatMap(_ => IO.println(s"Wrote TypeScript to '${io.pathOps.asString(outPath)}'."))
                      }
                    }
                  }
                }
              }
            }
          }
        }
    } yield ()
  }

  private def resolveJSImportSignatures(source: Source, content: String): F[Map[String, JSImportSignature]] = {
    val specifiers = extractJSImportSpecifiers(source).getOrElse(Set.empty)
    specifiers.foldLeft(Runner.pure[F, Map[String, JSImportSignature]](Map.empty)) { (accF, spec) =>
      accF.flatMap { acc =>
        resolveJSImportSignature(spec).map {
          case Some(sig) => acc + (JSImportSupport.normalizeModuleSpecifier(spec) -> sig)
          case None      => acc
        }
      }
    }
  }

  private def extractJSImportSpecifiers(source: Source): Option[Set[String]] = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val parsed = for
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    yield Parser.parseFile(tokens)
    parsed.toOption.map { cst =>
      def walk(node: chester.core.CST): Vector[String] = node match
        case chester.core.CST.Block(elements, tail, _) =>
          elements.flatMap(walk) ++ tail.toVector.flatMap(walk)
        case chester.core.CST.SeqOf(elements, _) =>
          val elems = elements.toVector
          val here =
            if elems.headOption.exists { case chester.core.CST.Symbol("import", _) => true; case _ => false }
            then
              elems.collectFirst { case chester.core.CST.StringLiteral(mod, _) => mod }.toVector
            else Vector.empty
          here ++ elems.flatMap(walk)
        case chester.core.CST.Tuple(elements, _) =>
          elements.flatMap(walk)
        case chester.core.CST.ListLiteral(elements, _) =>
          elements.flatMap(walk)
        case _ => Vector.empty
      walk(cst).toSet
    }
  }

  private def resolveJSImportSignature(moduleSpec: String): F[Option[JSImportSignature]] = {
    resolveDtsForModule(moduleSpec, allowTypesFallback = true).map { maybeDts =>
      maybeDts.map { dts =>
        val dtsSource = Source(FileNameAndContent(s"$moduleSpec.d.ts", dts))
        val tsAst = TypeScriptDeclParser.parse(dts, dtsSource)
        TypeScriptToChester.moduleSignature(tsAst, moduleSpec)
      }
    }
  }

  private def resolveDtsForModule(moduleSpec: String, allowTypesFallback: Boolean): F[Option[String]] = {
    val normalized = JSImportSupport.normalizeModuleSpecifier(moduleSpec)
    resolveNodeBuiltinDts(normalized).flatMap {
      case some @ Some(_) => Runner.pure(some)
      case None           => resolveNpmPackageDts(normalized, allowTypesFallback)
    }
  }

  private def resolveNodeBuiltinDts(normalizedModule: String): F[Option[String]] = {
    findTypesNodeBase().flatMap {
      case None => Runner.pure(None)
      case Some(base) =>
        val parts = normalizedModule.split('/').toVector.filter(_.nonEmpty)
        val (dirParts, fileName) =
          if parts.isEmpty then (Vector.empty[String], "index.d.ts") else (parts.dropRight(1), parts.last + ".d.ts")
        val dir = dirParts.foldLeft(base)((p, seg) => io.pathOps.join(p, seg))
        val fullPath = io.pathOps.join(dir, fileName)
        IO.exists(fullPath).flatMap(ok => if ok then IO.readString(fullPath).map(Some(_)) else Runner.pure(None))
    }
  }

  private def findTypesNodeBase(): F[Option[io.Path]] = {
    for
      wd <- IO.workingDir
      direct = io.pathOps.join(io.pathOps.join(io.pathOps.join(wd, "node_modules"), "@types"), "node")
      directOk <- IO.exists(direct)
      result <-
        if directOk then Runner.pure(Some(direct))
        else {
          val pnpmDir = io.pathOps.join(io.pathOps.join(wd, "node_modules"), ".pnpm")
          IO.exists(pnpmDir).flatMap { ok =>
            if !ok then Runner.pure(None)
            else {
              IO.listFiles(pnpmDir).flatMap { entries =>
                val candidates = entries.filter(p => io.pathOps.baseName(p).startsWith("@types+node@"))
                def firstOk(xs: Seq[io.Path]): F[Option[io.Path]] =
                  xs.headOption match
                    case None => Runner.pure(None)
                    case Some(p) =>
                      val base = io.pathOps.join(io.pathOps.join(io.pathOps.join(p, "node_modules"), "@types"), "node")
                      IO.exists(base).flatMap(ex => if ex then Runner.pure(Some(base)) else firstOk(xs.tail))
                firstOk(candidates)
              }
            }
          }
        }
    yield result
  }

  private def resolveNpmPackageDts(normalizedModule: String, allowTypesFallback: Boolean): F[Option[String]] = {
    val (pkgName, subpath) = splitNpmPackage(normalizedModule)
    fetchAndReadPackageTypes(pkgName, subpath).flatMap {
      case some @ Some(_) => Runner.pure(some)
      case None =>
        if !allowTypesFallback || pkgName.startsWith("@types/") then Runner.pure(None)
        else fetchAndReadPackageTypes(toDefinitelyTypedPackage(pkgName), subpath)
    }
  }

  private def fetchAndReadPackageTypes(pkgName: String, subpath: String): F[Option[String]] = {
    val encoded = encodeNpmRegistryPath(pkgName)
    val safeDir = pkgName.replace('/', '+').replace('@', '_')
    for
      wd <- IO.workingDir
      cacheRoot = io.pathOps.join(io.pathOps.join(wd, "js-typings"), "npm-cache")
      _ <- IO.createDirRecursiveIfNotExists(cacheRoot)
      pkgCache = io.pathOps.join(cacheRoot, safeDir)
      _ <- IO.createDirRecursiveIfNotExists(pkgCache)
      metaPath = io.pathOps.join(pkgCache, "metadata.json")
      _ <- IO.downloadToFile(s"https://registry.npmjs.org/$encoded", metaPath)
      metaStr <- IO.readString(metaPath)
      meta = ujson.read(metaStr)
      version = meta("dist-tags")("latest").str
      tarballUrl = meta("versions")(version)("dist")("tarball").str
      tgzPath = io.pathOps.join(pkgCache, s"$version.tgz")
      tgzExists <- IO.exists(tgzPath)
      _ <- if tgzExists then Runner.pure(()) else IO.downloadToFile(tarballUrl, tgzPath)
      extractDir = io.pathOps.join(pkgCache, s"pkg-$version")
      _ <- IO.createDirRecursiveIfNotExists(extractDir)
      packageDir = io.pathOps.join(extractDir, "package")
      packageOk <- IO.exists(packageDir)
      _ <-
        if packageOk then Runner.pure(())
        else IO.call(Seq("tar", "-xzf", io.pathOps.asString(tgzPath), "-C", io.pathOps.asString(extractDir))).map(_ => ())
      packageJsonPath = io.pathOps.join(packageDir, "package.json")
      pkgJsonOk <- IO.exists(packageJsonPath)
      result <-
        if !pkgJsonOk then Runner.pure(None)
        else {
          IO.readString(packageJsonPath).flatMap { pkgJsonStr =>
            val pkgJson = ujson.read(pkgJsonStr)
            val typesField = pkgJson.obj.get("types").orElse(pkgJson.obj.get("typings")).flatMap(_.strOpt)
            val cleaned = typesField.map(_.stripPrefix("./"))
            val baseCandidates = cleaned.toVector ++ Vector("index.d.ts")
            val candidates =
              if subpath.isEmpty then baseCandidates else baseCandidates ++ Vector(subpath + ".d.ts", subpath + "/index.d.ts")

            def firstExisting(paths: Vector[String]): F[Option[String]] =
              paths.headOption match
                case None => Runner.pure(None)
                case Some(rel) =>
                  val full = rel.split('/').foldLeft(packageDir)((p, seg) => io.pathOps.join(p, seg))
                  IO.exists(full).flatMap(ex => if ex then IO.readString(full).map(Some(_)) else firstExisting(paths.tail))

            firstExisting(candidates)
          }
        }
    yield result
  }

  private def splitNpmPackage(moduleSpec: String): (String, String) = {
    val parts = moduleSpec.split('/').toVector.filter(_.nonEmpty)
    if moduleSpec.startsWith("@") && parts.length >= 2 then
      val pkg = parts.take(2).mkString("/")
      val rest = parts.drop(2).mkString("/")
      (pkg, rest)
    else
      val pkg = parts.headOption.getOrElse(moduleSpec)
      val rest = parts.drop(1).mkString("/")
      (pkg, rest)
  }

  private def toDefinitelyTypedPackage(pkgName: String): String = {
    if pkgName.startsWith("@") then
      val parts = pkgName.split('/').toVector.filter(_.nonEmpty)
      if parts.length >= 2 then s"@types/${parts(0).stripPrefix("@")}__${parts(1)}" else s"@types/${pkgName.stripPrefix("@")}"
    else s"@types/$pkgName"
  }

  private def encodeNpmRegistryPath(pkgName: String): String =
    if pkgName.startsWith("@") then pkgName.replace("/", "%2F") else pkgName

  // Go import support (stub for future implementation)
  // To enable: build tools/go-type-extractor and ensure Go toolchain is available
  private def resolveGoImportSignatures(source: Source, content: String): F[Map[String, GoImportSignature]] = {
    // TODO: Uncomment when Go toolchain integration is needed
    // val specifiers = extractGoImportSpecifiers(source).getOrElse(Set.empty)
    // if (specifiers.nonEmpty) {
    //   // Call go-type-extractor utility
    //   ...
    // }
    Runner.pure(Map.empty)
  }

  private def extractGoImportSpecifiers(source: Source): Option[Set[String]] = {
    extractJSImportSpecifiers(source).map(_.filter(_.startsWith("go:")))
  }

  def run(config: Config): F[Unit] = config match {
    case Config.Version =>
      IO.println(s"Chester version: ${VersionInfo.current}")
    case Config.Help =>
      IO.println(Main.usage)
    case Config.Run(None) =>
      runRepl()
    case Config.Run(Some(path)) =>
      runFile(path)
    case Config.Compile(input, output) =>
      compileFile(input, output)
    case Config.CompileTS(input, output) =>
      compileToTypeScript(input, output)
    case Config.Format(input) =>
      formatFile(input)
  }
}

object CLI {
  def run[F[_]](config: Config)(using
      runner: Runner[F],
      terminal: Terminal[F],
      io: IO[F],
      spawn: Spawn[F]
  ): Unit = {
    Spawn.spawn {
      (new CLI[F]).run(config)
    }
  }
}
