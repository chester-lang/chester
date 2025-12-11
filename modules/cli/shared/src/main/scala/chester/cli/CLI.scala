package chester.cli

import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.error.*
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.backend.TypeScriptBackend
import chester.tyck.{CoreTypeChecker, ElabConstraint, ElabContext, ElabHandlerConf, ElabProblem, substituteSolutions}
import chester.error.VectorReporter
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

  private def analyze(source: Source): Either[Seq[String], (AST, Option[AST])] = {
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
          val ctx = ElabContext(bindings = Map.empty, types = Map.empty, reporter = elabReporter)

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

  private def showType(expr: String): F[Unit] = {
    val source = Source(FileNameAndContent("<stdin>", expr))
    analyze(source) match
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
        else
          maybeTy match
            case Some(tpe) =>
              IO.println(s"Type: ${tpe.toDoc.toString}")
            case None =>
              IO.println("No type information available.", toStderr = true)
  }

  private def runRepl(): F[Unit] = {
    Terminal.runTerminal(TerminalInit.Default) {
      def loop(using term: InTerminal[F]): F[Unit] = {
        InTerminal.readline(ReplInfo).flatMap {
          case LineRead(line) =>
            val trimmed = line.trim
            trimmed match
              case ":quit" | ":q" =>
                IO.println("Goodbye.")
              case TypeCommand(_, expr) =>
                val exprStr = Option(expr).map(_.trim).getOrElse("")
                val action =
                  if exprStr.isEmpty then IO.println("Usage: :t <expression>", toStderr = true)
                  else showType(exprStr)
                action.flatMap(_ => loop)
              case _ =>
                showAnalysis(analyze(Source(FileNameAndContent("<stdin>", line))), None)
                  .flatMap(_ => loop)
          case StatusError(message) =>
            IO.println(s"Input error: $message", toStderr = true).flatMap(_ => loop)
          case UserInterrupted =>
            IO.println("Interrupted.")
          case EndOfFile =>
            IO.println("Goodbye.")
        }
      }

      loop
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
            _ <- showAnalysis(analyze(Source(FileNameAndContent(pathStr, content))), None)
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
            _ <- showAnalysis(analyze(Source(FileNameAndContent(input, content))), output)
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
                      val result = analyze(Source(FileNameAndContent(io.pathOps.asString(p), content)))
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
    } yield ()
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
