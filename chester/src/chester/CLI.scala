package chester

import chester.core.{AST, Param, Telescope, BuiltinEffect, EffectRef, Implicitness}
import chester.tyck.{ElabConstraint, ElabContext, ElabHandlerConf, ElabProblem, GoImportSignature, JSImportSignature, CoreTypeChecker, substituteSolutions}
import chester.uniqid.Uniqid
import chester.utils.elab.ProceduralSolverModule
import chester.error.VectorReporter
import chester.backend.GoBackend
import chester.backend.TypeScriptBackend
import chester.syntax.TypeScriptAST
import java.nio.file.{Paths, Files}
import scala.sys.process.*
import chester.utils.doc.{DocConf, render}

object CLI:
  private def fmtSignature: GoImportSignature = {
    val formatParam = Param(Uniqid.make, "format", AST.StringType(None), Implicitness.Explicit, None)
    val argsParam = Param(Uniqid.make, "args", AST.ListType(AST.AnyType(None), None), Implicitness.Explicit, None)
    val printfTy = AST.Pi(
      Vector(Telescope(Vector(formatParam, argsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    val printlnTy = AST.Pi(
      Vector(Telescope(Vector(argsParam), Implicitness.Explicit)),
      AST.TupleType(Vector.empty, None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    GoImportSignature(
      Vector(
        Param(Uniqid.make, "Printf", printfTy, Implicitness.Explicit, None),
        Param(Uniqid.make, "Println", printlnTy, Implicitness.Explicit, None)
      ),
      "fmt"
    )
  }

  private def pathSignature: JSImportSignature = {
    val pathsParam = Param(Uniqid.make, "paths", AST.ListType(AST.StringType(None), None), Implicitness.Explicit, None)
    val joinTy = AST.Pi(
      Vector(Telescope(Vector(pathsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    JSImportSignature(
      Vector(
        Param(Uniqid.make, "join", joinTy, Implicitness.Explicit, None)
      ),
      chester.core.JSImportKind.Default
    )
  }

  def elaborate(content: String): (Option[AST], Option[AST], Vector[ElabProblem]) = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val source = Source(FileNameAndContent("file.chester", content))
    val parsed = for
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    yield {
      val cst = Parser.parseFile(tokens)

      val module = ProceduralSolverModule
      val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))
      val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

      val stdGoImports = Map("fmt" -> fmtSignature)
      val stdJsImports = Map("path" -> pathSignature)
      val allImports = stdGoImports.map { case (k, v) => k -> JSImportSignature(v.fields) } ++ stdJsImports

      val ctx = ElabContext(bindings = Map.empty, types = Map.empty, jsImports = allImports, reporter = elabReporter)

      module.addConstraint(solver, ElabConstraint.InferTopLevel(cst, resultCell, typeCell, ctx))
      module.run(solver)

      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))

      if (elabReporter.getReports.isEmpty && zonkedResult.isDefined) {
        CoreTypeChecker.typeCheck(zonkedResult.get)(using elabReporter)
      }

      (zonkedResult, zonkedTy)
    }

    parsed match
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(err)        => (None, None, Vector(ElabProblem.UnboundVariable(err.toString, None)))
  }

  def main(args: Array[String]): Unit = {
    var filePathStr: String = ""
    var run = false
    var target = "go"
    var i = 0
    while (i < args.length) {
      args(i) match {
        case "--run" => run = true
        case "--target" if i + 1 < args.length =>
          target = args(i + 1)
          i += 1
        case arg if arg.startsWith("-") =>
          // Ignore other options
        case arg =>
          if (filePathStr.isEmpty) {
            filePathStr = arg
          }
      }
      i += 1
    }

    if (filePathStr.isEmpty) {
      println("Usage: mill chester.run [file.chester] [--target go|ts] [--run]")
      return
    }

    val path = Paths.get(filePathStr)
    if (!Files.exists(path)) {
      System.err.println(s"Error: file not found: $filePathStr")
      System.exit(1)
    }

    val content = Files.readString(path)
    println(s"Reading program from $filePathStr...")
    
    // Elaborate surface CST into typed AST with standard signatures preloaded
    val (astOpt, _, errors) = elaborate(content)
    
    if (errors.nonEmpty) {
      System.err.println("Elaboration failed with the following errors:")
      errors.foreach(err => System.err.println(s"  - $err"))
      System.exit(1)
    }

    val ast = astOpt.get

    if (target == "ts") {
      println("Elaboration successful! Compiling to TypeScript...")
      val program = TypeScriptBackend.lowerProgram(ast)

      val updatedStatements = if (program.statements.nonEmpty) {
        program.statements.last match {
          case TypeScriptAST.ExpressionStatement(expr, span) =>
            val isUnit = expr match {
              case TypeScriptAST.UndefinedLiteral(_) => true
              case TypeScriptAST.Array(elems, _) if elems.isEmpty => true
              case TypeScriptAST.Call(TypeScriptAST.PropertyAccess(TypeScriptAST.Identifier("console", _), "log", _), _, _) => true
              case _ => false
            }
            if (isUnit) program.statements
            else {
              val logCall = TypeScriptAST.ExpressionStatement(
                TypeScriptAST.Call(
                  TypeScriptAST.PropertyAccess(TypeScriptAST.Identifier("console", span), "log", span),
                  Vector(expr),
                  span
                ),
                span
              )
              program.statements.init :+ logCall
            }
          case _ => program.statements
        }
      } else {
        program.statements
      }

      val finalProgram = TypeScriptAST.Program(updatedStatements, program.span)
      given DocConf = DocConf.Default
      val tsCode = render(finalProgram.toDoc).toString

      val outPath = Paths.get("out.ts")
      Files.writeString(outPath, tsCode)
      println(s"TypeScript code generated successfully in ${outPath.toAbsolutePath}")

      if (run) {
        println("Running the compiled TypeScript program...")
        val tscCmd = if (Process(Seq("sh", "-lc", "command -v tsc >/dev/null 2>&1")).! == 0) {
          Seq("tsc", "--target", "es2020", "--moduleResolution", "node", "out.ts")
        } else {
          Seq("nix", "shell", "nixpkgs#typescript", "--command", "tsc", "--target", "es2020", "--moduleResolution", "node", "out.ts")
        }
        val tscExit = Process(tscCmd).!
        val jsExists = Files.exists(Paths.get("out.js"))
        if (tscExit != 0 && !jsExists) {
          System.exit(tscExit)
        }

        val nodeCmd = if (Process(Seq("sh", "-lc", "command -v node >/dev/null 2>&1")).! == 0) {
          Seq("node", "out.js")
        } else {
          Seq("nix", "shell", "nixpkgs#nodejs", "--command", "node", "out.js")
        }
        val nodeExit = Process(nodeCmd).!
        if (nodeExit != 0) {
          System.exit(nodeExit)
        }
      }
    } else {
      println("Elaboration successful! Compiling to Go...")
      val goFile = GoBackend.lowerProgram(ast, packageName = "main")
      val goCode = goFile.toDoc.layout(0)

      val outPath = Paths.get("out.go")
      Files.writeString(outPath, goCode)
      println(s"Go code generated successfully in ${outPath.toAbsolutePath}")

      if (run) {
        println("Running the compiled Go program...")
        val cmd = if (Process(Seq("sh", "-lc", "command -v go >/dev/null 2>&1")).! == 0) {
          Seq("go", "run", "out.go")
        } else {
          Seq("nix", "shell", "nixpkgs#go", "--command", "go", "run", "out.go")
        }
        val exitCode = Process(cmd).!
        if (exitCode != 0) {
          System.exit(exitCode)
        }
      }
    }
  }
