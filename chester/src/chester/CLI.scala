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
    val sprintfTy = AST.Pi(
      Vector(Telescope(Vector(formatParam, argsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector.empty,
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
        Param(Uniqid.make, "Sprintf", sprintfTy, Implicitness.Explicit, None),
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

  private def consoleSignature: JSImportSignature = {
    val logParam = Param(Uniqid.make, "message", AST.AnyType(None), Implicitness.Explicit, None)
    val logTy = AST.Pi(
      Vector(Telescope(Vector(logParam), Implicitness.Explicit)),
      AST.TupleType(Vector.empty, None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    JSImportSignature(
      Vector(
        Param(Uniqid.make, "log", logTy, Implicitness.Explicit, None)
      ),
      chester.core.JSImportKind.Default
    )
  }

  private def utilSignature: JSImportSignature = {
    val formatParam = Param(Uniqid.make, "format", AST.StringType(None), Implicitness.Explicit, None)
    val argsParam = Param(Uniqid.make, "args", AST.ListType(AST.AnyType(None), None), Implicitness.Explicit, None)
    val formatTy = AST.Pi(
      Vector(Telescope(Vector(formatParam, argsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector.empty,
      None
    )
    JSImportSignature(
      Vector(
        Param(Uniqid.make, "format", formatTy, Implicitness.Explicit, None)
      ),
      chester.core.JSImportKind.Default
    )
  }

  def loadStdlib(target: String): String = {
    val resourcePath = s"/stdlib/$target/std.chester"
    val stream = getClass.getResourceAsStream(resourcePath)
    if (stream != null) {
      try {
        new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
      } finally {
        stream.close()
      }
    } else {
      val localPath = Paths.get(s"stdlib/$target/std.chester")
      if (Files.exists(localPath)) {
        Files.readString(localPath)
      } else {
        ""
      }
    }
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
      val stdJsImports = Map("path" -> pathSignature, "console" -> consoleSignature, "util" -> utilSignature)
      val allImports = stdGoImports.map { case (k, v) => k -> JSImportSignature(v.fields) } ++ stdJsImports

      val tId_length = Uniqid.make[AST]
      val listLengthTy = AST.Pi(
        Vector(
          Telescope(Vector(Param(tId_length, "T", AST.Type(AST.LevelLit(0, None), None), Implicitness.Implicit, None)), Implicitness.Implicit),
          Telescope(Vector(Param(Uniqid.make, "list", AST.ListType(AST.Ref(tId_length, "T", None), None), Implicitness.Explicit, None)), Implicitness.Explicit)
        ),
        AST.IntegerType(None),
        Vector.empty,
        None
      )

      val tId_get = Uniqid.make[AST]
      val listGetTy = AST.Pi(
        Vector(
          Telescope(Vector(Param(tId_get, "T", AST.Type(AST.LevelLit(0, None), None), Implicitness.Implicit, None)), Implicitness.Implicit),
          Telescope(Vector(
            Param(Uniqid.make, "list", AST.ListType(AST.Ref(tId_get, "T", None), None), Implicitness.Explicit, None),
            Param(Uniqid.make, "index", AST.IntegerType(None), Implicitness.Explicit, None)
          ), Implicitness.Explicit)
        ),
        AST.Ref(tId_get, "T", None),
        Vector.empty,
        None
      )

      val tId_make = Uniqid.make[AST]
      val genParam = Param(Uniqid.make, "i", AST.IntegerType(None), Implicitness.Explicit, None)
      val generatorTy = AST.Pi(
        Vector(Telescope(Vector(genParam), Implicitness.Explicit)),
        AST.Ref(tId_make, "T", None),
        Vector.empty,
        None
      )
      val listMakeTy = AST.Pi(
        Vector(
          Telescope(Vector(Param(tId_make, "T", AST.Type(AST.LevelLit(0, None), None), Implicitness.Implicit, None)), Implicitness.Implicit),
          Telescope(Vector(
            Param(Uniqid.make, "size", AST.IntegerType(None), Implicitness.Explicit, None),
            Param(Uniqid.make, "generator", generatorTy, Implicitness.Explicit, None)
          ), Implicitness.Explicit)
        ),
        AST.ListType(AST.Ref(tId_make, "T", None), None),
        Vector.empty,
        None
      )

      val tId_if = Uniqid.make[AST]
      val ifElseTy = AST.Pi(
        Vector(
          Telescope(Vector(Param(tId_if, "T", AST.Type(AST.LevelLit(0, None), None), Implicitness.Implicit, None)), Implicitness.Implicit),
          Telescope(Vector(
            Param(Uniqid.make, "cond", AST.BoolType(None), Implicitness.Explicit, None),
            Param(Uniqid.make, "thenVal", AST.Ref(tId_if, "T", None), Implicitness.Explicit, None),
            Param(Uniqid.make, "elseVal", AST.Ref(tId_if, "T", None), Implicitness.Explicit, None)
          ), Implicitness.Explicit)
        ),
        AST.Ref(tId_if, "T", None),
        Vector.empty,
        None
      )

      val intEqTy = AST.Pi(
        Vector(Telescope(Vector(
          Param(Uniqid.make, "a", AST.IntegerType(None), Implicitness.Explicit, None),
          Param(Uniqid.make, "b", AST.IntegerType(None), Implicitness.Explicit, None)
        ), Implicitness.Explicit)),
        AST.BoolType(None),
        Vector.empty,
        None
      )

      val intLtTy = AST.Pi(
        Vector(Telescope(Vector(
          Param(Uniqid.make, "a", AST.IntegerType(None), Implicitness.Explicit, None),
          Param(Uniqid.make, "b", AST.IntegerType(None), Implicitness.Explicit, None)
        ), Implicitness.Explicit)),
        AST.BoolType(None),
        Vector.empty,
        None
      )

      val id_length = Uniqid.make[AST]
      val cell_length = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_length, listLengthTy)

      val id_get = Uniqid.make[AST]
      val cell_get = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_get, listGetTy)

      val id_make = Uniqid.make[AST]
      val cell_make = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_make, listMakeTy)

      val id_if = Uniqid.make[AST]
      val cell_if = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_if, ifElseTy)

      val id_eq = Uniqid.make[AST]
      val cell_eq = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_eq, intEqTy)

      val id_lt = Uniqid.make[AST]
      val cell_lt = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_lt, intLtTy)

      val initialBindings = Map(
        "prim__list_length" -> id_length,
        "prim__list_get" -> id_get,
        "prim__list_make" -> id_make,
        "prim__if_else" -> id_if,
        "prim__int_eq" -> id_eq,
        "prim__int_lt" -> id_lt
      )
      val initialTypes = Map(
        id_length -> cell_length,
        id_get -> cell_get,
        id_make -> cell_make,
        id_if -> cell_if,
        id_eq -> cell_eq,
        id_lt -> cell_lt
      )

      val ctx = ElabContext(bindings = initialBindings, types = initialTypes, jsImports = allImports, reporter = elabReporter)

      module.addConstraint(solver, ElabConstraint.InferTopLevel(cst, resultCell, typeCell, ctx))
      module.run(solver)

      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))

      if (elabReporter.getReports.isEmpty && zonkedResult.isDefined) {
        val solvedInitialTypes = initialTypes.flatMap { case (id, cell) =>
          module.readStable(solver, cell).map(substituteSolutions(_)(using module, solver)).map(id -> _)
        }
        CoreTypeChecker.typeCheck(zonkedResult.get, solvedInitialTypes)(using elabReporter)
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
    val stdlibContent = loadStdlib(target)
    val fullContent = if (stdlibContent.nonEmpty) {
      println(s"Loading standard library for target $target...")
      stdlibContent + "\n" + content
    } else {
      content
    }
    println(s"Reading program from $filePathStr...")
    
    // Elaborate surface CST into typed AST with standard signatures preloaded
    val (astOpt, tyOpt, errors) = elaborate(fullContent)
    
    if (errors.nonEmpty) {
      System.err.println("Elaboration failed with the following errors:")
      errors.foreach(err => System.err.println(s"  - $err"))
      System.exit(1)
    }

    val ast = astOpt.get

    if (target == "ts") {
      println("Elaboration successful! Compiling to TypeScript...")
      val program = TypeScriptBackend.lowerProgram(ast)

      val isUnitType = tyOpt.exists {
        case AST.TupleType(elems, _) if elems.isEmpty => true
        case _ => false
      }

      val updatedStatements = if (program.statements.nonEmpty && !isUnitType) {
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
      var tsCode = render(finalProgram.toDoc).toString
      if (tsCode.contains("__chester_list_make")) {
        tsCode += "\n\nfunction __chester_list_make(size: number, generator: (i: number) => any): any[] {\n" +
                  "  return Array.from({length: size}, (_, i) => generator(i));\n" +
                  "}\n"
      }

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
      var goCode = goFile.toDoc.layout(0)
      if (goCode.contains("__chester_")) {
        goCode += "\n\n" +
          "func __chester_as_bool(v any) bool { b, _ := v.(bool); return b }\n" +
          "func __chester_int_add(a, b any) any { return a.(int) + b.(int) }\n" +
          "func __chester_int_sub(a, b any) any { return a.(int) - b.(int) }\n" +
          "func __chester_int_mul(a, b any) any { return a.(int) * b.(int) }\n" +
          "func __chester_int_eq(a, b any) bool { return a.(int) == b.(int) }\n" +
          "func __chester_int_lt(a, b any) bool { return a.(int) < b.(int) }\n" +
          "\n" +
          "func __chester_list_len(list any) any {\n" +
          "\treturn len(list.([]any))\n" +
          "}\n" +
          "\n" +
          "func __chester_list_get(list any, idx any) any {\n" +
          "\treturn list.([]any)[idx.(int)]\n" +
          "}\n" +
          "\n" +
          "func __chester_list_make(size any, generator func(any) any) []any {\n" +
          "\tn := size.(int)\n" +
          "\tres := make([]any, n)\n" +
          "\tfor i := 0; i < n; i++ {\n" +
          "\t\tres[i] = generator(i)\n" +
          "\t}\n" +
          "\treturn res\n" +
          "}\n"
      }

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
