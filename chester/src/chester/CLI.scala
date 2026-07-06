package chester

import chester.core.{AST, Param, Telescope, BuiltinEffect, EffectRef, Implicitness}
import chester.tyck.{ElabConstraint, ElabContext, ElabHandlerConf, ElabProblem, GoImportSignature, JSImportSignature, CoreTypeChecker, substituteSolutions}
import chester.uniqid.Uniqid
import chester.utils.elab.ProceduralSolverModule
import chester.error.VectorReporter
import chester.backend.GoBackend
import chester.backend.TypeScriptBackend
import chester.backend.JavaBackend
import chester.syntax.TypeScriptAST
import chester.syntax.JavaAST
import java.nio.file.{Paths, Files}
import scala.sys.process.*
import chester.utils.doc.{DocConf, render}

object CLI:
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
      println("Usage: mill chester.run [file.chester] [--target go|ts|java] [--run]")
      return
    }

    val path = Paths.get(filePathStr)
    if (!Files.exists(path)) {
      System.err.println(s"Error: file not found: $filePathStr")
      System.exit(1)
    }

    val content = Files.readString(path)
    val stdlibContent = CompilerPipeline.loadStdlib(target)
    val fullContent = if (stdlibContent.nonEmpty) {
      println(s"Loading standard library for target $target...")
      stdlibContent + "\n" + content
    } else {
      content
    }
    println(s"Reading program from $filePathStr...")
    
    // Elaborate surface CST into typed AST with standard signatures preloaded
    val (astOpt, tyOpt, errors) = CompilerPipeline.elaborate(fullContent)
    
    if (errors.nonEmpty) {
      System.err.println("Elaboration failed with the following errors:")
      errors.foreach(err => System.err.println(s"  - $err"))
      System.exit(1)
    }

    val ast = astOpt.get

    if (target == "java") {
      println("Elaboration successful! Compiling to Java...")
      val program = JavaBackend.lowerProgram(ast)
      given DocConf = DocConf.Default
      var javaCode = program.toDoc.layout(0)
      
      if (javaCode.contains("__chester_")) {
        // Simple manual injection of Java helper methods at the end of the Main class
        // Replace the last closing brace with the helpers + closing brace
        val helpers = """
  interface ChesterFunc { Object apply(Object... args); }

  public static boolean __chester_as_bool(Object v) { return (Boolean) v; }
  public static Object __chester_int_add(Object a, Object b) { return (Integer) a + (Integer) b; }
  public static Object __chester_int_sub(Object a, Object b) { return (Integer) a - (Integer) b; }
  public static Object __chester_int_mul(Object a, Object b) { return (Integer) a * (Integer) b; }
  public static boolean __chester_int_eq(Object a, Object b) { return ((Integer) a).equals((Integer) b); }
  public static boolean __chester_int_lt(Object a, Object b) { return (Integer) a < (Integer) b; }

  public static Object __chester_list_length(Object list) {
    if (list instanceof java.util.List) {
      return ((java.util.List<?>) list).size();
    } else if (list instanceof Object[]) {
      return ((Object[]) list).length;
    }
    return 0;
  }

  public static Object __chester_list_get(Object list, Object idx) {
    int i = (Integer) idx;
    if (list instanceof java.util.List) {
      return ((java.util.List<?>) list).get(i);
    } else if (list instanceof Object[]) {
      return ((Object[]) list)[i];
    }
    return null;
  }

  public static Object __chester_list_make(Object size, Object generator) {
    int n = (Integer) size;
    Object[] arr = new Object[n];
    if (generator instanceof ChesterFunc) {
      ChesterFunc gen = (ChesterFunc) generator;
      for (int i = 0; i < n; i++) arr[i] = gen.apply(i);
    }
    return java.util.Arrays.asList(arr);
  }

  public static Object __chester_if_else(boolean cond, java.util.function.Supplier<Object> thenBranch, java.util.function.Supplier<Object> elseBranch) {
    return cond ? thenBranch.get() : elseBranch.get();
  }
}"""
        javaCode = javaCode.substring(0, javaCode.lastIndexOf('}')) + helpers
      }

      val outPath = Paths.get("Main.java")
      Files.writeString(outPath, javaCode)
      println(s"Java code generated successfully in ${outPath.toAbsolutePath}")

      if (run) {
        println("Running the compiled Java program...")
        val cmd = if (Process(Seq("sh", "-lc", "command -v java >/dev/null 2>&1")).! == 0) {
          Seq("java", "Main.java")
        } else {
          Seq("nix", "shell", "nixpkgs#jdk", "--command", "java", "Main.java")
        }
        val exitCode = Process(cmd).!
        if (exitCode != 0) {
          System.exit(exitCode)
        }
      }
    } else if (target == "ts") {
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
          "}\n" +
          "\n" +
          "var __chester_effect_handlers = map[string][]func(...any)any{}\n" +
          "\n" +
          "func __chester_push_handler(op string, handler func(...any)any) {\n" +
          "\t__chester_effect_handlers[op] = append(__chester_effect_handlers[op], handler)\n" +
          "}\n" +
          "\n" +
          "func __chester_pop_handler(op string) {\n" +
          "\thandlers := __chester_effect_handlers[op]\n" +
          "\t__chester_effect_handlers[op] = handlers[:len(handlers)-1]\n" +
          "}\n" +
          "\n" +
          "func __chester_do(op string, args ...any) any {\n" +
          "\thandlers := __chester_effect_handlers[op]\n" +
          "\treturn handlers[len(handlers)-1](args...)\n" +
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
