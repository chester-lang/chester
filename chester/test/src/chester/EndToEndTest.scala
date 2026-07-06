package chester

import scala.sys.process._
import java.nio.file.{Files, Paths}
import chester.utils.doc.DocConf
import chester.backend.GoBackend

class EndToEndTest extends munit.FunSuite {
  test("test_extension.chester executes correctly via Go backend") {
    val target = "go"
    
    val content = """
      |import go "fmt";
      |
      |extension def to_upper_str <to_upper> (s: String): String = {
      |  s
      |};
      |
      |def main(): Unit = {
      |  let s = "hello";
      |  
      |  // method style application using <method> name
      |  go.fmt.Println(s.to_upper());
      |  
      |  // normal function application using the first name
      |  go.fmt.Println(to_upper_str(s))
      |};
      |""".stripMargin

    val stdlibContent = CompilerPipeline.loadStdlib(target)
    val fullContent = if (stdlibContent.nonEmpty) {
      stdlibContent + "\n" + content
    } else {
      content
    }

    val (astOpt, tyOpt, errors) = CompilerPipeline.elaborate(fullContent)
    
    if (errors.nonEmpty) {
      fail(s"Elaboration failed with errors:\n${errors.mkString("\n")}")
    }

    val ast = astOpt.get
    val program = GoBackend.lowerProgram(ast)
    given DocConf = DocConf.Default
    val goCode = program.toDoc.layout(0)
    
    val outPath = Paths.get("out_main.go")
    Files.writeString(outPath, addChesterHelpers(goCode))
    
    val process = if (commandExists("go")) {
      Process(Seq("go", "run", "out_main.go"))
    } else {
      Process(Seq("nix", "shell", "nixpkgs#go", "--command", "sh", "-lc", "go run out_main.go"))
    }
    
    val output = process.!!
    
    val expectedOutput = "hello\nhello\n"
    assertEquals(output.replace("\r\n", "\n"), expectedOutput)
    
    // Cleanup
    Files.deleteIfExists(outPath)
  }

  private def commandExists(name: String): Boolean =
    Process(Seq("sh", "-lc", s"command -v $name >/dev/null 2>&1")).! == 0

  private def addChesterHelpers(code: String): String =
    if !code.contains("__chester_") then code
    else code + "\n\n" +
      "func __chester_as_bool(v any) bool { b, _ := v.(bool); return b }\n" +
      "func __chester_int_add(a, b any) any { return a.(int) + b.(int) }\n" +
      "func __chester_int_sub(a, b any) any { return a.(int) - b.(int) }\n" +
      "func __chester_int_mul(a, b any) any { return a.(int) * b.(int) }\n" +
      "func __chester_int_eq(a, b any) bool { return a.(int) == b.(int) }\n" +
      "func __chester_int_lt(a, b any) bool { return a.(int) < b.(int) }\n" +
      "func __chester_list_len(list any) any { return len(list.([]any)) }\n" +
      "func __chester_list_get(list any, idx any) any { return list.([]any)[idx.(int)] }\n" +
      "func __chester_list_make(size any, generator func(any) any) []any {\n" +
      "\tn := size.(int); res := make([]any, n)\n" +
      "\tfor i := 0; i < n; i++ { res[i] = generator(i) }\n" +
      "\treturn res\n" +
      "}\n" +
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
