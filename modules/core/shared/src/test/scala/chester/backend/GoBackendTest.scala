package chester.backend

import munit.FunSuite

import java.nio.file.Files

import scala.language.experimental.genericNumberLiterals
import scala.sys.process.*

import chester.core.{AST, BuiltinEffect, EffectRef, Implicitness, Param, Telescope}
import chester.error.VectorReporter
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.tyck.ElabTestUtils
import chester.tyck.{ElabConstraint, ElabContext, ElabHandlerConf, ElabProblem, GoImportSignature, JSImportSignature, substituteSolutions}
import chester.uniqid.Uniqid
import chester.utils.elab.ProceduralSolverModule
import chester.utils.doc.{DocConf, render, *, given}

class GoBackendTest extends FunSuite:

  private given DocConf = DocConf.Default

  private def normalize(output: String): String =
    output.linesIterator.map(_.trim).filter(_.nonEmpty).mkString("\n").trim

  private def commandExists(name: String): Boolean =
    Process(Seq("sh", "-lc", s"command -v $name >/dev/null 2>&1")).! == 0

  private def compileWithGoCompiler(rendered: String, fileName: String): Unit = {
    val tempDir = Files.createTempDirectory("chester-go-test")
    val sourcePath = tempDir.resolve(fileName)
    Files.writeString(sourcePath, rendered)

    val exitCode =
      if commandExists("go") then
        Process(Seq("go", "build", sourcePath.getFileName.toString), tempDir.toFile).!
      else
        Process(
          Seq("nix", "shell", "nixpkgs#go", "--command", "sh", "-lc", s"go build ${sourcePath.getFileName}"),
          tempDir.toFile
        ).!

    assertEquals(exitCode, 0, s"Expected generated Go to compile:\n$rendered")
  }

  private def elaborateWithGoImports(code: String, goImports: Map[String, GoImportSignature]): AST = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val source = Source(FileNameAndContent("test.chester", code))
    val cst = (for
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    yield Parser.parseFile(tokens)).toOption.getOrElse(fail("Expected parse result"))

    val module = ProceduralSolverModule
    val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))
    val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
    val typeCell = module.newOnceCell[ElabConstraint, AST](solver)
    val allImports = goImports.map { case (k, v) => k -> JSImportSignature(v.fields) }
    val ctx = ElabContext(bindings = Map.empty, types = Map.empty, jsImports = allImports, reporter = elabReporter)

    module.addConstraint(solver, ElabConstraint.InferTopLevel(cst, resultCell, typeCell, ctx))
    module.run(solver)

    assert(elabReporter.getReports.isEmpty, s"Elaboration failed: ${elabReporter.getReports}")
    module.readStable(solver, resultCell).map(ast => substituteSolutions(ast)(using module, solver)).getOrElse(fail("Expected elaborated AST"))
  }

  private def fmtPrintfSignature: GoImportSignature = {
    val argsParam = Param(Uniqid.make, "args", AST.AnyType(None), Implicitness.Explicit, None)
    val printfTy = AST.Pi(
      Vector(Telescope(Vector(argsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    GoImportSignature(Vector(Param(Uniqid.make, "Printf", printfTy, Implicitness.Explicit, None)), "fmt")
  }

  test("backend renders a simple program shape") {
    val code =
      """{ record Box(value: String);
        |  def add1(x: Integer) = x + 1;
        |  () }""".stripMargin

    val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val file = GoBackend.lowerProgram(astOpt.get, packageName = "main")
    val rendered = normalize(render(file.toDoc).toString)

    val expected = normalize(
      """package main
        |type Box struct {
        |value string
        |}
        |func add1(x int) {
        |return x + 1
        |}""".stripMargin
    )

    assertEquals(rendered, expected)
  }

  test("unit lowers to struct{} return type and nil value") {
    val code =
      """{ def noop(x: Integer): Unit = ();
        |  () }""".stripMargin

    val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val file = GoBackend.lowerProgram(astOpt.get, packageName = "main")
    val rendered = normalize(render(file.toDoc).toString)

    assert(rendered.contains("func noop(x int) struct {}"), s"Expected unit type to lower to struct {}, got:\n$rendered")
    assert(rendered.contains("return struct {}{}"), s"Expected unit value to lower to struct {}{}, got:\n$rendered")
  }

  test("let expression lowers to Go IIFE with explicit result type") {
    val code = "{ let x = 1; x }"

    val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val file = GoBackend.lowerProgram(astOpt.get, packageName = "main")
    val rendered = normalize(render(file.toDoc).toString)

    assert(!rendered.contains("func() any"), s"Did not expect statement-level let to lower through an IIFE, got:\n$rendered")
    assert(rendered.contains("x := 1"), s"Expected let binding assignment in lowered Go, got:\n$rendered")
    assert(rendered.contains("fmt.Println(x)"), s"Expected top-level tail to consume the let-bound value directly, got:\n$rendered")
  }

  test("block expression with statements lowers to Go IIFE with explicit result type") {
    val code =
      """{ def id(x: Integer) = x;
        |  { id(1); id(2) } }""".stripMargin

    val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val file = GoBackend.lowerProgram(astOpt.get, packageName = "main")
    val rendered = normalize(render(file.toDoc).toString)

    assert(rendered.contains("func() any"), s"Expected block IIFE to declare an explicit result type, got:\n$rendered")
    assert(rendered.contains("id(1)"), s"Expected lowered Go block to preserve intermediate statement, got:\n$rendered")
    assert(rendered.contains("return id(2)"), s"Expected lowered Go block to return the tail value, got:\n$rendered")
  }

  test("record constructor lowering preserves declared field names") {
    val code =
      """{ record Vec2d(x: Integer, y: Integer);
        |  let v: Vec2d.t = Vec2d(1, 2);
        |  v.x }""".stripMargin

    val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val file = GoBackend.lowerProgram(astOpt.get, packageName = "main")
    val rendered = normalize(render(file.toDoc).toString)

    assert(rendered.contains("v := Vec2d{"), s"Expected record constructor assignment in lowered Go, got:\n$rendered")
    assert(rendered.contains("x: 1"), s"Expected named field x in lowered Go constructor, got:\n$rendered")
    assert(rendered.contains("y: 2"), s"Expected named field y in lowered Go constructor, got:\n$rendered")
    assert(rendered.contains("fmt.Println(v.x)"), s"Expected named field access in lowered Go, got:\n$rendered")
    assert(!rendered.contains("_1"), s"Did not expect positional record field names in lowered Go, got:\n$rendered")
  }

  test("Bool lowers to Go bool") {
    val code = """{ def negate(flag: Bool): Bool = flag; negate(false) }"""

    val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val file = GoBackend.lowerProgram(astOpt.get, packageName = "main")
    val rendered = normalize(render(file.toDoc).toString)

    assert(rendered.contains("func negate(flag bool) bool"), s"Expected Bool to lower to bool, got:\n$rendered")
    assert(rendered.contains("fmt.Println(negate(false))"), s"Expected false literal to stay boolean, got:\n$rendered")
  }

  test("generated Go for pure program compiles with Go compiler") {
    val code =
      """{ def add(x: Integer, y: Integer): Integer = {
        |    x + y
        |  };
        |  add(40, 2) }""".stripMargin

    val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val file = GoBackend.lowerProgram(astOpt.get, packageName = "main")
    val rendered = render(file.toDoc).toString
    compileWithGoCompiler(rendered, "pure.go")
  }

  test("generated Go for fmt FFI program compiles with Go compiler") {
    val code =
      """import go "fmt";
        |def main(): Unit / [io] = {
        |  go.fmt.Printf("Hello from Chester with Go FFI!");
        |  ()
        |};
        |main()""".stripMargin

    val ast = elaborateWithGoImports(code, Map("fmt" -> fmtPrintfSignature))
    val file = GoBackend.lowerProgram(ast, packageName = "main")
    val rendered = render(file.toDoc).toString
    compileWithGoCompiler(rendered, "ffi.go")
  }
