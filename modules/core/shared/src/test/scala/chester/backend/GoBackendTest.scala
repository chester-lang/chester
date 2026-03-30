package chester.backend

import munit.FunSuite

import scala.language.experimental.genericNumberLiterals

import chester.tyck.ElabTestUtils
import chester.utils.doc.{DocConf, render, *, given}

class GoBackendTest extends FunSuite:

  private given DocConf = DocConf.Default

  private def normalize(output: String): String =
    output.linesIterator.map(_.trim).filter(_.nonEmpty).mkString("\n").trim

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
    assert(rendered.contains("return nil"), s"Expected unit value to lower to nil, got:\n$rendered")
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
