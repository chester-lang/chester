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
