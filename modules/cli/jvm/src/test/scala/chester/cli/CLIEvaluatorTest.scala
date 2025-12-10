package chester.cli

import scala.language.experimental.genericNumberLiterals
import scala.annotation.experimental

import java.io.{ByteArrayOutputStream, PrintStream}

import cats.Id
import chester.core.AST
import chester.utils.io.*
import chester.utils.io.impl.{given}
import munit.FunSuite

@experimental
class CLIEvaluatorTest extends FunSuite:

  private def captureOutput(body: => Unit): String =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    Console.withOut(ps) {
      Console.withErr(ps) {
        body
      }
    }
    baos.toString("UTF-8")

  test("evaluate pure addition 2+2") {
    val expr = AST.App(
      AST.Ref(chester.uniqid.Uniqid.make, "+", None),
      Vector(
        chester.core.Arg(AST.IntLit(2, None)),
        chester.core.Arg(AST.IntLit(2, None))
      ),
      implicitArgs = false,
      None
    )
    val value = Evaluator.eval[Id](expr)
    assert(value == Evaluator.IntV(4))
  }

  test("evaluate println returns unit and prints") {
    val expr = AST.App(
      AST.Ref(chester.uniqid.Uniqid.make, "println", None),
      Vector(chester.core.Arg(AST.StringLit("word", None))),
      implicitArgs = false,
      None
    )
    val out = captureOutput {
      val value = Evaluator.eval[Id](expr)
      assert(value == Evaluator.UnitV)
    }
    assert(out.contains("word"), clue = s"captured output: '$out'")
  }
