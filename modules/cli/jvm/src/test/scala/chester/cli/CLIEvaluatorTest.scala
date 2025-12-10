package chester.cli

import scala.language.experimental.genericNumberLiterals
import scala.annotation.experimental

import java.io.{ByteArrayOutputStream, PrintStream}

import cats.Id
import chester.core.AST
import chester.utils.io.*
import chester.utils.io.impl.{given}
import munit.FunSuite
import chester.utils.term.Terminal

@experimental
class CLIEvaluatorTest extends FunSuite:

  // Use the JVM default terminal for Id-based runs
  private given Terminal[Id] = chester.utils.io.DefaultTerminal

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

  test("CLI run evaluates pure expression file") {
    val tmp = os.temp.dir()
    val src = tmp / "expr.chester"
    os.write.over(src, "2+2")
    val out = captureOutput {
      CLI.run[Id](Config.Run(Some(src.toString)))
    }
    assert(out.contains("=> 4"), clue = out)
  }

  test("CLI run evaluates println file") {
    val tmp = os.temp.dir()
    val src = tmp / "print.chester"
    os.write.over(src, """println("hi")""")
    val out = captureOutput {
      CLI.run[Id](Config.Run(Some(src.toString)))
    }
    assert(out.contains("hi"), clue = out)
    assert(out.contains("=> ()"), clue = out)
  }
