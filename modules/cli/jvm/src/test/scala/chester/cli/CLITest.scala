package chester.cli

import munit.FunSuite

import scala.language.experimental.genericNumberLiterals
import scala.annotation.experimental

import cats.Id
import chester.utils.io.impl.given
import chester.utils.term.given
import os.Path

@experimental
class CLITest extends FunSuite:

  private given chester.utils.term.Terminal[Id] = chester.utils.io.DefaultTerminal

  private def capture(body: => Unit): String =
    val baos = new java.io.ByteArrayOutputStream()
    val ps = new java.io.PrintStream(baos)
    Console.withOut(ps) {
      Console.withErr(ps) {
        body
      }
    }
    baos.toString("UTF-8")

  test("ts command writes output file for single input") {
    val tmpDir = os.temp.dir()
    val src = tmpDir / "hello.chester"
    val code =
      """def id(x: Integer) = x;
        |id""".stripMargin
    os.write.over(src, code)

    CLI.run[Id](Config.CompileTS(src.toString, Some(tmpDir.toString)))

    val outFile = tmpDir / "hello.ts"
    assert(os.exists(outFile), clue = s"Expected $outFile to be created")
    val content = os.read(outFile)
    assert(content.nonEmpty, clue = "TypeScript output should not be empty")
  }

  test("help prints usage") {
    val output = capture {
      CLI.run[Id](Config.Help)
    }
    assert(output.contains("Usage:"), clue = output)
  }
