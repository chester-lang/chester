package chester.cli

import munit.FunSuite

import scala.language.experimental.genericNumberLiterals
import scala.annotation.experimental

import cats.Id
import chester.utils.io.impl.given
import chester.utils.term.*
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

  private class ScriptedTerminal(script: List[ReadLineResult]) extends Terminal[Id], InTerminal[Id]:
    private val queue = scala.collection.mutable.Queue.from(script)

    override def runTerminal[T](init: TerminalInit, block: InTerminal[Id] ?=> T): T =
      block(using this)

    override def writeln(line: fansi.Str): Id[Unit] = ()

    override def readline(info: TerminalInfo): ReadLineResult =
      if queue.nonEmpty then queue.dequeue() else EndOfFile

    override def getHistory: Id[Seq[String]] = Vector.empty

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

  test("repl :t prints inferred type") {
    val terminal = ScriptedTerminal(List(LineRead("1+1"), LineRead(":t 1")))
    given Terminal[Id] = terminal

    val output = capture {
      CLI.run[Id](Config.Run(None))
    }

    assert(output.contains("=> 2"), clue = output)
    assert(output.contains("Type: Integer"), clue = output)
  }

  test(":l loads file definitions for later expressions") {
    val tmpDir = os.temp.dir()
    val src = tmpDir / "pair.chester"
    val code =
      """record Box(value: Integer);
        |def inc(x: Integer) = x + 1;""".stripMargin
    os.write.over(src, code)

    val terminal = ScriptedTerminal(
      List(
        LineRead(s":l ${src.toString}"),
        LineRead("inc(41)")
      )
    )
    given Terminal[Id] = terminal

    val output = capture {
      CLI.run[Id](Config.Run(None))
    }

    assert(output.contains("Loaded"), clue = output)
    assert(output.contains("=> 42"), clue = output)
  }
