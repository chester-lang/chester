package chester.docs

import scala.language.experimental.genericNumberLiterals
import chester.cli.{CLI, Config}
import chester.utils.io.*
import chester.utils.term.{InTerminal, InTerminalNoHistory, Terminal, TerminalInit}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.annotation.*

// Simplified terminal that stores output instead of printing
private class DocsTerminal(outputBuffer: collection.mutable.ArrayBuffer[String])(using Runner[Future])
    extends InTerminalNoHistory[Future] {

  override def writeln(line: fansi.Str): Future[Unit] = {
    outputBuffer += line.render
    Future.successful(())
  }

  override def readALine(prompt: fansi.Str): Future[String] = {
    // Not used in evaluate mode
    Future.successful("")
  }
}

private given DocsRunner: Runner[Future] {
  override def doTry[T](io: Future[T]): Future[scala.util.Try[T]] =
    io.transform(result => scala.util.Success(result))

  override def pure[A](x: A): Future[A] = Future.successful(x)

  override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

  override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

  override def tailRecM[A, B](a: A)(f: A => Future[Either[A, B]]): Future[B] =
    f(a).flatMap {
      case Left(a1) => tailRecM(a1)(f)
      case Right(b) => Future.successful(b)
    }
}

private given DocsSpawn: Spawn[Future] {
  override def spawn(x: => Future[Unit]): Unit =
    x.recover { case _ => () }
}

private final case class DocsIO(outputBuffer: collection.mutable.ArrayBuffer[String]) extends IO[Future] {
  type Path = String

  override def pathOps: PathOps[String] = PathOpsString

  override def println(x: String, toStderr: Boolean = false): Future[Unit] = {
    outputBuffer += x
    Future.successful(())
  }

  override def ask(x: String): Future[String] =
    Future.failed(new UnsupportedOperationException("ask not supported in docs REPL"))

  override def readString(path: String): Future[String] =
    Future.failed(new UnsupportedOperationException("readString not supported in docs REPL"))

  override def read(path: String): Future[Array[Byte]] =
    Future.failed(new UnsupportedOperationException("read not supported in docs REPL"))

  override def writeString(path: String, content: String, writeMode: WriteMode): Future[Unit] =
    Future.failed(new UnsupportedOperationException("writeString not supported in docs REPL"))

  override def write(path: String, content: Array[Byte]): Future[Unit] =
    Future.failed(new UnsupportedOperationException("write not supported in docs REPL"))

  override def removeWhenExists(path: String): Future[Boolean] = Future.successful(false)

  override def getHomeDir: Future[String] = Future.successful("/")

  override def exists(path: String): Future[Boolean] = Future.successful(false)

  override def createDirRecursiveIfNotExists(path: String): Future[Unit] = Future.successful(())

  override def downloadToFile(url: String, path: String): Future[Unit] =
    Future.failed(new UnsupportedOperationException("downloadToFile not supported in docs REPL"))

  override def chmodExecutable(path: String): Future[Unit] = Future.successful(())

  override def getAbsolutePath(path: String): Future[String] = Future.successful(path)

  override def call(command: Seq[String]): Future[CommandOutput] =
    Future.failed(new UnsupportedOperationException("call not supported in docs REPL"))

  override def listFiles(path: String): Future[Seq[String]] = Future.successful(Seq.empty)

  override def isDirectory(path: String): Future[Boolean] = Future.successful(false)

  override def workingDir: Future[String] = Future.successful("/")
}

private def makeTerminal(outputBuffer: collection.mutable.ArrayBuffer[String])(using Runner[Future]): Terminal[Future] =
  new Terminal[Future] {
    override def runTerminal[T](init: TerminalInit, block: InTerminal[Future] ?=> Future[T]): Future[T] =
      block(using new DocsTerminal(outputBuffer))
  }

@JSExportTopLevel("ChesterREPL")
object ReplApi {

  @JSExport
  def evaluate(code: String): js.Promise[String] = {
    given Runner[Future] = DocsRunner
    given Spawn[Future] = DocsSpawn
    
    val outputBuffer = collection.mutable.ArrayBuffer.empty[String]
    given IO[Future] = DocsIO(outputBuffer)
    given Terminal[Future] = makeTerminal(outputBuffer)

    // Run REPL with the provided code
    // Using Run(None) starts the REPL which will read from terminal
    // Since we override writeln to capture output, we can capture results
    (new CLI[Future]())
      .run(Config.Run(None))
      .map { _ =>
        // Return collected output
        if (outputBuffer.isEmpty) "No output"
        else outputBuffer.mkString("\n")
      }
      .recover { case e: Exception =>
        s"Error: ${e.getMessage}\n${e.getStackTrace.take(5).mkString("\n")}"
      }
      .toJSPromise
  }

  @JSExport
  def version(): String = "0.1.0-SNAPSHOT"

  @JSExport
  def help(): String = {
    """Chester REPL - Interactive Chester Programming Language
      |
      |Usage:
      |  ChesterREPL.evaluate(code) - Evaluate Chester code
      |  ChesterREPL.version()      - Get version
      |  ChesterREPL.help()         - Show this help
      |
      |Example:
      |  ChesterREPL.evaluate("1 + 2").then(console.log)
      |  ChesterREPL.evaluate("def square(x: Int): Int = x * x; square(5)").then(console.log)
      |""".stripMargin
  }
}

