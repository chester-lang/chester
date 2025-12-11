package chester.web

import chester.cli.{CLI, Config}
import chester.utils.io.*
import chester.utils.term.{InTerminal, InTerminalNoHistory, Terminal, TerminalInit}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.language.experimental.genericNumberLiterals
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

trait ReplCallbacks extends js.Object {
  def readLine(prompt: String): js.Promise[js.UndefOr[String]]
  def print(line: String, isError: js.UndefOr[Boolean] = js.undefined): Unit
}

private class JsTerminal(callbacks: ReplCallbacks)(using Runner[Future]) extends InTerminalNoHistory[Future] {

  override def writeln(line: fansi.Str): Future[Unit] =
    Future.successful(callbacks.print(line.render))

  override def readALine(prompt: fansi.Str): Future[String] = {
    callbacks
      .readLine(prompt.render)
      .toFuture
      .map(opt => opt.map(_.toString).getOrElse(null.asInstanceOf[String]))
  }
}

private given BrowserRunner: Runner[Future] {
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

private given BrowserSpawn: Spawn[Future] {
  override def spawn(x: => Future[Unit]): Unit =
    x.recover { case _ => () }
}

private final case class BrowserIO(callbacks: ReplCallbacks) extends IO[Future] {
  type Path = String

  override def pathOps: PathOps[String] = PathOpsString

  override def println(x: String, toStderr: Boolean = false): Future[Unit] = Future.successful(callbacks.print(x, toStderr))

  override def ask(x: String): Future[String] =
    Future.failed(new UnsupportedOperationException("ask is not implemented in browser REPL"))

  override def readString(path: String): Future[String] =
    Future.failed(new UnsupportedOperationException("readString is not implemented in browser REPL"))

  override def read(path: String): Future[Array[Byte]] =
    Future.failed(new UnsupportedOperationException("read is not implemented in browser REPL"))

  override def writeString(path: String, content: String, writeMode: WriteMode): Future[Unit] =
    Future.failed(new UnsupportedOperationException("writeString is not implemented in browser REPL"))

  override def write(path: String, content: Array[Byte]): Future[Unit] =
    Future.failed(new UnsupportedOperationException("write is not implemented in browser REPL"))

  override def removeWhenExists(path: String): Future[Boolean] = Future.successful(false)

  override def getHomeDir: Future[String] = Future.successful("/")

  override def exists(path: String): Future[Boolean] = Future.successful(false)

  override def createDirRecursiveIfNotExists(path: String): Future[Unit] = Future.successful(())

  override def downloadToFile(url: String, path: String): Future[Unit] =
    Future.failed(new UnsupportedOperationException("downloadToFile is not implemented in browser REPL"))

  override def chmodExecutable(path: String): Future[Unit] = Future.successful(())

  override def getAbsolutePath(path: String): Future[String] = Future.successful(path)

  override def call(command: Seq[String]): Future[CommandOutput] =
    Future.failed(new UnsupportedOperationException("call is not implemented in browser REPL"))

  override def listFiles(path: String): Future[Seq[String]] = Future.successful(Seq.empty)

  override def isDirectory(path: String): Future[Boolean] = Future.successful(false)

  override def workingDir: Future[String] = Future.successful("/")
}

private def makeTerminal(callbacks: ReplCallbacks)(using Runner[Future]): Terminal[Future] =
  new Terminal[Future] {
    override def runTerminal[T](init: TerminalInit, block: InTerminal[Future] ?=> Future[T]): Future[T] =
      block(using new JsTerminal(callbacks))
  }

@JSExportTopLevel("startRepl")
def startRepl(callbacks: ReplCallbacks): js.Promise[Unit] = {
  given Runner[Future] = BrowserRunner
  given Spawn[Future] = BrowserSpawn
  given IO[Future] = BrowserIO(callbacks)
  given Terminal[Future] = makeTerminal(callbacks)

  (new CLI[Future]())
    .run(Config.Run(None))
    .recover { case _ => () }
    .toJSPromise
}
