package chester.utils.io

import scala.language.implicitConversions
import scala.util.Try

import cats.Monad

trait Spawn[F[_]] {
  def spawn(x: => F[Unit]): Unit
}

trait Runner[F[_]] extends Monad[F] {
  def doTry[T](IO: F[T]): F[Try[T]]
}

extension [F[_], A](m: F[A])(using runner: Runner[F]) {
  inline def flatMap[B](inline f: A => F[B]): F[B] = runner.flatMap(m)(f)
  inline def map[B](inline f: A => B): F[B] = runner.map(m)(f)
}

implicit inline def summonPathOpsFromIO[F[_]](using
    io: IO[F]
): PathOps[io.Path] = io.pathOps

enum WriteMode extends Enum[WriteMode] {
  case Create
  case Append
  case Overwrite
}

trait IO[F[_]] {
  type Path

  def workingDir: F[Path]

  def pathOps: PathOps[Path]

  def println(x: String, toStderr: Boolean = false): F[Unit]
  def ask(x: String): F[String]

  def readString(path: Path): F[String]

  def read(path: Path): F[Array[Byte]]

  def writeString(path: Path, content: String, writeMode: WriteMode = WriteMode.Create): F[Unit]

  def write(path: Path, content: Array[Byte]): F[Unit]

  def removeWhenExists(path: Path): F[Boolean]

  def getHomeDir: F[Path]

  def exists(path: Path): F[Boolean]

  def createDirRecursiveIfNotExists(path: Path): F[Unit]

  def downloadToFile(url: String, path: Path): F[Unit]

  def chmodExecutable(path: Path): F[Unit]

  def getAbsolutePath(path: Path): F[Path]

  def call(
      command: Seq[String]
  ): F[CommandOutput]

  def listFiles(path: Path): F[Seq[Path]]
  def isDirectory(path: Path): F[Boolean]
}

case class CommandOutput(exitCode: Option[Int])

object Runner {
  inline def pure[F[_]: Runner as runner, A](inline x: A): F[A] =
    runner.pure(x)

  inline def doTry[F[_]: Runner as runner, T](inline IO: F[T]): F[Try[T]] =
    runner.doTry(IO)

}

object Spawn {
  inline def spawn[F[_]](inline x: => F[Unit])(using
      inline spawn: Spawn[F]
  ): Unit = spawn.spawn(x)
}

object IO {
  inline def println[F[_]](inline x: String, toStderr: Boolean = false)(using inline io: IO[F]): F[Unit] =
    io.println(x, toStderr)
}

extension [F[_]](_io: IO.type)(using io: IO[F]) {
  inline def ask(inline x: String): F[String] = io.ask(x)
  inline def workingDir: F[io.Path] = io.workingDir
  inline def readString(inline path: io.Path): F[String] = io.readString(path)
  inline def read(inline path: io.Path): F[Array[Byte]] = io.read(path)
  inline def writeString(
      inline path: io.Path,
      inline content: String,
      inline writeMode: WriteMode = WriteMode.Create
  ): F[Unit] = io.writeString(path, content, writeMode)
  inline def write(inline path: io.Path, inline content: Array[Byte]): F[Unit] =
    io.write(path, content)
  inline def removeWhenExists(inline path: io.Path): F[Boolean] =
    io.removeWhenExists(path)
  inline def getHomeDir: F[io.Path] = io.getHomeDir
  inline def exists(inline path: io.Path): F[Boolean] = io.exists(path)
  inline def createDirRecursiveIfNotExists(inline path: io.Path): F[Unit] =
    io.createDirRecursiveIfNotExists(path)
  inline def downloadToFile(inline url: String, inline path: io.Path): F[Unit] =
    io.downloadToFile(url, path)
  inline def chmodExecutable(inline path: io.Path): F[Unit] =
    io.chmodExecutable(path)
  inline def getAbsolutePath(inline path: io.Path): F[io.Path] =
    io.getAbsolutePath(path)
  inline def call(
      inline command: Seq[String]
  ): F[CommandOutput] = io.call(command)
  inline def listFiles(inline path: io.Path): F[Seq[io.Path]] = io.listFiles(path)
  inline def isDirectory(inline path: io.Path): F[Boolean] = io.isDirectory(path)
}
