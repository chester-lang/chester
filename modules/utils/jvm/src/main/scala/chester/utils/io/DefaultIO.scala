package chester.utils.io.impl

import scala.annotation.tailrec
import scala.util.Try

import cats.Id
import chester.utils.os2
import chester.utils.io.*
import _root_.os.*
import com.eed3si9n.ifdef.*
import java.nio.file.Files

given DefaultSpawn: Spawn[Id] {
  override inline def spawn(x: => Unit): Unit = x
}

given DefaultRunner: Runner[Id] {
  override inline def pure[A](x: A): A = x

  override inline def flatMap[A, B](fa: A)(f: A => B): B = f(fa)

  override inline def map[A, B](fa: A)(f: A => B): B = f(fa)

  @tailrec
  override final def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(
    a
  ) match {
    case Left(a1) => tailRecM(a1)(f)
    case Right(b) => b
  }

  override inline def doTry[T](IO: Id[T]): Try[T] = Try(IO)
}

object DefaultPathOps extends PathOps[_root_.os.FilePath] {
  override inline def of(path: String): _root_.os.FilePath = FilePath(path)

  override inline def join(p1: os.FilePath, p2: String): _root_.os.FilePath = {
    (p1 / os.SubPath(p2)).asInstanceOf[os.FilePath]
  }

  override inline def asString(p: os.FilePath): String = p.toString

  override inline def baseName(p: os.FilePath): String = p.last

  override inline def isAbsolute(p: os.FilePath): Boolean = p.toNIO.isAbsolute

  override inline def isRelative(p: os.FilePath): Boolean = !p.toNIO.isAbsolute
}

given DefaultIO: IO[Id] {
  type Path = os.FilePath

  override inline def pathOps: PathOps[FilePath] = DefaultPathOps

  override inline def println(x: String, toStderr: Boolean = false): Unit = if (toStderr) {
    System.err.println(x)
  } else {
    Predef.println(x)
  }
  override inline def ask(x: String): String = {
    Predef.print(x)
    scala.io.StdIn.readLine()
  }

  override def workingDir: os.Path = os2.pwd

  override inline def readString(path: Path): String = {
    os.read(path.resolveFrom(workingDir))
  }

  override inline def read(path: Path): Array[Byte] = {
    os.read.bytes(path.resolveFrom(workingDir))
  }

  override inline def writeString(
      path: Path,
      content: String,
      writeMode: WriteMode = WriteMode.Create
  ): Unit = {
    writeMode match {
      case WriteMode.Create =>
        os.write(path.resolveFrom(workingDir), content)
      case WriteMode.Append =>
        os.write.append(path.resolveFrom(workingDir), content)
      case WriteMode.Overwrite =>
        os.write.over(path.resolveFrom(workingDir), content)
    }
  }

  override inline def write(path: Path, content: Array[Byte]): Unit = {
    os.write(path.resolveFrom(workingDir), content)
  }

  override inline def removeWhenExists(path: Path): Boolean = {
    os.remove(path.resolveFrom(workingDir), true)
  }

  override inline def getHomeDir: Path = FilePath(
    java.nio.file.Paths.get(System.getProperty("user.home"))
  )

  override inline def exists(path: Path): Boolean = {
    os.exists(path.resolveFrom(workingDir))
  }

  override inline def createDirRecursiveIfNotExists(path: Path): Unit = {
    os.makeDir.all(path.resolveFrom(workingDir))
  }

  override inline def downloadToFile(url: String, path: Path): Unit = {
    FileDownloader.downloadFile(url, path.toNIO)
  }

  override inline def chmodExecutable(path: Path): Unit = {
    val perms = Files.getPosixFilePermissions(path.toNIO)
    perms.add(java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE)
    perms.add(java.nio.file.attribute.PosixFilePermission.GROUP_EXECUTE)
    perms.add(java.nio.file.attribute.PosixFilePermission.OTHERS_EXECUTE)
    val _ = Files.setPosixFilePermissions(path.toNIO, perms)
  }

  override inline def getAbsolutePath(path: Path): Path = path.resolveFrom(workingDir)

  @ifndef("scalaNativeNoMultithread")
  override inline def call(command: Seq[String]): CommandOutput = {
    os2.init()
    val result = os.call(command, stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
    CommandOutput(exitCode = Some(result.exitCode))
  }

  @ifdef("scalaNativeNoMultithread")
  override inline def call(command: Seq[String]): CommandOutput = {
    throw new NotImplementedError("Not implemented for Termux")
  }

  override def listFiles(path: Path): Id[Seq[Path]] = {
    os.list(path.resolveFrom(workingDir))
  }

  override def isDirectory(path: Path): Id[Boolean] = {
    os.isDir(path.resolveFrom(workingDir))
  }
}
