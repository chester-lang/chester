package chester.utils.io.impl

import scala.scalajs.js.Thenable.Implicits.*
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.*
import scala.scalajs.js.JSConverters.*

import chester.utils.io.*
import typings.node.bufferMod.global.BufferEncoding
import typings.node.fsMod.MakeDirectoryOptions
import typings.node.{childProcessMod, fsMod, fsPromisesMod, osMod, pathMod, processMod}
import typings.node.childProcessMod.{IOType, SpawnOptions, SpawnSyncOptions}
import typings.std.global.fetch
import chester.i18n.*
import chester.utils.asInt
import typings.node.nodeStrings.close
import java.io.IOException

private val ExistsUseSync = false
private val UseSpawnSync = false

given DefaultIO: IO[Future] {
  // https://stackoverflow.com/questions/75031248/scala-js-convert-uint8array-to-arraybyte/75344498#75344498
  private def toScalaArray(input: Uint8Array): Array[Byte] =
    // Create a view as Int8 on the same underlying data.
    new Int8Array(input.buffer, input.byteOffset, input.length).toArray

  type Path = String

  def pathOps: PathOps[String] = PathOpsString

  override inline def println(x: String, toStderr: Boolean = false): Future[Unit] =
    if (toStderr)
      Future.successful(System.err.println(x))
    else
      Future.successful(Predef.println(x))

  override inline def ask(x: String): Future[String] = throw new UnsupportedOperationException(
    t"ask is not implemented in JS environment"
  )

  override inline def readString(path: String): Future[String] =
    fsPromisesMod.readFile(path, BufferEncoding.utf8)

  // TODO: maybe use https://stackoverflow.com/questions/75031248/scala-js-convert-uint8array-to-arraybyte
  override inline def read(path: String): Future[Array[Byte]] = for {
    buffer <- fsPromisesMod.readFile(path)
  } yield toScalaArray(buffer.asInstanceOf[Uint8Array])

  override inline def writeString(
      path: String,
      content: String,
      writeMode: WriteMode = WriteMode.Create
  ): Future[Unit] =
    writeMode match {
      case WriteMode.Create =>
        for {
          e <- exists(path)
          _ <-
            if e then Future.failed(new IOException(t"File $path already exists.")): Future[Unit]
            else fsPromisesMod.writeFile(path, content): Future[Unit]
        } yield ()
      case WriteMode.Append =>
        fsPromisesMod.appendFile(path, content): Future[Unit]
      case WriteMode.Overwrite =>
        fsPromisesMod.writeFile(path, content): Future[Unit]
    }

  // https://stackoverflow.com/questions/76455786/scala-js-how-to-convert-arraybyte-to-blob/76463887#76463887
  override inline def write(path: String, content: Array[Byte]): Future[Unit] =
    fsPromisesMod.writeFile(path, content.toTypedArray)

  override def removeWhenExists(path: String): Future[Boolean] =
    fsPromisesMod.unlink(path).map(_ => true).recover { case _: js.JavaScriptException =>
      false
    }

  override inline def workingDir: Future[String] =
    Future.successful(processMod.^.cwd())

  override inline def getHomeDir: Future[String] =
    Future.successful(osMod.homedir())

  override def exists(path: String): Future[Boolean] =
    if (ExistsUseSync) {
      Future.successful(fsMod.existsSync(path))
    } else {
      val p = fsPromisesMod.access(path): Future[?]
      p.map(_ => true).recover { case _: js.JavaScriptException => false }
    }

  override inline def createDirRecursiveIfNotExists(
      path: String
  ): Future[Unit] =
    fsPromisesMod
      .mkdir(path, MakeDirectoryOptions().setRecursive(true))
      .map(_ => ())

  override inline def downloadToFile(url: String, path: String): Future[Unit] =
    for {
      fetched <- fetch(url).toFuture
      read <-
        if fetched.ok then fetched.arrayBuffer().toFuture
        else Future.failed(new IOException(t"Failed to fetch $url"))
      _ <- fsPromisesMod.writeFile(path, new Uint8Array(read))
    } yield ()

  override inline def chmodExecutable(path: String): Future[Unit] =
    fsPromisesMod.chmod(path, "755")

  override inline def getAbsolutePath(path: String): Future[String] =
    Future.successful(pathMod.resolve(path))
  override def call(command: Seq[String]): Future[CommandOutput] =
    if (UseSpawnSync) {
      val result = childProcessMod.spawnSync(command.head, command.tail.toJSArray, SpawnSyncOptions().setStdio(IOType.inherit))
      val status = result.status match {
        case null      => None
        case s: Double => Some(s.asInt)
      }
      Future.successful(CommandOutput(status))
    } else {
      val process = childProcessMod.spawn(command.head, command.tail.toJSArray, SpawnOptions().setStdio(IOType.inherit))
      val p = Promise[CommandOutput]()
      process.on_close(
        close,
        (code, _) =>
          p.success(CommandOutput(code match {
            case null      => None
            case s: Double => Some(s.asInt)
          }))
      )
      p.future
    }

  override def listFiles(path: String): Future[Seq[String]] =
    fsPromisesMod
      .readdir(path)
      .map(_.toSeq.map(file => pathMod.join(path, file)))

  override def isDirectory(path: String): Future[Boolean] =
    fsPromisesMod.lstat(path).map(_.isDirectory())
}
