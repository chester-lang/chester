package chester.utils.io

import chester.utils.io.impl.given
import chester.utils.term.*
import fansi.Str
import typings.node.{processMod, readlineMod}

import scala.concurrent.{Future, Promise}

class NodejsSimpleTerminal(init: TerminalInit) extends AbstractInTerminal[Future] {
  private val rl = readlineMod.createInterface(
    processMod.^.stdin.asInstanceOf,
    processMod.^.stdout.asInstanceOf
  )

  private var live: Boolean = true

  private var reading: Promise[String] | Null = null

  private def closeCallback(): Unit =
    if (live) {
      reading match {
        case null => ()
        case r =>
          reading = null
          r.success("")
      }
    }

  rl.on("close", x => closeCallback())

  override def readALine(prompt: fansi.Str): Future[String] = {
    assert(reading == null)
    assert(live)
    val p = Promise[String]()
    reading = p
    rl.question(
      prompt.render,
      { result =>
        assert(reading == p)
        reading = null
        p.success(result)
      }
    )
    p.future
  }

  def initHistory: Future[Seq[String]] = Future.successful(Vector())

  def close(): Unit = {
    live = false
    rl.close()
  }

  override def writeln(line: Str): Future[Unit] =
    Future.successful(println(line.render))
}
