package chester.utils.io.impl

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import chester.utils.io.*
import typings.node.processMod

given DefaultSpawn: Spawn[Future] {
  override inline def spawn(x: => Future[Unit]): Unit = x.recover { e =>
    e.printStackTrace()
    processMod.^.exit(1)
  }
}
