package chester.utils.io

import chester.utils.term.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

given DefaultTerminal: Terminal[Future] {
  override def runTerminal[T](
      init: TerminalInit,
      block: InTerminal[Future] ?=> Future[T]
  ): Future[T] = {
    val terminal = new NodejsSimpleTerminal(init)
    val future =
      block(using terminal)
    future.transform { result =>
      terminal.close()
      result
    }
  }

}
