package chester.io

import chester.repl.{NodejsSimpleTerminal, ReadLineResult, TerminalInfo, TerminalInit}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

implicit object DefaultTerminal extends Terminal[Future] {
  override def runTerminal[T](init: TerminalInit, block: InTerminal[Future] ?=> Future[T]): Future[T] = {
    val terminal = new NodejsSimpleTerminal(init)
    val future =
      block(using new InTerminal[Future] {
        @inline
        inline override def readline(info: TerminalInfo): Future[ReadLineResult] = terminal.readLine(info)

        @inline
        inline override def getHistory: Future[Seq[String]] = Future.successful(terminal.getHistory)
      })
    future.flatMap { result =>
      terminal.close()
      Future.successful(result)
    }
  }

}
