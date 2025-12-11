package chester.utils.io

import cats.Id
import chester.utils.term.*

given DefaultTerminal(using runner: Runner[Id]): Terminal[Id] with
  inline def runTerminal[T](
      init: TerminalInit,
      block: InTerminal[Id] ?=> T
  ): T = {
    val terminal = new SimpleTerminalFactory(using runner)
    terminal.runTerminal(init, block)
  }
