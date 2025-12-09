package chester.utils.io

import cats.Id
import chester.repl.JLineTerminal
import chester.utils.term.*

class InTerm(terminal: JLineTerminal) extends InTerminal[Id] {
  inline def writeln(line: fansi.Str): Unit = println(line.render)

  inline def readline(info: TerminalInfo): ReadLineResult = {
    terminal.readLine(info)
  }

  inline def getHistory: Seq[String] = terminal.getHistory
}

given DefaultTerminal: Terminal[Id] {
  inline def runTerminal[T](
      init: TerminalInit,
      block: InTerminal[Id] ?=> T
  ): T = {
    val terminal = new JLineTerminal(init)
    try {
      block(using new InTerm(terminal))
    } finally {
      terminal.close()
    }
  }
}
