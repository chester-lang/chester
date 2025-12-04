package chester.utils.term

import cats.Id
import chester.utils.io.*
import fansi.Str

class SimpleTerminal(_init: TerminalInit)(using Runner[Id]) extends AbstractInTerminal[Id] {
  override inline def initHistory: Id[Seq[String]] = Vector()

  override inline def readALine(prompt: Str): String = {
    print(prompt.render)
    scala.io.StdIn.readLine()
  }

  override inline def writeln(line: Str): Unit = println(line.render)
}

class SimpleTerminalFactory(using Runner[Id]) extends Terminal[Id] {
  inline def runTerminal[T](
      init: TerminalInit,
      block: InTerminal[Id] ?=> T
  ): T = {
    val terminal = new SimpleTerminal(init)
    block(using terminal)
  }
}
