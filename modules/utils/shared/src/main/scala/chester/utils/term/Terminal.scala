package chester.utils.term

trait Terminal[F[_]] {
  def runTerminal[T](init: TerminalInit, block: InTerminal[F] ?=> F[T]): F[T]
}

object Terminal {
  inline def runTerminal[F[_], T](inline init: TerminalInit)(
      inline block: InTerminal[F] ?=> F[T]
  )(using inline terminal: Terminal[F]): F[T] = {
    terminal.runTerminal(init, block)
  }
}

trait InTerminal[F[_]] {
  def writeln(line: fansi.Str): F[Unit]

  def readline(info: TerminalInfo): F[ReadLineResult]

  def getHistory: F[Seq[String]]
}

object InTerminal {
  inline def writeln[F[_]](inline line: fansi.Str)(using
      inline terminal: InTerminal[F]
  ): F[Unit] = terminal.writeln(line)

  inline def readline[F[_]](inline info: TerminalInfo)(using
      inline terminal: InTerminal[F]
  ): F[ReadLineResult] = terminal.readline(info)

  inline def getHistory[F[_]](using
      inline terminal: InTerminal[F]
  ): F[Seq[String]] = terminal.getHistory
}
