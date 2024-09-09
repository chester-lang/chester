package chester.repl

import chester.parser.InputStatus.*
import org.jline.reader.*
import org.jline.reader.impl.DefaultParser
import org.jline.reader.impl.history.DefaultHistory
class JLineTerminal(init: TerminalInit) {
  private val terminal = org.jline.terminal.TerminalBuilder.terminal()
  private val history = new org.jline.reader.impl.history.DefaultHistory()

  private def createParser(info: TerminalInfo): org.jline.reader.Parser = new org.jline.reader.impl.DefaultParser() {
    override def parse(line: String, cursor: Int, context: org.jline.reader.Parser.ParseContext): org.jline.reader.ParsedLine = {
      info.checkInputStatus(line) match {
        case Complete => super.parse(line, cursor, context)
        case Incomplete => throw new org.jline.reader.EOFError(-1, cursor, "Incomplete input, missing matching bracket")
        case Error(message) => super.parse(line, cursor, context)
      }
    }
  }

  def readLine(info: TerminalInfo): ReadLineResult = {
    val parser = createParser(info)
    val reader: org.jline.reader.LineReader = org.jline.reader.LineReaderBuilder.builder()
      .terminal(terminal)
      .history(history)
      .parser(parser)
      .build()

    var prompt = info.defaultPrompt
    var continue = true
    var result: ReadLineResult = EndOfFile

    while (continue) {
      try {
        var line = reader.readLine(prompt.render)
        history.add(line)
        while (line.forall(_.isWhitespace)) {
          line = reader.readLine(prompt.render)
          history.add(line)
        }

        val status = chester.parser.ParserEngine.checkInputStatus(line)

        status match {
          case Complete =>
            val historySeq = (0 until history.size()).map(history.get(_).toString)
            result = LineRead(line)
            continue = false
          case Incomplete =>
            prompt = info.continuationPrompt
          case Error(message) =>
            result = StatusError(message)
            continue = false
        }
      } catch {
        case _: org.jline.reader.EOFError =>
          result = LineRead("")
          continue = false
        case _: org.jline.reader.UserInterruptException =>
          result = UserInterrupted
          continue = false
        case _: org.jline.reader.EndOfFileException =>
          result = EndOfFile
          continue = false
      }
    }
    result
  }

  def close(): Unit = terminal.close()

  def getHistory: Seq[String] = (0 until history.size()).map(history.get(_).toString)
}