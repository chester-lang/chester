package chester.utils.term

sealed trait ReadLineResult extends Product with Serializable

case class LineRead(line: String) extends ReadLineResult

case class StatusError(message: String) extends ReadLineResult

case object UserInterrupted extends ReadLineResult

case object EndOfFile extends ReadLineResult
