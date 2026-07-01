package chester.error

import upickle.default.*

object Problem {
  enum Stage derives ReadWriter {
    case TYCK, PARSE, OTHER
  }

  enum Severity derives ReadWriter {
    case Error, Goal, Warning, Info
  }
}

trait Problem {
  def severity: Problem.Severity
  def stage: Problem.Stage
  def message: String
  def span0: Option[Span]
}
