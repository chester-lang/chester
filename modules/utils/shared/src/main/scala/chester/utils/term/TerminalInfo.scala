package chester.utils.term

trait TerminalInfo {
  def checkInputStatus(input: String): InputStatus

  def defaultPrompt: fansi.Str

  def continuationPrompt: fansi.Str
}
