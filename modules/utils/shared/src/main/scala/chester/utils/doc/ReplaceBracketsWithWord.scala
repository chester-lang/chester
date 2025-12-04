package chester.utils.doc

// will work better with Windows Narrator on Windows Terminal
case object ReplaceBracketsWithWord extends DocConfKey[Boolean] {
  val default: Boolean = false
}
