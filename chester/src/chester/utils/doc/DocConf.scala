package chester.utils.doc

trait DocConfKey[T] {
  def default: T
  def get(using conf: chester.DocConf): T = default
}

case object ReplaceBracketsWithWord extends DocConfKey[Boolean] {
  val default = false
}
