package chester.utils.doc

trait Styling
object Styling {
  case object BoldOn extends Styling
}

extension (d: chester.Doc) {
  def styled(style: Styling): chester.Doc = d
}
