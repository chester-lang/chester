package chester.utils.doc

import kiama2.output.AbstractPrettyPrinter

trait StylePrettyPrinter extends AbstractPrettyPrinter {
  type Attribute = Style

  def noAttribute: Attribute = Style.Empty
}
