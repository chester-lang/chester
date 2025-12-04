package chester.error

import scala.language.experimental.genericNumberLiterals

trait SpanOptional0 extends Any {
  def span0: Option[Span]
}

trait SpanOptional extends Any with SpanOptional0 {
  def span: Option[Span]
  override def span0: Option[Span] = span
}
