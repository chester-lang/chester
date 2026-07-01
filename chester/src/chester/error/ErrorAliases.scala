package chester

package object error {
  type Span = chester.Span
  val Span = chester.Span
  type SpanOptional = chester.SpanOptional
  type SpanRequired = chester.SpanRequired
  type SpanOptional0 = chester.SpanOptional0

  type Reporter[-T] = chester.Reporter[T]
  val Reporter = chester.Reporter
  type VectorReporter[T] = chester.VectorReporter[T]
}
