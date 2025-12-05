package chester.utils.elab

/** implementations should be case object */
trait Kind {
  def name: String = toString
  type Of <: Constraint
}

trait Constraint(val kind: Kind) {
  private[chester] final var cache: Handler[Nothing, Kind] | Null = null
  private[chester] final def cached[Ops, K <: Kind](x: => Handler[Ops, K]): Handler[Ops, K] = {
    val c = cache
    if (c != null) {
      c.asInstanceOf[Handler[Ops, K]]
    } else {
      val result = x
      cache = result
      result
    }
  }
}

trait ConstraintResult[+A] extends Constraint {
  def result: A
}
