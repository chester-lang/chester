package chester.utils.elab

/** implementations should be case object */
trait Kind {
  def name: String = toString
  type Of <: Constraint
}

trait Constraint(val kind: Kind) {
  private[chester] final var cache: Handler[Kind] | Null = null
  private[chester] final def cached[K <: Kind](x: => Handler[K]): Handler[K] = {
    val c = cache
    if (c != null) {
      c.asInstanceOf[Handler[K]]
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
