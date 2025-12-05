package chester.utils

class Parameter[T](default: Option[T] = None) {
  private var tl: Option[T] = default

  def withValue[U](value: T)(block: => U): U = {
    val previousValue = tl
    try {
      tl = Some(value)
      block
    } finally tl = previousValue
  }

  /** Gets the current value, throwing if none is set (neither scoped nor default).
    */
  def get: T = tl.getOrElse(throw new IllegalStateException("No value set for Parameter"))

  /** Gets the current value, or returns the provided default if none is set.
    */
  def getOrElse(defaultVal: => T): T = tl.getOrElse(defaultVal)

  /** Gets the current value as an Option.
    */
  def getOption: Option[T] = tl
}

object Parameter {
  def apply[T](): Parameter[T] = new Parameter(None)
  def withDefault[T](default: T): Parameter[T] = new Parameter(Some(default))
}
