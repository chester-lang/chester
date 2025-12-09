package chester.utils
import com.eed3si9n.ifdef.*
@ifndef("scalaNativeNoMultithread")
class Parameter[T](default: Option[T] = None) {
  private val tl: InheritableThreadLocal[Option[T]] = {
    new InheritableThreadLocal[Option[T]] {
      override def initialValue(): Option[T] = default
    }
  }

  def withValue[U](value: T)(block: => U): U = {
    val previousValue = tl.get()
    try {
      tl.set(Some(value))
      block
    } finally tl.set(previousValue)
  }

  /** Gets the current value, throwing if none is set (neither scoped nor default).
    */
  def get: T = {
    tl.get().getOrElse(throw new IllegalStateException("No value set for Parameter"))
  }

  /** Gets the current value, or returns the provided default if none is set.
    */
  def getOrElse(defaultVal: => T): T = {
    tl.get().getOrElse(defaultVal)
  }

  /** Gets the current value as an Option.
    */
  def getOption: Option[T] = {
    tl.get()
  }
}

@ifdef("scalaNativeNoMultithread")
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

// Add companion object with factory methods
object Parameter {
  def apply[T](): Parameter[T] = new Parameter(None)
  def withDefault[T](default: T): Parameter[T] = new Parameter(Some(default))
}
