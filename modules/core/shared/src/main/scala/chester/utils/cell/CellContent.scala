package chester.utils.cell

import chester.i18n.*
trait CellContent[+A, -B] {
  def default: Option[A] = None

  /** stable means can only change once from None to a fixed Some value or always be a fixed value
    */
  def readStable: Option[A]

  def readUnstable: Option[A] = readStable

  def hasStableValue: Boolean = readStable.isDefined

  def noStableValue: Boolean = !hasStableValue

  def hasSomeValue: Boolean = readUnstable.isDefined

  def noAnyValue: Boolean = !hasSomeValue

  /** fill an unstable cell */
  def fill(newValue: B): CellContent[A, B]
}
type CellContentRW[A] = CellContent[A, A]
type CellContentR[+A] = CellContent[A, Nothing]
type CellContentW[-A] = CellContent[Any, A]

trait SeqCellContent[+A, -B] extends UnstableCellContent[Seq[A], Seq[B]] with NoFill[Seq[A], Seq[B]] {
  def add(newValue: B): SeqCellContent[A, B]
}

trait BaseMapCell[A, B] {
  def add(key: A, value: B): BaseMapCell[A, B]
}

trait UnstableCellContent[+A, -B] extends CellContent[A, B] {
  override def readStable: Option[A] = {
    throw new UnsupportedOperationException(
      t"${getClass.getName} is not stable"
    )
  }

  override def hasStableValue: Boolean = {
    throw new UnsupportedOperationException(
      t"${getClass.getName} is not stable"
    )
  }

  override def noStableValue: Boolean = {
    throw new UnsupportedOperationException(
      t"${getClass.getName} is not stable"
    )
  }
}

trait NoFill[+A, -B] extends CellContent[A, B] {
  override def fill(newValue: B): NoFill[A, B] = {
    throw new UnsupportedOperationException(
      t"${getClass.getName} cannot be filled"
    )
  }
}

trait MapCellContent[A, B] extends UnstableCellContent[Map[A, B], Map[A, B]] with BaseMapCell[A, B] with NoFill[Map[A, B], Map[A, B]] {}

object OnceCellContent:
  private val EnableDiagnostics = false

case class OnceCellContent[T](
    value: Option[T] = None,
    override val default: Option[T] = None
) extends CellContentRW[T] {
  override def readStable: Option[T] = value

  override def fill(newValue: T): OnceCellContent[T] = {
    if (value.contains(newValue)) return this
    if (value.nonEmpty && OnceCellContent.EnableDiagnostics) {
      println(s"[diag] conflicting fill: existing=${value.get} new=$newValue")
    }
    require(value.isEmpty, s"OnceCellContent can only be filled once, but was already filled with ${value.get} and now with $newValue")
    copy(value = Some(newValue))
  }
}

case class MutableCellContent[T](value: Option[T]) extends CellContentRW[T] {
  override def readStable: Option[T] = value

  override def fill(newValue: T): MutableCellContent[T] = {
    copy(value = Some(newValue))
  }
}

case class CollectionCellContent[+A, -B <: A](value: Vector[A] = Vector.empty) extends SeqCellContent[A, B] {
  override def readUnstable: Option[Vector[A]] = Some(value)

  override def add(newValue: B): CollectionCellContent[A, B] = {
    copy(value = value :+ newValue)
  }
}

case class MappingCellContent[A, B](value: Map[A, B] = Map.empty[A, B]) extends MapCellContent[A, B] {
  override def readStable: Option[Map[A, B]] = Some(value)

  override def add(key: A, newValue: B): MappingCellContent[A, B] = {
    copy(value = value + (key -> newValue))
  }
}

case class LiteralCellContent[T](value: T) extends CellContentRW[T] {
  override def readStable: Option[T] = Some(value)

  override def hasStableValue: Boolean = true

  override def fill(newValue: T): LiteralCellContent[T] = {
    throw new UnsupportedOperationException("LiteralCell cannot be filled")
  }
}

/** A cell that automatically provides a default value during zonking if no other propagator fills it. This is used to avoid "not covered by any
  * propagator" errors for cells that are allowed to have default values.
  *
  * @param defaultValue
  *   The default value to use if no other propagator fills this cell
  * @param value
  *   The current value (if any)
  */
case class DefaultValueCellContent[T](
    defaultValue: T,
    value: Option[T] = None
) extends CellContentRW[T] {
  override def readStable: Option[T] = value

  override val default: Option[T] = Some(defaultValue)

  override def fill(newValue: T): DefaultValueCellContent[T] = {
    copy(value = Some(newValue))
  }
}
