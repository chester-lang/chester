package chester.utils

import upickle.default.*

import scala.reflect.Selectable.reflectiveSelectable
// allow to write, not allow read
given holdNotReadableRW: ReadWriter[HoldNotReadable[?]] =
  readwriter[HoldNotReadableRW].bimap(
    _ => HoldNotReadableRW(),
    _ => throw new UnsupportedOperationException("Cannot read HoldNotReadable")
  )

case class HoldNotReadable[T](inner: T) {
  override def equals(obj: Any): Boolean =
    obj match {
      case HoldNotReadable(other) => (inner eq other) || inner == other
      case _                      => false
    }
}

private case class HoldNotReadableRW() derives ReadWriter

case class HoldOptionNoRead[T](inner: Option[T]) extends AnyVal {
  def get: T = inner.get
}

given HoldOptionNoReadRW[T]: ReadWriter[HoldOptionNoRead[T]] =
  readwriter[HoldNotReadableRW].bimap(
    _ => HoldNotReadableRW(),
    _ => HoldOptionNoRead(None)
  )
