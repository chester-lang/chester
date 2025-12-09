package chester.utils

import scala.language.implicitConversions

import cats.data.*
import upickle.default.*

implicit inline def elimNonEmptySeq[T](x: NonEmptySeq[T]): Seq[T] = x.toSeq
implicit inline def elimNonEmptyVector[T](x: NonEmptyVector[T]): Vector[T] = {
  x.toVector
}
implicit inline def convertVec[T](x: NonEmptyVector[T]): NonEmptySeq[T] = {
  NonEmptySeq.fromSeqUnsafe(x.toVector)
}

extension [T](xs: NonEmptySeq[T]) {
  inline def toVector: NonEmptyVector[T] = {
    NonEmptyVector.fromVectorUnsafe(xs.toSeq.toVector)
  }
  inline def foreach(f: T => Unit): Unit = xs.toSeq.foreach(f)
}

extension [T](x: Seq[T]) {
  inline def assumeNonEmpty: NonEmptySeq[T] = NonEmptySeq.fromSeqUnsafe(x)
}
extension [T](x: Vector[T]) {
  inline def assumeNonEmpty: NonEmptyVector[T] = {
    NonEmptyVector.fromVectorUnsafe(x)
  }
}

extension [T](x: NonEmptySeq[T]) {
  def toNonEmptyVector: NonEmptyVector[T] = {
    NonEmptyVector.fromVectorUnsafe(x.toSeq.toVector)
  }
}

implicit def nonEmptySeqRW[T: ReadWriter]: ReadWriter[NonEmptySeq[T]] = {
  readwriter[Seq[T]].bimap(_.toSeq, NonEmptySeq.fromSeqUnsafe)
}
implicit def nonEmptyVectorRW[T: ReadWriter]: ReadWriter[NonEmptyVector[T]] = {
  readwriter[Vector[T]].bimap(_.toVector, NonEmptyVector.fromVectorUnsafe)
}
