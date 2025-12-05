package chester.utils.elab

import chester.utils.cell.CellContent

open trait Cell[+A, -B, +C <: CellContent[A, B]] {
  def tag: String = Integer.toHexString(hashCode)

  // for debugging
  val stack = Thread.currentThread.getStackTrace

  override def toString: String = s"Cell@$tag"
}

type CellOf[+A, -B] = Cell[A, B, CellContent[A, B]]
type CellRW[T] = Cell[T, T, CellContent[T, T]]
type CellAny = Cell[Any, Nothing, CellContent[Any, Nothing]]
type CellR[+T] = Cell[T, Nothing, CellContent[T, Nothing]]
type CellW[-T] = Cell[Any, T, CellContent[Any, T]]
type CellRWOr[A] = CellOf[A, A] | A
type CellROr[A] = CellOf[A, Nothing] | A
