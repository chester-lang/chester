package chester.utils.elab

import chester.utils.cell.CellContent

/** Top-level cell trait that all solver implementations must extend.
  * 
  * DESIGN DECISIONS:
  * 1. Variance: Cell[+A, -B, +C] allows covariant reads, contravariant writes
  *    - This prevents type errors when passing cells with different types
  *    - Concrete implementations hide mutable storage with private[elab] accessors
  * 
  * 2. Users define data structures using this top-level Cell type
  *    - SolverModule.Cell extends this, so solver-specific cells are compatible
  *    - Implicit conversions in SolverModule treat all Cells as module-specific
  * 
  * 3. Type erasure + unsafe casts at module boundaries
  *    - Since all solvers implement the same Cell interface, we can unsafely
  *      convert between Cell and M#Cell
  *    - This is safe because concrete cell types are never mixed between solvers
  */
trait Cell[+A, -B, +C <: CellContent[A, B]] {
  def tag: String = Integer.toHexString(hashCode)

  // for debugging
  val stack: Array[StackTraceElement] = Thread.currentThread.getStackTrace

  override def toString: String = s"Cell@$tag"
}

type CellOf[+A, -B] = Cell[A, B, CellContent[A, B]]
type CellRW[T] = Cell[T, T, CellContent[T, T]]
type CellAny = Cell[Any, Nothing, CellContent[Any, Nothing]]
type CellR[+T] = Cell[T, Nothing, CellContent[T, Nothing]]
type CellW[-T] = Cell[Any, T, CellContent[Any, T]]
type CellRWOr[A] = CellOf[A, A] | A
type CellROr[A] = CellOf[A, Nothing] | A
