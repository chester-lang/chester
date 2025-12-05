package chester.utils.elab

import chester.utils.cell.{CellContent, CellContentR, OnceCellContent, MutableCellContent, CollectionCellContent, MappingCellContent, LiteralCellContent}

import scala.language.implicitConversions

trait SolverOps {
  // Abstract cell type - each solver defines its own implementation
  // Variance in interface, concrete implementations hide mutable storage
  type Cell[+A, -B, +C <: CellContent[A, B]]
  
  // Derived cell type aliases
  type CellOf[+A, -B] = Cell[A, B, CellContent[A, B]]
  type CellRW[T] = Cell[T, T, CellContent[T, T]]
  type CellAny = Cell[Any, Nothing, CellContent[Any, Nothing]]
  type CellR[+T] = Cell[T, Nothing, CellContent[T, Nothing]]
  type CellW[-T] = Cell[Any, T, CellContent[Any, T]]
  
  // Specific cell types for common content types
  type OnceCell[T] = Cell[T, T, OnceCellContent[T]]
  type MutableCell[T] = Cell[T, T, MutableCellContent[T]]
  type CollectionCell[A] = Cell[Seq[A], Seq[A], CollectionCellContent[A, A]]
  type MapCell[K, V] = Cell[Map[K, V], Map[K, V], MappingCellContent[K, V]]
  type LiteralCell[T] = Cell[T, Nothing, LiteralCellContent[T]]
  
  def hasStableValue(id: CellAny): Boolean
  def noStableValue(id: CellAny): Boolean
  def readStable[U](id: CellR[U]): Option[U]
  def hasSomeValue(id: CellAny): Boolean
  def noAnyValue(id: CellAny): Boolean
  def readUnstable[U](id: CellR[U]): Option[U]

  def run(): Unit
  def stable: Boolean

  def addConstraint(x: Constraint): Unit
  def addConstraints(xs: Seq[Constraint]): Unit = xs.foreach(addConstraint)
  def callConstraint[A](x: ConstraintResult[A]): A = {
    addConstraint(x)
    x.result
  }

  // Typed cell constructors - no CellContent exposure
  def newOnceCell[T](default: Option[T] = None): OnceCell[T]
  def newMutableCell[T](initial: Option[T] = None): MutableCell[T]
  def newCollectionCell[A]: CollectionCell[A]
  def newMapCell[K, V]: MapCell[K, V]
  def newLiteralCell[T](value: T): LiteralCell[T]
  
  // Generic constructor for extensibility
  def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C]
  def fill[T](id: CellW[T], value: T): Unit
}

object SolverOps {
  def callConstraint[A](x: ConstraintResult[A])(using ops: SolverOps): A = ops.callConstraint(x)
  def addConstraint(x: Constraint)(using ops: SolverOps): Unit = ops.addConstraint(x)
  def hasSomeValue(using ops: SolverOps)(id: ops.CellAny): Boolean = ops.hasSomeValue(id)
  def readUnstable[U](using ops: SolverOps)(id: ops.CellR[U]): Option[U] = ops.readUnstable(id)
  def hasStableValue(using ops: SolverOps)(id: ops.CellAny): Boolean = ops.hasStableValue(id)
  def readStable[U](using ops: SolverOps)(id: ops.CellR[U]): Option[U] = ops.readStable(id)
  def addCell[A, B, C <: CellContent[A, B]](cell: C)(using ops: SolverOps): ops.Cell[A, B, C] = ops.addCell(cell)
  def fill[T](using ops: SolverOps)(id: ops.CellW[T], value: T): Unit = ops.fill(id, value)
}

extension [T](using ops: SolverOps)(id: ops.CellW[T]) {
  def fill(value: T): Unit = ops.fill(id, value)
}

trait BasicSolverOps extends SolverOps {
  protected def peakCell[T](id: CellR[T]): CellContentR[T]
  protected def updateCell[A, B](id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit

  override def hasStableValue(id: CellAny): Boolean = peakCell(id).hasStableValue

  override def noStableValue(id: CellAny): Boolean = peakCell(id).noStableValue

  override def readStable[U](id: CellR[U]): Option[U] = peakCell(id).readStable

  override def hasSomeValue(id: CellAny): Boolean = peakCell(id).hasSomeValue

  override def noAnyValue(id: CellAny): Boolean = peakCell(id).noAnyValue

  override def readUnstable[U](id: CellR[U]): Option[U] = peakCell(id).readUnstable

  override def fill[T](id: CellW[T], value: T): Unit = updateCell(id, _.fill(value))
}

trait SolverFactory {
  def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps
}
