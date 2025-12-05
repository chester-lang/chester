package chester.utils.elab

import chester.utils.cell.{CellContent, CellContentR}

import scala.language.implicitConversions

trait SolverOps {
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

  def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C]
  def fill[T](id: CellW[T], value: T): Unit
}

object SolverOps {
  def callConstraint[A](x: ConstraintResult[A])(using ops: SolverOps): A = ops.callConstraint(x)
  def addConstraint(x: Constraint)(using ops: SolverOps): Unit = ops.addConstraint(x)
  def hasSomeValue(id: CellAny)(using ops: SolverOps): Boolean = ops.hasSomeValue(id)
  def readUnstable[U](id: CellR[U])(using ops: SolverOps): Option[U] = ops.readUnstable(id)
  def hasStableValue(id: CellAny)(using ops: SolverOps): Boolean = ops.hasStableValue(id)
  def readStable[U](id: CellR[U])(using ops: SolverOps): Option[U] = ops.readStable(id)
  def addCell[A, B, C <: CellContent[A, B]](cell: C)(using ops: SolverOps): Cell[A, B, C] = ops.addCell(cell)
  def fill[T](id: CellW[T], value: T)(using ops: SolverOps): Unit = ops.fill(id, value)
}

extension [T](id: CellW[T]) {
  def fill(value: T)(using ops: SolverOps): Unit = ops.fill(id, value)
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
