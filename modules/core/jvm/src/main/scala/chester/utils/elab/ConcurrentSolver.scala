package chester.utils.elab
import chester.utils.Parameter
import chester.utils.cell.{
  CellContent,
  CellContentR,
  CollectionCellContent,
  LiteralCellContent,
  MappingCellContent,
  MutableCellContent,
  OnceCellContent
}

import java.util.concurrent.{ForkJoinPool, TimeUnit}
import scala.language.implicitConversions
import scala.util.boundary
import scala.concurrent.stm.*

final class ConcurrentCell[+A, -B, +C <: CellContent[A, B]](
    initialValue: C
) extends Cell[A, B, C] {
  private[elab] val _storeRef: Ref[Any] = Ref(initialValue)
  private[elab] inline def storeRefAs[T]: Ref[T] = _storeRef.asInstanceOf[Ref[T]]
}

// Solver instance - just holds state, parameterized by constraint type
final class ConcurrentSolverInstance[C](val conf: HandlerConf[C, ConcurrentSolverModule.type]) {
  val pool = new ForkJoinPool()
  val delayedConstraints: Ref[Vector[WaitingConstraint]] = Ref(Vector[WaitingConstraint]())
}

// ML-style module - implements all operations
object ConcurrentSolverModule extends SolverModule {
  // Define Cell type as ConcurrentCell
  override type Cell[+A, -B, +C <: CellContent[A, B]] = ConcurrentCell[A, B, C]

  // Define Solver instance type - parameterized by constraint type
  override type Solver[C] = ConcurrentSolverInstance[C]

  override def makeSolver[C](conf: HandlerConf[C, this.type]): Solver[C] =
    new ConcurrentSolverInstance[C](conf)

  // Helper methods
  private def peakCell[T](id: CellR[T]): CellContentR[T] = atomic(implicit txn => id.storeRefAs[CellContentR[T]].get)

  private def updateCell[C, A, B](solver: Solver[C], id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    val relatedConstraints = atomic { implicit txn =>
      val ref = id.storeRefAs[CellContent[A, B]]
      val current = ref.get
      val newValue = f(current)
      if (current != newValue) {
        ref.set(newValue)
        val (related, others) = solver.delayedConstraints.get.partition(_.related(id))
        solver.delayedConstraints.set(others)
        related.map(_.constraint)
      } else {
        Vector.empty[Any]
      }
    }
    if (relatedConstraints.nonEmpty) {
      addConstraints(solver, relatedConstraints.asInstanceOf[Seq[C]])
    }
  }

  // Implement module interface
  override def hasStableValue[C](solver: Solver[C], id: CellAny): Boolean =
    peakCell(id.asInstanceOf[CellR[Any]]).hasStableValue

  override def noStableValue[C](solver: Solver[C], id: CellAny): Boolean =
    peakCell(id.asInstanceOf[CellR[Any]]).noStableValue

  override def readStable[C, U](solver: Solver[C], id: CellR[U]): Option[U] =
    peakCell(id).readStable

  override def hasSomeValue[C](solver: Solver[C], id: CellAny): Boolean =
    peakCell(id.asInstanceOf[CellR[Any]]).hasSomeValue

  override def noAnyValue[C](solver: Solver[C], id: CellAny): Boolean =
    peakCell(id.asInstanceOf[CellR[Any]]).noAnyValue

  override def readUnstable[C, U](solver: Solver[C], id: CellR[U]): Option[U] =
    peakCell(id).readUnstable

  override def fill[C, T](solver: Solver[C], id: CellW[T], value: T): Unit =
    updateCell(solver, id.asInstanceOf[CellOf[Any, Any]], _.fill(value))

  override def newOnceCell[C, T](solver: Solver[C], default: Option[T] = None): OnceCell[T] =
    ConcurrentCell(OnceCellContent[T](None, default))

  override def newMutableCell[C, T](solver: Solver[C], initial: Option[T] = None): MutableCell[T] =
    ConcurrentCell(MutableCellContent[T](initial))

  override def newCollectionCell[C, A](solver: Solver[C]): CollectionCell[A] =
    ConcurrentCell(CollectionCellContent[A, A]())

  override def newMapCell[C, K, V](solver: Solver[C]): MapCell[K, V] =
    ConcurrentCell(MappingCellContent[K, V]())

  override def newLiteralCell[C, T](solver: Solver[C], value: T): LiteralCell[T] =
    ConcurrentCell(LiteralCellContent(value))

  override def addCell[C, A, B, CC <: CellContent[A, B]](solver: Solver[C], cell: CC): Cell[A, B, CC] =
    ConcurrentCell(cell)

  override def addConstraint[C](solver: Solver[C], constraint: C): Unit = {
    val handler = solver.conf.getHandler(constraint).getOrElse(throw new IllegalStateException(s"no handler for $constraint"))
    solver.pool.execute { () =>
      atomic { implicit txn =>
        val result = handler.run(constraint)(using this, solver)
        result match {
          case Result.Done => ()
          case Result.Waiting(vars @ _*) =>
            val delayed = WaitingConstraint(vars.toVector, constraint)
            solver.delayedConstraints.set(solver.delayedConstraints.get.appended(delayed))
        }
      }
    }
  }

  override def stable[C](solver: Solver[C]): Boolean = {
    val hasDelayed = atomic(implicit txn => solver.delayedConstraints.get).nonEmpty
    if (hasDelayed) {
      false
    } else if (solver.pool.isShutdown) {
      true
    } else if (solver.pool.isQuiescent) {
      finish(solver)
      true
    } else {
      false
    }
  }

  private def finish[C](solver: Solver[C]): Unit = {
    atomic { implicit txn =>
      assume(solver.delayedConstraints.get.isEmpty)
      assume(solver.pool.isQuiescent)
    }
    val tasks = solver.pool.shutdownNow()
    assume(tasks.isEmpty)
  }

  private def entropy[C](solver: Solver[C]) = atomic(implicit txn => solver.delayedConstraints.get.map(c => c.constraint.hashCode()).sorted.toVector)

  private def inPoolTickStage1[C](solver: Solver[C], zonkLevel: DefaultingLevel): Unit = {
    val delayed = atomic(implicit txn => solver.delayedConstraints.get)
    delayed.foreach(x => doDefaulting(solver, x, zonkLevel))
  }

  override def run[C](solver: Solver[C]): Unit = boundary[Unit] { outer ?=>
    while (true) boundary[Unit] { inner ?=>
      assume(!solver.pool.isShutdown)
      val _ = solver.pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
      assume(solver.pool.isQuiescent)
      if (atomic(implicit txn => solver.delayedConstraints.get).isEmpty) {
        finish(solver)
        boundary.break()(using outer)
      }
      for (level <- DefaultingLevel.Values) {
        val entropyBefore = entropy(solver)
        assume(!solver.pool.isShutdown)
        assume(solver.pool.isQuiescent)
        solver.pool.execute(() => inPoolTickStage1(solver, level))
        val _ = solver.pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
        assume(solver.pool.isQuiescent)
        if (atomic(implicit txn => solver.delayedConstraints.get).isEmpty) {
          finish(solver)
          boundary.break()(using outer)
        }
        if (entropy(solver) != entropyBefore) boundary.break()(using inner)
      }
      throw new IllegalStateException("cannot finish")
    }
  }

  private def doDefaulting[C](solver: Solver[C], delayed: WaitingConstraint, zonkLevel: DefaultingLevel): Unit = {
    val constraint = delayed.constraint.asInstanceOf[C]
    val handler = solver.conf.getHandler(constraint).getOrElse(throw new IllegalStateException("no handler"))
    if (!handler.canDefaulting(zonkLevel)) {
      return // OK to use return here - simple early exit
    }
    solver.pool.execute { () =>
      atomic { implicit txn =>
        val result = handler.run(constraint)(using this, solver)
        result match {
          case Result.Done => solver.delayedConstraints.set(solver.delayedConstraints.get.filterNot(_ == delayed))
          case Result.Waiting(vars @ _*) =>
            if (handler.defaulting(constraint, zonkLevel)(using this, solver)) {
              val result = handler.run(constraint)(using this, solver)
              result match {
                case Result.Done => solver.delayedConstraints.set(solver.delayedConstraints.get.filterNot(_ == delayed))
                case Result.Waiting(vars2 @ _*) =>
                  val delayed1 = WaitingConstraint(vars2.toVector, constraint)
                  val got = solver.delayedConstraints.get
                  val delayedConstraints1 = got.filterNot(_ == delayed)
                  if (delayedConstraints1.length < got.length) {
                    solver.delayedConstraints.set(delayedConstraints1.appended(delayed1))
                  }
              }
            } else {
              val delayed1 = WaitingConstraint(vars.toVector, constraint)
              val got = solver.delayedConstraints.get
              val delayedConstraints1 = got.filterNot(_ == delayed)
              if (delayedConstraints1.length < got.length) {
                solver.delayedConstraints.set(delayedConstraints1.appended(delayed1))
              }
            }
        }
      }
    }
  }
}
