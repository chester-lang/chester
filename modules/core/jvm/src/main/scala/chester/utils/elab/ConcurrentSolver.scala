package chester.utils.elab

import chester.utils.Parameter
import chester.utils.cell.{CellContent, CellContentR, OnceCellContent, MutableCellContent, CollectionCellContent, MappingCellContent, LiteralCellContent}

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

val parameterTxn = new Parameter[InTxn]()

// Helper functions
private def optionalAtom[T](f: InTxn ?=> T): T =
  if (parameterTxn.getOption.isEmpty) {
    atomic { implicit txn: InTxn =>
      parameterTxn.withValue(txn)(f(using txn))
    }
  } else {
    f(using parameterTxn.get)
  }

private def assumeNotInAtom(): Unit =
  if (parameterTxn.getOption.isDefined) {
    throw new IllegalStateException("in transaction")
  }

private def atom[T](f: InTxn ?=> T): T =
  atomic { implicit txn: InTxn =>
    parameterTxn.withValue(txn)(f(using txn))
  }

// Solver instance - just holds state
final class ConcurrentSolverInstance(val conf: HandlerConf[ConcurrentSolverModule.type]) {
  implicit def inTxn: InTxn = parameterTxn.getOrElse(throw new IllegalStateException("not in transaction"))
  
  val pool = new ForkJoinPool()
  val delayedConstraints = Ref(Vector[WaitingConstraint]())
}

// ML-style module - implements all operations
object ConcurrentSolverModule extends SolverModule {
  // Define Cell type as ConcurrentCell
  override type Cell[+A, -B, +C <: CellContent[A, B]] = ConcurrentCell[A, B, C]
  
  // Define Solver instance type
  override type Solver = ConcurrentSolverInstance
  
  override def makeSolver[Ops](conf: HandlerConf[this.type]): Solver =
    new ConcurrentSolverInstance(conf)
  
  // Helper methods
  private def peakCell[T](id: CellR[T]): CellContentR[T] = optionalAtom {
    id.storeRefAs[CellContentR[T]].get
  }

  private def updateCell[A, B](solver: Solver, id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = optionalAtom {
    val ref = id.storeRefAs[CellContent[A, B]]
    val current = ref.get
    val newValue = f(current)
    if (current != newValue) {
      ref.set(newValue)
      val (related, others) = solver.delayedConstraints.get.partition(_.related(id))
      solver.delayedConstraints.set(others)
      addConstraints(solver, related.map(_.x))
    }
  }
  
  // Implement module interface
  override def hasStableValue(solver: Solver, id: CellAny): Boolean = 
    peakCell(id.asInstanceOf[CellR[Any]]).hasStableValue

  override def noStableValue(solver: Solver, id: CellAny): Boolean = 
    peakCell(id.asInstanceOf[CellR[Any]]).noStableValue

  override def readStable[U](solver: Solver, id: CellR[U]): Option[U] = 
    peakCell(id).readStable

  override def hasSomeValue(solver: Solver, id: CellAny): Boolean = 
    peakCell(id.asInstanceOf[CellR[Any]]).hasSomeValue

  override def noAnyValue(solver: Solver, id: CellAny): Boolean = 
    peakCell(id.asInstanceOf[CellR[Any]]).noAnyValue

  override def readUnstable[U](solver: Solver, id: CellR[U]): Option[U] = 
    peakCell(id).readUnstable

  override def fill[T](solver: Solver, id: CellW[T], value: T): Unit = 
    updateCell(solver, id.asInstanceOf[CellOf[Any, Any]], _.fill(value))
  
  override def newOnceCell[T](solver: Solver, default: Option[T] = None): OnceCell[T] = 
    ConcurrentCell(OnceCellContent[T](None, default))
  
  override def newMutableCell[T](solver: Solver, initial: Option[T] = None): MutableCell[T] = 
    ConcurrentCell(MutableCellContent[T](initial))
  
  override def newCollectionCell[A](solver: Solver): CollectionCell[A] = 
    ConcurrentCell(CollectionCellContent[A, A]())
  
  override def newMapCell[K, V](solver: Solver): MapCell[K, V] = 
    ConcurrentCell(MappingCellContent[K, V]())
  
  override def newLiteralCell[T](solver: Solver, value: T): LiteralCell[T] = 
    ConcurrentCell(LiteralCellContent(value))

  override def addCell[A, B, C <: CellContent[A, B]](solver: Solver, cell: C): Cell[A, B, C] = 
    ConcurrentCell(cell)
  
  override def addConstraint(solver: Solver, x: Constraint): Unit = {
    val handler = x.cached(solver.conf.getHandler(x.kind).getOrElse(throw new IllegalStateException(s"no handler for ${x.kind}")))
    solver.pool.execute { () =>
      atom {
        val result = handler.run(x.asInstanceOf[handler.kind.Of])(using this, solver)
        result match {
          case Result.Done => ()
          case Result.Waiting(vars @ _*) =>
            val delayed = WaitingConstraint(vars.toVector, x)
            solver.delayedConstraints.set(solver.delayedConstraints.get.appended(delayed))
        }
      }
    }
  }

  override def stable(solver: Solver): Boolean = {
    assumeNotInAtom()
    if (atom(solver.delayedConstraints.get).nonEmpty) return false
    if (solver.pool.isShutdown) return true
    if (solver.pool.isQuiescent) {
      finish(solver)
      return true
    }
    false
  }

  private def finish(solver: Solver): Unit = {
    assumeNotInAtom()
    atom {
      assume(solver.delayedConstraints.get.isEmpty)
      assume(solver.pool.isQuiescent)
    }
    val tasks = solver.pool.shutdownNow()
    assume(tasks.isEmpty)
  }

  private def entropy(solver: Solver) = optionalAtom {
    solver.delayedConstraints.get.map(c => c.x.kind.hashCode() << 8 + c.x.hashCode()).sorted.toVector
  }

  private def inPoolTickStage1(solver: Solver, zonkLevel: DefaultingLevel): Unit = {
    assumeNotInAtom()
    val delayed = atom {
      solver.delayedConstraints.get
    }
    delayed.foreach(x => doDefaulting(solver, x, zonkLevel))
  }

  override def run(solver: Solver): Unit = boundary[Unit] { outer ?=>
    assumeNotInAtom()
    while (true) boundary[Unit] { inner ?=>
      assume(!solver.pool.isShutdown)
      val _ = solver.pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
      assume(solver.pool.isQuiescent)
      if (atom{ solver.delayedConstraints.get }.isEmpty) {
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
        if (atom{ solver.delayedConstraints.get }.isEmpty) {
          finish(solver)
          boundary.break()(using outer)
        }
        if (entropy(solver) != entropyBefore) boundary.break()(using inner)
      }
      throw new IllegalStateException("cannot finish")
    }
  }

  private def doDefaulting(solver: Solver, delayed: WaitingConstraint, zonkLevel: DefaultingLevel): Unit = {
    assumeNotInAtom()
    val x = delayed.x
    val handler = x.cached(solver.conf.getHandler(x.kind).getOrElse(throw new IllegalStateException("no handler")))
    if (!handler.canDefaulting(zonkLevel)) {
      return
    }
    solver.pool.execute { () =>
      atom {
        val result = handler.run(x.asInstanceOf[handler.kind.Of])(using this, solver)
        result match {
          case Result.Done => solver.delayedConstraints.set(solver.delayedConstraints.get.filterNot(_ == delayed))
          case Result.Waiting(vars @ _*) =>
            if (handler.defaulting(x.asInstanceOf[handler.kind.Of], zonkLevel)(using this, solver)) {
              val result = handler.run(x.asInstanceOf[handler.kind.Of])(using this, solver)
              result match {
                case Result.Done => solver.delayedConstraints.set(solver.delayedConstraints.get.filterNot(_ == delayed))
                case Result.Waiting(vars2 @ _*) =>
                  val delayed1 = WaitingConstraint(vars2.toVector, x)
                  val got = solver.delayedConstraints.get
                  val delayedConstraints1 = got.filterNot(_ == delayed)
                  if (delayedConstraints1.length < got.length) {
                    solver.delayedConstraints.set(delayedConstraints1.appended(delayed1))
                  }
              }
            } else {
              val delayed1 = WaitingConstraint(vars.toVector, x)
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
