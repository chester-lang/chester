package chester.utils.elab

import chester.utils.Parameter
import chester.utils.cell.{OnceCellContent, MutableCellContent, CollectionCellContent, MappingCellContent, LiteralCellContent, *}

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

object ConcurrentSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ConcurrentSolver(conf)
}

// might be inside an transaction might be not if not wrapped in an atomic block
private def optionalAtom[T](f: => T): T =
  if (parameterTxn.getOption.isEmpty) {
    atom {
      f
    }
  } else {
    f
  }

private def assumeNotInAtom(): Unit =
  if (parameterTxn.getOption.isDefined) {
    throw new IllegalStateException(" in transaction")
  }

private def atom[T](f: => T): T =
  atomic { implicit txn: InTxn =>
    parameterTxn.withValue(txn) {
      f
    }
  }

final class ConcurrentSolver[Ops](val conf: HandlerConf[Ops])(using Ops) extends BasicSolverOps {

  // Define Cell type using type lambda to match variance in interface  
  override type Cell[+A, -B, +C <: CellContent[A, B]] = ConcurrentCell[A, B, C]

  implicit def inTxn: InTxn = parameterTxn.getOrElse(throw new IllegalStateException("not in transaction"))

  override protected def peakCell[T](id: CellR[T]): CellContentR[T] = optionalAtom {
    id.storeRefAs[CellContentR[T]].get
  }
  
  override def newOnceCell[T](default: Option[T] = None): OnceCell[T] = 
    ConcurrentCell(OnceCellContent[T](None, default))
  
  override def newMutableCell[T](initial: Option[T] = None): MutableCell[T] = 
    ConcurrentCell(MutableCellContent[T](initial))
  
  override def newCollectionCell[A]: CollectionCell[A] = 
    ConcurrentCell(CollectionCellContent[A, A]())
  
  override def newMapCell[K, V]: MapCell[K, V] = 
    ConcurrentCell(MappingCellContent[K, V]())
  
  override def newLiteralCell[T](value: T): LiteralCell[T] = 
    ConcurrentCell(LiteralCellContent(value))

  given SolverOps = this
  private val pool = new ForkJoinPool()
  private val delayedConstraints = Ref(Vector[WaitingConstraint]())

  private def entropy() = optionalAtom {
    delayedConstraints.get.map(c => c.x.kind.hashCode() << 8 + c.x.hashCode()).sorted.toVector
  }
  override def stable: Boolean = {
    assumeNotInAtom()
    if (atom(delayedConstraints.get).nonEmpty) return false
    if (pool.isShutdown) return true
    if (pool.isQuiescent) {
      finish()
      return true
    }
    false
  }

  private def finish(): Unit = {
    assumeNotInAtom()
    atom {
      assume(delayedConstraints.get.isEmpty)
      assume(pool.isQuiescent)
    }
    val tasks = pool.shutdownNow()
    assume(tasks.isEmpty)
  }

  private def inPoolTickStage1(zonkLevel: DefaultingLevel): Unit = {
    assumeNotInAtom()
    val delayed = atom {
      delayedConstraints.get
    }
    delayed.foreach(x => doDefaulting(x, zonkLevel))
  }
  override def run(): Unit = boundary[Unit] { outer ?=>
    assumeNotInAtom()
    while (true) boundary[Unit] { inner ?=>
      assume(!pool.isShutdown)
      val _ = pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
      assume(pool.isQuiescent)
      if (atom(delayedConstraints.get).isEmpty) {
        finish()
        boundary.break()(using outer)
      }
      for (level <- DefaultingLevel.Values) {
        val entropyBefore = entropy()
        assume(!pool.isShutdown)
        assume(pool.isQuiescent)
        pool.execute(() => inPoolTickStage1(level))
        val _ = pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
        assume(pool.isQuiescent)
        if (atom(delayedConstraints.get).isEmpty) {
          finish()
          boundary.break()(using outer)
        }
        if (entropy() != entropyBefore) boundary.break()(using inner)
      }
      throw new IllegalStateException("cannot finish")
    }
  }

  private def doDefaulting(delayed: WaitingConstraint, zonkLevel: DefaultingLevel): Unit = {
    assumeNotInAtom()
    val x = delayed.x
    val handler = x.cached(conf.getHandler(x.kind).getOrElse(throw new IllegalStateException("no handler")))
    if (!handler.canDefaulting(zonkLevel)) {
      return
    }
    pool.execute { () =>
      atom {
        val result = handler.run(x.asInstanceOf[handler.kind.Of])
        result match {
          case Result.Done => delayedConstraints.set(delayedConstraints.get.filterNot(_ == delayed))
          case Result.Waiting(vars*) =>
            if (handler.defaulting(x.asInstanceOf[handler.kind.Of], zonkLevel)) {
              val result = handler.run(x.asInstanceOf[handler.kind.Of])
              result match {
                case Result.Done => delayedConstraints.set(delayedConstraints.get.filterNot(_ == delayed))
                case Result.Waiting(vars*) =>
                  val delayed1 = WaitingConstraint(vars.toVector, x)
                  val got = delayedConstraints.get
                  val delayedConstraints1 = got.filterNot(_ == delayed)
                  if (delayedConstraints1.length < got.length) {
                    delayedConstraints.set(delayedConstraints1.appended(delayed1))
                  }
              }
            } else {
              val delayed1 = WaitingConstraint(vars.toVector, x)
              val got = delayedConstraints.get
              val delayedConstraints1 = got.filterNot(_ == delayed)
              if (delayedConstraints1.length < got.length) {
                delayedConstraints.set(delayedConstraints1.appended(delayed1))
              }
            }
        }
      }
    }
  }

  override def addConstraint(x: Constraint): Unit =
    val handler = x.cached(conf.getHandler(x.kind).getOrElse(throw new IllegalStateException(s"no handler for ${x.kind}")))
    pool.execute { () =>
      atom {
        val result = handler.run(x.asInstanceOf[handler.kind.Of])
        result match {
          case Result.Done => ()
          case Result.Waiting(vars*) =>
            val delayed = WaitingConstraint(vars.toVector, x)
            delayedConstraints.set(delayedConstraints.get.appended(delayed))
        }
      }
    }

  override protected def updateCell[A, B](id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    val ref = id.storeRefAs[CellContent[A, B]]
    val current = ref.get
    val newValue = f(current)
    if (current == newValue) return
    ref.set(newValue)
    val (related, others) = delayedConstraints.get.partition(_.related(id))
    delayedConstraints.set(others)
    addConstraints(related.map(_.x))
  }

  override def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C] = ConcurrentCell(cell)
}
