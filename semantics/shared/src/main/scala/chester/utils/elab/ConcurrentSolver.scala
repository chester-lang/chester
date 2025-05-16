package chester.utils.elab

import chester.utils.Parameter
import chester.utils.cell.*

import java.util.concurrent.{ForkJoinPool, TimeUnit}
import scala.language.implicitConversions
import scala.util.boundary
import scala.concurrent.stm.*

final class ConcurrentCell[+A, -B, C <: CellContent[A, B]](
    initialValue: C
) extends Cell[A, B, C] {
  val storeRef: Ref[C] = Ref(initialValue)
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

private def atom[T](f: => T): T =
  atomic { implicit txn: InTxn =>
    parameterTxn.withValue(txn) {
      f
    }
  }

final class ConcurrentSolver[Ops](val conf: HandlerConf[Ops])(using Ops) extends BasicSolverOps {

  implicit inline def thereAreAllConcurrent[A, B, C <: CellContent[A, B]](inline x: Cell[A, B, C]): ConcurrentCell[A, B, C] =
    x.asInstanceOf[ConcurrentCell[A, B, C]]

  implicit def inTxn: InTxn = parameterTxn.getOrElse(throw new IllegalStateException("not in transaction"))

  override protected def peakCell[T](id: CellR[T]): CellContentR[T] = optionalAtom {
    id.storeRef.get
  }

  given SolverOps = this
  private val pool = new ForkJoinPool()
  private val delayedConstraints = Ref(Vector[WaitingConstraint]())

  private def entropy() = optionalAtom {
    delayedConstraints.get.map(c => c.x.kind.hashCode() << 8 + c.x.hashCode()).sorted.toVector
  }
  override def stable: Boolean = {
    if (atom(delayedConstraints.get).nonEmpty) return false
    if (pool.isShutdown) return true
    if (pool.isQuiescent) {
      finish()
      return true
    }
    false
  }

  private def finish(): Unit = {
    atom {
      assume(delayedConstraints.get.isEmpty)
      assume(pool.isQuiescent)
    }
    val tasks = pool.shutdownNow()
    assume(tasks.isEmpty)
  }

  private def inPoolTickStage1(zonkLevel: DefaultingLevel): Unit = {
    val delayed = atom {
      val delayed = delayedConstraints.get
      delayedConstraints.set(Vector.empty)
      delayed
    }
    delayed.foreach(x => doDefaulting(x, zonkLevel))
  }
  override def run(): Unit = boundary[Unit] { outer ?=>
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
    val x = delayed.x
    val handler = conf.getHandler(x.kind).getOrElse(throw new IllegalStateException("no handler"))
    if (!handler.canDefaulting(zonkLevel)) {
      atom {
        delayedConstraints.set(delayedConstraints.get.appended(delayed))
      }
      return
    }
    pool.execute { () =>
      atom {
        val result = handler.run(x.asInstanceOf[handler.kind.Of])
        result match {
          case Result.Done => ()
          case Result.Waiting(vars*) =>
            if (handler.defaulting(x.asInstanceOf[handler.kind.Of], zonkLevel)) {
              val result = handler.run(x.asInstanceOf[handler.kind.Of])
              result match {
                case Result.Done => ()
                case Result.Waiting(vars*) =>
                  val delayed = WaitingConstraint(vars.toVector, x)
                  delayedConstraints.set(delayedConstraints.get.appended(delayed))
              }
            } else {
              val delayed = WaitingConstraint(vars.toVector, x)
              delayedConstraints.set(delayedConstraints.get.appended(delayed))
            }
        }
      }
    }
  }

  override def addConstraint(x: Constraint): Unit =
    val handler = conf.getHandler(x.kind).getOrElse(throw new IllegalStateException(s"no handler for ${x.kind}"))
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
    val current = id.storeRef.get
    id.storeRef.set(f(current))
    val (related, others) = delayedConstraints.get.partition(_.related(id))
    delayedConstraints.set(others)
    addConstraints(related.map(_.x))
  }

  override def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C] = ConcurrentCell(cell)
}
