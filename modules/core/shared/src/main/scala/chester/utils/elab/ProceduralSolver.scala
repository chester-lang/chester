package chester.utils.elab

import chester.utils.cell.{CellContent, CellContentR}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

final class ProceduralCell[+A, -B, C <: CellContent[A, B]](
    initialValue: C
) extends Cell[A, B, C] {
  var store = initialValue
}

object ProceduralSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ProceduralSolver(conf)
}

final class ProceduralSolver[Ops](val conf: HandlerConf[Ops])(using Ops) extends BasicSolverOps {
  given SolverOps = this
  val todo: Queue[Constraint] = mutable.Queue[Constraint]()
  val delayedConstraints: ArrayBuffer[WaitingConstraint] = mutable.ArrayBuffer[WaitingConstraint]()
  val updatedCells: ArrayBuffer[Cell[Any, Nothing, CellContent[Any, Nothing]]] = mutable.ArrayBuffer[CellAny]()

  implicit inline def thereAreAllProcedural[A, B, C <: CellContent[A, B]](inline x: Cell[A, B, C]): ProceduralCell[A, B, C] =
    x.asInstanceOf[ProceduralCell[A, B, C]]

  override protected def peakCell[T](id: CellR[T]): CellContentR[T] = id.store

  override protected def updateCell[A, B](id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    val current = id.store
    val newValue = f(current)
    if (current == newValue) return
    id.store = newValue
    updatedCells.append(id)
  }

  @tailrec
  override def run(): Unit = {
    while (todo.nonEmpty) {
      var heuristics: Int = 1
      while (todo.nonEmpty && heuristics < 32) {
        heuristics += 1
        val c = todo.dequeue()
        val handler = c.cached(conf.getHandler(c.kind).getOrElse(throw new IllegalStateException("no handler")))
        val result = handler.run(c.asInstanceOf[handler.kind.Of])
        result match {
          case Result.Done =>
          // do nothing
          case Result.Waiting(vars*) =>
            delayedConstraints.append(WaitingConstraint(vars.toVector, c))
        }
      }
      if (delayedConstraints.nonEmpty) {
        val _ = delayedConstraints.filterInPlace { c =>
          val call = c.vars.exists(updatedCells.contains)
          if (call) todo.enqueue(c.x)
          !call
        }
        updatedCells.clear()
      }
    }
    var defaults = DefaultingLevel.Values
    var nothingChanged = true
    while (nothingChanged && defaults.nonEmpty) {
      val default = defaults.head
      defaults = defaults.tail
      val _ = delayedConstraints.flatMapInPlace { x =>
        val c = x.x
        val handler = c.cached(conf.getHandler(c.kind).getOrElse(throw new IllegalStateException("no handler")))
        if (handler.canDefaulting(default)) {
          val result = handler.run(c.asInstanceOf[handler.kind.Of])
          result match {
            case Result.Done =>
              Vector()
            case Result.Waiting(vars*) =>
              if (handler.defaulting(c.asInstanceOf[handler.kind.Of], default)) {
                val result = handler.run(c.asInstanceOf[handler.kind.Of])
                result match {
                  case Result.Done =>
                    Vector()
                  case Result.Waiting(vars*) =>
                    Vector(WaitingConstraint(vars.toVector, c))
                }
              } else {
                Vector(WaitingConstraint(vars.toVector, c))
              }
          }
        } else {
          Vector(x)
        }
      }
      if (updatedCells.nonEmpty) nothingChanged = false
    }
    if (defaults.isEmpty && nothingChanged && todo.isEmpty) {
      if (stable) return
      throw new IllegalStateException("cannot finish some constraints")
    }
    val _ = delayedConstraints.filterInPlace { c =>
      val call = c.vars.exists(updatedCells.contains)
      if (call) todo.enqueue(c.x)
      !call
    }
    updatedCells.clear()
    if (!stable) return run()
  }

  override def stable: Boolean = delayedConstraints.isEmpty && todo.isEmpty

  override def addConstraint(x: Constraint): Unit = todo.enqueue(x)

  override def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C] = ProceduralCell(cell)
}
