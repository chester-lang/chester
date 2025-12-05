package chester.utils.elab

import chester.utils.cell.{CellContent, CellContentR, OnceCellContent, MutableCellContent, CollectionCellContent, MappingCellContent, LiteralCellContent}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

final class ProceduralCell[+A, -B, +C <: CellContent[A, B]](
    initialValue: C
) extends Cell[A, B, C] {
  private[elab] var _store: Any = initialValue
  private[elab] inline def store: Any = _store
  private[elab] inline def storeAs[T]: T = _store.asInstanceOf[T]
}

object ProceduralSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ProceduralSolver(conf)
}

final class ProceduralSolver[Ops](val conf: HandlerConf[Ops])(using Ops) extends BasicSolverOps {
  // Define Cell type using type lambda to match variance in interface
  override type Cell[+A, -B, +C <: CellContent[A, B]] = ProceduralCell[A, B, C]
  
  given SolverOps = this
  val todo: Queue[Constraint] = mutable.Queue[Constraint]()
  val delayedConstraints: ArrayBuffer[WaitingConstraint] = mutable.ArrayBuffer[WaitingConstraint]()
  val updatedCells: ArrayBuffer[CellAny] = mutable.ArrayBuffer[CellAny]()

  override protected def peakCell[T](id: CellR[T]): CellContentR[T] = id.storeAs[CellContentR[T]]

  override protected def updateCell[A, B](id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    val current = id.storeAs[CellContent[A, B]]
    val newValue = f(current)
    if (current == newValue) return
    id._store = newValue
    updatedCells.append(id.asInstanceOf[CellAny])
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
  
  override def newOnceCell[T](default: Option[T] = None): OnceCell[T] = 
    ProceduralCell(OnceCellContent[T](None, default))
  
  override def newMutableCell[T](initial: Option[T] = None): MutableCell[T] = 
    ProceduralCell(MutableCellContent[T](initial))
  
  override def newCollectionCell[A]: CollectionCell[A] = 
    ProceduralCell(CollectionCellContent[A, A]())
  
  override def newMapCell[K, V]: MapCell[K, V] = 
    ProceduralCell(MappingCellContent[K, V]())
  
  override def newLiteralCell[T](value: T): LiteralCell[T] = 
    ProceduralCell(LiteralCellContent(value))

  override def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C] = ProceduralCell(cell)
}
