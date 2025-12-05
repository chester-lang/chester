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

// Solver instance - just holds state
final class ProceduralSolverInstance(val conf: HandlerConf[ProceduralSolverModule.type]) {
  val todo: Queue[Constraint] = mutable.Queue[Constraint]()
  val delayedConstraints: ArrayBuffer[WaitingConstraint] = mutable.ArrayBuffer[WaitingConstraint]()
  val updatedCells: ArrayBuffer[ProceduralSolverModule.CellAny] = mutable.ArrayBuffer[ProceduralSolverModule.CellAny]()
}

// ML-style module - implements all operations
object ProceduralSolverModule extends SolverModule {
  // Define Cell type as ProceduralCell
  override type Cell[+A, -B, +C <: CellContent[A, B]] = ProceduralCell[A, B, C]
  
  // Define Solver instance type
  override type Solver = ProceduralSolverInstance
  
  override def makeSolver[Ops](conf: HandlerConf[this.type]): Solver =
    new ProceduralSolverInstance(conf)
  
  // Helper methods
  private def peakCell[T](id: CellR[T]): CellContentR[T] = id.storeAs[CellContentR[T]]

  private def updateCell[A, B](solver: Solver, id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    val current = id.storeAs[CellContent[A, B]]
    val newValue = f(current)
    if (current != newValue) {
      id._store = newValue
      solver.updatedCells.append(id.asInstanceOf[CellAny])
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
    ProceduralCell(OnceCellContent[T](None, default))
  
  override def newMutableCell[T](solver: Solver, initial: Option[T] = None): MutableCell[T] = 
    ProceduralCell(MutableCellContent[T](initial))
  
  override def newCollectionCell[A](solver: Solver): CollectionCell[A] = 
    ProceduralCell(CollectionCellContent[A, A]())
  
  override def newMapCell[K, V](solver: Solver): MapCell[K, V] = 
    ProceduralCell(MappingCellContent[K, V]())
  
  override def newLiteralCell[T](solver: Solver, value: T): LiteralCell[T] = 
    ProceduralCell(LiteralCellContent(value))

  override def addCell[A, B, C <: CellContent[A, B]](solver: Solver, cell: C): Cell[A, B, C] = 
    ProceduralCell(cell)
  
  override def addConstraint(solver: Solver, x: Constraint): Unit = 
    solver.todo.enqueue(x)

  override def stable(solver: Solver): Boolean = 
    solver.delayedConstraints.isEmpty && solver.todo.isEmpty

  @tailrec
  override def run(solver: Solver): Unit = {
    while (solver.todo.nonEmpty) {
      var heuristics: Int = 1
      while (solver.todo.nonEmpty && heuristics < 32) {
        heuristics += 1
        val c = solver.todo.dequeue()
        val handler = c.cached(solver.conf.getHandler(c.kind).getOrElse(throw new IllegalStateException("no handler")))
        val result = handler.run(c.asInstanceOf[handler.kind.Of])(using this, solver)
        result match {
          case Result.Done =>
          // do nothing
          case Result.Waiting(vars @ _*) =>
            solver.delayedConstraints.append(WaitingConstraint(vars.toVector, c))
        }
      }
      if (solver.delayedConstraints.nonEmpty) {
        val _ = solver.delayedConstraints.filterInPlace { c =>
          val call = c.vars.exists(solver.updatedCells.contains)
          if (call) solver.todo.enqueue(c.x)
          !call
        }
        solver.updatedCells.clear()
      }
    }
    var defaults = DefaultingLevel.Values
    var nothingChanged = true
    while (nothingChanged && defaults.nonEmpty) {
      val default = defaults.head
      defaults = defaults.tail
      val _ = solver.delayedConstraints.flatMapInPlace { x =>
        val c = x.x
        val handler = c.cached(solver.conf.getHandler(c.kind).getOrElse(throw new IllegalStateException("no handler")))
        if (handler.canDefaulting(default)) {
          val result = handler.run(c.asInstanceOf[handler.kind.Of])(using this, solver)
          result match {
            case Result.Done =>
              Vector()
            case Result.Waiting(vars @ _*) =>
            if (handler.defaulting(c.asInstanceOf[handler.kind.Of], default)(using this, solver)) {
              val result = handler.run(c.asInstanceOf[handler.kind.Of])(using this, solver)
                result match {
                  case Result.Done =>
                    Vector()
                  case Result.Waiting(vars2 @ _*) =>
                    Vector(WaitingConstraint(vars2.toVector, c))
                }
              } else {
                Vector(WaitingConstraint(vars.toVector, c))
              }
          }
        } else {
          Vector(x)
        }
      }
      if (solver.updatedCells.nonEmpty) nothingChanged = false
    }
    if (defaults.isEmpty && nothingChanged && solver.todo.isEmpty) {
      if (stable(solver)) return
      throw new IllegalStateException("cannot finish some constraints")
    }
    val _ = solver.delayedConstraints.filterInPlace { c =>
      val call = c.vars.exists(solver.updatedCells.contains)
      if (call) solver.todo.enqueue(c.x)
      !call
    }
    solver.updatedCells.clear()
    if (!stable(solver)) return run(solver)
  }
}
