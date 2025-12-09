package chester.utils.elab

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

import chester.utils.cell.{
  CellContent,
  CellContentR,
  CollectionCellContent,
  LiteralCellContent,
  MappingCellContent,
  MutableCellContent,
  OnceCellContent
}

final class ProceduralCell[+A, -B, +C <: CellContent[A, B]](
    initialValue: C
) extends Cell[A, B, C] {
  private[elab] var _store: Any = initialValue
  private[elab] inline def store: Any = _store
  private[elab] inline def storeAs[T]: T = _store.asInstanceOf[T]
}

// Solver instance - just holds state, parameterized by constraint type
final class ProceduralSolverInstance[C](val conf: HandlerConf[C, ProceduralSolverModule.type]) {
  val todo: Queue[C] = mutable.Queue[C]()
  val delayedConstraints: ArrayBuffer[WaitingConstraint] = mutable.ArrayBuffer[WaitingConstraint]()
  val updatedCells: ArrayBuffer[ProceduralSolverModule.CellAny] = mutable.ArrayBuffer[ProceduralSolverModule.CellAny]()
}

// ML-style module - implements all operations
object ProceduralSolverModule extends SolverModule {
  // Define Cell type as ProceduralCell
  override type Cell[+A, -B, +C <: CellContent[A, B]] = ProceduralCell[A, B, C]

  // Define Solver instance type - parameterized by constraint type
  override type Solver[C] = ProceduralSolverInstance[C]

  override def makeSolver[C](conf: HandlerConf[C, this.type]): Solver[C] =
    new ProceduralSolverInstance[C](conf)

  // Helper methods
  private def peakCell[T](id: CellR[T]): CellContentR[T] = id.storeAs[CellContentR[T]]

  private def updateCell[C, A, B](solver: Solver[C], id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    val current = id.storeAs[CellContent[A, B]]
    val newValue = f(current)
    if (current != newValue) {
      id._store = newValue
      solver.updatedCells.append(id.asInstanceOf[CellAny])
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
    ProceduralCell(OnceCellContent[T](None, default))

  override def newMutableCell[C, T](solver: Solver[C], initial: Option[T] = None): MutableCell[T] =
    ProceduralCell(MutableCellContent[T](initial))

  override def newCollectionCell[C, A](solver: Solver[C]): CollectionCell[A] =
    ProceduralCell(CollectionCellContent[A, A]())

  override def newMapCell[C, K, V](solver: Solver[C]): MapCell[K, V] =
    ProceduralCell(MappingCellContent[K, V]())

  override def newLiteralCell[C, T](solver: Solver[C], value: T): LiteralCell[T] =
    ProceduralCell(LiteralCellContent(value))

  override def addCell[C, A, B, CC <: CellContent[A, B]](solver: Solver[C], cell: CC): Cell[A, B, CC] =
    ProceduralCell(cell)

  override def addConstraint[C](solver: Solver[C], constraint: C): Unit =
    solver.todo.enqueue(constraint)

  override def stable[C](solver: Solver[C]): Boolean =
    solver.delayedConstraints.isEmpty && solver.todo.isEmpty

  private val IterationLimit = 200000
  private val EnableIterationLimit = true
  private val EnableDebugLogging = false

  @tailrec
  override def run[C](solver: Solver[C]): Unit = {
    var iterations = 0
    while (solver.todo.nonEmpty) {
      var heuristics: Int = 1
      while (solver.todo.nonEmpty && heuristics < 32) {
        iterations += 1
        if (EnableIterationLimit && iterations > IterationLimit) {
          val pending = solver.todo.headOption.map(_.toString).getOrElse("<empty>")
          throw new IllegalStateException(s"procedural solver exceeded iteration budget ($IterationLimit); next constraint: $pending")
        }
        heuristics += 1
        val c = solver.todo.dequeue()
        if (EnableDebugLogging && iterations <= 50) {
          println(s"[solver] processing: $c")
        }
        val handler = solver.conf.getHandler(c).getOrElse(throw new IllegalStateException("no handler"))
        val result = handler.run(c)(using this, solver)
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
          if (call) solver.todo.enqueue(c.constraint.asInstanceOf[C])
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
        val c = x.constraint.asInstanceOf[C]
        val handler = solver.conf.getHandler(c).getOrElse(throw new IllegalStateException("no handler"))
        if (handler.canDefaulting(default)) {
          val result = handler.run(c)(using this, solver)
          result match {
            case Result.Done =>
              Vector()
            case Result.Waiting(vars @ _*) =>
              if (handler.defaulting(c, default)(using this, solver)) {
                val result = handler.run(c)(using this, solver)
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
      val waitingInfo = solver.delayedConstraints.map(wc => s"  - ${wc.constraint} waiting on ${wc.vars.size} cells").mkString("\n")
      throw new IllegalStateException(s"cannot finish some constraints. ${solver.delayedConstraints.size} waiting:\n$waitingInfo")
    }
    val _ = solver.delayedConstraints.filterInPlace { c =>
      val call = c.vars.exists(solver.updatedCells.contains)
      if (call) solver.todo.enqueue(c.constraint.asInstanceOf[C])
      !call
    }
    solver.updatedCells.clear()
    if (!stable(solver)) return run(solver)
  }
}
