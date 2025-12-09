package chester.utils.elab

import scala.language.implicitConversions

import chester.utils.cell.{
  CellContent,
  CellContentR,
  CellContentW,
  CollectionCellContent,
  LiteralCellContent,
  MappingCellContent,
  MutableCellContent,
  OnceCellContent
}

/** ML-style module signature for constraint solvers.
  *
  * DESIGN DECISIONS:
  *
  *   1. ML-Style Modules: Each solver is a module object (not a class)
  *      - ProceduralSolverModule, ConcurrentSolverModule are singleton objects
  *      - They define type members (Cell, Solver) and operations
  *      - Solver instances are separate from the module definition
  *   2. Operations take solver as explicit parameter
  *      - Instead of solver.read(cell), we have module.read(solver, cell)
  *      - This allows solver to be just data, all logic is in the module
  *      - Removes need for SolverOps trait - everything is in SolverModule
  *   3. Handlers are polymorphic over modules
  *      - Handler methods take [M <: SolverModule] type parameter
  *      - Same handler can work with any solver implementation
  *      - Handlers receive (using module: M, solver: module.Solver)
  *   4. Implicit conversions treat all cells as module-specific
  *      - User code defines structures with top-level Cell type
  *      - Module provides conversions Cell <-> M#Cell
  *      - Safe because cells from different solvers never mix
  *   5. Type projections M#Cell reference module's cell type
  *      - Can't use M#Solver in using clause (Scala 3 limitation)
  *      - Solution: pass module first, then use module.Solver
  */
trait SolverModule {
  // The solver instance type - parameterized by constraint type
  type Solver[C]

  // Abstract cell type - must extend top-level Cell trait
  // This allows user code to use Cell without knowing the solver
  type Cell[+A, -B, +C <: CellContent[A, B]] <: chester.utils.elab.Cell[A, B, C]

  // AXIOM: All cells we see are this module's cells
  // These conversions are unsafe but correct because:
  // 1. User code creates cells through module.newXXXCell methods
  // 2. Cells from different solvers never mix in practice
  // 3. The top-level Cell trait is just an interface - implementation is module-specific
  //
  // Simple conversion: top-level Cell → module.Cell with same type parameters
  // This handles the common case where constraint case classes store Cell[A, B, C]
  // and handlers need to work with module.Cell[A, B, C]
  given cellToModuleCell[A, B, C <: CellContent[A, B]]: Conversion[chester.utils.elab.Cell[A, B, C], Cell[A, B, C]] =
    _.asInstanceOf[Cell[A, B, C]]

  // Derived cell type aliases
  type CellOf[A, B] = Cell[A, B, CellContent[A, B]]
  type CellRW[T] = Cell[T, T, CellContent[T, T]]
  type CellAny = Cell[Any, Nothing, CellContent[Any, Nothing]]
  type CellR[+T] = Cell[T, Nothing, CellContentR[T]]
  type CellW[-T] = Cell[Any, T, CellContent[Any, T]]

  // Specific cell types for common content types
  type OnceCell[T] = Cell[T, T, OnceCellContent[T]]
  type MutableCell[T] = Cell[T, T, MutableCellContent[T]]
  type CollectionCell[A] = Cell[Seq[A], Seq[A], CollectionCellContent[A, A]]
  type MapCell[K, V] = Cell[Map[K, V], Map[K, V], MappingCellContent[K, V]]
  type LiteralCell[T] = Cell[T, Nothing, LiteralCellContent[T]]

  // Factory method to create solver instance
  def makeSolver[C](conf: HandlerConf[C, this.type]): Solver[C]

  // All solver operations take solver instance as parameter
  def hasStableValue[C](solver: Solver[C], id: CellAny): Boolean
  def noStableValue[C](solver: Solver[C], id: CellAny): Boolean
  def readStable[C, U](solver: Solver[C], id: CellR[U]): Option[U]
  def hasSomeValue[C](solver: Solver[C], id: CellAny): Boolean
  def noAnyValue[C](solver: Solver[C], id: CellAny): Boolean
  def readUnstable[C, U](solver: Solver[C], id: CellR[U]): Option[U]

  def run[C](solver: Solver[C]): Unit
  def stable[C](solver: Solver[C]): Boolean

  // Constraints can be any user-defined type
  def addConstraint[C](solver: Solver[C], constraint: C): Unit
  def addConstraints[C](solver: Solver[C], constraints: Seq[C]): Unit = constraints.foreach(addConstraint(solver, _))

  // Typed cell constructors
  def newOnceCell[C, T](solver: Solver[C], default: Option[T] = None): OnceCell[T]
  def newMutableCell[C, T](solver: Solver[C], initial: Option[T] = None): MutableCell[T]
  def newCollectionCell[C, A](solver: Solver[C]): CollectionCell[A]
  def newMapCell[C, K, V](solver: Solver[C]): MapCell[K, V]
  def newLiteralCell[C, T](solver: Solver[C], value: T): LiteralCell[T]

  // Generic constructor for extensibility
  def addCell[Constraint, A, B, CellType <: CellContent[A, B]](solver: Solver[Constraint], cell: CellType): Cell[A, B, CellType]
  def fill[C, T](solver: Solver[C], id: CellW[T], value: T): Unit
}
