package chester.utils.elab

enum Result {
  case Done
  case Waiting(vars: CellAny*)
}

/** Constraint handler that works with any solver module and any constraint type.
  *
  * DESIGN DECISIONS:
  *
  *   1. Handlers are fully generic over constraint type C
  *      - No need for Kind trait - constraint type is the key
  *      - User defines any constraint type they want
  *   2. Handlers take [M <: SolverModule] type parameter per method
  *      - Same handler can work with different solvers
  *      - Easy to switch between ProceduralSolver and ConcurrentSolver
  *   3. Methods receive (using module: M, solver: module.Solver)
  *      - Module comes first to establish the type
  *      - Then solver: module.Solver uses path-dependent type
  */
trait Handler[C] {
  def run[M <: SolverModule](constraint: C)(using module: M, solver: module.Solver[C]): Result

  /** return true means did something false means nothing */
  def defaulting[M <: SolverModule](constraint: C, level: DefaultingLevel)(using module: M, solver: module.Solver[C]): Boolean = false

  def canDefaulting(level: DefaultingLevel): Boolean
}

/** Handler configuration for a specific constraint type C. Each solver instance is configured for one constraint type.
  */
trait HandlerConf[C, M <: SolverModule] {
  def getHandler(constraint: C): Option[Handler[C]]
}

enum DefaultingLevel extends Enum[DefaultingLevel] {
  case ListOfSetListType
  case Lit
  case UnifyMerge
  case UnifyMultipleMerge
  case IsType
}
object DefaultingLevel {
  val Values: Vector[DefaultingLevel] = DefaultingLevel.values.toVector.sortBy(_.precedence)
}
extension (x: DefaultingLevel) {
  // depend on this assumption: the first one should be 0
  def precedence: Int = x.ordinal
}
