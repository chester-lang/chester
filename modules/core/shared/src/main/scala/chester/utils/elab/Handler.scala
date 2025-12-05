package chester.utils.elab

enum Result {
  case Done
  case Waiting(vars: CellAny*)
}

/** Constraint handler that works with any solver module.
  * 
  * DESIGN DECISIONS:
  * 
  * 1. Handlers are NOT parameterized by solver module at class level
  *    - Instead, each method takes [M <: SolverModule] type parameter
  *    - This allows same handler to work with different solvers
  *    - User can easily switch between ProceduralSolver and ConcurrentSolver
  * 
  * 2. Methods receive (using module: M, solver: module.Solver)
  *    - Module comes first to establish the type
  *    - Then solver: module.Solver uses path-dependent type
  *    - Can't use M#Solver in using clause (Scala 3 syntax limitation)
  * 
  * 3. Handler caching in Constraint
  *    - Handler lookup is cached per constraint instance
  *    - No type parameter needed since handlers are polymorphic
  */
trait Handler[+K <: Kind](val kind: K) {
  def run[M <: SolverModule](constant: kind.Of)(using module: M, solver: module.Solver): Result

  /** return true means did something false means nothing */
  def defaulting[M <: SolverModule](constant: kind.Of, level: DefaultingLevel)(using module: M, solver: module.Solver): Boolean = false

  def canDefaulting(level: DefaultingLevel): Boolean
}

import scala.collection.mutable

extension [A, B](x: mutable.HashMap[A, B]) {
  // putIfAbsent is available in concurrent.TrieMap
  def putIfAbsent(key: A, value: B): Option[B] =
    if (x.contains(key)) Some(x(key))
    else {
      val _ = x.put(key, value)
      None
    }
}

trait HandlerConf[M <: SolverModule] {
  def getHandler(kind: Kind): Option[Handler[? <: Kind]]
}

final class MutHandlerConf[M <: SolverModule](hs: Handler[? <: Kind]*) extends HandlerConf[M] {
  private val store = mutable.HashMap[Kind, Handler[? <: Kind]](hs.map(h => (h.kind, h))*)

  override def getHandler(kind: Kind): Option[Handler[? <: Kind]] = store.get(kind)

  def register[K <: Kind](handler: Handler[K]): Unit = {
    val oldValue = store.putIfAbsent(handler.kind, handler)
    if (oldValue.isDefined) throw new IllegalStateException("already")
  }
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
