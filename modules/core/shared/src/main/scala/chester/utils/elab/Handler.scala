package chester.utils.elab

enum Result {
  case Done
  case Waiting(vars: CellAny*)
}

open trait Handler[-Ops, +K <: Kind](val kind: K) {
  def run(constant: kind.Of)(using Ops, SolverOps): Result

  /** return true means did something false means nothing */
  def defaulting(constant: kind.Of, level: DefaultingLevel)(using Ops, SolverOps): Boolean = false

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

trait HandlerConf[Ops] {
  def getHandler(kind: Kind): Option[Handler[Ops, Kind]]
}

final class MutHandlerConf[Ops](hs: Handler[Ops, Kind]*) extends HandlerConf[Ops] {
  private val store = mutable.HashMap[Kind, Handler[Ops, Kind]](hs.map(h => (h.kind, h))*)

  override def getHandler(kind: Kind): Option[Handler[Ops, Kind]] = store.get(kind)

  def register(handler: Handler[Ops, Kind]): Unit = {
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
