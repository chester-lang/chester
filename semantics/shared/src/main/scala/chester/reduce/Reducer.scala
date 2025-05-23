package chester.reduce

import chester.syntax.core.*

/** A reducer that can reduce terms to their normal forms.
  *
  * IMPORTANT: Always use the passed reducer `r` for recursion, never `this.reduce` directly. This allows other reducers to be composed/wrapped around
  * this one.
  */
trait Reducer {
  def reduce(term: Term)(using ReduceContext, Reducer): Term
}

/** Controls how aggressively terms are reduced */
enum ReduceMode {
  case TypeLevel // Only reduce type-level computations
  case Normal // Normal reduction strategy
}

object Reducer {
  def reduce(term: Term)(using ctx: ReduceContext, r: Reducer): Term = r.reduce(term)
}

object DefaultReducer extends Reducer {

  /** Check if a term is a type-level computation that should be reduced */
  private def isTypeLevel(term: Term): Boolean = term match {
    case FCallTerm(f, _, _) =>
      f match {
        case Function(FunctionType(_, retTy, _, _), _, _) =>
          retTy match {
            case Type(_, _) => true
            case _          => false
          }
        case _ => false
      }
    case _ => false
  }

  /** Helper method for proper reduction of type structures. This ensures consistent handling of types, especially for dependent type systems.
    */
  private def reduceTypeStructure(term: Term)(using ctx: ReduceContext, r: Reducer): Term =
    term match {
      case Union(types, meta) =>
        val reducedTypes = types.map(ty => reduceTypeStructure(r.reduce(ty)))
        Union(reducedTypes, meta)

      case Intersection(types, meta) =>
        val reducedTypes = types.map(ty => reduceTypeStructure(r.reduce(ty)))
        Intersection(reducedTypes, meta)

      case fcall: FCallTerm if isTypeLevel(fcall) =>
        // First reduce normally
        val reduced = reduceStandard(fcall, ReduceMode.TypeLevel)

        // Then check if the result needs further type structure handling
        reduced match {
          // If still a complex type after reduction, process it recursively
          case Union(_, _) | Intersection(_, _) =>

            val result = reduceTypeStructure(reduced)

            result
          case _ =>

            reduced
        }

      // Other terms are handled by standard reduction
      case _ =>

        term
    }

  /** Standard reduction logic for terms */
  private def reduceStandard(term: Term, _mode: ReduceMode)(using ctx: ReduceContext, r: Reducer): Term = term match {
    // WHNF terms - return as is
    case t: WHNF => t

    // Block terms - reduce statements and result
    case BlockTerm(statements, result, meta) =>
      // We need to preserve the StmtTerm type while avoiding pattern matching with *C/*T suffixes
      val reducedStatements = statements.map(stmt =>
        // Keep the type information while reducing
        r.reduce(stmt).asInstanceOf[StmtTerm]
      )
      val reducedResult = r.reduce(result)
      BlockTerm(reducedStatements, reducedResult, meta)

    // Annotations - reduce the term and type
    case Annotation(term, ty, effects, meta) =>
      val reducedTerm = r.reduce(term)
      val reducedTy = ty.map(r.reduce)
      val reducedEffects = effects // Effects don't need reduction
      Annotation(reducedTerm, reducedTy, reducedEffects, meta)

    // For other cases, leave as is for now
    case other => other
  }

  def reduce(term: Term, mode: ReduceMode)(using ReduceContext, Reducer): Term = {
    // First, apply standard reduction
    val standardReduced = reduceStandard(term, mode)

    // For type-level mode, apply additional type structure handling
    mode match {
      case ReduceMode.TypeLevel =>
        // In type-level mode, ensure complex type structures are properly handled
        reduceTypeStructure(standardReduced)
      case ReduceMode.Normal =>
        // In normal mode, just use the standard reduction
        standardReduced
    }
  }

  // Default to normal reduction mode for backward compatibility
  override def reduce(term: Term)(using ReduceContext, Reducer): Term =
    reduce(term, ReduceMode.Normal)
}
