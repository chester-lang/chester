package chester.tyck

import chester.error.*
import chester.syntax.Name
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.utils.*
import chester.reduce.{DefaultReducer, ReduceContext, ReduceMode, Reducer}
import cats.data.NonEmptyVector

import scala.language.implicitConversions
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

trait TyckPropagator extends ElaboraterCommon with Alpha {

  // Helper method to handle common defaulting setup for cells with left-hand side and right-hand side values
  private def setupDefaulting[T](
      lhs: CellId[T],
      rhs: Vector[CellId[T]],
      needed: Vector[CellIdAny]
  )(using state: StateOps[TyckOps]): Either[DefaultingResult, (Option[T], Vector[T])] = {
    val lhsValueOpt = state.readStable(lhs)
    val rhsValuesOpt = rhs.map(state.readStable)

    // First check if any of our cells are in the needed list
    val ourNeededCells = (Vector(lhs) ++ rhs).filter(needed.contains)
    if (ourNeededCells.isEmpty) {
      return Left(DefaultingResult.Done) // None of our cells are needed
    }

    // Check if we're waiting for rhs values
    val unknownRhs = rhs.zip(rhsValuesOpt).collect { case (id, None) => id }
    if (unknownRhs.nonEmpty) {
      return Left(DefaultingResult.Require(unknownRhs))
    }

    // Get all rhs values - we know they're all defined at this point
    val rhsValues = rhsValuesOpt.collect { case Some(value) => value }

    // Return values - include lhsValueOpt which might be None
    Right((lhsValueOpt, rhsValues))
  }

  def unify(lhs: Term, rhs: Term, cause: Expr)(using
      localCtx: Context,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Unit = {
    def addUnificationPropagator(lhsId: CellId[Term], rhsId: CellId[Term]): Unit =
      state.addPropagator(Unify(lhsId, rhsId, cause))

    // Handle meta variables
    (toTerm(lhs), toTerm(rhs)) match {
      case (Meta(cellId), rhs) =>
        addUnificationPropagator(cellId, toId(rhs))
      case (lhs, Meta(cellId)) =>
        addUnificationPropagator(toId(lhs), cellId)
      case (lhs, rhs) if lhs != rhs =>
        // Use TypeLevel reduction for type equality checking
        given ReduceContext = localCtx.toReduceContext
        given Reducer = localCtx.given_Reducer

        val lhsResolved = DefaultReducer.reduce(lhs, ReduceMode.TypeLevel)
        val rhsResolved = DefaultReducer.reduce(rhs, ReduceMode.TypeLevel)
        if (lhsResolved == rhsResolved) return

        (lhsResolved, rhsResolved) match {
          // Handle Union types - rhs must be a subtype of lhs
          case (lhsType, Union(types2, _)) =>
            // For a union on the right (like Integer | String),
            // the left side type (like Integer) just needs to match ONE component
            val lhsTypeId = toId(lhsType)

            // Find any compatible union component
            val compatibleComponent = types2.find(t2 => tryUnify(lhsType, t2))

            if (compatibleComponent.isDefined) {
              // Create a propagator connecting lhs to the matching component
              val compatibleId = toId(compatibleComponent.get)
              // Use the direct unification propagator
              addUnificationPropagator(lhsTypeId, compatibleId)
            } else {
              // No compatible components
              ck.reporter.report(TypeMismatch(lhs, rhs, cause))
            }

          case (Union(types1, _), rhsType) =>
            // For a union on the left, ANY type in the union must be compatible with rhs
            // We need to check if at least one component is compatible
            val rhsTypeId = toId(rhsType)

            val anyCompatible = types1.exists(t1 => tryUnify(t1, rhsType))

            if (anyCompatible) {
              // At least one component is compatible, create propagators for that component
              types1.find(t1 => tryUnify(t1, rhsType)).foreach { compatibleComponent =>
                val compatibleId = toId(compatibleComponent)
                // Use the direct unification propagator
                addUnificationPropagator(compatibleId, rhsTypeId)
              }
            } else {
              // No compatible components
              ck.reporter.report(TypeMismatch(lhs, rhs, cause))
            }

          // Record implementing trait (structural subtyping)
          case (RecordTypeTerm(recordDef, _, _), TraitTypeTerm(traitDef, _)) =>
            if (!checkTraitImplementation(recordDef, traitDef, cause)) {
              ck.reporter.report(TypeMismatch(lhs, rhs, cause))
            }

          // Record type implementing trait type (structural subtyping)
          case (lhsType @ RecordStmtTerm(name, _, fields, _, extendsClause, _), rhsType @ TraitStmtTerm(_, _, _, _, _)) =>
            if (!checkTraitImplementation(lhsType, rhsType, cause)) {
              ck.reporter.report(TypeMismatch(lhs, rhs, cause))
            }

          // Trait extending trait (structural subtyping)
          case (TraitTypeTerm(childTraitDef, _), TraitTypeTerm(parentTraitDef, _)) =>
            if (!checkTraitExtends(childTraitDef, parentTraitDef, cause)) {
              ck.reporter.report(TypeMismatch(lhs, rhs, cause))
            }

          // Handle Intersection types
          case (Intersection(types1, _), rhsType) =>
            if (!types1.exists(t1 => tryUnify(t1, rhsType))) {
              ck.reporter.report(TypeMismatch(lhs, rhs, cause))
            }

          case (lhsType, Intersection(types2, _)) =>
            if (!types2.forall(t2 => tryUnify(lhsType, t2))) {
              ck.reporter.report(TypeMismatch(lhs, rhs, cause))
            }

          // For other cases, add a direct unification propagator
          case (lhsType, rhsType) =>
            // If terms are not identical after reduction, add a propagator
            val lhsId = toId(lhsType)
            val rhsId = toId(rhsType)
            addUnificationPropagator(lhsId, rhsId)
        }
      case _ => // Terms are already equal, nothing to do
    }
  }

  def unify(t1: Term, t2: CellId[Term], cause: Expr)(using
      localCtx: Context,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Unit =
    state.addPropagator(Unify(literal(t1), t2, cause))

  def unify(t1: CellId[Term], t2: Term, cause: Expr)(using
      localCtx: Context,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Unit =
    state.addPropagator(Unify(t1, literal(t2), cause))

  def unify(t1: CellId[Term], t2: CellId[Term], cause: Expr)(using
      localCtx: Context,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Unit =
    state.addPropagator(Unify(t1, t2, cause))

  type Literals = Expr & (IntegerLiteral | RationalLiteral | StringLiteral | SymbolLiteral)

  case class Unify(lhs: CellId[Term], rhs: CellId[Term], cause: Expr)(using
      Context
  ) extends Propagator[TyckOps] {
    override def readingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs, rhs)
    override def writingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs, rhs)
    override def defaultingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs, rhs)

    override def run(using state: StateOps[TyckOps], more: TyckOps): Boolean = {
      val lhs = state.readStable(this.lhs)
      val rhs = state.readStable(this.rhs)

      (lhs, rhs) match {
        case (Some(l), Some(r)) =>
          unify(l, r, cause)
          true
        case (Some(Meta(l)), _) =>
          state.addPropagator(Unify(l, this.rhs, cause))
          true
        case (_, Some(Meta(r))) =>
          state.addPropagator(Unify(this.lhs, r, cause))
          true
        case _ => false
      }
    }

    override def defaulting(
        needed: Vector[CellIdAny]
    )(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult = {
      // Only process if one of our cells is needed
      if (!needed.contains(this.lhs) && !needed.contains(this.rhs)) {
        return DefaultingResult.Done
      }

      val lhs = state.readStable(this.lhs)
      val rhs = state.readStable(this.rhs)

      (lhs, rhs) match {
        case (Some(l), Some(r)) if l == r => DefaultingResult.Done
        case (Some(l), None) =>
          state.fill(this.rhs, l)
          DefaultingResult.Done
        case (None, Some(r)) =>
          state.fill(this.lhs, r)
          DefaultingResult.Done
        case (Some(Union(types1, _)), Some(Union(types2, _))) =>
          // For union-to-union, handle properly
          handleUnionUnion(types1, types2)
        case (Some(lhsType), Some(Union(types2, _))) =>
          // For specific-to-union, handle properly
          handleSpecificUnion(lhsType, types2)
        case (Some(Union(types1, _)), Some(rhsType)) =>
          // For union-to-specific, handle properly
          handleUnionSpecific(types1, rhsType)
        case (Some(Intersection(types1, _)), Some(Intersection(types2, _))) =>
          // For intersection-to-intersection
          if (types1.forall(t1 => types2.exists(t2 => tryUnify(t1, t2)))) {
            DefaultingResult.Done
          } else {
            DefaultingResult.NotYet
          }
        case (Some(lhsType), Some(Intersection(types2, _))) =>
          // For specific-to-intersection
          if (types2.exists(t2 => tryUnify(lhsType, t2))) {
            DefaultingResult.Done
          } else {
            DefaultingResult.NotYet
          }
        case (Some(Intersection(types1, _)), Some(rhsType)) =>
          // For intersection-to-specific
          if (types1.forall(t1 => tryUnify(t1, rhsType))) {
            DefaultingResult.Done
          } else {
            DefaultingResult.NotYet
          }
        case _ =>
          // Need both values to continue
          DefaultingResult.Require(Vector(this.lhs, this.rhs))
      }
    }

    // Handle union-to-union case properly
    private def handleUnionUnion(
        types1: NonEmptyVector[Term],
        types2: NonEmptyVector[Term]
    )(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult =
      // For each type in RHS union, at least one type in LHS union must accept it
      if (unionUnionCompatible(types1, types2)) {
        // Create proper unification between component types directly
        types2.foreach { t2 =>
          // Find compatible type in types1
          types1.find(t1 => tryUnify(t1, t2)).foreach { compatibleType =>
            // Add direct unification between these compatible types
            state.addPropagator(Unify(toId(compatibleType), toId(t2), cause))
          }
        }
        DefaultingResult.Done
      } else {
        DefaultingResult.NotYet
      }

    // Handle specific-to-union case properly
    private def handleSpecificUnion(
        lhsType: Term,
        types2: NonEmptyVector[Term]
    )(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult =
      // For a specific type to be compatible with a union type,
      // the specific type must be compatible with at least one of the union components
      if (specificUnionCompatible(lhsType, types2)) {
        // Find each compatible union component and connect directly
        types2.withFilter(unionType => tryUnify(lhsType, unionType)).foreach { compatibleType =>
          // Create direct link between specific type and compatible component
          state.addPropagator(Unify(toId(lhsType), toId(compatibleType), cause))
        }
        DefaultingResult.Done
      } else {
        DefaultingResult.NotYet
      }

    // Handle union-to-specific case properly
    private def handleUnionSpecific(
        types1: NonEmptyVector[Term],
        rhsType: Term
    )(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult =
      // For a union type to be compatible with a specific type,
      // at least one component must be compatible
      if (unionSpecificCompatible(types1, rhsType)) {
        // Connect each compatible component directly
        types1.withFilter(t1 => tryUnify(t1, rhsType)).foreach { compatibleType =>
          state.addPropagator(Unify(toId(compatibleType), toId(rhsType), cause))
        }
        DefaultingResult.Done
      } else {
        DefaultingResult.NotYet
      }
  }

  case class UnionOf(
      lhs: CellId[Term],
      rhs: Vector[CellId[Term]],
      cause: Expr
  )(using Context)
      extends Propagator[TyckOps] {
    override def readingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs) ++ rhs.toSet
    override def writingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs)
    override def defaultingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs) ++ rhs.toSet

    override def run(using state: StateOps[TyckOps], more: TyckOps): Boolean = {
      val lhsValueOpt = state.readStable(lhs)
      val rhsValuesOpt = rhs.map(state.readStable)

      if (lhsValueOpt.isDefined && rhsValuesOpt.forall(_.isDefined)) {
        val lhsValue = lhsValueOpt.get
        val rhsValues = rhsValuesOpt.map(_.get)

        lhsValue match {
          case Meta(lhsId) =>
            // Create a new union type and unify with the meta variable
            val unionType = Union(rhsValues.assumeNonEmpty, None)
            unify(lhsId, unionType, cause)
            true
          case _ =>
            // Check that each rhsValue is assignable to lhsValue
            rhsValues.forall { rhsValue =>
              unify(lhsValue, rhsValue, cause)
              true // Assuming unify reports errors internally
            }
        }
      } else {
        // Not all values are available yet
        false
      }
    }

    override def defaulting(
        needed: Vector[CellIdAny]
    )(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult =
      setupDefaulting(lhs, rhs, needed) match {
        case Left(result) => result
        case Right((lhsValueOpt, rhsValues)) =>
          lhsValueOpt match {
            case Some(Meta(lhsId)) =>
              // Create union type and unify with meta variable
              val unionType = Union(rhsValues.assumeNonEmpty, None)
              unify(lhsId, unionType, cause)
              DefaultingResult.Done
            case Some(lhsValue) =>
              // LHS is known, check if it's compatible with all RHS values
              if (rhsValues.forall(rhsValue => tryUnify(lhsValue, rhsValue))) {
                DefaultingResult.Done
              } else {
                DefaultingResult.NotYet
              }
            case None =>
              // LHS is unknown, create UnionType from RHS values
              val unionType = Union(rhsValues.assumeNonEmpty, None)
              state.fill(lhs, unionType)
              DefaultingResult.Done
          }
      }
  }

  case class IntersectionOf(
      lhs: CellId[Term],
      rhs: Vector[CellId[Term]],
      cause: Expr
  )(using Context)
      extends Propagator[TyckOps] {
    override def readingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs) ++ rhs.toSet
    override def writingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs)
    override def defaultingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(lhs) ++ rhs.toSet

    override def run(using state: StateOps[TyckOps], more: TyckOps): Boolean = {
      val lhsValueOpt = state.readStable(lhs)
      val rhsValuesOpt = rhs.map(state.readStable)

      if (lhsValueOpt.isDefined && rhsValuesOpt.forall(_.isDefined)) {
        val lhsValue = lhsValueOpt.get
        val rhsValues = rhsValuesOpt.map(_.get)

        lhsValue match {
          case Meta(lhsId) =>
            // Create a new intersection type and unify with the meta variable
            val intersectionType = Intersection(rhsValues.assumeNonEmpty, None)
            unify(lhsId, intersectionType, cause)
            true
          case _ =>
            // Check that each rhsValue is a subtype of lhsValue
            rhsValues.forall { rhsValue =>
              unify(rhsValue, lhsValue, cause)
              true // Assuming unify reports errors internally
            }
        }
      } else {
        // Not all values are available yet
        false
      }
    }

    override def defaulting(
        needed: Vector[CellIdAny]
    )(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult =
      setupDefaulting(lhs, rhs, needed) match {
        case Left(result) => result
        case Right((lhsValueOpt, rhsValues)) =>
          lhsValueOpt match {
            case Some(Meta(lhsId)) =>
              // Create intersection type and unify with meta variable
              val intersectionType = Intersection(rhsValues.assumeNonEmpty, None)
              unify(lhsId, intersectionType, cause)
              DefaultingResult.Done
            case Some(lhsValue) =>
              // LHS is known, check if it's compatible with all RHS values
              if (rhsValues.forall(rhsValue => tryUnify(rhsValue, lhsValue))) {
                DefaultingResult.Done
              } else {
                DefaultingResult.NotYet
              }
            case None =>
              // LHS is unknown, create IntersectionType from RHS values
              val intersectionType = Intersection(rhsValues.assumeNonEmpty, None)
              state.fill(lhs, intersectionType)
              DefaultingResult.Done
          }
      }
  }

  /** Attempts to unify two terms without producing error messages.
    *
    * This method performs type unification, which includes:
    *   1. Regular structural equality after type-level reduction
    *   2. Alpha-equivalence checking for terms with bound variables
    *   3. Special handling for union and intersection types
    *
    * In a dependent type system, this is crucial as it allows:
    *   - Different variable names but equivalent binding structure to be unifiable
    *   - Type-level computations to be properly reduced and compared
    *   - Complex type structures to be handled correctly
    *
    * @param lhs
    *   Left-hand side term
    * @param rhs
    *   Right-hand side term
    * @return
    *   true if terms can be unified, false otherwise
    */
  def tryUnify(lhs: Term, rhs: Term)(using
      state: StateOps[TyckOps],
      localCtx: Context
  ): Boolean = {
    // Recursion counter for debugging
    new AtomicInteger(0)

    def tryUnifyInternal(lhs: Term, rhs: Term, depth: Int): Boolean = {
      " " * depth

      // Helper function to fully resolve references in terms
      @tailrec
      def fullyResolveReference(term: Term): Term = {
        val resolved = term match {
          case varCall: ReferenceCall =>
            localCtx
              .getKnown(varCall)
              .flatMap(tyAndVal => state.readStable(tyAndVal.valueId))
              .getOrElse(term)
          case _ => term
        }

        // Continue resolving if we got a new reference
        if (resolved != term && resolved.isInstanceOf[ReferenceCall]) {
          fullyResolveReference(resolved)
        } else {
          resolved
        }
      }

      if (lhs == rhs) {
        true
      } else {
        // Use TypeLevel reduction for type equality checking
        given ReduceContext = localCtx.toReduceContext
        given Reducer = localCtx.given_Reducer

        // Fully resolve references for deeper resolution
        val lhsResolved = fullyResolveReference(DefaultReducer.reduce(lhs, ReduceMode.TypeLevel))
        val rhsResolved = fullyResolveReference(DefaultReducer.reduce(rhs, ReduceMode.TypeLevel))

        if (lhsResolved == rhsResolved) {
          true
        } else {
          // If structural equality check fails, try alpha-equivalence
          // which is crucial for dependent type systems
          if (areAlphaEquivalent(lhsResolved, rhsResolved)) {
            true
          } else {
            (lhsResolved, rhsResolved) match {
              case (Type(level1, _), Type(level2, _)) =>
                val result = isLevelCompatible(level1, level2)(using state, localCtx)
                result

              case (ListType(elem1, _), ListType(elem2, _)) =>
                val result = tryUnifyInternal(elem1, elem2, depth + 1)
                result

              case (lhsType, Union(types2, _)) =>
                // For a specific type and a union type, check if the specific type
                // is compatible with at least one union component
                val result = types2.exists(t2 => tryUnifyInternal(lhsType, t2, depth + 1))
                result

              case (Union(types1, _), rhsType) =>
                // For a union type and a specific type, check if at least one union component
                // is compatible with the specific type (NOT all components need to be compatible)
                val result = types1.exists(t1 => tryUnifyInternal(t1, rhsType, depth + 1))
                result

              case _ =>
                false
            }
          }
        }
      }
    }

    // Start the recursion
    val result = tryUnifyInternal(lhs, rhs, 0)
    result
  }

  // Helper method to get the appropriate type for a literal expression
  private def getLiteralType(x: Literals): Term = x match {
    case IntegerLiteral(_, _)  => IntegerType(None)
    case RationalLiteral(_, _) => RationalType(None)
    case StringLiteral(_, _)   => StringType(None)
    case SymbolLiteral(_, _)   => SymbolType(None)
  }

  case class LiteralType(x: Literals, tyLhs: CellId[Term])(using
      Context
  ) extends Propagator[TyckOps] {
    override def readingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(tyLhs)
    override def writingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(tyLhs)
    override def defaultingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(tyLhs)

    override def run(using state: StateOps[TyckOps], more: TyckOps): Boolean =
      if (state.noStableValue(tyLhs)) false
      else {
        val ty_ = state.readStable(this.tyLhs).get
        ty_ match {
          case Meta(ty) =>
            state.addPropagator(LiteralType(x, ty))
            true
          case _ =>
            x match {
              case IntegerLiteral(value, _) =>
                // Try to handle union types
                ty_ match {
                  case Union(types, _) =>
                    // Check if any of the union types is compatible with this integer literal
                    val compatibleTypes = types.filter {
                      case IntegerType(_) => true
                      case unionType =>
                        val reduced =
                          DefaultReducer.reduce(unionType, ReduceMode.TypeLevel)(using summon[Context].toReduceContext, summon[Context].given_Reducer)
                        reduced match {
                          case IntegerType(_) => true
                          case _              => false
                        }
                    }

                    if (compatibleTypes.nonEmpty) {
                      // Found at least one compatible type in the union
                      true
                    } else {
                      more.reporter.report(TypeMismatch(IntegerType(None), ty_, x))
                      true
                    }
                  // Handle normal integer literal case
                  case _ =>
                    if (value.isValidInt && tryUnify(ty_, IntType(None))) true
                    else if (value > 0 && tryUnify(ty_, NaturalType(None))) true
                    else if (tryUnify(ty_, IntegerType(None))) true
                    else {
                      // Create a vector of compatible types based on value properties
                      val possibleTypes = Vector(
                        IntegerType(None) // Always included
                      ) ++
                        (if (value > 0) Vector(NaturalType(None)) else Vector.empty) ++ // Only if value > 0
                        (if (value.isValidInt) Vector(IntType(None)) else Vector.empty) ++ // Only if value is a valid Int
                        (if (value > 0 && value.isValidInt) Vector(UIntType(None)) else Vector.empty) // Only if value > 0 and is a valid Int
                      unify(ty_, Intersection(possibleTypes.assumeNonEmpty, None), x)
                      true
                    }
                }
              case RationalLiteral(_, _) =>
                unify(ty_, RationalType(None), x)
                true
              case StringLiteral(_, _) =>
                unify(ty_, StringType(None), x)
                true
              case SymbolLiteral(_, _) =>
                unify(ty_, SymbolType(None), x)
                true
            }
        }
      }

    override def defaulting(
        needed: Vector[CellIdAny]
    )(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult = {
      // First, check if we already have a value for this cell
      val existingValue = state.readStable(tyLhs)
      if (existingValue.isDefined) {
        existingValue.get match {
          case Union(types, _) =>
            // If we have a union type, we need to make sure the literal is compatible with at least one component
            val literalType = getLiteralType(x)

            // Find compatible types
            val compatibleTypes = types.filter(unionType => tryUnify(literalType, unionType)(using state, summon[Context]))

            if (compatibleTypes.nonEmpty) {
              DefaultingResult.Done // Leave the union type as is
            } else {
              state.fill(
                tyLhs,
                getLiteralType(x)
              )
              DefaultingResult.Done
            }
          case _ =>
            // Not a union type, fill with default literal type
            state.fill(
              tyLhs,
              getLiteralType(x)
            )
            DefaultingResult.Done
        }
      } else {
        // No existing value, fill with default literal type
        state.fill(
          tyLhs,
          getLiteralType(x)
        )
        DefaultingResult.Done
      }
    }
  }

  /** t is rhs, listT is lhs */
  case class ListOf(tRhs: CellId[Term], listTLhs: CellId[Term], cause: Expr)(using
      ck: TyckOps,
      localCtx: Context
  ) extends Propagator[TyckOps] {
    override def readingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(tRhs, listTLhs)
    override def writingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(tRhs, listTLhs)
    override def defaultingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(listTLhs)

    override def run(using state: StateOps[TyckOps], more: TyckOps): Boolean = {
      val t1 = state.readStable(this.tRhs)
      val listT1 = state.readStable(this.listTLhs)
      (t1, listT1) match {
        case (_, Some(Meta(listTLhs))) =>
          state.addPropagator(ListOf(tRhs, listTLhs, cause))
          true
        case (_, Some(l)) if !l.isInstanceOf[ListType] =>
          ck.reporter.report(TypeMismatch(ListType(AnyType0, meta = None), l, cause))
          true
        case (Some(t1), Some(ListType(t2, _))) =>
          unify(t2, t1, cause)
          true
        case (_, Some(ListType(t2, _))) =>
          unify(t2, tRhs, cause)
          true
        case (Some(t1), None) =>
          unify(this.listTLhs, ListType(t1, meta = None): Term, cause)
          true
        case (None, None) =>
          unify(this.listTLhs, ListType(Meta(tRhs), meta = None): Term, cause)
          true
        case _ => ???
      }
    }

    override def defaulting(
        needed: Vector[CellIdAny]
    )(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult = {
      val t1 = state.readStable(this.tRhs)
      val listT1 = state.readStable(this.listTLhs)
      if (t1.isEmpty) return DefaultingResult.Require(Vector(this.tRhs))
      val ty = t1.get
      assert(listT1.isEmpty)
      state.fill(this.listTLhs, ListType(ty, meta = None))
      DefaultingResult.Done
    }
  }

  /** Handles field access on record types, accounting for dependent fields.
    *
    * This propagator ensures that when accessing a field from a record type:
    *   1. The field exists in the record type
    *   2. Type-level computations in field types are properly reduced
    *   3. Dependent fields (where the type depends on other fields) are correctly handled
    *
    * @param recordTy
    *   The record type being accessed
    * @param fieldName
    *   The name of the field being accessed
    * @param expectedTy
    *   The expected type of the field
    * @param cause
    *   The expression causing this check
    */
  case class RecordFieldPropagator(
      recordTy: CellId[Term],
      fieldName: Name,
      expectedTy: CellId[Term],
      cause: Expr
  )(using Context)
      extends Propagator[TyckOps] {
    override def readingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(recordTy)
    override def writingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(expectedTy)
    override def defaultingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set(recordTy, expectedTy)

    override def run(using state: StateOps[TyckOps], more: TyckOps): Boolean =
      state.readStable(recordTy) match {
        case Some(Meta(id)) =>
          state.addPropagator(RecordFieldPropagator(id, fieldName, expectedTy, cause))
          true
        case Some(recordType) =>
          // Apply type-level reduction to ensure we correctly handle dependent types
          given ctx: Context = summon[Context]
          given ReduceContext = ctx.toReduceContext
          given Reducer = ctx.given_Reducer
          val reducedRecord = DefaultReducer.reduce(recordType, ReduceMode.TypeLevel)

          reducedRecord match {
            case IntegerType(_) | IntType(_) if fieldName == "+" =>
              // Special handling for Integer.+ method
              // For Integer.+, the return type is always Integer
              unify(expectedTy, IntegerType(None), cause)
              true
            case RecordTypeTerm(recordDef, _, _) =>
              recordDef.fields.find(_.name == fieldName) match {
                case Some(fieldTerm) =>
                  // For dependent fields, we may need to further reduce the field type
                  val fieldType = DefaultReducer.reduce(fieldTerm.ty, ReduceMode.TypeLevel)
                  unify(expectedTy, fieldType, cause)
                  true
                case None =>
                  val problem = FieldNotFound(fieldName, recordDef.name, cause)
                  more.reporter.report(problem)
                  true
              }
            case other =>
              val problem = NotARecordType(other, cause)
              more.reporter.report(problem)
              true
          }
        case None => false
      }

    override def defaulting(needed: Vector[CellIdAny])(using state: StateOps[TyckOps], more: TyckOps): DefaultingResult =
      state.readStable(recordTy) match {
        case None => DefaultingResult.Require(Vector(recordTy))
        case _    => DefaultingResult.Done
      }
  }

  /** Helper method to check if a source level is compatible with a target level */
  private def isLevelCompatible(source: Term, target: Term)(using
      StateOps[TyckOps],
      Context
  ): Boolean =
    (source, target) match {
      case (LevelFinite(_, _), LevelUnrestricted(_)) => true // Finite is compatible with unrestricted
      case (LevelUnrestricted(_), LevelFinite(_, _)) => false // Unrestricted is not compatible with finite
      case (LevelFinite(n1, _), LevelFinite(n2, _))  =>
        // Try to extract numeric values for comparison
        (extractNumericValue(n1), extractNumericValue(n2)) match {
          // If we can extract both values, lower level is compatible with higher level
          case (Some(v1), Some(v2)) => v1 <= v2
          // If we can't extract, fall back to exact equality
          case _ => source == target
        }
      case _ => source == target // For other cases, keep the exact equality check
    }

  // Helper to extract numeric value from a term
  private def extractNumericValue(term: Term): Option[BigInt] = PartialFunction.condOpt(term) {
    case IntTerm(value, _)     => BigInt(value)
    case IntegerTerm(value, _) => value
  }

  // Add helper method for trait implementation checking
  private def checkTraitImplementation(
      recordDef: RecordStmtTerm,
      traitDef: TraitStmtTerm,
      cause: Expr
  )(using
      localCtx: Context,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Boolean = {

    // First check for direct extension relationship
    val hasExtendsClause = recordDef.extendsClause.exists {
      case traitCall: TraitTypeTerm =>
        val matches = traitCall.traitDef.uniqId == traitDef.uniqId
        matches
      case _ =>
        false
    }

    if (!hasExtendsClause) {
      ck.reporter.report(NotImplementingTrait(recordDef.name, traitDef.name, cause))
      false
    } else {
      // Check that all required fields from the trait are present in the record
      val traitFields = traitDef.body.map(_.statements).getOrElse(Vector.empty).collect {
        case ExprStmtTerm(DefStmtTerm(localv, _, ty, _), _, _) => (localv.name, ty)
        case DefStmtTerm(localv, _, ty, _)                     => (localv.name, ty)
      }

      val recordFields = recordDef.fields.map(field => (field.name, field.ty)).toMap

      // Check each trait field
      val allFieldsPresent = traitFields.forall { case (fieldName, fieldTy) =>
        recordFields.get(fieldName) match {
          case None =>
            ck.reporter.report(MissingTraitField(fieldName, recordDef.name, traitDef.name, cause))
            false
          case Some(recordFieldTy) =>
            // Add type compatibility check
            state.addPropagator(Unify(toId(recordFieldTy), toId(fieldTy), cause))
            true
        }
      }
      allFieldsPresent
    }
  }

  // Helper method to check if one trait extends another
  private def checkTraitExtends(
      childTraitDef: TraitStmtTerm,
      parentTraitDef: TraitStmtTerm,
      _cause: Expr
  )(using
      Context,
      TyckOps,
      StateOps[TyckOps]
  ): Boolean = {
    // Check if they're the same trait (reflexivity)
    if (childTraitDef.uniqId == parentTraitDef.uniqId) {
      return true
    }

    // Check direct parent
    val directParent = childTraitDef.extendsClause match {
      case Some(traitCall: TraitTypeTerm) =>
        traitCall.traitDef.uniqId == parentTraitDef.uniqId
      case _ => false
    }

    directParent
  }

  // Add helper methods for union subtyping compatibility checking
  private def unionUnionCompatible(types1: NonEmptyVector[Term], types2: NonEmptyVector[Term])(using
      StateOps[TyckOps],
      Context
  ): Boolean =
    // For each type in RHS union, at least one type in LHS union must accept it
    types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))

  private def specificUnionCompatible(specificType: Term, unionTypes: NonEmptyVector[Term])(using
      StateOps[TyckOps],
      Context
  ): Boolean = {
    // For a specific type to be compatible with a union type,
    // the specific type must be compatible with at least one of the union components
    // This is for cases like: let y: Integer | String = x
    // where x: Integer
    val result = unionTypes.exists { unionType =>
      val compatible = tryUnify(specificType, unionType)
      compatible
    }
    result
  }

  private def unionSpecificCompatible(unionTypes: NonEmptyVector[Term], specificType: Term)(using
      StateOps[TyckOps],
      Context
  ): Boolean = {
    // For a union type to be compatible with a specific type,
    // at least one type in the union must be compatible with the specific type
    // This is for cases like: let x: Integer | String; let y: SomeType = x;
    // where y: SomeType must accept at least one of Integer or String
    val result = unionTypes.exists { unionType =>
      val compatible = tryUnify(unionType, specificType)
      compatible
    }
    result
  }

  /** Helper method to ensure a cell has a default value This creates a cell with a default value to avoid "not covered by propagator" errors
    */
  def ensureDefaultValue[T](defaultValue: T)(using StateOps[TyckOps]): CellId[T] =
    withDefault(defaultValue)

  /** Propagator to verify that a record properly implements a trait
    */
  case class CheckTraitImplementation(
      recordDef: RecordStmtTerm,
      traitDef: TraitStmtTerm,
      cause: Expr
  )(using Context)
      extends Propagator[TyckOps] {
    override def readingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set.empty
    override def writingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set.empty
    override def defaultingCells(using StateRead[TyckOps], TyckOps): Set[CellIdAny] = Set.empty

    override def run(using StateOps[TyckOps], TyckOps): Boolean = {
      // Delegate to the checkTraitImplementation method
      // We don't need to check the result - any errors will be reported directly
      val _ = checkTraitImplementation(recordDef, traitDef, cause)
      // Always return true to ensure the propagator is removed
      true
    }

    override def defaulting(needed: Vector[CellIdAny])(using StateOps[TyckOps], TyckOps): DefaultingResult =
      DefaultingResult.Done
  }
}
