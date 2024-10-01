package chester.propagator

import cats.implicits.*
import chester.error.*
import chester.resolve.{SimpleDesalt, resolveOpSeq}
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.*
import chester.utils.propagator.*
import chester.utils.*

import scala.language.implicitConversions

def resolve(expr: Expr, localCtx: LocalCtx)(using reporter: Reporter[TyckProblem]): Expr = {
  val result = SimpleDesalt.desugarUnwrap(expr) match {
    case opseq: OpSeq => {
      val result = resolveOpSeq(reporter, localCtx.ctx.operators, opseq)
      result
    }
    case default => default
  }
  reuse(expr, result)
}


object BaseTycker {
  type Literals = Expr & (IntegerLiteral | RationalLiteral | StringLiteral | SymbolLiteral)

  case class Unify(lhs: CellId[Term], rhs: CellId[Term], cause: Expr, uniqId: UniqIdOf[Unify] = UniqId.generate[Unify]) extends Propagator[Ck] {
    override val readingCells = Set(lhs, rhs)
    override val writingCells = Set(lhs, rhs)
    override val zonkingCells = Set(lhs, rhs)

    override def run(using state: StateAbility[Ck], more: Ck): Boolean = {
      val lhs = state.read(this.lhs)
      val rhs = state.read(this.rhs)
      if (lhs.isDefined && rhs.isDefined) {
        unify(lhs.get, rhs.get, cause)
        return true
      }
      return false
    }

    override def naiveZonk(needed: Vector[UniqIdOf[Cell[?]]])(using state: StateAbility[Ck], more: Ck): ZonkResult = {
      val lhs = state.read(this.lhs)
      val rhs = state.read(this.rhs)
      (lhs, rhs) match {
        case (Some(lhs), Some(rhs)) if lhs == rhs => return ZonkResult.Done
        case (Some(lhs), None) => {
          state.fill(this.rhs, lhs)
          return ZonkResult.Done
        }
        case (None, Some(rhs)) => {
          state.fill(this.lhs, rhs)
          return ZonkResult.Done
        }
        case _ => return ZonkResult.NotYet
      }
    }
  }

  case class UnionOf(
                      lhs: CellId[Term],
                      rhs: Vector[CellId[Term]],
                      cause: Expr,
                      uniqId: UniqIdOf[UnionOf] = UniqId.generate[UnionOf]
                    ) extends Propagator[Ck] {
    override val readingCells = Set(lhs) ++ rhs.toSet
    override val writingCells = Set(lhs)
    override val zonkingCells = Set(lhs) ++ rhs.toSet

    override def run(using state: StateAbility[Ck], more: Ck): Boolean = {
      val lhsValueOpt = state.read(lhs)
      val rhsValuesOpt = rhs.map(state.read)

      if (lhsValueOpt.isDefined && rhsValuesOpt.forall(_.isDefined)) {
        val lhsValue = lhsValueOpt.get
        val rhsValues = rhsValuesOpt.map(_.get)

        // Check that each rhsValue is assignable to lhsValue
        val assignable = rhsValues.forall { rhsValue =>
          unify(lhsValue, rhsValue, cause)
          true // Assuming unify reports errors internally
        }
        assignable
      } else {
        // Not all values are available yet
        false
      }
    }

    override def naiveZonk(needed: Vector[UniqIdOf[Cell[?]]])(using state: StateAbility[Ck], more: Ck): ZonkResult = {
      val lhsValueOpt = state.read(lhs)
      val rhsValuesOpt = rhs.map(state.read)

      val unknownRhs = rhs.zip(rhsValuesOpt).collect { case (id, None) => id }
      if (unknownRhs.nonEmpty) {
        // Wait for all rhs values to be known
        ZonkResult.Require(unknownRhs.toVector)
      } else {
        val rhsValues = rhsValuesOpt.map(_.get)

        lhsValueOpt match {
          case Some(lhsValue) =>
            // LHS is known, unify each RHS with LHS
            rhsValues.foreach { rhsValue =>
              unify(lhsValue, rhsValue, cause)
            }
            ZonkResult.Done
          case None =>
            // LHS is unknown, create UnionType from RHS values and set LHS
            val unionType = Union.from(rhsValues.assumeNonEmpty)
            state.fill(lhs, unionType)
            ZonkResult.Done
        }
      }
    }
  }


  case class LiteralType(x: Literals, ty: CellId[Term], meta: Option[ExprMeta], uniqId: UniqIdOf[LiteralType] = UniqId.generate[LiteralType]) extends Propagator[Ck] {
    override val readingCells = Set(ty)
    override val writingCells = Set(ty)
    override val zonkingCells = Set(ty)

    override def run(using state: StateAbility[Ck], more: Ck): Boolean = {
      if (state.noValue(ty)) return false
      val ty_ = state.read(this.ty).get
      val result = x match {
        case IntegerLiteral(_, _) => ty_ == IntegerType
        case RationalLiteral(_, _) => ty_ == RationalType
        case StringLiteral(_, _) => ty_ == StringType
        case SymbolLiteral(_, _) => ty_ == SymbolType
      }
      if (result) {
        return true
      } else {
        ???
      }
    }

    override def naiveZonk(needed: Vector[UniqIdOf[Cell[?]]])(using state: StateAbility[Ck], more: Ck): ZonkResult =
      state.fill(ty, x match {
        case IntegerLiteral(_, _) => IntegerType
        case RationalLiteral(_, _) => RationalType
        case StringLiteral(_, _) => StringType
        case SymbolLiteral(_, _) => SymbolType
      })
      ZonkResult.Done
  }

  case class IsEffects(effects: CellId[Effects], uniqId: UniqIdOf[IsEffects] = UniqId.generate[IsEffects]) extends Propagator[Ck] {
    override val zonkingCells = Set(effects)

    override def run(using state: StateAbility[Ck], more: Ck): Boolean = state.hasValue(effects)

    override def naiveZonk(needed: Vector[UniqIdOf[Cell[?]]])(using state: StateAbility[Ck], more: Ck): ZonkResult = {
      state.fill(effects, Effects.Empty)
      ZonkResult.Done
    }
  }

  case class IsType(ty: CellId[Term], uniqId: UniqIdOf[IsType] = UniqId.generate[IsType]) extends Propagator[Ck] {
    override val readingCells = Set(ty)
    override val zonkingCells = Set(ty)

    override def run(using state: StateAbility[Ck], more: Ck): Boolean = state.hasValue(ty)

    override def naiveZonk(needed: Vector[UniqIdOf[Cell[?]]])(using state: StateAbility[Ck], more: Ck): ZonkResult = {
      if(state.readingZonkings(Vector(ty)).nonEmpty) return ZonkResult.NotYet
      state.fill(ty, AnyType0)
      ZonkResult.Done
    }
  }

  def newType(using ck: Ck, state: StateAbility[Ck]): CellId[Term] = {
    val cell = OnceCell[Term]()
    state.addCell(cell)
    state.addPropagator(IsType(cell.uniqId))
    cell.uniqId
  }

  case class FlatMaping[T, U](xs: Vector[CellId[T]], f: Vector[T] => U, result: CellId[U], uniqId: UniqIdOf[FlatMaping[T, U]] = UniqId.generate[FlatMaping[T, U]]) extends Propagator[Ck] {
    override val readingCells = xs.toSet
    override val writingCells = Set(result)
    override val zonkingCells = Set(result)

    override def run(using state: StateAbility[Ck], more: Ck): Boolean = {
      xs.traverse(state.read(_)).map(f) match {
        case Some(result) => {
          state.fill(this.result, result)
          true
        }
        case None => false
      }
    }

    override def naiveZonk(needed: Vector[UniqIdOf[Cell[?]]])(using state: StateAbility[Ck], more: Ck): ZonkResult = {
      val needed = xs.filter(state.noValue(_))
      if (needed.nonEmpty) return ZonkResult.Require(needed)
      val done = run
      require(done)
      ZonkResult.Done
    }
  }

  def FlatMap[T, U](xs: Vector[CellId[T]])(f: Vector[T] => U)(using ck: Ck, state: StateAbility[Ck]): CellId[U] = {
    val cell = OnceCell[U]()
    state.addCell(cell)
    state.addPropagator(FlatMaping(xs, f, cell.uniqId))
    cell.uniqId
  }

  def Map[T, U](x: CellId[T])(f: T => U)(using ck: Ck, state: StateAbility[Ck]): CellId[U] = {
    val cell = OnceCell[U]()
    state.addCell(cell)
    state.addPropagator(FlatMaping(Vector(x), (xs: Vector[T]) => f(xs.head), cell.uniqId))
    cell.uniqId
  }

  def unify(lhs: Term, rhs: Term, cause: Expr)(using ck: Ck, state: StateAbility[Ck]): Unit = {
    if (lhs == rhs) return
    (lhs, rhs) match {

      // Structural unification for ListType
      case (ListType(elem1), ListType(elem2)) =>
        unify(elem1, elem2, cause)

      // Structural unification for Union types
      case (Union(types1), Union(types2)) =>
        val minLength = math.min(types1.length, types2.length)
        (types1.take(minLength), types2.take(minLength)).zipped.foreach { (ty1, ty2) =>
          unify(ty1, ty2, cause)
        }
        if (types1.length != types2.length) {
          ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
        }

      // Base case: types do not match
      case _ =>
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))

    }
  }

  def unify(t1: Term, t2: CellId[Term], cause: Expr)(using ck: Ck, state: StateAbility[Ck]): Unit = {
    state.addPropagator(Unify(literal(t1), t2, cause))
  }

  def literal[T](t: T)(using ck: Ck, state: StateAbility[Ck]): CellId[T] = {
    val cell = LiteralCell[T](t)
    state.addCell(cell)
    cell.uniqId
  }

  /** t is rhs, listT is lhs */
  case class ListOf(t: CellId[Term], listT: CellId[Term], cause: Expr, uniqId: UniqIdOf[ListOf] = UniqId.generate[ListOf])(using localCtx: LocalCtx) extends Propagator[Ck] {
    override val readingCells = Set(t, listT)
    override val writingCells = Set(t, listT)
    override val zonkingCells = Set(listT)

    override def run(using state: StateAbility[Ck], more: Ck): Boolean = {
      val t1 = state.read(this.t)
      val listT1 = state.read(this.listT)
      (t1, listT1) match {
        case (Some(t1), Some(ListType(t2))) => {
          unify(t2, t1, cause)
          true
        }
        case (_, Some(ListType(t2))) => {
          unify(t2, t, cause)
          true
        }
        case (_, _) => false
      }
    }

    override def naiveZonk(needed: Vector[UniqIdOf[Cell[?]]])(using state: StateAbility[Ck], more: Ck): ZonkResult = {
      val t1 = state.read(this.t)
      val listT1 = state.read(this.listT)
      if (!t1.isDefined) return ZonkResult.Require(Vector(this.t))
      val ty = t1.get
      assert(listT1.isEmpty)
      state.fill(this.listT, ListType(ty))
      ZonkResult.Done
    }
  }

  // TODO: add something for implicit conversion

  /** ty is lhs */
  def check(expr: Expr, ty: CellId[Term], effects: CellId[Effects])(using localCtx: LocalCtx, ck: Ck, state: StateAbility[Ck]): CellId[Term] = state.toId {
    resolve(expr, localCtx) match {
      case expr@IntegerLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty, meta))
        AbstractIntTerm.from(value)
      }
      case expr@RationalLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty, meta))
        RationalTerm(value)
      }
      case expr@StringLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty, meta))
        StringTerm(value)
      }
      case expr@SymbolLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty, meta))
        SymbolTerm(value)
      }
      case expr@ListExpr(terms, meta) => {
        val t = newType
        // Relate the list type 'ty' to 'ListType(t)'
        state.addPropagator(ListOf(t, ty, expr))

        // For each term, check it with its own type variable and collect the results
        val termResults = terms.map { term =>
          val elemTy = newType
          val wellTypedTerm = check(term, elemTy, effects)
          (wellTypedTerm, elemTy)
        }

        // Collect the types of the elements
        val elemTypes = termResults.map(_._2).toVector

        // Ensure that 't' is the union of the element types
        state.addPropagator(UnionOf(t, elemTypes, expr))

        // Build the ListTerm with the well-typed terms
        FlatMap(termResults.map(_._1)) { xs =>
          ListTerm(xs)
        }
      }
      case expr: Expr => ???
    }
  }
}

type Ck = Get[TyckProblem, CkState]

given ckToReport(using ck: Ck): Reporter[TyckProblem] = ck.reporter

case class CkState(
                    symbols: SymbolTable = Set.empty,
                  )

object Cker {
  def check(expr: Expr, ty: Option[Term] = None, effects: Option[Effects] = None, localCtx: LocalCtx = LocalCtx.Empty): TyckResult[CkState, Judge] = {
    val reporter = new VectorReporter[TyckProblem]
    implicit val get: Ck = new Get(reporter, new MutBox(CkState()))
    implicit val able: StateAbility[Ck] = new StateCells[Ck]()
    val ty1: CellId[Term] = ty match {
      case Some(ty) => {
        val cell = LiteralCell[Term](ty)
        able.addCell(cell)
        cell.uniqId
      }
      case None => {
        val cell = OnceCell[Term](None)
        able.addCell(cell)
        cell.uniqId
      }
    }
    val effects1: CellId[Effects] = effects match {
      case Some(effects) => {
        val cell = LiteralCell[Effects](effects)
        able.addCell(cell)
        cell.uniqId
      }
      case None => {
        val cell = OnceCell[Effects]()
        able.addCell(cell)
        cell.uniqId
      }
    }
    able.addPropagator(BaseTycker.IsEffects(effects1))
    implicit val ctx: LocalCtx = LocalCtx.Empty
    val wellTyped = BaseTycker.check(expr, ty1, effects1)
    able.naiveZonk(Vector(ty1, effects1, wellTyped))
    TyckResult0(get.getState, Judge(able.read(wellTyped).get, able.read(ty1).get, able.read(effects1).get), reporter.getReports)
  }
}