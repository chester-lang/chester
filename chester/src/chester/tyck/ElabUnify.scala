package chester.tyck

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable

import chester.core.{AST, Arg, BuiltinEffect, CST, Coeffect, EffectRef, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.error.{Problem, Reporter, Span, VectorReporter}
import chester.uniqid.{Uniqid, UniqidOf}
import chester.utils.elab.*
import chester.utils.{HoldNotReadable, given}
import chester.utils.doc.{<>, Doc, DocConf, DocOps, StringPrinter, ToDoc, given}
import chester.tyck.ASTOps.normalizeType
import cats.data.NonEmptyVector
/** Unification result following the paper's architecture */
private enum UnifyResult:
  case Success
  case Failure(message: String)

private def handleUnify[M <: SolverModule](c: ElabConstraint.Unify)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
  import module.given

  (module.readStable(solver, c.ty1), module.readStable(solver, c.ty2)) match
    case (Some(t1), Some(t2)) =>
      // Proper unification with occurs check and meta-variable solving
      unify(t1, t2, c.span, c.ctx)(using module, solver) match
        case UnifyResult.Success => Result.Done
        case UnifyResult.Failure(_) =>
          c.ctx.reporter.report(ElabProblem.TypeMismatch(t1, t2, c.span))
          Result.Done
    case (None, _) => Result.Waiting(c.ty1)
    case (_, None) => Result.Waiting(c.ty2)
}

/** Unify two types with occurs check and meta-variable solving. Following the paper's architecture, this acts as a specialized unification solver.
  * TODO: Enable reduction before unification (paper Section 7.5) after fixing infinite loops.
  */
private def unify[M <: SolverModule](
    t1: AST,
    t2: AST,
    span: Option[Span],
    ctx: ElabContext
)(using module: M, solver: module.Solver[ElabConstraint]): UnifyResult = {
  import module.given

  val r1 = reduce(t1, ctx)
  val r2 = reduce(t2, ctx)

  (r1, r2) match
    // Identical terms
    case _ if r1 == r2 => UnifyResult.Success

    // Meta-variable cases - solve by unification (following paper's "Solver U1")
    case (AST.MetaCell(HoldNotReadable(cell1), _), ty2) =>
      module.readStable(solver, cell1) match
        case Some(solved1) => unify(solved1, ty2, span, ctx)
        case None          =>
          // Solve: ?α := ty2 with occurs check
          if occursIn(cell1, ty2)(using module, solver) then UnifyResult.Failure("Occurs check failed: infinite type")
          else {
            module.fill(solver, cell1, ty2)
            UnifyResult.Success
          }

    case (ty1, AST.MetaCell(HoldNotReadable(cell2), _)) =>
      module.readStable(solver, cell2) match
        case Some(solved2) => unify(ty1, solved2, span, ctx)
        case None          =>
          // Solve: ?β := ty1 with occurs check
          if occursIn(cell2, ty1)(using module, solver) then UnifyResult.Failure("Occurs check failed: infinite type")
          else {
            module.fill(solver, cell2, ty1)
            UnifyResult.Success
          }

    // Structural unification
    case (AST.IntLit(v1, _), AST.IntLit(v2, _)) =>
      if v1 == v2 then UnifyResult.Success else UnifyResult.Failure("Integer literals differ")

    case (AST.LevelLit(v1, _), AST.LevelLit(v2, _)) =>
      if v1 == v2 then UnifyResult.Success else UnifyResult.Failure("Level literals differ")

    case (AST.StringLit(v1, _), AST.StringLit(v2, _)) =>
      if v1 == v2 then UnifyResult.Success else UnifyResult.Failure("String literals differ")

    case (AST.Ref(id1, _, _), AST.Ref(id2, _, _)) =>
      if id1 == id2 then UnifyResult.Success else UnifyResult.Failure("Different variables")

    case (AST.Type(l1, _), AST.Type(l2, _)) => unify(l1, l2, span, ctx)

    case (AST.TypeOmega(l1, _), AST.TypeOmega(l2, _)) => unify(l1, l2, span, ctx)

    // Tolerate redundant applications of Type/Typeω (e.g., Type(0)(0)) by comparing the base
    case (AST.App(t1 @ AST.Type(_, _), _, _, _), t2 @ AST.Type(_, _)) =>
      unify(t1, t2, span, ctx)
    case (t1 @ AST.Type(_, _), AST.App(t2 @ AST.Type(_, _), _, _, _)) =>
      unify(t1, t2, span, ctx)
    case (AST.App(t1 @ AST.TypeOmega(_, _), _, _, _), t2 @ AST.TypeOmega(_, _)) =>
      unify(t1, t2, span, ctx)
    case (t1 @ AST.TypeOmega(_, _), AST.App(t2 @ AST.TypeOmega(_, _), _, _, _)) =>
      unify(t1, t2, span, ctx)

    case (AST.LevelType(_), AST.LevelType(_)) => UnifyResult.Success

    case (AST.AnyType(_), AST.AnyType(_))           => UnifyResult.Success
    case (AST.BoolType(_), AST.BoolType(_))         => UnifyResult.Success
    case (AST.StringType(_), AST.StringType(_))     => UnifyResult.Success
    case (AST.IntegerType(_), AST.IntegerType(_))   => UnifyResult.Success
    case (AST.ListType(e1, _), AST.ListType(e2, _)) => unify(e1, e2, span, ctx)

    case (AST.Tuple(e1, _), AST.Tuple(e2, _)) =>
      if e1.size != e2.size then UnifyResult.Failure("Tuple arity mismatch")
      else unifyAll(e1.zip(e2), span, ctx)
    case (AST.TupleType(e1, _), AST.TupleType(e2, _)) =>
      if e1.size != e2.size then UnifyResult.Failure("Tuple type arity mismatch")
      else unifyAll(e1.zip(e2), span, ctx)

    case (AST.App(f1, args1, imp1, _), AST.App(f2, args2, imp2, _)) =>
      if imp1 != imp2 || args1.size != args2.size then UnifyResult.Failure("Application mismatch")
      else {
        unify(f1, f2, span, ctx) match
          case UnifyResult.Success => unifyAll(args1.map(_.value).zip(args2.map(_.value)), span, ctx)
          case failure             => failure
      }

    case (AST.Pi(tel1, r1, eff1, _), AST.Pi(tel2, r2, eff2, _)) =>
      if tel1.size != tel2.size then UnifyResult.Failure("Function arity mismatch")
      else {
        val paramPairs = tel1.zip(tel2).flatMap { case (t1, t2) =>
          t1.params.zip(t2.params).map((p1, p2) => (p1.ty, p2.ty))
        }
        unifyAll(paramPairs, span, ctx) match
          case UnifyResult.Success =>
            if eff1.toSet == eff2.toSet then unify(r1, r2, span, ctx)
            else UnifyResult.Failure("Effect mismatch")
          case failure => failure
      }

    case (AST.RecordTypeRef(id1, _, _), AST.RecordTypeRef(id2, _, _)) =>
      if id1 == id2 then UnifyResult.Success else UnifyResult.Failure("Record type mismatch")
    case (AST.EnumTypeRef(id1, _, _), AST.EnumTypeRef(id2, _, _)) =>
      if id1 == id2 then UnifyResult.Success else UnifyResult.Failure("Enum type mismatch")

    case _ => UnifyResult.Failure(s"Type mismatch: ${t1.getClass.getSimpleName} vs ${t2.getClass.getSimpleName}")
}

/** Unify a list of type pairs */
private def unifyAll[M <: SolverModule](
    pairs: Vector[(AST, AST)],
    span: Option[Span],
    ctx: ElabContext
)(using module: M, solver: module.Solver[ElabConstraint]): UnifyResult = {
  pairs.foldLeft(UnifyResult.Success: UnifyResult) { case (acc, (a, b)) =>
    acc match
      case UnifyResult.Success => unify(a, b, span, ctx)
      case failure             => failure
  }
}

/** Handle subtyping constraint: ty1 <: ty2 */
private def handleSubtype[M <: SolverModule](c: ElabConstraint.Subtype)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
  import module.given

  (module.readStable(solver, c.ty1), module.readStable(solver, c.ty2)) match
    case (Some(t1), Some(t2)) =>
      if isSubtype(t1, t2, c.span, c.ctx)(using module, solver) then Result.Done
      else {
        c.ctx.reporter.report(ElabProblem.TypeMismatch(t1, t2, c.span))
        Result.Done
      }
    case (None, _) => Result.Waiting(c.ty1)
    case (_, None) => Result.Waiting(c.ty2)
}

/** Check if ty1 is a subtype of ty2
  *
  * Subtyping rules:
  *   - Everything is a subtype of Any
  *   - Reflexive: T <: T
  *   - Function subtyping: contravariant in parameters, covariant in result
  *   - Structural for tuples, etc.
  */
private def isSubtype[M <: SolverModule](
    ty1: AST,
    ty2: AST,
    span: Option[Span],
    ctx: ElabContext
)(using module: M, solver: module.Solver[ElabConstraint]): Boolean = {

  val normTy1 = reduce(ty1, ctx)
  val normTy2 = reduce(ty2, ctx)

  // Any acts as a supertype for value-level types, but not for universe/level types
  normTy2 match
    case _: AST.AnyType =>
      normTy1 match
        case _: AST.Type | _: AST.TypeOmega | _: AST.LevelType => ()
        case _                                                 => return true
    case _ => ()

  // Any is only a subtype of itself when on the left
  if normTy1.isInstanceOf[AST.AnyType] then return normTy2.isInstanceOf[AST.AnyType]

  // Bool is only a subtype of itself (and Any, handled above)
  if normTy1.isInstanceOf[AST.BoolType] then return normTy2.isInstanceOf[AST.BoolType]

  // String is only a subtype of itself (and Any, handled above)
  if normTy1.isInstanceOf[AST.StringType] then return normTy2.isInstanceOf[AST.StringType]

  // Integer is only a subtype of itself (and Any)
  if normTy1.isInstanceOf[AST.IntegerType] then return normTy2.isInstanceOf[AST.IntegerType]

  // Reflexive case and unification fallback
  unify(normTy1, normTy2, span, ctx) match
    case UnifyResult.Success    => true
    case UnifyResult.Failure(_) =>
      // Check structural subtyping
      (normTy1, normTy2) match
        // Function subtyping: contravariant in parameters, covariant in result
        // (A -> B) <: (A' -> B') if A' <: A and B <: B'
        case (AST.Pi(tel1, r1, eff1, _), AST.Pi(tel2, r2, eff2, _)) =>
          if tel1.size != tel2.size then false
          else {
            // Parameters are contravariant
            val paramsOk = tel1.zip(tel2).forall { case (t1, t2) =>
              if t1.params.size != t2.params.size then false
              else {
                t1.params.zip(t2.params).forall { case (p1, p2) =>
                  // p2.ty <: p1.ty (contravariant!)
                  isSubtype(p2.ty, p1.ty, span, ctx)
                }
              }
            }
            // Result is covariant; effects must be subsets (fewer requirements is more specific)
            paramsOk && isSubtype(r1, r2, span, ctx) && eff1.toSet.subsetOf(eff2.toSet)
          }

        // Tuple subtyping: covariant in all components
        case (AST.Tuple(e1, _), AST.Tuple(e2, _)) =>
          e1.size == e2.size && e1.zip(e2).forall { case (a, b) =>
            isSubtype(a, b, span, ctx)
          }
        case (AST.TupleType(e1, _), AST.TupleType(e2, _)) =>
          e1.size == e2.size && e1.zip(e2).forall { case (a, b) =>
            isSubtype(a, b, span, ctx)
          }

        // List subtyping: covariant
        case (AST.ListType(elem1, _), AST.ListType(elem2, _)) =>
          isSubtype(elem1, elem2, span, ctx)

        case _ => false
}

/** Occurs check: does a meta-variable cell occur in a type? */
private def occursIn[M <: SolverModule](
    cell: Any, // The cell we're checking for (stored in HoldNotReadable)
    ty: AST
)(using module: M, solver: module.Solver[ElabConstraint]): Boolean = {
  ty match
    case AST.MetaCell(HoldNotReadable(c), _) =>
      if c == cell then true
      else module.readStable(solver, c).exists(occursIn(cell, _))
    case AST.Ref(_, _, _) | AST.StringLit(_, _) | AST.IntLit(_, _) | AST.AnyType(_) | AST.StringType(_) | AST.IntegerType(_) =>
      false
    case AST.NaturalType(_)           => false
    case AST.ListType(element, _)     => occursIn(cell, element)
    case AST.Type(level, _)           => occursIn(cell, level)
    case AST.TypeOmega(level, _)      => occursIn(cell, level)
    case AST.Tuple(elements, _)       => elements.exists(occursIn(cell, _))
    case AST.TupleType(elements, _)   => elements.exists(occursIn(cell, _))
    case AST.ListLit(elements, _)     => elements.exists(occursIn(cell, _))
    case AST.Block(elements, tail, _) => elements.exists(occursInStmt(cell, _)) || occursIn(cell, tail)
    case AST.Pi(telescopes, resultTy, _, _) =>
      telescopes.exists(t => t.params.exists(p => occursIn(cell, p.ty))) || occursIn(cell, resultTy)
    case AST.Lam(telescopes, body, _) =>
      telescopes.exists(t => t.params.exists(p => occursIn(cell, p.ty))) || occursIn(cell, body)
    case AST.App(func, args, _, _) =>
      occursIn(cell, func) || args.exists(a => occursIn(cell, a.value))
    case AST.Let(_, _, ty, value, body, _) =>
      ty.exists(occursIn(cell, _)) || occursIn(cell, value) || occursIn(cell, body)
    case AST.Ann(expr, ty, _) => occursIn(cell, expr) || occursIn(cell, ty)
    case AST.RecordCtor(_, _, args, _) =>
      args.exists(occursIn(cell, _))
    case AST.EnumCtor(_, _, _, _, args, _) =>
      args.exists(occursIn(cell, _))
    case AST.EnumCaseRef(_, _, _, _, _) => false
    case AST.EnumTypeRef(_, _, _)       => false
    case AST.FieldAccess(target, _, _) =>
      occursIn(cell, target)
    case _ => false
}

private def occursInStmt[M <: SolverModule](cell: Any, stmt: StmtAST)(using module: M, solver: module.Solver[ElabConstraint]): Boolean = {
  stmt match
    case StmtAST.ExprStmt(expr, _)           => occursIn(cell, expr)
    case StmtAST.JSImport(_, _, _, _, ty, _) => occursIn(cell, ty)
    case StmtAST.Def(_, _, teles, resTy, body, _, _) =>
      teles.exists(t => t.params.exists(p => occursIn(cell, p.ty))) || resTy.exists(occursIn(cell, _)) || occursIn(cell, body)
    case StmtAST.Record(_, _, fields, _) =>
      fields.exists(p => occursIn(cell, p.ty))
    case StmtAST.Enum(_, _, typeParams, cases, _) =>
      typeParams.exists(p => occursIn(cell, p.ty)) || cases.exists(c => c.params.exists(p => occursIn(cell, p.ty)))
    case StmtAST.Coenum(_, _, typeParams, cases, _) =>
      typeParams.exists(p => occursIn(cell, p.ty)) || cases.exists(c => c.params.exists(p => occursIn(cell, p.ty)))
    case StmtAST.Pkg(_, body, _) => occursIn(cell, body)
}
