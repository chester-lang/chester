package chester.tyck

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable

import chester.core.{AST, Arg, BuiltinEffect, CST, Coeffect, EffectRef, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.error.{Problem, Reporter, Span, VectorReporter}
import chester.uniqid.{Uniqid, UniqidOf}
import chester.utils.elab.*
import chester.utils.{HoldNotReadable, given}
import chester.utils.doc.{<>, Doc, DocConf, DocOps, StringPrinter, ToDoc, given}
import chester.tyck.CoreTypeChecker.normalizeType
import cats.data.NonEmptyVector
/** Reduce (normalize) a term by performing beta-reduction. Following the paper's recommendation (Section 7.5), we reduce before unification. This
  * handles lambda applications.
  */
private def reduce[M <: SolverModule](
    term: AST,
    ctx: ElabContext,
    depth: Int = 0
)(using module: M, solver: module.Solver[ElabConstraint]): AST = {
  import module.given

  def isTypeLevelBinding(ty: AST): Boolean =
    ty match
      case AST.Type(_, _) | AST.TypeOmega(_, _) | AST.LevelType(_) => true
      case _                                                      => false

  // Depth limit to prevent infinite recursion
  if depth > 100 then return term

  term match
    // Resolve MetaCells first (but don't recurse if it leads back to a MetaCell)
    case AST.MetaCell(HoldNotReadable(cell), span) =>
      module.readStable(solver, cell) match
        case Some(solved) if !solved.isInstanceOf[AST.MetaCell] => reduce(solved, ctx, depth + 1)
        case _                                                  => term

    // Unfold local let-bound aliases when they are used as type-level values.
    case AST.Ref(id, _, _) =>
      val bodyOpt = ctx.lookupDefBody(id).flatMap(cell => module.readStable(solver, cell))
      val tyOpt = ctx.lookupType(id).flatMap(cell => module.readStable(solver, cell))
      (bodyOpt, tyOpt.map(reduce(_, ctx, depth + 1))) match
        case (Some(body), Some(boundTy)) if isTypeLevelBinding(boundTy) =>
          reduce(body, ctx, depth + 1)
        case _ =>
          term

    // Beta-reduction: (λ params. body) args -> body[params := args]
    case AST.App(func, args, implicitArgs, span) =>
      reduce(func, ctx, depth + 1) match
        case AST.Lam(telescopes, body, lamSpan) =>
          val targetImplicitness =
            if implicitArgs then Implicitness.Implicit else Implicitness.Explicit
          val (appliedTelescopes, remainingTelescopes) =
            telescopes.span(_.implicitness == targetImplicitness)
          val paramsToApply = appliedTelescopes.flatMap(_.params)
          if paramsToApply.size == args.size then
            val substMap = paramsToApply
              .zip(args)
              .map { case (param, arg) => param.id -> arg.value }
              .toMap
            val substitutedBody = substituteInType(body, substMap)
            if remainingTelescopes.nonEmpty then
              val newLam = AST.Lam(remainingTelescopes, substitutedBody, lamSpan)
              reduce(newLam, ctx, depth + 1)
            else reduce(substitutedBody, ctx, depth + 1)
          else term
        case AST.Ref(id, _, _) =>
          (ctx.lookupDefBody(id), ctx.lookupType(id)) match
            case (Some(bodyCell), Some(defTyCell)) =>
              (module.readStable(solver, bodyCell), module.readStable(solver, defTyCell)) match
                case (Some(bodyAst), Some(AST.Pi(teles, _, _, defSpan))) =>
                  val lam = AST.Lam(teles, bodyAst, defSpan)
                  reduce(AST.App(lam, args, implicitArgs, span), ctx, depth + 1)
                case _ => term
            case _ => term
        case _ => term

    // For all other constructs, no reduction
    case _ => term
}

/** Handler configuration for elaboration */
class ElabHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
  def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] =
    Some(ElabHandler)

/** Substitute meta-cell solutions throughout an AST This resolves MetaCell nodes by reading their cell contents after constraint solving Also known
  * as "zonking" in some type checkers
  */
def substituteSolutions[M <: SolverModule](ast: AST)(using module: M, solver: module.Solver[ElabConstraint]): AST = {

  def isLevelParamLam(teles: Vector[Telescope]): Boolean =
    teles.length == 1 && teles.head.params.length == 1 && teles.head.params.head.ty.isInstanceOf[AST.LevelType]

  def desugarKindSugar(t: AST): AST = t match
    case AST.Lam(teles, AST.Type(AST.Ref(_, _, _), tSpan), lamSpan) if isLevelParamLam(teles) =>
      AST.Type(AST.LevelLit(0, None), tSpan.orElse(lamSpan))
    case AST.Lam(teles, AST.TypeOmega(AST.Ref(_, _, _), tSpan), lamSpan) if isLevelParamLam(teles) =>
      AST.TypeOmega(AST.LevelLit(0, None), tSpan.orElse(lamSpan))
    case other => other

  // First normalize common kind sugar (e.g., bare Type => Type(0))
  val normalizedKind = desugarKindSugar(ast)
  val targetAst = if normalizedKind != ast then normalizedKind else ast

  targetAst match
    case AST.MetaCell(HoldNotReadable(cell), span) =>
      // Try to read the solution from the cell
      module.readStable(solver, cell.asInstanceOf[module.CellR[AST]]) match
        case Some(solution) =>
          // Recursively substitute in the solution (it might contain more meta-cells)
          substituteSolutions(solution)
        case None =>
          // Cell not filled - keep as meta-cell (shouldn't happen if solving succeeded)
          ast

    case AST.Ref(id, name, span) => ast

    case AST.Tuple(elements, span) =>
      AST.Tuple(elements.map(substituteSolutions), span)
    case AST.TupleType(elements, span) =>
      AST.TupleType(elements.map(substituteSolutions), span)

    case AST.ListLit(elements, span) =>
      AST.ListLit(elements.map(substituteSolutions), span)

    case AST.Block(elements, tail, span) =>
      AST.Block(elements.map(substituteSolutionsStmt), substituteSolutions(tail), span)
    case AST.StringLit(value, span) => ast
    case AST.IntLit(value, span)    => ast
    case AST.NaturalLit(value, span) =>
      AST.NaturalLit(value, span)

    case AST.Type(level, span) =>
      AST.Type(substituteSolutions(level), span)
    case AST.TypeOmega(level, span) =>
      AST.TypeOmega(substituteSolutions(level), span)

    case AST.AnyType(span)     => ast
    case AST.StringType(span)  => ast
    case AST.IntegerType(span) => ast
    case AST.NaturalType(span) => ast
    case AST.ListType(element, span) =>
      AST.ListType(substituteSolutions(element), span)

    case AST.Pi(telescopes, resultTy, effects, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions))),
          tel.implicitness
        )
      }
      AST.Pi(newTelescopes, substituteSolutions(resultTy), effects, span)

    case AST.Lam(telescopes, body, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions))),
          tel.implicitness
        )
      }
      AST.Lam(newTelescopes, substituteSolutions(body), span)

    case AST.App(func, args, implicitArgs, span) =>
      val normalizedFunc = substituteSolutions(func)
      val normalizedArgs = args.map(arg => arg.copy(value = substituteSolutions(arg.value)))
      AST.App(normalizedFunc, normalizedArgs, implicitArgs, span)

    case AST.Let(id, name, ty, value, body, span) =>
      AST.Let(
        id,
        name,
        ty.map(substituteSolutions),
        substituteSolutions(value),
        substituteSolutions(body),
        span
      )

    case AST.Ann(expr, ty, span) =>
      AST.Ann(substituteSolutions(expr), substituteSolutions(ty), span)
    case AST.RecordCtor(id, name, args, span) =>
      AST.RecordCtor(id, name, args.map(substituteSolutions), span)
    case AST.EnumTypeRef(id, name, span) =>
      AST.EnumTypeRef(id, name, span)
    case AST.EnumCaseRef(enumId, caseId, enumName, caseName, span) =>
      AST.EnumCaseRef(enumId, caseId, enumName, caseName, span)
    case AST.EnumCtor(enumId, caseId, enumName, caseName, args, span) =>
      AST.EnumCtor(enumId, caseId, enumName, caseName, args.map(substituteSolutions), span)
    case AST.FieldAccess(target, field, span) =>
      AST.FieldAccess(substituteSolutions(target), field, span)
    case AST.Handle(action, effRef, handlers, span) =>
      val substHandlers = handlers.map { case (name, lam) => (name, substituteSolutions(lam)) }
      AST.Handle(substituteSolutions(action), effRef, substHandlers, span)
    case AST.Do(op, args, span) =>
      AST.Do(substituteSolutions(op), args.map(substituteSolutions), span)
    case other => other
}

private def substituteSolutionsStmt[M <: SolverModule](stmt: StmtAST)(using module: M, solver: module.Solver[ElabConstraint]): StmtAST = {
  stmt match
    case StmtAST.ExprStmt(expr, span) => StmtAST.ExprStmt(substituteSolutions(expr), span)
    case StmtAST.JSImport(id, localName, modulePath, kind, ty, span) =>
      StmtAST.JSImport(id, localName, modulePath, kind, substituteSolutions(ty), span)
    case StmtAST.Def(id, name, teles, resTy, body, span, effects) =>
      val newTeles =
        teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))))
      StmtAST.Def(id, name, newTeles, resTy.map(substituteSolutions), substituteSolutions(body), span, effects)
    case StmtAST.Record(id, name, fields, span) =>
      val newFields = fields.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))
      StmtAST.Record(id, name, newFields, span)
    case StmtAST.Effect(id, name, ops, span) =>
      val newOps = ops.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))
      StmtAST.Effect(id, name, newOps, span)
    case StmtAST.Enum(id, name, typeParams, cases, span) =>
      val newTypeParams = typeParams.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))
      val newCases =
        cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))))
      StmtAST.Enum(id, name, newTypeParams, newCases, span)
    case StmtAST.Coenum(id, name, typeParams, cases, span) =>
      val newTypeParams = typeParams.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))
      val newCases =
        cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))))
      StmtAST.Coenum(id, name, newTypeParams, newCases, span)
    case StmtAST.Pkg(name, body, span) =>
      StmtAST.Pkg(name, substituteSolutions(body), span)
}

