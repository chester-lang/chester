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
/** All elaboration constraints */
enum ElabConstraint:
  /** Check that CST has expected type, produce AST in result cell */
  case Check(
      cst: CST,
      expectedTy: CellR[AST],
      result: CellRW[AST],
      ctx: ElabContext
  )

  /** Infer type of CST, produce AST and inferred type */
  case Infer(
      cst: CST,
      result: CellRW[AST],
      inferredTy: CellRW[AST],
      ctx: ElabContext,
      asType: Boolean = false
  )

  /** Infer type in top-level (file) mode, permitting top-level block declarations. */
  case InferTopLevel(
      cst: CST,
      result: CellRW[AST],
      inferredTy: CellRW[AST],
      ctx: ElabContext
  )

  /** Unify two types (type equality) */
  case Unify(
      ty1: CellR[AST],
      ty2: CellR[AST],
      span: Option[Span],
      ctx: ElabContext
  )

  /** Check subtyping: ty1 <: ty2 */
  case Subtype(
      ty1: CellR[AST],
      ty2: CellR[AST],
      span: Option[Span],
      ctx: ElabContext
  )

  /** Ensure ty is a universe type */
  case IsUniverse(
      ty: CellR[AST],
      level: CellRW[AST] // Extract the level if it's a universe
  )

  /** Ensure ty is a Pi type */
  case IsPi(
      ty: CellR[AST],
      telescopes: CellRW[Vector[Telescope]],
      resultTy: CellRW[AST]
  )

  /** Assemble a function application from elaborated function and arguments */
  case AssembleApp(
      funcResult: CellR[AST],
      funcTy: CellR[AST],
      explicitTypeArgResults: Vector[CellR[AST]],
      explicitTypeArgTypes: Vector[CellR[AST]],
      argResults: Vector[CellR[AST]],
      argTypes: Vector[CellR[AST]],
      result: CellRW[AST],
      inferredTy: CellRW[AST],
      span: Option[Span],
      ctx: ElabContext
  )

  /** Assemble an annotated expression expr: Type */
  case AssembleAnn(
      exprResult: CellR[AST],
      annotationTy: CellR[AST],
      result: CellRW[AST],
      inferredTy: CellRW[AST],
      span: Option[Span]
  )

  /** Assemble a def statement once body is elaborated */
  case AssembleDef(
      defId: UniqidOf[AST],
      name: String,
      telescopes: Vector[Telescope],
      resultTyCell: Option[CellR[AST]],
      bodyResult: CellR[AST],
      bodyTy: CellR[AST],
      result: CellRW[AST],
      inferredTy: CellRW[AST],
      effects: Vector[EffectRef],
      effectAnnotated: Boolean,
      defTypeCell: CellRW[AST],
      span: Option[Span],
      ctx: ElabContext
  )

  /** Pre-fill a def type cell when the return type is annotated, resolving recursion deadlock */
  case PreFillDefType(
      defTypeCell: CellRW[AST],
      telescopes: Vector[Telescope],
      resultTyCell: Option[CellR[AST]],
      effects: Vector[EffectRef],
      effectAnnotated: Boolean,
      span: Option[Span]
  )

  case AssemblePi(
      telescopes: Vector[Telescope],
      resultTy: CellR[AST],
      result: CellRW[AST],
      inferredTy: CellRW[AST],
      span: Option[Span]
  )

private[tyck] case class DefInfo(
    name: String,
    id: UniqidOf[AST],
    tyCell: CellRW[AST],
    resultCell: CellRW[AST],
    telescopes: Vector[Telescope] = Vector.empty,
    resultTyCell: Option[CellRW[AST]] = None,
    effects: Vector[EffectRef] = Vector.empty,
    effectAnnotated: Boolean = false,
    span: Option[Span] = None
)

/** Substitute arguments for parameters in a type Replaces occurrences of parameter IDs with corresponding argument ASTs
  */
def substituteInType(ty: AST, substitutions: Map[UniqidOf[AST], AST]): AST = {
  ty match
    case AST.Ref(id, name, span) =>
      substitutions.getOrElse(id, ty)
    case AST.Tuple(elements, span) =>
      AST.Tuple(elements.map(substituteInType(_, substitutions)), span)
    case AST.TupleType(elements, span) =>
      AST.TupleType(elements.map(substituteInType(_, substitutions)), span)
    case AST.ListLit(elements, span) =>
      AST.ListLit(elements.map(substituteInType(_, substitutions)), span)
    case AST.Block(elements, tail, span) =>
      AST.Block(elements.map(substituteStmtInType(_, substitutions)), substituteInType(tail, substitutions), span)
    case AST.Type(level, span) =>
      AST.Type(substituteInType(level, substitutions), span)
    case AST.TypeOmega(level, span) =>
      AST.TypeOmega(substituteInType(level, substitutions), span)
    case AST.AnyType(span) =>
      AST.AnyType(span)
    case AST.StringType(span) =>
      AST.StringType(span)
    case AST.NaturalType(span) =>
      AST.NaturalType(span)
    case AST.IntegerType(span) =>
      AST.IntegerType(span)
    case AST.ListType(element, span) =>
      AST.ListType(substituteInType(element, substitutions), span)
    case AST.Pi(telescopes, resultTy, effects, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => p.copy(ty = substituteInType(p.ty, substitutions), default = p.default.map(substituteInType(_, substitutions)))),
          tel.implicitness
        )
      }
      val boundIds = telescopes.flatMap(_.params.map(_.id)).toSet
      val filteredSubs = substitutions.filterNot { case (id, _) => boundIds.contains(id) }
      AST.Pi(newTelescopes, substituteInType(resultTy, filteredSubs), effects, span)
    case AST.Lam(telescopes, body, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => p.copy(ty = substituteInType(p.ty, substitutions), default = p.default.map(substituteInType(_, substitutions)))),
          tel.implicitness
        )
      }
      val boundIds = telescopes.flatMap(_.params.map(_.id)).toSet
      val filteredSubs = substitutions.filterNot { case (id, _) => boundIds.contains(id) }
      AST.Lam(newTelescopes, substituteInType(body, filteredSubs), span)
    case AST.App(func, args, implicitArgs, span) =>
      AST.App(
        substituteInType(func, substitutions),
        args.map(a => a.copy(value = substituteInType(a.value, substitutions))),
        implicitArgs,
        span
      )
    case AST.RecordTypeRef(id, name, span) =>
      AST.RecordTypeRef(id, name, span)
    case AST.RecordCtor(id, name, args, span) =>
      AST.RecordCtor(id, name, args.map(substituteInType(_, substitutions)), span)
    case AST.FieldAccess(target, field, span) =>
      AST.FieldAccess(substituteInType(target, substitutions), field, span)
    case AST.EnumTypeRef(id, name, span) =>
      AST.EnumTypeRef(id, name, span)
    case AST.EnumCaseRef(enumId, caseId, enumName, caseName, span) =>
      AST.EnumCaseRef(enumId, caseId, enumName, caseName, span)
    case AST.EnumCtor(enumId, caseId, enumName, caseName, args, span) =>
      AST.EnumCtor(enumId, caseId, enumName, caseName, args.map(substituteInType(_, substitutions)), span)
    case AST.Let(id, name, ty, value, body, span) =>
      val filteredSubs = substitutions - id
      AST.Let(
        id,
        name,
        ty.map(substituteInType(_, substitutions)),
        substituteInType(value, substitutions),
        substituteInType(body, filteredSubs),
        span
      )
    case AST.Ann(expr, ty, span) =>
      AST.Ann(substituteInType(expr, substitutions), substituteInType(ty, substitutions), span)
    case other => other
}

private def substituteStmtInType(stmt: StmtAST, substitutions: Map[UniqidOf[AST], AST]): StmtAST = stmt match
  case StmtAST.ExprStmt(expr, span) =>
    StmtAST.ExprStmt(substituteInType(expr, substitutions), span)
  case StmtAST.JSImport(id, localName, modulePath, kind, ty, span) =>
    StmtAST.JSImport(id, localName, modulePath, kind, substituteInType(ty, substitutions), span)
  case StmtAST.Def(id, name, telescopes, resultTy, body, span, effects) =>
    val newTelescopes = telescopes.map { tel =>
      Telescope(
        tel.params.map(p =>
          Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))
        ),
        tel.implicitness
      )
    }
    val boundIds = telescopes.flatMap(_.params.map(_.id)).toSet + id
    val filteredSubs = substitutions.filterNot { case (i, _) => boundIds.contains(i) }
    StmtAST.Def(id, name, newTelescopes, resultTy.map(substituteInType(_, filteredSubs)), substituteInType(body, filteredSubs), span, effects)
  case StmtAST.Record(id, name, fields, span) =>
    val newFields =
      fields.map(p => Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions))))
    StmtAST.Record(id, name, newFields, span)
  case StmtAST.Effect(id, name, ops, span) =>
    val newOps =
      ops.map(p => Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions))))
    StmtAST.Effect(id, name, newOps, span)
  case StmtAST.Enum(id, name, typeParams, cases, span) =>
    val newTypeParams = typeParams.map(p =>
      Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))
    )
    val newCases = cases.map { c =>
      EnumCase(
        c.id,
        c.name,
        c.params.map(p =>
          Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))
        )
      )
    }
    StmtAST.Enum(id, name, newTypeParams, newCases, span)
  case StmtAST.Coenum(id, name, typeParams, cases, span) =>
    val newTypeParams = typeParams.map(p =>
      Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))
    )
    val newCases = cases.map { c =>
      EnumCase(
        c.id,
        c.name,
        c.params.map(p =>
          Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))
        )
      )
    }
    StmtAST.Coenum(id, name, newTypeParams, newCases, span)
  case StmtAST.Pkg(name, body, span) =>
    StmtAST.Pkg(name, substituteInType(body, substitutions), span)

