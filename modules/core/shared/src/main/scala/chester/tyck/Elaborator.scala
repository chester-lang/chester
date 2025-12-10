package chester.tyck

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable

import chester.core.{AST, Arg, BuiltinEffect, CST, EffectRef, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.error.{Problem, Reporter, Span, VectorReporter}
import chester.uniqid.{Uniqid, UniqidOf}
import chester.utils.elab.*
import chester.utils.{HoldNotReadable, given}
import chester.utils.doc.{<>, Doc, DocConf, DocOps, StringPrinter, ToDoc, given}
import chester.tyck.CoreTypeChecker.normalizeType
import cats.data.NonEmptyVector

/** Elaboration problems */
enum ElabProblem(val span0: Option[Span]) extends Problem:
  case UnboundVariable(name: String, override val span0: Option[Span]) extends ElabProblem(span0)
  case TypeMismatch(expected: AST, actual: AST, override val span0: Option[Span]) extends ElabProblem(span0)
  case NotAFunction(ty: AST, override val span0: Option[Span]) extends ElabProblem(span0)
  case NotAUniverse(ty: AST, override val span0: Option[Span]) extends ElabProblem(span0)
  case UnknownEffect(name: String, override val span0: Option[Span]) extends ElabProblem(span0)

  override def stage: Problem.Stage = Problem.Stage.TYCK
  override def severity: Problem.Severity = Problem.Severity.Error

  override def toDoc(using DocConf): Doc = this match
    case ElabProblem.UnboundVariable(name, _) =>
      Doc.text(s"Unbound variable: $name")
    case ElabProblem.TypeMismatch(expected, actual, _) =>
      (Doc.text("Type mismatch: expected "): ToDoc) <> expected.toDoc <> Doc.text(", but got ") <> actual.toDoc
    case ElabProblem.NotAFunction(ty, _) =>
      (Doc.text("Not a function type: "): ToDoc) <> ty.toDoc
    case ElabProblem.NotAUniverse(ty, _) =>
      (Doc.text("Not a universe type: "): ToDoc) <> ty.toDoc
    case ElabProblem.UnknownEffect(name, _) =>
      Doc.text(s"Unknown effect: $name")

/** Elaboration context tracking bindings and types during CST to AST conversion */
case class ElabContext(
    bindings: Map[String, UniqidOf[AST]], // Name -> variable ID mapping
    types: Map[UniqidOf[AST], CellRW[AST]], // Variable ID -> type cell mapping
    defBodies: Map[UniqidOf[AST], CellRW[AST]] = Map.empty, // Definition ID -> AST cell mapping
    effects: Map[String, EffectRef] = ElabContext.defaultEffects, // Declared effects
    recordsByName: Map[String, ElabContext.RecordDef] = Map.empty,
    recordsById: Map[UniqidOf[AST], ElabContext.RecordDef] = Map.empty,
    enumsByName: Map[String, ElabContext.EnumDef] = Map.empty,
    enumsById: Map[UniqidOf[AST], ElabContext.EnumDef] = Map.empty,
    builtins: Set[String] = ElabContext.defaultBuiltins, // Built-in names
    builtinTypes: Map[String, AST] = ElabContext.defaultBuiltinTypes, // Built-in types
    reporter: Reporter[ElabProblem] // Error reporter
):
  def bind(name: String, id: UniqidOf[AST], ty: CellRW[AST]): ElabContext =
    copy(bindings = bindings + (name -> id), types = types + (id -> ty))

  def registerDefBody(id: UniqidOf[AST], cell: CellRW[AST]): ElabContext =
    copy(defBodies = defBodies + (id -> cell))

  def lookup(name: String): Option[UniqidOf[AST]] = bindings.get(name)
  def lookupType(id: UniqidOf[AST]): Option[CellRW[AST]] = types.get(id)
  def lookupDefBody(id: UniqidOf[AST]): Option[CellRW[AST]] = defBodies.get(id)
  def isBuiltin(name: String): Boolean = builtins.contains(name)
  def lookupBuiltinType(name: String): Option[AST] = builtinTypes.get(name)
  def registerRecordPlaceholder(name: String, id: UniqidOf[AST], ctorType: CellRW[AST]): ElabContext = {
    val defn = ElabContext.RecordDef(id, name, Vector.empty, ctorType)
    copy(
      recordsByName = recordsByName + (name -> defn),
      recordsById = recordsById + (id -> defn),
      bindings = bindings + (name -> id),
      types = types + (id -> ctorType)
    )
  }
  def updateRecord(name: String, fields: Vector[Param]): ElabContext = {
    recordsByName.get(name) match
      case Some(defn) =>
        val updated = defn.copy(fields = fields)
        copy(recordsByName = recordsByName + (name -> updated), recordsById = recordsById + (defn.id -> updated))
      case None => this
  }
  def lookupRecord(name: String): Option[ElabContext.RecordDef] = recordsByName.get(name)
  def lookupRecordById(id: UniqidOf[AST]): Option[ElabContext.RecordDef] = recordsById.get(id)
  def registerEnumPlaceholder(name: String, id: UniqidOf[AST], typeParams: Vector[Param], isCoinductive: Boolean): ElabContext = {
    val defn = ElabContext.EnumDef(id, name, typeParams, Vector.empty, isCoinductive = isCoinductive)
    copy(
      enumsByName = enumsByName + (name -> defn),
      enumsById = enumsById + (id -> defn)
    )
  }
  def updateEnum(name: String, typeParams: Vector[Param], cases: Vector[EnumCase], isCoinductive: Boolean): ElabContext = {
    enumsByName.get(name) match
      case Some(defn) =>
        val updated = defn.copy(typeParams = typeParams, cases = cases, isCoinductive = isCoinductive)
        copy(enumsByName = enumsByName + (name -> updated), enumsById = enumsById + (defn.id -> updated))
      case None => this
  }
  def lookupEnum(name: String): Option[ElabContext.EnumDef] = enumsByName.get(name)
  def lookupEnumById(id: UniqidOf[AST]): Option[ElabContext.EnumDef] = enumsById.get(id)
  def lookupEnumCase(enumId: UniqidOf[AST], caseName: String): Option[EnumCase] =
    enumsById.get(enumId).flatMap(_.cases.find(_.name == caseName))
  def declareEffect(name: String): ElabContext = {
    val eff = EffectRef.User(Uniqid.make, name)
    copy(effects = effects + (name -> eff))
  }
  def lookupEffect(name: String): Option[EffectRef] = effects.get(name)

object ElabContext:
  case class RecordDef(id: UniqidOf[AST], name: String, fields: Vector[Param], ctorType: CellRW[AST])
  case class EnumDef(id: UniqidOf[AST], name: String, typeParams: Vector[Param], cases: Vector[EnumCase], isCoinductive: Boolean)

  val defaultEffects: Map[String, EffectRef] = Map("io" -> EffectRef.Builtin(BuiltinEffect.Io))
  val defaultBuiltins: Set[String] = Set(
    "Integer",
    "Int",
    "String",
    "Bool",
    "Nat",
    "Natural",
    "Type",
    "Level",
    "U",
    "Universe",
    "true",
    "false",
    "List",
    "println"
  )

  val defaultBuiltinTypes: Map[String, AST] = {
    val stringParam = Param(Uniqid.make, "value", AST.StringType(None), Implicitness.Explicit, None)
    val tele = Vector(Telescope(Vector(stringParam), Implicitness.Explicit))
    val unitTy = AST.TupleType(Vector.empty, None)
    val printlnTy = AST.Pi(tele, unitTy, Vector(ElabContext.defaultEffects("io")), None)
    Map(
      "println" -> printlnTy,
      "Level" -> AST.Type(AST.LevelLit(0, None), None)
    )
  }

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

private case class DefInfo(
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
      // Don't substitute bound variables
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p =>
            Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))
          ),
          tel.implicitness
        )
      }
      val boundIds = telescopes.flatMap(_.params.map(_.id)).toSet
      val filteredSubs = substitutions.filterNot { case (id, _) => boundIds.contains(id) }
      AST.Pi(newTelescopes, substituteInType(resultTy, filteredSubs), effects, span)
    case AST.Lam(telescopes, body, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p =>
            Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))
          ),
          tel.implicitness
        )
      }
      val boundIds = telescopes.flatMap(_.params.map(_.id)).toSet
      val filteredSubs = substitutions.filterNot { case (id, _) => boundIds.contains(id) }
      AST.Lam(newTelescopes, substituteInType(body, filteredSubs), span)
    case AST.App(func, args, implicitArgs, span) =>
      AST.App(
        substituteInType(func, substitutions),
        args.map(a => Arg(substituteInType(a.value, substitutions), a.implicitness)),
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
  case StmtAST.Def(id, name, telescopes, resultTy, body, span) =>
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
    StmtAST.Def(id, name, newTelescopes, resultTy.map(substituteInType(_, filteredSubs)), substituteInType(body, filteredSubs), span)
  case StmtAST.Record(id, name, fields, span) =>
    val newFields =
      fields.map(p => Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions))))
    StmtAST.Record(id, name, newFields, span)
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

/** Handler for elaboration constraints */
class ElabHandler extends Handler[ElabConstraint]:

  def run[M <: SolverModule](constraint: ElabConstraint)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    constraint match
      case c: ElabConstraint.Check => handleCheck(c)
      case c: ElabConstraint.Infer => handleInferExpr(c)
      case c: ElabConstraint.InferTopLevel =>
        handleInferTopLevel(c)
      case c: ElabConstraint.Unify       => handleUnify(c)
      case c: ElabConstraint.Subtype     => handleSubtype(c)
      case c: ElabConstraint.IsUniverse  => handleIsUniverse(c)
      case c: ElabConstraint.IsPi        => handleIsPi(c)
      case c: ElabConstraint.AssembleApp => handleAssembleApp(c)
      case c: ElabConstraint.AssembleAnn => handleAssembleAnn(c)
      case c: ElabConstraint.AssembleDef => handleAssembleDef(c)
  }

  def canDefaulting(level: DefaultingLevel): Boolean = true
  override def defaulting[M <: SolverModule](constraint: ElabConstraint, level: DefaultingLevel)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Boolean = {
    def handleLit(cst: CST, result: module.CellRW[AST], inferredTy: module.CellRW[AST]): Boolean = {
      cst match
        case CST.IntegerLiteral(value, span) =>
          var changed = false
          if !module.hasStableValue(solver, result) then
            module.fill(solver, result, AST.IntLit(value, span))
            changed = true
          if !module.hasStableValue(solver, inferredTy) then
            module.fill(solver, inferredTy, AST.IntegerType(None))
            changed = true
          changed
        case _ => false
    }

    level match
      case DefaultingLevel.Lit =>
        constraint match
          case c: ElabConstraint.Infer =>
            handleLit(c.cst, c.result.asInstanceOf[module.CellRW[AST]], c.inferredTy.asInstanceOf[module.CellRW[AST]])
          case c: ElabConstraint.InferTopLevel =>
            handleLit(c.cst, c.result.asInstanceOf[module.CellRW[AST]], c.inferredTy.asInstanceOf[module.CellRW[AST]])
          case _ => false
      case _ => false
  }

  private def handleCheck[M <: SolverModule](c: ElabConstraint.Check)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    import module.given

    module.readStable(solver, c.expectedTy) match
      case Some(AST.LevelType(_)) =>
        c.cst match
          case CST.IntegerLiteral(value, span) if value.sign >= 0 =>
            module.fill(solver, c.result, AST.LevelLit(value, span))
            Result.Done
          case _ =>
            val inferredTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(c.cst, c.result, inferredTy, c.ctx))
            module.addConstraint(solver, ElabConstraint.Unify(inferredTy, c.expectedTy, c.cst.span, c.ctx))
            Result.Done
      case Some(AST.NaturalType(_)) =>
        c.cst match
          case CST.IntegerLiteral(value, span) if value.sign >= 0 =>
            module.fill(solver, c.result, AST.NaturalLit(value, span))
            Result.Done
          case _ =>
            val inferredTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(c.cst, c.result, inferredTy, c.ctx))
            module.addConstraint(solver, ElabConstraint.Unify(inferredTy, c.expectedTy, c.cst.span, c.ctx))
            Result.Done
      case Some(_) =>
        val inferredTy = module.newOnceCell[ElabConstraint, AST](solver)
        module.addConstraint(solver, ElabConstraint.Infer(c.cst, c.result, inferredTy, c.ctx))
        module.addConstraint(solver, ElabConstraint.Unify(inferredTy, c.expectedTy, c.cst.span, c.ctx))
        Result.Done
      case None =>
        Result.Waiting(c.expectedTy)
  }

  private def handleInferExpr[M <: SolverModule](c: ElabConstraint.Infer)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Result = {
    import module.given

    def addInferConstraint(cst: CST, result: CellRW[AST], inferredTy: CellRW[AST], ctx: ElabContext, asType: Boolean = false): Unit =
      module.addConstraint(solver, ElabConstraint.Infer(cst, result, inferredTy, ctx, asType))

    // Check if we've already filled the result and type - if so, we're done (avoid re-processing)
    if module.hasStableValue(solver, c.result) && module.hasStableValue(solver, c.inferredTy) then return Result.Done

    c.cst match
      // Integer literal: default to Integer, but in type mode treat as a level literal
      case CST.IntegerLiteral(value, span) =>
        if c.asType && value.sign >= 0 then
          val ast = AST.LevelLit(value, span)
          module.fill(solver, c.result, ast)
          module.fill(solver, c.inferredTy, AST.LevelType(None))
        else {
          val ast = AST.IntLit(value, span)
          module.fill(solver, c.result, ast)
          module.fill(solver, c.inferredTy, AST.IntegerType(None))
        }
        Result.Done

      // String literal: type is the built-in String type
      case CST.StringLiteral(value, span) =>
        val ast = AST.StringLit(value, span)
        module.fill(solver, c.result, ast)
        module.fill(solver, c.inferredTy, AST.StringType(None))
        Result.Done

      // Symbol: lookup in context
      case CST.Symbol(name, span) =>
        // Builtins first to avoid creating Refs for well-known types like String
        if name == "Any" then
          val ast = AST.AnyType(None)
          module.fill(solver, c.result, ast)
          // Any has type Type(0)
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
          Result.Done
        else if name == "Level" then
          val ast = AST.LevelType(span)
          module.fill(solver, c.result, ast)
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
          Result.Done
        else if name == "String" then
          val ast = AST.StringType(None)
          module.fill(solver, c.result, ast)
          // String has type Type(0)
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
          Result.Done
        else if name == "Natural" then
          val ast = AST.NaturalType(None)
          module.fill(solver, c.result, ast)
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
          Result.Done
        else if name == "Integer" then
          val ast = AST.IntegerType(None)
          module.fill(solver, c.result, ast)
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
          Result.Done
        else if name == "Unit" then
          val ast = AST.TupleType(Vector.empty, span)
          module.fill(solver, c.result, ast)
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
          Result.Done
        else if name == "Type" then
          // Type is a function from a level to its universe Type(n)
          val levelParamId = Uniqid.make[AST]
          val levelParamTy = AST.LevelType(None)
          val levelParam = Param(levelParamId, "n", levelParamTy, Implicitness.Explicit, None)
          val levelTele = Vector(Telescope(Vector(levelParam), Implicitness.Explicit))
          // Value: λ n : Level => Type(n)
          val ast = AST.Lam(levelTele, AST.Type(AST.Ref(levelParamId, "n", span), span), span)
          module.fill(solver, c.result, ast)
          // Type of Type: (n : Level) -> Typeω(n)
          val typeOfType = AST.Pi(
            levelTele,
            AST.TypeOmega(AST.Ref(levelParamId, "n", span), None),
            Vector.empty,
            span
          )
          module.fill(solver, c.inferredTy, typeOfType)
          Result.Done
        else if name == "List" then
          val elemParamId = Uniqid.make[AST]
          val paramTy = AST.Type(AST.LevelLit(0, None), None)
          val param = Param(elemParamId, "A", paramTy, Implicitness.Explicit, None)
          val lamTelescopes = Vector(Telescope(Vector(param), Implicitness.Explicit))
          val body = AST.ListType(AST.Ref(elemParamId, "A", span), span)
          val lam = AST.Lam(lamTelescopes, body, span)
          module.fill(solver, c.result, lam)

          val typeParamId = Uniqid.make[AST]
          val typeParam = Param(typeParamId, "A", paramTy, Implicitness.Explicit, None)
          val typeTele = Vector(Telescope(Vector(typeParam), Implicitness.Explicit))
          val lamType = AST.Pi(typeTele, AST.Type(AST.LevelLit(1, None), None), Vector.empty, span)
          module.fill(solver, c.inferredTy, lamType)
          Result.Done
        else
          c.ctx.lookup(name) match
            case Some(id) =>
              val ast = AST.Ref(id, name, span)
              module.fill(solver, c.result, ast)
              c.ctx.lookupType(id) match
                case Some(tyCell) =>
                  // Copy the type cell content
                  module.readStable(solver, tyCell) match
                    case Some(ty) =>
                      module.fill(solver, c.inferredTy, ty)
                      Result.Done
                    case None =>
                      Result.Waiting(tyCell)
                case None =>
                  // Variable without type - use a meta-variable
                  val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
                  module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
                  Result.Done
            case None =>
              // Built-in but not handled above or unknown identifier
              if c.ctx.isBuiltin(name) then
                // Built-in: create a special reference
                val metaId = Uniqid.make[AST]
                val ast = AST.Ref(metaId, name, span)
                module.fill(solver, c.result, ast)
                // Look up builtin type
                c.ctx.lookupBuiltinType(name) match
                  case Some(ty) =>
                    module.fill(solver, c.inferredTy, ty)
                  case None =>
                    // Default: Type(0)
                    module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
                Result.Done
              else {
                // Unbound variable - report error and recover
                println(s"[debug-unbound] symbol $name at $span in asType=${c.asType}, cst=${c.cst}, ctxBindings=${c.ctx.bindings.keySet}")
                c.ctx.reporter.report(ElabProblem.UnboundVariable(name, span))

                // Error recovery: create a meta-variable to continue elaboration
                val metaId = Uniqid.make[AST]
                val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
                val ast = AST.Ref(metaId, name, span)
                module.fill(solver, c.result, ast)
                module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
                Result.Done
              }

      // Tuple: infer each element (lazily zonked via MetaCells)
      case CST.Tuple(elements, span) =>
        val elemResults = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))
        val elemTypes = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))

        elements.zip(elemResults).zip(elemTypes).foreach { case ((cstElem, resultCell), tyCell) =>
          module.addConstraint(solver, ElabConstraint.Infer(cstElem, resultCell, tyCell, c.ctx, asType = c.asType))
        }

        val tupleElems = elemResults.zip(elements).map { case (cell, elemCst) =>
          AST.MetaCell(HoldNotReadable(cell), elemCst.span)
        }

        if c.asType then
          module.fill(solver, c.result, AST.TupleType(tupleElems, span))
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), span))
        else {
          module.fill(solver, c.result, AST.Tuple(tupleElems, span))
          val tupleTypes = elemTypes.zip(elements).map { case (cell, elemCst) =>
            AST.MetaCell(HoldNotReadable(cell), elemCst.span)
          }
          module.fill(solver, c.inferredTy, AST.TupleType(tupleTypes, None))
        }
        Result.Done

      // List literal: infer elements
      case CST.ListLiteral(elements, span) =>
        val elemResults = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))
        val elemTypes = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))

        elements.zip(elemResults).zip(elemTypes).foreach { case ((cstElem, resultCell), tyCell) =>
          module.addConstraint(solver, ElabConstraint.Infer(cstElem, resultCell, tyCell, c.ctx))
        }

        val astElems = elemResults.zip(elements).map { case (cell, elemCst) =>
          AST.MetaCell(HoldNotReadable(cell), elemCst.span)
        }
        module.fill(solver, c.result, AST.ListLit(astElems, span))

        if elemTypes.nonEmpty then
          val baseTy = elemTypes.head
          elemTypes.tail.foreach(tyCell => module.addConstraint(solver, ElabConstraint.Unify(baseTy, tyCell, span, c.ctx)))
          module.fill(solver, c.inferredTy, AST.ListType(AST.MetaCell(HoldNotReadable(baseTy), span), span))
        else {
          val elemMeta = module.newOnceCell[ElabConstraint, AST](solver)
          module.fill(solver, c.inferredTy, AST.ListType(AST.MetaCell(HoldNotReadable(elemMeta), span), span))
        }
        Result.Done

      // Block: infer each element with two-pass elaboration for defs
      case CST.Block(elements, tail, span) =>
        elaborateBlockLike(c.ctx, c.result, c.inferredTy, elements, tail, span)

      // SeqOf: could be function application or sequence (block-only statements handled separately)
      case CST.SeqOf(elements, span) =>
        val elems = elements.toVector

        // Reject block-only statements in expression context
        elems.headOption match
          case Some(CST.Symbol("def", _)) =>
            c.ctx.reporter.report(
              ElabProblem.UnboundVariable("def statement only allowed in block elements, not at top level or in expressions", span)
            )
            val metaId = Uniqid.make[AST]
            val ast = AST.Ref(metaId, "def", span)
            module.fill(solver, c.result, ast)
            val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
            Result.Done
          case Some(CST.Symbol("let", _)) =>
            c.ctx.reporter.report(
              ElabProblem.UnboundVariable("let statement only allowed in block elements, not at top level or in expressions", span)
            )
            val metaId = Uniqid.make[AST]
            val ast = AST.Ref(metaId, "let", span)
            module.fill(solver, c.result, ast)
            val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
            Result.Done
          case Some(CST.Symbol("effect", _)) =>
            c.ctx.reporter.report(
              ElabProblem.UnboundVariable("effect statement only allowed in block elements, not at top level or in expressions", span)
            )
            val metaId = Uniqid.make[AST]
            val ast = AST.Ref(metaId, "effect", span)
            module.fill(solver, c.result, ast)
            val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
            Result.Done
          case Some(CST.Symbol("record", _)) =>
            c.ctx.reporter.report(
              ElabProblem.UnboundVariable("record statement only allowed in block elements, not at top level or in expressions", span)
            )
            val metaId = Uniqid.make[AST]
            val ast = AST.Ref(metaId, "record", span)
            module.fill(solver, c.result, ast)
            val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
            Result.Done
          case Some(CST.Symbol("enum", _)) | Some(CST.Symbol("coenum", _)) =>
            c.ctx.reporter.report(
              ElabProblem.UnboundVariable("enum statement only allowed in block elements; add ';' to terminate it", span)
            )
            val metaId = Uniqid.make[AST]
            val ast = AST.Ref(metaId, "enum", span)
            module.fill(solver, c.result, ast)
            val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
            Result.Done
          case _ =>
            if elems.headOption.exists { case CST.Symbol("package", _) => true; case _ => false } then
              // package name [body...]
              if elems.length < 2 then
                c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected package name", span))
                module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
                module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
                Result.Done
              else {
                val pkgName = elems(1) match
                  case CST.Symbol(n, _) => n
                  case _ =>
                    c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected package name", span))
                    "<error>"
                val bodyCst = {
                  if elems.length > 2 then
                    val rest = elems.drop(2)
                    if rest.length == 1 then rest.head
                    else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(rest), combinedSpan(rest))
                  else CST.Block(Vector.empty, None, span)
                }

                val bodyResult = module.newOnceCell[ElabConstraint, AST](solver)
                val bodyTy = module.newOnceCell[ElabConstraint, AST](solver)
                module.addConstraint(solver, ElabConstraint.Infer(bodyCst, bodyResult, bodyTy, c.ctx))

                val pkgStmt = StmtAST.Pkg(pkgName, AST.MetaCell(HoldNotReadable(bodyResult), span), span)
                val block = AST.Block(Vector(pkgStmt), AST.MetaCell(HoldNotReadable(bodyResult), span), span)
                module.fill(solver, c.result, block)
                module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(bodyTy), span))
                Result.Done
              }
            else {
              tryHandleDotSequence(c.ctx, c.result, c.inferredTy, false, elems, span) match
                case Some(r) => r
                case None =>
                  annotationPattern(elems) match
                    case Some((exprCst, typeCst)) =>
                      handleAnnotatedExpression(c.ctx, c.result, c.inferredTy, exprCst, typeCst, span)
                    case None =>
                      // Normalize chained applications like f(a)(b) or f[a](b)(c)
                      normalizeApplicationSeq(elems) match
                        case Some(normalized) =>
                          module.addConstraint(solver, ElabConstraint.Infer(normalized, c.result, c.inferredTy, c.ctx))
                          Result.Done
                        case None =>
                          val maybeTuple = elems.lastOption.collect { case t: CST.Tuple => t }
                          val maybeTypeArgs =
                            if elems.length >= 2 then elems(elems.length - 2) match
                              case l: CST.ListLiteral => Some(l)
                              case _                  => None
                            else None

                          (maybeTuple, maybeTypeArgs) match
                            case (Some(tuple), Some(typeArgs)) =>
                              val funcElems = elems.dropRight(2)
                              if funcElems.nonEmpty then
                                val funcCst =
                                  if funcElems.length == 1 then funcElems.head
                                  else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(funcElems), combinedSpan(funcElems))
                                handleFunctionApplication(c.ctx, c.asType, c.result, c.inferredTy, funcCst, Some(typeArgs), tuple, span)
                              else
                                val blockCst = CST.Block(elems.dropRight(1), Some(elems.last), span)
                                module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
                                Result.Done
                            case (Some(tuple), None) =>
                              val funcElems = elems.dropRight(1)
                              if funcElems.nonEmpty then
                                val funcCst =
                                  if funcElems.length == 1 then funcElems.head
                                  else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(funcElems), combinedSpan(funcElems))
                                handleFunctionApplication(c.ctx, c.asType, c.result, c.inferredTy, funcCst, None, tuple, span)
                              else {
                                val blockCst = CST.Block(elems.dropRight(1), Some(elems.last), span)
                                module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
                                Result.Done
                              }
                            case _ =>
                              // Default: treat as a block-like sequence
                              val blockCst = CST.Block(elems.dropRight(1), Some(elems.last), span)
                              module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
                              Result.Done
            }
  }

  private def handleInferTopLevel[M <: SolverModule](c: ElabConstraint.InferTopLevel)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Result = {
    import module.given

    if module.hasStableValue(solver, c.result) && module.hasStableValue(solver, c.inferredTy) then return Result.Done

    c.cst match
      case CST.Block(elements, tail, span) =>
        elaborateBlockLike(c.ctx, c.result, c.inferredTy, elements, tail, span)

      case CST.SeqOf(elements, span) =>
        val elems = elements.toVector
        elems.headOption match
          case Some(CST.Symbol("def", _)) | Some(CST.Symbol("effect", _)) | Some(CST.Symbol("record", _)) |
              Some(CST.Symbol("enum", _)) | Some(CST.Symbol("coenum", _)) =>
            val blockElem = CST.SeqOf(NonEmptyVector.fromVectorUnsafe(elems), span)
            elaborateBlockLike(c.ctx, c.result, c.inferredTy, Vector(blockElem), None, span)
          case Some(CST.Symbol("let", _)) =>
            c.ctx.reporter.report(
              ElabProblem.UnboundVariable("let statement only allowed in block elements, not at top level or in expressions", span)
            )
            val metaId = Uniqid.make[AST]
            val ast = AST.Ref(metaId, "let", span)
            module.fill(solver, c.result, ast)
            val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
            Result.Done
          case _ =>
            if elems.headOption.exists { case CST.Symbol("package", _) => true; case _ => false } then
              if elems.length < 2 then
                c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected package name", span))
                module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
                module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
                Result.Done
              else {
                val pkgName = elems(1) match
                  case CST.Symbol(n, _) => n
                  case _ =>
                    c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected package name", span))
                    "<error>"
                val bodyCst = {
                  if elems.length > 2 then
                    val rest = elems.drop(2)
                    if rest.length == 1 then rest.head
                    else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(rest), combinedSpan(rest))
                  else CST.Block(Vector.empty, None, span)
                }

                val bodyResult = module.newOnceCell[ElabConstraint, AST](solver)
                val bodyTy = module.newOnceCell[ElabConstraint, AST](solver)
                module.addConstraint(solver, ElabConstraint.InferTopLevel(bodyCst, bodyResult, bodyTy, c.ctx))

                val pkgStmt = StmtAST.Pkg(pkgName, AST.MetaCell(HoldNotReadable(bodyResult), span), span)
                val block = AST.Block(Vector(pkgStmt), AST.MetaCell(HoldNotReadable(bodyResult), span), span)
                module.fill(solver, c.result, block)
                module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(bodyTy), span))
                Result.Done
              }
            else {
              module.addConstraint(solver, ElabConstraint.Infer(c.cst, c.result, c.inferredTy, c.ctx))
              Result.Done
            }
      case other =>
        module.addConstraint(solver, ElabConstraint.Infer(other, c.result, c.inferredTy, c.ctx))
        Result.Done
  }

  /** Handle function application: f(args) or f[typeArgs](args) */
  private def handleFunctionApplication[M <: SolverModule](
      ctx: ElabContext,
      asType: Boolean,
      resultCell: CellRW[AST],
      inferredTyCell: CellRW[AST],
      funcCst: CST,
      explicitTypeArgs: Option[CST.ListLiteral],
      argsTuple: CST.Tuple,
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result = {

    funcCst match
      case CST.Symbol(name, funcSpan) if ctx.lookupRecord(name).isDefined =>
        val rec = ctx.lookupRecord(name).get
        val fields = rec.fields
        val argPairs = argsTuple.elements
          .zipAll(
            fields,
            CST.Symbol("<missing>", span),
            Param(
              Uniqid.make,
              "<missing>",
              AST.MetaCell(HoldNotReadable(module.newOnceCell[ElabConstraint, AST](solver)), None),
              Implicitness.Explicit,
              None
            )
          )
          .map { case (argCst, field) =>
            val argResult = module.newOnceCell[ElabConstraint, AST](solver)
            val argTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(argCst, argResult, argTy, ctx, asType = asType))
            // If we know the expected field type, add a unification constraint
            if field.name != "<missing>" then
              val expectedTyCell = module.newOnceCell[ElabConstraint, AST](solver)
              module.fill(solver, expectedTyCell, field.ty)
              module.addConstraint(solver, ElabConstraint.Unify(argTy, expectedTyCell, span, ctx))
              field.ty match
                case AST.MetaCell(HoldNotReadable(cell), _) if !module.hasStableValue(solver, cell.asInstanceOf[module.CellRW[AST]]) =>
                  module.readStable(solver, argTy) match
                    case Some(argTypeAst) => module.fill(solver, cell.asInstanceOf[module.CellRW[AST]], argTypeAst)
                    case None             => ()
                case _ => ()
            (argResult, argCst)
          }
        val argAsts = argPairs.map { case (cell, argCst) => AST.MetaCell(HoldNotReadable(cell), argCst.span) }.toVector
        module.fill(solver, resultCell, AST.RecordCtor(rec.id, name, argAsts, span))
        module.fill(solver, inferredTyCell, AST.RecordTypeRef(rec.id, name, span))
        return Result.Done
      case _ => ()

    def resolveEnumCase(cst: CST): Option[(ElabContext.EnumDef, EnumCase)] = cst match
      case CST.SeqOf(seqElems, _) =>
        seqElems.toVector match
          case Vector(CST.Symbol(enumName, _), CST.Symbol(".", _), CST.Symbol(caseName, _)) =>
            ctx.lookupEnum(enumName).flatMap(en => en.cases.find(_.name == caseName).map(en -> _))
          case _ => None
      case _ => None

    // Elaborate function
    val funcResult = module.newOnceCell[ElabConstraint, AST](solver)
    val funcTy = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(funcCst, funcResult, funcTy, ctx, asType = asType))

    // Elaborate explicit type arguments if provided
    val typeArgPairs = explicitTypeArgs
      .map { typeArgsList =>
        typeArgsList.elements.map { typeArg =>
          val typeArgResult = module.newOnceCell[ElabConstraint, AST](solver)
          val typeArgTy = module.newOnceCell[ElabConstraint, AST](solver)
          module.addConstraint(solver, ElabConstraint.Infer(typeArg, typeArgResult, typeArgTy, ctx, asType = true))
          (typeArgResult, typeArgTy)
        }
      }
      .getOrElse(Vector.empty)
    val typeArgResults = typeArgPairs.map(_._1)
    val typeArgTypes = typeArgPairs.map(_._2)

    // Elaborate regular arguments
    val argPairs = argsTuple.elements.map { arg =>
      val argResult = module.newOnceCell[ElabConstraint, AST](solver)
      val argTy = module.newOnceCell[ElabConstraint, AST](solver)
      module.addConstraint(solver, ElabConstraint.Infer(arg, argResult, argTy, ctx, asType = asType))
      (argResult, argTy)
    }
    val argResults = argPairs.map(_._1)
    val argTypes = argPairs.map(_._2)

    // Add constraint to assemble application once all parts are elaborated
    module.addConstraint(
      solver,
      ElabConstraint.AssembleApp(
        funcResult,
        funcTy,
        typeArgResults,
        typeArgTypes,
        argResults,
        argTypes,
        resultCell,
        inferredTyCell,
        span,
        ctx
      )
    )

    Result.Done
  }

  /** Elaborate a block-like group of statements (shared by real blocks and top-level blockish SeqOf forms). */
  private def elaborateBlockLike[M <: SolverModule](
      ctx: ElabContext,
      resultCell: CellRW[AST],
      inferredTyCell: CellRW[AST],
      elements: Vector[CST],
      tail: Option[CST],
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    import module.given

    // Check if tail contains a def/let/record statement (which is not allowed)
    tail.foreach { t =>
      if isDefStatement(t) then ctx.reporter.report(ElabProblem.UnboundVariable("def statement not allowed in tail position", t.span))
      if isLetStatement(t) then ctx.reporter.report(ElabProblem.UnboundVariable("let statement not allowed in tail position", t.span))
      if isRecordStatement(t) then ctx.reporter.report(ElabProblem.UnboundVariable("record statement not allowed in tail position", t.span))
      if isEnumStatement(t) then
        ctx.reporter.report(ElabProblem.UnboundVariable("enum/coenum statement not allowed in tail position; terminate with ';'", t.span))
    }

    // TWO-PASS ELABORATION for forward references:
    // Pass 1: Scan elements, collect all def declarations, create placeholder cells (reusing any pre-bound ones)
    var enrichedCtx = ctx
    val defInfoMap = scala.collection.mutable.Map.empty[CST, DefInfo]
    val recordInfoMap = scala.collection.mutable.Map.empty[CST, (String, UniqidOf[AST], module.CellRW[AST])]
    val enumInfoMap = scala.collection.mutable.Map.empty[CST, (String, UniqidOf[AST], Vector[Param], Boolean)]

    elements.foreach {
      case elem @ CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
        // Extract def name
        val elems = seqElems.toVector
        elems match
          case _ +: CST.Symbol(name, _) +: _ =>
            // Reuse existing binding if present (for cross-file/module pre-pass), otherwise create new
            val (defId, defTypeCell) = enrichedCtx.lookup(name) match
              case Some(existingId) =>
                val cell = enrichedCtx.lookupType(existingId).getOrElse(module.newOnceCell[ElabConstraint, AST](solver))
                (existingId, cell)
              case None =>
                val newId = Uniqid.make[AST]
                val cell = module.newOnceCell[ElabConstraint, AST](solver)
                (newId, cell)
            val defResultCell = module.newOnceCell[ElabConstraint, AST](solver)
            // Add to context so it can be referenced (including by other defs)
            enrichedCtx = enrichedCtx.bind(name, defId, defTypeCell).registerDefBody(defId, defResultCell)
            defInfoMap(elem) = DefInfo(name, defId, defTypeCell, defResultCell, span = elem.span)
          case _ => ()
      case CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("effect", _) => true; case _ => false } =>
        seqElems.lift(1) match
          case Some(CST.Symbol(name, _)) =>
            enrichedCtx = enrichedCtx.declareEffect(name)
          case _ => ()
      case elem @ CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("record", _) => true; case _ => false } =>
        seqElems.lift(1) match
          case Some(CST.Symbol(name, _)) =>
            val recordId = Uniqid.make[AST]
            val ctorTyCell = enrichedCtx.lookup(name).flatMap(enrichedCtx.lookupType).getOrElse(module.newOnceCell[ElabConstraint, AST](solver))
            enrichedCtx = enrichedCtx.registerRecordPlaceholder(name, recordId, ctorTyCell)
            recordInfoMap(elem) = (name, recordId, ctorTyCell)
          case _ => ()
      case elem @ CST.SeqOf(seqElems, _) if seqElems.headOption.exists {
            case CST.Symbol("enum", _) | CST.Symbol("coenum", _) => true
            case _ => false
          } =>
        seqElems.lift(1) match
          case Some(CST.Symbol(name, _)) =>
            val typeParams = parseEnumTypeParams(seqElems.toVector, enrichedCtx)
            val enumId = Uniqid.make[AST]
            val isCo = seqElems.headOption.exists { case CST.Symbol("coenum", _) => true; case _ => false }
            enrichedCtx = enrichedCtx.registerEnumPlaceholder(name, enumId, typeParams, isCo)
            enumInfoMap(elem) = (name, enumId, typeParams, isCo)
          case _ => ()
      case _ => ()
    }

    // Pass 2: Elaborate all elements with enriched context and sequential lets
    val elaboratedElemsBuffer = scala.collection.mutable.ArrayBuffer.empty[StmtAST]
    var currentCtx = enrichedCtx

    elements.foreach { elem =>
      val elemResult = {
        defInfoMap.get(elem) match
          case Some(info) => info.resultCell
          case None       => module.newOnceCell[ElabConstraint, AST](solver)
      }
      val elemType = module.newOnceCell[ElabConstraint, AST](solver)

      elem match
        case CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
          val elems = seqElems.toVector
          val info = defInfoMap(elem)
          val defConstraint: ElabConstraint.Infer = ElabConstraint.Infer(elem, elemResult, elemType, currentCtx)
          handleDefStatement(defConstraint, elems, elem.span, info.id, info.tyCell, currentCtx, defInfoMap)(using module, solver)
          // Construct a Stmt placeholder with MetaCells for body/result type
          val bodyAst = AST.MetaCell(HoldNotReadable(elemResult), elem.span)
          val resTyAst = info.resultTyCell.map(c => AST.MetaCell(HoldNotReadable(c), elem.span))
          val stmt = StmtAST.Def(info.id, info.name, info.telescopes, resTyAst, bodyAst, elem.span)
          elaboratedElemsBuffer += stmt
        case CST.SeqOf(seqElems, span) if seqElems.headOption.exists { case CST.Symbol("effect", _) => true; case _ => false } =>
          val (maybeName, maybeBody) = (seqElems.lift(1), seqElems.lift(2))
          maybeName.collect { case CST.Symbol(name, _) => name } match
            case Some(name) =>
              // Declare the effect and elaborate its operation signatures (if any)
              val withEffectCtx = currentCtx.declareEffect(name)
              val effRef = withEffectCtx.lookupEffect(name).get
              val bodyCst: CST.Block = maybeBody.collect { case b: CST.Block => b }.getOrElse(CST.Block(Vector.empty, None, span))
              currentCtx = processEffectBody(bodyCst, effRef, withEffectCtx)
              // The effect declaration itself evaluates to unit
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
            case None =>
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
          elaboratedElemsBuffer += StmtAST.ExprStmt(AST.MetaCell(HoldNotReadable(elemResult), span), span)
        case CST.SeqOf(seqElems, span) if seqElems.headOption.exists { case CST.Symbol("record", _) => true; case _ => false } =>
          val recordMeta = recordInfoMap(elem)
          val fields = parseRecordFields(seqElems.toVector, currentCtx)(using module, solver)
          val ctorType = AST.Pi(
            Vector(Telescope(fields, Implicitness.Explicit)),
            AST.RecordTypeRef(recordMeta._2, recordMeta._1, span),
            Vector.empty,
            span
          )
          module.fill(solver, recordMeta._3, ctorType)
          currentCtx = currentCtx.updateRecord(recordMeta._1, fields)
          module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
          module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
          elaboratedElemsBuffer += StmtAST.Record(recordMeta._2, recordMeta._1, fields, span)
        case CST.SeqOf(seqElems, span) if seqElems.headOption.exists {
              case CST.Symbol("enum", _) | CST.Symbol("coenum", _) => true
              case _ => false
            } =>
          enumInfoMap.get(elem) match
            case Some((name, enumId, typeParams, isCo)) =>
              val cases = parseEnumCases(seqElems.toVector, currentCtx, typeParams)(using module, solver)
              val resolvedTypeParams =
                typeParams.map { p =>
                  val resolved = normalizeTypeLikeKind(substituteSolutions(p.ty))
                  p.copy(ty = resolved, default = p.default.map(substituteSolutions))
                }
              val resolvedCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))))
              currentCtx = currentCtx.updateEnum(name, resolvedTypeParams, resolvedCases, isCo)
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
              if isCo then elaboratedElemsBuffer += StmtAST.Coenum(enumId, name, resolvedTypeParams, resolvedCases, span)
              else elaboratedElemsBuffer += StmtAST.Enum(enumId, name, resolvedTypeParams, resolvedCases, span)
            case None =>
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
        case CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("let", _) => true; case _ => false } =>
          val elems = seqElems.toVector
          currentCtx = handleLetStatement(elems, elem.span, currentCtx, elemResult, elemType)(using module, solver)
          elaboratedElemsBuffer += StmtAST.ExprStmt(AST.MetaCell(HoldNotReadable(elemResult), elem.span), elem.span)
        case _ =>
          module.addConstraint(solver, ElabConstraint.Infer(elem, elemResult, elemType, currentCtx))
          elaboratedElemsBuffer += StmtAST.ExprStmt(AST.MetaCell(HoldNotReadable(elemResult), elem.span), elem.span)
    }

    val elaboratedElems = elaboratedElemsBuffer.toVector

    // Elaborate tail (or use empty tuple if None)
    val elaboratedTail = tail match
      case Some(t) =>
        val tailResult = module.newOnceCell[ElabConstraint, AST](solver)
        // Type of block is type of tail - use c.inferredTy directly
        module.addConstraint(solver, ElabConstraint.Infer(t, tailResult, inferredTyCell, currentCtx))
        AST.MetaCell(HoldNotReadable(tailResult), t.span)
      case None =>
        // Empty tail = unit value (empty tuple)
        val emptyTuple = AST.Tuple(Vector.empty, span)
        module.fill(solver, inferredTyCell, AST.TupleType(Vector.empty, None))
        emptyTuple

    // Construct block directly with MetaCells - they'll be resolved by substituteSolutions
    val block = AST.Block(elaboratedElems, elaboratedTail, span)
    module.fill(solver, resultCell, block)
    Result.Done
  }

  /** Handle dotted sequences such as `Vec2d.t` or `point.x`. */
  private def tryHandleDotSequence[M <: SolverModule](
      ctx: ElabContext,
      resultCell: CellRW[AST],
      inferredTyCell: CellRW[AST],
      asType: Boolean,
      elems: Vector[CST],
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint]): Option[Result] = {
    import module.given

    def fillResultOnce(cell: module.CellRW[AST], value: AST): Unit =
      if !module.hasStableValue(solver, cell) then module.fill(solver, cell, value)

    elems match
      case Vector(CST.Symbol(name, _), CST.Symbol(".", _), CST.Symbol("t", _)) =>
        ctx.lookupEnum(name) match
          case Some(en) =>
            val ast = AST.EnumTypeRef(en.id, name, span)
            fillResultOnce(resultCell, ast)
            val teleImplicitness = en.typeParams.headOption.map(_.implicitness).getOrElse(Implicitness.Explicit)
            val enumTy =
              if en.typeParams.nonEmpty then
                AST.Pi(Vector(Telescope(en.typeParams, teleImplicitness)), AST.Type(AST.LevelLit(0, None), span), Vector.empty, span)
              else AST.Type(AST.LevelLit(0, None), None)
            fillResultOnce(inferredTyCell, enumTy)
            Some(Result.Done)
          case None =>
            ctx.lookupRecord(name) match
              case Some(rec) =>
                val ast = AST.RecordTypeRef(rec.id, name, span)
                fillResultOnce(resultCell, ast)
                fillResultOnce(inferredTyCell, AST.Type(AST.LevelLit(0, None), None))
                Some(Result.Done)
              case None => None
      case Vector(lhs @ CST.Symbol(enumOrVal, _), CST.Symbol(".", _), CST.Symbol(field, _)) =>
        ctx.lookupEnum(enumOrVal) match
          case Some(enumDef) =>
            enumDef.cases.find(_.name == field) match
              case Some(caseDef) =>
                val teleImplicitness = enumDef.typeParams.headOption.map(_.implicitness).getOrElse(Implicitness.Explicit)
                val typeParamTele = if enumDef.typeParams.nonEmpty then Vector(Telescope(enumDef.typeParams, teleImplicitness)) else Vector.empty
                val caseTele = if caseDef.params.nonEmpty then Vector(Telescope(caseDef.params, Implicitness.Explicit)) else Vector.empty
                val typeArgs = enumDef.typeParams.map(p => Arg(AST.Ref(p.id, p.name, span), Implicitness.Explicit))
                val enumTy =
                  if typeArgs.nonEmpty then AST.App(AST.EnumTypeRef(enumDef.id, enumOrVal, span), typeArgs, implicitArgs = false, span)
                  else AST.EnumTypeRef(enumDef.id, enumOrVal, span)
                val ctorType =
                  if typeParamTele.isEmpty && caseTele.isEmpty then enumTy
                  else AST.Pi(typeParamTele ++ caseTele, enumTy, Vector.empty, span)
                val ast = AST.EnumCaseRef(enumDef.id, caseDef.id, enumOrVal, field, span)
                fillResultOnce(resultCell, ast)
                fillResultOnce(inferredTyCell, ctorType)
                Some(Result.Done)
              case None => None
          case None =>
            // Fallback to regular field access
            val targetResult = module.newOnceCell[ElabConstraint, AST](solver)
            val targetTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(lhs, targetResult, targetTy, ctx, asType = asType))
            val targetAst = AST.MetaCell(HoldNotReadable(targetResult), lhs.span)
            val accessAst = AST.FieldAccess(targetAst, field, span)
            fillResultOnce(resultCell, accessAst)
            module.readStable(solver, targetTy) match
              case None =>
                lhs match
                  case CST.Symbol(sym, _) =>
                    (for
                      id <- ctx.lookup(sym)
                      tyCell <- ctx.lookupType(id)
                      ty <- module.readStable(solver, tyCell)
                    yield (id, ty)) match
                      case Some((id, AST.RecordTypeRef(recId, _, _))) =>
                        ctx.lookupRecordById(recId).flatMap(_.fields.find(_.name == field)) match
                          case Some(param) => fillResultOnce(inferredTyCell, param.ty)
                          case None        => fillResultOnce(inferredTyCell, AST.AnyType(span))
                      case _ => ()
                  case _ => ()
                if !module.hasSomeValue(solver, inferredTyCell.asInstanceOf[module.CellAny]) then
                  fillResultOnce(inferredTyCell, AST.MetaCell(HoldNotReadable(targetTy), span))
                Some(Result.Done)
              case Some(AST.RecordTypeRef(recId, _, _)) =>
                ctx.lookupRecordById(recId) match
                  case Some(rec) =>
                    rec.fields.find(_.name == field) match
                      case Some(param) =>
                        fillResultOnce(inferredTyCell, param.ty)
                        Some(Result.Done)
                      case None =>
                        fillResultOnce(inferredTyCell, AST.AnyType(span))
                        Some(Result.Done)
                  case None =>
                    fillResultOnce(inferredTyCell, AST.AnyType(span))
                    Some(Result.Done)
              case Some(_) =>
                fillResultOnce(inferredTyCell, AST.AnyType(span))
                Some(Result.Done)
      case Vector(lhs, CST.Symbol(".", _), CST.Symbol(field, _)) =>
        val targetResult = module.newOnceCell[ElabConstraint, AST](solver)
        val targetTy = module.newOnceCell[ElabConstraint, AST](solver)
        module.addConstraint(solver, ElabConstraint.Infer(lhs, targetResult, targetTy, ctx, asType = asType))
        val targetAst = AST.MetaCell(HoldNotReadable(targetResult), lhs.span)
        val accessAst = AST.FieldAccess(targetAst, field, span)
        fillResultOnce(resultCell, accessAst)
        module.readStable(solver, targetTy) match
          case None =>
            // Try to use a known binding type (if lhs is a symbol) before deferring
            lhs match
              case CST.Symbol(sym, _) =>
                (for
                  id <- ctx.lookup(sym)
                  tyCell <- ctx.lookupType(id)
                  ty <- module.readStable(solver, tyCell)
                yield (id, ty)) match
                  case Some((id, AST.RecordTypeRef(recId, _, _))) =>
                    ctx.lookupRecordById(recId).flatMap(_.fields.find(_.name == field)) match
                      case Some(param) => fillResultOnce(inferredTyCell, param.ty)
                      case None        => fillResultOnce(inferredTyCell, AST.AnyType(span))
                  case _ => ()
              case _ => ()
            // If we still haven't produced a type, defer to the eventual target type
            if !module.hasSomeValue(solver, inferredTyCell.asInstanceOf[module.CellAny]) then
              fillResultOnce(inferredTyCell, AST.MetaCell(HoldNotReadable(targetTy), span))
            Some(Result.Done)
          case Some(AST.RecordTypeRef(recId, _, _)) =>
            ctx.lookupRecordById(recId) match
              case Some(rec) =>
                rec.fields.find(_.name == field) match
                  case Some(param) =>
                    fillResultOnce(inferredTyCell, param.ty)
                    Some(Result.Done)
                  case None =>
                    // Unknown field on a known record type – fall back to Any to keep solving
                    fillResultOnce(inferredTyCell, AST.AnyType(span))
                    Some(Result.Done)
              case None =>
                // Should not happen; fall back conservatively
                fillResultOnce(inferredTyCell, AST.AnyType(span))
                Some(Result.Done)
          case Some(_) =>
            // Field access on non-record type – yield Any to avoid dangling metas
            fillResultOnce(inferredTyCell, AST.AnyType(span))
            Some(Result.Done)
      case _ => None
  }

  private def annotationPattern(elems: Vector[CST]): Option[(CST, CST)] = {
    if elems.length >= 3 then
      val colonIndex = elems.lastIndexWhere {
        case CST.Symbol(":", _) => true
        case _                  => false
      }
      if colonIndex >= 1 && colonIndex < elems.length - 1 then
        val exprElems = elems.take(colonIndex)
        val typeElems = elems.drop(colonIndex + 1)
        val expr = {
          if exprElems.length == 1 then exprElems.head
          else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(exprElems), combinedSpan(exprElems))
        }
        val ty = {
          if typeElems.length == 1 then typeElems.head
          else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(typeElems), combinedSpan(typeElems))
        }
        Some((expr, ty))
      else None
    else None
  }

  private def handleAnnotatedExpression[M <: SolverModule](
      ctx: ElabContext,
      resultCell: CellRW[AST],
      inferredTyCell: CellRW[AST],
      exprCst: CST,
      typeCst: CST,
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result = {

    val annotationTy = module.newOnceCell[ElabConstraint, AST](solver)
    val annotationTyTy = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(typeCst, annotationTy, annotationTyTy, ctx, asType = true))

    val exprResult = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Check(exprCst, annotationTy, exprResult, ctx))

    module.addConstraint(
      solver,
      ElabConstraint.AssembleAnn(
        exprResult,
        annotationTy,
        resultCell,
        inferredTyCell,
        span
      )
    )

    Result.Done
  }

  /** Convert chained SeqOf expressions like f(a)(b) into nested SeqOf nodes we already know how to elaborate. */
  private def normalizeApplicationSeq(elems: Vector[CST]): Option[CST] = {
    if elems.length <= 1 then None
    else if elems.length == 2 && elems(1).isInstanceOf[CST.Tuple] then None
    else if elems.length == 3 && elems(1).isInstanceOf[CST.ListLiteral] && elems(2).isInstanceOf[CST.Tuple] then None
    else {
      var idx = 1
      var current = elems.head
      var changed = false

      while idx < elems.length do
        elems(idx) match
          case list: CST.ListLiteral =>
            if idx + 1 >= elems.length then return None
            elems(idx + 1) match
              case tuple: CST.Tuple =>
                current = buildSeqOf(current, Some(list), tuple)
                idx += 2
                changed = true
              case _ => return None
          case tuple: CST.Tuple =>
            current = buildSeqOf(current, None, tuple)
            idx += 1
            changed = true
          case _ => return None

      if changed then Some(current) else None
    }
  }

  private def buildSeqOf(func: CST, typeArgs: Option[CST.ListLiteral], tuple: CST.Tuple): CST = {
    val seqElems = typeArgs match
      case Some(list) => Vector(func, list, tuple)
      case None       => Vector(func, tuple)
    CST.SeqOf(NonEmptyVector.fromVectorUnsafe(seqElems), combinedSpan(seqElems))
  }

  private def combinedSpan(elems: Seq[CST]): Option[Span] = {
    val spans = elems.iterator.flatMap(_.span)
    if !spans.hasNext then None
    else {
      val first = spans.next()
      var last = first
      while spans.hasNext do last = spans.next()
      Some(first.combine(last))
    }
  }

  /** Elaborate an effect body that only contains operation signatures. Each operation is registered with a type that carries the enclosing effect in
    * its effect row. No bodies are required or expected.
    */
  private def processEffectBody[M <: SolverModule](
      body: CST.Block,
      effectRef: EffectRef,
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): ElabContext = {
    import module.given

    // Treat tail as another element so a final operation without semicolon is allowed
    val allElems = body.elements ++ body.tail.toVector

    var currentCtx = ctx

    allElems.foreach {
      case seq @ CST.SeqOf(seqElems, span) if seqElems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
        val elems = seqElems.toVector
        val name = elems.lift(1) match
          case Some(CST.Symbol(n, _)) => n
          case _ =>
            ctx.reporter.report(ElabProblem.UnboundVariable("Expected name after def in effect body", span))
            "<error>"

        // Parse telescopes, threading parameter context for dependencies
        var idx = 2
        val telescopes = scala.collection.mutable.ArrayBuffer.empty[Telescope]
        var paramCtx = currentCtx

        while idx < elems.length && (elems(idx).isInstanceOf[CST.ListLiteral] || elems(idx).isInstanceOf[CST.Tuple]) do
          val tel = elems(idx) match
            case CST.ListLiteral(params, _) => parseTelescopeFromCST(params, Implicitness.Implicit, paramCtx)(using module, solver)
            case CST.Tuple(params, _)       => parseTelescopeFromCST(params, Implicitness.Explicit, paramCtx)(using module, solver)
            case _                          => Telescope(Vector.empty, Implicitness.Explicit)
          telescopes += tel
          // Extend parameter context so later params/result types can reference earlier params
          tel.params.foreach { param =>
            val paramTyCell = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, paramTyCell, param.ty)
            paramCtx = paramCtx.bind(param.name, param.id, paramTyCell)
          }
          idx += 1

        // Expect a result type annotation
        if idx >= elems.length || !elems(idx).isInstanceOf[CST.Symbol] || elems(idx).asInstanceOf[CST.Symbol].name != ":" then
          ctx.reporter.report(ElabProblem.UnboundVariable("Effect operation requires a result type", span))
        else {
          idx += 1
          if idx >= elems.length then ctx.reporter.report(ElabProblem.UnboundVariable("Missing result type after ':' in effect operation", span))
          else {
            val typeElems = elems.drop(idx)
            val resultTypeCst = {
              if typeElems.length == 1 then typeElems.head
              else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(typeElems), combinedSpan(typeElems))
            }

            val resultTyCell = module.newOnceCell[ElabConstraint, AST](solver)
            val resultTyTyCell = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(resultTypeCst, resultTyCell, resultTyTyCell, paramCtx, asType = true))

            val resultTyAst = AST.MetaCell(HoldNotReadable(resultTyCell), resultTypeCst.span)
            val opId = Uniqid.make[AST]
            val opTy = AST.Pi(telescopes.toVector, resultTyAst, Vector(effectRef), span)
            val opTyCell = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, opTyCell, opTy)
            // Register operation in the surrounding context so it can be referenced
            currentCtx = currentCtx.bind(name, opId, opTyCell)
          }
        }

      case _ => () // Ignore non-def statements inside effect body for now
    }

    currentCtx
  }

  /** Handle def statement: def name [implicit] (explicit) : resultTy = body
    * @param defId
    *   The unique ID for this def (created in block's pass 1)
    * @param defTypeCell
    *   The type cell for this def (created in block's pass 1)
    */
  private def handleDefStatement[M <: SolverModule](
      c: ElabConstraint.Infer,
      elems: Vector[CST],
      span: Option[Span],
      defId: UniqidOf[AST],
      defTypeCell: CellRW[AST],
      ctx: ElabContext,
      defInfoMap: scala.collection.mutable.Map[CST, DefInfo]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    import module.given

    // Parse: def name [telescope]* (telescope)* = body
    // or:    def name [telescope]* (telescope)* : type = body
    if elems.length < 4 then
      ctx.reporter.report(ElabProblem.UnboundVariable("Invalid def syntax", span))
      module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
      module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
      module.fill(solver, defTypeCell, AST.Type(AST.LevelLit(0, None), None))
      return Result.Done

    val name = elems(1) match
      case CST.Symbol(n, _) => n
      case _ =>
        ctx.reporter.report(ElabProblem.UnboundVariable("Expected name after def", span))
        "<error>"

    println(s"[trace] def elems: $elems")
    // Collect telescopes (lists for implicit, tuples for explicit)
    // IMPORTANT: We need to accumulate context as we go, so later telescopes can reference earlier parameters
    var idx = 2
    val telescopes = scala.collection.mutable.ArrayBuffer.empty[Telescope]
    var accumulatedCtx = ctx

    while idx < elems.length && (elems(idx).isInstanceOf[CST.ListLiteral] || elems(idx).isInstanceOf[CST.Tuple]) do
      println(s"[trace] handling telescope element at idx=$idx: ${elems(idx)}")
      elems(idx) match
        case CST.ListLiteral(params, _) =>
          println(s"[trace] before parsing list telescope, ctx=${accumulatedCtx.bindings.keySet}")
          val telescope = parseTelescopeFromCST(params, Implicitness.Implicit, accumulatedCtx)(using module, solver)
          telescopes += telescope
          println(s"[trace] parsed list telescope with ${telescope.params.size} params")
          // Update context with parameters from this telescope
          telescope.params.foreach { param =>
            val paramTyCell = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, paramTyCell, param.ty)
            accumulatedCtx = accumulatedCtx.bind(param.name, param.id, paramTyCell)
            println(s"[trace] added ${param.name}, bindings now=${accumulatedCtx.bindings.keySet}")
          }
          idx += 1
        case CST.Tuple(params, _) =>
          println(s"[trace] before parsing tuple telescope, ctx=${accumulatedCtx.bindings.keySet}")
          val telescope = parseTelescopeFromCST(params, Implicitness.Explicit, accumulatedCtx)(using module, solver)
          telescopes += telescope
          println(s"[trace] parsed tuple telescope with ${telescope.params.size} params")
          // Update context with parameters from this telescope
          telescope.params.foreach { param =>
            val paramTyCell = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, paramTyCell, param.ty)
            accumulatedCtx = accumulatedCtx.bind(param.name, param.id, paramTyCell)
          }
          idx += 1
        case _ => ()

    // Check for optional result type annotation
    val resultTyCell = module.newOnceCell[ElabConstraint, AST](solver)
    var hasResultTy = false
    var annotatedEffects: Vector[EffectRef] = Vector.empty
    var effectAnnotated = false
    if idx < elems.length && elems(idx).isInstanceOf[CST.Symbol] && elems(idx).asInstanceOf[CST.Symbol].name == ":" then
      idx += 1
      if idx < elems.length then
        val resultTyTyCell = module.newOnceCell[ElabConstraint, AST](solver)
        val baseTyCst = elems(idx)
        module.addConstraint(solver, ElabConstraint.Infer(baseTyCst, resultTyCell, resultTyTyCell, ctx, asType = true))
        hasResultTy = true
        idx += 1
        // Optional effect row after type: / [e1, e2]
        if idx + 1 < elems.length && elems(idx).isInstanceOf[CST.Symbol] && elems(idx).asInstanceOf[CST.Symbol].name == "/" then
          elems(idx + 1) match
            case list: CST.ListLiteral =>
              annotatedEffects = parseEffectNames(list, accumulatedCtx, c.cst.span)(using module, solver)
              effectAnnotated = true
              idx += 2
            case _ => ()

    // Update stored def info with parsed metadata
    defInfoMap.get(c.cst).foreach { info =>
      defInfoMap.update(
        c.cst,
        info.copy(
          telescopes = telescopes.toVector,
          resultTyCell = if hasResultTy then Some(resultTyCell) else None,
          effects = annotatedEffects,
          effectAnnotated = effectAnnotated
        )
      )
    }

    // Expect =
    if idx >= elems.length || !elems(idx).isInstanceOf[CST.Symbol] || elems(idx).asInstanceOf[CST.Symbol].name != "=" then
      ctx.reporter.report(ElabProblem.UnboundVariable("Expected = in def", span))
      module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
      module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
      module.fill(solver, defTypeCell, AST.Type(AST.LevelLit(0, None), None))
      return Result.Done

    idx += 1

    // Body is remaining elements
    val bodyCst = {
      if idx < elems.length then
        if elems.length - idx == 1 then elems(idx)
        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(elems.drop(idx)), span)
      else {
        ctx.reporter.report(ElabProblem.UnboundVariable("Expected body in def", span))
        CST.Symbol("<error>", span)
      }
    }

    // Create extended context with parameters
    var extCtx = ctx
    for telescope <- telescopes; param <- telescope.params do
      val paramTyCell = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, paramTyCell, param.ty)
      extCtx = extCtx.bind(param.name, param.id, paramTyCell)

    // Elaborate body (asynchronously)
    val bodyResult = module.newOnceCell[ElabConstraint, AST](solver)
    val bodyTy = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(bodyCst, bodyResult, bodyTy, extCtx))

    // Add constraint to assemble the def once body is ready
    module.addConstraint(
      solver,
      ElabConstraint.AssembleDef(
        defId,
        name,
        telescopes.toVector,
        if hasResultTy then Some(resultTyCell) else None,
        bodyResult,
        bodyTy,
        c.result,
        c.inferredTy,
        annotatedEffects,
        effectAnnotated,
        defTypeCell,
        span,
        ctx
      )
    )

    Result.Done
  }

  private def handleLetStatement[M <: SolverModule](
      elems: Vector[CST],
      span: Option[Span],
      ctx: ElabContext,
      resultCell: CellRW[AST],
      elemTypeCell: CellRW[AST]
  )(using module: M, solver: module.Solver[ElabConstraint]): ElabContext = {
    import module.given

    def fail(message: String): ElabContext = {
      ctx.reporter.report(ElabProblem.UnboundVariable(message, span))
      module.fill(solver, resultCell, AST.Ref(Uniqid.make, "<error>", span))
      module.fill(solver, elemTypeCell, AST.Type(AST.LevelLit(0, None), None))
      ctx
    }

    if elems.length < 4 then return fail("Invalid let syntax")

    val name = elems(1) match
      case CST.Symbol(n, _) => n
      case _ =>
        ctx.reporter.report(ElabProblem.UnboundVariable("Expected name after let", span))
        "<error>"

    val letId = Uniqid.make[AST]
    val letTypeCell = module.newOnceCell[ElabConstraint, AST](solver)

    var idx = 2
    var hasAnnotation = false
    var annotationSpan: Option[Span] = None
    var annotationCstOpt: Option[CST] = None

    if idx < elems.length && elems(idx).isInstanceOf[CST.Symbol] && elems(idx).asInstanceOf[CST.Symbol].name == ":" then
      hasAnnotation = true
      val annElems = elems.drop(idx + 1).takeWhile {
        case CST.Symbol("=", _) => false
        case _                  => true
      }
      if annElems.isEmpty then return fail("Expected type annotation after colon in let")
      val annCst = {
        if annElems.length == 1 then annElems.head
        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(annElems), combinedSpan(annElems))
      }
      annotationCstOpt = Some(annCst)
      annotationSpan = annCst.span
      val annotationTyTy = module.newOnceCell[ElabConstraint, AST](solver)
      module.addConstraint(solver, ElabConstraint.Infer(annCst, letTypeCell, annotationTyTy, ctx, asType = true))
      idx = idx + 1 + annElems.length

    if idx >= elems.length || !elems(idx).isInstanceOf[CST.Symbol] || elems(idx).asInstanceOf[CST.Symbol].name != "=" then
      return fail("Expected = in let")

    idx += 1

    val valueCst = {
      if idx < elems.length then
        if elems.length - idx == 1 then elems(idx)
        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(elems.drop(idx)), span)
      else {
        ctx.reporter.report(ElabProblem.UnboundVariable("Expected value in let", span))
        CST.Symbol("<error>", span)
      }
    }

    val valueResult = module.newOnceCell[ElabConstraint, AST](solver)

    if hasAnnotation then module.addConstraint(solver, ElabConstraint.Check(valueCst, letTypeCell, valueResult, ctx))
    else {
      val valueTyCell = module.newOnceCell[ElabConstraint, AST](solver)
      module.addConstraint(solver, ElabConstraint.Infer(valueCst, valueResult, valueTyCell, ctx))
      module.fill(solver, letTypeCell, AST.MetaCell(HoldNotReadable(valueTyCell), valueCst.span))
    }

    val valueAst = AST.MetaCell(HoldNotReadable(valueResult), valueCst.span)
    val tyAstOpt = {
      if hasAnnotation then Some(AST.MetaCell(HoldNotReadable(letTypeCell), annotationSpan.orElse(annotationCstOpt.flatMap(_.span))))
      else None
    }
    val bodyAst = AST.Ref(letId, name, valueCst.span.orElse(span))
    val letAst = AST.Let(letId, name, tyAstOpt, valueAst, bodyAst, span)

    module.fill(solver, resultCell, letAst)
    module.fill(solver, elemTypeCell, AST.MetaCell(HoldNotReadable(letTypeCell), span))

    ctx.bind(name, letId, letTypeCell)
  }

  /** Parse a telescope from CST parameter list This creates a telescope with types as meta-cells that will be filled by constraints.
    *
    * IMPORTANT: For dependent types, this progressively extends the context as it processes each parameter, so later parameters can reference earlier
    * ones in their types. For example, in `def id[a: Type](x: a)`, the type of `x` references parameter `a`.
    */
  private def parseTelescopeFromCST[M <: SolverModule](
      params: Vector[CST],
      implicitness: Implicitness,
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): Telescope = {

    var currentCtx = ctx

    val parsedParams = params.flatMap {
      // Pattern: SeqOf(name, :, type)
      case CST.SeqOf(elems, _) if elems.length >= 3 =>
        val elemsVec = elems.toVector
        (elemsVec.head, elemsVec(1)) match
          case (CST.Symbol(name, _), CST.Symbol(":", _)) =>
            val typeElems = elemsVec.drop(2)
            // Allow complex types after the colon; if there are multiple elems, wrap them in a SeqOf
            val typeCst = {
              if typeElems.length == 1 then typeElems.head
              else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(typeElems), combinedSpan(typeElems))
            }

            val paramId = Uniqid.make[AST]
            // Elaborate the type in the CURRENT context (which includes previous params)
            val tyResult = module.newOnceCell[ElabConstraint, AST](solver)
            val tyTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(typeCst, tyResult, tyTy, currentCtx, asType = true))

            // Store cell reference as MetaCell - it will be resolved after constraints run
            val paramTy = AST.MetaCell(HoldNotReadable(tyResult), None)
            val param = Param(paramId, name, paramTy, implicitness, None)

            // Extend context for the next parameter
            currentCtx = currentCtx.bind(name, paramId, tyResult)

            Some(param)
          case _ => None
      // Just a bare symbol for name-only parameters
      case CST.Symbol(name, _) =>
        val paramId = Uniqid.make[AST]
        // Use a meta-variable for the type
        val tyCell = module.newOnceCell[ElabConstraint, AST](solver)
        val paramTy = AST.MetaCell(HoldNotReadable(tyCell), None)
        val param = Param(paramId, name, paramTy, implicitness, None)

        // Extend context for the next parameter
        currentCtx = currentCtx.bind(name, paramId, tyCell)

        Some(param)
      case _ => None
    }
    Telescope(parsedParams, implicitness)
  }

  /** Parse the field list of a record declaration. */
  private def parseRecordFields[M <: SolverModule](elems: Vector[CST], ctx: ElabContext)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[Param] = {
    val fieldsTuple = elems.lift(2) match
      case Some(CST.Tuple(params, _)) => params
      case _                          => Vector.empty
    val telescope = parseTelescopeFromCST(fieldsTuple, Implicitness.Explicit, ctx)
    telescope.params
  }

  /** Parse optional type parameters that appear between the enum name and its body. */
  private def parseEnumTypeParams[M <: SolverModule](elems: Vector[CST], ctx: ElabContext)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[Param] = {
    val paramsTupleOpt = elems.drop(2).takeWhile {
      case _: CST.Block => false
      case _            => true
    }.collectFirst { case CST.Tuple(params, _) => params }

    paramsTupleOpt
      .map { params =>
        val telescope = parseTelescopeFromCST(params, Implicitness.Explicit, ctx)
        telescope.params
      }
      .getOrElse(Vector.empty)
  }

  /** Normalize the common "Type" sugar used in parameter kinds: a bare Type desugars to Type(0) for ease of use. */
  private def normalizeTypeLikeKind(ty: AST): AST = {
    def isLevelParamLam(teles: Vector[Telescope]): Boolean =
      teles.length == 1 && teles.head.params.length == 1 && teles.head.params.head.ty.isInstanceOf[AST.LevelType]

    val zonked = ty
    zonked match
      case AST.Lam(teles, AST.Type(AST.Ref(_, _, _), tSpan), lamSpan) if isLevelParamLam(teles) =>
        AST.Type(AST.LevelLit(0, None), tSpan.orElse(lamSpan))
      case AST.Lam(teles, AST.TypeOmega(AST.Ref(_, _, _), tSpan), lamSpan) if isLevelParamLam(teles) =>
        AST.TypeOmega(AST.LevelLit(0, None), tSpan.orElse(lamSpan))
      case other => other
  }

  /** Parse enum cases from the enum declaration body. */
  private def parseEnumCases[M <: SolverModule](elems: Vector[CST], ctx: ElabContext, typeParams: Vector[Param])(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[EnumCase] = {
    val casesBlockOpt = elems.collectFirst { case b: CST.Block => b }
    val ctxWithParams = typeParams.foldLeft(ctx) { (acc, param) =>
      val paramTyCell = module.newOnceCell[ElabConstraint, AST](solver)
      val normalizedTy = normalizeTypeLikeKind(substituteSolutions(param.ty))
      module.fill(solver, paramTyCell, normalizedTy)
      acc.bind(param.name, param.id, paramTyCell)
    }
    val caseElems = casesBlockOpt.map(b => b.elements ++ b.tail.toVector).getOrElse(Vector.empty)
    caseElems.collect { case CST.SeqOf(seqElems, span) if seqElems.headOption.exists { case CST.Symbol("case", _) => true; case _ => false } =>
      val name = seqElems.lift(1) match
        case Some(CST.Symbol(n, _)) => n
        case _ =>
          ctx.reporter.report(ElabProblem.UnboundVariable("Expected case name in enum", span))
          "<error>"
      val paramsTuple = seqElems.lift(2) match
        case Some(CST.Tuple(params, _)) => params
        case _                          => Vector.empty
      val telescope = parseTelescopeFromCST(paramsTuple, Implicitness.Explicit, ctxWithParams)
      EnumCase(Uniqid.make, name, telescope.params)
    }
  }

  /** Check if a CST node is a def statement */
  private def isDefStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.headOption.exists { case CST.Symbol("def", _) => true; case _ => false }
    case _ => false

  private def isLetStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.headOption.exists { case CST.Symbol("let", _) => true; case _ => false }
    case _ => false

  private def isRecordStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.headOption.exists { case CST.Symbol("record", _) => true; case _ => false }
    case _ => false
  private def isEnumStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.headOption.exists { case CST.Symbol("enum", _) | CST.Symbol("coenum", _) => true; case _ => false }
    case _ => false

  private def parseEffectNames(list: CST.ListLiteral, ctx: ElabContext, span: Option[Span])(using
      module: SolverModule,
      solver: module.Solver[ElabConstraint]
  ): Vector[EffectRef] = {
    list.elements.collect { case CST.Symbol(name, _) =>
      ctx
        .lookupEffect(name)
        .orElse(ElabContext.defaultEffects.get(name))
        .getOrElse {
          ctx.reporter.report(ElabProblem.UnknownEffect(name, span))
          EffectRef.User(Uniqid.make, name)
        }
    }
  }

  /** Extract a base type and an optional effect list from `Type / [e1, e2]` forms. */
  private def extractEffectAnnotation(cst: CST, ctx: ElabContext)(using
      module: SolverModule,
      solver: module.Solver[ElabConstraint]
  ): (CST, Vector[EffectRef]) = cst match
    case CST.SeqOf(elements, _) if elements.length == 3 =>
      val elems = elements.toVector
      elems match
        case Vector(base, CST.Symbol("/", _), list: CST.ListLiteral) =>
          (base, parseEffectNames(list, ctx, cst.span))
        case _ => (cst, Vector.empty)
    case _ => (cst, Vector.empty)

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

  /** Reduce (normalize) a term by performing beta-reduction. Following the paper's recommendation (Section 7.5), we reduce before unification. This
    * handles lambda applications.
    */
  private def reduce[M <: SolverModule](
      term: AST,
      ctx: ElabContext,
      depth: Int = 0
  )(using module: M, solver: module.Solver[ElabConstraint]): AST = {
    import module.given

    // Depth limit to prevent infinite recursion
    if depth > 100 then return term

    term match
      // Resolve MetaCells first (but don't recurse if it leads back to a MetaCell)
      case AST.MetaCell(HoldNotReadable(cell), span) =>
        module.readStable(solver, cell) match
          case Some(solved) if !solved.isInstanceOf[AST.MetaCell] => reduce(solved, ctx, depth + 1)
          case _                                                  => term

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
        else
          unify(f1, f2, span, ctx) match
            case UnifyResult.Success => unifyAll(args1.map(_.value).zip(args2.map(_.value)), span, ctx)
            case failure             => failure

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
      case StmtAST.ExprStmt(expr, _) => occursIn(cell, expr)
      case StmtAST.Def(_, _, teles, resTy, body, _) =>
        teles.exists(t => t.params.exists(p => occursIn(cell, p.ty))) || resTy.exists(occursIn(cell, _)) || occursIn(cell, body)
      case StmtAST.Record(_, _, fields, _) =>
        fields.exists(p => occursIn(cell, p.ty))
      case StmtAST.Enum(_, _, typeParams, cases, _) =>
        typeParams.exists(p => occursIn(cell, p.ty)) || cases.exists(c => c.params.exists(p => occursIn(cell, p.ty)))
      case StmtAST.Coenum(_, _, typeParams, cases, _) =>
        typeParams.exists(p => occursIn(cell, p.ty)) || cases.exists(c => c.params.exists(p => occursIn(cell, p.ty)))
      case StmtAST.Pkg(_, body, _) => occursIn(cell, body)
  }

  private def handleIsUniverse[M <: SolverModule](c: ElabConstraint.IsUniverse)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    import module.given

    module.readStable(solver, c.ty) match
      case Some(AST.Type(level, _)) =>
        module.fill(solver, c.level, level)
        Result.Done
      case Some(_) =>
        // Not a universe - error
        Result.Done
      case None =>
        Result.Waiting(c.ty)
  }

  private def handleIsPi[M <: SolverModule](c: ElabConstraint.IsPi)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    import module.given

    module.readStable(solver, c.ty) match
      case Some(AST.Pi(telescopes, resultTy, _, _)) =>
        module.fill(solver, c.telescopes, telescopes)
        module.fill(solver, c.resultTy, resultTy)
        Result.Done
      case Some(_) =>
        // Not a Pi type - error
        Result.Done
      case None =>
        Result.Waiting(c.ty)
  }

  private def handleAssembleApp[M <: SolverModule](c: ElabConstraint.AssembleApp)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    import module.given

    // Check if we've already filled the result - if so, we're done (avoid re-processing)
    if module.hasStableValue(solver, c.result) then return Result.Done

    // Wait for function type to be ready first (most important dependency)
    if !module.hasStableValue(solver, c.funcTy) then return Result.Waiting(c.funcTy)

    // Wait for all parts to be elaborated
    if !module.hasStableValue(solver, c.funcResult) then return Result.Waiting(c.funcResult)

    if !c.explicitTypeArgResults.forall(module.hasStableValue(solver, _)) then
      return Result.Waiting(c.explicitTypeArgResults.filter(!module.hasStableValue(solver, _))*)

    if !c.argResults.forall(module.hasStableValue(solver, _)) then return Result.Waiting(c.argResults.filter(!module.hasStableValue(solver, _))*)

    // Wait for all argument types to be inferred
    if !c.argTypes.forall(module.hasStableValue(solver, _)) then return Result.Waiting(c.argTypes.filter(!module.hasStableValue(solver, _))*)

    // Wait for all explicit type argument types to be inferred
    if !c.explicitTypeArgTypes.forall(module.hasStableValue(solver, _)) then
      return Result.Waiting(c.explicitTypeArgTypes.filter(!module.hasStableValue(solver, _))*)

    // All prerequisites are ready - we're committed to processing now
    val func = module.readStable(solver, c.funcResult).get
    val explicitTypeArgs = c.explicitTypeArgResults.flatMap(module.readStable(solver, _))
    val explicitArgs = c.argResults.flatMap(module.readStable(solver, _))

    module.readStable(solver, c.funcTy) match
      case Some(AST.MetaCell(HoldNotReadable(cell), _)) =>
        // Wait until the referenced type is available
        if module.hasStableValue(solver, cell.asInstanceOf[module.CellAny]) then
          // Try again with substituted solution
          module.fill(solver, c.funcTy.asInstanceOf[module.CellRW[AST]], substituteSolutions(AST.MetaCell(HoldNotReadable(cell), None)))
          Result.Waiting(cell.asInstanceOf[module.CellAny])
        else Result.Waiting(cell.asInstanceOf[module.CellAny])

      case Some(piTy @ AST.Pi(telescopes, resultTy, _, _)) =>
        // Resolve MetaCells in telescopes (parameter types may contain unresolved cells)
        val resolvedTelescopes = telescopes.map(tel =>
          tel.copy(params = tel.params.map(param => param.copy(ty = normalizeTypeLikeKind(substituteSolutions(param.ty)))))
        )
        val resolvedResultTy = substituteSolutions(resultTy)

        // Wait until all parameter types (and result type) are stabilized so implicit substitution can succeed
        def firstUnresolved(cellAst: AST): Option[module.CellAny] = {
          cellAst match
            case AST.MetaCell(HoldNotReadable(cell), _) =>
              if module.hasStableValue(solver, cell.asInstanceOf[module.CellAny]) then None else Some(cell.asInstanceOf[module.CellAny])
            case AST.Type(level, _)      => firstUnresolved(level)
            case AST.TypeOmega(level, _) => firstUnresolved(level)
            case AST.Tuple(elems, _)     => elems.iterator.flatMap(firstUnresolved).take(1).toSeq.headOption
            case AST.TupleType(elems, _) => elems.iterator.flatMap(firstUnresolved).take(1).toSeq.headOption
            case AST.ListType(elem, _)   => firstUnresolved(elem)
            case AST.Pi(tels, res, _, _) =>
              val inParams = tels.iterator.flatMap(_.params.iterator).flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption
              inParams.orElse(firstUnresolved(res))
            case AST.Lam(tels, body, _) =>
              val inParams = tels.iterator.flatMap(_.params.iterator).flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption
              inParams.orElse(firstUnresolved(body))
            case AST.App(func, args, _, _) =>
              firstUnresolved(func).orElse(args.iterator.flatMap(a => firstUnresolved(a.value)).take(1).toSeq.headOption)
            case AST.Let(_, _, ty, value, body, _) =>
              ty.flatMap(firstUnresolved).orElse(firstUnresolved(value)).orElse(firstUnresolved(body))
            case AST.Ann(expr, ty, _) => firstUnresolved(expr).orElse(firstUnresolved(ty))
            case AST.Block(elems, tail, _) =>
              elems.iterator.flatMap(firstUnresolvedStmt).take(1).toSeq.headOption.orElse(firstUnresolved(tail))
            case AST.RecordCtor(_, _, args, _) =>
              args.iterator.flatMap(firstUnresolved).take(1).toSeq.headOption
            case AST.EnumCtor(_, _, _, _, args, _) =>
              args.iterator.flatMap(firstUnresolved).take(1).toSeq.headOption
            case AST.FieldAccess(target, _, _) =>
              firstUnresolved(target)
            case _ => None
        }

        def firstUnresolvedStmt(stmt: StmtAST): Option[module.CellAny] = {
          stmt match
            case StmtAST.ExprStmt(expr, _) => firstUnresolved(expr)
            case StmtAST.Def(_, _, teles, resTy, body, _) =>
              teles.iterator
                .flatMap(_.params.iterator)
                .flatMap(p => firstUnresolved(p.ty))
                .take(1)
                .toSeq
                .headOption
                .orElse(resTy.flatMap(firstUnresolved))
                .orElse(firstUnresolved(body))
            case StmtAST.Record(_, _, fields, _) =>
              fields.iterator.flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption
            case StmtAST.Enum(_, _, typeParams, cases, _) =>
              typeParams.iterator.flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption
                .orElse(cases.iterator.flatMap(_.params.iterator).flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption)
            case StmtAST.Coenum(_, _, typeParams, cases, _) =>
              typeParams.iterator.flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption
                .orElse(cases.iterator.flatMap(_.params.iterator).flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption)
            case StmtAST.Pkg(_, body, _) => firstUnresolved(body)
        }

        val unresolvedParamCell = {
          resolvedTelescopes.iterator
            .flatMap(_.params.iterator)
            .flatMap(p => firstUnresolved(p.ty))
            .take(1)
            .toSeq
            .headOption
            .orElse(firstUnresolved(resolvedResultTy))
        }
        unresolvedParamCell match
          case Some(cell) => return Result.Waiting(cell)
          case None       => ()

        // Separate implicit and explicit parameters
        val implicitParams = resolvedTelescopes.filter(_.implicitness == Implicitness.Implicit).flatMap(_.params)
        val explicitParams = resolvedTelescopes.filter(_.implicitness == Implicitness.Explicit).flatMap(_.params)

        // Build arguments: use provided explicit type args for implicit params, create metas for rest
        val implicitArgs = if explicitTypeArgs.nonEmpty then
          // User provided explicit type arguments like id[String]
          if explicitTypeArgs.size != implicitParams.size then
            c.ctx.reporter.report(ElabProblem.NotAFunction(func, c.span))
            module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
            module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
            return Result.Done

          // Type check explicit type arguments against implicit parameter types synchronously
          val typeArgsOk = implicitParams.zip(explicitTypeArgs).zip(c.explicitTypeArgTypes).forall { case ((param, _), argTyCell) =>
            module.readStable(solver, argTyCell) match
              case Some(actualTy @ AST.MetaCell(_, _)) =>
                val expectedTy = substituteSolutions(param.ty)
                expectedTy match
                  case AST.MetaCell(_, _) => true
                  case other =>
                    if !module.hasStableValue(solver, argTyCell.asInstanceOf[module.CellRW[AST]]) then
                      module.fill(solver, argTyCell.asInstanceOf[module.CellRW[AST]], other)
                    true
              case Some(actualTy) =>
                val expectedTy = substituteSolutions(param.ty)
                expectedTy match
                  case AST.MetaCell(_, _) => true
                  case _ =>
                    unify(expectedTy, actualTy, c.span, c.ctx) match
                      case UnifyResult.Success => true
                      case UnifyResult.Failure(_) =>
                        c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedTy, actualTy, c.span))
                        false
              case None => true // Not stable yet
          }

          if !typeArgsOk then
            module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
            module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
            return Result.Done

          explicitTypeArgs
        else {
          // No explicit type arguments - create meta-variables for implicit parameters
          // These will be solved through unification when we check explicit argument types
          implicitParams.map { _ =>
            val metaCell = module.newOnceCell[ElabConstraint, AST](solver)
            AST.MetaCell(HoldNotReadable(metaCell), c.span)
          }
        }

        // Check explicit argument arity
        if explicitArgs.size != explicitParams.size then
          c.ctx.reporter.report(ElabProblem.NotAFunction(func, c.span))
          module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
          return Result.Done

        // Type check arguments synchronously (not via constraints to avoid loops)
        // This may fill the implicit arg MetaCells through unification
        val implicitSubst = implicitParams.map(_.id).zip(implicitArgs).toMap
        var runningSubst = implicitSubst
        val allTypeChecksPassed = explicitParams.zip(c.argTypes).zipWithIndex.forall { case ((param, argTyCell), idx) =>
          val baseParamTy = substituteSolutions(param.ty)
          val expectedParamTy = substituteInType(baseParamTy, runningSubst)
          val typeCheckOk = module.readStable(solver, argTyCell) match
            case Some(actualArgTy) =>
              expectedParamTy match
                case metaExpected @ AST.MetaCell(_, _) =>
                  unify(metaExpected, actualArgTy, c.span, c.ctx) match
                    case UnifyResult.Success    => true
                    case UnifyResult.Failure(_) =>
                      c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedParamTy, actualArgTy, c.span))
                      false
                case _ =>
                  actualArgTy match
                    case AST.MetaCell(_, _) =>
                      if !module.hasStableValue(solver, argTyCell.asInstanceOf[module.CellRW[AST]]) then
                        module.fill(solver, argTyCell.asInstanceOf[module.CellRW[AST]], expectedParamTy)
                      true
                    case _ =>
                      // Try unification first (for implicit argument inference), then subtyping
                      unify(expectedParamTy, actualArgTy, c.span, c.ctx) match
                        case UnifyResult.Success    => true
                        case UnifyResult.Failure(_) =>
                          // Unification failed, try subtyping: actualArgTy <: expectedParamTy
                          if isSubtype(actualArgTy, expectedParamTy, c.span, c.ctx) then true
                          else {
                            c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedParamTy, actualArgTy, c.span))
                            false
                          }
            case None => true // Not stable yet, will be checked later
          if typeCheckOk then
            // Extend substitution with this explicit argument for later parameters
            explicitArgs.lift(idx).foreach(argAst => runningSubst = runningSubst + (param.id -> argAst))
          typeCheckOk
        }

        if !allTypeChecksPassed then
          module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
          return Result.Done

        // After type checking, resolve any MetaCells in implicit args that may have been filled
        val resolvedImplicitArgs = implicitArgs.map {
          case AST.MetaCell(HoldNotReadable(cell), span) =>
            module.readStable(solver, cell).getOrElse(AST.MetaCell(HoldNotReadable(cell), span))
          case arg => arg
        }

        // Build application - nest two Apps if we have both implicit and explicit args
        val app = if resolvedImplicitArgs.nonEmpty && explicitArgs.nonEmpty then
          // Nested: func[implicitArgs](explicitArgs)
          val implicitApp = AST.App(func, resolvedImplicitArgs.map(Arg(_, Implicitness.Implicit)), implicitArgs = true, c.span)
          AST.App(implicitApp, explicitArgs.map(Arg(_, Implicitness.Explicit)), implicitArgs = false, c.span)
        else if resolvedImplicitArgs.nonEmpty then
          // Only implicit: func[implicitArgs]
          AST.App(func, resolvedImplicitArgs.map(Arg(_, Implicitness.Implicit)), implicitArgs = true, c.span)
        else {
          // Only explicit: func(explicitArgs)
          AST.App(func, explicitArgs.map(Arg(_, Implicitness.Explicit)), implicitArgs = false, c.span)
        }

        // Perform substitution: replace parameter references in result type with resolved arguments
        val allParams = implicitParams ++ explicitParams
        val allArgs = resolvedImplicitArgs ++ explicitArgs
        val appValue = func match
          case AST.EnumCaseRef(enumId, caseId, enumName, caseName, _) =>
            AST.EnumCtor(enumId, caseId, enumName, caseName, allArgs, c.span)
          case _ => app

        module.fill(solver, c.result, appValue)
        val substitutedTy = substituteInType(resolvedResultTy, allParams.map(_.id).zip(allArgs).toMap)
        val normalizedTy = reduce(substitutedTy, c.ctx)

        module.fill(solver, c.inferredTy, normalizeType(normalizedTy))
        Result.Done
      case Some(ty) =>
        c.ctx.reporter.report(ElabProblem.NotAFunction(ty, c.span))
        module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
        module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
        Result.Done
      case None =>
        Result.Waiting(c.funcTy)
  }

  private def handleAssembleAnn[M <: SolverModule](c: ElabConstraint.AssembleAnn)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    import module.given

    if module.hasStableValue(solver, c.result) && module.hasStableValue(solver, c.inferredTy) then return Result.Done

    if !module.hasStableValue(solver, c.exprResult) then return Result.Waiting(c.exprResult)
    if !module.hasStableValue(solver, c.annotationTy) then return Result.Waiting(c.annotationTy)

    val exprAst = module.readStable(solver, c.exprResult).get
    val tyAst = module.readStable(solver, c.annotationTy).get

    module.fill(solver, c.result, AST.Ann(exprAst, tyAst, c.span))
    module.fill(solver, c.inferredTy, tyAst)

    Result.Done
  }

  private def handleAssembleDef[M <: SolverModule](c: ElabConstraint.AssembleDef)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    import module.given

    // Wait for body to be elaborated
    if !module.hasStableValue(solver, c.bodyResult) then return Result.Waiting(c.bodyResult)

    val body = module.readStable(solver, c.bodyResult).get

    // Get or infer result type
    val resultTy: Option[AST] = c.resultTyCell match
      case Some(rtCell) =>
        if !module.hasStableValue(solver, rtCell) then return Result.Waiting(rtCell)
        module.readStable(solver, rtCell)
      case None => None

    // Result cell carries the elaborated body to resolve block placeholders
    module.fill(solver, c.result, body)

    def effectsFromRef(ref: AST.Ref): Set[EffectRef] = {
      // Prefer user-defined types in context; fall back to builtin signatures
      val fromCtx = c.ctx
        .lookupType(ref.id)
        .flatMap(cell => module.readStable(solver, cell.asInstanceOf[module.CellR[AST]]))
        .collect { case AST.Pi(_, _, effs, _) => effs.toSet }
        .getOrElse(Set.empty[EffectRef])
      if fromCtx.nonEmpty then fromCtx
      else {
        ElabContext.defaultBuiltinTypes.get(ref.name) match
          case Some(AST.Pi(_, _, effs, _)) => effs.toSet
          case _                           => Set.empty[EffectRef]
      }
    }

    // Compute required effects by scanning the body for calls whose types carry effect rows
    def gatherEffects(ast: AST): Set[EffectRef] = {
      ast match
        case AST.App(func, args, _, _) =>
          val fromFuncType = func match
            case r: AST.Ref => effectsFromRef(r)
            case _          => Set.empty[EffectRef]
          fromFuncType ++ gatherEffects(func) ++ args.flatMap(a => gatherEffects(a.value))
        case AST.Block(elements, tail, _) =>
          elements.flatMap(gatherEffectsStmt).toSet ++ gatherEffects(tail)
        case AST.Tuple(elements, _)   => elements.flatMap(gatherEffects).toSet
        case AST.ListLit(elements, _) => elements.flatMap(gatherEffects).toSet
        case AST.Lam(_, body, _)      => gatherEffects(body)
        case AST.Let(_, _, ty, value, body, _) =>
          ty.map(gatherEffects).getOrElse(Set.empty) ++ gatherEffects(value) ++ gatherEffects(body)
        case AST.Ann(expr, ty, _) => gatherEffects(expr) ++ gatherEffects(ty)
        case AST.RecordCtor(_, _, args, _) =>
          args.flatMap(gatherEffects).toSet
        case AST.EnumCtor(_, _, _, _, args, _) =>
          args.flatMap(gatherEffects).toSet
        case AST.FieldAccess(target, _, _) =>
          gatherEffects(target)
        case _ => Set.empty
    }

    def gatherEffectsStmt(stmt: StmtAST): Set[EffectRef] = stmt match
      case StmtAST.ExprStmt(expr, _) => gatherEffects(expr)
      case StmtAST.Def(_, _, teles, resTy, body, _) =>
        teles.flatMap(t => t.params.map(p => gatherEffects(p.ty))).flatten.toSet ++ resTy.map(gatherEffects).getOrElse(Set.empty) ++ gatherEffects(
          body
        )
      case StmtAST.Record(_, _, fields, _) =>
        fields.flatMap(p => gatherEffects(p.ty)).toSet
      case StmtAST.Enum(_, _, typeParams, cases, _) =>
        typeParams.flatMap(p => gatherEffects(p.ty)).toSet ++ cases.flatMap(_.params).flatMap(p => gatherEffects(p.ty)).toSet
      case StmtAST.Coenum(_, _, typeParams, cases, _) =>
        typeParams.flatMap(p => gatherEffects(p.ty)).toSet ++ cases.flatMap(_.params).flatMap(p => gatherEffects(p.ty)).toSet
      case StmtAST.Pkg(_, body, _) => gatherEffects(body)

    val requiredEffects = gatherEffects(body)

    // Validate effects are declared
    val declaredNames = c.ctx.effects.keySet ++ ElabContext.defaultEffects.keySet
    val unknownEffects =
      (requiredEffects ++ c.effects.toSet).map(_.name).filterNot(declaredNames.contains)
    if unknownEffects.nonEmpty then unknownEffects.foreach(e => c.ctx.reporter.report(ElabProblem.UnknownEffect(e, c.span)))

    // Compute def type (Pi type)
    val finalResultTy = resultTy match
      case Some(rt) => rt
      case None     =>
        // Infer from body type
        if !module.hasStableValue(solver, c.bodyTy) then return Result.Waiting(c.bodyTy)
        module.readStable(solver, c.bodyTy).get

    // If an effect row was annotated, ensure it covers required effects
    if c.effectAnnotated && !requiredEffects.subsetOf(c.effects.toSet) then
      c.ctx.reporter.report(
        ElabProblem.TypeMismatch(
          AST.Pi(c.telescopes, finalResultTy, c.effects, c.span),
          AST.Pi(c.telescopes, finalResultTy, requiredEffects.toVector, c.span),
          c.span
        )
      )

    val piTy = AST.Pi(c.telescopes, finalResultTy, if c.effectAnnotated then c.effects else requiredEffects.toVector, c.span)
    module.fill(solver, c.inferredTy, normalizeType(piTy))
    module.fill(solver, c.defTypeCell, normalizeType(piTy))

    Result.Done
  }

/** Handler configuration for elaboration */
class ElabHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
  private val handler = new ElabHandler()

  def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] =
    Some(handler)

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
          tel.params.map(p => Param(p.id, p.name, substituteSolutions(p.ty), p.implicitness, p.default.map(substituteSolutions))),
          tel.implicitness
        )
      }
      AST.Pi(newTelescopes, substituteSolutions(resultTy), effects, span)

    case AST.Lam(telescopes, body, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => Param(p.id, p.name, substituteSolutions(p.ty), p.implicitness, p.default.map(substituteSolutions))),
          tel.implicitness
        )
      }
      AST.Lam(newTelescopes, substituteSolutions(body), span)

    case AST.App(func, args, implicitArgs, span) =>
      val normalizedFunc = substituteSolutions(func)
      val normalizedArgs = args.map(arg => Arg(substituteSolutions(arg.value), arg.implicitness))
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
    case other => other
}

private def substituteSolutionsStmt[M <: SolverModule](stmt: StmtAST)(using module: M, solver: module.Solver[ElabConstraint]): StmtAST = {
  stmt match
    case StmtAST.ExprStmt(expr, span) => StmtAST.ExprStmt(substituteSolutions(expr), span)
    case StmtAST.Def(id, name, teles, resTy, body, span) =>
      val newTeles =
        teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))))
      StmtAST.Def(id, name, newTeles, resTy.map(substituteSolutions), substituteSolutions(body), span)
    case StmtAST.Record(id, name, fields, span) =>
      val newFields = fields.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))
      StmtAST.Record(id, name, newFields, span)
    case StmtAST.Enum(id, name, typeParams, cases, span) =>
      val newTypeParams = typeParams.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))
      val newCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))))
      StmtAST.Enum(id, name, newTypeParams, newCases, span)
    case StmtAST.Coenum(id, name, typeParams, cases, span) =>
      val newTypeParams = typeParams.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))
      val newCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = substituteSolutions(p.ty), default = p.default.map(substituteSolutions)))))
      StmtAST.Coenum(id, name, newTypeParams, newCases, span)
    case StmtAST.Pkg(name, body, span) =>
      StmtAST.Pkg(name, substituteSolutions(body), span)
}

/** Main elaborator object */
object Elaborator:
  /** Pre-register top-level def names across multiple CSTs so they can mutually reference each other. */
  def preRegisterDefs[M <: SolverModule](csts: Seq[CST], ctx: ElabContext)(using module: M, solver: module.Solver[ElabConstraint]): ElabContext = {
    csts.foldLeft(ctx) { (acc, cst) =>
      cst match
        case CST.Block(elements, _, _) =>
          elements.foldLeft(acc) { (innerCtx, elem) =>
            elem match
              case CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
                seqElems.toVector match
                  case _ +: CST.Symbol(name, _) +: _ =>
                    if innerCtx.lookup(name).isEmpty then
                      val defTypeCell = module.newOnceCell[ElabConstraint, AST](solver)
                      val defId = Uniqid.make[AST]
                      innerCtx.bind(name, defId, defTypeCell)
                    else innerCtx
                  case _ => innerCtx
              case _ => innerCtx
          }
        case CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("package", _) => true; case _ => false } =>
          val elems = seqElems.toVector
          val bodyOpt = {
            if elems.length > 2 then
              val rest = elems.drop(2)
              val span = rest.headOption.flatMap(_.span)
              if rest.length == 1 then Some(rest.head)
              else Some(CST.SeqOf(NonEmptyVector.fromVectorUnsafe(rest), span))
            else None
          }
          bodyOpt.map(body => preRegisterDefs(Seq(body), acc)(using module, solver)).getOrElse(acc)
        case _ => acc
    }
  }

  /** Elaborate multiple files within one module, sharing a context and allowing cross-file def references. */
  def elaborateModule[M <: SolverModule](
      csts: Seq[CST],
      reporter: Reporter[ElabProblem]
  )(using module: M): Seq[(Option[AST], Option[AST], Vector[ElabProblem])] = {
    val baseCtx = ElabContext(bindings = Map.empty, types = Map.empty, reporter = reporter)
    val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))
    val preCtx = preRegisterDefs(csts, baseCtx)(using module, solver)

    val results = csts.map { cst =>
      val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeCell = module.newOnceCell[ElabConstraint, AST](solver)
      module.addConstraint(solver, ElabConstraint.InferTopLevel(cst, resultCell, typeCell, preCtx))
      (resultCell, typeCell)
    }

    module.run(solver)

    val reports = reporter match
      case vr: VectorReporter[ElabProblem] => vr.getReports
      case _                               => Vector.empty

    results.map { (resCell, tyCell) =>
      val astOpt = module.readStable(solver, resCell).map(r => substituteSolutions(r)(using module, solver))
      val tyOpt = module.readStable(solver, tyCell).map(t => substituteSolutions(t)(using module, solver))
      (astOpt, tyOpt, reports)
    }
  }

  /** Elaborate a CST into an AST with type inference
    *
    * @param cst
    *   The concrete syntax tree to elaborate
    * @param reporter
    *   The reporter for errors (defaults to VectorReporter)
    * @param ctx
    *   The elaboration context with bindings (if None, creates default with reporter)
    * @param module
    *   The solver module to use (defaults to ProceduralSolverModule)
    * @return
    *   The elaborated AST and its inferred type
    */
  def elaborate[M <: SolverModule](
      cst: CST,
      reporter: Reporter[ElabProblem],
      ctx: Option[ElabContext] = None
  )(using module: M): (AST, AST) = {

    val elaborationContext = ctx.getOrElse(
      ElabContext(
        bindings = Map.empty,
        types = Map.empty,
        builtins = ElabContext.defaultBuiltins,
        builtinTypes = ElabContext.defaultBuiltinTypes,
        reporter = reporter
      )
    )

    val solver = module.makeSolver[ElabConstraint](new ElabHandlerConf(module))

    val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
    val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

    module.addConstraint(solver, ElabConstraint.Infer(cst, resultCell, typeCell, elaborationContext))

    module.run(solver)

    val result = module
      .readStable(solver, resultCell)
      .getOrElse(throw new Exception(s"Failed to elaborate: $cst"))
    val ty = module
      .readStable(solver, typeCell)
      .getOrElse(throw new Exception(s"Failed to infer type for: $cst"))

    // Substitute all meta-cell solutions after constraint solving
    val zonkedResult = substituteSolutions(result)(using module, solver)
    val zonkedTy = substituteSolutions(ty)(using module, solver)

    (zonkedResult, zonkedTy)
  }

  /** Elaborate with default ProceduralSolver and new VectorReporter */
  def elaborate(cst: CST)(using Reporter[ElabProblem]): (AST, AST) =
    elaborate(cst, summon[Reporter[ElabProblem]], None)(using ProceduralSolverModule)

  /** Elaborate with custom context */
  def elaborate(cst: CST, ctx: ElabContext): (AST, AST) =
    elaborate(cst, ctx.reporter, Some(ctx))(using ProceduralSolverModule)

  /** Expose normalizeType for tests and downstream utilities. */
  def normalizeType(ast: AST): AST = CoreTypeChecker.normalizeType(ast)
