package chester.tyck

import chester.error._
import chester.syntax.concrete._
import chester.syntax.core.{_, given}
import chester.tyck._
import chester.utils._
import chester.utils.propagator._
import chester.syntax._
import chester.tyck.api.{NoopSemanticCollector, SemanticCollector, UnusedVariableWarningWrapper}

import scala.language.implicitConversions
import scala.util.boundary
import scala.util.boundary.break

trait Elaborater extends ProvideCtx with TyckPropagator {

  def checkType(expr: Expr)(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term = {
    // Create a new type cell representing the kind Typeω (the type of types)
    val kindType = literal(Typeω: Term)

    elab(expr, kindType, toEffectsCell(Effects.Empty))
  }

  def checkTypeId(expr: Expr)(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): CellId[Term] = {
    toId(checkType(expr))
  }

  def elabTy(expr: Option[Expr])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term =
    expr match {
      case Some(expr) => checkType(expr)
      case None       => Meta(newType)
    }

  def elab(expr: Expr, ty: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term

  def elabId(expr: Expr, ty: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): CellId[Term] = {
    val term = elab(expr, ty, effects)
    toId(term)
  }
}

trait ProvideElaborater extends ProvideCtx with Elaborater with ElaboraterFunction with ElaboraterFunctionCall with ElaboraterBlock {

  // TODO: add something for implicit conversion

  def newSubtype(ty: CellIdOr[Term], cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): CellId[Term] = {
    val cell = newType
    state.addPropagator(Unify(toId(ty), cell, cause))
    cell
  }

  /** ty is lhs */
  override def elab(
      expr: Expr,
      ty0: CellIdOr[Term],
      effects: CIdOf[EffectsCell]
  )(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term = toTerm {
    val ty = toId(readMetaVar(toTerm(ty0)))
    resolve(expr) match {
      case expr @ Identifier(name, _) => {
        localCtx.get(name) match {
          case Some(c: ContextItem) => {
            if (c.reference.isDefined) {
              c.reference.get.referencedOn(expr)
            }
            state.addPropagator(Unify(ty, c.tyId, expr))
            c.ref
          }
          case None => {
            // Check if 'name' refers to an object definition
            localCtx.getTypeDefinition(name) match {
              case Some(objectDef: ObjectStmtTerm) =>
                val objectCallTerm = ObjectCallTerm(objectDef, convertMeta(expr.meta))
                unify(ty, ObjectTypeTerm(objectDef, convertMeta(expr.meta)), expr)
                objectCallTerm
              case Some(recordDef: RecordStmtTerm) =>
                val recordCallTerm = RecordCallTerm(recordDef, TelescopeTerm(Vector(), meta = None), convertMeta(expr.meta)) // TODO
                unify(ty, Type0, expr) // TODO: Type
                recordCallTerm
              case Some(todo) => ???
              case None =>
                val problem = UnboundVariable(name, expr)
                ck.reporter.apply(problem)
                ErrorTerm(problem, convertMeta(expr.meta))
            }
          }
        }
      }
      case expr @ IntegerLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty))
        AbstractIntTerm_.from(value, convertMeta(meta))
      }
      case expr @ RationalLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty))
        RationalTerm(value, convertMeta(meta))
      }
      case expr @ StringLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty))
        StringTerm(value, convertMeta(meta))
      }
      case expr @ SymbolLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty))
        SymbolTerm(value, convertMeta(meta))
      }
      case expr @ UnitExpr(meta) => {
        unify(ty, UnitType(convertMeta(meta)), expr)
        UnitTerm_(convertMeta(meta))
      }
      case expr @ ListExpr(terms, meta) => {
        val t = newType
        // Relate the list type 'ty' to 'ListType(t)'
        state.addPropagator(ListOf(t, ty, expr))

        // For each term, check it with its own type variable and collect the results
        val termResults = terms.map { term =>
          val elemTy = newType
          val wellTypedTerm = elab(term, elemTy, effects)
          (wellTypedTerm, elemTy)
        }

        // Collect the types of the elements
        val elemTypes = termResults.map(_._2).toVector

        // Ensure that 't' is the union of the element types
        if (elemTypes.nonEmpty) state.addPropagator(UnionOf(t, elemTypes, expr))

        ListTerm(termResults.map(_._1), convertMeta(meta))
      }
      case expr @ TypeAnotationNoEffects(innerExpr, tyExpr, _) =>
        // Check the type annotation expression to get its type
        val declaredTyTerm = checkType(tyExpr)

        unify(ty, declaredTyTerm, expr)

        elab(innerExpr, declaredTyTerm, effects)
      case expr: FunctionExpr       => elabFunction(expr, ty, effects)
      case expr: Block              => elabBlock(expr, ty, effects)
      case expr: DesaltFunctionCall => elabFunctionCall(expr, ty, effects)
      case expr @ ObjectExpr(fields, _) =>
        elabObjectExpr(expr, fields, ty, effects)
      case expr @ DotCall(recordExpr, fieldExpr, telescopes, meta) =>
        if (telescopes.nonEmpty) {
          val problem = NotImplementedFeature("Field access with arguments is not yet supported", expr)
          ck.reporter.apply(problem)
          ErrorTerm(problem, convertMeta(expr.meta))
        } else {
          fieldExpr match {
            case Identifier(fieldName, _) =>
              val recordTy = newType
              val recordTerm = elab(recordExpr, recordTy, effects)
              val resultTerm = FieldAccessTerm(recordTerm, fieldName, toTerm(ty), convertMeta(meta))
              state.addPropagator(RecordFieldPropagator(recordTy, fieldName, ty, expr))
              resultTerm
            case _ =>
              val problem = InvalidFieldName(fieldExpr)
              ck.reporter.apply(problem)
              ErrorTerm(problem, convertMeta(expr.meta))
          }
        }
      case expr: Expr => {
        val problem = NotImplemented(expr)
        ck.reporter.apply(problem)
        ErrorTerm(problem, convertMeta(expr.meta))
      }
    }
  }

  // TODO: untested
  def elabObjectExpr(
      expr: ObjectExpr,
      fields: Vector[ObjectClause],
      ty: CellId[Term],
      effects: CIdOf[EffectsCell]
  )(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term = {
    // Create collections to store field keys and types
    val fieldTypeVars = scala.collection.mutable.Map[Term, CellId[Term]]()
    val elaboratedFields = fields.flatMap {
      case ObjectExprClauseOnValue(keyExpr, valueExpr) =>
        // Elaborate the key and value expressions
        val elaboratedKey = elab(keyExpr, newType, effects)
        val fieldType = newType
        val elaboratedValue = elab(valueExpr, fieldType, effects)
        val _ = fieldTypeVars.put(elaboratedKey, fieldType)
        Some(ObjectClauseValueTerm(elaboratedKey, elaboratedValue, convertMeta(expr.meta)))
      // Handle other possible clauses
      case _ => ???
    }

    // Construct the object term with elaborated fields
    val objectTerm = ObjectTerm(elaboratedFields, convertMeta(expr.meta))

    // Construct the expected object type
    val expectedObjectType = ObjectType(
      elaboratedFields.map { case ObjectClauseValueTerm(keyTerm, _, _) =>
        ObjectClauseValueTerm(keyTerm, Meta(fieldTypeVars(keyTerm)), convertMeta(expr.meta))
      },
      meta = convertMeta(expr.meta)
    )

    // Unify the expected type with the object's type
    unify(ty, expectedObjectType, expr)

    objectTerm
  }
}

trait DefaultImpl
    extends ProvideElaborater
    with ProvideImpl
    with ProvideElaboraterFunction
    with ProvideElaboraterFunctionCall
    with ProvideElaboraterBlock {

  def check(
      expr: Expr,
      ty: Option[Term] = None,
      effects: Option[Effects] = None,
      sementicCollector: SemanticCollector = NoopSemanticCollector
  ): TyckResult[Unit, Judge] = {
    implicit val collecter: UnusedVariableWarningWrapper =
      new UnusedVariableWarningWrapper(sementicCollector)
    val reporter = new VectorReporter[TyckProblem]
    implicit val get: Tyck = new Get(reporter, new MutBox(()))
    implicit val able: StateAbility[Tyck] = stateAbilityImpl
    val ty1: CellId[Term] = ty match {
      case Some(ty) => {
        val cell = literal[Term](ty)
        cell
      }
      case None => {
        val cell = newType
        cell
      }
    }
    val effects1: CIdOf[EffectsCell] = effects match {
      case Some(effects) => {
        val cell = toEffectsCell(effects)
        cell
      }
      case None => {
        newEffects
      }
    }
    implicit val ctx: Context = Context.default
    val wellTyped = elabId(expr, ty1, effects1)
    able.naiveZonk(Vector(ty1, effects1, wellTyped))
    val judge = Judge(
      able.readStable(wellTyped).get,
      able.readStable(ty1).get,
      able.readUnstable(effects1).get
    )
    val finalJudge = finalizeJudge(judge)

    TyckResult0((), finalJudge, reporter.getReports)

  }

  def finalizeJudge(
      judge0: Judge
  )(using
      ck: Tyck,
      able: StateAbility[Tyck],
      recording: SemanticCollector,
      reporter: Reporter[TyckProblem]
  ): Judge = {
    var judge = judge0
    boundary {
      while (true) {
        val metas = judge.collectMeta
        if (metas.isEmpty) break()
        able.naiveZonk(metas.map(x => x.unsafeRead[CellId[Term]]))
        judge = judge.replaceMeta(x => able.readUnstable(x.unsafeRead[CellId[Term]]).get)
      }
    }
    recording.metaFinished(x => able.readUnstable(x.unsafeRead[CellId[Term]]).get)
    judge
  }

  def checkTop(
      fileName: String,
      expr: Expr,
      reporter0: Reporter[Problem],
      sementicCollector: SemanticCollector = NoopSemanticCollector,
      loadedModules: LoadedModules = LoadedModules.Empty
  ): chester.syntax.TAST = {
    implicit val collecter: UnusedVariableWarningWrapper =
      new UnusedVariableWarningWrapper(sementicCollector)
    implicit val reporter: ReporterTrackError[Problem] = new ReporterTrackError(
      reporter0
    )
    implicit val get: Tyck = new Get(reporter, new MutBox(()))
    implicit val able: StateAbility[Tyck] = stateAbilityImpl
    implicit var ctx: Context = Context.default.copy(loadedModules = loadedModules)
    val (module, block): (ModuleRef, Block) = resolve(expr) match {
      case b @ Block(head +: heads, tail, _) =>
        resolve(head) match {
          case ModuleStmt(module, meta) => (module, Block(heads, tail, meta))
          case _                        => (DefaultModule, b)
        }
      case expr => (DefaultModule, Block(Vector(), Some(expr), expr.meta))
    }
    ctx = ctx.updateModule(module)
    val ty = newType
    val effects = newEffects
    val wellTyped = elabBlock(block, ty, effects)
    able.naiveZonk(Vector(ty, effects))
    val judge =
      Judge(wellTyped, able.readStable(ty).get, able.readUnstable(effects).get)
    val finalJudge = finalizeJudge(judge)

    TAST(
      fileName = fileName,
      module = module,
      ast = finalJudge.wellTyped.asInstanceOf[BlockTerm],
      ty = finalJudge.ty,
      effects = finalJudge.effects,
      problems = reporter.getSeverityMap
    )
  }
}

object Tycker extends DefaultImpl with ProvideMutable {}

export Tycker.{check, checkTop}