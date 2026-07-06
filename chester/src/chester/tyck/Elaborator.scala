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
/** Main elaborator object */
object Elaborator:
  /** Pre-register top-level def names across multiple CSTs so they can mutually reference each other. */
  def preRegisterDefs[M <: SolverModule](csts: Seq[CST], ctx: ElabContext)(using module: M, solver: module.Solver[ElabConstraint]): ElabContext = {
    csts.foldLeft(ctx) { (acc, cst) =>
      cst match
        case CST.Block(elements, _, _) =>
          elements.foldLeft(acc) { (innerCtx, elem) =>
            elem match
              case CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
                seqElems.toVector match
                  case _ +: CST.Symbol(name, _) +: _ =>
                    if innerCtx.lookup(name).isEmpty then
                      val defTypeCell = module.newOnceCell[ElabConstraint, AST](solver)
                      val defId = Uniqid.make[AST]
                      innerCtx.bind(name, defId, defTypeCell)
                    else innerCtx
                  case _ => innerCtx
              case CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists { case CST.Symbol("extension", _) => true; case _ => false } =>
                seqElems.toVector match
                  case _ +: CST.Symbol("def", _) +: CST.Symbol(name, _) +: rest =>
                    if innerCtx.lookup(name).isEmpty then
                      val defTypeCell = module.newOnceCell[ElabConstraint, AST](solver)
                      val defId = Uniqid.make[AST]
                      var methodName = name
                      if rest.length >= 3 && rest(0).isInstanceOf[CST.Symbol] && rest(0).asInstanceOf[CST.Symbol].name == "<" && rest(2).isInstanceOf[CST.Symbol] && rest(2).asInstanceOf[CST.Symbol].name == ">" then
                        methodName = rest(1).asInstanceOf[CST.Symbol].name
                      val newBindings = innerCtx.extensionBindings.updatedWith(methodName) {
                        case Some(s) => Some(s + defId)
                        case None    => Some(Set(defId))
                      }
                      innerCtx.bind(name, defId, defTypeCell).copy(extensionBindings = newBindings)
                    else innerCtx
                  case _ => innerCtx
              case _ => innerCtx
          }
        case CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists { case CST.Symbol("package", _) => true; case _ => false } =>
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
      case vr: VectorReporter[?] => vr.getReports.asInstanceOf[Vector[ElabProblem]]
      case _                     => Vector.empty

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
  def elaborateSafe[M <: SolverModule](
      cst: CST,
      reporter: Reporter[ElabProblem],
      ctx: Option[ElabContext] = None
  )(using module: M): (Option[AST], Option[AST]) = {

    val elaborationContext = ctx.getOrElse(
      ElabContext(
        bindings = Map.empty,
        types = Map.empty,
        builtins = ElabContext.defaultBuiltins,
        builtinTypes = ElabContext.defaultBuiltinTypes,
        reporter = reporter
      )
    )

    val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))

    val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
    val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

    try {
      module.addConstraint(solver, ElabConstraint.Infer(cst, resultCell, typeCell, elaborationContext))
      module.run(solver)
    } catch {
      case ex: MatchError =>
        reporter.report(ElabProblem.InternalElaborationFailure(ex.getMessage, cst.span))
        return (None, None)
      case ex: Throwable =>
        reporter.report(ElabProblem.InternalElaborationFailure(ex.getMessage.nn, cst.span))
        return (None, None)
    }

    val resultOpt = module.readStable(solver, resultCell)
    val tyOpt = module.readStable(solver, typeCell)

    if resultOpt.isEmpty then
      reporter.report(ElabProblem.InternalElaborationFailure(s"Failed to elaborate: $cst", cst.span))
    if tyOpt.isEmpty then
      reporter.report(ElabProblem.InternalElaborationFailure(s"Failed to infer type for: $cst", cst.span))

    val zonkedResult = resultOpt.map(substituteSolutions(_)(using module, solver))
    val zonkedTy = tyOpt.map(substituteSolutions(_)(using module, solver))

    (zonkedResult, zonkedTy)
  }

  def elaborate[M <: SolverModule](
      cst: CST,
      reporter: Reporter[ElabProblem],
      ctx: Option[ElabContext] = None
  )(using module: M): (AST, AST) = {
    val (resultOpt, tyOpt) = elaborateSafe(cst, reporter, ctx)
    val result = resultOpt.getOrElse(throw new Exception(s"Failed to elaborate: $cst"))
    val ty = tyOpt.getOrElse(throw new Exception(s"Failed to infer type for: $cst"))
    (result, ty)
  }

  /** Elaborate with default ProceduralSolver and new VectorReporter */
  def elaborate(cst: CST)(using Reporter[ElabProblem]): (AST, AST) =
    elaborate(cst, summon[Reporter[ElabProblem]], None)(using ProceduralSolverModule)

  /** Elaborate with custom context */
  def elaborate(cst: CST, ctx: ElabContext): (AST, AST) =
    elaborate(cst, ctx.reporter, Some(ctx))(using ProceduralSolverModule)

  /** Expose normalizeType for tests and downstream utilities. */
  def normalizeType(ast: AST): AST = CoreTypeChecker.normalizeType(ast)
