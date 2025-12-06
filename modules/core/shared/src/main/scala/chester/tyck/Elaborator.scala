package chester.tyck

import scala.language.experimental.genericNumberLiterals
import chester.core.{AST, CST, Param, Arg}
import chester.error.{Span, Problem, Reporter}
import chester.uniqid.{UniqidOf, Uniqid}
import chester.utils.elab.*
import chester.utils.{HoldNotReadable, given}
import chester.utils.doc.{Doc, DocConf, DocOps, StringPrinter, given}
import scala.collection.mutable

/** Elaboration problems */
enum ElabProblem(val span0: Option[Span]) extends Problem:
  case UnboundVariable(name: String, override val span0: Option[Span]) extends ElabProblem(span0)
  case TypeMismatch(expected: AST, actual: AST, override val span0: Option[Span]) extends ElabProblem(span0)
  case NotAFunction(ty: AST, override val span0: Option[Span]) extends ElabProblem(span0)
  case NotAUniverse(ty: AST, override val span0: Option[Span]) extends ElabProblem(span0)

  override def stage: Problem.Stage = Problem.Stage.TYCK
  override def severity: Problem.Severity = Problem.Severity.Error

  override def toDoc(using DocConf): Doc = this match
    case ElabProblem.UnboundVariable(name, _) =>
      Doc.text(s"Unbound variable: $name")
    case ElabProblem.TypeMismatch(expected, actual, _) =>
      Doc.text(s"Type mismatch: expected ${expected.toDoc.render}, but got ${actual.toDoc.render}")
    case ElabProblem.NotAFunction(ty, _) =>
      Doc.text(s"Not a function type: ${ty.toDoc.render}")
    case ElabProblem.NotAUniverse(ty, _) =>
      Doc.text(s"Not a universe type: ${ty.toDoc.render}")

/** Elaboration context tracking bindings and types during CST to AST conversion */
case class ElabContext(
    bindings: Map[String, UniqidOf[AST]], // Name -> variable ID mapping
    types: Map[UniqidOf[AST], CellRW[AST]], // Variable ID -> type cell mapping
    builtins: Set[String] = ElabContext.defaultBuiltins, // Built-in names
    reporter: Reporter[ElabProblem] // Error reporter
):
  def bind(name: String, id: UniqidOf[AST], ty: CellRW[AST]): ElabContext =
    copy(bindings = bindings + (name -> id), types = types + (id -> ty))

  def lookup(name: String): Option[UniqidOf[AST]] = bindings.get(name)
  def lookupType(id: UniqidOf[AST]): Option[CellRW[AST]] = types.get(id)
  def isBuiltin(name: String): Boolean = builtins.contains(name)

object ElabContext:
  val defaultBuiltins: Set[String] = Set(
    "Integer",
    "Int",
    "String",
    "Bool",
    "Nat",
    "Natural",
    "Type",
    "U",
    "Universe",
    "true",
    "false"
  )

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
      ctx: ElabContext
  )

  /** Unify two types (type equality) */
  case Unify(
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
      params: CellRW[Vector[Param]],
      resultTy: CellRW[AST]
  )

  /** Assemble a block from elaborated elements */
  case AssembleBlock(
      elemResults: Vector[CellR[AST]],
      elemTypes: Vector[CellR[AST]],
      result: CellRW[AST],
      inferredTy: CellRW[AST],
      span: Option[Span]
  )

/** Handler for elaboration constraints */
class ElabHandler extends Handler[ElabConstraint]:

  def run[M <: SolverModule](constraint: ElabConstraint)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given
    constraint match
      case c: ElabConstraint.Check         => handleCheck(c)
      case c: ElabConstraint.Infer         => handleInfer(c)
      case c: ElabConstraint.Unify         => handleUnify(c)
      case c: ElabConstraint.IsUniverse    => handleIsUniverse(c)
      case c: ElabConstraint.IsPi          => handleIsPi(c)
      case c: ElabConstraint.AssembleBlock => handleAssembleBlock(c)

  def canDefaulting(level: DefaultingLevel): Boolean = false

  private def handleCheck[M <: SolverModule](c: ElabConstraint.Check)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    // For now, convert Check to Infer + Unify
    module.readStable(solver, c.expectedTy) match
      case Some(expectedTy) =>
        // Infer the type and unify with expected
        val inferredTy = module.newOnceCell[ElabConstraint, AST](solver)
        module.addConstraint(solver, ElabConstraint.Infer(c.cst, c.result, inferredTy, c.ctx))
        module.addConstraint(solver, ElabConstraint.Unify(inferredTy, c.expectedTy, c.cst.span, c.ctx))
        Result.Done
      case None =>
        Result.Waiting(c.expectedTy)

  private def handleInfer[M <: SolverModule](c: ElabConstraint.Infer)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    c.cst match
      // Integer literal: type is Universe(0)
      case CST.IntegerLiteral(value, span) =>
        val ast = AST.IntLit(value, span)
        module.fill(solver, c.result, ast)
        module.fill(solver, c.inferredTy, AST.Universe(AST.IntLit(0, None), None))
        Result.Done

      // String literal: type is Universe(0)
      case CST.StringLiteral(value, span) =>
        val ast = AST.StringLit(value, span)
        module.fill(solver, c.result, ast)
        module.fill(solver, c.inferredTy, AST.Universe(AST.IntLit(0, None), None))
        Result.Done

      // Symbol: lookup in context
      case CST.Symbol(name, span) =>
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
            // Check if it's a builtin
            if c.ctx.isBuiltin(name) then
              // Built-in: create a special reference
              val metaId = Uniqid.make[AST]
              val ast = AST.Ref(metaId, name, span)
              module.fill(solver, c.result, ast)
              // Built-ins have Universe(0) as type for now
              module.fill(solver, c.inferredTy, AST.Universe(AST.IntLit(0, None), None))
              Result.Done
            else
              // Unbound variable - report error and recover
              c.ctx.reporter.report(ElabProblem.UnboundVariable(name, span))

              // Error recovery: create a meta-variable to continue elaboration
              val metaId = Uniqid.make[AST]
              val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
              val ast = AST.Ref(metaId, name, span)
              module.fill(solver, c.result, ast)
              module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
              Result.Done

      // Tuple: infer each element
      case CST.Tuple(elements, span) =>
        val elemResults = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))
        val elemTypes = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))

        elements.zip(elemResults).zip(elemTypes).foreach { case ((cstElem, resultCell), tyCell) =>
          module.addConstraint(solver, ElabConstraint.Infer(cstElem, resultCell, tyCell, c.ctx))
        }

        // Wait for all elements to be filled
        val allFilled = elemResults.forall(module.hasStableValue(solver, _))
        if allFilled then
          val astElems = elemResults.flatMap(module.readStable(solver, _))
          if astElems.size == elements.size then
            module.fill(solver, c.result, AST.Tuple(astElems, span))

            // Type is Tuple of element types
            val allTypesFilled = elemTypes.forall(module.hasStableValue(solver, _))
            if allTypesFilled then
              val types = elemTypes.flatMap(module.readStable(solver, _))
              if types.size == elements.size then
                module.fill(solver, c.inferredTy, AST.Tuple(types, None))
                Result.Done
              else Result.Waiting(elemTypes.filter(!module.hasStableValue(solver, _))*)
            else Result.Waiting(elemTypes.filter(!module.hasStableValue(solver, _))*)
          else Result.Waiting(elemResults.filter(!module.hasStableValue(solver, _))*)
        else Result.Waiting(elemResults.filter(!module.hasStableValue(solver, _))*)

      // List literal: infer elements
      case CST.ListLiteral(elements, span) =>
        val elemResults = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))
        val elemTypes = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))

        elements.zip(elemResults).zip(elemTypes).foreach { case ((cstElem, resultCell), tyCell) =>
          module.addConstraint(solver, ElabConstraint.Infer(cstElem, resultCell, tyCell, c.ctx))
        }

        // For now, don't unify element types - just create the list
        val allFilled = elemResults.forall(module.hasStableValue(solver, _))
        if allFilled then
          val astElems = elemResults.flatMap(module.readStable(solver, _))
          if astElems.size == elements.size then
            module.fill(solver, c.result, AST.ListLit(astElems, span))
            // Type is List of some element type (simplified)
            module.fill(solver, c.inferredTy, AST.Universe(AST.IntLit(0, None), None))
            Result.Done
          else Result.Waiting(elemResults.filter(!module.hasStableValue(solver, _))*)
        else Result.Waiting(elemResults.filter(!module.hasStableValue(solver, _))*)

      // Block: infer each element
      case CST.Block(elements, tail, span) =>
        val allElements = tail.map(elements :+ _).getOrElse(elements)
        if allElements.isEmpty then
          // Empty block
          val unit = AST.Tuple(Vector.empty, span)
          module.fill(solver, c.result, unit)
          module.fill(solver, c.inferredTy, AST.Tuple(Vector.empty, None))
          Result.Done
        else
          // Create cells for each element and add Infer constraints
          val elemResults = allElements.map { elem =>
            val elemResult = module.newOnceCell[ElabConstraint, AST](solver)
            val elemType = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(elem, elemResult, elemType, c.ctx))
            (elemResult, elemType)
          }

          // Add a helper constraint to assemble the block once all elements are elaborated
          module.addConstraint(
            solver,
            ElabConstraint.AssembleBlock(
              elemResults.map(_._1).toVector,
              elemResults.map(_._2).toVector,
              c.result,
              c.inferredTy,
              span
            )
          )

          // Return Done - the AssembleBlock constraint will wait for the element cells
          Result.Done

      // SeqOf: for now, just elaborate as a Block
      case CST.SeqOf(elements, span) =>
        val blockCst = CST.Block(elements.toVector.dropRight(1), Some(elements.last), span)
        module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
        Result.Done

  private def handleUnify[M <: SolverModule](c: ElabConstraint.Unify)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    (module.readStable(solver, c.ty1), module.readStable(solver, c.ty2)) match
      case (Some(t1), Some(t2)) =>
        // Simple structural unification
        if unifyTypes(t1, t2) then Result.Done
        else
          // Types don't unify - report error and continue (error recovery)
          c.ctx.reporter.report(ElabProblem.TypeMismatch(t1, t2, c.span))
          Result.Done
      case (None, _) => Result.Waiting(c.ty1)
      case (_, None) => Result.Waiting(c.ty2)

  private def unifyTypes(t1: AST, t2: AST): Boolean =
    (t1, t2) match
      case (AST.IntLit(v1, _), AST.IntLit(v2, _))       => v1 == v2
      case (AST.StringLit(v1, _), AST.StringLit(v2, _)) => v1 == v2
      case (AST.Ref(id1, _, _), AST.Ref(id2, _, _))     => id1 == id2
      case (AST.Universe(l1, _), AST.Universe(l2, _))   => unifyTypes(l1, l2)
      case (AST.Tuple(e1, _), AST.Tuple(e2, _)) =>
        e1.size == e2.size && e1.zip(e2).forall((a, b) => unifyTypes(a, b))
      case (AST.Pi(p1, r1, _), AST.Pi(p2, r2, _)) =>
        p1.size == p2.size &&
        p1.zip(p2).forall((a, b) => unifyTypes(a.ty, b.ty)) &&
        unifyTypes(r1, r2)
      case (AST.MetaCell(_, _), _) => true // Meta-variables unify with anything for now
      case (_, AST.MetaCell(_, _)) => true
      case _                       => false

  private def handleAssembleBlock[M <: SolverModule](
      c: ElabConstraint.AssembleBlock
  )(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    // Check if all element results are filled
    val allFilled = c.elemResults.forall(module.hasStableValue(solver, _))

    if allFilled then
      // All elements are elaborated, assemble the block
      val astElems = c.elemResults.flatMap(module.readStable(solver, _))
      if astElems.size == c.elemResults.size then
        module.fill(solver, c.result, AST.Block(astElems, c.span))

        // Block type is the type of the last element
        if c.elemTypes.nonEmpty then
          module.readStable(solver, c.elemTypes.last) match
            case Some(ty) =>
              module.fill(solver, c.inferredTy, ty)
              Result.Done
            case None =>
              Result.Waiting(c.elemTypes.last)
        else
          // Empty block (shouldn't happen, but handle it)
          module.fill(solver, c.inferredTy, AST.Tuple(Vector.empty, None))
          Result.Done
      else
        // Some elements failed to elaborate
        Result.Waiting(c.elemResults.filter(!module.hasStableValue(solver, _))*)
    else
      // Wait for all element results
      Result.Waiting(c.elemResults.filter(!module.hasStableValue(solver, _))*)

  private def handleIsUniverse[M <: SolverModule](c: ElabConstraint.IsUniverse)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    module.readStable(solver, c.ty) match
      case Some(AST.Universe(level, _)) =>
        module.fill(solver, c.level, level)
        Result.Done
      case Some(_) =>
        // Not a universe - error
        Result.Done
      case None =>
        Result.Waiting(c.ty)

  private def handleIsPi[M <: SolverModule](c: ElabConstraint.IsPi)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    module.readStable(solver, c.ty) match
      case Some(AST.Pi(params, resultTy, _)) =>
        module.fill(solver, c.params, params)
        module.fill(solver, c.resultTy, resultTy)
        Result.Done
      case Some(_) =>
        // Not a Pi type - error
        Result.Done
      case None =>
        Result.Waiting(c.ty)

/** Handler configuration for elaboration */
class ElabHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
  private val handler = new ElabHandler()

  def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] =
    Some(handler)

/** Main elaborator object */
object Elaborator:
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
  )(using module: M): (AST, AST) =
    import module.given

    val elaborationContext = ctx.getOrElse(
      ElabContext(Map.empty, Map.empty, ElabContext.defaultBuiltins, reporter)
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

    (result, ty)

  /** Elaborate with default ProceduralSolver and new VectorReporter */
  def elaborate(cst: CST)(using Reporter[ElabProblem]): (AST, AST) =
    elaborate(cst, summon[Reporter[ElabProblem]], None)(using ProceduralSolverModule)

  /** Elaborate with custom context */
  def elaborate(cst: CST, ctx: ElabContext): (AST, AST) =
    elaborate(cst, ctx.reporter, Some(ctx))(using ProceduralSolverModule)
