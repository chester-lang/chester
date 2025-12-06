package chester.tyck

import scala.language.experimental.genericNumberLiterals
import chester.core.{AST, CST, Param, Arg, Telescope, Implicitness}
import chester.error.{Span, Problem, Reporter}
import chester.uniqid.{UniqidOf, Uniqid}
import chester.utils.elab.*
import chester.utils.{HoldNotReadable, given}
import chester.utils.doc.{Doc, DocConf, DocOps, StringPrinter, given}
import cats.data.NonEmptyVector
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
    builtinTypes: Map[String, AST] = ElabContext.defaultBuiltinTypes, // Built-in types
    reporter: Reporter[ElabProblem] // Error reporter
):
  def bind(name: String, id: UniqidOf[AST], ty: CellRW[AST]): ElabContext =
    copy(bindings = bindings + (name -> id), types = types + (id -> ty))

  def lookup(name: String): Option[UniqidOf[AST]] = bindings.get(name)
  def lookupType(id: UniqidOf[AST]): Option[CellRW[AST]] = types.get(id)
  def isBuiltin(name: String): Boolean = builtins.contains(name)
  def lookupBuiltinType(name: String): Option[AST] = builtinTypes.get(name)

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
    "false",
    "id"
  )

  /** Built-in function types
    * id : [a: Type] (x: a) -> a
    */
  val defaultBuiltinTypes: Map[String, AST] = {
    val typeUniverse = AST.Universe(AST.IntLit(0, None), None)
    val aId = Uniqid.make[AST]
    val xId = Uniqid.make[AST]
    val aRef = AST.Ref(aId, "a", None)
    
    Map(
      "id" -> AST.Pi(
        Vector(
          Telescope(Vector(Param(aId, "a", typeUniverse, Implicitness.Implicit, None)), Implicitness.Implicit),
          Telescope(Vector(Param(xId, "x", aRef, Implicitness.Explicit, None)), Implicitness.Explicit)
        ),
        aRef,
        None
      )
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
      telescopes: CellRW[Vector[Telescope]],
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

  /** Assemble a function application from elaborated function and arguments */
  case AssembleApp(
      funcResult: CellR[AST],
      funcTy: CellR[AST],
      argResults: Vector[CellR[AST]],
      argTypes: Vector[CellR[AST]],
      result: CellRW[AST],
      inferredTy: CellRW[AST],
      span: Option[Span],
      ctx: ElabContext
  )

/** Substitute arguments for parameters in a type
  * Replaces occurrences of parameter IDs with corresponding argument ASTs
  */
def substituteInType(ty: AST, substitutions: Map[UniqidOf[AST], AST]): AST =
  ty match
    case AST.Ref(id, name, span) =>
      substitutions.getOrElse(id, ty)
    case AST.Tuple(elements, span) =>
      AST.Tuple(elements.map(substituteInType(_, substitutions)), span)
    case AST.ListLit(elements, span) =>
      AST.ListLit(elements.map(substituteInType(_, substitutions)), span)
    case AST.Block(elements, span) =>
      AST.Block(elements.map(substituteInType(_, substitutions)), span)
    case AST.Universe(level, span) =>
      AST.Universe(substituteInType(level, substitutions), span)
    case AST.Pi(telescopes, resultTy, span) =>
      // Don't substitute bound variables
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))),
          tel.implicitness
        )
      }
      val boundIds = telescopes.flatMap(_.params.map(_.id)).toSet
      val filteredSubs = substitutions.filterNot { case (id, _) => boundIds.contains(id) }
      AST.Pi(newTelescopes, substituteInType(resultTy, filteredSubs), span)
    case AST.Lam(telescopes, body, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))),
          tel.implicitness
        )
      }
      val boundIds = telescopes.flatMap(_.params.map(_.id)).toSet
      val filteredSubs = substitutions.filterNot { case (id, _) => boundIds.contains(id) }
      AST.Lam(newTelescopes, substituteInType(body, filteredSubs), span)
    case AST.App(func, args, span) =>
      AST.App(
        substituteInType(func, substitutions),
        args.map(a => Arg(a.name, substituteInType(a.value, substitutions))),
        span
      )
    case AST.Let(id, name, ty, value, body, span) =>
      val filteredSubs = substitutions - id
      AST.Let(
        id, name,
        ty.map(substituteInType(_, substitutions)),
        substituteInType(value, substitutions),
        substituteInType(body, filteredSubs),
        span
      )
    case AST.Def(id, name, telescopes, resultTy, body, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))),
          tel.implicitness
        )
      }
      val boundIds = id +: telescopes.flatMap(_.params.map(_.id))
      val filteredSubs = substitutions.filterNot { case (id, _) => boundIds.contains(id) }
      AST.Def(
        id, name, newTelescopes,
        resultTy.map(substituteInType(_, filteredSubs)),
        substituteInType(body, filteredSubs),
        span
      )
    case AST.Ann(expr, ty, span) =>
      AST.Ann(substituteInType(expr, substitutions), substituteInType(ty, substitutions), span)
    case other @ (AST.StringLit(_, _) | AST.IntLit(_, _) | AST.MetaCell(_, _)) =>
      other

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
      case c: ElabConstraint.AssembleApp   => handleAssembleApp(c)

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
              // Look up builtin type
              c.ctx.lookupBuiltinType(name) match
                case Some(ty) =>
                  module.fill(solver, c.inferredTy, ty)
                case None =>
                  // Default: Universe(0)
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

        // Check all elements have been elaborated
        val allFilled = elemResults.forall(module.hasStableValue(solver, _))
        if allFilled then
          val astElems = elemResults.flatMap(module.readStable(solver, _))
          if astElems.size == elements.size then
            module.fill(solver, c.result, AST.ListLit(astElems, span))
            
            // Infer list type: List[T] where T is unified from all element types
            val allTypesFilled = elemTypes.forall(module.hasStableValue(solver, _))
            if allTypesFilled then
              val types = elemTypes.flatMap(module.readStable(solver, _))
              if types.nonEmpty then
                // Use first element type as base, unify others with it
                val elemTy = types.head
                types.tail.foreach { ty =>
                  val cell1 = module.newOnceCell[ElabConstraint, AST](solver)
                  module.fill(solver, cell1, elemTy)
                  val cell2 = module.newOnceCell[ElabConstraint, AST](solver)
                  module.fill(solver, cell2, ty)
                  module.addConstraint(solver, ElabConstraint.Unify(cell1, cell2, span, c.ctx))
                }
                // Type is List applied to element type (represented as App for now)
                val listTy = AST.App(
                  AST.Ref(Uniqid.make, "List", None),
                  Vector(Arg(None, elemTy)),
                  None
                )
                module.fill(solver, c.inferredTy, listTy)
              else
                // Empty list - use polymorphic type
                module.fill(solver, c.inferredTy, AST.App(
                  AST.Ref(Uniqid.make, "List", None),
                  Vector(Arg(None, AST.MetaCell(HoldNotReadable(module.newOnceCell[ElabConstraint, AST](solver)), None))),
                  None
                ))
              Result.Done
            else
              Result.Waiting(elemTypes.filter(!module.hasStableValue(solver, _))*)
          else Result.Waiting(elemResults.filter(!module.hasStableValue(solver, _))*)
        else Result.Waiting(elemResults.filter(!module.hasStableValue(solver, _))*)

      // Block: infer each element
      case CST.Block(elements, tail, span) =>
        // Check if tail contains a def statement (which is not allowed)
        tail.foreach { t =>
          if isDefStatement(t) then
            c.ctx.reporter.report(ElabProblem.UnboundVariable("def statement not allowed in tail position", t.span))
        }

        if elements.isEmpty && tail.isEmpty then
          // Empty block
          val unit = AST.Tuple(Vector.empty, span)
          module.fill(solver, c.result, unit)
          module.fill(solver, c.inferredTy, AST.Tuple(Vector.empty, None))
          Result.Done
        else
          // Elaborate elements (can contain def statements)
          val elemResults = elements.map { elem =>
            val elemResult = module.newOnceCell[ElabConstraint, AST](solver)
            val elemType = module.newOnceCell[ElabConstraint, AST](solver)
            // Check if this element is a def statement
            elem match
              case CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
                // This is a def statement - handle it specially (inline, not via constraint)
                val elems = seqElems.toVector
                val defConstraint: ElabConstraint.Infer = ElabConstraint.Infer(elem, elemResult, elemType, c.ctx)
                handleDefStatement(defConstraint, elems, elem.span)(using module, solver)
              case _ =>
                // Regular element - add Infer constraint
                module.addConstraint(solver, ElabConstraint.Infer(elem, elemResult, elemType, c.ctx))
            (elemResult, elemType)
          }

          // Elaborate tail (cannot contain def statements)
          val tailResults = tail.map { t =>
            val tailResult = module.newOnceCell[ElabConstraint, AST](solver)
            val tailType = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(t, tailResult, tailType, c.ctx))
            (tailResult, tailType)
          }.toVector

          val allResults = elemResults ++ tailResults

          // Add a helper constraint to assemble the block once all elements are elaborated
          module.addConstraint(
            solver,
            ElabConstraint.AssembleBlock(
              allResults.map(_._1),
              allResults.map(_._2),
              c.result,
              c.inferredTy,
              span
            )
          )

          // Return Done - the AssembleBlock constraint will wait for the element cells
          Result.Done

      // SeqOf: could be function application or sequence (but NOT def - def only allowed in Block elements)
      case CST.SeqOf(elements, span) =>
        val elems = elements.toVector
        
        // Report error if this looks like a def statement (only allowed in Block elements)
        if elems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } then
          c.ctx.reporter.report(ElabProblem.UnboundVariable("def statement only allowed in block elements, not at top level or in expressions", span))
          // Error recovery: treat as unbound variable reference to "def"
          val metaId = Uniqid.make[AST]
          val ast = AST.Ref(metaId, "def", span)
          module.fill(solver, c.result, ast)
          val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
          module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
          Result.Done
        // Check for function application: f(args) becomes SeqOf(f, Tuple(args))
        else if elems.length == 2 && elems(1).isInstanceOf[CST.Tuple] then
          handleFunctionApplication(c, elems(0), elems(1).asInstanceOf[CST.Tuple], span)
        else
          // Default: treat as a block-like sequence
          val blockCst = CST.Block(elems.dropRight(1), Some(elems.last), span)
          module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
          Result.Done

  /** Handle function application: f(args) */
  private def handleFunctionApplication[M <: SolverModule](
      c: ElabConstraint.Infer,
      funcCst: CST,
      argsTuple: CST.Tuple,
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    // Elaborate function
    val funcResult = module.newOnceCell[ElabConstraint, AST](solver)
    val funcTy = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(funcCst, funcResult, funcTy, c.ctx))

    // Elaborate arguments
    val argPairs = argsTuple.elements.map { arg =>
      val argResult = module.newOnceCell[ElabConstraint, AST](solver)
      val argTy = module.newOnceCell[ElabConstraint, AST](solver)
      module.addConstraint(solver, ElabConstraint.Infer(arg, argResult, argTy, c.ctx))
      (argResult, argTy)
    }
    val argResults = argPairs.map(_._1)
    val argTypes = argPairs.map(_._2)

    // Add constraint to assemble application once all parts are elaborated
    module.addConstraint(solver, ElabConstraint.AssembleApp(
      funcResult, funcTy, argResults, argTypes, c.result, c.inferredTy, span, c.ctx
    ))

    Result.Done

  /** Handle def statement: def name [implicit] (explicit) : resultTy = body */
  private def handleDefStatement[M <: SolverModule](
      c: ElabConstraint.Infer,
      elems: Vector[CST],
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    // Parse: def name [telescope]* (telescope)* = body
    // or:    def name [telescope]* (telescope)* : type = body
    if elems.length < 4 then
      c.ctx.reporter.report(ElabProblem.UnboundVariable("Invalid def syntax", span))
      module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
      module.fill(solver, c.inferredTy, AST.Universe(AST.IntLit(0, None), None))
      return Result.Done

    val name = elems(1) match
      case CST.Symbol(n, _) => n
      case _ =>
        c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected name after def", span))
        "<error>"

    // Collect telescopes (lists for implicit, tuples for explicit)
    var idx = 2
    val telescopes = scala.collection.mutable.ArrayBuffer.empty[Telescope]
    
    while idx < elems.length && (elems(idx).isInstanceOf[CST.ListLiteral] || elems(idx).isInstanceOf[CST.Tuple]) do
      elems(idx) match
        case CST.ListLiteral(params, _) =>
          telescopes += parseTelescopeFromCST(params, Implicitness.Implicit, c.ctx)(using module, solver)
          idx += 1
        case CST.Tuple(params, _) =>
          telescopes += parseTelescopeFromCST(params, Implicitness.Explicit, c.ctx)(using module, solver)
          idx += 1
        case _ => ()

    // Check for optional result type annotation
    var resultTy: Option[AST] = None
    if idx < elems.length && elems(idx).isInstanceOf[CST.Symbol] && elems(idx).asInstanceOf[CST.Symbol].name == ":" then
      idx += 1
      if idx < elems.length then
        val resultTyCell = module.newOnceCell[ElabConstraint, AST](solver)
        val resultTyTyCell = module.newOnceCell[ElabConstraint, AST](solver)
        module.addConstraint(solver, ElabConstraint.Infer(elems(idx), resultTyCell, resultTyTyCell, c.ctx))
        if !module.hasStableValue(solver, resultTyCell) then
          return Result.Waiting(resultTyCell)
        resultTy = module.readStable(solver, resultTyCell)
        idx += 1

    // Expect =
    if idx >= elems.length || !elems(idx).isInstanceOf[CST.Symbol] || elems(idx).asInstanceOf[CST.Symbol].name != "=" then
      c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected = in def", span))
      module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
      module.fill(solver, c.inferredTy, AST.Universe(AST.IntLit(0, None), None))
      return Result.Done

    idx += 1

    // Body is remaining elements
    val bodyCst = if idx < elems.length then
      if elems.length - idx == 1 then elems(idx)
      else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(elems.drop(idx)), span)
    else
      c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected body in def", span))
      CST.Symbol("<error>", span)

    // Create extended context with parameters
    var extCtx = c.ctx
    for telescope <- telescopes; param <- telescope.params do
      val paramTyCell = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, paramTyCell, param.ty)
      extCtx = extCtx.bind(param.name, param.id, paramTyCell)

    // Elaborate body
    val bodyResult = module.newOnceCell[ElabConstraint, AST](solver)
    val bodyTy = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(bodyCst, bodyResult, bodyTy, extCtx))

    if !module.hasStableValue(solver, bodyResult) then
      return Result.Waiting(bodyResult)

    val body = module.readStable(solver, bodyResult).get
    val defId = Uniqid.make[AST]
    val defAst = AST.Def(defId, name, telescopes.toVector, resultTy, body, span)
    
    module.fill(solver, c.result, defAst)
    
    // Type of def is Pi type
    val piTy = resultTy match
      case Some(rt) => AST.Pi(telescopes.toVector, rt, span)
      case None =>
        if module.hasStableValue(solver, bodyTy) then
          AST.Pi(telescopes.toVector, module.readStable(solver, bodyTy).get, span)
        else
          return Result.Waiting(bodyTy)
    
    module.fill(solver, c.inferredTy, piTy)
    Result.Done

  /** Parse a telescope from CST parameter list
    * This creates a telescope with placeholder types that should be elaborated
    * in a separate pass by the caller
    */
  private def parseTelescopeFromCST[M <: SolverModule](
      params: Vector[CST],
      implicitness: Implicitness,
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): Telescope =
    import module.given
    
    val parsedParams = params.flatMap {
      // Pattern: name : type
      case CST.SeqOf(elems, _) if elems.length == 3 =>
        (elems(0), elems(1), elems(2)) match
          case (CST.Symbol(name, _), CST.Symbol(":", _), typeCst) =>
            val paramId = Uniqid.make[AST]
            // Elaborate the type
            val tyResult = module.newOnceCell[ElabConstraint, AST](solver)
            val tyTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(typeCst, tyResult, tyTy, ctx))
            
            // Get elaborated type or use placeholder
            val paramTy = module.readStable(solver, tyResult).getOrElse(
              AST.Ref(Uniqid.make, "Type", None)
            )
            Some(Param(paramId, name, paramTy, implicitness, None))
          case _ => None
      case CST.Symbol(name, _) =>
        // Just a name, type must be inferred
        val paramId = Uniqid.make[AST]
        // Use a meta-variable for the type
        val tyCell = module.newOnceCell[ElabConstraint, AST](solver)
        val paramTy = AST.MetaCell(HoldNotReadable(tyCell), None)
        Some(Param(paramId, name, paramTy, implicitness, None))
      case _ => None
    }
    Telescope(parsedParams, implicitness)

  /** Check if a CST node is a def statement */
  private def isDefStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.headOption.exists { case CST.Symbol("def", _) => true; case _ => false }
    case _ => false

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
      case (AST.Pi(t1, r1, _), AST.Pi(t2, r2, _)) =>
        t1.size == t2.size &&
        t1.zip(t2).forall((a, b) => 
          a.params.size == b.params.size &&
          a.params.zip(b.params).forall((p1, p2) => unifyTypes(p1.ty, p2.ty))
        ) &&
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
      case Some(AST.Pi(telescopes, resultTy, _)) =>
        module.fill(solver, c.telescopes, telescopes)
        module.fill(solver, c.resultTy, resultTy)
        Result.Done
      case Some(_) =>
        // Not a Pi type - error
        Result.Done
      case None =>
        Result.Waiting(c.ty)

  private def handleAssembleApp[M <: SolverModule](c: ElabConstraint.AssembleApp)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    // Wait for all parts to be elaborated
    if !module.hasStableValue(solver, c.funcResult) then
      return Result.Waiting(c.funcResult)
    
    if !c.argResults.forall(module.hasStableValue(solver, _)) then
      return Result.Waiting(c.argResults.filter(!module.hasStableValue(solver, _))*)

    // Build application AST
    val func = module.readStable(solver, c.funcResult).get
    val args = c.argResults.flatMap(module.readStable(solver, _))
    val app = AST.App(func, args.map(Arg(None, _)), c.span)
    module.fill(solver, c.result, app)

    // Type check: function type should be Pi
    if !module.hasStableValue(solver, c.funcTy) then
      return Result.Waiting(c.funcTy)

    module.readStable(solver, c.funcTy) match
      case Some(AST.Pi(telescopes, resultTy, _)) =>
        val allParams = telescopes.flatMap(_.params)
        
        // Check arity matches
        if allParams.size != args.size then
          c.ctx.reporter.report(ElabProblem.NotAFunction(func, c.span))
          module.fill(solver, c.inferredTy, AST.Universe(AST.IntLit(0, None), None))
          return Result.Done
        
        // Type check arguments against parameter types
        allParams.zip(args).foreach { case (param, arg) =>
          val argTyCell = module.newOnceCell[ElabConstraint, AST](solver)
          module.fill(solver, argTyCell, param.ty)
          c.argTypes.lift(args.indexOf(arg)).foreach { argTy =>
            module.addConstraint(solver, ElabConstraint.Unify(argTy, argTyCell, c.span, c.ctx))
          }
        }
        
        // Perform substitution: replace parameter references in result type with arguments
        val substitutedTy = substituteInType(resultTy, allParams.map(_.id).zip(args).toMap)
        module.fill(solver, c.inferredTy, substitutedTy)
        Result.Done
      case Some(ty) =>
        c.ctx.reporter.report(ElabProblem.NotAFunction(ty, c.span))
        module.fill(solver, c.inferredTy, AST.Universe(AST.IntLit(0, None), None))
        Result.Done
      case None =>
        Result.Waiting(c.funcTy)

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
      ElabContext(Map.empty, Map.empty, ElabContext.defaultBuiltins, ElabContext.defaultBuiltinTypes, reporter)
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
