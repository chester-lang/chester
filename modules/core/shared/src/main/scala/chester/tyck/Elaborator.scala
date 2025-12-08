package chester.tyck

import chester.core.{AST, Arg, CST, Implicitness, Param, Telescope}
import chester.error.{Problem, Reporter, Span}
import chester.uniqid.{Uniqid, UniqidOf}
import chester.utils.elab.*
import chester.utils.{HoldNotReadable, given}
import chester.utils.doc.{<>, Doc, DocConf, DocOps, StringPrinter, ToDoc, given}
import cats.data.NonEmptyVector

import scala.language.experimental.genericNumberLiterals
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
      (Doc.text("Type mismatch: expected "): ToDoc) <> expected.toDoc <> Doc.text(", but got ") <> actual.toDoc
    case ElabProblem.NotAFunction(ty, _) =>
      (Doc.text("Not a function type: "): ToDoc) <> ty.toDoc
    case ElabProblem.NotAUniverse(ty, _) =>
      (Doc.text("Not a universe type: "): ToDoc) <> ty.toDoc

/** Elaboration context tracking bindings and types during CST to AST conversion */
case class ElabContext(
    bindings: Map[String, UniqidOf[AST]], // Name -> variable ID mapping
    types: Map[UniqidOf[AST], CellRW[AST]], // Variable ID -> type cell mapping
    defBodies: Map[UniqidOf[AST], CellRW[AST]] = Map.empty, // Definition ID -> AST cell mapping
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
    "List"
  )

  val defaultBuiltinTypes: Map[String, AST] = Map.empty

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
      defTypeCell: CellRW[AST],
      span: Option[Span]
  )

/** Substitute arguments for parameters in a type Replaces occurrences of parameter IDs with corresponding argument ASTs
  */
def substituteInType(ty: AST, substitutions: Map[UniqidOf[AST], AST]): AST =
  ty match
    case AST.Ref(id, name, span) =>
      substitutions.getOrElse(id, ty)
    case AST.Tuple(elements, span) =>
      AST.Tuple(elements.map(substituteInType(_, substitutions)), span)
    case AST.ListLit(elements, span) =>
      AST.ListLit(elements.map(substituteInType(_, substitutions)), span)
    case AST.Block(elements, tail, span) =>
      AST.Block(elements.map(substituteInType(_, substitutions)), substituteInType(tail, substitutions), span)
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
    case AST.Pi(telescopes, resultTy, span) =>
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
      AST.Pi(newTelescopes, substituteInType(resultTy, filteredSubs), span)
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
    case AST.Def(id, name, telescopes, resultTy, body, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p =>
            Param(p.id, p.name, substituteInType(p.ty, substitutions), p.implicitness, p.default.map(substituteInType(_, substitutions)))
          ),
          tel.implicitness
        )
      }
      val boundIds = id +: telescopes.flatMap(_.params.map(_.id))
      val filteredSubs = substitutions.filterNot { case (id, _) => boundIds.contains(id) }
      AST.Def(
        id,
        name,
        newTelescopes,
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
    constraint match
      case c: ElabConstraint.Check       => handleCheck(c)
      case c: ElabConstraint.Infer       => handleInfer(c)
      case c: ElabConstraint.Unify       => handleUnify(c)
      case c: ElabConstraint.Subtype     => handleSubtype(c)
      case c: ElabConstraint.IsUniverse  => handleIsUniverse(c)
      case c: ElabConstraint.IsPi        => handleIsPi(c)
      case c: ElabConstraint.AssembleApp => handleAssembleApp(c)
      case c: ElabConstraint.AssembleAnn => handleAssembleAnn(c)
      case c: ElabConstraint.AssembleDef => handleAssembleDef(c)

  def canDefaulting(level: DefaultingLevel): Boolean = true
  override def defaulting[M <: SolverModule](constraint: ElabConstraint, level: DefaultingLevel)(using module: M, solver: module.Solver[ElabConstraint]): Boolean =
    level match
      case DefaultingLevel.Lit =>
        constraint match
          case c: ElabConstraint.Infer =>
            c.cst match
              case CST.IntegerLiteral(value, span) =>
                var changed = false
                if !module.hasStableValue(solver, c.result) then
                  module.fill(solver, c.result, AST.IntLit(value, span))
                  changed = true
                if !module.hasStableValue(solver, c.inferredTy) then
                  module.fill(solver, c.inferredTy, AST.IntegerType(None))
                  changed = true
                changed
              case _ => false
          case _ => false
      case _ => false

  private def handleCheck[M <: SolverModule](c: ElabConstraint.Check)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    module.readStable(solver, c.expectedTy) match
      case Some(AST.NaturalType(_)) =>
        c.cst match
          case CST.IntegerLiteral(value, span) if value.sign >= 0 =>
            module.fill(solver, c.result, AST.IntLit(value, span))
            // No need to fill expectedTy (read-only); rely on unification to respect Natural
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

  private def handleInfer[M <: SolverModule](c: ElabConstraint.Infer)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    // Check if we've already filled the result and type - if so, we're done (avoid re-processing)
    if module.hasStableValue(solver, c.result) && module.hasStableValue(solver, c.inferredTy) then return Result.Done

    c.cst match
      // Integer literal: type is Type(0)
      case CST.IntegerLiteral(value, span) =>
        val ast = AST.IntLit(value, span)
        module.fill(solver, c.result, ast)
        // Default to Integer, but allow unification to refine (e.g., Natural) via expected type
        module.fill(solver, c.inferredTy, if value.sign >= 0 then AST.IntegerType(None) else AST.IntegerType(None))
        Result.Done

      // String literal: type is the built-in String type
      case CST.StringLiteral(value, span) =>
        val ast = AST.StringLit(value, span)
        module.fill(solver, c.result, ast)
        module.fill(solver, c.inferredTy, AST.StringType(None))
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
            // Check if it's a builtin type
            if name == "Any" then
              val ast = AST.AnyType(None)
              module.fill(solver, c.result, ast)
              // Any has type Type(1)
              module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(1, None), None))
              Result.Done
            else if name == "String" then
              val ast = AST.StringType(None)
              module.fill(solver, c.result, ast)
              // String has type Type(0)
              module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
              Result.Done
            else if name == "Natural" then
              val ast = AST.NaturalType(None)
              module.fill(solver, c.result, ast)
              module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
              Result.Done
            else if name == "Integer" then
              val ast = AST.IntegerType(None)
              module.fill(solver, c.result, ast)
              module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
              Result.Done
            else if name == "Type" then
              // Type is the universe at level 0; its type quantifies over a natural level and yields Typeω(0)
              val ast = AST.Type(AST.IntLit(0, None), span)
              module.fill(solver, c.result, ast)

              val levelParamId = Uniqid.make[AST]
              val levelParamTy = AST.Type(AST.IntLit(0, None), None)
              val levelParam = Param(levelParamId, "n", levelParamTy, Implicitness.Explicit, None)
              val levelTele = Vector(Telescope(Vector(levelParam), Implicitness.Explicit))
              // Treat the codomain as a higher "omega" universe approximation: Typeω(0)
              val typeOfType = AST.Pi(
                levelTele,
                AST.TypeOmega(AST.IntLit(0, None), None),
                span
              )
              module.fill(solver, c.inferredTy, typeOfType)
              Result.Done
            else if name == "List" then
              val elemParamId = Uniqid.make[AST]
              val paramTy = AST.Type(AST.IntLit(0, None), None)
              val param = Param(elemParamId, "A", paramTy, Implicitness.Explicit, None)
              val lamTelescopes = Vector(Telescope(Vector(param), Implicitness.Explicit))
              val body = AST.ListType(AST.Ref(elemParamId, "A", span), span)
              val lam = AST.Lam(lamTelescopes, body, span)
              module.fill(solver, c.result, lam)

              val typeParamId = Uniqid.make[AST]
              val typeParam = Param(typeParamId, "A", paramTy, Implicitness.Explicit, None)
              val typeTele = Vector(Telescope(Vector(typeParam), Implicitness.Explicit))
              val lamType = AST.Pi(typeTele, AST.Type(AST.IntLit(1, None), None), span)
              module.fill(solver, c.inferredTy, lamType)
              Result.Done
            else if c.ctx.isBuiltin(name) then
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
                  module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
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

      // Tuple: infer each element (lazily zonked via MetaCells)
      case CST.Tuple(elements, span) =>
        val elemResults = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))
        val elemTypes = elements.map(_ => module.newOnceCell[ElabConstraint, AST](solver))

        elements.zip(elemResults).zip(elemTypes).foreach { case ((cstElem, resultCell), tyCell) =>
          module.addConstraint(solver, ElabConstraint.Infer(cstElem, resultCell, tyCell, c.ctx))
        }

        val tupleElems = elemResults.zip(elements).map { case (cell, elemCst) =>
          AST.MetaCell(HoldNotReadable(cell), elemCst.span)
        }
        module.fill(solver, c.result, AST.Tuple(tupleElems, span))

        val tupleTypes = elemTypes.zip(elements).map { case (cell, elemCst) =>
          AST.MetaCell(HoldNotReadable(cell), elemCst.span)
        }
        module.fill(solver, c.inferredTy, AST.Tuple(tupleTypes, None))
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
        else
          val elemMeta = module.newOnceCell[ElabConstraint, AST](solver)
          module.fill(solver, c.inferredTy, AST.ListType(AST.MetaCell(HoldNotReadable(elemMeta), span), span))
        Result.Done

      // Block: infer each element with two-pass elaboration for defs
      case CST.Block(elements, tail, span) =>
        // Check if tail contains a def/let statement (which is not allowed)
        tail.foreach { t =>
          if isDefStatement(t) then c.ctx.reporter.report(ElabProblem.UnboundVariable("def statement not allowed in tail position", t.span))
          if isLetStatement(t) then c.ctx.reporter.report(ElabProblem.UnboundVariable("let statement not allowed in tail position", t.span))
        }

        // TWO-PASS ELABORATION for forward references:
        // Pass 1: Scan elements, collect all def declarations, create placeholder cells
        var enrichedCtx = c.ctx
        val defInfoMap =
          scala.collection.mutable.Map.empty[CST, (String, UniqidOf[AST], module.OnceCell[AST], CellRW[AST])]

        elements.foreach {
          case elem @ CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
            // Extract def name
            val elems = seqElems.toVector
            elems match
              case _ +: CST.Symbol(name, _) +: _ =>
                // Create unique ID and type cell for this def
                val defId = Uniqid.make[AST]
                val defTypeCell = module.newOnceCell[ElabConstraint, AST](solver)
                val defResultCell = module.newOnceCell[ElabConstraint, AST](solver)
                // Add to context so it can be referenced (including by other defs)
                enrichedCtx = enrichedCtx.bind(name, defId, defTypeCell).registerDefBody(defId, defResultCell)
                defInfoMap(elem) = (name, defId, defTypeCell, defResultCell)
              case _ => ()
          case _ => ()
        }

        // Pass 2: Elaborate all elements with enriched context and sequential lets
        val elaboratedElemsBuffer = scala.collection.mutable.ArrayBuffer.empty[AST]
        var currentCtx = enrichedCtx

        elements.foreach { elem =>
          val elemResult =
            defInfoMap.get(elem) match
              case Some((_, _, _, defCell)) => defCell
              case None                     => module.newOnceCell[ElabConstraint, AST](solver)
          val elemType = module.newOnceCell[ElabConstraint, AST](solver)

          elem match
            case CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
              val elems = seqElems.toVector
              val (_, defId, defTypeCell, _) = defInfoMap(elem)
              val defConstraint: ElabConstraint.Infer = ElabConstraint.Infer(elem, elemResult, elemType, currentCtx)
              handleDefStatement(defConstraint, elems, elem.span, defId, defTypeCell, currentCtx)(using module, solver)
            case CST.SeqOf(seqElems, _) if seqElems.headOption.exists { case CST.Symbol("let", _) => true; case _ => false } =>
              val elems = seqElems.toVector
              currentCtx = handleLetStatement(c, elems, elem.span, currentCtx, elemResult, elemType)(using module, solver)
            case _ =>
              module.addConstraint(solver, ElabConstraint.Infer(elem, elemResult, elemType, currentCtx))

          elaboratedElemsBuffer += AST.MetaCell(HoldNotReadable(elemResult), elem.span)
        }

        val elaboratedElems = elaboratedElemsBuffer.toVector

        // Elaborate tail (or use empty tuple if None)
        val elaboratedTail = tail match
          case Some(t) =>
            val tailResult = module.newOnceCell[ElabConstraint, AST](solver)
            // Type of block is type of tail - use c.inferredTy directly
            module.addConstraint(solver, ElabConstraint.Infer(t, tailResult, c.inferredTy, currentCtx))
            AST.MetaCell(HoldNotReadable(tailResult), t.span)
          case None =>
            // Empty tail = unit type
            val emptyTuple = AST.Tuple(Vector.empty, span)
            module.fill(solver, c.inferredTy, AST.Tuple(Vector.empty, None))
            emptyTuple

        // Construct block directly with MetaCells - they'll be resolved by substituteSolutions
        val block = AST.Block(elaboratedElems, elaboratedTail, span)
        module.fill(solver, c.result, block)
        Result.Done

      // SeqOf: could be function application or sequence (but NOT def - def only allowed in Block elements)
      case CST.SeqOf(elements, span) =>
        val elems = elements.toVector

        // Report error if this looks like a def/let statement (only allowed in Block elements)
        if elems.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } then
          c.ctx.reporter.report(ElabProblem.UnboundVariable("def statement only allowed in block elements, not at top level or in expressions", span))
          // Error recovery: treat as unbound variable reference to "def"
          val metaId = Uniqid.make[AST]
          val ast = AST.Ref(metaId, "def", span)
          module.fill(solver, c.result, ast)
          val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
          module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
          Result.Done
        else if elems.headOption.exists { case CST.Symbol("let", _) => true; case _ => false } then
          c.ctx.reporter.report(ElabProblem.UnboundVariable("let statement only allowed in block elements, not at top level or in expressions", span))
          val metaId = Uniqid.make[AST]
          val ast = AST.Ref(metaId, "let", span)
          module.fill(solver, c.result, ast)
          val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
          module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
          Result.Done
        else
          annotationPattern(elems) match
            case Some((exprCst, typeCst)) =>
              handleAnnotatedExpression(c, exprCst, typeCst, span)
            case None =>
              // Normalize chained applications like f(a)(b) or f[a](b)(c)
              normalizeApplicationSeq(elems) match
                case Some(normalized) =>
                  module.addConstraint(solver, ElabConstraint.Infer(normalized, c.result, c.inferredTy, c.ctx))
                  Result.Done
                case None =>
                  // Check for function application patterns:
                  // f(args) becomes SeqOf(f, Tuple(args))
                  // f[typeArgs](args) becomes SeqOf(f, ListLiteral(typeArgs), Tuple(args))
                  if elems.length == 2 && elems(1).isInstanceOf[CST.Tuple] then
                    // f(args) - no explicit type arguments
                    handleFunctionApplication(c, elems(0), None, elems(1).asInstanceOf[CST.Tuple], span)
                  else if elems.length == 3 && elems(1).isInstanceOf[CST.ListLiteral] && elems(2).isInstanceOf[CST.Tuple] then
                    // f[typeArgs](args) - explicit type arguments provided
                    handleFunctionApplication(c, elems(0), Some(elems(1).asInstanceOf[CST.ListLiteral]), elems(2).asInstanceOf[CST.Tuple], span)
                  else
                    // Default: treat as a block-like sequence
                    val blockCst = CST.Block(elems.dropRight(1), Some(elems.last), span)
                    module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
                    Result.Done

  /** Handle function application: f(args) or f[typeArgs](args) */
  private def handleFunctionApplication[M <: SolverModule](
      c: ElabConstraint.Infer,
      funcCst: CST,
      explicitTypeArgs: Option[CST.ListLiteral],
      argsTuple: CST.Tuple,
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result =

    // Elaborate function
    val funcResult = module.newOnceCell[ElabConstraint, AST](solver)
    val funcTy = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(funcCst, funcResult, funcTy, c.ctx))

    // Elaborate explicit type arguments if provided
    val typeArgPairs = explicitTypeArgs
      .map { typeArgsList =>
        typeArgsList.elements.map { typeArg =>
          val typeArgResult = module.newOnceCell[ElabConstraint, AST](solver)
          val typeArgTy = module.newOnceCell[ElabConstraint, AST](solver)
          module.addConstraint(solver, ElabConstraint.Infer(typeArg, typeArgResult, typeArgTy, c.ctx))
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
      module.addConstraint(solver, ElabConstraint.Infer(arg, argResult, argTy, c.ctx))
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
        c.result,
        c.inferredTy,
        span,
        c.ctx
      )
    )

    Result.Done

  private def annotationPattern(elems: Vector[CST]): Option[(CST, CST)] =
    if elems.length >= 3 then
      val colonIndex = elems.lastIndexWhere {
        case CST.Symbol(":", _) => true
        case _                  => false
      }
      if colonIndex >= 1 && colonIndex < elems.length - 1 then
        val exprElems = elems.take(colonIndex)
        val typeElems = elems.drop(colonIndex + 1)
        val expr =
          if exprElems.length == 1 then exprElems.head
          else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(exprElems), combinedSpan(exprElems))
        val ty =
          if typeElems.length == 1 then typeElems.head
          else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(typeElems), combinedSpan(typeElems))
        Some((expr, ty))
      else None
    else None

  private def handleAnnotatedExpression[M <: SolverModule](
      c: ElabConstraint.Infer,
      exprCst: CST,
      typeCst: CST,
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result =

    val annotationTy = module.newOnceCell[ElabConstraint, AST](solver)
    val annotationTyTy = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(typeCst, annotationTy, annotationTyTy, c.ctx))

    val exprResult = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Check(exprCst, annotationTy, exprResult, c.ctx))

    module.addConstraint(
      solver,
      ElabConstraint.AssembleAnn(
        exprResult,
        annotationTy,
        c.result,
        c.inferredTy,
        span
      )
    )

    Result.Done

  /** Convert chained SeqOf expressions like f(a)(b) into nested SeqOf nodes we already know how to elaborate. */
  private def normalizeApplicationSeq(elems: Vector[CST]): Option[CST] =
    if elems.length <= 1 then None
    else if elems.length == 2 && elems(1).isInstanceOf[CST.Tuple] then None
    else if elems.length == 3 && elems(1).isInstanceOf[CST.ListLiteral] && elems(2).isInstanceOf[CST.Tuple] then None
    else
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

  private def buildSeqOf(func: CST, typeArgs: Option[CST.ListLiteral], tuple: CST.Tuple): CST =
    val seqElems = typeArgs match
      case Some(list) => Vector(func, list, tuple)
      case None       => Vector(func, tuple)
    CST.SeqOf(NonEmptyVector.fromVectorUnsafe(seqElems), combinedSpan(seqElems))

  private def combinedSpan(elems: Seq[CST]): Option[Span] =
    val spans = elems.iterator.flatMap(_.span)
    if !spans.hasNext then None
    else
      val first = spans.next()
      var last = first
      while spans.hasNext do last = spans.next()
      Some(first.combine(last))

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
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    // Parse: def name [telescope]* (telescope)* = body
    // or:    def name [telescope]* (telescope)* : type = body
    if elems.length < 4 then
      ctx.reporter.report(ElabProblem.UnboundVariable("Invalid def syntax", span))
      module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
      module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
      module.fill(solver, defTypeCell, AST.Type(AST.IntLit(0, None), None))
      return Result.Done

    val name = elems(1) match
      case CST.Symbol(n, _) => n
      case _ =>
        ctx.reporter.report(ElabProblem.UnboundVariable("Expected name after def", span))
        "<error>"

    // Collect telescopes (lists for implicit, tuples for explicit)
    // IMPORTANT: We need to accumulate context as we go, so later telescopes can reference earlier parameters
    var idx = 2
    val telescopes = scala.collection.mutable.ArrayBuffer.empty[Telescope]
    var accumulatedCtx = ctx

    while idx < elems.length && (elems(idx).isInstanceOf[CST.ListLiteral] || elems(idx).isInstanceOf[CST.Tuple]) do
      elems(idx) match
        case CST.ListLiteral(params, _) =>
          val telescope = parseTelescopeFromCST(params, Implicitness.Implicit, accumulatedCtx)(using module, solver)
          telescopes += telescope
          // Update context with parameters from this telescope
          telescope.params.foreach { param =>
            val paramTyCell = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, paramTyCell, param.ty)
            accumulatedCtx = accumulatedCtx.bind(param.name, param.id, paramTyCell)
          }
          idx += 1
        case CST.Tuple(params, _) =>
          val telescope = parseTelescopeFromCST(params, Implicitness.Explicit, accumulatedCtx)(using module, solver)
          telescopes += telescope
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
    if idx < elems.length && elems(idx).isInstanceOf[CST.Symbol] && elems(idx).asInstanceOf[CST.Symbol].name == ":" then
      idx += 1
      if idx < elems.length then
        val resultTyTyCell = module.newOnceCell[ElabConstraint, AST](solver)
        module.addConstraint(solver, ElabConstraint.Infer(elems(idx), resultTyCell, resultTyTyCell, ctx))
        hasResultTy = true
        idx += 1

    // Expect =
    if idx >= elems.length || !elems(idx).isInstanceOf[CST.Symbol] || elems(idx).asInstanceOf[CST.Symbol].name != "=" then
      ctx.reporter.report(ElabProblem.UnboundVariable("Expected = in def", span))
      module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
      module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
      module.fill(solver, defTypeCell, AST.Type(AST.IntLit(0, None), None))
      return Result.Done

    idx += 1

    // Body is remaining elements
    val bodyCst =
      if idx < elems.length then
        if elems.length - idx == 1 then elems(idx)
        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(elems.drop(idx)), span)
      else
        ctx.reporter.report(ElabProblem.UnboundVariable("Expected body in def", span))
        CST.Symbol("<error>", span)

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
        defTypeCell,
        span
      )
    )

    Result.Done

  private def handleLetStatement[M <: SolverModule](
      c: ElabConstraint.Infer,
      elems: Vector[CST],
      span: Option[Span],
      ctx: ElabContext,
      resultCell: CellRW[AST],
      elemTypeCell: CellRW[AST]
  )(using module: M, solver: module.Solver[ElabConstraint]): ElabContext =
    import module.given

    def fail(message: String): ElabContext =
      ctx.reporter.report(ElabProblem.UnboundVariable(message, span))
      module.fill(solver, resultCell, AST.Ref(Uniqid.make, "<error>", span))
      module.fill(solver, elemTypeCell, AST.Type(AST.IntLit(0, None), None))
      ctx

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

    if idx < elems.length && elems(idx).isInstanceOf[CST.Symbol] && elems(idx).asInstanceOf[CST.Symbol].name == ":" then
      hasAnnotation = true
      idx += 1
      if idx >= elems.length then return fail("Expected type annotation after colon in let")
      val annotationCst = elems(idx)
      annotationSpan = annotationCst.span
      val annotationTyTy = module.newOnceCell[ElabConstraint, AST](solver)
      module.addConstraint(solver, ElabConstraint.Infer(annotationCst, letTypeCell, annotationTyTy, ctx))
      idx += 1

    if idx >= elems.length || !elems(idx).isInstanceOf[CST.Symbol] || elems(idx).asInstanceOf[CST.Symbol].name != "=" then
      return fail("Expected = in let")

    idx += 1

    val valueCst =
      if idx < elems.length then
        if elems.length - idx == 1 then elems(idx)
        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(elems.drop(idx)), span)
      else
        ctx.reporter.report(ElabProblem.UnboundVariable("Expected value in let", span))
        CST.Symbol("<error>", span)

    val valueResult = module.newOnceCell[ElabConstraint, AST](solver)

    if hasAnnotation then module.addConstraint(solver, ElabConstraint.Check(valueCst, letTypeCell, valueResult, ctx))
    else
      val valueTyCell = module.newOnceCell[ElabConstraint, AST](solver)
      module.addConstraint(solver, ElabConstraint.Infer(valueCst, valueResult, valueTyCell, ctx))
      module.fill(solver, letTypeCell, AST.MetaCell(HoldNotReadable(valueTyCell), valueCst.span))

    val valueAst = AST.MetaCell(HoldNotReadable(valueResult), valueCst.span)
    val tyAstOpt =
      if hasAnnotation then Some(AST.MetaCell(HoldNotReadable(letTypeCell), annotationSpan))
      else None
    val bodyAst = AST.Ref(letId, name, valueCst.span.orElse(span))
    val letAst = AST.Let(letId, name, tyAstOpt, valueAst, bodyAst, span)

    module.fill(solver, resultCell, letAst)
    module.fill(solver, elemTypeCell, AST.MetaCell(HoldNotReadable(letTypeCell), span))

    ctx.bind(name, letId, letTypeCell)

  /** Parse a telescope from CST parameter list This creates a telescope with types as meta-cells that will be filled by constraints.
    *
    * IMPORTANT: For dependent types, this progressively extends the context as it processes each parameter, so later parameters can reference earlier
    * ones in their types. For example, in `def id[a: Type](x: a)`, the type of `x` references parameter `a`.
    */
  private def parseTelescopeFromCST[M <: SolverModule](
      params: Vector[CST],
      implicitness: Implicitness,
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): Telescope =

    var currentCtx = ctx

    val parsedParams = params.flatMap {
      // Pattern: SeqOf(name, :, type)
      case CST.SeqOf(elems, _) if elems.length == 3 =>
        val elemsVec = elems.toVector
        (elemsVec(0), elemsVec(1), elemsVec(2)) match
          case (CST.Symbol(name, _), CST.Symbol(":", _), typeCst) =>
            val paramId = Uniqid.make[AST]
            // Elaborate the type in the CURRENT context (which includes previous params)
            val tyResult = module.newOnceCell[ElabConstraint, AST](solver)
            val tyTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(typeCst, tyResult, tyTy, currentCtx))

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

  /** Check if a CST node is a def statement */
  private def isDefStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.headOption.exists { case CST.Symbol("def", _) => true; case _ => false }
    case _ => false

  private def isLetStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.headOption.exists { case CST.Symbol("let", _) => true; case _ => false }
    case _ => false

  /** Unification result following the paper's architecture */
  private enum UnifyResult:
    case Success
    case Failure(message: String)

  private def handleUnify[M <: SolverModule](c: ElabConstraint.Unify)(using module: M, solver: module.Solver[ElabConstraint]): Result =
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

  /** Reduce (normalize) a term by performing beta-reduction. Following the paper's recommendation (Section 7.5), we reduce before unification. This
    * handles lambda applications.
    */
  private def reduce[M <: SolverModule](
      term: AST,
      ctx: ElabContext,
      depth: Int = 0
  )(using module: M, solver: module.Solver[ElabConstraint]): AST =
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
            ctx.lookupDefBody(id) match
              case Some(defCell) =>
                module.readStable(solver, defCell) match
                  case Some(AST.Def(_, _, telescopes, _, body, defSpan)) =>
                    val lam = AST.Lam(telescopes, body, defSpan)
                    reduce(AST.App(lam, args, implicitArgs, span), ctx, depth + 1)
                  case _ => term
              case None => term
          case _ => term

      // For all other constructs, no reduction
      case _ => term

  /** Unify two types with occurs check and meta-variable solving. Following the paper's architecture, this acts as a specialized unification solver.
    * TODO: Enable reduction before unification (paper Section 7.5) after fixing infinite loops.
    */
  private def unify[M <: SolverModule](
      t1: AST,
      t2: AST,
      span: Option[Span],
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): UnifyResult =
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
            else
              module.fill(solver, cell1, ty2)
              UnifyResult.Success

      case (ty1, AST.MetaCell(HoldNotReadable(cell2), _)) =>
        module.readStable(solver, cell2) match
          case Some(solved2) => unify(ty1, solved2, span, ctx)
          case None          =>
            // Solve: ?β := ty1 with occurs check
            if occursIn(cell2, ty1)(using module, solver) then UnifyResult.Failure("Occurs check failed: infinite type")
            else
              module.fill(solver, cell2, ty1)
              UnifyResult.Success

      // Structural unification
      case (AST.IntLit(v1, _), AST.IntLit(v2, _)) =>
        if v1 == v2 then UnifyResult.Success else UnifyResult.Failure("Integer literals differ")

      case (AST.StringLit(v1, _), AST.StringLit(v2, _)) =>
        if v1 == v2 then UnifyResult.Success else UnifyResult.Failure("String literals differ")

      case (AST.Ref(id1, _, _), AST.Ref(id2, _, _)) =>
        if id1 == id2 then UnifyResult.Success else UnifyResult.Failure("Different variables")

      case (AST.Type(l1, _), AST.Type(l2, _)) => unify(l1, l2, span, ctx)

      case (AST.AnyType(_), AST.AnyType(_))           => UnifyResult.Success
      case (AST.StringType(_), AST.StringType(_))     => UnifyResult.Success
      case (AST.IntegerType(_), AST.IntegerType(_))   => UnifyResult.Success
      case (AST.ListType(e1, _), AST.ListType(e2, _)) => unify(e1, e2, span, ctx)

      case (AST.Tuple(e1, _), AST.Tuple(e2, _)) =>
        if e1.size != e2.size then UnifyResult.Failure("Tuple arity mismatch")
        else unifyAll(e1.zip(e2), span, ctx)

      case (AST.Pi(tel1, r1, _), AST.Pi(tel2, r2, _)) =>
        if tel1.size != tel2.size then UnifyResult.Failure("Function arity mismatch")
        else
          val paramPairs = tel1.zip(tel2).flatMap { case (t1, t2) =>
            t1.params.zip(t2.params).map((p1, p2) => (p1.ty, p2.ty))
          }
          unifyAll(paramPairs, span, ctx) match
            case UnifyResult.Success => unify(r1, r2, span, ctx)
            case failure             => failure

      case _ => UnifyResult.Failure(s"Type mismatch: ${t1.getClass.getSimpleName} vs ${t2.getClass.getSimpleName}")

  /** Unify a list of type pairs */
  private def unifyAll[M <: SolverModule](
      pairs: Vector[(AST, AST)],
      span: Option[Span],
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): UnifyResult =
    pairs.foldLeft(UnifyResult.Success: UnifyResult) { case (acc, (a, b)) =>
      acc match
        case UnifyResult.Success => unify(a, b, span, ctx)
        case failure             => failure
    }

  /** Handle subtyping constraint: ty1 <: ty2 */
  private def handleSubtype[M <: SolverModule](c: ElabConstraint.Subtype)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    (module.readStable(solver, c.ty1), module.readStable(solver, c.ty2)) match
      case (Some(t1), Some(t2)) =>
        if isSubtype(t1, t2, c.span, c.ctx)(using module, solver) then Result.Done
        else
          c.ctx.reporter.report(ElabProblem.TypeMismatch(t1, t2, c.span))
          Result.Done
      case (None, _) => Result.Waiting(c.ty1)
      case (_, None) => Result.Waiting(c.ty2)

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
  )(using module: M, solver: module.Solver[ElabConstraint]): Boolean =

    val normTy1 = reduce(ty1, ctx)
    val normTy2 = reduce(ty2, ctx)

    // Everything is a subtype of Any
    if normTy2.isInstanceOf[AST.AnyType] then return true

    // Any is only a subtype of itself
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
          case (AST.Pi(tel1, r1, _), AST.Pi(tel2, r2, _)) =>
            if tel1.size != tel2.size then false
            else
              // Parameters are contravariant
              val paramsOk = tel1.zip(tel2).forall { case (t1, t2) =>
                if t1.params.size != t2.params.size then false
                else
                  t1.params.zip(t2.params).forall { case (p1, p2) =>
                    // p2.ty <: p1.ty (contravariant!)
                    isSubtype(p2.ty, p1.ty, span, ctx)
                  }
              }
              // Result is covariant
              paramsOk && isSubtype(r1, r2, span, ctx)

          // Tuple subtyping: covariant in all components
          case (AST.Tuple(e1, _), AST.Tuple(e2, _)) =>
            e1.size == e2.size && e1.zip(e2).forall { case (a, b) =>
              isSubtype(a, b, span, ctx)
            }

          // List subtyping: covariant
          case (AST.ListType(elem1, _), AST.ListType(elem2, _)) =>
            isSubtype(elem1, elem2, span, ctx)

          case _ => false

  /** Occurs check: does a meta-variable cell occur in a type? */
  private def occursIn[M <: SolverModule](
      cell: Any, // The cell we're checking for (stored in HoldNotReadable)
      ty: AST
  )(using module: M, solver: module.Solver[ElabConstraint]): Boolean =
    ty match
      case AST.MetaCell(HoldNotReadable(c), _) =>
        if c == cell then true
        else module.readStable(solver, c).exists(occursIn(cell, _))
      case AST.Ref(_, _, _) | AST.StringLit(_, _) | AST.IntLit(_, _) | AST.AnyType(_) | AST.StringType(_) | AST.IntegerType(_) =>
        false
      case AST.NaturalType(_)          => false
      case AST.ListType(element, _)     => occursIn(cell, element)
      case AST.Type(level, _)           => occursIn(cell, level)
      case AST.TypeOmega(level, _)      => occursIn(cell, level)
      case AST.Tuple(elements, _)       => elements.exists(occursIn(cell, _))
      case AST.ListLit(elements, _)     => elements.exists(occursIn(cell, _))
      case AST.Block(elements, tail, _) => elements.exists(occursIn(cell, _)) || occursIn(cell, tail)
      case AST.Pi(telescopes, resultTy, _) =>
        telescopes.exists(t => t.params.exists(p => occursIn(cell, p.ty))) || occursIn(cell, resultTy)
      case AST.Lam(telescopes, body, _) =>
        telescopes.exists(t => t.params.exists(p => occursIn(cell, p.ty))) || occursIn(cell, body)
      case AST.App(func, args, _, _) =>
        occursIn(cell, func) || args.exists(a => occursIn(cell, a.value))
      case AST.Let(_, _, ty, value, body, _) =>
        ty.exists(occursIn(cell, _)) || occursIn(cell, value) || occursIn(cell, body)
      case AST.Def(_, _, telescopes, resultTy, body, _) =>
        telescopes.exists(t => t.params.exists(p => occursIn(cell, p.ty))) ||
        resultTy.exists(occursIn(cell, _)) || occursIn(cell, body)
      case AST.Ann(expr, ty, _) => occursIn(cell, expr) || occursIn(cell, ty)

  private def handleIsUniverse[M <: SolverModule](c: ElabConstraint.IsUniverse)(using module: M, solver: module.Solver[ElabConstraint]): Result =
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
      case Some(AST.Pi(telescopes, resultTy, _)) =>
        // Resolve MetaCells in telescopes (parameter types may contain unresolved cells)
        val resolvedTelescopes = telescopes.map(tel => tel.copy(params = tel.params.map(param => param.copy(ty = substituteSolutions(param.ty)))))
        val resolvedResultTy = substituteSolutions(resultTy)

        // Separate implicit and explicit parameters
        val implicitParams = resolvedTelescopes.filter(_.implicitness == Implicitness.Implicit).flatMap(_.params)
        val explicitParams = resolvedTelescopes.filter(_.implicitness == Implicitness.Explicit).flatMap(_.params)

        // Build arguments: use provided explicit type args for implicit params, create metas for rest
        val implicitArgs = if explicitTypeArgs.nonEmpty then
          // User provided explicit type arguments like id[String]
          if explicitTypeArgs.size != implicitParams.size then
            c.ctx.reporter.report(ElabProblem.NotAFunction(func, c.span))
            module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
            module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
            return Result.Done

          // Type check explicit type arguments against implicit parameter types synchronously
          val typeArgsOk = implicitParams.zip(explicitTypeArgs).zip(c.explicitTypeArgTypes).forall { case ((param, _), argTyCell) =>
            module.readStable(solver, argTyCell) match
              case Some(actualTy) =>
                unify(param.ty, actualTy, c.span, c.ctx) match
                  case UnifyResult.Success => true
                  case UnifyResult.Failure(_) =>
                    c.ctx.reporter.report(ElabProblem.TypeMismatch(param.ty, actualTy, c.span))
                    false
              case None => true // Not stable yet
          }

          if !typeArgsOk then
            module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
            module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
            return Result.Done

          explicitTypeArgs
        else
          // No explicit type arguments - create meta-variables for implicit parameters
          // These will be solved through unification when we check explicit argument types
          implicitParams.map { _ =>
            val metaCell = module.newOnceCell[ElabConstraint, AST](solver)
            AST.MetaCell(HoldNotReadable(metaCell), c.span)
          }

        // Check explicit argument arity
        if explicitArgs.size != explicitParams.size then
          c.ctx.reporter.report(ElabProblem.NotAFunction(func, c.span))
          module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
          module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
          return Result.Done

        // Type check arguments synchronously (not via constraints to avoid loops)
        // This may fill the implicit arg MetaCells through unification
        val implicitSubst = implicitParams.map(_.id).zip(implicitArgs).toMap
        val allTypeChecksPassed = explicitParams.zip(c.argTypes).forall { case (param, argTyCell) =>
          val expectedParamTy = substituteInType(param.ty, implicitSubst)
          module.readStable(solver, argTyCell) match
            case Some(actualArgTy) =>
              // Try unification first (for implicit argument inference), then subtyping
              unify(expectedParamTy, actualArgTy, c.span, c.ctx) match
                case UnifyResult.Success    => true
                case UnifyResult.Failure(_) =>
                  // Unification failed, try subtyping: actualArgTy <: expectedParamTy
                  if isSubtype(actualArgTy, expectedParamTy, c.span, c.ctx) then true
                  else
                    c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedParamTy, actualArgTy, c.span))
                    false
            case None => true // Not stable yet, will be checked later
        }

        if !allTypeChecksPassed then
          module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
          module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
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
        else
          // Only explicit: func(explicitArgs)
          AST.App(func, explicitArgs.map(Arg(_, Implicitness.Explicit)), implicitArgs = false, c.span)

        module.fill(solver, c.result, app)

        // Perform substitution: replace parameter references in result type with resolved arguments
        val allParams = implicitParams ++ explicitParams
        val allArgs = resolvedImplicitArgs ++ explicitArgs
        val substitutedTy = substituteInType(resolvedResultTy, allParams.map(_.id).zip(allArgs).toMap)
        val normalizedTy = reduce(substitutedTy, c.ctx)
        module.fill(solver, c.inferredTy, normalizedTy)
        Result.Done
      case Some(ty) =>
        c.ctx.reporter.report(ElabProblem.NotAFunction(ty, c.span))
        module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
        module.fill(solver, c.inferredTy, AST.Type(AST.IntLit(0, None), None))
        Result.Done
      case None =>
        Result.Waiting(c.funcTy)

  private def handleAssembleAnn[M <: SolverModule](c: ElabConstraint.AssembleAnn)(using module: M, solver: module.Solver[ElabConstraint]): Result =
    import module.given

    if module.hasStableValue(solver, c.result) && module.hasStableValue(solver, c.inferredTy) then return Result.Done

    if !module.hasStableValue(solver, c.exprResult) then return Result.Waiting(c.exprResult)
    if !module.hasStableValue(solver, c.annotationTy) then return Result.Waiting(c.annotationTy)

    val exprAst = module.readStable(solver, c.exprResult).get
    val tyAst = module.readStable(solver, c.annotationTy).get

    module.fill(solver, c.result, AST.Ann(exprAst, tyAst, c.span))
    module.fill(solver, c.inferredTy, tyAst)

    Result.Done

  private def handleAssembleDef[M <: SolverModule](c: ElabConstraint.AssembleDef)(using module: M, solver: module.Solver[ElabConstraint]): Result =
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

    // Build def AST
    val defAst = AST.Def(c.defId, c.name, c.telescopes, resultTy, body, c.span)
    module.fill(solver, c.result, defAst)

    // Compute def type (Pi type)
    val finalResultTy = resultTy match
      case Some(rt) => rt
      case None     =>
        // Infer from body type
        if !module.hasStableValue(solver, c.bodyTy) then return Result.Waiting(c.bodyTy)
        module.readStable(solver, c.bodyTy).get

    val piTy = AST.Pi(c.telescopes, finalResultTy, c.span)
    module.fill(solver, c.inferredTy, piTy)
    module.fill(solver, c.defTypeCell, piTy)

    Result.Done

/** Handler configuration for elaboration */
class ElabHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
  private val handler = new ElabHandler()

  def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] =
    Some(handler)

/** Substitute meta-cell solutions throughout an AST This resolves MetaCell nodes by reading their cell contents after constraint solving Also known
  * as "zonking" in some type checkers
  */
def substituteSolutions[M <: SolverModule](ast: AST)(using module: M, solver: module.Solver[ElabConstraint]): AST =

  ast match
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

    case AST.ListLit(elements, span) =>
      AST.ListLit(elements.map(substituteSolutions), span)

    case AST.Block(elements, tail, span) =>
      AST.Block(elements.map(substituteSolutions), substituteSolutions(tail), span)

    case AST.StringLit(value, span) => ast
    case AST.IntLit(value, span)    => ast

    case AST.Type(level, span) =>
      AST.Type(substituteSolutions(level), span)
    case AST.TypeOmega(level, span) =>
      AST.TypeOmega(substituteSolutions(level), span)

    case AST.AnyType(span)      => ast
    case AST.StringType(span)   => ast
    case AST.IntegerType(span)  => ast
    case AST.NaturalType(span)  => ast
    case AST.ListType(element, span) =>
      AST.ListType(substituteSolutions(element), span)

    case AST.Pi(telescopes, resultTy, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => Param(p.id, p.name, substituteSolutions(p.ty), p.implicitness, p.default.map(substituteSolutions))),
          tel.implicitness
        )
      }
      AST.Pi(newTelescopes, substituteSolutions(resultTy), span)

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

      normalizedFunc match
        case AST.Lam(Vector(telescope), body @ AST.ListType(listElem, bodySpan), _) if !implicitArgs =>
          val params = telescope.params
          if params.length == 1 && normalizedArgs.length == 1 then
            val param = params.head
            listElem match
              case AST.Ref(id, _, _) if id == param.id =>
                return AST.ListType(normalizedArgs.head.value, span.orElse(bodySpan))
              case _ => ()
          AST.App(normalizedFunc, normalizedArgs, implicitArgs, span)
        case _ =>
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

    case AST.Def(id, name, telescopes, resultTy, body, span) =>
      val newTelescopes = telescopes.map { tel =>
        Telescope(
          tel.params.map(p => Param(p.id, p.name, substituteSolutions(p.ty), p.implicitness, p.default.map(substituteSolutions))),
          tel.implicitness
        )
      }
      AST.Def(
        id,
        name,
        newTelescopes,
        resultTy.map(substituteSolutions),
        substituteSolutions(body),
        span
      )

    case AST.Ann(expr, ty, span) =>
      AST.Ann(substituteSolutions(expr), substituteSolutions(ty), span)

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

  /** Elaborate with default ProceduralSolver and new VectorReporter */
  def elaborate(cst: CST)(using Reporter[ElabProblem]): (AST, AST) =
    elaborate(cst, summon[Reporter[ElabProblem]], None)(using ProceduralSolverModule)

  /** Elaborate with custom context */
  def elaborate(cst: CST, ctx: ElabContext): (AST, AST) =
    elaborate(cst, ctx.reporter, Some(ctx))(using ProceduralSolverModule)
