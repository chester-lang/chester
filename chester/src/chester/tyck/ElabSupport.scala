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
private object ElabSupport:
  def combinedSpan(elems: Seq[CST]): Option[Span] = {
    val spans = elems.iterator.flatMap(_.span)
    if !spans.hasNext then None
    else {
      val first = spans.next()
      var last = first
      while spans.hasNext do last = spans.next()
      Some(first.combine(last))
    }
  }
  def spanOf(elems: Seq[CST]): Option[Span] = combinedSpan(elems)
  def parseEnumParams[M <: SolverModule](elems: Vector[CST], ctx: ElabContext)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[Param] = parseEnumTypeParams(elems, ctx)
  def parseRecordParams[M <: SolverModule](elems: Vector[CST], ctx: ElabContext)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[Param] = parseRecordFields(elems, ctx)
  def parseEnumCasesF[M <: SolverModule](elems: Vector[CST], ctx: ElabContext, typeParams: Vector[Param])(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[EnumCase] = parseEnumCases(elems, ctx, typeParams)
  def handleDef[M <: SolverModule](
      c: ElabConstraint.Infer,
      elems: Vector[CST],
      span: Option[Span],
      defId: UniqidOf[AST],
      defTypeCell: CellRW[AST],
      ctx: ElabContext,
      defInfoMap: scala.collection.mutable.Map[CST, DefInfo]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result =
    handleDefStatement(c, elems, span, defId, defTypeCell, ctx, defInfoMap)
  def processEffect[M <: SolverModule](body: CST.Block, effectRef: EffectRef, ctx: ElabContext)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): (ElabContext, Vector[Param]) = processEffectBody(body, effectRef, ctx)
  def handleLet[M <: SolverModule](
      elems: Vector[CST],
      span: Option[Span],
      ctx: ElabContext,
      resultCell: CellRW[AST],
      inferredTyCell: CellRW[AST]
  )(using module: M, solver: module.Solver[ElabConstraint]): ElabContext =
    handleLetStatement(elems, span, ctx, resultCell, inferredTyCell)
  def normalizeKindish(ty: AST): AST = normalizeTypeLikeKind(ty)
  def substitute(ast: AST)(using module: SolverModule, solver: module.Solver[ElabConstraint]): AST =
    substituteSolutions(ast)

private given blockHelpers: ElaboratorBlocks.Helpers = new ElaboratorBlocks.Helpers {
  override def combinedSpan(elems: Seq[CST]): Option[Span] = ElabSupport.spanOf(elems)
  override def parseEnumTypeParams[M <: SolverModule](elems: Vector[CST], ctx: ElabContext)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[Param] = ElabSupport.parseEnumParams(elems, ctx)
  override def parseRecordFields[M <: SolverModule](elems: Vector[CST], ctx: ElabContext)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[Param] = ElabSupport.parseRecordParams(elems, ctx)
  override def parseEnumCases[M <: SolverModule](elems: Vector[CST], ctx: ElabContext, typeParams: Vector[Param])(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): Vector[EnumCase] = ElabSupport.parseEnumCasesF(elems, ctx, typeParams)
  override def handleDefStatement[M <: SolverModule](
      c: ElabConstraint.Infer,
      elems: Vector[CST],
      span: Option[Span],
      defId: UniqidOf[AST],
      defTypeCell: CellRW[AST],
      ctx: ElabContext,
      defInfoMap: scala.collection.mutable.Map[CST, DefInfo]
  )(using module: M, solver: module.Solver[ElabConstraint]): Result =
    ElabSupport.handleDef(c, elems, span, defId, defTypeCell, ctx, defInfoMap)

  override def processEffectBody[M <: SolverModule](body: CST.Block, effectRef: EffectRef, ctx: ElabContext)(using
      module: M,
      solver: module.Solver[ElabConstraint]
  ): (ElabContext, Vector[Param]) = ElabSupport.processEffect(body, effectRef, ctx)

  override def handleLetStatement[M <: SolverModule](
      elems: Vector[CST],
      span: Option[Span],
      ctx: ElabContext,
      resultCell: CellRW[AST],
      inferredTyCell: CellRW[AST]
  )(using module: M, solver: module.Solver[ElabConstraint]): ElabContext =
    ElabSupport.handleLet(elems, span, ctx, resultCell, inferredTyCell)

  override def normalizeTypeLikeKind(ty: AST): AST = ElabSupport.normalizeKindish(ty)
  override def substituteSolutions(ast: AST): AST = ast
}

/** Elaborate an effect body that only contains operation signatures. Each operation is registered with a type that carries the enclosing effect in
  * its effect row. No bodies are required or expected.
  */
private def processEffectBody[M <: SolverModule](
    body: CST.Block,
    effectRef: EffectRef,
    ctx: ElabContext
)(using module: M, solver: module.Solver[ElabConstraint]): (ElabContext, Vector[Param]) = {
  import module.given

  // Treat tail as another element so a final operation without semicolon is allowed
  val allElems = body.elements ++ body.tail.toVector

  var currentCtx = ctx
  val operations = scala.collection.mutable.ArrayBuffer.empty[Param]

  allElems.foreach {
    case seq @ CST.SeqOf(seqElems, span) if seqElems.toVector.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
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
            else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(typeElems), ElabSupport.combinedSpan(typeElems))
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
          operations += Param(opId, name, opTy, Implicitness.Explicit, None, chester.core.Coeffect.Unrestricted)
        }
      }

    case _ => () // Ignore non-def statements inside effect body for now
  }

  (currentCtx, operations.toVector)
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

  // Parse: [extension] def name [telescope]* (telescope)* = body
  // or:    [extension] def name [telescope]* (telescope)* : type = body
  val isExtension = elems.headOption.exists { case CST.Symbol("extension", _) => true; case _ => false }
  val defOffset = if (isExtension) 1 else 0

  if elems.length < 4 + defOffset then
    ctx.reporter.report(ElabProblem.UnboundVariable("Invalid def syntax", span))
    module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
    module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
    module.fill(solver, defTypeCell, AST.Type(AST.LevelLit(0, None), None))
    return Result.Done

  val name = elems(1 + defOffset) match
    case CST.Symbol(n, _) => n
    case _ =>
      ctx.reporter.report(ElabProblem.UnboundVariable("Expected name after def", span))
      "<error>"


  // Collect telescopes (lists for implicit, tuples for explicit)
  // IMPORTANT: We need to accumulate context as we go, so later telescopes can reference earlier parameters
  var idx = 2 + defOffset
  if isExtension && idx + 2 < elems.length && elems(idx).isInstanceOf[CST.Symbol] && elems(idx).asInstanceOf[CST.Symbol].name == "<" && elems(idx + 2).isInstanceOf[CST.Symbol] && elems(idx + 2).asInstanceOf[CST.Symbol].name == ">" then
    idx += 3

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
  var annotatedEffects: Vector[EffectRef] = Vector.empty
  var effectAnnotated = false
  if idx < elems.length && elems(idx).isInstanceOf[CST.Symbol] && elems(idx).asInstanceOf[CST.Symbol].name == ":" then
    idx += 1
    if idx < elems.length then
      val resultTyTyCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeStart = idx
      val typeEnd = elems.indexWhere(
        {
          case CST.Symbol("=", _) => true
          case CST.Symbol("/", _) => true
          case _                  => false
        },
        from = typeStart
      ) match
        case -1 => elems.length
        case n  => n
      val typeElems = elems.slice(typeStart, typeEnd)
      val baseTyCst = {
        if typeElems.length == 1 then typeElems.head
        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(typeElems), ElabSupport.combinedSpan(typeElems))
      }
      module.addConstraint(solver, ElabConstraint.Infer(baseTyCst, resultTyCell, resultTyTyCell, accumulatedCtx, asType = true))
      hasResultTy = true
      idx = typeEnd
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

  // Expect '='

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

  // Add constraint to pre-fill def type cell if return type is annotated
  if (hasResultTy) {
    module.addConstraint(
      solver,
      ElabConstraint.PreFillDefType(
        defTypeCell,
        telescopes.toVector,
        Some(resultTyCell),
        annotatedEffects,
        effectAnnotated,
        span
      )
    )
  }

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
      else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(annElems), ElabSupport.combinedSpan(annElems))
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

  ctx.bind(name, letId, letTypeCell).registerDefBody(letId, valueResult)
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
      val (coeffect, startIdx) = elemsVec.headOption match
        case Some(CST.IntegerLiteral(v, _)) if v == 0 => (Coeffect.Zero, 1)
        case Some(CST.IntegerLiteral(v, _)) if v == 1 => (Coeffect.One, 1)
        case _                                        => (Coeffect.Unrestricted, 0)

      (elemsVec.lift(startIdx), elemsVec.lift(startIdx + 1)) match
        case (Some(CST.Symbol(name, _)), Some(CST.Symbol(":", _))) =>
          val typeElems = elemsVec.drop(startIdx + 2)
          // Allow complex types after the colon; if there are multiple elems, wrap them in a SeqOf
          val typeCst = {
            if typeElems.length == 1 then typeElems.head
            else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(typeElems), ElabSupport.combinedSpan(typeElems))
          }


          val paramId = Uniqid.make[AST]
          // Elaborate the type in the CURRENT context (which includes previous params)
          val tyResult = module.newOnceCell[ElabConstraint, AST](solver)
          val tyTy = module.newOnceCell[ElabConstraint, AST](solver)
          module.addConstraint(solver, ElabConstraint.Infer(typeCst, tyResult, tyTy, currentCtx, asType = true))

          // Store cell reference as MetaCell - it will be resolved after constraints run
          val paramTy = AST.MetaCell(HoldNotReadable(tyResult), None)
          val param = Param(paramId, name, paramTy, implicitness, None, coeffect)

          // Extend context for the next parameter
          currentCtx = currentCtx.bind(name, paramId, tyResult)

          Some(param)
        case _ => None
    // Just a bare symbol for name-only parameters
    case CST.Symbol(name, _) =>
      val paramId = Uniqid.make[AST]
      // Use a meta-variable for the type
      val tyCell = module.newOnceCell[ElabConstraint, AST](solver)
      if implicitness == Implicitness.Implicit then
        module.fill(solver, tyCell, AST.Type(AST.LevelLit(0, None), None))
      val paramTy = AST.MetaCell(HoldNotReadable(tyCell), None)
      val defaultCoeffect = if implicitness == Implicitness.Implicit then chester.core.Coeffect.Zero else chester.core.Coeffect.Unrestricted
      val param = Param(paramId, name, paramTy, implicitness, None, defaultCoeffect)

      // Extend context for the next parameter
      currentCtx = currentCtx.bind(name, paramId, tyCell)

      Some(param)
    case _ => None
  }
  Telescope(parsedParams, implicitness)
}

private def extendCtxWithTelescope[M <: SolverModule](ctx: ElabContext, tel: Telescope)(using
    module: M,
    solver: module.Solver[ElabConstraint]
): ElabContext = {
  tel.params.foldLeft(ctx) { (acc, param) =>
    val paramTyCell = module.newOnceCell[ElabConstraint, AST](solver)
    module.fill(solver, paramTyCell, param.ty)
    acc.bind(param.name, param.id, paramTyCell)
  }
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
  val paramsTupleOpt = elems
    .drop(2)
    .takeWhile {
      case _: CST.Block => false
      case _            => true
    }
    .collectFirst { case CST.Tuple(params, _) => params }

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
  caseElems.collect {
    case CST.SeqOf(seqElems, span) if seqElems.toVector.headOption.exists { case CST.Symbol("case", _) => true; case _ => false } =>
      val name = seqElems.toVector.lift(1) match
        case Some(CST.Symbol(n, _)) => n
        case _ =>
          ctx.reporter.report(ElabProblem.UnboundVariable("Expected case name in enum", span))
          "<error>"
      val paramsTuple = seqElems.toVector.lift(2) match
        case Some(CST.Tuple(params, _)) => params
        case _                          => Vector.empty
      val telescope = parseTelescopeFromCST(paramsTuple, Implicitness.Explicit, ctxWithParams)
      EnumCase(Uniqid.make, name, telescope.params)
  }
}

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

  // We do not wait for argument types to be stable at the beginning,
  // so we can propagate type information bidirectionally to un-inferred argument types (like lambda parameters)

  // Wait for all explicit type argument types to be inferred
  if !c.explicitTypeArgTypes.forall(module.hasStableValue(solver, _)) then
    return Result.Waiting(c.explicitTypeArgTypes.filter(!module.hasStableValue(solver, _))*)

  // All prerequisites are ready - we're committed to processing now
  var func = module.readStable(solver, c.funcResult).get
  val explicitTypeArgs = c.explicitTypeArgResults.flatMap(module.readStable(solver, _))
  var explicitArgs = c.argResults.flatMap(module.readStable(solver, _))
  var argTypes = c.argTypes

  func match {
    case AST.ExtensionAccess(target, methodId, methodName, targetTyCell, span) =>
      func = AST.Ref(methodId, methodName, span)
      explicitArgs = target +: explicitArgs
      argTypes = targetTyCell.inner.asInstanceOf[module.CellRW[AST]] +: argTypes
    case _ =>
  }

  val funcTyOpt = module.readStable(solver, c.funcTy).map(substituteSolutions(_))

  funcTyOpt match
    case Some(AST.MetaCell(HoldNotReadable(cell), _)) =>
      // Underlying type not solved yet; wait for it to become available without refilling the once cell
      Result.Waiting(cell.asInstanceOf[module.CellAny])

    case Some(piTy @ AST.Pi(telescopes, resultTy, _, _)) =>
      // Resolve MetaCells in telescopes (parameter types may contain unresolved cells)
      val resolvedTelescopes =
        telescopes.map(tel => tel.copy(params = tel.params.map(param => param.copy(ty = normalizeTypeLikeKind(substituteSolutions(param.ty))))))
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
          case StmtAST.ExprStmt(expr, _)           => firstUnresolved(expr)
          case StmtAST.JSImport(_, _, _, _, ty, _) => firstUnresolved(ty)
          case StmtAST.Def(_, _, teles, resTy, body, _, _) =>
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
          case StmtAST.Effect(_, _, ops, _) =>
            ops.iterator.flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption
          case StmtAST.Enum(_, _, typeParams, cases, _) =>
            typeParams.iterator
              .flatMap(p => firstUnresolved(p.ty))
              .take(1)
              .toSeq
              .headOption
              .orElse(cases.iterator.flatMap(_.params.iterator).flatMap(p => firstUnresolved(p.ty)).take(1).toSeq.headOption)
          case StmtAST.Coenum(_, _, typeParams, cases, _) =>
            typeParams.iterator
              .flatMap(p => firstUnresolved(p.ty))
              .take(1)
              .toSeq
              .headOption
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
      val restParamOpt = explicitParams.lastOption.flatMap { param =>
        substituteSolutions(param.ty) match
          case AST.ListType(elem, _) => Some((param, elem))
          case _                     => None
      }
      val fixedParams = restParamOpt.map(_ => explicitParams.dropRight(1)).getOrElse(explicitParams)

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
      restParamOpt match
        case Some(_) =>
          if explicitArgs.size < fixedParams.size then
            c.ctx.reporter.report(ElabProblem.NotAFunction(func, c.span))
            module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
            module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
            return Result.Done
        case None =>
          if explicitArgs.size != explicitParams.size then
            c.ctx.reporter.report(ElabProblem.NotAFunction(func, c.span))
            module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
            module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
            return Result.Done

      // Type check arguments synchronously (not via constraints to avoid loops)
      // This may fill the implicit arg MetaCells through unification
      val implicitSubst = implicitParams.map(_.id).zip(implicitArgs).toMap
      var runningSubst = implicitSubst
      val fixedTypeChecksPassed = fixedParams.zip(argTypes.take(fixedParams.size)).zipWithIndex.forall {
        case ((param, argTyCell), idx) =>
          val baseParamTy = substituteSolutions(param.ty)
          val expectedParamTy = substituteInType(baseParamTy, runningSubst)
          val typeCheckOk = module.readStable(solver, argTyCell) match
            case Some(actualArgTy) =>
              expectedParamTy match
                case metaExpected @ AST.MetaCell(_, _) =>
                  unify(metaExpected, actualArgTy, c.span, c.ctx) match
                    case UnifyResult.Success => true
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
            case None =>
              unify(expectedParamTy, AST.MetaCell(HoldNotReadable(argTyCell.asInstanceOf[chester.utils.elab.CellRW[AST]]), c.span), c.span, c.ctx) match
                case UnifyResult.Success    => true
                case UnifyResult.Failure(_) =>
                  c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedParamTy, AST.MetaCell(HoldNotReadable(argTyCell.asInstanceOf[chester.utils.elab.CellRW[AST]]), c.span), c.span))
                  false
          if typeCheckOk then
            // Extend substitution with this explicit argument for later parameters
            explicitArgs.lift(idx).foreach(argAst => runningSubst = runningSubst + (param.id -> argAst))
          typeCheckOk
      }

      val restTypeChecksPassed = restParamOpt match
        case Some((restParam, restElemTy)) =>
          val baseElemTy = substituteSolutions(restElemTy)
          val expectedElemTy = substituteInType(baseElemTy, runningSubst)
          c.argTypes.drop(fixedParams.size).forall { argTyCell =>
            module.readStable(solver, argTyCell) match
              case Some(actualArgTy) =>
                expectedElemTy match
                  case metaExpected @ AST.MetaCell(_, _) =>
                    unify(metaExpected, actualArgTy, c.span, c.ctx) match
                      case UnifyResult.Success => true
                      case UnifyResult.Failure(_) =>
                        c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedElemTy, actualArgTy, c.span))
                        false
                  case _ =>
                    actualArgTy match
                      case AST.MetaCell(_, _) =>
                        if !module.hasStableValue(solver, argTyCell.asInstanceOf[module.CellRW[AST]]) then
                          module.fill(solver, argTyCell.asInstanceOf[module.CellRW[AST]], expectedElemTy)
                        true
                      case _ =>
                        unify(expectedElemTy, actualArgTy, c.span, c.ctx) match
                          case UnifyResult.Success    => true
                          case UnifyResult.Failure(_) =>
                            if isSubtype(actualArgTy, expectedElemTy, c.span, c.ctx) then true
                            else {
                              c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedElemTy, actualArgTy, c.span))
                              false
                            }
              case None =>
                unify(expectedElemTy, AST.MetaCell(HoldNotReadable(argTyCell.asInstanceOf[chester.utils.elab.CellRW[AST]]), c.span), c.span, c.ctx) match
                  case UnifyResult.Success    => true
                  case UnifyResult.Failure(_) =>
                    c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedElemTy, AST.MetaCell(HoldNotReadable(argTyCell.asInstanceOf[chester.utils.elab.CellRW[AST]]), c.span), c.span))
                    false
          }
        case None => true

      val allTypeChecksPassed = fixedTypeChecksPassed && restTypeChecksPassed

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

      val implicitArgsWithCoeffect = implicitParams.zip(resolvedImplicitArgs).map { case (p, a) => Arg(a, Implicitness.Implicit, p.coeffect) }
      val explicitArgsWithCoeffect = explicitArgs.zipWithIndex.map { case (a, idx) =>
        val coeffect = if idx < fixedParams.size then fixedParams(idx).coeffect else restParamOpt.map(_._1.coeffect).getOrElse(chester.core.Coeffect.Unrestricted)
        Arg(a, Implicitness.Explicit, coeffect)
      }

      // Build application - nest two Apps if we have both implicit and explicit args
      val app = if resolvedImplicitArgs.nonEmpty && explicitArgs.nonEmpty then
        // Nested: func[implicitArgs](explicitArgs)
        val implicitApp = AST.App(func, implicitArgsWithCoeffect, implicitArgs = true, c.span)
        AST.App(implicitApp, explicitArgsWithCoeffect, implicitArgs = false, c.span)
      else if resolvedImplicitArgs.nonEmpty then
        // Only implicit: func[implicitArgs]
        AST.App(func, implicitArgsWithCoeffect, implicitArgs = true, c.span)
      else {
        // Only explicit: func(explicitArgs)
        AST.App(func, explicitArgsWithCoeffect, implicitArgs = false, c.span)
      }

      // Perform substitution: replace parameter references in result type with resolved arguments
      val restArgForSubst = restParamOpt.map { _ =>
        val restArgs = explicitArgs.drop(fixedParams.size)
        AST.ListLit(restArgs.toVector, c.span)
      }
      val paramsForSubst = fixedParams ++ restParamOpt.map(_._1).toVector
      val argsForSubst = explicitArgs.take(fixedParams.size) ++ restArgForSubst.toVector
      val allParams = implicitParams ++ paramsForSubst
      val allArgs = resolvedImplicitArgs ++ argsForSubst
      val appValue = func match
        case AST.EnumCaseRef(enumId, caseId, enumName, caseName, _) =>
          AST.EnumCtor(enumId, caseId, enumName, caseName, allArgs, c.span)
        case _ => app

      if !module.hasStableValue(solver, c.result) then
        module.fill(solver, c.result, appValue)
      val substitutedTy = substituteInType(resolvedResultTy, allParams.map(_.id).zip(allArgs).toMap)
      val normalizedTy = reduce(substitutedTy, c.ctx)

      if !module.hasStableValue(solver, c.inferredTy) then
        module.fill(solver, c.inferredTy, normalizeType(normalizedTy))
      Result.Done
    case Some(ty) =>
      c.ctx.reporter.report(ElabProblem.NotAFunction(ty, c.span))
      if !module.hasStableValue(solver, c.result) then
        module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", c.span))
      if !module.hasStableValue(solver, c.inferredTy) then
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

  if !module.hasStableValue(solver, c.result) then
    module.fill(solver, c.result, AST.Ann(exprAst, tyAst, c.span))
  if !module.hasStableValue(solver, c.inferredTy) then
    module.fill(solver, c.inferredTy, tyAst)

  Result.Done
}

private def handleAssemblePi[M <: SolverModule](c: ElabConstraint.AssemblePi)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
  import module.given

  if module.hasStableValue(solver, c.result) && module.hasStableValue(solver, c.inferredTy) then return Result.Done

  if !module.hasStableValue(solver, c.resultTy) then return Result.Waiting(c.resultTy)

  val resTy = module.readStable(solver, c.resultTy).get
  val pi = AST.Pi(c.telescopes, resTy, Vector.empty, c.span)
  if !module.hasStableValue(solver, c.result) then
    module.fill(solver, c.result, pi)
  if !module.hasStableValue(solver, c.inferredTy) then
    module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), c.span))

  Result.Done
}

private def handlePreFillDefType[M <: SolverModule](c: ElabConstraint.PreFillDefType)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
  import module.given

  val resultTy = c.resultTyCell match {
    case Some(rtCell) =>
      val rtStable = module.hasStableValue(solver, rtCell)
      if !rtStable then return Result.Waiting(rtCell)
      module.readStable(solver, rtCell).get
    case None =>
      return Result.Done
  }

  val piTy = AST.Pi(c.telescopes, resultTy, c.effects, c.span)
  val normalizedPi = normalizeType(piTy)

  if (!module.hasStableValue(solver, c.defTypeCell)) {
    module.fill(solver, c.defTypeCell, normalizedPi)
  }
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

  def calleeEffects(callee: AST): Set[EffectRef] = callee match
    case r: AST.Ref        => effectsFromRef(r)
    case AST.Ann(expr, _, _) => calleeEffects(expr)
    case _                 => Set.empty

  // Compute required effects by scanning the body for calls whose types carry effect rows
  def gatherEffects(ast: AST): Set[EffectRef] = {
    ast match
      case AST.App(func, args, _, _) =>
        val fromFuncType = calleeEffects(func)
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
    case StmtAST.ExprStmt(expr, _)           => gatherEffects(expr)
    case StmtAST.JSImport(_, _, _, _, ty, _) => gatherEffects(ty)
    case StmtAST.Def(_, _, teles, resTy, body, _, _) =>
      teles.flatMap(t => t.params.map(p => gatherEffects(p.ty))).flatten.toSet ++ resTy.map(gatherEffects).getOrElse(Set.empty) ++ gatherEffects(
        body
      )
    case StmtAST.Record(_, _, fields, _) =>
      fields.flatMap(p => gatherEffects(p.ty)).toSet
    case StmtAST.Effect(_, _, ops, _) =>
      ops.flatMap(p => gatherEffects(p.ty)).toSet
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
  val normalizedPi = normalizeType(piTy)
  module.fill(solver, c.inferredTy, normalizedPi)
  if module.hasStableValue(solver, c.defTypeCell) then
    module.readStable(solver, c.defTypeCell.asInstanceOf[module.CellR[AST]]) match
      case Some(existingTy) => unify(existingTy, normalizedPi, c.span, c.ctx)
      case None             => module.fill(solver, c.defTypeCell, normalizedPi)
  else module.fill(solver, c.defTypeCell, normalizedPi)

  Result.Done
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

