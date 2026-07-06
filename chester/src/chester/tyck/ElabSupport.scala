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
