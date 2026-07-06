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
