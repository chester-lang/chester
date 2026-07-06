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
private val DebugUnboundLogging = false

/** Handler for elaboration constraints */
object ElabHandler extends Handler[ElabConstraint]:

  def run[M <: SolverModule](constraint: ElabConstraint)(using module: M, solver: module.Solver[ElabConstraint]): Result = {
    constraint match
      case c: ElabConstraint.Check => handleCheck(c)
      case c: ElabConstraint.Infer => handleInferExpr(c)
      case c: ElabConstraint.InferTopLevel =>
        ElaboratorBlocks.handleInferTopLevel(c)
      case c: ElabConstraint.Unify       => handleUnify(c)
      case c: ElabConstraint.Subtype     => handleSubtype(c)
      case c: ElabConstraint.IsUniverse  => handleIsUniverse(c)
      case c: ElabConstraint.IsPi        => handleIsPi(c)
      case c: ElabConstraint.AssembleApp => handleAssembleApp(c)
      case c: ElabConstraint.AssembleAnn => handleAssembleAnn(c)
      case c: ElabConstraint.AssembleDef => handleAssembleDef(c)
      case c: ElabConstraint.PreFillDefType => handlePreFillDefType(c)
      case c: ElabConstraint.AssemblePi => handleAssemblePi(c)
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

    def rewriteEffectQualifiers(cst: CST, effectName: String): CST = cst match
      case CST.SeqOf(elems, span) =>
        val buf = scala.collection.mutable.ArrayBuffer.empty[CST]
        var idx = 0
        while idx < elems.length do
          elems.toVector(idx) match
            case CST.Symbol(name, _) if name == effectName && idx + 1 < elems.length &&
                  elems.toVector(idx + 1).isInstanceOf[CST.Symbol] && elems.toVector(idx + 1).asInstanceOf[CST.Symbol].name == "." =>
              idx += 2 // drop qualifier and dot
            case other =>
              buf += rewriteEffectQualifiers(other, effectName)
              idx += 1
        CST.SeqOf(NonEmptyVector.fromVectorUnsafe(buf.toVector), span)
      case CST.Tuple(elements, span) =>
        CST.Tuple(elements.map(rewriteEffectQualifiers(_, effectName)), span)
      case CST.ListLiteral(elements, span) =>
        CST.ListLiteral(elements.map(rewriteEffectQualifiers(_, effectName)), span)
      case CST.Block(elements, tail, span) =>
        CST.Block(elements.map(rewriteEffectQualifiers(_, effectName)), tail.map(rewriteEffectQualifiers(_, effectName)), span)
      case other => other

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

      // Comments are syntax-only and elaborate to unit when preserved by the parser.
      case CST.Comment(_, _, span) =>
        module.fill(solver, c.result, AST.Tuple(Vector.empty, span))
        module.fill(solver, c.inferredTy, AST.TupleType(Vector.empty, span))
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
        else if name == "Bool" then
          val ast = AST.BoolType(span)
          module.fill(solver, c.result, ast)
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
        else if name == "->" then

          c.ctx.reporter.report(ElabProblem.UnboundVariable(name, span))
          val metaId = Uniqid.make[AST]
          val ast = AST.Ref(metaId, name, span)
          module.fill(solver, c.result, ast)
          module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
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
        else {
          c.ctx.lookup(name) match
            case Some(id) =>
              val ast = AST.Ref(id, name, span)
              module.fill(solver, c.result, ast)
              c.ctx.lookupType(id) match
                case Some(tyCell) =>
                  // Copy the type cell content
                  val isStable = module.hasStableValue(solver, tyCell)

                  module.readStable(solver, tyCell) match
                    case Some(ty) =>
                      if !module.hasStableValue(solver, c.inferredTy) then
                        module.fill(solver, c.inferredTy, ty)
                      else
                        val expectedTy = module.readStable(solver, c.inferredTy).get
                        unify(expectedTy, ty, span, c.ctx) match
                          case UnifyResult.Success => ()
                          case UnifyResult.Failure(_) =>
                            c.ctx.reporter.report(ElabProblem.TypeMismatch(expectedTy, ty, span))
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
                if DebugUnboundLogging then
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
          if elements.isEmpty then
            c.ctx.reporter.report(ElabProblem.UnitValueUsedAsType(span))
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
        ElaboratorBlocks.elaborateBlockLike(c.ctx, c.result, c.inferredTy, elements, tail, span)

      // SeqOf: could be function application or sequence (block-only statements handled separately)
      case CST.SeqOf(elements, span) =>
        val elems = elements.toVector

        lambdaPattern(elems, c.ctx) match
          case Some(LambdaParts(telescopes, bodyCst, paramCtx, lamSpan)) =>
            val bodyResult = module.newOnceCell[ElabConstraint, AST](solver)
            val bodyTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.addConstraint(solver, ElabConstraint.Infer(bodyCst, bodyResult, bodyTy, paramCtx, asType = c.asType))

            val bodyAst = AST.MetaCell(HoldNotReadable(bodyResult), bodyCst.span)
            if c.asType then
              val pi = AST.Pi(telescopes, bodyAst, Vector.empty, lamSpan)
              module.fill(solver, c.result, pi)
              module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), lamSpan))
            else {
              val lam = AST.Lam(telescopes, bodyAst, lamSpan)
              val lamTy = AST.Pi(telescopes, AST.MetaCell(HoldNotReadable(bodyTy), bodyCst.span), Vector.empty, lamSpan)
              module.fill(solver, c.result, lam)
              module.fill(solver, c.inferredTy, lamTy)
            }
            Result.Done
          case None =>
            piPattern(elems, c.ctx) match
              case Some(parts @ PiParts(telescopes, resultTyCst, piSpan)) =>

                val resultTyResult = module.newOnceCell[ElabConstraint, AST](solver)
                val resultTyTy = module.newOnceCell[ElabConstraint, AST](solver)
                var paramCtx = c.ctx
                for tel <- telescopes do paramCtx = extendCtxWithTelescope(paramCtx, tel)
                module.addConstraint(solver, ElabConstraint.Infer(resultTyCst, resultTyResult, resultTyTy, paramCtx, asType = true))

                module.addConstraint(
                  solver,
                  ElabConstraint.AssemblePi(
                    telescopes,
                    resultTyResult,
                    c.result,
                    c.inferredTy,
                    piSpan
                  )
                )
                return Result.Done
              case None =>

            // Reject block-only statements in expression context

            elems.headOption match
              case Some(CST.Symbol("def", _)) =>
                c.ctx.reporter.report(
                  ElabProblem.UnboundVariable("def statement only allowed in block elements, not in expression position", span)
                )
                val metaId = Uniqid.make[AST]
                val ast = AST.Ref(metaId, "def", span)
                module.fill(solver, c.result, ast)
                val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
                module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
                Result.Done
              case Some(CST.Symbol("let", _)) =>
                c.ctx.reporter.report(
                  ElabProblem.UnboundVariable("let statement only allowed in block elements, not in expression position", span)
                )
                val metaId = Uniqid.make[AST]
                val ast = AST.Ref(metaId, "let", span)
                module.fill(solver, c.result, ast)
                val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
                module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
                Result.Done
              case Some(CST.Symbol("do", _)) =>
                // `do op(args)` — perform an effect operation
                // Syntax: do <op> (<args>)
                // This is sugar for a function call that carries the effect in its type
                if elems.length < 2 then
                  c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected operation name after 'do'", span))
                  module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
                  module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
                  Result.Done
                else {
                  // Reconstruct as a normal function call: op(args)
                  val callElems = elems.tail
                  val callCst = if callElems.length == 1 then callElems.head
                                else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(callElems), ElabSupport.combinedSpan(callElems))
                  module.addConstraint(solver, ElabConstraint.Infer(callCst, c.result, c.inferredTy, c.ctx))
                  Result.Done
                }
              case Some(CST.Symbol("handle", _)) =>

                // `handle <expr> with <EffName> { def op1(args) = body; ... }`
                // Syntax: handle expr with EffectName { ... }
                // elems = [handle, expr..., with, EffName, Block{...}]
                val withIdx = elems.indexWhere { case CST.Symbol("with", _) => true; case _ => false }
                if withIdx < 0 || withIdx + 2 >= elems.length then
                  c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected 'with EffectName { handlers }' after handle expression", span))
                  module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
                  module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
                  Result.Done
                else {
                  val actionElems = elems.slice(1, withIdx)
                  val effNameCst = elems(withIdx + 1)
                  val handlerBlockCst = elems(withIdx + 2)
                  
                  val effName = effNameCst match
                    case CST.Symbol(n, _) => n
                    case _ =>
                      c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected effect name after 'with'", span))
                      "<error>"
                  
                  c.ctx.lookupEffect(effName) match
                    case None =>
                      c.ctx.reporter.report(ElabProblem.UnknownEffect(effName, span))
                      module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
                      module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
                      Result.Done
                    case Some(effRef) =>
                      // Elaborate the action as a thunk: () -> R / {EffName}
                      val actionCst = if actionElems.length == 1 then actionElems.head
                                      else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(actionElems), ElabSupport.combinedSpan(actionElems))
                      val actionResult = module.newOnceCell[ElabConstraint, AST](solver)
                      val actionTy = module.newOnceCell[ElabConstraint, AST](solver)
                      module.addConstraint(solver, ElabConstraint.Infer(actionCst, actionResult, actionTy, c.ctx))
                      
                      // Parse handler block: { def op1(args) = body; ... }
                      // Each handler lambda gets a `resume` parameter for the continuation
                      val handlerDefs = handlerBlockCst match
                        case CST.Block(blockElems, blockTail, _) =>
                          (blockElems ++ blockTail.toVector).collect {
                            case CST.SeqOf(seqElems, defSpan) if seqElems.toVector.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
                              val deVec = seqElems.toVector
                              val opName = deVec.lift(1).collect { case CST.Symbol(n, _) => n }.getOrElse("<error>")
                              // Find the op id in context (registered by processEffectBody)
                              val opId = c.ctx.lookup(opName).getOrElse(Uniqid.make[AST])
                              // Add `resume` to the handler's scope as a continuation function
                              val resumeId = Uniqid.make[AST]
                              val resumeTyCell = module.newOnceCell[ElabConstraint, AST](solver)
                              val handlerCtx = c.ctx.bind("resume", resumeId, resumeTyCell)
                              // Parse: def opName(params) = body  — find the = and elaborte body
                              val eqIdx = deVec.indexWhere { case CST.Symbol("=", _) => true; case _ => false }
                              val handlerBodyCst = if eqIdx >= 0 && eqIdx + 1 < deVec.length then
                                if deVec.length - eqIdx == 2 then deVec(eqIdx + 1)
                                else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(deVec.drop(eqIdx + 1)), defSpan)
                              else CST.Symbol("<error>", defSpan)
                              val bodyResult = module.newOnceCell[ElabConstraint, AST](solver)
                              val bodyTy = module.newOnceCell[ElabConstraint, AST](solver)
                              module.addConstraint(solver, ElabConstraint.Infer(handlerBodyCst, bodyResult, bodyTy, handlerCtx))
                              (opName, AST.MetaCell(HoldNotReadable(bodyResult), defSpan))
                          }
                        case other =>
                          // Single handler
                          Vector.empty
                      
                      val actionAst = AST.MetaCell(HoldNotReadable(actionResult), actionCst.span)
                      val handleAst = AST.Handle(actionAst, effRef, handlerDefs, span)

                      module.fill(solver, c.result, handleAst)
                      // The result type is the action's return type (with the effect removed)
                      module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(actionTy), span))
                      Result.Done
                }
              case Some(CST.Symbol("effect", _)) =>
                c.ctx.reporter.report(
                  ElabProblem.UnboundVariable("effect statement only allowed in block elements, not in expression position", span)
                )
                val metaId = Uniqid.make[AST]
                val ast = AST.Ref(metaId, "effect", span)
                module.fill(solver, c.result, ast)
                val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
                module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
                Result.Done
              case Some(CST.Symbol("record", _)) =>
                c.ctx.reporter.report(
                  ElabProblem.UnboundVariable("record statement only allowed in block elements, not in expression position", span)
                )
                val metaId = Uniqid.make[AST]
                val ast = AST.Ref(metaId, "record", span)
                module.fill(solver, c.result, ast)
                val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
                module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
                Result.Done
              case Some(CST.Symbol("import", _)) =>
                c.ctx.reporter.report(
                  ElabProblem.UnboundVariable("import statement only allowed in block elements, not in expression position", span)
                )
                val metaId = Uniqid.make[AST]
                val ast = AST.Ref(metaId, "import", span)
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
                        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(rest), ElabSupport.combinedSpan(rest))
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
                    case None    =>
                      // Binary operator sugar: lhs op rhs
                      // Unary prefix operator sugar: op rhs (e.g. -1)
                      elems match
                        case Vector(lhs, CST.Symbol(op, opSpan), rhs) if c.ctx.isBuiltin(op) =>
                          val argTuple: CST.Tuple = CST.Tuple(Vector(lhs, rhs), ElabSupport.combinedSpan(Vector(lhs, rhs)))
                          val funcCst = CST.Symbol(op, opSpan)
                          handleFunctionApplication(c.ctx, c.asType, c.result, c.inferredTy, funcCst, None, argTuple, span)
                        case Vector(CST.Symbol(op, opSpan), rhs) if c.ctx.isBuiltin(op) =>
                          val argTuple: CST.Tuple = CST.Tuple(Vector(rhs), rhs.span)
                          val funcCst = CST.Symbol(op, opSpan)
                          handleFunctionApplication(c.ctx, c.asType, c.result, c.inferredTy, funcCst, None, argTuple, span)
                        case _ =>
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
                              val maybeTypeArgs = {
                                if elems.length >= 2 then
                                  elems(elems.length - 2) match
                                    case l: CST.ListLiteral => Some(l)
                                    case _                  => None
                                else None
                              }

                              (maybeTuple, maybeTypeArgs) match
                                case (Some(tuple), Some(typeArgs)) =>
                                  val funcElems = elems.dropRight(2)
                                  if funcElems.nonEmpty then
                                    val funcCst = {
                                      if funcElems.length == 1 then funcElems.head
                                      else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(funcElems), ElabSupport.combinedSpan(funcElems))
                                    }
                                    handleFunctionApplication(c.ctx, c.asType, c.result, c.inferredTy, funcCst, Some(typeArgs), tuple, span)
                                  else {
                                    val blockCst = CST.Block(elems.dropRight(1), Some(elems.last), span)
                                    module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
                                    Result.Done
                                  }
                                case (Some(tuple), None) =>
                                  val funcElems = elems.dropRight(1)
                                  if funcElems.nonEmpty then
                                    val funcCst = {
                                      if funcElems.length == 1 then funcElems.head
                                      else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(funcElems), ElabSupport.combinedSpan(funcElems))
                                    }
                                    handleFunctionApplication(c.ctx, c.asType, c.result, c.inferredTy, funcCst, None, tuple, span)
                                  else {
                                    val blockCst = CST.Block(elems.dropRight(1), Some(elems.last), span)
                                    module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
                                    Result.Done
                                  }
                                case _ =>
                                  if c.asType then
                                    val funcCst = elems.head
                                    val args = elems.tail
                                    val argsTuple: CST.Tuple = CST.Tuple(args, ElabSupport.combinedSpan(args))
                                    handleFunctionApplication(c.ctx, c.asType, c.result, c.inferredTy, funcCst, None, argsTuple, span)
                                  else
                                    // Default: treat as a block-like sequence
                                    val blockCst = CST.Block(elems.dropRight(1), Some(elems.last), span)
                                    module.addConstraint(solver, ElabConstraint.Infer(blockCst, c.result, c.inferredTy, c.ctx))
                                    Result.Done
                }
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

    def handleFieldAccess(lhs: CST, field: String): Result = {
      val targetResult = module.newOnceCell[ElabConstraint, AST](solver)
      val targetTy = module.newOnceCell[ElabConstraint, AST](solver)
      module.addConstraint(solver, ElabConstraint.Infer(lhs, targetResult, targetTy, ctx, asType = asType))
      val targetAst = AST.MetaCell(HoldNotReadable(targetResult), lhs.span)

      val extIds = ctx.extensionBindings.getOrElse(field, Set.empty)
      if extIds.nonEmpty then
        val extId = extIds.head // just taking the first one for now
        val extNameOption = ctx.bindings.find(_._2 == extId).map(_._1).getOrElse(field)
        val targetTyCell = HoldNotReadable(targetTy).asInstanceOf[HoldNotReadable[chester.utils.elab.CellRW[AST]]]
        val accessAst = AST.ExtensionAccess(targetAst, extId, extNameOption, targetTyCell, span)
        fillResultOnce(resultCell, accessAst)
        val methodTyCell = ctx.lookupType(extId).get
        val methodTy = module.readStable(solver, methodTyCell).getOrElse(AST.MetaCell(HoldNotReadable(methodTyCell), span))
        fillResultOnce(inferredTyCell, methodTy)
        return Result.Done

      val accessAst = AST.FieldAccess(targetAst, field, span)
      fillResultOnce(resultCell, accessAst)
      module.readStable(solver, targetTy) match
        case None =>
          // Try to use a known binding type (if lhs is a symbol) before deferring
          lhs match
            case CST.Symbol(sym, _) =>
              val knownRecOpt = (for
                id <- ctx.lookup(sym)
                tyCell <- ctx.lookupType(id)
                ty <- module.readStable(solver, tyCell)
              yield (id, ty)).orElse {
                if ctx.jsImports.contains(sym) then
                  val jsNameStr = JSImportSignature.recordTypeNameFor(sym)
                  val goNameStr = s"GoImport_${GoImportSignature.normalizePackagePath(sym)}"
                  ctx.lookupRecord(goNameStr).orElse(ctx.lookupRecord(jsNameStr)) match
                    case Some(rec) => Some((rec.id, AST.RecordTypeRef(rec.id, rec.name, span)))
                    case None => None
                else None
              }

              knownRecOpt match
                case Some((_, AST.RecordTypeRef(recId, _, _))) =>
                  ctx.lookupRecordById(recId).flatMap(_.fields.find(_.name == field)) match
                    case Some(param) => fillResultOnce(inferredTyCell, param.ty)
                    case None        => fillResultOnce(inferredTyCell, AST.AnyType(span))
                case _ => ()
            case _ => ()
          // If we still haven't produced a type, defer to the eventual target type
          if !module.hasSomeValue(solver, inferredTyCell.asInstanceOf[module.CellAny]) then
            fillResultOnce(inferredTyCell, AST.MetaCell(HoldNotReadable(targetTy), span))
          Result.Done
        case Some(AST.RecordTypeRef(recId, _, _)) =>
          ctx.lookupRecordById(recId) match
            case Some(rec) =>
              rec.fields.find(_.name == field) match
                case Some(param) =>
                  fillResultOnce(inferredTyCell, param.ty)
                  Result.Done
                case None =>
                  // Unknown field on a known record type – fall back to Any to keep solving
                  fillResultOnce(inferredTyCell, AST.AnyType(span))
                  Result.Done
            case None =>
              // Should not happen; fall back conservatively
              fillResultOnce(inferredTyCell, AST.AnyType(span))
              Result.Done
        case Some(_) =>
          // Field access on non-record type – yield Any to avoid dangling metas
          fillResultOnce(inferredTyCell, AST.AnyType(span))
          Result.Done
    }

    elems match
      case _ if elems.length > 3 && elems.lastOption.exists(_.isInstanceOf[CST.Symbol]) &&
            elems.lift(elems.length - 2).exists {
              case CST.Symbol(".", _) => true
              case _                  => false
            } =>
        val dottedSymbols = {
          val positionsOk = elems.indices.forall { idx =>
            if idx % 2 == 0 then elems(idx).isInstanceOf[CST.Symbol]
            else elems(idx) match
              case CST.Symbol(".", _) => true
              case _                  => false
          }
          if positionsOk then Some(elems.indices.collect { case idx if idx % 2 == 0 => elems(idx).asInstanceOf[CST.Symbol] }.toVector)
          else None
        }

        dottedSymbols match
          case Some(symbols) if symbols.length >= 2 =>
            val head = symbols.head
            ctx.lookup(head.name).flatMap(ctx.lookupType).flatMap(module.readStable(solver, _)) match
              case Some(initialTy) =>
                val baseRef = AST.Ref(ctx.lookup(head.name).get, head.name, head.span)
                val (finalExpr, finalTyOpt) = symbols.tail.foldLeft((baseRef: AST, Option(initialTy))) { case ((exprAcc, tyOpt), sym) =>
                  val nextExpr = AST.FieldAccess(exprAcc, sym.name, sym.span.orElse(span))
                  val nextTy = tyOpt match
                    case Some(AST.RecordTypeRef(recId, _, _)) =>
                      ctx.lookupRecordById(recId).flatMap(_.fields.find(_.name == sym.name)).map(_.ty)
                    case _ => None
                  (nextExpr, nextTy)
                }
                fillResultOnce(resultCell, finalExpr)
                finalTyOpt match
                  case Some(finalTy) =>
                    fillResultOnce(inferredTyCell, finalTy)
                    Some(Result.Done)
                  case None =>
                    Some(handleFieldAccess(
                      if symbols.length == 2 then head
                      else {
                        val lhsElems = elems.dropRight(2)
                        if lhsElems.length == 1 then lhsElems.head
                        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(lhsElems), ElabSupport.combinedSpan(lhsElems))
                      },
                      symbols.last.name
                    ))
              case None =>
                val fieldSym = elems.last.asInstanceOf[CST.Symbol]
                val lhsElems = elems.dropRight(2)
                val lhs = {
                  if lhsElems.length == 1 then lhsElems.head
                  else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(lhsElems), ElabSupport.combinedSpan(lhsElems))
                }
                Some(handleFieldAccess(lhs, fieldSym.name))
          case _ =>
            val fieldSym = elems.last.asInstanceOf[CST.Symbol]
            val lhsElems = elems.dropRight(2)
            val lhs = {
              if lhsElems.length == 1 then lhsElems.head
              else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(lhsElems), ElabSupport.combinedSpan(lhsElems))
            }
            Some(handleFieldAccess(lhs, fieldSym.name))
      case Vector(CST.Symbol(name, _), CST.Symbol(".", _), CST.Symbol("t", _)) =>
        ctx.lookupEnum(name) match
          case Some(en) =>
            val ast = AST.EnumTypeRef(en.id, name, span)
            fillResultOnce(resultCell, ast)
            val teleImplicitness = en.typeParams.headOption.map(_.implicitness).getOrElse(Implicitness.Explicit)
            val enumTy = {
              if en.typeParams.nonEmpty then
                AST.Pi(Vector(Telescope(en.typeParams, teleImplicitness)), AST.Type(AST.LevelLit(0, None), span), Vector.empty, span)
              else AST.Type(AST.LevelLit(0, None), None)
            }
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
                val enumTy = {
                  if typeArgs.nonEmpty then AST.App(AST.EnumTypeRef(enumDef.id, enumOrVal, span), typeArgs, implicitArgs = false, span)
                  else AST.EnumTypeRef(enumDef.id, enumOrVal, span)
                }
                val ctorType = {
                  if typeParamTele.isEmpty && caseTele.isEmpty then enumTy
                  else AST.Pi(typeParamTele ++ caseTele, enumTy, Vector.empty, span)
                }
                val ast = AST.EnumCaseRef(enumDef.id, caseDef.id, enumOrVal, field, span)
                fillResultOnce(resultCell, ast)
                fillResultOnce(inferredTyCell, ctorType)
                Some(Result.Done)
              case None => None
          case None =>
            ctx.lookupRecord(enumOrVal).flatMap(_.fields.find(_.name == field)) match
              case Some(param) =>
                val ast = AST.RecordTypeRef(ctx.lookupRecord(enumOrVal).get.id, enumOrVal, span)
                val accessAst = AST.FieldAccess(ast, field, span)
                fillResultOnce(resultCell, accessAst)
                fillResultOnce(inferredTyCell, param.ty)
                Some(Result.Done)
              case None =>
                Some(handleFieldAccess(lhs, field))
      case Vector(lhs, CST.Symbol(".", _), CST.Symbol(field, _)) =>
        Some(handleFieldAccess(lhs, field))
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
          else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(exprElems), ElabSupport.combinedSpan(exprElems))
        }
        val ty = {
          if typeElems.length == 1 then typeElems.head
          else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(typeElems), ElabSupport.combinedSpan(typeElems))
        }
        Some((expr, ty))
      else None
    else None
  }

  private case class LambdaParts(
      telescopes: Vector[Telescope],
      body: CST,
      paramCtx: ElabContext,
      span: Option[Span]
  )

  private def lambdaPattern[M <: SolverModule](
      elems: Vector[CST],
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): Option[LambdaParts] = {
    elems.headOption match
      case Some(CST.Symbol(sym, lamSpan)) if sym == "\\" || sym == "λ" =>
        val arrowIndex = elems.indexWhere {
          case CST.Symbol("->", _) | CST.Symbol("=>", _) => true
          case _                                         => false
        }
        if arrowIndex < 1 || arrowIndex >= elems.length - 1 then None
        else {
          val paramElems = elems.slice(1, arrowIndex)
          val bodyElems = elems.drop(arrowIndex + 1)
          val bodyCst = {
            if bodyElems.length == 1 then bodyElems.head
            else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(bodyElems), ElabSupport.combinedSpan(bodyElems))
          }

          var paramCtx = ctx
          val telescopes = scala.collection.mutable.ArrayBuffer.empty[Telescope]

          paramElems.foreach {
            case CST.ListLiteral(params, _) =>
              val tel = parseTelescopeFromCST(params, Implicitness.Implicit, paramCtx)(using module, solver)
              telescopes += tel
              paramCtx = extendCtxWithTelescope(paramCtx, tel)
            case CST.Tuple(params, _) =>
              val tel = parseTelescopeFromCST(params, Implicitness.Explicit, paramCtx)(using module, solver)
              telescopes += tel
              paramCtx = extendCtxWithTelescope(paramCtx, tel)
            case sym: CST.Symbol =>
              val tel = parseTelescopeFromCST(Vector(sym), Implicitness.Explicit, paramCtx)(using module, solver)
              telescopes += tel
              paramCtx = extendCtxWithTelescope(paramCtx, tel)
            case other =>
              ctx.reporter.report(ElabProblem.UnboundVariable("Invalid lambda parameter", other.span))
          }

          Some(LambdaParts(telescopes.toVector, bodyCst, paramCtx, lamSpan))
        }
      case _ => None
  }

  private case class PiParts(
      telescopes: Vector[Telescope],
      resultTy: CST,
      span: Option[Span]
  )

  private def piPattern[M <: SolverModule](
      elems: Vector[CST],
      ctx: ElabContext
  )(using module: M, solver: module.Solver[ElabConstraint]): Option[PiParts] = {

    val arrowIndex = elems.indexWhere {
      case CST.Symbol("->", _) => true
      case _                   => false
    }

    if arrowIndex < 0 || arrowIndex >= elems.length - 1 then None
    else {
      val paramElems = elems.slice(0, arrowIndex)
      val bodyElems = elems.drop(arrowIndex + 1)
      val bodyCst = {
        if bodyElems.length == 1 then bodyElems.head
        else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(bodyElems), ElabSupport.combinedSpan(bodyElems))
      }

      var paramCtx = ctx
      val telescopes = scala.collection.mutable.ArrayBuffer.empty[Telescope]

      paramElems.foreach {
        case CST.ListLiteral(params, _) =>
          val tel = parseTelescopeFromCST(params, Implicitness.Implicit, paramCtx)(using module, solver)
          telescopes += tel
          paramCtx = extendCtxWithTelescope(paramCtx, tel)
        case CST.Tuple(params, _) =>
          val tel = parseTelescopeFromCST(params, Implicitness.Explicit, paramCtx)(using module, solver)
          telescopes += tel
          paramCtx = extendCtxWithTelescope(paramCtx, tel)
        case sym: CST.Symbol =>
          val tel = parseTelescopeFromCST(Vector(sym), Implicitness.Explicit, paramCtx)(using module, solver)
          telescopes += tel
          paramCtx = extendCtxWithTelescope(paramCtx, tel)
        case other =>
          ctx.reporter.report(ElabProblem.UnboundVariable("Invalid Pi parameter", other.span))
      }

      Some(PiParts(telescopes.toVector, bodyCst, ElabSupport.combinedSpan(elems)))
    }
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
    CST.SeqOf(NonEmptyVector.fromVectorUnsafe(seqElems), ElabSupport.combinedSpan(seqElems))
  }

