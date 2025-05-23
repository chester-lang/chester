package chester.tyck

import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.api.SemanticCollector
import chester.error.*
import chester.uniqid.*
import chester.cell.*

trait ElaboraterFunction { this: ElaboraterBase & ElaboraterCommon =>
  def elabFunction(
      expr: FunctionExpr,
      ty: CellId[Term],
      outerEffects: CIdOf[EffectsCellContent]
  )(using
      ctx: Context,
      parameter: SemanticCollector,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Term
}

trait ProvideElaboraterFunction extends ElaboraterFunction { this: Elaborater & ElaboraterBase & ElaboraterCommon =>
  // Flag to enable or disable termination checking
  private val terminationCheckEnabled: Boolean = true // Set to false to disable termination checking

  private def elabArg(arg: Arg, effects: CIdOf[EffectsCellContent])(using
      localCtx: MutableContext,
      parameter: SemanticCollector,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): ArgTerm = {
    require(arg.decorations.isEmpty, "decorations are not supported yet")
    val ty = elabTy(arg.ty)
    val default = arg.exprOrDefault.map(elab(_, ty, effects))
    val id = Uniqid.generate[LocalV]
    val bind = newLocalv(arg.name.name, ty, id, arg.meta)
    val r = parameter.newSymbol(bind, id, arg, localCtx)
    localCtx.update(_.add(ContextItem(arg.name.name, id, bind, ty, Some(r))))
    default match {
      case Some(defaultValue) =>
        ArgTerm(bind, ty, Some(defaultValue), arg.vararg, meta = None)
      case None =>
        ArgTerm(bind, ty, None, arg.vararg, meta = None)
    }
  }

  private def elabTelescope(telescope: DefTelescope, effects: CIdOf[EffectsCellContent])(using
      MutableContext,
      SemanticCollector,
      TyckOps,
      StateOps[TyckOps]
  ): TelescopeTerm = {
    // Process each argument in the telescope, updating the context
    val argTerms = telescope.args.map(arg => elabArg(arg, effects))

    TelescopeTerm(argTerms, telescope.implicitly, meta = None)
  }

  def elabFunction(
      expr: FunctionExpr,
      ty: CellId[Term],
      outerEffects: CIdOf[EffectsCellContent]
  )(using
      ctx: Context,
      parameter: SemanticCollector,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Term = {
    // Start with a mutable local context based on the current context
    val mutableCtx = new MutableContext(ctx)

    val effects = newEffects

    // Elaborate each telescope and collect TelescopeTerms
    val telescopeTerms: Vector[TelescopeTerm] = expr.telescope.map { telescope =>
      elabTelescope(telescope, effects)(using
        mutableCtx,
        parameter,
        ck,
        state
      )
    }

    // Process the return type, if provided
    val returnType: Term = expr.resultTy match {
      case Some(rtExpr) =>
        checkType(rtExpr)(using mutableCtx.ctx, parameter, ck, state)
      case None =>
        newTypeTerm(using ck, state)
    }

    // Process the body of the function using the updated context
    val bodyTerm: Term = elab(expr.body, returnType, effects)(using
      mutableCtx.ctx,
      parameter,
      ck,
      state
    )

    // Build the function type by folding over the telescopes
    val functionType =
      FunctionType(telescopeTerms, returnType, effects = toTerm(effects), meta = None)

    // Unify the expected type with the constructed function type
    unify(ty, functionType, expr)

    // Extract the function name from 'expr.name', if available
    val functionNameOpt: Option[String] = None // TODO // placeholder

    // Termination check logic (can be easily removed or disabled)
    if (terminationCheckEnabled) {
      // placeholder
      val isTerminating = analyzeTermination(bodyTerm, functionNameOpt)
      if (!isTerminating) {
        val problem = PotentialNonterminatingFunction(expr)
        ck.reporter.report(problem)
      }
    }

    Function(functionType, bodyTerm, meta = None)
  }

  // placeholder, broken code
  // Simple termination analysis by traversing the Term using inspectRecursive
  private def analyzeTermination(
      term: Term,
      functionNameOpt: Option[String]
  )(using
      TyckOps,
      StateOps[TyckOps]
  ): Boolean = {
    // Collect function calls within the term
    val functionCalls = collectFunctionCalls(term)

    functionNameOpt.forall(functionName => !functionCalls.contains(functionName))
  }

  // Helper function to collect function call names from a Term using inspectRecursive
  private def collectFunctionCalls(term: Term): Set[String] = {
    val calls = scala.collection.mutable.Set[String]()

    term.inspectRec {
      case FCallTerm(function, _, _) =>
        function match {
          // TODO: calls += name
          case _ =>
            // Continue traversing the function term
            function.inspectRec {
              // TODO: calls += name
              _ =>
            }
        }
      case _ =>
    }

    calls.toSet
  }
}
