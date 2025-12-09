package chester.transform

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, BuiltinEffect, EffectRef, Implicitness, Param, Telescope}
import chester.error.Span
import chester.uniqid.Uniqid

/** A small, type-directed CPS transformation scaffold for user-defined effects.
  *
  * The current elaborator tracks effect rows in `AST.Pi` types. This pass rewrites effectful function types into continuation-passing style and
  * offers a minimal expression transformation that threads a continuation through lambdas. It is designed as a future-facing building block: the pass
  * is not wired into the pipeline yet, but gives us a single place to evolve CPS lowering for user-defined effects (and optionally IO) without
  * disturbing type checking.
  *
  * Design notes:
  *   - Only `Pi` types with non-empty effect rows (or io when `transformIO` is enabled) are rewritten.
  *   - Effectful `Pi` becomes a pure `Pi` that accepts an extra continuation parameter `k` of type `(resultTy) -> answerType` and returns
  *     `answerType`.
  *   - For expressions, lambdas are rewritten to accept the extra continuation and immediately call it on the transformed body. Other nodes are
  *     transformed structurally; application rewriting would require type information for the callee, which we don't have yet, so it is left
  *     untouched.
  */
object EffectCPS:

  final case class Config(
      /** The ultimate answer type used by continuations. Defaults to unit (). */
      answerType: AST = AST.TupleType(Vector.empty, None),
      /** Also CPS-convert built-in io effects when true. */
      transformIO: Boolean = false
  )

  private def isBuiltinIo(effect: EffectRef): Boolean = {
    effect match
      case EffectRef.Builtin(BuiltinEffect.Io) => true
      case _                                   => false
  }

  private def isEffectfulPi(pi: AST.Pi, config: Config): Boolean =
    pi.effects.nonEmpty || (config.transformIO && pi.effects.exists(isBuiltinIo))

  /** Transform a type; effectful Pis gain an explicit continuation parameter and shed their effect row. */
  def transformType(ty: AST, config: Config): AST = {
    ty match
      case pi @ AST.Pi(telescopes, resultTy, effects, span) if isEffectfulPi(pi, config) =>
        val contId = Uniqid.make[AST]
        val contTy = continuationType(transformType(resultTy, config), config.answerType, span)
        val contParam = Param(contId, "k", contTy, Implicitness.Explicit, None)
        AST.Pi(
          telescopes :+ Telescope(Vector(contParam), Implicitness.Explicit),
          config.answerType,
          Vector.empty,
          span
        )
      case AST.Pi(telescopes, resultTy, effects, span) =>
        AST.Pi(
          telescopes.map(t => t.copy(params = t.params.map(p => p.copy(ty = transformType(p.ty, config))))),
          transformType(resultTy, config),
          effects,
          span
        )
      case AST.TupleType(elems, span) => AST.TupleType(elems.map(transformType(_, config)), span)
      case AST.ListType(elem, span)   => AST.ListType(transformType(elem, config), span)
      case other                      => other
  }

  private def continuationType(resultTy: AST, answerTy: AST, span: Option[Span]): AST = {
    val paramId = Uniqid.make[AST]
    val param = Param(paramId, "value", resultTy, Implicitness.Explicit, None)
    val tele = Vector(Telescope(Vector(param), Implicitness.Explicit))
    AST.Pi(tele, answerTy, Vector.empty, span)
  }

  /** Transform an expression with its (already known) type.
    *
    * The transformer is intentionally conservative: only lambdas gain explicit continuations. Call sites are left as-is because we currently lack the
    * callee's type information at this stage; wiring this pass after elaboration with a typed AST will let us close that gap later.
    */
  def transformExpr(expr: AST, exprTy: AST, config: Config): (AST, AST) = {
    (expr, exprTy) match
      case (AST.Lam(telescopes, body, span), pi @ AST.Pi(_, resultTy, _, _)) if isEffectfulPi(pi, config) =>
        val cpsTy = transformType(pi, config).asInstanceOf[AST.Pi]
        val contId = cpsTy.telescopes.last.params.head.id
        val contRef = AST.Ref(contId, "k", span)
        val (cpsBody, _) = transformExpr(body, resultTy, config)
        val contCall = AST.App(contRef, Vector(Arg(cpsBody)), implicitArgs = false, span)
        (AST.Lam(cpsTy.telescopes, contCall, span), cpsTy)

      case (AST.Lam(telescopes, body, span), pi @ AST.Pi(_, resultTy, effects, _)) =>
        val (cpsBody, cpsBodyTy) = transformExpr(body, resultTy, config)
        val cpsPi = AST.Pi(
          telescopes.map(t => t.copy(params = t.params.map(p => p.copy(ty = transformType(p.ty, config))))),
          cpsBodyTy,
          effects.filterNot(e => isBuiltinIo(e) && config.transformIO),
          span
        )
        (AST.Lam(telescopes, cpsBody, span), cpsPi)

      // Structural cases
      case (AST.Block(elems, tail, span), ty) =>
        val cpsElems = elems.map {
          case stmt @ chester.core.StmtAST.ExprStmt(e, _) =>
            val (newE, _) = transformExpr(e, ty, config)
            chester.core.StmtAST.ExprStmt(newE, stmt.span)
          case other => other
        }
        val (cpsTail, cpsTy) = transformExpr(tail, ty, config)
        (AST.Block(cpsElems.toVector, cpsTail, span), cpsTy)

      case (AST.Tuple(values, span), AST.TupleType(elemTys, _)) =>
        val pairs = values.zip(elemTys).map { case (v, t) => transformExpr(v, t, config)._1 }
        (AST.Tuple(pairs.toVector, span), AST.TupleType(elemTys.map(transformType(_, config)), span))

      case (AST.ListLit(values, span), AST.ListType(elemTy, _)) =>
        val elems = values.map(v => transformExpr(v, elemTy, config)._1)
        (AST.ListLit(elems, span), AST.ListType(transformType(elemTy, config), span))

      // Default: transform children structurally, leave type unchanged (or transformed for type nodes)
      case (otherExpr, otherTy) =>
        (otherExpr, transformType(otherTy, config))
  }
