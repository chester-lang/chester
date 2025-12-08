package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, StmtAST, Telescope}
import chester.utils.HoldNotReadable

/** Lightweight core type checker used to validate elaborated ASTs and normalize types. */
object CoreTypeChecker:
  private type Env = Map[chester.uniqid.UniqidOf[AST], AST]

  /** Normalize a type AST with shallow beta-reduction of type-level lambdas/applications. */
  def normalizeType(ast: AST): AST =
    ast match
      case AST.App(func, args, implicitArgs, span) =>
        val nFunc = normalizeType(func)
        val nArgs = args.map(a => Arg(normalizeType(a.value), a.implicitness))
        nFunc match
          case AST.Lam(teles, body, _) if teles.flatMap(_.params).length == nArgs.length =>
            val subst = teles.flatMap(_.params).map(_.id).zip(nArgs.map(_.value)).toMap
            normalizeType(substituteInType(body, subst))
          case _ => AST.App(nFunc, nArgs, implicitArgs, span)
      case AST.Lam(teles, body, span) =>
        val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))))
        AST.Lam(nTeles, normalizeType(body), span)
      case AST.Pi(teles, resultTy, effs, span) =>
        val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))))
        AST.Pi(nTeles, normalizeType(resultTy), effs, span)
      case AST.ListType(elem, span) => AST.ListType(normalizeType(elem), span)
      case AST.Tuple(elems, span)   => AST.Tuple(elems.map(normalizeType), span)
      case AST.ListLit(elems, span) => AST.ListLit(elems.map(normalizeType), span)
      case AST.Let(id, name, ty, value, body, span) =>
        AST.Let(id, name, ty.map(normalizeType), normalizeType(value), normalizeType(body), span)
      case AST.Ann(expr, ty, span)      => AST.Ann(normalizeType(expr), normalizeType(ty), span)
      case AST.Block(elems, tail, span) => AST.Block(elems.map(normalizeTypeStmt), normalizeType(tail), span)
      case other                        => other

  private def normalizeTypeStmt(stmt: StmtAST): StmtAST =
    stmt match
      case StmtAST.ExprStmt(expr, span) => StmtAST.ExprStmt(normalizeType(expr), span)
      case StmtAST.Def(id, name, teles, resTy, body, span) =>
        val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))))
        StmtAST.Def(id, name, nTeles, resTy.map(normalizeType), normalizeType(body), span)
      case StmtAST.Pkg(name, body, span) => StmtAST.Pkg(name, normalizeType(body), span)

  /** Entry point to check whether an AST is well-typed according to a simple dependent type checker. */
  def typeChecks(ast: AST): Boolean = infer(ast, Map.empty).isDefined

  private def check(ast: AST, expected: AST, env: Env): Boolean =
    infer(ast, env).exists(t => normalizeType(t) == normalizeType(expected))

  private def infer(ast: AST, env: Env): Option[AST] =
    ast match
      case AST.Ref(id, _, _)          => env.get(id)
      case AST.StringLit(_, _)        => Some(AST.StringType(None))
      case AST.IntLit(_, _)           => Some(AST.IntegerType(None))
      case AST.AnyType(span)          => Some(AST.Type(AST.IntLit(1, None), span))
      case AST.StringType(span)       => Some(AST.Type(AST.IntLit(0, None), span))
      case AST.IntegerType(span)      => Some(AST.Type(AST.IntLit(0, None), span))
      case AST.NaturalType(span)      => Some(AST.Type(AST.IntLit(0, None), span))
      case AST.Type(level, _)         => Some(AST.TypeOmega(level, None))
      case AST.TypeOmega(level, span) => Some(AST.TypeOmega(level, span))
      case AST.Tuple(elems, span) =>
        val elemTys = elems.map(infer(_, env))
        if elemTys.forall(_.isDefined) then Some(AST.Tuple(elemTys.flatten, span)) else None
      case AST.ListLit(elems, span) =>
        val elemTys = elems.map(infer(_, env))
        if elemTys.nonEmpty && elemTys.forall(_.isDefined) then
          val headTy = elemTys.head.get
          if elemTys.flatten.forall(t => normalizeType(t) == normalizeType(headTy)) then Some(AST.ListType(headTy, span)) else None
        else None
      case AST.ListType(elem, span) =>
        infer(elem, env).map(_ => AST.Type(AST.IntLit(0, None), span))
      case AST.Ann(expr, ty, _) =>
        if check(expr, ty, env) then Some(ty) else None
      case AST.App(func, args, _, span) =>
        infer(func, env) match
          case Some(AST.Pi(teles, resTy, _, _)) =>
            val params = teles.flatMap(_.params)
            if params.length != args.length then None
            else
              val argTysOk = args.zip(params).forall { case (arg, param) => check(arg.value, param.ty, env) }
              if argTysOk then
                val subst = params.map(_.id).zip(args.map(_.value)).toMap
                Some(normalizeType(substituteInType(resTy, subst)))
              else None
          case _ => None
      case AST.Lam(_, _, _) => None // cannot infer lambda without expected type
      case AST.Pi(teles, res, effs, span) =>
        val env1 = teles.foldLeft(env)((e, tel) => tel.params.foldLeft(e)((acc, p) => acc + (p.id -> AST.Type(AST.IntLit(0, None), None))))
        if teles.forall(_.params.forall(p => infer(p.ty, env1).isDefined)) && infer(res, env1).isDefined then
          Some(AST.Type(AST.IntLit(0, None), span))
        else None
      case AST.Let(id, _, ty, value, body, _) =>
        val vTy = ty.orElse(infer(value, env))
        vTy.flatMap(vt => infer(body, env + (id -> vt)))
      case AST.Block(elems, tail, _) =>
        val env1 = elems.foldLeft(env)((e, stmt) => extendEnvWithStmt(e, stmt))
        if elems.forall(stmt => checkStmt(stmt, env1)) then infer(tail, env1) else None
      case AST.MetaCell(_, _) => None

  private def checkStmt(stmt: StmtAST, env: Env): Boolean =
    stmt match
      case StmtAST.ExprStmt(expr, _) => infer(expr, env).isDefined
      case StmtAST.Def(_, _, teles, resTy, body, _) =>
        val paramEnv = teles.foldLeft(env)((e, tel) => tel.params.foldLeft(e)((acc, p) => acc + (p.id -> p.ty)))
        resTy match
          case Some(rt) => check(body, rt, paramEnv)
          case None     => infer(body, paramEnv).isDefined
      case StmtAST.Pkg(_, body, _) => infer(body, env).isDefined

  private def extendEnvWithStmt(env: Env, stmt: StmtAST): Env =
    stmt match
      case StmtAST.Def(id, _, teles, resTy, _, _) =>
        val resultTy = resTy.getOrElse(AST.Type(AST.IntLit(0, None), None))
        val pi = AST.Pi(teles, resultTy, Vector.empty, None)
        env + (id -> pi)
      case _ => env
