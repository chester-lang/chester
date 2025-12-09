package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, Param, StmtAST, Telescope}
import chester.utils.HoldNotReadable

/** Lightweight core type checker used to validate elaborated ASTs and normalize types. */
object CoreTypeChecker:
  private type Env = Map[chester.uniqid.UniqidOf[AST], AST]
  private type RecordEnv = Map[chester.uniqid.UniqidOf[AST], (String, Vector[chester.core.Param])]

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
      case AST.TupleType(elems, span) =>
        AST.TupleType(elems.map(normalizeType), span)
      case AST.ListLit(elems, span) => AST.ListLit(elems.map(normalizeType), span)
      case AST.Let(id, name, ty, value, body, span) =>
        AST.Let(id, name, ty.map(normalizeType), normalizeType(value), normalizeType(body), span)
      case AST.Ann(expr, ty, span)      => AST.Ann(normalizeType(expr), normalizeType(ty), span)
      case AST.Block(elems, tail, span) => AST.Block(elems.map(normalizeTypeStmt), normalizeType(tail), span)
      case AST.RecordCtor(id, name, args, span) =>
        AST.RecordCtor(id, name, args.map(normalizeType), span)
      case AST.FieldAccess(target, field, span) =>
        AST.FieldAccess(normalizeType(target), field, span)
      case other                        => other

  private def normalizeTypeStmt(stmt: StmtAST): StmtAST =
    stmt match
      case StmtAST.ExprStmt(expr, span) => StmtAST.ExprStmt(normalizeType(expr), span)
      case StmtAST.Def(id, name, teles, resTy, body, span) =>
        val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))))
        StmtAST.Def(id, name, nTeles, resTy.map(normalizeType), normalizeType(body), span)
      case StmtAST.Record(id, name, fields, span) =>
        val normFields = fields.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))
        StmtAST.Record(id, name, normFields, span)
      case StmtAST.Pkg(name, body, span) => StmtAST.Pkg(name, normalizeType(body), span)

  /** Entry point to check whether an AST is well-typed according to a simple dependent type checker. */
  def typeChecks(ast: AST): Boolean = infer(ast, Map.empty, Map.empty).isDefined

  private def check(ast: AST, expected: AST, env: Env, records: RecordEnv): Boolean =
    infer(ast, env, records).exists(t => normalizeType(t) == normalizeType(expected))

  /** Extract sort (Type vs TypeΩ) and its level for a type-of-type. */
  private case class Sort(isOmega: Boolean, level: Int)
  private def sortOfType(ty: AST): Option[Sort] =
    normalizeType(ty) match
      case AST.Type(AST.LevelLit(n, _), _)      => Some(Sort(isOmega = false, n.toInt))
      case AST.TypeOmega(AST.LevelLit(n, _), _) => Some(Sort(isOmega = true, n.toInt))
      // Backwards compat: allow integer level literals
      case AST.Type(AST.IntLit(n, _), _)      => Some(Sort(isOmega = false, n.toInt))
      case AST.TypeOmega(AST.IntLit(n, _), _) => Some(Sort(isOmega = true, n.toInt))
      case _                                  => None

  private def infer(ast: AST, env: Env, records: RecordEnv): Option[AST] =
    ast match
      case AST.Ref(id, _, _)     => env.get(id)
      case AST.StringLit(_, _)   => Some(AST.StringType(None))
      case AST.IntLit(_, _)      => Some(AST.IntegerType(None))
      case AST.LevelLit(_, span) => Some(AST.LevelType(span))
      case AST.AnyType(span)     => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.StringType(span)  => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.IntegerType(span) => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.NaturalType(span) => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.LevelType(span)   => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.Type(level, span) =>
        val lvl = level match
          case AST.IntLit(n, sp) => AST.LevelLit(n, sp)
          case other             => other
        infer(lvl, env, records) match
          case Some(AST.LevelType(_)) => Some(AST.TypeOmega(lvl, span))
          case _                      => None
      case AST.TypeOmega(level, span) =>
        val lvl = level match
          case AST.IntLit(n, sp) => AST.LevelLit(n, sp)
          case other             => other
        infer(lvl, env, records) match
          case Some(AST.LevelType(_)) =>
            normalizeType(lvl) match
              case AST.LevelLit(n, _) => Some(AST.TypeOmega(AST.LevelLit(n + 1, None), span))
              case _                  => Some(AST.TypeOmega(lvl, span))
          case _ => None
      case AST.Tuple(elems, span) =>
        if elems.isEmpty then Some(AST.TupleType(Vector.empty, span))
        else
          val elemTys = elems.map(infer(_, env, records))
          if elemTys.forall(_.isDefined) then Some(AST.TupleType(elemTys.flatten, span)) else None
      case AST.TupleType(elems, span) =>
        val elemTys = elems.map(infer(_, env, records))
        if elemTys.forall(_.isDefined) then
          val sorts = elemTys.flatten.flatMap(sortOfType)
          val maxLevel = sorts.map(_.level).maxOption.getOrElse(0)
          val isOmega = sorts.exists(_.isOmega)
          if isOmega then Some(AST.TypeOmega(AST.LevelLit(maxLevel, None), span))
          else Some(AST.Type(AST.LevelLit(maxLevel, None), span))
        else None
      case AST.ListLit(elems, span) =>
        val elemTys = elems.map(infer(_, env, records))
        if elemTys.nonEmpty && elemTys.forall(_.isDefined) then
          val headTy = elemTys.head.get
          if elemTys.flatten.forall(t => normalizeType(t) == normalizeType(headTy)) then Some(AST.ListType(headTy, span)) else None
        else None
      case AST.ListType(elem, span) =>
        infer(elem, env, records).map(_ => AST.Type(AST.LevelLit(0, None), span))
      case AST.Ann(expr, ty, _) =>
        if check(expr, ty, env, records) then Some(ty) else None
      case AST.App(func, args, _, span) =>
        infer(func, env, records) match
          case Some(AST.Pi(teles, resTy, _, _)) =>
            val params = teles.flatMap(_.params)
            if params.length != args.length then None
            else
              val argTysOk = args.zip(params).forall { case (arg, param) => check(arg.value, param.ty, env, records) }
              if argTysOk then
                val subst = params.map(_.id).zip(args.map(_.value)).toMap
                Some(normalizeType(substituteInType(resTy, subst)))
              else None
          case _ => None
      case AST.Lam(_, _, _) => None // cannot infer lambda without expected type
      case AST.Pi(teles, res, effs, span) =>
        val env1 = teles.foldLeft(env)((e, tel) => tel.params.foldLeft(e)((acc, p) => acc + (p.id -> AST.Type(AST.LevelLit(0, None), None))))
        if teles.forall(_.params.forall(p => infer(p.ty, env1, records).isDefined)) && infer(res, env1, records).isDefined then
          Some(AST.Type(AST.LevelLit(0, None), span))
        else None
      case AST.Let(id, _, ty, value, body, _) =>
        val vTy = ty.orElse(infer(value, env, records))
        vTy.flatMap(vt => infer(body, env + (id -> vt), records))
      case AST.Block(elems, tail, _) =>
        val envRec = elems.foldLeft((env, records)) { case ((e, r), stmt) => (extendEnvWithStmt(e, stmt), extendRecordEnv(r, stmt)) }
        val env1 = envRec._1
        val rec1 = envRec._2
        if elems.forall(stmt => checkStmt(stmt, env1, rec1)) then infer(tail, env1, rec1) else None
      case AST.RecordTypeRef(id, name, span) =>
        records.get(id).map(_ => AST.Type(AST.LevelLit(0, None), span))
      case AST.RecordCtor(id, _, args, span) =>
        records.get(id) match
          case Some((recName, fields)) if fields.length == args.length =>
            val argsOk = args.zip(fields).forall { case (arg, field) => check(arg, field.ty, env, records) }
            if argsOk then Some(AST.RecordTypeRef(id, recName, span)) else None
          case _ => None
      case AST.FieldAccess(target, field, span) =>
        infer(target, env, records) match
          case Some(AST.RecordTypeRef(id, _, _)) =>
            records.get(id).flatMap { case (_, flds) => flds.find(_.name == field).map(_.ty) }
          case _ => None
      case AST.MetaCell(_, _) => None

  private def checkStmt(stmt: StmtAST, env: Env, records: RecordEnv): Boolean =
    stmt match
      case StmtAST.ExprStmt(expr, _) => infer(expr, env, records).isDefined
      case StmtAST.Def(_, _, teles, resTy, body, _) =>
        val paramEnv = teles.foldLeft(env)((e, tel) => tel.params.foldLeft(e)((acc, p) => acc + (p.id -> p.ty)))
        resTy match
          case Some(rt) => check(body, rt, paramEnv, records)
          case None     => infer(body, paramEnv, records).isDefined
      case StmtAST.Record(_, _, _, _) => true
      case StmtAST.Pkg(_, body, _)    => infer(body, env, records).isDefined

  private def extendEnvWithStmt(env: Env, stmt: StmtAST): Env =
    stmt match
      case StmtAST.Def(id, _, teles, resTy, _, _) =>
        val resultTy = resTy.getOrElse(AST.Type(AST.LevelLit(0, None), None))
        val pi = AST.Pi(teles, resultTy, Vector.empty, None)
        env + (id -> pi)
      case _ => env

  private def extendRecordEnv(records: RecordEnv, stmt: StmtAST): RecordEnv =
    stmt match
      case StmtAST.Record(id, name, fields, _) => records + (id -> (name, fields))
      case _                                   => records
