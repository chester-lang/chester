package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.utils.HoldNotReadable
import chester.tyck.ElabContext
import chester.uniqid.UniqidOf
import chester.error.{Reporter, VectorReporter}

/** Lightweight core type checker used to validate elaborated ASTs and normalize types. */
object CoreTypeChecker:
  private type Env = Map[chester.uniqid.UniqidOf[AST], AST]
  private type RecordEnv = Map[chester.uniqid.UniqidOf[AST], (String, Vector[chester.core.Param])]
  private type EnumEnv = Map[chester.uniqid.UniqidOf[AST], (String, Vector[EnumCase], Vector[Param])]
  private val builtinTypes: Map[String, AST] = ElabContext.defaultBuiltinTypes

  def typeCheck(ast: AST, initialEnv: Map[UniqidOf[AST], AST] = Map.empty)(using Reporter[ElabProblem]): Unit = {
    val result = infer(ast, initialEnv, Map.empty, Map.empty)
    if result.isEmpty then summon[Reporter[ElabProblem]].report(ElabProblem.UnboundVariable("Core type check failed", ast.span))
  }

  /** Boolean wrapper for legacy call sites; collects problems into a VectorReporter. */
  def typeChecks(ast: AST, initialEnv: Map[UniqidOf[AST], AST] = Map.empty): Boolean = {
    given vr: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()
    typeCheck(ast, initialEnv)(using vr)
    vr.getReports.isEmpty
  }

  private def ensureType(
      ast: AST,
      expected: AST,
      env: Env,
      records: RecordEnv,
      enums: EnumEnv
  )(using Reporter[ElabProblem]): Boolean = {
    infer(ast, env, records, enums) match
      case Some(actual) =>
        if ASTOps.sameType(actual, expected) then true
        else {
          summon[Reporter[ElabProblem]].report(ElabProblem.TypeMismatch(expected, actual, ast.span))
          false
        }
      case None =>
        summon[Reporter[ElabProblem]].report(ElabProblem.UnboundVariable(s"Unable to infer type of AST: $ast", ast.span))
        false
  }

  private def check(
      ast: AST,
      expected: AST,
      env: Env,
      records: RecordEnv,
      enums: EnumEnv
  )(using Reporter[ElabProblem]): Unit =
    ensureType(ast, expected, env, records, enums)

  /** Extract sort (Type vs TypeΩ) and its level for a type-of-type. */
  private case class Sort(isOmega: Boolean, level: Int)
  private def sortOfType(ty: AST): Option[Sort] = {
    ASTOps.normalizeType(ty) match
      case AST.Type(AST.LevelLit(n, _), _)      => Some(Sort(isOmega = false, n.toInt))
      case AST.TypeOmega(AST.LevelLit(n, _), _) => Some(Sort(isOmega = true, n.toInt))
      // Backwards compat: allow integer level literals
      case AST.Type(AST.IntLit(n, _), _)      => Some(Sort(isOmega = false, n.toInt))
      case AST.TypeOmega(AST.IntLit(n, _), _) => Some(Sort(isOmega = true, n.toInt))
      case _                                  => None
  }

  private def infer(ast: AST, env: Env, records: RecordEnv, enums: EnumEnv)(using Reporter[ElabProblem]): Option[AST] = {
    ast match
      case AST.ExtensionAccess(target, _, _, _, _) => None
      case AST.Handle(action, _, _, _) => infer(action, env, records, enums) // return type of handled computation
      case AST.Do(op, _, _) => infer(op, env, records, enums) // type of the op being performed
      case AST.Ref(id, name, _) =>
        env.get(id).orElse(builtinTypes.get(name))
      case AST.StringLit(_, _)   => Some(AST.StringType(None))
      case AST.IntLit(_, _)      => Some(AST.IntegerType(None))
      case AST.NaturalLit(_, _)  => Some(AST.NaturalType(None))
      case AST.LevelLit(_, span) => Some(AST.LevelType(span))
      case AST.AnyType(span)     => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.BoolType(span)    => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.StringType(span)  => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.IntegerType(span) => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.NaturalType(span) => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.LevelType(span)   => Some(AST.Type(AST.LevelLit(0, None), span))
      case AST.Type(level, span) =>
        val lvl = level match
          case AST.IntLit(n, sp) => AST.LevelLit(n, sp)
          case other             => other
        infer(lvl, env, records, enums) match
          case Some(AST.LevelType(_)) => Some(AST.TypeOmega(lvl, span))
          case _                      => None
      case AST.TypeOmega(level, span) =>
        val lvl = level match
          case AST.IntLit(n, sp) => AST.LevelLit(n, sp)
          case other             => other
        infer(lvl, env, records, enums) match
          case Some(AST.LevelType(_)) =>
            ASTOps.normalizeType(lvl) match
              case AST.LevelLit(n, _) => Some(AST.TypeOmega(AST.LevelLit(n + 1, None), span))
              case _                  => Some(AST.TypeOmega(lvl, span))
          case _ => None
      case AST.Tuple(elems, span) =>
        if elems.isEmpty then Some(AST.TupleType(Vector.empty, span))
        else {
          val elemTys = elems.map(infer(_, env, records, enums))
          if elemTys.forall(_.isDefined) then Some(AST.TupleType(elemTys.flatten, span)) else None
        }
      case AST.TupleType(elems, span) =>
        val elemTys = elems.map(infer(_, env, records, enums))
        if elemTys.forall(_.isDefined) then
          val sorts = elemTys.flatten.flatMap(sortOfType)
          val maxLevel = sorts.map(_.level).maxOption.getOrElse(0)
          val isOmega = sorts.exists(_.isOmega)
          if isOmega then Some(AST.TypeOmega(AST.LevelLit(maxLevel, None), span))
          else Some(AST.Type(AST.LevelLit(maxLevel, None), span))
        else None
      case AST.ListLit(elems, span) =>
        val elemTys = elems.map(infer(_, env, records, enums))
        if elemTys.nonEmpty && elemTys.forall(_.isDefined) then
          val headTy = elemTys.head.get
          if elemTys.flatten.forall(t => ASTOps.sameType(t, headTy)) then Some(AST.ListType(headTy, span)) else None
        else None
      case AST.ListType(elem, span) =>
        infer(elem, env, records, enums).map(_ => AST.Type(AST.LevelLit(0, None), span))
      case AST.Ann(expr, ty, _) =>
        if ensureType(expr, ty, env, records, enums) then Some(ty) else None
      case AST.App(func, args, _, span) =>
        val funcTyOpt = infer(func, env, records, enums)
        funcTyOpt match
          case Some(AST.Pi(teles, resTy, effs, piSpan)) if teles.nonEmpty =>
            val currentTele = teles.head
            val params = currentTele.params
            val restParamOpt = params.lastOption.flatMap { param =>
              ASTOps.normalizeType(param.ty) match
                case AST.ListType(elem, _) => Some((param, elem))
                case _                     => None
            }
            val fixedParams = restParamOpt.map(_ => params.dropRight(1)).getOrElse(params)
            val arityOk = restParamOpt match
              case Some(_) => args.length >= fixedParams.length
              case None    => args.length == params.length

            if !arityOk then

              None
            else {
              val fixedArgsOk = args.take(fixedParams.length).zip(fixedParams).forall { case (arg, param) => ensureType(arg.value, param.ty, env, records, enums) }
              val restArgsOk = restParamOpt match
                case Some((_, elemTy)) =>
                  args.drop(fixedParams.length).forall { arg => ensureType(arg.value, elemTy, env, records, enums) }
                case None => true

              if fixedArgsOk && restArgsOk then
                val restArgForSubst = restParamOpt.map { _ =>
                  val restArgs = args.drop(fixedParams.length).map(_.value)
                  AST.ListLit(restArgs, span)
                }
                val paramsForSubst = fixedParams ++ restParamOpt.map(_._1).toVector
                val argsForSubst = args.take(fixedParams.length).map(_.value) ++ restArgForSubst.toVector
                val subst = paramsForSubst.map(_.id).zip(argsForSubst).toMap
                
                val substitutedTeles = teles.tail.map { tel =>
                  tel.copy(params = tel.params.map { p =>
                    p.copy(ty = ASTOps.substituteInType(p.ty, subst), default = p.default.map(ASTOps.substituteInType(_, subst)))
                  })
                }
                val substitutedResTy = ASTOps.substituteInType(resTy, subst)
                
                if substitutedTeles.nonEmpty then
                  Some(ASTOps.normalizeType(AST.Pi(substitutedTeles, substitutedResTy, effs, piSpan)))
                else
                  Some(ASTOps.normalizeType(substitutedResTy))
              else None
            }
          case other =>
            func match
              case AST.Lam(teles, body, _) if teles.flatMap(_.params).length == args.length =>
                val params = teles.flatMap(_.params)
                val argsOk = args.zip(params).forall { case (arg, param) => ensureType(arg.value, param.ty, env, records, enums) }
                if argsOk then
                  val subst = params.map(_.id).zip(args.map(_.value)).toMap
                  Some(ASTOps.normalizeType(ASTOps.substituteInType(body, subst)))
                else None
              case _ => None
      case AST.Lam(teles, body, span) =>
        val env1 = teles.foldLeft(env)((e, tel) => tel.params.foldLeft(e)((acc, p) => acc + (p.id -> p.ty)))
        infer(body, env1, records, enums).map(bodyTy => AST.Pi(teles, bodyTy, Vector.empty, span))
      case AST.Pi(teles, res, effs, span) =>
        val env1 = teles.foldLeft(env)((e, tel) => tel.params.foldLeft(e)((acc, p) => acc + (p.id -> AST.Type(AST.LevelLit(0, None), None))))
        if teles.forall(_.params.forall(p => infer(p.ty, env1, records, enums).isDefined)) &&
          infer(res, env1, records, enums).isDefined
        then Some(AST.Type(AST.LevelLit(0, None), span))
        else None
      case AST.Let(id, _, ty, value, body, _) =>
        val vTy = ty.orElse(infer(value, env, records, enums))
        vTy.flatMap(vt => infer(body, env + (id -> vt), records, enums))
      case AST.Block(elems, tail, _) =>
        val rec1 = elems.foldLeft(records)(extendRecordEnv)
        val en1 = elems.foldLeft(enums)(extendEnumEnv)
        var env1 = elems.foldLeft(env)(extendEnvWithStmt)
        elems.foreach { stmt =>
          checkStmt(stmt, env1, rec1, en1)
          env1 = extendSequentialEnvWithStmt(env1, stmt, rec1, en1)
        }
        infer(tail, env1, rec1, en1)
      case AST.RecordTypeRef(id, name, span) =>
        records.get(id).map(_ => AST.Type(AST.LevelLit(0, None), span))
      case AST.RecordCtor(id, _, args, span) =>
        records.get(id) match
          case Some((recName, fields)) if fields.length == args.length =>
            val argsOk = args.zip(fields).forall { case (arg, field) => ensureType(arg, field.ty, env, records, enums) }
            if argsOk then Some(AST.RecordTypeRef(id, recName, span)) else None
          case _ => None
      case AST.FieldAccess(target, field, span) =>
        infer(target, env, records, enums) match
          case Some(AST.RecordTypeRef(id, _, _)) =>
            records.get(id).flatMap { case (_, flds) => flds.find(_.name == field).map(_.ty) }
          case _ => None
      case AST.EnumTypeRef(id, name, span) =>
        enums.get(id).map { case (_, _, typeParams) =>
          if typeParams.nonEmpty then
            val teleImplicitness = typeParams.headOption.map(_.implicitness).getOrElse(Implicitness.Explicit)
            val tele = Vector(Telescope(typeParams, teleImplicitness))
            AST.Pi(tele, AST.Type(AST.LevelLit(0, None), span), Vector.empty, span)
          else AST.Type(AST.LevelLit(0, None), span)
        }
      case AST.EnumCaseRef(enumId, caseId, _, _, span) =>
        enums.get(enumId).flatMap { case (enumName, cases, typeParams) =>
          cases.find(_.id == caseId).map { c =>
            val teleImplicitness = typeParams.headOption.map(_.implicitness).getOrElse(Implicitness.Explicit)
            val typeParamTele = if typeParams.nonEmpty then Vector(Telescope(typeParams, teleImplicitness)) else Vector.empty
            val caseTele = if c.params.nonEmpty then Vector(Telescope(c.params, Implicitness.Explicit)) else Vector.empty
            val typeParamRefs = typeParams.map(p => Arg(AST.Ref(p.id, p.name, span), Implicitness.Explicit))
            val enumType = {
              if typeParamRefs.nonEmpty then AST.App(AST.EnumTypeRef(enumId, enumName, span), typeParamRefs, implicitArgs = false, span)
              else AST.EnumTypeRef(enumId, enumName, span)
            }
            if typeParamTele.isEmpty && caseTele.isEmpty then enumType
            else AST.Pi(typeParamTele ++ caseTele, enumType, Vector.empty, span)
          }
        }
      case AST.EnumCtor(enumId, caseId, enumName, _, args, span) =>
        enums.get(enumId) match
          case Some((_, cases, typeParams)) =>
            cases.find(_.id == caseId) match
              case Some(ecase) if ecase.params.length + typeParams.length == args.length =>
                val allParams = typeParams ++ ecase.params
                var subst: Map[chester.uniqid.UniqidOf[AST], AST] = Map.empty
                val paramsOk = args.zip(allParams).forall { case (arg, param) =>
                  val expected = ASTOps.substituteInType(param.ty, subst)
                  val ok = ensureType(arg, expected, env, records, enums)
                  if ok then subst = subst + (param.id -> arg)
                  ok
                }
                if paramsOk then
                  val typeArgs = args.take(typeParams.length).map(arg => Arg(arg, Implicitness.Explicit))
                  val enumType = {
                    if typeArgs.nonEmpty then AST.App(AST.EnumTypeRef(enumId, enumName, span), typeArgs, implicitArgs = false, span)
                    else AST.EnumTypeRef(enumId, enumName, span)
                  }
                  Some(enumType)
                else None
              case _ => None
          case None => None
      case AST.MetaCell(_, span) => Some(AST.AnyType(span))
  }

  private def checkStmt(stmt: StmtAST, env: Env, records: RecordEnv, enums: EnumEnv)(using Reporter[ElabProblem]): Unit = {
    stmt match
      case StmtAST.ExprStmt(expr, _)           => infer(expr, env, records, enums)
      case StmtAST.JSImport(_, _, _, _, ty, _) =>
        // Treat as a value binding whose type must be well-formed.
        infer(ty, env, records, enums)
      case StmtAST.Def(_, _, teles, resTy, body, _, _) =>
        val paramEnv = teles.foldLeft(env)((e, tel) => tel.params.foldLeft(e)((acc, p) => acc + (p.id -> p.ty)))
        resTy match
          case Some(rt) => check(body, rt, paramEnv, records, enums)
          case None     => infer(body, paramEnv, records, enums)
      case StmtAST.Record(_, _, _, _)    => ()
      case StmtAST.Enum(_, _, _, _, _)   => ()
      case StmtAST.Coenum(_, _, _, _, _) => ()
      case StmtAST.Pkg(_, body, _)       => infer(body, env, records, enums)
      case StmtAST.Effect(_, _, _, _)    => ()
  }

  private def extendEnvWithStmt(env: Env, stmt: StmtAST): Env = stmt match
    case StmtAST.Def(id, _, teles, resTy, _, _, effects) =>
      val resultTy = resTy.getOrElse(AST.Type(AST.LevelLit(0, None), None))
      val pi = AST.Pi(teles, resultTy, effects, None)
      env + (id -> pi)
    case StmtAST.JSImport(id, _, _, _, ty, _) => env + (id -> ty)
    case StmtAST.Record(id, name, _, span) =>
      env + (id -> AST.RecordTypeRef(id, name, span))
    case StmtAST.Effect(_, _, operations, _) =>
      operations.foldLeft(env)((acc, op) => acc + (op.id -> op.ty))
    case _ => env

  private def extendSequentialEnvWithStmt(env: Env, stmt: StmtAST, records: RecordEnv, enums: EnumEnv)(using Reporter[ElabProblem]): Env = {
    stmt match
      case StmtAST.ExprStmt(AST.Let(id, _, ty, value, _, _), _) =>
        val bindingTy = ty.orElse(infer(value, env, records, enums))
        bindingTy.map(vt => env + (id -> vt)).getOrElse(env)
      case _ =>
        env
  }

  private def extendRecordEnv(records: RecordEnv, stmt: StmtAST): RecordEnv = {
    stmt match
      case StmtAST.Record(id, name, fields, _) => records + (id -> (name, fields))
      case _                                   => records
  }

  private def extendEnumEnv(enums: EnumEnv, stmt: StmtAST): EnumEnv = {
    stmt match
      case StmtAST.Enum(id, name, typeParams, cases, _)   => enums + (id -> (name, cases, typeParams))
      case StmtAST.Coenum(id, name, typeParams, cases, _) => enums + (id -> (name, cases, typeParams))
      case _                                              => enums
  }
