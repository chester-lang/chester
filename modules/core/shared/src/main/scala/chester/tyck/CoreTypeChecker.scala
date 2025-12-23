package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.utils.HoldNotReadable
import chester.tyck.ElabContext
import chester.error.{Reporter, VectorReporter}

/** Lightweight core type checker used to validate elaborated ASTs and normalize types. */
object CoreTypeChecker:
  private type Env = Map[chester.uniqid.UniqidOf[AST], AST]
  private type RecordEnv = Map[chester.uniqid.UniqidOf[AST], (String, Vector[chester.core.Param])]
  private type EnumEnv = Map[chester.uniqid.UniqidOf[AST], (String, Vector[EnumCase], Vector[Param])]
  private val builtinTypes: Map[String, AST] = ElabContext.defaultBuiltinTypes

  private def substituteInType(ast: AST, subst: Map[chester.uniqid.UniqidOf[AST], AST]): AST = {
    ast match
      case AST.Ref(id, _, _) => subst.getOrElse(id, ast)
      case AST.App(func, args, imp, span) =>
        AST.App(substituteInType(func, subst), args.map(a => Arg(substituteInType(a.value, subst), a.implicitness)), imp, span)
      case AST.Pi(teles, res, effs, span) =>
        val newTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = substituteInType(p.ty, subst)))))
        AST.Pi(newTeles, substituteInType(res, subst), effs, span)
      case AST.Lam(teles, body, span) =>
        val newTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = substituteInType(p.ty, subst)))))
        AST.Lam(newTeles, substituteInType(body, subst), span)
      case AST.Tuple(elems, span)     => AST.Tuple(elems.map(substituteInType(_, subst)), span)
      case AST.TupleType(elems, span) => AST.TupleType(elems.map(substituteInType(_, subst)), span)
      case AST.ListType(elem, span)   => AST.ListType(substituteInType(elem, subst), span)
      case AST.ListLit(elems, span)   => AST.ListLit(elems.map(substituteInType(_, subst)), span)
      case AST.RecordCtor(id, name, args, span) =>
        AST.RecordCtor(id, name, args.map(substituteInType(_, subst)), span)
      case AST.EnumCtor(enumId, caseId, enumName, caseName, args, span) =>
        AST.EnumCtor(enumId, caseId, enumName, caseName, args.map(substituteInType(_, subst)), span)
      case AST.Ann(expr, ty, span) => AST.Ann(substituteInType(expr, subst), substituteInType(ty, subst), span)
      case AST.Let(id, name, ty, value, body, span) =>
        AST.Let(id, name, ty.map(substituteInType(_, subst)), substituteInType(value, subst), substituteInType(body, subst), span)
      case AST.Block(elems, tail, span) =>
        AST.Block(elems.map(substituteInTypeStmt(_, subst)), substituteInType(tail, subst), span)
      case other => other
  }

  private def substituteInTypeStmt(stmt: StmtAST, subst: Map[chester.uniqid.UniqidOf[AST], AST]): StmtAST = {
    stmt match
      case StmtAST.ExprStmt(expr, span) => StmtAST.ExprStmt(substituteInType(expr, subst), span)
      case StmtAST.JSImport(id, localName, modulePath, kind, ty, span) =>
        StmtAST.JSImport(id, localName, modulePath, kind, substituteInType(ty, subst), span)
      case StmtAST.Def(id, name, teles, resTy, body, span) =>
        val newTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = substituteInType(p.ty, subst)))))
        StmtAST.Def(id, name, newTeles, resTy.map(substituteInType(_, subst)), substituteInType(body, subst), span)
      case StmtAST.Record(id, name, fields, span) =>
        val newFields = fields.map(p => p.copy(ty = substituteInType(p.ty, subst)))
        StmtAST.Record(id, name, newFields, span)
      case StmtAST.Enum(id, name, tps, cases, span) =>
        val newTps = tps.map(p => p.copy(ty = substituteInType(p.ty, subst)))
        val newCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = substituteInType(p.ty, subst)))))
        StmtAST.Enum(id, name, newTps, newCases, span)
      case StmtAST.Coenum(id, name, tps, cases, span) =>
        val newTps = tps.map(p => p.copy(ty = substituteInType(p.ty, subst)))
        val newCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = substituteInType(p.ty, subst)))))
        StmtAST.Coenum(id, name, newTps, newCases, span)
      case StmtAST.Pkg(name, body, span) => StmtAST.Pkg(name, substituteInType(body, subst), span)
  }

  /** Normalize a type AST with shallow beta-reduction of type-level lambdas/applications. */
  def normalizeType(ast: AST): AST = {
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
      case AST.EnumCtor(enumId, caseId, enumName, caseName, args, span) =>
        AST.EnumCtor(enumId, caseId, enumName, caseName, args.map(normalizeType), span)
      case AST.EnumCaseRef(_, _, _, _, _) => ast
      case AST.EnumTypeRef(_, _, _)       => ast
      case other                          => other
  }

  private def normalizeTypeStmt(stmt: StmtAST): StmtAST = {
    stmt match
      case StmtAST.ExprStmt(expr, span) => StmtAST.ExprStmt(normalizeType(expr), span)
      case StmtAST.JSImport(id, localName, modulePath, kind, ty, span) =>
        StmtAST.JSImport(id, localName, modulePath, kind, normalizeType(ty), span)
      case StmtAST.Def(id, name, teles, resTy, body, span) =>
        val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))))
        StmtAST.Def(id, name, nTeles, resTy.map(normalizeType), normalizeType(body), span)
      case StmtAST.Record(id, name, fields, span) =>
        val normFields = fields.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))
        StmtAST.Record(id, name, normFields, span)
      case StmtAST.Enum(id, name, typeParams, cases, span) =>
        val normTypeParams = typeParams.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))
        val normCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))))
        StmtAST.Enum(id, name, normTypeParams, normCases, span)
      case StmtAST.Coenum(id, name, typeParams, cases, span) =>
        val normTypeParams = typeParams.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))
        val normCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))))
        StmtAST.Coenum(id, name, normTypeParams, normCases, span)
      case StmtAST.Pkg(name, body, span) => StmtAST.Pkg(name, normalizeType(body), span)
  }

  private def eraseSpans(ast: AST): AST = {
    ast match
      case AST.Ref(id, name, _)      => AST.Ref(id, name, None)
      case AST.Tuple(elems, _)       => AST.Tuple(elems.map(eraseSpans), None)
      case AST.ListLit(elems, _)     => AST.ListLit(elems.map(eraseSpans), None)
      case AST.Block(elems, tail, _) => AST.Block(elems.map(eraseSpansStmt), eraseSpans(tail), None)
      case AST.StringLit(v, _)       => AST.StringLit(v, None)
      case AST.IntLit(v, _)          => AST.IntLit(v, None)
      case AST.NaturalLit(v, _)      => AST.NaturalLit(v, None)
      case AST.LevelLit(v, _)        => AST.LevelLit(v, None)
      case AST.Type(level, _)        => AST.Type(eraseSpans(level), None)
      case AST.TypeOmega(level, _)   => AST.TypeOmega(eraseSpans(level), None)
      case AST.AnyType(_)            => AST.AnyType(None)
      case AST.StringType(_)         => AST.StringType(None)
      case AST.NaturalType(_)        => AST.NaturalType(None)
      case AST.IntegerType(_)        => AST.IntegerType(None)
      case AST.LevelType(_)          => AST.LevelType(None)
      case AST.TupleType(elems, _)   => AST.TupleType(elems.map(eraseSpans), None)
      case AST.ListType(elem, _)     => AST.ListType(eraseSpans(elem), None)
      case AST.Pi(teles, res, effs, _) =>
        val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))))
        AST.Pi(nTeles, eraseSpans(res), effs, None)
      case AST.Lam(teles, body, _) =>
        val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))))
        AST.Lam(nTeles, eraseSpans(body), None)
      case AST.App(func, args, implicitArgs, _) =>
        AST.App(eraseSpans(func), args.map(a => Arg(eraseSpans(a.value), a.implicitness)), implicitArgs, None)
      case AST.Let(id, name, ty, value, body, _) =>
        AST.Let(id, name, ty.map(eraseSpans), eraseSpans(value), eraseSpans(body), None)
      case AST.Ann(expr, ty, _) => AST.Ann(eraseSpans(expr), eraseSpans(ty), None)
      case AST.RecordTypeRef(id, name, _) =>
        AST.RecordTypeRef(id, name, None)
      case AST.RecordCtor(id, name, args, _) =>
        AST.RecordCtor(id, name, args.map(eraseSpans), None)
      case AST.FieldAccess(target, field, _) =>
        AST.FieldAccess(eraseSpans(target), field, None)
      case AST.EnumTypeRef(id, name, _) =>
        AST.EnumTypeRef(id, name, None)
      case AST.EnumCaseRef(enumId, caseId, enumName, caseName, _) =>
        AST.EnumCaseRef(enumId, caseId, enumName, caseName, None)
      case AST.EnumCtor(enumId, caseId, enumName, caseName, args, _) =>
        AST.EnumCtor(enumId, caseId, enumName, caseName, args.map(eraseSpans), None)
      case AST.MetaCell(cell, _) => AST.MetaCell(cell, None)
  }

  private def eraseSpansStmt(stmt: StmtAST): StmtAST = stmt match
    case StmtAST.ExprStmt(expr, _) => StmtAST.ExprStmt(eraseSpans(expr), None)
    case StmtAST.JSImport(id, localName, modulePath, kind, ty, _) =>
      StmtAST.JSImport(id, localName, modulePath, kind, eraseSpans(ty), None)
    case StmtAST.Def(id, name, teles, resTy, body, _) =>
      val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))))
      StmtAST.Def(id, name, nTeles, resTy.map(eraseSpans), eraseSpans(body), None)
    case StmtAST.Record(id, name, fields, _) =>
      val nFields = fields.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))
      StmtAST.Record(id, name, nFields, None)
    case StmtAST.Enum(id, name, typeParams, cases, _) =>
      val nTypeParams = typeParams.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))
      val nCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))))
      StmtAST.Enum(id, name, nTypeParams, nCases, None)
    case StmtAST.Coenum(id, name, typeParams, cases, _) =>
      val nTypeParams = typeParams.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))
      val nCases = cases.map(c => c.copy(params = c.params.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))))
      StmtAST.Coenum(id, name, nTypeParams, nCases, None)
    case StmtAST.Pkg(name, body, _) =>
      StmtAST.Pkg(name, eraseSpans(body), None)

  private def hasMeta(ast: AST): Boolean = ast match
    case AST.MetaCell(_, _)                => true
    case AST.Ref(_, _, _)                  => false
    case AST.Tuple(elems, _)               => elems.exists(hasMeta)
    case AST.ListLit(elems, _)             => elems.exists(hasMeta)
    case AST.Block(elems, tail, _)         => elems.exists(hasMetaStmt) || hasMeta(tail)
    case AST.TupleType(elems, _)           => elems.exists(hasMeta)
    case AST.ListType(elem, _)             => hasMeta(elem)
    case AST.Pi(teles, res, _, _)          => teles.exists(t => t.params.exists(p => hasMeta(p.ty))) || hasMeta(res)
    case AST.Lam(teles, body, _)           => teles.exists(t => t.params.exists(p => hasMeta(p.ty))) || hasMeta(body)
    case AST.App(func, args, _, _)         => hasMeta(func) || args.exists(a => hasMeta(a.value))
    case AST.Let(_, _, ty, value, body, _) => ty.exists(hasMeta) || hasMeta(value) || hasMeta(body)
    case AST.Ann(expr, ty, _)              => hasMeta(expr) || hasMeta(ty)
    case AST.RecordCtor(_, _, args, _)     => args.exists(hasMeta)
    case AST.EnumCtor(_, _, _, _, args, _) => args.exists(hasMeta)
    case AST.FieldAccess(target, _, _)     => hasMeta(target)
    case AST.Type(level, _)                => hasMeta(level)
    case AST.TypeOmega(level, _)           => hasMeta(level)
    case AST.EnumCaseRef(_, _, _, _, _)    => false
    case AST.EnumTypeRef(_, _, _)          => false
    case AST.RecordTypeRef(_, _, _)        => false
    case AST.StringLit(_, _)               => false
    case AST.IntLit(_, _)                  => false
    case AST.NaturalLit(_, _)              => false
    case AST.LevelLit(_, _)                => false
    case AST.AnyType(_)                    => false
    case AST.StringType(_)                 => false
    case AST.NaturalType(_)                => false
    case AST.IntegerType(_)                => false
    case AST.LevelType(_)                  => false

  private def hasMetaStmt(stmt: StmtAST): Boolean = stmt match
    case StmtAST.ExprStmt(expr, _)           => hasMeta(expr)
    case StmtAST.JSImport(_, _, _, _, ty, _) => hasMeta(ty)
    case StmtAST.Def(_, _, teles, resTy, body, _) =>
      teles.exists(t => t.params.exists(p => hasMeta(p.ty))) || resTy.exists(hasMeta) || hasMeta(body)
    case StmtAST.Record(_, _, fields, _) => fields.exists(f => hasMeta(f.ty))
    case StmtAST.Enum(_, _, tps, cases, _) =>
      tps.exists(tp => hasMeta(tp.ty)) || cases.exists(c => c.params.exists(p => hasMeta(p.ty)))
    case StmtAST.Coenum(_, _, tps, cases, _) =>
      tps.exists(tp => hasMeta(tp.ty)) || cases.exists(c => c.params.exists(p => hasMeta(p.ty)))
    case StmtAST.Pkg(_, body, _) => hasMeta(body)

  private def sameType(a: AST, b: AST): Boolean = {
    if hasMeta(a) || hasMeta(b) then true
    else eraseSpans(normalizeType(a)) == eraseSpans(normalizeType(b))
  }

  /** Entry point to check whether an AST is well-typed according to a simple dependent type checker. */
  def typeCheck(ast: AST)(using Reporter[ElabProblem]): Unit = {
    val result = infer(ast, Map.empty, Map.empty, Map.empty)
    if result.isEmpty then summon[Reporter[ElabProblem]].report(ElabProblem.UnboundVariable("Core type check failed", ast.span))
  }

  /** Boolean wrapper for legacy call sites; collects problems into a VectorReporter. */
  def typeChecks(ast: AST): Boolean = {
    given vr: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()
    typeCheck(ast)(using vr)
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
        if sameType(actual, expected) then true
        else {
          summon[Reporter[ElabProblem]].report(ElabProblem.TypeMismatch(expected, actual, ast.span))
          false
        }
      case None =>
        summon[Reporter[ElabProblem]].report(ElabProblem.UnboundVariable("Unable to infer type", ast.span))
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
    normalizeType(ty) match
      case AST.Type(AST.LevelLit(n, _), _)      => Some(Sort(isOmega = false, n.toInt))
      case AST.TypeOmega(AST.LevelLit(n, _), _) => Some(Sort(isOmega = true, n.toInt))
      // Backwards compat: allow integer level literals
      case AST.Type(AST.IntLit(n, _), _)      => Some(Sort(isOmega = false, n.toInt))
      case AST.TypeOmega(AST.IntLit(n, _), _) => Some(Sort(isOmega = true, n.toInt))
      case _                                  => None
  }

  private def infer(ast: AST, env: Env, records: RecordEnv, enums: EnumEnv)(using Reporter[ElabProblem]): Option[AST] = {
    ast match
      case AST.Ref(id, name, _) =>
        env.get(id).orElse(builtinTypes.get(name))
      case AST.StringLit(_, _)   => Some(AST.StringType(None))
      case AST.IntLit(_, _)      => Some(AST.IntegerType(None))
      case AST.NaturalLit(_, _)  => Some(AST.NaturalType(None))
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
        infer(lvl, env, records, enums) match
          case Some(AST.LevelType(_)) => Some(AST.TypeOmega(lvl, span))
          case _                      => None
      case AST.TypeOmega(level, span) =>
        val lvl = level match
          case AST.IntLit(n, sp) => AST.LevelLit(n, sp)
          case other             => other
        infer(lvl, env, records, enums) match
          case Some(AST.LevelType(_)) =>
            normalizeType(lvl) match
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
          if elemTys.flatten.forall(t => sameType(t, headTy)) then Some(AST.ListType(headTy, span)) else None
        else None
      case AST.ListType(elem, span) =>
        infer(elem, env, records, enums).map(_ => AST.Type(AST.LevelLit(0, None), span))
      case AST.Ann(expr, ty, _) =>
        if ensureType(expr, ty, env, records, enums) then Some(ty) else None
      case AST.App(func, args, _, span) =>
        val funcTyOpt = infer(func, env, records, enums)
        funcTyOpt match
          case Some(AST.Pi(teles, resTy, _, _)) =>
            val params = teles.flatMap(_.params)
            if params.length != args.length then None
            else {
              val argTysOk = args.zip(params).forall { case (arg, param) => ensureType(arg.value, param.ty, env, records, enums) }
              if argTysOk then
                val subst = params.map(_.id).zip(args.map(_.value)).toMap
                Some(normalizeType(substituteInType(resTy, subst)))
              else None
            }
          case _ =>
            func match
              case AST.Lam(teles, body, _) if teles.flatMap(_.params).length == args.length =>
                val params = teles.flatMap(_.params)
                val argsOk = args.zip(params).forall { case (arg, param) => ensureType(arg.value, param.ty, env, records, enums) }
                if argsOk then
                  val subst = params.map(_.id).zip(args.map(_.value)).toMap
                  Some(normalizeType(substituteInType(body, subst)))
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
        val envRecEnum = elems.foldLeft((env, records, enums)) { case ((e, r, en), stmt) =>
          (extendEnvWithStmt(e, stmt), extendRecordEnv(r, stmt), extendEnumEnv(en, stmt))
        }
        val env1 = envRecEnum._1
        val rec1 = envRecEnum._2
        val en1 = envRecEnum._3
        elems.foreach(stmt => checkStmt(stmt, env1, rec1, en1))
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
                  val expected = substituteInType(param.ty, subst)
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
      case StmtAST.Def(_, _, teles, resTy, body, _) =>
        val paramEnv = teles.foldLeft(env)((e, tel) => tel.params.foldLeft(e)((acc, p) => acc + (p.id -> p.ty)))
        resTy match
          case Some(rt) => check(body, rt, paramEnv, records, enums)
          case None     => infer(body, paramEnv, records, enums)
      case StmtAST.Record(_, _, _, _)    => ()
      case StmtAST.Enum(_, _, _, _, _)   => ()
      case StmtAST.Coenum(_, _, _, _, _) => ()
      case StmtAST.Pkg(_, body, _)       => infer(body, env, records, enums)
  }

  private def extendEnvWithStmt(env: Env, stmt: StmtAST): Env = {
    stmt match
      case StmtAST.JSImport(id, _, _, _, ty, _) =>
        env + (id -> ty)
      case StmtAST.Def(id, _, teles, resTy, _, _) =>
        val resultTy = resTy.getOrElse(AST.Type(AST.LevelLit(0, None), None))
        val pi = AST.Pi(teles, resultTy, Vector.empty, None)
        env + (id -> pi)
      case _ => env
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
