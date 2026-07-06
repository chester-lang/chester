package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.utils.HoldNotReadable
import chester.tyck.ElabContext
import chester.uniqid.UniqidOf
import chester.error.{Reporter, VectorReporter}

object ASTOps:
  def substituteInType(ast: AST, subst: Map[chester.uniqid.UniqidOf[AST], AST]): AST = {
    ast match
      case AST.ExtensionAccess(target, id, name, targetTy, span) => AST.ExtensionAccess(substituteInType(target, subst), id, name, targetTy, span)
      case AST.Ref(id, _, _) => subst.getOrElse(id, ast)
      case AST.App(func, args, imp, span) =>
        AST.App(substituteInType(func, subst), args.map(a => Arg(substituteInType(a.value, subst), a.implicitness, a.coeffect)), imp, span)
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

  def substituteInTypeStmt(stmt: StmtAST, subst: Map[chester.uniqid.UniqidOf[AST], AST]): StmtAST = {
    stmt match
      case StmtAST.ExprStmt(expr, span) => StmtAST.ExprStmt(substituteInType(expr, subst), span)
      case StmtAST.JSImport(id, localName, modulePath, kind, ty, span) =>
        StmtAST.JSImport(id, localName, modulePath, kind, substituteInType(ty, subst), span)
      case StmtAST.Def(id, name, teles, resTy, body, span, effects) =>
        val newTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = substituteInType(p.ty, subst)))))
        StmtAST.Def(id, name, newTeles, resTy.map(substituteInType(_, subst)), substituteInType(body, subst), span, effects)
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
      case StmtAST.Effect(id, name, ops, span) =>
        val newOps = ops.map(p => p.copy(ty = substituteInType(p.ty, subst)))
        StmtAST.Effect(id, name, newOps, span)
      case StmtAST.Pkg(name, body, span) => StmtAST.Pkg(name, substituteInType(body, subst), span)
  }

  /** Normalize a type AST with shallow beta-reduction of type-level lambdas/applications. */
  def normalizeType(ast: AST): AST = {
    ast match
      case AST.ExtensionAccess(target, id, name, targetTy, span) => AST.ExtensionAccess(normalizeType(target), id, name, targetTy, span)
      case AST.App(func, args, implicitArgs, span) =>
        val nFunc = normalizeType(func)
        val nArgs = args.map(a => Arg(normalizeType(a.value), a.implicitness, a.coeffect))
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

  def normalizeTypeStmt(stmt: StmtAST): StmtAST = {
    stmt match
      case StmtAST.ExprStmt(expr, span) => StmtAST.ExprStmt(normalizeType(expr), span)
      case StmtAST.JSImport(id, localName, modulePath, kind, ty, span) =>
        StmtAST.JSImport(id, localName, modulePath, kind, normalizeType(ty), span)
      case StmtAST.Def(id, name, teles, resTy, body, span, effects) =>
        val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))))
        StmtAST.Def(id, name, nTeles, resTy.map(normalizeType), normalizeType(body), span, effects)
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
      case StmtAST.Effect(id, name, ops, span) =>
        val normOps = ops.map(p => p.copy(ty = normalizeType(p.ty), default = p.default.map(normalizeType)))
        StmtAST.Effect(id, name, normOps, span)
      case StmtAST.Pkg(name, body, span) => StmtAST.Pkg(name, normalizeType(body), span)
  }

  def eraseSpans(ast: AST): AST = {
    ast match
      case AST.ExtensionAccess(target, id, name, targetTy, _) => AST.ExtensionAccess(eraseSpans(target), id, name, targetTy, None)
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
      case AST.Handle(action, effRef, handlers, _) =>
        AST.Handle(eraseSpans(action), effRef, handlers.map { case (n, lam) => (n, eraseSpans(lam)) }, None)
      case AST.Do(op, args, _) =>
        AST.Do(eraseSpans(op), args.map(eraseSpans), None)
      case AST.AnyType(_)            => AST.AnyType(None)
      case AST.BoolType(_)           => AST.BoolType(None)
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
        AST.App(eraseSpans(func), args.map(a => Arg(eraseSpans(a.value), a.implicitness, a.coeffect)), implicitArgs, None)
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

  def eraseSpansStmt(stmt: StmtAST): StmtAST = stmt match
    case StmtAST.ExprStmt(expr, _) => StmtAST.ExprStmt(eraseSpans(expr), None)
    case StmtAST.JSImport(id, localName, modulePath, kind, ty, _) =>
      StmtAST.JSImport(id, localName, modulePath, kind, eraseSpans(ty), None)
    case StmtAST.Def(id, name, teles, resTy, body, _, effects) =>
      val nTeles = teles.map(t => t.copy(params = t.params.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))))
      StmtAST.Def(id, name, nTeles, resTy.map(eraseSpans), eraseSpans(body), None, effects)
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
    case StmtAST.Effect(id, name, ops, _) =>
      val nOps = ops.map(p => p.copy(ty = eraseSpans(p.ty), default = p.default.map(eraseSpans)))
      StmtAST.Effect(id, name, nOps, None)
    case StmtAST.Pkg(name, body, _) =>
      StmtAST.Pkg(name, eraseSpans(body), None)

  def hasMeta(ast: AST): Boolean = ast match
    case AST.ExtensionAccess(target, _, _, _, _) => hasMeta(target)
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
    case AST.BoolType(_)                   => false
    case AST.StringType(_)                 => false
    case AST.NaturalType(_)                => false
    case AST.IntegerType(_)                => false
    case AST.LevelType(_)                  => false
    case AST.Handle(action, _, handlers, _) => hasMeta(action) || handlers.exists { case (_, lam) => hasMeta(lam) }
    case AST.Do(op, args, _)              => hasMeta(op) || args.exists(hasMeta)

  def hasMetaStmt(stmt: StmtAST): Boolean = stmt match
    case StmtAST.ExprStmt(expr, _)           => hasMeta(expr)
    case StmtAST.JSImport(_, _, _, _, ty, _) => hasMeta(ty)
    case StmtAST.Def(_, _, teles, resTy, body, _, _) =>
      teles.exists(t => t.params.exists(p => hasMeta(p.ty))) || resTy.exists(hasMeta) || hasMeta(body)
    case StmtAST.Record(_, _, fields, _) => fields.exists(f => hasMeta(f.ty))
    case StmtAST.Effect(_, _, ops, _) => ops.exists(o => hasMeta(o.ty))
    case StmtAST.Enum(_, _, tps, cases, _) =>
      tps.exists(tp => hasMeta(tp.ty)) || cases.exists(c => c.params.exists(p => hasMeta(p.ty)))
    case StmtAST.Coenum(_, _, tps, cases, _) =>
      tps.exists(tp => hasMeta(tp.ty)) || cases.exists(c => c.params.exists(p => hasMeta(p.ty)))
    case StmtAST.Pkg(_, body, _) => hasMeta(body)

  def sameType(a: AST, b: AST): Boolean = {
    if hasMeta(a) || hasMeta(b) then true
    else {
      val normA = normalizeType(a)
      val normB = normalizeType(b)
      (normA, normB) match {
        case (AST.AnyType(_), _) => true
        case (_, AST.AnyType(_)) => true
        case _ =>
          alphaEquivalent(normA, normB, Map.empty)
      }
    }
  }

  def alphaEquivalent(a: AST, b: AST, ren: Map[UniqidOf[AST], UniqidOf[AST]]): Boolean = {
    (a, b) match {
      case (AST.Ref(idA, nameA, _), AST.Ref(idB, nameB, _)) =>
        val mappedIdA = ren.getOrElse(idA, idA)
        mappedIdA == idB || (nameA == nameB && ren.get(idA).isEmpty)
      case (AST.StringLit(s1, _), AST.StringLit(s2, _)) => s1 == s2
      case (AST.IntLit(i1, _), AST.IntLit(i2, _)) => i1 == i2
      case (AST.NaturalLit(n1, _), AST.NaturalLit(n2, _)) => n1 == n2
      case (AST.LevelLit(l1, _), AST.LevelLit(l2, _)) => l1 == l2
      case (AST.AnyType(_), AST.AnyType(_)) => true
      case (AST.BoolType(_), AST.BoolType(_)) => true
      case (AST.StringType(_), AST.StringType(_)) => true
      case (AST.IntegerType(_), AST.IntegerType(_)) => true
      case (AST.NaturalType(_), AST.NaturalType(_)) => true
      case (AST.LevelType(_), AST.LevelType(_)) => true
      case (AST.Type(l1, _), AST.Type(l2, _)) => alphaEquivalent(l1, l2, ren)
      case (AST.TypeOmega(l1, _), AST.TypeOmega(l2, _)) => alphaEquivalent(l1, l2, ren)
      case (AST.Tuple(e1, _), AST.Tuple(e2, _)) =>
        e1.length == e2.length && e1.zip(e2).forall { case (x, y) => alphaEquivalent(x, y, ren) }
      case (AST.TupleType(e1, _), AST.TupleType(e2, _)) =>
        e1.length == e2.length && e1.zip(e2).forall { case (x, y) => alphaEquivalent(x, y, ren) }
      case (AST.ListLit(e1, _), AST.ListLit(e2, _)) =>
        e1.length == e2.length && e1.zip(e2).forall { case (x, y) => alphaEquivalent(x, y, ren) }
      case (AST.ListType(el1, _), AST.ListType(el2, _)) => alphaEquivalent(el1, el2, ren)
      case (AST.Ann(ex1, ty1, _), AST.Ann(ex2, ty2, _)) =>
        alphaEquivalent(ex1, ex2, ren) && alphaEquivalent(ty1, ty2, ren)
      case (AST.RecordTypeRef(id1, name1, _), AST.RecordTypeRef(id2, name2, _)) =>
        id1 == id2
      case (AST.RecordCtor(id1, _, args1, _), AST.RecordCtor(id2, _, args2, _)) =>
        id1 == id2 && args1.length == args2.length && args1.zip(args2).forall { case (x, y) => alphaEquivalent(x, y, ren) }
      case (AST.EnumTypeRef(id1, _, _), AST.EnumTypeRef(id2, _, _)) =>
        id1 == id2
      case (AST.EnumCaseRef(e1, c1, _, _, _), AST.EnumCaseRef(e2, c2, _, _, _)) =>
        e1 == e2 && c1 == c2
      case (AST.EnumCtor(e1, c1, _, _, args1, _), AST.EnumCtor(e2, c2, _, _, args2, _)) =>
        e1 == e2 && c1 == c2 && args1.length == args2.length && args1.zip(args2).forall { case (x, y) => alphaEquivalent(x, y, ren) }
      case (AST.FieldAccess(t1, f1, _), AST.FieldAccess(t2, f2, _)) =>
        f1 == f2 && alphaEquivalent(t1, t2, ren)
      case (AST.App(f1, args1, _, _), AST.App(f2, args2, _, _)) =>
        alphaEquivalent(f1, f2, ren) && args1.length == args2.length &&
          args1.zip(args2).forall { case (a1, a2) => a1.implicitness == a2.implicitness && alphaEquivalent(a1.value, a2.value, ren) }
      case (AST.Let(id1, _, ty1, v1, b1, _), AST.Let(id2, _, ty2, v2, b2, _)) =>
        ty1.zip(ty2).forall { case (t1, t2) => alphaEquivalent(t1, t2, ren) } &&
          alphaEquivalent(v1, v2, ren) &&
          alphaEquivalent(b1, b2, ren + (id1 -> id2))
      case (AST.Lam(t1, b1, _), AST.Lam(t2, b2, _)) =>
        compareTeles(t1, t2, ren) match {
          case Some(newRen) => alphaEquivalent(b1, b2, newRen)
          case None => false
        }
      case (AST.Pi(t1, res1, effs1, _), AST.Pi(t2, res2, effs2, _)) =>
        effs1.toSet == effs2.toSet && {
          compareTeles(t1, t2, ren) match {
            case Some(newRen) => alphaEquivalent(res1, res2, newRen)
            case None => false
          }
        }
      case (b1: AST.Block, b2: AST.Block) =>
        eraseSpans(b1) == eraseSpans(b2)
      case _ => false
    }
  }

  def compareTeles(t1: Vector[Telescope], t2: Vector[Telescope], ren: Map[UniqidOf[AST], UniqidOf[AST]]): Option[Map[UniqidOf[AST], UniqidOf[AST]]] = {
    if t1.length != t2.length then None
    else {
      var currentRen = ren
      var ok = true
      for (i <- t1.indices if ok) {
        val tel1 = t1(i)
        val tel2 = t2(i)
        if tel1.implicitness != tel2.implicitness || tel1.params.length != tel2.params.length then {
          ok = false
        } else {
          for (j <- tel1.params.indices if ok) {
            val p1 = tel1.params(j)
            val p2 = tel2.params(j)
            if p1.coeffect != p2.coeffect || !alphaEquivalent(p1.ty, p2.ty, currentRen) then {
              ok = false
            } else {
              currentRen = currentRen + (p1.id -> p2.id)
            }
          }
        }
      }
      if ok then Some(currentRen) else None
    }
  }

