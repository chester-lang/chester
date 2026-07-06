package chester.tyck

import chester.core.{AST, Coeffect, Implicitness, StmtAST}
import chester.error.{Problem, Reporter, Span}
import chester.uniqid.UniqidOf
import scala.collection.mutable

object UsageChecker {

  def checkUsage(ast: AST)(using Reporter[ElabProblem]): Unit = {
    check(ast, Map.empty, computational = true)
  }

  def checkUsageStmt(stmt: StmtAST)(using Reporter[ElabProblem]): Unit = {
    stmt match {
      case StmtAST.Def(_, _, _, resultTy, body, _, _) =>
        resultTy.foreach(check(_, Map.empty, computational = false))
        check(body, Map.empty, computational = true)
      case StmtAST.ExprStmt(expr, _) =>
        check(expr, Map.empty, computational = true)
      case StmtAST.JSImport(_, _, _, _, ty, _) =>
        check(ty, Map.empty, computational = false)
      case StmtAST.Record(_, _, fields, _) =>
        fields.foreach { f =>
          check(f.ty, Map.empty, computational = false)
          f.default.foreach(check(_, Map.empty, computational = true))
        }
      case StmtAST.Enum(_, _, typeParams, cases, _) =>
        typeParams.foreach { p =>
          check(p.ty, Map.empty, computational = false)
          p.default.foreach(check(_, Map.empty, computational = true))
        }
        cases.foreach { c =>
          c.params.foreach { p =>
            check(p.ty, Map.empty, computational = false)
            p.default.foreach(check(_, Map.empty, computational = true))
          }
        }
      case StmtAST.Effect(_, _, ops, _) =>
        ops.foreach { o =>
          check(o.ty, Map.empty, computational = false)
          o.default.foreach(check(_, Map.empty, computational = true))
        }
      case StmtAST.Coenum(_, _, typeParams, cases, _) =>
        typeParams.foreach { p =>
          check(p.ty, Map.empty, computational = false)
          p.default.foreach(check(_, Map.empty, computational = true))
        }
        cases.foreach { c =>
          c.params.foreach { p =>
            check(p.ty, Map.empty, computational = false)
            p.default.foreach(check(_, Map.empty, computational = true))
          }
        }
      case StmtAST.Pkg(_, body, _) =>
        check(body, Map.empty, computational = true)
    }
  }

  /**
    * @param computational true if the AST is inside a computational position (i.e. evaluated at runtime). 
    *                      false if the AST is purely a type (types are erased at runtime).
    */
  private def check(ast: AST, bound: Map[UniqidOf[AST], Coeffect], computational: Boolean)(using r: Reporter[ElabProblem]): Unit = {
    
    // Helper to count occurrences of a variable ID in an AST in computational positions
    def countUsages(a: AST, target: UniqidOf[AST]): Int = a match {
      case AST.Ref(id, _, _) => if (id == target) 1 else 0
      case AST.App(func, args, _, _) => 
        countUsages(func, target) + args.map(arg => countUsages(arg.value, target)).sum
      case AST.Lam(teles, body, _) =>
        if teles.exists(_.params.exists(_.id == target)) then 0
        else countUsages(body, target)
      case AST.Pi(teles, res, _, _) =>
        // Types don't count towards computational usage, so we return 0.
        0
      case AST.Let(id, _, _, value, body, _) =>
        countUsages(value, target) + (if id == target then 0 else countUsages(body, target))
      case AST.Block(stmts, tail, _) =>
        val stmtUsages = stmts.map {
          case StmtAST.Def(_, _, _, _, body, _, _) => countUsages(body, target)
          case StmtAST.ExprStmt(expr, _) => countUsages(expr, target)
          case _ => 0
        }.sum
        stmtUsages + countUsages(tail, target)
      case AST.FieldAccess(record, _, _) => countUsages(record, target)
      case AST.ExtensionAccess(targetObj, _, _, _, _) => countUsages(targetObj, target)
      case AST.RecordCtor(_, _, args, _) => args.map(countUsages(_, target)).sum
      case AST.Tuple(elems, _) => elems.map(countUsages(_, target)).sum
      case AST.ListLit(elems, _) => elems.map(countUsages(_, target)).sum
      case AST.Handle(action, _, handlers, _) =>
        countUsages(action, target) + handlers.map(_._2).map(countUsages(_, target)).sum
      case AST.Do(op, args, _) =>
        countUsages(op, target) + args.map(countUsages(_, target)).sum
      case AST.Ann(expr, _, _) => countUsages(expr, target)
      case AST.Type(_, _) | AST.TypeOmega(_, _) | AST.IntLit(_, _) | AST.NaturalLit(_, _) | AST.StringLit(_, _) | AST.LevelLit(_, _) | AST.AnyType(_) | AST.BoolType(_) | AST.StringType(_) | AST.NaturalType(_) | AST.IntegerType(_) | AST.LevelType(_) | AST.TupleType(_, _) | AST.ListType(_, _) | AST.RecordTypeRef(_, _, _) | AST.EnumTypeRef(_, _, _) => 0
      case AST.MetaCell(_, _) => 0
      case AST.EnumCaseRef(_, _, _, _, _) => 0
      case AST.EnumCtor(_, _, _, _, args, _) => args.map(countUsages(_, target)).sum
    }

    ast match {
      case AST.Ref(id, name, span) =>
        bound.get(id) match {
          case Some(Coeffect.Zero) if computational =>
            r.report(ElabProblem.LinearityError(s"Variable '$name' is marked as erased (usage 0), but is used in a computational position", span))
          case _ => ()
        }
      case AST.App(func, args, _, _) =>
        check(func, bound, computational)
        args.foreach(arg => check(arg.value, bound, computational))
      case AST.Lam(teles, body, _) =>
        var newBound = bound
        for tel <- teles; p <- tel.params do
          newBound = newBound + (p.id -> p.coeffect)
          if computational then
            val usages = countUsages(body, p.id)
            p.coeffect match {
              case Coeffect.One if usages != 1 =>
                r.report(ElabProblem.LinearityError(s"Variable '${p.name}' is marked as linear (usage 1), but is used $usages times", ast.span))
              case _ => ()
            }
        check(body, newBound, computational)
      case AST.Pi(teles, res, _, _) =>
        var newBound = bound
        for tel <- teles; p <- tel.params do
          check(p.ty, newBound, computational = false)
          newBound = newBound + (p.id -> Coeffect.Zero) // Bindings in Pi are types, so they are erased
        check(res, newBound, computational = false)
      case AST.Let(id, name, tyOpt, value, body, span) =>
        tyOpt.foreach(check(_, bound, computational = false))
        check(value, bound, computational)
        check(body, bound + (id -> Coeffect.Unrestricted), computational)
      case AST.Block(stmts, tail, _) =>
        stmts.foreach(checkUsageStmt)
        check(tail, bound, computational)
      case AST.FieldAccess(record, _, _) => check(record, bound, computational)
      case AST.ExtensionAccess(targetObj, _, _, _, _) => check(targetObj, bound, computational)
      case AST.RecordCtor(_, _, args, _) => args.foreach(check(_, bound, computational))
      case AST.Tuple(elems, _) => elems.foreach(check(_, bound, computational))
      case AST.ListLit(elems, _) => elems.foreach(check(_, bound, computational))
      case AST.Handle(action, _, handlers, _) =>
        check(action, bound, computational)
        handlers.foreach(h => check(h._2, bound, computational))
      case AST.Do(op, args, _) =>
        check(op, bound, computational)
        args.foreach(check(_, bound, computational))
      case AST.Ann(expr, _, _) => check(expr, bound, computational)
      case AST.Type(_, _) | AST.TypeOmega(_, _) | AST.IntLit(_, _) | AST.NaturalLit(_, _) | AST.StringLit(_, _) | AST.LevelLit(_, _) | AST.AnyType(_) | AST.BoolType(_) | AST.StringType(_) | AST.NaturalType(_) | AST.IntegerType(_) | AST.LevelType(_) | AST.TupleType(_, _) | AST.ListType(_, _) | AST.RecordTypeRef(_, _, _) | AST.EnumTypeRef(_, _, _) => ()
      case AST.MetaCell(_, _) => ()
      case AST.EnumCaseRef(_, _, _, _, _) => ()
      case AST.EnumCtor(_, _, _, _, args, _) => args.foreach(check(_, bound, computational))
    }
  }
}
