package chester.backend

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.error.Span
import chester.syntax.{
  CatchClause as TSCatchClause,
  EnumMember as TSEnumMember,
  InterfaceMember as TSInterfaceMember,
  InterfaceMemberType as TSInterfaceMemberType,
  Modifier as TSModifier,
  ObjectProperty as TSObjectProperty,
  Parameter as TSParameter,
  TypeParameter as TSTypeParameter,
  TypeScriptAST,
  TypeScriptType,
  VarKind as TSVarKind,
  VariableDeclarator as TSVariableDeclarator
}
import chester.transform.EffectCPS

/** Backend lowering from Chester core `AST` into a lightweight TypeScript AST.
  *
  * The goal is to provide a predictable bridge for code generation. The lowering deliberately targets a minimal subset:
  *   - Functions (`def`) become function declarations.
  *   - Lambdas become arrow functions.
  *   - Records become interfaces; enums/coenums become TypeScript enums.
  *   - Expressions map to their closest TypeScript counterpart (calls, literals, arrays, objects, property access).
  *
  * The transformer is best-effort: if a construct has no obvious TypeScript analogue, it degrades to an identifier or `undefined` rather than failing
  * outright.
  */
object TypeScriptBackend:

  final case class Config(
      /** Optionally apply a CPS-style type rewrite before lowering. Expression CPS requires typing; this flag only affects types. */
      applyEffectCPS: Boolean = false,
      cpsConfig: EffectCPS.Config = EffectCPS.Config()
  )

  /** Entry point: lower a Chester AST into a TypeScript program. */
  def lowerProgram(ast: AST, config: Config = Config()): TypeScriptAST.Program = {
    val lowered = lowerAsStatements(ast, config, topLevel = true)
    TypeScriptAST.Program(lowered, ast.span)
  }

  /** Lower a Chester statement into zero or more TypeScript statements. */
  private def lowerStmt(stmt: StmtAST, config: Config): Vector[TypeScriptAST] = {
    stmt match
      case StmtAST.ExprStmt(expr, _) =>
        Vector(TypeScriptAST.ExpressionStatement(lowerExpr(expr, config), stmt.span))

      case StmtAST.Def(_, name, telescopes, resultTy, body, _) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config)))
        val retTy = resultTy.map(t => lowerType(t, config))
        val fnBody = TypeScriptAST.Block(
          Vector(TypeScriptAST.Return(Some(lowerExpr(body, config)), body.span)),
          body.span
        )
        val fn = TypeScriptAST.FunctionDeclaration(name, params, retTy, fnBody, Vector.empty, stmt.span)
        Vector(fn)

      case StmtAST.Record(_, name, fields, _) =>
        val members = fields.map { f =>
          TSInterfaceMember(
            f.name,
            TSInterfaceMemberType.PropertySignature(lowerType(f.ty, config), isOptional = false, isReadonly = false),
            None
          )
        }
        Vector(TypeScriptAST.InterfaceDeclaration(name, Vector.empty, Vector.empty, members.toVector, stmt.span))

      case StmtAST.Enum(_, name, _, cases, _) =>
        val members = cases.map(c => TSEnumMember(c.name, None, None))
        Vector(TypeScriptAST.EnumDeclaration(name, members.toVector, isConst = false, stmt.span))

      case StmtAST.Coenum(_, name, _, cases, _) =>
        val members = cases.map(c => TSEnumMember(c.name, None, None))
        Vector(TypeScriptAST.EnumDeclaration(name, members.toVector, isConst = false, stmt.span))

      case StmtAST.Pkg(name, body, span) =>
        val inner = lowerAsStatements(body, config, topLevel = true)
        Vector(TypeScriptAST.NamespaceDeclaration(name, inner, span))
  }

  /** Lower a block-like AST into a list of statements, appending a return for the tail when necessary. */
  private def lowerAsStatements(ast: AST, config: Config, topLevel: Boolean = false): Vector[TypeScriptAST] = {
    ast match
      case AST.Block(elems, tail, _) =>
        val loweredElems = elems.flatMap(lowerStmt(_, config))
        val tailExpr = lowerExpr(tail, config)
        val tailIsUnit = tailExpr match
          case TypeScriptAST.Array(e, _) if e.isEmpty   => true
          case TypeScriptAST.Identifier("undefined", _) => true
          case _                                        => false
        val tailStmt = {
          if topLevel && tailIsUnit then Vector.empty
          else Vector(TypeScriptAST.Return(Some(tailExpr), tail.span))
        }
        (loweredElems ++ tailStmt).toVector
      case other =>
        val expr = lowerExpr(other, config)
        val stmt = {
          if topLevel && (expr == TypeScriptAST.UndefinedLiteral(other.span) || (expr match
              case TypeScriptAST.Array(e, _) if e.isEmpty => true
              case _                                      => false))
          then Vector.empty
          else Vector(TypeScriptAST.Return(Some(expr), other.span))
        }
        stmt
  }

  /** Lower a Chester expression to a TypeScript expression. */
  private def lowerExpr(expr: AST, config: Config): TypeScriptAST = {
    expr match
      // Literals
      case AST.IntLit(value, span)     => TypeScriptAST.NumberLiteral(value.toString, span)
      case AST.NaturalLit(value, span) => TypeScriptAST.NumberLiteral(value.toString, span)
      case AST.LevelLit(value, span)   => TypeScriptAST.NumberLiteral(value.toString, span)
      case AST.StringLit(value, span)  => TypeScriptAST.StringLiteral(value, span)

      // Simple identifiers and built-ins
      case AST.Ref(_, name, span) =>
        name match
          case "true"  => TypeScriptAST.BooleanLiteral(true, span)
          case "false" => TypeScriptAST.BooleanLiteral(false, span)
          case _       => TypeScriptAST.Identifier(name, span)

      // Collections
      case AST.ListLit(elems, span) =>
        TypeScriptAST.Array(elems.map(e => lowerExpr(e, config)), span)
      case AST.Tuple(elems, span) =>
        // Emit tuple as array literal
        TypeScriptAST.Array(elems.map(e => lowerExpr(e, config)), span)

      // Lambdas and application
      case AST.Lam(telescopes, body, span) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config)))
        TypeScriptAST.Arrow(params.toVector, lowerExpr(body, config), span)

      case AST.App(AST.Ref(_, "+", _), args, _, span) if args.length == 2 =>
        val left = lowerExpr(args(0).value, config)
        val right = lowerExpr(args(1).value, config)
        TypeScriptAST.BinaryOp(left, "+", right, span)

      case AST.App(func, args, _, span) =>
        val callee = lowerExpr(func, config)
        val loweredArgs = args.map(a => lowerExpr(a.value, config))
        TypeScriptAST.Call(callee, loweredArgs, span)

      // Let-binding as an IIFE
      case AST.Let(_, name, _, value, body, span) =>
        val valueExpr = lowerExpr(value, config)
        val declarator = TSVariableDeclarator(name, None, Some(valueExpr), span)
        val block = TypeScriptAST.Block(
          Vector(
            TypeScriptAST.VariableDeclaration(TSVarKind.Const, Vector(declarator), span),
            TypeScriptAST.Return(Some(lowerExpr(body, config)), body.span)
          ),
          span
        )
        TypeScriptAST.Call(TypeScriptAST.Arrow(Vector.empty, block, span), Vector.empty, span)

      case AST.Ann(expr, _, _) =>
        lowerExpr(expr, config)

      // Data constructors and selections
      case AST.RecordCtor(_, name, args, span) =>
        val props = args.zipWithIndex.map { case (arg, idx) =>
          val key = Left(s"_${idx + 1}")
          TSObjectProperty(key, lowerExpr(arg, config), isShorthand = false, isMethod = false, span)
        }
        val tagProp = TSObjectProperty(Left("_tag"), TypeScriptAST.StringLiteral(name, span), isShorthand = false, isMethod = false, span)
        TypeScriptAST.Object((Vector(tagProp) ++ props).toVector, span)

      case AST.FieldAccess(target, field, span) =>
        TypeScriptAST.PropertyAccess(lowerExpr(target, config), field, span)

      case AST.EnumCaseRef(_, _, enumName, caseName, span) =>
        TypeScriptAST.PropertyAccess(TypeScriptAST.Identifier(enumName, span), caseName, span)

      case AST.EnumCtor(_, _, enumName, caseName, args, span) =>
        val ctor = TypeScriptAST.PropertyAccess(TypeScriptAST.Identifier(enumName, span), caseName, span)
        TypeScriptAST.Call(ctor, args.map(a => lowerExpr(a, config)), span)

      // Blocks become IIFEs to preserve expression-ness
      case AST.Block(elems, tail, span) =>
        val blockStmts = elems.flatMap(lowerStmt(_, config))
        val ret = TypeScriptAST.Return(Some(lowerExpr(tail, config)), tail.span)
        TypeScriptAST.Call(TypeScriptAST.Arrow(Vector.empty, TypeScriptAST.Block((blockStmts :+ ret).toVector, span), span), Vector.empty, span)

      // Fallback
      case _ =>
        TypeScriptAST.Identifier("undefined", expr.span)
  }

  /** Lower a Chester parameter into a TypeScript function parameter. */
  private def lowerParam(param: Param, config: Config): TSParameter = {
    TSParameter(
      name = param.name,
      paramType = Some(lowerType(param.ty, config)),
      defaultValue = param.default.map(d => lowerExpr(d, config)),
      isRest = false,
      span = None
    )
  }

  /** Lower a Chester type into a TypeScript type. */
  private def lowerType(ty: AST, config: Config): TypeScriptType = {
    val rewritten = if config.applyEffectCPS then EffectCPS.transformType(ty, config.cpsConfig) else ty
    rewritten match
      case AST.StringType(span)  => TypeScriptType.PrimitiveType("string", span)
      case AST.IntegerType(span) => TypeScriptType.PrimitiveType("number", span)
      case AST.NaturalType(span) => TypeScriptType.PrimitiveType("number", span)
      case AST.AnyType(span)     => TypeScriptType.PrimitiveType("any", span)
      case AST.ListType(elem, span) =>
        TypeScriptType.TypeReference("Array", Vector(lowerType(elem, config)), span)
      case AST.TupleType(elems, span) =>
        TypeScriptType.TupleType(elems.map(e => lowerType(e, config)), span)
      case AST.Pi(telescopes, resultTy, _, span) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config)))
        TypeScriptType.FunctionType(params.toVector, lowerType(resultTy, config), span)
      case AST.RecordTypeRef(_, name, span) =>
        TypeScriptType.TypeReference(name, Vector.empty, span)
      case AST.EnumTypeRef(_, name, span) =>
        TypeScriptType.TypeReference(name, Vector.empty, span)
      case AST.Type(_, span) =>
        TypeScriptType.PrimitiveType("any", span)
      case AST.TypeOmega(_, span) =>
        TypeScriptType.PrimitiveType("any", span)
      case _ =>
        TypeScriptType.PrimitiveType("any", ty.span)
  }
