package chester.backend

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.error.Span
import chester.uniqid.UniqidOf
import chester.syntax.{
  CatchClause as TSCatchClause,
  EnumMember as TSEnumMember,
  ImportSpecifier as TSImportSpecifier,
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
  * The transformer is best-effort: if a construct has no obvious TypeScript analogue, it degrades to `undefined` rather than failing
  * outright.
  */
object TypeScriptBackend:

  private type RecordEnv = Map[UniqidOf[AST], Vector[String]]

  final case class Config(
      /** Optionally apply a CPS-style type rewrite before lowering. Expression CPS requires typing; this flag only affects types. */
      applyEffectCPS: Boolean = false,
      cpsConfig: EffectCPS.Config = EffectCPS.Config()
  )

  /** Entry point: lower a Chester AST into a TypeScript program. */
  def lowerProgram(ast: AST, config: Config = Config()): TypeScriptAST.Program = {
    val lowered = lowerAsStatements(ast, config, collectRecordEnv(ast), topLevel = true)
    TypeScriptAST.Program(lowered, ast.span)
  }

  /** Lower a Chester statement into zero or more TypeScript statements. */
  private def lowerStmt(stmt: StmtAST, config: Config, recordEnv: RecordEnv): Vector[TypeScriptAST] = {
    stmt match
      case StmtAST.ExprStmt(AST.Let(_, name, _, value, _, span), _) =>
        val declarator = TSVariableDeclarator(name, None, Some(lowerExpr(value, config, recordEnv)), span)
        Vector(TypeScriptAST.VariableDeclaration(TSVarKind.Const, Vector(declarator), span))

      case StmtAST.ExprStmt(expr, _) =>
        Vector(TypeScriptAST.ExpressionStatement(lowerExpr(expr, config, recordEnv), stmt.span))

      case StmtAST.JSImport(_, localName, modulePath, kind, _, span) =>
        val spec = kind match
          case chester.core.JSImportKind.Namespace => TSImportSpecifier.Namespace(localName, span)
          case chester.core.JSImportKind.Default   => TSImportSpecifier.Default(localName, span)
        Vector(TypeScriptAST.ImportDeclaration(Vector(spec), modulePath, stmt.span))

      case StmtAST.Def(_, name, telescopes, resultTy, body, _) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config, recordEnv)))
        val retTy = resultTy.map(t => lowerType(t, config))
        val fnBody = TypeScriptAST.Block(
          Vector(TypeScriptAST.Return(Some(lowerExpr(body, config, recordEnv)), body.span)),
          body.span
        )
        val fn = TypeScriptAST.FunctionDeclaration(name, params, retTy, fnBody, Vector.empty, stmt.span)
        Vector(fn)

      case StmtAST.Record(_, name, fields, _) =>
        if name.startsWith("JSImport_") || name.startsWith("GoImport_") then
          Vector.empty
        else {
          val members = fields.map { f =>
            TSInterfaceMember(
              f.name,
              TSInterfaceMemberType.PropertySignature(lowerType(f.ty, config), isOptional = false, isReadonly = false),
              None
            )
          }
          Vector(TypeScriptAST.InterfaceDeclaration(name, Vector.empty, Vector.empty, members.toVector, stmt.span))
        }

      case StmtAST.Enum(_, name, _, cases, _) =>
        val members = cases.map(c => TSEnumMember(c.name, None, None))
        Vector(TypeScriptAST.EnumDeclaration(name, members.toVector, isConst = false, stmt.span))

      case StmtAST.Coenum(_, name, _, cases, _) =>
        val members = cases.map(c => TSEnumMember(c.name, None, None))
        Vector(TypeScriptAST.EnumDeclaration(name, members.toVector, isConst = false, stmt.span))

      case StmtAST.Pkg(name, body, span) =>
        val inner = lowerAsStatements(body, config, recordEnv, topLevel = true)
        val (imports, rest) = inner.partition {
          case _: TypeScriptAST.ImportDeclaration => true
          case _                                  => false
        }
        (imports ++ Vector(TypeScriptAST.NamespaceDeclaration(name, rest, span))).toVector
  }

  /** Lower a block-like AST into a list of statements, appending a return for the tail when necessary. */
  private def lowerAsStatements(ast: AST, config: Config, recordEnv: RecordEnv, topLevel: Boolean = false): Vector[TypeScriptAST] = {
    ast match
      case AST.Block(elems, tail, _) =>
        val loweredElems = elems.flatMap(lowerStmt(_, config, recordEnv))
        val tailExpr = lowerExpr(tail, config, recordEnv)
        val tailIsUnit = tailExpr match
          case TypeScriptAST.Array(e, _) if e.isEmpty => true
          case TypeScriptAST.UndefinedLiteral(_)      => true
          case _                                      => false
        val tailStmt = {
          if topLevel then
            if tailIsUnit then Vector.empty
            else Vector(TypeScriptAST.ExpressionStatement(tailExpr, tail.span))
          else Vector(TypeScriptAST.Return(Some(tailExpr), tail.span))
        }
        (loweredElems ++ tailStmt).toVector
      case other =>
        val expr = lowerExpr(other, config, recordEnv)
        val stmt = {
          val isUnitExpr = expr == TypeScriptAST.UndefinedLiteral(other.span) || (expr match
            case TypeScriptAST.Array(e, _) if e.isEmpty => true
            case _                                      => false
          )
          if topLevel then
            if isUnitExpr then Vector.empty
            else Vector(TypeScriptAST.ExpressionStatement(expr, other.span))
          else Vector(TypeScriptAST.Return(Some(expr), other.span))
        }
        stmt
  }

  /** Lower a Chester expression to a TypeScript expression. */
  private def lowerExpr(expr: AST, config: Config, recordEnv: RecordEnv): TypeScriptAST = {
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
        TypeScriptAST.Array(elems.map(e => lowerExpr(e, config, recordEnv)), span)
      case AST.Tuple(elems, span) =>
        if elems.isEmpty then TypeScriptAST.UndefinedLiteral(span)
        else
          // Emit tuple as array literal
          TypeScriptAST.Array(elems.map(e => lowerExpr(e, config, recordEnv)), span)

      // Lambdas and application
      case AST.Lam(telescopes, body, span) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config, recordEnv)))
        TypeScriptAST.Arrow(params.toVector, lowerExpr(body, config, recordEnv), span)

      case AST.App(AST.Ref(_, "+", _), args, _, span) if args.length == 2 =>
        val left = lowerExpr(args(0).value, config, recordEnv)
        val right = lowerExpr(args(1).value, config, recordEnv)
        TypeScriptAST.BinaryOp(left, "+", right, span)

      case AST.App(func, args, _, span) =>
        val callee = lowerExpr(func, config, recordEnv)
        val loweredArgs = args.map(a => lowerExpr(a.value, config, recordEnv))
        TypeScriptAST.Call(callee, loweredArgs, span)

      // Let-binding as an IIFE
      case AST.Let(_, name, _, value, body, span) =>
        val valueExpr = lowerExpr(value, config, recordEnv)
        val declarator = TSVariableDeclarator(name, None, Some(valueExpr), span)
        val block = TypeScriptAST.Block(
          Vector(
            TypeScriptAST.VariableDeclaration(TSVarKind.Const, Vector(declarator), span),
            TypeScriptAST.Return(Some(lowerExpr(body, config, recordEnv)), body.span)
          ),
          span
        )
        TypeScriptAST.Call(TypeScriptAST.Arrow(Vector.empty, block, span), Vector.empty, span)

      case AST.Ann(expr, _, _) =>
        lowerExpr(expr, config, recordEnv)

      // Data constructors and selections
      case AST.RecordCtor(id, _, args, span) =>
        val props = recordEnv.get(id).toVector.flatten.zip(args).map { case (fieldName, arg) =>
          TSObjectProperty(Left(fieldName), lowerExpr(arg, config, recordEnv), isShorthand = false, isMethod = false, span)
        }
        TypeScriptAST.Object(props.toVector, span)

      case AST.FieldAccess(target, field, span) =>
        TypeScriptAST.PropertyAccess(lowerExpr(target, config, recordEnv), field, span)

      case AST.EnumCaseRef(_, _, enumName, caseName, span) =>
        TypeScriptAST.PropertyAccess(TypeScriptAST.Identifier(enumName, span), caseName, span)

      case AST.EnumCtor(_, _, enumName, caseName, args, span) =>
        val ctor = TypeScriptAST.PropertyAccess(TypeScriptAST.Identifier(enumName, span), caseName, span)
        TypeScriptAST.Call(ctor, args.map(a => lowerExpr(a, config, recordEnv)), span)

      // Blocks become IIFEs to preserve expression-ness
      case AST.Block(elems, tail, span) =>
        val blockStmts = elems.flatMap(lowerStmt(_, config, recordEnv))
        val ret = TypeScriptAST.Return(Some(lowerExpr(tail, config, recordEnv)), tail.span)
        TypeScriptAST.Call(TypeScriptAST.Arrow(Vector.empty, TypeScriptAST.Block((blockStmts :+ ret).toVector, span), span), Vector.empty, span)

      // Fallback
      case _ =>
        TypeScriptAST.UndefinedLiteral(expr.span)
  }

  private def collectRecordEnv(ast: AST): RecordEnv = {
    def fromStmt(stmt: StmtAST): RecordEnv =
      stmt match
        case StmtAST.Record(id, _, fields, _) =>
          Map(id -> fields.map(_.name))
        case StmtAST.Def(_, _, _, _, body, _) =>
          fromAst(body)
        case StmtAST.ExprStmt(expr, _) =>
          fromAst(expr)
        case StmtAST.Pkg(_, body, _) =>
          fromAst(body)
        case _ =>
          Map.empty

    def fromAst(node: AST): RecordEnv =
      node match
        case AST.Block(elements, tail, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], Vector[String]])(_ ++ fromStmt(_)) ++ fromAst(tail)
        case AST.Let(_, _, _, value, body, _) =>
          fromAst(value) ++ fromAst(body)
        case AST.Lam(_, body, _) =>
          fromAst(body)
        case AST.App(func, args, _, _) =>
          fromAst(func) ++ args.foldLeft(Map.empty[UniqidOf[AST], Vector[String]])((acc, arg) => acc ++ fromAst(arg.value))
        case AST.Ann(expr, ty, _) =>
          fromAst(expr) ++ fromAst(ty)
        case AST.Tuple(elements, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], Vector[String]])(_ ++ fromAst(_))
        case AST.ListLit(elements, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], Vector[String]])(_ ++ fromAst(_))
        case AST.RecordCtor(_, _, args, _) =>
          args.foldLeft(Map.empty[UniqidOf[AST], Vector[String]])(_ ++ fromAst(_))
        case AST.FieldAccess(target, _, _) =>
          fromAst(target)
        case AST.EnumCtor(_, _, _, _, args, _) =>
          args.foldLeft(Map.empty[UniqidOf[AST], Vector[String]])(_ ++ fromAst(_))
        case _ =>
          Map.empty

    fromAst(ast)
  }

  /** Lower a Chester parameter into a TypeScript function parameter. */
  private def lowerParam(param: Param, config: Config, recordEnv: RecordEnv): TSParameter = {
    TSParameter(
      name = param.name,
      paramType = Some(lowerType(param.ty, config)),
      defaultValue = param.default.map(d => lowerExpr(d, config, recordEnv)),
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
      case AST.BoolType(span)    => TypeScriptType.PrimitiveType("boolean", span)
      case AST.ListType(elem, span) =>
        TypeScriptType.TypeReference("Array", Vector(lowerType(elem, config)), span)
      case AST.TupleType(elems, span) =>
        if elems.isEmpty then TypeScriptType.PrimitiveType("void", span)
        else TypeScriptType.TupleType(elems.map(e => lowerType(e, config)), span)
      case AST.Pi(telescopes, resultTy, _, span) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config, Map.empty)))
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
