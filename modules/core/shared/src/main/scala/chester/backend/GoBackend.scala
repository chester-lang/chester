package chester.backend

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, EnumCase, Param, StmtAST, Telescope}
import chester.syntax.{GoAST, GoCompositeElement, GoField, GoType, GoTypeParam, GoTypeSpec, GoValueDeclKind, GoValueSpec}
import chester.transform.EffectCPS

/** Backend lowering from Chester core `AST` into a lightweight Go AST for code generation. */
object GoBackend:

  final case class Config(
      applyEffectCPS: Boolean = false,
      cpsConfig: EffectCPS.Config = EffectCPS.Config()
  )

  /** Entry point: lower a Chester AST into a Go file. */
  def lowerProgram(ast: AST, config: Config = Config(), packageName: String = "main"): GoAST.File = {
    val (pkg, decls) = lowerAsDeclarations(ast, config, packageName, topLevel = true)
    GoAST.File(pkg, Vector.empty, decls, ast.span)
  }

  /** Lower a Chester statement into zero or more Go declarations/statements. */
  private def lowerStmt(stmt: StmtAST, config: Config, packageName: String): (String, Vector[GoAST]) = {
    stmt match
      case StmtAST.ExprStmt(expr, span) =>
        (packageName, Vector(GoAST.ExprStmt(lowerExpr(expr, config), span)))

      case StmtAST.Def(_, name, telescopes, resultTy, body, span) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config)))
        val results = resultTy.map(t => Vector(lowerResultField(t, config))).getOrElse(Vector.empty)
        val fnBody = GoAST.Block(
          Vector(GoAST.Return(Vector(lowerExpr(body, config)), body.span)),
          body.span
        )
        val fn = GoAST.FuncDecl(
          name = name,
          typeParams = Vector.empty,
          params = params.toVector,
          results = results,
          body = Some(fnBody),
          receiver = None,
          span = span
        )
        (packageName, Vector(fn))

      case StmtAST.Record(_, name, fields, span) =>
        val structFields =
          fields.map(f => GoField(Vector(f.name), lowerType(f.ty, config), None, isEmbedded = false, isVariadic = false, span = f.ty.span))
        val typeSpec = GoTypeSpec(name, Vector.empty, GoType.Struct(structFields.toVector, span), isAlias = false, span = span)
        (packageName, Vector(GoAST.TypeDecl(Vector(typeSpec), span)))

      case StmtAST.Enum(_, name, _, cases, span) =>
        (packageName, lowerEnumLike(name, cases, span))

      case StmtAST.Coenum(_, name, _, cases, span) =>
        (packageName, lowerEnumLike(name, cases, span))

      case StmtAST.Pkg(name, body, _) =>
        val (_, inner) = lowerAsDeclarations(body, config, name, topLevel = true)
        (name, inner)
  }

  /** Lower a block-like AST into a list of Go statements, appending a return for the tail when necessary. */
  private def lowerAsDeclarations(ast: AST, config: Config, packageName: String, topLevel: Boolean): (String, Vector[GoAST]) = {
    ast match
      case AST.Block(elems, tail, _) =>
        val (pkgAfterElems, loweredElems) = elems.foldLeft((packageName, Vector.empty[GoAST])) { case ((pkg, acc), stmt) =>
          val (nextPkg, stmts) = lowerStmt(stmt, config, pkg)
          (nextPkg, acc ++ stmts)
        }
        val tailExpr = lowerExpr(tail, config)
        val tailIsUnit = tailExpr match
          case GoAST.NilLiteral(_)                                        => true
          case GoAST.CompositeLiteral(_, elements, _) if elements.isEmpty => true
          case _                                                          => false
        val tailStmt = {
          if topLevel && tailIsUnit then Vector.empty
          else Vector(GoAST.Return(Vector(tailExpr), tail.span))
        }
        (pkgAfterElems, (loweredElems ++ tailStmt).toVector)
      case other =>
        val expr = lowerExpr(other, config)
        val tailIsUnit = expr match
          case GoAST.NilLiteral(_)                                        => true
          case GoAST.CompositeLiteral(_, elements, _) if elements.isEmpty => true
          case _                                                          => false
        val stmt = {
          if topLevel && tailIsUnit then Vector.empty
          else Vector(GoAST.Return(Vector(expr), other.span))
        }
        (packageName, stmt)
  }

  /** Lower a Chester expression to a Go expression node. */
  private def lowerExpr(expr: AST, config: Config): GoAST = {
    expr match
      // Literals
      case AST.IntLit(value, span)     => GoAST.IntLiteral(value.toString, span)
      case AST.NaturalLit(value, span) => GoAST.IntLiteral(value.toString, span)
      case AST.LevelLit(value, span)   => GoAST.IntLiteral(value.toString, span)
      case AST.StringLit(value, span)  => GoAST.StringLiteral(value, span)

      // Identifiers and built-ins
      case AST.Ref(_, name, span) =>
        name match
          case "true"  => GoAST.BoolLiteral(true, span)
          case "false" => GoAST.BoolLiteral(false, span)
          case _       => GoAST.Identifier(name, span)

      // Collections
      case AST.ListLit(elems, span) =>
        val sliceType = GoType.Slice(GoType.Named("any", span), span)
        GoAST.CompositeLiteral(
          sliceType,
          elems.map(e => GoCompositeElement(None, lowerExpr(e, config), e.span)).toVector,
          span
        )
      case AST.Tuple(elems, span) =>
        val sliceType = GoType.Slice(GoType.Named("any", span), span)
        GoAST.CompositeLiteral(
          sliceType,
          elems.map(e => GoCompositeElement(None, lowerExpr(e, config), e.span)).toVector,
          span
        )

      // Lambdas and application
      case AST.Lam(telescopes, body, span) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config)))
        val fnBody = GoAST.Block(Vector(GoAST.Return(Vector(lowerExpr(body, config)), body.span)), body.span)
        GoAST.FuncLiteral(Vector.empty, params.toVector, Vector.empty, fnBody, span)

      case AST.App(AST.Ref(_, "+", _), args, _, span) if args.length == 2 =>
        val left = lowerExpr(args(0).value, config)
        val right = lowerExpr(args(1).value, config)
        GoAST.Binary(left, "+", right, span)

      case AST.App(func, args, _, span) =>
        val callee = lowerExpr(func, config)
        val loweredArgs = args.map(a => lowerExpr(a.value, config))
        GoAST.Call(callee, loweredArgs, span)

      // Let-binding as an IIFE
      case AST.Let(_, name, _, value, body, span) =>
        val valueExpr = lowerExpr(value, config)
        val assign = GoAST.Assign(Vector(GoAST.Identifier(name, span)), Vector(valueExpr), ":=", span)
        val block = GoAST.Block(
          Vector(
            assign,
            GoAST.Return(Vector(lowerExpr(body, config)), body.span)
          ),
          span
        )
        GoAST.Call(GoAST.FuncLiteral(Vector.empty, Vector.empty, Vector.empty, block, span), Vector.empty, span)

      case AST.Ann(expr, _, _) =>
        lowerExpr(expr, config)

      // Data constructors and selections
      case AST.RecordCtor(_, name, args, span) =>
        val tagField = GoField(Vector("_tag"), GoType.Named("string", span), None, isEmbedded = false, isVariadic = false, span = span)
        val argFields = args.zipWithIndex.map { case (_, idx) =>
          GoField(Vector(s"_${idx + 1}"), GoType.Named("any", span), None, isEmbedded = false, isVariadic = false, span = span)
        }
        val structType = GoType.Struct((tagField +: argFields).toVector, span)
        val tagElem = GoCompositeElement(Some(GoAST.Identifier("_tag", span)), GoAST.StringLiteral(name, span), span)
        val elems = args.zipWithIndex.map { case (arg, idx) =>
          GoCompositeElement(Some(GoAST.Identifier(s"_${idx + 1}", span)), lowerExpr(arg, config), arg.span)
        }
        GoAST.CompositeLiteral(structType, (Vector(tagElem) ++ elems).toVector, span)

      case AST.FieldAccess(target, field, span) =>
        GoAST.Selector(lowerExpr(target, config), field, span)

      case AST.EnumCaseRef(_, _, enumName, caseName, span) =>
        GoAST.Selector(GoAST.Identifier(enumName, span), caseName, span)

      case AST.EnumCtor(_, _, enumName, caseName, args, span) =>
        val ctor = GoAST.Selector(GoAST.Identifier(enumName, span), caseName, span)
        GoAST.Call(ctor, args.map(a => lowerExpr(a, config)), span)

      // Blocks become IIFEs to preserve expression-ness
      case AST.Block(elems, tail, span) =>
        val stmts = elems.flatMap(lowerStmt(_, config, "main")._2)
        val ret = GoAST.Return(Vector(lowerExpr(tail, config)), tail.span)
        GoAST.Call(GoAST.FuncLiteral(Vector.empty, Vector.empty, Vector.empty, GoAST.Block((stmts :+ ret).toVector, span), span), Vector.empty, span)

      // Fallback
      case _ =>
        GoAST.NilLiteral(expr.span)
  }

  /** Lower a Chester parameter into a Go function parameter. */
  private def lowerParam(param: Param, config: Config): GoField = {
    GoField(
      names = Vector(param.name),
      fieldType = lowerType(param.ty, config),
      tag = None,
      isEmbedded = false,
      isVariadic = false,
      span = param.ty.span
    )
  }

  /** Lower a Chester type into a Go type. */
  private def lowerType(ty: AST, config: Config): GoType = {
    val rewritten = if config.applyEffectCPS then EffectCPS.transformType(ty, config.cpsConfig) else ty
    rewritten match
      case AST.StringType(span)  => GoType.Named("string", span)
      case AST.IntegerType(span) => GoType.Named("int", span)
      case AST.NaturalType(span) => GoType.Named("uint", span)
      case AST.AnyType(span)     => GoType.Named("any", span)
      case AST.ListType(elem, span) =>
        GoType.Slice(lowerType(elem, config), span)
      case AST.TupleType(_, span) =>
        GoType.Slice(GoType.Named("any", span), span)
      case AST.Pi(telescopes, resultTy, _, span) =>
        val params = telescopes.flatMap(t => t.params.map(p => lowerParam(p, config)))
        val results = Vector(lowerResultField(resultTy, config))
        GoType.Func(Vector.empty, params.toVector, results, span)
      case AST.RecordTypeRef(_, name, span) =>
        GoType.Named(name, span)
      case AST.EnumTypeRef(_, name, span) =>
        GoType.Named(name, span)
      case AST.Type(_, span) =>
        GoType.Named("any", span)
      case AST.TypeOmega(_, span) =>
        GoType.Named("any", span)
      case _ =>
        GoType.Named("any", ty.span)
  }

  private def lowerResultField(ty: AST, config: Config): GoField =
    GoField(Vector.empty, lowerType(ty, config), None, isEmbedded = false, isVariadic = false, span = ty.span)

  private def lowerEnumLike(name: String, cases: Vector[EnumCase], span: Option[chester.error.Span]): Vector[GoAST] = {
    val stringType = GoType.Named("string", span)
    val structFields = cases.map(c => GoField(Vector(c.name), stringType, None, isEmbedded = false, isVariadic = false, span = span))
    val structType = GoType.Struct(structFields.toVector, span)
    val typeSpec = GoTypeSpec(name, Vector.empty, structType, isAlias = false, span = span)
    val compositeElems = cases.map(c => GoCompositeElement(Some(GoAST.Identifier(c.name, span)), GoAST.StringLiteral(c.name, span), span))
    val valueSpec = GoValueSpec(
      names = Vector(name),
      valueType = Some(structType),
      values = Vector(GoAST.CompositeLiteral(structType, compositeElems.toVector, span)),
      span = span
    )
    Vector(GoAST.TypeDecl(Vector(typeSpec), span), GoAST.ValueDecl(GoValueDeclKind.Var, Vector(valueSpec), span))
  }
