package chester.backend

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, EnumCase, Param, StmtAST, Telescope}
import chester.syntax.{GoAST, GoCompositeElement, GoField, GoImportSpec, GoType, GoTypeParam, GoTypeSpec, GoValueDeclKind, GoValueSpec}
import chester.transform.EffectCPS
import chester.uniqid.UniqidOf

/** Backend lowering from Chester core `AST` into a lightweight Go AST for code generation. */
object GoBackend:

  private type RecordEnv = Map[UniqidOf[AST], (String, Vector[String])]
  private type GoImportEnv = Map[String, String]
  private type EffectEnv = Set[String]

  final case class Config(
      applyEffectCPS: Boolean = false,
      cpsConfig: EffectCPS.Config = EffectCPS.Config()
  )

  private def anyResultField(span: Option[chester.error.Span]): GoField =
    GoField(Vector.empty, GoType.Named("any", span), None, isEmbedded = false, isVariadic = false, span = span)

  private def unitValue(span: Option[chester.error.Span]): GoAST =
    GoAST.CompositeLiteral(GoType.Struct(Vector.empty, span), Vector.empty, span)

  /** Entry point: lower a Chester AST into a Go file. */
  def lowerProgram(ast: AST, config: Config = Config(), packageName: String = "main"): GoAST.File = {
    val recordEnv = collectRecordEnv(ast)
    val goImportEnv = collectGoImportEnv(ast)
    val effectEnv = collectEffectEnv(ast)
    val (pkg, decls) = lowerAsDeclarations(ast, config, recordEnv, goImportEnv, effectEnv, packageName, topLevel = true)

    // Separate type/function declarations from executable statements
    val (typeFuncDecls, execStmts) = decls.partition {
      case _: GoAST.TypeDecl | _: GoAST.FuncDecl | _: GoAST.ValueDecl => true
      case _                                                          => false
    }

    // If there are executable statements (returns, expr stmts), wrap them in main()
    val hasUserMain = typeFuncDecls.exists {
      case GoAST.FuncDecl("main", _, _, _, _, _, _) => true
      case _                                         => false
    }
    val execStmtsForWrapper =
      if hasUserMain && execStmts.forall(isTopLevelCallToMain) then Vector.empty
      else execStmts

    val allDecls = if (execStmtsForWrapper.nonEmpty) {
      // Convert return statements to expression statements or println for main()
      val mainStmts = execStmtsForWrapper.map {
        case GoAST.Return(Vector(expr), span) =>
          // For non-unit returns, print the result using fmt.Println
          expr match {
            case GoAST.CompositeLiteral(_, Vector(), _) =>
              // Unit value - just ignore
              GoAST.ExprStmt(expr, span)
            case _ =>
              // Non-unit value - print it
              val fmtPrintln = GoAST.Selector(GoAST.Identifier("fmt", span), "Println", span)
              GoAST.ExprStmt(GoAST.Call(fmtPrintln, Vector(expr), span), span)
          }
        case other => other
      }

      val mainBody = GoAST.Block(mainStmts.toVector, ast.span)
      val mainFunc = GoAST.FuncDecl(
        name = "main",
        typeParams = Vector.empty,
        params = Vector.empty,
        results = Vector.empty,
        body = Some(mainBody),
        receiver = None,
        span = ast.span
      )
      typeFuncDecls :+ mainFunc
    } else {
      typeFuncDecls
    }

    // Check if we need to import fmt (if we're printing results)
    val needsFmt = execStmtsForWrapper.exists {
      case GoAST.Return(Vector(expr), _) =>
        expr match {
          case GoAST.CompositeLiteral(_, Vector(), _) => false
          case _                                                            => true
        }
      case _ => false
    }

    val autoImports = goImportEnv.values.toVector.distinct ++ (if needsFmt then Vector("fmt") else Vector.empty)
    val imports = autoImports.distinct.map(path => GoImportSpec(None, path, ast.span))

    GoAST.File(pkg, imports, allDecls.toVector, ast.span)
  }

  /** Lower a Chester statement into zero or more Go declarations/statements. */
  private def lowerStmt(
      stmt: StmtAST,
      config: Config,
      recordEnv: RecordEnv,
      goImportEnv: GoImportEnv,
      effectEnv: EffectEnv,
      packageName: String
  ): (String, Vector[GoAST]) = {
    stmt match
      case StmtAST.ExprStmt(AST.Let(_, name, _, value, _, span), _) =>
        val assign = GoAST.Assign(Vector(GoAST.Identifier(name, span)), Vector(lowerExpr(value, config, recordEnv, goImportEnv, effectEnv)), ":=", span)
        (packageName, Vector(assign))

      case StmtAST.ExprStmt(expr, span) =>
        (packageName, Vector(GoAST.ExprStmt(lowerExpr(expr, config, recordEnv, goImportEnv, effectEnv), span)))

      case StmtAST.JSImport(_, localName, _, _, _, _) if localName == "go" =>
        // Go imports are represented as actual Go import specs on the file, not statements.
        (packageName, Vector.empty)
      case StmtAST.JSImport(_, _, _, _, _, _) =>
        // JSImport is a JS/TS-only construct; ignore when lowering to Go.
        (packageName, Vector.empty)

      case StmtAST.Def(_, name, telescopes, resultTy, body, span, _) =>
        // Only include params from EXPLICIT telescopes — implicit type params (e.g. [T]) are erased to any
        val params = telescopes.flatMap(_.params).filter(_.coeffect != chester.core.Coeffect.Zero).map(p => lowerParam(p, config))
        val isGoMain = name == "main" && packageName == "main"
        // When there's no explicit return type annotation, default to `any` so that
        // the `return expr` in the body is always valid Go (non-void function).
        val results = if isGoMain then Vector.empty
                      else resultTy.map(t => Vector(lowerResultField(t, config)))
                                   .getOrElse(Vector(anyResultField(span)))
        val fnBody = lowerFunctionBody(body, config, recordEnv, goImportEnv, effectEnv, returnsValue = !isGoMain)
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

      case StmtAST.Effect(_, _, _, _) =>
        (packageName, Vector.empty)

      case StmtAST.Pkg(name, body, _) =>
        val (_, inner) = lowerAsDeclarations(body, config, recordEnv, goImportEnv, effectEnv, name, topLevel = true)
        (name, inner)
  }

  private def lowerFunctionBody(
      body: AST,
      config: Config,
      recordEnv: RecordEnv,
      goImportEnv: GoImportEnv,
      effectEnv: EffectEnv,
      returnsValue: Boolean
  ): GoAST.Block = {
    body match
      case AST.Block(elems, tail, span) =>
        val loweredElems = elems.flatMap(lowerStmt(_, config, recordEnv, goImportEnv, effectEnv, "main")._2)
        val tailExpr = lowerExpr(tail, config, recordEnv, goImportEnv, effectEnv)
        val tailIsUnit = tailExpr match {
          case GoAST.CompositeLiteral(_, elements, _) if elements.isEmpty => true
          case _ => false
        }
        val stmts =
          if returnsValue && !tailIsUnit then loweredElems :+ GoAST.Return(Vector(tailExpr), tail.span)
          else if returnsValue && tailIsUnit then loweredElems :+ GoAST.Return(Vector(unitValue(tail.span)), tail.span)
          else if !tailIsUnit then loweredElems :+ GoAST.ExprStmt(tailExpr, tail.span)
          else loweredElems
        GoAST.Block(stmts.toVector, span)
      case other =>
        val expr = lowerExpr(other, config, recordEnv, goImportEnv, effectEnv)
        val stmts =
          if returnsValue then Vector(GoAST.Return(Vector(expr), other.span))
          else Vector(GoAST.ExprStmt(expr, other.span))
        GoAST.Block(stmts, other.span)
  }

  /** Lower a block-like AST into a list of Go statements, appending a return for the tail when necessary. */
  private def lowerAsDeclarations(
      ast: AST,
      config: Config,
      recordEnv: RecordEnv,
      goImportEnv: GoImportEnv,
      effectEnv: EffectEnv,
      packageName: String,
      topLevel: Boolean
  ): (String, Vector[GoAST]) = {
    ast match
      case AST.Block(elems, tail, _) =>
        val (pkgAfterElems, loweredElems) = elems.foldLeft((packageName, Vector.empty[GoAST])) { case ((pkg, acc), stmt) =>
          val (nextPkg, stmts) = lowerStmt(stmt, config, recordEnv, goImportEnv, effectEnv, pkg)
          (nextPkg, acc ++ stmts)
        }
        val tailExpr = lowerExpr(tail, config, recordEnv, goImportEnv, effectEnv)
        val tailIsUnit = tailExpr match
          case GoAST.CompositeLiteral(_, elements, _) if elements.isEmpty => true
          case _                                                          => false
        val tailStmt = {
          if topLevel && tailIsUnit then Vector.empty
          else Vector(GoAST.Return(Vector(tailExpr), tail.span))
        }
        (pkgAfterElems, (loweredElems ++ tailStmt).toVector)
      case other =>
        val expr = lowerExpr(other, config, recordEnv, goImportEnv, effectEnv)
        val tailIsUnit = expr match
          case GoAST.CompositeLiteral(_, elements, _) if elements.isEmpty => true
          case _                                                          => false
        val stmt = {
          if topLevel && tailIsUnit then Vector.empty
          else Vector(GoAST.Return(Vector(expr), other.span))
        }
        (packageName, stmt)
  }

  private def collectArgs(expr: AST): (AST, Vector[Arg]) = {
    expr match {
      case AST.App(func, args, _, _) =>
        val (base, prevArgs) = collectArgs(func)
        (base, prevArgs ++ args)
      case _ => (expr, Vector.empty)
    }
  }

  /** Lower a Chester expression to a Go expression node. */
  private def lowerExpr(expr: AST, config: Config, recordEnv: RecordEnv, goImportEnv: GoImportEnv, effectEnv: EffectEnv): GoAST = {
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
          elems.map(e => GoCompositeElement(None, lowerExpr(e, config, recordEnv, goImportEnv, effectEnv), e.span)),
          span
        )
      case AST.Tuple(elems, span) =>
        if elems.isEmpty then unitValue(span)
        else {
          val sliceType = GoType.Slice(GoType.Named("any", span), span)
          GoAST.CompositeLiteral(
            sliceType,
            elems.map(e => GoCompositeElement(None, lowerExpr(e, config, recordEnv, goImportEnv, effectEnv), e.span)),
            span
          )
        }

      // Lambdas and application
      case AST.Lam(telescopes, body, span) =>
        val params = telescopes.flatMap(_.params).filter(_.coeffect != chester.core.Coeffect.Zero).map(p => lowerParam(p, config))
        val fnBody = GoAST.Block(Vector(GoAST.Return(Vector(lowerExpr(body, config, recordEnv, goImportEnv, effectEnv)), body.span)), body.span)
        GoAST.FuncLiteral(Vector.empty, params.toVector, Vector(anyResultField(span)), fnBody, span)

      case app @ AST.App(_, _, _, _) =>
        val (base, allArgs) = collectArgs(app)
        base match {
          case AST.Ref(_, "+", _) if allArgs.length == 2 =>
            val left = lowerExpr(allArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
            val right = lowerExpr(allArgs(1).value, config, recordEnv, goImportEnv, effectEnv)
            GoAST.Call(GoAST.Identifier("__chester_int_add", app.span), Vector(left, right), app.span)

          case AST.Ref(_, "-", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            if explicitArgs.length == 2 then
              val left = lowerExpr(explicitArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
              val right = lowerExpr(explicitArgs(1).value, config, recordEnv, goImportEnv, effectEnv)
              GoAST.Call(GoAST.Identifier("__chester_int_sub", app.span), Vector(left, right), app.span)
            else if explicitArgs.length == 1 then
              // Unary negation: -(x) = 0 - x
              val arg = lowerExpr(explicitArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
              GoAST.Call(GoAST.Identifier("__chester_int_sub", app.span), Vector(GoAST.IntLiteral("0", app.span), arg), app.span)
            else
              GoAST.Identifier("-", app.span)

          case AST.Ref(_, "prim__list_length", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val list = lowerExpr(explicitArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
            GoAST.Call(GoAST.Identifier("__chester_list_len", app.span), Vector(list), app.span)

          case AST.Ref(_, "prim__list_get", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val list = lowerExpr(explicitArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
            val index = lowerExpr(explicitArgs(1).value, config, recordEnv, goImportEnv, effectEnv)
            GoAST.Call(GoAST.Identifier("__chester_list_get", app.span), Vector(list, index), app.span)

          case AST.Ref(_, "prim__list_make", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val size = lowerExpr(explicitArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
            val generator = lowerExpr(explicitArgs(1).value, config, recordEnv, goImportEnv, effectEnv)

            GoAST.Call(GoAST.Identifier("__chester_list_make", app.span), Vector(size, generator), app.span)

          case AST.Ref(_, "prim__if_else", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val rawCond = lowerExpr(explicitArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
            // Wrap condition with __chester_as_bool so both bool and any-typed predicates work
            val cond = GoAST.Call(GoAST.Identifier("__chester_as_bool", app.span), Vector(rawCond), app.span)
            val thenVal = lowerExpr(explicitArgs(1).value, config, recordEnv, goImportEnv, effectEnv)
            val elseVal = lowerExpr(explicitArgs(2).value, config, recordEnv, goImportEnv, effectEnv)
            val ifStmt = GoAST.If(None, cond, GoAST.Block(Vector(GoAST.Return(Vector(thenVal), app.span)), app.span), None, app.span)
            val returnElse = GoAST.Return(Vector(elseVal), app.span)
            val body = GoAST.Block(Vector(ifStmt, returnElse), app.span)
            val funcLit = GoAST.FuncLiteral(Vector.empty, Vector.empty, Vector(anyResultField(app.span)), body, app.span)
            GoAST.Call(funcLit, Vector.empty, app.span)

          case AST.Ref(_, "prim__int_eq", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val a = lowerExpr(explicitArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
            val b = lowerExpr(explicitArgs(1).value, config, recordEnv, goImportEnv, effectEnv)
            GoAST.Call(GoAST.Identifier("__chester_int_eq", app.span), Vector(a, b), app.span)

          case AST.Ref(_, "prim__int_lt", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val a = lowerExpr(explicitArgs(0).value, config, recordEnv, goImportEnv, effectEnv)
            val b = lowerExpr(explicitArgs(1).value, config, recordEnv, goImportEnv, effectEnv)
            GoAST.Call(GoAST.Identifier("__chester_int_lt", app.span), Vector(a, b), app.span)

          case AST.Ref(_, name, _) if effectEnv.contains(name) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val loweredArgs = explicitArgs.map(a => lowerExpr(a.value, config, recordEnv, goImportEnv, effectEnv))
            GoAST.Call(GoAST.Identifier("__chester_do", app.span), GoAST.StringLiteral(name, app.span) +: loweredArgs, app.span)

          case _ =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val callee = lowerExpr(base, config, recordEnv, goImportEnv, effectEnv)
            val loweredArgs = explicitArgs.map(a => lowerExpr(a.value, config, recordEnv, goImportEnv, effectEnv))
            // Always generate a Call — even with no explicit args (e.g. main())
            GoAST.Call(callee, loweredArgs, app.span)
        }

      // Let-binding as an IIFE
      case AST.Let(_, name, _, value, body, span) =>
        val valueExpr = lowerExpr(value, config, recordEnv, goImportEnv, effectEnv)
        val assign = GoAST.Assign(Vector(GoAST.Identifier(name, span)), Vector(valueExpr), ":=", span)
        val bodyExpr = lowerExpr(body, config, recordEnv, goImportEnv, effectEnv)
        val block = GoAST.Block(Vector(assign, GoAST.Return(Vector(bodyExpr), span)), span)
        GoAST.Call(GoAST.FuncLiteral(Vector.empty, Vector.empty, Vector(anyResultField(span)), block, span), Vector.empty, span)

      case AST.Ann(expr, _, _) =>
        lowerExpr(expr, config, recordEnv, goImportEnv, effectEnv)

      // Lower synthetic go-import wrapper selections to direct Go package/member references.
      case AST.FieldAccess(AST.Ref(_, "go", _), packageField, span) if goImportEnv.contains(packageField) =>
        GoAST.Identifier(packageField, span)
      case AST.FieldAccess(AST.FieldAccess(AST.Ref(_, "go", _), packageField, _), member, span) if goImportEnv.contains(packageField) =>
        GoAST.Selector(GoAST.Identifier(packageField, span), member, span)

      // Data constructors and selections
      case AST.RecordCtor(id, fallbackName, args, span) =>
        recordEnv.get(id) match
          case Some((recordName, fieldNames)) =>
            val elems = fieldNames.zip(args).map { case (fieldName, arg) =>
              GoCompositeElement(Some(GoAST.Identifier(fieldName, span)), lowerExpr(arg, config, recordEnv, goImportEnv, effectEnv), arg.span)
            }
            GoAST.CompositeLiteral(GoType.Named(recordName, span), elems.toVector, span)
          case None =>
            val elems = args.zipWithIndex.map { case (arg, idx) =>
              GoCompositeElement(Some(GoAST.Identifier(s"_${idx + 1}", span)), lowerExpr(arg, config, recordEnv, goImportEnv, effectEnv), arg.span)
            }
            GoAST.CompositeLiteral(GoType.Named(fallbackName, span), elems.toVector, span)

      case AST.FieldAccess(target, field, span) =>
        GoAST.Selector(lowerExpr(target, config, recordEnv, goImportEnv, effectEnv), field, span)

      case AST.EnumCaseRef(_, _, enumName, caseName, span) =>
        GoAST.Selector(GoAST.Identifier(enumName, span), caseName, span)

      case AST.EnumCtor(_, _, enumName, caseName, args, span) =>
        val ctor = GoAST.Selector(GoAST.Identifier(enumName, span), caseName, span)
        GoAST.Call(ctor, args.map(a => lowerExpr(a, config, recordEnv, goImportEnv, effectEnv)), span)

      // Blocks become IIFEs only when they have statements; otherwise just return the tail
      case AST.Block(elems, tail, span) =>
        val stmts = elems.flatMap(lowerStmt(_, config, recordEnv, goImportEnv, effectEnv, "main")._2)
        if (stmts.isEmpty) {
          // No statements, just return the tail expression directly
          lowerExpr(tail, config, recordEnv, goImportEnv, effectEnv)
        } else {
          // Has statements, need IIFE to execute them
          val tailExpr = lowerExpr(tail, config, recordEnv, goImportEnv, effectEnv)
          val block = GoAST.Block((stmts :+ GoAST.Return(Vector(tailExpr), span)).toVector, span)
          val iife = GoAST.FuncLiteral(Vector.empty, Vector.empty, Vector(anyResultField(span)), block, span)
          GoAST.Call(iife, Vector.empty, span)
        }

      case AST.Handle(action, _, handlers, span) =>
        val pushStmts = handlers.map { case (opName, handlerAst) =>
          val handlerExpr = lowerExpr(handlerAst, config, recordEnv, goImportEnv, effectEnv)
          val bodyReturn = GoAST.Return(Vector(handlerExpr), span)
          val param = GoField(Vector("_args"), GoType.Named("any", span), None, isEmbedded = false, isVariadic = true, span = span)
          val handlerLit = GoAST.FuncLiteral(Vector.empty, Vector(param), Vector(anyResultField(span)), GoAST.Block(Vector(bodyReturn), span), span)
          GoAST.ExprStmt(GoAST.Call(GoAST.Identifier("__chester_push_handler", span), Vector(GoAST.StringLiteral(opName, span), handlerLit), span), span)
        }
        val popStmts = handlers.map { case (opName, _) =>
          val call = GoAST.Call(GoAST.Identifier("__chester_pop_handler", span), Vector(GoAST.StringLiteral(opName, span)), span)
          GoAST.Defer(call, span)
        }
        val actionExpr = lowerExpr(action, config, recordEnv, goImportEnv, effectEnv)
        val actionReturn = GoAST.Return(Vector(actionExpr), span)
        val block = GoAST.Block((pushStmts ++ popStmts :+ actionReturn).toVector, span)
        val iife = GoAST.FuncLiteral(Vector.empty, Vector.empty, Vector(anyResultField(span)), block, span)
        GoAST.Call(iife, Vector.empty, span)

      case AST.Do(op, args, span) =>
        val opName = op match {
          case AST.Ref(_, name, _) => GoAST.StringLiteral(name, span)
          case _ => GoAST.StringLiteral("<unknown>", span)
        }
        val loweredArgs = args.map(a => lowerExpr(a, config, recordEnv, goImportEnv, effectEnv))
        GoAST.Call(GoAST.Identifier("__chester_do", span), opName +: loweredArgs, span)

      // Fallback
      case _ =>
        GoAST.NilLiteral(expr.span)
  }

  private def collectEffectEnv(ast: AST): EffectEnv = {
    def fromStmt(stmt: StmtAST): EffectEnv = stmt match {
      case StmtAST.Effect(_, _, operations, _) =>
        operations.map(_.name).toSet
      case StmtAST.Def(_, _, _, _, body, _, _) =>
        fromAst(body)
      case StmtAST.ExprStmt(expr, _) =>
        fromAst(expr)
      case StmtAST.Pkg(_, body, _) =>
        fromAst(body)
      case _ => Set.empty
    }
    def fromAst(node: AST): EffectEnv = node match {
      case AST.Block(elems, tail, _) =>
        elems.map(fromStmt).foldLeft(Set.empty[String])(_ ++ _) ++ fromAst(tail)
      case AST.Let(_, _, _, value, body, _) =>
        fromAst(value) ++ fromAst(body)
      case AST.Handle(action, _, handlers, _) =>
        fromAst(action) ++ handlers.map(_._2).map(fromAst).foldLeft(Set.empty[String])(_ ++ _)
      case _ => Set.empty
    }
    fromAst(ast)
  }

  private def collectRecordEnv(ast: AST): RecordEnv = {
    def fromStmt(stmt: StmtAST): RecordEnv =
      stmt match
        case StmtAST.Record(id, name, fields, _) =>
          Map(id -> (name, fields.map(_.name)))
        case StmtAST.Def(_, _, _, _, body, _, _) =>
          fromAst(body)
        case StmtAST.ExprStmt(expr, _) =>
          fromAst(expr)
        case StmtAST.Pkg(_, body, _) =>
          fromAst(body)
        case StmtAST.Effect(_, _, _, _) =>
          Map.empty
        case _ =>
          Map.empty

    def fromAst(node: AST): RecordEnv =
      node match
        case AST.Block(elements, tail, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], (String, Vector[String])])(_ ++ fromStmt(_)) ++ fromAst(tail)
        case AST.Let(_, _, _, value, body, _) =>
          fromAst(value) ++ fromAst(body)
        case AST.Lam(_, body, _) =>
          fromAst(body)
        case AST.App(func, args, _, _) =>
          fromAst(func) ++ args.foldLeft(Map.empty[UniqidOf[AST], (String, Vector[String])])((acc, arg) => acc ++ fromAst(arg.value))
        case AST.Ann(expr, ty, _) =>
          fromAst(expr) ++ fromAst(ty)
        case AST.Tuple(elements, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], (String, Vector[String])])(_ ++ fromAst(_))
        case AST.ListLit(elements, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], (String, Vector[String])])(_ ++ fromAst(_))
        case AST.RecordCtor(_, _, args, _) =>
          args.foldLeft(Map.empty[UniqidOf[AST], (String, Vector[String])])(_ ++ fromAst(_))
        case AST.FieldAccess(target, _, _) =>
          fromAst(target)
        case AST.EnumCtor(_, _, _, _, args, _) =>
          args.foldLeft(Map.empty[UniqidOf[AST], (String, Vector[String])])(_ ++ fromAst(_))
        case AST.Handle(action, _, handlers, _) =>
          fromAst(action) ++ handlers.foldLeft(Map.empty[UniqidOf[AST], (String, Vector[String])])((acc, h) => acc ++ fromAst(h._2))
        case AST.Do(op, args, _) =>
          fromAst(op) ++ args.foldLeft(Map.empty[UniqidOf[AST], (String, Vector[String])])((acc, a) => acc ++ fromAst(a))
        case _ =>
          Map.empty

    fromAst(ast)
  }

  private def collectGoImportEnv(ast: AST): GoImportEnv = {
    def packageFieldFor(modulePath: String): String =
      modulePath.split('/').lastOption.getOrElse(modulePath).replace(".", "_")

    def fromStmt(stmt: StmtAST): GoImportEnv =
      stmt match
        case StmtAST.JSImport(_, localName, modulePath, _, _, _) if localName == "go" =>
          Map(packageFieldFor(modulePath) -> modulePath)
        case StmtAST.Def(_, _, _, _, body, _, _) =>
          fromAst(body)
        case StmtAST.ExprStmt(expr, _) =>
          fromAst(expr)
        case StmtAST.Pkg(_, body, _) =>
          fromAst(body)
        case StmtAST.Effect(_, _, _, _) =>
          Map.empty
        case _ =>
          Map.empty

    def fromAst(node: AST): GoImportEnv =
      node match
        case AST.Block(elements, tail, _) =>
          elements.foldLeft(Map.empty[String, String])(_ ++ fromStmt(_)) ++ fromAst(tail)
        case AST.Let(_, _, _, value, body, _) =>
          fromAst(value) ++ fromAst(body)
        case AST.Lam(_, body, _) =>
          fromAst(body)
        case AST.App(func, args, _, _) =>
          fromAst(func) ++ args.foldLeft(Map.empty[String, String])((acc, arg) => acc ++ fromAst(arg.value))
        case AST.Ann(expr, ty, _) =>
          fromAst(expr) ++ fromAst(ty)
        case AST.Tuple(elements, _) =>
          elements.foldLeft(Map.empty[String, String])(_ ++ fromAst(_))
        case AST.ListLit(elements, _) =>
          elements.foldLeft(Map.empty[String, String])(_ ++ fromAst(_))
        case AST.RecordCtor(_, _, args, _) =>
          args.foldLeft(Map.empty[String, String])(_ ++ fromAst(_))
        case AST.FieldAccess(target, _, _) =>
          fromAst(target)
        case AST.EnumCtor(_, _, _, _, args, _) =>
          args.foldLeft(Map.empty[String, String])(_ ++ fromAst(_))
        case _ =>
          Map.empty

    fromAst(ast)
  }

  private def isTopLevelCallToMain(ast: GoAST): Boolean =
    ast match
      case GoAST.Return(Vector(GoAST.Call(GoAST.Identifier("main", _), Vector(), _)), _) => true
      case GoAST.ExprStmt(GoAST.Call(GoAST.Identifier("main", _), Vector(), _), _)        => true
      case _                                                                               => false

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

  /** Lower a Chester type into a Go type.
   *  All scalar types are represented as `any` for uniform runtime representation.
   *  Only unit (empty tuple/struct) keeps its concrete type.
   */
  private def lowerType(ty: AST, config: Config): GoType = {
    val rewritten = if config.applyEffectCPS then EffectCPS.transformType(ty, config.cpsConfig) else ty
    rewritten match
      // Unit type stays concrete
      case AST.TupleType(elems, span) if elems.isEmpty => GoType.Struct(Vector.empty, span)
      // Function types stay concrete (needed for higher-order functions)
      case AST.Pi(telescopes, resultTy, _, span) =>
        val params = telescopes.flatMap(_.params).filter(_.coeffect != chester.core.Coeffect.Zero).map(p => lowerParam(p, config))
        val results = Vector(lowerResultField(resultTy, config))
        GoType.Func(Vector.empty, params.toVector, results, span)
      // Named record / enum types stay concrete
      case AST.RecordTypeRef(_, name, span) => GoType.Named(name, span)
      case AST.EnumTypeRef(_, name, span) => GoType.Named(name, span)
      // Everything else — integers, bools, strings, lists, etc. — is `any`
      case _ => GoType.Named("any", ty.span)
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
