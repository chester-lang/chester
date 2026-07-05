package chester.backend

import chester.core.{AST, Arg, Implicitness, StmtAST, Telescope}
import chester.syntax.{JavaAST, JavaField, JavaMethod}
import chester.uniqid.UniqidOf

/** Backend lowering from Chester core AST into Java AST for code generation. */
object JavaBackend:

  private type RecordEnv = Map[UniqidOf[AST], (String, Vector[String])]
  private type FuncEnv = Set[String]

  final case class Config()

  def lowerProgram(ast: AST, config: Config = Config(), packageName: String = "chester.main", className: String = "Main"): JavaAST.File = {
    val recordEnv = collectRecordEnv(ast)
    val funcEnv = collectFuncEnv(ast)
    val members = lowerAsMembers(ast, config, recordEnv, funcEnv)

    // Separate methods, nested classes, and executable statements
    val methods = members.collect { case m: JavaMethod => m }
    val classes = members.collect { case c: JavaAST.ClassDecl => c }
    val execStmts = members.collect { case s: JavaAST if !s.isInstanceOf[JavaAST.ClassDecl] => s }

    // If there are top-level statements, generate a public static void main
    val allMethods = if (execStmts.nonEmpty) {
      val mainStmts = execStmts.map {
        case JavaAST.Return(Some(expr), span) =>
          // Non-unit top-level expressions get printed
          val printSys = JavaAST.FieldAccess(JavaAST.Identifier("System", span), "out", span)
          JavaAST.ExprStmt(JavaAST.MethodCall(Some(printSys), "println", Vector(expr), span), span)
        case JavaAST.Return(None, span) =>
          JavaAST.Empty(span) // ignore unit returns at top-level
        case other => other
      }

      val mainMethod = JavaMethod(
        name = "main",
        returnType = "void",
        params = Vector(("String[]", "args")),
        isStatic = true,
        body = Some(JavaAST.Block(mainStmts, ast.span)),
        span = ast.span
      )
      methods :+ mainMethod
    } else {
      methods
    }

    val mainClass = JavaAST.ClassDecl(
      name = className,
      isStatic = false,
      fields = Vector.empty,
      methods = allMethods,
      nestedClasses = classes,
      span = ast.span
    )

    JavaAST.File(packageName, Vector.empty, mainClass.asInstanceOf[JavaAST.ClassDecl], ast.span)
  }

  private def lowerAsMembers(ast: AST, config: Config, recordEnv: RecordEnv, funcEnv: FuncEnv): Vector[Any] = {
    ast match
      case AST.Block(elems, tail, _) =>
        val stmts = elems.flatMap(e => lowerStmt(e, config, recordEnv, funcEnv))
        val tLowered = lowerExpr(tail, config, recordEnv, funcEnv)
        val tailStmt = if isUnitExpr(tail) then Vector(JavaAST.ExprStmt(tLowered, tail.span))
                       else Vector(JavaAST.Return(Some(tLowered), tail.span))
        stmts ++ tailStmt
      case expr =>
        val eLowered = lowerExpr(expr, config, recordEnv, funcEnv)
        if isUnitExpr(expr) then Vector(JavaAST.ExprStmt(eLowered, expr.span))
        else Vector(JavaAST.Return(Some(eLowered), expr.span))
  }

  private def lowerStmt(stmt: StmtAST, config: Config, recordEnv: RecordEnv, funcEnv: FuncEnv): Vector[Any] = {
    stmt match
      case StmtAST.ExprStmt(AST.Let(_, name, _, value, _, span), _) =>
        val init = lowerExpr(value, config, recordEnv, funcEnv)
        Vector(JavaAST.VariableDecl("var", name, Some(init), span))

      case StmtAST.ExprStmt(expr, span) =>
        Vector(JavaAST.ExprStmt(lowerExpr(expr, config, recordEnv, funcEnv), span))

      case StmtAST.Def(_, name, telescopes, resultTy, body, span, _) =>
        val params = telescopes
          .flatMap(_.params)
          .filter(_.coeffect != chester.core.Coeffect.Zero)
          .map(p => ("Object", p.name)) // Universal Object representation
        
        val isMain = name == "main"
        val actualName = if isMain then "user_main" else name
        val returnType = "Object"
        
        val loweredBody = lowerExpr(body, config, recordEnv, funcEnv)
        val bodyBlock = JavaAST.Block(Vector(JavaAST.Return(Some(loweredBody), body.span)), body.span)

        Vector(JavaMethod(
          name = actualName,
          returnType = returnType,
          params = params,
          isStatic = true,
          body = Some(bodyBlock.asInstanceOf[JavaAST.Block]),
          span = span
        ))

      case StmtAST.Record(_, name, fields, span) =>
        val javaFields = fields.map(f => JavaField("Object", f.name, isStatic = false, span = f.ty.span))
        val ctorParams = fields.map(f => ("Object", f.name))
        
        // Generate constructor manually
        val assigns = fields.map(f => JavaAST.Assign(JavaAST.FieldAccess(JavaAST.Identifier("this", f.ty.span), f.name, f.ty.span), JavaAST.Identifier(f.name, f.ty.span), "=", f.ty.span))
        val ctor = JavaMethod(
          name = name,
          returnType = "",
          params = ctorParams,
          isStatic = false,
          body = Some(JavaAST.Block(assigns, span)),
          span = span
        )
        
        Vector(JavaAST.ClassDecl(
          name = name,
          isStatic = true,
          fields = javaFields,
          methods = Vector(ctor),
          nestedClasses = Vector.empty,
          span = span
        ))
      
      case _ => Vector.empty
  }

  private def collectArgs(expr: AST): (AST, Vector[Arg]) = {
    expr match {
      case AST.App(func, args, _, _) =>
        val (base, prevArgs) = collectArgs(func)
        (base, prevArgs ++ args)
      case _ => (expr, Vector.empty)
    }
  }

  private def lowerExpr(expr: AST, config: Config, recordEnv: RecordEnv, funcEnv: FuncEnv): JavaAST = {
    expr match
      case AST.IntLit(value, span) => JavaAST.IntLiteral(value.toString, span)
      case AST.NaturalLit(value, span) => JavaAST.IntLiteral(value.toString, span)
      case AST.StringLit(value, span) => JavaAST.StringLiteral(value, span)
      case AST.Ref(_, "true", span) => JavaAST.BooleanLiteral(true, span)
      case AST.Ref(_, "false", span) => JavaAST.BooleanLiteral(false, span)
      case AST.Ref(_, name, span) =>
        if name == "main" then JavaAST.Identifier("user_main", span)
        else JavaAST.Identifier(name, span)
      
      case AST.ListLit(elems, span) =>
        val args = elems.map(e => lowerExpr(e, config, recordEnv, funcEnv))
        JavaAST.MethodCall(Some(JavaAST.Identifier("java.util.Arrays", span)), "asList", args, span)
        
      case AST.Tuple(elems, span) =>
        if elems.isEmpty then JavaAST.NullLiteral(span)
        else
          val args = elems.map(e => lowerExpr(e, config, recordEnv, funcEnv))
          JavaAST.MethodCall(Some(JavaAST.Identifier("java.util.Arrays", span)), "asList", args, span)
          
      case AST.Lam(telescopes, body, span) =>
        val params = telescopes.flatMap(_.params).filter(_.coeffect != chester.core.Coeffect.Zero).map(_.name)
        val assignArgs = params.zipWithIndex.map { case (p, i) =>
          JavaAST.VariableDecl("var", p, Some(JavaAST.ArrayAccess(JavaAST.Identifier("__args", span), JavaAST.IntLiteral(i.toString, span), span)), span)
        }
        val bodyExpr = lowerExpr(body, config, recordEnv, funcEnv)
        val block = JavaAST.Block(assignArgs :+ JavaAST.Return(Some(bodyExpr), body.span), body.span)
        JavaAST.Cast("ChesterFunc", JavaAST.Lambda(Vector("__args"), block, span), span)

      case app: AST.App =>
        val (base, allArgs) = collectArgs(app)
        base match {
          case AST.FieldAccess(AST.FieldAccess(AST.Ref(_, "go", _), "fmt", _), "Println", _) =>
            val msg = lowerExpr(allArgs(0).value, config, recordEnv, funcEnv)
            JavaAST.MethodCall(Some(JavaAST.FieldAccess(JavaAST.Identifier("System", app.span), "out", app.span)), "println", Vector(msg), app.span)
            
          case AST.FieldAccess(AST.FieldAccess(AST.Ref(_, "go", _), "fmt", _), "Sprintf", _) if allArgs.length == 3 =>
            // Specifically handling Sprintf("%s%s", a, b) for string concatenation
            val a = lowerExpr(allArgs(1).value, config, recordEnv, funcEnv)
            val b = lowerExpr(allArgs(2).value, config, recordEnv, funcEnv)
            JavaAST.Binary(JavaAST.Cast("String", a, app.span), "+", JavaAST.Cast("String", b, app.span), app.span)
            
          case AST.FieldAccess(AST.FieldAccess(AST.Ref(_, "go", _), "fmt", _), "Sprintf", _) if allArgs.length == 2 =>
            // Specifically handling Sprintf("%d", val) for int_to_string
            val msg = lowerExpr(allArgs(1).value, config, recordEnv, funcEnv)
            JavaAST.MethodCall(Some(JavaAST.Identifier("String", app.span)), "valueOf", Vector(msg), app.span)
            
          case AST.Ref(_, "+", _) if allArgs.length == 2 =>
            val left = lowerExpr(allArgs(0).value, config, recordEnv, funcEnv)
            val right = lowerExpr(allArgs(1).value, config, recordEnv, funcEnv)
            JavaAST.MethodCall(None, "__chester_int_add", Vector(left, right), app.span)
            
          case AST.Ref(_, "-", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            if explicitArgs.length == 2 then
              val left = lowerExpr(explicitArgs(0).value, config, recordEnv, funcEnv)
              val right = lowerExpr(explicitArgs(1).value, config, recordEnv, funcEnv)
              JavaAST.MethodCall(None, "__chester_int_sub", Vector(left, right), app.span)
            else if explicitArgs.length == 1 then
              val arg = lowerExpr(explicitArgs(0).value, config, recordEnv, funcEnv)
              JavaAST.MethodCall(None, "__chester_int_sub", Vector(JavaAST.IntLiteral("0", app.span), arg), app.span)
            else
              JavaAST.Identifier("-", app.span)
              
          case AST.Ref(_, "prim__list_length", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val listArg = explicitArgs.head.value
            JavaAST.MethodCall(None, "__chester_list_length", Vector(lowerExpr(listArg, config, recordEnv, funcEnv)), app.span)
            
          case AST.Ref(_, "prim__list_get", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val list = lowerExpr(explicitArgs(0).value, config, recordEnv, funcEnv)
            val index = lowerExpr(explicitArgs(1).value, config, recordEnv, funcEnv)
            JavaAST.MethodCall(None, "__chester_list_get", Vector(list, index), app.span)
            
          case AST.Ref(_, "prim__list_make", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val size = lowerExpr(explicitArgs(0).value, config, recordEnv, funcEnv)
            val generator = lowerExpr(explicitArgs(1).value, config, recordEnv, funcEnv)
            JavaAST.MethodCall(None, "__chester_list_make", Vector(size, generator), app.span)
            
          case AST.Ref(_, "prim__if_else", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val cond = JavaAST.MethodCall(None, "__chester_as_bool", Vector(lowerExpr(explicitArgs(0).value, config, recordEnv, funcEnv)), app.span)
            val thenVal = lowerExpr(explicitArgs(1).value, config, recordEnv, funcEnv)
            val elseVal = lowerExpr(explicitArgs(2).value, config, recordEnv, funcEnv)
            val thenSupplier = JavaAST.Lambda(Vector.empty, thenVal, app.span)
            val elseSupplier = JavaAST.Lambda(Vector.empty, elseVal, app.span)
            JavaAST.MethodCall(None, "__chester_if_else", Vector(cond, thenSupplier, elseSupplier), app.span)
            
          case AST.Ref(_, "prim__int_eq", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val a = lowerExpr(explicitArgs(0).value, config, recordEnv, funcEnv)
            val b = lowerExpr(explicitArgs(1).value, config, recordEnv, funcEnv)
            JavaAST.MethodCall(None, "__chester_int_eq", Vector(a, b), app.span)
            
          case AST.Ref(_, "prim__int_lt", _) =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val a = lowerExpr(explicitArgs(0).value, config, recordEnv, funcEnv)
            val b = lowerExpr(explicitArgs(1).value, config, recordEnv, funcEnv)
            JavaAST.MethodCall(None, "__chester_int_lt", Vector(a, b), app.span)
            
          case _ =>
            val explicitArgs = allArgs.filter(_.coeffect != chester.core.Coeffect.Zero)
            val callee = lowerExpr(base, config, recordEnv, funcEnv)
            val loweredArgs = explicitArgs.map(a => lowerExpr(a.value, config, recordEnv, funcEnv))
            base match {
              case AST.Ref(_, name, _) if funcEnv.contains(name) || name == "main" => 
                val actualName = if name == "main" then "user_main" else name
                JavaAST.MethodCall(None, actualName, loweredArgs, app.span)
              case _ => 
                JavaAST.MethodCall(Some(JavaAST.Paren(JavaAST.Cast("ChesterFunc", callee, app.span), app.span)), "apply", loweredArgs, app.span)
            }
        }
        
      case AST.Let(_, name, _, value, body, span) =>
        val valueExpr = lowerExpr(value, config, recordEnv, funcEnv)
        val assign = JavaAST.VariableDecl("var", name, Some(valueExpr), span)
        val block = JavaAST.Block(
          Vector(
            assign,
            JavaAST.Return(Some(lowerExpr(body, config, recordEnv, funcEnv)), body.span)
          ),
          span
        )
        JavaAST.MethodCall(Some(JavaAST.Paren(JavaAST.Cast("java.util.function.Supplier<Object>", JavaAST.Lambda(Vector.empty, block, span), span), span)), "get", Vector.empty, span)

      case AST.Ann(expr, _, _) => lowerExpr(expr, config, recordEnv, funcEnv)

      case AST.RecordCtor(id, _, args, span) =>
        val recordName = recordEnv.get(id).map(_._1).getOrElse("UnknownRecord")
        JavaAST.ObjectCreation(recordName, args.map(a => lowerExpr(a, config, recordEnv, funcEnv)), span)

      case AST.FieldAccess(target, field, span) =>
        JavaAST.FieldAccess(lowerExpr(target, config, recordEnv, funcEnv), field, span)

      case AST.Block(elems, tail, span) =>
        val stmts = elems.flatMap(e => lowerStmt(e, config, recordEnv, funcEnv)).asInstanceOf[Vector[JavaAST]]
        val ret = JavaAST.Return(Some(lowerExpr(tail, config, recordEnv, funcEnv)), tail.span)
        val block = JavaAST.Block(stmts :+ ret, span)
        JavaAST.MethodCall(Some(JavaAST.Paren(JavaAST.Cast("java.util.function.Supplier<Object>", JavaAST.Lambda(Vector.empty, block, span), span), span)), "get", Vector.empty, span)

      case AST.Handle(action, _, _, _) =>
        lowerExpr(action, config, recordEnv, funcEnv)

      case AST.Do(op, args, span) =>
        JavaAST.MethodCall(Some(lowerExpr(op, config, recordEnv, funcEnv)), "apply", args.map(a => lowerExpr(a, config, recordEnv, funcEnv)), span)

      case _ => JavaAST.NullLiteral(expr.span)
  }

  private def collectRecordEnv(ast: AST): RecordEnv = {
    var env = Map.empty[UniqidOf[AST], (String, Vector[String])]
    def visit(a: AST): Unit = a match {
      case b: AST.Block =>
        b.elements.foreach {
          case StmtAST.Record(id, name, fields, _) =>
            env += (id -> (name, fields.map(_.name)))
          case _ =>
        }
        b.elements.foreach {
          case s: StmtAST.ExprStmt => visit(s.expr)
          case _ =>
        }
        visit(b.tail)
      case _ => // shallow scan for now
    }
    visit(ast)
    env
  }

  private def collectFuncEnv(ast: AST): FuncEnv = {
    var env = Set.empty[String]
    def visit(a: AST): Unit = a match {
      case b: AST.Block =>
        b.elements.foreach {
          case StmtAST.Def(_, name, _, _, _, _, _) => env += name
          case _ =>
        }
        b.elements.foreach {
          case s: StmtAST.ExprStmt => visit(s.expr)
          case _ =>
        }
        visit(b.tail)
      case _ =>
    }
    visit(ast)
    env
  }

  private def isUnitExpr(ast: AST): Boolean = ast match {
    case AST.Tuple(elems, _) if elems.isEmpty => true
    case _ => false
  }


