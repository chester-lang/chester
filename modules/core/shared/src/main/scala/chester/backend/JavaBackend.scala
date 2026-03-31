package chester.backend

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Param, StmtAST}
import chester.uniqid.UniqidOf
import chester.utils.doc.{Doc, DocConf, ToDoc}
import chester.transform.EffectCPS

/** Minimal Java backend for the same practical subset exercised by the TypeScript/Go backends. */
object JavaBackend:

  final case class Config(
      applyEffectCPS: Boolean = false,
      cpsConfig: EffectCPS.Config = EffectCPS.Config()
  )

  final case class CompilationUnit(code: String) extends ToDoc:
    def toDoc(using DocConf): Doc = Doc.text(code)

  private type RecordEnv = Map[UniqidOf[AST], String]

  def lowerProgram(ast: AST, className: String = "Main", config: Config = Config()): CompilationUnit =
    val recordEnv = collectRecordEnv(ast)
    val (nestedClasses, methods, mainStatements) = lowerTopLevel(ast, config, recordEnv)

    val imports = Vector("import java.util.function.Supplier;")
    val bodyParts = (nestedClasses ++ methods ++ mainStatements).filter(_.nonEmpty)
    val body =
      if bodyParts.isEmpty then ""
      else bodyParts.mkString("\n\n")

    val code =
      s"""${imports.mkString("", "\n", "\n\n")}public final class $className {
         |${indent(body)}
         |}
         |""".stripMargin
    CompilationUnit(code)

  private def lowerTopLevel(ast: AST, config: Config, recordEnv: RecordEnv): (Vector[String], Vector[String], Vector[String]) =
    ast match
      case AST.Block(elements, tail, _) =>
        val (nestedClasses, methods, stmts) = elements.foldLeft((Vector.empty[String], Vector.empty[String], Vector.empty[String])) {
          case ((classesAcc, methodsAcc, stmtsAcc), stmt) =>
            val (classes, methods, stmts) = lowerStmt(stmt, config, recordEnv)
            (classesAcc ++ classes, methodsAcc ++ methods, stmtsAcc ++ stmts)
        }
        val tailStmts =
          if isUnitExpr(tail) then Vector.empty
          else Vector(s"System.out.println(${lowerExpr(tail, config, recordEnv)});")
        val mainMethod = renderMainMethod(stmts ++ tailStmts)
        (nestedClasses, methods, Vector(mainMethod))
      case other =>
        val mainBody =
          if isUnitExpr(other) then Vector.empty
          else Vector(s"System.out.println(${lowerExpr(other, config, recordEnv)});")
        (Vector.empty, Vector.empty, Vector(renderMainMethod(mainBody)))

  private def renderMainMethod(statements: Vector[String]): String =
    val body = if statements.isEmpty then "" else statements.mkString("\n")
    s"""public static void main(String[] args) {
       |${indent(body)}
       |}""".stripMargin

  private def lowerStmt(stmt: StmtAST, config: Config, recordEnv: RecordEnv): (Vector[String], Vector[String], Vector[String]) =
    stmt match
      case StmtAST.ExprStmt(AST.Let(_, name, _, value, _, _), _) =>
        (Vector.empty, Vector.empty, Vector(s"final var $name = ${lowerExpr(value, config, recordEnv)};"))
      case StmtAST.ExprStmt(expr, _) =>
        if isUnitExpr(expr) then (Vector.empty, Vector.empty, Vector.empty)
        else (Vector.empty, Vector.empty, Vector(s"${lowerExpr(expr, config, recordEnv)};"))
      case StmtAST.Def(_, name, telescopes, resultTy, body, _) =>
        val params = telescopes.flatMap(_.params).map(lowerParam(_, config)).mkString(", ")
        val returnsVoid = resultTy.exists(isUnitType)
        val returnType = if returnsVoid then "void" else resultTy.map(lowerType(_, config)).getOrElse("Object")
        val bodyCode = lowerMethodBody(body, returnsVoid, config, recordEnv)
        val method =
          s"""public static $returnType $name($params) {
             |${indent(bodyCode)}
             |}""".stripMargin
        (Vector.empty, Vector(method), Vector.empty)
      case StmtAST.Record(id, name, fields, _) =>
        val fieldDecls = fields.map(p => s"public final ${lowerType(p.ty, config)} ${p.name};")
        val ctorParams = fields.map(lowerParam(_, config)).mkString(", ")
        val ctorAssignments = fields.map(p => s"this.${p.name} = ${p.name};")
        val ctor =
          s"""public $name($ctorParams) {
             |${indent(ctorAssignments.mkString("\n"))}
             |}""".stripMargin
        val cls =
          s"""public static final class $name {
             |${indent((fieldDecls :+ ctor).mkString("\n\n"))}
             |}""".stripMargin
        (Vector(cls), Vector.empty, Vector.empty)
      case StmtAST.Pkg(_, body, _) =>
        lowerTopLevel(body, config, recordEnv)
      case _ =>
        (Vector.empty, Vector.empty, Vector.empty)

  private def lowerMethodBody(body: AST, returnsVoid: Boolean, config: Config, recordEnv: RecordEnv): String =
    body match
      case AST.Block(elements, tail, _) =>
        val stmts = elements.flatMap(lowerStmt(_, config, recordEnv)._3)
        val tailStmts =
          if returnsVoid then
            if isUnitExpr(tail) then Vector.empty else Vector(s"${lowerExpr(tail, config, recordEnv)};")
          else
            Vector(s"return ${lowerExpr(tail, config, recordEnv)};")
        (stmts ++ tailStmts).mkString("\n")
      case other =>
        if returnsVoid then
          if isUnitExpr(other) then ""
          else s"${lowerExpr(other, config, recordEnv)};"
        else s"return ${lowerExpr(other, config, recordEnv)};"

  private def lowerExpr(expr: AST, config: Config, recordEnv: RecordEnv): String =
    expr match
      case AST.IntLit(value, _)     => value.toString
      case AST.NaturalLit(value, _) => value.toString
      case AST.LevelLit(value, _)   => value.toString
      case AST.StringLit(value, _)  => "\"" + escapeJavaString(value) + "\""
      case AST.Ref(_, name, _)      => name
      case AST.Tuple(elements, _) =>
        if elements.isEmpty then "null"
        else s"java.util.List.of(${elements.map(lowerExpr(_, config, recordEnv)).mkString(", ")})"
      case AST.ListLit(elements, _) =>
        s"java.util.List.of(${elements.map(lowerExpr(_, config, recordEnv)).mkString(", ")})"
      case AST.App(AST.Ref(_, "+", _), args, _, _) if args.length == 2 =>
        s"${lowerExpr(args(0).value, config, recordEnv)} + ${lowerExpr(args(1).value, config, recordEnv)}"
      case AST.App(func, args, _, _) =>
        s"${lowerExpr(func, config, recordEnv)}(${args.map(a => lowerExpr(a.value, config, recordEnv)).mkString(", ")})"
      case AST.Let(_, name, _, value, body, _) =>
        s"((Supplier<Object>)(() -> { final var $name = ${lowerExpr(value, config, recordEnv)}; return ${lowerExpr(body, config, recordEnv)}; })).get()"
      case AST.Ann(inner, _, _) =>
        lowerExpr(inner, config, recordEnv)
      case AST.RecordCtor(id, fallbackName, args, _) =>
        val recordName = recordEnv.getOrElse(id, fallbackName)
        s"new $recordName(${args.map(lowerExpr(_, config, recordEnv)).mkString(", ")})"
      case AST.FieldAccess(target, field, _) =>
        s"${lowerExpr(target, config, recordEnv)}.$field"
      case AST.Block(elements, tail, _) =>
        val stmts = elements.flatMap(lowerStmt(_, config, recordEnv)._3)
        val stmtsCode = if stmts.isEmpty then "" else stmts.mkString(" ", " ", " ")
        s"((Supplier<Object>)(() -> {$stmtsCode return ${lowerExpr(tail, config, recordEnv)}; })).get()"
      case _ =>
        "null"

  private def lowerParam(param: Param, config: Config): String =
    s"${lowerType(param.ty, config)} ${param.name}"

  private def lowerType(ty: AST, config: Config): String =
    val rewritten = if config.applyEffectCPS then EffectCPS.transformType(ty, config.cpsConfig) else ty
    rewritten match
      case AST.StringType(_)  => "String"
      case AST.IntegerType(_) => "int"
      case AST.NaturalType(_) => "int"
      case AST.BoolType(_)    => "boolean"
      case AST.AnyType(_)     => "Object"
      case AST.TupleType(elements, _) if elements.isEmpty => "void"
      case AST.ListType(_, _)                             => "java.util.List<Object>"
      case AST.RecordTypeRef(_, name, _)                  => name
      case _                                              => "Object"

  private def collectRecordEnv(ast: AST): RecordEnv =
    def fromStmt(stmt: StmtAST): RecordEnv =
      stmt match
        case StmtAST.Record(id, name, _, _) => Map(id -> name)
        case StmtAST.Def(_, _, _, _, body, _) => fromAst(body)
        case StmtAST.ExprStmt(expr, _) => fromAst(expr)
        case StmtAST.Pkg(_, body, _) => fromAst(body)
        case _ => Map.empty

    def fromAst(node: AST): RecordEnv =
      node match
        case AST.Block(elements, tail, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], String])(_ ++ fromStmt(_)) ++ fromAst(tail)
        case AST.Let(_, _, _, value, body, _) =>
          fromAst(value) ++ fromAst(body)
        case AST.Lam(_, body, _) =>
          fromAst(body)
        case AST.App(func, args, _, _) =>
          fromAst(func) ++ args.foldLeft(Map.empty[UniqidOf[AST], String])((acc, arg) => acc ++ fromAst(arg.value))
        case AST.Ann(expr, ty, _) =>
          fromAst(expr) ++ fromAst(ty)
        case AST.Tuple(elements, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], String])(_ ++ fromAst(_))
        case AST.ListLit(elements, _) =>
          elements.foldLeft(Map.empty[UniqidOf[AST], String])(_ ++ fromAst(_))
        case AST.RecordCtor(_, _, args, _) =>
          args.foldLeft(Map.empty[UniqidOf[AST], String])(_ ++ fromAst(_))
        case AST.FieldAccess(target, _, _) =>
          fromAst(target)
        case _ =>
          Map.empty

    fromAst(ast)

  private def isUnitExpr(ast: AST): Boolean =
    ast match
      case AST.Tuple(elements, _) if elements.isEmpty => true
      case _                                          => false

  private def isUnitType(ast: AST): Boolean =
    ast match
      case AST.TupleType(elements, _) if elements.isEmpty => true
      case _                                              => false

  private def indent(code: String, spaces: Int = 4): String =
    if code.isEmpty then ""
    else code.linesIterator.map((" " * spaces) + _).mkString("\n") + "\n"

  private def escapeJavaString(value: String): String =
    value
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\t", "\\t")
      .replace("\r", "\\r")
