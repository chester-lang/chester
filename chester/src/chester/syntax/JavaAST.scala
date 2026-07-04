package chester.syntax

import chester.error.Span
import chester.utils.doc.*
import chester.utils.doc.given

import scala.language.experimental.genericNumberLiterals

/** Java AST representation for code generation */
enum JavaAST extends ToDoc:
  // Expressions
  case IntLiteral(value: String, span: Option[Span])
  case FloatLiteral(value: String, span: Option[Span])
  case StringLiteral(value: String, span: Option[Span])
  case CharLiteral(value: String, span: Option[Span])
  case BooleanLiteral(value: Boolean, span: Option[Span])
  case NullLiteral(span: Option[Span])
  case Identifier(name: String, span: Option[Span])
  case FieldAccess(expr: JavaAST, field: String, span: Option[Span])
  case ArrayAccess(expr: JavaAST, index: JavaAST, span: Option[Span])
  case MethodCall(target: Option[JavaAST], methodName: String, args: Vector[JavaAST], span: Option[Span])
  case ObjectCreation(className: String, args: Vector[JavaAST], span: Option[Span])
  case Lambda(params: Vector[String], body: JavaAST, span: Option[Span])
  case Unary(op: String, expr: JavaAST, span: Option[Span])
  case Binary(left: JavaAST, op: String, right: JavaAST, span: Option[Span])
  case Paren(expr: JavaAST, span: Option[Span])
  case Cast(tpe: String, expr: JavaAST, span: Option[Span])

  // Statements
  case Block(statements: Vector[JavaAST], span: Option[Span])
  case ExprStmt(expr: JavaAST, span: Option[Span])
  case Return(value: Option[JavaAST], span: Option[Span])
  case VariableDecl(tpe: String, name: String, init: Option[JavaAST], span: Option[Span])
  case If(cond: JavaAST, thenBranch: JavaAST, elseBranch: Option[JavaAST], span: Option[Span])
  case Assign(lhs: JavaAST, rhs: JavaAST, op: String, span: Option[Span])
  case Empty(span: Option[Span])
  
  // Declarations
  case ClassDecl(
      name: String,
      isStatic: Boolean,
      fields: Vector[JavaField],
      methods: Vector[JavaMethod],
      nestedClasses: Vector[JavaAST], // For records/classes inside Main
      span: Option[Span]
  )

  // File
  case File(packageName: String, imports: Vector[String], mainClass: JavaAST.ClassDecl, span: Option[Span])

  def toDoc(using DocConf): Doc = JavaAST.toDoc(this)

case class JavaField(tpe: String, name: String, isStatic: Boolean, span: Option[Span])
case class JavaMethod(
    name: String,
    returnType: String,
    params: Vector[(String, String)], // type, name
    isStatic: Boolean,
    body: Option[JavaAST.Block],
    span: Option[Span]
)

object JavaAST:
  def escapeString(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")

  def toDoc(ast: JavaAST)(using DocConf): Doc = ast match
    case IntLiteral(value, _) => text(value)
    case FloatLiteral(value, _) => text(value)
    case StringLiteral(value, _) => text("\"") <> text(escapeString(value)) <> text("\"")
    case CharLiteral(value, _) => text("'") <> text(value) <> text("'")
    case BooleanLiteral(value, _) => text(value.toString)
    case NullLiteral(_) => text("null")
    case Identifier(name, _) => text(name)
    case FieldAccess(expr, field, _) => toDoc(expr) <> text(".") <> text(field)
    case ArrayAccess(expr, index, _) => toDoc(expr) <> text("[") <> toDoc(index) <> text("]")
    case MethodCall(target, methodName, args, _) =>
      val targetDoc = target match
        case Some(t) => toDoc(t) <> text(".")
        case None => empty
      targetDoc <> text(methodName) <> text("(") <> hsep(args.map(toDoc), text(", ")) <> text(")")
    case ObjectCreation(className, args, _) =>
      text("new ") <> text(className) <> text("(") <> hsep(args.map(toDoc), text(", ")) <> text(")")
    case Lambda(params, body, _) =>
      val paramsDoc = if params.length == 1 then text(params.head) else text("(") <> hsep(params.map(text), text(", ")) <> text(")")
      paramsDoc <+> text("->") <+> toDoc(body)
    case Unary(op, expr, _) => text(op) <> toDoc(expr)
    case Binary(left, op, right, _) => toDoc(left) <+> text(op) <+> toDoc(right)
    case Paren(expr, _) => text("(") <> toDoc(expr) <> text(")")
    case Cast(tpe, expr, _) => text("(") <> text(tpe) <> text(")") <+> toDoc(expr)

    case Block(statements, _) =>
      if statements.isEmpty then text("{}")
      else text("{") <@@> ssep(statements.map(toDoc), hardline).indented() <@@> text("}")
    case ExprStmt(expr, _) => toDoc(expr) <> text(";")
    case Return(value, _) =>
      value match
        case Some(v) => text("return") <+> toDoc(v) <> text(";")
        case None => text("return;")
    case VariableDecl(tpe, name, init, _) =>
      val decl = text(tpe) <+> text(name)
      init match
        case Some(v) => decl <+> text("=") <+> toDoc(v) <> text(";")
        case None => decl <> text(";")
    case If(cond, thenBranch, elseBranch, _) =>
      val base = text("if (") <> toDoc(cond) <> text(") ") <> toDoc(thenBranch)
      elseBranch match
        case Some(eb) => base <+> text("else") <+> toDoc(eb)
        case None => base
    case Assign(lhs, rhs, op, _) => toDoc(lhs) <+> text(op) <+> toDoc(rhs) <> text(";")
    case Empty(_) => empty
    
    case ClassDecl(name, isStatic, fields, methods, nestedClasses, _) =>
      val staticDoc = if isStatic then text("static ") else empty
      val header = staticDoc <> text("class") <+> text(name) <+> text("{")
      val fieldsDoc = fields.map(f => (if f.isStatic then text("static ") else empty) <> text(f.tpe) <+> text(f.name) <> text(";"))
      val methodsDoc = methods.map { m =>
        val staticM = if m.isStatic then text("public static ") else text("public ")
        val paramsM = hsep(m.params.map((t, n) => text(t) <+> text(n)), text(", "))
        val sig = staticM <> text(m.returnType) <+> text(m.name) <> text("(") <> paramsM <> text(")")
        m.body match
          case Some(b) => sig <+> toDoc(b)
          case None => sig <> text(";")
      }
      val nestedDoc = nestedClasses.map(toDoc)
      val body = ssep(fieldsDoc ++ methodsDoc ++ nestedDoc, hardline <@@> hardline)
      if body == empty then header <> text("}")
      else header <@@> body.indented() <@@> text("}")
      
    case File(packageName, imports, mainClass, _) =>
      val pkgDoc = if packageName.nonEmpty then text("package") <+> text(packageName) <> text(";") else empty
      val importsDoc = ssep(imports.map(i => text("import") <+> text(i) <> text(";")), hardline)
      val parts = Vector(pkgDoc, importsDoc, toDoc(mainClass)).filter(_ != empty)
      ssep(parts, hardline <@@> hardline)
