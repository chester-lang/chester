package chester.syntax

import scala.language.experimental.genericNumberLiterals

import chester.error.Span
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*

/** Go AST representation for code generation */
enum GoAST extends ToDoc derives ReadWriter {
  // Expressions
  case IntLiteral(value: String, span: Option[Span])
  case FloatLiteral(value: String, span: Option[Span])
  case StringLiteral(value: String, span: Option[Span])
  case RuneLiteral(value: String, span: Option[Span])
  case BoolLiteral(value: Boolean, span: Option[Span])
  case NilLiteral(span: Option[Span])
  case Identifier(name: String, span: Option[Span])
  case Selector(expr: GoAST, field: String, span: Option[Span])
  case Index(expr: GoAST, index: GoAST, span: Option[Span])
  case CompositeLiteral(tpe: GoType, elements: Vector[GoCompositeElement], span: Option[Span])
  case FuncLiteral(
      typeParams: Vector[GoTypeParam],
      params: Vector[GoField],
      results: Vector[GoField],
      body: GoAST,
      span: Option[Span]
  )
  case Call(func: GoAST, args: Vector[GoAST], span: Option[Span])
  case Unary(op: String, expr: GoAST, span: Option[Span])
  case Binary(left: GoAST, op: String, right: GoAST, span: Option[Span])
  case Paren(expr: GoAST, span: Option[Span])

  // Statements
  case Block(statements: Vector[GoAST], span: Option[Span])
  case ExprStmt(expr: GoAST, span: Option[Span])
  case Return(values: Vector[GoAST], span: Option[Span])
  case Assign(lhs: Vector[GoAST], rhs: Vector[GoAST], op: String, span: Option[Span])
  case If(init: Option[GoAST], cond: GoAST, body: GoAST, elseBranch: Option[GoAST], span: Option[Span])
  case For(init: Option[GoAST], cond: Option[GoAST], post: Option[GoAST], body: GoAST, span: Option[Span])
  case Range(key: Option[GoAST], value: Option[GoAST], expr: GoAST, op: String, body: GoAST, span: Option[Span])
  case Defer(call: GoAST, span: Option[Span])
  case Empty(span: Option[Span])

  // Declarations
  case FuncDecl(
      name: String,
      typeParams: Vector[GoTypeParam],
      params: Vector[GoField],
      results: Vector[GoField],
      body: Option[GoAST],
      receiver: Option[GoField],
      span: Option[Span]
  )
  case TypeDecl(specs: Vector[GoTypeSpec], span: Option[Span])
  case ValueDecl(kind: GoValueDeclKind, specs: Vector[GoValueSpec], span: Option[Span])

  // File root
  case File(packageName: String, imports: Vector[GoImportSpec], decls: Vector[GoAST], span: Option[Span])

  def toDoc(using DocConf): Doc = GoAST.toDoc(this)
}

object GoAST {
  def toDoc(ast: GoAST)(using DocConf): Doc = ast match {
    // Expressions
    case GoAST.IntLiteral(value, _)   => text(value)
    case GoAST.FloatLiteral(value, _) => text(value)
    case GoAST.StringLiteral(value, _) =>
      text("\"") <> text(escapeString(value)) <> text("\"")
    case GoAST.RuneLiteral(value, _) => text("'") <> text(escapeRune(value)) <> text("'")
    case GoAST.BoolLiteral(value, _) => text(value.toString)
    case GoAST.NilLiteral(_)         => text("nil")
    case GoAST.Identifier(name, _)   => text(name)
    case GoAST.Selector(expr, field, _) =>
      toDoc(expr) <> text(".") <> text(field)
    case GoAST.Index(expr, index, _) =>
      toDoc(expr) <> text("[") <> toDoc(index) <> text("]")
    case GoAST.CompositeLiteral(tpe, elements, _) =>
      val typeDoc = typeToDoc(tpe)
      if elements.isEmpty then typeDoc <> text("{}")
      else typeDoc <> text("{") <@@> ssep(elements.map(compositeElemToDoc), hardline).indented() <@@> text("}")
    case GoAST.FuncLiteral(typeParams, params, results, body, _) =>
      text("func") <> typeParamListDoc(typeParams) <> paramListDoc(params) <> resultListDoc(results) <+> toDoc(body)
    case GoAST.Call(func, args, _) =>
      toDoc(func) <> text("(") <> hsep(args.map(toDoc), text(", ")) <> text(")")
    case GoAST.Unary(op, expr, _) => text(op) <> toDoc(expr)
    case GoAST.Binary(left, op, right, _) =>
      toDoc(left) <+> text(op) <+> toDoc(right)
    case GoAST.Paren(expr, _) => text("(") <> toDoc(expr) <> text(")")

    // Statements
    case GoAST.Block(statements, _) =>
      if statements.isEmpty then text("{}")
      else text("{") <@@> ssep(statements.map(toDoc), hardline).indented() <@@> text("}")
    case GoAST.ExprStmt(expr, _) =>
      toDoc(expr)
    case GoAST.Return(values, _) =>
      if values.isEmpty then text("return")
      else text("return") <+> hsep(values.map(toDoc), text(", "))
    case GoAST.Assign(lhs, rhs, op, _) =>
      hsep(lhs.map(toDoc), text(", ")) <+> text(op) <+> hsep(rhs.map(toDoc), text(", "))
    case GoAST.If(init, cond, body, elseBranch, _) =>
      val initDoc = init.map(i => toDoc(i) <> text("; ")).getOrElse(empty)
      val elseDoc = elseBranch.map(e => text(" else ") <> toDoc(e)).getOrElse(empty)
      text("if ") <> initDoc <> toDoc(cond) <+> toDoc(body) <> elseDoc
    case GoAST.For(init, cond, post, body, _) =>
      (init, cond, post) match
        case (None, None, None) =>
          text("for") <+> toDoc(body)
        case (None, Some(c), None) =>
          text("for ") <> toDoc(c) <+> toDoc(body)
        case _ =>
          val initDoc = init.map(toDoc).getOrElse(empty)
          val condDoc = cond.map(toDoc).getOrElse(empty)
          val postDoc = post.map(toDoc).getOrElse(empty)
          text("for ") <> initDoc <> text("; ") <> condDoc <> text("; ") <> postDoc <+> toDoc(body)
    case GoAST.Range(key, value, expr, op, body, _) =>
      val head = (key, value) match
        case (None, None)       => text("for range ")
        case (Some(k), None)    => text("for ") <> toDoc(k) <+> text(op) <+> text("range ")
        case (Some(k), Some(v)) => text("for ") <> hsep(Vector(k, v).map(toDoc), text(", ")) <+> text(op) <+> text("range ")
        case (None, Some(v))    => text("for ") <> toDoc(v) <+> text(op) <+> text("range ")
      head <> toDoc(expr) <+> toDoc(body)
    case GoAST.Defer(call, _) =>
      text("defer ") <> toDoc(call)
    case GoAST.Empty(_) =>
      empty

    // Declarations
    case GoAST.FuncDecl(name, typeParams, params, results, body, receiver, _) =>
      val receiverDoc = receiver.map(r => text("(") <> fieldToDoc(r) <> text(") ")).getOrElse(empty)
      val signature =
        text("func ") <> receiverDoc <> text(name) <> typeParamListDoc(typeParams) <> paramListDoc(params) <> resultListDoc(results)
      body.map(b => signature <+> toDoc(b)).getOrElse(signature)
    case GoAST.TypeDecl(specs, _) =>
      specs.toList match
        case spec :: Nil => text("type ") <> typeSpecToDoc(spec)
        case _ =>
          text("type (") <@@> ssep(specs.map(typeSpecToDoc), hardline).indented() <@@> text(")")
    case GoAST.ValueDecl(kind, specs, _) =>
      specs.toList match
        case spec :: Nil => text(kind.keyword) <+> valueSpecToDoc(spec)
        case _ =>
          text(kind.keyword) <+> text("(") <@@> ssep(specs.map(valueSpecToDoc), hardline).indented() <@@> text(")")

    case GoAST.File(packageName, imports, decls, _) =>
      val packageDoc = text("package ") <> text(packageName)
      val importsDoc = {
        if imports.isEmpty then empty
        else {
          val rendered = imports.map(importSpecToDoc)
          if rendered.length == 1 then hardline <> text("import ") <> rendered.head
          else hardline <> text("import (") <@@> ssep(rendered, hardline).indented() <@@> text(")")
        }
      }
      val declsDoc =
        if decls.isEmpty then empty else hardline <> hardline <> ssep(decls.map(toDoc), hardline <> hardline)
      packageDoc <> importsDoc <> declsDoc
  }

  def typeToDoc(tpe: GoType)(using DocConf): Doc = tpe match {
    case GoType.Named(name, _) =>
      text(name)
    case GoType.Pointer(inner, _) =>
      text("*") <> typeToDoc(inner)
    case GoType.Slice(elem, _) =>
      text("[]") <> typeToDoc(elem)
    case GoType.Array(length, elem, _) =>
      val lengthDoc = length.map(toDoc).getOrElse(text("..."))
      text("[") <> lengthDoc <> text("]") <> typeToDoc(elem)
    case GoType.Map(key, value, _) =>
      text("map[") <> typeToDoc(key) <> text("]") <> typeToDoc(value)
    case GoType.Struct(fields, _) =>
      if fields.isEmpty then text("struct {}")
      else text("struct {") <@@> ssep(fields.map(fieldToDoc), hardline).indented() <@@> text("}")
    case GoType.Interface(methods, _) =>
      if methods.isEmpty then text("interface {}")
      else text("interface {") <@@> ssep(methods.map(fieldToDoc), hardline).indented() <@@> text("}")
    case GoType.Func(typeParams, params, results, _) =>
      text("func") <> typeParamListDoc(typeParams) <> paramListDoc(params) <> resultListDoc(results)
  }

  def fieldToDoc(field: GoField)(using DocConf): Doc = {
    val namesDoc =
      if field.names.nonEmpty then hsep(field.names.map(n => text(n)), text(", ")) else empty
    val variadicDoc = if field.isVariadic then text("...") else empty
    val typeDoc = variadicDoc <> typeToDoc(field.fieldType)
    val head =
      if field.isEmbedded || field.names.isEmpty then typeDoc else namesDoc <+> typeDoc
    val tagDoc = field.tag.map(t => text(" `") <> text(escapeTag(t)) <> text("`")).getOrElse(empty)
    head <> tagDoc
  }

  def typeSpecToDoc(spec: GoTypeSpec)(using DocConf): Doc = {
    val typeParamsDoc = typeParamListDoc(spec.typeParams)
    val assignDoc = if spec.isAlias then text(" = ") else text(" ")
    text(spec.name) <> typeParamsDoc <> assignDoc <> typeToDoc(spec.tpe)
  }

  def valueSpecToDoc(spec: GoValueSpec)(using DocConf): Doc = {
    val namesDoc = hsep(spec.names.map(n => text(n)), text(", "))
    val typeDoc = spec.valueType.map(t => text(" ") <> typeToDoc(t)).getOrElse(empty)
    val valuesDoc =
      if spec.values.isEmpty then empty else text(" = ") <> hsep(spec.values.map(toDoc), text(", "))
    namesDoc <> typeDoc <> valuesDoc
  }

  def typeParamListDoc(params: Vector[GoTypeParam])(using DocConf): Doc = {
    if params.isEmpty then empty
    else text("[") <> hsep(params.map(typeParamToDoc), text(", ")) <> text("]")
  }

  def typeParamToDoc(param: GoTypeParam)(using DocConf): Doc = {
    val constraintDoc = param.constraint.map(c => text(" ") <> typeToDoc(c)).getOrElse(empty)
    text(param.name) <> constraintDoc
  }

  def paramListDoc(params: Vector[GoField])(using DocConf): Doc =
    text("(") <> hsep(params.map(fieldToDoc), text(", ")) <> text(")")

  def resultListDoc(results: Vector[GoField])(using DocConf): Doc = {
    results.toList match
      case Nil => empty
      case single :: Nil if single.names.isEmpty && !single.isVariadic && single.tag.isEmpty && !single.isEmbedded =>
        text(" ") <> typeToDoc(single.fieldType)
      case _ =>
        text(" (") <> hsep(results.map(fieldToDoc), text(", ")) <> text(")")
  }

  def compositeElemToDoc(elem: GoCompositeElement)(using DocConf): Doc = {
    val keyDoc = elem.key.map(k => toDoc(k) <> text(": ")).getOrElse(empty)
    keyDoc <> toDoc(elem.value)
  }

  def importSpecToDoc(spec: GoImportSpec)(using DocConf): Doc = {
    val aliasDoc = spec.alias.map(a => text(a) <> text(" ")).getOrElse(empty)
    aliasDoc <> text("\"") <> text(escapeString(spec.path)) <> text("\"")
  }

  private def escapeString(value: String): String = {
    value
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\t", "\\t")
      .replace("\r", "\\r")
  }

  private def escapeRune(value: String): String = {
    value
      .replace("\\", "\\\\")
      .replace("'", "\\'")
      .replace("\n", "\\n")
      .replace("\t", "\\t")
      .replace("\r", "\\r")
  }

  private def escapeTag(value: String): String =
    value.replace("`", "\\`")
}

enum GoType derives ReadWriter {
  case Named(name: String, span: Option[Span])
  case Pointer(inner: GoType, span: Option[Span])
  case Slice(elementType: GoType, span: Option[Span])
  case Array(length: Option[GoAST], elementType: GoType, span: Option[Span])
  case Map(keyType: GoType, valueType: GoType, span: Option[Span])
  case Struct(fields: Vector[GoField], span: Option[Span])
  case Interface(methods: Vector[GoField], span: Option[Span])
  case Func(typeParams: Vector[GoTypeParam], params: Vector[GoField], results: Vector[GoField], span: Option[Span])
}

case class GoField(
    names: Vector[String],
    fieldType: GoType,
    tag: Option[String],
    isEmbedded: Boolean,
    isVariadic: Boolean,
    span: Option[Span]
) derives ReadWriter

case class GoTypeParam(
    name: String,
    constraint: Option[GoType],
    span: Option[Span]
) derives ReadWriter

case class GoCompositeElement(
    key: Option[GoAST],
    value: GoAST,
    span: Option[Span]
) derives ReadWriter

case class GoTypeSpec(
    name: String,
    typeParams: Vector[GoTypeParam],
    tpe: GoType,
    isAlias: Boolean,
    span: Option[Span]
) derives ReadWriter

case class GoValueSpec(
    names: Vector[String],
    valueType: Option[GoType],
    values: Vector[GoAST],
    span: Option[Span]
) derives ReadWriter

enum GoValueDeclKind derives ReadWriter {
  case Const
  case Var

  def keyword: String = this match
    case Const => "const"
    case Var   => "var"
}

case class GoImportSpec(
    alias: Option[String],
    path: String,
    span: Option[Span]
) derives ReadWriter
