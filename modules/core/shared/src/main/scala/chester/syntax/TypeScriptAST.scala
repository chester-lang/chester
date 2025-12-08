package chester.syntax

import chester.error.Span
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*

import scala.language.experimental.genericNumberLiterals

/** TypeScript AST representation for code generation */
enum TypeScriptAST derives ReadWriter {
  // Literals
  case NumberLiteral(value: String, span: Option[Span])
  case StringLiteral(value: String, span: Option[Span])
  case BooleanLiteral(value: Boolean, span: Option[Span])
  case NullLiteral(span: Option[Span])
  case UndefinedLiteral(span: Option[Span])

  // Identifiers and References
  case Identifier(name: String, span: Option[Span])
  case PropertyAccess(obj: TypeScriptAST, property: String, span: Option[Span])
  case ElementAccess(obj: TypeScriptAST, index: TypeScriptAST, span: Option[Span])

  // Expressions
  case BinaryOp(left: TypeScriptAST, op: String, right: TypeScriptAST, span: Option[Span])
  case UnaryOp(op: String, operand: TypeScriptAST, span: Option[Span])
  case Call(callee: TypeScriptAST, args: Vector[TypeScriptAST], span: Option[Span])
  case New(constructor: TypeScriptAST, args: Vector[TypeScriptAST], span: Option[Span])
  case Arrow(params: Vector[Parameter], body: TypeScriptAST, span: Option[Span])
  case Function(name: Option[String], params: Vector[Parameter], returnType: Option[TypeScriptType], body: TypeScriptAST, span: Option[Span])
  case Conditional(condition: TypeScriptAST, thenExpr: TypeScriptAST, elseExpr: TypeScriptAST, span: Option[Span])
  case Object(properties: Vector[ObjectProperty], span: Option[Span])
  case Array(elements: Vector[TypeScriptAST], span: Option[Span])
  case Template(parts: Vector[String], expressions: Vector[TypeScriptAST], span: Option[Span])
  case Await(expr: TypeScriptAST, span: Option[Span])
  case Yield(expr: Option[TypeScriptAST], span: Option[Span])
  case This(span: Option[Span])
  case Super(span: Option[Span])
  case Spread(expr: TypeScriptAST, span: Option[Span])
  case Cast(expr: TypeScriptAST, targetType: TypeScriptType, span: Option[Span])
  case NonNull(expr: TypeScriptAST, span: Option[Span])
  case Parenthesized(expr: TypeScriptAST, span: Option[Span])

  // Statements
  case Block(statements: Vector[TypeScriptAST], span: Option[Span])
  case VariableDeclaration(kind: VarKind, declarations: Vector[VariableDeclarator], span: Option[Span])
  case ExpressionStatement(expr: TypeScriptAST, span: Option[Span])
  case If(condition: TypeScriptAST, thenStmt: TypeScriptAST, elseStmt: Option[TypeScriptAST], span: Option[Span])
  case While(condition: TypeScriptAST, body: TypeScriptAST, span: Option[Span])
  case DoWhile(body: TypeScriptAST, condition: TypeScriptAST, span: Option[Span])
  case For(init: Option[TypeScriptAST], condition: Option[TypeScriptAST], update: Option[TypeScriptAST], body: TypeScriptAST, span: Option[Span])
  case ForOf(variable: TypeScriptAST, iterable: TypeScriptAST, body: TypeScriptAST, span: Option[Span])
  case ForIn(variable: TypeScriptAST, obj: TypeScriptAST, body: TypeScriptAST, span: Option[Span])
  case Switch(discriminant: TypeScriptAST, cases: Vector[SwitchCase], span: Option[Span])
  case Return(expr: Option[TypeScriptAST], span: Option[Span])
  case Throw(expr: TypeScriptAST, span: Option[Span])
  case Try(block: TypeScriptAST, handler: Option[CatchClause], finalizer: Option[TypeScriptAST], span: Option[Span])
  case Break(label: Option[String], span: Option[Span])
  case Continue(label: Option[String], span: Option[Span])
  case Labeled(label: String, statement: TypeScriptAST, span: Option[Span])
  case Empty(span: Option[Span])
  case Debugger(span: Option[Span])

  // Declarations
  case FunctionDeclaration(
      name: String,
      params: Vector[Parameter],
      returnType: Option[TypeScriptType],
      body: TypeScriptAST,
      modifiers: Vector[Modifier],
      span: Option[Span]
  )
  case ClassDeclaration(
      name: String,
      typeParams: Vector[TypeParameter],
      superClass: Option[TypeScriptAST],
      implements: Vector[TypeScriptType],
      members: Vector[ClassMember],
      modifiers: Vector[Modifier],
      span: Option[Span]
  )
  case InterfaceDeclaration(
      name: String,
      typeParams: Vector[TypeParameter],
      extendsTypes: Vector[TypeScriptType],
      members: Vector[InterfaceMember],
      span: Option[Span]
  )
  case TypeAliasDeclaration(name: String, typeParams: Vector[TypeParameter], aliasType: TypeScriptType, span: Option[Span])
  case EnumDeclaration(name: String, members: Vector[EnumMember], isConst: Boolean, span: Option[Span])
  case NamespaceDeclaration(name: String, body: Vector[TypeScriptAST], span: Option[Span])

  // Module system
  case ImportDeclaration(specifiers: Vector[ImportSpecifier], source: String, span: Option[Span])
  case ExportDeclaration(
      declaration: Option[TypeScriptAST],
      specifiers: Vector[ExportSpecifier],
      source: Option[String],
      isDefault: Boolean,
      span: Option[Span]
  )

  // Program root
  case Program(statements: Vector[TypeScriptAST], span: Option[Span])

  def toDoc(using DocConf): Doc = TypeScriptAST.toDoc(this)
}

object TypeScriptAST {
  def toDoc(ast: TypeScriptAST)(using DocConf): Doc = ast match {
    // Literals
    case NumberLiteral(value, _)  => text(value)
    case StringLiteral(value, _)  => text("\"") <> text(value.replace("\"", "\\\"")) <> text("\"")
    case BooleanLiteral(value, _) => text(value.toString)
    case NullLiteral(_)           => text("null")
    case UndefinedLiteral(_)      => text("undefined")

    // Identifiers
    case Identifier(name, _)              => text(name)
    case PropertyAccess(obj, property, _) => toDoc(obj) <> text(".") <> text(property)
    case ElementAccess(obj, index, _)     => toDoc(obj) <> text("[") <> toDoc(index) <> text("]")

    // Expressions
    case BinaryOp(left, op, right, _) => toDoc(left) <+> text(op) <+> toDoc(right)
    case UnaryOp(op, operand, _)      => text(op) <> toDoc(operand)
    case Call(callee, args, _)        => toDoc(callee) <> text("(") <> hsep(args.map(toDoc), text(", ")) <> text(")")
    case New(constructor, args, _)    => text("new ") <> toDoc(constructor) <> text("(") <> hsep(args.map(toDoc), text(", ")) <> text(")")
    case Arrow(params, body, _) =>
      val paramsDoc = if (params.length == 1 && params.head.paramType.isEmpty && !params.head.isRest) {
        text(params.head.name)
      } else {
        text("(") <> hsep(params.map(paramToDoc), text(", ")) <> text(")")
      }
      paramsDoc <+> text("=>") <+> (body match {
        case Block(_, _) => toDoc(body)
        case _           => toDoc(body)
      })
    case Function(name, params, returnType, body, _) =>
      val nameDoc = name.map(n => text(" ") <> text(n)).getOrElse(empty)
      text("function") <> nameDoc <> text("(") <> hsep(params.map(paramToDoc), text(", ")) <> text(")") <>
        returnType.map(t => text(": ") <> typeToDoc(t)).getOrElse(empty) <+> toDoc(body)
    case Conditional(condition, thenExpr, elseExpr, _) =>
      toDoc(condition) <+> text("?") <+> toDoc(thenExpr) <+> text(":") <+> toDoc(elseExpr)
    case Object(properties, _) =>
      if (properties.isEmpty) text("{}")
      else text("{") <+> hsep(properties.map(objPropToDoc), text(", ")) <+> text("}")
    case Array(elements, _) =>
      text("[") <> hsep(elements.map(toDoc), text(", ")) <> text("]")
    case Template(parts, expressions, _) =>
      val combined = parts.zip(expressions :+ null).flatMap { case (part, expr) =>
        if (expr == null) Vector(text(part))
        else Vector(text(part), text("${") <> toDoc(expr) <> text("}"))
      }
      text("`") <> concat(combined) <> text("`")
    case Await(expr, _)            => text("await ") <> toDoc(expr)
    case Yield(exprOpt, _)         => text("yield") <> exprOpt.map(e => text(" ") <> toDoc(e)).getOrElse(empty)
    case This(_)                   => text("this")
    case Super(_)                  => text("super")
    case Spread(expr, _)           => text("...") <> toDoc(expr)
    case Cast(expr, targetType, _) => toDoc(expr) <+> text("as") <+> typeToDoc(targetType)
    case NonNull(expr, _)          => toDoc(expr) <> text("!")
    case Parenthesized(expr, _)    => text("(") <> toDoc(expr) <> text(")")

    // Statements
    case Block(statements, _) =>
      if (statements.isEmpty) text("{}")
      else text("{") <@@> ssep(statements.map(s => toDoc(s) <> text(";")), hardline).indented() <@@> text("}")
    case VariableDeclaration(kind, declarations, _) =>
      val kindStr = kind match {
        case VarKind.Const => "const"
        case VarKind.Let   => "let"
        case VarKind.Var   => "var"
      }
      text(kindStr) <+> hsep(declarations.map(varDeclToDoc), text(", "))
    case ExpressionStatement(expr, _) => toDoc(expr)
    case If(condition, thenStmt, elseStmt, _) =>
      val base = text("if (") <> toDoc(condition) <> text(")") <+> toDoc(thenStmt)
      elseStmt.map(e => base <+> text("else") <+> toDoc(e)).getOrElse(base)
    case While(condition, body, _) =>
      text("while (") <> toDoc(condition) <> text(")") <+> toDoc(body)
    case DoWhile(body, condition, _) =>
      text("do") <+> toDoc(body) <+> text("while (") <> toDoc(condition) <> text(")")
    case For(init, condition, update, body, _) =>
      val initDoc = init.map(toDoc).getOrElse(empty)
      val condDoc = condition.map(toDoc).getOrElse(empty)
      val updateDoc = update.map(toDoc).getOrElse(empty)
      text("for (") <> initDoc <> text("; ") <> condDoc <> text("; ") <> updateDoc <> text(")") <+> toDoc(body)
    case ForOf(variable, iterable, body, _) =>
      text("for (") <> toDoc(variable) <+> text("of") <+> toDoc(iterable) <> text(")") <+> toDoc(body)
    case ForIn(variable, obj, body, _) =>
      text("for (") <> toDoc(variable) <+> text("in") <+> toDoc(obj) <> text(")") <+> toDoc(body)
    case Switch(discriminant, cases, _) =>
      text("switch (") <> toDoc(discriminant) <> text(") {") <@@>
        ssep(cases.map(switchCaseToDoc), hardline).indented() <@@> text("}")
    case Return(exprOpt, _) => text("return") <> exprOpt.map(e => text(" ") <> toDoc(e)).getOrElse(empty)
    case Throw(expr, _)     => text("throw ") <> toDoc(expr)
    case Try(block, handler, finalizer, _) =>
      val base = text("try") <+> toDoc(block)
      val withCatch = handler.map(h => base <+> catchClauseToDoc(h)).getOrElse(base)
      finalizer.map(f => withCatch <+> text("finally") <+> toDoc(f)).getOrElse(withCatch)
    case Break(labelOpt, _)           => text("break") <> labelOpt.map(l => text(" ") <> text(l)).getOrElse(empty)
    case Continue(labelOpt, _)        => text("continue") <> labelOpt.map(l => text(" ") <> text(l)).getOrElse(empty)
    case Labeled(label, statement, _) => text(label) <> text(": ") <> toDoc(statement)
    case Empty(_)                     => empty
    case Debugger(_)                  => text("debugger")

    // Declarations
    case FunctionDeclaration(name, params, returnType, body, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) hsep(modifiers.map(modifierToDoc), text(" ")) <> text(" ") else empty
      modsDoc <> text("function ") <> text(name) <> text("(") <>
        hsep(params.map(paramToDoc), text(", ")) <> text(")") <>
        returnType.map(t => text(": ") <> typeToDoc(t)).getOrElse(empty) <+> toDoc(body)
    case ClassDeclaration(name, typeParams, superClass, implements, members, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) hsep(modifiers.map(modifierToDoc), text(" ")) <> text(" ") else empty
      val typeParamsDoc = if (typeParams.nonEmpty) text("<") <> hsep(typeParams.map(typeParamToDoc), text(", ")) <> text(">") else empty
      val extendsDoc = superClass.map(s => text(" extends ") <> toDoc(s)).getOrElse(empty)
      val implementsDoc = if (implements.nonEmpty) text(" implements ") <> hsep(implements.map(typeToDoc), text(", ")) else empty
      modsDoc <> text("class ") <> text(name) <> typeParamsDoc <> extendsDoc <> implementsDoc <+> text("{") <@@>
        ssep(members.map(classMemberToDoc), hardline).indented() <@@> text("}")
    case InterfaceDeclaration(name, typeParams, extendsTypes, members, _) =>
      val typeParamsDoc = if (typeParams.nonEmpty) text("<") <> hsep(typeParams.map(typeParamToDoc), text(", ")) <> text(">") else empty
      val extendsDoc = if (extendsTypes.nonEmpty) text(" extends ") <> hsep(extendsTypes.map(typeToDoc), text(", ")) else empty
      text("interface ") <> text(name) <> typeParamsDoc <> extendsDoc <+> text("{") <@@>
        ssep(members.map(interfaceMemberToDoc), hardline).indented() <@@> text("}")
    case TypeAliasDeclaration(name, typeParams, aliasType, _) =>
      val typeParamsDoc = if (typeParams.nonEmpty) text("<") <> hsep(typeParams.map(typeParamToDoc), text(", ")) <> text(">") else empty
      text("type ") <> text(name) <> typeParamsDoc <+> text("=") <+> typeToDoc(aliasType)
    case EnumDeclaration(name, members, isConst, _) =>
      val constDoc = if (isConst) text("const ") else empty
      constDoc <> text("enum ") <> text(name) <+> text("{") <@@>
        hsep(members.map(enumMemberToDoc), text(",") <@@> empty).indented() <@@> text("}")
    case NamespaceDeclaration(name, body, _) =>
      text("namespace ") <> text(name) <+> text("{") <@@>
        ssep(body.map(s => toDoc(s) <> text(";")), hardline).indented() <@@> text("}")

    // Module system
    case ImportDeclaration(specifiers, source, _) =>
      text("import ") <> hsep(specifiers.map(importSpecToDoc), text(", ")) <+>
        text("from \"") <> text(source) <> text("\"")
    case ExportDeclaration(declaration, specifiers, source, isDefault, _) =>
      if (isDefault && declaration.isDefined) {
        text("export default ") <> toDoc(declaration.get)
      } else if (declaration.isDefined) {
        text("export ") <> toDoc(declaration.get)
      } else {
        val exportDoc = text("export ") <> text("{") <> hsep(specifiers.map(exportSpecToDoc), text(", ")) <> text("}")
        source.map(s => exportDoc <+> text("from \"") <> text(s) <> text("\"")).getOrElse(exportDoc)
      }

    case Program(statements, _) =>
      ssep(statements.map(s => toDoc(s) <> text(";")), hardline)
  }

  def paramToDoc(param: Parameter)(using DocConf): Doc = {
    val restDoc = if (param.isRest) text("...") else empty
    val typeDoc = param.paramType.map(t => text(": ") <> typeToDoc(t)).getOrElse(empty)
    val defaultDoc = param.defaultValue.map(v => text(" = ") <> toDoc(v)).getOrElse(empty)
    restDoc <> text(param.name) <> typeDoc <> defaultDoc
  }

  def varDeclToDoc(decl: VariableDeclarator)(using DocConf): Doc = {
    val typeDoc = decl.varType.map(t => text(": ") <> typeToDoc(t)).getOrElse(empty)
    val initDoc = decl.init.map(i => text(" = ") <> toDoc(i)).getOrElse(empty)
    text(decl.name) <> typeDoc <> initDoc
  }

  def objPropToDoc(prop: ObjectProperty)(using DocConf): Doc = {
    val keyDoc = prop.key match {
      case Left(str)  => text(str)
      case Right(ast) => text("[") <> toDoc(ast) <> text("]")
    }
    if (prop.isShorthand) keyDoc
    else keyDoc <> text(": ") <> toDoc(prop.value)
  }

  def switchCaseToDoc(sc: SwitchCase)(using DocConf): Doc = {
    val testDoc = sc.test.map(t => text("case ") <> toDoc(t) <> text(":")).getOrElse(text("default:"))
    testDoc <@@> ssep(sc.consequent.map(s => toDoc(s) <> text(";")), hardline).indented()
  }

  def catchClauseToDoc(cc: CatchClause)(using DocConf): Doc = {
    val paramDoc = cc.param.map(p => text("(") <> text(p) <> text(")")).getOrElse(empty)
    text("catch") <> paramDoc <+> toDoc(cc.body)
  }

  def classMemberToDoc(member: ClassMember)(using DocConf): Doc = member match {
    case ClassMember.Constructor(params, body, _) =>
      text("constructor(") <> hsep(params.map(paramToDoc), text(", ")) <> text(")") <+> toDoc(body)
    case ClassMember.Method(name, params, returnType, body, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) hsep(modifiers.map(modifierToDoc), text(" ")) <> text(" ") else empty
      modsDoc <> text(name) <> text("(") <> hsep(params.map(paramToDoc), text(", ")) <> text(")") <>
        returnType.map(t => text(": ") <> typeToDoc(t)).getOrElse(empty) <+> toDoc(body)
    case ClassMember.Property(name, propertyType, init, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) hsep(modifiers.map(modifierToDoc), text(" ")) <> text(" ") else empty
      val typeDoc = propertyType.map(t => text(": ") <> typeToDoc(t)).getOrElse(empty)
      val initDoc = init.map(i => text(" = ") <> toDoc(i)).getOrElse(empty)
      modsDoc <> text(name) <> typeDoc <> initDoc <> text(";")
    case ClassMember.Getter(name, returnType, body, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) hsep(modifiers.map(modifierToDoc), text(" ")) <> text(" ") else empty
      modsDoc <> text("get ") <> text(name) <> text("()") <>
        returnType.map(t => text(": ") <> typeToDoc(t)).getOrElse(empty) <+> toDoc(body)
    case ClassMember.Setter(name, param, body, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) hsep(modifiers.map(modifierToDoc), text(" ")) <> text(" ") else empty
      modsDoc <> text("set ") <> text(name) <> text("(") <> paramToDoc(param) <> text(")") <+> toDoc(body)
    case ClassMember.Index(params, returnType, _) =>
      text("[") <> hsep(params.map(paramToDoc), text(", ")) <> text("]: ") <> typeToDoc(returnType) <> text(";")
  }

  def interfaceMemberToDoc(member: InterfaceMember)(using DocConf): Doc = {
    val typeDoc = member.memberType match {
      case InterfaceMemberType.PropertySignature(propertyType, isOptional, isReadonly) =>
        val readonlyDoc = if (isReadonly) text("readonly ") else empty
        val optionalDoc = if (isOptional) text("?") else empty
        readonlyDoc <> text(member.name) <> optionalDoc <> text(": ") <> typeToDoc(propertyType)
      case InterfaceMemberType.MethodSignature(params, returnType) =>
        text(member.name) <> text("(") <> hsep(params.map(paramToDoc), text(", ")) <>
          text("): ") <> typeToDoc(returnType)
      case InterfaceMemberType.CallSignature(params, returnType) =>
        text("(") <> hsep(params.map(paramToDoc), text(", ")) <> text("): ") <> typeToDoc(returnType)
      case InterfaceMemberType.ConstructSignature(params, returnType) =>
        text("new (") <> hsep(params.map(paramToDoc), text(", ")) <> text("): ") <> typeToDoc(returnType)
      case InterfaceMemberType.IndexSignature(params, returnType) =>
        text("[") <> hsep(params.map(paramToDoc), text(", ")) <> text("]: ") <> typeToDoc(returnType)
    }
    typeDoc <> text(";")
  }

  def enumMemberToDoc(member: EnumMember)(using DocConf): Doc = {
    val valueDoc = member.value.map(v => text(" = ") <> toDoc(v)).getOrElse(empty)
    text(member.name) <> valueDoc
  }

  def importSpecToDoc(spec: ImportSpecifier)(using DocConf): Doc = spec match {
    case ImportSpecifier.Default(local, _)   => text(local)
    case ImportSpecifier.Namespace(local, _) => text("* as ") <> text(local)
    case ImportSpecifier.Named(imported, localOpt, _) =>
      localOpt.map(local => text(imported) <+> text("as") <+> text(local)).getOrElse(text(imported))
  }

  def exportSpecToDoc(spec: ExportSpecifier)(using DocConf): Doc = spec match {
    case ExportSpecifier.Named(local, exportedOpt, _) =>
      exportedOpt.map(exported => text(local) <+> text("as") <+> text(exported)).getOrElse(text(local))
    case ExportSpecifier.Default(_) => text("default")
    case ExportSpecifier.All(exportedOpt, _) =>
      text("*") <> exportedOpt.map(e => text(" as ") <> text(e)).getOrElse(empty)
  }

  def modifierToDoc(modifier: Modifier)(using DocConf): Doc = modifier match {
    case Modifier.Public    => text("public")
    case Modifier.Private   => text("private")
    case Modifier.Protected => text("protected")
    case Modifier.Static    => text("static")
    case Modifier.Readonly  => text("readonly")
    case Modifier.Abstract  => text("abstract")
    case Modifier.Async     => text("async")
    case Modifier.Const     => text("const")
    case Modifier.Declare   => text("declare")
    case Modifier.Export    => text("export")
  }

  def typeParamToDoc(tp: TypeParameter)(using DocConf): Doc = {
    val constraintDoc = tp.constraint.map(c => text(" extends ") <> typeToDoc(c)).getOrElse(empty)
    val defaultDoc = tp.default.map(d => text(" = ") <> typeToDoc(d)).getOrElse(empty)
    text(tp.name) <> constraintDoc <> defaultDoc
  }

  def typeToDoc(tsType: TypeScriptType)(using DocConf): Doc = tsType match {
    case TypeScriptType.PrimitiveType(name, _) => text(name)
    case TypeScriptType.TypeReference(name, typeArgs, _) =>
      if (typeArgs.isEmpty) text(name)
      else text(name) <> text("<") <> hsep(typeArgs.map(typeToDoc), text(", ")) <> text(">")
    case TypeScriptType.ArrayType(elementType, _) => typeToDoc(elementType) <> text("[]")
    case TypeScriptType.TupleType(elements, _) =>
      text("[") <> hsep(elements.map(typeToDoc), text(", ")) <> text("]")
    case TypeScriptType.UnionType(types, _) =>
      hsep(types.map(typeToDoc), text(" | "))
    case TypeScriptType.IntersectionType(types, _) =>
      hsep(types.map(typeToDoc), text(" & "))
    case TypeScriptType.FunctionType(params, returnType, _) =>
      text("(") <> hsep(params.map(paramToDoc), text(", ")) <> text(") => ") <> typeToDoc(returnType)
    case TypeScriptType.ObjectType(members, _) =>
      text("{") <+> hsep(members.map(interfaceMemberToDoc), text(" ")) <+> text("}")
    case TypeScriptType.LiteralType(value, _)    => toDoc(value)
    case TypeScriptType.KeyofType(objectType, _) => text("keyof ") <> typeToDoc(objectType)
    case TypeScriptType.TypeofType(expr, _)      => text("typeof ") <> toDoc(expr)
    case TypeScriptType.IndexedAccessType(objectType, indexType, _) =>
      typeToDoc(objectType) <> text("[") <> typeToDoc(indexType) <> text("]")
    case TypeScriptType.MappedType(typeParam, nameType, objectType, _) =>
      val nameDoc = nameType.map(n => text(" as ") <> typeToDoc(n)).getOrElse(empty)
      text("{[") <> text(typeParam.name) <+> text("in") <+> typeToDoc(objectType) <> nameDoc <> text("]: ") <> text("...}")
    case TypeScriptType.ConditionalType(checkType, extendsType, trueType, falseType, _) =>
      typeToDoc(checkType) <+> text("extends") <+> typeToDoc(extendsType) <+> text("?") <+>
        typeToDoc(trueType) <+> text(":") <+> typeToDoc(falseType)
    case TypeScriptType.InferType(typeParam, _) => text("infer ") <> text(typeParam.name)
    case TypeScriptType.TemplateLiteralType(parts, types, _) =>
      val combined = parts.zip(types :+ null).flatMap { case (part, tpe) =>
        if (tpe == null) Vector(text(part))
        else Vector(text(part), text("${") <> typeToDoc(tpe) <> text("}"))
      }
      text("`") <> concat(combined) <> text("`")
    case TypeScriptType.ParenthesizedType(innerType, _) => text("(") <> typeToDoc(innerType) <> text(")")
  }
}

enum VarKind derives ReadWriter {
  case Const
  case Let
  case Var
}

case class Parameter(
    name: String,
    paramType: Option[TypeScriptType],
    defaultValue: Option[TypeScriptAST],
    isRest: Boolean,
    span: Option[Span]
) derives ReadWriter

case class VariableDeclarator(
    name: String,
    varType: Option[TypeScriptType],
    init: Option[TypeScriptAST],
    span: Option[Span]
) derives ReadWriter

case class ObjectProperty(
    key: Either[String, TypeScriptAST],
    value: TypeScriptAST,
    isShorthand: Boolean,
    isMethod: Boolean,
    span: Option[Span]
) derives ReadWriter

case class SwitchCase(
    test: Option[TypeScriptAST], // None for default case
    consequent: Vector[TypeScriptAST],
    span: Option[Span]
) derives ReadWriter

case class CatchClause(
    param: Option[String],
    body: TypeScriptAST,
    span: Option[Span]
) derives ReadWriter

enum ClassMember derives ReadWriter {
  case Constructor(params: Vector[Parameter], body: TypeScriptAST, span: Option[Span])
  case Method(
      name: String,
      params: Vector[Parameter],
      returnType: Option[TypeScriptType],
      body: TypeScriptAST,
      modifiers: Vector[Modifier],
      span: Option[Span]
  )
  case Property(name: String, propertyType: Option[TypeScriptType], init: Option[TypeScriptAST], modifiers: Vector[Modifier], span: Option[Span])
  case Getter(name: String, returnType: Option[TypeScriptType], body: TypeScriptAST, modifiers: Vector[Modifier], span: Option[Span])
  case Setter(name: String, param: Parameter, body: TypeScriptAST, modifiers: Vector[Modifier], span: Option[Span])
  case Index(params: Vector[Parameter], returnType: TypeScriptType, span: Option[Span])
}

case class InterfaceMember(
    name: String,
    memberType: InterfaceMemberType,
    span: Option[Span]
) derives ReadWriter

enum InterfaceMemberType derives ReadWriter {
  case PropertySignature(propertyType: TypeScriptType, isOptional: Boolean, isReadonly: Boolean)
  case MethodSignature(params: Vector[Parameter], returnType: TypeScriptType)
  case CallSignature(params: Vector[Parameter], returnType: TypeScriptType)
  case ConstructSignature(params: Vector[Parameter], returnType: TypeScriptType)
  case IndexSignature(params: Vector[Parameter], returnType: TypeScriptType)
}

case class EnumMember(
    name: String,
    value: Option[TypeScriptAST],
    span: Option[Span]
) derives ReadWriter

enum ImportSpecifier derives ReadWriter {
  case Default(local: String, span: Option[Span])
  case Namespace(local: String, span: Option[Span])
  case Named(imported: String, local: Option[String], span: Option[Span])
}

enum ExportSpecifier derives ReadWriter {
  case Named(local: String, exported: Option[String], span: Option[Span])
  case Default(span: Option[Span])
  case All(exported: Option[String], span: Option[Span])
}

enum Modifier derives ReadWriter {
  case Public
  case Private
  case Protected
  case Static
  case Readonly
  case Abstract
  case Async
  case Const
  case Declare
  case Export
}

case class TypeParameter(
    name: String,
    constraint: Option[TypeScriptType],
    default: Option[TypeScriptType],
    span: Option[Span]
) derives ReadWriter

/** TypeScript type system */
enum TypeScriptType derives ReadWriter {
  case PrimitiveType(name: String, span: Option[Span]) // string, number, boolean, void, any, unknown, never, etc.
  case TypeReference(name: String, typeArgs: Vector[TypeScriptType], span: Option[Span])
  case ArrayType(elementType: TypeScriptType, span: Option[Span])
  case TupleType(elements: Vector[TypeScriptType], span: Option[Span])
  case UnionType(types: Vector[TypeScriptType], span: Option[Span])
  case IntersectionType(types: Vector[TypeScriptType], span: Option[Span])
  case FunctionType(params: Vector[Parameter], returnType: TypeScriptType, span: Option[Span])
  case ObjectType(members: Vector[InterfaceMember], span: Option[Span])
  case LiteralType(value: TypeScriptAST, span: Option[Span])
  case KeyofType(objectType: TypeScriptType, span: Option[Span])
  case TypeofType(expr: TypeScriptAST, span: Option[Span])
  case IndexedAccessType(objectType: TypeScriptType, indexType: TypeScriptType, span: Option[Span])
  case MappedType(typeParam: TypeParameter, nameType: Option[TypeScriptType], objectType: TypeScriptType, span: Option[Span])
  case ConditionalType(
      checkType: TypeScriptType,
      extendsType: TypeScriptType,
      trueType: TypeScriptType,
      falseType: TypeScriptType,
      span: Option[Span]
  )
  case InferType(typeParam: TypeParameter, span: Option[Span])
  case TemplateLiteralType(parts: Vector[String], types: Vector[TypeScriptType], span: Option[Span])
  case ParenthesizedType(innerType: TypeScriptType, span: Option[Span])
}
