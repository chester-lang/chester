package chester.syntax

import chester.error.Span
import chester.doc.*
import upickle.default.*

/** TypeScript AST representation for code generation */
enum TypeScriptAST derives ReadWriter {
  // Literals
  case NumberLiteral(value: String, span: Span)
  case StringLiteral(value: String, span: Span)
  case BooleanLiteral(value: Boolean, span: Span)
  case NullLiteral(span: Span)
  case UndefinedLiteral(span: Span)
  
  // Identifiers and References
  case Identifier(name: String, span: Span)
  case PropertyAccess(obj: TypeScriptAST, property: String, span: Span)
  case ElementAccess(obj: TypeScriptAST, index: TypeScriptAST, span: Span)
  
  // Expressions
  case BinaryOp(left: TypeScriptAST, op: String, right: TypeScriptAST, span: Span)
  case UnaryOp(op: String, operand: TypeScriptAST, span: Span)
  case Call(callee: TypeScriptAST, args: Vector[TypeScriptAST], span: Span)
  case New(constructor: TypeScriptAST, args: Vector[TypeScriptAST], span: Span)
  case Arrow(params: Vector[Parameter], body: TypeScriptAST, span: Span)
  case Function(name: Option[String], params: Vector[Parameter], returnType: Option[TypeScriptType], body: TypeScriptAST, span: Span)
  case Conditional(condition: TypeScriptAST, thenExpr: TypeScriptAST, elseExpr: TypeScriptAST, span: Span)
  case Object(properties: Vector[ObjectProperty], span: Span)
  case Array(elements: Vector[TypeScriptAST], span: Span)
  case Template(parts: Vector[String], expressions: Vector[TypeScriptAST], span: Span)
  case Await(expr: TypeScriptAST, span: Span)
  case Yield(expr: Option[TypeScriptAST], span: Span)
  case This(span: Span)
  case Super(span: Span)
  case Spread(expr: TypeScriptAST, span: Span)
  case Cast(expr: TypeScriptAST, targetType: TypeScriptType, span: Span)
  case NonNull(expr: TypeScriptAST, span: Span)
  case Parenthesized(expr: TypeScriptAST, span: Span)
  
  // Statements
  case Block(statements: Vector[TypeScriptAST], span: Span)
  case VariableDeclaration(kind: VarKind, declarations: Vector[VariableDeclarator], span: Span)
  case ExpressionStatement(expr: TypeScriptAST, span: Span)
  case If(condition: TypeScriptAST, thenStmt: TypeScriptAST, elseStmt: Option[TypeScriptAST], span: Span)
  case While(condition: TypeScriptAST, body: TypeScriptAST, span: Span)
  case DoWhile(body: TypeScriptAST, condition: TypeScriptAST, span: Span)
  case For(init: Option[TypeScriptAST], condition: Option[TypeScriptAST], update: Option[TypeScriptAST], body: TypeScriptAST, span: Span)
  case ForOf(variable: TypeScriptAST, iterable: TypeScriptAST, body: TypeScriptAST, span: Span)
  case ForIn(variable: TypeScriptAST, obj: TypeScriptAST, body: TypeScriptAST, span: Span)
  case Switch(discriminant: TypeScriptAST, cases: Vector[SwitchCase], span: Span)
  case Return(expr: Option[TypeScriptAST], span: Span)
  case Throw(expr: TypeScriptAST, span: Span)
  case Try(block: TypeScriptAST, handler: Option[CatchClause], finalizer: Option[TypeScriptAST], span: Span)
  case Break(label: Option[String], span: Span)
  case Continue(label: Option[String], span: Span)
  case Labeled(label: String, statement: TypeScriptAST, span: Span)
  case Empty(span: Span)
  case Debugger(span: Span)
  
  // Declarations
  case FunctionDeclaration(name: String, params: Vector[Parameter], returnType: Option[TypeScriptType], body: TypeScriptAST, modifiers: Vector[Modifier], span: Span)
  case ClassDeclaration(name: String, typeParams: Vector[TypeParameter], superClass: Option[TypeScriptAST], implements: Vector[TypeScriptType], members: Vector[ClassMember], modifiers: Vector[Modifier], span: Span)
  case InterfaceDeclaration(name: String, typeParams: Vector[TypeParameter], extends: Vector[TypeScriptType], members: Vector[InterfaceMember], span: Span)
  case TypeAliasDeclaration(name: String, typeParams: Vector[TypeParameter], aliasType: TypeScriptType, span: Span)
  case EnumDeclaration(name: String, members: Vector[EnumMember], isConst: Boolean, span: Span)
  case NamespaceDeclaration(name: String, body: Vector[TypeScriptAST], span: Span)
  
  // Module system
  case ImportDeclaration(specifiers: Vector[ImportSpecifier], source: String, span: Span)
  case ExportDeclaration(declaration: Option[TypeScriptAST], specifiers: Vector[ExportSpecifier], source: Option[String], isDefault: Boolean, span: Span)
  
  // Program root
  case Program(statements: Vector[TypeScriptAST], span: Span)

  def toDoc: Doc = TypeScriptAST.toDoc(this)
}

object TypeScriptAST {
  def toDoc(ast: TypeScriptAST): Doc = ast match {
    // Literals
    case NumberLiteral(value, _) => Doc.text(value)
    case StringLiteral(value, _) => Doc.text("\"") <> Doc.text(value.replace("\"", "\\\"")) <> Doc.text("\"")
    case BooleanLiteral(value, _) => Doc.text(value.toString)
    case NullLiteral(_) => Doc.text("null")
    case UndefinedLiteral(_) => Doc.text("undefined")
    
    // Identifiers
    case Identifier(name, _) => Doc.text(name)
    case PropertyAccess(obj, property, _) => toDoc(obj) <> Doc.text(".") <> Doc.text(property)
    case ElementAccess(obj, index, _) => toDoc(obj) <> Doc.text("[") <> toDoc(index) <> Doc.text("]")
    
    // Expressions
    case BinaryOp(left, op, right, _) => toDoc(left) <+> Doc.text(op) <+> toDoc(right)
    case UnaryOp(op, operand, _) => Doc.text(op) <> toDoc(operand)
    case Call(callee, args, _) => toDoc(callee) <> Doc.text("(") <> Doc.join(args.map(toDoc), Doc.text(", ")) <> Doc.text(")")
    case New(constructor, args, _) => Doc.text("new ") <> toDoc(constructor) <> Doc.text("(") <> Doc.join(args.map(toDoc), Doc.text(", ")) <> Doc.text(")")
    case Arrow(params, body, _) =>
      val paramsDoc = if (params.length == 1 && params.head.paramType.isEmpty && !params.head.isRest) {
        Doc.text(params.head.name)
      } else {
        Doc.text("(") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text(")")
      }
      paramsDoc <+> Doc.text("=>") <+> (body match {
        case Block(_, _) => toDoc(body)
        case _ => toDoc(body)
      })
    case Function(name, params, returnType, body, _) =>
      val nameDoc = name.map(n => Doc.text(" ") <> Doc.text(n)).getOrElse(Doc.empty)
      Doc.text("function") <> nameDoc <> Doc.text("(") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text(")") <>
        returnType.map(t => Doc.text(": ") <> typeToDoc(t)).getOrElse(Doc.empty) <+> toDoc(body)
    case Conditional(condition, thenExpr, elseExpr, _) =>
      toDoc(condition) <+> Doc.text("?") <+> toDoc(thenExpr) <+> Doc.text(":") <+> toDoc(elseExpr)
    case Object(properties, _) =>
      if (properties.isEmpty) Doc.text("{}")
      else Doc.text("{") <+> Doc.join(properties.map(objPropToDoc), Doc.text(", ")) <+> Doc.text("}")
    case Array(elements, _) =>
      Doc.text("[") <> Doc.join(elements.map(toDoc), Doc.text(", ")) <> Doc.text("]")
    case Template(parts, expressions, _) =>
      val combined = parts.zip(expressions :+ null).flatMap { case (part, expr) =>
        if (expr == null) Vector(Doc.text(part))
        else Vector(Doc.text(part), Doc.text("${") <> toDoc(expr) <> Doc.text("}"))
      }
      Doc.text("`") <> Doc.concat(combined) <> Doc.text("`")
    case Await(expr, _) => Doc.text("await ") <> toDoc(expr)
    case Yield(exprOpt, _) => Doc.text("yield") <> exprOpt.map(e => Doc.text(" ") <> toDoc(e)).getOrElse(Doc.empty)
    case This(_) => Doc.text("this")
    case Super(_) => Doc.text("super")
    case Spread(expr, _) => Doc.text("...") <> toDoc(expr)
    case Cast(expr, targetType, _) => toDoc(expr) <+> Doc.text("as") <+> typeToDoc(targetType)
    case NonNull(expr, _) => toDoc(expr) <> Doc.text("!")
    case Parenthesized(expr, _) => Doc.text("(") <> toDoc(expr) <> Doc.text(")")
    
    // Statements
    case Block(statements, _) =>
      if (statements.isEmpty) Doc.text("{}")
      else Doc.text("{") <@@> Doc.indent(Doc.vcat(statements.map(s => toDoc(s) <> Doc.text(";")))) <@@> Doc.text("}")
    case VariableDeclaration(kind, declarations, _) =>
      val kindStr = kind match {
        case VarKind.Const => "const"
        case VarKind.Let => "let"
        case VarKind.Var => "var"
      }
      Doc.text(kindStr) <+> Doc.join(declarations.map(varDeclToDoc), Doc.text(", "))
    case ExpressionStatement(expr, _) => toDoc(expr)
    case If(condition, thenStmt, elseStmt, _) =>
      val base = Doc.text("if (") <> toDoc(condition) <> Doc.text(")") <+> toDoc(thenStmt)
      elseStmt.map(e => base <+> Doc.text("else") <+> toDoc(e)).getOrElse(base)
    case While(condition, body, _) =>
      Doc.text("while (") <> toDoc(condition) <> Doc.text(")") <+> toDoc(body)
    case DoWhile(body, condition, _) =>
      Doc.text("do") <+> toDoc(body) <+> Doc.text("while (") <> toDoc(condition) <> Doc.text(")")
    case For(init, condition, update, body, _) =>
      val initDoc = init.map(toDoc).getOrElse(Doc.empty)
      val condDoc = condition.map(toDoc).getOrElse(Doc.empty)
      val updateDoc = update.map(toDoc).getOrElse(Doc.empty)
      Doc.text("for (") <> initDoc <> Doc.text("; ") <> condDoc <> Doc.text("; ") <> updateDoc <> Doc.text(")") <+> toDoc(body)
    case ForOf(variable, iterable, body, _) =>
      Doc.text("for (") <> toDoc(variable) <+> Doc.text("of") <+> toDoc(iterable) <> Doc.text(")") <+> toDoc(body)
    case ForIn(variable, obj, body, _) =>
      Doc.text("for (") <> toDoc(variable) <+> Doc.text("in") <+> toDoc(obj) <> Doc.text(")") <+> toDoc(body)
    case Switch(discriminant, cases, _) =>
      Doc.text("switch (") <> toDoc(discriminant) <> Doc.text(") {") <@@>
        Doc.indent(Doc.vcat(cases.map(switchCaseToDoc))) <@@> Doc.text("}")
    case Return(exprOpt, _) => Doc.text("return") <> exprOpt.map(e => Doc.text(" ") <> toDoc(e)).getOrElse(Doc.empty)
    case Throw(expr, _) => Doc.text("throw ") <> toDoc(expr)
    case Try(block, handler, finalizer, _) =>
      val base = Doc.text("try") <+> toDoc(block)
      val withCatch = handler.map(h => base <+> catchClauseToDoc(h)).getOrElse(base)
      finalizer.map(f => withCatch <+> Doc.text("finally") <+> toDoc(f)).getOrElse(withCatch)
    case Break(labelOpt, _) => Doc.text("break") <> labelOpt.map(l => Doc.text(" ") <> Doc.text(l)).getOrElse(Doc.empty)
    case Continue(labelOpt, _) => Doc.text("continue") <> labelOpt.map(l => Doc.text(" ") <> Doc.text(l)).getOrElse(Doc.empty)
    case Labeled(label, statement, _) => Doc.text(label) <> Doc.text(": ") <> toDoc(statement)
    case Empty(_) => Doc.empty
    case Debugger(_) => Doc.text("debugger")
    
    // Declarations
    case FunctionDeclaration(name, params, returnType, body, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) Doc.join(modifiers.map(modifierToDoc), Doc.space) <> Doc.space else Doc.empty
      modsDoc <> Doc.text("function ") <> Doc.text(name) <> Doc.text("(") <> 
        Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text(")") <>
        returnType.map(t => Doc.text(": ") <> typeToDoc(t)).getOrElse(Doc.empty) <+> toDoc(body)
    case ClassDeclaration(name, typeParams, superClass, implements, members, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) Doc.join(modifiers.map(modifierToDoc), Doc.space) <> Doc.space else Doc.empty
      val typeParamsDoc = if (typeParams.nonEmpty) Doc.text("<") <> Doc.join(typeParams.map(typeParamToDoc), Doc.text(", ")) <> Doc.text(">") else Doc.empty
      val extendsDoc = superClass.map(s => Doc.text(" extends ") <> toDoc(s)).getOrElse(Doc.empty)
      val implementsDoc = if (implements.nonEmpty) Doc.text(" implements ") <> Doc.join(implements.map(typeToDoc), Doc.text(", ")) else Doc.empty
      modsDoc <> Doc.text("class ") <> Doc.text(name) <> typeParamsDoc <> extendsDoc <> implementsDoc <+> Doc.text("{") <@@>
        Doc.indent(Doc.vcat(members.map(classMemberToDoc))) <@@> Doc.text("}")
    case InterfaceDeclaration(name, typeParams, extends, members, _) =>
      val typeParamsDoc = if (typeParams.nonEmpty) Doc.text("<") <> Doc.join(typeParams.map(typeParamToDoc), Doc.text(", ")) <> Doc.text(">") else Doc.empty
      val extendsDoc = if (extends.nonEmpty) Doc.text(" extends ") <> Doc.join(extends.map(typeToDoc), Doc.text(", ")) else Doc.empty
      Doc.text("interface ") <> Doc.text(name) <> typeParamsDoc <> extendsDoc <+> Doc.text("{") <@@>
        Doc.indent(Doc.vcat(members.map(interfaceMemberToDoc))) <@@> Doc.text("}")
    case TypeAliasDeclaration(name, typeParams, aliasType, _) =>
      val typeParamsDoc = if (typeParams.nonEmpty) Doc.text("<") <> Doc.join(typeParams.map(typeParamToDoc), Doc.text(", ")) <> Doc.text(">") else Doc.empty
      Doc.text("type ") <> Doc.text(name) <> typeParamsDoc <+> Doc.text("=") <+> typeToDoc(aliasType)
    case EnumDeclaration(name, members, isConst, _) =>
      val constDoc = if (isConst) Doc.text("const ") else Doc.empty
      constDoc <> Doc.text("enum ") <> Doc.text(name) <+> Doc.text("{") <@@>
        Doc.indent(Doc.join(members.map(enumMemberToDoc), Doc.text(",") <@@> Doc.empty)) <@@> Doc.text("}")
    case NamespaceDeclaration(name, body, _) =>
      Doc.text("namespace ") <> Doc.text(name) <+> Doc.text("{") <@@>
        Doc.indent(Doc.vcat(body.map(s => toDoc(s) <> Doc.text(";")))) <@@> Doc.text("}")
    
    // Module system
    case ImportDeclaration(specifiers, source, _) =>
      Doc.text("import ") <> Doc.join(specifiers.map(importSpecToDoc), Doc.text(", ")) <+>
        Doc.text("from \"") <> Doc.text(source) <> Doc.text("\"")
    case ExportDeclaration(declaration, specifiers, source, isDefault, _) =>
      if (isDefault && declaration.isDefined) {
        Doc.text("export default ") <> toDoc(declaration.get)
      } else if (declaration.isDefined) {
        Doc.text("export ") <> toDoc(declaration.get)
      } else {
        val exportDoc = Doc.text("export ") <> Doc.text("{") <> Doc.join(specifiers.map(exportSpecToDoc), Doc.text(", ")) <> Doc.text("}")
        source.map(s => exportDoc <+> Doc.text("from \"") <> Doc.text(s) <> Doc.text("\"")).getOrElse(exportDoc)
      }
    
    case Program(statements, _) =>
      Doc.vcat(statements.map(s => toDoc(s) <> Doc.text(";")))
  }

  def paramToDoc(param: Parameter): Doc = {
    val restDoc = if (param.isRest) Doc.text("...") else Doc.empty
    val typeDoc = param.paramType.map(t => Doc.text(": ") <> typeToDoc(t)).getOrElse(Doc.empty)
    val defaultDoc = param.defaultValue.map(v => Doc.text(" = ") <> toDoc(v)).getOrElse(Doc.empty)
    restDoc <> Doc.text(param.name) <> typeDoc <> defaultDoc
  }

  def varDeclToDoc(decl: VariableDeclarator): Doc = {
    val typeDoc = decl.varType.map(t => Doc.text(": ") <> typeToDoc(t)).getOrElse(Doc.empty)
    val initDoc = decl.init.map(i => Doc.text(" = ") <> toDoc(i)).getOrElse(Doc.empty)
    Doc.text(decl.name) <> typeDoc <> initDoc
  }

  def objPropToDoc(prop: ObjectProperty): Doc = {
    val keyDoc = prop.key match {
      case Left(str) => Doc.text(str)
      case Right(ast) => Doc.text("[") <> toDoc(ast) <> Doc.text("]")
    }
    if (prop.isShorthand) keyDoc
    else keyDoc <> Doc.text(": ") <> toDoc(prop.value)
  }

  def switchCaseToDoc(sc: SwitchCase): Doc = {
    val testDoc = sc.test.map(t => Doc.text("case ") <> toDoc(t) <> Doc.text(":")).getOrElse(Doc.text("default:"))
    testDoc <@@> Doc.indent(Doc.vcat(sc.consequent.map(s => toDoc(s) <> Doc.text(";"))))
  }

  def catchClauseToDoc(cc: CatchClause): Doc = {
    val paramDoc = cc.param.map(p => Doc.text("(") <> Doc.text(p) <> Doc.text(")")).getOrElse(Doc.empty)
    Doc.text("catch") <> paramDoc <+> toDoc(cc.body)
  }

  def classMemberToDoc(member: ClassMember): Doc = member match {
    case ClassMember.Constructor(params, body, _) =>
      Doc.text("constructor(") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text(")") <+> toDoc(body)
    case ClassMember.Method(name, params, returnType, body, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) Doc.join(modifiers.map(modifierToDoc), Doc.space) <> Doc.space else Doc.empty
      modsDoc <> Doc.text(name) <> Doc.text("(") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text(")") <>
        returnType.map(t => Doc.text(": ") <> typeToDoc(t)).getOrElse(Doc.empty) <+> toDoc(body)
    case ClassMember.Property(name, propertyType, init, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) Doc.join(modifiers.map(modifierToDoc), Doc.space) <> Doc.space else Doc.empty
      val typeDoc = propertyType.map(t => Doc.text(": ") <> typeToDoc(t)).getOrElse(Doc.empty)
      val initDoc = init.map(i => Doc.text(" = ") <> toDoc(i)).getOrElse(Doc.empty)
      modsDoc <> Doc.text(name) <> typeDoc <> initDoc <> Doc.text(";")
    case ClassMember.Getter(name, returnType, body, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) Doc.join(modifiers.map(modifierToDoc), Doc.space) <> Doc.space else Doc.empty
      modsDoc <> Doc.text("get ") <> Doc.text(name) <> Doc.text("()") <>
        returnType.map(t => Doc.text(": ") <> typeToDoc(t)).getOrElse(Doc.empty) <+> toDoc(body)
    case ClassMember.Setter(name, param, body, modifiers, _) =>
      val modsDoc = if (modifiers.nonEmpty) Doc.join(modifiers.map(modifierToDoc), Doc.space) <> Doc.space else Doc.empty
      modsDoc <> Doc.text("set ") <> Doc.text(name) <> Doc.text("(") <> paramToDoc(param) <> Doc.text(")") <+> toDoc(body)
    case ClassMember.Index(params, returnType, _) =>
      Doc.text("[") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text("]: ") <> typeToDoc(returnType) <> Doc.text(";")
  }

  def interfaceMemberToDoc(member: InterfaceMember): Doc = {
    val typeDoc = member.memberType match {
      case InterfaceMemberType.PropertySignature(propertyType, isOptional, isReadonly) =>
        val readonlyDoc = if (isReadonly) Doc.text("readonly ") else Doc.empty
        val optionalDoc = if (isOptional) Doc.text("?") else Doc.empty
        readonlyDoc <> Doc.text(member.name) <> optionalDoc <> Doc.text(": ") <> typeToDoc(propertyType)
      case InterfaceMemberType.MethodSignature(params, returnType) =>
        Doc.text(member.name) <> Doc.text("(") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> 
          Doc.text("): ") <> typeToDoc(returnType)
      case InterfaceMemberType.CallSignature(params, returnType) =>
        Doc.text("(") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text("): ") <> typeToDoc(returnType)
      case InterfaceMemberType.ConstructSignature(params, returnType) =>
        Doc.text("new (") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text("): ") <> typeToDoc(returnType)
      case InterfaceMemberType.IndexSignature(params, returnType) =>
        Doc.text("[") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text("]: ") <> typeToDoc(returnType)
    }
    typeDoc <> Doc.text(";")
  }

  def enumMemberToDoc(member: EnumMember): Doc = {
    val valueDoc = member.value.map(v => Doc.text(" = ") <> toDoc(v)).getOrElse(Doc.empty)
    Doc.text(member.name) <> valueDoc
  }

  def importSpecToDoc(spec: ImportSpecifier): Doc = spec match {
    case ImportSpecifier.Default(local, _) => Doc.text(local)
    case ImportSpecifier.Namespace(local, _) => Doc.text("* as ") <> Doc.text(local)
    case ImportSpecifier.Named(imported, localOpt, _) =>
      localOpt.map(local => Doc.text(imported) <+> Doc.text("as") <+> Doc.text(local)).getOrElse(Doc.text(imported))
  }

  def exportSpecToDoc(spec: ExportSpecifier): Doc = spec match {
    case ExportSpecifier.Named(local, exportedOpt, _) =>
      exportedOpt.map(exported => Doc.text(local) <+> Doc.text("as") <+> Doc.text(exported)).getOrElse(Doc.text(local))
    case ExportSpecifier.Default(_) => Doc.text("default")
    case ExportSpecifier.All(exportedOpt, _) =>
      Doc.text("*") <> exportedOpt.map(e => Doc.text(" as ") <> Doc.text(e)).getOrElse(Doc.empty)
  }

  def modifierToDoc(modifier: Modifier): Doc = modifier match {
    case Modifier.Public => Doc.text("public")
    case Modifier.Private => Doc.text("private")
    case Modifier.Protected => Doc.text("protected")
    case Modifier.Static => Doc.text("static")
    case Modifier.Readonly => Doc.text("readonly")
    case Modifier.Abstract => Doc.text("abstract")
    case Modifier.Async => Doc.text("async")
    case Modifier.Const => Doc.text("const")
    case Modifier.Declare => Doc.text("declare")
    case Modifier.Export => Doc.text("export")
  }

  def typeParamToDoc(tp: TypeParameter): Doc = {
    val constraintDoc = tp.constraint.map(c => Doc.text(" extends ") <> typeToDoc(c)).getOrElse(Doc.empty)
    val defaultDoc = tp.default.map(d => Doc.text(" = ") <> typeToDoc(d)).getOrElse(Doc.empty)
    Doc.text(tp.name) <> constraintDoc <> defaultDoc
  }

  def typeToDoc(tsType: TypeScriptType): Doc = tsType match {
    case TypeScriptType.PrimitiveType(name, _) => Doc.text(name)
    case TypeScriptType.TypeReference(name, typeArgs, _) =>
      if (typeArgs.isEmpty) Doc.text(name)
      else Doc.text(name) <> Doc.text("<") <> Doc.join(typeArgs.map(typeToDoc), Doc.text(", ")) <> Doc.text(">")
    case TypeScriptType.ArrayType(elementType, _) => typeToDoc(elementType) <> Doc.text("[]")
    case TypeScriptType.TupleType(elements, _) =>
      Doc.text("[") <> Doc.join(elements.map(typeToDoc), Doc.text(", ")) <> Doc.text("]")
    case TypeScriptType.UnionType(types, _) =>
      Doc.join(types.map(typeToDoc), Doc.text(" | "))
    case TypeScriptType.IntersectionType(types, _) =>
      Doc.join(types.map(typeToDoc), Doc.text(" & "))
    case TypeScriptType.FunctionType(params, returnType, _) =>
      Doc.text("(") <> Doc.join(params.map(paramToDoc), Doc.text(", ")) <> Doc.text(") => ") <> typeToDoc(returnType)
    case TypeScriptType.ObjectType(members, _) =>
      Doc.text("{") <+> Doc.join(members.map(interfaceMemberToDoc), Doc.space) <+> Doc.text("}")
    case TypeScriptType.LiteralType(value, _) => toDoc(value)
    case TypeScriptType.KeyofType(objectType, _) => Doc.text("keyof ") <> typeToDoc(objectType)
    case TypeScriptType.TypeofType(expr, _) => Doc.text("typeof ") <> toDoc(expr)
    case TypeScriptType.IndexedAccessType(objectType, indexType, _) =>
      typeToDoc(objectType) <> Doc.text("[") <> typeToDoc(indexType) <> Doc.text("]")
    case TypeScriptType.MappedType(typeParam, nameType, objectType, _) =>
      val nameDoc = nameType.map(n => Doc.text(" as ") <> typeToDoc(n)).getOrElse(Doc.empty)
      Doc.text("{[") <> Doc.text(typeParam.name) <+> Doc.text("in") <+> typeToDoc(objectType) <> nameDoc <> Doc.text("]: ") <> Doc.text("...}")
    case TypeScriptType.ConditionalType(checkType, extendsType, trueType, falseType, _) =>
      typeToDoc(checkType) <+> Doc.text("extends") <+> typeToDoc(extendsType) <+> Doc.text("?") <+> 
        typeToDoc(trueType) <+> Doc.text(":") <+> typeToDoc(falseType)
    case TypeScriptType.InferType(typeParam, _) => Doc.text("infer ") <> Doc.text(typeParam.name)
    case TypeScriptType.TemplateLiteralType(parts, types, _) =>
      val combined = parts.zip(types :+ null).flatMap { case (part, tpe) =>
        if (tpe == null) Vector(Doc.text(part))
        else Vector(Doc.text(part), Doc.text("${") <> typeToDoc(tpe) <> Doc.text("}"))
      }
      Doc.text("`") <> Doc.concat(combined) <> Doc.text("`")
    case TypeScriptType.ParenthesizedType(innerType, _) => Doc.text("(") <> typeToDoc(innerType) <> Doc.text(")")
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
  span: Span
) derives ReadWriter

case class VariableDeclarator(
  name: String,
  varType: Option[TypeScriptType],
  init: Option[TypeScriptAST],
  span: Span
) derives ReadWriter

case class ObjectProperty(
  key: Either[String, TypeScriptAST],
  value: TypeScriptAST,
  isShorthand: Boolean,
  isMethod: Boolean,
  span: Span
) derives ReadWriter

case class SwitchCase(
  test: Option[TypeScriptAST], // None for default case
  consequent: Vector[TypeScriptAST],
  span: Span
) derives ReadWriter

case class CatchClause(
  param: Option[String],
  body: TypeScriptAST,
  span: Span
) derives ReadWriter

enum ClassMember derives ReadWriter {
  case Constructor(params: Vector[Parameter], body: TypeScriptAST, span: Span)
  case Method(name: String, params: Vector[Parameter], returnType: Option[TypeScriptType], body: TypeScriptAST, modifiers: Vector[Modifier], span: Span)
  case Property(name: String, propertyType: Option[TypeScriptType], init: Option[TypeScriptAST], modifiers: Vector[Modifier], span: Span)
  case Getter(name: String, returnType: Option[TypeScriptType], body: TypeScriptAST, modifiers: Vector[Modifier], span: Span)
  case Setter(name: String, param: Parameter, body: TypeScriptAST, modifiers: Vector[Modifier], span: Span)
  case Index(params: Vector[Parameter], returnType: TypeScriptType, span: Span)
}

case class InterfaceMember(
  name: String,
  memberType: InterfaceMemberType,
  span: Span
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
  span: Span
) derives ReadWriter

enum ImportSpecifier derives ReadWriter {
  case Default(local: String, span: Span)
  case Namespace(local: String, span: Span)
  case Named(imported: String, local: Option[String], span: Span)
}

enum ExportSpecifier derives ReadWriter {
  case Named(local: String, exported: Option[String], span: Span)
  case Default(span: Span)
  case All(exported: Option[String], span: Span)
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
  span: Span
) derives ReadWriter

/** TypeScript type system */
enum TypeScriptType derives ReadWriter {
  case PrimitiveType(name: String, span: Span) // string, number, boolean, void, any, unknown, never, etc.
  case TypeReference(name: String, typeArgs: Vector[TypeScriptType], span: Span)
  case ArrayType(elementType: TypeScriptType, span: Span)
  case TupleType(elements: Vector[TypeScriptType], span: Span)
  case UnionType(types: Vector[TypeScriptType], span: Span)
  case IntersectionType(types: Vector[TypeScriptType], span: Span)
  case FunctionType(params: Vector[Parameter], returnType: TypeScriptType, span: Span)
  case ObjectType(members: Vector[InterfaceMember], span: Span)
  case LiteralType(value: TypeScriptAST, span: Span)
  case KeyofType(objectType: TypeScriptType, span: Span)
  case TypeofType(expr: TypeScriptAST, span: Span)
  case IndexedAccessType(objectType: TypeScriptType, indexType: TypeScriptType, span: Span)
  case MappedType(typeParam: TypeParameter, nameType: Option[TypeScriptType], objectType: TypeScriptType, span: Span)
  case ConditionalType(checkType: TypeScriptType, extendsType: TypeScriptType, trueType: TypeScriptType, falseType: TypeScriptType, span: Span)
  case InferType(typeParam: TypeParameter, span: Span)
  case TemplateLiteralType(parts: Vector[String], types: Vector[TypeScriptType], span: Span)
  case ParenthesizedType(innerType: TypeScriptType, span: Span)
}
