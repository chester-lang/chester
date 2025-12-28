package chester.interop.typescript

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, Implicitness, Param, Telescope}
import chester.syntax.{InterfaceMember, InterfaceMemberType, Parameter as TSParameter, TypeScriptAST, TypeScriptType, VariableDeclarator}
import chester.tyck.{JSImportSignature, JSImportSignature as JSImportSupport}
import chester.uniqid.Uniqid

/** Very small bridge from parsed TypeScript declarations into Chester types.
  *
  * This intentionally supports only a minimal subset needed to type module imports:
  *   - exported `function` declarations
  *   - exported `const/let/var` declarations
  *
  * Everything else is conservatively mapped to `Any`.
  */
object TypeScriptToChester:

  def moduleSignature(program: TypeScriptAST, moduleSpecifier: String): JSImportSignature = {
    val normalized = JSImportSupport.normalizeModuleSpecifier(moduleSpecifier)
    val moduleBody = selectModuleBody(program, moduleSpecifier, normalized).getOrElse(program match
      case TypeScriptAST.Program(stmts, _) => stmts
      case other                           => Vector(other))
    val exports = exportedValueFields(moduleBody)
    JSImportSignature(exports)
  }

  private def selectModuleBody(
      program: TypeScriptAST,
      moduleSpecifier: String,
      normalized: String
  ): Option[Vector[TypeScriptAST]] = {
    val candidates = Vector(moduleSpecifier, s"node:$normalized", normalized).distinct

    def scoreStmt(stmt: TypeScriptAST): Int = stmt match
      case TypeScriptAST.InterfaceDeclaration(_, _, _, _, _)   => 2
      case TypeScriptAST.FunctionDeclaration(_, _, _, _, _, _) => 2
      case TypeScriptAST.VariableDeclaration(_, _, _)          => 2
      case TypeScriptAST.TypeAliasDeclaration(_, _, _, _)      => 2
      case TypeScriptAST.EnumDeclaration(_, _, _, _)           => 2
      case TypeScriptAST.ClassDeclaration(_, _, _, _, _, _, _) => 2
      case TypeScriptAST.NamespaceDeclaration(_, body, _)      => 1 + body.map(scoreStmt).sum
      case TypeScriptAST.ExportDeclaration(Some(inner), _, _, _, _) =>
        scoreStmt(inner)
      case TypeScriptAST.ExportAssignment(_, _) => 1
      case _                                     => 0

    def scoreBody(body: Vector[TypeScriptAST]): Int =
      body.map(scoreStmt).sum

    program match
      case TypeScriptAST.Program(statements, _) =>
        val modules = statements.collect { case TypeScriptAST.NamespaceDeclaration(name, body, _) => (name, body) }
        def findBody(name: String): Option[Vector[TypeScriptAST]] =
          modules.collectFirst { case (n, body) if n == name => body }

        var best: Option[(Vector[TypeScriptAST], Int)] = None
        candidates.foreach { name =>
          findBody(name).foreach { body =>
            val score = scoreBody(body)
            best match
              case None                             => best = Some((body, score))
              case Some((_, bestScore)) if score > bestScore => best = Some((body, score))
              case _                                 => ()
          }
        }
        best.map(_._1)
      case _ => None
  }

  private def exportedValueFields(stmts: Vector[TypeScriptAST]): Vector[Param] = {
    val env = collectDecls(stmts, Vector.empty)
    val fields = scala.collection.mutable.LinkedHashMap.empty[String, AST]

    def add(name: String, ty: AST): Unit =
      if !fields.contains(name) then fields.update(name, ty)

    def addMembers(members: Vector[InterfaceMember]): Unit =
      members.foreach { member =>
        if member.name.nonEmpty then
          interfaceMemberType(member).foreach(ty => add(member.name, ty))
      }

    def addFromType(tsType: TypeScriptType): Unit =
      fieldsFromType(tsType, env).foreach { case (name, ty) => add(name, ty) }

    def resolveExportAssignment(expr: TypeScriptAST): Unit = {
      val target = expr match
        case TypeScriptAST.Identifier(name, _)            => env.values.get(name)
        case TypeScriptAST.PropertyAccess(_, property, _) =>
          env.values.get(property).orElse(env.values.find { case (k, _) => k.endsWith("." + property) }.map(_._2))
        case TypeScriptAST.ElementAccess(_, _, _)         => None
        case TypeScriptAST.Call(_, _, _)                  => None
        case TypeScriptAST.Parenthesized(inner, _)        => resolveExportAssignment(inner); None
        case _                                            => None
      target.foreach(addFromType)
    }

    def walk(stmt: TypeScriptAST): Unit = stmt match
      case TypeScriptAST.ExportDeclaration(Some(decl), _, _, _, _) =>
        decl match
          case fn: TypeScriptAST.FunctionDeclaration =>
            add(fn.name, chesterTypeFromFunction(fn))
          case TypeScriptAST.VariableDeclaration(_, declarators, _) =>
            declarators.foreach(d => add(d.name, chesterTypeFromVar(d)))
          case _ => ()
      case TypeScriptAST.ExportAssignment(expr, _) =>
        resolveExportAssignment(expr)
      case TypeScriptAST.NamespaceDeclaration(_, body, _) =>
        body.foreach(walk)
      case _ => ()

    stmts.foreach(walk)
    fields.toVector.map { case (name, ty) =>
      Param(Uniqid.make, name, ty, Implicitness.Explicit, None)
    }
  }

  private def chesterTypeFromVar(decl: VariableDeclarator): AST =
    decl.varType.map(chesterTypeFromTs).getOrElse(AST.AnyType(None))

  private def chesterTypeFromSignature(params: Vector[TSParameter], returnType: TypeScriptType): AST = {
    val ps = params.map { p =>
      val baseTy = p.paramType.map(chesterTypeFromTs).getOrElse(AST.AnyType(None))
      val paramTy = if p.isRest then AST.ListType(baseTy, None) else baseTy
      Param(Uniqid.make, p.name, paramTy, Implicitness.Explicit, None)
    }
    AST.Pi(Vector(Telescope(ps, Implicitness.Explicit)), chesterTypeFromTs(returnType), Vector.empty, None)
  }

  private def chesterTypeFromFunction(fn: TypeScriptAST.FunctionDeclaration): AST = {
    val params = fn.params.map { p =>
      val baseTy = p.paramType.map(chesterTypeFromTs).getOrElse(AST.AnyType(None))
      val paramTy = if p.isRest then AST.ListType(baseTy, None) else baseTy
      Param(Uniqid.make, p.name, paramTy, Implicitness.Explicit, None)
    }
    val retTy = fn.returnType.map(chesterTypeFromTs).getOrElse(AST.AnyType(None))
    AST.Pi(Vector(Telescope(params, Implicitness.Explicit)), retTy, Vector.empty, None)
  }

  private def chesterTypeFromTs(tsType: TypeScriptType): AST = {
    tsType match
      case TypeScriptType.PrimitiveType("string", span) => AST.StringType(span)
      case TypeScriptType.PrimitiveType("number", span) => AST.IntegerType(span)
      case TypeScriptType.PrimitiveType("void", span)   => AST.TupleType(Vector.empty, span)
      case TypeScriptType.PrimitiveType("any", span)    => AST.AnyType(span)
      case TypeScriptType.PrimitiveType("unknown", span) =>
        AST.AnyType(span)
      case TypeScriptType.PrimitiveType(_, span) =>
        AST.AnyType(span)
      case TypeScriptType.ArrayType(elem, span) =>
        AST.ListType(chesterTypeFromTs(elem), span)
      case TypeScriptType.TypeReference("Array", Vector(elem), span) =>
        AST.ListType(chesterTypeFromTs(elem), span)
      case TypeScriptType.FunctionType(params, returnType, span) =>
        val ps = params.map { p =>
          val baseTy = p.paramType.map(chesterTypeFromTs).getOrElse(AST.AnyType(None))
          val paramTy = if p.isRest then AST.ListType(baseTy, None) else baseTy
          Param(Uniqid.make, p.name, paramTy, Implicitness.Explicit, None)
        }
        AST.Pi(Vector(Telescope(ps, Implicitness.Explicit)), chesterTypeFromTs(returnType), Vector.empty, span)
      case TypeScriptType.ParenthesizedType(inner, _) =>
        chesterTypeFromTs(inner)
      case TypeScriptType.LiteralType(value, span) =>
        value match
          case TypeScriptAST.StringLiteral(_, _)  => AST.StringType(span)
          case TypeScriptAST.NumberLiteral(_, _)  => AST.IntegerType(span)
          case TypeScriptAST.BooleanLiteral(_, _) => AST.AnyType(span)
          case _                                  => AST.AnyType(span)
      case _ =>
        AST.AnyType(tsType.span)
  }

  private final case class DeclEnv(
      values: Map[String, TypeScriptType],
      interfaces: Map[String, Vector[InterfaceMember]]
  )

  private def collectDecls(stmts: Vector[TypeScriptAST], prefix: Vector[String]): DeclEnv = {
    val values = scala.collection.mutable.LinkedHashMap.empty[String, TypeScriptType]
    val interfaces = scala.collection.mutable.LinkedHashMap.empty[String, Vector[InterfaceMember]]

    def qualify(parts: Vector[String], name: String): String =
      (parts :+ name).filter(_.nonEmpty).mkString(".")

    def recordValue(name: String, tpe: TypeScriptType, parts: Vector[String]): Unit = {
      val qualified = qualify(parts, name)
      if !values.contains(qualified) then values.update(qualified, tpe)
      if !values.contains(name) then values.update(name, tpe)
    }

    def recordInterface(name: String, members: Vector[InterfaceMember], parts: Vector[String]): Unit = {
      val qualified = qualify(parts, name)
      if !interfaces.contains(qualified) then interfaces.update(qualified, members)
      if !interfaces.contains(name) then interfaces.update(name, members)
    }

    def visit(stmt: TypeScriptAST, parts: Vector[String]): Unit = stmt match
      case TypeScriptAST.InterfaceDeclaration(name, _, _, members, _) =>
        recordInterface(name, members, parts)
      case TypeScriptAST.FunctionDeclaration(name, params, returnType, _, _, _) =>
        val ret = returnType.getOrElse(TypeScriptType.PrimitiveType("any", None))
        val fnType = TypeScriptType.FunctionType(params, ret, None)
        recordValue(name, fnType, parts)
      case TypeScriptAST.VariableDeclaration(_, declarators, _) =>
        declarators.foreach { decl =>
          decl.varType.foreach { tpe =>
            recordValue(decl.name, tpe, parts)
          }
        }
      case TypeScriptAST.NamespaceDeclaration(name, body, _) =>
        val nextParts = if name == "global" then parts else parts :+ name
        body.foreach(stmt => visit(stmt, nextParts))
      case TypeScriptAST.ExportDeclaration(Some(inner), _, _, _, _) =>
        visit(inner, parts)
      case _ => ()

    stmts.foreach(stmt => visit(stmt, prefix))
    DeclEnv(values.toMap, interfaces.toMap)
  }

  private def fieldsFromType(tsType: TypeScriptType, env: DeclEnv): Vector[(String, AST)] = {
    tsType match
      case TypeScriptType.ObjectType(members, _) =>
        members.flatMap(member => interfaceMemberType(member).map(ty => member.name -> ty))
      case TypeScriptType.TypeReference(name, _, _) =>
        val exact = env.interfaces.get(name)
        val fallback = env.interfaces.get(name.split('.').lastOption.getOrElse(name))
        exact
          .orElse(fallback)
          .toVector
          .flatten
          .flatMap(member => interfaceMemberType(member).map(ty => member.name -> ty))
      case _ =>
        Vector.empty
  }

  private def interfaceMemberType(member: InterfaceMember): Option[AST] = {
    member.memberType match
      case InterfaceMemberType.PropertySignature(propertyType, _, _) =>
        Some(chesterTypeFromTs(propertyType))
      case InterfaceMemberType.MethodSignature(params, returnType) =>
        Some(chesterTypeFromSignature(params, returnType))
      case InterfaceMemberType.CallSignature(_, _) =>
        None
      case InterfaceMemberType.ConstructSignature(_, _) =>
        None
      case InterfaceMemberType.IndexSignature(_, _) =>
        None
  }

  extension (tsType: TypeScriptType)
    private def span: Option[chester.error.Span] = tsType match
      case TypeScriptType.PrimitiveType(_, span)        => span
      case TypeScriptType.TypeReference(_, _, span)     => span
      case TypeScriptType.ArrayType(_, span)            => span
      case TypeScriptType.TupleType(_, span)            => span
      case TypeScriptType.UnionType(_, span)            => span
      case TypeScriptType.IntersectionType(_, span)     => span
      case TypeScriptType.FunctionType(_, _, span)      => span
      case TypeScriptType.ObjectType(_, span)           => span
      case TypeScriptType.LiteralType(_, span)          => span
      case TypeScriptType.KeyofType(_, span)            => span
      case TypeScriptType.TypeofType(_, span)           => span
      case TypeScriptType.IndexedAccessType(_, _, span) => span
      case TypeScriptType.MappedType(_, _, _, span)     => span
      case TypeScriptType.ConditionalType(_, _, _, _, span) =>
        span
      case TypeScriptType.InferType(_, span)              => span
      case TypeScriptType.TemplateLiteralType(_, _, span) => span
      case TypeScriptType.ParenthesizedType(_, span)      => span
