package chester.interop.typescript

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Arg, Implicitness, Param, Telescope}
import chester.syntax.{TypeScriptAST, TypeScriptType, VariableDeclarator}
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
    val want = Vector(moduleSpecifier, normalized, s"node:$normalized").distinct
    program match
      case TypeScriptAST.Program(statements, _) =>
        val bodies = statements.collect { case TypeScriptAST.NamespaceDeclaration(name, body, _) if want.contains(name) => body }
        bodies.headOption
      case _ => None
  }

  private def exportedValueFields(stmts: Vector[TypeScriptAST]): Vector[Param] = {
    val fields = scala.collection.mutable.LinkedHashMap.empty[String, AST]

    def add(name: String, ty: AST): Unit =
      if !fields.contains(name) then fields.update(name, ty)

    def walk(stmt: TypeScriptAST): Unit = stmt match
      case TypeScriptAST.ExportDeclaration(Some(decl), _, _, _, _) =>
        decl match
          case fn: TypeScriptAST.FunctionDeclaration =>
            add(fn.name, chesterTypeFromFunction(fn))
          case TypeScriptAST.VariableDeclaration(_, declarators, _) =>
            declarators.foreach(d => add(d.name, chesterTypeFromVar(d)))
          case _ => ()
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
