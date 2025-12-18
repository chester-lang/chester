package chester.tyck

import scala.collection.mutable
import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, StmtAST, Telescope}
import chester.error.Span
import chester.uniqid.UniqidOf
import chester.utils.asInt

enum DefinitionKind:
  case Def, Record

case class DefinitionEntry(kind: DefinitionKind, name: String, span: Span)

/** Simple index of definition locations and their usages. */
case class SymbolIndex(
    definitions: Map[UniqidOf[AST], DefinitionEntry],
    references: Map[UniqidOf[AST], Vector[Span]]
):

  private def contains(span: Span, line: Int, character: Int): Boolean = {
    val start = span.range.start
    val end = span.range.end
    val startLine = start.line.asInt
    val endLine = end.line.asInt
    val startCol = start.column.utf16.asInt
    val endCol = end.column.utf16.asInt

    if line < startLine || line > endLine then false
    else if startLine == endLine then character >= startCol && character < endCol
    else if line == startLine then character >= startCol
    else if line == endLine then character < endCol
    else true
  }

  private def spanSize(span: Span): Int =
    span.range.end.index.unicode.asInt - span.range.start.index.unicode.asInt

  /** Locate the nearest symbol (def/record) ID at the given zero-based position. */
  def findSymbolAt(line: Int, character: Int): Option[UniqidOf[AST]] = {
    val candidates = {
      definitions.toVector.flatMap { case (id, entry) =>
        if contains(entry.span, line, character) then Some((id, entry.span)) else None
      } ++ references.toVector.flatMap { case (id, spans) =>
        spans.flatMap(s => if contains(s, line, character) then Some((id, s)) else None)
      }
    }

    candidates.sortBy((_, span) => spanSize(span)).headOption.map(_._1)
  }

  def definition(id: UniqidOf[AST]): Option[DefinitionEntry] = definitions.get(id)

  def usages(id: UniqidOf[AST]): Vector[Span] = references.getOrElse(id, Vector.empty)

object SymbolIndex:
  def fromAst(ast: AST): SymbolIndex = {
    val defs = mutable.Map.empty[UniqidOf[AST], DefinitionEntry]
    val refs = mutable.Map.empty[UniqidOf[AST], mutable.Builder[Span, Vector[Span]]]
    val names = mutable.Map.empty[UniqidOf[AST], String]

    def recordRef(id: UniqidOf[AST], span: Option[Span], name: Option[String]): Unit = {
      name.foreach(n => names.getOrElseUpdate(id, n))
      span.foreach { s =>
        val builder = refs.getOrElseUpdate(id, Vector.newBuilder[Span])
        builder += s
      }
    }

    def pickSpan(spans: Seq[Option[Span]]): Option[Span] =
      spans.collectFirst { case Some(s) => s }

    def walkStmt(stmt: StmtAST): Unit = stmt match
      case StmtAST.ExprStmt(expr, _) =>
        walkAst(expr)
      case StmtAST.JSImport(id, localName, _, _, ty, span) =>
        pickSpan(Seq(span, ty.span)).foreach(s => defs.update(id, DefinitionEntry(DefinitionKind.Def, localName, s)))
        names.update(id, localName)
        walkAst(ty)
      case StmtAST.Def(id, name, telescopes, resultTy, body, span) =>
        pickSpan(Seq(span, body.span, resultTy.flatMap(_.span))).foreach(s => defs.update(id, DefinitionEntry(DefinitionKind.Def, name, s)))
        names.update(id, name)
        telescopes.foreach(walkTele)
        resultTy.foreach(walkAst)
        walkAst(body)
      case StmtAST.Record(id, name, fields, span) =>
        val fieldSpans = fields.flatMap(_.ty.span)
        pickSpan(Seq(span) ++ fieldSpans.map(Some(_))).foreach(s => defs.update(id, DefinitionEntry(DefinitionKind.Record, name, s)))
        names.update(id, name)
        fields.foreach(walkParam)
      case StmtAST.Enum(_, _, typeParams, cases, _) =>
        typeParams.foreach(walkParam)
        cases.foreach(c => c.params.foreach(walkParam))
      case StmtAST.Coenum(_, _, typeParams, cases, _) =>
        typeParams.foreach(walkParam)
        cases.foreach(c => c.params.foreach(walkParam))
      case StmtAST.Pkg(_, body, _) =>
        walkAst(body)

    def walkParam(param: chester.core.Param): Unit = {
      walkAst(param.ty)
      param.default.foreach(walkAst)
    }

    def walkTele(tele: Telescope): Unit =
      tele.params.foreach(walkParam)

    def walkAst(ast: AST): Unit = ast match
      case AST.Ref(id, name, span) =>
        recordRef(id, span, Some(name))
      case AST.Tuple(elements, _) =>
        elements.foreach(walkAst)
      case AST.ListLit(elements, _) =>
        elements.foreach(walkAst)
      case AST.Block(elements, tail, _) =>
        elements.foreach(walkStmt)
        walkAst(tail)
      case AST.StringLit(_, _) | AST.IntLit(_, _) | AST.NaturalLit(_, _) | AST.LevelLit(_, _) | AST.AnyType(_) | AST.StringType(_) |
          AST.NaturalType(_) | AST.IntegerType(_) | AST.LevelType(_) =>
        ()
      case AST.TupleType(elements, _) =>
        elements.foreach(walkAst)
      case AST.ListType(element, _) =>
        walkAst(element)
      case AST.Pi(telescopes, resultTy, _, _) =>
        telescopes.foreach(walkTele)
        walkAst(resultTy)
      case AST.Lam(telescopes, body, _) =>
        telescopes.foreach(walkTele)
        walkAst(body)
      case AST.App(func, args, _, _) =>
        walkAst(func)
        args.foreach(a => walkAst(a.value))
      case AST.Let(_, _, ty, value, body, _) =>
        ty.foreach(walkAst)
        walkAst(value)
        walkAst(body)
      case AST.Ann(expr, ty, _) =>
        walkAst(expr)
        walkAst(ty)
      case AST.RecordTypeRef(id, name, span) =>
        recordRef(id, span, Some(name))
      case AST.RecordCtor(id, name, args, span) =>
        recordRef(id, span, Some(name))
        args.foreach(walkAst)
      case AST.FieldAccess(target, _, _) =>
        walkAst(target)
      case AST.EnumTypeRef(_, _, _) =>
        ()
      case AST.EnumCaseRef(_, _, _, _, _) =>
        ()
      case AST.EnumCtor(_, _, _, _, args, _) =>
        args.foreach(walkAst)
      case AST.MetaCell(cell, _) =>
        // MetaCell is opaque here; nothing to traverse
        ()
      case AST.Type(level, _) =>
        walkAst(level)
      case AST.TypeOmega(level, _) =>
        walkAst(level)

    walkAst(ast)

    // Backfill definitions for IDs we saw in references but had no declaration spans.
    refs.foreach { case (id, builder) =>
      if !defs.contains(id) then
        builder.result().headOption.foreach { span =>
          val name = names.getOrElse(id, "<unknown>")
          defs.update(id, DefinitionEntry(DefinitionKind.Def, name, span))
        }
    }

    SymbolIndex(defs.toMap, refs.view.mapValues(_.result()).toMap)
  }
