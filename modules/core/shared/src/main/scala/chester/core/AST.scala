package chester.core

import scala.language.experimental.genericNumberLiterals
import chester.error.Span
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*
import cats.data.NonEmptyVector
import chester.utils.{*, given}
import chester.uniqid.{UniqidOf, Uniqid, rwUniqIDOf, UniqidCollector, UniqidReplacer, ContainsUniqid, given}

enum AST(val span: Span) extends ToDoc with ContainsUniqid derives ReadWriter:
  case Ref(id: UniqidOf[AST], name: String, override val span: Span) extends AST(span)
  case Tuple(elements: Vector[AST], override val span: Span) extends AST(span)
  case ListLit(elements: Vector[AST], override val span: Span) extends AST(span)
  case Block(elements: Vector[AST], override val span: Span) extends AST(span)
  case StringLit(value: String, override val span: Span) extends AST(span)
  case IntLit(value: BigInt, override val span: Span) extends AST(span)
  case App(func: AST, args: Vector[AST], override val span: Span) extends AST(span)
  case Lam(params: Vector[(UniqidOf[AST], String)], body: AST, override val span: Span) extends AST(span)
  case Let(id: UniqidOf[AST], name: String, value: AST, body: AST, override val span: Span) extends AST(span)

  def toDoc(using options: DocConf): Doc = this match
    case AST.Ref(id, name, _) =>
      text(name)
    case AST.Tuple(elements, _) =>
      parens(hsep(elements.map(_.toDoc), `,` <+> empty))
    case AST.ListLit(elements, _) =>
      brackets(hsep(elements.map(_.toDoc), `,` <+> empty))
    case AST.Block(elements, _) =>
      if elements.isEmpty then braces(empty)
      else braces(line <> ssep(elements.map(_.toDoc), `;` <> line).indented() <> line)
    case AST.StringLit(value, _) =>
      text("\"") <> text(value) <> text("\"")
    case AST.IntLit(value, _) =>
      text(value.toString)
    case AST.App(func, args, _) =>
      func.toDoc <+> parens(hsep(args.map(_.toDoc), `,` <+> empty))
    case AST.Lam(params, body, _) =>
      text("λ") <> parens(hsep(params.map((_, name) => text(name)), `,` <+> empty)) <+> text("=>") <+> body.toDoc
    case AST.Let(id, name, value, body, _) =>
      text("let") <+> text(name) <+> text("=") <+> value.toDoc <+> text("in") <@@> body.toDoc.indented()

  def collectUniqids(collector: UniqidCollector): Unit = this match
    case AST.Ref(id, _, _) =>
      collector(id)
    case AST.Tuple(elements, _) =>
      elements.foreach(_.collectUniqids(collector))
    case AST.ListLit(elements, _) =>
      elements.foreach(_.collectUniqids(collector))
    case AST.Block(elements, _) =>
      elements.foreach(_.collectUniqids(collector))
    case AST.StringLit(_, _) =>
      ()
    case AST.IntLit(_, _) =>
      ()
    case AST.App(func, args, _) =>
      func.collectUniqids(collector)
      args.foreach(_.collectUniqids(collector))
    case AST.Lam(params, body, _) =>
      params.foreach((id, _) => collector(id))
      body.collectUniqids(collector)
    case AST.Let(id, _, value, body, _) =>
      collector(id)
      value.collectUniqids(collector)
      body.collectUniqids(collector)

  def mapUniqids(mapper: UniqidReplacer): AST = this match
    case AST.Ref(id, name, span) =>
      AST.Ref(mapper(id), name, span)
    case AST.Tuple(elements, span) =>
      AST.Tuple(elements.map(_.mapUniqids(mapper)), span)
    case AST.ListLit(elements, span) =>
      AST.ListLit(elements.map(_.mapUniqids(mapper)), span)
    case AST.Block(elements, span) =>
      AST.Block(elements.map(_.mapUniqids(mapper)), span)
    case AST.StringLit(value, span) =>
      AST.StringLit(value, span)
    case AST.IntLit(value, span) =>
      AST.IntLit(value, span)
    case AST.App(func, args, span) =>
      AST.App(func.mapUniqids(mapper), args.map(_.mapUniqids(mapper)), span)
    case AST.Lam(params, body, span) =>
      AST.Lam(params.map((id, name) => (mapper(id), name)), body.mapUniqids(mapper), span)
    case AST.Let(id, name, value, body, span) =>
      AST.Let(mapper(id), name, value.mapUniqids(mapper), body.mapUniqids(mapper), span)
