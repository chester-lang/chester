package chester.core

import scala.language.experimental.genericNumberLiterals
import chester.error.{Span, SpanOptional}
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*
import cats.data.NonEmptyVector
import chester.utils.{*, given}
import chester.uniqid.{UniqidOf, Uniqid, rwUniqIDOf, UniqidCollector, UniqidReplacer, ContainsUniqid, given}

case class Param(
  id: UniqidOf[AST],
  name: String,
  ty: AST,
  default: Option[AST] = None
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit =
    collector(id)
    ty.collectUniqids(collector)
    default.foreach(_.collectUniqids(collector))
  
  def mapUniqids(mapper: UniqidReplacer): Param =
    Param(mapper(id), name, ty.mapUniqids(mapper), default.map(_.mapUniqids(mapper)))

case class Arg(
  name: Option[String],
  value: AST
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit =
    value.collectUniqids(collector)
  
  def mapUniqids(mapper: UniqidReplacer): Arg =
    Arg(name, value.mapUniqids(mapper))

enum AST(val span: Option[Span]) extends ToDoc with ContainsUniqid with SpanOptional derives ReadWriter:
  case Ref(id: UniqidOf[AST], name: String, override val span: Option[Span]) extends AST(span)
  case Tuple(elements: Vector[AST], override val span: Option[Span]) extends AST(span)
  case ListLit(elements: Vector[AST], override val span: Option[Span]) extends AST(span)
  case Block(elements: Vector[AST], override val span: Option[Span]) extends AST(span)
  case StringLit(value: String, override val span: Option[Span]) extends AST(span)
  case IntLit(value: BigInt, override val span: Option[Span]) extends AST(span)
  case Universe(level: AST, override val span: Option[Span]) extends AST(span)
  case Pi(params: Vector[Param], resultTy: AST, override val span: Option[Span]) extends AST(span)
  case Lam(params: Vector[Param], body: AST, override val span: Option[Span]) extends AST(span)
  case App(func: AST, args: Vector[Arg], override val span: Option[Span]) extends AST(span)
  case Let(id: UniqidOf[AST], name: String, ty: Option[AST], value: AST, body: AST, override val span: Option[Span]) extends AST(span)
  case Ann(expr: AST, ty: AST, override val span: Option[Span]) extends AST(span)

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
    case AST.Universe(level, _) =>
      text("Type") <> brackets(level.toDoc)
    case AST.Pi(params, resultTy, _) =>
      val paramsDoc = hsep(params.map(p => 
        parens(text(p.name) <> text(":") <+> p.ty.toDoc <>
          p.default.map(d => text(" = ") <> d.toDoc).getOrElse(empty))
      ), empty)
      paramsDoc <+> text("->") <+> resultTy.toDoc
    case AST.Lam(params, body, _) =>
      val paramsDoc = hsep(params.map(p =>
        parens(text(p.name) <> text(":") <+> p.ty.toDoc <>
          p.default.map(d => text(" = ") <> d.toDoc).getOrElse(empty))
      ), empty)
      text("λ") <> paramsDoc <+> text("=>") <+> body.toDoc
    case AST.App(func, args, _) =>
      val argsDoc = hsep(args.map(a =>
        a.name.map(n => text(n) <> text(" = ") <> a.value.toDoc).getOrElse(a.value.toDoc)
      ), `,` <+> empty)
      func.toDoc <> parens(argsDoc)
    case AST.Let(id, name, ty, value, body, _) =>
      val tyDoc = ty.map(t => text(":") <+> t.toDoc).getOrElse(empty)
      text("let") <+> text(name) <+> tyDoc <+> text("=") <+> value.toDoc <+> text("in") <@@> body.toDoc.indented()
    case AST.Ann(expr, ty, _) =>
      parens(expr.toDoc <+> text(":") <+> ty.toDoc)

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
    case AST.Universe(level, _) =>
      level.collectUniqids(collector)
    case AST.Pi(params, resultTy, _) =>
      params.foreach { p =>
        collector(p.id)
        p.ty.collectUniqids(collector)
        p.default.foreach(_.collectUniqids(collector))
      }
      resultTy.collectUniqids(collector)
    case AST.Lam(params, body, _) =>
      params.foreach { p =>
        collector(p.id)
        p.ty.collectUniqids(collector)
        p.default.foreach(_.collectUniqids(collector))
      }
      body.collectUniqids(collector)
    case AST.App(func, args, _) =>
      func.collectUniqids(collector)
      args.foreach(_.value.collectUniqids(collector))
    case AST.Let(id, _, ty, value, body, _) =>
      collector(id)
      ty.foreach(_.collectUniqids(collector))
      value.collectUniqids(collector)
      body.collectUniqids(collector)
    case AST.Ann(expr, ty, _) =>
      expr.collectUniqids(collector)
      ty.collectUniqids(collector)

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
    case AST.Universe(level, span) =>
      AST.Universe(level.mapUniqids(mapper), span)
    case AST.Pi(params, resultTy, span) =>
      AST.Pi(
        params.map(p => Param(mapper(p.id), p.name, p.ty.mapUniqids(mapper), p.default.map(_.mapUniqids(mapper)))),
        resultTy.mapUniqids(mapper),
        span
      )
    case AST.Lam(params, body, span) =>
      AST.Lam(
        params.map(p => Param(mapper(p.id), p.name, p.ty.mapUniqids(mapper), p.default.map(_.mapUniqids(mapper)))),
        body.mapUniqids(mapper),
        span
      )
    case AST.App(func, args, span) =>
      AST.App(
        func.mapUniqids(mapper),
        args.map(a => Arg(a.name, a.value.mapUniqids(mapper))),
        span
      )
    case AST.Let(id, name, ty, value, body, span) =>
      AST.Let(mapper(id), name, ty.map(_.mapUniqids(mapper)), value.mapUniqids(mapper), body.mapUniqids(mapper), span)
    case AST.Ann(expr, ty, span) =>
      AST.Ann(expr.mapUniqids(mapper), ty.mapUniqids(mapper), span)
