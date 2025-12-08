package chester.core

import chester.error.{Span, SpanOptional}
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*
import cats.data.NonEmptyVector
import chester.utils.{*, given}
import chester.utils.{HoldNotReadable, holdNotReadableRW}
import chester.uniqid.{ContainsUniqid, Uniqid, UniqidCollector, UniqidOf, UniqidReplacer, rwUniqIDOf, given}

import scala.language.experimental.genericNumberLiterals

enum Implicitness derives ReadWriter:
  case Implicit
  case Explicit

case class Param(
    id: UniqidOf[AST],
    name: String,
    ty: AST,
    implicitness: Implicitness = Implicitness.Explicit,
    default: Option[AST] = None
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit =
    collector(id)
    ty.collectUniqids(collector)
    default.foreach(_.collectUniqids(collector))

  def mapUniqids(mapper: UniqidReplacer): Param =
    Param(mapper(id), name, ty.mapUniqids(mapper), implicitness, default.map(_.mapUniqids(mapper)))

/** Telescope: a sequence of parameters with the same implicitness */
case class Telescope(
    params: Vector[Param],
    implicitness: Implicitness
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit =
    params.foreach(_.collectUniqids(collector))

  def mapUniqids(mapper: UniqidReplacer): Telescope =
    Telescope(params.map(_.mapUniqids(mapper)), implicitness)

case class Arg(
    value: AST,
    implicitness: Implicitness = Implicitness.Explicit
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit =
    value.collectUniqids(collector)

  def mapUniqids(mapper: UniqidReplacer): Arg =
    Arg(value.mapUniqids(mapper), implicitness)

// Ensure HoldNotReadable ReadWriter is in scope for MetaCell
given [T]: ReadWriter[HoldNotReadable[T]] = holdNotReadableRW.asInstanceOf[ReadWriter[HoldNotReadable[T]]]

enum AST(val span: Option[Span]) extends ToDoc with ContainsUniqid with SpanOptional derives ReadWriter:
  case Package(name: String, body: AST, override val span: Option[Span]) extends AST(span)
  case Ref(id: UniqidOf[AST], name: String, override val span: Option[Span]) extends AST(span)
  case Tuple(elements: Vector[AST], override val span: Option[Span]) extends AST(span)
  case ListLit(elements: Vector[AST], override val span: Option[Span]) extends AST(span)
  case Block(elements: Vector[AST], tail: AST, override val span: Option[Span]) extends AST(span)
  case StringLit(value: String, override val span: Option[Span]) extends AST(span)
  case IntLit(value: BigInt, override val span: Option[Span]) extends AST(span)
  case Type(level: AST, override val span: Option[Span]) extends AST(span)
  case TypeOmega(level: AST, override val span: Option[Span]) extends AST(span)
  case AnyType(override val span: Option[Span]) extends AST(span)
  case StringType(override val span: Option[Span]) extends AST(span)
  case NaturalType(override val span: Option[Span]) extends AST(span)
  case IntegerType(override val span: Option[Span]) extends AST(span)
  case ListType(element: AST, override val span: Option[Span]) extends AST(span)
  case Pi(telescopes: Vector[Telescope], resultTy: AST, effects: Vector[String], override val span: Option[Span]) extends AST(span)
  case Lam(telescopes: Vector[Telescope], body: AST, override val span: Option[Span]) extends AST(span)
  case App(func: AST, args: Vector[Arg], implicitArgs: Boolean, override val span: Option[Span]) extends AST(span)
  case Let(id: UniqidOf[AST], name: String, ty: Option[AST], value: AST, body: AST, override val span: Option[Span]) extends AST(span)
  case Def(id: UniqidOf[AST], name: String, telescopes: Vector[Telescope], resultTy: Option[AST], body: AST, override val span: Option[Span])
      extends AST(span)
  case Ann(expr: AST, ty: AST, override val span: Option[Span]) extends AST(span)
  case MetaCell(cell: HoldNotReadable[chester.utils.elab.CellRW[AST]], override val span: Option[Span]) extends AST(span)

  def toDoc(using options: DocConf): Doc = this match
    case AST.Package(name, body, _) =>
      text("package") <+> text(name) <@@> body.toDoc.indented()
    case AST.Ref(id, name, _) =>
      text(name)
    case AST.Tuple(elements, _) =>
      parens(hsep(elements.map(_.toDoc), `,` <+> empty))
    case AST.ListLit(elements, _) =>
      brackets(hsep(elements.map(_.toDoc), `,` <+> empty))
    case AST.Block(elements, tail, _) =>
      val elemsDoc = if elements.isEmpty then empty else ssep(elements.map(_.toDoc), `;` <> line) <> `;` <> line
      braces(line <> (elemsDoc <> tail.toDoc).indented() <> line)
    case AST.StringLit(value, _) =>
      text("\"") <> text(value) <> text("\"")
    case AST.IntLit(value, _) =>
      text(value.toString)
    case AST.Type(level, _) =>
      text("Type") <> parens(level.toDoc)
    case AST.TypeOmega(level, _) =>
      text("Typeω") <> parens(level.toDoc)
    case AST.AnyType(_) =>
      text("Any")
    case AST.StringType(_) =>
      text("String")
    case AST.NaturalType(_) =>
      text("Natural")
    case AST.IntegerType(_) =>
      text("Integer")
    case AST.ListType(element, _) =>
      text("List") <> brackets(element.toDoc)
    case AST.Pi(telescopes, resultTy, effects, _) =>
      val telescopeDocs = telescopes.map { tel =>
        val bracket = if tel.implicitness == Implicitness.Implicit then (brackets, brackets) else (parens, parens)
        val paramsDoc = hsep(
          tel.params.map(p =>
            text(p.name) <> text(":") <+> p.ty.toDoc <>
              p.default.map(d => text(" = ") <> d.toDoc).getOrElse(empty)
          ),
          `,` <+> empty
        )
        bracket._1(paramsDoc)
      }
      val effDoc =
        if effects.isEmpty then empty
        else text(" / ") <> brackets(hsep(effects.map(text(_)), `,` <+> empty))
      hsep(telescopeDocs, empty) <+> text("->") <+> resultTy.toDoc <> effDoc
    case AST.Lam(telescopes, body, _) =>
      val telescopeDocs = telescopes.map { tel =>
        val bracket = if tel.implicitness == Implicitness.Implicit then (brackets, brackets) else (parens, parens)
        val paramsDoc = hsep(
          tel.params.map(p =>
            text(p.name) <> text(":") <+> p.ty.toDoc <>
              p.default.map(d => text(" = ") <> d.toDoc).getOrElse(empty)
          ),
          `,` <+> empty
        )
        bracket._1(paramsDoc)
      }
      text("λ") <> hsep(telescopeDocs, empty) <+> text("=>") <+> body.toDoc
    case AST.App(func, args, implicitArgs, _) =>
      val bracket = if implicitArgs then (brackets, brackets) else (parens, parens)
      val argsDoc = hsep(args.map(_.value.toDoc), `,` <+> empty)
      func.toDoc <> bracket._1(argsDoc)
    case AST.Let(id, name, ty, value, body, _) =>
      val tyDoc = ty.map(t => text(":") <+> t.toDoc).getOrElse(empty)
      text("let") <+> text(name) <+> tyDoc <+> text("=") <+> value.toDoc <+> text("in") <@@> body.toDoc.indented()
    case AST.Def(id, name, telescopes, resultTy, body, _) =>
      val telescopeDocs = telescopes.map { tel =>
        val bracket = if tel.implicitness == Implicitness.Implicit then (brackets, brackets) else (parens, parens)
        val paramsDoc = hsep(tel.params.map(p => text(p.name) <> text(":") <+> p.ty.toDoc), `,` <+> empty)
        bracket._1(paramsDoc)
      }
      val tyDoc = resultTy.map(t => text(":") <+> t.toDoc).getOrElse(empty)
      text("def") <+> text(name) <> hsep(telescopeDocs, empty) <+> tyDoc <+> text("=") <+> body.toDoc
    case AST.Ann(expr, ty, _) =>
      parens(expr.toDoc <+> text(":") <+> ty.toDoc)
    case AST.MetaCell(cell, _) =>
      text("?") <> text(cell.toString)

  def collectUniqids(collector: UniqidCollector): Unit = this match
    case AST.Package(_, body, _) =>
      body.collectUniqids(collector)
    case AST.Ref(id, _, _) =>
      collector(id)
    case AST.Tuple(elements, _) =>
      elements.foreach(_.collectUniqids(collector))
    case AST.ListLit(elements, _) =>
      elements.foreach(_.collectUniqids(collector))
    case AST.Block(elements, tail, _) =>
      elements.foreach(_.collectUniqids(collector))
      tail.collectUniqids(collector)
    case AST.StringLit(_, _) =>
      ()
    case AST.IntLit(_, _) =>
      ()
    case AST.Type(level, _) =>
      level.collectUniqids(collector)
    case AST.TypeOmega(level, _) =>
      level.collectUniqids(collector)
    case AST.AnyType(_) =>
      ()
    case AST.StringType(_) =>
      ()
    case AST.NaturalType(_) =>
      ()
    case AST.IntegerType(_) =>
      ()
    case AST.ListType(element, _) =>
      element.collectUniqids(collector)
    case AST.Pi(telescopes, resultTy, _, _) =>
      telescopes.foreach(_.collectUniqids(collector))
      resultTy.collectUniqids(collector)
    case AST.Lam(telescopes, body, _) =>
      telescopes.foreach(_.collectUniqids(collector))
      body.collectUniqids(collector)
    case AST.App(func, args, _, _) =>
      func.collectUniqids(collector)
      args.foreach(_.collectUniqids(collector))
    case AST.Let(id, _, ty, value, body, _) =>
      collector(id)
      ty.foreach(_.collectUniqids(collector))
      value.collectUniqids(collector)
      body.collectUniqids(collector)
    case AST.Def(id, _, telescopes, resultTy, body, _) =>
      collector(id)
      telescopes.foreach(_.collectUniqids(collector))
      resultTy.foreach(_.collectUniqids(collector))
      body.collectUniqids(collector)
    case AST.Ann(expr, ty, _) =>
      expr.collectUniqids(collector)
      ty.collectUniqids(collector)
    case AST.MetaCell(_, _) =>
      ()

  def mapUniqids(mapper: UniqidReplacer): AST = this match
    case AST.Package(name, body, span) =>
      AST.Package(name, body.mapUniqids(mapper), span)
    case AST.Ref(id, name, span) =>
      AST.Ref(mapper(id), name, span)
    case AST.Tuple(elements, span) =>
      AST.Tuple(elements.map(_.mapUniqids(mapper)), span)
    case AST.ListLit(elements, span) =>
      AST.ListLit(elements.map(_.mapUniqids(mapper)), span)
    case AST.Block(elements, tail, span) =>
      AST.Block(elements.map(_.mapUniqids(mapper)), tail.mapUniqids(mapper), span)
    case AST.StringLit(value, span) =>
      AST.StringLit(value, span)
    case AST.IntLit(value, span) =>
      AST.IntLit(value, span)
    case AST.Type(level, span) =>
      AST.Type(level.mapUniqids(mapper), span)
    case AST.TypeOmega(level, span) =>
      AST.TypeOmega(level.mapUniqids(mapper), span)
    case AST.AnyType(span) =>
      AST.AnyType(span)
    case AST.StringType(span) =>
      AST.StringType(span)
    case AST.NaturalType(span) =>
      AST.NaturalType(span)
    case AST.IntegerType(span) =>
      AST.IntegerType(span)
    case AST.ListType(element, span) =>
      AST.ListType(element.mapUniqids(mapper), span)
    case AST.Pi(telescopes, resultTy, effects, span) =>
      AST.Pi(
        telescopes.map(_.mapUniqids(mapper)),
        resultTy.mapUniqids(mapper),
        effects,
        span
      )
    case AST.Lam(telescopes, body, span) =>
      AST.Lam(
        telescopes.map(_.mapUniqids(mapper)),
        body.mapUniqids(mapper),
        span
      )
    case AST.App(func, args, implicitArgs, span) =>
      AST.App(
        func.mapUniqids(mapper),
        args.map(_.mapUniqids(mapper)),
        implicitArgs,
        span
      )
    case AST.Let(id, name, ty, value, body, span) =>
      AST.Let(mapper(id), name, ty.map(_.mapUniqids(mapper)), value.mapUniqids(mapper), body.mapUniqids(mapper), span)
    case AST.Def(id, name, telescopes, resultTy, body, span) =>
      AST.Def(mapper(id), name, telescopes.map(_.mapUniqids(mapper)), resultTy.map(_.mapUniqids(mapper)), body.mapUniqids(mapper), span)
    case AST.Ann(expr, ty, span) =>
      AST.Ann(expr.mapUniqids(mapper), ty.mapUniqids(mapper), span)
    case AST.MetaCell(cell, span) =>
      AST.MetaCell(cell, span)
