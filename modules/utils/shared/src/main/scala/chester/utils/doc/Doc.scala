package chester.utils.doc

import scala.annotation.targetName
import scala.language.implicitConversions

import chester.utils.{*, given}
import kiama2.output.*
import kiama2.output.PrettyPrinterTypes.Width
import upickle.default.*

type DocPrinter = ParenPrettyPrinter & StylePrettyPrinter

implicit object StringPrinter extends StringPrettyPrinter with ParenPrettyPrinter with StylePrettyPrinter {}

sealed trait Doc extends ToDoc derives ReadWriter {
  implicit final inline def getDoc(using printer: DocPrinter): printer.Doc =
    printer.toParenDoc(printToExpr)

  def printToExpr(using printer: DocPrinter): printer.Expr

  final inline def toDoc(using DocConf): Doc = this

  def descent(f: Doc => Doc): Doc = this

  def styled(style: Style): Doc = descent(_.styled(style))

  override def toString: String =
    StringPrinter.render(this)(using DocConf.Default)
}

implicit inline def textFrom(inline s: String): Doc = text(s)

inline def text(inline s: String, inline style: Style = Style.Empty): Doc =
  Text(s, style)

case class Text(s: String, style: Style = Style.Empty) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    printer.text(s, style)

  override def styled(style: Style): Doc = copy(style = this.style ++ style)
}

inline def group(inline doc: Doc): Doc = Group(doc)

case class Group(doc: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    printer.group(doc.getDoc)

  override def descent(f: Doc => Doc): Doc = copy(doc = f(doc))
}

val maxWidth = Integer.MAX_VALUE
export PrettyPrinter.defaultWidth

def renderToDocument(doc: Doc, w: Width = maxWidth)(using
    printer: DocPrinter
): printer.Document = printer.pretty(printer.toParenDoc(doc.printToExpr), w)

private def render0(doc: Doc, w: Width = maxWidth)(using
    printer: DocPrinter
): printer.Layout = renderToDocument(doc, w).layout
def render(doc: Doc, w: Width = maxWidth)(using
    printer: DocPrinter
): printer.Layout = render0(doc, w)
def render(
    doc: ToDoc
)(using options: DocConf, printer: DocPrinter): printer.Layout =
  render0(doc.toDoc)
def render(doc: ToDoc, w: Width)(using
    options: DocConf,
    printer: DocPrinter
): printer.Layout = render0(doc.toDoc, w)

/** already use softline to either insert a space or a linebreak after sep, no need to add one */
def mkList(docs: Iterable[ToDoc], begin: ToDoc = empty, end: ToDoc = empty, sep: ToDoc = ",")(using DocConf): Doc = group {
  docs.toList match {
    case Nil =>
      begin.toDoc <> end.toDoc
    case head :: Nil =>
      begin.toDoc <> head.toDoc <> end.toDoc
    case head :: tail =>
      val content = tail.foldLeft(head.toDoc)((acc, doc) => acc <> sep.toDoc </> doc.toDoc)
      begin.toDoc <> content <> end.toDoc
  }
}

/** no softline compared to mkList */
def mkDoc(docs: Iterable[ToDoc], begin: ToDoc = empty, end: ToDoc = empty, sep: ToDoc = ",")(using
    DocConf
): Doc = group {
  docs.toList match {
    case Nil =>
      begin.toDoc <> end.toDoc
    case head :: Nil =>
      begin.toDoc <> head.toDoc <> end.toDoc
    case head :: tail =>
      val content = tail.foldLeft(head.toDoc)((acc, doc) => acc <> sep.toDoc <> doc.toDoc)
      begin.toDoc <> content <> end.toDoc
  }
}

def concat(docs: ToDoc*)(using DocConf): Doc = group {
  docs.foldLeft(Doc.empty)((acc, doc) => acc <> doc.toDoc)
}

@targetName("concatIterable")
def concat(docs: Iterable[ToDoc])(using DocConf): Doc = group {
  docs.foldLeft(Doc.empty)((acc, doc) => acc <> doc.toDoc)
}

val empty = text("")
val hardline = text("\n") // TODO: CRLF?
val line = hardline

object Doc {
  def indented(doc: ToDoc)(using DocConf): Doc = doc.indented()
  def indent(doc: ToDoc)(using DocConf): Doc = doc.indented()

  export chester.utils.doc.{renderToDocument, render, text, group, mkList, mkDoc, empty, concat, hardline, line, sep, link}
}

implicit class DocOps(doc: Doc) extends AnyVal {
  def renderToDocument(w: Width = maxWidth)(using
      printer: DocPrinter
  ): printer.Document = Doc.renderToDocument(doc, w)

  def render(w: Width = maxWidth)(using printer: DocPrinter): printer.Layout =
    Doc.render(doc, w)
}
implicit class DocPrinterOps[T <: DocPrinter](val printer: T) extends AnyVal {
  def render(doc: Doc, maxWidth: Width = maxWidth): printer.Layout =
    doc.render(maxWidth)(using printer)

  def render(doc: ToDoc)(using options: DocConf): printer.Layout =
    Doc.render(doc)(using options, printer)

  def render(doc: ToDoc, maxWidth: Width)(using
      options: DocConf
  ): printer.Layout = Doc.render(doc, maxWidth)(using options, printer)

  def renderToDocument(doc: Doc, maxWidth: Width = maxWidth): printer.Document =
    doc.renderToDocument(maxWidth)(using printer)
}

trait ToDoc extends Any {
  def toDoc(using options: DocConf): Doc
  override def toString: String = StringPrinter.render(this)(using DocConf.Default)
}

case class `$<>`(left: Doc, right: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    left.getDoc <> right.getDoc

  override def descent(f: Doc => Doc): Doc =
    copy(left = f(left), right = f(right))
}

case class `$<+>`(left: Doc, right: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    left.getDoc <+> right.getDoc

  override def descent(f: Doc => Doc): Doc =
    copy(left = f(left), right = f(right))
}

case class `$</>`(left: Doc, right: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    left.getDoc </> right.getDoc

  override def descent(f: Doc => Doc): Doc =
    copy(left = f(left), right = f(right))
}

case class `$<\\>`(left: Doc, right: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    left.getDoc <\> right.getDoc

  override def descent(f: Doc => Doc): Doc =
    copy(left = f(left), right = f(right))
}

case class `$<@>`(left: Doc, right: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    left.getDoc <@> right.getDoc

  override def descent(f: Doc => Doc): Doc =
    copy(left = f(left), right = f(right))
}

case class `$<@@>`(left: Doc, right: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    left.getDoc <@@> right.getDoc

  override def descent(f: Doc => Doc): Doc =
    copy(left = f(left), right = f(right))
}

case class `$<%>`(left: Doc, right: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    left.getDoc <%> right.getDoc

  override def descent(f: Doc => Doc): Doc =
    copy(left = f(left), right = f(right))
}

case class `$<%%>`(left: Doc, right: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    left.getDoc <%%> right.getDoc

  override def descent(f: Doc => Doc): Doc =
    copy(left = f(left), right = f(right))
}

// TODO: add custom indent
case class $indent(doc: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    printer.indent(doc.getDoc)

  override def descent(f: Doc => Doc): Doc = copy(doc = f(doc))
}

case class $hsep(docs: Seq[Doc], sep: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    printer.hsep(docs.map(_.getDoc), sep.getDoc)

  override def descent(f: Doc => Doc): Doc =
    copy(docs = docs.map(f), sep = f(sep))
}

case class $ssep(docs: Seq[Doc], sep: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    printer.ssep(docs.map(_.getDoc), sep.getDoc)

  override def descent(f: Doc => Doc): Doc =
    copy(docs = docs.map(f), sep = f(sep))
}

case class $link(n: HoldOptionNoRead[AnyRef], d: Doc) extends Doc {
  def printToExpr(using printer: DocPrinter): printer.Expr =
    printer.link(n.get, d.getDoc)

  override def descent(f: Doc => Doc): Doc = copy(d = f(d))
}

def hsep(ds: Seq[ToDoc], sep: ToDoc)(using DocConf): Doc = $hsep(ds.map(_.toDoc), sep.toDoc)
def ssep(ds: Seq[ToDoc], sep: ToDoc)(using DocConf): Doc = $ssep(ds.map(_.toDoc), sep.toDoc)
def sep(sep: ToDoc, ds: Seq[ToDoc])(using DocConf): Doc = hsep(ds, sep)
def link(n: AnyRef, d: ToDoc)(using DocConf): Doc = $link(HoldOptionNoRead(Some(n)), d.toDoc)
def link(n: AnyRef, d: Doc): Doc = $link(HoldOptionNoRead(Some(n)), d)
extension (self: ToDoc)(using options: DocConf) {
  implicit inline def asDoc: Doc = self.toDoc

  /** Return the concatenation of this document with the argument.
    */
  def <>(other: ToDoc): Doc = `$<>`(self, other)

  /** Return the concatenation of this document with the argument using a `space` separator.
    */
  def <+>(other: ToDoc): Doc = `$<+>`(self, other)
  def <+?>[A <: ToDoc](tuple: (Boolean, A)): Doc = <+?>[A](tuple._1, tuple._2)
  @targetName("plusQMark")
  def <+?>[A <: ToDoc](tuple: (A => Boolean, A)): Doc =
    <+?>[A](tuple._1, tuple._2)
  def <+?>[A <: ToDoc](pred: Boolean, other: A): Doc =
    if pred then self <+> other else self
  def <+?>[A <: ToDoc](pred: A => Boolean, other: A): Doc =
    if pred(other) then self <+> other else self

  /** Return the concatenation of this document with the argument using a `softline` separator.
    *
    * softline: Return a document that behaves like `space` if the resulting output fits the page, otherwise it behaves like `line`.
    */
  def </>(other: ToDoc): Doc = `$</>`(self, other)

  /** Return the concatenation of this document with the argument using a `softbreak` separator.
    *
    * softbreak: Return a document that behaves like `empty` if the resulting output fits the page, otherwise it behaves like `line`.
    */
  def <\>(other: ToDoc): Doc = `$<\\>`(self, other)

  /** Return the concatenation of this document with the argument using a `line` separator.
    */
  def <@>(other: ToDoc): Doc = `$<@>`(self, other)

  /** Return the concatenation of this document with the argument using a `linebreak` separator.
    */
  def <@@>(other: ToDoc): Doc = `$<@@>`(self, other)

  /** Align the argument below this document using a `line` separator.
    */
  def <%>(other: ToDoc): Doc = `$<%>`(self, other)

  /** Align the argument below this document using a `linebreak` separator.
    */
  def <%%>(other: ToDoc): Doc = `$<%%>`(self, other)
  def end: Doc = self <> hardline
  def <|>(other: ToDoc): Doc = self <> hardline <> other
  // TODO: add custom indent
  def indented(): Doc = $indent(self)
}
