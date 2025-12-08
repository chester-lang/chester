package chester.core

import chester.error.{Span, SpanOptional}
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*
import cats.data.NonEmptyVector
import chester.utils.{*, given}

import scala.language.experimental.genericNumberLiterals

enum CST(val span: Option[Span]) extends ToDoc with SpanOptional derives ReadWriter:
  case Symbol(name: String, override val span: Option[Span]) extends CST(span)
  case Tuple(elements: Vector[CST], override val span: Option[Span]) extends CST(span)
  case ListLiteral(elements: Vector[CST], override val span: Option[Span]) extends CST(span)
  case Block(elements: Vector[CST], tail: Option[CST], override val span: Option[Span]) extends CST(span)
  case StringLiteral(value: String, override val span: Option[Span]) extends CST(span)
  case IntegerLiteral(value: BigInt, override val span: Option[Span]) extends CST(span)
  case SeqOf(elements: NonEmptyVector[CST], override val span: Option[Span]) extends CST(span)

  def toDoc(using options: DocConf): Doc = this match
    case CST.Symbol(name, _) =>
      text(name)
    case CST.Tuple(elements, _) =>
      parens(hsep(elements.map(_.toDoc), `,`))
    case CST.ListLiteral(elements, _) =>
      brackets(hsep(elements.map(_.toDoc), `,`))
    case CST.Block(elements, tail, _) =>
      val elemDocs = elements.map(e => e.toDoc <> text(";"))
      val allDocs = tail match
        case Some(t) => elemDocs :+ t.toDoc
        case None    => elemDocs
      braces(hsep(allDocs, text(" ")))
    case CST.StringLiteral(value, _) =>
      text("\"") <> text(value) <> text("\"")
    case CST.IntegerLiteral(value, _) =>
      text(value.toString)
    case CST.SeqOf(elements, _) =>
      hsep(elements.toVector.map(_.toDoc), text(" "))
