package chester.core

import scala.language.experimental.genericNumberLiterals
import chester.error.Span
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*

/** Simple Concrete Syntax Tree using Scala 3 enum */
enum CST(val span: Span) extends ToDoc derives ReadWriter:
  /** A symbol reference */
  case Symbol(name: String, override val span: Span) extends CST(span)
  
  // (a,b)
  case Tuple(elements: Vector[CST], override val span: Span) extends CST(span)
  // [a,b]
  case ListLiteral(elements: Vector[CST], override val span: Span) extends CST(span)

  case StringLiteral(value: String, override val span: Span) extends CST(span)
  case IntegerLiteral(value: BigInt, override val span: Span) extends CST(span)

// a b
  case SeqOf(elements: Vector[CST], override val span: Span) extends CST(span)
  
  def toDoc(using options: DocConf): Doc = this match
    case CST.Symbol(name, _) =>
      text(name)
    case CST.Tuple(elements, _) =>
      parens(hsep(elements.map(_.toDoc), `,`))
    case CST.ListLiteral(elements, _) =>
      brackets(hsep(elements.map(_.toDoc), `,`))
    case CST.StringLiteral(value, _) =>
      text("\"") <> text(value) <> text("\"")
    case CST.IntegerLiteral(value, _) =>
      text(value.toString)
    case CST.SeqOf(elements, _) =>
      hsep(elements.map(_.toDoc), text(" "))
