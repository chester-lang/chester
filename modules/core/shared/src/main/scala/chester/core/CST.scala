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
  
  /** Function application */
  case Apply(function: CST, arguments: List[CST], override val span: Span) extends CST(span)
  
  def toDoc(using options: DocConf): Doc = this match
    case CST.Symbol(name, _) =>
      text(name)
    case CST.Apply(function, arguments, _) =>
      function.toDoc <> parens(hsep(arguments.map(_.toDoc), `,`))
