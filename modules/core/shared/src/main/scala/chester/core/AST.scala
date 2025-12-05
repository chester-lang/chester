package chester.core

import scala.language.experimental.genericNumberLiterals
import chester.error.Span
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*

/** Simple AST using Scala 3 enum */
enum AST(val span: Span) extends ToDoc derives ReadWriter:
  /** A symbol reference */
  case Symbol(name: String, override val span: Span) extends AST(span)
  
  /** Function application */
  case Apply(function: AST, arguments: List[AST], override val span: Span) extends AST(span)
  
  def toDoc(using options: DocConf): Doc = this match
    case AST.Symbol(name, _) => 
      text(name)
    case AST.Apply(function, arguments, _) =>
      function.toDoc <> parens(hsep(arguments.map(_.toDoc), `,`))
