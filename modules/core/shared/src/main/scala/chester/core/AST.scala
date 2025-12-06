package chester.core

import scala.language.experimental.genericNumberLiterals
import chester.error.Span
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*
import cats.data.NonEmptyVector
import chester.utils.{*, given}

enum AST(val span: Span) extends ToDoc derives ReadWriter:
  case Ref(name: String, override val span: Span) extends AST(span)
  case Tuple(elements: Vector[AST], override val span: Span) extends AST(span)
  case ListLit(elements: Vector[AST], override val span: Span) extends AST(span)
  case Block(elements: Vector[AST], override val span: Span) extends AST(span)
  case StringLit(value: String, override val span: Span) extends AST(span)
  case IntLit(value: BigInt, override val span: Span) extends AST(span)
  case App(func: AST, args: Vector[AST], override val span: Span) extends AST(span)
  case Lam(params: Vector[String], body: AST, override val span: Span) extends AST(span)
  case Let(name: String, value: AST, body: AST, override val span: Span) extends AST(span)

  def toDoc(using options: DocConf): Doc = this match
    case AST.Ref(name, _) =>
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
      text("λ") <> parens(hsep(params.map(p => text(p)), `,` <+> empty)) <+> text("=>") <+> body.toDoc
    case AST.Let(name, value, body, _) =>
      text("let") <+> text(name) <+> text("=") <+> value.toDoc <+> text("in") <@@> body.toDoc.indented()
