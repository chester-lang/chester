package chester.reader

import scala.language.experimental.genericNumberLiterals

import chester.error.*
import chester.i18n.*
import chester.utils.codepointToString
import chester.utils.lenIsOne

type Text = Vector[StringChar]

case class StringChar(text: String, span: Span) {
  require(
    text.lenIsOne,
    "StringChar must represent a single UTF-16 character or a valid surrogate pair in UTF-16"
  )
}

object StringChar {
  def apply(codePoint: Int, span: Span): StringChar = {
    new StringChar(codepointToString(codePoint), span)
  }

  extension (text: Text) {
    def asString: String = text.map(_.text).mkString
  }
}

enum Token extends SpanRequired {
  case LParen(span: Span)
  case RParen(span: Span)
  case LBracket(span: Span)
  case RBracket(span: Span)
  case LBrace(span: Span)
  case RBrace(span: Span)
  case Comma(span: Span)
  case Semicolon(span: Span)
  case Dot(span: Span)
  case At(span: Span)
  case EOF(span: Span)
  case Whitespace(span: Span, hasNewline: Boolean = false)
  case Comment(text: Text, span: Span)
  case IntegerLiteral(value: Text, span: Span)
  case RationalLiteral(value: Text, span: Span)
  case StringLiteral(value: Text, span: Span)
  case SymbolLiteral(value: Text, span: Span)
  case Identifier(parts: Text, span: Span)
  case Hash(span: Span)

  def isWhitespace: Boolean = this match {
    case Whitespace(_, _) => true
    case _                => false
  }

  def isComment: Boolean = this match {
    case Comment(_, _) => true
    case _             => false
  }

  def containsNewline: Boolean = this match {
    case Whitespace(_, hasNewline) => hasNewline
    case _                         => false
  }

  def tokenType: String = this match {
    case LParen(_)             => t"left parenthesis '('"
    case RParen(_)             => t"right parenthesis ')'"
    case LBracket(_)           => t"left bracket '['"
    case RBracket(_)           => t"right bracket ']"
    case LBrace(_)             => t"left brace '{'"
    case RBrace(_)             => t"right brace '}'"
    case Comma(_)              => t"comma ','"
    case Semicolon(_)          => t"semicolon ';'"
    case Dot(_)                => t"dot '.'"
    case At(_)                 => t"at '@'"
    case EOF(_)                => t"end of file"
    case Whitespace(_, _)      => t"whitespace"
    case Comment(_, _)         => t"comment"
    case IntegerLiteral(_, _)  => t"integer literal"
    case RationalLiteral(_, _) => t"rational literal"
    case StringLiteral(_, _)   => t"string literal"
    case SymbolLiteral(_, _)   => t"symbol literal"
    case Identifier(_, _)      => t"identifier"
    case Hash(_)               => t"hash '#' "
  }
}

object Token {
  extension (token: Token.Identifier) {

    /** Returns the text representation of this identifier */
    def toStr: String = token.parts.map(_.text).mkString

    /** Returns true if this identifier represents an operator according to the language rules */
    def isOperator: Boolean = {
      if (token.parts.isEmpty) return false
      chester.syntax.IdentifierRules.strIsOperator(toStr)
    }

    /** Returns the text representation of this identifier */
    def text: String = toStr
  }
}
