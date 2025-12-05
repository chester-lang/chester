package chester.reader

import chester.core.CST
import chester.error.{Span, SpanInFile, Pos, Reporter}
import scala.language.experimental.genericNumberLiterals
import cats.data.NonEmptyVector

import scala.collection.mutable.ArrayBuffer

object Parser {

  case class ParseResult(cst: CST, rest: Seq[Token])

  private class ParserState(val tokens: Seq[Token], val reporter: Reporter[ParseError]) {
    private var position = 0

    def hasNext: Boolean = position < tokens.length && !current.exists(_.isInstanceOf[Token.EOF])

    def current: Option[Token] =
      if (position < tokens.length) Some(tokens(position)) else None

    def peek(offset: Int = 1): Option[Token] = {
      val idx = position + offset
      if (idx >= 0 && idx < tokens.length) Some(tokens(idx)) else None
    }

    def advance(): Unit = if (hasNext) position += 1

    def currentPos: Int = position

    def getRest: Seq[Token] = tokens.drop(position)

    // Skip whitespace and comments
    def skipTrivia(): Unit =
      while (hasNext && current.exists(t => t.isWhitespace || t.isComment))
        advance()

    def recordError(message: String, span: Span): Unit =
      reporter.report(ParseError(message, Some(span)))

    def recordError(message: String, spanOpt: Option[Span]): Unit =
      reporter.report(ParseError(message, spanOpt))
  }

  def parse(tokens: Seq[Token])(using reporter: Reporter[ParseError]): ParseResult = {
    val state = new ParserState(tokens, reporter)
    state.skipTrivia()

    if (!state.hasNext) {
      state.recordError("No tokens to parse", None)
      // Return a dummy symbol for error recovery since SeqOf requires at least one element
      val dummySpan =
        if (tokens.nonEmpty) tokens.last.span
        else
          Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero))
      return ParseResult(CST.Symbol("<empty>", dummySpan), state.getRest)
    }

    val elements = scala.collection.mutable.ArrayBuffer.empty[CST]
    val startSpan = state.current.get.span

    while (state.hasNext) {
      elements += parseAtom(state)
      state.skipTrivia()
    }

    val cst =
      if (elements.length == 1) elements(0)
      else {
        val endSpan = if (elements.nonEmpty) elements.last.span else startSpan
        CST.SeqOf(NonEmptyVector.fromVectorUnsafe(elements.toVector), startSpan.combine(endSpan))
      }
    ParseResult(cst, state.getRest)
  }

  private def parseAtom(state: ParserState): CST = {
    state.skipTrivia()

    state.current match {
      case Some(token) =>
        token match {
          case Token.LParen(span)   => parseTuple(state)
          case Token.LBracket(span) => parseListLiteral(state)
          case Token.LBrace(span)   => parseBlock(state)
          case Token.StringLiteral(value, span) =>
            state.advance()
            CST.StringLiteral(value.asString, span)
          case Token.IntegerLiteral(value, span) =>
            state.advance()
            CST.IntegerLiteral(BigInt(value.asString), span)
          case Token.SymbolLiteral(value, span) =>
            state.advance()
            CST.Symbol(value.asString, span)
          case Token.Identifier(parts, span) =>
            state.advance()
            CST.Symbol(parts.map(_.text).mkString, span)
          case _ =>
            // Error recovery: skip unexpected token and continue
            state.recordError(s"Unexpected token: ${token.tokenType}", token.span)
            state.advance()
            // Try to parse next atom
            if (state.hasNext) parseAtom(state)
            else CST.Symbol("<error>", token.span)
        }
      case None =>
        val lastSpan =
          if (state.tokens.nonEmpty) state.tokens.last.span
          else {
            Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero))
          }
        state.recordError("Unexpected end of input", Some(lastSpan))
        CST.Symbol("<error>", lastSpan)
    }
  }

  private def parseTuple(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance() // Skip LParen
    state.skipTrivia()

    val elements = ArrayBuffer.empty[CST]

    while (state.hasNext && !state.current.exists(_.isInstanceOf[Token.RParen])) {
      elements += parseAtom(state)
      state.skipTrivia()

      state.current match {
        case Some(Token.Comma(_)) =>
          state.advance()
          state.skipTrivia()
        case Some(Token.RParen(_)) =>
        // End of tuple
        case Some(Token.EOF(_)) | None =>
          // Error recovery: missing closing paren
          state.recordError("Expected ')' to close tuple", Some(start))
          val endSpan = if (elements.nonEmpty) elements.last.span else start
          val result = CST.Tuple(elements.toVector, start.combine(endSpan))
          return result
        case Some(other) =>
          // Error recovery: skip unexpected token
          state.recordError(s"Expected ',' or ')' but got ${other.tokenType}", other.span)
          state.advance()
          state.skipTrivia()
      }
    }

    state.current match {
      case Some(Token.RParen(endSpan)) =>
        state.advance()
        CST.Tuple(elements.toVector, start.combine(endSpan))
      case _ =>
        // Error recovery: missing closing paren
        state.recordError("Expected ')' to close tuple", Some(start))
        val endSpan = if (elements.nonEmpty) elements.last.span else start
        CST.Tuple(elements.toVector, start.combine(endSpan))
    }
  }

  private def parseListLiteral(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance() // Skip LBracket
    state.skipTrivia()

    val elements = ArrayBuffer.empty[CST]

    while (state.hasNext && !state.current.exists(_.isInstanceOf[Token.RBracket])) {
      elements += parseAtom(state)
      state.skipTrivia()

      state.current match {
        case Some(Token.Comma(_)) =>
          state.advance()
          state.skipTrivia()
        case Some(Token.RBracket(_)) =>
        // End of list
        case Some(Token.EOF(_)) | None =>
          // Error recovery: missing closing bracket - treat [a as [a]
          state.recordError("Expected ']' to close list", Some(start))
          val endSpan = if (elements.nonEmpty) elements.last.span else start
          val result = CST.ListLiteral(elements.toVector, start.combine(endSpan))
          return result
        case Some(other) =>
          // Error recovery: skip unexpected token
          state.recordError(s"Expected ',' or ']' but got ${other.tokenType}", other.span)
          state.advance()
          state.skipTrivia()
      }
    }

    state.current match {
      case Some(Token.RBracket(endSpan)) =>
        state.advance()
        CST.ListLiteral(elements.toVector, start.combine(endSpan))
      case _ =>
        // Error recovery: missing closing bracket - treat [a as [a]
        state.recordError("Expected ']' to close list", Some(start))
        val endSpan = if (elements.nonEmpty) elements.last.span else start
        CST.ListLiteral(elements.toVector, start.combine(endSpan))
    }
  }

  private def parseBlock(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance() // Skip LBrace
    state.skipTrivia()

    val elements = ArrayBuffer.empty[CST]
    var tail: Option[CST] = None

    while (state.hasNext && !state.current.exists(_.isInstanceOf[Token.RBrace])) {
      val elem = parseAtom(state)
      state.skipTrivia()

      state.current match {
        case Some(Token.Semicolon(_)) =>
          // Element followed by semicolon
          elements += elem
          state.advance()
          state.skipTrivia()
        case Some(Token.RBrace(_)) =>
          // Final element without semicolon - this is the tail
          tail = Some(elem)
        case Some(Token.EOF(_)) | None =>
          // Error recovery: missing closing brace
          tail = Some(elem)
          state.recordError("Expected '}' to close block", Some(start))
          val endSpan = elem.span
          val result = CST.Block(elements.toVector, tail, start.combine(endSpan))
          return result
        case Some(other) =>
          // Error recovery: missing semicolon, treat as element and continue
          state.recordError(s"Expected ';' or '}}' but got ${other.tokenType}", other.span)
          elements += elem
          state.advance()
          state.skipTrivia()
      }
    }

    state.current match {
      case Some(Token.RBrace(endSpan)) =>
        state.advance()
        CST.Block(elements.toVector, tail, start.combine(endSpan))
      case _ =>
        // Error recovery: missing closing brace
        state.recordError("Expected '}' to close block", Some(start))
        val endSpan = tail.map(_.span).orElse(elements.lastOption.map(_.span)).getOrElse(start)
        CST.Block(elements.toVector, tail, start.combine(endSpan))
    }
  }
}
