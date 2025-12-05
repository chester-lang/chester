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
    
    // Helper to check if current token is a closing delimiter
    def isClosing(token: Token): Boolean = token match {
      case _: Token.RParen | _: Token.RBracket | _: Token.RBrace => true
      case _ => false
    }
    
    // Helper to check if we should stop parsing in certain contexts
    def shouldStopInBlock: Boolean = current.exists {
      case _: Token.Semicolon | _: Token.RBrace | _: Token.RParen | _: Token.EOF => true
      case _ => false
    }
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

  /** Parse a delimited sequence of elements (tuple or list) */
  private def parseDelimited(
    state: ParserState,
    start: Span,
    closingDelimiter: Token => Boolean,
    separator: Token.Comma.type,
    closingName: String,
    build: (Vector[CST], Span) => CST
  ): CST = {
    val elements = ArrayBuffer.empty[CST]

    while (state.hasNext && !state.current.exists(closingDelimiter)) {
      elements += parseAtom(state)
      state.skipTrivia()

      state.current match {
        case Some(_: Token.Comma) =>
          state.advance()
          state.skipTrivia()
        case Some(tok) if closingDelimiter(tok) =>
          // End of delimited sequence
        case Some(Token.EOF(_)) | None =>
          // Error recovery: missing closing delimiter
          state.recordError(s"Expected $closingName", Some(start))
          val endSpan = if (elements.nonEmpty) elements.last.span else start
          return build(elements.toVector, start.combine(endSpan))
        case Some(_: Token.Semicolon) =>
          // Error recovery: semicolon ends this structure in block context
          state.recordError(s"Expected $closingName", Some(start))
          val endSpan = if (elements.nonEmpty) elements.last.span else start
          return build(elements.toVector, start.combine(endSpan))
        case Some(other) =>
          // Error recovery: skip unexpected token
          state.recordError(s"Expected ',' or $closingName but got ${other.tokenType}", other.span)
          state.advance()
          state.skipTrivia()
      }
    }

    state.current match {
      case Some(tok) if closingDelimiter(tok) =>
        val endSpan = tok.span
        state.advance()
        build(elements.toVector, start.combine(endSpan))
      case _ =>
        // Error recovery: missing closing delimiter
        state.recordError(s"Expected $closingName", Some(start))
        val endSpan = if (elements.nonEmpty) elements.last.span else start
        build(elements.toVector, start.combine(endSpan))
    }
  }

  private def parseTuple(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance() // Skip LParen
    state.skipTrivia()
    parseDelimited(
      state, start,
      _.isInstanceOf[Token.RParen],
      Token.Comma,
      "')' to close tuple",
      (elems, span) => CST.Tuple(elems, span)
    )
  }

  private def parseListLiteral(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance() // Skip LBracket
    state.skipTrivia()
    parseDelimited(
      state, start,
      _.isInstanceOf[Token.RBracket],
      Token.Comma,
      "']' to close list",
      (elems, span) => CST.ListLiteral(elems, span)
    )
  }

  /** Parse multiple atoms into a single CST, creating SeqOf if needed */
  private def parseAtomSequence(state: ParserState): CST = {
    val atoms = ArrayBuffer.empty[CST]
    
    while (state.hasNext && !state.shouldStopInBlock) {
      atoms += parseAtom(state)
      state.skipTrivia()
    }
    
    if (atoms.isEmpty) {
      // Should not happen in normal flow
      val span = state.current.map(_.span).getOrElse(
        Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero))
      )
      CST.Symbol("<error>", span)
    } else if (atoms.length == 1) {
      atoms(0)
    } else {
      val startSpan = atoms(0).span
      val endSpan = atoms.last.span
      CST.SeqOf(NonEmptyVector.fromVectorUnsafe(atoms.toVector), startSpan.combine(endSpan))
    }
  }

  private def parseBlock(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance() // Skip LBrace
    state.skipTrivia()

    val elements = ArrayBuffer.empty[CST]
    var tail: Option[CST] = None
    val isBlockClosed = (tok: Token) => tok.isInstanceOf[Token.RBrace]
    val wrongClosing = (tok: Token) => tok.isInstanceOf[Token.RParen]

    // Parse statements separated by semicolons
    while (state.hasNext && !state.current.exists(isBlockClosed) && !state.current.exists(wrongClosing)) {
      // Skip empty statements (multiple semicolons)
      if (state.current.exists(_.isInstanceOf[Token.Semicolon])) {
        state.advance()
        state.skipTrivia()
        // Continue to next iteration
      } else {
        val statement = parseAtomSequence(state)
        
        state.current match {
          case Some(_: Token.Semicolon) =>
            // Statement followed by semicolon - add to elements
            elements += statement
            state.advance()
            state.skipTrivia()
          case Some(tok) if isBlockClosed(tok) || wrongClosing(tok) =>
            // Final statement without semicolon - this is the tail
            tail = Some(statement)
          case Some(Token.EOF(_)) | None =>
            // Error recovery: missing closing brace
            tail = Some(statement)
            state.recordError("Expected '}' to close block", Some(start))
            val endSpan = statement.span
            return CST.Block(elements.toVector, tail, start.combine(endSpan))
          case Some(other) =>
            // Shouldn't reach here - error in logic
            state.recordError(s"Unexpected token: ${other.tokenType}", other.span)
            elements += statement
            state.advance()
            state.skipTrivia()
        }
      }
    }

    // Handle closing delimiter
    state.current match {
      case Some(Token.RBrace(endSpan)) =>
        state.advance()
        CST.Block(elements.toVector, tail, start.combine(endSpan))
      case Some(Token.RParen(_)) =>
        // Error recovery: block closed with ) instead of }
        state.recordError("Expected '}' to close block", Some(start))
        val endSpan = tail.map(_.span).orElse(elements.lastOption.map(_.span)).getOrElse(start)
        CST.Block(elements.toVector, tail, start.combine(endSpan))
      case _ =>
        // Error recovery: missing closing brace
        state.recordError("Expected '}' to close block", Some(start))
        val endSpan = tail.map(_.span).orElse(elements.lastOption.map(_.span)).getOrElse(start)
        CST.Block(elements.toVector, tail, start.combine(endSpan))
    }
  }
}
