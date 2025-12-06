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
    def current: Option[Token] = if (position < tokens.length) Some(tokens(position)) else None
    def advance(): Unit = if (hasNext) position += 1
    def getRest: Seq[Token] = tokens.drop(position)

    def skipTrivia(): Unit =
      while (hasNext && current.exists(t => t.isWhitespace || t.isComment)) advance()

    def recordError(message: String, span: Span): Unit =
      reporter.report(ParseError(message, Some(span)))

    // Helpers for common checks
    def is(check: Token => Boolean): Boolean = current.exists(check)
    def isEOF: Boolean = is(_.isInstanceOf[Token.EOF]) || !hasNext
    def isSemicolon: Boolean = is(_.isInstanceOf[Token.Semicolon])
    def shouldStopInBlock: Boolean = is {
      case _: Token.Semicolon | _: Token.RBrace | _: Token.RParen | _: Token.EOF => true
      case _                                                                     => false
    }
  }

  /** Parse a complete file as a block (without braces). All tokens must be consumed; any remaining tokens result in a parse error. Returns a Block
    * CST where:
    *   - elements: statements terminated by semicolons
    *   - tail: final expression (no semicolon after it)
    */
  def parseFile(tokens: Seq[Token])(using reporter: Reporter[ParseError]): CST = {
    val state = new ParserState(tokens, reporter)
    state.skipTrivia()

    // Handle empty file
    if (!state.hasNext) {
      val dummySpan =
        if (tokens.nonEmpty) Some(tokens.last.span)
        else Some(Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero)))
      return CST.Block(Vector.empty, None, dummySpan)
    }

    val startSpan = state.current.get.span
    val elements = ArrayBuffer.empty[CST]
    var tail: Option[CST] = None

    // Parse statements like a block (without braces)
    while (state.hasNext && !state.isEOF)
      if (state.isSemicolon) {
        state.advance() // Skip empty statement
        state.skipTrivia()
      } else {
        val statement = parseAtomSequence(state)
        state.skipTrivia()

        if (state.isSemicolon) {
          elements += statement
          state.advance()
          state.skipTrivia()
        } else if (state.isEOF) {
          // Last statement with no semicolon becomes tail
          tail = Some(statement)
        } else {
          // Unexpected token - recover by treating as statement and continuing
          state.recordError(s"Expected ';' or end of file, got: ${state.current.get.tokenType}", state.current.get.span)
          elements += statement
          // Skip the problematic token to continue parsing
          state.advance()
          state.skipTrivia()
        }
      }

    // Ensure all tokens consumed
    if (!state.isEOF && state.hasNext) {
      state.recordError(s"Unexpected token at end of file: ${state.current.get.tokenType}", state.current.get.span)
    }

    val endSpan = tail.flatMap(_.span).orElse(elements.lastOption.flatMap(_.span)).getOrElse(startSpan)
    CST.Block(elements.toVector, tail, Some(startSpan.combine(endSpan)))
  }

  def parse(tokens: Seq[Token])(using reporter: Reporter[ParseError]): ParseResult = {
    val state = new ParserState(tokens, reporter)
    state.skipTrivia()

    if (!state.hasNext) {
      // Return a dummy symbol for error recovery since SeqOf requires at least one element
      val dummySpan =
        if (tokens.nonEmpty) Some(tokens.last.span)
        else Some(Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero)))
      return ParseResult(CST.Symbol("<empty>", dummySpan), state.getRest)
    }

    val elements = scala.collection.mutable.ArrayBuffer.empty[CST]
    val startSpan = state.current.get.span

    while (state.hasNext) {
      elements += parseAtom(state)
      state.skipTrivia()
    }

    val cst = if (elements.length == 1) elements(0) else combineAtoms(elements.toSeq)
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
            CST.StringLiteral(value.asString, Some(span))
          case Token.IntegerLiteral(value, span) =>
            state.advance()
            CST.IntegerLiteral(BigInt(value.asString), Some(span))
          case Token.SymbolLiteral(value, span) =>
            state.advance()
            CST.Symbol(value.asString, Some(span))
          case Token.Identifier(parts, span) =>
            state.advance()
            CST.Symbol(parts.map(_.text).mkString, Some(span))
          case _ =>
            // Error recovery: skip unexpected token and continue
            state.recordError(s"Unexpected token: ${token.tokenType}", token.span)
            state.advance()
            // Try to parse next atom
            if (state.hasNext) parseAtom(state)
            else CST.Symbol("<error>", Some(token.span))
        }
      case None =>
        val lastSpan =
          if (state.tokens.nonEmpty) state.tokens.last.span
          else {
            Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero))
          }
        state.recordError("Unexpected end of input", lastSpan)
        CST.Symbol("<error>", Some(lastSpan))
    }
  }

  /** Parse a delimited sequence of elements (tuple or list) */
  private def parseDelimited(
      state: ParserState,
      start: Span,
      isClosing: Token => Boolean,
      closingName: String,
      build: (Vector[CST], Option[Span]) => CST
  ): CST = {
    val elements = ArrayBuffer.empty[CST]

    def closeWithError(reason: String): CST = {
      state.recordError(s"Expected $closingName", start)
      val endSpan = if (elements.nonEmpty) elements.last.span.getOrElse(start) else start
      build(elements.toVector, Some(start.combine(endSpan)))
    }

    while (state.hasNext && !state.is(isClosing)) {
      elements += parseAtom(state)
      state.skipTrivia()

      state.current match {
        case Some(_: Token.Comma) =>
          state.advance()
          state.skipTrivia()
          // Allow trailing comma: if we see closing delimiter after comma, exit loop
          if (state.is(isClosing)) {
            // Exit loop, closing will be handled after
          }
        case Some(tok) if isClosing(tok) => // Done, will be handled after loop
        case _ if state.isEOF            => return closeWithError("EOF")
        case Some(_: Token.Semicolon)    => return closeWithError("semicolon in block context")
        case Some(other) =>
          state.recordError(s"Expected ',' or $closingName but got ${other.tokenType}", other.span)
          state.advance()
          state.skipTrivia()
        case None => return closeWithError("unexpected end")
      }
    }

    state.current match {
      case Some(tok) if isClosing(tok) =>
        state.advance()
        build(elements.toVector, Some(start.combine(tok.span)))
      case _ => closeWithError("missing delimiter")
    }
  }

  private def parseTuple(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance()
    state.skipTrivia()
    parseDelimited(state, start, _.isInstanceOf[Token.RParen], "')' to close tuple", CST.Tuple.apply)
  }

  private def parseListLiteral(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance()
    state.skipTrivia()
    parseDelimited(state, start, _.isInstanceOf[Token.RBracket], "']' to close list", CST.ListLiteral.apply)
  }

  /** Combine atoms into single CST - SeqOf if multiple, single atom if one */
  private def combineAtoms(atoms: Seq[CST]): CST =
    if (atoms.length == 1) atoms(0)
    else
      CST.SeqOf(
        NonEmptyVector.fromVectorUnsafe(atoms.toVector),
        for {
          h <- atoms.head.span
          l <- atoms.last.span
        } yield h.combine(l)
      )

  /** Parse multiple atoms until hitting a block stop token */
  private def parseAtomSequence(state: ParserState): CST = {
    val atoms = ArrayBuffer.empty[CST]
    while (state.hasNext && !state.shouldStopInBlock) {
      atoms += parseAtom(state)
      state.skipTrivia()
    }

    if (atoms.isEmpty) {
      val span = state.current.map(_.span).orElse(Some(Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero))))
      CST.Symbol("<error>", span)
    } else combineAtoms(atoms.toSeq)
  }

  private def parseBlock(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance()
    state.skipTrivia()

    val elements = ArrayBuffer.empty[CST]
    var tail: Option[CST] = None
    val isRBrace = (tok: Token) => tok.isInstanceOf[Token.RBrace]
    val isRParen = (tok: Token) => tok.isInstanceOf[Token.RParen]

    def closeWithError(): CST = {
      state.recordError("Expected '}' to close block", start)
      val endSpan = tail.flatMap(_.span).orElse(elements.lastOption.flatMap(_.span)).getOrElse(start)
      CST.Block(elements.toVector, tail, Some(start.combine(endSpan)))
    }

    while (state.hasNext && !state.is(isRBrace) && !state.is(isRParen))
      if (state.isSemicolon) {
        state.advance() // Skip empty statement
        state.skipTrivia()
      } else {
        val statement = parseAtomSequence(state)

        if (state.isSemicolon) {
          elements += statement
          state.advance()
          state.skipTrivia()
        } else if (state.is(isRBrace) || state.is(isRParen)) {
          tail = Some(statement)
        } else if (state.isEOF) {
          tail = Some(statement)
          return closeWithError()
        } else {
          state.recordError(s"Unexpected token: ${state.current.get.tokenType}", state.current.get.span)
          elements += statement
          state.advance()
          state.skipTrivia()
        }
      }

    state.current match {
      case Some(tok @ Token.RBrace(_)) =>
        state.advance()
        CST.Block(elements.toVector, tail, Some(start.combine(tok.span)))
      case Some(Token.RParen(_)) => closeWithError() // Wrong closing delimiter
      case _                     => closeWithError() // Missing closing delimiter
    }
  }
}
