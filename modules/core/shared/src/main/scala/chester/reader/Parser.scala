package chester.reader

import chester.core.CST
import chester.error.{Span, SpanInFile, Pos}
import scala.language.experimental.genericNumberLiterals

import scala.collection.mutable.ArrayBuffer

object Parser {
  
  case class ParseResult(cst: CST, rest: Seq[Token])
  
  private class ParserState(val tokens: Seq[Token]) {
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
    def skipTrivia(): Unit = {
      while (hasNext && current.exists(t => t.isWhitespace || t.isComment)) {
        advance()
      }
    }
  }
  
  def parse(tokens: Seq[Token]): Either[ParseError, ParseResult] = {
    val state = new ParserState(tokens)
    state.skipTrivia()
    
    if (!state.hasNext) {
      return Left(ParseError("No tokens to parse", None))
    }
    
    try {
      val cst = parseExpression(state)
      Right(ParseResult(cst, state.getRest))
    } catch {
      case e: ParseException => Left(ParseError(e.message, Some(e.span)))
    }
  }
  
  private def parseExpression(state: ParserState): CST = {
    state.skipTrivia()
    
    state.current match {
      case Some(token) => token match {
        case Token.LParen(span) => parseTuple(state)
        case Token.LBracket(span) => parseListLiteral(state)
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
          throw ParseException(s"Unexpected token: ${token.tokenType}", token.span)
      }
      case None =>
        val lastSpan = if (state.tokens.nonEmpty) state.tokens.last.span else {
          throw ParseException("No tokens available", 
            Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero)))
        }
        throw ParseException("Unexpected end of input", lastSpan)
    }
  }
  
  private def parseTuple(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance() // Skip LParen
    state.skipTrivia()
    
    val elements = ArrayBuffer.empty[CST]
    
    while (state.hasNext && !state.current.exists(_.isInstanceOf[Token.RParen])) {
      elements += parseExpression(state)
      state.skipTrivia()
      
      state.current match {
        case Some(Token.Comma(_)) =>
          state.advance()
          state.skipTrivia()
        case Some(Token.RParen(_)) =>
          // End of tuple
        case Some(other) =>
          throw ParseException(s"Expected ',' or ')' but got ${other.tokenType}", other.span)
        case None =>
          throw ParseException("Unexpected end of input in tuple", start)
      }
    }
    
    state.current match {
      case Some(Token.RParen(endSpan)) =>
        state.advance()
        CST.Tuple(elements.toVector, start.combine(endSpan))
      case _ =>
        throw ParseException("Expected ')' to close tuple", start)
    }
  }
  
  private def parseListLiteral(state: ParserState): CST = {
    val start = state.current.get.span
    state.advance() // Skip LBracket
    state.skipTrivia()
    
    val elements = ArrayBuffer.empty[CST]
    
    while (state.hasNext && !state.current.exists(_.isInstanceOf[Token.RBracket])) {
      elements += parseExpression(state)
      state.skipTrivia()
      
      state.current match {
        case Some(Token.Comma(_)) =>
          state.advance()
          state.skipTrivia()
        case Some(Token.RBracket(_)) =>
          // End of list
        case Some(other) =>
          throw ParseException(s"Expected ',' or ']' but got ${other.tokenType}", other.span)
        case None =>
          throw ParseException("Unexpected end of input in list", start)
      }
    }
    
    state.current match {
      case Some(Token.RBracket(endSpan)) =>
        state.advance()
        CST.ListLiteral(elements.toVector, start.combine(endSpan))
      case _ =>
        throw ParseException("Expected ']' to close list", start)
    }
  }
  
  private case class ParseException(message: String, span: Span) extends Exception(message)
}
