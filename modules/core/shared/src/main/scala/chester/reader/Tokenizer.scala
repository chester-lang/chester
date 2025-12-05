package chester.reader

import chester.error.{Span, SpanInFile}
import chester.i18n.*
import chester.syntax.IdentifierRules
import chester.utils.asInt
import chester.utils.parse.Character
 import scala.language.experimental.genericNumberLiterals

import scala.collection.mutable.ArrayBuffer

object Tokenizer {
  
  private class TokenizerState(val chars: Seq[StringChar]) {
    private var position = 0
    
    def hasNext: Boolean = position < chars.length
    
    def peek(offset: Int = 0): Option[StringChar] = {
      val idx = position + offset
      if (idx >= 0 && idx < chars.length) Some(chars(idx)) else None
    }
    
    def current: Option[StringChar] = peek(0)
    
    def advance(): Unit = if (hasNext) position += 1
    
    def codePoint: Option[Character] = current.map(_.text.codePointAt(0))
    
    def currentPos: Int = position
    
    def spanFrom(start: Int): Span = {
      if (start >= chars.length) {
        chars.lastOption.map(_.span).getOrElse(
          // Return a dummy span at the end
          chars.lastOption.map(_.span).getOrElse(
            throw new IllegalStateException("Cannot create span from empty character sequence")
          )
        )
      } else if (position >= chars.length) {
        chars(start).span.combine(chars.last.span)
      } else {
        chars(start).span.combine(chars(position - 1).span)
      }
    }
  }
  
  def tokenize(chars: Seq[StringChar]): Either[ParseError, Vector[Token]] = {
    if (chars.isEmpty) {
      return Left(ParseError("Cannot tokenize empty input", None))
    }
    
    val state = new TokenizerState(chars)
    val tokens = ArrayBuffer.empty[Token]
    
    try {
      while (state.hasNext) {
        readToken(state) match {
          case Some(token) => tokens += token
          case None => // Skip
        }
      }
      
      // Add EOF token
      val eofSpan = chars.last.span
      tokens += Token.EOF(eofSpan)
      
      Right(tokens.toVector)
    } catch {
      case e: TokenizeException => Left(ParseError(e.message, Some(e.span)))
    }
  }
  
  private def readToken(state: TokenizerState): Option[Token] = {
    state.codePoint match {
      case None => None
      case Some(cp) =>
        cp.asInt match {
          case '(' => Some(readSingleChar(state, Token.LParen.apply))
          case ')' => Some(readSingleChar(state, Token.RParen.apply))
          case '[' => Some(readSingleChar(state, Token.LBracket.apply))
          case ']' => Some(readSingleChar(state, Token.RBracket.apply))
          case '{' => Some(readSingleChar(state, Token.LBrace.apply))
          case '}' => Some(readSingleChar(state, Token.RBrace.apply))
          case ',' => Some(readSingleChar(state, Token.Comma.apply))
          case ';' => Some(readSingleChar(state, Token.Semicolon.apply))
          case '.' => Some(readSingleChar(state, Token.Dot.apply))
          case '@' => Some(readSingleChar(state, Token.At.apply))
          case '#' => Some(readSingleChar(state, Token.Hash.apply))
          case '"' => Some(readStringLiteral(state))
          case '\'' => Some(readSymbolLiteral(state))
          case '/' if state.peek(1).exists(_.text.codePointAt(0) == '/') => 
            Some(readLineComment(state))
          case '/' if state.peek(1).exists(_.text.codePointAt(0) == '*') => 
            Some(readBlockComment(state))
          case _ if Character.isWhitespace(cp) => 
            readWhitespace(state)
            None
          case _ if Character.isDigit(cp) => Some(readNumberLiteral(state))
          case _ if IdentifierRules.isIdentifierFirst(cp) => Some(readIdentifier(state))
          case _ if IdentifierRules.isOperatorSymbol(cp) => Some(readOperatorIdentifier(state))
          case _ => 
            val span = state.current.get.span
            throw TokenizeException(t"Unexpected character: '${state.current.get.text}'", span)
        }
    }
  }
  
  private def readSingleChar(state: TokenizerState, constructor: Span => Token): Token = {
    val start = state.currentPos
    state.advance()
    constructor(state.spanFrom(start))
  }
  
  private def readWhitespace(state: TokenizerState): Unit = {
    var hasNewline = false
    while (state.codePoint.exists(Character.isWhitespace)) {
      if (state.current.get.text == "\n" || state.current.get.text == "\r") {
        hasNewline = true
      }
      state.advance()
    }
  }
  
  private def readLineComment(state: TokenizerState): Token = {
    val start = state.currentPos
    // Skip //
    state.advance()
    state.advance()
    
    val chars = ArrayBuffer.empty[Char]
    while (state.hasNext && state.current.get.text != "\n") {
      chars += state.current.get.text.head
      state.advance()
    }
    
    Token.Comment(chars.mkString, state.spanFrom(start))
  }
  
  private def readBlockComment(state: TokenizerState): Token = {
    val start = state.currentPos
    // Skip /*
    state.advance()
    state.advance()
    
    val chars = ArrayBuffer.empty[Char]
    var depth = 1
    while (state.hasNext && depth > 0) {
      if (state.current.get.text == "/" && state.peek(1).exists(_.text == "*")) {
        depth += 1
        chars += state.current.get.text.head
        state.advance()
        chars += state.current.get.text.head
        state.advance()
      } else if (state.current.get.text == "*" && state.peek(1).exists(_.text == "/")) {
        depth -= 1
        chars += state.current.get.text.head
        state.advance()
        chars += state.current.get.text.head
        state.advance()
      } else {
        chars += state.current.get.text.head
        state.advance()
      }
    }
    
    if (depth > 0) {
      throw TokenizeException(t"Unclosed block comment", state.spanFrom(start))
    }
    
    Token.Comment(chars.mkString, state.spanFrom(start))
  }
  
  private def readStringLiteral(state: TokenizerState): Token = {
    val start = state.currentPos
    state.advance() // Skip opening "
    
    val chars = ArrayBuffer.empty[StringChar]
    
    while (state.hasNext && state.current.get.text != "\"") {
      if (state.current.get.text == "\\") {
        state.advance()
        if (!state.hasNext) {
          throw TokenizeException(t"Unexpected end of string", state.spanFrom(start))
        }
        // Handle escape sequences
        val escaped = state.current.get.text match {
          case "n" => StringChar("\n".codePointAt(0), state.current.get.span)
          case "t" => StringChar("\t".codePointAt(0), state.current.get.span)
          case "r" => StringChar("\r".codePointAt(0), state.current.get.span)
          case "\\" => StringChar("\\".codePointAt(0), state.current.get.span)
          case "\"" => StringChar("\"".codePointAt(0), state.current.get.span)
          case other => throw TokenizeException(t"Invalid escape sequence: \\$other", state.current.get.span)
        }
        chars += escaped
        state.advance()
      } else {
        chars += state.current.get
        state.advance()
      }
    }
    
    if (!state.hasNext) {
      throw TokenizeException(t"Unclosed string literal", state.spanFrom(start))
    }
    
    state.advance() // Skip closing "
    Token.StringLiteral(chars.toVector, state.spanFrom(start))
  }
  
  private def readSymbolLiteral(state: TokenizerState): Token = {
    val start = state.currentPos
    state.advance() // Skip opening '
    
    val chars = ArrayBuffer.empty[Char]
    
    // Read identifier characters using IdentifierRules
    while (state.codePoint.exists(IdentifierRules.isIdentifierPart)) {
      chars += state.current.get.text.head
      state.advance()
    }
    
    if (chars.isEmpty) {
      throw TokenizeException(t"Empty symbol literal", state.spanFrom(start))
    }
    
    Token.SymbolLiteral(chars.mkString, state.spanFrom(start))
  }
  
  private def readNumberLiteral(state: TokenizerState): Token = {
    val start = state.currentPos
    val chars = ArrayBuffer.empty[Char]
    
    while (state.codePoint.exists(cp => Character.isDigit(cp) || cp == '_'.asInt)) {
      if (state.current.get.text != "_") {
        chars += state.current.get.text.head
      }
      state.advance()
    }
    
    // Check for rational literal (/)
    if (state.current.exists(_.text == "/") && state.peek(1).exists(c => Character.isDigit(c.text.codePointAt(0)))) {
      chars += '/'
      state.advance()
      
      while (state.codePoint.exists(cp => Character.isDigit(cp) || cp == '_'.asInt)) {
        if (state.current.get.text != "_") {
          chars += state.current.get.text.head
        }
        state.advance()
      }
      
      Token.RationalLiteral(chars.mkString, state.spanFrom(start))
    } else {
      Token.IntegerLiteral(chars.mkString, state.spanFrom(start))
    }
  }
  
  private def readIdentifier(state: TokenizerState): Token = {
    val start = state.currentPos
    val chars = ArrayBuffer.empty[StringChar]
    
    // First character must be valid identifier start
    chars += state.current.get
    state.advance()
    
    // Middle characters
    while (state.codePoint.exists(IdentifierRules.isIdentifierPart)) {
      chars += state.current.get
      state.advance()
    }
    
    // Check if last character is valid identifier end
    if (chars.nonEmpty && !IdentifierRules.isIdentifierEnd(chars.last.text.codePointAt(0))) {
      throw TokenizeException(t"Invalid identifier: cannot end with '${chars.last.text}'", state.spanFrom(start))
    }
    
    Token.Identifier(chars.toVector, state.spanFrom(start))
  }
  
  private def readOperatorIdentifier(state: TokenizerState): Token = {
    val start = state.currentPos
    val chars = ArrayBuffer.empty[StringChar]
    
    while (state.codePoint.exists(IdentifierRules.isOperatorSymbol)) {
      chars += state.current.get
      state.advance()
    }
    
    Token.Identifier(chars.toVector, state.spanFrom(start))
  }
  
  private case class TokenizeException(message: String, span: Span) extends Exception(message)
}
