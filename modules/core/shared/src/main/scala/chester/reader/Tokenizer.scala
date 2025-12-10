package chester.reader

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable.ArrayBuffer

import chester.core.CommentKind
import chester.error.{Span, SpanInFile}
import chester.i18n.*
import chester.syntax.IdentifierRules
import chester.utils.asInt
import chester.utils.parse.Character

/** Result of tokenization - either a token or an error with best-effort recovery */
enum TokenResult {
  case Success(token: Token)
  case Error(error: ParseError, recoveredToken: Option[Token])
}

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
        chars.lastOption
          .map(_.span)
          .getOrElse(
            // Return a dummy span at the end
            chars.lastOption
              .map(_.span)
              .getOrElse(
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

  /** Tokenize with lazy evaluation and error recovery. Returns a LazyList that can be consumed incrementally, useful for:
    *   - Large files where we only need to see the beginning
    *   - IDE scenarios where files are being edited
    *   - Streaming/incremental parsing
    *
    * Error recovery strategies:
    *   - Unclosed strings: Close at newline or EOF
    *   - Unclosed block comments: Close at EOF
    *   - Invalid escape sequences: Use the literal character
    *   - Unexpected characters: Skip and continue
    *   - Invalid identifiers: Accept them anyway and report error
    */
  def tokenizeLazy(chars: Seq[StringChar]): LazyList[TokenResult] = {
    if (chars.isEmpty) {
      return LazyList(
        TokenResult.Error(
          ParseError("Cannot tokenize empty input", None),
          None
        )
      )
    }

    val state = new TokenizerState(chars)

    def readTokens(): LazyList[TokenResult] = {
      if (!state.hasNext) {
        // Add EOF token
        val eofSpan = chars.last.span
        LazyList(TokenResult.Success(Token.EOF(eofSpan)))
      } else {
        readTokenWithRecovery(state) match {
          case Some(result) => result #:: readTokens()
          case None         => readTokens() // Skip trivia
        }
      }
    }

    readTokens()
  }

  /** Legacy non-lazy version for backward compatibility */
  def tokenize(chars: Seq[StringChar]): Either[ParseError, Vector[Token]] = {
    val results = tokenizeLazy(chars).toVector
    val errors = results.collect { case TokenResult.Error(err, _) => err }

    if (errors.nonEmpty) {
      Left(errors.head) // Return first error
    } else {
      val tokens = results.collect { case TokenResult.Success(token) => token }
      Right(tokens)
    }
  }

  /** Read a token with error recovery - returns None for trivia (whitespace) */
  private def readTokenWithRecovery(state: TokenizerState): Option[TokenResult] = {
    try
      readToken(state).map(TokenResult.Success(_))
    catch {
      case e: TokenizeException =>
        // Error recovery: try to continue parsing
        Some(TokenResult.Error(ParseError(e.message, Some(e.span)), None))
    }
  }

  private def readToken(state: TokenizerState): Option[Token] = {
    state.codePoint match {
      case None => None
      case Some(cp) =>
        cp.asInt match {
          case '('  => Some(readSingleChar(state, Token.LParen.apply))
          case ')'  => Some(readSingleChar(state, Token.RParen.apply))
          case '['  => Some(readSingleChar(state, Token.LBracket.apply))
          case ']'  => Some(readSingleChar(state, Token.RBracket.apply))
          case '{'  => Some(readSingleChar(state, Token.LBrace.apply))
          case '}'  => Some(readSingleChar(state, Token.RBrace.apply))
          case ','  => Some(readSingleChar(state, Token.Comma.apply))
          case ';'  => Some(readSingleChar(state, Token.Semicolon.apply))
          case '.'  => Some(readSingleChar(state, Token.Dot.apply))
          case '@'  => Some(readSingleChar(state, Token.At.apply))
          case '#'  => Some(readSingleChar(state, Token.Hash.apply))
          case '"'  => Some(readStringLiteral(state))
          case '\'' => Some(readSymbolLiteral(state))
          case '/' if state.peek(1).exists(_.text.codePointAt(0) == '/') =>
            Some(readLineComment(state))
          case '/' if state.peek(1).exists(_.text.codePointAt(0) == '*') =>
            Some(readBlockComment(state))
          case _ if Character.isWhitespace(cp) =>
            readWhitespace(state)
            None
          case _ if Character.isDigit(cp)                 => Some(readNumberLiteral(state))
          case _ if IdentifierRules.isIdentifierFirst(cp) => Some(readIdentifier(state))
          case _ if IdentifierRules.isOperatorSymbol(cp)  => Some(readOperatorIdentifier(state))
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

    val chars = ArrayBuffer.empty[StringChar]
    while (state.hasNext && state.current.get.text != "\n" && state.current.get.text != "\r") {
      chars += state.current.get
      state.advance()
    }

    Token.Comment(chars.toVector, state.spanFrom(start), CommentKind.Line)
  }

  /** Read block comment with error recovery:
    *   - Unclosed comments: close at EOF (common when editing)
    *   - Supports nested block comments
    */
  private def readBlockComment(state: TokenizerState): Token = {
    val start = state.currentPos
    // Skip /*
    state.advance()
    state.advance()

    val chars = ArrayBuffer.empty[StringChar]
    var depth = 1
    while (state.hasNext && depth > 0) {
      if (state.current.get.text == "/" && state.peek(1).exists(_.text == "*")) {
        depth += 1
        chars += state.current.get
        state.advance()
        chars += state.current.get
        state.advance()
      } else if (state.current.get.text == "*" && state.peek(1).exists(_.text == "/")) {
        depth -= 1
        if depth > 0 then
          chars += state.current.get
          state.advance()
          chars += state.current.get
          state.advance()
        else
          // Final closing delimiter - consume but do not include in comment text
          state.advance()
          state.advance()
      } else {
        chars += state.current.get
        state.advance()
      }
    }

    if (depth > 0) {
      // Error recovery: unclosed block comment at EOF - common in editing
      throw TokenizeException(t"Unclosed block comment (closed at end of file)", state.spanFrom(start))
    }

    Token.Comment(chars.toVector, state.spanFrom(start), CommentKind.Block)
  }

  /** Read string literal with error recovery:
    *   - Unclosed strings: close at newline or EOF (common in incomplete code)
    *   - Invalid escape sequences: use the literal character after backslash
    */
  private def readStringLiteral(state: TokenizerState): Token = {
    val start = state.currentPos
    state.advance() // Skip opening "

    val chars = ArrayBuffer.empty[StringChar]
    var foundClosing = false

    while (state.hasNext && !foundClosing) {
      state.current.get.text match {
        case "\"" =>
          foundClosing = true
        case "\n" | "\r" =>
          // Error recovery: unclosed string at newline - common in editing
          throw TokenizeException(t"Unclosed string literal (closed at newline)", state.spanFrom(start))
        case "\\" =>
          state.advance()
          if (!state.hasNext) {
            // Error recovery: backslash at EOF
            throw TokenizeException(t"Unexpected end of string after backslash", state.spanFrom(start))
          }
          // Handle escape sequences with recovery
          val escaped = state.current.get.text match {
            case "n"   => StringChar("\n".codePointAt(0), state.current.get.span)
            case "t"   => StringChar("\t".codePointAt(0), state.current.get.span)
            case "r"   => StringChar("\r".codePointAt(0), state.current.get.span)
            case "\\"  => StringChar("\\".codePointAt(0), state.current.get.span)
            case "\""  => StringChar("\"".codePointAt(0), state.current.get.span)
            case other =>
              // Error recovery: invalid escape - use literal character
              throw TokenizeException(t"Invalid escape sequence: \\$other (using literal)", state.current.get.span)
          }
          chars += escaped
          state.advance()
        case _ =>
          chars += state.current.get
          state.advance()
      }
    }

    if (!foundClosing) {
      // Error recovery: unclosed string at EOF
      throw TokenizeException(t"Unclosed string literal at end of file", state.spanFrom(start))
    }

    state.advance() // Skip closing "
    Token.StringLiteral(chars.toVector, state.spanFrom(start))
  }

  private def readSymbolLiteral(state: TokenizerState): Token = {
    val start = state.currentPos
    state.advance() // Skip opening '

    val chars = ArrayBuffer.empty[StringChar]

    // Read identifier characters using IdentifierRules
    while (state.codePoint.exists(IdentifierRules.isIdentifierPart)) {
      chars += state.current.get
      state.advance()
    }

    if (chars.isEmpty) {
      throw TokenizeException(t"Empty symbol literal", state.spanFrom(start))
    }

    Token.SymbolLiteral(chars.toVector, state.spanFrom(start))
  }

  private def readNumberLiteral(state: TokenizerState): Token = {
    val start = state.currentPos
    val chars = ArrayBuffer.empty[StringChar]

    while (state.codePoint.exists(cp => Character.isDigit(cp) || cp == '_'.asInt)) {
      if (state.current.get.text != "_") {
        chars += state.current.get
      }
      state.advance()
    }

    // Check for rational literal (/)
    if (state.current.exists(_.text == "/") && state.peek(1).exists(c => Character.isDigit(c.text.codePointAt(0)))) {
      chars += state.current.get
      state.advance()

      while (state.codePoint.exists(cp => Character.isDigit(cp) || cp == '_'.asInt)) {
        if (state.current.get.text != "_") {
          chars += state.current.get
        }
        state.advance()
      }

      Token.RationalLiteral(chars.toVector, state.spanFrom(start))
    } else {
      Token.IntegerLiteral(chars.toVector, state.spanFrom(start))
    }
  }

  /** Read identifier with error recovery:
    *   - Accept identifiers with invalid endings (report error but continue)
    *   - Useful for incomplete identifiers being typed in IDE
    */
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
      // Error recovery: accept invalid identifier ending (common when typing)
      throw TokenizeException(t"Invalid identifier: cannot end with '${chars.last.text}' (accepted anyway)", state.spanFrom(start))
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
