package chester

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable.ArrayBuffer
import cats.data.NonEmptyVector
import upickle.default.*
import chester.i18n.*

enum CommentKind derives ReadWriter:
  case Line
  case Block

import chester.utils.doc.{Doc, DocConf, ToDoc}

enum CST(val span: Option[Span]) extends ToDoc with SpanOptional derives ReadWriter:
  override def toDoc(using conf: DocConf): Doc = Formatter.toDoc(this)
  case Symbol(name: String, override val span: Option[Span]) extends CST(span)
  case Tuple(elements: Vector[CST], override val span: Option[Span]) extends CST(span)
  case ListLiteral(elements: Vector[CST], override val span: Option[Span]) extends CST(span)
  case Block(elements: Vector[CST], tail: Option[CST], override val span: Option[Span]) extends CST(span)
  case StringLiteral(value: String, override val span: Option[Span]) extends CST(span)
  case IntegerLiteral(value: BigInt, override val span: Option[Span]) extends CST(span)
  case SeqOf(elements: NonEmptyVector[CST], override val span: Option[Span]) extends CST(span)
  case Comment(text: String, kind: CommentKind, override val span: Option[Span]) extends CST(span)

type Text = Vector[StringChar]

case class StringChar(text: String, span: Span) {
  require(
    text.lenIsOne,
    "StringChar must represent a single UTF-16 character or a valid surrogate pair in UTF-16"
  )
}

object StringChar {
  def apply(codePoint: Int, span: Span): StringChar =
    new StringChar(codepointToString(codePoint), span)

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
  case Comment(text: Text, span: Span, kind: CommentKind)
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
    case Comment(_, _, _) => true
    case _                => false
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
    case Comment(_, _, _)      => t"comment"
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
    def toStr: String = token.parts.map(_.text).mkString
    def isOperator: Boolean = {
      if (token.parts.isEmpty) return false
      IdentifierRules.strIsOperator(toStr)
    }
    def text: String = toStr
  }
}

enum TokenResult {
  case Success(token: Token)
  case Error(error: ParseError, recoveredToken: Option[Token])
}

object CharReader {
  private def reading(strings: Seq[String], source: Source, offset: Offset): LazyList[StringChar] = {
    if (strings.isEmpty) return LazyList.empty
    val head = strings.head
    val tail = strings.tail
    val chars = ArrayBuffer.empty[StringChar]
    var currentOffset = offset
    for (codePoint <- head.getCodePoints) {
      val beginPos = currentOffset.getPos
      currentOffset = currentOffset.next(codePoint)
      val endPos = currentOffset.getPos
      chars += StringChar(codePoint, Span(source, SpanInFile(beginPos, endPos)))
    }
    LazyList.from(chars) #::: reading(tail, source, currentOffset)
  }
  def read(source: Source): Either[ParseError, Seq[StringChar]] = source.readContent.map(strings => reading(strings, source, source.offset))
}

object IdentifierRules {
  private val AllowedOperatorSymbols: Set[Int] = ".:=-+\\|<>/?`~!@$%^&*".toSet.map(_.asInt)
  private val AllowedWordingSymbols: Set[Int] = "_".toSet.map(_.asInt)
  private val AllowedMiddleWordingSymbols: Set[Int] = "-".toSet.map(_.asInt)
  val ReservedSymbols = ";,#()[]{}'\""

  private def isEmoji(codePoint: Int): Boolean =
    codePointIsEmoji(codePoint)

  private def isWording(x: Int): Boolean = java.lang.Character.isLetter(x) || isEmoji(x)

  def isOperatorSymbol(x: Int): Boolean = AllowedOperatorSymbols.contains(x)

  private def isWordingSymbol(x: Int): Boolean = AllowedWordingSymbols.contains(x)

  private def isMiddleWordingSymbol(x: Int): Boolean =
    AllowedMiddleWordingSymbols.contains(x)

  def isIdentifierFirst(x: Int): Boolean = isWording(x) || isWordingSymbol(x)

  def isIdentifierPart(x: Int): Boolean =
    isIdentifierFirst(x) || java.lang.Character.isDigit(x) || isMiddleWordingSymbol(x)

  def isIdentifierEnd(x: Int): Boolean = isIdentifierFirst(x) || java.lang.Character.isDigit(x)

  def isOperatorIdentifierFirst(x: Int): Boolean = isOperatorSymbol(x)

  def isOperatorIdentifierRest(x: Int): Boolean =
    isOperatorSymbol(x) || isWordingSymbol(x)

  def strIsOperator(s: String): Boolean = {
    val codepoints = s.getCodePoints
    if (codepoints.isEmpty) return false
    if (!isOperatorSymbol(codepoints.head)) return false
    codepoints.forall(isOperatorSymbol)
  }
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
    def codePoint: Option[Int] = current.map(_.text.codePointAt(0))
    def currentPos: Int = position
    def spanFrom(start: Int): Span = {
      if (start >= chars.length) {
        chars.lastOption.getOrElse(
          throw new IllegalStateException("Cannot create span from empty character sequence")
        ).span
      } else if (position >= chars.length) {
        chars(start).span.combine(chars.last.span)
      } else {
        chars(start).span.combine(chars(position - 1).span)
      }
    }
  }

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

  def tokenize(chars: Seq[StringChar]): Either[ParseError, Vector[Token]] = {
    val results = tokenizeLazy(chars).toVector
    val errors = results.collect { case TokenResult.Error(err, _) => err }
    if (errors.nonEmpty) {
      Left(errors.head)
    } else {
      val tokens = results.collect { case TokenResult.Success(token) => token }
      Right(tokens)
    }
  }

  private def readTokenWithRecovery(state: TokenizerState): Option[TokenResult] = {
    try
      readToken(state).map(TokenResult.Success(_))
    catch {
      case e: TokenizeException =>
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
          case _ if java.lang.Character.isWhitespace(cp) =>
            readWhitespace(state)
            None
          case _ if java.lang.Character.isDigit(cp)       => Some(readNumberLiteral(state))
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
    while (state.codePoint.exists(java.lang.Character.isWhitespace)) {
      if (state.current.get.text == "\n" || state.current.get.text == "\r") {
        hasNewline = true
      }
      state.advance()
    }
  }

  private def readLineComment(state: TokenizerState): Token = {
    val start = state.currentPos
    state.advance()
    state.advance()
    val chars = ArrayBuffer.empty[StringChar]
    while (state.hasNext && state.current.get.text != "\n" && state.current.get.text != "\r") {
      chars += state.current.get
      state.advance()
    }
    Token.Comment(chars.toVector, state.spanFrom(start), CommentKind.Line)
  }

  private def readBlockComment(state: TokenizerState): Token = {
    val start = state.currentPos
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
        if (depth > 0) {
          chars += state.current.get
          state.advance()
          chars += state.current.get
          state.advance()
        } else {
          state.advance()
          state.advance()
        }
      } else {
        chars += state.current.get
        state.advance()
      }
    }
    if (depth > 0) {
      throw TokenizeException(t"Unclosed block comment (closed at end of file)", state.spanFrom(start))
    }
    Token.Comment(chars.toVector, state.spanFrom(start), CommentKind.Block)
  }

  private def readStringLiteral(state: TokenizerState): Token = {
    val start = state.currentPos
    state.advance()
    val chars = ArrayBuffer.empty[StringChar]
    var foundClosing = false
    while (state.hasNext && !foundClosing) {
      state.current.get.text match {
        case "\"" =>
          foundClosing = true
        case "\n" | "\r" =>
          throw TokenizeException(t"Unclosed string literal (closed at newline)", state.spanFrom(start))
        case "\\" =>
          state.advance()
          if (!state.hasNext) {
            throw TokenizeException(t"Unexpected end of string after backslash", state.spanFrom(start))
          }
          val escaped = state.current.get.text match {
            case "n"   => StringChar("\n", state.current.get.span)
            case "t"   => StringChar("\t", state.current.get.span)
            case "r"   => StringChar("\r", state.current.get.span)
            case "\\"  => StringChar("\\", state.current.get.span)
            case "\""  => StringChar("\"", state.current.get.span)
            case other =>
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
      throw TokenizeException(t"Unclosed string literal at end of file", state.spanFrom(start))
    }
    state.advance()
    Token.StringLiteral(chars.toVector, state.spanFrom(start))
  }

  private def readSymbolLiteral(state: TokenizerState): Token = {
    val start = state.currentPos
    state.advance()
    val chars = ArrayBuffer.empty[StringChar]
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
    while (state.codePoint.exists(cp => java.lang.Character.isDigit(cp) || cp == '_'.asInt)) {
      if (state.current.get.text != "_") {
        chars += state.current.get
      }
      state.advance()
    }
    if (state.current.exists(_.text == "/") && state.peek(1).exists(c => java.lang.Character.isDigit(c.text.codePointAt(0)))) {
      chars += state.current.get
      state.advance()
      while (state.codePoint.exists(cp => java.lang.Character.isDigit(cp) || cp == '_'.asInt)) {
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

  private def readIdentifier(state: TokenizerState): Token = {
    val start = state.currentPos
    val chars = ArrayBuffer.empty[StringChar]
    chars += state.current.get
    state.advance()
    while (state.codePoint.exists(IdentifierRules.isIdentifierPart)) {
      chars += state.current.get
      state.advance()
    }
    if (chars.nonEmpty && !IdentifierRules.isIdentifierEnd(chars.last.text.codePointAt(0))) {
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

object Parser {

  case class ParseResult(cst: CST, rest: Seq[Token])

  private class ParserState(val tokens: Seq[Token], val reporter: Reporter[ParseError], val preserveComments: Boolean) {
    private var position = 0

    def hasNext: Boolean = position < tokens.length && !current.exists(_.isInstanceOf[Token.EOF])
    def current: Option[Token] = if (position < tokens.length) Some(tokens(position)) else None
    def advance(): Unit = if (hasNext) position += 1
    def getRest: Seq[Token] = tokens.drop(position)

    def skipWhitespace(): Unit =
      while (hasNext && current.exists(_.isWhitespace)) advance()

    def skipTrivia(): Unit = {
      if (preserveComments) skipWhitespace()
      else while (hasNext && current.exists(t => t.isWhitespace || t.isComment)) advance()
    }

    def recordError(message: String, span: Span): Unit =
      reporter.report(ParseError(message, Some(span)))

    def is(check: Token => Boolean): Boolean = current.exists(check)
    def isEOF: Boolean = is(_.isInstanceOf[Token.EOF]) || !hasNext
    def isSemicolon: Boolean = is(_.isInstanceOf[Token.Semicolon])
    def shouldStopInBlock: Boolean = is {
      case _: Token.Semicolon | _: Token.RBrace | _: Token.RParen | _: Token.EOF => true
      case _                                                                     => false
    }
  }

  def parseFile(tokens: Seq[Token], preserveComments: Boolean = false)(using reporter: Reporter[ParseError]): CST = {
    val state = new ParserState(tokens, reporter, preserveComments)
    state.skipTrivia()

    if (!state.hasNext) {
      val dummySpan = {
        if (tokens.nonEmpty) Some(tokens.last.span)
        else Some(Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero)))
      }
      return CST.Block(Vector.empty, None, dummySpan)
    }

    val startSpan = state.current.get.span
    val elements = ArrayBuffer.empty[CST]
    var tail: Option[CST] = None

    while (state.hasNext && !state.isEOF) {
      if (state.isSemicolon) {
        state.advance()
        state.skipTrivia()
      } else {
        val statement = parseAtomSequence(state)
        state.skipTrivia()

        if (state.isSemicolon) {
          elements += statement
          state.advance()
          state.skipTrivia()
        } else if (state.isEOF) {
          tail = Some(statement)
        } else {
          state.recordError(s"Expected ';' or end of file, got: ${state.current.get.tokenType}", state.current.get.span)
          elements += statement
          state.advance()
          state.skipTrivia()
        }
      }
    }

    if (!state.isEOF && state.hasNext) {
      state.recordError(s"Unexpected token at end of file: ${state.current.get.tokenType}", state.current.get.span)
    }

    val endSpan = tail.flatMap(_.span).orElse(elements.lastOption.flatMap(_.span)).getOrElse(startSpan)
    CST.Block(elements.toVector, tail, Some(startSpan.combine(endSpan)))
  }

  def parse(tokens: Seq[Token], preserveComments: Boolean = false)(using reporter: Reporter[ParseError]): ParseResult = {
    val state = new ParserState(tokens, reporter, preserveComments)
    state.skipTrivia()

    if (!state.hasNext) {
      val dummySpan = {
        if (tokens.nonEmpty) Some(tokens.last.span)
        else Some(Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero)))
      }
      return ParseResult(CST.Symbol("<empty>", dummySpan), state.getRest)
    }

    val elements = scala.collection.mutable.ArrayBuffer.empty[CST]

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
          case Token.Dot(span) =>
            state.advance()
            CST.Symbol(".", Some(span))
          case Token.SymbolLiteral(value, span) =>
            state.advance()
            CST.Symbol(value.asString, Some(span))
          case Token.Identifier(parts, span) =>
            state.advance()
            CST.Symbol(parts.map(_.text).mkString, Some(span))
          case Token.Comment(value, span, kind) if state.preserveComments =>
            state.advance()
            CST.Comment(value.asString, kind, Some(span))
          case _ =>
            state.recordError(s"Unexpected token: ${token.tokenType}", token.span)
            state.advance()
            if (state.hasNext) parseAtom(state)
            else CST.Symbol("<error>", Some(token.span))
        }
      case None =>
        val lastSpan = {
          if (state.tokens.nonEmpty) state.tokens.last.span
          else {
            Span(Source(FileNameAndContent("unknown", "")), SpanInFile(Pos.zero, Pos.zero))
          }
        }
        state.recordError("Unexpected end of input", lastSpan)
        CST.Symbol("<error>", Some(lastSpan))
    }
  }

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
      val sequenceAtoms = ArrayBuffer.empty[CST]
      while (
        state.hasNext &&
        !state.is(isClosing) &&
        !state.current.exists(tok => tok.isInstanceOf[Token.Comma] || tok.isInstanceOf[Token.Semicolon])
      ) {
        sequenceAtoms += parseAtom(state)
        state.skipTrivia()
      }

      if (sequenceAtoms.nonEmpty) {
        val element = {
          if (sequenceAtoms.length == 1) sequenceAtoms(0)
          else {
            CST.SeqOf(
              NonEmptyVector.fromVectorUnsafe(sequenceAtoms.toVector),
              for {
                h <- sequenceAtoms.head.span
                l <- sequenceAtoms.last.span
              } yield h.combine(l)
            )
          }
        }
        elements += element
      }

      state.current match {
        case Some(_: Token.Comma) =>
          state.advance()
          state.skipTrivia()
          if (state.is(isClosing)) {
          }
        case Some(tok) if isClosing(tok) =>
        case _ if state.isEOF            => return closeWithError("EOF")
        case Some(_: Token.Semicolon)    => return closeWithError("semicolon in block context")
        case Some(_)                     => return closeWithError("unexpected token")
        case None                        => return closeWithError("unexpected end")
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

  private def combineAtoms(atoms: Seq[CST]): CST = {
    if (atoms.length == 1) atoms(0)
    else {
      CST.SeqOf(
        NonEmptyVector.fromVectorUnsafe(atoms.toVector),
        for {
          h <- atoms.head.span
          l <- atoms.last.span
        } yield h.combine(l)
      )
    }
  }

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

    while (state.hasNext && !state.is(isRBrace) && !state.is(isRParen)) {
      if (state.isSemicolon) {
        state.advance()
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
    }

    state.current match {
      case Some(tok @ Token.RBrace(_)) =>
        state.advance()
        CST.Block(elements.toVector, tail, Some(start.combine(tok.span)))
      case Some(Token.RParen(_)) => closeWithError()
      case _                     => closeWithError()
    }
  }
}
