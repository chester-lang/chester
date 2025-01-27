package chester.readerv2

import chester.error.{Pos, RangeInFile, SourcePos}
import chester.reader.{ParseError, SourceOffset}
import chester.readerv2.Token
import chester.syntax.concrete.*

case class LexerState(
    tokens: TokenStream,
    current: Token,
    errors: Vector[ParseError] = Vector.empty,
    ignoreLocation: Boolean = false,
    sourceOffset: SourceOffset
)

object LexerV2 {
  private def createMeta(pos: Pos, sourceOffset: SourceOffset): ExprMeta = {
    ExprMeta(Some(SourcePos(sourceOffset, RangeInFile(pos, pos))), None)
  }

  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false): LexerState = {
    tokens.headOption match {
      case Some(Right(token)) => LexerState(tokens.tail, token, ignoreLocation = ignoreLocation, sourceOffset = sourceOffset)
      case Some(Left(error)) => 
        val initialState = LexerState(tokens.tail, Token.EOF(error.pos), Vector(error), ignoreLocation, sourceOffset)
        advance(initialState)
      case None => LexerState(LazyList.empty, Token.EOF(Pos.zero), ignoreLocation = ignoreLocation, sourceOffset = sourceOffset)
    }
  }

  def advance(state: LexerState): LexerState = {
    var currentState = state
    while (true) {
      currentState.tokens.headOption match {
        case Some(Right(token)) =>
          return currentState.copy(tokens = currentState.tokens.tail, current = token)
        case Some(Left(error)) =>
          currentState = currentState.copy(
            tokens = currentState.tokens.tail,
            errors = currentState.errors :+ error
          )
        case None =>
          return currentState.copy(current = Token.EOF(Pos.zero), tokens = LazyList.empty)
      }
    }
    currentState
  }

  def skipWhitespaceAndComments(state: LexerState): LexerState = {
    var currentState = state
    while (currentState.current match {
      case _: Token.Whitespace | _: Token.SingleLineComment => true
      case _ => false
    }) {
      currentState = advance(currentState)
    }
    currentState
  }

  def peek(state: LexerState): Option[Token] = {
    state.tokens.headOption match {
      case Some(Right(token)) => Some(token)
      case _ => None
    }
  }

  def parseExpr(state: LexerState): Either[ParseError, (ParsedExpr, LexerState)] = {
    val cleanState = skipWhitespaceAndComments(state)
    cleanState.current match {
      case Token.IntegerLiteral(value, radix, pos) =>
        Right((IntegerLiteral(value, if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))), advance(cleanState)))
        
      case Token.RationalLiteral(value, pos) =>
        Right((RationalLiteral(value.toDouble, if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))), advance(cleanState)))
        
      case Token.StringLiteral(segments, pos) =>
        Right((StringLiteral(segments.map {
          case Token.StringChars(chars) => chars.mkString
          case Token.StringEscape(c) => c.toString
          case _ => "" // Handle interpolation later
        }.mkString, if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))), advance(cleanState)))
        
      case Token.Identifier(parts, pos) =>
        Right((Identifier(parts.map {
          case Token.IdentifierPart(chars) => chars.mkString
          case Token.OperatorPart(chars) => chars.mkString
        }.mkString, if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))), advance(cleanState)))
        
      case Token.LParen(pos) =>
        for {
          (exprs, state1) <- parseExprList(advance(cleanState))
          (rparen, state2) <- expect(state1, classOf[Token.RParen])
        } yield (Tuple(exprs, if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))), state2)
        
      case Token.LBracket(pos) =>
        for {
          (exprs, state1) <- parseExprList(advance(cleanState))
          (rbracket, state2) <- expect(state1, classOf[Token.RBracket])
        } yield (ListExpr(exprs, if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))), state2)
        
      case Token.LBrace(pos) =>
        for {
          (fields, state1) <- parseObjectFields(advance(cleanState))
          (rbrace, state2) <- expect(state1, classOf[Token.RBrace])
        } yield (ObjectExpr(fields, if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))), state2)
        
      case token =>
        Left(ParseError(s"Unexpected token: ${token.text}", token.pos))
    }
  }

  private def parseExprList(state: LexerState): Either[ParseError, (Vector[ParsedExpr], LexerState)] = {
    def loop(state: LexerState, acc: Vector[ParsedExpr]): Either[ParseError, (Vector[ParsedExpr], LexerState)] = {
      val cleanState = skipWhitespaceAndComments(state)
      cleanState.current match {
        case _: Token.RParen | _: Token.RBracket => Right((acc, cleanState))
        case _: Token.Comma => loop(advance(cleanState), acc)
        case _ =>
          for {
            (expr, state1) <- parseExpr(cleanState)
            result <- loop(state1, acc :+ expr)
          } yield result
      }
    }
    loop(state, Vector.empty)
  }

  private def parseObjectFields(state: LexerState): Either[ParseError, (Vector[ObjectClause], LexerState)] = {
    def loop(state: LexerState, acc: Vector[ObjectClause]): Either[ParseError, (Vector[ObjectClause], LexerState)] = {
      val cleanState = skipWhitespaceAndComments(state)
      cleanState.current match {
        case _: Token.RBrace => Right((acc, cleanState))
        case _: Token.Comma => loop(advance(cleanState), acc)
        case _ =>
          for {
            (field, state1) <- parseObjectField(cleanState)
            result <- loop(state1, acc :+ field)
          } yield result
      }
    }
    loop(state, Vector.empty)
  }

  private def parseObjectField(state: LexerState): Either[ParseError, (ObjectClause, LexerState)] = {
    for {
      (nameToken, state1) <- expect(state, classOf[Token.Identifier])
      (_, state2) <- expect(state1, classOf[Token.Equal])
      (value, state3) <- parseExpr(state2)
    } yield (ObjectExprClause(
      Identifier(nameToken.text, if (state.ignoreLocation) None else Some(createMeta(nameToken.pos, state.sourceOffset))), 
      value
    ), state3)
  }

  private def expect[T <: Token](state: LexerState, tokenType: Class[T]): Either[ParseError, (T, LexerState)] = {
    if (tokenType.isInstance(state.current)) {
      val token = state.current.asInstanceOf[T]
      val nextState = advance(state)
      Right((token, nextState))
    } else {
      Left(ParseError(
        s"Expected ${tokenType.getSimpleName} but got ${state.current.getClass.getSimpleName}",
        state.current.pos
      ))
    }
  }
}

