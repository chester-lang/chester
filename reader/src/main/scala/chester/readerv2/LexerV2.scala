package chester.readerv2

/*
 * ReaderV2 implements Chester's token-based parser with unified symbol treatment.
 *
 * For detailed documentation on the parser design principles, syntax decisions,
 * and implementation approach, see:
 *   docs/src/dev/parser-migration.md
 *
 * Key principles:
 * - Uniform symbol treatment (no special keywords)
 * - Space and newline significance in specific contexts
 * - Block return value semantics
 * - Operator sequences with later precedence resolution
 */
import chester.error.{Pos, RangeInFile, SourcePos}
import chester.reader.{ParseError, SourceOffset}
import chester.syntax.concrete.{
  Block,
  DotCall,
  Expr,
  ExprMeta,
  FunctionCall,
  ListExpr,
  ObjectClause,
  ObjectExpr,
  ObjectExprClause,
  ObjectExprClauseOnValue,
  OpSeq,
  QualifiedName,
  Tuple
}
import chester.syntax.concrete.{
  Identifier as ConcreteIdentifier,
  IntegerLiteral as ConcreteIntegerLiteral,
  RationalLiteral as ConcreteRationalLiteral,
  StringLiteral as ConcreteStringLiteral
}
import chester.syntax.concrete.Literal.*
import chester.reader.FileNameAndContent
import chester.syntax.IdentifierRules.strIsOperator
import chester.error.*
import chester.reader.*
import chester.syntax.*
import chester.syntax.concrete.*
import chester.readerv2.Token.*

case class StmtExpr(expr: Expr)

case class LexerState(
    tokens: Vector[Either[ParseError, Token]],
    index: Int,
    previousToken: Option[Token] = None,
    newLineAfterBlockMeansEnds: Boolean = false
) {
  def current: Either[ParseError, Token] = tokens(index)
  def isAtEnd: Boolean = index >= tokens.length
  def advance(): LexerState = current match {
    case Right(token) => LexerState(tokens, index + 1, Some(token), newLineAfterBlockMeansEnds)
    case Left(_)      => LexerState(tokens, index + 1, previousToken, newLineAfterBlockMeansEnds)
  }
  def sourcePos: SourcePos = current match {
    case Left(err) => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
    case Right(t)  => t.sourcePos
  }

  // Helper methods for common state checks
  def isAfterClosingBrace: Boolean = previousToken.exists(_.isInstanceOf[Token.RBrace])
  def isAtNewline: Boolean = current.exists {
    case Token.Whitespace(_, hasNewline) => hasNewline
    case Token.EOF(_)                    => true
    case _                               => false
  }
  def isAtClosingBraceNewlinePattern: Boolean = isAfterClosingBrace && isAtNewline
  def isAtTerminator: Boolean = current.exists(t =>
    t.isInstanceOf[Token.EOF] || t.isInstanceOf[Token.RParen] ||
      t.isInstanceOf[Token.RBrace] || t.isInstanceOf[Token.RBracket] ||
      t.isInstanceOf[Token.Comma] || t.isInstanceOf[Token.Semicolon]
  )

  // Helper method to create a state with modified context
  def withNewLineTermination(enabled: Boolean): LexerState =
    if (this.newLineAfterBlockMeansEnds == enabled) this
    else copy(newLineAfterBlockMeansEnds = enabled)

  override def toString: String =
    s"LexerState(index=$index, current=$current, previousToken=$previousToken, remaining=${tokens.length - index} tokens)"
}

object LexerV2 {
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false): LexerV2 =
    new LexerV2(tokens, sourceOffset, ignoreLocation)

  var DEBUG = false // Keep DEBUG flag for tests that use it
  val MAX_LIST_ELEMENTS = 50 // Constants for parser configuration
}

class LexerV2(_tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean) {
  import LexerV2.DEBUG

  private def debug(msg: => String): Unit = if (DEBUG) println(s"[DEBUG] $msg")

  // Helper methods
  private def charsToString(chars: Seq[StringChar]): String = chars.map(_.text).mkString

  private def expectedError(expected: String, token: Either[ParseError, Token]): ParseError = {
    def getTokenType(t: Token): String = t match {
      case _: Token.Identifier      => "identifier"
      case _: Token.IntegerLiteral  => "integer literal"
      case _: Token.RationalLiteral => "rational literal"
      case _: Token.StringLiteral   => "string literal"
      case _: Token.Operator        => "operator"
      case _: Token.LParen          => "left parenthesis '('"
      case _: Token.RParen          => "right parenthesis ')'"
      case _: Token.LBrace          => "left brace '{'"
      case _: Token.RBrace          => "right brace '}'"
      case _: Token.LBracket        => "left bracket '['"
      case _: Token.RBracket        => "right bracket ']'"
      case _: Token.Colon           => "colon ':'"
      case _: Token.Comma           => "comma ','"
      case _: Token.Dot             => "dot '.'"
      case _: Token.Semicolon       => "semicolon ';'"
      case _: Token.EOF             => "end of file"
      case _: Token.Whitespace      => "whitespace"
      case _                        => "unknown token"
    }

    token.fold(
      identity,
      t =>
        ParseError(
          s"Expected $expected but found ${getTokenType(t)} at ${t.sourcePos.range.start.line}:${t.sourcePos.range.start.column}",
          t.sourcePos.range.start
        )
    )
  }

  /** Creates expression metadata from source positions and comments. */
  private def createMeta(startPos: Option[SourcePos], endPos: Option[SourcePos]): Option[ExprMeta] =
    if (ignoreLocation) None
    else
      PartialFunction.condOpt((startPos, endPos)) {
        case (Some(start), Some(end)) =>
          ExprMeta(Some(SourcePos(sourceOffset, RangeInFile(start.range.start, end.range.end))), None)
        case (Some(pos), None) =>
          ExprMeta(Some(pos), None)
        case (None, Some(pos)) =>
          ExprMeta(Some(pos), None)
      }

  private def getStartPos(token: Either[ParseError, Token]): Pos =
    token.fold(_.pos, _.sourcePos.range.start)

  // Type alias for clarity
  type LexerError = ParseError

  private def mergeMeta(existing: Option[ExprMeta], newMeta: Option[ExprMeta]): Option[ExprMeta] =
    (existing, newMeta) match {
      case (Some(existing), Some(ExprMeta(newSourcePos, newCommentInfo))) =>
        val mergedSourcePos = existing.sourcePos.orElse(newSourcePos)
        val mergedCommentInfo = (existing.commentInfo, newCommentInfo) match {
          case (Some(existingInfo), Some(newInfo)) =>
            Some(
              chester.syntax.concrete.CommentInfo(
                commentBefore = existingInfo.commentBefore ++ newInfo.commentBefore,
                commentInBegin = existingInfo.commentInBegin,
                commentInEnd = existingInfo.commentInEnd,
                commentEndInThisLine = existingInfo.commentEndInThisLine ++ newInfo.commentEndInThisLine
              )
            )
          case (None, commentInfo) => commentInfo
          case (commentInfo, None) => commentInfo
        }
        Some(ExprMeta(mergedSourcePos, mergedCommentInfo))
      case (None, meta) => meta
      case (meta, None) => meta
    }

  // Token extractors for cleaner pattern matching
  private object TokenExtractors {
    // Define extractors using pattern matching
    object Id {
      def unapply(token: Either[ParseError, Token]): Option[(Vector[StringChar], SourcePos)] = PartialFunction.condOpt(token) {
        case Right(Token.Identifier(chars, pos)) => (chars, pos)
      }
    }

    object Op {
      def unapply(token: Either[ParseError, Token]): Option[(String, SourcePos)] = PartialFunction.condOpt(token) {
        case Right(Token.Operator(op, pos)) => (op, pos)
      }
    }

    object Str {
      def unapply(token: Either[ParseError, Token]): Option[(Vector[StringChar], SourcePos)] = PartialFunction.condOpt(token) {
        case Right(Token.StringLiteral(chars, pos)) => (chars, pos)
      }
    }

    object Sym {
      def unapply(token: Either[ParseError, Token]): Option[(String, SourcePos)] = PartialFunction.condOpt(token) {
        case Right(Token.SymbolLiteral(value, pos)) => (value, pos)
      }
    }

    object Int {
      def unapply(token: Either[ParseError, Token]): Option[(String, SourcePos)] = PartialFunction.condOpt(token) {
        case Right(Token.IntegerLiteral(value, pos)) => (value, pos)
      }
    }

    object Rat {
      def unapply(token: Either[ParseError, Token]): Option[(String, SourcePos)] = PartialFunction.condOpt(token) {
        case Right(Token.RationalLiteral(value, pos)) => (value, pos)
      }
    }

    // Helper for source position extractors - consolidated into a single implementation
    private def posExtract(tokenPredicate: Token => Boolean): Either[ParseError, Token] => Option[SourcePos] = {
      case Right(token) if tokenPredicate(token) => Some(token.sourcePos)
      case _                                     => None
    }

    // Define all delimiter token extractors using the posExtract helper
    object LParen { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.LParen])(t) }
    object RParen { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.RParen])(t) }
    object LBrace { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.LBrace])(t) }
    object RBrace { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.RBrace])(t) }
    object LBracket { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.LBracket])(t) }
    object RBracket { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.RBracket])(t) }
    object Dot { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.Dot])(t) }
    object Comma { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.Comma])(t) }
    object Colon { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.Colon])(t) }
    object Semi { def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.Semicolon])(t) }

    object Err {
      def unapply(token: Either[ParseError, Token]): Option[ParseError] = PartialFunction.condOpt(token) { case Left(err) =>
        err
      }
    }
  }

  import TokenExtractors._

  // Main parsing methods
  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    val (leadingComments, current) = collectComments(state)
    var terms = Vector.empty[Expr]
    debug(s"Starting parseExpr with state: $current")

    def buildOpSeq(terms: Vector[Expr]): Either[ParseError, Expr] = {
      debug(s"Building OpSeq with terms: $terms")
      terms match {
        case Vector() => Left(ParseError("Empty operator sequence", getStartPos(state.current)))
        case Vector(expr) if leadingComments.nonEmpty =>
          Right(expr.updateMeta(meta => mergeMeta(meta, createMetaWithComments(meta.flatMap(_.sourcePos), leadingComments))))
        case Vector(expr) => Right(expr)
        case _            => Right(OpSeq(terms, Option.when(leadingComments.nonEmpty)(createMetaWithComments(None, leadingComments).get)))
      }
    }

    def parseRest(expr: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
      var localTerms = Vector(expr)
      debug(s"parseRest called with expr: $expr, state: $state, current terms: $localTerms")

      // Check for "}\n" pattern
      def checkForRBraceNewlinePattern(state: LexerState): Boolean = {
        // Only consider }\n as terminating if we're in the right context
        if (!state.newLineAfterBlockMeansEnds) return false

        expr match {
          case block: Block =>
            state.current match {
              case Right(whitespaceToken: Token.Whitespace) => {
                val startPos = whitespaceToken.sourcePos.range.start.index.utf16
                val endPos = whitespaceToken.sourcePos.range.end.index.utf16
                val maybeSource = sourceOffset.readContent.toOption
                maybeSource.exists { source =>
                  if (startPos < source.length && endPos <= source.length) {
                    val whitespaceText = source.substring(startPos, endPos)
                    whitespaceText.contains('\n')
                  } else false
                }
              }
              case _ => false
            }
          case _ => false
        }
      }

      if (checkForRBraceNewlinePattern(state)) {
        debug("parseRest: Terminating expression due to }\n pattern")
        return buildOpSeq(localTerms).map(result => (result, state))
      }

      val (restComments, current) = collectComments(state)

      // Handle terminators
      if (isAtTerminator(current)) {
        debug("parseRest: Hit terminator token")
        return buildOpSeq(localTerms).map(result => (result, current))
      }

      current.current match {
        // Handle block after expression
        case Right(Token.LBrace(braceSourcePos)) => {
          debug("parseRest: Found LBrace after expression, treating as block argument")
          handleBlockArgument(expr, state, localTerms, braceSourcePos)
        }

        // Handle colon
        case Right(Token.Colon(sourcePos)) => {
          debug("parseRest: Found colon")
          handleColon(sourcePos, state, localTerms)
        }

        // Handle dot call
        case Right(Token.Dot(dotSourcePos)) => {
          debug("parseRest: Found dot")
          handleDotCall(dotSourcePos, current, localTerms).flatMap { case (dotCall, newState) =>
            localTerms = Vector(dotCall)
            debug(s"parseRest: After dot call, terms: $localTerms")
            parseRest(dotCall, newState)
          }
        }

        // Handle operators
        case Right(Token.Operator(op, sourcePos)) => {
          debug(s"parseRest: Found operator $op")
          handleOperatorInRest(op, sourcePos, current, localTerms)
        }

        // Handle identifiers
        case Right(Token.Identifier(chars, sourcePos)) => {
          val text = charsToString(chars)
          debug(s"parseRest: Found identifier $text")
          handleIdentifierInRest(text, sourcePos, current, localTerms)
        }

        // Handle other tokens
        case Right(_) => {
          debug("parseRest: Found other token, parsing as atom")
          parseAtomWithComments(current).flatMap { case (next, afterNext) =>
            localTerms = localTerms :+ next
            debug(s"parseRest: After parsing other token as atom, terms: $localTerms")
            parseRest(next, afterNext).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(localTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(localTerms, None), finalState)
              }
            }
          }
        }

        // Handle errors
        case Left(error) => {
          debug(s"parseRest: Got error: $error")
          Left(error)
        }
      }
    }

    def handleBlockArgument(expr: Expr, state: LexerState, terms: Vector[Expr], braceSourcePos: SourcePos): Either[ParseError, (Expr, LexerState)] = {
      parseBlockWithComments(state).flatMap { case (block, afterBlock) =>
        // Handle block argument differently based on previous expression type
        val newExpr = expr match {
          // If previous expression is already a function call, add the block as another argument
          case funcCall: FunctionCall => {
            debug("parseRest: Adding block as argument to existing function call")
            FunctionCall(
              funcCall,
              Tuple(Vector(block), createMeta(None, None)),
              createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
            )
          }
          // If previous expression is an identifier, create a function call with block argument
          case id: ConcreteIdentifier => {
            debug("parseRest: Creating function call with block argument from identifier")
            FunctionCall(
              id,
              Tuple(Vector(block), createMeta(None, None)),
              createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
            )
          }
          // Otherwise, just add the block as a term
          case _ => {
            debug("parseRest: Default handling for block after expression")
            block
          }
        }

        // For function calls with blocks, don't wrap in OpSeq
        if (
          (newExpr.isInstanceOf[FunctionCall] && expr.isInstanceOf[ConcreteIdentifier]) ||
          (newExpr.isInstanceOf[FunctionCall] && expr.isInstanceOf[FunctionCall])
        ) {
          debug("parseRest: Returning function call with block directly")

          if (afterBlock.isAtTerminator) {
            // No more tokens, return the function call directly
            Right((newExpr, afterBlock))
          } else {
            // More tokens follow, continue parsing
            parseRest(newExpr, afterBlock)
          }
        } else {
          // Replace the last term with the new expression that includes the block
          val updatedTerms = terms.dropRight(1) :+ newExpr
          debug(s"parseRest: After handling block argument, terms: $updatedTerms")

          parseRest(newExpr, afterBlock).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(updatedTerms, None), finalState)
            }
          }
        }
      }
    }

    def handleColon(sourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
      val afterColon = state.advance()
      val updatedTerms = terms :+ ConcreteIdentifier(":", createMeta(Some(sourcePos), Some(sourcePos)))
      debug(s"parseRest: After adding colon, terms: $updatedTerms")

      parseAtomWithComments(afterColon).flatMap { case (next, afterNext) =>
        val newTerms = updatedTerms :+ next
        debug(s"parseRest: After parsing atom after colon, terms: $newTerms")

        parseRest(next, afterNext).map { case (result, finalState) =>
          result match {
            case opSeq: OpSeq =>
              (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
            case _ =>
              (OpSeq(newTerms, None), finalState)
          }
        }
      }
    }

    def handleOperatorInRest(op: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
      val afterOp = state.advance()

      // Add operator to terms
      val updatedTerms = terms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))

      // Create a regular OpSeq if we're at the end of a function call argument or similar boundary
      if (
        afterOp.current match {
          case Right(Token.RParen(_)) | Right(Token.Comma(_)) => true
          case _                                              => false
        }
      ) {
        debug(s"parseRest: Added operator $op at argument boundary, terms: $updatedTerms")
        buildOpSeq(updatedTerms).map(result => (result, afterOp))
      } else {
        // Continue parsing the rest of the expression
        parseAtomWithComments(afterOp).flatMap { case (next, afterNext) =>
          debug(s"parseRest: After parsing atom after operator, got: $next")
          val newTerms = updatedTerms :+ next
          debug(s"parseRest: Updated terms after operator: $newTerms")

          // Continue parsing the rest
          parseRest(next, afterNext).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(newTerms, None), finalState)
            }
          }
        }
      }
    }

    def handleIdentifierInRest(text: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
      val afterId = state.advance()

      afterId.current match {
        case Right(Token.LParen(_)) => {
          debug("parseRest: Found lparen after identifier")
          parseTuple(afterId).flatMap { case (tuple, afterTuple) =>
            val functionCall = FunctionCall(
              ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
              tuple,
              createMeta(Some(sourcePos), Some(sourcePos))
            )
            val updatedTerms = terms :+ functionCall
            debug(s"parseRest: After function call, terms: $updatedTerms")

            parseRest(functionCall, afterTuple).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(updatedTerms, None), finalState)
              }
            }
          }
        }
        case Right(Token.LBrace(_)) => {
          debug("parseRest: Found lbrace after identifier")
          parseBlockWithComments(afterId).flatMap { case (block, afterBlock) =>
            // In V1 parser, a block after an identifier in infix is treated as part of the OpSeq
            val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
            val updatedTerms = terms :+ id :+ block
            debug(s"parseRest: After block in infix, terms: $updatedTerms")

            parseRest(block, afterBlock).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(updatedTerms, None), finalState)
              }
            }
          }
        }
        case Right(Token.Operator(op, opSourcePos)) => {
          debug(s"parseRest: Found operator $op after identifier")
          val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
          val opId = ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
          val updatedTerms = terms :+ id :+ opId
          debug(s"parseRest: After adding id and op, terms: $updatedTerms")

          val afterOp = afterId.advance()
          parseAtomWithComments(afterOp).flatMap { case (next, afterNext) =>
            val newTerms = updatedTerms :+ next
            debug(s"parseRest: After parsing atom after operator, terms: $newTerms")

            parseRest(next, afterNext).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(newTerms, None), finalState)
              }
            }
          }
        }
        case _ => {
          debug(s"parseRest: Found bare identifier $text")
          val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
          val updatedTerms = terms :+ id
          debug(s"parseRest: After adding bare id, terms: $updatedTerms")

          parseRest(id, afterId).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(updatedTerms, None), finalState)
            }
          }
        }
      }
    }

    // Main parseExpr logic
    current.current match {
      case Right(Token.Operator(op, sourcePos)) => {
        debug(s"parseExpr: Starting with operator $op")
        val afterOp = current.advance()
        afterOp.current match {
          case Right(Token.LParen(_)) => {
            debug("parseExpr: Found lparen after initial operator")
            parseTuple(afterOp).map { case (tuple, afterTuple) =>
              (
                FunctionCall(
                  ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))),
                  tuple,
                  createMeta(Some(sourcePos), Some(sourcePos))
                ),
                afterTuple
              )
            }
          }
          case _ => {
            debug("parseExpr: Parsing atom after initial operator")
            // Create an operator term
            terms = Vector(ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))))
            parseAtomWithComments(afterOp).flatMap { case (expr, afterExpr) =>
              terms = terms :+ expr
              debug(s"parseExpr: After initial operator and atom, terms: $terms")

              if (afterExpr.isAtTerminator) {
                debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
                Right((OpSeq(terms, None), afterExpr))
              } else {
                parseRest(expr, afterExpr)
              }
            }
          }
        }
      }
      case Right(Token.Identifier(chars, sourcePos)) if strIsOperator(charsToString(chars)) => {
        // Handle keyword operators like "not", "if", etc. as prefix operators
        debug(s"parseExpr: Starting with keyword operator ${charsToString(chars)}")
        val afterOp = current.advance()
        // Create an operator term
        terms = Vector(ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos))))
        parseAtomWithComments(afterOp).flatMap { case (expr, afterExpr) =>
          terms = terms :+ expr
          debug(s"parseExpr: After initial keyword operator and atom, terms: $terms")

          if (afterExpr.isAtTerminator) {
            debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
            Right((OpSeq(terms, None), afterExpr))
          } else {
            parseRest(expr, afterExpr)
          }
        }
      }
      case _ => {
        debug("parseExpr: Starting with atom")
        parseAtomWithComments(current).flatMap { case (first, afterFirst) =>
          debug(s"parseExpr: After initial atom, got: $first")
          parseRest(first, afterFirst)
        }
      }
    }
  }

  def handleDotCall(dotSourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
    val afterDot = state.advance() // Skip the dot
    afterDot.current match {
      case Right(Token.Identifier(chars1, idSourcePos1)) => {
        val afterId = afterDot.advance()
        val field = createIdentifier(chars1, idSourcePos1)
        var telescope = Vector.empty[Tuple]

        def parseNextTelescope(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
          state.current match {
            case Right(Token.LParen(_)) => {
              parseTuple(state).flatMap { case (args, afterArgs) =>
                telescope = telescope :+ args
                parseNextTelescope(afterArgs)
              }
            }
            case Right(Token.LBrace(_)) => {
              parseBlock(state).flatMap { case (block, afterBlock) =>
                telescope = telescope :+ Tuple(Vector(block), None)
                parseNextTelescope(afterBlock)
              }
            }
            case Right(Token.Dot(nextDotSourcePos)) => {
              val dotCall = createDotCall(terms.last, field, telescope, Some(dotSourcePos), Some(state.sourcePos))
              handleDotCall(nextDotSourcePos, state, Vector(dotCall))
            }
            case _ => {
              Right((createDotCall(terms.last, field, telescope, Some(dotSourcePos), Some(state.sourcePos)), state))
            }
          }
        }

        parseNextTelescope(afterId)
      }
      case Right(Token.Operator(op, idSourcePos)) => {
        val afterOp = afterDot.advance()
        val field = ConcreteIdentifier(op, createMeta(Some(idSourcePos), Some(idSourcePos)))
        afterOp.current match {
          case Right(Token.LParen(_)) => {
            parseTuple(afterOp).map { case (args, afterArgs) =>
              (createDotCall(terms.last, field, Vector(args), Some(dotSourcePos), Some(afterArgs.sourcePos)), afterArgs)
            }
          }
          case _ => {
            Right((createDotCall(terms.last, field, Vector.empty, Some(dotSourcePos), Some(idSourcePos)), afterOp))
          }
        }
      }
      case Right(t)  => Left(ParseError("Expected identifier or operator after '.'", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  def handleOperator(op: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
    // Advance once and parse the next atom
    parseAtomWithComments(state.advance()).flatMap { case (next, newState) =>
      val updatedTerms = terms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))) :+ next
      newState.current match {
        case Right(Token.Operator(nextOp, nextSourcePos)) => {
          handleOperator(nextOp, nextSourcePos, newState, updatedTerms)
        }
        case _ => Right((next, newState))
      }
    }
  }

  def parseAtom(current: LexerState): Either[ParseError, (Expr, LexerState)] = {
    current.current match {
      case Left(err)         => Left(err)
      case LBrace(sourcePos) =>
        // Check for empty object or block
        current.advance().current match {
          case Left(err)                        => Left(err)
          case RBrace(_)                        => parseObject(current) // Empty object
          case Id(_, _) | Sym(_, _) | Str(_, _) =>
            // Look ahead for = or => to determine if it's an object
            val afterId = current.advance().advance()
            afterId.current match {
              case Left(err)                            => Left(err)
              case Op(op, _) if op == "=" || op == "=>" => parseObject(current)
              case _                                    => parseBlockWithComments(current)
            }
          case _ => parseBlockWithComments(current)
        }

      case LParen(sourcePos) => parseTuple(current)

      case Id(chars, sourcePos) =>
        val afterId = current.advance()
        afterId.current match {
          case LBracket(_) =>
            // Generic type parameters
            val identifier = createIdentifier(chars, sourcePos)
            parseListWithComments(afterId).flatMap { case (typeParams, afterTypeParams) =>
              afterTypeParams.current match {
                case LParen(_) =>
                  // Function call with generic type args
                  parseTuple(afterTypeParams).map { case (tuple, afterArgs) =>
                    val typeParamsList = typeParams match {
                      case list: ListExpr => list
                      case other          => throw new RuntimeException(s"Expected ListExpr but got ${other.getClass.getSimpleName}")
                    }
                    val typeCall = createFunctionCallWithTypeParams(identifier, typeParamsList, Some(sourcePos), Some(afterTypeParams.sourcePos))
                    (createFunctionCall(typeCall, tuple, Some(sourcePos), Some(afterArgs.sourcePos)), afterArgs)
                  }
                case _ =>
                  // Just the generic type parameters
                  val typeParamsList = typeParams match {
                    case list: ListExpr => list
                    case other          => throw new RuntimeException(s"Expected ListExpr but got ${other.getClass.getSimpleName}")
                  }
                  Right(
                    (createFunctionCallWithTypeParams(identifier, typeParamsList, Some(sourcePos), Some(afterTypeParams.sourcePos)), afterTypeParams)
                  )
              }
            }
          case LParen(_) =>
            // Regular function call
            val identifier = createIdentifier(chars, sourcePos)
            parseFunctionCallWithId(identifier, afterId)
          case _ =>
            // Plain identifier
            Right((createIdentifier(chars, sourcePos), afterId))
        }

      case Int(value, sourcePos) => parseInt(current).asInstanceOf[Either[ParseError, (Expr, LexerState)]]
      case Rat(value, sourcePos) => parseRational(current).asInstanceOf[Either[ParseError, (Expr, LexerState)]]
      case Str(chars, sourcePos) => parseString(current)
      case Sym(value, sourcePos) => parseSymbol(current)

      case LBracket(sourcePos) => parseListWithComments(current)

      case Right(token) => Left(ParseError(s"Unexpected token: $token", token.sourcePos.range.start))

      case Err(error) => Left(error)
    }
  }

  // Helper method to check for expected delimiter tokens and advance
  private def expectDelimiter(state: LexerState, expectedType: Token => Boolean, errorMsg: String): Either[ParseError, LexerState] = {
    state.current match {
      case Right(token) if expectedType(token) => Right(state.advance())
      case Right(token)                        => Left(ParseError(errorMsg, token.sourcePos.range.start))
      case Left(err)                           => Left(err)
    }
  }

  // Helper method to check if a token is a terminator (right delimiter or comma/semicolon)
  private def isTerminator(token: Token): Boolean = token match {
    case _: Token.RParen | _: Token.RBrace | _: Token.RBracket | _: Token.Comma | _: Token.Semicolon | _: Token.EOF => true
    case _                                                                                                          => false
  }

  // Helper to check if current state has a terminator
  private def isAtTerminator(state: LexerState): Boolean = state.current match {
    case Right(token) => isTerminator(token)
    case _            => false
  }

  // Helper method to check if a token is specifically a right delimiter
  private def isRightDelimiter(token: Token): Boolean = token match {
    case _: Token.RParen | _: Token.RBrace | _: Token.RBracket => true
    case _                                                     => false
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    // Replace skipComments with collectComments to preserve comments
    val (leadingListComments, initialState) = collectComments(state)

    @scala.annotation.tailrec
    def parseElements(current: LexerState, exprs: Vector[Expr], maxExprs: Int): Either[ParseError, (Vector[Expr], LexerState)] = {
      if (exprs.length >= maxExprs) {
        Left(ParseError(s"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", state.sourcePos.range.start))
      } else {
        debug(s"Iteration ${exprs.length + 1}: maxExprs=$maxExprs, current token=${current.current}")
        current.current match {
          case Right(token) if isRightDelimiter(token) => {
            debug("Found right delimiter after expression")
            Right((exprs, current))
          }
          case Right(_: Token.Comment | _: Token.Whitespace) => {
            // Collect comments instead of skipping them
            val (_, afterComments) = collectComments(current)
            parseElements(afterComments, exprs, maxExprs)
          }
          case Right(_: Token.Comma | _: Token.Semicolon) => {
            debug("Found comma or semicolon, skipping")
            // Collect any comments after comma/semicolon
            val (_, afterDelimiter) = collectComments(current.advance())
            parseElements(afterDelimiter, exprs, maxExprs)
          }
          case _ => {
            debug("Parsing expression")
            parseExpr(current) match {
              case Left(err)                => Left(err)
              case Right((expr, afterExpr)) =>
                // Collect comments after the expression
                val (trailingComments, afterComments) = collectComments(afterExpr)

                // Check if we've reached a terminator
                afterComments.current match {
                  case Right(token) if isRightDelimiter(token) =>
                    // Attach trailing comments to the expression if any
                    val updatedExpr = if (trailingComments.nonEmpty) {
                      expr.updateMeta { meta =>
                        val newMeta = createMetaWithComments(
                          meta.flatMap(_.sourcePos),
                          Vector.empty,
                          trailingComments
                        )
                        // Merge with existing meta
                        mergeMeta(meta, newMeta)
                      }
                    } else {
                      expr
                    }
                    Right((exprs :+ updatedExpr, afterComments))

                  case Right(_: Token.Comma | _: Token.Semicolon) => {
                    debug("Found comma or semicolon after expression")
                    // Attach trailing comments to the expression if any
                    val updatedExpr = if (trailingComments.nonEmpty) {
                      expr.updateMeta { meta =>
                        val newMeta = createMetaWithComments(
                          meta.flatMap(_.sourcePos),
                          Vector.empty,
                          trailingComments
                        )
                        // Merge with existing meta
                        mergeMeta(meta, newMeta)
                      }
                    } else {
                      expr
                    }
                    val (_, afterDelimiter) = collectComments(afterComments.advance())
                    parseElements(afterDelimiter, exprs :+ updatedExpr, maxExprs)
                  }

                  case _ =>
                    // We haven't reached a terminator, treat this as a parsing error
                    Left(ParseError("Expected delimiter after expression", afterComments.sourcePos.range.start))
                }
            }
          }
        }
      }
    }

    parseElements(initialState, Vector.empty, LexerV2.MAX_LIST_ELEMENTS)
  }

  def parseTuple(state: LexerState): Either[ParseError, (Tuple, LexerState)] = state.current match {
    case LParen(sourcePos) =>
      val (leadingComments, afterLParen) = collectComments(state.advance())
      for {
        (exprs, afterExprs) <- parseExprList(afterLParen)
        (trailingComments, afterList) = collectComments(afterExprs)
        result <- afterList.current match {
          case RParen(_) =>
            val meta = (leadingComments.nonEmpty || trailingComments.nonEmpty) match {
              case true =>
                createMeta(Some(sourcePos), Some(afterList.sourcePos))
                  .map(m => ExprMeta(m.sourcePos, createCommentInfo(leadingComments, trailingComments)))
              case false => createMeta(Some(sourcePos), Some(afterList.sourcePos))
            }
            Right((Tuple(exprs, meta), afterList.advance()))
          case _ => Left(expectedError("right parenthesis", afterList.current))
        }
      } yield result
    case _ => Left(expectedError("left parenthesis", state.current))
  }

  def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    // IMPORTANT: Enable newLineAfterBlockMeansEnds for all blocks
    // This preserves uniform treatment without special-casing any operators
    val contextState = state.withNewLineTermination(true)

    // Replace skipComments with collectComments
    val (_, current) = collectComments(contextState)
    var statements = Vector[Expr]()
    var result: Option[Expr] = None
    var maxExpressions = 100 // Prevent infinite loops

    // Skip the opening brace
    current.current match {
      case Right(Token.LBrace(sourcePos)) => {
        // Use collectComments instead of skipComments
        val (blockStartComments, afterBrace) = collectComments(current.advance())

        // Create a local state to track comments and expressions
        var blockCurrent = afterBrace

        // Regular block parsing - no special case for "case"
        while (maxExpressions > 0) {
          maxExpressions -= 1
          blockCurrent.current match {
            case Right(Token.RBrace(_)) => {
              val finalBlock = Block(statements, result, None)
              blockCurrent = blockCurrent.advance()
              return Right((finalBlock, blockCurrent))
            }
            case Right(Token.Semicolon(_)) => {
              // Collect any comments after semicolon
              val (_, afterSemi) = collectComments(blockCurrent.advance())
              blockCurrent = afterSemi
            }
            case Right(Token.Whitespace(_, _)) => {
              // Collect comments instead of skipping
              val (_, afterWs) = collectComments(blockCurrent.advance())
              blockCurrent = afterWs
            }
            case _ => {
              parseExpr(blockCurrent) match {
                case Left(err) => return Left(err)
                case Right((expr, next)) => {
                  next.current match {
                    case Right(Token.RBrace(_)) => {
                      // This is the V1 style: put the last expression in the result field
                      // to match the V1 parser's behavior for blocks
                      result = Some(expr)
                      blockCurrent = next.advance()
                      return Right((Block(statements, result, None), blockCurrent))
                    }
                    case Right(Token.Semicolon(_)) | Right(Token.Whitespace(_, _)) => {
                      // Add the expression to statements for all but the last one
                      statements = statements :+ expr
                      // Collect comments after statement
                      val (_, afterStmt) = collectComments(next)
                      blockCurrent = afterStmt
                    }
                    case Right(t)  => return Left(ParseError("Expected ';', whitespace, or '}' after expression in block", t.sourcePos.range.start))
                    case Left(err) => return Left(err)
                  }
                }
              }
            }
          }
        }
        Left(ParseError("Too many expressions in block", current.sourcePos.range.start))
      }
      case Right(t)  => return Left(ParseError("Expected '{' at start of block", t.sourcePos.range.start))
      case Left(err) => return Left(err)
    }
  }

  def parseObject(initialState: LexerState): Either[ParseError, (ObjectExpr, LexerState)] = {
    // Collect comments before the object
    val (leadingComments, current) = collectComments(initialState)

    current.current match {
      case Right(Token.LBrace(sourcePos)) => {
        // Collect comments after the opening brace
        val (afterBraceComments, afterBrace) = collectComments(current.advance())

        def parseFields(state: LexerState, clauses: Vector[ObjectClause]): Either[ParseError, (Vector[ObjectClause], LexerState)] = {
          state.current match {
            case Right(Token.RBrace(_)) =>
              Right((clauses, state))
            case Right(Token.Identifier(chars, idSourcePos)) =>
              val identifier = ConcreteIdentifier(charsToString(chars), createMeta(Some(idSourcePos), Some(idSourcePos)))
              parseField(state.advance(), identifier, idSourcePos).flatMap { case (clause, nextState) =>
                checkAfterField(nextState).flatMap { nextStateAfterComma =>
                  parseFields(nextStateAfterComma, clauses :+ clause)
                }
              }
            case Right(Token.StringLiteral(chars, strSourcePos)) =>
              val stringLiteral = ConcreteStringLiteral(charsToString(chars), createMeta(Some(strSourcePos), Some(strSourcePos)))
              parseField(state.advance(), stringLiteral, strSourcePos).flatMap { case (clause, nextState) =>
                checkAfterField(nextState).flatMap { nextStateAfterComma =>
                  parseFields(nextStateAfterComma, clauses :+ clause)
                }
              }
            case Right(Token.SymbolLiteral(value, symSourcePos)) =>
              val symbolLiteral = chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(symSourcePos), Some(symSourcePos)))
              parseField(state.advance(), symbolLiteral, symSourcePos).flatMap { case (clause, nextState) =>
                checkAfterField(nextState).flatMap { nextStateAfterComma =>
                  parseFields(nextStateAfterComma, clauses :+ clause)
                }
              }
            case Right(t) =>
              Left(ParseError("Expected identifier, string literal, symbol literal or '}' in object", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }
        }

        def parseField(state: LexerState, key: Expr, keySourcePos: SourcePos): Either[ParseError, (ObjectClause, LexerState)] = {
          state.current match {
            case Right(Token.Operator(op, _)) => {
              val afterOp = state.advance()
              parseExpr(afterOp).flatMap { case (value, afterValue) =>
                if (op == "=>") {
                  Right((ObjectExprClauseOnValue(key, value), afterValue))
                } else { // op == "="
                  // For string literals with "=", convert to identifier
                  key match {
                    case stringLit: ConcreteStringLiteral =>
                      val idKey = ConcreteIdentifier(stringLit.value, createMeta(Some(keySourcePos), Some(keySourcePos)))
                      Right((ObjectExprClause(idKey, value), afterValue))
                    case qualifiedName: QualifiedName =>
                      Right((ObjectExprClause(qualifiedName, value), afterValue))
                    case other =>
                      // This case should never happen due to validation in caller
                      Left(ParseError(s"Expected identifier for object field key with = operator but got: $other", keySourcePos.range.start))
                  }
                }
              }
            }
            case Right(t) =>
              Left(ParseError("Expected operator in object field", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }
        }

        def checkAfterField(state: LexerState): Either[ParseError, LexerState] = {
          state.current match {
            case Right(Token.Comma(_)) => {
              // Collect comments after comma
              val (_, afterComma) = collectComments(state.advance())
              Right(afterComma)
            }
            case Right(Token.RBrace(_)) =>
              Right(state)
            case Right(t) =>
              Left(ParseError("Expected ',' or '}' after object field", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }
        }

        parseFields(afterBrace, Vector.empty).flatMap { case (clauses, finalState) =>
          finalState.current match {
            case Right(Token.RBrace(endPos)) => {
              // Create meta with comments
              val objectMeta = if (leadingComments.nonEmpty || afterBraceComments.nonEmpty) {
                val meta = createMeta(Some(sourcePos), Some(endPos))
                meta.map(m => ExprMeta(m.sourcePos, createCommentInfo(leadingComments ++ afterBraceComments)))
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }

              Right((ObjectExpr(clauses, objectMeta), finalState.advance()))
            }
            case Right(t) =>
              Left(ParseError("Expected '}' at end of object", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }
        }
      }
      case Right(t) =>
        Left(ParseError("Expected '{' at start of object", t.sourcePos.range.start))
      case Left(err) =>
        Left(err)
    }
  }

  def parseList(state: LexerState): Either[ParseError, (ListExpr, LexerState)] = {
    val (leadingComments, initialState) = collectComments(state)

    initialState.current match {
      case LBracket(sourcePos) =>
        val (afterBracketComments, afterBracket) = collectComments(initialState.advance())

        @scala.annotation.tailrec
        def parseElements(current: LexerState, exprs: Vector[Expr]): Either[ParseError, (Vector[Expr], LexerState)] = {
          if (exprs.length >= LexerV2.MAX_LIST_ELEMENTS) {
            Left(ParseError(s"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", sourcePos.range.start))
          } else
            current.current match {
              case RBracket(_) => Right((exprs, current))
              case Comma(_) =>
                val (_, afterComma) = collectComments(current.advance())
                parseElements(afterComma, exprs)
              case Right(Token.Comment(_, _)) | Right(Token.Whitespace(_, _)) =>
                val (_, afterComments) = collectComments(current)
                parseElements(afterComments, exprs)
              case _ =>
                parseExpr(current) match {
                  case Left(err) => Left(err)
                  case Right((expr, afterExpr)) =>
                    val (_, afterComments) = collectComments(afterExpr)
                    afterComments.current match {
                      case RBracket(_) => Right((exprs :+ expr, afterComments))
                      case Comma(_) =>
                        val (_, afterComma) = collectComments(afterComments.advance())
                        parseElements(afterComma, exprs :+ expr)
                      case _ => Left(expectedError("',' or ']' in list", afterComments.current))
                    }
                }
            }
        }

        parseElements(afterBracket, Vector.empty).flatMap { case (exprs, finalState) =>
          finalState.current match {
            case RBracket(endPos) =>
              val listMeta = if (leadingComments.nonEmpty || afterBracketComments.nonEmpty) {
                createMeta(Some(sourcePos), Some(endPos))
                  .map(m => ExprMeta(m.sourcePos, createCommentInfo(leadingComments ++ afterBracketComments)))
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }
              Right((ListExpr(exprs, listMeta), finalState.advance()))
            case _ => Left(expectedError("']' at end of list", finalState.current))
          }
        }
      case _ => Left(expectedError("[", initialState.current))
    }
  }

  def collectIdentifier(state: LexerState): (Vector[StringChar], LexerState) = {
    @scala.annotation.tailrec
    def collectRec(current: LexerState, chars: Vector[StringChar]): (Vector[StringChar], LexerState) = {
      if (!current.isAtEnd && current.current.exists(token => token.isInstanceOf[Token.Identifier])) {
        current.current match {
          case Right(id: Token.Identifier) =>
            collectRec(current.advance(), chars ++ id.parts)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have an Identifier token")
        }
      } else {
        (chars, current)
      }
    }

    collectRec(state, Vector.empty)
  }

  def isIdentifier(token: Either[ParseError, Token]): Boolean = token match {
    case Right(token) => { token.isInstanceOf[Token.Identifier] }
    case _            => { false }
  }

  def expectIdentifier(expected: String, state: LexerState): Either[ParseError, LexerState] = {
    state.current match {
      case Right(Token.Identifier(chars, _)) if charsToString(chars) == expected => {
        Right(state.advance())
      }
      case other => {
        Left(ParseError(s"Expected identifier '$expected' but got $other", state.sourcePos.range.start))
      }
    }
  }

  def skipComments(state: LexerState): LexerState = {
    @scala.annotation.tailrec
    def skipRec(current: LexerState): LexerState = {
      if (current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
        skipRec(current.advance())
      } else {
        current
      }
    }

    skipRec(state)
  }

  /** Collects comments from the current state. Returns a tuple of (collected comments, updated state).
    */
  def collectComments(state: LexerState): (Vector[chester.syntax.concrete.Comment], LexerState) = {
    @scala.annotation.tailrec
    def collectRec(current: LexerState, comments: Vector[chester.syntax.concrete.Comment]): (Vector[chester.syntax.concrete.Comment], LexerState) = {
      if (!current.isAtEnd && current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
        current.current match {
          case Right(Token.Comment(text, sourcePos)) =>
            val commentType = if (text.trim.startsWith("//")) {
              chester.syntax.concrete.CommentType.OneLine
            } else {
              chester.syntax.concrete.CommentType.MultiLine
            }

            val comment = chester.syntax.concrete.Comment(
              content = text.trim,
              typ = commentType,
              sourcePos = Some(sourcePos)
            )
            collectRec(current.advance(), comments :+ comment)
          case Right(Token.Whitespace(_, _)) =>
            // In Whitespace tokens, we don't have the actual text content
            // Just advance the token - we'll hit another token eventually
            collectRec(current.advance(), comments)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        (comments, current)
      }
    }

    collectRec(state, Vector.empty)
  }

  /** Collects trailing comments after an expression until a newline or non-comment token.
    */
  def collectTrailingComments(state: LexerState): (Vector[chester.syntax.concrete.Comment], LexerState) = {
    // For trailing comments, we only collect comments that appear on the same line
    // (until we hit a newline in whitespace)
    @scala.annotation.tailrec
    def collectRec(
        current: LexerState,
        comments: Vector[chester.syntax.concrete.Comment],
        hitNewline: Boolean
    ): (Vector[chester.syntax.concrete.Comment], LexerState) = {
      if (
        !current.isAtEnd && !hitNewline &&
        current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])
      ) {
        current.current match {
          case Right(Token.Comment(text, sourcePos)) =>
            val commentType = if (text.trim.startsWith("//")) {
              chester.syntax.concrete.CommentType.OneLine
            } else {
              chester.syntax.concrete.CommentType.MultiLine
            }

            val comment = chester.syntax.concrete.Comment(
              content = text.trim,
              typ = commentType,
              sourcePos = Some(sourcePos)
            )
            collectRec(current.advance(), comments :+ comment, hitNewline)
          case Right(Token.Whitespace(_, _)) =>
            // In Whitespace tokens, we don't have the actual text content
            // Just assume any whitespace might contain a newline and stop collecting
            collectRec(current.advance(), comments, true)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        (comments, current)
      }
    }

    collectRec(state, Vector.empty, false)
  }

  /** Creates ExprMeta with comments.
    */
  def createMetaWithComments(
      sourcePos: Option[SourcePos],
      leadingComments: Vector[chester.syntax.concrete.Comment] = Vector.empty,
      trailingComments: Vector[chester.syntax.concrete.Comment] = Vector.empty
  ): Option[ExprMeta] =
    Option.when(sourcePos.nonEmpty || leadingComments.nonEmpty || trailingComments.nonEmpty) {
      ExprMeta(sourcePos, createCommentInfo(leadingComments, trailingComments))
    }

  // Helper for creating function calls
  private def createFunctionCall(
      func: Expr,
      args: Expr,
      startSourcePos: Option[SourcePos],
      endSourcePos: Option[SourcePos]
  ): FunctionCall = {
    FunctionCall(
      func,
      args.asInstanceOf[Tuple],
      createMeta(startSourcePos, endSourcePos)
    )
  }

  // Special version for type parameters
  private def createFunctionCallWithTypeParams(
      func: Expr,
      typeParams: ListExpr,
      startSourcePos: Option[SourcePos],
      endSourcePos: Option[SourcePos]
  ): FunctionCall = {
    FunctionCall(
      func,
      typeParams,
      createMeta(startSourcePos, endSourcePos)
    )
  }

  // Parse a function call with the given identifier
  def parseFunctionCallWithId(
      identifier: ConcreteIdentifier,
      state: LexerState
  ): Either[ParseError, (FunctionCall, LexerState)] = {
    state.current match {
      case LParen(_) =>
        parseTuple(state).map { case (args, afterArgs) =>
          val funcSourcePos = identifier.meta.flatMap(_.sourcePos)
          (createFunctionCall(identifier, args, funcSourcePos, Some(afterArgs.sourcePos)), afterArgs)
        }
      case _ =>
        Left(ParseError("Expected left parenthesis for function call", state.sourcePos.range.start))
    }
  }

  /** Generic parser combinator that adds comment handling to any parse method */
  private def withComments[T <: Expr](
      parseMethod: LexerState => Either[ParseError, (T, LexerState)]
  )(state: LexerState): Either[ParseError, (T, LexerState)] = {
    // Collect leading comments
    val (leadingComments, afterLeadingComments) = collectComments(state)

    // Parse the expression using the provided method
    parseMethod(afterLeadingComments).flatMap { case (expr, afterExpr) =>
      // Collect trailing comments
      val (trailingComments, finalState) = collectTrailingComments(afterExpr)

      // Update expression with comments
      val updatedExpr = if (leadingComments.nonEmpty || trailingComments.nonEmpty) {
        expr.updateMeta { existingMeta =>
          val newMeta = createMetaWithComments(
            existingMeta.flatMap(_.sourcePos),
            leadingComments,
            trailingComments
          )

          // Merge the existing meta with new comment information
          mergeMeta(existingMeta, newMeta)
        }
      } else {
        expr
      }

      // Cast is safe because we're only modifying metadata
      Right((updatedExpr.asInstanceOf[T], finalState))
    }
  }

  // Simplified versions using the withComments combinator
  def parseAtomWithComments(state: LexerState): Either[ParseError, (Expr, LexerState)] =
    withComments(parseAtom)(state)

  def parseBlockWithComments(state: LexerState): Either[ParseError, (Expr, LexerState)] =
    withComments(parseBlock)(state)

  def parseListWithComments(state: LexerState): Either[ParseError, (Expr, LexerState)] =
    withComments(parseList)(state)

  private def createCommentInfo(
      leadingComments: Vector[chester.syntax.concrete.Comment],
      trailingComments: Vector[chester.syntax.concrete.Comment] = Vector.empty
  ): Option[chester.syntax.concrete.CommentInfo] =
    Option.when(leadingComments.nonEmpty || trailingComments.nonEmpty) {
      chester.syntax.concrete.CommentInfo(
        commentBefore = leadingComments,
        commentInBegin = Vector.empty,
        commentInEnd = Vector.empty,
        commentEndInThisLine = trailingComments
      )
    }

  // Helper for handling common error patterns
  private def withErrorHandling[T](
      parser: LexerState => Either[ParseError, (T, LexerState)],
      errorMsg: String
  ): LexerState => Either[ParseError, (T, LexerState)] = state =>
    parser(state) match {
      case Left(err) => Left(ParseError(s"$errorMsg: ${err.message}", err.pos))
      case right     => right
    }

  // Helper that combines collecting comments and parsing with error handling
  private def withCommentsAndErrorHandling[T <: Expr](
      parser: LexerState => Either[ParseError, (T, LexerState)],
      errorMsg: String
  ): LexerState => Either[ParseError, (T, LexerState)] = state => {
    val (leadingComments, afterLeadingComments) = collectComments(state)

    withErrorHandling(parser, errorMsg)(afterLeadingComments).flatMap { case (expr, afterExpr) =>
      val (trailingComments, finalState) = collectTrailingComments(afterExpr)

      // Update expression with comments if needed
      val updatedExpr = if (leadingComments.nonEmpty || trailingComments.nonEmpty) {
        expr.updateMeta { existingMeta =>
          val newMeta = createMetaWithComments(
            existingMeta.flatMap(_.sourcePos),
            leadingComments,
            trailingComments
          )
          mergeMeta(existingMeta, newMeta)
        }
      } else {
        expr
      }

      Right((updatedExpr.asInstanceOf[T], finalState))
    }
  }

  def parseString(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    withCommentsAndErrorHandling(
      st =>
        parseLiteral(
          st,
          token =>
            PartialFunction.condOpt(token) { case Token.StringLiteral(chars, sourcePos) =>
              (charsToString(chars), sourcePos)
            },
          (value, meta) => ConcreteStringLiteral(value, meta),
          "Expected string literal"
        ),
      "Error parsing string"
    )(state)
  }

  // Create a helper method for parsing literals with common pattern
  private def parseLiteral[T <: Expr](
      state: LexerState,
      extract: Token => Option[(String, SourcePos)],
      create: (String, Option[ExprMeta]) => T,
      errorMsg: String
  ): Either[ParseError, (T, LexerState)] = {
    state.current match {
      case Right(token) =>
        extract(token) match {
          case Some((value, sourcePos)) =>
            val meta = createMeta(Some(sourcePos), Some(sourcePos))
            Right((create(value, meta), state.advance()))
          case None =>
            Left(ParseError(errorMsg, state.sourcePos.range.start))
        }
      case Left(err) => Left(err)
    }
  }

  // Helper functions for other literal types
  def parseInt(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    withCommentsAndErrorHandling(
      st =>
        parseLiteral(
          st,
          token =>
            token match {
              case Token.IntegerLiteral(value, sourcePos) =>
                // Handle different bases
                val (numStr, base) =
                  if (value.startsWith("0x")) (value.drop(2), 16)
                  else if (value.startsWith("0b")) (value.drop(2), 2)
                  else (value, 10)
                try {
                  Some((BigInt(numStr, base).toString, sourcePos))
                } catch {
                  case _: NumberFormatException => None
                }
              case _ => None
            },
          (value, meta) => ConcreteIntegerLiteral(BigInt(value), meta),
          "Expected integer literal"
        ),
      "Error parsing integer"
    )(state)
  }

  def parseRational(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    withCommentsAndErrorHandling(
      st =>
        parseLiteral(
          st,
          token =>
            token match {
              case Token.RationalLiteral(value, sourcePos) =>
                try {
                  Some((value, sourcePos))
                } catch {
                  case _: NumberFormatException => None
                }
              case _ => None
            },
          (value, meta) => ConcreteRationalLiteral(spire.math.Rational(BigDecimal(value)), meta),
          "Expected rational literal"
        ),
      "Error parsing rational number"
    )(state)
  }

  def parseSymbol(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    withCommentsAndErrorHandling(
      st =>
        parseLiteral(
          st,
          token =>
            PartialFunction.condOpt(token) { case Token.SymbolLiteral(value, sourcePos) =>
              (value, sourcePos)
            },
          chester.syntax.concrete.SymbolLiteral,
          "Expected symbol literal"
        ),
      "Error parsing symbol"
    )(state)
  }

  // Helper to create identifier expressions
  private def createIdentifier(chars: Vector[StringChar], sourcePos: SourcePos): ConcreteIdentifier = {
    ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos)))
  }

  // Parse a simple identifier (no function calls or type parameters)
  def parseIdentifier(state: LexerState): Either[ParseError, (ConcreteIdentifier, LexerState)] = {
    withCommentsAndErrorHandling(
      st =>
        st.current match {
          case Right(Token.Identifier(chars, sourcePos)) =>
            Right((createIdentifier(chars, sourcePos), st.advance()))
          case _ =>
            Left(ParseError("Expected identifier", st.sourcePos.range.start))
        },
      "Error parsing identifier"
    )(state).asInstanceOf[Either[ParseError, (ConcreteIdentifier, LexerState)]]
  }

  // Helper for creating field access expressions
  private def createDotCall(
      target: Expr,
      field: ConcreteIdentifier,
      args: Vector[Tuple] = Vector.empty,
      startSourcePos: Option[SourcePos],
      endSourcePos: Option[SourcePos]
  ): DotCall = {
    DotCall(
      target,
      field,
      args,
      createMeta(startSourcePos, endSourcePos)
    )
  }

}
