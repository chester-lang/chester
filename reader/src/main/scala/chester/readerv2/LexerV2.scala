package chester.readerv2

/*
 * ReaderV2 Design Principles:
 * 
 * 1. Separation of Concerns:
 *    - Parser only produces OpSeq without knowledge of operator semantics
 *    - Operator precedence, fixity (infix/prefix/postfix/mixfix) handled in later passes
 *    - This separation allows flexible operator definition and extension
 * 
 * 2. Error Recovery:
 *    - Designed to handle incomplete/broken source code (e.g. during editing)
 *    - Attempts to produce meaningful partial results when possible
 *    - Uses ErrorExpr to represent recoverable parse errors inline
 *    - Continues parsing after encountering errors when safe to do so
 * 
 * 3. OpSeq Design:
 *    - Raw sequence of expressions and operators
 *    - No precedence or associativity information at parse time
 *    - Later passes will restructure based on operator properties
 *    - Allows for flexible operator definition and semantics
 * 
 * 4. Incremental Parsing:
 *    - Supports partial parsing of incomplete expressions
 *    - Maintains parser state for potential incremental updates
 *    - Useful for IDE integration and live editing
 */

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break
import chester.error.{Pos, SourcePos, RangeInFile}
import chester.error.WithPos
import chester.utils.WithUTF16
import io.github.iltotore.iron.autoRefine
import chester.reader.{ParseError, SourceOffset, ParserSource}
import chester.syntax.concrete.{
  Block, Expr, ExprMeta, ExprStmt, Identifier, ListExpr, ObjectExpr,
  QualifiedName, Tuple, FunctionCall, IntegerLiteral, RationalLiteral, StringLiteral,
  ObjectExprClauseOnValue, ObjectExprClause, ObjectClause, OpSeq, ErrorExpr, RecoverableParseError,
  MaybeTelescope
}
import chester.syntax.concrete.Literal.*
import spire.math.Rational
import chester.reader.FileNameAndContent
import chester.syntax.IdentifierRules.strIsOperator
import scala.reflect.ClassTag

case class StmtExpr(expr: Expr)

case class LexerState(
  tokens: Vector[Either[ParseError, Token]],
  index: Int
) {
  def current: Either[ParseError, Token] = tokens(index)
  def isAtEnd: Boolean = index >= tokens.length
  def advance(): LexerState = LexerState(tokens, index + 1)
  def sourcePos: SourcePos = current match {
    case Left(err) => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
    case Right(t) => t.sourcePos
  }
  override def toString: String = s"LexerState(index=$index, current=$current, remaining=${tokens.length - index} tokens)"
}

object LexerV2 {
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false): LexerV2 =
    new LexerV2(tokens, sourceOffset, ignoreLocation)
    
  var DEBUG = false // Global debug flag
  private var globalRecursionDepth = 0 // Track global recursion depth
  private var maxRecursionDepth = 0 // Track maximum recursion depth reached
  private var methodCallCounts = Map[String, Int]() // Track number of calls per method
}

class LexerV2(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean) {
  import LexerV2.{DEBUG, globalRecursionDepth, maxRecursionDepth, methodCallCounts}
  
  private def debug(msg: => String): Unit = if (DEBUG) {
    val indent = " " * globalRecursionDepth
    println(s"[DEBUG]$indent $msg")
    System.out.flush()
  }
  
  private def debugState(label: String): Unit = if (DEBUG) {
    debug(s"$label:")
    debug(s"  Current state: $state")
    debug(s"  Loop count: $loopCount")
    debug(s"  Recursion depth: $globalRecursionDepth")
    debug(s"  Max recursion depth: $maxRecursionDepth")
    debug(s"  Method call counts: ${methodCallCounts.mkString(", ")}")
    System.out.flush()
  }
  
  private var state: LexerState = LexerState(tokens.toVector, 0)
  private var loopCount = 0 // Track loop iterations
  
  private def withRecursion[T](name: String)(f: => T): T = {
    val MaxRecursionDepth = 100 // Add reasonable limit
    try {
      if (globalRecursionDepth >= MaxRecursionDepth) {
        throw new RuntimeException(s"Maximum recursion depth ($MaxRecursionDepth) exceeded")
      }
      globalRecursionDepth += 1
      maxRecursionDepth = math.max(maxRecursionDepth, globalRecursionDepth)
      methodCallCounts = methodCallCounts.updated(name, methodCallCounts.getOrElse(name, 0) + 1)
      
      debug(s"ENTER $name (depth=$globalRecursionDepth, calls=${methodCallCounts(name)})")
      debugState(s"Before $name")
      
      val result = f
      debug(s"EXIT $name with result=$result")
      debugState(s"After $name")
      result
    } catch {
      case e: Throwable =>
        debug(s"ERROR in $name: ${e.getMessage}")
        throw e
    } finally {
      globalRecursionDepth -= 1
    }
  }

  private def createMeta(startPos: Option[SourcePos], endPos: Option[SourcePos]): Option[ExprMeta] = {
    if (ignoreLocation) {
      None
    } else {
      (startPos, endPos) match {
        case (Some(start), Some(end)) =>
          Some(ExprMeta(Some(SourcePos(sourceOffset, RangeInFile(start.range.start, end.range.end))), None))
        case (Some(pos), _) =>
          Some(ExprMeta(Some(pos), None))
        case (_, Some(pos)) =>
          Some(ExprMeta(Some(pos), None))
        case _ =>
          None
      }
    }
  }

  private def getSourcePos(token: Either[ParseError, Token]): SourcePos = {
    debug(s"getSourcePos: token=$token")
    token match {
      case Left(err) => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
      case Right(t) => t.sourcePos
    }
  }

  private def getStartPos(token: Either[ParseError, Token]): Pos = token match {
    case Right(t) => t.sourcePos.range.start
    case Left(err) => err.pos
  }

  private def peek(): LexerState = {
    debug(s"peek: current state=$state")
    state
  }

  private def advance(): LexerState = {
    if (state.index + 1 >= state.tokens.length) {
      LexerState(state.tokens, state.index + 1)
    } else {
      LexerState(state.tokens, state.index + 1)
    }
  }

  // Design Notes for Uniform Operator/Identifier Handling:
  // 
  // 1. No Special Cases:
  //    - All identifiers and operators are treated uniformly in parsing
  //    - IMPORTANT: No special cases for keywords like "if", "then", "else" - they are just identifiers
  //    - No predefined keywords (if/then/else/val) or operators (+/-/*)
  //    - No special parsing rules for any identifiers
  //    - Semantic meaning determined in later passes
  //    - Examples:
  //      - Traditional: if x then y else z
  //      - Custom: myIf x myThen y myElse z
  //      - Both parse to: OpSeq([identifier, expr, identifier, expr, identifier, expr])
  //      - Keywords like "if", "then", "else" are treated exactly the same as any other identifier
  // 
  // 2. Operator/Identifier Rules:
  //    - Operators start with operator symbols (.:=-+\|<>/?`~!@$%^&*)
  //    - Identifiers start with letters/emoji/underscore
  //    - Both can contain operator symbols and word symbols
  //    - See IdentifierRules.scala for complete rules
  //    - IMPORTANT: We use IdentifierRules.strIsOperator for uniform operator identification
  //    - NO local redefinition of operator rules to ensure consistency
  //    - Keywords are NOT special - they follow the same rules as any other identifier
  // 
  // 3. Sequence Construction:
  //    - All terms form a uniform sequence: expr op expr op expr ...
  //    - Structure preserved for later semantic analysis
  //    - Examples:
  //      - 1 + 2 -> OpSeq([1, +, 2])
  //      - if x then y -> OpSeq([if, x, then, y])  // "if" and "then" are just identifiers
  //      - val x = 1 -> OpSeq([val, x, =, 1])      // "val" is just an identifier
  //      - myOp1 x myOp2 y -> OpSeq([myOp1, x, myOp2, y])
  // 
  // 4. Benefits:
  //    - Allows user-defined operators and keywords
  //    - Consistent parsing rules for all identifiers
  //    - Flexible operator definition and extension
  //    - Operator precedence and fixity handled in later passes
  //    - Supports domain-specific language extensions
  //    - Single source of truth for operator identification (IdentifierRules)
  //    - No special cases means simpler, more maintainable code
  //    - Keywords can be redefined or extended by users

  // Design Notes for Operator Sequence Parsing:
  // 
  // 1. Operators are handled uniformly through character-based identification
  // 2. No special casing of operators - determined by character patterns in IdentifierRules
  // 3. Operator sequences are built incrementally
  // 4. The sequence maintains the alternating pattern: term operator term operator ...
  // 5. Both prefix and infix operators are supported through the same mechanism
  // 6. All operator identification is delegated to IdentifierRules.strIsOperator
  //    to maintain consistency across the codebase
  type LexerError = ParseError

  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    val startPos = current.current.map(_.sourcePos).getOrElse(current.sourcePos)
    
    def buildOpSeq(terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
      current.current match {
        case Right(Token.EOF(_)) => {
          Right((terms match {
            case Vector(single) => single
            case _ => OpSeq(terms, None)
          }), current)
        }
        case Right(t @ (Token.RParen(_) | Token.RBrace(_) | Token.RBracket(_) | Token.Comma(_))) => {
          Right((terms match {
            case Vector(single) => single
            case _ => OpSeq(terms, None)
          }), current)
        }
        case Right(Token.Operator(op, sourcePos)) => {
          current = current.advance()
          parseAtom(current).flatMap { case (next, newState) => {
            current = newState
            buildOpSeq(terms :+ Identifier(op, createMeta(Some(sourcePos), Some(sourcePos))) :+ next)
          }}
        }
        case Right(Token.Equal(sourcePos)) => {
          current = current.advance()
          parseAtom(current).flatMap { case (next, newState) => {
            current = newState
            buildOpSeq(terms :+ Identifier("=", createMeta(Some(sourcePos), Some(sourcePos))) :+ next)
          }}
        }
        case Right(Token.Identifier(chars, sourcePos)) => {
          val text = chars.map(_.text).mkString
          current = current.advance()
          current.current match {
            case Right(Token.LParen(_)) => {
              parseTuple(current).flatMap { case (tuple, newState) => {
                current = newState
                buildOpSeq(terms :+ FunctionCall(
                  Identifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
                  tuple,
                  createMeta(Some(sourcePos), Some(sourcePos))
                ))
              }}
            }
            case Right(Token.LBrace(_)) => {
              parseBlock(current).flatMap { case (block, newState) => {
                current = newState
                buildOpSeq(terms :+ Identifier(text, createMeta(Some(sourcePos), Some(sourcePos))) :+ block)
              }}
            }
            case _ => {
              buildOpSeq(terms :+ Identifier(text, createMeta(Some(sourcePos), Some(sourcePos))))
            }
          }
        }
        case Right(Token.LBrace(_)) => {
          parseBlock(current).flatMap { case (block, newState) => {
            current = newState
            buildOpSeq(terms :+ block)
          }}
        }
        case Right(token) => {
          parseAtom(current).flatMap { case (next, newState) => {
            current = newState
            buildOpSeq(terms :+ next)
          }}
        }
        case Left(error) => {
          Left(error)
        }
      }
    }

    current.current match {
      case Right(Token.Operator(op, sourcePos)) => {
        current = current.advance()
        current.current match {
          case Right(Token.LParen(_)) => {
            parseTuple(current).map { case (tuple, newState) =>
              (FunctionCall(
                Identifier(op, createMeta(Some(sourcePos), Some(sourcePos))),
                tuple,
                createMeta(Some(sourcePos), Some(sourcePos))
              ), newState)
            }
          }
          case _ => parseAtom(current).flatMap { case (expr, state2) => {
            current = state2
            buildOpSeq(Vector(
              Identifier(op, createMeta(Some(sourcePos), Some(sourcePos))),
              expr
            ))
          }}
        }
      }
      case Right(Token.Identifier(chars, sourcePos)) => {
        val text = chars.map(_.text).mkString
        current = current.advance()
        current.current match {
          case Right(Token.LParen(_)) => {
            parseTuple(current).flatMap { case (tuple, newState) => {
              current = newState
              buildOpSeq(Vector(FunctionCall(
                Identifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
                tuple,
                createMeta(Some(sourcePos), Some(sourcePos))
              )))
            }}
          }
          case Right(Token.LBrace(_)) => {
            parseBlock(current).flatMap { case (block, newState) => {
              current = newState
              buildOpSeq(Vector(
                Identifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
                block
              ))
            }}
          }
          case _ => buildOpSeq(Vector(Identifier(text, createMeta(Some(sourcePos), Some(sourcePos)))))
        }
      }
      case Right(Token.LBrace(_)) => {
        parseBlock(current).flatMap { case (block, newState) => {
          current = newState
          buildOpSeq(Vector(block))
        }}
      }
      case _ => parseAtom(current).flatMap { case (expr, state2) => {
        current = state2
        buildOpSeq(Vector(expr))
      }}
    }
  }

  def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    current.current match {
      case Right(Token.IntegerLiteral(value, sourcePos)) => {
        current = current.advance()
        Right((IntegerLiteral(BigInt(value), createMeta(Some(sourcePos), Some(sourcePos))), current))
      }
      case Right(Token.StringLiteral(chars, sourcePos)) => {
        current = current.advance()
        Right((StringLiteral(chars.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))), current))
      }
      case Right(Token.Identifier(chars, sourcePos)) => {
        current = current.advance()
        current.current match {
          case Right(Token.LParen(_)) => {
            parseTuple(current).map {
              case (tuple, newState) => {
                (FunctionCall(
                  Identifier(chars.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))),
                  tuple,
                  createMeta(Some(sourcePos), Some(sourcePos))
                ), newState)
              }
            }
          }
          case _ => {
            Right((Identifier(chars.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))), current))
          }
        }
      }
      case Right(Token.LParen(_)) => {
        parseTuple(current).map {
          case (Tuple(Vector(single), _), state) => { (single, state) }
          case (tuple, state) => { (tuple, state) }
        }
      }
      case Right(Token.LBrace(_)) => {
        parseObject(current)
      }
      case Right(Token.LBracket(_)) => {
        parseList(current)
      }
      case Right(token) => {
        Left(ParseError(s"Unexpected token: $token", token.sourcePos.range.start))
      }
      case Left(error) => {
        Left(error)
      }
    }
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    var current = state
    var exprs = Vector.empty[Expr]
    var maxExprs = 50

    while (maxExprs > 0 && !current.current.exists(_.isInstanceOf[Token.RParen])) {
      if (current.index >= current.tokens.length) {
        return Left(ParseError("Unexpected end of input while parsing expressions", getStartPos(state.current)))
      }

      current.current match {
        case Right(Token.Comma(_)) => {
          current = current.advance()
        }
        case Right(Token.RParen(_)) => {
          current = current.advance()
        }
        case _ => {
          parseExpr(current).flatMap { case (expr, afterExpr) => {
            exprs = exprs :+ expr
            current = afterExpr
            current.current match {
              case Right(Token.RParen(_)) => {
                Right((exprs, current))
              }
              case Right(Token.Comma(_)) => {
                current = current.advance()
                Right(())
              }
              case Right(t) => {
                Left(ParseError("Expected ',' or ')' after expression", t.sourcePos.range.start))
              }
              case Left(err) => { Left(err) }
            }
          }}
        }
      }
      maxExprs -= 1
    }

    if (maxExprs <= 0) {
      Left(ParseError("Too many expressions", getStartPos(state.current)))
    } else {
      Right((exprs, current))
    }
  }

  private def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(sourcePos)) => {
        var current = state
        current = current.advance()
        parseExpr(current).flatMap { case (expr, afterExpr) => {
          current = afterExpr
          current.current match {
            case Right(Token.RBrace(endPos)) => {
              current = current.advance()
              Right((Block(Vector(), Some(expr), createMeta(Some(sourcePos), Some(endPos))), current))
            }
            case Right(Token.Semicolon(_)) => {
              current = current.advance()
              var statements = Vector(expr)
              var result: Option[Expr] = None
              var maxStatements = 50

              while (maxStatements > 0) {
                current.current match {
                  case Right(Token.RBrace(endPos)) => {
                    current = current.advance()
                    return Right((Block(statements, result, createMeta(Some(sourcePos), Some(endPos))), current))
                  }
                  case _ => {
                    parseExpr(current) match {
                      case Right((nextExpr, afterNext)) => {
                        current = afterNext
                        current.current match {
                          case Right(Token.Semicolon(_)) => {
                            statements = statements :+ nextExpr
                            current = current.advance()
                          }
                          case Right(Token.RBrace(endPos)) => {
                            result = Some(nextExpr)
                            current = current.advance()
                            return Right((Block(statements, result, createMeta(Some(sourcePos), Some(endPos))), current))
                          }
                          case Right(t) => {
                            return Left(ParseError("Expected ';' or '}' after expression in block", t.sourcePos.range.start))
                          }
                          case Left(err) => return Left(err)
                        }
                      }
                      case Left(err) => return Left(err)
                    }
                  }
                }
                maxStatements -= 1
              }
              Left(ParseError("Too many statements in block", sourcePos.range.start))
            }
            case Right(t) => {
              Left(ParseError("Expected '}' or ';' after expression in block", t.sourcePos.range.start))
            }
            case Left(err) => Left(err)
          }
        }}
      }
      case Right(t) => {
        Left(ParseError("Expected '{' at start of block", t.sourcePos.range.start))
      }
      case Left(err) => Left(err)
    }
  }

  private def parseTuple(state: LexerState): Either[ParseError, (Tuple, LexerState)] = {
    var current = state
    current.current match {
      case Right(Token.LParen(_)) => {
        current = current.advance()
        var exprs = Vector.empty[Expr]
        
        def parseElement(): Either[ParseError, Unit] = {
          parseExpr(current).map { case (expr, afterExpr) =>
            current = afterExpr
            exprs = exprs :+ expr
            current.current match {
              case Right(Token.Comma(_)) => {
                current = current.advance()
              }
              case Right(Token.RParen(_)) => ()
              case Right(t) => return Left(ParseError("Expected ',' or ')' in tuple", t.sourcePos.range.start))
              case Left(err) => return Left(err)
            }
          }
        }
        
        while (!current.current.exists(_.isInstanceOf[Token.RParen])) {
          parseElement() match {
            case Right(_) => ()
            case Left(err) => return Left(err)
          }
        }
        
        current.current match {
          case Right(Token.RParen(_)) => {
            current = current.advance()
            Right((Tuple(exprs, None), current))
          }
          case Right(t) => Left(ParseError("Expected ')' at end of tuple", t.sourcePos.range.start))
          case Left(err) => Left(err)
        }
      }
      case Right(t) => Left(ParseError("Expected '(' at start of tuple", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  private def parseList(state: LexerState): Either[ParseError, (ListExpr, LexerState)] = {
    var current = state
    var exprs = Vector[Expr]()
    
    current.current match {
      case Right(Token.LBracket(sourcePos)) => {
        current = current.advance()
        while (exprs.length < 50) {
          current.current match {
            case Right(Token.RBracket(endPos)) => {
              current = current.advance()
              return Right((ListExpr(exprs, createMeta(Some(sourcePos), Some(endPos))), current))
            }
            case Right(Token.Comma(_)) => {
              current = current.advance()
            }
            case _ => {
              val result = for {
                (expr, afterExpr) <- parseExpr(current)
                _ = {
                  exprs = exprs :+ expr
                  current = afterExpr
                }
                result <- current.current match {
                  case Right(Token.RBracket(endPos)) => {
                    current = current.advance()
                    Right((ListExpr(exprs, createMeta(Some(sourcePos), Some(endPos))), current))
                  }
                  case Right(Token.Comma(_)) => {
                    current = current.advance()
                    Right(())
                  }
                  case Right(t) => {
                    Left(ParseError("Expected ',' or ']' in list", t.sourcePos.range.start))
                  }
                  case Left(err) => { Left(err) }
                }
              } yield result
              
              result match {
                case Right(_) => ()
                case Left(err) => return Left(err)
              }
            }
          }
        }
        Left(ParseError("List has too many expressions (max 50)", sourcePos.range.start))
      }
      case Right(t) => Left(ParseError("Expected '[' at start of list", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  private def parseObject(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    current.current match {
      case Right(Token.LBrace(_)) => {
        current = current.advance()
        var clauses = Vector.empty[ObjectClause]
        
        def parseClause(): Either[ParseError, Unit] = {
          parseExpr(current).flatMap { case (expr, afterExpr) =>
            current = afterExpr
            current.current match {
              case Right(Token.Operator("->", _)) => {
                current = current.advance()
                parseExpr(current).map { case (value, afterValue) =>
                  current = afterValue
                  clauses = clauses :+ ObjectExprClauseOnValue(expr, value)
                  current.current match {
                    case Right(Token.Comma(_)) => {
                      current = current.advance()
                    }
                    case Right(Token.RBrace(_)) => ()
                    case Right(t) => return Left(ParseError("Expected ',' or '}' after object clause", t.sourcePos.range.start))
                    case Left(err) => return Left(err)
                  }
                }
              }
              case Right(t) => Left(ParseError("Expected '->' in object expression", t.sourcePos.range.start))
              case Left(err) => Left(err)
            }
          }
        }
        
        while (!current.current.exists(_.isInstanceOf[Token.RBrace])) {
          parseClause() match {
            case Right(_) => ()
            case Left(err) => return Left(err)
          }
        }
        
        current.current match {
          case Right(Token.RBrace(_)) => {
            current = current.advance()
            Right((ObjectExpr(clauses, None), current))
          }
          case Right(t) => Left(ParseError("Expected '}' at end of object", t.sourcePos.range.start))
          case Left(err) => Left(err)
        }
      }
      case Right(t) => Left(ParseError("Expected '{' at start of object", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  def parseObjectExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseObjectExpr") {
    var clauses = Vector.empty[ObjectClause]
    var currentState = state
    
    while (true) {
      val result = for {
        (left, afterLeft) <- parseExpr(currentState)
        result <- {
          if (afterLeft.current.exists(t => t match {
            case Token.Operator("->", _) => true
            case _ => false
          })) {
            val afterArrow = afterLeft.advance()
            for {
              (right, afterRight) <- parseExpr(afterArrow)
              _ = {
                clauses = clauses :+ ObjectExprClauseOnValue(left, right)
                if (afterRight.current.exists(t => t.isInstanceOf[Token.Comma])) {
                  currentState = afterRight.advance()
                }
              }
            } yield {
              if (afterRight.current.exists(t => t.isInstanceOf[Token.Comma])) {
                Right(())
              } else {
                Right((ObjectExpr(clauses, None), afterRight))
              }
            }
          } else {
            Left(ParseError("Expected '->' in object expression", afterLeft.sourcePos.range.start))
          }
        }
      } yield result
      
      result match {
        case Right(Right((expr: ObjectExpr, state: LexerState))) => return Right((expr, state))
        case Right(Right(())) => ()
        case Left(err) => return Left(err)
      }
    }
    
    Right((ObjectExpr(clauses, None), currentState))
  }

  def collectIdentifier(state: LexerState): (Vector[StringChar], LexerState) = {
    var chars = Vector.empty[StringChar]
    var currentState = state
    
    while (!currentState.isAtEnd && currentState.current.exists(token => token.isInstanceOf[Token.Identifier])) {
      currentState.current match {
        case Right(id: Token.Identifier) => {
          chars = chars ++ id.parts
          currentState = currentState.advance()
        }
        case _ => { throw new RuntimeException("Unreachable: exists check ensures this case never happens") }
      }
    }
    
    (chars, currentState)
  }

  def isIdentifier(token: Either[ParseError, Token]): Boolean = token match {
    case Right(token) => { token.isInstanceOf[Token.Identifier] }
    case _ => { false }
  }

  def expectIdentifier(expected: String, state: LexerState): Either[ParseError, LexerState] = {
    state.current match {
      case Right(Token.Identifier(chars, _)) if chars.map(_.text).mkString == expected => {
        Right(advance())
      }
      case other => {
        Left(ParseError(s"Expected identifier '$expected' but got $other", state.sourcePos.range.start))
      }
    }
  }

  private def expectToken[T <: Token](state: LexerState)(using tag: ClassTag[T]): Either[ParseError, LexerState] = {
    state.current match {
      case Right(token) if token.isInstanceOf[T] => Right(state.advance())
      case Right(token) => Left(ParseError(s"Expected token of type ${tag.runtimeClass.getSimpleName}", token.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }
}