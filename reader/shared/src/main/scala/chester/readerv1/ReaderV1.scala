package chester.readerv1

import chester.error.*
import chester.reader.Source
import chester.syntax.IdentifierRules.*
import chester.syntax.concrete.*
import chester.utils.parse.*
import chester.utils.{Nat, StringIndex, WithUTF16, asInt, silentDeprecated}
import fastparse.*
import fastparse.NoWhitespace.*
import spire.math.Natural

import scala.util.*

case class ReaderV1(
    source: Source,
    ignoreLocation: Boolean = false,
    defaultIndexer: Option[StringIndex] = None
)(using p: P[?]) {
  val fileName: String = source.fileName

  private def nEnd: P[Unit] = P("\n" | End)

  @silentDeprecated("comment is lost")
  def comment: P[Unit] = P("//" ~ CharPred(_ != '\n').rep ~ nEnd)

  private def commentOneLine: P[Comment] =
    PwithMeta("//" ~ CharPred(_ != '\n').rep.! ~ ("\n" | End)).map((content, meta) => Comment(content, CommentType.OneLine, meta.flatMap(_.span)))

  private def allComment: P[Comment] = P(commentOneLine)

  private def simpleDelimiter: P[Unit] = P(CharsWhileIn(" \t\r\n"))

  @silentDeprecated("comment is lost")
  def delimiter: P[Unit] = P((simpleDelimiter | comment).rep)

  private def delimiter1: P[Vector[Comment]] = P(
    (simpleDelimiter.map(x => Vector()) | allComment.map(Vector(_))).rep
  ).map(_.flatten.toVector)

  @silentDeprecated("comment is lost")
  def lineEnding: P[Unit] = P(
    comment | (CharsWhileIn(" \t\r").? ~ ("\n" | End))
  )

  def lineEnding1: P[Vector[Comment]] = P(
    commentOneLine.map(Vector(_)) | (CharsWhileIn(" \t\r").? ~ nEnd).map(x => Vector())
  )

  private def lineNonEndingSpace: P[Unit] = P(CharsWhileIn(" \t\r"))

  @silentDeprecated("comment is lost")
  def maybeSpace: P[Unit] = P(delimiter.?)

  private def maybeSpace1: P[Vector[Comment]] = P(delimiter1.?.map(_.toVector.flatten))

  private def simpleId: P[String] = P(
    (CharacterPred(isIdentifierFirst).rep(1) ~ CharacterPred(
      isIdentifierPart
    ).rep.? ~ CharacterPred(isIdentifierEnd).?).!
  )

  def id: P[String] = operatorId | simpleId

  private def operatorId: P[String] = P(
    (CharacterPred(isOperatorIdentifierFirst).rep(1) ~ CharacterPred(
      isOperatorIdentifierRest
    ).rep).!
  )

  def begin: P[spire.math.Natural] =
    Index.map(Nat(_))

  def end: P[spire.math.Natural] =
    Index.map(Nat(_))

  val indexer: StringIndex = defaultIndexer.getOrElse(StringIndex(p.input))

  private def loc(
      begin: spire.math.Natural,
      end: spire.math.Natural
  ): Option[Span] = {
    if (ignoreLocation) return None
    val start = indexer.charIndexToLineAndColumnWithUTF16(begin.asInt)
    val endPos = indexer.charIndexToLineAndColumnWithUTF16(end.asInt)
    val range = SpanInFile(
      Pos(
        WithUTF16(indexer.charIndexToUnicodeIndex(begin), begin),
        start.line,
        start.column
      ),
      Pos(
        WithUTF16(indexer.charIndexToUnicodeIndex(end), end),
        endPos.line,
        endPos.column
      )
    )
    Some(source.offset.add(Span(source, range)))
  }

  private def createMeta(
      pos: Option[Span],
      comments: Option[CommentInfo]
  ): Option[ExprMeta] =
    (pos, comments) match {
      case (None, None) => None
      case _            => Some(ExprMeta(pos, comments))
    }

  /*
  TODO: difference bettween
  x // comment
  y

  x
  // comment
  y
   */
  extension [T <: Expr](inline parse0: P[T]) {
    private inline def relax(
        inline start: Boolean = true,
        inline onEnd: Boolean = false
    ): P[T] = (start, onEnd) match {
      case (true, true) =>
        (maybeSpace1 ~ parse0 ~ maybeSpace1).map { (b, x, e) =>
          if (ignoreLocation) x
          else
            x.metaUpdated(
              MetaFactory.add(commentBefore = b, commentEndInThisLine = e)
            ).asInstanceOf[T]
        }
      case (true, false) =>
        (maybeSpace1 ~ parse0).map { (b, x) =>
          if (ignoreLocation) x
          else
            x.metaUpdated(MetaFactory.add(commentBefore = b)).asInstanceOf[T]
        }
      case (false, true) =>
        (parse0 ~ maybeSpace1).map { (x, e) =>
          if (ignoreLocation) x
          else
            x.metaUpdated(MetaFactory.add(commentEndInThisLine = e))
              .asInstanceOf[T]
        }
      case (false, false) => parse0
    }
    inline def relax1: P[T] = relax(true)
    private inline def relax2: P[T] = relax(true, true)
  }

  extension [T](inline parse0: P[T]) {
    @silentDeprecated("I forgot what is this doing seemingly wrong logic")
    inline def withMeta[R](using
        s: fastparse.Implicits.Sequencer[T, Option[ExprMeta], R]
    ): P[R] = (begin ~ parse0 ~ end).map { (b, x, e) =>
      val meta = createMeta(loc(b, e), None)
      s(x, meta)
    }

    inline def withSpaceAtStart[R](using
        s: fastparse.Implicits.Sequencer[T, Vector[Comment], R]
    ): P[R] = (maybeSpace1 ~ parse0).map((comments, x) => s(x, comments))

    private inline def must(inline message: String = "Expected something"): P[T] =
      parse0.? flatMap {
        case Some(x) => Pass(x)
        case None    => Fail.opaque(message)./
      }

    inline def on(inline condition: Boolean): P[T] =
      if condition then parse0 else Fail("")

    private inline def checkOn(inline condition: Boolean): P[Unit] =
      if condition then parse0 else Pass(())

    inline def thenTry(inline parse1: P[T]): P[T] = parse0.?.flatMap {
      case Some(result) => Pass(result)
      case None         => parse1
    }
  }

  private inline def PwithMeta[T, R](inline parse0: P[T])(using
      fastparse.Implicits.Sequencer[T, Option[ExprMeta], R]
  ): P[R] = P(parse0.withMeta)

  def identifier: P[Identifier] = P(id.withMeta).map((name, meta) => Identifier(name, meta))

  def infixIdentifier: P[Identifier] = P(operatorId.withMeta).map((name, meta) => Identifier(name, meta))

  private def signed: P[String] = P("".!) // P(CharIn("+\\-").?.!)

  private def hexLiteral: P[String] = P("0x" ~ CharsWhileIn("0-9a-fA-F").must()).!

  private def binLiteral: P[String] = P("0b" ~ CharsWhileIn("01").must()).!

  private def decLiteral: P[String] = P(CharsWhileIn("0-9")).!

  private def expLiteral: P[String] = P(
    CharsWhileIn("0-9") ~ "." ~ CharsWhileIn("0-9") ~ (CharIn(
      "eE"
    ) ~ signed ~ CharsWhileIn("0-9")).?
  ).!

  private def integerLiteral: P[ParsedExpr] =
    P(signed ~ (hexLiteral | binLiteral | decLiteral).!).withMeta.map { (sign, value, meta) =>
      val actualValue =
        if (value.startsWith("0x")) BigInt(sign + value.drop(2), 16)
        else if (value.startsWith("0b")) BigInt(sign + value.drop(2), 2)
        else BigInt(sign + value)
      IntegerLiteral(actualValue, meta)
    }

  private def doubleLiteral: P[ParsedExpr] = P(signed ~ expLiteral.withMeta).map { case (sign, (value, meta)) =>
    RationalLiteral(BigDecimal(sign + value), meta)
  }

  private def escapeSequence: P[String] = P("\\" ~ CharIn("rnt\\\"").!).map {
    case "r"  => "\r"
    case "n"  => "\n"
    case "t"  => "\t"
    case "\\" => "\\"
    case "\"" => "\""
  }

  private def normalChar: P[String] = P(CharPred(c => c != '\\' && c != '"')).!

  def stringLiteral: P[String] = P(
    "\"" ~ (normalChar | escapeSequence).rep.map(_.mkString) ~ "\""
  )

  private def heredocLiteral: P[String] = {
    def validateIndentation(str: String): Either[String, String] = {
      val lines = str.split("\n")
      val indentStrings =
        lines.withFilter(_.trim.nonEmpty).map(_.takeWhile(_.isWhitespace))

      if (indentStrings.distinct.length > 1)
        Left("Inconsistent indentation in heredoc string literal")
      else {
        val indentSize =
          if (indentStrings.nonEmpty) indentStrings.head.length else 0
        val trimmedLines = lines.map(_.drop(indentSize))
        Right(trimmedLines.mkString("\n").stripPrefix("\n").stripSuffix("\n"))
      }
    }

    P("\"\"\"" ~ (!"\"\"\"".rep ~ AnyChar).rep.!.flatMap(str => validateIndentation(str).fold(Fail.opaque, validStr => Pass(validStr))) ~ "\"\"\"")
  }

  private def stringLiteralExpr: P[ParsedExpr] =
    P((stringLiteral | heredocLiteral).withMeta).map((value, meta) => StringLiteral(value, meta))

  def literal: P[ParsedExpr] = P(
    doubleLiteral | integerLiteral | stringLiteralExpr
  )

  def simpleAnnotation: P[Identifier] = "@" ~ identifier

  @silentDeprecated
  def comma: P[Unit] = P(maybeSpace ~ "," ~ maybeSpace)

  private def comma1: P[Unit] = ","

  private def list: P[ListExpr] = PwithMeta(
    "[" ~ parse().relax2
      .rep(sep = comma1) ~ maybeSpace ~ comma1.? ~ maybeSpace ~ "]"
  ).map((terms, meta) => ListExpr(terms.toVector, meta))

  def tuple: P[Tuple] = PwithMeta(
    "(" ~ parse().relax2
      .rep(sep = comma1) ~ maybeSpace ~ comma1.? ~ maybeSpace ~ ")"
  ).map((terms, meta) => Tuple(terms.toVector, meta))

  def annotation: P[(Identifier, Vector[ParsedMaybeTelescope])] = P(
    "@" ~ identifier ~ callingZeroOrMore()
  )

  private def annotated: P[AnnotatedExpr] =
    PwithMeta(annotation ~ parse()).map((annotation, telescope, expr, meta) => AnnotatedExpr(annotation, telescope, expr, meta))

  case class ParsingContext(
      inOpSeq: Boolean = false,
      dontallowOpSeq: Boolean = false,
      newLineAfterBlockMeansEnds: Boolean = false,
      dontAllowBlockApply: Boolean = false
  ) {
    def opSeq: Boolean = !inOpSeq && !dontallowOpSeq

    def blockCall: Boolean = !inOpSeq && !dontAllowBlockApply
  }

  private def callingOnce(
      ctx: ParsingContext = ParsingContext()
  ): P[ParsedMaybeTelescope] = P(
    (list | tuple) | (lineNonEndingSpace.? ~ anonymousBlockLikeFunction.on(
      ctx.blockCall
    )).withMeta.map((block, meta) => Tuple(Vector(block), meta))
  )

  def callingMultiple(
      ctx: ParsingContext = ParsingContext()
  ): P[Vector[ParsedMaybeTelescope]] = P(
    callingOnce(ctx = ctx).rep(min = 1).map(_.toVector)
  )

  private def callingZeroOrMore(
      ctx: ParsingContext = ParsingContext()
  ): P[Vector[ParsedMaybeTelescope]] = P(
    callingOnce(ctx = ctx).rep.map(_.toVector)
  )

  def functionCall(
      function: ParsedExpr,
      p: Option[ExprMeta] => Option[ExprMeta],
      ctx: ParsingContext = ParsingContext()
  ): P[FunctionCall] = PwithMeta(callingOnce(ctx = ctx)).map((telescope, meta) => FunctionCall(function, telescope, p(meta)))

  def dotCall(
      expr: ParsedExpr,
      p: Option[ExprMeta] => Option[ExprMeta],
      ctx: ParsingContext = ParsingContext()
  ): P[DotCall] = PwithMeta(
    maybeSpace ~ "." ~ identifier ~ callingZeroOrMore(ctx = ctx)
  ).map((field, telescope, meta) => DotCall(expr, field, telescope, p(meta)))

  private def insideBlock: P[Block] = PwithMeta(
    (maybeSpace ~ statement).rep ~ maybeSpace ~ parse().? ~ maybeSpace
  ).flatMap { (heads, tail, meta) =>
    if (heads.isEmpty && tail.isEmpty) Fail("expect something")
    else Pass(Block(Vector.from(heads), tail, meta))
  }

  // empty block is allowed
  def block: P[ParsedExpr] = PwithMeta(
    "{" ~ (maybeSpace ~ statement).rep ~ maybeSpace ~ parse().? ~ maybeSpace ~ "}"
  ).map { case (heads, tail, meta) => Block(Vector.from(heads), tail, meta) }

  private inline def anonymousBlockLikeFunction: P[ParsedExpr] = block | objectParse

  private def statement: P[ParsedExpr] = P(
    (parse(ctx = ParsingContext(newLineAfterBlockMeansEnds = true)) ~ Index)
      .flatMap { (expr, index) =>
        val itWasBlockEnding = p.input(index - 1) == '}'
        Pass(expr) ~ (maybeSpace ~ ";" | lineEnding.on(itWasBlockEnding))
      }
  )

  def opSeq(
      expr: ParsedExpr,
      p: Option[ExprMeta] => Option[ExprMeta],
      ctx: ParsingContext
  ): P[OpSeq] =
    PwithMeta(opSeqGettingExprs(ctx = ctx)).flatMap { (exprs, meta) =>
      val xs = expr +: exprs
      lazy val exprCouldPrefix = expr match {
        case Identifier(name, _) if strIsOperator(name) => true
        case _                                          => false
      }

      if (!(exprCouldPrefix || xs.exists(_.isInstanceOf[Identifier]))) {
        Fail("Expected identifier")
      } else {
        Pass(OpSeq(xs, p(meta)))
      }
    }

  private def qualifiedNameOn(x: QualifiedName): P[QualifiedName] =
    PwithMeta("." ~ identifier).flatMap { (id, meta) =>
      val built = QualifiedName.build(x, id, meta)
      qualifiedNameOn(built) | Pass(built)
    }

  def qualifiedName: P[QualifiedName] = P(identifier).flatMap(id => qualifiedNameOn(id) | Pass(id))

  def symbol: P[SymbolLiteral] = P("'" ~ id).withMeta.map((name, meta) => SymbolLiteral(name, meta))

  private def objectClause0: P[ObjectClause] =
    (maybeSpace ~ qualifiedName ~ maybeSpace ~ "=" ~ maybeSpace ~ parse() ~ maybeSpace)
      .map(ObjectExprClause(_, _))

  private def objectClause1: P[ObjectClause] =
    (maybeSpace ~ parse(ctx = ParsingContext(dontallowOpSeq = true)) ~ maybeSpace ~ "=>" ~ maybeSpace ~ parse() ~ maybeSpace)
      .map(ObjectExprClauseOnValue(_, _))

  private def objectParse: P[ParsedExpr] = PwithMeta(
    "%{" ~ (objectClause0 | objectClause1).rep(sep = comma) ~ comma.? ~ maybeSpace ~ "}"
  ).map((fields, meta) => ObjectExpr(fields.toVector, meta))

  private def keyword: P[ParsedExpr] = PwithMeta(
    "#" ~ id ~ callingZeroOrMore(ParsingContext(dontAllowBlockApply = true))
  ).map((id, telescope, meta) => Keyword(id, telescope, meta))

  private def opSeqGettingExprs(ctx: ParsingContext): P[Vector[ParsedExpr]] =
    P(maybeSpace ~ parse(ctx = ctx.copy(inOpSeq = true)) ~ Index).flatMap { (expr, index) =>
      val itWasBlockEnding = p.input(index - 1) == '}'
      ((!lineEnding).checkOn(
        itWasBlockEnding && ctx.newLineAfterBlockMeansEnds
      ) ~ opSeqGettingExprs(ctx = ctx).map(expr +: _)) | Pass(Vector(expr))
    }

  private def combineMeta(
      meta1: Option[ExprMeta],
      meta2: Option[ExprMeta]
  ): Option[ExprMeta] =
    (meta1, meta2) match {
      case (Some(ExprMeta(pos1, comments1)), Some(ExprMeta(pos2, comments2))) =>
        createMeta(pos1.orElse(pos2), comments1.orElse(comments2))
      case (Some(meta), None) => Some(meta)
      case (None, Some(meta)) => Some(meta)
      case (None, None)       => None
    }

  def tailExpr(
      expr: ParsedExpr,
      getMeta: Option[ExprMeta] => Option[ExprMeta],
      ctx: ParsingContext = ParsingContext()
  ): P[ParsedExpr] = P(
    (dotCall(expr, getMeta, ctx) | functionCall(expr, getMeta, ctx = ctx).on(
      expr.isInstanceOf[Identifier] || expr
        .isInstanceOf[FunctionCall] || !ctx.inOpSeq
    ) | opSeq(expr, getMeta, ctx = ctx).on(ctx.opSeq)).withMeta ~ Index
  ).flatMap { (expr, meta, index) =>
    val itWasBlockEnding = p.input(index - 1) == '}'
    val getMeta1 =
      (endMeta: Option[ExprMeta]) => getMeta(combineMeta(meta, endMeta))
    ((!lineEnding).checkOn(
      itWasBlockEnding && ctx.newLineAfterBlockMeansEnds
    ) ~ tailExpr(expr, getMeta1, ctx = ctx)) | Pass(expr)
  }

  private inline def parse0: P[ParsedExpr] =
    symbol | keyword | objectParse | block | annotated | list | tuple | literal | identifier

  def parse(ctx: ParsingContext = ParsingContext()): P[ParsedExpr] =
    P(parse0.withMeta ~ Index).flatMap { (expr, meta, index) =>
      val getMeta = (endMeta: Option[ExprMeta]) => combineMeta(meta, endMeta)
      tailExpr(expr, getMeta, ctx = ctx) | Pass(expr)
    }

  def exprEntrance: P[ParsedExpr] = P(
    Start ~ maybeSpace ~ parse() ~ maybeSpace ~ End
  )

  def statementsEntrance: P[Vector[ParsedExpr]] = P(
    Start ~ (maybeSpace ~ statement ~ maybeSpace).rep ~ maybeSpace ~ End
  ).map(_.toVector)

  def toplevelEntrance: P[Block] = P(
    Start ~ maybeSpace ~ insideBlock ~ maybeSpace ~ End
  )
}
