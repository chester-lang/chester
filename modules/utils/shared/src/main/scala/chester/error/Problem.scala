package chester.error

import chester.utils.doc.*
import upickle.default.*
import chester.i18n.*
import chester.utils.Nat

import scala.language.experimental.genericNumberLiterals

object Problem {
  enum Stage derives ReadWriter {
    case TYCK, PARSE, OTHER
  }

  enum Severity derives ReadWriter {
    case Error, Goal, Warning, Info
  }
}

trait WithServerity extends Any {
  def severity: Problem.Severity

  final def isError: Boolean = severity == Problem.Severity.Error
}

case class DescriptionElement(doc: ToDoc, span0: Option[Span]) extends SpanOptional0

case class FullDescription(begin: ToDoc, explanations: Vector[DescriptionElement], end: ToDoc)

trait Problem extends ToDoc with SpanOptional0 with WithServerity {
  def stage: Problem.Stage
  // TODO: use this
  def fullDescription: Option[FullDescription] = None
}

private case class ProblemSer(
    stage: Problem.Stage,
    severity: Problem.Severity,
    message: Doc,
    span0: Option[Span]
) extends Problem derives ReadWriter {
  override def toDoc(using DocConf): Doc = message
}

private object ProblemSer {
  def from(problem: Problem): ProblemSer = ProblemSer(
    problem.stage,
    problem.severity,
    problem.toDoc(using DocConf.Default),
    problem.span0
  )
}

object ProblemUpickle {
  implicit val problemRW: ReadWriter[Problem] =
    readwriter[ProblemSer].bimap(ProblemSer.from, x => x)
}

extension (p: Problem) {
  def renderDoc(using options: DocConf, sourceReader: SourceReader): Doc =
    p.fullDescription match {
      case Some(desc) => renderFullDescription(desc)(using options, sourceReader)
      case None       => renderToDocWithSource(p)(using options, sourceReader)
    }
}

private def renderFullDescription(desc: FullDescription)(using options: DocConf, sourceReader: SourceReader): Doc = {
  val beginDoc = desc.begin.toDoc
  val explanationsDoc = desc.explanations.map { elem =>
    val elemDoc = elem.doc.toDoc
    elem.span0.flatMap(sourceReader.apply) match {
      case Some(lines) =>
        val sourceLines = lines.map((lineNumber, line) => Doc.text(t"$lineNumber") <+> Doc.text(line, Styling.BoldOn))
        val codeBlock = Doc.group(Doc.concat(sourceLines.map(_.end)*))
        elemDoc </> codeBlock
      case None => elemDoc
    }
  }
  val endDoc = desc.end.toDoc

  Doc.concat(
    beginDoc,
    Doc.line,
    ssep(explanationsDoc, Doc.line),
    Doc.line,
    endDoc
  )
}

private def renderToDocWithSource(p: Problem)(using options: DocConf, sourceReader: SourceReader): Doc = {
  val severityDoc = p.severity match {
    case Problem.Severity.Error   => Doc.text(t"Error")
    case Problem.Severity.Warning => Doc.text(t"Warning")
    case Problem.Severity.Goal    => Doc.text(t"Goal")
    case Problem.Severity.Info    => Doc.text(t"Info")
  }

  val baseDoc = severityDoc <+> p.toDoc

  p.span0 match {
    case Some(pos) =>
      val locationHeader = Doc.text(t"Location") <+>
        Doc.text(
          t"${pos.fileName} [${pos.range.start.line + 1}:${pos.range.start.column.unicode + 1}] to [${pos.range.end.line + 1}:${pos.range.end.column.unicode + 1}]",
          Styling.BoldOn
        )

      val sourceLines = sourceReader(pos)
        .map {
          _.map { case (lineNumber, line) =>
            Doc.text(t"$lineNumber") <+> Doc.text(line, Styling.BoldOn)
          }
        }
        .getOrElse(Vector.empty)

      val codeBlock = Doc.group(Doc.concat(sourceLines.map(_.end)*))

      baseDoc <|> locationHeader <|> codeBlock

    case None =>
      baseDoc
  }
}

/** A reader for source code that provides line-numbered content.
  *
  * @param readSource
  *   A function that takes a SourcePos and returns line-numbered content. The returned Vector contains tuples of (lineNumber, lineContent) where:
  *   - lineNumber: 1-based line numbers (e.g., lines 3,4,5)
  *   - lineContent: The actual text content of that line Note: While internal line tracking is 0-based, this API returns 1-based line numbers for
  *     display
  */
case class SourceReader(readSource: Span => Option[Vector[(Int, String)]]) {
  def apply(pos: Span): Option[Vector[(Int, String)]] = readSource(pos)
}

object SourceReader {
  def fromFileContent(content: FileContent): SourceReader =
    SourceReader(_.getLinesInRange)

  def default: SourceReader = SourceReader(_.getLinesInRange)

  def empty: SourceReader = SourceReader(_ => None)
}
