package chester.error

import scala.language.experimental.genericNumberLiterals

import spire.math.Natural
import chester.reader.{Offset, Source}
import chester.utils.{Nat, WithUTF16, asInt, encodeString, given}
import fastparse.ParserInput
import upickle.default.*
import chester.i18n.*

case class Pos(index: WithUTF16, line: spire.math.Natural, column: WithUTF16) derives ReadWriter

object Pos {
  val zero: Pos = Pos(WithUTF16.Zero, 0, WithUTF16.Zero)
}

/** start <= i < end */
case class SpanInFile(start: Pos, end: Pos) derives ReadWriter {}

type AcceptedString = String | LazyList[String] | ParserInput

case class FileContent(
    // maybe a LazyList[String]
    content: Seq[String],
    offset: Offset
) {
  @deprecated
  lazy val convertToString: String = content.mkString
}

object FileContent {
  def apply(source: Source): FileContent = FileContent(
    source.readContent.getOrElse(Vector.empty),
    source.offset
  )
}

//given SourcePosCodec: JsonValueCodec[SourcePos] = JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

case class Span(source: Source, range: SpanInFile) derives ReadWriter {
  private lazy val fileContent: Option[FileContent] =
    source.readContent.toOption.map(content => FileContent(content, source.offset))
  val fileName: String = source.fileName

  /** Extracts all lines within the range with their line numbers.
    *
    * @return
    *   Option containing a Vector of (lineNumber, lineContent) tuples where:
    *   - lineNumber: 1-based line numbers for display (e.g., if range spans lines 3-5, returns exactly [(3,"line3"), (4,"line4"), (5,"line5")])
    *   - lineContent: The actual text content of that line Note: While internal line tracking is 0-based, this API returns 1-based line numbers for
    *     display
    */
  def getLinesInRange: Option[Vector[(Int, String)]] = fileContent map { fileContent =>
    val startLine = range.start.line.asInt - fileContent.offset.lineOffset.asInt
    val endLine = range.end.line.asInt - fileContent.offset.lineOffset.asInt
    val contentString = fileContent.convertToString
    val lines = contentString.split('\n').toVector

    // Assert that the start and end lines are within valid bounds
    assert(
      startLine >= 0 && startLine <= endLine && endLine < lines.length,
      t"Invalid line range: startLine=${startLine + fileContent.offset.lineOffset.asInt}, endLine=${endLine + fileContent.offset.lineOffset.asInt}, totalLines=${lines.length}"
    )

    // Slice the lines and keep their line numbers
    lines.zipWithIndex
      .slice(startLine, endLine + 1)
      .map { case (line, index) =>
        (fileContent.offset.lineOffset.asInt + index + 1, line)
      } // Line numbers are 1-based
  }

  def combine(other: Span): Span = {
    if (fileName != other.fileName) {
      throw new IllegalArgumentException(
        "Cannot combine source positions from different files"
      )
    }
    require(range.start.index <= other.range.start.index)
    require(source == other.source)
    val newRange = SpanInFile(range.start, other.range.end)
    Span(source, newRange)
  }

  override def toString: String =
    t"Span(\"${encodeString(fileName)}\",$range)"
}

extension (pos: Option[Span]) {
  def combineInOption(other: Option[Span]): Option[Span] =
    (pos, other) match {
      case (None, None)         => None
      case (Some(p), None)      => Some(p)
      case (None, Some(p))      => Some(p)
      case (Some(p1), Some(p2)) => Some(p1.combine(p2))
    }
}
