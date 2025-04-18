package chester.error

import chester.reader.Source
import chester.utils.{WithUTF16, encodeString, parserInputToLazyList}
import fastparse.ParserInput
import upickle.default.*
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.all.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import _root_.io.github.iltotore.iron.upickle.given
import chester.i18n.*

import scala.annotation.tailrec

case class Pos(index: WithUTF16, line: Int :| Positive0, column: WithUTF16) derives ReadWriter

object Pos {
  val zero: Pos = Pos(WithUTF16.Zero, 0, WithUTF16.Zero)
}

/** start <= i < end */
case class RangeInFile(start: Pos, end: Pos) derives ReadWriter {}

type AcceptedString = String | LazyList[String] | ParserInput

case class FileContent(
    content: AcceptedString,
    lineOffset: Int,
    indexOffset: WithUTF16
)

object FileContent {
  @tailrec
  private def convertToString0(fileContent: AcceptedString): String =
    fileContent match {
      case s: String            => s
      case ll: LazyList[String] => ll.mkString
      case pi: ParserInput      => convertToString0(parserInputToLazyList(pi))
    }

  def convertToString(fileContent: FileContent): String = convertToString0(
    fileContent.content
  )
}

case class SourcePos(source: Source, range: RangeInFile) derives ReadWriter {
  lazy val fileContent: Option[FileContent] = source.readContent.toOption.map(
    FileContent(_, source.linesOffset, source.posOffset)
  )
  val fileName = source.fileName

  /** Extracts all lines within the range with their line numbers.
    *
    * @return
    *   Option containing a Vector of (lineNumber, lineContent) tuples where:
    *   - lineNumber: 1-based line numbers for display (e.g., if range spans lines 3-5, returns exactly [(3,"line3"), (4,"line4"), (5,"line5")])
    *   - lineContent: The actual text content of that line Note: While internal line tracking is 0-based, this API returns 1-based line numbers for
    *     display
    */
  def getLinesInRange: Option[Vector[(Int, String)]] = fileContent map { fileContent =>
    val startLine = range.start.line - fileContent.lineOffset
    val endLine = range.end.line - fileContent.lineOffset
    val contentString = FileContent.convertToString(fileContent)
    val lines = contentString.split('\n').toVector

    // Assert that the start and end lines are within valid bounds
    assert(
      startLine >= 0 && endLine < lines.length,
      t"Invalid line range: startLine=$startLine, endLine=$endLine, totalLines=${lines.length}"
    )

    // Slice the lines and keep their line numbers
    lines.zipWithIndex
      .slice(startLine, endLine + 1)
      .map { case (line, index) =>
        (fileContent.lineOffset + index + 1, line)
      } // Line numbers are 1-based
  }

  def combine(other: SourcePos): SourcePos = {
    if (fileName != other.fileName) {
      throw new IllegalArgumentException(
        "Cannot combine source positions from different files"
      )
    }
    require(range.start.index <= other.range.start.index)
    require(source == other.source)
    val newRange = RangeInFile(range.start, other.range.end)
    SourcePos(source, newRange)
  }

  override def toString: String =
    t"SourcePos(\"${encodeString(fileName)}\",$range)"
}

extension (pos: Option[SourcePos]) {
  def combineInOption(other: Option[SourcePos]): Option[SourcePos] =
    (pos, other) match {
      case (None, None)         => None
      case (Some(p), None)      => Some(p)
      case (None, Some(p))      => Some(p)
      case (Some(p1), Some(p2)) => Some(p1.combine(p2))
    }
}
