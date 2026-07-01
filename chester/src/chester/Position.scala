package chester

import scala.language.experimental.genericNumberLiterals
import spire.math.Natural
import upickle.default.*
import chester.i18n.*

case class WithUTF16(unicode: spire.math.Natural, utf16: spire.math.Natural) derives ReadWriter {
  require(unicode <= utf16, "unicode must be less than or equal to utf16")
  def <(other: WithUTF16): Boolean = unicode < other.unicode && utf16 < other.utf16
  def >(other: WithUTF16): Boolean = unicode > other.unicode && utf16 > other.utf16
  def <=(other: WithUTF16): Boolean = unicode <= other.unicode && utf16 <= other.utf16
  def >=(other: WithUTF16): Boolean = unicode >= other.unicode && utf16 >= other.utf16
  def +(other: WithUTF16): WithUTF16 =
    WithUTF16(unicode + other.unicode, utf16 + other.utf16)
  def isZero: Boolean = unicode == 0 && utf16 == 0
  def nonZero: Boolean = unicode != 0 && utf16 != 0
}

object WithUTF16 {
  val Zero: WithUTF16 = WithUTF16(0, 0)
  val One: WithUTF16 = WithUTF16(1, 1)
}

case class Pos(index: WithUTF16, line: spire.math.Natural, column: WithUTF16) derives ReadWriter

object Pos {
  val zero: Pos = Pos(WithUTF16.Zero, 0, WithUTF16.Zero)
}

case class SpanInFile(start: Pos, end: Pos) derives ReadWriter

trait SpanOptional0 extends Any {
  def span0: Option[Span]
}

trait SpanOptional extends Any with SpanOptional0 {
  def span: Option[Span]
  override def span0: Option[Span] = span
}

trait SpanRequired extends Any with SpanOptional0 {
  def span: Span
  override def span0: Option[Span] = Some(span)
}


case class Span(source: Source, range: SpanInFile) derives ReadWriter {
  private lazy val fileContent: Option[FileContent] =
    source.readContent.toOption.map(content => FileContent(content, source.offset))
  val fileName: String = source.fileName

  def getLinesInRange: Option[Vector[(Int, String)]] = fileContent map { fileContent =>
    val startLine = range.start.line.asInt - fileContent.offset.lineOffset.asInt
    val endLine = range.end.line.asInt - fileContent.offset.lineOffset.asInt
    val contentString = fileContent.convertToString
    val lines = contentString.split('\n').toVector

    assert(
      startLine >= 0 && startLine <= endLine && endLine < lines.length,
      t"Invalid line range: startLine=${startLine + fileContent.offset.lineOffset.asInt}, endLine=${endLine + fileContent.offset.lineOffset.asInt}, totalLines=${lines.length}"
    )

    lines.zipWithIndex
      .slice(startLine, endLine + 1)
      .map { case (line, index) =>
        (fileContent.offset.lineOffset.asInt + index + 1, line)
      }
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
  def combineInOption(other: Option[Span]): Option[Span] = {
    (pos, other) match {
      case (None, None)         => None
      case (Some(p), None)      => Some(p)
      case (None, Some(p))      => Some(p)
      case (Some(p1), Some(p2)) => Some(p1.combine(p2))
    }
  }
}
