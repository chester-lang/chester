package chester.reader

import upickle.default.*
import chester.error.*
import chester.utils.{Nat, WithUTF16, codepointToString, given}
import chester.utils.doc.{Doc, DocConf}
import spire.math.Natural
import chester.utils.utf16Len

import scala.language.experimental.genericNumberLiterals

case class ParseError(message: String, span0: Option[Span] = None) extends Problem {
  override def severity: Problem.Severity = Problem.Severity.Error
  override def stage: Problem.Stage = Problem.Stage.PARSE

  override def toDoc(using DocConf): Doc = Doc.text(message)
}

sealed trait ParserSource extends Product with Serializable derives ReadWriter {
  def fileName: String

  // maybe a LazyList[String]
  def readContent: Either[ParseError, Seq[String]]
}

case class FileNameAndContent(fileName: String, content: String) extends ParserSource derives ReadWriter {
  override val readContent: Either[ParseError, Seq[String]] = Right(Vector(content))
}

trait FilePathImpl {
  def readContent(fileName: String): Either[ParseError, String]

  def absolute(fileName: String): String
}

object FilePath {
  def apply(fileName: String)(using used: FilePathImpl): FilePath = {
    val path = used.absolute(fileName)
    val result = new FilePath(path)
    result.impl = used
    result
  }
}

case class FilePath private (fileName: String) extends ParserSource {
  private[chester] var impl: FilePathImpl = scala.compiletime.uninitialized
  override lazy val readContent: Either[ParseError, Seq[String]] =
    if (impl == null) Left(ParseError("No FilePathImpl provided"))
    else impl.readContent(fileName).map(Vector(_))
}

// TODO: maybe column offset for the first line also
case class Source(
    source: ParserSource,
    offset: Offset = Offset.Zero
) extends ParserSource derives ReadWriter {
  override def fileName: String = source.fileName

  override def readContent: Either[ParseError, Seq[String]] = source.readContent
}

case class Offset(
    lineOffset: spire.math.Natural = 0,
    posOffset: WithUTF16 = WithUTF16.Zero,
    firstLineColumnOffset: WithUTF16 = WithUTF16.Zero
) derives ReadWriter {

  if (lineOffset != 0) require(posOffset.nonZero)
  require(posOffset >= firstLineColumnOffset)
  if (lineOffset == 0) require(posOffset == firstLineColumnOffset)
  def getPos: Pos = add(Pos.zero)
  def next(codePoint: Int): Offset = codepointToString(codePoint) match {
    case s @ "\n" =>
      val newLineSize = WithUTF16(1, s.utf16Len)
      copy(
        lineOffset = lineOffset + (1: Natural),
        posOffset = posOffset + newLineSize
        // firstLineColumnOffset stays the same - it remembers the starting column from first line
        // posOffset continues to accumulate, keeping the invariant posOffset >= firstLineColumnOffset
      )
    case s =>
      if (lineOffset == 0) {
        // On first line, both posOffset and firstLineColumnOffset must grow together
        val newOffset = firstLineColumnOffset + WithUTF16(1, s.utf16Len)
        copy(posOffset = newOffset, firstLineColumnOffset = newOffset)
      } else {
        // On subsequent lines, only posOffset grows, firstLineColumnOffset stays fixed
        copy(posOffset = posOffset + WithUTF16(1, s.utf16Len))
      }
  }
  def add(x: Pos): Pos =
    if (x.line == 0)
      Pos(index = posOffset + x.index, line = x.line + lineOffset, column = x.column + firstLineColumnOffset)
    else
      Pos(index = posOffset + x.index, line = x.line + lineOffset, column = x.column)

  def add(x: SpanInFile): SpanInFile = SpanInFile(start = add(x.start), end = add(x.end))
  def add(x: Span): Span = Span(source = x.source, range = add(x.range))
}

object Offset {
  val Zero: Offset = Offset()
}
