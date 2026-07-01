package chester

import scala.language.experimental.genericNumberLiterals
import upickle.default.*
import spire.math.Natural
import fastparse.ParserInput

object Problem {
  enum Stage derives ReadWriter {
    case TYCK, PARSE, OTHER
  }

  enum Severity derives ReadWriter {
    case Error, Goal, Warning, Info
  }
}

trait Problem {
  def severity: Problem.Severity
  def stage: Problem.Stage
  def message: String
  def span0: Option[Span]
}

case class ParseError(message: String, span0: Option[Span] = None) extends Problem {
  override def severity: Problem.Severity = Problem.Severity.Error
  override def stage: Problem.Stage = Problem.Stage.PARSE
}

sealed trait ParserSource extends Product with Serializable derives ReadWriter {
  def fileName: String
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
  override lazy val readContent: Either[ParseError, Seq[String]] = {
    if (impl == null) Left(ParseError("No FilePathImpl provided"))
    else impl.readContent(fileName).map(Vector(_))
  }
}

case class Source(
    source: ParserSource,
    offset: Offset = Offset.Zero
) extends ParserSource derives ReadWriter {
  override def fileName: String = source.fileName
  override def readContent: Either[ParseError, Seq[String]] = source.readContent
}

case class FileContent(
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
      )
    case s =>
      if (lineOffset == 0) {
        val newOffset = firstLineColumnOffset + WithUTF16(1, s.utf16Len)
        copy(posOffset = newOffset, firstLineColumnOffset = newOffset)
      } else {
        copy(posOffset = posOffset + WithUTF16(1, s.utf16Len))
      }
  }
  def add(x: Pos): Pos = {
    if (x.line == 0) {
      Pos(index = posOffset + x.index, line = x.line + lineOffset, column = x.column + firstLineColumnOffset)
    } else {
      Pos(index = posOffset + x.index, line = x.line + lineOffset, column = x.column)
    }
  }

  def add(x: SpanInFile): SpanInFile = SpanInFile(start = add(x.start), end = add(x.end))
  def add(x: Span): Span = Span(source = x.source, range = add(x.range))
}

object Offset {
  val Zero: Offset = Offset()
}
