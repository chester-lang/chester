package chester.reader

import chester.error.{Span, SpanInFile}
import chester.reader.{Offset, ParseError, Source}
import chester.reader.StringChar
import chester.utils.getCodePoints
 import scala.language.experimental.genericNumberLiterals

import scala.collection.mutable.ArrayBuffer

object CharReader {
  private def reading(strings: Seq[String], source: Source, offset: Offset): LazyList[StringChar] = {
    if (strings.isEmpty) return LazyList.empty
    val head = strings.head
    val tail = strings.tail
    val chars = ArrayBuffer.empty[StringChar]
    var currentOffset = offset
    for (codePoint <- getCodePoints(head)) {
      val beginPos = currentOffset.getPos
      currentOffset = currentOffset.next(codePoint)
      val endPos = currentOffset.getPos
      chars += StringChar(codePoint, Span(source, SpanInFile(beginPos, endPos)))
    }
    LazyList.from(chars) #::: reading(tail, source, currentOffset)
  }
  def read(source: Source): Either[ParseError, Seq[StringChar]] = source.readContent.map(strings => reading(strings, source, source.offset))
}
