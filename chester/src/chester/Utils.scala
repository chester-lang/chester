package chester

import scala.language.experimental.genericNumberLiterals
import scala.util.FromDigits
import scala.util.FromDigits.NumberTooSmall
import spire.math.Natural
import cats.data.NonEmptyVector
import upickle.default.*
import fastparse.ParserInput

given nonEmptyVectorRW[T](using ReadWriter[T]): ReadWriter[NonEmptyVector[T]] =
  readwriter[Vector[T]].bimap(_.toVector, NonEmptyVector.fromVectorUnsafe)

object Nat {
  def apply(n: Int): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")
  def apply(n: Long): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")
  def apply(n: BigInt): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")
}

given NatFromDigits: FromDigits[Natural] {
  override def fromDigits(digits: String): Natural = {
    if (digits.startsWith("-")) throw NumberTooSmall()
    Natural.apply(digits)
  }
}

given naturalRW: ReadWriter[Natural] =
  readwriter[BigInt].bimap(_.toBigInt, Nat(_))

private val MaxInt = Nat(Integer.MAX_VALUE)

extension (n: Natural) {
  def asInt: Int = if (n <= MaxInt) n.toInt else throw new IllegalArgumentException("Natural is too large to fit in an Int")
}

extension (n: Double) {
  inline def asInt: Int = n.toInt
}

extension (n: BigInt) {
  def asInt: Int = if (n.isValidInt) n.toInt else throw new IllegalArgumentException("BigInt is too large to fit in an Int")
}

extension (c: Char) {
  def asInt: Int = c.toInt
}

extension (s: String) {
  def asInt: Int = s.toInt
}

extension (text: String) {
  def utf16Len: Natural = Nat(text.length)
  def unicodeLen: Natural = Nat(text.codePointCount(0, text.length))
  def lenIsOne: Boolean =
    text.length == 1 || (text.length == 2 && Character.isHighSurrogate(text.charAt(0)) && Character.isLowSurrogate(text.charAt(1)))
}

extension (codePoint: Int) {
  def utf16Len: Natural = Nat(Character.charCount(codePoint))
  def unicodeLen: Natural = 1
}

def codepointToString(codePoint: Int): String =
  new String(Character.toChars(codePoint))

def encodeString(x: String): String = x
  .replace("\\", "\\\\")
  .replace("\n", "\\n")
  .replace("\t", "\\t")
  .replace("\r", "\\r")
  .replace("\"", "\\\"")

def parserInputToLazyList(pi: ParserInput): LazyList[String] = {
  LazyList
    .from(0)
    .takeWhile(pi.isReachable)
    .map(index => pi.slice(index, index + 1))
}

extension (x: String) {
  def getCodePoints: Seq[Int] = x.codePoints().toArray.toIndexedSeq
}

def codePointIsEmoji(codePoint: Int): Boolean = {
  (codePoint >= 0x1f600 && codePoint <= 0x1f64f) || // Emoticons
  (codePoint >= 0x1f300 && codePoint <= 0x1f5ff) || // Miscellaneous Symbols and Pictographs
  (codePoint >= 0x1f680 && codePoint <= 0x1f6ff) || // Transport and Map Symbols
  (codePoint >= 0x1f900 && codePoint <= 0x1f9ff) || // Supplemental Symbols and Pictographs
  (codePoint >= 0xe000 && codePoint <= 0xf8ff) || // Supplementary Private Use Area A
  (codePoint >= 0xf0000 && codePoint <= 0xfffff) || // Supplementary Private Use Area B
  (codePoint >= 0x100000 && codePoint <= 0x10ffff) // Supplementary Private Use Area B continuation
}

package i18n {
  import chester.*
  extension (sc: StringContext) {
    def t(args: Any*): String = sc.s(args*)
    def dt(args: ToDoc*)(using DocConf): Doc = {
      val parts = sc.parts.map(text)
      val zipped = parts.zip(args.map(_.toDoc)).flatMap { case (p, a) => List(p, a) }
      val remaining = parts.drop(args.length)
      val all = zipped ++ remaining
      chester.concat(all.toSeq)
    }
  }
}
