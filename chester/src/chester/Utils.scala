package chester

import scala.language.experimental.genericNumberLiterals
import scala.util.FromDigits
import scala.util.FromDigits.NumberTooSmall
import spire.math.Natural
import upickle.default.*
import fastparse.ParserInput

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

package i18n {
  extension (sc: StringContext) {
    def t(args: Any*): String = sc.s(args*)
  }
}
