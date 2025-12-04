package chester.utils

import spire.math.Natural
import upickle.default.*
import scala.language.experimental.genericNumberLiterals

//given WithUTF16Codec: JsonValueCodec[WithUTF16] = JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

// i is unicode character position
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
