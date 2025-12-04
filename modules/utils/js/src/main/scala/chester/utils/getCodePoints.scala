package chester.utils
import chester.utils.asInt

extension (x: String) {
  def getCodePoints: Seq[Int] = {
    val codePoints = Seq.newBuilder[Int]
    var i = 0
    while (i < x.length) {
      val codePoint = x.charAt(i)
      if (
        Character.isHighSurrogate(codePoint) && i + 1 < x.length && Character
          .isLowSurrogate(x.charAt(i + 1))
      ) {
        codePoints += Character.toCodePoint(codePoint, x.charAt(i + 1))
        i += 2
      } else {
        codePoints += codePoint.asInt
        i += 1
      }
    }
    codePoints.result()
  }
}
