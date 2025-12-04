package chester.utils

import scala.collection.immutable.ArraySeq

extension (x: String) {
  def getCodePoints: Seq[Int] = ArraySeq.unsafeWrapArray(x.codePoints().toArray)
}
