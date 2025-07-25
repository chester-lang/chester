package chester.utils

import upickle.default.*

import scala.reflect.ClassTag

def union2RW[A: ClassTag, B: ClassTag](using
    a: ReadWriter[A],
    b: ReadWriter[B]
): ReadWriter[A | B] = {
  require(a != b)
  sealed trait Union2 extends Product with Serializable derives ReadWriter {
    def toUnionType: A | B
  }
  extension (u: A | B) {
    def toUnion2: Union2 = u match {
      case a: A => Union2A(a)
      case b: B => Union2B(b)
    }
  }
  case class Union2A(a: A) extends Union2 {
    def toUnionType: A | B = a
  }
  case class Union2B(b: B) extends Union2 {
    def toUnionType: A | B = b
  }
  readwriter[Union2].bimap(_.toUnion2, _.toUnionType)
}

def union4RW[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag](using
    a: ReadWriter[A],
    b: ReadWriter[B],
    c: ReadWriter[C],
    d: ReadWriter[D]
): ReadWriter[A | B | C | D] = {
  require(a != b && a != c && a != d && b != c && b != d && c != d)
  sealed trait Union4 extends Product with Serializable derives ReadWriter {
    def toUnionType: A | B | C | D
  }
  extension (u: A | B | C | D) {
    def toUnion4: Union4 = u match {
      case a: A => Union4A(a)
      case b: B => Union4B(b)
      case c: C => Union4C(c)
      case d: D => Union4D(d)
    }
  }
  case class Union4A(a: A) extends Union4 {
    def toUnionType: A | B | C | D = a
  }
  case class Union4B(b: B) extends Union4 {
    def toUnionType: A | B | C | D = b
  }
  case class Union4C(c: C) extends Union4 {
    def toUnionType: A | B | C | D = c
  }
  case class Union4D(d: D) extends Union4 {
    def toUnionType: A | B | C | D = d
  }
  readwriter[Union4].bimap(_.toUnion4, _.toUnionType)
}
