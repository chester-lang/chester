package chester.utils

import scala.util.FromDigits
import scala.util.FromDigits.NumberTooSmall

import spire.math.Natural

object Nat {
  def apply(n: Int): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")
  def apply(n: Long): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")

  // TODO: check if BigInt >= 0 is ok or not?
  def apply(n: BigInt): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")
}

given NatFromDigits: FromDigits[Natural] {
  override def fromDigits(digits: String): Natural = {
    if (digits.startsWith("-")) throw NumberTooSmall()
    Natural.apply(digits)
  }
}
