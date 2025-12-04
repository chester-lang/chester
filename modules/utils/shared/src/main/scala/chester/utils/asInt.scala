package chester.utils

import spire.math.{Natural, UInt}

private val MaxInt = Nat(Integer.MAX_VALUE)
private val MaxIntUInt = UInt(Integer.MAX_VALUE)

extension (n: Natural) {
  def asInt: Int = if (n <= MaxInt) n.toInt else throw new IllegalArgumentException("Natural is too large to fit in an Int")
}

extension (n: Double) {
  // TODO: make more safe
  inline def asInt: Int = n.toInt
}

extension (n: BigInt) {
  def asInt: Int = if (n.isValidInt) n.toInt else throw new IllegalArgumentException("BigInt is too large to fit in an Int")
}

extension (n: UInt) {
  def asInt: Int = if (n <= MaxIntUInt) n.toInt else throw new IllegalArgumentException("UInt is too large to fit in an Int")
}

// Char is 16bit - always safe
extension (c: Char) {
  def asInt: Int = c.toInt
}

extension (s: String) {
  def asInt: Int = s.toInt
}

// for Scala2
object AsInt {
  implicit class AsIntFor(private val n: BigInt) extends AnyVal {
    def asInt: Int = if (n.isValidInt) n.toInt else throw new IllegalArgumentException("BigInt is too large to fit in an Int")
  }
  implicit class AsIntForLong(private val n: Long) extends AnyVal {
    def asInt: Int = if (n.isValidInt) n.toInt else throw new IllegalArgumentException("Long is too large to fit in an Int")
  }
}
