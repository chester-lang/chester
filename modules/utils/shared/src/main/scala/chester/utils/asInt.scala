package chester.utils

import spire.math.Natural

def Nat(n: Int): Natural = Natural(n.toLong)
def Nat(n: Long): Natural = Natural(n)

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
