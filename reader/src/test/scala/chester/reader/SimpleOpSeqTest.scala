package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite
import chester.reader.*

class SimpleOpSeqTest extends FunSuite {
  test("parse simple opSeq with single operator") {
    val input = "1 + 2"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(2, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }
}
