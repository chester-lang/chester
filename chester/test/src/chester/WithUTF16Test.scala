package chester

import scala.language.experimental.genericNumberLiterals

import chester.given
import spire.math.Natural
import upickle.default.*

class WithUTF16Test extends munit.FunSuite {
  test("WithUTF16 basic properties and operators") {
    val zero = WithUTF16.Zero
    val one = WithUTF16.One
    
    assert(zero.isZero)
    assert(!zero.nonZero)
    assert(!one.isZero)
    assert(one.nonZero)

    val two = one + one
    assertEquals(two.unicode, 2: Natural)
    assertEquals(two.utf16, 2: Natural)

    assert(zero < one)
    assert(one <= two)
    assert(two > one)
    assert(two >= two)
  }

  test("genericNumberLiterals support") {
    // Under genericNumberLiterals and the NatFromDigits given instance,
    // integer literals automatically convert to spire.math.Natural.
    val n: Natural = 12345
    assertEquals(n, 12345: Natural)

    // WithUTF16 expects (spire.math.Natural, spire.math.Natural)
    val w = WithUTF16(10, 20)
    assertEquals(w.unicode, 10: Natural)
    assertEquals(w.utf16, 20: Natural)

    // Pos expects (WithUTF16, spire.math.Natural, WithUTF16)
    val p = Pos(WithUTF16(0, 0), 100, WithUTF16(5, 5))
    assertEquals(p.line, 100: Natural)
  }

  test("Pos and SpanInFile serialization") {
    val pos1 = Pos(WithUTF16(0, 0), 0, WithUTF16(0, 0))
    val pos2 = Pos(WithUTF16(10, 15), 1, WithUTF16(5, 7))
    val span = SpanInFile(pos1, pos2)

    val serializedPos = write[Pos](pos2)
    val deserializedPos = read[Pos](serializedPos)
    assertEquals(deserializedPos, pos2)

    val serializedSpan = write[SpanInFile](span)
    val deserializedSpan = read[SpanInFile](serializedSpan)
    assertEquals(deserializedSpan, span)
  }

  test("Offset progress and Span extraction") {
    val source = Source(FileNameAndContent("test.txt", "hello\nworld"))
    var offset = Offset.Zero
    
    // Process "hello\n"
    "hello\n".foreach { char =>
      offset = offset.next(char.toInt)
    }

    // Now offset should point to line 1, pos 6 (unicode/utf16), column 0
    assertEquals(offset.lineOffset, 1: Natural)
    assertEquals(offset.posOffset, WithUTF16(6, 6))
    
    val startPos = Pos.zero
    val endPos = Pos(WithUTF16(5, 5), 0, WithUTF16(5, 5))
    val span = Span(source, SpanInFile(startPos, endPos))

    val lines = span.getLinesInRange
    assert(lines.isDefined)
    assertEquals(lines.get, Vector((1, "hello")))
  }
}
