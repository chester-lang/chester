package chester.reader

import scala.language.experimental.genericNumberLiterals

import chester.core.CST
import chester.error.{Pos, Span, SpanInFile, VectorReporter}

class ParserTest extends munit.FunSuite {

  private def parse(input: String): (CST, Vector[ParseError]) = {
    given reporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val source = Source(FileNameAndContent("test.chester", input))
    val dummySpan = Span(source, SpanInFile(Pos.zero, Pos.zero))

    val result = for {
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    } yield Parser.parse(tokens).cst

    (result.getOrElse(CST.Symbol("<error>", Some(dummySpan))), reporter.getReports)
  }

  // Helper assertions
  private def assertNoErrors(errors: Vector[ParseError]): Unit =
    assert(errors.isEmpty, s"Expected no errors, got: $errors")

  private def assertHasError(errors: Vector[ParseError], msg: String): Unit = {
    assert(errors.nonEmpty, s"Expected errors containing '$msg'")
    assert(errors.exists(_.message.contains(msg)), s"Expected error with '$msg', got: $errors")
  }

  private def expectSymbol(cst: CST, name: String): Unit = cst match {
    case CST.Symbol(n, _) => assertEquals(n, name)
    case _                => fail(s"Expected Symbol($name), got: $cst")
  }

  private def expectInt(cst: CST, value: Int): Unit = cst match {
    case CST.IntegerLiteral(v, _) => assertEquals(v, BigInt(value))
    case _                        => fail(s"Expected IntegerLiteral($value), got: $cst")
  }

  private def expectString(cst: CST, value: String): Unit = cst match {
    case CST.StringLiteral(v, _) => assertEquals(v, value)
    case _                       => fail(s"Expected StringLiteral($value), got: $cst")
  }

  test("parse literals") {
    val (int, e1) = parse("42")
    assertNoErrors(e1)
    expectInt(int, 42)

    val (str, e2) = parse("\"hello\"")
    assertNoErrors(e2)
    expectString(str, "hello")

    val (sym, e3) = parse("'foo")
    assertNoErrors(e3)
    expectSymbol(sym, "foo")

    val (id, e4) = parse("myVar")
    assertNoErrors(e4)
    expectSymbol(id, "myVar")
  }

  test("parse tuples") {
    val (empty, e1) = parse("()")
    assertNoErrors(e1)
    assert(empty.isInstanceOf[CST.Tuple] && empty.asInstanceOf[CST.Tuple].elements.isEmpty)

    val (single, e2) = parse("(42)")
    assertNoErrors(e2)
    val CST.Tuple(elems1, _) = single: @unchecked
    assertEquals(elems1.length, 1)
    expectInt(elems1(0), 42)

    val (multi, e3) = parse("(1, 2, 3)")
    assertNoErrors(e3)
    val CST.Tuple(elems2, _) = multi: @unchecked
    assertEquals(elems2.length, 3)
    elems2.zipWithIndex.foreach { case (e, i) => expectInt(e, i + 1) }

    val (nested, e4) = parse("(1, (2, 3))")
    assertNoErrors(e4)
    val CST.Tuple(elems3, _) = nested: @unchecked
    assertEquals(elems3.length, 2)
    expectInt(elems3(0), 1)
    assert(elems3(1).isInstanceOf[CST.Tuple])
  }

  test("parse lists") {
    val (empty, e1) = parse("[]")
    assertNoErrors(e1)
    assert(empty.isInstanceOf[CST.ListLiteral] && empty.asInstanceOf[CST.ListLiteral].elements.isEmpty)

    val (list, e2) = parse("[1, 2, 3]")
    assertNoErrors(e2)
    val CST.ListLiteral(elems1, _) = list: @unchecked
    assertEquals(elems1.length, 3)
    elems1.zipWithIndex.foreach { case (e, i) => expectInt(e, i + 1) }

    val (nested, e3) = parse("[1, [2, 3]]")
    assertNoErrors(e3)
    val CST.ListLiteral(elems2, _) = nested: @unchecked
    assertEquals(elems2.length, 2)
    expectInt(elems2(0), 1)
    assert(elems2(1).isInstanceOf[CST.ListLiteral])

    val (mixed, e4) = parse("(1, [2, 3], 4)")
    assertNoErrors(e4)
    val CST.Tuple(elems3, _) = mixed: @unchecked
    assertEquals(elems3.length, 3)
    assert(elems3(1).isInstanceOf[CST.ListLiteral])
  }

  test("error recovery") {
    val (t1, e1) = parse("(1, 2")
    assertHasError(e1, "')'")
    assert(t1.asInstanceOf[CST.Tuple].elements.length == 2)

    val (t2, e2) = parse("[1, 2")
    assertHasError(e2, "']'")
    assert(t2.asInstanceOf[CST.ListLiteral].elements.length == 2)
  }

  test("parse with whitespace and comments") {
    val (cst, errors) = parse("( 1 , /* comment */ 2 )")
    assertNoErrors(errors)
    val CST.Tuple(elems, _) = cst: @unchecked
    assertEquals(elems.length, 2)
  }

  test("parse complex nested structures") {
    val (cst, errors) = parse("(fn, [1, 2], (a, b))")
    assertNoErrors(errors)
    val CST.Tuple(elems, _) = cst: @unchecked
    assertEquals(elems.length, 3)
    expectSymbol(elems(0), "fn")
    assert(elems(1).isInstanceOf[CST.ListLiteral])
    assert(elems(2).isInstanceOf[CST.Tuple])
  }

  test("parse string escapes") {
    val (cst, errors) = parse("\"hello\\nworld\"")
    assertNoErrors(errors)
    expectString(cst, "hello\nworld")
  }

  test("empty input") {
    val (cst, _) = parse("")
    assert(cst.isInstanceOf[CST.Symbol])
  }

  test("parse SeqOf expressions") {
    val (seq1, e1) = parse("f(a)")
    assertNoErrors(e1)
    val CST.SeqOf(elems1NEV, _) = seq1: @unchecked
    val elems1 = elems1NEV.toVector
    assertEquals(elems1.length, 2)
    expectSymbol(elems1(0), "f")
    assert(elems1(1).isInstanceOf[CST.Tuple])

    val (seq2, e2) = parse("a b c")
    assertNoErrors(e2)
    val CST.SeqOf(elems2, _) = seq2: @unchecked
    assertEquals(elems2.length, 3)
    val names = elems2.toVector.collect { case CST.Symbol(n, _) => n }
    assertEquals(names, Vector("a", "b", "c"))

    val (seq3, e3) = parse("f(a, b) g(c)")
    assertNoErrors(e3)
    val CST.SeqOf(elems3NEV, _) = seq3: @unchecked
    val elems3 = elems3NEV.toVector
    assertEquals(elems3.length, 4)
    expectSymbol(elems3(0), "f")
    assert(elems3(1).isInstanceOf[CST.Tuple])
    expectSymbol(elems3(2), "g")
    assert(elems3(3).isInstanceOf[CST.Tuple])
  }

  test("parse blocks - basic cases") {
    val (empty, e1) = parse("{}")
    assertNoErrors(e1)
    val CST.Block(emptyElems, emptyTail, _) = empty: @unchecked
    assertEquals(emptyElems.length, 0)
    assertEquals(emptyTail, None)

    val (tailOnly, e2) = parse("{a}")
    assertNoErrors(e2)
    val CST.Block(_, tail1, _) = tailOnly: @unchecked
    expectSymbol(tail1.get, "a")

    val (elemsTail, e3) = parse("{a;b}")
    assertNoErrors(e3)
    val CST.Block(elems2, tail2, _) = elemsTail: @unchecked
    assertEquals(elems2.length, 1)
    expectSymbol(elems2(0), "a")
    expectSymbol(tail2.get, "b")

    val (elemsOnly, e4) = parse("{a;b;}")
    assertNoErrors(e4)
    val CST.Block(elems3, tail3, _) = elemsOnly: @unchecked
    assertEquals(elems3.length, 2)
    expectSymbol(elems3(0), "a")
    expectSymbol(elems3(1), "b")
    assertEquals(tail3, None)

    val (multiElems, e5) = parse("{a;b;c}")
    assertNoErrors(e5)
    val CST.Block(elems4, tail4, _) = multiElems: @unchecked
    assertEquals(elems4.length, 2)
    expectSymbol(elems4(0), "a")
    expectSymbol(elems4(1), "b")
    expectSymbol(tail4.get, "c")
  }

  test("parse blocks - nested and typed") {
    val (nested, e1) = parse("{a;{b;c}}")
    assertNoErrors(e1)
    val CST.Block(elems1, tail1, _) = nested: @unchecked
    assertEquals(elems1.length, 1)
    expectSymbol(elems1(0), "a")
    val CST.Block(innerElems, innerTail, _) = tail1.get: @unchecked
    assertEquals(innerElems.length, 1)
    expectSymbol(innerElems(0), "b")
    expectSymbol(innerTail.get, "c")

    val (ints, e2) = parse("{1;2;3}")
    assertNoErrors(e2)
    val CST.Block(elems2, tail2, _) = ints: @unchecked
    assertEquals(elems2.length, 2)
    expectInt(elems2(0), 1)
    expectInt(elems2(1), 2)
    expectInt(tail2.get, 3)
  }

  test("parse blocks - error recovery") {
    val (unclosed, e1) = parse("{a;b")
    assertHasError(e1, "Expected '}'")
    val CST.Block(elems1, tail1, _) = unclosed: @unchecked
    assertEquals(elems1.length, 1)
    expectSymbol(tail1.get, "b")
  }

  test("parse blocks - complex with function calls") {
    val (wellFormed, e1) = parse("{print(a);b}")
    assertNoErrors(e1)
    val CST.Block(elems1, tail1, _) = wellFormed: @unchecked
    assertEquals(elems1.length, 1)
    val CST.SeqOf(seqElems1NEV, _) = elems1(0): @unchecked
    val seqElems1 = seqElems1NEV.toVector
    assertEquals(seqElems1.length, 2)
    expectSymbol(seqElems1(0), "print")
    assert(seqElems1(1).isInstanceOf[CST.Tuple])
    expectSymbol(tail1.get, "b")

    val (unclosedTuple, e2) = parse("{print(a;b}")
    assertHasError(e2, "")
    val CST.Block(elems2, tail2, _) = unclosedTuple: @unchecked
    assertEquals(elems2.length, 1)
    val CST.SeqOf(seqElems2NEV, _) = elems2(0): @unchecked
    val seqElems2 = seqElems2NEV.toVector
    assertEquals(seqElems2.length, 2)
    expectSymbol(seqElems2(0), "print")
    assert(seqElems2(1).isInstanceOf[CST.Tuple])
    expectSymbol(tail2.get, "b")

    val (wrongCloser, e3) = parse("f({print(a);b)")
    assertHasError(e3, "")
    val CST.SeqOf(topElemsNEV, _) = wrongCloser: @unchecked
    val topElems = topElemsNEV.toVector
    assertEquals(topElems.length, 2)
    expectSymbol(topElems(0), "f")
    val CST.Tuple(tupleElems, _) = topElems(1): @unchecked
    val CST.Block(blockElems, blockTail, _) = tupleElems(0): @unchecked
    assertEquals(blockElems.length, 1)
    assert(blockElems(0).isInstanceOf[CST.SeqOf])
    expectSymbol(blockTail.get, "b")
  }

  // Helper for parseFile tests
  private def parseFile(input: String): (CST, Vector[ParseError]) = {
    given reporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val source = Source(FileNameAndContent("test.chester", input))
    val dummySpan = Span(source, SpanInFile(Pos.zero, Pos.zero))

    // Handle empty file specially since tokenizer rejects empty input
    if (input.trim.isEmpty) {
      val emptyTokens = Vector(Token.EOF(dummySpan))
      return (Parser.parseFile(emptyTokens), reporter.getReports)
    }

    val result = for {
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    } yield Parser.parseFile(tokens)

    (result.getOrElse(CST.Symbol("<error>", Some(dummySpan))), reporter.getReports)
  }

  test("parseFile - empty file") {
    val (cst, errors) = parseFile("")
    assertNoErrors(errors)
    val CST.Block(elements, tail, _) = cst: @unchecked
    assertEquals(elements.length, 0)
    assertEquals(tail, None)
  }

  test("parseFile - single expression") {
    val (cst, errors) = parseFile("42")
    assertNoErrors(errors)
    val CST.Block(elements, tail, _) = cst: @unchecked
    assertEquals(elements.length, 0)
    expectInt(tail.get, 42)
  }

  test("parseFile - statements with semicolons") {
    val (cst, errors) = parseFile("a; b; c")
    assertNoErrors(errors)
    val CST.Block(elements, tail, _) = cst: @unchecked
    assertEquals(elements.length, 2)
    expectSymbol(elements(0), "a")
    expectSymbol(elements(1), "b")
    expectSymbol(tail.get, "c")
  }

  test("parseFile - all statements end with semicolon") {
    val (cst, errors) = parseFile("a; b; c;")
    assertNoErrors(errors)
    val CST.Block(elements, tail, _) = cst: @unchecked
    assertEquals(elements.length, 3)
    expectSymbol(elements(0), "a")
    expectSymbol(elements(1), "b")
    expectSymbol(elements(2), "c")
    assertEquals(tail, None)
  }

  test("parseFile - complex expressions") {
    val (cst, errors) = parseFile("f(x); {a; b}; 42")
    assertNoErrors(errors)
    val CST.Block(elements, tail, _) = cst: @unchecked
    assertEquals(elements.length, 2)
    assert(elements(0).isInstanceOf[CST.SeqOf])
    assert(elements(1).isInstanceOf[CST.Block])
    expectInt(tail.get, 42)
  }

  test("parseFile - error recovery: unexpected token after file end") {
    // This test is tricky since all tokens should be part of the file
    // The error would occur if parseFile somehow doesn't consume all tokens
    val (cst, errors) = parseFile("a;; b")
    // Double semicolon creates empty statement (which is valid)
    assertNoErrors(errors)
    val CST.Block(elements, tail, _) = cst: @unchecked
    assertEquals(elements.length, 1)
    expectSymbol(elements(0), "a")
    expectSymbol(tail.get, "b")
  }

  test("parseFile - whitespace and comments") {
    val (cst, errors) = parseFile("a; /* comment */ b; // line comment\nc")
    assertNoErrors(errors)
    val CST.Block(elements, tail, _) = cst: @unchecked
    assertEquals(elements.length, 2)
    expectSymbol(elements(0), "a")
    expectSymbol(elements(1), "b")
    expectSymbol(tail.get, "c")
  }

  test("parse trailing commas in tuples") {
    val (single, e1) = parse("(42,)")
    assertNoErrors(e1)
    val CST.Tuple(elems1, _) = single: @unchecked
    assertEquals(elems1.length, 1)
    expectInt(elems1(0), 42)

    val (multi, e2) = parse("(1, 2, 3,)")
    assertNoErrors(e2)
    val CST.Tuple(elems2, _) = multi: @unchecked
    assertEquals(elems2.length, 3)
    elems2.zipWithIndex.foreach { case (e, i) => expectInt(e, i + 1) }

    val (nested, e3) = parse("(1, (2, 3,),)")
    assertNoErrors(e3)
    val CST.Tuple(elems3, _) = nested: @unchecked
    assertEquals(elems3.length, 2)
    expectInt(elems3(0), 1)
    assert(elems3(1).isInstanceOf[CST.Tuple])
  }

  test("parse trailing commas in lists") {
    val (single, e1) = parse("[42,]")
    assertNoErrors(e1)
    val CST.ListLiteral(elems1, _) = single: @unchecked
    assertEquals(elems1.length, 1)
    expectInt(elems1(0), 42)

    val (multi, e2) = parse("[1, 2, 3,]")
    assertNoErrors(e2)
    val CST.ListLiteral(elems2, _) = multi: @unchecked
    assertEquals(elems2.length, 3)
    elems2.zipWithIndex.foreach { case (e, i) => expectInt(e, i + 1) }

    val (nested, e3) = parse("[1, [2, 3,],]")
    assertNoErrors(e3)
    val CST.ListLiteral(elems3, _) = nested: @unchecked
    assertEquals(elems3.length, 2)
    expectInt(elems3(0), 1)
    assert(elems3(1).isInstanceOf[CST.ListLiteral])
  }
}
