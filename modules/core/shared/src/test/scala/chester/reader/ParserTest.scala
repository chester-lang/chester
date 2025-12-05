package chester.reader

import chester.core.CST
import chester.error.{Span, SpanInFile, Pos, VectorReporter}

import scala.language.experimental.genericNumberLiterals

class ParserTest extends munit.FunSuite {

  private def createTestSource(content: String): Source =
    Source(FileNameAndContent("test.chester", content))

  private def parseString(input: String): (CST, Vector[ParseError]) = {
    given reporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val source = createTestSource(input)
    val charsResult = CharReader.read(source)
    if (charsResult.isLeft) {
      // Return dummy Symbol for empty input since SeqOf requires at least one element
      val dummySpan = Span(source, SpanInFile(Pos.zero, Pos.zero))
      return (CST.Symbol("<empty>", dummySpan), reporter.getReports)
    }
    val chars = charsResult.toOption.get
    val tokensResult = Tokenizer.tokenize(chars)
    if (tokensResult.isLeft) {
      // Return dummy Symbol for tokenization failures
      val dummySpan = Span(source, SpanInFile(Pos.zero, Pos.zero))
      return (CST.Symbol("<error>", dummySpan), reporter.getReports)
    }
    val tokens = tokensResult.toOption.get
    val result = Parser.parse(tokens)
    (result.cst, reporter.getReports)
  }

  test("parse integer literal") {
    val (cst, errors) = parseString("42")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.IntegerLiteral(value, _) => assertEquals(value, BigInt(42))
      case _                            => fail(s"Expected IntegerLiteral, got: $cst")
    }
  }

  test("parse string literal") {
    val (cst, errors) = parseString("\"hello\"")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.StringLiteral(value, _) => assertEquals(value, "hello")
      case _                           => fail(s"Expected StringLiteral, got: $cst")
    }
  }

  test("parse symbol literal") {
    val (cst, errors) = parseString("'foo")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Symbol(name, _) => assertEquals(name, "foo")
      case _                   => fail(s"Expected Symbol, got: $cst")
    }
  }

  test("parse identifier") {
    val (cst, errors) = parseString("myVar")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Symbol(name, _) => assertEquals(name, "myVar")
      case _                   => fail(s"Expected Symbol, got: $cst")
    }
  }

  test("parse empty tuple") {
    val (cst, errors) = parseString("()")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Tuple(elements, _) => assertEquals(elements.length, 0)
      case _                      => fail(s"Expected Tuple, got: $cst")
    }
  }

  test("parse tuple with single element") {
    val (cst, errors) = parseString("(42)")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Tuple(elements, _) =>
        assertEquals(elements.length, 1)
        elements(0) match {
          case CST.IntegerLiteral(value, _) => assertEquals(value, BigInt(42))
          case _                            => fail(s"Expected IntegerLiteral, got: ${elements(0)}")
        }
      case _ => fail(s"Expected Tuple, got: $cst")
    }
  }

  test("parse tuple with multiple elements") {
    val (cst, errors) = parseString("(1, 2, 3)")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Tuple(elements, _) =>
        assertEquals(elements.length, 3)
        elements.zipWithIndex.foreach { case (elem, idx) =>
          elem match {
            case CST.IntegerLiteral(value, _) => assertEquals(value, BigInt(idx + 1))
            case _                            => fail(s"Expected IntegerLiteral at index $idx, got: $elem")
          }
        }
      case _ => fail(s"Expected Tuple, got: $cst")
    }
  }

  test("parse nested tuple") {
    val (cst, errors) = parseString("(1, (2, 3))")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Tuple(elements, _) =>
        assertEquals(elements.length, 2)
        elements(0) match {
          case CST.IntegerLiteral(value, _) => assertEquals(value, BigInt(1))
          case _                            => fail(s"Expected IntegerLiteral, got: ${elements(0)}")
        }
        elements(1) match {
          case CST.Tuple(inner, _) => assertEquals(inner.length, 2)
          case _                   => fail(s"Expected Tuple, got: ${elements(1)}")
        }
      case _ => fail(s"Expected Tuple, got: $cst")
    }
  }

  test("parse empty list") {
    val (cst, errors) = parseString("[]")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.ListLiteral(elements, _) => assertEquals(elements.length, 0)
      case _                            => fail(s"Expected ListLiteral, got: $cst")
    }
  }

  test("parse list with elements") {
    val (cst, errors) = parseString("[1, 2, 3]")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.ListLiteral(elements, _) =>
        assertEquals(elements.length, 3)
        elements.zipWithIndex.foreach { case (elem, idx) =>
          elem match {
            case CST.IntegerLiteral(value, _) => assertEquals(value, BigInt(idx + 1))
            case _                            => fail(s"Expected IntegerLiteral at index $idx, got: $elem")
          }
        }
      case _ => fail(s"Expected ListLiteral, got: $cst")
    }
  }

  test("parse nested list") {
    val (cst, errors) = parseString("[1, [2, 3]]")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.ListLiteral(elements, _) =>
        assertEquals(elements.length, 2)
        elements(0) match {
          case CST.IntegerLiteral(value, _) => assertEquals(value, BigInt(1))
          case _                            => fail(s"Expected IntegerLiteral, got: ${elements(0)}")
        }
        elements(1) match {
          case CST.ListLiteral(inner, _) => assertEquals(inner.length, 2)
          case _                         => fail(s"Expected ListLiteral, got: ${elements(1)}")
        }
      case _ => fail(s"Expected ListLiteral, got: $cst")
    }
  }

  test("parse mixed tuple and list") {
    val (cst, errors) = parseString("(1, [2, 3], 4)")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Tuple(elements, _) =>
        assertEquals(elements.length, 3)
        elements(1) match {
          case CST.ListLiteral(inner, _) => assertEquals(inner.length, 2)
          case _                         => fail(s"Expected ListLiteral, got: ${elements(1)}")
        }
      case _ => fail(s"Expected Tuple, got: $cst")
    }
  }

  test("error recovery: unclosed tuple") {
    val (cst, errors) = parseString("(1, 2")
    assert(errors.nonEmpty, "Expected errors for unclosed tuple")
    assert(errors.exists(_.message.contains("Expected ')'")))
    cst match {
      case CST.Tuple(elements, _) =>
        assertEquals(elements.length, 2, "Should recover and create tuple with 2 elements")
      case _ => fail(s"Expected Tuple for error recovery, got: $cst")
    }
  }

  test("error recovery: unclosed list") {
    val (cst, errors) = parseString("[1, 2")
    assert(errors.nonEmpty, "Expected errors for unclosed list")
    assert(errors.exists(_.message.contains("Expected ']'")))
    cst match {
      case CST.ListLiteral(elements, _) =>
        assertEquals(elements.length, 2, "Should recover and create list with 2 elements")
      case _ => fail(s"Expected ListLiteral for error recovery, got: $cst")
    }
  }

  test("error recovery: missing comma in tuple") {
    val (cst, errors) = parseString("(1 2)")
    assert(errors.nonEmpty, "Expected errors for missing comma")
    cst match {
      case CST.Tuple(elements, _) =>
        // Should skip unexpected token and continue parsing
        assert(elements.nonEmpty, "Should recover and parse some elements")
      case _ => fail(s"Expected Tuple for error recovery, got: $cst")
    }
  }

  test("parse tuple with whitespace and comments") {
    val (cst, errors) = parseString("( 1 , /* comment */ 2 )")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Tuple(elements, _) =>
        assertEquals(elements.length, 2)
      case _ => fail(s"Expected Tuple, got: $cst")
    }
  }

  test("parse complex expression") {
    val (cst, errors) = parseString("(fn, [1, 2], (a, b))")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Tuple(elements, _) =>
        assertEquals(elements.length, 3)
        elements(0) match {
          case CST.Symbol(name, _) => assertEquals(name, "fn")
          case _                   => fail(s"Expected Symbol, got: ${elements(0)}")
        }
        elements(1) match {
          case CST.ListLiteral(_, _) => // OK
          case _                     => fail(s"Expected ListLiteral, got: ${elements(1)}")
        }
        elements(2) match {
          case CST.Tuple(_, _) => // OK
          case _               => fail(s"Expected Tuple, got: ${elements(2)}")
        }
      case _ => fail(s"Expected Tuple, got: $cst")
    }
  }

  test("parse string with escapes") {
    val (cst, errors) = parseString("\"hello\\nworld\"")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.StringLiteral(value, _) => assertEquals(value, "hello\nworld")
      case _                           => fail(s"Expected StringLiteral, got: $cst")
    }
  }

  test("empty input produces dummy CST") {
    val (cst, errors) = parseString("")
    // Empty input gets handled and returns a Symbol since SeqOf requires at least one element
    cst match {
      case CST.Symbol(name, _) => assert(name == "<empty>" || name == "<error>")
      case _                   => fail(s"Expected Symbol for empty input, got: $cst")
    }
  }

  test("parse function call f(a) as SeqOf") {
    val (cst, errors) = parseString("f(a)")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.SeqOf(elements, _) =>
        val elemVec = elements.toVector
        assertEquals(elemVec.length, 2, "Expected SeqOf with 2 elements")
        elemVec(0) match {
          case CST.Symbol(name, _) => assertEquals(name, "f")
          case _                   => fail(s"Expected Symbol 'f', got: ${elemVec(0)}")
        }
        elemVec(1) match {
          case CST.Tuple(tupleElements, _) =>
            assertEquals(tupleElements.length, 1)
            tupleElements(0) match {
              case CST.Symbol(name, _) => assertEquals(name, "a")
              case _                   => fail(s"Expected Symbol 'a', got: ${tupleElements(0)}")
            }
          case _ => fail(s"Expected Tuple, got: ${elemVec(1)}")
        }
      case _ => fail(s"Expected SeqOf, got: $cst")
    }
  }

  test("parse multiple atoms a b c as SeqOf") {
    val (cst, errors) = parseString("a b c")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.SeqOf(elements, _) =>
        val elemVec = elements.toVector
        assertEquals(elemVec.length, 3)
        val names = elemVec.collect { case CST.Symbol(name, _) => name }
        assertEquals(names, Vector("a", "b", "c"))
      case _ => fail(s"Expected SeqOf, got: $cst")
    }
  }

  test("parse f(a, b) g(c) as SeqOf") {
    val (cst, errors) = parseString("f(a, b) g(c)")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.SeqOf(elements, _) =>
        val elemVec = elements.toVector
        assertEquals(elemVec.length, 4, "Expected f, (a,b), g, (c)")
        elemVec(0) match {
          case CST.Symbol(name, _) => assertEquals(name, "f")
          case _                   => fail(s"Expected Symbol 'f', got: ${elemVec(0)}")
        }
        elemVec(1) match {
          case CST.Tuple(tupleElements, _) => assertEquals(tupleElements.length, 2)
          case _                           => fail(s"Expected Tuple, got: ${elemVec(1)}")
        }
        elemVec(2) match {
          case CST.Symbol(name, _) => assertEquals(name, "g")
          case _                   => fail(s"Expected Symbol 'g', got: ${elemVec(2)}")
        }
        elemVec(3) match {
          case CST.Tuple(tupleElements, _) => assertEquals(tupleElements.length, 1)
          case _                           => fail(s"Expected Tuple, got: ${elemVec(3)}")
        }
      case _ => fail(s"Expected SeqOf, got: $cst")
    }
  }

  test("parse empty block") {
    val (cst, errors) = parseString("{}")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.length, 0)
        assertEquals(tail, None)
      case _ => fail(s"Expected Block, got: $cst")
    }
  }

  test("parse block with tail only") {
    val (cst, errors) = parseString("{a}")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.length, 0)
        tail match {
          case Some(CST.Symbol(name, _)) => assertEquals(name, "a")
          case _                         => fail(s"Expected Symbol 'a' as tail, got: $tail")
        }
      case _ => fail(s"Expected Block, got: $cst")
    }
  }

  test("parse block with elements and tail") {
    val (cst, errors) = parseString("{a;b}")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.length, 1)
        elements(0) match {
          case CST.Symbol(name, _) => assertEquals(name, "a")
          case _                   => fail(s"Expected Symbol 'a', got: ${elements(0)}")
        }
        tail match {
          case Some(CST.Symbol(name, _)) => assertEquals(name, "b")
          case _                         => fail(s"Expected Symbol 'b' as tail, got: $tail")
        }
      case _ => fail(s"Expected Block, got: $cst")
    }
  }

  test("parse block with elements only (no tail)") {
    val (cst, errors) = parseString("{a;b;}")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.length, 2)
        elements(0) match {
          case CST.Symbol(name, _) => assertEquals(name, "a")
          case _                   => fail(s"Expected Symbol 'a', got: ${elements(0)}")
        }
        elements(1) match {
          case CST.Symbol(name, _) => assertEquals(name, "b")
          case _                   => fail(s"Expected Symbol 'b', got: ${elements(1)}")
        }
        assertEquals(tail, None)
      case _ => fail(s"Expected Block, got: $cst")
    }
  }

  test("parse block with multiple elements and tail") {
    val (cst, errors) = parseString("{a;b;c}")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.length, 2)
        elements(0) match {
          case CST.Symbol(name, _) => assertEquals(name, "a")
          case _                   => fail(s"Expected Symbol 'a', got: ${elements(0)}")
        }
        elements(1) match {
          case CST.Symbol(name, _) => assertEquals(name, "b")
          case _                   => fail(s"Expected Symbol 'b', got: ${elements(1)}")
        }
        tail match {
          case Some(CST.Symbol(name, _)) => assertEquals(name, "c")
          case _                         => fail(s"Expected Symbol 'c' as tail, got: $tail")
        }
      case _ => fail(s"Expected Block, got: $cst")
    }
  }

  test("parse nested block") {
    val (cst, errors) = parseString("{a;{b;c}}")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.length, 1)
        elements(0) match {
          case CST.Symbol(name, _) => assertEquals(name, "a")
          case _                   => fail(s"Expected Symbol 'a', got: ${elements(0)}")
        }
        tail match {
          case Some(CST.Block(innerElements, innerTail, _)) =>
            assertEquals(innerElements.length, 1)
            innerElements(0) match {
              case CST.Symbol(name, _) => assertEquals(name, "b")
              case _                   => fail(s"Expected Symbol 'b', got: ${innerElements(0)}")
            }
            innerTail match {
              case Some(CST.Symbol(name, _)) => assertEquals(name, "c")
              case _                         => fail(s"Expected Symbol 'c' as inner tail, got: $innerTail")
            }
          case _ => fail(s"Expected Block as tail, got: $tail")
        }
      case _ => fail(s"Expected Block, got: $cst")
    }
  }

  test("parse block with integers") {
    val (cst, errors) = parseString("{1;2;3}")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.length, 2)
        elements.zipWithIndex.foreach { case (elem, idx) =>
          elem match {
            case CST.IntegerLiteral(value, _) => assertEquals(value, BigInt(idx + 1))
            case _                            => fail(s"Expected IntegerLiteral at index $idx, got: $elem")
          }
        }
        tail match {
          case Some(CST.IntegerLiteral(value, _)) => assertEquals(value, BigInt(3))
          case _                                  => fail(s"Expected IntegerLiteral 3 as tail, got: $tail")
        }
      case _ => fail(s"Expected Block, got: $cst")
    }
  }

  test("error recovery: unclosed block") {
    val (cst, errors) = parseString("{a;b")
    assert(errors.nonEmpty, "Expected errors for unclosed block")
    assert(errors.exists(_.message.contains("Expected '}'")))
    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.length, 1, "Should recover and create block with 1 element")
        tail match {
          case Some(CST.Symbol(name, _)) => assertEquals(name, "b")
          case _                         => fail(s"Expected Symbol 'b' as tail, got: $tail")
        }
      case _ => fail(s"Expected Block for error recovery, got: $cst")
    }
  }

  test("parse block with function call - well formed") {
    val (cst, errors) = parseString("{print(a);b}")
    assert(errors.isEmpty, s"Expected no errors, got: $errors")
    cst match {
      case CST.Block(elements, tail, _) =>
        // Should have 1 element: SeqOf(print, (a)) before semicolon
        assertEquals(elements.length, 1, s"Should have 1 element, got ${elements.length}: ${elements.map(_.getClass.getSimpleName)}")
        elements(0) match {
          case CST.SeqOf(seqElements, _) =>
            val seqVec = seqElements.toVector
            assertEquals(seqVec.length, 2, "Expected SeqOf with 2 elements (print and tuple)")
            seqVec(0) match {
              case CST.Symbol(name, _) => assertEquals(name, "print")
              case _                   => fail(s"Expected Symbol 'print', got: ${seqVec(0)}")
            }
            seqVec(1) match {
              case CST.Tuple(tupleElements, _) =>
                assertEquals(tupleElements.length, 1)
                tupleElements(0) match {
                  case CST.Symbol(name, _) => assertEquals(name, "a")
                  case _                   => fail(s"Expected Symbol 'a', got: ${tupleElements(0)}")
                }
              case _ => fail(s"Expected Tuple, got: ${seqVec(1)}")
            }
          case _ => fail(s"Expected SeqOf, got: ${elements(0)}")
        }
        // Tail should be 'b'
        tail match {
          case Some(CST.Symbol(name, _)) => assertEquals(name, "b")
          case _                         => fail(s"Expected Symbol 'b' as tail, got: $tail")
        }
      case _ => fail(s"Expected Block, got: $cst")
    }
  }

  test("error recovery: complex block with unclosed tuple") {
    val (cst, errors) = parseString("{print(a;b}")
    // Should have errors - parser will recover from unclosed structures
    assert(errors.nonEmpty, s"Expected errors for malformed block with unclosed tuple")
    cst match {
      case CST.Block(elements, tail, _) =>
        // With improved error recovery: tuple parser stops at semicolon
        // Result should be: elements=[SeqOf(print, (a))], tail=b
        assertEquals(elements.length, 1, s"Should have 1 element before semicolon, got ${elements.length}: ${elements.map(_.getClass.getSimpleName)}")
        elements(0) match {
          case CST.SeqOf(seqElements, _) =>
            val seqVec = seqElements.toVector
            assertEquals(seqVec.length, 2, "Expected SeqOf with 2 elements (print and tuple)")
            seqVec(0) match {
              case CST.Symbol(name, _) => assertEquals(name, "print")
              case _                   => fail(s"Expected Symbol 'print', got: ${seqVec(0)}")
            }
            seqVec(1) match {
              case CST.Tuple(tupleElements, _) =>
                assertEquals(tupleElements.length, 1, "Tuple should recover with 1 element")
                tupleElements(0) match {
                  case CST.Symbol(name, _) => assertEquals(name, "a")
                  case _                   => fail(s"Expected Symbol 'a', got: ${tupleElements(0)}")
                }
              case _ => fail(s"Expected Tuple, got: ${seqVec(1)}")
            }
          case _ => fail(s"Expected SeqOf, got: ${elements(0)}")
        }
        // Tail should be 'b'
        tail match {
          case Some(CST.Symbol(name, _)) => assertEquals(name, "b")
          case _                         => fail(s"Expected Symbol 'b' as tail, got: $tail")
        }
      case _ => fail(s"Expected Block for error recovery, got: $cst")
    }
  }
}
