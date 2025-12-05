package chester.reader

import chester.error.{Span, SpanInFile, Pos}
import chester.reader.{Source, Offset}
import chester.utils.WithUTF16

import scala.language.experimental.genericNumberLiterals
class TokenizerTest extends munit.FunSuite {

  private def createTestSource(content: String): Source = {
    Source(FileNameAndContent("test.chester", content))
  }

  private def tokenizeString(input: String): Either[ParseError, Vector[Token]] = {
    val source = createTestSource(input)
    for {
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    } yield tokens
  }

  test("tokenize empty input should fail") {
    val result = tokenizeString("")
    assert(result.isLeft)
  }

  test("tokenize single character tokens") {
    val result = tokenizeString("()[]{},.;@#")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 11)
    assert(tokens(0).isInstanceOf[Token.LParen])
    assert(tokens(1).isInstanceOf[Token.RParen])
    assert(tokens(2).isInstanceOf[Token.LBracket])
    assert(tokens(3).isInstanceOf[Token.RBracket])
    assert(tokens(4).isInstanceOf[Token.LBrace])
    assert(tokens(5).isInstanceOf[Token.RBrace])
    assert(tokens(6).isInstanceOf[Token.Comma])
    assert(tokens(7).isInstanceOf[Token.Dot])
    assert(tokens(8).isInstanceOf[Token.Semicolon])
    assert(tokens(9).isInstanceOf[Token.At])
    assert(tokens(10).isInstanceOf[Token.Hash])
  }

  test("tokenize integer literal") {
    val result = tokenizeString("12345")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.IntegerLiteral(value, _) => assertEquals(value, "12345")
      case _ => fail("Expected IntegerLiteral")
    }
  }

  test("tokenize integer literal with underscores") {
    val result = tokenizeString("1_000_000")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.IntegerLiteral(value, _) => assertEquals(value, "1000000")
      case _ => fail("Expected IntegerLiteral")
    }
  }

  test("tokenize rational literal") {
    val result = tokenizeString("22/7")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.RationalLiteral(value, _) => assertEquals(value, "22/7")
      case _ => fail("Expected RationalLiteral")
    }
  }

  test("tokenize simple identifier") {
    val result = tokenizeString("hello")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.Identifier(parts, _) => 
        assertEquals(parts.map(_.text).mkString, "hello")
      case _ => fail("Expected Identifier")
    }
  }

  test("tokenize identifier with underscore") {
    val result = tokenizeString("hello_world")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.Identifier(parts, _) => 
        assertEquals(parts.map(_.text).mkString, "hello_world")
      case _ => fail("Expected Identifier")
    }
  }

  test("tokenize operator identifier") {
    val result = tokenizeString("++")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.Identifier(parts, _) => 
        assertEquals(parts.map(_.text).mkString, "++")
      case _ => fail("Expected Identifier")
    }
  }

  test("tokenize string literal") {
    val result = tokenizeString("\"hello world\"")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.StringLiteral(chars, _) => 
        assertEquals(chars.map(_.text).mkString, "hello world")
      case _ => fail("Expected StringLiteral")
    }
  }

  test("tokenize string literal with escape sequences") {
    val result = tokenizeString("\"hello\\nworld\\t!\"")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.StringLiteral(chars, _) => 
        assertEquals(chars.map(_.text).mkString, "hello\nworld\t!")
      case _ => fail("Expected StringLiteral")
    }
  }

  test("tokenize unclosed string literal should fail") {
    val result = tokenizeString("\"hello")
    assert(result.isLeft)
  }

  test("tokenize symbol literal") {
    val result = tokenizeString("'mysymbol")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 1)
    tokens(0) match {
      case Token.SymbolLiteral(value, _) => 
        assertEquals(value, "mysymbol")
      case _ => fail("Expected SymbolLiteral")
    }
  }

  test("tokenize line comment is skipped") {
    val result = tokenizeString("hello // this is a comment\nworld")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 2)
    tokens(0) match {
      case Token.Identifier(parts, _) => 
        assertEquals(parts.map(_.text).mkString, "hello")
      case _ => fail("Expected Identifier")
    }
    tokens(1) match {
      case Token.Identifier(parts, _) => 
        assertEquals(parts.map(_.text).mkString, "world")
      case _ => fail("Expected Identifier")
    }
  }

  test("tokenize block comment is skipped") {
    val result = tokenizeString("hello /* comment */ world")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 2)
    tokens(0) match {
      case Token.Identifier(parts, _) => 
        assertEquals(parts.map(_.text).mkString, "hello")
      case _ => fail("Expected Identifier")
    }
    tokens(1) match {
      case Token.Identifier(parts, _) => 
        assertEquals(parts.map(_.text).mkString, "world")
      case _ => fail("Expected Identifier")
    }
  }

  test("tokenize nested block comments") {
    val result = tokenizeString("a /* outer /* inner */ still comment */ b")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 2)
  }

  test("tokenize complex expression") {
    val result = tokenizeString("(foo 123 \"bar\")")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 5)
    assert(tokens(0).isInstanceOf[Token.LParen])
    assert(tokens(1).isInstanceOf[Token.Identifier])
    assert(tokens(2).isInstanceOf[Token.IntegerLiteral])
    assert(tokens(3).isInstanceOf[Token.StringLiteral])
    assert(tokens(4).isInstanceOf[Token.RParen])
  }

  test("tokenize function definition syntax") {
    val result = tokenizeString("def add(x, y) { x + y }")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assert(tokens.length >= 10)
  }

  test("whitespace is filtered out") {
    val result = tokenizeString("  a   b  ")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 2)
  }

  test("tokenize multiple operators") {
    val result = tokenizeString("+ - * / == != <= >=")
    assert(result.isRight)
    val tokens = result.toOption.get.filterNot(_.isInstanceOf[Token.EOF])
    assertEquals(tokens.length, 8)
    tokens.foreach {
      case Token.Identifier(_, _) => // OK
      case other => fail(s"Expected Identifier, got $other")
    }
  }
}
