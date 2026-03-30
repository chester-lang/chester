package chester.syntax

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable

import chester.error.{Pos, Span, SpanInFile}
import chester.reader.Source
import chester.utils.{Nat, WithUTF16}
import spire.math.Natural

/** Lightweight parser for Go source files to extract type signatures.
  *
  * This parser extracts exported (capitalized) declarations:
  *   - Function signatures: `func Name(params) returns`
  *   - Struct types: `type Name struct { fields }`
  *   - Interface types: `type Name interface { methods }`
  *
  * Unlike a full Go parser, this focuses on extracting type information for Chester FFI bindings, not compiling Go code.
  */
class GoDeclParser(source: String, sourceRef: Source):
  private case class Cursor(index: WithUTF16, line: Natural, column: WithUTF16, utf16Index: Int):
    def toPos: Pos = Pos(index, line, column)

    def advance(cp: Int): Cursor = {
      val step = WithUTF16(Nat(1), Nat(Character.charCount(cp)))
      val nextIndex = index + step
      val nextUtf16 = utf16Index + Character.charCount(cp)
      if cp == '\n' then Cursor(nextIndex, line + Nat(1), WithUTF16.Zero, nextUtf16)
      else Cursor(nextIndex, line, column + step, nextUtf16)
    }

  private var cursor: Cursor = Cursor(WithUTF16.Zero, Nat(0), WithUTF16.Zero, 0)

  private def eof: Boolean = cursor.utf16Index >= source.length

  private def peekCodePoint: Option[Int] =
    if eof then None else Some(source.codePointAt(cursor.utf16Index))

  private def advance(): Option[Int] = {
    peekCodePoint.map { cp =>
      cursor = cursor.advance(cp)
      cp
    }
  }

  private def spanFrom(start: Cursor, end: Cursor): Option[Span] =
    Some(Span(sourceRef, SpanInFile(start.toPos, end.toPos)))

  private def isIdentStart(cp: Int): Boolean =
    Character.isLetter(cp) || cp == '_'

  private def isIdentPart(cp: Int): Boolean =
    isIdentStart(cp) || Character.isDigit(cp)

  private def skipTrivia(): Unit = {
    var keepGoing = true
    while keepGoing && !eof do
      peekCodePoint match
        case Some(cp) if Character.isWhitespace(cp) => advance()
        case Some('/') if startsWith("//") =>
          skipLineComment()
        case Some('/') if startsWith("/*") =>
          skipBlockComment()
        case _ =>
          keepGoing = false
  }

  private def skipLineComment(): Unit = {
    advance() // '/'
    advance() // '/'
    while !eof && !peekChar('\n') do advance()
  }

  private def skipBlockComment(): Unit = {
    advance() // '/'
    advance() // '*'
    while !eof do
      if startsWith("*/") then
        advance()
        advance()
        return
      else advance()
  }

  private def peekChar(ch: Char): Boolean =
    peekCodePoint.contains(ch.toInt)

  private def startsWith(str: String): Boolean =
    source.regionMatches(cursor.utf16Index, str, 0, str.length)

  private def consumeKeyword(word: String): Boolean = {
    val saved = cursor
    skipTrivia()
    readIdentifier() match
      case Some(name) if name == word => true
      case _ =>
        cursor = saved
        false
  }

  private def readIdentifier(): Option[String] = {
    skipTrivia()
    peekCodePoint match
      case Some(cp) if isIdentStart(cp) =>
        val builder = new java.lang.StringBuilder
        builder.appendCodePoint(cp)
        advance()
        while peekCodePoint.exists(isIdentPart) do
          builder.appendCodePoint(peekCodePoint.get)
          advance()
        Some(builder.toString)
      case _ => None
  }

  private def maybeConsume(ch: Char): Boolean = {
    skipTrivia()
    if peekChar(ch) then
      advance()
      true
    else false
  }

  private def skipBalanced(open: Char, close: Char): Unit = {
    if !maybeConsume(open) then return
    var depth = 1
    while depth > 0 && !eof do
      peekCodePoint match
        case Some(cp) if cp == open.toInt =>
          depth += 1
          advance()
        case Some(cp) if cp == close.toInt =>
          depth -= 1
          advance()
        case Some('/') if startsWith("//") =>
          skipLineComment()
        case Some('/') if startsWith("/*") =>
          skipBlockComment()
        case _ =>
          advance()
  }

  private def captureBalancedContent(open: Char, close: Char): Option[String] = {
    skipTrivia()
    if !peekChar(open) then return None

    advance() // open
    val startIndex = cursor.utf16Index
    var depth = 1

    while depth > 0 && !eof do
      peekCodePoint match
        case Some(cp) if cp == open.toInt =>
          depth += 1
          advance()
        case Some(cp) if cp == close.toInt =>
          depth -= 1
          if depth == 0 then
            val endIndex = cursor.utf16Index
            advance() // close
            return Some(source.substring(startIndex, endIndex))
          advance()
        case Some('/') if startsWith("//") =>
          skipLineComment()
        case Some('/') if startsWith("/*") =>
          skipBlockComment()
        case _ =>
          advance()

    None
  }

  /** Parse Go source and extract type information in JSON format. Returns a ujson.Value with structure matching go-type-extractor output.
    */
  def parseToJson(): ujson.Value = {
    val packageName = extractPackageName()
    val functions = mutable.ArrayBuffer[ujson.Value]()
    val structs = mutable.ArrayBuffer[ujson.Value]()
    val interfaces = mutable.ArrayBuffer[ujson.Value]()

    skipTrivia()
    while !eof do
      if consumeKeyword("func") then parseFunctionDecl().foreach(functions += _)
      else if consumeKeyword("type") then
        parseTypeDecl() match
          case Some(("struct", name, value))    => structs += value
          case Some(("interface", name, value)) => interfaces += value
          case _                                => ()
      else {
        // Skip unknown content
        advance()
      }
      skipTrivia()

    ujson.Obj(
      "package" -> packageName,
      "functions" -> functions.toVector,
      "structs" -> structs.toVector,
      "interfaces" -> interfaces.toVector
    )
  }

  private def extractPackageName(): String = {
    val saved = cursor
    cursor = Cursor(WithUTF16.Zero, Nat(0), WithUTF16.Zero, 0)
    skipTrivia()
    if consumeKeyword("package") then
      val name = readIdentifier().getOrElse("main")
      cursor = saved
      name
    else {
      cursor = saved
      "main"
    }
  }

  private def parseFunctionDecl(): Option[ujson.Value] = {
    // func Name(params) (results)
    val name = readIdentifier().getOrElse("unknown")
    if !name.headOption.exists(_.isUpper) then return None // Only exported functions

    val params = parseParamList(captureBalancedContent('(', ')').getOrElse(""))

    // Check for return type
    skipTrivia()
    val results = {
      if peekChar('(') then
        // Multiple returns
        parseResultList(captureBalancedContent('(', ')').getOrElse(""))
      else if peekCodePoint.exists(cp => isIdentStart(cp) || cp == '*' || cp == '[') then
        // Single return type
        parseSingleResultType()
      else ujson.Arr()
    }

    skipToStatementEnd()

    Some(
      ujson.Obj(
        "name" -> name,
        "params" -> params,
        "results" -> results
      )
    )
  }

  private def parseTypeDecl(): Option[(String, String, ujson.Value)] = {
    val name = readIdentifier().getOrElse("unknown")
    if !name.headOption.exists(_.isUpper) then return None // Only exported types

    skipTrivia()
    if consumeKeyword("struct") then
      val fields = parseStructFields()
      Some(
        (
          "struct",
          name,
          ujson.Obj(
            "name" -> name,
            "fields" -> fields
          )
        )
      )
    else if consumeKeyword("interface") then
      val methods = parseInterfaceMethods()
      Some(
        (
          "interface",
          name,
          ujson.Obj(
            "name" -> name,
            "methods" -> methods
          )
        )
      )
    else {
      // Other type (alias, etc.) - skip
      skipToStatementEnd()
      None
    }
  }

  private def parseStructFields(): ujson.Arr = {
    val fields = mutable.ArrayBuffer[ujson.Value]()
    if !maybeConsume('{') then return ujson.Arr()

    while !peekChar('}') && !eof do
      readIdentifier() match
        case Some(fieldName) if fieldName.headOption.exists(_.isUpper) =>
          skipTrivia()
          val typeName = readTypeName()
          fields += ujson.Obj(
            "name" -> fieldName,
            "type" -> typeName
          )
          skipToNextField()
        case _ =>
          skipToNextField()

    maybeConsume('}')
    ujson.Arr(fields.toVector*)
  }

  private def parseInterfaceMethods(): ujson.Arr = {
    val methods = mutable.ArrayBuffer[ujson.Value]()
    if !maybeConsume('{') then return ujson.Arr()

    while !peekChar('}') && !eof do
      readIdentifier() match
        case Some(methodName) if methodName.headOption.exists(_.isUpper) =>
          val params = parseParamList(captureBalancedContent('(', ')').getOrElse(""))
          skipTrivia()
          val results = {
            if peekChar('(') then parseResultList(captureBalancedContent('(', ')').getOrElse(""))
            else if peekCodePoint.exists(cp => isIdentStart(cp) || cp == '*' || cp == '[') then parseSingleResultType()
            else ujson.Arr()
          }

          methods += ujson.Obj(
            "name" -> methodName,
            "params" -> params,
            "results" -> results
          )
          skipToNextField()
        case _ =>
          skipToNextField()

    maybeConsume('}')
    ujson.Arr(methods.toVector*)
  }

  private def parseParamList(content: String): ujson.Arr =
    parseFieldList(content, includeNames = true)

  private def parseResultList(content: String): ujson.Arr =
    parseFieldList(content, includeNames = true)

  private def parseFieldList(content: String, includeNames: Boolean): ujson.Arr = {
    val entries = mutable.ArrayBuffer.empty[ujson.Value]
    val pendingNames = mutable.ArrayBuffer.empty[String]

    val segments = splitTopLevel(content, ',')
    segments.zipWithIndex.foreach { case (segment, index) =>
      val trimmed = segment.trim
      if trimmed.nonEmpty then
        firstTopLevelWhitespace(trimmed) match
          case Some(idx) =>
            val prefix = trimmed.take(idx).trim
            val suffix = normalizeTypeExpression(trimmed.drop(idx).trim)
            if includeNames && isNameList(prefix) && suffix.nonEmpty then
              val names = pendingNames.toVector ++ prefix
                .split(',')
                .iterator
                .map(_.trim)
                .filter(_.nonEmpty)
                .toVector
              entries ++= names.map(name => ujson.Obj("name" -> name, "type" -> suffix))
              pendingNames.clear()
            else {
              entries += ujson.Obj("type" -> normalizeTypeExpression(trimmed))
              pendingNames.clear()
            }
          case None =>
            if includeNames && isIdentifier(trimmed) && index < segments.length - 1 then pendingNames += trimmed
            else {
              entries += ujson.Obj("type" -> normalizeTypeExpression(trimmed))
              pendingNames.clear()
            }
    }

    ujson.Arr(entries.toVector*)
  }

  private def parseSingleResultType(): ujson.Arr = {
    val resultType = captureTypeExpression()
    if resultType.isEmpty then ujson.Arr()
    else ujson.Arr(ujson.Obj("type" -> resultType))
  }

  private def captureTypeExpression(): String = {
    skipTrivia()
    val startIndex = cursor.utf16Index
    var parenDepth = 0
    var bracketDepth = 0
    var braceDepth = 0
    var done = false

    while !eof && !done do
      peekCodePoint match
        case Some('/') if parenDepth == 0 && bracketDepth == 0 && braceDepth == 0 && (startsWith("//") || startsWith("/*")) =>
          done = true
        case Some(',') | Some(')') if parenDepth == 0 && bracketDepth == 0 && braceDepth == 0 =>
          done = true
        case Some('{') if parenDepth == 0 && bracketDepth == 0 && braceDepth == 0 =>
          done = true
        case Some('\n') | Some(';') if parenDepth == 0 && bracketDepth == 0 && braceDepth == 0 =>
          done = true
        case Some('(') =>
          parenDepth += 1
          advance()
        case Some(')') =>
          parenDepth -= 1
          advance()
        case Some('[') =>
          bracketDepth += 1
          advance()
        case Some(']') =>
          bracketDepth -= 1
          advance()
        case Some('{') =>
          braceDepth += 1
          advance()
        case Some('}') =>
          braceDepth -= 1
          advance()
        case _ =>
          advance()

    normalizeTypeExpression(source.substring(startIndex, cursor.utf16Index))
  }

  private def splitTopLevel(input: String, delimiter: Char): Vector[String] = {
    val parts = mutable.ArrayBuffer.empty[String]
    val current = new java.lang.StringBuilder
    var parenDepth = 0
    var bracketDepth = 0
    var braceDepth = 0

    input.foreach { ch =>
      ch match
        case '(' =>
          parenDepth += 1
          current.append(ch)
        case ')' =>
          parenDepth -= 1
          current.append(ch)
        case '[' =>
          bracketDepth += 1
          current.append(ch)
        case ']' =>
          bracketDepth -= 1
          current.append(ch)
        case '{' =>
          braceDepth += 1
          current.append(ch)
        case '}' =>
          braceDepth -= 1
          current.append(ch)
        case c if c == delimiter && parenDepth == 0 && bracketDepth == 0 && braceDepth == 0 =>
          parts += current.toString
          current.setLength(0)
        case _ =>
          current.append(ch)
    }

    parts += current.toString
    parts.toVector
  }

  private def firstTopLevelWhitespace(input: String): Option[Int] = {
    var parenDepth = 0
    var bracketDepth = 0
    var braceDepth = 0
    var idx = 0

    while idx < input.length do
      input.charAt(idx) match
        case '(' => parenDepth += 1
        case ')' => parenDepth -= 1
        case '[' => bracketDepth += 1
        case ']' => bracketDepth -= 1
        case '{' => braceDepth += 1
        case '}' => braceDepth -= 1
        case ch if ch.isWhitespace && parenDepth == 0 && bracketDepth == 0 && braceDepth == 0 =>
          return Some(idx)
        case _ => ()
      idx += 1

    None
  }

  private def isNameList(prefix: String): Boolean = {
    val reservedTypeStarters = Set("chan", "func", "map", "struct", "interface")
    val names = prefix.split(',').iterator.map(_.trim).filter(_.nonEmpty).toVector
    names.nonEmpty && !reservedTypeStarters.contains(prefix) && names.forall(_.forall(ch => ch == '_' || ch.isLetterOrDigit))
  }

  private def isIdentifier(value: String): Boolean =
    value.nonEmpty && value.forall(ch => ch == '_' || ch.isLetterOrDigit)

  private def normalizeTypeExpression(input: String): String = {
    val builder = new java.lang.StringBuilder
    var pendingSpace = false

    def isTightPunctuation(ch: Char): Boolean =
      "[]{}(),.*".contains(ch)

    input.trim.foreach { ch =>
      if ch.isWhitespace then pendingSpace = true
      else {
        if pendingSpace && builder.length > 0 then
          val prev = builder.charAt(builder.length - 1)
          if !isTightPunctuation(prev) && !isTightPunctuation(ch) then builder.append(' ')
        builder.append(ch)
        pendingSpace = false
      }
    }

    builder.toString
  }

  private def readTypeName(): String = {
    skipTrivia()
    val builder = new java.lang.StringBuilder

    // Handle pointer
    while peekChar('*') do
      builder.append('*')
      advance()
      skipTrivia()

    // Handle slice/array
    if peekChar('[') then
      builder.append('[')
      advance()
      if peekChar(']') then
        builder.append(']')
        advance()
      else {
        skipBalanced('[', ']')
        builder.append(']')
      }
      skipTrivia()

    // Read the base type name
    readIdentifier() match
      case Some(name) =>
        builder.append(name)
        // Check for package qualification
        if maybeConsume('.') then
          builder.append('.')
          readIdentifier() match
            case Some(typeName) => builder.append(typeName)
            case _              => ()
      case _ => ()

    val result = builder.toString
    if result.isEmpty then "any" else result
  }

  private def skipTypeExpression(): Unit = {
    skipTrivia()
    if peekChar('[') then skipBalanced('[', ']')
    if peekChar('*') then advance()
    readIdentifier()
    if maybeConsume('.') then readIdentifier()
  }

  private def skipToNextField(): Unit = {
    // Skip to newline or closing brace
    while !eof && !peekChar('\n') && !peekChar('}') do
      if peekChar('{') then skipBalanced('{', '}')
      else advance()
    if peekChar('\n') then advance()
  }

  private def skipToStatementEnd(): Unit = {
    // Skip to newline, semicolon, or closing brace
    var braceDepth = 0
    while !eof do
      peekCodePoint match
        case Some('{') =>
          braceDepth += 1
          advance()
        case Some('}') if braceDepth > 0 =>
          braceDepth -= 1
          advance()
        case Some('}') | Some('\n') | Some(';') if braceDepth == 0 =>
          return
        case _ =>
          advance()
  }

object GoDeclParser:
  /** Parse Go source file and return type information as JSON */
  def parse(source: String, sourceRef: Source): ujson.Value =
    new GoDeclParser(source, sourceRef).parseToJson()
