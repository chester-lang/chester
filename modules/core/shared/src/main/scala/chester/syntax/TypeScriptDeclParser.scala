package chester.syntax

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable

import chester.error.{Pos, Span, SpanInFile}
import chester.reader.Source
import chester.utils.{Nat, WithUTF16}
import spire.math.Natural

/** Simple parser for TypeScript declaration files (.d.ts)
  *
  * This parser provides basic support for parsing TypeScript declarations to enable binding generation for Chester code interacting with TypeScript
  * libraries.
  */
class TypeScriptDeclParser(source: String, sourceRef: Source):
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
    Character.isLetter(cp) || cp == '_' || cp == '$'

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
    // Consume leading //
    advance()
    advance()
    while !eof && !peekChar('\n') do advance()
  }

  private def skipBlockComment(): Unit = {
    // Consume leading /*
    advance()
    advance()
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

  private def readIdentifier(skip: Boolean = true): Option[(String, Cursor, Cursor)] = {
    if skip then skipTrivia()
    peekCodePoint match
      case Some(cp) if isIdentStart(cp) =>
        val start = cursor
        val builder = new java.lang.StringBuilder
        builder.appendCodePoint(cp)
        advance()
        while peekCodePoint.exists(isIdentPart) do
          builder.appendCodePoint(peekCodePoint.get)
          advance()
        Some((builder.toString, start, cursor))
      case _ => None
  }

  private def consumeKeyword(word: String, skip: Boolean = true): Option[(Cursor, Cursor)] = {
    val saved = cursor
    readIdentifier(skip) match
      case Some((name, start, end)) if name == word => Some((start, end))
      case _ =>
        cursor = saved
        None
  }

  private def maybeConsume(ch: Char): Boolean = {
    skipTrivia()
    if peekChar(ch) then
      advance()
      true
    else false
  }

  private def expectChar(ch: Char): Boolean =
    if maybeConsume(ch) then true else false

  private def skipUnknownToken(): Unit =
    if !eof then advance()

  /** Parse a TypeScript declaration file into a TypeScriptAST.Program */
  def parse(): TypeScriptAST = {
    val statements = mutable.ArrayBuffer[TypeScriptAST]()
    val programStart = cursor

    skipTrivia()
    while !eof do
      parseStatement() match
        case Some(stmt) => statements += stmt
        case None       => skipUnknownToken()
      skipTrivia()

    val span = spanFrom(programStart, cursor)
    TypeScriptAST.Program(statements.toVector, span)
  }

  private def parseStatement(): Option[TypeScriptAST] = {
    if eof then None
    else {
      consumeKeyword("interface", skip = true)
        .map { case (start, _) => parseInterface(start) }
        .orElse(consumeKeyword("type", skip = true).map { case (start, _) => parseTypeAlias(start) })
        .orElse(consumeKeyword("declare", skip = true).map { case (start, _) => parseDeclaration(start) })
        .orElse(consumeKeyword("export", skip = true).map { case (start, _) => parseExport(start) })
    }
  }

  private def parseInterface(start: Cursor): TypeScriptAST = {
    val name = readIdentifier().map(_._1).getOrElse("Unknown")
    val typeParams = Vector.empty[TypeParameter]
    val extendsTypes = Vector.empty[TypeScriptType]
    val members = parseMemberBlock()
    TypeScriptAST.InterfaceDeclaration(name, typeParams, extendsTypes, members, spanFrom(start, cursor))
  }

  private def parseMemberBlock(): Vector[InterfaceMember] = {
    val members = mutable.ArrayBuffer[InterfaceMember]()
    if expectChar('{') then
      skipTrivia()
      while !peekChar('}') && !eof do
        readIdentifier(skip = false) match
          case Some((name, memberStart, _)) =>
            val isOptional = maybeConsume('?')
            if expectChar(':') then
              val memberType = parseTypeReference()
              val span = spanFrom(memberStart, cursor)
              members += InterfaceMember(name, InterfaceMemberType.PropertySignature(memberType, isOptional, false), span)
              maybeConsume(';')
            else {
              // Skip until end of line to avoid infinite loops
              skipToLineEnd()
            }
          case None =>
            skipUnknownToken()
        skipTrivia()
      expectChar('}')
    members.toVector
  }

  private def skipToLineEnd(): Unit =
    while !eof && !peekChar('\n') && !peekChar('}') do advance()

  private def parseTypeReference(): TypeScriptType = {
    val start = cursor
    val types = mutable.ArrayBuffer[TypeScriptType]()
    types += parseSimpleType()
    skipTrivia()
    while maybeConsume('|') do
      types += parseSimpleType()
      skipTrivia()
    if types.length == 1 then types.head else TypeScriptType.UnionType(types.toVector, spanFrom(start, cursor))
  }

  private def parseSimpleType(): TypeScriptType = {
    val startCursor = cursor
    val (name, nameEnd) = {
      readIdentifier() match
        case Some((value, _, end)) => (value, end)
        case None                  => ("any", cursor)
    }
    val baseSpan = spanFrom(startCursor, nameEnd)
    val base = {
      if Set("string", "number", "boolean", "void", "any", "unknown", "never").contains(name) then TypeScriptType.PrimitiveType(name, baseSpan)
      else TypeScriptType.TypeReference(name, Vector.empty, baseSpan)
    }
    parseArraySuffix(base, startCursor)
  }

  private def parseArraySuffix(base: TypeScriptType, start: Cursor): TypeScriptType = {
    skipTrivia()
    if startsWith("[]") then
      advance()
      advance()
      TypeScriptType.ArrayType(base, spanFrom(start, cursor))
    else base
  }

  private def parseTypeAlias(start: Cursor): TypeScriptAST = {
    val name = readIdentifier().getOrElse(("Unknown", cursor, cursor))._1
    expectChar('=')
    val aliasType = parseTypeReference()
    maybeConsume(';')
    TypeScriptAST.TypeAliasDeclaration(name, Vector.empty, aliasType, spanFrom(start, cursor))
  }

  private def parseDeclaration(start: Cursor): TypeScriptAST = {
    skipTrivia()
    consumeKeyword("function", skip = false)
      .map(_ => parseDeclareFunction(start))
      .orElse(parseDeclareVariable(start).orElse(Some(TypeScriptAST.Empty(spanFrom(start, cursor)))))
      .get
  }

  private def parseDeclareFunction(start: Cursor): TypeScriptAST = {
    val name = readIdentifier().map(_._1).getOrElse("anonymous")
    val params = parseParameterList()
    val returnType = if maybeConsume(':') then Some(parseTypeReference()) else None
    val span = spanFrom(start, cursor)
    TypeScriptAST.FunctionDeclaration(
      name,
      params,
      returnType,
      TypeScriptAST.Block(Vector.empty, span),
      Vector(Modifier.Declare),
      span
    )
  }

  private def parseParameterList(): Vector[Parameter] = {
    if !expectChar('(') then return Vector.empty
    val params = mutable.ArrayBuffer[Parameter]()
    skipTrivia()
    while !peekChar(')') && !eof do
      val paramStart = cursor
      val isRest = startsWith("...")
      if isRest then
        advance()
        advance()
        advance()
      val name = readIdentifier(skip = false).map(_._1).getOrElse("param" + params.length)
      val paramType = if maybeConsume(':') then Some(parseTypeReference()) else None
      params += Parameter(name, paramType, None, isRest, spanFrom(paramStart, cursor))
      skipTrivia()
      if peekChar(',') then
        advance()
        skipTrivia()
    expectChar(')')
    params.toVector
  }

  private def parseDeclareVariable(start: Cursor): Option[TypeScriptAST] = {
    val kindOpt = {
      if consumeKeyword("const", skip = false).isDefined then Some(VarKind.Const)
      else if consumeKeyword("let", skip = false).isDefined then Some(VarKind.Let)
      else if consumeKeyword("var", skip = false).isDefined then Some(VarKind.Var)
      else None
    }
    kindOpt.map { kind =>
      val declarators = mutable.ArrayBuffer[VariableDeclarator]()
      skipTrivia()
      var keepGoing = true
      while keepGoing && !eof do
        val declStart = cursor
        val name = readIdentifier(skip = false).map(_._1).getOrElse("value")
        val varType = if maybeConsume(':') then Some(parseTypeReference()) else None
        declarators += VariableDeclarator(name, varType, None, spanFrom(declStart, cursor))
        skipTrivia()
        if peekChar(',') then
          advance()
          skipTrivia()
        else keepGoing = false
      maybeConsume(';')
      TypeScriptAST.VariableDeclaration(kind, declarators.toVector, spanFrom(start, cursor))
    }
  }

  private def parseExport(start: Cursor): TypeScriptAST = {
    skipTrivia()
    val isDefault = consumeKeyword("default", skip = false).isDefined
    val inner = parseStatement().orElse {
      skipUnknownToken()
      None
    }
    TypeScriptAST.ExportDeclaration(inner, Vector.empty, None, isDefault, spanFrom(start, cursor))
  }

object TypeScriptDeclParser:
  /** Parse a TypeScript declaration file */
  def parse(source: String, sourceRef: Source): TypeScriptAST =
    new TypeScriptDeclParser(source, sourceRef).parse()
