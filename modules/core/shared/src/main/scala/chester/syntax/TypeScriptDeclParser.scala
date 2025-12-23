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

  private def readStringLiteral(skip: Boolean = true): Option[(String, Cursor, Cursor)] = {
    if skip then skipTrivia()
    peekCodePoint match
      case Some(cp) if cp == '"'.toInt || cp == '\''.toInt =>
        val quote = cp.toChar
        val start = cursor
        advance() // opening quote
        val builder = new java.lang.StringBuilder
        while !eof && !peekChar(quote) do
          peekCodePoint match
            case Some('\\') =>
              advance()
              // Very small escape handling: keep escaped char verbatim if present.
              advance().foreach(cp => builder.appendCodePoint(cp))
            case Some(cp) =>
              builder.appendCodePoint(cp)
              advance()
            case None => ()
        if peekChar(quote) then advance()
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

  private def maybeConsumeString(str: String): Boolean = {
    skipTrivia()
    if startsWith(str) then
      // Scala.js doesn't support `String.codePoints()` / `java.util.stream.*`, so advance by characters here.
      var i = 0
      while i < str.length do
        advance()
        i += 1
      true
    else false
  }

  private def expectChar(ch: Char): Boolean =
    if maybeConsume(ch) then true else false

  private def skipUnknownToken(): Unit =
    if !eof then advance()

  private def skipBalanced(open: Char, close: Char): Boolean = {
    skipTrivia()
    if !peekChar(open) then return false
    advance()
    var depth = 1
    while depth > 0 && !eof do
      peekCodePoint match
        case Some(cp) if cp == open.toInt =>
          depth += 1
          advance()
        case Some(cp) if cp == close.toInt =>
          depth -= 1
          advance()
        case Some(cp) if cp == '"'.toInt || cp == '\''.toInt =>
          // Consume string literals to avoid being confused by delimiters inside them.
          readStringLiteral(skip = false)
        case _ =>
          advance()
    true
  }

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
      consumeKeyword("import", skip = true)
        .map { case (start, _) => parseImport(start) }
        .orElse(consumeKeyword("interface", skip = true).map { case (start, _) => parseInterface(start) })
        .orElse(consumeKeyword("type", skip = true).map { case (start, _) => parseTypeAlias(start) })
        .orElse(consumeKeyword("function", skip = true).map { case (start, _) => parseFunction(start, modifiers = Vector.empty) })
        .orElse(parseVariableWithoutDeclare())
        .orElse(consumeKeyword("declare", skip = true).map { case (start, _) => parseDeclaration(start) })
        .orElse(consumeKeyword("export", skip = true).map { case (start, _) => parseExport(start) })
    }
  }

  private def parseVariableWithoutDeclare(): Option[TypeScriptAST] = {
    val start = cursor
    skipTrivia()
    val kindOpt = {
      if consumeKeyword("const", skip = false).isDefined then Some(VarKind.Const)
      else if consumeKeyword("let", skip = false).isDefined then Some(VarKind.Let)
      else if consumeKeyword("var", skip = false).isDefined then Some(VarKind.Var)
      else None
    }
    kindOpt.map(kind => parseVariableDeclaration(start, kind))
  }

  private def parseImport(start: Cursor): TypeScriptAST = {
    skipTrivia()
    val specifiers = mutable.ArrayBuffer[ImportSpecifier]()
    if maybeConsume('{') then
      skipTrivia()
      while !peekChar('}') && !eof do
        val imported = readIdentifier(skip = false).map(_._1).getOrElse("Unknown")
        val local = if consumeKeyword("as", skip = true).isDefined then readIdentifier(skip = true).map(_._1) else None
        specifiers += ImportSpecifier.Named(imported, local, spanFrom(start, cursor))
        skipTrivia()
        if peekChar(',') then advance(); skipTrivia()
      expectChar('}')
    else if maybeConsume('*') then
      consumeKeyword("as", skip = true)
      val local = readIdentifier(skip = true).map(_._1).getOrElse("Unknown")
      specifiers += ImportSpecifier.Namespace(local, spanFrom(start, cursor))
    else {
      // default import
      val local = readIdentifier(skip = false).map(_._1).getOrElse("Unknown")
      specifiers += ImportSpecifier.Default(local, spanFrom(start, cursor))
    }

    consumeKeyword("from", skip = true)
    val source = readStringLiteral(skip = true).map(_._1).getOrElse("")
    maybeConsume(';')
    TypeScriptAST.ImportDeclaration(specifiers.toVector, source, spanFrom(start, cursor))
  }

  private def parseInterface(start: Cursor): TypeScriptAST = {
    val name = readIdentifier().map(_._1).getOrElse("Unknown")
    // Skip type parameters like <T, U extends X>
    skipBalanced('<', '>')
    skipTrivia()
    val typeParams = Vector.empty[TypeParameter]

    val extendsTypes = {
      if consumeKeyword("extends", skip = true).isDefined then
        val buf = mutable.ArrayBuffer[TypeScriptType]()
        buf += parseTypeReference()
        skipTrivia()
        while maybeConsume(',') do
          buf += parseTypeReference()
          skipTrivia()
        buf.toVector
      else Vector.empty[TypeScriptType]
    }
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
    // Parenthesized type or function type
    if maybeConsume('(') then
      val afterOpen = cursor
      val typeCandidate = parseTypeReference()
      skipTrivia()
      if maybeConsume(')') then
        skipTrivia()
        if maybeConsumeString("=>") then
          // Rewind and parse parameters as a function type.
          cursor = afterOpen
          val params = parseParameterListInsideParens()
          skipTrivia()
          expectChar(')')
          skipTrivia()
          maybeConsumeString("=>")
          val ret = parseTypeReference()
          val parsed = TypeScriptType.FunctionType(params, ret, spanFrom(startCursor, cursor))
          if cursor == startCursor && !eof then
            advance()
            TypeScriptType.PrimitiveType("any", spanFrom(startCursor, cursor))
          else parsed
        else {
          val parsed = TypeScriptType.ParenthesizedType(typeCandidate, spanFrom(startCursor, cursor))
          if cursor == startCursor && !eof then
            advance()
            TypeScriptType.PrimitiveType("any", spanFrom(startCursor, cursor))
          else parsed
        }
      else {
        val parsed = TypeScriptType.PrimitiveType("any", spanFrom(startCursor, cursor))
        if cursor == startCursor && !eof then
          advance()
          TypeScriptType.PrimitiveType("any", spanFrom(startCursor, cursor))
        else parsed
      }
    else if peekChar('{') then
      val members = parseMemberBlock()
      val parsed = TypeScriptType.ObjectType(members, spanFrom(startCursor, cursor))
      if cursor == startCursor && !eof then
        advance()
        TypeScriptType.PrimitiveType("any", spanFrom(startCursor, cursor))
      else parsed
    else {
      readStringLiteral() match
        case Some((lit, _, end)) =>
          val span = spanFrom(startCursor, end)
          val parsed = parseArraySuffix(TypeScriptType.LiteralType(TypeScriptAST.StringLiteral(lit, span), span), startCursor)
          if cursor == startCursor && !eof then
            advance()
            TypeScriptType.PrimitiveType("any", spanFrom(startCursor, cursor))
          else parsed
        case None =>
          val (name, nameEnd) = readQualifiedName().getOrElse(("any", cursor))
          val baseSpan = spanFrom(startCursor, nameEnd)
          val base0 = {
            if Set("string", "number", "boolean", "void", "any", "unknown", "never", "null", "undefined", "bigint", "symbol", "object")
                .contains(name)
            then TypeScriptType.PrimitiveType(name, baseSpan)
            else if name == "true" || name == "false" then
              TypeScriptType.LiteralType(TypeScriptAST.BooleanLiteral(name == "true", baseSpan), baseSpan)
            else TypeScriptType.TypeReference(name, Vector.empty, baseSpan)
          }
          val base = parseTypeArguments(base0, startCursor)
          val parsed = parseArraySuffix(base, startCursor)
          if cursor == startCursor && !eof then
            advance()
            TypeScriptType.PrimitiveType("any", spanFrom(startCursor, cursor))
          else parsed
    }
  }

  private def readQualifiedName(): Option[(String, Cursor)] = {
    readIdentifier() match
      case Some((first, _, end0)) =>
        val builder = new java.lang.StringBuilder(first)
        var end = end0
        skipTrivia()
        while maybeConsume('.') do
          readIdentifier(skip = true) match
            case Some((next, _, nextEnd)) =>
              builder.append('.')
              builder.append(next)
              end = nextEnd
              skipTrivia()
            case None => ()
        Some((builder.toString, end))
      case None => None
  }

  private def parseTypeArguments(base: TypeScriptType, start: Cursor): TypeScriptType = {
    skipTrivia()
    if !peekChar('<') then return base
    val saved = cursor
    if !maybeConsume('<') then return base
    val args = mutable.ArrayBuffer[TypeScriptType]()
    skipTrivia()
    while !peekChar('>') && !eof do
      args += parseTypeReference()
      skipTrivia()
      if peekChar(',') then advance(); skipTrivia()
    if peekChar('>') then advance()
    base match
      case TypeScriptType.TypeReference(name, _, span) =>
        TypeScriptType.TypeReference(name, args.toVector, spanFrom(start, cursor).orElse(span))
      case other =>
        cursor = saved
        other
  }

  private def parseArraySuffix(base: TypeScriptType, start: Cursor): TypeScriptType = {
    skipTrivia()
    var current = base
    var keep = true
    while keep && startsWith("[]") do
      advance()
      advance()
      current = TypeScriptType.ArrayType(current, spanFrom(start, cursor))
      skipTrivia()
      keep = startsWith("[]")
    current
  }

  private def parseTypeAlias(start: Cursor): TypeScriptAST = {
    val name = readIdentifier().getOrElse(("Unknown", cursor, cursor))._1
    // Skip type parameters like <T>
    skipBalanced('<', '>')
    expectChar('=')
    val aliasType = parseTypeReference()
    maybeConsume(';')
    TypeScriptAST.TypeAliasDeclaration(name, Vector.empty, aliasType, spanFrom(start, cursor))
  }

  private def parseDeclaration(start: Cursor): TypeScriptAST = {
    skipTrivia()
    if consumeKeyword("module", skip = false).isDefined || consumeKeyword("namespace", skip = false).isDefined then parseModule(start)
    else {
      consumeKeyword("function", skip = false)
        .map(_ => parseFunction(start, modifiers = Vector(Modifier.Declare)))
        .orElse(parseDeclareVariable(start).orElse(Some(TypeScriptAST.Empty(spanFrom(start, cursor)))))
        .get
    }
  }

  private def parseModule(start: Cursor): TypeScriptAST = {
    skipTrivia()
    val name = readStringLiteral(skip = true).map(_._1).orElse(readIdentifier(skip = true).map(_._1)).getOrElse("Unknown")
    val body = mutable.ArrayBuffer[TypeScriptAST]()
    if expectChar('{') then
      skipTrivia()
      while !peekChar('}') && !eof do
        parseStatement() match
          case Some(stmt) => body += stmt
          case None       => skipUnknownToken()
        skipTrivia()
      expectChar('}')
    TypeScriptAST.NamespaceDeclaration(name, body.toVector, spanFrom(start, cursor))
  }

  private def parseFunction(start: Cursor, modifiers: Vector[Modifier]): TypeScriptAST = {
    val name = readIdentifier().map(_._1).getOrElse("anonymous")
    // Skip type parameters on functions.
    skipBalanced('<', '>')
    val params = parseParameterList()
    val returnType = if maybeConsume(':') then Some(parseTypeReference()) else None
    val span = spanFrom(start, cursor)
    TypeScriptAST.FunctionDeclaration(
      name,
      params,
      returnType,
      TypeScriptAST.Block(Vector.empty, span),
      modifiers,
      span
    )
  }

  private def parseParameterListInsideParens(): Vector[Parameter] = {
    val params = mutable.ArrayBuffer[Parameter]()
    skipTrivia()
    while !peekChar(')') && !eof do
      val loopStart = cursor
      val paramStart = cursor
      val isRest = startsWith("...")
      if isRest then
        advance()
        advance()
        advance()
      val name = readIdentifier(skip = false).map(_._1).getOrElse("param" + params.length)
      val isOptional = maybeConsume('?')
      val paramType = if maybeConsume(':') then Some(parseTypeReference()) else None
      // Optional params are treated as having an explicit type; we don't model `undefined` here.
      val effectiveType = paramType.orElse(if isOptional then Some(TypeScriptType.PrimitiveType("any", spanFrom(paramStart, cursor))) else None)
      if maybeConsume('=') then skipToParamBoundary()
      params += Parameter(name, effectiveType, None, isRest, spanFrom(paramStart, cursor))
      skipTrivia()
      if peekChar(',') then
        advance()
        skipTrivia()
      if cursor == loopStart && !eof then
        skipToParamBoundary()
        skipTrivia()
        if peekChar(',') then advance(); skipTrivia()
    params.toVector
  }

  private def parseParameterList(): Vector[Parameter] = {
    if !expectChar('(') then return Vector.empty
    val params = mutable.ArrayBuffer[Parameter]()
    skipTrivia()
    while !peekChar(')') && !eof do
      val loopStart = cursor
      val paramStart = cursor
      val isRest = startsWith("...")
      if isRest then
        advance()
        advance()
        advance()
      val name = readIdentifier(skip = false).map(_._1).getOrElse("param" + params.length)
      // Optional param marker: `x?: T`
      maybeConsume('?')
      val paramType = if maybeConsume(':') then Some(parseTypeReference()) else None
      // Default value: `x: T = ...` (we don't model it, just skip so we don't get stuck)
      if maybeConsume('=') then skipToParamBoundary()
      params += Parameter(name, paramType, None, isRest, spanFrom(paramStart, cursor))
      skipTrivia()
      if peekChar(',') then
        advance()
        skipTrivia()
      // Safety: ensure progress even on unexpected syntax (e.g. destructuring params)
      if cursor == loopStart && !eof then
        skipToParamBoundary()
        skipTrivia()
        if peekChar(',') then advance(); skipTrivia()
    expectChar(')')
    params.toVector
  }

  private def skipToParamBoundary(): Unit = {
    var parenDepth = 0
    var braceDepth = 0
    var bracketDepth = 0

    def atBoundary: Boolean =
      parenDepth == 0 && braceDepth == 0 && bracketDepth == 0 && (peekChar(',') || peekChar(')'))

    while !eof && !atBoundary do
      peekCodePoint match
        case Some(cp) if cp == '('.toInt =>
          parenDepth += 1
          advance()
        case Some(cp) if cp == ')'.toInt =>
          if parenDepth > 0 then parenDepth -= 1
          advance()
        case Some(cp) if cp == '{'.toInt =>
          braceDepth += 1
          advance()
        case Some(cp) if cp == '}'.toInt =>
          if braceDepth > 0 then braceDepth -= 1
          advance()
        case Some(cp) if cp == '['.toInt =>
          bracketDepth += 1
          advance()
        case Some(cp) if cp == ']'.toInt =>
          if bracketDepth > 0 then bracketDepth -= 1
          advance()
        case Some(cp) if cp == '"'.toInt || cp == '\''.toInt =>
          readStringLiteral(skip = false)
        case _ =>
          advance()
  }

  private def parseVariableDeclaration(start: Cursor, kind: VarKind): TypeScriptAST = {
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

  private def parseDeclareVariable(start: Cursor): Option[TypeScriptAST] = {
    val kindOpt = {
      if consumeKeyword("const", skip = false).isDefined then Some(VarKind.Const)
      else if consumeKeyword("let", skip = false).isDefined then Some(VarKind.Let)
      else if consumeKeyword("var", skip = false).isDefined then Some(VarKind.Var)
      else None
    }
    kindOpt.map(kind => parseVariableDeclaration(start, kind))
  }

  private def parseExport(start: Cursor): TypeScriptAST = {
    skipTrivia()
    val isDefault = consumeKeyword("default", skip = false).isDefined
    val inner = {
      // export {...} or export * from "x"
      if maybeConsume('{') then
        val specs = mutable.ArrayBuffer[ExportSpecifier]()
        skipTrivia()
        while !peekChar('}') && !eof do
          val local = readIdentifier(skip = false).map(_._1).getOrElse("Unknown")
          val exported = if consumeKeyword("as", skip = true).isDefined then readIdentifier(skip = true).map(_._1) else None
          specs += ExportSpecifier.Named(local, exported, spanFrom(start, cursor))
          skipTrivia()
          if peekChar(',') then advance(); skipTrivia()
        expectChar('}')
        consumeKeyword("from", skip = true)
        val src = readStringLiteral(skip = true).map(_._1)
        maybeConsume(';')
        return TypeScriptAST.ExportDeclaration(None, specs.toVector, src, isDefault, spanFrom(start, cursor))
      else if maybeConsume('*') then
        consumeKeyword("from", skip = true)
        val src = readStringLiteral(skip = true).map(_._1)
        maybeConsume(';')
        return TypeScriptAST.ExportDeclaration(
          None,
          Vector(ExportSpecifier.All(None, spanFrom(start, cursor))),
          src,
          isDefault,
          spanFrom(start, cursor)
        )
      else parseStatement()
    }.orElse {
      skipUnknownToken()
      None
    }
    TypeScriptAST.ExportDeclaration(inner, Vector.empty, None, isDefault, spanFrom(start, cursor))
  }

object TypeScriptDeclParser:
  /** Parse a TypeScript declaration file */
  def parse(source: String, sourceRef: Source): TypeScriptAST =
    new TypeScriptDeclParser(source, sourceRef).parse()
