package chester.syntax

import scala.language.experimental.genericNumberLiterals
import chester.error.{Span, Pos, SpanInFile}
import chester.reader.Source
import chester.utils.WithUTF16
import scala.collection.mutable

/** Simple parser for TypeScript declaration files (.d.ts)
  * 
  * This parser provides basic support for parsing TypeScript declarations
  * to enable binding generation for Chester code interacting with TypeScript libraries.
  */
class TypeScriptDeclParser(source: String, sourceRef: Source):
  private var pos = 0
  private var line = 1
  private var column = 1

  private def currentChar: Option[Char] =
    if pos < source.length then Some(source.charAt(pos)) else None

  private def advance(): Unit =
    if pos < source.length then
      if source.charAt(pos) == '\n' then
        line += 1
        column = 1
      else
        column += 1
      pos += 1

  private def skipWhitespace(): Unit =
    while currentChar.exists(_.isWhitespace) do advance()

  private def makeSpan(start: Int, end: Int): Option[Span] =
    Some(Span(sourceRef, SpanInFile(Pos.zero, Pos.zero)))

  private def parseIdentifier(): Option[String] =
    skipWhitespace()
    val start = pos
    if currentChar.exists(c => c.isLetter || c == '_' || c == '$') then
      advance()
      while currentChar.exists(c => c.isLetterOrDigit || c == '_' || c == '$') do
        advance()
      Some(source.substring(start, pos))
    else
      None

  private def expect(char: Char): Boolean =
    skipWhitespace()
    if currentChar.contains(char) then
      advance()
      true
    else
      false

  private def peekWord(): Option[String] =
    val saved = pos
    val savedLine = line
    val savedColumn = column
    val result = parseIdentifier()
    pos = saved
    line = savedLine
    column = savedColumn
    result

  /** Parse a TypeScript declaration file into a TypeScriptAST.Program */
  def parse(): TypeScriptAST =
    val statements = mutable.ArrayBuffer[TypeScriptAST]()
    
    skipWhitespace()
    while pos < source.length do
      peekWord() match
        case Some("interface") =>
          statements += parseInterface()
        case Some("type") =>
          statements += parseTypeAlias()
        case Some("declare") =>
          advance() // skip "declare"
          parseIdentifier() // function/var/const/let/class
          statements += parseDeclaration()
        case Some("export") =>
          statements += parseExport()
        case _ =>
          parseIdentifier() // skip unknown tokens
      skipWhitespace()
    
    val span = makeSpan(0, source.length)
    TypeScriptAST.Program(statements.toVector, span)

  private def parseInterface(): TypeScriptAST =
    val start = pos
    parseIdentifier() // skip "interface"
    val name = parseIdentifier().getOrElse("Unknown")
    val typeParams = Vector.empty[TypeParameter] // simplified
    val extendsTypes = Vector.empty[TypeScriptType] // simplified
    val members = parseMemberBlock()
    val span = makeSpan(start, pos)
    TypeScriptAST.InterfaceDeclaration(name, typeParams, extendsTypes, members, span)

  private def parseMemberBlock(): Vector[InterfaceMember] =
    val members = mutable.ArrayBuffer[InterfaceMember]()
    expect('{')
    skipWhitespace()
    while !currentChar.contains('}') && pos < source.length do
      parseIdentifier() match
        case Some(name) =>
          expect(':')
          val memberType = parseTypeReference()
          val span = makeSpan(0, pos)
          members += InterfaceMember(name, InterfaceMemberType.PropertySignature(memberType, false, false), span)
          expect(';')
          skipWhitespace()
        case None =>
          advance()
    expect('}')
    members.toVector

  private def parseTypeReference(): TypeScriptType =
    val start = pos
    val name = parseIdentifier().getOrElse("any")
    val span = makeSpan(start, pos)
    if Set("string", "number", "boolean", "void", "any", "unknown", "never").contains(name) then
      TypeScriptType.PrimitiveType(name, span)
    else
      TypeScriptType.TypeReference(name, Vector.empty, span)

  private def parseTypeAlias(): TypeScriptAST =
    val start = pos
    parseIdentifier() // skip "type"
    val name = parseIdentifier().getOrElse("Unknown")
    expect('=')
    val aliasType = parseTypeReference()
    expect(';')
    val span = makeSpan(start, pos)
    TypeScriptAST.TypeAliasDeclaration(name, Vector.empty, aliasType, span)

  private def parseDeclaration(): TypeScriptAST =
    // Simplified: just create a placeholder
    val span = makeSpan(pos, pos)
    TypeScriptAST.Empty(span)

  private def parseExport(): TypeScriptAST =
    val start = pos
    parseIdentifier() // skip "export"
    val span = makeSpan(start, pos)
    TypeScriptAST.ExportDeclaration(None, Vector.empty, None, false, span)

object TypeScriptDeclParser:
  /** Parse a TypeScript declaration file */
  def parse(source: String, sourceRef: Source): TypeScriptAST =
    new TypeScriptDeclParser(source, sourceRef).parse()
