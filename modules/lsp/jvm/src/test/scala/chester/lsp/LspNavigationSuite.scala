package chester.lsp

import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*
import scala.language.experimental.genericNumberLiterals
import scala.language.unsafeNulls

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.TextDocumentService

class LspNavigationSuite extends munit.FunSuite:

  private val uri = "file:///test.chester"

  private def openDoc(service: TextDocumentService, text: String): Unit =
    val item = new TextDocumentItem(uri, "chester", 1, text)
    service.didOpen(new DidOpenTextDocumentParams(item))

  private def definitionAt(service: TextDocumentService, line: Int, column: Int): Vector[Location] =
    val params = new DefinitionParams()
    params.setTextDocument(new TextDocumentIdentifier(uri))
    params.setPosition(new Position(line, column))
    val result: Either[java.util.List[? <: Location], java.util.List[? <: LocationLink]] =
      service.definition(params).get(3, TimeUnit.SECONDS)
    val leftLocs = Option(result.getLeft).map(_.nn.asScala.toVector.map(_.asInstanceOf[Location])).getOrElse(Vector.empty)
    if leftLocs.nonEmpty then leftLocs
    else
      Option(result.getRight)
        .map(_.nn.asScala.toVector.map(link => Location(link.getTargetUri, link.getTargetRange)))
        .getOrElse(Vector.empty)

  private def referencesAt(service: TextDocumentService, line: Int, column: Int, includeDecl: Boolean): Vector[Location] =
    val params = new ReferenceParams()
    params.setTextDocument(new TextDocumentIdentifier(uri))
    params.setPosition(new Position(line, column))
    params.setContext(new ReferenceContext(includeDecl))
    service.references(params).get(3, TimeUnit.SECONDS).nn.asScala.toVector.map(_.asInstanceOf[Location])

  test("jump to definition and usages for def and record") {
    val code =
      """record Box(value: Integer);
        |def id(x: Integer) = x;
        |id(1);
        |Box(2)""".stripMargin
    val lines = code.linesIterator.toVector

    val server = ChesterLanguageServer()
    server.initialize(new InitializeParams()).get(3, TimeUnit.SECONDS)
    val textService = server.getTextDocumentService

    openDoc(textService, code)

    // def id: usage at line 2, column of "id"
    val idUsageCol = lines(2).indexOf("id")
    val idDefs = definitionAt(textService, 2, idUsageCol)
    assert(idDefs.nonEmpty, clue = "Expected definition for id")
    assertEquals(idDefs.head.getRange.getStart.getLine, 1)

    val idRefs = referencesAt(textService, 2, idUsageCol, includeDecl = true)
    assert(idRefs.exists(_.getRange.getStart.getLine == 2), clue = s"Refs: $idRefs")
    assert(idRefs.exists(_.getRange.getStart.getLine == 1), clue = s"Refs: $idRefs")

    // record Box: usage at line 3
    val boxUsageCol = lines(3).indexOf("Box")
    val boxDefs = definitionAt(textService, 3, boxUsageCol)
    assert(boxDefs.nonEmpty, clue = "Expected definition for Box")
    assertEquals(boxDefs.head.getRange.getStart.getLine, 0)

    val boxRefs = referencesAt(textService, 3, boxUsageCol, includeDecl = true)
    assert(boxRefs.exists(_.getRange.getStart.getLine == 3), clue = s"Refs: $boxRefs")
    assert(boxRefs.exists(_.getRange.getStart.getLine == 0), clue = s"Refs: $boxRefs")
  }
