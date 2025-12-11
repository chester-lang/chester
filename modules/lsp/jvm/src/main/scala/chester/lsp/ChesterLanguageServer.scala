package chester.lsp

import java.net.URI
import java.nio.file.Paths
import java.util
import java.util.concurrent.CompletableFuture

import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.language.experimental.genericNumberLiterals
import scala.util.Try

import chester.core.AST
import chester.error.Span
import chester.tyck.SymbolIndex
import chester.uniqid.UniqidOf
import chester.utils.asInt
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}

class ChesterLanguageServer extends LanguageServer, LanguageClientAware:
  private var client: LanguageClient = uninitialized
  private val documents = new DocumentManager
  private val textService = ChesterTextDocumentService(documents)
  private val workspaceService = ChesterWorkspaceService()

  override def connect(client: LanguageClient): Unit =
    this.client = client

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    capabilities.setDefinitionProvider(java.lang.Boolean.TRUE)
    capabilities.setReferencesProvider(java.lang.Boolean.TRUE)
    CompletableFuture.completedFuture(InitializeResult(capabilities))

  override def shutdown(): CompletableFuture[AnyRef] =
    CompletableFuture.completedFuture(null)

  override def exit(): Unit = ()

  override def getTextDocumentService: TextDocumentService = textService

  override def getWorkspaceService: WorkspaceService = workspaceService

class ChesterTextDocumentService(documents: DocumentManager) extends TextDocumentService:
  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    val doc = params.getTextDocument
    documents.open(doc.getUri, doc.getText)

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    val uri = params.getTextDocument.getUri
    // Use the last change; assume full-text sync
    params.getContentChanges.asScala.lastOption.foreach(change => documents.update(uri, change.getText))

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    documents.close(params.getTextDocument.getUri)

  override def didSave(params: DidSaveTextDocumentParams): Unit = ()

  override def definition(
      params: DefinitionParams
  ): CompletableFuture[Either[java.util.List[? <: Location], java.util.List[? <: LocationLink]]] =
    CompletableFuture.completedFuture {
      val uri = params.getTextDocument.getUri
      val pos = params.getPosition
      resolveTarget(uri, pos) match
        case Some((_, span, _)) =>
          val loc = toLocation(span, uri)
          val locs: java.util.List[? <: Location] = util.List.of(loc)
          Either.forLeft(locs)
        case None =>
          Either.forLeft(util.List.of[Location]())
    }

  override def references(params: ReferenceParams): CompletableFuture[java.util.List[? <: Location]] =
    CompletableFuture.completedFuture {
      val uri = params.getTextDocument.getUri
      val pos = params.getPosition
      resolveTarget(uri, pos) match
        case Some((id, _, analysis)) =>
          val includeDecl = Option(params.getContext).exists(_.isIncludeDeclaration)
          val refSpans = analysis.index.usages(id)
          val defLoc = analysis.index.definition(id).filter(_ => includeDecl).map(entry => toLocation(entry.span, uri))
          val refLocs = refSpans.map(s => toLocation(s, uri))
          (defLoc.toVector ++ refLocs).asJava.asInstanceOf[java.util.List[? <: Location]]
        case None =>
          util.List.of[Location]().asInstanceOf[java.util.List[? <: Location]]
    }

  private def resolveTarget(uri: String, pos: Position): Option[(UniqidOf[AST], Span, DocumentAnalysis)] =
    documents.analyze(uri).toOption.flatMap { analysis =>
      val index = analysis.index
      val idFromPos = index.findSymbolAt(pos.getLine, pos.getCharacter)
      val idFromText =
        idFromPos.orElse {
          documents.text(uri).flatMap { text =>
            val lines = text.linesIterator.toVector
            if pos.getLine < lines.length then
              val line = lines(pos.getLine)
              val word = extractWord(line, pos.getCharacter)
              index.definitions.collectFirst { case (id, entry) if entry.name == word => id }
            else None
          }
        }

      idFromText.flatMap { id =>
        val defSpan = index.definition(id).map(_.span).orElse(index.usages(id).headOption)
        defSpan.map(span => (id, span, analysis))
      }
    }

  private def extractWord(line: String, col: Int): String =
    val start = line.take(col).reverse.takeWhile(_.isLetterOrDigit).reverse
    val end = line.drop(col).takeWhile(_.isLetterOrDigit)
    (start + end).trim

  private def toLocation(span: Span, fallbackUri: String): Location =
    val range = Range(
      Position(span.range.start.line.asInt, span.range.start.column.utf16.asInt),
      Position(span.range.end.line.asInt, span.range.end.column.utf16.asInt)
    )
    val uri = spanUri(span, fallbackUri)
    Location(uri, range)

  private def spanUri(span: Span, fallback: String): String =
    val name = span.fileName
    if name.startsWith("file:") then name
    else
      Try(Paths.get(name).toUri.toString).orElse(Try(URI.create(name).toString)).getOrElse(fallback)

class ChesterWorkspaceService extends WorkspaceService:
  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = ()
  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = ()

object Main:
  def main(args: Array[String]): Unit =
    val server = ChesterLanguageServer()
    val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
    server.connect(launcher.getRemoteProxy)
    launcher.startListening().get()
