# Chester Language Server Protocol Implementation

## Overview

The Language Server Protocol (LSP) implementation in Chester provides a robust foundation for IDE integrations and developer tooling. It enables features such as syntax highlighting, code completion, error reporting, and navigation capabilities that modern developers expect in their workflow.

## Architecture

Chester's LSP is implemented in Scala and based on the Eclipse LSP4J library. The implementation consists of two main components:

1. **ChesterLanguageServer**: The core server implementation that handles LSP requests and responses
2. **Main**: An entry point that sets up socket communication and launches the server

## Features

The Chester LSP supports the following features:

- **Document Synchronization**: Incremental text document synchronization
- **Code Completion**: Intelligent completion suggestions triggered by `.` and `:`
- **Hover Information**: Type information and documentation displayed on hover
- **Go to Definition**: Navigation to symbol definitions
- **Find References**: Locating all references to a symbol
- **Document Symbols**: Outline view of symbols in the current document
- **Workspace Symbols**: Project-wide symbol search
- **Code Actions**: Quick fixes and refactorings for errors and warnings
- **Diagnostics**: Real-time error reporting and type checking

## Implementation Details

### Server Initialization

The server initializes by setting up capabilities and establishing a connection with the client:

```scala
override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
  val capabilities = new ServerCapabilities()
  capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
  capabilities.setCompletionProvider(new CompletionOptions(false, List(".", ":").asJava))
  capabilities.setHoverProvider(true)
  capabilities.setDefinitionProvider(true)
  capabilities.setReferencesProvider(true)
  capabilities.setDocumentSymbolProvider(true)
  capabilities.setWorkspaceSymbolProvider(true)
  capabilities.setCodeActionProvider(true)
  CompletableFuture.completedFuture(new InitializeResult(capabilities))
}
```

### Document Management

The server maintains document state using a synchronized map:

```scala
private val documents = mutable.Map[String, DocumentInfo]()
```

Each document is processed to extract syntax, types, and symbols:

```scala
case class DocumentInfo(
  content: String,
  symbols: Vector[CollectedSymbol],
  tyckResult: TyckResult[Unit, Judge]
)
```

### Document Processing

When a document is opened or changed, it goes through the following process:

1. Parse the document using Chester's reader
2. Type check the parsed AST
3. Collect symbols and their information
4. Generate diagnostics for any errors
5. Update the document store
6. Publish diagnostics to the client

```scala
def processDocument(
    uri: String,
    text: String
): (TyckResult[Unit, Judge], Vector[CollectedSymbol], List[Diagnostic]) = {
  val parseResult = ChesterReader.parseTopLevel(FileNameAndContent(uri, text))
  val collecter = new VectorSemanticCollector()
  // Process parse result and run type checking
  // Return tyck result, collected symbols, and diagnostics
}
```

### Position Mapping

The server handles conversions between different position representations:

- **LSP Positions**: 0-based line and character offsets using UTF-16 code units
- **Chester SourcePos**: Source positions with file name, start, and end positions

```scala
def lspPositionToSourcePos(uri: String, position: Position): Option[SourcePos]
def sourcePosToLspRange(pos: SourcePos): Range
```

## Connection and Communication

The server uses socket-based communication:

```scala
def main(args: Array[String]): Unit = {
  val port = if (args.nonEmpty) args(0).toInt else 1044
  val serverSocket = new ServerSocket(port)
  val clientSocket = serverSocket.accept()
  
  val in: InputStream = clientSocket.getInputStream
  val out: OutputStream = clientSocket.getOutputStream
  
  val server = new ChesterLanguageServer()
  val launcher = LSPLauncher.createServerLauncher(server, in, out)
  
  val client = launcher.getRemoteProxy
  server.connect(client)
  
  launcher.startListening()
}
```

## Integration with Chester Compiler

The LSP server integrates with Chester's compiler components:

- **Reader**: Parses source code into AST
- **Tycker**: Performs type checking and elaboration
- **Error Reporting**: Translates compiler errors to LSP diagnostics

This integration ensures that the language features exposed through the LSP are consistent with the compiler's behavior.

## Using the LSP

To connect to the LSP server:

1. Start the LSP server using `sbt lsp/run` (default port: 1044)
2. Configure your IDE to connect to the LSP server
3. Edit Chester files with full language support

## Future Improvements

Potential areas for enhancement in the Chester LSP implementation:

- Support for additional LSP features such as code formatting and rename refactoring
- Performance optimizations for large codebases
- Enhanced semantic token support for better syntax highlighting
- Workspace-wide analysis and refactoring capabilities
