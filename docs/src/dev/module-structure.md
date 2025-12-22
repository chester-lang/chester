# Chester Module Structure

This document explains the organization of the Chester codebase, which is structured as a multi-platform Scala 3 project.

## Overview

Chester uses sbt (Scala Build Tool) with cross-compilation for three platforms:
- **JVM** - Standard Java Virtual Machine
- **JavaScript** - Via Scala.js
- **Native** - Via Scala Native

## Top-Level Directory Structure

```
chester/
├── modules/              # All source code modules
├── vendor/               # Vendored dependencies (Kiama)
├── site/                 # Next.js website
├── docs/                 # Documentation (mdbook)
├── build.sbt             # Build configuration
└── project/              # sbt build definition
```

## Modules

### Core Modules

#### `modules/cli/`

Command-line interface for Chester.

**Platforms:** JVM, JavaScript, Native

**Key Components:**
- `CLI.scala` - Main CLI logic (REPL, file execution, compilation)
- `Config.scala` - Command-line argument parsing
- `Main.scala` - Entry point
- `Evaluator.scala` - AST evaluation

**Responsibilities:**
- Parse command-line arguments
- Run the REPL
- Execute and compile Chester files
- Coordinate TypeScript code generation
- Manage import resolution (JS/TypeScript packages)

#### `modules/core/`

Core compiler components.

**Platforms:** JVM, JavaScript, Native

**Key Components:**

**Syntax & Parsing:**
- `CST.scala` - Concrete Syntax Tree
- `AST.scala` - Abstract Syntax Tree  
- `Parser.scala` - Recursive descent parser
- `Tokenizer.scala` - Lexical analysis

**Type System:**
- `tyck/` - Type checking and elaboration
- `ElabContext.scala` - Elaboration context
- `CoreTypeChecker.scala` - Core type checker
- `GoImportSignature.scala` - Go package signatures
- `JSImportSignature.scala` - JavaScript/TypeScript signatures

**Backends:**
- `backend/TypeScriptBackend.scala` - TypeScript code generation
- `backend/GoBackend.scala` - Go code generation (in progress)

**Transforms:**
- `transform/EffectCPS.scala` - Effect CPS transformation

**Interop:**
- `interop/typescript/` - TypeScript `.d.ts` parsing
- `interop/golang/` - Go type extraction

**Responsibilities:**
- Parse Chester source to CST/AST
- Elaborate and type-check programs
- Lower to target backends
- Apply transformations (CPS, etc.)

#### `modules/utils/`

Shared utilities used across modules.

**Platforms:** JVM, JavaScript, Native

**Key Components:**
- `doc/` - Pretty-printing and document rendering
- `elab/` - Elaboration solver infrastructure
- `io/` - Cross-platform I/O abstractions
- `term/` - Terminal/REPL utilities

**Responsibilities:**
- Provide platform-agnostic I/O
- Document pretty-printing
- Constraint solver for elaboration

#### `modules/lsp/`

Language Server Protocol implementation.

**Platforms:** JVM

**Key Components:**
- LSP server handlers (not yet fully implemented)

**Responsibilities:**
- Provide IDE integration via LSP
- Support VS Code, IntelliJ, and other LSP clients

#### `modules/web-repl/`

Browser-based REPL.

**Platforms:** JavaScript (Scala.js)

**Key Components:**
- Web entry point that reuses CLI logic

**Build:**
```bash
sbt webRepl/copyWebRepl
```

This generates JavaScript bundles for the browser.

**Responsibilities:**
- Provide in-browser Chester REPL
- Integrate with the Next.js website

#### `modules/intellij-plugin/`

IntelliJ IDEA plugin.

**Platform:** JVM

**Responsibilities:**
- Syntax highlighting
- Basic IDE features for Chester

## Platform-Specific Code

Each cross-compiled module has platform-specific subdirectories:

```
modules/cli/
├── shared/       # Shared code (all platforms)
├── jvm/          # JVM-specific
├── js/           # JavaScript-specific (Scala.js)
└── native/       # Native-specific (Scala Native)
```

**Shared code** goes in `shared/src/main/scala/`.  
**Platform code** goes in `<platform>/src/main/scala/`.

## Vendored Dependencies

### `vendor/kiama/`

Chester vendors a modified version of the [Kiama](https://github.com/effekt-lang/kiama) library for:
- Tree manipulation
- Attribute grammars
- Rewriting

This is cross-compiled for JVM, JS, and Native.

## Build System (sbt)

### Key Build Definitions

**`build.sbt`:**
- Defines all modules and their dependencies
- Configures cross-platform settings
- Sets Scala version (3.7.4)

**Common Settings:**
- Semantic DB enabled (for Metals/LSP)
- UTF-8 encoding
- Explicit nulls (`-Yexplicit-nulls`)

### Useful sbt Commands

```bash
# Compile all modules
sbt compile

# Run JVM CLI
sbt "cliJVM/run"

# Build CLI assembly (fat JAR)
sbt cliJVM/assembly

# Build native binary
sbt cliNative/nativeLink

# Run tests
sbt test

# Build web REPL
sbt webRepl/fullOptJS

# Copy web REPL to website
sbt webRepl/copyWebRepl
```

## Data Flow

### Compilation Pipeline

```
Source File (.chester)
    ↓
CharReader → Tokenizer → Parser
    ↓
CST (Concrete Syntax Tree)
    ↓
Elaborator (with type inference)
    ↓
AST (Abstract Syntax Tree)
    ↓
Core Type Checker
    ↓
Transformations (Effect CPS, etc.)
    ↓
Backend (TypeScript, Go)
    ↓
Target Code (.ts, .go)
```

### Import Resolution (TypeScript)

```
import "package-name" in Chester source
    ↓
CLI detects import
    ↓
Resolve package in node_modules/
    ↓
Extract .d.ts files
    ↓
TypeScriptDeclParser parses definitions
    ↓
TypeScriptToChester converts to Chester signatures
    ↓
ElabContext includes import signatures
    ↓
Type-check against imported types
```

## Module Dependencies

```
cli → core, utils
core → utils
lsp → core, utils
web-repl → cli (reuses CLI for browser)
intellij-plugin → (standalone)
```

All modules depend on vendored Kiama.

## Testing

Tests are colocated with source code in `src/test/scala/`.

**Test Framework:** MUnit

**Running Tests:**

```bash
# All tests
sbt test

# Specific module
sbt core/test

# Specific test
sbt "core/testOnly chester.backend.TypeScriptBackendTest"
```

## Development Workflow

1. **Edit code** in `modules/<module>/shared/src/main/scala/`
2. **Compile** with `sbt compile`
3. **Run tests** with `sbt test`
4. **Test CLI** with `sbt "cliJVM/run <args>"`
5. **Build assembly** with `sbt cliJVM/assembly`

## Next Steps

- **[Development Guide](development.md)** - Contribution guidelines
- **[Type Checking System](type-checking-system.md)** - How type inference works
- **[Elaboration System](elaboration-system.md)** - The elaboration algorithm
- **[TypeScript Backend](typescript-backend.md)** - Code generation details
