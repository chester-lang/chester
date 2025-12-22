# Chester Programming Language

Welcome to the Chester Programming Language documentation! Chester is a modern, statically-typed functional language that compiles to TypeScript and features an advanced effect system.

> [!WARNING]
> **Development Status**: Chester is under active development and not ready for production use. Many features are still being implemented and the language design may change.

## What is Chester?

Chester is a statically-typed language designed to bring advanced type system features and effect tracking to the JavaScript/TypeScript ecosystem. It combines ideas from functional programming languages like Haskell and dependently-typed languages with practical interoperability with the JavaScript ecosystem.

**Key Features:**

- **TypeScript Backend**: Compiles Chester code to readable, idiomatic TypeScript
- **Effect System**: Track and manage side effects with CPS transformation
- **Strong Type System**: Type inference, dependent types, and algebraic data types
- **JavaScript/TypeScript Interop**: Import and use npm packages with automatic type signature extraction
- **Multi-Platform**: Runs on JVM, JavaScript (via Scala.js), and Native (via Scala Native)
- **Developer Tools**: REPL, LSP server, IntelliJ plugin, and browser-based REPL

## Quick Example

```chester,playground,editable
module example;

def greet(name: String): String = 
  "Hello, " ++ name ++ "!";

def main: String = greet("World");
```

## Getting Started

To start using Chester:

1. **[Getting Started Guide](guide/getting-started.md)** - Build from source and run your first program
2. **[CLI Usage](guide/cli-usage.md)** - Learn the command-line interface
3. **[Language Guide](guide/README.md)** - Understand Chester's syntax and features

## Architecture

Chester is implemented in Scala 3 and consists of several modules:

- **CLI**: Command-line interface with REPL
- **Core**: Parser, elaborator, type checker, and backends
- **LSP**: Language Server Protocol implementation
- **Web REPL**: Browser-based REPL (Scala.js)
- **IntelliJ Plugin**: IDE integration

For detailed internals, see the [Development Documentation](dev/README.md).

## Current Backends

- **TypeScript** ✅ - Fully implemented and tested
- **Go** 🚧 - Type signatures implemented, codegen in progress

## Website & REPL

Try Chester in your browser at the [interactive REPL](https://github.com/chester-lang/chester) (see the `site/` directory for the Next.js-based website).