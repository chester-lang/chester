# Go Backend

Chester includes a Go code generation backend, currently in development.

> [!NOTE]
> **Status**: The Go backend type signature system is implemented, but code generation and CLI integration are still in progress.

## Overview

The Go backend aims to allow Chester code to compile to Go, similar to how the TypeScript backend works. This enables Chester to target Go's ecosystem while maintaining type safety.

## Current Implementation

### Type Signatures (`GoImportSignature`)

Located in `modules/core/shared/src/main/scala/chester/tyck/GoImportSignature.scala`.

**Purpose**: Represent Go package exports for type checking Chester imports.

```scala
final case class GoImportSignature(
    fields: Vector[Param],
    packageName: String
)
```

**Features:**
- Package path normalization
- Type name generation for imports
- Parameter freshening for safe reuse

**Example:**

For a Go package `go:fmt` with exports like `Println`, `Printf`, etc., Chester generates a record type signature:

```chester
// When you write:
import "go:fmt";

// Chester treats it as:
type GoImport_fmt = {
  Println: (a: Any) -> IO Unit,
  Printf: (format: String, args: Array[Any]) -> IO Unit,
  ...
};
```

### Code Generation (`GoBackend`)

Located in `modules/core/shared/src/main/scala/chester/backend/GoBackend.scala`.

**Current Status**: Structure defined, code generation in progress.

**Planned Features:**
- AST → Go AST lowering
- Function and type declarations
- Module/package generation
- Import management

## Architecture

### Import Resolution Flow

```
Chester source with `import "go:..."`
    ↓
Extract Go import specifiers
    ↓
Call go-type-extractor tool
    ↓
Parse Go package types
    ↓
Generate GoImportSignature
    ↓
Add to ElabContext
    ↓
Type-check Chester code against Go types
```

### Code Generation Flow (Planned)

```
Type-checked Chester AST
    ↓
GoBackend.lowerProgram
    ↓
Go AST representation
    ↓
Pretty-print to .go file
```

## Comparison with TypeScript Backend

| Feature | TypeScript Backend | Go Backend |
|---------|-------------------|------------|
| Import signatures | ✅ Fully implemented | ✅ Implemented |
| CLI integration | ✅ `chester ts` command | ⏳ Not yet |
| Code generation | ✅ Complete | ⏳ In progress |
| npm integration | ✅ Auto-resolve packages | N/A |
| Testing | ✅ Comprehensive tests | ⏳ Basic tests |

## Go Type Extraction Tool

Chester includes a Go type extraction utility in `tools/go-type-extractor/` (planned).

**Purpose**: Analyze Go packages and extract type information.

**Requirements:**
- Go toolchain installed
- Access to Go package sources

**Usage (planned):**

```bash
# Extract type signatures from a Go package
go-type-extractor go:fmt > fmt-signature.json

# Use in Chester
import "go:fmt";
fmt.Println("Hello from Chester!");
```

## Example Usage (Future)

Once complete, the Go backend will work like this:

```chester
module hello;

import "go:fmt";
import "go:time";

def greet(name: String): IO Unit = {
  let now = time.Now();
  fmt.Printf("Hello, %s! Time: %v\n", [name, now])
};

def main: IO Unit = greet("Gopher");
```

Compile to Go:

```bash
chester go hello.chester --output hello.go
```

Generated `hello.go`:

```go
package hello

import (
    "fmt"
    "time"
)

func greet(name string) {
    now := time.Now()
    fmt.Printf("Hello, %s! Time: %v\n", name, now)
}

func main() {
    greet("Gopher")
}
```

## Implementation Roadmap

### Phase 1: Type Signatures ✅

- [x] `GoImportSignature` data structure
- [x] Package path normalization
- [x] Type name generation
- [x] Integration with elaboration context

### Phase 2: Type Extraction ⏳

- [ ] Build `go-type-extractor` tool
- [ ] Parse Go type declarations
- [ ] Handle Go generics (latest Go versions)
- [ ] Extract function signatures
- [ ] Extract struct definitions

### Phase 3: Code Generation ⏳

- [ ] Lower Chester AST to Go
- [ ] Function declarations
- [ ] Variable bindings
- [ ] Control flow (if, loops)
- [ ] Type definitions
- [ ] Import management

### Phase 4: CLI Integration ⏳

- [ ] Add `chester go` command
- [ ] Auto-resolve Go packages
- [ ] Directory compilation
- [ ] Integration tests

### Phase 5: Advanced Features 📋

- [ ] Go generics mapping
- [ ] Interface implementation
- [ ] Goroutines and channels (effects)
- [ ] CGo interop
- [ ] Build tool integration

## Technical Challenges

### Type System Differences

**Go:**
- Structural typing for interfaces
- No sum types (use interfaces + type assertions)
- Simpler generics (type parameters)

**Chester:**
- Dependent types
- Sum types (algebraic data types)
- Advanced type inference

**Strategy**: Lower Chester's richer types to Go patterns (interface{}, type assertions, etc.).

### Effect System

Go doesn't have an effect system. Chester's `IO` effects need to map to:
- Functions with side effects
- Error handling (return multiple values)
- Goroutines for concurrency

### Memory Management

- Go: Garbage collected
- Chester: Abstract (platform-dependent)

**Strategy**: Rely on Go's GC, avoid manual memory management in generated code.

## Testing

Run Go backend tests:

```bash
sbt "core/testOnly chester.backend.GoBackendTest"
```

Current tests cover:
- Import signature generation
- Package path normalization
- Type name generation

## Contributing

To contribute to the Go backend:

1. **Type Extraction**: Implement `go-type-extractor` in Go
2. **Code Generation**: Extend `GoBackend.scala` with lowering logic
3. **Testing**: Add test cases for various Chester constructs
4. **Documentation**: Keep this doc updated with progress

See the [TypeScript Backend](typescript-backend.md) for a reference implementation.

## Resources

- [Go Language Specification](https://go.dev/ref/spec)
- [Go AST Package](https://pkg.go.dev/go/ast)
- [TypeScript Backend (Chester)](typescript-backend.md)
- [Go Import Signature Source](file:///home/dev/Documents/chester/modules/core/shared/src/main/scala/chester/tyck/GoImportSignature.scala)
