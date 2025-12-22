# Chester Compiler Backend Architecture

## Overview

This document outlines the backend architecture of the Chester compiler system. The backend is responsible for transforming Chester's internal representation into executable code for various target platforms.

## Supported Compiler Targets

Chester currently supports two compiler backends with different maturity levels:

### TypeScript Backend ✅ FULLY IMPLEMENTED

**Status**: Production-ready, actively used

The TypeScript backend ([`TypeScriptBackend.scala`](file:///home/dev/Documents/chester/modules/core/shared/src/main/scala/chester/backend/TypeScriptBackend.scala)) transforms Chester code into readable TypeScript.

**Features**:
- Complete AST lowering from Chester to TypeScript
- Type annotation generation
- Function declarations and arrow functions
- Record → interface transformation
- Enum support
- ES module import/export
- Effect CPS transformation (optional)

**Usage**:
```bash
chester ts input.chester --output output.ts
```

**For detailed documentation**, see [TypeScript Backend Implementation](typescript-backend.md).

### Go Backend 🚧 IN PROGRESS

**Status**: Type signatures implemented, code generation in progress

The Go backend ([`GoBackend.scala`](file:///home/dev/Documents/chester/modules/core/shared/src/main/scala/chester/backend/GoBackend.scala)) aims to compile Chester to Go.

**Current State**:
- ✅ Go import signatures (`GoImportSignature.scala`)
- ✅ Package path normalization
- ✅ Go AST structure defined
- 🚧 Code generation partially implemented
- ⏳ CLI integration pending

**For detailed documentation**, see [Go Backend](go-backend.md).

## Backend Pipeline

The Chester compiler backend follows a multi-phase code generation pipeline:

```
Core AST (type-checked)
    ↓
Backend Lowering (target-specific)
    ↓
Target AST (TypeScript AST / Go AST)
    ↓
Pretty Printing
    ↓
Source Code (.ts / .go)
```

## Multi-Platform Execution

Chester itself is implemented in Scala 3 and runs on multiple platforms:

- **JVM**: Standard Java Virtual Machine (primary development platform)
- **JavaScript**: Via Scala.js (for browser-based REPL)
- **Native**: Via Scala Native (for fast CLI startup)

This is separate from the *compilation targets* (TypeScript, Go) which are what Chester programs compile *to*.

##

## Type System Mapping

Chester's rich type system needs careful mapping to target language types:

| Chester Type        | JavaScript/TypeScript | JVM                | Native (Planned)    |
|---------------------|----------------------|--------------------|--------------------|
| Integer             | number               | scala.BigInt       | int64_t            |
| Natural             | number               | scala.BigInt       | uint64_t           |
| Boolean             | boolean              | scala.Boolean      | bool               |
| String              | string               | java.lang.String   | std::string        |
| Union Types (A|B)   | A \| B               | Specialized classes| Tagged unions      |
| Record              | interface/class      | case class         | struct             |
| Functions           | function             | Function objects   | Function pointers  |

## Effects Handling

Chester's effect system is implemented differently for each target language:

- **JavaScript/TypeScript**: Using promises or custom effect handlers
- **JVM**: Using exceptions and monadic structures
- **Native**: Using error codes or custom effect handling

## Implementation Example: JavaScript Backend

### JavaScript/TypeScript AST Example

The JavaScript target provides a good example of target-specific AST:

```scala
// Example: Function declaration in JavaScript AST
FunctionDeclaration(
  id = Some(Identifier("greet")),
  params = List(Parameter(TypedIdentifier("name", StringTypeAnnotation()))),
  returnType = Some(StringTypeAnnotation()),
  body = BlockStatement(List(
    ReturnStatement(Some(
      BinaryExpression(
        BinaryOperator.Plus,
        StringLiteral("Hello, "),
        Identifier("name")
      )
    ))
  ))
)
```

This represents the TypeScript function:

```typescript
function greet(name: string): string {
  return "Hello, " + name;
}
```

### JavaScript AST Node Categories

The JavaScript AST supports a wide range of node types:

#### Expressions

- **Literals**: Numbers, strings, booleans, null, BigInt, RegExp
- **Identifiers**: Named references (typed and untyped)
- **Operators**: Binary, logical, assignment, unary, update
- **Function Expressions**: Regular functions and arrow functions
- **Object and Array Expressions**: Object literals and array literals
- **Class Expressions**: Class definitions with inheritance and method definitions

#### Statements

- **Block Statements**: Groups of statements
- **Expression Statements**: Expressions used as statements
- **Control Flow Statements**: if/else, while, do-while, for, switch
- **Declaration Statements**: let, const, var declarations

#### TypeScript Features

- **Type Annotations**: For variables, parameters, return types
- **Interface and Type Declarations**: For defining complex types
- **Generics**: Type parameters for functions and classes
- **Union and Intersection Types**: Type combinations

## Build System Integration

The Chester compiler backend integrates with build systems through:

- **SBT Plugin**: For JVM builds
- **NPM Package**: For JavaScript/TypeScript integration
- **CLI Interface**: For command-line usage

## Future Directions

Planned improvements to the compiler backend include:

- **WebAssembly Support**: Direct compilation to WebAssembly
- **More Native Targets**: Support for various native platforms
- **Interoperability Enhancements**: Better interop with target languages
- **Performance Optimizations**: Target-specific optimizations
- **Cross-Compilation**: Single-command compilation to multiple targets
- **Advanced Optimizations**: Target-specific performance improvements

## References

- JavaScript AST is inspired by the [ESTree Spec](https://github.com/estree/estree)
- JVM codegen draws from [Scala 3 compiler techniques](https://github.com/lampepfl/dotty)
- LLVM-based compilation follows the [LLVM Language Reference](https://llvm.org/docs/LangRef.html) 