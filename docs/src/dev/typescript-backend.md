# TypeScript Backend

The Chester TypeScript backend transforms type-checked Chester AST into readable TypeScript code.

> [!NOTE]
> **Status**: ✅ Fully Implemented and Working

## Overview

Located at [`modules/core/shared/src/main/scala/chester/backend/TypeScriptBackend.scala`](file:///home/dev/Documents/chester/modules/core/shared/src/main/scala/chester/backend/TypeScriptBackend.scala), the TypeScript backend provides a complete lowering from Chester's core AST to TypeScript.

## Features

| Feature | Status | TypeScript Output |
|---------|--------|-------------------|
| Functions (`def`) | ✅ | Function declarations |
| Lambdas | ✅ | Arrow functions |
| Records | ✅ | Interface declarations |
| Enums | ✅ | TypeScript enums |
| Coenums | ✅ | TypeScript enums |
| Let bindings | ✅ | IIFEs (Immediately Invoked Function Expressions) |
| Blocks | ✅ | IIFEs for expression contexts |
| Literals | ✅ | Number, string, boolean literals |
| Lists/Tuples | ✅ | Arrays |
| Field access | ✅ | Property access |
| Binary operators | ✅ | TypeScript operators |
| Type annotations | ✅ | TypeScript type annotations |
| JS/TS imports | ✅ | Import declarations |

## API

### Main Entry Point

```scala
def lowerProgram(ast: AST, config: Config = Config()): TypeScriptAST.Program
```

Converts a Chester AST into a complete TypeScript program.

### Configuration

```scala
case class Config(
  applyEffectCPS: Boolean = false,
  cpsConfig: EffectCPS.Config = EffectCPS.Config()
)
```

- **`applyEffectCPS`**: Enable CPS transformation for effect types
- **`cpsConfig`**: Configuration for effect CPS transformation

## How It Works

### Lowering Pipeline

```
Chester AST → TypeScriptBackend.lowerProgram → TypeScript AST → Pretty Print → .ts file
```

### Statement Lowering

Chester statements are transformed as follows:

**`def` Declarations:**
```chester
def greet(name: String): String = "Hello, " ++ name
```
↓
```typescript
function greet(name: string): string {
  return "Hello, " + name;
}
```

**Records:**
```chester
record Person(name: String, age: Int);
```
↓
```typescript
interface Person {
  name: string;
  age: number;
}
```

**Enums:**
```chester
enum Color { Red, Green, Blue };
```
↓
```typescript
enum Color {
  Red,
  Green,
  Blue
}
```

**JS/TS Imports:**
```chester
import "lodash";
```
↓
```typescript
import * as lodash from "lodash";
```

### Expression Lowering

**Lambdas:**
```chester
(x: Int) => x * 2
```
↓
```typescript
(x: number) => x * 2
```

**Let Bindings (as IIFEs):**
```chester
let x = 5; x + 10
```
↓
```typescript
(() => {
  const x = 5;
  return x + 10;
})()
```

**List Literals:**
```chester
[1, 2, 3]
```
↓
```typescript
[1, 2, 3]
```

### Type Lowering

| Chester Type | TypeScript Type |
|--------------|-----------------|
| `String` | `string` |
| `Int` / `Natural` | `number` |
| `Boolean` | `boolean` |
| `Any` | `any` |
| `Type` / `TypeOmega` | `any` |
| `List[T]` | `Array<T>` |
| `(T1, T2)` | `[T1, T2]` (tuple) |
| `(A) -> B` | `(A) => B` (function) |
| Record types | Type references |
| Enum types | Type references |

## Effect System Integration

The backend supports optional CPS transformation for Chester's effect system:

```scala
val config = Config(applyEffectCPS = true)
TypeScriptBackend.lowerProgram(ast, config)
```

When enabled, effect types are transformed before lowering (handled by `EffectCPS.transformType`).

## Usage in CLI

The TypeScript backend is integrated into the Chester CLI via the `ts` command:

```bash
chester ts input.chester --output output.ts
```

This command:
1. Parses and elaborates the Chester source
2. Type-checks the AST
3. Calls `TypeScriptBackend.lowerProgram`
4. Pretty-prints the TypeScript AST to a `.ts` file

See [`CLI.scala`](file:///home/dev/Documents/chester/modules/cli/shared/src/main/scala/chester/cli/CLI.scala) for implementation.

## Example: Complete Transformation

### Chester Input

```chester
module math;

record Point(x: Int, y: Int);

def distance(p1: Point, p2: Point): Int = {
  let dx = p1.x - p2.x;
  let dy = p1.y - p2.y;
  dx * dx + dy * dy
};

def main: Int = {
  let origin = Point(0, 0);
  let point = Point(3, 4);
  distance(origin, point)
};
```

### TypeScript Output

```typescript
interface Point {
  x: number;
  y: number;
}

function distance(p1: Point, p2: Point): number {
  return (() => {
    const dx = p1.x - p2.x;
    return (() => {
      const dy = p1.y - p2.y;
      return dx * dx + dy * dy;
    })();
  })();
}

function main(): number {
  return (() => {
    const origin = { _tag: "Point", _1: 0, _2: 0 };
    return (() => {
      const point = { _tag: "Point", _1: 3, _2: 4 };
      return distance(origin, point);
    })();
  })();
}
```

## Design Decisions

### Why IIFEs for Let Bindings?

Chester's `let` bindings are expressions that return values. TypeScript's `let` statements don't return values, so we wrap them in immediately-invoked arrow functions to preserve expression semantics.

### Best-Effort Lowering

The backend uses a "best-effort" approach: if a Chester construct has no obvious TypeScript equivalent, it falls back to `undefined` or an identifier rather than failing. This ensures partial code generation even for experimental features.

### Blocks as Statements vs. Expressions

- **Top-level blocks**: Lowered as statement sequences
- **Expression blocks**: Wrapped in IIFEs to preserve expression context

## Implementation Details

### Key Functions

- **`lowerProgram`**: Entry point, converts AST to TypeScript program
- **`lowerStmt`**: Converts Chester statements to TypeScript statements
- **`lowerExpr`**: Converts Chester expressions to TypeScript expressions
- **`lowerType`**: Maps Chester types to TypeScript types
- **`lowerParam`**: Converts function parameters

### AST Representation

The backend uses a TypeScript-specific AST defined in [`syntax/TypeScriptAST.scala`](file:///home/dev/Documents/chester/modules/core/shared/src/main/scala/chester/syntax/TypeScriptAST.scala), which includes:

- Expression nodes (literals, identifiers, calls, arrows, etc.)
- Statement nodes (declarations, blocks, returns, etc.)
- Type annotation nodes
- TypeScript-specific constructs (interfaces, enums, namespaces)

## Testing

The TypeScript backend is tested via:

```bash
sbt "core/testOnly chester.backend.TypeScriptBackendTest"
```

Tests cover:
- Basic type lowering
- Function declarations
- Expression transformations
- Edge cases

## Future Enhancements

Potential improvements:

- More idiomatic TypeScript output (avoid excessive IIFEs)
- Better pattern matching → switch/if-else lowering
- Optimization passes to simplify generated code
- Source map generation for debugging
- Module system improvements

## Related Documentation

- [CLI Usage](../guide/cli-usage.md) - How to use the `chester ts` command
- [Go Backend](go-backend.md) - Similar backend for Go (in progress)
- [Module Structure](module-structure.md) - Codebase organization