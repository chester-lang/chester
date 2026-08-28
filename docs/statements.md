# Statements and scoping

Adapted from the Scala draft guide. Chester's verified compiler (`theories/Elaborator.v`)
handles `let` and `def` bindings in blocks.

## `let` bindings

- Visible **after** the binding in the current block.
- No forward references.
- Type annotations are optional when the type can be inferred.

```chester
let x = 5;
let y = x;
```

This fails because `x` is not in scope yet:

```chester
let y = x;
let x = 5;
```

## `def` bindings

- Visible throughout the block, including **before** the definition site.
- Forward references are allowed when parameter types are annotated.

```chester
def main(): Integer = {
  add(40, 2)
};

def add(x: Integer, y: Integer): Integer = {
  x + y
}
```

## Top-level and prelude files

Multiple files passed to `main.exe` share elaboration state — definitions from an
earlier file are visible in later files. Use `--prelude` to elaborate shared
libraries without emitting them (see [CLI Usage](cli-usage.md)).

## Unit values

- `Unit` is the unit **type** (`def f(): Unit`).
- `()` is the unit **value** (preferred in expression position).
- `resume(Unit)` in effect handlers is accepted for compatibility with existing
  tests; prefer `()` in new code (see `AGENTS.md`).

## See also

- [Getting Started](getting-started.md)
- [Universal Parsed Syntax](universal-syntax.md)
