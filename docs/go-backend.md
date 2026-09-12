# Go backend

Chester's verified Go emitter lives in `theories/Backend.v` and `theories/GoAST.v`,
extracted to OCaml and invoked from `bin/main.exe --go`. Self-hosted emit lives in
`self-hosted/codegen_go.chester`.

## Typing

The emitter maps Chester types to concrete Go types where it can:

| Chester | Go |
|---------|-----|
| `Integer` / `Int` | `int` |
| `String` | `string` |
| `Bool` | `bool` |
| records / enums / typarams / `List(_)` / `Any` / `Unit` | `interface{}` |

Record **declarations** still emit `type Point struct{ x int; y int }` from field
types; using `Point` as a Go nominal type in signatures is deferred until a
record env is threaded through emit.

Applied today (Rocq / `main.exe --go` and self-hosted `codegen_go.chester`):

- **Record fields** — `type Point struct{ x int; y int }`
- **`def` params / returns** — `func add(a int, b int) int`
- **`let` bindings** — inferred from literals, scalar ops, known def returns, and
  prior locals
- **`var` bindings** — stay `interface{}` (mutable / effect-friendly)
- **Lambdas / handlers** — stay `interface{}` (effects runtime)
- **Lists, enums, effects maps** — still dynamic (`list_length` returns `int`)
- **Call / return boundaries** — top-level def and extension-method signatures are
  collected; args and returns are coerced with `__chester_as_*` when the value may
  still be `interface{}` (so `let a = id(1); id(a)`, `var x = 1; x`, and
  `ListOps_get(list, i)` all compile). Known-typed locals skip redundant coerces.
- **Bool conditions** — skip `__chester_as_bool` when the cond is already `bool`

Scalar primitives in the Go preamble take/return concrete types
(`prim__int_add(a, b int) int`) and expose surface aliases (`var int_add = …`).
The emitter skips re-declaring those alias names when compiling stdlib/prelude.
Rocq/self-hosted call sites coerce with `__chester_as_*` so both concrete and
`interface{}` arguments work. With `--prelude`, Go also emits non-alias prelude
defs (e.g. `forty`) into the package.

## Current behavior

- **Entry point:** user `def main()` is renamed to `chester_main`; the driver emits a
  small `func main()` that prints the result.
- **Primitives:** `prim__int_add`, `prim__list_length`, etc. map to Go helpers in the
  effects preamble (`bin/effects_runtime.ml`).
- **FFI:** bare selectors such as `fmt.Println(...)` emit as direct Go calls (no
  `import go "fmt"` surface syntax). See `stdlib/go/std.chester` and `examples/go/`.
- **Effects:** `handle` / `perform` lower via the shared effects runtime (see
  `tests/effects*.chester` and Go runtime tests in `test/test_compiler.ml`).

## Reference data

`data/go-signatures.json` (ported from `chester2026draft`) lists pre-baked `fmt` and
other package signatures. `main.exe --go` loads it automatically when present;
use `--list-go-sigs` to inspect. Type-checking against signatures in the elaborator
is not wired yet.

## Examples

```bash
nix develop --command ./_build/default/bin/main.exe --go \
  -o /tmp/hello.go examples/go/hello.chester

go run /tmp/hello.go
```

## See also

- [CLI Usage](cli-usage.md)
- [Getting Started](getting-started.md)
- Draft design notes: `chester2026draft/docs/src/dev/go-backend.md` (Scala-era; historical)
