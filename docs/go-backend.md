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
| named records (`Point`, …) | same Go struct name |
| `List(_)`, `Any`, `Unit`, unknowns | `interface{}` |

Applied today (Rocq / `main.exe --go`):

- **Record fields** — `type Point struct{ x int; y int }`
- **`def` params / returns** — `func add(a int, b int) int`
- **`let` bindings** — inferred from literals and scalar ops (`var x int = 40`)
- **`var` bindings** — stay `interface{}` (mutable / effect-friendly)
- **Lambdas / handlers** — stay `interface{}` (effects runtime)
- **Lists, enums, effects maps** — still dynamic

Self-hosted `codegen_go.chester` matches record-field and `let` typing; `def`
params/returns stay `interface{}` until its call-site coercion catches up with
Rocq.

Scalar primitives in the Go preamble take/return concrete types
(`prim__int_add(a, b int) int`). Rocq call sites coerce with `__chester_as_int` /
`__chester_as_string` / `__chester_as_bool` so both concrete and `interface{}`
arguments work.

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
