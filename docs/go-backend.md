# Go backend

Chester's verified Go emitter lives in `theories/Backend.v` and `theories/GoAST.v`,
extracted to OCaml and invoked from `bin/main.exe --go`.

## Current behavior

- **Entry point:** user `def main()` is renamed to `chester_main`; the driver emits a
  small `func main()` that prints the result.
- **Primitives:** `prim__int_add`, `prim__list_length`, etc. map to Go helpers in the
  effects preamble.
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
