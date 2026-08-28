# Agent Rules

Guidance for humans and agents working in this repository. Rules below are checked against
the current Coq/Rocq → OCaml compiler (`theories/`, `extraction/`, `bin/`) and Chester
sources (`stdlib/`, `tests/`, `self-hosted/`, `examples/`).

## Language semantics

- `Unit` is the unit **type**. Use it in type positions: `def put(s: T): Unit`, `def main(): Unit`.
- `()` is the unit **value**. Use it as an expression, e.g. the tail of `print_str` in `stdlib/*/std.chester`.
- Types are first-class. `Unit` may appear in contexts that look value-like when it is being used as a type-level value.
- In Chester code, if anything uses `()` in a type position, that code is wrong and must be corrected.

### `Unit` as an expression (legacy, still accepted)

Effect handlers and several tests currently use bare `Unit` as a value (`resume(Unit)`). The
elaborator represents this as `AstRef "Unit"` and backends treat it like void. This is
accepted today but inconsistent with the `()`-as-value rule above. Prefer `()` for new
code; do not introduce new `Unit`-as-value sites unless matching surrounding style.

## Surface syntax (current compiler)

- Effects are declared with `effect ... { def op(...): Ret }` and used with `handle`, `perform`,
  `resume`, `box`, `unbox`. Functions may also carry effect rows such as `/ [io]` (see
  `stdlib/*/std.chester`).
- TypeScript FFI: `extern ts "module" { def name(...): Ret; }` plus optional bindgen via
  `scripts/dts2chester.mjs` / `bin/chester_bindgen.exe`.
- Go FFI: bare package calls such as `fmt.Println(...)` in `stdlib/go/std.chester` (no
  `import go "fmt"` surface syntax).
- Algebraic data: `enum` / `case`, not draft `data` / `trait` syntax.

## Repository conventions

- Verified core lives in `theories/*.v` and is extracted to `extraction/`.
- `self-hosted/*.chester` must elaborate; `test/test_compiler.ml` checks this.
- New language fixtures go in `tests/*.chester` and should be wired into `dune runtest`.
- Build with Nix: `nix develop --command sh -c "coq_makefile -f _CoqProject -o Makefile && make && dune build && dune runtest"`.
