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

Bare `Unit` as a value (`resume(Unit)`) is still accepted for compatibility: the
elaborator represents it as `AstRef "Unit"` and backends treat it like void. Prefer
`resume(())` / `()` for the unit value in new and updated code.

## Surface syntax (current compiler)

- Effects are declared with `effect ... { def op(...): Ret }` and used with `handle`, `perform`,
  `resume`, `box`, `unbox`. Functions may also carry effect rows such as `/ [io]` (see
  `stdlib/*/std.chester`).
- TypeScript FFI: `extern ts "module" { def name(...): Ret; }` plus optional bindgen via
  `scripts/dts2chester.mjs` / `bin/chester_bindgen.exe`.
- Go FFI: bare package calls such as `fmt.Println(...)` in `stdlib/go/std.chester` (no
  `import go "fmt"` surface syntax).
- Algebraic data: `enum` / `case`, not draft `data` / `trait` syntax.
- Modules (SML-core, same-file): `module M { ... }`, `signature S { ... }`,
  opaque sealing `module M :> S { ... }`, transparent `module M : S { ... }`,
  generative functors `module F(X: S) { ... }` / `module N = F(M)`,
  applicative functors `module app F(X: S) { ... }`, paths `M.x`,
  type components `type t` / `type t = T` (incl. `M.t` in type position),
  effect rows on defs/signature members (`def f(): T / [e]`), checked under sealing,
  first-class `pack M as S` / `unpack (X : S) = e in body`,
  sharing `signature T = S with type t = Int`,
  and Chester file imports `import Math` / `import Math from "math.chester"`
  (resolved via `--module-path` / `CHESTER_PATH`).

## Repository conventions

- Verified core lives in `theories/*.v` and is extracted to `extraction/`.
- `self-hosted/*.chester` must elaborate; `test/test_compiler.ml` checks this.
- Rocq vs self-hosted Go runtime parity: `rocq vs self-hosted go runtime parity` in
  `test/test_compiler.ml` (builds stage1, compares outputs on core fixtures); flake
  `.#default` also checks Stage 2 against Rocq emit on the same set.
- New language fixtures go in `tests/*.chester` and should be wired into `dune runtest`.
- Build with Nix: `nix develop --command sh -c "coq_makefile -f _CoqProject -o Makefile && make && dune build && dune runtest"`.
- CLI flags (`--module-path`, `--prelude`, backends) are documented in `docs/guide/cli-usage.md`.

### Rocq recursion / fuel

- Do **not** hardcode magic fuel constants (e.g. `4096`, `1000`) for recursive AST/CST
  walks when a structural or measure-derived bound is available.
- Prefer `{struct t}` recursion on the term, or fuel derived from a size/depth measure of
  the input (same idea as `cst_size` / `cst_fuel` in `theories/CST.v`, or `ast_size` for
  `subst_ast` in `theories/CoreChecker.v`).
- If fuel is still required for well-foundedness (e.g. rename-then-subst), compute it from
  the input measure; do not invent a large fixed ceiling.
