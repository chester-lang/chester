# Getting Started with Chester

Chester is a verified programming language: semantics live in Coq/Rocq theories
(`theories/`), the compiler is extracted to OCaml, and backends emit TypeScript, Go,
and Rocq.

## Prerequisites

- [Nix](https://nixos.org/download/) (recommended — matches CI)
- Optional: [Node.js](https://nodejs.org/) for TypeScript/React examples
- Optional: [Go](https://go.dev/) for Go runtime tests

## Build from source

```bash
git clone https://github.com/chester-lang/chester.git
cd chester
nix develop --command sh -c \
  "coq_makefile -f _CoqProject -o Makefile && make && dune build"
```

Run the full test suite (Coq + OCaml + examples):

```bash
nix develop --command sh -c \
  "coq_makefile -f _CoqProject -o Makefile && make && dune build && dune runtest"
```

## Your first program

Create `hello.chester`:

```chester
def main(): Integer = {
  42
}
```

Compile to TypeScript (default):

```bash
nix develop --command ./_build/default/bin/main.exe hello.chester
# → out/compiler.ts
```

Compile to Go:

```bash
nix develop --command ./_build/default/bin/main.exe --go -o hello.go hello.chester
```

Format a file:

```bash
nix develop --command ./_build/default/bin/chester_fmt.exe hello.chester
```

## TypeScript / Node FFI

Current Chester uses `extern ts` blocks (not draft-style `import "pkg"`):

```chester
extern ts "node:console" {
  def log(msg: String): Any;
};

def main(): Any = {
  log("Hello from Chester on Node!")
}
```

See `examples/ts/hello.chester` and `scripts/dts2chester.mjs` for bindgen from `.d.ts`.

## React example

The counter app under `examples/counter/` shows Vite + React + Chester codegen:

```bash
nix develop --command bash examples/counter/build-chester.sh
cd examples/counter && npm install && npm run build:app
```

## Go FFI

Go examples use bare package selectors (e.g. `fmt.Println`) — see `examples/go/`
and `stdlib/go/std.chester`. Pre-baked Go package signatures for a future extractor
live in `data/go-signatures.json` (ported from `chester2026draft`).

## Learn more

- [Universal Parsed Syntax](universal-syntax.md) — CST / expander / elaborator pipeline
- [Literature](LITERATURE.md) — foundational papers
- [AGENTS.md](../AGENTS.md) — language rules for contributors

## Troubleshooting

### `dune: command not found`

Use `nix develop` to enter the dev shell defined in `flake.nix`.

### Coq / Rocq version mismatch

Always build via `coq_makefile -f _CoqProject -o Makefile && make` inside `nix develop`.

### Counter example npm build fails

Run `bash examples/counter/build-chester.sh` from the repo root first to generate
`examples/counter/src/gen/counter.ts`.
