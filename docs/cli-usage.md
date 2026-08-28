# CLI Usage

The Chester compiler ships as OCaml executables built by Dune inside the Nix
dev shell.

## Binaries

| Command | Role |
|---------|------|
| `_build/default/bin/main.exe` | Parse, elaborate, type-check, emit TS/Go/Rocq |
| `_build/default/bin/chester_fmt.exe` | Format `.chester` sources |
| `_build/default/bin/chester_bindgen.exe` | Generate Chester `extern` blocks from `.d.ts` |

Typical invocation:

```bash
nix develop --command ./_build/default/bin/main.exe [options] file.chester
```

## `main.exe` options

| Flag | Description |
|------|-------------|
| `--go` | Emit Go instead of TypeScript |
| `--rocq` | Emit Rocq |
| `--ts-module` | Emit TypeScript as an ES module (no script preamble) |
| `--emit-ts-runtime PATH` | Write `runtime/chester-runtime.ts` and exit |
| `-o PATH` / `--output PATH` | Output file (default: `out/compiler.{ts,go,v}`) |
| `--module-path DIR` | Add a directory to the search path (repeatable) |
| `--prelude FILE` | Elaborate a prelude file before inputs (repeatable, not emitted) |

### Search paths

`--module-path` and the `CHESTER_PATH` environment variable (colon-separated on
Unix) are used to resolve relative input paths. The compiler also searches the
repository root (directory containing `dune-project` or `theories/`) and the
current working directory.

Example:

```bash
export CHESTER_PATH="$PWD/examples:$PWD/stdlib"
nix develop --command ./_build/default/bin/main.exe go/simple.chester
```

Or pass an explicit path (repo root and cwd are searched automatically):

```bash
nix develop --command ./_build/default/bin/main.exe examples/go/simple.chester
```

### Prelude files

`--prelude` elaborates Chester sources before your main inputs so definitions
and effects are available across files. Prelude files are **not** emitted to
the output — only the listed input files are.

```bash
nix develop --command ./_build/default/bin/main.exe \
  --prelude stdlib/std.chester \
  my-app.chester
```

You can also pass multiple `.chester` files directly; elaboration state threads
from earlier files to later ones.

## `chester_fmt.exe`

Format a file in place (skips if output is not idempotent yet):

```bash
nix develop --command ./_build/default/bin/chester_fmt.exe path/to/file.chester
```

## `chester_bindgen.exe`

See `scripts/dts2chester.mjs` and `test/bindgen/` for React/TypeScript FFI
generation.

## Examples

```bash
# TypeScript (default)
nix develop --command ./_build/default/bin/main.exe \
  -o out/hello.ts examples/ts/hello.chester

# Go
nix develop --command ./_build/default/bin/main.exe --go \
  -o /tmp/hello.go examples/go/hello.chester

# Counter example pipeline
nix develop --command bash examples/counter/build-chester.sh
```

## See also

- [Getting Started](getting-started.md)
- [Statements & scoping](statements.md)
