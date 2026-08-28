# Go FFI signatures (reference data)

Ported from `chester2026draft/go-signatures.json`. These are pre-baked Go package
signatures intended for a future `go` import extractor CLI. The current compiler
uses bare selectors such as `fmt.Println` in `stdlib/go/std.chester` instead.

`bin/main.exe` supports `--module-path` and `CHESTER_PATH` for resolving relative
input paths; this JSON file is reference data until a Go signature loader is wired in.
