# Go FFI signatures (reference data)

Ported from `chester2026draft/go-signatures.json`. These are pre-baked Go package
signatures intended for a future `go` import extractor CLI. The current compiler
uses bare selectors such as `fmt.Println` in `stdlib/go/std.chester` instead.

Not wired into `bin/main.exe` yet — kept for when module search paths land.
