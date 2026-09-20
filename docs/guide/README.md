# Guide

Practical documentation for people writing and running Chester programs.

| Page | Audience |
|------|----------|
| [Getting Started](getting-started.md) | Build with Nix, compile hello world, modules sketch |
| [CLI Usage](cli-usage.md) | Flags, `--module-path`, prelude, backends |
| [Effects](effects.md) | `effect` / `handle` / `perform` / `resume`, effect rows |
| [Statements & scoping](statements.md) | `let` vs `def`, blocks |
| [Universal Parsed Syntax](universal-syntax.md) | How CST → expander → elaborator works (conceptual) |
| [Go backend](go-backend.md) | `--go` emission and FFI style |

For exhaustive grammar, see the [Reference](../references/README.md). For papers and
formal foundations, see the [Paper](../paper/README.md) section.
