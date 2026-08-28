# Universal Parsed Syntax (UPS)

Chester's front end separates **parsing** from **language recognition**. The lexer and
parser produce a generic Concrete Syntax Tree (CST); the expander and elaborator assign
meaning. This matches the design in `theories/CST.v`, `theories/Parser.v`,
`theories/Expander.v`, and `theories/Elaborator.v`.

## Pipeline

```
source → Lexer (bin/lexer.ml)
       → Parser (theories/Parser.v)     … generic CST (Symbol, Tuple, Block, …)
       → Expander (theories/Expander.v) … surface CST (DefCST, EffectCST, …)
       → Elaborator (theories/Elaborator.v) … AST (theories/AST.v)
       → Backends (theories/Backend.v)  … TypeScript / Go / Rocq
```

The verified theories are extracted to OCaml (`extraction/Compiler.ml`) and used by
`bin/main.exe`, `bin/chester_fmt.ml`, and `dune runtest`.

## Generic CST nodes (parser output)

After parsing, most surface syntax is still untyped trees (`theories/CST.v`):

| Node | Role |
|------|------|
| `Symbol` | Any identifier or punctuation token (`def`, `+`, `main`, …) |
| `Tuple` | Parenthesized groups; also used for calls `f(x)` |
| `ListLiteral` | `[a, b]` lists and implicit type args `f[T]` |
| `Block` | `{ stmts; tail }` with optional trailing expression |
| `SeqOf` | Flattened statement/expression sequences |
| `StringLiteral` / `IntegerLiteral` / `BoolLiteral` | Literals |
| `CommentCST` | `//` comments preserved for the formatter |

`theories/CST.v` also documents an example: `def main(): Unit = { println("hello") }`
is initially a `SeqOf` of symbols and blocks before expansion.

## Expander: surface syntax recognition

`theories/Expander.v` walks generic CST and builds typed CST nodes such as `DefCST`,
`EffectCST`, `HandleCST`, `MatchCST`, `ExternCST`, and `MacroDefCST`. It also:

- Collapses application syntax (`collapse_apps`) including `f(x)`, `obj.field`, and
  implicit args `f[T]`.
- Recognizes `if … then … else` via `expand_if`.
- Maps `import` / `extern` forms to `ImportCST` / `ExternCST`.

**Important:** keywords like `def`, `effect`, and `handle` are **not** hardcoded in the
lexer. They are ordinary `Symbol` nodes until the expander matches them.

## Elaborator: CST → AST

`theories/Elaborator.v` type-checks expanded CST into semantic AST (`theories/AST.v`),
including effects (`perform`, `resume`, `box`), dependent types, and FFI imports.

## Design benefits

### 1. Keyword-free parsing

The parser tokenizes identifiers and operators uniformly. For example:

```
def main(): Unit = { println("hello") }
```

is parsed roughly as:

- `Symbol("def")`, `Symbol("main")`, `Tuple(…)`, `Symbol(":")`, `Symbol("Unit")`,
  `Symbol("=")`, `Block(…)`

Adding a new statement form usually means updating the **expander** (and elaborator), not
the tokenizer or delimiter parser.

### 2. IDE-friendly spans

CST nodes carry `Span` values with UTF-16-aware positions (`WithUTF16` in
`theories/CST.v`), which aligns with JavaScript/TypeScript editor APIs.

### 3. Error recovery

`theories/Parser.v` defines a `sync` tokenizer skip that resumes at `;`, `}`, `)`, or
`]`. This supports partial trees for formatting and future LSP work (`bin/chester_fmt.ml`
already reformats from CST).

## What this does *not* mean

- The expander **does** encode Chester-specific grammar; UPS is not a parser for arbitrary
  languages without changing `Expander.v`.
- Self-hosted Chester sources (`self-hosted/*.chester`) reimplement lexer/parser/expander in
  Chester itself; they follow the same UPS philosophy but are a separate bootstrap track.

## See also

- [Universal Parsed Syntax](universal-syntax.md) — CST / expander / elaborator pipeline
- `AGENTS.md` — contributor rules for current surface syntax
