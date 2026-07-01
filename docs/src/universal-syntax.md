# Universal Parsed Syntax

Chester utilizes a **Universal Parsed Syntax** represented by a generic Concrete Syntax Tree (CST). Unlike traditional compilers where the parser must be aware of language-specific constructs (such as variable declarations, class definitions, or control flow), Chester's parsing phase is completely agnostic of these details.

This design enables a flexible, robust, and tooling-friendly front-end that can adapt to changing language features or even serve as a base for completely different programming languages without modifying the tokenizer or parser.

---

## The CST Representation

The universal parsed syntax consists of a small number of generic structural elements:

| CST Node | Syntax Pattern | Purpose |
| :--- | :--- | :--- |
| **`Symbol(name)`** | `foo`, `+`, `my-var` | Standard names, identifiers, and operators. |
| **`Tuple(elements)`** | `(a, b, c)`, `()` | Commas-separated values enclosed in parentheses. |
| **`ListLiteral(elements)`** | `[1, 2, 3]`, `[]` | Commas-separated values enclosed in square brackets. |
| **`Block(elements, tail)`** | `{ stmt1; stmt2; expr }` | Semicolon-separated statements enclosed in curly braces, with an optional final expression. |
| **`SeqOf(elements)`** | `f(x)`, `let x = 42` | Sequences of adjacent atoms/nodes without separating commas or semicolons. |
| **`StringLiteral(value)`** | `"hello world"` | Double-quoted strings. |
| **`IntegerLiteral(value)`** | `42` | Numeric values. |
| **`Comment(text, kind)`** | `// comment`, `/* block */` | Line or block comments, preserved for formatting and documentation. |

---

## Design Benefits

### 1. Zero Language-Specific Keywords
The parser and tokenizer do not have hardcoded keywords like `if`, `while`, `def`, `class`, or `import`. The parser simply parses these words as generic `Symbol` nodes. 
For example, the statement:
```
def main(): Unit = { println("hello") }
```
is parsed into a `SeqOf` containing:
- `Symbol("def")`
- `Symbol("main")`
- `Tuple(...)`
- `Symbol(":")`
- `Symbol("Unit")`
- `Symbol("=")`
- `Block(...)`

### 2. De-coupled Elaboration Phase
Because parsing produces a generic tree of symbols and delimiters, the language's actual grammar rules and semantics are deferred to the **Elaboration** phase. During elaboration:
- The CST is validated and compiled into an **Abstract Syntax Tree (AST)**.
- If we want to change how functions are defined or add a new control flow keyword, we only need to update the Elaborator. The core parser remains untouched.

### 3. High Tolerance for Incomplete Code (IDE Friendly)
IDE tools require syntax trees for code that is currently being typed (and is therefore syntactically invalid). 
Because the universal parser only enforces matching delimiters (`()`, `[]`, `{}`), it can recover from errors extremely gracefully. For example, if a user writes:
```
{
  print(a
  b
}
```
The parser easily detects the missing parenthesis, records the error, recovers immediately at the semicolon/newline, and parses `b` as a subsequent statement in the block. This makes building language servers (LSP) and syntax highlighting tools straightforward.
