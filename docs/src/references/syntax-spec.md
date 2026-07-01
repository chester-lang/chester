# Universal Parsed Syntax Specification

This document defines the formal lexical and syntactic grammar of the Chester universal parsed syntax.

---

## Lexical Grammar

### 1. Characters and Code Points
All input is treated as a sequence of Unicode code points. Unicode emojis and private-use characters are fully supported as part of identifier naming rules.

### 2. Whitespace and Comments
- **Whitespace**: Any sequence of characters matching `java.lang.Character.isWhitespace`. Line breaks (`\n`, `\r`) are recorded inside whitespace tokens.
- **Line Comment**: Starts with `//` and consumes characters until a line break (`\n`, `\r`) or EOF.
- **Block Comment**: Starts with `/*` and ends with `*/`. Block comments support arbitrary nesting (e.g., `/* outer /* inner */ outer */`).

### 3. Literals
- **StringLiteral**: Double-quoted text. Escape sequences supported:
  - `\n` (newline)
  - `\t` (horizontal tab)
  - `\r` (carriage return)
  - `\\` (backslash)
  - `\"` (double quote)
  - Any other escape sequence `\<char>` triggers a tokenization error but falls back to the literal character `<char>`.
- **IntegerLiteral**: Consists of digits `[0-9]` optionally separated by underscores `_` (which are ignored during parsing).
- **RationalLiteral**: Consists of an integer literal, followed directly by `/`, followed directly by another integer literal.
- **SymbolLiteral**: Starts with a single quote `'` followed directly by identifier characters (e.g., `'foo`).

### 4. Identifiers and Operators
- **Identifier**:
  - Starts with a letter (`isLetter`), emoji (`codePointIsEmoji`), or `_`.
  - Can contain letters, emojis, digits, `_`, or `-` (middle wording symbol).
  - Must end with a letter, emoji, digit, or `_`.
- **OperatorIdentifier**:
  - Consists entirely of one or more operator characters from the set: `.:=-+\|<>/?`~!@$%^&*`.

---

## Syntactic Grammar (CST)

The Concrete Syntax Tree (CST) parses tokens into structural forms:

### EBNF Grammar

```ebnf
File            ::= BlockBody EOF
BlockBody       ::= ( Statement ";" )* Expression?
Statement       ::= SeqOf
Expression      ::= SeqOf

SeqOf           ::= Atom+
Atom            ::= Symbol
                  | StringLiteral
                  | IntegerLiteral
                  | SymbolLiteral
                  | Tuple
                  | ListLiteral
                  | Block
                  | Comment

Symbol          ::= Identifier | OperatorIdentifier | "."

Tuple           ::= "(" DelimitedBody ")"
ListLiteral     ::= "[" DelimitedBody "]"
DelimitedBody   ::= ( SeqOf "," )* SeqOf? ","?

Block           ::= "{" BlockBody "}"
```

### CST Mappings
- **`Symbol`**: Any identifier, operator, or single dot `.`.
- **`Tuple`**: Parentheses enclosing a comma-separated list of elements. Trailing commas are permitted.
- **`ListLiteral`**: Square brackets enclosing a comma-separated list of elements. Trailing commas are permitted.
- **`Block`**: Curly braces enclosing a list of statements separated by semicolons and ending with an optional tail expression.
- **`SeqOf`**: Syntactic concatenation of multiple elements without separating punctuation (e.g. `foo bar [1]` compiles to a `SeqOf` containing three items).
