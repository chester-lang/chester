# Chester Syntax Grammar (BNF-like)

**IMPORTANT NOTE:** This document provides only a rough description of the Chester language syntax and might be highly incorrect or not fully aligned with the current implementation. It should be used as a general guide rather than a precise specification.

This document attempts to describe the Chester language syntax using a BNF-like notation.

**Note:** Whitespace and comments (`// ...`) are generally allowed between tokens and are not explicitly shown in the grammar rules unless significant (like newlines in blocks).

## Top Level

```bnf
Toplevel ::= Statement* (Expr | ';')?

Statement ::= Expr (';' | Newline)
```
*Note: In many contexts (like blocks or top-level), newlines can act as statement separators similar to semicolons. The last expression in a block acts as its return value unless followed by a semicolon.*

## Expressions

```bnf
Expr ::= Atom | Expr Operator Expr | Operator Expr | Expr Operator

Atom ::= ID
       | Literal
       | Tuple
       | List
       | Block
       | Object
       | Application
       | MethodCall
       | SymbolLookup
       | Keyword

Tuple ::= '(' ExprList? ')'
List  ::= '[' ExprList? ']'
Block ::= '{' Statement* Expr? '}'

ExprList ::= Expr (',' Expr)* ','?

Application ::= GenericApplication | FunctionCall | BlockApplication | CombinedApplication
GenericApplication ::= Atom '[' ExprList ']'
FunctionCall ::= Atom '(' ExprList? ')'
BlockApplication ::= Atom Block
CombinedApplication ::= GenericApplication FunctionCall
                      | GenericApplication BlockApplication
                      | FunctionCall BlockApplication
                      | GenericApplication FunctionCall BlockApplication

MethodCall   ::= Atom '.' ID ('(' ExprList? ')')?  // Parentheses are optional for field access
Attribute ::= '@' ID // NOT IMPLEMENTED YET
Keyword      ::= '#' ID (GenericApplication | FunctionCall)*
```

*Note: All identifiers are treated uniformly at the syntax level, with no distinction between different kinds of identifiers. Operator precedence and associativity are handled during semantic analysis, not in the grammar.*

## Object Literals

```bnf
Object ::= '{' ObjectClauseList? '}'

ObjectClauseList ::= ObjectClause (',' ObjectClause)* ','?

ObjectClause ::= ObjectKey ('=' | '=>') Expr

ObjectKey ::= ID
            | QualifiedName
            | STRING_LITERAL
            | SYMBOL_LITERAL

QualifiedName ::= ID ('.' ID)*
```

## Literals

```bnf
Literal ::= INT_LITERAL
          | RAT_LITERAL
          | STRING_LITERAL
          | SYMBOL_LITERAL

SYMBOL_LITERAL ::= "'" ID
```

## Terminals

```bnf
ID             ::= /* Starts with letter, _, or emoji; contains letters, digits, _, emoji */
Operator       ::= /* Sequence of operator symbols like +, -, *, /, =, ->, =>, :, ., etc. */
INT_LITERAL    ::= /* Integer in decimal, hex (0x...), or binary (0b...) format */
RAT_LITERAL    ::= /* Rational/float literal (e.g., 1.2, 3.14e-5) */
STRING_LITERAL ::= /* Double-quoted string "..." with escapes */
Newline        ::= /* \n or \r\n */
```

## Punctuation and Symbols (Terminals)

```bnf
'(' | ')' | '{' | '}' | '[' | ']' | ',' | ';' | ':' | '.' | '=' | '=>' | '->' | '@' | '#'
``` 