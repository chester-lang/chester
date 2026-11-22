# Chester Syntax Grammar

This reference documents the Concrete Syntax Tree (CST) and surface grammar implemented by Chester's verified front end (`theories/Parser.v`, `theories/Expander.v`, `theories/Elaborator.v`) and self-hosted compiler (`self-hosted/*.chester`).

Chester uses **Universal Parsed Syntax (UPS)**: parsing is keyword-free and delimiter-driven into a generic CST (Symbols, Tuples, Lists, Blocks), while the expander and elaborator recognize constructs and bind semantic forms.

---

## 1. Top Level & Declarations

```bnf
Program       ::= Statement*

Statement     ::= DefDecl
                | LetBinding
                | VarBinding
                | AssignStmt
                | EffectDecl
                | EnumDecl
                | ExtensionDecl
                | ModuleDecl
                | SignatureDecl
                | ImportDecl
                | ExternDecl
                | InfixDecl
                | Expr (';' | Newline)?
```

### Definitions (`def`)
```bnf
DefDecl       ::= 'def' ID TypeParams? '(' ParamList? ')' (':' TypeAnnotation)? ('/' EffectRow)? '=' Expr ';'?
TypeParams    ::= '[' ID (',' ID)* ']'
ParamList     ::= Param (',' Param)*
Param         ::= ID (':' TypeAnnotation)?
EffectRow     ::= '[' ID (',' ID)* ']'
```

### Bindings & Assignments
```bnf
LetBinding    ::= 'let' (ID | Pattern) (':' TypeAnnotation)? '=' Expr ';'?
VarBinding    ::= 'var' ID (':' TypeAnnotation)? '=' Expr ';'?
AssignStmt    ::= ID '=' Expr ';'?
```

### Algebraic Effects (`effect`, `handle`)
```bnf
EffectDecl    ::= 'effect' ID TypeParams? '{' EffectOp* '}' ';'?
EffectOp      ::= 'def' ID '(' ParamList? ')' ':' TypeAnnotation ';'?

HandleExpr    ::= 'handle' Block 'with' ID '{' HandlerCase* '}'
HandlerCase   ::= 'case' ID '(' ParamList? ')' '=>' Expr ';'?
                | 'case' 'return' '(' ID ')' '=>' Expr ';'?
                  (* optional Plotkin-style return/value clause *)
PerformExpr   ::= 'perform' ID '(' ExprList? ')'
ResumeExpr    ::= 'resume' '(' Expr ')'
BoxExpr       ::= 'box' '(' Expr ')'
UnboxExpr     ::= 'unbox' '(' Expr ')'
```

### Enums & Pattern Matching
```bnf
EnumDecl      ::= 'enum' ID TypeParams? '{' EnumVariant (';' EnumVariant)* ';'? '}' ';'?
EnumVariant   ::= 'case' ID ('(' RecordFields? ')')?
RecordFields  ::= RecordField (',' RecordField)*
RecordField   ::= ID ':' TypeAnnotation

MatchExpr     ::= 'match' Expr '{' MatchArm* '}'
MatchArm      ::= 'case' Pattern '=>' Expr ';'?
Pattern       ::= '_'
                | ID
                | ID '(' (ID (',' ID)*)? ')'
                | QualifiedName ('(' (ID (',' ID)*)? ')')?
```

### Modules & Signatures (SML-Core)
```bnf
ModuleDecl    ::= 'module' 'app'? ID FunctorParam? ModuleSeal? '{' Statement* '}' ';'?
                | 'module' ID '=' FunctorApp ';'?
FunctorParam  ::= '(' ID ':' ID ')'
ModuleSeal    ::= ':>' ID           /* Opaque sealing */
                | ':' ID            /* Transparent sealing */
FunctorApp    ::= ID '(' ID ')'

SignatureDecl ::= 'signature' ID SignatureWith? '{' SigMember* '}' ';'?
SignatureWith ::= 'with' 'type' ID '=' TypeAnnotation
SigMember     ::= 'def' ID TypeParams? '(' ParamList? ')' ':' TypeAnnotation ('/' EffectRow)? ';'?
                | 'type' ID ('=' TypeAnnotation)? ';'?

FirstClassMod ::= 'pack' ID 'as' ID
                | 'unpack' '(' ID ':' ID ')' '=' Expr 'in' Expr
```

### Extensions & Methods
```bnf
ExtensionDecl ::= 'extension' ID TypeParams? ('on' | 'for') TypeAnnotation '{' DefDecl* '}' ';'?
```

### Imports & Foreign Function Interface (FFI)
```bnf
ImportDecl    ::= 'import' ID ('from' STRING_LITERAL)? ';'?
ExternDecl    ::= 'extern' 'ts' STRING_LITERAL '{' ExternDef* '}' ';'?
ExternDef     ::= 'def' ID '(' ParamList? ')' ':' TypeAnnotation ';'?
```

### Operator Declarations
```bnf
InfixDecl     ::= ('infixl' | 'infixr') STRING_LITERAL InfixRel? ';'?
InfixRel      ::= 'same_as' STRING_LITERAL
                | 'tighter_than' STRING_LITERAL
                | 'looser_than' STRING_LITERAL
```

---

## 2. Expressions

```bnf
Expr          ::= Atom
                | Expr InfixOp Expr
                | PrefixOp Expr
                | Expr PostfixOp
                | IfExpr
                | MatchExpr
                | HandleExpr

Atom          ::= Literal
                | ID
                | QualifiedName
                | TupleExpr
                | ListLiteral
                | Block
                | LambdaExpr
                | FunctionCall
                | MethodCall
                | PerformExpr
                | ResumeExpr
                | BoxExpr
                | UnboxExpr

TupleExpr     ::= '(' ExprList? ')'
ListLiteral   ::= '[' ExprList? ']'
Block         ::= '{' Statement* Expr? '}'
ExprList      ::= Expr (',' Expr)* ','?

LambdaExpr    ::= '\' ID '=>' Expr
                | '\' '(' ParamList? ')' '=>' Expr

FunctionCall  ::= Atom '(' ExprList? ')'
MethodCall    ::= Atom '.' ID ('(' ExprList? ')')?
QualifiedName ::= ID '.' ID

IfExpr        ::= 'if' Expr 'then' Block 'else' (Block | IfExpr)
```

---

## 3. Lexical Structure & Literals

```bnf
Literal       ::= INT_LITERAL
                | STRING_LITERAL
                | BOOL_LITERAL
                | UNIT_LITERAL

UNIT_LITERAL   ::= '()'
BOOL_LITERAL   ::= 'true' | 'false'
INT_LITERAL    ::= [0-9]+ | '0x'[0-9a-fA-F]+ | '0b'[01]+
STRING_LITERAL ::= '"' [^"\\]* '"'
ID             ::= [a-zA-Z_][a-zA-Z0-9_]*
InfixOp        ::= '+' | '-' | '*' | '/' | '%' | '==' | '<' | '<=' | '>' | '>=' | ID
```

---

## 4. Key Semantic Notes

- **Unit Type vs Value**: `Unit` is the unit **type**; `()` is the unit **value**.
- **First-Class Types**: Types like `Int`, `String`, `Bool`, `Unit`, `Type` can be passed as values.
- **Effect Annotations**: Written `/ [eff1, eff2]`. Non-structural recursion requires `/ [div]` (or is inferred as `div`).
- **Punctuation Independence**: Token delimiters `()`, `[]`, `{}` are parsed prior to semantic keyword analysis.

---

## See Also

- [Guide: Universal Parsed Syntax](../guide/universal-syntax.md)
- [Guide: Effects & Handlers](../guide/effects.md)
- [Guide: Modules & Functors](../guide/modules.md)
- [Guide: Getting Started](../guide/getting-started.md)
