# Statements

## Scope of `let` and `def`

In Chester, `let` and `def` are used to declare bindings, but they differ in how they handle scoping and forward references. Understanding these differences is crucial for writing correct and efficient Chester programs.

### `let` Bindings

- **Local Scope**: `let` bindings are only visible **after** their declaration within the current block.
- **No Forward References**: You cannot reference a `let` binding before it's declared.
- **Type Inference**: If no type annotation is provided, the compiler infers the type from the binding's body.

**Example:**

```chester,playground,editable
// Correct usage of 'let'
let x = 5;
let y = x; // 'x' is defined before use
```

```chester,playground,editable
// Incorrect usage of 'let'
let y = x + 2; // Error: 'x' is not defined yet
let x = 5;
```

### `def` Bindings

- **Global Scope**: `def` bindings are visible throughout the entire block, even before their declaration.
- **Allows Forward References**: You can reference a `def` binding before it's declared.
- **Type Annotation Required for Forward References**: If you use a `def` binding before its declaration, you must provide a type annotation.

**Example:**

```chester,playground,editable
// Correct usage of 'def' with type annotation
def y = square(5); // 'square' is used before its declaration

def square(n: Int) = n * n; // Type annotation for 'n' is required
```

```chester,playground,editable
// Incorrect usage of 'def' without type annotation
def y = increment(5); // 'increment' is used before its declaration

def increment(n) = n + 1; // Error: Missing type annotation for 'n'
```

### Summary of Scoping Rules

- **`let` Bindings**:
  - Visible only after their declaration within the current block.
  - Do **not** allow forward references.
  - Type annotations are optional if the type can be inferred.

- **`def` Bindings**:
  - Visible throughout the entire block.
  - Allow forward references.
  - Require type annotations when used before their declarations.

### Compiler Behavior

When processing a block, the Chester compiler handles `let` and `def` bindings differently to manage scope and type checking.

#### Processing `def` Bindings

1. **Collection Phase**:
   - The compiler collects all `def` bindings, noting their names, type annotations, and identifiers.
   - It tracks forward references to detect usages before declarations.

2. **Type Annotation Checks**:
   - For forward-referenced `def` bindings without type annotations, the compiler reports a `MissingTypeAnnotationError`.

3. **Context Updates**:
   - The compiler adds placeholders or inferred types to the context, allowing forward-referenced `def` bindings to be used.

#### Processing `let` Bindings

- **Sequential Processing**:
  - `let` bindings are processed in order of their appearance.
  - Each `let` binding is added to the context **after** its declaration.
- **No Forward References**:
  - Referencing a `let` binding before its declaration results in an error.

### Best Practices

- Use `let` when you don't need to reference the binding before its declaration.
- Use `def` when you need forward references or are defining recursive functions.
- Always provide type annotations for `def` bindings that are forward-referenced to avoid compilation errors.

By understanding these scoping rules, you can write more predictable and maintainable Chester code.