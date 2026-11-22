# Chester Effects Guide

Chester supports a practical subset of algebraic effects and handlers. Effects let you
write computations that `perform` operations; a lexical `handle` supplies capabilities.
This is **not** a full Plotkin/Pretnar or Effekt implementation — see caveats below and
`docs/paper/LITERATURE.md`.

## Declaring Effects

Effects are declared with the `effect` keyword. Type parameters use parentheses
(e.g. `State(T)`), same as type applications / enum params — not `[]`.

```chester
effect State(T) {
  def get(): T;
  def put(s: T): Unit;
}
```

## Performing and Handling Effects

```chester
def main() = {
  handle {
    let x = perform get();
    perform put(x);
    x
  } with State {
    case get() => resume(42);
    case put(s) => resume(())
  }
}
```

Inside handler cases, `resume(value)` returns to the perform site. Prefer `resume(())`
for the unit value (legacy bare `Unit` is still accepted).

### Return / value clause

An optional `case return(x) => …` maps the pure result of the `handle` body. The clause
runs **after** the handler frame is popped (Plotkin: return outside the handler). If
omitted, the body value is the handle result (identity return).

```chester
handle { … ; 1 } with Choose {
  case choose() => resume(true);
  case return(x) => int_add(x, 10)
}
```

### Incomplete handlers (forwarding)

Handlers may omit operations. Missing ops are not installed on the frame; at runtime a
perform continues searching outer frames (Effekt-style forwarding). Only when no outer
match exists does the runtime throw / panic (`Unhandled effect operation`).

### Deep vs shallow

Default `handle` is **deep**: resume reinstalls the handler around the continuation
(truncate + answer-stream). Opt in to shallow with `handle shallow Eff { … }`: resume
does **not** reinstall that frame around the continuation.

```chester
handle shallow {
  perform choose()
} with Choose {
  case choose() => resume(true)
}
```

### One-shot vs multi-shot resume

Default resume is **one-shot**: a second `resume` errors
(`one-shot resume: continuation already resumed`). Opt in with `handle multishot`:

```chester
handle multishot {
  …
} with Choose { … }
```

Multi-shot uses an **answer stream** (prefix replay of the body from the perform). It is
**not** delimited-continuation capture; side effects before `perform` re-execute.

Modifiers may combine: `handle shallow multishot { … } with Eff { … }`.

Perform sites encode `EffectName/op` so the runtime matches both the handler **label** and
the operation name (not op-name alone across unrelated frames).

## `box` / `unbox`

`box { … }` captures the currently active capabilities claimed by the boxed thunk
(evidence). Caps must be on the lexical stack at the box site — empty / unsealed capture
is rejected (`Missing capability for box`). `unbox` reinstalls that evidence and runs the
body.

## Effect Rows in Signatures

Annotated rows use braces: `/ {label}`. Annotated rows are **closed** (must list all
effects). Open rows appear only via inferred unannotated defs (`with_open_row`). Stdlib
I/O uses lowercase **`io`** (e.g. `Unit / {io}`), not `IO`.

```chester
def do_io(): Unit / {io} = {
  // FFI / print — not a user-handleable algebraic effect
}
```

### `io` ambient

`io` is an **ambient top-level capability**: the elaborator allows pending `io` at program
top / prelude boundary, and the runtime installs a trivial ambient `io` frame for script
entry. It is **not** a user-`handle`able algebraic effect (unlike `State` / `Choose`).

### `div` classifier

`div` is a termination classifier, not a capability: it does not need a `handle`.

## Termination and `div`

A recursive function is accepted when every self-call passes a strictly smaller argument: `int_sub` / `prim__int_sub` of a parameter by a positive literal, `advance` / `prim__list_tail` of a parameter, or a field bound by matching on that parameter. `let` names bound to such a decrease count as smaller too.

Any other recursion must carry the `div` effect. An unannotated definition gets `div` inferred. A written row such as `/ {io}` that omits `div` is rejected.

```chester
def fact(n: Integer): Integer = {
  if int_eq(n, 0) then 1 else fact(int_sub(n, 1))
};

def loop(): Integer / {div} = loop();
```

## Delimiter roles (effects-related)

| Form | Syntax |
|------|--------|
| Effect / enum / record type params | `effect State(T)`, `enum Option(T)` — `()` |
| Type applications | `Option(T)`, `List(T)` — `()` |
| Effect rows | `Unit / {io}` — `{}` |
| Implicit binders / apps | `def f[T]`, `f[A]` — `[]` |
| List literals | `[1, 2, 3]` — `[]` |

## See also

- [Getting Started](getting-started.md)
- [Literature (effects papers)](../paper/LITERATURE.md)
- [Docs hub](../README.md)
