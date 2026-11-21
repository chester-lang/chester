# Chester Effects Guide

Chester supports a practical subset of algebraic effects and handlers. Effects let you
write computations that `perform` operations; a lexical `handle` supplies capabilities.
This is **not** a full Plotkin/Pretnar or Effekt implementation — see caveats below and
`docs/paper/LITERATURE.md`.

## Declaring Effects

Effects are declared with the `effect` keyword. Type parameters (e.g. `State[T]`) may
appear in operation signatures; the elaborator instantiates them at use sites where
supported by the registry.

```chester
effect State[T] {
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

### Incomplete handlers

The elaborator rejects a `handle` that omits any operation declared on the effect
(`incomplete handler for effect: …`). The core checker also rejects a handle with an
empty clause list. At runtime, a perform with no matching capability still throws /
panics (`Unhandled effect operation`).

### Deep handlers

Before a handler clause runs, the matched capability frame (and anything above it) is
**truncated** from the stack so effects performed inside the clause escape to outer
handlers (Plotkin/Pretnar deep semantics). The `handle` `finally` only pops if that
frame is still on top (compare by identity / frame id).

### Multi-shot resume (answer-stream)

`resume` may be called more than once. The TS/Go runtime implements multi-shot via an
**answer stream**: each `resume(v)` forks a replay of the body from the perform, feeding
prior answers then `v`. Limitation: this replays a **prefix** of the body stream; it is
not a full delimited-continuation capture (side effects before `perform` re-execute).

Perform sites encode `EffectName/op` so the runtime matches both the handler **label** and
the operation name (not op-name alone across unrelated frames).

## `box` / `unbox`

`box { … }` captures the currently active capabilities listed for the boxed thunk
(evidence). `unbox` reinstalls that evidence and runs the body. If a claimed label is
missing at the box site, the runtime throws / panics (`Missing capability for box`) —
silent dropping is not allowed.

## Effect Rows in Signatures

Functions may carry effect rows with `/ [label]` syntax. Stdlib I/O uses lowercase
**`io`** (e.g. `Unit / [io]` in `stdlib/*/std.chester`), not `IO`. Labels are
case-sensitive strings in the effect registry.

```chester
def do_io(): Unit / [io] = {
  // perform IO operations
}
```

## Termination and `div`

A recursive function is accepted when every self-call passes a strictly smaller argument: `int_sub` / `prim__int_sub` of a parameter by a positive literal, `advance` / `prim__list_tail` of a parameter, or a field bound by matching on that parameter. `let` names bound to such a decrease count as smaller too.

Any other recursion must carry the `div` effect. An unannotated definition gets `div` inferred. A written row such as `/ [io]` that omits `div` is rejected. `div` is a classifier, not a capability: it does not need a `handle`.

```chester
def fact(n: Integer): Integer = {
  if int_eq(n, 0) then 1 else fact(int_sub(n, 1))
};

def loop(): Integer / [div] = loop();
```

## See also

- [Getting Started](getting-started.md)
- [Literature (effects papers)](../paper/LITERATURE.md)
- [Docs hub](../README.md)
