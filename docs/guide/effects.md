# Chester Effects Guide

Chester supports algebraic effects and handlers. Effects allow you to write pure functions that can perform side effects (like state, I/O, or exceptions) which are then implemented by a handler.

## Declaring Effects

Effects are declared using the `effect` keyword:

```chester
effect State[T] {
  def get(): T;
  def put(s: T): Unit;
}
```

## Performing and Handling Effects

To use an effect, you `perform` it. The surrounding `handle` block specifies how to execute those effects.

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

Notice that inside the handler cases, you use `resume(value)` to return control to the point where the effect was performed. For effects that don't return a meaningful value, use `resume(())` instead of `resume(Unit)` (where `()` is the unit value).

## Effect Rows in Signatures

Functions may also carry effect rows indicating which effects they perform, using the `/ [effect_name]` syntax.

Example:
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
