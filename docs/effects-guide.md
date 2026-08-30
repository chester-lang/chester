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
