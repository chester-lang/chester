# Modules and Functors (SML-Core)

Chester implements an SML-style module *sketch* informed by *F-ing modules* (Rossberg,
Russo, Dreyer). It is **not** an F-ing elaboration into System Fω. Structures, signatures,
opaque/transparent sealing, generative and name-based applicative functors, type sharing,
first-class pack/unpack, and file-as-module imports are supported; higher-order functors
are absent.

The sealing/export checks that *are* proven live in `theories/CoreTyping.v` and
`theories/TypeSystemProps.v`; elaboration is in `theories/Elaborator.v`; fixtures in
`tests/modules*.chester`.

---

## 1. Structures and Signatures

### Structures
A module groups definitions, types, and submodules:

```chester
module Math {
  def add(a: Int, b: Int): Int = a + b;
  def mul(a: Int, b: Int): Int = a * b
};

def main(): Int = Math.add(40, 2);
```

### Signatures
A signature specifies the interface of a module:

```chester
signature Printable {
  def show(): String
};

module Counter {
  def show(): String = "42";
  def internal_secret(): Int = 99
};
```

---

## 2. Sealing: Transparent vs. Opaque

### Transparent ascription (`:`)
The module is checked against the signature; **implementation types remain visible**
(transparent view of type components that were defined concretely).

```chester
module Visible : Printable {
  def show(): String = "visible"
};
```

### Opaque sealing (`:>`)
Definitions not in the signature are hidden. Abstract `type t` in the signature hides
its concrete representation outside the module.

```chester
signature CounterSig {
  type t;
  def zero: t;
  def bump(c: t): t;
  def value(c: t): Int
};

module Counter :> CounterSig {
  type t = Int;
  def zero: Int = 0;
  def bump(c: Int): Int = c + 1;
  def value(c: Int): Int = c;
  def secret_helper(): Unit = ()
};
```

Any attempt to call `Counter.secret_helper()` or treat `Counter.t` as `Int` outside the module
is rejected (opaque exports keep signature types abstract — `Counter.zero` has type `Counter.t`,
not `Int`).

---

## 3. Effect Rows under Sealing

Functions in signatures can specify effect bounds. Under sealing, the elaborator checks that the module implementation's effect row is subsumed by the signature's declared row:

```chester
signature SafeIO {
  def run(): Unit / [io]
};

// Valid: empty effect row subsumed by [io]
module Impl :> SafeIO {
  def run(): Unit = ()
};
```

If an implementation performs an unadvertised effect, elaboration fails with an error (see `tests/modules_effects_bad.chester`).

---

## 4. Functors

### Generative Functors
By default, each application is a fresh structure identity (Chester’s generative default):

```chester
module MakeSet(Element: Printable) {
  def render(x: String): String = Element.show()
};

module S1 = MakeSet(Counter);
```

### Applicative Functors (`module app`)
Prefixing with `app` declares an applicative functor. Chester’s applicative identity is
**name-based** (same functor path + argument names), **not** F-ing generative stamps.

```chester
module app Id(X: Printable) {
  def tag(): String = "id"
};

module P = Id(Counter);
```

Functor parameters are first-order (signature-typed). Higher-order functor types /
`(X: S) → T` as modules are **not** implemented.

---

## 5. Type Components & Sharing (`with type`)

Signatures can refine type components using `with type` sharing constraints. Sharing is
**applied** during elaboration (constraints rewrite the signature’s type components):

```chester
signature Seq = SeqSig with type item = Int;
```

---

## 6. First-Class Modules (`pack` and `unpack`)

`pack M as S` packages a structure as a first-class value **sealed** to signature `S`.
`unpack` binds a local structure at that signature:

```chester
let packed = pack Counter as Printable;

unpack (X : Printable) = packed in {
  X.show()
};
```

---

## 7. File-as-Module Imports

```chester
import Math;
import Geometry from "geo/shapes.chester";

def main(): Int = Math.add(10, 20);
```

The compiler resolves imports using `--module-path` or the `CHESTER_PATH` environment variable (searching the current directory, repository root, and specified search paths).

---

## See Also

- [CLI Usage](cli-usage.md) — `--module-path` and `--prelude` flags
- [Syntax Grammar](../references/syntax-grammar.md)
- [Literature on Module Foundations](../paper/LITERATURE.md)
