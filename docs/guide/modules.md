# Modules and Functors (SML-Core)

Chester implements an SML-style module system informed by *F-ing modules* (Rossberg, Russo, Dreyer). It supports structures, signatures, transparent and opaque sealing, generative and applicative functors, type sharing, first-class modules, and file-as-module imports.

The semantics are verified in `theories/CoreTyping.v` and `theories/TypeSystemProps.v`, executed by `theories/Elaborator.v`, and tested in `tests/modules*.chester`.

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

Chester supports two signature ascription modes:

### Transparent Sealing (`:`)
The module is checked against the signature, but exported types and structure remain visible:

```chester
module Visible : Printable {
  def show(): String = "visible"
};
```

### Opaque Sealing (`:>`)
Definitions not in the signature are hidden (data abstraction). Abstract types in the signature hide their concrete representation:

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

Any attempt to call `Counter.secret_helper()` or assume `Counter.t` is `Int` outside the module is rejected by the elaborator.

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

Functors are parameterized modules (functions from modules to modules).

### Generative Functors
By default, functors generate fresh abstract types upon each application:

```chester
module MakeSet(Element: Printable) {
  def render(x: String): String = Element.show()
};

module S1 = MakeSet(Counter);
```

### Applicative Functors (`module app`)
Prefixing with `app` declares an applicative functor, where applications to equivalent module arguments share type identities:

```chester
module app Id(X: Printable) {
  def tag(): String = "id"
};

module P = Id(Counter);
```

---

## 5. Type Components & Sharing (`with type`)

Signatures can refine type components using `with type` sharing constraints:

```chester
signature Seq = SeqSig with type item = Int;
```

---

## 6. First-Class Modules (`pack` and `unpack`)

Modules can be packaged as first-class runtime values and unpacked into local structures:

```chester
// Pack structure M as signature S
let packed = pack Counter as Printable;

// Unpack into local module X in body
unpack (X : Printable) = packed in {
  X.show()
};
```

---

## 7. File-as-Module Imports

Chester supports modular multi-file compilation:

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
