# Chester Literature & References

Foundational reading for Chester's design. This repo implements a verified compiler
(Coq/Rocq theories in `theories/`, extracted OCaml in `extraction/`). The Scala drafts in
sibling directories explored many of the same ideas in a product/tooling stack.

**Honesty note:** Chester takes inspiration from these papers; it does **not** claim full
literature fidelity. Deviations are called out below and in the guides
([effects](../guide/effects.md), [modules](../guide/modules.md)).

Some papers are behind anti-bot protections or institutional firewalls (ACM Digital Library,
MIT DSpace, etc.) and cannot be fetched automatically. Use the direct links below, or the
local PDF copies in the sibling `chester-references` repository (`../../chester-references/`).
Where earlier local copies were corrupted or wrong papers, they were replaced when a
verified mirror was available.

## 1. Elaboration & extensibility

- **"ExEl: Building an Elaborator Using Extensible Constraints"** (Bohdan Liesnikov, Jesper Cockx)
  - *Significance:* Inspiration for Chester's UPS → CST → elaborator pipeline
    (`theories/CST.v`, `theories/Expander.v`, `theories/Elaborator.v`). Chester does **not**
    implement ExEl-style extensible constraint plugins.
  - [ACM Digital Library (DOI: 10.1145/3652561.3652565)](https://dl.acm.org/doi/pdf/10.1145/3652561.3652565)
  - Local PDF: `../../chester-references/3652561.3652565.pdf`

## 2. Propagator networks (historical / exploratory)

The 2025/2026 Scala drafts experimented with propagator-based type inference. The current
verified compiler uses the elaborator in `theories/Elaborator.v` instead. These remain useful
background if incremental or constraint-propagation elaboration returns.

- **"The Art of the Propagator"** (Alexey Radul, Gerald Jay Sussman)
  - *Significance:* Propagator networks for incremental computation; explored in Scala drafts, not the current verified elaborator.
  - [MIT DSpace (Handle: 1721.1/44215)](https://dspace.mit.edu/handle/1721.1/44215)
  - [Direct PDF (CSAIL TR; DSpace may block bots)](https://dspace.mit.edu/bitstream/handle/1721.1/44215/MIT-CSAIL-TR-2009-002.pdf)
  - Local PDF: `../../chester-references/The-Art-of-the-Propagator-Radul-Sussman.pdf`
    (recovered via Wayback when live mirrors blocked automated download)
- **"Propagation Networks: A Flexible and Expressive Substrate for Computation"** (Alexey Radul, Ph.D. thesis)
  - *Significance:* Comprehensive foundation of the propagator model.
  - [MIT DSpace (Handle: 1721.1/49525)](https://dspace.mit.edu/handle/1721.1/49525)
  - [Author mirror](http://web.mit.edu/~axch/www/phd-thesis.pdf)
  - Local PDF: `../../chester-references/Propagation-Networks-Radul-thesis.pdf`
    (kept when Art-of-the-Propagator mirrors fail)

## 3. Algebraic effects & capabilities (partial in Chester)

Chester's surface (`effect` / `handle` / `perform` / `resume`, `box` / `unbox`) follows the
algebraic-effects / capability line. Runtime deep handlers + answer-stream multi-shot live in
`bin/effects_runtime.ml`. Perform encodes `Effect/op` so dispatch matches **label and op**.
Lambdas keep latent effects on the Pi (not charged at creation). Match binders use constructor
field types when the ctor is in scope.

**Still out of scope / partial vs literature:**
- no return/value clauses; multi-shot is answer-stream prefix replay (not delimited continuations)
- no equational handler theory; not full Effects-as-Capabilities (stack + evidence, not explicit
  capability parameters)
- `io` is a phantom FFI row (ambient at top level); open rows are Koka-lite (row var accepts labels)
- `CoreFrag` / emit proofs exclude handlers; Rocq `__chester_*` are Parameters
- no shallow handlers / forwarding

See `docs/guide/effects.md`.

- **"Handling Algebraic Effects"** (Plotkin & Pretnar, 2013)
  - Local PDF: `../../chester-references/handling-algebraic-effects-plotkin-pretnar-2013.pdf`
- **"Effekt: Extensible Algebraic Effects in Scala"** (Brachthäuser & Schuster, Scala'17 short paper)
  - Local PDF: `../../chester-references/effekt-2017.pdf`
- **"Effects as Capabilities: Effect Handlers and Lightweight Effect Polymorphism"** (Brachthäuser et al., 2020)
  - [ACM PACMPL (DOI: 10.1145/3428194)](https://dl.acm.org/doi/pdf/10.1145/3428194)
  - [EPFL Infoscience mirror](http://infoscience.epfl.ch/record/288487)
  - Local PDF: `../../chester-references/Effects-as-Capabilities-2020.pdf`

## 4. Dependent types (citation only / partial surface)

Chester has basic dependent features (`Type` as a kind, polymorphic `enum` field
types, `tests/dependent_types.chester`). Length-indexed enums are **not** implemented
(`tests/vector.chester` is a non-indexed `Vector[T]`). Soft spots remain: `[Any]` /
`[Unknown]` wildcards and single-letter type-param refs. **Two-Level Type Theory is
citation-only** — not implemented as a 2LTT system in the theories.

- **"Two-Level Type Theory"** (Annenkov, Capriotti, Kraus, Sattler, 2017)
  - Local PDF: `../../chester-references/two-level-type-theory-annenkov-2017.pdf`

## 5. Module systems (SML-core sketch in Chester)

Chester supports ML-style structures, signatures, opaque (`:>`) and transparent (`:`) sealing,
generative and applicative (`module app`) functors, first-class `pack`/`unpack`, type components,
`with type` sharing (applied in elaboration; unknown names error), Rocq `Module`/`Module Type`
emit, and file-as-module import via `--module-path` / `CHESTER_PATH`.

Opaque sealing exports **unsubstituted** signature value types; manifest `type t = …` equality
is checked; nested module/enum/record components are retained under opaque keep when sealed.

**Still out of scope vs F-ing modules:**
- no System Fω packaging; applicative identity is **name-based**, not stamps
- no higher-order functors / `include` / `open` / `with module` / recursive modules
- sealing is not full Σ≤Ξ; functor application is export rewrite, not re-elaboration under subst
- modules outside `CoreFrag`; self-hosted elaborator does not seal/functors
See `docs/guide/modules.md`.

- **"F-ing modules"** (Rossberg, Russo, Dreyer)
  - [Journal PDF](https://people.mpi-sws.org/~dreyer/papers/f-ing/firstview-journal.pdf)
  - Local PDF: `../../chester-references/f-ing-modules.pdf`
- OCaml module manual — [structures, signatures, functors](https://ocaml.org/manual/5.1/moduleexamples.html)

## See also

- [Getting Started](../guide/getting-started.md) — build, CLI, and first programs
- [Universal Parsed Syntax](../guide/universal-syntax.md) — how UPS maps to `theories/CST.v` and `theories/Expander.v`
- [Paper overview](README.md)
