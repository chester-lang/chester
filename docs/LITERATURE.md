# Chester Literature & References

Foundational reading for Chester's design. This repo implements a verified compiler
(Coq/Rocq theories in `theories/`, extracted OCaml in `extraction/`). The Scala drafts in
sibling directories explored many of the same ideas in a product/tooling stack.

Some papers are behind anti-bot protections or institutional firewalls (ACM Digital Library,
MIT DSpace, etc.) and cannot be fetched automatically. Use the direct links below, or the
local PDF copies in the sibling `chester-references` repository (`../chester-references/`).

## 1. Elaboration & extensibility

- **"ExEl: Building an Elaborator Using Extensible Constraints"** (Bohdan Liesnikov, Jesper Cockx)
  - *Significance:* Modern elaborator framework; maps to Chester's UPS/CST phase (`theories/CST.v`,
    `theories/Expander.v`) and semantic AST (`theories/AST.v`, `theories/Elaborator.v`).
  - [ACM Digital Library (DOI: 10.1145/3652561.3652565)](https://dl.acm.org/doi/pdf/10.1145/3652561.3652565)
  - Local PDF: `../chester-references/3652561.3652565.pdf`

## 2. Propagator networks (historical / exploratory)

The 2025/2026 Scala drafts experimented with propagator-based type inference. The current
verified compiler uses the elaborator in `theories/Elaborator.v` instead. These remain useful
background if incremental or constraint-propagation elaboration returns.

- **"The Art of the Propagator"** (Alexey Radul, Gerald Jay Sussman)
  - *Significance:* Propagator networks for incremental computation; explored in Scala drafts, not the current verified elaborator.
  - [MIT DSpace (Handle: 1721.1/44215)](https://dspace.mit.edu/handle/1721.1/44215)
  - [Direct PDF (requires manual download due to DSpace bot protection)](https://dspace.mit.edu/bitstream/handle/1721.1/44215/MIT-CSAIL-TR-2009-002.pdf)
- **"Propagation Networks: A Flexible and Expressive Substrate for Computation"** (Alexey Radul, Ph.D. thesis)
  - *Significance:* Comprehensive foundation of the propagator model.
  - [MIT DSpace (Handle: 1721.1/49525)](https://dspace.mit.edu/handle/1721.1/49525)
  - [Author mirror](http://web.mit.edu/~axch/www/phd-thesis.pdf)

## 3. Algebraic effects & capabilities (implemented in Chester)

Chester's effect system (`effect` / `handle` / `perform` / `resume`, `box` / `unbox`) follows
the algebraic-effects line of work. See `stdlib/std.chester`, `tests/effects*.chester`, and
`bin/effects_runtime.ml`.

- **"Effect Handlers for Algol-Like Languages"** / handling algebraic effects (Plotkin & Pretnar, 2013)
  - Local PDF: `../chester-references/handling-algebraic-effects-plotkin-pretnar-2013.pdf`
- **"Effekt: Capability-Passing Style for Extensible Algebraic Effects"** (Brachthäuser et al., 2017)
  - Local PDF: `../chester-references/effekt-2017.pdf`
- **"Effects as Capabilities: Effect Handlers and Lightweight Effect Polymorphism"** (Brachthäuser et al., 2020)
  - [ACM PACMPL (DOI: 10.1145/3428194)](https://dl.acm.org/doi/pdf/10.1145/3428194)
  - Local PDF: `../chester-references/Effects-as-Capabilities-2020.pdf`

## 4. Dependent types (partial support)

Chester has basic dependent features (`Type` as a kind, indexed `enum`, `tests/dependent_types.chester`).

- **"Two-Level Type Theory"** (Annenkov, Capriotti, Kraus, Sattler, 2017)
  - Local PDF: `../chester-references/two-level-type-theory-annenkov-2017.pdf`

## See also

- [Universal Parsed Syntax](universal-syntax.md) — how UPS maps to `theories/CST.v` and `theories/Expander.v`
