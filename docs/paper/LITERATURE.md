# Chester Literature & References

Foundational reading for Chester's design. This repo implements a verified compiler
(Coq/Rocq theories in `theories/`, extracted OCaml in `extraction/`). The Scala drafts in
sibling directories explored many of the same ideas in a product/tooling stack.

**Honesty note:** Chester takes inspiration from these papers; it does **not** claim full
literature fidelity. Deviations are called out below and in the guides
([effects](../guide/effects.md), [modules](../guide/modules.md)).

Local PDF copies live in the sibling `chester-references` repository
(`../../chester-references/`). Titles were checked with `pdftotext` on first page. Some
ACM / institutional mirrors block automated download; use the direct links below when a
local copy is missing.

---

## 1. Elaboration & extensibility

- **"ExEl: Building an Elaborator Using Extensible Constraints"** (Liesnikov & Cockx, IFL 2023)
  - *Significance:* Inspiration for Chester's UPS → CST → elaborator pipeline
    (`theories/CST.v`, `theories/Expander.v`, `theories/Elaborator.v`).
  - *Gap:* ExEl is an **open constraint datatype + plugin solvers** (implicits, instances,
    HO unification as modules). Chester uses a fixed recursive `ElabM` walk + unify — no
    constraint store, no solver plugins. Surface implicits (`def f[T]`, `f[A]`) are expander
    sugar, not ExEl-style deferred constraints.
  - [ACM (DOI: 10.1145/3652561.3652565)](https://dl.acm.org/doi/pdf/10.1145/3652561.3652565)
  - Local PDF: `../../chester-references/3652561.3652565.pdf`

## 2. Propagator networks (historical / exploratory)

The 2025/2026 Scala drafts experimented with propagator-based type inference. The current
verified compiler uses `theories/Elaborator.v` instead.

- **"The Art of the Propagator"** (Radul & Sussman, MIT-CSAIL-TR-2009-002)
  - Local PDF: `../../chester-references/The-Art-of-the-Propagator-Radul-Sussman.pdf`
- **"Propagation Networks"** (Radul, Ph.D. thesis)
  - Local PDF: `../../chester-references/Propagation-Networks-Radul-thesis.pdf`

## 3. Algebraic effects & capabilities (partial in Chester)

### What Chester ships (honest subset)

Surface: `effect` / `handle` / `perform` / `resume`, `box` / `unbox`. Runtime:
`bin/effects_runtime.ml`, `runtime/chester-runtime.ts`. Perform encodes `Effect/op` so
dispatch matches **label and op**. Lambdas keep latent effects on the Pi; **direct AppCST**
charges callee FunTy/Pi rows into pending (module-method / self-hosted AppCST paths do
**not** yet — soft spot). Match binders use constructor field types when the ctor is in scope.

| Feature | Chester behavior |
|---------|------------------|
| Deep / shallow | Default **deep**; opt-in `handle shallow` (resume does not reinstall the frame) |
| One-shot / multi-shot | Default **one-shot**; opt-in `handle multishot` via **answer-stream prefix replay** |
| Incomplete handlers | Missing ops **forward** to outer frames; fail only when no outer match |
| Return clause | `case return(x) => …` runs **after** popping the handler frame |
| Annotated rows | `/ {e}` are **closed**; open only via inferred unannotated defs (`with_open_row`) |
| `io` | Ambient top-level / FFI phantom — **not** user-`handle`able |
| `box` | Requires claimed caps on the lexical stack (no empty-evidence silent drop) |

See `docs/guide/effects.md`.

### Gaps vs the literature (sharpened)

**Plotkin & Pretnar — equational handlers.** Handlers are models of an equational theory;
handling is the free-model homomorphism; return clauses are Benton–Kennedy style.
Chester has **no equational theory / handler-correctness check** — any clause list is
accepted; return-after-pop is operational only.

**Kammar, Lindley, Oury — deep vs shallow.** Deep = fold over the computation tree;
shallow = case-split. Chester implements a runtime shallow flag, but has **no shallow
fixtures** and no fold/case semantic story.

**Hillerström et al. / delimited control — true multi-shot.** Real multi-shot needs
delimited continuations (CPS / CEK / stack copy). Chester’s `multishot` **replays a prefix
of the body stream**; side effects before `perform` re-execute. This is **not**
Plotkin/Pretnar or Effekt multi-shot continuations. (One-shot default is closer to
OCaml’s choice for efficiency.)

**Effekt / Effects-as-Capabilities.** EaC semantics = translation to **System Ξ** (explicit
capability-passing); functions are **second-class** for effect safety; polymorphism is
**contextual** (no parametric row variables). Chester uses a lexical capability **stack +
evidence labels**, first-class functions, and soft open rows — not System Ξ, not
capability-as-parameter rewrite, not second-class functions.

**Koka rows (Leijen; TFP’22 open/close).** Closed `⟨exn,div⟩` vs open `⟨exn \| μ⟩` with
principal row unification (duplicate labels matter); named defs get **closed** rows and
instantiation **opens** them. Chester’s `EffectRowVar` is treated as covering every label
in places (`effect_row_has`) — a **vacuous open tail**, not Koka unify. Annotated
`/ {e}` + `with_open_row` is only a sketch of open/close. Ambient `io` / classifier `div`
are Koka-flavored names, not the same system.

**Verified core.** `CoreFrag` / `emit_*_correct` still exclude `AstHandle` / `AstDo` /
`AstBox`; Rocq `__chester_*` are Parameters.

### Primary citations (local PDFs)

- **"Handling Algebraic Effects"** (Plotkin & Pretnar, 2013)
  - Local: `../../chester-references/handling-algebraic-effects-plotkin-pretnar-2013.pdf`
- **"An Introduction to Algebraic Effects and Handlers"** (Pretnar, MFPS 2015 tutorial)
  - <https://www.eff-lang.org/handlers-tutorial.pdf>
  - Local: `../../chester-references/pretnar-handlers-tutorial.pdf`
- **"Handlers in Action"** (Kammar, Lindley, Oury)
  - <http://www.cs.ox.ac.uk/people/ohad.kammar/publications/kammar-lindley-oury-handlers-in-action.pdf>
  - Local: `../../chester-references/handlers-in-action-kammar-lindley-oury.pdf`
- **"Effect handlers via generalised continuations"** (Hillerström, Lindley, et al., JFP 2020)
  - Local: `../../chester-references/effect-handlers-generalised-continuations.pdf`
- **"Effekt: Extensible Algebraic Effects in Scala"** (Brachthäuser & Schuster, Scala’17)
  - Local: `../../chester-references/effekt-2017.pdf`
- **"Effects as Capabilities…"** (Brachthäuser, Schuster, Ostermann, PACMPL 2020)
  - [ACM DOI 10.1145/3428194](https://dl.acm.org/doi/pdf/10.1145/3428194) /
    [EPFL](http://infoscience.epfl.ch/record/288487)
  - Local: `../../chester-references/Effects-as-Capabilities-2020.pdf`
- **"Koka: Programming with Row-polymorphic Effect Types"** (Leijen, 2013)
  - Local: `../../chester-references/koka-row-polymorphic-effects-2013.pdf`
- **"Algebraic Effects for Functional Programming"** (Leijen, MSR-TR 2016)
  - Local: `../../chester-references/algebraic-effects-functional-programming-leijen.pdf`
- **"Sound and Complete Type Inference for Closed Effect Rows"** (Ikemori, Cong, Masuhara, Leijen, TFP 2022)
  - Local: `../../chester-references/koka-closed-effect-rows-tfp2022.pdf`

## 4. Dependent types (citation only / partial surface)

Chester has basic dependent features (`Type` as a kind, polymorphic `enum` field types,
`tests/dependent_types.chester`). Length-indexed enums are **not** implemented
(`tests/vector.chester` is a non-indexed `Vector(T)`).

**Design (rigid type params + prenex instantiate):** Single-letter surface type-param
refs (`T`, `A`, …) are **not** unification wildcards (`is_wildcard_ty` is only explicit
`Any` / `Unknown`). Call sites prenex-instantiate `AstFunTy` type parameters to fresh
metas (`freshen_fun_tps` / `instantiate_funty`). Match binders instantiate the ctor scheme
against the scrutinee. Soft spot: explicit `[Any]` / `[Unknown]`.

**Two-Level Type Theory is citation-only** — no inner/outer level split in the theories.

- **"Two-Level Type Theory"** (Annenkov, Capriotti, Kraus, Sattler, 2017)
  - Local PDF: `../../chester-references/two-level-type-theory-annenkov-2017.pdf`

## 5. Module systems (SML-core sketch in Chester)

Chester supports ML-style structures, signatures, opaque (`:>`) and transparent (`:`) sealing,
generative and applicative (`module app`) functors, first-class `pack`/`unpack`, type components,
`with type` sharing, Rocq `Module`/`Module Type` emit, and file-as-module import via
`--module-path` / `CHESTER_PATH`.

Opaque sealing exports **unsubstituted** signature value types; manifest `type t = …`
equality is checked; nested components are retained under opaque keep when sealed.

**Still out of scope vs F-ing modules:**
- no System Fω packaging; applicative identity is **name-based**, not **stamps** /
  phantom types (F-ing: applicative abstraction safety needs Fω + stamps)
- no higher-order functors / `include` / `open` / `with module` / recursive modules
- sealing is not full Σ≤Ξ; functor application is export rewrite, not re-elaboration under subst
- modules outside `CoreFrag`; self-hosted elaborates module CST as AST skeletons (no
  sealing / `with type` / opaque checks yet)

See `docs/guide/modules.md`.

- **"F-ing modules"** (Rossberg, Russo, Dreyer)
  - [Journal PDF](https://people.mpi-sws.org/~dreyer/papers/f-ing/firstview-journal.pdf)
  - Local PDF: `../../chester-references/f-ing-modules.pdf`
- OCaml module manual — [structures, signatures, functors](https://ocaml.org/manual/5.1/moduleexamples.html)

## 6. Verified fragment (honest scope)

`CoreFrag` / `CoreWT` (`theories/CoreTyping.v`) cover a tiny island: literals, refs, λ,
unary app, if, spans. Backend theorems `emit_{go,ts,rocq}_correct` apply to that fragment
only. Handlers, box, modules, match, multi-arg apps, and most of the elaborator are
**outside** the proved emit relation. Do not read “verified compiler” as verified effects
or verified modules.

## See also

- [Getting Started](../guide/getting-started.md) — build, CLI, and first programs
- [Universal Parsed Syntax](../guide/universal-syntax.md) — UPS → CST → Expander
- [Effects guide](../guide/effects.md) — surface effects semantics
- [Paper overview](README.md)
