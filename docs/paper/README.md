# Paper

Academic and foundational material for Chester.

This verified compiler does not yet ship a single LaTeX “main paper” in-tree (the
2026 draft kept one under `docs/paper/`). Here we collect:

| Page | Contents |
|------|----------|
| [Literature & References](LITERATURE.md) | ExEl, effects/capabilities, 2LTT, F-ing modules, local PDF pointers in `../../chester-references/` |

## Rocq properties

Executable typing lives in `theories/Elaborator.v` and `theories/CoreChecker.v`
(algorithmic, not a declarative `HasType` judgment). Properties that matter for
module sealing and the core fragment are split across:

| Theory | Contents |
|--------|----------|
| `theories/TypeSystemProps.v` | Effect-row lattice (`effect_row_subsumes_*`), binder/`free_in` lemmas, seal examples, subst capture-avoidance |
| `theories/CoreTyping.v` | Declarative `CoreWT` / `CoreFrag`; `seal_exports_check_names_from_full`; `seal_sigval_requires_effect_subsumption` |
| `theories/RocqIRSem.v` | Rocq IR values/`REval`; logical relation `RValTy`; `emit_rocq_correct` for the core fragment |
| `theories/TypeScriptIRSem.v` | Tiny TS expression model; `emit_ts_lit_correct` for int/bool/ref (arrow/if need statement eval) |

Formal definitions that *are* executable live in `theories/*.v` (AST, elaborator,
core checker, backends). Treat those theories as the ground truth for semantics;
the literature page explains which papers informed the design.
