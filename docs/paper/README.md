# Paper

Academic and foundational material for Chester.

This verified compiler does not yet ship a single LaTeX “main paper” in-tree (the
2026 draft kept one under `docs/paper/`). Here we collect:

| Page | Contents |
|------|----------|
| [Literature & References](LITERATURE.md) | ExEl, effects/capabilities, 2LTT, F-ing modules, local PDF pointers in `../../chester-references/` |

Formal definitions that *are* executable live in `theories/*.v` (AST, elaborator,
core checker, backends). Treat those theories as the ground truth for semantics;
the literature page explains which papers informed the design.
