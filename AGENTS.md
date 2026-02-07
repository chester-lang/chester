# Agent Rules

- Chester language rule: `()` is value-only.
- Chester language rule: `Unit` is type-only.
- In Chester code, if anything uses `()` in a type position, or otherwise misaligns with this design, that code is wrong and must be corrected.
- Note (Chester semantics): Types are first-class. `Unit` may appear in contexts that look value-like when it is being used as a type-level value.
