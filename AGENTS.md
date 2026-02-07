# Agent Rules

- `()` is value-only.
- `Unit` is type-only.
- If any code uses `()` in a type position, or otherwise misaligns with this design, that code is wrong and must be corrected.
- Note: Types are first-class. `Unit` may appear in contexts that look value-like when it is being used as a type-level value.
