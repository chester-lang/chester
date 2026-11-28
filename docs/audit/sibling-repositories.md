# Chester Repository Family: Problem Notes

- **Audit date:** 2026-11-28
- **Repositories inspected:** `../chester`, `../chester-references`, `../chester2025draft`, `../chester2026draft`, `../chester2026draft-a-branch`, and `../chester2026draft2`.
- **Approach:** inventoried tracked/worktree files, read repository guidance and project READMEs, inspected compiler/type-checker and test entry points, and checked repository status and latest local commits. This is a source-level triage, not a claim that every vendored dependency, generated artifact, binary, or every line of the 2025 tree has been audited. Findings below are limited to concrete issues visible in Chester-owned code or explicit project status.
- **Existing audit:** `docs/audit/problems.md` covers the current Rocq-based checkout. These notes describe the earlier reading pass; subsequent development is confined to `/Users/dev/Documents/chester`.

## Confirmed Issues

### HIGH: Core checker does not validate all match cases

- **Repository:** `chester2026draft2`
- **Location:** `chester/src/chester/tyck/CoreTypeChecker.scala:73-75`
- **Problem:** `AST.Match` infers the type of `cases.head.body` only. Remaining branch bodies are not checked, and their inferred types are not compared with the first branch. A malformed or differently typed later branch can therefore pass this checker. Pattern validity/exhaustiveness are also not validated here.
- **Impact:** The core checker cannot serve as a validating second pass for matches; malformed elaborated ASTs can be accepted.
- **Suggested fix:** Check every case body under an environment extended with that case's pattern bindings, unify/check all branch result types, and validate patterns/exhaustiveness in the appropriate checker phase. Add a test with a valid first branch and an invalid later branch.

### HIGH: Core checker skips handler clause validation

- **Repository:** `chester2026draft2`
- **Location:** `chester/src/chester/tyck/CoreTypeChecker.scala:76`
- **Problem:** `AST.Handle` simply infers the handled action, ignoring the handler clauses.
- **Impact:** This checker does not establish that handler clauses are well-typed. Whether the elaborator catches every such case must be tested separately; the core checker itself does not.
- **Suggested fix:** Check handler clause parameter and result types and the resulting effect row. Add negative tests that construct ASTs directly if this checker is intended as an independent validation pass.

### MEDIUM: Prototype Go compiler silently lowers unsupported type syntax to `any`

- **Repository:** `chester2026draft2`
- **Location:** `chester/src/chester/GoCompiler.scala:36-43`
- **Problem:** `lowerType` maps every unrecognized/non-symbol type CST to Go `any` rather than reporting an unsupported type. The compiler can emit compilable code with erased type information.
- **Impact:** Source-level type distinctions can disappear silently in this compiler path.
- **Suggested fix:** Return a structured lowering error for unsupported types, or explicitly support the CST forms and test their generated Go types.

## Project Status / Scope Risks

### The 2025 draft explicitly describes itself as not usable

- **Repository:** `chester2025draft`
- **Location:** `README.md:1-5`
- **Observation:** The README says the project is "under development and not usable yet" and describes Scala compilation resource costs. Treat its examples and feature claims as design/prototype material, not a stable compiler contract.

### Adjacent repositories are distinct generations, not one buildable source tree

- The inspected roots have separate build systems and language implementations: current Rocq/OCaml, Scala-based 2025/2026 trees, and the compact Mill-based `chester2026draft2` tree.
- The `2026draft` and `2026draft-a-branch` checkouts are on different local branches and commits. Their history/worktree content should not be assumed interchangeable; changes in one do not automatically fix the other.
- No build/test suite was run as part of this source-reading pass. In particular, findings are static and runtime behavior should be confirmed with focused regression tests before remediation is considered complete.

## Development Scope

Per the subsequent user instruction, development is confined to
`/Users/dev/Documents/chester`. The draft findings above are historical review
notes, not requests to modify or maintain those repositories. The original
request to read everything was only partially completed by this triage.
