# Chester — Consolidated Problem Inventory

Full-repository audit: every source file in `theories/`, `self-hosted/`, `bin/`, `runtime/`,
`stdlib/`, `scripts/`, `test/`, `tests/`, `examples/`, `docs/`, `website/`, plus
`flake.nix`, `README.md` and the `../chester-references` PDF corpus (all 14 PDFs verified
readable and matched to their titles).

- **Date:** 2026-11-26
- **Tree audited:** `main` at `3354c52` ("test: cover preserved function parameter types")
- **Method:** line-by-line static reading of each subsystem by independent auditors,
  cross-checked against `AGENTS.md`, the compiler pipeline, and the reference literature.
  Findings are *not* all reproduced end-to-end; line numbers are from the audited tree and
  may drift after later edits. Anything marked "Counterexample" was traced through the
  source path.

## Fix status

The inventory below is a snapshot of the audited tree and is left as written. These
findings have since been fixed (each is covered by a test added with the fix):

| Finding | Fix |
| --- | --- |
| Lexer drops an escaped quote at EOF in an unterminated string | `consume_str` now reports whether it encountered an unescaped closing quote, instead of inferring termination from the final character. The `lexer string boundaries` regression checks recovered and terminated strings, escaped quotes/backslashes, token spans, and following tokens. It failed on the escaped quote at EOF before the fix. |
| File imports discard a final expression-bodied `def` without a trailing semicolon (priority 14) | The file-import loader retains a `DefCST` in the parsed block tail. The `modules_import` regression calls the final expression-bodied `Math.mul` and checks that its declaration is emitted. |
| **C1** — `!=` rewritten to `int_eq` in `self-hosted/expander.chester`; `==`/`!=` missing from `op_to_fn` in `theories/Expander.v` | Both expanders map `==` → `int_eq`, `!=` → `int_ne`; `int_ne` added to both runtimes, the Go/TS emitters, the preamble whitelists and both stdlibs. Parity fixture `tests/infix_ne.chester` (returns 7 only if `!=` means inequality). |
| Lexer splits `<=`, `>=`, `!=` into single-character symbols, and mis-slices strings ending at EOF (`bin/lexer.ml`) | Multi-character operators tokenised as one symbol. |
| `if` whose condition is an operator expression (`if a < 2 then …`), or a parenthesised condition, is rejected with `Unbound variable: if` / `If condition is not Bool` | `expand_if` in both expanders now gathers the condition, then-branch and else-branch token runs (nesting them as `SeqOf` so infix parsing runs on them); conditions unwrap the one-element `Tuple` that a parenthesised group produces. |
| `if` bound by `let`/`var` stays unexpanded in the Rocq pipeline (`process_stmts` built the node after the statement map) | `theories/Expander.v` re-expands the bound value, matching the self-hosted expander. |
| `get_relation` (`self-hosted/expander.chester`) returns `RelSameL`/`RelSameR` without their `Integer` argument, so stage 1 panics (`interface conversion`) on any chained same-precedence infix such as `a + b + c` | Constructors applied with `0`. |
| Rocq preamble defines no comparison primitives (`bin/effects_runtime.ml`), so `--rocq` output refers to unbound `int_lt`/`int_le`/`int_gt`/`int_ge`/`int_eq`/`int_ne` | Added as `nat -> nat -> bool` definitions; `tests/rocq_compare.chester` is compiled with `coqc` in `dune runtest`. |

Still open from the entries below: `int_add a b := a`, the remaining missing Rocq
preamble primitives (`int_sub`, `int_mul`, `string_*`, `bool_*`, …), and the
`chester_dyn` conditional typing that keeps `--rocq` output from computing. The
self-hosted Go backend also returns `nil` for an `if` nested directly in another
`if`'s branch (both still parse and elaborate), so that form is deliberately left
out of the parity fixture. A parenthesised branch such as `(if c then 1 else 2)`
still fails: the parser gives it the same one-element `Tuple` shape as a tuple
literal, which the expander cannot tell apart.

Validation for the trailing-import fix (2026-11-28): reproduced the original
`module has no export: mul` error before the change; the rebuilt CLI and the
focused `fixture modules import` expect test pass afterward. `dune build` passes
with the installed OCaml toolchain. The escaped-quote-at-EOF fix was checked the
same way: for the unterminated input `"abc\"` the lexer now returns the body
`abc\"`, where the old code inferred termination from the final byte and dropped
the escaped quote. The rebuilt lexer was exercised directly on the inputs of the
`lexer string boundaries` test, and every body, span and following token matched
the test's expectations. Building with the pinned Nix dev shell was attempted
again and was still loading the environment after several minutes, so these
checks use the standalone dune/OCaml toolchain from the Nix store. The full
`dune runtest` run fails in multiple other areas. Its local extracted `extraction/Compiler.ml` is stale (for example,
it lacks `int_ne`, which is present in `theories/Expander.v`); regenerate extraction
with the documented Rocq build before treating that run as a current-core baseline.
The review rerun also passes the focused test and executes the emitted import
fixture successfully with Bun. A fresh Nix build was attempted but stopped while
loading the pinned environment after several minutes without progress; the
installed standalone Rocq cannot rebuild the core because `Stdlib` is missing.

## Severity legend

| Level | Meaning |
| --- | --- |
| **CRITICAL** | Type-soundness hole, silent miscompilation, or a compiler/runtime crash reachable from ordinary source. Must be fixed before the compiler can be trusted on untrusted input. |
| **HIGH** | Rejects valid programs, emits code that fails to build, loses declarations, or makes a documented feature unusable. |
| **MEDIUM** | Incorrect docs/diagnostics, portability or hygiene defect, or a gap that will bite as the language grows. |
| **LOW** | Dead code, cosmetic drift, unused dependency, polish. |

## Executive summary

The audit found roughly **180 distinct defects**, concentrated in five root causes:

1. **The type checker has a universal escape hatch.** `is_allowed_unbound` types every
   capitalized identifier, `true`/`false`, every `prim__*`/`__*` name and every builtin
   operator as `Any`, and `Any` unifies with everything — in both `Elaborator.v` and
   `CoreChecker.v`. This alone makes `def f(): String = 1 + "x"`, unbound identifiers,
   and any mistyped operator call typecheck, then panic or fail `go build`.
2. **Effects are not actually tracked.** Effect rows are compared *symmetrically* in both
   `Elaborator.unify` and `CoreChecker.types_compat`, so any impure row subsumes `[]`:
   pure higher-order parameters accept divergent/effectful callbacks. Lambdas never unify
   their effect row or arrow type against `expected`. Cross-file collection in
   `bin/main.ml` rewrites every imported/prelude `AstDef` effect row to `[]`.
3. **The verified core is disconnected from the compiler that ships.** `CoreWT` in
   `CoreTyping.v` is a 7-constructor judgment with no link to `Elaborator`/`CoreChecker`
   (and it disagrees with `CoreChecker` on lambdas). The Go/TS/Rocq "correctness"
   theorems quantify over an idealized `GoEval`/`TSEval` that pattern-matches on backend
   helper functions, and prove properties of programs whose emitted text is a syntax
   error. `flake.nix` installs the self-hosted Stage 2 (which has no checker at all) as
   `$out/bin/chester`.
4. **The self-hosted compiler is a prototype, not a port.** `unify` returns `Unit` and
   cannot fail; there is no `CoreChecker`; every `def` is bound in the environment to its
   *return type* instead of its arrow type; `!=` compiles to `int_eq`; the Pratt parser
   reduces `1 + 2 * 3` to `9`; and Stage 1/Stage 2 string escaping diverges so Stage 2
   cannot bootstrap Stage 3.
5. **Surface, runtime and docs drift from reality.** `bin/lexer.ml` splits `<=`/`>=` into
   two tokens; the TS/Go effect runtimes replay handler bodies from scratch on `resume`
   (duplicating side effects and returning wrong values); the TS backend emits `args[0]`
   while enums emit `_f0`; and the website advertises `Type 0`/`Type 1` universes and
   hygienic macros that do not exist.

### Highest-priority defects (fix in this order)

| # | Defect | Where |
| --- | --- | --- |
| 1 | `Any` wildcard for unbound capitalized names, literals and all primitives | `Elaborator.v:1477`, `CoreChecker.v:581-620` |
| 2 | Symmetric effect-row subsumption collapses every row into `{}` | `Elaborator.v:625-641`, `CoreChecker.v:1040-1044` |
| 3 | Immediately-applied lambda skips body checking (bypasses `resume`-outside-handler) | `CoreChecker.v:1360-1365, 1417-1418` |
| 4 | `AssignCST`/`AstAssign` never checks existence, mutability or type | `Elaborator.v:1633-1636`, `CoreChecker.v:1710-1714` |
| 5 | Opaque sealing and abstract type identity unsound across modules | `Elaborator.v:1230, 3256-3260`, `CoreChecker.v:1062` |
| 6 | `Handler` runtime replays `bodyFn()` on every `resume` (wrong values, duplicated effects) | `bin/effects_runtime.ml:47-111, 440-550` |
| 7 | Self-hosted `unify` cannot fail; no `CoreChecker`; `def` bound to return type | `self-hosted/elaborator.chester:908-1008, 2367-2372` |
| 8 | Self-hosted `!=` → `int_eq`; Pratt precedence dead code (`1 + 2 * 3 == 9`) | `self-hosted/expander.chester:950-976, 1309` |
| 9 | `bin/lexer.ml` does not tokenize `<=`, `>=`, `!=`; crashes/truncates unterminated strings | `bin/lexer.ml:65-102` |
| 10 | TS enum emits `_f0`, TS match reads `args[0]` → runtime `TypeError` | `theories/Backend.v:550, 618-633, 716, 757` |
| 11 | `string_of_nat` maps every `n >= 10` to `"X"` (functors/hygiene collide) | `Elaborator.v:12-16` |
| 12 | Rocq preamble defines `int_add a b := a` and omits most primitives | `bin/effects_runtime.ml:649-677` |
| 13 | Go raw backtick strings break escape parity and reject backticks | `GoAST.v:110`, `lexer.ml:66-80` |
| 14 | File imports drop a trailing declaration whose body is not a block (`def f(): T = e`) | `bin/main.ml:158-163`, `Parser.v:276-300` |
| 15 | Stage 1/Stage 2 escape divergence prevents Stage 3 bootstrap | `self-hosted/codegen_go.chester:485-497` |
| 16 | Website/docs claim `Type 0`/`Type 1`, hygienic macros, and verified soundness | `website/index.html`, `README.md`, `docs/` |

### How this document is organised

Sections 1–5 are the five subsystem audits, preserved in full (deduplicated only where the
same defect appears twice under different names; cross-references are called out inline).
Section 6 maps the duplicated root causes and proposes an ordered remediation plan.

1. [Rocq frontend & elaborator (`theories/`)](#1-rocq-frontend--elaborator-theories)
2. [Core checker, proofs, backends & extraction (`theories/`)](#2-core-checker-proofs-backends--extraction-theories)
3. [Self-hosted compiler (`self-hosted/`)](#3-self-hosted-compiler-self-hosted)
4. [CLI, runtimes, stdlib, scripts, tests & examples](#4-cli-runtimes-stdlib-scripts-tests--examples)
5. [Documentation, website, grammar & build](#5-documentation-website-grammar--build)
6. [Cross-cutting root causes & remediation plan](#6-cross-cutting-root-causes--remediation-plan)

---

## 1. Rocq frontend & elaborator (`theories/`)

All 7 files (`CST.v`, `Tokenizer.v`, `Parser.v`, `Expander.v`, `Formatter.v`, `AST.v`, `Elaborator.v`) and `AGENTS.md` (plus `CoreChecker.v` / `Extract.v` where directly referenced) have been audited line-by-line. Below is the complete, categorized inventory of bugs, soundness holes, fuel/measure flaws, parser/expander/formatter defects, and ad-hoc workarounds.

---

#### Category 1: Type, Effect, and Module System Soundness Holes (Critical)

##### 1.1 Polymorphic Call Inside Block Drops Expected Type + CoreChecker `Any` Erasure → Arbitrary Type Coercion (`Int` as `String`)
- **Location**: `theories/Elaborator.v:1601, 1731-1732`, `theories/Elaborator.v:1808-1840` (interacting with `theories/CoreChecker.v:648-660, 1425-1452`)
- **Actual vs Expected**:
  - In `elaborate` on `Block stmts ret_expr _` (`Elaborator.v:1731`), `expected` is **dropped** (`retAst <- elaborate fuel' final_env ret_expr None`) and `snd retAst` is **never unified** with `expected`.
  - `DefCST` (`Elaborator.v:1808-1840`) passes `Some (fst retAst)` as `expected` when elaborating `body`, assuming the recursive call enforced it, and ignores `snd bodyAst`.
  - In `elaborate_top`, `CoreChecker.infer_check` instantiates all `AstFunTy` type parameters with `AnyType` (`core_subst_tps`, `CoreChecker.v:651-660`), so polymorphic calls return `Any` (or `Option(Any)`), which passes `meet_expected` against any concrete type.
- **Concrete Counterexample**:
  ```chester
  def id[T](x: T): T = x;
  def unsound(): String = {
    id(42)
  }
  def unsound_opt(): Option(String) = {
    Some(42)
  }
  ```
  Both pass `Expander`, `Elaborator`, and `CoreChecker` with zero errors, returning an `Int` (`42`) typed as `String` (and `Option(Int)` typed as `Option(String)`). Dropping `expected` in `Block` also breaks bidirectional checking of lambdas (`{ \x => x }`) and module type components (`def t(): Type = { M.t }` throws `"type component used as value"`).
- **Severity**: **Critical**

##### 1.2 Unbound Capitalized Identifiers, `true`/`false`, and All Operators Elaborate to Wildcard `Any`
- **Location**: `theories/Elaborator.v:1476-1480`, `theories/Parser.v:172-176`, `theories/Expander.v:50-60` (with `theories/CoreChecker.v:581-620`)
- **Actual vs Expected**:
  - `Parser.v` never constructs `BoolLiteral` (dead constructor in `ParserCST` at `CST.v:230`); `true` and `false` parse as `Symbol "true"` / `Symbol "false"`.
  - In `Elaborator.v:1477`, `if is_allowed_unbound name then ret (AstRef name, AstRef "Any")` fires **before** `is_type_expected` and **without unifying against `expected`**.
  - `is_allowed_unbound` returns `true` for **every capitalized identifier** (`name_is_upper`), `"true"`, `"false"`, and all arithmetic/comparison primitives (`"int_add"`, `"int_sub"`, `"int_lt"`, etc.).
  - Consequently:
    1. Any typo'd or completely undeclared capitalized identifier in value position (`BogusIdentifier`) typechecks as `Any` (unifying with `Int`, `String`, functions, etc.).
    2. `true` and `false` have static type `Any` instead of `Bool`, so `let x: Int = true` and `let s: String = false` typecheck!
    3. All built-in operators `+`, `-`, `*`, `/`, `%`, `<`, `>`, `<=`, `>=` rewrite to `int_add` etc., which resolve via `is_allowed_unbound` to `Any`, so `"hello" - true` typechecks with return type `Any`!
- **Concrete Counterexample**:
  ```chester
  def main(): Int = CompletelyUndefinedCapitalizedName;
  def bad_bool(): String = false;
  def bad_arith(): Bool = "hello" * true;
  ```
- **Severity**: **Critical**

##### 1.3 List Literals Ignore Element Types and Unify With Any `List(T)`
- **Location**: `theories/Elaborator.v:2394-2405`, `theories/Elaborator.v:673` (via `theories/CoreChecker.v:1057-1058`)
- **Actual vs Expected**: `ListLiteral` elaborates every element with `expected = None`, never unifies element types with each other or with a target element type, and returns unparameterized `AstRef "List"`. `unify` delegates to `types_compat`, which equates bare `AstRef "List"` with `AstApp (AstRef "List") [T]` for **any** `T`.
- **Concrete Counterexample**:
  ```chester
  let xs: List(Int) = ["not an int", true, 42];
  ```
  Typechecks cleanly in both `Elaborator` and `CoreChecker`.
- **Severity**: **Critical**

##### 1.4 `AstTuple` Overloaded for Both Tuples and Lists → Empty List `[]` and Non-Empty Tuples `(1, 2)` Fail or Mismatch in `CoreChecker`
- **Location**: `theories/AST.v:107`, `theories/Elaborator.v:2380-2406` (interacting with `theories/CoreChecker.v:1293-1315`)
- **Actual vs Expected**: `AST` has no `AstList` node. `Elaborator` compiles `(1, 2)` to `(AstTuple [1; 2], AstRef "Tuple")` and `[1, 2]` to `(AstTuple [1; 2], AstRef "List")`; `()` and `[]` both become `AstTuple []`. `CoreChecker` then types `AstTuple []` unconditionally as `UnitType` and non-empty `AstTuple (_ :: _)` unconditionally as `AstRef "List"`.
- **Concrete Counterexample**:
  - `def empty_ints(): List(Int) = []` → `Elaborator` succeeds, `CoreChecker` rejects with `"Core check: type mismatch against expected"` (`Unit` vs `List(Int)`).
  - `def pair(): Tuple = (1, 2)` → `CoreChecker` types `(1, 2)` as `List` and rejects against `Tuple`.
  - Codegen cannot distinguish `[]` from `()` or `[1, 2]` from `(1, 2)`.
- **Severity**: **Critical**

##### 1.5 Symmetric Effect Row Subsumption (`orb`) Breaks Effect Safety on Higher-Order Functions + Lambdas Skip Effect/Return Unification
- **Location**: `theories/AST.v:87-93`, `theories/Elaborator.v:625-628, 639-641, 1884-1887`
- **Actual vs Expected**:
  - `unify` checks `orb (effect_row_subsumes eff1 eff2) (effect_row_subsumes eff2 eff1)`. Because `effect_row_subsumes [] [Boom]` is `true`, unifying an expected **pure** function type `Int -> Int / {}` with an **impure** function `Int -> Int / {Boom}` succeeds!
  - Additionally, `LamCST` (`Elaborator.v:1884-1887`) **never unifies** its synthesized arrow type `arrTy` or `open_effs` against `expected` at all (comment explicitly admits skipping unification).
- **Concrete Counterexample**:
  ```chester
  effect Boom { def boom(): Int }
  def runPure(f: (x: Int) -> Int): Int = f(0);
  def explode(x: Int): Int / {Boom} = perform boom();
  def main(): Int = runPure(explode); // or runPure(\x => perform boom())
  ```
  Executes unhandled `Boom` inside pure `main` with zero compile-time errors.
- **Severity**: **Critical**

##### 1.6 Opaque Module Sealing (`:>`) and `unpack` Lack Generative Type Identity & Skolem Escape Checks
- **Location**: `theories/Elaborator.v:668-670, 1229-1232, 2739-2781, 3257-3260`
- **Actual vs Expected**:
  1. Under opaque sealing `module M :> S`, abstract `type t` exports value members mentioning bare `AstRef "t"`, and `FieldAccessCST` in type position (`Elaborator.v:3260`) elaborates `M.t` to bare `AstRef "t"` (erasing `M`!). Worse, `unify` (`lines 668-670`) equates `AstFieldAccess _ f` with `AstRef f` regardless of module receiver!
  2. `UnpackCST` (`lines 2739-2781`) performs **no existential skolem escape check** on `snd bodyAst` (and `lines 2776-2780` silently bind `x` to `AstModTy []` if `sig` is malformed).
- **Concrete Counterexample**:
  ```chester
  signature Counter { type t; def zero(): t; def get(x: t): Int }
  module IntC :> Counter { type t = Int; def zero(): Int = 0; def get(x: Int): Int = x }
  module StrC :> Counter { type t = String; def zero(): String = "bad"; def get(x: String): Int = 1 }
  def crash1(): Int = IntC.get(StrC.zero()); // Both have type AstRef "t"!
  def crash2(): Int = {
    let leaked = unpack (X : Counter) = (pack StrC as Counter) in X.zero();
    unpack (Y : Counter) = (pack IntC as Counter) in Y.get(leaked)
  };
  ```
  Both pass a runtime `String` (`"bad"`) into `IntC.get(x: Int)` with zero type errors.
- **Severity**: **Critical**

##### 1.7 Mutable Assignment (`AssignCST`), `IfCST`, and `ExternCST` Completely Bypass Type Checking
- **Location**:
  - `AssignCST`: `theories/Elaborator.v:1633-1636, 1744-1746` (and `CoreChecker.v:1710-1714`)
  - `IfCST`: `theories/Elaborator.v:1778-1782`
  - `ExternCST`: `theories/Elaborator.v:908-921, 3000-3002`
- **Actual vs Expected**:
  1. `AssignCST` **never looks up `name` in `env`**: assigning to an undeclared variable (`undeclared = 1`), mutating an immutable `let` binding or function parameter (`let x = 1; x = 2`), and assigning an incompatible type (`var x = 1; x = "hello"`) all succeed without error!
  2. `IfCST` in `Elaborator` never unifies `cond` with `BoolType` and never unifies `snd thenAst` with `snd elseAst` when `expected = None` (`let x = if 123 then 42 else "str"`).
  3. `elab_extern_decls` (`Elaborator.v:913-914`) parses `extern ts "mod" { def f(x: Int): String; }` and **discards `_params` and `_ret_ty_cst`**, binding every extern declaration to `AstRef "Any"`! Furthermore, standalone `ExternCST` outside `Block` (`line 3001`) discards the updated `TypeEnv` completely (`let (syms, _) := ...`).
- **Severity**: **Critical**

##### 1.8 Explicit Type Arguments on Polymorphic Functions `f[String](42)` Are Silently Ignored
- **Location**: `theories/Elaborator.v:2009-2073, 2214-2246`
- **Actual vs Expected**:
  - In `AppCST (ImplicitAppCST inner_func targs)` (`lines 2024-2032`), `checked_ty` only uses `raw_ty` when `inner_func` starts with an uppercase letter (`ctor_callee`). For all normal functions (`def id[T](x: T): T`), `inner_func` was already prenex-instantiated at line 2012 (`tps = []`), so arity checking is bypassed (`id[Int, Bool, String](42)` succeeds!) and `targsAst` substitution is skipped (`id[String](42)` succeeds with type `Int`!).
  - For standalone `ImplicitAppCST` (`lines 2214-2246`, e.g., `None[Int]` or `let f = id[Int]`), value-position elaboration returns `AstApp (fst funcAst) argsRes` — typing `None[Int]` as `None(Int)` (failing against `Option(Int)`) and `id[Int]` as `id(Int)` instead of `Int -> Int`!
- **Severity**: **High**

##### 1.9 Forward-Referenced Functions Erase Types & Effects; Partial Application Erases Effects; Unannotated Helpers Leak Effects
- **Location**: `theories/Elaborator.v:1603-1617, 1820-1826, 2096, 2166, 2198-2206`
- **Actual vs Expected**:
  1. **Forward-ref erasure (`lines 1606-1607`)**: `Block` pre-binds every `DefCST` to `AstRef "Any"`. Calling a function defined later in the same block skips argument/arity checking and records `[]` effects — allowing unhandled effects (`def caller(): Unit = callee(); def callee(): Unit / {Boom} = perform boom(); def main(): Unit = caller()`).
  2. **Partial application effect erasure (`lines 2096, 2166, 2206`)**: Partially applying `def f(x: Int, y: Int): Int / {Boom}` as `let g = f(1)` gives `g` type `AstFunTy [] [("y", Int)] Int []` (**pure `[]` effects!**) while charging `Boom` prematurely at `f(1)`.
  3. **Unannotated `def` effect leak (`line 1823`)**: Unannotated `def helper(): Int = perform ask()` merges `body_effs` (`[Ask]`) into top-level `old_pending` **at definition time**, so `def main(): Int = handle { helper() } with Ask { case ask() => resume(1) }` fails at `elaborate_top` with `"Unhandled effect: Ask"` even though every call to `helper` is handled!
- **Severity**: **High**

##### 1.10 `BoxCST` / `UnboxCST` Type Disagreement Between `Elaborator` and `CoreChecker` + Unchecked `unbox`
- **Location**: `theories/Elaborator.v:1748-1776` (vs `theories/CoreChecker.v:1872-1884`)
- **Actual vs Expected**:
  - `Elaborator` types `box e` as `AstPi "x" Unit T []` (`Unit -> T`), whereas `CoreChecker` types `AstBox e _` as `T` (not `Unit -> T`) and `AstUnbox e` as the inner expression's type (`Unit -> T` instead of `T`)! Thus any typed function returning or accepting a boxed computation (`def mk(): (x: Unit) -> Int = ... box { 1 }`) is rejected by `CoreChecker`.
  - Meanwhile, `unbox 42` elaborates to `Int` with zero error in both `Elaborator` and `CoreChecker`, and `unbox (\s: String => s)` ignores the parameter type `String` and types as `String`.
- **Severity**: **High**

##### 1.11 Parameterized Effects (`EffectCST`) Use a Single Global Mutable Meta Variable
- **Location**: `theories/Elaborator.v:3006-3043, 3069`
- **Actual vs Expected**: `EffectCST` (`lines 3009-3016`) binds each type parameter of `effect State(T)` to a **single global `fresh_meta`** once at declaration time. The first `handle` or `perform` in the program unifies `T := Int`, permanently locking `State` to `Int` and rejecting a second `State` handler with `String`! Additionally, `find_registered_op` (`line 3069`) picks the most recently declared effect when two effects share an operation name (`get`).
- **Severity**: **High**

---

#### Category 2: Termination, Positivity, Universe Consistency, and Dependent Types (Critical / High)

##### 2.1 Logical & Total-Fragment Inconsistency: `Type : Type`, Missing Strict Positivity, Mutual Recursion, and Signed `Int` Subtraction
- **Location**: `theories/Elaborator.v:1478, 1812-1834, 2248-2308, 3240, 3298` (with `theories/CoreChecker.v:1071, 1106-1236, 1290`)
- **Actual vs Expected**:
  1. **No strict positivity check on `enum` (`Elaborator.v:2248-2308`)**: Negative occurrences allow non-termination without recursion or `div`:
     ```chester
     enum Bad { case Mk(f: (x: Bad) -> Int) }
     def omega(b: Bad): Int = match b { case Mk(f) => f(b) };
     def loop(): Int = omega(Mk(omega)); // Pure / {}, non-recursive, diverges!
     ```
  2. **Signed `Int` subtraction accepted without lower-bound guard (`CoreChecker.v:1114-1116`)**:
     ```chester
     def diverge(n: Int): Int = diverge(n - 1); // Certified total (no `div` required)!
     ```
  3. **Mutual recursion ignored (`Elaborator.v:1813`, `CoreChecker.v:1235`)**: `def a(x: Int): Int = b(x); def b(x: Int): Int = a(x)` has `free_in "a" (b(x)) = false`, so both are certified total without `div`.
  4. **Shadowing/mutation of `smaller` names (`CoreChecker.v:1140-1153, 1202`)**: Inside `case Cons(h, t) => { let t = xs; loop(t) }`, `"t"` remains in `smaller` after `let t = xs` (or `t = xs`), certifying `loop(xs)` as structurally decreasing.
  5. **Universe collapse (`Type : Type`)**: `is_universe_ty` equates `AstRef "Type"` with `AstUniverse l` for all `l` (`CoreChecker.v:621-627, 1071`), collapsing `AstUniverse 0 == Type == AstUniverse 1`.
- **Severity**: **Critical**

##### 2.2 Dependent Types Misrepresentation: Dependent `DefCST` Scopes Broken, Value Indices Rejected as Types, and `unify` Rejects Alpha-Equivalent Dependent Pis
- **Location**: `theories/Elaborator.v:622-641, 1785-1806, 1891-1903`
- **Actual vs Expected**:
  1. **No telescope scope in `DefCST` (`lines 1785-1802`)**: `map_params` and `ret_cst` elaborate in outer `env` without binding earlier parameters (or even `type_params`!). `def my_id(T: Type, x: T): T` only accidentally worked because `"T"` is uppercase (`is_allowed_unbound`) and `is_type_expected` accepts any unbound symbol!
  2. **Type family value arguments rejected (`lines 1891-1903`)**: In `AppCST` when `is_type_expected` is true, **every argument** is elaborated with `expected = Some TypeUniverse`. Thus `Vec(0)` unifies `IntType` with `TypeUniverse` and fails with `"Type mismatch"`, and `def f(n: Int, v: Vec(n)): Vec(n)` fails in `unknown_ty_in_program` with `"Unknown type: n"`!
  3. **No alpha-equivalence in `unify` (`lines 622-641`)**: Unifying `(x: Type) -> x` with `(y: Type) -> y` compares `AstRef "x"` with `AstRef "y"` without binder renaming and throws `"Type mismatch"`.
- **Severity**: **High**

---

#### Category 3: Unification, Meta-Solver, Fuel Bounds, and `AGENTS.md` Violations (Critical / High)

##### 3.1 `zonk` and `expand_metas` Compute Fuel From Unexpanded `AstMeta` Size (`ast_size = 1`) → Residual Unsolved Metas & Occurs-Check Bypass
- **Location**: `theories/Elaborator.v:519-556, 678-865, 1861, 2124, 2197, 2340, 3351, 3352`
- **Actual vs Expected**:
  - `zonk (S (ast_size expr)) expr` computes fuel from `expr` **before** meta substitution! If `expr` is `AstMeta 0` (`ast_size = 1`, so `fuel = 2`), and `?0 := Option(Option(?1))` with `?1 := Int`, `zonk 2 (AstMeta 0)` exhausts fuel at depth 2 and returns `Option(Option(AstMeta 1))` with unsolved metas left in the AST/type!
  - `assign_meta` (`line 551`) calls `expand_metas (S (ast_size t)) sol t` AND `expand_metas` (`lines 519-545`) omits `AstImplicitApp`, `AstFieldAccess`, `AstModTy`, `AstTypeDecl`, `AstSigWith`, etc. (`| other => other`). A chain of $\ge 3$ metas `?1 -> ?2 -> ?3 -> ?0` or a meta inside `AstImplicitApp` / `AstModTy` is **not** expanded before `occurs_meta`, creating cyclic meta graphs!
- **Severity**: **Critical**

##### 3.2 `string_of_nat` Hardcodes `"X"` for All Numbers $\ge 10$ ("AI Slop") → Functor, Hygiene, and Row-Var Collisions
- **Location**: `theories/Elaborator.v:12-16` (used at lines `47`, `252`, `2962`)
- **Actual vs Expected**:
  ```coq
  Fixpoint string_of_nat (n : nat) : string :=
      match n with
      | 0 => "0" | ... | 9 => "9" | _ => "X"
      end.
  ```
  Declared `Fixpoint` with **no recursive call**! Once `ElabState`'s shared counter `n >= 10`:
  - Every generative functor application `module A = F(M1); module B = F(M2)` gets the **identical** generated name `F_gX` (`line 2962`), colliding generative functor instances!
  - Every hygiene scope ID $\ge 10$ mangles to `"_X"` (`line 47`).
  - Every fresh row variable $\ge 10$ is named `"μX"` (`line 252`).
  - Also `string_to_nat_aux` (`lines 26-34`) silently truncates `"0x10"` to `0` and `"1_000"` to `1`.
- **Severity**: **Critical**

##### 3.3 Exponential Re-Elaboration on Chained Extension Calls Exhausts `cst_fuel`; Macro Fuel Silently Truncates; `Formatter` Uses Caller Fuel
- **Location**:
  - `theories/CST.v:223` (`cst_fuel`)
  - `theories/Elaborator.v:1983-1984, 2004-2005, 3350`
  - `theories/Expander.v:1600-1602, 1679`
  - `theories/Formatter.v:207-209, 453-455, 486`
- **Actual vs Expected**:
  1. **Exponential extension call blowup**: `x.m1().m2()...mK()` elaborates `expr` at `line 1907`, then constructs `AppCST (Symbol full_name) (expr :: args)` and calls `elaborate fuel'` on `expr` **again** (`line 1984`), doing $2^K$ work! With `cst_fuel = 8 * (cst_size + 1)`, just ~5 chained extension calls exhaust fuel and fail with `"Out of fuel"`.
  2. **Macro fuel silent fallback**: `apply_macros` (`Expander.v:1679`) uses `cst_fuel` even though macros duplicate arguments; on exhaustion (`fuel = 0`), line `1602` **silently returns unexpanded macro calls** instead of erroring.
  3. **`Formatter.v` violates `AGENTS.md:64-70`**: `format_cst` and `format_program` take raw `fuel : nat` from the caller instead of structural recursion or `cst_fuel`, emitting `"/* ERROR: formatter out of fuel */"` into formatted source text.
- **Severity**: **High**

##### 3.4 Direct `AGENTS.md` Language Semantics & Delimiter Violations
- **Location**:
  - `()` in type position: `theories/Elaborator.v:2383-2392`, `theories/Expander.v:302` (and `CoreChecker.v:702, 810`)
  - `[]` for type application: `theories/Elaborator.v:2243-2244`
  - `record` delimiters: `theories/Expander.v:1176-1192`
- **Actual vs Expected**:
  - `AGENTS.md:12`: *"In Chester code, if anything uses `()` in a type position, that code is wrong and must be corrected."* Yet `def f(x: ()): () = ()` is **accepted** by `Elaborator` (`Tuple []` elaborates to `(AstTuple [], AstRef "Unit")` even when `is_type_expected` is true) and `expand_arrow_type` synthesizes `Tuple []` for empty `rhs`!
  - `AGENTS.md:24-27`: `[]` is for list literals and implicit value apps only, `()` for type apps; yet `ImplicitAppCST` (`Elaborator.v:2243-2244`) still accepts `Option[Int]` in type positions!
  - `Expander.v:1180-1188` requires parentheses `record R(x: Int)` / `record R(T)(x: T)` (despite its own comment saying `record R(T) { ... }`), leaving brace-delimited `record R { x: Int }` unexpanded!
- **Severity**: **High**

##### 3.5 Dead / Abandoned Effect Constraint Solver in `AST.v`
- **Location**: `theories/AST.v:19-27, 167-211`
- **Actual vs Expected**: `effect_metas`, `Constrained`, `update_effect_state`, `add_effect_constraint`, and `finalize_meta` are completely dead code (never called anywhere). `add_effect_constraint` (`lines 199-202`) silently drops constraints on `Solved` metas, and `SolverState` stores meta maps as linear function-closure chains (`O(updates)` per lookup).
- **Severity**: **Low**

---

#### Category 4: Tokenizer, Parser, Expander, and Hygiene Bugs (Critical / High)

##### 4.1 `Tokenizer.v` Is a Fake 17-Line Stub That Drops Everything Except `{` and `}`
- **Location**: `theories/Tokenizer.v:1-17`
- **Actual vs Expected**: Extracted to `Compiler.ml` (`Extract.v:29`), `tokenize` **only** emits `TokSym "{" empty_span` and `TokSym "}" empty_span`, never updates `current_pos`, and silently discards all identifiers, literals, operators, `()`, `[]`, `;`, and comments! `tokenize "def main(): Unit = { 42 }" zero_pos` produces `[TokSym "{"; TokSym "}"]`. Also `_CoqProject:10-11` lists `Tokenizer.v` before `Parser.v` despite `Tokenizer.v:8` importing `Chester.Parser`.
- **Severity**: **High**

##### 4.2 Bottom-Up Single-Pass `Expander` Leaves Unexpanded `SeqOf` Inside Lambdas, `match` Arms, `handle` Clauses, `box`/`unbox`, and Curried `->` Types
- **Location**: `theories/Expander.v:302, 320-393, 397-406, 1272-1290`, `theories/Elaborator.v:380, 383, 1496`
- **Actual vs Expected**:
  - `expand_cst` expands children bottom-up **before** stripping syntax prefixes (`\x =>`, `case pat =>`, `box`, `unbox`, `->`). When `expand_seq_expr` strips `\x =>` or `extract_cases` strips `case pat =>`, it wraps the remaining tokens in a fresh `SeqOf` **and never expands that `SeqOf` again** (`expand_cst` on `MatchCST` at `lines 1288-1290` doesn't even traverse `cases`, directly contradicting the comment at `line 325`!).
  - Same bug in `Elaborator.v:380, 383` (`parse_handler_case` wraps handler arm bodies in raw `SeqOf`).
  - Also notice `Elaborator.v:1496` still contains debug slop `if string_dec name "\" then throw "Lambda!"`!
- **Concrete Counterexamples**:
  - `map(xs, \x => x + 1)` → body of lambda stays `SeqOf [x; +; 1]`; `Elaborator` tries to call `x` as a function on `(+, 1)` → **`"cannot apply non-function"`**!
  - `\x => \y => x` → **`"Lambda!"`**!
  - `match opt { case Some(x) => x + 1; case None => 0 }` → **`"cannot apply non-function"`**!
  - `handle { ... } with Eff { case op() => 1 + 2 }` → **`"cannot apply non-function"`**!
  - `type F = Int -> Int -> Int` → inner `Int -> Int` stays unexpanded `SeqOf`!
- **Severity**: **Critical**

##### 4.3 `case None => ...` (Nullary Enum Constructors in `match`) Misparsed as Irrefutable Variable Pattern `PatVar "None"`!
- **Location**: `theories/Expander.v:378-390` (interacting with `Elaborator.v:2318-2321` and `CoreChecker.v:972-981`)
- **Actual vs Expected**: In `extract_cases`, `case None => 0` (any bare nullary constructor without `()` or `Enum.` prefix) matches `Symbol "case" :: Symbol vname :: Symbol "=>" :: body` and produces **`PatVarCST "None"`** instead of `PatConstructorCST "None" []`! Meanwhile, literal patterns (`case 0 => ...`, `case "a" => ...`) and nested patterns hit `| _ :: rest => extract_cases rest` (`line 390`) and are **silently deleted**! And nested sub-patterns inside constructors (`case Pair(Some(x), y) => y`, `lines 331-336`) silently drop `Some(x)` so `y` binds to the **first** field of `Pair`!
- **Concrete Counterexample**:
  ```chester
  match Some(42) {
    case None => 0;
    case Some(x) => x;
  }
  ```
  `case None => 0` compiles as `PatVar "None"` (catch-all variable binding `None = Some(42)`), **evaluating to `0` instead of `42`**! And `match Some(42) { case None => 0 }` passes exhaustiveness checking because `CoreChecker` sees `PatVar "None"` as a catch-all wildcard!
- **Severity**: **Critical**

##### 4.4 `collapse_apps_aux` Strips Hygiene Spans on Every Call AND Corrupts `if ... then [1, 2]`, `box (e)`, `unbox (e)`, `unpack ... in (e)`
- **Location**: `theories/Expander.v:14-48`
- **Actual vs Expected**:
  1. Line `29`: `AppCST (Symbol kw empty_span) args span` **replaces the function symbol's `Span` (including `context : list nat`) with `empty_span`**, breaking hygiene resolution (`resolve_hygiene`) on every ordinary function call `f(x)`!
  2. Line `26`: Keyword exclusion list for `(...)` omits `box`, `unbox`, `return`, `perform`, `in`, `as`, `with`: `box (1 + 2)` collapses into `AppCST (Symbol "box") [...]`, failing `BoxCST` expansion and throwing `"Unbound variable: box"`! `unpack (X : S) = e in (body)` collapses `in (body)` into `AppCST (Symbol "in") [body]`!
  3. Line `38`: Keyword exclusion list for `[...]` omits `then`, `else`, `box`, `unbox`, `in` and all binary operators: **`if c then [1, 2] else [3, 4]`** collapses into `ImplicitAppCST (Symbol "then") [1, 2]` and `ImplicitAppCST (Symbol "else") [3, 4]`, breaking `expand_if`!
- **Severity**: **Critical**

##### 4.5 Missing Operators (`==`, `!=`, `&&`, `||`, Unary `-`), Broken Same-Block `infixl`/`infixr`, Ignored Right-Associativity, and `/` vs Effect-Row Collision
- **Location**: `theories/Expander.v:50-72, 121, 169-179, 459-484, 564-565`
- **Actual vs Expected**:
  1. `==`, `!=`, `&&`, `||` are absent from `op_prec` and `op_to_fn`: `a == b` and `a && b` fail with `"cannot apply non-function"`.
  2. Unary `-1` or `-x` fails with `"Unbound variable: -"`, and `a + -b` breaks the entire infix chain.
  3. `infixr` is treated identically to `infixl` (`line 472` discards `kwd`; `line 121` hardcodes `prec + 1` left-associativity).
  4. In `Block` (`lines 459-460` vs `1221`), `map_expand` expands all child `SeqOf` expressions **before** `process_stmts` registers `infixl`/`infixr` declarations, so custom operators fail in block tails, calls `f(a ++ b)`, tuples `(a ++ b)`, and conditions in the same file/block!
  5. `type T = Int / {io}` calls `expand_seq_expr` (`line 565`), where `try_parse_infix` sees `"/"` (`int_div`, prec 20) and rewrites the effectful type into `int_div(Int, {io})`!
- **Severity**: **High**

##### 4.6 `Parser.v` Defects: Unclosed Delimiters Accepted, `;`/`,` Interchangeable, `TokSym` vs `TokId` Block-Tail Asymmetry, and `"Unit"` Sentinel Clobbering
- **Location**: `theories/Parser.v:46-57, 83-96, 98-112, 137-168, 240-306, 984-986` and `theories/Expander.v:1229-1242`
- **Actual vs Expected**:
  1. **Unclosed `{`, `(`, `[` silently accepted (`lines 137, 145, 150`)**: Missing closing delimiters or mismatched `( 1 + 2 ]` never emit `CST.Error`; `sync` (`lines 46-57`) is completely dead code, and `parse` (`line 985`) discards unconsumed trailing tokens.
  2. **`TokSym` vs `TokId` block-tail bug (`lines 256-261` vs `287-292`)**: In `def f(): Int = { { 42 } }`, the inner block starts with `TokSym "{"`, hitting line `258` (`(item :: [], (unit_cst, rest1))`), which **pushes `{ 42 }` into discarded `stmts` and sets the outer block's return value to `Symbol "Unit"`**! Same happens at EOF (`lines 295-299`) for top-level `match` / `if ... else { ... }`.
  3. **User-written `Unit` clobbered (`Parser.v:83, 94`, `Expander.v:599, 1230-1241, 1517`)**: Missing block tails use `Symbol "Unit" empty_span`, and subsequent passes check `eqb u "Unit"` **without checking `empty_span`**. In `def f(): Type = { x = 1; Unit }`, `promote_tail` (`Expander.v:1236`) deletes the user's trailing `Unit` return type-value and promotes `x = 1`!
  4. **`;` and `,` completely interchangeable (`lines 160-161, 242-255, 280-286`)**: `f(1; 2,,, 3)` and `{ let x = 1, x }` parse identically to normal syntax; empty sequences fabricate `Symbol "Empty"` (`lines 87, 165`), which elaborates as an allowed unbound uppercase symbol of type `Any`!
  5. **`is_block_continue` (`lines 98-112, 139-142`)**: `{ 1 } + 2` splits into `{ 1 }; (+ 2)`, while `for`/`with`/`=` after a block-ending declaration glues unrelated statements together.
- **Severity**: **High**

##### 4.7 Macro System Is Completely Unhygienic, Truncates Multi-Token Bodies, and Skips Half of `CST`
- **Location**: `theories/Expander.v:1486-1687`
- **Actual vs Expected**:
  1. **`extract_macro_case` (`lines 1498, 1500`)**: Matches `Symbol "=>" _ :: body :: _` — **taking only the first token `body` after `=>` and silently discarding the rest**! `macro add1 { case (x) => x + 1 }` expands `add1(41)` to `41`!
  2. **Zero hygiene (`subst_cst`, `lines 1558-1588`)**: `Span.context` is never populated anywhere in the compiler (`[]` everywhere). `subst_cst` performs raw capture-prone substitution, doesn't even stop at shadowing binders (`LetCST "x"`, `LamCST "x"`), and hits `| _ => c` (`line 1587`) on `VarCST`, `AssignCST`, `ImplicitAppCST`, `DoCST`, `HandleCST`, `BoxCST`, `UnboxCST`, `PackCST`, `UnpackCST`, and `DefCST` param types — failing to substitute macro parameters inside any of them!
- **Severity**: **High**

##### 4.8 Module, Signature, Functor, Record, Pattern, and Extension Elaboration Defects
- **Location**:
  - `FunctorAppCST`: `theories/Elaborator.v:2910-2994`
  - `ModuleCST` / `SignatureCST`: `theories/Expander.v:636-666, 720-765`, `theories/Elaborator.v:2448-2474`
  - `RecordCST` / `FieldAccessCST` / `ExtensionCST`: `theories/Elaborator.v:868-897, 1977-2006, 3234-3325`
  - `EnumCST` / `MatchCST`: `theories/Elaborator.v:1670-1693, 2322-2364`
- **Actual vs Expected**:
  1. **`FunctorAppCST` never substitutes functor parameters in `body` (`lines 2966-2993`)**: `AstModule pref [] seal body` keeps raw references to formal parameter `X` (`X.f`), which only passes `CoreChecker` if `X` is uppercase (`is_allowed_unbound "X" = true` → `Any`)! Furthermore, `X.t` in unsealed functor exports stays `AstRef "t"` instead of resolving to the argument module's concrete `type t = Int`.
  2. **Sealed applicative functors lose `#applicative` (`Expander.v:661-666`, `Elaborator.v:2585-2588`)**: `module app F(X: S) :> R` overwrites `app_mark` with `opaque_seal`, silently turning it generative.
  3. **Polymorphic signature specs erase `[T]` (`Expander.v:734, 736, 750`)**: `signature S { def id[T](x: T): T }` drops `ListLiteral` type params and emits `SigValCST "id" [] ...`, causing `"Unknown type: T"`. Also `signature T = S with type t = A and type u = B` fails to expand (`lines 760-765` hardcode 1 equation).
  4. **`ModuleCST` drops `enum`/`record`/`let` exports and lacks mutual recursion (`Elaborator.v:2456-2467`)**.
  5. **Records & unknown methods silently type as `Any`**: `RecordCST` (`lines 3234-3246`) binds no constructor and records no field types; `r.anyFakeField` (`line 3274`) and `x.anyFakeMethod(1, 2)` (`lines 1980, 2001`) silently return `AstRef "Any"`.
  6. **Extensions ignore type arguments (`lines 868-897`) and fail on self-recursion (`lines 3304-3323`)**: `check_type_match` strips `List(Int)` and `List(String)` to `AstRef "List"`, colliding same-named methods, and recursive `xs.len()` inside an extension body fails with `"Unbound variable: Ops_len"`.
  7. **Pattern matching ignores GADT variant return types (`line 1680`), constructor arity (`lines 2343-2359` — excess binders get `Any`), unknown constructors (`line 2326` — gets `Any`), and user `enum` exhaustiveness**; `AppCST` in `Elaborator` (`lines 2169-2192`) also silently accepts excess value arguments and calls on `Int` variables (`42(1)` → `Any`).
- **Severity**: **High**

---

#### Category 5: Formatter & Span Round-Trip Corruption Bugs (High / Medium)

##### 5.1 Coq Literal `"\n"` Emits Backslash + `n` Instead of Newlines in `ModuleCST` and `SignatureCST`
- **Location**: `theories/Formatter.v:298, 337, 338, 364, 366`
- **Actual vs Expected**: Coq's `string_scope` has no `\n` escape (which is why `line 10` defines `newline := String (ascii_of_nat 10) ""`). Lines `298, 337, 338, 364, 366` use `" {\n"`, `"\n}"`, and `"\n"`, emitting literal `\n` characters (`\` is parsed as lambda!) and corrupting every formatted module and signature!
- **Severity**: **High**

##### 5.2 Unescaped `StringLiteral`, `/* EXPANDED NODE */` Stubs, `ExternCST` Ellipses, and Broken `get_span` / `cst_adjacent`
- **Location**: `theories/CST.v:118-159`, `theories/Parser.v:89, 138, 147, 152`, `theories/Formatter.v:81-121, 166-181, 213, 343, 378, 418-448`
- **Actual vs Expected**:
  1. **Unescaped strings (`Formatter.v:213`)**: `quote ++ val ++ quote` emits raw quotes, backslashes, and newlines inside string literals, breaking syntax on round-trip.
  2. **19 CST nodes emit `"/* EXPANDED NODE */"` (`lines 430-448`)** and **`ExternCST` emits `"def name(...): ..."` (`lines 421-425`)**, while `SigValCST` (`lines 343, 378`) drops type parameters `[T]`.
  3. **Broken `get_span` & parser spans break `cst_adjacent`**:
     - `CST.get_span` (`CST.v:158`) omits `LamCST`, `AppCST`, and `MacroDefCST`, returning `empty_span` (`zero_pos`..`zero_pos`).
     - `Parser.v` gives `SeqOf` `empty_span` (`line 89`) and gives `Block`/`Tuple`/`ListLiteral` **only the opening token's span** (`lines 138, 147, 152`).
     - On nodes with `empty_span`, `cst_adjacent` (`Formatter.v:166`) sees `zero_pos == zero_pos` (`true`) and omits spaces (`if(x)`, `return(x)`, `then[1]`).
  4. **Unconditional semicolons (`lines 81-121`)**: `trailing_semicolon` ignores `rest` and `tail` (`starts_with_let` at `line 75` is dead code) and appends `;` to `def`, `enum`, and `module` declarations.
  5. **Build-output pollution**: `Print Assumptions` left in `Parser.v:1241-1243` and `Eval compute` in `Elaborator.v:3343` (plus unused `Require Import Chester.Formatter` at `Elaborator.v:56`).
- **Severity**: **Medium**

---

## 2. Core checker, proofs, backends & extraction (`theories/`)

Every line of all 15 requested files (`AGENTS.md`, `CoreChecker.v`, `CoreTyping.v`, `TypeSystemProps.v`, `Backend.v`, `GoAST.v`, `GoInterop.v`, `GoIRSem.v`, `TypeScriptAST.v`, `TypeScriptInterop.v`, `TypeScriptIRSem.v`, `RocqAST.v`, `RocqBackend.v`, `RocqIRSem.v`, `Extract.v`) was read in full and cross-checked against `AST.v`, `Elaborator.v`, `bin/main.ml`, `bin/effects_runtime.ml`, and `bin/lexer.ml`.

---

#### Part 1: `CoreChecker.v` Soundness Bugs, Bypasses & `Elaborator.v` Disagreements

##### 1.0 Pipeline Invocation Status & Cross-File Erasure
- **Is `CoreChecker` run in the main pipeline?** Yes: `bin/main.ml:210` calls `elaborate_top`, and `theories/Elaborator.v:3364-3381` runs `CoreChecker.infer_check (core_env_of_elab env) (fst res') None`, followed by `unknown_effect_label`, `duplicate_def_in_program`, and `unknown_ty_in_program`.
- **[HIGH] `elaborate_top` drops `expected` when invoking `CoreChecker` (`theories/Elaborator.v:3364`)**: `infer_check` is called with `None` instead of `expected`.
- **[CRITICAL] Cross-file environment collection erases all effect rows and module seals (`bin/main.ml:58-103`)**:
  - `main.ml:70-71` and `77-78` record every top-level and module `AstDef` from `--prelude` or prior files with `AstFunTy (tps, ps, rt, [])`, **erasing all declared effects (including `div` and user effects) to `[]`**.
  - `main.ml:73-84` walks `AstModule` `body` directly without respecting `seal` and types nested `AstModule` exports as `AstModTy []` (`main.ml:80`), losing all nested exports.

---

##### 1.1 Critical Type-Soundness Bypasses in `CoreChecker.v` (and `Elaborator.v`)

1. **[CRITICAL] Immediately-Applied Lambda Shortcut Completely Skips Typechecking the Lambda Body (`theories/CoreChecker.v:1360-1365, 1417-1418`, `theories/CoreChecker.v:611, 1282`, `theories/Elaborator.v:1477`)**
   - In `infer_check` on `AstApp func args`, lines `1360-1365` extract:
     ```coq
     let lam_pi := match strip_span func with | AstLam n aty _ => Some (n, aty, AnyType) | _ => None end
     ```
     and line `1418` dispatches directly to `apply_pi n aty AnyType` **without ever calling `infer_check` on `func` or the lambda's `body`**, and hardcoding the return type to `AnyType`.
   - **End-to-End Exploit (`resume` outside handler)**: `"resume"` is listed in `is_preamble_prim` (`CoreChecker.v:611`), so `Elaborator.is_allowed_unbound "resume" = true` (`Elaborator.v:40, 1477`). `Elaborator` relies *exclusively* on `CoreChecker.v:1282` (`if String.eqb name "resume" then TyErr "resume outside handler"`) to reject top-level `resume`. Wrapping it in an applied lambda bypasses `CoreChecker` completely:
     ```chester
     def crash(): Integer = (\x: Integer => resume(42))(0)
     ```
     Passes both `Elaborator` and `CoreChecker`; fails Go build (`undefined: resume`) and crashes TS at runtime (`ReferenceError: resume is not defined`). Any ill-typed AST node (`AstError`, `1(2)`, unbound vars) inside an applied lambda body is likewise invisible to `CoreChecker`, and its return type `AnyType` matches any expected type.

2. **[CRITICAL] Symmetric Effect-Row Subsumption Collapses Every Effect Row Into Pure `[]` (`theories/CoreChecker.v:544, 564, 1040, 1044`, `theories/Elaborator.v:626, 639`)**
   - Both `Elaborator.unify` and `CoreChecker.types_compat_raw` / `equiv_ast_raw` compare function effect rows using **symmetric** disjunction:
     ```coq
     orb (effect_row_subsumes e1 e2) (effect_row_subsumes e2 e1)
     ```
   - Because `effect_row_subsumes [] anything = true` (`AST.v:88-89`), comparing **any** impure/divergent row `e1 = [UserEffect "State"; BuiltinEffect "div"]` against the pure row `e2 = []` evaluates `orb false true = true`!
   - **Counterexample**:
     ```chester
     effect State { def get(): Integer }
     def impure(x: Integer): Integer / {State, div} = perform get()
     def run_pure(f: (x: Integer) -> Integer): Integer = f(0)
     def main(): Integer = run_pure(\x: Integer => impure(x))
     ```
     `run_pure` has pure parameter `(x: Integer) -> Integer` (`effs = []`) and pure return type, yet accepts an impure/divergent callback, laundering `{State, div}` into `{}` and crashing at runtime with `Unhandled effect operation: State/get`.

3. **[CRITICAL] `is_allowed_unbound` Treats All Builtin Primitives, Uppercase Names, `prim__*`, and `__*` as Universal Wildcard `Any` (`theories/CoreChecker.v:581-620, 1283`, `theories/Elaborator.v:134, 1477`)**
   - `init_elab_state` (`Elaborator.v:134`) and `main.ml:441` start with an **empty** `TypeEnv`. None of `int_add`, `int_sub`, `int_eq`, `bool_and`, `string_concat`, etc. have function signatures in `TypeEnv` unless `--prelude` defines them!
   - Instead, `is_allowed_unbound` returns `true` for all `is_preamble_prim` names, **every identifier starting with `A`–`Z`**, `prim__*`, and `__*`, typing them as `AstRef "Any"` (which `is_wildcard_ty` / `types_compat` unifies with *everything*).
   - **Counterexample 1 (Type-corrupt arithmetic)**:
     ```chester
     def bad(): String = 1 + "not_an_int"
     ```
     Without `--prelude`, `int_add` has type `Any`, arguments `1` and `"not_an_int"` are checked against `None`, and return type `Any` unifies with `String`. Passes both `Elaborator` and `CoreChecker`; Go emits `func bad() string { return __chester_as_string(int_add(1, __chester_as_int(`not_an_int`))) }` which **panics at runtime** (`interface conversion: interface {} is string, not int`).
   - **Counterexample 2 (Arbitrary unbound uppercase identifiers in expressions)**:
     ```chester
     def bad2(): Integer = CompletelyUndefinedIdentifier
     ```
     `name_is_upper` is `true` -> typed as `Any` -> unifies with `Integer` (`unknown_ty_in_program` only checks declaration *type annotations*, not expression bodies). Fails Go compile / crashes TS with `ReferenceError`.

4. **[CRITICAL] Opaque Module Sealing & Generative Functors Are Completely Unsound Across Multiple Modules (`theories/Elaborator.v:1230, 3256-3260`, `theories/CoreChecker.v:1062-1063, 1600-1628, 1899-1921, 1997-2026`)**
   - In `Elaborator.v:3256-3260`, projecting an abstract type `M.t` (`AstTypeDecl _ None`) in type position drops `M` and returns bare **`AstRef "t"`**. Likewise `seal_sigval_member` (`Elaborator.v:1230`) exports `AstFunTy tps params ret effs` with unqualified `AstRef "t"`.
   - In `CoreChecker.v:1062-1063`, `types_compat_raw` equates `AstFieldAccess _ f` with `AstRef f` regardless of module path.
   - **Counterexample (Breaking Opaque Sealing)**:
     ```chester
     signature S { type t; def mk(): t; def eat(x: t): Integer }
     module M1 :> S { type t = String; def mk(): String = "boom"; def eat(x: String): Integer = 0 }
     module M2 :> S { type t = Integer; def mk(): Integer = 1; def eat(x: Integer): Integer = x + 1 }
     def exploit(): Integer = M2.eat(M1.mk())
     ```
     Both `M1.mk()` and `M2.eat`'s parameter have elaborated type `AstRef "t"`, so `M2.eat(M1.mk())` typechecks cleanly and **panics at runtime in Go** (`__chester_as_int("boom")`). Generative functors (`module N1 = F(A); module N2 = F(B)`) suffer the exact same confusion (`N1.t` ≡ `AstRef "t"` ≡ `N2.t`).
   - Furthermore, `CoreChecker` **never checks module sealing at all**:
     - `CoreChecker.v:1600-1626` matches `seal` against `Some (AstSignature _ _)` / `Some (AstModTy _)`, but `Elaborator.v:2576-2582` wraps seals in `Some (AstApp (AstRef "#opaque") [...])`, `Some (AstApp (AstRef "#transparent") [...])`, or `Some (AstRef sigName)`, so `CoreChecker` **always hits `_ => exports_of body`** (leaking unsealed exports!).
     - `AstModule` (`CoreChecker.v:1899`) ignores `seal` and `params`; `AstPack` (`1997-2005`) never checks `m` against `s`; `AstUnpack` (`2019-2022`) accepts *any* expression `e` (e.g. `42`) whenever `sty` is an `AstSignature` or `AstModTy`!

5. **[CRITICAL] `AstAssign` Is Completely Unchecked in Both `Elaborator.v` and `CoreChecker.v` (`theories/Elaborator.v:1633-1636, 1744-1746`, `theories/CoreChecker.v:1710-1714`)**
   - Neither checker looks up the target variable of `x = expr` in `env` or checks mutability (`var` vs `let`).
   - **Counterexamples** (all pass both `Elaborator` and `CoreChecker`):
     1. `never_declared = 42` -> invalid Go (`undefined: never_declared`) / ReferenceError in strict JS.
     2. `let x = 1; x = 2` -> mutates immutable `let` (`const x = 1` in TS -> **runtime `TypeError: Assignment to constant variable`**).
     3. `var x = 1; x = "wrong_type"; x + 1` -> in Go emits `var x int = 1; x = __chester_as_int("wrong_type")` -> **runtime panic**.
   - Also `AstLet _ _` outside `AstBlock.stmts` (`CoreChecker.v:1704`) does not even typecheck the RHS expression.

6. **[HIGH] `core_instantiate` Erases Parametric Polymorphism to `Any` (`theories/CoreChecker.v:648-671, 1278, 1426`)**
   - `core_subst_tps` substitutes every type parameter `T` in `AstFunTy` with `AnyType`. Thus in `CoreChecker`, `def same[T](a: T, b: T): T` has instantiated type `(Any, Any) -> Any`; `CoreChecker` accepts `same(1, "str")` and `let s: String = id(42)` if an AST reaches it.

7. **[HIGH] Unsound `types_compat` & Non-Alpha-Equivalent `equiv_ast_raw` (`theories/CoreChecker.v:524-570, 1025-1075, 1293-1315`, `theories/Elaborator.v:673, 2380-2406`)**
   - **List/Tuple/Type-App erasure (`CoreChecker.v:1057-1058, 1293-1315`, `Elaborator.v:2394-2405`)**: `[ "not_an_int", true ]` has type `AstRef "List"`, and `types_compat_raw` equates `AstRef n` with `AstApp (AstRef n) args` for *every* constructor `n`! Because `Elaborator.unify` (`line 673`) falls back to `types_compat`, `def bad(): List(Integer) = ["not_an_int", true]` **passes both `Elaborator` and `CoreChecker`**! Also non-empty `TupleCST` elaborates to `AstTuple`, which `CoreChecker.v:1311` types as `List`.
   - **Non-reflexivity of `equiv_ast` and `types_compat`**: Neither `equiv_ast_raw` nor `types_compat_raw` has a case for non-empty `AstTuple`, `AstModTy`, `AstSignature`, or `AstTypeDecl`. Hence `types_compat (AstTuple [IntType]) (AstTuple [IntType]) = false`!
   - **Broken binder equality (`CoreChecker.v:539-544`)**: `equiv_ast_raw` compares `AstLam` and `AstPi` bodies structurally **ignoring binder names without alpha-renaming**:
     - False negative: `(x: Type) -> x` is NOT `equiv_ast` to `(y: Type) -> y`.
     - False positive (unsound): `AstPi "x" TypeUniverse (AstRef "y") []` IS `equiv_ast` to `AstPi "y" TypeUniverse (AstRef "y") []`!

8. **[HIGH] Impredicative `Type : Type` Universe Collapse / Girard's Paradox & Unchecked `AstFunTy` Formation (`theories/CoreChecker.v:572, 621-627, 1071, 1290, 1518-1550`, `theories/Elaborator.v:1478`)**
   - `AstUniverse l` has type `TypeUniverse = AstRef "Type"` for *every* `l`, `Elaborator.v:1478` types `Symbol "Type"` at `TypeUniverse`, and `types_compat` (`CoreChecker.v:1071`) equates all universes (`is_universe_ty t1 && is_universe_ty t2`). Thus `Type : Type` holds, admitting Girard's/Hurkens' paradox.
   - `AstFunTy` (`CoreChecker.v:1532-1550`) discards the inferred sort of parameter and return types (`TyOk _`), accepting nonsense types like `AstFunTy [] [("x", AstIntLit 42)] (AstBoolLit true) [] : Type`.
   - Conversely, `AstPi "A" (AstRef "Type") ...` **fails** `CoreChecker.v:1520-1521` when `"Type"` is not in `env`, because `infer_check [] (AstRef "Type") None` returns `AnyType` (`is_allowed_unbound "Type" = true`), and `is_universe_ty AnyType = false`!

---

##### 1.2 Disagreements Where `CoreChecker.v` Rejects Valid `Elaborator.v` Output

1. **[HIGH] `AstBox` / `AstUnbox` Type Disagreement (`theories/Elaborator.v:1748-1776` vs `theories/CoreChecker.v:1872-1884`)**
   - `Elaborator` types `box { e }` as a thunk **`AstPi "x" (AstRef "Unit") T []`** (`Unit -> T`) and `unbox b` peels `AstPi`/`AstFunTy` to return **`T`**.
   - `CoreChecker` (`lines 1872-1884`) types `AstBox e _` as **`T`** (not `Unit -> T`) and `AstUnbox b` as **`typeof(b)`** (without peeling `Unit -> T`)!
   - **Counterexample**:
     ```chester
     def run_boxed(b: (x: Unit) -> Integer): Integer = unbox b
     ```
     `Elaborator` succeeds; `CoreChecker` infers `unbox b : (x: Unit) -> Integer` against expected `Integer` and **fails with `Core check: type mismatch against expected`**. Likewise `def mk(): (x: Unit) -> Integer = box { 42 }` fails `CoreChecker`.

2. **[HIGH] `AstHandle` Plotkin `return` Clause Type Disagreement (`theories/Elaborator.v:3136-3230` vs `theories/CoreChecker.v:1850-1871`)**
   - `Elaborator` allows `case return(x) => ...` to change the handler's answer type from `body_ty` to `answer_ty` (checking `body` with `None` and unifying `answer_ty` with `expected`).
   - `CoreChecker.v:1856` checks the handled body `e` directly against `expected` (`infer_check env e expected`)!
   - **Counterexample**:
     ```chester
     effect E { def op(): Integer }
     def test(): String =
       handle { 42 } with E {
         case return(x) => int_to_string(x)
         case op() => resume(1)
       }
     ```
     `Elaborator` succeeds (`body = 42 : Int`, `answer_ty = String`), then `CoreChecker` checks `42` against `Some String` and **fails with `Core check: type mismatch against expected`**. Meanwhile, when `expected = None`, `CoreChecker.v:1863` checks handler clauses with `None`, failing to verify clause return types match `ty`!

3. **[MEDIUM] `AstLam` Checked Against `AstFunTy` Rejected (`theories/CoreChecker.v:1317-1335` vs `theories/Elaborator.v:643-646`)**
   - `Elaborator.unify` unifies `AstFunTy` and `AstPi` via currying (`funty_as_pi`), whereas `CoreChecker` on `AstLam` with `expected = Some (AstFunTy ...)` hits line `1335` (`TyErr "Expected Pi type for lambda"`).

4. **[MEDIUM] Lowercase Functor Parameters Fail `CoreChecker` (`theories/CoreChecker.v:1899` vs `theories/Elaborator.v:2425-2447`)**
   - `CoreChecker` ignores functor `params` on `AstModule _ _ _ body`. Uppercase functor params (`module F(X: S)`) only pass `CoreChecker` by accident via `is_allowed_unbound "X" = true` (`Any`); lowercase `module F(x: S) { def f(): Integer = x.v }` passes `Elaborator` and fails `CoreChecker` with `Unbound variable: x`.

5. **[MEDIUM] Macro Hygiene Mangling Mismatch on `DefCST` Params and `MatchCST` Binders (`theories/Elaborator.v:1791-1798, 2320, 2356, 3346`)**
   - `DefCST` emits unmangled parameter names in `AstDef` (`line 1791`), but binds `(pname, context span)` in `body_env` (`line 1798`), so references in macro-generated `def`s elaborate to `AstRef "pname_1"` while `AstDef` binds `"pname"`. Same mismatch on `PatVar` / `PatConstructor` in `MatchCST` (`lines 2320-2321, 2356-2363`).

---

##### 1.3 Substitution, Fuel (`AGENTS.md`), Exhaustiveness & Termination Bugs

1. **[HIGH] Variable Capture Bugs in `subst_ast_fuel` (`theories/CoreChecker.v:196-205, 286-367, 379-402, 435-440`)**
   - **`fresh_name` `avoid` list omits free variables of `v` and `body` (`lines 290, 298, 326, 346, 381, 391`)**:
     Line `290` computes `let z := fresh_name argName [x; argName]`. Because `[x; argName]` omits free variables of `argBody` and `v`, `"y'"` is chosen even when `"y'"` is already free!
     - **Counterexample 1 (captures free var in body)**:
       `subst_ast "x" (AstRef "y") (AstLam "y" IntType (AstTuple [AstRef "x"; AstRef "y'"]))`
       evaluates to `AstLam "y'" IntType (AstTuple [AstRef "y"; AstRef "y'"])` — **capturing free `"y'"` in `body`**!
     - **Counterexample 2 (captures free var in `v`)**:
       `subst_ast "x" (AstTuple [AstRef "y"; AstRef "y'"]) (AstLam "y" IntType (AstRef "x"))`
       evaluates to `AstLam "y'" IntType (AstTuple [AstRef "y"; AstRef "y'"])` — **capturing free `"y'"` in `v`**!
   - **`AstUnpack` forgets capture avoidance (`lines 435-440`)**: Checks `String.eqb n x` but never checks `free_in n v`.
   - **`AstBlock` ignores `AstLet`/`AstVar` shadowing (`lines 45, 115, 281-282`)**: `subst_ast "x" (AstIntLit 99) (AstBlock [AstLet "x" (AstIntLit 1)] (AstRef "x"))` rewrites the shadowed tail `x` to `99`.
   - **`AstFunTy` substitutes into all param types before checking shadowing/capture of earlier dependent params (`line 338`)**.
   - **`strip_span` is incomplete (`lines 493-498, 521`)**: Leaves `AstSpan` inside `AstFunTy` parameter types, `AstDef`, `AstMatch` case bodies, `AstVar`, `AstAssign`, `AstBox`, `AstUnbox`.

2. **[HIGH] `AGENTS.md` Fuel Audit — Under-Counted `ast_size` Causes 0-Fuel Truncation Bypasses (`theories/CoreChecker.v:236, 357-367, 785, 791-914`)**
   - While literal magic constants (`4096`/`1000`) were removed per `AGENTS.md`, **`ast_size` (`line 236`) defines `AstEnum _ _ _ => 1` and `AstRecord _ _ _ => 1`**, ignoring all variants and fields!
   - **Consequence 1 (`unknown_ty_in_program` 100% bypassed on `enum` and `record`)**:
     For `prog = AstBlock [AstRecord "R" [] [("x", AstRef "NoSuchType")]] (AstRef "Unit")`, `ast_size prog = 3`, so `unknown_ty_in_program` (`line 913`) passes `fuel = 4`:
     `AstBlock` (4→3) → `unknown_ty_in_decls` (3→2) → `AstRecord` (2→1) → `first_unknown_params` (1→0) → `first_unknown_name 0` = **`None`**!
     Every `record` and `enum` with unknown field types silently passes `unknown_ty_in_program`!
   - **Consequence 2 (`subst_ast` fails inside `AstEnum` variants, `lines 357-367`)**: `subst_ast` passes `fuel = 2` for any `AstEnum`, running out of fuel at depth 1 inside variant field types.
   - **Consequence 3 (`ty_name_ok` underscore bypass, `line 785`)**: `string_has_underscore n` accepts **any** undeclared type containing `_` (e.g. `def f(x: Bogus_Type): Int`).

3. **[HIGH] Match Exhaustiveness & Pattern Soundness Holes (`theories/CoreChecker.v:972-994, 1755-1826`, `theories/Elaborator.v:2309-2379`)**
   - **Zero exhaustiveness checking on user-defined `enum`s, `Boolean`, `List`, `Integer`, or `String`**.
   - **Undeclared constructors accepted in patterns & defeat builtin exhaustiveness**: Both `Elaborator.v:2326` and `CoreChecker.v:1786` default unknown constructor names to `AnyType`! Thus:
     ```chester
     def crash(): Integer = match 42 { case MadeUpCtor(a, b) => a }
     ```
     passes *both* `Elaborator` and `CoreChecker` (since `"MadeUpCtor"` is not in `["Some";"None"]`, `match_missing_builtin_variant` returns `None`!) and **panics at runtime** in Go (`_match_val.(map[string]any)` on `int`). Even `match opt { case Some(x) => x; case Fake => 0 }` bypasses `Option` exhaustiveness!
   - **Unchecked constructor pattern arity (`Elaborator.v:2358`, `CoreChecker.v:1800`)**: `match Some(1) { case Some(a, b, c) => b; case None => 0 }` types extra binders `b, c` as `Any` and panics at runtime in Go (`index out of range [1]`).

4. **[HIGH] Seven Distinct Termination Checker Bypasses (`theories/CoreChecker.v:1105-1236`)**
   All of the following compile as **pure/total (`[]` effect row, no `div`)** and diverge at runtime:
   1. **Mutual recursion**: `def a(x: Integer): Integer = b(x)` / `def b(x: Integer): Integer = a(x)` (`check_terminates` only inspects direct self-calls to `fn`).
   2. **Signed integer subtraction (`lines 1114-1117`)**: `def loop(n: Integer): Integer = loop(n - 1); loop(0)` -> `0, -1, -2, ...` (`int_sub(n, 1)` is treated as unconditionally decreasing with no `n > 0` guard!).
   3. **`advance` on empty list (`lines 1118-1121` + `bin/effects_runtime.ml:14, 307-309`)**: `advance([])` returns `[]` at runtime, yet `is_decreasing_arg` treats `advance(xs)` as strictly smaller: `def loop(xs: List(Integer)): Integer = loop(advance(xs)); loop([])` loops forever!
   4. **Argument position swap (`line 1159`)**: `existsb (is_decreasing_arg params smaller) args` does not check *which* parameter decreased! On `enum Nat { case Z; case S(p: Nat) }`:
      `def loop(a: Nat, b: Nat): Integer = match a { case Z => 0; case S(p) => loop(S(S(a)), p) }` — `p` is in arg 2 while arg 1 grows (`loop(S(Z), Z)` diverges!).
   5. **Module-qualified self-call**: `module M { def loop(): Integer = M.loop() }` — `free_in "loop" (AstFieldAccess (AstRef "M") "loop")` is `false` (`line 83`), bypassing `check_terminates` at line `1235`!
   6. **Shadowing / `var` mutation of smaller binder (`lines 1145-1150, 1176`)**: `smaller` list is never invalidated on `AstAssign` or inner shadowing.
   7. **Non-positive inductive types (Landin's / Girard's knot)**: No positivity check on `AstEnum`: `enum Bad { case Mk(f: (x: Bad) -> Integer) }` encodes `\x => x(x)` without `rec` or `div`.

5. **[LOW] `declared_effect_labels` & `top_def_names` Miss Subtrees (`theories/CoreChecker.v:917-968`)**
   - Both ignore `AstBlock`'s `tail` expression and `AstExtension` methods (`extension E { def f(): Int / {undeclared_eff} = 1 }` bypasses `unknown_effect_label`).

---

#### Part 2: Proof Audit (`CoreTyping.v`, `TypeSystemProps.v`, `GoIRSem.v`, `TypeScriptIRSem.v`, `RocqIRSem.v`)

The formal proof files contain zero `Admitted` / axioms (`Print Assumptions` is closed), **but they do NOT prove semantic preservation of the real compiler pipeline**. There are five fundamental disconnects:

1. **[CRITICAL] Zero Formal Link Between `CoreWT` and `Elaborator` / `CoreChecker` — In Fact They Disagree! (`theories/CoreTyping.v:252-280`)**
   - `CoreWT` is a standalone 7-rule declarative judgment invented in `CoreTyping.v`. Neither `elaborate_top ... -> CoreWT` nor `infer_check ... -> CoreWT` (nor the converse) is stated or proved.
   - Worse: `CoreTyping.v:252` defines `CoreArrow σ τ := AstFunTy [] [("_", σ)] τ []`, so `CWT_Lam` types `AstLam` as `AstFunTy`, whereas `CoreChecker.infer_check` (`CoreChecker.v:1335-1338`) types `AstLam` as `AstPi` and **rejects** `AstLam` checked against `AstFunTy` (`TyErr "Expected Pi type for lambda"`)!

2. **[CRITICAL] Vacuous Hypothesis on Higher-Order Functions in `CoreFrag ∩ CoreWT` (`theories/CoreTyping.v:240-273`)**
   - `CF_Lam` (`line 244-245`) requires **`CoreFrag ty`** on the lambda's parameter type annotation `ty`.
   - `CWT_Lam` (`line 267-269`) uses that exact `ty` (`σ`) in `(x, σ) :: Γ`, and `CWT_App` (`line 271`) requires `f` to have type `CoreArrow σ0 τ0 = AstFunTy [] [("_", σ0)] τ0 []`.
   - **Yet `AstFunTy` is NOT in `CoreFrag`!**
   - Consequently, **no closed higher-order lambda (`\f: (Int -> Int) => f(1)`) can EVER satisfy `CoreFrag e /\ CoreWT [] e ty`**! In `emit_go_correct_closed`, `emit_ts_correct_closed`, and `emit_rocq_correct_closed`, every lambda parameter is restricted to first-order `Integer` or `Boolean`.

3. **[CRITICAL] `GoIRSem.v` Cheats by Baking `Backend.v` Helpers With Unconstrained AST Arguments Directly Into `GoEval` (`theories/GoIRSem.v:109-118`)**
   - Look at `GE_Call` (`GoIRSem.v:109-114`):
     ```coq
     | GE_Call : forall env direct loc f f_ast arg x body envf va v,
         GoEval env f (GVClo x body envf) ->
         GoEval env arg va ->
         GoEvalBlock ((x, va) :: envf) body v ->
         GoEval env (go_call_emitted direct f (go_call_args [] loc f_ast [arg])) v
     ```
   - Instead of giving real Go semantics to `GoCall`, `__chester_call`, `__chester_as_int`, and `__chester_as_bool`, `GoEval` pattern-matches on the unexpanded Coq functions `go_call_emitted`, `go_call_args`, and `go_bool_cond` (`GE_CondDirect`, `line 118`)!
   - **False "Correctness" Consequence 1 (Open terms fail Go compilation)**: In `emit_go_expr_correct` (`GoIRSem.v:408-417`), `loc` is hardcoded to `[]` even when `gamma = [("g", CoreArrow IntType IntType)]`. For `e = AstApp (AstRef "g") [AstIntLit 42]`, `go_app_direct [] (AstRef "g") = true` (`Backend.v:424-433`), emitting direct `g(42)` on `var g any = func(x any) any { ... }` — **a Go compile error (`cannot call non-function g (variable of type any)`)**, yet `emit_go_expr_correct` "proves" it evaluates to `GVNum 42` because `GE_Call` ignores `direct = true`!
   - **False "Correctness" Consequence 2 (`GE_Call` derives values for panicking Go coercions)**: Because `f_ast` in `GE_Call` is universally quantified and unconstrained, setting `direct := true`, `loc := []`, `f_ast := AstRef "int_add"`, `arg := GoBoolLiteral true` derives `GoEval [] (GoCall id_fn [GoCall (GoIdentifier "__chester_as_int") [GoBoolLiteral true]]) (GVBool true)` — even though `__chester_as_int(true)` **panics at runtime in real Go**!

4. **[CRITICAL] `emit_rocq_correct_closed` and `emit_ts_correct_closed` Prove "Correctness" of Terms That Fail to Parse or Typecheck in Real Rocq and TypeScript!**
   - **TypeScript (`TypeScriptIRSem.v:464` + `CoreTyping.v:292` + `TypeScriptAST.v:126, 134`)**:
     Take `CoreTyping.core_wt_id_app`: `AstApp (AstLam "x" IntType (AstRef "x")) [AstIntLit 42]`.
     `emit_ts_correct_closed` proves its IR evaluates to `TSVNum 42`.
     Yet `stringify_ts_expr` serializes `TsCall (TsArrow ["x"] [TsReturn (TsIdentifier "x")]) [TsNumberLiteral "42"]` as:
     ```ts
     (x) => { return x; }(42)
     ```
     which is a **JavaScript / TypeScript `SyntaxError: Unexpected token '('`** (unparenthesized arrow callee)!
   - **Rocq (`RocqIRSem.v:285` + `RocqAST.v:70, 76` + `bin/effects_runtime.ml:654-672`)**:
     Take the closed `CoreFrag ∩ CoreWT` term `AstLam "b" BoolType (AstIf (AstRef "b") (AstIntLit 1) (AstIntLit 0))`.
     `emit_rocq_correct_closed` proves it evaluates to `RVClo`.
     Yet `stringify_rocq_expr` emits `(chester_fun (fun b : chester_dyn => (if b then 1%nat else 0%nat)))`, which **Rocq rejects with a type error** because `b : chester_dyn` (`Axiom chester_dyn : Type`) is not an inductive `bool`! Moreover, in `rocq_effects_preamble` (`effects_runtime.ml:659-660`), `chester_fun` and `chester_app` are uninterpreted `Axiom`s with **no beta-reduction rule**, so emitted Rocq applications never compute!

5. **[HIGH] Scope & Realism Gaps Across All Semantics Files**
   - **`CoreFrag` covers < 5% of `AST`** (7 of 39 constructors; 1-arg apps only; zero primitives/arithmetic, blocks, `let`/`var`/`assign`, `def`/recursion, strings, tuples/lists, enums/match, records, polymorphism, modules/functors, or algebraic effects `handle`/`perform`/`box`/`unbox`).
   - **Top-level emitters & stringifiers unverified**: `emit_go_top_with`, `emit_ts_top`, `emit_rocq_top`, `stringify_go_stmt`, `stringify_ts_stmt`, `stringify_rocq_stmt` have zero proofs.
   - **Idealized `nat` numbers**: `TSE_Num` / `GE_Num` assume exact unbounded `nat`, falsified by JS IEEE-754 `double` precision loss at $n \ge 2^{53}$ and Go `int` compile-time overflow at $n \ge 2^{63}$.
   - **Scoping/Syntax**: `CoreWT` allows target keywords as variable names (`AstLam "return" IntType (AstRef "return")`), duplicate `TsLet`/`GoLet` in blocks without redeclaration error, and `TSB_IfT`/`GEB_IfT` silently drop trailing statements `rest` after an `if` without `return`.
   - **`CoreTyping.v:53-234` & `TypeSystemProps.v`**: Only prove shallow helper properties (list membership / effect subsumption head-of-list on `seal_exports_check_flat`, reflexivity of `effect_row_subsumes`, 3-line `free_in`/`strip_span` lemmas, and `reflexivity` unit tests). `GoInterop.v` and `TypeScriptInterop.v` are unused 30-line `Any`-returning mocks.

---

#### Part 3: Backend Codegen & Extraction Bugs (`Backend.v`, `GoAST.v`, `TypeScriptAST.v`, `RocqBackend.v`, `RocqAST.v`, `Extract.v`)

##### 3.1 TypeScript Backend (`theories/Backend.v`, `theories/TypeScriptAST.v`)
1. **[CRITICAL] Enum Constructor Field vs. `AstMatch` Destructuring Mismatch Crashes at Runtime (`Backend.v:550, 618-633, 716, 757`)**
   - `AstEnum` (`lines 618-633`) emits variant payloads as `{ _tag: "Ctor", _f0: ..., _f1: ... }` (no `.args` field).
   - `emit_ts_expr` (`line 550`) reads `_match_val._f0`, **but `emit_ts_stmt` (`line 716`) and `emit_ts_block` (`line 757`, used for ALL `def`, `lam`, and `if` bodies!) read `_match_val.args[0]`**!
   - **Counterexample**:
     ```chester
     enum Boxed { case Wrap(x: Integer) }
     def unwrap(b: Boxed): Integer = match b { case Wrap(x) => x }
     unwrap(Wrap(42))
     ```
     Crashes at runtime in JS/TS with **`TypeError: Cannot read properties of undefined (reading '0')`**!
   - Also `bind_vars` (`lines 550, 716, 757`) prepends to `acc`, emitting bindings in reverse order.
2. **[CRITICAL] Nested `AstMatch` Triggers JS Temporal Dead Zone (`ReferenceError`) or Duplicate `const _match_val` (`SyntaxError`) (`Backend.v:746-765`)**
   - In `emit_ts_block`, `PatConstructor` emits `{ const inner = _match_val.args[0]; <emit_ts_block body> }`. If `body` is another `AstMatch`, `<emit_ts_block body>` emits `const _match_val = inner;` **in the same `{ ... }` block**, hoisting `_match_val`'s TDZ above `const inner = _match_val...` -> **`ReferenceError: Cannot access '_match_val' before initialization`**!
   - In `PatVar v => match ...` (`line 761`), there is no new `{ ... }` block at all, emitting two `const _match_val = ...` in the same scope -> **`SyntaxError: Identifier '_match_val' has already been declared`**!
3. **[CRITICAL] Missing Parentheses on `TsCall` / `TsPropertyAccess` / `TsIndexAccess` (`TypeScriptAST.v:101-126, 134`)**
   - Calling an arrow expression `(\x: Integer => x)(42)` emits `(x) => { return x; }(42)` -> **`SyntaxError`**. Accessing a property on an object literal `{a: 1}.a` or arrow fails similarly.
4. **[CRITICAL] Unescaped String Literals & `TsThrow` Injection (`TypeScriptAST.v:72, 98, 149`)**
   - `TsStringLiteral` wraps raw `s` in `"` without escaping literal newlines or quotes; `TsThrow` wraps `msg` in `'...'` without escaping `'` or `\n`.
5. **[HIGH] Legacy Callee-Name Hijacking in `stringify_ts_expr` (`TypeScriptAST.v:105-125`)**
   - Calling any function whose stringified callee is `"if"`, `"def"`, or `"\"` (e.g. a function or parameter named `def` or `if`, or property) rewrites the call expression into a raw `if (...) { ... }` or `function ...` statement inside an expression!
6. **[HIGH] Broken Modules & Functors in TypeScript (`Backend.v:564, 584-592, 825-835, 860-873`, `Elaborator.v:2966-2993`)**
   - `AstModule` inside `emit_ts_block` (`lines 825-831`) builds `emit_body` and **throws it away**, emitting `; return <name>;`!
   - Inside `namespace M { ... }` (`lines 860-866`), `AstLet` (`const`), `AstEnum` (`{ const ... }`), and nested `AstModule` (`namespace`) lack `export`, making `M.c`, `M.Ctor`, and `M.Sub` inaccessible outside `M`.
   - **All 3 Backends — Functor Application Unbound Parameter (`Elaborator.v:2966-2993`)**: `FunctorAppCST` (`module N = F(M)`) substitutes the formal parameter `X` in the *export signature* `ex0` (`subst_exports`), **but leaves the runtime `body` AST untouched (`AstFieldAccess (AstRef "X") ...`)**, causing unbound `X` / `X_foo` errors in TS, Go, and Rocq!
7. **[MEDIUM] `AstAssign` in `emit_ts_block` (`Backend.v:814`) lacks `return Unit`; `AstLet` in `emit_ts_expr` (`line 528`) traps binding in a dead IIFE; `AstTuple []` (`()`) emits `[]` (`line 498`) instead of `Unit` (`{}`).**

---

##### 3.2 Go Backend (`theories/Backend.v`, `theories/GoAST.v`, `bin/main.ml`, `bin/effects_runtime.ml`)
1. **[CRITICAL] Hardcoded `"f"` / `"pred"` / `"predicate"` Heuristic in `go_app_direct` Breaks Higher-Order Calls (`Backend.v:422-443`)**
   - `AstMatch` pattern binders (`lines 993, 1292`), `AstHandle` clause binders (`line 950`), and top-level `let` closures are **never added to `locals` (`GoLocalEnv`)**.
   - `go_app_direct` (`lines 429-431`) papers over this by hardcoding `if name = "f" || name = "predicate" || name = "pred" then false else true`!
   - **Counterexample**:
     ```chester
     enum BoxFn { case Fn(cb: (x: Integer) -> Integer) }
     def apply_box(b: BoxFn): Integer = match b { case Fn(cb) => cb(42) }
     ```
     `cb` has Go type `any`, is not in `locals`, and is not named `f`/`pred`/`predicate` -> emits direct `cb(42)` instead of `__chester_call(cb, 42)` -> **Go compile error: `cannot call non-function cb (variable of type any)`**! (Renaming `cb` to `f` makes it compile!)
2. **[CRITICAL] `go_map_return_stmt` Uses Function-Entry `body_locals`, Breaking Shadowed Return Coercions (`Backend.v:219-252, 1079-1080`)**
   - `go_map_return_stmt` checks `go_expr_known_ty sigs body_locals e` using only the outer function's parameters `body_locals`, ignoring inner `var x any` bindings from `match` or `let`!
   - **Counterexample**:
     ```chester
     def pick(x: Integer, opt: Option(Integer)): Integer =
       match opt { case Some(x) => x; case None => x }
     ```
     In `case Some(x) => x`, pattern binder `x` has Go type **`any`**, but `body_locals` says `("x", "int")`. `go_wrap_ret` skips `__chester_as_int` and emits `return x` -> **Go compile error: `cannot use x (variable of type any) as int value in return statement`**!
3. **[CRITICAL] Raw Backtick Strings Break Escape Parity With TS and Fail Compilation on Backticks (`GoAST.v:14, 82, 98, 110, 130`, `bin/lexer.ml:66-80`)**
   - `bin/lexer.ml:69-75` keeps raw escape sequences (`\n` = 2 chars `\` + `n`, `\"` = 2 chars `\` + `"`) in `AstStringLit`.
   - `GoAST.v:110` emits Go **raw backtick literals** `` `...` `` where escapes are NOT interpreted!
   - **Counterexample 1**: `"a\nb"` has length **3** in TypeScript (newline) and length **4** in Go (literal `\` and `n`)! `"\""` is 1 char `"` in TS and 2 chars `\"` in Go!
   - **Counterexample 2**: Any string containing `` ` `` (e.g. `` "hello `world`" ``) emits `` `hello `world`` `` -> **Go syntax error / code injection**!
4. **[CRITICAL] `AstModule` Drops `sigs`, Breaks Intra-Module Calls, and Silently Drops `AstEnum`/`AstLet` — Breaking `import Foo` (`Backend.v:1027-1061, 1411-1455`, `bin/main.ml:163`)**
   - `emit_go_nested_module_decls` (`Backend.v:1418-1424`) renames `def a` inside `module M` to `M_a`, **without renaming intra-module calls `a()` inside sibling `def b`**, and passes `sigs = []` (`line 1423`), breaking both intra-module calls (`undefined: a`) and argument coercions to top-level functions!
   - `AstEnum`, `AstLet`, `AstExtension`, and `AstImport` inside `AstModule` are **silently dropped** (`Backend.v:1044, 1435`), and `module_values` omits nested submodules (`X.Sub` panics in `__chester_field`).
   - Since `bin/main.ml:163` desugars `import Foo` (`FileImportCST`) into `ModuleCST`, **importing any Chester file containing an `enum` or two functions calling each other fails to compile in Go**!
5. **[HIGH] Nested `PatVar` Match Redeclares `_match_val` in Same Go Block (`Backend.v:1297-1302`)**
   - `match x { case a => match a { case b => b } }` emits two `var _match_val any = ...` in the same block -> **Go compile error: `_match_val redeclared in this block`**.
6. **[HIGH] Missing `GoReturn` in `emit_go_block` (`Backend.v:1355-1404`)**
   - `AstAssign`, `AstModule`, `AstImport`, `AstSignature`, `AstFunctorApp`, `AstTypeDecl`, and `go_preamble_surface` `AstDef` as the tail of a function body emit statements **with no `return`**, causing **Go compile error: `missing return`**.
7. **[HIGH] Unchecked Type Assert in `PatConstructor` Match (`Backend.v:986, 1210, 1285`)**
   - Emits `_match_val.(map[string]any)["_tag"] == ...` without comma-ok; matching an `any` value against `PatConstructor` followed by `PatWildcard` panics if `_match_val` is not a `map[string]any`.
8. **[MEDIUM] `rename_chester_main` Substring Replacement Corrupts String Literals Containing `"func main("` (`bin/main.ml:34-57`, `bin/effects_runtime.ml:626`)**.

---

##### 3.3 Rocq Backend (`theories/RocqBackend.v`, `theories/RocqAST.v`, `bin/effects_runtime.ml`)
1. **[CRITICAL] Broken Rocq Preamble: `int_add a b := a` and Missing Primitives (`bin/effects_runtime.ml:649-677`)**
   - Line `671` literally defines: **`Definition int_add (a b : chester_dyn) : chester_dyn := a.`** (`2 + 3` = `2`!).
   - `int_sub`, `int_mul`, `int_div`, `int_mod`, `int_eq`, `int_lt`, `bool_and`, `bool_or`, `bool_not`, `string_concat`, etc. are **completely missing** from `rocq_effects_preamble`.
2. **[CRITICAL] Double Parameter Wrapping in `AstModule` Breaks Every Multi-Arg Module Function (`RocqBackend.v:201-204, 210-212`)**
   - Emits `RocqDefinition dname (map fst params) (emit_rocq_lam_params (map fst params) (emit_rocq_expr bd))` -> `Definition f x : chester_dyn := (fun x : chester_dyn => ...).` — **doubling the binders** and failing Rocq typechecking (`chester_dyn -> chester_dyn` != `chester_dyn`)!
3. **[CRITICAL] `AstDef` Always Emits `Definition` (Never `Fixpoint`), `AstEnum` Emits `RocqEmpty`, `AstRecord` Emits Empty Inductive (`RocqBackend.v:185-189`)**
   - Every recursive function, every `enum` / `match`, and every `record` / field access fails to compile in Rocq.
4. **[HIGH] Colliding Top-Level Names `_app`, `_if`, `_match`, `_tuple` (`RocqBackend.v:241-309`) & Unescaped Strings/Comments (`RocqAST.v:55, 108`)**
   - Two top-level expressions of the same kind emit duplicate `Definition _app` -> Rocq error `_app already exists`. Strings containing `"` and comments containing `*)` or `AstUniverse`/`AstMeta` inside expressions break Rocq syntax.

---

##### 3.4 `Extract.v` & Hygiene Counter Overflow Bugs (`theories/Extract.v`, `theories/Elaborator.v`)
1. **[CRITICAL] `Elaborator.string_of_nat` Maps All $n \ge 10$ to `"X"` (`theories/Elaborator.v:12-16, 47, 252, 2962`)**
   - `string_of_nat` only handles `0..9` and returns `"X"` for **every number $\ge 10$**!
   - **Impact 1 (Generative Functors Broken, `line 2962`)**: Once the elaborator state counter `n >= 10` (after ~10 fresh metas/row-vars), **every generative functor application `F(A)`, `F(B)` receives the identical name `F_gX`**, colliding in Go (`F_gX_foo redeclared`) and TS!
   - **Impact 2 (Macro Hygiene Collisions, `line 47`)**: Hygiene scope IDs $\ge 10$ all mangle to `_X` (`x_10` and `x_11` both become `x_X`).
2. **[HIGH] `ExtrOcamlNatInt` 63-Bit Signed Overflow Corrupts Large Literals (`theories/Extract.v:19`, `theories/Elaborator.v:26-36`, `theories/Backend.v:330-340`)**
   - `Extract.v:19` extracts `nat` to OCaml `int` via `ExtrOcamlNatInt` (unverified axiom mapping).
   - For integer literals $\ge 2^{62}$ (`4611686018427387904`) or `max_int` (`4611686018427387903`), `string_to_nat` / `S n` wraps to a **negative OCaml `int`**, causing `nat_to_string_fuel (S n)` (`if fuel <= 0 then ""`) to return the **empty string `""`**, emitting invalid syntax (`var x int = ;`)!
   - Also `bin/lexer.ml:68-76` crashes with `Invalid_argument "String.sub"` on an unterminated `"` at EOF.

---

## 3. Self-hosted compiler (`self-hosted/`)

All 9 files in `self-hosted/` (8,742 lines total) and `AGENTS.md` were read line-by-line and cross-referenced against `theories/*.v`, `bin/*.ml`, `stdlib/std.chester`, `flake.nix`, and `test/test_compiler.ml`.

---

#### Executive Summary

The self-hosted compiler is **not** a faithful port of the verified Rocq compiler (`theories/`). Instead, it is an ad-hoc prototype tuned to pass the 28 positive fixtures and 5 hand-picked negative fixtures in `test/test_compiler.ml:315-380`, while bypassing or stubbing core invariants:

1. **`CoreChecker` (`theories/CoreChecker.v`, 2,125 lines) is completely absent**, and **`zonk` (`elaborator.chester:2884`) is dead code that is never called anywhere**.
2. **`unify` (`elaborator.chester:908-1008`) has return type `Unit` and cannot fail**: incompatible types hit `case _ => ()` and silently succeed; effect row checks literally execute `if effect_rows_compatible(eff1, eff2) then () else ()` (lines 984, 994). `unify_ok` is a band-aid called **only** on the 3 literal cases (`StringLiteral`, `IntegerLiteral`, `BoolLiteral`) to pass `literal_mismatch_bad.chester` and `higher_order_bad.chester`.
3. **Every top-level/block `def` is bound in `env` to its *return type* instead of `AstFunTy` (`elaborator.chester:2367-2372`)**: as a result, **every call to a user-defined `def` skips parameter type checking, skips arity checking, drops all latent effect annotations (`/ {e}`), and types the call result as an unsolved `AstMeta`**.
4. **Critical miscompilations in `expander.chester` and `codegen_go.chester`**:
   - `!=` is rewritten to `int_eq` (`expander.chester:1309`), inverting `a != b` into `a == b`.
   - Operator precedence in `pratt_loop` (`expander.chester:950`) is completely broken by a `case _` wildcard placed **before** `case RelLooser(_)` and `case RelIncomparable(_)`, plus unapplied enum constructors `RelResult.RelSameL` / `RelResult.RelSameR` (`expander.chester:1410-1416`): `1 + 2 * 3` evaluates to `9` instead of `7`.
   - Non-tail `if` statements in blocks (`codegen_go.chester:1004-1010`) emit `return` inside both branches with `ret_ty = "any"`, prematurely returning from the enclosing function (or failing `go build` when the enclosing function returns `int`/`string`/`bool`).
   - **Stage 2 string-escaping corruption & Stage 3 bootstrap failure** (`codegen_go.chester:485-497` + `lexer.chester:15-23`): Stage 1 (built by Rocq using raw backtick Go strings) and Stage 2 (built by Stage 1 using double-quoted Go strings) have incompatible runtime representations of `"\\"`, `"\""`, and `"\n"`. The installed Stage 2 binary (`$out/bin/chester`) double-escapes `\n` into literal `\\n` in user strings and emits raw unescaped newlines inside `"..."` on multi-line string literals—meaning **Stage 2 cannot compile `self-hosted` to Stage 3 (`go build` fails with `newline in string`)**.

---

#### Top P0 / Critical Findings

##### C1. `[CRITICAL]` `!=` Miscompiles to Equality (`int_eq`)
- **Location**: `self-hosted/expander.chester:1308-1309`
- **Code**:
  ```chester
  else if string_eq(op, "==") then "int_eq"
  else if string_eq(op, "!=") then "int_eq"
  ```
- **Impact**: Every expression `a != b` compiled by `self-hosted` is silently rewritten to `int_eq(a, b)` (`a == b`), producing the exact opposite boolean value at runtime. (In Rocq `theories/Expander.v:50-60`, neither `==` nor `!=` is in `op_to_fn`.)

##### C2. `[CRITICAL]` Operator Precedence Is Completely Broken (`1 + 2 * 3 == 9`)
- **Locations**: `self-hosted/expander.chester:935-977`, `1313-1326`, `1385-1419`
- **Root causes (three compounding bugs)**:
  1. **Unreachable `RelLooser` / `RelIncomparable` in `pratt_loop` (`lines 950-976`)**:
     ```chester
     match rel {
       case RelTighter(_) => PrattResult.Parsed(lhs, elems);
       case RelSameL(_) => PrattResult.Parsed(lhs, elems);
       case RelSameR(_) => { ... };
       case _ => PrattResult.Parsed(lhs, elems);          // <-- LINE 950 MATCHES EVERYTHING!
       case RelLooser(_) => { ... };                      // <-- DEAD CODE
       case _ => PrattResult.Parsed(lhs, elems);          // <-- DUPLICATE WILDCARD
       case RelSameR(_) => { ... };                       // <-- DUPLICATE ARM / DEAD CODE
       case _ => PrattResult.Parsed(lhs, elems);          // <-- TRIPLICATE WILDCARD
       case RelIncomparable(_) => PrattResult.PrattError("Ambiguous precedence") // <-- DEAD CODE
     }
     ```
     When parsing `1 + 2 * 3` (even with `stdlib/std.chester`'s `infixl "*" tighter_than "+"`), `get_relation("+", "*", op_env)` returns `RelLooser(0)`, which hits line 950 `case _` and immediately reduces `1 + 2` instead of shifting `*`, producing `int_mul(int_add(1, 2), 3)` = `9`.
  2. **Precedence integers in `builtin_op_fixity` (`lines 1313-1326`) are ignored**: `InfixL(10)`, `InfixL(20)`, `InfixL(5)` store precedence numbers, but `lookup_op`, `check_env_for_looser`, and `is_tighter_than` (`lines 1332, 1378, 1391`) bind `let _dummy_fixity = fixity;` and never inspect the integer. Without `stdlib`'s `infixl` declarations, `+` and `*` return `RelIncomparable(0)`, which again hits line 950 `case _` and parses strictly left-to-right.
  3. **Unapplied enum constructors for arity-1 variants (`lines 1410-1416`, `656`)**: `RelSameL(d: Integer)` and `RelSameR(d: Integer)` (`lines 535-536`) take 1 argument, yet lines 1410-1416 return bare `RelResult.RelSameL` / `RelResult.RelSameR` (Go `func(_f0 any) any` function values rather than `map[string]any` tagged values!). Matching on `rel` fails `_ok := _match_val.(map[string]any)` and falls into `case _`, breaking right-associative `infixr` operators too. Line 656 similarly returns bare `ZipResult.ZipFailed` without `(0)`.

##### C3. `[CRITICAL]` Stage 1 vs Stage 2 String Escaping Divergence & Broken Stage 3 Bootstrap
- **Locations**: `self-hosted/codegen_go.chester:485-497, 662`, `self-hosted/lexer.chester:15-23, 152-156`, vs `theories/GoAST.v:14, 110`
- **Mechanism**:
  - Neither `bin/lexer.ml` nor `self-hosted/lexer.chester` unescapes string literals (`"\n"` is stored in `AstStringLit` as two characters `\` and `n`; `"\""` is stored as `\` and `"`).
  - Rocq's Go backend (`theories/GoAST.v:14, 110`) emits Go **raw backtick strings** (`` `...` ``), so in `stage1`, `"\\"` in `escape_go_string` (`codegen_go.chester:489`) is a **2-character string** `` `\\` ``. Comparing 1-char `c = string_char_at(s, idx)` against 2-char `"\\"` or `"\""` is **always `false` in Stage 1**!
  - `self-hosted/codegen_go.chester:662` emits Go **double-quoted strings** (`"..."`). Because Stage 1's `escape_go_string` fails to match `"\\"` and `"\""`, Stage 1 passes raw `\` and `"` through into `stage2.go`, where the Go compiler unescapes them!
  - Consequently, in **Stage 2** (`$out/bin/chester` installed by `flake.nix:125`):
    1. `"\\"` at `codegen_go.chester:489` is now a 1-char `\`, so Stage 2 **double-escapes** every escape sequence in user strings: `"hello\n"` (lexed as `hello` + `\` + `n`) becomes `"hello\\n"`, printing literal backslash-`n`, and `"\""` becomes `"\\\""` (2 chars `\"` instead of `"`).
    2. `"\n"`, `"\r"`, `"\t"` at `codegen_go.chester:491-493` were compiled by Go in `stage2.go` into **raw ASCII control characters (LF 10, CR 13, TAB 9)**. When Stage 2 compiles any multi-line string literal (such as `self-hosted/lexer.chester:2-3`!), `escape_go_string` replaces LF with raw LF inside `"..."`, emitting invalid Go (`syntax error: newline in string`). **Stage 2 cannot compile `self-hosted` to Stage 3.** (Also note: Rocq's raw backtick strings break if a Chester string literal contains `` ` ``.)

##### C4. `[CRITICAL]` `map_elabs` Binds Every `def` to Its Return Type Instead of `AstFunTy`
- **Location**: `self-hosted/elaborator.chester:2367-2385` (vs `theories/Elaborator.v:1637-1641`)
- **Code**:
  ```chester
  case DefCST(name, _, _, ret_ty, _, span) => {
    let tyAst_res = elaborate(env, ret_ty, OptionAST.SomeAST(AST.AstRef("TypeUniverse")));
    match tyAst_res {
      case MkPair(tAst, _) => {
        let new_env = list_insert_first(env, Pair.MkPair(Pair.MkPair(name, span_context(span)), tAst));
        let res = elaborate(env, x, OptionAST.NoneAST);
  ```
- **Impact**:
  1. `def foo(x: Integer): String = "hi"` binds `foo` in `new_env` to `AstRef("String")` (or `AstApp(AstRef("#effect_row"), ...)` when effect-annotated!) instead of `snd res` (`AstFunTy`).
  2. Every subsequent call `foo(arg1, arg2, ...)` sees `fTy = AstRef("String")` in `elab_check_args` (`lines 2217-2231`), calls `unify_top(AstRef("String"), AstPi("x", ?m1, ?m2, []))` (which is a silent no-op!), checks arguments against unconstrained fresh metas, allows **any number of arguments of any types** (`foo(true, 1, "extra")` succeeds!), and types `foo(...)` as an **unsolved `AstMeta`**.
  3. **Interprocedural effect checking is completely broken**: calling `def boom(): Unit / { MyEff } = perform op()` charges **zero effects** to the caller (`charge_call_effs` at `lines 1174-1179` sees `AstApp("#effect_row", ...)` instead of `AstFunTy` and returns `[]`). Unhandled effectful calls compile cleanly and panic at runtime!
  4. Chained extension methods on call results (`foo().length()`) fail to resolve `StringOps_length` because `foo()` has type `AstMeta` instead of `AstRef("String")`, emitting invalid Go `foo().length()`.

##### C5. `[CRITICAL]` `unify` Cannot Report Errors + Total Absence of `CoreChecker` & `zonk`
- **Locations**: `self-hosted/elaborator.chester:895-1008`, `2884-2909`, `3834-3849`
- **Details**:
  - `unify(fuel, t1, t2): Unit` (`lines 908-1008`) returns `Unit`, has no error channel in `ElabState`, performs **no occurs check** (`?m = List(?m)` creates a cycle), ignores `AstFunTy` parameter lists (`lines 990-995`), never unifies `AstFunTy` with `AstPi`, and turns `effect_rows_compatible` into `if effect_rows_compatible(eff1, eff2) then () else ()` (`lines 984, 994`).
  - `unify_ok` (`lines 895-898`) is called **only** on `StringLiteral`, `IntegerLiteral`, and `BoolLiteral` (`lines 2966, 2975, 2984`)—never on `Symbol`, `AppCST`, `Block`, `IfCST`, `MatchCST`, or `DefCST`.
  - `Block` (`line 3019`) drops `expected` (`elaborate(final_env, tail, OptionAST.NoneAST)`), so **even literal mismatches inside braces (`def main(): Integer = { "hi" }`) are accepted!**
  - `zonk` (`line 2884`) is **never called anywhere**, and `CoreChecker` (`theories/CoreChecker.v`) is completely absent.
  - **Concrete invalid programs silently accepted by `self-hosted` (exit status 0)**:
    - `def bad(s: String): Integer = s;` (variable type mismatch -> invalid Go / runtime panic)
    - `def main(): Integer = { "hi" };` (block drops expected type -> invalid Go)
    - `tests/typeparam_rigid_bad.chester`: `def leak[T](x: T): Int = x;`
    - `tests/call_non_function_bad.chester`: `def main(): Integer = 1(2);`
    - `tests/duplicate_def_bad.chester`: duplicate `def f(): Integer`
    - `tests/unknown_type_bad.chester`: `def f(x: Missing): Integer = 1;`
    - `tests/unknown_effect_bad.chester`: `def f(): Integer / {nope} = 1;`
    - `tests/typeargs_arity_bad.chester`: `def main(): Integer = List(Int, Int);`
    - `tests/typeargs_mismatch_bad.chester`: `Some[String](1)`
    - `tests/match_non_exhaustive_bad.chester`: `match Some(1) { case Some(x) => x }`

##### C6. `[CRITICAL]` Non-Tail `if` Statements in Blocks Emit Premature `return` / Invalid Go
- **Location**: `self-hosted/codegen_go.chester:1004-1010` (vs `theories/Backend.v:1202`)
- **Code**:
  ```chester
  // In emit_go_expr_stmt (for middle, non-tail statements in a block):
  case AstIf(cond, then_b, else_b) => {
    ...
    let t_str = emit_go_stmt(next_fuel, sigs, locals, go_iface(), then_b);
    let e_str = emit_go_stmt(next_fuel, sigs, locals, go_iface(), else_b);
    string_concat("if ", ... t_str ... e_str ...)
  };
  ```
- **Impact**: Passing `ret_ty = go_iface()` (`"any"`) instead of `""` (or wrapping in an IIFE like Rocq `Backend.v:1202`) causes both branches of any non-tail `if` statement to emit `return ...;` directly inside the enclosing Go function!
  - Example: `def main(): Integer = { var x = 0; if true then { x = 1; } else { x = 2; }; x }` emits `return nil;` inside `func chester_main() int`, failing `go build` (`cannot use nil as int value in return statement`), and in `Any`/`Unit`-returning functions it exits the function early, skipping `x` and all subsequent statements!

---

#### File-by-File Technical Audit

##### 1. `self-hosted/ast.chester` (47 lines)
- **`[HIGH]` Stringly-typed `EffectSet` causes soundness bug with `"mu*"` user effects (`lines 18, 23, 24, 33, 34, 41` + `elaborator.chester:1132-1137`)**:
  - Rocq (`theories/AST.v:29-33`) uses `Inductive EffectRef := BuiltinEffect | UserEffect | EffectRowVar`.
  - Self-hosted uses `List(String)` and detects open-row variables via `string_substring(s, 0, 2) == "mu"` (`elaborator.chester:1135`). Any user-declared effect whose name begins with `"mu"` (e.g., `effect mutex { def lock(): Unit }`, `effect music { ... }`) is treated as an open row variable (`is_row_var == true`), completely bypassing handler/annotation checks in `check_pending` (`line 3838`) and `effect_row_subsumes` (`line 2062`) and panicking at runtime!
- **`[MEDIUM]` Unparameterized `Pair` and `List`-as-`Option` representations (`lines 23-28, 33, 37, 40-43`)**:
  - Uses untyped `List(Pair)` instead of `List(Pair(String, AST))`, `List(Pair(PatternAST, AST))`, etc., and encodes `option AST` as `List(AST)` on `AstModule.seal` (`line 37`) and `AstTypeDecl.def` (`line 42`), diverging from `theories/AST.v:138, 145`.

##### 2. `self-hosted/cst.chester` (109 lines)
- **`[HIGH]` `LamCST` encodes `None` parameter type as `CST.Error("None", sp)` (`line 32` + `expander.chester:1017` + `elaborator.chester:3188`)**:
  - Rocq uses `option CST`. Self-hosted uses `opt_arg_ty: CST`, passing `Error("None", sp)` when omitted, and `elaborator.chester:3188` matches `case Error(_, _) => fresh_meta()`—which silently swallows *real* parse errors in lambda parameter types as fresh metavariables!
- **`[MEDIUM]` Stripped `Span` loses file, line, column, and UTF-16 offsets (`lines 2-13` vs `theories/CST.v:8-31`)**:
  - Self-hosted `Span(start, end_idx, context)` cannot report file names or `line:col` diagnostics.
- **`[LOW]` Incomplete `cst_size` measure (`lines 95, 99, 102` vs `theories/CST.v:205-218`)**:
  - `ModuleCST` ignores `params` and `seal` sizes; `SigValCST` ignores `params` sizes; `SigWithCST` ignores `eqs` sizes. Also `DefCST` (`line 31`) keeps raw `List(CST)` params instead of `list (string * CST)`, and `MatchCST` (`line 36`) keeps raw `List(CST)` instead of `list (PatternCST * CST)`.

##### 3. `self-hosted/lexer.chester` (202 lines)
- **`[CRITICAL]` `get_cr_char()` returns LF (`\n`), breaking all CRLF (`\r\n`) files (`lines 10-14, 41-47, 88-94`)**:
  - Lines 11-12 contain a literal LF newline with AI-slop comment `// We can just use newline for CR, or ignore it. Let's just use string_char_at fallback?`.
  - Consequently `is_whitespace("\r")` is `false`, `is_symbol("\r")` is `false`, and **`is_alpha("\r")` (`lines 88-94`) is `true`!** Every identifier before `\r\n` has `\r` appended to its name (`"foo\r"`), and standalone `\r` is lexed as an `Identifier` token!
- **`[HIGH]` Stack overflow / silent character truncation on unterminated strings (`lines 114-121, 152-156`)**:
  - If a file ends with `\` inside a string (`"abc\`), `lex_string` (`line 118`) jumps to `idx + 2 = len + 1`, bypassing `int_eq(idx, len)` (`line 115`) and recursing infinitely until stack overflow.
  - If a file ends with `"abc` (no closing quote), `lex_string` returns `len`, and `line 154` slices `string_substring(source, idx + 1, end_idx - 1)`, silently dropping `'c'` and emitting a valid `StringLit` instead of an error; a lone `"` at EOF panics Go slice bounds (`start > end`).
- **`[HIGH]` Missing operator characters in `is_symbol` (`lines 61-87`)**:
  - Omits `%`, `&`, `|`, `@`, `~`, `^`, `#`, `?`. Because `is_alpha` is defined by negation (`not ws && not digit && not symbol`), **all of those characters are lexed as identifier letters!**
  - Example: `%` is a stdlib operator (`stdlib/std.chester:161`), yet `a%b` or `x%2` without spaces lexes as a **single identifier `"a%b"` / `"x%2"`**! Similarly `a&&b`, `a||b`, and `@@` (`test_compiler.ml:406`) lex as single identifiers instead of symbol tokens.
- **`[MEDIUM]` Comments are discarded (`lines 159-161` vs `bin/lexer.ml:32-42`)**:
  - `TokenKind.CommentToken` (`line 30`) is dead: `lex_helper` drops `//` comments completely, making comment preservation in `formatter.chester` impossible. (And if `CommentToken` were emitted, `parser.chester:316` lacks a match arm for it and would panic!)
- **`[LOW]` Tokenization divergences & dead stubs**:
  - `lexer.chester:171-179` lexes `<=`, `>=`, `!=` as 2-char `SymToken`s, whereas `bin/lexer.ml:81-97` splits them into two 1-char `TokSym`s.
  - `check_keyword` (`lines 129-133`: `if string_eq(text, "def") then Identifier else Identifier`), `TokenKind.Whitespace` (`line 29`), and `lexer_main` (`lines 199-202`) are dead/no-op leftovers. `lex_helper` uses `O(N^2)` `list_append` per token.

##### 4. `self-hosted/parser.chester` (400 lines)
- **`[HIGH]` All compound CST nodes hardcode `Span(0, 0, [])` (`lines 20, 35, 61, 86, 101, 127, 166, 182, 217, 268, 340-374`)**:
  - Destroys source spans and breaks `formatter.chester:241-245` (`cst_adjacent`), which sees `Span_start == 0` on every `Tuple(...)` and inserts bogus spaces into function calls (`foo (x)` instead of `foo(x)`).
- **`[MEDIUM]` Empty statements / double semicolons (`let x = 1;;`) fail to elaborate (`lines 178, 275, 361` vs `theories/Parser.v:242`)**:
  - Rocq skips redundant `;` and `,`. Self-hosted `parse_atom_sequence` sees `;` as a terminator, consumes 0 tokens, produces `SeqOf([])`, and appends it to block statements—causing `elaborator.chester:2993` to fail with `Empty SeqOf`. Similarly `(, a)` is accepted as `[SeqOf([]), a]`.
- **`[MEDIUM]` No error recovery & unconsumed tokens ignored (`lines 12-394` + `cli.chester:39-40`)**:
  - Lacks Rocq's `sync` recovery (`Parser.v:46-57`), and `cli.chester:40` matches `case MkParseResult(cst, _)` ignoring remaining unconsumed tokens.
- **`[LOW]` Severe copy-paste duplication & dead `main_parser`**:
  - Lines 21-45 / 48-71, 88-111 / 114-137, and 168-199 / 203-234 are verbatim duplicated 25-to-30-line blocks; `filter_whitespace` (`lines 329-336`) filters a token kind the lexer never emits; `main_parser` (`lines 395-400`) is dead test scaffolding.

##### 5. `self-hosted/expander.chester` (2,512 lines)
Beyond **C1** (`!=` -> `int_eq`) and **C2** (broken Pratt precedence & unapplied `RelSameL`/`RelSameR`/`ZipFailed`):
- **`[HIGH]` Match arm bodies without `{ ... }` are NEVER expanded (`lines 68, 2453-2458`)**:
  - `extract_cases` (`lines 39, 53, 68, 83, 96`) packs multi-token arm bodies into `SeqOf(body_elems)`, and `expand_cst` on `MatchCST` (`lines 2453-2458`) only expands `expr`, **never `cases`**!
  - Concrete reproducer: `match Some(1) { case Some(x) => x + 1; case None => 0 }` fails with **`Elaboration error: Unbound variable: +`** because `SeqOf([x, +, 1])` reaches `elaborator.chester:2992` unexpanded and is treated as calling `x` on `+` and `1`! Same for `case Some(x) => if c then 1 else 2`, lambdas, or macro calls in match arms.
- **`[HIGH]` Variable assignment `x = expr` fails on multi-token RHS (`lines 2216-2232` vs `theories/Expander.v:1202-1206`)**:
  - Only recognizes `AssignCST` when `int_eq(list_length(elems), 3)` after `collapse_apps`!
  - Concrete reproducer: `var x = 0; x = x + 1;` has `list_length(elems) == 5` (`[x, =, x, +, 1]`), misses `AssignCST`, fails Pratt parsing on `=`, and fails elaboration with **`Unbound variable: =`**!
- **`[HIGH]` Arrow types `(x: A) -> B` / `A -> B` are NOT expanded (`lines 994-1148` vs `theories/Expander.v:272-308, 432-439`)**:
  - `expander.chester` has no `->` expansion at all. Only `def` value parameters have a workaround in `elaborator.chester:2547`. Arrow types in return types (`def f(): (x: Int) -> Int`), `type` aliases (`type F = (x: Int) -> Int`), `signature` members (`SigValCST`), `Option((x: Int) -> Int)`, or nested arrows fail to elaborate.
- **`[HIGH]` Chester file imports (`import Math`, `import Math from "math.chester"`) are missing & miscompiled (`lines 2136-2184` vs `theories/Expander.v:536-553`)**:
  - `FileImportCST` (`cst.chester:53`) is never constructed anywhere! Worse, `import Math from "math.chester"` matches `Symbol(lang="Math") :: Symbol(alias="from") :: StringLiteral("math.chester")` (`lines 2139-2160`) and silently miscompiles into an FFI `ImportCST("Math", "from", "math.chester", [])`!
- **`[HIGH]` Macro hygiene & substitution skip half the CST + truncate macro bodies (`lines 551-575, 616-638, 1222-1278`)**:
  - `add_mark_cst` and `subst_cst` omit `VarCST`, `AssignCST`, `DoCST`, `HandleCST`, `BoxCST`, `UnboxCST`, `ExtensionCST`, `ModuleCST`, `SignatureCST`, `PackCST`, `UnpackCST`, etc. (`case _ => c`).
  - `extract_macro_cases` (`lines 1240, 1246, 1258`) grabs only `list_get(elems, 4)` / `list_get(elems, 3)` without bounds checks, silently truncating multi-token macro bodies like `case m(x) => x + 1` to `x`, and turning any unrecognized syntax into a catch-all `PatWildcardCST`!
- **`[MEDIUM]` Unchecked `list_get` index panics on malformed surface forms**:
  - `extension` (`lines 1523, 1534`), `enum` (`line 1566`), `record` (`line 1643`), `macro` (`lines 1674-1675`), `effect` (`line 1711`), `perform` (`line 1787`), `handle` (`lines 1067, 1801, 1827-1833`), `def` (`line 1859`), `let` (`lines 1958-1959`—doesn't even check `elems[2] == "="`!), and `var` (`lines 1989-1990`) index into `elems` without checking `list_length`, panicking the compiler on malformed input.
- **`[MEDIUM]` `record R(T)(...)` delimiter role broken (`lines 1642-1672` vs `AGENTS.md` & `theories/Expander.v:1182`)**:
  - Only recognizes `record R[T](...)` (`ImplicitAppCST`), rejecting standard `record R(T)(...)`.
- **`[LOW]` Hardcoded fuel `100` (`lines 1400-1401`), TS mock comment (`line 690`), `List[CST]` delimiter violation (`line 318`), `_dummy_*` slop (`lines 922-923, 1171, 1228, 1332, 1378, 1391, 1407-1408`)**.

##### 6. `self-hosted/elaborator.chester` (3,849 lines)
Beyond **C4** (`map_elabs` binding `def` to `ret_ty`) and **C5** (no-op `unify`, dead `zonk`, missing `CoreChecker`):
- **`[HIGH]` `is_allowed_unbound` treats EVERY capitalized identifier as a valid value of type `Any` (`lines 1091-1097, 2945`)**:
  - Combined with `map_elabs` omitting `EnumCST` and `ExtensionCST` (`lines 2322-2459` vs `theories/Elaborator.v:1668-1721`), **enum constructors are never registered in `env` at all!**
  - Any typo or non-existent capitalized identifier (`FooBarBaz(1, 2, 3)` or `NonExistentValue`) elaborates without error as `AstRef` of type `Any`, failing only when `go build` sees an undefined Go identifier!
- **`[HIGH]` Type applications `Option(t)` / `List(M.t)` fail on lowercase or module-qualified type arguments (`lines 3231-3315` vs `theories/Elaborator.v:1891-1904`)**:
  - `AppCST` is missing `if is_type_expected(expected)`! When elaborating `Option(t)` or `Option(M.t)` in a type annotation, `AppCST` elaborates `Option` and its type arguments with `OptionAST.NoneAST` instead of `SomeAST(TypeUniverse)`. For `M.t`, `FieldAccess` (`line 3372`) sees `is_type_expected(NoneAST) == false` and errors with **`type component used as value: t`**; for abstract/lowercase `t`, `Symbol` (`line 2947`) errors with **`Unbound variable: t`**!
- **`[HIGH]` Polymorphism / generics are completely unimplemented (`lines 3132, 3277-3296, 3388`)**:
  - No `instantiate_funty` / `freshen_fun_tps` (`theories/Elaborator.v:110-131`).
  - `AppCST(ImplicitAppCST(inner_func, targs), args)` (`lines 3277-3296`) discards `_targsAst` without checking type-argument arity or substituting `targs` into parameter/return types (`Some[String](1)` is accepted).
  - `ExtensionCST` (`lines 3388-3404`) neither binds `tparams` in `env` nor prepends `tparams` to extension methods (unlike `Elaborator.v:3290-3316`).
- **`[HIGH]` Module & Signature bodies do NOT thread `env` across declarations (`lines 2868-2876, 3525, 3643` vs `theories/Elaborator.v:2448-2475, 2603-2622`)**:
  - `ModuleCST` and `SignatureCST` call `elab_stmt_list(body_env, body)`, which passes the **same unchanged `body_env`** to every statement without `collect_def_binders`!
  - Concrete reproducer: `module M { def a(): Int = 1; def b(): Int = a() }` fails with **`Elaboration error: Unbound variable: a`**! Similarly, in `signature S { type t; def f(x: t): t }`, `t` is not in `env` when elaborating `f` (it only works if `t` is bare `Symbol("t")` under `SomeAST(TypeUniverse)`—and fails on `Option(t)`!).
- **`[HIGH]` `HandleCST` hardcodes `"x"` as the `return` clause binder & drops effect op arg types (`lines 52-99, 162-199, 3428-3498`)**:
  - Line 3469 literally writes `let x = "x";` when elaborating `case return(val) => ...`, ignoring the user's binder name! `handle { 1 } with E { case return(v) => v }` fails with **`Unbound variable: v`** (while `parse_hcase`'s `"return"` branch at `lines 162-170` is dead code!).
  - `elab_ops` (`lines 60, 73, 82`) discards all effect operation parameter types (`list_empty[AST]()`), `EffectCST` (`line 3417`) ignores effect type parameters (`effect State(T)`), `DoCST` (`line 3442`) never checks `perform` argument types/arity, and handler clauses (`lines 172, 203`) bind all op params and `resume`'s input to `Any`.
  - Handler clause body extraction (`lines 161, 185, 3468`) takes `list_get(elems, len - 1)`, silently dropping earlier tokens in unbraced multi-token expressions like `case op() => 1 + 2`.
- **`[HIGH]` `MatchCST` rolls back solver `subst` after every arm & never unifies arm types (`lines 2720-2756` vs `theories/Elaborator.v:2312-2378`)**:
  - Binds constructor pattern vars to unconstrained `fresh_meta()`, resets `subst` to `old_subst` after each arm (`lines 2738-2747`), and returns the last arm's type `bTy` without unifying across arms (`match c { case true => 1; case false => "str" }` when `expected == NoneAST` types as `String`!).
- **`[MEDIUM]` Broken `whnf`, `subst_ast`, `fresh_name`, and `unpack_inline` (`lines 509-592, 1282-1293`)**:
  - `whnf` (`lines 580-585`) has no fuel (diverges on omega terms) and **drops `advance(args)`** on multi-arg beta-reduction (`(\x => \y => y)(a, b)` evaluates to `\y => y`, losing `b`!).
  - `subst_ast` (`lines 541-574`) has no fuel, covers only 10 of 40 AST constructors, and `fresh_name(argName, x, argName)` (`lines 537-540, 557, 568`) fails to avoid free variables in `v` or `argBody`, allowing variable capture.
  - `unpack_inline` (`lines 1282-1293`) uses `rename_free` (which ignores `AstMatch`, `AstHandle`, `AstDef`, etc.) instead of `subst_ast` (`Elaborator.v:1429`).
- **`[MEDIUM]` `AssignCST` never checks mutability, existence, or type (`lines 2353, 3058`)**:
  - `undeclared_var = 123` and `let immut = 1; immut = "wrong_type"` both elaborate without error!
- **`[LOW]` Dead code & `AGENTS.md` delimiter violation**:
  - `zonk` / `zonk_list` (`lines 2877-2909`), `find_ext_method_any` (`lines 2827-2837`), `hs_has_op` (`lines 248-256`), `equiv_ast` (`lines 620-643`) are unused dead code; line 2937 writes `List[Pair]`.

##### 7. `self-hosted/formatter.chester` (424 lines)
- **`[MEDIUM]` Entire file is dead code in the self-hosted compiler (`cli.chester`)**:
  - Never called anywhere in `self-hosted/`; there is no `--fmt` flag or `chester_fmt` entrypoint, yet all 424 lines are compiled into `stage1` and `stage2`.
- **`[MEDIUM]` Emits `"/* EXPANDED NODE */"` for all 31 expanded CST constructors (`line 402` vs `theories/Formatter.v:275-470`)**:
  - Cannot format expanded CSTs, even though `needs_space_before_group` (`lines 256-258`) checks `AppCST`, `ImplicitAppCST`, and `FieldAccess`.
- **`[MEDIUM]` Spacing divergences & broken adjacency (`lines 169-193, 241-253` vs `theories/Formatter.v:162-175`)**:
  - Missing `":"`, `","`, `";"`, `"%"` from `spaced_group_after_symbols`, and when paired with `self-hosted/parser.chester` (which sets `Tuple` spans to `0..0`), `cst_adjacent` is always `false`, formatting `f(x)` as `f (x)`. `starts_with_let` (`lines 28-40`) is unused dead code.

##### 8. `self-hosted/codegen_go.chester` (1,110 lines)
Beyond **C3** (`escape_go_string`) and **C6** (non-tail `AstIf` emitting `return`):
- **`[CRITICAL]` Hardcoded parameter names `"f"`, `"predicate"`, `"pred"` in `go_app_direct` + missing pattern binders in `locals` (`lines 460-482, 593-617`)**:
  - Lines 601-603 literally hardcode `else if string_eq(name, "f") then false else if string_eq(name, "predicate") then false else if string_eq(name, "pred") then false` (copied from a hack in `theories/Backend.v:429-431` for `stdlib/std.chester`)!
  - Meanwhile, `emit_go_` (`lines 460-482`) **never adds `PatVar(v)` or `PatConstructor(_, vars)` bindings to `locals`**!
  - Concrete reproducer: `match Some(\x => x + 1) { case Some(cb) => cb(41); case None => 0 }` emits `var cb any = ...; return cb(41)` -> **`go build` fails: `cannot call non-function cb (variable of type any)`**—whereas renaming `cb` to `f` works only because `"f"` is hardcoded on line 601! Conversely, any top-level `def f(...)` is unnecessarily routed through `reflect.ValueOf` in `__chester_call`.
- **`[HIGH]` Non-exhaustive `match` in `emit_go_expr` panics the compiler (`lines 660-772`)**:
  - `emit_go_expr` has no `case _` wildcard and omits `AstDef`, `AstSpan`, and `AstFunctorApp`. Any local `def`, `AstSpan`-wrapped node, or functor application in expression position crashes the compiler with `panic("Non-exhaustive match")`.
- **`[HIGH]` Local `def` inside function/block emits invalid Go nested `func` declaration (`lines 937-946, 1016` vs `theories/Backend.v:1240-1244`, `GoAST.v:84-88`)**:
  - `def main(): Integer = { def inner(x: Integer): Integer = x; inner(1) }` emits `func inner(x int) int { ... }` inside `func chester_main()`, which is illegal Go syntax (Rocq uses `GoLocalFuncDecl`: `var inner any; inner = func...`).
- **`[HIGH]` Functor definitions & applications emit broken Go when functor parameter is referenced (`lines 949, 954, 1056-1076` + `elaborator.chester:3694, 3818`)**:
  - `emit_go_top_modules` (`line 1062`) emits top-level `func F_foo()` even for parameterized functors `module F(X: S) { ... }` where `X` is unbound, and `module P = F(M)` copies `F`'s body without substituting `X` -> `M`, emitting undefined `X_...` calls (`tests/modules.chester` only passed because `module Id(X: Printable)` ignored `X`!).
- **`[HIGH]` Module function calls skip Go argument coercion & return-type tracking (`lines 201-208, 370-390, 1101`)**:
  - `collect_go_sigs` (`lines 149-167`) skips `AstModule`, and `go_call_args` (`line 388`) ignores `AstFieldAccess`. Passing an `any`-typed value (`list_get([1], 0)`) to `Math.add(a: Int, b: Int)` emits `Math_add(x, 2)` without `__chester_as_int(x)`, failing `go build`!
- **`[HIGH]` `AstRecord` emits `type R struct{...}` with NO constructor, so records cannot be instantiated (`lines 832-835`)**:
  - `record Point(x: Int, y: Int); Point(1, 2)` emits Go type conversion `Point(1, 2)` instead of a struct literal or constructor function, failing `go build`.
- **`[HIGH]` `AstImport("go", ...)` emits `import "..."` after preamble functions (`lines 826-831` + `effects_runtime.ml:636`)**:
  - Emits `import "pkg";` in the middle of top-level `func` declarations, which is a Go syntax error; and Go FFI calls (`math.Abs`) outside the 6 hardcoded preamble imports (`fmt`, `reflect`, `strings`, `strconv`, `io`, `os`) fail with `undefined: math`.
- **`[MEDIUM]` `AstVar` bugs & Go redeclaration / identifier collisions**:
  - `AstVar` (`lines 695-698, 844, 994-997`): omits `_ = name;` (unused `var x = 0` fails Go build), and in expression/tail position emits `return var x int = 0` (invalid Go syntax!).
  - `AstAssign` in `emit_go_expr` (`lines 761-771`) omits `go_coerce_arg` (assigning `any` to typed `int` `var` fails Go build).
  - Same-block `let x = 1; let x = 2` emits two `var x` in the same Go block (`x redeclared in this block`).
  - Enum variants emit unprefixed global aliases (`var None any`, `func Some(...)`), colliding across enums that share a variant name; Go keywords/builtins (`type`, `map`, `range`, `len`, `error`, `string`) are never escaped/mangled.
  - Line 226 violates `AGENTS.md` delimiter rules (`locals: List[Pair]`).

##### 9. `self-hosted/cli.chester` (89 lines) + Bootstrap Quine Wrapper
- **`[HIGH]` Massive completeness gaps vs `bin/main.ml` & `docs/guide/cli-usage.md`**:
  - **Missing backends**: No `--ts` (default in `main.exe`), no `--ts-module`, no `--emit-ts-runtime`, no `--rocq`.
  - **Missing flags**: No `--prelude`, no `--module-path` / `CHESTER_PATH`, no `--go-sigs` / `--list-go-sigs`, no `--output` long flag, no `-` stdin sentinel.
  - **Broken multi-file & unknown-flag handling (`lines 31-33`)**: Passing multiple files (`chester stdlib/std.chester app.chester`) silently overwrites `in_file` with the last argument and drops all preceding files! Unknown flags like `chester --ts app.chester` silently ignore `--ts` and emit Go instead of erroring!
- **`[MEDIUM]` Naive `"func main("` string replace & quine bloat in `__chester_assemble_go` (`line 53` + `bin/effects_runtime.ml:624-646`)**:
  - Every compiled user program embeds ~430 lines of runtime preamble **plus** a full quoted quine copy of the preamble (`__chester_go_preamble`) and assembler (`__chester_assemble_go_src`), and wraps execution in `fmt.Println(chester_main())` (printing trailing `<nil>` on `Unit` programs, and printing a spurious blank line at the end of every `stage1`/`stage2` CLI run!).
  - `strings.Replace(body, "func main(", "func chester_main(", 1)` corrupts the first string literal containing `"func main("` if it precedes `def main`.
- **`[LOW]` Dead match arms & exit-code bugs (`lines 18-20, 64-74`)**:
  - Missing `-o` argument writes to stdout and returns exit code 0; lines 64-67 and 70-73 are unreachable `case _` arms on single-constructor `Pair` and `ParseResult`.

---

#### Summary of `AGENTS.md` Violations

1. **Delimiter roles (`[]` vs `()`)**:
   - `self-hosted/expander.chester:318`: `rest_ss: List[CST]` (must be `List(CST)`).
   - `self-hosted/elaborator.chester:2937`: `env: List[Pair]` (must be `List(Pair)`).
   - `self-hosted/codegen_go.chester:226`: `locals: List[Pair]` (must be `List(Pair)`).
   - `self-hosted/expander.chester:1642-1672`: `record` only accepts `record R[T](...)` and rejects `record R(T)(...)`.
   - `self-hosted/ast.chester` / `elaborator.chester` / `codegen_go.chester`: pervasive unapplied `Pair` (`List(Pair)`) instead of `Pair(A, B)`.
2. **Rocq / compiler recursion fuel**:
   - `self-hosted/expander.chester:1400-1401`: Hardcodes magic fuel `100` in `is_tighter_than(100, op1, op2, env)` instead of deriving fuel from `list_length(env)`.
   - `self-hosted/elaborator.chester:541, 575`: `subst_ast` and `whnf` omit measure-derived fuel where non-structural recursion occurs.
3. **`Unit` vs `()`**:
   - `parser.chester:166, 340, 344, 353, 371`, `expander.chester:1969, 2000`, and `elaborator.chester:156, 181, 193, 197, 3407, 3408, 3424, 3466, 3475` still construct `Symbol("Unit")` / `AstRef("Unit")` in value positions instead of `()`.

---

## 4. CLI, runtimes, stdlib, scripts, tests & examples

Every line of all requested files (`AGENTS.md`, `bin/*.ml`, `runtime/chester-runtime.ts`, `stdlib/**/*.chester`, `scripts/*`, `data/*`, `test/**/*`, `tests/*.chester`, and `examples/**/*`) has been audited. Below is the complete catalogue of findings organized by subsystem, with exact file paths, line numbers, concrete reproductions, and severity ratings.

---

#### 1. Effects & Backend Runtime Bugs (`bin/effects_runtime.ml`, `runtime/chester-runtime.ts`)

##### 1.1 [CRITICAL] One-Shot Deep & Shallow Handlers Replay `bodyFn()` from Scratch on Every `resume`, Re-Executing Side Effects & Causing $O(N^2)$ Time + $O(N)$ Stack Growth
- **Locations**:
  - `bin/effects_runtime.ml:47-88, 104-110` (TS)
  - `bin/effects_runtime.ml:440-505, 537-549` (Go)
  - `runtime/chester-runtime.ts:61-102, 118-124`
- **Mechanism**:
  - Neither runtime captures delimited continuations, generators/CPS, or fibers. Instead, `resume(v)` calls `frame.fork(v)`, which constructs `next = answers.slice(0, ai).concat([v])` and invokes `run(next, true, ...)` — re-executing `bodyFn()` **from the very beginning of the `handle` block** even for ordinary **one-shot** deep and shallow handlers.
  - Furthermore, in `__chester_perform` (`effects_runtime.ml:108-109` TS, `547-548` Go), `const result = fn(resume)` runs **before** `throw { __chester_handled: frame, result }` / `panic(__chesterHandled{...})`. Thus the stack is **never unwound before entering `resume`**: `N` sequential `perform`s nest `N` copies of `bodyFn -> __chester_perform -> handler -> fork -> run -> bodyFn` on the call stack ($O(N)$ stack frames, blowing Node's stack around ~1,000 state ops) and take $O(N^2)$ total execution steps.
- **Concrete Failure (Wrong Result on One-Shot Deep Handler)**:
  ```chester
  var count = 0;
  handle {
    count = int_add(count, 1); // side effect / state read-modify-write before perform
    let x = perform get();
    int_add(count, x)
  } with State {
    case get() => resume(10)
  }
  ```
  - **Expected**: `count` increments once (`1`), `get()` resumes with `10`, returns `11`.
  - **Actual (Go & TS)**: Run 1 (`answers=[]`) sets `count = 1`, `get()` calls `resume(10)`. Run 2 (`answers=[10]`) re-runs `bodyFn()` from the top, setting `count = 2` (or printing `print_str` twice!), then takes `10`, returning **`12`**.
  - Even reading `var st` before `put` breaks:
    ```chester
    var st = 0;
    handle { let old = st; perform put(99); old }
    with State { case get() => resume(st); case put(s) => { st = s; resume(()) } }
    ```
    Run 1 binds `old = 0`, `put(99)` mutates `st = 99`, and `resume(())` replays `let old = st` with `st == 99`, returning **`99` instead of `0`**.

---

##### 1.2 [CRITICAL] Nested Handlers Corrupt Each Other's Answer Streams & Re-Perform Already-Handled Outer Effects
- **Locations**: `bin/effects_runtime.ml:47-111` (TS), `bin/effects_runtime.ml:440-550` (Go), `runtime/chester-runtime.ts:61-125`
- **Mechanism**:
  Each handler frame maintains an independent mutable cursor `ai` over its own `answers` array. When an outer handler `Outer` replays (`Outer` has `answers = [0, Unit]`) and enters an inner `handle { ... } with Inner`, `Outer`'s cursor `ai` advances to `2` (`ai == answers.length`) as statements before `perform Inner/bump()` execute. When `Inner` handles `bump()` and calls `resume(10)`, `Inner` re-runs its own `bodyFn()` from the start of the inner `handle` block **while `Outer` is still at `ai = 2` (`ai == answers.length`)**.
- **Concrete Trace**:
  ```chester
  var st = 0;
  handle {
    handle {
      let x = perform get();          // Outer State/get
      perform put(int_add(x, 1));     // Outer State/put
      perform bump();                 // Inner/bump
      perform get()
    } with Inner { case bump() => resume(10) }
  } with State {
    case get() => resume(st);
    case put(s) => { st = s; resume(()) }
  }
  ```
  1. `Outer` accumulates `answers = [0, Unit]` (`st = 1`, `ai = 2`).
  2. `Inner` hits `perform bump()`, calls `resume(10)`, starting `Inner` replay (`answers = [10]`).
  3. `Inner` replays `let x = perform get()`. `Outer.take()` sees `ai == 2 == answers.length`, returns `{ok: false}`!
  4. `Outer` mistakes `Inner`'s replay of a **past** `get()` for a **brand-new** `get()`, unwinds right past `Inner` (destroying `Inner`'s `[10]` answer stream!), reads `st = 1` (not `0`!), and starts `Outer` with `answers = [0, Unit, 1]`.
  5. Next `Inner` replay reads `x = 1` from `answers[2]` and performs `put(2)`, mutating `st` to **`2` instead of `1`**!

---

##### 1.3 [CRITICAL] TypeScript `__chester_with_evidence` Mutable Index Bug (`let i = 0; ev[i++]`) Breaks Multi-Capability `box`/`unbox` on `resume`
- **Locations**: `bin/effects_runtime.ml:129-137` (TS), `runtime/chester-runtime.ts:143-151`
- **Code**:
  ```js
  const __chester_with_evidence = (ev, bodyFn) => {
    let i = 0;
    const go = () => {
      if (i >= ev.length) return bodyFn();
      const { label, handlers } = ev[i++];
      return __chester_handle(label, go, handlers);
    };
    return go();
  };
  ```
- **Bug**:
  `i` is mutated across recursive entries instead of passed by value (contrast Go `nest(i + 1)` at `effects_runtime.ml:577-585`).
  - When `ev` has 1 capability (`tests/effects_box.chester`), `i` becomes `1`, so re-calling `go()` from `__chester_handle` on `resume` hits `1 >= 1` and calls `bodyFn()` — masking the bug in 1-effect tests.
  - When `ev` has $\ge 2$ capabilities `[Eff1, Eff2]`: initial setup increments `i` to `2`. When `bodyFn()` performs `Eff1.op()` and `Eff1` calls `resume(v)`, `Eff1`'s `__chester_handle` calls `go()` again. Now `i` is **still `2`** (`2 >= 2`), so **`go()` skips reinstalling `Eff2` (`ev[1]`) completely**, crashing on the next `perform Eff2.op()` with `Error: Unhandled effect operation: Eff2/...`!

---

##### 1.4 [CRITICAL] `box`/`unbox` Type-Soundness & Semantics Bugs (Both Go & TS)
- **Locations**: `bin/effects_runtime.ml:113-141, 553-591`, `runtime/chester-runtime.ts:127-155`
1. **Aborting handler inside `unbox` violates static type safety**:
   `__chester_evidence` copies `{ label, handlers }` from `__chester_caps` and `__chester_with_evidence` wraps `unbox` in a **brand-new `__chester_handle` call** at the `unbox` site rather than returning to the original `handle` delimiter. If a handler clause aborts (does not call `resume`) and returns a value of the original `handle` block's return type `R` (e.g., `Option(Box(Integer))` -> `None`), `unbox` (statically typed as `Integer`) directly returns `None` (`{_tag: "None"}`)! In Go, `__chester_as_int` immediately panics with `interface conversion: interface {} is map[string]interface {}, not int` on a well-typed `Any`-free program.
2. **Handler `return` clause wrongly re-applied on `unbox`**:
   `__chester_evidence` copies the entire `handlers` map including `handlers["return"]` (`case return(x) => ...`), which has type `HandleBodyTy -> HandleResTy`. When `unbox` finishes with type `BoxBodyTy` (completely unrelated to `HandleBodyTy`), `__chester_handle` invokes `handlers["return"](v)` on `BoxBodyTy`, causing type confusion / runtime crashes.
3. **`box` inside a resumed `shallow` handler captures `{}` empty handlers**:
   After `handle shallow` resumes (`effects_runtime.ml:67, 466`), an answer-only frame `{ label: "Choose", handlers: {} }` sits on top of `__chester_caps`, shadowing any outer `"Choose"` handler. Calling `box { perform choose() }` finds `__chester_caps[i].label === "Choose"` on that empty frame, captures `handlers: {}`, and crashes on `unbox` with `Unhandled effect operation`.

---

##### 1.5 [HIGH] Shallow Handler Bugs (`handle shallow`)
- **Locations**: `bin/effects_runtime.ml:45-88, 98-103, 437-505, 528-535`, `runtime/chester-runtime.ts:59-102, 112-117`
1. **`return` clause still runs after shallow resume**: `fork` passes `hs = {}` (`useHandlers = {}`), but line 75 (TS) and line 499 (Go) check `handlers["return"]` on the **original** `handlers` closure variable rather than `useHandlers["return"]`. In Plotkin/Kammar shallow handlers, resuming does not re-wrap the continuation in the shallow handler's `return` clause.
2. **Outer handler replay re-executes inner shallow handler clauses**: In `handle { handle shallow { perform choose(); perform choose() } with Choose { case choose() => { side_effect(); resume(true) } } } with Choose { case choose() => resume(false) }`, when the 2nd `perform` forwards to the outer handler and the outer handler calls `resume(false)`, the outer replay re-enters `handle shallow` from scratch with `answers = []` and full `handlers`, executing `side_effect()` a **second time** for the first `perform`.
3. **Unqualified `op` (`wantLabel === null` / `""`) steals answers from unrelated effect frames**: In `__chester_perform` (`effects_runtime.ml:97-101` TS, `525-530` Go), `if (wantLabel !== null && frame.label !== wantLabel) continue; const got = frame.take(); if (got.ok) return got.v;` — if `op` ever lacks a `"Label/"` prefix, `frame.take()` consumes pending replay answers from whichever handler happens to be on top of `__chester_caps`, regardless of effect label.

---

##### 1.6 [HIGH] Persistent List Corruption & Runtime Divergences Between Go and TS Primitives
- **Locations**: `bin/effects_runtime.ml:10-40, 184-370`, `runtime/chester-runtime.ts:7-36`
1. **Go `prim__list_append` + `prim__list_drop_last` / `prim__list_advance` silently mutates immutable Chester `List`s via shared backing arrays (`effects_runtime.ml:310, 364, 368`)**:
   - `prim__list_drop_last` returns `ls[:len(ls)-1]` (spare capacity `>= 1` sharing `ls`'s backing array) and `prim__list_append` runs `append(l1.([]any), l2)` without copying!
   - Reproduction:
     ```chester
     let a = [1, 2, 3];
     let b = list_drop_last(a);   // len=2, cap=3, aliases `a`
     let c = list_append(b, 99);  // overwrites a[2] = 99 in place!
     // In Go, list_get(a, 2) is now 99 instead of 3! (In TS, it is 3.)
     ```
     Even without `list_drop_last`, branching `list_append` calls on any slice with spare capacity (`let b = list_append([1, 2], 3); let x = list_append(b, 10); let y = list_append(b, 20)`) overwrite `x`'s last element with `20` in Go!
2. **`prim__list_drop_last([])` panics in Go vs returns `[]` in TS**:
   - Go (`effects_runtime.ml:368`): `ls[:len(ls)-1]` -> `panic: runtime error: slice bounds out of range [:-1]`.
   - TS (`effects_runtime.ml:20`): `[].slice(0, -1)` -> `[]`.
3. **`prim__int_div` and `prim__int_mod` disagree on negative numbers & division by zero**:
   - Go (`effects_runtime.ml:193-194`): `a / b` (truncates toward 0: `-7 / 2 == -3`, `7 / -2 == -3`) and `a % b` (`-7 % 2 == -1`, `7 % -2 == 1`); panics on `b == 0`.
   - TS (`effects_runtime.ml:31-32`, `chester-runtime.ts:27-28`): `Math.floor(a / b)` (floors toward $-\infty$: `prim__int_div(-7, 2) === -4`, `prim__int_div(7, -2) === -4`) and `((a % b) + b) % b` (`prim__int_mod(-7, 2) === 1`, `prim__int_mod(7, -2) === -1`); returns `Infinity`/`NaN` silently on `b === 0`!
   - Also TS `Integer` is JS `number` (53-bit float precision) vs Go 64-bit `int`.
4. **`prim__string_substring` & `prim__string_length` disagree on bounds and Unicode**:
   - Go (`effects_runtime.ml:186-187`): raw UTF-8 byte indexing `len(a)` / `s[start:end]`; panics on out-of-bounds or `start > end`; splits multi-byte codepoints.
   - TS (`effects_runtime.ml:26-27`): UTF-16 code units; `s.substring(start, end)` clamps negatives to `0`, clamps `> length`, and **swaps `start` and `end` when `start > end`**.
5. **Go `__chester_call_reflect` (`effects_runtime.ml:245-264`) fails on variadic Go functions**:
   - Checks `len(args) != ft.NumIn()` without handling `ft.IsVariadic()` / `fv.CallSlice`, so passing any variadic Go function (`fmt.Println`, `fmt.Sprintf`) as a first-class function panics.
6. **Go `__chester_field` (`effects_runtime.ml:418-426`) only supports `map[string]any`**:
   - Panics on actual Go structs returned by Go FFI.
7. **Go `handlers["return"]` (`effects_runtime.ml:499`) silently ignored if typed**:
   - `if retFn, ok := handlers["return"].(func(any) any); ok` silently drops the return clause if `retFn` has a concrete Go function type rather than `func(any) any`.
8. **Missing CLI/IO intrinsics in TS runtime**:
   - Go defines `__chester_get_args`, `__chester_write_file`, `__chester_read_file`, `__chester_read_stdin` (which silently ignores `io.ReadAll` errors at line 399), `__chester_write_stdout`, `__chester_write_stderr`, `__chester_exit`, `__chester_field`. None of these exist in `ts_runtime_file` / `runtime/chester-runtime.ts`.
9. **Concurrently unsafe global mutable state in Go & TS runtimes**:
   - Go (`effects_runtime.ml:428-429, 601`): `var __chester_caps []map[string]any`, `var __chester_frame_id int`, `var _global_elab_state any` are unsynchronized package globals; any two goroutines using effects or `box`/`unbox` trigger Go data races and corrupt the handler stack.
10. **Broken `rocq_effects_preamble` (`effects_runtime.ml:649-677`)**:
    - Line 671: `Definition int_add (a b : chester_dyn) : chester_dyn := a.` — **`int_add` literally discards `b` and returns `a`!**
    - Missing `int_sub`, `int_mul`, `int_div`, `int_mod`, `int_eq`, `int_lt`, `bool_*`, `string_*`, `list_*`; effects are uninhabited `Parameter` axioms.

---

#### 2. CLI, Lexer & Frontend Hacks (`bin/*.ml`)

##### 2.1 [CRITICAL] `bin/lexer.ml` Missing `<=`, `>=`, `!=` Multi-Char Operators & Crashes on Unterminated Strings
- **Locations**: `bin/lexer.ml:11-23, 65-102`
1. **`<=`, `>=`, `!=`, `&&`, `||` are NOT lexed as single tokens (`bin/lexer.ml:81-102`)**:
   - `bin/lexer.ml` only recognizes four 2-char symbols: `==`, `=>`, `:>`, `->`.
   - Even though `stdlib/std.chester:165-166` declares `infixl "<=" same_as "<"; infixl ">=" same_as "<";`, `theories/Expander.v:58-59` maps `"<="` -> `"int_le"` and `">="` -> `"int_ge"`, and `self-hosted/lexer.chester:172-179` lexes `"<="`, `">="`, `"!="`, **`bin/lexer.ml` splits `a <= b` into 4 tokens (`a`, `<`, `=`, `b`)**! Writing `1 <= 2` or `1 >= 2` in any file compiled by `main.exe` fails!
2. **Uncaught exception / silent truncation on unterminated string literal (`bin/lexer.ml:65-80`)**:
   - `let _end_idx = consume_str (i + 1) in String.sub source (start_idx + 1) (_end_idx - start_idx - 2)`:
   - Input ending with a lone `"` gives `_end_idx = start_idx + 1`, so length is `-1` -> **uncaught `Invalid_argument("String.sub / Bytes.sub")` crash**!
   - Input `"hello` (unterminated at EOF) gives `_end_idx = len`, **silently truncating the last character to `"hell"`** and succeeding with zero error!
   - Also allows raw unescaped newlines inside `"..."`, which emit invalid multiline `"..."` literals in Go and TS.
3. **Fake spans (`bin/lexer.ml:11-23`)**:
   - `line = 0` is hardcoded for every token in every file, and `unicode = utf16 = idx` uses raw UTF-8 byte offsets.

---

##### 2.2 [CRITICAL] `bin/main.ml`: `collect_elab_env` Erases Effect Rows & Breaks Module Sealing Across `--prelude` / Multi-File Boundaries
- **Locations**: `bin/main.ml:58-104` (and out-of-sync duplicate at `test/test_compiler.ml:65-90`)
1. **Complete Effect Row Erasure (`main.ml:70-71, 78`)**:
   - `| AstDef (name, tps, ps, rt, _, _) -> [ ((name, []), AstFunTy (tps, ps, rt, [])) ]`
   - Drops the 6th field (`effs`) of `AstDef` and substitutes `[]` (pure)! Any function in `--prelude` or an earlier file carrying `/ {io}`, `/ {div}`, or user effects `/ {State}` appears **pure** to subsequent files — completely bypassing the effect checker and termination checker across file boundaries!
2. **Module Sealing & Nested Module Erasure (`main.ml:73-90`)**:
   - Ignores `seal` (`:>` opaque sealing and `:` signature filtering), leaking private definitions and concrete types of sealed modules across files.
   - Line 80: `| AstModule (n, _, _, _) :: xs -> (n, AstModTy []) :: go xs` — erases all exports of nested modules to `[]`.
3. **Broken Polymorphic Enum Constructors (`main.ml:92-100`)**:
   - Zero-arg variant `None` of `enum Option(T)` gets `AstRef "Option"` (drops `tps`), and `Some(x: T)` gets return type `AstRef "Option"` instead of `AstApp (AstRef "Option") [AstRef "T"]`.
   - Top-level `let`/`var` bindings in preludes are completely dropped (`| _ -> []`).

---

##### 2.3 [HIGH] `bin/main.ml`: `resolve_file_imports` Drops a Trailing Imported Declaration Whose Body Is Not a Block
- **Locations**: `bin/main.ml:129-193`
- **Code (`main.ml:158-163`)**:
  ```ocaml
  let body =
    match expanded with
    | Block (stmts, _, _) -> stmts
    | other -> [ other ]
  in
  ModuleCST (binder, [], None, body, empty_span)
  ```
- **Bug**:
  `Parser.parse` (`theories/Parser.v:984-986`) builds `Block stmts tail empty_span`, and `parse_body_step` (`theories/Parser.v:276-300`) only appends the final item to `stmts` when it *ends with a block*:
  ```coq
  | [] =>
      if ends_with_block item then (item :: [], (unit_cst, rest1))
      else ([], (item, rest1))
  ```
  A trailing declaration written without a `{}` body and without `;` — e.g. `def f(...): T = <expression>` — therefore becomes `tail`, which `resolve_file_imports` discards.
  - **Verified reproducer** (current `main.exe`, `--module-path .`): library
    `def add(a: Int, b: Int): Int = { a + b };` followed by
    `def mul(a: Int, b: Int): Int = a * b` (no block body, no `;`), imported as
    `import Math from "math_lib.chester"` and called as `Math.mul(2, 3)`, fails with
    `Type Error: module has no export: mul` — the definition was dropped.
  - **Not affected**: the last declaration of `tests/math_lib.chester` is
    `def mul(a: Int, b: Int): Int = { a * b }`, which *does* end with a block and *is*
    preserved (verified: `Math.mul` is emitted). The existing fixture does not exercise
    the bug; a block-less trailing export does.
  - Rated **HIGH**, not CRITICAL, by this document's own legend: the loss normally surfaces as an unbound-export error rather than silently unsound code, although a trailing export nobody calls disappears with no diagnostic at all.
- **Additional `resolve_file_imports` bugs**:
  - `walk` (`main.ml:165-191`) does not recurse into `DefCST`, `LetCST`, `IfCST`, `MatchCST`, `HandleCST`, etc.
  - `visited` (`main.ml:156`) is only passed downward (not threaded across sibling/diamond imports) and paths are not canonicalized (`realpath`), duplicating diamond imports and missing cycles with non-identical relative paths.
  - Relative imports are not resolved relative to `Filename.dirname resolved` of the importing file.
  - Imported file `op_env` (`infixl`/`macro`) mutates global `op_env` (`main.ml:152-153`), leaking module-internal operators/macros.

---

##### 2.4 [HIGH] Other Ad-Hoc Hacks & Bugs in `bin/main.ml`, `bin/chester_bindgen.ml`, `bin/go_signatures.ml`, `bin/chester_fmt.ml`, `bin/chester_paths.ml`
1. **Shell Command Injection & Regex Corruption in `bin/chester_bindgen.ml:9, 32-36`**:
   - `let quote s = "'" ^ String.escaped s ^ "'"` uses OCaml string escaping instead of `Filename.quote`!
   - OCaml `String.escaped` does **not** escape `'` (enabling arbitrary shell injection via `Sys.command` if an argument contains `'`), and **doubles backslashes** inside single quotes (`--filter '\d+'` becomes `'\\d+'`, breaking regexes!).
   - Also `repo_root` (`chester_bindgen.ml:1-7`) crashes with `Failure("could not locate repository root")` when run outside the Chester repo tree, and `ensure_scripts_deps` (`lines 11-20`) runs `npm install` at runtime.
2. **Hardcoded Mock `lex`/`Span`/`ParseResult` Stubs in `bin/main.ml:25-33`**:
   - `main.ml:32` injects `const lex = (s) => [{kind: "Whitespace"}, {kind: "Id", text: "let"}];` plus `const ParseResult = ...` and `const Span = ...` into **every** `--ts` output! Any user program defining `def lex`, `Span`, or `ParseResult` crashes in JS with `SyntaxError: Identifier 'lex' has already been declared`.
3. **Raw Substring `"func main("` Replacement & Forced `fmt.Println` (`bin/main.ml:34-56, 456-459`, `bin/effects_runtime.ml:626-637`, `test/test_compiler.ml:163-185`)**:
   - `rename_chester_main` replaces the first substring `"func main("` anywhere in the Go output — including inside string literals! If a program contains `"func main()"` in a string literal before `def main()`, it corrupts the string literal and leaves `func main()` unrenamed (Go compile error: `main redeclared`). Conversely, `"func chester_main("` inside a string literal fools `find_m` into skipping the fallback stub (`undefined: chester_main`).
   - `fmt.Println(chester_main())` (`main.ml:459`) unconditionally prints `<nil>` / `{}` after every Go program's stdout.
4. **`--prelude` Broken on `--ts` and `--rocq` (`bin/main.ml:444-449`)**:
   - `let emit_prelude = opts.target = EmitGo` skips emitting prelude definitions for TS and Rocq! Compiling with `main.exe --ts --prelude stdlib/std.chester` typechecks and then crashes at runtime (`ReferenceError: Some is not defined` / `option_map is not defined`). Notice `test/test_compiler.ml:986` (`run_stdlib_fixture_ts`) silently worked around this by omitting `--prelude` and passing `stdlib/std.chester` as a regular positional file!
5. **`--ts-module` Produces Broken Standalone TypeScript (`bin/main.ml:263, 431`)**:
   - `--ts-module` neither sets `target = EmitTS` nor emits runtime imports/helpers, leaving `prim__*`, `__chester_*`, `int_add`, `Unit` as unbound globals (`TS2304`).
6. **Duplicate `Go_signatures.load` & Fragile Whitespace-Regex JSON Parser (`bin/main.ml:372-407`, `bin/go_signatures.ml:16-56`, `data/README.md:9-10`)**:
   - `main.ml` loads and parses `data/go-signatures.json` **twice** in a row (lines 384 and 404).
   - `go_signatures.ml:22-23` uses `Str.regexp "^    \"\\([^\"]+\\)\": {$"` and `"^        \"\\([A-Z][^\"]*\\)\": {$"` instead of `yojson` (which is already in `flake.nix:149`!). Breaks on CRLF `\r\n`, compact JSON, or 2/4-space reformatting, and discards **100% of `params`, `returns`, and `effects`** from `data/go-signatures.json` (so Go FFI calls bypass arity, argument type, return type, and `/ {io}` effect checking).
   - `data/README.md:9-10` falsely states *"Signature data is not yet wired into the Coq elaborator"*, contradicting `main.ml:434` (`init_elab_with_go`).
7. **Unconditional `./out` Directory Creation & Unknown CLI Flags (`bin/main.ml:298, 408-409`)**:
   - Always creates `./out/` in CWD even when `-o /other/path` is given; `Sys.mkdir` fails on nested parent dirs; unknown flags like `--foo` are treated as input filenames.
8. **`bin/chester_fmt.ml:25-41` Overwrites Syntax-Errored Files via Parser Recovery**:
   - `parse` never fails — it performs error recovery (`test_compiler.ml:405-420`). Running `chester_fmt.exe` on a file with syntax errors (or with `<=` which `lexer.ml` mis-tokenizes!) can overwrite the user's file in place with mangled recovered syntax; also lacks `--check`.
9. **`bin/chester_paths.ml:4, 37-50, 54-80`**:
   - Uses `Sys.file_exists` without `not (Sys.is_directory path)` (directories matching candidate names pass resolution and fail on `open_in`); `split_search_path` hardcodes `:` (breaks Windows paths); `resolve_chester_module` searches all `search_paths` for `Name.chester` before checking local `name.chester`.

---

#### 3. Stdlib Bugs, Dead Code & `AGENTS.md` Violations (`stdlib/**/*.chester`)

1. **[HIGH] Dead & Copy-Pasted `stdlib/java/std.chester` (`lines 1-14`)**:
   - No Java backend exists anywhere in Chester, and `stdlib/java/std.chester` is byte-for-byte identical to `stdlib/ts/std.chester` — calling JavaScript `console.log(msg)` and `prim__string_concat`!
   - None of `stdlib/java/std.chester`, `stdlib/go/std.chester`, `stdlib/ts/std.chester`, or `stdlib/ts/react.chester` is tested anywhere in `test/test_compiler.ml`.
2. **[HIGH] Leftover Test Stub `effect State` & `run_state` Polluting `stdlib/std.chester:2-19`**:
   - Defines a monomorphic `Integer`-only `effect State` (colliding with parametric `effect State(T)` in user programs) and `def run_state(init: Integer): Integer` which takes **no computation argument** and just hardcodes `perform put(int_add(st, 1)); perform get()`.
3. **[HIGH] Broken `stdlib/ts/react.chester:1-3`**:
   - Declares `def createElement(type: String, props: Any, children: List(Any)): Any`.
   - `type: String` forbids React function/class components; `children: List(Any)` passes a single JS Array (triggering React missing-`key` warnings and preventing `null`/text children) because Chester does not spread `List` into JS variadics. Neither `tests/react_mini.chester` nor `examples/counter/counter.chester` can use `stdlib/ts/react.chester` — both redeclare `createElement` manually!
4. **[MEDIUM] Non-TCO Recursive Stdlib Helpers Overflow Call Stack on Moderate Inputs (`stdlib/std.chester:245-342, 423-510, 539-545`)**:
   - `string_find_from`, `string_trim_start`, `string_trim_end`, `string_split_from`, `string_to_int_aux`, `string_all_digits`, `list_foldl_helper`, `list_foldr_helper`, `list_any_helper`, `list_all_helper`, `list_find_helper`, `list_contains_helper`, `find_matching_index` all recurse per character/element without TCO in JS or Go (~10k chars/items throws `RangeError: Maximum call stack size exceeded` in Node), and `string_split_from` copies `acc` on each element ($O(K^2)$).
5. **[MEDIUM] Stdlib API Defects & Inconsistencies**:
   - `string_to_int` (`stdlib/std.chester:249-253`) turns non-digits into `0` (`string_to_int("1a2") == 102`, `string_to_int("abc") == 0`).
   - Higher-order callbacks (`option_map`, `list_map`, `list_foldl`, etc.) require pure `(x: A) -> B` functions (no effect polymorphism), rejecting effectful closures.
   - `list_contains` (`stdlib/std.chester:497-510`) is hardcoded to `List(Integer)` and missing from `ListOps`.
   - `prim__join_strings` exists in both Go and TS runtimes but is never exposed in `stdlib/std.chester`.
   - `advance` (`stdlib/std.chester:411`) lacks the `list_` prefix; `stdlib/{go,ts}/std.chester` use `concat_str`/`int_to_str` vs `stdlib/std.chester`'s `string_concat`/`int_to_string`; `infixl "=="` / `"!="` / `"&&"` / `"||"` are missing.
6. **[MEDIUM] `AGENTS.md` & Surface-Type Violations**:
   - Legacy `Unit` as value expression in `test/test_compiler.ml:406, 416, 461, 471`.
   - Legacy `extension ListExt[T] on List T` (space type-app + `on` instead of `for List(T)`) in `test/test_compiler.ml:424-432`.
   - `Boolean` vs `Bool` split: `tests/effects_multishot.chester:2`, `tests/effects_rows.chester:2`, `tests/effects_shallow.chester:2` use `Boolean`, whereas runtimes only bind `const Bool = "Bool"` (so `Boolean` as a first-class type value crashes with `ReferenceError: Boolean` / Go `undefined: Boolean`).

---

#### 4. Alpha-Comparison & Bindgen Bugs (`scripts/*`)

##### 4.1 [CRITICAL] `scripts/compare_go_emit_alpha.py` Masks Severe Semantic & Type-Safety Regressions
1. **Conflates distinct user/compiler identifiers (`lines 15, 129-133`)**:
   - `HYGIENE = re.compile(r"^[A-Za-z][A-Za-z0-9]*_[0-9]+$")` maps every matching token by first-appearance index globally with **no scope tracking** and **no check that the identifier is compiler-generated**: calling `sha_256(x)` in File A and `crc_32(x)` in File B both normalize to `__a0__(x)` (`"equivalent"`!). Meanwhile, hygienic renames of names already containing underscores (`my_tmp_1` vs `my_tmp_2`) do **not** match `HYGIENE`!
2. **Conflates `nil` with `[]any{}` everywhere (`lines 108-128`)**:
   - Maps `nil`, `[]any{}`, and `[]interface{}{}` all to `__UNIT__`. In Go, `prim__list_length([]any{})` is `0` while `prim__list_length(nil)` **panics** (`interface {} is nil, not []interface {}`), and `fmt.Println` prints `[]` vs `<nil>`.
3. **Strips `__chester_as_int` / `__chester_as_string` / `__chester_as_bool` (`lines 139-170`)**:
   - Completely hides missing Go type assertions that either fail `go build` (`cannot use e (type any) as int`) or change runtime panic behavior. Also references non-existent `__chester_as_iface`.
4. **Deletes embedded runtime/assembler functions without comparing (`lines 33-82`)**:
   - `strip_embed_funcs` replaces `__chester_go_preamble`, `__chester_assemble_go`, and `__chester_assemble_go_src` with empty `{}` bodies, masking any Stage 1/Stage 2 preamble corruption.
5. **Go raw string lexer bug & unconditional `;` stripping (`lines 21, 92-98`)**:
   - ``(?P<raw>`(?:\\`|[^`])*`)`` treats `\`` as an escaped backtick inside Go raw strings (Go raw strings have **no** backslash escapes!), eating tokens across strings; deleting all `;` hides invalid Go `for` headers.
6. **Zero coverage in `dune runtest` and only 3 tiny fixtures in `flake.nix:98-100`**.

---

##### 4.2 [HIGH] `scripts/dts2chester.mjs` Unsound Type Mappings & Invalid Output
1. **Unsound union collapsing (`lines 81-88`)**:
   - `if (parts.includes("String")) return "String"; if (parts.includes("Integer")) return "Integer"; if (parts.includes("Bool")) return "Bool";`
   - Turns nullable returns like `string | null` or `string | undefined` into non-null `String` (unsound crash at runtime!), turns `string | number` into `String`, and narrows `type: string | Function` in `React.createElement` to `String`-only!
2. **Maps JS `number` (float64) to `Integer` (`lines 36, 58`)**.
3. **Broken JS variadics (`lines 111-116`)**:
   - Emits `...args: T[]` as `List(T)`, passing a single Array argument at runtime because Chester's TS emitter does not spread variadic extern calls (`path.join(["a", "b"])` throws `ERR_INVALID_ARG_TYPE`; `Math.max([1, 2])` returns `NaN`).
4. **Emits invalid Chester block comment on empty match (`line 168`)**:
   - Outputs `/* no matching declarations */`, which **fails to lex/parse in Chester** because `lexer.ml` only supports `//` line comments!
5. **Fails on real-world `.d.ts` files (`lines 108-164`)**:
   - Emits duplicate `def`s for TS function overloads (rejected by Chester elaborator: `Duplicate definition`); ignores optional `?` parameters; does not escape Chester keywords (`match`, `in`, `with`, `type`, `case`, `def`) or destructured params `{a, b}`; ignores `declare module "..."` (fails on `test/ts/shim.d.ts`!), namespaces, interfaces, and `/ {io}` effects.

---

##### 4.3 [MEDIUM] `scripts/go2chester/main.go` Issues
- Emits `"Unit"` for 0-return (`line 118`) mixed with raw Go types (`"string"`, `"int"`, `"[]byte"`, `"(string, error)"`, `"io/fs.FileMode"`) everywhere else (`lines 32-39, 119-127`).
- Ignores generic `sig.TypeParams()` (emitting unbound type param names), collisions on short `pkg.Name` (`math/rand` vs `crypto/rand`, `line 136`), and marks pure functions (`strings.*`, `math.*`) with `"effects": ["io"]` (`line 132`).

---

#### 5. Test Suite Gaps (`test/test_compiler.ml`, `tests/*.chester`, `test/ts/*`)

1. **[HIGH] `examples/counter` Fails TypeScript Typechecking with 74 Errors (`examples/counter/*`, `runtime/chester-runtime.ts`)**:
   - Running `./examples/counter/node_modules/.bin/tsc -p examples/counter/tsconfig.json --noEmit` fails with **74 errors**:
     - `src/gen/counter-body.ts`: `TS7006: Parameter 'count' implicitly has an 'any' type` and `TS2304: Cannot find name 'prim__int_to_string'`.
     - `src/gen/counter.ts` (and `runtime/chester-runtime.ts`): 72 `TS7006` / `TS7034` / `TS7005` (`noImplicitAny`) and `TS2339` (`Property '__chester_handled' does not exist on type '{}'` under strict `catch (e)`).
   - `examples/counter/package.json` runs only `vite build` (esbuild type-stripping, zero `tsc`), and `test/test_compiler.ml:1127-1131` (`react mini tsc smoke`) only passed `tsc` because `tests/react_mini.chester` had zero function parameters and omitted `chester-runtime.ts`!
   - Also `examples/counter/build-chester.sh:11` mutates git-tracked `runtime/chester-runtime.ts` during tests, `cat`s runtime + ES module body (`import` at line 156!), and `examples/counter/src/gen/counter.ts` on disk had a stale 121-line pre-shallow/pre-io runtime.
   - `counter.chester` doesn't even implement counter state/events — `src/main.tsx` does everything in React `useState` while `counter.chester` just formats `<p>{count}</p>` using internal `prim__int_to_string`.
2. **[HIGH] Non-Exhaustive Match on User `enum` Passes Typechecking (`tests/vector.chester:1-9`, `test/test_compiler.ml:1230`)**:
   - `tests/vector.chester` defines `enum Vector(T) { case Nil; case Cons(head: T, tail: Vector(T)) }` and `match v { case Cons(h, t) => h }` (missing `Nil`), yet passes `check_fixture` because `CoreChecker.v:1821` (`match_missing_builtin_variant`) only checks hardcoded `Option`/`Result` constructor names, not user-defined `enum`s!
3. **[HIGH] `tests/macro_hygiene.chester:1-16` Does Not Actually Test Hygiene**:
   - `swap(a, b)` expands to a nested `{ let tmp = a; let a = b; let b = tmp }` block. Because blocks compile to IIFEs in both Go and TS, `let tmp` is block-local and would leave outer `let tmp = 42` untouched even with **zero** hygiene renaming (and `swap` doesn't swap `a` and `b` either). It never passes caller `tmp` into a macro argument to test capture avoidance!
4. **[HIGH] `first_line` / `input_line` Output Truncation & `2>&1` Mixing (`test/test_compiler.ml:140-148, 195-211, 962-971`)**:
   - Both `run_fixture_main` and `run_fixture_go` merge `2>&1` and **discard everything after line 1**! In `examples/go/{hello,greet,field_access}.chester`, line 2 (`chester_main()`'s return value `42`) is completely ignored; multi-line outputs or warnings after line 1 are invisible.
5. **[MEDIUM] Large Backend & Self-Hosted Coverage Gaps**:
   - `check_fixture` (`test_compiler.ml:28-46`) only checks `code <> ""` — `tests/react_mini.chester` emits `null` in Go (invalid Go: `undefined: null`), yet `check_fixture` passes because `go build` is never run!
   - **Zero** module fixtures (`modules*.chester`), binder-shadowing fixtures, `higher_order.chester`, `func_values.chester`, `effects.chester`, `effects_box.chester`, `effects_nested.chester`, or `effects_state.chester` are executed at runtime on the **TypeScript** backend (note: Node strip-only TS rejects `namespace` emitted for modules!).
   - 14 of 19 negative (`*_bad.chester`) fixtures are never tested against the self-hosted compiler (`test_compiler.ml:376-379`).
   - `examples/go/wordcount.chester` and `examples/ts/wordcount.chester` are fake stubs returning hardcoded `42` (and never executed); `examples/go/field_access.chester` tests `fmt.Println`, not field access.

---

## 5. Documentation, website, grammar & build

All 21 target files were read line-by-line and cross-checked against `theories/*.v`, `bin/*.ml`, `self-hosted/*.chester`, `stdlib/**/*.chester`, `test/test_compiler.ml`, `tests/*.chester`, and `examples/**`.

---

#### Executive Summary of Severity Counts

- **CRITICAL (4):** Uncompilable/broken homepage showcase code (`website/index.html`), fabricated "True Theorem Proving" & `Type 0`/`Type 1` universe hierarchy claims (`website/index.html`), false "dependently typed, mathematically verified" hero claim (`website/index.html`), and personal/vent notes left in root `README.md`.
- **HIGH (11):** False "doesn't miscompile valid code" & Rocq-hygienic-macro / Pratt-parsing claims (`website/index.html`), invalid `signature ... with type ... { ... }` BNF (`syntax-grammar.md`), non-existent `let`/`var` type annotations & `let` pattern destructuring in grammar/guides, unimplemented `PrefixOp`/`PostfixOp`, `\(params) =>` lambdas, `0x`/`0b` literals, broken `<=`/`>=`/`==` & `infixl "..."` in Rocq front end vs grammar, invalid `def zero: t` parameterless syntax in `docs/guide/modules.md`, contradictory `--prelude` + missing `--go` command in `docs/guide/getting-started.md`, fake Java stdlib claim (`docs/guide/stdlib.md` + `stdlib/java/std.chester`), non-existent `dune runtest` alpha test in `docs/guide/go-backend.md`, missing `stdlib.md` in `docs/SUMMARY.md`, and off-by-one broken relative paths `../../chester-references/` in `docs/paper/`.
- **MEDIUM (14):** False `var` -> `any` Go backend claim, false `ResultOps.or_else` claim, outdated duplicate-`def` claim in `stdlib.md`, false empty-`box` rejection claim in `effects.md`, dead `MacroDefCST` / `BoolLiteral` / `sync` / `WithUTF16` claims in `universal-syntax.md`, inaccurate `--emit-ts-runtime` docs in `cli-usage.md`, misleading generative functor & opaque-type-identity claims in `modules.md`/`LITERATURE.md`, invalid mdBook syntax in `SUMMARY.md`, `flake.nix` `src = ./.` unfiltered source tree, missing `dune runtest` check phase in `flake.nix`, `flake.nix` formatter omitting `examples/` and failing without prior Coq extraction, and `List[...]` delimiter rule violations in `self-hosted/`.
- **LOW (7):** Duplicate packages & unused deps in `flake.nix`, typo/garbled sealing text in `modules.md`, unreferenced BNF non-terminals (`TypeAnnotation`, `FirstClassMod`), missing `website/` docs link & unused CSS custom property, stale `flake.nix` description, etc.

---

#### 1. Root `README.md` Issues

##### [CRITICAL] Personal / vent notes left in public `README.md`
- **File:** `README.md` (lines 7–9)
- **Problem:** Contains informal personal commentary and a typo (`Sementics`):
  - Line 7: `i really get a bit discouraged from trying this tool because its original name had dirty connotations. it is not pleasant when people mentioned its original name in english. but let's see..`
  - Line 9: `Oh no this thing became another ai slop. Let's limit ai slop to implementations. Sementics shall be designed and understood by me`

##### [MEDIUM] Unqualified "Verified Chester compiler" claim in `README.md` and `docs/README.md`
- **Files:** `README.md` (line 3), `docs/README.md` (lines 3–4)
- **Problem:** Calls the project `Verified Chester compiler (Coq/Rocq → OCaml → TypeScript / Go / Rocq)` and `the verified Chester language` without noting that backend correctness (`emit_{rocq,ts,go}_correct` in `theories/{Rocq,TypeScript,Go}IRSem.v`) is proved **only** for the 7-constructor pure `CoreFrag` (`theories/CoreTyping.v` lines 240–250: `Int`, `Bool`, `Ref`, `Lam`, unary `App`, `If`, `Span`). This contradicts the warning in `docs/paper/LITERATURE.md` lines 181–182 (*"Do not read 'verified compiler' as verified effects or verified modules"*) and `docs/guide/getting-started.md` lines 3–7.

---

#### 2. Website (`website/index.html`, `website/styles.css`, `website/assets/logo.svg`)

##### [CRITICAL] Showcase code on homepage does not compile and does not do what it claims
- **File:** `website/index.html` (lines 31–43)
- **Problems:**
  1. **Line 31 (`def my_id(T: Type 0, x: T): T = x;`):** Chester has **no `Type 0` universe syntax**. Neither `theories/Expander.v` nor `theories/Elaborator.v` (nor `self-hosted/`) ever parses `Type <nat>` into `AstUniverse`. Instead, `Type 0` parses as `SeqOf [Symbol "Type"; IntegerLiteral "0"]` and elaborates via `is_allowed_unbound "Type"` (`theories/Elaborator.v` lines 1477, 1581–1585) into an untyped `Any` function call `AstApp (AstRef "Type") [AstIntLit 0]` (which fails `is_universe_ty` in `theories/CoreChecker.v` line 621!). Actual Chester uses `T: Type` (cf. `tests/dependent_types.chester` line 1: `def my_id(T: Type, x: T): T = x;`).
  2. **Line 42 (`swap(my_val, other_val);`):** `other_val` is never declared anywhere in the snippet; compiling fails immediately with `Type Error: Unbound variable: other_val` (`theories/Elaborator.v` line 1479).
  3. **Lines 33–39 (`macro swap`):**
     ```chester
     macro swap {
       case (a, b) => {
         let tmp = a;
         let a = b;
         let b = tmp;
       }
     }
     ```
     Inside `{ ... }`, `let a = b; let b = tmp;` merely introduces **immutable block-local shadow bindings** and returns `Unit` (due to trailing `;` on line 37). It **never mutates or swaps** `my_val` and `other_val` in the caller's scope! (`tests/macro_hygiene.chester` only used those lines to test that inner `let tmp` did not overwrite outer `tmp`, not to swap variables.)

##### [CRITICAL] Fabricated "True Theorem Proving" & universe hierarchy claims
- **File:** `website/index.html` (lines 53–54)
- **Quote:** *"True Theorem Proving — Inspired by Lean and Idris 2, Chester supports a rigorous universe hierarchy (`Type 0`, `Type 1`...), dependent types, and compile-time beta evaluation via Weak Head Normal Form (WHNF)."*
- **Reality vs Source Code:**
  - **Not a theorem prover:** Chester has no equality/proposition types, no inductive families / indexed GADTs (`docs/paper/LITERATURE.md` lines 125–127: *"Length-indexed enums are **not** implemented (`tests/vector.chester` is a non-indexed `Vector(T)`)"*), and its type checker admits `Any` / `Unknown` wildcards (`CoreChecker.v` lines 641–646) plus arbitrary unbound uppercase identifiers typed as `Any` (`CoreChecker.v` lines 615–619), making it logically inconsistent as a proof assistant.
  - **No `Type 0`, `Type 1`... universe hierarchy:** `AstUniverse` (`theories/AST.v` line 106) is never constructed from user syntax anywhere in `Parser.v`, `Expander.v`, or `Elaborator.v` (only used internally as `Some (AstUniverse 0)` in `Elaborator.v` lines 3240, 3298 and a hardcoded Coq test env in `CoreChecker.v` line 2093). Surface `Type` satisfies `is_universe_ty (AstRef "Type") = true`, giving `Type : Type`.

##### [CRITICAL] Exaggerated hero tagline
- **File:** `website/index.html` (line 15)
- **Quote:** *"The dependently typed, mathematically verified language for the modern era."*
- **Problem:** Directly contradicts `docs/paper/LITERATURE.md` §4 (*"Dependent types (citation only / partial surface)"*) and §6 (*"Verified fragment (honest scope)"*).

##### [HIGH] False claims in "Mathematically Verified" feature card
- **File:** `website/index.html` (lines 49–50)
- **Quote:** *"Chester's core compiler pipeline is fully extracted from Coq/Rocq proofs. Its parser, elaborator, and type checker have strong structural guarantees, ensuring the compiler doesn't miscompile valid code."*
- **Reality vs Source Code:**
  1. The lexer (`bin/lexer.ml`), CLI & file-import loader (`bin/main.ml`, `bin/chester_paths.ml`), Go signature loader (`bin/go_signatures.ml`), and effects runtime (`bin/effects_runtime.ml`) are unverified OCaml/TS/Go (`theories/Tokenizer.v` is an unused 17-line stub that only recognizes `{` and `}`).
  2. Neither `theories/Elaborator.v` nor `theories/CoreChecker.v` has soundness theorems (`docs/paper/README.md` lines 15–16).
  3. Non-miscompilation (`emit_{rocq,ts,go}_correct`) is proved **only** for `CoreFrag`, not `let`, `var`, `def`, `match`, `enum`, `record`, `extension`, `module`, or `effect`/`handle`. (In fact, outside `CoreFrag`, `theories/Backend.v` has an actual mismatch: `AstEnum` in `emit_ts_stmt` line 628 emits `{_tag: vname, _f0: ...}`, while `AstMatch` in `emit_ts_stmt`/`emit_ts_block` lines 716 & 757 indexes `_match_val.args[0]` instead of `_match_val._f0`!)
  4. Moreover, `flake.nix` line 125 installs `stage2` (the unverified self-hosted Go compiler) as `$out/bin/chester`.

##### [HIGH] False claims in "Extensible Syntax" feature card (for the Rocq compiler)
- **File:** `website/index.html` (lines 57–58)
- **Quote:** *"Chester supports hygienic macros that prevent variable capture, topological Pratt parsing for custom operator precedence, and Scala-style extension methods."*
- **Reality vs Source Code:**
  1. **No macro hygiene in Rocq compiler:** `bin/lexer.ml` line 22 initializes every `Span.context = []`, and nothing in `theories/` ever pushes a hygiene mark onto `Span.context`. `theories/Expander.v` (`subst_cst`, lines 1558–1588; `apply_macros_fuel`, lines 1600–1676) performs naive capture-prone CST substitution without marks (`tests/macro_hygiene.chester` only passes because `{ let tmp = ... }` is a block scope). Only `self-hosted/expander.chester` (`add_mark_cst`, line 551) implements hygiene marks.
  2. **No topological Pratt parsing or working custom operators in Rocq compiler:** `theories/Expander.v` (lines 62–158) uses flat integer precedences (`OpEnv := list (string * nat)`), ignores `infixr` vs `infixl` (`rhs_min := Nat.add prec 1` on line 121 makes all ops left-associative), omits `looser_than` (lines 97–102), and `parse_infix_rel` (lines 89–95) matches `Symbol op` instead of `StringLiteral op`, meaning `infixl "..."` declarations are **completely ignored** in `theories/Expander.v`! Only `self-hosted/expander.chester` (lines 907–985, 1361–1449) implements relational `OpRel` graph search.

##### [LOW] Minor website polish issues (`website/index.html`, `website/styles.css`, `website/assets/logo.svg`)
- `website/index.html` (lines 16–19): No navigation/CTA link to the documentation (`docs/`).
- `website/index.html` (line 65) vs `LICENSE` (line 3): Copyright holder named `The Chester Language Project` on website vs `The Chester Programming Language` in `LICENSE`.
- `website/styles.css` (line 6): CSS custom property `--primary: #d4af37;` is defined on `:root` and never used anywhere (`.btn.primary` on line 81 uses `var(--accent)`).
- `website/assets/logo.svg` (lines 1–15): Valid SVG, no issues found.

---

#### 3. Grammar & Surface Syntax Discrepancies (`docs/references/syntax-grammar.md`)

Cross-checking `docs/references/syntax-grammar.md` (lines 1–195) against `bin/lexer.ml`, `theories/Parser.v`, `theories/Expander.v`, `theories/Elaborator.v`, and `self-hosted/{lexer,parser,expander}.chester`:

1. **[HIGH] Broken `SignatureDecl` / `SignatureWith` BNF (lines 86–87):**
   - Grammar writes:
     ```bnf
     SignatureDecl ::= 'signature' ID SignatureWith? '{' SigMember* '}' ';'?
     SignatureWith ::= 'with' 'type' ID '=' TypeAnnotation
     ```
   - This specifies `signature T with type t = Int { ... }`, which **does not parse** in either compiler!
   - Actual syntax (`theories/Expander.v` lines 755–770, `self-hosted/expander.chester` lines 424–461, `AGENTS.md` line 47, `tests/modules_with_type.chester` line 6) is:
     - `'signature' ID '{' SigMember* '}' ';'?`
     - `'signature' ID '=' ID ('with' 'type' ID '=' TypeAnnotation)? ';'?` (also supports signature alias `signature A = B;`).

2. **[HIGH] Unimplemented `LetBinding` pattern destructuring & `LetBinding`/`VarBinding` type annotations (lines 41–42):**
   - Grammar writes:
     ```bnf
     LetBinding ::= 'let' (ID | Pattern) (':' TypeAnnotation)? '=' Expr ';'?
     VarBinding ::= 'var' ID (':' TypeAnnotation)? '=' Expr ';'?
     ```
   - Neither `theories/Expander.v` (lines 509–526: requires `Symbol name _ :: Symbol "=" _ :: val_exprs`) nor `self-hosted/expander.chester` (lines 1957–2008: requires `Symbol(name, _)` and blindly takes `advance(advance(advance(elems)))`) supports `Pattern` in `let` or `: TypeAnnotation` on `let` or `var`! Writing `let x: Integer = 5;` fails to produce `LetCST`.

3. **[HIGH] Unimplemented multi-param / parenthesized `LambdaExpr` (line 148):**
   - Grammar writes: `| '\' '(' ParamList? ')' '=>' Expr`
   - Neither `theories/Expander.v` (lines 320–322: matches only `Symbol "\" :: Symbol arg :: Symbol "=>" :: body`) nor `self-hosted/expander.chester` (lines 1005–1025) supports parenthesized, multi-parameter, zero-parameter, or type-annotated `\ (x: T) => ...` value lambdas! (`(x: A) -> B` is only expanded at arrows via `expand_arrow_type` in `Expander.v` lines 301–307.)

4. **[HIGH] Unimplemented `PrefixOp Expr` and `Expr PostfixOp` (lines 122–123):**
   - Listed in `Expr` (lines 122–123), with `PrefixOp` and `PostfixOp` never defined anywhere in the file.
   - Neither `theories/Expander.v` nor `self-hosted/expander.chester` (`pratt_parse` / `pratt_loop`) implements unary prefix or postfix operator expressions — even unary `-1` fails with `Unbound variable: -` (`stdlib/std.chester` line 290 has to write `int_neg(1)`).

5. **[HIGH] Unimplemented `0x` hex and `0b` binary integer literals (line 169):**
   - Grammar writes: `INT_LITERAL ::= [0-9]+ | '0x'[0-9a-fA-F]+ | '0b'[01]+`
   - Neither `bin/lexer.ml` (lines 54–64), `self-hosted/lexer.chester` (lines 48–60, 101–106), nor `theories/Elaborator.v` (`string_to_nat`, lines 18–36) supports `0x` or `0b`; `0xff` lexes as `TokInt "0"` followed by `TokId "xff"`.

6. **[HIGH] Wrong `STRING_LITERAL` escape rule (line 170):**
   - Grammar writes: `STRING_LITERAL ::= '"' [^"\\]* '"'` (forbidding backslash escapes).
   - Both `bin/lexer.ml` (line 69) and `self-hosted/lexer.chester` (line 118) support `\` escape pairs (`\"`, `\\`, `\n`, `\t`, `\r`), used throughout `stdlib/std.chester` (line 303) and `self-hosted/lexer.chester` (lines 16, 21).

7. **[HIGH] Broken `InfixOp` (`==`, `<=`, `>=`) and `InfixDecl` (`infixl`/`infixr`/`looser_than`) vs actual front ends (lines 109–113, 172):**
   - **Lexer/Expander mismatch on `<=`, `>=`, `==`:** `bin/lexer.ml` (lines 81–102) only lexes `==`, `=>`, `:>`, `->` as two-char `TokSym`s — it **splits `<=` and `>=` into two tokens** (`<` + `=` and `>` + `=`), so `op_prec`'s `"<="` / `">="` cases in `theories/Expander.v` (lines 70–71) are unreachable! Conversely, `bin/lexer.ml` lexes `"=="`, which `theories/Expander.v` omits from `op_prec` and `op_to_fn` (lines 50–72)! And in `self-hosted/expander.chester` line 1309, `op_to_fn("!=") = "int_eq"` (without `bool_not`!).
   - **Broken `InfixDecl` in Rocq:** `theories/Expander.v` `parse_infix_rel` (lines 89–95) matches `Symbol op` instead of `StringLiteral op`, ignores `infixr` associativity, and omits `looser_than`. Furthermore, neither `theories/Parser.v` (`is_block_continue`, lines 98–107) nor `self-hosted/parser.chester` (`is_block_continue_kw`, lines 256–264) includes `"looser_than"`.
   - **Identifier `InfixOp` (line 172):** Arbitrary `ID` is not an infix operator unless registered via `InfixDecl` (which works only in `self-hosted/expander.chester`, not in `theories/Expander.v`).

8. **[MEDIUM] False `Newline` statement terminator in `Statement` (line 26):**
   - Grammar writes: `Expr (';' | Newline)?`
   - Both `bin/lexer.ml` (line 31) and `self-hosted/lexer.chester` (line 140) strip `\n` as whitespace. Newlines **never** terminate statements; only block-ending forms (`ends_with_block`) can omit `;` before a following statement (`theories/Parser.v` lines 256–305).

9. **[MEDIUM] Missing surface declarations in `Statement` (lines 14–27):**
   - Missing `RecordDecl` (`record Name(x: T, y: U)` / `record Name(T)(x: T)` — `theories/Expander.v` lines 1176–1192, `Elaborator.v` lines 3234–3246; note `self-hosted/expander.chester` lines 1642–1672 only accepts `record Name[T](fields)` for parameterized records, violating `AGENTS.md` line 26).
   - Missing `MacroDecl` (`macro Name { case (a, b) => ... }` — `theories/Expander.v` lines 1522–1532, `tests/macro_hygiene.chester`).
   - Missing `TypeDecl` (`type ID ('=' TypeAnnotation)?` — needed inside `module { Statement* }` on line 79, currently only listed in `SigMember` on line 89).

10. **[MEDIUM] Inaccurate `IfExpr` BNF (line 154):**
    - Grammar writes: `IfExpr ::= 'if' Expr 'then' Block 'else' (Block | IfExpr)`
    - Neither `then` nor `else` requires a `Block` (`if int_lt(a, b) then a else b` in `stdlib/std.chester` line 180), and `else` is optional (`theories/Expander.v` line 267: `if cond then thenB` defaults else to `()`).

11. **[MEDIUM] Inaccurate `DefDecl`, `SigMember`, `ModuleDecl`, `HandlerCase`, `ImportDecl` details:**
    - `DefDecl` (line 31): `('/' EffectRow)?` only parses when `':' TypeAnnotation` is present (`theories/Expander.v` lines 812–824).
    - `SigMember` (lines 88–89):
      - Missing nested module signature alias `module Inner = InnerSig` (used in `docs/guide/modules.md` line 182 & `tests/modules_nested_sig.chester` line 6).
      - `ImplicitParams?` on `SigMember` (`def f[T](...): ...`) is **broken in Rocq `theories/Expander.v`** (lines 720–752 only handle `AppCST (Symbol name _) args` without `ImplicitAppCST`, whereas `self-hosted/expander.chester` lines 267–273 handles it).
    - `ModuleDecl` (lines 79–83):
      - Combining `'app'` with `ModuleSeal` (`module app F(X: S) :> R { ... }`) silently drops `'app'` in both `theories/Expander.v` (lines 645–666 overwrite `app_mark`) and `self-hosted/expander.chester` (lines 324–325).
      - Omits module alias `module N = M;` (`Expander.v` line 677), multi-arg functors, and `'applicative'` keyword synonym (`Expander.v` line 630).
    - `HandlerCase` (line 53): Writes `'case' ID '(' ParamList? ')' '=>' Expr`, but `parse_handler_case` (`theories/Elaborator.v` lines 377–388) only binds bare `ID`s (writing typed `s: T` silently turns the binder into `"_"`!), and also accepts paren-less `'case' ID '=>' Expr`.
    - `BoxExpr` (line 58): Accepts arbitrary `Expr`, not only `Block` (`Expander.v` lines 397–401).
    - `ImportDecl` (line 101): Omits `import "path.chester"` (`Expander.v` line 547); also note `self-hosted/expander.chester` (lines 2136–2185) does **not** implement `FileImportCST` (`import Math` / `import Math from "..."`) at all.

12. **[LOW] Undefined & unreferenced non-terminals in `syntax-grammar.md`:**
    - `TypeAnnotation` is referenced 11 times (lines 31, 34, 41, 42, 49, 67, 87, 88, 89, 98, 104) and **never defined** (missing arrow types `(x: A) -> B` / `A -> B`, type apps `Option(T)`, qualified types `M.t`).
    - `ImplicitApp` (`Atom '[' ExprList ']'`, e.g. `list_empty[String]()`) is mentioned in prose on line 183 (`f[A]`) but missing from `Atom` / `Expr` BNF (lines 128–141).
    - `FirstClassMod` (lines 91–92) is never referenced from `Statement`, `Expr`, or `Atom`.

---

#### 4. Documentation Issues (`docs/`)

##### `docs/SUMMARY.md`
- **[HIGH] Missing `docs/guide/stdlib.md` (lines 7–15):** `docs/guide/stdlib.md` is listed in `docs/guide/README.md` (line 9) but completely omitted from `docs/SUMMARY.md`.
- **[MEDIUM] Invalid mdBook `SUMMARY.md` item (lines 25–26):**
  `- Rocq properties (see [paper overview](paper/README.md#rocq-properties)): \`TypeSystemProps\`, ...`
  mdBook requires summary list items to be strictly `[Link Title](path.md)` (no surrounding prose or inline code spans).

##### `docs/guide/getting-started.md`
- **[HIGH] Broken command + contradictory `--prelude` explanation (lines 54–60):**
  ```markdown
  Use the standard library as a prelude (definitions are elaborated but not emitted):

  ```bash
  nix develop --command ./_build/default/bin/main.exe \
    --prelude stdlib/std.chester \
    -o app.go my-app.chester
  ```
  ```
  1. **Missing `--go`:** Default target in `bin/main.ml` (line 302) is `EmitTS`; `-o app.go` does not select Go, so this command writes **TypeScript** into `app.go`!
  2. **Contradicts Go prelude behavior:** Line 54 states *"(definitions are elaborated but not emitted)"*, contradicting `docs/guide/cli-usage.md` (lines 30, 60–62), `docs/guide/go-backend.md` (lines 61–62), and `bin/main.ml` (line 445: `let emit_prelude = opts.target = EmitGo`), where `--go` **does** emit prelude definitions!
- **[MEDIUM] Misleading top-level bodyless `def` in Modules sketch (line 110):** Shows `def go(): Unit / {io};` at top level outside any `signature { ... }` block.
- **[LOW] Incomplete "Learn more" links (lines 119–125):** Omits `effects.md`, `modules.md`, and `stdlib.md`.

##### `docs/guide/cli-usage.md`
- **[MEDIUM] Inaccurate `--emit-ts-runtime PATH` description (line 28):**
  `| --emit-ts-runtime PATH | Write runtime/chester-runtime.ts and exit |`
  In `bin/main.ml` (lines 280–287, 347–351), it writes to the user-supplied `PATH` (not literally `runtime/chester-runtime.ts`), and only exits early when no input `.chester` files are given on the command line.
- **[MEDIUM] Unusable `--prelude stdlib/std.chester` example on default `--ts` target (lines 64–68):** Because `--ts` does not emit `--prelude` files (`bin/main.ml` line 445), calling any non-preamble stdlib function (`option_map`, `list_map`, `string_split`, `Some`/`None`, etc.) in `my-app.chester` produces TypeScript that fails at runtime with `ReferenceError` unless `stdlib/std.chester` is passed as a regular input file instead of `--prelude` (cf. `test/test_compiler.ml` lines 986–988).

##### `docs/guide/stdlib.md`
- **[HIGH] Fake Java stdlib shim claim (line 16):**
  `| stdlib/java/std.chester | The same four names on Java |`
  Chester has **no Java backend**, and `stdlib/java/std.chester` (lines 1–14) is byte-for-byte identical to `stdlib/ts/std.chester` (calling `console.log(msg)`, `prim__string_concat`, `prim__int_to_string`)!
- **[MEDIUM] Non-existent `ResultOps.or_else` method documented (lines 92–93):**
  Lists `or_else` under `ResultOps` methods, but `extension ResultOps` in `stdlib/std.chester` (lines 147–155) **omits `def or_else`** (only the free function `result_or_else` on line 41 exists).
- **[MEDIUM] Outdated claim that the elaborator accepts duplicate `def`s (lines 117–119):**
  States *"the elaborator accepts duplicate `def`s, but the Go backend emits two functions with the same name and `go build` rejects the result."* In fact, `theories/Elaborator.v` (lines 3369–3370) runs `CoreChecker.duplicate_def_in_program` (`CoreChecker.v` lines 935–936) and **rejects** duplicate top-level `def`s in the same unit with `Type Error: Duplicate definition: <name>` (`test/test_compiler.ml` lines 536–539). (Duplicates only slip past when files are elaborated separately rather than concatenated.)
- **[LOW] Incomplete table summary (line 13):** Mentions `ListOps` / `StringOps` extensions for `stdlib/std.chester`, omitting `OptionOps` and `ResultOps`.

##### `docs/guide/effects.md`
- **[MEDIUM] False claim that empty `box` capture is rejected (lines 96–98):**
  States: *"Caps must be on the lexical stack at the box site — empty / unsealed capture is rejected (`Missing capability for box`)."*
  In `theories/Elaborator.v` (lines 233–241, 1748–1761), `effects_covered_by [] active` returns **`true`**, so **empty capture (`box { 42 }`, `caps = []`) is accepted**! Only *unhandled non-empty* capture (`caps <> []` when the required capability is missing from `active`, as in `tests/effects_unbox_unhandled.chester`) throws `Missing capability for box`.
- **[LOW] Non-existent `prim__list_tail` mentioned in termination prose (line 126):** Matches `CoreChecker.v` line 1119's hardcoded string check, though the runtime/stdlib primitive is actually `prim__list_advance` (`stdlib/std.chester` line 411) and `prim__list_tail` does not exist anywhere in `stdlib/` or `bin/effects_runtime.ml`.

##### `docs/guide/statements.md`
- **[HIGH] False claim that `let` supports type annotations (line 10):**
  States *"Type annotations are optional when the type can be inferred."* Type annotations on `let` (`let x: Integer = 5`) are **not implemented** in `theories/Expander.v` (lines 509–517) or `self-hosted/expander.chester` (lines 1957–1987).
- **[MEDIUM] Inaccurate `def` forward-reference rule & `--prelude` claim (lines 27, 42–43):**
  - Line 27 says *"Forward references are allowed when parameter types are annotated"*, whereas `Elaborator.v` (`collect_def_binders`, lines 1603–1617) pre-binds all block `DefCST`s to `AstRef "Any"` regardless of parameter annotations (which also silently drops latent effect rows on forward calls!).
  - Lines 42–43 say `--prelude` elaborates *"without emitting them"*, omitting that `--go` **does** emit prelude definitions.

##### `docs/guide/universal-syntax.md`
- **[MEDIUM] Wrong backend file path for Rocq emitter (line 15):** Attributes `TypeScript / Go / Rocq` backends solely to `theories/Backend.v`, omitting `theories/RocqBackend.v` where the Rocq emitter actually lives.
- **[MEDIUM] False claim that parser outputs `BoolLiteral` CST nodes (line 32):** `BoolLiteral` (`theories/CST.v` line 46) is never produced by `bin/lexer.ml`, `theories/Parser.v`, or `theories/Expander.v` (`true`/`false` parse as `Symbol` and elaborate via `is_allowed_unbound` to `AstRef`).
- **[MEDIUM] False claim that `Expander.v` builds `MacroDefCST` (line 40):** `MacroDefCST` does not appear anywhere in `theories/Expander.v` (macros are stripped by `collect_macros_stmts` into `MacroEnv` on lines 1522–1532, and `Elaborator.v` line 3003 rejects `MacroDefCST`).
- **[MEDIUM] Misleading `WithUTF16` span claim (lines 76–77):** Although `WithUTF16` is defined in `theories/CST.v`, `bin/lexer.ml` (lines 11–18) hardcodes `line = 0` and sets both `unicode` and `utf16` to the raw UTF-8 byte offset `idx` (not UTF-16 code units or Unicode scalar counts), and `self-hosted/cst.chester` has no `WithUTF16` at all.
- **[MEDIUM] False claim that `Parser.v` uses `sync` for error recovery (lines 81–83):** `sync` (`theories/Parser.v` lines 46–57) is dead code — it is never called anywhere in `Parser.v` or the rest of the compiler.

##### `docs/guide/modules.md`
- **[HIGH] Uncompilable `CounterSig` / `Counter` example using parameterless `def zero: t` (lines 78, 85, 93):**
  ```chester
  signature CounterSig {
    type t;
    def zero: t;
    ...
  };
  module Counter :> CounterSig {
    type t = Int;
    def zero: Int = 0;
  ```
  Neither `theories/Expander.v` (lines 720–752, 809–902) nor `self-hosted/expander.chester` (lines 261–278, 1858–1956) supports parameterless `def` without `()`. Both lines fail elaboration (`Unbound variable: def`); valid Chester requires `def zero(): t;` and `def zero(): Int = 0;` (cf. `tests/modules_types.chester` lines 3, 9, 15).
- **[MEDIUM] Misleading claims on opaque type identity & generative vs applicative functors (lines 13–14, 20–22, 92–94, 120, 131–132) & `docs/paper/LITERATURE.md` (lines 158–159):**
  - In `theories/Elaborator.v` (lines 1230, 3257–3260) and `CoreChecker.v` (lines 1062–1063), an abstract type `t` in `Counter :> CounterSig` exports bare `AstRef "t"` (and `Counter.t` elaborates to `AstRef "t"`, unifying with any other module's abstract `t` of the same member name!).
  - In `FunctorAppCST` (`Elaborator.v` lines 2954–2994), generative `_gN` vs applicative `__Arg` (`pref`, lines 2960–2962) **only renames the emitted runtime module prefix** — it does **not** stamp or freshen exported abstract types in `AstModTy`, and `Span.context` is always `[]` in Rocq so applicative identity is not keyed by "hygienic declaration sites" in the Rocq compiler.
- **[LOW] Garbled markdown & typo on lines 11–12:** `(opaque `:`/`:`sealing, ...)` repeats `:` instead of `:`/`:>`, labels both "opaque", and misses a space before `sealing`.

##### `docs/guide/go-backend.md`
- **[HIGH] Claims non-existent `dune runtest` test `bootstrap self-compile emit alpha` (lines 46–48):**
  `bootstrap self-compile emit alpha` does **not** exist anywhere in `test/test_compiler.ml` or `dune runtest`. Alpha comparison via `scripts/compare_go_emit_alpha.py` only exists in `flake.nix` (lines 89–100).
- **[MEDIUM] False claim that `var` bindings stay `any` (line 28):**
  States **"`var` bindings** — stay `any` (mutable / effect-friendly)". Both `theories/Backend.v` (lines 202, 1188) and `self-hosted/codegen_go.chester` (lines 248, 994–996) infer concrete Go types (`int`, `string`, `bool`) for `AstVar` via `go_type_of_ast_value`, identically to `let`!
- **[LOW] Slightly outdated Go-signatures wiring note (lines 79–80, plus `data/README.md` lines 9–10):** Says *"Type-checking against signatures in the elaborator is not wired yet"*, whereas `bin/main.ml` line 434 does wire package/function names via `init_elab_with_go` (`Elaborator.v` lines 170, 1961), typing matched calls as `Any` (parameter/return types in JSON are still ignored).
- **[LOW] External sibling path reference (line 96):** References `chester2026draft/docs/src/dev/go-backend.md`, which is outside this repository.

##### `docs/paper/README.md` & `docs/paper/LITERATURE.md`
- **[HIGH] Off-by-one broken relative paths `../../chester-references/...` (16 occurrences):**
  - `docs/paper/README.md` line 11
  - `docs/paper/LITERATURE.md` lines 13, 29, 36, 39, 101, 104, 107, 109, 111, 115, 117, 119, 121, 138, 173
  - From `docs/paper/`, `../..` resolves to the **repo root** (`chester/`), **not** its parent directory (`Documents/`)! Even with the sibling repo present at `Documents/chester-references/`, the relative path from `docs/paper/` would be `../../../chester-references/` (three `..`s), and referencing outside-of-repo paths breaks in standalone clones and GitHub rendering.

---

#### 5. Nix Flake & Build Issues (`flake.nix` & Repo Tree)

##### [HIGH] `src = ./.;` is unfiltered (`flake.nix` line 28)
- **File:** `flake.nix` (line 28)
- **Problem:**
  1. In `path:.` builds, `--impure`, or when untracked files exist outside `.gitignore`, `src = ./.;` copies local workspace artifacts into the Nix store (the working directory currently has ~100+ `fix_*.py`/`patch_*.py`/`rewrite_*.py` files, precompiled `theories/*.vo`, `test2.vo`, `test_coq.vo`, `test_gen.vo`, `_build/`, `extraction/Compiler.ml`, `.freebuff/`, and three `node_modules/` trees).
  2. Even in clean git mode, `src = ./.;` includes `docs/`, `website/`, `README.md`, `AGENTS.md`, and `.github/`, causing every doc/website edit to invalidate and rebuild the entire Coq + OCaml + Stage1 + Stage2 derivation. Use `lib.fileset.toSource` to whitelist build inputs (`_CoqProject`, `dune-project`, `theories/`, `extraction/dune`, `bin/`, `stdlib/`, `self-hosted/`, `tests/`, `examples/`, `scripts/compare_go_emit_alpha.py`, `data/`).

##### [MEDIUM] Missing `dune runtest` (or hermetic test subset) in Nix build (`flake.nix` lines 40–50)
- **File:** `flake.nix` (lines 36–37, 40–50)
- **Problem:** `packages.default` has no `doCheck = true` / `checkPhase` (despite including `alcotest` and `ppx_expect` in `buildInputs` on lines 36–37). Over 80% of `test/test_compiler.ml` (all parser/formatter expect tests, positive & negative `*_bad.chester` elaboration tests, Rocq emit tests, Go runtime tests, `--prelude`/`--module-path` CLI tests, and Rocq-vs-Stage1 parity) need **zero network/npm access** and could run hermetically in `checkPhase` if the 4 npm/tsc/vite tests are gated or given offline deps.

##### [MEDIUM] `formatter` omits `examples/`, fails without prior Coq build, and ignores args (`flake.nix` lines 160–180)
- **File:** `flake.nix` (lines 160–180)
- **Problems:**
  1. **Line 178:** `$(find self-hosted stdlib tests -name "*.chester")` **omits `examples/`** (`examples/go/*.chester`, `examples/ts/*.chester`, `examples/counter/counter.chester`) and `test/bindgen/expected/react_mini.chester`.
  2. **Line 178:** `dune exec bin/chester_fmt.exe` fails on a fresh tree if `extraction/Compiler.ml` has not already been generated via `coq_makefile -f _CoqProject -o Makefile && make`, and `formatterPath` (lines 164–172) lacks `pkgs.coq` / `pkgs.rocqPackages.stdlib`.
  3. **Lines 176–178:** Ignores CLI arguments passed by `nix fmt [files...]`, assumes CWD is repo root, and leaves `$(find ...)` unquoted.
  4. **Line 166 vs Line 150:** Uses `pkgs.ocamlformat` in `formatter` vs `pkgs.ocamlPackages.ocamlformat` in `devShells.default`.

##### [MEDIUM] Undocumented `$out/bin/chester` behavior in `packages.default` (`flake.nix` lines 123–126)
- **File:** `flake.nix` (lines 123–126) vs `self-hosted/cli.chester` (lines 2–35)
- **Problem:** `installPhase` installs **only** `stage2` as `$out/bin/chester` (which supports only `-o`, `--go`, `-h`/`--help` and cannot emit TypeScript/Rocq or resolve `--prelude`, `--module-path`, or `import`), while `main.exe`, `chester_fmt.exe`, and `chester_bindgen.exe` are not installed in `$out/bin`, and `docs/guide/cli-usage.md` never mentions `nix build .#default` or `result/bin/chester`.

##### [LOW] Duplicate & unused dependencies and stale description in `flake.nix`
- **Lines 145–148:** `pkgs.ocamlPackages.alcotest` and `pkgs.ocamlPackages.ppx_expect` are duplicated verbatim in `devShells.default.packages`.
- **Lines 36, 145, 149, 152, 170 & `test/dune` line 5:** `alcotest`, `yojson`, and `bun` are unused across the codebase (`test/test_compiler.ml` uses only `ppx_expect`; `bin/go_signatures.ml` uses `Str`, not `yojson`).
- **Line 2:** `description = "Chester CST Rocq Environment";` is outdated.

---

#### 6. `AGENTS.md` Compliance Violations Found in `self-hosted/`

While cross-checking `AGENTS.md` (lines 24–27: *"`[]` — list literals and implicit binders/apps only ... `()` — type applications and enum/record/effect type params: `Option(T)`, `List(T)`"*):
- **[MEDIUM] `List[...]` square-bracket type applications remain in `self-hosted/`:**
  - `self-hosted/expander.chester` (line 318): `rest_ss: List[CST]`
  - `self-hosted/elaborator.chester` (line 2937): `env: List[Pair]`
  - `self-hosted/codegen_go.chester` (line 226): `locals: List[Pair]`
- **[MEDIUM] `record` type-parameter delimiter mismatch in `self-hosted/expander.chester` (lines 1655–1667):**
  `self-hosted/expander.chester` only recognizes `ImplicitAppCST` (`record R[T](fields)`) for generic records, whereas `AGENTS.md` line 26 and `theories/Expander.v` line 1182 specify `()` (`record R(T)(fields)`).
- **[LOW] `test/test_compiler.ml` (lines 424–431):** Formatter test fixture uses legacy juxtaposition `List T` (`extension ListExt[T] on List T`) instead of `List(T)`.

---

## 6. Cross-cutting root causes & remediation plan

### 6.1 Duplicated findings (same defect, reported from multiple angles)

| Root cause | Frontend | Checker/backends | Self-hosted | CLI/runtime | Docs |
| --- | --- | --- | --- | --- | --- |
| `Any` wildcard for unbound names/primitives | 1.2 | 1.1 #3, 1.1 #6 | C5, `is_allowed_unbound` | — | website "verified" |
| Symmetric effect-row subsumption | 1.5 | 1.1 #2 | `effect_rows_compatible` no-op | `main.ml` erases rows | `effects.md` false claim |
| `Assign` unchecked | 1.7 | 1.1 #5 | `elaborator.chester:2353, 3058` | — | — |
| Opaque sealing / abstract type identity | 1.6 | 1.1 #4 | missing module env threading | `main.ml` ignores `seal` | `modules.md` misleading |
| No `CoreChecker` / `CoreWT` link | 1.1 | Part 2 all | C5 | — | README "verified" |
| `string_of_nat >= 10 -> "X"` | 3.2 | 3.4 #1 | uses Go `int_to_string`, unaffected | — | — |
| `!=`/`==`/`<=`/`>=` operator handling | 4.5 | — | C1, C2 | `lexer.ml` split tokens | grammar §3.7 |
| TS enum `_f0` vs match `args[0]` | 1.4 (tuple/list erasure) | 3.1 #1 | — | — | website "no miscompile" |
| Stage 1/2 string escaping | — | 3.2 #3 (Go backticks) | C3 | `lexer.ml` keeps raw escapes | — |
| Non-tail `if` / missing `return` in blocks | — | 3.1 #7, 3.2 #6 | C6 | — | — |
| Imports drop last decl / sealing | — | — | imports not implemented | 2.3, 2.4 #4 | — |
| Macros unhygienic & truncate bodies | 4.7 | — | expander §5 | website "hygienic" | `universal-syntax.md` |
| Fuel not derived from input | 3.1, 3.3 | 1.3 #2 | expander fuel `100` | — | — |
| `List[...]` delimiter violations | 3.4 | — | §Summary | `test_compiler.ml` | — |

### 6.2 Ordered remediation

The ordering below fixes blast radius first: anything that silently accepts a wrong
program, then anything that silently emits a wrong program, then usability and docs.
Each phase should land with a failing fixture that reproduces the finding, and the fixture
must be executed on **both** the Rocq pipeline and the self-hosted pipeline where both
claim to support the construct.

**Phase 0 — stop the bleeding (soundness escapes).**
1. Remove the `Any` escape hatch: either give `true`/`false`/operators real `TypeEnv`
   entries (from a checked prelude, not `is_allowed_unbound`), or reject unknown
   identifiers. Keep `Any` only for explicitly declared FFI values whose signature is
   known.
2. Make effect-row compatibility directional (`actual ⊆ expected`), never symmetric;
   make `LamCST` unify its arrow type and row with `expected`.
3. Check `AstLam` bodies in `CoreChecker` instead of the immediately-applied-lambda
   shortcut; delete the shortcut or restrict it to a checked node.
4. Check `Assign`: binding exists, is `var`, and the RHS matches the declared type.
5. Stop erasing effect rows and seals in `collect_elab_env` (`bin/main.ml`), and thread
   module seals/exports through it.
6. Add positivity checking for `enum` and replace `Type : Type` with a real level or
   explicitly document/limit the dependent fragment.
7. Fix `string_of_nat` (real decimal conversion) — it silently collides functor and
   hygiene names.

**Phase 1 — stop silent miscompilation.**
8. Rewrite the effect runtimes to capture a real continuation (CPS/generator/fiber) or
   document one-shot-only semantics and enforce them statically; at minimum stop
   re-running `bodyFn()` on `resume` and stop replaying outer effects from inner handlers.
9. Fix `box`/`unbox` evidence: pass the evidence index by value, do not copy the
   `return` clause, do not re-handle at the `unbox` site.
10. Unify Go/TS primitive semantics: `int_div`/`int_mod` rounding, division by zero,
    `string_length`/`substring` units and bounds, list copy-on-write for
    `advance`/`drop_last`/`append`.
11. Fix TS codegen: `_f0` vs `args[0]`, nested `_match_val` scoping, parenthesized
    callees/accessors, string escaping, module/namespace exports.
12. Fix Go codegen: raw backtick strings, `go_app_direct` heuristics replaced by a real
    local-binding environment, pattern binders in `locals`, module `sigs`/renaming,
    missing `return`, `_match_val` redeclaration.
13. Fix the Rocq backend: real `Fixpoint`/inductives, no doubled binders, a preamble that
    actually computes (`int_add` etc.), escaped names.
14. Fix `bin/lexer.ml`: `<=`, `>=`, `!=`, `&&`, `||`; unterminated strings raise a
    diagnostic; real line/column spans; interning escapes consistently with codegen.

**Phase 2 — frontend correctness.**
15. Expand `SeqOf` produced by lambda/match/handle/arrow stripping; re-expand after every
    desugaring step. Fix `case None =>` to be a constructor pattern, not `PatVar`.
16. Parse `match` arms, nested patterns, and literal patterns correctly; enforce
    constructor arity and exhaustiveness for user enums.
17. Template macros: substitute all CST nodes, avoid capture, and keep multi-token bodies;
    apply the same fix in Rocq and self-hosted.
18. Infix declarations: honour `infixr`, honour `looser_than`, register before expanding
    block tails, and make `type T = A / {e}` not parse `/` as division.
19. `try_parse_infix`/`apply_macros`/`zonk`/`expand_metas`: derive fuel from a size
    measure of the *expanded* term; fail loudly instead of returning partial results.
20. Parser: reject unclosed delimiters, remove the `Unit` sentinel ambiguity, fix the
    `TokSym "{"` block-tail bug, and stop treating `;` and `,` as interchangeable.

**Phase 3 — self-hosted parity.**
21. Add a real checker stage (port `CoreChecker` or a well-specified subset) and make
    `unify` fallible; wire `zonk`.
22. Bind `def`s to `AstFunTy` (including effect rows and type params) so calls check
    arity/types and charge effects.
23. Fix `!=`, Pratt precedence (remove dead/wildcard arms), unapplied enum constructors,
    non-tail `if`, match-arm expansion, assignment RHS, arrow types, modules threading
    `env`, handler `return` binder, and imports.
24. Make Stage 1 and Stage 2 agree on escape handling; add a Stage 3 bootstrap test to
    `dune runtest`.

**Phase 4 — CLI, build, tests.**
25. `--prelude` on TS/Rocq (or reject the combination); fix `--ts-module`, `--module-path`,
    `import` last-declaration loss, and `--emit-ts-runtime`.
26. Replace `"func main("` substring surgery with AST-level renaming; stop printing
    `<nil>` after Go programs.
27. Replace the regex JSON parser with `yojson` and actually consume params/returns/effects
    from `go-signatures.json`.
28. Flake: `lib.fileset.toSource` instead of `src = ./.;`, add `doCheck` for hermetic tests,
    install the verified `main.exe` alongside/prefer it over Stage 2, fix the formatter.
29. Tests: execute fixtures on the TS backend, run self-hosted on all negative fixtures,
    stop truncating output to line 1, and make the alpha comparator stop normalizing away
    coercions, `nil` vs `[]`, and runtime functions.
30. Delete dead code and artifacts: root `patch_*.py`/`fix_*.py`/`rewrite_*.py` (~400
    untracked files), `Tokenizer.v` stub, dead `effect_metas` solver, `stdlib/java/`,
    personal notes in `README.md`.

**Phase 5 — documentation.**
31. Website/README: remove `Type 0`/`Type 1`, "dependently typed", "theorem proving",
    "hygienic macros", and unqualified "verified"; state the `CoreFrag` scope precisely.
32. Grammar: rewrite `syntax-grammar.md` from the parser (signature-with, `let`/`var`
    annotations, lambdas, `PrefixOp`/`PostfixOp`, literals, escapes, infix decls,
    `record`, `macro`, `type`) or implement what it documents.
33. `modules.md` (`def zero(): t`), `getting-started.md` (`--go`), `stdlib.md` (Java/`or_else`),
    `effects.md` (empty `box`), `statements.md` (`let` annotations), `go-backend.md`
    (test name, `var` typing), and fix the `../../chester-references/` paths.

### 6.3 Findings that need a design decision, not a patch

- **Effect runtime model.** Continutation capture is the only way to satisfy the documented
  deep/shallow semantics; the alternative is to define handlers as one-shot and enforce
  linearity statically. Pick one before fixing the runtime, because the checker and the
  docs both depend on the answer.
- **Verification scope.** `CoreFrag`-only proofs are honest but narrow. Either extend the
  fragment/proofs (blocks, `let`, `def`, `match`, effects) or rename the claims everywhere
  ("verified core, unverified frontend/backends") and stop installing Stage 2 as
  `$out/bin/chester`.
- **Dependent types.** Tuple/list overloading (`AstTuple` serves both), missing type-family
  indices, and `Type : Type` mean the dependent fragment is currently both unsound and
  incomplete. Decide whether to implement a real core (indices, levels, positivity,
  alpha-equivalence) or to restrict and document.
- **Macro hygiene.** Rocq has no hygiene marks at all (`Span.context` is always `[]`);
  self-hosted has marks that are only partially applied. Decide on one algorithm
  (e.g. scope sets) and implement it once.
- **Numeric semantics.** 53-bit JS numbers vs 64-bit Go ints vs unbounded `nat` in the
  proofs cannot all be "Integer". Pick a semantics (machine int with checked overflow, or
  arbitrary precision) and propagate it to runtime, proofs, and docs.

### 6.4 Audit limitations

- Findings are from reading the tree at `3354c52`; only the reproductions explicitly marked
  were traced. No fuzzing or differential testing was performed.
- The `../chester-references` corpus was verified (14 PDFs, titles matched); no literature
  defects are reported, and the literature claims in `docs/paper/LITERATURE.md` were
  checked only where they describe implemented features.
- The fifth auditor (runtime/stdlib/tests) reported before its run hit a quota error, so its
  section may not cover every file in `tests/` and `examples/` exhaustively; it does cover
  every finding reproduced above.
