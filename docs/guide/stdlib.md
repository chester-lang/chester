# Standard library

Chester ships two preludes plus per-backend shims. Pass a prelude with
`--prelude` (see [CLI usage](cli-usage.md)):

```bash
main.exe --go --prelude stdlib/std.chester -o app.go app.chester
```

| File | Contents |
|------|----------|
| `stdlib/minimal.chester` | Operators, `Pair` / `Option` / `Result`, a few `option_*` / `result_*` helpers, `Integer` primitives, the `div` effect |
| `stdlib/std.chester` | Everything in `minimal` plus the full `Option` / `Result` / `List` / `String` APIs, `digit_value`, `int_to_string` / `string_to_int`, `State` + `run_state`, and the `ListOps` / `StringOps` extensions |
| `stdlib/go/std.chester` | `print_str`, `print_int`, `concat_str`, `int_to_str` on Go |
| `stdlib/ts/std.chester` | The same four names on TypeScript |
| `stdlib/java/std.chester` | The same four names on Java |
| `stdlib/ts/react.chester` | `extern ts "react"` binding for `createElement` |

## Conventions

- `Unit` is the unit type and `()` the unit value: `def print_str(s: String): Unit / {io}`.
- Recursive helpers that need fuel carry `/ {div}`; callers do not have to repeat it.
- Functions returning a possibly-missing value use `Option`, failures use `Result`.
- Method-call syntax (`xs.map(f)`) comes from the `ListOps` / `StringOps` /
  `OptionOps` / `ResultOps` extensions; the free functions are equally public.

## Integer

`int_add`, `int_sub`, `int_mul`, `int_div`, `int_mod`, `int_neg`, `int_eq`,
`int_ne`, `int_lt`, `int_gt`, `int_le`, `int_ge`, `int_min`, `int_max`, `int_abs`,
`int_to_string`.

## Bool

`bool_or`, `bool_and`, `bool_not`.

## String

| Function | Result |
|----------|--------|
| `string_length(s)` | character count |
| `string_char_at(s, i)` | one-character string, `""` when out of range (including negative) |
| `string_substring(s, start, end)` | slice `[start, end)` |
| `string_concat(a, b)` / `string_append(a, b)` | concatenation |
| `string_eq(a, b)` | equality |
| `string_contains(s, needle)` | `true` when `needle` occurs in `s` |
| `string_starts_with(s, prefix)` / `string_ends_with(s, suffix)` | prefix / suffix test |
| `string_index_of(s, needle)` | first index, or `-1` |
| `string_to_int(s)` | lenient parse (`"12a"` reads as `120`) |
| `string_to_int_opt(s)` | strict parse: `None` unless all characters after an optional `-` are digits |
| `string_trim(s)` / `string_trim_left(s)` / `string_trim_right(s)` | strip spaces, tabs, `\n`, `\r` |
| `string_split(s, sep)` | fields split on `sep`, empty fields kept; an empty `sep` yields `[s]` |

`StringOps` methods: `length`, `get`, `concat`, `contains`, `starts_with`,
`ends_with`, `index_of`, `to_int_opt`, `trim`, `trim_left`, `trim_right`,
`split`.

## List

`list_empty[T]()`, `list_length`, `list_get`, `list_make`, `list_append`,
`list_insert_first`, `list_drop_last`, `advance`, `list_is_empty`, `list_head`,
`list_tail`, `list_last`, `list_reverse`, `list_concat`, `list_take`,
`list_drop`, `list_map`, `list_filter`, `list_foldl`, `list_foldr`, `list_any`,
`list_all`, `list_find`, `list_index_of`, `list_zip`.

- `list_take` / `list_drop` clamp out-of-range counts instead of failing.
- `list_index_of(list, pred)` returns the first matching index or `-1`.
- `list_foldl` takes `(acc, elem) -> acc`; `list_foldr` takes `(elem, acc) -> acc`.
- `list_zip` stops at the shorter list and produces `Pair(A, B)` values.
- `list_contains` is `Integer`-only (`List(Integer)`, target); use
  `list_index_of` with a predicate for other element types.

`ListOps` methods: `length`, `get`, `is_empty`, `head`, `tail`, `last`,
`reverse`, `append`, `map`, `filter`, `foldl`, `foldr`, `any`, `all`, `find`,
`index_of`, `take`, `drop`, `concat`, `zip`.

## Option

`Some(x)` / `None`, plus `option_is_some`, `option_is_none`,
`option_unwrap_or`, `option_map`, `option_and_then`, `option_or_else`,
`option_to_result`.

`OptionOps` methods: `is_some`, `is_none`, `unwrap_or`, `map`, `and_then`,
`or_else`, `to_result`.

## Result

`Ok(x)` / `Err(e)`, plus `result_is_ok`, `result_is_err`, `result_unwrap_or`,
`result_unwrap_err`, `result_map`, `result_map_err`, `result_and_then`,
`result_or_else`, `result_to_option`.

`ResultOps` methods: `is_ok`, `is_err`, `unwrap_or`, `map`, `map_err`,
`and_then`, `or_else`, `to_option`.

## Pair

`MkPair(fst, snd)` with `pair_fst(pair)` / `pair_snd(pair)`.

## Effects

```chester
effect div {};
effect State {
  def get(): Integer;
  def put(s: Integer): Unit
};
def run_state(init: Integer): Integer;
```

`div` marks recursion/fuel; `run_state` is a small example handler that closes
over a mutable cell. See [Effects](effects.md) for `handle` / `perform` /
`resume`, and `tests/effects*.chester` for runnable examples.

## Adding to the prelude

`stdlib/std.chester` is concatenated with `self-hosted/*.chester` for the
self-hosted compiler build, so avoid adding a name that a self-hosted source
already defines — the elaborator accepts duplicate `def`s, but the Go backend
emits two functions with the same name and `go build` rejects the result.
