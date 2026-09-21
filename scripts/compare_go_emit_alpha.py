#!/usr/bin/env python3
"""Compare Chester-emitted Go up to alpha-renaming and formatting.

Used to check: Rocq compiling self-hosted ≈ self-hosted compiling itself.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


HYGIENE = re.compile(r"^[A-Za-z][A-Za-z0-9]*_[0-9]+$")
TOKEN = re.compile(
    r"""
    (?P<ws>\s+)
  | (?P<line_comment>//[^\n]*)
  | (?P<block_comment>/\*.*?\*/)
  | (?P<raw>`(?:\\`|[^`])*`)
  | (?P<str>"(?:\\.|[^"\\])*")
  | (?P<char>'(?:\\.|[^'\\])*')
  | (?P<num>0[xX][0-9a-fA-F]+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)
  | (?P<op>==|!=|<=|>=|:=|\.\.\.|&&|\|\||<<|>>|[+\-*/%&|^<>=!~.:,;()\[\]{}])
  | (?P<ident>[A-Za-z_][A-Za-z0-9_]*)
  | (?P<other>.)
    """,
    re.VERBOSE | re.DOTALL,
)

# Match full func header through the body-opening brace (not interface{} braces).
EMBED_HEADER = re.compile(
    r"func (__chester_go_preamble|__chester_assemble_go_src|__chester_assemble_go)\s*"
    r"\([^)]*\)\s*(?:interface\{\}|[A-Za-z_][A-Za-z0-9_]*)?\s*\{"
)


def skip_string(src: str, k: int) -> int:
    q = src[k]
    k += 1
    n = len(src)
    while k < n:
        if q == '"' and src[k] == "\\":
            k += 2
            continue
        if src[k] == q:
            return k + 1
        k += 1
    return n


def strip_embed_funcs(src: str) -> str:
    """Drop bodies of preamble/assemble helpers that embed huge string literals."""
    out: list[str] = []
    i = 0
    n = len(src)
    while i < n:
        m = EMBED_HEADER.match(src, i)
        if not m:
            out.append(src[i])
            i += 1
            continue
        # m ends at body '{'
        depth = 0
        k = m.end() - 1
        while k < n:
            c = src[k]
            if c in "\"`":
                k = skip_string(src, k)
                continue
            if c == "{":
                depth += 1
            elif c == "}":
                depth -= 1
                if depth == 0:
                    k += 1
                    break
            k += 1
        out.append(f"func {m.group(1)}() {{}}\n")
        i = k
    return "".join(out)


def tokenize(src: str) -> list[str]:
    toks: list[str] = []
    for m in TOKEN.finditer(src):
        kind = m.lastgroup
        if kind in ("ws", "line_comment", "block_comment"):
            continue
        val = m.group(0)
        if val == ";":
            continue
        if kind in ("raw", "str"):
            inner = val[1:-1]
            if kind == "raw":
                inner = inner.replace("\\", "\\\\").replace('"', '\\"')
            toks.append('"' + inner + '"')
        else:
            toks.append(val)
    return toks


def alpha_tokens(toks: list[str]) -> list[str]:
    mapping: dict[str, str] = {}
    empty_unit = ["[", "]", "interface", "{", "}", "{", "}"]
    out: list[str] = []
    i = 0
    while i < len(toks):
        t = toks[i]
        if t == "[" and toks[i : i + 7] == empty_unit:
            out.append("__UNIT__")
            i += 7
            continue
        if t == "nil":
            out.append("__UNIT__")
            i += 1
            continue
        if HYGIENE.match(t):
            if t not in mapping:
                mapping[t] = f"__a{len(mapping)}__"
            out.append(mapping[t])
        else:
            out.append(t)
        i += 1
    return out


def peel_chester_as_wrappers(toks: list[str]) -> list[str]:
    """Treat [__chester_as_int(e)] etc. as equivalent to [e] for emit compare."""
    wraps = {
        "__chester_as_int",
        "__chester_as_string",
        "__chester_as_bool",
        "__chester_as_iface",
    }
    out: list[str] = []
    i = 0
    n = len(toks)
    while i < n:
        if toks[i] in wraps and i + 1 < n and toks[i + 1] == "(":
            depth = 0
            j = i + 1
            while j < n:
                if toks[j] == "(":
                    depth += 1
                elif toks[j] == ")":
                    depth -= 1
                    if depth == 0:
                        out.extend(peel_chester_as_wrappers(toks[i + 2 : j]))
                        i = j + 1
                        break
                j += 1
            else:
                out.append(toks[i])
                i += 1
        else:
            out.append(toks[i])
            i += 1
    return out


def normalize_tokens(src: str) -> list[str]:
    return peel_chester_as_wrappers(alpha_tokens(tokenize(strip_embed_funcs(src))))


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("a")
    ap.add_argument("b")
    args = ap.parse_args()
    a = normalize_tokens(Path(args.a).read_text(encoding="utf-8", errors="replace"))
    b = normalize_tokens(Path(args.b).read_text(encoding="utf-8", errors="replace"))
    if a == b:
        print("equivalent")
        return 0
    for i, (x, y) in enumerate(zip(a, b)):
        if x != y:
            lo = max(0, i - 4)
            hi = i + 8
            print(f"differ at token {i}")
            print("a:", " ".join(a[lo:hi]))
            print("b:", " ".join(b[lo:hi]))
            return 1
    print(f"differ length {len(a)} vs {len(b)}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
