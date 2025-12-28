#!/usr/bin/env bash
set -euo pipefail
shopt -s nullglob

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GO_EXAMPLES_DIR="$ROOT_DIR/examples/go"
TS_EXAMPLES_DIR="$ROOT_DIR/examples/ts"

UPDATE=0
if [[ "${1:-}" == "--update" ]]; then
  UPDATE=1
fi

TMP_DIR="$(mktemp -d)"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

SBT_CMD="${SBT_CMD:-sbt}"
fail=0

check_outputs() {
  local lang="$1"
  local src_dir="$2"
  local out_dir="$3"
  local ext="$4"
  local lang_fail=0

  if [[ ! -d "$src_dir" ]]; then
    echo "Expected examples directory not found: $src_dir" >&2
    return 1
  fi

  local src
  for src in "$src_dir"/*.chester; do
    local base
    base="$(basename "$src" .chester)"
    local expected="$src_dir/$base.$ext"
    local actual="$out_dir/$base.$ext"

    if [[ ! -f "$actual" ]]; then
      echo "Missing generated output for $src" >&2
      lang_fail=1
      continue
    fi

    if [[ -f "$expected" ]]; then
      if ! diff -u "$expected" "$actual" >/dev/null; then
        if [[ "$UPDATE" -eq 1 ]]; then
          cp "$actual" "$expected"
          echo "Updated $expected"
        else
          echo "Mismatch for $expected" >&2
          diff -u "$expected" "$actual" || true
          lang_fail=1
        fi
      fi
    else
      if [[ "$UPDATE" -eq 1 ]]; then
        cp "$actual" "$expected"
        echo "Created $expected"
      else
        echo "Missing expected file $expected" >&2
        lang_fail=1
      fi
    fi
  done

  if [[ "$lang_fail" -ne 0 ]]; then
    echo "$lang example check failed." >&2
    return 1
  fi

  echo "$lang example check passed."
}

if [[ -d "$GO_EXAMPLES_DIR" ]]; then
  GO_OUT_DIR="$TMP_DIR/go-out"
  mkdir -p "$GO_OUT_DIR"

  GO_SIGS_FILE="$ROOT_DIR/go-signatures.json"
  GO_SIGS_ARG=()
  if [[ -f "$GO_SIGS_FILE" ]]; then
    GO_SIGS_ARG=(--go-sigs "$GO_SIGS_FILE")
  fi

  "$SBT_CMD" "cliJVM/run go $GO_EXAMPLES_DIR --output $GO_OUT_DIR ${GO_SIGS_ARG[*]}"
  check_outputs "Go" "$GO_EXAMPLES_DIR" "$GO_OUT_DIR" "go" || fail=1
fi

if [[ -d "$TS_EXAMPLES_DIR" ]]; then
  TS_OUT_DIR="$TMP_DIR/ts-out"
  mkdir -p "$TS_OUT_DIR"

  "$SBT_CMD" "cliJVM/run ts $TS_EXAMPLES_DIR --output $TS_OUT_DIR"
  check_outputs "TypeScript" "$TS_EXAMPLES_DIR" "$TS_OUT_DIR" "ts" || fail=1
fi

if [[ "$fail" -ne 0 ]]; then
  echo "Example check failed." >&2
  exit 1
fi

echo "Example check passed."
