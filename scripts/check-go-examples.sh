#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXAMPLES_DIR="$ROOT_DIR/examples/go"

if [[ ! -d "$EXAMPLES_DIR" ]]; then
  echo "Expected examples directory not found: $EXAMPLES_DIR" >&2
  exit 1
fi

UPDATE=0
if [[ "${1:-}" == "--update" ]]; then
  UPDATE=1
fi

TMP_DIR="$(mktemp -d)"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

OUT_DIR="$TMP_DIR/go-out"
mkdir -p "$OUT_DIR"

SBT_CMD="${SBT_CMD:-sbt}"
GO_SIGS_FILE="$ROOT_DIR/go-signatures.json"
GO_SIGS_ARG=()
if [[ -f "$GO_SIGS_FILE" ]]; then
  GO_SIGS_ARG=(--go-sigs "$GO_SIGS_FILE")
fi

"$SBT_CMD" "cliJVM/run go $EXAMPLES_DIR --output $OUT_DIR ${GO_SIGS_ARG[*]}"

fail=0
for src in "$EXAMPLES_DIR"/*.chester; do
  base="$(basename "$src" .chester)"
  expected="$EXAMPLES_DIR/$base.go"
  actual="$OUT_DIR/$base.go"

  if [[ ! -f "$actual" ]]; then
    echo "Missing generated output for $src" >&2
    fail=1
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
        fail=1
      fi
    fi
  else
    if [[ "$UPDATE" -eq 1 ]]; then
      cp "$actual" "$expected"
      echo "Created $expected"
    else
      echo "Missing expected file $expected" >&2
      fail=1
    fi
  fi
done

if [[ "$fail" -ne 0 ]]; then
  echo "Go example check failed." >&2
  exit 1
fi

echo "Go example check passed."
