#!/usr/bin/env sh
set -e
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"
MAIN="${CHESTER_MAIN:-$ROOT/_build/default/bin/main.exe}"
if [ ! -x "$MAIN" ]; then
  dune build bin/main.exe
  MAIN="$ROOT/_build/default/bin/main.exe"
fi
mkdir -p examples/counter/src/gen runtime
"$MAIN" --emit-ts-runtime runtime/chester-runtime.ts
"$MAIN" --ts-module -o examples/counter/src/gen/counter-body.ts examples/counter/counter.chester
cat runtime/chester-runtime.ts examples/counter/src/gen/counter-body.ts > examples/counter/src/gen/counter.ts
