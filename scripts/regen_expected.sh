#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
dune build ./bin/optc.exe
for src in test/programs/*.mini; do
  out="${src%.mini}.expected"
  ./_build/default/bin/optc.exe interp "$src" > "$out" 2> "$out.err" || true
  cat "$out.err" >> "$out"
  rm -f "$out.err"
done
echo "regenerated $(ls test/programs/*.expected | wc -l) expected outputs"
