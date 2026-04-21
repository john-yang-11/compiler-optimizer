#!/usr/bin/env bash
set -uo pipefail

cd "$(dirname "$0")/.."
LLI="${LLI:-$(command -v lli || command -v lli-18)}"
OPT="${OPT:-$(command -v opt || command -v opt-18)}"
if [ -z "$LLI" ]; then
  echo "lli not found; install LLVM to run this check" >&2
  exit 2
fi

dune build ./bin/optc.exe || exit 1
OPTC=./_build/default/bin/optc.exe
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

pass=0
fail=0
for src in test/programs/*.mini; do
  name=$(basename "$src" .mini)
  expected="${src%.mini}.expected"
  for level in -O0 -O1; do
    "$OPTC" llvm "$level" "$src" > "$tmp/$name.ll"
    if [ -n "$OPT" ] && ! "$OPT" -passes=verify -disable-output "$tmp/$name.ll" 2> "$tmp/err"; then
      echo "FAIL $name $level: invalid LLVM IR"
      cat "$tmp/err"
      fail=$((fail + 1))
      continue
    fi
    "$LLI" "$tmp/$name.ll" > "$tmp/out" 2>&1
    if cmp -s "$tmp/out" "$expected"; then
      pass=$((pass + 1))
    else
      echo "FAIL $name $level"
      diff "$expected" "$tmp/out" | head -5
      fail=$((fail + 1))
    fi
  done
done
echo "llvm backend: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
