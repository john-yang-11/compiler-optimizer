#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
dune build --profile release web/playground.bc.js
cp -f _build/default/web/playground.bc.js web/optimizer.js
echo "built web/optimizer.js"
