#!/usr/bin/env bash
# Report strict-mode errors in the generated spec without making package builds
# depend on its current TypeScript-completeness baseline.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PKG_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SRC="$PKG_DIR/generated.ts"
TMP="$PKG_DIR/.generated_strict_check.ts"

sed '/^\/\/ @ts-nocheck/d' "$SRC" > "$TMP"
cleanup() { rm -f "$TMP"; }
trap cleanup EXIT

ERRORS=$(cd "$PKG_DIR" && npx tsc --ignoreConfig --noEmit --strict \
  --moduleResolution bundler --target esnext --module esnext \
  ".generated_strict_check.ts" 2>&1 || true)

if [[ "${1:-}" == "--count" ]]; then
  echo "$ERRORS" | grep -c "error TS" || true
else
  if [[ -z "$ERRORS" ]]; then
    echo "✅  0 strict-mode errors"
  else
    echo "$ERRORS"
    echo ""
    echo "──────────────────────────────"
    echo "Total: $(echo "$ERRORS" | grep -c "error TS" || true) strict-mode errors"
  fi
fi
