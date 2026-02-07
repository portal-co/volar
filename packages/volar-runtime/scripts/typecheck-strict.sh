#!/usr/bin/env bash
# Typecheck generated.ts in --strict mode WITHOUT @ts-nocheck.
#
# Usage:
#   ./scripts/typecheck-strict.sh          # print errors
#   ./scripts/typecheck-strict.sh --count  # print only the error count
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PKG_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SRC="$PKG_DIR/src/generated.ts"
TMP="$PKG_DIR/src/.generated_strict_check.ts"

# Strip @ts-nocheck line, keep everything else
sed '/^\/\/ @ts-nocheck/d' "$SRC" > "$TMP"

cleanup() { rm -f "$TMP"; }
trap cleanup EXIT

ERRORS=$(cd "$PKG_DIR" && npx tsc --noEmit --strict \
  --moduleResolution bundler --target esnext --module esnext \
  "src/.generated_strict_check.ts" 2>&1 || true)

if [[ "${1:-}" == "--count" ]]; then
  COUNT=$(echo "$ERRORS" | grep -c "error TS" || true)
  echo "$COUNT"
else
  if [[ -z "$ERRORS" ]]; then
    echo "✅  0 strict-mode errors"
  else
    echo "$ERRORS"
    echo ""
    COUNT=$(echo "$ERRORS" | grep -c "error TS" || true)
    echo "──────────────────────────────"
    echo "Total: $COUNT strict-mode errors"
  fi
fi
