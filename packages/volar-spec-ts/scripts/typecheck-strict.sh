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

# `--ignoreConfig` is only supported by the TypeScript 7 native port; tsc 5.x
# rejects it with TS5023. Probe for support and include it only when available,
# mirroring the Rust ts_backend tests.
IGNORE_CONFIG=()
if (cd "$PKG_DIR" && npx tsc --ignoreConfig --version >/dev/null 2>&1); then
  IGNORE_CONFIG=(--ignoreConfig)
fi

# Capture exit status without aborting on type errors. A nonzero status with
# no `error TS` diagnostics means tsc itself failed (missing/broken launcher,
# bad CLI args, ...), which must fail this script rather than report 0 errors.
STATUS=0
ERRORS=$(cd "$PKG_DIR" && npx tsc ${IGNORE_CONFIG[@]+"${IGNORE_CONFIG[@]}"} --noEmit --strict \
  --moduleResolution bundler --target esnext --module esnext \
  ".generated_strict_check.ts" 2>&1) || STATUS=$?
if [[ "$STATUS" -ne 0 ]] && ! grep -q "error TS" <<< "$ERRORS"; then
  echo "$ERRORS" >&2
  echo "typecheck-strict.sh: tsc failed without TypeScript diagnostics (status $STATUS)" >&2
  exit "$STATUS"
fi

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
