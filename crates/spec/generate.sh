#!/usr/bin/env bash
# Regenerate crates/spec/volar-spec-dyn/src/generated.rs from volar-spec source.
#
# Run from the workspace root:
#   ./crates/spec/generate.sh
#
# Or via the cargo alias (also from workspace root):
#   cargo generate-spec
#
# Optional flags are forwarded to volar-codegen:
#   ./crates/spec/generate.sh --dump-ir
#   ./crates/spec/generate.sh --dump-ir-dyn
#   ./crates/spec/generate.sh --spec-dir path/to/other/spec/src
set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$WORKSPACE_ROOT"

echo "[generate-spec] workspace root: $WORKSPACE_ROOT"
exec cargo run \
    --package volar-compiler \
    --bin volar-codegen \
    --features volar-compiler/parsing \
    -- dyn "$@"
