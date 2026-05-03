# docs/archive

Historical planning documents that are no longer load-bearing. They describe
work that is either complete (and now reflected in the live reference docs) or
has been superseded.

These documents are kept as a record of why the codebase looks the way it does.
**Do not** use them as authoritative specification when modifying code: the
code has often diverged. Use the active docs in `docs/` instead.

## Contents

### Compiler plans (all complete)

| Document | Original goal | Where the result lives |
|---|---|---|
| [`compiler/IR_DOCUMENTATION.md`](compiler/IR_DOCUMENTATION.md) | Early IR overview | Superseded by [`../compiler.md`](../compiler.md) |
| [`compiler/ITER_CHAIN_PLAN.md`](compiler/ITER_CHAIN_PLAN.md) | Flatten nested iterator IR variants | `IrIterChain` in `volar-compiler/src/ir.rs` |
| [`compiler/TRAIT_IMPL_PLAN.md`](compiler/TRAIT_IMPL_PLAN.md) | Custom trait & impl IR support | `IrTrait`, `IrImpl` in `volar-compiler/src/ir.rs` |
| [`compiler/TRAIT_UNIFY_AND_RAW_OPS_PLAN.md`](compiler/TRAIT_UNIFY_AND_RAW_OPS_PLAN.md) | Collapse `CryptoTrait`; add `RawMap`/`RawZip`/`RawFold` | Same file |
| [`compiler/TYPE_MANIFEST_PLAN.md`](compiler/TYPE_MANIFEST_PLAN.md) | `.volar.d` manifest format | `volar-compiler/src/manifest.rs` |
| [`compiler/TYPESCRIPT_BACKEND_PLAN.md`](compiler/TYPESCRIPT_BACKEND_PLAN.md) | Add a TypeScript backend | `volar-compiler/src/printer_ts.rs` |
| [`compiler/TS_BACKEND_PROGRESS.md`](compiler/TS_BACKEND_PROGRESS.md) | TS backend bug tracker | Closed; tracker no longer maintained |

### Subsystem plans

| Document | Status |
|---|---|
| [`primitives-compiler-plan.md`](primitives-compiler-plan.md) | Phases 1, 3, 4 complete; Phase 2 (registry-driven primitives) explicitly deferred |
| [`oram-channel-plan.md`](oram-channel-plan.md) | Implemented in `volar-channel`, `volar-oram`, `volar-oram-core`; see [`../agent-context/oram.md`](../agent-context/oram.md) |
| [`fhe-memory-enhancement-plan.md`](fhe-memory-enhancement-plan.md) | Superseded by ORAM weaver work; Phase 2b (PBS storage) deliberately skipped |

## Removing an archived plan

Archived documents are kept indefinitely. If a plan describes work that turned
out to be wrong (rather than just complete), update it with a note pointing to
the corrective document, but do not delete it — the history is part of the
project's [reliability story](../reliability.md).
