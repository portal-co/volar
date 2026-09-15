# Future provider integration ledger

**Status:** authoritative deferred-check ledger for generic FHE/deferred-compute
plumbing. Updated 2026-09-14.

This ledger is required reading before adding or changing a provider adapter,
provider artifact toolchain, FHE boundary, heavy-garbling backend import, or a
check/test that is intentionally deferred. The current tree contains no valid
FHE implementation suitable for this plumbing. In particular, the historical
`FheScheme` / legacy TFHE path is not a validation target and must not be used
as a stand-in provider.

Every skipped check or test must have:

1. a stable ledger ID from this file;
2. a nearby `TODO(provider-ledger: ID)` source comment; and
3. a concrete prerequisite and intended public seam below.

Do not delete an entry when it becomes actionable: change its status, record
the selected provider/artifact revision and command, and link the new test or
check.

## Current reusable plumbing

- `crates/compiler/volar-build/src/fhe_provider.rs` constructs a deterministic
  Rust-to-LLVM-bitcode command for the baseline `wasm32v1-none` target.
- The command is generic deferred-compute plumbing, not an FHE implementation.
  It can later build a reviewed FHE provider or a viable heavy-garbling
  implementation.
- Existing `Pipeline::<VaffleStage>::from_command` /
  `from_command_inlined` consume its output through the normal structural LLVM
  import path. A provider artifact must expose a fixed public entry ABI before
  import; protocol/admission code remains responsible for binding that ABI to
  a session and provider profile.

## Deferred checks and tests

| ID | Seam | Required prerequisite / independent oracle | Status |
|---|---|---|---|
| FHE-PLUMB-TOOLCHAIN-01 | `RustWasmLlvmBuild::command_build` end-to-end compile of a minimal `#![no_std]` provider fixture to LLVM bitcode | This checkout's active `rustc` must have the `wasm32v1-none` standard-core artifacts; use `rustc`, not merely a different `rustup` installation | BLOCKED: `rustc 1.98.1` reports missing `core` for `wasm32v1-none`, despite `rustup target list --installed` reporting it. No fallback target is permitted. |
| FHE-PLUMB-TOOLCHAIN-02 | Imported Rust-WASM LLVM artifact reaches VAFFLE/Volar IR through `Pipeline::from_command` | A reviewed, deterministic fixture with an explicit entry ABI; after toolchain prerequisite clears | TODO |
| FHE-PLUMB-TOOLCHAIN-03 | Heavy-garbling LLVM artifact import | A practical, reviewed heavy-garbling implementation and a fixed public ABI; reference output must be independently checkable | TODO |
| FHE-PLUMB-PROVIDER-01 | Concrete provider artifact/profile validation | Maintained implementation, parameter/profile fingerprint, canonical frame format, key/evaluation-material lifecycle, and independent oracle | TODO |
| FHE-PLUMB-PROVIDER-02 | Provider boundary attached to `PreFheStoragePlan` | Reviewed provider plus end-to-end strict-ORAM/held-material reference test with no secret host exposure | TODO |
| FHE-PLUMB-PROVIDER-03 | Provider-induced failure/abort/replay semantics | Reviewed session/key-epoch binding and adversarial transport test plan | TODO |
| FHE-PLUMB-MOVF-01 | 5a1/5a2 plans consume provider chunk inputs after movfuscation | Stable frontend marker consumer plus reviewed provider | TODO |

## Completion rule for a provider adapter

A provider adapter may be added only after its rows are updated with its exact
artifact revision, command line, target/toolchain profile, ABI, parameter
fingerprint, frame validation behavior, and tests. It starts Unpinned and Very
unstable unless separate evidence warrants another classification. A passing
LLVM import alone is compiler evidence; it is not an FHE correctness, security,
interoperability, or deployment claim.
