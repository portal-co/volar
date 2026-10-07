# Future provider integration ledger

**Status:** authoritative deferred-check ledger for generic FHE/deferred-compute
plumbing. Updated 2026-09-14.

This ledger is required reading before adding or changing a provider adapter,
provider artifact toolchain, FHE boundary, heavy-garbling backend import, or a
check/test that is intentionally deferred. The V2 `volar_spec::binfhe` module
now provides evidence-grounded **internal arithmetic and plan execution** for
this plumbing, but remains Unpinned and Very unstable and has no deployment,
frame-interoperability, or parameter-security claim. The historical
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
| FHE-PLUMB-TOOLCHAIN-01 | `ProviderArtifactSpec::command_build` end-to-end compile of a minimal `#![no_std]` provider fixture to LLVM bitcode | `rustup run stable rustc` with the installed `wasm32v1-none` standard-core artifacts | **PASS (toolchain only):** 2026-09-14, `rustup run stable rustc 1.98.1` emitted a 2.3 KiB raw LLVM bitcode artifact using the fixed flags below. The Homebrew PATH `rustc` fails to locate target `core`; provider tooling must use the resolved rustup toolchain executable. This is not provider correctness evidence. |
| FHE-PLUMB-TOOLCHAIN-02 | Imported Rust-WASM LLVM artifact reaches VAFFLE/Volar IR through `Pipeline::from_command` | A reviewed, deterministic fixture with an explicit entry ABI; after toolchain prerequisite clears | TODO |
| FHE-PLUMB-TOOLCHAIN-03 | Heavy-garbling LLVM artifact import | A practical, reviewed heavy-garbling implementation and a fixed public ABI; reference output must be independently checkable | TODO |
| FHE-PLUMB-PROVIDER-01 | Concrete provider artifact/profile validation | Maintained implementation, parameter/profile fingerprint, canonical frame format, key/evaluation-material lifecycle, and independent oracle | TODO |
| FHE-PLUMB-BINFHE-BOUNDARY-01 | `binfhe::boundary::PlanBoundary`: encrypt fixed plan inputs, execute, decrypt only declared Boolean outputs | `toy` exact profile and the already validated `BootstrapPlan` executor | **PASS (internal arithmetic only):** boundary test exhausts the AND truth table, validates before input conversion, and checks input shape. No frame, transport, security, or interoperable-provider claim. |
| FHE-PLUMB-BINFHE-BOUNDARY-02 | Bind `PlanBoundary` to generated BinFHE weaver function input/output ABI | Generated-code compile-and-run test against the plan boundary under the same profile and key material | TODO |
| FHE-PLUMB-BINFHE-ADAPTER-01 | `BinFhePlanAdapter`: source Boolar variable order is one-to-one with validated plan encryption/decryption order | Fused plan/binding test and independent generated-ABI integration | **PARTIAL:** binding cardinality/duplicate rejection and delta agreement are tested; generated ABI execution remains `FHE-PLUMB-BINFHE-BOUNDARY-02`. |
| FHE-PLUMB-WASM-ARENA-01 | `PlanWorkspace` / `CiphertextInputBuffer` fixed-capacity reuse and `StaticBumpAllocator` on `wasm32v1-none` | A concrete no-std provider artifact, explicit static heap budget, and repeated-call peak-memory measurement | **PARTIAL:** exact-toy tests prove arena/input reuse and reset. `RUSTC="$(rustup which rustc --toolchain stable)" cargo build -p volar-spec --target wasm32v1-none` passed on 2026-09-15. This proves only that the no-std+alloc spec compiles for the target; a concrete provider artifact with real BinFHE key/workspace setup is not yet imported or peak-measured. |
| FHE-PLUMB-GC-TRANSITION-01 | Fuse GC transition circuit after ciphertext/plaintext exchange; reset module-local globals/storage after output handoff | Stable transition IR/ABI, real strict garbler/evaluator integration, and independent boundary semantics test | TODO |
| FHE-PLUMB-CIRCUIT-ABI-01 | `CircuitProviderComposer`: descriptor validation, full unroll, inline composition, and fixed `derive_key` / `encrypt` / `decrypt` circuit geometry | Reviewed finite fixture and source-to-combined-circuit integration test | **DESIGNED:** `docs/fhe/circuit-provider-abi.md`; no provider fixture or imported ABI exists yet. |
| FHE-PLUMB-CIRCUIT-ABI-02 | AES-GCM-keystream-based split-seed KDF circuit | Profile-owned construction, scalar independent oracle, known-answer vectors, and domain-separation encoding | **DESIGNED:** generic shape only; no cryptographic construction is admitted. |
| FHE-PLUMB-CIRCUIT-ABI-03 | Demand-driven encrypted/clear wire tracker: lazy decryption, cached barrier, and mixed-select encryption | Source-wire identity preserving analysis plus optimizer and semantic-equivalence fixtures | **DESIGNED:** transition rules recorded; no tracker implementation exists yet. |
| FHE-PLUMB-CIRCUIT-ABI-04 | Seed shares, key epoch, profile/circuit/use labels, and encryption randomness provenance | Reviewed session protocol, adversarial binding/replay plan, and transport implementation | TODO |
| FHE-PLUMB-CIRCUIT-STORAGE-01 | Readonly base-storage descriptor, exact source mapping, tailored static/public/secret read manifest, and source-to-cache copy | Provider-neutral clear evaluator plus strict held/ORAM fixture; stale epoch and alias-isolation tests | **PARTIAL:** `ReadonlyStorageLayout`, canonical static/public manifests, explicit secret read demands, `HeldSlots` prefetch allocation, and isolated wire-level cache copy landed in `volar-vc`; only local generic fixtures exist—no provider/ORAM transport oracle yet. |
| FHE-PLUMB-CIRCUIT-STORAGE-02 | Fully materialized finite provider loop step scheduling and whole-loop optimization | Scalar loop oracle, unbounded-unroll integration, and loop-carried cache differential tests | **PARTIAL:** validated pure Boolar `ProviderLoopStep` statically composes all iterations and covers loop-carried cache wiring in generic differential fixtures. Whole-loop demand/manifest rematerialization and a real IR-origin provider fixture remain open. |
| FHE-PLUMB-CIRCUIT-STORAGE-03 | Per-invocation cache copy/use/discard/export lifecycle and strict `HeldSlots`/`Prefetch` registry integration | Independent storage/version oracle, boundary-only prefetch fixture, stale/replay and cross-invocation isolation tests | **PARTIAL:** invocation/cache identities, isolated writes, consume-on-discard/export, epoch-checked export, and deterministic held prefetch/cache allocation are implemented with generic tests. No durable provider storage adapter or strict transport fixture exists yet. |
| FHE-PLUMB-CIRCUIT-STORAGE-04 | Concrete provider storage profile, epoch/key/randomness session binding, and adversarial transport/replay testing | Reviewed provider, canonical storage/ciphertext frame, end-to-end strict ORAM/held material oracle | TODO |
| FHE-PLUMB-PROVIDER-02 | Provider boundary attached to `PreFheStoragePlan` | Reviewed provider plus end-to-end strict-ORAM/held-material reference test with no secret host exposure | TODO |
| FHE-PLUMB-PROVIDER-03 | Provider-induced failure/abort/replay semantics | Reviewed session/key-epoch binding and adversarial transport test plan | TODO |
| FHE-PLUMB-MOVF-01 | 5a1/5a2 plans consume provider chunk inputs after movfuscation | Stable frontend marker consumer plus reviewed provider | TODO |

## Recorded toolchain probe

The passing toolchain-only probe used an ephemeral source:

```rust
#![no_std]
#[unsafe(no_mangle)]
pub extern "C" fn provider_gate(a: u32, b: u32) -> u32 { a ^ b }
```

and this exact command shape (with temporary source/output paths):

```sh
rustup run stable rustc --edition=2024 --crate-type=lib \
  --target=wasm32v1-none --emit=llvm-bc \
  -Copt-level=2 -Ccodegen-units=1 -Cpanic=abort \
  -Cdebuginfo=0 -Coverflow-checks=on \
  -o provider.bc provider.rs
```

`file provider.bc` identified the output as `LLVM IR bitcode`. The test is a
**toolchain availability probe only**; it did not run the artifact, import it
into VAFFLE, instantiate FHE, or make any cryptographic claim. The importer
and all provider rows remain deferred as listed above.

## Completion rule for a provider adapter

A provider adapter may be added only after its rows are updated with its exact
artifact revision, command line, target/toolchain profile, ABI, parameter
fingerprint, frame validation behavior, and tests. It starts Unpinned and Very
unstable unless separate evidence warrants another classification. A passing
LLVM import alone is compiler evidence; it is not an FHE correctness, security,
interoperability, or deployment claim.
