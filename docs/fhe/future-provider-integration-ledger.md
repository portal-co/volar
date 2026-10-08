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
| BITGC-LEDGER-01 | Authors'-artifact `sub64` conformance vectors for `volar_spec::bitgc::small` (ePrint 2025/1810 §4) | QEMU software-emulation Linux guest build of `wtd2/more-efficient-bitgc` (SEAL/HEXL, per repo Linux-coverage policy; fail-closed), `--offline-dim=9 --online-dim=8 --check=wire` wire dumps | **PARTIAL:** `volar_spec::bitgc::small` reference-adapter structure landed and passes self-contained truth-table tests; the artifact differential (the conformance oracle) is not yet run — a QEMU Linux guest build of the SEAL/HEXL artifact is the remaining prerequisite, and ePrint PDF access (BITGC-LEDGER-04) blocks confirming the equation matches |
| BITGC-LEDGER-02 | Authors'-artifact `sub64` conformance vectors for `volar_spec::bitgc::fast` (ePrint 2025/1810 §5.1) | Same QEMU artifact build as BITGC-LEDGER-01, `--variant=fast` | **PARTIAL:** `ReferenceEquation::FAST` (5-bit AND records, free-XOR payload add) passes truth-table tests; artifact differential pending as in BITGC-LEDGER-01 |
| BITGC-LEDGER-03 | Independent noise/parameter security review of the BitGC profiles (RLWE with KDM security, 128-bit claim) | External/reviewer parameter check against ePrint 2024/1988 and 2025/1810 parameter tables | TODO |
| BITGC-LEDGER-04 | Paper-pinned gate equations replace the reference gate equation behind `bitgc::GateEquation` | ePrint 2025/1810 Figures 4–5 (small) and §5.1 (fast) verified against the archived PDF; differential test of the swapped equation against the reference adapter. **Resolved (evidence) 2026-10-08:** PDFs archived in `docs/research/papers/` (`2025-1810.pdf`, `2024-1988.pdf`); exact equations extracted and cross-checked against the artifact — they agree. Recorded in `docs/research/bitgc-paper-pinning.md`. **Confirmed structure:** HSS-based (additive key-shares, per-gate distributed decryption, 1-bit determination; Δ = SWHE secret key), not the shipped `ReferenceEquation`'s self-contained SWHE table. Remaining (implementation): port the DD/HSS label layer per the pinned equations + differential test | **IN PROGRESS (2026-10-08):** `volar_spec::bitgc::pinned` lands the paper-pinned gate algebra + a reference DD gadget (exact `R_p` DD identities, cryptographically hollow) + full `garble_circuit`/`evaluate_circuit` drivers; both small and fast round-trips pass over all inputs and `record_bit_counts` matches Table 1. Verified findings recorded in `docs/research/bitgc-paper-pinning.md` (shared `(U,V,W)` eval key, fast-XOR global-Δ rows, LSB wrap = the per-gate failure term). **Landed (2026-10-08):** `volar_spec::bitgc::lattice_dd` — the lattice SWHEwDD gadget: GSW-style RLWE row ciphertexts `τ=(u1,u2)` over `R_q` (`u1+u2·Δ = t·Δ + p·e`), CP-RLWE triple `(U,V,W)` for k=2, and DD via mod-`p` rounding (`round_to_rp`). The DD identities hold **exactly** mod `p` (noise is a multiple of `p`, vanishes on reduction) — verified by `dd_linear_identity_exact`, `dd_correlated_identity_exact`, `hss_mul_product_share_exact`, `eval_row_reconstruction`, and both circuit round-trips (`lattice_small/fast_circuit_truth_table`) run through the shared `DdGadget` seam. The drivers are now generic over `DdGadget` (implemented by both `ReferenceDd` and `LatticeDd`). **Caveats (reference-grade):** schoolbook O(N²) negacyclic mul (no NTT), symmetric ternary noise sampler (not Gaussian), reduced test profile (p=97/q≈2^23 in unit tests, p=2^62 in round-trips) vs the paper's log p≈60/log q≈120, and no bit-decomposition/extended-gadget form. **Still open:** noise/parameter security review (BITGC-LEDGER-03) and the artifact differential (BITGC-LEDGER-01/02) |
| BITGC-LEDGER-05 | BitGC garble/eval benchmarks (Thumb SHA-256 fixture, all variants) vs. the half-gate locked baseline | QEMU software-emulation Linux guest with bounded timeout; artifact comparison numbers from BITGC-LEDGER-01/02 | TODO |
| BITGC-LEDGER-06 | BitGC-tiny (ePrint 2025/1810 §5.2) implementation | Paper-only construction (the public artifact ships no `tiny` mode); deferred by maintainer decision 2026-10-08 until the §5.2 equations are pinned via BITGC-LEDGER-04 evidence | TODO |

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
