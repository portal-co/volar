# Pre-FHE storage scheduling infrastructure — v1 exhaustive deferred-test log

**Status:** implementation infrastructure only, 2026-09-14.

This v1 landing intentionally has **no FHE provider, no FHE key/ciphertext
format, no FHE encryption/decryption, and no test pinned to an insecure or
legacy FHE implementation**. It makes two currently applicable transforms
explicit at the compiler/runtime scheduling seam so a reviewed provider can
later swap a long deferred chunk into a stable plan.

The implementation is:

- `crates/vc/volar-vc/src/hybrid_storage.rs`
- `crates/mpc/volar-mpc/src/strict_chain.rs`

## Implemented interface

`PreFheStoragePlan` is a public, provider-neutral plan with three isolated
parts:

1. **`oram_reads: Vec<OramReadPreRun>`** — a declared, ordered set of
   secret-address ORAM reads to perform before a future FHE chunk. Each item
   has only a public schedule token, pre-access ORAM epoch, and future input
   slot. It does **not** contain a logical address, physical leaf, result,
   label, plaintext, or ciphertext.
2. **`held_material: Vec<HeldMaterialPreOpen>`** — lazy durable held-material
   opens to execute through the existing split-AES `HeldMaterialStore` at a
   `ChainStoragePhase` boundary. `held_prefetch_operations()` emits the new
   explicit `StorageOperation::Prefetch` form.
3. **`public_address: PublicAddressStoragePlan`** — a wrapper around the
   existing public-only Path-ORAM read reuse and disjoint-below-root write
   planning. It is deliberately separate from secret-address ORAM pre-runs.

`PreFheStoragePlan::new` currently validates scheduling facts that require no
cryptographic provider:

- unique ORAM schedule tokens;
- unique future deferred-input slots;
- exact contiguous ORAM epoch sequence, with overflow rejection; and
- canonical held-material demand merging by slot (`Garbler` + `Evaluator`
  becomes `Both`).

The plan is intentionally not an execution engine. The existing strict ORAM
runner still owns secret addresses and access semantics. The paired durable
material adapters still own AES, OT, epochs, cache state, and ciphertext
storage. A future reviewed provider owns encrypt/evaluate/validate/decrypt.

## Semantics fixed now

### 4a: pre-run ORAM reads

```text
strict GC secret-address ORAM read
  -> opaque result in the existing role-local path
  -> future reviewed provider encrypt
  -> deferred FHE chunk
```

The plan is a `MustConsumeBeforeMovfuscation`-style intent, but v1 does not
add IR instruction-group variants because neither the front-end annotation nor
the provider-chunk lowering contract is stable yet. A future compiler pass
must consume this plan *before* movfuscation. If it cannot prove the bounded
read schedule, it must leave/split the chunk at a normal explicit
`GC -> decrypt -> ORAM -> encrypt -> FHE` boundary. It must never attempt an
ORAM action from inside flattened movfuscated dataflow.

### 5a: lazy AES held-material opening

```text
held ciphertext block
  -> ChainStoragePhase::Prefetch
  -> direction-specific split AES open circuit
  -> role-local held labels/bases
  -> strict GC / ORAM work
```

`StorageOperation::Prefetch` is intentionally an alias of the established
boundary-only material load behavior. Its name records that it prepares a
future chunk; it does not introduce a second storage protocol. The underlying
adapter keeps the directional ownership rule:

- `OpenGarbler` returns only a garbler false-label base;
- `OpenEvaluator` returns only an evaluator active label;
- `Both` expands to paired role-local transactions;
- neither host combines or locally decrypts material.

## Explicit non-goals

- No native FHE ORAM.
- No evaluator-hosted secret-address map/cache.
- No test or benchmark of legacy `tfhe.rs` / Track S as a production provider.
- No claimed FHE parameter, security level, ciphertext size, runtime, key
  split, bootstrapping material, or threshold-decryption semantics.
- No conversion of opaque FHE ciphertext bytes into GC bit labels.
- No storage transport started inside an ordinary strict circuit round.
- No public-address batching across a secret ORAM operation.

## Exhaustive v2 test log

These tests are intentionally deferred until a maintained, independently
reviewed provider adapter and its parameter/profile binding exist. They should
be added at the public seams named below, not by inspecting private state.

| ID | Seam / scenario | Required independent oracle | Status | Reason deferred |
|---|---|---|---|---|
| V2-ORAM-PRERUN-01 | `PreFheStoragePlan::new`: ordered pre-runs through the real split ORAM driver | Existing non-FHE ORAM reference / strict driver result | TODO | Requires actual reviewed provider handoff of opaque results |
| V2-ORAM-PRERUN-02 | stale epoch, replayed manifest, failed access, retry | Driver success-only epoch and transcript result | TODO | Must bind a provider/session epoch without using legacy TFHE |
| V2-ORAM-PRERUN-03 | aliasing write before planned read is rejected or commits first | Reference sequential ORAM trace | TODO | Requires compiler dependency analysis / front-end annotations |
| V2-ORAM-PRERUN-04 | bounded conservative superset and opaque selection | Reference strict circuit result and access-shape trace | TODO | Requires stable IR marker/lowering contract |
| V2-ORAM-PRERUN-05 | manifest miss forces an explicit new boundary | Driver transcript/frame sequence | TODO | Requires chunk scheduler and provider adapter |
| V2-HELD-OPEN-01 | `Prefetch` then later `ChainFeed::Held`, both roles | Existing paired durable material TCP/OT behavior | TODO | New plan needs integration only after scheduling consumer exists |
| V2-HELD-OPEN-02 | `Both` expands to two correct directional opens | Directional material protocol and role-local outputs | TODO | Must test over real paired adapter, not a local decrypt fake |
| V2-HELD-OPEN-03 | stale version / rekey / eviction invalidates a planned open | Adapter rejection and public epoch counters | TODO | Requires scheduler-to-adapter epoch binding |
| V2-HELD-OPEN-04 | no recursive transport while strict round runs | Strict transport transcript ordering | TODO | Requires a full plan executor |
| V2-PUBLIC-01 | repeated public read reuse | Existing `oram_batch` reference plan plus durable tree behavior | TODO | Network-tree integration remains pending |
| V2-PUBLIC-02 | non-root-disjoint grouping retains serialized roots | Physical tree I/O trace / reference Path ORAM result | TODO | Network-tree integration remains pending |
| V2-PROVIDER-01 | pre-run opaque result encrypts into declared input slot | Provider's independently verified encrypt/decrypt oracle | TODO | No reviewed provider selected |
| V2-PROVIDER-02 | FHE chunk result re-enters explicit ORAM boundary | End-to-end strict ORAM + provider reference computation | TODO | No reviewed provider selected |
| V2-MOVF-01 | required pre-run/held-demand marker is consumed before movfuscation | IR evaluator before/after pass + marker rejection | TODO | IR representation not pinned yet |
| V2-MOVF-02 | unconsumed marker makes movfuscation/lowering fail closed | Compiler diagnostic | TODO | IR representation not pinned yet |

## Required v2 design gates

Do not implement the deferred tests or a provider adapter until all are true:

1. A specific maintained FHE implementation and parameter set is reviewed and
   version-pinned in a dedicated provider module.
2. The provider specifies canonical ciphertext frames, evaluation-key material,
   parameter fingerprint, key epoch, validation behavior, error/abort rules,
   and measured profile.
3. The compiler has a stable pre-movfuscation marker/consumer contract for
   bounded ORAM reads and held-material demand.
4. An end-to-end test can use an independent expected result without exporting
   a secret address, held label, key share, or plaintext to either host.
5. The test matrix above is revised—not silently deleted—with a concrete
   provider binding and an explicit decision for each row.
