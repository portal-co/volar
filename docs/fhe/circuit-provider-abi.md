# Self-contained circuit-provider ABI (draft)

**Status:** design record. **Pinnedness:** unpinned. **Stability:** very
unstable. This defines compiler-facing circuit geometry only. It neither
selects a BinFHE parameter set nor defines ciphertext framing, key transport,
security, replay protection, or a deployable provider.

Read [`future-provider-integration-ledger.md`](future-provider-integration-ledger.md)
before turning any item here into an imported artifact.

## Decision

The provider is a **circuit factory**, not a runtime cryptographic service. It
builds three ordinary, finite Boolean circuits:

1. key derivation;
2. randomized encryption; and
3. decryption.

A compiler imports/lowers each program, calls
`unroll_ir_everything_unbounded`, and inlines the resulting single-block
Boolar circuit directly at the consuming site. It then runs the normal,
deterministic Boolean optimization fixpoint (constant folding, CSE, and DCE)
on the *combined* circuit before garbling.

There is deliberately no external `derive_key`, `encrypt`, `decrypt`, RNG, or
ciphertext-object call in a garbled schedule. That prevents an invocation from
escaping the optimizers and makes every required wire dependency visible to the
same schedule that evaluates the guest.

“Unbounded” means **no compiler-imposed unroll cap**, not “support unbounded
programs.” A provider program must be statically finite. Symbolic control,
non-finite control, or allocation failure is a compilation failure; it must
never silently turn into a bounded circuit.

## Bit and ordering conventions

Every ABI item is an ordered vector of Boolean wires. Byte values are
byte-major and LSB-first within each byte, matching `volar_vc::aes_gadget`.
`concat(A, B)` means `A` immediately followed by `B`. An ABI descriptor must
name the exact bit lengths below and reject a mismatch before composition.

`SeedId`, `ProviderProfileId`, `KeyEpoch`, `CircuitId`, and `UseId` are public,
compile-time or session-bound domain-separation values. They are not secret
wires. Their encoded bits enter the circuit as constants, so the combined
optimizer removes their construction and folds all constant-only work.

## Fixed circuit interface

A selected provider profile supplies the following fixed geometry:

```text
seed_half_bits          // normally 128; chosen profile value
key_bits
plaintext_bits
ciphertext_bits
randomness_bits
key_label_bits
ciphertext_label_bits
```

The labels are public fixed-width encodings. They distinguish the key schedule
from ciphertext randomness and distinguish every key/ciphertext use. Their
format belongs to the ABI descriptor, not to individual callers.

### 1. `derive_key`

```text
inputs:
  seed_left[seed_half_bits]
  seed_right[seed_half_bits]
outputs:
  key[key_bits]
```

The two seed halves are independently supplied private input shares. The
program reconstructs a derivation seed *inside the circuit* and obtains its
keystream material from a fixed-shape AES-GCM-based derivation construction:

```text
KDF(seed_left || seed_right,
    domain = "volar/fhe/circuit-key/v1",
    profile, key_epoch, circuit_id, key_label)
  -> key_bits
```

The precise AES-GCM construction (AES key extraction, IV derivation, AAD and
counter layout, and keystream block count) is a **profile-owned fixed circuit**.
It must be published with a scalar test oracle before an artifact is admitted.
The generic ABI intentionally does not call this “raw AES-GCM”: encryption
keystream bytes are only one input to a defined KDF construction, never an
unauthenticated AES-GCM plaintext/ciphertext interchange format.

`derive_key` is inlined once for every distinct `KeyUse` required by surviving
operations. A repeated `KeyUse` maps to the same output wire vector and is not
re-derived.

### 2. `encrypt`

```text
inputs:
  key[key_bits]
  plaintext[plaintext_bits]
  randomness[randomness_bits]
outputs:
  ciphertext[ciphertext_bits]
```

The caller supplies independent private random wires. They are created as
ordinary circuit inputs or, where the protocol has a reviewed joint-randomness
source, ordinary wires from that source; the provider never reaches for a host
RNG. The encryption algorithm and its exact ciphertext expansion belong to the
selected fixed profile.

Each encryption has a distinct `CiphertextUse` label and consumes a distinct
randomness vector. Reusing `(key wires, ciphertext label, randomness wires)`
is a compile-time error. Optimizers may share **deterministic key derivation**;
they must never CSE encryption across separate ciphertext uses merely because
the visible dataflow is structurally equal.

### 3. `decrypt`

```text
inputs:
  key[key_bits]
  ciphertext[ciphertext_bits]
outputs:
  plaintext[plaintext_bits]
```

Decryption has no randomness input. Whether a profile emits a validity bit is a
separate future ABI version; V1 fails closed by admitting only profiles whose
decrypt circuit has exactly the declared plaintext output geometry.

## Composition plan

The interface at the compiler seam is intentionally small:

```rust
plan.compose(guest_circuit, provider_descriptor, requested_operations)
    -> optimized_combined_circuit
```

`requested_operations` is compiler-produced demand metadata, not a user-facing
cryptographic interface. The deep `CircuitProviderComposer` module owns:

- ABI length/profile/label validation;
- full unrolling of every provider program;
- wiring seed halves and randomness into inlined calls;
- deterministic key-use memoization;
- ciphertext-use uniqueness checks;
- normal optimizer ordering; and
- the final mapping from source values to ordinary Boolean wires.

Callers never manually wire KDF/AES, key bits, random bits, or ciphertext bits.
That keeps the cryptographic circuit implementation behind one seam and gives
one place to test source-value lifetime policy.

## Demand-driven wire state

Wire tracking is a conservative compiler analysis. It is not a dynamic runtime
cache and it does not inspect plaintext values.

For every source wire/vector the tracker records one of:

```text
Clear                     ordinary combined-circuit wire(s)
Encrypted { key_use, ciphertext_use }
DecryptRequired { reason }
```

and separately records its consumers. `reason` is one of:

```text
Cached                    persisted / crosses the FHE-region lifetime
FirstNonSelectUse         first consumer is not a pure select branch/selector
```

The only allowed transitions are:

```text
Clear --encrypt-on-merge--> Encrypted
Encrypted --decrypt-on-demand--> Clear
```

A tracker must **not** invoke an expensive circuit merely because a value is
present in an FHE candidate region:

- **Decrypt lazily.** An encrypted wire stays encrypted through eligible
  select-only propagation. Insert/decorate `decrypt` only when it is cached or
  reaches its first non-select consumer. The result is memoized by exact source
  wire identity for that composed circuit, so all later clear consumers share
  the same decrypt output wires.
- **Encrypt at an actual mixed merge.** A clear branch stays clear until it is
  merged via a select with an encrypted alternative. At that select, encrypt
  the clear branch once with a fresh `CiphertextUse` and randomness wires, then
  use the provider's encrypted select representation. Do not pre-encrypt all
  clear values “just in case.”
- **Do not silently decrypt select operands.** If the selected operation is
  unavailable for the profile, the planner must cut the FHE region and record
  `FirstNonSelectUse`; it may not replace encrypted selection with a cleartext
  host observation.
- **Cached is a semantic barrier.** Caching forces decryption before the
  cache representation unless a future version defines a durable authenticated
  ciphertext cache with key-epoch binding. A cache lookup must never derive a
  new key or re-encrypt implicitly.

Select tracking must use exact IR wire identity (`IRVarId` in Boolar, and SWC
`Ident`/`Id` where it originates in SWC) rather than a textual name. Same-named
bindings are distinct values.

## Optimizer contract

After all demanded KDF/encrypt/decrypt circuits are inlined, run to a stable
fixpoint:

```text
fold_biir_blocks -> cse_biir_blocks -> dce_biir_blocks
```

Repeat if a later composition/rewrite adds a new constant or alias. The pass
may remove dead provider work and fold public labels/domain-separation values.
It may CSE pure Boolean gates and deterministic KDF subgraphs *within one
identical `KeyUse`*. It must treat encryptions as use-distinct even if their
ordinary Boolar implementations happen to look identical: each one consumes
fresh randomness and represents a separate ciphertext event.

The final schedule compiler already applies this same fold/CSE/DCE sequence;
the composer runs it before demand accounting is finalized so dead requests do
not allocate input randomness or key-derivation work.

## Required descriptor and admission checks

A concrete profile descriptor must include:

- ABI version and profile fingerprint;
- all fixed bit lengths and label encodings;
- a finite program for KDF, encrypt, decrypt, and supported encrypted select;
- proof-by-validation that each program unrolls fully with the **unbounded**
  unroller and yields a single return circuit;
- an independently testable scalar oracle and known-answer vectors for KDF,
  encrypt, decrypt, and each select shape;
- key/ciphertext label uniqueness validation;
- tests for lazy decrypt reuse, cached decrypt, mixed-select encrypt, and
  dead-demand removal after optimization; and
- session bindings for profile fingerprint, key epoch, circuit identifier,
  seed contribution commitments, and randomness provenance.

Until those exist, this document is a design contract only. In particular,
`PlanBoundary` remains the current narrow internal BinFHE host conversion seam;
it is not replaced by this draft.

## Deferred ledger IDs

- `FHE-PLUMB-CIRCUIT-ABI-01`: descriptor + full unroll/composition fixture.
- `FHE-PLUMB-CIRCUIT-ABI-02`: AES-GCM-based KDF profile and independent oracle.
- `FHE-PLUMB-CIRCUIT-ABI-03`: demand tracker and optimizer correctness fixture.
- `FHE-PLUMB-CIRCUIT-ABI-04`: session/key-epoch/randomness binding.
