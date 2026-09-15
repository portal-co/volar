# Private-key FHE deferral with garbled-circuit boundaries and storage threading

**Status:** research and architecture plan, 2026-09-14. This is a more
plausible direction than replacing the whole current garbling construction with
reusable FE/ABE/iO-style garbling, but it is not yet a deployable protocol.

The two executable, copied scenario models are:

```sh
python3 docs/research/hybrid_fhe_gc_cost.py
python3 docs/research/hybrid_fhe_storage_cost.py
```

They deliberately accept external measured provider values. They do not derive
FHE security parameters, FHE runtime, ORAM cost, or a correctness proof.

## Decision

Explore a hybrid **only** for a long computation whose private inputs,
intermediate state, and output ownership all belong to one FHE-key owner. The
FHE provider evaluates that segment over ciphertexts; the garbled circuit/MPC
handles only an explicitly required boundary: two-party policy/input binding,
selective key operation, or final jointly authorized release.

This is not a general replacement for MPC or garbling. It is particularly not
a solution to generic private mutable RAM. The default remains direct strict
MPC/garbling with Ferret/direct OT. FHE deferral is a feature-gated provider
experiment, and the current TFHE module is explicitly Unpinned and Very
unstable.

## What in the proposed framing works

### Long one-owner computation is a good FHE target

If only one party contributes secret state to a long segment, FHE can prevent
the evaluator from learning that state while moving the long gate sequence off
the garbled-table stream. The FHE evaluator receives ciphertexts plus an
evaluation key; it returns ciphertexts. This can be useful if the selected FHE
scheme/provider has a measured advantage for that workload—especially packed
arithmetic, SIMD, programmable bootstrap LUTs, or accelerator-friendly kernels.

A private/symmetric FHE interface is sufficient for the **encryption** side:
standard FHE syntax can have `KeyGen -> (sk, evk)`, secret-key encryption, and
public evaluation under `evk`. Public-key encryption is not required merely
because a server evaluates. [GSW13] [FHE-LECTURE]

### A garbled boundary is sometimes meaningful

A GC can evaluate a *fixed, bounded* FHE encryption/decryption or key-release
circuit if its actual byte representation and key material are inputs to the
GC. This can enforce a joint policy around a one-owner FHE key without exposing
the key outright. The initial reusable-GC/FE construction is an important
precedent: it uses leveled FHE and a garbled FHE decryption component. [GKP+13]

The bounded boundary may be compiled from LLVM/ERT through a future
`FheProvider` adapter. The provider must expose a deterministic, versioned,
fixed-shape boundary circuit and a separate host evaluator implementation; no
compiler pass may treat an opaque FHE library call as proven equivalent.

## What does **not** work as stated

### “Split AES infrastructure” does not automatically split FHE keys

AES split-key use is XOR-linear and tailored to one block-cipher circuit. FHE
secret keys, evaluation keys, bootstrapping keys, key switching, ciphertext
validity, and rounding are scheme-specific. XOR-splitting an FHE secret key and
putting a decryption formula in a GC may be possible for a specified scheme,
but it is **not** a generic key-splitting adapter.

Threshold/collective FHE exists, but secure partial decryption is nontrivial:
naïvely opening linear phase shares can leak noise and aid key recovery; common
solutions require smudging noise or MPC for rounding. Consequently, use a GC
for final decryption only after a scheme-specific proof/implementation design,
not as a shortcut around threshold-FHE design. [PARK-ROVIRA] [THRESHOLD-FHE]

### Bootstrapping/evaluation keys are sensitive protocol artifacts

Private-key FHE does not mean no public material. For bootstrappable schemes,
the evaluation/bootstrapping key commonly contains encryptions of secret-key
material; circular/KDM assumptions or a scheme-specific alternative may be
required. The provider must name exactly what it publishes, under what
assumption, and how it is bound to a session/key epoch. [BOOTSTRAP-SOK]

A GC containing a full evaluation key or a complete ciphertext as bit labels
can be worse than the deferred computation. For example, with a structural
2,524-byte legacy LWE ciphertext, merely supplying one full ciphertext as
16-byte GC input labels is 2,524 × 8 × 16 = 315.5 KiB of labels before the
boundary circuit. Keep ciphertexts provider-local whenever possible; GC should
receive a compact commitment, public handle, or only the fixed data actually
needed for a measured boundary.

### This does not solve generic private storage / ORAM

The user framing is right to treat generic persistent private storage as a
separate hard problem, but “ORAM requires FE/iO” is too broad:

- normal interactive Path ORAM works under conventional symmetric cryptography;
- Garbled RAM has several constructions under different assumptions, including
  IBE/OWF variants and recent concrete work;
- RAM-FHE/reusable persistent RAM is harder because an evaluator can rewind or
  run many programs on the same encrypted state, and known strong solutions
  introduce rewindable ORAM plus VBB/iO-like machinery or strong assumptions.
  [GHL+14] [HHWW19]

The hybrid plan therefore does **not** claim *native FHE oblivious mutable
storage*. Existing ORAM remains supported, but is an explicit boundary back to
the established strict path:

```text
FHE ciphertext/opaque handle -> fixed GC boundary -> authorized decrypt
  -> existing ORAM operation -> encrypt/validate -> FHE ciphertext handle
```

The ORAM access therefore retains the current garbled-circuit/decrypt/ORAM
semantics and its existing transport, versioning, and root-commit constraints;
it must not be represented as an FHE-native read or write. FHE deferral may
resume only after the result is re-encrypted and validated under the same
provider/key epoch.

A direct evaluator-hosted ciphertext store is allowed only under one of these
narrower contracts:

1. public addresses and an accepted access-pattern leak (an optimization, not
   ORAM);
2. the explicit GC/decrypt/existing-ORAM/re-encrypt boundary above; or
3. a separately specified FHE-ORAM/GRAM provider with its own security proof.

## Provider module and deep seam

Add a feature-gated, provider-neutral module—conceptually `FheProvider`—at the
same deep seam as batched input-label delivery. It must expose a small
interface, not leak FHE internals to passes:

```text
provider_id() / parameter_fingerprint() / key_epoch()
boundary_shape(operation) -> fixed public circuit descriptor
submit(ciphertext inputs, public operation descriptor) -> ciphertext outputs
validate(ciphertext frame, binding) -> opaque ciphertext handle
```

The provider owns all scheme-specific ciphertext formats, evaluation keys,
noise/level management, relinearization, bootstrap scheduling, key switching,
and FHE metrics. The GC runner sees only public handles and fixed boundary
circuits. Any output decryption is an explicitly named provider operation with
a policy binding—not a generic `decrypt` action callable by arbitrary guest
code.

### Admission requirements

Before a segment may defer to FHE, require:

- a single declared ciphertext/key owner, or a separately reviewed collective
  FHE mode;
- a fixed `provider_id`, parameters, canonical ciphertext frame, key epoch,
  evaluation-key fingerprint, and failure policy;
- static public operation shape and an explicit circuit/LLVM/ERT hash;
- a measured break-even against the all-GC segment; and
- no implicit conversion from ciphertext bytes to garbled labels.

## Boundary designs

### Preferred: one-owner FHE encryption/decryption outside GC

When the FHE owner is permitted to know plaintext inputs/outputs, let that owner
encrypt before the FHE segment and decrypt after it. GC only binds policy or
selects public/other-party controls. This avoids embedding a wide FHE
ciphertext/key circuit in GC.

### Joint-authorized output release

If the owner may decrypt but should only release after a two-party predicate,
run the predicate in GC first, then allow the owner to decrypt/reveal only on
success. Do not feed decrypted plaintext back into an evaluator-controlled
branch or action without a new policy boundary.

### Joint decryption / split key: research-only

If neither party may decrypt alone, use a named threshold/collective FHE
construction or a scheme-specific GC boundary that implements the full secure
rounding/validity protocol. Include malicious ciphertext behavior, noise
masking/MPC rounding, key epoch, and abort semantics. This cannot be inferred
from AES split-key support.

## Tailored storage and ciphertext threading passes

These are **conservative proof-carrying optimizations**, not general semantic
rewrites. They operate only after a provider validates frame identity and after
the compiler has classified address/epoch facts as public.

### 1. Optimistic storage threading with check branches

**Useful idea:** speculate that a ciphertext resident in the local provider
cache remains current; pass its opaque handle forward without decrypting.

**Required form:** emit a public/version/epoch check at the next required
materialization point. On mismatch, take an explicit fallback path that fetches
and validates the provider ciphertext. The check must not depend on a secret
address or plaintext equality. If a mismatch bit is observable, it is a public
cache/epoch event and must be declared acceptable.

**Does not work:** silently assuming a cache hit, or branching on encrypted
ciphertext equality/secret freshness. That leaks or changes failure behavior.

### 2. Storage write identity caching

**Useful idea:** eliminate a write only when the stored opaque ciphertext value
is the exact same SSA/provider handle already resident at the exact same public
address and key/version epoch. This is a syntactic identity optimization, not a
plaintext-equality optimization.

**Required checks:** address, provider ID, key epoch, ciphertext frame digest,
and storage version all match; no intervening possible write/eviction/rekey;
and the storage contract allows a no-op physical operation. This optimization
applies to public-address storage; ORAM writes use the explicit boundary and
retain their physical protocol effects. Fixed-shape/privacy modes may
intentionally retain no-op writes.

**Does not work:** comparing randomized FHE ciphertext byte strings to infer
plaintext identity, or dropping writes just because a GC MUX selected the old
plaintext branch. Re-randomization and version changes make those invalid.

### 3. Distinct-write batching

**Useful idea:** queue writes from FHE evaluation when their target addresses
are already public and statically proven pairwise distinct. Submit one provider
batch with deterministic public ordering.

**Required checks:** no read between the queued write and its commit observes
that location; public addresses are pairwise distinct; every provider operation
is independent; and the provider gives batch atomicity/error semantics. This is
for public-address storage only, not an optimization across the ORAM boundary.

**Does not work:** treating secret symbolic addresses as “probably distinct,”
or claiming Path ORAM whole-path commits parallelize. Existing ORAM planning
already records that roots overlap and root commits serialize.

### 4. FHE ciphertext reads as GC inputs

**Useful idea:** the FHE provider supplies an opaque ciphertext handle/input to
a fixed GC boundary, rather than decrypting it eagerly.

**Required checks:** the boundary consumes a canonical authenticated frame or
provider-local handle; the byte width is measured; and the FHE ciphertext does
not become thousands of independent GC input labels unless the cost model says
it wins.

**Does not work:** declaring arbitrary evaluator-supplied ciphertexts valid.
Ciphertext validity, key epoch, and chosen-ciphertext/decryption behavior are
provider security obligations.

### 5. Lazy decryption with select/MUX preservation

**Useful idea:** keep both old/new FHE ciphertext handles through GC-style
selects. If a select resolves to the unchanged handle, retain the same opaque
path and defer decrypt/refresh until a plaintext-required boundary. This aligns
with existing free-XOR/select/MUX dataflow and avoids needless conversions.

**Required checks:** both alternatives share provider/key/frame domain; the
selected handle remains valid for the later consumer; no operation requires a
fresh noise budget in between; and equality is handle identity, not plaintext
equality.

**Does not work:** assuming an FHE ciphertext can be evaluated indefinitely
because decryption is lazy. Noise/level exhaustion still requires a provider
refresh/bootstrap schedule, independent of host laziness.

## Pass ordering

```text
LLVM or ERT fixed computation
  -> provider compilation / fixed FHE segment descriptor
  -> public-address and key-epoch analysis
  -> conservative handle-SSA threading
  -> identity-write elimination
  -> distinct-public-write queue formation
  -> required cache/version checks + fallback branches
  -> provider batch submission / FHE evaluation
  -> only then fixed GC boundary or authorized decrypt
```

The optimization pass must preserve a sidecar **proof record** for every
elision/queue item: source SSA IDs, public address fact, provider ID/key epoch,
frame digest, storage version, intervening-effect summary, and fallback site.
Tests should invalidate each fact independently and prove that the fallback or
non-elision occurs.

## Cost model

`hybrid_fhe_gc_cost.py` compares all-GC AND-table payload to a boundary GC plus
externally supplied FHE ciphertext/evaluation-key measurements. It explicitly
models the often-missed cost of passing a full ciphertext as GC labels.

`hybrid_fhe_storage_cost.py` models only public static elisions: resident
forwards, SSA-identical write elisions, and provably distinct public-address
batching. It makes no ORAM/FHE privacy or correctness claim.

Use real provider measurements before a product decision. Example only:

```sh
python3 docs/research/hybrid_fhe_gc_cost.py \
  --gc-encrypt-ands 16000 --gc-decrypt-ands 16000 \
  --fhe-input-ciphertexts 16 --fhe-output-ciphertexts 1 \
  --fhe-ciphertext-bytes 2524 --fhe-evaluation-key-bytes 1048576
```

This reports a 1.95 MiB illustrative boundary table payload versus a 7.58 MiB
all-GC Thumb SHA-256 baseline, plus 41.9 KiB ciphertext frames and a 1 MiB
illustrative evaluation key. It says nothing about FHE evaluation latency or
security, and the boundary AND counts are placeholders until compiled from a
real provider.

## Research sequence

1. Freeze the FHE provider contract and select one scheme with a maintained,
   independently parameterized implementation. Do not start with the legacy
   `tfhe.rs` Track S surface.
2. Build a clear provider oracle plus LLVM/ERT fixed operation description for
   `encrypt`, `evaluate`, `refresh`, `validate`, and any allowed `decrypt`.
3. Compile and measure a minimal one-owner long segment, first with no storage.
   Compare end-to-end all-GC, all-FHE, and hybrid costs.
4. Add an explicit `FHE handle -> GC -> decrypt -> existing ORAM -> encrypt ->
   FHE handle` boundary, with integration tests proving that every secret
   address uses it and retains existing ORAM semantics.
5. Add opaque-handle SSA threading and its proof-record tests for
   public-address storage only.
6. Add only syntactic identity write elision, then public-distinct batching,
   then optimistic cache checks—each under a separate feature flag and
   adversarial invalidation tests. Do not apply them across an ORAM boundary.
7. Consider a jointly authorized output boundary. Treat split/threshold FHE as
   a separate research project after the one-owner path has a measured win.
8. Do not claim *native FHE ORAM* until a provider-specific storage construction
   clears its own security and replay/rollback model; the explicit fallback to
   existing ORAM remains supported independently.

## Sources

- **[GKP+13]** Goldwasser, Kalai, Popa, Vaikuntanathan, Zeldovich,
  *Reusable Garbled Circuits and Succinct Functional Encryption*, IACR ePrint
  2012/733. https://eprint.iacr.org/2012/733
- **[GSW13]** Gentry, Sahai, Waters, *Homomorphic Encryption from Learning
  with Errors*, IACR ePrint 2013/340. https://eprint.iacr.org/2013/340
- **[BOOTSTRAP-SOK]** *Demystifying Bootstrapping in Fully Homomorphic
  Encryption*, IACR ePrint 2023/149.
  https://eprint.iacr.org/2023/149
- **[GHL+14]** Gentry, Halevi, Lu, Ostrovsky, Raykova, Wichs, *Garbled RAM
  Revisited*, IACR ePrint 2014/082. https://eprint.iacr.org/2014/082
- **[HHWW19]** *Fully Homomorphic Encryption for RAMs*, IACR ePrint 2019/632.
  https://eprint.iacr.org/2019/632
- **[PARK-ROVIRA]** Park and Rovira, *Efficient TFHE Bootstrapping in the
  Multiparty Setting* (primary-source result located during research; exact
  parameter/implementation binding required before use).
- **[THRESHOLD-FHE]** *Threshold FHE without Noise Flooding* (primary-source
  threshold-decryption direction; exact scheme/profile binding required before
  use).
- Existing repository constraints: `docs/fhe/README.md`,
  `docs/fhe/tfhe-two-track-cleanup-plan.md`, `docs/agent-context/oram.md`, and
  `docs/handoffs/strict-material-oram-transport-issue.md`.
