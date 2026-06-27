# The boundary-link embedding (VOLE ⇄ folding)

**Status:** Chosen scheme = **dual-preimage Keccak digest**. **Folding leg
DONE**: the Keccak reference + SHA3-256 ([`keccak.rs`](../crates/fold/volar-fold/src/keccak.rs),
anchored to the `sha3` crate); the now-fast field/MSM that make it practical; the
**Keccak-in-R1CS arithmetization** ([`keccak_r1cs.rs`](../crates/fold/volar-fold/src/keccak_r1cs.rs),
~155k constraints/Keccak-f, cross-checked against the reference); and
[`KeccakDigestLink`](../crates/fold/volar-fold/src/link.rs) — the folding-committed
boundary is proved (in-circuit) to hash to a public `d` and Pedersen-bound, with
e2e bridge tests. **VOLE leg circuit DONE**: [`emit_keccak256`](../crates/compiler/volar-weaver/src/gadgets.rs)
emits the same Keccak as a boolean `GateBuf` gadget (XOR/NOT free, χ's ANDs →
hats), cross-checked against the `sha3` crate. **VOLE-leg lowering DONE**:
[`weave_keccak_check`](../crates/compiler/volar-weaver/src/storage_loop.rs) /
`weave_keccak_check_verifier` lower `emit_keccak256` + the **public-digest
equality** (`match = ⋀_i(out_i XNOR d_i)`, prover-opened and verifier
`assert_one_check`-ed) to VOLE prover/verifier IR; the generated code **compiles**
(`cargo check`-validated on a reduced-round circuit; the full 24-round lowering +
printer is exercised structurally). **Spliced into the real gap boundary**
(opt-in, [`hybrid_net.rs`](../crates/compiler/volar-weaver/src/hybrid_net.rs)):
`weave_hybrid_net_vole_prover/verifier` take a `boundary_attest: Option<(&[bool],
usize)>`; when set, the prover's resume seams (B5/B8) emit a `keccak_check_<name>`
call over the committed boundary-state wires, **open** the `match` bit
(`vope_open_mask`) and **send** it; the verifier emits the matching
`keccak_check_verify_<name>`. Default `None` is byte-identical to before.
**Transport adapter DONE**: [`FoldingTransport`](../crates/fold/volar-fold/src/transport.rs)
is a concrete `ResilientVoleTransport` whose `prover_bridge`/`verifier_bridge`
delegate to `FoldingBridge` — so the generated bridge calls produce a real
`GapVerdict` (`Proven` iff `native_verify` on the folded gap + both
`BoundaryLink`s pass), and `send_opening`/`recv_opening` carry the boundary match
mask; tested honest→`Proven` / boundary-mismatch→`Unproven` through the trait.
**Remaining (one architectural item)**: the fully **in-circuit** VOLE-leg match
(the woven `keccak_check_verify` over the boundary `Q`-wires + streamed χ hats,
`assert_one_check`-ing the opened match) needs a verifier-CFG checkpoint that
carries the gap-boundary `Q`-state — the current hybrid verifier drops the
boundary wires before exit and delegates resume to the transport. The
transport-level check uses the (sound, non-ZK) `KeccakDigestLink` meanwhile; plus
the digest `d` is still baked as gate structure (runtime-`d` variant passes it as
public bits). Companion to [`vcb-ivc-folding.md`](vcb-ivc-folding.md) §4.

## Why this is hard

The boundary state `S` (the ℓ live loop-state bits crossing into / out of the gap)
is committed in **two incompatible worlds**:

- **VOLE / IT-MAC** over a binary extension field `GF(2^k)`: each bit `b_j` has a
  MAC `M_j` (prover) and key `K_j = M_j + b_j·Δ` (verifier), with `Δ` the
  verifier's **secret**.
- **Folding** over the prime field `F_ℓ` (Ed25519 scalar field): the bits are
  packed into a Pedersen commitment over the curve.

Two obstacles:
1. **Cross-field.** `GF(2^k)` addition is XOR; `F_ℓ` addition is integer-mod-ℓ.
   A single linear/algebraic identity cannot bridge them.
2. **`Δ` is verifier-held.** The prover can't run a public Σ-protocol "about" the
   VOLE commitment the way it can about a public-key commitment — IT-MACs aren't
   a prover-side group commitment.

The escape: **operate on the bit-string, which is field-agnostic.** A digest of
the bits is the same object regardless of field.

## Candidate families

### (A) Dual-preimage hash digest — **CHOSEN**
Public `d = Keccak256(S)`. Each proof system independently proves "I know a
preimage of `d` equal to *my* boundary." Collision-resistance ⇒ the two
boundaries are the same bit-string. **No cross-field algebra at all.**
- *Soundness:* reduces cleanly to Keccak collision-resistance + each side's
  preimage soundness. The most clearly-sound option.
- *Cost:* a Keccak-f[1600] in **both** systems, `O(1)` in the gap length `m`.
  Asymmetric: cheap on the VOLE side (XOR free; only χ's `24·1600 = 38 400` ANDs
  cost hats), **expensive on the R1CS side** because **XOR is quadratic over a
  prime field** (`a⊕b = a+b−2ab`), so θ/ι XORs also become constraints —
  ~hundreds of thousands total. Practical only because the field (Montgomery) and
  MSM (Pippenger) are now fast.

### (B) Masked universal-hash (Toeplitz) bit-level link
Verifier sends a random binary Toeplitz matrix `T` *after* both boundaries are
committed; prover reveals `d' = T·S ⊕ r` (`r` = a committed mask) and proves it in
both worlds. Pure **GF(2) bit operations** (parities), which both systems compute
natively: VOLE side ≈ **free linear openings**, R1CS side ≈ `2λ` parity
constraints (`~256`, vs A's hundreds of k). Sound (binding `2^-2λ` from `T`-after-
commit), ZK (mask `r`). **Far cheaper than A**; the leading alternative if A's
R1CS cost bites. Not chosen now only because A's soundness is the most
transparent.

### (C) Homogeneous binary-field folding commitment — research
Replace the prime-field Pedersen on the fold side with a commitment over the same
`GF(2^k)`, making the link a free VOLE linear check. Needs an additively-
homomorphic, binding commitment with a hard relation over a binary field
compatible with Nova folding — an open design question.

## Chosen scheme in detail (dual-preimage Keccak)

**Digest.** `d = Keccak256(S)` (single Keccak-f[1600]; the boundary ≤ the
1088-bit rate ⇒ one absorb block + 256-bit squeeze). `d` is a public 256-bit
string sent once on reconnect.

**Folding side (R1CS over `F_ℓ`).** The folded instance's boundary step includes
`Keccak(S_fold) = d` as constraints; `native_verify` then covers it. Build by
lowering a **boolean circuit** to R1CS generically:
- represent each wire as a linear combination `L` over the witness (`map var→coeff`
  + constant);
- `NOT L = 1 − L` (LC, free); `AND(L1,L2)`: fresh var `p`, constraint
  `(L1)·(L2) = p`; `XOR(L1,L2) = L1 + L2 − 2p` with `p = L1·L2` (one constraint);
- bit-rotations/permutations (ρ, π) are wire re-indexing (free);
- input bits boolean-constrained (`b² = b`); output 256 bits constrained to `d`.

The gate-level Keccak (θ/ρ/π/χ/ι over 1600 bit-wires × 24 rounds) is written once
and **cross-checked against `keccak256_bits`** (the sha3-anchored reference); the
witness builder evaluates the gates on the boundary bits.

**VOLE side.** The streamed proof, at the gap boundary, weaves the same Keccak as
a boolean gadget — [`emit_keccak256`](../crates/compiler/volar-weaver/src/gadgets.rs)
over `GateBuf` (reusing `xor`/`not`/`and`), **built and sha3-verified** — to be
lowered via the existing `lower_gadget_{prover,verifier}` (XOR/NOT free, AND →
`vole_and_prover_step` hats), constraining the squeezed output to the public `d`.
Reuses the exact pattern of the sortedness gadget in `storage_loop`. (Cross-crate:
this lives in `volar-weaver`; the digest `d` is the shared public value.) The gate
count is similar to the R1CS side (~155k gates), but **only χ's 24·1600 = 38 400
ANDs cost hats** — θ/ι XORs are free in the VOLE cost model (unlike R1CS, where
XOR is quadratic).

**`KeccakDigestLink: BoundaryLink`** carries `d`; `verify` confirms (a) the folded
instance — which includes the Keccak(`S_fold`)=`d` constraints — passes
`native_verify`, and (b) the `d` it holds equals the `d` the VOLE verifier checked.

### Security
Completeness: honest boundary hashes to `d` on both sides. Soundness: if the two
boundaries differed, they'd be a Keccak collision for `d` (negligible);
preimage-knowledge on each side is the respective proof system's soundness. ZK:
`d` is a hash of the (committed, hidden) boundary; if leaking the digest is a
concern, mask with a committed nonce (as in family B) — for a deterministic
boundary the digest reveals only what a commitment-opening oracle would, and the
preimage proofs are themselves ZK.

### Cost recap
One Keccak-f each side, `O(1)` in `m`; VOLE side `38 400` AND hats; R1CS side
~hundreds of k constraints (XOR-quadratic). Tractable now that `F_ℓ` is Montgomery
and MSM is Pippenger (~seconds, not minutes). The digest CRHF is **pluggable** —
a future swap to family (B)'s Toeplitz link, or an arithmetization-friendlier
construction, drops in behind the same `BoundaryLink` trait.
