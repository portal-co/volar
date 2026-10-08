# BitGC artifact construction notes (wtd2/more-efficient-bitgc)

**Status:** research notes, 2026-10-08. Extracted from the authors' public
artifact at <https://github.com/wtd2/more-efficient-bitgc> (cloned at
`/tmp/more-efficient-bitgc`, commit `main` HEAD). These notes back the
paper-pinning work in `docs/bitgc-variants-plan.md` §10 and the
`GateEquation` seam in `volar_spec::bitgc`.

The artifact is the EUROCRYPT 2026 paper's ([ZLL+26], ePrint 2025/1810)
reference implementation, exposing `--variant=fast|small`.

---

## Critical structural finding: HSS, not plain SWHE table-assembly

The artifact does **not** reconstruct garbled tables purely by SWHE
ciphertext arithmetic and decrypt once at the end. Its per-gate evaluation
uses a **Homomorphic Secret Sharing (HSS)** layer with **distributed
decryption (DD)** and a **determination bit** exchanged per gate.

This is materially different from the `ReferenceEquation` shipped in
`volar_spec::bitgc`, which unwraps `(L − A0)·Δ⁻¹` and reassembles a table
homomorphically with no per-gate interaction. The artifact's construction
is the actual BitGC family structure; the reference adapter is a
placeholder with the same *communication shape* (1 or 5 bits/gate) but a
different and weaker security posture. **The reference adapter must be
replaced, not merely re-parameterized** (BITGC-LEDGER-04).

### Wire-label representation (HSS)

Each wire carries an `HShare` — a pair `(pt_share, value)`:

- `pt_share`: an `Rp` polynomial (mod the plaintext modulus) holding the
  party's *additive share* of the wire's masked label.
- `value`: a bit. For the garbler, always `0`; for the evaluator, the
  actual permute/masked bit.
- A share is `determined` once its value bit is known; `HSSMul` and `DD`
  require determined boolean inputs.

The global offset Δ is **the SWHE secret key `sk` itself**, embedded in
`Rp` (ternary coefficients mapped to `{0, 1, p−1}`); `LSB(Δ)=1`. Labels
relate by `A1 = A0 ± Δ (mod p)`, with the sign chosen by `A0`'s LSB
(`GetPositiveLabel`: LSB 0 → `+Δ`, LSB 1 → `−Δ`). This is the paper's
`A1 = A0 + (−1)^π·Δ` with `π` folded into the label's own LSB.

### The `DD` (distributed decryption) primitive

`DD(ct_rlwe_m_sk, x)` where `ct = (u1, u2)` with `u1 + u2·sk = m·sk +
p·e` and `x` is a determined boolean share:

- `val = 0`: result share = `x_share · u2`
- `val = 1`: result share = `u1 + x_share · u2`

The output is an *undetermined* share of `x · m`. So DD turns an encrypted
bit-times-key plus a boolean share into a share of their product — this is
how the garbled "table" rows get multiplied by the active wire labels
without revealing them.

### Determination bits (the actual per-gate communication)

After the HSS combination for a gate, the share is undetermined. The two
parties exchange the **LSB of their shares** to recover the gate's masked
bit:

- Garbler: `DetermineGen` — reads its share's LSB, sets its value to 0,
  returns that LSB (`garble_bit`, 1 bit).
- Evaluator: `DetermineEva(h, garble_bit)` — its value becomes
  `own_lsb XOR garble_bit`.

So the garbled circuit's per-gate payload is the **garbler's share LSB**
— one bit per gate whose output must be determined. This is the "stitching
bit."

---

## BitGC-small (§4) — `src/protocol/small_{gen,eva}.cc`

**Offline bits required:** `num_wires − inv_gate_count` (one mask bit per
non-INV wire).

### AND gate

Permutation bits `π_a, π_b, π_c` (per-wire masks from the offline
expansion); gate mask bit `m0 = π_c ⊕ (π_a ∧ π_b)`.

Garbler (and evaluator, identically on the ciphertext side) builds the
3-ciphertext garbling table from the *mask ciphertexts* `ct_a, ct_b, ct_c`
(encryptions of the wire mask bits):

```text
s      = (1 − ct_c) − ct_c          = 1 − 2·ct_c
u0     = ct_b  · (1 − 2·ct_a)
u1     = ct_a  · (1 − 2·ct_b)
u2     = (1−2·ct_a)·(1 − 2·ct_b)
table  = [ s·u0 , s·u1 , s·u2 ]     (then each RGSW-external-multiplied
                                     and mod-switched to DD level)
```

Then the label computation (HSS):

```text
t1      = DD(table[0], label_a)
t2      = DD(table[1], label_b)
label_ab= HSSMul(label_a, label_b)
t3      = DD(table[2], label_ab)
label_c = t1 + t2 + t3
if m0:  label_c += (−Δ)            // garbler: HSSAddPlain(pt_neg_delta)
garble_bit = DetermineGen(label_c) // 1 bit emitted
```

Evaluator runs the same `DD`/`HSSMul`/`HSSAdd` chain, then
`DetermineEva(label_c, garble_bit)`.

### XOR gate

```text
garble_bit = π_a ⊕ π_b ⊕ π_c
label_c    = (label_a + label_b) − 2·HSSMul(label_a, label_b)
if garble_bit:  label_c = −label_c + (−Δ)   // garbler
evaluator:      label_c.value = σ_a ⊕ σ_b ⊕ garble_bit
```

XOR is **charged one bit** (the `garble_bit`), matching Table 1.

### INV gate

```text
label_c = label_a          (share unchanged)
ct_mask_c = 1 − ct_mask_a  (CtNot)
```

---

## BitGC-fast (§5.1) — `src/protocol/fast_{gen,eva}.cc`

**Offline bits required:** `4 × and_gate_count`
(`PRG_PER_GATE = kLegacyPrgPerGate = 4`).

### AND gate — no online HSS-ciphertext multiplications

Four per-gate mask bits from the offline expansion:
`m = [ π_c⊕(π_a∧π_b), π_c⊕(¬π_a∧π_b), π_c⊕(π_a∧¬π_b), π_c⊕(¬π_a∧¬π_b) ]`
(all four truth-table rows of the AND, masked by π_c).

Each is XOR-corrected against a plaintext random bit (`pt_rbit`): the
**communicated** value is `gate_mask_bits[i] ⊕ pt_rand_bit` — 4 bits per
gate, recorded in `F.prg_mask_bits`. The evaluator flips its offline mask
ciphertext accordingly (`ct_flip = ct_sk − ct_rbit` when the mask bit is
set). Then a **3-entry table by subtraction only** (no HE
multiplication — this is the "no online homomorphic multiplications"
property):

```text
table[0] = m1 − m0
table[1] = m2 − m0
table[2] = (m3 − m1) − (m2 − m0) = m3 − m1 − m2 + m0
```

Label computation identical shape to small (DD·label_a, DD·label_b,
DD·HSSMul(a,b)) and `DetermineGen` emits 1 bit (`garble_bits[wire_c]`).

**Total communicated per AND: 4 mask bits + 1 garble bit = 5 bits** —
matching Table 1's "5 bits per AND."

### XOR gate — free

```text
π_c = π_a ⊕ π_b            (mask derived locally, no record)
label_c = (label_a + label_b) − 2·HSSMul(label_a, label_b)
```

No communication, no ciphertext multiplication — matching Table 1's
"0 bits per XOR." (Note: the HSS share-level `HSSMul` is on plaintext
shares, not an HE ciphertext operation.)

---

## The HSS multiplication (`HSSMul`)

Uses a **CP-RLWE triple** (a key-dependent precomputed bundle in
`HSSEvalKey`):
`s·common + zero = 0 (mod t)` and `s²·common + secret = −s (mod t)`.

For shares `A` (value `va`), `B` (value `vb`):

```text
linear = va·B + vb·A                          (value-dependent)
accum  = A·B·common + linear·zero
       − va·vb·secret
result.value = va · vb
```

This is a bilinear map on the shares that produces a share of the product
of the underlying masked values — the HSS analogue of a multiplication
triple, computed without any network round.

---

## Offline pipeline (`src/protocol/offline.*`, `src/he/goldreich_prg.*`,
`src/unpack/*`)

1. **Seeds:** encrypted seed ciphertexts (`ct_seeds`), garbler also holds
   plaintext mirrors.
2. **PRG:** a **Goldreich-style low-depth PRG** (`GoldreichPRG`) expands
   the seeds homomorphically into many encrypted bits (this is the
   [LWYY25b] seed expansion).
3. **Unpack:** `unpack/` (`slot2coeff`, `ring_dim_reducer`, `expander`)
   converts the SIMD-packed large-ring expansion into per-bit small-ring
   RLWE ciphertexts — the "extract individual ciphertexts from packed SIMD
   slots" step.
4. **Modulus management:** a two-context shape (large offline ring →
   small online ring) with explicit mod-switch counts, matching the
   `offline-dim`/`online-dim` artifact CLI.

---

## Implications for `volar_spec::bitgc`

| Reference-adapter assumption | Artifact reality | Action |
|---|---|---|
| Δ is a small odd plaintext constant | Δ **is the SWHE secret key** (full ring element, LSB 1) | `GlobalDelta` must become a ring-element / key-bound value |
| Wire label = `A0 + b·Δ` bit payload | Wire label = additive **share** of a masked ring element; value recovered by cross-party LSB determination | Add an HSS-share layer; per-gate 1-bit determination replaces the self-contained stitch |
| Table assembled from `Enc(active)` by mults | Small: monomial mask-ciphertext products + RGSW external-mul; Fast: 4 corrected mask bits, subtraction-only table | Rewrite `GateEquation::{eval_and,eval_xor}` to the artifact's equations |
| Offline expansion yields per-wire `Enc(A0)`, `Enc(π)` | Offline yields per-wire **mask-bit ciphertexts** (`ct_rbits`), 4/AND for fast, `wires − inv` for small | Re-shape `ExpandedWire`/stream to mask bits |
| Evaluator decrypts with the SWHE key at the end | Evaluator never holds `sk`; per-gate `DetermineEva` uses the garbler's 1-bit LSB | Output opening is per-gate determination, not a final decrypt |

The reference adapter stays as the differential-test oracle for the
transport/stream shape only. BITGC-PIN-05..08 move from PARTIAL to
"equations identified from the artifact; pending paper-PDF cross-check."
