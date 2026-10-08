# BitGC paper-pinned construction (from ePrint 2025/1810 + 2024/1988)

**Status:** paper-pinned reference, 2026-10-08. Extracted directly from the
PDFs now archived in `docs/research/papers/` (`2025-1810.pdf`,
`2024-1988.pdf`, with extracted text `1810.txt`, `2024-1988.txt`). This
supersedes the artifact-recovery notes in
`docs/research/bitgc-artifact-notes.md` (which it confirms) and resolves
BITGC-LEDGER-04's "paper text" prerequisite. Cross-checked against the
authors' artifact `wtd2/more-efficient-bitgc`.

This document records the exact construction equations that a faithful
`GateEquation` implementation must realize. Section/figure references are
into [ZLL+26] (ePrint 2025/1810) unless noted; [LWYY25b] is ePrint
2024/1988.

---

## 1. The SWHEwDD primitive ([LWYY25b] §2.5, [ZLL+26] §3.3 Def. 6)

The garbling is built on a private-key **SWHE with distributed
decryption** ("SWHEwDD"). Its defining feature is a *non-standard*
decryption: `Dec(Δ, τ)` on a ciphertext `τ = Enc(Δ, m)` outputs **`m·Δ`**,
not `m` (GSW-style, [LWYY25b] §2.5). The key `Δ ∈ R` has `LSB(Δ) = 1`.

**Distributed decryption** lets a party holding an *additive share* of a
key decrypt. With `X` uniform, `LSB(X)=0`, `X₁ = X + Δ`:

- **Linear DD (k=1):** `DD_i(X + i·Δ, τ) = DD_0(X, τ) + i·Dec(Δ, τ)`.
  ([ZLL+26] Def. 6, and [LWYY25b] linear DD.)
- **Correlated-key DD (k=2):** `DD_{i,j}(X+i·Δ, Y+j·Δ, τ) =
  DD_{0,0}(X, Y, τ) + i·j·Dec(Δ, τ)`.

The garbled-table evaluator is
([ZLL+26] §3.3, eq. above Fig. 4):
```text
Eval_{i,j}(A, B, τ₁, τ₂, τ₃) := DD_i(A, τ₁) + DD_j(B, τ₂) + DD_{i,j}(A, B, τ₃)
```
which, by the DD properties, satisfies
```text
Eval_{i,j}(A+iΔ, B+jΔ, τ) = Eval_{0,0}(A,B,τ) + i·Dec(Δ,τ₁) + j·Dec(Δ,τ₂) + i·j·Dec(Δ,τ₃)
```
— exactly the garbled-row reconstruction property (the `τ`'s are the
encrypted rows `Dec(Δ, τₖ)` of the AND truth table).

**Instantiation ([ZLL+26] §6 / App. A):** BGV framework under RLWE; k=1 DD
follows Chillotti et al. [COS+22]; k=2 DD replaces the original GSW-style
ciphertexts with a **CP-RLWE-based** approach [ILL25b]. (The artifact's
`HSSMul` CP-RLWE triple and `DD` map onto these.)

---

## 2. BitGC-small ([ZLL+26] §4, Figures 4–5)

**Setup / offline (Gb steps 1–3):**
- `params ← Setup(1^λ, 2^L)`, `L = t + 2` (t = PRG depth).
- `(keypar, Δ) ← Gen(params)`, `LSB(Δ)=1`; `[[Δ]] := Enc(Δ, Δ)`. PRF key K.
- PRG seed `s`; `[[s_i]] := Enc(Δ, s_i)`. Expand:
  `(π_0..π_{N-1}) := PRG(s)` and homomorphically `([[π_0]]..[[π_{N-1}]])`,
  `N = |I| + |W|` (one permutation bit per input + gate wire).

**Input wires (Gb step 4):** label pair `W_i^0 ←$ R_p`,
`W_i^1 := W_i^0 + (−1)^{π_i}·Δ`.

**AND gate (α,β,γ) — Gb (Fig. 4 Case 1):**
```text
(a) ẑ[i,j] := (π_α ⊕ i) ∧ (π_β ⊕ j) ⊕ π_γ   for i,j ∈ {0,1}
    homomorphically compute [[ẑ[i,j]·Δ]] from [[π_α]],[[π_β]],[[π_γ]],[[Δ]]
    (two levels of homomorphic multiplication)
(b) τ₁ := [[ẑ[1,0]·Δ]] − [[ẑ[0,0]·Δ]]
    τ₂ := [[ẑ[0,1]·Δ]] − [[ẑ[0,0]·Δ]]
    τ₃ := [[ẑ[1,1]·Δ]] − [[ẑ[0,0]·Δ]] − τ₁ − τ₂
(c) W_γ^{π_γ}   := Eval_{0,0}(W_α^{π_α}, W_β^{π_β}, τ₁,τ₂,τ₃) − ẑ[0,0]·Δ + PRF_K(γ)
    W_γ^{π_γ⊕1} := W_γ^{π_γ} + Δ
    d_γ := LSB(W_γ^0) ⊕ π_γ                       ← the 1 communicated bit
```
**AND gate — Ev (Fig. 5 Case 1):**
```text
(c) W_γ := Eval_{x̂_α, x̂_β}(W_α, W_β, τ₁,τ₂,τ₃) + PRF_K(γ)
(d) x̂_γ := LSB(W_γ) ⊕ d_γ                       ← uses the communicated d_γ
```

**XOR gate — Gb (Fig. 4 Case 2):**
```text
(a) d_γ := π_α ⊕ π_β ⊕ π_γ                       ← the 1 communicated bit
    τ₁ := (1 − 2·d_γ)·[[Δ]];  τ₂ := τ₁;  τ₃ := −τ₁ − τ₂
(b) W_γ^{π_γ} := Eval_{0,0}(...) − d_γ·Δ + PRF_K(γ);  W_γ^{π_γ⊕1} := W_γ^{π_γ} + Δ
```
**XOR — Ev (Fig. 5 Case 2):**
```text
(c) x̂_γ := x̂_α ⊕ x̂_β ⊕ d_γ
```
(The evaluator's masked bit for XOR is pure XOR — no table needed beyond
the label update.)

**En:** `x̂_i := x_i ⊕ π_i`; `W_i := W_i^0 + (−1)^{π_i}·x_i·Δ`.
**De:** `x_i := x̂_i ⊕ π_i` (or hash-match against `H(W_i^0), H(W_i^1)` for
authenticity).

**Communication:** 1 bit per AND (`d_γ`), 1 bit per XOR (`d_γ`). Matches
Table 1.

---

## 3. BitGC-fast ([ZLL+26] §5.1, Figures 6–7)

Differences from small (marked blue in Fig. 6):

- **PRG output:** `4·|W_AND|` pseudorandom bits (four per AND gate), not
  per-wire permutation bits. AND-gate output `π_γ` is **sampled uniformly**
  (not a PRG output); XOR output `π_γ := π_α ⊕ π_β`.
- **`L = t + 1`** (one fewer level than small).

**AND gate — Gb (Fig. 6 Case 1):**
```text
(a) sample π_γ ←$ {0,1}
    ẑ[i,j] := (π_α⊕i) ∧ (π_β⊕j) ⊕ π_γ
    d_γ^{i,j} := ẑ[i,j] ⊕ r_γ^{i,j}        (r = PRG bit)   ← 4 communicated bits
    [[ẑ[i,j]·Δ]] := d_γ^{i,j}·[[Δ]] + (1 − 2·d_γ^{i,j})·[[r_γ^{i,j}·Δ]]
                                                       ↑ homomorphic ADDITION only
(b) τ₁,τ₂,τ₃ as in small (subtractions of the [[ẑ·Δ]])
(c) labels + d_γ := LSB(W_γ^0) ⊕ π_γ        ← 1 determination bit
```
**Communicated per AND: 4 mask bits `d_γ^{i,j}` + 1 determination bit
`d_γ` = 5 bits.** No online homomorphic multiplication — the table is
assembled by addition because the garbler *sends* the masked `ẑ` bits and
the evaluator unmaskes them linearly from the PRG ciphertexts.

**XOR gate — Gb (Fig. 6 Case 2):**
```text
π_γ := π_α ⊕ π_β;  τ₁ := [[Δ]]; τ₂ := τ₁; τ₃ := −τ₁ − τ₂
labels as usual;  d_γ = π_α⊕π_β⊕π_γ = 0 → NOT sent  (free-XOR)
```

**Ev (Fig. 7):** identical gate handling; XOR recovers `x̂_γ = x̂_α ⊕ x̂_β`
with no communicated bit.

The 5-bit count is derived in [ZLL+26] §2.4: the three table plaintexts
`t₁,t₂,t₃ ∈ {−1,0,1}` are transmitted as the four masked values
`z_{i,j} ⊕ π_c` (the identity `(−1)^{π_c}(z_{i,j} − z_{0,0}) =
(z_{i,j}⊕π_c) − (z_{0,0}⊕π_c)`), plus the one determination bit.

---

## 4. BitGC-tiny ([ZLL+26] §5.2, Figure 8) — deferred

Shares `En`/`De` with small. Differences:
- Uses a **leveled HE** (LHEwDD), depth `L = ⌈log(ℓ_XOR + 1)⌉ + t + 2`,
  where `ℓ_XOR` is the longest consecutive XOR run.
- PRG outputs `|I| + |W_AND|` bits.
- XOR output `π_γ := π_α ⊕ π_β` (free-XOR); `[[π_γ]]` is still computed
  homomorphically (recursively flattening XOR chains into a binary XOR-sum
  tree) because subsequent AND gates need it. `d_γ = 0` not sent.

Deferred per maintainer decision (BITGC-LEDGER-06).

---

## 5. Parameter / PRG instantiation ([ZLL+26] §6)

- **PRG:** Goldreich-style random local PRG; predicate **XOR₄–Majority₇**
  (Applebaum–Lovett), 11-bit input, arithmetized to multiplicative depth
  **t = 4**. Seed `ℓ = 2^16`, stretch `ℓ² = 2^32`.
- **SWHE:** BGV in RNS form, two contexts (large PRG ring `N_PRG`,
  small gate ring `N_gate` after ring-dimension reduction).
  - Plaintext modulus `log p ≈ 60`; DD moduli `log q ≈ 120` (k=1),
    `log r ≈ 240` (k=2).
  - **fast:** `log Q_PRG ≈ 600` (10 limbs), `log Q_gate ≈ 180` (3 limbs);
    `N_PRG = 2^15`, `N_gate = 2^13`.
  - **small:** `log Q_PRG ≈ 780` (13 limbs), `log Q_gate ≈ 300` (5 limbs);
    `N_PRG = 2^15`, `N_gate = 2^14`.
- Per-gate failure probability target `< 2^-40`; 128-bit RLWE security
  per [ACC+19].

---

## 6. Cross-check: paper ↔ artifact ↔ current code

| Property | Paper (2025/1810) | Artifact (`more-efficient-bitgc`) | Current `volar_spec::bitgc` |
|---|---|---|---|
| Per-wire offline value | **one permutation bit π** | one mask bit per wire (`AssignWireMasks`) | (A0, π) pair — **wrong shape** |
| AND masked bits | `ẑ[i,j] = (π_α⊕i)∧(π_β⊕j)⊕π_γ` | `gate_mask_bit0 = π_c ⊕ (π_a∧π_b)` + monomial table | `ẑ` via `π_a⊕i` — **equations differ** |
| AND comm | 1 bit (`d_γ = LSB(W⁰_γ)⊕π_γ`) | 1 bit (`DetermineGen` share LSB) | 1 bit placeholder |
| fast AND comm | 4 mask bits + 1 det bit = 5 | 4 `prg_mask_bits` + 1 `garble_bit` = 5 | 5 bits placeholder |
| fast table | subtraction-only (4 PRG bits) | subtraction-only (`table[i]` subtractions) | — |
| free-XOR | `π_γ = π_α⊕π_β` | `wire_masks[c] = π_a⊕π_b` | implemented |
| Δ | `LSB(Δ)=1`, Δ = SWHE secret key | Δ = `sk` (SubDelta/AddDelta) | small odd constant — **placeholder** |
| Offline→online | PRG expansion → SIMD unpack → per-bit RLWE | `goldreich_prg` + `unpack/` pipeline | `stream_expansion` (no unpack yet) |

The **core equations now match the paper** (the artifact implements
Figures 4–7 faithfully, with SEAL-level optimizations like the monomial
table form for noise). The remaining gap in `volar_spec::bitgc` is the
**DD/HSS label layer**: the paper's labels are additive key-shares
evaluated through `DD_i`/`DD_{i,j}`, with the per-gate bit being the
garbler's share LSB — not the self-contained SWHE table the
`ReferenceEquation` uses. Porting that layer is the next implementation
step (see plan §5 / BITGC-LEDGER-04).

---

## 7. Resolved and remaining pinning

**Resolved by the PDFs (no longer TODO on evidence):**
- [ZLL+26] Table 1 numbers confirmed verbatim (fast 5/0 bits 4.0 KB 116 s;
  small 1/1 4.3 KB 345 s; tiny 1/0 0.8 KB ≥56 min) — BITGC-PIN-11.
- Exact per-gate equations for small (Fig. 4–5) and fast (Fig. 6–7) —
  BITGC-PIN-05..08 now **paper-pinned** (this document).
- PRG = XOR₄–Majority₇ Goldreich, t=4 — BITGC-PIN-03 pinned.
- Parameter shapes (N_PRG/N_gate, limb counts, log-moduli) — BITGC-PIN-10
  pinned (paper §6).

**Landed (2026-10-08) — the pinned circuit drivers:**
`volar_spec::bitgc::pinned` now has full `garble_circuit` /
`evaluate_circuit` drivers over the reference DD gadget, realizing the
equations above end-to-end. Both small and fast truth tables pass over
all inputs (`small_circuit_truth_table`, `fast_circuit_truth_table`),
and `record_bit_counts` confirms the Table 1 communication accounting
(small: 1+1 bits for And+Xor; fast: 5 bits for the AND, 0 for XOR).

**Key implementation findings** (verified by the round-trip tests and an
empirical probe):

1. **The DD `(U,V,W)` triple is an evaluation key** — the
   `Eval_{i,j}(A+iΔ,B+jΔ) = Eval_{0,0}(A,B) + (i·t₁+j·t₂+ij·t₃)·Δ`
   identity holds **only if garbler and evaluator use the same
   `(U,V,W)`**. The drivers therefore take a shared `&ReferenceDd` and
   never re-derive it per gate.
2. **Fast XOR uses the global `[[Δ]]`** (Fig. 6 Case 2: `τ₁=τ₂=[[Δ]],
   τ₃=−2[[Δ]]`), *not* plain label addition — plain addition leaves a
   `2Δ` payload that breaks downstream `DD_{i,j}` (which needs the
   `Δ·U+V=0` correlation). This is exactly why fast XOR is free: the
   rows derive from the one global `[[Δ]]` with no per-gate table and no
   communicated bit. Small XOR is the same with a per-gate sign
   `d = π_α⊕π_β⊕π_γ` (the 1 communicated bit).
3. **LSB wrap-around is the per-gate failure term.** The determination
   relation `LSB(W^0+k·Δ) = LSB(W^0)⊕k` holds only when the
   coefficient-0 sum `W^0[0]+k·Δ[0]` does not wrap mod `p`; wrap (prob.
   ~`N/p` per gate for degree `N`) flips a bit. Measured 1.03% per gate
   at `p=97`, degree 8 (empirical probe); the paper's `log p ≈ 60`
   makes this cryptographically negligible (`< 2^-40`). Tests use
   `p = 2^62+135` (prime) to stay exact; production parameters per §6.

**Still open (implementation, not evidence):**
- BITGC-LEDGER-04: replace `ReferenceEquation` with the paper equations
  above + differential test. Evidence now complete (PDF + artifact agree).
- BITGC-LEDGER-01/02: conformance vectors against the artifact
  (`sub64`, QEMU Linux build).
- BITGC-LEDGER-03: independent parameter-security review.
- BITGC-LEDGER-06: bitgc-tiny implementation (deferred).
