# TFHE / GINX Core Mathematical Specification (Draft)

**Status:** draft, Phase 1 deliverable of `docs/tfhe-pbs-rework-plan.md`. This
document is a **paper-derived mathematical model**, checked by an independent
cleartext oracle (`crates/spec/volar-spec/src/tfhe_ginx_oracle.rs`, test-only).
It is **not** a statement that the existing ciphertext-level implementation in
`crates/spec/volar-spec/src/tfhe.rs` conforms to it — that comparison is
Phase 2/3 of the rework plan and is explicitly **not done by this document**.

**Reliability:** this file is documentation only (Tier 1). It contains no
executable cryptography. The oracle it specifies is compiled test-only and
makes no security claims (see the oracle file's own header).

**Primary sources** (page numbers are physical PDF pages, visually confirmed
by page render where noted):

- **[MP20]** Micciancio, Polyakov, *Bootstrapping in FHEW-like Cryptosystems*,
  ePrint 2020/086 (local copy `2020-086.pdf`, 27pp, PALISADE-based unified
  FHEW/TFHE analysis). Used for the generic accumulator model, admissibility
  condition, and the canonical Boolean-gate preparation table.
- **[CGGI18]** Chillotti, Gama, Georgieva, Izabachène, *TFHE: Fast Fully
  Homomorphic Encryption over the Torus*, ePrint 2018/421 (local copy
  `2018-421.pdf`, 62pp). Used for TLWE/TRLWE/TRGSW definitions, the external
  product, CMux, BlindRotate (Algorithm 4), gate bootstrapping (Algorithms 9–10),
  and the native Boolean-gate formulas.

---

## 1. Domains and encodings

### 1.1 Two moduli, not one

[MP20] p.10 (§3) and p.21 (§5.1) are explicit that a FHEW/TFHE-style scheme has
**at least two independent modulus/dimension pairs**:

- `(n, q)` — the *input* LWE ciphertext dimension and modulus, over which the
  caller's Boolean-encoded ciphertexts live.
- `(N, Q)` — the RLWE/RGSW ring dimension and modulus used **internally** by
  the accumulator during a single bootstrap.
- `(n, Q_ks)` — a possibly distinct key-switching modulus (p.21, §5.1: "we
  introduced an additional modulus switching operation ... between the core
  bootstrapping operation and key switching").

A specification (or implementation) that stores every value in the same `u32`
domain must show that this is a deliberate, checked simplification (e.g. "we
fix `q = Q = Q_ks = 2^32` and accept the resulting noise/precision cost") and
not an accidental conflation. This document treats `q` and `Q` as distinct
symbols throughout, and any implementation note must say explicitly which
concrete modulus it uses for which role.

### 1.2 Boolean message encoding

Per [MP20] p.13 (§3.2) and [CGGI18] p.44 (§6.1 "Fully Homomorphic Boolean
Gates"): a Boolean ciphertext encrypts `m ∈ {0, 1}` as

```
LWE(m · q/4) = (a, b),  b = ⟨a, s⟩ + e + m·(q/4)
```

i.e. **false → phase 0, true → phase q/4**, with fresh-ciphertext error bound
`|e| < q/16` ([MP20] p.13). This is a convention choice (call it the
*canonical Boolean encoding*); anything that emits a different scale (e.g.
`q/2`) is not directly composable with gates built against this table without
re-deriving the intervals below.

### 1.3 Ring and secret conventions

[MP20] p.10–11 (§3.1): `Q = 2^k` a power of two, ring `R_Q = Z_Q[X]/(X^{Q/2}+1)`
in the abstract FHEW presentation, generalized in the implementation to a ring
dimension `N ≥ Q/2` with `Q | 2N`. [CGGI18] uses `T_N[X] = R[X]/(X^N+1)` over
the real torus directly (no separate `Q`; the ring modulus is the real-number
torus itself, discretized to `u32`/`u64` only in the implementation).

The GINX bootstrapping method ([MP20] p.10, §3, bullet 2) requires the LWE
secret `s` to be **binary** (`s_i ∈ {0,1}`), because it "supports basic updates
`ACC ← c·E(s)` where `c` is arbitrary but `s ∈ {0,1}` is a single bit." This is
a hard structural precondition of the GINX update rule, not a parameter choice;
using GINX with a non-binary secret requires the ternary/Gaussian extension in
[MP20] §3.1 (not this document's scope — flag it separately if the repository
ever changes its key distribution).

---

## 2. The accumulator abstraction

[MP20] p.9 (§3), Figure 1, defines bootstrapping generically via a
cryptographic accumulator `ACC` holding a value from `Z_q`, with three
operations:

1. **Initialize:** `ACC ← b` for a known `b ∈ Z_q`.
2. **Update:** `ACC ← c·E(s)`, changing the accumulator content from `v` to
   `v + c·s` for `c, s ∈ Z_q`, where `s` is encrypted.
3. **Extract:** `f̃(ACC)`, returning an encryption of `f(v)` for the *current*
   accumulator content `v`, where `f` was fixed when `ACC` was initialized.

Generic bootstrap pseudocode ([MP20] Figure 1, p.10):

```
Bootstrap(ek = (E(s_i))_i, (a, b)):
    ACC ← b
    for i = 1..n:
        c_i = -a_i mod q
        ACC ← c_i · ek_i     # update
    return f(ACC)            # extract
```

### 2.1 FHEW/GINX ring accumulator (the concrete instantiation)

[MP20] p.11 (§3.1), Figure 2: for a **fixed** function `f: Z_q → Z_Q` satisfying
the antipodal relation

```
f(v + q/2) = -f(v)                                              (★)
```

the accumulator is instantiated as a *noiseless* trivial RLWE encryption:

```
ACC_f[v] = RLWE( Σ_{i=0}^{q/2-1} f(v - i) · X^i )   ∈ R_Q
```

- **Init(v):** for `i = 0..q/2-1`, set `m_i = f(v - i)`; return the trivial
  RLWE ciphertext `(0, m(X))` with `m(X) = Σ_i m_i X^i`.
- **Extract(a, b):** given RLWE ciphertext `(a(X), b(X))`, return the LWE
  ciphertext `(a, b_0)` where `a = (a_0, …, a_{q/2-1})` are the coefficients of
  `a(X)` and `b_0` is the constant term of `b(X)`.

[MP20] explicitly notes (p.11, footnote-equivalent text) this coefficient
vector must be read with a **sign-permuted key** `z = (z_0, -z_{q/2-1}, …,
-z_1)` because of the negacyclic ring relation `X^{q/2} = -1` — this is the
same phenomenon as [CGGI18]'s `SampleExtract`'s "N-antiperiodic indexes" (see
§3.2 below). Any independent oracle or implementation must reproduce this sign
flip in the extracted mask vector, not just the constant term.

**(★) is the single most important admissibility fact for this entire
document.** A candidate gate function `f` — however many inputs it takes — is
usable in **one** bootstrap **iff** its post-affine-combination cleartext
function satisfies (★) over `Z_q`. This is arity-independent (Majority/3-input
in §4 satisfies it; some 2-input functions do not, depending on how they are
prepared — see §4.4).

### 2.2 GINX update rule

[MP20] p.13 (§3.1, second bullet) and Figure 4 (p.14): each secret bit
`s ∈ Z_q` is expressed as a subset sum `s = Σ_{u∈U} u·x_u` with `x_u ∈ {0,1}`
(for binary secrets, `U = {1}` suffices: `s` itself is `x_1 ∈ {0,1}`). The
encryption function is

```
E'(t) = { Z_u = RGSW(I_T(u)) | u ∈ U },   t = Σ_{u∈U} u   for  T ⊆ U
```

and the accumulator update is

```
ACC ← ACC + (X^{u·c} - 1) · (ACC ⋄ Z_u)     for each u ∈ U        (GINX-update)
```

which [MP20] notes "requires only a single RLWE × RGSW product" (p.13). For a
binary secret bit this reduces to one CMux-equivalent update per key bit, over
`n` key bits total — matching [CGGI18]'s BlindRotate loop (§3 below).

---

## 3. [CGGI18] mechanics: CMux, BlindRotate, SampleExtract

These are the concrete building blocks that realize §2 in [CGGI18]'s notation.
Page citations are to `2018-421.pdf`.

### 3.1 External product and CMux (pp. 15, 17–18)

Definition 3.12 (p.15): the external product `⊡ : TGSW × TLWE → TLWE` is
`A ⊡ b = Dec_{H,β,ε}(b) · A`, where `Dec` is the gadget decomposition of
Definition 3.6/Lemma 3.7 (p.11–13). Theorem 3.13 (p.15) proves `A ⊡ b`
encrypts `msg(A) · msg(b)`.

Lemma 3.16 (p.17), CMux gate: for a TGSW sample `C` of a bit and TLWE samples
`d0, d1`:

```
CMux(C, d1, d0) = C ⊡ (d1 - d0) + d0
```

`msg(CMux(C, d1, d0)) = msg(C) ? msg(d1) : msg(d0)` — **selects `d1` when the
control bit is 1, `d0` when it is 0.**

### 3.2 BlindRotate (Algorithm 4, pp. 23–24)

```
Input:  TRLWE sample c of v ∈ T_N[X]
        p+1 integers a_1..a_p, b ∈ Z/2N
        p TRGSW samples C_1..C_p of bits s_1..s_p
ACC ← X^{-b} · c
for i = 1..p:
    ACC ← CMux(C_i, X^{a_i} · ACC, ACC)
return ACC
```

Output: `ACC ∈ TRLWE(X^{-ρ} · v)` where `ρ = b - Σ s_i a_i` (Theorem 4.3,
p.23). **Rotation direction:** the initial rotation by the *known* exponent
`b` is applied first (as `X^{-b}`), then each *secret* exponent `a_i` is
conditionally applied via CMux — this is the concrete realization of the
GINX update loop in §2.2, with the sign convention `X^{-b}·X^{a_i·s_i}·(…)` per
key bit.

### 3.3 SampleExtract (p.23, §4.2)

Given TRLWE `c = (a(X), b(X))` encrypting `μ`, `SampleExtract(c)` at position
`p=0` returns the TLWE sample `(a, b)` where `b = b_0` and `a_i` is the
`(p−j)`-th coefficient of `a(X)` **using the N-antiperiodic indices** — i.e.
for `j > p` the coefficient is *negated* (this is the same negacyclic sign
flip noted in §2.1 for the FHEW accumulator's permuted key). SampleExtract adds
**no noise** (p.24).

### 3.4 Gate bootstrapping (Algorithms 9–10, pp. 41–43)

Algorithm 9 (TLWE-to-TLWE bootstrap, calling BlindRotate):

```
Input:  constant μ_1 ∈ T, TLWE sample c = (a,b) ∈ TLWE(x·1/2), bootstrapping key BK
1. rescale ã_i = round(a_i · 2N), b̃ = round(b · 2N)      (torus → Z/2N)
2. v := (1+X+...+X^{N-1}) · X^{N/2} · μ_1                  (test polynomial)
3. ACC ← BlindRotate((0, v), (ã_1..ã_n, b̃), (BK_1..BK_n))
4. return (0, μ_1) + SampleExtract(ACC)
```

Correctness (Theorem 6.2, p.41–42): the output is `μ_1` if `|ϕ(a,b)| > 1/4+δ`,
else `0`, for a small rounding term `δ`. This is exactly the antipodal
sign-selection behavior of (★), specialized to a **binary** output function
(`f(v) = ±μ_1`) rather than a general LUT.

Algorithm 10 adds the final key-switch (Theorem 4.1/4.2, pp. 19–21) to return
to the caller's original key/dimension.

### 3.5 Native Boolean gate formulas (p.44, §6.1)

```
HomNOT(c)          = (0, 1/4) - c                         (no bootstrap)
HomAND(c1,c2)      = Bootstrap_{1/4}( (0, -1/8) + c1 + c2 )
HomNAND(c1,c2)     = Bootstrap_{1/4}( (0,  5/8) - c1 - c2 )
HomOR(c1,c2)       = Bootstrap_{1/4}( (0,  1/8) + c1 + c2 )
HomXOR(c1,c2)      = Bootstrap_{1/4}( 2·(c1 - c2) )
```

These match [MP20] Table 1 (§4 below) up to the placement of the additive
`±q/8` constant: [CGGI18] folds the constant into the pre-bootstrap value,
[MP20] folds it into the *interval test* performed at extraction time. Both
are the same mathematics restated; **an implementation must pick one
convention consistently and not mix folded-in and folded-out constants**,
which is exactly the class of bug the earlier (reverted) prototype exhibited.

---

## 4. Canonical Boolean-gate certificates ([MP20] Table 1, p.15)

This is the authoritative, paper-cited table this document adopts for the
Phase 1 cleartext oracle. All intervals are stated as subsets of `Z_q`
(equivalently, in units of `q/8`, since every listed boundary is a multiple of
`q/8`). `c_i` denotes the *i*-th input ciphertext's phase, encoded per §1.2
(`false=0, true=q/4`).

| Gate | Affine preparation | Maps to `q/8` (bootstrap output `+q/8`) | Maps to `-q/8` |
|---|---|---|---|
| AND | `c1 + c2` | `[3q/8, 7q/8)` | complement |
| NAND | `c1 + c2` | `[-q/8, 3q/8)` | complement |
| OR | `c1 + c2` | `[q/8, 5q/8)` | complement |
| NOR | `c1 + c2` | `[-3q/8, q/8)` | complement |
| XOR | `2(c1 - c2)` | `[q/8, 5q/8)` | complement |
| XNOR | `2(c1 - c2)` | `[-3q/8, q/8)` | complement |
| Majority | `c1 + c2 + c3` | `[3q/8, 7q/8)` | complement |
| NOT | `(-a, -b + q/4)` | — no bootstrap — | — |

**Output restoration** ([MP20] p.15): the bootstrap itself returns a signed
value in `{-q/8, +q/8}` (via the antipodal accumulator of §2.1, with
`f = ±q/8` fixed at initialization). A **noiseless** `(0, q/8)` is added
afterward to restore the canonical `{0, q/4}` encoding of §1.2:

```
canonical_output = bootstrap_output_signed + q/8       (bootstrap_output_signed ∈ {-q/8, +q/8})
                  ∈ {0, q/4}
```

### 4.1 Why arity alone does not determine "needs circuit bootstrapping"

Majority is a **3-input gate realized in one bootstrap** (Table 1, last data
row) because its affine preparation `c1+c2+c3` and interval `[3q/8,7q/8)`
happen to satisfy (★) over the resulting range of sums. This directly falsifies
any claim of the form "3+ inputs require circuit bootstrapping" — arity is not
the discriminant; **whether a chosen affine map's image satisfies the
antipodal relation is**.

### 4.2 Why "any complete truth table fits" is also false

Not every `2^k`-entry Boolean table can be assigned a single affine
preparation whose image satisfies (★). The admissible tables in Table 1 are
precisely the ones the paper derived; a generic table (e.g. arbitrary 3-input
functions that are not expressible as a threshold-of-a-linear-combination)
requires either a different, individually-derived affine map and proof, or
falls back to circuit bootstrapping / a packed LUT evaluation (out of this
document's scope — see `docs/tfhe-pbs-rework-plan.md` §9).

### 4.3 Composability

Because every certificate above restores the canonical `{0, q/4}` encoding
(§1.2) before returning, and each Boolean input is likewise expected in that
same encoding, outputs of one certified gate are valid inputs to the next.
**This is only true for outputs that go through the bootstrap + restoration
step.** A "free" linear operation (e.g. computing `c1 XOR c2` as an unrouted
`c1+c2` without ever bootstrapping) does *not* produce a canonically-encoded
result and must not be treated as composable — see [CGGI18] p.44's own remark
that the XOR "middle ⊕" inside `HomMUX`/native MUX is deliberately *not*
bootstrapped because "this xor has at most one operand which is true," a
narrower and explicitly load-bearing precondition, not a general license for
unbootstrapped XOR chaining.

### 4.4 Native MUX (p.44, §6.1)

```
MUX(c, d1, d0) = (c ∧ d1) ⊕ ((1-c) ∧ d0)
```

evaluated as two gate bootstraps (`c∧d1`, `¬c∧d0`) plus one **public key
switch** on the sum (not a third bootstrap), exploiting the fact that at most
one of the two AND results can be true. This is a distinct, narrower
optimization from the general CMux of §3.1 and must be labeled as such if
implemented — it is not "CMux is free," it is "this specific sum needs no
third bootstrap because of a semantic invariant on its two operands."

---

## 5. What this document does **not** establish

Per `docs/tfhe-pbs-rework-plan.md` §3–§4, the following remain explicitly
unresolved by this draft and must not be treated as validated:

- Whether `crates/spec/volar-spec/src/tfhe.rs`'s concrete `u32`-torus
  implementation (its `torus_to_exp`, `poly_rotate`, `sample_extract`,
  `key_switch`, RGSW layout, etc.) faithfully implements §2–§3 above. That is
  Phase 2 (stage-by-stage conformance tests) of the rework plan.
- Any noise, failure-probability, or parameter claim. §1.1's modulus/dimension
  distinctions are not yet mapped onto the repository's structs.
- Circuit bootstrapping (TLWE→TRGSW) or packed/leveled LUT evaluation — a
  separate, later specification (rework plan §9).
- Ternary/Gaussian-secret GINX extensions ([MP20] §3.1) — the repository
  currently assumes binary secrets only (§1.3), consistent with the paper's
  base GINX construction, but this has not been cross-checked against the
  repository's key-generation code.

## 6. Independent cleartext oracle

`crates/spec/volar-spec/src/tfhe_ginx_oracle.rs` (test-only, `#[cfg(test)]`,
no ciphertexts, no keys, no `SpecRng`) implements exactly §4's table as
integer arithmetic modulo a generic power-of-two `q`, exhaustively checks every
gate against its plaintext truth table for multiple `q`, includes mutation
tests for sign/offset/doubling-omission/interval errors (Gate A of the rework
plan), and a composition test chaining XOR into AND. It is deliberately
independent of `tfhe.rs`'s bootstrap implementation and must remain so — it is
the reference this document's Phase 2 conformance tests will be checked
against, not a second copy of the same code path.