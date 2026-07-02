# FoldLift constraint expansion: the spaced-packing GF(2^k) → F_ℓ embedding

Status: **specified; implementation tracked here**. Supersedes the one-scalar
`FoldLift` lift whose unsoundness `docs/agent-context/gf2k-to-fell-embedding.md`
documents (and which `e2e_fold_verifier.rs` pins as a known-failing assert).
This construction is deterministic-testable and comes with a written soundness
argument (§5), but the seam **remains flagged for cryptographic review** — see
§7 for exactly what is and is not claimed.

## 1. Problem restated

Per AND gate the woven VOLE verifier checks, in the VOLE field `T = GF(2^k)`:

```text
K_a · K_b + V̂ = K_c · Δ        (Quicksilver; char 2 ⇒ equivalently)
K_a · K_b + V̂ + K_c · Δ = 0
```

The Nova fold needs an R1CS relation over the folding scalar field `F_ℓ`
(`volar_fold::scalar::Scalar`, ℓ ≈ 2^252, the Ed25519 scalar field) that holds
**iff** the GF(2^k) check holds. No field homomorphism exists; instead each `T`
element expands into its GF(2) coefficient bits plus constraints that encode
GF(2^k) arithmetic over those bits. Even `T = GF(2)` needs this: the naive lift
satisfies multiplication (`AND` = `F_ℓ`-product on {0,1}) but not addition
(1 + 1 = 2 ≠ 0 in F_ℓ); addition must be kept inside {0,1} by construction.

## 2. Parameters

For `GF(2^k)` with irreducible `P(x) = x^k + p(x)`, `deg p < k` (`p` given as an
integer bitmask, LSB = x^0; `k = 8, p = 0x1b` is the AES field, matching
`volar_primitives::GF8_POLY`):

- **Spacing** `s = ⌈log2(2k + 2)⌉` (k=8 → s=5; k=1 → s=2).
- **Columns** `j ∈ [0, 2k−1)` — the carry-less product coefficient positions.
- **Reduction rows** `red[j] = x^j mod P(x)` as k-bit masks, for `j ∈ [k, 2k−1)`.
  For k=8, p=0x1b these are, j = 8..14:
  `0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a` (pin these in a test).
- **Column weight** `weight_i = |{j : red[j] bit i set}|`; **τ bound**
  `τmax_i = (1 + weight_i) · (2k + 1)`; **half-bits** `m_i = ⌈log2(⌊τmax_i/2⌋ + 1)⌉`.
  For k=8: weights `[3,4,3,6,4,3,3,3]`, τmax `[68,85,68,119,85,68,68,68]`,
  `m_i = 6` for all i.

**Static soundness asserts** (in the params constructor; panic = out of scope,
this is the deliberate "single-limb only, not fully generic" boundary):

1. `k ≥ 1`, `p < 2^k`.
2. `2k + 1 < 2^s` (holds by definition of s; assert anyway).
3. `2·k·s ≤ 250` — packed products never wrap mod ℓ.
4. `(2k−1)·s ≤ 250` — the packed sum S and its bit decomposition never wrap.

(k=8: 80 and 75 ✓. k=16: 192/186 ✓. k=32: 448 ✗ → panics; Galois64/128 need a
future multi-limb variant.)

## 3. The relation (witness + constraints)

Inputs per gate, LSB-first GF(2) coefficient bits of the five values:
`a_i = bits(K_a)`, `b_i = bits(K_b)`, `c_i = bits(K_c)`, `d_i = bits(Δ)`,
`v_i = bits(V̂)`, `i ∈ [0, k)`.

Witness allocation order (pinned so layouts are reproducible; the builder
allocates in call order):

```text
a_0..a_{k−1}, b_0.., c_0.., d_0.., v_0..,           5k input bits
P_ab, P_cd,                                          2 product values
e_{0,0}..e_{2k−2,s−1}   (j-major, t-minor),          (2k−1)·s sum bits
h_{0,0}..h_{k−1,m_i−1}  (i-major, t-minor),          Σ m_i half bits
```

Linear combinations (free, no witness): `A = Σ_i a_i·2^{i·s}`, likewise
`B, C, D`, and `V = Σ_i v_i·2^{i·s}`;
`σ_j = Σ_t e_{j,t}·2^t`; `h_i = Σ_t h_{i,t}·2^t`;
`τ_i = σ_i + Σ_{j≥k, red[j] bit i} σ_j`.

Constraints, in emission order:

| # | rows | constraint |
|---|------|-----------|
| C1 | 5k | booleanity `x·x = x` for every input bit |
| C2 | 2 | `A·B = P_ab`, `C·D = P_cd` |
| C3 | (2k−1)·s | booleanity for every `e_{j,t}` |
| C4 | 1 | `(P_ab + P_cd + V − Σ_{j,t} e_{j,t}·2^{j·s+t}) · 1 = 0` |
| C5 | Σ m_i | booleanity for every `h_{i,t}` |
| C6 | k | `(τ_i − 2·h_i) · 1 = 0` for each output column i |

"`· 1`" rows put the linear combination in the A matrix and the constant 1
(→ u column) in B, C = 0 — the same shape `and_check_r1cs`'s third row uses,
so relaxation/folding behaves identically.

**Exact totals** (assert in tests): constraints `5k + 2 + (2k−1)s + 1 + Σm_i + k`,
witness vars `5k + 2 + (2k−1)s + Σm_i`. k=8: **174 constraints, 165 witness
vars** (`num_vars = 166` with the u column). k=1: **12 constraints, 10 witness
vars**.

## 4. Why this encodes the Quicksilver check (completeness)

Spaced packing turns carry-less multiplication into one field product: with all
bits boolean and slots `s` wide, `A·B = Σ_j (Σ_{i} a_i·b_{j−i}) · 2^{j·s}` as an
*integer* — column sums land in disjoint s-bit slots because each is ≤ k < 2^s,
and nothing wraps mod ℓ (assert 3). `S = P_ab + P_cd + V` then has base-2^s
digits `σ_j ≤ 2k+1 < 2^s` (assert 2): the *combined* column sums of
`clmul(K_a,K_b) + clmul(K_c,Δ) + V̂` with no inter-slot carry, by construction —
this is where addition is "kept contained," including the k=1/GF(2) case.

The GF(2^k) check holds iff the polynomial `Σ_j (σ_j mod 2)·x^j` reduces to 0
mod P(x), iff for every output column `i < k` the integer
`τ_i = σ_i + Σ_{j≥k, red[j] bit i} σ_j` is **even** — C6 with the bit-bounded
half `h_i` enforces exactly that. Honest values satisfy every row exactly
(`u = 1, E = 0`).

## 5. Soundness argument (adversarial witness)

1. C1/C3/C5 booleanity ⇒ every bit variable ∈ {0,1} (b² = b has no other roots
   in a field). So `A,B,C,D,V` are genuine spaced packings, `< 2^{k·s}`.
2. Assert 3 ⇒ `A·B` computed in F_ℓ equals the integer product ⇒ `P_ab`, `P_cd`
   are the true spaced carry-less products (C2).
3. `S = P_ab + P_cd + V < 2^{(2k−1)s} ≤ 2^{250}` (digit bound + assert 4), so C4
   plus e-booleanity forces `e` to be **the** unique binary representation of S
   ⇒ `σ_j(e)` are the true combined column sums.
4. C6: the true integer `τ_i ≤ τmax_i ≪ ℓ`. Over F_ℓ, `τ_i = 2·h_i` with
   `h_i ≤ 2^{m_i} − 1 < 2^{250}` bounded by its boolean bits. If τ_i were odd,
   the unique field solution `h_i = τ_i·2^{−1} = (τ_i + ℓ)/2 > 2^{250}` lies
   outside that range — contradiction. Hence every τ_i is even, hence the
   GF(2^k) relation holds for the values the bits encode. ∎

Relaxation: all rows are standard R1CS rows; the folded accumulator satisfies
the *relaxed* relation `Az∘Bz = u·Cz + E` exactly as for `and_check_r1cs` — the
argument above applies to fresh (`u=1, E=0`) instances, which is what each
per-gate instance is before folding; folding soundness is Nova's, unchanged.

## 6. Interfaces (implementation contract)

### volar-fold: `src/gf2k.rs` (new)

```rust
pub struct Gf2kParams {
    pub k: usize,
    pub poly: u128,        // p(x) low bits, x^k implicit
    pub s: usize,
    pub red: Vec<u128>,    // red[j-k] = x^j mod P(x), j in [k, 2k-1)
    pub m: Vec<usize>,     // m[i], per output column
}
impl Gf2kParams {
    /// Panics on the static bound asserts of §2.
    pub fn new(k: usize, poly: u128) -> Self;
}

/// Build the expanded AND-check R1CS *and* its satisfying witness together
/// (same pattern as keccak_r1cs::Builder::finish). Bits are LSB-first GF(2)
/// coefficients; all five slices must have length k.
/// The R1CS shape depends only on `params`, never on the bit values.
pub fn and_check_gf2k(
    params: &Gf2kParams,
    a: &[bool], b: &[bool], c: &[bool], d: &[bool], v: &[bool],
) -> (R1CS, Vec<Scalar>);
```

Shared gadget kit: extract `Lc` + `Builder` from `keccak_r1cs.rs` into a
`pub(crate) mod r1cs_builder` (same file contents, no behavior change;
`keccak_r1cs` imports from it). `gf2k.rs` needs two small additions that must
go in the shared builder, not a fork: an `enforce_linear(&Lc)` helper emitting
the `(lc)·(1) = 0` row, and `Lc::scale`d accumulation is already present.

### volar-fold: `src/nifs.rs`

Make the cross term reusable witness-only:

```rust
/// Cross term T (length num_cons) from raw witness slices.
pub fn cross_term_z(r1cs: &R1CS, w1: &[Scalar], u1: &Scalar,
                    w2: &[Scalar], u2: &Scalar) -> Vec<Scalar>;
```

`prove_fold` refactors to call it (behavior identical; existing nifs tests must
stay green unmodified).

### volar-verifier-runtime: `src/lib.rs`

```rust
pub trait FoldLift {
    /// GF(2)-degree k of T.
    const BITS: usize;
    /// Irreducible polynomial low bits (x^BITS implicit), LSB = x^0.
    const POLY: u128;
    /// LSB-first GF(2) coefficients of self; len == BITS.
    fn lift_bits(&self) -> Vec<bool>;
}
impl FoldLift for Bit    { BITS = 1; POLY = 0;    /* vec![self.0] */ }
impl FoldLift for Galois { BITS = 8; POLY = 0x1b; /* bits of self.0 */ }
```

(`Vec<bool>` rather than the plan's earlier `Vec<FoldScalar>`: the bits feed
`and_check_gf2k`'s native witness evaluation, so `bool` is the honest type and
avoids scalar equality tests.)

`FoldAccumulator.inner: Option<(Vec<FoldScalar>, Vec<FoldScalar>, FoldScalar)>`
(W, E, u); `witness()` returns slice refs. `fold_and_gate` keeps its **exact
signature** (the weaver emits it by bare name — zero weaver changes) and body
becomes: lane-0 projection (`q[0]` — unchanged, still a documented separate
simplification) → `lift_bits` × 5 → `and_check_gf2k` → fresh `(w, 0^cons, 1)` or
witness-only Nova fold: `t = cross_term_z(…)`, `w' = w1 + r·w2`,
`e' = e1 + r·t` (e2 = 0 for a fresh instance), `u' = u1 + r`.
Update the module doc: the fixed-size `volar_spec::fold` linkage is retired for
the expanded relation; `volar_spec::fold`, `and_check_r1cs`, `GateObservation`/
`VerifierStep` remain untouched as the documented legacy/batch path.

## 7. Honest scope (kept — do not delete)

- The R1CS binds the gate relation over **whatever bits the runtime supplies**.
  Nothing here commits the bits to the VOLE transcript or to commitments; that
  is the separate `BoundaryLink` seam (`docs/vcb-ivc-folding.md` §4).
- Δ appears as independent per-gate witness bits; cross-gate Δ-consistency is
  not enforced in-circuit (same shape as the 7-slot relation it replaces).
- Lane-0 projection of the N VOLE lanes remains.
- Single-limb only (k ≤ 16 by the §2 asserts).
- The soundness argument in §5 is written, deterministic checks in §8 exercise
  it, and **cryptographic review is still invited** before calling the seam
  closed — per `docs/agent-context/gf2k-to-fell-embedding.md`'s "Do not".

## 8. Test matrix (the deterministic soundness checker)

In `volar-fold` (module tests, plain `#[test]`; if the exhaustive sweep exceeds
~2 min in debug, keep it exhaustive and note that `--release` is the intended
way to run it — do not shrink coverage):

1. **Pin constants**: k=8 reduction rows `[0x1b,0x36,0x6c,0xd8,0xab,0x4d,0x9a]`,
   s=5, m=[6;8], 174 constraints / 166 num_vars; k=1: s=2, 12 / 11.
2. **Exhaustive k=8 completeness**: all 2^16 `(K_a, K_b)`, `Δ=1, V̂=0,
   K_c = gf_mul_u8(K_a, K_b, 0x1b)` → witness satisfies `is_satisfied(full_z)`.
3. **Randomized honest tuples**: deterministic LCG, ≥256 tuples: random
   `K_a,K_b,V̂`, random nonzero `Δ`, `K_c = (K_a·K_b + V̂)·Δ⁻¹` (Galois ops from
   volar-primitives) → satisfied. Include edge values 0, 1, 0xff.
4. **Negatives**: honest tuple with one bit of `K_c` flipped → unsatisfied;
   one bit of `V̂` flipped → unsatisfied; the naive-embedding counterexample
   family (tuples where F_ℓ integer arithmetic on the packed bytes holds but
   GF(2^8) does not, e.g. K_a=2,K_b=3 vs 2·3=6≠gf_mul(2,3)) → build the honest
   GF witness for a *false* GF relation must be unsatisfied.
   (Adversarial-witness negatives beyond honest-witness tampering are covered
   by §5's argument plus test 6.)
5. **k=1 exhaustive**: all 32 `(a,b,c,d,v)` — honest witness satisfies **iff**
   `a·b ⊕ v ⊕ c·d = 0`. Both directions asserted.
6. **Tampered-witness soundness probes**: for a few honest k=8 witnesses, flip
   each single witness variable (0↔1 for bit vars; +1 for P_ab/P_cd) →
   unsatisfied in every case.
7. **Fold**: two honest gate witnesses, fold via `cross_term_z` + the §6 vector
   math with a fixed r → `is_satisfied_relaxed(w', e', u')`.
8. **Shape determinism**: R1CS from two different honest inputs are equal
   (num_cons, num_vars, and all three sparse matrices).
9. **Panic tests**: `Gf2kParams::new(32, …)`, `new(0, …)`, `new(8, 0x100)`.

In `volar-verifier-fold` (Phase C): the e2e pinned assert **flips** to
`assert!(r1cs.is_satisfied_relaxed(w, e, u))` against `and_check_gf2k`'s R1CS
(`Gf2kParams::new(8, 0x1b)`), structural asserts updated (`w.len() == 165`,
`u == ONE`, `E` all zero), `assert!(all_ok)` kept.
