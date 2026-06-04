# Binding Pedersen generators via hash-to-curve

**Status:** Implemented. `volar_spec::curve::hash_to_curve` (try-and-increment,
SHA3-256 RO) + `fe_sqrt`/`is_square`/`fe_pow`; `PedersenParams::setup`
([`pedersen.rs`](../crates/fold/volar-fold/src/pedersen.rs)) now derives binding
generators. Companion to [`vcb-ivc-folding.md`](vcb-ivc-folding.md) §5.

## Why

A Pedersen commitment `Commit(x⃗; ρ) = Σ x_i·G_i + ρ·H` is **binding** only if no
efficient adversary can find a non-trivial relation `Σ a_i G_i + b H = 0`. That
holds when the generators are *random* group elements with **unknown pairwise
discrete logs**. Deriving `G_i = [s_i]·B` from the base point with known `s_i`
(the earlier scaffold) gives a *known* relation, so a prover who knows the `s_i`
can equivocate — the commitment is homomorphic but not binding, and folding
soundness (which rests on commitment binding) collapses.

The fix is to derive each generator by **hash-to-curve**: map a public label to a
curve point through a random oracle, so its discrete log w.r.t. `B` is unknown.

## Construction (try-and-increment)

For `ctr = 0, 1, 2, …`:
1. `h = SHA3-256(domain ‖ index ‖ ctr)`. Take `x = h[..32] mod p` with the top bit
   cleared and used as a `sign` bit.
2. Solve the Edwards equation for `y²`. From `−x² + y² = 1 + d·x²·y²`:
   `y² = (1 + x²) / (1 − d·x²)` (retry if the denominator is 0).
3. If `y²` is a quadratic residue, `y = √(y²)` with sign fixed by the `sign` bit;
   else retry (`≈ ½` of `x` values are residues, so ~2 iterations expected).
4. `P = (x, y)`; **clear the cofactor** `P ← [8]·P` (`h = 8`) so `P` lands in the
   prime-order-`ℓ` subgroup; retry if `P` is the identity.

### Field square root (`p = 2^255 − 19`, `p ≡ 5 mod 8`)
`c = w^{(p+3)/8}` with `(p+3)/8 = 2^252 − 2`. Then `c² ∈ {w, −w}`:
- `c² = w` ⇒ `√w = c`;
- `c² = −w` ⇒ `√w = c·√−1`, where `√−1 = 2^{(p−1)/4}` (`(p−1)/4 = 2^253 − 5`);
- otherwise `w` is a non-residue (`None`).

Implemented as `fe_sqrt`/`is_square`, built on a generalised `fe_pow` (the
square-and-multiply chain that `fe_invert` already used).

## Soundness

In the ROM, each `hash_to_curve` output is an independent uniform prime-order
point; its discrete log w.r.t. `B` (or any other generator) is unknown, so no
generator relations are known ⇒ Pedersen **binding** holds. Cofactor clearing
ensures the point has order exactly `ℓ` (verified in tests by `[ℓ]·P = O`).

**Indifferentiability is not required here.** Try-and-increment is *not* an
indifferentiable hash-to-curve (it slightly biases toward residue `x`), which
matters when hashing *messages* in some proofs — but for **generators** we only
need one-wayness of the discrete log, which it provides. (RFC 9380's Elligator2
map is the constant-time, indifferentiable, interop-grade alternative; it needs
the Montgomery↔Edwards conversion and the full `hash_to_field`/`map_to_curve` —
overkill when no external interoperability is required.)

Non-constant-time is acceptable because generator derivation is a public,
input-independent setup step (no secrets flow through the loop or the branches).

## API

- `volar_spec::curve::{fe_pow, fe_sqrt, is_square, fe_from_bytes_le, ed_mul_cofactor, hash_to_curve}`.
- `PedersenParams::setup(n, seed)` → `G_i = hash_to_curve(b"volar-fold/pedersen/G", …)`,
  `H = hash_to_curve(b"volar-fold/pedersen/H", seed)`. `seed` domain-separates
  independent generator sets.

## Tests
On-curve, prime-order (`[ℓ]·P = O`), deterministic, domain/index-distinct
(`curve.rs`); homomorphism preserved (`pedersen.rs`).
