// @reliability: experimental
//! @ai: assisted
//! WARP accumulation (Bünz–Chiesa–Fenzi–Wang, *Linear-Time Accumulation
//! Schemes*, ePrint 2025/753, Construction 10.4) in the linkable spec
//! subset, over the **VOLE field** `GF(2^128)` (`x^128 + x^7 + x^2 + x + 1`,
//! [`Galois128`]).
//!
//! This module is the single-source-of-truth port of
//! `volar-iop`'s `warp.rs` oracle: same transcript byte schedule, same
//! domain separators, same Merkle layout — the differential tests pin
//! byte-for-byte agreement over `Galois128` (the §5.4 field decision in
//! `volar-ir`'s `docs/warp-vole-verifier-zksnark-plan.md`). Differences from
//! the oracle are representational only:
//!
//! - concrete `Galois128` instead of a generic `Field` (the tower stays an
//!   oracle-side test fixture);
//! - `Array<T, N>` typenum sizing instead of `Vec` (the spec subset bans
//!   `Vec`; see `volar-compiler-passes`' `vec_lint`);
//! - **total functions**: the spec dialect has no `Result` construction and
//!   no `panic!`, so errors become `Option` (construction boundaries:
//!   genesis/prove) or `bool` (verify/decide — the verifier-as-computation
//!   accept bit). Rejection is fail-closed: verify returns `false`, the
//!   constructors return `None`;
//! - capacity-plus-count bounded vectors (`WarpTerm.vars`,
//!   `WarpConstraint.terms`, `WarpUni.evals`) with boundary checks instead of
//!   growable vectors;
//! - Merkle trees restricted to power-of-two leaf counts (every WARP use —
//!   `n` codeword positions — is a power of two; the oracle's pad-by-
//!   duplicate path is unreachable here);
//! - the `<N as Logarithm2>::Output` type-level logarithm (`typenum::Logarithm2`) sizes
//!   evaluation points.
//!
//! Type-level shape invariants (`ELL = FRESH + A`, `R = 1 + S + SHIFTS`,
//! `BETA = public_len + log2(M)`, `NODES = 2N - 1`, power-of-two `N`/`M`/
//! `ELL`/`R`) are checked at the boundary functions and trusted inside.
//!
//! Like the oracle, this is a construction-faithful algebraic reference: the
//! bundled `WarpIdentityCode`/`WarpRepetitionCode` fixtures carry no
//! proximity soundness claim, and parameter selection against the paper's
//! bounds is a separate reviewed step.

use cipher::consts::*;
use cipher::typenum::{Logarithm2, Unsigned};

use super::*;
use crate::field::Galois128;
use crate::keccak::{
    SpecSponge, SHA3_256_RATE, SHAKE128_RATE, spec_sponge_absorb_array, spec_sponge_finalize_xof,
    spec_sponge_new, spec_xof_read_array,
};

/// Type-level `log2` for the power-of-two size parameters WARP uses.
/// Type-level log2 alias (documentation name; the compiler sees the projection form).
#[allow(dead_code)]
pub type Log2<N> = <N as Logarithm2>::Output;

/// A 32-byte SHA3-256 digest (the dialect's native array form).
pub type WarpDigest = Array<u8, U32>;

pub(crate) fn is_pow2(n: usize) -> bool {
    n != 0 && n.is_power_of_two()
}

pub(crate) fn log2_pow2(n: usize) -> usize {
    n.trailing_zeros() as usize
}

// ============================================================================
// Transcript (W2) — SHAKE128, byte-identical to the oracle's IopTranscript
// schedule (domain separator, absorb ordering, length prefixes, squeeze
// grouping).
// ============================================================================

/// The Construction 10.4 transcript domain — identical to the oracle's.
pub const WARP_TRANSCRIPT_DOMAIN: [u8; 51] =
    *b"volar-iop/warp/eprint-2025-753/construction-10.4/v1";

/// A field element's canonical 16-byte LE encoding as a dialect array.
fn le16(v: Galois128) -> Array<u8, U16> {
    let b = v.0.to_le_bytes();
    Array::<u8, U16>::from_fn(|i| b[i])
}

/// A running SHAKE128 sponge with the oracle's absorb/squeeze semantics.
///
/// The sponge is the spec Keccak ([`crate::keccak`]), so the whole WARP
/// path lowers through LIR to VAFFLE without an extern hashing boundary;
/// the byte schedule is pinned against the `sha3` crate by the keccak
/// module's differential tests.
#[derive(Clone)]
pub struct WarpTranscript {
    sponge: SpecSponge,
}

impl Default for WarpTranscript {
    fn default() -> Self {
        warp_transcript_new()
    }
}

/// A fresh WARP transcript (free-function constructor: the dialect lowers
/// static `X::new` calls poorly; a plain function keeps the receiver types
/// exact).
pub fn warp_transcript_new() -> WarpTranscript {
    WarpTranscript {
        sponge: spec_sponge_new(SHAKE128_RATE),
    }
}

impl WarpTranscript {

    /// Absorb a domain-separation label (once, first).
    pub fn domain_sep<N: ArraySize>(mut self, label: &Array<u8, N>) -> Self {
        spec_sponge_absorb_array(&mut self.sponge, label);
        self
    }

    /// Absorb a fixed-size byte array (the dialect form — concrete `N` loop
    /// bound, no runtime-length slice loops).
    pub fn absorb_array<N: ArraySize>(&mut self, data: &Array<u8, N>) {
        spec_sponge_absorb_array(&mut self.sponge, data);
    }

    /// Length-prefixed absorb (`u64` LE length, then bytes) — the oracle's
    /// `absorb_len_prefixed`.
    pub fn absorb_len_prefixed<N: ArraySize>(&mut self, data: &Array<u8, N>) {
        let len_bytes = (N::USIZE as u64).to_le_bytes();
        self.absorb_array(&Array::<u8, U8>::from_fn(|i| len_bytes[i]));
        self.absorb_array(data);
    }

    pub fn absorb_usize(&mut self, value: usize) {
        let bytes = (value as u64).to_le_bytes();
        self.absorb_array(&Array::<u8, U8>::from_fn(|i| bytes[i]));
    }

    /// One field element as its canonical 16-byte LE encoding,
    /// length-prefixed — the oracle's `absorb_field`.
    pub fn absorb_field(&mut self, value: Galois128) {
        self.absorb_len_prefixed(&le16(value));
    }

    /// Count-then-elements absorb — the oracle's `absorb_field_vec`.
    pub fn absorb_fields<N: ArraySize>(&mut self, values: &Array<Galois128, N>) {
        self.absorb_usize(N::USIZE);
        for i in 0..N::USIZE {
            self.absorb_field(values[i]);
        }
    }

    /// Absorb one sumcheck polynomial: the count prefix then the fields —
    /// the oracle's `absorb_polynomial`. `CNT` is the polynomial's
    /// evaluation count (a shape constant: first-degree+1 or 3), so the
    /// loop bound is concrete; the boundary check ties it to `poly.count`.
    pub fn absorb_poly<SC: ArraySize, CNT: ArraySize>(&mut self, poly: &WarpUni<SC>) {
        self.absorb_usize(CNT::USIZE);
        for i in 0..CNT::USIZE {
            self.absorb_field(poly.evaluations[i]);
        }
    }

    /// Squeeze `N` field elements from ONE XOF stream (16 LE bytes each) —
    /// byte-identical to the oracle's single `squeeze_bytes(16N)` + chunking
    /// (never N independent squeezes: each squeeze must advance one stream).
    pub fn squeeze_fields<N: ArraySize>(&self) -> Array<Galois128, N> {
        let mut reader = spec_sponge_finalize_xof(&self.sponge, 0x1F);
        Array::from_fn(|_| {
            let mut bytes: Array<u8, U16> = Array::<u8, U16>::default();
            spec_xof_read_array(&mut reader, &mut bytes);
            let mut b16 = [0u8; 16];
            for i in 0..16 {
                b16[i] = bytes[i];
            }
            Galois128(u128::from_le_bytes(b16))
        })
    }

    /// Squeeze an `R × C` grid of field elements from ONE XOF stream —
    /// byte-identical to the oracle's single `squeeze_bytes(R*C*16)` +
    /// chunking (used for the Construction 5.10 `tau_i` and the
    /// Construction 7.2 out-of-domain points).
    pub fn squeeze_field_grid<R: ArraySize, C: ArraySize>(
        &self,
    ) -> Array<Array<Galois128, C>, R> {
        let mut reader = spec_sponge_finalize_xof(&self.sponge, 0x1F);
        Array::from_fn(|_| {
            Array::from_fn(|_| {
                let mut bytes: Array<u8, U16> = Array::<u8, U16>::default();
                spec_xof_read_array(&mut reader, &mut bytes);
                let mut b16 = [0u8; 16];
                for i in 0..16 {
                    b16[i] = bytes[i];
                }
                Galois128(u128::from_le_bytes(b16))
            })
        })
    }

    /// Construction 7.2's combined shift-index + `xi` squeeze: one stream of
    /// `SHIFTS * index_bytes + LOGR * 16` bytes, indices first — the
    /// oracle's `squeeze_shift_and_xi`.
    pub fn squeeze_shift_and_xi<SHIFTS: ArraySize, LOGR: ArraySize, LOGDOM: ArraySize>(
        &self,
    ) -> (Array<u32, SHIFTS>, Array<Galois128, LOGR>) {
        let mut reader = spec_sponge_finalize_xof(&self.sponge, 0x1F);
        let shifts = Array::<u32, SHIFTS>::from_fn(|_| {
            let mut value = 0usize;
            for position in 0..((LOGDOM::USIZE + 7) / 8) {
                let mut byte: Array<u8, U1> = Array::<u8, U1>::default();
                spec_xof_read_array(&mut reader, &mut byte);
                value |= (byte[0] as usize) << (8 * position);
            }
            (value & ((1usize << LOGDOM::USIZE) - 1)) as u32
        });
        let xi = Array::<Galois128, LOGR>::from_fn(|_| {
            let mut bytes: Array<u8, U16> = Array::<u8, U16>::default();
            spec_xof_read_array(&mut reader, &mut bytes);
            let mut b16 = [0u8; 16];
            for i in 0..16 {
                b16[i] = bytes[i];
            }
            Galois128(u128::from_le_bytes(b16))
        });
        (shifts, xi)
    }
}

// ============================================================================
// Merkle (W2) — SHA3-256, domain-separated leaf/node hashing, power-of-two
// leaf counts only (see module doc).
// ============================================================================

/// Hash one field element's 16 LE bytes as a Merkle leaf.
pub(crate) fn warp_hash_leaf_field(v: Galois128) -> WarpDigest {
    let mut h = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb_array(&mut h, &Array::<u8, U1>::from_fn(|_| 0x00));
    let v_bytes: Array<u8, U16> = le16(v);
    spec_sponge_absorb_array(&mut h, &v_bytes);
    let mut reader = spec_sponge_finalize_xof(&h, 0x06);
    let mut out: Array<u8, U32> = Array::<u8, U32>::default();
    spec_xof_read_array(&mut reader, &mut out);
    out
}

pub(crate) fn warp_hash_node(left: &WarpDigest, right: &WarpDigest) -> WarpDigest {
    let mut h = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb_array(&mut h, &Array::<u8, U1>::from_fn(|_| 0x01));
    spec_sponge_absorb_array(&mut h, left);
    spec_sponge_absorb_array(&mut h, right);
    let mut reader = spec_sponge_finalize_xof(&h, 0x06);
    let mut out: Array<u8, U32> = Array::<u8, U32>::default();
    spec_xof_read_array(&mut reader, &mut out);
    out
}

/// A committed binary Merkle tree, nodes in level order (leaves first, root
/// last). `leaves` must be a power of two and `NODES == 2 * leaves - 1`
/// (checked by the boundary functions).
#[derive(Clone, Debug)]
pub struct WarpMerkleTree<NODES: ArraySize> {
    /// Number of committed leaves (a power of two).
    pub leaves: usize,
    /// All nodes, level order: leaves first, root last.
    pub nodes: Array<WarpDigest, NODES>,
}

/// An authentication path: sibling hashes leaf-up, `LOG2_LEAVES` long.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpAuthPath<LOG2_LEAVES: ArraySize> {
    pub siblings: Array<WarpDigest, LOG2_LEAVES>,
}


    /// Commit to pre-hashed leaves in level order. Requires `LEAVES` a power
    /// of two and `NODES == 2 * LEAVES - 1` (boundary-checked).
    pub fn warp_merkle_commit_leaves<NODES: ArraySize, LEAVES: ArraySize>(leaves: &Array<WarpDigest, LEAVES>) -> WarpMerkleTree<NODES> {
        let mut nodes = Array::<WarpDigest, NODES>::default();
        for i in 0..LEAVES::USIZE {
            nodes[i] = leaves[i].clone();
        }
        // Level i starts at offset LEAVES + LEAVES/2 + ... + LEAVES/2^(i-1).
        // Exactly log2(LEAVES) level transitions; guarded over LEAVES so the
        // loop bound is concrete (the spec dialect's requirement).
        let mut level_start = 0usize;
        let mut level_len = LEAVES::USIZE;
        for _level in 0..LEAVES::USIZE {
            if level_len > 1 {
                let next_start = level_start + level_len;
                for j in 0..(level_len / 2) {
                    nodes[next_start + j] = warp_hash_node(
                        &nodes[level_start + 2 * j],
                        &nodes[level_start + 2 * j + 1],
                    );
                }
                level_start = next_start;
                level_len /= 2;
            }
        }
        WarpMerkleTree {
            leaves: LEAVES::USIZE,
            nodes,
        }
    }

    pub fn warp_merkle_root<NODES: ArraySize>(tree: &WarpMerkleTree<NODES>) -> WarpDigest {
        tree.nodes[NODES::USIZE - 1].clone()
    }

    /// Open leaf `index` — siblings bottom-up. `LOG2` must equal
    /// `log2(tree.leaves)`.
    pub fn warp_merkle_open<NODES: ArraySize, LOG2: ArraySize>(tree: &WarpMerkleTree<NODES>, index: usize) -> WarpAuthPath<LOG2> {
        let mut siblings = Array::<WarpDigest, LOG2>::default();
        let mut idx = index;
        let mut level_start = 0usize;
        let mut level_len = tree.leaves;
        // The path length is exactly log2(leaves) = LOG2 (bounded loop).
        for depth in 0..LOG2::USIZE {
            siblings[depth] = tree.nodes[level_start + (idx ^ 1)].clone();
            idx /= 2;
            level_start += level_len;
            level_len /= 2;
        }
        WarpAuthPath { siblings }
    }


/// Verify that `leaf_hash` (already leaf-hashed) is committed at `index`
/// under `root` given `path`.
pub fn warp_merkle_verify_hashed<LOG2_LEAVES: ArraySize>(
    root: &WarpDigest,
    leaf_hash: &WarpDigest,
    index: usize,
    path: &WarpAuthPath<LOG2_LEAVES>,
) -> bool {
    let mut cur = leaf_hash.clone();
    let mut idx = index;
    for i in 0..LOG2_LEAVES::USIZE {
        let sibling = &path.siblings[i];
        cur = if idx % 2 == 0 {
            warp_hash_node(&cur, sibling)
        } else {
            warp_hash_node(sibling, &cur)
        };
        idx /= 2;
    }
    cur == *root
}

/// Commit to one codeword: leaf `i` is the hash of element `i`'s 16 LE
/// bytes (the oracle's `single_codeword_tree`).
pub fn warp_commit_codeword<N, NODES>(
    codeword: &Array<Galois128, N>,
) -> WarpMerkleTree<NODES>
where
    N: ArraySize,
    NODES: ArraySize,
{
    let leaves = Array::<WarpDigest, N>::from_fn(|i| warp_hash_leaf_field(codeword[i]));
    warp_merkle_commit_leaves::<NODES, N>(&leaves)
}

/// Commit to stacked fresh codewords: leaf `i` is the hash of the
/// concatenation of element `i` of every fresh codeword, in fresh order
/// (the oracle's `codeword_leaves`).
pub fn warp_commit_stacked<N, FRESH, NODES>(
    codewords: &Array<Array<Galois128, N>, FRESH>,
) -> WarpMerkleTree<NODES>
where
    N: ArraySize,
    FRESH: ArraySize,
    NODES: ArraySize,
{
    // Streaming absorb is byte-identical to hashing the concatenated parts.
    let leaves = Array::<WarpDigest, N>::from_fn(|i| {
        let mut h = spec_sponge_new(SHA3_256_RATE);
        spec_sponge_absorb_array(&mut h, &Array::<u8, U1>::from_fn(|_| 0x00));
        for j in 0..FRESH::USIZE {
            let elem_bytes: Array<u8, U16> = le16(codewords[j][i]);
            spec_sponge_absorb_array(&mut h, &elem_bytes);
        }
        let mut reader = spec_sponge_finalize_xof(&h, 0x06);
        let mut out: Array<u8, U32> = Array::<u8, U32>::default();
        spec_xof_read_array(&mut reader, &mut out);
        out
    });
    warp_merkle_commit_leaves::<NODES, N>(&leaves)
}

// ============================================================================
// Multilinear helpers (shared by prover, verifier, decider)
// ============================================================================

/// `eq(point, b)` for every Boolean `b`, little-endian — the oracle's
/// `build_eq_evals`. Boundary invariant: `OUT == 2^POINTS` (checked by the
/// boundary functions' shape validation).
pub fn warp_eq_evals<POINTS: ArraySize, OUT: ArraySize>(
    point: &Array<Galois128, POINTS>,
) -> Array<Galois128, OUT> {
    let mut out = Array::<Galois128, OUT>::default();
    out[0] = Galois128(1);
    // At outer step c the live prefix has length 2^c; iterate it directly
    // (a const-expr bound over the unrolled counter — the spec dialect
    // requires concrete loop bounds).
    for c in 0..POINTS::USIZE {
        let coordinate = point[c];
        for index in 0..(1usize << c) {
            let product = out[index] * coordinate;
            // characteristic 2: subtraction is addition (XOR).
            out[index] = out[index] - product;
            out[index + (1usize << c)] = product;
        }
    }
    out
}

/// Scalar equality polynomial `eq(a, b)` — the oracle's `eq_scalar`.
pub fn warp_eq_scalar<L: ArraySize>(a: &Array<Galois128, L>, b: &Array<Galois128, L>) -> Galois128 {
    let mut out = Galois128(1);
    for i in 0..L::USIZE {
        let left = a[i];
        let right = b[i];
        let same = left * right;
        let different = (Galois128(1) - left) * (Galois128(1) - right);
        out = out * (same + different);
    }
    out
}

/// Little-endian Boolean embedding of `index` — the oracle's `binary_point`.
pub fn warp_binary_point<LOG: ArraySize>(index: usize) -> Array<Galois128, LOG> {
    Array::from_fn(|bit| {
        if (index >> bit) & 1 == 1 {
            Galois128(1)
        } else {
            Galois128(0)
        }
    })
}

/// MLE evaluation of a power-of-two table — the oracle's `mle_eval`.
/// Boundary invariant: `N` a power of two, `LOGN == log2(N)`.
pub fn warp_mle_eval<N, LOGN>(
    values: &Array<Galois128, N>,
    point: &Array<Galois128, LOGN>,
) -> Galois128
where
    N: ArraySize + Logarithm2,
    LOGN: ArraySize,
    <N as Logarithm2>::Output: ArraySize,
{
    let eq = warp_eq_evals::<LOGN, N>(point);
    let mut out = Galois128(0);
    for i in 0..N::USIZE {
        out = out + values[i] * eq[i];
    }
    out
}

// ============================================================================
// PESAT types (W3) — sparse constant-degree constraints over x || w.
// ============================================================================

/// One term `coeff * prod vars` in a PESAT constraint. Capacity-plus-count:
/// `vars` holds at most `DEG` variable indices into the concatenation
/// `x || w` (`x` = public inputs, then `w` = witness), `n_vars` in use.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpTerm<DEG: ArraySize> {
    pub coeff: Galois128,
    pub vars: Array<usize, DEG>,
    pub n_vars: u32,
}

/// One sparse polynomial constraint: the sum of its first `n_terms` terms.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpConstraint<TERMS: ArraySize, DEG: ArraySize> {
    pub terms: Array<WarpTerm<DEG>, TERMS>,
    pub n_terms: u32,
}

/// The PESAT index `i = (p_hat, M, N, k)` with a global degree bound.
/// `M` (the constraint count) must be a power of two (paper requirement).
#[derive(Clone, Debug)]
pub struct WarpIndex<M: ArraySize, TERMS: ArraySize, DEG: ArraySize> {
    pub constraints: Array<WarpConstraint<TERMS, DEG>, M>,
    pub public_len: usize,
    pub witness_len: usize,
    pub degree_bound: usize,
}

/// A fresh PESAT statement: public part `x` plus prover witness `w`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpWitness<K: ArraySize, X: ArraySize> {
    pub public: Array<Galois128, X>,
    pub witness: Array<Galois128, K>,
}

/// One term's degree.
fn warp_term_degree<DEG: ArraySize>(term: &WarpTerm<DEG>) -> usize {
    term.n_vars as usize
}

/// Evaluate a term at the assignment (public, witness) split — variable `v`
/// reads `public[v]` when `v < X`, else `witness[v - X]`.
fn warp_eval_term<DEG: ArraySize, X: ArraySize, K: ArraySize>(
    term: &WarpTerm<DEG>,
    public: &Array<Galois128, X>,
    witness: &Array<Galois128, K>,
) -> Galois128 {
    let mut out = term.coeff;
    for i in 0..DEG::USIZE {
        if i < term.n_vars as usize {
            let var = term.vars[i];
            let value = if var < X::USIZE {
                public[var]
            } else {
                witness[var - X::USIZE]
            };
            out = out * value;
        }
    }
    out
}

/// A constraint's degree (max over its live terms).
fn warp_constraint_max_degree<TERMS: ArraySize, DEG: ArraySize>(
    constraint: &WarpConstraint<TERMS, DEG>,
) -> usize {
    let mut out = 0usize;
    for i in 0..TERMS::USIZE {
        if i < constraint.n_terms as usize {
            let d = warp_term_degree(&constraint.terms[i]);
            if d > out {
                out = d;
            }
        }
    }
    out
}

/// Evaluate a constraint (sum of its live terms) at (public, witness).
fn warp_eval_constraint<TERMS: ArraySize, DEG: ArraySize, X: ArraySize, K: ArraySize>(
    constraint: &WarpConstraint<TERMS, DEG>,
    public: &Array<Galois128, X>,
    witness: &Array<Galois128, K>,
) -> Galois128 {
    let mut out = Galois128(0);
    for i in 0..TERMS::USIZE {
        if i < constraint.n_terms as usize {
            out = out + warp_eval_term(&constraint.terms[i], public, witness);
        }
    }
    out
}


    pub fn warp_variable_count<M: ArraySize, TERMS: ArraySize, DEG: ArraySize>(pesat: &WarpIndex<M, TERMS, DEG>) -> usize {
        pesat.public_len + pesat.witness_len
    }

    pub fn warp_log_constraints<M: ArraySize, TERMS: ArraySize, DEG: ArraySize>(pesat: &WarpIndex<M, TERMS, DEG>) -> usize {
        log2_pow2(M::USIZE)
    }

    /// Fail-closed index validation (returns `false` on any violation):
    /// `M` a power of two, per-constraint degree within the bound, term
    /// variable indices in range, counts within capacity.
    pub fn warp_index_is_valid<M: ArraySize, TERMS: ArraySize, DEG: ArraySize>(pesat: &WarpIndex<M, TERMS, DEG>) -> bool {
        if !is_pow2(M::USIZE) {
            return false;
        }
        for c in 0..M::USIZE {
            let constraint = &pesat.constraints[c];
            if constraint.n_terms as usize > TERMS::USIZE {
                return false;
            }
            if warp_constraint_max_degree(constraint) > pesat.degree_bound {
                return false;
            }
            for i in 0..TERMS::USIZE {
                if i < constraint.n_terms as usize {
                    let term = &constraint.terms[i];
                    if term.n_vars as usize > DEG::USIZE {
                        return false;
                    }
                    for j in 0..DEG::USIZE {
                        if j < term.n_vars as usize && term.vars[j] >= warp_variable_count(pesat) {
                            return false;
                        }
                    }
                }
            }
        }
        true
    }

    /// Evaluate the bundled polynomial `P_hat(beta, w)` from Definition 5.5.
    ///
    /// `beta` is `x || tau` (Construction 10.4 step 2(f)): the first
    /// `public_len` entries are the public input, the rest `tau` (length
    /// `log M`). Boundary invariant: `BETA == public_len + log2(M)` and
    /// `K == witness_len`.
    pub fn warp_evaluate_bundled<M: ArraySize, TERMS: ArraySize, DEG: ArraySize, K: ArraySize, BETA: ArraySize>(
        pesat: &WarpIndex<M, TERMS, DEG>,
        beta: &Array<Galois128, BETA>,
        witness: &Array<Galois128, K>,
    ) -> Galois128 {
        let log_m = warp_log_constraints(pesat);
        // eq evals over tau (beta's tail, length log M), indexed by
        // constraint — the same doubling loop as `warp_eq_evals`, written
        // out because `tau` is a runtime tail of `beta`, not an Array.
        // Guarded form over M: exactly the first log2(M) rounds act (the
        // spec dialect requires concrete loop bounds).
        let mut eq = Array::<Galois128, M>::default();
        eq[0] = Galois128(1);
        for j in 0..M::USIZE {
            if (1usize << j) < M::USIZE {
                let coordinate = beta[pesat.public_len + j];
                for index in 0..(1usize << j) {
                    let product = eq[index] * coordinate;
                    eq[index] = eq[index] - product;
                    eq[index + (1usize << j)] = product;
                }
            }
        }
        let mut out = Galois128(0);
        for index in 0..M::USIZE {
            let constraint = &pesat.constraints[index];
            // Public part of beta is beta[..public_len]; witness follows.
            let mut value = Galois128(0);
            for i in 0..constraint.n_terms as usize {
                let term = &constraint.terms[i];
                let mut product = term.coeff;
                for v in 0..term.n_vars as usize {
                    let var = term.vars[v];
                    let x = if var < pesat.public_len {
                        beta[var]
                    } else {
                        witness[var - pesat.public_len]
                    };
                    product = product * x;
                }
                value = value + product;
            }
            out = out + eq[index] * value;
        }
        out
    }

    /// Ordinary PESAT satisfaction of a fresh witness (all constraints
    /// zero). Boundary invariant: `X == public_len`, `K == witness_len`.
    pub fn warp_is_instance_satisfied<M: ArraySize, TERMS: ArraySize, DEG: ArraySize, K: ArraySize, X: ArraySize>(
        pesat: &WarpIndex<M, TERMS, DEG>,
        instance: &WarpWitness<K, X>,
    ) -> bool {
        for index in 0..M::USIZE {
            let constraint = &pesat.constraints[index];
            if warp_eval_constraint(constraint, &instance.public, &instance.witness) != Galois128(0) {
                return false;
            }
        }
        true
    }

    /// Absorb the index into the transcript — byte-identical to the
    /// oracle's `absorb_index`.
    pub fn warp_absorb_index<M: ArraySize, TERMS: ArraySize, DEG: ArraySize>(pesat: &WarpIndex<M, TERMS, DEG>, transcript: &mut WarpTranscript) {
        transcript.absorb_array(&Array::<u8, U5>::from_fn(|i| b"index"[i]));
        transcript.absorb_usize(M::USIZE);
        transcript.absorb_usize(pesat.public_len);
        transcript.absorb_usize(pesat.witness_len);
        transcript.absorb_usize(pesat.degree_bound);
        for c in 0..M::USIZE {
            let constraint = &pesat.constraints[c];
            transcript.absorb_usize(constraint.n_terms as usize);
            for i in 0..TERMS::USIZE {
                if i < constraint.n_terms as usize {
                    let term = &constraint.terms[i];
                    transcript.absorb_field(term.coeff);
                    transcript.absorb_usize(term.n_vars as usize);
                    for j in 0..DEG::USIZE {
                        if j < term.n_vars as usize {
                            transcript.absorb_usize(term.vars[j]);
                        }
                    }
                }
            }
        }
    }


// ============================================================================
// Linear codes (W5) — test fixtures only; a reviewed production code is a
// separate step (see the module doc's soundness note).
// ============================================================================

/// A linear code `C: F^k -> F^n` (`n` a power of two for MLE indexing).
pub trait WarpCode<K: ArraySize, N: ArraySize> {
    /// Encode a message of length `K` into a codeword of length `N`.
    fn encode(&self, message: &Array<Galois128, K>) -> Array<Galois128, N>;
}

/// The identity code. Not a proximity-code parameter choice; no production
/// soundness claim by itself.
#[derive(Clone, Copy, Debug, Default)]
pub struct WarpIdentityCode;

impl<N: ArraySize> WarpCode<N, N> for WarpIdentityCode {
    fn encode(&self, message: &Array<Galois128, N>) -> Array<Galois128, N> {
        message.clone()
    }
}

/// A systematic repetition code `w -> (w, ..., w)` — `N / K` copies.
#[derive(Clone, Copy, Debug, Default)]
pub struct WarpRepetitionCode;

impl<K: ArraySize, N: ArraySize> WarpCode<K, N> for WarpRepetitionCode {
    fn encode(&self, message: &Array<Galois128, K>) -> Array<Galois128, N> {
        Array::from_fn(|i| message[i % K::USIZE])
    }
}

// ============================================================================
// Sumcheck univariates — evaluations at 0..=count-1 (the oracle's
// WarpUnivariate), capacity-plus-count.
// ============================================================================

/// A univariate polynomial as its evaluations at `0, 1, ..., count - 1`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpUni<CAP: ArraySize> {
    pub evaluations: Array<Galois128, CAP>,
    pub count: u32,
}


    pub fn warp_uni_at_zero<CAP: ArraySize>(poly: &WarpUni<CAP>) -> Galois128 {
        poly.evaluations[0]
    }

    pub fn warp_uni_at_one<CAP: ArraySize>(poly: &WarpUni<CAP>) -> Galois128 {
        poly.evaluations[1]
    }

    /// Lagrange evaluation at an arbitrary field point — the oracle's
    /// `WarpUnivariate::evaluate` (O(d^2), evaluation form).
    pub fn warp_uni_evaluate<CAP: ArraySize>(poly: &WarpUni<CAP>, x: Galois128) -> Galois128 {
        let n = poly.count as usize;
        let mut out = Galois128(0);
        // Guarded over the capacity: exactly the first `count` evaluations
        // participate (concrete loop bound, runtime guard).
        for i in 0..CAP::USIZE {
            if i < n {
                let mut numerator = Galois128(1);
                let mut denominator = Galois128(1);
                for j in 0..CAP::USIZE {
                    if j < n && i != j {
                        numerator = numerator * (x - Galois128(j as u128));
                        denominator = denominator * (Galois128(i as u128) - Galois128(j as u128));
                    }
                }
                out = out + poly.evaluations[i] * numerator * warp_inv(denominator);
            }
        }
        out
    }


/// Multiplicative inverse in GF(2^128) via Fermat exponentiation
/// (`a^(2^128 - 2)`), matching the oracle's `Field::inv` exactly.
/// Returns 0 for 0 (the oracle's convention).
pub fn warp_inv(a: Galois128) -> Galois128 {
    let mut result = Galois128(1);
    for _ in 0..127u32 {
        result = (result * result) * a;
    }
    result * result
}

// ============================================================================
// Accumulators (split instance/witness) and the Fiat–Shamir proof.
// ============================================================================

/// The public/instance half of a WARP accumulator.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpAccInstance<LOGN: ArraySize, BETA: ArraySize> {
    /// Merkle root over the codeword.
    pub root: WarpDigest,
    /// Final multilinear-evaluation point.
    pub alpha: Array<Galois128, LOGN>,
    /// Claimed `f_hat(alpha)`.
    pub mu: Galois128,
    /// Final bundled-PESAT evaluation point (`x || tau`).
    pub beta: Array<Galois128, BETA>,
    /// Claimed bundled PESAT value.
    pub eta: Galois128,
}

/// The prover/decider half of a WARP accumulator.
#[derive(Clone, Debug)]
pub struct WarpAccWitness<N: ArraySize, K: ArraySize, NODES: ArraySize> {
    /// Merkle tree committing to `codeword`.
    pub tree: WarpMerkleTree<NODES>,
    /// The codeword `f`.
    pub codeword: Array<Galois128, N>,
    /// The message/witness `w` with `f = C(w)`.
    pub witness: Array<Galois128, K>,
}

/// A split WARP accumulator.
#[derive(Clone, Debug)]
pub struct WarpAcc<N: ArraySize, K: ArraySize, LOGN: ArraySize, BETA: ArraySize, NODES: ArraySize>
{
    pub instance: WarpAccInstance<LOGN, BETA>,
    pub witness: WarpAccWitness<N, K, NODES>,
}

/// The Fiat–Shamir proof emitted by [`warp_prove_spec`].
#[derive(Clone, Debug)]
pub struct WarpProof<
    FRESH: ArraySize,
    ELL: ArraySize,
    A: ArraySize,
    S: ArraySize,
    SHIFTS: ArraySize,
    SC: ArraySize,
    LOGN: ArraySize,
> where
    ELL: Logarithm2,
    <ELL as Logarithm2>::Output: ArraySize,
{
    /// Root of the stacked fresh-witness Merkle commitment.
    pub initial_root: WarpDigest,
    /// Fresh-instance evaluations at `alpha = 0^{log n}`.
    pub fresh_mus: Array<Galois128, FRESH>,
    /// The `log ell` twin-constraint pseudo-batching polynomials.
    pub first_sumcheck: Array<WarpUni<SC>, <ELL as Logarithm2>::Output>,
    /// Claimed evaluation of the folded codeword at `A_hat(gamma)`.
    pub nu0: Galois128,
    /// Out-of-domain answers for the folded codeword.
    pub ood_answers: Array<Galois128, S>,
    /// The `log n` multilinear-claim batching polynomials.
    pub second_sumcheck: Array<WarpUni<SC>, LOGN>,
    /// One authentication path per shift query for the fresh commitment.
    pub initial_paths: Array<WarpAuthPath<LOGN>, SHIFTS>,
    /// One path list per input accumulator, one path per shift query.
    pub accumulator_paths: Array<Array<WarpAuthPath<LOGN>, SHIFTS>, A>,
    /// Query-major opened values: `openings[query][input]`.
    pub openings: Array<Array<Galois128, ELL>, SHIFTS>,
}

/// Absorb one accumulator instance — the oracle's `absorb_instance`.
pub fn warp_absorb_instance<LOGN: ArraySize, BETA: ArraySize>(
    transcript: &mut WarpTranscript,
    instance: &WarpAccInstance<LOGN, BETA>,
) {
    transcript.absorb_array(&Array::<u8, U12>::from_fn(|i| b"acc-instance"[i]));
    transcript.absorb_len_prefixed(&instance.root);
    transcript.absorb_fields(&instance.alpha);
    transcript.absorb_field(instance.mu);
    transcript.absorb_fields(&instance.beta);
    transcript.absorb_field(instance.eta);
}

/// The Construction 10.4 transcript prefix — the oracle's `transcript_for`.
pub fn warp_transcript_for<M, TERMS, DEG, X, FRESH, A, LOGN, BETA>(
    index: &WarpIndex<M, TERMS, DEG>,
    fresh_publics: &Array<Array<Galois128, X>, FRESH>,
    accumulators: &Array<WarpAccInstance<LOGN, BETA>, A>,
) -> WarpTranscript
where
    M: ArraySize,
    TERMS: ArraySize,
    DEG: ArraySize,
    X: ArraySize,
    FRESH: ArraySize,
    A: ArraySize,
    LOGN: ArraySize,
    BETA: ArraySize,
{
    let domain = Array::<u8, U51>::from_fn(|i| WARP_TRANSCRIPT_DOMAIN[i]);
    let mut transcript = warp_transcript_new().domain_sep(&domain);
    warp_absorb_index(index, &mut transcript);
    transcript.absorb_usize(FRESH::USIZE);
    for i in 0..FRESH::USIZE {
        transcript.absorb_array(&Array::<u8, U12>::from_fn(|i| b"fresh-public"[i]));
        transcript.absorb_fields(&fresh_publics[i]);
    }
    transcript.absorb_usize(A::USIZE);
    for i in 0..A::USIZE {
        warp_absorb_instance(&mut transcript, &accumulators[i]);
    }
    transcript
}

/// Construct a genesis accumulator for one fresh PESAT statement — the
/// oracle's `warp_genesis` (Construction 5.10 with deterministic
/// `tau = 0^{log M}`). Returns `None` when the index is invalid, the shape
/// is off, or the witness does not satisfy the PESAT instance.
pub fn warp_genesis_spec<M, TERMS, DEG, K, X, N, C, NODES, BETA>(
    index: &WarpIndex<M, TERMS, DEG>,
    code: &C,
    instance: &WarpWitness<K, X>,
) -> Option<WarpAcc<N, K, <N as Logarithm2>::Output, BETA, NODES>>
where
    M: ArraySize,
    TERMS: ArraySize,
    DEG: ArraySize,
    K: ArraySize,
    X: ArraySize,
    N: ArraySize + Logarithm2,
    <N as Logarithm2>::Output: ArraySize,
    C: WarpCode<K, N>,
    NODES: ArraySize,
    BETA: ArraySize,
{
    if !warp_index_is_valid(index) || !is_pow2(N::USIZE) {
        return None;
    }
    if X::USIZE != index.public_len
        || K::USIZE != index.witness_len
        || BETA::USIZE != index.public_len + warp_log_constraints(index)
    {
        return None;
    }
    if !warp_is_instance_satisfied(index, instance) {
        return None;
    }
    let codeword = code.encode(&instance.witness);
    let tree = warp_commit_codeword::<N, NODES>(&codeword);
    let alpha: Array<Galois128, <N as Logarithm2>::Output> = Array::<Galois128, <N as Logarithm2>::Output>::default();
    let mut beta = Array::<Galois128, BETA>::default();
    for i in 0..X::USIZE {
        beta[i] = instance.public[i];
    }
    let mu = warp_mle_eval(&codeword, &alpha);
    let eta = warp_evaluate_bundled(index, &beta, &instance.witness);
    Some(WarpAcc {
        instance: WarpAccInstance {
            root: warp_merkle_root(&tree),
            alpha,
            mu,
            beta,
            eta,
        },
        witness: WarpAccWitness {
            tree,
            codeword,
            witness: instance.witness.clone(),
        },
    })
}

/// Decide a split WARP accumulator — the oracle's `warp_decide` (D_ACC):
/// Merkle root recomputation, `f = C(w)`, `f_hat(alpha) = mu`, and
/// `P_hat(beta, w) = eta`. Returns the accept bit.
pub fn warp_decide_spec<M, TERMS, DEG, N, K, LOGN, BETA, NODES, C>(
    index: &WarpIndex<M, TERMS, DEG>,
    code: &C,
    accumulator: &WarpAcc<N, K, LOGN, BETA, NODES>,
) -> bool
where
    M: ArraySize,
    TERMS: ArraySize,
    DEG: ArraySize,
    N: ArraySize + Logarithm2,
    K: ArraySize,
    LOGN: ArraySize,
    BETA: ArraySize,
    NODES: ArraySize,
    C: WarpCode<K, N>,
    <N as Logarithm2>::Output: ArraySize,
{
    if !warp_index_is_valid(index) {
        return false;
    }
    if LOGN::USIZE != log2_pow2(N::USIZE)
        || BETA::USIZE != index.public_len + warp_log_constraints(index)
        || K::USIZE != index.witness_len
        || warp_merkle_root(&accumulator.witness.tree) != accumulator.instance.root
    {
        return false;
    }
    let recomputed = warp_commit_codeword::<N, NODES>(&accumulator.witness.codeword);
    if warp_merkle_root(&recomputed) != accumulator.instance.root {
        return false;
    }
    if code.encode(&accumulator.witness.witness) != accumulator.witness.codeword {
        return false;
    }
    if warp_mle_eval(&accumulator.witness.codeword, &accumulator.instance.alpha)
        != accumulator.instance.mu
    {
        return false;
    }
    warp_evaluate_bundled(index, &accumulator.instance.beta, &accumulator.witness.witness)
        == accumulator.instance.eta
}

// ============================================================================
// The prover (W4) — Construction 10.4's P_ACC.
// ============================================================================

/// `eq`-weighted combination of `ELL` vectors by `point` (`log ELL`
/// coordinates) — the oracle's `combine_vectors`. Boundary invariant:
/// `ELL == 2^LOGL`.
pub fn warp_combine_vectors<ELL, LOGL, LEN>(
    items: &Array<Array<Galois128, LEN>, ELL>,
    point: &Array<Galois128, LOGL>,
) -> Array<Galois128, LEN>
where
    ELL: ArraySize,
    LOGL: ArraySize,
    LEN: ArraySize,
{
    let weights = warp_eq_evals::<LOGL, ELL>(point);
    let mut out = Array::<Galois128, LEN>::default();
    for i in 0..ELL::USIZE {
        let weight = weights[i];
        for j in 0..LEN::USIZE {
            out[j] = out[j] + items[i][j] * weight;
        }
    }
    out
}

/// `prefix || [current] || binary(suffix)` — the oracle's `point_with_suffix`.
fn warp_point_with_suffix<LOG: ArraySize>(
    prefix: &Array<Galois128, LOG>,
    prefix_len: usize,
    current: Galois128,
    suffix: usize,
    suffix_len: usize,
) -> Array<Galois128, LOG> {
    Array::from_fn(|i| {
        if i < prefix_len {
            prefix[i]
        } else if i == prefix_len {
            current
        } else {
            let bit = i - prefix_len - 1;
            if bit < suffix_len && (suffix >> bit) & 1 == 1 {
                Galois128(1)
            } else {
                Galois128(0)
            }
        }
    })
}

/// Construction 6.3's `G_hat` summand — the oracle's `evaluate_g`.
fn warp_evaluate_g<M, TERMS, DEG, N, K, LOGN, BETA>(
    index: &WarpIndex<M, TERMS, DEG>,
    codeword: &Array<Galois128, N>,
    witness: &Array<Galois128, K>,
    alpha: &Array<Galois128, LOGN>,
    beta: &Array<Galois128, BETA>,
    omega: Galois128,
) -> Galois128
where
    M: ArraySize,
    TERMS: ArraySize,
    DEG: ArraySize,
    N: ArraySize + Logarithm2,
    K: ArraySize,
    LOGN: ArraySize,
    BETA: ArraySize,
    <N as Logarithm2>::Output: ArraySize,
{
    let evaluation = warp_mle_eval(codeword, alpha);
    let bundled = warp_evaluate_bundled(index, beta, witness);
    evaluation + omega * bundled
}

/// The Construction 8.2 eq-star sum — the oracle's `eq_star_at`.
/// Boundary invariant: `R == 2^LOGR`.
fn warp_eq_star_at<R, LOGR, LOGN>(
    claim_points: &Array<Array<Galois128, LOGN>, R>,
    xi: &Array<Galois128, LOGR>,
    point: &Array<Galois128, LOGN>,
) -> Galois128
where
    R: ArraySize,
    LOGR: ArraySize,
    LOGN: ArraySize,
{
    let mut out = Galois128(0);
    for index in 0..R::USIZE {
        let claim_index = warp_binary_point::<LOGR>(index);
        let weight = warp_eq_scalar(xi, &claim_index) * warp_eq_scalar(&claim_points[index], point);
        out = out + weight;
    }
    out
}

/// One first-sumcheck evaluation at `x` — the oracle's `first_sumcheck_eval`.
#[allow(clippy::too_many_arguments)]
fn warp_first_sumcheck_eval<M, TERMS, DEG, K, N, ELL, LOGL, LOGN, BETA>(
    index: &WarpIndex<M, TERMS, DEG>,
    codewords: &Array<Array<Galois128, N>, ELL>,
    witnesses: &Array<Array<Galois128, K>, ELL>,
    alphas: &Array<Array<Galois128, LOGN>, ELL>,
    betas: &Array<Array<Galois128, BETA>, ELL>,
    tau: &Array<Galois128, LOGL>,
    omega: Galois128,
    prefix: &Array<Galois128, LOGL>,
    round: usize,
    x: Galois128,
) -> Galois128
where
    M: ArraySize,
    TERMS: ArraySize,
    DEG: ArraySize,
    K: ArraySize,
    N: ArraySize + Logarithm2,
    ELL: ArraySize,
    LOGL: ArraySize,
    LOGN: ArraySize,
    BETA: ArraySize,
    <N as Logarithm2>::Output: ArraySize,
{
    let log_ell = log2_pow2(ELL::USIZE);
    let suffix_len = log_ell - round - 1;
    let mut out = Galois128(0);
    // Suffix count = 2^(log_ell - round - 1) = ELL >> (round + 1) — a
    // const-expr bound (the dialect requires concrete bounds; `round` is
    // the unrolled outer counter, a per-iteration constant).
    for suffix in 0..(ELL::USIZE >> (round + 1)) {
        let point = warp_point_with_suffix::<LOGL>(prefix, round, x, suffix, suffix_len);
        let codeword = warp_combine_vectors(codewords, &point);
        let witness = warp_combine_vectors(witnesses, &point);
        let alpha = warp_combine_vectors(alphas, &point);
        let beta = warp_combine_vectors(betas, &point);
        let g = warp_evaluate_g(index, &codeword, &witness, &alpha, &beta, omega);
        out = out + warp_eq_scalar(tau, &point) * g;
    }
    out
}

/// One second-sumcheck evaluation at `x` — the oracle's
/// `second_sumcheck_eval`.
fn warp_second_sumcheck_eval<N, R, LOGR, LOGN>(
    codeword: &Array<Galois128, N>,
    claim_points: &Array<Array<Galois128, LOGN>, R>,
    xi: &Array<Galois128, LOGR>,
    prefix: &Array<Galois128, LOGN>,
    round: usize,
    x: Galois128,
) -> Galois128
where
    N: ArraySize + Logarithm2,
    R: ArraySize,
    LOGR: ArraySize,
    LOGN: ArraySize,
    <N as Logarithm2>::Output: ArraySize,
{
    let log_n = log2_pow2(N::USIZE);
    let suffix_len = log_n - round - 1;
    let mut out = Galois128(0);
    // Suffix count = N >> (round + 1) — a const-expr bound (see the first
    // sumcheck's note).
    for suffix in 0..(N::USIZE >> (round + 1)) {
        let point = warp_point_with_suffix::<LOGN>(prefix, round, x, suffix, suffix_len);
        let eq = warp_eq_star_at(claim_points, xi, &point);
        out = out + eq * warp_mle_eval(codeword, &point);
    }
    out
}

/// The sumcheck link/target check — the oracle's `check_sumcheck`.
pub fn warp_check_sumcheck<SC: ArraySize, ROUNDS: ArraySize>(
    polys: &Array<WarpUni<SC>, ROUNDS>,
    initial_claim: Galois128,
    challenges: &Array<Galois128, ROUNDS>,
    final_claim: Galois128,
) -> bool {
    if ROUNDS::USIZE == 0 {
        return initial_claim == final_claim;
    }
    if warp_uni_at_zero(&polys[0]) + warp_uni_at_one(&polys[0]) != initial_claim {
        return false;
    }
    for round in 1..ROUNDS::USIZE {
        let expected = warp_uni_evaluate(&polys[round - 1], challenges[round - 1]);
        if warp_uni_at_zero(&polys[round]) + warp_uni_at_one(&polys[round]) != expected {
            return false;
        }
    }
    warp_uni_evaluate(&polys[ROUNDS::USIZE - 1], challenges[ROUNDS::USIZE - 1]) == final_claim
}

/// Prove Construction 10.4 for fresh PESAT inputs plus prior accumulators.
///
/// Returns `None` (fail-closed) on any shape violation — the type-level
/// relations `ELL = FRESH + A`, `R = 1 + S + SHIFTS`, `TW = log2(ELL) + 1`,
/// `BETA = public_len + log2(M)`, `NODES = 2N - 1`, power-of-two `N`/`M`/
/// `ELL`/`R`, plus PESAT satisfaction of every fresh witness and each
/// accumulator's root consistency. The first-sumcheck degree is
/// `1 + max(log n + 1, log M + degree_bound)` and must fit `SC - 1`.
#[allow(clippy::too_many_arguments, clippy::type_complexity)]
pub fn warp_prove_spec<M, TERMS, DEG, K, X, N, FRESH, A, ELL, S, SHIFTS, R, SC, TW, FSD1, NODES, BETA, C>(
    index: &WarpIndex<M, TERMS, DEG>,
    code: &C,
    fresh: &Array<WarpWitness<K, X>, FRESH>,
    accumulators: &Array<WarpAcc<N, K, <N as Logarithm2>::Output, BETA, NODES>, A>,
) -> Option<(
    WarpAcc<N, K, <N as Logarithm2>::Output, BETA, NODES>,
    WarpProof<FRESH, ELL, A, S, SHIFTS, SC, <N as Logarithm2>::Output>,
)>
where
    M: ArraySize + Logarithm2,
    TERMS: ArraySize,
    DEG: ArraySize,
    K: ArraySize,
    X: ArraySize,
    N: ArraySize + Logarithm2,
    FRESH: ArraySize,
    A: ArraySize,
    ELL: ArraySize + Logarithm2,
    S: ArraySize,
    SHIFTS: ArraySize,
    R: ArraySize + Logarithm2,
    SC: ArraySize,
    TW: ArraySize,
    FSD1: ArraySize,
    NODES: ArraySize,
    BETA: ArraySize,
    C: WarpCode<K, N>,
    <M as Logarithm2>::Output: ArraySize,
    <N as Logarithm2>::Output: ArraySize,
    <ELL as Logarithm2>::Output: ArraySize,
    <R as Logarithm2>::Output: ArraySize,
{
    // ---- shape validation (the oracle's validate_input_shapes) ----
    if !warp_index_is_valid(index) {
        return None;
    }
    let log_n = log2_pow2(N::USIZE);
    let log_m = warp_log_constraints(index);
    let log_ell = log2_pow2(ELL::USIZE);
    if !is_pow2(N::USIZE)
        || !is_pow2(ELL::USIZE)
        || ELL::USIZE != FRESH::USIZE + A::USIZE
        || FRESH::USIZE == 0
        || !is_pow2(R::USIZE)
        || R::USIZE != 1 + S::USIZE + SHIFTS::USIZE
        || TW::USIZE != log_ell + 1
        || BETA::USIZE != index.public_len + log_m
        || K::USIZE != index.witness_len
        || X::USIZE != index.public_len
        || NODES::USIZE != 2 * N::USIZE - 1
    {
        return None;
    }
    let first_degree = 1 + (log_n + 1).max(log_m + index.degree_bound);
    if FSD1::USIZE != first_degree + 1 || FSD1::USIZE > SC::USIZE || 3 > SC::USIZE {
        return None;
    }
    for i in 0..FRESH::USIZE {
        if !warp_is_instance_satisfied(index, &fresh[i]) {
            return None;
        }
    }
    for i in 0..A::USIZE {
        if warp_merkle_root(&accumulators[i].witness.tree) != accumulators[i].instance.root {
            return None;
        }
    }

    // ---- transcript prefix ----
    let fresh_publics = Array::<Array<Galois128, X>, FRESH>::from_fn(|i| fresh[i].public.clone());
    let acc_instances =
        Array::<WarpAccInstance<<N as Logarithm2>::Output, BETA>, A>::from_fn(|i| accumulators[i].instance.clone());
    let mut transcript = warp_transcript_for(index, &fresh_publics, &acc_instances);

    // ---- Construction 5.10 + Remark 5.9: encode, commit, zerocheck ----
    let fresh_codewords =
        Array::<Array<Galois128, N>, FRESH>::from_fn(|i| code.encode(&fresh[i].witness));
    let initial_tree = warp_commit_stacked::<N, FRESH, NODES>(&fresh_codewords);
    let initial_root = warp_merkle_root(&initial_tree);
    transcript.absorb_len_prefixed(&initial_root);

    let taus: Array<Array<Galois128, <M as Logarithm2>::Output>, FRESH> = transcript.squeeze_field_grid();
    let zero_point_n = Array::<Galois128, <N as Logarithm2>::Output>::default();
    let fresh_mus = Array::<Galois128, FRESH>::from_fn(|i| {
        warp_mle_eval(&fresh_codewords[i], &zero_point_n)
    });

    let mut alphas = Array::<Array<Galois128, <N as Logarithm2>::Output>, ELL>::default();
    let mut mus = Array::<Galois128, ELL>::default();
    let mut betas = Array::<Array<Galois128, BETA>, ELL>::default();
    let mut etas = Array::<Galois128, ELL>::default();
    let mut codewords = Array::<Array<Galois128, N>, ELL>::default();
    let mut witnesses = Array::<Array<Galois128, K>, ELL>::default();
    for i in 0..FRESH::USIZE {
        alphas[i] = Array::<Galois128, <N as Logarithm2>::Output>::default();
        mus[i] = fresh_mus[i];
        let mut beta = Array::<Galois128, BETA>::default();
        for j in 0..X::USIZE {
            beta[j] = fresh[i].public[j];
        }
        for j in 0..M::USIZE {
            if (1usize << j) < M::USIZE {
                beta[X::USIZE + j] = taus[i][j];
            }
        }
        betas[i] = beta;
        etas[i] = Galois128(0);
        codewords[i] = fresh_codewords[i].clone();
        witnesses[i] = fresh[i].witness.clone();
    }
    transcript.absorb_fields(&fresh_mus);
    for i in 0..A::USIZE {
        let acc = &accumulators[i];
        alphas[FRESH::USIZE + i] = acc.instance.alpha.clone();
        mus[FRESH::USIZE + i] = acc.instance.mu;
        betas[FRESH::USIZE + i] = acc.instance.beta.clone();
        etas[FRESH::USIZE + i] = acc.instance.eta;
        codewords[FRESH::USIZE + i] = acc.witness.codeword.clone();
        witnesses[FRESH::USIZE + i] = acc.witness.witness.clone();
    }

    // ---- Construction 6.3 (inlined in 9.4): twin pseudo-batching ----
    let tau_omega: Array<Galois128, TW> = transcript.squeeze_fields();
    let tau = Array::<Galois128, <ELL as Logarithm2>::Output>::from_fn(|i| tau_omega[i]);
    let omega = tau_omega[TW::USIZE - 1];
    let sigma1 = {
        let eq_tau = warp_eq_evals::<<ELL as Logarithm2>::Output, ELL>(&tau);
        let mut out = Galois128(0);
        for i in 0..ELL::USIZE {
            out = out + eq_tau[i] * (mus[i] + omega * etas[i]);
        }
        out
    };

    let mut first_sumcheck = Array::<WarpUni<SC>, <ELL as Logarithm2>::Output>::from_fn(|_| WarpUni {
        evaluations: Array::<Galois128, SC>::default(),
        count: 0,
    });
    let mut gamma = Array::<Galois128, <ELL as Logarithm2>::Output>::default();
    // Guarded over ELL: exactly the first log2(ELL) rounds act (concrete
    // bound, per the dialect's loop rule).
    for round in 0..ELL::USIZE {
        if (1usize << round) < ELL::USIZE {
            let mut evals = Array::<Galois128, SC>::default();
            for x in 0..FSD1::USIZE {
                evals[x] = warp_first_sumcheck_eval(
                    index,
                    &codewords,
                    &witnesses,
                    &alphas,
                    &betas,
                    &tau,
                    omega,
                    &gamma,
                    round,
                    Galois128(x as u128),
                );
            }
            let poly = WarpUni::<SC> {
                evaluations: evals,
                count: FSD1::USIZE as u32,
            };
            transcript.absorb_poly::<SC, FSD1>(&poly);
            gamma[round] = transcript.squeeze_fields::<U1>()[0];
            first_sumcheck[round] = poly;
        }
    }
    let zeta0 = warp_combine_vectors(&alphas, &gamma);
    let beta = warp_combine_vectors(&betas, &gamma);
    let folded_codeword = warp_combine_vectors(&codewords, &gamma);
    let folded_witness = warp_combine_vectors(&witnesses, &gamma);
    let nu0 = warp_mle_eval(&folded_codeword, &zeta0);
    let eta = warp_evaluate_bundled(index, &beta, &folded_witness);
    if !warp_check_sumcheck(&first_sumcheck, sigma1, &gamma, {
        warp_eq_scalar(&tau, &gamma) * (nu0 + omega * eta)
    }) {
        return None;
    }

    // ---- Construction 7.2: commit, OOD samples, shift queries ----
    let output_tree = warp_commit_codeword::<N, NODES>(&folded_codeword);
    let output_root = warp_merkle_root(&output_tree);
    transcript.absorb_len_prefixed(&output_root);
    transcript.absorb_field(nu0);
    transcript.absorb_field(eta);
    let ood_points: Array<Array<Galois128, <N as Logarithm2>::Output>, S> = transcript.squeeze_field_grid();
    let ood_answers = Array::<Galois128, S>::from_fn(|i| {
        warp_mle_eval(&folded_codeword, &ood_points[i])
    });
    transcript.absorb_fields(&ood_answers);

    let (shift_indices, xi): (Array<u32, SHIFTS>, Array<Galois128, <R as Logarithm2>::Output>) =
        transcript.squeeze_shift_and_xi::<SHIFTS, <R as Logarithm2>::Output, <N as Logarithm2>::Output>();
    let mut claim_points = Array::<Array<Galois128, <N as Logarithm2>::Output>, R>::default();
    let mut claim_values = Array::<Galois128, R>::default();
    claim_points[0] = zeta0;
    claim_values[0] = nu0;
    for i in 0..S::USIZE {
        claim_points[1 + i] = ood_points[i].clone();
        claim_values[1 + i] = ood_answers[i];
    }
    for i in 0..SHIFTS::USIZE {
        claim_points[1 + S::USIZE + i] = warp_binary_point(shift_indices[i] as usize);
        claim_values[1 + S::USIZE + i] = folded_codeword[shift_indices[i] as usize];
    }
    let sigma2 = {
        let eq_xi = warp_eq_evals::<<R as Logarithm2>::Output, R>(&xi);
        let mut out = Galois128(0);
        for i in 0..R::USIZE {
            out = out + claim_values[i] * eq_xi[i];
        }
        out
    };

    // ---- Construction 8.2: batch the claims into one ----
    let mut second_sumcheck = Array::<WarpUni<SC>, <N as Logarithm2>::Output>::from_fn(|_| WarpUni {
        evaluations: Array::<Galois128, SC>::default(),
        count: 0,
    });
    let mut alpha = Array::<Galois128, <N as Logarithm2>::Output>::default();
    for round in 0..N::USIZE {
        if (1usize << round) < N::USIZE {
            let mut evals = Array::<Galois128, SC>::default();
            for x in 0..3usize {
                evals[x] = warp_second_sumcheck_eval(
                    &folded_codeword,
                    &claim_points,
                    &xi,
                    &alpha,
                    round,
                    Galois128(x as u128),
                );
            }
            let poly = WarpUni::<SC> {
                evaluations: evals,
                count: 3,
            };
            transcript.absorb_poly::<SC, U3>(&poly);
            alpha[round] = transcript.squeeze_fields::<U1>()[0];
            second_sumcheck[round] = poly;
        }
    }
    let mu = warp_mle_eval(&folded_codeword, &alpha);
    if !warp_check_sumcheck(&second_sumcheck, sigma2, &alpha, {
        let eq = warp_eq_star_at(&claim_points, &xi, &alpha);
        mu * eq
    }) {
        return None;
    }

    // ---- openings ----
    let initial_paths = Array::<WarpAuthPath<<N as Logarithm2>::Output>, SHIFTS>::from_fn(|i| {
        warp_merkle_open(&initial_tree, shift_indices[i] as usize)
    });
    let accumulator_paths = Array::<Array<WarpAuthPath<<N as Logarithm2>::Output>, SHIFTS>, A>::from_fn(|i| {
        Array::<WarpAuthPath<<N as Logarithm2>::Output>, SHIFTS>::from_fn(|j| {
            warp_merkle_open(&accumulators[i].witness.tree, shift_indices[j] as usize)
        })
    });
    let openings = Array::<Array<Galois128, ELL>, SHIFTS>::from_fn(|j| {
        Array::<Galois128, ELL>::from_fn(|i| codewords[i][shift_indices[j] as usize])
    });

    let accumulator = WarpAcc {
        instance: WarpAccInstance {
            root: output_root,
            alpha,
            mu,
            beta,
            eta,
        },
        witness: WarpAccWitness {
            tree: output_tree,
            codeword: folded_codeword,
            witness: folded_witness,
        },
    };
    let proof = WarpProof {
        initial_root,
        fresh_mus,
        first_sumcheck,
        nu0,
        ood_answers,
        second_sumcheck,
        initial_paths,
        accumulator_paths,
        openings,
    };
    Some((accumulator, proof))
}

// ============================================================================
// The verifier (W4) — Construction 10.4's V_ACC.
// ============================================================================

/// Hash one stacked fresh leaf: the concatenation of the first `FRESH`
/// entries of an openings row (the oracle's fresh-leaf construction).
fn warp_hash_fresh_row<ELL: ArraySize, FRESH: ArraySize>(
    row: &Array<Galois128, ELL>,
) -> WarpDigest {
    let mut h = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb_array(&mut h, &Array::<u8, U1>::from_fn(|_| 0x00));
    for i in 0..FRESH::USIZE {
        let row_bytes: Array<u8, U16> = le16(row[i]);
        spec_sponge_absorb_array(&mut h, &row_bytes);
    }
    let mut reader = spec_sponge_finalize_xof(&h, 0x06);
    let mut out: Array<u8, U32> = Array::<u8, U32>::default();
    spec_xof_read_array(&mut reader, &mut out);
    out
}

/// Verify a Construction 10.4 accumulation proof, returning the accept bit
/// (the verifier-as-computation output: `true` = accept, `false` = reject —
/// fail-closed on any mismatch).
///
/// Every shape the verifier relies on is carried in types (part of the
/// verification key, compile-time fixed) and cross-checked fail-closed at
/// entry — the verifier never infers a shape from prover-controlled data
/// (the oracle's `WarpSampling::log_codeword_len` lesson: here `LOGN` comes
/// from the proof/output *types*, which the caller picks).
#[allow(clippy::too_many_arguments, clippy::type_complexity)]
pub fn warp_verify_spec<M, TERMS, DEG, X, FRESH, A, ELL, S, SHIFTS, R, SC, TW, FSD1, N, BETA>(
    index: &WarpIndex<M, TERMS, DEG>,
    fresh_publics: &Array<Array<Galois128, X>, FRESH>,
    input_accumulators: &Array<WarpAccInstance<<N as Logarithm2>::Output, BETA>, A>,
    output: &WarpAccInstance<<N as Logarithm2>::Output, BETA>,
    proof: &WarpProof<FRESH, ELL, A, S, SHIFTS, SC, <N as Logarithm2>::Output>,
) -> bool
where
    M: ArraySize + Logarithm2,
    TERMS: ArraySize,
    DEG: ArraySize,
    X: ArraySize,
    FRESH: ArraySize,
    A: ArraySize,
    ELL: ArraySize + Logarithm2,
    S: ArraySize,
    SHIFTS: ArraySize,
    R: ArraySize + Logarithm2,
    SC: ArraySize,
    TW: ArraySize,
    FSD1: ArraySize,
    N: ArraySize + Logarithm2,
    BETA: ArraySize,
    <M as Logarithm2>::Output: ArraySize,
    <ELL as Logarithm2>::Output: ArraySize,
    <R as Logarithm2>::Output: ArraySize,
    <N as Logarithm2>::Output: ArraySize,
{
    if !warp_index_is_valid(index) {
        return false;
    }
    let log_m = warp_log_constraints(index);
    let log_ell = log2_pow2(ELL::USIZE);
    let log_n = log2_pow2(N::USIZE);
    // `N` is the codeword length, `<N as Logarithm2>::Output` its log2 — the verification key's
    // own shape, checked here (never inferred from prover-controlled data).
    if FRESH::USIZE == 0
        || !is_pow2(ELL::USIZE)
        || ELL::USIZE != FRESH::USIZE + A::USIZE
        || !is_pow2(R::USIZE)
        || R::USIZE != 1 + S::USIZE + SHIFTS::USIZE
        || TW::USIZE != log_ell + 1
        || BETA::USIZE != index.public_len + log_m
        || X::USIZE != index.public_len
        || N::USIZE != (1usize << log2_pow2(N::USIZE))
        || FSD1::USIZE != 2 + (log_n + 1).max(log_m + index.degree_bound)
    {
        return false;
    }

    // ---- transcript prefix ----
    let mut transcript = warp_transcript_for(index, fresh_publics, input_accumulators);
    transcript.absorb_len_prefixed(&proof.initial_root);
    // log_m == 0 squeezes zero bytes here (the oracle's explicit skip).
    let taus: Array<Array<Galois128, <M as Logarithm2>::Output>, FRESH> = transcript.squeeze_field_grid();
    transcript.absorb_fields(&proof.fresh_mus);

    // ---- instance vectors (fresh then accumulators) ----
    let mut alphas = Array::<Array<Galois128, <N as Logarithm2>::Output>, ELL>::default();
    let mut mus = Array::<Galois128, ELL>::default();
    let mut betas = Array::<Array<Galois128, BETA>, ELL>::default();
    let mut etas = Array::<Galois128, ELL>::default();
    for i in 0..FRESH::USIZE {
        mus[i] = proof.fresh_mus[i];
        let mut beta_i = Array::<Galois128, BETA>::default();
        for j in 0..X::USIZE {
            beta_i[j] = fresh_publics[i][j];
        }
        for j in 0..M::USIZE {
            if (1usize << j) < M::USIZE {
                beta_i[X::USIZE + j] = taus[i][j];
            }
        }
        betas[i] = beta_i;
        etas[i] = Galois128(0);
    }
    for i in 0..A::USIZE {
        let acc = &input_accumulators[i];
        alphas[FRESH::USIZE + i] = acc.alpha.clone();
        mus[FRESH::USIZE + i] = acc.mu;
        betas[FRESH::USIZE + i] = acc.beta.clone();
        etas[FRESH::USIZE + i] = acc.eta;
    }

    // ---- Construction 6.3: twin pseudo-batching ----
    let tau_omega: Array<Galois128, TW> = transcript.squeeze_fields();
    let tau = Array::<Galois128, <ELL as Logarithm2>::Output>::from_fn(|i| tau_omega[i]);
    let omega = tau_omega[TW::USIZE - 1];
    let mut gamma = Array::<Galois128, <ELL as Logarithm2>::Output>::default();
    for round in 0..ELL::USIZE {
        if (1usize << round) < ELL::USIZE {
            let poly = &proof.first_sumcheck[round];
            transcript.absorb_poly::<SC, FSD1>(poly);
            gamma[round] = transcript.squeeze_fields::<U1>()[0];
        }
    }
    let sigma1 = {
        let eq_tau = warp_eq_evals::<<ELL as Logarithm2>::Output, ELL>(&tau);
        let mut out = Galois128(0);
        for i in 0..ELL::USIZE {
            out = out + eq_tau[i] * (mus[i] + omega * etas[i]);
        }
        out
    };
    let zeta0 = warp_combine_vectors(&alphas, &gamma);
    let beta = warp_combine_vectors(&betas, &gamma);
    if beta != output.beta {
        return false;
    }
    if !warp_check_sumcheck(&proof.first_sumcheck, sigma1, &gamma, {
        warp_eq_scalar(&tau, &gamma) * (proof.nu0 + omega * output.eta)
    }) {
        return false;
    }

    // ---- Construction 7.2 ----
    transcript.absorb_len_prefixed(&output.root);
    transcript.absorb_field(proof.nu0);
    transcript.absorb_field(output.eta);
    let ood_points: Array<Array<Galois128, <N as Logarithm2>::Output>, S> = transcript.squeeze_field_grid();
    transcript.absorb_fields(&proof.ood_answers);
    let (shift_indices, xi): (Array<u32, SHIFTS>, Array<Galois128, <R as Logarithm2>::Output>) =
        transcript.squeeze_shift_and_xi::<SHIFTS, <R as Logarithm2>::Output, <N as Logarithm2>::Output>();

    // Openings: fresh share one stacked commitment; each prior accumulator
    // keeps its own root.
    for query in 0..SHIFTS::USIZE {
        let path = &proof.initial_paths[query];
        let row = &proof.openings[query];
        let leaf = warp_hash_fresh_row::<ELL, FRESH>(row);
        if !warp_merkle_verify_hashed(
            &proof.initial_root,
            &leaf,
            shift_indices[query] as usize,
            path,
        ) {
            return false;
        }
    }
    for accumulator_index in 0..A::USIZE {
        for query in 0..SHIFTS::USIZE {
            let path = &proof.accumulator_paths[accumulator_index][query];
            let value = proof.openings[query][FRESH::USIZE + accumulator_index];
            let leaf = warp_hash_leaf_field(value);
            if !warp_merkle_verify_hashed(
                &input_accumulators[accumulator_index].root,
                &leaf,
                shift_indices[query] as usize,
                path,
            ) {
                return false;
            }
        }
    }

    // ---- claim assembly ----
    let mut claim_points = Array::<Array<Galois128, <N as Logarithm2>::Output>, R>::default();
    let mut claim_values = Array::<Galois128, R>::default();
    claim_points[0] = zeta0;
    claim_values[0] = proof.nu0;
    for i in 0..S::USIZE {
        claim_points[1 + i] = ood_points[i].clone();
        claim_values[1 + i] = proof.ood_answers[i];
    }
    let eq_gamma = warp_eq_evals::<<ELL as Logarithm2>::Output, ELL>(&gamma);
    for i in 0..SHIFTS::USIZE {
        claim_points[1 + S::USIZE + i] = warp_binary_point(shift_indices[i] as usize);
        let row = &proof.openings[i];
        let mut value = Galois128(0);
        for j in 0..ELL::USIZE {
            value = value + row[j] * eq_gamma[j];
        }
        claim_values[1 + S::USIZE + i] = value;
    }
    let sigma2 = {
        let eq_xi = warp_eq_evals::<<R as Logarithm2>::Output, R>(&xi);
        let mut out = Galois128(0);
        for i in 0..R::USIZE {
            out = out + claim_values[i] * eq_xi[i];
        }
        out
    };

    // ---- Construction 8.2 ----
    let mut alpha = Array::<Galois128, <N as Logarithm2>::Output>::default();
    for round in 0..N::USIZE {
        if (1usize << round) < N::USIZE {
            let poly = &proof.second_sumcheck[round];
            transcript.absorb_poly::<SC, U3>(poly);
            alpha[round] = transcript.squeeze_fields::<U1>()[0];
        }
    }
    if alpha != output.alpha {
        return false;
    }
    warp_check_sumcheck(&proof.second_sumcheck, sigma2, &alpha, {
        let eq = warp_eq_star_at(&claim_points, &xi, &output.alpha);
        output.mu * eq
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use alloc::vec::Vec;
    use cipher::consts::*;

    /// The toy PESAT `w0*w1 + w2 + w3 = 0` (degree 2, one constraint) shared
    /// with the oracle's `toy_index`.
    pub(crate) fn toy_index() -> WarpIndex<U1, U4, U2> {
        let mk = |vars: [usize; 2], n_vars: u32| WarpTerm::<U2> {
            coeff: Galois128(1),
            vars: Array::from_fn(|i| vars[i]),
            n_vars,
        };
        WarpIndex {
            constraints: Array::from_fn(|_| WarpConstraint::<U4, U2> {
                terms: Array::from_fn(|i| match i {
                    0 => mk([0, 1], 2),
                    1 => mk([2, 0], 1),
                    2 => mk([3, 0], 1),
                    _ => mk([0, 0], 0),
                }),
                n_terms: 3,
            }),
            public_len: 0,
            witness_len: 4,
            degree_bound: 2,
        }
    }

    /// In characteristic 2, `a*b + c + d = 0` means `d = a*b + c`.
    pub(crate) fn toy_witness(a: u128, b: u128, c: u128) -> WarpWitness<U4, U0> {
        let d = Galois128(a) * Galois128(b) + Galois128(c);
        WarpWitness {
            public: Array::<Galois128, U0>::default(),
            witness: Array::from_fn(|i| [Galois128(a), Galois128(b), Galois128(c), d][i]),
        }
    }

    /// The oracle's index with identical content, for differentials.
    pub(crate) fn oracle_index() -> volar_iop::warp::PesatIndex<volar_primitives::Galois128> {
        use volar_iop::warp::{PesatConstraint, PesatIndex, PesatTerm};
        let mk = |vars: Vec<usize>| PesatTerm {
            coeff: volar_primitives::Galois128(1),
            vars,
        };
        PesatIndex {
            constraints: vec![PesatConstraint {
                terms: vec![mk(vec![0, 1]), mk(vec![2]), mk(vec![3])],
            }],
            public_len: 0,
            witness_len: 4,
            degree_bound: 2,
        }
    }

    pub(crate) fn oracle_witness(
        w: &WarpWitness<U4, U0>,
    ) -> volar_iop::warp::PesatWitness<volar_primitives::Galois128> {
        volar_iop::warp::PesatWitness {
            public: Vec::new(),
            witness: w.witness.to_vec(),
        }
    }

    #[test]
    fn genesis_matches_the_oracle_byte_for_byte() {
        let index = toy_index();
        let witness = toy_witness(3, 5, 7);
        let acc = warp_genesis_spec::<_, _, _, _, _, U4, _, U7, U0>(
            &index,
            &WarpIdentityCode,
            &witness,
        )
        .expect("genesis succeeds");
        assert!(warp_decide_spec(&index, &WarpIdentityCode, &acc));

        let oracle = volar_iop::warp::warp_genesis(
            &oracle_index(),
            &volar_iop::warp::IdentityCode::new(4).unwrap(),
            &oracle_witness(&toy_witness(3, 5, 7)),
        )
        .unwrap();
        assert_eq!(
            acc.instance.root.as_slice(),
            oracle.instance.root.as_slice(),
            "genesis Merkle roots must match the oracle"
        );
        assert_eq!(acc.instance.mu, oracle.instance.mu);
        assert_eq!(acc.instance.eta, oracle.instance.eta);
        assert_eq!(acc.instance.beta.to_vec(), oracle.instance.beta.clone());
        assert_eq!(acc.instance.alpha.to_vec(), oracle.instance.alpha.clone());
    }

    #[test]
    fn unsatisfied_witness_is_rejected() {
        let index = toy_index();
        let mut witness = toy_witness(3, 5, 7);
        witness.witness[3] = witness.witness[3] + Galois128(1);
        assert!(
            warp_genesis_spec::<_, _, _, _, _, U4, _, U7, U0>(&index, &WarpIdentityCode, &witness)
                .is_none(),
            "unsatisfied witness must reject"
        );
    }

    #[test]
    fn decider_rejects_witness_that_does_not_encode_to_codeword() {
        let index = toy_index();
        let witness = toy_witness(17, 19, 23);
        let mut acc = warp_genesis_spec::<_, _, _, _, _, U4, _, U7, U0>(
            &index,
            &WarpIdentityCode,
            &witness,
        )
        .expect("genesis succeeds");
        acc.witness.witness[0] = acc.witness.witness[0] + Galois128(1);
        assert!(
            !warp_decide_spec(&index, &WarpIdentityCode, &acc),
            "decider must reject a witness that does not encode to the codeword"
        );
    }
}

#[cfg(test)]
mod prove_tests {
    use super::*;
    use super::tests::{oracle_index, oracle_witness, toy_index, toy_witness};
    use alloc::vec;
    use alloc::vec::Vec;
    use cipher::consts::*;

    /// Two-fresh accumulation: the spec prover must match the oracle
    /// byte-for-byte (roots, mus, both sumchecks, OOD answers, openings).
    #[test]
    fn prove_two_fresh_matches_the_oracle() {
        let index = toy_index();
        let fresh = Array::<WarpWitness<U4, U0>, U2>::from_fn(|i| {
            if i == 0 { toy_witness(3, 5, 7) } else { toy_witness(9, 11, 13) }
        });
        let accumulators =
            Array::<WarpAcc<U4, U4, U2, U0, U7>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        let (acc, proof) = warp_prove_spec::<
            U1, U4, U2, U4, U0, U4, U2, U0, U2, U1, U2, U4, U8, U2, U5, U7, U0, _
        >(&index, &WarpIdentityCode, &fresh, &accumulators)
        .expect("proving succeeds");
        assert!(warp_decide_spec(&index, &WarpIdentityCode, &acc));

        let ofresh: Vec<_> = fresh.iter().map(oracle_witness).collect();
        let (oacc, oproof) = volar_iop::warp::warp_prove(
            &oracle_index(),
            &volar_iop::warp::IdentityCode::new(4).unwrap(),
            volar_iop::warp::WarpSampling {
                log_codeword_len: 2,
                out_of_domain: 1,
                shift_queries: 2,
            },
            &ofresh,
            &[],
        )
        .unwrap();

        assert_eq!(acc.instance.root.as_slice(), oacc.instance.root.as_slice());
        assert_eq!(acc.instance.mu, oacc.instance.mu);
        assert_eq!(acc.instance.eta, oacc.instance.eta);
        assert_eq!(acc.instance.alpha.to_vec(), oacc.instance.alpha);
        assert_eq!(acc.instance.beta.to_vec(), oacc.instance.beta);
        assert_eq!(proof.initial_root.as_slice(), oproof.initial_root.as_slice());
        assert_eq!(proof.fresh_mus.to_vec(), oproof.fresh_mus);
        assert_eq!(proof.nu0, oproof.nu0);
        assert_eq!(proof.ood_answers.to_vec(), oproof.ood_answers);
        for (spec_poly, oracle_poly) in proof
            .first_sumcheck
            .iter()
            .zip(oproof.first_sumcheck.iter())
        {
            assert_eq!(
                spec_poly.evaluations[..spec_poly.count as usize].to_vec(),
                oracle_poly.evaluations,
                "first sumcheck polynomials must match the oracle"
            );
        }
        for (spec_poly, oracle_poly) in proof
            .second_sumcheck
            .iter()
            .zip(oproof.second_sumcheck.iter())
        {
            assert_eq!(
                spec_poly.evaluations[..spec_poly.count as usize].to_vec(),
                oracle_poly.evaluations,
                "second sumcheck polynomials must match the oracle"
            );
        }
        for (spec_row, oracle_row) in proof.openings.iter().zip(oproof.openings.iter()) {
            assert_eq!(spec_row.to_vec(), *oracle_row);
        }
        for (spec_path, oracle_path) in proof
            .initial_paths
            .iter()
            .zip(oproof.initial_paths.iter())
        {
            assert_eq!(
                spec_path.siblings.iter().map(|s| s.as_slice()).collect::<Vec<_>>(),
                oracle_path.siblings.iter().map(|s| s.as_slice()).collect::<Vec<_>>(),
            );
        }
    }

    /// Genesis-then-accumulate: a prior accumulator folds with a fresh
    /// statement, matching the oracle (FRESH=1, A=1, ELL=2).
    #[test]
    fn prove_with_prior_accumulator_matches_the_oracle() {
        let index = toy_index();
        let genesis = warp_genesis_spec::<_, _, _, _, _, U4, _, U7, U0>(
            &index,
            &WarpIdentityCode,
            &toy_witness(17, 19, 23),
        )
        .expect("genesis succeeds");
        let fresh = Array::<WarpWitness<U4, U0>, U1>::from_fn(|_| toy_witness(29, 31, 37));
        let accumulators = Array::<WarpAcc<U4, U4, U2, U0, U7>, U1>::from_fn(|_| genesis.clone());
        let (acc, _proof) = warp_prove_spec::<
            U1, U4, U2, U4, U0, U4, U1, U1, U2, U1, U2, U4, U8, U2, U5, U7, U0, _
        >(&index, &WarpIdentityCode, &fresh, &accumulators)
        .expect("proving succeeds");
        assert!(warp_decide_spec(&index, &WarpIdentityCode, &acc));

        let ogenesis = volar_iop::warp::warp_genesis(
            &oracle_index(),
            &volar_iop::warp::IdentityCode::new(4).unwrap(),
            &oracle_witness(&toy_witness(17, 19, 23)),
        )
        .unwrap();
        let ofresh = vec![oracle_witness(&toy_witness(29, 31, 37))];
        let (oacc, _oproof) = volar_iop::warp::warp_prove(
            &oracle_index(),
            &volar_iop::warp::IdentityCode::new(4).unwrap(),
            volar_iop::warp::WarpSampling {
                log_codeword_len: 2,
                out_of_domain: 1,
                shift_queries: 2,
            },
            &ofresh,
            &[ogenesis],
        )
        .unwrap();
        assert_eq!(acc.instance.root.as_slice(), oacc.instance.root.as_slice());
        assert_eq!(acc.instance.mu, oacc.instance.mu);
        assert_eq!(acc.instance.eta, oacc.instance.eta);
        assert_eq!(acc.instance.alpha.to_vec(), oacc.instance.alpha);
    }

    /// Repetition-code round trip (non-identity code), matching the oracle.
    #[test]
    fn prove_repetition_code_matches_the_oracle() {
        let index = toy_index();
        let fresh = Array::<WarpWitness<U4, U0>, U2>::from_fn(|i| {
            if i == 0 { toy_witness(41, 43, 47) } else { toy_witness(53, 59, 61) }
        });
        let accumulators = Array::<WarpAcc<U8, U4, U3, U0, U15>, U0>::from_fn(|_| {
            unreachable!("U0 is empty")
        });
        // N=8 (U8), LOGN=U3, NODES=U15; R=U4, TW=U2, BETA=U0, SC=U8.
        let (acc, _proof) = warp_prove_spec::<
            U1, U4, U2, U4, U0, U8, U2, U0, U2, U1, U2, U4, U8, U2, U6, U15, U0, _
        >(&index, &WarpRepetitionCode, &fresh, &accumulators)
        .expect("proving succeeds");
        assert!(warp_decide_spec(&index, &WarpRepetitionCode, &acc));

        let ofresh: Vec<_> = fresh.iter().map(oracle_witness).collect();
        let (oacc, _oproof) = volar_iop::warp::warp_prove(
            &oracle_index(),
            &volar_iop::warp::RepetitionCode::new(4, 2).unwrap(),
            volar_iop::warp::WarpSampling {
                log_codeword_len: 3,
                out_of_domain: 1,
                shift_queries: 2,
            },
            &ofresh,
            &[],
        )
        .unwrap();
        assert_eq!(acc.instance.root.as_slice(), oacc.instance.root.as_slice());
        assert_eq!(acc.instance.mu, oacc.instance.mu);
        assert_eq!(acc.instance.eta, oacc.instance.eta);
    }

    #[test]
    fn unsatisfied_fresh_witness_is_rejected_before_proving() {
        let index = toy_index();
        let mut bad = toy_witness(3, 5, 7);
        bad.witness[3] = bad.witness[3] + Galois128(1);
        let fresh = Array::<WarpWitness<U4, U0>, U2>::from_fn(|i| {
            if i == 0 { bad.clone() } else { toy_witness(9, 11, 13) }
        });
        let accumulators =
            Array::<WarpAcc<U4, U4, U2, U0, U7>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        assert!(
            warp_prove_spec::<U1, U4, U2, U4, U0, U4, U2, U0, U2, U1, U2, U4, U8, U2, U5, U7, U0, _>(
                &index,
                &WarpIdentityCode,
                &fresh,
                &accumulators,
            )
            .is_none(),
            "unsatisfied fresh witness must reject"
        );
    }
}

#[cfg(test)]
mod verify_tests {
    use super::*;
    use super::tests::{toy_index, toy_witness};
    use cipher::consts::*;

    /// The shared two-fresh shape: index, fresh inputs, empty accumulators.
    fn two_fresh() -> (
        WarpIndex<U1, U4, U2>,
        Array<WarpWitness<U4, U0>, U2>,
        WarpAcc<U4, U4, U2, U0, U7>,
        WarpProof<U2, U2, U0, U1, U2, U8, U2>,
    ) {
        let index = toy_index();
        let fresh = Array::<WarpWitness<U4, U0>, U2>::from_fn(|i| {
            if i == 0 { toy_witness(3, 5, 7) } else { toy_witness(9, 11, 13) }
        });
        let accumulators =
            Array::<WarpAcc<U4, U4, U2, U0, U7>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        let (acc, proof) = warp_prove_spec::<
            U1, U4, U2, U4, U0, U4, U2, U0, U2, U1, U2, U4, U8, U2, U5, U7, U0, _
        >(&index, &WarpIdentityCode, &fresh, &accumulators)
        .expect("proving succeeds");
        (index, fresh, acc, proof)
    }

    fn verify_two_fresh(
        index: &WarpIndex<U1, U4, U2>,
        acc: &WarpAcc<U4, U4, U2, U0, U7>,
        proof: &WarpProof<U2, U2, U0, U1, U2, U8, U2>,
    ) -> bool {
        let fresh_publics = Array::<Array<Galois128, U0>, U2>::default();
        let inputs = Array::<WarpAccInstance<U2, U0>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        warp_verify_spec::<U1, U4, U2, U0, U2, U0, U2, U1, U2, U4, U8, U2, U5, U4, U0>(
            index,
            &fresh_publics,
            &inputs,
            &acc.instance,
            proof,
        )
    }

    #[test]
    fn honest_two_fresh_verifies() {
        let (index, _fresh, acc, proof) = two_fresh();
        assert!(verify_two_fresh(&index, &acc, &proof));
        assert!(warp_decide_spec(&index, &WarpIdentityCode, &acc));
    }

    #[test]
    fn tampered_first_sumcheck_is_rejected() {
        let (index, _fresh, acc, mut proof) = two_fresh();
        proof.first_sumcheck[0].evaluations[0] =
            proof.first_sumcheck[0].evaluations[0] + Galois128(1);
        assert!(!verify_two_fresh(&index, &acc, &proof));
    }

    #[test]
    fn tampered_merkle_path_is_rejected() {
        let (index, _fresh, acc, mut proof) = two_fresh();
        proof.initial_paths[0].siblings[0][0] ^= 1;
        assert!(!verify_two_fresh(&index, &acc, &proof));
    }

    #[test]
    fn tampered_output_root_is_rejected() {
        let (index, _fresh, mut acc, proof) = two_fresh();
        acc.instance.root[0] ^= 1;
        assert!(!verify_two_fresh(&index, &acc, &proof));
    }

    #[test]
    fn tampered_output_alpha_is_rejected() {
        let (index, _fresh, mut acc, proof) = two_fresh();
        acc.instance.alpha[0] = acc.instance.alpha[0] + Galois128(1);
        assert!(!verify_two_fresh(&index, &acc, &proof));
    }
}

#[cfg(test)]
mod public_tests {
    use super::*;
    use alloc::vec;
    use alloc::vec::Vec;
    use cipher::consts::*;

    /// Two constraints over one public input: `x*w0 + w1 = 0` and
    /// `x + w2 + w3 = 0` — the oracle's `public_index` (M=2, so log_m=1 and
    /// the `tau` squeeze is exercised).
    fn public_index() -> WarpIndex<U2, U4, U2> {
        let mk = |vars: [usize; 2], n_vars: u32| WarpTerm::<U2> {
            coeff: Galois128(1),
            vars: Array::from_fn(|i| vars[i]),
            n_vars,
        };
        WarpIndex {
            constraints: Array::from_fn(|c| match c {
                0 => WarpConstraint::<U4, U2> {
                    terms: Array::from_fn(|i| match i {
                        0 => mk([0, 1], 2),
                        1 => mk([2, 0], 1),
                        _ => mk([0, 0], 0),
                    }),
                    n_terms: 2,
                },
                _ => WarpConstraint::<U4, U2> {
                    terms: Array::from_fn(|i| match i {
                        0 => mk([0, 0], 1),
                        1 => mk([3, 0], 1),
                        2 => mk([4, 0], 1),
                        _ => mk([0, 0], 0),
                    }),
                    n_terms: 3,
                },
            }),
            public_len: 1,
            witness_len: 4,
            degree_bound: 2,
        }
    }

    fn public_witness(x: u128, w0: u128, w2: u128) -> WarpWitness<U4, U1> {
        let (x, w0, w2) = (Galois128(x), Galois128(w0), Galois128(w2));
        let w1 = x * w0;
        let w3 = x + w2;
        WarpWitness {
            public: Array::from_fn(|_| x),
            witness: Array::from_fn(|i| [w0, w1, w2, w3][i]),
        }
    }

    #[test]
    fn public_inputs_and_multi_constraint_bundling_match_the_oracle() {
        let index = public_index();
        let fresh = Array::<WarpWitness<U4, U1>, U2>::from_fn(|i| {
            if i == 0 {
                public_witness(5, 7, 11)
            } else {
                public_witness(13, 17, 19)
            }
        });
        let accumulators =
            Array::<WarpAcc<U4, U4, U2, U2, U7>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        // BETA = public_len + log2(M) = 1 + 1 = 2.
        let (acc, proof) = warp_prove_spec::<
            U2, U4, U2, U4, U1, U4, U2, U0, U2, U1, U2, U4, U8, U2, U5, U7, U2, _
        >(&index, &WarpIdentityCode, &fresh, &accumulators)
        .expect("proving succeeds");
        assert!(warp_decide_spec(&index, &WarpIdentityCode, &acc));

        let fresh_publics =
            Array::<Array<Galois128, U1>, U2>::from_fn(|i| fresh[i].public.clone());
        let inputs =
            Array::<WarpAccInstance<U2, U2>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        assert!(warp_verify_spec::<U2, U4, U2, U1, U2, U0, U2, U1, U2, U4, U8, U2, U5, U4, U2>(
            &index,
            &fresh_publics,
            &inputs,
            &acc.instance,
            &proof,
        ));

        // Oracle differential.
        let oracle_index = {
            use volar_iop::warp::{PesatConstraint, PesatIndex, PesatTerm};
            let mk = |vars: Vec<usize>| PesatTerm {
                coeff: volar_primitives::Galois128(1),
                vars,
            };
            PesatIndex {
                constraints: vec![
                    PesatConstraint {
                        terms: vec![mk(vec![0, 1]), mk(vec![2])],
                    },
                    PesatConstraint {
                        terms: vec![mk(vec![0]), mk(vec![3]), mk(vec![4])],
                    },
                ],
                public_len: 1,
                witness_len: 4,
                degree_bound: 2,
            }
        };
        let ofresh: Vec<_> = fresh
            .iter()
            .map(|w| volar_iop::warp::PesatWitness {
                public: w.public.to_vec(),
                witness: w.witness.to_vec(),
            })
            .collect();
        let (oacc, _oproof) = volar_iop::warp::warp_prove(
            &oracle_index,
            &volar_iop::warp::IdentityCode::new(4).unwrap(),
            volar_iop::warp::WarpSampling {
                log_codeword_len: 2,
                out_of_domain: 1,
                shift_queries: 2,
            },
            &ofresh,
            &[],
        )
        .unwrap();
        assert_eq!(acc.instance.root.as_slice(), oacc.instance.root.as_slice());
        assert_eq!(acc.instance.mu, oacc.instance.mu);
        assert_eq!(acc.instance.eta, oacc.instance.eta);
        assert_eq!(acc.instance.beta.to_vec(), oacc.instance.beta);

        // Wrong public input must reject.
        let wrong_publics = Array::<Array<Galois128, U1>, U2>::from_fn(|i| {
            if i == 0 {
                Array::from_fn(|_| Galois128(23))
            } else {
                fresh[i].public.clone()
            }
        });
        assert!(
            !warp_verify_spec::<U2, U4, U2, U1, U2, U0, U2, U1, U2, U4, U8, U2, U5, U4, U2>(
                &index,
                &wrong_publics,
                &inputs,
                &acc.instance,
                &proof,
            ),
            "changing a public input must change the transcript and reject"
        );
    }
}
