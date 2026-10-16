// @reliability: experimental
//! @ai: assisted
//! WARP accumulation (Bünz–Chiesa–Fenzi–Wang, *Linear-Time Accumulation
//! Schemes*, ePrint 2025/753, Construction 10.4) in the linkable spec
//! subset, over the **VOLE field** `GF(2^128)` (`x^128 + x^7 + x^2 + x + 1`,
//! [`Galois128`]).
//!
//! This module is the single-source-of-truth port of
//! `volar-iop`'s `warp.rs` oracle: same transcript byte schedule, same
//! domain separators, same Merkle layout — the differential tests in
//! `volar-lir-codegen`/`volar-iop` pin byte-for-byte agreement over
//! `Galois128` (the §5.4 field decision in `volar-ir`'s
//! `docs/warp-vole-verifier-zksnark-plan.md`). Differences from the oracle
//! are representational only:
//!
//! - concrete `Galois128` instead of a generic `Field` (the tower stays an
//!   oracle-side test fixture);
//! - `Array<T, N>` typenum sizing instead of `Vec` (the spec subset bans
//!   `Vec`; see `volar-compiler-passes`' `vec_lint`);
//! - capacity-plus-count bounded vectors (`WarpTerm.vars`,
//!   `WarpConstraint.terms`, `WarpUni.evals`) with fail-closed count checks
//!   instead of growable vectors;
//! - Merkle trees restricted to power-of-two leaf counts (every WARP use —
//!   `n` codeword positions — is a power of two; the oracle's pad-by-
//!   duplicate path is unreachable here);
//! - the `Log2<N>` type-level logarithm (`typenum::Logarithm2`) sizes
//!   evaluation points, with runtime power-of-two checks at every boundary.
//!
//! Like the oracle, this is a construction-faithful algebraic reference: the
//! bundled `IdentityCode`/`RepetitionCode` fixtures carry no proximity
//! soundness claim, and parameter selection against the paper's bounds is a
//! separate reviewed step.

use cipher::typenum::{Logarithm2, Unsigned};

use super::*;
use crate::field::Galois128;
use digest::{ExtendableOutput, FixedOutput, Update, XofReader};


/// Type-level `log2` for the power-of-two size parameters WARP uses.
pub type Log2<N> = <N as Logarithm2>::Output;

/// A 32-byte SHA3-256 digest (the dialect's native array form).
pub type WarpDigest = Array<u8, cipher::consts::U32>;

/// Errors returned by the spec WARP construction — mirroring the oracle's
/// `WarpError` variants one-for-one.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WarpError {
    /// The PESAT index is malformed or exceeds its degree bound.
    MalformedIndex,
    /// An input, witness, codeword, proof, or accumulator has the wrong shape.
    ShapeMismatch,
    /// A dimension that the paper requires to be a power of two is not.
    NonPowerOfTwo,
    /// A fresh PESAT witness does not satisfy a constraint (constraint index).
    UnsatisfiedPesat(u32),
    /// A sumcheck link or final target check failed.
    SumcheckMismatch,
    /// A Merkle path did not verify against the bound root.
    MerkleMismatch,
    /// The accumulator witness does not match its public commitment.
    CommitmentMismatch,
}

pub type WarpResult<T> = Result<T, WarpError>;

pub(crate) fn is_pow2(n: usize) -> bool {
    n != 0 && n.is_power_of_two()
}

pub(crate) fn log2_pow2(n: usize) -> u32 {
    n.trailing_zeros()
}

// ============================================================================
// Transcript (W2) — SHAKE128, byte-identical to the oracle's IopTranscript
// schedule (domain separator, absorb ordering, length prefixes, squeeze
// grouping).
// ============================================================================

/// The Construction 10.4 transcript domain — identical to the oracle's.
pub const WARP_TRANSCRIPT_DOMAIN: &[u8] = b"volar-iop/warp/eprint-2025-753/construction-10.4/v1";

/// A running SHAKE128 sponge with the oracle's absorb/squeeze semantics.
#[derive(Clone)]
pub struct WarpTranscript {
    sponge: sha3::Shake128,
}

impl Default for WarpTranscript {
    fn default() -> Self {
        Self::new()
    }
}

impl WarpTranscript {
    pub fn new() -> Self {
        WarpTranscript {
            sponge: sha3::Shake128::default(),
        }
    }

    /// Absorb a domain-separation label (once, first).
    pub fn domain_sep(mut self, label: &[u8]) -> Self {
        Update::update(&mut self.sponge, label);
        self
    }

    pub fn absorb(&mut self, data: &[u8]) {
        Update::update(&mut self.sponge, data);
    }

    /// Length-prefixed absorb (`u64` LE length, then bytes) — the oracle's
    /// `absorb_len_prefixed`.
    pub fn absorb_len_prefixed(&mut self, data: &[u8]) {
        self.absorb(&(data.len() as u64).to_le_bytes());
        self.absorb(data);
    }

    pub fn absorb_usize(&mut self, value: usize) {
        self.absorb(&(value as u64).to_le_bytes());
    }

    /// One field element as its canonical 16-byte LE encoding,
    /// length-prefixed — the oracle's `absorb_field`.
    pub fn absorb_field(&mut self, value: Galois128) {
        self.absorb_len_prefixed(&value.0.to_le_bytes());
    }

    /// Count-then-elements absorb — the oracle's `absorb_field_vec`.
    pub fn absorb_fields<N: ArraySize>(&mut self, values: &Array<Galois128, N>) {
        self.absorb_usize(N::USIZE);
        for value in values.iter() {
            self.absorb_field(*value);
        }
    }

    /// Count-then-elements absorb over a slice — for capacity-plus-count
    /// vectors (sumcheck polynomials) where the absorbed length is the
    /// *count*, not the capacity. The oracle's `absorb_polynomial`.
    pub fn absorb_field_slice(&mut self, values: &[Galois128]) {
        self.absorb_usize(values.len());
        for value in values {
            self.absorb_field(*value);
        }
    }

    /// Squeeze `N` field elements from ONE XOF stream (16 LE bytes each) —
    /// byte-identical to the oracle's single `squeeze_bytes(16N)` + chunking
    /// (never N independent squeezes: each squeeze must advance one stream).
    pub fn squeeze_fields<N: ArraySize>(&self) -> Array<Galois128, N> {
        let mut reader = self.sponge.clone().finalize_xof();
        Array::from_fn(|_| {
            let mut bytes = [0u8; 16];
            reader.read(&mut bytes);
            Galois128(u128::from_le_bytes(bytes))
        })
    }

    /// Squeeze an `R × C` grid of field elements from ONE XOF stream —
    /// byte-identical to the oracle's single `squeeze_bytes(R*C*16)` +
    /// chunking (used for the Construction 5.10 `tau_i` and the
    /// Construction 7.2 out-of-domain points).
    pub fn squeeze_field_grid<R: ArraySize, C: ArraySize>(
        &self,
    ) -> Array<Array<Galois128, C>, R> {
        let mut reader = self.sponge.clone().finalize_xof();
        Array::from_fn(|_| {
            Array::from_fn(|_| {
                let mut bytes = [0u8; 16];
                reader.read(&mut bytes);
                Galois128(u128::from_le_bytes(bytes))
            })
        })
    }

    /// Construction 7.2's combined shift-index + `xi` squeeze: one stream of
    /// `SHIFTS * index_bytes + LOGR * 16` bytes, indices first — the
    /// oracle's `squeeze_shift_and_xi`.
    pub fn squeeze_shift_and_xi<SHIFTS: ArraySize, LOGR: ArraySize>(
        &self,
        log_domain: u32,
    ) -> (Array<u32, SHIFTS>, Array<Galois128, LOGR>) {
        let index_bytes = ((log_domain as usize) + 7) / 8;
        let mut reader = self.sponge.clone().finalize_xof();
        let shifts = Array::<u32, SHIFTS>::from_fn(|_| {
            let mut value = 0usize;
            for position in 0..index_bytes.max(1) {
                let mut byte = [0u8; 1];
                reader.read(&mut byte);
                value |= (byte[0] as usize) << (8 * position);
            }
            (value & ((1usize << log_domain) - 1)) as u32
        });
        let xi = Array::<Galois128, LOGR>::from_fn(|_| {
            let mut bytes = [0u8; 16];
            reader.read(&mut bytes);
            Galois128(u128::from_le_bytes(bytes))
        });
        (shifts, xi)
    }
}

// ============================================================================
// Merkle (W2) — SHA3-256, domain-separated leaf/node hashing, power-of-two
// leaf counts only (see module doc).
// ============================================================================

pub(crate) fn warp_hash_leaf(parts: &[&[u8]]) -> WarpDigest {
    let mut h = sha3::Sha3_256::new();
    Digest::update(&mut h, [0x00]);
    for part in parts {
        Digest::update(&mut h, part);
    }
    h.finalize_fixed()
}

pub(crate) fn warp_hash_node(left: &WarpDigest, right: &WarpDigest) -> WarpDigest {
    let mut h = sha3::Sha3_256::new();
    Digest::update(&mut h, [0x01]);
    Digest::update(&mut h, left);
    Digest::update(&mut h, right);
    h.finalize_fixed()
}

/// A committed binary Merkle tree, nodes in level order (leaves first, root
/// last). `leaves` must be a power of two and `NODES == 2 * leaves - 1`.
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

impl<NODES: ArraySize> WarpMerkleTree<NODES> {
    /// Commit to pre-hashed leaves in level order.
    pub fn commit_leaves<LEAVES: ArraySize>(
        leaves: &Array<WarpDigest, LEAVES>,
    ) -> WarpResult<Self> {
        if !is_pow2(LEAVES::USIZE) || NODES::USIZE != 2 * LEAVES::USIZE - 1 {
            return Err(WarpError::ShapeMismatch);
        }
        let mut nodes = Array::<WarpDigest, NODES>::default();
        for i in 0..LEAVES::USIZE {
            nodes[i] = leaves[i].clone();
        }
        // Level i starts at offset LEAVES + LEAVES/2 + ... + LEAVES/2^(i-1).
        let mut level_start = 0usize;
        let mut level_len = LEAVES::USIZE;
        while level_len > 1 {
            let next_start = level_start + level_len;
            for j in 0..(level_len / 2) {
                nodes[next_start + j] =
                    warp_hash_node(&nodes[level_start + 2 * j], &nodes[level_start + 2 * j + 1]);
            }
            level_start = next_start;
            level_len /= 2;
        }
        Ok(WarpMerkleTree {
            leaves: LEAVES::USIZE,
            nodes,
        })
    }

    pub fn root(&self) -> WarpDigest {
        self.nodes[NODES::USIZE - 1].clone()
    }

    /// Open leaf `index` — siblings bottom-up. `LOG2` must equal
    /// `log2(self.leaves)`.
    pub fn open<LOG2: ArraySize>(&self, index: usize) -> WarpAuthPath<LOG2> {
        assert!(
            index < self.leaves && LOG2::USIZE == log2_pow2(self.leaves) as usize,
            "WarpMerkleTree::open: index/log shape mismatch"
        );
        let mut siblings = Array::<WarpDigest, LOG2>::default();
        let mut idx = index;
        let mut level_start = 0usize;
        let mut level_len = self.leaves;
        let mut depth = 0usize;
        while level_len > 1 {
            siblings[depth] = self.nodes[level_start + (idx ^ 1)].clone();
            idx /= 2;
            level_start += level_len;
            level_len /= 2;
            depth += 1;
        }
        WarpAuthPath { siblings }
    }
}

/// Verify that `leaf` (pre-hashed parts) is committed at `index` under
/// `root` given `path`.
pub fn warp_merkle_verify<LOG2_LEAVES: ArraySize>(
    root: &WarpDigest,
    leaf_parts: &[&[u8]],
    index: usize,
    path: &WarpAuthPath<LOG2_LEAVES>,
) -> bool {
    let mut cur = warp_hash_leaf(leaf_parts);
    let mut idx = index;
    for sibling in path.siblings.iter() {
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
) -> WarpResult<WarpMerkleTree<NODES>>
where
    N: ArraySize,
    NODES: ArraySize,
{
    let leaves = Array::<WarpDigest, N>::from_fn(|i| {
        warp_hash_leaf(&[&codeword[i].0.to_le_bytes()])
    });
    WarpMerkleTree::commit_leaves(&leaves)
}

/// Commit to stacked fresh codewords: leaf `i` is the hash of the
/// concatenation of element `i` of every fresh codeword, in fresh order
/// (the oracle's `codeword_leaves`).
pub fn warp_commit_stacked<N, FRESH, NODES>(
    codewords: &Array<Array<Galois128, N>, FRESH>,
) -> WarpResult<WarpMerkleTree<NODES>>
where
    N: ArraySize,
    FRESH: ArraySize,
    NODES: ArraySize,
{
    // Streaming absorb is byte-identical to hashing the concatenated parts.
    let leaves = Array::<WarpDigest, N>::from_fn(|i| {
        let mut h = sha3::Sha3_256::new();
        Digest::update(&mut h, [0x00]);
        for codeword in codewords.iter() {
            Digest::update(&mut h, codeword[i].0.to_le_bytes());
        }
        h.finalize_fixed()
    });
    WarpMerkleTree::commit_leaves(&leaves)
}

// ============================================================================
// Multilinear helpers (shared by prover, verifier, decider)
// ============================================================================

/// `eq(point, b)` for every Boolean `b`, little-endian — the oracle's
/// `build_eq_evals`. `OUT` must equal `2^POINTS`.
pub fn warp_eq_evals<POINTS: ArraySize, OUT: ArraySize>(
    point: &Array<Galois128, POINTS>,
) -> WarpResult<Array<Galois128, OUT>> {
    if OUT::USIZE != (1usize << POINTS::USIZE) {
        return Err(WarpError::ShapeMismatch);
    }
    let mut out = Array::<Galois128, OUT>::default();
    out[0] = Galois128(1);
    let mut len = 1usize;
    for coordinate in point.iter() {
        for index in 0..len {
            let product = out[index] * *coordinate;
            // characteristic 2: subtraction is addition (XOR).
            out[index] = out[index] - product;
            out[index + len] = product;
        }
        len <<= 1;
    }
    Ok(out)
}

/// Scalar equality polynomial `eq(a, b)` — the oracle's `eq_scalar`.
pub fn warp_eq_scalar<L: ArraySize>(a: &Array<Galois128, L>, b: &Array<Galois128, L>) -> Galois128 {
    let mut out = Galois128(1);
    for (left, right) in a.iter().zip(b.iter()) {
        let same = *left * *right;
        let different = (Galois128(1) - *left) * (Galois128(1) - *right);
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
pub fn warp_mle_eval<N, LOGN>(
    values: &Array<Galois128, N>,
    point: &Array<Galois128, LOGN>,
) -> WarpResult<Galois128>
where
    N: ArraySize + Logarithm2,
    LOGN: ArraySize,
    Log2<N>: ArraySize,
{
    if !is_pow2(N::USIZE) || LOGN::USIZE != log2_pow2(N::USIZE) as usize {
        return Err(WarpError::ShapeMismatch);
    }
    let eq = warp_eq_evals::<LOGN, N>(point)?;
    let mut out = Galois128(0);
    for (value, weight) in values.iter().zip(eq.iter()) {
        out = out + *value * *weight;
    }
    Ok(out)
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

impl<DEG: ArraySize> WarpTerm<DEG> {
    fn degree(&self) -> usize {
        self.n_vars as usize
    }

    /// Evaluate at the assignment (public, witness) split — variable `v`
    /// reads `public[v]` when `v < X`, else `witness[v - X]`.
    fn evaluate<X: ArraySize, K: ArraySize>(
        &self,
        public: &Array<Galois128, X>,
        witness: &Array<Galois128, K>,
    ) -> Galois128 {
        let mut out = self.coeff;
        for i in 0..self.n_vars as usize {
            let var = self.vars[i];
            let value = if var < X::USIZE {
                public[var]
            } else {
                witness[var - X::USIZE]
            };
            out = out * value;
        }
        out
    }
}

impl<TERMS: ArraySize, DEG: ArraySize> WarpConstraint<TERMS, DEG> {
    fn degree(&self) -> usize {
        let mut out = 0usize;
        for i in 0..self.n_terms as usize {
            let d = self.terms[i].degree();
            if d > out {
                out = d;
            }
        }
        out
    }

    fn evaluate<X: ArraySize, K: ArraySize>(
        &self,
        public: &Array<Galois128, X>,
        witness: &Array<Galois128, K>,
    ) -> Galois128 {
        let mut out = Galois128(0);
        for i in 0..self.n_terms as usize {
            out = out + self.terms[i].evaluate(public, witness);
        }
        out
    }
}

impl<M: ArraySize, TERMS: ArraySize, DEG: ArraySize> WarpIndex<M, TERMS, DEG> {
    pub fn variable_count(&self) -> usize {
        self.public_len + self.witness_len
    }

    pub fn log_constraints(&self) -> usize {
        log2_pow2(M::USIZE) as usize
    }

    pub fn validate(&self) -> WarpResult<()> {
        if !is_pow2(M::USIZE) {
            return Err(WarpError::NonPowerOfTwo);
        }
        for constraint in self.constraints.iter() {
            if constraint.n_terms as usize > TERMS::USIZE {
                return Err(WarpError::MalformedIndex);
            }
            if constraint.degree() > self.degree_bound {
                return Err(WarpError::MalformedIndex);
            }
            for i in 0..constraint.n_terms as usize {
                let term = &constraint.terms[i];
                if term.n_vars as usize > DEG::USIZE {
                    return Err(WarpError::MalformedIndex);
                }
                for j in 0..term.n_vars as usize {
                    if term.vars[j] >= self.variable_count() {
                        return Err(WarpError::MalformedIndex);
                    }
                }
            }
        }
        Ok(())
    }

    /// Evaluate the bundled polynomial `P_hat(beta, w)` from Definition 5.5.
    ///
    /// `beta` is `x || tau` (Construction 10.4 step 2(f)): the first
    /// `public_len` entries are the public input, the rest `tau` (length
    /// `log M`).
    pub fn evaluate_bundled<K: ArraySize, BETA: ArraySize>(
        &self,
        beta: &Array<Galois128, BETA>,
        witness: &Array<Galois128, K>,
    ) -> WarpResult<Galois128> {
        let log_m = self.log_constraints();
        if BETA::USIZE != self.public_len + log_m || K::USIZE != self.witness_len {
            return Err(WarpError::ShapeMismatch);
        }
        // eq evals over tau (beta's tail, length log M), indexed by
        // constraint — the same doubling loop as `warp_eq_evals`, written
        // out because `tau` is a runtime tail of `beta`, not an Array.
        let mut eq = Array::<Galois128, M>::default();
        eq[0] = Galois128(1);
        let mut len = 1usize;
        for j in 0..log_m {
            let coordinate = beta[self.public_len + j];
            for index in 0..len {
                let product = eq[index] * coordinate;
                eq[index] = eq[index] - product;
                eq[index + len] = product;
            }
            len <<= 1;
        }
        let mut out = Galois128(0);
        for (index, constraint) in self.constraints.iter().enumerate() {
            // Public part of beta is beta[..public_len]; read through the
            // split helper below.
            let value = {
                let mut acc = Galois128(0);
                for i in 0..constraint.n_terms as usize {
                    let term = &constraint.terms[i];
                    let mut product = term.coeff;
                    for v in 0..term.n_vars as usize {
                        let var = term.vars[v];
                        let x = if var < self.public_len {
                            beta[var]
                        } else {
                            witness[var - self.public_len]
                        };
                        product = product * x;
                    }
                    acc = acc + product;
                }
                acc
            };
            out = out + eq[index] * value;
        }
        Ok(out)
    }

    /// Check ordinary PESAT satisfaction for a fresh prover input.
    pub fn check_instance<K: ArraySize, X: ArraySize>(
        &self,
        instance: &WarpWitness<K, X>,
    ) -> WarpResult<()> {
        if X::USIZE != self.public_len || K::USIZE != self.witness_len {
            return Err(WarpError::ShapeMismatch);
        }
        for (index, constraint) in self.constraints.iter().enumerate() {
            if constraint.evaluate(&instance.public, &instance.witness) != Galois128(0) {
                return Err(WarpError::UnsatisfiedPesat(index as u32));
            }
        }
        Ok(())
    }

    /// Absorb the index into the transcript — byte-identical to the
    /// oracle's `absorb_index`.
    pub fn absorb_into(&self, transcript: &mut WarpTranscript) {
        transcript.absorb(b"index");
        transcript.absorb_usize(M::USIZE);
        transcript.absorb_usize(self.public_len);
        transcript.absorb_usize(self.witness_len);
        transcript.absorb_usize(self.degree_bound);
        for constraint in self.constraints.iter() {
            transcript.absorb_usize(constraint.n_terms as usize);
            for i in 0..constraint.n_terms as usize {
                let term = &constraint.terms[i];
                transcript.absorb_field(term.coeff);
                transcript.absorb_usize(term.n_vars as usize);
                for j in 0..term.n_vars as usize {
                    transcript.absorb_usize(term.vars[j]);
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

/// A systematic repetition code `w -> (w, ..., w)` — `REPS` copies.
#[derive(Clone, Copy, Debug, Default)]
pub struct WarpRepetitionCode;

impl<K: ArraySize, N: ArraySize> WarpCode<K, N> for WarpRepetitionCode {
    fn encode(&self, message: &Array<Galois128, K>) -> Array<Galois128, N> {
        Array::from_fn(|i| message[i % K::USIZE])
    }
}

// ============================================================================
// Sumcheck univariates — evaluations at 0..=degree (the oracle's
// WarpUnivariate), capacity-plus-count.
// ============================================================================

/// A univariate polynomial as its evaluations at `0, 1, ..., count - 1`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpUni<CAP: ArraySize> {
    pub evaluations: Array<Galois128, CAP>,
    pub count: u32,
}

impl<CAP: ArraySize> WarpUni<CAP> {
    pub fn from_evals(evals: &[Galois128]) -> WarpResult<Self> {
        if evals.len() > CAP::USIZE {
            return Err(WarpError::ShapeMismatch);
        }
        let mut out = Array::<Galois128, CAP>::default();
        for (i, value) in evals.iter().enumerate() {
            out[i] = *value;
        }
        Ok(WarpUni {
            evaluations: out,
            count: evals.len() as u32,
        })
    }

    pub fn at_zero(&self) -> Galois128 {
        self.evaluations[0]
    }

    pub fn at_one(&self) -> Galois128 {
        self.evaluations[1]
    }

    /// Lagrange evaluation at an arbitrary field point — the oracle's
    /// `WarpUnivariate::evaluate` (O(d^2), evaluation form).
    pub fn evaluate(&self, x: Galois128) -> Galois128 {
        let n = self.count as usize;
        let mut out = Galois128(0);
        for i in 0..n {
            let mut numerator = Galois128(1);
            let mut denominator = Galois128(1);
            for j in 0..n {
                if i == j {
                    continue;
                }
                numerator = numerator * (x - Galois128(j as u128));
                denominator = denominator * (Galois128(i as u128) - Galois128(j as u128));
            }
            out = out + self.evaluations[i] * numerator * warp_inv(denominator);
        }
        out
    }
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
    Log2<ELL>: ArraySize,
{
    /// Root of the stacked fresh-witness Merkle commitment.
    pub initial_root: WarpDigest,
    /// Fresh-instance evaluations at `alpha = 0^{log n}`.
    pub fresh_mus: Array<Galois128, FRESH>,
    /// The `log ell` twin-constraint pseudo-batching polynomials.
    pub first_sumcheck: Array<WarpUni<SC>, Log2<ELL>>,
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
    transcript.absorb(b"acc-instance");
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
    let mut transcript = WarpTranscript::new().domain_sep(WARP_TRANSCRIPT_DOMAIN);
    index.absorb_into(&mut transcript);
    transcript.absorb_usize(FRESH::USIZE);
    for public in fresh_publics.iter() {
        transcript.absorb(b"fresh-public");
        transcript.absorb_fields(public);
    }
    transcript.absorb_usize(A::USIZE);
    for accumulator in accumulators.iter() {
        warp_absorb_instance(&mut transcript, accumulator);
    }
    transcript
}

/// Construct a genesis accumulator for one fresh PESAT statement — the
/// oracle's `warp_genesis` (Construction 5.10 with deterministic
/// `tau = 0^{log M}`).
pub fn warp_genesis_spec<M, TERMS, DEG, K, X, N, C, NODES, BETA>(
    index: &WarpIndex<M, TERMS, DEG>,
    code: &C,
    instance: &WarpWitness<K, X>,
) -> WarpResult<WarpAcc<N, K, Log2<N>, BETA, NODES>>
where
    M: ArraySize,
    TERMS: ArraySize,
    DEG: ArraySize,
    K: ArraySize,
    X: ArraySize,
    N: ArraySize + Logarithm2,
    Log2<N>: ArraySize,
    C: WarpCode<K, N>,
    NODES: ArraySize,
    BETA: ArraySize,
{
    index.validate()?;
    if !is_pow2(N::USIZE) {
        return Err(WarpError::NonPowerOfTwo);
    }
    index.check_instance(instance)?;
    if BETA::USIZE != index.public_len + index.log_constraints() {
        return Err(WarpError::ShapeMismatch);
    }
    let codeword = code.encode(&instance.witness);
    let tree = warp_commit_codeword::<N, NODES>(&codeword)?;
    let alpha = Array::<Galois128, Log2<N>>::default();
    let mut beta = Array::<Galois128, BETA>::default();
    for i in 0..X::USIZE {
        beta[i] = instance.public[i];
    }
    let mu = warp_mle_eval(&codeword, &alpha)?;
    let eta = index.evaluate_bundled(&beta, &instance.witness)?;
    Ok(WarpAcc {
        instance: WarpAccInstance {
            root: tree.root(),
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
/// `P_hat(beta, w) = eta`.
pub fn warp_decide_spec<M, TERMS, DEG, N, K, LOGN, BETA, NODES, C>(
    index: &WarpIndex<M, TERMS, DEG>,
    code: &C,
    accumulator: &WarpAcc<N, K, LOGN, BETA, NODES>,
) -> WarpResult<()>
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
    Log2<N>: ArraySize,
{
    index.validate()?;
    if LOGN::USIZE != log2_pow2(N::USIZE) as usize
        || BETA::USIZE != index.public_len + index.log_constraints()
        || K::USIZE != index.witness_len
        || accumulator.witness.tree.root() != accumulator.instance.root
    {
        return Err(WarpError::ShapeMismatch);
    }
    let recomputed = warp_commit_codeword::<N, NODES>(&accumulator.witness.codeword)?;
    if recomputed.root() != accumulator.instance.root {
        return Err(WarpError::CommitmentMismatch);
    }
    if code.encode(&accumulator.witness.witness) != accumulator.witness.codeword {
        return Err(WarpError::CommitmentMismatch);
    }
    if warp_mle_eval(&accumulator.witness.codeword, &accumulator.instance.alpha)?
        != accumulator.instance.mu
    {
        return Err(WarpError::CommitmentMismatch);
    }
    if index.evaluate_bundled(&accumulator.instance.beta, &accumulator.witness.witness)?
        != accumulator.instance.eta
    {
        return Err(WarpError::CommitmentMismatch);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{vec, vec::Vec};
    use cipher::consts::*;

    /// The toy PESAT `w0*w1 + w2 + w3 = 0` (degree 2, one constraint) shared
    /// with the oracle's `toy_index`.
    fn toy_index() -> WarpIndex<U1, U4, U2> {
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
    fn toy_witness(a: u128, b: u128, c: u128) -> WarpWitness<U4, U0> {
        let d = Galois128(a) * Galois128(b) + Galois128(c);
        WarpWitness {
            public: Array::default(),
            witness: Array::from_fn(|i| [Galois128(a), Galois128(b), Galois128(c), d][i]),
        }
    }

    /// The oracle's index with identical content, for differentials.
    fn oracle_index() -> volar_iop::warp::PesatIndex<volar_primitives::Galois128> {
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

    fn oracle_witness(a: u128, b: u128, c: u128) -> volar_iop::warp::PesatWitness<volar_primitives::Galois128> {
        let spec = toy_witness(a, b, c);
        volar_iop::warp::PesatWitness {
            public: Vec::new(),
            witness: spec.witness.to_vec(),
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
        .unwrap();
        warp_decide_spec(&index, &WarpIdentityCode, &acc).unwrap();

        let oracle = volar_iop::warp::warp_genesis(
            &oracle_index(),
            &volar_iop::warp::IdentityCode::new(4).unwrap(),
            &oracle_witness(3, 5, 7),
        )
        .unwrap();
        assert_eq!(
            acc.instance.root.as_slice(),
            oracle.instance.root.as_slice(),
            "genesis Merkle roots must match the oracle"
        );
        assert_eq!(acc.instance.mu, oracle.instance.mu);
        assert_eq!(acc.instance.eta, oracle.instance.eta);
        assert_eq!(
            acc.instance.beta.to_vec(),
            oracle.instance.beta.clone()
        );
        assert_eq!(acc.instance.alpha.to_vec(), oracle.instance.alpha.clone());
    }

    #[test]
    fn unsatisfied_witness_is_rejected() {
        let index = toy_index();
        let mut witness = toy_witness(3, 5, 7);
        witness.witness[3] = witness.witness[3] + Galois128(1);
        assert_eq!(
            warp_genesis_spec::<_, _, _, _, _, U4, _, U7, U0>(&index, &WarpIdentityCode, &witness)
                .unwrap_err(),
            WarpError::UnsatisfiedPesat(0)
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
        .unwrap();
        acc.witness.witness[0] = acc.witness.witness[0] + Galois128(1);
        assert_eq!(
            warp_decide_spec(&index, &WarpIdentityCode, &acc),
            Err(WarpError::CommitmentMismatch)
        );
    }
}

// ============================================================================
// The prover (W4) — Construction 10.4's P_ACC.
// ============================================================================

/// `eq`-weighted combination of `ELL` vectors by `point` (`log ELL`
/// coordinates) — the oracle's `combine_vectors`.
pub fn warp_combine_vectors<ELL, LOGL, LEN>(
    items: &Array<Array<Galois128, LEN>, ELL>,
    point: &Array<Galois128, LOGL>,
) -> WarpResult<Array<Galois128, LEN>>
where
    ELL: ArraySize,
    LOGL: ArraySize,
    LEN: ArraySize,
{
    let weights = warp_eq_evals::<LOGL, ELL>(point)?;
    let mut out = Array::<Galois128, LEN>::default();
    for (item, weight) in items.iter().zip(weights.iter()) {
        for (dst, value) in out.iter_mut().zip(item.iter()) {
            *dst = *dst + *value * *weight;
        }
    }
    Ok(out)
}

/// `prefix || [current] || binary(suffix)` — the oracle's `point_with_suffix`.
fn warp_point_with_suffix<LOG: ArraySize>(
    prefix: &[Galois128],
    current: Galois128,
    suffix: usize,
    suffix_len: usize,
) -> Array<Galois128, LOG> {
    Array::from_fn(|i| {
        if i < prefix.len() {
            prefix[i]
        } else if i == prefix.len() {
            current
        } else {
            let bit = i - prefix.len() - 1;
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
) -> WarpResult<Galois128>
where
    M: ArraySize,
    TERMS: ArraySize,
    DEG: ArraySize,
    N: ArraySize + Logarithm2,
    K: ArraySize,
    LOGN: ArraySize,
    BETA: ArraySize,
    Log2<N>: ArraySize,
{
    let evaluation = warp_mle_eval(codeword, alpha)?;
    let bundled = index.evaluate_bundled(beta, witness)?;
    Ok(evaluation + omega * bundled)
}

/// The Construction 8.2 eq-star sum — the oracle's `eq_star_at`.
fn warp_eq_star_at<R, LOGR, LOGN>(
    claim_points: &Array<Array<Galois128, LOGN>, R>,
    xi: &Array<Galois128, LOGR>,
    point: &Array<Galois128, LOGN>,
) -> WarpResult<Galois128>
where
    R: ArraySize,
    LOGR: ArraySize,
    LOGN: ArraySize,
{
    if !is_pow2(R::USIZE) || LOGR::USIZE != log2_pow2(R::USIZE) as usize {
        return Err(WarpError::ShapeMismatch);
    }
    let mut out = Galois128(0);
    for (index, claim_point) in claim_points.iter().enumerate() {
        let claim_index = warp_binary_point::<LOGR>(index);
        let weight = warp_eq_scalar(xi, &claim_index) * warp_eq_scalar(claim_point, point);
        out = out + weight;
    }
    Ok(out)
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
    prefix: &[Galois128],
    x: Galois128,
) -> WarpResult<Galois128>
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
    Log2<N>: ArraySize,
{
    let log_ell = log2_pow2(ELL::USIZE) as usize;
    let round = prefix.len();
    let suffix_len = log_ell - round - 1;
    let mut out = Galois128(0);
    for suffix in 0..(1usize << suffix_len) {
        let point = warp_point_with_suffix::<LOGL>(prefix, x, suffix, suffix_len);
        let codeword = warp_combine_vectors(codewords, &point)?;
        let witness = warp_combine_vectors(witnesses, &point)?;
        let alpha = warp_combine_vectors(alphas, &point)?;
        let beta = warp_combine_vectors(betas, &point)?;
        let g = warp_evaluate_g(index, &codeword, &witness, &alpha, &beta, omega)?;
        out = out + warp_eq_scalar(tau, &point) * g;
    }
    Ok(out)
}

/// One second-sumcheck evaluation at `x` — the oracle's
/// `second_sumcheck_eval`.
fn warp_second_sumcheck_eval<N, R, LOGR, LOGN>(
    codeword: &Array<Galois128, N>,
    claim_points: &Array<Array<Galois128, LOGN>, R>,
    xi: &Array<Galois128, LOGR>,
    prefix: &[Galois128],
    x: Galois128,
) -> WarpResult<Galois128>
where
    N: ArraySize + Logarithm2,
    R: ArraySize,
    LOGR: ArraySize,
    LOGN: ArraySize,
    Log2<N>: ArraySize,
{
    let log_n = log2_pow2(N::USIZE) as usize;
    let round = prefix.len();
    let suffix_len = log_n - round - 1;
    let mut out = Galois128(0);
    for suffix in 0..(1usize << suffix_len) {
        let point = warp_point_with_suffix::<LOGN>(prefix, x, suffix, suffix_len);
        let eq = warp_eq_star_at(claim_points, xi, &point)?;
        out = out + eq * warp_mle_eval(codeword, &point)?;
    }
    Ok(out)
}

/// The sumcheck link/target check — the oracle's `check_sumcheck`.
pub fn warp_check_sumcheck<SC: ArraySize>(
    polys: &[WarpUni<SC>],
    initial_claim: Galois128,
    challenges: &[Galois128],
    final_claim: Galois128,
) -> bool {
    if polys.len() != challenges.len() {
        return false;
    }
    if polys.is_empty() {
        return initial_claim == final_claim;
    }
    if polys[0].at_zero() + polys[0].at_one() != initial_claim {
        return false;
    }
    for round in 1..polys.len() {
        let expected = polys[round - 1].evaluate(challenges[round - 1]);
        if polys[round].at_zero() + polys[round].at_one() != expected {
            return false;
        }
    }
    match polys.last() {
        Some(poly) => poly.evaluate(challenges[challenges.len() - 1]) == final_claim,
        None => false,
    }
}

/// Prove Construction 10.4 for fresh PESAT inputs plus prior accumulators.
///
/// Shape parameters are carried in types; every derived relation
/// (`ELL = FRESH + A`, `R = 1 + S + SHIFTS`, `TW = log2(ELL) + 1`,
/// `BETA = public_len + log2(M)`, `NODES = 2N - 1`, power-of-two `N`/`M`/
/// `ELL`/`R`) is checked fail-closed at entry. The first-sumcheck degree is
/// `1 + max(log n + 1, log M + degree_bound)` and must fit `SC - 1`.
#[allow(clippy::too_many_arguments, clippy::type_complexity)]
pub fn warp_prove_spec<M, TERMS, DEG, K, X, N, FRESH, A, ELL, S, SHIFTS, R, SC, TW, NODES, BETA, C>(
    index: &WarpIndex<M, TERMS, DEG>,
    code: &C,
    fresh: &Array<WarpWitness<K, X>, FRESH>,
    accumulators: &Array<WarpAcc<N, K, Log2<N>, BETA, NODES>, A>,
) -> WarpResult<(
    WarpAcc<N, K, Log2<N>, BETA, NODES>,
    WarpProof<FRESH, ELL, A, S, SHIFTS, SC, Log2<N>>,
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
    NODES: ArraySize,
    BETA: ArraySize,
    C: WarpCode<K, N>,
    Log2<M>: ArraySize,
    Log2<N>: ArraySize,
    Log2<ELL>: ArraySize,
    Log2<R>: ArraySize,
{
    // ---- shape validation (the oracle's validate_input_shapes) ----
    index.validate()?;
    let log_n = log2_pow2(N::USIZE) as usize;
    let log_m = index.log_constraints();
    let log_ell = log2_pow2(ELL::USIZE) as usize;
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
        return Err(WarpError::ShapeMismatch);
    }
    let first_degree = 1 + (log_n + 1).max(log_m + index.degree_bound);
    if first_degree + 1 > SC::USIZE || 3 > SC::USIZE {
        return Err(WarpError::ShapeMismatch);
    }
    for instance in fresh.iter() {
        index.check_instance(instance)?;
    }
    for accumulator in accumulators.iter() {
        if accumulator.witness.tree.root() != accumulator.instance.root {
            return Err(WarpError::CommitmentMismatch);
        }
    }

    // ---- transcript prefix ----
    let fresh_publics = Array::<Array<Galois128, X>, FRESH>::from_fn(|i| fresh[i].public.clone());
    let acc_instances =
        Array::<WarpAccInstance<Log2<N>, BETA>, A>::from_fn(|i| accumulators[i].instance.clone());
    let mut transcript = warp_transcript_for(index, &fresh_publics, &acc_instances);

    // ---- Construction 5.10 + Remark 5.9: encode, commit, zerocheck ----
    let fresh_codewords =
        Array::<Array<Galois128, N>, FRESH>::from_fn(|i| code.encode(&fresh[i].witness));
    let initial_tree = warp_commit_stacked::<N, FRESH, NODES>(&fresh_codewords)?;
    let initial_root = initial_tree.root();
    transcript.absorb_len_prefixed(&initial_root);

    let taus: Array<Array<Galois128, Log2<M>>, FRESH> = transcript.squeeze_field_grid();
    let zero_point_n = Array::<Galois128, Log2<N>>::default();
    let fresh_mus = Array::<Galois128, FRESH>::from_fn(|i| {
        warp_mle_eval(&fresh_codewords[i], &zero_point_n)
            .expect("fresh codeword MLE at the zero point")
    });

    let mut alphas = Array::<Array<Galois128, Log2<N>>, ELL>::default();
    let mut mus = Array::<Galois128, ELL>::default();
    let mut betas = Array::<Array<Galois128, BETA>, ELL>::default();
    let mut etas = Array::<Galois128, ELL>::default();
    let mut codewords = Array::<Array<Galois128, N>, ELL>::default();
    let mut witnesses = Array::<Array<Galois128, K>, ELL>::default();
    for i in 0..FRESH::USIZE {
        alphas[i] = Array::default();
        mus[i] = fresh_mus[i];
        let mut beta = Array::<Galois128, BETA>::default();
        for j in 0..X::USIZE {
            beta[j] = fresh[i].public[j];
        }
        for j in 0..log_m {
            beta[X::USIZE + j] = taus[i][j];
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
    let tau = Array::<Galois128, Log2<ELL>>::from_fn(|i| tau_omega[i]);
    let omega = tau_omega[TW::USIZE - 1];
    let sigma1 = {
        let eq_tau = warp_eq_evals::<Log2<ELL>, ELL>(&tau)?;
        let mut out = Galois128(0);
        for ((mu, eta), weight) in mus.iter().zip(etas.iter()).zip(eq_tau.iter()) {
            out = out + *weight * (*mu + omega * *eta);
        }
        out
    };

    let mut first_sumcheck = Array::<WarpUni<SC>, Log2<ELL>>::from_fn(|_| WarpUni {
        evaluations: Array::default(),
        count: 0,
    });
    let mut gamma = Array::<Galois128, Log2<ELL>>::default();
    for round in 0..log_ell {
        let mut evals = Array::<Galois128, SC>::default();
        for x in 0..=first_degree {
            evals[x] = warp_first_sumcheck_eval(
                index,
                &codewords,
                &witnesses,
                &alphas,
                &betas,
                &tau,
                omega,
                &gamma[..round],
                Galois128(x as u128),
            )?;
        }
        let poly = WarpUni::<SC> {
            evaluations: evals,
            count: (first_degree + 1) as u32,
        };
        transcript.absorb_field_slice(&poly.evaluations[..poly.count as usize]);
        gamma[round] = transcript.squeeze_fields::<cipher::consts::U1>()[0];
        first_sumcheck[round] = poly;
    }
    let zeta0 = warp_combine_vectors(&alphas, &gamma)?;
    let beta = warp_combine_vectors(&betas, &gamma)?;
    let folded_codeword = warp_combine_vectors(&codewords, &gamma)?;
    let folded_witness = warp_combine_vectors(&witnesses, &gamma)?;
    let nu0 = warp_mle_eval(&folded_codeword, &zeta0)?;
    let eta = index.evaluate_bundled(&beta, &folded_witness)?;
    if !warp_check_sumcheck(first_sumcheck.as_slice(), sigma1, &gamma, {
        warp_eq_scalar(&tau, &gamma) * (nu0 + omega * eta)
    }) {
        return Err(WarpError::SumcheckMismatch);
    }

    // ---- Construction 7.2: commit, OOD samples, shift queries ----
    let output_tree = warp_commit_codeword::<N, NODES>(&folded_codeword)?;
    let output_root = output_tree.root();
    transcript.absorb_len_prefixed(&output_root);
    transcript.absorb_field(nu0);
    transcript.absorb_field(eta);
    let ood_points: Array<Array<Galois128, Log2<N>>, S> = transcript.squeeze_field_grid();
    let ood_answers = Array::<Galois128, S>::from_fn(|i| {
        warp_mle_eval(&folded_codeword, &ood_points[i]).expect("OOD MLE")
    });
    transcript.absorb_fields(&ood_answers);

    let (shift_indices, xi): (Array<u32, SHIFTS>, Array<Galois128, Log2<R>>) =
        transcript.squeeze_shift_and_xi(log_n as u32);
    let mut claim_points = Array::<Array<Galois128, Log2<N>>, R>::default();
    let mut claim_values = Array::<Galois128, R>::default();
    claim_points[0] = zeta0.clone();
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
        let eq_xi = warp_eq_evals::<Log2<R>, R>(&xi)?;
        let mut out = Galois128(0);
        for (value, weight) in claim_values.iter().zip(eq_xi.iter()) {
            out = out + *value * *weight;
        }
        out
    };

    // ---- Construction 8.2: batch the claims into one ----
    let mut second_sumcheck = Array::<WarpUni<SC>, Log2<N>>::from_fn(|_| WarpUni {
        evaluations: Array::default(),
        count: 0,
    });
    let mut alpha = Array::<Galois128, Log2<N>>::default();
    for round in 0..log_n {
        let mut evals = Array::<Galois128, SC>::default();
        for x in 0..=2usize {
            evals[x] = warp_second_sumcheck_eval(
                &folded_codeword,
                &claim_points,
                &xi,
                &alpha[..round],
                Galois128(x as u128),
            )?;
        }
        let poly = WarpUni::<SC> {
            evaluations: evals,
            count: 3,
        };
        transcript.absorb_field_slice(&poly.evaluations[..poly.count as usize]);
        alpha[round] = transcript.squeeze_fields::<cipher::consts::U1>()[0];
        second_sumcheck[round] = poly;
    }
    let mu = warp_mle_eval(&folded_codeword, &alpha)?;
    if !warp_check_sumcheck(second_sumcheck.as_slice(), sigma2, &alpha, {
        let eq = warp_eq_star_at(&claim_points, &xi, &alpha)?;
        mu * eq
    }) {
        return Err(WarpError::SumcheckMismatch);
    }

    // ---- openings ----
    let initial_paths = Array::<WarpAuthPath<Log2<N>>, SHIFTS>::from_fn(|i| {
        initial_tree.open(shift_indices[i] as usize)
    });
    let accumulator_paths = Array::<Array<WarpAuthPath<Log2<N>>, SHIFTS>, A>::from_fn(|i| {
        Array::<WarpAuthPath<Log2<N>>, SHIFTS>::from_fn(|j| {
            accumulators[i].witness.tree.open(shift_indices[j] as usize)
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
    Ok((accumulator, proof))
}

#[cfg(test)]
mod prove_tests {
    use super::*;
    use alloc::vec;
    use alloc::vec::Vec;
    use cipher::consts::*;

    fn toy_index() -> WarpIndex<U1, U4, U2> {
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

    fn toy_witness(a: u128, b: u128, c: u128) -> WarpWitness<U4, U0> {
        let d = Galois128(a) * Galois128(b) + Galois128(c);
        WarpWitness {
            public: Array::default(),
            witness: Array::from_fn(|i| [Galois128(a), Galois128(b), Galois128(c), d][i]),
        }
    }

    fn oracle_index() -> volar_iop::warp::PesatIndex<volar_primitives::Galois128> {
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

    fn oracle_witness(w: &WarpWitness<U4, U0>) -> volar_iop::warp::PesatWitness<volar_primitives::Galois128> {
        volar_iop::warp::PesatWitness {
            public: Vec::new(),
            witness: w.witness.to_vec(),
        }
    }

    /// Two-fresh accumulation: the spec prover must match the oracle
    /// byte-for-byte (roots, mus, both sumchecks, OOD answers, openings).
    #[test]
    fn prove_two_fresh_matches_the_oracle() {
        let index = toy_index();
        let fresh = Array::<WarpWitness<U4, U0>, U2>::from_fn(|i| {
            if i == 0 { toy_witness(3, 5, 7) } else { toy_witness(9, 11, 13) }
        });
        let accumulators = Array::<WarpAcc<U4, U4, U2, U0, U7>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        let (acc, proof) = warp_prove_spec::<
            U1, U4, U2, U4, U0, U4, U2, U0, U2, U1, U2, U4, U8, U2, U7, U0, _
        >(&index, &WarpIdentityCode, &fresh, &accumulators)
        .unwrap();
        warp_decide_spec(&index, &WarpIdentityCode, &acc).unwrap();

        let oacc = {
            let ofresh: Vec<_> = fresh.iter().map(oracle_witness).collect();
            volar_iop::warp::warp_prove(
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
            .unwrap()
        };
        let (oacc, oproof) = oacc;

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
        .unwrap();
        let fresh = Array::<WarpWitness<U4, U0>, U1>::from_fn(|_| toy_witness(29, 31, 37));
        let accumulators = Array::<WarpAcc<U4, U4, U2, U0, U7>, U1>::from_fn(|_| genesis.clone());
        let (acc, _proof) = warp_prove_spec::<
            U1, U4, U2, U4, U0, U4, U1, U1, U2, U1, U2, U4, U8, U2, U7, U0, _
        >(&index, &WarpIdentityCode, &fresh, &accumulators)
        .unwrap();
        warp_decide_spec(&index, &WarpIdentityCode, &acc).unwrap();

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
        let accumulators = Array::<WarpAcc<U8, U4, U3, U0, U15>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        // N=8 (U8), LOGN=U3, NODES=U15; R=U4, TW=U2, BETA=U0, SC=U8.
        let (acc, _proof) = warp_prove_spec::<
            U1, U4, U2, U4, U0, U8, U2, U0, U2, U1, U2, U4, U8, U2, U15, U0, _
        >(&index, &WarpRepetitionCode, &fresh, &accumulators)
        .unwrap();
        warp_decide_spec(&index, &WarpRepetitionCode, &acc).unwrap();

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
        let accumulators = Array::<WarpAcc<U4, U4, U2, U0, U7>, U0>::from_fn(|_| unreachable!("U0 is empty"));
        assert_eq!(
            warp_prove_spec::<U1, U4, U2, U4, U0, U4, U2, U0, U2, U1, U2, U4, U8, U2, U7, U0, _>(
                &index,
                &WarpIdentityCode,
                &fresh,
                &accumulators,
            )
            .unwrap_err(),
            WarpError::UnsatisfiedPesat(0)
        );
    }
}
