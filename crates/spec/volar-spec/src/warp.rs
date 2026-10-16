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
