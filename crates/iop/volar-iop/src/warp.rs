// @reliability: experimental
//! @ai: assisted
//! Paper-pinned WARP accumulation over Volar's binary tower fields.
//!
//! This module implements the Construction 10.4 accumulation scheme from
//! Bünz–Chiesa–Fenzi–Wang, *Linear-Time Accumulation Schemes*, ePrint
//! 2025/753 (archived in `docs/research/papers/2025-753.pdf`). The protocol
//! is composed from the paper's Constructions 5.10, 6.3, 7.2, and 8.2:
//!
//! 1. encode fresh PESAT witnesses with a linear code;
//! 2. batch the resulting twin-constrained instances with the `log ell`
//!    multilinear sumcheck;
//! 3. commit to the batched codeword, add out-of-domain and shift-query
//!    evaluation claims, and authenticate the queried inputs with Merkle
//!    paths;
//! 4. batch those claims with the `log n` quadratic sumcheck; and
//! 5. decide the final accumulator by checking the Merkle commitment, the
//!    multilinear claim, the bundled PESAT claim, and `f = C(w)`.
//!
//! The implementation is deliberately a construction-faithful reference over
//! [`Field`]/[`LinearCode`]. It favors exact statement shape and reviewable
//! algebra over the paper's asymptotic table-maintenance optimizations. A
//! production deployment still needs a reviewed linear-code/parameter choice
//! satisfying the paper's field-size, distance, and mutual-correlated-
//! agreement bounds.
//!
//! **Transcript pinning note.** Construction 5.10's prose writes
//! `beta := (tau, x)`, while Construction 10.4 step 2(f) explicitly uses
//! `beta_i := (x_i, tau_i)`. This implementation follows Construction 10.4's
//! `x || tau` order.

use alloc::vec;
use alloc::vec::Vec;

use crate::field::Field;
use crate::merkle::{AuthPath, Digest32, MerkleTree, verify as merkle_verify};
use crate::transcript::{FromBytes, IopTranscript};

const TRANSCRIPT_DOMAIN: &[u8] = b"volar-iop/warp/eprint-2025-753/construction-10.4/v1";

/// Errors returned by the WARP construction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WarpError {
    /// The PESAT index is malformed or exceeds its degree bound.
    MalformedIndex,
    /// An input, witness, codeword, proof, or accumulator has the wrong shape.
    ShapeMismatch,
    /// A dimension that the paper requires to be a power of two is not.
    NonPowerOfTwo,
    /// A fresh PESAT witness does not satisfy every constraint.
    UnsatisfiedPesat { constraint: usize },
    /// A sumcheck link or final target check failed.
    SumcheckMismatch,
    /// A Merkle path did not verify against the bound root.
    MerkleMismatch,
    /// The accumulator witness does not match its public commitment.
    CommitmentMismatch,
}

pub type WarpResult<T> = Result<T, WarpError>;

/// A linear code `C: F^k -> F^n`.
///
/// WARP only needs encoding on the honest prover/decider path. Extraction in
/// the paper additionally requires erasure correction, but no extractor is
/// implemented here.
pub trait LinearCode<F: Field> {
    /// Message length `k`.
    fn message_len(&self) -> usize;
    /// Codeword length `n`. It must be a power of two for MLE indexing.
    fn codeword_len(&self) -> usize;
    /// Encode a message of length `k`.
    fn encode(&self, message: &[F]) -> Vec<F>;
}

/// The identity code, useful as a test/reference code. It is not a proximity
/// code parameter choice and carries no production soundness claim by itself.
#[derive(Clone, Debug)]
pub struct IdentityCode {
    len: usize,
}

impl IdentityCode {
    pub fn new(len: usize) -> WarpResult<Self> {
        if !len.is_power_of_two() {
            return Err(WarpError::NonPowerOfTwo);
        }
        Ok(Self { len })
    }
}

impl<F: Field> LinearCode<F> for IdentityCode {
    fn message_len(&self) -> usize {
        self.len
    }

    fn codeword_len(&self) -> usize {
        self.len
    }

    fn encode(&self, message: &[F]) -> Vec<F> {
        assert_eq!(message.len(), self.len);
        message.to_vec()
    }
}

/// A systematic repetition code `w -> (w, ..., w)`.
///
/// This is useful as a non-identity linear-code test fixture when both `k`
/// and the repetition count are powers of two. Parameter review remains the
/// caller's responsibility.
#[derive(Clone, Debug)]
pub struct RepetitionCode {
    message_len: usize,
    repetitions: usize,
}

impl RepetitionCode {
    pub fn new(message_len: usize, repetitions: usize) -> WarpResult<Self> {
        if !message_len.is_power_of_two() || !repetitions.is_power_of_two() {
            return Err(WarpError::NonPowerOfTwo);
        }
        Ok(Self {
            message_len,
            repetitions,
        })
    }
}

impl<F: Field> LinearCode<F> for RepetitionCode {
    fn message_len(&self) -> usize {
        self.message_len
    }

    fn codeword_len(&self) -> usize {
        self.message_len * self.repetitions
    }

    fn encode(&self, message: &[F]) -> Vec<F> {
        assert_eq!(message.len(), self.message_len);
        let mut out = Vec::with_capacity(self.message_len * self.repetitions);
        for _ in 0..self.repetitions {
            out.extend_from_slice(message);
        }
        out
    }
}

/// A Reed-Solomon code `[n, k]` over `F`: the message `[m_0..m_{k-1}]` is the
/// coefficient vector of `p(X) = sum_i m_i X^i`, and the codeword is the
/// evaluation `[p(0), p(1), ..., p(n-1)]` at `n` distinct points (the integer
/// representatives `0..n` via [`Field::from_u128`]).
///
/// This is the paper's named **larger-distance** alternative to the
/// linear-time Spielman code (ePrint 2025/753, "Choice of parameters"): RS is
/// MDS, so its minimum distance is `d = n - k + 1` and its relative distance
/// is `(n-k+1)/n ≈ 1 - rate`. The proximity parameter `δ` (Theorem 9.1) can be
/// set up to the unique-decoding radius `(1 - rate)/2` with **no**
/// out-of-domain samples (`s = 0`), giving the query count
/// `t ≈ λ / -log2(1 - δ)`:
///
/// | rate | `δ` (unique-decoding) | `t` for λ=100 | `t` for λ=128 |
/// |---|---|---|---|
/// | 1/2 | 1/4 | 241 | 308 |
/// | 1/4 | 3/8 | 171 | 219 |
/// | 1/8 | 7/16 | 147 | 189 |
///
/// (The list-decoding / correlated-agreement regime reaches larger `δ` —
/// `1 - sqrt(rate)` — at the cost of `s > 0` out-of-domain samples; that is
/// *not* modelled here.)
///
/// Unlike [`IdentityCode`]/[`RepetitionCode`] (test fixtures with `δ -> 0`),
/// this is a real proximity code: the `t` query count yields the claimed
/// soundness. Encoding is `O(n·k)` here (naive Horner) — the honest-prover /
/// decider path only; production would use an additive FFT (`O(n log n)`).
#[derive(Clone, Debug)]
pub struct ReedSolomonCode {
    message_len: usize,
    codeword_len: usize,
}

impl ReedSolomonCode {
    /// Build an `[n, k]` RS code. Both `k` and `n` must be powers of two (the
    /// codeword length must be a power of two for the MLE indexing in WARP),
    /// with `n > k` (a non-trivial rate).
    pub fn new(message_len: usize, codeword_len: usize) -> WarpResult<Self> {
        if !message_len.is_power_of_two() || !codeword_len.is_power_of_two() {
            return Err(WarpError::NonPowerOfTwo);
        }
        if codeword_len <= message_len {
            return Err(WarpError::ShapeMismatch);
        }
        Ok(Self {
            message_len,
            codeword_len,
        })
    }

    /// The rate `k / n`.
    pub fn rate(&self) -> f64 {
        self.message_len as f64 / self.codeword_len as f64
    }

    /// The unique-decoding proximity radius `(1 - rate) / 2` — the largest
    /// `δ` that needs no out-of-domain samples.
    pub fn unique_decoding_delta(&self) -> f64 {
        (1.0 - self.rate()) / 2.0
    }
}

impl<F: Field> LinearCode<F> for ReedSolomonCode {
    fn message_len(&self) -> usize {
        self.message_len
    }

    fn codeword_len(&self) -> usize {
        self.codeword_len
    }

    fn encode(&self, message: &[F]) -> Vec<F> {
        assert_eq!(message.len(), self.message_len);
        (0..self.codeword_len)
            .map(|j| {
                let point = F::from_u128(j as u128);
                // Horner's method: p(point) = sum_i message[i] * point^i.
                let mut acc = F::ZERO;
                for i in (0..self.message_len).rev() {
                    acc = acc.mul(&point).add(&message[i]);
                }
                acc
            })
            .collect()
    }
}

/// One term `coeff * prod_{var in vars} z_var` in a PESAT constraint.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PesatTerm<F: Field> {
    pub coeff: F,
    pub vars: Vec<usize>,
}

impl<F: Field> PesatTerm<F> {
    fn degree(&self) -> usize {
        self.vars.len()
    }

    fn evaluate(&self, z: &[F]) -> F {
        let mut out = self.coeff;
        for &var in &self.vars {
            out = out.mul(&z[var]);
        }
        out
    }
}

/// One sparse polynomial constraint `p_hat_j(Z) = sum terms`.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PesatConstraint<F: Field> {
    pub terms: Vec<PesatTerm<F>>,
}

impl<F: Field> PesatConstraint<F> {
    fn degree(&self) -> usize {
        self.terms.iter().map(PesatTerm::degree).max().unwrap_or(0)
    }

    fn evaluate(&self, z: &[F]) -> F {
        let mut out = F::ZERO;
        for term in &self.terms {
            out = out.add(&term.evaluate(z));
        }
        out
    }
}

/// The PESAT index `i = (p_hat, M, N, k)` with a global degree bound.
#[derive(Clone, Debug)]
pub struct PesatIndex<F: Field> {
    pub constraints: Vec<PesatConstraint<F>>,
    pub public_len: usize,
    pub witness_len: usize,
    pub degree_bound: usize,
}

impl<F: Field> PesatIndex<F> {
    pub fn constraint_count(&self) -> usize {
        self.constraints.len()
    }

    pub fn variable_count(&self) -> usize {
        self.public_len + self.witness_len
    }

    pub fn validate(&self) -> WarpResult<()> {
        if !self.constraint_count().is_power_of_two() {
            return Err(WarpError::NonPowerOfTwo);
        }
        for constraint in &self.constraints {
            if constraint.degree() > self.degree_bound {
                return Err(WarpError::MalformedIndex);
            }
            for term in &constraint.terms {
                if term.vars.iter().any(|&var| var >= self.variable_count()) {
                    return Err(WarpError::MalformedIndex);
                }
            }
        }
        Ok(())
    }

    fn log_constraints(&self) -> usize {
        self.constraint_count().trailing_zeros() as usize
    }

    /// Evaluate the bundled polynomial `P_hat(beta, w)` from Definition 5.5.
    ///
    /// `beta` is `x || tau` as in Construction 10.4 step 2(f), where `tau`
    /// has length `log M`.
    pub fn evaluate_bundled(&self, beta: &[F], witness: &[F]) -> WarpResult<F> {
        let log_m = self.log_constraints();
        if beta.len() != self.public_len + log_m || witness.len() != self.witness_len {
            return Err(WarpError::ShapeMismatch);
        }
        let public = &beta[..self.public_len];
        let tau = &beta[self.public_len..];
        let mut z = Vec::with_capacity(self.variable_count());
        z.extend_from_slice(public);
        z.extend_from_slice(witness);
        let eq = build_eq_evals(tau);
        let mut out = F::ZERO;
        for (index, constraint) in self.constraints.iter().enumerate() {
            out = out.add(&eq[index].mul(&constraint.evaluate(&z)));
        }
        Ok(out)
    }

    /// Check ordinary PESAT satisfaction for a fresh prover input.
    pub fn check_instance(&self, instance: &PesatWitness<F>) -> WarpResult<()> {
        if instance.public.len() != self.public_len || instance.witness.len() != self.witness_len {
            return Err(WarpError::ShapeMismatch);
        }
        let mut z = Vec::with_capacity(self.variable_count());
        z.extend_from_slice(&instance.public);
        z.extend_from_slice(&instance.witness);
        for (index, constraint) in self.constraints.iter().enumerate() {
            if !constraint.evaluate(&z).is_zero() {
                return Err(WarpError::UnsatisfiedPesat { constraint: index });
            }
        }
        Ok(())
    }
}

/// A fresh PESAT statement: public part `x` plus prover witness `w`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PesatWitness<F: Field> {
    pub public: Vec<F>,
    pub witness: Vec<F>,
}

/// The public/instance half of a WARP accumulator.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpAccumulatorInstance<F: Field> {
    /// Merkle root over the codeword.
    pub root: Digest32,
    /// Final multilinear-evaluation point.
    pub alpha: Vec<F>,
    /// Claimed `f_hat(alpha)`.
    pub mu: F,
    /// Final bundled-PESAT evaluation point (`x || tau`).
    pub beta: Vec<F>,
    /// Claimed bundled PESAT value.
    pub eta: F,
}

/// The prover/decider half of a WARP accumulator.
#[derive(Clone, Debug)]
pub struct WarpAccumulatorWitness<F: Field> {
    /// Merkle tree committing to `codeword`.
    pub tree: MerkleTree,
    /// The codeword `f`.
    pub codeword: Vec<F>,
    /// The message/witness `w` with `f = C(w)`.
    pub witness: Vec<F>,
}

/// A split WARP accumulator.
#[derive(Clone, Debug)]
pub struct WarpAccumulator<F: Field> {
    pub instance: WarpAccumulatorInstance<F>,
    pub witness: WarpAccumulatorWitness<F>,
}

/// A sumcheck polynomial sent as its evaluations at `0..=degree`.
///
/// This is an informationally complete representation of the polynomial and
/// matches the proof shape used by the paper's Fiat–Shamir compilation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpUnivariate<F: Field> {
    pub evaluations: Vec<F>,
}

impl<F: Field + FromBytes> WarpUnivariate<F> {
    fn at_zero(&self) -> F {
        self.evaluations[0]
    }

    fn at_one(&self) -> F {
        self.evaluations[1]
    }

    fn evaluate(&self, x: F) -> F {
        let mut out = F::ZERO;
        for (i, value) in self.evaluations.iter().enumerate() {
            let mut numerator = F::ONE;
            let mut denominator = F::ONE;
            for (j, _) in self.evaluations.iter().enumerate() {
                if i == j {
                    continue;
                }
                numerator = numerator.mul(&x.sub(&F::from_u64(j as u64)));
                denominator = denominator.mul(&F::from_u64(i as u64).sub(&F::from_u64(j as u64)));
            }
            out = out.add(&value.mul(&numerator).mul(&denominator.inv()));
        }
        out
    }
}

/// Fiat–Shamir proof emitted by [`warp_prove`].
#[derive(Clone, Debug)]
pub struct WarpProof<F: Field> {
    /// Root of the stacked fresh-witness Merkle commitment.
    pub initial_root: Digest32,
    /// Fresh-instance evaluations at `alpha = 0^{log n}`.
    pub fresh_mus: Vec<F>,
    /// The `log ell` twin-constraint pseudo-batching polynomials.
    pub first_sumcheck: Vec<WarpUnivariate<F>>,
    /// Claimed evaluation of the folded codeword at `A_hat(gamma)`.
    pub nu0: F,
    /// Out-of-domain answers for the folded codeword.
    pub ood_answers: Vec<F>,
    /// The `log n` multilinear-claim batching polynomials.
    pub second_sumcheck: Vec<WarpUnivariate<F>>,
    /// One authentication path per shift query for the fresh commitment.
    pub initial_paths: Vec<AuthPath>,
    /// One path list per input accumulator, one path per shift query.
    pub accumulator_paths: Vec<Vec<AuthPath>>,
    /// Query-major opened values: `openings[query][input]`.
    pub openings: Vec<Vec<F>>,
}

/// Public sampling shape for Constructions 7.2/9.4.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WarpSampling {
    /// Public `log2` of the linear code's codeword length. This belongs to
    /// the verification key: the verifier must not infer `log n` from a
    /// prover-controlled output accumulator.
    pub log_codeword_len: usize,
    /// Number `s` of out-of-domain MLE samples.
    pub out_of_domain: usize,
    /// Number `t` of in-domain shift queries.
    pub shift_queries: usize,
}

impl WarpSampling {
    fn validate(&self) -> WarpResult<()> {
        if self.log_codeword_len >= usize::BITS as usize {
            return Err(WarpError::ShapeMismatch);
        }
        let claim_count = 1 + self.out_of_domain + self.shift_queries;
        if !claim_count.is_power_of_two() {
            return Err(WarpError::NonPowerOfTwo);
        }
        Ok(())
    }

    fn claim_count(&self) -> usize {
        1 + self.out_of_domain + self.shift_queries
    }
}

/// Build `eq(point, b)` for every Boolean `b`, in little-endian order.
pub fn build_eq_evals<F: Field>(point: &[F]) -> Vec<F> {
    let mut out = vec![F::ONE];
    let mut len = 1;
    for coordinate in point {
        for index in 0..len {
            let product = out[index].mul(coordinate);
            out[index] = out[index].sub(&product);
            out.push(product);
        }
        len <<= 1;
    }
    out
}

/// Scalar equality polynomial `eq(a, b)`.
pub fn eq_scalar<F: Field>(a: &[F], b: &[F]) -> F {
    assert_eq!(a.len(), b.len());
    let mut out = F::ONE;
    for (left, right) in a.iter().zip(b) {
        let same = left.mul(right);
        let different = F::ONE.sub(left).mul(&F::ONE.sub(right));
        out = out.mul(&same.add(&different));
    }
    out
}

/// Little-endian Boolean embedding of `index` in `F^log_len`.
pub fn binary_point<F: Field + FromBytes>(index: usize, log_len: usize) -> Vec<F> {
    (0..log_len)
        .map(|bit| {
            if (index >> bit) & 1 == 1 {
                F::ONE
            } else {
                F::ZERO
            }
        })
        .collect()
}

/// Evaluate the multilinear extension of a power-of-two-length table.
pub fn mle_eval<F: Field>(values: &[F], point: &[F]) -> F {
    assert!(values.len().is_power_of_two());
    assert_eq!(point.len(), values.len().trailing_zeros() as usize);
    let eq = build_eq_evals(point);
    let mut out = F::ZERO;
    for (value, weight) in values.iter().zip(eq) {
        out = out.add(&value.mul(&weight));
    }
    out
}

fn combine_vectors<F: Field>(items: &[Vec<F>], point: &[F]) -> WarpResult<Vec<F>> {
    if items.is_empty() || !items.len().is_power_of_two() {
        return Err(WarpError::NonPowerOfTwo);
    }
    if point.len() != items.len().trailing_zeros() as usize {
        return Err(WarpError::ShapeMismatch);
    }
    let len = items[0].len();
    if items.iter().any(|item| item.len() != len) {
        return Err(WarpError::ShapeMismatch);
    }
    let weights = build_eq_evals(point);
    let mut out = vec![F::ZERO; len];
    for (item, weight) in items.iter().zip(weights) {
        for (dst, value) in out.iter_mut().zip(item) {
            *dst = dst.add(&value.mul(&weight));
        }
    }
    Ok(out)
}

fn absorb_usize(transcript: &mut IopTranscript, value: usize) {
    transcript.absorb(&(value as u64).to_le_bytes());
}

fn absorb_field<F: Field>(transcript: &mut IopTranscript, value: &F) {
    transcript.absorb_len_prefixed(&value.to_bytes());
}

fn absorb_field_vec<F: Field>(transcript: &mut IopTranscript, values: &[F]) {
    absorb_usize(transcript, values.len());
    for value in values {
        absorb_field(transcript, value);
    }
}

fn absorb_polynomial<F: Field>(transcript: &mut IopTranscript, poly: &WarpUnivariate<F>) {
    absorb_field_vec(transcript, &poly.evaluations);
}

fn absorb_index<F: Field>(transcript: &mut IopTranscript, index: &PesatIndex<F>) {
    transcript.absorb(b"index");
    absorb_usize(transcript, index.constraints.len());
    absorb_usize(transcript, index.public_len);
    absorb_usize(transcript, index.witness_len);
    absorb_usize(transcript, index.degree_bound);
    for constraint in &index.constraints {
        absorb_usize(transcript, constraint.terms.len());
        for term in &constraint.terms {
            absorb_field(transcript, &term.coeff);
            absorb_usize(transcript, term.vars.len());
            for var in &term.vars {
                absorb_usize(transcript, *var);
            }
        }
    }
}

fn absorb_instance<F: Field>(
    transcript: &mut IopTranscript,
    instance: &WarpAccumulatorInstance<F>,
) {
    transcript.absorb(b"acc-instance");
    transcript.absorb_len_prefixed(&instance.root);
    absorb_field_vec(transcript, &instance.alpha);
    absorb_field(transcript, &instance.mu);
    absorb_field_vec(transcript, &instance.beta);
    absorb_field(transcript, &instance.eta);
}

fn transcript_for<F: Field>(
    index: &PesatIndex<F>,
    fresh_publics: &[Vec<F>],
    accumulators: &[WarpAccumulatorInstance<F>],
) -> IopTranscript {
    let mut transcript = IopTranscript::new().domain_sep(TRANSCRIPT_DOMAIN);
    absorb_index(&mut transcript, index);
    absorb_usize(&mut transcript, fresh_publics.len());
    for public in fresh_publics {
        transcript.absorb(b"fresh-public");
        absorb_field_vec(&mut transcript, public);
    }
    absorb_usize(&mut transcript, accumulators.len());
    for accumulator in accumulators {
        absorb_instance(&mut transcript, accumulator);
    }
    transcript
}

fn squeeze_field_vec<F: Field + FromBytes>(transcript: &IopTranscript, len: usize) -> Vec<F> {
    let nbytes = (F::BITS / 8) as usize;
    transcript
        .squeeze_bytes(len * nbytes)
        .chunks_exact(nbytes)
        .map(F::from_bytes)
        .collect()
}

fn squeeze_shift_and_xi<F: Field + FromBytes>(
    transcript: &IopTranscript,
    shift_count: usize,
    log_claim_count: usize,
    domain: usize,
) -> (Vec<usize>, Vec<F>) {
    assert!(domain.is_power_of_two());
    let index_bytes = ((domain.trailing_zeros() as usize) + 7) / 8;
    let field_bytes = (F::BITS / 8) as usize;
    let bytes = transcript.squeeze_bytes(shift_count * index_bytes + log_claim_count * field_bytes);
    let mut offset = 0usize;
    let mut shifts = Vec::with_capacity(shift_count);
    for _ in 0..shift_count {
        let mut value = 0usize;
        for (shift, byte) in bytes[offset..offset + index_bytes].iter().enumerate() {
            value |= (*byte as usize) << (8 * shift);
        }
        shifts.push(value & (domain - 1));
        offset += index_bytes;
    }
    let xi = bytes[offset..]
        .chunks_exact(field_bytes)
        .map(F::from_bytes)
        .collect();
    (shifts, xi)
}

fn codeword_leaves<F: Field>(codewords: &[Vec<F>]) -> Vec<Vec<u8>> {
    let n = codewords.first().map(Vec::len).unwrap_or(0);
    (0..n)
        .map(|index| {
            let mut leaf = Vec::new();
            for codeword in codewords {
                leaf.extend_from_slice(&codeword[index].to_bytes());
            }
            leaf
        })
        .collect()
}

fn single_codeword_tree<F: Field>(codeword: &[F]) -> MerkleTree {
    MerkleTree::commit(&codeword_leaves(&[codeword.to_vec()]))
}

fn first_sumcheck_degree(log_n: usize, log_m: usize, degree_bound: usize) -> usize {
    1 + (log_n + 1).max(log_m + degree_bound)
}

fn point_with_suffix<F: Field + FromBytes>(
    prefix: &[F],
    current: F,
    suffix: usize,
    suffix_len: usize,
) -> Vec<F> {
    let mut point = prefix.to_vec();
    point.push(current);
    point.extend((0..suffix_len).map(|bit| {
        if (suffix >> bit) & 1 == 1 {
            F::ONE
        } else {
            F::ZERO
        }
    }));
    point
}

fn evaluate_g<F: Field>(
    index: &PesatIndex<F>,
    codeword: &[F],
    witness: &[F],
    alpha: &[F],
    beta: &[F],
    omega: F,
) -> WarpResult<F> {
    let eq_alpha = build_eq_evals(alpha);
    if eq_alpha.len() != codeword.len() {
        return Err(WarpError::ShapeMismatch);
    }
    let mut evaluation = F::ZERO;
    for (weight, value) in eq_alpha.iter().zip(codeword) {
        evaluation = evaluation.add(&weight.mul(value));
    }
    let bundled = index.evaluate_bundled(beta, witness)?;
    Ok(evaluation.add(&omega.mul(&bundled)))
}

fn first_sumcheck_eval<F: Field + FromBytes>(
    index: &PesatIndex<F>,
    codewords: &[Vec<F>],
    witnesses: &[Vec<F>],
    alphas: &[Vec<F>],
    betas: &[Vec<F>],
    tau: &[F],
    omega: F,
    prefix: &[F],
    x: F,
) -> WarpResult<F> {
    let log_ell = codewords.len().trailing_zeros() as usize;
    let round = prefix.len();
    let suffix_len = log_ell - round - 1;
    let mut out = F::ZERO;
    for suffix in 0..(1usize << suffix_len) {
        let point = point_with_suffix(prefix, x, suffix, suffix_len);
        let codeword = combine_vectors(codewords, &point)?;
        let witness = combine_vectors(witnesses, &point)?;
        let alpha = combine_vectors(alphas, &point)?;
        let beta = combine_vectors(betas, &point)?;
        let g = evaluate_g(index, &codeword, &witness, &alpha, &beta, omega)?;
        out = out.add(&eq_scalar(tau, &point).mul(&g));
    }
    Ok(out)
}

fn make_first_sumcheck<F: Field + FromBytes>(
    index: &PesatIndex<F>,
    codewords: &[Vec<F>],
    witnesses: &[Vec<F>],
    alphas: &[Vec<F>],
    betas: &[Vec<F>],
    tau: &[F],
    omega: F,
    prefix: &[F],
    degree: usize,
) -> WarpResult<WarpUnivariate<F>> {
    let mut evaluations = Vec::with_capacity(degree + 1);
    for x in 0..=degree {
        evaluations.push(first_sumcheck_eval(
            index,
            codewords,
            witnesses,
            alphas,
            betas,
            tau,
            omega,
            prefix,
            F::from_u64(x as u64),
        )?);
    }
    Ok(WarpUnivariate { evaluations })
}

fn eq_star_at<F: Field + FromBytes>(
    claim_points: &[Vec<F>],
    xi: &[F],
    point: &[F],
) -> WarpResult<F> {
    if claim_points.is_empty() || !claim_points.len().is_power_of_two() {
        return Err(WarpError::NonPowerOfTwo);
    }
    if xi.len() != claim_points.len().trailing_zeros() as usize {
        return Err(WarpError::ShapeMismatch);
    }
    let mut out = F::ZERO;
    for (index, claim_point) in claim_points.iter().enumerate() {
        let claim_index = binary_point(index, xi.len());
        let weight = eq_scalar(xi, &claim_index).mul(&eq_scalar(claim_point, point));
        out = out.add(&weight);
    }
    Ok(out)
}

fn second_sumcheck_eval<F: Field + FromBytes>(
    codeword: &[F],
    claim_points: &[Vec<F>],
    xi: &[F],
    prefix: &[F],
    x: F,
) -> WarpResult<F> {
    let log_n = codeword.len().trailing_zeros() as usize;
    let round = prefix.len();
    let suffix_len = log_n - round - 1;
    let mut out = F::ZERO;
    for suffix in 0..(1usize << suffix_len) {
        let point = point_with_suffix(prefix, x, suffix, suffix_len);
        let eq = eq_star_at(claim_points, xi, &point)?;
        out = out.add(&eq.mul(&mle_eval(codeword, &point)));
    }
    Ok(out)
}

fn make_second_sumcheck<F: Field + FromBytes>(
    codeword: &[F],
    claim_points: &[Vec<F>],
    xi: &[F],
    prefix: &[F],
) -> WarpResult<WarpUnivariate<F>> {
    let mut evaluations = Vec::with_capacity(3);
    for x in 0..=2usize {
        evaluations.push(second_sumcheck_eval(
            codeword,
            claim_points,
            xi,
            prefix,
            F::from_u64(x as u64),
        )?);
    }
    Ok(WarpUnivariate { evaluations })
}

fn check_sumcheck<F: Field + FromBytes>(
    polys: &[WarpUnivariate<F>],
    initial_claim: F,
    challenges: &[F],
    final_claim: F,
) -> bool {
    if polys.len() != challenges.len() {
        return false;
    }
    if polys.is_empty() {
        return initial_claim == final_claim;
    }
    if polys[0].at_zero().add(&polys[0].at_one()) != initial_claim {
        return false;
    }
    for round in 1..polys.len() {
        let expected = polys[round - 1].evaluate(challenges[round - 1]);
        if polys[round].at_zero().add(&polys[round].at_one()) != expected {
            return false;
        }
    }
    polys.last().is_some_and(|poly| {
        poly.evaluate(*challenges.last().expect("nonempty challenges")) == final_claim
    })
}

fn validate_input_shapes<F: Field, C: LinearCode<F>>(
    index: &PesatIndex<F>,
    code: &C,
    sampling: WarpSampling,
    fresh: &[PesatWitness<F>],
    accumulators: &[WarpAccumulator<F>],
) -> WarpResult<(usize, usize, usize, usize)> {
    index.validate()?;
    sampling.validate()?;
    if fresh.is_empty() {
        return Err(WarpError::ShapeMismatch);
    }
    let ell = fresh.len() + accumulators.len();
    if !ell.is_power_of_two() {
        return Err(WarpError::NonPowerOfTwo);
    }
    if code.message_len() != index.witness_len
        || !code.codeword_len().is_power_of_two()
        || code.codeword_len().trailing_zeros() as usize != sampling.log_codeword_len
    {
        return Err(WarpError::ShapeMismatch);
    }
    for instance in fresh {
        index.check_instance(instance)?;
    }
    let log_n = code.codeword_len().trailing_zeros() as usize;
    let log_m = index.log_constraints();
    for accumulator in accumulators {
        if accumulator.instance.alpha.len() != log_n
            || accumulator.instance.beta.len() != index.public_len + log_m
            || accumulator.witness.codeword.len() != code.codeword_len()
            || accumulator.witness.witness.len() != index.witness_len
        {
            return Err(WarpError::ShapeMismatch);
        }
        if accumulator.witness.tree.root() != accumulator.instance.root {
            return Err(WarpError::CommitmentMismatch);
        }
    }
    Ok((ell, log_n, log_m, index.public_len + log_m))
}

/// Construct a genesis accumulator for one fresh PESAT statement.
///
/// This applies Construction 5.10 with the deterministic `tau = 0^{log M}`
/// point permitted for seeding the split accumulator state. It is not an
/// accumulation proof by itself; use [`warp_prove`] for updates.
pub fn warp_genesis<F, C>(
    index: &PesatIndex<F>,
    code: &C,
    instance: &PesatWitness<F>,
) -> WarpResult<WarpAccumulator<F>>
where
    F: Field + FromBytes,
    C: LinearCode<F>,
{
    index.validate()?;
    if code.message_len() != index.witness_len || !code.codeword_len().is_power_of_two() {
        return Err(WarpError::ShapeMismatch);
    }
    index.check_instance(instance)?;
    let codeword = code.encode(&instance.witness);
    let tree = single_codeword_tree(&codeword);
    let log_n = code.codeword_len().trailing_zeros() as usize;
    let log_m = index.log_constraints();
    let alpha = vec![F::ZERO; log_n];
    let mut beta = instance.public.clone();
    beta.extend(vec![F::ZERO; log_m]);
    let mu = mle_eval(&codeword, &alpha);
    let eta = index.evaluate_bundled(&beta, &instance.witness)?;
    Ok(WarpAccumulator {
        instance: WarpAccumulatorInstance {
            root: tree.root(),
            alpha,
            mu,
            beta,
            eta,
        },
        witness: WarpAccumulatorWitness {
            tree,
            codeword,
            witness: instance.witness.clone(),
        },
    })
}

/// Prove Construction 10.4 for fresh PESAT inputs plus prior accumulators.
///
/// The caller chooses fresh and accumulator counts whose sum `ell` is a
/// power of two. All fresh witnesses are checked against the PESAT index
/// before proving; this is an honest-prover API, not a statement of
/// soundness for malformed caller inputs.
pub fn warp_prove<F, C>(
    index: &PesatIndex<F>,
    code: &C,
    sampling: WarpSampling,
    fresh: &[PesatWitness<F>],
    accumulators: &[WarpAccumulator<F>],
) -> WarpResult<(WarpAccumulator<F>, WarpProof<F>)>
where
    F: Field + FromBytes,
    C: LinearCode<F>,
{
    let (ell, log_n, log_m, _) = validate_input_shapes(index, code, sampling, fresh, accumulators)?;
    let n = code.codeword_len();
    let fresh_publics: Vec<Vec<F>> = fresh.iter().map(|item| item.public.clone()).collect();
    let accumulator_instances: Vec<WarpAccumulatorInstance<F>> = accumulators
        .iter()
        .map(|accumulator| accumulator.instance.clone())
        .collect();
    let mut transcript = transcript_for(index, &fresh_publics, &accumulator_instances);

    // Construction 5.10 + Remark 5.9: encode witnesses and reveal the
    // deterministic alpha=0 evaluation needed by Construction 9.4.
    let fresh_codewords: Vec<Vec<F>> = fresh
        .iter()
        .map(|instance| code.encode(&instance.witness))
        .collect();
    let initial_tree = MerkleTree::commit(&codeword_leaves(&fresh_codewords));
    let initial_root = initial_tree.root();
    transcript.absorb_len_prefixed(&initial_root);

    let mut alphas = Vec::with_capacity(ell);
    let mut mus = Vec::with_capacity(ell);
    let mut betas = Vec::with_capacity(ell);
    let mut etas = Vec::with_capacity(ell);
    let mut codewords = Vec::with_capacity(ell);
    let mut witnesses = Vec::with_capacity(ell);

    let fresh_mus: Vec<F> = fresh_codewords
        .iter()
        .map(|codeword| mle_eval(codeword, &vec![F::ZERO; log_n]))
        .collect();
    let taus: Vec<Vec<F>> = if log_m == 0 {
        vec![Vec::new(); fresh.len()]
    } else {
        let bytes = transcript.squeeze_bytes(fresh.len() * log_m * (F::BITS as usize / 8));
        let nbytes = F::BITS as usize / 8;
        bytes
            .chunks_exact(log_m * nbytes)
            .map(|chunk| chunk.chunks_exact(nbytes).map(F::from_bytes).collect())
            .collect()
    };

    for ((instance, codeword), (mu, tau)) in fresh
        .iter()
        .zip(fresh_codewords.iter())
        .zip(fresh_mus.iter().zip(taus.iter()))
    {
        alphas.push(vec![F::ZERO; log_n]);
        mus.push(*mu);
        let mut beta = instance.public.clone();
        beta.extend_from_slice(tau);
        betas.push(beta);
        etas.push(F::ZERO);
        codewords.push(codeword.clone());
        witnesses.push(instance.witness.clone());
    }
    absorb_field_vec(&mut transcript, &fresh_mus);

    for accumulator in accumulators {
        alphas.push(accumulator.instance.alpha.clone());
        mus.push(accumulator.instance.mu);
        betas.push(accumulator.instance.beta.clone());
        etas.push(accumulator.instance.eta);
        codewords.push(accumulator.witness.codeword.clone());
        witnesses.push(accumulator.witness.witness.clone());
    }

    // Construction 6.3, as inlined in Construction 9.4.
    let tau_omega: Vec<F> = squeeze_field_vec(&transcript, ell.trailing_zeros() as usize + 1);
    let tau: Vec<F> = tau_omega[..tau_omega.len() - 1].to_vec();
    let omega: F = tau_omega[tau_omega.len() - 1];
    let sigma1 = {
        let eq_tau = build_eq_evals(&tau);
        let mut out = F::ZERO;
        for ((mu, eta), weight) in mus.iter().zip(&etas).zip(eq_tau) {
            out = out.add(&weight.mul(&mu.add(&omega.mul(eta))));
        }
        out
    };

    let degree = first_sumcheck_degree(log_n, log_m, index.degree_bound);
    let mut first_sumcheck = Vec::with_capacity(ell.trailing_zeros() as usize);
    let mut gamma = Vec::with_capacity(ell.trailing_zeros() as usize);
    for _ in 0..ell.trailing_zeros() {
        let poly = make_first_sumcheck(
            index, &codewords, &witnesses, &alphas, &betas, &tau, omega, &gamma, degree,
        )?;
        absorb_polynomial(&mut transcript, &poly);
        gamma.push(squeeze_field_vec(&transcript, 1)[0]);
        first_sumcheck.push(poly);
    }
    if !check_sumcheck(&first_sumcheck, sigma1, &gamma, {
        let zeta0 = combine_vectors(&alphas, &gamma)?;
        let beta = combine_vectors(&betas, &gamma)?;
        let folded = combine_vectors(&codewords, &gamma)?;
        let witness = combine_vectors(&witnesses, &gamma)?;
        let nu0 = mle_eval(&folded, &zeta0);
        let eta = index.evaluate_bundled(&beta, &witness)?;
        eq_scalar(&tau, &gamma).mul(&nu0.add(&omega.mul(&eta)))
    }) {
        return Err(WarpError::SumcheckMismatch);
    }

    let zeta0 = combine_vectors(&alphas, &gamma)?;
    let beta = combine_vectors(&betas, &gamma)?;
    let folded_codeword = combine_vectors(&codewords, &gamma)?;
    let folded_witness = combine_vectors(&witnesses, &gamma)?;
    let nu0 = mle_eval(&folded_codeword, &zeta0);
    let eta = index.evaluate_bundled(&beta, &folded_witness)?;

    // Construction 7.2: commit and add OOD plus shift-query claims.
    let output_tree = single_codeword_tree(&folded_codeword);
    let output_root = output_tree.root();
    transcript.absorb_len_prefixed(&output_root);
    absorb_field(&mut transcript, &nu0);
    absorb_field(&mut transcript, &eta);
    let ood_points: Vec<Vec<F>> = if sampling.out_of_domain == 0 || log_n == 0 {
        vec![Vec::new(); sampling.out_of_domain]
    } else {
        let nbytes = F::BITS as usize / 8;
        transcript
            .squeeze_bytes(sampling.out_of_domain * log_n * nbytes)
            .chunks_exact(log_n * nbytes)
            .map(|chunk| chunk.chunks_exact(nbytes).map(F::from_bytes).collect())
            .collect()
    };
    let ood_answers: Vec<F> = ood_points
        .iter()
        .map(|point| mle_eval(&folded_codeword, point))
        .collect();
    absorb_field_vec(&mut transcript, &ood_answers);

    let (shift_indices, xi): (Vec<usize>, Vec<F>) = squeeze_shift_and_xi(
        &transcript,
        sampling.shift_queries,
        sampling.claim_count().trailing_zeros() as usize,
        n,
    );
    let mut claim_points = Vec::with_capacity(sampling.claim_count());
    let mut claim_values = Vec::with_capacity(sampling.claim_count());
    claim_points.push(zeta0);
    claim_values.push(nu0);
    claim_points.extend(ood_points);
    claim_values.extend(ood_answers.iter().copied());
    for index in &shift_indices {
        claim_points.push(binary_point(*index, log_n));
        claim_values.push(folded_codeword[*index]);
    }
    let sigma2 = {
        let eq_xi = build_eq_evals(&xi);
        let mut out = F::ZERO;
        for (value, weight) in claim_values.iter().zip(eq_xi) {
            out = out.add(&value.mul(&weight));
        }
        out
    };

    // Construction 8.2: batch the r=1+s+t claims into one.
    let mut second_sumcheck = Vec::with_capacity(log_n);
    let mut alpha = Vec::with_capacity(log_n);
    for _ in 0..log_n {
        let poly = make_second_sumcheck(&folded_codeword, &claim_points, &xi, &alpha)?;
        absorb_polynomial(&mut transcript, &poly);
        alpha.push(squeeze_field_vec(&transcript, 1)[0]);
        second_sumcheck.push(poly);
    }
    let mu = mle_eval(&folded_codeword, &alpha);
    if !check_sumcheck(&second_sumcheck, sigma2, &alpha, {
        let eq = eq_star_at(&claim_points, &xi, &alpha)?;
        mu.mul(&eq)
    }) {
        return Err(WarpError::SumcheckMismatch);
    }

    let initial_paths = shift_indices
        .iter()
        .map(|&index| initial_tree.open(index))
        .collect();
    let accumulator_paths = accumulators
        .iter()
        .map(|accumulator| {
            shift_indices
                .iter()
                .map(|&index| accumulator.witness.tree.open(index))
                .collect()
        })
        .collect();
    let openings = shift_indices
        .iter()
        .map(|&index| codewords.iter().map(|codeword| codeword[index]).collect())
        .collect();

    let accumulator = WarpAccumulator {
        instance: WarpAccumulatorInstance {
            root: output_root,
            alpha,
            mu,
            beta,
            eta,
        },
        witness: WarpAccumulatorWitness {
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

/// Verify a Construction 10.4 accumulation proof.
pub fn warp_verify<F: Field + FromBytes>(
    index: &PesatIndex<F>,
    sampling: WarpSampling,
    fresh_publics: &[Vec<F>],
    input_accumulators: &[WarpAccumulatorInstance<F>],
    output: &WarpAccumulatorInstance<F>,
    proof: &WarpProof<F>,
) -> WarpResult<()> {
    index.validate()?;
    sampling.validate()?;
    if fresh_publics.is_empty() {
        return Err(WarpError::ShapeMismatch);
    }
    let ell = fresh_publics.len() + input_accumulators.len();
    if !ell.is_power_of_two() {
        return Err(WarpError::NonPowerOfTwo);
    }
    let log_ell = ell.trailing_zeros() as usize;
    let log_m = index.log_constraints();
    let beta_len = index.public_len + log_m;
    if proof.fresh_mus.len() != fresh_publics.len()
        || proof.first_sumcheck.len() != log_ell
        || proof.ood_answers.len() != sampling.out_of_domain
        || proof.initial_paths.len() != sampling.shift_queries
        || proof.accumulator_paths.len() != input_accumulators.len()
        || proof.openings.len() != sampling.shift_queries
    {
        return Err(WarpError::ShapeMismatch);
    }
    if input_accumulators
        .iter()
        .any(|accumulator| accumulator.beta.len() != beta_len)
    {
        return Err(WarpError::ShapeMismatch);
    }
    let log_n = sampling.log_codeword_len;
    if output.alpha.len() != log_n
        || output.beta.len() != beta_len
        || input_accumulators
            .iter()
            .any(|accumulator| accumulator.alpha.len() != log_n)
        || proof.second_sumcheck.len() != log_n
    {
        return Err(WarpError::ShapeMismatch);
    }
    if proof
        .accumulator_paths
        .iter()
        .any(|paths| paths.len() != sampling.shift_queries)
        || proof.openings.iter().any(|row| row.len() != ell)
    {
        return Err(WarpError::ShapeMismatch);
    }

    let mut transcript = transcript_for(index, fresh_publics, input_accumulators);
    transcript.absorb_len_prefixed(&proof.initial_root);
    let taus: Vec<Vec<F>> = if log_m == 0 {
        vec![Vec::new(); fresh_publics.len()]
    } else {
        let nbytes = F::BITS as usize / 8;
        transcript
            .squeeze_bytes(fresh_publics.len() * log_m * nbytes)
            .chunks_exact(log_m * nbytes)
            .map(|chunk| chunk.chunks_exact(nbytes).map(F::from_bytes).collect())
            .collect()
    };
    absorb_field_vec(&mut transcript, &proof.fresh_mus);

    let mut alphas = Vec::with_capacity(ell);
    let mut mus = Vec::with_capacity(ell);
    let mut betas = Vec::with_capacity(ell);
    let mut etas = Vec::with_capacity(ell);
    for ((public, tau), mu) in fresh_publics.iter().zip(taus).zip(proof.fresh_mus.iter()) {
        alphas.push(vec![F::ZERO; log_n]);
        mus.push(*mu);
        let mut beta = public.clone();
        beta.extend_from_slice(&tau);
        betas.push(beta);
        etas.push(F::ZERO);
    }
    for accumulator in input_accumulators {
        alphas.push(accumulator.alpha.clone());
        mus.push(accumulator.mu);
        betas.push(accumulator.beta.clone());
        etas.push(accumulator.eta);
    }

    let tau_omega: Vec<F> = squeeze_field_vec(&transcript, log_ell + 1);
    let tau: Vec<F> = tau_omega[..tau_omega.len() - 1].to_vec();
    let omega: F = tau_omega[tau_omega.len() - 1];
    let mut gamma = Vec::with_capacity(log_ell);
    for poly in &proof.first_sumcheck {
        absorb_polynomial(&mut transcript, poly);
        gamma.push(squeeze_field_vec(&transcript, 1)[0]);
    }
    let sigma1 = {
        let eq_tau = build_eq_evals(&tau);
        let mut out = F::ZERO;
        for ((mu, eta), weight) in mus.iter().zip(&etas).zip(eq_tau) {
            out = out.add(&weight.mul(&mu.add(&omega.mul(eta))));
        }
        out
    };
    let zeta0 = combine_vectors(&alphas, &gamma)?;
    let beta = combine_vectors(&betas, &gamma)?;
    if beta != output.beta {
        return Err(WarpError::ShapeMismatch);
    }
    if !check_sumcheck(&proof.first_sumcheck, sigma1, &gamma, {
        eq_scalar(&tau, &gamma).mul(&proof.nu0.add(&omega.mul(&output.eta)))
    }) {
        return Err(WarpError::SumcheckMismatch);
    }

    transcript.absorb_len_prefixed(&output.root);
    absorb_field(&mut transcript, &proof.nu0);
    absorb_field(&mut transcript, &output.eta);
    let ood_points: Vec<Vec<F>> = if sampling.out_of_domain == 0 || log_n == 0 {
        vec![Vec::new(); sampling.out_of_domain]
    } else {
        let nbytes = F::BITS as usize / 8;
        transcript
            .squeeze_bytes(sampling.out_of_domain * log_n * nbytes)
            .chunks_exact(log_n * nbytes)
            .map(|chunk| chunk.chunks_exact(nbytes).map(F::from_bytes).collect())
            .collect()
    };
    absorb_field_vec(&mut transcript, &proof.ood_answers);
    let (shift_indices, xi): (Vec<usize>, Vec<F>) = squeeze_shift_and_xi(
        &transcript,
        sampling.shift_queries,
        sampling.claim_count().trailing_zeros() as usize,
        1usize << log_n,
    );

    // Construction 7.2 openings: fresh inputs share one stacked commitment;
    // each prior accumulator keeps its own root.
    for (query, (&index, path)) in shift_indices
        .iter()
        .zip(proof.initial_paths.iter())
        .enumerate()
    {
        let row = &proof.openings[query];
        let mut leaf = Vec::new();
        for value in &row[..fresh_publics.len()] {
            leaf.extend_from_slice(&value.to_bytes());
        }
        if !merkle_verify(&proof.initial_root, &leaf, index, path) {
            return Err(WarpError::MerkleMismatch);
        }
    }
    for (accumulator_index, (accumulator, paths)) in input_accumulators
        .iter()
        .zip(proof.accumulator_paths.iter())
        .enumerate()
    {
        for (query, (&index, path)) in shift_indices.iter().zip(paths.iter()).enumerate() {
            let value = proof.openings[query][fresh_publics.len() + accumulator_index];
            if !merkle_verify(&accumulator.root, &value.to_bytes(), index, path) {
                return Err(WarpError::MerkleMismatch);
            }
        }
    }

    let mut claim_points = Vec::with_capacity(sampling.claim_count());
    let mut claim_values = Vec::with_capacity(sampling.claim_count());
    claim_points.push(zeta0);
    claim_values.push(proof.nu0);
    claim_points.extend(ood_points);
    claim_values.extend(proof.ood_answers.iter().copied());
    let eq_gamma = build_eq_evals(&gamma);
    for (&index, row) in shift_indices.iter().zip(proof.openings.iter()) {
        claim_points.push(binary_point(index, log_n));
        let mut value = F::ZERO;
        for (opening, weight) in row.iter().zip(eq_gamma.iter()) {
            value = value.add(&opening.mul(weight));
        }
        claim_values.push(value);
    }
    let sigma2 = {
        let eq_xi = build_eq_evals(&xi);
        let mut out = F::ZERO;
        for (value, weight) in claim_values.iter().zip(eq_xi) {
            out = out.add(&value.mul(&weight));
        }
        out
    };
    let alpha: Vec<F> = proof
        .second_sumcheck
        .iter()
        .map(|poly| {
            absorb_polynomial(&mut transcript, poly);
            squeeze_field_vec(&transcript, 1)[0]
        })
        .collect();
    if alpha != output.alpha {
        return Err(WarpError::ShapeMismatch);
    }
    if !check_sumcheck(&proof.second_sumcheck, sigma2, &alpha, {
        let points: Vec<Vec<F>> = claim_points;
        let eq = eq_star_at(&points, &xi, &output.alpha)?;
        output.mu.mul(&eq)
    }) {
        return Err(WarpError::SumcheckMismatch);
    }
    Ok(())
}

/// Decide a split WARP accumulator.
pub fn warp_decide<F, C>(
    index: &PesatIndex<F>,
    code: &C,
    accumulator: &WarpAccumulator<F>,
) -> WarpResult<()>
where
    F: Field,
    C: LinearCode<F>,
{
    index.validate()?;
    if code.message_len() != index.witness_len
        || code.codeword_len() != accumulator.witness.codeword.len()
        || accumulator.witness.witness.len() != index.witness_len
        || accumulator.witness.tree.root() != accumulator.instance.root
    {
        return Err(WarpError::ShapeMismatch);
    }
    let recomputed = single_codeword_tree(&accumulator.witness.codeword);
    if recomputed.root() != accumulator.instance.root {
        return Err(WarpError::CommitmentMismatch);
    }
    if accumulator.witness.codeword != code.encode(&accumulator.witness.witness) {
        return Err(WarpError::CommitmentMismatch);
    }
    if mle_eval(&accumulator.witness.codeword, &accumulator.instance.alpha)
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
    use crate::field::Gf128;

    fn f(value: u64) -> Gf128 {
        let mut bytes = [0u8; 16];
        bytes[..8].copy_from_slice(&value.to_le_bytes());
        Gf128::from_bytes(&bytes)
    }

    fn toy_index() -> PesatIndex<Gf128> {
        // w0*w1 + w2 + w3 = 0 over the characteristic-2 field.
        PesatIndex {
            constraints: vec![PesatConstraint {
                terms: vec![
                    PesatTerm {
                        coeff: Gf128::ONE,
                        vars: vec![0, 1],
                    },
                    PesatTerm {
                        coeff: Gf128::ONE,
                        vars: vec![2],
                    },
                    PesatTerm {
                        coeff: Gf128::ONE,
                        vars: vec![3],
                    },
                ],
            }],
            public_len: 0,
            witness_len: 4,
            degree_bound: 2,
        }
    }

    fn witness(a: u64, b: u64, c: u64) -> PesatWitness<Gf128> {
        // In characteristic 2, a*b + c + d = 0 implies d = a*b + c.
        let d = f(a).mul(&f(b)).add(&f(c));
        PesatWitness {
            public: Vec::new(),
            witness: vec![f(a), f(b), f(c), d],
        }
    }

    fn public_index() -> PesatIndex<Gf128> {
        // Constraint 0: x*w0 + w1 = 0.
        // Constraint 1: x + w2 + w3 = 0.
        // Variable order is x || w, matching Construction 10.4's beta=x||tau.
        PesatIndex {
            constraints: vec![
                PesatConstraint {
                    terms: vec![
                        PesatTerm {
                            coeff: Gf128::ONE,
                            vars: vec![0, 1],
                        },
                        PesatTerm {
                            coeff: Gf128::ONE,
                            vars: vec![2],
                        },
                    ],
                },
                PesatConstraint {
                    terms: vec![
                        PesatTerm {
                            coeff: Gf128::ONE,
                            vars: vec![0],
                        },
                        PesatTerm {
                            coeff: Gf128::ONE,
                            vars: vec![3],
                        },
                        PesatTerm {
                            coeff: Gf128::ONE,
                            vars: vec![4],
                        },
                    ],
                },
            ],
            public_len: 1,
            witness_len: 4,
            degree_bound: 2,
        }
    }

    fn public_witness(x: u64, w0: u64, w2: u64) -> PesatWitness<Gf128> {
        let x = f(x);
        let w0 = f(w0);
        let w2 = f(w2);
        let w1 = x.mul(&w0);
        let w3 = x.add(&w2);
        PesatWitness {
            public: vec![x],
            witness: vec![w0, w1, w2, w3],
        }
    }

    /// §5.4: WARP accumulates over the VOLE field `Galois128`. This oracle
    /// instance is the differential reference for the `volar-spec` port.
    #[test]
    fn oracle_runs_over_the_vole_field() {
        type F = volar_primitives::Galois128;
        let f = |v: u64| volar_primitives::Galois128(v as u128);
        let index = PesatIndex {
            constraints: vec![PesatConstraint {
                terms: vec![
                    PesatTerm {
                        coeff: f(1),
                        vars: vec![0, 1],
                    },
                    PesatTerm {
                        coeff: f(1),
                        vars: vec![2],
                    },
                    PesatTerm {
                        coeff: f(1),
                        vars: vec![3],
                    },
                ],
            }],
            public_len: 0,
            witness_len: 4,
            degree_bound: 2,
        };
        let witness = |a: u64, b: u64, c: u64| {
            let d = f(a).mul(&f(b)).add(&f(c));
            PesatWitness {
                public: Vec::new(),
                witness: vec![f(a), f(b), f(c), d],
            }
        };
        let code = IdentityCode::new(4).unwrap();
        let sampling = WarpSampling {
            log_codeword_len: 2,
            out_of_domain: 1,
            shift_queries: 2,
        };
        let fresh = vec![witness(3, 5, 7), witness(9, 11, 13)];
        let (accumulator, proof) = warp_prove(&index, &code, sampling, &fresh, &[]).unwrap();
        let publics: Vec<Vec<F>> = fresh.iter().map(|item| item.public.clone()).collect();
        warp_verify(
            &index,
            sampling,
            &publics,
            &[],
            &accumulator.instance,
            &proof,
        )
        .unwrap();
        warp_decide(&index, &code, &accumulator).unwrap();
    }

    #[test]
    fn honest_two_fresh_accumulation_proves_verifies_and_decides() {
        let index = toy_index();
        let code = IdentityCode::new(4).unwrap();
        let sampling = WarpSampling {
            log_codeword_len: 2,
            out_of_domain: 1,
            shift_queries: 2,
        };
        let fresh = vec![witness(3, 5, 7), witness(9, 11, 13)];
        let (accumulator, proof) = warp_prove(&index, &code, sampling, &fresh, &[]).unwrap();
        let publics: Vec<Vec<Gf128>> = fresh.iter().map(|item| item.public.clone()).collect();
        warp_verify(
            &index,
            sampling,
            &publics,
            &[],
            &accumulator.instance,
            &proof,
        )
        .unwrap();
        warp_decide(&index, &code, &accumulator).unwrap();
    }

    #[test]
    fn prior_accumulator_folds_with_new_fresh_statement() {
        let index = toy_index();
        let code = IdentityCode::new(4).unwrap();
        let sampling = WarpSampling {
            log_codeword_len: 2,
            out_of_domain: 1,
            shift_queries: 2,
        };
        let first = warp_genesis(&index, &code, &witness(17, 19, 23)).unwrap();
        warp_decide(&index, &code, &first).unwrap();
        let fresh = vec![witness(29, 31, 37)];
        let (next, proof) = warp_prove(&index, &code, sampling, &fresh, &[first.clone()]).unwrap();
        let publics: Vec<Vec<Gf128>> = fresh.iter().map(|item| item.public.clone()).collect();
        warp_verify(
            &index,
            sampling,
            &publics,
            &[first.instance.clone()],
            &next.instance,
            &proof,
        )
        .unwrap();
        warp_decide(&index, &code, &next).unwrap();
    }

    #[test]
    fn repetition_code_accumulation_round_trips() {
        let index = toy_index();
        let code = RepetitionCode::new(4, 2).unwrap();
        let sampling = WarpSampling {
            log_codeword_len: 3,
            out_of_domain: 1,
            shift_queries: 2,
        };
        let fresh = vec![witness(2, 4, 8), witness(16, 32, 64)];
        let (accumulator, proof) = warp_prove(&index, &code, sampling, &fresh, &[]).unwrap();
        let publics: Vec<Vec<Gf128>> = fresh.iter().map(|item| item.public.clone()).collect();
        warp_verify(
            &index,
            sampling,
            &publics,
            &[],
            &accumulator.instance,
            &proof,
        )
        .unwrap();
        warp_decide(&index, &code, &accumulator).unwrap();
    }

    #[test]
    fn unsatisfied_pesat_witness_is_rejected_before_proving() {
        let index = toy_index();
        let code = IdentityCode::new(4).unwrap();
        let bad = PesatWitness {
            public: Vec::new(),
            witness: vec![f(1), f(2), f(3), f(99)],
        };
        let two = vec![witness(1, 2, 3), bad];
        assert!(matches!(
            warp_prove(
                &index,
                &code,
                WarpSampling {
                    log_codeword_len: 2,
                    out_of_domain: 1,
                    shift_queries: 2,
                },
                &two,
                &[],
            ),
            Err(WarpError::UnsatisfiedPesat { constraint: 0 })
        ));
    }

    #[test]
    fn public_inputs_and_multi_constraint_bundling_round_trip() {
        let index = public_index();
        let code = IdentityCode::new(4).unwrap();
        let sampling = WarpSampling {
            log_codeword_len: 2,
            out_of_domain: 1,
            shift_queries: 2,
        };
        let fresh = vec![public_witness(5, 7, 11), public_witness(13, 17, 19)];
        let (accumulator, proof) = warp_prove(&index, &code, sampling, &fresh, &[]).unwrap();
        let publics: Vec<Vec<Gf128>> = fresh.iter().map(|item| item.public.clone()).collect();
        warp_verify(
            &index,
            sampling,
            &publics,
            &[],
            &accumulator.instance,
            &proof,
        )
        .unwrap();
        warp_decide(&index, &code, &accumulator).unwrap();

        let wrong_publics = vec![vec![f(23)], vec![f(29)]];
        assert!(
            warp_verify(
                &index,
                sampling,
                &wrong_publics,
                &[],
                &accumulator.instance,
                &proof,
            )
            .is_err(),
            "changing a public input must change the transcript and reject"
        );
    }

    #[test]
    fn tampered_output_root_is_rejected() {
        let index = toy_index();
        let code = IdentityCode::new(4).unwrap();
        let sampling = WarpSampling {
            log_codeword_len: 2,
            out_of_domain: 1,
            shift_queries: 2,
        };
        let fresh = vec![witness(3, 5, 7), witness(9, 11, 13)];
        let (mut accumulator, proof) = warp_prove(&index, &code, sampling, &fresh, &[]).unwrap();
        accumulator.instance.root[0] ^= 1;
        let publics: Vec<Vec<Gf128>> = fresh.iter().map(|item| item.public.clone()).collect();
        assert!(
            warp_verify(
                &index,
                sampling,
                &publics,
                &[],
                &accumulator.instance,
                &proof,
            )
            .is_err(),
            "the output root is bound into the Fiat-Shamir transcript"
        );
    }

    #[test]
    fn decider_rejects_witness_that_does_not_encode_to_codeword() {
        let index = toy_index();
        let code = IdentityCode::new(4).unwrap();
        let mut accumulator = warp_genesis(&index, &code, &witness(17, 19, 23)).unwrap();
        accumulator.witness.witness[0] = accumulator.witness.witness[0].add(&Gf128::ONE);
        assert_eq!(
            warp_decide(&index, &code, &accumulator),
            Err(WarpError::CommitmentMismatch)
        );
    }

    #[test]
    fn tampered_sumcheck_is_rejected() {
        let index = toy_index();
        let code = IdentityCode::new(4).unwrap();
        let sampling = WarpSampling {
            log_codeword_len: 2,
            out_of_domain: 1,
            shift_queries: 2,
        };
        let fresh = vec![witness(3, 5, 7), witness(9, 11, 13)];
        let (accumulator, mut proof) = warp_prove(&index, &code, sampling, &fresh, &[]).unwrap();
        proof.first_sumcheck[0].evaluations[0] =
            proof.first_sumcheck[0].evaluations[0].add(&Gf128::ONE);
        let publics: Vec<Vec<Gf128>> = fresh.iter().map(|item| item.public.clone()).collect();
        assert_eq!(
            warp_verify(
                &index,
                sampling,
                &publics,
                &[],
                &accumulator.instance,
                &proof
            ),
            Err(WarpError::SumcheckMismatch)
        );
    }

    #[test]
    fn tampered_merkle_path_is_rejected() {
        let index = toy_index();
        let code = IdentityCode::new(4).unwrap();
        let sampling = WarpSampling {
            log_codeword_len: 2,
            out_of_domain: 1,
            shift_queries: 2,
        };
        let fresh = vec![witness(3, 5, 7), witness(9, 11, 13)];
        let (accumulator, mut proof) = warp_prove(&index, &code, sampling, &fresh, &[]).unwrap();
        proof.initial_paths[0].siblings[0][0] ^= 1;
        let publics: Vec<Vec<Gf128>> = fresh.iter().map(|item| item.public.clone()).collect();
        assert_eq!(
            warp_verify(
                &index,
                sampling,
                &publics,
                &[],
                &accumulator.instance,
                &proof
            ),
            Err(WarpError::MerkleMismatch)
        );
    }
}
