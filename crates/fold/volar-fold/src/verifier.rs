// @reliability: experimental
// @ai: assisted
//! **Prove-the-verifier**: fold the *whole* VOLE verifier into one relaxed-R1CS
//! instance, reusing the gap-folding IVC machinery ([`crate::ivc`]).
//!
//! # The idea
//!
//! After the pre-ZK passes and the ZK weave, the VOLE **verifier** is itself a
//! computation: a stream of per-AND-gate checks
//!
//! ```text
//!     K_a · K_b + V̂ = K_c · Δ
//! ```
//!
//! plus a memory multiset-hash accumulator update (`mem_acc`).  Each check is an
//! R1CS step (see [`and_check_r1cs`]); folding *all* of them — exactly as
//! [`crate::ivc::prove_gap`] folds a network gap — collapses the entire verifier
//! into **one** relaxed-R1CS instance of size `O(|F|)`, independent of the number
//! of gates.  The carried `mem_acc` is the boundary state, Pedersen-committed as
//! `c_in` / `c_out` (the memory-commitment reuse).
//!
//! # Why we can drop the zkSNARK
//!
//! The *inner* VOLE proof already accounts for zero-knowledge.  The outer proof
//! of the verifier's execution therefore does **not** need to be zero-knowledge —
//! it only handles public / committed data (gate MACs, Δ, the memory hash).  So
//! we verify the folded instance **natively** ([`crate::verify::native_verify`]),
//! with no final SNARK compression.  A *regular* (non-ZK) SNARK may compress it in
//! the future — see [`compress_with_snark`].
//!
//! # Discipline safety (compile-time)
//!
//! Mixing a ZK prover with a non-ZK prover is unsafe.  [`prove_verifier`] is bound
//! `where Z: NonZk`, so a [`volar_discipline::Zk`]-tagged artifact **cannot** be
//! folded here — it is a compile error — and the output is
//! [`volar_discipline::Transparent`].  The arithmetization frontend that turns a
//! woven verifier `IrModule` into a [`VerifierTrace`] preserves that tag (it lives
//! in the build pipeline, which depends on `volar-weaver`; this `no_std` crypto
//! crate stays dependency-light and consumes the already-tagged trace).

use alloc::vec::Vec;

use volar_discipline::{NonZk, Tagged, Transparent};

use crate::ivc::{prove_gap, GapProof, Step};
use crate::pedersen::PedersenParams;
use crate::r1cs::R1CS;
use crate::scalar::Scalar;
use crate::verify::native_verify;

/// Build the R1CS for **one** VOLE AND-gate verifier check
/// `K_a · K_b + V̂ = K_c · Δ`.
///
/// Witness layout `W` (length 7), with `u = z[7]` the relaxation/constant column:
///
/// | idx | 0   | 1   | 2   | 3 | 4   | 5            | 6            |
/// |-----|-----|-----|-----|---|-----|--------------|--------------|
/// | var | K_a | K_b | K_c | Δ | V̂   | P₁ = K_a·K_b | P₂ = K_c·Δ   |
///
/// Constraints:
/// 1. `K_a · K_b = P₁`
/// 2. `K_c · Δ   = P₂`
/// 3. `(P₁ + V̂ − P₂) · u = 0`  ⇔  `K_a·K_b + V̂ = K_c·Δ` (since `u = 1` fresh).
pub fn and_check_r1cs() -> R1CS {
    let one = Scalar::ONE;
    let neg_one = Scalar::ONE.neg();
    // column indices
    const K_A: usize = 0;
    const K_B: usize = 1;
    const K_C: usize = 2;
    const DELTA: usize = 3;
    const V_HAT: usize = 4;
    const P1: usize = 5;
    const P2: usize = 6;
    const U: usize = 7;
    R1CS {
        num_cons: 3,
        num_vars: 8, // 7 witness columns + the `u` column
        a: alloc::vec![
            (0, K_A, one),
            (1, K_C, one),
            (2, P1, one),
            (2, V_HAT, one),
            (2, P2, neg_one),
        ],
        b: alloc::vec![
            (0, K_B, one),
            (1, DELTA, one),
            (2, U, one),
        ],
        c: alloc::vec![
            (0, P1, one),
            (1, P2, one),
            // constraint 3 has C-row 0 (the check is `… = 0`)
        ],
    }
}

/// One verifier step: the verifier-side wire values of a single AND gate, plus
/// the public-coin folding challenges (passed explicitly for determinism, as in
/// [`crate::ivc::Step`]).
#[derive(Clone)]
pub struct VerifierStep {
    /// Verifier MAC share of the left input wire, `K_a`.
    pub k_a: Scalar,
    /// Verifier MAC share of the right input wire, `K_b`.
    pub k_b: Scalar,
    /// Verifier MAC share of the output wire, `K_c`.
    pub k_c: Scalar,
    /// The verifier's global secret Δ.
    pub delta: Scalar,
    /// The prover-sent gate opening `V̂`.
    pub v_hat: Scalar,
    /// Witness-commitment blinder for this step.
    pub r_w: Scalar,
    /// Folding challenge `r`.
    pub r: Scalar,
    /// Cross-term challenge `r_t`.
    pub r_t: Scalar,
}

impl VerifierStep {
    /// Assemble this step's satisfying R1CS assignment for [`and_check_r1cs`].
    fn to_fold_step(&self) -> Step {
        let p1 = self.k_a.mul(&self.k_b);
        let p2 = self.k_c.mul(&self.delta);
        Step {
            w: alloc::vec![self.k_a, self.k_b, self.k_c, self.delta, self.v_hat, p1, p2],
            r_w: self.r_w,
            r: self.r,
            r_t: self.r_t,
        }
    }

    /// Whether the verifier check `K_a·K_b + V̂ = K_c·Δ` holds for this step.
    pub fn checks(&self) -> bool {
        self.k_a.mul(&self.k_b).add(&self.v_hat) == self.k_c.mul(&self.delta)
    }
}

/// The whole-verifier trace plus its carried memory-accumulator boundary.
///
/// Produced by the arithmetization frontend (build-pipeline side) from a woven
/// verifier `IrModule`.  `mem_acc_in` / `mem_acc_out` are the multiset-hash
/// memory accumulator at the start / end of the verifier run; they are committed
/// as the folding boundaries.
#[derive(Clone)]
pub struct VerifierTrace {
    /// Per-AND-gate verifier steps, in evaluation order.
    pub steps: Vec<VerifierStep>,
    /// Memory accumulator at the start of the run.
    pub mem_acc_in: Vec<Scalar>,
    /// Memory accumulator at the end of the run.
    pub mem_acc_out: Vec<Scalar>,
    /// Boundary blinder for `mem_acc_in`.
    pub r_in: Scalar,
    /// Boundary blinder for `mem_acc_out`.
    pub r_out: Scalar,
}

/// A folded proof of the whole verifier: one relaxed-R1CS instance (+ its opened
/// witness for native verification) and the committed memory boundaries.
pub struct VerifierFold {
    /// The per-step relation that was folded.
    pub r1cs: R1CS,
    /// The folded gap proof (final instance/witness + boundary commitments).
    pub gap: GapProof,
}

/// Fold the **whole** verifier into one relaxed-R1CS instance, carrying the
/// memory accumulator across folds as the committed boundary.
///
/// Bound `where Z: NonZk`: a [`volar_discipline::Zk`] artifact cannot be folded
/// here (compile error — mixing ZK and non-ZK proving is unsafe).  The result is
/// [`Transparent`]: no zkSNARK, because the inner VOLE proof already accounts for
/// zero-knowledge.
///
/// # Panics
/// Panics if the trace has no steps (matching [`crate::ivc::prove_gap`]).
pub fn prove_verifier<Z: NonZk>(
    verifier: Tagged<Z, VerifierTrace>,
    params: &PedersenParams,
) -> Tagged<Transparent, VerifierFold> {
    let trace = verifier.into_inner();
    let r1cs = and_check_r1cs();
    let steps: Vec<Step> = trace.steps.iter().map(VerifierStep::to_fold_step).collect();
    let gap = prove_gap(
        &r1cs,
        params,
        &steps,
        &trace.mem_acc_in,
        &trace.r_in,
        &trace.mem_acc_out,
        &trace.r_out,
    );
    Tagged::seal(VerifierFold { r1cs, gap })
}

/// Natively verify a folded verifier (the "Nova minus the zkSNARK" final check).
///
/// Takes a [`Transparent`] fold by construction — the type system guarantees we
/// never feed a ZK artifact to the transparent verifier.
pub fn verify_folded(fold: &Tagged<Transparent, VerifierFold>, params: &PedersenParams) -> bool {
    let vf = fold.inner();
    native_verify(&vf.r1cs, params, &vf.gap.final_u, &vf.gap.final_w)
}

/// **Future seam**: compress the folded verifier with a *regular* (non-ZK) SNARK.
///
/// Gated `where Z: NonZk` so a ZK instance can never be compressed by a
/// transparent/regular SNARK.  Not yet implemented — the native check
/// ([`verify_folded`]) is the current terminal.
pub fn compress_with_snark<Z: NonZk>(_fold: &Tagged<Z, VerifierFold>, _params: &PedersenParams) -> ! {
    unimplemented!("regular (non-ZK) SNARK compression of the folded verifier — future work")
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use alloc::vec;
    use alloc::vec::Vec;
    use volar_discipline::Transparent;

    /// Build an honest verifier step: choose `K_a, K_b, K_c, Δ` and set
    /// `V̂ = K_c·Δ − K_a·K_b` so the check passes.
    fn honest_step(a: u64, b: u64, c: u64, delta: u64, seed: u64) -> VerifierStep {
        let k_a = Scalar::from_u64(a);
        let k_b = Scalar::from_u64(b);
        let k_c = Scalar::from_u64(c);
        let d = Scalar::from_u64(delta);
        let v_hat = k_c.mul(&d).sub(&k_a.mul(&k_b));
        VerifierStep {
            k_a, k_b, k_c, delta: d, v_hat,
            r_w: Scalar::from_u64(seed.wrapping_mul(7).wrapping_add(1)),
            r: Scalar::from_u64(seed.wrapping_mul(13).wrapping_add(3)),
            r_t: Scalar::from_u64(seed.wrapping_mul(17).wrapping_add(5)),
        }
    }

    fn trace(steps: Vec<VerifierStep>) -> Tagged<Transparent, VerifierTrace> {
        Tagged::seal(VerifierTrace {
            steps,
            mem_acc_in: vec![Scalar::from_u64(1)],
            mem_acc_out: vec![Scalar::from_u64(99)],
            r_in: Scalar::from_u64(2),
            r_out: Scalar::from_u64(3),
        })
    }

    #[test]
    fn and_check_r1cs_accepts_honest_step() {
        let r1cs = and_check_r1cs();
        let s = honest_step(3, 4, 5, 6, 1);
        assert!(s.checks());
        let st = s.to_fold_step();
        // u = 1, E = 0 for a fresh honest step.
        let e = vec![Scalar::ZERO; r1cs.num_cons];
        assert!(r1cs.is_satisfied_relaxed(&st.w, &e, &Scalar::ONE));
    }

    #[test]
    fn whole_verifier_folds_to_one_native_verifiable_instance() {
        let params = PedersenParams::setup(8, 41);
        let steps: Vec<VerifierStep> = (1..=10u64)
            .map(|i| honest_step(i, i + 2, i + 5, i + 1, i))
            .collect();
        let fold = prove_verifier(trace(steps), &params);
        // The whole verifier collapses to ONE relaxed-R1CS instance...
        assert_eq!(fold.inner().gap.steps, 10);
        // ...that verifies natively (no zkSNARK).
        assert!(verify_folded(&fold, &params));
        // Memory boundaries are committed and distinct.
        assert_ne!(fold.inner().gap.c_in, fold.inner().gap.c_out);
    }

    #[test]
    fn dishonest_verifier_step_is_rejected() {
        let params = PedersenParams::setup(8, 41);
        let mut steps: Vec<VerifierStep> = (1..=6u64)
            .map(|i| honest_step(i, i + 2, i + 5, i + 1, i))
            .collect();
        // Tamper one step's V̂ so the AND check no longer holds.
        steps[3].v_hat = steps[3].v_hat.add(&Scalar::ONE);
        assert!(!steps[3].checks());
        let fold = prove_verifier(trace(steps), &params);
        assert!(!verify_folded(&fold, &params), "a lying verifier step must fail");
    }

    #[test]
    fn output_discipline_is_transparent() {
        let params = PedersenParams::setup(8, 41);
        let fold = prove_verifier(trace(vec![honest_step(2, 3, 4, 5, 1)]), &params);
        assert_eq!(fold.discipline(), volar_discipline::Discipline::Transparent);
    }
}

/// Compile-time proof that a ZK artifact cannot be folded by the transparent
/// prove-the-verifier path (mixing ZK and non-ZK proving is rejected by the
/// `Z: NonZk` bound):
///
/// ```compile_fail
/// use volar_fold::verifier::{prove_verifier, VerifierTrace};
/// use volar_fold::pedersen::PedersenParams;
/// use volar_fold::scalar::Scalar;
/// use volar_discipline::{Tagged, Zk};
///
/// // Doctests link `std`, so the `vec!`/`Vec` prelude is available here.
/// let trace: Tagged<Zk, VerifierTrace> = Tagged::seal(VerifierTrace {
///     steps: vec![], mem_acc_in: vec![], mem_acc_out: vec![],
///     r_in: Scalar::ZERO, r_out: Scalar::ZERO,
/// });
/// // `Zk: NonZk` is unsatisfied → this does not compile:
/// let _ = prove_verifier(trace, &PedersenParams::setup(8, 1));
/// ```
#[cfg(doctest)]
struct ZkCannotBeFolded;
