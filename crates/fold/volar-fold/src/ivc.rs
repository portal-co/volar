// @reliability: experimental
// @ai: assisted
//! The **gap-folding driver** + boundary commitments.
//!
//! During a network gap the prover computes `S_{k+1} … S_{k+m} = F^m(S_k)` in the
//! clear; for each step it builds a fresh R1CS instance witnessing
//! `F(S_{t-1}) = S_t` and folds it into a running relaxed instance (consuming **no
//! VOLE correlations**).  After `m` steps the accumulator is **one** R1CS instance
//! of size `O(|F|)` — independent of `m`.  The boundary states `S_k` and
//! `S_{k+m}` are separately Pedersen-committed as `C_in` / `C_out`; the bridge
//! then links those commitments to the VOLE-authenticated anchor / re-commit via
//! a [`crate::link::BoundaryLink`].
//!
//! ## Continuity (honest scope, v1)
//! This driver folds the per-step `F`-instances and commits the boundaries; it
//! assumes the caller chains step witnesses so step `t`'s output state equals
//! step `t+1`'s input state.  A production IVC additionally enforces that
//! continuity with an in-circuit constraint (Nova's *augmented* circuit); that
//! refinement is noted in `docs/vcb-ivc-folding.md` and does not change this API.

use alloc::vec::Vec;

use volar_spec::curve::EdPoint;

use crate::nifs::{fresh, prove_fold};
use crate::pedersen::PedersenParams;
use crate::r1cs::{R1CS, RelaxedInstance, RelaxedWitness};
use crate::scalar::Scalar;

/// One gap step: the satisfying assignment `w` for `F(S_{t-1}) = S_t` plus its
/// witness-commitment blinder `r_w`, and the folding challenges `(r, r_t)`
/// (public-coin in the protocol; passed explicitly here for determinism/testing).
#[derive(Clone)]
pub struct Step {
    pub w: Vec<Scalar>,
    pub r_w: Scalar,
    pub r: Scalar,
    pub r_t: Scalar,
}

/// The output of folding a whole gap.
pub struct GapProof {
    pub final_u: RelaxedInstance,
    /// Opened for native verification (see [`crate::verify`]).
    pub final_w: RelaxedWitness,
    pub c_in: EdPoint,
    pub c_out: EdPoint,
    pub steps: usize,
}

/// Commit a boundary state vector: `Commit(state; blind)`.
pub fn commit_state(params: &PedersenParams, state: &[Scalar], blind: &Scalar) -> EdPoint {
    params.commit(state, blind)
}

/// Streaming counterpart to [`prove_gap`]: fold one [`Step`] at a time instead of
/// requiring the whole `&[Step]` slice materialized up front. `push` never grows —
/// the accumulator stays a single `(RelaxedInstance, RelaxedWitness)` pair
/// regardless of how many steps have been pushed, so a caller driving this from a
/// loop of unknown (or hidden) length gets a fold that stays succinct.
#[derive(Default)]
pub struct GapAccumulator {
    acc: Option<(RelaxedInstance, RelaxedWitness)>,
    steps: usize,
}

impl GapAccumulator {
    pub fn new() -> Self {
        Self {
            acc: None,
            steps: 0,
        }
    }

    /// Fold in one more step: `fresh()` for the first push, `prove_fold()`
    /// against the running accumulator thereafter.
    pub fn push(&mut self, r1cs: &R1CS, params: &PedersenParams, step: &Step) {
        self.acc = Some(match self.acc.take() {
            None => fresh(r1cs, params, &step.w, step.r_w),
            Some((acc_u, acc_w)) => {
                let (su, sw) = fresh(r1cs, params, &step.w, step.r_w);
                let (uf, wf, _) =
                    prove_fold(r1cs, params, &acc_u, &acc_w, &su, &sw, &step.r, &step.r_t);
                (uf, wf)
            }
        });
        self.steps += 1;
    }

    /// Commit the boundary states and assemble the final [`GapProof`].
    ///
    /// # Panics
    /// Panics if no step has been [`push`](Self::push)ed.
    pub fn finish(
        self,
        params: &PedersenParams,
        s_in: &[Scalar],
        r_in: &Scalar,
        s_out: &[Scalar],
        r_out: &Scalar,
    ) -> GapProof {
        let (final_u, final_w) = self
            .acc
            .expect("GapAccumulator::finish: need at least one push");
        GapProof {
            final_u,
            final_w,
            c_in: commit_state(params, s_in, r_in),
            c_out: commit_state(params, s_out, r_out),
            steps: self.steps,
        }
    }
}

/// Fold all gap steps into one relaxed instance and commit the boundaries.
///
/// `s_in` / `s_out` are the boundary state vectors (`S_k` / `S_{k+m}`) with
/// blinders `r_in` / `r_out`.  Requires at least one step.
///
/// Thin wrapper over [`GapAccumulator`] — kept as the batch entry point for
/// callers that already have the whole step slice; [`GapAccumulator`] is the one
/// to use when steps arrive one at a time (e.g. from a running loop).
///
/// # Why this discards [`crate::nifs::FoldProof`] (intentional)
///
/// Each step's [`crate::nifs::FoldProof`] (from `prove_fold`) is dropped here
/// (`let (uf, wf, _) = prove_fold(..)`) — `GapProof` has no field for it.
/// `FoldProof` is consumed only by [`crate::nifs::verify_fold`], an
/// instance-only, no-witness reconstruction of the folded instance; this
/// design instead has the verifier check the folded instance natively, on the
/// *opened* witness (see [`crate::verify::native_verify`]), so `verify_fold`
/// is never called outside its own unit tests. `tests/fold_proof_parity.rs`
/// confirms this is a no-loss simplification for this design specifically:
/// reconstructing the instance-only leg by keeping every `FoldProof` and
/// folding via `verify_fold` instead always agrees with this function's
/// output, bit-for-bit, on both honest *and* dishonest step chains — the
/// discarded `FoldProof`s carry no information `native_verify` doesn't
/// already provide here. `FoldProof`/`verify_fold` remain as tested-but-unwired
/// scaffolding for a possible future instance-only verification mode.
#[allow(clippy::too_many_arguments)]
pub fn prove_gap(
    r1cs: &R1CS,
    params: &PedersenParams,
    steps: &[Step],
    s_in: &[Scalar],
    r_in: &Scalar,
    s_out: &[Scalar],
    r_out: &Scalar,
) -> GapProof {
    assert!(!steps.is_empty(), "prove_gap: need at least one step");
    let mut acc = GapAccumulator::new();
    for st in steps {
        acc.push(r1cs, params, st);
    }
    acc.finish(params, s_in, r_in, s_out, r_out)
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::verify::native_verify;
    use alloc::vec;
    use alloc::vec::Vec;

    fn mul_gate() -> R1CS {
        R1CS {
            num_cons: 1,
            num_vars: 4,
            a: vec![(0, 0, Scalar::ONE)],
            b: vec![(0, 1, Scalar::ONE)],
            c: vec![(0, 2, Scalar::ONE)],
        }
    }

    fn step(a: u64, b: u64, c: u64, seed: u64) -> Step {
        Step {
            w: vec![
                Scalar::from_u64(a),
                Scalar::from_u64(b),
                Scalar::from_u64(c),
            ],
            r_w: Scalar::from_u64(seed.wrapping_mul(7).wrapping_add(1)),
            r: Scalar::from_u64(seed.wrapping_mul(13).wrapping_add(3)),
            r_t: Scalar::from_u64(seed.wrapping_mul(17).wrapping_add(5)),
        }
    }

    #[test]
    fn gap_of_m_steps_folds_to_one_verifiable_instance() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 31);
        // m = 8 honest steps (a·b = c).
        let steps: Vec<Step> = (1..=8u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        let s_in = vec![Scalar::from_u64(1)];
        let s_out = vec![Scalar::from_u64(99)];
        let gp = prove_gap(
            &r1cs,
            &params,
            &steps,
            &s_in,
            &Scalar::from_u64(2),
            &s_out,
            &Scalar::from_u64(3),
        );
        assert_eq!(gp.steps, 8);
        // The folded instance verifies natively — and is ONE R1CS regardless of m.
        assert!(native_verify(&r1cs, &params, &gp.final_u, &gp.final_w));
        // Boundary commitments are present and distinct.
        assert_ne!(gp.c_in, gp.c_out);
    }

    #[test]
    fn unsatisfying_step_is_rejected() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 31);
        // Step 3 is a lie: 3·4 ≠ 99.
        let mut steps: Vec<Step> = (1..=5u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        steps[2] = step(3, 4, 99, 3);
        let s_in = vec![Scalar::from_u64(1)];
        let s_out = vec![Scalar::from_u64(2)];
        let gp = prove_gap(
            &r1cs,
            &params,
            &steps,
            &s_in,
            &Scalar::from_u64(2),
            &s_out,
            &Scalar::from_u64(3),
        );
        assert!(
            !native_verify(&r1cs, &params, &gp.final_u, &gp.final_w),
            "lying step must fail"
        );
    }

    #[test]
    fn gap_accumulator_matches_prove_gap() {
        // Pushing steps one at a time into a GapAccumulator (as a streaming
        // caller would) must produce the same GapProof as batch prove_gap over
        // the same steps — the accumulator never grows past a single
        // (RelaxedInstance, RelaxedWitness) pair regardless of how many steps
        // are pushed.
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 31);
        let steps: Vec<Step> = (1..=8u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        let s_in = vec![Scalar::from_u64(1)];
        let s_out = vec![Scalar::from_u64(99)];
        let r_in = Scalar::from_u64(2);
        let r_out = Scalar::from_u64(3);

        let batch = prove_gap(&r1cs, &params, &steps, &s_in, &r_in, &s_out, &r_out);

        let mut acc = GapAccumulator::new();
        for st in &steps {
            acc.push(&r1cs, &params, st);
        }
        let streamed = acc.finish(&params, &s_in, &r_in, &s_out, &r_out);

        assert_eq!(streamed.steps, batch.steps);
        assert_eq!(streamed.final_u.comm_w, batch.final_u.comm_w);
        assert_eq!(streamed.final_u.comm_e, batch.final_u.comm_e);
        assert_eq!(streamed.final_u.u, batch.final_u.u);
        assert_eq!(streamed.c_in, batch.c_in);
        assert_eq!(streamed.c_out, batch.c_out);
        assert!(native_verify(
            &r1cs,
            &params,
            &streamed.final_u,
            &streamed.final_w
        ));
    }

    #[test]
    #[should_panic(expected = "GapAccumulator::finish: need at least one push")]
    fn gap_accumulator_finish_without_push_panics() {
        let params = PedersenParams::setup(4, 31);
        let s_in = vec![Scalar::from_u64(1)];
        let s_out = vec![Scalar::from_u64(2)];
        let acc = GapAccumulator::new();
        let _ = acc.finish(
            &params,
            &s_in,
            &Scalar::from_u64(2),
            &s_out,
            &Scalar::from_u64(3),
        );
    }
}
