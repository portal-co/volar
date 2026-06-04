// @reliability: experimental
// @ai: assisted
//! **Native interactive verification** of a folded relaxed-R1CS instance — the
//! "Nova minus the zkSNARK" final check.
//!
//! Rather than compressing the accumulated instance with a succinct
//! non-interactive argument, the prover **opens** the folded witness `(W, E)` and
//! the verifier checks, in `O(|F|)`:
//! 1. the commitments open correctly (`Commit(W) = comm_W`, `Commit(E) = comm_E`),
//! 2. the relaxed relation `(A z) ∘ (B z) = u·(C z) + E` holds (`z = [W ‖ u]`).
//!
//! This is sound because folding is knowledge-sound: a satisfying opening of the
//! final folded instance implies satisfying openings of every folded step
//! (Nova's folding theorem).  It is *interactive / non-succinct in proof size*
//! (the opening is `O(|F|)`), which is exactly the accepted trade-off here —
//! online bandwidth is cheap, and the win is **independence from the gap length
//! `m`** (the folded instance is one R1CS regardless of how many steps folded
//! into it).

use crate::nifs::commit_witness;
use crate::pedersen::PedersenParams;
use crate::r1cs::{R1CS, RelaxedInstance, RelaxedWitness};

/// Verify a final folded instance against its opened witness.  Returns `true`
/// iff the commitments open and the relaxed relation holds.
pub fn native_verify(
    r1cs: &R1CS,
    params: &PedersenParams,
    inst: &RelaxedInstance,
    wit: &RelaxedWitness,
) -> bool {
    // 1. commitment openings
    let (cw, ce) = commit_witness(params, wit);
    if cw != inst.comm_w || ce != inst.comm_e {
        return false;
    }
    // 2. relaxed R1CS relation
    r1cs.is_satisfied_relaxed(&wit.w, &wit.e, &inst.u)
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::nifs::{fresh, prove_fold};
    use crate::scalar::Scalar;
    use alloc::vec;

    fn mul_gate() -> R1CS {
        R1CS {
            num_cons: 1,
            num_vars: 4,
            a: vec![(0, 0, Scalar::ONE)],
            b: vec![(0, 1, Scalar::ONE)],
            c: vec![(0, 2, Scalar::ONE)],
        }
    }

    fn folded() -> (R1CS, PedersenParams, RelaxedInstance, RelaxedWitness) {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 5);
        let (u1, w1) = fresh(&r1cs, &params, &[Scalar::from_u64(3), Scalar::from_u64(4), Scalar::from_u64(12)], Scalar::from_u64(11));
        let (u2, w2) = fresh(&r1cs, &params, &[Scalar::from_u64(5), Scalar::from_u64(6), Scalar::from_u64(30)], Scalar::from_u64(13));
        let (uf, wf, _) = prove_fold(&r1cs, &params, &u1, &w1, &u2, &w2, &Scalar::from_u64(77), &Scalar::from_u64(88));
        (r1cs, params, uf, wf)
    }

    #[test]
    fn accepts_honest() {
        let (r1cs, params, inst, wit) = folded();
        assert!(native_verify(&r1cs, &params, &inst, &wit));
    }

    #[test]
    fn rejects_tampered_witness() {
        let (r1cs, params, inst, mut wit) = folded();
        wit.w[0] = wit.w[0].add(&Scalar::ONE); // opening no longer matches comm_W
        assert!(!native_verify(&r1cs, &params, &inst, &wit));
    }

    #[test]
    fn rejects_tampered_error() {
        let (r1cs, params, inst, mut wit) = folded();
        wit.e[0] = wit.e[0].add(&Scalar::ONE);
        assert!(!native_verify(&r1cs, &params, &inst, &wit));
    }

    #[test]
    fn rejects_tampered_instance_u() {
        let (r1cs, params, mut inst, wit) = folded();
        inst.u = inst.u.add(&Scalar::ONE); // relation breaks
        assert!(!native_verify(&r1cs, &params, &inst, &wit));
    }
}
