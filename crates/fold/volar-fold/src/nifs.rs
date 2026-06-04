// @reliability: experimental
// @ai: assisted
//! The Nova **non-interactive folding step** (NIFS).
//!
//! Folds two relaxed-R1CS instance/witness pairs `(U1, W1)`, `(U2, W2)` for the
//! same R1CS into a single `(U, W)` using a public-coin challenge `r`:
//!
//! ```text
//! T        = A z1 ∘ B z2 + A z2 ∘ B z1 − u1·(C z2) − u2·(C z1)   (cross term)
//! comm_T   = Commit(T; r_T)
//! W = W1 + r·W2          E = E1 + r·T + r²·E2
//! u = u1 + r·u2          comm_W = comm_W1 + r·comm_W2
//!                        comm_E = comm_E1 + r·comm_T + r²·comm_E2
//! ```
//!
//! The verifier folds the **instance** (commitments + `u`) from `comm_T` and `r`
//! alone; the prover additionally folds the witness.  If both inputs satisfy the
//! relaxed relation, so does the fold (NIFS correctness).

use alloc::vec::Vec;

use volar_spec::curve::{ed_add, EdPoint};

use crate::pedersen::{scalar_mul, PedersenParams};
use crate::r1cs::{R1CS, RelaxedInstance, RelaxedWitness};
use crate::scalar::Scalar;

/// What the prover sends so the verifier can fold the instance: the commitment
/// to the cross term.
#[derive(Clone)]
pub struct FoldProof {
    pub comm_t: EdPoint,
}

fn vec_add(a: &[Scalar], b: &[Scalar]) -> Vec<Scalar> {
    a.iter().zip(b.iter()).map(|(x, y)| x.add(y)).collect()
}
fn vec_scale(a: &[Scalar], k: &Scalar) -> Vec<Scalar> {
    a.iter().map(|x| x.mul(k)).collect()
}

/// Commit a relaxed witness: `comm_W = Commit(W; r_W)`, `comm_E = Commit(E; r_E)`.
pub fn commit_witness(params: &PedersenParams, w: &RelaxedWitness) -> (EdPoint, EdPoint) {
    (params.commit(&w.w, &w.r_w), params.commit(&w.e, &w.r_e))
}

/// Build a **fresh** relaxed instance/witness (`u = 1`, `E = 0`) from a satisfying
/// assignment `w` (length `num_vars − 1`) with blinder `r_w`.
pub fn fresh(
    r1cs: &R1CS,
    params: &PedersenParams,
    w: &[Scalar],
    r_w: Scalar,
) -> (RelaxedInstance, RelaxedWitness) {
    let e = alloc::vec![Scalar::ZERO; r1cs.num_cons];
    let witness = RelaxedWitness { w: w.to_vec(), e, r_w, r_e: Scalar::ZERO };
    let (comm_w, comm_e) = commit_witness(params, &witness);
    let inst = RelaxedInstance { comm_w, comm_e, u: Scalar::ONE };
    (inst, witness)
}

/// Compute the cross term `T` (length `num_cons`).
fn cross_term(
    r1cs: &R1CS,
    w1: &RelaxedWitness,
    u1: &Scalar,
    w2: &RelaxedWitness,
    u2: &Scalar,
) -> Vec<Scalar> {
    let z1 = r1cs.full_z(&w1.w, u1);
    let z2 = r1cs.full_z(&w2.w, u2);
    let (az1, bz1, cz1) = r1cs.eval_abc(&z1);
    let (az2, bz2, cz2) = r1cs.eval_abc(&z2);
    (0..r1cs.num_cons)
        .map(|i| {
            let cross = az1[i].mul(&bz2[i]).add(&az2[i].mul(&bz1[i]));
            let sub = u1.mul(&cz2[i]).add(&u2.mul(&cz1[i]));
            cross.sub(&sub)
        })
        .collect()
}

/// **Prover** fold: returns the folded instance + witness and the `FoldProof`.
#[allow(clippy::too_many_arguments)]
pub fn prove_fold(
    r1cs: &R1CS,
    params: &PedersenParams,
    u1: &RelaxedInstance,
    w1: &RelaxedWitness,
    u2: &RelaxedInstance,
    w2: &RelaxedWitness,
    r: &Scalar,
    r_t: &Scalar,
) -> (RelaxedInstance, RelaxedWitness, FoldProof) {
    let t = cross_term(r1cs, w1, &u1.u, w2, &u2.u);
    let comm_t = params.commit(&t, r_t);
    let r2 = r.mul(r);

    // Witness fold.
    let w = vec_add(&w1.w, &vec_scale(&w2.w, r));
    let e = vec_add(&vec_add(&w1.e, &vec_scale(&t, r)), &vec_scale(&w2.e, &r2));
    let r_w = w1.r_w.add(&r.mul(&w2.r_w));
    let r_e = w1.r_e.add(&r.mul(r_t)).add(&r2.mul(&w2.r_e));
    let folded_w = RelaxedWitness { w, e, r_w, r_e };

    // Instance fold (must match `verify_fold`).
    let folded_u = verify_fold(u1, u2, &FoldProof { comm_t }, r);
    (folded_u, folded_w, FoldProof { comm_t })
}

/// **Verifier** fold of the instance alone.
pub fn verify_fold(
    u1: &RelaxedInstance,
    u2: &RelaxedInstance,
    proof: &FoldProof,
    r: &Scalar,
) -> RelaxedInstance {
    let r2 = r.mul(r);
    let comm_w = ed_add(&u1.comm_w, &scalar_mul(&u2.comm_w, r));
    let comm_e = ed_add(
        &ed_add(&u1.comm_e, &scalar_mul(&proof.comm_t, r)),
        &scalar_mul(&u2.comm_e, &r2),
    );
    let u = u1.u.add(&r.mul(&u2.u));
    RelaxedInstance { comm_w, comm_e, u }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::r1cs::R1CS;
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

    #[test]
    fn fold_of_two_fresh_is_relaxed_satisfied() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 7);
        // Two satisfying assignments: 3·4=12 and 5·6=30.
        let (u1, w1) = fresh(&r1cs, &params, &[Scalar::from_u64(3), Scalar::from_u64(4), Scalar::from_u64(12)], Scalar::from_u64(11));
        let (u2, w2) = fresh(&r1cs, &params, &[Scalar::from_u64(5), Scalar::from_u64(6), Scalar::from_u64(30)], Scalar::from_u64(13));

        let r = Scalar::from_u64(0xabcd);
        let r_t = Scalar::from_u64(0x1357);
        let (uf, wf, proof) = prove_fold(&r1cs, &params, &u1, &w1, &u2, &w2, &r, &r_t);

        // Algebraic relaxed relation holds.
        assert!(r1cs.is_satisfied_relaxed(&wf.w, &wf.e, &uf.u), "folded relation");
        // Prover instance == independent verifier fold.
        let uv = verify_fold(&u1, &u2, &proof, &r);
        assert_eq!(uf.comm_w, uv.comm_w);
        assert_eq!(uf.comm_e, uv.comm_e);
        assert_eq!(uf.u, uv.u);
        // Commitments are consistent with the folded witness.
        let (cw, ce) = commit_witness(&params, &wf);
        assert_eq!(cw, uf.comm_w, "comm_W matches folded witness");
        assert_eq!(ce, uf.comm_e, "comm_E matches folded witness");
    }

    #[test]
    fn fold_chain_three_steps() {
        // Fold three fresh instances sequentially; the running instance stays
        // relaxed-satisfied and commitment-consistent.
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 99);
        let mk = |a: u64, b: u64, rw: u64| {
            fresh(&r1cs, &params, &[Scalar::from_u64(a), Scalar::from_u64(b), Scalar::from_u64(a * b)], Scalar::from_u64(rw))
        };
        let (mut acc_u, mut acc_w) = mk(2, 3, 1);
        for (a, b, rw, rr, rt) in [(4u64, 5u64, 2u64, 7u64, 8u64), (6, 7, 3, 9, 10)] {
            let (u2, w2) = mk(a, b, rw);
            let r = Scalar::from_u64(rr);
            let (uf, wf, _) = prove_fold(&r1cs, &params, &acc_u, &acc_w, &u2, &w2, &r, &Scalar::from_u64(rt));
            acc_u = uf;
            acc_w = wf;
        }
        assert!(r1cs.is_satisfied_relaxed(&acc_w.w, &acc_w.e, &acc_u.u));
        let (cw, ce) = commit_witness(&params, &acc_w);
        assert_eq!(cw, acc_u.comm_w);
        assert_eq!(ce, acc_u.comm_e);
    }
}
