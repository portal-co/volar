// @reliability: experimental
//! Completeness check for whether `prove_gap` dropping the per-step
//! [`FoldProof`] is intentional (see `ivc::prove_gap`'s doc comment).
//!
//! `FoldProof` (from `nifs::prove_fold`) is consumed only by `nifs::verify_fold`,
//! an **instance-only, no-witness** reconstruction of the folded instance from
//! `(comm_T, r)` alone. `prove_gap` discards it every step (`let (uf, wf, _) =
//! prove_fold(..)`), and `GapProof` has no field for it. This file reconstructs
//! the discarded leg independently — keeping every `FoldProof` and folding via
//! `verify_fold` instead of `prove_fold`'s internal instance fold — and checks
//! two things:
//!
//! 1. The reconstructed instance-only leg agrees with `prove_gap`'s own output,
//!    bit-for-bit, on **honest** chains (completeness).
//! 2. It *also* agrees bit-for-bit on **dishonest** chains — `verify_fold` never
//!    disagrees with `prove_fold`'s instance fold, because both compute the same
//!    formula from `(comm_T, r)`; only `native_verify` (which opens the witness)
//!    actually catches a lie in this design.
//!
//! Together these support the "intentional, not a soundness gap" reading:
//! `FoldProof`/`verify_fold` give a verifier no information `native_verify`
//! doesn't already provide in the current native-open-witness design — dropping
//! `FoldProof` from `GapProof`'s output costs nothing here.

use proptest::prelude::*;
use volar_fold::ivc::{prove_gap, GapAccumulator, Step};
use volar_fold::nifs::{fresh, prove_fold, verify_fold};
use volar_fold::pedersen::PedersenParams;
use volar_fold::r1cs::{RelaxedInstance, R1CS};
use volar_fold::scalar::Scalar;
use volar_fold::verify::native_verify;

fn s(x: u64) -> Scalar {
    Scalar::from_u64(x)
}

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
        w: vec![s(a), s(b), s(c)],
        r_w: s(seed.wrapping_mul(7).wrapping_add(1)),
        r: s(seed.wrapping_mul(13).wrapping_add(3)),
        r_t: s(seed.wrapping_mul(17).wrapping_add(5)),
    }
}

fn assert_instances_eq(a: &RelaxedInstance, b: &RelaxedInstance, msg: &str) {
    assert_eq!(a.comm_w, b.comm_w, "{msg}: comm_w");
    assert_eq!(a.comm_e, b.comm_e, "{msg}: comm_e");
    assert_eq!(a.u, b.u, "{msg}: u");
}

/// Replay a step chain, tracking **two independent** running instances:
/// `batch_u` (evolved via `prove_fold`'s returned instance — exactly what
/// `prove_gap`/`GapAccumulator` do) and `recon_u` (evolved via `verify_fold`
/// alone, from the `FoldProof`s `prove_gap` throws away). Both consume the same
/// per-step `proof`/fresh instance, but `recon_u` never reads `batch_u` after
/// step 0 — it's a genuinely separate reconstruction, not a telescoping
/// tautology. Returns `recon_u`, to be compared against `prove_gap`'s
/// `final_u` (i.e. `batch_u`).
fn reconstruct_instance_only_leg(r1cs: &R1CS, params: &PedersenParams, steps: &[Step]) -> RelaxedInstance {
    assert!(!steps.is_empty());
    let (mut batch_u, mut acc_w) = fresh(r1cs, params, &steps[0].w, steps[0].r_w);
    let mut recon_u = batch_u.clone();
    for st in &steps[1..] {
        let (su, sw) = fresh(r1cs, params, &st.w, st.r_w);
        let (uf, wf, proof) = prove_fold(r1cs, params, &batch_u, &acc_w, &su, &sw, &st.r, &st.r_t);
        recon_u = verify_fold(&recon_u, &su, &proof, &st.r);
        batch_u = uf;
        acc_w = wf;
    }
    recon_u
}

#[test]
fn discarded_foldproofs_reconstruct_the_same_final_instance_as_prove_gap() {
    let r1cs = mul_gate();
    let params = PedersenParams::setup(4, 31);
    let steps: Vec<Step> = (1..=8u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
    let s_in = vec![s(1)];
    let s_out = vec![s(99)];
    let r_in = s(2);
    let r_out = s(3);

    let gp = prove_gap(&r1cs, &params, &steps, &s_in, &r_in, &s_out, &r_out);
    assert!(native_verify(&r1cs, &params, &gp.final_u, &gp.final_w), "honest chain verifies");

    let reconstructed = reconstruct_instance_only_leg(&r1cs, &params, &steps);
    assert_instances_eq(
        &reconstructed,
        &gp.final_u,
        "instance-only leg (kept FoldProofs) must match prove_gap's discarded-FoldProof output",
    );
}

#[test]
fn verify_fold_leg_matches_prove_gap_even_on_a_dishonest_chain() {
    // Step 3 is a lie: 3·4 ≠ 99. native_verify (the design's real soundness
    // check) must reject it. The instance-only leg still agrees with
    // prove_gap's instance bit-for-bit regardless — verify_fold recomputes the
    // exact same formula prove_fold's internal instance fold does, honest or
    // not, so it provides no independent soundness signal in this design.
    let r1cs = mul_gate();
    let params = PedersenParams::setup(4, 31);
    let mut steps: Vec<Step> = (1..=5u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
    steps[2] = step(3, 4, 99, 3);
    let s_in = vec![s(1)];
    let s_out = vec![s(2)];
    let r_in = s(2);
    let r_out = s(3);

    let gp = prove_gap(&r1cs, &params, &steps, &s_in, &r_in, &s_out, &r_out);
    assert!(!native_verify(&r1cs, &params, &gp.final_u, &gp.final_w), "lying step must fail native_verify");

    let reconstructed = reconstruct_instance_only_leg(&r1cs, &params, &steps);
    assert_instances_eq(
        &reconstructed,
        &gp.final_u,
        "instance-only leg must match prove_gap's instance even when the chain is dishonest",
    );
}

proptest! {
    // Pedersen MSM makes each case relatively expensive (up to ~6 steps *
    // ~2 folds/step); keep the case/chain-length budget modest so this stays
    // fast enough to run routinely while still covering many chains.
    #![proptest_config(ProptestConfig::with_cases(16))]

    /// Completeness, many random honest chains: for any step count and any
    /// honest `(a,b,c=a*b)` triples with random blinders/challenges, prove_gap
    /// and the reconstructed instance-only leg agree, and the chain verifies
    /// natively.
    #[test]
    fn prop_foldproof_reconstruction_matches_prove_gap_on_honest_chains(
        vals in proptest::collection::vec((1u64..50, 1u64..50, 1u64..1000), 1..6),
    ) {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 31);
        let steps: Vec<Step> = vals.iter()
            .map(|&(a, b, seed)| step(a, b, a * b, seed))
            .collect();
        let s_in = vec![s(1)];
        let s_out = vec![s(2)];
        let r_in = s(2);
        let r_out = s(3);

        let gp = prove_gap(&r1cs, &params, &steps, &s_in, &r_in, &s_out, &r_out);
        prop_assert!(native_verify(&r1cs, &params, &gp.final_u, &gp.final_w));

        let reconstructed = reconstruct_instance_only_leg(&r1cs, &params, &steps);
        prop_assert_eq!(reconstructed.comm_w, gp.final_u.comm_w);
        prop_assert_eq!(reconstructed.comm_e, gp.final_u.comm_e);
        prop_assert_eq!(reconstructed.u, gp.final_u.u);

        // Also: GapAccumulator (the streaming entry point) agrees with prove_gap.
        let mut acc = GapAccumulator::new();
        for st in &steps {
            acc.push(&r1cs, &params, st);
        }
        let streamed = acc.finish(&params, &s_in, &r_in, &s_out, &r_out);
        prop_assert_eq!(streamed.final_u.comm_w, gp.final_u.comm_w);
        prop_assert_eq!(streamed.final_u.comm_e, gp.final_u.comm_e);
        prop_assert_eq!(streamed.final_u.u, gp.final_u.u);
    }
}
