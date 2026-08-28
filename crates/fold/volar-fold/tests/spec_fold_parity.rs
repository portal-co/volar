// @reliability: experimental
//! Cross-test: the field-generic spec fold primitives (`volar_spec::fold`)
//! reproduce the `volar-fold` runtime **oracle** (`nifs::prove_fold`) on the
//! verifier-gate `and_check` R1CS. This is the parity check the plan calls for:
//! the spec primitives (which the verifier weaver will emit + lower to a backend)
//! must agree with the reference folding library bit-for-bit.

use proptest::prelude::*;
use volar_fold::nifs::{fresh, prove_fold};
use volar_fold::pedersen::PedersenParams;
use volar_fold::r1cs::R1CS;
use volar_fold::scalar::Scalar;
use volar_spec::fold as spec;

fn s(x: u64) -> Scalar {
    Scalar::from_u64(x)
}

/// The verifier-gate R1CS `K_a·K_b + V̂ = K_c·Δ` — witness layout
/// `W = [K_a, K_b, K_c, Δ, V̂, P₁, P₂]` (`z = [W ‖ u]`, `spec::AND_VARS`/
/// `spec::AND_CONS` document the same fixed shape on the spec side this
/// test cross-checks against). Kept local to this test: it's the fixed
/// relation `volar_spec::fold` was written to be cross-tested against, not
/// production prove-the-verifier machinery (that now lives natively in
/// `volar-iop::fold::and_check_r1cs`, over a different field).
fn and_check_r1cs() -> R1CS {
    let one = Scalar::ONE;
    let neg_one = one.neg();
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
        num_vars: 8,
        a: vec![
            (0, K_A, one),
            (1, K_C, one),
            (2, P1, one),
            (2, V_HAT, one),
            (2, P2, neg_one),
        ],
        b: vec![(0, K_B, one), (1, DELTA, one), (2, U, one)],
        c: vec![(0, P1, one), (1, P2, one)],
    }
}

/// An honest gate witness `[k_a,k_b,k_c,Δ,V̂,P₁,P₂]` with `V̂ = K_c·Δ − K_a·K_b`,
/// built via the spec helper so `P₁ = K_a·K_b`, `P₂ = K_c·Δ`.
fn gate(k_a: u64, k_b: u64, k_c: u64, delta: u64) -> [Scalar; 7] {
    let v_hat = s(k_c).mul(&s(delta)).sub(&s(k_a).mul(&s(k_b)));
    spec::gate_witness(s(k_a), s(k_b), s(k_c), s(delta), v_hat)
}

#[test]
fn spec_fold_matches_oracle() {
    let r1cs = and_check_r1cs();
    let params = PedersenParams::setup(8, 7);
    let w1 = gate(3, 4, 5, 6);
    let w2 = gate(7, 8, 9, 2);
    let r = s(0xabcd);
    let r_t = s(0x1357);

    // Oracle fold (the reference library).
    let (u1, ow1) = fresh(&r1cs, &params, &w1, s(11));
    let (u2, ow2) = fresh(&r1cs, &params, &w2, s(13));
    let (_uf, owf, _proof) = prove_fold(&r1cs, &params, &u1, &ow1, &u2, &ow2, &r, &r_t);

    // Spec fold (scalar core): fresh instances have u = 1, E = 0.
    let e_zero = [Scalar::default(); 3];
    let t = spec::cross_term(&w1, &u1.u, &w2, &u2.u);
    let (sw, se) = spec::fold_witness(&w1, &e_zero, &w2, &e_zero, &t, &r);

    assert_eq!(sw.to_vec(), owf.w, "spec folded W must match oracle");
    assert_eq!(se.to_vec(), owf.e, "spec folded E must match oracle");

    // Relaxed satisfaction agrees between spec and oracle on the folded instance.
    let uf_u = spec::fold_u(&u1.u, &u2.u, &r);
    assert!(
        spec::is_satisfied_relaxed(&sw, &se, &uf_u),
        "spec relaxed-sat"
    );
    assert!(
        r1cs.is_satisfied_relaxed(&owf.w, &owf.e, &uf_u),
        "oracle relaxed-sat"
    );
}

#[test]
fn spec_commit_fold_matches_oracle() {
    // The spec commitment (instance) fold reproduces the oracle's verify_fold.
    let r1cs = and_check_r1cs();
    let params = PedersenParams::setup(8, 7);
    let w1 = gate(3, 4, 5, 6);
    let w2 = gate(7, 8, 9, 2);
    let r = s(0xabcd);
    let r_t = s(0x1357);

    let (u1, ow1) = fresh(&r1cs, &params, &w1, s(11));
    let (u2, ow2) = fresh(&r1cs, &params, &w2, s(13));
    let (uf, _owf, proof) = prove_fold(&r1cs, &params, &u1, &ow1, &u2, &ow2, &r, &r_t);

    let r_le = r.to_bytes_le();
    let r2_le = r.mul(&r).to_bytes_le();
    let spec_cw = volar_spec::fold::fold_commit_w(&u1.comm_w, &u2.comm_w, &r_le);
    let spec_ce =
        volar_spec::fold::fold_commit_e(&u1.comm_e, &proof.comm_t, &u2.comm_e, &r_le, &r2_le);

    assert_eq!(spec_cw, uf.comm_w, "spec comm_W fold matches oracle");
    assert_eq!(spec_ce, uf.comm_e, "spec comm_E fold matches oracle");
}

#[test]
fn spec_pedersen_commit_matches_oracle() {
    // The spec Pedersen commit reproduces the oracle's MSM-based commit, for
    // both witness-length (7) and error-length (3) vectors.
    let params = PedersenParams::setup(8, 7);

    let w = gate(3, 4, 5, 6);
    let blind = s(0x9e3);
    let x_bytes: Vec<[u8; 32]> = w.iter().map(|s| s.to_bytes_le()).collect();
    let oracle = params.commit(&w, &blind);
    let spec =
        volar_spec::fold::pedersen_commit(&params.g, &params.h, &x_bytes, &blind.to_bytes_le());
    assert_eq!(spec, oracle, "spec witness commit matches oracle");

    let e = [s(11), s(22), s(33)];
    let e_bytes: Vec<[u8; 32]> = e.iter().map(|s| s.to_bytes_le()).collect();
    let oracle_e = params.commit(&e, &blind);
    let spec_e =
        volar_spec::fold::pedersen_commit(&params.g, &params.h, &e_bytes, &blind.to_bytes_le());
    assert_eq!(spec_e, oracle_e, "spec error commit matches oracle");
}

#[test]
fn spec_relaxed_sat_accepts_honest_rejects_tampered() {
    let one = s(1);
    let zero_e = [Scalar::default(); 3];

    let honest = gate(3, 4, 5, 6);
    assert!(
        spec::is_satisfied_relaxed(&honest, &zero_e, &one),
        "honest gate accepts"
    );

    let mut tampered = gate(3, 4, 5, 6);
    tampered[4] = tampered[4].add(&one); // break V̂
    assert!(
        !spec::is_satisfied_relaxed(&tampered, &zero_e, &one),
        "tampered gate rejects"
    );
}

#[test]
fn spec_fold_chain_matches_oracle() {
    // Fold three gates sequentially via both paths; final W/E must agree.
    let r1cs = and_check_r1cs();
    let params = PedersenParams::setup(8, 99);
    let gates = [gate(2, 3, 4, 5), gate(4, 5, 6, 7), gate(6, 7, 8, 9)];
    let rs = [s(0x10), s(0x20)];
    let rts = [s(0x11), s(0x21)];

    // Oracle chain.
    let (mut ou, mut ow) = fresh(&r1cs, &params, &gates[0], s(1));
    // Spec chain (carry W, E, u).
    let mut sw = gates[0];
    let mut se = [Scalar::default(); 3];
    let mut su = ou.u;

    for k in 0..2 {
        let (nu, nw) = fresh(&r1cs, &params, &gates[k + 1], s(100 + k as u64));
        let (uf, wf, _) = prove_fold(&r1cs, &params, &ou, &ow, &nu, &nw, &rs[k], &rts[k]);
        ou = uf;
        ow = wf;

        let t = spec::cross_term(&sw, &su, &gates[k + 1], &nu.u);
        let (nsw, nse) =
            spec::fold_witness(&sw, &se, &gates[k + 1], &[Scalar::default(); 3], &t, &rs[k]);
        sw = nsw;
        se = nse;
        su = spec::fold_u(&su, &nu.u, &rs[k]);
    }

    assert_eq!(sw.to_vec(), ow.w, "chained spec W matches oracle");
    assert_eq!(se.to_vec(), ow.e, "chained spec E matches oracle");
    assert!(r1cs.is_satisfied_relaxed(&ow.w, &ow.e, &su));
}

#[test]
fn spec_fold_chain_matches_oracle_with_tampered_step_and_both_reject() {
    // Same three-gate chain as `spec_fold_chain_matches_oracle`, but the
    // middle gate lies (its V̂ is off by one). Two things must hold:
    // 1. Spec and oracle still agree bit-for-bit — parity holds regardless of
    //    honesty, since both sides fold the same (dishonest) witness the same
    //    way; only the final relaxed-satisfaction check is honesty-sensitive.
    // 2. The final relaxed relation is rejected by *both* r1cs.is_satisfied_relaxed
    //    (oracle) and spec::is_satisfied_relaxed (spec) — completeness of the
    //    tamper detection on the spec side, not just the oracle side.
    let r1cs = and_check_r1cs();
    let params = PedersenParams::setup(8, 99);
    let mut gates = [gate(2, 3, 4, 5), gate(4, 5, 6, 7), gate(6, 7, 8, 9)];
    gates[1][4] = gates[1][4].add(&s(1)); // break gate 1's V̂
    let rs = [s(0x10), s(0x20)];
    let rts = [s(0x11), s(0x21)];

    let (mut ou, mut ow) = fresh(&r1cs, &params, &gates[0], s(1));
    let mut sw = gates[0];
    let mut se = [Scalar::default(); 3];
    let mut su = ou.u;

    for k in 0..2 {
        let (nu, nw) = fresh(&r1cs, &params, &gates[k + 1], s(100 + k as u64));
        let (uf, wf, _) = prove_fold(&r1cs, &params, &ou, &ow, &nu, &nw, &rs[k], &rts[k]);
        ou = uf;
        ow = wf;

        let t = spec::cross_term(&sw, &su, &gates[k + 1], &nu.u);
        let (nsw, nse) =
            spec::fold_witness(&sw, &se, &gates[k + 1], &[Scalar::default(); 3], &t, &rs[k]);
        sw = nsw;
        se = nse;
        su = spec::fold_u(&su, &nu.u, &rs[k]);
    }

    assert_eq!(
        sw.to_vec(),
        ow.w,
        "chained spec W matches oracle even with a lying step"
    );
    assert_eq!(
        se.to_vec(),
        ow.e,
        "chained spec E matches oracle even with a lying step"
    );
    assert!(
        !r1cs.is_satisfied_relaxed(&ow.w, &ow.e, &su),
        "oracle must reject the dishonest chain"
    );
    assert!(
        !spec::is_satisfied_relaxed(&sw, &se, &su),
        "spec must reject the dishonest chain too"
    );
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(16))]

    /// Completeness, many random honest gate chains: for any sequence of
    /// honest `and_check` gates (`V̂ = K_c·Δ − K_a·K_b`) and random
    /// blinders/challenges, the spec fold and the oracle fold agree
    /// bit-for-bit, and the chained result satisfies the relaxed relation on
    /// both sides.
    #[test]
    fn prop_spec_fold_matches_oracle_on_honest_chains(
        gates_raw in proptest::collection::vec((1u64..30, 1u64..30, 1u64..30, 1u64..30), 1..6),
        seed in 1u64..1000,
    ) {
        let r1cs = and_check_r1cs();
        let params = PedersenParams::setup(8, seed);
        let gates: Vec<[Scalar; 7]> = gates_raw.iter()
            .map(|&(a, b, c, d)| gate(a, b, c, d))
            .collect();

        let (mut ou, mut ow) = fresh(&r1cs, &params, &gates[0], s(1));
        let mut sw = gates[0];
        let mut se = [Scalar::default(); 3];
        let mut su = ou.u;

        for k in 0..gates.len().saturating_sub(1) {
            let (nu, nw) = fresh(&r1cs, &params, &gates[k + 1], s(100 + k as u64));
            let r = s(0x10 + k as u64);
            let rt = s(0x11 + k as u64);
            let (uf, wf, _) = prove_fold(&r1cs, &params, &ou, &ow, &nu, &nw, &r, &rt);
            ou = uf;
            ow = wf;

            let t = spec::cross_term(&sw, &su, &gates[k + 1], &nu.u);
            let (nsw, nse) = spec::fold_witness(&sw, &se, &gates[k + 1], &[Scalar::default(); 3], &t, &r);
            sw = nsw;
            se = nse;
            su = spec::fold_u(&su, &nu.u, &r);
        }

        prop_assert_eq!(sw.to_vec(), ow.w.clone());
        prop_assert_eq!(se.to_vec(), ow.e.clone());
        prop_assert!(r1cs.is_satisfied_relaxed(&ow.w, &ow.e, &su));
        prop_assert!(spec::is_satisfied_relaxed(&sw, &se, &su));
    }
}
