// @reliability: experimental
//! End-to-end test closing the trace-emission seam `docs/prove-the-verifier.md`
//! documents: weave a small AND-only circuit with `NovaFoldSink`, lower it to
//! real Rust source, compile and run it for real (via
//! `volar-verifier-runtime::run_folded_verifier`), and confirm the recovered
//! `fold_state` genuinely satisfies the expanded gf2k AND-check R1CS relation
//! (`volar_fold::gf2k::and_check_gf2k`, `docs/fold-lift-expansion.md`).
//!
//! This is the first test in the repo that exercises "compile the woven
//! verifier to real Rust, run it, fold happens inside that run" end to end —
//! and, since the constraint-expansion embedding replaced the old one-scalar
//! `FoldLift`, the satisfaction assert is **positive** (it was pinned
//! known-failing while the embedding gap was open; see the driver's comment).

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_verifier_fold::emit_verifier_rust;
use volar_verifier_runtime::run_folded_verifier;
use volar_weaver::{weave_vole_verifier_with_trace, NovaFoldSink, ZkWitnessConfig};

/// `c = a AND b`; params = [a, b]; returns c.
fn and_circuit() -> BIrBlocks<()> {
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: 2,
            stmts: vec![Node::new(BIrStmt::And(IRVarId(0), IRVarId(1)), (), None)],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            }),
        }],
        pre_init: vec![],
    }
}

#[test]
fn nova_fold_sink_verifier_compiles_runs_and_folds_correctly() {
    let circuit = and_circuit();
    let config = ZkWitnessConfig::default();
    let module = weave_vole_verifier_with_trace(&circuit, "and1", &config, &NovaFoldSink, None);
    let rust_src = emit_verifier_rust(&module);

    // Sanity: the printed source actually contains the fold machinery
    // (mirrors volar-weaver's own IR-shape test, but on the real printed
    // Rust text this time).
    assert!(rust_src.contains("fold_and_gate"), "printed source missing fold_and_gate call:\n{rust_src}");
    assert!(rust_src.contains("vole_verify_and1"), "printed source missing the woven fn name:\n{rust_src}");

    let driver = r#"
        use volar_fold::gf2k::{and_check_gf2k, Gf2kParams};
        use volar_spec::field::Galois;
        use volar_spec::ot::IdealCot;
        use volar_spec::vole::setup::{derive_and_q, random_nonzero_delta, vole_commit_bit};
        use volar_spec::SpecRng;

        type N = cipher::consts::U16;

        struct TestRng(u64);
        impl SpecRng for TestRng {
            fn next_u32(&mut self) -> u32 {
                self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
                let mut z = self.0;
                z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
                z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
                (z ^ (z >> 31)) as u32
            }
        }
        fn sample_g<R: SpecRng>(r: &mut R) -> Galois { Galois(r.next_u8()) }
        fn lift_bit_g(b: bool) -> Galois { Galois(if b { 1 } else { 0 }) }
        fn is_zero_g(g: &Galois) -> bool { g.0 == 0 }

        #[test]
        fn honest_and_gate_folds_correctly() {
            let mut rng = TestRng(0xC0FFEE_C0FFEE);
            let delta = random_nonzero_delta::<N, Galois, _>(&mut rng, sample_g, is_zero_g);
            let cot = IdealCot::new(delta.clone());

            let (vope_a, q_a) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, true);
            let (vope_b, q_b) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, true);
            let (_vope_c, hat) = vole_and_prover_step(vope_a, vope_b);
            let q_and = derive_and_q(&delta, &q_a, &q_b, &hat);

            let r_and_0 = FoldScalar::from_u64(0xabcd);

            let (_out, all_ok, fold_state) =
                vole_verify_and1::<N, Galois>(&delta, q_and.clone(), hat, r_and_0, q_a, q_b);

            // The real GF(2^8) Quicksilver check (unaffected by any fold
            // embedding question) passes for this honest gate — the woven
            // verifier's own logic is unchanged and correct.
            assert!(all_ok, "honest AND gate must pass the woven verifier's own check");

            // Structural correctness of the threaded state: single (first)
            // gate is the "fresh" case (u = 1, E = 0) over the expanded
            // GF(2^8) relation (165 witness vars, 174 constraints — see
            // docs/fold-lift-expansion.md section 3).
            let (w, e, u) = fold_state.witness().expect("fold_state must be Some after one gate");
            assert_eq!(w.len(), 165, "expanded GF(2^8) gf2k witness size");
            assert_eq!(e.len(), 174, "E has one slot per gf2k constraint");
            assert_eq!(*u, FoldScalar::ONE, "single-gate fold must have u = 1 (fresh, not yet folded with anything)");
            assert!(e.iter().all(|x| *x == FoldScalar::ZERO), "single-gate fold must have E = 0 (fresh)");

            // History: this satisfaction check used to be pinned as a
            // known-FAILING assert. The old FoldLift transmuted each GF(2^8)
            // element to a single F_ell integer, and Galois's real field
            // multiplication (polynomial mod an irreducible) is not what
            // plain-integer F_ell multiplication computes on those bytes —
            // so an honest gate's GF(2^8) relation did NOT carry over, and
            // the test pinned that gap empirically (see
            // docs/agent-context/gf2k-to-fell-embedding.md). fold_and_gate
            // now expands each gate into the spaced-packing bit relation of
            // docs/fold-lift-expansion.md (volar_fold::gf2k::and_check_gf2k),
            // which encodes genuine GF(2^8) arithmetic over F_ell — so the
            // same honest gate now genuinely satisfies the folded R1CS, and
            // the assert flips to positive. (The R1CS shape depends only on
            // the params, so zero input bits reproduce the woven gate's
            // constraint system.)
            let params = Gf2kParams::new(8, 0x1b);
            let zeros = [false; 8];
            let (r1cs, _) = and_check_gf2k(&params, &zeros, &zeros, &zeros, &zeros, &zeros);
            assert!(
                r1cs.is_satisfied_relaxed(w, e, u),
                "the constraint-expansion GF(2^8) -> F_ell embedding \
                 (docs/fold-lift-expansion.md) must hold on a genuinely honest \
                 GF(2^8) VOLE proof: the woven verifier's folded witness has to \
                 satisfy and_check_gf2k's R1CS"
            );
        }
    "#;

    run_folded_verifier(&rust_src, driver);
}
