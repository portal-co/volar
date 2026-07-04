// @reliability: experimental
//! End-to-end test for the **IOP-based** prove-the-verifier path
//! (`docs/prove-the-verifier-iop.md`): weave a small AND-only circuit
//! with `IopSink`, lower it to real Rust source, compile and run it for
//! real (via `volar_verifier_iop_runtime::run_iop_verifier`), and confirm
//! the recovered `fold_state` genuinely satisfies `and_check_r1cs` natively
//! (no cross-field embedding needed).
//!
//! `emit_verifier_rust` (this crate) is generic over any
//! `Tagged<Transparent, IrModule>` and doesn't reference any sink's bare
//! names — confirming it needs zero backend-specific code.

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_verifier_fold::emit_verifier_rust;
use volar_verifier_iop_runtime::run_iop_verifier;
use volar_weaver::{weave_vole_verifier_with_trace, IopSink, ZkWitnessConfig};

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
fn iop_sink_verifier_compiles_runs_and_finalization_proof_verifies() {
    let circuit = and_circuit();
    let config = ZkWitnessConfig::default();
    let module = weave_vole_verifier_with_trace(&circuit, "and1", &config, &IopSink, None);
    let rust_src = emit_verifier_rust(&module);

    assert!(rust_src.contains("iop_fold_gate"), "printed source missing iop_fold_gate call:\n{rust_src}");
    assert!(rust_src.contains("vole_verify_and1"), "printed source missing the woven fn name:\n{rust_src}");

    let driver = r#"
        use volar_iop::field::{Field as _, Gf128};
        use volar_iop::fold::and_check_r1cs;
        use volar_iop::transcript::FromBytes as _;
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
        fn honest_and_gate_folds_and_finalizes() {
            let mut rng = TestRng(0xC0FFEE_C0FFEE);
            let delta = random_nonzero_delta::<N, Galois, _>(&mut rng, sample_g, is_zero_g);
            let cot = IdealCot::new(delta.clone());

            let (vope_a, q_a) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, true);
            let (vope_b, q_b) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, true);
            let (_vope_c, hat) = vole_and_prover_step(vope_a, vope_b);
            let q_and = derive_and_q(&delta, &q_a, &q_b, &hat);

            let r_and_0 = IopChallenge::from_u64(0xabcd);

            let (_out, all_ok, fold_state) =
                vole_verify_and1::<N, Galois>(&delta, q_and.clone(), hat, r_and_0, q_a, q_b);

            // The real GF(2^8) Quicksilver check passes for this honest gate
            // — the woven verifier's own logic is identical regardless of
            // which trace sink is threaded alongside it.
            assert!(all_ok, "honest AND gate must pass the woven verifier's own check");

            // Structural correctness of the threaded state: single (first)
            // gate is the "fresh" case (u = 1, E = 0) over and_check_r1cs's
            // native, unexpanded 7-slot/3-constraint shape.
            let (w, e, u) = fold_state.witness().expect("fold_state must be Some after one gate");
            assert_eq!(w.len(), 7, "native and_check_r1cs witness size (no bit-expansion)");
            assert_eq!(e.len(), 3, "native and_check_r1cs constraint count (no bit-expansion)");
            assert_eq!(*u, Gf128::ONE, "single-gate fold must have u = 1 (fresh)");
            assert!(e.iter().all(|x| *x == Gf128::ZERO), "single-gate fold must have E = 0 (fresh)");
            assert!(
                and_check_r1cs::<Gf128>().is_satisfied_relaxed(w, e, u),
                "an honest GF(2^8) VOLE proof's native-folded witness must satisfy and_check_r1cs"
            );

            // The finalization proof (Phase 2, Merkle+Fiat-Shamir) over this
            // small accumulator must verify — no memory boundary for this
            // AND-only circuit (empty mem_acc_in/out, no expectation).
            let tagged: volar_discipline::Tagged<volar_discipline::Transparent, _> =
                volar_discipline::Tagged::seal(fold_state);
            let (_proof, ok) = prove_and_verify_iop(tagged, &[], &[], None);
            assert!(ok, "the Merkle+Fiat-Shamir finalization proof must verify for an honest gate");
        }
    "#;

    run_iop_verifier(&rust_src, driver);
}
