//! Milestone 1, M1.5-M1.7: a real, end-to-end run of the whole stack --
//! real `IdealCot`-driven VOLE, real compiled+executed woven prover and
//! verifier, real Merkle+Fiat-Shamir IOP finalization, and (the specific,
//! previously-untested gap this closes) a **real, non-empty** memory-
//! accumulator boundary check -- every existing IOP test before this one
//! passed empty `mem_acc_in`/`mem_acc_out` slices.
//!
//! # Why this circuit, not the RISC-V interpreter's
//!
//! The full interpreter (or even `mem_probe`'s tiny load/increment/store
//! loop) movfuscates into thousands of *chained* AND gates. Driving a
//! chained AND gate for real requires supplying `q_and_i: Q<N,T>` --
//! `derive_and_q(delta, q_a, q_b, hat)` -- where `q_a`/`q_b` are the
//! gate's own operand wires' Q-values, which for any wire *after* the
//! first are themselves internal to the woven function (not parameters).
//! Because Q-values satisfy `Q_wire = bit_wire * delta` for every wire in
//! an honest run (XOR/NOT preserve this by linearity; the AND-gate check
//! is exactly what makes it hold across `derive_and_q` too), this is
//! *solvable* in general, but only by replicating the entire wire graph
//! in a parallel bit-level simulator -- real, valuable future work,
//! tracked in `docs/agent-context/circuit-size-optimization-backlog.md`,
//! not attempted here.
//!
//! This circuit sidesteps that entirely by having **exactly one AND
//! gate**, with both operands fully known externally (the oracle bit
//! itself, and a literal constant-1 wire) -- `prove_verifier_iop`'s
//! `finish_fold` requires at least one folded gate, so truly zero gates
//! isn't an option, but one gate with top-level-known operands needs no
//! wire-graph simulation at all. Read one committed bit, AND it with 1
//! (identity), NOT it (flip), write it back. Run three times, with memory
//! (not any block param) carrying state across calls -- the simplest
//! circuit that still genuinely exercises `StorageMode::Commitment`'s
//! oracle-read machinery, a real multi-call memory trace, and a real
//! non-empty memory multiset-check boundary.

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use volar_ir::ir::{
        IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRTypes, IRVarId,
    };
    use volar_ir_common::{Constant, IrType as IRType, Node, StorageId, Type};

    /// The flip-bit circuit: `params = []` (all state lives in committed
    /// memory, not in any loop-carried param); one `StorageRead`, one
    /// `Poly` (GF(2) NOT: `1 + x`), one `StorageWrite`, both at address 0
    /// of memory 0. Already satisfies `is_circuit()` directly (single
    /// block, `Jmp(Return)`) -- no movfuscation/unrolling needed, since
    /// there is no loop at all: the driver itself calls this same
    /// function repeatedly, with memory (not a return value) threading
    /// state between calls.
    fn build_flip_bit_circuit() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(Type::Bit));

        // One genuine AND gate: `var1 AND var2` where `var2` is a literal
        // constant-1 wire (identity: AND-with-1 == the value itself). Both
        // operands are fully known externally (var1 = the chosen oracle
        // bit, var2 always 1) -- `prove_verifier_iop`'s `finish_fold`
        // requires at least one folded gate ("accumulator has no folded
        // gates" otherwise), and this is the simplest way to supply one
        // without introducing a chained (non-parameter) operand.
        let mut and_coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        and_coeffs.insert(vec![IRVarId(1), IRVarId(2)], 1u8);

        let mut not_coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        not_coeffs.insert(vec![IRVarId(3)], 1u8);

        let stmts = vec![
            // var 0: address 0 (params.len() == 0, so this is the first var id)
            IRStmt::Const(Constant { hi: 0, lo: 0 }, bit),
            // var 1: read the committed bit at address 0
            IRStmt::StorageRead { storage: StorageId::memory(0), ty: bit, addr: IRVarId(0) },
            // var 2: literal constant 1
            IRStmt::Const(Constant { hi: 0, lo: 1 }, bit),
            // var 3: var1 AND var2 (== var1, since var2 == 1) -- the one AND gate
            IRStmt::Poly { ty: bit, coeffs: and_coeffs, constant: Constant { hi: 0, lo: 0 } },
            // var 4: NOT(var 3) = 1 + var3 (GF(2)) -- the flipped bit
            IRStmt::Poly { ty: bit, coeffs: not_coeffs, constant: Constant { hi: 0, lo: 1 } },
            // var 5: write the flipped bit back to address 0 (no output wire)
            IRStmt::StorageWrite { storage: StorageId::memory(0), src: IRVarId(4), ty: bit, addr: IRVarId(0) },
        ]
        .into_iter()
        .map(|s| Node::new(s, (), None))
        .collect();

        let block = IRBlock {
            params: vec![],
            stmts,
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(IRBlockTargetId::Return, vec![IRVarId(4)]),
            },
        };
        (IRBlocks::new(vec![block]), types)
    }

    #[test]
    fn flip_bit_circuit_is_already_a_circuit() {
        let (circuit, _types) = build_flip_bit_circuit();
        assert!(circuit.is_circuit());
    }

    /// Weave both prover and verifier once, sanity-check the shape this
    /// module's doc comment claims (exactly one AND gate with fully-known
    /// operands, one oracle read, one committed write, real memory trace)
    /// before spending time on the full compile+run.
    #[test]
    fn flip_bit_weaves_with_commitment_and_one_and_gate() {
        use volar_weaver::{
            weave_vole_prover_ir_with_mode, weave_vole_verifier_ir_with_mode_and_trace, IopSink,
            StorageMode,
        };

        let (circuit, types) = build_flip_bit_circuit();
        let mode = StorageMode::Commitment;

        let (prover_module, prover_trace) =
            weave_vole_prover_ir_with_mode(&circuit, &types, "flip", &mode, None);
        let prover_code = volar_weaver::print_weaved_vole_module(prover_module.inner());
        assert!(prover_code.contains("fn vole_prove_ir_flip"), "{prover_code}");
        assert!(prover_code.contains("oracle_rd_0"), "{prover_code}");
        assert!(prover_code.contains("vole_and_prover_step::<"), "expected exactly one AND gate:\n{prover_code}");
        assert_eq!(prover_trace.entries.len(), 2, "one read + one write");

        let (verifier_module, verifier_trace) = weave_vole_verifier_ir_with_mode_and_trace(
            &circuit, &types, "flip", &mode, &IopSink, None,
        );
        let verifier_code = volar_weaver::print_weaved_vole_module(verifier_module.inner());
        assert!(verifier_code.contains("fn vole_verify_ir_flip"), "{verifier_code}");
        assert!(verifier_code.contains("vole_and_verifier_check::<"), "expected exactly one AND gate:\n{verifier_code}");
        assert_eq!(verifier_trace.entries.len(), 2);
    }

    /// M1.5-M1.7's real checkpoint: compile and run the woven prover +
    /// verifier for `STEPS` real calls (memory persisting across calls,
    /// per this module's doc comment), using real `IdealCot`-driven VOLE
    /// commitments for every witness bit, then fold + finalize via the
    /// real Merkle+Fiat-Shamir IOP path with a **non-empty**
    /// `mem_acc_in`/`mem_acc_out` boundary computed from
    /// `volar_spec::vole::memory::MemoryCheckState` over the real trace.
    ///
    /// # Honest scope note on the memory boundary's binding
    ///
    /// `mem_acc_in`/`mem_acc_out` are opaque, caller-supplied attestations
    /// as far as `prove_verifier_iop`/`verify_iop` are concerned — neither
    /// function derives them from the woven circuit or checks
    /// `H_produce == H_consume` internally (confirmed by reading
    /// `volar-iop`'s implementation: they're spliced into the Ligero
    /// message and Merkle/FS-checked for internal consistency and, if
    /// `expected_mem_acc` is given, equality with the caller's own
    /// expectation — nothing more). This test's own `MemoryCheckState`
    /// bookkeeping is a **self-consistency check** the driver performs
    /// independently (mirroring `docs/memory-checking.md`'s protocol) —
    /// it demonstrates the full plumbing (`IdealCot` → real compiled
    /// prover/verifier → IOP fold → Merkle+FS finalization → a non-empty,
    /// internally-consistent memory boundary) works end-to-end, which is
    /// this milestone's specific, previously-untested goal. Cryptographically
    /// *binding* `mem_acc_in`/`mem_acc_out` to the real VOLE-authenticated
    /// `oracle_rd_i`/write-value wires is a separate, currently
    /// unimplemented concern in this codebase (no code path anywhere
    /// derives an authenticated encode/absorb from `StorageMode::Commitment`
    /// today) — not something this test can or should paper over.
    #[test]
    fn honest_flip_bit_run_folds_and_finalizes_with_real_memory_boundary() {
        use volar_weaver::{
            weave_vole_prover_ir_with_mode, weave_vole_verifier_ir_with_mode_and_trace, IopSink,
            StorageMode, print_weaved_vole_module,
        };
        use volar_verifier_iop_runtime::run_iop_verifier;

        let (circuit, types) = build_flip_bit_circuit();
        let mode = StorageMode::Commitment;

        let (prover_module, _prover_trace) =
            weave_vole_prover_ir_with_mode(&circuit, &types, "flip", &mode, None);
        let prover_code = print_weaved_vole_module(prover_module.inner());

        let (verifier_module, _verifier_trace) = weave_vole_verifier_ir_with_mode_and_trace(
            &circuit, &types, "flip", &mode, &IopSink, None,
        );
        let verifier_code = print_weaved_vole_module(verifier_module.inner());

        // Both printed modules carry their own copy of the shared
        // `#![allow(...)]`/`use ...;` header; concatenating both verbatim
        // would duplicate every `use` line (a hard error, not just an
        // unused-import lint). Keep the verifier's full header and splice
        // in only the prover's function body (starting at its `pub fn`).
        let prover_fn_only = &prover_code[prover_code.find("pub fn").expect("prover source must have a pub fn")..];
        let rust_source = format!("{verifier_code}\n{prover_fn_only}");

        let driver = r#"
            use volar_iop::field::{Field as _, Gf128};
            use volar_iop::transcript::FromBytes as _;
            use volar_spec::field::Galois;
            use volar_spec::ot::IdealCot;
            use volar_spec::vole::setup::{derive_and_q, random_nonzero_delta, vole_commit_bit};
            use volar_spec::vole::memory::{ChallengeKey, MemoryCheckState};
            use volar_spec::vole::{Delta, Q, Vope};
            use volar_spec::{Array, SpecRng};
            use hybrid_array::Array as HArray;

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

            fn vope_one(delta: &Delta<N, Galois>) -> Vope<N, Galois, cipher::consts::U1> {
                let _ = delta;
                Vope {
                    u: HArray::<HArray<Galois, N>, cipher::consts::U1>::from_fn(|_| HArray::<Galois, N>::from_fn(|_| Galois(1))),
                    v: HArray::<Galois, N>::from_fn(|_| Galois(0)),
                }
            }
            fn q_one(delta: &Delta<N, Galois>) -> Q<N, Galois> {
                Q { q: HArray::<Galois, N>::from_fn(|i| delta.delta[i].clone()) }
            }

            #[test]
            fn honest_flip_bit_three_steps_verifies_with_real_memory_boundary() {
                let mut rng = TestRng(0xC0FFEE_C0FFEE);
                let delta = random_nonzero_delta::<N, Galois, _>(&mut rng, sample_g, is_zero_g);
                let cot = IdealCot::new(delta.clone());

                let r = Galois(0x53); // memory-check challenge, fixed for reproducibility
                let key = ChallengeKey::from_challenge(r);
                let mut mem = MemoryCheckState::<Galois>::new(key);

                // Address 0 is never pre-initialised (no `pre_init` segment
                // in the circuit), so per `docs/memory-checking.md`, the
                // driver synthesizes the genesis `init` itself, matching
                // WASM's own zero-initialised-memory semantics.
                mem.init(Galois(0), Galois(0));
                let mut last_ts: u64 = 0;
                let mut current_bit = false;
                let mut fold_state = None;

                for step in 0..3u64 {
                    let (vope_oracle, q_oracle) =
                        vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, current_bit);

                    let (out_vope, hats): (Vope<N, Galois, cipher::consts::U1>, [Array<Galois, N>; 1]) =
                        vole_prove_ir_flip::<N, Galois>(vope_one(&delta), vope_oracle.clone());
                    let _ = out_vope;

                    // The one AND gate is `oracle_bit AND 1` -- both
                    // operands fully known externally (q_oracle itself,
                    // and q_one for the literal constant-1 wire), so
                    // `derive_and_q` can be computed here directly instead
                    // of needing a general per-wire Q-value simulator (see
                    // this module's doc comment).
                    let hat_0 = hats[0].clone();
                    let q_and_0 = derive_and_q(&delta, &q_oracle, &q_one(&delta), &hat_0);
                    let r_and_0 = Gf128::from_u64(0xabcd_0000 + step);

                    let (out_q, all_ok, verifier_fold_state) =
                        vole_verify_ir_flip::<N, Galois>(&delta, [q_and_0], [hat_0], [r_and_0], q_one(&delta), q_oracle);
                    let _ = out_q;

                    assert!(all_ok, "honest flip-bit run must pass the woven verifier's own check");
                    fold_state = Some(verifier_fold_state);

                    let read_ts = last_ts + 1;
                    mem.read(Galois(0), lift_bit_g(current_bit), read_ts, last_ts);
                    last_ts = read_ts;

                    let new_bit = !current_bit;
                    let write_ts = last_ts + 1;
                    mem.write(Galois(0), lift_bit_g(new_bit), write_ts, lift_bit_g(current_bit), last_ts);
                    last_ts = write_ts;
                    current_bit = new_bit;
                }

                mem.drain(Galois(0), lift_bit_g(current_bit), last_ts);
                assert!(mem.verify(), "the driver's own independent memory multiset check must balance");

                let h_produce = mem.produce().iop_embed();
                let h_consume = mem.consume().iop_embed();
                assert_eq!(h_produce, h_consume, "an honest run's H_produce/H_consume must embed identically");

                let mem_acc_in = [h_produce];
                let mem_acc_out = [h_consume];

                let tagged: volar_discipline::Tagged<volar_discipline::Transparent, _> =
                    volar_discipline::Tagged::seal(fold_state.expect("at least one step ran"));
                let (proof, ok) = prove_and_verify_iop(tagged, &mem_acc_in, &mem_acc_out, Some((&mem_acc_in, &mem_acc_out)));
                assert!(ok, "the finalization proof over a real, non-empty memory boundary must verify");

                // M1.7's other half: a corrupted expected boundary (as if a
                // dishonest driver claimed a different final memory state)
                // must be rejected -- reusing the same real proof from the
                // honest run above, matching
                // `volar_verifier_iop_runtime`'s own `memory_boundary_mismatch_is_rejected` pattern.
                let corrupted_out = [Gf128::from_u64(0xdead_beef)];
                let corrupted_ok = volar_iop::verify_iop(&proof, Some((&mem_acc_in, &corrupted_out)));
                assert!(!corrupted_ok, "a corrupted expected memory boundary must be rejected");
            }
        "#;

        run_iop_verifier(&rust_source, driver);
    }
}
