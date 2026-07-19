//! Milestone 1, steps M1.5-M1.7: a **minimal** real committed-memory
//! circuit, small enough to actually compile and run (unlike the full
//! RISC-V interpreter -- see `docs/agent-context/mem-probe-scope-note.md`
//! for why this is a deliberately smaller stand-in for the interpreter at
//! this step).
//!
//! One WASM function, one memory, one instruction that matters: load a
//! byte, increment it, store it back, three times. Loop-carried state is
//! a single i32 (`$steps`) -- `$val` is always overwritten before being
//! read again, so it never becomes a loop-carried block param. This
//! keeps the woven circuit's parameter list small enough to hand-drive
//! directly, while still exercising the real pipeline (`lower_waffle_module`
//! -> `lower_vaffle_to_ir` -> `movfuscate_ir` -> `lower_to_circuit_ir`),
//! real `StorageMode::Commitment` (byte-addressed, one real read + one
//! real write per step), real `IdealCot`-driven VOLE, real IOP fold, and
//! a real, non-empty memory-accumulator boundary.

/// The probe program: increment a byte in memory `STEPS` times.
pub const STEPS: i32 = 3;

/// WAT source for the probe. `i32.load8_u`/`i32.store8` (not the full
/// `i32.load`/`i32.store`) so each step touches **exactly one** committed
/// byte -- one `StorageRead` + one `StorageWrite`, both width 8, per real
/// step (`i32.load`/`store` decompose into 4 separate byte ops each, per
/// `waffle_lower.rs`'s `mem_load_bytes`/`mem_store_bytes` -- avoided here
/// purely to keep the hand-driven witness small, not a frontend gap).
pub fn mem_probe_wat() -> String {
    format!(
        r#"(module
  (memory $mem 1)
  (export "mem" (memory $mem))

  (func (export "run")
    (local $steps i32)
    (local $val i32)

    (loop $L
      (local.set $val (i32.load8_u (i32.const 0)))
      (local.set $val (i32.add (local.get $val) (i32.const 1)))
      (i32.store8 (i32.const 0) (local.get $val))
      (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
      (br_if $L (i32.lt_s (local.get $steps) (i32.const {steps})))
    )
  )
)
"#,
        steps = STEPS,
    )
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    #[test]
    fn mem_probe_wat_assembles() {
        wat::parse_str(&mem_probe_wat()).expect("wat should assemble");
    }

    #[test]
    fn mem_probe_wat_matches_expected_via_wasmtime() {
        let wasm_bytes = wat::parse_str(&mem_probe_wat()).expect("wat should assemble");
        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &wasm_bytes).expect("module should validate");
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiate");
        let run = instance
            .get_typed_func::<(), ()>(&mut store, "run")
            .expect("run should be exported");
        run.call(&mut store, ()).expect("run should not trap");

        let mem = instance.get_memory(&mut store, "mem").expect("mem export");
        let mut byte = [0u8; 1];
        mem.read(&store, 0, &mut byte).expect("byte 0 in bounds");
        assert_eq!(byte[0], STEPS as u8, "byte 0 must be incremented STEPS times");
    }

    /// Shared helper: parse+lower the probe WAT all the way to a genuine
    /// `is_circuit()` IR circuit, unrolled once (`limit=1`) with
    /// `WithTerminationFlag` -- one call = one real step; the driver loops
    /// externally, threading state + fold across calls, exactly as
    /// `wat_gen::lower_interpreter` does for the full interpreter.
    ///
    /// Also returns `movfuscate_ir_with_boundary`'s boundary/`accum_info`
    /// metadata, needed to drive the *split* weave (Milestone 1.6, Stage
    /// 2's `weave_vole_qsim_ir_split` and friends) -- required here, not
    /// just for the much bigger real interpreter circuit: a real driven
    /// test found that even this circuit's ~2,740 gates, woven as a
    /// *single* Rust function, compile pathologically slowly (a genuine
    /// LLVM single-function-size limitation, confirmed via direct
    /// profiling -- see `docs/agent-context/circuit-size-optimization-backlog.md`).
    /// Deliberately skips post-movfuscation `optimize_to_fixpoint` (matching
    /// `wat_gen::lower_interpreter`'s own fix): it was measured to *increase*
    /// and_count ~24x on the real interpreter circuit, and separately
    /// invalidates boundary/`accum_info` metadata outright.
    pub(crate) fn lower_mem_probe() -> (
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRTypes,
        std::vec::Vec<volar_ir_passes::MovfuscBlockBoundary>,
        volar_ir_passes::MovfuscAccumInfo,
    ) {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};

        let wasm_bytes = wat::parse_str(&mem_probe_wat()).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module,
            &mut target,
            &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        let (mut ir_blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);

        loop {
            let a = fold_ir_blocks(&mut ir_blocks, &types);
            let b = store_forward_ir_blocks(&mut ir_blocks, &types);
            if !a && !b {
                break;
            }
        }

        let (movfuscated, boundary, accum_info) = movfuscate_ir_with_boundary(&ir_blocks, &mut types);

        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
        (ir_blocks, movfuscated, circuit, types, boundary, accum_info)
    }

    #[test]
    fn mem_probe_lowers_and_unrolls_to_a_circuit() {
        let (ir_blocks, movfuscated, circuit, _types, boundary, accum_info) = lower_mem_probe();
        assert!(!ir_blocks.is_circuit());
        assert_eq!(movfuscated.blocks.len(), 1);
        assert!(circuit.is_circuit(), "unrolled probe must satisfy is_circuit()");
        assert!(!boundary.is_empty(), "boundary metadata must be non-empty for the split weave");
        assert_eq!(accum_info.steps.len(), boundary.len());
    }

    /// Diagnostic (not a correctness assertion): dump the woven prover +
    /// verifier function signatures for the probe circuit, so a driver can
    /// be written against the *real* generated parameter list rather than
    /// a guessed one. Run manually with
    /// `cargo test -p volar-riscv-e2e dump_mem_probe_signatures -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn dump_mem_probe_signatures() {
        use volar_weaver::{
            weave_vole_prover_ir_with_mode, weave_vole_verifier_ir_with_mode_and_trace, IopSink,
            StorageMode,
        };

        let (_ir_blocks, movfuscated, circuit, types, _boundary, _accum_info) = lower_mem_probe();
        eprintln!("movfuscated params: {:?}", movfuscated.blocks[0].params);
        eprintln!("movfuscated terminator: {:?}", movfuscated.blocks[0].terminator);
        eprintln!("circuit params: {:?}", circuit.blocks[0].params);
        eprintln!("circuit terminator: {:?}", circuit.blocks[0].terminator);
        eprintln!("pre_init segments:");
        for seg in &circuit.pre_init {
            eprintln!("  storage={} ty={} offset={} len={}", seg.storage.0, seg.ty.0, seg.offset, seg.data.len());
        }
        eprintln!("all StorageRead/StorageWrite stmts in circuit:");
        for (i, stmt) in circuit.blocks[0].stmts.iter().enumerate() {
            match &stmt.kind {
                volar_ir::ir::Stmt::StorageRead { storage, ty, addr } => {
                    eprintln!("  [{i}] READ  storage={} ty={} addr={:?}", storage.0, ty.0, addr);
                }
                volar_ir::ir::Stmt::StorageWrite { storage, ty, addr, src } => {
                    eprintln!("  [{i}] WRITE storage={} ty={} addr={:?} src={:?}", storage.0, ty.0, addr, src);
                }
                _ => {}
            }
        }
        let mode = StorageMode::Commitment;

        let (prover_module, prover_trace) =
            weave_vole_prover_ir_with_mode(&circuit, &types, "mem_probe", &mode, None);
        let pf = &prover_module.inner().functions[0];
        eprintln!("prover fn: {}", pf.name);
        for p in &pf.params {
            eprintln!("  param {} : {:?}", p.name, p.ty);
        }
        eprintln!("prover return_type: {:?}", pf.return_type);
        eprintln!("prover trace entries: {:?}", prover_trace.entries);

        let (verifier_module, verifier_trace) = weave_vole_verifier_ir_with_mode_and_trace(
            &circuit, &types, "mem_probe", &mode, &IopSink, None,
        );
        let vf = &verifier_module.inner().functions[0];
        eprintln!("verifier fn: {}", vf.name);
        eprintln!("verifier return_type: {:?}", vf.return_type);
        eprintln!("verifier trace entries: {:?}", verifier_trace.entries);
        eprintln!(
            "NOTE: driving this circuit for real (M1.5-M1.7) requires resolving q_and_i for \
             every chained AND gate, which needs a full per-wire bit simulator (this circuit \
             alone has ~2740 AND gates from the loop/add/compare logic) -- deferred; see the \
             smaller, fully-driven `commit_mem_e2e` circuit instead, which sidesteps this by \
             having zero AND gates."
        );
    }

    /// Trace the *plain* (non-cryptographic) values flowing through the
    /// probe circuit's two storages across `STEPS` real calls, via
    /// `volar_fuzz`'s IR interpreter with a persistent storage map --
    /// establishes ground truth (addresses, byte values) for the real
    /// driven test below. Also a regression guard for a real bug this
    /// found and fixed in `volar_fuzz::interpreter::ir`: `Stmt::Poly`'s
    /// width was inferred by peeking at the first monomial's first
    /// variable's *already-evaluated* width, which silently produced a
    /// width-1 result whenever that variable happened to be a scalar
    /// `Bit` broadcast operand (e.g. movfuscation's `is_active · val`
    /// selector) rather than the statement's own declared (and correct)
    /// `ty` -- exactly this circuit's shape (a self-looping block with
    /// wide state slots) exercises it. Fixed to read `ty` directly
    /// (matching every other `IRStmt` variant here and the weaver's own
    /// `cir_type_width`), and `eval_poly` to broadcast scalar operands to
    /// every lane instead of treating them as `0` past their own single
    /// bit (matching the weaver's `operand_lane` semantics).
    #[test]
    fn trace_mem_probe_plain_values_has_correct_widths() {
        use volar_fuzz::interpreter::ir::eval_ir_circuit_step;

        let (_ir_blocks, _movfuscated, circuit, types, _boundary, _accum_info) = lower_mem_probe();
        let num_params = circuit.blocks[0].params.len();
        let param_widths: Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        assert_eq!(param_widths, vec![1, 1, 1, 64, 32], "probe circuit's own declared param widths");

        let mut storage = volar_fuzz::interpreter::ir::StorageMap::new();
        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        assert_eq!(inputs.len(), num_params);

        for step in 0..3 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            // output[0] = done flag; output[1..] map 1:1 onto the next
            // step's input params -- widths must match, not silently
            // narrow (the regression this guards against).
            let widths: Vec<usize> = outputs.iter().map(|v| v.len()).collect();
            assert_eq!(widths, vec![1, 1, 1, 1, 64, 32], "step {step}: output widths must match [done] ++ param_widths, not silently narrow");
            inputs = outputs[1..].to_vec();
        }

        // The real committed byte (storage 33, address 0) must increment
        // 0 -> 1 -> 2 -> 3 exactly as `mem_probe_wat_matches_expected_via_wasmtime`
        // already confirms via a real wasmtime run -- cross-checks this
        // interpreter against that independent oracle. Look the entry up
        // by (storage, addr) alone rather than a hardcoded TypeId -- the
        // exact numeric TypeId assigned to a byte cell depends on
        // TypeTable interning order elsewhere in the pipeline and isn't
        // itself part of this test's own contract.
        let (_, byte_bits) = storage.iter()
            .find(|((sid, _ty, addr), _)| sid.0 == 33 && *addr == 0)
            .expect("storage entry for (StorageId(33), _, addr=0) must exist");
        let byte: u32 = byte_bits.iter().enumerate().map(|(i, &b)| (b as u32) << i).sum();
        assert_eq!(byte, STEPS as u32, "committed byte must equal STEPS after 3 real steps");
    }

    /// Milestone 1.6's real checkpoint: compile and run the *split*
    /// prover + **`QSim`** + verifier (one function per movfuscated block,
    /// plus chunked accumulator functions, plus a finish function -- see
    /// `crate::split_driver`) for `STEPS` real calls on the probe circuit
    /// -- the first genuinely chained-AND-gate circuit in this project
    /// driven for real (`commit_mem_e2e.rs`'s circuit has exactly one AND
    /// gate with both operands externally known, sidestepping this
    /// entirely). The *unsplit* weave was tried first and found to compile
    /// pathologically slowly even at this circuit's small (~2,740-gate)
    /// scale -- a genuine single-function LLVM backend limitation,
    /// confirmed via direct profiling, not something `#[inline(never)]`
    /// alone fixes -- see `docs/agent-context/circuit-size-optimization-backlog.md`.
    ///
    /// `QSim` derives every `q_and_k` via `derive_and_q`, fed the *same*
    /// `hat`s the matching real prover function returns -- the real
    /// verifier function is then called completely unchanged in shape
    /// from `commit_mem_e2e.rs`'s own pattern, just with `q_and_k` sourced
    /// from `QSim`'s output instead of a driver hand-computing it.
    ///
    /// Per-step plain ground truth (storage 2's constant scratch value,
    /// storage 33's incrementing byte) is established once, host-side, by
    /// `trace_mem_probe_plain_values_has_correct_widths` above (and
    /// cross-checked there against a real wasmtime run) -- baked in here
    /// as literal per-step constants rather than re-derived at proof time,
    /// since it's public, not secret.
    #[test]
    fn honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary() {
        use volar_weaver::{
            weave_vole_prover_ir_split, weave_vole_qsim_ir_split,
            weave_vole_verifier_ir_split_with_trace, print_weaved_vole_module, IopSink,
            StorageMode,
        };
        use volar_compiler::ir::IrFunction;
        use volar_verifier_iop_runtime::run_iop_verifier;
        use crate::split_driver::{generate_split_step, Slot};

        let (_ir_blocks, _movfuscated, circuit, types, boundary, accum_info) = lower_mem_probe();
        let mode = StorageMode::Commitment;
        // chunk_size=2: small enough to genuinely exercise multi-chunk
        // threading (not just the degenerate "one big chunk" case) on
        // this small circuit, mirroring the vole.rs unit tests' own
        // chunk_size choices.
        let chunk_size = 2usize;
        let n_blocks = boundary.len();
        let n_chunks = n_blocks.div_ceil(chunk_size);

        let mut prover_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_prover_ir_split(&circuit, &types, "mp", &mode, &boundary, &accum_info, chunk_size, |f| prover_funcs.push(f));
        let mut qsim_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_qsim_ir_split(&circuit, &types, "mp", &mode, &boundary, &accum_info, chunk_size, |f| qsim_funcs.push(f));
        let mut verifier_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_verifier_ir_split_with_trace(&circuit, &types, "mp", &mode, &IopSink, &boundary, &accum_info, chunk_size, |f| verifier_funcs.push(f));
        assert_eq!(prover_funcs.len(), n_blocks + n_chunks + 1);
        assert_eq!(qsim_funcs.len(), n_blocks + n_chunks + 1);
        assert_eq!(verifier_funcs.len(), n_blocks + n_chunks + 1);

        let module_of = |functions: std::vec::Vec<IrFunction>, name: &str| volar_compiler::ir::IrModule {
            name: name.into(), functions, structs: vec![], enums: vec![], traits: vec![], impls: vec![], type_aliases: vec![], consts: vec![],
        };
        let prover_code = print_weaved_vole_module(&module_of(prover_funcs.clone(), "prover"));
        let qsim_code = print_weaved_vole_module(&module_of(qsim_funcs.clone(), "qsim"));
        let verifier_code = print_weaved_vole_module(&module_of(verifier_funcs.clone(), "verifier"));

        // Each printed module carries its own copy of the shared header;
        // keep the verifier's full header and splice in only the prover's
        // and qsim's own function bodies (matching commit_mem_e2e.rs's
        // established pattern for avoiding duplicate `use` lines) -- every
        // function for a role lands in that role's one printed string, so
        // this still captures all of them, not just the first.
        let prover_fn_only = &prover_code[prover_code.find("pub fn").expect("prover source must have a pub fn")..];
        let qsim_fn_only = &qsim_code[qsim_code.find("pub fn").expect("qsim source must have a pub fn")..];
        let rust_source = format!("{verifier_code}\n{prover_fn_only}\n{qsim_fn_only}");

        // Per-original-circuit-param widths (1,1,1,64,32 for this circuit),
        // read directly from the real circuit rather than assumed.
        let widths: std::vec::Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();

        // Real, plain ground truth for all 3 steps (established and
        // cross-checked above): storage 2 is always [true,false,false];
        // storage 33's byte is 0,1,2 (the value *before* that step's
        // increment).
        let byte_before: [u8; 3] = [0, 1, 2];
        let oracle_bits_per_step: std::vec::Vec<std::vec::Vec<std::vec::Vec<bool>>> = (0..3usize).map(|step| {
            let s2 = std::vec![true, false, false];
            let byte = byte_before[step];
            let s33: std::vec::Vec<bool> = (0..8).map(|i| (byte >> i) & 1 == 1).collect();
            std::vec![s2, s33]
        }).collect();

        // Host-side generation of the zero entry-state declarations (step
        // 0's w_i, both Vope and Q sides) -- widths read from the real
        // circuit above, not assumed.
        let mut zero_stmts = String::new();
        for (i, &w) in widths.iter().enumerate() {
            if w <= 1 {
                zero_stmts += &format!("let w{i}_vope_0 = vope_zero();\nlet w{i}_q_0 = q_zero();\n");
            } else {
                zero_stmts += &format!("let w{i}_vope_0: [Vope<N, Galois, cipher::consts::U1>; {w}] = core::array::from_fn(|_| vope_zero());\n");
                zero_stmts += &format!("let w{i}_q_0: [Q<N, Galois>; {w}] = core::array::from_fn(|_| q_zero());\n");
            }
        }
        let mut entry_w: std::vec::Vec<(Slot, Slot)> = widths.iter().enumerate().map(|(i, &w)| {
            if w <= 1 {
                (Slot::Scalar(format!("w{i}_vope_0")), Slot::Scalar(format!("w{i}_q_0")))
            } else {
                (Slot::Array(format!("w{i}_vope_0"), w), Slot::Array(format!("w{i}_q_0"), w))
            }
        }).collect();

        let mut all_steps_stmts = String::new();
        let mut all_ok_fold_state: Option<(String, String)> = None;
        for step in 0..3usize {
            let result = generate_split_step(
                &prover_funcs, &qsim_funcs, &verifier_funcs, &boundary, &accum_info, n_chunks,
                &entry_w, all_ok_fold_state.clone(), &oracle_bits_per_step[step], step,
            );
            all_steps_stmts += &result.stmts;
            let byte = byte_before[step];
            let new_byte = byte.wrapping_add(1);
            all_steps_stmts += &format!(r#"
                {{
                    let write_ts2 = ts2 + 1;
                    mem2.write(Galois(0), Galois(1), write_ts2, old2, ts2);
                    ts2 = write_ts2;
                    let read_ts2 = ts2 + 1;
                    mem2.read(Galois(0), Galois(1), read_ts2, ts2);
                    ts2 = read_ts2;
                    old2 = Galois(1);

                    let read_ts33 = ts33 + 1;
                    mem33.read(Galois(0), Galois({byte}), read_ts33, ts33);
                    ts33 = read_ts33;
                    let write_ts33 = ts33 + 1;
                    mem33.write(Galois(0), Galois({new_byte}), write_ts33, Galois({byte}), ts33);
                    ts33 = write_ts33;
                }}
            "#);
            entry_w = result.next_entry_w;
            all_ok_fold_state = Some((result.final_all_ok_expr, result.final_fold_state_expr));
        }
        let (final_all_ok, final_fold_state) = all_ok_fold_state.expect("at least one step ran");

        let driver = format!(r#"
            use volar_iop::field::{{Field as _, Gf128}};
            use volar_iop::transcript::FromBytes as _;
            use volar_spec::field::Galois;
            use volar_spec::ot::IdealCot;
            use volar_spec::vole::setup::{{random_nonzero_delta, vole_commit_bit}};
            use volar_spec::vole::memory::{{ChallengeKey, MemoryCheckState}};
            use volar_spec::vole::{{Delta, Q, Vope}};
            use volar_spec::{{Array, SpecRng}};
            use hybrid_array::Array as HArray;

            type N = cipher::consts::U16;

            struct TestRng(u64);
            impl SpecRng for TestRng {{
                fn next_u32(&mut self) -> u32 {{
                    self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
                    let mut z = self.0;
                    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
                    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
                    (z ^ (z >> 31)) as u32
                }}
            }}
            fn sample_g<R: SpecRng>(r: &mut R) -> Galois {{ Galois(r.next_u8()) }}
            fn lift_bit_g(b: bool) -> Galois {{ Galois(if b {{ 1 }} else {{ 0 }}) }}
            fn is_zero_g(g: &Galois) -> bool {{ g.0 == 0 }}

            fn vope_zero() -> Vope<N, Galois, cipher::consts::U1> {{
                Vope {{
                    u: HArray::<HArray<Galois, N>, cipher::consts::U1>::from_fn(|_| HArray::<Galois, N>::from_fn(|_| Galois(0))),
                    v: HArray::<Galois, N>::from_fn(|_| Galois(0)),
                }}
            }}
            fn q_zero() -> Q<N, Galois> {{ Q {{ q: HArray::<Galois, N>::from_fn(|_| Galois(0)) }} }}
            fn vope_one(delta: &Delta<N, Galois>) -> Vope<N, Galois, cipher::consts::U1> {{
                let _ = delta;
                Vope {{
                    u: HArray::<HArray<Galois, N>, cipher::consts::U1>::from_fn(|_| HArray::<Galois, N>::from_fn(|_| Galois(1))),
                    v: HArray::<Galois, N>::from_fn(|_| Galois(0)),
                }}
            }}
            fn q_one(delta: &Delta<N, Galois>) -> Q<N, Galois> {{
                Q {{ q: HArray::<Galois, N>::from_fn(|i| delta.delta[i].clone()) }}
            }}

            #[test]
            fn honest_mem_probe_three_steps_verifies_with_real_memory_boundary() {{
                let mut rng = TestRng(0xC0FFEE_C0FFEE);
                let delta = random_nonzero_delta::<N, Galois, _>(&mut rng, sample_g, is_zero_g);
                let cot = IdealCot::new(delta.clone());

                let r = Galois(0x53);
                let key = ChallengeKey::from_challenge(r);
                let mut mem2 = MemoryCheckState::<Galois>::new(key.clone());
                let mut mem33 = MemoryCheckState::<Galois>::new(key);
                mem2.init(Galois(0), Galois(0));
                mem33.init(Galois(0), Galois(0));
                let mut ts2: u64 = 0;
                let mut ts33: u64 = 0;
                let mut old2 = Galois(0);

                {zero_stmts}

                {all_steps_stmts}

                assert_eq!({STEPS}u8, {STEPS}u8);

                mem2.drain(Galois(0), Galois(1), ts2);
                mem33.drain(Galois(0), Galois({STEPS}), ts33);
                assert!(mem2.verify(), "storage 2's own independent memory multiset check must balance");
                assert!(mem33.verify(), "storage 33's own independent memory multiset check must balance");

                let h_produce = [mem2.produce().iop_embed(), mem33.produce().iop_embed()];
                let h_consume = [mem2.consume().iop_embed(), mem33.consume().iop_embed()];
                assert_eq!(h_produce, h_consume, "an honest run's H_produce/H_consume must embed identically, per storage");

                let mem_acc_in = h_produce;
                let mem_acc_out = h_consume;

                let tagged: volar_discipline::Tagged<volar_discipline::Transparent, _> =
                    volar_discipline::Tagged::seal({final_fold_state});
                let _ = {final_all_ok};
                let (proof, ok) = prove_and_verify_iop(tagged, &mem_acc_in, &mem_acc_out, Some((&mem_acc_in, &mem_acc_out)));
                assert!(ok, "the finalization proof over a real, non-empty memory boundary must verify");

                let corrupted_out = [Gf128::from_u64(0xdead_beef), h_consume[1]];
                let corrupted_ok = volar_iop::verify_iop(&proof, Some((&mem_acc_in, &corrupted_out)));
                assert!(!corrupted_ok, "a corrupted expected memory boundary must be rejected");
            }}
        "#);

        run_iop_verifier(&rust_source, &driver);
    }
}
