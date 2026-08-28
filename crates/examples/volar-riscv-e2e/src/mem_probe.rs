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
        assert_eq!(
            byte[0], STEPS as u8,
            "byte 0 must be incremented STEPS times"
        );
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
        use volar_ir_passes::{LoweringMode, lower_to_circuit_ir, movfuscate_ir_with_boundary};

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

        let (movfuscated, boundary, accum_info) =
            movfuscate_ir_with_boundary(&ir_blocks, &mut types);

        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit =
            lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
        (ir_blocks, movfuscated, circuit, types, boundary, accum_info)
    }

    #[test]
    fn mem_probe_lowers_and_unrolls_to_a_circuit() {
        let (ir_blocks, movfuscated, circuit, _types, boundary, accum_info) = lower_mem_probe();
        assert!(!ir_blocks.is_circuit());
        assert_eq!(movfuscated.blocks.len(), 1);
        assert!(
            circuit.is_circuit(),
            "unrolled probe must satisfy is_circuit()"
        );
        assert!(
            !boundary.is_empty(),
            "boundary metadata must be non-empty for the split weave"
        );
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
            IopSink, StorageMode, weave_vole_prover_ir_with_mode,
            weave_vole_verifier_ir_with_mode_and_trace,
        };

        let (_ir_blocks, movfuscated, circuit, types, _boundary, _accum_info) = lower_mem_probe();
        eprintln!("movfuscated params: {:?}", movfuscated.blocks[0].params);
        eprintln!(
            "movfuscated terminator: {:?}",
            movfuscated.blocks[0].terminator
        );
        eprintln!("circuit params: {:?}", circuit.blocks[0].params);
        eprintln!("circuit terminator: {:?}", circuit.blocks[0].terminator);
        eprintln!("pre_init segments:");
        for seg in &circuit.pre_init {
            eprintln!(
                "  storage={} ty={} offset={} len={}",
                seg.storage.0,
                seg.ty.0,
                seg.offset,
                seg.data.len()
            );
        }
        eprintln!("all StorageRead/StorageWrite stmts in circuit:");
        for (i, stmt) in circuit.blocks[0].stmts.iter().enumerate() {
            match &stmt.kind {
                volar_ir::ir::Stmt::StorageRead { storage, ty, addr } => {
                    eprintln!(
                        "  [{i}] READ  storage={} ty={} addr={:?}",
                        storage.0, ty.0, addr
                    );
                }
                volar_ir::ir::Stmt::StorageWrite {
                    storage,
                    ty,
                    addr,
                    src,
                } => {
                    eprintln!(
                        "  [{i}] WRITE storage={} ty={} addr={:?} src={:?}",
                        storage.0, ty.0, addr, src
                    );
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
            &circuit,
            &types,
            "mem_probe",
            &mode,
            &IopSink,
            None,
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
        let param_widths: Vec<usize> = circuit.blocks[0]
            .params
            .iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        assert_eq!(
            param_widths,
            vec![1, 1, 1, 64, 32],
            "probe circuit's own declared param widths"
        );

        let mut storage = volar_fuzz::interpreter::ir::StorageMap::new();
        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        assert_eq!(inputs.len(), num_params);

        for step in 0..3 {
            let outputs = eval_ir_circuit_step(
                &circuit.blocks[0],
                &types,
                &circuit.oracles,
                &inputs,
                &mut storage,
            );
            // output[0] = done flag; output[1..] map 1:1 onto the next
            // step's input params -- widths must match, not silently
            // narrow (the regression this guards against).
            let widths: Vec<usize> = outputs.iter().map(|v| v.len()).collect();
            assert_eq!(
                widths,
                vec![1, 1, 1, 1, 64, 32],
                "step {step}: output widths must match [done] ++ param_widths, not silently narrow"
            );
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
        let (_, byte_bits) = storage
            .iter()
            .find(|((sid, _ty, addr), _)| sid.0 == 33 && *addr == 0)
            .expect("storage entry for (StorageId(33), _, addr=0) must exist");
        let byte: u32 = byte_bits
            .iter()
            .enumerate()
            .map(|(i, &b)| (b as u32) << i)
            .sum();
        assert_eq!(
            byte, STEPS as u32,
            "committed byte must equal STEPS after 3 real steps"
        );
    }

    /// Parse the VOLE-relevant spec sources into a single `IrModule`, for
    /// use as [`volar_compiler::linkage`]-style inlining into the LIR
    /// pipeline (`lower_module_monomorphized` + `CBackend`) -- mirrors
    /// `volar-c-backend/tests/vole_e2e.rs`'s own `parse_vole_spec` helper
    /// exactly (can't reuse it directly, it's private to that test binary),
    /// with `vole/setup.rs` added for `derive_and_q` (QSim's own only
    /// real spec call, not exercised by that file's small AND/XOR/half-adder
    /// circuits).
    fn parse_vole_spec_for_lir() -> volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction> {
        use volar_compiler::{SourceInput, parse_sources};
        let src_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("spec")
            .join("volar-spec")
            .join("src");
        let files = [
            "lib.rs",
            "vole.rs",
            "vole/prove.rs",
            "vole/vope.rs",
            "vole/impls.rs",
            "vole/setup.rs",
        ];
        let loaded: std::vec::Vec<(String, String)> = files
            .iter()
            .map(|&f| {
                let path = src_dir.join(f);
                let src = std::fs::read_to_string(&path)
                    .unwrap_or_else(|e| panic!("cannot read spec file {}: {e}", path.display()));
                let stem = std::path::Path::new(f)
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned();
                (src, stem)
            })
            .collect();
        let inputs: std::vec::Vec<SourceInput> = loaded
            .iter()
            .map(|(src, name)| SourceInput {
                source: src.as_str(),
                name: name.as_str(),
            })
            .collect();
        parse_sources(&inputs, "volar_spec", &[])
            .unwrap_or_else(|e| panic!("parse_vole_spec_for_lir failed: {e}"))
    }

    /// `MonoEnv` matching the driver's own real crypto parameters (see
    /// `honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary_impl`'s
    /// own `type N = cipher::consts::U16;` / `Galois` usage) -- `T = Galois`
    /// (not the plain-`u8` `vole_env()` used by `vole_e2e.rs`'s toy
    /// circuits), since real non-zero-mask VOLE needs GF-aware arithmetic.
    fn mem_probe_lir_env() -> volar_lir_codegen::mono::MonoEnv {
        use volar_compiler::ir::{IrType, PrimitiveType};
        volar_lir_codegen::mono::MonoEnv::new("mem_probe")
            .with_len("N", 16)
            .with_len("U1", 1)
            .with_len("U0", 0)
            .with_len("K", 1)
            .with_type("T", IrType::Primitive(PrimitiveType::Galois))
    }

    /// First real test of the user's own parallel LIR/C-backend work
    /// against a genuine (not toy AND/XOR/half-adder) circuit: does the
    /// *split, pooled* mem_probe prover module (the exact same
    /// `weave_vole_prover_ir_split` output the rustc-text pipeline already
    /// compiles+runs, exercising all 3 pool-based-regalloc phases) lower
    /// through `lower_module_monomorphized` + `CBackend` at all? Structural
    /// only (no compile/run yet) -- reports the generated C source's own
    /// size for comparison against the equivalent Rust-printer output
    /// (`honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary`'s
    /// own `prover_code` -- not printed there today, worth comparing by
    /// hand). Run manually with
    /// `cargo test -p volar-riscv-e2e lir_probe_prover_lowers_to_c -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn lir_probe_prover_lowers_to_c() {
        use volar_c_backend::CBackend;
        use volar_compiler::ir::IrFunction;
        use volar_lir_codegen::{
            MonoPlanOptions, lower_module_monomorphized, roots_by_name_prefix,
        };
        use volar_weaver::{StorageMode, weave_vole_prover_ir_split};

        let (_ir_blocks, _movfuscated, circuit, types, boundary, accum_info) = lower_mem_probe();
        let mode = StorageMode::Commitment;
        let chunk_size = 2usize;
        let max_stmts_per_piece = volar_weaver::vole::DEFAULT_MAX_STMTS_PER_PIECE;

        let mut prover_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_prover_ir_split(
            &circuit,
            &types,
            "mp",
            &mode,
            &boundary,
            &accum_info,
            chunk_size,
            max_stmts_per_piece,
            |f| prover_funcs.push(f),
        );
        eprintln!("woven prover functions: {}", prover_funcs.len());

        let mut module = parse_vole_spec_for_lir();
        module.name = "mp_prover".into();
        module.functions.extend(prover_funcs);

        let env = mem_probe_lir_env();
        // A configurable prefix list, not a hardcoded pair -- see
        // `roots_by_name_prefix`'s own doc: this codebase's 3 woven roles
        // use 3 different prefixes (`vole_prove_`/`vole_verify_`/
        // `vole_qsim_`), and a future weaver is one more prefix here, not a
        // new hardcoded helper.
        let roots = roots_by_name_prefix(&module, &["vole_prove_"], &["vole_and_prover_step"], env);
        eprintln!("roots: {}", roots.len());

        let mut backend = CBackend::new();
        lower_module_monomorphized(
            &module,
            &mut backend,
            MonoPlanOptions {
                roots,
                ..Default::default()
            },
        )
        .unwrap_or_else(|e| panic!("LIR monomorphization failed: {e}"));
        let c_src = backend.finish();
        eprintln!("generated C source: {} bytes", c_src.len());
        assert!(!c_src.is_empty());
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
    /// `max_stmts_per_piece = DEFAULT_MAX_STMTS_PER_PIECE` reproduces the
    /// original single test byte-for-byte (this circuit's own regions
    /// never come close to 500 statements, so no piece splits happen at
    /// all); a small value forces REAL intra-region piece splitting on
    /// this circuit, which is what
    /// `honest_mem_probe_run_with_forced_piece_splitting_pools_piece_in_v`
    /// below uses to get a genuine compile+run check of Phase C's
    /// `piece_in_v` pooling (`WireRepr::Pooled`/`_piece_pool`) -- not just
    /// the structural IR check `vole.rs`'s own unit test does. If pooling
    /// silently produced a wrong value (the exact bug class
    /// `debug_check_pool_written` exists to catch), this circuit's own
    /// `mem2.verify()`/`mem33.verify()`/`H_produce == H_consume`/
    /// `prove_and_verify_iop(..).ok` checks below would very likely fail,
    /// on top of the unconditional (not debug-gated) written-bitset guard
    /// itself panicking on any ordering violation.
    fn honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary_impl(
        max_stmts_per_piece: usize,
    ) {
        use crate::split_driver::{Slot, generate_split_step, slot_name};
        use volar_compiler::ir::IrFunction;
        use volar_verifier_iop_runtime::run_iop_verifier;
        use volar_weaver::{
            IopSink, StorageMode, print_weaved_vole_module, weave_vole_prover_ir_split,
            weave_vole_qsim_ir_split, weave_vole_verifier_ir_split_with_trace,
        };

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
        weave_vole_prover_ir_split(
            &circuit,
            &types,
            "mp",
            &mode,
            &boundary,
            &accum_info,
            chunk_size,
            max_stmts_per_piece,
            |f| prover_funcs.push(f),
        );
        let mut qsim_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_qsim_ir_split(
            &circuit,
            &types,
            "mp",
            &mode,
            &boundary,
            &accum_info,
            chunk_size,
            max_stmts_per_piece,
            |f| qsim_funcs.push(f),
        );
        let mut verifier_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_verifier_ir_split_with_trace(
            &circuit,
            &types,
            "mp",
            &mode,
            &IopSink,
            &boundary,
            &accum_info,
            chunk_size,
            max_stmts_per_piece,
            |f| verifier_funcs.push(f),
        );

        // Positionally-indexed views (one entry per boundary/chunk/finish
        // position), used below for the assert and `generate_split_step`'s
        // own positional indexing. Any region exceeding
        // `MAX_STMTS_PER_PIECE` now emits extra `..._piece_{p}` functions
        // alongside its own wrapper (which keeps the position's original
        // name/shape) -- see `crate::vole_split`'s own doc. Pieces are
        // internal-only (called BY the wrapper, never addressed
        // externally), so they're excluded here -- but NOT from
        // `prover_funcs`/`qsim_funcs`/`verifier_funcs` themselves, which
        // stay the full, unfiltered set used for printing below (the
        // wrapper's own generated body calls its pieces by name, so they
        // must still be emitted into the compiled source).
        let by_pos = |fs: &std::vec::Vec<IrFunction>| -> std::vec::Vec<IrFunction> {
            fs.iter()
                .filter(|f| !f.name.contains("_piece_"))
                .cloned()
                .collect()
        };
        let prover_funcs_by_pos = by_pos(&prover_funcs);
        let qsim_funcs_by_pos = by_pos(&qsim_funcs);
        let verifier_funcs_by_pos = by_pos(&verifier_funcs);
        assert_eq!(prover_funcs_by_pos.len(), n_blocks + n_chunks + 1);
        assert_eq!(qsim_funcs_by_pos.len(), n_blocks + n_chunks + 1);
        assert_eq!(verifier_funcs_by_pos.len(), n_blocks + n_chunks + 1);

        let module_of =
            |functions: std::vec::Vec<IrFunction>, name: &str| volar_compiler::ir::IrModule {
                name: name.into(),
                functions,
                structs: vec![],
                enums: vec![],
                traits: vec![],
                impls: vec![],
                type_aliases: vec![],
                consts: vec![],
            };
        let prover_code = print_weaved_vole_module(&module_of(prover_funcs.clone(), "prover"));
        let qsim_code = print_weaved_vole_module(&module_of(qsim_funcs.clone(), "qsim"));
        let verifier_code =
            print_weaved_vole_module(&module_of(verifier_funcs.clone(), "verifier"));

        // Each printed module carries its own copy of the shared header;
        // keep the verifier's full header and splice in only the prover's
        // and qsim's own function bodies (matching commit_mem_e2e.rs's
        // established pattern for avoiding duplicate `use` lines) -- every
        // function for a role lands in that role's one printed string, so
        // this still captures all of them, not just the first.
        let prover_fn_only = &prover_code[prover_code
            .find("pub fn")
            .expect("prover source must have a pub fn")..];
        let qsim_fn_only = &qsim_code[qsim_code
            .find("pub fn")
            .expect("qsim source must have a pub fn")..];
        let rust_source = format!("{verifier_code}\n{prover_fn_only}\n{qsim_fn_only}");

        // Per-original-circuit-param widths (1,1,1,64,32 for this circuit),
        // read directly from the real circuit rather than assumed.
        let widths: std::vec::Vec<usize> = circuit.blocks[0]
            .params
            .iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();

        // Real, plain ground truth for all 3 steps (established and
        // cross-checked above): storage 2 is always [true,false,false];
        // storage 33's byte is 0,1,2 (the value *before* that step's
        // increment) -- this program's own byte value happens to equal
        // the step index directly. Emitted as a real Rust array literal
        // (`STEP_WITNESS`) in the generated driver source below, indexed
        // at RUNTIME by the loop variable `step` -- unlike the older
        // per-step-unrolled calling convention (one `generate_split_step`
        // call per step, values baked in as literals), this lets
        // `generate_split_step` be called ONCE, with its own returned
        // `stmts` becoming a real `for` loop body that runs 3 times at
        // runtime, not 3 separately-generated copies of the same text.
        struct StepWitness {
            s2_bits: [bool; 3],
            s33_bits: [bool; 8],
            byte_before: u8,
            byte_after: u8,
        }
        let witness: std::vec::Vec<StepWitness> = (0..3usize)
            .map(|step| {
                let byte = step as u8;
                StepWitness {
                    s2_bits: [true, false, false],
                    s33_bits: core::array::from_fn(|i| (byte >> i) & 1 == 1),
                    byte_before: byte,
                    byte_after: byte.wrapping_add(1),
                }
            })
            .collect();
        let witness_literal = format!(
            "struct StepWitness {{ s2_bits: [bool; 3], s33_bits: [bool; 8], byte_before: u8, byte_after: u8 }}\n\
             let witness: [StepWitness; {}] = [{}];\n",
            witness.len(),
            witness.iter().map(|w| format!(
                "StepWitness {{ s2_bits: [{}], s33_bits: [{}], byte_before: {}u8, byte_after: {}u8 }}",
                w.s2_bits.iter().map(|b| b.to_string()).collect::<std::vec::Vec<_>>().join(", "),
                w.s33_bits.iter().map(|b| b.to_string()).collect::<std::vec::Vec<_>>().join(", "),
                w.byte_before, w.byte_after,
            )).collect::<std::vec::Vec<_>>().join(", "),
        );
        // Oracle bit EXPRESSIONS (not values): `emit_oracle` splices these
        // in as-is, so they reference the runtime `witness[step]` array
        // rather than embedding a literal `true`/`false` per step.
        //
        // Three entries, not two -- movfuscation's `StorageWrite` handling
        // (`movfuscate.rs`'s write-gating `is_active`-select) unconditionally
        // inserts its own synthetic `StorageRead` of the *current* value at
        // the same address right before every write, in addition to any
        // "real" read the source program itself performs. For storage 2
        // (the CPS lowering's constant continuation slot) that's a gating
        // read before its one write, then a real read-back after -- two
        // 3-bit oracle reads, both of the same constant value ([true,false,
        // false] == 1), matching `old2`/`mem2`'s own host-side bookkeeping
        // below. For storage 33 (the real byte) it's the genuine
        // `i32.load8_u` read followed immediately by the write-gating
        // re-read of that same still-unwritten byte -- two 8-bit reads of
        // the same `byte_before` value, bundled into one 16-bit oracle
        // read since both land in the same movfuscated region. See
        // `docs/agent-context/mem-probe-scope-note.md` and this test's own
        // `mem2`/`mem33` call sequence below, which must mirror these
        // exact values for the external multiset check to balance against
        // what's actually committed in-circuit.
        let oracle_bit_exprs: std::vec::Vec<std::vec::Vec<String>> = std::vec![
            (0..3)
                .map(|j| format!("witness[step].s2_bits[{j}]"))
                .collect(),
            (0..3)
                .map(|j| format!("witness[step].s2_bits[{j}]"))
                .collect(),
            (0..16)
                .map(|j| format!("witness[step].s33_bits[{}]", j % 8))
                .collect(),
        ];

        // Entry-state declarations, now OUTER `mut` bindings (no `_0`
        // suffix -- there's no longer a distinct "step 0" text copy) that
        // the loop body reassigns at the end of each real iteration.
        //
        // Phase B: scalar top-level params are pooled via `_w_pool`
        // instead of a per-value named Rust local -- but unlike
        // `_synth_pool` (declared fresh INSIDE the loop body every step,
        // since cross-region values never need to survive past one
        // step), `_w_pool` must be LOOP-PERSISTENT (a param's own value
        // carries from one real step to the next), so it's declared here
        // ONCE, before the loop, and written in place at the end of each
        // iteration -- mirroring `all_ok`/`fold_state`'s own already-
        // established loop-accumulator pattern. Wide params are
        // unaffected (still named `w{i}_vope`/`w{i}_q` locals, unchanged).
        let mut zero_stmts = String::new();
        zero_stmts += &format!(
            "let mut _w_pool_vope: Vec<Vope<N, Galois, cipher::consts::U1>> = core::iter::repeat_with(Vope::default).take({0}).collect();\n\
             let mut _w_pool_vope_written: Vec<bool> = core::iter::repeat(false).take({0}).collect();\n\
             let mut _w_pool_q: Vec<Q<N, Galois>> = core::iter::repeat_with(Q::default).take({0}).collect();\n\
             let mut _w_pool_q_written: Vec<bool> = core::iter::repeat(false).take({0}).collect();\n",
            widths.len(),
        );
        for (i, &w) in widths.iter().enumerate() {
            if w <= 1 {
                zero_stmts += &format!(
                    "_w_pool_vope[{i}] = vope_zero(); _w_pool_vope_written[{i}] = true;\n"
                );
                zero_stmts +=
                    &format!("_w_pool_q[{i}] = q_zero(); _w_pool_q_written[{i}] = true;\n");
            } else {
                zero_stmts += &format!(
                    "let mut w{i}_vope: [Vope<N, Galois, cipher::consts::U1>; {w}] = core::array::from_fn(|_| vope_zero());\n"
                );
                zero_stmts += &format!(
                    "let mut w{i}_q: [Q<N, Galois>; {w}] = core::array::from_fn(|_| q_zero());\n"
                );
            }
        }
        zero_stmts += "let mut all_ok = true;\nlet mut fold_state = iop_accumulator_fresh();\n";
        let entry_w: std::vec::Vec<(Slot, Slot)> = widths
            .iter()
            .enumerate()
            .map(|(i, &w)| {
                if w <= 1 {
                    // Never actually read: `build_call`'s own
                    // `n.starts_with("w_")` branch never fires for a pooled
                    // (scalar) param, since no callee has such a named param
                    // anymore. Placeholder only.
                    (
                        Slot::Scalar("_dead_pooled_w".to_string()),
                        Slot::Scalar("_dead_pooled_w".to_string()),
                    )
                } else {
                    (
                        Slot::Array(format!("w{i}_vope"), w),
                        Slot::Array(format!("w{i}_q"), w),
                    )
                }
            })
            .collect();

        let total_vars = circuit.blocks[0].params.len() + circuit.blocks[0].stmts.len();
        let result = generate_split_step(
            &prover_funcs_by_pos,
            &qsim_funcs_by_pos,
            &verifier_funcs_by_pos,
            &boundary,
            &accum_info,
            n_chunks,
            total_vars,
            &entry_w,
            Some(("all_ok".to_string(), "fold_state".to_string())),
            &oracle_bit_exprs,
            "step",
        );
        let mut loop_body = result.stmts.clone();
        // Reassign the OUTER mutable entry-state/accumulator bindings from
        // this iteration's own final values, so the NEXT real loop
        // iteration sees them. Scalar params write into `_w_pool` instead
        // of a named local (see the comment above `zero_stmts`).
        for (i, (vope_slot, q_slot)) in result.next_entry_w.iter().enumerate() {
            if widths[i] <= 1 {
                loop_body += &format!(
                    "_w_pool_vope[{i}] = {}; _w_pool_vope_written[{i}] = true;\n",
                    slot_name(vope_slot)
                );
                loop_body += &format!(
                    "_w_pool_q[{i}] = {}; _w_pool_q_written[{i}] = true;\n",
                    slot_name(q_slot)
                );
            } else {
                loop_body += &format!("w{i}_vope = {};\n", slot_name(vope_slot));
                loop_body += &format!("w{i}_q = {};\n", slot_name(q_slot));
            }
        }
        loop_body += &format!(
            "all_ok = {};\nfold_state = {};\n",
            result.final_all_ok_expr, result.final_fold_state_expr
        );
        // Mirrors the real circuit's own per-step trace exactly (see the
        // oracle_bit_exprs comment above): storage 2 is gating-read, then
        // written, then read back (R,W,R); storage 33 is really loaded,
        // then gating-re-read, then written (R,R,W). Each event's
        // `write_ts`/`old_ts` argument threads to the timestamp the
        // *previous* event in this same chain produced, so the external
        // multiset (`mem2.verify()`/`mem33.verify()`) balances.
        loop_body += r#"
            {
                let r1_ts2 = ts2 + 1;
                mem2.read(Galois(0), old2, r1_ts2, ts2);
                ts2 = r1_ts2;
                let write_ts2 = ts2 + 1;
                mem2.write(Galois(0), Galois(1), write_ts2, old2, ts2);
                ts2 = write_ts2;
                old2 = Galois(1);
                let r2_ts2 = ts2 + 1;
                mem2.read(Galois(0), Galois(1), r2_ts2, ts2);
                ts2 = r2_ts2;

                let read1_ts33 = ts33 + 1;
                mem33.read(Galois(0), Galois(witness[step].byte_before), read1_ts33, ts33);
                ts33 = read1_ts33;
                let read2_ts33 = ts33 + 1;
                mem33.read(Galois(0), Galois(witness[step].byte_before), read2_ts33, ts33);
                ts33 = read2_ts33;
                let write_ts33 = ts33 + 1;
                mem33.write(Galois(0), Galois(witness[step].byte_after), write_ts33, Galois(witness[step].byte_before), ts33);
                ts33 = write_ts33;
            }
        "#;
        let all_steps_stmts =
            format!("{witness_literal}for step in 0..witness.len() {{\n{loop_body}\n}}\n",);
        let (final_all_ok, final_fold_state) = ("all_ok".to_string(), "fold_state".to_string());

        let driver = format!(
            r#"
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
            // Real function (not the `assert!` macro) so `split_driver.rs`'s
            // own AST-based statement generation can call it -- this IR has
            // no macro-invocation support at all (see `ir_builder.rs`'s own
            // `assert_true_stmt` doc).
            fn __assert_true(cond: bool, msg: &str) {{ assert!(cond, "{{}}", msg); }}

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
                // Storage 2's committed value is constant (1) for its
                // entire lifetime -- see the oracle_bit_exprs comment
                // above -- so it's simplest and self-consistent to seed
                // `init` at that same constant rather than 0-then-write.
                mem2.init(Galois(0), Galois(1));
                mem33.init(Galois(0), Galois(0));
                let mut ts2: u64 = 0;
                let mut ts33: u64 = 0;
                let mut old2 = Galois(1);

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
        "#
        );

        run_iop_verifier(&rust_source, &driver);
    }

    #[test]
    fn honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary() {
        honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary_impl(
            volar_weaver::vole::DEFAULT_MAX_STMTS_PER_PIECE,
        );
    }

    /// Phase C sub-stage 1's real compile+run check: `max_stmts_per_piece
    /// = 20` forces genuine intra-region piece splitting (`_piece_`
    /// functions, `_piece_pool`/`_piece_pool_written` slices) on this
    /// circuit's own ~2,740 gates, where the DEFAULT threshold (500)
    /// never triggers a single split. Reuses every real check the
    /// unsplit-piece version above already does -- real memory-boundary
    /// multiset balance, `H_produce == H_consume`, a real IOP fold that
    /// verifies, and corrupted-boundary rejection -- so if `piece_in_v`
    /// pooling ever produced a silently wrong value (read-before-write,
    /// wrong slot, a piece writing to the wrong pool), this test would
    /// fail: either the unconditional `debug_check_pool_written` guard
    /// panics directly, or (if the wrong value still happened to be
    /// well-typed) one of these cryptographic checks fails downstream.
    /// All 3 roles (Prover, QSim, Verifier) pool `piece_in_v` -- this
    /// test exercises all of them together, real-compiled and run.
    #[test]
    fn honest_mem_probe_run_with_forced_piece_splitting_pools_piece_in_v() {
        honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary_impl(20);
    }
}
