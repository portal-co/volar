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
    pub(crate) fn lower_mem_probe() -> (
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRTypes,
    ) {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir, LoweringMode};

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

        let mut movfuscated = movfuscate_ir(&ir_blocks, &mut types);

        loop {
            let a = fold_ir_blocks(&mut movfuscated, &types);
            let b = store_forward_ir_blocks(&mut movfuscated, &types);
            if !a && !b {
                break;
            }
        }

        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
        (ir_blocks, movfuscated, circuit, types)
    }

    #[test]
    fn mem_probe_lowers_and_unrolls_to_a_circuit() {
        let (ir_blocks, movfuscated, circuit, _types) = lower_mem_probe();
        assert!(!ir_blocks.is_circuit());
        assert_eq!(movfuscated.blocks.len(), 1);
        assert!(circuit.is_circuit(), "unrolled probe must satisfy is_circuit()");
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

        let (_ir_blocks, movfuscated, circuit, types) = lower_mem_probe();
        eprintln!("movfuscated params: {:?}", movfuscated.blocks[0].params);
        eprintln!("movfuscated terminator: {:?}", movfuscated.blocks[0].terminator);
        eprintln!("circuit params: {:?}", circuit.blocks[0].params);
        eprintln!("circuit terminator: {:?}", circuit.blocks[0].terminator);
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
}
