//! Hand-authored WAT source for the Milestone-1 RISC-V interpreter.
//!
//! Deliberately hand-written (not rustc-emitted) so that, if
//! `lower_waffle_module` rejects something, it's a real lowering gap and
//! not an artifact of what LLVM happened to emit. Only ops
//! `docs/wasm-feature-support.md` lists as supported are used: i32
//! arithmetic/bitwise/shifts/compares, `i32.load`/`i32.store`, `br`/`br_if`,
//! multi-memory. No globals, no `br_table`, no `call_indirect`.
//!
//! Registers/PC/step-counter are WASM locals (WAFFLE lowers a function's
//! locals to SSA on its own -- loop-carried locals become the natural
//! per-iteration SSA values, cheaper than routing them through memory).
//! Only the data RAM -- addressed by genuinely runtime-computed byte
//! offsets from `LW`/`SW` -- lives in linear memory, since that's the one
//! thing with no local-based equivalent, and exactly the one thing that
//! needs `StorageMode::Commitment` downstream.
//!
//! Register read/write dispatch is a **flat sequence** of independent
//! `(if (i32.eq idx K) (then ...))` statements (mutually exclusive by
//! construction, since `idx` can only equal one `K`), not a nested
//! if/else-if chain -- semantically identical, far simpler to write
//! correctly, and lowers to a sequence of independent diamonds rather than
//! deep nesting.

use crate::interp::{MAX_STEPS, RESULT_ADDR};

/// RV32I base opcodes for the instructions this interpreter supports.
mod opcode {
    pub const ADDI: u32 = 0x13;
    pub const ADD: u32 = 0x33;
    pub const LW: u32 = 0x03;
    pub const SW: u32 = 0x23;
    pub const BEQ: u32 = 0x63;
    pub const JAL: u32 = 0x6F;
}

/// Escape raw bytes as a WAT string-data literal (`\XX` hex per byte --
/// always valid, never relies on printable-ASCII passthrough).
fn escape_bytes(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 4);
    for b in bytes {
        s.push_str(&format!("\\{b:02x}"));
    }
    s
}

/// Flat register-read dispatch: `local.set $out` defaults to zero (covers
/// `x0` and any index outside the registers this program uses), then one
/// `if` per architectural register `1..=5` overwrites it on a match.
fn get_reg(idx_local: &str, out_local: &str) -> String {
    let mut s = format!("(local.set {out_local} (i32.const 0))\n");
    for r in 1..=5u32 {
        s.push_str(&format!(
            "(if (i32.eq (local.get {idx_local}) (i32.const {r})) (then (local.set {out_local} (local.get $r{r}))))\n"
        ));
    }
    s
}

/// Flat register-write dispatch: writes `val_local` into whichever `$rN`
/// local `idx_local` names (`1..=5`); a write to `x0` (or any other index)
/// is a no-op, matching RV32I's hardwired-zero register.
fn set_reg(idx_local: &str, val_local: &str) -> String {
    let mut s = String::new();
    for r in 1..=5u32 {
        s.push_str(&format!(
            "(if (i32.eq (local.get {idx_local}) (i32.const {r})) (then (local.set $r{r} (local.get {val_local}))))\n"
        ));
    }
    s
}

/// Build the full interpreter module for a given program + initial data
/// RAM image. `code_bytes`/`data_bytes` become `data` segments in two
/// separate linear memories (`$code`, `$data`) -- multi-memory is fully
/// supported by `volar-vaffle-target`, and this split is exactly what lets
/// `StorageMode::Commitment` be applied to only `$data` later.
pub fn interpreter_wat(code_bytes: &[u8], data_bytes: &[u8]) -> String {
    let code_pages = code_bytes.len().div_ceil(65536).max(1);
    let data_pages = data_bytes.len().div_ceil(65536).max(1);

    format!(
        r#"(module
  (memory $code {code_pages})
  (memory $data {data_pages})
  (export "code" (memory $code))
  (export "data" (memory $data))
  (data (memory $code) (i32.const 0) "{code_data}")
  (data (memory $data) (i32.const 0) "{init_data}")

  (func (export "run") (result i32)
    (local $pc i32)
    (local $next_pc i32)
    (local $steps i32)
    (local $halted i32)

    (local $r1 i32) (local $r2 i32) (local $r3 i32) (local $r4 i32) (local $r5 i32)

    (local $word i32)
    (local $opcode i32)
    (local $rd i32) (local $rs1 i32) (local $rs2 i32)
    (local $rs1v i32) (local $rs2v i32)
    (local $imm_i i32) (local $imm_s i32) (local $imm_b i32) (local $imm_j i32)
    (local $addr i32)
    (local $result i32)

    (block $exit
      (loop $L
        ;; bounded-loop guard: safety net, never hit by an honest program.
        (br_if $exit (i32.ge_s (local.get $steps) (i32.const {max_steps})))
        (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
        (br_if $exit (local.get $halted))

        ;; fetch: real dynamic load from public, constant-initialized code
        ;; memory -- ordinary data access, not authenticated storage.
        (local.set $word (i32.load $code (local.get $pc)))

        ;; decode fixed fields (RV32 standard layout).
        (local.set $opcode (i32.and (local.get $word) (i32.const 0x7F)))
        (local.set $rd     (i32.and (i32.shr_u (local.get $word) (i32.const 7)) (i32.const 0x1F)))
        (local.set $rs1    (i32.and (i32.shr_u (local.get $word) (i32.const 15)) (i32.const 0x1F)))
        (local.set $rs2    (i32.and (i32.shr_u (local.get $word) (i32.const 20)) (i32.const 0x1F)))

        ;; I-type immediate: arithmetic-shift already sign-extends word[31:20].
        (local.set $imm_i (i32.shr_s (local.get $word) (i32.const 20)))

        ;; S-type immediate: high bits from imm_i's sign-extended top,
        ;; low 5 bits from word[11:7].
        (local.set $imm_s
          (i32.or
            (i32.and (local.get $imm_i) (i32.const -32))
            (i32.and (i32.shr_u (local.get $word) (i32.const 7)) (i32.const 0x1F))))

        ;; B-type immediate: reassemble bit-by-bit, then sign-extend the
        ;; 13-bit field by shifting bit 12 up to bit 31 and back down (arith).
        (local.set $imm_b
          (i32.shr_s
            (i32.shl
              (i32.or
                (i32.or
                  (i32.and (i32.shr_u (local.get $word) (i32.const 19)) (i32.const 0x1000))
                  (i32.and (i32.shl (local.get $word) (i32.const 4)) (i32.const 0x800)))
                (i32.or
                  (i32.and (i32.shr_u (local.get $word) (i32.const 20)) (i32.const 0x7E0))
                  (i32.and (i32.shr_u (local.get $word) (i32.const 7)) (i32.const 0x1E))))
              (i32.const 19))
            (i32.const 19)))

        ;; J-type immediate: reassemble bit-by-bit, then sign-extend the
        ;; 21-bit field by shifting bit 20 up to bit 31 and back down (arith).
        (local.set $imm_j
          (i32.shr_s
            (i32.shl
              (i32.or
                (i32.or
                  (i32.and (i32.shr_u (local.get $word) (i32.const 11)) (i32.const 0x100000))
                  (i32.and (local.get $word) (i32.const 0xFF000)))
                (i32.or
                  (i32.and (i32.shr_u (local.get $word) (i32.const 9)) (i32.const 0x800))
                  (i32.and (i32.shr_u (local.get $word) (i32.const 20)) (i32.const 0x7FE))))
              (i32.const 11))
            (i32.const 11)))

        ;; register reads (harmless if the current opcode doesn't need one).
{get_rs1v}
{get_rs2v}

        ;; default next pc; each opcode below may override it.
        (local.set $next_pc (i32.add (local.get $pc) (i32.const 4)))

        ;; ADDI: rd = rs1v + imm_i
        (if (i32.eq (local.get $opcode) (i32.const {op_addi}))
          (then
            (local.set $result (i32.add (local.get $rs1v) (local.get $imm_i)))
{set_result_to_rd}
          ))

        ;; ADD: rd = rs1v + rs2v
        (if (i32.eq (local.get $opcode) (i32.const {op_add}))
          (then
            (local.set $result (i32.add (local.get $rs1v) (local.get $rs2v)))
{set_result_to_rd}
          ))

        ;; LW: rd = data[rs1v + imm_i]  (JALR-style dynamic addressing --
        ;; ordinary bounds-free load; commitment mode is enforced downstream).
        (if (i32.eq (local.get $opcode) (i32.const {op_lw}))
          (then
            (local.set $addr (i32.add (local.get $rs1v) (local.get $imm_i)))
            (local.set $result (i32.load $data (local.get $addr)))
{set_result_to_rd}
          ))

        ;; SW: data[rs1v + imm_s] = rs2v ; this program's designated halt.
        (if (i32.eq (local.get $opcode) (i32.const {op_sw}))
          (then
            (local.set $addr (i32.add (local.get $rs1v) (local.get $imm_s)))
            (i32.store $data (local.get $addr) (local.get $rs2v))
            (local.set $halted (i32.const 1))
          ))

        ;; BEQ: if rs1v == rs2v, next_pc = pc + imm_b
        (if (i32.eq (local.get $opcode) (i32.const {op_beq}))
          (then
            (if (i32.eq (local.get $rs1v) (local.get $rs2v))
              (then (local.set $next_pc (i32.add (local.get $pc) (local.get $imm_b)))))
          ))

        ;; JAL: rd = pc + 4 (link, using next_pc's still-default value);
        ;; next_pc = pc + imm_j.
        (if (i32.eq (local.get $opcode) (i32.const {op_jal}))
          (then
            (local.set $result (local.get $next_pc))
{set_result_to_rd}
            (local.set $next_pc (i32.add (local.get $pc) (local.get $imm_j)))
          ))

        (local.set $pc (local.get $next_pc))
        (br $L)
      )
    )

    (local.get $r3)
  )
)
"#,
        code_pages = code_pages,
        data_pages = data_pages,
        code_data = escape_bytes(code_bytes),
        init_data = escape_bytes(data_bytes),
        max_steps = MAX_STEPS,
        get_rs1v = get_reg("$rs1", "$rs1v"),
        get_rs2v = get_reg("$rs2", "$rs2v"),
        set_result_to_rd = set_reg("$rd", "$result"),
        op_addi = opcode::ADDI,
        op_add = opcode::ADD,
        op_lw = opcode::LW,
        op_sw = opcode::SW,
        op_beq = opcode::BEQ,
        op_jal = opcode::JAL,
    )
}

/// The Milestone-1 test program's WAT source, built from
/// [`crate::interp::assemble_program`] / [`crate::interp::initial_data_bytes`].
pub fn test_program_wat() -> String {
    interpreter_wat(&crate::interp::program_bytes(), &crate::interp::initial_data_bytes())
}

#[allow(dead_code)]
const _: i32 = RESULT_ADDR; // referenced by doc comments above; keep import live.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interpreter_wat_assembles() {
        let src = test_program_wat();
        wat::parse_str(&src).unwrap_or_else(|e| panic!("wat failed to assemble: {e}\n\n{src}"));
    }

    /// Fast, cheap sanity oracle: actually *run* the hand-authored WAT (via
    /// wasmtime, dev-only -- never part of the circuit) and check it agrees
    /// with the native Rust reference, before spending any time on the much
    /// slower weave/compile/run ZK pipeline. Isolates "is the WAT logic
    /// right" from "does the lowering pipeline handle it right".
    #[test]
    fn wat_interpreter_matches_native_reference_via_wasmtime() {
        let src = test_program_wat();
        let wasm_bytes = wat::parse_str(&src).expect("wat should assemble");

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &wasm_bytes).expect("module should validate");
        let mut store = wasmtime::Store::new(&engine, ());
        let instance =
            wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiation should succeed");

        let run = instance
            .get_typed_func::<(), i32>(&mut store, "run")
            .expect("run function should be exported with the right type");
        let actual_sum = run.call(&mut store, ()).expect("run should not trap");

        let data_mem = instance.get_memory(&mut store, "data").expect("data memory export");
        let mut result_bytes = [0u8; 4];
        data_mem
            .read(&store, crate::interp::RESULT_ADDR as usize, &mut result_bytes)
            .expect("result address should be in bounds");
        let actual_stored = i32::from_le_bytes(result_bytes);

        let program = crate::interp::assemble_program();
        let mut expected_mem = crate::interp::initial_data_bytes();
        let expected_sum = crate::interp::native_reference(&program, &mut expected_mem);
        let expected_stored = i32::from_le_bytes(
            expected_mem[crate::interp::RESULT_ADDR as usize..crate::interp::RESULT_ADDR as usize + 4]
                .try_into()
                .unwrap(),
        );

        assert_eq!(actual_sum, expected_sum, "WAT interpreter's returned sum must match native reference");
        assert_eq!(actual_stored, expected_stored, "WAT interpreter's stored result must match native reference");
    }

    /// The actual Milestone-1 checkpoint for this step: the real interpreter
    /// -- not the trivial "answer" module -- parsed via the real WASM
    /// frontend and lowered via the real `lower_waffle_module`, with zero
    /// hand-built IR. Real debugging is expected here (per the plan); this
    /// test is deliberately written to fail loudly with the lowering
    /// errors, not just a boolean, so a real gap is diagnosable in place.
    #[test]
    fn interpreter_wat_lowers_through_the_real_pipeline() {
        let wasm_bytes = wat::parse_str(&test_program_wat()).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module,
            &mut target,
            &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );

        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");
        assert_eq!(target.module.funcs.len(), 1, "expected exactly one lowered function");
    }

    /// The next step of the same claim: the lowered VAFFLE module continues
    /// through `lower_vaffle_to_ir` (Volar IR), `movfuscate_ir` (collapse the
    /// real branches/loop into a single self-looping block), and
    /// `lower_to_circuit_ir` (bounded unroll into a genuine `is_circuit()`
    /// circuit) -- with zero detour through Boolar IR. This is the real
    /// interpreter's control flow (opcode dispatch + the loop back-edge),
    /// not a hand-built fixture.
    #[test]
    fn interpreter_ir_movfuscates_and_unrolls_to_a_circuit() {
        use volar_ir_passes::LoweringMode;

        let (ir_blocks, movfuscated, circuit, _types, _bit_ty, _boundary, _accum_info) =
            lower_interpreter(crate::interp::MAX_STEPS as u32, LoweringMode::Unconditional);
        assert!(
            ir_blocks.blocks.len() > 1,
            "the real interpreter has real control flow -- expected multiple blocks, got {}",
            ir_blocks.blocks.len()
        );
        assert!(!ir_blocks.is_circuit());
        assert_eq!(movfuscated.blocks.len(), 1, "movfuscate_ir must collapse to a single block");
        assert!(
            !movfuscated.is_circuit(),
            "the movfuscated block still self-loops (JumpCond back to Block(0)); \
             is_circuit() requires an unconditional Jmp(Return), which only the \
             *unrolled* circuit has"
        );
        assert!(circuit.is_circuit(), "unrolled interpreter must satisfy is_circuit()");
    }

    /// Shared helper: parse+lower the interpreter WAT all the way to a
    /// genuine `is_circuit()` IR circuit, unrolled `limit` times in `mode`.
    /// Returns every intermediate stage so callers can assert on whichever
    /// they need. `limit=1` with `WithTerminationFlag` is the shape the
    /// real driver uses (the movfuscated block already *is* "one step";
    /// the driver loops, threading state + the IOP fold accumulator across
    /// calls, until the returned flag says the program halted -- per the
    /// plan, "the resulting circuit will run a varied amount of times").
    ///
    /// Milestone 1.5 Step A (`virtualize_ir` block-skeleton dedup as a
    /// pre-movfuscation pass) was attempted and *reverted* here -- see
    /// `docs/agent-context/circuit-size-optimization-backlog.md` for the
    /// full writeup of the real, virt-internal incompatibility found
    /// (`movfuscate_ir`'s per-slot-position type-uniformity requirement
    /// isn't met by virt's dispatcher/handler/setup block shapes) and the
    /// separate, genuine VAFFLE bug this investigation *did* fix
    /// (`plan_functions`'s continuation-type arity, `lower_to_ir.rs`).
    ///
    /// Milestone 1.5 Step B: no longer runs post-movfuscation
    /// `optimize_to_fixpoint` -- a direct A/B measurement found it
    /// *increases* and_count ~24x (115,780 -> 2,771,980) on this circuit,
    /// almost certainly `store_forward_ir_blocks` duplicating AND-gate-
    /// containing expressions across multiple use sites (see the backlog
    /// doc's "Step B: store_forward_ir_blocks" section -- deferred, not
    /// fixed, since dropping the pass alone is a strict improvement here
    /// with no known downside). This also sidesteps a separate, confirmed
    /// incompatibility: `movfuscate_ir_with_boundary`'s boundary metadata
    /// is invalidated by that same optimization pass (it renumbers/deletes
    /// statements), so returning boundary metadata directly from this
    /// shared helper (below) requires skipping it anyway.
    pub(crate) fn lower_interpreter(
        limit: u32,
        mode: volar_ir_passes::LoweringMode,
    ) -> (
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRBlocks,
        volar_ir::ir::IRTypes,
        volar_ir::ir::IRTypeId,
        std::vec::Vec<volar_ir_passes::MovfuscBlockBoundary>,
        volar_ir_passes::MovfuscAccumInfo,
    ) {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary};

        let wasm_bytes = wat::parse_str(&test_program_wat()).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module,
            &mut target,
            &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        let (mut ir_blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);

        // Optimize *before* movfuscation, to fixpoint: constant-fold and
        // store-forward feed each other (folding can turn a computed address
        // into a constant that store-forwarding can then match, and vice
        // versa), so alternate both until neither changes anything. Safe
        // here (unlike post-movfuscation) since it runs *before*
        // `movfuscate_ir_with_boundary` even computes boundary metadata.
        optimize_to_fixpoint(&mut ir_blocks, &types, &mut fold_ir_blocks, &mut store_forward_ir_blocks);

        let (movfuscated, boundary, accum_info) = movfuscate_ir_with_boundary(&ir_blocks, &mut types);

        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, limit, mode);
        (ir_blocks, movfuscated, circuit, types, bit_ty, boundary, accum_info)
    }

    /// Alternate two boolean-returning "did anything change" passes until
    /// neither reports a change -- the minimal fixpoint driver so
    /// `fold_ir_blocks`/`store_forward_ir_blocks` (which can each unlock
    /// further opportunities for the other) both run to exhaustion rather
    /// than just once each.
    fn optimize_to_fixpoint<P: Clone>(
        blocks: &mut volar_ir::ir::IRBlocks<P>,
        types: &volar_ir::ir::IRTypes,
        pass_a: &mut dyn FnMut(&mut volar_ir::ir::IRBlocks<P>, &volar_ir::ir::IRTypes) -> bool,
        pass_b: &mut dyn FnMut(&mut volar_ir::ir::IRBlocks<P>, &volar_ir::ir::IRTypes) -> bool,
    ) {
        loop {
            let a = pass_a(blocks, types);
            let b = pass_b(blocks, types);
            if !a && !b {
                break;
            }
        }
    }

    /// M1.3: weave the real interpreter's one-step batch circuit into VOLE
    /// prover + verifier modules with `StorageMode::Commitment` (the data
    /// RAM) and `IopSink` fold-trace threading on the verifier -- the exact
    /// combination Milestone 1 needs, all real, none hand-built.
    ///
    /// Deliberately does **not** call `print_weaved_vole_module` on this
    /// circuit: after the width-at-rest fix + the `lower_to_circuit_ir`
    /// return-padding fix (both `docs/agent-context/boolar-ir-conflicts.md`),
    /// this circuit's woven prover is ~3.27M statements -- stringifying
    /// that (measured directly: killed at 125GB+ physical footprint on a
    /// 32GB machine, climbing) is not safe to run as part of the default
    /// test suite. Every property this test checks is checked structurally
    /// instead (function names, param shapes, non-empty trace) -- no
    /// printing needed.
    ///
    /// **`#[ignore]`d**: even *without* printing, just building the woven
    /// `IrModule`/`IrFunction` trees in memory (the `weave_vole_prover_ir_with_mode`/
    /// `weave_vole_verifier_ir_with_mode_and_trace` calls below) measured at
    /// **~12.2GB peak RSS / ~148s**, run in isolation with nothing else
    /// concurrently allocating -- too large for routine `cargo test` runs
    /// (a parallel run with other tests can push this well past that). This
    /// is the exact, expected `and_count = 2,771,980` scale problem
    /// Milestone 1.5 exists to fix (`docs/agent-context/circuit-size-optimization-backlog.md`
    /// and the plan's "Milestone 1.5" section) -- not a new regression.
    /// Re-enable only once Milestone 1.5 Step B's split-the-verifier design
    /// makes weaving this circuit's woven verifier safe at default-suite
    /// scale (per the plan's own "Verification for this sub-milestone").
    /// Until then, use `cargo test -p volar-riscv-e2e --release
    /// "wat_gen::tests::interpreter_batch_circuit_weaves_with_commitment_and_trace"
    /// -- --exact --ignored` under RSS monitoring to re-run manually.
    #[test]
    #[ignore]
    fn interpreter_batch_circuit_weaves_with_commitment_and_trace() {
        use volar_ir_passes::LoweringMode;
        use volar_weaver::{
            IopSink, StorageMode, weave_vole_prover_ir_with_mode,
            weave_vole_verifier_ir_with_mode_and_trace,
        };

        // limit=1, WithTerminationFlag: the movfuscated block already *is*
        // one step; the driver (M1.5/M1.6) loops, threading state and the
        // IOP fold accumulator across calls, until the returned flag says
        // the program halted.
        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, _boundary, _accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);

        let mode = StorageMode::Commitment;

        let (prover_module, prover_trace) =
            weave_vole_prover_ir_with_mode(&circuit, &types, "riscv_step", &mode, None);
        let pf = &prover_module.inner().functions[0];
        assert_eq!(pf.name, "vole_prove_ir_riscv_step", "missing woven prover fn");

        let (verifier_module, verifier_trace) = weave_vole_verifier_ir_with_mode_and_trace(
            &circuit, &types, "riscv_step", &mode, &IopSink, None,
        );
        let vf = &verifier_module.inner().functions[0];
        assert_eq!(vf.name, "vole_verify_ir_riscv_step", "missing woven verifier fn");
        assert!(
            vf.params.iter().any(|p| p.name.starts_with("oracle_rd_")),
            "commitment-mode reads must be oracle params: {:?}",
            vf.params.iter().map(|p| &p.name).collect::<Vec<_>>()
        );
        // Fix A array-batched `q_and_0`, `q_and_1`, ... into one bare
        // `q_and` array param -- match the array param itself, not a
        // per-gate name prefix.
        assert!(
            vf.params.iter().any(|p| p.name == "q_and"),
            "at least one AND gate expected (IopSink folds every one via iop_fold_gate): {:?}",
            vf.params.iter().map(|p| &p.name).collect::<Vec<_>>()
        );

        // Commitment mode: both memories' reads/writes are traced (code
        // fetch is public-but-still-committed under today's uniform-mode
        // API; data RAM is the one that actually matters for soundness).
        assert!(!prover_trace.entries.is_empty(), "expected a non-empty memory trace for one interpreter step");
        assert_eq!(prover_trace.entries.len(), verifier_trace.entries.len());
    }

    /// Cheap sanity check (no printing -- that alone was 51GB RSS on the
    /// unreduced circuit): count woven statements directly, before and
    /// after the pre/post-movfuscation optimization passes, to see
    /// whether fold_ir_blocks/store_forward_ir_blocks are actually
    /// shrinking anything.
    /// Diagnostic only (not a correctness assertion) -- run manually with
    /// `cargo test -p volar-riscv-e2e count_woven_statements -- --ignored --nocapture`
    /// to inspect circuit-size counts and the movfuscated Poly-statement
    /// shape distribution (width/degree/AND-monomial-count histograms).
    /// Historical finding (pre-width-at-rest-fix): 99% of Poly statements
    /// were width 1, since `volar-vaffle-target` bit-decomposed every
    /// i32/i64 value across block boundaries before movfuscation ever ran
    /// -- see `docs/agent-context/boolar-ir-conflicts.md` conflicts #3-4.
    /// Fixed: block params/branch args now stay packed at rest (only
    /// unpacked on-the-fly inside a block), which alone cut woven prover
    /// statements from 5,342,031 to 3,272,222 (~55% off the original
    /// 7,249,314 baseline). Further optimization ideas (bitwise-op
    /// widening, movfuscation-level dedup, polynomial merging) are
    /// deliberately deferred -- see
    /// `docs/agent-context/circuit-size-optimization-backlog.md`.
    #[test]
    #[ignore]
    fn count_woven_statements_after_optimization() {
        use volar_ir_passes::LoweringMode;
        use volar_weaver::{StorageMode, weave_vole_prover_ir_with_mode};

        let (ir_blocks, movfuscated, circuit, types, _bit_ty, _boundary, _accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);
        eprintln!("pre-movfuscation blocks: {}", ir_blocks.blocks.len());
        eprintln!(
            "pre-movfuscation total stmts: {}",
            ir_blocks.blocks.iter().map(|b| b.stmts.len()).sum::<usize>()
        );
        eprintln!("movfuscated blocks: {}", movfuscated.blocks.len());
        eprintln!(
            "movfuscated total stmts: {}",
            movfuscated.blocks.iter().map(|b| b.stmts.len()).sum::<usize>()
        );
        eprintln!("circuit (post lower_to_circuit_ir) blocks: {}", circuit.blocks.len());
        eprintln!(
            "circuit total stmts: {}",
            circuit.blocks.iter().map(|b| b.stmts.len()).sum::<usize>()
        );

        // and_count: mirrors volar_weaver::vole's private count_ir_ands --
        // total per-lane AND checks (one q_and/hat/r_and triple each in
        // the woven verifier), the thing that actually drives verifier
        // weave cost (not just Poly-statement count).
        {
            use volar_ir::ir::Stmt as DiagStmt;
            fn diag_width(ty: &volar_ir::ir::IRTypeId, types: &volar_ir::ir::IRTypes) -> usize {
                use volar_ir_common::{IrType, Type};
                match &types.0[ty.0 as usize] {
                    IrType::Primitive(Type::Bit) => 1,
                    IrType::Primitive(Type::_8) => 8,
                    IrType::Primitive(Type::_16) => 16,
                    IrType::Primitive(Type::_32) => 32,
                    IrType::Primitive(Type::_64) => 64,
                    IrType::Primitive(Type::_128) => 128,
                    IrType::Primitive(Type::_256) => 256,
                    IrType::Vec(k, _) => *k,
                    _ => 1,
                }
            }
            let mut and_count = 0usize;
            for stmt in &circuit.blocks[0].stmts {
                if let DiagStmt::Poly { ty, coeffs, .. } = &stmt.kind {
                    let width = diag_width(ty, &types);
                    for (mono, coeff) in coeffs {
                        if *coeff % 2 == 1 && mono.len() >= 2 {
                            and_count += (mono.len() - 1) * width;
                        }
                    }
                }
            }
            eprintln!("and_count (verifier q_and/hat/r_and param triples): {and_count}");
        }

        // Diagnostic: distribution of Poly statement shapes in the
        // movfuscated circuit, to see how many actually qualify for the
        // width-collapsed weave path (width > 1, max monomial degree <= 2)
        // vs. fall back to per-lane unrolling.
        use volar_ir::ir::Stmt;
        use volar_ir_common::{IrType, Type};
        fn test_type_width(ty: &volar_ir::ir::IRTypeId, types: &volar_ir::ir::IRTypes) -> usize {
            match &types.0[ty.0 as usize] {
                IrType::Primitive(Type::Bit) => 1,
                IrType::Primitive(Type::_8) => 8,
                IrType::Primitive(Type::_16) => 16,
                IrType::Primitive(Type::_32) => 32,
                IrType::Primitive(Type::_64) => 64,
                IrType::Primitive(Type::_128) => 128,
                IrType::Primitive(Type::_256) => 256,
                IrType::Vec(k, _) => *k,
                _ => 1,
            }
        }
        let mut poly_total = 0usize;
        let mut poly_wide = 0usize;
        let mut poly_wide_supported = 0usize;
        let mut max_degree_hist: std::collections::BTreeMap<usize, usize> = std::collections::BTreeMap::new();
        let mut and_monos_hist: std::collections::BTreeMap<usize, usize> = std::collections::BTreeMap::new();
        for stmt in &circuit.blocks[0].stmts {
            if let Stmt::Poly { ty, coeffs, .. } = &stmt.kind {
                poly_total += 1;
                let width = test_type_width(ty, &types);
                let max_deg = coeffs.keys().map(|m| m.len()).max().unwrap_or(0);
                let and_count = coeffs.iter().filter(|(m, c)| *c % 2 == 1 && m.len() == 2).count();
                *max_degree_hist.entry(max_deg).or_insert(0) += 1;
                if width > 1 {
                    poly_wide += 1;
                    *and_monos_hist.entry(and_count).or_insert(0) += 1;
                    if max_deg <= 2 {
                        poly_wide_supported += 1;
                    }
                }
            }
        }
        eprintln!("poly stmts total: {poly_total}, wide (width>1): {poly_wide}, wide+degree<=2: {poly_wide_supported}");
        eprintln!("max-degree histogram: {max_degree_hist:?}");
        eprintln!("and-monomial-count histogram (wide only): {and_monos_hist:?}");

        let mode = StorageMode::Commitment;
        let (module, trace) =
            weave_vole_prover_ir_with_mode(&circuit, &types, "riscv_step", &mode, None);
        let total_woven_stmts: usize =
            module.inner().functions.iter().map(|f| f.body.stmts.len()).sum();
        eprintln!("woven prover total stmts: {total_woven_stmts}");
        eprintln!("memory trace entries: {}", trace.entries.len());
    }

    /// Direct A/B check of a surprising Step B measurement (now documented
    /// in `docs/agent-context/circuit-size-optimization-backlog.md`): does
    /// post-movfuscation `optimize_to_fixpoint` (`fold_ir_blocks`/
    /// `store_forward_ir_blocks` -- `lower_interpreter` itself no longer
    /// runs this post-movfuscation, precisely because of this finding)
    /// actually *increase* and_count on the real interpreter circuit,
    /// rather than decrease it as its name suggests? Computes and_count
    /// both ways from the exact same movfuscated starting point, via the
    /// real weaver (`weave_vole_verifier_ir_split_with_trace`'s own
    /// q_and-param counting -- not a re-implemented diagnostic formula),
    /// so this is directly trustworthy either way. Kept as a permanent
    /// regression-guard for the finding, not just a one-off measurement.
    /// `#[ignore]`d (real interpreter scale); run manually under RSS
    /// monitoring.
    #[test]
    #[ignore]
    fn compare_and_count_with_and_without_post_movfuscation_optimization() {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_weaver::{StorageMode, weave_vole_verifier_ir_split_with_trace, IopSink};

        let wasm_bytes = wat::parse_str(&test_program_wat()).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");
        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module, &mut target, &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty());
        let (mut ir_blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);
        optimize_to_fixpoint(&mut ir_blocks, &types, &mut fold_ir_blocks, &mut store_forward_ir_blocks);

        let and_count_via_real_weaver = |movfuscated_types: &mut volar_ir::ir::IRTypes, movfuscated: &volar_ir::ir::IRBlocks, boundary: &[volar_ir_passes::MovfuscBlockBoundary], accum_info: &volar_ir_passes::MovfuscAccumInfo| -> usize {
            let bit_ty = movfuscated_types.intern(IRType::Primitive(Type::Bit));
            let circuit = lower_to_circuit_ir(movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
            let mut total = 0usize;
            weave_vole_verifier_ir_split_with_trace(
                &circuit, movfuscated_types, "cmp", &StorageMode::Commitment, &IopSink, boundary, accum_info, 1,
                |f| total += f.params.iter().filter(|p| p.name.starts_with("q_and_")).count(),
            );
            total
        };

        // WITHOUT post-movfuscation optimization.
        let mut types_no_opt = types.clone();
        let (movfuscated_no_opt, boundary_no_opt, accum_info_no_opt) = movfuscate_ir_with_boundary(&ir_blocks, &mut types_no_opt);
        let and_count_no_opt = and_count_via_real_weaver(&mut types_no_opt, &movfuscated_no_opt, &boundary_no_opt, &accum_info_no_opt);

        // WITH post-movfuscation optimization.
        let mut types_with_opt = types.clone();
        let (mut movfuscated_with_opt, boundary_with_opt, accum_info_with_opt) = movfuscate_ir_with_boundary(&ir_blocks, &mut types_with_opt);
        optimize_to_fixpoint(&mut movfuscated_with_opt, &types_with_opt, &mut fold_ir_blocks, &mut store_forward_ir_blocks);
        // NOTE: boundary_with_opt/accum_info_with_opt are almost certainly
        // invalid after this optimization pass -- if the real weaver panics
        // or produces a nonsensical count here, *that itself* is the answer
        // to "is boundary safe to reuse post-optimization" (no).
        let and_count_with_opt_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            and_count_via_real_weaver(&mut types_with_opt, &movfuscated_with_opt, &boundary_with_opt, &accum_info_with_opt)
        }));

        eprintln!("and_count WITHOUT post-movfuscation optimization: {and_count_no_opt}");
        match &and_count_with_opt_result {
            Ok(n) => eprintln!("and_count WITH post-movfuscation optimization (boundary reused, may be invalid): {n}"),
            Err(_) => eprintln!("and_count WITH post-movfuscation optimization: PANICKED (boundary is stale/invalid post-optimization, as suspected)"),
        }
    }

    /// Milestone 1.5 Step B measurement: does the split verifier/prover
    /// weave actually bound per-function size on the *real* interpreter
    /// circuit -- the whole point of the split, checked directly rather
    /// than assumed. `#[ignore]`d (real interpreter scale); run manually
    /// under RSS monitoring:
    /// `cargo test -p volar-riscv-e2e --release measure_split_weave_on_real_interpreter -- --ignored --nocapture`
    #[test]
    #[ignore]
    fn measure_split_weave_on_real_interpreter() {
        use volar_ir_passes::LoweringMode;
        use volar_weaver::{StorageMode, weave_vole_prover_ir_split, weave_vole_verifier_ir_split_with_trace, IopSink};

        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, boundary, accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);
        eprintln!("n_blocks (boundary entries): {}", boundary.len());

        let mode = StorageMode::Commitment;
        // chunk_size=8: bounds the accumulator's own params to O(8 * state
        // width) instead of O(n_blocks * state width) -- the whole point of
        // Milestone 1.5 Step B's combiner-splitting.
        let chunk_size = 8usize;

        let mut verifier_and_counts: std::vec::Vec<usize> = std::vec::Vec::new();
        let mut verifier_param_counts: std::vec::Vec<usize> = std::vec::Vec::new();
        let verifier_trace = weave_vole_verifier_ir_split_with_trace(
            &circuit, &types, "riscv_step", &mode, &IopSink, &boundary, &accum_info, chunk_size,
            |f| {
                // `q_and` is one array-batched param (Milestone 1.6's
                // 65535-arg-limit fix), not one scalar per gate -- read
                // its own declared array length instead of counting params.
                let and_count = match f.params.iter().find(|p| p.name == "q_and") {
                    Some(p) => match &p.ty {
                        volar_compiler::ir::IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                        _ => 0,
                    },
                    None => 0,
                };
                verifier_and_counts.push(and_count);
                verifier_param_counts.push(f.params.len());
                // Drop `f` here (Step B.4): in a real driver this is where
                // print_weaved_vole_module(&IrModule{functions: vec![f], ..})
                // would run, immediately followed by dropping the printed
                // string too, before the next function is even built.
            },
        );

        let mut prover_hats_counts: std::vec::Vec<usize> = std::vec::Vec::new();
        let mut prover_param_counts: std::vec::Vec<usize> = std::vec::Vec::new();
        let prover_trace = weave_vole_prover_ir_split(
            &circuit, &types, "riscv_step", &mode, &boundary, &accum_info, chunk_size,
            |f| {
                let hats_len = match f.return_type.as_ref().unwrap() {
                    volar_compiler::ir::IrType::Tuple(elems) => match elems.last().unwrap() {
                        volar_compiler::ir::IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                        _ => 0,
                    },
                    _ => 0,
                };
                prover_hats_counts.push(hats_len);
                prover_param_counts.push(f.params.len());
            },
        );

        eprintln!("verifier per-function q_and counts: {verifier_and_counts:?}");
        eprintln!("verifier per-function param counts: {verifier_param_counts:?}");
        eprintln!("verifier max param count: {}", verifier_param_counts.iter().max().unwrap());
        eprintln!("verifier total and_count (sum across functions): {}", verifier_and_counts.iter().sum::<usize>());
        eprintln!("prover per-function hats counts: {prover_hats_counts:?}");
        eprintln!("prover per-function param counts: {prover_param_counts:?}");
        eprintln!("prover max param count: {}", prover_param_counts.iter().max().unwrap());
        eprintln!("verifier trace entries: {}", verifier_trace.entries.len());
        eprintln!("prover trace entries: {}", prover_trace.entries.len());

        assert_eq!(
            verifier_and_counts, prover_hats_counts,
            "prover hats and verifier q_and must line up per block/combiner for interleaved driving"
        );
    }

    /// Trace the *plain* (non-cryptographic) values flowing through the
    /// real interpreter circuit across real steps, via `volar_fuzz`'s IR
    /// interpreter with a persistent storage map seeded from
    /// `circuit.pre_init` (unlike `mem_probe`'s own trace test, whose
    /// circuit happens to have an *empty* `pre_init` -- this circuit's
    /// pre_init is real and non-trivial, the actual program bytes + initial
    /// RAM words, so seeding it is required, not optional) -- establishes
    /// ground truth for the real driven test and cross-checks this
    /// interpreter (run through the real WASM pipeline + movfuscation)
    /// against `interp::native_reference`, an independent, hand-written
    /// oracle.
    ///
    /// **Currently fails, and is expected to** (kept as a regression guard
    /// for whoever picks this up, not a "should pass today" test). Five
    /// bugs found and fixed so far, each confirmed via a minimal isolated
    /// repro before touching the real interpreter:
    ///
    /// 1. `VaffleTarget::begin_function`'s discarded return-type hint.
    /// 2. `movfuscate_ir`'s state-slot type agreement (`SlotSig`/
    ///    `compute_position_groups`).
    /// 3. `lower_to_circuit_ir`'s MUX cascade conflating state and return
    ///    at the same output slots -- fixed by making them separate,
    ///    non-overlapping segments.
    /// 4. `lower_to_ir.rs`'s per-block `val_map` silently resolving any
    ///    VAFFLE cross-block (dominance-based) value reference to
    ///    `IRVarId(0)` -- fixed with `compute_cross_block_values` (spill
    ///    at definition, reload only where actually used).
    /// 5. `movfuscate.rs`'s `scatter_args_to_state` zero-filling any state
    ///    slot a jump target didn't explicitly cover, instead of passing
    ///    the combined block's own current value through -- silently
    ///    wiping loop-carried state the moment *any* block along a path
    ///    didn't itself thread it forward as an explicit arg (confirmed
    ///    via `minimal_dispatch_write_repro`, which now passes).
    ///
    /// With all five fixed, the circuit now shows **genuine, healthy
    /// progress** -- real values flowing and incrementing correctly (a
    /// `$steps`-shaped counter reaching 7, an address-shaped value
    /// stepping by 4 each time, matching a real word-array traversal) --
    /// confirmed via a manual 250-raw-step run. It does **not** yet halt
    /// within that budget, though: each real RISC-V instruction spans
    /// roughly ~30 raw movfuscated circuit calls (fetch/decode/dispatch
    /// through many of the interpreter's 120 original blocks), not 1, so
    /// the real program's ~27 instructions need on the order of 800-900
    /// raw steps -- far more than `interp::MAX_STEPS` (a *real-WASM-loop*
    /// bound, a different, smaller granularity than *raw circuit calls*).
    /// `eval_ir_circuit_step` itself is slow at this circuit's scale
    /// (~1s/step, since movfuscation runs every original block's own
    /// logic every single call) -- a real, expensive, but bounded cost of
    /// this debugging path, not a hang. Not yet run to actual completion;
    /// do that (with a step budget in the low thousands) before assuming
    /// anything beyond "halts and produces the right answer" remains
    /// broken.
    ///
    /// `#[ignore]`d: real interpreter scale, run manually:
    /// `cargo test -p volar-riscv-e2e --release trace_interpreter_plain_values_matches_native_reference -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn trace_interpreter_plain_values_matches_native_reference() {
        use volar_ir_passes::LoweringMode;
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, apply_pre_init, StorageMap};

        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, _boundary, _accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);

        let param_widths: Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        eprintln!("param widths: {param_widths:?}");

        let mut storage: StorageMap = StorageMap::new();
        apply_pre_init(&mut storage, &circuit.pre_init, &types);
        eprintln!("storage map after pre_init: {} entries", storage.len());

        let to_u64 = |v: &[bool]| -> u64 { v.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum() };

        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        let mut done = false;
        let mut step = 0usize;
        // ~30 raw circuit calls per real instruction (see this test's own
        // doc comment) -- 27 real instructions need on the order of
        // 800-900, not `interp::MAX_STEPS` (a different, real-WASM-loop
        // granularity). ~1s/step at this circuit's scale; budget real time
        // to run this.
        const RAW_STEP_BUDGET: usize = 1200;
        while !done && step < RAW_STEP_BUDGET {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            if step % 20 == 0 || done { eprintln!("step {step}: done={done} full_state={full_state:?}"); }
            // `outputs` is `[done, state[0..state_width], ret[0..ret_width]]` --
            // state and return are separate, non-overlapping segments; only
            // the first `param_widths.len()` slots are real next-state.
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "interpreter circuit must halt within RAW_STEP_BUDGET raw steps via its own termination flag");

        // Find whichever (StorageId, TypeId) pair holds the data RAM's
        // real byte contents (the one with pre-init data whose length
        // matches `initial_data_bytes()`) and read the result word back --
        // don't assume a specific storage id, discover it. Exact-length
        // match (not `>=`): the *code* segment is also `>= RESULT_ADDR + 4`
        // bytes long (32 vs. the data segment's 20), so `>=` matched both
        // and silently read the result word back from program bytes on a
        // wrong-but-plausible-looking address instead of real data RAM.
        let expected: i32 = crate::interp::initial_data_words().iter().sum();
        let mut found = false;
        for seg in &circuit.pre_init {
            if seg.data.len() as i32 == RESULT_ADDR + 4 {
                let bytes: Vec<u8> = (0..4).map(|i| {
                    let addr = (RESULT_ADDR + i) as u64;
                    let bits = &storage[&(seg.storage, seg.ty, addr)];
                    bits.iter().enumerate().map(|(j, &b)| (b as u8) << j).fold(0u8, |a, b| a | b)
                }).collect();
                let word = i32::from_le_bytes(bytes.try_into().unwrap());
                eprintln!("data RAM (storage={}) result word: {word} (expected {expected})", seg.storage.0);
                if word == expected {
                    found = true;
                }
            }
        }
        assert!(found, "some pre_init-seeded storage must hold the correct result word after real steps");
    }

    /// Diagnostic (not a correctness assertion): dump the real circuit's
    /// entry-state param widths and every real `StorageRead`/`StorageWrite`'s
    /// storage id -- needed to write a real driver rather than guess these
    /// (widths and storage-id assignment are both driven by WAFFLE/movfuscation
    /// internals, not the WAT source's declaration order; see
    /// `mem_probe.rs`'s own `dump_mem_probe_signatures` for the precedent
    /// that a single declared memory does NOT land at `StorageId::memory(0)`).
    /// Run manually: `cargo test -p volar-riscv-e2e dump_interpreter_signatures -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn dump_interpreter_signatures() {
        use volar_ir_passes::LoweringMode;

        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, boundary, accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);
        eprintln!("n_blocks: {}", boundary.len());
        eprintln!("circuit params: {:?}", circuit.blocks[0].params);
        let widths: std::vec::Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        eprintln!("circuit param widths: {widths:?}");
        eprintln!("circuit terminator: {:?}", circuit.blocks[0].terminator);

        let mut storage_ids: std::collections::BTreeSet<u32> = std::collections::BTreeSet::new();
        let mut read_count = 0usize;
        let mut write_count = 0usize;
        for stmt in circuit.blocks[0].stmts.iter() {
            match &stmt.kind {
                volar_ir::ir::Stmt::StorageRead { storage, ty, .. } => {
                    storage_ids.insert(storage.0);
                    read_count += 1;
                    let _ = ty;
                }
                volar_ir::ir::Stmt::StorageWrite { storage, ty, .. } => {
                    storage_ids.insert(storage.0);
                    write_count += 1;
                    let _ = ty;
                }
                _ => {}
            }
        }
        eprintln!("distinct storage ids referenced: {storage_ids:?}");
        eprintln!("total StorageRead count: {read_count}, StorageWrite count: {write_count}");
        eprintln!("pre_init segments:");
        for seg in &circuit.pre_init {
            eprintln!("  storage={} ty={} offset={} len={}", seg.storage.0, seg.ty.0, seg.offset, seg.data.len());
        }
        eprintln!("accum_info.init: done_acc={} next_pc.len()={} next_state.len()={} ret_vals.len()={}",
            accum_info.init.done_acc, accum_info.init.next_pc.len(), accum_info.init.next_state.len(), accum_info.init.ret_vals.len());
    }

    /// Feasibility check (not exercised by default) for Stage 2: does the
    /// *largest* generated split-verifier function (a chunk combiner, per
    /// `measure_split_weave_on_real_interpreter`'s own measurement, up to
    /// ~236K params) actually print+compile, on its own, before investing
    /// in the full interleaved driver?
    ///
    /// **Currently fails, and is expected to**: `rustc` hard-caps functions
    /// at 65535 arguments (`error: function can not have more than 65535
    /// arguments`) -- a real, non-negotiable compiler limit, not a
    /// performance/RSS issue that `--release` or more chunking headroom can
    /// route around. `chunk_size=8`'s largest chunk (`accum_chunk_6`) alone
    /// exceeds it. This is Stage 2's second genuine architectural blocker
    /// (see `trace_interpreter_plain_values_matches_native_reference`'s doc
    /// for the first) -- the fix is real design work Milestone 1.5's own
    /// plan flagged as a "complementary, fold in if it fits naturally"
    /// optimization and deferred: batch each function's own `hat`/`q_and`/
    /// `r_and` parameters into `[T; k]` array params (matching `entry_w`'s
    /// own `w_i_j` convention) instead of one scalar param per AND-gate
    /// lane, not a smaller `chunk_size` alone (per-block skew means some
    /// *single* blocks already carry thousands of gates). Left unfixed
    /// here deliberately, per the plan's own honest risk note.
    ///
    /// Run manually: `cargo test -p volar-riscv-e2e --release largest_chunk_function_compiles -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn largest_chunk_function_compiles() {
        use volar_ir_passes::LoweringMode;
        use volar_weaver::{StorageMode, weave_vole_verifier_ir_split_with_trace, print_weaved_vole_module, IopSink};

        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, boundary, accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);
        let mode = StorageMode::Commitment;
        let chunk_size = 8usize;

        let mut biggest: Option<volar_compiler::ir::IrFunction> = None;
        weave_vole_verifier_ir_split_with_trace(
            &circuit, &types, "riscv_step", &mode, &IopSink, &boundary, &accum_info, chunk_size,
            |f| {
                if biggest.as_ref().map(|b| b.params.len()).unwrap_or(0) < f.params.len() {
                    biggest = Some(f);
                }
            },
        );
        let f = biggest.expect("at least one function woven");
        eprintln!("largest function: {} with {} params", f.name, f.params.len());

        let module = volar_compiler::ir::IrModule {
            name: "riscv_step".into(), functions: vec![f], structs: vec![], enums: vec![],
            traits: vec![], impls: vec![], type_aliases: vec![], consts: vec![],
        };
        let code = print_weaved_vole_module(&module);
        eprintln!("printed source length: {} bytes", code.len());
        // Reuse the same "print -> temp Cargo project -> cargo test --release"
        // harness the real driven tests use (rather than volar-weaver's own
        // internal, crate-private `run_compile_check`, inaccessible from
        // here) -- a no-op driver is enough to force a real compile.
        volar_verifier_iop_runtime::run_iop_verifier(&code, "#[test]\nfn compiles() {}\n");
        eprintln!("compiled successfully");
    }

    /// Minimal isolation repro for the "circuit state never changes" bug
    /// found while investigating `trace_interpreter_plain_values_matches_native_reference`:
    /// a tiny loop that just writes a constant into a local once, then
    /// halts -- exercises the *same* self-loop/movfuscation/circuit-lowering
    /// mechanism as the real interpreter, at a scale small enough to reason
    /// about by hand, with **no** register-dispatch `if`-chains and **no**
    /// memory, to isolate whether the bug is in the core loop-carried-state
    /// mechanism itself or specific to the interpreter's larger shape.
    /// Run manually: `cargo test -p volar-riscv-e2e --release minimal_state_write_repro -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn minimal_state_write_repro() {
        minimal_state_write_repro_inner(true);
    }

    /// Same repro, but with `(result i32)` removed (mem_probe.rs's own
    /// shape) -- A/B tests whether Fix B's now-correct return-type
    /// handling is what's newly breaking state threading, vs. a
    /// pre-existing bug unrelated to it.
    /// Run manually: `cargo test -p volar-riscv-e2e --release minimal_state_write_repro_no_result -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn minimal_state_write_repro_no_result() {
        minimal_state_write_repro_inner(false);
    }

    fn minimal_state_write_repro_inner(with_result: bool) {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, StorageMap};

        let wat = if with_result {
            r#"(module
  (func (export "run") (result i32)
    (local $r1 i32) (local $steps i32) (local $halted i32)
    (block $exit
      (loop $L
        (br_if $exit (i32.ge_s (local.get $steps) (i32.const 3)))
        (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
        (br_if $exit (local.get $halted))
        (local.set $r1 (i32.const 4))
        (local.set $halted (i32.const 1))
        (br $L)
      )
    )
    (local.get $r1)
  )
)
"#.to_string()
        } else {
            r#"(module
  (func (export "run")
    (local $r1 i32) (local $steps i32) (local $halted i32)
    (block $exit
      (loop $L
        (br_if $exit (i32.ge_s (local.get $steps) (i32.const 3)))
        (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
        (br_if $exit (local.get $halted))
        (local.set $r1 (i32.const 4))
        (local.set $halted (i32.const 1))
        (br $L)
      )
    )
  )
)
"#.to_string()
        };
        let wasm_bytes = wat::parse_str(&wat).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module,
            &mut target,
            &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        let (mut ir_blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);
        optimize_to_fixpoint(&mut ir_blocks, &types, &mut fold_ir_blocks, &mut store_forward_ir_blocks);

        let (movfuscated, _boundary, _accum_info) = movfuscate_ir_with_boundary(&ir_blocks, &mut types);
        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);

        let param_widths: Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        eprintln!("param widths: {param_widths:?}");
        eprintln!("circuit terminator: {:?}", circuit.blocks[0].terminator);

        let to_u64 = |v: &[bool]| -> u64 { v.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum() };
        let mut storage: StorageMap = StorageMap::new();
        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        let mut done = false;
        let mut step = 0usize;
        while !done && step < 10 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            eprintln!("step {step}: done={done} full_state={full_state:?}");
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "minimal repro must halt");
    }

    /// Second-stage isolation repro: adds *conditional*, index-dispatched
    /// register writes (`set_reg`'s own exact shape -- a flat sequence of
    /// independent `if (i32.eq idx K) (then local.set $rK ...)`) on top of
    /// `minimal_state_write_repro`'s already-confirmed-working unconditional
    /// write.
    ///
    /// **Now passes**, after two real bugs were found and fixed:
    /// 1. `lower_to_ir.rs`'s cross-block VAFFLE value resolution (see
    ///    `compute_cross_block_values`) -- VAFFLE, like WAFFLE, uses a flat,
    ///    dominance-based value space, but `lower_function`'s per-block
    ///    `val_map` silently resolved any cross-block reference to
    ///    `IRVarId(0)` instead of the real value.
    /// 2. `movfuscate.rs`'s `scatter_args_to_state` zero-filled any state
    ///    slot a jump target didn't explicitly cover, instead of passing
    ///    the combined block's own current value through -- silently
    ///    wiping loop-carried state (like this program's own `halted` exit
    ///    flag) the moment *any* block along a dispatch chain didn't
    ///    itself thread it forward as an explicit arg.
    /// Run manually: `cargo test -p volar-riscv-e2e --release minimal_dispatch_write_repro -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn minimal_dispatch_write_repro() {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, StorageMap};

        let wat = format!(
            r#"(module
  (func (export "run")
    (local $r1 i32) (local $r2 i32) (local $r3 i32) (local $r4 i32) (local $r5 i32)
    (local $idx i32) (local $val i32)
    (local $steps i32) (local $halted i32)
    (block $exit
      (loop $L
        (br_if $exit (i32.ge_s (local.get $steps) (i32.const 3)))
        (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
        (br_if $exit (local.get $halted))
        (local.set $idx (i32.const 1))
        (local.set $val (i32.const 4))
{set_reg}
        (local.set $halted (i32.const 1))
        (br $L)
      )
    )
  )
)
"#,
            set_reg = set_reg("$idx", "$val"),
        );
        let wasm_bytes = wat::parse_str(&wat).unwrap_or_else(|e| panic!("wat failed to assemble: {e}\n\n{wat}"));
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module,
            &mut target,
            &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        let (mut ir_blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);
        optimize_to_fixpoint(&mut ir_blocks, &types, &mut fold_ir_blocks, &mut store_forward_ir_blocks);

        let (movfuscated, _boundary, _accum_info) = movfuscate_ir_with_boundary(&ir_blocks, &mut types);
        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);

        let param_widths: Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        eprintln!("param widths: {param_widths:?}");

        let to_u64 = |v: &[bool]| -> u64 { v.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum() };
        let mut storage: StorageMap = StorageMap::new();
        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        let mut done = false;
        let mut step = 0usize;
        let mut ever_saw_4: bool = false;
        while !done && step < 30 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            eprintln!("step {step}: done={done} full_state={full_state:?}");
            if full_state.iter().any(|&v| v == 4) {
                ever_saw_4 = true;
            }
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "minimal dispatch repro must halt");
        assert!(ever_saw_4, "the dispatched register write (r1=4) must become visible in some state slot");
    }

    /// Same as `minimal_dispatch_write_repro`, but skips the pre-movfuscation
    /// optimizer (`fold_ir_blocks`/`store_forward_ir_blocks`) entirely --
    /// A/B tests whether constant-folding a *statically-foldable* dispatch
    /// condition (this repro's `idx`/`val` are both compile-time constants)
    /// is what's discarding the conditional write's effect, vs. a bug in
    /// movfuscation/circuit-lowering themselves.
    /// Run manually: `cargo test -p volar-riscv-e2e --release minimal_dispatch_write_repro_no_optimize -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn minimal_dispatch_write_repro_no_optimize() {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, StorageMap};

        let wat = format!(
            r#"(module
  (func (export "run")
    (local $r1 i32) (local $r2 i32) (local $r3 i32) (local $r4 i32) (local $r5 i32)
    (local $idx i32) (local $val i32)
    (local $steps i32) (local $halted i32)
    (block $exit
      (loop $L
        (br_if $exit (i32.ge_s (local.get $steps) (i32.const 3)))
        (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
        (br_if $exit (local.get $halted))
        (local.set $idx (i32.const 1))
        (local.set $val (i32.const 4))
{set_reg}
        (local.set $halted (i32.const 1))
        (br $L)
      )
    )
  )
)
"#,
            set_reg = set_reg("$idx", "$val"),
        );
        let wasm_bytes = wat::parse_str(&wat).unwrap_or_else(|e| panic!("wat failed to assemble: {e}\n\n{wat}"));
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module,
            &mut target,
            &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        let (ir_blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);
        eprintln!("block count (no optimize): {}", ir_blocks.blocks.len());

        let (movfuscated, _boundary, _accum_info) = movfuscate_ir_with_boundary(&ir_blocks, &mut types);
        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);

        let param_widths: Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        eprintln!("param widths: {param_widths:?}");

        let to_u64 = |v: &[bool]| -> u64 { v.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum() };
        let mut storage: StorageMap = StorageMap::new();
        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        let mut done = false;
        let mut step = 0usize;
        let mut ever_saw_4: bool = false;
        while !done && step < 30 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            eprintln!("step {step}: done={done} full_state={full_state:?}");
            if full_state.iter().any(|&v| v == 4) {
                ever_saw_4 = true;
            }
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "minimal dispatch repro (no optimize) must halt");
        assert!(ever_saw_4, "the dispatched register write (r1=4) must become visible in some state slot");
    }

    /// Smallest possible repro: exactly one `if (cond) (then local.set $r1
    /// val))`, no loop. Dumps the *raw VAFFLE* module (before any Volar-IR
    /// lowering at all) directly, to inspect the if/else diamond's own
    /// block/value structure by hand.
    /// Run manually: `cargo test -p volar-riscv-e2e --release dump_smallest_dispatch_vaffle -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn dump_smallest_dispatch_vaffle() {
        let wat = r#"(module
  (func (export "run") (result i32)
    (local $r1 i32) (local $idx i32) (local $val i32)
    (local.set $idx (i32.const 1))
    (local.set $val (i32.const 4))
    (if (i32.eq (local.get $idx) (i32.const 1))
      (then (local.set $r1 (local.get $val))))
    (local.get $r1)
  )
)
"#;
        let wasm_bytes = wat::parse_str(wat).unwrap_or_else(|e| panic!("wat failed to assemble: {e}\n\n{wat}"));
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        for (fid, func) in module.funcs.entries() {
            eprintln!("=== waffle func {fid:?}: {func:#?}");
        }

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module,
            &mut target,
            &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        for (fi, func) in target.module.funcs.iter().enumerate() {
            eprintln!("=== func {fi}: {func:#?}", );
        }

        let (ir_blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);
        for (i, b) in ir_blocks.blocks.iter().enumerate() {
            eprintln!("block {i}: params={:?} term={:?}", b.params, b.terminator);
            for (j, s) in b.stmts.iter().enumerate() {
                eprintln!("  stmt {j}: {:?}", s.kind);
            }
        }

        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, StorageMap};
        let (movfuscated, _boundary, _accum_info) = movfuscate_ir_with_boundary(&ir_blocks, &mut types);
        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
        let param_widths: Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        eprintln!("param widths: {param_widths:?}");
        let to_u64 = |v: &[bool]| -> u64 { v.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum() };
        let mut storage: StorageMap = StorageMap::new();
        let inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
        eprintln!("done={:?}", outputs[0]);
        let full_state: Vec<u64> = (1..outputs.len()).map(|i| to_u64(&outputs[i])).collect();
        eprintln!("full_state={full_state:?}");
    }
}
