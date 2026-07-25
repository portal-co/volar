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
            // Dead-code elimination after the baseline passes: folding/
            // store-forwarding can expose newly-dead statements (and DCE
            // shrinking a block can occasionally expose further folding),
            // so it's part of the same fixpoint, not a one-shot final step.
            let c = volar_ir_opt::ir::dce_ir_blocks(blocks, types);
            if !a && !b && !c {
                break;
            }
        }
    }

    /// Like [`optimize_to_fixpoint`], but for the post-movfuscation
    /// single-block case: also tracks and returns the CUMULATIVE var-id
    /// remap induced by DCE across every fixpoint iteration (identity if
    /// DCE never fires), so a caller holding `MovfuscBlockBoundary`/
    /// `MovfuscAccumInfo` (computed against the *pre*-optimization block)
    /// can translate it via `volar_ir_passes::remap_movfusc_boundaries`/
    /// `remap_movfusc_accum_info` to stay valid post-optimization.
    /// `fold_ir_blocks`/`store_forward_ir_blocks` contribute no remap --
    /// see `dce_ir_blocks_with_remap`'s own doc comment for why neither
    /// ever changes a statement's own index.
    fn optimize_to_fixpoint_with_remap<P: Clone>(
        blocks: &mut volar_ir::ir::IRBlocks<P>,
        types: &volar_ir::ir::IRTypes,
        pass_a: &mut dyn FnMut(&mut volar_ir::ir::IRBlocks<P>, &volar_ir::ir::IRTypes) -> bool,
        pass_b: &mut dyn FnMut(&mut volar_ir::ir::IRBlocks<P>, &volar_ir::ir::IRTypes) -> bool,
    ) -> std::collections::BTreeMap<u32, u32> {
        assert_eq!(
            blocks.blocks.len(), 1,
            "optimize_to_fixpoint_with_remap: only meaningful for a single \
             (post-movfuscation) block -- MovfuscBlockBoundary/MovfuscAccumInfo \
             var ids are only well-defined against exactly one block",
        );
        let n0 = (blocks.blocks[0].params.len() + blocks.blocks[0].stmts.len()) as u32;
        let mut cumulative: std::collections::BTreeMap<u32, u32> = (0..n0).map(|v| (v, v)).collect();
        loop {
            let a = pass_a(blocks, types);
            let b = pass_b(blocks, types);
            let (c, mut remaps) = volar_ir_opt::ir::dce_ir_blocks_with_remap(blocks, types);
            if c {
                let step_remap = remaps.remove(0);
                cumulative = cumulative.into_iter()
                    .filter_map(|(old, mid)| step_remap.get(&mid).map(|&new| (old, new)))
                    .collect();
            }
            if !a && !b && !c {
                break;
            }
        }
        cumulative
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

        // WITH post-movfuscation optimization. `optimize_to_fixpoint_with_remap`
        // (unlike the plain `optimize_to_fixpoint` used for the WITHOUT case
        // above) tracks DCE's own cumulative var-id remap across the whole
        // fixpoint and hands it back -- `remap_movfusc_boundaries`/
        // `remap_movfusc_accum_info` then translate the PRE-optimization
        // boundary/accum-info to stay valid, instead of reusing them stale
        // (fold_ir_blocks/store_forward_ir_blocks need no remap contribution;
        // only DCE renumbers -- see `dce_ir_blocks_with_remap`'s doc comment).
        let mut types_with_opt = types.clone();
        let (mut movfuscated_with_opt, boundary_with_opt, accum_info_with_opt) = movfuscate_ir_with_boundary(&ir_blocks, &mut types_with_opt);
        let remap = optimize_to_fixpoint_with_remap(&mut movfuscated_with_opt, &types_with_opt, &mut fold_ir_blocks, &mut store_forward_ir_blocks);
        let boundary_with_opt = volar_ir_passes::remap_movfusc_boundaries(&boundary_with_opt, &remap);
        let accum_info_with_opt = volar_ir_passes::remap_movfusc_accum_info(&accum_info_with_opt, &remap);
        let and_count_with_opt = and_count_via_real_weaver(&mut types_with_opt, &movfuscated_with_opt, &boundary_with_opt, &accum_info_with_opt);

        eprintln!("and_count WITHOUT post-movfuscation optimization: {and_count_no_opt}");
        eprintln!("and_count WITH post-movfuscation optimization (boundary remapped through DCE): {and_count_with_opt}");
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
        // Milestone 1.5 Step B's combiner-splitting. This test only
        // measures weave time/param counts, not compile -- `chunk_size=8`
        // is fine for that. For anything that actually *compiles* the
        // woven output, use `chunk_size=1` instead (see
        // `largest_chunk_function_compiles`'s own doc comment: `8`'s
        // largest chunk is 44.6MB of source and OOMs rustc).
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
    /// With all five fixed, the circuit shows **genuine, healthy
    /// progress** -- real values flowing and incrementing correctly (a
    /// step-shaped counter, an address-shaped value stepping by 4 each
    /// time, matching a real word-array traversal).
    ///
    /// **CORRECTED (was: "does not halt within 1200 raw steps" / "genuine
    /// never-breaking period-140 cycle").** Both of those were wrong. At
    /// a 2600-raw-step budget, the circuit genuinely halts (`done=true`)
    /// at **step 1405** -- ~4.7x the ~300-step estimate ("~30 raw calls
    /// per real instruction * 27 instructions") this doc comment used to
    /// cite, and far past the 1200-step budget the earlier "never halts"
    /// conclusion was drawn from; that conclusion mistook "hasn't halted
    /// by an under-provisioned budget" for "will never halt" (the same
    /// class of mistake `minimal_dispatch_feedback_loop_repro`'s own
    /// step-budget false positive made, see memory). **But the halted
    /// result is wrong**: `data RAM result word: 0 (expected 65)`. This
    /// is now a real, reproducible, well-characterized correctness bug,
    /// not a halting bug: the real program's loop only needs 4
    /// iterations (`N_WORDS=4`, see `interp.rs`), but the address-shaped
    /// state slot climbed to ~160 (40 iterations' worth of `ptr += 4`)
    /// before the circuit finally halted -- strongly suggesting the
    /// `BEQ`/loop-exit comparison (`i == bound`, bound is a fixed
    /// constant 4 set once via `ADDI`) either computes wrong, or reads a
    /// corrupted `bound` value, letting the loop run ~10x too many
    /// iterations before something (not the real exit condition)
    /// eventually satisfies `done`. Leading hypothesis, not yet
    /// confirmed: `movfuscate.rs`'s `(position, type-signature)`-keyed
    /// slot dedup (Fix B) guarantees type agreement for state slots
    /// shared across different original blocks, but not liveness/
    /// identity stability -- if `bound`'s slot is shared with some other
    /// i32 local that a *different* original block writes on a
    /// *different* dispatch pass, `bound` could be transiently or
    /// permanently clobbered. Per-slot identification (dump which state
    /// slot maps to which WAT local -- e.g. temporarily give each local
    /// in a smaller test program a distinctive constant value) is the
    /// natural next step, not a repro shape change.
    ///
    /// `eval_ir_circuit_step` is slow at this circuit's scale (~0.8s/
    /// step, since movfuscation runs every original block's own logic
    /// every single call) -- the 1405-step halting run took ~19 minutes.
    /// A real, expensive, but bounded cost of this debugging path.
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
        // Confirmed halts (done=true) at step 1405 -- far past the naive
        // "~30 raw calls/instruction * 27 instructions ~= 800-900"
        // estimate (see this test's own doc comment for why: the real
        // loop runs ~10x more iterations than its own 4-iteration bound
        // implies, a real correctness bug, not a halting one). ~0.8s/step
        // at this circuit's scale; budget real time to run this.
        const RAW_STEP_BUDGET: usize = 1700;
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

    /// Milestone 1's own real checkpoint: drive the *real* RISC-V
    /// interpreter (not `mem_probe.rs`'s small stand-in) through the real
    /// split weave -> `QSim` -> real Gf128-based multi-storage memory
    /// boundary -> IOP finalization proof, for a small, fixed number of
    /// real raw steps -- proving the whole honest pipeline is wired
    /// correctly at real interpreter circuit scale, using
    /// `chunk_size=1` (`docs/interpreter-honest-e2e-zk-plan.md`'s own
    /// confirmed-compilable choice) and `crate::memory_check_driver`'s
    /// generic per-`(storage_id, type_id)` accounting (mem_probe.rs's own
    /// 2-storage, single-address `mem2`/`mem33` hand-threading doesn't
    /// generalize -- the real interpreter's circuit touches dozens of
    /// storages across many addresses, including
    /// `StorageId::VAFFLE_SSA_SPILL`'s own cross-block spill slots).
    ///
    /// **Deliberately does *not* run the guest program to completion**
    /// (the real 27-instruction sum-4-words program needs on the order of
    /// 1000+ raw circuit steps, per `trace_interpreter_plain_values_matches_native_reference`'s
    /// own ~1405-hop finding on an earlier repro) -- at `chunk_size=1`'s
    /// 241 functions per role, a driver unrolling that many real steps
    /// worth of straight-line calling code would need a fundamentally
    /// different design (e.g. a real runtime loop over witness data
    /// instead of one generated Rust statement block per step) to stay
    /// compile-tractable, out of scope here. `RAW_STEPS` below is small
    /// on purpose: enough to exercise a real write-then-read/write
    /// sequence (so the multiset check's own `old_value`/`write_ts`
    /// threading is genuinely tested, not just a trivial single-touch
    /// case), not a claim that the guest program finishes.
    ///
    /// `#[ignore]`d: real interpreter scale. Run manually:
    /// `cargo test -p volar-riscv-e2e --release honest_interpreter_run_folds_and_finalizes_with_real_memory_boundary -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn honest_interpreter_run_folds_and_finalizes_with_real_memory_boundary() {
        use volar_ir_passes::LoweringMode;
        use volar_ir_common::TypeId;
        use volar_weaver::{
            weave_vole_prover_ir_split, weave_vole_qsim_ir_split,
            weave_vole_verifier_ir_split_with_trace, print_weaved_vole_module, IopSink,
            StorageMode,
        };
        use volar_compiler::ir::IrFunction;
        use volar_verifier_iop_runtime::run_iop_verifier;
        use volar_fuzz::interpreter::ir::{
            eval_ir_circuit_step_with_watch, apply_pre_init, bits_to_u64, bit_width, StorageMap,
        };
        use crate::split_driver::{generate_split_step, Slot};
        use crate::memory_check_driver::MemCheckAccounting;

        const RAW_STEPS: usize = 2;

        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, boundary, accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);
        let mode = StorageMode::Commitment;
        let chunk_size = 1usize; // confirmed-compilable -- docs/interpreter-honest-e2e-zk-plan.md
        let n_blocks = boundary.len();
        let n_chunks = n_blocks.div_ceil(chunk_size);

        let mut prover_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        let trace = weave_vole_prover_ir_split(&circuit, &types, "riscv", &mode, &boundary, &accum_info, chunk_size, |f| prover_funcs.push(f));
        let mut qsim_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_qsim_ir_split(&circuit, &types, "riscv", &mode, &boundary, &accum_info, chunk_size, |f| qsim_funcs.push(f));
        let mut verifier_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_verifier_ir_split_with_trace(&circuit, &types, "riscv", &mode, &IopSink, &boundary, &accum_info, chunk_size, |f| verifier_funcs.push(f));
        assert_eq!(prover_funcs.len(), n_blocks + n_chunks + 1);
        assert_eq!(qsim_funcs.len(), n_blocks + n_chunks + 1);
        assert_eq!(verifier_funcs.len(), n_blocks + n_chunks + 1);
        eprintln!("woven: {} functions per role ({n_blocks} blocks + {n_chunks} chunks + 1 finish)", prover_funcs.len());

        let module_of = |functions: std::vec::Vec<IrFunction>, name: &str| volar_compiler::ir::IrModule {
            name: name.into(), functions, structs: vec![], enums: vec![], traits: vec![], impls: vec![], type_aliases: vec![], consts: vec![],
        };
        let prover_code = print_weaved_vole_module(&module_of(prover_funcs.clone(), "prover"));
        let qsim_code = print_weaved_vole_module(&module_of(qsim_funcs.clone(), "qsim"));
        let verifier_code = print_weaved_vole_module(&module_of(verifier_funcs.clone(), "verifier"));
        let prover_fn_only = &prover_code[prover_code.find("pub fn").expect("prover source must have a pub fn")..];
        let qsim_fn_only = &qsim_code[qsim_code.find("pub fn").expect("qsim source must have a pub fn")..];
        let rust_source = format!("{verifier_code}\n{prover_fn_only}\n{qsim_fn_only}");
        eprintln!("printed source length: {} bytes", rust_source.len());

        // Ordered watch list: every real StorageRead/StorageWrite's own
        // addr_var + value_var, in the same statement order `trace`
        // itself lists them (verified by construction -- both come from
        // the same statement-order walk, per `split_driver.rs`'s own
        // doc). One watch pass per step recovers every real value
        // `generate_split_step`'s own `oracle_bits` and
        // `MemCheckAccounting` both need, without hand-deriving them.
        let watch_vars: std::vec::Vec<u32> = trace.entries.iter().flat_map(|e| [e.addr_var, e.value_var]).collect();

        let mut storage: StorageMap = StorageMap::new();
        apply_pre_init(&mut storage, &circuit.pre_init, &types);
        let mut pre_init_map: std::collections::BTreeMap<(u32, u32, u64), u64> = std::collections::BTreeMap::new();
        for seg in &circuit.pre_init {
            for i in 0..seg.data.len() {
                pre_init_map.insert((seg.storage.0, seg.ty.0, (seg.offset + i) as u64), seg.as_u64(i));
            }
        }

        let param_widths: std::vec::Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| bit_width(tid, &types)).collect();
        let mut inputs: std::vec::Vec<std::vec::Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();

        let mut zero_stmts = String::new();
        for (i, &w) in param_widths.iter().enumerate() {
            if w <= 1 {
                zero_stmts += &format!("let w{i}_vope_0 = vope_zero();\nlet w{i}_q_0 = q_zero();\n");
            } else {
                zero_stmts += &format!("let w{i}_vope_0: [Vope<N, Galois, cipher::consts::U1>; {w}] = core::array::from_fn(|_| vope_zero());\n");
                zero_stmts += &format!("let w{i}_q_0: [Q<N, Galois>; {w}] = core::array::from_fn(|_| q_zero());\n");
            }
        }
        let mut entry_w: std::vec::Vec<(Slot, Slot)> = param_widths.iter().enumerate().map(|(i, &w)| {
            if w <= 1 {
                (Slot::Scalar(format!("w{i}_vope_0")), Slot::Scalar(format!("w{i}_q_0")))
            } else {
                (Slot::Array(format!("w{i}_vope_0"), w), Slot::Array(format!("w{i}_q_0"), w))
            }
        }).collect();

        let mut all_steps_stmts = String::new();
        let mut all_ok_fold_state: Option<(String, String)> = None;
        let mut mem_check = MemCheckAccounting::new();

        for step in 0..RAW_STEPS {
            let (outputs, watched) = eval_ir_circuit_step_with_watch(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage, &watch_vars);
            let watched_map: std::collections::BTreeMap<u32, std::vec::Vec<bool>> = watched.into_iter().collect();

            let mut oracle_bits: std::vec::Vec<std::vec::Vec<bool>> = std::vec::Vec::new();
            for e in &trace.entries {
                let addr_bits = watched_map.get(&e.addr_var).unwrap_or_else(|| panic!("step {step}: addr_var {} not watched (dead statement?)", e.addr_var));
                let value_bits = watched_map.get(&e.value_var).unwrap_or_else(|| panic!("step {step}: value_var {} not watched (dead statement?)", e.value_var));
                let addr = bits_to_u64(addr_bits);
                let width = bit_width(TypeId(e.type_id), &types);
                let value = bits_to_u64(&value_bits[..width.min(64)]);
                mem_check.emit_op(&mut all_steps_stmts, e.storage_id, e.type_id, addr, value, e.is_write, &pre_init_map);
                if !e.is_write {
                    oracle_bits.push(value_bits.clone());
                }
            }

            let result = generate_split_step(
                &prover_funcs, &qsim_funcs, &verifier_funcs, &boundary, &accum_info, n_chunks,
                &entry_w, all_ok_fold_state.clone(), &oracle_bits, step,
            );
            all_steps_stmts += &result.stmts;
            entry_w = result.next_entry_w;
            all_ok_fold_state = Some((result.final_all_ok_expr, result.final_fold_state_expr));

            inputs = outputs[1..1 + param_widths.len()].to_vec();
        }
        assert!(!mem_check.is_empty(), "the real interpreter must touch at least one real committed storage");
        let (h_produce_expr, h_consume_expr) = mem_check.finish(&mut all_steps_stmts);
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
            fn honest_interpreter_run_verifies_with_real_memory_boundary() {{
                let mut rng = TestRng(0xC0FFEE_C0FFEE);
                let delta = random_nonzero_delta::<N, Galois, _>(&mut rng, sample_g, is_zero_g);
                let cot = IdealCot::new(delta.clone());

                let r = Gf128::from_u64(0x53);
                let key = ChallengeKey::from_challenge(r);

                {zero_stmts}

                {all_steps_stmts}

                assert_eq!({RAW_STEPS}u8, {RAW_STEPS}u8);

                let mem_acc_in = {h_produce_expr};
                let mem_acc_out = {h_consume_expr};

                let tagged: volar_discipline::Tagged<volar_discipline::Transparent, _> =
                    volar_discipline::Tagged::seal({final_fold_state});
                let _ = {final_all_ok};
                let (proof, ok) = prove_and_verify_iop(tagged, &mem_acc_in, &mem_acc_out, Some((&mem_acc_in, &mem_acc_out)));
                assert!(ok, "the finalization proof over the real interpreter's own memory boundary must verify");

                let mut corrupted_out = mem_acc_out;
                corrupted_out[0] = Gf128::from_u64(0xdead_beef);
                let corrupted_ok = volar_iop::verify_iop(&proof, Some((&mem_acc_in, &corrupted_out)));
                assert!(!corrupted_ok, "a corrupted expected memory boundary must be rejected");
            }}
        "#);

        run_iop_verifier(&rust_source, &driver);
    }

    /// Cheap sanity check for `bound_register_survives_across_dispatch`'s
    /// own hand-assembled program -- decodes each word back and asserts it
    /// matches the intended instruction, so an encoding mistake (e.g. a
    /// wrong branch/jump offset) can't masquerade as an interpreter bug.
    #[test]
    fn bound_register_program_round_trips_through_decode() {
        use rv_asm::{Imm, Inst, Reg, Xlen};
        const X1: Reg = Reg::RA;
        const X5: Reg = Reg::T0;
        let e = |inst: Inst| inst.encode_normal(Xlen::Rv32);
        let program: Vec<(u32, Inst)> = vec![
            (e(Inst::Addi { imm: Imm::new_i32(4), dest: X5, src1: Reg::ZERO }), Inst::Addi { imm: Imm::new_i32(4), dest: X5, src1: Reg::ZERO }),
            (e(Inst::Addi { imm: Imm::new_i32(0), dest: X1, src1: Reg::ZERO }), Inst::Addi { imm: Imm::new_i32(0), dest: X1, src1: Reg::ZERO }),
            (e(Inst::Beq { offset: Imm::new_i32(12), src1: X1, src2: X5 }), Inst::Beq { offset: Imm::new_i32(12), src1: X1, src2: X5 }),
            (e(Inst::Addi { imm: Imm::new_i32(1), dest: X1, src1: X1 }), Inst::Addi { imm: Imm::new_i32(1), dest: X1, src1: X1 }),
            (e(Inst::Jal { offset: Imm::new_i32(-8), dest: Reg::ZERO }), Inst::Jal { offset: Imm::new_i32(-8), dest: Reg::ZERO }),
            (e(Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: X1, base: Reg::ZERO }), Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: X1, base: Reg::ZERO }),
        ];
        for (i, (word, intended)) in program.iter().enumerate() {
            let (decoded, is_compressed) = Inst::decode(*word, Xlen::Rv32).expect("word should decode");
            assert_eq!(is_compressed, rv_asm::IsCompressed::No);
            assert_eq!(&decoded, intended, "instruction {i} (pc={}) decoded wrong -- encoding bug in the repro itself", i * 4);
        }
    }

    /// Isolates the "loop bound register written once, compared many raw
    /// steps later" hypothesis (see
    /// `trace_interpreter_plain_values_matches_native_reference`'s own doc
    /// comment / memory) against the **real opcode-dispatch machinery**
    /// (`interpreter_wat`'s decode/opcode-`if`-chain), not a hand-rolled
    /// stand-in like `minimal_dispatch_feedback_loop_repro` -- that repro
    /// skips opcode fetch/decode entirely, so it can't exercise whatever
    /// interaction between the decode blocks and the register-dispatch
    /// blocks might be corrupting a register's physical slot.
    ///
    /// Program: `x5 = 4` (bound, written once), `x1 = 0` (loop counter),
    /// then a genuine 4-iteration `BEQ`-guarded loop (`ADDI`/`BEQ`/`JAL`,
    /// all real opcodes dispatched through the real interpreter), storing
    /// the final loop counter to RAM. If `x5` survives correctly, the
    /// loop runs exactly 4 times and the stored result is `4`. If it's
    /// getting clobbered (this session's leading hypothesis for the real
    /// interpreter's wrong-answer bug), the loop will run some other
    /// number of times (or never satisfy `BEQ`, hitting `MAX_STEPS`).
    ///
    /// Sanity-checks the *encoding itself* of
    /// `bound_register_survives_across_dispatch`'s program, independent of
    /// any lowering/movfuscation machinery -- rules out a unit/shift bug in
    /// how `rv_asm::Imm` offsets are constructed (raised as a live
    /// hypothesis this session: RV32 B/J-type immediates are packed with
    /// the low bit implicit, so a wrong assumption about byte-offset vs.
    /// pre-shifted units would silently double every branch/jump target).
    /// Decodes each assembled word back via `Inst::decode` (rv_asm's own,
    /// independent of `interpreter_wat`'s hand-rolled bit-shuffle) and
    /// checks every field, including the actual numeric offset, against
    /// what was intended.
    #[test]
    fn bound_register_program_decodes_correctly() {
        use rv_asm::{Imm, Inst, Reg, Xlen};
        const X1: Reg = Reg::RA;
        const X5: Reg = Reg::T0;
        let e = |inst: Inst| inst.encode_normal(Xlen::Rv32);
        let program: Vec<u32> = vec![
            e(Inst::Addi { imm: Imm::new_i32(4), dest: X5, src1: Reg::ZERO }),
            e(Inst::Addi { imm: Imm::new_i32(0), dest: X1, src1: Reg::ZERO }),
            e(Inst::Beq { offset: Imm::new_i32(12), src1: X1, src2: X5 }),
            e(Inst::Addi { imm: Imm::new_i32(1), dest: X1, src1: X1 }),
            e(Inst::Jal { offset: Imm::new_i32(-8), dest: Reg::ZERO }),
            e(Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: X1, base: Reg::ZERO }),
        ];
        // Independently verify the *program's own logic* (not just its
        // encoding) via `interp::native_reference` -- rules out a hand-trace
        // mistake in this repro's own instruction sequence before blaming
        // the movfuscation/circuit pipeline. `native_reference`'s return
        // value is hardcoded to `R_SUM` (x3, unused here); what matters is
        // the side effect it leaves in `mem` via this program's own `SW`.
        let mut mem = vec![0u8; (RESULT_ADDR as usize) + 4];
        let _ = crate::interp::native_reference(&program, &mut mem);
        let stored = i32::from_le_bytes(mem[RESULT_ADDR as usize..RESULT_ADDR as usize + 4].try_into().unwrap());
        assert_eq!(stored, 4, "this repro's own program logic (independent of any circuit/movfuscation pipeline) must store 4");

        for (i, &word) in program.iter().enumerate() {
            let (inst, is_compressed) = Inst::decode(word, Xlen::Rv32).expect("every assembled word must decode");
            assert_eq!(is_compressed, rv_asm::IsCompressed::No);
            eprintln!("word {i} = {word:#010x}");
            match (i, inst) {
                (0, Inst::Addi { imm, dest, src1 }) => {
                    assert_eq!(imm.as_i32(), 4); assert_eq!(dest, X5); assert_eq!(src1, Reg::ZERO);
                }
                (1, Inst::Addi { imm, dest, src1 }) => {
                    assert_eq!(imm.as_i32(), 0); assert_eq!(dest, X1); assert_eq!(src1, Reg::ZERO);
                }
                (2, Inst::Beq { offset, src1, src2 }) => {
                    assert_eq!(offset.as_i32(), 12, "BEQ offset must decode as +12 bytes (pc=8 -> pc=20)");
                    assert_eq!(src1, X1); assert_eq!(src2, X5);
                }
                (3, Inst::Addi { imm, dest, src1 }) => {
                    assert_eq!(imm.as_i32(), 1); assert_eq!(dest, X1); assert_eq!(src1, X1);
                }
                (4, Inst::Jal { offset, dest }) => {
                    assert_eq!(offset.as_i32(), -8, "JAL offset must decode as -8 bytes (pc=16 -> pc=8)");
                    assert_eq!(dest, Reg::ZERO);
                }
                (5, Inst::Sw { offset, src, base }) => {
                    assert_eq!(offset.as_i32(), RESULT_ADDR); assert_eq!(src, X1); assert_eq!(base, Reg::ZERO);
                }
                (i, other) => panic!("word {i}: unexpected decode shape (not necessarily wrong variant, but check manually): opcode-discriminant mismatch, got a variant that isn't the {i}-th expected one; inst={other:?}"),
            }
        }
    }

    /// Regression test for the 7th architectural bug this milestone: a
    /// `TypeId` mismatch between how WASM active data-segment pre-init
    /// content gets typed (`waffle_lower.rs`'s pre_init construction) and
    /// how every runtime `StorageRead`/`StorageWrite` types the same
    /// memory (`mem_load_bytes`/`mem_store_bytes`'s `byte_tid()`, i.e.
    /// `Vec(8, Bit)`). Before the fix, pre_init used a *structurally
    /// different* `Primitive(_8)` TypeId (via `lir_type_to_tid(U8)`), so
    /// any read of pre-initialized memory that was never subsequently
    /// overwritten by a runtime store -- e.g. the program's own code
    /// bytes -- permanently missed its own correctly-populated entry in
    /// the interpreter's `(StorageId, TypeId, addr)`-keyed storage map,
    /// silently reading back as zero. This made every opcode dispatch
    /// check false, so the loop always ran to the `$steps >= MAX_STEPS`
    /// safety net (~1400 raw steps) instead of a real `$halted`-triggered
    /// exit.
    ///
    /// Program: `x1 = 77; mem[RESULT_ADDR] = x1` (2 real RISC-V
    /// instructions). Verified via the plain pre-movfuscation CFG
    /// interpreter (`eval_ir_with_trace`) -- cheap (raw block hops, not
    /// movfuscated-circuit steps), and sufficient to catch this class of
    /// bug without needing the (much slower) real circuit simulation.
    #[test]
    fn halted_flag_triggers_loop_exit_immediately() {
        use rv_asm::{Imm, Inst, Reg, Xlen};

        const X1: Reg = Reg::RA;
        let e = |inst: Inst| inst.encode_normal(Xlen::Rv32);
        let program: Vec<u32> = vec![
            e(Inst::Addi { imm: Imm::new_i32(77), dest: X1, src1: Reg::ZERO }), // pc=0: x1=77
            e(Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: X1, base: Reg::ZERO }), // pc=4: mem[RESULT_ADDR]=77; halt
        ];
        let code_bytes: Vec<u8> = program.iter().flat_map(|w| w.to_le_bytes()).collect();
        let data_bytes = vec![0u8; (RESULT_ADDR as usize) + 4];

        let wasm_bytes = wat::parse_str(&interpreter_wat(&code_bytes, &data_bytes)).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module, &mut target, &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        // `lower_vaffle_to_ir` runs the `vaffle_ssa` max-SSA pass
        // internally, so cross-block values are already explicit block
        // params/jump-args by the time this returns -- no separate call
        // needed here.
        let (ir_blocks, types) = volar_vaffle_target::lower_to_ir::lower_vaffle_to_ir(&target.module);

        let entry_widths: Vec<usize> = ir_blocks.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        let entry_inputs: Vec<Vec<bool>> = entry_widths.iter().map(|&w| vec![false; w]).collect();
        let (ret, storage, visited, _watched) =
            volar_fuzz::interpreter::ir::eval_ir_with_trace(&ir_blocks, &types, &entry_inputs, &[]);

        assert!(ret.is_some(), "interpreter should return, not hit MAX_ITERS");
        assert!(
            visited.len() < 200,
            "halted after {} raw block hops -- expected a real $halted-triggered exit \
             (~80-100 hops for this 2-instruction program), not the ~1400-hop \
             $steps >= MAX_STEPS safety net",
            visited.len(),
        );

        let (result_sid, result_ty) = ir_blocks.pre_init.iter()
            .find(|seg| seg.data.len() as i32 == RESULT_ADDR + 4)
            .map(|seg| (seg.storage, seg.ty))
            .expect("result-holding pre_init segment must exist");
        let bytes: Vec<u8> = (0..4).map(|i| {
            let addr = (RESULT_ADDR + i) as u64;
            match storage.get(&(result_sid, result_ty, addr)) {
                Some(b) => b.iter().enumerate().map(|(j, &bit)| (bit as u8) << j).fold(0u8, |a, b| a | b),
                None => 0,
            }
        }).collect();
        let stored = i32::from_le_bytes(bytes.try_into().unwrap());
        assert_eq!(stored, 77, "RESULT_ADDR should hold 77 after the SW instruction runs");
    }

    /// Does `BEQ` ever take its "equal" branch at all, for two registers
    /// set to the *same* value moments earlier (no loop, no far-apart
    /// spill/reload)? Straight-line: `x1=5; x5=5; beq x1,x5,+8 (skip);
    /// sw 0xBAD (should NOT execute); skip: sw 0xGOOD`. If the stored
    /// result is 0xBAD, BEQ's "equal" branch never fires even in the
    /// simplest possible case -- a genuine comparison/branch-selection
    /// bug, independent of loops or spill/reload liveness.
    #[test]
    fn beq_equal_branch_fires_straight_line() {
        use rv_asm::{Imm, Inst, Reg, Xlen};
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, apply_pre_init, StorageMap};

        const X1: Reg = Reg::RA;
        const X5: Reg = Reg::T0;
        let e = |inst: Inst| inst.encode_normal(Xlen::Rv32);
        let program: Vec<u32> = vec![
            e(Inst::Addi { imm: Imm::new_i32(5), dest: X1, src1: Reg::ZERO }), // pc=0: x1=5
            e(Inst::Addi { imm: Imm::new_i32(5), dest: X5, src1: Reg::ZERO }), // pc=4: x5=5
            e(Inst::Beq { offset: Imm::new_i32(12), src1: X1, src2: X5 }),     // pc=8: if x1==x5 goto pc=20 (should fire, they're equal)
            e(Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: X1, base: Reg::ZERO }), // pc=12: BAD path -- should be skipped
            e(Inst::Jal { offset: Imm::new_i32(0), dest: Reg::ZERO }),         // pc=16: (padding, unreachable if BEQ works)
            e(Inst::Addi { imm: Imm::new_i32(9), dest: X1, src1: Reg::ZERO }), // pc=20: GOOD path -- x1=9
            e(Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: X1, base: Reg::ZERO }), // pc=24: store 9
        ];
        let code_bytes: Vec<u8> = program.iter().flat_map(|w| w.to_le_bytes()).collect();
        let data_bytes = vec![0u8; (RESULT_ADDR as usize) + 4];

        let mut mem = data_bytes.clone();
        let native = crate::interp::native_reference(&program, &mut mem);
        let native_stored = i32::from_le_bytes(mem[RESULT_ADDR as usize..RESULT_ADDR as usize + 4].try_into().unwrap());
        eprintln!("native: returned x3={native} stored={native_stored}");
        assert_eq!(native_stored, 9, "this repro's own program logic must store 9 via the GOOD path");

        let wasm_bytes = wat::parse_str(&interpreter_wat(&code_bytes, &data_bytes)).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module, &mut target, &volar_vaffle_target::import_config::WaffleImportConfig::default(),
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
        let mut storage: StorageMap = StorageMap::new();
        apply_pre_init(&mut storage, &circuit.pre_init, &types);

        let to_u64 = |v: &[bool]| -> u64 { v.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum() };
        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        let mut done = false;
        let mut step = 0usize;
        while !done && step < 1700 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            if step % 20 == 0 || done { eprintln!("step {step}: done={done} full_state={full_state:?}"); }
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "circuit must halt within budget");

        let mut found_word = None;
        for seg in &circuit.pre_init {
            if seg.data.len() as i32 == RESULT_ADDR + 4 {
                let bytes: Vec<u8> = (0..4).map(|i| {
                    let addr = (RESULT_ADDR + i) as u64;
                    let bits = &storage[&(seg.storage, seg.ty, addr)];
                    bits.iter().enumerate().map(|(j, &b)| (b as u8) << j).fold(0u8, |a, b| a | b)
                }).collect();
                found_word = Some(i32::from_le_bytes(bytes.try_into().unwrap()));
            }
        }
        eprintln!("circuit stored result: {found_word:?} (expect Some(9) if BEQ's equal branch fired; Some(5) if it didn't)");
        assert_eq!(found_word, Some(9), "BEQ must take its equal branch when comparing two equal registers");
    }

    /// Isolates whether `JAL`'s *negative*-offset backward branch works
    /// correctly through the real interpreter machinery -- the one
    /// negative offset in `bound_register_survives_across_dispatch`'s own
    /// program (`BEQ`'s own offset there is positive). A 3-instruction
    /// infinite loop with no exit: `i=0; loop: i+=1; jal loop`. If JAL's
    /// backward offset resolves correctly, `i`'s slot should climb
    /// cleanly by 1 every ~2 real instructions worth of raw steps,
    /// forever. If the offset's sign-extension is broken (e.g. `shr_s`
    /// silently behaving as unsigned), the computed jump target would be
    /// a huge wrong address, landing in zero-padded memory (opcode 0,
    /// matches nothing, `next_pc` defaults to `pc+4` every step) -- `i`
    /// would freeze at whatever value it last reached and never move
    /// again, while `pc` marches forward forever until `MAX_STEPS`.
    #[test]
    fn jal_negative_offset_backward_branch() {
        use rv_asm::{Imm, Inst, Reg, Xlen};
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, apply_pre_init, StorageMap};

        const X1: Reg = Reg::RA;
        let e = |inst: Inst| inst.encode_normal(Xlen::Rv32);
        let program: Vec<u32> = vec![
            e(Inst::Addi { imm: Imm::new_i32(0), dest: X1, src1: Reg::ZERO }), // pc=0: i=0
            e(Inst::Addi { imm: Imm::new_i32(1), dest: X1, src1: X1 }),        // pc=4: i+=1 (loop target)
            e(Inst::Jal { offset: Imm::new_i32(-4), dest: Reg::ZERO }),        // pc=8: goto pc=4
        ];
        let code_bytes: Vec<u8> = program.iter().flat_map(|w| w.to_le_bytes()).collect();
        let data_bytes = vec![0u8; (RESULT_ADDR as usize) + 4];

        let wasm_bytes = wat::parse_str(&interpreter_wat(&code_bytes, &data_bytes)).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module, &mut target, &volar_vaffle_target::import_config::WaffleImportConfig::default(),
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
        let mut storage: StorageMap = StorageMap::new();
        apply_pre_init(&mut storage, &circuit.pre_init, &types);

        let to_u64 = |v: &[bool]| -> u64 { v.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum() };
        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        // ~30 raw steps/instruction * 3 instructions/iteration -> budget
        // for several loop iterations' worth of raw steps.
        for step in 0..500 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            if step % 10 == 0 { eprintln!("step {step}: full_state={full_state:?}"); }
            inputs = outputs[1..1 + param_widths.len()].to_vec();
        }
    }

    /// Fast diagnostic sibling of `bound_register_survives_across_dispatch`
    /// (below): dumps the new edge-aware slot layout for the same 6
    /// -instruction program WITHOUT running the (slow, ~1s/raw-step) full
    /// circuit evaluation loop -- lets us directly inspect whether x1
    /// (loop counter) and x5 (bound) end up on separate physical slots,
    /// in seconds rather than minutes.
    #[test]
    fn dump_bound_register_slot_layout() {
        use rv_asm::{Imm, Inst, Reg, Xlen};
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};

        const X1: Reg = Reg::RA;
        const X5: Reg = Reg::T0;
        let e = |inst: Inst| inst.encode_normal(Xlen::Rv32);
        let program: Vec<u32> = vec![
            e(Inst::Addi { imm: Imm::new_i32(4), dest: X5, src1: Reg::ZERO }),
            e(Inst::Addi { imm: Imm::new_i32(0), dest: X1, src1: Reg::ZERO }),
            e(Inst::Beq { offset: Imm::new_i32(12), src1: X1, src2: X5 }),
            e(Inst::Addi { imm: Imm::new_i32(1), dest: X1, src1: X1 }),
            e(Inst::Jal { offset: Imm::new_i32(-8), dest: Reg::ZERO }),
            e(Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: X1, base: Reg::ZERO }),
        ];
        let code_bytes: Vec<u8> = program.iter().flat_map(|w| w.to_le_bytes()).collect();
        let data_bytes = vec![0u8; (RESULT_ADDR as usize) + 4];

        let wasm_bytes = wat::parse_str(&interpreter_wat(&code_bytes, &data_bytes)).expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module, &mut target, &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        eprintln!("VAFFLE module: {} funcs", target.module.funcs.len());
        for (fi, fd) in target.module.funcs.iter().enumerate() {
            match fd {
                vaffle::FuncDecl::Body(body) => {
                    let n_calls: usize = body.blocks.iter().flat_map(|b| b.stmts.iter())
                        .filter(|&&vid| matches!(&body.values[vid.0].kind, vaffle::Value::Call { .. })).count();
                    eprintln!("  func {fi}: {} blocks, {n_calls} Value::Call stmts", body.blocks.len());
                }
                _ => eprintln!("  func {fi}: non-Body"),
            }
        }

        let (mut ir_blocks, types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);
        optimize_to_fixpoint(&mut ir_blocks, &types, &mut fold_ir_blocks, &mut store_forward_ir_blocks);

        eprintln!("{}", volar_ir_passes::movfuscate::debug_dump_slot_of(&ir_blocks, &types));
    }

    /// `#[ignore]`d: still real-interpreter-scale codegen (though a much
    /// shorter run than the full 4-word-sum program). Run manually:
    /// `cargo test -p volar-riscv-e2e --release bound_register_survives_across_dispatch -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn bound_register_survives_across_dispatch() {
        use rv_asm::{Imm, Inst, Reg, Xlen};
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, apply_pre_init, StorageMap};

        const X1: Reg = Reg::RA; // loop counter
        const X5: Reg = Reg::T0; // bound, written once

        let e = |inst: Inst| inst.encode_normal(Xlen::Rv32);
        let program: Vec<u32> = vec![
            e(Inst::Addi { imm: Imm::new_i32(4), dest: X5, src1: Reg::ZERO }),   // pc=0:  x5 = 4
            e(Inst::Addi { imm: Imm::new_i32(0), dest: X1, src1: Reg::ZERO }),   // pc=4:  x1 = 0
            e(Inst::Beq { offset: Imm::new_i32(12), src1: X1, src2: X5 }),       // pc=8:  if x1==x5 goto pc=20
            e(Inst::Addi { imm: Imm::new_i32(1), dest: X1, src1: X1 }),          // pc=12: x1 += 1
            e(Inst::Jal { offset: Imm::new_i32(-8), dest: Reg::ZERO }),          // pc=16: goto pc=8
            e(Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: X1, base: Reg::ZERO }), // pc=20: mem[16] = x1; halt
        ];
        let code_bytes: Vec<u8> = program.iter().flat_map(|w| w.to_le_bytes()).collect();
        let data_bytes = vec![0u8; (RESULT_ADDR as usize) + 4];

        let wasm_bytes = wat::parse_str(&interpreter_wat(&code_bytes, &data_bytes))
            .expect("wat should assemble");
        let module = crate::parse_and_expand(&wasm_bytes).expect("wasm should parse+expand");

        let mut target = volar_vaffle_target::VaffleTarget::new();
        let errors = volar_vaffle_target::waffle_lower::lower_waffle_module(
            &module, &mut target, &volar_vaffle_target::import_config::WaffleImportConfig::default(),
        );
        assert!(errors.is_empty(), "unexpected lowering errors: {errors:?}");

        let (mut ir_blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&target.module);
        let pre_opt_stmts: usize = ir_blocks.blocks.iter().map(|b| b.stmts.len()).sum();
        // SKIP_OPT env var lets this repro be run with optimize_to_fixpoint
        // disabled entirely, to A/B whether fold_ir_blocks/
        // store_forward_ir_blocks (not movfuscation) are mixing/zeroing
        // values -- run manually: `SKIP_OPT=1 cargo test ... --ignored --nocapture`.
        let skip_opt = std::env::var("SKIP_OPT").is_ok();
        if !skip_opt {
            optimize_to_fixpoint(&mut ir_blocks, &types, &mut fold_ir_blocks, &mut store_forward_ir_blocks);
        }
        let post_opt_stmts: usize = ir_blocks.blocks.iter().map(|b| b.stmts.len()).sum();
        eprintln!("skip_opt={skip_opt} pre_opt_stmts={pre_opt_stmts} post_opt_stmts={post_opt_stmts}");

        let (movfuscated, _boundary, _accum_info) = movfuscate_ir_with_boundary(&ir_blocks, &mut types);
        let bit_ty = types.intern(IRType::Primitive(Type::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);

        let param_widths: Vec<usize> = circuit.blocks[0].params.iter()
            .map(|&tid| volar_fuzz::interpreter::ir::bit_width(tid, &types))
            .collect();
        eprintln!("param widths: {param_widths:?}");
        let pc_width = volar_ir_passes::movfuscate::pc_bits_needed(120);
        eprintln!("pc_width={pc_width} (first {pc_width} params are the active-block-index bits)");

        let mut storage: StorageMap = StorageMap::new();
        apply_pre_init(&mut storage, &circuit.pre_init, &types);

        // `full_state` only reflects circuit.blocks[0]'s own params (the
        // movfuscated state vector). A cross-block VAFFLE value spilled by
        // `compute_cross_block_values` (lower_to_ir.rs) lives in the
        // persistent `storage` map instead, under `StorageId::STACK` (=1),
        // completely invisible to `full_state`. Dump every nonzero STACK
        // entry at checkpoints to see whether x5's spilled bits (set once,
        // early) survive unchanged, or get clobbered.
        // movfuscation remaps every StorageId(n) to two lanes (see
        // movfuscate.rs: "Non-Block StorageWrite remaps to storage ID
        // 2n+1 (odd)", Block-typed to 2n even) -- StorageId::STACK (=1)
        // becomes lanes 2 and 3 in the *movfuscated* circuit, not literal
        // id 1. Dump every distinct nonzero storage id's entry count so
        // this isn't blind to the remap.
        let dump_stack = |storage: &StorageMap, label: &str| {
            let mut by_sid: std::collections::BTreeMap<u32, Vec<(u64, u64)>> = std::collections::BTreeMap::new();
            for ((sid, _ty, addr), bits) in storage.iter() {
                if bits.iter().any(|&b| b) {
                    let val = bits.iter().enumerate().map(|(i, &b)| (b as u64) << i).fold(0u64, |a, b| a | b);
                    by_sid.entry(sid.0).or_default().push((*addr, val));
                }
            }
            eprintln!("{label}: nonzero entries by storage id: {:?}", by_sid.iter().map(|(k, v)| (k, v.len())).collect::<Vec<_>>());
            for (sid, entries) in &by_sid {
                if *sid != 35 { // storage 35 is the data RAM (already tracked separately below)
                    eprintln!("    sid={sid}: {entries:?}");
                }
            }
        };

        let to_u64 = |v: &[bool]| -> u64 { v.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum() };
        let mut inputs: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
        let mut done = false;
        let mut step = 0usize;
        while !done && step < 2000 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            // outputs[1..1+pc_width] are the pc_width individual-Bit params
            // encoding "which of the 120 original VAFFLE blocks is active
            // next" -- decode LSB-first into a block index, independent of
            // knowing which physical state slot holds any given WASM local.
            let active_block: u64 = (0..pc_width).map(|b| (outputs[1 + b][0] as u64) << b).sum();
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            if step % 100 == 0 || done {
                eprintln!("step {step}: done={done} active_block={active_block} full_state={full_state:?}");
                dump_stack(&storage, &format!("  after step {step}"));
            }
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "circuit must halt within budget via its own termination flag");

        let expected: i32 = 4;
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
        assert!(found, "loop counter must read back as exactly 4 -- if not, x5 (bound) was corrupted mid-loop");
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
    /// *largest* generated split-verifier function (a chunk combiner)
    /// actually print+compile, on its own, before investing in the full
    /// interleaved driver?
    ///
    /// **History**: `q_and`/`hat`/`r_and` are already array-batched in the
    /// split-weave path (`crates/compiler/volar-weaver/src/vole.rs`), so the
    /// classic rustc 65535-argument limit is not the live blocker here (a
    /// stale doc note on an earlier version of this test claimed otherwise
    /// -- confirmed wrong by direct measurement: the largest `chunk_size=8`
    /// function has only 5,512 params). The real blocker is generated
    /// *source size*: at `chunk_size=8`, the largest accumulator chunk
    /// (`accum_chunk_0`) is 44.6MB of printed Rust source, and `rustc`
    /// SIGKILL's (OOM) after ~28 minutes trying to compile it. Root cause:
    /// `movfuscate.rs`'s cross-block accumulation loop emits one AND-gate +
    /// one XOR-add per `(block, slot)` pair unconditionally, regardless of
    /// whether that block actually touches that slot -- see
    /// `docs/interpreter-honest-e2e-zk-plan.md` for the deferred, safer
    /// fix (a real one exists, but requires split-weave changes too; a
    /// first attempt was implemented, found unsafe, and reverted).
    ///
    /// **Working mitigation, confirmed by direct measurement**: printed
    /// source size scales ~linearly with `chunk_size` (`chunk_size=8`:
    /// 46.8MB; `4`: 24.4MB; `2`: 13.3MB; `1`: 7.7MB --
    /// `probe_chunk_size_vs_largest_function_size`), and `chunk_size=1`
    /// compiles successfully in ~9 minutes (536.59s), vs. `chunk_size=8`'s
    /// OOM after ~28 minutes. `chunk_size=1` means one accumulator-chunk
    /// function per original block (241 functions total for this circuit,
    /// vs. 136 at `chunk_size=8`) -- more functions, but each individually
    /// tractable. Use `chunk_size=1` for the real interpreter until the
    /// movfuscation-level fix lands.
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
        // chunk_size=1: the confirmed-fast choice (47s, 3.15MB) once the
        // tunnelled-slot elimination fix landed (movfuscate.rs +
        // vole.rs's insert_w_wires fix). chunk_size=8 also compiles now
        // (down from OOM to 24.1MB source), but takes 30+ minutes -- not
        // worth it over chunk_size=1's 47s for no clear benefit. See
        // docs/interpreter-honest-e2e-zk-plan.md for the full comparison.
        let chunk_size = 1usize;

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

    /// Cheap (weave + print only, no compile) probe: does a smaller
    /// `chunk_size` meaningfully shrink the largest chunk function's own
    /// printed source size? `largest_chunk_function_compiles` found
    /// `chunk_size=8`'s largest chunk to be 44.6MB of source and OOM `rustc`
    /// after ~28 minutes -- this measures (in seconds, not tens of
    /// minutes) whether a smaller `chunk_size` is a viable mitigation on
    /// its own, before investing in movfuscation-level codegen changes.
    /// Run manually: `cargo test -p volar-riscv-e2e --release probe_chunk_size_vs_largest_function_size -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn probe_chunk_size_vs_largest_function_size() {
        use volar_ir_passes::LoweringMode;
        use volar_weaver::{StorageMode, weave_vole_verifier_ir_split_with_trace, print_weaved_vole_module, IopSink};

        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, boundary, accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);
        let mode = StorageMode::Commitment;

        for &chunk_size in &[1usize, 2, 4, 8] {
            let mut biggest: Option<volar_compiler::ir::IrFunction> = None;
            let mut n_funcs = 0usize;
            weave_vole_verifier_ir_split_with_trace(
                &circuit, &types, "riscv_step", &mode, &IopSink, &boundary, &accum_info, chunk_size,
                |f| {
                    n_funcs += 1;
                    if biggest.as_ref().map(|b| b.params.len()).unwrap_or(0) < f.params.len() {
                        biggest = Some(f);
                    }
                },
            );
            let f = biggest.expect("at least one function woven");
            let module = volar_compiler::ir::IrModule {
                name: "riscv_step".into(), functions: vec![f.clone()], structs: vec![], enums: vec![],
                traits: vec![], impls: vec![], type_aliases: vec![], consts: vec![],
            };
            let code = print_weaved_vole_module(&module);
            eprintln!(
                "chunk_size={chunk_size}: {n_funcs} functions, largest={} params={} printed_len={} bytes",
                f.name, f.params.len(), code.len(),
            );
        }
    }

    /// `probe_chunk_size_vs_largest_function_size` only measures a
    /// single, largest chunk function in isolation -- it never printed
    /// *every* one of the ~241 split-weave functions for a role together,
    /// which is what a real driven step actually needs
    /// (`print_weaved_vole_module` on the full module, once per role).
    /// That full-module print is what drove RSS past 13GB+ (system swap
    /// nearly exhausted) before `WireRepr::Array` replaced
    /// `insert_w_wires`'s eager per-bit unpacking -- this measures the
    /// full-module print's own size/time directly, at `chunk_size=1`, to
    /// confirm the fix actually holds at the scale that mattered.
    /// Run manually: `cargo test -p volar-riscv-e2e --release probe_full_module_print_size -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn probe_full_module_print_size() {
        use volar_ir_passes::LoweringMode;
        use volar_weaver::{StorageMode, weave_vole_prover_ir_split, print_weaved_vole_module};

        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, boundary, accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);
        let mode = StorageMode::Commitment;
        let chunk_size = 1usize;

        eprintln!("num_params (circuit.blocks[0].params.len()) = {}", circuit.blocks[0].params.len());
        {
            use volar_ir_common::Stmt;
            use volar_fuzz::interpreter::ir::bit_width;
            let mut n_shuffle = 0usize;
            let mut shuffle_bits = 0usize;
            let mut n_merge = 0usize;
            let mut merge_parts = 0usize;
            let mut n_poly = 0usize;
            let mut n_stmts = 0usize;
            let mut poly_deg0 = 0usize; // no monomials at all (pure constant)
            let mut poly_deg1_single = 0usize; // exactly one degree-1 monomial, no others (pass-through / NOT-ish)
            let mut poly_deg1_multi = 0usize; // 2+ degree-1 monomials, no degree>=2 (XOR chain)
            let mut poly_deg2plus = 0usize; // has at least one degree>=2 monomial (AND-bearing)
            let mut poly_deg2plus_monomial_total = 0usize;
            let mut poly_width_gt1 = 0usize;
            let mut poly_by_ty: std::collections::BTreeMap<u32, usize> = std::collections::BTreeMap::new();
            for node in &circuit.blocks[0].stmts {
                n_stmts += 1;
                match &node.kind {
                    Stmt::Shuffle { result_bits, .. } => { n_shuffle += 1; shuffle_bits += result_bits.len(); }
                    Stmt::Merge { parts, .. } => { n_merge += 1; merge_parts += parts.len(); }
                    Stmt::Poly { ty, coeffs, .. } => {
                        n_poly += 1;
                        *poly_by_ty.entry(ty.0).or_insert(0) += 1;
                        let w = bit_width(*ty, &types);
                        if w > 1 { poly_width_gt1 += 1; }
                        let max_deg = coeffs.keys().map(|m| m.len()).max().unwrap_or(0);
                        let deg1_count = coeffs.keys().filter(|m| m.len() == 1).count();
                        let deg2plus_count = coeffs.keys().filter(|m| m.len() >= 2).count();
                        if max_deg == 0 { poly_deg0 += 1; }
                        else if max_deg == 1 && deg1_count == 1 { poly_deg1_single += 1; }
                        else if max_deg == 1 { poly_deg1_multi += 1; }
                        else { poly_deg2plus += 1; poly_deg2plus_monomial_total += deg2plus_count; }
                    }
                    _ => {}
                }
            }
            eprintln!("total stmts={n_stmts} poly={n_poly} shuffle={n_shuffle} (total result_bits={shuffle_bits}) merge={n_merge} (total parts={merge_parts})");
            eprintln!("poly breakdown: deg0(const)={poly_deg0} deg1_single={poly_deg1_single} deg1_multi(xor-chain)={poly_deg1_multi} deg2plus(and-bearing)={poly_deg2plus} (total and-monomials={poly_deg2plus_monomial_total}) width>1={poly_width_gt1}");
            eprintln!("poly by output type id (top 10): {:?}", poly_by_ty.iter().collect::<Vec<_>>().into_iter().rev().take(10).collect::<Vec<_>>());
        }
        let mut funcs: Vec<volar_compiler::ir::IrFunction> = Vec::new();
        let _trace = weave_vole_prover_ir_split(&circuit, &types, "riscv_step", &mode, &boundary, &accum_info, chunk_size, |f| funcs.push(f));
        eprintln!("woven: {} functions", funcs.len());
        let avg_params = funcs.iter().map(|f| f.params.len()).sum::<usize>() as f64 / funcs.len() as f64;
        eprintln!("avg params per function = {avg_params:.1}");
        let module = volar_compiler::ir::IrModule {
            name: "riscv_step".into(), functions: funcs, structs: vec![], enums: vec![],
            traits: vec![], impls: vec![], type_aliases: vec![], consts: vec![],
        };
        let code = print_weaved_vole_module(&module);
        eprintln!("full prover module printed_len={} bytes", code.len());
    }

    /// First bounded feasibility check for the C-backend path: does the
    /// REAL split-woven module (not the toy `BIrBlocks` circuits
    /// `volar-c-backend/tests/vole_e2e.rs` already validates end-to-end)
    /// even lower through the SAME `LinkageSystem` + `lower_module_with_opts`
    /// + `CBackend` pipeline, at real interpreter scale, for a single block
    /// function only (not all 241 -- that's the next step once this
    /// succeeds).
    ///
    /// `vole_e2e.rs` proves the pipeline itself is real and correct (a
    /// full OT-based VOLE AND-gate check, 4/4 pass) for the OLDER
    /// `BIrBlocks`/`weave_vole_prover` path. The real interpreter goes
    /// through `weave_vole_prover_ir_split` (`IRBlocks`/`CirBlock`-based,
    /// chunk_size=1) instead, which currently has no `linkage` parameter
    /// -- but `LinkageSystem::apply` is public and only mutates
    /// `structs`/`enums`/`traits`/`impls`/`functions`/`type_aliases`, so
    /// it can be applied externally without touching `vole.rs` at all.
    /// `mem_probe.rs`'s own field config (`N=16`, `T=Galois`, `U1=1`,
    /// `K=1`) matches `vole_e2e.rs`'s already-validated `galois_vole_env`
    /// exactly, so no new `MonoEnv` config should be needed either.
    #[test]
    #[ignore]
    fn probe_split_weave_single_block_lowers_to_c() {
        use volar_ir_passes::LoweringMode;
        use volar_weaver::{StorageMode, weave_vole_prover_ir_split};
        use volar_compiler::{
            SourceInput, ir::IrType, ir::PrimitiveType,
            linkage::{LinkageKind, LinkageSystem, LinkedSpec},
            parse_sources,
        };
        use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};
        use volar_c_backend::CBackend;

        fn spec_src_dir() -> std::path::PathBuf {
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent().unwrap()
                .parent().unwrap()
                .join("spec").join("volar-spec").join("src")
        }
        fn read_spec(name: &str) -> (String, String) {
            let path = spec_src_dir().join(name);
            let src = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("cannot read spec file {}: {e}", path.display()));
            let stem = std::path::Path::new(name).file_stem().unwrap().to_string_lossy().into_owned();
            (src, stem)
        }
        fn make_vole_linkage() -> LinkageSystem {
            let files = ["lib.rs", "vole.rs", "vole/prove.rs", "vole/vope.rs", "vole/impls.rs"];
            let loaded: Vec<(String, String)> = files.iter().map(|&f| read_spec(f)).collect();
            let inputs: Vec<SourceInput> = loaded.iter()
                .map(|(src, name)| SourceInput { source: src.as_str(), name: name.as_str() })
                .collect();
            let spec_module = parse_sources(&inputs, "volar_spec", &[])
                .unwrap_or_else(|e| panic!("make_vole_linkage failed: {e}"));
            let mut ls = LinkageSystem::new();
            ls.add(LinkedSpec { name: "volar_spec".into(), module: spec_module, kind: LinkageKind::Inline });
            ls
        }
        fn galois_vole_env() -> MonoEnv {
            MonoEnv::new("sha256")
                .with_len("N", 16)
                .with_len("U1", 1)
                .with_len("U0", 0)
                .with_len("K", 1)
                .with_type("T", IrType::Primitive(PrimitiveType::Galois))
        }

        let (_ir_blocks, _movfuscated, circuit, types, _bit_ty, boundary, accum_info) =
            lower_interpreter(1, LoweringMode::WithTerminationFlag);
        let mode = StorageMode::Commitment;
        let chunk_size = 1usize;

        let mut funcs: Vec<volar_compiler::ir::IrFunction> = Vec::new();
        let _trace = weave_vole_prover_ir_split(&circuit, &types, "riscv_step", &mode, &boundary, &accum_info, chunk_size, |f| funcs.push(f));
        eprintln!("woven: {} functions", funcs.len());

        let one_func = funcs.into_iter().next().expect("at least one function woven");
        eprintln!("testing single function: {} ({} params)", one_func.name, one_func.params.len());
        let mut module = volar_compiler::ir::IrModule {
            name: "riscv_step_probe".into(), functions: vec![one_func], structs: vec![], enums: vec![],
            traits: vec![], impls: vec![], type_aliases: vec![], consts: vec![],
        };

        let linkage = make_vole_linkage();
        linkage.apply(&mut module);
        eprintln!("after linkage: {} functions, {} structs", module.functions.len(), module.structs.len());
        for s in &module.structs {
            eprintln!("struct {:?}:", s.kind);
            for f in &s.fields {
                eprintln!("  field {} : {:?}", f.name, f.ty);
            }
        }

        let env = galois_vole_env();
        eprintln!("env.const_params = {:?}", env.const_params);
        eprintln!("env.type_params = {:?}", env.type_params);
        let mut b = CBackend::new();
        lower_module_with_opts(&module, &mut b, &env);
        let c_src = b.finish();
        eprintln!("lowered to C successfully: {} bytes", c_src.len());
        assert!(!c_src.is_empty());
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

    /// Isolates the real interpreter's own **two-`br_if`-to-the-same-`$exit`**
    /// loop-exit pattern (`br_if $exit (steps>=max)`, then later
    /// `br_if $exit (halted)`, both targeting the *same* block) -- never
    /// exercised by any earlier repro this session (all had exactly one
    /// exit check). If this alone reproduces "stuck forever, done never
    /// fires", the bug is in how WAFFLE/lower_to_ir.rs/movfuscate_ir
    /// handle multiple distinct branches converging on one target, not
    /// in anything already fixed this session.
    /// Run manually: `cargo test -p volar-riscv-e2e --release minimal_double_exit_repro -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn minimal_double_exit_repro() {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, StorageMap};

        let wat = r#"(module
  (func (export "run") (result i32)
    (local $steps i32) (local $halted i32) (local $r3 i32)
    (block $exit
      (loop $L
        (br_if $exit (i32.ge_s (local.get $steps) (i32.const 5)))
        (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
        (br_if $exit (local.get $halted))
        (local.set $r3 (i32.add (local.get $r3) (i32.const 1)))
        (if (i32.eq (local.get $steps) (i32.const 3))
          (then (local.set $halted (i32.const 1))))
        (br $L)
      )
    )
    (local.get $r3)
  )
)
"#;
        let wasm_bytes = wat::parse_str(wat).unwrap_or_else(|e| panic!("wat failed to assemble: {e}\n\n{wat}"));
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
        while !done && step < 60 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            eprintln!("step {step}: done={done} full_state={full_state:?}");
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "minimal double-exit repro must halt (expected via the halted-check: steps reaches 3, sets halted=1, exits with r3=3 on the next pass)");
    }

    /// Combines the two patterns already individually confirmed correct
    /// (`minimal_dispatch_write_repro`'s flat `get_reg`/`set_reg` dispatch
    /// chain, `minimal_double_exit_repro`'s two-`br_if`-to-`$exit` loop) --
    /// but, unlike either alone, the loop's own exit condition is decided
    /// by a value that made a full **read (dispatch) -> compute -> write
    /// (dispatch) -> next-iteration read (dispatch)** round trip, exactly
    /// like the real interpreter's `ADDI`/register-loop pattern (`get_rs1v`
    /// each iteration, dispatch-write `$result` to `$rd`, then next
    /// iteration's `get_rs1v` must see the new value to eventually satisfy
    /// a data-dependent branch). Neither existing repro exercises a
    /// register value that must survive a real dispatch round trip *and*
    /// feed a loop-exit decision.
    ///
    /// **DOES NOT REPRODUCE THE BUG -- was a step-budget false positive.**
    /// A prior investigation session read a 60-raw-step failure here as
    /// confirmation that a dispatch read-modify-write value fails to
    /// survive the loop's own back-edge. That conclusion was wrong: each
    /// WAT-level `loop $L` iteration costs ~26 raw movfuscated-circuit
    /// steps (the `get_reg`/`set_reg` flat dispatch chains alone are ~10
    /// `if`-diamonds), and `$r3` genuinely does climb by 1 per WAT
    /// iteration via the dispatch round trip (confirmed by hand-decoding
    /// the PC bits and state slots of the 60-step trace: `r3` reads back
    /// as 0, 1, 2 on its first three loop-header revisits, each ~26 steps
    /// apart) -- the repro simply needs ~107 raw steps to actually reach
    /// `r3 == 3` and halt, not 60. Re-run with a 150-step budget: passes
    /// cleanly, `done` fires at step 106 (halted after 107 steps).
    /// **This means the dispatch-write-then-read-across-a-back-edge
    /// pattern is not, by itself, broken** -- it does *not* isolate
    /// whatever is causing the real interpreter's own non-halting
    /// behavior (which was confirmed via a full 1200-step PC decode to be
    /// a genuine, never-breaking period-140 cycle, not merely "needs more
    /// steps"). Whoever resumes that investigation should look elsewhere
    /// for a repro that isolates it (e.g. something that forces the same
    /// physical state slot to be read by *two different* original blocks
    /// depending on which logical loop iteration it is, since `movfuscate.rs`'s
    /// `(position, type-signature)`-keyed slot dedup only guarantees type
    /// agreement, not that a slot's *identity* is stable across a real
    /// multi-iteration back-edge) rather than assuming this repro's shape
    /// already covers it.
    /// Run manually: `cargo test -p volar-riscv-e2e --release minimal_dispatch_feedback_loop_repro -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn minimal_dispatch_feedback_loop_repro() {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, StorageMap};

        let wat = format!(
            r#"(module
  (func (export "run") (result i32)
    (local $r1 i32) (local $r2 i32) (local $r3 i32) (local $r4 i32) (local $r5 i32)
    (local $idx i32) (local $val i32) (local $cur i32)
    (local $steps i32) (local $halted i32)
    (block $exit
      (loop $L
        (br_if $exit (i32.ge_s (local.get $steps) (i32.const 10)))
        (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
        (br_if $exit (local.get $halted))

        ;; read r3 via the flat dispatch chain (like real `get_rs1v`).
        (local.set $idx (i32.const 3))
{get_cur}
        ;; compute + write back via the flat dispatch chain (like real
        ;; `ADDI`'s `set_result_to_rd`).
        (local.set $val (i32.add (local.get $cur) (i32.const 1)))
{set_val}
        ;; loop-exit decision depends on the value *read back* through
        ;; dispatch, not a plain local -- the untested combination.
        (if (i32.eq (local.get $cur) (i32.const 3))
          (then (local.set $halted (i32.const 1))))

        (br $L)
      )
    )
    (local.get $r3)
  )
)
"#,
            get_cur = get_reg("$idx", "$cur"),
            set_val = set_reg("$idx", "$val"),
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
        // ~26 raw steps/WAT-loop-iteration * 4 iterations to reach r3==3, + margin.
        while !done && step < 150 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            eprintln!("step {step}: done={done} full_state={full_state:?}");
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "dispatch-feedback-loop repro must halt (r3 climbs 1 per iteration via dispatch round trip, halts when it reads back as 3)");
    }

    /// Same as `minimal_dispatch_feedback_loop_repro`, but with the
    /// two-`br_if`-to-`$exit` structure collapsed to a **single** `br_if`
    /// (the safety-net check becomes a plain `if` that also just sets
    /// `$halted`, like the data-dependent check already does) -- isolates
    /// whether "double exit" is actually the interacting ingredient, or
    /// whether the dispatch read/compute/write round trip alone is enough
    /// to break the loop regardless of how many `br_if $exit`s there are.
    ///
    /// **Result: a different, earlier failure** -- this shape doesn't even
    /// get past `lower_waffle_module` (`UnsupportedOp("undefined v364")`),
    /// so it can't test the hypothesis it was built for. This is a real,
    /// separate, previously-unknown WAFFLE-frontend lowering gap in its own
    /// right (a loop with one `br_if $exit` plus two later plain `if`s that
    /// both assign the same local, one of them the loop's own safety net --
    /// not yet investigated further), but is NOT evidence about whether
    /// double-exit specifically matters for the read-modify-write bug --
    /// that comparison remains unresolved.
    /// Run manually: `cargo test -p volar-riscv-e2e --release minimal_dispatch_feedback_single_exit_repro -- --ignored --nocapture`.
    #[test]
    #[ignore]
    fn minimal_dispatch_feedback_single_exit_repro() {
        use volar_ir::ir::IRType;
        use volar_ir_common::Type;
        use volar_ir_opt::{ir::fold_ir_blocks, store_forward::store_forward_ir_blocks};
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};
        use volar_fuzz::interpreter::ir::{eval_ir_circuit_step, StorageMap};

        let wat = format!(
            r#"(module
  (func (export "run") (result i32)
    (local $r1 i32) (local $r2 i32) (local $r3 i32) (local $r4 i32) (local $r5 i32)
    (local $idx i32) (local $val i32) (local $cur i32)
    (local $steps i32) (local $halted i32)
    (block $exit
      (loop $L
        (br_if $exit (local.get $halted))
        (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
        (if (i32.ge_s (local.get $steps) (i32.const 10))
          (then (local.set $halted (i32.const 1))))

        (local.set $idx (i32.const 3))
{get_cur}
        (local.set $val (i32.add (local.get $cur) (i32.const 1)))
{set_val}
        (if (i32.eq (local.get $cur) (i32.const 3))
          (then (local.set $halted (i32.const 1))))

        (br $L)
      )
    )
    (local.get $r3)
  )
)
"#,
            get_cur = get_reg("$idx", "$cur"),
            set_val = set_reg("$idx", "$val"),
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
        while !done && step < 60 {
            let outputs = eval_ir_circuit_step(&circuit.blocks[0], &types, &circuit.oracles, &inputs, &mut storage);
            done = outputs[0].iter().any(|&b| b);
            let full_state: Vec<u64> = (1..1 + param_widths.len()).map(|i| to_u64(&outputs[i])).collect();
            eprintln!("step {step}: done={done} full_state={full_state:?}");
            inputs = outputs[1..1 + param_widths.len()].to_vec();
            step += 1;
        }
        eprintln!("halted after {step} steps (done={done})");
        assert!(done, "single-exit dispatch-feedback-loop repro must halt");
    }
}
