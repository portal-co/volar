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
}
