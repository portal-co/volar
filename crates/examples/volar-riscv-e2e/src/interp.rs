//! Milestone 1's minimal RISC-V test program, plus its two independent
//! implementations: a native Rust reference (the trusted oracle) and a
//! hand-authored WAT module (the thing that actually gets lowered through
//! `lower_waffle_module`).
//!
//! The program: sum four `i32` words out of data RAM into a register, then
//! store the sum back to RAM. Minimal RV32I subset: `ADDI`, `ADD`, `LW`,
//! `SW`, `BEQ`, `JAL` -- enough for a real loop with real dynamic memory
//! addressing, nothing more (see the plan's Milestone 1 scope).

use rv_asm::{Imm, Inst, Reg, Xlen};

/// x1 = loop counter, x2 = data pointer, x3 = sum accumulator,
/// x4 = load scratch, x5 = loop bound.
const R_I: Reg = Reg::RA; // x1
const R_PTR: Reg = Reg::SP; // x2
const R_SUM: Reg = Reg::GP; // x3
const R_TMP: Reg = Reg::TP; // x4
const R_BOUND: Reg = Reg::T0; // x5

/// Number of words the test program sums.
pub const N_WORDS: usize = 4;
/// Byte address in data memory where the result is stored.
pub const RESULT_ADDR: i32 = 16;
/// Safety bound on interpreter steps (real program takes 27; generous margin).
pub const MAX_STEPS: i32 = 40;

/// Assemble the fixed test program as raw RV32I instruction words.
///
/// ```text
/// pc=0:  addi x5, x0, 4        ; bound = 4
/// pc=4:  beq  x1, x5, 24       ; loop: if i == bound, goto end (pc=28)
/// pc=8:  lw   x4, 0(x2)        ; tmp = mem[ptr]
/// pc=12: add  x3, x3, x4       ; sum += tmp
/// pc=16: addi x2, x2, 4        ; ptr += 4
/// pc=20: addi x1, x1, 1        ; i += 1
/// pc=24: jal  x0, -20          ; goto loop (pc=4)
/// pc=28: sw   x3, 16(x0)       ; end: mem[16] = sum ; halt
/// ```
pub fn assemble_program() -> Vec<u32> {
    let e = |inst: Inst| inst.encode_normal(Xlen::Rv32);
    vec![
        e(Inst::Addi { imm: Imm::new_i32(N_WORDS as i32), dest: R_BOUND, src1: Reg::ZERO }),
        e(Inst::Beq { offset: Imm::new_i32(24), src1: R_I, src2: R_BOUND }),
        e(Inst::Lw { offset: Imm::new_i32(0), dest: R_TMP, base: R_PTR }),
        e(Inst::Add { dest: R_SUM, src1: R_SUM, src2: R_TMP }),
        e(Inst::Addi { imm: Imm::new_i32(4), dest: R_PTR, src1: R_PTR }),
        e(Inst::Addi { imm: Imm::new_i32(1), dest: R_I, src1: R_I }),
        e(Inst::Jal { offset: Imm::new_i32(-20), dest: Reg::ZERO }),
        e(Inst::Sw { offset: Imm::new_i32(RESULT_ADDR), src: R_SUM, base: Reg::ZERO }),
    ]
}

/// The fixed initial contents of data RAM (four words to sum).
pub fn initial_data_words() -> [i32; N_WORDS] {
    [10, 20, 30, 5]
}

/// Little-endian byte image of the initial data RAM (at least
/// `RESULT_ADDR + 4` bytes, zero-padded).
pub fn initial_data_bytes() -> Vec<u8> {
    let mut bytes = vec![0u8; (RESULT_ADDR as usize) + 4];
    for (i, word) in initial_data_words().iter().enumerate() {
        bytes[i * 4..i * 4 + 4].copy_from_slice(&word.to_le_bytes());
    }
    bytes
}

/// Little-endian byte image of the program (code memory).
pub fn program_bytes() -> Vec<u8> {
    assemble_program().iter().flat_map(|w| w.to_le_bytes()).collect()
}

/// Native Rust reference interpreter -- the trusted oracle. Decodes with
/// the exact same `rv_asm::Inst::decode` the real toolchain uses, executes
/// against a byte-addressable memory, and halts on the `SW` (this program's
/// only store, used here as the designated final instruction).
///
/// Returns the final value of `x3` (the sum). `mem` is mutated in place,
/// so the caller can also inspect the stored result at `RESULT_ADDR`.
pub fn native_reference(program: &[u32], mem: &mut [u8]) -> i32 {
    let mut regs = [0i32; 32];
    let mut pc: i32 = 0;
    let mut steps = 0;
    loop {
        steps += 1;
        assert!(steps <= MAX_STEPS, "native reference exceeded MAX_STEPS");

        let word = program[(pc / 4) as usize];
        let (inst, _) = Inst::decode(word, Xlen::Rv32).expect("valid instruction");
        let mut next_pc = pc + 4;
        let mut halt = false;

        match inst {
            Inst::Addi { imm, dest, src1 } => {
                let v = regs[src1.0 as usize].wrapping_add(imm.as_i32());
                if dest.0 != 0 {
                    regs[dest.0 as usize] = v;
                }
            }
            Inst::Add { dest, src1, src2 } => {
                let v = regs[src1.0 as usize].wrapping_add(regs[src2.0 as usize]);
                if dest.0 != 0 {
                    regs[dest.0 as usize] = v;
                }
            }
            Inst::Lw { offset, dest, base } => {
                let addr = regs[base.0 as usize].wrapping_add(offset.as_i32()) as usize;
                let bytes: [u8; 4] = mem[addr..addr + 4].try_into().unwrap();
                let v = i32::from_le_bytes(bytes);
                if dest.0 != 0 {
                    regs[dest.0 as usize] = v;
                }
            }
            Inst::Sw { offset, src, base } => {
                let addr = regs[base.0 as usize].wrapping_add(offset.as_i32()) as usize;
                mem[addr..addr + 4].copy_from_slice(&regs[src.0 as usize].to_le_bytes());
                halt = true;
            }
            Inst::Beq { offset, src1, src2 } => {
                if regs[src1.0 as usize] == regs[src2.0 as usize] {
                    next_pc = pc + offset.as_i32();
                }
            }
            Inst::Jal { offset, dest } => {
                if dest.0 != 0 {
                    regs[dest.0 as usize] = pc + 4;
                }
                next_pc = pc + offset.as_i32();
            }
            other => panic!("native_reference: unsupported instruction {other:?}"),
        }

        if halt {
            return regs[R_SUM.0 as usize];
        }
        pc = next_pc;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn program_round_trips_through_decode() {
        for word in assemble_program() {
            let (_inst, is_compressed) =
                Inst::decode(word, Xlen::Rv32).expect("every assembled word must decode");
            assert_eq!(is_compressed, rv_asm::IsCompressed::No);
        }
    }

    #[test]
    fn native_reference_sums_the_words_and_stores_result() {
        let program = assemble_program();
        let mut mem = initial_data_bytes();
        let sum = native_reference(&program, &mut mem);

        let expected: i32 = initial_data_words().iter().sum();
        assert_eq!(sum, expected, "native reference must compute the correct sum");

        let stored = i32::from_le_bytes(
            mem[RESULT_ADDR as usize..RESULT_ADDR as usize + 4].try_into().unwrap(),
        );
        assert_eq!(stored, expected, "native reference must store the sum to RAM");
    }
}
