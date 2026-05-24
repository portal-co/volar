// @reliability: normal
// @ai: assisted
//! Shared test corpus for LIR backend integration tests.
//!
//! # Layers
//!
//! 1. **LirTarget programs** (`generated.rs`): generic `build_*<B: LirTarget>(b: &mut B)`
//!    functions + `ALL_CASES` static + `for_each_build!` macro.
//!
//! 2. **IR circuits** (this file): `make_*_biir()` and `make_*_ir()` helpers that
//!    build `BIrBlocks` / `(IRBlocks, IRTypes)` for circuit-lowering e2e tests.
//!
//! 3. **C harness** (this file): `compile_and_run` and `c_main_body_for` shared
//!    across all C-backend test files.

pub mod generated;
pub use generated::ALL_CASES;

use std::{fs, process::Command};
use tempfile::TempDir;

// ============================================================================
// Corpus metadata types
// ============================================================================

/// One set of inputs + expected output for a corpus function.
pub struct CorpusIo {
    /// Raw integer inputs (cast as needed by the C caller).
    pub inputs: &'static [u64],
    /// Expected return value (packed as u64 for uniformity).
    pub expected: u64,
}

/// Metadata for a single corpus case.
pub struct CorpusCase {
    pub name: &'static str,
    /// I/O pairs for correctness verification.
    pub ios: &'static [CorpusIo],
    /// C argument type strings (e.g. `"uint32_t"`), one per parameter.
    pub c_arg_types: &'static [&'static str],
    /// printf format for the return value (e.g. `"%u"` or `"%llu"`).
    pub c_ret_fmt: &'static str,
    /// C cast expression applied to the call result (e.g. `"(unsigned)"`).
    pub c_ret_cast: &'static str,
    /// Template for the C call expression using identifiers a0, a1, …
    /// (e.g. `"add_u32(a0, a1)"`).
    pub c_call_template: &'static str,
}

impl CorpusCase {
    /// Generate the C `main` body that calls this function with `io`'s inputs
    /// and prints the result.
    pub fn c_main_body(&self, io: &CorpusIo) -> String {
        // Build argument declarations.
        let mut decls = String::new();
        for (i, (&ty, &val)) in self.c_arg_types.iter().zip(io.inputs.iter()).enumerate() {
            decls.push_str(&format!("  {ty} a{i} = {val};\n"));
        }
        format!(
            "{decls}  printf(\"{fmt}\\n\", {cast}{call});",
            fmt = self.c_ret_fmt,
            cast = self.c_ret_cast,
            call = self.c_call_template,
        )
    }
}

// ============================================================================
// C harness
// ============================================================================

/// Compile `c_src` (with an appended `main` whose body is `main_body`) and
/// return the program's stdout as a `String`.
///
/// Requires `cc` in `$PATH`.  Compiles with `-O0 -std=c99`.
pub fn compile_and_run(c_src: &str, main_body: &str) -> String {
    let dir = TempDir::new().expect("tempdir");
    let c_path = dir.path().join("test.c");
    let exe_path = dir.path().join("test");

    let full_src = format!(
        "{c_src}\n#include <stdio.h>\n#include <stdint.h>\n#include <string.h>\n\
         int main(void) {{\n{main_body}\n  return 0;\n}}\n"
    );

    fs::write(&c_path, &full_src).expect("write C source");

    let status = Command::new("cc")
        .args(["-O0", "-std=c99", "-o"])
        .arg(&exe_path)
        .arg(&c_path)
        .status()
        .expect("cc not found — install a C compiler");
    assert!(
        status.success(),
        "C compilation failed.\nSource:\n{full_src}"
    );

    let output = Command::new(&exe_path)
        .output()
        .expect("failed to run compiled program");
    String::from_utf8(output.stdout).expect("non-UTF8 output")
}

// ============================================================================
// BIrBlocks circuit builders (shared between C and LLVM e2e tests)
// ============================================================================

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRTypeId, IRTypes, IRVarId,
};
use volar_ir_common::{Constant, IrType as CommonIrType, Type};

/// 1-bit identity: return the input.
pub fn make_biir_identity() -> BIrBlocks {
    BIrBlocks { blocks: vec![BIrBlock {
        params: 1,
        stmts: vec![],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(0)],
        }),
    }], pre_init: vec![] }
}

/// 1-bit NOT.
pub fn make_biir_not() -> BIrBlocks {
    BIrBlocks { blocks: vec![BIrBlock {
        params: 1,
        stmts: vec![BIrStmt::Not(IRVarId(0))],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(1)],
        }),
    }], pre_init: vec![] }
}

/// 2-bit AND.
pub fn make_biir_and() -> BIrBlocks {
    BIrBlocks { blocks: vec![BIrBlock {
        params: 2,
        stmts: vec![BIrStmt::And(IRVarId(0), IRVarId(1))],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(2)],
        }),
    }], pre_init: vec![] }
}

/// 2-bit XOR.
pub fn make_biir_xor() -> BIrBlocks {
    BIrBlocks { blocks: vec![BIrBlock {
        params: 2,
        stmts: vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(2)],
        }),
    }], pre_init: vec![] }
}

/// Half adder: 2 inputs → (sum=XOR, carry=AND) packed as 2-bit output.
pub fn make_biir_half_adder() -> BIrBlocks {
    BIrBlocks { blocks: vec![BIrBlock {
        params: 2,
        stmts: vec![
            BIrStmt::Xor(IRVarId(0), IRVarId(1)), // var 2 = sum
            BIrStmt::And(IRVarId(0), IRVarId(1)), // var 3 = carry
        ],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(2), IRVarId(3)], // [sum, carry] packed LSB-first
        }),
    }], pre_init: vec![] }
}

/// Two-block NOT: block 0 → block 1 → return NOT(input).
pub fn make_biir_two_block_not() -> BIrBlocks {
    BIrBlocks { blocks: vec![
        BIrBlock {
            params: 1,
            stmts: vec![],
            stmt_provs: vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(0)],
            }),
        },
        BIrBlock {
            params: 1,
            stmts: vec![BIrStmt::Not(IRVarId(0))],
            stmt_provs: vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(1)],
            }),
        },
    ], pre_init: vec![] }
}

/// Self-loop: if input=1 return input, else loop with constant 1.
/// Output is always 1 (terminates in ≤1 iteration).
pub fn make_biir_self_loop() -> BIrBlocks {
    BIrBlocks { blocks: vec![BIrBlock {
        params: 1,
        stmts: vec![BIrStmt::One], // var 1 = constant 1
        stmt_provs: vec![],
        terminator: BIrTerminator::CondJmp {
            val: IRVarId(0),
            then_target: BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
            else_target: BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(0)),
                args: vec![IRVarId(1)],
            },
        },
    }], pre_init: vec![] }
}

// ============================================================================
// IRBlocks circuit builders (typed, using Poly)
// ============================================================================

use std::collections::BTreeMap;

fn bit_types() -> IRTypes {
    IRTypes(vec![CommonIrType::Primitive(Type::Bit)])
}

fn bit_tid() -> IRTypeId {
    IRTypeId(0)
}

/// IR: 2-bit XOR via Poly.
pub fn make_ir_xor() -> (IRBlocks, IRTypes) {
    let types = bit_types();
    let mut coeffs = BTreeMap::new();
    coeffs.insert(vec![IRVarId(0)], 1u8);
    coeffs.insert(vec![IRVarId(1)], 1u8);
    let blocks = IRBlocks::new(vec![IRBlock {
        params: vec![bit_tid(), bit_tid()],
        stmts: vec![IRStmt::Poly {
            ty: bit_tid(),
            coeffs,
            constant: Constant { hi: 0, lo: 0 },
        }],
        stmt_provs: vec![],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: vec![IRVarId(2)],
        },
    }]);
    (blocks, types)
}

/// IR: 2-bit AND via Poly.
pub fn make_ir_and() -> (IRBlocks, IRTypes) {
    let types = bit_types();
    let mut coeffs = BTreeMap::new();
    let mut key = vec![IRVarId(0), IRVarId(1)];
    key.sort();
    coeffs.insert(key, 1u8);
    let blocks = IRBlocks::new(vec![IRBlock {
        params: vec![bit_tid(), bit_tid()],
        stmts: vec![IRStmt::Poly {
            ty: bit_tid(),
            coeffs,
            constant: Constant { hi: 0, lo: 0 },
        }],
        stmt_provs: vec![],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: vec![IRVarId(2)],
        },
    }]);
    (blocks, types)
}

/// IR: 1-bit NOT via Poly (a + 1 in GF(2)).
pub fn make_ir_not() -> (IRBlocks, IRTypes) {
    let types = bit_types();
    let mut coeffs = BTreeMap::new();
    coeffs.insert(vec![IRVarId(0)], 1u8);
    let blocks = IRBlocks::new(vec![IRBlock {
        params: vec![bit_tid()],
        stmts: vec![IRStmt::Poly {
            ty: bit_tid(),
            coeffs,
            constant: Constant { hi: 0, lo: 1 },
        }],
        stmt_provs: vec![],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: vec![IRVarId(1)],
        },
    }]);
    (blocks, types)
}
