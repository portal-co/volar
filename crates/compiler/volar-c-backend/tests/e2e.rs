// @reliability: normal
//! End-to-end tests: construct IR → run passes → lower to C → compile → execute.
//!
//! If a test fails, the pass under test is producing incorrect output.
//! The C backend and native execution are the verification mechanism, not the
//! unit under test.
//!
//! # Test categories
//!
//! 1. **biir_direct** — `BIrBlocks` single-block circuits → `lower_biir` → C
//! 2. **biir_movfuscate** — multi-block `BIrBlocks` → `movfuscate_biir` → `lower_to_circuit` → C
//! 3. **biir_to_circuit** — self-looping `BIrBlocks` → `lower_to_circuit` → C
//! 4. **ir_direct** — `IRBlocks` circuits (Poly stmts) → `lower_ir` → C
//! 5. **compiler_externals** — `IrModule` with `#[oracle]`/`#[action]`/`#[rng]` → C

use std::collections::BTreeMap;
use std::{fs, process::Command};
use tempfile::TempDir;

use volar_c_backend::CBackend;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId, IRTypes,
    IRVarId,
};
use volar_ir_common::{Constant, IrType as CommonIrType, Type};
use volar_ir_passes::{
    lower_lir::{lower_biir, lower_ir},
    lower_to_circuit::lower_to_circuit,
    movfuscate_biir, LoweringMode,
};
use volar_lir::LirTarget;

// ============================================================================
// Harness
// ============================================================================

fn compile_and_run(c_src: &str, main_body: &str) -> String {
    let dir = TempDir::new().expect("tempdir");
    let c_path = dir.path().join("test.c");
    let exe_path = dir.path().join("test");

    let full_src = format!(
        "{c_src}\n#include <stdio.h>\n#include <string.h>\nint main(void) {{\n{main_body}\n  return 0;\n}}\n"
    );

    fs::write(&c_path, &full_src).expect("write C source");

    let status = Command::new("cc")
        .args(["-O0", "-std=c99", "-o"])
        .arg(&exe_path)
        .arg(&c_path)
        .status()
        .expect("cc not found — install a C compiler");
    assert!(status.success(), "C compilation failed.\nSource:\n{full_src}");

    let output = Command::new(&exe_path)
        .output()
        .expect("failed to run compiled program");
    String::from_utf8(output.stdout).expect("non-UTF8 output")
}

/// Lower `BIrBlocks` to C source via the CBackend.
fn biir_to_c(blocks: &BIrBlocks, name: &str) -> String {
    let mut b = CBackend::new();
    lower_biir(blocks, name, &mut b);
    b.finish()
}

/// Lower `IRBlocks` + `IRTypes` to C source via the CBackend.
fn ir_to_c(blocks: &IRBlocks, types: &IRTypes, name: &str) -> String {
    let mut b = CBackend::new();
    lower_ir(blocks, types, name, &mut b);
    b.finish()
}

/// Run a boolean circuit with bool inputs and check the packed u64 output.
fn run_biir(blocks: &BIrBlocks, name: &str, inputs: &[bool], expected: u64) {
    let c = biir_to_c(blocks, name);
    let args = inputs
        .iter()
        .map(|&v| if v { "1" } else { "0" })
        .collect::<Vec<_>>()
        .join(", ");
    let body = format!(
        r#"  printf("%llu\n", (unsigned long long){name}({args}));"#,
    );
    let out = compile_and_run(&c, &body);
    let actual: u64 = out.trim().parse().unwrap_or_else(|_| panic!("parse error: {out:?}"));
    assert_eq!(actual, expected, "{name}({inputs:?}): expected {expected}, got {actual}");
}

/// Run a typed IR circuit with bool inputs and check the packed u64 output.
fn run_ir(blocks: &IRBlocks, types: &IRTypes, name: &str, inputs: &[bool], expected: u64) {
    let c = ir_to_c(blocks, types, name);
    let args = inputs
        .iter()
        .map(|&v| if v { "1" } else { "0" })
        .collect::<Vec<_>>()
        .join(", ");
    let body = format!(
        r#"  printf("%llu\n", (unsigned long long){name}({args}));"#,
    );
    let out = compile_and_run(&c, &body);
    let actual: u64 = out.trim().parse().unwrap_or_else(|_| panic!("parse error: {out:?}"));
    assert_eq!(actual, expected, "{name}({inputs:?}): expected {expected}, got {actual}");
}

// ============================================================================
// Shared BIrBlocks corpus
// ============================================================================

/// 1-bit identity: return the input.
fn biir_identity() -> BIrBlocks {
    BIrBlocks(vec![BIrBlock {
        params: 1,
        stmts: vec![],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(0)],
        }),
    }])
}

/// 1-bit NOT.
fn biir_not() -> BIrBlocks {
    BIrBlocks(vec![BIrBlock {
        params: 1,
        stmts: vec![BIrStmt::Not(IRVarId(0))],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(1)],
        }),
    }])
}

/// 2-bit AND.
fn biir_and() -> BIrBlocks {
    BIrBlocks(vec![BIrBlock {
        params: 2,
        stmts: vec![BIrStmt::And(IRVarId(0), IRVarId(1))],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(2)],
        }),
    }])
}

/// 2-bit XOR.
fn biir_xor() -> BIrBlocks {
    BIrBlocks(vec![BIrBlock {
        params: 2,
        stmts: vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(2)],
        }),
    }])
}

/// Half adder: 2 inputs → (sum=XOR, carry=AND) packed as 2-bit output.
fn biir_half_adder() -> BIrBlocks {
    BIrBlocks(vec![BIrBlock {
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
    }])
}

/// Two-block NOT: block 0 → block 1 → return NOT(input).
fn biir_two_block_not() -> BIrBlocks {
    BIrBlocks(vec![
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
    ])
}

/// Self-loop: if input=1 return input, else loop with constant 1.
/// Output is always 1 (terminates in ≤1 iteration).
fn biir_self_loop() -> BIrBlocks {
    BIrBlocks(vec![BIrBlock {
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
    }])
}

// ============================================================================
// Shared IRBlocks corpus (Bit-typed, using Poly)
// ============================================================================

fn bit_types() -> IRTypes {
    IRTypes(vec![CommonIrType::Primitive(Type::Bit)])
}

fn bit_tid() -> IRTypeId {
    IRTypeId(0)
}

/// IR: 2-bit XOR via Poly.
fn ir_xor() -> (IRBlocks, IRTypes) {
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
fn ir_and() -> (IRBlocks, IRTypes) {
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
fn ir_not() -> (IRBlocks, IRTypes) {
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

// ############################################################################
//
// Category 1: BIrBlocks direct circuit lowering
//
// ############################################################################

#[test]
fn biir_direct_identity() {
    let c = biir_identity();
    run_biir(&c, "id", &[false], 0);
    run_biir(&c, "id", &[true], 1);
}

#[test]
fn biir_direct_not() {
    let c = biir_not();
    run_biir(&c, "not", &[false], 1);
    run_biir(&c, "not", &[true], 0);
}

#[test]
fn biir_direct_and() {
    let c = biir_and();
    run_biir(&c, "and", &[false, false], 0);
    run_biir(&c, "and", &[false, true], 0);
    run_biir(&c, "and", &[true, false], 0);
    run_biir(&c, "and", &[true, true], 1);
}

#[test]
fn biir_direct_xor() {
    let c = biir_xor();
    run_biir(&c, "xor", &[false, false], 0);
    run_biir(&c, "xor", &[false, true], 1);
    run_biir(&c, "xor", &[true, false], 1);
    run_biir(&c, "xor", &[true, true], 0);
}

#[test]
fn biir_direct_half_adder() {
    let c = biir_half_adder();
    // Output: bit 0 = sum (XOR), bit 1 = carry (AND).
    // Packed u64: carry << 1 | sum.
    run_biir(&c, "ha", &[false, false], 0b00); // sum=0, carry=0
    run_biir(&c, "ha", &[false, true], 0b01);  // sum=1, carry=0
    run_biir(&c, "ha", &[true, false], 0b01);  // sum=1, carry=0
    run_biir(&c, "ha", &[true, true], 0b10);   // sum=0, carry=1
}

// ############################################################################
//
// Category 2: BIrBlocks movfuscation → circuit → C
//
// ############################################################################

#[test]
fn biir_movfuscate_two_block_not() {
    // Two-block DAG: block 0 → block 1 (NOT) → return.
    // movfuscate → single self-loop with params [pc_bit, state_bit]
    // lower_to_circuit flattens the loop into a pure circuit.
    //
    // To execute: pc=0 ("start at block 0"), state=input.
    // pc=0 activates block 0 which passes state to block 1;
    // pc=1 activates block 1 which returns NOT(state).
    // With limit≥2, both iterations complete and the MUX selects the final result.
    let dag = biir_two_block_not();
    let movf = movfuscate_biir(&dag);
    assert!(movf.is_movfuscated(), "should be single block after movfuscation");
    assert_eq!(movf.0[0].params, 2, "combined block has pc(1) + state(1) params");
    let circuit = lower_to_circuit(&movf, 4, LoweringMode::Unconditional);
    assert!(circuit.is_circuit(), "should be a flat circuit after lower_to_circuit");
    // [pc=false, state=false] → NOT(false) = 1
    run_biir(&circuit, "movf_not", &[false, false], 1);
    // [pc=false, state=true]  → NOT(true)  = 0
    run_biir(&circuit, "movf_not", &[false, true], 0);
}

// ############################################################################
//
// Category 3: BIrBlocks lower-to-circuit (self-loop unrolling)
//
// ############################################################################

#[test]
fn biir_lower_to_circuit_self_loop() {
    // Self-loop: if input=1, return it; else loop with 1.
    // After ≤1 iteration the output is always 1.
    let looped = biir_self_loop();
    let circuit = lower_to_circuit(&looped, 4, LoweringMode::Unconditional);
    assert!(circuit.is_circuit());
    run_biir(&circuit, "loop_c", &[false], 1);
    run_biir(&circuit, "loop_c", &[true], 1);
}

#[test]
fn biir_lower_to_circuit_with_flag() {
    // Same self-loop, but WithTerminationFlag → prepends a done bit.
    // Output: bit 0 = done flag, bit 1 = result.
    // Both inputs terminate within 4 iterations → done=1.
    let looped = biir_self_loop();
    let circuit = lower_to_circuit(&looped, 4, LoweringMode::WithTerminationFlag);
    assert!(circuit.is_circuit());
    // input=false: done=1, result=1 → packed 0b11 = 3
    run_biir(&circuit, "loop_f", &[false], 0b11);
    // input=true:  done=1, result=1 → packed 0b11 = 3
    run_biir(&circuit, "loop_f", &[true], 0b11);
}

// ############################################################################
//
// Category 4: IRBlocks direct circuit lowering (Poly stmts)
//
// ############################################################################

#[test]
fn ir_direct_xor() {
    let (blocks, types) = ir_xor();
    run_ir(&blocks, &types, "ir_xor", &[false, false], 0);
    run_ir(&blocks, &types, "ir_xor", &[false, true], 1);
    run_ir(&blocks, &types, "ir_xor", &[true, false], 1);
    run_ir(&blocks, &types, "ir_xor", &[true, true], 0);
}

#[test]
fn ir_direct_and() {
    let (blocks, types) = ir_and();
    run_ir(&blocks, &types, "ir_and", &[false, false], 0);
    run_ir(&blocks, &types, "ir_and", &[false, true], 0);
    run_ir(&blocks, &types, "ir_and", &[true, false], 0);
    run_ir(&blocks, &types, "ir_and", &[true, true], 1);
}

#[test]
fn ir_direct_not() {
    let (blocks, types) = ir_not();
    run_ir(&blocks, &types, "ir_not", &[false], 1);
    run_ir(&blocks, &types, "ir_not", &[true], 0);
}

// ############################################################################
//
// Category 5: Compiler IR with oracle / action / rng dispatch
//
// ############################################################################

#[test]
fn compiler_oracle_dispatch() {
    use volar_compiler::ir::*;
    use volar_lir_codegen::lower_module_with_opts;

    // Module: #[oracle] fn double(x: u64) -> u64;
    //         fn call_it(x: u64) -> u64 { double(x) }
    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        functions: vec![
            IrFunction {
                name: "double".to_owned(),
                generics: vec![],
                receiver: None,
                params: vec![IrParam {
                    name: "x".to_owned(),
                    ty: IrType::Primitive(PrimitiveType::U64),
                }],
                return_type: Some(IrType::Primitive(PrimitiveType::U64)),
                where_clause: vec![],
                body: IrBlock { stmts: vec![], stmt_provs: vec![], expr: None },
                external_kind: ExternalKind::Oracle,
            },
            IrFunction {
                name: "call_it".to_owned(),
                generics: vec![],
                receiver: None,
                params: vec![IrParam {
                    name: "x".to_owned(),
                    ty: IrType::Primitive(PrimitiveType::U64),
                }],
                return_type: Some(IrType::Primitive(PrimitiveType::U64)),
                where_clause: vec![],
                body: IrBlock {
                    stmts: vec![],
                    stmt_provs: vec![],
                    expr: Some(Box::new(IrExpr::Call {
                        func: Box::new(IrExpr::Var("double".to_owned())),
                        args: vec![IrExpr::Var("x".to_owned())],
                    })),
                },
                external_kind: ExternalKind::Normal,
            },
        ],
    };

    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, "");
    let c_src = b.finish();

    // Provide the oracle stub BEFORE main (as a top-level function).
    let full_c = format!(
        "{c_src}\nuint64_t oracle_double(uint64_t x) {{ return x * 2; }}\n"
    );
    let out = compile_and_run(
        &full_c,
        r#"printf("%llu\n", (unsigned long long)call_it(21));"#,
    );
    assert_eq!(out.trim(), "42");
}

#[test]
fn compiler_rng_dispatch() {
    use volar_compiler::ir::*;
    use volar_lir_codegen::lower_module_with_opts;

    // Module: #[rng] fn get_rand() -> u64;
    //         fn use_rng() -> u64 { get_rand() }
    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        functions: vec![
            IrFunction {
                name: "get_rand".to_owned(),
                generics: vec![],
                receiver: None,
                params: vec![],
                return_type: Some(IrType::Primitive(PrimitiveType::U64)),
                where_clause: vec![],
                body: IrBlock { stmts: vec![], stmt_provs: vec![], expr: None },
                external_kind: ExternalKind::Rng,
            },
            IrFunction {
                name: "use_rng".to_owned(),
                generics: vec![],
                receiver: None,
                params: vec![],
                return_type: Some(IrType::Primitive(PrimitiveType::U64)),
                where_clause: vec![],
                body: IrBlock {
                    stmts: vec![],
                    stmt_provs: vec![],
                    expr: Some(Box::new(IrExpr::Call {
                        func: Box::new(IrExpr::Var("get_rand".to_owned())),
                        args: vec![],
                    })),
                },
                external_kind: ExternalKind::Normal,
            },
        ],
    };

    let mut b = CBackend::new().with_rng_fn("test_rng".to_owned());
    lower_module_with_opts(&module, &mut b, "");
    let c_src = b.finish();

    // The RNG stub must be defined *before* the generated `use_rng()` which
    // calls it.  Prepend the stub (with its own needed headers) before c_src.
    let full_c = format!(
        "#include <string.h>\nvoid test_rng(void *out, size_t len) {{ memset(out, 0x42, len); }}\n{c_src}\n"
    );
    let out = compile_and_run(
        &full_c,
        r#"printf("%llu\n", (unsigned long long)use_rng());"#,
    );
    // 8 bytes of 0x42 = 0x4242424242424242
    assert_eq!(out.trim(), "4774451407313060418");
}
