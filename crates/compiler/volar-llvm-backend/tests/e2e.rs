// @reliability: normal
//! End-to-end tests: construct IR → lower to LLVM → object file → link → run.
//!
//! The LLVM backend is the unit under test.  Correctness is verified by
//! compiling the emitted object file with `cc`, running the resulting
//! executable, and comparing its stdout to the expected value.
//!
//! # Test categories
//!
//! 1. **biir_direct** — `BIrBlocks` → `lower_biir` → LlvmBackend → run
//! 2. **ir_direct**   — `IRBlocks`  → `lower_ir`   → LlvmBackend → run

use std::collections::BTreeMap;
use std::{fs, process::Command};
use tempfile::TempDir;

use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::OptimizationLevel;

use volar_llvm_backend::LlvmBackend;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRTypeId, IRTypes,
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
// Test harness
// ============================================================================

/// Initialise the native LLVM target once per process.
fn init_target() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| {
        Target::initialize_native(&InitializationConfig::default())
            .expect("failed to initialise native LLVM target");
    });
}

/// Create a `TargetMachine` for the native target with no optimisation.
fn native_machine() -> TargetMachine {
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).expect("target from triple");
    target
        .create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default,
        )
        .expect("create target machine")
}

/// Compile an LLVM module to an object file, link with a C `main` stub that
/// calls the generated function, and return the program's stdout.
///
/// `decl` is a C declaration for the generated function (e.g. `"uint64_t
/// f(uint64_t a, uint64_t b);"`) and `main_body` is the body of `main()`.
fn compile_and_run(
    backend: LlvmBackend<'_>,
    decl: &str,
    main_body: &str,
) -> String {
    init_target();

    let dir = TempDir::new().expect("tempdir");
    let obj_path = dir.path().join("test.o");
    let c_path = dir.path().join("main.c");
    let exe_path = dir.path().join("test");

    // Emit the object file.
    let module = backend.finish();
    module.verify().expect("LLVM module verify failed");
    let machine = native_machine();
    machine
        .write_to_file(&module, FileType::Object, &obj_path)
        .expect("write object file");

    // Write the C main stub.
    let c_src = format!(
        "#include <stdio.h>\n#include <stdint.h>\n#include <stdbool.h>\n\
         {decl}\n\
         int main(void) {{\n{main_body}\n  return 0;\n}}\n"
    );
    fs::write(&c_path, &c_src).expect("write C main");

    // Link.
    let status = Command::new("cc")
        .args(["-o"])
        .arg(&exe_path)
        .arg(&obj_path)
        .arg(&c_path)
        .status()
        .expect("cc not found — install a C compiler");
    assert!(status.success(), "linking failed.\nC stub:\n{c_src}");

    // Run.
    let output = Command::new(&exe_path)
        .output()
        .expect("failed to run compiled program");
    String::from_utf8(output.stdout).expect("non-UTF8 output")
}

/// Lower `BIrBlocks` to an `LlvmBackend` using `lower_biir`.
fn make_biir_backend<'ctx>(
    ctx: &'ctx Context,
    blocks: &BIrBlocks,
    name: &str,
) -> LlvmBackend<'ctx> {
    let mut b = LlvmBackend::new(ctx, name);
    lower_biir(blocks, name, &mut b);
    b
}

/// Run a boolean circuit and check the packed u64 output.
///
/// Inputs are individual bools (0/1); the circuit output is a packed u64
/// (same convention as the C backend tests).
fn run_biir(blocks: &BIrBlocks, name: &str, inputs: &[bool], expected: u64) {
    let ctx = Context::create();
    let b = make_biir_backend(&ctx, blocks, name);

    let n = inputs.len();
    let params: Vec<String> = (0..n).map(|_| "uint8_t".to_string()).collect();
    let param_names: Vec<String> = (0..n).map(|i| format!("a{i}")).collect();
    let decl = format!(
        "uint64_t {name}({});",
        params
            .iter()
            .zip(param_names.iter())
            .map(|(t, n)| format!("{t} {n}"))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let args = inputs
        .iter()
        .zip(param_names.iter())
        .map(|(&v, n)| format!("(uint8_t){}", if v { 1 } else { 0 }))
        .collect::<Vec<_>>()
        .join(", ");
    let body = format!(r#"  printf("%llu\n", (unsigned long long){name}({args}));"#);

    let out = compile_and_run(b, &decl, &body);
    let actual: u64 = out
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("parse error: {out:?}"));
    assert_eq!(
        actual, expected,
        "{name}({inputs:?}): expected {expected}, got {actual}"
    );
}

// ============================================================================
// Shared BIrBlocks corpus (same as C backend tests)
// ============================================================================

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

fn biir_half_adder() -> BIrBlocks {
    BIrBlocks(vec![BIrBlock {
        params: 2,
        stmts: vec![
            BIrStmt::Xor(IRVarId(0), IRVarId(1)), // sum
            BIrStmt::And(IRVarId(0), IRVarId(1)), // carry
        ],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(2), IRVarId(3)],
        }),
    }])
}

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

fn biir_self_loop() -> BIrBlocks {
    BIrBlocks(vec![BIrBlock {
        params: 1,
        stmts: vec![BIrStmt::One],
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
// biir_direct tests
// ============================================================================

#[test]
fn biir_direct_identity_0() {
    run_biir(&biir_identity(), "biir_identity", &[false], 0);
}

#[test]
fn biir_direct_identity_1() {
    run_biir(&biir_identity(), "biir_identity1", &[true], 1);
}

#[test]
fn biir_direct_not_0() {
    run_biir(&biir_not(), "biir_not0", &[false], 1);
}

#[test]
fn biir_direct_not_1() {
    run_biir(&biir_not(), "biir_not1", &[true], 0);
}

#[test]
fn biir_direct_and() {
    run_biir(&biir_and(), "biir_and_ff", &[false, false], 0);
    // Cannot reuse same backend; create fresh for each call above.
    // Note: run_biir creates a fresh context each time.
}

#[test]
fn biir_direct_and_tt() {
    run_biir(&biir_and(), "biir_and_tt", &[true, true], 1);
}

#[test]
fn biir_direct_and_tf() {
    run_biir(&biir_and(), "biir_and_tf", &[true, false], 0);
}

#[test]
fn biir_direct_xor_tf() {
    run_biir(&biir_xor(), "biir_xor_tf", &[true, false], 1);
}

#[test]
fn biir_direct_xor_tt() {
    run_biir(&biir_xor(), "biir_xor_tt", &[true, true], 0);
}

#[test]
fn biir_direct_half_adder_tt() {
    // 1+1 = 0 sum, 1 carry → packed: sum=bit0, carry=bit1 → 0b10 = 2
    run_biir(&biir_half_adder(), "biir_ha_tt", &[true, true], 2);
}

#[test]
fn biir_direct_half_adder_tf() {
    // 1+0 = 1 sum, 0 carry → packed: 0b01 = 1
    run_biir(&biir_half_adder(), "biir_ha_tf", &[true, false], 1);
}

#[test]
fn biir_two_block_not_0() {
    run_biir(&biir_two_block_not(), "biir_2bn_0", &[false], 1);
}

#[test]
fn biir_two_block_not_1() {
    run_biir(&biir_two_block_not(), "biir_2bn_1", &[true], 0);
}

// ============================================================================
// biir_movfuscate + lower_to_circuit tests
// ============================================================================

fn run_biir_movfuscate(blocks: &BIrBlocks, name: &str, inputs: &[bool], expected: u64) {
    // movfuscate into a single self-looping block, then unroll to a flat circuit.
    let movfuscated = movfuscate_biir(blocks);
    let circuit = lower_to_circuit(&movfuscated, 16, LoweringMode::Unconditional);

    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, name);
    lower_biir(&circuit, name, &mut b);

    let n = inputs.len();
    let params: Vec<String> = (0..n).map(|_| "uint8_t".to_string()).collect();
    let param_names: Vec<String> = (0..n).map(|i| format!("a{i}")).collect();
    let decl = format!(
        "uint64_t {name}({});",
        params
            .iter()
            .zip(param_names.iter())
            .map(|(t, n)| format!("{t} {n}"))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let args = inputs
        .iter()
        .zip(param_names.iter())
        .map(|(&v, n)| format!("(uint8_t){}", if v { 1 } else { 0 }))
        .collect::<Vec<_>>()
        .join(", ");
    let body = format!(r#"  printf("%llu\n", (unsigned long long){name}({args}));"#);

    let out = compile_and_run(b, &decl, &body);
    let actual: u64 = out
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("parse error: {out:?}"));
    assert_eq!(
        actual, expected,
        "{name}({inputs:?}): expected {expected}, got {actual}"
    );
}

#[test]
fn biir_movfuscate_not_0() {
    run_biir_movfuscate(&biir_not(), "mv_not_0", &[false], 1);
}

#[test]
fn biir_movfuscate_not_1() {
    run_biir_movfuscate(&biir_not(), "mv_not_1", &[true], 0);
}

#[test]
fn biir_movfuscate_and_tt() {
    run_biir_movfuscate(&biir_and(), "mv_and_tt", &[true, true], 1);
}

#[test]
fn biir_movfuscate_xor_tf() {
    run_biir_movfuscate(&biir_xor(), "mv_xor_tf", &[true, false], 1);
}

// ============================================================================
// biir_to_circuit (self-loop unroll) tests
// ============================================================================

#[test]
fn biir_to_circuit_self_loop() {
    // self_loop always outputs 1 regardless of input
    let blocks = biir_self_loop();
    let ctx = Context::create();
    let name = "circuit_self_loop";
    let circuit = lower_to_circuit(&blocks, 4, LoweringMode::Unconditional);
    let mut b = LlvmBackend::new(&ctx, name);
    lower_biir(&circuit, name, &mut b);

    let decl = format!("uint64_t {name}(uint8_t a0);");
    let body = format!(r#"  printf("%llu\n", (unsigned long long){name}(0));"#);
    let out = compile_and_run(b, &decl, &body);
    let actual: u64 = out.trim().parse().unwrap_or_else(|_| panic!("{out:?}"));
    assert_eq!(actual, 1);
}

// ============================================================================
// Shared IRBlocks corpus
// ============================================================================

fn bit_types() -> IRTypes {
    IRTypes(vec![CommonIrType::Primitive(Type::Bit)])
}

fn bit_tid() -> IRTypeId {
    IRTypeId(0)
}

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

// ============================================================================
// ir_direct tests (typed IRBlocks)
// ============================================================================

fn run_ir(blocks: &IRBlocks, types: &IRTypes, name: &str, inputs: &[bool], expected: u64) {
    let ctx = Context::create();
    let mut b = LlvmBackend::new(&ctx, name);
    lower_ir(blocks, types, name, &mut b);

    let n = inputs.len();
    let params: Vec<String> = (0..n).map(|_| "uint8_t".to_string()).collect();
    let param_names: Vec<String> = (0..n).map(|i| format!("a{i}")).collect();
    let decl = format!(
        "uint64_t {name}({});",
        params
            .iter()
            .zip(param_names.iter())
            .map(|(t, n)| format!("{t} {n}"))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let args = inputs
        .iter()
        .zip(param_names.iter())
        .map(|(&v, _n)| format!("(uint8_t){}", if v { 1 } else { 0 }))
        .collect::<Vec<_>>()
        .join(", ");
    let body = format!(r#"  printf("%llu\n", (unsigned long long){name}({args}));"#);

    let out = compile_and_run(b, &decl, &body);
    let actual: u64 = out
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("parse error: {out:?}"));
    assert_eq!(
        actual, expected,
        "{name}({inputs:?}): expected {expected}, got {actual}"
    );
}

#[test]
fn ir_direct_xor_tf() {
    let (blocks, types) = ir_xor();
    run_ir(&blocks, &types, "ir_xor_tf", &[true, false], 1);
}

#[test]
fn ir_direct_xor_tt() {
    let (blocks, types) = ir_xor();
    run_ir(&blocks, &types, "ir_xor_tt", &[true, true], 0);
}

#[test]
fn ir_direct_and_tt() {
    let (blocks, types) = ir_and();
    run_ir(&blocks, &types, "ir_and_tt", &[true, true], 1);
}

#[test]
fn ir_direct_and_tf() {
    let (blocks, types) = ir_and();
    run_ir(&blocks, &types, "ir_and_tf", &[true, false], 0);
}

#[test]
fn ir_direct_not_0() {
    let (blocks, types) = ir_not();
    run_ir(&blocks, &types, "ir_not_0", &[false], 1);
}

#[test]
fn ir_direct_not_1() {
    let (blocks, types) = ir_not();
    run_ir(&blocks, &types, "ir_not_1", &[true], 0);
}
