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

use volar_c_backend::CBackend;
use volar_ir::boolar::BIrBlocks;
use volar_ir::ir::{IRBlocks, IRTypes};
use volar_ir_passes::{
    LoweringMode,
    lower_lir::{lower_biir, lower_ir},
    lower_to_circuit::lower_to_circuit,
    movfuscate_biir,
};
use volar_lir::LirTarget;
use volar_lir_test_corpus::{
    compile_and_run, make_biir_and, make_biir_half_adder, make_biir_identity, make_biir_not,
    make_biir_self_loop, make_biir_two_block_not, make_biir_xor, make_ir_and, make_ir_not,
    make_ir_xor,
};

/// Construct a fresh `IrExpr` with no provenance/side — test fixtures here
/// have no real source to attribute.
fn ir_expr(kind: volar_compiler::ir::IrExprKind) -> volar_compiler::ir::IrExpr {
    volar_compiler::ir::IrExpr::new(kind, (), None)
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
    let body = format!(r#"  printf("%llu\n", (unsigned long long){name}({args}));"#,);
    let out = compile_and_run(&c, &body);
    let actual: u64 = out
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("parse error: {out:?}"));
    assert_eq!(
        actual, expected,
        "{name}({inputs:?}): expected {expected}, got {actual}"
    );
}

/// Run a typed IR circuit with bool inputs and check the packed u64 output.
fn run_ir(blocks: &IRBlocks, types: &IRTypes, name: &str, inputs: &[bool], expected: u64) {
    let c = ir_to_c(blocks, types, name);
    let args = inputs
        .iter()
        .map(|&v| if v { "1" } else { "0" })
        .collect::<Vec<_>>()
        .join(", ");
    let body = format!(r#"  printf("%llu\n", (unsigned long long){name}({args}));"#,);
    let out = compile_and_run(&c, &body);
    let actual: u64 = out
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("parse error: {out:?}"));
    assert_eq!(
        actual, expected,
        "{name}({inputs:?}): expected {expected}, got {actual}"
    );
}

// ############################################################################
//
// Category 1: BIrBlocks direct circuit lowering
//
// ############################################################################

#[test]
fn biir_direct_identity() {
    let c = make_biir_identity();
    run_biir(&c, "id", &[false], 0);
    run_biir(&c, "id", &[true], 1);
}

#[test]
fn biir_direct_not() {
    let c = make_biir_not();
    run_biir(&c, "not", &[false], 1);
    run_biir(&c, "not", &[true], 0);
}

#[test]
fn biir_direct_and() {
    let c = make_biir_and();
    run_biir(&c, "and", &[false, false], 0);
    run_biir(&c, "and", &[false, true], 0);
    run_biir(&c, "and", &[true, false], 0);
    run_biir(&c, "and", &[true, true], 1);
}

#[test]
fn biir_direct_xor() {
    let c = make_biir_xor();
    run_biir(&c, "xor", &[false, false], 0);
    run_biir(&c, "xor", &[false, true], 1);
    run_biir(&c, "xor", &[true, false], 1);
    run_biir(&c, "xor", &[true, true], 0);
}

#[test]
fn biir_direct_half_adder() {
    let c = make_biir_half_adder();
    // Output: bit 0 = sum (XOR), bit 1 = carry (AND).
    // Packed u64: carry << 1 | sum.
    run_biir(&c, "ha", &[false, false], 0b00); // sum=0, carry=0
    run_biir(&c, "ha", &[false, true], 0b01); // sum=1, carry=0
    run_biir(&c, "ha", &[true, false], 0b01); // sum=1, carry=0
    run_biir(&c, "ha", &[true, true], 0b10); // sum=0, carry=1
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
    let dag = make_biir_two_block_not();
    let movf = movfuscate_biir(&dag);
    assert!(
        movf.is_movfuscated(),
        "should be single block after movfuscation"
    );
    assert_eq!(
        movf.blocks[0].params, 2,
        "combined block has pc(1) + state(1) params"
    );
    let circuit = lower_to_circuit(&movf, 4, LoweringMode::Unconditional);
    assert!(
        circuit.is_circuit(),
        "should be a flat circuit after lower_to_circuit"
    );
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
    let looped = make_biir_self_loop();
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
    let looped = make_biir_self_loop();
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
    let (blocks, types) = make_ir_xor();
    run_ir(&blocks, &types, "ir_xor", &[false, false], 0);
    run_ir(&blocks, &types, "ir_xor", &[false, true], 1);
    run_ir(&blocks, &types, "ir_xor", &[true, false], 1);
    run_ir(&blocks, &types, "ir_xor", &[true, true], 0);
}

#[test]
fn ir_direct_and() {
    let (blocks, types) = make_ir_and();
    run_ir(&blocks, &types, "ir_and", &[false, false], 0);
    run_ir(&blocks, &types, "ir_and", &[false, true], 0);
    run_ir(&blocks, &types, "ir_and", &[true, false], 0);
    run_ir(&blocks, &types, "ir_and", &[true, true], 1);
}

#[test]
fn ir_direct_not() {
    let (blocks, types) = make_ir_not();
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
    use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};

    // Module: #[oracle] fn double(x: u64) -> u64;
    //         fn call_it(x: u64) -> u64 { double(x) }
    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        consts: vec![],
        type_aliases: vec![],
        functions: vec![
            IrFunction {
                no_inline: false,
                name: "double".to_owned(),
                module_path: vec![],
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
                    expr: None,
                },
                external_kind: ExternalKind::Oracle,
            },
            IrFunction {
                no_inline: false,
                name: "call_it".to_owned(),
                module_path: vec![],
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
                    expr: Some(Box::new(ir_expr(IrExprKind::Call {
                        func: Box::new(ir_expr(IrExprKind::Var("double".to_owned()))),
                        args: vec![ir_expr(IrExprKind::Var("x".to_owned()))],
                    }))),
                },
                external_kind: ExternalKind::Normal,
            },
        ],
    };

    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &MonoEnv::new(""));
    let c_src = b.finish();

    // Provide the oracle stub BEFORE main (as a top-level function).
    let full_c = format!("{c_src}\nuint64_t oracle_double(uint64_t x) {{ return x * 2; }}\n");
    let out = compile_and_run(
        &full_c,
        r#"printf("%llu\n", (unsigned long long)call_it(21));"#,
    );
    assert_eq!(out.trim(), "42");
}

#[test]
fn compiler_rng_dispatch() {
    use volar_compiler::ir::*;
    use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};

    // Module: #[rng] fn get_rand() -> u64;
    //         fn use_rng() -> u64 { get_rand() }
    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        consts: vec![],
        type_aliases: vec![],
        functions: vec![
            IrFunction {
                no_inline: false,
                name: "get_rand".to_owned(),
                module_path: vec![],
                generics: vec![],
                receiver: None,
                params: vec![],
                return_type: Some(IrType::Primitive(PrimitiveType::U64)),
                where_clause: vec![],
                body: IrBlock {
                    stmts: vec![],
                    expr: None,
                },
                external_kind: ExternalKind::Rng,
            },
            IrFunction {
                no_inline: false,
                name: "use_rng".to_owned(),
                module_path: vec![],
                generics: vec![],
                receiver: None,
                params: vec![],
                return_type: Some(IrType::Primitive(PrimitiveType::U64)),
                where_clause: vec![],
                body: IrBlock {
                    stmts: vec![],
                    expr: Some(Box::new(ir_expr(IrExprKind::Call {
                        func: Box::new(ir_expr(IrExprKind::Var("get_rand".to_owned()))),
                        args: vec![],
                    }))),
                },
                external_kind: ExternalKind::Normal,
            },
        ],
    };

    let mut b = CBackend::new().with_rng_fn("test_rng".to_owned());
    lower_module_with_opts(&module, &mut b, &MonoEnv::new(""));
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

// ############################################################################
//
// Category 6: Enum round-trip
//
// Tests for enum representation (tag + payload flat layout), construction,
// and pattern matching via ir_expr(IrExprKind::Match).
//
// ############################################################################

/// Build a module containing an `Option<u32>` enum and two functions:
/// - `make_some(x: u32) -> Option<u32>` returns `Some(x)`
/// - `unwrap_or(opt: Option<u32>, default: u32) -> u32` — returns inner value or default
#[test]
fn enum_option_roundtrip() {
    use volar_compiler::ir::*;
    use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};

    // enum Option<u32> { None, Some(u32) }  — hand-built IrEnum
    let option_enum = IrEnum {
        kind: StructKind::Custom("OptionU32".into()),
        generics: vec![],
        variants: vec![
            IrEnumVariant {
                name: "None".into(),
                fields: IrEnumVariantData::Unit,
            },
            IrEnumVariant {
                name: "Some".into(),
                fields: IrEnumVariantData::Tuple(vec![IrType::Primitive(PrimitiveType::U32)]),
            },
        ],
        derives: vec![],
    };

    let opt_ty = IrType::Struct {
        kind: StructKind::Custom("OptionU32".into()),
        type_args: vec![],
    };

    // fn make_some(x: u32) -> OptionU32 { Some(x) }
    let make_some = IrFunction {
        no_inline: false,
        name: "make_some".into(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![IrParam {
            name: "x".into(),
            ty: IrType::Primitive(PrimitiveType::U32),
        }],
        return_type: Some(opt_ty.clone()),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(ir_expr(IrExprKind::Call {
                func: Box::new(ir_expr(IrExprKind::Path {
                    segments: vec!["Some".into()],
                    type_args: vec![],
                })),
                args: vec![ir_expr(IrExprKind::Var("x".into()))],
            }))),
        },
        external_kind: ExternalKind::Normal,
    };

    // fn make_none() -> OptionU32 { None }
    let make_none = IrFunction {
        no_inline: false,
        name: "make_none".into(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![],
        return_type: Some(opt_ty.clone()),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(ir_expr(IrExprKind::Path {
                segments: vec!["None".into()],
                type_args: vec![],
            }))),
        },
        external_kind: ExternalKind::Normal,
    };

    // fn unwrap_or(opt: OptionU32, default: u32) -> u32 {
    //     match opt { Some(v) => v, None => default, _ => default }
    // }
    let unwrap_or = IrFunction {
        no_inline: false,
        name: "unwrap_or".into(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![
            IrParam {
                name: "opt".into(),
                ty: opt_ty.clone(),
            },
            IrParam {
                name: "default".into(),
                ty: IrType::Primitive(PrimitiveType::U32),
            },
        ],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(ir_expr(IrExprKind::Match {
                expr: Box::new(ir_expr(IrExprKind::Var("opt".into()))),
                arms: vec![
                    IrMatchArm {
                        pattern: IrPattern::TupleStruct {
                            kind: StructKind::Custom("Some".into()),
                            elems: vec![IrPattern::Ident {
                                mutable: false,
                                name: "v".into(),
                                subpat: None,
                            }],
                        },
                        guard: None,
                        body: ir_expr(IrExprKind::Var("v".into())),
                    },
                    IrMatchArm {
                        pattern: IrPattern::Wild,
                        guard: None,
                        body: ir_expr(IrExprKind::Var("default".into())),
                    },
                ],
            }))),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "test".into(),
        structs: vec![],
        enums: vec![option_enum],
        traits: vec![],
        impls: vec![],
        consts: vec![],
        type_aliases: vec![],
        functions: vec![make_some, make_none, unwrap_or],
    };

    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &MonoEnv::new(""));
    let c_src = b.finish();

    // make_some(42) → Some(42), unwrap_or(Some(42), 0) = 42
    let out = compile_and_run(
        &c_src,
        r#"printf("%u %u\n", (unsigned)unwrap_or(make_some(42), 0), (unsigned)unwrap_or(make_none(), 99));"#,
    );
    assert_eq!(out.trim(), "42 99");
}
