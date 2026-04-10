// @reliability: normal
//! Integration tests: emit C, compile with `cc`, run, check stdout.

use std::{fs, process::Command};
use tempfile::TempDir;
use volar_c_backend::CBackend;
use volar_lir::{IcmpPred, LirTarget, LirType};

// ============================================================================
// Helper: compile + run C source with an appended main
// ============================================================================

fn compile_and_run(c_src: &str, main_body: &str) -> String {
    let dir = TempDir::new().expect("tempdir");
    let c_path = dir.path().join("test.c");
    let exe_path = dir.path().join("test");

    let full_src = format!(
        "{c_src}\n#include <stdio.h>\nint main(void) {{\n{main_body}\n  return 0;\n}}\n"
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
// Test 1: simple addition
// ============================================================================

#[test]
fn test_add_two() {
    let mut b = CBackend::new();

    let (entry, params) =
        b.begin_function("add_two", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    // Scalar params: each group has exactly one value.
    let sum = b.add(params[0][0], params[1][0]);
    b.ret(&[sum]);
    b.end_function();

    let c_src = b.finish();
    let output = compile_and_run(
        &c_src,
        r#"  printf("%u\n", add_two(3u, 4u));"#,
    );
    assert_eq!(output.trim(), "7");
}

// ============================================================================
// Test 2: countdown loop (sum 1..=10 = 55)
// ============================================================================

#[test]
fn test_countdown() {
    let mut b = CBackend::new();

    // countdown(n: u64, acc: u64) -> u64
    let (entry, entry_params) = b.begin_function(
        "countdown",
        &[LirType::U64, LirType::U64],
        Some(LirType::U64),
    );
    let n_init   = entry_params[0][0];
    let acc_init = entry_params[1][0];

    let loop_block = b.create_block();
    let counter = b.add_block_param(loop_block, LirType::U64);
    let accum   = b.add_block_param(loop_block, LirType::U64);

    let done_block  = b.create_block();
    let done_result = b.add_block_param(done_block, LirType::U64);

    b.switch_to_block(entry);
    b.jump(loop_block, &[n_init, acc_init]);

    b.switch_to_block(loop_block);
    let zero    = b.iconst(LirType::U64, 0);
    let cond    = b.icmp(IcmpPred::Eq, counter, zero);
    let new_acc = b.add(accum, counter);
    let one     = b.iconst(LirType::U64, 1);
    let new_ctr = b.sub(counter, one);
    b.branch(cond, done_block, &[accum], loop_block, &[new_ctr, new_acc]);

    b.switch_to_block(done_block);
    b.ret(&[done_result]);
    b.end_function();

    let c_src = b.finish();
    let output = compile_and_run(
        &c_src,
        r#"  printf("%llu\n", (unsigned long long)countdown(10ull, 0ull));"#,
    );
    assert_eq!(output.trim(), "55");
}

// ============================================================================
// Test 3: if/else via IrModule lowering
// ============================================================================

#[test]
fn test_if_max_via_codegen() {
    use volar_compiler::ir::{
        IrBlock, IrExpr, IrFunction, IrParam, IrType, PrimitiveType,
        SpecBinOp,
    };
    use volar_lir_codegen::lower_function;

    let func = IrFunction {
        name: "ir_max".to_owned(),
        generics: vec![],
        receiver: None,
        params: vec![
            IrParam { name: "a".to_owned(), ty: IrType::Primitive(PrimitiveType::U32) },
            IrParam { name: "b".to_owned(), ty: IrType::Primitive(PrimitiveType::U32) },
        ],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(IrExpr::If {
                cond: Box::new(IrExpr::Binary {
                    op: SpecBinOp::Gt,
                    left: Box::new(IrExpr::Var("a".to_owned())),
                    right: Box::new(IrExpr::Var("b".to_owned())),
                }),
                then_branch: IrBlock {
                    stmts: vec![],
                    expr: Some(Box::new(IrExpr::Var("a".to_owned()))),
                },
                else_branch: Some(Box::new(IrExpr::Var("b".to_owned()))),
            })),
        },
    };

    let mut b = CBackend::new();
    lower_function(&func, &mut b);
    let c_src = b.finish();

    let output = compile_and_run(
        &c_src,
        r#"  printf("%llu\n", (unsigned long long)ir_max(5u, 3u));"#,
    );
    assert_eq!(output.trim(), "5");
}

// ============================================================================
// Test 4: array splatting via IrModule lowering
//
// fn arr_sum(a: [u8; 4]) -> u32
//   (a[0] as u32) + (a[1] as u32) + (a[2] as u32) + (a[3] as u32)
//
// After splatting, the parameter `a` is 4 separate U8 scalars.
// IrExpr::Index generates a select mux tree (all compile-time indices here).
// ============================================================================

#[test]
fn test_array_splat() {
    use volar_compiler::ir::{
        ArrayKind, ArrayLength, IrBlock, IrExpr, IrFunction, IrLit, IrModule, IrParam,
        IrType, PrimitiveType, SpecBinOp,
    };
    use volar_lir_codegen::{lower_module_with_opts, mono::{MonoEnv, monomorphize_module}};

    // fn arr_sum(a: [u8; 4]) -> u32 {
    //     (a[0] as u32) + (a[1] as u32) + (a[2] as u32) + (a[3] as u32)
    // }
    let arr_ty = IrType::Array {
        kind: ArrayKind::FixedArray,
        elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
        len: ArrayLength::Const(4),
    };
    let cast_index = |i: u64| IrExpr::Cast {
        expr: Box::new(IrExpr::Index {
            base: Box::new(IrExpr::Var("a".to_owned())),
            index: Box::new(IrExpr::Lit(IrLit::Int(i.into()))),
        }),
        ty: Box::new(IrType::Primitive(PrimitiveType::U32)),
    };
    let body_expr = IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(IrExpr::Binary {
            op: SpecBinOp::Add,
            left: Box::new(IrExpr::Binary {
                op: SpecBinOp::Add,
                left: Box::new(cast_index(0)),
                right: Box::new(cast_index(1)),
            }),
            right: Box::new(cast_index(2)),
        }),
        right: Box::new(cast_index(3)),
    };

    let func = IrFunction {
        name: "arr_sum".to_owned(),
        generics: vec![],
        receiver: None,
        params: vec![IrParam { name: "a".to_owned(), ty: arr_ty }],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock { stmts: vec![], expr: Some(Box::new(body_expr)) },
    };

    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![],
        traits: vec![],
        impls: vec![],
        functions: vec![func],
        type_aliases: vec![],
    };

    let env = MonoEnv::new("sha256");
    let mono = monomorphize_module(&module, &env);

    let mut b = CBackend::new();
    lower_module_with_opts(&mono, &mut b, "sha256");
    let c_src = b.finish();

    // arr_sum takes Arr_U8_4 — pass { .data = {1, 2, 3, 4} } → 1+2+3+4 = 10
    let output = compile_and_run(
        &c_src,
        r#"  Arr_U8_4 a = { .data = {1, 2, 3, 4} }; printf("%u\n", arr_sum(a));"#,
    );
    assert_eq!(output.trim(), "10");
}

// ============================================================================
// Test 5: struct splatting via IrModule lowering
//
// struct Point { x: u32, y: u32 }
// fn manhattan(p: Point) -> u32 { p.x + p.y }
//
// After splatting, `p` is 2 separate U32 scalars.
// Field access uses the flat-offset approach.
// ============================================================================

#[test]
fn test_struct_splat() {
    use volar_compiler::ir::{
        IrBlock, IrExpr, IrField, IrFunction, IrModule, IrParam, IrStruct, IrType,
        PrimitiveType, SpecBinOp, StructKind,
    };
    use volar_lir_codegen::{lower_module_with_opts, mono::{MonoEnv, monomorphize_module}};

    // struct Point { x: u32, y: u32 }
    let point_struct = IrStruct {
        kind: StructKind::Custom("Point".to_owned()),
        generics: vec![],
        fields: vec![
            IrField { name: "x".to_owned(), ty: IrType::Primitive(PrimitiveType::U32), public: true },
            IrField { name: "y".to_owned(), ty: IrType::Primitive(PrimitiveType::U32), public: true },
        ],
        is_tuple: false,
    };

    let point_ty = IrType::Struct {
        kind: StructKind::Custom("Point".to_owned()),
        type_args: vec![],
    };

    // fn manhattan(p: Point) -> u32 { p.x + p.y }
    let func = IrFunction {
        name: "manhattan".to_owned(),
        generics: vec![],
        receiver: None,
        params: vec![IrParam { name: "p".to_owned(), ty: point_ty }],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(IrExpr::Binary {
                op: SpecBinOp::Add,
                left: Box::new(IrExpr::Field {
                    base: Box::new(IrExpr::Var("p".to_owned())),
                    field: "x".to_owned(),
                }),
                right: Box::new(IrExpr::Field {
                    base: Box::new(IrExpr::Var("p".to_owned())),
                    field: "y".to_owned(),
                }),
            })),
        },
    };

    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![point_struct],
        traits: vec![],
        impls: vec![],
        functions: vec![func],
        type_aliases: vec![],
    };

    let env = MonoEnv::new("sha256");
    let mono = monomorphize_module(&module, &env);

    let mut b = CBackend::new();
    lower_module_with_opts(&mono, &mut b, "sha256");
    let c_src = b.finish();

    // manhattan takes Point by value — pass { .x = 3, .y = 7 } → 10
    let output = compile_and_run(
        &c_src,
        r#"  Point p = { .x = 3, .y = 7 }; printf("%u\n", manhattan(p));"#,
    );
    assert_eq!(output.trim(), "10");
}

// ============================================================================
// Test 6: struct + array via IrModule lowering (Phase 2 codegen)
// ============================================================================

#[test]
fn test_phase2_codegen_struct_array() {
    use volar_compiler::ir::{
        IrBlock, IrExpr, IrFunction, IrModule, IrParam,
        IrType, PrimitiveType, SpecBinOp,
    };
    use volar_lir_codegen::{lower_module_with_opts, mono::{MonoEnv, monomorphize_module}};

    // fn xor_bytes(x: u8, y: u8) -> u8 { x ^ y }
    let func = IrFunction {
        name: "xor_bytes".to_owned(),
        generics: vec![],
        receiver: None,
        params: vec![
            IrParam { name: "x".to_owned(), ty: IrType::Primitive(PrimitiveType::U8) },
            IrParam { name: "y".to_owned(), ty: IrType::Primitive(PrimitiveType::U8) },
        ],
        return_type: Some(IrType::Primitive(PrimitiveType::U8)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(IrExpr::Binary {
                op: SpecBinOp::BitXor,
                left: Box::new(IrExpr::Var("x".to_owned())),
                right: Box::new(IrExpr::Var("y".to_owned())),
            })),
        },
    };

    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![],
        traits: vec![],
        impls: vec![],
        functions: vec![func],
        type_aliases: vec![],
    };

    let env = MonoEnv::new("sha256");
    let mono = monomorphize_module(&module, &env);

    let mut b = CBackend::new();
    lower_module_with_opts(&mono, &mut b, "sha256");
    let c_src = b.finish();

    let output = compile_and_run(&c_src, r#"  printf("%u\n", (unsigned)xor_bytes(0xABu, 0x0Fu));"#);
    assert_eq!(output.trim(), format!("{}", 0xABu8 ^ 0x0Fu8));
}
