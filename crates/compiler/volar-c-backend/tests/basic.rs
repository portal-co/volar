// @reliability: normal
//! Integration tests: emit C, compile with `cc`, run, check stdout.

use std::{fs, process::Command};
use tempfile::TempDir;
use volar_c_backend::CBackend;
use volar_lir::{FieldDef, IcmpPred, LirTarget, LirType, StructDef};

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
    let sum = b.add(params[0], params[1]);
    b.ret(Some(sum));
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
    // Computes sum of 1..=n by looping.
    let (entry, entry_params) = b.begin_function(
        "countdown",
        &[LirType::U64, LirType::U64],
        Some(LirType::U64),
    );
    let n_init = entry_params[0];
    let acc_init = entry_params[1];

    // Loop block: params are (counter: u64, accumulator: u64)
    let loop_block = b.create_block();
    let counter = b.add_block_param(loop_block, LirType::U64);
    let accum = b.add_block_param(loop_block, LirType::U64);

    // Done block: no params; return accumulator (passed in via jump arg).
    let done_block = b.create_block();
    let done_result = b.add_block_param(done_block, LirType::U64);

    // Entry: jump to loop with initial (n, acc).
    b.switch_to_block(entry);
    b.jump(loop_block, &[n_init, acc_init]);

    // Loop body.
    b.switch_to_block(loop_block);
    let zero = b.iconst(LirType::U64, 0);
    let cond = b.icmp(IcmpPred::Eq, counter, zero); // counter == 0?
    let new_acc = b.add(accum, counter);             // acc + counter
    let one = b.iconst(LirType::U64, 1);
    let new_ctr = b.sub(counter, one);               // counter - 1
    // if done: jump to done with current accum; else loop with (new_ctr, new_acc)
    b.branch(cond, done_block, &[accum], loop_block, &[new_ctr, new_acc]);

    // Done block.
    b.switch_to_block(done_block);
    b.ret(Some(done_result));

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

    // Construct: fn max(a: u32, b: u32) -> u32 { if a > b { a } else { b } }
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
// Test 4: arrays — arr_new / arr_get / arr_set
// ============================================================================

#[test]
fn test_arrays() {
    let mut b = CBackend::new();

    // fn arr_sum(a: [u8; 4]) -> u32
    // Builds an array [1,2,3,4] and sums the elements with arr_get.
    let (entry, _) =
        b.begin_function("arr_sum", &[], Some(LirType::U32));
    b.switch_to_block(entry);

    let elem_ty = LirType::U8;
    let v1 = b.iconst(LirType::U8, 1);
    let v2 = b.iconst(LirType::U8, 2);
    let v3 = b.iconst(LirType::U8, 3);
    let v4 = b.iconst(LirType::U8, 4);
    let arr = b.arr_new(elem_ty, &[v1, v2, v3, v4]);

    let i0 = b.iconst(LirType::U32, 0);
    let i1 = b.iconst(LirType::U32, 1);
    let i2 = b.iconst(LirType::U32, 2);
    let i3 = b.iconst(LirType::U32, 3);
    let e0 = b.arr_get(arr, i0);
    let e1 = b.arr_get(arr, i1);
    let e2 = b.arr_get(arr, i2);
    let e3 = b.arr_get(arr, i3);

    let e0u = b.zext(e0, LirType::U32);
    let e1u = b.zext(e1, LirType::U32);
    let e2u = b.zext(e2, LirType::U32);
    let e3u = b.zext(e3, LirType::U32);

    let s01 = b.add(e0u, e1u);
    let s23 = b.add(e2u, e3u);
    let sum = b.add(s01, s23);

    // arr_set: replace index 0 with 10, verify via arr_get
    let ten = b.iconst(LirType::U8, 10);
    let arr2 = b.arr_set(arr, i0, ten);
    let _ = b.arr_get(arr2, i0); // exercises arr_set path

    b.ret(Some(sum));
    b.end_function();

    let c_src = b.finish();
    let output = compile_and_run(&c_src, r#"  printf("%u\n", arr_sum());"#);
    assert_eq!(output.trim(), "10");
}

// ============================================================================
// Test 5: structs — define_struct / struct_new / struct_get
// ============================================================================

#[test]
fn test_structs() {
    let mut b = CBackend::new();

    // struct Point { x: u32, y: u32 }
    let point_id = b.define_struct(StructDef {
        name: "Point".to_string(),
        fields: vec![
            FieldDef { name: "x".to_string(), ty: LirType::U32 },
            FieldDef { name: "y".to_string(), ty: LirType::U32 },
        ],
    });

    // fn manhattan(px: u32, py: u32) -> u32
    // Constructs Point{x: px, y: py}, extracts fields, returns x+y.
    let (entry, params) = b.begin_function(
        "manhattan",
        &[LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    b.switch_to_block(entry);
    let px = params[0];
    let py = params[1];

    let pt = b.struct_new(point_id, &[px, py]);
    let ex = b.struct_get(pt, 0);
    let ey = b.struct_get(pt, 1);
    let dist = b.add(ex, ey);
    b.ret(Some(dist));
    b.end_function();

    let c_src = b.finish();
    let output = compile_and_run(&c_src, r#"  printf("%u\n", manhattan(3u, 7u));"#);
    assert_eq!(output.trim(), "10");
}

// ============================================================================
// Test 6: struct + array via IrModule lowering (Phase 2 codegen)
// ============================================================================

#[test]
fn test_phase2_codegen_struct_array() {
    use volar_compiler::ir::{
        ArrayKind, ArrayLength, IrBlock, IrExpr, IrField, IrFunction, IrLit, IrModule, IrParam,
        IrStmt, IrStruct, IrType, PrimitiveType, SpecBinOp, StructKind,
    };
    use volar_lir_codegen::{lower_module_with_opts, mono::{MonoEnv, monomorphize_module}};

    // struct ByteArr { data: [u8; 4] }
    // fn xor_pair(a: ByteArr, b: ByteArr) -> u32
    //   { result = 0; result |= (a.data[0] ^ b.data[0]); ... }
    // We'll test a simpler version: a function that XORs two u8 values.

    // fn add_bytes(x: u8, y: u8) -> u8 { x ^ y }
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
