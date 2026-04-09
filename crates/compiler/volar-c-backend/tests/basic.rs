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
