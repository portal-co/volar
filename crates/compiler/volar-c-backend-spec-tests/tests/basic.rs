// @reliability: normal
//! Integration tests: emit C, compile with `cc`, run, check stdout.

use volar_c_backend::CBackend;
use volar_lir::{BranchTarget, IcmpPred, LirTarget, LirType};
use volar_lir_test_corpus::compile_and_run;

/// Construct a fresh `IrExpr` with no provenance/side — test fixtures here
/// have no real source to attribute.
fn ir_expr(kind: volar_compiler::ir::IrExprKind) -> volar_compiler::ir::IrExpr {
    volar_compiler::ir::IrExpr::new(kind, (), None)
}

/// Construct a fresh `IrStmt` with no provenance/side.
fn ir_stmt(kind: volar_compiler::ir::IrStmtKind) -> volar_compiler::ir::IrStmt {
    volar_compiler::ir::IrStmt::new(kind, (), None)
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
    let output = compile_and_run(&c_src, r#"  printf("%u\n", add_two(3u, 4u));"#);
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
    let n_init = entry_params[0][0];
    let acc_init = entry_params[1][0];

    let loop_block = b.create_block();
    let counter = b.add_block_param(loop_block, LirType::U64);
    let accum = b.add_block_param(loop_block, LirType::U64);

    let done_block = b.create_block();
    let done_result = b.add_block_param(done_block, LirType::U64);

    b.switch_to_block(entry);
    b.jump(loop_block, BranchTarget::args([n_init, acc_init]));

    b.switch_to_block(loop_block);
    let zero = b.iconst(LirType::U64, 0);
    let cond = b.icmp(IcmpPred::Eq, counter, zero);
    let new_acc = b.add(accum, counter);
    let one = b.iconst(LirType::U64, 1);
    let new_ctr = b.sub(counter, one);
    b.branch(
        cond,
        done_block,
        BranchTarget::args([accum]),
        loop_block,
        BranchTarget::args([new_ctr, new_acc]),
    );

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
        ExternalKind, IrBlock, IrExprKind, IrFunction, IrParam, IrType, PrimitiveType, SpecBinOp,
    };
    use volar_lir_codegen::lower_function;

    let func = IrFunction {
        no_inline: false,
        name: "ir_max".to_owned(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![
            IrParam {
                name: "a".to_owned(),
                ty: IrType::Primitive(PrimitiveType::U32),
            },
            IrParam {
                name: "b".to_owned(),
                ty: IrType::Primitive(PrimitiveType::U32),
            },
        ],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(ir_expr(IrExprKind::If {
                cond: Box::new(ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::Gt,
                    left: Box::new(ir_expr(IrExprKind::Var("a".to_owned()))),
                    right: Box::new(ir_expr(IrExprKind::Var("b".to_owned()))),
                })),
                then_branch: IrBlock {
                    stmts: vec![],
                    expr: Some(Box::new(ir_expr(IrExprKind::Var("a".to_owned())))),
                },
                else_branch: Some(Box::new(ir_expr(IrExprKind::Var("b".to_owned())))),
            }))),
        },
        external_kind: ExternalKind::Normal,
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
// ir_expr(IrExprKind::Index) generates a select mux tree (all compile-time indices here).
// ============================================================================

#[test]
fn test_array_splat() {
    use volar_compiler::ir::{
        ArrayKind, ArrayLength, ExternalKind, IrBlock, IrExprKind, IrFunction, IrLit, IrModule,
        IrParam, IrType, PrimitiveType, SpecBinOp,
    };
    use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};

    // fn arr_sum(a: [u8; 4]) -> u32 {
    //     (a[0] as u32) + (a[1] as u32) + (a[2] as u32) + (a[3] as u32)
    // }
    let arr_ty = IrType::Array {
        kind: ArrayKind::FixedArray,
        elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
        len: ArrayLength::Const(4),
    };
    let cast_index = |i: u64| {
        ir_expr(IrExprKind::Cast {
            expr: Box::new(ir_expr(IrExprKind::Index {
                base: Box::new(ir_expr(IrExprKind::Var("a".to_owned()))),
                index: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(i.into())))),
            })),
            ty: Box::new(IrType::Primitive(PrimitiveType::U32)),
        })
    };
    let body_expr = ir_expr(IrExprKind::Binary {
        op: SpecBinOp::Add,
        left: Box::new(ir_expr(IrExprKind::Binary {
            op: SpecBinOp::Add,
            left: Box::new(ir_expr(IrExprKind::Binary {
                op: SpecBinOp::Add,
                left: Box::new(cast_index(0)),
                right: Box::new(cast_index(1)),
            })),
            right: Box::new(cast_index(2)),
        })),
        right: Box::new(cast_index(3)),
    });

    let func = IrFunction {
        no_inline: false,
        name: "arr_sum".to_owned(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![IrParam {
            name: "a".to_owned(),
            ty: arr_ty,
        }],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(body_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        consts: vec![],
        functions: vec![func],
        type_aliases: vec![],
    };

    let env = MonoEnv::new("sha256");

    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
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
        ExternalKind, IrBlock, IrExprKind, IrField, IrFunction, IrModule, IrParam, IrStruct,
        IrType, PrimitiveType, SpecBinOp, StructKind,
    };
    use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};

    // struct Point { x: u32, y: u32 }
    let point_struct = IrStruct {
        kind: StructKind::Custom("Point".to_owned()),
        module_path: vec![],
        generics: vec![],
        fields: vec![
            IrField {
                name: "x".to_owned(),
                ty: IrType::Primitive(PrimitiveType::U32),
                public: true,
            },
            IrField {
                name: "y".to_owned(),
                ty: IrType::Primitive(PrimitiveType::U32),
                public: true,
            },
        ],
        is_tuple: false,
        native_volar_type: None,
        derives: vec![],
    };

    let point_ty = IrType::Struct {
        kind: StructKind::Custom("Point".to_owned()),
        type_args: vec![],
    };

    // fn manhattan(p: Point) -> u32 { p.x + p.y }
    let func = IrFunction {
        no_inline: false,
        name: "manhattan".to_owned(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![IrParam {
            name: "p".to_owned(),
            ty: point_ty,
        }],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(ir_expr(IrExprKind::Binary {
                op: SpecBinOp::Add,
                left: Box::new(ir_expr(IrExprKind::Field {
                    base: Box::new(ir_expr(IrExprKind::Var("p".to_owned()))),
                    field: "x".to_owned(),
                })),
                right: Box::new(ir_expr(IrExprKind::Field {
                    base: Box::new(ir_expr(IrExprKind::Var("p".to_owned()))),
                    field: "y".to_owned(),
                })),
            }))),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![point_struct],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        consts: vec![],
        functions: vec![func],
        type_aliases: vec![],
    };

    let env = MonoEnv::new("sha256");

    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
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
        ExternalKind, IrBlock, IrExprKind, IrFunction, IrModule, IrParam, IrType, PrimitiveType,
        SpecBinOp,
    };
    use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};

    // fn xor_bytes(x: u8, y: u8) -> u8 { x ^ y }
    let func = IrFunction {
        no_inline: false,
        name: "xor_bytes".to_owned(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![
            IrParam {
                name: "x".to_owned(),
                ty: IrType::Primitive(PrimitiveType::U8),
            },
            IrParam {
                name: "y".to_owned(),
                ty: IrType::Primitive(PrimitiveType::U8),
            },
        ],
        return_type: Some(IrType::Primitive(PrimitiveType::U8)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(ir_expr(IrExprKind::Binary {
                op: SpecBinOp::BitXor,
                left: Box::new(ir_expr(IrExprKind::Var("x".to_owned()))),
                right: Box::new(ir_expr(IrExprKind::Var("y".to_owned()))),
            }))),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "test".to_owned(),
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        consts: vec![],
        functions: vec![func],
        type_aliases: vec![],
    };

    let env = MonoEnv::new("sha256");

    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &env);
    let c_src = b.finish();

    let output = compile_and_run(
        &c_src,
        r#"  printf("%u\n", (unsigned)xor_bytes(0xABu, 0x0Fu));"#,
    );
    assert_eq!(output.trim(), format!("{}", 0xABu8 ^ 0x0Fu8));
}

// ============================================================================
// LirAbi tests
// ============================================================================

#[test]
fn test_c_backend_abi_is_c_native() {
    use volar_lir::LirAbi;
    let b = CBackend::new();
    let abi = b.abi();
    assert!(
        abi.native_aggregates,
        "CBackend must report native_aggregates = true"
    );
    assert_eq!(
        abi.aggregate_byval_limit,
        LirAbi::C_NATIVE.aggregate_byval_limit
    );
}

#[test]
fn test_c_backend_pass_by_ptr_threshold() {
    let b = CBackend::new();
    let abi = b.abi();
    // Below threshold: inline
    assert!(!abi.pass_by_ptr(64));
    // Above threshold: by pointer
    assert!(abi.pass_by_ptr(65));
}

// ============================================================================
// Tuple pattern destructuring
// ============================================================================

/// Exercises `IrPattern::Tuple` in `bind_pattern`:
/// build a module with two functions — `make_pair() -> (u32, u32)` and
/// `sum_pair() -> u32` which calls make_pair and destructures the result.
#[test]
fn test_tuple_pattern_destructuring() {
    use volar_compiler::ir::{
        ExternalKind, IrBlock, IrExprKind, IrFunction, IrLit, IrModule, IrPattern, IrStmtKind,
        IrType, PrimitiveType, SpecBinOp,
    };
    use volar_lir_codegen::{lower_module_with_opts, mono::MonoEnv};

    // fn make_pair() -> (u32, u32) { (3, 7) }
    let make_pair = IrFunction {
        no_inline: false,
        name: "make_pair".into(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![],
        return_type: Some(IrType::Tuple(vec![
            IrType::Primitive(PrimitiveType::U32),
            IrType::Primitive(PrimitiveType::U32),
        ])),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(ir_expr(IrExprKind::Tuple(vec![
                ir_expr(IrExprKind::Lit(IrLit::Int(3u64.into()))),
                ir_expr(IrExprKind::Lit(IrLit::Int(7u64.into()))),
            ])))),
        },
        external_kind: ExternalKind::Normal,
    };

    // fn sum_pair() -> u32 {
    //     let (a, b) = make_pair();
    //     a + b
    // }
    let sum_pair = IrFunction {
        no_inline: false,
        name: "sum_pair".into(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::Tuple(vec![
                    IrPattern::Ident {
                        mutable: false,
                        name: "a".into(),
                        subpat: None,
                    },
                    IrPattern::Ident {
                        mutable: false,
                        name: "b".into(),
                        subpat: None,
                    },
                ]),
                ty: Some(IrType::Tuple(vec![
                    IrType::Primitive(PrimitiveType::U32),
                    IrType::Primitive(PrimitiveType::U32),
                ])),
                init: Some(ir_expr(IrExprKind::Call {
                    func: Box::new(ir_expr(IrExprKind::Var("make_pair".into()))),
                    args: vec![],
                })),
            })],
            expr: Some(Box::new(ir_expr(IrExprKind::Binary {
                op: SpecBinOp::Add,
                left: Box::new(ir_expr(IrExprKind::Var("a".into()))),
                right: Box::new(ir_expr(IrExprKind::Var("b".into()))),
            }))),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "test".into(),
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        consts: vec![],
        type_aliases: vec![],
        functions: vec![make_pair, sum_pair],
    };

    let mut b = CBackend::new();
    lower_module_with_opts(&module, &mut b, &MonoEnv::new(""));
    let c_src = b.finish();

    let out = compile_and_run(&c_src, r#"printf("%u\n", sum_pair());"#);
    assert_eq!(out.trim(), "10");
}

// ============================================================================
// NameConfig: prefix and per-name remap
// ============================================================================

#[test]
fn test_name_config_prefix() {
    use volar_c_backend::NameConfig;

    let mut b = CBackend::new().with_prefix("pfx_");

    // Emit a simple function: u32 add(u32 a, u32 b) { return a + b; }
    let (entry, pvs) = b.begin_function("add", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let sum = b.add(pvs[0][0], pvs[1][0]);
    b.ret(&[sum]);
    b.end_function();

    let src = b.finish();
    assert!(
        src.contains("pfx_add"),
        "expected prefixed function name 'pfx_add' in:\n{src}"
    );
    assert!(
        !src.contains("\nadd(") && !src.contains(" add("),
        "un-prefixed 'add(' should not appear in:\n{src}"
    );
}

#[test]
fn test_name_config_remap() {
    use std::collections::BTreeMap;
    use volar_c_backend::NameConfig;

    let mut remap = BTreeMap::new();
    remap.insert("add".to_string(), "vector_add".to_string());
    let cfg = NameConfig {
        prefix: "pfx_".to_string(),
        remap,
    };

    let mut b = CBackend::new().with_name_config(cfg);

    let (entry, pvs) = b.begin_function("add", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry);
    let sum = b.add(pvs[0][0], pvs[1][0]);
    b.ret(&[sum]);
    b.end_function();

    // Also emit a second function that is NOT remapped to check prefix still applies.
    let (entry2, pvs2) = b.begin_function("sub", &[LirType::U32, LirType::U32], Some(LirType::U32));
    b.switch_to_block(entry2);
    let diff = b.sub(pvs2[0][0], pvs2[1][0]);
    b.ret(&[diff]);
    b.end_function();

    let src = b.finish();
    // Remap: "add" → "vector_add" (prefix not applied)
    assert!(
        src.contains("vector_add"),
        "expected remapped name 'vector_add' in:\n{src}"
    );
    // Non-remapped "sub" gets the prefix.
    assert!(
        src.contains("pfx_sub"),
        "expected prefixed name 'pfx_sub' in:\n{src}"
    );
}

// ============================================================================
// Corpus: smoke test (all cases build without panic)
// ============================================================================

#[test]
fn test_corpus_smoke() {
    volar_lir_test_corpus::for_each_build!(CBackend::new());
}

// ============================================================================
// Corpus: e2e I/O verification (build → C → compile → run → check)
// ============================================================================

#[test]
fn test_corpus_e2e() {
    use volar_lir_test_corpus::ALL_CASES;
    use volar_lir_test_corpus::generated::*;

    macro_rules! run_case {
        ($build_fn:ident) => {{
            let mut b = CBackend::new();
            $build_fn(&mut b);
            let c_src = b.finish();
            let case_name = stringify!($build_fn)
                .strip_prefix("build_")
                .unwrap_or(stringify!($build_fn));
            let case = ALL_CASES
                .iter()
                .find(|c| c.name == case_name)
                .unwrap_or_else(|| panic!("case not found: {case_name}"));
            for io in case.ios {
                let body = case.c_main_body(io);
                let out = compile_and_run(&c_src, &body);
                let actual: u64 = out
                    .trim()
                    .parse()
                    .unwrap_or_else(|_| panic!("{case_name}: parse error: {out:?}"));
                assert_eq!(
                    actual, io.expected,
                    "{case_name}({:?}): expected {}, got {actual}",
                    io.inputs, io.expected
                );
            }
        }};
    }

    run_case!(build_const_u32);
    run_case!(build_add_u32);
    run_case!(build_sub_u32);
    run_case!(build_mul_u32);
    run_case!(build_udiv_u32);
    run_case!(build_and_u64);
    run_case!(build_or_u64);
    run_case!(build_xor_u64);
    run_case!(build_not_bool);
    run_case!(build_shl_u32);
    run_case!(build_lshr_u32);
    run_case!(build_icmp_eq_u32);
    run_case!(build_icmp_ult_u32);
    run_case!(build_zext_u8_to_u32);
    run_case!(build_trunc_u32_to_u8);
    run_case!(build_select_u32);
    run_case!(build_branch_merge_u32);
    run_case!(build_loop_sum_u32);
}

// ============================================================================
// Native loop lowering (docs/lir-native-loops-plan.md, Stage 1)
//
// fn sum_to(n: u64) -> u64 {
//     let mut acc = 0u64;
//     for i in 0..n { acc = acc + (i as u64); }
//     acc
// }
// ============================================================================

#[test]
fn test_native_loop_sum_to() {
    use volar_compiler::ir::{
        ExternalKind, IrBlock, IrExprKind, IrFunction, IrLit, IrParam, IrPattern, IrStmtKind,
        IrType, PrimitiveType, SpecBinOp,
    };
    use volar_lir_codegen::{LoopLowering, lower_function_with_loop_lowering};

    let build = || IrFunction {
        no_inline: false,
        name: "sum_to".to_owned(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![IrParam {
            name: "n".to_owned(),
            ty: IrType::Primitive(PrimitiveType::U64),
        }],
        return_type: Some(IrType::Primitive(PrimitiveType::U64)),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![
                ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::Ident {
                        mutable: true,
                        name: "acc".to_owned(),
                        subpat: None,
                    },
                    ty: Some(IrType::Primitive(PrimitiveType::U64)),
                    init: Some(ir_expr(IrExprKind::Lit(IrLit::Int(0)))),
                }),
                ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::BoundedLoop {
                    var: "i".to_owned(),
                    start: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(0)))),
                    end: Box::new(ir_expr(IrExprKind::Var("n".to_owned()))),
                    inclusive: false,
                    body: IrBlock {
                        stmts: vec![],
                        expr: Some(Box::new(ir_expr(IrExprKind::Assign {
                            left: Box::new(ir_expr(IrExprKind::Var("acc".to_owned()))),
                            right: Box::new(ir_expr(IrExprKind::Binary {
                                op: SpecBinOp::Add,
                                left: Box::new(ir_expr(IrExprKind::Var("acc".to_owned()))),
                                right: Box::new(ir_expr(IrExprKind::Cast {
                                    expr: Box::new(ir_expr(IrExprKind::Var("i".to_owned()))),
                                    ty: Box::new(IrType::Primitive(PrimitiveType::U64)),
                                })),
                            })),
                        }))),
                    },
                }))),
            ],
            expr: Some(Box::new(ir_expr(IrExprKind::Var("acc".to_owned())))),
        },
        external_kind: ExternalKind::Normal,
    };

    // Native mode: the loop lowers to a CFG back-edge (block params carry the
    // loop variable and the loop-carried accumulator), not unrolled code.
    {
        let mut b = CBackend::new();
        lower_function_with_loop_lowering(&build(), &mut b, LoopLowering::Native);
        let c_src = b.finish();
        std::fs::write("/tmp/test_c_src.c", &c_src).unwrap();
        if std::env::var("VOLAR_DUMP_NATIVE_C").is_ok() { eprintln!("{c_src}"); }
        assert!(
            c_src.contains("goto block"),
            "native mode must emit a CFG back-edge, got straight-line code"
        );
        let output = compile_and_run(
            &c_src,
            r#"  printf("%llu\n", (unsigned long long)sum_to(10ull));"#,
        );
        assert_eq!(output.trim(), "45");
    }

    // Unroll mode on the same loop shape with a concrete bound: the unrolled
    // path must produce the same result as the native CFG loop (dual-path
    // parity). (With a dynamic bound, `Unroll` selects the legacy skeleton,
    // whose discarded-aggregate/loop-carried writes are documented as
    // unsound — it is not a parity baseline.)
    {
        let mut func = build();
        let loop_body = match &func.body.stmts[1].kind {
            IrStmtKind::Semi(e) => match &e.kind {
                IrExprKind::BoundedLoop { body, .. } => body.clone(),
                other => panic!("unexpected stmt kind {other:?}"),
            },
            other => panic!("unexpected stmt kind {other:?}"),
        };
        func.body.stmts[1] = ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::BoundedLoop {
            var: "i".to_owned(),
            start: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(0)))),
            end: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(10)))),
            inclusive: false,
            body: loop_body,
        })));
        let mut b = CBackend::new();
        lower_function_with_loop_lowering(&func, &mut b, LoopLowering::Unroll);
        let c_src = b.finish();
        let output = compile_and_run(
            &c_src,
            r#"  printf("%llu\n", (unsigned long long)sum_to(10ull));"#,
        );
        assert_eq!(output.trim(), "45");
    }
}

// ============================================================================
// Native loop with concrete bounds still lowers to a CFG loop under Native
// mode (unrolling is opt-out, not implicit), and an aggregate-assign body is
// rejected from the native path and falls back to unrolling.
// ============================================================================

#[test]
fn test_native_loop_concrete_bounds_and_aggregate_fallback() {
    use volar_compiler::ir::{
        ArrayKind, ArrayLength, ExternalKind, IrBlock, IrExprKind, IrFunction, IrLit, IrParam,
        IrPattern, IrStmtKind, IrType, PrimitiveType, SpecBinOp,
    };
    use volar_lir_codegen::{LoopLowering, lower_function_with_loop_lowering};

    // fn fill4() -> [u64; 4] {
    //     let mut arr = [0u64; 4];
    //     for i in 0..4 { arr[i] = (i as u64) + 1; }
    //     arr
    // }
    let arr_ty = IrType::Array {
        kind: ArrayKind::FixedArray,
        elem: Box::new(IrType::Primitive(PrimitiveType::U64)),
        len: ArrayLength::Const(4),
    };
    let build = || IrFunction {
        no_inline: false,
        name: "fill4".to_owned(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![],
        return_type: Some(arr_ty.clone()),
        where_clause: vec![],
        body: IrBlock {
            stmts: vec![
                ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::Ident {
                        mutable: true,
                        name: "arr".to_owned(),
                        subpat: None,
                    },
                    ty: Some(arr_ty.clone()),
                    init: Some(ir_expr(IrExprKind::FixedArray(vec![
                        ir_expr(IrExprKind::Lit(IrLit::Int(0))),
                        ir_expr(IrExprKind::Lit(IrLit::Int(0))),
                        ir_expr(IrExprKind::Lit(IrLit::Int(0))),
                        ir_expr(IrExprKind::Lit(IrLit::Int(0))),
                    ]))),
                }),
                ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::BoundedLoop {
                    var: "i".to_owned(),
                    start: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(0)))),
                    end: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(4)))),
                    inclusive: false,
                    body: IrBlock {
                        stmts: vec![],
                        expr: Some(Box::new(ir_expr(IrExprKind::Assign {
                            left: Box::new(ir_expr(IrExprKind::Index {
                                base: Box::new(ir_expr(IrExprKind::Var("arr".to_owned()))),
                                index: Box::new(ir_expr(IrExprKind::Var("i".to_owned()))),
                            })),
                            right: Box::new(ir_expr(IrExprKind::Binary {
                                op: SpecBinOp::Add,
                                left: Box::new(ir_expr(IrExprKind::Cast {
                                    expr: Box::new(ir_expr(IrExprKind::Var("i".to_owned()))),
                                    ty: Box::new(IrType::Primitive(PrimitiveType::U64)),
                                })),
                                right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(1)))),
                            })),
                        }))),
                    },
                }))),
            ],
            expr: Some(Box::new(ir_expr(IrExprKind::Var("arr".to_owned())))),
        },
        external_kind: ExternalKind::Normal,
    };

    // The aggregate (`arr[i] = …`) assignment makes the body native-ineligible
    // (Stage 1); the loop must fall back to concrete unrolling and still
    // produce the right answer.
    let mut b = CBackend::new();
    lower_function_with_loop_lowering(&build(), &mut b, LoopLowering::Native);
    let c_src = b.finish();
    assert!(
        !c_src.contains("goto block"),
        "aggregate-assign loop must fall back to unrolled straight-line code"
    );
    std::fs::write("/tmp/fill4.c", &c_src).unwrap();
    let output = compile_and_run(
        &c_src,
        r#"
  Arr_U64_4 r = fill4();
  printf("%llu %llu %llu %llu\n", (unsigned long long)r.data[0], (unsigned long long)r.data[1], (unsigned long long)r.data[2], (unsigned long long)r.data[3]);
"#,
    );
    assert_eq!(output.trim(), "1 2 3 4");
}
