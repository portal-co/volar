// @reliability: experimental
//! Isolated test for the `Box<[T; N]>` heap-pool abstraction
//! (`volar_compiler::ir::box_type`/`box_new_array_expr`,
//! `volar_lir::HeapAllocExt`, and their `volar-lir-codegen`/`CBackend`
//! wiring) — independent of `volar-weaver`'s own (much larger, pooling-
//! heavy) VOLE circuits, so a real regression here points straight at this
//! mechanism rather than getting lost in a huge generated function.
//!
//! Builds one hand-constructed `IrFunction` exercising exactly what a pool
//! declaration needs: a statically-sized, heap-backed, default-initialized
//! array (`let mut _pool: Box<[u32; 4]> = Box::new([0u32; 4]);`), two
//! indexed writes, and an indexed-read-based return value — then lowers it
//! through the real `lower_module_monomorphized` + `CBackend` pipeline,
//! compiles the result with a system C compiler, and checks the actual
//! numeric output.

use volar_c_backend::CBackend;
use volar_compiler::ir::{
    ExternalKind, IrBlock, IrExpr, IrExprKind, IrFunction, IrLit, IrModule, IrPattern, IrStmt,
    IrStmtKind, IrType, PrimitiveType,
};
use volar_lir_codegen::{lower_module_monomorphized, mono::MonoEnv, MonoPlanOptions, MonoRoot};
use volar_lir_test_corpus::compile_and_run;

fn ir_expr(kind: IrExprKind) -> IrExpr {
    IrExpr::new(kind, (), None)
}

fn ir_stmt(kind: IrStmtKind) -> IrStmt {
    IrStmt::new(kind, (), None)
}

fn var(name: &str) -> IrExpr {
    ir_expr(IrExprKind::Var(name.into()))
}

fn u32_lit(n: i128) -> IrExpr {
    ir_expr(IrExprKind::Lit(IrLit::Int(n)))
}

fn arr_index(base: &str, idx: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::Index {
        base: Box::new(var(base)),
        index: Box::new(idx),
    })
}

/// Builds:
/// ```text
/// fn pool_test() -> u32 {
///     let mut _pool: Box<[u32; 4]> = Box::new([0u32; 4]);
///     _pool[1] = 42;
///     _pool[2] = 7;
///     _pool[1] + _pool[2]
/// }
/// ```
fn build_pool_test_fn() -> IrFunction {
    let elem_ty = IrType::Primitive(PrimitiveType::U32);
    let box_array_ty = volar_compiler::ir::box_type(IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray,
        elem: Box::new(elem_ty.clone()),
        len: volar_compiler::ir::ArrayLength::Const(4),
    });
    let init = volar_compiler::ir::box_new_array_expr(elem_ty, u32_lit(0), 4);

    let stmts = vec![
        ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident("_pool").as_mut(),
            ty: Some(box_array_ty),
            init: Some(init),
        }),
        ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
            left: Box::new(arr_index("_pool", u32_lit(1))),
            right: Box::new(u32_lit(42)),
        }))),
        ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
            left: Box::new(arr_index("_pool", u32_lit(2))),
            right: Box::new(u32_lit(7)),
        }))),
    ];
    let tail = ir_expr(IrExprKind::Binary {
        op: volar_compiler::ir::SpecBinOp::Add,
        left: Box::new(arr_index("_pool", u32_lit(1))),
        right: Box::new(arr_index("_pool", u32_lit(2))),
    });

    IrFunction {
        name: "pool_test".into(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params: vec![],
        return_type: Some(IrType::Primitive(PrimitiveType::U32)),
        where_clause: vec![],
        body: IrBlock { stmts, expr: Some(Box::new(tail)) },
        external_kind: ExternalKind::Normal,
        no_inline: false,
    }
}

#[test]
fn box_pool_indexed_write_and_read_round_trips_through_c() {
    let module = IrModule {
        name: "box_pool_test".into(),
        functions: vec![build_pool_test_fn()],
        ..Default::default()
    };

    let env = MonoEnv::new("box_pool_test");
    let roots = vec![MonoRoot::new("pool_test", env)];

    let mut backend = CBackend::new();
    lower_module_monomorphized(&module, &mut backend, MonoPlanOptions { roots, ..Default::default() })
        .unwrap_or_else(|e| panic!("LIR monomorphization failed: {e}"));
    let c_src = backend.finish();
    assert!(!c_src.is_empty());
    assert!(c_src.contains("calloc"), "expected a calloc-backed heap allocation in generated C:\n{c_src}");

    let output = compile_and_run(&c_src, r#"  printf("%u\n", pool_test());"#);
    assert_eq!(output.trim(), "49", "expected _pool[1] (42) + _pool[2] (7) == 49\ngenerated C:\n{c_src}");
}
