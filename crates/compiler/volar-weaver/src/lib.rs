// @reliability: normal
//! @ai: assisted
//! IR-to-protected pass: weaves a boolean circuit into garbled-circuit spec calls.
//!
//! Takes a single-block `BIrBlocks` circuit and produces an `IrModule` where each
//! gate is replaced by a call to the corresponding `volar-spec` garble operation.
//! The output can be rendered to Rust or TypeScript via the existing printer machinery.
//!
//! Two variants are generated:
//! - **Evaluator** (`weave_evaluator`): evaluates the garbled circuit given precomputed tables.
//! - **Garbler** (`weave_garbler`): generates the AND-gate tables from wire labels.
#![no_std]
extern crate alloc;

use alloc::{
    collections::BTreeMap,
    format,
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        IrBlock, IrClosureParam, IrExpr, IrFunction, IrGenericParam,
        IrGenericParamKind, IrLit, IrModule, IrParam, IrPattern, IrStmt, IrTraitBound, IrType,
        MethodKind, SpecBinOp, SpecUnaryOp, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
#[cfg(feature = "linking")]
use volar_compiler::linkage::LinkedSpec;
use volar_ir::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTerminator},
    ir::{IRBlockTargetId, IRVarId},
    lower_to_circuit::lower_to_circuit,
};
pub use volar_ir::lower_to_circuit::LoweringMode;

// ============================================================================
// Type helpers
// ============================================================================

fn eval_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Eval".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

fn garble_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Garble".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

fn garble_table_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("GarbleTable".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

fn ref_to(ty: IrType) -> IrType {
    IrType::Reference {
        mutable: false,
        elem: alloc::boxed::Box::new(ty),
    }
}

/// Generic params `<N: ArraySize, D: Digest>`.
fn generic_params() -> Vec<IrGenericParam> {
    vec![
        IrGenericParam {
            name: "N".into(),
            kind: IrGenericParamKind::Type,
            bounds: vec![IrTraitBound {
                trait_kind: TraitKind::Custom("ArraySize".into()),
                type_args: vec![],
                assoc_bindings: vec![],
            }],
            default: None,
        },
        IrGenericParam {
            name: "D".into(),
            kind: IrGenericParamKind::Type,
            bounds: vec![IrTraitBound {
                trait_kind: TraitKind::Custom("Digest".into()),
                type_args: vec![],
                assoc_bindings: vec![],
            }],
            default: None,
        },
    ]
}

// ============================================================================
// Expression helpers
// ============================================================================

/// `expr.clone()`
fn clone_expr(expr: IrExpr) -> IrExpr {
    IrExpr::MethodCall {
        receiver: alloc::boxed::Box::new(expr),
        method: MethodKind::Std("clone".into()),
        type_args: vec![],
        args: vec![],
    }
}

/// `&expr`
fn ref_expr(expr: IrExpr) -> IrExpr {
    IrExpr::Unary {
        op: SpecUnaryOp::Ref,
        expr: alloc::boxed::Box::new(expr),
    }
}

/// Variable reference by name.
fn var(name: &str) -> IrExpr {
    IrExpr::Var(name.into())
}

/// `Array::<u8, N>::default()`
fn array_default() -> IrExpr {
    IrExpr::Call {
        func: alloc::boxed::Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "default".into()],
            type_args: vec![
                IrType::Primitive(volar_compiler::ir::PrimitiveType::U8),
                IrType::TypeParam("N".into()),
            ],
        }),
        args: vec![],
    }
}

/// `Array::<u8, N>::from_fn(|{idx}| {body})`
fn array_from_fn(idx: &str, body: IrExpr) -> IrExpr {
    IrExpr::Call {
        func: alloc::boxed::Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "from_fn".into()],
            type_args: vec![
                IrType::Primitive(volar_compiler::ir::PrimitiveType::U8),
                IrType::TypeParam("N".into()),
            ],
        }),
        args: vec![IrExpr::Closure {
            params: vec![IrClosureParam {
                pattern: IrPattern::ident(idx),
                ty: None,
            }],
            ret_type: None,
            body: alloc::boxed::Box::new(body),
        }],
    }
}

/// `wire.base[idx]`
fn base_index(wire_name: &str, idx_name: &str) -> IrExpr {
    IrExpr::Index {
        base: alloc::boxed::Box::new(IrExpr::Field {
            base: alloc::boxed::Box::new(var(wire_name)),
            field: "base".into(),
        }),
        index: alloc::boxed::Box::new(var(idx_name)),
    }
}

// ============================================================================
// Or-gate lowering
// ============================================================================

/// Expand `Or(a, b)` gates inline using De Morgan: `Not(And(Not(a), Not(b)))`.
///
/// Returns a flat list of `(result_var_id, stmt)` pairs with no `Or` gates.
/// Synthetic var IDs start above the existing SSA range.
fn expand_ors(block: &BIrBlock) -> Vec<(IRVarId, BIrStmt)> {
    let num_params = block.params as u32;
    let num_stmts = block.stmts.len() as u32;
    // Existing SSA range: 0..num_params (params) + num_params..num_params+num_stmts (stmts).
    let mut next_synthetic = num_params + num_stmts;
    let mut out: Vec<(IRVarId, BIrStmt)> = Vec::new();

    for (i, stmt) in block.stmts.iter().enumerate() {
        let result_id = IRVarId(num_params + i as u32);
        match stmt {
            BIrStmt::Or(a, b) => {
                let not_a = IRVarId(next_synthetic);
                next_synthetic += 1;
                let not_b = IRVarId(next_synthetic);
                next_synthetic += 1;
                let and_ab = IRVarId(next_synthetic);
                next_synthetic += 1;
                out.push((not_a.clone(), BIrStmt::Not(a.clone())));
                out.push((not_b.clone(), BIrStmt::Not(b.clone())));
                out.push((and_ab.clone(), BIrStmt::And(not_a, not_b)));
                // result = Not(And(Not(a), Not(b))) = De Morgan's law for Or
                out.push((result_id, BIrStmt::Not(and_ab)));
            }
            other => out.push((result_id, other.clone())),
        }
    }
    out
}

// ============================================================================
// Evaluator weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a garbled-circuit **evaluator** `IrModule`.
///
/// The generated function signature is:
/// ```text
/// fn <name><N: ArraySize, D: Digest>(
///     one_wire: &Eval<N>,           // label for constant 1
///     and_table_0: &GarbleTable<N>, // one per AND gate, in circuit order
///     ...
///     input_0: &Eval<N>,            // one per block parameter, in order
///     ...
/// ) -> Eval<N>
/// ```
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_evaluator(circuit: &BIrBlocks, name: &str, linkage: Option<&LinkageSystem>) -> IrModule {
    assert!(
        circuit.is_circuit(),
        "weave_evaluator: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;

    // Expand Or gates.
    let expanded = expand_ors(block);

    // Count AND gates to determine table parameter count.
    let and_count = expanded
        .iter()
        .filter(|(_, s)| matches!(s, BIrStmt::And(..)))
        .count();

    // Map from SSA var ID to the Rust variable name used in the generated function.
    let mut var_names: BTreeMap<u32, String> = BTreeMap::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    // Build parameter list: one_wire, then and_table_k, then input_i.
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "one_wire".into(),
        ty: ref_to(eval_type()),
    });
    for k in 0..and_count {
        params.push(IrParam {
            name: format!("and_table_{}", k),
            ty: ref_to(garble_table_type()),
        });
    }
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: ref_to(eval_type()),
        });
    }

    // Build the function body.
    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut and_counter: usize = 0;

    for (result_id, stmt) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        let init_expr = match stmt {
            BIrStmt::Zero => {
                // Eval { target: Array::<u8, N>::default() }
                IrExpr::StructExpr {
                    kind: StructKind::Custom("Eval".into()),
                    type_args: vec![],
                    fields: vec![("target".into(), array_default())],
                    rest: None,
                }
            }

            BIrStmt::One => {
                // one_wire.clone()
                clone_expr(var("one_wire"))
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                // wire_a.clone() ^ wire_b.clone()
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    right: alloc::boxed::Box::new(clone_expr(var(&name_b))),
                }
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_name = format!("and_table_{}", and_counter);
                and_counter += 1;
                // wire_a.clone().and_via_table::<D>(&wire_b.clone(), and_table_k)
                IrExpr::MethodCall {
                    receiver: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Std("and_via_table".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![
                        ref_expr(clone_expr(var(&name_b))),
                        var(&table_name),
                    ],
                }
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                // wire_a.clone() ^ one_wire.clone()  (free-XOR NOT)
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    right: alloc::boxed::Box::new(clone_expr(var("one_wire"))),
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        var_names.insert(result_id.0, let_name);
    }

    // Return expression from terminator.
    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let func = IrFunction {
        name: name.into(),
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(alloc::boxed::Box::new(ret_expr)),
        },
    };

    let mut module = IrModule {
        name: "weaved".into(),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// Garbler weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a garbled-circuit **garbler** `IrModule`.
///
/// The generated function signature is:
/// ```text
/// fn <name>_garble<N: ArraySize, D: Digest>(
///     secret: &GlobalSecret<N>,
///     input_0: &Garble<N>,   // garbler's false-label for input bit 0
///     ...
/// ) -> (Vec<GarbleTable<N>>, Garble<N>)
/// ```
///
/// The returned tuple is (all AND tables in circuit order, output wire false-label).
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_garbler(circuit: &BIrBlocks, name: &str, linkage: Option<&LinkageSystem>) -> IrModule {
    assert!(
        circuit.is_circuit(),
        "weave_garbler: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let mut var_names: BTreeMap<u32, String> = BTreeMap::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    // Parameters: secret, then input_i.
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "secret".into(),
        ty: ref_to(IrType::Struct {
            kind: StructKind::Custom("GlobalSecret".into()),
            type_args: vec![IrType::TypeParam("N".into())],
        }),
    });
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: ref_to(garble_type()),
        });
    }

    // Return type: (Vec<GarbleTable<N>>, Garble<N>)
    let ret_type = IrType::Tuple(vec![
        IrType::Vector {
            elem: alloc::boxed::Box::new(garble_table_type()),
        },
        garble_type(),
    ]);

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut table_counter: usize = 0;

    // `let mut tables: Vec<GarbleTable<N>> = Vec::new();`
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident {
            mutable: true,
            name: "tables".into(),
            subpat: None,
        },
        ty: Some(IrType::Vector {
            elem: alloc::boxed::Box::new(garble_table_type()),
        }),
        init: Some(IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["Vec".into(), "new".into()],
                type_args: vec![],
            }),
            args: vec![],
        }),
    });

    for (result_id, stmt) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        let garble_expr = match stmt {
            BIrStmt::Zero | BIrStmt::One => {
                // Garble { base: Array::<u8, N>::default() }
                garble_struct(array_default())
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                // Garble { base: Array::from_fn(|j| wire_a.base[j] ^ wire_b.base[j]) }
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: alloc::boxed::Box::new(base_index(&name_a, "j")),
                        right: alloc::boxed::Box::new(base_index(&name_b, "j")),
                    },
                ))
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                // NOT: flip the false-label by XOR-ing with the global secret.
                // Garble { base: Array::from_fn(|j| wire_a.base[j] ^ secret.secret()[j]) }
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: alloc::boxed::Box::new(base_index(&name_a, "j")),
                        right: alloc::boxed::Box::new(IrExpr::Index {
                            base: alloc::boxed::Box::new(IrExpr::MethodCall {
                                receiver: alloc::boxed::Box::new(var("secret")),
                                method: MethodKind::Std("secret".into()),
                                type_args: vec![],
                                args: vec![],
                            }),
                            index: alloc::boxed::Box::new(var("j")),
                        }),
                    },
                ))
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_var = format!("table_{}", table_counter);
                table_counter += 1;

                // `let table_k = secret.gen_and_table::<D>(&wire_a, &wire_b);`
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&table_var),
                    ty: None,
                    init: Some(IrExpr::MethodCall {
                        receiver: alloc::boxed::Box::new(var("secret")),
                        method: MethodKind::Std("gen_and_table".into()),
                        type_args: vec![IrType::TypeParam("D".into())],
                        args: vec![
                            ref_expr(clone_expr(var(&name_a))),
                            ref_expr(clone_expr(var(&name_b))),
                        ],
                    }),
                });

                // `tables.push(table_k);`
                stmts.push(IrStmt::Semi(IrExpr::MethodCall {
                    receiver: alloc::boxed::Box::new(var("tables")),
                    method: MethodKind::Std("push".into()),
                    type_args: vec![],
                    args: vec![var(&table_var)],
                }));

                // Result wire false-label: wire_a.and_result::<D>(&wire_b)
                // = H(a.base || b.base), consistent with and_via_table on the evaluator side.
                IrExpr::MethodCall {
                    receiver: alloc::boxed::Box::new(var(&name_a)),
                    method: MethodKind::Std("and_result".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![ref_expr(var(&name_b))],
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(garble_expr),
        });
        var_names.insert(result_id.0, let_name);
    }

    // Return (tables, output_garble).
    let (output_garble_expr, _) = build_return(block, &var_names, garble_type());
    let ret_expr = IrExpr::Tuple(vec![var("tables"), output_garble_expr]);

    let func = IrFunction {
        name: format!("{}_garble", name),
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(alloc::boxed::Box::new(ret_expr)),
        },
    };

    let mut module = IrModule {
        name: "weaved_garbler".into(),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// GarbledCircuit / EvalSetup weaving passes
// ============================================================================

fn global_secret_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("GlobalSecret".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

/// `GarbledCircuit<N, I, A>` — const-generic params as literal `TypeParam` strings.
fn garbled_circuit_type(num_inputs: usize, num_and: usize) -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("GarbledCircuit".into()),
        type_args: vec![
            IrType::TypeParam("N".into()),
            IrType::TypeParam(format!("{}", num_inputs)),
            IrType::TypeParam(format!("{}", num_and)),
        ],
    }
}

/// `EvalSetup<N, A>` — const-generic `A` as a literal `TypeParam` string.
fn eval_setup_type(num_and: usize) -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("EvalSetup".into()),
        type_args: vec![
            IrType::TypeParam("N".into()),
            IrType::TypeParam(format!("{}", num_and)),
        ],
    }
}

/// Weave a single-block boolean circuit into a function that returns a
/// [`GarbledCircuit`], capturing all garbling material for multi-evaluation.
///
/// The generated function signature is:
/// ```text
/// fn <name>_into_gc<N: ArraySize, D: Digest>(
///     secret: GlobalSecret<N>,
///     input_0: Garble<N>,   // garbler's false-label for input bit 0 (moved)
///     ...
/// ) -> GarbledCircuit<N>
/// ```
///
/// Unlike `weave_garbler`, all parameters are **owned** (not borrowed) so they
/// can be stored directly in the returned `GarbledCircuit`. Call this once per
/// circuit instance; use `GarbledCircuit::encode_inputs` + the evaluator function
/// for each subsequent evaluation.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_into_gc(circuit: &BIrBlocks, name: &str, linkage: Option<&LinkageSystem>) -> IrModule {
    assert!(
        circuit.is_circuit(),
        "weave_into_gc: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let mut var_names: BTreeMap<u32, String> = BTreeMap::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    // Parameters: secret (owned), then input_i (owned).
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "secret".into(),
        ty: global_secret_type(),
    });
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: garble_type(),
        });
    }

    // Count AND gates now so we can compute the return type and build FixedArrays.
    let and_count = expanded
        .iter()
        .filter(|(_, s)| matches!(s, BIrStmt::And(..)))
        .count();

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut table_counter: usize = 0;

    // Gate loop — same logic as weave_garbler except:
    // - `and_result` receiver is cloned to avoid moving a potentially reused wire.
    // - No Vec push; table vars are collected by name and assembled into a FixedArray.
    for (result_id, stmt) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        let garble_expr = match stmt {
            BIrStmt::Zero | BIrStmt::One => garble_struct(array_default()),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: alloc::boxed::Box::new(base_index(&name_a, "j")),
                        right: alloc::boxed::Box::new(base_index(&name_b, "j")),
                    },
                ))
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: alloc::boxed::Box::new(base_index(&name_a, "j")),
                        right: alloc::boxed::Box::new(IrExpr::Index {
                            base: alloc::boxed::Box::new(IrExpr::MethodCall {
                                receiver: alloc::boxed::Box::new(var("secret")),
                                method: MethodKind::Std("secret".into()),
                                type_args: vec![],
                                args: vec![],
                            }),
                            index: alloc::boxed::Box::new(var("j")),
                        }),
                    },
                ))
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_var = format!("table_{}", table_counter);
                table_counter += 1;

                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&table_var),
                    ty: None,
                    init: Some(IrExpr::MethodCall {
                        receiver: alloc::boxed::Box::new(var("secret")),
                        method: MethodKind::Std("gen_and_table".into()),
                        type_args: vec![IrType::TypeParam("D".into())],
                        args: vec![
                            ref_expr(clone_expr(var(&name_a))),
                            ref_expr(clone_expr(var(&name_b))),
                        ],
                    }),
                });

                // Clone receiver to avoid moving a potentially reused wire.
                IrExpr::MethodCall {
                    receiver: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Std("and_result".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![ref_expr(var(&name_b))],
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(garble_expr),
        });
        var_names.insert(result_id.0, let_name);
    }

    // Build fixed-size array literals for the struct fields.
    // Input labels: [input_0, input_1, …] — params are never moved in the gate loop.
    let input_labels_expr = IrExpr::FixedArray(
        (0..num_params).map(|i| var(&format!("input_{}", i))).collect(),
    );
    // Tables: [table_0, table_1, …] — each table var is defined above and used once here.
    let tables_expr = IrExpr::FixedArray(
        (0..and_count).map(|k| var(&format!("table_{}", k))).collect(),
    );

    // Return GarbledCircuit<N, I, A> { secret, input_labels, tables, output_label }.
    let (output_garble_expr, _) = build_return(block, &var_names, garble_type());
    let ret_expr = IrExpr::StructExpr {
        kind: StructKind::Custom("GarbledCircuit".into()),
        type_args: vec![],
        fields: vec![
            ("secret".into(), var("secret")),
            ("input_labels".into(), input_labels_expr),
            ("tables".into(), tables_expr),
            ("output_label".into(), output_garble_expr),
        ],
        rest: None,
    };

    let func = IrFunction {
        name: format!("{}_into_gc", name),
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(garbled_circuit_type(num_params, and_count)),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(alloc::boxed::Box::new(ret_expr)),
        },
    };

    let mut module = IrModule {
        name: "weaved_into_gc".into(),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

/// Weave a single-block boolean circuit into a garbled-circuit **evaluator** that
/// takes a pre-built [`EvalSetup`] instead of individual table parameters.
///
/// The generated function signature is:
/// ```text
/// fn <name>_eval_from_setup<N: ArraySize, D: Digest>(
///     setup: &EvalSetup<N>,
///     input_0: &Eval<N>,    // one per block parameter, in order
///     ...
/// ) -> Eval<N>
/// ```
///
/// `setup` holds the reusable garbling material (`one_wire`, tables, output label).
/// Call `GarbledCircuit::eval_setup()` once to obtain it; this function can then
/// be called once per evaluation with freshly encoded input labels.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_eval_from_setup(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    assert!(
        circuit.is_circuit(),
        "weave_eval_from_setup: circuit must satisfy is_circuit()"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    // Count AND gates to compute the concrete EvalSetup<N, A> parameter type.
    let and_count = expanded
        .iter()
        .filter(|(_, s)| matches!(s, BIrStmt::And(..)))
        .count();

    let mut var_names: BTreeMap<u32, String> = BTreeMap::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    // Parameters: setup: &EvalSetup<N, A> (concrete A), then input_i: &Eval<N>.
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "setup".into(),
        ty: ref_to(eval_setup_type(and_count)),
    });
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: ref_to(eval_type()),
        });
    }

    // Helper: `setup.one_wire`
    let setup_one_wire = || IrExpr::Field {
        base: alloc::boxed::Box::new(var("setup")),
        field: "one_wire".into(),
    };

    // Helper: `setup.tables[k]`
    let setup_table = |k: usize| IrExpr::Index {
        base: alloc::boxed::Box::new(IrExpr::Field {
            base: alloc::boxed::Box::new(var("setup")),
            field: "tables".into(),
        }),
        index: alloc::boxed::Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
    };

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut and_counter: usize = 0;

    for (result_id, stmt) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        let init_expr = match stmt {
            BIrStmt::Zero => IrExpr::StructExpr {
                kind: StructKind::Custom("Eval".into()),
                type_args: vec![],
                fields: vec![("target".into(), array_default())],
                rest: None,
            },

            BIrStmt::One => clone_expr(setup_one_wire()),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    right: alloc::boxed::Box::new(clone_expr(var(&name_b))),
                }
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let k = and_counter;
                and_counter += 1;
                IrExpr::MethodCall {
                    receiver: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Std("and_via_table".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![
                        ref_expr(clone_expr(var(&name_b))),
                        ref_expr(setup_table(k)),
                    ],
                }
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    right: alloc::boxed::Box::new(clone_expr(setup_one_wire())),
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let func = IrFunction {
        name: format!("{}_eval_from_setup", name),
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(alloc::boxed::Box::new(ret_expr)),
        },
    };

    let mut module = IrModule {
        name: "weaved_eval_from_setup".into(),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// Shared helpers
// ============================================================================

/// `Garble { base: {base_expr} }`
fn garble_struct(base_expr: IrExpr) -> IrExpr {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Garble".into()),
        type_args: vec![],
        fields: vec![("base".into(), base_expr)],
        rest: None,
    }
}

/// Build the return expression and type from a circuit terminator.
///
/// For single-output circuits returns `(Var(wire_N), T)`.
/// For multi-output returns `(Tuple([...]), Tuple([T; n]))`.
fn build_return(
    block: &BIrBlock,
    var_names: &BTreeMap<u32, String>,
    elem_ty: IrType,
) -> (IrExpr, IrType) {
    match &block.terminator {
        BIrTerminator::Jmp(target) => {
            assert!(
                matches!(target.block, IRBlockTargetId::Return),
                "build_return: expected Return terminator"
            );
            let args = &target.args;
            if args.len() == 1 {
                let expr = var(var_names[&args[0].0].as_str());
                (expr, elem_ty)
            } else {
                let exprs: Vec<IrExpr> = args
                    .iter()
                    .map(|id| var(var_names[&id.0].as_str()))
                    .collect();
                let tys: Vec<IrType> = args.iter().map(|_| elem_ty.clone()).collect();
                (IrExpr::Tuple(exprs), IrType::Tuple(tys))
            }
        }
        _ => panic!("build_return: circuit must have a Jmp(Return) terminator"),
    }
}

// ============================================================================
// Bounded (movfuscated) weaving wrappers
// ============================================================================

/// Weave a bounded movfuscated Boolar circuit into a garbled-circuit **evaluator**.
///
/// Applies `lower_to_circuit(circuit, limit, mode)` to flatten any self-loop into
/// a plain circuit, then delegates to [`weave_evaluator`].
///
/// # Panics
/// Inherits all panics from [`lower_to_circuit`] (back-edges to non-zero blocks, Dyn dispatch).
pub fn weave_evaluator_bounded(
    circuit: &BIrBlocks,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_evaluator(&lowered, name, linkage)
}

/// Weave a bounded movfuscated Boolar circuit into a garbled-circuit **garbler**.
///
/// Applies `lower_to_circuit(circuit, limit, mode)` first, then delegates to
/// [`weave_garbler`].
///
/// # Panics
/// Inherits all panics from [`lower_to_circuit`].
pub fn weave_garbler_bounded(
    circuit: &BIrBlocks,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_garbler(&lowered, name, linkage)
}

/// Weave a bounded movfuscated Boolar circuit into a `GarbledCircuit`-returning function.
///
/// Applies `lower_to_circuit(circuit, limit, mode)` first, then delegates to
/// [`weave_into_gc`].
pub fn weave_into_gc_bounded(
    circuit: &BIrBlocks,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_into_gc(&lowered, name, linkage)
}

/// Weave a bounded movfuscated Boolar circuit into an `EvalSetup`-based evaluator.
///
/// Applies `lower_to_circuit(circuit, limit, mode)` first, then delegates to
/// [`weave_eval_from_setup`].
pub fn weave_eval_from_setup_bounded(
    circuit: &BIrBlocks,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_eval_from_setup(&lowered, name, linkage)
}

// ============================================================================
// Weaver-specific printer helper
// ============================================================================

/// Render a weaved `IrModule` to Rust source.
///
/// When `self_contained` is `false` (the default for externally-linked output),
/// the preamble imports types from `volar_spec::garble`. When `self_contained`
/// is `true` (used after [`LinkageSystem::apply`] has merged the spec into the
/// module), those imports are omitted — all required types are already present
/// in the module body.
pub fn print_weaved_module(module: &IrModule, self_contained: bool) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut out = String::new();
    // Write the module body (no doc preamble).
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module }));

    let preamble: &str = if self_contained {
        concat!(
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
            "extern crate alloc;\n",
            "use alloc::vec::Vec;\n",
            "use alloc::vec;\n",
            "use core::ops::{BitXor, Div};\n",
            "use hybrid_array::{Array, ArraySize};\n",
            "use digest::Digest;\n",
            "\n",
        )
    } else {
        concat!(
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
            "extern crate alloc;\n",
            "use alloc::vec::Vec;\n",
            "use alloc::vec;\n",
            "use core::ops::{BitXor};\n",
            "use hybrid_array::{Array, ArraySize};\n",
            "use digest::Digest;\n",
            "use volar_spec::garble::{Eval, EvalSetup, Garble, GarbledCircuit, GarbleTable, GlobalSecret};\n",
            "\n",
        )
    };

    let mut full = String::with_capacity(preamble.len() + out.len());
    full.push_str(preamble);
    full.push_str(&out);
    full
}

/// Parse the `garble.rs` spec source and return a [`LinkedSpec`] ready for use
/// with [`LinkageSystem::add`].
///
/// This embeds `volar-spec/src/garble.rs` at compile time, so the weaver crate
/// does not need a runtime path to the spec. Requires the `linking` feature.
#[cfg(feature = "linking")]
pub fn garble_linked_spec() -> LinkedSpec {
    use volar_compiler::parser::parse_sources;
    let source = include_str!("../../../spec/volar-spec/src/garble.rs");
    let module = parse_sources(&[("garble.rs", source)], "garble")
        .expect("garble_linked_spec: failed to parse garble.rs");
    LinkedSpec {
        name: "garble".into(),
        module,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use std::{fs, path::Path, process::Command, string::String};

    use super::*;
    use volar_ir::{
        boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
        ir::{IRBlockId, IRBlockTargetId, IRVarId},
    };

    /// Workspace root derived from `CARGO_MANIFEST_DIR` at compile time.
    fn workspace_root() -> String {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .ancestors()
            .nth(3) // volar-weaver -> compiler -> crates -> workspace root
            .expect("could not find workspace root")
            .to_string_lossy()
            .into_owned()
    }

    /// Two-input circuit: Xor(0,1)->wire2, And(0,2)->wire3, Return wire3.
    fn build_xor_and_circuit() -> BIrBlocks {
        BIrBlocks(vec![BIrBlock {
            params: 2,
            stmts: vec![
                BIrStmt::Xor(IRVarId(0), IRVarId(1)),
                BIrStmt::And(IRVarId(0), IRVarId(2)),
            ],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(3)],
            }),
        }])
    }

    /// Generate a temp Cargo project, put `code` in src/lib.rs,
    /// and run `cargo check` to verify compilation.
    fn run_compile_check(code: &str, test_name: &str) {
        let root = workspace_root();
        let tmpdir = std::env::temp_dir().join(format!("volar_weaver_{}", test_name));
        let srcdir = tmpdir.join("src");
        fs::create_dir_all(&srcdir).unwrap();

        let cargo_toml = std::format!(
            "[package]\n\
             name = \"weave-check-{name}\"\n\
             version = \"0.1.0\"\n\
             edition = \"2024\"\n\
             \n\
             [lib]\n\
             path = \"src/lib.rs\"\n\
             \n\
             [dependencies]\n\
             volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
             volar-primitives = {{ path = \"{root}/crates/spec/volar-primitives\" }}\n\
             volar-common = {{ path = \"{root}/crates/spec/volar-common\" }}\n\
             hybrid-array = \"0.4.8\"\n\
             digest = {{ version = \"0.11.2\", default-features = false }}\n\
             cipher = {{ version = \"0.5.1\", default-features = false }}\n\
             rand = {{ version = \"0.9.2\", default-features = false }}\n\
             typenum = {{ version = \"1.17\", default-features = false }}\n\
             elliptic-curve = {{ version = \"0.13.8\", features = [\"arithmetic\"], default-features = false }}\n",
            name = test_name,
            root = root,
        );

        fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).unwrap();
        fs::write(srcdir.join("lib.rs"), code).unwrap();

        let output = Command::new("cargo")
            .args(["check", "--quiet"])
            .current_dir(&tmpdir)
            // Use a per-test target dir to avoid locking conflicts.
            .env(
                "CARGO_TARGET_DIR",
                String::from(tmpdir.join("target").to_str().unwrap()),
            )
            .output()
            .expect("failed to run cargo check");

        // Capture output before cleanup so we can report it on failure.
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let _ = fs::remove_dir_all(&tmpdir);

        if !output.status.success() {
            panic!(
                "Generated code failed to compile (test: {})\n--- code ---\n{}\n--- stderr ---\n{}",
                test_name, code, stderr
            );
        }
    }

    /// Single-bit self-loop: params=1, stmts=[One], CondJmp → Return or Block(0).
    fn build_simple_loop() -> BIrBlocks {
        BIrBlocks(vec![BIrBlock {
            params: 1,
            stmts: vec![BIrStmt::One], // IRVarId(1) = constant 1
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

    #[test]
    fn test_weave_evaluator_bounded_unconditional_compiles() {
        let circuit = build_simple_loop();
        let module = weave_evaluator_bounded(&circuit, "loop_eval", 4, LoweringMode::Unconditional, None);
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "bounded_eval_uncond");
    }

    #[test]
    fn test_weave_evaluator_bounded_with_flag_compiles() {
        let circuit = build_simple_loop();
        let module = weave_evaluator_bounded(&circuit, "loop_eval_flag", 4, LoweringMode::WithTerminationFlag, None);
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "bounded_eval_flag");
    }

    #[test]
    fn test_weave_garbler_bounded_compiles() {
        let circuit = build_simple_loop();
        let module = weave_garbler_bounded(&circuit, "loop_garble", 4, LoweringMode::Unconditional, None);
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "bounded_garbler");
    }

    #[test]
    fn test_weave_evaluator_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_evaluator(&circuit, "test_circuit", None);
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "evaluator");
    }

    #[test]
    fn test_weave_garbler_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_garbler(&circuit, "test_circuit", None);
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "garbler");
    }

    #[test]
    fn test_weave_into_gc_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_into_gc(&circuit, "test_circuit", None);
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "into_gc");
    }

    #[test]
    fn test_weave_eval_from_setup_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_eval_from_setup(&circuit, "test_circuit", None);
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "eval_from_setup");
    }

    /// Runtime multi-evaluation correctness: garble a 2-input AND circuit once,
    /// then evaluate all four input combinations using the same GarbledCircuit.
    #[test]
    fn test_multi_eval_and_circuit() {
        let root = workspace_root();

        // Build a 2-input AND circuit: params=2, stmts=[And(0,1)], Return wire_2.
        let and_circuit = BIrBlocks(vec![BIrBlock {
            params: 2,
            stmts: vec![BIrStmt::And(IRVarId(0), IRVarId(1))],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            }),
        }]);

        let gc_module = weave_into_gc(&and_circuit, "and2", None);
        let eval_module = weave_eval_from_setup(&and_circuit, "and2", None);

        // Merge both functions into one module so they share a single preamble.
        let combined_module = IrModule {
            name: "combined".into(),
            functions: gc_module.functions.into_iter().chain(eval_module.functions).collect(),
            structs: vec![],
            traits: vec![],
            impls: vec![],
            type_aliases: vec![],
        };
        let fns_code = print_weaved_module(&combined_module, false);

        // Append a runtime test that exercises all 4 AND input combinations.
        let test_code = std::format!(
            "{fns}\n\
             #[cfg(test)]\n\
             mod multi_eval_tests {{\n\
                 use super::*;\n\
                 use volar_spec::garble::{{Garble, GlobalSecret}};\n\
                 use hybrid_array::{{Array, typenum::U16}};\n\
                 use sha2::Sha256;\n\
                 \n\
                 type N = U16;\n\
                 type D = Sha256;\n\
                 \n\
                 fn det_bytes(seed: u8) -> Array<u8, N> {{\n\
                     let mut a = Array::<u8, N>::default();\n\
                     for (i, b) in a.iter_mut().enumerate() {{\n\
                         *b = (i as u8).wrapping_mul(37).wrapping_add(seed);\n\
                     }}\n\
                     // Force LSB = 1 so it can serve as a GlobalSecret.\n\
                     a[0] |= 1;\n\
                     a\n\
                 }}\n\
                 \n\
                 #[test]\n\
                 fn and_truth_table() {{\n\
                     let secret = GlobalSecret::<N>::new(det_bytes(13));\n\
                     let label0 = Garble::<N> {{ base: det_bytes(7) }};\n\
                     let label1 = Garble::<N> {{ base: det_bytes(91) }};\n\
                     let gc = and2_into_gc::<N, D>(secret, label0, label1);\n\
                     let setup = gc.eval_setup();\n\
                     for a in [false, true] {{\n\
                         for b in [false, true] {{\n\
                             let encoded = gc.encode_inputs(&[a, b]);\n\
                             let result = and2_eval_from_setup::<N, D>(\n\
                                 &setup, &encoded[0], &encoded[1]);\n\
                             assert_eq!(\n\
                                 setup.recover_output(&result), a && b,\n\
                                 \"AND({{}},{{}}) should be {{}}\", a, b, a && b);\n\
                         }}\n\
                     }}\n\
                 }}\n\
             }}\n",
            fns = fns_code,
        );

        let tmpdir = std::env::temp_dir().join("volar_weaver_multi_eval_and");
        let srcdir = tmpdir.join("src");
        std::fs::create_dir_all(&srcdir).unwrap();

        let cargo_toml = std::format!(
            "[package]\n\
             name = \"weave-check-multi-eval-and\"\n\
             version = \"0.1.0\"\n\
             edition = \"2024\"\n\
             \n\
             [[test]]\n\
             name = \"multi_eval\"\n\
             path = \"src/lib.rs\"\n\
             \n\
             [dependencies]\n\
             volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
             volar-primitives = {{ path = \"{root}/crates/spec/volar-primitives\" }}\n\
             volar-common = {{ path = \"{root}/crates/spec/volar-common\" }}\n\
             hybrid-array = {{ version = \"0.4.8\" }}\n\
             digest = {{ version = \"0.11.2\", default-features = false }}\n\
             cipher = {{ version = \"0.5.1\", default-features = false }}\n\
             rand = {{ version = \"0.9.2\", default-features = false }}\n\
             typenum = {{ version = \"1.17\", default-features = false }}\n\
             elliptic-curve = {{ version = \"0.13.8\", features = [\"arithmetic\"], default-features = false }}\n\
             sha2 = {{ version = \"0.11\", default-features = false }}\n",
            root = root,
        );

        std::fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).unwrap();
        std::fs::write(srcdir.join("lib.rs"), &test_code).unwrap();

        let output = std::process::Command::new("cargo")
            .args(["test", "--quiet", "--test", "multi_eval"])
            .current_dir(&tmpdir)
            .env("CARGO_TARGET_DIR", String::from(tmpdir.join("target").to_str().unwrap()))
            .output()
            .expect("failed to run cargo test");

        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let _ = std::fs::remove_dir_all(&tmpdir);

        if !output.status.success() {
            panic!(
                "Multi-eval test failed\n--- code ---\n{}\n--- stdout ---\n{}\n--- stderr ---\n{}",
                test_code, stdout, stderr
            );
        }
    }
}
