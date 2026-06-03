// @reliability: normal
//! @ai: assisted
//! Garbled-circuit weaving passes.
//!
//! Taken from the original `lib.rs`; split out so the VOLE passes can
//! live in their own submodule without sharing a 1 400-line file.
//!
//! Four public passes are provided:
//! - [`weave_evaluator`] – evaluates a garbled circuit given pre-computed tables.
//! - [`weave_garbler`]  – garbles a circuit, producing AND tables.
//! - [`weave_into_gc`]  – garbles a circuit into a reusable `GarbledCircuit` struct.
//! - [`weave_eval_from_setup`] – evaluates using a pre-built `EvalSetup`.

use alloc::{
    boxed::Box,
    format,
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        ExternalKind, IrBlock, IrExpr, IrFunction, IrGenericParam,
        IrGenericParamKind, IrLit, IrModule, IrParam, IrPattern, IrStmt, IrTraitBound, IrType,
        MethodKind, SpecBinOp, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
#[cfg(feature = "linking")]
use volar_compiler::linkage::LinkedSpec;
use volar_ir::boolar::{BIrBlocks, BIrStmt};
pub use volar_ir_passes::lower_to_circuit::LoweringMode;

use crate::{
    array_default, array_from_fn, base_index, build_return, clone_expr, expand_ors,
    ref_expr, var, NoProvenance, ProvenanceHandler,
};

// ============================================================================
// Garble-specific type helpers
// ============================================================================

pub(crate) fn eval_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Eval".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

pub(crate) fn garble_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Garble".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

pub(crate) fn garble_table_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("GarbleTable".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

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

/// Generic params `<N: ArraySize, D: Digest>`.
fn generic_params() -> Vec<IrGenericParam> {
    vec![
        IrGenericParam {
            name: "N".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![IrTraitBound {
                trait_kind: TraitKind::ArraySize,
                type_args: vec![],
                assoc_bindings: vec![],
            }],
            default: None,
        },
        IrGenericParam {
            name: "D".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![IrTraitBound {
                trait_kind: TraitKind::Digest,
                type_args: vec![],
                assoc_bindings: vec![],
            }],
            default: None,
        },
    ]
}

/// `Garble { base: {base_expr} }`
fn garble_struct<P: Clone + Default>(base_expr: IrExpr<P>) -> IrExpr<P> {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Garble".into()),
        type_args: vec![],
        fields: vec![("base".into(), base_expr)],
        rest: None,
    }
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
/// Backwards-compatible evaluator weave — discards provenance.
///
/// See [`weave_evaluator_with_handler`] for the provenance-preserving variant.
pub fn weave_evaluator<P: Clone + Default>(circuit: &BIrBlocks<P>, name: &str, linkage: Option<&LinkageSystem>) -> IrModule<IrFunction> {
    weave_evaluator_with_handler(circuit, name, linkage, &NoProvenance)
}

/// Weave a single-block boolean circuit into a garbled-circuit **evaluator** `IrModule`,
/// using `handler` to map input provenance into the output IR.
pub fn weave_evaluator_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    assert!(
        circuit.is_circuit(),
        "weave_evaluator: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let and_count = expanded
        .iter()
        .filter(|(_, s, _)| matches!(s, BIrStmt::And(..)))
        .count();

    let mut var_names = alloc::collections::BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "one_wire".into(),
        ty: crate::ref_to(eval_type()),
    });
    for k in 0..and_count {
        params.push(IrParam {
            name: format!("and_table_{}", k),
            ty: crate::ref_to(garble_table_type()),
        });
    }
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: crate::ref_to(eval_type()),
        });
    }

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut stmt_provs: Vec<H::Output> = Vec::new();
    let mut and_counter: usize = 0;

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        let init_expr = match stmt {
            BIrStmt::Zero => IrExpr::StructExpr {
                kind: StructKind::Custom("Eval".into()),
                type_args: vec![],
                fields: vec![("target".into(), array_default())],
                rest: None,
            },

            BIrStmt::One => clone_expr(var("one_wire")),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var(&name_b))),
                }
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_name = format!("and_table_{}", and_counter);
                and_counter += 1;
                IrExpr::MethodCall {
                    receiver: Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Other("and_via_table".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![
                        ref_expr(clone_expr(var(&name_b))),
                        var(&table_name),
                    ],
                }
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var("one_wire"))),
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::ActionCall { .. }
            | BIrStmt::ActionBit { .. }
            | BIrStmt::Rng { .. }
            | BIrStmt::StorageRead { .. }
            | BIrStmt::StorageWrite { .. } => {
                unimplemented!("garble weaver: extended BIrStmt variants not supported")
            }
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        stmt_provs.push(q);
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let func = IrFunction {
        name: name.into(),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
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
/// ) -> ([GarbleTable<N>; AND_COUNT], Garble<N>)
/// ```
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
/// Backwards-compatible garbler weave — discards provenance.
pub fn weave_garbler<P: Clone + Default>(circuit: &BIrBlocks<P>, name: &str, linkage: Option<&LinkageSystem>) -> IrModule<IrFunction> {
    weave_garbler_with_handler(circuit, name, linkage, &NoProvenance)
}

/// Weave a single-block boolean circuit into a garbled-circuit **garbler** `IrModule`,
/// using `handler` to map input provenance into the output IR.
pub fn weave_garbler_with_handler<P, H>(circuit: &BIrBlocks<P>, name: &str, linkage: Option<&LinkageSystem>, handler: &H) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    assert!(
        circuit.is_circuit(),
        "weave_garbler: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let mut var_names = alloc::collections::BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "secret".into(),
        ty: crate::ref_to(global_secret_type()),
    });
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: crate::ref_to(garble_type()),
        });
    }

    // Count AND gates up front so we can build a fixed-size table array in the return type.
    let and_count = expanded
        .iter()
        .filter(|(_, s, _)| matches!(s, BIrStmt::And(..)))
        .count();

    let ret_type = IrType::Tuple(vec![
        IrType::Array {
            kind: volar_compiler::ir::ArrayKind::FixedArray,
            elem: Box::new(garble_table_type()),
            len: volar_compiler::ir::ArrayLength::Const(and_count),
        },
        garble_type(),
    ]);

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut stmt_provs: Vec<H::Output> = Vec::new();
    let mut table_counter: usize = 0;
    let mut table_names: Vec<String> = Vec::new();

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        let garble_expr = match stmt {
            BIrStmt::Zero | BIrStmt::One => garble_struct(array_default()),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(base_index(&name_b, "j")),
                    },
                ))
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(IrExpr::Index {
                            base: Box::new(IrExpr::MethodCall {
                                receiver: Box::new(var("secret")),
                                method: MethodKind::Other("secret".into()),
                                type_args: vec![],
                                args: vec![],
                            }),
                            index: Box::new(var("j")),
                        }),
                    },
                ))
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_var = format!("table_{}", table_counter);
                table_counter += 1;
                table_names.push(table_var.clone());

                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&table_var),
                    ty: None,
                    init: Some(IrExpr::MethodCall {
                        receiver: Box::new(var("secret")),
                        method: MethodKind::Other("gen_and_table".into()),
                        type_args: vec![IrType::TypeParam("D".into())],
                        args: vec![
                            ref_expr(clone_expr(var(&name_a))),
                            ref_expr(clone_expr(var(&name_b))),
                        ],
                    }),
                });
                stmt_provs.push(q.clone());

                IrExpr::MethodCall {
                    receiver: Box::new(var(&name_a)),
                    method: MethodKind::Other("and_result".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![ref_expr(var(&name_b))],
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::ActionCall { .. }
            | BIrStmt::ActionBit { .. }
            | BIrStmt::Rng { .. }
            | BIrStmt::StorageRead { .. }
            | BIrStmt::StorageWrite { .. } => {
                unimplemented!("garble weaver: extended BIrStmt variants not supported")
            }
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(garble_expr),
        });
        stmt_provs.push(q);
        var_names.insert(result_id.0, let_name);
    }

    let (output_garble_expr, _) = build_return(block, &var_names, garble_type());
    let tables_expr = IrExpr::FixedArray(table_names.iter().map(|t| var(t)).collect());
    let ret_expr = IrExpr::Tuple(vec![tables_expr, output_garble_expr]);

    let func = IrFunction {
        name: format!("{}_garble", name),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_garbler".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// GarbledCircuit weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a function that returns a
/// [`GarbledCircuit`], capturing all garbling material for multi-evaluation.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
/// Backwards-compatible GarbledCircuit weave — discards provenance.
pub fn weave_into_gc<P: Clone + Default>(circuit: &BIrBlocks<P>, name: &str, linkage: Option<&LinkageSystem>) -> IrModule<IrFunction> {
    weave_into_gc_with_handler(circuit, name, linkage, &NoProvenance)
}

/// Weave a single-block boolean circuit into a `GarbledCircuit`-returning function,
/// using `handler` to map input provenance into the output IR.
pub fn weave_into_gc_with_handler<P, H>(circuit: &BIrBlocks<P>, name: &str, linkage: Option<&LinkageSystem>, handler: &H) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    assert!(
        circuit.is_circuit(),
        "weave_into_gc: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let mut var_names = alloc::collections::BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

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

    let and_count = expanded
        .iter()
        .filter(|(_, s, _)| matches!(s, BIrStmt::And(..)))
        .count();

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut stmt_provs: Vec<H::Output> = Vec::new();
    let mut table_counter: usize = 0;

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        let garble_expr = match stmt {
            BIrStmt::Zero | BIrStmt::One => garble_struct(array_default()),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(base_index(&name_b, "j")),
                    },
                ))
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(IrExpr::Index {
                            base: Box::new(IrExpr::MethodCall {
                                receiver: Box::new(var("secret")),
                                method: MethodKind::Other("secret".into()),
                                type_args: vec![],
                                args: vec![],
                            }),
                            index: Box::new(var("j")),
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
                        receiver: Box::new(var("secret")),
                        method: MethodKind::Other("gen_and_table".into()),
                        type_args: vec![IrType::TypeParam("D".into())],
                        args: vec![
                            ref_expr(clone_expr(var(&name_a))),
                            ref_expr(clone_expr(var(&name_b))),
                        ],
                    }),
                });
                stmt_provs.push(q.clone());

                IrExpr::MethodCall {
                    receiver: Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Other("and_result".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![ref_expr(var(&name_b))],
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::ActionCall { .. }
            | BIrStmt::ActionBit { .. }
            | BIrStmt::Rng { .. }
            | BIrStmt::StorageRead { .. }
            | BIrStmt::StorageWrite { .. } => {
                unimplemented!("garble weaver: extended BIrStmt variants not supported")
            }
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(garble_expr),
        });
        stmt_provs.push(q);
        var_names.insert(result_id.0, let_name);
    }

    let input_labels_expr = IrExpr::FixedArray(
        (0..num_params).map(|i| var(&format!("input_{}", i))).collect(),
    );
    let tables_expr = IrExpr::FixedArray(
        (0..and_count).map(|k| var(&format!("table_{}", k))).collect(),
    );

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
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(garbled_circuit_type(num_params, and_count)),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_into_gc".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// EvalSetup weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a garbled-circuit **evaluator** that
/// takes a pre-built [`EvalSetup`] instead of individual table parameters.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
/// Backwards-compatible EvalSetup weave — discards provenance.
pub fn weave_eval_from_setup<P: Clone + Default>(
    circuit: &BIrBlocks<P>,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    weave_eval_from_setup_with_handler(circuit, name, linkage, &NoProvenance)
}

/// Weave a single-block boolean circuit into an EvalSetup-based evaluator,
/// using `handler` to map input provenance into the output IR.
pub fn weave_eval_from_setup_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    assert!(
        circuit.is_circuit(),
        "weave_eval_from_setup: circuit must satisfy is_circuit()"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let and_count = expanded
        .iter()
        .filter(|(_, s, _)| matches!(s, BIrStmt::And(..)))
        .count();

    let mut var_names = alloc::collections::BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "setup".into(),
        ty: crate::ref_to(eval_setup_type(and_count)),
    });
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: crate::ref_to(eval_type()),
        });
    }

    let setup_one_wire = || -> IrExpr<H::Output> { IrExpr::Field {
        base: Box::new(var("setup")),
        field: "one_wire".into(),
    } };

    let setup_table = |k: usize| -> IrExpr<H::Output> { IrExpr::Index {
        base: Box::new(IrExpr::Field {
            base: Box::new(var("setup")),
            field: "tables".into(),
        }),
        index: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
    } };

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut stmt_provs: Vec<H::Output> = Vec::new();
    let mut and_counter: usize = 0;

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

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
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var(&name_b))),
                }
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let k = and_counter;
                and_counter += 1;
                IrExpr::MethodCall {
                    receiver: Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Other("and_via_table".into()),
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
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(setup_one_wire())),
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::ActionCall { .. }
            | BIrStmt::ActionBit { .. }
            | BIrStmt::Rng { .. }
            | BIrStmt::StorageRead { .. }
            | BIrStmt::StorageWrite { .. } => {
                unimplemented!("garble weaver: extended BIrStmt variants not supported")
            }
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        stmt_provs.push(q);
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let func = IrFunction {
        name: format!("{}_eval_from_setup", name),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_eval_from_setup".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// Bounded (movfuscated) weaving wrappers
// ============================================================================

/// Backwards-compatible bounded evaluator weave — discards provenance.
pub fn weave_evaluator_bounded<P: Clone + Default>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    weave_evaluator_bounded_with_handler(circuit, name, limit, mode, linkage, &NoProvenance)
}

/// Bounded evaluator weave with provenance handler.
pub fn weave_evaluator_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    use volar_ir_passes::lower_to_circuit::lower_to_circuit;
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_evaluator_with_handler(&lowered, name, linkage, handler)
}

/// Backwards-compatible bounded garbler weave — discards provenance.
pub fn weave_garbler_bounded<P: Clone + Default>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    weave_garbler_bounded_with_handler(circuit, name, limit, mode, linkage, &NoProvenance)
}

/// Bounded garbler weave with provenance handler.
pub fn weave_garbler_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    use volar_ir_passes::lower_to_circuit::lower_to_circuit;
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_garbler_with_handler(&lowered, name, linkage, handler)
}

/// Backwards-compatible bounded GarbledCircuit weave — discards provenance.
pub fn weave_into_gc_bounded<P: Clone + Default>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    weave_into_gc_bounded_with_handler(circuit, name, limit, mode, linkage, &NoProvenance)
}

/// Bounded GarbledCircuit weave with provenance handler.
pub fn weave_into_gc_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    use volar_ir_passes::lower_to_circuit::lower_to_circuit;
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_into_gc_with_handler(&lowered, name, linkage, handler)
}

/// Backwards-compatible bounded EvalSetup weave — discards provenance.
pub fn weave_eval_from_setup_bounded<P: Clone + Default>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    weave_eval_from_setup_bounded_with_handler(circuit, name, limit, mode, linkage, &NoProvenance)
}

/// Bounded EvalSetup weave with provenance handler.
pub fn weave_eval_from_setup_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    use volar_ir_passes::lower_to_circuit::lower_to_circuit;
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_eval_from_setup_with_handler(&lowered, name, linkage, handler)
}

// ============================================================================
// Printer
// ============================================================================

/// Render a weaved garble `IrModule` to Rust source.
pub fn print_weaved_module(module: &IrModule<IrFunction>, self_contained: bool) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut out = String::new();
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module, emit_async: false }));

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

/// Parse the `garble.rs` spec source and return a [`LinkedSpec`].
///
/// Requires the `linking` feature.
#[cfg(feature = "linking")]
pub fn garble_linked_spec() -> LinkedSpec {
    use volar_compiler::parser::{SourceInput, parse_sources};
    let source = include_str!("../../../spec/volar-spec/src/garble.rs");
    let module = parse_sources(&[SourceInput { source, name: "garble.rs" }], "garble")
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
    use std::string::String;

    use super::*;
    use crate::tests_common::{build_xor_and_circuit, build_simple_loop, run_compile_check, workspace_root};

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
        // xor_and has 1 AND gate → return type must be `[GarbleTable<N>; 1]`, not Vec
        assert!(
            code.contains("[GarbleTable<N>; 1]"),
            "Expected fixed-size table array in return type, got:\n{}",
            code
        );
        assert!(
            !code.contains("Vec<GarbleTable"),
            "Should not contain Vec<GarbleTable in generated garbler:\n{}",
            code
        );
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

        let and_circuit = crate::tests_common::build_and_circuit();

        let gc_module = weave_into_gc(&and_circuit, "and2", None);
        let eval_module = weave_eval_from_setup(&and_circuit, "and2", None);

        let combined_module = IrModule {
            name: "combined".into(),
            functions: gc_module.functions.into_iter().chain(eval_module.functions).collect(),
            structs: vec![],
            enums: vec![],
            traits: vec![],
            impls: vec![],
            type_aliases: vec![],

            consts: vec![],
        };
        let fns_code = print_weaved_module(&combined_module, false);

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
