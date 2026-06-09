// @reliability: experimental
// @ai: assisted
//! Network-protocol weaving pass for VOLE-ZK proving and verifying.
//!
//! Generates IR functions that run the Quicksilver VOLE-ZK proof over a
//! pluggable IO transport (`volar_net::VoleTransport`).  The output compiles
//! through both the Rust and TypeScript backends unchanged — no new backends
//! or IR extensions are required.
//!
//! ## Functions produced
//!
//! | Weaver function | IR type | Use case |
//! |----------------|---------|---------|
//! | [`weave_net_vole_prover`] | `IrModule<IrFunction>` | flat circuit, all hats sent at once |
//! | [`weave_net_vole_verifier`] | `IrModule<IrFunction>` | flat circuit, hats received at once |
//! | [`weave_net_vole_prover_loop`] | `IrCfgModule` | streaming loop, one hat-batch per iteration |
//! | [`weave_net_vole_verifier_loop`] | `IrCfgModule` | streaming loop, per-iteration verification |
//!
//! ## Transport trait
//!
//! Generated functions take `transport: &mut Tr` where `Tr: volar_net::VoleTransport<N, T>`.
//! The transport is responsible for serialisation, framing, and IO.

use alloc::{
    boxed::Box,
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        AssociatedType, ExternalKind, IrAnyFunction, IrBlock, IrCfgBlock, IrCfgBody,
        IrCfgFunction, IrCfgJump, IrCfgModule, IrCfgTerminator, IrExpr, IrFunction,
        IrGenericParam, IrGenericParamKind, IrLit, IrModule, IrParam, IrPattern, IrStmt,
        IrTraitBound, IrType, IrWherePredicate, MathTrait, MethodKind, PrimitiveType,
        SpecBinOp, StdMethod, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir::ir::IRVarId;

use crate::{
    clone_expr, expand_ors, ref_expr, var,
    NoProvenance,
    vole_common,
};

// ── Type helpers ──────────────────────────────────────────────────────────────

pub(crate) fn vope_type() -> IrType {
    vole_common::vope_type_k(1)
}

pub(crate) fn q_type() -> IrType {
    vole_common::q_type()
}

pub(crate) fn delta_type() -> IrType {
    vole_common::delta_type()
}

pub(crate) fn array_t_n() -> IrType {
    vole_common::array_t_n()
}

pub(crate) fn ref_to(ty: IrType) -> IrType {
    IrType::Reference { mutable: false, elem: Box::new(ty) }
}

pub(crate) fn ref_mut_to(ty: IrType) -> IrType {
    IrType::Reference { mutable: true, elem: Box::new(ty) }
}

pub(crate) fn bool_type() -> IrType {
    IrType::Primitive(PrimitiveType::Bool)
}

pub(crate) fn usize_type() -> IrType {
    IrType::Primitive(PrimitiveType::Usize)
}

/// `Vec<Array<T, N>>`
fn vec_array_t_n() -> IrType {
    IrType::Vector { elem: Box::new(array_t_n()) }
}

/// `Vec<Q<N, T>>`
fn vec_q_t_n() -> IrType {
    IrType::Vector { elem: Box::new(q_type()) }
}

/// `Result<T, E>` — two-arg generic Result.
pub(crate) fn result_type(ok: IrType, err: IrType) -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Result".into()),
        type_args: vec![ok, err],
    }
}

/// `<Tr as volar_net::VoleTransport<N, T>>::Error`
pub(crate) fn tr_error_type() -> IrType {
    IrType::Projection {
        base: Box::new(IrType::TypeParam("Tr".into())),
        trait_path: Some("volar_net::VoleTransport".into()),
        trait_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
        assoc: AssociatedType::Other("Error".into()),
    }
}

/// `&[Q<N, T>]` — slice reference for q_ands supply in loop verifier.
pub(crate) fn q_slice_type() -> IrType {
    ref_to(IrType::Array {
        kind: volar_compiler::ir::ArrayKind::Slice,
        elem: Box::new(q_type()),
        len: volar_compiler::ir::ArrayLength::Const(0),
    })
}

// ── Trait bound helpers ───────────────────────────────────────────────────────

pub(crate) fn array_size_bound() -> IrTraitBound {
    IrTraitBound { trait_kind: TraitKind::ArraySize, type_args: vec![], assoc_bindings: vec![] }
}

pub(crate) fn vole_array_t_bound() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::VoleArray,
        type_args: vec![IrType::TypeParam("T".into())],
        assoc_bindings: vec![],
    }
}

pub(crate) fn clone_t() -> IrTraitBound {
    IrTraitBound { trait_kind: TraitKind::Math(MathTrait::Clone), type_args: vec![], assoc_bindings: vec![] }
}

pub(crate) fn add_output_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Math(MathTrait::Add),
        type_args: vec![],
        assoc_bindings: vec![(AssociatedType::Output, IrType::TypeParam("T".into()))],
    }
}

pub(crate) fn mul_output_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Math(MathTrait::Mul),
        type_args: vec![],
        assoc_bindings: vec![(AssociatedType::Output, IrType::TypeParam("T".into()))],
    }
}

pub(crate) fn default_t() -> IrTraitBound {
    IrTraitBound { trait_kind: TraitKind::Math(MathTrait::Default), type_args: vec![], assoc_bindings: vec![] }
}

fn partial_eq_t() -> IrTraitBound {
    IrTraitBound { trait_kind: TraitKind::Math(MathTrait::PartialEq), type_args: vec![], assoc_bindings: vec![] }
}

fn vole_transport_bound() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::External {
            path: vec!["volar_net".into(), "VoleTransport".into()],
        },
        type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
        assoc_bindings: vec![],
    }
}

/// `<N: ArraySize, T>` with prover where clause + `Tr: VoleTransport<N, T>`.
pub(crate) fn net_prover_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    let generics = vec![
        IrGenericParam {
            name: "N".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![array_size_bound()],
            default: None,
        },
        IrGenericParam {
            name: "T".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![],
            default: None,
        },
        IrGenericParam {
            name: "Tr".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![vole_transport_bound()],
            default: None,
        },
    ];
    let where_clause = vec![
        IrWherePredicate::TypeBound {
            ty: IrType::TypeParam("N".into()),
            bounds: vec![vole_array_t_bound()],
        },
        IrWherePredicate::TypeBound {
            ty: IrType::TypeParam("T".into()),
            bounds: vec![clone_t(), add_output_t(), mul_output_t(), default_t()],
        },
    ];
    (generics, where_clause)
}

/// Adds `T: PartialEq` for the verifier and loop-prover variants.
pub(crate) fn net_verifier_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    let (generics, mut where_clause) = net_prover_generics_and_where();
    if let Some(IrWherePredicate::TypeBound { bounds, .. }) = where_clause.last_mut() {
        bounds.push(partial_eq_t());
    }
    (generics, where_clause)
}

/// Loop prover also needs `T: PartialEq` for `vope_bit`.
pub(crate) fn net_prover_loop_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    net_verifier_generics_and_where()
}

// ── Expression helpers ────────────────────────────────────────────────────────

/// `Ok(expr)` — wrap in Result::Ok.
pub(crate) fn ok_expr(inner: IrExpr) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path { segments: vec!["Ok".into()], type_args: vec![] }),
        args: vec![inner],
    }
}

/// `transport.METHOD(args...)?`
pub(crate) fn transport_call_try(method: &str, args: Vec<IrExpr>) -> IrExpr {
    IrExpr::Try(Box::new(IrExpr::MethodCall {
        receiver: Box::new(var("transport")),
        method: MethodKind::Other(method.into()),
        type_args: vec![],
        args,
    }))
}

/// `volar_net::vope_bit(&wire_name)`
pub(crate) fn vope_bit_call(wire_name: &str) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["volar_net".into(), "vope_bit".into()],
            type_args: vec![],
        }),
        args: vec![ref_expr(var(wire_name))],
    }
}

/// `&[hat_0, hat_1, ...]` as a slice reference to a fixed array.
pub(crate) fn hats_slice_expr(hat_names: &[String]) -> IrExpr {
    let arr = IrExpr::FixedArray(hat_names.iter().map(|h| var(h)).collect());
    ref_expr(arr)
}

/// `let (wire_k, hat_k) = vole_and_prover_step::<N,T>(a.clone(), b.clone());`
pub(crate) fn emit_prover_and_gate(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    hat_name: &str,
    stmts: &mut Vec<IrStmt>,
) {
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(hat_name),
        ]),
        ty: None,
        init: Some(IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["vole_and_prover_step".into()],
                type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
            }),
            args: vec![clone_expr(var(name_a)), clone_expr(var(name_b))],
        }),
    });
}

/// `let (wire_k, ok_k) = vole_and_verifier_check::<N,T>(delta, &qa, &qb, &q_and, &hat); all_ok = all_ok && ok_k;`
pub(crate) fn emit_verifier_and_gate(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    ok_name: &str,
    q_and_expr: IrExpr,  // expression that yields the Q_and value
    hat_expr: IrExpr,    // expression that yields the hat value
    stmts: &mut Vec<IrStmt>,
) {
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(ok_name),
        ]),
        ty: None,
        init: Some(IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["vole_and_verifier_check".into()],
                type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
            }),
            args: vec![
                var("delta"),
                ref_expr(var(name_a)),
                ref_expr(var(name_b)),
                ref_expr(q_and_expr),
                ref_expr(hat_expr),
            ],
        }),
    });
    stmts.push(IrStmt::Semi(IrExpr::Assign {
        left: Box::new(var("all_ok")),
        right: Box::new(IrExpr::Binary {
            op: SpecBinOp::And,
            left: Box::new(var("all_ok")),
            right: Box::new(var(ok_name)),
        }),
    }));
}

// ── Gate count helper ─────────────────────────────────────────────────────────

pub(crate) fn count_and_gates<P: Clone>(circuit: &BIrBlocks<P>) -> usize {
    let block = &circuit.blocks[0];
    expand_ors(block)
        .into_iter()
        .filter(|(_, s, _)| matches!(s, BIrStmt::And(..)))
        .count()
}

// ============================================================================
// Single-shot prover: weave_net_vole_prover
// ============================================================================

/// Weave a flat (non-looping) circuit into a VOLE **prover** `IrModule` that
/// runs over a pluggable network transport.
///
/// The generated function signature is:
/// ```text
/// fn vole_prove_net_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     vope_one: Vope<N, T, U1>,
///     vope_input_0: Vope<N, T, U1>, ...
///     transport: &mut Tr,
/// ) -> Result<Vope<N, T, U1>, <Tr as VoleTransport<N, T>>::Error>
/// ```
///
/// The prover computes all AND-gate hats, calls `transport.send_hats(&hats)?`,
/// then `transport.recv_verdict()?`, and returns `Ok(output_wire)`.
pub fn weave_net_vole_prover(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    assert!(circuit.is_circuit(), "weave_net_vole_prover: circuit must satisfy is_circuit()");

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let (generics, where_clause) = net_prover_generics_and_where();

    // Parameters: vope_one, then vope_input_i, then transport.
    let mut params: Vec<IrParam> = vec![
        IrParam { name: "vope_one".into(), ty: vope_type() },
    ];
    let mut var_names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        let pname = format!("vope_input_{}", i);
        var_names.insert(i as u32, pname.clone());
        params.push(IrParam { name: pname, ty: vope_type() });
    }
    params.push(IrParam { name: "transport".into(), ty: ref_mut_to(IrType::TypeParam("Tr".into())) });

    let ret_type = result_type(vope_type(), tr_error_type());

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut and_counter: usize = 0;
    let mut hat_names: Vec<String> = Vec::new();

    for (result_id, stmt, _) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        match stmt {
            BIrStmt::Zero => {
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Vope".into()),
                        type_args: vec![],
                        fields: vec![
                            ("u".into(), crate::array_default()),
                            ("v".into(), IrExpr::Call {
                                func: Box::new(IrExpr::Path {
                                    segments: vec!["Array".into(), "default".into()],
                                    type_args: vec![
                                        IrType::TypeParam("T".into()),
                                        IrType::TypeParam("N".into()),
                                    ],
                                }),
                                args: vec![],
                            }),
                        ],
                        rest: None,
                    }),
                });
            }
            BIrStmt::One => {
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var("vope_one"))),
                });
            }
            BIrStmt::Xor(a, b) => {
                let na = var_names[&a.0].clone();
                let nb = var_names[&b.0].clone();
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&na))),
                        right: Box::new(clone_expr(var(&nb))),
                    }),
                });
            }
            BIrStmt::Not(a) => {
                let na = var_names[&a.0].clone();
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&na))),
                        right: Box::new(clone_expr(var("vope_one"))),
                    }),
                });
            }
            BIrStmt::And(a, b) => {
                let na = var_names[&a.0].clone();
                let nb = var_names[&b.0].clone();
                let hat_name = format!("hat_{}", and_counter);
                and_counter += 1;
                hat_names.push(hat_name.clone());
                emit_prover_and_gate(&na, &nb, &let_name, &hat_name, &mut stmts);
            }
            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. } | BIrStmt::ActionCall { .. }
            | BIrStmt::OracleBit { .. } | BIrStmt::ActionBit { .. }
            | BIrStmt::Rng { .. } => {
                // Skip external operations — net pass works on simple circuits.
                // Callers using oracles/actions should use the IRBlocks weavers.
                var_names.insert(result_id.0, let_name.clone());
                continue;
            }
            BIrStmt::StorageRead { .. } | BIrStmt::StorageWrite { .. } => {
                unimplemented!("StorageRead/Write: use weave_net_vole_prover_ir")
            }
        }
        var_names.insert(result_id.0, let_name);
    }

    // Emit: transport.send_hats(&[hat_0, hat_1, ...])?;
    let hats_ref = hats_slice_expr(&hat_names);
    stmts.push(IrStmt::Semi(transport_call_try("send_hats", vec![hats_ref])));
    // Emit: transport.recv_verdict()?;
    stmts.push(IrStmt::Semi(transport_call_try("recv_verdict", vec![])));

    // Return Ok(output_wire).
    let (output_expr, _) = crate::build_return(block, &var_names, vope_type());
    let ret_expr = ok_expr(output_expr);

    let func = IrFunction {
        name: format!("vole_prove_net_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock { stmts, stmt_provs: vec![], expr: Some(Box::new(ret_expr)) },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: format!("weaved_net_prover_{}", name),
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
// Single-shot verifier: weave_net_vole_verifier
// ============================================================================

/// Weave a flat (non-looping) circuit into a VOLE **verifier** `IrModule` that
/// runs over a pluggable network transport.
///
/// The generated function signature is:
/// ```text
/// fn vole_verify_net_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     delta: &Delta<N, T>,
///     q_and_0: Q<N, T>, ...   // pre-shared VOLE wire shares for AND outputs
///     q_input_0: Q<N, T>, ... // pre-shared VOLE wire shares for inputs
///     transport: &mut Tr,
/// ) -> Result<bool, <Tr as VoleTransport<N, T>>::Error>
/// ```
///
/// The verifier calls `transport.recv_hats(AND_COUNT)?` to receive the hat
/// values, runs gate-level checks, calls `transport.send_verdict(ok)?`, and
/// returns `Ok(ok)`.
pub fn weave_net_vole_verifier(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    assert!(circuit.is_circuit(), "weave_net_vole_verifier: circuit must satisfy is_circuit()");

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let and_count = expanded.iter().filter(|(_, s, _)| matches!(s, BIrStmt::And(..))).count();

    let (generics, where_clause) = net_verifier_generics_and_where();

    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to(delta_type()) },
    ];
    // Pre-shared Q shares for AND gate outputs.
    for k in 0..and_count {
        params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
    }
    // Pre-shared Q shares for inputs.
    let mut var_names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        let pname = format!("q_input_{}", i);
        var_names.insert(i as u32, pname.clone());
        params.push(IrParam { name: pname, ty: q_type() });
    }
    params.push(IrParam { name: "transport".into(), ty: ref_mut_to(IrType::TypeParam("Tr".into())) });

    let ret_type = result_type(bool_type(), tr_error_type());

    let mut stmts: Vec<IrStmt> = Vec::new();

    // let mut all_ok = true;
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(IrExpr::Lit(IrLit::Bool(true))),
    });

    // let hats = transport.recv_hats(AND_COUNT)?;
    stmts.push(IrStmt::Let {
        pattern: IrPattern::ident("hats"),
        ty: None,
        init: Some(transport_call_try("recv_hats", vec![
            IrExpr::Lit(IrLit::Int(and_count as i128)),
        ])),
    });

    let mut and_counter: usize = 0;

    for (result_id, stmt, _) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        match stmt {
            BIrStmt::Zero => {
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Q".into()),
                        type_args: vec![],
                        fields: vec![("q".into(), IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["Array".into(), "default".into()],
                                type_args: vec![
                                    IrType::TypeParam("T".into()),
                                    IrType::TypeParam("N".into()),
                                ],
                            }),
                            args: vec![],
                        })],
                        rest: None,
                    }),
                });
            }
            BIrStmt::One => {
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Q".into()),
                        type_args: vec![],
                        fields: vec![("q".into(), IrExpr::MethodCall {
                            receiver: Box::new(IrExpr::Field {
                                base: Box::new(var("delta")),
                                field: "delta".into(),
                            }),
                            method: MethodKind::Known(StdMethod::Clone),
                            type_args: vec![],
                            args: vec![],
                        })],
                        rest: None,
                    }),
                });
            }
            BIrStmt::Xor(a, b) => {
                let na = var_names[&a.0].clone();
                let nb = var_names[&b.0].clone();
                // Q(a XOR b) = Q(a) + Q(b) element-wise.
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Q".into()),
                        type_args: vec![],
                        fields: vec![("q".into(), IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["Array".into(), "from_fn".into()],
                                type_args: vec![
                                    IrType::TypeParam("T".into()),
                                    IrType::TypeParam("N".into()),
                                ],
                            }),
                            args: vec![IrExpr::Closure {
                                params: vec![volar_compiler::ir::IrClosureParam {
                                    pattern: IrPattern::ident("i"),
                                    ty: None,
                                }],
                                ret_type: None,
                                body: Box::new(IrExpr::Binary {
                                    op: SpecBinOp::Add,
                                    left: Box::new(clone_expr(IrExpr::Index {
                                        base: Box::new(IrExpr::Field {
                                            base: Box::new(var(&na)),
                                            field: "q".into(),
                                        }),
                                        index: Box::new(var("i")),
                                    })),
                                    right: Box::new(clone_expr(IrExpr::Index {
                                        base: Box::new(IrExpr::Field {
                                            base: Box::new(var(&nb)),
                                            field: "q".into(),
                                        }),
                                        index: Box::new(var("i")),
                                    })),
                                }),
                            }],
                        })],
                        rest: None,
                    }),
                });
            }
            BIrStmt::Not(a) => {
                let na = var_names[&a.0].clone();
                // Q(NOT a) = Q(a) + Delta element-wise.
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Q".into()),
                        type_args: vec![],
                        fields: vec![("q".into(), IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["Array".into(), "from_fn".into()],
                                type_args: vec![
                                    IrType::TypeParam("T".into()),
                                    IrType::TypeParam("N".into()),
                                ],
                            }),
                            args: vec![IrExpr::Closure {
                                params: vec![volar_compiler::ir::IrClosureParam {
                                    pattern: IrPattern::ident("i"),
                                    ty: None,
                                }],
                                ret_type: None,
                                body: Box::new(IrExpr::Binary {
                                    op: SpecBinOp::Add,
                                    left: Box::new(clone_expr(IrExpr::Index {
                                        base: Box::new(IrExpr::Field {
                                            base: Box::new(var(&na)),
                                            field: "q".into(),
                                        }),
                                        index: Box::new(var("i")),
                                    })),
                                    right: Box::new(clone_expr(IrExpr::Index {
                                        base: Box::new(IrExpr::Field {
                                            base: Box::new(var("delta")),
                                            field: "delta".into(),
                                        }),
                                        index: Box::new(var("i")),
                                    })),
                                }),
                            }],
                        })],
                        rest: None,
                    }),
                });
            }
            BIrStmt::And(a, b) => {
                let na = var_names[&a.0].clone();
                let nb = var_names[&b.0].clone();
                let ok_name = format!("ok_and_{}", and_counter);
                let q_and_name = format!("q_and_{}", and_counter);
                // hat comes from hats[and_counter].
                let hat_expr = IrExpr::Index {
                    base: Box::new(var("hats")),
                    index: Box::new(IrExpr::Lit(IrLit::Int(and_counter as i128))),
                };
                let q_and_expr = var(&q_and_name);
                emit_verifier_and_gate(&na, &nb, &let_name, &ok_name, q_and_expr, hat_expr, &mut stmts);
                and_counter += 1;
            }
            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. } | BIrStmt::ActionCall { .. }
            | BIrStmt::OracleBit { .. } | BIrStmt::ActionBit { .. }
            | BIrStmt::Rng { .. } => {
                var_names.insert(result_id.0, let_name.clone());
                continue;
            }
            BIrStmt::StorageRead { .. } | BIrStmt::StorageWrite { .. } => {
                unimplemented!("StorageRead/Write: use weave_net_vole_verifier_ir")
            }
        }
        var_names.insert(result_id.0, let_name);
    }

    // transport.send_verdict(all_ok)?;
    stmts.push(IrStmt::Semi(transport_call_try("send_verdict", vec![var("all_ok")])));

    let ret_expr = ok_expr(var("all_ok"));

    let func = IrFunction {
        name: format!("vole_verify_net_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock { stmts, stmt_provs: vec![], expr: Some(Box::new(ret_expr)) },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: format!("weaved_net_verifier_{}", name),
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
// Streaming loop prover: weave_net_vole_prover_loop
// ============================================================================

/// Weave a single-iteration circuit into a VOLE **prover** `IrCfgModule` with
/// a dynamic streaming loop.
///
/// The circuit represents ONE iteration of a loop.  The generated CFG function
/// runs that iteration repeatedly, sending one batch of AND-gate hats per
/// iteration via `transport.send_iteration(hats, done_bit)?`.  When the
/// circuit's done-flag wire (the **last output** of the circuit) becomes `true`,
/// the prover sends `is_sentinel = true` and waits for the verifier's verdict.
///
/// The generated function signature is:
/// ```text
/// fn vole_prove_net_loop_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     vope_one: Vope<N, T, U1>,
///     init_w0: Vope<N, T, U1>, ...   // initial state wires (one per circuit input)
///     transport: &mut Tr,
/// ) -> Result<Vope<N, T, U1>, <Tr as VoleTransport<N, T>>::Error>
/// ```
///
/// ## Convention
///
/// The circuit must produce at least two output wires.  The **last** output wire
/// is treated as the loop-termination done-flag; all other outputs become the
/// next iteration's inputs.  This matches `LoweringMode::WithTerminationFlag`
/// semantics (but the circuit is run per-iteration, not statically unrolled).
pub fn weave_net_vole_prover_loop(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_movfuscated(), "weave_net_vole_prover_loop: circuit must be single-block");

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let and_count = count_and_gates(circuit);

    let (generics, where_clause) = net_prover_loop_generics_and_where();

    // Function params: vope_one, init_w_i..., transport.
    let mut func_params: Vec<IrParam> = vec![
        IrParam { name: "vope_one".into(), ty: vope_type() },
    ];
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_w{}", i), ty: vope_type() });
    }
    func_params.push(IrParam {
        name: "transport".into(),
        ty: ref_mut_to(IrType::TypeParam("Tr".into())),
    });

    let ret_type = result_type(vope_type(), tr_error_type());

    // ── Block 0: entry ─────────────────────────────────────────────────────
    // No params. Terminator: Goto(Block 1, args=[init_w0.clone(), ...])
    // Clone is required because the entry block is inside the generated `loop`
    // body, and Rust's borrow checker conservatively treats any `move` inside a
    // loop as potentially happening more than once.
    let b0_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("init_w{}", i))))
        .collect();
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: loop body ────────────────────────────────────────────────
    // Params: w0, w1, ... (one Vope per circuit input — the loop state).
    let mut b1_params: Vec<IrParam> = (0..num_params)
        .map(|i| IrParam { name: format!("w{}", i), ty: vope_type() })
        .collect();
    // We also carry vope_one through (it's constant but CFG blocks receive all
    // live values as params; reference it from the outer function arg instead).

    let mut b1_stmts: Vec<IrStmt> = Vec::new();
    let mut b1_var_names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        b1_var_names.insert(i as u32, format!("w{}", i));
    }

    let mut hat_names: Vec<String> = Vec::new();
    let mut and_counter_loop = 0usize;

    // Emit gate computation (identical structure to weave_vole_prover_inner).
    for (result_id, stmt, _) in &expanded {
        let let_name = format!("lw_{}", result_id.0);
        match stmt {
            BIrStmt::Zero => {
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Vope".into()),
                        type_args: vec![],
                        fields: vec![
                            ("u".into(), crate::array_default()),
                            ("v".into(), IrExpr::Call {
                                func: Box::new(IrExpr::Path {
                                    segments: vec!["Array".into(), "default".into()],
                                    type_args: vec![
                                        IrType::TypeParam("T".into()),
                                        IrType::TypeParam("N".into()),
                                    ],
                                }),
                                args: vec![],
                            }),
                        ],
                        rest: None,
                    }),
                });
            }
            BIrStmt::One => {
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var("vope_one"))),
                });
            }
            BIrStmt::Xor(a, b) => {
                let na = b1_var_names[&a.0].clone();
                let nb = b1_var_names[&b.0].clone();
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&na))),
                        right: Box::new(clone_expr(var(&nb))),
                    }),
                });
            }
            BIrStmt::Not(a) => {
                let na = b1_var_names[&a.0].clone();
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&na))),
                        right: Box::new(clone_expr(var("vope_one"))),
                    }),
                });
            }
            BIrStmt::And(a, b) => {
                let na = b1_var_names[&a.0].clone();
                let nb = b1_var_names[&b.0].clone();
                let hat_name = format!("lhat_{}", and_counter_loop);
                and_counter_loop += 1;
                hat_names.push(hat_name.clone());
                emit_prover_and_gate(&na, &nb, &let_name, &hat_name, &mut b1_stmts);
            }
            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            _ => {
                b1_var_names.insert(result_id.0, let_name.clone());
                continue;
            }
        }
        b1_var_names.insert(result_id.0, let_name);
    }

    // Determine the done-flag wire and output/next-state wires from the terminator.
    // Two supported shapes:
    //   Jmp(Return, args=[out0, ..., outN-1, done]):  last arg = done, rest = next state
    //   CondJmp { val=done, then=Return[out0], else=Block[next0, ..] }
    let (done_wire_name, output_wire_name, back_edge_args) = match &block.terminator {
        volar_ir::boolar::BIrTerminator::Jmp(t) => {
            assert!(!t.args.is_empty(), "loop Jmp must have at least one arg");
            let done_id = t.args.last().unwrap().0;
            let out_id = t.args[0].0;
            let back: Vec<IrExpr> = t.args[..t.args.len() - 1]
                .iter()
                .map(|id| clone_expr(var(&b1_var_names[&id.0])))
                .collect();
            (b1_var_names[&done_id].clone(), b1_var_names[&out_id].clone(), back)
        }
        volar_ir::boolar::BIrTerminator::CondJmp { val, then_target, else_target } => {
            let done_name = b1_var_names[&val.0].clone();
            let out_id = then_target.args.get(0).map(|id| id.0).unwrap_or(val.0);
            let back: Vec<IrExpr> = else_target.args
                .iter()
                .map(|id| clone_expr(var(&b1_var_names[&id.0])))
                .collect();
            (done_name, b1_var_names[&out_id].clone(), back)
        }
    };

    // let done_bit = volar_net::vope_bit(&done_wire);
    b1_stmts.push(IrStmt::Let {
        pattern: IrPattern::ident("done_bit"),
        ty: None,
        init: Some(vope_bit_call(&done_wire_name)),
    });

    // transport.send_iteration(&[hat_0, ...], done_bit)?;
    let hats_ref = hats_slice_expr(&hat_names);
    b1_stmts.push(IrStmt::Semi(transport_call_try(
        "send_iteration",
        vec![hats_ref, var("done_bit")],
    )));

    let b1_terminator = IrCfgTerminator::CondGoto {
        cond: var("done_bit"),
        then_: IrCfgJump {
            target: 2,
            args: vec![clone_expr(var(&output_wire_name))],
        },
        else_: IrCfgJump { target: 1, args: back_edge_args },
    };

    let block1 = IrCfgBlock {
        params: b1_params,
        stmts: b1_stmts,
        stmt_provs: vec![],
        terminator: b1_terminator,
    };

    // ── Block 2: exit ─────────────────────────────────────────────────────
    // Param: output (the result wire from the terminating iteration).
    // Stmts: transport.recv_verdict()?;
    // Terminator: Return(Ok(output))
    let block2 = IrCfgBlock {
        params: vec![IrParam { name: "output".into(), ty: vope_type() }],
        stmts: vec![IrStmt::Semi(transport_call_try("recv_verdict", vec![]))],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(ok_expr(var("output")))),
    };

    let func = IrCfgFunction {
        name: format!("vole_prove_net_loop_{}", name),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0, block1, block2] },
    };

    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_net_prover_loop_{}", name),
        functions: vec![IrAnyFunction::Cfg(func)],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply_cfg(&mut module);
    }
    module
}

// ============================================================================
// Streaming loop verifier: weave_net_vole_verifier_loop
// ============================================================================

/// Weave a single-iteration circuit into a VOLE **verifier** `IrCfgModule` with
/// a dynamic streaming loop.
///
/// The generated function signature is:
/// ```text
/// fn vole_verify_net_loop_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     delta: &Delta<N, T>,
///     q_ands: &[Q<N, T>],    // pre-allocated Q shares: q_ands[iter * AND_COUNT + k]
///     q_input_0: Q<N, T>, ...
///     transport: &mut Tr,
/// ) -> Result<bool, <Tr as VoleTransport<N, T>>::Error>
/// ```
///
/// Per iteration: receives hats via `transport.recv_iteration(AND_COUNT)?`, runs
/// gate-level checks, and checks the sentinel.  After the sentinel, sends the
/// accumulated verdict and returns.
pub fn weave_net_vole_verifier_loop(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_movfuscated(), "weave_net_vole_verifier_loop: circuit must be single-block");

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let and_count = count_and_gates(circuit);

    let (generics, where_clause) = net_verifier_generics_and_where();

    // Function params: delta, q_ands slice, q_input_i..., transport.
    let mut func_params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to(delta_type()) },
        IrParam { name: "q_ands".into(), ty: q_slice_type() },
    ];
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_q{}", i), ty: q_type() });
    }
    func_params.push(IrParam {
        name: "transport".into(),
        ty: ref_mut_to(IrType::TypeParam("Tr".into())),
    });

    let ret_type = result_type(bool_type(), tr_error_type());

    // ── Block 0: entry ─────────────────────────────────────────────────────
    // Args to Block 1: [init_q0.clone()..., all_ok=true, iter=0]
    // Clone required for same reason as prover: move inside generated loop body.
    let mut b0_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("init_q{}", i))))
        .collect();
    b0_args.push(IrExpr::Lit(IrLit::Bool(true)));  // all_ok
    b0_args.push(IrExpr::Lit(IrLit::Int(0)));       // iter index

    let block0 = IrCfgBlock {
        params: vec![],
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: loop body ─────────────────────────────────────────────────
    // Params: q0..qN-1 (state), all_ok: bool, iter: usize
    let mut b1_params: Vec<IrParam> = (0..num_params)
        .map(|i| IrParam { name: format!("q{}", i), ty: q_type() })
        .collect();
    b1_params.push(IrParam { name: "all_ok".into(), ty: bool_type() });
    b1_params.push(IrParam { name: "iter".into(), ty: usize_type() });

    let mut b1_stmts: Vec<IrStmt> = Vec::new();

    // let (hats, is_sentinel) = transport.recv_iteration(AND_COUNT)?;
    b1_stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident("iter_hats"),
            IrPattern::ident("is_sentinel"),
        ]),
        ty: None,
        init: Some(transport_call_try("recv_iteration", vec![
            IrExpr::Lit(IrLit::Int(and_count as i128)),
        ])),
    });

    // let mut all_ok_new = all_ok;
    b1_stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok_new".into(), subpat: None },
        ty: None,
        init: Some(var("all_ok")),
    });

    // Gate computation — same as verifier inner but hats come from iter_hats[k]
    // and q_and values come from q_ands[iter * AND_COUNT + k].
    let mut b1_var_names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        b1_var_names.insert(i as u32, format!("q{}", i));
    }

    let mut and_counter_loop = 0usize;

    for (result_id, stmt, _) in &expanded {
        let let_name = format!("lq_{}", result_id.0);
        match stmt {
            BIrStmt::Zero => {
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Q".into()),
                        type_args: vec![],
                        fields: vec![("q".into(), IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["Array".into(), "default".into()],
                                type_args: vec![
                                    IrType::TypeParam("T".into()),
                                    IrType::TypeParam("N".into()),
                                ],
                            }),
                            args: vec![],
                        })],
                        rest: None,
                    }),
                });
            }
            BIrStmt::One => {
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Q".into()),
                        type_args: vec![],
                        fields: vec![("q".into(), IrExpr::MethodCall {
                            receiver: Box::new(IrExpr::Field {
                                base: Box::new(var("delta")),
                                field: "delta".into(),
                            }),
                            method: MethodKind::Known(StdMethod::Clone),
                            type_args: vec![],
                            args: vec![],
                        })],
                        rest: None,
                    }),
                });
            }
            BIrStmt::Xor(a, b) => {
                let na = b1_var_names[&a.0].clone();
                let nb = b1_var_names[&b.0].clone();
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Q".into()),
                        type_args: vec![],
                        fields: vec![("q".into(), IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["Array".into(), "from_fn".into()],
                                type_args: vec![
                                    IrType::TypeParam("T".into()),
                                    IrType::TypeParam("N".into()),
                                ],
                            }),
                            args: vec![IrExpr::Closure {
                                params: vec![volar_compiler::ir::IrClosureParam {
                                    pattern: IrPattern::ident("i"),
                                    ty: None,
                                }],
                                ret_type: None,
                                body: Box::new(IrExpr::Binary {
                                    op: SpecBinOp::Add,
                                    left: Box::new(clone_expr(IrExpr::Index {
                                        base: Box::new(IrExpr::Field {
                                            base: Box::new(var(&na)),
                                            field: "q".into(),
                                        }),
                                        index: Box::new(var("i")),
                                    })),
                                    right: Box::new(clone_expr(IrExpr::Index {
                                        base: Box::new(IrExpr::Field {
                                            base: Box::new(var(&nb)),
                                            field: "q".into(),
                                        }),
                                        index: Box::new(var("i")),
                                    })),
                                }),
                            }],
                        })],
                        rest: None,
                    }),
                });
            }
            BIrStmt::Not(a) => {
                let na = b1_var_names[&a.0].clone();
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Q".into()),
                        type_args: vec![],
                        fields: vec![("q".into(), IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["Array".into(), "from_fn".into()],
                                type_args: vec![
                                    IrType::TypeParam("T".into()),
                                    IrType::TypeParam("N".into()),
                                ],
                            }),
                            args: vec![IrExpr::Closure {
                                params: vec![volar_compiler::ir::IrClosureParam {
                                    pattern: IrPattern::ident("i"),
                                    ty: None,
                                }],
                                ret_type: None,
                                body: Box::new(IrExpr::Binary {
                                    op: SpecBinOp::Add,
                                    left: Box::new(clone_expr(IrExpr::Index {
                                        base: Box::new(IrExpr::Field {
                                            base: Box::new(var(&na)),
                                            field: "q".into(),
                                        }),
                                        index: Box::new(var("i")),
                                    })),
                                    right: Box::new(clone_expr(IrExpr::Index {
                                        base: Box::new(IrExpr::Field {
                                            base: Box::new(var("delta")),
                                            field: "delta".into(),
                                        }),
                                        index: Box::new(var("i")),
                                    })),
                                }),
                            }],
                        })],
                        rest: None,
                    }),
                });
            }
            BIrStmt::And(a, b) => {
                let na = b1_var_names[&a.0].clone();
                let nb = b1_var_names[&b.0].clone();
                let ok_name = format!("lok_and_{}", and_counter_loop);
                // q_and = q_ands[iter * AND_COUNT + k]
                let q_and_idx = IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(IrExpr::Binary {
                        op: SpecBinOp::Mul,
                        left: Box::new(var("iter")),
                        right: Box::new(IrExpr::Lit(IrLit::Int(and_count as i128))),
                    }),
                    right: Box::new(IrExpr::Lit(IrLit::Int(and_counter_loop as i128))),
                };
                let q_and_expr = clone_expr(IrExpr::Index {
                    base: Box::new(var("q_ands")),
                    index: Box::new(q_and_idx),
                });
                // hat = iter_hats[k]
                let hat_expr = IrExpr::Index {
                    base: Box::new(var("iter_hats")),
                    index: Box::new(IrExpr::Lit(IrLit::Int(and_counter_loop as i128))),
                };
                // Emit check and update all_ok_new (not all_ok).
                b1_stmts.push(IrStmt::Let {
                    pattern: IrPattern::Tuple(vec![
                        IrPattern::ident(&let_name),
                        IrPattern::ident(&ok_name),
                    ]),
                    ty: None,
                    init: Some(IrExpr::Call {
                        func: Box::new(IrExpr::Path {
                            segments: vec!["vole_and_verifier_check".into()],
                            type_args: vec![
                                IrType::TypeParam("N".into()),
                                IrType::TypeParam("T".into()),
                            ],
                        }),
                        args: vec![
                            var("delta"),
                            ref_expr(var(&na)),
                            ref_expr(var(&nb)),
                            ref_expr(q_and_expr),
                            ref_expr(hat_expr),
                        ],
                    }),
                });
                b1_stmts.push(IrStmt::Semi(IrExpr::Assign {
                    left: Box::new(var("all_ok_new")),
                    right: Box::new(IrExpr::Binary {
                        op: SpecBinOp::And,
                        left: Box::new(var("all_ok_new")),
                        right: Box::new(var(&ok_name)),
                    }),
                }));
                and_counter_loop += 1;
            }
            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            _ => {
                b1_var_names.insert(result_id.0, let_name.clone());
                continue;
            }
        }
        b1_var_names.insert(result_id.0, let_name);
    }

    // Build back-edge args (next-state Q wires, all_ok, iter+1).
    // Two supported terminator shapes:
    //   Jmp(Return, args=[out0, ..., done]):  next state = all but last
    //   CondJmp { val=done, then=Return, else=Block[next0, ..] }
    let next_state_ids: Vec<IRVarId> = match &block.terminator {
        volar_ir::boolar::BIrTerminator::Jmp(t) => {
            t.args[..t.args.len().saturating_sub(1)].to_vec()
        }
        volar_ir::boolar::BIrTerminator::CondJmp { else_target, .. } => {
            else_target.args.clone()
        }
    };

    let mut back_args: Vec<IrExpr> = next_state_ids
        .iter()
        .map(|id| clone_expr(var(&b1_var_names[&id.0])))
        .collect();
    back_args.push(var("all_ok_new"));
    back_args.push(IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(var("iter")),
        right: Box::new(IrExpr::Lit(IrLit::Int(1))),
    });

    let b1_terminator = IrCfgTerminator::CondGoto {
        cond: var("is_sentinel"),
        then_: IrCfgJump { target: 2, args: vec![var("all_ok_new")] },
        else_: IrCfgJump { target: 1, args: back_args },
    };

    let block1 = IrCfgBlock {
        params: b1_params,
        stmts: b1_stmts,
        stmt_provs: vec![],
        terminator: b1_terminator,
    };

    // ── Block 2: exit ─────────────────────────────────────────────────────
    let block2 = IrCfgBlock {
        params: vec![IrParam { name: "final_ok".into(), ty: bool_type() }],
        stmts: vec![IrStmt::Semi(transport_call_try("send_verdict", vec![var("final_ok")]))],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(ok_expr(var("final_ok")))),
    };

    let func = IrCfgFunction {
        name: format!("vole_verify_net_loop_{}", name),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0, block1, block2] },
    };

    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_net_verifier_loop_{}", name),
        functions: vec![IrAnyFunction::Cfg(func)],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply_cfg(&mut module);
    }
    module
}

// ============================================================================
// Printers
// ============================================================================

/// Render a network VOLE `IrModule<IrFunction>` (flat) to Rust source.
pub fn print_net_vole_module(module: &IrModule<IrFunction>) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut body = String::new();
    let _ = write!(body, "{}", DisplayRust(ModuleWriter { module, emit_async: false }));

    let preamble = concat!(
        "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
        "extern crate alloc;\n",
        "use alloc::vec::Vec;\n",
        "use alloc::vec;\n",
        "use core::ops::{Add, Mul};\n",
        "use hybrid_array::{Array, ArraySize};\n",
        "use cipher::consts::U1;\n",
        "use volar_spec::vole::{Delta, Q, Vope, VoleArray};\n",
        "use volar_spec::vole::prove::{vole_and_prover_step, vole_and_verifier_check};\n",
        "use volar_net::VoleTransport;\n",
        "\n",
    );

    let mut out = String::with_capacity(preamble.len() + body.len());
    out.push_str(preamble);
    out.push_str(&body);
    out
}

/// Render a network VOLE `IrCfgModule` (loop) to Rust source.
pub fn print_net_vole_cfg_module(module: &IrCfgModule) -> String {
    use volar_compiler::printer::{CfgModuleWriter, DisplayRust};
    use alloc::fmt::Write as _;

    let mut body = String::new();
    let _ = write!(body, "{}", DisplayRust(CfgModuleWriter { module, emit_async: false }));

    let preamble = concat!(
        "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
        "extern crate alloc;\n",
        "use alloc::vec::Vec;\n",
        "use alloc::vec;\n",
        "use core::ops::{Add, Mul};\n",
        "use hybrid_array::{Array, ArraySize};\n",
        "use cipher::consts::U1;\n",
        "use volar_spec::vole::{Delta, Q, Vope, VoleArray};\n",
        "use volar_spec::vole::prove::{vole_and_prover_step, vole_and_verifier_check};\n",
        "use volar_net::VoleTransport;\n",
        "\n",
    );

    let mut out = String::with_capacity(preamble.len() + body.len());
    out.push_str(preamble);
    out.push_str(&body);
    out
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::tests_common::{build_xor_and_circuit, build_simple_loop, run_compile_check_net};

    #[test]
    fn test_net_prover_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_net_vole_prover(&circuit, "test_circuit", None);
        let code = print_net_vole_module(&module);
        run_compile_check_net(&code, "net_vole_prover");
    }

    #[test]
    fn test_net_verifier_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_net_vole_verifier(&circuit, "test_circuit", None);
        let code = print_net_vole_module(&module);
        run_compile_check_net(&code, "net_vole_verifier");
    }

    #[test]
    fn test_net_prover_loop_compiles() {
        let circuit = build_simple_loop();
        let module = weave_net_vole_prover_loop(&circuit, "test_loop", None);
        let code = print_net_vole_cfg_module(&module);
        run_compile_check_net(&code, "net_vole_prover_loop");
    }

    #[test]
    fn test_net_verifier_loop_compiles() {
        let circuit = build_simple_loop();
        let module = weave_net_vole_verifier_loop(&circuit, "test_loop", None);
        let code = print_net_vole_cfg_module(&module);
        run_compile_check_net(&code, "net_vole_verifier_loop");
    }
}
