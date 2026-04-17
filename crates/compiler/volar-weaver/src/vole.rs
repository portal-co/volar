// @reliability: experimental
//! @ai: assisted
//! VOLE proving and verifying weaving passes (Quicksilver-style ZK).
//!
//! These passes lower a boolean circuit (`BIrBlocks`) into two `IrModule`s
//! implementing a one-round interactive ZK proof based on the Quicksilver
//! VOLE-in-the-head (VOLEitH) protocol.
//!
//! See `docs/vole-weaving.md` for the full design and security rationale.
//!
//! ## VOLE relation
//!
//! Each wire `w` with bit value `x_w` is authenticated by:
//! ```text
//! K_w = M_w + x_w · Δ    (element-wise in the extension field T)
//! ```
//! The prover holds `(x_w, M_w)` as a `Vope<N, T, U1>`. The verifier holds
//! `K_w` as a `Q<N, T>` and the global secret `Δ` as `Delta<N, T>`.
//!
//! ## Gate lowering
//!
//! | Gate | Prover | Verifier |
//! |------|--------|----------|
//! | XOR  | `vope_a + vope_b` (free) | `q_a.q[i] + q_b.q[i]` (free) |
//! | NOT  | `vope_a + vope_one` (free) | `q_a.q[i] + delta.delta[i]` (free) |
//! | AND  | `vole_and_prover_step(a, b)` → `(vope_c, hat)` | `vole_and_verifier_check(delta, a, b, q_c, hat)` → `(Q_c, bool)` |
//!
//! The prover collects all AND gate `hat` values and returns them alongside
//! the output wire commitment. The verifier takes pre-assigned `Q<N,T>` shares
//! for AND output wires (from the VOLE setup phase) plus the prover-sent `hat`
//! values, checks each gate, and returns the output Q and an aggregate boolean.

use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        AssociatedType, ExternalKind, IrBlock, IrExpr, IrFunction, IrGenericParam, IrGenericParamKind,
        IrModule, IrParam, IrPattern, IrStmt, IrTraitBound, IrType, IrWherePredicate,
        MethodKind, SpecBinOp, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir::ir::{
    IRBlocks, IRBlock as CirBlock, IRBlockTargetId, IRTerminator,
    IRType as CircuitIrType, IRTypeId as CirTyId, IRTypes as CirTypes,
    IRVarId as CirVar, PrimType, Stmt, StorageId,
};
use volar_ir_passes::lower_to_circuit::lower_to_circuit;
pub use volar_ir_passes::lower_to_circuit::LoweringMode;

use crate::{array_default, build_return, clone_expr, expand_ors, ref_expr, var};

// ============================================================================
// VOLE-specific type helpers
// ============================================================================

/// `Vope<N, T, U1>` — prover's degree-1 VOLE wire commitment.
fn vope_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Vope".into()),
        type_args: vec![
            IrType::TypeParam("N".into()),
            IrType::TypeParam("T".into()),
            IrType::TypeParam("U1".into()),
        ],
    }
}

/// `Q<N, T>` — verifier's VOLE wire share.
fn q_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Q".into()),
        type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
    }
}

/// `Delta<N, T>` — verifier's global secret.
fn delta_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Delta".into()),
        type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
    }
}

/// `Array<T, N>` — element-wise hat / field vector.
fn array_t_n() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Array".into()),
        type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
    }
}

/// `[Array<T, N>; AND_COUNT]` — fixed-size hat array returned by the prover.
fn hat_array_type(and_count: usize) -> IrType {
    IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray,
        elem: Box::new(array_t_n()),
        len: volar_compiler::ir::ArrayLength::Const(and_count),
    }
}

/// `&T` reference helper.
fn ref_to_vole(ty: IrType) -> IrType {
    IrType::Reference {
        mutable: false,
        elem: Box::new(ty),
    }
}

// ============================================================================
// Generic parameter helpers
// ============================================================================

/// `T: Add<Output = T>` bound.
fn add_output_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Math(volar_compiler::ir::MathTrait::Add),
        type_args: vec![],
        assoc_bindings: vec![(AssociatedType::Output, IrType::TypeParam("T".into()))],
    }
}

/// `T: Mul<Output = T>` bound.
fn mul_output_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Math(volar_compiler::ir::MathTrait::Mul),
        type_args: vec![],
        assoc_bindings: vec![(AssociatedType::Output, IrType::TypeParam("T".into()))],
    }
}

/// `T: PartialEq` bound.
fn partial_eq_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Math(volar_compiler::ir::MathTrait::PartialEq),
        type_args: vec![],
        assoc_bindings: vec![],
    }
}

/// `T: Clone` bound.
fn clone_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Custom("Clone".into()),
        type_args: vec![],
        assoc_bindings: vec![],
    }
}

/// `T: Default` bound.
fn default_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Custom("Default".into()),
        type_args: vec![],
        assoc_bindings: vec![],
    }
}

/// `N: ArraySize` bound.
fn array_size_bound() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Custom("ArraySize".into()),
        type_args: vec![],
        assoc_bindings: vec![],
    }
}

/// `N: VoleArray<T>` bound.
fn vole_array_t_bound() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Custom("VoleArray".into()),
        type_args: vec![IrType::TypeParam("T".into())],
        assoc_bindings: vec![],
    }
}

/// Generic params `<N: ArraySize, T>` with prover where clause
/// `N: VoleArray<T>, T: Clone + Add<Output=T> + Mul<Output=T> + Default`.
fn prover_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    let generics = vec![
        IrGenericParam {
            name: "N".into(),
            kind: IrGenericParamKind::Type,
            bounds: vec![array_size_bound()],
            default: None,
        },
        IrGenericParam {
            name: "T".into(),
            kind: IrGenericParamKind::Type,
            bounds: vec![],
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

/// Generic params and where clause for verifier (adds `T: PartialEq`).
fn verifier_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    let (generics, mut where_clause) = prover_generics_and_where();
    // Extend the T bound to also include PartialEq.
    if let Some(IrWherePredicate::TypeBound { bounds, .. }) = where_clause.last_mut() {
        bounds.push(partial_eq_t());
    }
    (generics, where_clause)
}

// ============================================================================
// Expression helpers for VOLE
// ============================================================================

/// `Array::<T, N>::from_fn(|{idx}| {body})`
fn array_t_from_fn(idx: &str, body: IrExpr) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "from_fn".into()],
            type_args: vec![
                IrType::TypeParam("T".into()),
                IrType::TypeParam("N".into()),
            ],
        }),
        args: vec![IrExpr::Closure {
            params: vec![volar_compiler::ir::IrClosureParam {
                pattern: IrPattern::ident(idx),
                ty: None,
            }],
            ret_type: None,
            body: Box::new(body),
        }],
    }
}

/// `Array::<T, N>::default()` — the zero vector in the extension field.
fn array_t_default() -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "default".into()],
            type_args: vec![
                IrType::TypeParam("T".into()),
                IrType::TypeParam("N".into()),
            ],
        }),
        args: vec![],
    }
}

/// `wire.q[i]` — the verifier's Q share lane.
fn q_index(wire_name: &str, idx: &str) -> IrExpr {
    IrExpr::Index {
        base: Box::new(IrExpr::Field {
            base: Box::new(var(wire_name)),
            field: "q".into(),
        }),
        index: Box::new(var(idx)),
    }
}

/// `delta.delta[i]`
fn delta_index(idx: &str) -> IrExpr {
    IrExpr::Index {
        base: Box::new(IrExpr::Field {
            base: Box::new(var("delta")),
            field: "delta".into(),
        }),
        index: Box::new(var(idx)),
    }
}

/// `Q { q: {body} }`
fn q_struct(body: IrExpr) -> IrExpr {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Q".into()),
        type_args: vec![],
        fields: vec![("q".into(), body)],
        rest: None,
    }
}

// ============================================================================
// AND gate helper calls
// ============================================================================

/// Emit `let (_wire_k, _hat_k) = vole_and_prover_step::<N, T>(wire_a.clone(), wire_b.clone());`
/// The hat variable is left in scope for the caller to collect into a `FixedArray`.
fn emit_prover_and_gate(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    hat_name: &str,
    stmts: &mut Vec<IrStmt>,
) {
    // let (wire_k, hat_k) = vole_and_prover_step::<N, T>(wire_a.clone(), wire_b.clone());
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(hat_name),
        ]),
        ty: None,
        init: Some(IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["vole_and_prover_step".into()],
                type_args: vec![
                    IrType::TypeParam("N".into()),
                    IrType::TypeParam("T".into()),
                ],
            }),
            args: vec![
                clone_expr(var(name_a)),
                clone_expr(var(name_b)),
            ],
        }),
    });
}

/// Emit `let (_wire_k, _ok_k) = vole_and_verifier_check::<N, T>(delta, &wire_a, &wire_b, &q_and_k, &hat_k);`
/// followed by `all_ok = all_ok && _ok_k;`.
fn emit_verifier_and_gate(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    ok_name: &str,
    q_and_name: &str,
    hat_name: &str,
    stmts: &mut Vec<IrStmt>,
) {
    // let (wire_k, ok_k) = vole_and_verifier_check::<N, T>(delta, &wire_a, &wire_b, &q_and_k, &hat_k);
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(ok_name),
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
                ref_expr(var(name_a)),
                ref_expr(var(name_b)),
                ref_expr(var(q_and_name)),
                ref_expr(var(hat_name)),
            ],
        }),
    });

    // all_ok = all_ok && ok_k;
    stmts.push(IrStmt::Semi(IrExpr::Assign {
        left: Box::new(var("all_ok")),
        right: Box::new(IrExpr::Binary {
            op: SpecBinOp::And,
            left: Box::new(var("all_ok")),
            right: Box::new(var(ok_name)),
        }),
    }));
}

// ============================================================================
// Prover weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a VOLE **prover** `IrModule`.
///
/// The generated function signature is:
/// ```text
/// fn vole_prove_<name><N: ArraySize, T>(
///     vope_one: Vope<N, T, U1>,      // committed constant 1: u=[[1..]], v=[0..]
///     vope_input_0: Vope<N, T, U1>, // prover's input wire commitments
///     ...
/// ) -> (Vope<N, T, U1>, Vec<Array<T, N>>)
/// where
///     N: VoleArray<T>,
///     T: Clone + Add<Output = T> + Mul<Output = T> + Default,
/// ```
///
/// The returned `Vec<Array<T, N>>` contains one `hat` per AND gate in circuit order.
/// These are the Quicksilver `V̂` values that must be sent to the verifier.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_vole_prover(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    assert!(
        circuit.is_circuit(),
        "weave_vole_prover: circuit must satisfy is_circuit()"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let mut var_names = alloc::collections::BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("vope_input_{}", i));
    }

    // Build parameter list: vope_one, then vope_input_i.
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "vope_one".into(),
        ty: vope_type(),
    });
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("vope_input_{}", i),
            ty: vope_type(),
        });
    }

    // Return type: (Vope<N, T, U1>, [Array<T, N>; AND_COUNT]) — AND_COUNT known at weave time.
    let and_count = expanded
        .iter()
        .filter(|(_, s)| matches!(s, BIrStmt::And(..)))
        .count();
    let ret_type = IrType::Tuple(vec![vope_type(), hat_array_type(and_count)]);

    let (generics, where_clause) = prover_generics_and_where();

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut and_counter: usize = 0;
    let mut hat_names: Vec<String> = Vec::new();

    for (result_id, stmt) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        match stmt {
            BIrStmt::Zero => {
                // Vope { u: Array::<Array<T,N>,U1>::default(), v: Array::<T,N>::default() }
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Vope".into()),
                        type_args: vec![],
                        fields: vec![
                            ("u".into(), array_default()),  // Array<Array<T,N>,U1>::default()
                            ("v".into(), array_t_default()),
                        ],
                        rest: None,
                    }),
                });
            }

            BIrStmt::One => {
                // vope_one.clone()
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var("vope_one"))),
                });
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                // wire_a.clone() + wire_b.clone()  (uses Add impl on Vope)
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&name_a))),
                        right: Box::new(clone_expr(var(&name_b))),
                    }),
                });
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                // wire_a.clone() + vope_one.clone()  (XOR with public 1)
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&name_a))),
                        right: Box::new(clone_expr(var("vope_one"))),
                    }),
                });
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let hat_name = format!("hat_{}", and_counter);
                and_counter += 1;
                hat_names.push(hat_name.clone());
                emit_prover_and_gate(&name_a, &name_b, &let_name, &hat_name, &mut stmts);
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        }

        var_names.insert(result_id.0, let_name);
    }

    // Return (output_wire, [hat_0, hat_1, ...]).
    let (output_expr, _) = build_return(block, &var_names, vope_type());
    let hats_expr = IrExpr::FixedArray(hat_names.iter().map(|h| var(h)).collect());
    let ret_expr = IrExpr::Tuple(vec![output_expr, hats_expr]);

    let func = IrFunction {
        name: format!("vole_prove_{}", name),
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_vole_prover".into(),
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
// Verifier weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a VOLE **verifier** `IrModule`.
///
/// The generated function signature is:
/// ```text
/// fn vole_verify_<name><N: ArraySize, T>(
///     delta: &Delta<N, T>,
///     // One pair per AND gate (circuit order):
///     q_and_0: Q<N, T>, hat_0: Array<T, N>,
///     ...
///     // Verifier's VOLE shares for input wires:
///     q_input_0: Q<N, T>,
///     ...
/// ) -> (Q<N, T>, bool)
/// where
///     N: ArraySize,
///     T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default,
/// ```
///
/// For each AND gate, the verifier checks: `K_a · K_b + hat == K_c · Δ`.
/// The returned `bool` is `true` iff all AND gate checks passed.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_vole_verifier(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    assert!(
        circuit.is_circuit(),
        "weave_vole_verifier: circuit must satisfy is_circuit()"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let and_count = expanded
        .iter()
        .filter(|(_, s)| matches!(s, BIrStmt::And(..)))
        .count();

    let mut var_names = alloc::collections::BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("q_input_{}", i));
    }

    // Build parameter list.
    let mut params: Vec<IrParam> = Vec::new();

    // delta: &Delta<N, T>
    params.push(IrParam {
        name: "delta".into(),
        ty: ref_to_vole(delta_type()),
    });

    // Pairs (q_and_k, hat_k) for each AND gate.
    for k in 0..and_count {
        params.push(IrParam {
            name: format!("q_and_{}", k),
            ty: q_type(),
        });
        params.push(IrParam {
            name: format!("hat_{}", k),
            ty: array_t_n(),
        });
    }

    // Input wire Q shares.
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("q_input_{}", i),
            ty: q_type(),
        });
    }

    // Return type: (Q<N, T>, bool)
    let ret_type = IrType::Tuple(vec![
        q_type(),
        IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool),
    ]);

    let (generics, where_clause) = verifier_generics_and_where();

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut and_counter: usize = 0;

    // `let mut all_ok: bool = true;`
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident {
            mutable: true,
            name: "all_ok".into(),
            subpat: None,
        },
        ty: None,
        init: Some(IrExpr::Lit(volar_compiler::ir::IrLit::Bool(true))),
    });

    for (result_id, stmt) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        match stmt {
            BIrStmt::Zero => {
                // Q { q: Array::<T, N>::default() }
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(array_t_default())),
                });
            }

            BIrStmt::One => {
                // Q { q: delta.delta.clone() }
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(IrExpr::MethodCall {
                        receiver: Box::new(IrExpr::Field {
                            base: Box::new(var("delta")),
                            field: "delta".into(),
                        }),
                        method: MethodKind::Std("clone".into()),
                        type_args: vec![],
                        args: vec![],
                    })),
                });
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                // Q { q: Array::from_fn(|i| q_a.q[i].clone() + q_b.q[i].clone()) }
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(array_t_from_fn(
                        "i",
                        IrExpr::Binary {
                            op: SpecBinOp::Add,
                            left: Box::new(clone_expr(q_index(&name_a, "i"))),
                            right: Box::new(clone_expr(q_index(&name_b, "i"))),
                        },
                    ))),
                });
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                // Q { q: Array::from_fn(|i| q_a.q[i].clone() + delta.delta[i].clone()) }
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(array_t_from_fn(
                        "i",
                        IrExpr::Binary {
                            op: SpecBinOp::Add,
                            left: Box::new(clone_expr(q_index(&name_a, "i"))),
                            right: Box::new(clone_expr(delta_index("i"))),
                        },
                    ))),
                });
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let q_and_name = format!("q_and_{}", and_counter);
                let hat_name = format!("hat_{}", and_counter);
                let ok_name = format!("ok_{}", and_counter);
                and_counter += 1;
                emit_verifier_and_gate(
                    &name_a, &name_b, &let_name, &ok_name,
                    &q_and_name, &hat_name,
                    &mut stmts,
                );
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        }

        var_names.insert(result_id.0, let_name);
    }

    // Return (output_wire, all_ok).
    let (output_expr, _) = build_return(block, &var_names, q_type());
    let ret_expr = IrExpr::Tuple(vec![output_expr, var("all_ok")]);

    let func = IrFunction {
        name: format!("vole_verify_{}", name),
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_vole_verifier".into(),
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
// Bounded wrappers
// ============================================================================

/// Weave a bounded movfuscated Boolar circuit into a VOLE **prover**.
pub fn weave_vole_prover_bounded(
    circuit: &BIrBlocks,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_vole_prover(&lowered, name, linkage)
}

/// Weave a bounded movfuscated Boolar circuit into a VOLE **verifier**.
pub fn weave_vole_verifier_bounded(
    circuit: &BIrBlocks,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_vole_verifier(&lowered, name, linkage)
}

// ============================================================================
// Volar IR (IRBlocks) VOLE weaving — with authenticated storage
// ============================================================================
//
// # Soundness of VOLE-authenticated oblivious storage
//
// ## Setting
//
// Each wire w in the circuit carries bit value x_w authenticated by the VOLE
// relation K_w = M_w + x_w · Δ (over the extension field T).  The prover holds
// (x_w, M_w) as `Vope<N,T,U1>`; the verifier holds K_w as `Q<N,T>` and the
// global secret Δ as `Delta<N,T>`.
//
// Storage introduces mutable state: an array of cells indexed by a *dynamic*
// (circuit-computed) address.  Each cell holds one authenticated bit.  We
// implement reads and writes via **oblivious linear scan** (OLS) so that the
// proof reveals nothing about which address was accessed.
//
// ## Oblivious read: MUX-tree selection
//
// Instead of a linear scan with one-hot selectors (O(N × addr_width) ANDs),
// we use a **binary MUX tree** that recursively halves the candidate set:
//
// ```text
// mux_tree([c0..c3], [a0,a1]):
//   left  = MUX(a0, c0, c1)         // 1 AND
//   right = MUX(a0, c2, c3)         // 1 AND
//   result = MUX(a1, left, right)   // 1 AND
//   total: 3 = N − 1 ANDs  (vs  N × addr_width = 4 × 2 = 8 ANDs linear)
// ```
//
// At each level the most-significant remaining address bit selects between
// two subtrees.  MUX(sel, a, b) = sel·(a ⊕ b) ⊕ a  costs 1 AND + 2 free XOR.
// The full tree has N_pad − 1 MUX nodes (N_pad = next power of 2 ≥ N),
// each costing V AND gates for V-bit values.
//
//   **read cost:  (N_pad − 1) × V  AND gates**
//
// ## Oblivious write: demux-tree + per-cell MUX
//
// A **demux tree** produces N one-hot selectors from K address bits by
// iteratively splitting:
//
// ```text
// level 0: s[0]=NOT(a0), s[1]=a0              0 ANDs
// level 1: s[0..3] = AND each prev × {NOT(a1), a1}   4 ANDs
// level 2: ...                                        8 ANDs
//   total: 2N − 4  ANDs
// ```
//
// Each cell is then updated by a single MUX:  cell′ = MUX(sel, new, old).
//
//   **write cost:  (2N_pad − 4) + N × V  AND gates**
//
// ## Improvement over linear scan
//
// | N | K  | V | linear read | tree read | linear write | tree write |
// |---|----|---|-------------|-----------|--------------|------------|
// | 4 | 16 | 1 |    64       |     3     |     64       |      8     |
// | 16| 16 | 1 |   256       |    15     |    256       |     44     |
// |256|  8 | 1 |  2048       |   255     |   2048       |    764     |
//
// The tree is asymptotically O(N) regardless of address width K, whereas
// the linear scan is O(N·K).
//
// ## Why the prover cannot cheat
//
// 1. **VOLE binding**: every wire (including every storage cell and address
//    bit) is authenticated.  Changing the bit value x_w to x_w′ ≠ x_w
//    without updating M_w makes K_w ≠ M_w + x_w′ · Δ; the Quicksilver AND
//    check will detect the inconsistency with overwhelming probability in |T|.
//
// 2. **Gate correctness**: each AND in the oblivious scan is checked by the
//    standard Quicksilver relation.  A prover that substitutes a wrong
//    product produces a hat value that violates the verifier's check equation
//    K_a · K_b + hat ≟ K_c · Δ .
//
// 3. **Read integrity**: because the one-hot selector and the accumulation
//    are computed entirely with checked AND + free XOR, the result wire is
//    bound to the contents of the addressed cell.  Any attempt to return a
//    different value requires forging an AND, which is caught by (2).
//
// 4. **Write integrity**: the MUX updates every cell through a checked AND.
//    Skipping a cell (i.e., not executing its MUX) would require omitting
//    circuit gates, which would change the hat count and cause the verifier
//    to reject.  Forging the MUX AND is caught by (2).
//
// 5. **Storage persistence**: between unrolled loop iterations, the cell
//    wires produced by the previous iteration are carried forward as the
//    next iteration's initial state.  They remain VOLE-authenticated; the
//    same binding argument applies.
//
// 6. **Address privacy**: the verifier sees only Q shares and hats, which
//    are masked by random M values from the VOLE setup.  Under the
//    standard VOLE-ZK simulation argument, the verifier's view is
//    indistinguishable from a simulation that never sees the address.
//
// ## Cost
//
//   read  :  (N_pad − 1) × value_width  AND gates  (MUX tree)
//   write :  (2·N_pad − 4 + N × value_width)  AND gates  (demux + MUX)
//
// where N_pad = next power of 2 ≥ N.  Both are O(N), independent of K.
//
// ────────────────────────────────────────────────────────────────────────────

/// Maps `(StorageId.0, TypeId.0)` → cell count.
pub type StorageSizes = alloc::collections::BTreeMap<(u32, u32), usize>;

/// Wire representation inside the VOLE IR weaver.
#[derive(Clone)]
enum WireRepr {
    /// Single authenticated bit.
    Scalar(String),
    /// Vector of authenticated bits (produced by `Merge`).
    Vec(Vec<String>),
}

/// Width of a circuit type in bits (1 for Bit, K for Vec(K, Bit)).
fn cir_type_width(ty: &CirTyId, types: &CirTypes) -> usize {
    match &types.0[ty.0 as usize] {
        CircuitIrType::Primitive(PrimType::Bit) => 1,
        CircuitIrType::Vec(k, _inner) => *k,
        other => panic!("unsupported type in VOLE IR weaving: {:?}", other),
    }
}

/// Pre-scan a circuit block to count total AND gates (for sizing the hat
/// array / verifier Q-share parameters).
fn count_ir_ands(
    block: &CirBlock,
    types: &CirTypes,
    storage_sizes: &StorageSizes,
) -> usize {
    let mut var_types: Vec<CirTyId> = block.params.clone();
    let bit_tid = CirTyId(0); // by convention, index 0 = Bit
    let mut count: usize = 0;

    for stmt in &block.stmts {
        let result_ty: CirTyId = match stmt {
            Stmt::Const(_, ty) => ty.clone(),
            Stmt::Poly { coeffs, .. } => {
                for (mono, coeff) in coeffs {
                    if *coeff % 2 == 1 && mono.len() >= 2 {
                        count += mono.len() - 1;
                    }
                }
                bit_tid
            }
            Stmt::Merge { ty, .. } | Stmt::Splat { ty, .. }
            | Stmt::Rol { ty, .. } | Stmt::Ror { ty, .. }
            | Stmt::Shuffle { ty, .. } | Stmt::Transmute { dst_ty: ty, .. } => ty.clone(),
            Stmt::StorageRead { storage, ty, addr } => {
                let key = (storage.0, ty.0);
                let n = *storage_sizes.get(&key).unwrap_or(&0);
                let vw = cir_type_width(&ty, types);
                let n_pad = n.next_power_of_two();
                // MUX tree: (N_pad - 1) × V ANDs
                count += n_pad.saturating_sub(1) * vw;
                ty.clone()
            }
            Stmt::StorageWrite { storage, ty, addr, .. } => {
                let key = (storage.0, ty.0);
                let n = *storage_sizes.get(&key).unwrap_or(&0);
                let vw = cir_type_width(&ty, types);
                let n_pad = n.next_power_of_two();
                // Demux tree: 2·N_pad - 4 ANDs + per-cell MUX: N × V ANDs
                count += (2 * n_pad).saturating_sub(4) + n * vw;
                bit_tid
            }
            Stmt::Rng { ty } => ty.clone(),
            Stmt::OracleCall { result_ty, .. } | Stmt::ActionCall { result_ty, .. } => result_ty.clone(),
            Stmt::OracleOutput { ty, .. } | Stmt::ActionOutput { ty, .. } => ty.clone(),
        };
        var_types.push(result_ty);
    }
    count
}

/// Context for emitting VOLE-authenticated wire computations.
struct VoleIrCtx {
    stmts: Vec<IrStmt>,
    wires: alloc::collections::BTreeMap<u32, WireRepr>,
    and_counter: usize,
    hat_names: Vec<String>,
    ok_names: Vec<String>,
    /// Current var name for each storage cell.
    stor: alloc::collections::BTreeMap<(u32, u32, usize), String>,
    is_prover: bool,
}

impl VoleIrCtx {
    fn new(is_prover: bool) -> Self {
        VoleIrCtx {
            stmts: Vec::new(),
            wires: alloc::collections::BTreeMap::new(),
            and_counter: 0,
            hat_names: Vec::new(),
            ok_names: Vec::new(),
            stor: alloc::collections::BTreeMap::new(),
            is_prover,
        }
    }

    /// Get scalar wire name for a var id.
    fn scalar(&self, v: &CirVar) -> &str {
        match &self.wires[&v.0] {
            WireRepr::Scalar(s) => s,
            WireRepr::Vec(_) => panic!("expected scalar wire for v{}", v.0),
        }
    }

    /// Get vec wire names for a var id.
    fn vec_parts(&self, v: &CirVar) -> &[String] {
        match &self.wires[&v.0] {
            WireRepr::Vec(v) => v,
            WireRepr::Scalar(_) => panic!("expected vec wire"),
        }
    }

    // ---- Primitive wire operations ----------------------------------------

    /// Emit a zero-valued wire (prover: Vope::default, verifier: Q::default).
    fn emit_zero(&mut self, name: &str) {
        if self.is_prover {
            // Vope { u: Array::<Array<T,N>, U1>::default(), v: Array::<T,N>::default() }
            let u_default = IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["Array".into(), "default".into()],
                    type_args: vec![
                        IrType::Struct {
                            kind: StructKind::Custom("Array".into()),
                            type_args: vec![
                                IrType::TypeParam("T".into()),
                                IrType::TypeParam("N".into()),
                            ],
                        },
                        IrType::Struct {
                            kind: StructKind::Custom("U1".into()),
                            type_args: vec![],
                        },
                    ],
                }),
                args: vec![],
            };
            self.stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(name),
                ty: None,
                init: Some(IrExpr::StructExpr {
                    kind: StructKind::Custom("Vope".into()),
                    type_args: vec![],
                    fields: vec![
                        ("u".into(), u_default),
                        ("v".into(), array_t_default()),
                    ],
                    rest: None,
                }),
            });
        } else {
            self.stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(name),
                ty: None,
                init: Some(q_struct(array_t_default())),
            });
        }
    }

    /// Emit a one-valued wire (clone of the committed-one wire).
    fn emit_one(&mut self, name: &str) {
        let src = if self.is_prover { "vope_one" } else { "q_one" };
        self.stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(name),
            ty: None,
            init: Some(clone_expr(var(src))),
        });
    }

    /// Emit XOR (free: prover a + b, verifier element-wise).
    fn emit_xor(&mut self, out: &str, a: &str, b: &str) {
        if self.is_prover {
            self.stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(out),
                ty: None,
                init: Some(IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(clone_expr(var(a))),
                    right: Box::new(clone_expr(var(b))),
                }),
            });
        } else {
            // Q { q: Array::from_fn(|i| a.q[i].clone() + b.q[i].clone()) }
            self.stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(out),
                ty: None,
                init: Some(q_struct(array_t_from_fn(
                    "i",
                    IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(q_index(a, "i"))),
                        right: Box::new(clone_expr(q_index(b, "i"))),
                    },
                ))),
            });
        }
    }

    /// Emit AND gate.  Returns the name of the output wire.
    fn emit_and(&mut self, a: &str, b: &str) -> String {
        let wire_name = format!("and_w_{}", self.and_counter);
        if self.is_prover {
            let hat_name = format!("hat_{}", self.and_counter);
            self.hat_names.push(hat_name.clone());
            emit_prover_and_gate(a, b, &wire_name, &hat_name, &mut self.stmts);
        } else {
            let ok_name = format!("ok_{}", self.and_counter);
            let q_and_name = format!("q_and_{}", self.and_counter);
            let hat_name = format!("hat_{}", self.and_counter);
            self.ok_names.push(ok_name.clone());
            emit_verifier_and_gate(
                a, b, &wire_name, &ok_name,
                &q_and_name, &hat_name, &mut self.stmts,
            );
        }
        self.and_counter += 1;
        wire_name
    }

    /// Emit NOT (free: a + one).
    fn emit_not(&mut self, out: &str, a: &str) {
        let one = if self.is_prover { "vope_one" } else { "q_one" };
        self.emit_xor(out, a, one);
    }

    // ---- Poly (generalised gate) ------------------------------------------

    fn emit_poly(
        &mut self,
        out_name: &str,
        coeffs: &alloc::collections::BTreeMap<Vec<CirVar>, u8>,
        constant: &volar_ir::ir::Constant,
    ) {
        // Collect terms with odd coefficients.
        let mut term_names: Vec<String> = Vec::new();

        // Constant term.
        if constant.lo & 1 == 1 {
            let cname = format!("{}_cst", out_name);
            self.emit_one(&cname);
            term_names.push(cname);
        }

        for (mono, &coeff) in coeffs {
            if coeff % 2 == 0 { continue; }
            match mono.len() {
                0 => {
                    // degree-0 monomial with coeff 1 → another constant 1
                    let cname = format!("{}_c0", out_name);
                    self.emit_one(&cname);
                    term_names.push(cname);
                }
                1 => {
                    // degree-1: just the wire itself (clone)
                    term_names.push(self.scalar(&mono[0]).to_string());
                }
                _ => {
                    // degree ≥ 2: chain of ANDs
                    let mut acc = self.scalar(&mono[0]).to_string();
                    for k in 1..mono.len() {
                        let b = self.scalar(&mono[k]).to_string();
                        acc = self.emit_and(&acc, &b);
                    }
                    term_names.push(acc);
                }
            }
        }

        // XOR all terms together.
        match term_names.len() {
            0 => self.emit_zero(out_name),
            1 => {
                // Just clone the single term.
                self.stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(out_name),
                    ty: None,
                    init: Some(clone_expr(var(&term_names[0]))),
                });
            }
            _ => {
                let first = term_names[0].clone();
                let tmp0 = format!("{}_xor0", out_name);
                self.stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&tmp0),
                    ty: None,
                    init: Some(clone_expr(var(&first))),
                });
                let mut acc = tmp0;
                for (i, tn) in term_names[1..].iter().enumerate() {
                    let next = if i == term_names.len() - 2 {
                        out_name.to_string()
                    } else {
                        format!("{}_xor{}", out_name, i + 1)
                    };
                    self.emit_xor(&next, &acc, tn);
                    acc = next;
                }
            }
        }
    }

    // ---- Oblivious storage access (tree-based) ---------------------------

    /// Effective address width: ceil(log₂(cell_count)).
    fn effective_addr_width(cell_count: usize) -> usize {
        if cell_count <= 1 { return 0; }
        usize::BITS as usize - (cell_count - 1).leading_zeros() as usize
    }

    /// MUX(sel, when_0, when_1) = sel·(a⊕b) ⊕ a.  Cost: 1 AND + 2 free XOR.
    fn emit_mux(&mut self, sel: &str, when_0: &str, when_1: &str) -> String {
        let id = self.and_counter;
        let diff = format!("_mxd_{}", id);
        self.emit_xor(&diff, when_0, when_1);
        let masked = self.emit_and(sel, &diff);
        let out = format!("_mxr_{}", id);
        self.emit_xor(&out, &masked, when_0);
        out
    }

    /// MUX tree read for 1-bit values.
    ///
    /// Recursively halves the cell array using the MSB of `addr_bits`.
    /// Cost: (N_pad − 1) AND gates where N_pad = next_power_of_2(cells.len()).
    fn mux_tree_read(
        &mut self,
        cells: &[String],
        addr_bits: &[String],
        tag: &str,
    ) -> String {
        match cells.len() {
            0 => {
                let z = format!("_mtz_{}", tag);
                self.emit_zero(&z);
                z
            }
            1 => cells[0].clone(),
            _ => {
                let n_pad = cells.len().next_power_of_two();
                let mid = n_pad / 2;
                let left = if mid <= cells.len() { &cells[..mid] } else { cells };
                let right = if mid < cells.len() { &cells[mid..] } else { &[] as &[String] };
                let rest = &addr_bits[..addr_bits.len() - 1];
                let left_r = self.mux_tree_read(left, rest, &format!("{}l", tag));
                let right_r = self.mux_tree_read(right, rest, &format!("{}r", tag));
                let sel = &addr_bits[addr_bits.len() - 1];
                self.emit_mux(sel, &left_r, &right_r)
            }
        }
    }

    /// Demux tree: produce N_pad one-hot selectors from K address bits.
    ///
    /// The output vector is indexed by cell index: `result[i] = (addr == i)`.
    /// Cost: 2·N_pad − 4 AND gates (0 for N_pad ≤ 2).
    fn demux_tree(&mut self, addr_bits: &[String], tag: &str) -> Vec<String> {
        if addr_bits.is_empty() {
            let name = format!("_dm1_{}", tag);
            self.emit_one(&name);
            return vec![name];
        }
        // Base case: 1 bit → 2 selectors, 0 ANDs.
        let not_name = format!("_dmn_{}_{}", tag, 0);
        self.emit_not(&not_name, &addr_bits[0]);
        let mut sels = vec![not_name, addr_bits[0].clone()];

        // Iteratively expand: at each level j, double the selector count.
        for j in 1..addr_bits.len() {
            let bit = &addr_bits[j];
            let not_bit = format!("_dmn_{}_{}", tag, j);
            self.emit_not(&not_bit, bit);
            let prev = core::mem::take(&mut sels);
            let prev_len = prev.len();
            // First half: each prev × NOT(bit)  → a_j = 0
            for (k, p) in prev.iter().enumerate() {
                let s = self.emit_and(p, &not_bit);
                sels.push(s);
            }
            // Second half: each prev × bit  → a_j = 1
            for (k, p) in prev.iter().enumerate() {
                let s = self.emit_and(p, bit);
                sels.push(s);
            }
        }
        sels
    }

    /// Oblivious read via MUX tree.
    fn emit_storage_read(
        &mut self,
        out_name: &str,
        storage_id: u32,
        type_id: u32,
        addr_var: &CirVar,
        types: &CirTypes,
        val_ty: &CirTyId,
    ) {
        let cell_count = self.stor.keys()
            .filter(|(s, t, _)| *s == storage_id && *t == type_id)
            .count();
        let vw = cir_type_width(val_ty, types);
        if cell_count == 0 {
            if vw == 1 {
                self.emit_zero(out_name);
            } else {
                let parts: Vec<String> = (0..vw).map(|j| {
                    let n = format!("{}_z{}", out_name, j);
                    self.emit_zero(&n);
                    n
                }).collect();
                // placeholder — will be overwritten by wires.insert in caller
            }
            return;
        }

        let full_addr: Vec<String> = match &self.wires[&addr_var.0] {
            WireRepr::Scalar(s) => vec![s.clone()],
            WireRepr::Vec(v) => v.clone(),
        };
        let aw = Self::effective_addr_width(cell_count);
        let addr_bits: Vec<String> = full_addr[..aw].to_vec();

        // For each value-bit position, build a MUX tree over all cells.
        let mut result_bits: Vec<String> = Vec::with_capacity(vw);
        for vb in 0..vw {
            let cells: Vec<String> = (0..cell_count).map(|ci| {
                let cn = self.stor[&(storage_id, type_id, ci)].clone();
                if vw == 1 { cn } else { format!("{}_{}", cn, vb) }
            }).collect();
            let tag = format!("sr_{}_{}", out_name, vb);
            let r = self.mux_tree_read(&cells, &addr_bits, &tag);
            let bit_name = if vw == 1 {
                out_name.to_string()
            } else {
                format!("{}_{}", out_name, vb)
            };
            self.stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&bit_name),
                ty: None,
                init: Some(clone_expr(var(&r))),
            });
            result_bits.push(bit_name);
        }
    }

    /// Oblivious write via demux tree + per-cell MUX.
    fn emit_storage_write(
        &mut self,
        storage_id: u32,
        type_id: u32,
        src_var: &CirVar,
        addr_var: &CirVar,
        types: &CirTypes,
        val_ty: &CirTyId,
    ) {
        let cell_count = self.stor.keys()
            .filter(|(s, t, _)| *s == storage_id && *t == type_id)
            .count();
        if cell_count == 0 { return; }

        let full_addr: Vec<String> = match &self.wires[&addr_var.0] {
            WireRepr::Scalar(s) => vec![s.clone()],
            WireRepr::Vec(v) => v.clone(),
        };
        let aw = Self::effective_addr_width(cell_count);
        let addr_bits: Vec<String> = full_addr[..aw].to_vec();

        let vw = cir_type_width(val_ty, types);
        let src_bits: Vec<String> = if vw == 1 {
            vec![self.scalar(src_var).to_string()]
        } else {
            self.vec_parts(src_var).to_vec()
        };

        // Build one-hot selectors via demux tree.
        let tag = format!("sw_{}", self.and_counter);
        let sels = self.demux_tree(&addr_bits, &tag);

        // Per-cell MUX: cell' = MUX(sel[ci], src, old_cell)
        for ci in 0..cell_count {
            let sel = if ci < sels.len() { &sels[ci] } else {
                // Cells beyond the demux range are never addressed; skip.
                continue;
            };
            for vb in 0..vw {
                let old_cell = if vw == 1 {
                    self.stor[&(storage_id, type_id, ci)].clone()
                } else {
                    format!("{}_{}", self.stor[&(storage_id, type_id, ci)], vb)
                };
                let new_cell = self.emit_mux(sel, &old_cell, &src_bits[vb]);
                if vw == 1 {
                    self.stor.insert((storage_id, type_id, ci), new_cell);
                } else if vb == 0 {
                    self.stor.insert((storage_id, type_id, ci),
                        format!("_sc_{}_{}_{}", storage_id, ci, 0));
                }
            }
        }
    }

    // ---- Merge / Shuffle (structural, free) --------------------------------

    fn emit_merge(&mut self, out_id: u32, parts: &[CirVar]) {
        let names: Vec<String> = parts.iter().map(|v| self.scalar(v).to_string()).collect();
        self.wires.insert(out_id, WireRepr::Vec(names));
        // No runtime code emitted — purely a tracking operation.
    }

    fn emit_shuffle(&mut self, out_name: &str, out_id: u32, result_bits: &[(u8, CirVar)]) {
        if result_bits.len() == 1 {
            let (bit_idx, src_var) = &result_bits[0];
            let src_parts = self.vec_parts(src_var);
            let src_name = &src_parts[*bit_idx as usize];
            self.stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(out_name),
                ty: None,
                init: Some(clone_expr(var(src_name))),
            });
            self.wires.insert(out_id, WireRepr::Scalar(out_name.to_string()));
        } else {
            // Multi-bit shuffle → Vec result.
            let names: Vec<String> = result_bits.iter().enumerate().map(|(i, (bit_idx, src_var))| {
                let src_parts = self.vec_parts(src_var);
                let src_name = &src_parts[*bit_idx as usize];
                let n = format!("{}_{}", out_name, i);
                self.stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&n),
                    ty: None,
                    init: Some(clone_expr(var(src_name))),
                });
                n
            }).collect();
            self.wires.insert(out_id, WireRepr::Vec(names));
        }
    }

    // ---- Main dispatch -----------------------------------------------------

    fn emit_circuit(
        &mut self,
        block: &CirBlock,
        types: &CirTypes,
        storage_sizes: &StorageSizes,
    ) {
        let p = block.params.len();

        // Register input wires.
        for i in 0..p {
            let name = format!("w_{}", i);
            self.wires.insert(i as u32, WireRepr::Scalar(name));
        }

        // Initialize storage cells to zero.
        for (&(sid, tid), &count) in storage_sizes {
            for ci in 0..count {
                let name = format!("_sinit_{}_{}_{}", sid, tid, ci);
                self.emit_zero(&name);
                self.stor.insert((sid, tid, ci), name);
            }
        }

        // Process stmts.
        for (si, stmt) in block.stmts.iter().enumerate() {
            let var_id = (p + si) as u32;
            let out_name = format!("w_{}", var_id);

            match stmt {
                Stmt::Const(c, ty) => {
                    let w = cir_type_width(ty, types);
                    if w == 1 {
                        if c.lo & 1 == 1 { self.emit_one(&out_name); }
                        else { self.emit_zero(&out_name); }
                        self.wires.insert(var_id, WireRepr::Scalar(out_name));
                    } else {
                        let mut bits = Vec::with_capacity(w);
                        for j in 0..w {
                            let n = format!("{}_{}", out_name, j);
                            if (c.lo >> j) & 1 == 1 { self.emit_one(&n); }
                            else { self.emit_zero(&n); }
                            bits.push(n);
                        }
                        self.wires.insert(var_id, WireRepr::Vec(bits));
                    }
                }

                Stmt::Poly { coeffs, constant } => {
                    self.emit_poly(&out_name, coeffs, constant);
                    self.wires.insert(var_id, WireRepr::Scalar(out_name));
                }

                Stmt::Merge { parts, .. } => {
                    self.emit_merge(var_id, parts);
                }

                Stmt::Shuffle { result_bits, .. } => {
                    self.emit_shuffle(&out_name, var_id, result_bits);
                }

                Stmt::StorageRead { storage, ty, addr } => {
                    self.emit_storage_read(&out_name, storage.0, ty.0, addr, types, ty);
                    let w = cir_type_width(ty, types);
                    if w == 1 {
                        self.wires.insert(var_id, WireRepr::Scalar(out_name));
                    } else {
                        let bits: Vec<String> = (0..w).map(|j| format!("{}_{}", out_name, j)).collect();
                        self.wires.insert(var_id, WireRepr::Vec(bits));
                    }
                }

                Stmt::StorageWrite { storage, src, ty, addr } => {
                    self.emit_storage_write(storage.0, ty.0, src, addr, types, ty);
                    // Write produces no meaningful result; register a zero.
                    self.emit_zero(&out_name);
                    self.wires.insert(var_id, WireRepr::Scalar(out_name));
                }

                Stmt::Transmute { src, .. } => {
                    // Reinterpret — same bits, different type label.
                    self.wires.insert(var_id, self.wires[&src.0].clone());
                }

                Stmt::Splat { src, ty } => {
                    let w = cir_type_width(ty, types);
                    let s = self.scalar(src).to_string();
                    let bits: Vec<String> = (0..w).map(|j| {
                        let n = format!("{}_{}", out_name, j);
                        self.stmts.push(IrStmt::Let {
                            pattern: IrPattern::ident(&n),
                            ty: None,
                            init: Some(clone_expr(var(&s))),
                        });
                        n
                    }).collect();
                    self.wires.insert(var_id, WireRepr::Vec(bits));
                }

                Stmt::Rol { src, ty, n } => {
                    let parts = self.vec_parts(src).to_vec();
                    let w = parts.len();
                    let rotated: Vec<String> = (0..w).map(|j| parts[(j + w - *n) % w].clone()).collect();
                    self.wires.insert(var_id, WireRepr::Vec(rotated));
                }
                Stmt::Ror { src, ty, n } => {
                    let parts = self.vec_parts(src).to_vec();
                    let w = parts.len();
                    let rotated: Vec<String> = (0..w).map(|j| parts[(j + *n) % w].clone()).collect();
                    self.wires.insert(var_id, WireRepr::Vec(rotated));
                }

                Stmt::Rng { .. }
                | Stmt::OracleCall { .. } | Stmt::OracleOutput { .. }
                | Stmt::ActionCall { .. } | Stmt::ActionOutput { .. } => {
                    panic!("VOLE IR weaving does not yet support external primitives");
                }
            }
        }
    }
}

/// Weave a single-block Volar IR circuit into a VOLE **prover** `IrModule`.
///
/// The circuit must satisfy `is_circuit()` (single block, `Jmp(Return)`).
/// `storage_sizes` maps each `(StorageId.0, TypeId.0)` pair to the number of
/// cells in that oblivious storage space.
pub fn weave_vole_prover_ir(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    storage_sizes: &StorageSizes,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    assert!(circuit.is_circuit(), "weave_vole_prover_ir: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let and_count = count_ir_ands(block, types, storage_sizes);
    let (generics, where_clause) = prover_generics_and_where();

    // Build function params: vope_one, then one per circuit input.
    let mut params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    for i in 0..num_params {
        params.push(IrParam { name: format!("w_{}", i), ty: vope_type() });
    }

    // Return type: (Vope, [Array<T,N>; AND_COUNT])  or  ((Vope, ...), [...])
    let ret_type = IrType::Tuple(vec![vope_type(), hat_array_type(and_count)]);

    // Generate body.
    let mut ctx = VoleIrCtx::new(true);
    ctx.emit_circuit(block, types, storage_sizes);

    // Build return expr.
    let ret_args = match &block.terminator {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => args,
        _ => panic!("expected Jmp(Return)"),
    };
    let output_expr = if ret_args.len() == 1 {
        clone_expr(var(ctx.scalar(&ret_args[0])))
    } else {
        IrExpr::Tuple(ret_args.iter().map(|v| clone_expr(var(ctx.scalar(v)))).collect())
    };
    let hats_expr = IrExpr::FixedArray(ctx.hat_names.iter().map(|h| var(h)).collect());
    let ret_expr = IrExpr::Tuple(vec![output_expr, hats_expr]);

    let func = IrFunction {
        name: format!("vole_prove_ir_{}", name),
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_vole_ir_prover".into(),
        functions: vec![func],
        structs: vec![], traits: vec![], impls: vec![], type_aliases: vec![],
    };
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Weave a single-block Volar IR circuit into a VOLE **verifier** `IrModule`.
///
/// Analogous to [`weave_vole_prover_ir`] but generates the verifier side:
/// each AND gate consumes a pre-assigned `Q<N,T>` share and a prover-sent
/// `hat` value, checking the Quicksilver relation.
pub fn weave_vole_verifier_ir(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    storage_sizes: &StorageSizes,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    assert!(circuit.is_circuit(), "weave_vole_verifier_ir: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let and_count = count_ir_ands(block, types, storage_sizes);
    let (generics, where_clause) = verifier_generics_and_where();

    // Params: delta, (q_and_k, hat_k) pairs, q_one, input Q shares.
    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
    ];
    for k in 0..and_count {
        params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
        params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
    }
    // q_one: the verifier's Q for the committed-one wire.
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
    }

    let ret_type = IrType::Tuple(vec![
        q_type(),
        IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool),
    ]);

    // Generate body.
    let mut ctx = VoleIrCtx::new(false);

    // `let mut all_ok: bool = true;`
    ctx.stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(IrExpr::Lit(volar_compiler::ir::IrLit::Bool(true))),
    });

    ctx.emit_circuit(block, types, storage_sizes);

    // Build return.
    let ret_args = match &block.terminator {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => args,
        _ => panic!("expected Jmp(Return)"),
    };
    let output_expr = if ret_args.len() == 1 {
        clone_expr(var(ctx.scalar(&ret_args[0])))
    } else {
        IrExpr::Tuple(ret_args.iter().map(|v| clone_expr(var(ctx.scalar(v)))).collect())
    };
    let ret_expr = IrExpr::Tuple(vec![output_expr, var("all_ok")]);

    let func = IrFunction {
        name: format!("vole_verify_ir_{}", name),
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_vole_ir_verifier".into(),
        functions: vec![func],
        structs: vec![], traits: vec![], impls: vec![], type_aliases: vec![],
    };
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

// ============================================================================
// Printer
// ============================================================================

/// Render a weaved VOLE `IrModule` to Rust source.
///
/// The preamble brings in the VOLE AND gate primitives from
/// `volar_spec::vole::prove` — the implementation that was formerly
/// embedded as a raw string is now the authoritative spec.
pub fn print_weaved_vole_module(module: &IrModule) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut body = String::new();
    let _ = write!(body, "{}", DisplayRust(ModuleWriter { module }));

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
    use crate::tests_common::{build_xor_and_circuit, build_simple_loop, run_compile_check};

    #[test]
    fn test_weave_vole_prover_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_vole_prover(&circuit, "test_circuit", None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_prover");
    }

    #[test]
    fn test_weave_vole_verifier_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_vole_verifier(&circuit, "test_circuit", None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_verifier");
    }

    #[test]
    fn test_weave_vole_prover_bounded_compiles() {
        let circuit = build_simple_loop();
        let module = weave_vole_prover_bounded(&circuit, "loop_vole", 4, LoweringMode::Unconditional, None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_prover_bounded");
    }

    #[test]
    fn test_weave_vole_verifier_bounded_compiles() {
        let circuit = build_simple_loop();
        let module = weave_vole_verifier_bounded(&circuit, "loop_vole", 4, LoweringMode::Unconditional, None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_verifier_bounded");
    }

    #[test]
    fn test_vole_prover_returns_fixed_array() {
        let circuit = crate::tests_common::build_xor_and_circuit();
        let module = weave_vole_prover(&circuit, "test_circuit", None);
        let code = print_weaved_vole_module(&module);
        // xor_and has 1 AND gate → return type must be `[Array<T, N>; 1]`
        assert!(
            code.contains("[Array<T, N>; 1]"),
            "Expected fixed-size hat array in return type, got:\n{}",
            code
        );
        assert!(
            !code.contains("Vec<"),
            "Should not contain Vec in generated VOLE prover:\n{}",
            code
        );
    }

    // ---- Volar IR weaver tests ---------------------------------------------

    use volar_ir::ir::{
        IRBlocks, IRBlock as CirBlock, IRBlockTargetId, IRTerminator,
        IRTypes as CirTypes, IRVarId as CirVar, Stmt, StorageId,
    };
    use volar_ir::ir::{Constant as CirConst, IRType as CircuitIrType, PrimType as PrimTy, IRTypeId as CirTyId};

    /// Build a trivial single-block IR circuit: params=[Bit], return param[0].
    fn build_ir_identity_circuit() -> (IRBlocks, CirTypes) {
        let mut types = CirTypes::new();
        let bit = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let block = CirBlock {
            params: std::vec![bit],
            stmts: std::vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![CirVar(0)],
            },
        };
        (IRBlocks::new(std::vec![block]), types)
    }

    /// Build a circuit that ANDs two inputs: Poly { {[0,1]: 1}, 0 }.
    fn build_ir_and_circuit() -> (IRBlocks, CirTypes) {
        let mut types = CirTypes::new();
        let bit = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let mut coeffs = alloc::collections::BTreeMap::new();
        coeffs.insert(std::vec![CirVar(0), CirVar(1)], 1u8);
        let block = CirBlock {
            params: std::vec![bit, bit],
            stmts: std::vec![
                Stmt::Poly { coeffs, constant: CirConst { hi: 0, lo: 0 } },
            ],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![CirVar(2)],
            },
        };
        (IRBlocks::new(std::vec![block]), types)
    }

    /// Build a circuit with storage: write input to cell, read it back.
    fn build_ir_storage_circuit() -> (IRBlocks, CirTypes, StorageSizes) {
        let mut types = CirTypes::new();
        let bit = types.intern(CircuitIrType::Primitive(PrimTy::Bit)); // 0
        // The address is just a single bit (1-bit address, 2 cells).
        let block = CirBlock {
            params: std::vec![bit, bit], // param 0 = value, param 1 = addr
            stmts: std::vec![
                // stmt 0 (var 2): write value to storage
                Stmt::StorageWrite {
                    storage: StorageId(0),
                    src: CirVar(0),
                    ty: CirTyId(0),
                    addr: CirVar(1),
                },
                // stmt 1 (var 3): read back from storage at same address
                Stmt::StorageRead {
                    storage: StorageId(0),
                    ty: CirTyId(0),
                    addr: CirVar(1),
                },
            ],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: std::vec![CirVar(3)],
            },
        };
        let mut ss = StorageSizes::new();
        ss.insert((0, 0), 2); // StorageId(0), TypeId(0) → 2 cells
        (IRBlocks::new(std::vec![block]), types, ss)
    }

    #[test]
    fn test_weave_vole_ir_prover_identity() {
        let (circuit, types) = build_ir_identity_circuit();
        let ss = StorageSizes::new();
        let module = weave_vole_prover_ir(&circuit, &types, "identity", &ss, None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_ir_prover_id");
    }

    #[test]
    fn test_weave_vole_ir_verifier_identity() {
        let (circuit, types) = build_ir_identity_circuit();
        let ss = StorageSizes::new();
        let module = weave_vole_verifier_ir(&circuit, &types, "identity", &ss, None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_ir_verifier_id");
    }

    #[test]
    fn test_weave_vole_ir_prover_and() {
        let (circuit, types) = build_ir_and_circuit();
        let ss = StorageSizes::new();
        let module = weave_vole_prover_ir(&circuit, &types, "and_gate", &ss, None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_ir_prover_and");
    }

    #[test]
    fn test_weave_vole_ir_verifier_and() {
        let (circuit, types) = build_ir_and_circuit();
        let ss = StorageSizes::new();
        let module = weave_vole_verifier_ir(&circuit, &types, "and_gate", &ss, None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_ir_verifier_and");
    }

    #[test]
    fn test_weave_vole_ir_prover_storage() {
        let (circuit, types, ss) = build_ir_storage_circuit();
        let module = weave_vole_prover_ir(&circuit, &types, "storage", &ss, None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_ir_prover_stor");
    }

    #[test]
    fn test_weave_vole_ir_verifier_storage() {
        let (circuit, types, ss) = build_ir_storage_circuit();
        let module = weave_vole_verifier_ir(&circuit, &types, "storage", &ss, None);
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_ir_verifier_stor");
    }
}
