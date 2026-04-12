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
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        AssociatedType, IrBlock, IrExpr, IrFunction, IrGenericParam, IrGenericParamKind,
        IrModule, IrParam, IrPattern, IrStmt, IrTraitBound, IrType, IrWherePredicate,
        MethodKind, SpecBinOp, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::{
    boolar::{BIrBlocks, BIrStmt},
    lower_to_circuit::lower_to_circuit,
};
pub use volar_ir::lower_to_circuit::LoweringMode;

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
}
