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
        ExternalKind, IrBlock, IrExpr, IrExprKind, IrFunction, IrGenericParam,
        IrGenericParamKind, IrLit, IrModule, IrParam, IrPattern, IrStmt, IrStmtKind,
        IrTraitBound, IrType, MethodKind, SpecBinOp, StructKind, TraitKind,
        PrimitiveType, ArrayKind, ArrayLength,
    },
    linkage::LinkageSystem,
};
#[cfg(feature = "linking")]
use volar_compiler::linkage::LinkedSpec;
use volar_ir::boolar::{BIrBlocks, BIrStmt};
pub use volar_ir_passes::lower_to_circuit::LoweringMode;
use volar_discipline::{Tagged, Transparent};

use crate::{
    array_default, array_from_fn, base_index, build_return, clone_expr, expand_ors,
    ir_expr, ref_expr, var, NoProvenance, ProvenanceHandler,
};

/// Construct a fresh `IrStmt` with default provenance and no side.
fn ir_stmt<Q: Clone + Default>(kind: IrStmtKind<Q>) -> IrStmt<Q> {
    IrStmt::new(kind, Q::default(), None)
}

/// Construct an `IrStmt` carrying an explicit provenance value (no side).
fn ir_stmt_p<Q: Clone>(kind: IrStmtKind<Q>, prov: Q) -> IrStmt<Q> {
    IrStmt::new(kind, prov, None)
}

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

/// Build the color-bit extraction expr for an evaluator label: the LSB of the
/// label's first byte, `label.target[0] & 1`. This is the decode the
/// cleartext GRAM gadget applies to each action arg label (the same color-bit
/// rule `EvalSetup::recover_output` / `gram_decode_label` use).
fn gram_color_bit<Q: Clone + Default>(label_expr: IrExpr<Q>) -> IrExpr<Q> {
    ir_expr(IrExprKind::Binary {
        op: SpecBinOp::Ne,
        left: Box::new(ir_expr(IrExprKind::Binary {
            op: SpecBinOp::BitAnd,
            left: Box::new(ir_expr(IrExprKind::Index {
                base: Box::new(ir_expr(IrExprKind::Field {
                    base: Box::new(label_expr),
                    field: "target".into(),
                })),
                index: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(0)))),
            })),
            right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(1)))),
        })),
        right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(0)))),
    })
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

/// Generic params `<N: ArraySize, D: Digest>` (crate-visible for the MPC weavers).
pub(crate) fn generic_params_pub() -> Vec<IrGenericParam> {
    generic_params()
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
/// `N: ArraySize` only — for the GRAM action extern stubs, whose signature
/// mentions `N` (via `Eval<N>`) but not `D`.
fn generic_param_n() -> Vec<IrGenericParam> {
    vec![IrGenericParam {
        name: "N".into(),
        kind: IrGenericParamKind::Type,
        const_ty: None,
        bounds: vec![IrTraitBound {
            trait_kind: TraitKind::ArraySize,
            type_args: vec![],
            assoc_bindings: vec![],
        }],
        default: None,
    }]
}

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
fn garble_struct<P: Clone>(base_expr: IrExpr<P>) -> IrExpr<P> {
    let prov = base_expr.prov.clone();
    let side = base_expr.side;
    IrExpr::new(
        IrExprKind::StructExpr {
            kind: StructKind::Custom("Garble".into()),
            type_args: vec![],
            fields: vec![("base".into(), base_expr)],
            rest: None,
        },
        prov,
        side,
    )
}

// ============================================================================
// Garbled-action configuration (GRAM sub-protocol gadgets)
// ============================================================================

/// How the garble weaver should lower a boolar `ActionCall` for a named
/// action — the GRAM sub-protocol gadget boundary (see `MPC_PLAN.md`
/// workstream A).
///
/// The boolar `ActionCall { name, guard, args, fallback, num_bits }` produces
/// a call-handle var whose bits are projected with `ActionBit { call, bit }`.
/// For an ORAM access these actions are `begin` / `process` / `evict`.
///
/// This is the garbled-circuit mirror of the FHE weaver's `FheActionConfig`,
/// but with a GC-specific output mode: an action result bit is either
/// **cleartext** (the evaluator decodes it and learns the value — used for
/// data-independent values like a Path-ORAM leaf index) or **re-garbled**
/// (the host re-encodes it to a fresh label so the evaluator gets back a
/// label it cannot read).
#[derive(Clone, Debug)]
pub struct GramActionConfig {
    /// Per-output-bit mode. `output_cleartext[i] = true` means result bit `i`
    /// is returned to the evaluator as cleartext; `false` means it is
    /// re-garbled to a fresh label. An empty vec means *all* cleartext — the
    /// increment-1 cleartext-read gadget (begin / tree path read).
    pub output_cleartext: Vec<bool>,
}

impl GramActionConfig {
    /// `true` if every output bit is cleartext (the increment-1 gadget).
    pub fn all_cleartext(&self) -> bool {
        self.output_cleartext.iter().all(|&c| c)
    }
    /// `true` if output bit `i` is cleartext. Empty vec ⇒ all cleartext.
    ///
    /// Maps to the shared volar-spec [`GramOutput`] vocabulary
    /// (`volar_spec::garble::GramOutput`): cleartext ⇒ `Cleartext`, re-garbled
    /// ⇒ `Regarble`. The weaver deliberately does not depend on volar-spec, so
    /// the conversion lives in the ORAM glue driver (increment 3), which
    /// consumes this config to drive the host.
    pub fn is_output_cleartext(&self, i: usize) -> bool {
        self.output_cleartext.is_empty() || self.output_cleartext.get(i).copied().unwrap_or(false)
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
pub fn weave_evaluator<P: Clone>(circuit: &BIrBlocks<P>, name: &str, linkage: Option<&LinkageSystem>) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut tagged = weave_evaluator_with_handler(circuit, name, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(tagged.inner_mut()); }
    tagged
}

/// Weave a single-block boolean circuit into a garbled-circuit **evaluator** `IrModule`,
/// using `handler` to map input provenance into the output IR.
///
/// Linkage spec injection is not performed here — use the backwards-compatible
/// [`weave_evaluator`] wrapper or call [`LinkageSystem::apply_converting`] after
/// weaving for custom-provenance targets.
pub fn weave_evaluator_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
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
    let mut and_counter: usize = 0;

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        let init_expr = match stmt {
            BIrStmt::Zero => ir_expr(IrExprKind::StructExpr {
                kind: StructKind::Custom("Eval".into()),
                type_args: vec![],
                fields: vec![("target".into(), array_default())],
                rest: None,
            }),

            BIrStmt::One => clone_expr(var("one_wire")),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var(&name_b))),
                })
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_name = format!("and_table_{}", and_counter);
                and_counter += 1;
                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Other("and_via_table".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![
                        ref_expr(clone_expr(var(&name_b))),
                        var(&table_name),
                    ],
                })
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var("one_wire"))),
                })
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
            _ => unimplemented!("garble weaver: unhandled BIrStmt variant — add support for this variant"),
        };

        stmts.push(ir_stmt_p(IrStmtKind::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        }, q));
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let func = IrFunction { no_inline: false,
        name: name.into(),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "weaved".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    Tagged::seal(module)
}

/// Config-carrying evaluator weaver: like [`weave_evaluator_with_handler`],
/// but additionally handles `BIrStmt::ActionCall` / `BIrStmt::ActionBit` for
/// the actions named in `configs` (the GRAM access gadget — `MPC_PLAN.md`
/// workstream A, increments 1–2).
///
/// # Output modes (increments 1 and 2)
///
/// [`GramActionConfig::output_cleartext`] records, per output bit, whether the
/// bit is **cleartext** (data-independent — the host may learn it, e.g. a
/// Path-ORAM leaf index) or **re-garbled** (secret — e.g. bucket data). This
/// is the *host contract*: the host decodes only the cleartext bits and
/// re-encodes every result bit to a fresh [`GramOutput::Regarble`] label.
///
/// At the **evaluator**, every action result bit flows as an `Eval<N>` label —
/// the host re-garbles all of them, so the evaluator cannot read any. The
/// evaluator-side code path is therefore *identical* for cleartext and
/// re-garbled outputs: decode each arg label via its color bit, call the host
/// extern `fn(guard, &[bool]) -> Vec<Eval<N>>`, and project result labels with
/// `ActionBit`. `output_cleartext` does not change the evaluator's dataflow —
/// it constrains what the host is permitted to learn, and is checked by the
/// ORAM glue driver / conformance test, not here.
///
/// The host call is emitted as an extern fn stub named after the action,
/// resolved at link time against the ORAM host: `fn(guard: bool, args:
/// &[bool]) -> Vec<Eval<N>>`. The `guard` selects real result vs `fallback`
/// in the host; the evaluator passes the decoded guard.
///
/// # Panics
/// - if the circuit is not `is_circuit()`;
/// - if an `ActionCall`/`ActionBit` names an action not in `configs`.
pub fn weave_evaluator_with_gram<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    handler: &H,
    configs: &[(&str, GramActionConfig)],
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    assert!(
        circuit.is_circuit(),
        "weave_evaluator_with_gram: circuit must satisfy is_circuit()"
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

    let bool_ty = IrType::Primitive(PrimitiveType::Bool);
    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut and_counter: usize = 0;
    // ActionCall handle var → its cleartext result-bits binding name.
    let mut action_results: alloc::collections::BTreeMap<u32, String> =
        alloc::collections::BTreeMap::new();
    // Extern action stubs to append (deduped by name).
    let mut action_stubs: alloc::collections::BTreeMap<String, (usize, usize)> =
        alloc::collections::BTreeMap::new();

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        let init_expr = match stmt {
            BIrStmt::Zero => ir_expr(IrExprKind::StructExpr {
                kind: StructKind::Custom("Eval".into()),
                type_args: vec![],
                fields: vec![("target".into(), array_default())],
                rest: None,
            }),

            BIrStmt::One => clone_expr(var("one_wire")),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var(&name_b))),
                })
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_name = format!("and_table_{}", and_counter);
                and_counter += 1;
                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Other("and_via_table".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![ref_expr(clone_expr(var(&name_b))), var(&table_name)],
                })
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var("one_wire"))),
                })
            }

            // GRAM action gadget: decode the guard + arg labels to plaintext
            // bits, call the host action, bind the returned result labels for
            // ActionBit to project. Every result bit flows as `Eval<N>` — the
            // host re-garbles all of them regardless of `output_cleartext`.
            BIrStmt::ActionCall {
                name: action_name,
                guard,
                args,
                fallback: _,
                num_bits,
            } => {
                let _cfg = configs
                    .iter()
                    .find(|(n, _)| n == action_name)
                    .map(|(_, c)| c)
                    .unwrap_or_else(|| {
                        panic!("weave_evaluator_with_gram: unconfigured action '{action_name}'")
                    });

                // Decode the guard label to its color bit (LSB of the label).
                // For the cleartext-read gadget the guard is data-independent,
                // so its value is the LSB of the label's first byte (the same
                // color-bit rule `gram_decode_label` applies against a base).
                let guard_name = var_names[&guard.0].clone();
                let guard_bit = format!("{}_guard_bit", let_name);
                stmts.push(ir_stmt_p(
                    IrStmtKind::Let {
                        pattern: IrPattern::ident(&guard_bit),
                        ty: None,
                        init: Some(gram_color_bit(clone_expr(var(&guard_name)))),
                    },
                    q.clone(),
                ));

                // Decode each arg label to its color bit.
                let mut arg_bit_exprs: Vec<IrExpr<_>> = Vec::new();
                for arg in args {
                    let arg_name = var_names[&arg.0].clone();
                    arg_bit_exprs.push(gram_color_bit(clone_expr(var(&arg_name))));
                }

                // Host call: action_name(guard_bit, &[arg_bits...]) -> Vec<Eval<N>>.
                // The host decodes the plaintext args, runs the ORAM client, and
                // returns each result bit re-garbled to a fresh `Eval<N>` label it
                // shares with the evaluator (the evaluator cannot re-garble itself).
                let call = ir_expr(IrExprKind::Call {
                    func: Box::new(ir_expr(IrExprKind::Path {
                        segments: vec![action_name.clone()],
                        type_args: vec![IrType::TypeParam("N".into())],
                    })),
                    args: vec![var(&guard_bit), ref_expr(ir_expr(IrExprKind::Array(arg_bit_exprs)))],
        });
                action_results.insert(result_id.0, let_name.clone());
                action_stubs
                    .entry(action_name.clone())
                    .or_insert((args.len(), *num_bits));
                call
            }

            // Project one result label from the call's returned Vec<Eval<N>>.
            BIrStmt::ActionBit { call, bit } => {
                let result_binding = action_results.get(&call.0).unwrap_or_else(|| {
                    panic!("weave_evaluator_with_gram: ActionBit on unknown call var")
                });
                // Index then clone: Vec<Eval<N>> indexing yields &Eval<N>, and the
                // wire binding owns its Eval<N>.
                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(ir_expr(IrExprKind::Index {
                        base: Box::new(clone_expr(var(result_binding))),
                        index: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(*bit as i128)))),
                    })),
                    method: MethodKind::from_str("clone"),
                    type_args: vec![],
                    args: vec![],
                })
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::Rng { .. }
            | BIrStmt::StorageRead { .. }
            | BIrStmt::StorageWrite { .. } => {
                unimplemented!("garble weaver: extended BIrStmt variants not supported")
            }
            _ => unimplemented!(
                "garble weaver: unhandled BIrStmt variant — add support for this variant"
            ),
        };

        stmts.push(ir_stmt_p(
            IrStmtKind::Let {
                pattern: IrPattern::ident(&let_name),
                ty: None,
                init: Some(init_expr),
            },
            q,
        ));
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let mut functions = vec![IrFunction {
        no_inline: false,
        name: name.into(),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    }];

    // Emit one `ExternalKind::Action` extern stub per configured action,
    // resolved at link time against the ORAM host.
    for (action_name, (num_args, num_bits)) in action_stubs {
        let stub = IrFunction {
            no_inline: false,
            name: action_name.clone(),
            module_path: vec![],
            generics: generic_param_n(),
            receiver: None,
            params: vec![
                IrParam {
                    name: "guard".into(),
                    ty: bool_ty.clone(),
                },
                IrParam {
                    name: "args".into(),
                    ty: crate::ref_to(IrType::Array {
                        kind: ArrayKind::Slice,
                        elem: Box::new(bool_ty.clone()),
                        len: ArrayLength::Const(num_args),
                    }),
                },
            ],
            return_type: Some(IrType::Struct {
                kind: StructKind::Custom("Vec".into()),
                type_args: vec![eval_type()],
            }),
            where_clause: vec![],
            body: IrBlock {
                stmts: vec![],
                expr: Some(Box::new(ir_expr(IrExprKind::Unreachable))),
            },
            // Plain extern (resolved at link time against the ORAM host), not
            // the FHE `#[volar_action]` proc-macro dispatch — the GRAM host ABI
            // is a plain fn(guard, args) -> Vec<Eval<N>>.
            external_kind: ExternalKind::Normal,
        };
        let _ = num_bits;
        functions.push(stub);
    }

    let module = IrModule {
        name: "weaved_gram_evaluator".into(),
        functions,
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    Tagged::seal(module)
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
pub fn weave_garbler<P: Clone>(circuit: &BIrBlocks<P>, name: &str, linkage: Option<&LinkageSystem>) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut tagged = weave_garbler_with_handler(circuit, name, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(tagged.inner_mut()); }
    tagged
}

/// Weave a single-block boolean circuit into a garbled-circuit **garbler** `IrModule`,
/// using `handler` to map input provenance into the output IR.
pub fn weave_garbler_with_handler<P, H>(circuit: &BIrBlocks<P>, name: &str, handler: &H) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
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
                    ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(base_index(&name_b, "j")),
                    }),
                ))
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(ir_expr(IrExprKind::Index {
                            base: Box::new(ir_expr(IrExprKind::MethodCall {
                                receiver: Box::new(var("secret")),
                                method: MethodKind::Other("secret".into()),
                                type_args: vec![],
                                args: vec![],
                            })),
                            index: Box::new(var("j")),
                        })),
                    }),
                ))
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_var = format!("table_{}", table_counter);
                table_counter += 1;
                table_names.push(table_var.clone());

                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&table_var),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::MethodCall {
                        receiver: Box::new(var("secret")),
                        method: MethodKind::Other("gen_and_table".into()),
                        type_args: vec![IrType::TypeParam("D".into())],
                        args: vec![
                            ref_expr(clone_expr(var(&name_a))),
                            ref_expr(clone_expr(var(&name_b))),
                        ],
                    })),
                }, q.clone()));

                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(var(&name_a)),
                    method: MethodKind::Other("and_result".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![ref_expr(var(&name_b))],
                })
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
            _ => unimplemented!("garble weaver: unhandled BIrStmt variant — add support for this variant"),
        };

        stmts.push(ir_stmt_p(IrStmtKind::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(garble_expr),
        }, q));
        var_names.insert(result_id.0, let_name);
    }

    let (output_garble_expr, _) = build_return(block, &var_names, garble_type());
    let tables_expr = ir_expr(IrExprKind::FixedArray(table_names.iter().map(|t| var(t)).collect()));
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![tables_expr, output_garble_expr]));

    let func = IrFunction { no_inline: false,
        name: format!("{}_garble", name),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "weaved_garbler".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    Tagged::seal(module)
}

// ============================================================================
// GRAM garbler (config-carrying)
// ============================================================================

/// Weave a single-block boolean circuit into a function `{name}_garble` that
/// computes the garbler's side of a circuit containing GRAM action calls.
///
/// Mirrors [`weave_garbler_with_handler`]: the function takes `secret` and
/// per-input [`Garble`] false-labels, computes each wire's false-label base,
/// generates the AND-gate tables, and returns `([GarbleTable<N>; A],
/// Garble<N>)`. The GRAM extension handles `BIrStmt::ActionCall` /
/// `ActionBit`:
///
/// - **`ActionCall { guard, args, .. }`** binds a placeholder handle and
///   records the call's guard and argument base var names for its bits.
/// - **`ActionBit { call, bit }`** derives that result wire's false-label
///   base as `Garble::action_result_base::<D>(guard_base, arg_bases, bit)` —
///   the *same* deterministic derivation the evaluator-side host shim uses
///   for its `base_for` supply. Because both sides compute the base as a
///   pure function of the (already-tracked) guard/arg bases, the host's
///   re-garbled label and the garbler's downstream AND tables agree with no
///   extra communication.
///
/// The garbler does *not* execute the action — the ORAM client runs
/// evaluator-side (the `OramHost` / `GramOramHost`). The garbler only
/// produces the result-wire base supply that pins the re-garble target.
///
/// `configs` maps action name → [`GramActionConfig`]; an action not present
/// is a panic (mirrors the evaluator side).
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`, or on an
/// unconfigured action.
pub fn weave_garbler_with_gram<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    handler: &H,
    configs: &[(&str, GramActionConfig)],
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    assert!(
        circuit.is_circuit(),
        "weave_garbler_with_gram: circuit must satisfy is_circuit() (single block with Return terminator)"
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
    let mut table_counter: usize = 0;
    let mut table_names: Vec<String> = Vec::new();
    // ActionCall handle var → (guard base var, arg base vars). The garbler
    // binds no wire for the call itself; ActionBit derives each bit's base.
    let mut action_bases: alloc::collections::BTreeMap<u32, (String, Vec<String>)> =
        alloc::collections::BTreeMap::new();

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
                    ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(base_index(&name_b, "j")),
                    }),
                ))
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(ir_expr(IrExprKind::Index {
                            base: Box::new(ir_expr(IrExprKind::MethodCall {
                                receiver: Box::new(var("secret")),
                                method: MethodKind::Other("secret".into()),
                                type_args: vec![],
                                args: vec![],
                            })),
                            index: Box::new(var("j")),
                        })),
                    }),
                ))
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_var = format!("table_{}", table_counter);
                table_counter += 1;
                table_names.push(table_var.clone());

                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&table_var),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::MethodCall {
                        receiver: Box::new(var("secret")),
                        method: MethodKind::Other("gen_and_table".into()),
                        type_args: vec![IrType::TypeParam("D".into())],
                        args: vec![
                            ref_expr(clone_expr(var(&name_a))),
                            ref_expr(clone_expr(var(&name_b))),
                        ],
                    })),
                }, q.clone()));

                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(var(&name_a)),
                    method: MethodKind::Other("and_result".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![ref_expr(var(&name_b))],
                })
            }

            // GRAM action: record the guard + arg base var names for its
            // result bits; bind a unit placeholder for the call handle.
            BIrStmt::ActionCall {
                name: action_name,
                guard,
                args,
                fallback: _,
                num_bits,
            } => {
                let _cfg = configs
                    .iter()
                    .find(|(n, _)| n == action_name)
                    .map(|(_, c)| c)
                    .unwrap_or_else(|| {
                        panic!("weave_garbler_with_gram: unconfigured action '{action_name}'")
                    });
                let guard_name = var_names[&guard.0].clone();
                let arg_names: Vec<String> =
                    args.iter().map(|a| var_names[&a.0].clone()).collect();
                action_bases.insert(result_id.0, (guard_name, arg_names));
                // The call handle binds a unit value; its result bits are
                // derived by ActionBit.
                ir_expr(IrExprKind::Tuple(vec![]))
            }

            // GRAM action bit: derive this result wire's false-label base
            // from the call's guard/arg bases (the same derivation the
            // evaluator-side shim uses for base_for).
            BIrStmt::ActionBit { call, bit } => {
                let (guard_name, arg_names) = action_bases
                    .get(&call.0)
                    .unwrap_or_else(|| {
                        panic!("weave_garbler_with_gram: ActionBit references non-action call")
                    })
                    .clone();
                let args_array = ir_expr(IrExprKind::FixedArray(
                    arg_names
                        .iter()
                        .map(|n| ref_expr(var(n)))
                        .collect(),
                ));
                // guard.action_result_base::<D>(&[&arg...], bit) -> Garble<N>
                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(var(&guard_name)),
                    method: MethodKind::Other("action_result_base".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![
                        ref_expr(args_array),
                        ir_expr(IrExprKind::Lit(IrLit::Int(*bit as i128))),
                    ],
                })
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::Rng { .. }
            | BIrStmt::StorageRead { .. }
            | BIrStmt::StorageWrite { .. } => {
                unimplemented!("garble weaver: extended BIrStmt variants not supported")
            }
            _ => unimplemented!("garble weaver: unhandled BIrStmt variant — add support for this variant"),
        };

        stmts.push(ir_stmt_p(IrStmtKind::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(garble_expr),
        }, q));
        var_names.insert(result_id.0, let_name);
    }

    let (output_garble_expr, _) = build_return(block, &var_names, garble_type());
    let tables_expr = ir_expr(IrExprKind::FixedArray(table_names.iter().map(|t| var(t)).collect()));
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![tables_expr, output_garble_expr]));

    let func = IrFunction { no_inline: false,
        name: format!("{}_garble", name),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "weaved_garbler".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    Tagged::seal(module)
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
pub fn weave_into_gc<P: Clone>(circuit: &BIrBlocks<P>, name: &str, linkage: Option<&LinkageSystem>) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut tagged = weave_into_gc_with_handler(circuit, name, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(tagged.inner_mut()); }
    tagged
}

/// Weave a single-block boolean circuit into a `GarbledCircuit`-returning function,
/// using `handler` to map input provenance into the output IR.
pub fn weave_into_gc_with_handler<P, H>(circuit: &BIrBlocks<P>, name: &str, handler: &H) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
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
                    ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(base_index(&name_b, "j")),
                    }),
                ))
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                garble_struct(array_from_fn(
                    "j",
                    ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::BitXor,
                        left: Box::new(base_index(&name_a, "j")),
                        right: Box::new(ir_expr(IrExprKind::Index {
                            base: Box::new(ir_expr(IrExprKind::MethodCall {
                                receiver: Box::new(var("secret")),
                                method: MethodKind::Other("secret".into()),
                                type_args: vec![],
                                args: vec![],
                            })),
                            index: Box::new(var("j")),
                        })),
                    }),
                ))
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_var = format!("table_{}", table_counter);
                table_counter += 1;

                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&table_var),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::MethodCall {
                        receiver: Box::new(var("secret")),
                        method: MethodKind::Other("gen_and_table".into()),
                        type_args: vec![IrType::TypeParam("D".into())],
                        args: vec![
                            ref_expr(clone_expr(var(&name_a))),
                            ref_expr(clone_expr(var(&name_b))),
                        ],
                    })),
                }, q.clone()));

                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Other("and_result".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![ref_expr(var(&name_b))],
                })
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
            _ => unimplemented!("garble weaver: unhandled BIrStmt variant — add support for this variant"),
        };

        stmts.push(ir_stmt_p(IrStmtKind::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(garble_expr),
        }, q));
        var_names.insert(result_id.0, let_name);
    }

    let input_labels_expr = ir_expr(IrExprKind::FixedArray(
        (0..num_params).map(|i| var(&format!("input_{}", i))).collect(),
    ));
    let tables_expr = ir_expr(IrExprKind::FixedArray(
        (0..and_count).map(|k| var(&format!("table_{}", k))).collect(),
    ));

    let (output_garble_expr, _) = build_return(block, &var_names, garble_type());
    let ret_expr = ir_expr(IrExprKind::StructExpr {
        kind: StructKind::Custom("GarbledCircuit".into()),
        type_args: vec![],
        fields: vec![
            ("secret".into(), var("secret")),
            ("input_labels".into(), input_labels_expr),
            ("tables".into(), tables_expr),
            ("output_label".into(), output_garble_expr),
        ],
        rest: None,
    });

    let func = IrFunction { no_inline: false,
        name: format!("{}_into_gc", name),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(garbled_circuit_type(num_params, and_count)),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "weaved_into_gc".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    Tagged::seal(module)
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
pub fn weave_eval_from_setup<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut tagged = weave_eval_from_setup_with_handler(circuit, name, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(tagged.inner_mut()); }
    tagged
}

/// Weave a single-block boolean circuit into an EvalSetup-based evaluator,
/// using `handler` to map input provenance into the output IR.
pub fn weave_eval_from_setup_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
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

    let setup_one_wire = || -> IrExpr<H::Output> { ir_expr(IrExprKind::Field {
        base: Box::new(var("setup")),
        field: "one_wire".into(),
    }) };

    let setup_table = |k: usize| -> IrExpr<H::Output> { ir_expr(IrExprKind::Index {
        base: Box::new(ir_expr(IrExprKind::Field {
            base: Box::new(var("setup")),
            field: "tables".into(),
        })),
        index: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(k as i128)))),
    }) };

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut and_counter: usize = 0;

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        let init_expr = match stmt {
            BIrStmt::Zero => ir_expr(IrExprKind::StructExpr {
                kind: StructKind::Custom("Eval".into()),
                type_args: vec![],
                fields: vec![("target".into(), array_default())],
                rest: None,
            }),

            BIrStmt::One => clone_expr(setup_one_wire()),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var(&name_b))),
                })
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let k = and_counter;
                and_counter += 1;
                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Other("and_via_table".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![
                        ref_expr(clone_expr(var(&name_b))),
                        ref_expr(setup_table(k)),
                    ],
                })
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(setup_one_wire())),
                })
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
            _ => unimplemented!("garble weaver: unhandled BIrStmt variant — add support for this variant"),
        };

        stmts.push(ir_stmt_p(IrStmtKind::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        }, q));
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let func = IrFunction { no_inline: false,
        name: format!("{}_eval_from_setup", name),
        module_path: vec![],
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "weaved_eval_from_setup".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    Tagged::seal(module)
}

// ============================================================================
// Bounded (movfuscated) weaving wrappers
// ============================================================================

/// Backwards-compatible bounded evaluator weave — discards provenance.
pub fn weave_evaluator_bounded<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut module = weave_evaluator_bounded_with_handler(circuit, name, limit, mode, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(module.inner_mut()); }
    module
}

/// Bounded evaluator weave with provenance handler.
pub fn weave_evaluator_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    use volar_ir_passes::lower_to_circuit::lower_to_circuit;
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_evaluator_with_handler(&lowered, name, handler)
}

/// Backwards-compatible bounded garbler weave — discards provenance.
pub fn weave_garbler_bounded<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut module = weave_garbler_bounded_with_handler(circuit, name, limit, mode, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(module.inner_mut()); }
    module
}

/// Bounded garbler weave with provenance handler.
pub fn weave_garbler_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    use volar_ir_passes::lower_to_circuit::lower_to_circuit;
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_garbler_with_handler(&lowered, name, handler)
}

/// Backwards-compatible bounded GarbledCircuit weave — discards provenance.
pub fn weave_into_gc_bounded<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut module = weave_into_gc_bounded_with_handler(circuit, name, limit, mode, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(module.inner_mut()); }
    module
}

/// Bounded GarbledCircuit weave with provenance handler.
pub fn weave_into_gc_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    use volar_ir_passes::lower_to_circuit::lower_to_circuit;
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_into_gc_with_handler(&lowered, name, handler)
}

/// Backwards-compatible bounded EvalSetup weave — discards provenance.
pub fn weave_eval_from_setup_bounded<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut module = weave_eval_from_setup_bounded_with_handler(circuit, name, limit, mode, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(module.inner_mut()); }
    module
}

/// Bounded EvalSetup weave with provenance handler.
pub fn weave_eval_from_setup_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    use volar_ir_passes::lower_to_circuit::lower_to_circuit;
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_eval_from_setup_with_handler(&lowered, name, handler)
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

    /// Circuit with one all-cleartext action call: out = action_bit(begin(in0), 0) XOR in1.
    /// begin is a 1-arg, 1-output-bit cleartext-read gadget (the ORAM leaf lookup shape).
    fn build_action_circuit() -> BIrBlocks {
        use volar_ir::boolar::BIrStmt;
        use volar_ir::ir::IRVarId;
        use volar_ir_common::Node;
        BIrBlocks { blocks: vec![volar_ir::boolar::BIrBlock {
            params: 2,
            stmts: vec![
                // wire_2 = ActionCall begin(guard=in0, args=[in0]) -> 1 result bit
                Node::new(BIrStmt::ActionCall {
                    name: "begin".into(),
                    guard: IRVarId(0),
                    args: vec![IRVarId(0)],
                    fallback: vec![IRVarId(1)],
                    num_bits: 1,
                }, (), None),
                // wire_3 = ActionBit(call=wire_2, bit=0)
                Node::new(BIrStmt::ActionBit { call: IRVarId(2), bit: 0 }, (), None),
                // wire_4 = Xor(wire_3, in1)
                Node::new(BIrStmt::Xor(IRVarId(3), IRVarId(1)), (), None),
            ],
            terminator: volar_ir::boolar::BIrTerminator::Jmp(volar_ir::boolar::BIrTarget {
                block: volar_ir::ir::IRBlockTargetId::Return,
                args: vec![IRVarId(4)],
            }),
        }], pre_init: vec![] }
    }

    #[test]
    fn test_weave_evaluator_with_gram_compiles() {
        let circuit = build_action_circuit();
        let configs = [("begin", GramActionConfig { output_cleartext: vec![] })];
        let module = weave_evaluator_with_gram(&circuit, "gram_eval", &crate::NoProvenance, &configs).into_inner();
        let code = print_weaved_module(&module, false);
        // The action must be emitted as an extern Action stub named `begin`.
        assert!(code.contains("begin"), "expected host action stub:\n{}", code);
        run_compile_check(&code, "gram_evaluator");
    }

    #[test]
    #[should_panic(expected = "unconfigured action")]
    fn test_weave_evaluator_with_gram_unconfigured_panics() {
        let circuit = build_action_circuit();
        let _ = weave_evaluator_with_gram(&circuit, "gram_eval", &crate::NoProvenance, &[]).into_inner();
    }

    #[test]
    fn test_weave_evaluator_with_gram_regarble_mode_compiles() {
        let circuit = build_action_circuit();
        // Re-garbled output (bit 0 secret): the evaluator dataflow is identical
        // — every result bit still flows as an `Eval<N>` label — so this weaves
        // and compiles. `output_cleartext` constrains the host, not the evaluator.
        let configs = [("begin", GramActionConfig { output_cleartext: vec![false] })];
        let module = weave_evaluator_with_gram(&circuit, "gram_eval_rg", &crate::NoProvenance, &configs).into_inner();
        let code = print_weaved_module(&module, false);
        assert!(code.contains("begin"), "expected host action stub:\n{}", code);
        run_compile_check(&code, "gram_evaluator_regarble");
    }

    #[test]
    fn test_weave_evaluator_with_gram_mixed_mode_compiles() {
        let circuit = build_action_circuit();
        // Mixed mode: bit 0 cleartext, bit 1 re-garbled. Same evaluator path.
        let configs = [("begin", GramActionConfig { output_cleartext: vec![true, false] })];
        let module = weave_evaluator_with_gram(&circuit, "gram_eval_mx", &crate::NoProvenance, &configs).into_inner();
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "gram_evaluator_mixed");
    }

    #[test]
    fn test_weave_garbler_with_gram_compiles() {
        let circuit = build_action_circuit();
        let configs = [("begin", GramActionConfig { output_cleartext: vec![] })];
        let module = weave_garbler_with_gram(&circuit, "gram_gb", &crate::NoProvenance, &configs).into_inner();
        let code = print_weaved_module(&module, false);
        // The garbler derives the action-result base via action_result_base.
        assert!(code.contains("action_result_base"), "expected action_result_base:\n{}", code);
        run_compile_check(&code, "gram_garbler");
    }

    #[test]
    #[should_panic(expected = "unconfigured action")]
    fn test_weave_garbler_with_gram_unconfigured_panics() {
        let circuit = build_action_circuit();
        let _ = weave_garbler_with_gram(&circuit, "gram_gb", &crate::NoProvenance, &[]).into_inner();
    }

    #[test]
    fn test_weave_evaluator_bounded_unconditional_compiles() {
        let circuit = build_simple_loop();
        let module = weave_evaluator_bounded(&circuit, "loop_eval", 4, LoweringMode::Unconditional, None).into_inner();
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "bounded_eval_uncond");
    }

    #[test]
    fn test_weave_evaluator_bounded_with_flag_compiles() {
        let circuit = build_simple_loop();
        let module = weave_evaluator_bounded(&circuit, "loop_eval_flag", 4, LoweringMode::WithTerminationFlag, None).into_inner();
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "bounded_eval_flag");
    }

    #[test]
    fn test_weave_garbler_bounded_compiles() {
        let circuit = build_simple_loop();
        let module = weave_garbler_bounded(&circuit, "loop_garble", 4, LoweringMode::Unconditional, None).into_inner();
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "bounded_garbler");
    }

    #[test]
    fn test_weave_evaluator_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_evaluator(&circuit, "test_circuit", None).into_inner();
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "evaluator");
    }

    #[test]
    fn test_weave_garbler_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_garbler(&circuit, "test_circuit", None).into_inner();
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
        let module = weave_into_gc(&circuit, "test_circuit", None).into_inner();
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "into_gc");
    }

    #[test]
    fn test_weave_eval_from_setup_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_eval_from_setup(&circuit, "test_circuit", None).into_inner();
        let code = print_weaved_module(&module, false);
        run_compile_check(&code, "eval_from_setup");
    }

    /// Runtime multi-evaluation correctness: garble a 2-input AND circuit once,
    /// then evaluate all four input combinations using the same GarbledCircuit.
    #[test]
    fn test_multi_eval_and_circuit() {
        let root = workspace_root();

        let and_circuit = crate::tests_common::build_and_circuit();

        let gc_module = weave_into_gc(&and_circuit, "and2", None).into_inner();
        let eval_module = weave_eval_from_setup(&and_circuit, "and2", None).into_inner();

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
