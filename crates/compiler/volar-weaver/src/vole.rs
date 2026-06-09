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
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        AssociatedType, ExternalKind, IrAnyFunction, IrBlock, IrCfgBlock, IrCfgBody, IrCfgFunction,
        IrCfgJump, IrCfgModule, IrCfgTerminator, IrExpr, IrFunction, IrGenericParam,
        IrGenericParamKind, IrLit, IrModule, IrParam, IrPattern, IrStmt, IrTraitBound, IrType,
        IrWherePredicate, MathTrait, MethodKind, PrimitiveType, SpecBinOp, StdMethod, StructKind,
        TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir::ir::{
    IRBlocks, IRBlock as CirBlock, IRBlockTargetId, IRTerminator,
    IRType as CircuitIrType, IRTypeId as CirTyId, IRTypes as CirTypes,
    IRVarId as CirVar, PrimType, PreInitSegment, Stmt, StorageId,
};
use volar_ir::public::PublicSet;
use volar_ir_passes::lower_to_circuit::lower_to_circuit;
pub use volar_ir_passes::lower_to_circuit::LoweringMode;

use crate::{array_default, build_return, clone_expr, expand_ors, ref_expr, var, NoProvenance, ProvenanceHandler};

// ============================================================================
// VOLE-specific type helpers
//
// The K-parametric versions live in `crate::vole_common` so future weavers
// (e.g. the FAEST weaver, which emits `Vope<N, T, U3>` for S-box gates) can
// share the same `IrType` factories. The zero-arg helpers here are thin
// wrappers around `vole_common::*` pinned at K=1 — the AND-check Quicksilver
// degree this weaver implements.
// ============================================================================

/// `Vope<N, T, U1>` — prover's degree-1 VOLE wire commitment.
fn vope_type() -> IrType {
    crate::vole_common::vope_type_k(1)
}

/// `Q<N, T>` — verifier's VOLE wire share.
fn q_type() -> IrType {
    crate::vole_common::q_type()
}

/// `Delta<N, T>` — verifier's global secret.
fn delta_type() -> IrType {
    crate::vole_common::delta_type()
}

/// `Array<T, N>` — element-wise hat / field vector.
fn array_t_n() -> IrType {
    crate::vole_common::array_t_n()
}

/// `[Array<T, N>; AND_COUNT]` — fixed-size hat array returned by the prover.
fn hat_array_type(and_count: usize) -> IrType {
    IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray,
        elem: Box::new(array_t_n()),
        len: volar_compiler::ir::ArrayLength::Const(and_count),
    }
}

/// `&T` reference helper. See [`crate::vole_common::ref_to_vole`].
fn ref_to_vole(ty: IrType) -> IrType {
    crate::vole_common::ref_to_vole(ty)
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
        trait_kind: TraitKind::Math(MathTrait::Clone),
        type_args: vec![],
        assoc_bindings: vec![],
    }
}

/// `T: Default` bound.
fn default_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Math(MathTrait::Default),
        type_args: vec![],
        assoc_bindings: vec![],
    }
}

/// `N: ArraySize` bound.
fn array_size_bound() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::ArraySize,
        type_args: vec![],
        assoc_bindings: vec![],
    }
}

/// `N: VoleArray<T>` bound.
fn vole_array_t_bound() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::VoleArray,
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
fn array_t_from_fn<P: Clone>(idx: &str, body: IrExpr<P>) -> IrExpr<P> {
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
fn array_t_default<P: Clone>() -> IrExpr<P> {
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
fn q_index<P: Clone>(wire_name: &str, idx: &str) -> IrExpr<P> {
    IrExpr::Index {
        base: Box::new(IrExpr::Field {
            base: Box::new(var(wire_name)),
            field: "q".into(),
        }),
        index: Box::new(var(idx)),
    }
}

/// `delta.delta[i]`
fn delta_index<P: Clone>(idx: &str) -> IrExpr<P> {
    IrExpr::Index {
        base: Box::new(IrExpr::Field {
            base: Box::new(var("delta")),
            field: "delta".into(),
        }),
        index: Box::new(var(idx)),
    }
}

/// `Q { q: {body} }`
fn q_struct<P: Clone>(body: IrExpr<P>) -> IrExpr<P> {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Q".into()),
        type_args: vec![],
        fields: vec![("q".into(), body)],
        rest: None,
    }
}

// ============================================================================
// ZK witness configuration
// ============================================================================

/// Per-circuit configuration controlling which inputs/outputs are public cleartext
/// versus private committed witnesses in the VOLE ZK weaver.
///
/// In ZK, *all* inputs are private witnesses by default.  Mark some as public to
/// have them typed as `bool` instead of `Vope`/`Q`; the wire commitment is then
/// synthesised from `vope_one` or `delta` rather than passed in as a VOLE pair.
///
/// Mirrors the `FheActionConfig`-based mechanism in the FHE weaver, but with the
/// opposite default: ZK is private-first, FHE is encrypted-first.
#[derive(Clone, Debug, Default)]
pub struct ZkWitnessConfig {
    /// Which circuit input params (0-based) are public cleartext inputs.
    ///
    /// Public inputs become `bool` parameters; the prover synthesises
    /// `vope_input_i = if b { vope_one.clone() } else { zero }` and the verifier
    /// synthesises `q_input_i = if b { Q { q: delta.clone() } } else { Q { q: 0 } }`.
    pub public_inputs: PublicSet,

    /// Per-action public output configuration, keyed by action name.
    ///
    /// For each named action, specifies which output bits are public cleartext
    /// (`bool`) versus private committed (`Vope`/`Q`).  Actions absent from this
    /// map default to all-private.
    pub action_configs: BTreeMap<String, ZkActionConfig>,
}

/// Per-action output configuration for the ZK weaver.
///
/// Structurally identical to `FheActionConfig` — same concept, different default
/// (all-private rather than all-public).
#[derive(Clone, Debug, Default)]
pub struct ZkActionConfig {
    /// Per-output-bit publicness flags (indexed by bit position within the action's output).
    ///
    /// `output_public[i] = true` → output bit `i` is cleartext (`bool` parameter).
    /// `false` (or absent) → committed VOLE wire (default).
    pub output_public: Vec<bool>,
}

impl ZkActionConfig {
    /// Returns `true` if output bit `idx` is public.
    pub fn is_output_public(&self, idx: usize) -> bool {
        self.output_public.get(idx).copied().unwrap_or(false)
    }
}

// ============================================================================
// Public-wire synthesis helpers
// ============================================================================

/// `bool` type — the Rust type for public ZK inputs.
fn bool_type() -> IrType {
    IrType::Primitive(PrimitiveType::Bool)
}

/// Synthesise a prover VOLE wire from a public `bool` variable `bool_name`.
///
/// Generates: `if {bool_name} { vope_one.clone() } else { vope_one.clone() + vope_one.clone() }`
///
/// `vope_one.clone() + vope_one.clone()` = the zero Vope (since addition is XOR in GF2
/// and adding a committed wire to itself cancels both bit and MAC).
fn synth_prover_public_wire<P: Clone>(bool_name: &str) -> IrExpr<P> {
    let vope_one = clone_expr(var("vope_one"));
    let vope_zero = IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(clone_expr(var("vope_one"))),
        right: Box::new(clone_expr(var("vope_one"))),
    };
    IrExpr::If {
        cond: Box::new(var(bool_name)),
        then_branch: IrBlock {
            stmts: vec![],
            stmt_provs: vec![],
            expr: Some(Box::new(vope_one)),
        },
        else_branch: Some(Box::new(vope_zero)),
    }
}

/// Synthesise a verifier Q wire from a public `bool` variable `bool_name`.
///
/// Generates: `if {bool_name} { Q { q: delta.delta.clone() } } else { Q { q: Array::default() } }`
///
/// For a public bit `b=1` the verifier computes `K = M + 1·Δ`; with `M=0` this is `Δ`.
/// For `b=0`, `K = 0`.  This is consistent with the prover's synthesis above.
fn synth_verifier_public_wire<P: Clone>(bool_name: &str) -> IrExpr<P> {
    let q_one = q_struct(IrExpr::MethodCall {
        receiver: Box::new(IrExpr::Field {
            base: Box::new(var("delta")),
            field: "delta".into(),
        }),
        method: MethodKind::Known(StdMethod::Clone),
        type_args: vec![],
        args: vec![],
    });
    let q_zero = q_struct(array_t_default());
    IrExpr::If {
        cond: Box::new(var(bool_name)),
        then_branch: IrBlock {
            stmts: vec![],
            stmt_provs: vec![],
            expr: Some(Box::new(q_one)),
        },
        else_branch: Some(Box::new(q_zero)),
    }
}

// ============================================================================
// AND gate helper calls
// ============================================================================

/// Emit `let (_wire_k, _hat_k) = vole_and_prover_step::<N, T>(wire_a.clone(), wire_b.clone());`
/// The hat variable is left in scope for the caller to collect into a `FixedArray`.
fn emit_prover_and_gate<P: Clone>(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    hat_name: &str,
    stmts: &mut Vec<IrStmt<P>>,
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
fn emit_verifier_and_gate<P: Clone>(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    ok_name: &str,
    q_and_name: &str,
    hat_name: &str,
    stmts: &mut Vec<IrStmt<P>>,
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

/// `[Vope<N, T, U2>; SBOX_COUNT]` — hat-free K=2 S-box product commitments.
fn sbox_vope_array_type(sbox_count: usize) -> IrType {
    IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray,
        elem: Box::new(crate::vole_common::vope_type_k(2)),
        len: volar_compiler::ir::ArrayLength::Const(sbox_count),
    }
}

/// Emit: `let (wire_k, sbox_k2_k) = vole_sbox_prover_step::<N, T>(wire_a.clone(), wire_b.clone());`
///
/// Calls the single-function prover step that returns both the K=1 downstream
/// wire AND the K=2 hat-free product Vope.
fn emit_prover_sbox_gate_k2<P: Clone>(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    k2_name: &str,
    stmts: &mut Vec<IrStmt<P>>,
) {
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(k2_name),
        ]),
        ty: None,
        init: Some(IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["vole_sbox_prover_step".into()],
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

/// Emit: `let (wire_k, ok_k) = vole_sbox_verifier_check::<N,T>(delta, &q_a, &q_b, sbox_vopes[idx].clone());`
/// followed by `all_ok = all_ok && ok_k;`.
fn emit_verifier_sbox_check_k2<P: Clone>(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    ok_name: &str,
    sbox_idx: usize,
    stmts: &mut Vec<IrStmt<P>>,
) {
    // let (wire_k, ok_k) = vole_sbox_verifier_check::<N,T>(delta, &q_a, &q_b, sbox_vopes[idx].clone());
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(ok_name),
        ]),
        ty: None,
        init: Some(IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["vole_sbox_verifier_check".into()],
                type_args: vec![
                    IrType::TypeParam("N".into()),
                    IrType::TypeParam("T".into()),
                ],
            }),
            args: vec![
                var("delta"),
                ref_expr(var(name_a)),
                ref_expr(var(name_b)),
                clone_expr(IrExpr::Index {
                    base: Box::new(var("sbox_vopes")),
                    index: Box::new(IrExpr::Lit(IrLit::Int(sbox_idx as i128))),
                }),
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
/// All inputs are treated as private committed witnesses.
/// Use [`weave_vole_prover_with_config`] to mark some inputs or action outputs
/// as public cleartext.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_vole_prover<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let mut module = weave_vole_prover_with_handler(circuit, name, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Weave a single-block boolean circuit into a VOLE **prover** `IrModule`,
/// using `handler` to map input provenance into the output IR.
///
/// All inputs are treated as private committed witnesses (default ZK behaviour).
/// Use [`weave_vole_prover_with_config_and_handler`] to mark some as public.
pub fn weave_vole_prover_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    weave_vole_prover_inner(circuit, name, &ZkWitnessConfig::default(), handler)
}

/// Weave a single-block boolean circuit into a VOLE **prover** `IrModule` with
/// explicit public/private witness configuration.
///
/// Public inputs in `config.public_inputs` are typed as `bool`; the wire
/// commitment is synthesised from `vope_one` at runtime.  Public action outputs
/// in `config.action_configs` are similarly typed as `bool` parameters.
pub fn weave_vole_prover_with_config<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let mut module = weave_vole_prover_inner(circuit, name, config, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Weave with both a [`ZkWitnessConfig`] and a provenance handler.
pub fn weave_vole_prover_with_config_and_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    weave_vole_prover_inner(circuit, name, config, handler)
}

fn weave_vole_prover_inner<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    assert!(
        circuit.is_circuit(),
        "weave_vole_prover: circuit must satisfy is_circuit()"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let ctrl_prov: H::Output = block.stmt_provs.first()
        .map(|p| handler.map(p))
        .expect("weave_vole_prover_inner: circuit has no statements; cannot derive provenance for infrastructure statements");

    // Pre-scan for external primitives (oracle calls, action calls, RNG sources).
    // Track (name, bit_count) for actions so we can look up per-action public configs.
    let mut oracle_handle_map = BTreeMap::<u32, usize>::new();
    let mut oracle_bit_counts: Vec<usize> = Vec::new();
    let mut action_handle_map = BTreeMap::<u32, usize>::new();
    let mut action_infos: Vec<(String, usize)> = Vec::new(); // (name, num_bits)
    let mut rng_var_map = BTreeMap::<u32, usize>::new();
    for (result_id, stmt, _) in &expanded {
        match stmt {
            BIrStmt::OracleCall { num_bits, .. } => {
                let k = oracle_bit_counts.len();
                oracle_handle_map.insert(result_id.0, k);
                oracle_bit_counts.push(*num_bits);
            }
            BIrStmt::ActionCall { name: action_name, num_bits, .. } => {
                let k = action_infos.len();
                action_handle_map.insert(result_id.0, k);
                action_infos.push((action_name.clone(), *num_bits));
            }
            BIrStmt::Rng { .. } => {
                let r = rng_var_map.len();
                rng_var_map.insert(result_id.0, r);
            }
            _ => {}
        }
    }

    // var_names maps param index → wire name in the emitted function body.
    // For private inputs the wire name IS the param name; for public inputs we
    // synthesise a wire after the params so the same name is used in gate logic.
    let mut var_names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("vope_input_{}", i));
    }

    // Build parameter list.
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "vope_one".into(),
        ty: vope_type(),
    });
    for i in 0..num_params {
        let is_pub = config.public_inputs.is_public(CirVar(i as u32));
        params.push(IrParam {
            name: if is_pub { format!("input_{}", i) } else { format!("vope_input_{}", i) },
            ty: if is_pub { bool_type() } else { vope_type() },
        });
    }
    // Oracle output bit commitments — always private (no oracle public config yet).
    for (k, &num_bits) in oracle_bit_counts.iter().enumerate() {
        for j in 0..num_bits {
            params.push(IrParam {
                name: format!("vope_oracle_{}_bit_{}", k, j),
                ty: vope_type(),
            });
        }
    }
    // Action output bit commitments — public or private per ZkActionConfig.
    for (k, (action_name, num_bits)) in action_infos.iter().enumerate() {
        let action_cfg = config.action_configs.get(action_name.as_str());
        for j in 0..*num_bits {
            let is_pub = action_cfg.map(|c| c.is_output_public(j)).unwrap_or(false);
            params.push(IrParam {
                name: if is_pub {
                    format!("action_{}_bit_{}", k, j)
                } else {
                    format!("vope_action_{}_bit_{}", k, j)
                },
                ty: if is_pub { bool_type() } else { vope_type() },
            });
        }
    }
    // RNG bit commitments — always private.
    for r in 0..rng_var_map.len() {
        params.push(IrParam {
            name: format!("vope_rng_{}", r),
            ty: vope_type(),
        });
    }

    // Count K=1 (regular AND) and K=2 (sbox AND) gates separately.
    let (and_count, sbox_count) = expanded.iter().fold((0usize, 0usize), |(k1, k2), (_, s, prov)| {
        if matches!(s, BIrStmt::And(..)) {
            if handler.gate_degree(prov) == 2 { (k1, k2 + 1) } else { (k1 + 1, k2) }
        } else {
            (k1, k2)
        }
    });

    // Return type: (Vope<N, T, U1>, [Array<T, N>; K1_COUNT])
    // or, when sbox_count > 0: (Vope<N, T, U1>, [Array<T, N>; K1_COUNT], [Vope<N,T,U2>; SBOX_COUNT])
    let ret_type = if sbox_count == 0 {
        IrType::Tuple(vec![vope_type(), hat_array_type(and_count)])
    } else {
        IrType::Tuple(vec![vope_type(), hat_array_type(and_count), sbox_vope_array_type(sbox_count)])
    };

    let (generics, where_clause) = prover_generics_and_where();

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut stmt_provs: Vec<H::Output> = Vec::new();
    let mut and_counter: usize = 0;
    let mut sbox_counter: usize = 0;
    let mut hat_names: Vec<String> = Vec::new();
    let mut sbox_k2_names: Vec<String> = Vec::new();

    // Synthesise Vope wires for public inputs from the bool params.
    for i in 0..num_params {
        if config.public_inputs.is_public(CirVar(i as u32)) {
            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&format!("vope_input_{}", i)),
                ty: None,
                init: Some(synth_prover_public_wire(&format!("input_{}", i))),
            });
            stmt_provs.push(ctrl_prov.clone());
        }
    }

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        match stmt {
            BIrStmt::Zero => {
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::StructExpr {
                        kind: StructKind::Custom("Vope".into()),
                        type_args: vec![],
                        fields: vec![
                            ("u".into(), array_default()),
                            ("v".into(), array_t_default()),
                        ],
                        rest: None,
                    }),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::One => {
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var("vope_one"))),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&name_a))),
                        right: Box::new(clone_expr(var(&name_b))),
                    }),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&name_a))),
                        right: Box::new(clone_expr(var("vope_one"))),
                    }),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                if handler.gate_degree(prov) == 2 {
                    // K=2 S-box gate: hat-free, produces (Vope<K=1>, Vope<K=2>).
                    let k2_name = format!("sbox_k2_{}", sbox_counter);
                    sbox_counter += 1;
                    sbox_k2_names.push(k2_name.clone());
                    emit_prover_sbox_gate_k2(&name_a, &name_b, &let_name, &k2_name, &mut stmts);
                } else {
                    // K=1 standard AND gate: produces K=1 Vope + hat.
                    let hat_name = format!("hat_{}", and_counter);
                    and_counter += 1;
                    hat_names.push(hat_name.clone());
                    emit_prover_and_gate(&name_a, &name_b, &let_name, &hat_name, &mut stmts);
                }
                stmt_provs.push(q.clone());
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),

            BIrStmt::OracleCall { .. } => {
                let k = oracle_handle_map[&result_id.0];
                var_names.insert(result_id.0, format!("oracle_handle_{}", k));
                continue;
            }

            BIrStmt::OracleBit { call, bit } => {
                let k = oracle_handle_map[&call.0];
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("vope_oracle_{}_bit_{}", k, bit)))),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::ActionCall { .. } => {
                let k = action_handle_map[&result_id.0];
                var_names.insert(result_id.0, format!("action_handle_{}", k));
                continue;
            }

            BIrStmt::ActionBit { call, bit } => {
                let k = action_handle_map[&call.0];
                let (action_name, _) = &action_infos[k];
                let is_pub = config.action_configs
                    .get(action_name.as_str())
                    .map(|c| c.is_output_public(*bit))
                    .unwrap_or(false);
                let init = if is_pub {
                    synth_prover_public_wire(&format!("action_{}_bit_{}", k, bit))
                } else {
                    clone_expr(var(&format!("vope_action_{}_bit_{}", k, bit)))
                };
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(init),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::Rng { .. } => {
                let r = rng_var_map[&result_id.0];
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("vope_rng_{}", r)))),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::StorageRead { .. } | BIrStmt::StorageWrite { .. } => {
                unimplemented!(
                    "StorageRead/Write not supported in BIrBlocks VOLE weavers; \
                     use IRBlocks-based weavers (weave_vole_prover_ir) instead"
                )
            }
            _ => unimplemented!("vole weaver: unhandled BIrStmt variant — add support for this variant"),
        }

        var_names.insert(result_id.0, let_name);
    }

    // Return (output_wire, [hat_0, hat_1, ...]) or
    //        (output_wire, [hat_0, ...], [sbox_k2_0, ...]) when sbox_count > 0.
    let (output_expr, _) = build_return(block, &var_names, vope_type());
    let hats_expr = IrExpr::FixedArray(hat_names.iter().map(|h| var(h)).collect());
    let ret_expr = if sbox_k2_names.is_empty() {
        IrExpr::Tuple(vec![output_expr, hats_expr])
    } else {
        let sbox_expr = IrExpr::FixedArray(sbox_k2_names.iter().map(|n| var(n)).collect());
        IrExpr::Tuple(vec![output_expr, hats_expr, sbox_expr])
    };

    let func = IrFunction {
        name: format!("vole_prove_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_vole_prover".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    module
}

// ============================================================================
// Verifier weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a VOLE **verifier** `IrModule`.
///
/// All inputs are treated as private committed witnesses.
/// Use [`weave_vole_verifier_with_config`] to mark some as public.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_vole_verifier<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let mut module = weave_vole_verifier_with_handler(circuit, name, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Weave a single-block boolean circuit into a VOLE **verifier** `IrModule`,
/// using `handler` to map input provenance into the output IR.
///
/// All inputs are private witnesses.  Use [`weave_vole_verifier_with_config_and_handler`]
/// for public/private control.
pub fn weave_vole_verifier_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    weave_vole_verifier_inner(circuit, name, &ZkWitnessConfig::default(), handler)
}

/// Weave a single-block boolean circuit into a VOLE **verifier** `IrModule` with
/// explicit public/private witness configuration.
///
/// Public inputs become `bool` parameters; the verifier synthesises Q wires from
/// `delta` rather than receiving them as VOLE shares.
pub fn weave_vole_verifier_with_config<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let mut module = weave_vole_verifier_inner(circuit, name, config, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Weave with both a [`ZkWitnessConfig`] and a provenance handler.
pub fn weave_vole_verifier_with_config_and_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    weave_vole_verifier_inner(circuit, name, config, handler)
}

fn weave_vole_verifier_inner<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    assert!(
        circuit.is_circuit(),
        "weave_vole_verifier: circuit must satisfy is_circuit()"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let ctrl_prov: H::Output = block.stmt_provs.first()
        .map(|p| handler.map(p))
        .expect("weave_vole_verifier_inner: circuit has no statements; cannot derive provenance for infrastructure statements");

    let (and_count, sbox_count) = expanded.iter().fold((0usize, 0usize), |(k1, k2), (_, s, prov)| {
        if matches!(s, BIrStmt::And(..)) {
            if handler.gate_degree(prov) == 2 { (k1, k2 + 1) } else { (k1 + 1, k2) }
        } else {
            (k1, k2)
        }
    });

    // Pre-scan: track (name, bit_count) for actions.
    let mut oracle_handle_map = BTreeMap::<u32, usize>::new();
    let mut oracle_bit_counts: Vec<usize> = Vec::new();
    let mut action_handle_map = BTreeMap::<u32, usize>::new();
    let mut action_infos: Vec<(String, usize)> = Vec::new(); // (name, num_bits)
    let mut rng_var_map = BTreeMap::<u32, usize>::new();
    for (result_id, stmt, _) in &expanded {
        match stmt {
            BIrStmt::OracleCall { num_bits, .. } => {
                let k = oracle_bit_counts.len();
                oracle_handle_map.insert(result_id.0, k);
                oracle_bit_counts.push(*num_bits);
            }
            BIrStmt::ActionCall { name: action_name, num_bits, .. } => {
                let k = action_infos.len();
                action_handle_map.insert(result_id.0, k);
                action_infos.push((action_name.clone(), *num_bits));
            }
            BIrStmt::Rng { .. } => {
                let r = rng_var_map.len();
                rng_var_map.insert(result_id.0, r);
            }
            _ => {}
        }
    }

    let mut var_names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("q_input_{}", i));
    }

    // Build parameter list.
    let mut params: Vec<IrParam> = Vec::new();

    // delta: &Delta<N, T> — always present (needed to synthesise public wires too).
    params.push(IrParam {
        name: "delta".into(),
        ty: ref_to_vole(delta_type()),
    });

    // Pairs (q_and_k, hat_k) for each K=1 AND gate.
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

    // K=2 S-box Vopes — one per sbox gate (verifier-side check).
    if sbox_count > 0 {
        params.push(IrParam {
            name: "sbox_vopes".into(),
            ty: sbox_vope_array_type(sbox_count),
        });
    }

    // Input wire Q shares — or bool for public inputs.
    for i in 0..num_params {
        let is_pub = config.public_inputs.is_public(CirVar(i as u32));
        params.push(IrParam {
            name: if is_pub { format!("input_{}", i) } else { format!("q_input_{}", i) },
            ty: if is_pub { bool_type() } else { q_type() },
        });
    }
    // Oracle output Q shares — always private.
    for (k, &num_bits) in oracle_bit_counts.iter().enumerate() {
        for j in 0..num_bits {
            params.push(IrParam {
                name: format!("q_oracle_{}_bit_{}", k, j),
                ty: q_type(),
            });
        }
    }
    // Action output Q shares — public or private per ZkActionConfig.
    for (k, (action_name, num_bits)) in action_infos.iter().enumerate() {
        let action_cfg = config.action_configs.get(action_name.as_str());
        for j in 0..*num_bits {
            let is_pub = action_cfg.map(|c| c.is_output_public(j)).unwrap_or(false);
            params.push(IrParam {
                name: if is_pub {
                    format!("action_{}_bit_{}", k, j)
                } else {
                    format!("q_action_{}_bit_{}", k, j)
                },
                ty: if is_pub { bool_type() } else { q_type() },
            });
        }
    }
    // RNG Q shares — always private.
    for r in 0..rng_var_map.len() {
        params.push(IrParam {
            name: format!("q_rng_{}", r),
            ty: q_type(),
        });
    }

    // Return type: (Q<N, T>, bool)
    let ret_type = IrType::Tuple(vec![
        q_type(),
        IrType::Primitive(PrimitiveType::Bool),
    ]);

    let (generics, where_clause) = verifier_generics_and_where();

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut stmt_provs: Vec<H::Output> = Vec::new();
    let mut and_counter: usize = 0;
    let mut sbox_counter: usize = 0;

    stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident {
            mutable: true,
            name: "all_ok".into(),
            subpat: None,
        },
        ty: None,
        init: Some(IrExpr::Lit(IrLit::Bool(true))),
    });
    stmt_provs.push(ctrl_prov.clone());

    // Synthesise Q wires for public inputs from the bool params.
    for i in 0..num_params {
        if config.public_inputs.is_public(CirVar(i as u32)) {
            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&format!("q_input_{}", i)),
                ty: None,
                init: Some(synth_verifier_public_wire(&format!("input_{}", i))),
            });
            stmt_provs.push(ctrl_prov.clone());
        }
    }

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        match stmt {
            BIrStmt::Zero => {
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(array_t_default())),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::One => {
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(IrExpr::MethodCall {
                        receiver: Box::new(IrExpr::Field {
                            base: Box::new(var("delta")),
                            field: "delta".into(),
                        }),
                        method: MethodKind::Known(StdMethod::Clone),
                        type_args: vec![],
                        args: vec![],
                    })),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
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
                stmt_provs.push(q.clone());
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
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
                stmt_provs.push(q.clone());
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let ok_name = format!("ok_and_{}", and_counter + sbox_counter);
                if handler.gate_degree(prov) == 2 {
                    // K=2 S-box gate: verify via sbox_vopes[sbox_idx] * delta == q_a * q_b.
                    emit_verifier_sbox_check_k2(
                        &name_a, &name_b, &let_name, &ok_name,
                        sbox_counter, &mut stmts,
                    );
                    sbox_counter += 1;
                    stmt_provs.push(q.clone());
                    stmt_provs.push(q.clone());
                    stmt_provs.push(q.clone());
                } else {
                    let q_and_name = format!("q_and_{}", and_counter);
                    let hat_name = format!("hat_{}", and_counter);
                    and_counter += 1;
                    emit_verifier_and_gate(
                        &name_a, &name_b, &let_name, &ok_name,
                        &q_and_name, &hat_name,
                        &mut stmts,
                    );
                    stmt_provs.push(q.clone());
                    stmt_provs.push(q.clone());
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),

            BIrStmt::OracleCall { .. } => {
                let k = oracle_handle_map[&result_id.0];
                var_names.insert(result_id.0, format!("oracle_handle_{}", k));
                continue;
            }

            BIrStmt::OracleBit { call, bit } => {
                let k = oracle_handle_map[&call.0];
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("q_oracle_{}_bit_{}", k, bit)))),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::ActionCall { .. } => {
                let k = action_handle_map[&result_id.0];
                var_names.insert(result_id.0, format!("action_handle_{}", k));
                continue;
            }

            BIrStmt::ActionBit { call, bit } => {
                let k = action_handle_map[&call.0];
                let (action_name, _) = &action_infos[k];
                let is_pub = config.action_configs
                    .get(action_name.as_str())
                    .map(|c| c.is_output_public(*bit))
                    .unwrap_or(false);
                let init = if is_pub {
                    synth_verifier_public_wire(&format!("action_{}_bit_{}", k, bit))
                } else {
                    clone_expr(var(&format!("q_action_{}_bit_{}", k, bit)))
                };
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(init),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::Rng { .. } => {
                let r = rng_var_map[&result_id.0];
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("q_rng_{}", r)))),
                });
                stmt_provs.push(q.clone());
            }

            BIrStmt::StorageRead { .. } | BIrStmt::StorageWrite { .. } => {
                unimplemented!(
                    "StorageRead/Write not supported in BIrBlocks VOLE weavers; \
                     use IRBlocks-based weavers (weave_vole_verifier_ir) instead"
                )
            }
            _ => unimplemented!("vole weaver: unhandled BIrStmt variant — add support for this variant"),
        }

        var_names.insert(result_id.0, let_name);
    }

    // Return (output_wire, all_ok).
    let (output_expr, _) = build_return(block, &var_names, q_type());
    let ret_expr = IrExpr::Tuple(vec![output_expr, var("all_ok")]);

    let func = IrFunction {
        name: format!("vole_verify_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_vole_verifier".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    module
}

// ============================================================================
// Bounded wrappers
// ============================================================================

/// Backwards-compatible bounded VOLE prover weave.
pub fn weave_vole_prover_bounded<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let mut module = weave_vole_prover_bounded_with_handler(circuit, name, limit, mode, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Bounded VOLE prover weave with provenance handler.
pub fn weave_vole_prover_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_vole_prover_inner(&lowered, name, &ZkWitnessConfig::default(), handler)
}

/// Bounded VOLE prover weave with witness config.
pub fn weave_vole_prover_bounded_with_config<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let lowered = lower_to_circuit(circuit, limit, mode);
    let mut module = weave_vole_prover_inner(&lowered, name, config, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Bounded VOLE prover weave with witness config and provenance handler.
pub fn weave_vole_prover_bounded_with_config_and_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_vole_prover_inner(&lowered, name, config, handler)
}

/// Backwards-compatible bounded VOLE verifier weave.
pub fn weave_vole_verifier_bounded<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let mut module = weave_vole_verifier_bounded_with_handler(circuit, name, limit, mode, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Bounded VOLE verifier weave with provenance handler.
pub fn weave_vole_verifier_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_vole_verifier_inner(&lowered, name, &ZkWitnessConfig::default(), handler)
}

/// Bounded VOLE verifier weave with witness config.
pub fn weave_vole_verifier_bounded_with_config<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let lowered = lower_to_circuit(circuit, limit, mode);
    let mut module = weave_vole_verifier_inner(&lowered, name, config, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Bounded VOLE verifier weave with witness config and provenance handler.
pub fn weave_vole_verifier_bounded_with_config_and_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
{
    let lowered = lower_to_circuit(circuit, limit, mode);
    weave_vole_verifier_inner(&lowered, name, config, handler)
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

/// How storage operations are authenticated in the VOLE proof.
#[derive(Clone, Debug)]
pub enum StorageMode {
    /// MUX/demux tree: O(N) AND gates per access, fully in-circuit.
    /// Every cell is a VOLE-authenticated wire; the verifier holds N Q values.
    Tree(StorageSizes),

    /// External commitment: **0 AND gates** for storage access.
    ///
    /// Reads are oracle parameters (fresh VOLE-authenticated values provided
    /// by the prover).  Writes are no-ops in the circuit.  The returned
    /// [`MemoryTrace`] records every access so that an external checker can
    /// verify memory consistency against a commitment (Merkle tree, KZG,
    /// or the multiset argument described below).
    ///
    /// # Verifier storage: O(1)
    ///
    /// The verifier no longer maintains per-cell Q values.  It only needs
    /// the random challenge `r` and a running hash/product commitment
    /// (one T-element) for the multiset check.
    ///
    /// # External multiset memory check (offline memory checking)
    ///
    /// After the VOLE proof, the verifier performs a separate check:
    ///
    /// 1. **Encode** each memory op as `h = addr·r + value·r² + timestamp·r³`
    ///    where `r` is a random challenge and all multiplications are
    ///    by public field constants (free on VOLE-authenticated values).
    ///
    /// 2. **Accumulate** into two multiset hashes:
    ///    - `H_produce`: init entries (addr, 0, 0) + write entries (addr, new, t)
    ///    - `H_consume`: overwritten entries (addr, old, t_old) + final state
    ///
    /// 3. **Check** `H_produce == H_consume`.  By Schwartz-Zippel over |T|,
    ///    a cheating prover passes with probability ≤ M/|T| (negligible for
    ///    M ≪ 2¹²⁸).
    ///
    /// The encoding step uses only public-scalar × authenticated-value
    /// multiplications (free in VOLE) and additions (free), so the
    /// multiset check adds **zero AND gates**.
    ///
    /// # Cost comparison (N cells, M accesses, K addr bits)
    ///
    /// | Mode       | AND gates | Verifier state | Extra VOLE correlations |
    /// |------------|-----------|----------------|------------------------|
    /// | Tree       | O(M·N)    | O(N) Q-values  | 0                      |
    /// | Commitment | **0**     | **O(1)**       | 1 per read             |
    Commitment,
}

/// One entry in the memory access trace (for external verification).
#[derive(Clone, Debug)]
pub struct MemoryTraceEntry {
    /// Circuit variable ID of the address (Merge result or scalar).
    pub addr_var: u32,
    /// Circuit variable ID of the value.
    pub value_var: u32,
    pub storage_id: u32,
    pub type_id: u32,
    pub is_write: bool,
    pub timestamp: u32,
}

/// Full memory access trace returned alongside the proof in
/// [`StorageMode::Commitment`] mode.
///
/// The external verifier uses this trace plus the random challenge `r` to
/// perform the multiset consistency check.
#[derive(Clone, Debug, Default)]
pub struct MemoryTrace {
    pub entries: Vec<MemoryTraceEntry>,
}

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
    mode: &StorageMode,
) -> usize {
    let storage_sizes = match mode {
        StorageMode::Tree(ss) => ss,
        StorageMode::Commitment => return count_ir_ands_no_storage(block, types),
    };
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
            Stmt::Rng { ty, .. } => ty.clone(),
            Stmt::OracleCall { result_ty, .. } | Stmt::ActionCall { result_ty, .. } => result_ty.clone(),
            Stmt::OracleOutput { ty, .. } | Stmt::ActionOutput { ty, .. } => ty.clone(),
            _ => panic!("count_ir_ands: unhandled Stmt variant — add AND count for this variant"),
        };
        var_types.push(result_ty);
    }
    count
}

/// AND count for commitment mode: only Poly stmts contribute.
fn count_ir_ands_no_storage(block: &CirBlock, types: &CirTypes) -> usize {
    let mut count = 0;
    for stmt in &block.stmts {
        if let Stmt::Poly { coeffs, .. } = stmt {
            for (mono, coeff) in coeffs {
                if *coeff % 2 == 1 && mono.len() >= 2 {
                    count += mono.len() - 1;
                }
            }
        }
    }
    count
}

/// Count storage reads in the circuit (for oracle parameter sizing).
fn count_storage_reads(block: &CirBlock) -> usize {
    block.stmts.iter().filter(|s| matches!(s, Stmt::StorageRead { .. })).count()
}

/// Per-oracle-call bit layout: total committed bits across all outputs.
struct ExternalCallBits {
    /// Total bits across all outputs of this call.
    total_bits: usize,
}

/// Counts of bits needed for external oracle/action/rng primitives in a circuit.
struct ExternalBitCounts {
    /// One entry per `OracleCall` stmt, in encounter order.
    oracle_calls: Vec<ExternalCallBits>,
    /// One entry per `ActionCall` stmt, in encounter order.
    action_calls: Vec<ExternalCallBits>,
    /// Width of each `Rng` stmt encountered, in encounter order.
    rng_widths: Vec<usize>,
}

/// Scan the circuit block and compute how many committed bits are needed for
/// each external oracle call, action call, and rng statement.
///
/// The order of the scan must match the order that [`VoleIrCtx::emit_circuit`]
/// processes statements, so that indices align.
fn count_external_primitives(block: &CirBlock, types: &CirTypes) -> ExternalBitCounts {
    let mut oracle_calls = Vec::new();
    let mut action_calls = Vec::new();
    let mut rng_widths = Vec::new();

    for stmt in &block.stmts {
        match stmt {
            Stmt::OracleCall { output_tys, .. } => {
                let total_bits: usize = output_tys.iter().map(|ty| cir_type_width(ty, types)).sum();
                oracle_calls.push(ExternalCallBits { total_bits });
            }
            Stmt::ActionCall { output_tys, .. } => {
                let total_bits: usize = output_tys.iter().map(|ty| cir_type_width(ty, types)).sum();
                action_calls.push(ExternalCallBits { total_bits });
            }
            Stmt::Rng { ty, .. } => {
                rng_widths.push(cir_type_width(ty, types));
            }
            _ => {}
        }
    }

    ExternalBitCounts { oracle_calls, action_calls, rng_widths }
}

/// Context for emitting VOLE-authenticated wire computations.
struct VoleIrCtx {
    stmts: Vec<IrStmt>,
    wires: alloc::collections::BTreeMap<u32, WireRepr>,
    and_counter: usize,
    hat_names: Vec<String>,
    ok_names: Vec<String>,
    /// Current var name for each storage cell (Tree mode only).
    stor: alloc::collections::BTreeMap<(u32, u32, usize), String>,
    /// Counter for oracle-read parameters (Commitment mode).
    oracle_counter: usize,
    /// Memory trace (Commitment mode).
    trace: MemoryTrace,
    /// Running timestamp for memory operations.
    mem_timestamp: u32,
    is_prover: bool,
    // ---- External primitive tracking (oracle calls, action calls, RNG) --------
    /// var_id of OracleCall → (oracle_index, per-output bit offsets).
    ext_oracle_map: alloc::collections::BTreeMap<u32, (usize, Vec<usize>)>,
    /// var_id of ActionCall → (action_index, per-output bit offsets).
    ext_action_map: alloc::collections::BTreeMap<u32, (usize, Vec<usize>)>,
    /// Index of the next OracleCall encountered (distinct from oracle_counter for storage reads).
    ext_oracle_counter: usize,
    /// Index of the next ActionCall encountered.
    ext_action_counter: usize,
    /// Index of the next Rng stmt encountered.
    ext_rng_counter: usize,
}

/// Returns the pre-init constant for storage cell `(sid, tid, ci)`, or `None`.
fn lookup_pre_init_value(
    pre_init: &[PreInitSegment],
    sid: u32,
    tid: u32,
    ci: usize,
) -> Option<volar_ir::ir::Constant> {
    for seg in pre_init {
        if seg.storage.0 != sid || seg.ty.0 != tid { continue; }
        if ci < seg.offset { continue; }
        let local = ci - seg.offset;
        if local < seg.data.len() { return Some(seg.data[local]); }
    }
    None
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
            oracle_counter: 0,
            trace: MemoryTrace::default(),
            mem_timestamp: 0,
            is_prover,
            ext_oracle_map: alloc::collections::BTreeMap::new(),
            ext_action_map: alloc::collections::BTreeMap::new(),
            ext_oracle_counter: 0,
            ext_action_counter: 0,
            ext_rng_counter: 0,
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

    // ---- Commitment-mode storage (0 AND gates) ----------------------------

    /// Commitment-mode read: use an oracle parameter as the value.
    fn emit_storage_read_committed(
        &mut self,
        out_name: &str,
        var_id: u32,
        storage_id: u32,
        type_id: u32,
        addr_var: &CirVar,
        types: &CirTypes,
        val_ty: &CirTyId,
    ) {
        let param_name = format!("oracle_rd_{}", self.oracle_counter);
        self.oracle_counter += 1;

        // The value is just a clone of the oracle parameter (0 AND gates).
        self.stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(out_name),
            ty: None,
            init: Some(clone_expr(var(&param_name))),
        });

        self.trace.entries.push(MemoryTraceEntry {
            addr_var: addr_var.0,
            value_var: var_id,
            storage_id,
            type_id,
            is_write: false,
            timestamp: self.mem_timestamp,
        });
        self.mem_timestamp += 1;
    }

    /// Commitment-mode write: record in trace, no circuit gates.
    fn emit_storage_write_committed(
        &mut self,
        _out_name: &str,
        storage_id: u32,
        type_id: u32,
        src_var: &CirVar,
        addr_var: &CirVar,
    ) {
        self.trace.entries.push(MemoryTraceEntry {
            addr_var: addr_var.0,
            value_var: src_var.0,
            storage_id,
            type_id,
            is_write: true,
            timestamp: self.mem_timestamp,
        });
        self.mem_timestamp += 1;
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
        mode: &StorageMode,
        pre_init: &[PreInitSegment],
    ) {
        let p = block.params.len();

        // Register input wires.
        for i in 0..p {
            let name = format!("w_{}", i);
            self.wires.insert(i as u32, WireRepr::Scalar(name));
        }

        // Initialize storage cells (Tree mode only).
        // Cells with a pre-init value get per-bit emit_one/emit_zero; others get emit_zero.
        if let StorageMode::Tree(storage_sizes) = mode {
            for (&(sid, tid), &count) in storage_sizes {
                let cell_tid = CirTyId(tid);
                let vw = cir_type_width(&cell_tid, types);
                for ci in 0..count {
                    let name = format!("_sinit_{}_{}_{}", sid, tid, ci);
                    match lookup_pre_init_value(pre_init, sid, tid, ci) {
                        None => self.emit_zero(&name),
                        Some(c) => {
                            let val = c.lo;
                            if vw == 1 {
                                if val & 1 == 1 { self.emit_one(&name); }
                                else { self.emit_zero(&name); }
                            } else {
                                for j in 0..vw {
                                    let n = format!("{}_{}", name, j);
                                    if (val >> j) & 1 == 1 { self.emit_one(&n); }
                                    else { self.emit_zero(&n); }
                                }
                            }
                        }
                    }
                    self.stor.insert((sid, tid, ci), name);
                }
            }
        }

        // Commitment mode: emit pre-init writes as constant authenticated wires
        // before any circuit stmts. Synthetic var IDs start after the last stmt var.
        if matches!(mode, StorageMode::Commitment) && !pre_init.is_empty() {
            let n_stmts = block.stmts.len() as u32;
            let mut syn_id = p as u32 + n_stmts;
            for seg in pre_init {
                let sid = seg.storage.0;
                let tid = seg.ty.0;
                let cell_tid = CirTyId(tid);
                let vw = cir_type_width(&cell_tid, types);
                for (local, c) in seg.data.iter().enumerate() {
                    let ci = seg.offset + local;
                    let val = c.lo;

                    // Emit constant value wire.
                    let val_id = syn_id; syn_id += 1;
                    let val_name = format!("_pinit_v_{}_{}_{}", sid, tid, ci);
                    if vw == 1 {
                        if val & 1 == 1 { self.emit_one(&val_name); } else { self.emit_zero(&val_name); }
                        self.wires.insert(val_id, WireRepr::Scalar(val_name));
                    } else {
                        let bits: Vec<String> = (0..vw).map(|j| {
                            let n = format!("{}_{}", val_name, j);
                            if (val >> j) & 1 == 1 { self.emit_one(&n); } else { self.emit_zero(&n); }
                            n
                        }).collect();
                        self.wires.insert(val_id, WireRepr::Vec(bits));
                    }

                    // Emit constant address wire (cell index ci, bit-decomposed).
                    let addr_id = syn_id; syn_id += 1;
                    let addr_name = format!("_pinit_a_{}_{}_{}", sid, tid, ci);
                    let addr_val = ci as u128;
                    // Use enough bits for the address — at least 1.
                    let aw = usize::max(1,
                        usize::BITS as usize - ci.saturating_sub(1).leading_zeros() as usize);
                    if aw == 1 {
                        if addr_val & 1 == 1 { self.emit_one(&addr_name); } else { self.emit_zero(&addr_name); }
                        self.wires.insert(addr_id, WireRepr::Scalar(addr_name));
                    } else {
                        let bits: Vec<String> = (0..aw).map(|j| {
                            let n = format!("{}_{}", addr_name, j);
                            if (addr_val >> j) & 1 == 1 { self.emit_one(&n); } else { self.emit_zero(&n); }
                            n
                        }).collect();
                        self.wires.insert(addr_id, WireRepr::Vec(bits));
                    }

                    // Record as a write at the start of the trace.
                    self.trace.entries.push(MemoryTraceEntry {
                        addr_var: addr_id,
                        value_var: val_id,
                        storage_id: sid,
                        type_id: tid,
                        is_write: true,
                        timestamp: self.mem_timestamp,
                    });
                    self.mem_timestamp += 1;
                }
            }
        }

        self.emit_circuit_stmts(block, types, mode);
    }

    /// Emit only the per-stmt gate computation.
    /// Caller must pre-populate `self.wires` (input wires) and `self.stor` (storage cells)
    /// before calling, then may read back `self.stor` for updated cell names after.
    fn emit_circuit_stmts(&mut self, block: &CirBlock, types: &CirTypes, mode: &StorageMode) {
        let p = block.params.len();

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

                Stmt::Poly { coeffs, constant, .. } => {
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
                    match mode {
                        StorageMode::Tree(_) => {
                            self.emit_storage_read(&out_name, storage.0, ty.0, addr, types, ty);
                            let w = cir_type_width(ty, types);
                            if w == 1 {
                                self.wires.insert(var_id, WireRepr::Scalar(out_name));
                            } else {
                                let bits: Vec<String> = (0..w).map(|j| format!("{}_{}", out_name, j)).collect();
                                self.wires.insert(var_id, WireRepr::Vec(bits));
                            }
                        }
                        StorageMode::Commitment => {
                            self.emit_storage_read_committed(
                                &out_name, var_id, storage.0, ty.0, addr, types, ty,
                            );
                            self.wires.insert(var_id, WireRepr::Scalar(out_name));
                        }
                    }
                }

                Stmt::StorageWrite { storage, src, ty, addr } => {
                    match mode {
                        StorageMode::Tree(_) => {
                            self.emit_storage_write(storage.0, ty.0, src, addr, types, ty);
                            self.emit_zero(&out_name);
                            self.wires.insert(var_id, WireRepr::Scalar(out_name));
                        }
                        StorageMode::Commitment => {
                            self.emit_storage_write_committed(
                                &out_name, storage.0, ty.0, src, addr,
                            );
                            self.emit_zero(&out_name);
                            self.wires.insert(var_id, WireRepr::Scalar(out_name));
                        }
                    }
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

                Stmt::Rng { ty, .. } => {
                    let r = self.ext_rng_counter;
                    self.ext_rng_counter += 1;
                    let w = cir_type_width(ty, types);
                    let prefix = if self.is_prover { "vope" } else { "q" };
                    if w == 1 {
                        let param_name = format!("{}_ext_rng_{}_bit_0", prefix, r);
                        self.stmts.push(IrStmt::Let {
                            pattern: IrPattern::ident(&out_name),
                            ty: None,
                            init: Some(clone_expr(var(&param_name))),
                        });
                        self.wires.insert(var_id, WireRepr::Scalar(out_name));
                    } else {
                        let bits: Vec<String> = (0..w).map(|j| {
                            let n = format!("{}_{}", out_name, j);
                            let param_name = format!("{}_ext_rng_{}_bit_{}", prefix, r, j);
                            self.stmts.push(IrStmt::Let {
                                pattern: IrPattern::ident(&n),
                                ty: None,
                                init: Some(clone_expr(var(&param_name))),
                            });
                            n
                        }).collect();
                        self.wires.insert(var_id, WireRepr::Vec(bits));
                    }
                }

                Stmt::OracleCall { output_tys, .. } => {
                    let k = self.ext_oracle_counter;
                    self.ext_oracle_counter += 1;
                    // Compute per-output bit offsets.
                    let mut offset = 0usize;
                    let mut bit_offsets = Vec::with_capacity(output_tys.len());
                    for ty in output_tys {
                        bit_offsets.push(offset);
                        offset += cir_type_width(ty, types);
                    }
                    self.ext_oracle_map.insert(var_id, (k, bit_offsets));
                    // Store a placeholder wire — never dereferenced for code gen.
                    let handle_name = format!("_oracle_handle_{}", k);
                    self.wires.insert(var_id, WireRepr::Scalar(handle_name));
                    // No stmts emitted for the call itself.
                }

                Stmt::OracleOutput { call, idx, ty } => {
                    let (k, bit_offsets) = self.ext_oracle_map[&call.0].clone();
                    let base = bit_offsets[*idx];
                    let w = cir_type_width(ty, types);
                    let prefix = if self.is_prover { "vope" } else { "q" };
                    if w == 1 {
                        let param_name = format!("{}_ext_oracle_{}_bit_{}", prefix, k, base);
                        self.stmts.push(IrStmt::Let {
                            pattern: IrPattern::ident(&out_name),
                            ty: None,
                            init: Some(clone_expr(var(&param_name))),
                        });
                        self.wires.insert(var_id, WireRepr::Scalar(out_name));
                    } else {
                        let bits: Vec<String> = (0..w).map(|j| {
                            let n = format!("{}_{}", out_name, j);
                            let param_name = format!("{}_ext_oracle_{}_bit_{}", prefix, k, base + j);
                            self.stmts.push(IrStmt::Let {
                                pattern: IrPattern::ident(&n),
                                ty: None,
                                init: Some(clone_expr(var(&param_name))),
                            });
                            n
                        }).collect();
                        self.wires.insert(var_id, WireRepr::Vec(bits));
                    }
                }

                Stmt::ActionCall { output_tys, .. } => {
                    let k = self.ext_action_counter;
                    self.ext_action_counter += 1;
                    let mut offset = 0usize;
                    let mut bit_offsets = Vec::with_capacity(output_tys.len());
                    for ty in output_tys {
                        bit_offsets.push(offset);
                        offset += cir_type_width(ty, types);
                    }
                    self.ext_action_map.insert(var_id, (k, bit_offsets));
                    let handle_name = format!("_action_handle_{}", k);
                    self.wires.insert(var_id, WireRepr::Scalar(handle_name));
                }

                Stmt::ActionOutput { call, idx, ty } => {
                    let (k, bit_offsets) = self.ext_action_map[&call.0].clone();
                    let base = bit_offsets[*idx];
                    let w = cir_type_width(ty, types);
                    let prefix = if self.is_prover { "vope" } else { "q" };
                    if w == 1 {
                        let param_name = format!("{}_ext_action_{}_bit_{}", prefix, k, base);
                        self.stmts.push(IrStmt::Let {
                            pattern: IrPattern::ident(&out_name),
                            ty: None,
                            init: Some(clone_expr(var(&param_name))),
                        });
                        self.wires.insert(var_id, WireRepr::Scalar(out_name));
                    } else {
                        let bits: Vec<String> = (0..w).map(|j| {
                            let n = format!("{}_{}", out_name, j);
                            let param_name = format!("{}_ext_action_{}_bit_{}", prefix, k, base + j);
                            self.stmts.push(IrStmt::Let {
                                pattern: IrPattern::ident(&n),
                                ty: None,
                                init: Some(clone_expr(var(&param_name))),
                            });
                            n
                        }).collect();
                        self.wires.insert(var_id, WireRepr::Vec(bits));
                    }
                }
                _ => panic!("emit_circuit_stmts: unhandled Stmt variant — add circuit emission for this variant"),
            }
        }
    }
}

/// Weave a single-block Volar IR circuit into a VOLE **prover** `IrModule`.
///
/// Uses [`StorageMode::Tree`] for backward compatibility.
pub fn weave_vole_prover_ir(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    storage_sizes: &StorageSizes,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let mode = StorageMode::Tree(storage_sizes.clone());
    weave_vole_prover_ir_with_mode(circuit, types, name, &mode, linkage).0
}

/// Weave a single-block Volar IR circuit into a VOLE **prover** `IrModule`,
/// with configurable [`StorageMode`].
///
/// Returns `(IrModule, MemoryTrace)`.  In [`StorageMode::Tree`] the trace is
/// empty.  In [`StorageMode::Commitment`] the trace records every read/write
/// for external verification.
pub fn weave_vole_prover_ir_with_mode(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
    linkage: Option<&LinkageSystem>,
) -> (IrModule<IrFunction>, MemoryTrace) {
    assert!(circuit.is_circuit(), "weave_vole_prover_ir: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let and_count = count_ir_ands(block, types, mode);
    let num_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads(block)
    } else { 0 };
    let (generics, where_clause) = prover_generics_and_where();

    let mut params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    for i in 0..num_params {
        params.push(IrParam { name: format!("w_{}", i), ty: vope_type() });
    }
    // Oracle read parameters (Commitment mode).
    for i in 0..num_oracle_reads {
        params.push(IrParam { name: format!("oracle_rd_{}", i), ty: vope_type() });
    }
    // External primitive parameters (oracle calls, action calls, rng).
    let ext = count_external_primitives(block, types);
    for (k, call) in ext.oracle_calls.iter().enumerate() {
        for j in 0..call.total_bits {
            params.push(IrParam { name: format!("vope_ext_oracle_{}_bit_{}", k, j), ty: vope_type() });
        }
    }
    for (k, call) in ext.action_calls.iter().enumerate() {
        for j in 0..call.total_bits {
            params.push(IrParam { name: format!("vope_ext_action_{}_bit_{}", k, j), ty: vope_type() });
        }
    }
    for (r, &width) in ext.rng_widths.iter().enumerate() {
        for j in 0..width {
            params.push(IrParam { name: format!("vope_ext_rng_{}_bit_{}", r, j), ty: vope_type() });
        }
    }

    let ret_type = IrType::Tuple(vec![vope_type(), hat_array_type(and_count)]);

    let mut ctx = VoleIrCtx::new(true);
    ctx.emit_circuit(block, types, mode, &circuit.pre_init);

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
    let trace = ctx.trace.clone();

    let func = IrFunction {
        name: format!("vole_prove_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.stmts,
            stmt_provs: vec![],
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_vole_ir_prover".into(),
        functions: vec![func],
        structs: vec![], enums: vec![], traits: vec![], impls: vec![], type_aliases: vec![],
 consts: vec![],
    };
    if let Some(ls) = linkage { ls.apply(&mut module); }
    (module, trace)
}

/// Weave a single-block Volar IR circuit into a VOLE **verifier** `IrModule`.
///
/// Uses [`StorageMode::Tree`] for backward compatibility.
pub fn weave_vole_verifier_ir(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    storage_sizes: &StorageSizes,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    let mode = StorageMode::Tree(storage_sizes.clone());
    weave_vole_verifier_ir_with_mode(circuit, types, name, &mode, linkage).0
}

/// Weave a single-block Volar IR circuit into a VOLE **verifier** `IrModule`,
/// with configurable [`StorageMode`].
pub fn weave_vole_verifier_ir_with_mode(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
    linkage: Option<&LinkageSystem>,
) -> (IrModule<IrFunction>, MemoryTrace) {
    assert!(circuit.is_circuit(), "weave_vole_verifier_ir: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let and_count = count_ir_ands(block, types, mode);
    let num_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads(block)
    } else { 0 };
    let (generics, where_clause) = verifier_generics_and_where();

    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
    ];
    for k in 0..and_count {
        params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
        params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
    }
    // Oracle read parameters (Commitment mode).
    for i in 0..num_oracle_reads {
        params.push(IrParam { name: format!("oracle_rd_{}", i), ty: q_type() });
    }
    // External primitive parameters (oracle calls, action calls, rng).
    let ext = count_external_primitives(block, types);
    for (k, call) in ext.oracle_calls.iter().enumerate() {
        for j in 0..call.total_bits {
            params.push(IrParam { name: format!("q_ext_oracle_{}_bit_{}", k, j), ty: q_type() });
        }
    }
    for (k, call) in ext.action_calls.iter().enumerate() {
        for j in 0..call.total_bits {
            params.push(IrParam { name: format!("q_ext_action_{}_bit_{}", k, j), ty: q_type() });
        }
    }
    for (r, &width) in ext.rng_widths.iter().enumerate() {
        for j in 0..width {
            params.push(IrParam { name: format!("q_ext_rng_{}_bit_{}", r, j), ty: q_type() });
        }
    }

    let ret_type = IrType::Tuple(vec![
        q_type(),
        IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool),
    ]);

    let mut ctx = VoleIrCtx::new(false);
    ctx.stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(IrExpr::Lit(volar_compiler::ir::IrLit::Bool(true))),
    });

    ctx.emit_circuit(block, types, mode, &circuit.pre_init);

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
    let trace = ctx.trace.clone();

    let func = IrFunction {
        name: format!("vole_verify_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.stmts,
            stmt_provs: vec![],
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_vole_ir_verifier".into(),
        functions: vec![func],
        structs: vec![], enums: vec![], traits: vec![], impls: vec![], type_aliases: vec![],
 consts: vec![],
    };
    if let Some(ls) = linkage { ls.apply(&mut module); }
    (module, trace)
}

// ============================================================================
// Network IR weaver — shared helpers
// ============================================================================

/// `Result<T, E>` — two-arg generic Result.
fn net_result_type(ok: IrType, err: IrType) -> IrType {
    IrType::Struct { kind: StructKind::Custom("Result".into()), type_args: vec![ok, err] }
}

/// `<Tr as volar_net::VoleTransport<N, T>>::Error`
fn net_tr_error_type() -> IrType {
    IrType::Projection {
        base: Box::new(IrType::TypeParam("Tr".into())),
        trait_path: Some("volar_net::VoleTransport".into()),
        trait_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
        assoc: AssociatedType::Other("Error".into()),
    }
}

/// `transport.METHOD(args...)?`
fn net_transport_try(method: &str, args: Vec<IrExpr>) -> IrExpr {
    IrExpr::Try(Box::new(IrExpr::MethodCall {
        receiver: Box::new(var("transport")),
        method: MethodKind::Other(method.into()),
        type_args: vec![],
        args,
    }))
}

/// `Ok(expr)`
fn net_ok_expr(inner: IrExpr) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path { segments: vec!["Ok".into()], type_args: vec![] }),
        args: vec![inner],
    }
}

/// `&[hat_0, ...]` — slice reference to fixed array of named wires.
fn net_hats_slice(hat_names: &[String]) -> IrExpr {
    ref_expr(IrExpr::FixedArray(hat_names.iter().map(|h| var(h)).collect()))
}

/// `volar_net::vope_bit(&wire)`
fn net_vope_bit_call(wire: &str) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["volar_net".into(), "vope_bit".into()],
            type_args: vec![],
        }),
        args: vec![ref_expr(var(wire))],
    }
}

/// `Q { q: Array::default() }` — zero Q value (verifier zero wire).
fn net_q_zero_expr() -> IrExpr {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Q".into()),
        type_args: vec![],
        fields: vec![("q".into(), IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["Array".into(), "default".into()],
                type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
            }),
            args: vec![],
        })],
        rest: None,
    }
}

/// `Vope { u: Array::default(), v: Array::default() }` — zero prover wire.
fn net_vope_zero_expr() -> IrExpr {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Vope".into()),
        type_args: vec![],
        fields: vec![
            ("u".into(), array_default()),
            ("v".into(), IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["Array".into(), "default".into()],
                    type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
                }),
                args: vec![],
            }),
        ],
        rest: None,
    }
}

fn net_vole_transport_bound() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::External { path: vec!["volar_net".into(), "VoleTransport".into()] },
        type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
        assoc_bindings: vec![],
    }
}

/// Generics and where clause for the net prover IR variants (adds Tr + PartialEq).
fn net_prover_ir_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    let (mut generics, mut wh) = prover_generics_and_where();
    generics.push(IrGenericParam {
        name: "Tr".into(),
        kind: IrGenericParamKind::Type,
        const_ty: None,
        bounds: vec![net_vole_transport_bound()],
        default: None,
    });
    // vope_bit requires PartialEq on T
    if let Some(IrWherePredicate::TypeBound { bounds, .. }) = wh.last_mut() {
        bounds.push(IrTraitBound {
            trait_kind: TraitKind::Math(MathTrait::PartialEq),
            type_args: vec![],
            assoc_bindings: vec![],
        });
    }
    (generics, wh)
}

/// Generics and where clause for the net verifier IR variants.
fn net_verifier_ir_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    let (mut generics, wh) = verifier_generics_and_where();
    generics.push(IrGenericParam {
        name: "Tr".into(),
        kind: IrGenericParamKind::Type,
        const_ty: None,
        bounds: vec![net_vole_transport_bound()],
        default: None,
    });
    (generics, wh)
}

fn net_transport_param() -> IrParam {
    IrParam {
        name: "transport".into(),
        ty: IrType::Reference {
            mutable: true,
            elem: Box::new(IrType::TypeParam("Tr".into())),
        },
    }
}

fn net_bool_type() -> IrType { IrType::Primitive(PrimitiveType::Bool) }

fn net_usize_type() -> IrType { IrType::Primitive(PrimitiveType::Usize) }

/// `&[Q<N,T>]` — slice type for q_ands.
fn net_q_slice_type() -> IrType {
    IrType::Reference {
        mutable: false,
        elem: Box::new(IrType::Array {
            kind: volar_compiler::ir::ArrayKind::Slice,
            elem: Box::new(q_type()),
            len: volar_compiler::ir::ArrayLength::Const(0),
        }),
    }
}

/// Emit `if is_first { then_expr } else { else_expr }` as a let statement.
fn net_if_first_let(name: &str, then_expr: IrExpr, else_expr: IrExpr) -> IrStmt {
    IrStmt::Let {
        pattern: IrPattern::ident(name),
        ty: None,
        init: Some(IrExpr::If {
            cond: Box::new(var("is_first")),
            then_branch: IrBlock { stmts: vec![], stmt_provs: vec![], expr: Some(Box::new(then_expr)) },
            else_branch: Some(Box::new(else_expr)),
        }),
    }
}

/// For each storage cell, push a `let _sinit_... = if is_first { const } else { param.clone() };`
/// into `stmts` and pre-populate `ctx.stor`. Returns ordered list of cell keys.
fn net_emit_conditional_storage_init(
    ctx: &mut VoleIrCtx,
    storage_sizes: &StorageSizes,
    pre_init: &[PreInitSegment],
    types: &CirTypes,
    is_prover: bool,
) -> Vec<(u32, u32, usize)> {
    let mut cell_keys: Vec<(u32, u32, usize)> = Vec::new();
    for (&(sid, tid), &count) in storage_sizes {
        let cell_tid = CirTyId(tid);
        let vw = cir_type_width(&cell_tid, types);
        for ci in 0..count {
            let name = format!("_sinit_{}_{}_{}", sid, tid, ci);
            let pi_val = lookup_pre_init_value(pre_init, sid, tid, ci);
            if vw == 1 {
                let bit = pi_val.map(|c| c.lo & 1 == 1).unwrap_or(false);
                let then_expr = if bit {
                    if is_prover { clone_expr(var("vope_one")) } else { clone_expr(var("q_one")) }
                } else {
                    if is_prover { net_vope_zero_expr() } else { net_q_zero_expr() }
                };
                let param_name = format!("{}_in", name);
                ctx.stmts.push(net_if_first_let(&name, then_expr, clone_expr(var(&param_name))));
                cell_keys.push((sid, tid, ci));
            } else {
                // Multi-bit: each sub-wire is a separate param.
                let val = pi_val.map(|c| c.lo).unwrap_or(0);
                for j in 0..vw {
                    let sub = format!("{}_{}", name, j);
                    let bit = (val >> j) & 1 == 1;
                    let then_expr = if bit {
                        if is_prover { clone_expr(var("vope_one")) } else { clone_expr(var("q_one")) }
                    } else {
                        if is_prover { net_vope_zero_expr() } else { net_q_zero_expr() }
                    };
                    let param_name = format!("{}_{}_in", name, j);
                    ctx.stmts.push(net_if_first_let(&sub, then_expr, clone_expr(var(&param_name))));
                }
                cell_keys.push((sid, tid, ci));
            }
            ctx.stor.insert((sid, tid, ci), name);
        }
    }
    cell_keys
}

/// Collect updated cell wire names from `ctx.stor` as back-edge args.
fn net_collect_cell_back_args(
    ctx: &VoleIrCtx,
    storage_sizes: &StorageSizes,
    types: &CirTypes,
) -> Vec<IrExpr> {
    let mut args: Vec<IrExpr> = Vec::new();
    for (&(sid, tid), &_count) in storage_sizes {
        let cell_tid = CirTyId(tid);
        let vw = cir_type_width(&cell_tid, types);
        let count = _count;
        for ci in 0..count {
            let name = &ctx.stor[&(sid, tid, ci)];
            if vw == 1 {
                args.push(clone_expr(var(name)));
            } else {
                for j in 0..vw {
                    args.push(clone_expr(var(&format!("{}_{}", name, j))));
                }
            }
        }
    }
    args
}

/// Build Block 1 params for storage cells (one or vw params per cell).
fn net_cell_block_params(
    storage_sizes: &StorageSizes,
    types: &CirTypes,
    wire_type: IrType,
) -> Vec<IrParam> {
    let mut params: Vec<IrParam> = Vec::new();
    for (&(sid, tid), &count) in storage_sizes {
        let cell_tid = CirTyId(tid);
        let vw = cir_type_width(&cell_tid, types);
        for ci in 0..count {
            let name = format!("_sinit_{}_{}_{}", sid, tid, ci);
            if vw == 1 {
                params.push(IrParam { name: format!("{}_in", name), ty: wire_type.clone() });
            } else {
                for j in 0..vw {
                    params.push(IrParam { name: format!("{}_{}_in", name, j), ty: wire_type.clone() });
                }
            }
        }
    }
    params
}

/// Build Block 0 → Block 1 dummy cell args (all zeros for prover/verifier).
fn net_cell_dummy_args(
    storage_sizes: &StorageSizes,
    types: &CirTypes,
    is_prover: bool,
) -> Vec<IrExpr> {
    let mut args: Vec<IrExpr> = Vec::new();
    for (&(_sid, tid), &count) in storage_sizes {
        let cell_tid = CirTyId(tid);
        let vw = cir_type_width(&cell_tid, types);
        for _ci in 0..count {
            let n = if vw == 1 { 1 } else { vw };
            for _ in 0..n {
                args.push(if is_prover { net_vope_zero_expr() } else { net_q_zero_expr() });
            }
        }
    }
    args
}

// ============================================================================
// Network IR weaver — flat variants
// ============================================================================

/// Weave a single-block Volar IR circuit into a VOLE **prover** network function (flat).
///
/// Generated signature:
/// ```text
/// fn vole_prove_net_ir_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     vope_one: Vope<N, T, U1>, w_0: Vope, ..., transport: &mut Tr,
/// ) -> Result<Vope<N, T, U1>, Tr::Error>
/// ```
pub fn weave_net_vole_prover_ir(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    storage_sizes: &StorageSizes,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    assert!(circuit.is_circuit(), "weave_net_vole_prover_ir: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let mode = StorageMode::Tree(storage_sizes.clone());
    let (generics, where_clause) = net_prover_ir_generics_and_where();

    let mut params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    for i in 0..num_params {
        params.push(IrParam { name: format!("w_{}", i), ty: vope_type() });
    }
    params.push(net_transport_param());

    let ret_type = net_result_type(vope_type(), net_tr_error_type());

    let mut ctx = VoleIrCtx::new(true);
    ctx.emit_circuit(block, types, &mode, &circuit.pre_init);

    let hats_ref = net_hats_slice(&ctx.hat_names);
    ctx.stmts.push(IrStmt::Semi(net_transport_try("send_hats", vec![hats_ref])));
    ctx.stmts.push(IrStmt::Semi(net_transport_try("recv_verdict", vec![])));

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => args,
        _ => panic!("weave_net_vole_prover_ir: expected Jmp(Return)"),
    };
    let output_expr = if ret_args.len() == 1 {
        clone_expr(var(ctx.scalar(&ret_args[0])))
    } else {
        IrExpr::Tuple(ret_args.iter().map(|v| clone_expr(var(ctx.scalar(v)))).collect())
    };

    let func = IrFunction {
        name: format!("vole_prove_net_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.stmts,
            stmt_provs: vec![],
            expr: Some(Box::new(net_ok_expr(output_expr))),
        },
        external_kind: ExternalKind::Normal,
    };
    let mut module = IrModule {
        name: format!("weaved_net_prover_ir_{}", name),
        functions: vec![func],
        structs: vec![], enums: vec![], traits: vec![], impls: vec![],
        type_aliases: vec![], consts: vec![],
    };
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

/// Weave a single-block Volar IR circuit into a VOLE **verifier** network function (flat).
///
/// Generated signature:
/// ```text
/// fn vole_verify_net_ir_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     delta: &Delta<N,T>, q_and_0: Q, hat_0: Array<T,N>, ..., q_one: Q, w_0: Q, ..., transport: &mut Tr,
/// ) -> Result<bool, Tr::Error>
/// ```
pub fn weave_net_vole_verifier_ir(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    storage_sizes: &StorageSizes,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    assert!(circuit.is_circuit(), "weave_net_vole_verifier_ir: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let mode = StorageMode::Tree(storage_sizes.clone());
    let and_count = count_ir_ands(block, types, &mode);
    let (generics, where_clause) = net_verifier_ir_generics_and_where();

    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
    ];
    for k in 0..and_count {
        params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
        params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
    }
    params.push(net_transport_param());

    let ret_type = net_result_type(net_bool_type(), net_tr_error_type());

    let mut ctx = VoleIrCtx::new(false);
    ctx.stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(IrExpr::Lit(IrLit::Bool(true))),
    });
    ctx.emit_circuit(block, types, &mode, &circuit.pre_init);

    ctx.stmts.push(IrStmt::Semi(net_transport_try("send_verdict", vec![var("all_ok")])));

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => args,
        _ => panic!("weave_net_vole_verifier_ir: expected Jmp(Return)"),
    };
    let output_expr = if ret_args.len() == 1 {
        clone_expr(var(ctx.scalar(&ret_args[0])))
    } else {
        IrExpr::Tuple(ret_args.iter().map(|v| clone_expr(var(ctx.scalar(v)))).collect())
    };

    let func = IrFunction {
        name: format!("vole_verify_net_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.stmts,
            stmt_provs: vec![],
            expr: Some(Box::new(net_ok_expr(output_expr))),
        },
        external_kind: ExternalKind::Normal,
    };
    let mut module = IrModule {
        name: format!("weaved_net_verifier_ir_{}", name),
        functions: vec![func],
        structs: vec![], enums: vec![], traits: vec![], impls: vec![],
        type_aliases: vec![], consts: vec![],
    };
    if let Some(ls) = linkage { ls.apply(&mut module); }
    module
}

// ============================================================================
// Network IR weaver — loop variants
// ============================================================================

/// Weave a single-block Volar IR circuit into a VOLE **prover** CFG loop with
/// streaming transport.
///
/// Storage is initialized on the first iteration via an `is_first: bool`
/// CFG parameter; subsequent iterations carry the updated cell values as
/// loop state.
///
/// Generated signature:
/// ```text
/// fn vole_prove_net_ir_loop_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     vope_one: Vope, init_w_0: Vope, ..., transport: &mut Tr,
/// ) -> Result<Vope, Tr::Error>
/// ```
pub fn weave_net_vole_prover_ir_loop(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    storage_sizes: &StorageSizes,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_circuit(), "weave_net_vole_prover_ir_loop: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let mode = StorageMode::Tree(storage_sizes.clone());
    let (generics, where_clause) = net_prover_ir_generics_and_where();

    // Function-level params.
    let mut func_params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_w{}", i), ty: vope_type() });
    }
    func_params.push(net_transport_param());

    let ret_type = net_result_type(vope_type(), net_tr_error_type());

    // ── Block 0: entry ────────────────────────────────────────────────────────
    let mut b0_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("init_w{}", i))))
        .collect();
    b0_args.extend(net_cell_dummy_args(storage_sizes, types, true));
    b0_args.push(IrExpr::Lit(IrLit::Bool(true))); // is_first = true
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: loop body ─────────────────────────────────────────────────
    let mut b1_params: Vec<IrParam> = (0..num_params)
        .map(|i| IrParam { name: format!("w{}", i), ty: vope_type() })
        .collect();
    b1_params.extend(net_cell_block_params(storage_sizes, types, vope_type()));
    b1_params.push(IrParam { name: "is_first".into(), ty: net_bool_type() });

    let mut ctx = VoleIrCtx::new(true);

    // Register circuit input wires.
    for i in 0..num_params {
        ctx.wires.insert(i as u32, WireRepr::Scalar(format!("w{}", i)));
    }

    // Conditional storage init: if is_first → const else → passed-in param.
    net_emit_conditional_storage_init(&mut ctx, storage_sizes, &circuit.pre_init, types, true);

    // Gate computation.
    ctx.emit_circuit_stmts(block, types, &mode);

    // Done bit.
    let ret_args = match &block.terminator {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => args,
        _ => panic!("weave_net_vole_prover_ir_loop: expected Jmp(Return)"),
    };
    let done_wire = ctx.scalar(ret_args.last().expect("ret_args must be non-empty"));
    ctx.stmts.push(IrStmt::Let {
        pattern: IrPattern::ident("done_bit"),
        ty: None,
        init: Some(net_vope_bit_call(done_wire)),
    });

    // Send iteration.
    let hats_ref = net_hats_slice(&ctx.hat_names);
    ctx.stmts.push(IrStmt::Semi(net_transport_try(
        "send_iteration",
        vec![hats_ref, var("done_bit")],
    )));

    // Back-edge args: next circuit inputs + updated cells + is_first=false.
    let next_state_args: Vec<IrExpr> = ret_args[..ret_args.len().saturating_sub(1)]
        .iter()
        .map(|v| clone_expr(var(ctx.scalar(v))))
        .collect();
    let output_wire = ctx.scalar(&ret_args[0]).to_string();
    let mut back_args = next_state_args;
    back_args.extend(net_collect_cell_back_args(&ctx, storage_sizes, types));
    back_args.push(IrExpr::Lit(IrLit::Bool(false))); // is_first = false

    let b1 = IrCfgBlock {
        params: b1_params,
        stmts: ctx.stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("done_bit"),
            then_: IrCfgJump { target: 2, args: vec![clone_expr(var(&output_wire))] },
            else_: IrCfgJump { target: 1, args: back_args },
        },
    };

    // ── Block 2: exit ─────────────────────────────────────────────────────
    let b2 = IrCfgBlock {
        params: vec![IrParam { name: "output".into(), ty: vope_type() }],
        stmts: vec![IrStmt::Semi(net_transport_try("recv_verdict", vec![]))],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(net_ok_expr(var("output")))),
    };

    let func = IrCfgFunction {
        name: format!("vole_prove_net_ir_loop_{}", name),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0, b1, b2] },
    };
    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_net_prover_ir_loop_{}", name),
        functions: vec![IrAnyFunction::Cfg(func)],
        structs: vec![], enums: vec![], traits: vec![], impls: vec![],
        type_aliases: vec![], consts: vec![],
    };
    if let Some(ls) = linkage { ls.apply_cfg(&mut module); }
    module
}

/// Weave a single-block Volar IR circuit into a VOLE **verifier** CFG loop with
/// streaming transport.
///
/// The `is_first: bool` CFG parameter gates storage initialization on the first
/// iteration. AND Q-shares are supplied pre-allocated in `q_ands[iter*AND_COUNT+k]`.
///
/// Generated signature:
/// ```text
/// fn vole_verify_net_ir_loop_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     q_one: Q, delta: Delta, q_ands: &[Q], init_q_0: Q, ..., transport: &mut Tr,
/// ) -> Result<bool, Tr::Error>
/// ```
pub fn weave_net_vole_verifier_ir_loop(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    storage_sizes: &StorageSizes,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_circuit(), "weave_net_vole_verifier_ir_loop: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let mode = StorageMode::Tree(storage_sizes.clone());
    let and_count = count_ir_ands(block, types, &mode);
    let (generics, where_clause) = net_verifier_ir_generics_and_where();

    // Function-level params.
    let mut func_params: Vec<IrParam> = vec![
        IrParam { name: "q_one".into(), ty: q_type() },
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
        IrParam { name: "q_ands".into(), ty: net_q_slice_type() },
    ];
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_q{}", i), ty: q_type() });
    }
    func_params.push(net_transport_param());

    let ret_type = net_result_type(net_bool_type(), net_tr_error_type());

    // ── Block 0: entry ───────────────────────────────────────────────────────
    let mut b0_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("init_q{}", i))))
        .collect();
    b0_args.extend(net_cell_dummy_args(storage_sizes, types, false));
    b0_args.push(IrExpr::Lit(IrLit::Bool(true)));  // all_ok = true
    b0_args.push(IrExpr::Lit(IrLit::Int(0)));       // iter = 0
    b0_args.push(IrExpr::Lit(IrLit::Bool(true)));   // is_first = true
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: loop body ─────────────────────────────────────────────────
    let mut b1_params: Vec<IrParam> = (0..num_params)
        .map(|i| IrParam { name: format!("q{}", i), ty: q_type() })
        .collect();
    b1_params.extend(net_cell_block_params(storage_sizes, types, q_type()));
    b1_params.push(IrParam { name: "all_ok".into(), ty: net_bool_type() });
    b1_params.push(IrParam { name: "iter".into(), ty: net_usize_type() });
    b1_params.push(IrParam { name: "is_first".into(), ty: net_bool_type() });

    let mut ctx = VoleIrCtx::new(false);

    // Receive hats from transport.
    ctx.stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident("iter_hats"),
            IrPattern::ident("is_sentinel"),
        ]),
        ty: None,
        init: Some(net_transport_try("recv_iteration", vec![
            IrExpr::Lit(IrLit::Int(and_count as i128)),
        ])),
    });

    // Mutable all_ok accumulator for this iteration.
    ctx.stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(var("all_ok")),
    });

    // Pre-bind q_and_k and hat_k so emit_circuit_stmts can reference them.
    for k in 0..and_count {
        let q_and_idx = IrExpr::Binary {
            op: SpecBinOp::Add,
            left: Box::new(IrExpr::Binary {
                op: SpecBinOp::Mul,
                left: Box::new(var("iter")),
                right: Box::new(IrExpr::Lit(IrLit::Int(and_count as i128))),
            }),
            right: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
        };
        ctx.stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&format!("q_and_{}", k)),
            ty: None,
            init: Some(clone_expr(IrExpr::Index {
                base: Box::new(var("q_ands")),
                index: Box::new(q_and_idx),
            })),
        });
        ctx.stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&format!("hat_{}", k)),
            ty: None,
            init: Some(clone_expr(IrExpr::Index {
                base: Box::new(var("iter_hats")),
                index: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
            })),
        });
    }

    // Register circuit input wires.
    for i in 0..num_params {
        ctx.wires.insert(i as u32, WireRepr::Scalar(format!("q{}", i)));
    }

    // Conditional storage init.
    net_emit_conditional_storage_init(&mut ctx, storage_sizes, &circuit.pre_init, types, false);

    // Gate computation.
    ctx.emit_circuit_stmts(block, types, &mode);

    // Extract back-edge args from terminator.
    let ret_args = match &block.terminator {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => args,
        _ => panic!("weave_net_vole_verifier_ir_loop: expected Jmp(Return)"),
    };

    let mut back_args: Vec<IrExpr> = ret_args[..ret_args.len().saturating_sub(1)]
        .iter()
        .map(|v| clone_expr(var(ctx.scalar(v))))
        .collect();
    back_args.extend(net_collect_cell_back_args(&ctx, storage_sizes, types));
    back_args.push(var("all_ok")); // accumulated all_ok
    back_args.push(IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(var("iter")),
        right: Box::new(IrExpr::Lit(IrLit::Int(1))),
    });
    back_args.push(IrExpr::Lit(IrLit::Bool(false))); // is_first = false

    let b1 = IrCfgBlock {
        params: b1_params,
        stmts: ctx.stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("is_sentinel"),
            then_: IrCfgJump { target: 2, args: vec![var("all_ok")] },
            else_: IrCfgJump { target: 1, args: back_args },
        },
    };

    // ── Block 2: exit ─────────────────────────────────────────────────────
    let b2 = IrCfgBlock {
        params: vec![IrParam { name: "final_ok".into(), ty: net_bool_type() }],
        stmts: vec![IrStmt::Semi(net_transport_try("send_verdict", vec![var("final_ok")]))],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(net_ok_expr(var("final_ok")))),
    };

    let func = IrCfgFunction {
        name: format!("vole_verify_net_ir_loop_{}", name),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0, b1, b2] },
    };
    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_net_verifier_ir_loop_{}", name),
        functions: vec![IrAnyFunction::Cfg(func)],
        structs: vec![], enums: vec![], traits: vec![], impls: vec![],
        type_aliases: vec![], consts: vec![],
    };
    if let Some(ls) = linkage { ls.apply_cfg(&mut module); }
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
pub fn print_weaved_vole_module(module: &IrModule<IrFunction>) -> String {
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
            stmt_provs: std::vec![],
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
                Stmt::Poly { ty: bit, coeffs, constant: CirConst { hi: 0, lo: 0 } },
            ],
            stmt_provs: std::vec![()],
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
            stmt_provs: std::vec![(), ()],
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

    // ---- Commitment-mode tests -------------------------------------------

    #[test]
    fn test_weave_vole_ir_prover_committed() {
        let (circuit, types, _ss) = build_ir_storage_circuit();
        let mode = StorageMode::Commitment;
        let (module, trace) = weave_vole_prover_ir_with_mode(
            &circuit, &types, "committed", &mode, None,
        );
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_ir_prover_commit");
        // Commitment mode should produce a non-empty trace.
        assert!(
            !trace.entries.is_empty(),
            "Commitment mode must produce a memory trace",
        );
        // The trace should have 1 write + 1 read = 2 entries.
        assert_eq!(trace.entries.len(), 2);
        assert!(trace.entries[0].is_write);
        assert!(!trace.entries[1].is_write);
    }

    #[test]
    fn test_weave_vole_ir_verifier_committed() {
        let (circuit, types, _ss) = build_ir_storage_circuit();
        let mode = StorageMode::Commitment;
        let (module, trace) = weave_vole_verifier_ir_with_mode(
            &circuit, &types, "committed", &mode, None,
        );
        let code = print_weaved_vole_module(&module);
        run_compile_check(&code, "vole_ir_verifier_commit");
        assert_eq!(trace.entries.len(), 2);
    }

    #[test]
    fn test_commitment_mode_zero_ands() {
        let (circuit, types, ss) = build_ir_storage_circuit();
        let block = &circuit.blocks[0];
        // Tree mode: should have AND gates for storage.
        let tree_ands = count_ir_ands(block, &types, &StorageMode::Tree(ss));
        assert!(tree_ands > 0, "Tree mode should have AND gates");
        // Commitment mode: 0 AND gates for storage.
        let commit_ands = count_ir_ands(block, &types, &StorageMode::Commitment);
        assert_eq!(commit_ands, 0, "Commitment mode should have 0 AND gates");
    }
}
