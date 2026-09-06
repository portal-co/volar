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
        IrCfgJump, IrCfgModule, IrCfgTerminator, IrExpr, IrExprKind, IrFunction, IrGenericParam,
        IrGenericParamKind, IrLit, IrModule, IrParam, IrPattern, IrStmt, IrStmtKind, IrTraitBound,
        IrType, IrWherePredicate, MathTrait, MethodKind, PrimitiveType, SpecBinOp, SpecUnaryOp,
        StdMethod, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir::ir::{
    IRBlocks, IRBlock as CirBlock, IRBlockTargetId, IRStmt, IRTerminator,
    IRType as CircuitIrType, IRTypeId as CirTyId, IRTypes as CirTypes,
    IRVarId as CirVar, PrimType, PreInitSegment, Stmt, StorageId, IRBranchTarget};
use volar_ir::public::PublicSet;
use volar_ir_common::PolyCoeffs;
use volar_ir_passes::lower_to_circuit::lower_to_circuit;
pub use volar_ir_passes::lower_to_circuit::LoweringMode;
use volar_discipline::{Tagged, Zk, Transparent};

use crate::{array_default, build_return, clone_expr, expand_ors, ref_expr, var, NoProvenance, ProvenanceHandler};

/// Construct a fresh `IrExpr` with default provenance and no side — for
/// genuinely fresh-construction sites with no natural source node to
/// inherit from. Generic over `Q` so it also serves Quicksilver-generic
/// (`Q: Clone + Default`) gate-emission helpers in this file.
fn ir_expr<Q: Clone + Default>(kind: IrExprKind<Q>) -> IrExpr<Q> {
    IrExpr::new(kind, Q::default(), None)
}

/// Construct a fresh `IrStmt` with default provenance and no side.
fn ir_stmt<Q: Clone + Default>(kind: IrStmtKind<Q>) -> IrStmt<Q> {
    IrStmt::new(kind, Q::default(), None)
}

/// Construct an `IrExpr` carrying an explicit provenance value (no side) —
/// used where the surrounding code already has a meaningful per-statement
/// provenance value (e.g. a mapped `handler.map(prov)` result) to attribute
/// instead of inventing one.
fn ir_expr_p<Q: Clone>(kind: IrExprKind<Q>, prov: Q) -> IrExpr<Q> {
    IrExpr::new(kind, prov, None)
}

/// Construct an `IrStmt` carrying an explicit provenance value (no side).
fn ir_stmt_p<Q: Clone>(kind: IrStmtKind<Q>, prov: Q) -> IrStmt<Q> {
    IrStmt::new(kind, prov, None)
}

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

/// `[Q<N, T>; AND_COUNT]` — fixed-size array of derived `q_and` values
/// returned by a `QSim` function (Milestone 1.6) — the `Q`-typed
/// counterpart of [`hat_array_type`].
fn q_and_array_type(and_count: usize) -> IrType {
    IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray,
        elem: Box::new(q_type()),
        len: volar_compiler::ir::ArrayLength::Const(and_count),
    }
}

/// `[{fold_scalar_type_name}; AND_COUNT]` — fixed-size array of per-gate
/// fold challenges, array-batched on the *param* side to stay under
/// `rustc`'s hard 65535-argument function limit (a real interpreter's
/// largest split-weave chunk function needs hundreds of thousands of
/// per-gate params if left scalar-per-gate — see
/// `docs/agent-context/circuit-size-optimization-backlog.md`). Unlike
/// `hat`/`q_and`, `r_and` never has a return-side counterpart (it's a
/// per-gate fold-challenge input only), so this is the params-only sibling
/// of `hat_array_type`/`q_and_array_type`.
fn r_and_array_type(and_count: usize, fold_scalar_type_name: &str) -> IrType {
    IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray,
        elem: Box::new(IrType::TypeParam(fold_scalar_type_name.into())),
        len: volar_compiler::ir::ArrayLength::Const(and_count),
    }
}

/// `[elem; n]` — generic fixed-size array param type, the params-side
/// batching every wide entry-state (`w_i`) / cross-block export
/// (`is_active_i`/`next_state_i_k`/etc., via `bind_scalar`) param uses
/// instead of `n` separate scalar params (same 65535-arg-limit reason as
/// `hat_array_type`/`q_and_array_type`/`r_and_array_type` above).
fn wide_array_type(elem: IrType, n: usize) -> IrType {
    IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray,
        elem: Box::new(elem),
        len: volar_compiler::ir::ArrayLength::Const(n),
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

/// `T: Invert` bound (custom, not a `MathTrait` variant) -- required by
/// `derive_and_q` (`volar_spec::vole::setup::derive_and_q`), which inverts
/// `Δ`. Milestone 1.6's `QSim` role is the only weaver output that calls
/// `derive_and_q`, hence the only one needing this bound.
fn invert_t() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::Custom("Invert".into()),
        type_args: vec![],
        assoc_bindings: vec![],
    }
}

/// Generic params and where clause for `QSim` (adds `T: Invert`, needed by
/// `derive_and_q` -- see [`invert_t`]). Deliberately does *not* add
/// `PartialEq` (unlike [`verifier_generics_and_where`]): `QSim` never
/// calls `vole_and_verifier_check`.
fn qsim_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    let (generics, mut where_clause) = prover_generics_and_where();
    if let Some(IrWherePredicate::TypeBound { bounds, .. }) = where_clause.last_mut() {
        bounds.push(invert_t());
    }
    (generics, where_clause)
}

// ============================================================================
// Expression helpers for VOLE
// ============================================================================

/// `Array::<T, N>::from_fn(|{idx}| {body})`
fn array_t_from_fn<P: Clone>(idx: &str, body: IrExpr<P>) -> IrExpr<P> {
    let prov = body.prov.clone();
    let side = body.side;
    IrExpr::new(
        IrExprKind::Call {
            func: Box::new(IrExpr::new(
                IrExprKind::Path {
                    segments: vec!["Array".into(), "from_fn".into()],
                    type_args: vec![
                        IrType::TypeParam("T".into()),
                        IrType::TypeParam("N".into()),
                    ],
                },
                prov.clone(),
                side,
            )),
            args: vec![IrExpr::new(
                IrExprKind::Closure {
                    params: vec![volar_compiler::ir::IrClosureParam {
                        pattern: IrPattern::ident(idx),
                        ty: None,
                    }],
                    ret_type: None,
                    body: Box::new(body),
                },
                prov.clone(),
                side,
            )],
        },
        prov,
        side,
    )
}

/// `core::array::from_fn(|{idx}| {body})` — a plain fixed-size `[Elem; W]`
/// Rust array, **not** `hybrid_array::Array<T,N>` (that's the unrelated
/// VOLE-repetition dimension `N` — [`array_t_from_fn`] above). This is what
/// [`VoleIrCtx::emit_poly_wide`] uses for its own bit-width dimension `W`.
fn fixed_array_from_fn<P: Clone>(idx: &str, body: IrExpr<P>) -> IrExpr<P> {
    let prov = body.prov.clone();
    let side = body.side;
    IrExpr::new(
        IrExprKind::Call {
            func: Box::new(IrExpr::new(
                IrExprKind::Path {
                    segments: vec!["core".into(), "array".into(), "from_fn".into()],
                    type_args: vec![],
                },
                prov.clone(),
                side,
            )),
            args: vec![IrExpr::new(
                IrExprKind::Closure {
                    params: vec![volar_compiler::ir::IrClosureParam {
                        pattern: IrPattern::ident(idx),
                        ty: None,
                    }],
                    ret_type: None,
                    body: Box::new(body),
                },
                prov.clone(),
                side,
            )],
        },
        prov,
        side,
    )
}

/// `Array::<T, N>::default()` — the zero vector in the extension field.
fn array_t_default<P: Clone + Default>() -> IrExpr<P> {
    ir_expr(IrExprKind::Call {
        func: Box::new(ir_expr(IrExprKind::Path {
            segments: vec!["Array".into(), "default".into()],
            type_args: vec![
                IrType::TypeParam("T".into()),
                IrType::TypeParam("N".into()),
            ],
        })),
        args: vec![],
    })
}

/// `wire.q[i]` — the verifier's Q share lane.
fn q_index<P: Clone + Default>(wire_name: &str, idx: &str) -> IrExpr<P> {
    q_index_of(var(wire_name), idx)
}

/// Same as [`q_index`], but takes an already-built base expression (e.g.
/// a `_pool` index reference) instead of a bare name.
fn q_index_of<P: Clone + Default>(base: IrExpr<P>, idx: &str) -> IrExpr<P> {
    ir_expr(IrExprKind::Index {
        base: Box::new(ir_expr(IrExprKind::Field {
            base: Box::new(base),
            field: "q".into(),
        })),
        index: Box::new(var(idx)),
    })
}

/// `base_name[idx]` — plain array indexing (no `.field` wrapper), for the
/// raw fixed-size arrays [`crate::vole::emit_poly_wide`] bundles wide
/// operands and per-lane AND-check parameters into.
fn arr_index<P: Clone + Default>(base_name: &str, idx: &str) -> IrExpr<P> {
    ir_expr(IrExprKind::Index {
        base: Box::new(var(base_name)),
        index: Box::new(var(idx)),
    })
}

/// `delta.delta[i]`
fn delta_index<P: Clone + Default>(idx: &str) -> IrExpr<P> {
    ir_expr(IrExprKind::Index {
        base: Box::new(ir_expr(IrExprKind::Field {
            base: Box::new(var("delta")),
            field: "delta".into(),
        })),
        index: Box::new(var(idx)),
    })
}

/// `Q { q: {body} }`
fn q_struct<P: Clone>(body: IrExpr<P>) -> IrExpr<P> {
    let prov = body.prov.clone();
    let side = body.side;
    IrExpr::new(
        IrExprKind::StructExpr {
            kind: StructKind::Custom("Q".into()),
            type_args: vec![],
            fields: vec![("q".into(), body)],
            rest: None,
        },
        prov,
        side,
    )
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
// Side-based witness/statement protection (replaces ZkWitnessConfig/
// ZkActionConfig — see docs/side.md)
// ============================================================================

/// What a VOLE-side value is: a private committed witness (`Vope`/`Q`), or a
/// public cleartext statement value (`bool`). The `volar-side` vocabulary for
/// this weaver — resolved from a [`SideId`] by any [`SideHandler`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum VoleProtection {
    /// Private, VOLE-committed value (`Vope` for the prover, `Q` for the verifier).
    Witness,
    /// Public cleartext value (`bool`), known to both parties.
    Statement,
}

/// Side assignment for a VOLE circuit's introduction points: circuit input
/// params and action output bits.
///
/// `BIrBlock` params aren't individually `Node`-wrapped (`params` is just a
/// count), and an action's output bits have no IR node of their own to carry
/// a side until the action call is lowered — so, exactly like the
/// `WaffleImportConfig` extension point for WASM-sourced oracle/action
/// imports, these introduction points take an explicit side from the weaver
/// caller rather than reading one off the IR.
#[derive(Clone, Debug, Default)]
pub struct VoleSideAssignments {
    /// Side for circuit input param `i` (0-based), if assigned.
    pub input_sides: BTreeMap<u32, volar_side::SideId>,
    /// Side for action `name`'s output bit `j` (0-based), if assigned.
    pub action_sides: BTreeMap<String, BTreeMap<usize, volar_side::SideId>>,
}

impl VoleSideAssignments {
    /// Assign `side` to circuit input param `idx`, returning `self` for chaining.
    pub fn with_input(mut self, idx: u32, side: volar_side::SideId) -> Self {
        self.input_sides.insert(idx, side);
        self
    }

    /// Assign `side` to action `name`'s output bit `bit`, returning `self` for chaining.
    pub fn with_action_output(mut self, name: &str, bit: usize, side: volar_side::SideId) -> Self {
        self.action_sides.entry(name.into()).or_default().insert(bit, side);
        self
    }
}

/// Source of witness/statement decisions for [`weave_vole_prover_inner`]/
/// [`weave_vole_verifier_inner`] — implemented by the legacy [`ZkWitnessConfig`]
/// and by the side-based [`VoleSideAssignments`] + [`SideHandler`] pairing, so
/// the weaving logic itself doesn't need to know which is in use.
///
/// Generic over the provenance type `Prov` (defaulted to `()`) purely so
/// [`trace_sink`](Self::trace_sink) can return a
/// [`VerifierTraceSink<Prov>`] tied to the *same* `Prov` the calling weave
/// function uses (`H::Output`) — `ZkWitnessConfig`/`VoleSideConfig` don't
/// otherwise care about `Prov` at all (their impls are blanket over it), so
/// this adds no burden on existing callers; only
/// [`weave_vole_verifier_inner`]'s bound spells out `VoleWitnessSource<H::Output>`
/// explicitly.
trait VoleWitnessSource<Prov: Clone + Default = ()> {
    fn is_public_input(&self, idx: u32) -> bool;
    fn is_public_action_output(&self, action_name: &str, bit: usize) -> bool;

    /// Weave-time trace-assembly plugin (see [`VerifierTraceSink`]). Default
    /// `None` — byte-identical woven output to today.
    fn trace_sink(&self) -> Option<&dyn VerifierTraceSink<Prov>> {
        None
    }
}

impl<Prov: Clone + Default> VoleWitnessSource<Prov> for ZkWitnessConfig {
    fn is_public_input(&self, idx: u32) -> bool {
        self.public_inputs.is_public(CirVar(idx))
    }
    fn is_public_action_output(&self, action_name: &str, bit: usize) -> bool {
        self.action_configs
            .get(action_name)
            .map(|c| c.is_output_public(bit))
            .unwrap_or(false)
    }
}

/// Pairs a [`VoleSideAssignments`] table with a [`SideHandler`] to resolve
/// witness/statement decisions — the side-based [`VoleWitnessSource`].
struct VoleSideConfig<'a, H> {
    assignments: &'a VoleSideAssignments,
    handler: &'a H,
}

impl<Prov: Clone + Default, H: volar_side::SideHandler<Protection = VoleProtection>> VoleWitnessSource<Prov> for VoleSideConfig<'_, H> {
    fn is_public_input(&self, idx: u32) -> bool {
        let side = self.assignments.input_sides.get(&idx).copied();
        self.handler.protection(side) == VoleProtection::Statement
    }
    fn is_public_action_output(&self, action_name: &str, bit: usize) -> bool {
        let side = self.assignments.action_sides.get(action_name).and_then(|m| m.get(&bit)).copied();
        self.handler.protection(side) == VoleProtection::Statement
    }
}

// ============================================================================
// Weave-time dynamic trace assembly (prove-the-verifier: see
// `docs/prove-the-verifier-iop.md`)
// ============================================================================

/// A weave-time plugin that threads a typed fold-accumulator state through
/// the woven VOLE verifier, updated once per AND gate alongside the existing
/// `all_ok` check — real typed IR (`AGENTS.md` rule 1: never raw strings as
/// expression data), not a bare externally-named hook and not implicit
/// global mutation. This is what makes trace/fold assembly *dynamic*: the
/// accumulator is built in step with each gate check, inside the verifier's
/// own loop, so its size never depends on how many gates ran — including a
/// hidden/variable-length loop (`hybrid_net.rs`/`storage_loop.rs`), which is
/// the whole point of folding the verifier at all.
///
/// Intentionally generic: prove-the-verifier folding (see [`IopSink`]) is
/// one instantiation of this extension point, not the only possible one.
///
/// # Bare, externally-resolved names are the pluggable seam
///
/// A gate's `K_a, K_b, K_c, Δ, V̂` live in the VOLE field `T` (e.g. GF(2^8),
/// `N` parallel lanes); a fold implementation needs them lifted into
/// whatever field its accumulator uses. This trait does **not** choose that
/// embedding: implementations emit calls to *externally-resolved* names
/// (e.g. `IopChallenge`, `IopAccumulator`, `iop_accumulator_fresh`,
/// `iop_fold_gate`) that are bare, unresolved identifiers in the woven IR —
/// deliberately not declared as generic parameters of the woven function,
/// so ordinary Rust name resolution requires *whoever compiles the woven
/// output* to supply concrete definitions (a `type IopChallenge = …;`
/// alias, an `iop_fold_gate` function, etc.). That is the pluggable seam:
/// this crate (compile-time) only guarantees the *call sites* are correctly
/// threaded; the harness (run-time, see
/// `crates/iop/volar-verifier-iop-runtime`) supplies the meaning.
///
/// Generic over the provenance type `P` **at the trait level**, not per
/// method — `and_gate_step` needs to hand back `IrExpr<P>`, and a per-method
/// generic would make `dyn VerifierTraceSink<P>` uncompilable (trait objects
/// can't have generic methods). `weave_vole_verifier_inner`'s own `P`/
/// `H::Output` is known at every call site, so `dyn VerifierTraceSink<H::Output>`
/// is what [`VoleWitnessSource::trace_sink`] actually returns.
pub trait VerifierTraceSink<P: Clone + Default> {
    /// Bare, externally-resolved type name for the threaded fold-accumulator
    /// state (e.g. `"IopAccumulator"`).
    fn state_type_name(&self) -> &str;

    /// Bare, externally-resolved function name producing the initial
    /// ("fresh") accumulator state: `fn() -> {state_type_name}`.
    fn init_state_fn_name(&self) -> &str;

    /// Emit statements folding this AND gate's *actual* IR variables
    /// (`k_a`, `k_b`, `k_c`, `delta` — real variable names bound in the
    /// woven function, not string placeholders; `hat`/`r_and` are
    /// already-built read expressions — a bare param reference for the
    /// legacy Boolar-IR path, or an array-index expression for the
    /// array-batched Volar-IR path, see [`emit_verifier_and_gate`]'s own
    /// doc) into `state_var`; return the new state expression to rebind
    /// `state_var` to. `gate_idx` is this AND gate's 0-based index.
    #[allow(clippy::too_many_arguments)]
    fn and_gate_step(
        &self,
        gate_idx: usize,
        k_a: &str,
        k_b: &str,
        k_c: &str,
        delta: &str,
        hat: IrExpr<P>,
        r_and: IrExpr<P>,
        state_var: &str,
        prov: P,
    ) -> IrExpr<P>;

    /// Bare, externally-resolved type name for the per-gate fold challenge
    /// (e.g. `"IopChallenge"`).
    fn fold_scalar_type_name(&self) -> &str;

    /// Bare, externally-resolved trait name added to the woven function's
    /// own `T: …` bound (via `TraitKind::Custom` — the same "open-ended
    /// user-defined trait" mechanism already used elsewhere in this weaver,
    /// e.g. `fhe.rs`'s `WordReducer` bound). This is what lets
    /// [`and_gate_step`](Self::and_gate_step) hand the gate's *whole*
    /// `Q<N,T>`/`Delta<N,T>`/`Array<T,N>` values to an externally-resolved
    /// function without the weaver needing to know how to project a `T`
    /// value down to a scalar itself — `Some("IopLift")` for [`IopSink`]
    /// means the harness must provide `impl IopLift for {whatever T it
    /// instantiates}` (its own trait, sidestepping the orphan-rule
    /// restriction on implementing `std::convert::From`/`Into` for two
    /// foreign types). `None` if the sink's `and_gate_step` doesn't need any
    /// extra bound on `T`.
    fn fold_lift_trait_name(&self) -> Option<&str> {
        None
    }
}

/// The IOP-based prove-the-verifier fold sink, described in
/// `docs/prove-the-verifier-iop.md`. Threads a fixed-size accumulator
/// through the woven verifier's loop, updated once per AND gate via
/// `iop_fold_gate` (externally resolved, § trait doc) — the harness this
/// links against (`volar-verifier-iop-runtime`) folds **natively** in a
/// `GF(2^k)` tower field (`volar_iop::field`), no cross-field embedding.
/// Named `IopAccumulator`/`IopChallenge`/`iop_fold_gate`/
/// `iop_accumulator_fresh` — the harness must supply matching definitions.
///
/// **A lift trait is still needed, but a trivial, sound one.** `T` (the
/// VOLE's own field, e.g. `Galois` for `GF(2^8)`) is a different Rust type
/// from the fold's tower field (e.g. `Gf128`), even though the latter is
/// built as an *extension* of (an encoding of) the former — Rust still
/// needs an explicit embedding function. This embedding (`IopLift`) is a
/// **canonical, characteristic-preserving ring embedding** (`T` sits inside
/// the tower field's base level, no bit-decomposition, no cross-
/// characteristic cast), sound by construction (the literal definition of a
/// field extension containing its base field) — ordinary type plumbing,
/// not a cryptographic design decision needing separate review.
#[derive(Clone, Copy, Debug, Default)]
pub struct IopSink;

impl<P: Clone + Default> VerifierTraceSink<P> for IopSink {
    fn state_type_name(&self) -> &str {
        "IopAccumulator"
    }

    fn init_state_fn_name(&self) -> &str {
        "iop_accumulator_fresh"
    }

    fn fold_scalar_type_name(&self) -> &str {
        "IopChallenge"
    }

    fn fold_lift_trait_name(&self) -> Option<&str> {
        Some("IopLift")
    }

    fn and_gate_step(
        &self,
        _gate_idx: usize,
        k_a: &str,
        k_b: &str,
        k_c: &str,
        delta: &str,
        hat: IrExpr<P>,
        r_and: IrExpr<P>,
        state_var: &str,
        prov: P,
    ) -> IrExpr<P> {
        ir_expr_p(IrExprKind::Call {
            func: Box::new(ir_expr_p(IrExprKind::Path {
                segments: vec!["iop_fold_gate".into()],
                type_args: vec![],
            }, prov.clone())),
            args: vec![
                clone_expr(var(state_var)),
                clone_expr(var(k_a)),
                clone_expr(var(k_b)),
                clone_expr(var(k_c)),
                // delta is already &Delta<N,T> here, so pass it bare (not
                // .clone()'d, which would auto-deref-then-clone into an
                // owned Delta<N,T>).
                var(delta),
                clone_expr(hat),
                clone_expr(r_and),
            ],
        }, prov)
    }
}

/// Wraps an existing [`VoleWitnessSource`], overriding [`trace_sink`] to
/// `Some` — the mechanism [`weave_vole_verifier_with_trace`] uses so no
/// changes are needed to `ZkWitnessConfig`/`VoleSideConfig` themselves. Fixed
/// (not blanket) over `Prov`, matching the one concrete provenance type its
/// stored `sink` was built for.
///
/// [`trace_sink`]: VoleWitnessSource::trace_sink
struct TracingConfig<'a, C, Prov: Clone + Default> {
    inner: &'a C,
    sink: &'a dyn VerifierTraceSink<Prov>,
}

impl<'a, C: VoleWitnessSource<Prov>, Prov: Clone + Default> VoleWitnessSource<Prov> for TracingConfig<'a, C, Prov> {
    fn is_public_input(&self, idx: u32) -> bool {
        self.inner.is_public_input(idx)
    }
    fn is_public_action_output(&self, action_name: &str, bit: usize) -> bool {
        self.inner.is_public_action_output(action_name, bit)
    }
    fn trace_sink(&self) -> Option<&dyn VerifierTraceSink<Prov>> {
        Some(self.sink)
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
fn synth_prover_public_wire<P: Clone + Default>(bool_name: &str) -> IrExpr<P> {
    let vope_one = clone_expr(var("vope_one"));
    let vope_zero = ir_expr(IrExprKind::Binary {
        op: SpecBinOp::Add,
        left: Box::new(clone_expr(var("vope_one"))),
        right: Box::new(clone_expr(var("vope_one"))),
    });
    ir_expr(IrExprKind::If {
        cond: Box::new(var(bool_name)),
        then_branch: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(vope_one)),
        },
        else_branch: Some(Box::new(vope_zero)),
    })
}

/// Synthesise a verifier Q wire from a public `bool` variable `bool_name`.
///
/// Generates: `if {bool_name} { Q { q: delta.delta.clone() } } else { Q { q: Array::default() } }`
///
/// For a public bit `b=1` the verifier computes `K = M + 1·Δ`; with `M=0` this is `Δ`.
/// For `b=0`, `K = 0`.  This is consistent with the prover's synthesis above.
fn synth_verifier_public_wire<P: Clone + Default>(bool_name: &str) -> IrExpr<P> {
    let q_one = q_struct(ir_expr(IrExprKind::MethodCall {
        receiver: Box::new(ir_expr(IrExprKind::Field {
            base: Box::new(var("delta")),
            field: "delta".into(),
        })),
        method: MethodKind::Known(StdMethod::Clone),
        type_args: vec![],
        args: vec![],
    }));
    let q_zero = q_struct(array_t_default());
    ir_expr(IrExprKind::If {
        cond: Box::new(var(bool_name)),
        then_branch: IrBlock {
            stmts: vec![],
            expr: Some(Box::new(q_one)),
        },
        else_branch: Some(Box::new(q_zero)),
    })
}

// ============================================================================
// AND gate helper calls
// ============================================================================

/// Emit `let (_wire_k, _hat_k) = vole_and_prover_step::<N, T>(wire_a.clone(), wire_b.clone());`
/// The hat variable is left in scope for the caller to collect into a `FixedArray`.
fn emit_prover_and_gate<P: Clone + Default>(
    a_expr: IrExpr<P>,
    b_expr: IrExpr<P>,
    wire_name: &str,
    hat_name: &str,
    stmts: &mut Vec<IrStmt<P>>,
    prov: P,
) {
    // let (wire_k, hat_k) = vole_and_prover_step::<N, T>(wire_a.clone(), wire_b.clone());
    stmts.push(ir_stmt_p(IrStmtKind::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(hat_name),
        ]),
        ty: None,
        init: Some(ir_expr_p(IrExprKind::Call {
            func: Box::new(ir_expr_p(IrExprKind::Path {
                segments: vec!["vole_and_prover_step".into()],
                type_args: vec![
                    IrType::TypeParam("N".into()),
                    IrType::TypeParam("T".into()),
                ],
            }, prov.clone())),
            args: vec![
                clone_expr(a_expr),
                clone_expr(b_expr),
            ],
        }, prov.clone())),
    }, prov));
}

/// Same call as [`emit_prover_and_gate`], but returns the tuple-returning
/// call expression itself instead of emitting a `let`-bound statement --
/// lets a caller push the whole `(wire, hat)` pair directly into
/// `_and_pool` (see [`VoleIrCtx::push_and_pair`]) without ever binding
/// either half to a named `let` first. Args are NOT `clone_expr`-wrapped
/// internally (unlike `emit_prover_and_gate`) -- callers decide, since
/// some already have cloned expressions in hand (`operand_expr`'s
/// output) and some don't (`wire_ref_raw`'s raw output).
fn vole_and_prover_step_expr<P: Clone + Default>(
    a_arg: IrExpr<P>,
    b_arg: IrExpr<P>,
    prov: P,
) -> IrExpr<P> {
    ir_expr_p(IrExprKind::Call {
        func: Box::new(ir_expr_p(IrExprKind::Path {
            segments: vec!["vole_and_prover_step".into()],
            type_args: vec![
                IrType::TypeParam("N".into()),
                IrType::TypeParam("T".into()),
            ],
        }, prov.clone())),
        args: vec![a_arg, b_arg],
    }, prov)
}

/// Emit `let (_wire_k, _ok_k) = vole_and_verifier_check::<N, T>(delta, &wire_a, &wire_b, &q_and_expr, &hat_expr);`
/// followed by `all_ok = all_ok && _ok_k;`. `q_and_expr`/`hat_expr` are
/// already-built read expressions (a bare param `var("q_and_5")` for the
/// legacy Boolar-IR path, or `arr_index("q_and", "5")` for the array-batched
/// Volar-IR path) — this function only borrows them, never decides how
/// they're sourced.
fn emit_verifier_and_gate<P: Clone + Default>(
    a_expr: IrExpr<P>,
    b_expr: IrExpr<P>,
    wire_name: &str,
    ok_name: &str,
    q_and_expr: IrExpr<P>,
    hat_expr: IrExpr<P>,
    stmts: &mut Vec<IrStmt<P>>,
    prov: P,
) {
    // let (wire_k, ok_k) = vole_and_verifier_check::<N, T>(delta, &wire_a, &wire_b, &q_and, &hat);
    stmts.push(ir_stmt_p(IrStmtKind::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(ok_name),
        ]),
        ty: None,
        init: Some(ir_expr_p(IrExprKind::Call {
            func: Box::new(ir_expr_p(IrExprKind::Path {
                segments: vec!["vole_and_verifier_check".into()],
                type_args: vec![
                    IrType::TypeParam("N".into()),
                    IrType::TypeParam("T".into()),
                ],
            }, prov.clone())),
            args: vec![
                var("delta"),
                ref_expr(a_expr),
                ref_expr(b_expr),
                ref_expr(q_and_expr),
                ref_expr(hat_expr),
            ],
        }, prov.clone())),
    }, prov.clone()));

    // all_ok = all_ok && ok_k;
    stmts.push(ir_stmt_p(IrStmtKind::Semi(ir_expr_p(IrExprKind::Assign {
        left: Box::new(var("all_ok")),
        right: Box::new(ir_expr_p(IrExprKind::Binary {
            op: SpecBinOp::And,
            left: Box::new(var("all_ok")),
            right: Box::new(var(ok_name)),
        }, prov.clone())),
    }, prov.clone())), prov));
}

/// Build `derive_and_q::<N, T>(delta, &a, &b, &hat)` as a bare expression
/// (not a statement) — `QSim`'s AND-gate handling (Milestone 1.6): unlike
/// [`emit_verifier_and_gate`], this *derives* `q_and` from an externally-
/// supplied `hat` (same shape `Verifier` already takes) instead of taking
/// `q_and_k` itself as an external parameter and checking it. No
/// `ok`/`all_ok`/fold plumbing — `QSim` never folds, that's `Verifier`'s
/// job once handed these derived values.
///
/// Returns the call expression directly rather than emitting a `let`, so
/// both callers (`emit_and`'s QSim branch, `emit_poly_wide`'s degree-2
/// QSim handling) can push the result straight onto `_pool` (see
/// [`VoleIrCtx::push_wire_scratch`]) with no intermediate name at all:
/// QSim's AND-gate output *is* its own `q_and` (the "wire" value is
/// always just a clone of it), so unlike the Prover's genuinely paired
/// wire+hat there's nothing here that needs splitting across two names.
fn derive_and_q_expr<P: Clone + Default>(
    a_expr: IrExpr<P>,
    b_expr: IrExpr<P>,
    hat_expr: IrExpr<P>,
    prov: P,
) -> IrExpr<P> {
    ir_expr_p(IrExprKind::Call {
        func: Box::new(ir_expr_p(IrExprKind::Path {
            segments: vec!["derive_and_q".into()],
            type_args: vec![
                IrType::TypeParam("N".into()),
                IrType::TypeParam("T".into()),
            ],
        }, prov.clone())),
        args: vec![
            var("delta"),
            ref_expr(a_expr),
            ref_expr(b_expr),
            ref_expr(hat_expr),
        ],
    }, prov)
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
fn emit_prover_sbox_gate_k2<P: Clone + Default>(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    k2_name: &str,
    stmts: &mut Vec<IrStmt<P>>,
    prov: P,
) {
    stmts.push(ir_stmt_p(IrStmtKind::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(k2_name),
        ]),
        ty: None,
        init: Some(ir_expr_p(IrExprKind::Call {
            func: Box::new(ir_expr_p(IrExprKind::Path {
                segments: vec!["vole_sbox_prover_step".into()],
                type_args: vec![
                    IrType::TypeParam("N".into()),
                    IrType::TypeParam("T".into()),
                ],
            }, prov.clone())),
            args: vec![
                clone_expr(var(name_a)),
                clone_expr(var(name_b)),
            ],
        }, prov.clone())),
    }, prov));
}

/// Emit: `let (wire_k, ok_k) = vole_sbox_verifier_check::<N,T>(delta, &q_a, &q_b, sbox_vopes[idx].clone());`
/// followed by `all_ok = all_ok && ok_k;`.
fn emit_verifier_sbox_check_k2<P: Clone + Default>(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    ok_name: &str,
    sbox_idx: usize,
    stmts: &mut Vec<IrStmt<P>>,
    prov: P,
) {
    // let (wire_k, ok_k) = vole_sbox_verifier_check::<N,T>(delta, &q_a, &q_b, sbox_vopes[idx].clone());
    stmts.push(ir_stmt_p(IrStmtKind::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident(wire_name),
            IrPattern::ident(ok_name),
        ]),
        ty: None,
        init: Some(ir_expr_p(IrExprKind::Call {
            func: Box::new(ir_expr_p(IrExprKind::Path {
                segments: vec!["vole_sbox_verifier_check".into()],
                type_args: vec![
                    IrType::TypeParam("N".into()),
                    IrType::TypeParam("T".into()),
                ],
            }, prov.clone())),
            args: vec![
                var("delta"),
                ref_expr(var(name_a)),
                ref_expr(var(name_b)),
                clone_expr(ir_expr_p(IrExprKind::Index {
                    base: Box::new(var("sbox_vopes")),
                    index: Box::new(ir_expr_p(IrExprKind::Lit(IrLit::Int(sbox_idx as i128)), prov.clone())),
                }, prov.clone())),
            ],
        }, prov.clone())),
    }, prov.clone()));

    // all_ok = all_ok && ok_k;
    stmts.push(ir_stmt_p(IrStmtKind::Semi(ir_expr_p(IrExprKind::Assign {
        left: Box::new(var("all_ok")),
        right: Box::new(ir_expr_p(IrExprKind::Binary {
            op: SpecBinOp::And,
            left: Box::new(var("all_ok")),
            right: Box::new(var(ok_name)),
        }, prov.clone())),
    }, prov.clone())), prov));
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
) -> Tagged<Zk, IrModule<IrFunction>> {
    let mut tagged = weave_vole_prover_with_handler(circuit, name, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(tagged.inner_mut()); }
    tagged
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
) -> Tagged<Zk, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    Tagged::seal(weave_vole_prover_inner(circuit, name, &ZkWitnessConfig::default(), handler))
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
) -> Tagged<Zk, IrModule<IrFunction>> {
    let mut module = weave_vole_prover_inner(circuit, name, config, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    Tagged::seal(module)
}

/// Weave with both a [`ZkWitnessConfig`] and a provenance handler.
pub fn weave_vole_prover_with_config_and_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    handler: &H,
) -> Tagged<Zk, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    Tagged::seal(weave_vole_prover_inner(circuit, name, config, handler))
}

/// Weave a single-block boolean circuit into a VOLE **prover** `IrModule`,
/// resolving witness/statement per circuit input and action output bit via
/// `side_handler` instead of a position-keyed [`ZkWitnessConfig`].
///
/// `assignments` supplies the side for each introduction point (circuit
/// inputs and action output bits have no IR node of their own to read a side
/// from); `side_handler` then resolves each assigned (or absent) side to a
/// [`VoleProtection`]. [`TableProtection<VoleProtection>`](volar_side::TableProtection)
/// is a ready-made `side_handler` for the common case of a small explicit map.
pub fn weave_vole_prover_with_side<P: Clone, SH>(
    circuit: &BIrBlocks<P>,
    name: &str,
    assignments: &VoleSideAssignments,
    side_handler: &SH,
) -> Tagged<Zk, IrModule<IrFunction>>
where
    SH: volar_side::SideHandler<Protection = VoleProtection>,
{
    let config = VoleSideConfig { assignments, handler: side_handler };
    Tagged::seal(weave_vole_prover_inner(circuit, name, &config, &NoProvenance))
}

/// Like [`weave_vole_prover_with_side`] but also threads a [`ProvenanceHandler`].
pub fn weave_vole_prover_with_side_and_handler<P, SH, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    assignments: &VoleSideAssignments,
    side_handler: &SH,
    handler: &H,
) -> Tagged<Zk, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    SH: volar_side::SideHandler<Protection = VoleProtection>,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    let config = VoleSideConfig { assignments, handler: side_handler };
    Tagged::seal(weave_vole_prover_inner(circuit, name, &config, handler))
}

fn weave_vole_prover_inner<P, H, C: VoleWitnessSource>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &C,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    assert!(
        circuit.is_circuit(),
        "weave_vole_prover: circuit must satisfy is_circuit()"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let ctrl_prov: H::Output = block.stmts.first()
        .map(|n| handler.map(&n.prov))
        .expect("weave_vole_prover_inner: circuit has no statements; cannot derive provenance for infrastructure statements");

    // Pre-scan for external primitives (oracle calls, action calls, RNG sources).
    // Track (name, bit_count) for actions so we can look up per-action public configs.
    //
    // Oracles are keyed by NAME: current lowering emits one direct
    // `OracleBit { name, args, bit, occurrence }` statement per output bit
    // (no aggregate handle), so all bits of one oracle share the name key.
    // Legacy handle-form circuits (`OracleCall` + `OracleProjectedBit`) keep
    // the call-var key for compatibility.
    let mut oracle_handle_map = BTreeMap::<u32, usize>::new();
    let mut oracle_name_map = BTreeMap::<alloc::string::String, usize>::new();
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
            BIrStmt::OracleBit { name, bit, .. } => {
                let k = match oracle_name_map.get(name) {
                    Some(&k) => k,
                    None => {
                        let k = oracle_bit_counts.len();
                        oracle_name_map.insert(name.clone(), k);
                        oracle_bit_counts.push(0);
                        k
                    }
                };
                oracle_bit_counts[k] = oracle_bit_counts[k].max(bit + 1);
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
        let is_pub = config.is_public_input(i as u32);
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
    // Action output bit commitments — public or private per the witness source.
    for (k, (action_name, num_bits)) in action_infos.iter().enumerate() {
        for j in 0..*num_bits {
            let is_pub = config.is_public_action_output(action_name, j);
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
    let mut and_counter: usize = 0;
    let mut sbox_counter: usize = 0;
    let mut hat_names: Vec<String> = Vec::new();
    let mut sbox_k2_names: Vec<String> = Vec::new();

    // Synthesise Vope wires for public inputs from the bool params.
    for i in 0..num_params {
        if config.is_public_input(i as u32) {
            stmts.push(ir_stmt_p(IrStmtKind::Let {
                pattern: IrPattern::ident(&format!("vope_input_{}", i)),
                ty: None,
                init: Some(synth_prover_public_wire(&format!("input_{}", i))),
            }, ctrl_prov.clone()));
        }
    }

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        match stmt {
            BIrStmt::Zero => {
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::StructExpr {
                        kind: StructKind::Custom("Vope".into()),
                        type_args: vec![],
                        fields: vec![
                            ("u".into(), array_default()),
                            ("v".into(), array_t_default()),
                        ],
                        rest: None,
                    })),
                }, q.clone()));
            }

            BIrStmt::One => {
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var("vope_one"))),
                }, q.clone()));
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&name_a))),
                        right: Box::new(clone_expr(var(&name_b))),
                    })),
                }, q.clone()));
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(var(&name_a))),
                        right: Box::new(clone_expr(var("vope_one"))),
                    })),
                }, q.clone()));
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                if handler.gate_degree(prov) == 2 {
                    // K=2 S-box gate: hat-free, produces (Vope<K=1>, Vope<K=2>).
                    let k2_name = format!("sbox_k2_{}", sbox_counter);
                    sbox_counter += 1;
                    sbox_k2_names.push(k2_name.clone());
                    emit_prover_sbox_gate_k2(&name_a, &name_b, &let_name, &k2_name, &mut stmts, q.clone());
                } else {
                    // K=1 standard AND gate: produces K=1 Vope + hat.
                    let hat_name = format!("hat_{}", and_counter);
                    and_counter += 1;
                    hat_names.push(hat_name.clone());
                    emit_prover_and_gate(var(&name_a), var(&name_b), &let_name, &hat_name, &mut stmts, q.clone());
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),

            BIrStmt::OracleCall { .. } => {
                let k = oracle_handle_map[&result_id.0];
                var_names.insert(result_id.0, format!("oracle_handle_{}", k));
                continue;
            }

            BIrStmt::OracleBit { name, bit, .. } => {
                let k = oracle_name_map[name];
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("vope_oracle_{}_bit_{}", k, bit)))),
                }, q.clone()));
            }

            BIrStmt::OracleProjectedBit { call, bit } => {
                let k = oracle_handle_map[&call.0];
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("vope_oracle_{}_bit_{}", k, bit)))),
                }, q.clone()));
            }

            BIrStmt::ActionCall { .. } => {
                let k = action_handle_map[&result_id.0];
                var_names.insert(result_id.0, format!("action_handle_{}", k));
                continue;
            }

            BIrStmt::ActionBit { call, bit } => {
                let k = action_handle_map[&call.0];
                let (action_name, _) = &action_infos[k];
                let is_pub = config.is_public_action_output(action_name, *bit);
                let init = if is_pub {
                    synth_prover_public_wire(&format!("action_{}_bit_{}", k, bit))
                } else {
                    clone_expr(var(&format!("vope_action_{}_bit_{}", k, bit)))
                };
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(init),
                }, q.clone()));
            }

            BIrStmt::Rng { .. } => {
                let r = rng_var_map[&result_id.0];
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("vope_rng_{}", r)))),
                }, q.clone()));
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
    // Legacy `BIrBlocks` weaver -- entirely independent of `VoleIrCtx`'s
    // own `_hat_pool` scratch pooling, always real names.
    let (output_expr, _) = build_return(block, &var_names, vope_type());
    let hats_expr = ir_expr(IrExprKind::FixedArray(hat_names.iter().map(|h| var(h)).collect()));
    let ret_expr = if sbox_k2_names.is_empty() {
        ir_expr(IrExprKind::Tuple(vec![output_expr, hats_expr]))
    } else {
        let sbox_expr = ir_expr(IrExprKind::FixedArray(sbox_k2_names.iter().map(|n| var(n)).collect()));
        ir_expr(IrExprKind::Tuple(vec![output_expr, hats_expr, sbox_expr]))
    };

    let func = IrFunction { no_inline: true,
        name: format!("vole_prove_{}", name),
        module_path: vec![],
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
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut tagged = weave_vole_verifier_with_handler(circuit, name, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(tagged.inner_mut()); }
    tagged
}

/// Weave a single-block boolean circuit into a VOLE **verifier** `IrModule`,
/// using `handler` to map input provenance into the output IR.
///
/// All inputs are private witnesses.  Use [`weave_vole_verifier_with_config_and_handler`]
/// for public/private control.
// The verifier-as-a-computation is `Transparent` (non-ZK): the inner VOLE
// proof already accounts for zero-knowledge, so re-checking it carries no
// ZK proving secrets of its own.
pub fn weave_vole_verifier_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    Tagged::seal(weave_vole_verifier_inner(circuit, name, &ZkWitnessConfig::default(), handler))
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
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut module = weave_vole_verifier_inner(circuit, name, config, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    Tagged::seal(module)
}

/// Like [`weave_vole_verifier_with_config`], but also threads a
/// [`VerifierTraceSink`] — the woven verifier gains a fold-accumulator state,
/// updated per AND gate, returned alongside `(output_wire, all_ok)`. See
/// [`VerifierTraceSink`]'s doc for what "trace sink" means here and the
/// GF(2^k) → F_ℓ seam it deliberately leaves pluggable.
pub fn weave_vole_verifier_with_trace<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    sink: &dyn VerifierTraceSink<()>,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Transparent, IrModule<IrFunction>> {
    // NoProvenance::Output is always () — the sink is fixed over that.
    let cfg: TracingConfig<'_, ZkWitnessConfig, ()> = TracingConfig { inner: config, sink };
    let mut module = weave_vole_verifier_inner(circuit, name, &cfg, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    Tagged::seal(module)
}

/// Weave with both a [`ZkWitnessConfig`] and a provenance handler.
pub fn weave_vole_verifier_with_config_and_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    Tagged::seal(weave_vole_verifier_inner(circuit, name, config, handler))
}

/// Like [`weave_vole_prover_with_side`] but for the **verifier**.
pub fn weave_vole_verifier_with_side<P: Clone, SH>(
    circuit: &BIrBlocks<P>,
    name: &str,
    assignments: &VoleSideAssignments,
    side_handler: &SH,
) -> Tagged<Transparent, IrModule<IrFunction>>
where
    SH: volar_side::SideHandler<Protection = VoleProtection>,
{
    let config = VoleSideConfig { assignments, handler: side_handler };
    Tagged::seal(weave_vole_verifier_inner(circuit, name, &config, &NoProvenance))
}

/// Like [`weave_vole_verifier_with_side`] but also threads a [`ProvenanceHandler`].
pub fn weave_vole_verifier_with_side_and_handler<P, SH, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    assignments: &VoleSideAssignments,
    side_handler: &SH,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    SH: volar_side::SideHandler<Protection = VoleProtection>,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    let config = VoleSideConfig { assignments, handler: side_handler };
    Tagged::seal(weave_vole_verifier_inner(circuit, name, &config, handler))
}

fn weave_vole_verifier_inner<P, H, C: VoleWitnessSource<H::Output>>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &C,
    handler: &H,
) -> IrModule<IrFunction<H::Output>, H::Output>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    assert!(
        circuit.is_circuit(),
        "weave_vole_verifier: circuit must satisfy is_circuit()"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let ctrl_prov: H::Output = block.stmts.first()
        .map(|n| handler.map(&n.prov))
        .expect("weave_vole_verifier_inner: circuit has no statements; cannot derive provenance for infrastructure statements");

    let (and_count, sbox_count) = expanded.iter().fold((0usize, 0usize), |(k1, k2), (_, s, prov)| {
        if matches!(s, BIrStmt::And(..)) {
            if handler.gate_degree(prov) == 2 { (k1, k2 + 1) } else { (k1 + 1, k2) }
        } else {
            (k1, k2)
        }
    });

    // Pre-scan: track (name, bit_count) for actions.
    //
    // Oracles are keyed by NAME: current lowering emits one direct
    // `OracleBit { name, args, bit, occurrence }` statement per output bit
    // (no aggregate handle), so all bits of one oracle share the name key.
    // Legacy handle-form circuits (`OracleCall` + `OracleProjectedBit`) keep
    // the call-var key for compatibility.
    let mut oracle_handle_map = BTreeMap::<u32, usize>::new();
    let mut oracle_name_map = BTreeMap::<alloc::string::String, usize>::new();
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
            BIrStmt::OracleBit { name, bit, .. } => {
                let k = match oracle_name_map.get(name) {
                    Some(&k) => k,
                    None => {
                        let k = oracle_bit_counts.len();
                        oracle_name_map.insert(name.clone(), k);
                        oracle_bit_counts.push(0);
                        k
                    }
                };
                oracle_bit_counts[k] = oracle_bit_counts[k].max(bit + 1);
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
        // Per-gate fold challenge (trace-sink only) — threaded exactly like
        // q_and_k/hat_k above, one per K=1 AND gate.
        if let Some(sink) = config.trace_sink() {
            params.push(IrParam {
                name: format!("r_and_{}", k),
                ty: IrType::TypeParam(sink.fold_scalar_type_name().into()),
            });
        }
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
        let is_pub = config.is_public_input(i as u32);
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
    // Action output Q shares — public or private per the witness source.
    for (k, (action_name, num_bits)) in action_infos.iter().enumerate() {
        for j in 0..*num_bits {
            let is_pub = config.is_public_action_output(action_name, j);
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

    // Return type: (Q<N, T>, bool) — or (Q<N, T>, bool, {state_type_name})
    // when a trace sink is configured.
    let mut ret_elems = vec![
        q_type(),
        IrType::Primitive(PrimitiveType::Bool),
    ];
    if let Some(sink) = config.trace_sink() {
        ret_elems.push(IrType::TypeParam(sink.state_type_name().into()));
    }
    let ret_type = IrType::Tuple(ret_elems);

    let (generics, mut where_clause) = verifier_generics_and_where();
    // Extend T's bound with the sink's fold-lift trait (bare, externally
    // resolved — see VerifierTraceSink::fold_lift_trait_name), so
    // and_gate_step can hand whole Q<N,T>/Delta<N,T>/Array<T,N> values to an
    // externally-resolved function without this weaver needing to know how
    // to project T down to a scalar itself.
    if let Some(sink) = config.trace_sink() {
        if let Some(trait_name) = sink.fold_lift_trait_name() {
            if let Some(IrWherePredicate::TypeBound { ty, bounds }) = where_clause
                .iter_mut()
                .find(|p| matches!(p, IrWherePredicate::TypeBound { ty: IrType::TypeParam(n), .. } if n == "T"))
            {
                let _ = ty;
                bounds.push(IrTraitBound {
                    trait_kind: TraitKind::Custom(trait_name.into()),
                    type_args: vec![],
                    assoc_bindings: vec![],
                });
            }
        }
    }

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut and_counter: usize = 0;
    let mut sbox_counter: usize = 0;

    stmts.push(ir_stmt_p(IrStmtKind::Let {
        pattern: IrPattern::Ident {
            mutable: true,
            name: "all_ok".into(),
            subpat: None,
        },
        ty: None,
        init: Some(ir_expr_p(IrExprKind::Lit(IrLit::Bool(true)), ctrl_prov.clone())),
    }, ctrl_prov.clone()));

    // Threaded fold-accumulator state (trace sink only) — parallel to
    // all_ok above: bound once at entry, reassigned per AND gate, returned.
    if let Some(sink) = config.trace_sink() {
        stmts.push(ir_stmt_p(IrStmtKind::Let {
            pattern: IrPattern::Ident {
                mutable: true,
                name: "fold_state".into(),
                subpat: None,
            },
            ty: None,
            init: Some(ir_expr_p(IrExprKind::Call {
                func: Box::new(ir_expr_p(IrExprKind::Path {
                    segments: vec![sink.init_state_fn_name().into()],
                    type_args: vec![],
                }, ctrl_prov.clone())),
                args: vec![],
            }, ctrl_prov.clone())),
        }, ctrl_prov.clone()));
    }

    // Synthesise Q wires for public inputs from the bool params.
    for i in 0..num_params {
        if config.is_public_input(i as u32) {
            stmts.push(ir_stmt_p(IrStmtKind::Let {
                pattern: IrPattern::ident(&format!("q_input_{}", i)),
                ty: None,
                init: Some(synth_verifier_public_wire(&format!("input_{}", i))),
            }, ctrl_prov.clone()));
        }
    }

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        match stmt {
            BIrStmt::Zero => {
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(array_t_default())),
                }, q.clone()));
            }

            BIrStmt::One => {
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(ir_expr_p(IrExprKind::MethodCall {
                        receiver: Box::new(ir_expr_p(IrExprKind::Field {
                            base: Box::new(var("delta")),
                            field: "delta".into(),
                        }, q.clone())),
                        method: MethodKind::Known(StdMethod::Clone),
                        type_args: vec![],
                        args: vec![],
                    }, q.clone()))),
                }, q.clone()));
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(array_t_from_fn(
                        "i",
                        ir_expr_p(IrExprKind::Binary {
                            op: SpecBinOp::Add,
                            left: Box::new(clone_expr(q_index(&name_a, "i"))),
                            right: Box::new(clone_expr(q_index(&name_b, "i"))),
                        }, q.clone()),
                    ))),
                }, q.clone()));
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(q_struct(array_t_from_fn(
                        "i",
                        ir_expr_p(IrExprKind::Binary {
                            op: SpecBinOp::Add,
                            left: Box::new(clone_expr(q_index(&name_a, "i"))),
                            right: Box::new(clone_expr(delta_index("i"))),
                        }, q.clone()),
                    ))),
                }, q.clone()));
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let ok_name = format!("ok_and_{}", and_counter + sbox_counter);
                if handler.gate_degree(prov) == 2 {
                    // K=2 S-box gate: verify via sbox_vopes[sbox_idx] * delta == q_a * q_b.
                    emit_verifier_sbox_check_k2(
                        &name_a, &name_b, &let_name, &ok_name,
                        sbox_counter, &mut stmts, q.clone(),
                    );
                    sbox_counter += 1;
                } else {
                    let gate_idx = and_counter;
                    let q_and_name = format!("q_and_{}", gate_idx);
                    let hat_name = format!("hat_{}", gate_idx);
                    and_counter += 1;
                    emit_verifier_and_gate(
                        var(&name_a), var(&name_b), &let_name, &ok_name,
                        var(&q_and_name), var(&hat_name),
                        &mut stmts, q.clone(),
                    );
                    if let Some(sink) = config.trace_sink() {
                        let r_param_name = format!("r_and_{}", gate_idx);
                        let new_state = sink.and_gate_step(
                            gate_idx, &name_a, &name_b, &let_name, "delta",
                            var(&hat_name), var(&r_param_name), "fold_state", q.clone(),
                        );
                        stmts.push(ir_stmt_p(IrStmtKind::Semi(ir_expr_p(IrExprKind::Assign {
                            left: Box::new(var("fold_state")),
                            right: Box::new(new_state),
                        }, q.clone())), q.clone()));
                    }
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),

            BIrStmt::OracleCall { .. } => {
                let k = oracle_handle_map[&result_id.0];
                var_names.insert(result_id.0, format!("oracle_handle_{}", k));
                continue;
            }

            BIrStmt::OracleBit { name, bit, .. } => {
                let k = oracle_name_map[name];
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("q_oracle_{}_bit_{}", k, bit)))),
                }, q.clone()));
            }

            BIrStmt::OracleProjectedBit { call, bit } => {
                let k = oracle_handle_map[&call.0];
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("q_oracle_{}_bit_{}", k, bit)))),
                }, q.clone()));
            }

            BIrStmt::ActionCall { .. } => {
                let k = action_handle_map[&result_id.0];
                var_names.insert(result_id.0, format!("action_handle_{}", k));
                continue;
            }

            BIrStmt::ActionBit { call, bit } => {
                let k = action_handle_map[&call.0];
                let (action_name, _) = &action_infos[k];
                let is_pub = config.is_public_action_output(action_name, *bit);
                let init = if is_pub {
                    synth_verifier_public_wire(&format!("action_{}_bit_{}", k, bit))
                } else {
                    clone_expr(var(&format!("q_action_{}_bit_{}", k, bit)))
                };
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(init),
                }, q.clone()));
            }

            BIrStmt::Rng { .. } => {
                let r = rng_var_map[&result_id.0];
                stmts.push(ir_stmt_p(IrStmtKind::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(clone_expr(var(&format!("q_rng_{}", r)))),
                }, q.clone()));
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

    // Return (output_wire, all_ok) — or (output_wire, all_ok, fold_state)
    // when a trace sink is configured.
    let (output_expr, _) = build_return(block, &var_names, q_type());
    let mut ret_tuple = vec![output_expr, var("all_ok")];
    if config.trace_sink().is_some() {
        ret_tuple.push(var("fold_state"));
    }
    let ret_expr = ir_expr(IrExprKind::Tuple(ret_tuple));

    let func = IrFunction { no_inline: true,
        name: format!("vole_verify_{}", name),
        module_path: vec![],
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
) -> Tagged<Zk, IrModule<IrFunction>> {
    let mut tagged = weave_vole_prover_bounded_with_handler(circuit, name, limit, mode, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(tagged.inner_mut()); }
    tagged
}

/// Bounded VOLE prover weave with provenance handler.
pub fn weave_vole_prover_bounded_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> Tagged<Zk, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    let lowered = lower_to_circuit(circuit, limit, mode);
    Tagged::seal(weave_vole_prover_inner(&lowered, name, &ZkWitnessConfig::default(), handler))
}

/// Bounded VOLE prover weave with witness config.
pub fn weave_vole_prover_bounded_with_config<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Zk, IrModule<IrFunction>> {
    let lowered = lower_to_circuit(circuit, limit, mode);
    let mut module = weave_vole_prover_inner(&lowered, name, config, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    Tagged::seal(module)
}

/// Bounded VOLE prover weave with witness config and provenance handler.
pub fn weave_vole_prover_bounded_with_config_and_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> Tagged<Zk, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    let lowered = lower_to_circuit(circuit, limit, mode);
    Tagged::seal(weave_vole_prover_inner(&lowered, name, config, handler))
}

/// Backwards-compatible bounded VOLE verifier weave.
pub fn weave_vole_verifier_bounded<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mut tagged = weave_vole_verifier_bounded_with_handler(circuit, name, limit, mode, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(tagged.inner_mut()); }
    tagged
}

/// Bounded VOLE verifier weave with provenance handler.
pub fn weave_vole_verifier_bounded_with_handler<P, H>(
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
    let lowered = lower_to_circuit(circuit, limit, mode);
    Tagged::seal(weave_vole_verifier_inner(&lowered, name, &ZkWitnessConfig::default(), handler))
}

/// Bounded VOLE verifier weave with witness config.
pub fn weave_vole_verifier_bounded_with_config<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    limit: u32,
    mode: LoweringMode,
    linkage: Option<&LinkageSystem>,
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let lowered = lower_to_circuit(circuit, limit, mode);
    let mut module = weave_vole_verifier_inner(&lowered, name, config, &NoProvenance);
    if let Some(ls) = linkage { ls.apply(&mut module); }
    Tagged::seal(module)
}

/// Bounded VOLE verifier weave with witness config and provenance handler.
pub fn weave_vole_verifier_bounded_with_config_and_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    config: &ZkWitnessConfig,
    limit: u32,
    mode: LoweringMode,
    handler: &H,
) -> Tagged<Transparent, IrModule<IrFunction<H::Output>, H::Output>>
where
    P: Clone,
    H: ProvenanceHandler<P>,
    H::Output: Default,
{
    let lowered = lower_to_circuit(circuit, limit, mode);
    Tagged::seal(weave_vole_verifier_inner(&lowered, name, config, handler))
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
    /// Vector of authenticated bits (produced by `Merge`), each an
    /// independently-named, already-bound local -- the "real local per
    /// bit" contract every generic consumer (`var(name)`) relies on.
    Vec(Vec<String>),
    /// A wide value backed directly by an existing `[T; width]`-typed
    /// array (a top-level circuit param, e.g. `w_i`, or a wide chunk/
    /// finish incoming param like `in_next_state_k`) -- lane `j` is
    /// `{name}[j]`, indexed lazily rather than eagerly unpacked into
    /// `width` bound locals up front.
    ///
    /// Exists specifically because `insert_w_wires` previously unpacked
    /// *every* top-level param into `width` `let`-bound locals
    /// unconditionally, for *every* one of the ~241 split-weave
    /// functions per role -- regardless of whether that function ever
    /// referenced them. With top-level parameter threading (every state
    /// slot is now a combined-circuit param), that is
    /// O(functions * total_param_width) generated statements, which is
    /// what drove `print_weaved_vole_module` to 13GB+ RSS printing the
    /// real interpreter's full split-weave module.
    ///
    /// Any consumer that only needs an `IrExpr` (not a bound name) can
    /// index directly (`arr_index(name, j)`) with no extra statement at
    /// all -- see `emit_poly_wide`'s bundling step and `slot_expr`. Any
    /// consumer that genuinely needs real bound-local names (address
    /// composition, `Merge` combining, the unrolled per-lane `Poly`
    /// fallback) must call `Self::materialize` first, which lazily binds
    /// exactly the lanes actually touched and memoizes the result back
    /// into `self.wires` as a normal `Vec` -- paying the "real locals"
    /// cost only for vars a given function actually uses, not eagerly
    /// for the whole top-level param set.
    Array(String, usize),
    /// A SCALAR (width-1) value living in a shared, `&mut`-threaded pool
    /// array rather than as its own named parameter/local at all --
    /// Phase C of the pool-based regalloc plan (cross-function/cross-
    /// piece values, e.g. `piece_in_{v}`). `(pool_param_name, slot)`:
    /// `pool_param_name` is the generated function's own parameter name
    /// for the pool (e.g. `"_piece_pool"`), `slot` is a compact index
    /// into it (NOT the raw circuit var id -- callers build a small
    /// `var_id -> slot` map covering only the vars actually threaded
    /// through a given pool, to avoid sizing the array to the whole
    /// circuit's var-id space). Reading a `Pooled` slot must go through
    /// [`pooled_read_expr`] (asserts the slot has actually been written
    /// first -- unconditionally, not just in debug builds, since this
    /// pipeline's own real validation compiles generated code via
    /// `cargo test --release` throughout; see its own doc); nothing
    /// should ever build `arr_index(pool_param_name, ..)` by hand.
    Pooled(&'static str, usize),
}

/// Width of a circuit type in bits (1 for Bit, K for Vec(K, Bit)).
///
/// `Primitive(_8/_16/_32/.../_256)` (packed-integer markers -- e.g. the byte
/// cells `volar-vaffle-target` uses for byte-addressed memory, or i32/i64
/// arithmetic results) are, at this bit-circuit level, still exactly that
/// many independent GF(2) wires -- VAFFLE bit-decomposes every integer
/// value via `BitCircuitBuilder` regardless of which of the two "N related
/// bits" type tags (`Vec(K, Bit)` vs `Primitive(_K)`) ends up on a given
/// `IRStmt`. Same treatment as `Vec(k, _)`, just a different width source.
fn cir_type_width(ty: &CirTyId, types: &CirTypes) -> usize {
    match &types.0[ty.0 as usize] {
        CircuitIrType::Primitive(PrimType::Bit) => 1,
        CircuitIrType::Primitive(PrimType::_8) => 8,
        CircuitIrType::Primitive(PrimType::_16) => 16,
        CircuitIrType::Primitive(PrimType::_32) => 32,
        CircuitIrType::Primitive(PrimType::_64) => 64,
        CircuitIrType::Primitive(PrimType::_128) => 128,
        CircuitIrType::Primitive(PrimType::_256) => 256,
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
        let result_ty: CirTyId = match &stmt.kind {
            Stmt::Const(_, ty) => ty.clone(),
            Stmt::Poly { ty, coeffs, .. } => {
                // Width-aware: `emit_poly` broadcasts each degree-≥2
                // monomial's AND-chain across every bit lane of `ty`
                // (`VoleIrCtx::emit_poly_lane`), so a `_32`-typed AND needs
                // 32 `(mono.len() - 1)`-gate chains, not 1.
                let width = cir_type_width(ty, types);
                for (mono, coeff) in coeffs {
                    if *coeff % 2 == 1 && mono.len() >= 2 {
                        count += (mono.len() - 1) * width;
                    }
                }
                ty.clone()
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

/// AND count for commitment mode: only Poly stmts contribute (storage costs
/// 0 ANDs under `StorageMode::Commitment`).
///
/// Width-aware: `emit_poly` broadcasts each degree-≥2 monomial's AND-chain
/// across every bit lane of the statement's declared type
/// (`VoleIrCtx::emit_poly_lane`), so a `_32`-typed AND needs 32
/// `(mono.len() - 1)`-gate chains, not 1.
fn count_ir_ands_no_storage(block: &CirBlock, types: &CirTypes) -> usize {
    count_ir_ands_no_storage_range(&block.stmts, types)
}

/// As [`count_ir_ands_no_storage`], but over an arbitrary stmt slice --
/// Milestone 1.5 Step B: sizing one split (per-`MovfuscBlockBoundary`)
/// function's own `q_and`/`hat`/`r_and` params, bounded by that block's own
/// AND-gate count instead of the whole circuit's.
fn count_ir_ands_no_storage_range(stmts: &[volar_ir_common::Node<IRStmt, ()>], types: &CirTypes) -> usize {
    let mut count = 0;
    for stmt in stmts {
        if let Stmt::Poly { ty, coeffs, .. } = &stmt.kind {
            let width = cir_type_width(ty, types);
            for (mono, coeff) in coeffs {
                if *coeff % 2 == 1 && mono.len() >= 2 {
                    count += (mono.len() - 1) * width;
                }
            }
        }
    }
    count
}

/// Count storage reads in the circuit (for oracle parameter sizing).
/// Total committed-oracle bits needed for every `StorageRead` in `block`:
/// one `oracle_rd_{k}` parameter **per bit lane**, not per statement — a
/// `_32`-typed read needs 32, matching [`VoleIrCtx::emit_storage_read_committed`]'s
/// own per-lane oracle-param allocation.
fn count_storage_reads(block: &CirBlock, types: &CirTypes) -> usize {
    count_storage_reads_range(&block.stmts, types)
}

/// As [`count_storage_reads`], but over an arbitrary stmt slice (Milestone
/// 1.5 Step B per-block sizing, same rationale as
/// [`count_ir_ands_no_storage_range`]).
fn count_storage_reads_range(stmts: &[volar_ir_common::Node<IRStmt, ()>], types: &CirTypes) -> usize {
    stmts.iter().filter_map(|s| match &s.kind {
        Stmt::StorageRead { ty, .. } => Some(cir_type_width(ty, types)),
        _ => None,
    }).sum()
}

/// Scan a statement range for operand references into the combined
/// circuit's own top-level param range (`< num_params`) -- used to
/// determine which `w_i` a specific split-weave function actually needs
/// to declare, instead of unconditionally declaring all of them (a real,
/// measured blowup once top-level parameter threading made `num_params`
/// cover every state slot -- see `WireRepr::Array`'s own doc).
///
/// Callers must additionally union in this function's own boundary-
/// derived *output* var ids (`is_active`/`done`/`next_pc`/`next_state`/
/// `ret_vals`) restricted to `< num_params` -- a block/chunk can return a
/// top-level param *directly* (movfuscate.rs's tunnelled-slot
/// elimination pass-through) without that id ever appearing as an
/// operand inside this range. Deliberately conservative in the other
/// direction too: this only ever *adds* real operand references, so a
/// missed id just means an unused `w_i` stays declared (harmless, same
/// as the pre-filtering behavior for every param) -- it can never
/// produce an *under*-inclusive set that causes a "no entry for key"
/// panic, since nothing here removes an id once found.
fn collect_used_top_level_params(
    stmts: &[volar_ir_common::Node<IRStmt, ()>],
    num_params: usize,
) -> alloc::collections::BTreeSet<u32> {
    let mut used = alloc::collections::BTreeSet::new();
    for node in stmts {
        let _ = node.kind.clone().map_var(
            &mut used,
            &mut |used: &mut alloc::collections::BTreeSet<u32>, v: CirVar| -> Result<CirVar, core::convert::Infallible> {
                if (v.0 as usize) < num_params {
                    used.insert(v.0);
                }
                Ok(v)
            },
            &mut |_used, ty| Ok(ty),
            &mut |_used, stor| Ok(stor),
        );
    }
    used
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
    count_external_primitives_range(&block.stmts, types)
}

/// As [`count_external_primitives`], but over an arbitrary stmt slice
/// (Milestone 1.5 Step B per-block sizing, same rationale as
/// [`count_ir_ands_no_storage_range`]).
fn count_external_primitives_range(stmts: &[volar_ir_common::Node<IRStmt, ()>], types: &CirTypes) -> ExternalBitCounts {
    let mut oracle_calls = Vec::new();
    let mut action_calls = Vec::new();
    let mut rng_widths = Vec::new();

    for stmt in stmts {
        match &stmt.kind {
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

/// Which role a [`VoleIrCtx`] is generating code for. `Prover` and
/// `Verifier` are today's original two roles (byte-identical codegen to
/// before this enum existed); `QSim` is a third, weaver-generated role
/// (Milestone 1.6) that derives `q_and` values for chained AND gates via
/// `derive_and_q` (taking `hat_k` as an input, same shape `Verifier`
/// already does) instead of taking `q_and_k` as an external parameter and
/// checking it. Everywhere except [`VoleIrCtx::emit_and`], `QSim` shares
/// `Verifier`'s exact codegen (see [`VoleRole::is_prover`]) -- the shared
/// per-statement dispatch (`Poly`/`Merge`/`Shuffle`/`Storage*`) is what
/// keeps "the actual semantics of more complex operations" in one place
/// across all three roles.
#[derive(Clone, Copy, PartialEq, Eq)]
enum VoleRole {
    Prover,
    Verifier,
    QSim,
}

impl VoleRole {
    /// `true` only for `Prover` -- `QSim` intentionally takes the same
    /// (`false`) branch as `Verifier` at every existing `is_prover` call
    /// site except `emit_and`, which matches on `VoleRole` directly.
    fn is_prover(&self) -> bool {
        matches!(self, VoleRole::Prover)
    }
}

/// Context for emitting VOLE-authenticated wire computations.
struct VoleIrCtx<'a> {
    stmts: Vec<IrStmt>,
    wires: alloc::collections::BTreeMap<u32, WireRepr>,
    and_counter: usize,
    hat_names: Vec<String>,
    ok_names: Vec<String>,
    /// `QSim`-only: derived `q_and` values, collected in gate order, to be
    /// returned as this function's own output (the `Verifier`-side
    /// counterpart of `hat_names`, but an output instead of an input).
    q_and_names: Vec<String>,
    /// Current var name for each storage cell (Tree mode only).
    stor: alloc::collections::BTreeMap<(u32, u32, usize), String>,
    /// Counter for oracle-read parameters (Commitment mode).
    oracle_counter: usize,
    /// Memory trace (Commitment mode).
    trace: MemoryTrace,
    /// Running timestamp for memory operations.
    mem_timestamp: u32,
    role: VoleRole,
    /// Weave-time trace-assembly plugin (verifier side only; see
    /// [`VerifierTraceSink`]) -- `None` is byte-identical to today's output.
    /// Threaded through [`VoleIrCtx::emit_and`], the single place every
    /// Quicksilver AND-gate check (including each degree-≥2 monomial inside
    /// [`VoleIrCtx::emit_poly`]) is emitted for both prover and verifier.
    trace_sink: Option<&'a dyn VerifierTraceSink<()>>,
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
    // ---- Local scratch pools (Phase A of the pool-based regalloc plan,
    //      see the plan file's "Pool-based value representation" section)
    //
    // Purely function-local intermediate values (Poly XOR-chain
    // accumulators, single-value AND-gate outputs for QSim, ...) that
    // used to get a fresh, uniquely-numbered `let` binding each
    // (`and_w_3`, `_ka_7`, ...) -- by far the largest volume of distinct
    // identifiers rustc's own name-resolution pass has to track per
    // function. Instead, each such value is pushed onto a shared `Vec`
    // ("_pool" for wire-typed scratch -- `Vope<N,T,U1>` for Prover,
    // `Q<N,T>` for Verifier/QSim) and referenced by its pool index
    // instead of a fresh name.
    //
    // `_pool` (and, Prover-only, `_and_pool`) are declared exactly ONCE,
    // unconditionally, at construction time (see `declare_pools`) -- they
    // are genuinely function-scoped, not lazily-declared-on-first-use.
    // The lazy-declare version of this design (an earlier iteration)
    // turned out to interact badly with `emit_poly_wide`'s own closure
    // body (see below): resetting to a *fresh* pool at every closure
    // boundary meant a function with many wide (`>1`-bit) `Poly`
    // statements got one `_pool` declaration per such statement, not one
    // per function -- a real, measured blowup (tens of thousands of
    // duplicate declarations at real interpreter scale). `pool_next`/
    // `and_pool_next` (the *index* counters) still DO reset at the
    // closure boundary, and are restored advanced by however many slots
    // the closure contributed overall (`width` invocations, once each) --
    // see `in_closure` below and `emit_poly_wide`'s own boundary code.
    pool_next: usize,
    // Prover-only: a SECOND pool, `_and_pool: Vec<(Vope<N,T,U1>, Array<T,N>)>`,
    // holding an AND gate's full `(wire, hat)` output PAIR in one slot.
    // Exists so `vole_and_prover_step(..)`'s call result can be pushed
    // directly (`_and_pool.push(vole_and_prover_step(a, b));`) without
    // EVER binding either half to a named `let` first -- `_pool` alone
    // can't do this, since splitting a tuple call's two
    // outputs into two SEPARATE pools still requires a `let (a, b) = ..;`
    // to destructure it first (itself two more identifiers for the
    // resolver to walk), which is exactly the residual cost this pool
    // exists to remove. See [`VoleIrCtx::push_and_pair`] and the `@`-
    // prefix convention below.
    and_pool_next: usize,
    // Whether the statements currently being emitted (`self.stmts`, per
    // `emit_poly_wide`'s own redirection) belong to a closure body that
    // runs multiple times at RUNTIME (`core::array::from_fn(|i| {..})`,
    // once per lane `i`) rather than the enclosing function body proper,
    // which runs once. `_pool`/`_and_pool` are the SAME shared Vec inside
    // and outside the closure (see the field doc above) -- but a literal
    // index baked into the closure's own text would be wrong for every
    // lane but the first, since each lane invocation grows the SAME pool
    // further. While `in_closure`, [`VoleIrCtx::push_wire_scratch`]/
    // [`VoleIrCtx::push_and_pair`] instead lazily capture a RUNTIME base
    // offset (`_lane_base = _pool.len();`, captured fresh at the START of
    // each of the closure's own W invocations) and encode subsequent
    // indices as `_lane_base`-relative (the `%`/`^` prefix convention
    // below) -- correct regardless of what order `core::array::from_fn`
    // happens to invoke lanes in, since it only relies on invocations not
    // overlapping (true for an ordinary synchronous `FnMut`), never on a
    // particular order between them.
    in_closure: bool,
    pool_lane_base_declared: bool,
    and_pool_lane_base_declared: bool,
}

/// `(Vope<N, T, U1>, Array<T, N>)` — the element type of `_and_pool`,
/// see its doc on [`VoleIrCtx`].
fn and_pair_type() -> IrType {
    IrType::Tuple(vec![vope_type(), array_t_n()])
}

/// Text convention for wire-typed (`Vope`/`Q`) scratch values that live
/// in a shared pool array rather than as a uniquely named `let`
/// binding:
/// - a plain ASCII digit string is an *absolute* `_pool` index (produced
///   by [`VoleIrCtx::push_wire_scratch`] outside a closure);
/// - `@N` is an absolute `_and_pool` index (reads field `.0`, the wire
///   half -- see [`VoleIrCtx::push_and_pair`]);
/// - `%N` is a `_pool` index *relative to `_lane_base`*, valid only
///   inside `emit_poly_wide`'s own per-lane closure (see
///   [`VoleIrCtx::in_closure`]'s doc);
/// - `^N` is the closure-relative counterpart of `@N` (`_and_pool`
///   field `.0`, offset by `_and_lane_base`).
///
/// No real identifier this weaver ever generates starts with `@`, `%`,
/// `^`, or a digit (all are `snake_case` words or `_`-prefixed), so this
/// dispatch is unambiguous. Exists so the many existing `&str`/`String`-
/// typed call sites (`operand_lane`, `emit_and`, `emit_poly_lane`'s
/// `term_names`, ...) don't need a new enum type threaded through them
/// -- a name and a pool-slot reference are both "a way to read a
/// wire-typed value," this is just the dispatch between the forms.
fn wire_ref_raw(name_or_slot: &str) -> IrExpr {
    if let Some(idx) = name_or_slot.strip_prefix('@') {
        and_pool_field(idx, "0", false)
    } else if let Some(idx) = name_or_slot.strip_prefix('%') {
        lane_relative_index("_pool", "_lane_base", idx)
    } else if let Some(idx) = name_or_slot.strip_prefix('^') {
        and_pool_field(idx, "0", true)
    } else if name_or_slot.as_bytes().first().is_some_and(u8::is_ascii_digit) {
        arr_index("_pool", name_or_slot)
    } else {
        var(name_or_slot)
    }
}

fn wire_ref_expr(name_or_slot: &str) -> IrExpr {
    clone_expr(wire_ref_raw(name_or_slot))
}

/// Same convention as [`wire_ref_raw`], for hat reads -- every hat-typed
/// scratch value this weaver pools comes from an AND gate, so the only
/// pooled cases here are `_and_pool`'s field `.1` (absolute `@N`, or
/// closure-relative `^N`). Anything else is assumed to be a real bound
/// identifier (e.g. the `extracts_pair` wide-hat-extraction path's own
/// uniquely-numbered names, which this deliberately never pools).
fn hat_ref_raw(name_or_slot: &str) -> IrExpr {
    if let Some(idx) = name_or_slot.strip_prefix('@') {
        and_pool_field(idx, "1", false)
    } else if let Some(idx) = name_or_slot.strip_prefix('^') {
        and_pool_field(idx, "1", true)
    } else {
        var(name_or_slot)
    }
}

fn hat_ref_expr(name_or_slot: &str) -> IrExpr {
    clone_expr(hat_ref_raw(name_or_slot))
}

/// `_and_pool[idx].{field}` (absolute) or `_and_pool[_and_lane_base +
/// idx].{field}` (`lane_relative`) — `field` is `"0"` (wire) or `"1"`
/// (hat).
fn and_pool_field(idx: &str, field: &str, lane_relative: bool) -> IrExpr {
    let base = if lane_relative {
        lane_relative_index("_and_pool", "_and_lane_base", idx)
    } else {
        arr_index("_and_pool", idx)
    };
    ir_expr(IrExprKind::Field {
        base: Box::new(base),
        field: field.into(),
    })
}

/// `base_name[lane_base_var + idx]` — a pool read whose index is a
/// RUNTIME sum of a per-invocation base offset (captured once per
/// closure invocation, see [`VoleIrCtx::in_closure`]'s doc) and a
/// compile-time-known offset within that invocation. `idx` prints as a
/// literal (same convention as [`arr_index`]'s own `idx` argument).
fn lane_relative_index(base_name: &str, lane_base_var: &str, idx: &str) -> IrExpr {
    ir_expr(IrExprKind::Index {
        base: Box::new(var(base_name)),
        index: Box::new(ir_expr(IrExprKind::Binary {
            op: SpecBinOp::Add,
            left: Box::new(var(lane_base_var)),
            right: Box::new(var(idx)),
        })),
    })
}

/// `{ debug_check_pool_written(<pool_name>_written[slot], slot);
/// <pool_name>[slot].clone() }` -- the ONLY way a [`WireRepr::Pooled`]
/// slot should ever be read (see its own doc). `<pool_name>_written` is
/// the pool's own paired debug bitset (see [`written_array_name`]).
fn pooled_read_expr(pool_name: &str, slot: usize) -> IrExpr {
    let slot_str = slot.to_string();
    let written_name = written_array_name(pool_name);
    ir_expr(IrExprKind::Block(IrBlock {
        stmts: vec![ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Call {
            func: Box::new(ir_expr(IrExprKind::Path {
                segments: vec!["debug_check_pool_written".into()],
                type_args: vec![],
            })),
            args: vec![
                arr_index(&written_name, &slot_str),
                ir_expr(IrExprKind::Lit(IrLit::Int(slot as i128))),
            ],
        })))],
        expr: Some(Box::new(clone_expr(arr_index(pool_name, &slot_str)))),
    }))
}

/// The debug-bitset array name paired with a given pool's own param
/// name (e.g. `"_piece_pool"` -> `"_piece_pool_written"`).
fn written_array_name(pool_name: &str) -> String {
    format!("{pool_name}_written")
}

/// Emit `var_id`'s own value as either a direct pool write (scalar) or
/// a contribution to the caller's own return tuple (wide) -- the write-
/// side counterpart of [`pooled_read_expr`]/`WireRepr::Pooled`, shared
/// by every "this function exports var v to a later function, keyed by
/// v's own raw circuit var id" site: `synthetic_out` (cross-region CSE
/// sharing) *and*, as of block-boundary-export/running-accumulator
/// pooling, `is_active`/`done`/`next_pc_bits`/`next_state`/`ret_vals`
/// and the chunk-to-chunk running accumulator -- all draw var ids from
/// the same global circuit var-id space `_synth_pool` is sized to, so
/// reusing one pool across every category is safe by construction (var
/// ids never collide across categories) and needs no new pool
/// infrastructure. `stmts` is the caller's own statement list (`ctx.stmts`
/// for a ctx-based unsplit region, `wrapper_stmts` for a split wrapper --
/// both are plain `Vec<IrStmt>`, no `ctx` access needed for a pool
/// write).
fn export_scalar_or_tuple(
    stmts: &mut Vec<IrStmt>,
    ret_tuple_tys: &mut Vec<IrType>,
    ret_tuple_exprs: &mut Vec<IrExpr>,
    var_id: u32,
    ty: IrType,
    expr: IrExpr,
) {
    if !matches!(ty, IrType::Array { .. }) {
        let slot_str = var_id.to_string();
        stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
            left: Box::new(arr_index("_synth_pool", &slot_str)),
            right: Box::new(expr),
        }))));
        stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
            left: Box::new(arr_index(&written_array_name("_synth_pool"), &slot_str)),
            right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
        }))));
        return;
    }
    ret_tuple_tys.push(ty);
    ret_tuple_exprs.push(expr);
}

/// `&mut expr`
fn ref_mut_expr(expr: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::Unary { op: SpecUnaryOp::RefMut, expr: Box::new(expr) })
}

/// `&mut <name>[..]` -- pass a pool by mutable SLICE reference, for a
/// callee whose own param type is `&mut [T]` (see [`pool_slice_type`]).
/// Deliberately re-slices via an explicit `[..]` rather than relying on
/// deref coercion at the call site (`ref_mut_expr(var(name))`, i.e. plain
/// `&mut <name>`): that single-step coercion is exactly what `Vec<T>`
/// provides for free (`&mut Vec<T> -> &mut [T]`, `Vec`'s own `DerefMut`),
/// but does NOT reliably auto-apply for `Box<[T; K]>` at a call site (a
/// TWO-step coercion -- deref through `Box`, then unsize the resulting
/// `[T; K]` to `[T]` -- confirmed to fail with a real type mismatch when
/// `_piece_pool` moved from `Vec` to `Box<[T; K]>`, real-scale on
/// `mem_probe.rs`'s own forced-piece-splitting test). Explicit `[..]`
/// indexing forces the unsize step directly, works identically for both
/// `Vec<T>` (still used by some pools) and `Box<[T; K]>` alike.
fn slice_ref_mut_expr(name: &str) -> IrExpr {
    ref_mut_expr(ir_expr(IrExprKind::Index {
        base: Box::new(var(name)),
        index: Box::new(ir_expr(IrExprKind::Range { start: None, end: None, inclusive: false })),
    }))
}

/// `&mut [elem_ty]` -- an unsized slice reference, deliberately NOT a
/// fixed-size `[elem_ty; K]` array: a cross-piece pool's own total slot
/// count `K` isn't known until every piece in a region has been visited
/// (values get assigned pool slots progressively, discovered piece by
/// piece), but each piece's own signature must be built as it's emitted,
/// before `K` is final. A slice type sidesteps this entirely -- no
/// compile-time size in the TYPE at all, and Rust's own deref coercion
/// (`&mut Vec<T>` -> `&mut [T]`) means the wrapper can pass `&mut
/// _piece_pool` (a `Vec`, sized once `K` is finally known) directly to
/// every piece call without any explicit slicing.
fn pool_slice_type(elem_ty: IrType, mutable: bool) -> IrType {
    IrType::Reference {
        mutable,
        elem: Box::new(IrType::Array {
            kind: volar_compiler::ir::ArrayKind::Slice,
            elem: Box::new(elem_ty),
            len: volar_compiler::ir::ArrayLength::Const(0), // ignored for Slice
        }),
    }
}

/// `Vope::default()`
fn vope_default_call() -> IrExpr {
    ir_expr(IrExprKind::Call {
        func: Box::new(ir_expr(IrExprKind::Path { segments: vec!["Vope".into(), "default".into()], type_args: vec![] })),
        args: vec![],
    })
}

/// `Q::default()`
fn q_default_call() -> IrExpr {
    ir_expr(IrExprKind::Call {
        func: Box::new(ir_expr(IrExprKind::Path { segments: vec!["Q".into(), "default".into()], type_args: vec![] })),
        args: vec![],
    })
}

/// `let mut <name>: Box<[elem_ty; count]> = Box::new([<elem_default_expr>; count]);`
/// -- a pool's own backing storage, pre-sized (and, since `Vope`/`Q`/
/// `bool` all implement `Default`, safely value-initialized -- never
/// `MaybeUninit`/`unsafe`) once and for all at the point its true final
/// size is known (see `pool_slot`'s own doc: a pool's size isn't final
/// until every piece in a region has been visited). Heap-backed (`Box`,
/// see `volar_compiler::ir::box_type`'s own doc) rather than stack-backed
/// -- some pools run into the thousands of slots at real interpreter
/// scale, too large to put on the C stack safely. Statically sized
/// (`[elem_ty; count]`, not a growable `Vec`) rather than dynamically
/// sized -- the count really is known here, and a real `LirType`
/// (`Ptr`-backed via `HeapAllocExt`) exists for this shape, unlike a true
/// runtime-length `Vec<T>` (see `docs`/the LIR side of this abstraction).
/// `elem_default_expr` must be a zero/default-shaped expression (see
/// `box_new_array_expr`'s own doc) -- true for every caller here
/// (`vope_default_call()`/`q_default_call()`/`Lit(Bool(false))`).
fn pool_decl_stmt(name: &str, elem_ty: IrType, elem_default_expr: IrExpr, count: usize) -> IrStmt {
    let box_array_ty = volar_compiler::ir::box_array_type(elem_ty.clone(), count);
    let init = volar_compiler::ir::box_new_array_expr(elem_ty, elem_default_expr, count);
    ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::ident(name).as_mut(),
        ty: Some(box_array_ty),
        init: Some(init),
    })
}

/// `<lhs> = <value>;` -- the write-side counterpart of `arr_index`/
/// `lane_relative_index` reads, used by [`VoleIrCtx::push_wire_scratch`]/
/// [`VoleIrCtx::push_and_pair`] to write into a statically-sized `_pool`/
/// `_and_pool` slot directly (no `.push()` -- these are fixed-size `Box`ed
/// arrays, not growable `Vec`s; see `finalize_pools`'s own doc).
fn pool_index_write_stmt(lhs: IrExpr, value: IrExpr) -> IrStmt {
    ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
        left: Box::new(lhs),
        right: Box::new(value),
    })))
}

/// `Array::<T, N>::default()` -- the `_and_pool` tuple element's second
/// half (see [`and_pair_default_expr`]).
fn array_t_n_default_call() -> IrExpr {
    ir_expr(IrExprKind::Call {
        func: Box::new(ir_expr(IrExprKind::Path {
            segments: vec!["Array".into(), "default".into()],
            type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
        })),
        args: vec![],
    })
}

/// `(Vope::default(), Array::default())` -- `_and_pool`'s own element
/// default value (see [`and_pair_type`]), for [`pool_decl_stmt`].
fn and_pair_default_expr() -> IrExpr {
    ir_expr(IrExprKind::Tuple(vec![vope_default_call(), array_t_n_default_call()]))
}

/// `let <name> = <base> + i * <per_lane>;` -- a closure-relative pool base
/// offset, computed as a pure compile-time-known formula rather than a
/// runtime `.len()` read (see `emit_poly_wide`'s own closure-exit code,
/// the sole caller, for why: `base`/`per_lane` are both known once the
/// closure body has been built once, and `i` is the closure's own
/// `core::array::from_fn` index variable).
fn lane_base_decl_stmt(name: &str, base: usize, per_lane: usize) -> IrStmt {
    let value = ir_expr(IrExprKind::Binary {
        op: SpecBinOp::Add,
        left: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(base as i128)))),
        right: Box::new(ir_expr(IrExprKind::Binary {
            op: SpecBinOp::Mul,
            left: Box::new(var("i")),
            right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(per_lane as i128)))),
        })),
    });
    ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::ident(name),
        ty: None,
        init: Some(value),
    })
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

impl VoleIrCtx<'static> {
    fn new(is_prover: bool) -> Self {
        let mut ctx = VoleIrCtx {
            stmts: Vec::new(),
            wires: alloc::collections::BTreeMap::new(),
            and_counter: 0,
            hat_names: Vec::new(),
            ok_names: Vec::new(),
            q_and_names: Vec::new(),
            stor: alloc::collections::BTreeMap::new(),
            oracle_counter: 0,
            trace: MemoryTrace::default(),
            mem_timestamp: 0,
            role: if is_prover { VoleRole::Prover } else { VoleRole::Verifier },
            trace_sink: None,
            ext_oracle_map: alloc::collections::BTreeMap::new(),
            ext_action_map: alloc::collections::BTreeMap::new(),
            ext_oracle_counter: 0,
            ext_action_counter: 0,
            ext_rng_counter: 0,
            pool_next: 0,
            and_pool_next: 0,
            in_closure: false,
            pool_lane_base_declared: false,
            and_pool_lane_base_declared: false,
        };
        ctx
    }

    /// `QSim`-role constructor (Milestone 1.6): derives `q_and` values via
    /// `derive_and_q` instead of taking them as external parameters. Never
    /// folds (`trace_sink: None`) -- folding is `Verifier`-only, since
    /// `QSim`'s whole job is producing the `q_and`s the real `Verifier`
    /// function will itself fold.
    fn new_qsim() -> Self {
        let mut ctx = VoleIrCtx {
            stmts: Vec::new(),
            wires: alloc::collections::BTreeMap::new(),
            and_counter: 0,
            hat_names: Vec::new(),
            ok_names: Vec::new(),
            q_and_names: Vec::new(),
            stor: alloc::collections::BTreeMap::new(),
            oracle_counter: 0,
            trace: MemoryTrace::default(),
            mem_timestamp: 0,
            role: VoleRole::QSim,
            trace_sink: None,
            ext_oracle_map: alloc::collections::BTreeMap::new(),
            ext_action_map: alloc::collections::BTreeMap::new(),
            ext_oracle_counter: 0,
            ext_action_counter: 0,
            ext_rng_counter: 0,
            pool_next: 0,
            and_pool_next: 0,
            in_closure: false,
            pool_lane_base_declared: false,
            and_pool_lane_base_declared: false,
        };
        ctx
    }
}

impl<'a> VoleIrCtx<'a> {
    /// Verifier-only constructor with a [`VerifierTraceSink`] attached (see
    /// `trace_sink`'s field doc). Role is always `Verifier`: a prover
    /// artifact must never observe the verifier's fold-accumulator plumbing
    /// (there is nothing for the prover to fold -- `and_gate_step` needs the
    /// verifier's own `K_a, K_b, K_c` Q-shares), and `QSim` never folds at
    /// all (see [`VoleIrCtx::new_qsim`]).
    fn new_verifier_with_trace_sink(sink: &'a dyn VerifierTraceSink<()>) -> Self {
        let mut ctx = VoleIrCtx {
            stmts: Vec::new(),
            wires: alloc::collections::BTreeMap::new(),
            and_counter: 0,
            hat_names: Vec::new(),
            ok_names: Vec::new(),
            q_and_names: Vec::new(),
            stor: alloc::collections::BTreeMap::new(),
            oracle_counter: 0,
            trace: MemoryTrace::default(),
            mem_timestamp: 0,
            role: VoleRole::Verifier,
            trace_sink: Some(sink),
            ext_oracle_map: alloc::collections::BTreeMap::new(),
            ext_action_map: alloc::collections::BTreeMap::new(),
            ext_oracle_counter: 0,
            ext_action_counter: 0,
            ext_rng_counter: 0,
            pool_next: 0,
            and_pool_next: 0,
            in_closure: false,
            pool_lane_base_declared: false,
            and_pool_lane_base_declared: false,
        };
        ctx
    }

    /// Prepend `_pool`'s (and, Prover-only, `_and_pool`'s) declaration to
    /// `self.stmts`, now that this function's own final pool size
    /// (`self.pool_next`/`self.and_pool_next`) is known -- which is only
    /// true once every `push_wire_scratch`/`push_and_pair` call for this
    /// function has already happened. Must be called exactly once, as the
    /// very last step before this ctx's `stmts` get consumed into the
    /// final `IrFunction` body -- see [`Self::finalize_and_take_stmts`],
    /// the sole caller.
    ///
    /// Heap-backed, statically-sized (`Box<Array<T, count>>`, see
    /// `volar_compiler::ir::box_array_type`'s own doc) rather than a
    /// growable `Vec` -- this is why the declaration can't just be emitted
    /// eagerly at construction time the way it originally was: a `Vec`
    /// doesn't need its final size known up front, a static `Box`ed array
    /// does, and that size genuinely isn't known until every push for this
    /// function has happened.
    fn finalize_pools(&mut self) {
        let elem_ty = if self.role.is_prover() { vope_type() } else { q_type() };
        let elem_default = if self.role.is_prover() { vope_default_call() } else { q_default_call() };
        let mut prelude = vec![pool_decl_stmt("_pool", elem_ty, elem_default, self.pool_next)];
        if self.role.is_prover() {
            prelude.push(pool_decl_stmt("_and_pool", and_pair_type(), and_pair_default_expr(), self.and_pool_next));
        }
        prelude.extend(core::mem::take(&mut self.stmts));
        self.stmts = prelude;
    }

    /// [`Self::finalize_pools`] then take `self.stmts` -- the standard way
    /// a woven function's own statement list is read out of its `ctx` once
    /// building is complete (replaces a plain `ctx.stmts` field read, which
    /// would grab the pool-less/wrong-size-declaration statements built so
    /// far and never patch them).
    fn finalize_and_take_stmts(&mut self) -> Vec<IrStmt> {
        self.finalize_pools();
        core::mem::take(&mut self.stmts)
    }

    /// Push a wire-typed (`Vope`/`Q`) scratch value into the shared
    /// `_pool` (its final declaration prepended once, function-wide, by
    /// [`Self::finalize_pools`]) at the next free index, via a direct
    /// indexed write (`_pool[idx] = value;`) -- `_pool` is a statically-
    /// sized array, not a growable `Vec`, so there is no `.push()`.
    /// Returns the pool index as a digit-string -- an absolute index
    /// outside a closure, or a `%`-prefixed `_lane_base`-relative index
    /// inside one (see [`VoleIrCtx::in_closure`]'s doc). See
    /// [`wire_ref_expr`] for how callers turn this back into a read
    /// expression.
    fn push_wire_scratch(&mut self, value: IrExpr) -> String {
        if self.in_closure {
            // `_lane_base`'s own declaration is prepended once the
            // closure's own per-invocation slot count is known -- see
            // `emit_poly_wide`'s closure-exit code, which checks this same
            // `pool_lane_base_declared` flag. Only the flag is set here;
            // the actual runtime value (`{base} + i * {slots_per_lane}`)
            // can't be computed until this closure body is fully built.
            self.pool_lane_base_declared = true;
            let idx = self.pool_next;
            self.pool_next += 1;
            self.stmts.push(pool_index_write_stmt(
                lane_relative_index("_pool", "_lane_base", &idx.to_string()),
                value,
            ));
            return format!("%{idx}");
        }
        let idx = self.pool_next;
        self.pool_next += 1;
        self.stmts.push(pool_index_write_stmt(arr_index("_pool", &idx.to_string()), value));
        idx.to_string()
    }

    /// Push a Prover AND-gate's full `(wire, hat)` output pair into the
    /// shared `_and_pool` (its final declaration prepended once, function-
    /// wide, by [`Self::finalize_pools`]) at the next free index -- `value`
    /// should be the tuple-returning call expression itself (e.g. from
    /// [`vole_and_prover_step_expr`]), NOT a name referencing an
    /// already-`let`-bound tuple; this is the whole point (see
    /// [`VoleIrCtx`]'s own doc on `pool_next`). Returns an `@`-prefixed
    /// (or, inside a closure, `^`-prefixed -- see [`VoleIrCtx::in_closure`])
    /// index string usable as both a wire reference ([`wire_ref_raw`]
    /// reads field `.0`) and a hat reference ([`hat_ref_raw`] reads
    /// field `.1`).
    fn push_and_pair(&mut self, value: IrExpr) -> String {
        if self.in_closure {
            self.and_pool_lane_base_declared = true;
            let idx = self.and_pool_next;
            self.and_pool_next += 1;
            self.stmts.push(pool_index_write_stmt(
                lane_relative_index("_and_pool", "_and_lane_base", &idx.to_string()),
                value,
            ));
            return format!("^{idx}");
        }
        let idx = self.and_pool_next;
        self.and_pool_next += 1;
        self.stmts.push(pool_index_write_stmt(arr_index("_and_pool", &idx.to_string()), value));
        format!("@{idx}")
    }

    /// Get scalar wire name for a var id. Panics on `Pooled` -- this
    /// weaver's own `Pooled`-producing call sites are all within the
    /// split-driver family, which reads pooled values through
    /// `slot_expr`/`materialize` (an `IrExpr`, or a real name via
    /// materialization), never this raw `&str` accessor; callers that
    /// might genuinely hit a pooled var should call `materialize` first.
    fn scalar(&self, v: &CirVar) -> &str {
        match &self.wires[&v.0] {
            WireRepr::Scalar(s) => s,
            WireRepr::Vec(_) => panic!("expected scalar wire for v{}", v.0),
            WireRepr::Array(..) => panic!("expected scalar wire for v{} (found wide Array)", v.0),
            WireRepr::Pooled(..) => panic!("expected scalar wire for v{} (found Pooled -- call materialize() first)", v.0),
        }
    }

    /// Ensure `v`'s wire is materialized as `Scalar`/`Vec` (never
    /// `Array`/`Pooled`) -- idempotent and memoized: a `WireRepr::Array`
    /// is unpacked into `width` real bound locals (`let _mat_{v}_{j} =
    /// {arr}[j].clone();`), and a `WireRepr::Pooled` slot is read once
    /// into a single real bound local (`let _mat_{v} = { debug_check...;
    /// pool[slot].clone() };`) the first time this is called for `v`,
    /// with the result written back into `self.wires` as `Vec`/`Scalar`
    /// respectively, so any later call (or lookup) for the same `v` sees
    /// the already-materialized form and does no further work. Call this
    /// before any code path that needs genuine bound-local names (address
    /// composition, `Merge` combining, `vec_parts`) rather than just an
    /// `IrExpr` (which `slot_expr`/`operand_expr` can build directly,
    /// without ever materializing).
    fn materialize(&mut self, v: &CirVar) {
        match self.wires[&v.0].clone() {
            WireRepr::Array(arr_name, w) => {
                let names: Vec<String> = (0..w)
                    .map(|j| {
                        let n = format!("_mat_{}_{}", v.0, j);
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&n),
                            ty: None,
                            init: Some(clone_expr(arr_index(&arr_name, &j.to_string()))),
                        }));
                        n
                    })
                    .collect();
                self.wires.insert(v.0, WireRepr::Vec(names));
            }
            WireRepr::Pooled(pool_name, slot) => {
                let n = format!("_mat_pool_{}", v.0);
                self.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::ident(&n),
                    ty: None,
                    init: Some(pooled_read_expr(pool_name, slot)),
                }));
                self.wires.insert(v.0, WireRepr::Scalar(n));
            }
            WireRepr::Scalar(_) | WireRepr::Vec(_) => {}
        }
    }

    /// Get vec wire names for a var id, materializing first if `v` is
    /// currently a `WireRepr::Array`.
    fn vec_parts(&mut self, v: &CirVar) -> Vec<String> {
        self.materialize(v);
        match &self.wires[&v.0] {
            WireRepr::Vec(v) => v.clone(),
            WireRepr::Scalar(_) => panic!("expected vec wire"),
            WireRepr::Array(..) => unreachable!("materialize just ran"),
            WireRepr::Pooled(..) => unreachable!("materialize just ran"),
        }
    }

    /// The `IrExpr` for a return-slot variable, honouring its actual width:
    /// a scalar wire is a plain `clone()`; a `Vec` wire becomes a
    /// **fixed-size array** expression (`[a.clone(), b.clone(), ...]`), not
    /// a tuple of repeated elements — arrays are more efficient on
    /// supported targets and lower gracefully to a dynamic array via
    /// `lowering_dyn` when a runtime-sized version is needed later. Tuples
    /// are reserved for combining genuinely different return slots (see
    /// callers), not for repeating one type.
    fn slot_expr(&self, v: &CirVar) -> IrExpr {
        let wire = self.wires.get(&v.0).unwrap_or_else(|| panic!(
            "slot_expr: var {} not bound in this ctx (ctx has {} wires bound, lowest={:?}, highest={:?})",
            v.0, self.wires.len(), self.wires.keys().next(), self.wires.keys().next_back(),
        ));
        match wire {
            WireRepr::Scalar(s) => clone_expr(var(s)),
            WireRepr::Vec(names) => {
                ir_expr(IrExprKind::FixedArray(names.iter().map(|n| clone_expr(var(n))).collect()))
            }
            // `arr_name` is already a valid identifier (a real function
            // param name) -- clone the whole `[T; w]` array directly
            // rather than building a `w`-element `FixedArray` literal of
            // individually-indexed clones.
            WireRepr::Array(arr_name, _) => clone_expr(var(arr_name)),
            WireRepr::Pooled(pool_name, slot) => pooled_read_expr(pool_name, *slot),
        }
    }

    /// The `IrType` for a return-slot variable: `base_ty` (e.g.
    /// `vope_type()`/`q_type()`) for a scalar wire, or a fixed-size array
    /// of `base_ty` for a `Vec` wire — the type counterpart of
    /// [`Self::slot_expr`], same array-not-tuple rationale.
    fn slot_type(&self, v: &CirVar, base_ty: &IrType) -> IrType {
        let wire = self.wires.get(&v.0).unwrap_or_else(|| panic!(
            "slot_type: var {} not bound in this ctx (ctx has {} wires bound, lowest={:?}, highest={:?})",
            v.0, self.wires.len(), self.wires.keys().next(), self.wires.keys().next_back(),
        ));
        match wire {
            WireRepr::Scalar(_) | WireRepr::Pooled(..) => base_ty.clone(),
            WireRepr::Vec(names) => IrType::Array {
                kind: volar_compiler::ir::ArrayKind::FixedArray,
                elem: Box::new(base_ty.clone()),
                len: volar_compiler::ir::ArrayLength::Const(names.len()),
            },
            WireRepr::Array(_, w) => IrType::Array {
                kind: volar_compiler::ir::ArrayKind::FixedArray,
                elem: Box::new(base_ty.clone()),
                len: volar_compiler::ir::ArrayLength::Const(*w),
            },
        }
    }

    // ---- Primitive wire operations ----------------------------------------

    /// Emit a zero-valued wire (prover: Vope::default, verifier: Q::default).
    fn emit_zero(&mut self, name: &str) {
        if self.role.is_prover() {
            // Vope { u: Array::<Array<T,N>, U1>::default(), v: Array::<T,N>::default() }
            let u_default = ir_expr(IrExprKind::Call {
                func: Box::new(ir_expr(IrExprKind::Path {
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
                })),
                args: vec![],
            });
            self.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(name),
                ty: None,
                init: Some(ir_expr(IrExprKind::StructExpr {
                    kind: StructKind::Custom("Vope".into()),
                    type_args: vec![],
                    fields: vec![
                        ("u".into(), u_default),
                        ("v".into(), array_t_default()),
                    ],
                    rest: None,
                })),
            }));
        } else {
            self.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(name),
                ty: None,
                init: Some(q_struct(array_t_default())),
            }));
        }
    }

    /// Emit a one-valued wire (clone of the committed-one wire).
    fn emit_one(&mut self, name: &str) {
        let src = if self.role.is_prover() { "vope_one" } else { "q_one" };
        self.stmts.push(ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident(name),
            ty: None,
            init: Some(clone_expr(var(src))),
        }));
    }

    /// Build `a + b` (prover) or `Q { q: Array::from_fn(|i| a.q[i].clone()
    /// + b.q[i].clone()) }` (verifier/qsim) -- `a`/`b` may be real names
    /// or `_pool` indices (see [`wire_ref_raw`]), shared by [`Self::emit_xor`]
    /// (binds the result to a real name) and [`Self::emit_xor_scratch`]
    /// (pushes it onto `_pool` instead).
    fn xor_value_expr(&self, a: &str, b: &str) -> IrExpr {
        if self.role.is_prover() {
            ir_expr(IrExprKind::Binary {
                op: SpecBinOp::Add,
                left: Box::new(clone_expr(wire_ref_raw(a))),
                right: Box::new(clone_expr(wire_ref_raw(b))),
            })
        } else {
            q_struct(array_t_from_fn(
                "i",
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(clone_expr(q_index_of(wire_ref_raw(a), "i"))),
                    right: Box::new(clone_expr(q_index_of(wire_ref_raw(b), "i"))),
                }),
            ))
        }
    }

    /// Emit XOR (free: prover a + b, verifier element-wise), bound to a
    /// real name `out` -- used for a statement's own genuine output.
    fn emit_xor(&mut self, out: &str, a: &str, b: &str) {
        let value = self.xor_value_expr(a, b);
        self.stmts.push(ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident(out),
            ty: None,
            init: Some(value),
        }));
    }

    /// Same computation as [`Self::emit_xor`], but for a purely
    /// intermediate XOR-chain accumulator: pushes onto `_pool` and
    /// returns the index instead of binding a fresh name.
    fn emit_xor_scratch(&mut self, a: &str, b: &str) -> String {
        let value = self.xor_value_expr(a, b);
        self.push_wire_scratch(value)
    }

    /// Emit AND gate. Returns a reference to the output wire -- a real
    /// name for `Verifier` (its own `and_gate_step` trace-sink call needs
    /// real bound identifiers for `k_a`/`k_b`/`k_c`, a public API this
    /// phase doesn't touch — see the doc on the `VoleRole::Verifier` arm
    /// below), or a `_pool` index (see [`wire_ref_expr`]) for
    /// `Prover`/`QSim`, which have no such constraint.
    fn emit_and(&mut self, a: &str, b: &str) -> String {
        // Raw (un-cloned): `vole_and_prover_step_expr`/`derive_and_q_expr`/
        // `emit_verifier_and_gate` each clone_expr's/ref_expr's their own
        // `a_expr`/`b_expr` internally -- wrapping here too would
        // double-clone.
        let a_expr = wire_ref_raw(a);
        let b_expr = wire_ref_raw(b);
        match self.role {
            VoleRole::Prover => {
                // Push the whole `(wire, hat)` output pair directly --
                // no intermediate `let (_and_wire, _and_hat) = ..;`
                // binding at all (see `_and_pool`'s own doc on
                // `VoleIrCtx`).
                let value = vole_and_prover_step_expr(clone_expr(a_expr), clone_expr(b_expr), ());
                let idx = self.push_and_pair(value);
                self.hat_names.push(idx.clone());
                self.and_counter += 1;
                idx
            }
            VoleRole::Verifier => {
                // Left as real, uniquely-numbered names (not pooled):
                // `and_gate_step` (`VerifierTraceSink`, a `pub trait`
                // other crates could implement) takes `k_a`/`k_b`/`k_c` as
                // `&str`, used internally as real bound identifiers
                // (`clone_expr(var(k_a))` etc) -- pooling here would mean
                // either a breaking trait-signature change or synthesizing
                // extra un-pooled temp bindings just to satisfy it, which
                // defeats the point. Verifier's own functions already
                // benefited the most from the earlier `emit_poly_wide`
                // laziness fix (unconditionally eligible, no
                // `extracts_pair` exception) -- deferred here, not lost.
                let wire_name = format!("and_w_{}", self.and_counter);
                let ok_name = format!("ok_{}", self.and_counter);
                let idx = self.and_counter.to_string();
                self.ok_names.push(ok_name.clone());
                emit_verifier_and_gate(
                    a_expr, b_expr, &wire_name, &ok_name,
                    arr_index("q_and", &idx), arr_index("hat", &idx), &mut self.stmts, (),
                );
                if let Some(sink) = self.trace_sink {
                    let new_state = sink.and_gate_step(
                        self.and_counter, a, b, &wire_name, "delta",
                        arr_index("hat", &idx), arr_index("r_and", &idx), "fold_state", (),
                    );
                    self.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(var("fold_state")),
                        right: Box::new(new_state),
                    }))));
                }
                self.and_counter += 1;
                wire_name
            }
            VoleRole::QSim => {
                // hat is a required *input* param (same shape Verifier
                // already takes, now array-batched — indexed by gate
                // number rather than named per-gate); q_and_k is *derived*
                // here and collected as this function's own output (see
                // `q_and_names`). QSim's "wire" output IS its own
                // `q_and` (no separate value) -- push once, alias both
                // uses to the same `_pool` slot, no intermediate `let`.
                let gate_idx = self.and_counter.to_string();
                let value = derive_and_q_expr(a_expr, b_expr, arr_index("hat", &gate_idx), ());
                let idx = self.push_wire_scratch(value);
                self.q_and_names.push(idx.clone());
                self.and_counter += 1;
                idx
            }
        }
    }

    /// Emit NOT (free: a + one).
    fn emit_not(&mut self, out: &str, a: &str) {
        let one = if self.role.is_prover() { "vope_one" } else { "q_one" };
        self.emit_xor(out, a, one);
    }

    // ---- Poly (generalised gate) ------------------------------------------

    /// Fetch the bit-wire name for operand `v` at `lane` of a (possibly
    /// wide) `Poly` statement.
    ///
    /// Introspects `v`'s **own** representation rather than assuming it
    /// matches the statement's declared output width: a monomial can
    /// legitimately mix a scalar `Bit` selector with a wide value in the
    /// same term (e.g. `movfuscate_ir`'s own `is_active · val` gate
    /// formula, `Poly{[is_active, val]: 1}}`, where `is_active` is always
    /// `Bit` regardless of `val`'s width) — a scalar operand is reused
    /// verbatim at every lane (exactly the broadcast a selector bit needs),
    /// while a `Vec` operand is indexed per lane.
    fn operand_lane(&mut self, v: &CirVar, lane: usize) -> String {
        self.materialize(v);
        match &self.wires[&v.0] {
            WireRepr::Scalar(s) => s.clone(),
            WireRepr::Vec(parts) => parts[lane].clone(),
            WireRepr::Array(..) => unreachable!("materialize just ran"),
            WireRepr::Pooled(..) => unreachable!("materialize just ran"),
        }
    }

    /// Emit one bit-lane of a (possibly wide) `Poly` statement: the exact
    /// single-bit Quicksilver AND/XOR-chain formula this function has
    /// always used, parameterized by `lane` so [`Self::emit_poly`] can
    /// broadcast it across every bit of a `>1`-bit-wide value.
    fn emit_poly_lane(
        &mut self,
        out_name: &str,
        coeffs: &PolyCoeffs<CirVar>,
        const_bit: bool,
        lane: usize,
    ) {
        // Collect terms with odd coefficients.
        let mut term_names: Vec<String> = Vec::new();

        // Constant term (this lane's bit of the statement's constant).
        if const_bit {
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
                    term_names.push(self.operand_lane(&mono[0], lane));
                }
                _ => {
                    // degree ≥ 2: chain of ANDs
                    let mut acc = self.operand_lane(&mono[0], lane);
                    for k in 1..mono.len() {
                        let b = self.operand_lane(&mono[k], lane);
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
                self.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::ident(out_name),
                    ty: None,
                    init: Some(clone_expr(wire_ref_raw(&term_names[0]))),
                }));
            }
            _ => {
                // `acc` starts as term_names[0] itself (no redundant
                // "_xor0 = term_names[0].clone();" binding needed --
                // `emit_xor`/`emit_xor_scratch` already clone their own
                // `a`/`b` operands, so the first term's own clone happens
                // as part of the first reduction step below). Every
                // intermediate accumulator is pushed onto `_pool`
                // (`emit_xor_scratch`); only the final step binds the
                // statement's own real output name.
                let mut acc = term_names[0].clone();
                let n = term_names.len();
                for (i, tn) in term_names[1..].iter().enumerate() {
                    if i == n - 2 {
                        self.emit_xor(out_name, &acc, tn);
                    } else {
                        acc = self.emit_xor_scratch(&acc, tn);
                    }
                }
            }
        }
    }

    /// Emit a (possibly wide) `Poly` statement. `width == 1` is the
    /// original single-bit behaviour (a [`WireRepr::Scalar`]). `width > 1`
    /// dispatches to [`Self::emit_poly_wide`] — a single `Array::from_fn`
    /// runtime loop over the same per-lane formula, instead of `width`
    /// independently unrolled statement chains (see
    /// `docs/agent-context/boolar-ir-conflicts.md`, conflict #3, for why
    /// this matters: `width`-unrolled Quicksilver AND-checks are the
    /// dominant cost of movfuscation-heavy circuits). Falls back to the
    /// original per-lane-unrolled path (still correct for any shape) when
    /// [`Self::emit_poly_wide`]'s documented scope limit doesn't cover this
    /// statement.
    fn emit_poly(
        &mut self,
        out_name: &str,
        coeffs: &PolyCoeffs<CirVar>,
        constant: &volar_ir::ir::Constant,
        width: usize,
    ) -> WireRepr {
        if width <= 1 {
            let bit0 = constant.lo & 1 == 1;
            self.emit_poly_lane(out_name, coeffs, bit0, 0);
            return WireRepr::Scalar(out_name.to_string());
        }
        if width <= 64 && self.poly_wide_supported(coeffs) {
            self.emit_poly_wide(out_name, coeffs, constant, width)
        } else {
            self.emit_poly_unrolled(out_name, coeffs, constant, width)
        }
    }

    /// Per-lane-unrolled fallback (the original `emit_poly` width > 1
    /// body): `width` independent calls to [`Self::emit_poly_lane`]. Used
    /// only when [`Self::emit_poly_wide`]'s scope limit doesn't apply —
    /// correct for any monomial shape, just not collapsed.
    fn emit_poly_unrolled(
        &mut self,
        out_name: &str,
        coeffs: &PolyCoeffs<CirVar>,
        constant: &volar_ir::ir::Constant,
        width: usize,
    ) -> WireRepr {
        let names: Vec<String> = (0..width)
            .map(|lane| {
                let lane_name = format!("{}_{}", out_name, lane);
                let bit = if lane < 128 {
                    (constant.lo >> lane) & 1 == 1
                } else {
                    (constant.hi >> (lane - 128)) & 1 == 1
                };
                self.emit_poly_lane(&lane_name, coeffs, bit, lane);
                lane_name
            })
            .collect();
        WireRepr::Vec(names)
    }

    /// Whether [`Self::emit_poly_wide`] can handle this statement's
    /// monomial shape: every monomial of degree ≤ 2 (the only degree this
    /// weaver's actual callers ever produce — `BIrStmt::And` and
    /// `movfuscate_ir`'s `is_active · val` gate are always degree ≤ 2),
    /// and at most one degree-2 (AND) monomial (also the only shape ever
    /// produced in practice — see
    /// `docs/agent-context/boolar-ir-conflicts.md`, conflict #3, for the
    /// rationale for this deliberate, documented scope limit rather than a
    /// silent one).
    fn poly_wide_supported(&self, coeffs: &PolyCoeffs<CirVar>) -> bool {
        coeffs.keys().all(|mono| mono.len() <= 2)
    }

    /// The collapsed width > 1 path: emits exactly one
    /// `Array::from_fn(|i| { ... })` statement computing every bit lane's
    /// Quicksilver AND/XOR-chain formula at once (the constant term is
    /// decoded from the raw literal *at runtime*, `(CONST >> i) & 1`, not
    /// specialized per lane at codegen time — this is what makes a single
    /// shared loop body correct for every lane), followed by `width`
    /// trivial per-lane extraction statements so the result is still a
    /// [`WireRepr::Vec`] of `width` independently-named wires — the exact
    /// same contract [`Self::emit_poly_unrolled`] produces, so every other
    /// `Stmt` handler in [`Self::emit_circuit_stmts`] (`Merge`,
    /// `StorageRead`/`Write`, `Shuffle`, ...) needs no changes at all.
    ///
    /// Any wide (`WireRepr::Vec`) operand referenced is bundled into one
    /// local array once (`let {out}_o{v} = [name0, name1, ...];`) so it can
    /// be indexed by the closure's symbolic lane variable; a Bit/scalar
    /// operand (e.g. movfuscation's `is_active` selector) is broadcast
    /// as-is, unindexed, exactly as [`Self::operand_lane`] already does for
    /// the unrolled path. The (at most one) AND monomial's `hat`/`q_and`/
    /// (trace-sink) `r_and` parameters — already-declared per-bit scalar
    /// function parameters, unchanged — are bundled the same way so the
    /// single Quicksilver check inside the loop can index them per lane.
    fn emit_poly_wide(
        &mut self,
        out_name: &str,
        coeffs: &PolyCoeffs<CirVar>,
        constant: &volar_ir::ir::Constant,
        width: usize,
    ) -> WireRepr {
        // ---- 1. Bundle every wide operand referenced, once each. -----------
        let mut wide_ops: Vec<CirVar> = Vec::new();
        for mono in coeffs.keys() {
            for v in mono {
                if wide_ops.contains(v) { continue; }
                if matches!(&self.wires[&v.0], WireRepr::Vec(_) | WireRepr::Array(..)) {
                    wide_ops.push(*v);
                }
            }
        }
        let mut bundle: alloc::collections::BTreeMap<u32, String> = alloc::collections::BTreeMap::new();
        for v in &wide_ops {
            match &self.wires[&v.0] {
                // Already array-shaped (a top-level circuit param kept
                // as a real `[T; w]` array rather than eagerly unpacked
                // -- see `WireRepr::Array`'s own doc) -- no bundling
                // statement needed, index it directly.
                WireRepr::Array(arr_name, _) => {
                    bundle.insert(v.0, arr_name.clone());
                }
                WireRepr::Vec(names) => {
                    let bname = format!("{out_name}_o{}", v.0);
                    let arr = ir_expr(IrExprKind::FixedArray(
                        names.iter().map(|n| clone_expr(var(n))).collect(),
                    ));
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&bname),
                        ty: None,
                        init: Some(arr),
                    }));
                    bundle.insert(v.0, bname);
                }
                WireRepr::Scalar(_) => unreachable!(),
                WireRepr::Pooled(..) => unreachable!(),
            }
        }

        // ---- 2. Bundle every AND monomial's hat/q_and/r_and, one bundle
        //         group per monomial (movfuscation's own `is_active·(a+b)
        //         + b` slot-accumulation formula expands to *two* AND
        //         monomials per `Poly`, not one — this must handle any
        //         count, not just the single-AND `is_active · val` case). --
        struct AndBundle { start: usize, hat: String, q_and: String, r: String }
        let mut and_bundles: Vec<AndBundle> = Vec::new();
        for (gi, _mono) in coeffs.iter().filter(|(m, c)| *c % 2 == 1 && m.len() == 2).map(|(m, _)| m).enumerate() {
            let start = self.and_counter;
            self.and_counter += width;
            let mut b = AndBundle { start, hat: String::new(), q_and: String::new(), r: String::new() };
            match self.role {
                VoleRole::Prover => {}
                VoleRole::Verifier => {
                    // Read from the array-batched `hat`/`q_and`/`r_and`
                    // params by index (`clone`, since Rust can't move an
                    // element out of an array by index) rather than moving
                    // named per-gate scalar params — the only place in this
                    // function that needs a real semantic change, not just
                    // a rename, since every other reader already reads
                    // through this local bundle by runtime lane index.
                    b.hat = format!("{out_name}_h{gi}_{start}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&b.hat),
                        ty: None,
                        init: Some(ir_expr(IrExprKind::FixedArray(
                            (start..start + width).map(|k| clone_expr(arr_index("hat", &k.to_string()))).collect(),
                        ))),
                    }));
                    b.q_and = format!("{out_name}_q{gi}_{start}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&b.q_and),
                        ty: None,
                        init: Some(ir_expr(IrExprKind::FixedArray(
                            (start..start + width).map(|k| clone_expr(arr_index("q_and", &k.to_string()))).collect(),
                        ))),
                    }));
                    if self.trace_sink.is_some() {
                        b.r = format!("{out_name}_r{gi}_{start}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&b.r),
                            ty: None,
                            init: Some(ir_expr(IrExprKind::FixedArray(
                                (start..start + width).map(|k| clone_expr(arr_index("r_and", &k.to_string()))).collect(),
                            ))),
                        }));
                    }
                }
                VoleRole::QSim => {
                    // Only `hat` is bundled as an *input* (same shape
                    // Verifier takes) -- `q_and` is *derived* per lane
                    // below and collected as an output, no `r` (no fold).
                    b.hat = format!("{out_name}_h{gi}_{start}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&b.hat),
                        ty: None,
                        init: Some(ir_expr(IrExprKind::FixedArray(
                            (start..start + width).map(|k| clone_expr(arr_index("hat", &k.to_string()))).collect(),
                        ))),
                    }));
                }
            }
            and_bundles.push(b);
        }
        let and_count_here = and_bundles.len();

        // ---- 3. Build the closure body (redirect self.stmts to a scratch
        //         buffer; restored below, exactly as e.g. `hat_names`
        //         bookkeeping already assumes single-threaded, in-order use). ---
        let saved_stmts = core::mem::take(&mut self.stmts);
        // The closure body runs `width` times at RUNTIME (once per lane),
        // sharing the SAME `_pool`/`_and_pool` as the enclosing function
        // (declared once, by `declare_pools`) -- but its own index
        // bookkeeping must restart fresh *per invocation*, hence the
        // `pool_next`/`and_pool_next` reset (their meaning switches from
        // "absolute pool index" to "offset from this invocation's own
        // `_lane_base`" -- see `VoleIrCtx::in_closure`'s field doc). On
        // exit, the OUTER counters are restored not to their pre-closure
        // snapshot but ADVANCED by `width` invocations' worth of
        // contributions, since the closure's own pushes really did grow
        // the shared pool by that much.
        let saved_pool = (self.pool_next, self.and_pool_next, self.in_closure);
        self.pool_next = 0;
        self.and_pool_next = 0;
        self.in_closure = true;
        self.pool_lane_base_declared = false;
        self.and_pool_lane_base_declared = false;

        let operand_expr = |ctx: &Self, v: &CirVar| -> IrExpr {
            match &ctx.wires[&v.0] {
                WireRepr::Scalar(s) => clone_expr(var(s)),
                WireRepr::Vec(_) | WireRepr::Array(..) => clone_expr(arr_index(&bundle[&v.0], "i")),
                // A pooled scalar behaves exactly like `Scalar` here --
                // reused verbatim at every lane (it's not `Vec`/`Array`,
                // so there's no per-lane indexing to do). `_piece_pool`
                // (or any future pool) is an ordinary function parameter,
                // visible via normal closure capture regardless of
                // whether this operand is read inside `emit_poly_wide`'s
                // own per-lane closure or not.
                WireRepr::Pooled(pool_name, slot) => pooled_read_expr(pool_name, *slot),
            }
        };

        // Constant term, decoded at *runtime* from the raw literal — the
        // same closure body must be correct for every lane, so the bit
        // can't be baked in per-lane at codegen time. `poly_wide_supported`
        // guarantees `width <= 64`, so the constant fits in a `u64` cast —
        // sidesteps `i128`/`u128` literal-sign edge cases entirely (our
        // real circuits never need more than 64 bits per value).
        self.emit_zero("_zero");
        let one_name = if self.role.is_prover() { "vope_one" } else { "q_one" };
        let const_lit = ir_expr(IrExprKind::Cast {
            expr: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(constant.lo as u64 as i128)))),
            ty: Box::new(IrType::Primitive(PrimitiveType::U64)),
        });
        let bit_check = ir_expr(IrExprKind::Binary {
            op: SpecBinOp::Eq,
            left: Box::new(ir_expr(IrExprKind::Binary {
                op: SpecBinOp::BitAnd,
                left: Box::new(ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::Shr,
                    left: Box::new(const_lit),
                    right: Box::new(var("i")),
                })),
                right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(1)))),
            })),
            right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(1)))),
        });
        let cst_name = "_cst".to_string();
        self.stmts.push(ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident(&cst_name),
            ty: None,
            init: Some(ir_expr(IrExprKind::If {
                cond: Box::new(bit_check),
                then_branch: IrBlock { stmts: vec![], expr: Some(Box::new(clone_expr(var(one_name)))) },
                else_branch: Some(Box::new(ir_expr(IrExprKind::Block(IrBlock {
                    stmts: vec![],
                    expr: Some(Box::new(clone_expr(var("_zero")))),
                })))),
            })),
        }));

        // Every term gets a *uniquely-numbered* local name (`_t{term_idx}`,
        // `_aw{term_idx}`, ...) -- a `Poly` routinely has more than one
        // degree-0/1 monomial (e.g. `emit_xor`'s own `a + b` shape is two
        // degree-1 terms), and reusing one fixed name across them would
        // silently shadow earlier terms instead of XOR-ing them all in.
        let mut term_names: Vec<String> = vec![cst_name];
        let mut hat_locals: Vec<String> = Vec::new();
        let mut q_and_locals: Vec<String> = Vec::new();
        let mut term_idx: usize = 0;
        let mut and_gi: usize = 0;
        for (mono, &coeff) in coeffs {
            if coeff % 2 == 0 { continue; }
            term_idx += 1;
            match mono.len() {
                0 => {
                    let n = format!("_c0_{term_idx}");
                    self.emit_one(&n);
                    term_names.push(n);
                }
                1 => {
                    let idx = self.push_wire_scratch(operand_expr(self, &mono[0]));
                    term_names.push(idx);
                }
                2 => {
                    let b = &and_bundles[and_gi];
                    and_gi += 1;
                    let a_op = operand_expr(self, &mono[0]);
                    let b_op = operand_expr(self, &mono[1]);
                    let wire_n: String;
                    match self.role {
                        VoleRole::Prover => {
                        // Push the whole `(wire, hat)` output pair
                        // directly onto `_and_pool` -- no intermediate
                        // `let (_aw, _ah) = ..;` binding at all (see
                        // `_and_pool`'s own doc on `VoleIrCtx`). `a_op`/
                        // `b_op` (`operand_expr`'s result) are already
                        // `clone_expr(...)`-wrapped -- don't wrap again.
                        let value = vole_and_prover_step_expr(a_op, b_op, ());
                        let idx = self.push_and_pair(value);
                        hat_locals.push(idx.clone());
                        wire_n = idx;
                        }
                        VoleRole::Verifier => {
                        // Real, uniquely-numbered names throughout (not
                        // pooled): `and_gate_step` (`VerifierTraceSink`, a
                        // `pub trait`) needs real bound identifiers for
                        // `k_a`/`k_b`/`k_c`/`hat` -- same constraint as
                        // `emit_and`'s own Verifier branch, see its doc.
                        let ka_n = format!("_ka_{term_idx}");
                        let kb_n = format!("_kb_{term_idx}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&ka_n), ty: None, init: Some(a_op),
                        }));
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&kb_n), ty: None, init: Some(b_op),
                        }));
                        let wire_n_real = format!("_aw_{term_idx}");
                        let hat_n = format!("_lane_hat_{term_idx}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&hat_n), ty: None,
                            init: Some(clone_expr(arr_index(&b.hat, "i"))),
                        }));
                        let ok_n = format!("_aok_{term_idx}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::Tuple(vec![IrPattern::ident(&wire_n_real), IrPattern::ident(&ok_n)]),
                            ty: None,
                            init: Some(ir_expr(IrExprKind::Call {
                                func: Box::new(ir_expr(IrExprKind::Path {
                                    segments: vec!["vole_and_verifier_check".into()],
                                    type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
                                })),
                                args: vec![
                                    var("delta"),
                                    ref_expr(var(&ka_n)),
                                    ref_expr(var(&kb_n)),
                                    ref_expr(arr_index(&b.q_and, "i")),
                                    ref_expr(var(&hat_n)),
                                ],
                            })),
                        }));
                        self.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                            left: Box::new(var("all_ok")),
                            right: Box::new(ir_expr(IrExprKind::Binary {
                                op: SpecBinOp::And,
                                left: Box::new(var("all_ok")),
                                right: Box::new(var(&ok_n)),
                            })),
                        }))));
                        if let Some(sink) = self.trace_sink {
                            let r_n = format!("_ar_{term_idx}");
                            self.stmts.push(ir_stmt(IrStmtKind::Let {
                                pattern: IrPattern::ident(&r_n), ty: None,
                                init: Some(clone_expr(arr_index(&b.r, "i"))),
                            }));
                            let new_state = sink.and_gate_step(
                                b.start, &ka_n, &kb_n, &wire_n_real, "delta", var(&hat_n), var(&r_n), "fold_state", (),
                            );
                            self.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                                left: Box::new(var("fold_state")),
                                right: Box::new(new_state),
                            }))));
                        }
                        wire_n = wire_n_real;
                        }
                        VoleRole::QSim => {
                        // Push `derive_and_q(..)`'s result directly --
                        // no intermediate `let _aw = ..;` binding. `a_op`/
                        // `b_op` are already `clone_expr(...)`-wrapped
                        // (`operand_expr`'s result); `derive_and_q_expr`
                        // does its own `ref_expr` wrapping around each
                        // arg, matching what this call site built by hand
                        // before.
                        let value = derive_and_q_expr(a_op, b_op, clone_expr(arr_index(&b.hat, "i")), ());
                        wire_n = self.push_wire_scratch(value);
                        q_and_locals.push(wire_n.clone());
                        }
                    }
                    term_names.push(wire_n);
                }
                _ => unreachable!("poly_wide_supported guarantees degree <= 2"),
            }
        }

        // XOR-reduce all terms (identical structure to `emit_poly_lane`'s
        // own reduction, just operating on this closure's local stmt
        // buffer via the redirected `self.stmts`).
        let final_name = "_result".to_string();
        match term_names.len() {
            0 => self.emit_zero(&final_name),
            1 => {
                self.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::ident(&final_name),
                    ty: None,
                    init: Some(clone_expr(wire_ref_raw(&term_names[0]))),
                }));
            }
            _ => {
                // Same "no redundant first-clone" shape as
                // `emit_poly_lane`'s own reduction -- see its own comment.
                let mut acc = term_names[0].clone();
                let n = term_names.len();
                for (i, tn) in term_names[1..].iter().enumerate() {
                    if i == n - 2 {
                        self.emit_xor(&final_name, &acc, tn);
                    } else {
                        acc = self.emit_xor_scratch(&acc, tn);
                    }
                }
            }
        }

        let has_ands = and_count_here > 0;
        let trailing = match self.role {
            // `hat_locals`/`q_and_locals` are always `_pool`/`_hat_pool`
            // indices now (both their producer sites push to pool) --
            // *must* `.clone()` (via `hat_ref_expr`/`wire_ref_expr`, not
            // the raw/move variants), since moving an element out of a
            // `Vec` by index isn't allowed.
            VoleRole::Prover if has_ands => ir_expr(IrExprKind::Tuple(vec![
                var(&final_name),
                ir_expr(IrExprKind::Tuple(hat_locals.iter().map(|h| hat_ref_expr(h)).collect())),
            ])),
            VoleRole::QSim if has_ands => ir_expr(IrExprKind::Tuple(vec![
                var(&final_name),
                ir_expr(IrExprKind::Tuple(q_and_locals.iter().map(|q| wire_ref_expr(q)).collect())),
            ])),
            _ => var(&final_name),
        };
        let mut body_stmts = core::mem::replace(&mut self.stmts, saved_stmts);
        // This closure's own final counters ARE its per-invocation slot
        // counts (`pool_next`/`and_pool_next` never advance except via
        // `push_wire_scratch`/`push_and_pair`, which reset-and-count
        // fresh per closure -- see the entry comment above) -- multiply
        // by `width` (one full run of the closure body per lane) to get
        // the TOTAL contribution to the shared pool, and advance the
        // restored outer counters by that, rather than simply restoring
        // their pre-closure snapshot: the shared `_pool`/`_and_pool`
        // really did grow by this much, and any push AFTER this
        // statement (in the outer, non-closure scope) needs a literal
        // index reflecting that.
        let wire_slots_per_lane = self.pool_next;
        let and_slots_per_lane = self.and_pool_next;
        let (saved_pool_next, saved_and_pool_next, saved_in_closure) = saved_pool;
        // Prepend `_lane_base`/`_and_lane_base`'s own declaration now that
        // this closure's per-invocation slot count is finally known (see
        // `push_wire_scratch`/`push_and_pair`'s own doc on why this can't
        // happen any earlier -- they only set the flag, they don't emit the
        // statement themselves). `{saved_pool_next} + i * {slots_per_lane}`
        // gives invocation `i`'s own absolute base offset into the shared
        // pool -- a pure compile-time-known formula, replacing what used to
        // be a runtime `_pool.len()` read (meaningless now that `_pool` is
        // a fixed-size array whose `.len()` never changes).
        if self.and_pool_lane_base_declared {
            body_stmts.insert(0, lane_base_decl_stmt("_and_lane_base", saved_and_pool_next, and_slots_per_lane));
        }
        if self.pool_lane_base_declared {
            body_stmts.insert(0, lane_base_decl_stmt("_lane_base", saved_pool_next, wire_slots_per_lane));
        }
        self.pool_next = saved_pool_next + wire_slots_per_lane * width;
        self.and_pool_next = saved_and_pool_next + and_slots_per_lane * width;
        self.in_closure = saved_in_closure;
        let closure_body = ir_expr(IrExprKind::Block(IrBlock { stmts: body_stmts, expr: Some(Box::new(trailing)) }));

        // ---- 4. One `core::array::from_fn` statement for the whole lane
        //         range (a plain `[Elem; W]`, not `Array<T,N>` — that's
        //         the unrelated VOLE-repetition dimension). An explicit
        //         type annotation is required so the const generic `W`
        //         (the array length) can be inferred.
        let elem_ty = match self.role {
            VoleRole::Prover if has_ands => IrType::Tuple(vec![vope_type(), IrType::Tuple(vec![array_t_n(); and_count_here])]),
            VoleRole::Prover => vope_type(),
            VoleRole::QSim if has_ands => IrType::Tuple(vec![q_type(), IrType::Tuple(vec![q_type(); and_count_here])]),
            VoleRole::Verifier | VoleRole::QSim => q_type(),
        };
        let arr_name = format!("{out_name}_arr");
        self.stmts.push(ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident(&arr_name),
            ty: Some(IrType::Array {
                kind: volar_compiler::ir::ArrayKind::FixedArray,
                elem: Box::new(elem_ty),
                len: volar_compiler::ir::ArrayLength::Const(width),
            }),
            init: Some(fixed_array_from_fn("i", closure_body)),
        }));

        // ---- 5. Per-lane extraction -- only when actually load-bearing.
        //
        // `extracts_pair` true means `arr_name`'s own backing array
        // holds *tuples* `(wire_value, hats/q_and-bundle)`, not raw
        // wire values (see `elem_ty` above) -- `WireRepr::Array`'s own
        // contract (every consumer in this file: `materialize`,
        // `slot_expr`, `Stmt::Transmute`'s pass-through, this
        // function's own step-1 operand bundling) assumes indexing the
        // array directly yields a raw `Vope`/`Q`, so in this case we
        // must eagerly strip the tuple down to real per-lane names
        // (also true for `self.hat_names`/`self.q_and_names`, which are
        // flat `Vec<String>` accumulated across every AND-gate in the
        // whole function and later read back by name to build the
        // return tuple's own hats/q_and arrays -- extending those to
        // carry array-index references instead of names is a separate,
        // larger change, not attempted here).
        //
        // When `extracts_pair` is false (`Verifier`, unconditionally --
        // its own AND-check bundles are *input* params built in step 2,
        // unrelated to this output array's element type; or `Prover`/
        // `QSim` with no AND monomial in this particular `Poly`),
        // `arr_name` already holds raw wire values directly (`elem_ty`
        // is plain `vope_type()`/`q_type()`) -- returning
        // `WireRepr::Array` here, skipping `width` per-lane `let`s
        // entirely, is exactly what `WireRepr::Array`'s own lazy
        // `materialize` contract exists for: real per-lane names only
        // get extracted later, if and when some specific downstream
        // consumer genuinely needs them (many operations -- cloning the
        // whole value onward, indexing a single lane, threading it as a
        // piece's own extra_out -- don't).
        let extracts_pair = has_ands && matches!(self.role, VoleRole::Prover | VoleRole::QSim);
        if !extracts_pair {
            return WireRepr::Array(arr_name, width);
        }
        let names: Vec<String> = (0..width)
            .map(|k| {
                let ln = format!("{out_name}_{k}");
                let base = ir_expr(IrExprKind::Field {
                    base: Box::new(arr_index(&arr_name, &k.to_string())),
                    field: "0".into(),
                });
                self.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::ident(&ln), ty: None, init: Some(clone_expr(base)),
                }));
                ln
            })
            .collect();
        if self.role.is_prover() {
            // `and_counter` allocated hats *group-major* (all `width` hats
            // for AND-group 0, then all `width` for group 1, ...) — this
            // loop must push into `self.hat_names` in that exact same
            // order, since the verifier's `hat_0..hat_{and_count-1}`
            // parameter list and the driver that feeds the prover's
            // returned hats array into it both assume that ordering.
            for gi in 0..and_count_here {
                for k in 0..width {
                    let hat_expr = clone_expr(ir_expr(IrExprKind::Field {
                        base: Box::new(ir_expr(IrExprKind::Field {
                            base: Box::new(arr_index(&arr_name, &k.to_string())),
                            field: "1".into(),
                        })),
                        field: gi.to_string(),
                    }));
                    let hn = format!("{out_name}_hat_{gi}_{k}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&hn), ty: None, init: Some(hat_expr),
                    }));
                    self.hat_names.push(hn);
                }
            }
        } else {
            // role == QSim (extracts_pair rules out Verifier here). Same
            // group-major ordering as the prover's `hat_names` above
            // (and as `emit_and`'s narrow-path `q_and_names` pushes) --
            // this is what `weave_vole_qsim_ir_with_mode`/`_split` read to
            // build the returned `q_and` array, in `and_counter` order.
            for gi in 0..and_count_here {
                for k in 0..width {
                    let q_and_expr = clone_expr(ir_expr(IrExprKind::Field {
                        base: Box::new(ir_expr(IrExprKind::Field {
                            base: Box::new(arr_index(&arr_name, &k.to_string())),
                            field: "1".into(),
                        })),
                        field: gi.to_string(),
                    }));
                    let qn = format!("{out_name}_qand_{gi}_{k}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&qn), ty: None, init: Some(q_and_expr),
                    }));
                    self.q_and_names.push(qn);
                }
            }
        }
        WireRepr::Vec(names)
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

        self.materialize(addr_var);
        let full_addr: Vec<String> = match &self.wires[&addr_var.0] {
            WireRepr::Scalar(s) => vec![s.clone()],
            WireRepr::Vec(v) => v.clone(),
            WireRepr::Array(..) => unreachable!("materialize just ran"),
            WireRepr::Pooled(..) => unreachable!("materialize just ran"),
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
            self.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(&bit_name),
                ty: None,
                init: Some(clone_expr(var(&r))),
            }));
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

        self.materialize(addr_var);
        let full_addr: Vec<String> = match &self.wires[&addr_var.0] {
            WireRepr::Scalar(s) => vec![s.clone()],
            WireRepr::Vec(v) => v.clone(),
            WireRepr::Array(..) => unreachable!("materialize just ran"),
            WireRepr::Pooled(..) => unreachable!("materialize just ran"),
        };
        let aw = Self::effective_addr_width(cell_count);
        let addr_bits: Vec<String> = full_addr[..aw].to_vec();

        let vw = cir_type_width(val_ty, types);
        let src_bits: Vec<String> = if vw == 1 {
            self.materialize(src_var);
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

    /// Commitment-mode read: use one oracle parameter **per bit lane** as
    /// the value (0 AND gates either way) — a `_32`-typed read needs 32
    /// independent committed bits, not one, matching [`count_storage_reads`]'s
    /// per-lane counting. Returns the [`WireRepr`] the caller should record
    /// for this var (`Scalar` at width 1, `Vec` of `width` lane names
    /// otherwise — same convention [`VoleIrCtx::emit_storage_read`] (Tree
    /// mode) already uses).
    fn emit_storage_read_committed(
        &mut self,
        out_name: &str,
        var_id: u32,
        storage_id: u32,
        type_id: u32,
        addr_var: &CirVar,
        types: &CirTypes,
        val_ty: &CirTyId,
    ) -> WireRepr {
        let width = cir_type_width(val_ty, types);
        let repr = if width <= 1 {
            let param_name = format!("oracle_rd_{}", self.oracle_counter);
            self.oracle_counter += 1;
            self.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(out_name),
                ty: None,
                init: Some(clone_expr(var(&param_name))),
            }));
            WireRepr::Scalar(out_name.to_string())
        } else {
            let names: Vec<String> = (0..width)
                .map(|lane| {
                    let param_name = format!("oracle_rd_{}", self.oracle_counter);
                    self.oracle_counter += 1;
                    let lane_name = format!("{}_{}", out_name, lane);
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&lane_name),
                        ty: None,
                        init: Some(clone_expr(var(&param_name))),
                    }));
                    lane_name
                })
                .collect();
            WireRepr::Vec(names)
        };

        self.trace.entries.push(MemoryTraceEntry {
            addr_var: addr_var.0,
            value_var: var_id,
            storage_id,
            type_id,
            is_write: false,
            timestamp: self.mem_timestamp,
        });
        self.mem_timestamp += 1;
        repr
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

    /// Concatenate `parts`' bit-wires (LSB-first, per `docs/wasm-feature-support.md`'s
    /// memory-model note) into one wider `Vec`. Each part contributes its
    /// **own** width, not necessarily one bit — e.g. `i32.load` merges four
    /// already-`_8`-wide (`Vec`-of-8) byte reads into one `_32` value, not
    /// four individual bits — so a scalar part contributes 1 name and a
    /// `Vec` part contributes all of its names, in order.
    fn emit_merge(&mut self, out_id: u32, parts: &[CirVar]) {
        let mut names: Vec<String> = Vec::new();
        for v in parts {
            self.materialize(v);
            match &self.wires[&v.0] {
                WireRepr::Scalar(s) => names.push(s.clone()),
                WireRepr::Vec(bits) => names.extend(bits.iter().cloned()),
                WireRepr::Array(..) => unreachable!("materialize just ran"),
                WireRepr::Pooled(..) => unreachable!("materialize just ran"),
            }
        }
        self.wires.insert(out_id, WireRepr::Vec(names));
        // No runtime code emitted — purely a tracking operation.
    }

    fn emit_shuffle(&mut self, out_name: &str, out_id: u32, result_bits: &[(u8, CirVar)]) {
        if result_bits.len() == 1 {
            let (bit_idx, src_var) = &result_bits[0];
            // Pure bit-extraction alias: when the source bit is *already*
            // a real bound name (a `WireRepr::Vec` entry, or the whole
            // value when `Scalar` and `bit_idx == 0`), the Shuffle's own
            // output can just alias that name directly -- skipping a
            // `let out = src_bit.clone();` statement entirely. Real,
            // measured motivation: every single-bit Shuffle in the real
            // interpreter's own circuit (23,408 of them) is part of a
            // tightly clustered "decompose one wide value into its
            // individual bits" group (544 groups sharing one source var
            // each, ~99% of same-group statements within 4 statements of
            // each other) -- exactly the shape this skips a statement for.
            match self.wires[&src_var.0].clone() {
                WireRepr::Scalar(s) if *bit_idx == 0 => {
                    self.wires.insert(out_id, WireRepr::Scalar(s));
                    return;
                }
                WireRepr::Vec(names) => {
                    let s = names[*bit_idx as usize].clone();
                    self.wires.insert(out_id, WireRepr::Scalar(s));
                    return;
                }
                // `Array` source (rare: a top-level param shuffled
                // directly) and mismatched-width `Scalar` (shouldn't
                // happen) fall through to the materializing path below.
                _ => {}
            }
            let src_parts = self.vec_parts(src_var);
            let src_name = &src_parts[*bit_idx as usize];
            self.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(out_name),
                ty: None,
                init: Some(clone_expr(var(src_name))),
            }));
            self.wires.insert(out_id, WireRepr::Scalar(out_name.to_string()));
        } else {
            // Multi-bit shuffle → Vec result.
            let names: Vec<String> = result_bits.iter().enumerate().map(|(i, (bit_idx, src_var))| {
                let src_parts = self.vec_parts(src_var);
                let src_name = &src_parts[*bit_idx as usize];
                let n = format!("{}_{}", out_name, i);
                self.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::ident(&n),
                    ty: None,
                    init: Some(clone_expr(var(src_name))),
                }));
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

        // Register input wires -- width-aware: a `_32`-typed (or `Vec(32,_)`)
        // input param needs 32 independent per-lane wires, matching the
        // param-list generation in `weave_vole_prover_ir_with_mode`/
        // `weave_vole_verifier_ir_with_mode(_and_trace)`. Array-batched on
        // the *param* side (one `w_{i}: [T; width]` array, not `width`
        // scalar params -- see `docs/agent-context/circuit-size-optimization-backlog.md`'s
        // 65535-arg-limit finding); kept as a lazy `WireRepr::Array`
        // rather than eagerly unpacked into `width` bound locals -- any
        // consumer needing real names materializes on demand (see
        // `WireRepr::Array`'s own doc for why eager unpacking here was a
        // real, measured blowup once top-level parameter threading made
        // `p` include every state slot).
        for i in 0..p {
            let w = cir_type_width(&block.params[i], types);
            if w <= 1 {
                self.wires.insert(i as u32, WireRepr::Scalar(format!("w_{}", i)));
            } else {
                self.wires.insert(i as u32, WireRepr::Array(format!("w_{}", i), w));
            }
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
        self.emit_circuit_stmts_range(block, types, mode, 0..block.stmts.len());
    }

    /// As [`Self::emit_circuit_stmts`], but processes only `stmt_range`
    /// (absolute indices into `block.stmts`) instead of the whole block --
    /// Milestone 1.5 Step B: weaving one Rust function per original
    /// (pre-movfuscation) block's own `MovfuscBlockBoundary` range instead
    /// of one function for the whole combined circuit. `si` (used for
    /// `var_id = p + si`) is the *absolute* stmt index, so wires/gates
    /// emitted here use the exact same names/numbering as a full-block
    /// `emit_circuit_stmts` call would -- callers process disjoint ranges
    /// across separate `VoleIrCtx`/`IrFunction` instances that only share
    /// `w_i`-input-param wires (never stmt-defined vars across ranges,
    /// since blocks are independent given the shared entry state).
    fn emit_circuit_stmts_range(
        &mut self,
        block: &CirBlock,
        types: &CirTypes,
        mode: &StorageMode,
        stmt_range: core::ops::Range<usize>,
    ) {
        let p = block.params.len();

        // Process stmts.
        for si in stmt_range {
            let stmt = &block.stmts[si];
            let var_id = (p + si) as u32;
            let out_name = format!("w_{}", var_id);

            match &stmt.kind {
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

                Stmt::Poly { ty, coeffs, constant } => {
                    let width = cir_type_width(ty, types);
                    let repr = self.emit_poly(&out_name, coeffs, constant, width);
                    self.wires.insert(var_id, repr);
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
                            let repr = self.emit_storage_read_committed(
                                &out_name, var_id, storage.0, ty.0, addr, types, ty,
                            );
                            self.wires.insert(var_id, repr);
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
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&n),
                            ty: None,
                            init: Some(clone_expr(var(&s))),
                        }));
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
                    let prefix = if self.role.is_prover() { "vope" } else { "q" };
                    if w == 1 {
                        let param_name = format!("{}_ext_rng_{}_bit_0", prefix, r);
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&out_name),
                            ty: None,
                            init: Some(clone_expr(var(&param_name))),
                        }));
                        self.wires.insert(var_id, WireRepr::Scalar(out_name));
                    } else {
                        let bits: Vec<String> = (0..w).map(|j| {
                            let n = format!("{}_{}", out_name, j);
                            let param_name = format!("{}_ext_rng_{}_bit_{}", prefix, r, j);
                            self.stmts.push(ir_stmt(IrStmtKind::Let {
                                pattern: IrPattern::ident(&n),
                                ty: None,
                                init: Some(clone_expr(var(&param_name))),
                            }));
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
                    let prefix = if self.role.is_prover() { "vope" } else { "q" };
                    if w == 1 {
                        let param_name = format!("{}_ext_oracle_{}_bit_{}", prefix, k, base);
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&out_name),
                            ty: None,
                            init: Some(clone_expr(var(&param_name))),
                        }));
                        self.wires.insert(var_id, WireRepr::Scalar(out_name));
                    } else {
                        let bits: Vec<String> = (0..w).map(|j| {
                            let n = format!("{}_{}", out_name, j);
                            let param_name = format!("{}_ext_oracle_{}_bit_{}", prefix, k, base + j);
                            self.stmts.push(ir_stmt(IrStmtKind::Let {
                                pattern: IrPattern::ident(&n),
                                ty: None,
                                init: Some(clone_expr(var(&param_name))),
                            }));
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
                    let prefix = if self.role.is_prover() { "vope" } else { "q" };
                    if w == 1 {
                        let param_name = format!("{}_ext_action_{}_bit_{}", prefix, k, base);
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&out_name),
                            ty: None,
                            init: Some(clone_expr(var(&param_name))),
                        }));
                        self.wires.insert(var_id, WireRepr::Scalar(out_name));
                    } else {
                        let bits: Vec<String> = (0..w).map(|j| {
                            let n = format!("{}_{}", out_name, j);
                            let param_name = format!("{}_ext_action_{}_bit_{}", prefix, k, base + j);
                            self.stmts.push(ir_stmt(IrStmtKind::Let {
                                pattern: IrPattern::ident(&n),
                                ty: None,
                                init: Some(clone_expr(var(&param_name))),
                            }));
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
) -> Tagged<Zk, IrModule<IrFunction>> {
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
) -> (Tagged<Zk, IrModule<IrFunction>>, MemoryTrace) {
    assert!(circuit.is_circuit(), "weave_vole_prover_ir: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let and_count = count_ir_ands(block, types, mode);
    let num_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads(block, types)
    } else { 0 };
    let (generics, where_clause) = prover_generics_and_where();

    let mut params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            params.push(IrParam { name: format!("w_{}", i), ty: vope_type() });
        } else {
            params.push(IrParam { name: format!("w_{}", i), ty: wide_array_type(vope_type(), w) });
        }
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

    let mut ctx = VoleIrCtx::new(true);
    ctx.emit_circuit(block, types, mode, &circuit.pre_init);

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("expected Jmp(Return)"),
    };
    // Computed after emit_circuit: each slot's width (scalar vs array) is
    // only known once `ctx.wires` has been populated.
    let output_ty = if ret_args.len() == 1 {
        ctx.slot_type(&ret_args[0], &vope_type())
    } else {
        IrType::Tuple(ret_args.iter().map(|v| ctx.slot_type(v, &vope_type())).collect())
    };
    let ret_type = IrType::Tuple(vec![output_ty, hat_array_type(and_count)]);
    let output_expr = if ret_args.len() == 1 {
        ctx.slot_expr(&ret_args[0])
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| ctx.slot_expr(v)).collect()))
    };
    let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| hat_ref_expr(h)).collect()));
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![output_expr, hats_expr]));
    let trace = ctx.trace.clone();

    let func = IrFunction { no_inline: true,
        name: format!("vole_prove_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
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
    (Tagged::seal(module), trace)
}

/// Milestone 1.5 Step B: as [`weave_vole_prover_ir_with_mode`], but split
/// into one Rust function per original (pre-movfuscation) block (per
/// `boundary`, from `movfuscate_ir_with_boundary`), a *chunked* accumulator
/// chain (per `accum_info`, groups of `chunk_size` blocks -- see
/// [`weave_vole_verifier_ir_split_with_trace`]'s doc for the full
/// rationale), and a trailing finish function -- the prover-side
/// counterpart, meant to be driven interleaved with the verifier (call
/// block `i`'s prover function, then block `i`'s verifier function with
/// the prover's returned `hat`s, fold `all_ok`, discard those `hat`s, move
/// to block `i+1`, then likewise through the accumulator chunks) so
/// neither side ever holds more than one function's own witness/gate data
/// at once. No `all_ok`/`fold_state` threading here (that's verifier-only);
/// each function simply returns its own exported state plus its own
/// local `hat`s array.
///
/// Same requirements on `boundary`/`accum_info` as the verifier
/// counterpart; `chunk_size` is clamped to at least 1.
///
/// Default circuit-statement-count threshold above which a region gets
/// split into multiple smaller Rust functions -- see [`crate::vole_split`].
/// Derived from real measurement (`~/.claude/plans/tidy-exploring-duckling.md`):
/// the known ~6.3-6.4MB outlier functions average ~1,389 printed bytes
/// per circuit statement; 500 circuit statements/piece targets ~694KB
/// pieces, comfortably inside the 500KB-1MB band real `rustc
/// -Z time-passes` profiling identified as a reasonable starting point.
///
/// Threaded through each `weave_vole_*_ir_split` function as an explicit
/// `max_stmts_per_piece` parameter (not a hardcoded constant) so tests
/// can pass a small value to force real multi-piece splitting on a tiny
/// circuit -- exercising the pool-based cross-piece threading
/// (`piece_in_v`, see `WireRepr::Pooled`) without needing a
/// 500+-statement fixture. Production callers should pass this default.
pub const DEFAULT_MAX_STMTS_PER_PIECE: usize = 500;

pub fn weave_vole_prover_ir_split(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
    boundary: &[volar_ir_passes::MovfuscBlockBoundary],
    accum_info: &volar_ir_passes::MovfuscAccumInfo,
    chunk_size: usize,
    max_stmts_per_piece: usize,
    mut emit_fn: impl FnMut(IrFunction),
) -> MemoryTrace {
    assert!(circuit.is_circuit(), "weave_vole_prover_ir_split: circuit must satisfy is_circuit()");
    assert!(!boundary.is_empty(), "weave_vole_prover_ir_split: boundary must be non-empty");
    assert_eq!(accum_info.steps.len(), boundary.len(), "accum_info must come from the same movfuscate_ir_with_boundary call as boundary");
    let chunk_size = chunk_size.max(1);
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let (generics, where_clause) = prover_generics_and_where();

    let mut w_params: Vec<IrParam> = Vec::new();
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            w_params.push(IrParam { name: format!("w_{}", i), ty: vope_type() });
        } else {
            w_params.push(IrParam { name: format!("w_{}", i), ty: wide_array_type(vope_type(), w) });
        }
    }
    let insert_w_wires = |ctx: &mut VoleIrCtx| {
        for i in 0..num_params {
            let w = cir_type_width(&block.params[i], types);
            if w <= 1 {
                // Phase B: scalar top-level params are pooled via
                // `_w_pool`, keyed by raw param index (0..num_params,
                // disjoint from `_synth_pool`'s own var-id space -- see
                // `_w_pool`'s own declaration site for why it needs a
                // separate, loop-persistent pool rather than reusing
                // `_synth_pool`). No more per-value `w_i` named param for
                // these; `used_w`/`piece_used_w` no longer gate them
                // either (harmless to keep computing -- see the matching
                // comment at every `w_params` filter site).
                ctx.wires.insert(i as u32, WireRepr::Pooled("_w_pool", i));
            } else {
                // Lazy `WireRepr::Array`, not eagerly unpacked -- see its
                // own doc. With top-level parameter threading, `num_params`
                // covers every state slot, and this closure runs once per
                // (block/chunk/finish) split-weave function per role
                // (~241 x 3): eagerly unpacking every wide param into `w`
                // bound locals here, regardless of whether a given
                // function ever references them, is exactly what drove
                // `print_weaved_vole_module` to 13GB+ RSS printing the
                // real interpreter's full split-weave module.
                ctx.wires.insert(i as u32, WireRepr::Array(format!("w_{}", i), w));
            }
        }
    };

    // Pre-init trace entries: same pure-metadata replication as the
    // verifier split (see its own doc for why no wire emission is needed).
    let mut overall_trace_entries: Vec<MemoryTraceEntry> = Vec::new();
    let mut global_ts: u32 = 0;
    if matches!(mode, StorageMode::Commitment) && !circuit.pre_init.is_empty() {
        let n_stmts = block.stmts.len() as u32;
        let mut syn_id = num_params as u32 + n_stmts;
        for seg in &circuit.pre_init {
            let sid = seg.storage.0;
            let tid = seg.ty.0;
            for (local, _c) in seg.data.iter().enumerate() {
                let _ci = seg.offset + local;
                let val_id = syn_id; syn_id += 1;
                let addr_id = syn_id; syn_id += 1;
                overall_trace_entries.push(MemoryTraceEntry {
                    addr_var: addr_id,
                    value_var: val_id,
                    storage_id: sid,
                    type_id: tid,
                    is_write: true,
                    timestamp: global_ts,
                });
                global_ts += 1;
            }
        }
    }

    let shared_prefix: core::ops::Range<usize> = 0..(boundary[0].start as usize - num_params);
    let mut interfaces: Vec<SplitBlockInterface> = Vec::with_capacity(boundary.len());

    // Cross-chunk-shared values (CSE-discovered post-movfuscation sharing;
    // see docs/interpreter-honest-e2e-zk-plan.md's "Cross-chunk locality"
    // section) thread as packed parameters between exactly the functions
    // that need them -- keyed by the var's own STABLE, globally-unique id
    // (so the same name works as both a producer's own output and a
    // consumer's own input, no separate naming scheme needed). Populated
    // incrementally in real call order (every boundary[i] in order, then
    // accum_info.init, then every steps[i] in order -- the same order
    // this function already processes them in): a var's own producer
    // range (the one where `synthetic_out` contains it but
    // `synthetic_in` does not) always runs before any consumer, so its
    // type is always already known by the time a later range needs to
    // bind it as an incoming param.
    let mut synthetic_types: alloc::collections::BTreeMap<u32, IrType> = alloc::collections::BTreeMap::new();

    for (i, b) in boundary.iter().enumerate() {
        let start = (b.start - num_params as u32) as usize;
        let end = (b.end - num_params as u32) as usize;
        let local_stmts = &block.stmts[start..end];
        let local_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(local_stmts, types)
        } else { 0 };
        let local_ext = count_external_primitives_range(local_stmts, types);

        // Only declare the `w_i` this specific block function actually
        // references, instead of unconditionally all `num_params` -- see
        // `collect_used_top_level_params`'s own doc. Output ids
        // (`is_active`/`done`/`next_pc`/`next_state`/`ret_vals`) must be
        // unioned in explicitly: a tunnelled-slot pass-through can return
        // a top-level param directly without it ever appearing as an
        // operand inside this block's own statement range.
        let mut used_w: alloc::collections::BTreeSet<u32> = collect_used_top_level_params(&block.stmts[shared_prefix.clone()], num_params);
        used_w.extend(collect_used_top_level_params(local_stmts, num_params));
        for &v in core::iter::once(&b.is_active).chain(core::iter::once(&b.done))
            .chain(b.next_pc_bits.iter()).chain(b.next_state.iter()).chain(b.ret_vals.iter())
        {
            if (v as usize) < num_params { used_w.insert(v); }
        }

        // ---- Phase 1 intra-region function splitting ----------------------
        //
        // Real `rustc -Z time-passes` profiling found MIR_borrow_checking
        // (a per-function-item rustc pass) dominates compile cost for the
        // largest woven functions (see `~/.claude/plans/tidy-exploring-duckling.md`).
        // When a region's own statement count exceeds `MAX_STMTS_PER_PIECE`,
        // split it into several small "piece" functions plus one thin
        // "wrapper" that keeps this region's exact original name/params/
        // return-tuple shape and calls the pieces in sequence -- invisible
        // to `thread_synthetic_slots`/`split_driver.rs`/every other
        // external consumer (see `crate::vole_split`'s own module doc for
        // the full algorithm and rationale).
        //
        // Guarded to skip regions with any external-primitive (oracle/
        // action/rng) call -- those need their own per-piece encounter-
        // order repartitioning too (mirroring the `oracle_rd_N`
        // repartitioning below), not yet implemented. None of the
        // currently-known oversized regions have any (confirmed via
        // direct measurement: the ~6.3-6.4MB outliers are storage-heavy,
        // not oracle/action/rng-heavy), so this costs nothing today and
        // just fails safe (falls back to one function, identical to
        // pre-splitting behavior) if a future region needs both.
        let can_split = local_ext.oracle_calls.is_empty() && local_ext.action_calls.is_empty() && local_ext.rng_widths.is_empty();
        let mut region_outputs: Vec<u32> = alloc::vec![b.is_active, b.done];
        region_outputs.extend(b.next_pc_bits.iter().copied());
        region_outputs.extend(b.next_state.iter().copied());
        region_outputs.extend(b.ret_vals.iter().copied());
        region_outputs.extend(b.synthetic_out.iter().copied());
        let pieces = if can_split {
            crate::vole_split::split_region_into_pieces(block, num_params, b.start, b.end, &region_outputs, max_stmts_per_piece)
        } else {
            alloc::vec![crate::vole_split::PieceSpec { start: b.start, end: b.end, extra_in: Vec::new(), extra_out: Vec::new() }]
        };

        if pieces.len() <= 1 {
            // ---- Unchanged: single function, exactly as before this session's addition ----
            let mut params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
            params.extend(w_params.iter().enumerate().filter(|(idx, p)| used_w.contains(&(*idx as u32)) && matches!(p.ty, IrType::Array { .. })).map(|(_, p)| p.clone()));
            for j in 0..local_oracle_reads {
                params.push(IrParam { name: format!("oracle_rd_{}", j), ty: vope_type() });
            }
            for (k, call) in local_ext.oracle_calls.iter().enumerate() {
                for j in 0..call.total_bits {
                    params.push(IrParam { name: format!("vope_ext_oracle_{}_bit_{}", k, j), ty: vope_type() });
                }
            }
            for (k, call) in local_ext.action_calls.iter().enumerate() {
                for j in 0..call.total_bits {
                    params.push(IrParam { name: format!("vope_ext_action_{}_bit_{}", k, j), ty: vope_type() });
                }
            }
            for (r, &width) in local_ext.rng_widths.iter().enumerate() {
                for j in 0..width {
                    params.push(IrParam { name: format!("vope_ext_rng_{}_bit_{}", r, j), ty: vope_type() });
                }
            }

            // Unconditional (not just when synthetic_in/out are non-empty):
            // block-boundary exports (is_active/done/next_pc/next_state/
            // ret_vals) are ALSO pooled now, and every block has at least
            // an is_active+done pair, so this is needed almost always in
            // practice anyway -- an unused pool param on the rare block
            // that somehow needs none of this is harmless (the generated
            // preamble already has #![allow(unused_variables, ...)]).
            let synth_pool_needed = true;
            if synth_pool_needed {
                params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(vope_type(), true) });
                params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                // Phase B: unconditional, same reasoning as `_synth_pool`.
                params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(vope_type(), true) });
                params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            }

            let mut ctx = VoleIrCtx::new(true);
            insert_w_wires(&mut ctx);
            for &v in &b.synthetic_in {
                let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                    "weave_vole_prover_ir_split: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                ));
                if !matches!(ty, IrType::Array { .. }) {
                    ctx.wires.insert(v, WireRepr::Pooled("_synth_pool", v as usize));
                    continue;
                }
                bind_scalar(&mut ctx, &mut params, v, format!("synth_{v}"), ty);
            }
            debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);
            ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
            ctx.emit_circuit_stmts_range(block, types, mode, start..end);

            let local_entry_count = ctx.trace.entries.len() as u32;
            for mut e in ctx.trace.entries.clone() {
                e.timestamp += global_ts;
                overall_trace_entries.push(e);
            }
            global_ts += local_entry_count.max(ctx.mem_timestamp);

            let is_active_v = CirVar(b.is_active);
            let done_v = CirVar(b.done);
            let is_active_expr = ctx.slot_expr(&is_active_v);
            let done_expr = ctx.slot_expr(&done_v);
            let is_active_ty = ctx.slot_type(&is_active_v, &vope_type());
            let done_ty = ctx.slot_type(&done_v, &vope_type());
            let next_pc_exprs: Vec<IrExpr> = b.next_pc_bits.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let next_pc_bit_tys: Vec<IrType> = b.next_pc_bits.iter().map(|&v| ctx.slot_type(&CirVar(v), &vope_type())).collect();
            let next_state_exprs: Vec<IrExpr> = b.next_state.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let next_state_tys: Vec<IrType> = b.next_state.iter().map(|&v| ctx.slot_type(&CirVar(v), &vope_type())).collect();
            let ret_val_exprs: Vec<IrExpr> = b.ret_vals.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let ret_val_tys: Vec<IrType> = b.ret_vals.iter().map(|&v| ctx.slot_type(&CirVar(v), &vope_type())).collect();

            let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| hat_ref_expr(h)).collect()));
            let hats_ty = hat_array_type(ctx.hat_names.len());

            // Block-boundary exports (is_active/done/next_pc/next_state/
            // ret_vals): pool scalar ones directly via `_synth_pool` (the
            // same shared pool `synthetic_out` below already uses -- see
            // `export_scalar_or_tuple`'s own doc), keep wide ones on the
            // return tuple exactly as before. `is_active_ty`/`done_ty`/
            // `next_pc_bit_tys`/`next_state_tys`/`ret_val_tys` are still
            // computed and stored on `SplitBlockInterface` below either
            // way -- the covering chunk's own read side needs the type
            // regardless of whether this value ends up pooled or tuple-
            // threaded, to make the identical pooling decision.
            let mut ret_tuple_tys: Vec<IrType> = Vec::new();
            let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
            export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.is_active, is_active_ty.clone(), is_active_expr);
            export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.done, done_ty.clone(), done_expr);
            for (j, expr) in next_pc_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_pc_bits[j], next_pc_bit_tys[j].clone(), expr);
            }
            for (k, expr) in next_state_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_state[k], next_state_tys[k].clone(), expr);
            }
            for (m, expr) in ret_val_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.ret_vals[m], ret_val_tys[m].clone(), expr);
            }
            ret_tuple_tys.push(hats_ty);
            ret_tuple_exprs.push(hats_expr);

            // Cross-chunk-shared values this range genuinely produces or
            // re-exports (pass-through) -- always appended last, after the
            // native movfuscation fields, in `b.synthetic_out`'s own
            // (ascending-var-id) order. `slot_type`/`slot_expr` resolve
            // correctly either way: for the genuine producer, `v`'s own
            // defining statement was just emitted for real above; for an
            // intervening/consuming range, `v` was already bound to its own
            // `synth_{v}` incoming param before emission (see the
            // `synthetic_in` binding above), so this is a pure pass-through.
            for &v in &b.synthetic_out {
                let ty = ctx.slot_type(&CirVar(v), &vope_type());
                synthetic_types.entry(v).or_insert_with(|| ty.clone());
                if !matches!(ty, IrType::Array { .. }) {
                    let slot_str = v.to_string();
                    let value = ctx.slot_expr(&CirVar(v));
                    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool", &slot_str)),
                        right: Box::new(value),
                    }))));
                    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                        right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                    }))));
                    continue;
                }
                ret_tuple_tys.push(ty);
                ret_tuple_exprs.push(ctx.slot_expr(&CirVar(v)));
            }

            let func = IrFunction { no_inline: true,
                name: format!("vole_prove_ir_{}_block_{}", name, i),
                module_path: vec![],
                generics: generics.clone(),
                receiver: None,
                params,
                return_type: Some(IrType::Tuple(ret_tuple_tys)),
                where_clause: where_clause.clone(),
                body: IrBlock {
                    stmts: ctx.finalize_and_take_stmts(),
                    expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
                },
                external_kind: ExternalKind::Normal,
            };
            emit_fn(func);

            interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
        } else {
            // ---- Split: K piece functions + 1 wrapper with the region's
            // original name/params/return-tuple shape. See
            // `crate::vole_split`'s own doc for the threading algorithm;
            // this block does the actual IR emission it plans for.
            debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);

            // Every entry of `region_outputs` lands in exactly one piece's
            // own `extra_out` (see `split_region_into_pieces`'s own doc) --
            // this map lets both the return-tuple assembly below and
            // `extra_in` resolution find which piece produced any given var.
            let mut producer_piece: alloc::collections::BTreeMap<u32, usize> = alloc::collections::BTreeMap::new();
            for (p, piece) in pieces.iter().enumerate() {
                for &v in &piece.extra_out {
                    producer_piece.entry(v).or_insert(p);
                }
            }

            // ---- Phase C sub-stage 1: pool scalar cross-piece
            // ("piece_in_v") values through a shared `_piece_pool` slice
            // instead of threading each individually through its own
            // named param + wrapper-local `piece{p}_v{v}` binding.
            // `all_extra_in_vars` is every var ANY piece needs as an
            // input from an earlier piece; `pool_slot` is filled in
            // progressively below as each var's producing piece runs and
            // its type becomes known (only SCALAR types get pooled --
            // wide ones keep the old per-value param mechanism, since
            // sizing/addressing a pool of arrays is out of this sub-
            // stage's scope). `region_has_cross_piece_vars` is decided
            // once, up front: a piece's own param list is finalized
            // before later pieces (which might be the ones needing the
            // pool) have even run, so "does THIS piece need the pool
            // params" can't be decided piece-by-piece as we go.
            let all_extra_in_vars: alloc::collections::BTreeSet<u32> =
                pieces.iter().flat_map(|pc| pc.extra_in.iter().copied()).collect();
            let mut pool_slot: alloc::collections::BTreeMap<u32, usize> = alloc::collections::BTreeMap::new();
            let region_has_cross_piece_vars = pieces.iter().any(|pc| !pc.extra_in.is_empty() || !pc.extra_out.is_empty());

            let mut piece_names: Vec<String> = Vec::with_capacity(pieces.len());
            let mut piece_used_w: Vec<alloc::collections::BTreeSet<u32>> = Vec::with_capacity(pieces.len());
            let mut piece_oracle_counts: Vec<usize> = Vec::with_capacity(pieces.len());
            let mut piece_hat_counts: Vec<usize> = Vec::with_capacity(pieces.len());
            // Types of every var any piece has returned so far -- needed
            // both by later pieces' own `extra_in` binding and by the
            // wrapper's final return-tuple type assembly.
            let mut piece_out_types: alloc::collections::BTreeMap<u32, IrType> = alloc::collections::BTreeMap::new();

            for (p, piece) in pieces.iter().enumerate() {
                let p_start = (piece.start - num_params as u32) as usize;
                let p_end = (piece.end - num_params as u32) as usize;
                let p_stmts = &block.stmts[p_start..p_end];
                let p_oracle_reads = if matches!(mode, StorageMode::Commitment) {
                    count_storage_reads_range(p_stmts, types)
                } else { 0 };

                let mut p_used_w: alloc::collections::BTreeSet<u32> = collect_used_top_level_params(&block.stmts[shared_prefix.clone()], num_params);
                p_used_w.extend(collect_used_top_level_params(p_stmts, num_params));
                for &v in &piece.extra_out {
                    if (v as usize) < num_params { p_used_w.insert(v); }
                }

                let mut p_params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
                p_params.extend(w_params.iter().enumerate().filter(|(idx, pr)| p_used_w.contains(&(*idx as u32)) && matches!(pr.ty, IrType::Array { .. })).map(|(_, pr)| pr.clone()));
                for j in 0..p_oracle_reads {
                    p_params.push(IrParam { name: format!("oracle_rd_{}", j), ty: vope_type() });
                }
                if region_has_cross_piece_vars {
                    p_params.push(IrParam { name: "_piece_pool".into(), ty: pool_slice_type(vope_type(), true) });
                    p_params.push(IrParam { name: "_piece_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                }
                // Phase B: unconditional, same reasoning as every other
                // `_w_pool` declaration site -- a piece may reference any
                // scalar top-level param directly.
                p_params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(vope_type(), true) });
                p_params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });

                let mut ctx = VoleIrCtx::new(true);
                insert_w_wires(&mut ctx);
                for &v in &b.synthetic_in {
                    let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                        "weave_vole_prover_ir_split: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                    ));
                    bind_scalar(&mut ctx, &mut p_params, v, format!("synth_{v}"), ty);
                }
                for &v in &piece.extra_in {
                    if let Some(&slot) = pool_slot.get(&v) {
                        ctx.wires.insert(v, WireRepr::Pooled("_piece_pool", slot));
                        continue;
                    }
                    let ty = piece_out_types.get(&v).cloned().unwrap_or_else(|| panic!(
                        "weave_vole_prover_ir_split: piece-local var {v} has no known type -- its own producing piece must run before this consuming piece"
                    ));
                    bind_scalar(&mut ctx, &mut p_params, v, format!("piece_in_{v}"), ty);
                }

                // Every piece independently re-derives shared_prefix --
                // safe (zero extra trace entries / zero extra hats): the
                // pre-existing `local_oracle_reads` computation above
                // never includes shared_prefix's own range, which only
                // works because shared_prefix is already provably
                // storage-free, and the `debug_assert_eq!` above confirms
                // it's also AND-gate-free (no hats). Re-deriving it K
                // times per split region just mirrors the pattern every
                // OTHER function in this module already relies on
                // (independently re-deriving shared_prefix once per
                // region), now applied once per piece instead.
                ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
                ctx.emit_circuit_stmts_range(block, types, mode, p_start..p_end);

                let local_entry_count = ctx.trace.entries.len() as u32;
                for mut e in ctx.trace.entries.clone() {
                    e.timestamp += global_ts;
                    overall_trace_entries.push(e);
                }
                global_ts += local_entry_count.max(ctx.mem_timestamp);

                let mut p_ret_tys: Vec<IrType> = Vec::with_capacity(piece.extra_out.len() + 1);
                let mut p_ret_exprs: Vec<IrExpr> = Vec::with_capacity(piece.extra_out.len() + 1);
                for &v in &piece.extra_out {
                    let ty = ctx.slot_type(&CirVar(v), &vope_type());
                    let is_scalar = !matches!(ty, IrType::Array { .. });
                    if is_scalar && all_extra_in_vars.contains(&v) {
                        if pool_slot.contains_key(&v) {
                            // The first producer owns the pool slot; later
                            // pass-through pieces reuse its mapping.
                            piece_out_types.insert(v, ty);
                            continue;
                        }
                        let slot = pool_slot.len();
                        pool_slot.insert(v, slot);
                        let slot_str = slot.to_string();
                        let value = ctx.slot_expr(&CirVar(v));
                        ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                            left: Box::new(arr_index("_piece_pool", &slot_str)),
                            right: Box::new(value),
                        }))));
                        ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                            left: Box::new(arr_index("_piece_pool_written", &slot_str)),
                            right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                        }))));
                        piece_out_types.insert(v, ty);
                        continue;
                    }
                    p_ret_exprs.push(ctx.slot_expr(&CirVar(v)));
                    piece_out_types.insert(v, ty.clone());
                    p_ret_tys.push(ty);
                }
                let p_hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| hat_ref_expr(h)).collect()));
                let p_hats_ty = hat_array_type(ctx.hat_names.len());
                piece_hat_counts.push(ctx.hat_names.len());
                p_ret_tys.push(p_hats_ty);
                p_ret_exprs.push(p_hats_expr);

                let p_name = format!("vole_prove_ir_{}_block_{}_piece_{}", name, i, p);
                let p_func = IrFunction { no_inline: true,
                    name: p_name.clone(),
                    module_path: vec![],
                    generics: generics.clone(),
                    receiver: None,
                    params: p_params,
                    return_type: Some(IrType::Tuple(p_ret_tys)),
                    where_clause: where_clause.clone(),
                    body: IrBlock {
                        stmts: ctx.finalize_and_take_stmts(),
                        expr: Some(Box::new(ir_expr(IrExprKind::Tuple(p_ret_exprs)))),
                    },
                    external_kind: ExternalKind::Normal,
                };
                emit_fn(p_func);

                piece_names.push(p_name);
                piece_used_w.push(p_used_w);
                piece_oracle_counts.push(p_oracle_reads);
            }

            // ---- Wrapper: the region's ORIGINAL external interface
            // (same name/params/return-tuple shape split_driver.rs and
            // `thread_synthetic_slots` already expect), built purely from
            // sequential calls + tuple destructuring -- no `VoleIrCtx` of
            // its own, cheap to borrowck by construction.
            let mut wrapper_params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
            wrapper_params.extend(w_params.iter().enumerate().filter(|(idx, p)| used_w.contains(&(*idx as u32)) && matches!(p.ty, IrType::Array { .. })).map(|(_, p)| p.clone()));
            for j in 0..local_oracle_reads {
                wrapper_params.push(IrParam { name: format!("oracle_rd_{}", j), ty: vope_type() });
            }
            // `can_split` already guarantees `local_ext` is empty here.
            // Unconditional (not just when synthetic_in/out are non-empty):
            // block-boundary exports (is_active/done/next_pc/next_state/
            // ret_vals) are ALSO pooled now, and every block has at least
            // an is_active+done pair, so this is needed almost always in
            // practice anyway -- an unused pool param on the rare block
            // that somehow needs none of this is harmless (the generated
            // preamble already has #![allow(unused_variables, ...)]).
            let synth_pool_needed = true;
            if synth_pool_needed {
                wrapper_params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(vope_type(), true) });
                wrapper_params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                // Phase B: unconditional, same reasoning as `_synth_pool`.
                wrapper_params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(vope_type(), true) });
                wrapper_params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            }
            for &v in &b.synthetic_in {
                let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                    "weave_vole_prover_ir_split: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                ));
                if matches!(ty, IrType::Array { .. }) {
                    wrapper_params.push(IrParam { name: format!("synth_{v}"), ty });
                }
            }

            let mut wrapper_stmts: Vec<IrStmt> = Vec::with_capacity(pieces.len() + 2);
            if region_has_cross_piece_vars {
                wrapper_stmts.push(pool_decl_stmt("_piece_pool", vope_type(), vope_default_call(), pool_slot.len()));
                wrapper_stmts.push(pool_decl_stmt("_piece_pool_written", IrType::Primitive(PrimitiveType::Bool), ir_expr(IrExprKind::Lit(IrLit::Bool(false))), pool_slot.len()));
            }
            let mut oracle_offset = 0usize;
            for (p, piece) in pieces.iter().enumerate() {
                // Every arg is `.clone()`d -- wrapper-local bindings
                // (`w_i`/`synth_{v}`/`piece{p}_v{v}`) are non-`Copy`
                // (`Vope<N,T,U1>`) and may be needed by more than one
                // piece call and/or the final return tuple, mirroring
                // `slot_expr`'s own always-clone convention (used
                // throughout this file for ctx-tracked wires) and
                // `split_driver.rs`'s own established `.clone()`-per-arg
                // pattern for its text-templated calls.
                let mut call_args: Vec<IrExpr> = vec![clone_expr(var("vope_one"))];
                // Phase B: only WIDE `w_i` are still real named args here --
                // a scalar one is pooled (see `insert_w_wires`'s own
                // doc), so the piece reads it via `_w_pool` (passed
                // unconditionally below), not as a call arg. `w_params`
                // (built once, indexed by original param id) is the same
                // source of truth `p_params`'s own filter used.
                for &idx in &piece_used_w[p] {
                    if matches!(w_params[idx as usize].ty, IrType::Array { .. }) {
                        call_args.push(clone_expr(var(&format!("w_{idx}"))));
                    }
                }
                for j in 0..piece_oracle_counts[p] {
                    call_args.push(clone_expr(var(&format!("oracle_rd_{}", oracle_offset + j))));
                }
                oracle_offset += piece_oracle_counts[p];
                if region_has_cross_piece_vars {
                    call_args.push(slice_ref_mut_expr("_piece_pool"));
                    call_args.push(slice_ref_mut_expr("_piece_pool_written"));
                }
                // Phase B: unconditional, matching every piece's own
                // unconditional `_w_pool`/`_w_pool_written` params.
                call_args.push(slice_ref_mut_expr("_w_pool"));
                call_args.push(slice_ref_mut_expr("_w_pool_written"));
                for &v in &b.synthetic_in {
                    let ty = synthetic_types.get(&v).cloned().expect("synthetic_types must already be populated");
                    if matches!(ty, IrType::Array { .. }) {
                        call_args.push(clone_expr(var(&format!("synth_{v}"))));
                    } else {
                        call_args.push(pooled_read_expr("_synth_pool", v as usize));
                    }
                }
                for &v in &piece.extra_in {
                    // Pooled vars need no per-value call arg at all --
                    // the consuming piece reads them straight out of the
                    // shared `_piece_pool` (passed once, above), not from
                    // a wrapper-local `piece{producer}_v{v}` binding.
                    if pool_slot.contains_key(&v) { continue; }
                    let producer = producer_piece.get(&v).copied().expect("weave_vole_prover_ir_split: extra_in var must have a producer piece");
                    call_args.push(clone_expr(var(&format!("piece{producer}_v{v}"))));
                }

                // Pooled `extra_out` vars are written directly into
                // `_piece_pool` by the piece itself (see the write-
                // statement emission above) -- they never come back
                // through this return tuple, so they're excluded here.
                let mut pattern_names: Vec<String> = piece.extra_out.iter()
                    .filter(|v| !pool_slot.contains_key(v))
                    .map(|&v| format!("piece{p}_v{v}"))
                    .collect();
                pattern_names.push(format!("piece{p}_hats"));

                wrapper_stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::Tuple(pattern_names.into_iter().map(IrPattern::ident).collect()),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::Call {
                        func: Box::new(ir_expr(IrExprKind::Path {
                            segments: vec![piece_names[p].clone()],
                            type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
                        })),
                        args: call_args,
                    })),
                }));
            }

            let field_expr = |v: u32| match pool_slot.get(&v) {
                Some(&slot) => pooled_read_expr("_piece_pool", slot),
                None => clone_expr(var(&format!("piece{}_v{}", producer_piece[&v], v))),
            };
            let field_ty = |v: u32| piece_out_types[&v].clone();

            let is_active_ty = field_ty(b.is_active);
            let done_ty = field_ty(b.done);
            let is_active_expr = field_expr(b.is_active);
            let done_expr = field_expr(b.done);
            let next_pc_exprs: Vec<IrExpr> = b.next_pc_bits.iter().map(|&v| field_expr(v)).collect();
            let next_pc_bit_tys: Vec<IrType> = b.next_pc_bits.iter().map(|&v| field_ty(v)).collect();
            let next_state_exprs: Vec<IrExpr> = b.next_state.iter().map(|&v| field_expr(v)).collect();
            let next_state_tys: Vec<IrType> = b.next_state.iter().map(|&v| field_ty(v)).collect();
            let ret_val_exprs: Vec<IrExpr> = b.ret_vals.iter().map(|&v| field_expr(v)).collect();
            let ret_val_tys: Vec<IrType> = b.ret_vals.iter().map(|&v| field_ty(v)).collect();

            // Combined hats array: every piece's own hats, concatenated in
            // piece order via direct indexing into each piece's own
            // destructured `piece{p}_hats` array -- exactly reconstructs
            // what one un-split emission would have produced, since
            // shared_prefix contributes zero hats (confirmed above) and
            // hat_names are accumulated in statement-processing order,
            // preserved by processing pieces contiguously/sequentially.
            let mut combined_hats_exprs: Vec<IrExpr> = Vec::new();
            for p in 0..pieces.len() {
                for j in 0..piece_hat_counts[p] {
                    combined_hats_exprs.push(clone_expr(arr_index(&format!("piece{p}_hats"), &j.to_string())));
                }
            }
            let hats_ty = hat_array_type(combined_hats_exprs.len());
            let hats_expr = ir_expr(IrExprKind::FixedArray(combined_hats_exprs));

            // Block-boundary exports: pool scalar ones directly via
            // `_synth_pool` -- see `export_scalar_or_tuple`'s own doc and
            // the matching comment at the unsplit-region site above.
            let mut ret_tuple_tys: Vec<IrType> = Vec::new();
            let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
            export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.is_active, is_active_ty.clone(), is_active_expr);
            export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.done, done_ty.clone(), done_expr);
            for (j, expr) in next_pc_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_pc_bits[j], next_pc_bit_tys[j].clone(), expr);
            }
            for (k, expr) in next_state_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_state[k], next_state_tys[k].clone(), expr);
            }
            for (m, expr) in ret_val_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.ret_vals[m], ret_val_tys[m].clone(), expr);
            }
            ret_tuple_tys.push(hats_ty);
            ret_tuple_exprs.push(hats_expr);

            for &v in &b.synthetic_out {
                let ty = field_ty(v);
                synthetic_types.entry(v).or_insert_with(|| ty.clone());
                if !matches!(ty, IrType::Array { .. }) {
                    let slot_str = v.to_string();
                    wrapper_stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool", &slot_str)),
                        right: Box::new(field_expr(v)),
                    }))));
                    wrapper_stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                        right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                    }))));
                    continue;
                }
                ret_tuple_tys.push(ty);
                ret_tuple_exprs.push(field_expr(v));
            }

            let wrapper_func = IrFunction { no_inline: true,
                name: format!("vole_prove_ir_{}_block_{}", name, i),
                module_path: vec![],
                generics: generics.clone(),
                receiver: None,
                params: wrapper_params,
                return_type: Some(IrType::Tuple(ret_tuple_tys)),
                where_clause: where_clause.clone(),
                body: IrBlock {
                    stmts: wrapper_stmts,
                    expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
                },
                external_kind: ExternalKind::Normal,
            };
            emit_fn(wrapper_func);

            interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
        }
    }

    // ---- Chunked accumulation: fold blocks in groups, never all at once ----
    //
    // Mirrors weave_vole_verifier_ir_split_with_trace's own chunking exactly
    // (see its doc/comments for the full rationale) minus all_ok/fold_state
    // threading (verifier-only) and using vope_type()/hats instead of
    // q_type()/q_and/hat/r_and: this function's own accumulation-phase AND
    // gates (e.g. `g = is_active AND done`, mux selects) still need real
    // witness (`hat`) generation, so each chunk produces its own local hats
    // just like each block function does.
    fn bind_scalar(ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, var_id: u32, base_name: String, ty: IrType) {
        match &ty {
            IrType::Array { elem, len: volar_compiler::ir::ArrayLength::Const(n), .. } => {
                // Array-batched on the *param* side (one array param, not
                // `n` scalar params — same 65535-arg-limit reason as
                // `hat`/`q_and`/`r_and`/`w_i`). Kept as a lazy
                // `WireRepr::Array` rather than eagerly unpacked into `n`
                // bound locals -- see `WireRepr::Array`'s own doc; any
                // consumer needing real names (`Merge`, address
                // composition, the unrolled `Poly` fallback) materializes
                // on demand.
                params.push(IrParam { name: base_name.clone(), ty: wide_array_type((**elem).clone(), *n) });
                ctx.wires.insert(var_id, WireRepr::Array(base_name, *n));
            }
            _ => {
                params.push(IrParam { name: base_name.clone(), ty });
                ctx.wires.insert(var_id, WireRepr::Scalar(base_name));
            }
        }
    }

    /// As [`bind_scalar`], but for a value this function may instead read
    /// straight from `_synth_pool` (a block-boundary export or running-
    /// accumulator input, keyed by its own raw circuit var id, exactly
    /// like `synthetic_in` -- see `export_scalar_or_tuple`'s own doc for
    /// why one shared pool safely covers every category). Pooling here
    /// must agree with whatever decision the *producer* made for this
    /// same var id -- both sides derive it identically, from the var's
    /// own type alone, so they can never disagree.
    fn bind_scalar_or_pool(ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, var_id: u32, base_name: String, ty: IrType) {
        if !matches!(ty, IrType::Array { .. }) {
            ctx.wires.insert(var_id, WireRepr::Pooled("_synth_pool", var_id as usize));
            return;
        }
        bind_scalar(ctx, params, var_id, base_name, ty);
    }

    let (init_next_state_tys, init_ret_val_tys) = {
        let mut probe_ctx = VoleIrCtx::new(true);
        // The circuit's own top-level params (`w_i`) must be bound here too
        // -- `accum_info.init`'s own statement range can (and, once the
        // tunnelled-slot elimination lands in `movfuscate.rs`, does)
        // reference them directly, matching the fact that this whole
        // circuit is a looped, return-to-parameter construction: a slot's
        // own "no block touched it" default *is* its own incoming param,
        // not a free-floating zero. Omitting this line previously produced
        // "no entry found for key" once such a reference was woven.
        insert_w_wires(&mut probe_ctx);
        // shared_prefix must be emitted here too -- unconstrained CSE can
        // (and does, in practice: a Bit-typed zero seed statement is a
        // prime dedup target) merge one of accum_info.init's own seed
        // statements onto shared_prefix's own statement, so a reference
        // into accum_info.init's own next_state/ret_vals can legitimately
        // point at a shared_prefix var -- every other ctx in this
        // function already emits shared_prefix before querying anything;
        // this probe was the one place that didn't.
        probe_ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
        let init_start = (accum_info.init.start - num_params as u32) as usize;
        let init_end = (accum_info.init.end - num_params as u32) as usize;
        probe_ctx.emit_circuit_stmts_range(block, types, mode, init_start..init_end);
        let next_state_tys: Vec<IrType> = accum_info.init.next_state.iter()
            .map(|&v| probe_ctx.slot_type(&CirVar(v), &vope_type())).collect();
        let ret_val_tys: Vec<IrType> = accum_info.init.ret_vals.iter()
            .map(|&v| probe_ctx.slot_type(&CirVar(v), &vope_type())).collect();
        (next_state_tys, ret_val_tys)
    };

    let bind_running = |ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, prefix: &str,
                         done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| {
        bind_scalar_or_pool(ctx, params, done_acc, format!("{prefix}_done_acc"), vope_type());
        for (j, &v) in next_pc.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_next_pc_{j}"), vope_type());
        }
        for (k, &v) in next_state.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_next_state_{k}"), init_next_state_tys[k].clone());
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_ret_val_{m}"), init_ret_val_tys[m].clone());
        }
    };
    // Running-accumulator OUTGOING state (this chunk's own new done_acc/
    // next_pc/next_state/ret_vals): pool scalar ones directly via
    // `_synth_pool`, exactly like block-boundary exports -- see
    // `export_scalar_or_tuple`'s own doc. Replaces the old unconditional
    // running_tys/running_exprs pair (which always pushed every value
    // onto the return tuple) since the pooling decision needs
    // type+expr+var_id together, not types and exprs built independently.
    let running_export = |ctx: &mut VoleIrCtx, ret_tuple_tys: &mut Vec<IrType>, ret_tuple_exprs: &mut Vec<IrExpr>,
                           done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| {
        let done_acc_expr = ctx.slot_expr(&CirVar(done_acc));
        export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, done_acc, vope_type(), done_acc_expr);
        for &v in next_pc {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, vope_type(), expr);
        }
        for (k, &v) in next_state.iter().enumerate() {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, init_next_state_tys[k].clone(), expr);
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, init_ret_val_tys[m].clone(), expr);
        }
    };

    let n_blocks = boundary.len();
    let mut running_done_acc = accum_info.init.done_acc;
    let mut running_next_pc = accum_info.init.next_pc.clone();
    let mut running_next_state = accum_info.init.next_state.clone();
    let mut running_ret_vals = accum_info.init.ret_vals.clone();

    let mut lo = 0usize;
    let mut chunk_idx = 0usize;
    while lo < n_blocks {
        let hi = (lo + chunk_size).min(n_blocks);
        let chunk_start = (accum_info.steps[lo].start - num_params as u32) as usize;
        let chunk_end = (accum_info.steps[hi - 1].end - num_params as u32) as usize;
        let chunk_stmts = &block.stmts[chunk_start..chunk_end];
        let chunk_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(chunk_stmts, types)
        } else { 0 };
        let chunk_ext = count_external_primitives_range(chunk_stmts, types);

        let mut params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
        params.extend(w_params.iter().filter(|p| matches!(p.ty, IrType::Array { .. })).cloned());
        for j in 0..chunk_oracle_reads {
            params.push(IrParam { name: format!("oracle_rd_{}", j), ty: vope_type() });
        }
        for (k, call) in chunk_ext.oracle_calls.iter().enumerate() {
            for j in 0..call.total_bits { params.push(IrParam { name: format!("vope_ext_oracle_{}_bit_{}", k, j), ty: vope_type() }); }
        }
        for (k, call) in chunk_ext.action_calls.iter().enumerate() {
            for j in 0..call.total_bits { params.push(IrParam { name: format!("vope_ext_action_{}_bit_{}", k, j), ty: vope_type() }); }
        }
        for (r, &width) in chunk_ext.rng_widths.iter().enumerate() {
            for j in 0..width { params.push(IrParam { name: format!("vope_ext_rng_{}_bit_{}", r, j), ty: vope_type() }); }
        }

        // Unconditional -- see the matching comment at the block-level
        // synth_pool_needed site: block-boundary exports are pooled too now.
        let chunk_synth_pool_needed = true;
        if chunk_synth_pool_needed {
            params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(vope_type(), true) });
            params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            // Phase B: unconditional, same reasoning as `_synth_pool`.
            params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(vope_type(), true) });
            params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
        }

        let mut ctx = VoleIrCtx::new(true);
        insert_w_wires(&mut ctx);
        bind_running(&mut ctx, &mut params, "in", running_done_acc, &running_next_pc, &running_next_state, &running_ret_vals);
        for i in lo..hi {
            let b = &boundary[i];
            let iface = &interfaces[i];
            bind_scalar_or_pool(&mut ctx, &mut params, b.is_active, format!("is_active_{i}"), iface.is_active_ty.clone());
            bind_scalar_or_pool(&mut ctx, &mut params, b.done, format!("done_{i}"), iface.done_ty.clone());
            for (j, &v) in b.next_pc_bits.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("next_pc_{i}_{j}"), iface.next_pc_bit_tys[j].clone());
            }
            for (k, &v) in b.next_state.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("next_state_{i}_{k}"), iface.next_state_tys[k].clone());
            }
            for (m, &v) in b.ret_vals.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("ret_val_{i}_{m}"), iface.ret_val_tys[m].clone());
            }
        }

        // Cross-chunk-shared values this chunk needs as an external
        // input: exactly the vars `accum_info.steps[lo]`'s own
        // `synthetic_in` names -- by `thread_synthetic_slots`'s own
        // construction, a region `r` has `v` in `synthetic_in` iff
        // `producer(v) < r <= last_consumer(v)`, so checking only the
        // chunk's own FIRST covered step answers "does this whole
        // chunk's producer for v lie strictly before it" for the whole
        // `[lo, hi)` span, whether `chunk_size` is 1 or many -- a var
        // whose producer AND every consumer fall inside this same chunk
        // never appears here at all (correctly: it's already threaded
        // internally, through this same `ctx`, no external param needed).
        let chunk_synth_in: Vec<u32> = accum_info.steps[lo].synthetic_in.clone();
        for &v in &chunk_synth_in {
            let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                "weave_vole_prover_ir_split: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
            ));
            if !matches!(ty, IrType::Array { .. }) {
                ctx.wires.insert(v, WireRepr::Pooled("_synth_pool", v as usize));
                continue;
            }
            bind_scalar(&mut ctx, &mut params, v, format!("synth_{v}"), ty);
        }

        debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);
        ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
        ctx.emit_circuit_stmts_range(block, types, mode, chunk_start..chunk_end);

        let local_entry_count = ctx.trace.entries.len() as u32;
        for mut e in ctx.trace.entries.clone() {
            e.timestamp += global_ts;
            overall_trace_entries.push(e);
        }
        global_ts += local_entry_count.max(ctx.mem_timestamp);

        let out_step = &accum_info.steps[hi - 1];
        let mut ret_tuple_tys: Vec<IrType> = Vec::new();
        let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
        running_export(&mut ctx, &mut ret_tuple_tys, &mut ret_tuple_exprs, out_step.done_acc, &out_step.next_pc, &out_step.next_state, &out_step.ret_vals);
        let hats_ty = hat_array_type(ctx.hat_names.len());
        let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| hat_ref_expr(h)).collect()));
        ret_tuple_tys.push(hats_ty);
        ret_tuple_exprs.push(hats_expr);

        // As the per-block loop's own synthetic_out handling above --
        // `accum_info.steps[hi - 1]`'s own `synthetic_out` correctly
        // captures "does this chunk need to re-export v" for the whole
        // `[lo, hi)` span (see `chunk_synth_in`'s own comment for why
        // checking only the boundary step is sufficient).
        for &v in &out_step.synthetic_out {
            let ty = ctx.slot_type(&CirVar(v), &vope_type());
            synthetic_types.entry(v).or_insert_with(|| ty.clone());
            if !matches!(ty, IrType::Array { .. }) {
                let slot_str = v.to_string();
                let value = ctx.slot_expr(&CirVar(v));
                ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                    left: Box::new(arr_index("_synth_pool", &slot_str)),
                    right: Box::new(value),
                }))));
                ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                    left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                    right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                }))));
                continue;
            }
            ret_tuple_tys.push(ty);
            ret_tuple_exprs.push(ctx.slot_expr(&CirVar(v)));
        }

        let chunk_func = IrFunction { no_inline: true,
            name: format!("vole_prove_ir_{}_accum_chunk_{}", name, chunk_idx),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause.clone(),
            body: IrBlock { stmts: ctx.finalize_and_take_stmts(), expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))) },
            external_kind: ExternalKind::Normal,
        };
        emit_fn(chunk_func);

        running_done_acc = out_step.done_acc;
        running_next_pc = out_step.next_pc.clone();
        running_next_state = out_step.next_state.clone();
        running_ret_vals = out_step.ret_vals.clone();
        lo = hi;
        chunk_idx += 1;
    }

    // ---- Finish: whatever remains after the accumulation phase (lower_to_circuit_ir's own terminator-select/padding) ----
    let finish_start = (accum_info.steps.last().expect("boundary is non-empty (asserted above)").end - num_params as u32) as usize;
    let finish_end = block.stmts.len();
    let finish_stmts = &block.stmts[finish_start..finish_end];
    let finish_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads_range(finish_stmts, types)
    } else { 0 };
    let finish_ext = count_external_primitives_range(finish_stmts, types);

    let mut params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    params.extend(w_params.iter().filter(|p| matches!(p.ty, IrType::Array { .. })).cloned());
    for j in 0..finish_oracle_reads {
        params.push(IrParam { name: format!("oracle_rd_{}", j), ty: vope_type() });
    }
    for (k, call) in finish_ext.oracle_calls.iter().enumerate() {
        for j in 0..call.total_bits { params.push(IrParam { name: format!("vope_ext_oracle_{}_bit_{}", k, j), ty: vope_type() }); }
    }
    for (k, call) in finish_ext.action_calls.iter().enumerate() {
        for j in 0..call.total_bits { params.push(IrParam { name: format!("vope_ext_action_{}_bit_{}", k, j), ty: vope_type() }); }
    }
    for (r, &width) in finish_ext.rng_widths.iter().enumerate() {
        for j in 0..width { params.push(IrParam { name: format!("vope_ext_rng_{}_bit_{}", r, j), ty: vope_type() }); }
    }
    // `bind_running`'s READ side can now register a pooled (`_synth_pool`)
    // read for the incoming running accumulator -- finish never had any
    // other reason to take `_synth_pool` (it has no `synthetic_in`/`out`
    // of its own, confirmed earlier), so this declaration was previously
    // entirely absent here. Unconditional for the same reason the block/
    // chunk sites are: done_acc/next_pc are unconditionally scalar (see
    // `bind_running`'s own doc), so finish needs this virtually always.
    params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(vope_type(), true) });
    params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
    // Phase B: unconditional, same reasoning as `_synth_pool`.
    params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(vope_type(), true) });
    params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });

    let mut ctx = VoleIrCtx::new(true);
    insert_w_wires(&mut ctx);
    bind_running(&mut ctx, &mut params, "in", running_done_acc, &running_next_pc, &running_next_state, &running_ret_vals);

    debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);
    ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
    ctx.emit_circuit_stmts_range(block, types, mode, finish_start..finish_end);

    let local_entry_count = ctx.trace.entries.len() as u32;
    for mut e in ctx.trace.entries.clone() {
        e.timestamp += global_ts;
        overall_trace_entries.push(e);
    }
    global_ts += local_entry_count.max(ctx.mem_timestamp);
    let _ = global_ts;

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("expected Jmp(Return)"),
    };
    let output_ty = if ret_args.len() == 1 {
        ctx.slot_type(&ret_args[0], &vope_type())
    } else {
        IrType::Tuple(ret_args.iter().map(|v| ctx.slot_type(v, &vope_type())).collect())
    };
    let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| hat_ref_expr(h)).collect()));
    let ret_type = IrType::Tuple(vec![output_ty, hat_array_type(ctx.hat_names.len())]);
    let output_expr = if ret_args.len() == 1 {
        ctx.slot_expr(&ret_args[0])
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| ctx.slot_expr(v)).collect()))
    };
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![output_expr, hats_expr]));

    let finish_func = IrFunction { no_inline: true,
        name: format!("vole_prove_ir_{}_finish", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };
    emit_fn(finish_func);

    MemoryTrace { entries: overall_trace_entries }
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
) -> Tagged<Transparent, IrModule<IrFunction>> {
    let mode = StorageMode::Tree(storage_sizes.clone());
    weave_vole_verifier_ir_with_mode(circuit, types, name, &mode, linkage).0
}

/// Weave a single-block Volar IR circuit into a VOLE **verifier** `IrModule`,
/// with configurable [`StorageMode`].
///
/// The verifier-as-a-computation is `Transparent` (non-ZK): the inner VOLE
/// proof already accounts for zero-knowledge.
pub fn weave_vole_verifier_ir_with_mode(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
    linkage: Option<&LinkageSystem>,
) -> (Tagged<Transparent, IrModule<IrFunction>>, MemoryTrace) {
    assert!(circuit.is_circuit(), "weave_vole_verifier_ir: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let and_count = count_ir_ands(block, types, mode);
    let num_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads(block, types)
    } else { 0 };
    let (generics, where_clause) = verifier_generics_and_where();

    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
    ];
    if and_count > 0 {
        params.push(IrParam { name: "q_and".into(), ty: q_and_array_type(and_count) });
        params.push(IrParam { name: "hat".into(), ty: hat_array_type(and_count) });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
        } else {
            params.push(IrParam { name: format!("w_{}", i), ty: wide_array_type(q_type(), w) });
        }
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

    let mut ctx = VoleIrCtx::new(false);
    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(ir_expr(IrExprKind::Lit(volar_compiler::ir::IrLit::Bool(true)))),
    }));

    ctx.emit_circuit(block, types, mode, &circuit.pre_init);

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("expected Jmp(Return)"),
    };
    let output_ty = if ret_args.len() == 1 {
        ctx.slot_type(&ret_args[0], &q_type())
    } else {
        IrType::Tuple(ret_args.iter().map(|v| ctx.slot_type(v, &q_type())).collect())
    };
    let ret_type = IrType::Tuple(vec![
        output_ty,
        IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool),
    ]);
    let output_expr = if ret_args.len() == 1 {
        ctx.slot_expr(&ret_args[0])
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| ctx.slot_expr(v)).collect()))
    };
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![output_expr, var("all_ok")]));
    let trace = ctx.trace.clone();

    let func = IrFunction { no_inline: true,
        name: format!("vole_verify_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
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
    (Tagged::seal(module), trace)
}

/// As [`weave_vole_verifier_ir_with_mode`], additionally threading a
/// [`VerifierTraceSink`] fold-accumulator through the woven verifier —
/// the Volar-IR analogue of [`weave_vole_verifier_with_trace`] (BIrBlocks).
/// Every Quicksilver AND-gate check counted by `and_count` (including each
/// degree-≥2 monomial inside an `IRStmt::Poly`, per
/// [`VoleIrCtx::emit_and`]) folds into `fold_state` via one `r_and_{k}`
/// challenge parameter each, mirroring `q_and_{k}`/`hat_{k}`'s existing
/// numbering exactly. `T`'s bound gains the sink's `fold_lift_trait_name()`
/// (if any) so `and_gate_step` can hand it whole `Q<N,T>`/`Array<T,N>`
/// values without this weaver needing to know how to project `T` itself —
/// this repo's only two `IopLift` impls (`Galois`, `Bit`) already cover
/// every scalar type this circuit's `IRStmt::Poly` degree-≥2 terms ever
/// operate on (RV32I's `Vec(K, Bit)` register/RAM words are bit-decomposed
/// into `K` independent scalar `Poly` statements well before this weaver
/// sees them — see `docs/wasm-feature-support.md` / `waffle_lower.rs`'s own
/// "bit-decomposed via `BitCircuitBuilder`" note — so no new lift logic is
/// needed here).
pub fn weave_vole_verifier_ir_with_mode_and_trace(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
    sink: &dyn VerifierTraceSink<()>,
    linkage: Option<&LinkageSystem>,
) -> (Tagged<Transparent, IrModule<IrFunction>>, MemoryTrace) {
    assert!(circuit.is_circuit(), "weave_vole_verifier_ir_with_trace: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let and_count = count_ir_ands(block, types, mode);
    let num_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads(block, types)
    } else { 0 };
    let (generics, mut where_clause) = verifier_generics_and_where();

    // Extend T's bound with the sink's fold-lift trait (bare, externally
    // resolved), mirroring `weave_vole_verifier_inner`'s identical step.
    if let Some(trait_name) = sink.fold_lift_trait_name() {
        if let Some(IrWherePredicate::TypeBound { bounds, .. }) = where_clause
            .iter_mut()
            .find(|p| matches!(p, IrWherePredicate::TypeBound { ty: IrType::TypeParam(n), .. } if n == "T"))
        {
            bounds.push(IrTraitBound {
                trait_kind: TraitKind::Custom(trait_name.into()),
                type_args: vec![],
                assoc_bindings: vec![],
            });
        }
    }

    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
    ];
    if and_count > 0 {
        params.push(IrParam { name: "q_and".into(), ty: q_and_array_type(and_count) });
        params.push(IrParam { name: "hat".into(), ty: hat_array_type(and_count) });
        params.push(IrParam {
            name: "r_and".into(),
            ty: r_and_array_type(and_count, sink.fold_scalar_type_name()),
        });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
        } else {
            params.push(IrParam { name: format!("w_{}", i), ty: wide_array_type(q_type(), w) });
        }
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

    let mut ctx = VoleIrCtx::new_verifier_with_trace_sink(sink);
    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(ir_expr(IrExprKind::Lit(volar_compiler::ir::IrLit::Bool(true)))),
    }));
    // Threaded fold-accumulator state -- bound once at entry, reassigned per
    // AND gate (inside `VoleIrCtx::emit_and`), returned alongside `all_ok`.
    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::Ident { mutable: true, name: "fold_state".into(), subpat: None },
        ty: None,
        init: Some(ir_expr(IrExprKind::Call {
            func: Box::new(ir_expr(IrExprKind::Path {
                segments: vec![sink.init_state_fn_name().into()],
                type_args: vec![],
            })),
            args: vec![],
        })),
    }));

    ctx.emit_circuit(block, types, mode, &circuit.pre_init);

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("expected Jmp(Return)"),
    };
    let output_ty = if ret_args.len() == 1 {
        ctx.slot_type(&ret_args[0], &q_type())
    } else {
        IrType::Tuple(ret_args.iter().map(|v| ctx.slot_type(v, &q_type())).collect())
    };
    let ret_type = IrType::Tuple(vec![
        output_ty,
        IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool),
        IrType::TypeParam(sink.state_type_name().into()),
    ]);
    let output_expr = if ret_args.len() == 1 {
        ctx.slot_expr(&ret_args[0])
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| ctx.slot_expr(v)).collect()))
    };
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![output_expr, var("all_ok"), var("fold_state")]));
    let trace = ctx.trace.clone();

    let func = IrFunction { no_inline: true,
        name: format!("vole_verify_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
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
    (Tagged::seal(module), trace)
}

/// Milestone 1.6: weave a single-block Volar IR circuit into a **`QSim`**
/// `IrModule` — a third, weaver-generated role (see [`VoleRole`]) that,
/// once compiled and run, *derives* the `q_and` values a real `Verifier`
/// function needs for its own chained AND gates, instead of a driver
/// hand-computing (or hand-mirroring the weaver to compute) them.
///
/// Unsplit counterpart to [`weave_vole_verifier_ir_with_mode_and_trace`],
/// matching its exact param/return shape **minus** the fold-only pieces
/// (`q_and_k` as an *input*, `r_and_k`, `all_ok`, `fold_state` — `QSim`
/// never folds, that's `Verifier`'s job once handed these derived values)
/// **plus** `hat_k` as an input per gate (same shape `Verifier` already
/// takes — the same real `hats` the compiled prover function returns) and
/// the derived `q_and` array as an output, alongside the circuit's own
/// output.
///
/// No `VerifierTraceSink`/linkage parameter: `QSim` has no fold-accumulator
/// plumbing to thread and produces a plain (untagged) `IrModule` — it is
/// driver-invoked scaffolding around the real `Verifier` call, not itself a
/// `Transparent`/`Zk` proving artifact.
pub fn weave_vole_qsim_ir_with_mode(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
) -> (IrModule<IrFunction>, MemoryTrace) {
    assert!(circuit.is_circuit(), "weave_vole_qsim_ir_with_mode: circuit must satisfy is_circuit()");
    let block = &circuit.blocks[0];
    let num_params = block.params.len();
    let and_count = count_ir_ands(block, types, mode);
    let num_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads(block, types)
    } else { 0 };
    let (generics, where_clause) = qsim_generics_and_where();

    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
    ];
    if and_count > 0 {
        params.push(IrParam { name: "hat".into(), ty: hat_array_type(and_count) });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
        } else {
            params.push(IrParam { name: format!("w_{}", i), ty: wide_array_type(q_type(), w) });
        }
    }
    // Oracle read parameters (Commitment mode) -- same real committed
    // memory Q-values as the Verifier call receives (the driver computes
    // these once and feeds them to both calls).
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

    let mut ctx = VoleIrCtx::new_qsim();
    ctx.emit_circuit(block, types, mode, &circuit.pre_init);

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("expected Jmp(Return)"),
    };
    let output_ty = if ret_args.len() == 1 {
        ctx.slot_type(&ret_args[0], &q_type())
    } else {
        IrType::Tuple(ret_args.iter().map(|v| ctx.slot_type(v, &q_type())).collect())
    };
    let q_and_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|n| wire_ref_expr(n)).collect()));
    let ret_type = IrType::Tuple(vec![output_ty, q_and_array_type(ctx.q_and_names.len())]);
    let output_expr = if ret_args.len() == 1 {
        ctx.slot_expr(&ret_args[0])
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| ctx.slot_expr(v)).collect()))
    };
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![output_expr, q_and_expr]));
    let trace = ctx.trace.clone();

    let func = IrFunction { no_inline: true,
        name: format!("vole_qsim_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "weaved_vole_ir_qsim".into(),
        functions: vec![func],
        structs: vec![], enums: vec![], traits: vec![], impls: vec![], type_aliases: vec![],
        consts: vec![],
    };
    (module, trace)
}

/// Milestone 1.6, Stage 2: as [`weave_vole_qsim_ir_with_mode`], but split
/// into one Rust function per original (pre-movfuscation) block plus a
/// *chunked* accumulator chain plus a trailing "finish" function --
/// mirroring [`weave_vole_verifier_ir_split_with_trace`]'s exact split
/// structure (same `boundary`/`accum_info`/`chunk_size` shape, same
/// per-function boundaries) **minus** the fold-only pieces (`q_and_k` as
/// an *input*, `r_and_k`, `all_ok`, `fold_state` -- `QSim` never folds)
/// **plus** `hat_k` as an input per gate (same shape `Verifier` takes --
/// the real `hat`s the matching, equally-split real prover function
/// returns) and a derived `q_and` array as an output, alongside each
/// function's own exported state.
///
/// A single unsplit `QSim` function was found to compile pathologically
/// slowly even for a *small* (~2,740-gate) real circuit -- confirmed via
/// direct LLVM profiling to be a single-function-size limitation in the
/// backend (`BranchRelaxation`/`AArch64PointerAuth`, both scaling poorly
/// with one very large function's statement count), not something
/// `#[inline(never)]` alone fixes. This split, already proven to weave
/// (and, for Prover/Verifier, compile) fast at real interpreter scale
/// (Milestone 1.5), is required for `QSim` too -- not just an optional
/// nicety for larger circuits.
pub fn weave_vole_qsim_ir_split(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
    boundary: &[volar_ir_passes::MovfuscBlockBoundary],
    accum_info: &volar_ir_passes::MovfuscAccumInfo,
    chunk_size: usize,
    max_stmts_per_piece: usize,
    mut emit_fn: impl FnMut(IrFunction),
) -> MemoryTrace {
    assert!(circuit.is_circuit(), "weave_vole_qsim_ir_split: circuit must satisfy is_circuit()");
    assert!(!boundary.is_empty(), "weave_vole_qsim_ir_split: boundary must be non-empty");
    assert_eq!(accum_info.steps.len(), boundary.len(), "accum_info must come from the same movfuscate_ir_with_boundary call as boundary");
    let chunk_size = chunk_size.max(1);
    let block = &circuit.blocks[0];
    let num_params = block.params.len();

    let (generics, where_clause) = qsim_generics_and_where();

    let mut w_params: Vec<IrParam> = Vec::new();
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            w_params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
        } else {
            w_params.push(IrParam { name: format!("w_{}", i), ty: wide_array_type(q_type(), w) });
        }
    }
    let insert_w_wires = |ctx: &mut VoleIrCtx| {
        for i in 0..num_params {
            let w = cir_type_width(&block.params[i], types);
            if w <= 1 {
                // Phase B: scalar top-level params are pooled via
                // `_w_pool`, keyed by raw param index (0..num_params,
                // disjoint from `_synth_pool`'s own var-id space -- see
                // `_w_pool`'s own declaration site for why it needs a
                // separate, loop-persistent pool rather than reusing
                // `_synth_pool`). No more per-value `w_i` named param for
                // these; `used_w`/`piece_used_w` no longer gate them
                // either (harmless to keep computing -- see the matching
                // comment at every `w_params` filter site).
                ctx.wires.insert(i as u32, WireRepr::Pooled("_w_pool", i));
            } else {
                // Lazy `WireRepr::Array`, not eagerly unpacked -- see its
                // own doc. With top-level parameter threading, `num_params`
                // covers every state slot, and this closure runs once per
                // (block/chunk/finish) split-weave function per role
                // (~241 x 3): eagerly unpacking every wide param into `w`
                // bound locals here, regardless of whether a given
                // function ever references them, is exactly what drove
                // `print_weaved_vole_module` to 13GB+ RSS printing the
                // real interpreter's full split-weave module.
                ctx.wires.insert(i as u32, WireRepr::Array(format!("w_{}", i), w));
            }
        }
    };

    let mut overall_trace_entries: Vec<MemoryTraceEntry> = Vec::new();
    let mut global_ts: u32 = 0;
    if matches!(mode, StorageMode::Commitment) && !circuit.pre_init.is_empty() {
        let n_stmts = block.stmts.len() as u32;
        let mut syn_id = num_params as u32 + n_stmts;
        for seg in &circuit.pre_init {
            let sid = seg.storage.0;
            let tid = seg.ty.0;
            for (local, _c) in seg.data.iter().enumerate() {
                let _ci = seg.offset + local;
                let val_id = syn_id; syn_id += 1;
                let addr_id = syn_id; syn_id += 1;
                overall_trace_entries.push(MemoryTraceEntry {
                    addr_var: addr_id,
                    value_var: val_id,
                    storage_id: sid,
                    type_id: tid,
                    is_write: true,
                    timestamp: global_ts,
                });
                global_ts += 1;
            }
        }
    }

    let shared_prefix: core::ops::Range<usize> = 0..(boundary[0].start as usize - num_params);

    fn bind_scalar(ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, var_id: u32, base_name: String, ty: IrType) {
        match &ty {
            IrType::Array { elem, len: volar_compiler::ir::ArrayLength::Const(n), .. } => {
                // Array-batched on the *param* side (one array param, not
                // `n` scalar params — same 65535-arg-limit reason as
                // `hat`/`q_and`/`r_and`/`w_i`). Kept as a lazy
                // `WireRepr::Array` rather than eagerly unpacked into `n`
                // bound locals -- see `WireRepr::Array`'s own doc; any
                // consumer needing real names (`Merge`, address
                // composition, the unrolled `Poly` fallback) materializes
                // on demand.
                params.push(IrParam { name: base_name.clone(), ty: wide_array_type((**elem).clone(), *n) });
                ctx.wires.insert(var_id, WireRepr::Array(base_name, *n));
            }
            _ => {
                params.push(IrParam { name: base_name.clone(), ty });
                ctx.wires.insert(var_id, WireRepr::Scalar(base_name));
            }
        }
    }

    /// As [`bind_scalar`], but for a value this function may instead read
    /// straight from `_synth_pool` (a block-boundary export or running-
    /// accumulator input, keyed by its own raw circuit var id, exactly
    /// like `synthetic_in` -- see `export_scalar_or_tuple`'s own doc for
    /// why one shared pool safely covers every category). Pooling here
    /// must agree with whatever decision the *producer* made for this
    /// same var id -- both sides derive it identically, from the var's
    /// own type alone, so they can never disagree.
    fn bind_scalar_or_pool(ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, var_id: u32, base_name: String, ty: IrType) {
        if !matches!(ty, IrType::Array { .. }) {
            ctx.wires.insert(var_id, WireRepr::Pooled("_synth_pool", var_id as usize));
            return;
        }
        bind_scalar(ctx, params, var_id, base_name, ty);
    }

    let mut interfaces: Vec<SplitBlockInterface> = Vec::with_capacity(boundary.len());

    // As `weave_vole_prover_ir_split`'s own `synthetic_types` -- see its
    // doc comment for the full rationale.
    let mut synthetic_types: alloc::collections::BTreeMap<u32, IrType> = alloc::collections::BTreeMap::new();

    for (i, b) in boundary.iter().enumerate() {
        let start = (b.start - num_params as u32) as usize;
        let end = (b.end - num_params as u32) as usize;
        let local_stmts = &block.stmts[start..end];
        let local_and_count = count_ir_ands_no_storage_range(local_stmts, types);
        let local_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(local_stmts, types)
        } else { 0 };
        let local_ext = count_external_primitives_range(local_stmts, types);

        // See the matching comment in `weave_vole_prover_ir_split`'s own
        // block loop -- only declare the `w_i` this block function
        // actually references.
        let mut used_w: alloc::collections::BTreeSet<u32> = collect_used_top_level_params(&block.stmts[shared_prefix.clone()], num_params);
        used_w.extend(collect_used_top_level_params(local_stmts, num_params));
        for &v in core::iter::once(&b.is_active).chain(core::iter::once(&b.done))
            .chain(b.next_pc_bits.iter()).chain(b.next_state.iter()).chain(b.ret_vals.iter())
        {
            if (v as usize) < num_params { used_w.insert(v); }
        }

        // See `weave_vole_prover_ir_split`'s own doc for the full
        // rationale -- same guard (skip splitting regions with any
        // oracle/action/rng call), same threshold. qsim ALSO needs the
        // prover's own `hat` witnesses as an INPUT array (unlike the
        // prover, whose hats are an OUTPUT) -- `local_and_count`-sized,
        // sliced into contiguous per-piece sub-arrays exactly like
        // `oracle_rd_N`, just as one array-literal argument per piece
        // instead of N scalar args.
        let can_split = local_ext.oracle_calls.is_empty() && local_ext.action_calls.is_empty() && local_ext.rng_widths.is_empty();
        let mut region_outputs: Vec<u32> = alloc::vec![b.is_active, b.done];
        region_outputs.extend(b.next_pc_bits.iter().copied());
        region_outputs.extend(b.next_state.iter().copied());
        region_outputs.extend(b.ret_vals.iter().copied());
        region_outputs.extend(b.synthetic_out.iter().copied());
        let pieces = if can_split {
            crate::vole_split::split_region_into_pieces(block, num_params, b.start, b.end, &region_outputs, max_stmts_per_piece)
        } else {
            alloc::vec![crate::vole_split::PieceSpec { start: b.start, end: b.end, extra_in: Vec::new(), extra_out: Vec::new() }]
        };

        if pieces.len() <= 1 {
            // ---- Unchanged: single function ----
            let mut params: Vec<IrParam> = vec![
                IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
            ];
            if local_and_count > 0 {
                params.push(IrParam { name: "hat".into(), ty: hat_array_type(local_and_count) });
            }
            params.push(IrParam { name: "q_one".into(), ty: q_type() });
            params.extend(w_params.iter().enumerate().filter(|(idx, p)| used_w.contains(&(*idx as u32)) && matches!(p.ty, IrType::Array { .. })).map(|(_, p)| p.clone()));
            for j in 0..local_oracle_reads {
                params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
            }
            for (k, call) in local_ext.oracle_calls.iter().enumerate() {
                for j in 0..call.total_bits {
                    params.push(IrParam { name: format!("q_ext_oracle_{}_bit_{}", k, j), ty: q_type() });
                }
            }
            for (k, call) in local_ext.action_calls.iter().enumerate() {
                for j in 0..call.total_bits {
                    params.push(IrParam { name: format!("q_ext_action_{}_bit_{}", k, j), ty: q_type() });
                }
            }
            for (r, &width) in local_ext.rng_widths.iter().enumerate() {
                for j in 0..width {
                    params.push(IrParam { name: format!("q_ext_rng_{}_bit_{}", r, j), ty: q_type() });
                }
            }

            // Unconditional (not just when synthetic_in/out are non-empty):
            // block-boundary exports (is_active/done/next_pc/next_state/
            // ret_vals) are ALSO pooled now, and every block has at least
            // an is_active+done pair, so this is needed almost always in
            // practice anyway -- an unused pool param on the rare block
            // that somehow needs none of this is harmless (the generated
            // preamble already has #![allow(unused_variables, ...)]).
            let synth_pool_needed = true;
            if synth_pool_needed {
                params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(q_type(), true) });
                params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                // Phase B: unconditional, same reasoning as `_synth_pool`.
                params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
                params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            }

            let mut ctx = VoleIrCtx::new_qsim();
            insert_w_wires(&mut ctx);
            for &v in &b.synthetic_in {
                let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                    "weave_vole_qsim_ir_split: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                ));
                if !matches!(ty, IrType::Array { .. }) {
                    ctx.wires.insert(v, WireRepr::Pooled("_synth_pool", v as usize));
                    continue;
                }
                bind_scalar(&mut ctx, &mut params, v, format!("synth_{v}"), ty);
            }
            debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);
            ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
            ctx.emit_circuit_stmts_range(block, types, mode, start..end);

            let local_entry_count = ctx.trace.entries.len() as u32;
            for mut e in ctx.trace.entries.clone() {
                e.timestamp += global_ts;
                overall_trace_entries.push(e);
            }
            global_ts += local_entry_count.max(ctx.mem_timestamp);

            let is_active_v = CirVar(b.is_active);
            let done_v = CirVar(b.done);
            let is_active_expr = ctx.slot_expr(&is_active_v);
            let done_expr = ctx.slot_expr(&done_v);
            let is_active_ty = ctx.slot_type(&is_active_v, &q_type());
            let done_ty = ctx.slot_type(&done_v, &q_type());
            let next_pc_exprs: Vec<IrExpr> = b.next_pc_bits.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let next_pc_bit_tys: Vec<IrType> = b.next_pc_bits.iter().map(|&v| ctx.slot_type(&CirVar(v), &q_type())).collect();
            let next_state_exprs: Vec<IrExpr> = b.next_state.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let next_state_tys: Vec<IrType> = b.next_state.iter().map(|&v| ctx.slot_type(&CirVar(v), &q_type())).collect();
            let ret_val_exprs: Vec<IrExpr> = b.ret_vals.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let ret_val_tys: Vec<IrType> = b.ret_vals.iter().map(|&v| ctx.slot_type(&CirVar(v), &q_type())).collect();

            let hats_ty = q_and_array_type(ctx.q_and_names.len());
            let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|h| wire_ref_expr(h)).collect()));

            // Block-boundary exports: pool scalar ones directly via
            // `_synth_pool` -- see `export_scalar_or_tuple`'s own doc and
            // the matching Prover-role site. QSim's own is_active/done/
            // next_pc/next_state/ret_vals values are discarded downstream
            // regardless (only Verifier's are threaded onward), but the
            // return-tuple layout must still match this function's own
            // real generated body, so the same pooling decision applies
            // here too.
            let mut ret_tuple_tys: Vec<IrType> = Vec::new();
            let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
            export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.is_active, is_active_ty.clone(), is_active_expr);
            export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.done, done_ty.clone(), done_expr);
            for (j, expr) in next_pc_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_pc_bits[j], next_pc_bit_tys[j].clone(), expr);
            }
            for (k, expr) in next_state_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_state[k], next_state_tys[k].clone(), expr);
            }
            for (m, expr) in ret_val_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.ret_vals[m], ret_val_tys[m].clone(), expr);
            }
            ret_tuple_tys.push(hats_ty);
            ret_tuple_exprs.push(hats_expr);

            // As `weave_vole_prover_ir_split`'s own synthetic_out handling.
            for &v in &b.synthetic_out {
                let ty = ctx.slot_type(&CirVar(v), &q_type());
                synthetic_types.entry(v).or_insert_with(|| ty.clone());
                if !matches!(ty, IrType::Array { .. }) {
                    let slot_str = v.to_string();
                    let value = ctx.slot_expr(&CirVar(v));
                    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool", &slot_str)),
                        right: Box::new(value),
                    }))));
                    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                        right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                    }))));
                    continue;
                }
                ret_tuple_tys.push(ty);
                ret_tuple_exprs.push(ctx.slot_expr(&CirVar(v)));
            }

            let func = IrFunction { no_inline: true,
                name: format!("vole_qsim_ir_{}_block_{}", name, i),
                module_path: vec![],
                generics: generics.clone(),
                receiver: None,
                params,
                return_type: Some(IrType::Tuple(ret_tuple_tys)),
                where_clause: where_clause.clone(),
                body: IrBlock {
                    stmts: ctx.finalize_and_take_stmts(),
                    expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
                },
                external_kind: ExternalKind::Normal,
            };
            emit_fn(func);

            interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
        } else {
            // ---- Split: K piece functions + 1 wrapper. See
            // `weave_vole_prover_ir_split`'s own "Split:" branch for the
            // full design; only the role-specific deltas are called out
            // here (base type `q_type()`, `VoleIrCtx::new_qsim()`,
            // `q_one`/`q_ext_*` naming, and the `hat` INPUT array's own
            // per-piece slicing).
            debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);

            let mut producer_piece: alloc::collections::BTreeMap<u32, usize> = alloc::collections::BTreeMap::new();
            for (p, piece) in pieces.iter().enumerate() {
                for &v in &piece.extra_out {
                    producer_piece.entry(v).or_insert(p);
                }
            }

            // ---- Phase C sub-stage 1: pool scalar cross-piece
            // ("piece_in_v") values -- see the identical Prover-side
            // design doc in `weave_vole_prover_ir_split`.
            let all_extra_in_vars: alloc::collections::BTreeSet<u32> =
                pieces.iter().flat_map(|pc| pc.extra_in.iter().copied()).collect();
            let mut pool_slot: alloc::collections::BTreeMap<u32, usize> = alloc::collections::BTreeMap::new();
            let region_has_cross_piece_vars = pieces.iter().any(|pc| !pc.extra_in.is_empty() || !pc.extra_out.is_empty());

            let mut piece_names: Vec<String> = Vec::with_capacity(pieces.len());
            let mut piece_used_w: Vec<alloc::collections::BTreeSet<u32>> = Vec::with_capacity(pieces.len());
            let mut piece_oracle_counts: Vec<usize> = Vec::with_capacity(pieces.len());
            let mut piece_and_counts: Vec<usize> = Vec::with_capacity(pieces.len());
            let mut piece_q_and_counts: Vec<usize> = Vec::with_capacity(pieces.len());
            let mut piece_out_types: alloc::collections::BTreeMap<u32, IrType> = alloc::collections::BTreeMap::new();

            for (p, piece) in pieces.iter().enumerate() {
                let p_start = (piece.start - num_params as u32) as usize;
                let p_end = (piece.end - num_params as u32) as usize;
                let p_stmts = &block.stmts[p_start..p_end];
                let p_and_count = count_ir_ands_no_storage_range(p_stmts, types);
                let p_oracle_reads = if matches!(mode, StorageMode::Commitment) {
                    count_storage_reads_range(p_stmts, types)
                } else { 0 };

                let mut p_used_w: alloc::collections::BTreeSet<u32> = collect_used_top_level_params(&block.stmts[shared_prefix.clone()], num_params);
                p_used_w.extend(collect_used_top_level_params(p_stmts, num_params));
                for &v in &piece.extra_out {
                    if (v as usize) < num_params { p_used_w.insert(v); }
                }

                let mut p_params: Vec<IrParam> = vec![
                    IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
                ];
                if p_and_count > 0 {
                    p_params.push(IrParam { name: "hat".into(), ty: hat_array_type(p_and_count) });
                }
                p_params.push(IrParam { name: "q_one".into(), ty: q_type() });
                p_params.extend(w_params.iter().enumerate().filter(|(idx, pr)| p_used_w.contains(&(*idx as u32)) && matches!(pr.ty, IrType::Array { .. })).map(|(_, pr)| pr.clone()));
                for j in 0..p_oracle_reads {
                    p_params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
                }
                if region_has_cross_piece_vars {
                    p_params.push(IrParam { name: "_piece_pool".into(), ty: pool_slice_type(q_type(), true) });
                    p_params.push(IrParam { name: "_piece_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                }
                // Phase B: unconditional, same reasoning as every other
                // `_w_pool` declaration site -- a piece may reference any
                // scalar top-level param directly.
                p_params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
                p_params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });

                let mut ctx = VoleIrCtx::new_qsim();
                insert_w_wires(&mut ctx);
                for &v in &b.synthetic_in {
                    let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                        "weave_vole_qsim_ir_split: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                    ));
                    bind_scalar(&mut ctx, &mut p_params, v, format!("synth_{v}"), ty);
                }
                for &v in &piece.extra_in {
                    if let Some(&slot) = pool_slot.get(&v) {
                        ctx.wires.insert(v, WireRepr::Pooled("_piece_pool", slot));
                        continue;
                    }
                    let ty = piece_out_types.get(&v).cloned().unwrap_or_else(|| panic!(
                        "weave_vole_qsim_ir_split: piece-local var {v} has no known type -- its own producing piece must run before this consuming piece"
                    ));
                    bind_scalar(&mut ctx, &mut p_params, v, format!("piece_in_{v}"), ty);
                }

                ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
                ctx.emit_circuit_stmts_range(block, types, mode, p_start..p_end);

                let local_entry_count = ctx.trace.entries.len() as u32;
                for mut e in ctx.trace.entries.clone() {
                    e.timestamp += global_ts;
                    overall_trace_entries.push(e);
                }
                global_ts += local_entry_count.max(ctx.mem_timestamp);

                let mut p_ret_tys: Vec<IrType> = Vec::with_capacity(piece.extra_out.len() + 1);
                let mut p_ret_exprs: Vec<IrExpr> = Vec::with_capacity(piece.extra_out.len() + 1);
                for &v in &piece.extra_out {
                    let ty = ctx.slot_type(&CirVar(v), &q_type());
                    let is_scalar = !matches!(ty, IrType::Array { .. });
                    if is_scalar && all_extra_in_vars.contains(&v) {
                        if pool_slot.contains_key(&v) {
                            // The first producer owns the pool slot; later
                            // pass-through pieces reuse its mapping.
                            piece_out_types.insert(v, ty);
                            continue;
                        }
                        let slot = pool_slot.len();
                        pool_slot.insert(v, slot);
                        let slot_str = slot.to_string();
                        let value = ctx.slot_expr(&CirVar(v));
                        ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                            left: Box::new(arr_index("_piece_pool", &slot_str)),
                            right: Box::new(value),
                        }))));
                        ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                            left: Box::new(arr_index("_piece_pool_written", &slot_str)),
                            right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                        }))));
                        piece_out_types.insert(v, ty);
                        continue;
                    }
                    p_ret_exprs.push(ctx.slot_expr(&CirVar(v)));
                    piece_out_types.insert(v, ty.clone());
                    p_ret_tys.push(ty);
                }
                let p_hats_ty = q_and_array_type(ctx.q_and_names.len());
                let p_hats_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|h| wire_ref_expr(h)).collect()));
                piece_q_and_counts.push(ctx.q_and_names.len());
                p_ret_tys.push(p_hats_ty);
                p_ret_exprs.push(p_hats_expr);

                let p_name = format!("vole_qsim_ir_{}_block_{}_piece_{}", name, i, p);
                let p_func = IrFunction { no_inline: true,
                    name: p_name.clone(),
                    module_path: vec![],
                    generics: generics.clone(),
                    receiver: None,
                    params: p_params,
                    return_type: Some(IrType::Tuple(p_ret_tys)),
                    where_clause: where_clause.clone(),
                    body: IrBlock {
                        stmts: ctx.finalize_and_take_stmts(),
                        expr: Some(Box::new(ir_expr(IrExprKind::Tuple(p_ret_exprs)))),
                    },
                    external_kind: ExternalKind::Normal,
                };
                emit_fn(p_func);

                piece_names.push(p_name);
                piece_used_w.push(p_used_w);
                piece_oracle_counts.push(p_oracle_reads);
                piece_and_counts.push(p_and_count);
            }

            let mut wrapper_params: Vec<IrParam> = vec![
                IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
            ];
            if local_and_count > 0 {
                wrapper_params.push(IrParam { name: "hat".into(), ty: hat_array_type(local_and_count) });
            }
            wrapper_params.push(IrParam { name: "q_one".into(), ty: q_type() });
            wrapper_params.extend(w_params.iter().enumerate().filter(|(idx, p)| used_w.contains(&(*idx as u32)) && matches!(p.ty, IrType::Array { .. })).map(|(_, p)| p.clone()));
            for j in 0..local_oracle_reads {
                wrapper_params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
            }
            // Unconditional (not just when synthetic_in/out are non-empty):
            // block-boundary exports (is_active/done/next_pc/next_state/
            // ret_vals) are ALSO pooled now, and every block has at least
            // an is_active+done pair, so this is needed almost always in
            // practice anyway -- an unused pool param on the rare block
            // that somehow needs none of this is harmless (the generated
            // preamble already has #![allow(unused_variables, ...)]).
            let synth_pool_needed = true;
            if synth_pool_needed {
                wrapper_params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(q_type(), true) });
                wrapper_params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                // Phase B: unconditional, same reasoning as `_synth_pool`.
                wrapper_params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
                wrapper_params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            }
            for &v in &b.synthetic_in {
                let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                    "weave_vole_qsim_ir_split: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                ));
                if matches!(ty, IrType::Array { .. }) {
                    wrapper_params.push(IrParam { name: format!("synth_{v}"), ty });
                }
            }

            let mut wrapper_stmts: Vec<IrStmt> = Vec::with_capacity(pieces.len() + 2);
            if region_has_cross_piece_vars {
                wrapper_stmts.push(pool_decl_stmt("_piece_pool", q_type(), q_default_call(), pool_slot.len()));
                wrapper_stmts.push(pool_decl_stmt("_piece_pool_written", IrType::Primitive(PrimitiveType::Bool), ir_expr(IrExprKind::Lit(IrLit::Bool(false))), pool_slot.len()));
            }
            let mut oracle_offset = 0usize;
            let mut and_offset = 0usize;
            for (p, piece) in pieces.iter().enumerate() {
                // `delta` is `&Delta<N,T>` -- a reference, trivially
                // `Copy` regardless of whether the pointee is -- cloning
                // it (`delta.clone()`) would auto-deref through to
                // `Delta::clone`, producing an OWNED `Delta<N,T>` where
                // every piece call expects `&Delta<N,T>`. No `.clone()`
                // needed at all, unlike the genuinely-owned non-`Copy`
                // wire values below.
                let mut call_args: Vec<IrExpr> = vec![var("delta")];
                if piece_and_counts[p] > 0 {
                    let hat_slice: Vec<IrExpr> = (0..piece_and_counts[p])
                        .map(|j| clone_expr(arr_index("hat", &(and_offset + j).to_string())))
                        .collect();
                    call_args.push(ir_expr(IrExprKind::FixedArray(hat_slice)));
                }
                and_offset += piece_and_counts[p];
                call_args.push(clone_expr(var("q_one")));
                for &idx in &piece_used_w[p] {
                    if matches!(w_params[idx as usize].ty, IrType::Array { .. }) {
                        call_args.push(clone_expr(var(&format!("w_{idx}"))));
                    }
                }
                for j in 0..piece_oracle_counts[p] {
                    call_args.push(clone_expr(var(&format!("oracle_rd_{}", oracle_offset + j))));
                }
                oracle_offset += piece_oracle_counts[p];
                if region_has_cross_piece_vars {
                    call_args.push(slice_ref_mut_expr("_piece_pool"));
                    call_args.push(slice_ref_mut_expr("_piece_pool_written"));
                }
                // Phase B: unconditional, matching every piece's own
                // unconditional `_w_pool`/`_w_pool_written` params.
                call_args.push(slice_ref_mut_expr("_w_pool"));
                call_args.push(slice_ref_mut_expr("_w_pool_written"));
                for &v in &b.synthetic_in {
                    let ty = synthetic_types.get(&v).cloned().expect("synthetic_types must already be populated");
                    if matches!(ty, IrType::Array { .. }) {
                        call_args.push(clone_expr(var(&format!("synth_{v}"))));
                    } else {
                        call_args.push(pooled_read_expr("_synth_pool", v as usize));
                    }
                }
                for &v in &piece.extra_in {
                    if pool_slot.contains_key(&v) { continue; }
                    let producer = producer_piece.get(&v).copied().expect("weave_vole_qsim_ir_split: extra_in var must have a producer piece");
                    call_args.push(clone_expr(var(&format!("piece{producer}_v{v}"))));
                }

                let mut pattern_names: Vec<String> = piece.extra_out.iter()
                    .filter(|v| !pool_slot.contains_key(v))
                    .map(|&v| format!("piece{p}_v{v}"))
                    .collect();
                pattern_names.push(format!("piece{p}_hats"));

                wrapper_stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::Tuple(pattern_names.into_iter().map(IrPattern::ident).collect()),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::Call {
                        func: Box::new(ir_expr(IrExprKind::Path {
                            segments: vec![piece_names[p].clone()],
                            type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
                        })),
                        args: call_args,
                    })),
                }));
            }

            let field_expr = |v: u32| match pool_slot.get(&v) {
                Some(&slot) => pooled_read_expr("_piece_pool", slot),
                None => clone_expr(var(&format!("piece{}_v{}", producer_piece[&v], v))),
            };
            let field_ty = |v: u32| piece_out_types[&v].clone();

            let is_active_ty = field_ty(b.is_active);
            let done_ty = field_ty(b.done);
            let is_active_expr = field_expr(b.is_active);
            let done_expr = field_expr(b.done);
            let next_pc_exprs: Vec<IrExpr> = b.next_pc_bits.iter().map(|&v| field_expr(v)).collect();
            let next_pc_bit_tys: Vec<IrType> = b.next_pc_bits.iter().map(|&v| field_ty(v)).collect();
            let next_state_exprs: Vec<IrExpr> = b.next_state.iter().map(|&v| field_expr(v)).collect();
            let next_state_tys: Vec<IrType> = b.next_state.iter().map(|&v| field_ty(v)).collect();
            let ret_val_exprs: Vec<IrExpr> = b.ret_vals.iter().map(|&v| field_expr(v)).collect();
            let ret_val_tys: Vec<IrType> = b.ret_vals.iter().map(|&v| field_ty(v)).collect();

            let mut combined_hats_exprs: Vec<IrExpr> = Vec::new();
            for p in 0..pieces.len() {
                for j in 0..piece_q_and_counts[p] {
                    combined_hats_exprs.push(clone_expr(arr_index(&format!("piece{p}_hats"), &j.to_string())));
                }
            }
            let hats_ty = q_and_array_type(combined_hats_exprs.len());
            let hats_expr = ir_expr(IrExprKind::FixedArray(combined_hats_exprs));

            // Block-boundary exports: pool scalar ones directly via
            // `_synth_pool` -- see `export_scalar_or_tuple`'s own doc.
            let mut ret_tuple_tys: Vec<IrType> = Vec::new();
            let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
            export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.is_active, is_active_ty.clone(), is_active_expr);
            export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.done, done_ty.clone(), done_expr);
            for (j, expr) in next_pc_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_pc_bits[j], next_pc_bit_tys[j].clone(), expr);
            }
            for (k, expr) in next_state_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_state[k], next_state_tys[k].clone(), expr);
            }
            for (m, expr) in ret_val_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.ret_vals[m], ret_val_tys[m].clone(), expr);
            }
            ret_tuple_tys.push(hats_ty);
            ret_tuple_exprs.push(hats_expr);

            for &v in &b.synthetic_out {
                let ty = field_ty(v);
                synthetic_types.entry(v).or_insert_with(|| ty.clone());
                if !matches!(ty, IrType::Array { .. }) {
                    let slot_str = v.to_string();
                    wrapper_stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool", &slot_str)),
                        right: Box::new(field_expr(v)),
                    }))));
                    wrapper_stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                        right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                    }))));
                    continue;
                }
                ret_tuple_tys.push(ty);
                ret_tuple_exprs.push(field_expr(v));
            }

            let wrapper_func = IrFunction { no_inline: true,
                name: format!("vole_qsim_ir_{}_block_{}", name, i),
                module_path: vec![],
                generics: generics.clone(),
                receiver: None,
                params: wrapper_params,
                return_type: Some(IrType::Tuple(ret_tuple_tys)),
                where_clause: where_clause.clone(),
                body: IrBlock {
                    stmts: wrapper_stmts,
                    expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
                },
                external_kind: ExternalKind::Normal,
            };
            emit_fn(wrapper_func);

            interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
        }
    }

    let (init_next_state_tys, init_ret_val_tys) = {
        let mut probe_ctx = VoleIrCtx::new_qsim();
        // See the matching comment in `weave_vole_prover_ir_split` -- the
        // circuit's own top-level params must be bound here too.
        insert_w_wires(&mut probe_ctx);
        // shared_prefix must be emitted here too -- unconstrained CSE can
        // (and does, in practice: a Bit-typed zero seed statement is a
        // prime dedup target) merge one of accum_info.init's own seed
        // statements onto shared_prefix's own statement, so a reference
        // into accum_info.init's own next_state/ret_vals can legitimately
        // point at a shared_prefix var -- every other ctx in this
        // function already emits shared_prefix before querying anything;
        // this probe was the one place that didn't.
        probe_ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
        let init_start = (accum_info.init.start - num_params as u32) as usize;
        let init_end = (accum_info.init.end - num_params as u32) as usize;
        probe_ctx.emit_circuit_stmts_range(block, types, mode, init_start..init_end);
        let next_state_tys: Vec<IrType> = accum_info.init.next_state.iter()
            .map(|&v| probe_ctx.slot_type(&CirVar(v), &q_type())).collect();
        let ret_val_tys: Vec<IrType> = accum_info.init.ret_vals.iter()
            .map(|&v| probe_ctx.slot_type(&CirVar(v), &q_type())).collect();
        (next_state_tys, ret_val_tys)
    };

    let bind_running = |ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, prefix: &str,
                         done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| {
        bind_scalar_or_pool(ctx, params, done_acc, format!("{prefix}_done_acc"), q_type());
        for (j, &v) in next_pc.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_next_pc_{j}"), q_type());
        }
        for (k, &v) in next_state.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_next_state_{k}"), init_next_state_tys[k].clone());
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_ret_val_{m}"), init_ret_val_tys[m].clone());
        }
    };
    // Running-accumulator OUTGOING state: pool scalar ones directly via
    // `_synth_pool` -- see the matching Prover-role comment.
    let running_export = |ctx: &mut VoleIrCtx, ret_tuple_tys: &mut Vec<IrType>, ret_tuple_exprs: &mut Vec<IrExpr>,
                           done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| {
        let done_acc_expr = ctx.slot_expr(&CirVar(done_acc));
        export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, done_acc, q_type(), done_acc_expr);
        for &v in next_pc {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, q_type(), expr);
        }
        for (k, &v) in next_state.iter().enumerate() {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, init_next_state_tys[k].clone(), expr);
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, init_ret_val_tys[m].clone(), expr);
        }
    };

    let n_blocks = boundary.len();
    let mut running_done_acc = accum_info.init.done_acc;
    let mut running_next_pc = accum_info.init.next_pc.clone();
    let mut running_next_state = accum_info.init.next_state.clone();
    let mut running_ret_vals = accum_info.init.ret_vals.clone();

    let mut lo = 0usize;
    let mut chunk_idx = 0usize;
    while lo < n_blocks {
        let hi = (lo + chunk_size).min(n_blocks);
        let chunk_start = (accum_info.steps[lo].start - num_params as u32) as usize;
        let chunk_end = (accum_info.steps[hi - 1].end - num_params as u32) as usize;
        let chunk_stmts = &block.stmts[chunk_start..chunk_end];
        let chunk_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(chunk_stmts, types)
        } else { 0 };
        let chunk_ext = count_external_primitives_range(chunk_stmts, types);
        let chunk_and_count = count_ir_ands_no_storage_range(chunk_stmts, types);

        let mut params: Vec<IrParam> = vec![
            IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
        ];
        if chunk_and_count > 0 {
            params.push(IrParam { name: "hat".into(), ty: hat_array_type(chunk_and_count) });
        }
        params.push(IrParam { name: "q_one".into(), ty: q_type() });
        params.extend(w_params.iter().filter(|p| matches!(p.ty, IrType::Array { .. })).cloned());
        for j in 0..chunk_oracle_reads {
            params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
        }
        for (k, call) in chunk_ext.oracle_calls.iter().enumerate() {
            for j in 0..call.total_bits { params.push(IrParam { name: format!("q_ext_oracle_{}_bit_{}", k, j), ty: q_type() }); }
        }
        for (k, call) in chunk_ext.action_calls.iter().enumerate() {
            for j in 0..call.total_bits { params.push(IrParam { name: format!("q_ext_action_{}_bit_{}", k, j), ty: q_type() }); }
        }
        for (r, &width) in chunk_ext.rng_widths.iter().enumerate() {
            for j in 0..width { params.push(IrParam { name: format!("q_ext_rng_{}_bit_{}", r, j), ty: q_type() }); }
        }

        // Unconditional -- see the matching comment at the block-level
        // synth_pool_needed site: block-boundary exports are pooled too now.
        let chunk_synth_pool_needed = true;
        if chunk_synth_pool_needed {
            params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(q_type(), true) });
            params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            // Phase B: unconditional, same reasoning as `_synth_pool`.
            params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
            params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
        }

        let mut ctx = VoleIrCtx::new_qsim();
        insert_w_wires(&mut ctx);
        bind_running(&mut ctx, &mut params, "in", running_done_acc, &running_next_pc, &running_next_state, &running_ret_vals);
        for i in lo..hi {
            let b = &boundary[i];
            let iface = &interfaces[i];
            bind_scalar_or_pool(&mut ctx, &mut params, b.is_active, format!("is_active_{i}"), iface.is_active_ty.clone());
            bind_scalar_or_pool(&mut ctx, &mut params, b.done, format!("done_{i}"), iface.done_ty.clone());
            for (j, &v) in b.next_pc_bits.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("next_pc_{i}_{j}"), iface.next_pc_bit_tys[j].clone());
            }
            for (k, &v) in b.next_state.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("next_state_{i}_{k}"), iface.next_state_tys[k].clone());
            }
            for (m, &v) in b.ret_vals.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("ret_val_{i}_{m}"), iface.ret_val_tys[m].clone());
            }
        }

        // As `weave_vole_prover_ir_split`'s own `chunk_synth_in` handling.
        let chunk_synth_in: Vec<u32> = accum_info.steps[lo].synthetic_in.clone();
        for &v in &chunk_synth_in {
            let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                "weave_vole_qsim_ir_split: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
            ));
            if !matches!(ty, IrType::Array { .. }) {
                ctx.wires.insert(v, WireRepr::Pooled("_synth_pool", v as usize));
                continue;
            }
            bind_scalar(&mut ctx, &mut params, v, format!("synth_{v}"), ty);
        }

        debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);
        ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
        ctx.emit_circuit_stmts_range(block, types, mode, chunk_start..chunk_end);

        let local_entry_count = ctx.trace.entries.len() as u32;
        for mut e in ctx.trace.entries.clone() {
            e.timestamp += global_ts;
            overall_trace_entries.push(e);
        }
        global_ts += local_entry_count.max(ctx.mem_timestamp);

        let out_step = &accum_info.steps[hi - 1];
        let mut ret_tuple_tys: Vec<IrType> = Vec::new();
        let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
        running_export(&mut ctx, &mut ret_tuple_tys, &mut ret_tuple_exprs, out_step.done_acc, &out_step.next_pc, &out_step.next_state, &out_step.ret_vals);
        let hats_ty = q_and_array_type(ctx.q_and_names.len());
        let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|h| wire_ref_expr(h)).collect()));
        ret_tuple_tys.push(hats_ty);
        ret_tuple_exprs.push(hats_expr);

        // As `weave_vole_prover_ir_split`'s own chunk-level synthetic_out handling.
        for &v in &out_step.synthetic_out {
            let ty = ctx.slot_type(&CirVar(v), &q_type());
            synthetic_types.entry(v).or_insert_with(|| ty.clone());
            if !matches!(ty, IrType::Array { .. }) {
                let slot_str = v.to_string();
                let value = ctx.slot_expr(&CirVar(v));
                ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                    left: Box::new(arr_index("_synth_pool", &slot_str)),
                    right: Box::new(value),
                }))));
                ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                    left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                    right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                }))));
                continue;
            }
            ret_tuple_tys.push(ty);
            ret_tuple_exprs.push(ctx.slot_expr(&CirVar(v)));
        }

        let chunk_func = IrFunction { no_inline: true,
            name: format!("vole_qsim_ir_{}_accum_chunk_{}", name, chunk_idx),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause.clone(),
            body: IrBlock { stmts: ctx.finalize_and_take_stmts(), expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))) },
            external_kind: ExternalKind::Normal,
        };
        emit_fn(chunk_func);

        running_done_acc = out_step.done_acc;
        running_next_pc = out_step.next_pc.clone();
        running_next_state = out_step.next_state.clone();
        running_ret_vals = out_step.ret_vals.clone();
        lo = hi;
        chunk_idx += 1;
    }

    // ---- Finish: whatever remains after the accumulation phase ----
    let finish_start = (accum_info.steps.last().expect("boundary is non-empty (asserted above)").end - num_params as u32) as usize;
    let finish_end = block.stmts.len();
    let finish_stmts = &block.stmts[finish_start..finish_end];
    let finish_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads_range(finish_stmts, types)
    } else { 0 };
    let finish_ext = count_external_primitives_range(finish_stmts, types);
    let finish_and_count = count_ir_ands_no_storage_range(finish_stmts, types);

    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
    ];
    if finish_and_count > 0 {
        params.push(IrParam { name: "hat".into(), ty: hat_array_type(finish_and_count) });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    params.extend(w_params.iter().filter(|p| matches!(p.ty, IrType::Array { .. })).cloned());
    for j in 0..finish_oracle_reads {
        params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
    }
    for (k, call) in finish_ext.oracle_calls.iter().enumerate() {
        for j in 0..call.total_bits { params.push(IrParam { name: format!("q_ext_oracle_{}_bit_{}", k, j), ty: q_type() }); }
    }
    for (k, call) in finish_ext.action_calls.iter().enumerate() {
        for j in 0..call.total_bits { params.push(IrParam { name: format!("q_ext_action_{}_bit_{}", k, j), ty: q_type() }); }
    }
    for (r, &width) in finish_ext.rng_widths.iter().enumerate() {
        for j in 0..width { params.push(IrParam { name: format!("q_ext_rng_{}_bit_{}", r, j), ty: q_type() }); }
    }
    // `bind_running`'s READ side can now register a pooled (`_synth_pool`)
    // read for the incoming running accumulator -- see the matching
    // Prover-role comment.
    params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(q_type(), true) });
    params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
    // Phase B: unconditional, same reasoning as `_synth_pool`.
    params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
    params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });

    let mut ctx = VoleIrCtx::new_qsim();
    insert_w_wires(&mut ctx);
    bind_running(&mut ctx, &mut params, "in", running_done_acc, &running_next_pc, &running_next_state, &running_ret_vals);

    debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);
    ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
    ctx.emit_circuit_stmts_range(block, types, mode, finish_start..finish_end);

    let local_entry_count = ctx.trace.entries.len() as u32;
    for mut e in ctx.trace.entries.clone() {
        e.timestamp += global_ts;
        overall_trace_entries.push(e);
    }
    global_ts += local_entry_count.max(ctx.mem_timestamp);
    let _ = global_ts;

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("expected Jmp(Return)"),
    };
    let output_ty = if ret_args.len() == 1 {
        ctx.slot_type(&ret_args[0], &q_type())
    } else {
        IrType::Tuple(ret_args.iter().map(|v| ctx.slot_type(v, &q_type())).collect())
    };
    let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|h| wire_ref_expr(h)).collect()));
    let ret_type = IrType::Tuple(vec![output_ty, q_and_array_type(ctx.q_and_names.len())]);
    let output_expr = if ret_args.len() == 1 {
        ctx.slot_expr(&ret_args[0])
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| ctx.slot_expr(v)).collect()))
    };
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![output_expr, hats_expr]));

    let finish_func = IrFunction { no_inline: true,
        name: format!("vole_qsim_ir_{}_finish", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };
    emit_fn(finish_func);

    MemoryTrace { entries: overall_trace_entries }
}

/// One original block's exported interface, as seen from the combiner: the
/// return type/expr pieces the block's own woven function produces, plus
/// enough shape info (width per `next_state`/`ret_vals` slot) for the
/// combiner to declare matching incoming params.
struct SplitBlockInterface {
    is_active_ty: IrType,
    done_ty: IrType,
    next_pc_bit_tys: Vec<IrType>,
    next_state_tys: Vec<IrType>,
    ret_val_tys: Vec<IrType>,
}

/// Milestone 1.5 Step B: as [`weave_vole_verifier_ir_with_mode_and_trace`],
/// but split into one Rust function per original (pre-movfuscation) block
/// (per `boundary`, from `movfuscate_ir_with_boundary`), plus a *chunked*
/// accumulator chain, plus one trailing "finish" function -- no single
/// function ever declares an `and_count`-sized param list, **and no single
/// function ever needs every block's exported state at once** (found to
/// still happen with a single combiner on the real interpreter circuit:
/// 120 blocks' worth of exported state pushed its param count to ~2.5M --
/// see `docs/agent-context/circuit-size-optimization-backlog.md`).
///
/// The accumulation phase (`Σ_i is_active_i · x_i`, `accum_info` from
/// `movfuscate_ir_with_boundary`) is folded in groups of `chunk_size`
/// blocks: each chunk-function takes only its own `chunk_size` blocks'
/// exported state plus the *previous* chunk's running accumulator
/// (`done_acc`/`next_pc`/`next_state`/`ret_vals`), and returns the updated
/// running accumulator -- turning "one function needs all N blocks' state"
/// into "each of `⌈N/chunk_size⌉` functions needs `chunk_size` blocks'
/// state plus one small running total". The final "finish" function
/// consumes the last chunk's running accumulator and processes whatever
/// stmts remain after the accumulation phase (`lower_to_circuit_ir`'s own
/// terminator-select/padding logic) to produce the final
/// `(output, all_ok, fold_state)`.
///
/// Each block function's own params are bounded by that block's own
/// AND-gate/oracle-read count; each chunk/finish function's params are
/// bounded by `chunk_size × state_width` (plus that chunk's own, typically
/// small, AND-gate count), never by `n_blocks` or `and_count`.
///
/// `emit_fn` is called once per generated function, in order (block 0,
/// block 1, ..., block `n-1`, then each accumulator chunk in order, then
/// finish) -- callers should print and drop each one (Step B.4) before the
/// next call returns, so peak codegen memory is bounded by one function's
/// own size, not the whole circuit's. Correspondingly, at *runtime*,
/// callers drive these the same way: call block function `i`'s prover and
/// verifier counterparts, fold `all_ok`/`fold_state`, discard that block's
/// `hat`s, then move to block `i+1`, then likewise through the chunk
/// functions -- see Milestone 1.5's own
/// `docs/agent-context/circuit-size-optimization-backlog.md` investigation
/// for why this needs no `volar-net`/streaming abstraction: ordinary
/// sequential Rust calls already bound both codegen and runtime peak
/// memory to one function's own size.
///
/// Requires `boundary`/`accum_info` to be non-empty and to come from
/// `movfuscate_ir_with_boundary` on the *same* circuit (only valid for
/// `limit == 1` in the `lower_to_circuit_ir` call that produced `circuit`
/// -- see [`MovfuscBlockBoundary`]'s own doc). `chunk_size` is clamped to
/// at least 1.
pub fn weave_vole_verifier_ir_split_with_trace(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
    sink: &dyn VerifierTraceSink<()>,
    boundary: &[volar_ir_passes::MovfuscBlockBoundary],
    accum_info: &volar_ir_passes::MovfuscAccumInfo,
    chunk_size: usize,
    max_stmts_per_piece: usize,
    mut emit_fn: impl FnMut(IrFunction),
) -> MemoryTrace {
    assert!(circuit.is_circuit(), "weave_vole_verifier_ir_split_with_trace: circuit must satisfy is_circuit()");
    assert!(!boundary.is_empty(), "weave_vole_verifier_ir_split_with_trace: boundary must be non-empty");
    assert_eq!(accum_info.steps.len(), boundary.len(), "accum_info must come from the same movfuscate_ir_with_boundary call as boundary");
    let chunk_size = chunk_size.max(1);
    let block = &circuit.blocks[0];
    let num_params = block.params.len();

    let (generics, base_where_clause) = verifier_generics_and_where();
    let where_clause_for = |sink: &dyn VerifierTraceSink<()>| -> Vec<IrWherePredicate> {
        let mut wc = base_where_clause.clone();
        if let Some(trait_name) = sink.fold_lift_trait_name() {
            if let Some(IrWherePredicate::TypeBound { bounds, .. }) = wc
                .iter_mut()
                .find(|p| matches!(p, IrWherePredicate::TypeBound { ty: IrType::TypeParam(n), .. } if n == "T"))
            {
                bounds.push(IrTraitBound {
                    trait_kind: TraitKind::Custom(trait_name.into()),
                    type_args: vec![],
                    assoc_bindings: vec![],
                });
            }
        }
        wc
    };

    // Shared entry-state params (`w_i`), identical across every block
    // function and the combiner -- every block reads the *same* incoming
    // state, per movfuscation's own "every handler sees the shared state"
    // design.
    let mut w_params: Vec<IrParam> = Vec::new();
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            w_params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
        } else {
            w_params.push(IrParam { name: format!("w_{}", i), ty: wide_array_type(q_type(), w) });
        }
    }
    let insert_w_wires = |ctx: &mut VoleIrCtx| {
        for i in 0..num_params {
            let w = cir_type_width(&block.params[i], types);
            if w <= 1 {
                // Phase B: scalar top-level params are pooled via
                // `_w_pool`, keyed by raw param index (0..num_params,
                // disjoint from `_synth_pool`'s own var-id space -- see
                // `_w_pool`'s own declaration site for why it needs a
                // separate, loop-persistent pool rather than reusing
                // `_synth_pool`). No more per-value `w_i` named param for
                // these; `used_w`/`piece_used_w` no longer gate them
                // either (harmless to keep computing -- see the matching
                // comment at every `w_params` filter site).
                ctx.wires.insert(i as u32, WireRepr::Pooled("_w_pool", i));
            } else {
                // Lazy `WireRepr::Array`, not eagerly unpacked -- see its
                // own doc. With top-level parameter threading, `num_params`
                // covers every state slot, and this closure runs once per
                // (block/chunk/finish) split-weave function per role
                // (~241 x 3): eagerly unpacking every wide param into `w`
                // bound locals here, regardless of whether a given
                // function ever references them, is exactly what drove
                // `print_weaved_vole_module` to 13GB+ RSS printing the
                // real interpreter's full split-weave module.
                ctx.wires.insert(i as u32, WireRepr::Array(format!("w_{}", i), w));
            }
        }
    };

    // Pre-init trace entries: pure metadata (Commitment mode only touches
    // real wires for the fresh `oracle_rd_k` params of each real
    // `StorageRead`, per `emit_storage_read_committed` -- these constant
    // pre-init wires are never read back by anything), so replicate just
    // the numeric var-id/entry bookkeeping `emit_circuit`'s pre_init pass
    // does, with no ctx/ctx.stmts needed. Var ids mirror the original
    // scheme (`p + n_stmts` onward) so they stay meaningful against
    // `circuit.blocks[0]` for anything cross-referencing them.
    let mut overall_trace_entries: Vec<MemoryTraceEntry> = Vec::new();
    let mut global_ts: u32 = 0;
    if matches!(mode, StorageMode::Commitment) && !circuit.pre_init.is_empty() {
        let n_stmts = block.stmts.len() as u32;
        let mut syn_id = num_params as u32 + n_stmts;
        for seg in &circuit.pre_init {
            let sid = seg.storage.0;
            let tid = seg.ty.0;
            for (local, _c) in seg.data.iter().enumerate() {
                let _ci = seg.offset + local;
                let val_id = syn_id; syn_id += 1;
                let addr_id = syn_id; syn_id += 1;
                overall_trace_entries.push(MemoryTraceEntry {
                    addr_var: addr_id,
                    value_var: val_id,
                    storage_id: sid,
                    type_id: tid,
                    is_write: true,
                    timestamp: global_ts,
                });
                global_ts += 1;
            }
        }
    }

    // `movfuscate`'s core loop emits a handful of statements *before* the
    // per-block loop starts (currently just the shared `bit_zero` constant
    // used for the PC/done accumulators) -- these var ids sit outside every
    // block's own `MovfuscBlockBoundary` range, but a block's own
    // terminator-handling stmts (e.g. a "done = 0" / "next_state padding"
    // that reuses the shared zero rather than emitting its own) can
    // reference them. Every ctx (each block's own, and the combiner's)
    // needs this shared prefix processed first so those references
    // resolve, exactly as the unsplit weave's single continuous pass would
    // have had them already defined.
    let shared_prefix: core::ops::Range<usize> = 0..(boundary[0].start as usize - num_params);

    // Bind an "incoming" var (a block's exported var, or a chunk's incoming
    // running-accumulator var) at its *original* var id to a fresh param,
    // so any later stmt referencing that var id (in a block's own
    // terminator handling, or the accumulation phase) resolves exactly as
    // it would have in the unsplit circuit.
    fn bind_scalar(ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, var_id: u32, base_name: String, ty: IrType) {
        match &ty {
            IrType::Array { elem, len: volar_compiler::ir::ArrayLength::Const(n), .. } => {
                // Array-batched on the *param* side (one array param, not
                // `n` scalar params — same 65535-arg-limit reason as
                // `hat`/`q_and`/`r_and`/`w_i`). Kept as a lazy
                // `WireRepr::Array` rather than eagerly unpacked into `n`
                // bound locals -- see `WireRepr::Array`'s own doc; any
                // consumer needing real names (`Merge`, address
                // composition, the unrolled `Poly` fallback) materializes
                // on demand.
                params.push(IrParam { name: base_name.clone(), ty: wide_array_type((**elem).clone(), *n) });
                ctx.wires.insert(var_id, WireRepr::Array(base_name, *n));
            }
            _ => {
                params.push(IrParam { name: base_name.clone(), ty });
                ctx.wires.insert(var_id, WireRepr::Scalar(base_name));
            }
        }
    }

    /// As [`bind_scalar`], but for a value this function may instead read
    /// straight from `_synth_pool` (a block-boundary export or running-
    /// accumulator input, keyed by its own raw circuit var id, exactly
    /// like `synthetic_in` -- see `export_scalar_or_tuple`'s own doc for
    /// why one shared pool safely covers every category). Pooling here
    /// must agree with whatever decision the *producer* made for this
    /// same var id -- both sides derive it identically, from the var's
    /// own type alone, so they can never disagree.
    fn bind_scalar_or_pool(ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, var_id: u32, base_name: String, ty: IrType) {
        if !matches!(ty, IrType::Array { .. }) {
            ctx.wires.insert(var_id, WireRepr::Pooled("_synth_pool", var_id as usize));
            return;
        }
        bind_scalar(ctx, params, var_id, base_name, ty);
    }

    let mut interfaces: Vec<SplitBlockInterface> = Vec::with_capacity(boundary.len());

    // As `weave_vole_prover_ir_split`'s own `synthetic_types` -- see its
    // doc comment for the full rationale.
    let mut synthetic_types: alloc::collections::BTreeMap<u32, IrType> = alloc::collections::BTreeMap::new();

    for (i, b) in boundary.iter().enumerate() {
        let start = (b.start - num_params as u32) as usize;
        let end = (b.end - num_params as u32) as usize;
        let local_stmts = &block.stmts[start..end];
        let local_and_count = count_ir_ands_no_storage_range(local_stmts, types);
        let local_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(local_stmts, types)
        } else { 0 };
        let local_ext = count_external_primitives_range(local_stmts, types);

        // See the matching comment in `weave_vole_prover_ir_split`'s own
        // block loop -- only declare the `w_i` this block function
        // actually references.
        let mut used_w: alloc::collections::BTreeSet<u32> = collect_used_top_level_params(&block.stmts[shared_prefix.clone()], num_params);
        used_w.extend(collect_used_top_level_params(local_stmts, num_params));
        for &v in core::iter::once(&b.is_active).chain(core::iter::once(&b.done))
            .chain(b.next_pc_bits.iter()).chain(b.next_state.iter()).chain(b.ret_vals.iter())
        {
            if (v as usize) < num_params { used_w.insert(v); }
        }

        // See `weave_vole_prover_ir_split`'s own doc for the general
        // design/rationale. The verifier ALSO needs `q_and`/`hat`/`r_and`
        // sliced per piece (three parallel and-count-sized arrays,
        // instead of qsim's single `hat`), AND a running `all_ok`/
        // `fold_state` accumulator chain threaded through every piece IN
        // ORDER (piece 0 seeded from the wrapper's own `all_ok_in`/
        // `fold_state_in`, each later piece seeded from the PRECEDING
        // piece's own final `all_ok`/`fold_state`, the wrapper's own
        // final values taken from the LAST piece) -- unlike
        // `is_active`/`done`/etc, which route to whichever piece
        // produces them, `all_ok`/`fold_state` are mutated by every
        // piece's own emission and so must flow through ALL of them,
        // mirroring the SAME pattern `split_driver.rs` already uses one
        // level up (between CHUNKS), just applied one level deeper
        // (between a single chunk's own split-out pieces).
        let can_split = local_ext.oracle_calls.is_empty() && local_ext.action_calls.is_empty() && local_ext.rng_widths.is_empty();
        let mut region_outputs: Vec<u32> = alloc::vec![b.is_active, b.done];
        region_outputs.extend(b.next_pc_bits.iter().copied());
        region_outputs.extend(b.next_state.iter().copied());
        region_outputs.extend(b.ret_vals.iter().copied());
        region_outputs.extend(b.synthetic_out.iter().copied());
        let pieces = if can_split {
            crate::vole_split::split_region_into_pieces(block, num_params, b.start, b.end, &region_outputs, max_stmts_per_piece)
        } else {
            alloc::vec![crate::vole_split::PieceSpec { start: b.start, end: b.end, extra_in: Vec::new(), extra_out: Vec::new() }]
        };

        if pieces.len() <= 1 {
            // ---- Unchanged: single function ----
            let mut params: Vec<IrParam> = vec![
                IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
            ];
            if local_and_count > 0 {
                params.push(IrParam { name: "q_and".into(), ty: q_and_array_type(local_and_count) });
                params.push(IrParam { name: "hat".into(), ty: hat_array_type(local_and_count) });
                params.push(IrParam {
                    name: "r_and".into(),
                    ty: r_and_array_type(local_and_count, sink.fold_scalar_type_name()),
                });
            }
            params.push(IrParam { name: "q_one".into(), ty: q_type() });
            params.extend(w_params.iter().enumerate().filter(|(idx, p)| used_w.contains(&(*idx as u32)) && matches!(p.ty, IrType::Array { .. })).map(|(_, p)| p.clone()));
            for j in 0..local_oracle_reads {
                params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
            }
            for (k, call) in local_ext.oracle_calls.iter().enumerate() {
                for j in 0..call.total_bits {
                    params.push(IrParam { name: format!("q_ext_oracle_{}_bit_{}", k, j), ty: q_type() });
                }
            }
            for (k, call) in local_ext.action_calls.iter().enumerate() {
                for j in 0..call.total_bits {
                    params.push(IrParam { name: format!("q_ext_action_{}_bit_{}", k, j), ty: q_type() });
                }
            }
            for (r, &width) in local_ext.rng_widths.iter().enumerate() {
                for j in 0..width {
                    params.push(IrParam { name: format!("q_ext_rng_{}_bit_{}", r, j), ty: q_type() });
                }
            }
            params.push(IrParam { name: "all_ok_in".into(), ty: IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool) });
            params.push(IrParam { name: "fold_state_in".into(), ty: IrType::TypeParam(sink.state_type_name().into()) });
            // Unconditional (not just when synthetic_in/out are non-empty):
            // block-boundary exports (is_active/done/next_pc/next_state/
            // ret_vals) are ALSO pooled now, and every block has at least
            // an is_active+done pair, so this is needed almost always in
            // practice anyway -- an unused pool param on the rare block
            // that somehow needs none of this is harmless (the generated
            // preamble already has #![allow(unused_variables, ...)]).
            let synth_pool_needed = true;
            if synth_pool_needed {
                params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(q_type(), true) });
                params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                // Phase B: unconditional, same reasoning as `_synth_pool`.
                params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
                params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            }

            let mut ctx = VoleIrCtx::new_verifier_with_trace_sink(sink);
            insert_w_wires(&mut ctx);
            for &v in &b.synthetic_in {
                let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                    "weave_vole_verifier_ir_split_with_trace: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                ));
                if !matches!(ty, IrType::Array { .. }) {
                    ctx.wires.insert(v, WireRepr::Pooled("_synth_pool", v as usize));
                    continue;
                }
                bind_scalar(&mut ctx, &mut params, v, format!("synth_{v}"), ty);
            }
            ctx.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
                ty: None,
                init: Some(var("all_ok_in")),
            }));
            ctx.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::Ident { mutable: true, name: "fold_state".into(), subpat: None },
                ty: None,
                init: Some(var("fold_state_in")),
            }));
            // Shared pre-loop prefix (currently just `bit_zero`) first, so any
            // reference to it from this block's own terminator-handling stmts
            // resolves. Assumed gate/oracle-free (see `local_and_count`/
            // `local_oracle_reads`, computed from `local_stmts` alone, above).
            debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);
            ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
            ctx.emit_circuit_stmts_range(block, types, mode, start..end);

            // Rebase this block's own (locally-zeroed) trace timestamps onto
            // the running global offset before merging.
            let local_entry_count = ctx.trace.entries.len() as u32;
            for mut e in ctx.trace.entries.clone() {
                e.timestamp += global_ts;
                overall_trace_entries.push(e);
            }
            global_ts += local_entry_count.max(ctx.mem_timestamp);

            let is_active_v = CirVar(b.is_active);
            let done_v = CirVar(b.done);
            let is_active_expr = ctx.slot_expr(&is_active_v);
            let done_expr = ctx.slot_expr(&done_v);
            let is_active_ty = ctx.slot_type(&is_active_v, &q_type());
            let done_ty = ctx.slot_type(&done_v, &q_type());
            let next_pc_exprs: Vec<IrExpr> = b.next_pc_bits.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let next_pc_bit_tys: Vec<IrType> = b.next_pc_bits.iter().map(|&v| ctx.slot_type(&CirVar(v), &q_type())).collect();
            let next_state_exprs: Vec<IrExpr> = b.next_state.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let next_state_tys: Vec<IrType> = b.next_state.iter().map(|&v| ctx.slot_type(&CirVar(v), &q_type())).collect();
            let ret_val_exprs: Vec<IrExpr> = b.ret_vals.iter().map(|&v| ctx.slot_expr(&CirVar(v))).collect();
            let ret_val_tys: Vec<IrType> = b.ret_vals.iter().map(|&v| ctx.slot_type(&CirVar(v), &q_type())).collect();

            // Block-boundary exports: pool scalar ones directly via
            // `_synth_pool` -- see `export_scalar_or_tuple`'s own doc.
            // Verifier has no `hats` array (all_ok/fold_state trail
            // instead, unconditionally, so its own return tuple can never
            // shrink to a single element even if every export pools).
            let mut ret_tuple_tys: Vec<IrType> = Vec::new();
            let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
            export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.is_active, is_active_ty.clone(), is_active_expr);
            export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.done, done_ty.clone(), done_expr);
            for (j, expr) in next_pc_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_pc_bits[j], next_pc_bit_tys[j].clone(), expr);
            }
            for (k, expr) in next_state_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_state[k], next_state_tys[k].clone(), expr);
            }
            for (m, expr) in ret_val_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut ctx.stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.ret_vals[m], ret_val_tys[m].clone(), expr);
            }
            ret_tuple_tys.push(IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool));
            ret_tuple_tys.push(IrType::TypeParam(sink.state_type_name().into()));
            ret_tuple_exprs.push(var("all_ok"));
            ret_tuple_exprs.push(var("fold_state"));

            // As `weave_vole_prover_ir_split`'s own synthetic_out handling.
            for &v in &b.synthetic_out {
                let ty = ctx.slot_type(&CirVar(v), &q_type());
                synthetic_types.entry(v).or_insert_with(|| ty.clone());
                if !matches!(ty, IrType::Array { .. }) {
                    let slot_str = v.to_string();
                    let value = ctx.slot_expr(&CirVar(v));
                    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool", &slot_str)),
                        right: Box::new(value),
                    }))));
                    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                        right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                    }))));
                    continue;
                }
                ret_tuple_tys.push(ty);
                ret_tuple_exprs.push(ctx.slot_expr(&CirVar(v)));
            }

            let func = IrFunction { no_inline: true,
                name: format!("vole_verify_ir_{}_block_{}", name, i),
                module_path: vec![],
                generics: generics.clone(),
                receiver: None,
                params,
                return_type: Some(IrType::Tuple(ret_tuple_tys)),
                where_clause: where_clause_for(sink),
                body: IrBlock {
                    stmts: ctx.finalize_and_take_stmts(),
                    expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
                },
                external_kind: ExternalKind::Normal,
            };
            emit_fn(func);

            interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
        } else {
            // ---- Split: K piece functions + 1 wrapper. See
            // `weave_vole_prover_ir_split`'s own "Split:" branch for the
            // base design; deltas here: `q_and`/`hat`/`r_and` (three
            // and-count-sized arrays, all sliced together using the same
            // per-piece and-count and offset) instead of qsim's single
            // `hat`, and the `all_ok`/`fold_state` running chain threaded
            // through every piece in order (see the doc above the
            // `can_split` computation).
            debug_assert_eq!(count_ir_ands_no_storage_range(&block.stmts[shared_prefix.clone()], types), 0);

            let mut producer_piece: alloc::collections::BTreeMap<u32, usize> = alloc::collections::BTreeMap::new();
            for (p, piece) in pieces.iter().enumerate() {
                for &v in &piece.extra_out {
                    producer_piece.entry(v).or_insert(p);
                }
            }

            // ---- Phase C sub-stage 1: pool scalar cross-piece
            // ("piece_in_v") values -- see the identical Prover-side
            // design doc in `weave_vole_prover_ir_split`.
            let all_extra_in_vars: alloc::collections::BTreeSet<u32> =
                pieces.iter().flat_map(|pc| pc.extra_in.iter().copied()).collect();
            let mut pool_slot: alloc::collections::BTreeMap<u32, usize> = alloc::collections::BTreeMap::new();
            let region_has_cross_piece_vars = pieces.iter().any(|pc| !pc.extra_in.is_empty() || !pc.extra_out.is_empty());

            let mut piece_names: Vec<String> = Vec::with_capacity(pieces.len());
            let mut piece_used_w: Vec<alloc::collections::BTreeSet<u32>> = Vec::with_capacity(pieces.len());
            let mut piece_oracle_counts: Vec<usize> = Vec::with_capacity(pieces.len());
            let mut piece_and_counts: Vec<usize> = Vec::with_capacity(pieces.len());
            let mut piece_out_types: alloc::collections::BTreeMap<u32, IrType> = alloc::collections::BTreeMap::new();

            for (p, piece) in pieces.iter().enumerate() {
                let p_start = (piece.start - num_params as u32) as usize;
                let p_end = (piece.end - num_params as u32) as usize;
                let p_stmts = &block.stmts[p_start..p_end];
                let p_and_count = count_ir_ands_no_storage_range(p_stmts, types);
                let p_oracle_reads = if matches!(mode, StorageMode::Commitment) {
                    count_storage_reads_range(p_stmts, types)
                } else { 0 };

                let mut p_used_w: alloc::collections::BTreeSet<u32> = collect_used_top_level_params(&block.stmts[shared_prefix.clone()], num_params);
                p_used_w.extend(collect_used_top_level_params(p_stmts, num_params));
                for &v in &piece.extra_out {
                    if (v as usize) < num_params { p_used_w.insert(v); }
                }

                let mut p_params: Vec<IrParam> = vec![
                    IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
                ];
                if p_and_count > 0 {
                    p_params.push(IrParam { name: "q_and".into(), ty: q_and_array_type(p_and_count) });
                    p_params.push(IrParam { name: "hat".into(), ty: hat_array_type(p_and_count) });
                    p_params.push(IrParam {
                        name: "r_and".into(),
                        ty: r_and_array_type(p_and_count, sink.fold_scalar_type_name()),
                    });
                }
                p_params.push(IrParam { name: "q_one".into(), ty: q_type() });
                p_params.extend(w_params.iter().enumerate().filter(|(idx, pr)| p_used_w.contains(&(*idx as u32)) && matches!(pr.ty, IrType::Array { .. })).map(|(_, pr)| pr.clone()));
                for j in 0..p_oracle_reads {
                    p_params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
                }
                p_params.push(IrParam { name: "all_ok_in".into(), ty: IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool) });
                p_params.push(IrParam { name: "fold_state_in".into(), ty: IrType::TypeParam(sink.state_type_name().into()) });
                if region_has_cross_piece_vars {
                    p_params.push(IrParam { name: "_piece_pool".into(), ty: pool_slice_type(q_type(), true) });
                    p_params.push(IrParam { name: "_piece_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                }
                // Phase B: unconditional, same reasoning as every other
                // `_w_pool` declaration site -- a piece may reference any
                // scalar top-level param directly.
                p_params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
                p_params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });

                let mut ctx = VoleIrCtx::new_verifier_with_trace_sink(sink);
                insert_w_wires(&mut ctx);
                for &v in &b.synthetic_in {
                    let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                        "weave_vole_verifier_ir_split_with_trace: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                    ));
                    bind_scalar(&mut ctx, &mut p_params, v, format!("synth_{v}"), ty);
                }
                for &v in &piece.extra_in {
                    if let Some(&slot) = pool_slot.get(&v) {
                        ctx.wires.insert(v, WireRepr::Pooled("_piece_pool", slot));
                        continue;
                    }
                    let ty = piece_out_types.get(&v).cloned().unwrap_or_else(|| panic!(
                        "weave_vole_verifier_ir_split_with_trace: piece-local var {v} has no known type -- its own producing piece must run before this consuming piece"
                    ));
                    bind_scalar(&mut ctx, &mut p_params, v, format!("piece_in_{v}"), ty);
                }
                ctx.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
                    ty: None,
                    init: Some(var("all_ok_in")),
                }));
                ctx.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::Ident { mutable: true, name: "fold_state".into(), subpat: None },
                    ty: None,
                    init: Some(var("fold_state_in")),
                }));

                ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
                ctx.emit_circuit_stmts_range(block, types, mode, p_start..p_end);

                let local_entry_count = ctx.trace.entries.len() as u32;
                for mut e in ctx.trace.entries.clone() {
                    e.timestamp += global_ts;
                    overall_trace_entries.push(e);
                }
                global_ts += local_entry_count.max(ctx.mem_timestamp);

                let mut p_ret_tys: Vec<IrType> = Vec::with_capacity(piece.extra_out.len() + 2);
                let mut p_ret_exprs: Vec<IrExpr> = Vec::with_capacity(piece.extra_out.len() + 2);
                for &v in &piece.extra_out {
                    let ty = ctx.slot_type(&CirVar(v), &q_type());
                    let is_scalar = !matches!(ty, IrType::Array { .. });
                    if is_scalar && all_extra_in_vars.contains(&v) {
                        if pool_slot.contains_key(&v) {
                            // The first producer owns the pool slot; later
                            // pass-through pieces reuse its mapping.
                            piece_out_types.insert(v, ty);
                            continue;
                        }
                        let slot = pool_slot.len();
                        pool_slot.insert(v, slot);
                        let slot_str = slot.to_string();
                        let value = ctx.slot_expr(&CirVar(v));
                        ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                            left: Box::new(arr_index("_piece_pool", &slot_str)),
                            right: Box::new(value),
                        }))));
                        ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                            left: Box::new(arr_index("_piece_pool_written", &slot_str)),
                            right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                        }))));
                        piece_out_types.insert(v, ty);
                        continue;
                    }
                    p_ret_exprs.push(ctx.slot_expr(&CirVar(v)));
                    piece_out_types.insert(v, ty.clone());
                    p_ret_tys.push(ty);
                }
                p_ret_tys.push(IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool));
                p_ret_tys.push(IrType::TypeParam(sink.state_type_name().into()));
                p_ret_exprs.push(var("all_ok"));
                p_ret_exprs.push(var("fold_state"));

                let p_name = format!("vole_verify_ir_{}_block_{}_piece_{}", name, i, p);
                let p_func = IrFunction { no_inline: true,
                    name: p_name.clone(),
                    module_path: vec![],
                    generics: generics.clone(),
                    receiver: None,
                    params: p_params,
                    return_type: Some(IrType::Tuple(p_ret_tys)),
                    where_clause: where_clause_for(sink),
                    body: IrBlock {
                        stmts: ctx.finalize_and_take_stmts(),
                        expr: Some(Box::new(ir_expr(IrExprKind::Tuple(p_ret_exprs)))),
                    },
                    external_kind: ExternalKind::Normal,
                };
                emit_fn(p_func);

                piece_names.push(p_name);
                piece_used_w.push(p_used_w);
                piece_oracle_counts.push(p_oracle_reads);
                piece_and_counts.push(p_and_count);
            }

            let mut wrapper_params: Vec<IrParam> = vec![
                IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
            ];
            if local_and_count > 0 {
                wrapper_params.push(IrParam { name: "q_and".into(), ty: q_and_array_type(local_and_count) });
                wrapper_params.push(IrParam { name: "hat".into(), ty: hat_array_type(local_and_count) });
                wrapper_params.push(IrParam {
                    name: "r_and".into(),
                    ty: r_and_array_type(local_and_count, sink.fold_scalar_type_name()),
                });
            }
            wrapper_params.push(IrParam { name: "q_one".into(), ty: q_type() });
            wrapper_params.extend(w_params.iter().enumerate().filter(|(idx, p)| used_w.contains(&(*idx as u32)) && matches!(p.ty, IrType::Array { .. })).map(|(_, p)| p.clone()));
            for j in 0..local_oracle_reads {
                wrapper_params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
            }
            // `all_ok_in`/`fold_state_in` BEFORE `synth_*` -- matches the
            // original unsplit function's own external param order
            // (`params.push(all_ok_in/fold_state_in)` happens before the
            // `synthetic_in` binding loop there), which this wrapper's
            // own signature must preserve exactly (external callers like
            // `split_driver.rs` depend on it).
            wrapper_params.push(IrParam { name: "all_ok_in".into(), ty: IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool) });
            wrapper_params.push(IrParam { name: "fold_state_in".into(), ty: IrType::TypeParam(sink.state_type_name().into()) });
            // Unconditional (not just when synthetic_in/out are non-empty):
            // block-boundary exports (is_active/done/next_pc/next_state/
            // ret_vals) are ALSO pooled now, and every block has at least
            // an is_active+done pair, so this is needed almost always in
            // practice anyway -- an unused pool param on the rare block
            // that somehow needs none of this is harmless (the generated
            // preamble already has #![allow(unused_variables, ...)]).
            let synth_pool_needed = true;
            if synth_pool_needed {
                wrapper_params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(q_type(), true) });
                wrapper_params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
                // Phase B: unconditional, same reasoning as `_synth_pool`.
                wrapper_params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
                wrapper_params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            }
            for &v in &b.synthetic_in {
                let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                    "weave_vole_verifier_ir_split_with_trace: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
                ));
                if matches!(ty, IrType::Array { .. }) {
                    wrapper_params.push(IrParam { name: format!("synth_{v}"), ty });
                }
            }

            let mut wrapper_stmts: Vec<IrStmt> = Vec::with_capacity(pieces.len() + 2);
            if region_has_cross_piece_vars {
                wrapper_stmts.push(pool_decl_stmt("_piece_pool", q_type(), q_default_call(), pool_slot.len()));
                wrapper_stmts.push(pool_decl_stmt("_piece_pool_written", IrType::Primitive(PrimitiveType::Bool), ir_expr(IrExprKind::Lit(IrLit::Bool(false))), pool_slot.len()));
            }
            let mut oracle_offset = 0usize;
            let mut and_offset = 0usize;
            for (p, piece) in pieces.iter().enumerate() {
                // `delta` is `&Delta<N,T>` -- a reference, trivially
                // `Copy` regardless of whether the pointee is -- cloning
                // it (`delta.clone()`) would auto-deref through to
                // `Delta::clone`, producing an OWNED `Delta<N,T>` where
                // every piece call expects `&Delta<N,T>`. No `.clone()`
                // needed at all, unlike the genuinely-owned non-`Copy`
                // wire values below.
                let mut call_args: Vec<IrExpr> = vec![var("delta")];
                if piece_and_counts[p] > 0 {
                    let q_and_slice: Vec<IrExpr> = (0..piece_and_counts[p])
                        .map(|j| clone_expr(arr_index("q_and", &(and_offset + j).to_string())))
                        .collect();
                    call_args.push(ir_expr(IrExprKind::FixedArray(q_and_slice)));
                    let hat_slice: Vec<IrExpr> = (0..piece_and_counts[p])
                        .map(|j| clone_expr(arr_index("hat", &(and_offset + j).to_string())))
                        .collect();
                    call_args.push(ir_expr(IrExprKind::FixedArray(hat_slice)));
                    let r_and_slice: Vec<IrExpr> = (0..piece_and_counts[p])
                        .map(|j| clone_expr(arr_index("r_and", &(and_offset + j).to_string())))
                        .collect();
                    call_args.push(ir_expr(IrExprKind::FixedArray(r_and_slice)));
                }
                and_offset += piece_and_counts[p];
                call_args.push(clone_expr(var("q_one")));
                for &idx in &piece_used_w[p] {
                    if matches!(w_params[idx as usize].ty, IrType::Array { .. }) {
                        call_args.push(clone_expr(var(&format!("w_{idx}"))));
                    }
                }
                for j in 0..piece_oracle_counts[p] {
                    call_args.push(clone_expr(var(&format!("oracle_rd_{}", oracle_offset + j))));
                }
                oracle_offset += piece_oracle_counts[p];
                // `all_ok_in`/`fold_state_in` BEFORE `synth_*`/`piece_in_*`
                // -- must match each piece's own declared param order,
                // which (mirroring the original unsplit function) pushes
                // these two right after `oracle_rd_*`, before the
                // `synthetic_in`/`extra_in` binding loops. Running
                // all_ok/fold_state chain: piece 0 takes the wrapper's
                // own incoming params; every later piece takes the
                // PRECEDING piece's own output.
                if p == 0 {
                    call_args.push(clone_expr(var("all_ok_in")));
                    call_args.push(clone_expr(var("fold_state_in")));
                } else {
                    call_args.push(clone_expr(var(&format!("piece{}_all_ok", p - 1))));
                    call_args.push(clone_expr(var(&format!("piece{}_fold_state", p - 1))));
                }
                if region_has_cross_piece_vars {
                    call_args.push(slice_ref_mut_expr("_piece_pool"));
                    call_args.push(slice_ref_mut_expr("_piece_pool_written"));
                }
                // Phase B: unconditional, matching every piece's own
                // unconditional `_w_pool`/`_w_pool_written` params.
                call_args.push(slice_ref_mut_expr("_w_pool"));
                call_args.push(slice_ref_mut_expr("_w_pool_written"));
                for &v in &b.synthetic_in {
                    let ty = synthetic_types.get(&v).cloned().expect("synthetic_types must already be populated");
                    if matches!(ty, IrType::Array { .. }) {
                        call_args.push(clone_expr(var(&format!("synth_{v}"))));
                    } else {
                        call_args.push(pooled_read_expr("_synth_pool", v as usize));
                    }
                }
                for &v in &piece.extra_in {
                    if pool_slot.contains_key(&v) { continue; }
                    let producer = producer_piece.get(&v).copied().expect("weave_vole_verifier_ir_split_with_trace: extra_in var must have a producer piece");
                    call_args.push(clone_expr(var(&format!("piece{producer}_v{v}"))));
                }

                let mut pattern_names: Vec<String> = piece.extra_out.iter()
                    .filter(|v| !pool_slot.contains_key(v))
                    .map(|&v| format!("piece{p}_v{v}"))
                    .collect();
                pattern_names.push(format!("piece{p}_all_ok"));
                pattern_names.push(format!("piece{p}_fold_state"));

                wrapper_stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::Tuple(pattern_names.into_iter().map(IrPattern::ident).collect()),
                    ty: None,
                    init: Some(ir_expr(IrExprKind::Call {
                        func: Box::new(ir_expr(IrExprKind::Path {
                            segments: vec![piece_names[p].clone()],
                            type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
                        })),
                        args: call_args,
                    })),
                }));
            }

            let field_expr = |v: u32| match pool_slot.get(&v) {
                Some(&slot) => pooled_read_expr("_piece_pool", slot),
                None => clone_expr(var(&format!("piece{}_v{}", producer_piece[&v], v))),
            };
            let field_ty = |v: u32| piece_out_types[&v].clone();

            let is_active_ty = field_ty(b.is_active);
            let done_ty = field_ty(b.done);
            let is_active_expr = field_expr(b.is_active);
            let done_expr = field_expr(b.done);
            let next_pc_exprs: Vec<IrExpr> = b.next_pc_bits.iter().map(|&v| field_expr(v)).collect();
            let next_pc_bit_tys: Vec<IrType> = b.next_pc_bits.iter().map(|&v| field_ty(v)).collect();
            let next_state_exprs: Vec<IrExpr> = b.next_state.iter().map(|&v| field_expr(v)).collect();
            let next_state_tys: Vec<IrType> = b.next_state.iter().map(|&v| field_ty(v)).collect();
            let ret_val_exprs: Vec<IrExpr> = b.ret_vals.iter().map(|&v| field_expr(v)).collect();
            let ret_val_tys: Vec<IrType> = b.ret_vals.iter().map(|&v| field_ty(v)).collect();

            // Block-boundary exports: pool scalar ones directly via
            // `_synth_pool` -- see `export_scalar_or_tuple`'s own doc.
            let mut ret_tuple_tys: Vec<IrType> = Vec::new();
            let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
            export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.is_active, is_active_ty.clone(), is_active_expr);
            export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.done, done_ty.clone(), done_expr);
            for (j, expr) in next_pc_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_pc_bits[j], next_pc_bit_tys[j].clone(), expr);
            }
            for (k, expr) in next_state_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.next_state[k], next_state_tys[k].clone(), expr);
            }
            for (m, expr) in ret_val_exprs.into_iter().enumerate() {
                export_scalar_or_tuple(&mut wrapper_stmts, &mut ret_tuple_tys, &mut ret_tuple_exprs, b.ret_vals[m], ret_val_tys[m].clone(), expr);
            }
            ret_tuple_tys.push(IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool));
            ret_tuple_tys.push(IrType::TypeParam(sink.state_type_name().into()));

            let last = pieces.len() - 1;
            ret_tuple_exprs.push(clone_expr(var(&format!("piece{last}_all_ok"))));
            ret_tuple_exprs.push(clone_expr(var(&format!("piece{last}_fold_state"))));

            for &v in &b.synthetic_out {
                let ty = field_ty(v);
                synthetic_types.entry(v).or_insert_with(|| ty.clone());
                if !matches!(ty, IrType::Array { .. }) {
                    let slot_str = v.to_string();
                    wrapper_stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool", &slot_str)),
                        right: Box::new(field_expr(v)),
                    }))));
                    wrapper_stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                        right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                    }))));
                    continue;
                }
                ret_tuple_tys.push(ty);
                ret_tuple_exprs.push(field_expr(v));
            }

            let wrapper_func = IrFunction { no_inline: true,
                name: format!("vole_verify_ir_{}_block_{}", name, i),
                module_path: vec![],
                generics: generics.clone(),
                receiver: None,
                params: wrapper_params,
                return_type: Some(IrType::Tuple(ret_tuple_tys)),
                where_clause: where_clause_for(sink),
                body: IrBlock {
                    stmts: wrapper_stmts,
                    expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
                },
                external_kind: ExternalKind::Normal,
            };
            emit_fn(wrapper_func);

            interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
        }
    }

    // ---- Chunked accumulation: fold blocks in groups, never all at once ----
    //
    // Determine the running accumulator's own next_state/ret_vals slot
    // types once, from `accum_info.init`'s zero-allocation stmts (a small,
    // gate-free range) -- done_acc/next_pc are always Bit-scalar (width 1),
    // needing no such lookup.
    let (init_next_state_tys, init_ret_val_tys) = {
        let mut probe_ctx = VoleIrCtx::new_verifier_with_trace_sink(sink);
        // See the matching comment in `weave_vole_prover_ir_split` -- the
        // circuit's own top-level params must be bound here too.
        insert_w_wires(&mut probe_ctx);
        // shared_prefix must be emitted here too -- unconstrained CSE can
        // (and does, in practice: a Bit-typed zero seed statement is a
        // prime dedup target) merge one of accum_info.init's own seed
        // statements onto shared_prefix's own statement, so a reference
        // into accum_info.init's own next_state/ret_vals can legitimately
        // point at a shared_prefix var -- every other ctx in this
        // function already emits shared_prefix before querying anything;
        // this probe was the one place that didn't.
        probe_ctx.emit_circuit_stmts_range(block, types, mode, shared_prefix.clone());
        let init_start = (accum_info.init.start - num_params as u32) as usize;
        let init_end = (accum_info.init.end - num_params as u32) as usize;
        probe_ctx.emit_circuit_stmts_range(block, types, mode, init_start..init_end);
        let next_state_tys: Vec<IrType> = accum_info.init.next_state.iter()
            .map(|&v| probe_ctx.slot_type(&CirVar(v), &q_type())).collect();
        let ret_val_tys: Vec<IrType> = accum_info.init.ret_vals.iter()
            .map(|&v| probe_ctx.slot_type(&CirVar(v), &q_type())).collect();
        (next_state_tys, ret_val_tys)
    };

    let bind_running = |ctx: &mut VoleIrCtx, params: &mut Vec<IrParam>, prefix: &str,
                         done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| {
        bind_scalar_or_pool(ctx, params, done_acc, format!("{prefix}_done_acc"), q_type());
        for (j, &v) in next_pc.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_next_pc_{j}"), q_type());
        }
        for (k, &v) in next_state.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_next_state_{k}"), init_next_state_tys[k].clone());
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            bind_scalar_or_pool(ctx, params, v, format!("{prefix}_ret_val_{m}"), init_ret_val_tys[m].clone());
        }
    };
    // Running-accumulator OUTGOING state: pool scalar ones directly via
    // `_synth_pool` -- see the matching Prover-role comment. `prefix` was
    // already unused by the old `running_tys` (`let _ = prefix;`), so it's
    // dropped here rather than threaded through for no purpose.
    let running_export = |ctx: &mut VoleIrCtx, ret_tuple_tys: &mut Vec<IrType>, ret_tuple_exprs: &mut Vec<IrExpr>,
                           done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| {
        let done_acc_expr = ctx.slot_expr(&CirVar(done_acc));
        export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, done_acc, q_type(), done_acc_expr);
        for &v in next_pc {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, q_type(), expr);
        }
        for (k, &v) in next_state.iter().enumerate() {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, init_next_state_tys[k].clone(), expr);
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            let expr = ctx.slot_expr(&CirVar(v));
            export_scalar_or_tuple(&mut ctx.stmts, ret_tuple_tys, ret_tuple_exprs, v, init_ret_val_tys[m].clone(), expr);
        }
    };

    let n_blocks = boundary.len();
    let mut running_done_acc = accum_info.init.done_acc;
    let mut running_next_pc = accum_info.init.next_pc.clone();
    let mut running_next_state = accum_info.init.next_state.clone();
    let mut running_ret_vals = accum_info.init.ret_vals.clone();

    let mut lo = 0usize;
    let mut chunk_idx = 0usize;
    while lo < n_blocks {
        let hi = (lo + chunk_size).min(n_blocks);
        let chunk_start = (accum_info.steps[lo].start - num_params as u32) as usize;
        let chunk_end = (accum_info.steps[hi - 1].end - num_params as u32) as usize;
        let chunk_stmts = &block.stmts[chunk_start..chunk_end];
        let chunk_and_count = count_ir_ands_no_storage_range(chunk_stmts, types);
        let chunk_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(chunk_stmts, types)
        } else { 0 };
        let chunk_ext = count_external_primitives_range(chunk_stmts, types);

        let mut params: Vec<IrParam> = vec![
            IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
        ];
        if chunk_and_count > 0 {
            params.push(IrParam { name: "q_and".into(), ty: q_and_array_type(chunk_and_count) });
            params.push(IrParam { name: "hat".into(), ty: hat_array_type(chunk_and_count) });
            params.push(IrParam { name: "r_and".into(), ty: r_and_array_type(chunk_and_count, sink.fold_scalar_type_name()) });
        }
        params.push(IrParam { name: "q_one".into(), ty: q_type() });
        params.extend(w_params.iter().filter(|p| matches!(p.ty, IrType::Array { .. })).cloned());
        for j in 0..chunk_oracle_reads {
            params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
        }
        for (k, call) in chunk_ext.oracle_calls.iter().enumerate() {
            for j in 0..call.total_bits { params.push(IrParam { name: format!("q_ext_oracle_{}_bit_{}", k, j), ty: q_type() }); }
        }
        for (k, call) in chunk_ext.action_calls.iter().enumerate() {
            for j in 0..call.total_bits { params.push(IrParam { name: format!("q_ext_action_{}_bit_{}", k, j), ty: q_type() }); }
        }
        for (r, &width) in chunk_ext.rng_widths.iter().enumerate() {
            for j in 0..width { params.push(IrParam { name: format!("q_ext_rng_{}_bit_{}", r, j), ty: q_type() }); }
        }

        let mut ctx = VoleIrCtx::new_verifier_with_trace_sink(sink);
        insert_w_wires(&mut ctx);
        bind_running(&mut ctx, &mut params, "in", running_done_acc, &running_next_pc, &running_next_state, &running_ret_vals);
        for i in lo..hi {
            let b = &boundary[i];
            let iface = &interfaces[i];
            bind_scalar_or_pool(&mut ctx, &mut params, b.is_active, format!("is_active_{i}"), iface.is_active_ty.clone());
            bind_scalar_or_pool(&mut ctx, &mut params, b.done, format!("done_{i}"), iface.done_ty.clone());
            for (j, &v) in b.next_pc_bits.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("next_pc_{i}_{j}"), iface.next_pc_bit_tys[j].clone());
            }
            for (k, &v) in b.next_state.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("next_state_{i}_{k}"), iface.next_state_tys[k].clone());
            }
            for (m, &v) in b.ret_vals.iter().enumerate() {
                bind_scalar_or_pool(&mut ctx, &mut params, v, format!("ret_val_{i}_{m}"), iface.ret_val_tys[m].clone());
            }
        }

        // Unconditional -- see the matching comment at the block-level
        // synth_pool_needed site: block-boundary exports are pooled too now.
        let chunk_synth_pool_needed = true;
        if chunk_synth_pool_needed {
            params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(q_type(), true) });
            params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
            // Phase B: unconditional, same reasoning as `_synth_pool`.
            params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
            params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
        }

        // As `weave_vole_prover_ir_split`'s own `chunk_synth_in` handling.
        let chunk_synth_in: Vec<u32> = accum_info.steps[lo].synthetic_in.clone();
        for &v in &chunk_synth_in {
            let ty = synthetic_types.get(&v).cloned().unwrap_or_else(|| panic!(
                "weave_vole_verifier_ir_split_with_trace: synthetic var {v} has no known type -- its own producer range must run before this consumer in call order"
            ));
            if !matches!(ty, IrType::Array { .. }) {
                ctx.wires.insert(v, WireRepr::Pooled("_synth_pool", v as usize));
                continue;
            }
            bind_scalar(&mut ctx, &mut params, v, format!("synth_{v}"), ty);
        }

        params.push(IrParam { name: "all_ok_in".into(), ty: IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool) });
        params.push(IrParam { name: "fold_state_in".into(), ty: IrType::TypeParam(sink.state_type_name().into()) });

        ctx.stmts.push(ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
            ty: None, init: Some(var("all_ok_in")),
        }));
        ctx.stmts.push(ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::Ident { mutable: true, name: "fold_state".into(), subpat: None },
            ty: None, init: Some(var("fold_state_in")),
        }));
        ctx.emit_circuit_stmts_range(block, types, mode, chunk_start..chunk_end);

        let local_entry_count = ctx.trace.entries.len() as u32;
        for mut e in ctx.trace.entries.clone() {
            e.timestamp += global_ts;
            overall_trace_entries.push(e);
        }
        global_ts += local_entry_count.max(ctx.mem_timestamp);

        let out_step = &accum_info.steps[hi - 1];
        let mut ret_tuple_tys: Vec<IrType> = Vec::new();
        let mut ret_tuple_exprs: Vec<IrExpr> = Vec::new();
        running_export(&mut ctx, &mut ret_tuple_tys, &mut ret_tuple_exprs, out_step.done_acc, &out_step.next_pc, &out_step.next_state, &out_step.ret_vals);
        ret_tuple_tys.push(IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool));
        ret_tuple_tys.push(IrType::TypeParam(sink.state_type_name().into()));
        ret_tuple_exprs.push(var("all_ok"));
        ret_tuple_exprs.push(var("fold_state"));

        // As `weave_vole_prover_ir_split`'s own chunk-level synthetic_out handling.
        for &v in &out_step.synthetic_out {
            let ty = ctx.slot_type(&CirVar(v), &q_type());
            synthetic_types.entry(v).or_insert_with(|| ty.clone());
            if !matches!(ty, IrType::Array { .. }) {
                let slot_str = v.to_string();
                let value = ctx.slot_expr(&CirVar(v));
                ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                    left: Box::new(arr_index("_synth_pool", &slot_str)),
                    right: Box::new(value),
                }))));
                ctx.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                    left: Box::new(arr_index("_synth_pool_written", &slot_str)),
                    right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
                }))));
                continue;
            }
            ret_tuple_tys.push(ty);
            ret_tuple_exprs.push(ctx.slot_expr(&CirVar(v)));
        }

        let chunk_func = IrFunction { no_inline: true,
            name: format!("vole_verify_ir_{}_accum_chunk_{}", name, chunk_idx),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause_for(sink),
            body: IrBlock { stmts: ctx.finalize_and_take_stmts(), expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))) },
            external_kind: ExternalKind::Normal,
        };
        emit_fn(chunk_func);

        running_done_acc = out_step.done_acc;
        running_next_pc = out_step.next_pc.clone();
        running_next_state = out_step.next_state.clone();
        running_ret_vals = out_step.ret_vals.clone();
        lo = hi;
        chunk_idx += 1;
    }

    // ---- Finish: whatever remains after the accumulation phase (lower_to_circuit_ir's own terminator-select/padding) ----
    let finish_start = (accum_info.steps.last().expect("boundary is non-empty (asserted above)").end - num_params as u32) as usize;
    let finish_end = block.stmts.len();
    let finish_stmts = &block.stmts[finish_start..finish_end];
    let finish_and_count = count_ir_ands_no_storage_range(finish_stmts, types);
    let finish_oracle_reads = if matches!(mode, StorageMode::Commitment) {
        count_storage_reads_range(finish_stmts, types)
    } else { 0 };
    let finish_ext = count_external_primitives_range(finish_stmts, types);

    let mut params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
    ];
    if finish_and_count > 0 {
        params.push(IrParam { name: "q_and".into(), ty: q_and_array_type(finish_and_count) });
        params.push(IrParam { name: "hat".into(), ty: hat_array_type(finish_and_count) });
        params.push(IrParam { name: "r_and".into(), ty: r_and_array_type(finish_and_count, sink.fold_scalar_type_name()) });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    params.extend(w_params.iter().filter(|p| matches!(p.ty, IrType::Array { .. })).cloned());
    for j in 0..finish_oracle_reads {
        params.push(IrParam { name: format!("oracle_rd_{}", j), ty: q_type() });
    }
    for (k, call) in finish_ext.oracle_calls.iter().enumerate() {
        for j in 0..call.total_bits { params.push(IrParam { name: format!("q_ext_oracle_{}_bit_{}", k, j), ty: q_type() }); }
    }
    for (k, call) in finish_ext.action_calls.iter().enumerate() {
        for j in 0..call.total_bits { params.push(IrParam { name: format!("q_ext_action_{}_bit_{}", k, j), ty: q_type() }); }
    }
    for (r, &width) in finish_ext.rng_widths.iter().enumerate() {
        for j in 0..width { params.push(IrParam { name: format!("q_ext_rng_{}_bit_{}", r, j), ty: q_type() }); }
    }
    // `bind_running`'s READ side can now register a pooled (`_synth_pool`)
    // read for the incoming running accumulator -- see the matching
    // Prover-role comment. Param declaration order doesn't affect
    // `split_driver.rs::build_call`'s own correctness (it matches params
    // by name, not position), so placing this before `all_ok_in`/
    // `fold_state_in` below is fine despite the "all_ok_in/fold_state_in
    // BEFORE synth_*" convention noted elsewhere -- that convention is
    // about wrapper-internal Rust binding order, not signature order.
    params.push(IrParam { name: "_synth_pool".into(), ty: pool_slice_type(q_type(), true) });
    params.push(IrParam { name: "_synth_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });
    // Phase B: unconditional, same reasoning as `_synth_pool`.
    params.push(IrParam { name: "_w_pool".into(), ty: pool_slice_type(q_type(), true) });
    params.push(IrParam { name: "_w_pool_written".into(), ty: pool_slice_type(IrType::Primitive(PrimitiveType::Bool), true) });

    let mut ctx = VoleIrCtx::new_verifier_with_trace_sink(sink);
    insert_w_wires(&mut ctx);
    bind_running(&mut ctx, &mut params, "in", running_done_acc, &running_next_pc, &running_next_state, &running_ret_vals);
    params.push(IrParam { name: "all_ok_in".into(), ty: IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool) });
    params.push(IrParam { name: "fold_state_in".into(), ty: IrType::TypeParam(sink.state_type_name().into()) });

    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None, init: Some(var("all_ok_in")),
    }));
    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::Ident { mutable: true, name: "fold_state".into(), subpat: None },
        ty: None, init: Some(var("fold_state_in")),
    }));
    ctx.emit_circuit_stmts_range(block, types, mode, finish_start..finish_end);

    let local_entry_count = ctx.trace.entries.len() as u32;
    for mut e in ctx.trace.entries.clone() {
        e.timestamp += global_ts;
        overall_trace_entries.push(e);
    }
    global_ts += local_entry_count.max(ctx.mem_timestamp);
    let _ = global_ts;

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("expected Jmp(Return)"),
    };
    let output_ty = if ret_args.len() == 1 {
        ctx.slot_type(&ret_args[0], &q_type())
    } else {
        IrType::Tuple(ret_args.iter().map(|v| ctx.slot_type(v, &q_type())).collect())
    };
    let ret_type = IrType::Tuple(vec![
        output_ty,
        IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool),
        IrType::TypeParam(sink.state_type_name().into()),
    ]);
    let output_expr = if ret_args.len() == 1 {
        ctx.slot_expr(&ret_args[0])
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| ctx.slot_expr(v)).collect()))
    };
    let ret_expr = ir_expr(IrExprKind::Tuple(vec![output_expr, var("all_ok"), var("fold_state")]));

    let finish_func = IrFunction { no_inline: true,
        name: format!("vole_verify_ir_{}_finish", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: where_clause_for(sink),
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };
    emit_fn(finish_func);

    MemoryTrace { entries: overall_trace_entries }
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
    ir_expr(IrExprKind::Try(Box::new(ir_expr(IrExprKind::MethodCall {
        receiver: Box::new(var("transport")),
        method: MethodKind::Other(method.into()),
        type_args: vec![],
        args,
    }))))
}

/// `Ok(expr)`
fn net_ok_expr(inner: IrExpr) -> IrExpr {
    ir_expr(IrExprKind::Call {
        func: Box::new(ir_expr(IrExprKind::Path { segments: vec!["Ok".into()], type_args: vec![] })),
        args: vec![inner],
    })
}

/// `&[hat_0, ...]` — slice reference to fixed array of named wires.
/// Unrelated to `VoleIrCtx`'s own `_hat_pool` scratch pooling (the
/// "net"/networked weave path this belongs to doesn't use `VoleIrCtx`
/// at all) -- always real names, plain `var(h)`.
fn net_hats_slice(hat_names: &[String]) -> IrExpr {
    ref_expr(ir_expr(IrExprKind::FixedArray(hat_names.iter().map(|h| var(h)).collect())))
}

/// `volar_net::vope_bit(&wire)`
fn net_vope_bit_call(wire: &str) -> IrExpr {
    ir_expr(IrExprKind::Call {
        func: Box::new(ir_expr(IrExprKind::Path {
            segments: vec!["volar_net".into(), "vope_bit".into()],
            type_args: vec![],
        })),
        args: vec![ref_expr(var(wire))],
    })
}

/// `Q { q: Array::default() }` — zero Q value (verifier zero wire).
fn net_q_zero_expr() -> IrExpr {
    ir_expr(IrExprKind::StructExpr {
        kind: StructKind::Custom("Q".into()),
        type_args: vec![],
        fields: vec![("q".into(), ir_expr(IrExprKind::Call {
            func: Box::new(ir_expr(IrExprKind::Path {
                segments: vec!["Array".into(), "default".into()],
                type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
            })),
            args: vec![],
        }))],
        rest: None,
    })
}

/// `Vope { u: Array::default(), v: Array::default() }` — zero prover wire.
fn net_vope_zero_expr() -> IrExpr {
    ir_expr(IrExprKind::StructExpr {
        kind: StructKind::Custom("Vope".into()),
        type_args: vec![],
        fields: vec![
            ("u".into(), array_default()),
            ("v".into(), ir_expr(IrExprKind::Call {
                func: Box::new(ir_expr(IrExprKind::Path {
                    segments: vec!["Array".into(), "default".into()],
                    type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
                })),
                args: vec![],
            })),
        ],
        rest: None,
    })
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
    ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::ident(name),
        ty: None,
        init: Some(ir_expr(IrExprKind::If {
            cond: Box::new(var("is_first")),
            then_branch: IrBlock { stmts: vec![], expr: Some(Box::new(then_expr)) },
            else_branch: Some(Box::new(else_expr)),
        })),
    })
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
    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(net_transport_try("send_hats", vec![hats_ref]))));
    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(net_transport_try("recv_verdict", vec![]))));

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("weave_net_vole_prover_ir: expected Jmp(Return)"),
    };
    let output_expr = if ret_args.len() == 1 {
        clone_expr(var(ctx.scalar(&ret_args[0])))
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| clone_expr(var(ctx.scalar(v)))).collect()))
    };

    let func = IrFunction { no_inline: true,
        name: format!("vole_prove_net_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
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
    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))),
    }));
    ctx.emit_circuit(block, types, &mode, &circuit.pre_init);

    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(net_transport_try("send_verdict", vec![var("all_ok")]))));

    let ret_args = match &block.terminator {
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("weave_net_vole_verifier_ir: expected Jmp(Return)"),
    };
    let output_expr = if ret_args.len() == 1 {
        clone_expr(var(ctx.scalar(&ret_args[0])))
    } else {
        ir_expr(IrExprKind::Tuple(ret_args.iter().map(|v| clone_expr(var(ctx.scalar(v)))).collect()))
    };

    let func = IrFunction { no_inline: true,
        name: format!("vole_verify_net_ir_{}", name),
        module_path: vec![],
        generics,
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause,
        body: IrBlock {
            stmts: ctx.finalize_and_take_stmts(),
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
    b0_args.push(ir_expr(IrExprKind::Lit(IrLit::Bool(true)))); // is_first = true
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args, reentry: None }),
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
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("weave_net_vole_prover_ir_loop: expected Jmp(Return)"),
    };
    let done_wire = ctx.scalar(ret_args.last().expect("ret_args must be non-empty"));
    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::ident("done_bit"),
        ty: None,
        init: Some(net_vope_bit_call(done_wire)),
    }));

    // Send iteration.
    let hats_ref = net_hats_slice(&ctx.hat_names);
    ctx.stmts.push(ir_stmt(IrStmtKind::Semi(net_transport_try(
        "send_iteration",
        vec![hats_ref, var("done_bit")],
    ))));

    // Back-edge args: next circuit inputs + updated cells + is_first=false.
    let next_state_args: Vec<IrExpr> = ret_args[..ret_args.len().saturating_sub(1)]
        .iter()
        .map(|v| clone_expr(var(ctx.scalar(v))))
        .collect();
    let output_wire = ctx.scalar(&ret_args[0]).to_string();
    let mut back_args = next_state_args;
    back_args.extend(net_collect_cell_back_args(&ctx, storage_sizes, types));
    back_args.push(ir_expr(IrExprKind::Lit(IrLit::Bool(false)))); // is_first = false

    let b1 = IrCfgBlock {
        params: b1_params,
        stmts: ctx.finalize_and_take_stmts(),
        terminator: IrCfgTerminator::CondGoto {
            cond: var("done_bit"),
            then_: IrCfgJump { target: 2, args: vec![clone_expr(var(&output_wire))], reentry: None },
            else_: IrCfgJump { target: 1, args: back_args, reentry: None },
        },
    };

    // ── Block 2: exit ─────────────────────────────────────────────────────
    let b2 = IrCfgBlock {
        params: vec![IrParam { name: "output".into(), ty: vope_type() }],
        stmts: vec![ir_stmt(IrStmtKind::Semi(net_transport_try("recv_verdict", vec![])))],
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
    b0_args.push(ir_expr(IrExprKind::Lit(IrLit::Bool(true))));  // all_ok = true
    b0_args.push(ir_expr(IrExprKind::Lit(IrLit::Int(0))));       // iter = 0
    b0_args.push(ir_expr(IrExprKind::Lit(IrLit::Bool(true))));   // is_first = true
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args, reentry: None }),
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
    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident("iter_hats"),
            IrPattern::ident("is_sentinel"),
        ]),
        ty: None,
        init: Some(net_transport_try("recv_iteration", vec![
            ir_expr(IrExprKind::Lit(IrLit::Int(and_count as i128))),
        ])),
    }));

    // Mutable all_ok accumulator for this iteration.
    ctx.stmts.push(ir_stmt(IrStmtKind::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(var("all_ok")),
    }));

    // Pre-bind q_and_k and hat_k so emit_circuit_stmts can reference them.
    for k in 0..and_count {
        let q_and_idx = ir_expr(IrExprKind::Binary {
            op: SpecBinOp::Add,
            left: Box::new(ir_expr(IrExprKind::Binary {
                op: SpecBinOp::Mul,
                left: Box::new(var("iter")),
                right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(and_count as i128)))),
            })),
            right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(k as i128)))),
        });
        ctx.stmts.push(ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident(&format!("q_and_{}", k)),
            ty: None,
            init: Some(clone_expr(ir_expr(IrExprKind::Index {
                base: Box::new(var("q_ands")),
                index: Box::new(q_and_idx),
            }))),
        }));
        ctx.stmts.push(ir_stmt(IrStmtKind::Let {
            pattern: IrPattern::ident(&format!("hat_{}", k)),
            ty: None,
            init: Some(clone_expr(ir_expr(IrExprKind::Index {
                base: Box::new(var("iter_hats")),
                index: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(k as i128)))),
            }))),
        }));
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
        IRTerminator::Jmp { target } if matches!(target.dest, IRBlockTargetId::Return) => &target.args,
        _ => panic!("weave_net_vole_verifier_ir_loop: expected Jmp(Return)"),
    };

    let mut back_args: Vec<IrExpr> = ret_args[..ret_args.len().saturating_sub(1)]
        .iter()
        .map(|v| clone_expr(var(ctx.scalar(v))))
        .collect();
    back_args.extend(net_collect_cell_back_args(&ctx, storage_sizes, types));
    back_args.push(var("all_ok")); // accumulated all_ok
    back_args.push(ir_expr(IrExprKind::Binary {
        op: SpecBinOp::Add,
        left: Box::new(var("iter")),
        right: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(1)))),
    }));
    back_args.push(ir_expr(IrExprKind::Lit(IrLit::Bool(false)))); // is_first = false

    let b1 = IrCfgBlock {
        params: b1_params,
        stmts: ctx.finalize_and_take_stmts(),
        terminator: IrCfgTerminator::CondGoto {
            cond: var("is_sentinel"),
            then_: IrCfgJump { target: 2, args: vec![var("all_ok")], reentry: None },
            else_: IrCfgJump { target: 1, args: back_args, reentry: None },
        },
    };

    // ── Block 2: exit ─────────────────────────────────────────────────────
    let b2 = IrCfgBlock {
        params: vec![IrParam { name: "final_ok".into(), ty: net_bool_type() }],
        stmts: vec![ir_stmt(IrStmtKind::Semi(net_transport_try("send_verdict", vec![var("final_ok")])))],
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

    let preamble = concat!(
        "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
        "extern crate alloc;\n",
        "use alloc::vec::Vec;\n",
        "use alloc::vec;\n",
        "use core::ops::{Add, Mul};\n",
        "use hybrid_array::{Array, ArraySize};\n",
        "use cipher::consts::U1;\n",
        "use volar_spec::vole::{Delta, Q, Vope, VoleArray, debug_check_pool_written};\n",
        "use volar_spec::vole::prove::{vole_and_prover_step, vole_and_verifier_check};\n",
        "use volar_spec::vole::setup::derive_and_q;\n",
        "use volar_spec::field::Invert;\n",
        "\n",
    );

    // Write the preamble + printed IR directly into ONE buffer, rather
    // than building the printed body as its own separate `String` and
    // then copying it in. At real interpreter scale (~2GB of printed
    // text) the old two-buffer approach meant `body` and `out` were both
    // fully resident at once -- and `body` was never explicitly dropped,
    // so Rust's default end-of-scope drop timing kept it alive (unused)
    // through the rest of this function too, including the whole
    // `chunk_function_bodies` call below (which allocates its own,
    // separate output buffer) -- avoidable peak-memory pressure that
    // contributed to a real machine hang during a real-scale compile
    // attempt.
    let mut out = String::with_capacity(preamble.len());
    out.push_str(preamble);
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module, emit_async: false }));

    // Bound how many flat `let` bindings share a single scope in any one
    // printed function -- see `crate::nested_block_chunk`'s own doc for
    // why this matters (real-interpreter-scale profiling found rustc's
    // own AST name-resolution pass, not MIR-borrowck or codegen, as the
    // dominant compile-time/memory cost once `vole_split`'s per-function
    // splitting was already in place).
    crate::nested_block_chunk::chunk_function_bodies(&out, NESTED_BLOCK_CHUNK_SIZE)
}

/// How many top-level statements a printed function body may hold before
/// `nested_block_chunk` re-chunks it into nested blocks. Deliberately
/// much smaller than `MAX_STMTS_PER_PIECE` (500) -- that threshold
/// bounds per-function MIR-borrowck cost; this one bounds the size of a
/// single rustc name-resolution scope ("rib"), a separate cost that
/// scales with flat binding count within *one* scope, not function
/// count. Not yet empirically tuned against real interpreter-scale
/// compiles -- start conservative, adjust based on real profiling.
const NESTED_BLOCK_CHUNK_SIZE: usize = 40;

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
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_prover");
    }

    #[test]
    fn test_weave_vole_verifier_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_vole_verifier(&circuit, "test_circuit", None);
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_verifier");
    }

    // ---- VerifierTraceSink: default None is a no-op, IopSink threads
    // real IR --------------------------------------------------------

    #[test]
    fn trace_sink_none_by_default_leaves_verifier_unchanged() {
        // weave_vole_verifier_with_config never configures a trace sink —
        // its output must not reference any of the trace-sink machinery.
        let circuit = build_xor_and_circuit();
        let config = ZkWitnessConfig::default();
        let module = weave_vole_verifier_with_config(&circuit, "test_circuit", &config, None);
        let code = print_weaved_vole_module(module.inner());
        for needle in ["fold_state", "IopAccumulator", "iop_fold_gate", "iop_accumulator_fresh", "IopChallenge", "r_and_"] {
            assert!(!code.contains(needle), "trace_sink()=None must not emit {needle:?}, got:\n{code}");
        }
        run_compile_check(&code, "vole_verifier_no_sink");
    }

    #[test]
    fn trace_sink_none_matches_plain_weave_vole_verifier() {
        // weave_vole_verifier_with_config(..., trace_sink=None by default)
        // must be byte-identical to the plain weave_vole_verifier entry
        // point — the new machinery adds nothing when no sink is configured.
        let circuit = build_xor_and_circuit();
        let plain = weave_vole_verifier(&circuit, "test_circuit", None);
        let via_config =
            weave_vole_verifier_with_config(&circuit, "test_circuit", &ZkWitnessConfig::default(), None);
        assert_eq!(
            print_weaved_vole_module(plain.inner()),
            print_weaved_vole_module(via_config.inner()),
        );
    }

    #[test]
    fn iop_sink_threads_typed_state_and_real_call_sites() {
        // build_xor_and_circuit has exactly one AND gate (gate index 0).
        let circuit = build_xor_and_circuit();
        let config = ZkWitnessConfig::default();
        let module =
            weave_vole_verifier_with_trace(&circuit, "test_circuit", &config, &IopSink, None);
        let code = print_weaved_vole_module(module.inner());

        // Threaded state: bound at entry via the externally-resolved init
        // function, reassigned via the per-gate fold call, returned.
        assert!(code.contains("iop_accumulator_fresh"), "missing init call:\n{code}");
        assert!(code.contains("fold_state"), "missing threaded state var:\n{code}");
        assert!(code.contains("iop_fold_gate"), "missing per-gate fold call:\n{code}");
        assert!(code.contains("IopAccumulator"), "missing state type:\n{code}");

        // Exactly one per-gate fold challenge param (gate index 0), matching
        // the circuit's single AND gate — not a second one.
        assert!(code.contains("r_and_0"), "missing r_and_0 param:\n{code}");
        assert!(!code.contains("r_and_1"), "unexpected r_and_1 for a single-AND-gate circuit:\n{code}");
        assert!(code.contains("IopChallenge"), "missing fold-challenge type:\n{code}");

        // Real IR, not a raw string: and_gate_step referenced the gate's
        // *actual* wire/param names (q_and_0/hat_0-derived wire, delta) —
        // not placeholders — so they still appear verbatim in the call.
        assert!(code.contains("hat_0"), "and_gate_step must reference the real hat_0, not a placeholder:\n{code}");
        assert!(code.contains("delta"), "and_gate_step must reference the real delta param:\n{code}");

        // NOT compile-checked here: IopAccumulator/iop_fold_gate/etc. are
        // deliberately unresolved (see VerifierTraceSink's doc) — resolving
        // them is the runtime harness's job (crates/iop/volar-verifier-iop-runtime),
        // not this weave-time test's.
    }

    // ---- Side 2: weave_*_with_side parity with the legacy ZkWitnessConfig path ---

    #[test]
    fn test_weave_vole_prover_with_side_matches_legacy_config() {
        use volar_side::{SideId, TableProtection};

        let circuit = build_xor_and_circuit();

        let legacy_config = ZkWitnessConfig {
            public_inputs: {
                let mut s = PublicSet::default();
                s.mark_public(CirVar(0));
                s
            },
            action_configs: BTreeMap::new(),
        };
        let legacy = weave_vole_prover_with_config(&circuit, "test_circuit", &legacy_config, None);

        let assignments = VoleSideAssignments::default().with_input(0, SideId(0));
        let side_handler = TableProtection::new(VoleProtection::Witness)
            .with(SideId(0), VoleProtection::Statement);
        let side = weave_vole_prover_with_side(&circuit, "test_circuit", &assignments, &side_handler);

        assert_eq!(
            print_weaved_vole_module(legacy.inner()),
            print_weaved_vole_module(side.inner()),
            "weave_vole_prover_with_side must produce identical output to the \
             equivalent weave_vole_prover_with_config call"
        );
    }

    #[test]
    fn test_weave_vole_verifier_with_side_matches_legacy_config() {
        use volar_side::{SideId, TableProtection};

        let circuit = build_xor_and_circuit();

        let legacy_config = ZkWitnessConfig {
            public_inputs: {
                let mut s = PublicSet::default();
                s.mark_public(CirVar(1));
                s
            },
            action_configs: BTreeMap::new(),
        };
        let legacy = weave_vole_verifier_with_config(&circuit, "test_circuit", &legacy_config, None);

        let assignments = VoleSideAssignments::default().with_input(1, SideId(0));
        let side_handler = TableProtection::new(VoleProtection::Witness)
            .with(SideId(0), VoleProtection::Statement);
        let side = weave_vole_verifier_with_side(&circuit, "test_circuit", &assignments, &side_handler);

        assert_eq!(
            print_weaved_vole_module(legacy.inner()),
            print_weaved_vole_module(side.inner()),
            "weave_vole_verifier_with_side must produce identical output to the \
             equivalent weave_vole_verifier_with_config call"
        );
    }

    #[test]
    fn test_weave_vole_prover_bounded_compiles() {
        let circuit = build_simple_loop();
        let module = weave_vole_prover_bounded(&circuit, "loop_vole", 4, LoweringMode::Unconditional, None);
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_prover_bounded");
    }

    #[test]
    fn test_weave_vole_verifier_bounded_compiles() {
        let circuit = build_simple_loop();
        let module = weave_vole_verifier_bounded(&circuit, "loop_vole", 4, LoweringMode::Unconditional, None);
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_verifier_bounded");
    }

    #[test]
    fn test_vole_prover_returns_fixed_array() {
        let circuit = crate::tests_common::build_xor_and_circuit();
        let module = weave_vole_prover(&circuit, "test_circuit", None);
        let code = print_weaved_vole_module(module.inner());
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
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![CirVar(0)],) },
        };
        (IRBlocks::new(std::vec![block]), types)
    }

    /// Build a circuit that ANDs two inputs: Poly { {[0,1]: 1}, 0 }.
    fn build_ir_and_circuit() -> (IRBlocks, CirTypes) {
        let mut types = CirTypes::new();
        let bit = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let mut coeffs = PolyCoeffs::new();
        coeffs.insert(std::vec![CirVar(0), CirVar(1)], 1u8);
        let block = CirBlock {
            params: std::vec![bit, bit],
            stmts: std::vec![
                volar_ir_common::Node::new(
                    Stmt::Poly { ty: bit, coeffs, constant: CirConst { hi: 0, lo: 0 } },
                    (),
                    None,
                ),
            ],
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![CirVar(2)],) },
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
                volar_ir_common::Node::new(
                    Stmt::StorageWrite {
                        storage: StorageId(0),
                        src: CirVar(0),
                        ty: CirTyId(0),
                        addr: CirVar(1),
                    },
                    (),
                    None,
                ),
                // stmt 1 (var 3): read back from storage at same address
                volar_ir_common::Node::new(
                    Stmt::StorageRead {
                        storage: StorageId(0),
                        ty: CirTyId(0),
                        addr: CirVar(1),
                    },
                    (),
                    None,
                ),
            ],
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![CirVar(3)],) },
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
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_ir_prover_id");
    }

    #[test]
    fn test_weave_vole_ir_verifier_identity() {
        let (circuit, types) = build_ir_identity_circuit();
        let ss = StorageSizes::new();
        let module = weave_vole_verifier_ir(&circuit, &types, "identity", &ss, None);
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_ir_verifier_id");
    }

    #[test]
    fn test_weave_vole_ir_prover_and() {
        let (circuit, types) = build_ir_and_circuit();
        let ss = StorageSizes::new();
        let module = weave_vole_prover_ir(&circuit, &types, "and_gate", &ss, None);
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_ir_prover_and");
    }

    #[test]
    fn test_weave_vole_ir_verifier_and() {
        let (circuit, types) = build_ir_and_circuit();
        let ss = StorageSizes::new();
        let module = weave_vole_verifier_ir(&circuit, &types, "and_gate", &ss, None);
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_ir_verifier_and");
    }

    /// Milestone 1.6: the new `QSim` role compiles, and its generated code
    /// derives `q_and` (via `derive_and_q`) rather than taking it as an
    /// external parameter -- the structural signature of the fix (real
    /// numeric validation, including chained gates, happens in
    /// `volar-riscv-e2e`'s `mem_probe`/`commit_mem_e2e`-style driven tests).
    #[test]
    fn test_weave_vole_ir_qsim_and() {
        let (circuit, types) = build_ir_and_circuit();
        let mode = StorageMode::Tree(StorageSizes::new());
        let (module, _trace) = weave_vole_qsim_ir_with_mode(&circuit, &types, "and_gate", &mode);
        let func = &module.functions[0];
        assert!(func.params.iter().any(|p| p.name == "hat"), "QSim must take a batched hat array as an input: {:?}", func.params.iter().map(|p| &p.name).collect::<std::vec::Vec<_>>());
        assert!(!func.params.iter().any(|p| p.name == "q_and"), "QSim must NOT take q_and as an input (it derives it): {:?}", func.params.iter().map(|p| &p.name).collect::<std::vec::Vec<_>>());
        assert!(func.no_inline, "QSim functions must be marked no_inline (see Milestone 1.6's compile-time fix)");
        let code = print_weaved_vole_module(&module);
        assert!(code.contains("#[inline(never)]"), "QSim's no_inline flag must be printed as #[inline(never)]:\n{code}");
        assert!(code.contains("derive_and_q::"), "QSim must derive q_and via derive_and_q:\n{code}");
        assert!(!code.contains("vole_and_verifier_check::"), "QSim must not call the verifier's check-only path:\n{code}");
        run_compile_check(&code, "vole_ir_qsim_and");
    }

    #[test]
    fn test_weave_vole_ir_prover_storage() {
        let (circuit, types, ss) = build_ir_storage_circuit();
        let module = weave_vole_prover_ir(&circuit, &types, "storage", &ss, None);
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_ir_prover_stor");
    }

    #[test]
    fn test_weave_vole_ir_verifier_storage() {
        let (circuit, types, ss) = build_ir_storage_circuit();
        let module = weave_vole_verifier_ir(&circuit, &types, "storage", &ss, None);
        let code = print_weaved_vole_module(module.inner());
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
        let code = print_weaved_vole_module(module.inner());
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
        let code = print_weaved_vole_module(module.inner());
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

    // ---- IR path: VerifierTraceSink (IopSink) threading --------------------
    // The Volar-IR analogue of `iop_sink_threads_typed_state_and_real_call_sites`.

    #[test]
    fn ir_trace_sink_threads_typed_state_and_real_call_sites() {
        // build_ir_and_circuit has exactly one AND gate (a degree-2 monomial
        // inside its single `Stmt::Poly`, per `VoleIrCtx::emit_and`).
        let (circuit, types) = build_ir_and_circuit();
        let mode = StorageMode::Tree(StorageSizes::new());
        let (module, _trace) = weave_vole_verifier_ir_with_mode_and_trace(
            &circuit, &types, "and_gate", &mode, &IopSink, None,
        );
        let code = print_weaved_vole_module(module.inner());

        assert!(code.contains("iop_accumulator_fresh"), "missing init call:\n{code}");
        assert!(code.contains("fold_state"), "missing threaded state var:\n{code}");
        assert!(code.contains("iop_fold_gate"), "missing per-gate fold call:\n{code}");
        assert!(code.contains("IopAccumulator"), "missing state type:\n{code}");

        assert!(code.contains("r_and: [IopChallenge; 1]"), "missing batched r_and param covering the one AND gate:\n{code}");
        assert!(code.contains("IopChallenge"), "missing fold-challenge type:\n{code}");

        assert!(code.contains("hat[0]"), "and_gate_step must reference the real hat[0], not a placeholder:\n{code}");
        assert!(code.contains("delta"), "and_gate_step must reference the real delta param:\n{code}");

        // Not compile-checked (same reason as the BIrBlocks test): IopAccumulator
        // / iop_fold_gate / etc. are deliberately unresolved bare names.
    }

    #[test]
    fn ir_trace_sink_composes_with_commitment_mode_storage() {
        // A circuit with both a real AND gate and committed storage -- the
        // exact combination the RISC-V interpreter needs (data RAM reads/
        // writes alongside real ALU/branch AND-gate checks). Commitment
        // mode's own storage ops contribute 0 ANDs (test_commitment_mode_zero_ands),
        // so the only fold_gate call this circuit produces comes from the
        // Poly-based AND gate below, not from storage.
        let mut types = CirTypes::new();
        let bit = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let mut coeffs = PolyCoeffs::new();
        coeffs.insert(std::vec![CirVar(0), CirVar(1)], 1u8);
        let block = CirBlock {
            params: std::vec![bit, bit, bit], // value, addr, and-operand
            stmts: std::vec![
                // var 3: and_result = param0 * param1
                volar_ir_common::Node::new(
                    Stmt::Poly { ty: bit, coeffs, constant: CirConst { hi: 0, lo: 0 } },
                    (),
                    None,
                ),
                // var 4: write and_result to storage at addr = param2
                volar_ir_common::Node::new(
                    Stmt::StorageWrite { storage: StorageId(0), src: CirVar(3), ty: CirTyId(0), addr: CirVar(2) },
                    (),
                    None,
                ),
                // var 5: read it back
                volar_ir_common::Node::new(
                    Stmt::StorageRead { storage: StorageId(0), ty: CirTyId(0), addr: CirVar(2) },
                    (),
                    None,
                ),
            ],
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![CirVar(5)]) },
        };
        let circuit = IRBlocks::new(std::vec![block]);
        let mode = StorageMode::Commitment;

        let (module, trace) = weave_vole_verifier_ir_with_mode_and_trace(
            &circuit, &types, "and_then_commit", &mode, &IopSink, None,
        );
        let code = print_weaved_vole_module(module.inner());

        assert!(code.contains("iop_fold_gate"), "missing per-gate fold call:\n{code}");
        assert!(code.contains("r_and: [IopChallenge; 1]"), "missing batched r_and param covering the one AND gate (storage ops must not contribute extra fold params):\n{code}");
        assert_eq!(trace.entries.len(), 2, "one write + one read");
    }

    // ==========================================================================
    // Multi-bit (width > 1) regression tests -- every existing IR fixture above
    // is Bit-only (width 1); these lock in the width-awareness fixes to
    // `emit_poly`/`operand_lane`/`emit_storage_read_committed`/
    // `count_storage_reads`/`count_ir_ands(_no_storage)`/the input-wire and
    // return-value handling in `emit_circuit`/`weave_vole_*_ir_with_mode*` --
    // all of which previously assumed width == 1 and were only ever
    // exercised at width 1 before the RISC-V interpreter e2e test (real
    // `_32`-typed registers) surfaced the gap.
    // ==========================================================================

    /// Two `_8`-typed params, one Poly computing their AND (a real 8-lane
    /// Quicksilver AND-gate chain via `emit_poly_lane`, not one).
    fn build_ir_wide_and_circuit() -> (IRBlocks, CirTypes) {
        let mut types = CirTypes::new();
        let byte = types.intern(CircuitIrType::Primitive(PrimTy::_8));
        let mut coeffs = PolyCoeffs::new();
        coeffs.insert(std::vec![CirVar(0), CirVar(1)], 1u8);
        let block = CirBlock {
            params: std::vec![byte, byte],
            stmts: std::vec![
                volar_ir_common::Node::new(
                    Stmt::Poly { ty: byte, coeffs, constant: CirConst { hi: 0, lo: 0 } },
                    (),
                    None,
                ),
            ],
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![CirVar(2)]) },
        };
        (IRBlocks::new(std::vec![block]), types)
    }

    #[test]
    fn test_wide_and_count_scales_with_width() {
        let (circuit, types) = build_ir_wide_and_circuit();
        let block = &circuit.blocks[0];
        // 1 degree-2 monomial × 8 lanes = 8 AND gates, not 1.
        assert_eq!(count_ir_ands(block, &types, &StorageMode::Tree(StorageSizes::new())), 8);
        assert_eq!(count_ir_ands_no_storage(block, &types), 8);
    }

    #[test]
    fn test_weave_vole_ir_prover_wide_and_compiles() {
        let (circuit, types) = build_ir_wide_and_circuit();
        let mode = StorageMode::Tree(StorageSizes::new());
        let (module, _trace) = weave_vole_prover_ir_with_mode(&circuit, &types, "wide_and", &mode, None);
        let code = print_weaved_vole_module(module.inner());
        run_compile_check(&code, "vole_ir_prover_wide_and");
    }

    #[test]
    fn test_weave_vole_ir_verifier_wide_and_compiles() {
        let (circuit, types) = build_ir_wide_and_circuit();
        let mode = StorageMode::Tree(StorageSizes::new());
        let (module, _trace) = weave_vole_verifier_ir_with_mode(&circuit, &types, "wide_and", &mode, None);
        let code = print_weaved_vole_module(module.inner());
        // The single (array-typed) return slot must be an array literal of
        // 8 elements, not a tuple of 8 identical types.
        assert!(code.contains('['), "wide return slot must be a fixed-size array, not a tuple:\n{code}");
        run_compile_check(&code, "vole_ir_verifier_wide_and");
    }

    #[test]
    fn test_wide_and_trace_sink_folds_all_eight_lanes() {
        let (circuit, types) = build_ir_wide_and_circuit();
        let mode = StorageMode::Tree(StorageSizes::new());
        let (module, _trace) = weave_vole_verifier_ir_with_mode_and_trace(
            &circuit, &types, "wide_and", &mode, &IopSink, None,
        );
        let code = print_weaved_vole_module(module.inner());
        // 8 lanes → 8 independent AND-gate folds, one batched r_and array
        // covering all 8, each lane folded via an indexed read inside the
        // one compact loop body (not one param per lane).
        assert!(code.contains("r_and: [IopChallenge; 8]"), "missing batched r_and array covering all 8 lanes:\n{code}");
        assert!(code.contains("r_and[0]") && code.contains("r_and[7]"), "expected the bundling step to read every lane (r_and[0]..r_and[7]) out of the batched param:\n{code}");
    }

    /// Milestone 1.6's real scaling fix: `QSim` must use the *compact*
    /// `emit_poly_wide` path (one `core::array::from_fn` runtime loop) for
    /// wide AND monomials, not the per-lane-unrolled fallback -- a real
    /// driven test on a 2,740-gate circuit hit a catastrophic LLVM codegen
    /// blowup (28GB+ RSS, 400+s, still failing) when `QSim` was forced
    /// through the unrolled path. Regression guard: 8 lanes must still
    /// only cost 8 `derive_and_q` calls in ONE compact loop body, not 8
    /// separately unrolled chains.
    #[test]
    fn test_weave_vole_ir_qsim_wide_and_uses_compact_path() {
        let (circuit, types) = build_ir_wide_and_circuit();
        let mode = StorageMode::Tree(StorageSizes::new());
        let (module, _trace) = weave_vole_qsim_ir_with_mode(&circuit, &types, "wide_and", &mode);
        let func = &module.functions[0];
        let hat_param = func.params.iter().find(|p| p.name == "hat");
        assert!(hat_param.is_some(), "QSim must take a batched hat array covering all 8 lanes: {:?}", func.params.iter().map(|p| &p.name).collect::<std::vec::Vec<_>>());
        assert!(
            matches!(&hat_param.unwrap().ty, volar_compiler::ir::IrType::Array { len: volar_compiler::ir::ArrayLength::Const(8), .. }),
            "hat array must cover all 8 AND-gate lanes: {:?}", hat_param.unwrap().ty
        );
        assert!(!func.params.iter().any(|p| p.name == "q_and"), "QSim must not take q_and as an input");
        let code = print_weaved_vole_module(&module);
        assert!(code.contains("derive_and_q::"), "QSim must derive q_and via derive_and_q:\n{code}");
        // The compact path emits exactly one `core::array::from_fn` for the
        // whole 8-lane Poly; the unrolled fallback would instead emit 8
        // separate `derive_and_q` call sites textually. Count occurrences
        // of the call to distinguish: compact = 1 call site (inside the
        // closure, executed 8 times at runtime); unrolled = 8 call sites.
        let call_sites = code.matches("derive_and_q::").count();
        assert_eq!(call_sites, 1, "expected exactly one derive_and_q call site (compact wide path), found {call_sites}:\n{code}");
        run_compile_check(&code, "vole_ir_qsim_wide_and");
    }

    /// A `_8`-typed value written to and read back from committed storage --
    /// `emit_storage_read_committed` must allocate 8 oracle params (one per
    /// lane), not 1, and the result must be a `WireRepr::Vec` of 8 names.
    fn build_ir_wide_committed_circuit() -> (IRBlocks, CirTypes) {
        let mut types = CirTypes::new();
        let byte = types.intern(CircuitIrType::Primitive(PrimTy::_8));
        let bit = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let block = CirBlock {
            params: std::vec![byte, bit], // value, addr
            stmts: std::vec![
                volar_ir_common::Node::new(
                    Stmt::StorageWrite { storage: StorageId(0), src: CirVar(0), ty: CirTyId(byte.0), addr: CirVar(1) },
                    (),
                    None,
                ),
                volar_ir_common::Node::new(
                    Stmt::StorageRead { storage: StorageId(0), ty: CirTyId(byte.0), addr: CirVar(1) },
                    (),
                    None,
                ),
            ],
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![CirVar(3)]) },
        };
        (IRBlocks::new(std::vec![block]), types)
    }

    #[test]
    fn test_wide_committed_storage_allocates_one_oracle_param_per_lane() {
        let (circuit, types) = build_ir_wide_committed_circuit();
        let block = &circuit.blocks[0];
        assert_eq!(count_storage_reads(block, &types), 8, "one _8 read = 8 committed bits");
    }

    #[test]
    fn test_weave_vole_ir_prover_wide_committed_compiles() {
        let (circuit, types) = build_ir_wide_committed_circuit();
        let mode = StorageMode::Commitment;
        let (module, trace) = weave_vole_prover_ir_with_mode(&circuit, &types, "wide_commit", &mode, None);
        let code = print_weaved_vole_module(module.inner());
        assert_eq!(trace.entries.len(), 2, "one write + one read");
        run_compile_check(&code, "vole_ir_prover_wide_commit");
    }

    #[test]
    fn test_weave_vole_ir_verifier_wide_committed_compiles() {
        let (circuit, types) = build_ir_wide_committed_circuit();
        let mode = StorageMode::Commitment;
        let (module, trace) = weave_vole_verifier_ir_with_mode(&circuit, &types, "wide_commit", &mode, None);
        let code = print_weaved_vole_module(module.inner());
        for j in 0..8 {
            assert!(code.contains(&format!("oracle_rd_{j}")), "missing per-lane oracle param oracle_rd_{j}:\n{code}");
        }
        assert_eq!(trace.entries.len(), 2);
        run_compile_check(&code, "vole_ir_verifier_wide_commit");
    }

    /// Two already-`_8`-wide committed reads, `Merge`d into one `_16` value
    /// -- `emit_merge` must concatenate each part's own 8 bits (16 total),
    /// not treat each part as a single bit (which would produce a
    /// 2-bit, not 16-bit, result and panic on the first `vec_parts` call
    /// downstream).
    fn build_ir_wide_merge_circuit() -> (IRBlocks, CirTypes) {
        let mut types = CirTypes::new();
        let byte = types.intern(CircuitIrType::Primitive(PrimTy::_8));
        let half = types.intern(CircuitIrType::Primitive(PrimTy::_16));
        let block = CirBlock {
            params: std::vec![byte, byte], // addr0, addr1 (byte-typed but used only as addresses -- fine, tests don't touch StorageMode arithmetic on them)
            stmts: std::vec![
                volar_ir_common::Node::new(
                    Stmt::StorageRead { storage: StorageId(0), ty: CirTyId(byte.0), addr: CirVar(0) },
                    (),
                    None,
                ),
                volar_ir_common::Node::new(
                    Stmt::StorageRead { storage: StorageId(0), ty: CirTyId(byte.0), addr: CirVar(1) },
                    (),
                    None,
                ),
                volar_ir_common::Node::new(
                    Stmt::Merge { parts: std::vec![CirVar(2), CirVar(3)], ty: half },
                    (),
                    None,
                ),
            ],
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![CirVar(4)]) },
        };
        (IRBlocks::new(std::vec![block]), types)
    }

    #[test]
    fn test_weave_vole_ir_prover_wide_merge_compiles() {
        let (circuit, types) = build_ir_wide_merge_circuit();
        let mode = StorageMode::Commitment;
        let (module, trace) = weave_vole_prover_ir_with_mode(&circuit, &types, "wide_merge", &mode, None);
        let code = print_weaved_vole_module(module.inner());
        assert_eq!(trace.entries.len(), 2, "two reads, no writes");
        run_compile_check(&code, "vole_ir_prover_wide_merge");
    }

    #[test]
    fn test_weave_vole_ir_verifier_wide_merge_compiles() {
        let (circuit, types) = build_ir_wide_merge_circuit();
        let mode = StorageMode::Commitment;
        let (module, _trace) = weave_vole_verifier_ir_with_mode(&circuit, &types, "wide_merge", &mode, None);
        let code = print_weaved_vole_module(module.inner());
        // The merged _16 result is the sole return value -- 16 elements.
        assert!(code.contains('['), "merged 16-bit result must be a fixed-size array:\n{code}");
        run_compile_check(&code, "vole_ir_verifier_wide_merge");
    }

    // ---- Milestone 1.5 Step B: split verifier weaving ----------------------

    /// Two real (pre-movfuscation) blocks, each with one AND gate; block 0
    /// additionally has a `StorageRead` -- small enough to hand-check the
    /// split function count/param sizes directly, but genuinely exercises
    /// AND-gate accounting + Commitment-mode oracle reads per block.
    fn build_ir_two_block_and_storage() -> (IRBlocks, CirTypes) {
        let mut types = CirTypes::new();
        let bit = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let mut coeffs0 = PolyCoeffs::new();
        coeffs0.insert(std::vec![CirVar(0), CirVar(3)], 1u8); // a AND (storage read)
        let mut coeffs1 = PolyCoeffs::new();
        coeffs1.insert(std::vec![CirVar(0), CirVar(1)], 1u8); // x AND y
        let blocks = IRBlocks::new(std::vec![
            CirBlock {
                params: std::vec![bit, bit], // a, b
                stmts: std::vec![
                    volar_ir_common::Node::new(Stmt::Const(CirConst { hi: 0, lo: 0 }, bit), (), None), // var 2: addr
                    volar_ir_common::Node::new(
                        Stmt::StorageRead { storage: StorageId(0), ty: bit, addr: CirVar(2) },
                        (), None,
                    ), // var 3
                    volar_ir_common::Node::new(
                        Stmt::Poly { ty: bit, coeffs: coeffs0, constant: CirConst { hi: 0, lo: 0 } },
                        (), None,
                    ), // var 4: a AND read
                ],
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(IRBlockTargetId::Block(volar_ir::ir::IRBlockId(1)), std::vec![CirVar(4), CirVar(1)]),
                },
            },
            CirBlock {
                params: std::vec![bit, bit], // x, y
                stmts: std::vec![
                    volar_ir_common::Node::new(
                        Stmt::Poly { ty: bit, coeffs: coeffs1, constant: CirConst { hi: 0, lo: 0 } },
                        (), None,
                    ), // var 2: x AND y
                ],
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, std::vec![CirVar(2)]) },
            },
        ]);
        (blocks, types)
    }

    #[test]
    fn test_split_verifier_produces_one_function_per_block_plus_chunked_accumulator() {
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};

        let (blocks, mut types) = build_ir_two_block_and_storage();
        let (movfuscated, boundary, accum_info) = movfuscate_ir_with_boundary(&blocks, &mut types);
        assert_eq!(boundary.len(), 2, "one boundary entry per original block");
        assert_eq!(accum_info.steps.len(), 2, "one accumulation step per original block");

        let bit_ty = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::Unconditional);
        assert!(circuit.is_circuit());

        let mode = StorageMode::Commitment;
        let mut funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        // chunk_size=1: forces 2 separate accumulator-chunk functions (one
        // per block) instead of 1 combiner, even with just 2 blocks --
        // exercises the actual chunking logic, not just the degenerate
        // "one big chunk" case.
        let trace = weave_vole_verifier_ir_split_with_trace(
            &circuit, &types, "split_test", &mode, &IopSink, &boundary, &accum_info, 1, DEFAULT_MAX_STMTS_PER_PIECE,
            |f| funcs.push(f),
        );

        assert_eq!(funcs.len(), 5, "2 blocks + 2 accumulator chunks (chunk_size=1) + 1 finish");
        assert_eq!(funcs[0].name, "vole_verify_ir_split_test_block_0");
        assert_eq!(funcs[1].name, "vole_verify_ir_split_test_block_1");
        assert_eq!(funcs[2].name, "vole_verify_ir_split_test_accum_chunk_0");
        assert_eq!(funcs[3].name, "vole_verify_ir_split_test_accum_chunk_1");
        assert_eq!(funcs[4].name, "vole_verify_ir_split_test_finish");

        // No chunk function's own body references *every* block -- with
        // chunk_size=1, each accum_chunk function should reference only
        // its own one block's exported vars (is_active_N/done_N/...), not
        // both blocks'. `is_active`/`done` are scalar for this fixture, so
        // block-boundary-export pooling means they're read via
        // `_synth_pool[v]` (v = the var's own raw circuit var id) rather
        // than taking a named `is_active_N` param -- check the printed
        // body's own pool-index references instead of param names.
        let is_active_0 = boundary[0].is_active;
        let is_active_1 = boundary[1].is_active;
        let one_fn_module = |f: &IrFunction| IrModule {
            name: "split_verifier_test_mod".into(), functions: std::vec![f.clone()],
            structs: std::vec![], enums: std::vec![], traits: std::vec![], impls: std::vec![],
            type_aliases: std::vec![], consts: std::vec![],
        };
        let printed_chunk0 = print_weaved_vole_module(&one_fn_module(&funcs[2]));
        let printed_chunk1 = print_weaved_vole_module(&one_fn_module(&funcs[3]));
        assert!(printed_chunk0.contains(&format!("_synth_pool[{is_active_0}]")), "chunk 0 must pool-read block 0's own is_active:\n{printed_chunk0}");
        assert!(!printed_chunk0.contains(&format!("_synth_pool[{is_active_1}]")), "chunk 0 must not reference block 1's own is_active:\n{printed_chunk0}");
        assert!(printed_chunk1.contains(&format!("_synth_pool[{is_active_1}]")), "chunk 1 must pool-read block 1's own is_active:\n{printed_chunk1}");
        assert!(!printed_chunk1.contains(&format!("_synth_pool[{is_active_0}]")), "chunk 1 must not reference block 0's own is_active:\n{printed_chunk1}");

        // Real, structural bound: the *split* param counts must sum back to
        // exactly the whole (unsplit) circuit's and_count (nothing lost,
        // nothing double-counted across block 0 / block 1 / the combiner's
        // own trailing-accumulation gates) -- checked directly, rather than
        // assuming a specific number, since movfuscation's own accumulation
        // overhead (Σ is_active·next_state[k] per slot, done-accumulation,
        // etc.) contributes its own AND gates on top of the 2 "real" ones
        // from this fixture's own Poly stmts, and dominates at this toy
        // scale (the whole point of Milestone 1.5: at real and_count=2.77M
        // scale this overhead is comparatively negligible).
        let total_and_count = count_ir_ands_no_storage(&circuit.blocks[0], &types);
        // `q_and` is now one array-batched param, not one scalar per gate —
        // read the array's own declared length instead of counting params.
        let q_and_count = |f: &IrFunction| match f.params.iter().find(|p| p.name == "q_and") {
            Some(p) => match &p.ty {
                IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                other => panic!("expected q_and to be an array param, got {other:?}"),
            },
            None => 0,
        };
        let split_and_sum: usize = funcs.iter().map(q_and_count).sum();
        assert_eq!(split_and_sum, total_and_count, "split gate counts must sum back to the whole circuit's");

        // Each of the two real blocks owns at least its own genuine AND
        // gate (the fixture's `a AND read`/`x AND y`), and strictly less
        // than the *whole* circuit's count -- the actual point of the split.
        assert!(q_and_count(&funcs[0]) >= 1 && q_and_count(&funcs[0]) < total_and_count);
        assert!(q_and_count(&funcs[1]) >= 1 && q_and_count(&funcs[1]) < total_and_count);

        // Block 0's own oracle read must appear as a param on block 0's
        // function specifically (not smeared across the others).
        assert!(funcs[0].params.iter().any(|p| p.name == "oracle_rd_0"));
        assert!(!funcs[1].params.iter().any(|p| p.name.starts_with("oracle_rd_")));

        // Every function shares access to the same w_i entry-state --
        // `w_0`/`w_1` are scalar in this fixture, so Phase B pooling
        // means they're read via `_w_pool[i]` (shared, unconditional on
        // every function) rather than a named `w_0`/`w_1` param -- check
        // pool-param presence and, for one function, real pool-index
        // references in the printed body (matching the same idiom used
        // for block-boundary export pooling above).
        for f in &funcs {
            assert!(f.params.iter().any(|p| p.name == "_w_pool"), "{} missing _w_pool", f.name);
        }
        let one_fn_module = |f: &IrFunction| IrModule {
            name: "split_verifier_w_pool_test_mod".into(), functions: std::vec![f.clone()],
            structs: std::vec![], enums: std::vec![], traits: std::vec![], impls: std::vec![],
            type_aliases: std::vec![], consts: std::vec![],
        };
        let printed_block0 = print_weaved_vole_module(&one_fn_module(&funcs[0]));
        assert!(printed_block0.contains("_w_pool[0]"), "block 0 must pool-read w_0:\n{printed_block0}");
        assert!(printed_block0.contains("_w_pool[1]"), "block 0 must pool-read w_1:\n{printed_block0}");

        // Commitment mode: the one real StorageRead produced a trace entry.
        assert_eq!(trace.entries.len(), 1, "one StorageRead in the whole circuit");
    }

    #[test]
    fn test_split_prover_produces_one_function_per_block_plus_combiner_with_local_hats() {
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};

        let (blocks, mut types) = build_ir_two_block_and_storage();
        let (movfuscated, boundary, accum_info) = movfuscate_ir_with_boundary(&blocks, &mut types);

        let bit_ty = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::Unconditional);
        assert!(circuit.is_circuit());

        let mode = StorageMode::Commitment;
        let mut funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        let trace = weave_vole_prover_ir_split(
            &circuit, &types, "split_test", &mode, &boundary, &accum_info, 1, DEFAULT_MAX_STMTS_PER_PIECE,
            |f| funcs.push(f),
        );

        assert_eq!(funcs.len(), 5, "2 blocks + 2 accumulator chunks (chunk_size=1) + 1 finish");
        assert_eq!(funcs[0].name, "vole_prove_ir_split_test_block_0");
        assert_eq!(funcs[1].name, "vole_prove_ir_split_test_block_1");
        assert_eq!(funcs[2].name, "vole_prove_ir_split_test_accum_chunk_0");
        assert_eq!(funcs[3].name, "vole_prove_ir_split_test_accum_chunk_1");
        assert_eq!(funcs[4].name, "vole_prove_ir_split_test_finish");

        // Each function's own *returned* hats array is sized to its own
        // local and_count (found in its return type's trailing element,
        // an `[Array<T,N>; k]`), never the whole circuit's -- the prover
        // counterpart of the verifier split's q_and-per-function bound.
        let hats_len = |f: &IrFunction| match f.return_type.as_ref().unwrap() {
            IrType::Tuple(elems) => match elems.last().unwrap() {
                IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                other => panic!("expected trailing hats array type, got {other:?}"),
            },
            other => panic!("expected tuple return type, got {other:?}"),
        };
        let total_and_count = count_ir_ands_no_storage(&circuit.blocks[0], &types);
        let split_hats_sum: usize = funcs.iter().map(hats_len).sum();
        assert_eq!(split_hats_sum, total_and_count, "split hats counts must sum back to the whole circuit's and_count");
        assert!(hats_len(&funcs[0]) >= 1 && hats_len(&funcs[0]) < total_and_count);
        assert!(hats_len(&funcs[1]) >= 1 && hats_len(&funcs[1]) < total_and_count);

        // No all_ok/fold_state plumbing on the prover side.
        for f in &funcs {
            assert!(!f.params.iter().any(|p| p.name.starts_with("all_ok") || p.name.starts_with("fold_state")));
        }

        assert_eq!(trace.entries.len(), 1, "one StorageRead in the whole circuit");
    }

    /// Phase C sub-stage 1 (`piece_in_v` pooling): forces REAL intra-
    /// region piece splitting on the same small fixture used above by
    /// passing `max_stmts_per_piece = 1` (vs. the production default of
    /// 500) -- exercising `WireRepr::Pooled`/`pool_slot`/the `_piece_pool`
    /// mechanism, which no other existing test reaches (every other test
    /// circuit is far smaller than the production threshold). Checks the
    /// generated IR structurally: at least one piece function must exist
    /// (splitting genuinely happened) and take the pool params, and the
    /// PRINTED source must contain real `_piece_pool[` indexing -- if
    /// pooling silently failed to activate, this would still show
    /// `piece_in_` named params instead, which the test also checks for.
    #[test]
    fn piece_in_v_pooling_activates_under_forced_piece_splitting() {
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};

        let (blocks, mut types) = build_ir_two_block_and_storage();
        let (movfuscated, boundary, accum_info) = movfuscate_ir_with_boundary(&blocks, &mut types);

        let bit_ty = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::Unconditional);
        assert!(circuit.is_circuit());

        let mode = StorageMode::Commitment;
        let mut funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_prover_ir_split(
            &circuit, &types, "pool_test", &mode, &boundary, &accum_info, 1, 1,
            |f| funcs.push(f),
        );

        let piece_funcs: std::vec::Vec<&IrFunction> = funcs.iter().filter(|f| f.name.contains("_piece_")).collect();
        assert!(!piece_funcs.is_empty(), "max_stmts_per_piece=1 must force real intra-region splitting on this fixture; got functions: {:?}", funcs.iter().map(|f| &f.name).collect::<std::vec::Vec<_>>());

        let pooled_piece_funcs: std::vec::Vec<&&IrFunction> = piece_funcs.iter()
            .filter(|f| f.params.iter().any(|p| p.name == "_piece_pool"))
            .collect();
        assert!(
            !pooled_piece_funcs.is_empty(),
            "expected at least one piece function to take _piece_pool -- pool_slot never activated. Piece function param names: {:?}",
            piece_funcs.iter().map(|f| f.params.iter().map(|p| p.name.clone()).collect::<std::vec::Vec<_>>()).collect::<std::vec::Vec<_>>(),
        );

        // The printed source must show real pool indexing, not just the
        // param -- confirms the read/write statements were actually
        // emitted, not just the (harmless-looking but useless) plumbing.
        let module = IrModule {
            name: "pool_test_mod".into(), functions: funcs.clone(),
            structs: std::vec![], enums: std::vec![], traits: std::vec![], impls: std::vec![],
            type_aliases: std::vec![], consts: std::vec![],
        };
        let printed = print_weaved_vole_module(&module);
        assert!(printed.contains("_piece_pool["), "printed source must contain real _piece_pool[..] indexing:\n{printed}");
        assert!(printed.contains("_piece_pool_written["), "printed source must contain real _piece_pool_written[..] indexing:\n{printed}");
        assert!(printed.contains("debug_check_pool_written"), "printed source must call the debug-bitset guard on every pooled read:\n{printed}");
    }

    /// Milestone 1.5's stated goal: the split prover and verifier must be
    /// driveable *interleaved* (block 0's prover, then block 0's verifier,
    /// then block 1's prover, then block 1's verifier, ..., then both
    /// combiners) -- structurally checked here by matching each prover
    /// block function's own local and_count/oracle-read count against the
    /// corresponding verifier block function's, block by block (not just
    /// in aggregate, as the two single-sided tests above already do
    /// individually) -- if these ever drifted apart per-block, an
    /// interleaved driver could not feed one side's per-block output into
    /// the other's per-block input.
    #[test]
    fn test_split_prover_and_verifier_block_shapes_match_for_interleaving() {
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};

        let (blocks, mut types) = build_ir_two_block_and_storage();
        let (movfuscated, boundary, accum_info) = movfuscate_ir_with_boundary(&blocks, &mut types);
        let bit_ty = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::Unconditional);
        let mode = StorageMode::Commitment;

        let mut prover_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_prover_ir_split(&circuit, &types, "il", &mode, &boundary, &accum_info, 1, DEFAULT_MAX_STMTS_PER_PIECE, |f| prover_funcs.push(f));
        let mut verifier_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_verifier_ir_split_with_trace(&circuit, &types, "il", &mode, &IopSink, &boundary, &accum_info, 1, DEFAULT_MAX_STMTS_PER_PIECE, |f| verifier_funcs.push(f));

        assert_eq!(prover_funcs.len(), verifier_funcs.len());
        // `q_and` is now one array-batched param, not one scalar per gate —
        // read the array's own declared length instead of counting params.
        let and_count_of = |f: &IrFunction| match f.params.iter().find(|p| p.name == "q_and") {
            Some(p) => match &p.ty {
                IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                other => panic!("expected q_and to be an array param, got {other:?}"),
            },
            None => 0,
        };
        let oracle_count_of = |f: &IrFunction| f.params.iter().filter(|p| p.name.starts_with("oracle_rd_")).count();
        let hats_len = |f: &IrFunction| match f.return_type.as_ref().unwrap() {
            IrType::Tuple(elems) => match elems.last().unwrap() {
                IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                other => panic!("expected trailing hats array type, got {other:?}"),
            },
            other => panic!("expected tuple return type, got {other:?}"),
        };
        for i in 0..prover_funcs.len() {
            // Block i's prover-returned hats count must equal block i's
            // verifier-consumed q_and count -- the exact interface an
            // interleaved driver relies on to feed one into the other.
            assert_eq!(
                hats_len(&prover_funcs[i]), and_count_of(&verifier_funcs[i]),
                "block {i}: prover hats count must match verifier q_and count for interleaved driving"
            );
            // Oracle reads (Commitment-mode witness the driver must supply
            // to *both* sides identically) must also line up per block.
            let prover_oracle = prover_funcs[i].params.iter().filter(|p| p.name.starts_with("oracle_rd_")).count();
            assert_eq!(
                prover_oracle, oracle_count_of(&verifier_funcs[i]),
                "block {i}: prover and verifier must agree on oracle-read count"
            );
        }
    }

    /// Milestone 1.6, Stage 2: `weave_vole_qsim_ir_split` must produce the
    /// same function-per-block-plus-chunked-accumulator-plus-finish shape
    /// as the prover/verifier splits, and its per-function `hat_k` input
    /// count / derived `q_and` output count must line up with the
    /// corresponding prover/verifier split functions, block by block --
    /// the three-way interleaved-driving contract (prover -> qsim ->
    /// verifier) this milestone's real driven tests rely on.
    #[test]
    fn test_split_qsim_matches_prover_and_verifier_block_shapes() {
        use volar_ir_passes::{lower_to_circuit_ir, movfuscate_ir_with_boundary, LoweringMode};

        let (blocks, mut types) = build_ir_two_block_and_storage();
        let (movfuscated, boundary, accum_info) = movfuscate_ir_with_boundary(&blocks, &mut types);
        let bit_ty = types.intern(CircuitIrType::Primitive(PrimTy::Bit));
        let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::Unconditional);
        let mode = StorageMode::Commitment;

        let mut prover_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_prover_ir_split(&circuit, &types, "qs", &mode, &boundary, &accum_info, 1, DEFAULT_MAX_STMTS_PER_PIECE, |f| prover_funcs.push(f));
        let mut verifier_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_verifier_ir_split_with_trace(&circuit, &types, "qs", &mode, &IopSink, &boundary, &accum_info, 1, DEFAULT_MAX_STMTS_PER_PIECE, |f| verifier_funcs.push(f));
        let mut qsim_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        let trace = weave_vole_qsim_ir_split(&circuit, &types, "qs", &mode, &boundary, &accum_info, 1, DEFAULT_MAX_STMTS_PER_PIECE, |f| qsim_funcs.push(f));

        assert_eq!(qsim_funcs.len(), 5, "2 blocks + 2 accumulator chunks (chunk_size=1) + 1 finish");
        assert_eq!(qsim_funcs[0].name, "vole_qsim_ir_qs_block_0");
        assert_eq!(qsim_funcs[1].name, "vole_qsim_ir_qs_block_1");
        assert_eq!(qsim_funcs[2].name, "vole_qsim_ir_qs_accum_chunk_0");
        assert_eq!(qsim_funcs[3].name, "vole_qsim_ir_qs_accum_chunk_1");
        assert_eq!(qsim_funcs[4].name, "vole_qsim_ir_qs_finish");
        assert_eq!(qsim_funcs.len(), prover_funcs.len());
        assert_eq!(qsim_funcs.len(), verifier_funcs.len());

        for f in &qsim_funcs {
            assert!(f.no_inline, "{} must be marked no_inline", f.name);
            assert!(!f.params.iter().any(|p| p.name == "q_and"), "{} must not take q_and as input", f.name);
        }

        // `hat`/`q_and` are now one array-batched param each, not one
        // scalar per gate — read the array's own declared length instead
        // of counting params.
        let hat_count_of = |f: &IrFunction| match f.params.iter().find(|p| p.name == "hat") {
            Some(p) => match &p.ty {
                IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                other => panic!("expected hat to be an array param, got {other:?}"),
            },
            None => 0,
        };
        let q_and_count_of = |f: &IrFunction| match f.params.iter().find(|p| p.name == "q_and") {
            Some(p) => match &p.ty {
                IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                other => panic!("expected q_and to be an array param, got {other:?}"),
            },
            None => 0,
        };
        let hats_len = |f: &IrFunction| match f.return_type.as_ref().unwrap() {
            IrType::Tuple(elems) => match elems.last().unwrap() {
                IrType::Array { len: volar_compiler::ir::ArrayLength::Const(n), .. } => *n,
                other => panic!("expected trailing array type, got {other:?}"),
            },
            other => panic!("expected tuple return type, got {other:?}"),
        };
        for i in 0..qsim_funcs.len() {
            // QSim's own hat_k input count must match the prover's own
            // returned hats count for that same function (same real hats,
            // fed straight through).
            assert_eq!(
                hat_count_of(&qsim_funcs[i]), hats_len(&prover_funcs[i]),
                "function {i}: qsim's hat_k input count must match the prover's own hats output count"
            );
            // QSim's own derived q_and output count must match the
            // verifier's own consumed q_and_k count for that same function.
            assert_eq!(
                hats_len(&qsim_funcs[i]), q_and_count_of(&verifier_funcs[i]),
                "function {i}: qsim's derived q_and count must match the verifier's own q_and_k input count"
            );
        }
        assert_eq!(trace.entries.len(), 1, "one StorageRead in the whole circuit");
    }
}
