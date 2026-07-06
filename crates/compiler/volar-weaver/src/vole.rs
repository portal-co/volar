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
        IrType, IrWherePredicate, MathTrait, MethodKind, PrimitiveType, SpecBinOp, StdMethod,
        StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir::ir::{
    IRBlocks, IRBlock as CirBlock, IRBlockTargetId, IRStmt, IRTerminator,
    IRType as CircuitIrType, IRTypeId as CirTyId, IRTypes as CirTypes,
    IRVarId as CirVar, PrimType, PreInitSegment, Stmt, StorageId, IRBranchTarget};
use volar_ir::public::PublicSet;
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
    ir_expr(IrExprKind::Index {
        base: Box::new(ir_expr(IrExprKind::Field {
            base: Box::new(var(wire_name)),
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
    /// (`k_a`, `k_b`, `k_c`, `delta`, `hat` — real variable names bound in
    /// the woven function, not string placeholders) into `state_var`,
    /// alongside a newly-introduced per-gate challenge parameter
    /// (`r_param_name`); return the new state expression to rebind
    /// `state_var` to. `gate_idx` is this AND gate's 0-based index (matches
    /// the existing `q_and_{gate_idx}`/`hat_{gate_idx}` param numbering).
    #[allow(clippy::too_many_arguments)]
    fn and_gate_step(
        &self,
        gate_idx: usize,
        k_a: &str,
        k_b: &str,
        k_c: &str,
        delta: &str,
        hat: &str,
        r_param_name: &str,
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
        hat: &str,
        r_param_name: &str,
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
                clone_expr(var(hat)),
                clone_expr(var(r_param_name)),
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
    name_a: &str,
    name_b: &str,
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
                clone_expr(var(name_a)),
                clone_expr(var(name_b)),
            ],
        }, prov.clone())),
    }, prov));
}

/// Emit `let (_wire_k, _ok_k) = vole_and_verifier_check::<N, T>(delta, &wire_a, &wire_b, &q_and_k, &hat_k);`
/// followed by `all_ok = all_ok && _ok_k;`.
fn emit_verifier_and_gate<P: Clone + Default>(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    ok_name: &str,
    q_and_name: &str,
    hat_name: &str,
    stmts: &mut Vec<IrStmt<P>>,
    prov: P,
) {
    // let (wire_k, ok_k) = vole_and_verifier_check::<N, T>(delta, &wire_a, &wire_b, &q_and_k, &hat_k);
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
                ref_expr(var(name_a)),
                ref_expr(var(name_b)),
                ref_expr(var(q_and_name)),
                ref_expr(var(hat_name)),
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

/// Emit `let q_and_k = derive_and_q::<N, T>(delta, &wire_a, &wire_b, &hat_k);`
/// followed by `let wire_k = q_and_k.clone();` — `QSim`'s AND-gate handling
/// (Milestone 1.6): unlike [`emit_verifier_and_gate`], this *derives*
/// `q_and` from an externally-supplied `hat_k` (same shape `Verifier`
/// already takes) instead of taking `q_and_k` itself as an external
/// parameter and checking it. No `ok`/`all_ok`/fold plumbing — `QSim`
/// never folds, that's `Verifier`'s job once handed these derived values.
fn emit_qsim_and_gate<P: Clone + Default>(
    name_a: &str,
    name_b: &str,
    wire_name: &str,
    q_and_name: &str,
    hat_name: &str,
    stmts: &mut Vec<IrStmt<P>>,
    prov: P,
) {
    // let q_and_k = derive_and_q::<N, T>(delta, &wire_a, &wire_b, &hat_k);
    stmts.push(ir_stmt_p(IrStmtKind::Let {
        pattern: IrPattern::ident(q_and_name),
        ty: None,
        init: Some(ir_expr_p(IrExprKind::Call {
            func: Box::new(ir_expr_p(IrExprKind::Path {
                segments: vec!["derive_and_q".into()],
                type_args: vec![
                    IrType::TypeParam("N".into()),
                    IrType::TypeParam("T".into()),
                ],
            }, prov.clone())),
            args: vec![
                var("delta"),
                ref_expr(var(name_a)),
                ref_expr(var(name_b)),
                ref_expr(var(hat_name)),
            ],
        }, prov.clone())),
    }, prov.clone()));

    // let wire_k = q_and_k.clone();
    stmts.push(ir_stmt_p(IrStmtKind::Let {
        pattern: IrPattern::ident(wire_name),
        ty: None,
        init: Some(clone_expr(var(q_and_name))),
    }, prov));
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
                    emit_prover_and_gate(&name_a, &name_b, &let_name, &hat_name, &mut stmts, q.clone());
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
                        &name_a, &name_b, &let_name, &ok_name,
                        &q_and_name, &hat_name,
                        &mut stmts, q.clone(),
                    );
                    if let Some(sink) = config.trace_sink() {
                        let r_param_name = format!("r_and_{}", gate_idx);
                        let new_state = sink.and_gate_step(
                            gate_idx, &name_a, &name_b, &let_name, "delta",
                            &hat_name, &r_param_name, "fold_state", q.clone(),
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

            BIrStmt::OracleBit { call, bit } => {
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
    /// Vector of authenticated bits (produced by `Merge`).
    Vec(Vec<String>),
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
        VoleIrCtx {
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
        }
    }

    /// `QSim`-role constructor (Milestone 1.6): derives `q_and` values via
    /// `derive_and_q` instead of taking them as external parameters. Never
    /// folds (`trace_sink: None`) -- folding is `Verifier`-only, since
    /// `QSim`'s whole job is producing the `q_and`s the real `Verifier`
    /// function will itself fold.
    fn new_qsim() -> Self {
        VoleIrCtx {
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
        }
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
        VoleIrCtx {
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

    /// The `IrExpr` for a return-slot variable, honouring its actual width:
    /// a scalar wire is a plain `clone()`; a `Vec` wire becomes a
    /// **fixed-size array** expression (`[a.clone(), b.clone(), ...]`), not
    /// a tuple of repeated elements — arrays are more efficient on
    /// supported targets and lower gracefully to a dynamic array via
    /// `lowering_dyn` when a runtime-sized version is needed later. Tuples
    /// are reserved for combining genuinely different return slots (see
    /// callers), not for repeating one type.
    fn slot_expr(&self, v: &CirVar) -> IrExpr {
        match &self.wires[&v.0] {
            WireRepr::Scalar(s) => clone_expr(var(s)),
            WireRepr::Vec(names) => {
                ir_expr(IrExprKind::FixedArray(names.iter().map(|n| clone_expr(var(n))).collect()))
            }
        }
    }

    /// The `IrType` for a return-slot variable: `base_ty` (e.g.
    /// `vope_type()`/`q_type()`) for a scalar wire, or a fixed-size array
    /// of `base_ty` for a `Vec` wire — the type counterpart of
    /// [`Self::slot_expr`], same array-not-tuple rationale.
    fn slot_type(&self, v: &CirVar, base_ty: &IrType) -> IrType {
        match &self.wires[&v.0] {
            WireRepr::Scalar(_) => base_ty.clone(),
            WireRepr::Vec(names) => IrType::Array {
                kind: volar_compiler::ir::ArrayKind::FixedArray,
                elem: Box::new(base_ty.clone()),
                len: volar_compiler::ir::ArrayLength::Const(names.len()),
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

    /// Emit XOR (free: prover a + b, verifier element-wise).
    fn emit_xor(&mut self, out: &str, a: &str, b: &str) {
        if self.role.is_prover() {
            self.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(out),
                ty: None,
                init: Some(ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(clone_expr(var(a))),
                    right: Box::new(clone_expr(var(b))),
                })),
            }));
        } else {
            // Q { q: Array::from_fn(|i| a.q[i].clone() + b.q[i].clone()) }
            self.stmts.push(ir_stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(out),
                ty: None,
                init: Some(q_struct(array_t_from_fn(
                    "i",
                    ir_expr(IrExprKind::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(clone_expr(q_index(a, "i"))),
                        right: Box::new(clone_expr(q_index(b, "i"))),
                    }),
                ))),
            }));
        }
    }

    /// Emit AND gate.  Returns the name of the output wire.
    fn emit_and(&mut self, a: &str, b: &str) -> String {
        let wire_name = format!("and_w_{}", self.and_counter);
        match self.role {
            VoleRole::Prover => {
                let hat_name = format!("hat_{}", self.and_counter);
                self.hat_names.push(hat_name.clone());
                emit_prover_and_gate(a, b, &wire_name, &hat_name, &mut self.stmts, ());
            }
            VoleRole::Verifier => {
                let ok_name = format!("ok_{}", self.and_counter);
                let q_and_name = format!("q_and_{}", self.and_counter);
                let hat_name = format!("hat_{}", self.and_counter);
                self.ok_names.push(ok_name.clone());
                emit_verifier_and_gate(
                    a, b, &wire_name, &ok_name,
                    &q_and_name, &hat_name, &mut self.stmts, (),
                );
                if let Some(sink) = self.trace_sink {
                    let r_param_name = format!("r_and_{}", self.and_counter);
                    let new_state = sink.and_gate_step(
                        self.and_counter, a, b, &wire_name, "delta",
                        &hat_name, &r_param_name, "fold_state", (),
                    );
                    self.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                        left: Box::new(var("fold_state")),
                        right: Box::new(new_state),
                    }))));
                }
            }
            VoleRole::QSim => {
                // hat_k is a required *input* param (same shape Verifier
                // already takes); q_and_k is *derived* here and collected
                // as this function's own output (see `q_and_names`).
                let hat_name = format!("hat_{}", self.and_counter);
                self.hat_names.push(hat_name.clone());
                let q_and_name = format!("q_and_{}", self.and_counter);
                self.q_and_names.push(q_and_name.clone());
                emit_qsim_and_gate(a, b, &wire_name, &q_and_name, &hat_name, &mut self.stmts, ());
            }
        }
        self.and_counter += 1;
        wire_name
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
    fn operand_lane(&self, v: &CirVar, lane: usize) -> String {
        match &self.wires[&v.0] {
            WireRepr::Scalar(s) => s.clone(),
            WireRepr::Vec(parts) => parts[lane].clone(),
        }
    }

    /// Emit one bit-lane of a (possibly wide) `Poly` statement: the exact
    /// single-bit Quicksilver AND/XOR-chain formula this function has
    /// always used, parameterized by `lane` so [`Self::emit_poly`] can
    /// broadcast it across every bit of a `>1`-bit-wide value.
    fn emit_poly_lane(
        &mut self,
        out_name: &str,
        coeffs: &alloc::collections::BTreeMap<Vec<CirVar>, u8>,
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
                    init: Some(clone_expr(var(&term_names[0]))),
                }));
            }
            _ => {
                let first = term_names[0].clone();
                let tmp0 = format!("{}_xor0", out_name);
                self.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::ident(&tmp0),
                    ty: None,
                    init: Some(clone_expr(var(&first))),
                }));
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
        coeffs: &alloc::collections::BTreeMap<Vec<CirVar>, u8>,
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
        coeffs: &alloc::collections::BTreeMap<Vec<CirVar>, u8>,
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
    fn poly_wide_supported(&self, coeffs: &alloc::collections::BTreeMap<Vec<CirVar>, u8>) -> bool {
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
        coeffs: &alloc::collections::BTreeMap<Vec<CirVar>, u8>,
        constant: &volar_ir::ir::Constant,
        width: usize,
    ) -> WireRepr {
        // ---- 1. Bundle every wide operand referenced, once each. -----------
        let mut wide_ops: Vec<CirVar> = Vec::new();
        for mono in coeffs.keys() {
            for v in mono {
                if wide_ops.contains(v) { continue; }
                if matches!(&self.wires[&v.0], WireRepr::Vec(_)) {
                    wide_ops.push(*v);
                }
            }
        }
        let mut bundle: alloc::collections::BTreeMap<u32, String> = alloc::collections::BTreeMap::new();
        for v in &wide_ops {
            let WireRepr::Vec(names) = &self.wires[&v.0] else { unreachable!() };
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
                    let hat_names: Vec<String> = (start..start + width).map(|k| format!("hat_{k}")).collect();
                    b.hat = format!("{out_name}_h{gi}_{start}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&b.hat),
                        ty: None,
                        init: Some(ir_expr(IrExprKind::FixedArray(hat_names.iter().map(|n| var(n)).collect()))),
                    }));
                    let q_names: Vec<String> = (start..start + width).map(|k| format!("q_and_{k}")).collect();
                    b.q_and = format!("{out_name}_q{gi}_{start}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&b.q_and),
                        ty: None,
                        init: Some(ir_expr(IrExprKind::FixedArray(q_names.iter().map(|n| var(n)).collect()))),
                    }));
                    if self.trace_sink.is_some() {
                        let r_names: Vec<String> = (start..start + width).map(|k| format!("r_and_{k}")).collect();
                        b.r = format!("{out_name}_r{gi}_{start}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&b.r),
                            ty: None,
                            init: Some(ir_expr(IrExprKind::FixedArray(r_names.iter().map(|n| var(n)).collect()))),
                        }));
                    }
                }
                VoleRole::QSim => {
                    // Only `hat` is bundled as an *input* (same shape
                    // Verifier takes) -- `q_and` is *derived* per lane
                    // below and collected as an output, no `r` (no fold).
                    let hat_names: Vec<String> = (start..start + width).map(|k| format!("hat_{k}")).collect();
                    b.hat = format!("{out_name}_h{gi}_{start}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&b.hat),
                        ty: None,
                        init: Some(ir_expr(IrExprKind::FixedArray(hat_names.iter().map(|n| var(n)).collect()))),
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

        let operand_expr = |ctx: &Self, v: &CirVar| -> IrExpr {
            match &ctx.wires[&v.0] {
                WireRepr::Scalar(s) => clone_expr(var(s)),
                WireRepr::Vec(_) => clone_expr(arr_index(&bundle[&v.0], "i")),
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
                    let n = format!("_t1_{term_idx}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&n), ty: None, init: Some(operand_expr(self, &mono[0])),
                    }));
                    term_names.push(n);
                }
                2 => {
                    // Bind operands to bare local names first: both the
                    // AND-check call and (verifier) `and_gate_step` need
                    // real in-scope identifiers, not arbitrary expressions.
                    let b = &and_bundles[and_gi];
                    and_gi += 1;
                    let ka_n = format!("_ka_{term_idx}");
                    let kb_n = format!("_kb_{term_idx}");
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&ka_n), ty: None, init: Some(operand_expr(self, &mono[0])),
                    }));
                    self.stmts.push(ir_stmt(IrStmtKind::Let {
                        pattern: IrPattern::ident(&kb_n), ty: None, init: Some(operand_expr(self, &mono[1])),
                    }));
                    let wire_n = format!("_aw_{term_idx}");
                    match self.role {
                        VoleRole::Prover => {
                        let hat_n = format!("_ah_{term_idx}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::Tuple(vec![IrPattern::ident(&wire_n), IrPattern::ident(&hat_n)]),
                            ty: None,
                            init: Some(ir_expr(IrExprKind::Call {
                                func: Box::new(ir_expr(IrExprKind::Path {
                                    segments: vec!["vole_and_prover_step".into()],
                                    type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
                                })),
                                args: vec![clone_expr(var(&ka_n)), clone_expr(var(&kb_n))],
                            })),
                        }));
                        hat_locals.push(hat_n);
                        }
                        VoleRole::Verifier => {
                        let hat_n = format!("_lane_hat_{term_idx}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&hat_n), ty: None,
                            init: Some(clone_expr(arr_index(&b.hat, "i"))),
                        }));
                        let ok_n = format!("_aok_{term_idx}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::Tuple(vec![IrPattern::ident(&wire_n), IrPattern::ident(&ok_n)]),
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
                                b.start, &ka_n, &kb_n, &wire_n, "delta", &hat_n, &r_n, "fold_state", (),
                            );
                            self.stmts.push(ir_stmt(IrStmtKind::Semi(ir_expr(IrExprKind::Assign {
                                left: Box::new(var("fold_state")),
                                right: Box::new(new_state),
                            }))));
                        }
                        }
                        VoleRole::QSim => {
                        // let hat_n = b.hat[i].clone();
                        // let wire_n = derive_and_q::<N, T>(delta, &ka_n, &kb_n, &hat_n);
                        let hat_n = format!("_lane_hat_{term_idx}");
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&hat_n), ty: None,
                            init: Some(clone_expr(arr_index(&b.hat, "i"))),
                        }));
                        self.stmts.push(ir_stmt(IrStmtKind::Let {
                            pattern: IrPattern::ident(&wire_n),
                            ty: None,
                            init: Some(ir_expr(IrExprKind::Call {
                                func: Box::new(ir_expr(IrExprKind::Path {
                                    segments: vec!["derive_and_q".into()],
                                    type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
                                })),
                                args: vec![
                                    var("delta"),
                                    ref_expr(var(&ka_n)),
                                    ref_expr(var(&kb_n)),
                                    ref_expr(var(&hat_n)),
                                ],
                            })),
                        }));
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
                    init: Some(clone_expr(var(&term_names[0]))),
                }));
            }
            _ => {
                let first = term_names[0].clone();
                let tmp0 = "_xor0".to_string();
                self.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::ident(&tmp0), ty: None, init: Some(clone_expr(var(&first))),
                }));
                let mut acc = tmp0;
                for (i, tn) in term_names[1..].iter().enumerate() {
                    let next = if i == term_names.len() - 2 { final_name.clone() } else { format!("_xor{}", i + 1) };
                    self.emit_xor(&next, &acc, tn);
                    acc = next;
                }
            }
        }

        let has_ands = and_count_here > 0;
        let trailing = match self.role {
            VoleRole::Prover if has_ands => ir_expr(IrExprKind::Tuple(vec![
                var(&final_name),
                ir_expr(IrExprKind::Tuple(hat_locals.iter().map(|h| var(h)).collect())),
            ])),
            VoleRole::QSim if has_ands => ir_expr(IrExprKind::Tuple(vec![
                var(&final_name),
                ir_expr(IrExprKind::Tuple(q_and_locals.iter().map(|q| var(q)).collect())),
            ])),
            _ => var(&final_name),
        };
        let body_stmts = core::mem::replace(&mut self.stmts, saved_stmts);
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

        // ---- 5. Trivial per-lane extraction: restores WireRepr::Vec's
        //         exact contract for every downstream consumer. ---
        let extracts_pair = has_ands && matches!(self.role, VoleRole::Prover | VoleRole::QSim);
        let names: Vec<String> = (0..width)
            .map(|k| {
                let ln = format!("{out_name}_{k}");
                let base = if extracts_pair {
                    ir_expr(IrExprKind::Field {
                        base: Box::new(arr_index(&arr_name, &k.to_string())),
                        field: "0".into(),
                    })
                } else {
                    arr_index(&arr_name, &k.to_string())
                };
                self.stmts.push(ir_stmt(IrStmtKind::Let {
                    pattern: IrPattern::ident(&ln), ty: None, init: Some(clone_expr(base)),
                }));
                ln
            })
            .collect();
        if self.role.is_prover() && has_ands {
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
        }
        if self.role == VoleRole::QSim && has_ands {
            // Same group-major ordering as the prover's `hat_names` above
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
            match &self.wires[&v.0] {
                WireRepr::Scalar(s) => names.push(s.clone()),
                WireRepr::Vec(bits) => names.extend(bits.iter().cloned()),
            }
        }
        self.wires.insert(out_id, WireRepr::Vec(names));
        // No runtime code emitted — purely a tracking operation.
    }

    fn emit_shuffle(&mut self, out_name: &str, out_id: u32, result_bits: &[(u8, CirVar)]) {
        if result_bits.len() == 1 {
            let (bit_idx, src_var) = &result_bits[0];
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
        // input param needs 32 independent per-lane parameter wires
        // (`w_{i}_{j}`), not one, matching the param-list generation in
        // `weave_vole_prover_ir_with_mode`/`weave_vole_verifier_ir_with_mode(_and_trace)`.
        for i in 0..p {
            let w = cir_type_width(&block.params[i], types);
            if w <= 1 {
                self.wires.insert(i as u32, WireRepr::Scalar(format!("w_{}", i)));
            } else {
                let bits: Vec<String> = (0..w).map(|j| format!("w_{}_{}", i, j)).collect();
                self.wires.insert(i as u32, WireRepr::Vec(bits));
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
            for j in 0..w {
                params.push(IrParam { name: format!("w_{}_{}", i, j), ty: vope_type() });
            }
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
    let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| var(h)).collect()));
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
            stmts: ctx.stmts,
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
pub fn weave_vole_prover_ir_split(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    mode: &StorageMode,
    boundary: &[volar_ir_passes::MovfuscBlockBoundary],
    accum_info: &volar_ir_passes::MovfuscAccumInfo,
    chunk_size: usize,
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
            for j in 0..w {
                w_params.push(IrParam { name: format!("w_{}_{}", i, j), ty: vope_type() });
            }
        }
    }
    let insert_w_wires = |ctx: &mut VoleIrCtx| {
        for i in 0..num_params {
            let w = cir_type_width(&block.params[i], types);
            if w <= 1 {
                ctx.wires.insert(i as u32, WireRepr::Scalar(format!("w_{}", i)));
            } else {
                let bits: Vec<String> = (0..w).map(|j| format!("w_{}_{}", i, j)).collect();
                ctx.wires.insert(i as u32, WireRepr::Vec(bits));
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

    for (i, b) in boundary.iter().enumerate() {
        let start = (b.start - num_params as u32) as usize;
        let end = (b.end - num_params as u32) as usize;
        let local_stmts = &block.stmts[start..end];
        let local_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(local_stmts, types)
        } else { 0 };
        let local_ext = count_external_primitives_range(local_stmts, types);

        let mut params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
        params.extend(w_params.iter().cloned());
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

        let mut ctx = VoleIrCtx::new(true);
        insert_w_wires(&mut ctx);
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

        let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| var(h)).collect()));
        let hats_ty = hat_array_type(ctx.hat_names.len());

        let mut ret_tuple_tys = vec![is_active_ty.clone(), done_ty.clone()];
        ret_tuple_tys.extend(next_pc_bit_tys.iter().cloned());
        ret_tuple_tys.extend(next_state_tys.iter().cloned());
        ret_tuple_tys.extend(ret_val_tys.iter().cloned());
        ret_tuple_tys.push(hats_ty);

        let mut ret_tuple_exprs = vec![is_active_expr, done_expr];
        ret_tuple_exprs.extend(next_pc_exprs);
        ret_tuple_exprs.extend(next_state_exprs);
        ret_tuple_exprs.extend(ret_val_exprs);
        ret_tuple_exprs.push(hats_expr);

        let func = IrFunction { no_inline: true,
            name: format!("vole_prove_ir_{}_block_{}", name, i),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause.clone(),
            body: IrBlock {
                stmts: ctx.stmts,
                expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
            },
            external_kind: ExternalKind::Normal,
        };
        emit_fn(func);

        interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
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
                let names: Vec<String> = (0..*n).map(|j| format!("{base_name}_{j}")).collect();
                for nm in &names {
                    params.push(IrParam { name: nm.clone(), ty: (**elem).clone() });
                }
                ctx.wires.insert(var_id, WireRepr::Vec(names));
            }
            _ => {
                params.push(IrParam { name: base_name.clone(), ty });
                ctx.wires.insert(var_id, WireRepr::Scalar(base_name));
            }
        }
    }

    let (init_next_state_tys, init_ret_val_tys) = {
        let mut probe_ctx = VoleIrCtx::new(true);
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
        bind_scalar(ctx, params, done_acc, format!("{prefix}_done_acc"), vope_type());
        for (j, &v) in next_pc.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_next_pc_{j}"), vope_type());
        }
        for (k, &v) in next_state.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_next_state_{k}"), init_next_state_tys[k].clone());
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_ret_val_{m}"), init_ret_val_tys[m].clone());
        }
    };
    let running_tys = |pc_width: usize| -> Vec<IrType> {
        let mut tys = vec![vope_type()];
        tys.extend((0..pc_width).map(|_| vope_type()));
        tys.extend(init_next_state_tys.iter().cloned());
        tys.extend(init_ret_val_tys.iter().cloned());
        tys
    };
    let running_exprs = |ctx: &VoleIrCtx, done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| -> Vec<IrExpr> {
        let mut exprs = vec![ctx.slot_expr(&CirVar(done_acc))];
        exprs.extend(next_pc.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs.extend(next_state.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs.extend(ret_vals.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs
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
        params.extend(w_params.iter().cloned());
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

        let mut ctx = VoleIrCtx::new(true);
        insert_w_wires(&mut ctx);
        bind_running(&mut ctx, &mut params, "in", running_done_acc, &running_next_pc, &running_next_state, &running_ret_vals);
        for i in lo..hi {
            let b = &boundary[i];
            let iface = &interfaces[i];
            bind_scalar(&mut ctx, &mut params, b.is_active, format!("is_active_{i}"), iface.is_active_ty.clone());
            bind_scalar(&mut ctx, &mut params, b.done, format!("done_{i}"), iface.done_ty.clone());
            for (j, &v) in b.next_pc_bits.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("next_pc_{i}_{j}"), iface.next_pc_bit_tys[j].clone());
            }
            for (k, &v) in b.next_state.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("next_state_{i}_{k}"), iface.next_state_tys[k].clone());
            }
            for (m, &v) in b.ret_vals.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("ret_val_{i}_{m}"), iface.ret_val_tys[m].clone());
            }
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
        let mut ret_tuple_tys = running_tys(accum_info.init.next_pc.len());
        let hats_ty = hat_array_type(ctx.hat_names.len());
        ret_tuple_tys.push(hats_ty);
        let mut ret_tuple_exprs = running_exprs(&ctx, out_step.done_acc, &out_step.next_pc, &out_step.next_state, &out_step.ret_vals);
        let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| var(h)).collect()));
        ret_tuple_exprs.push(hats_expr);

        let chunk_func = IrFunction { no_inline: true,
            name: format!("vole_prove_ir_{}_accum_chunk_{}", name, chunk_idx),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause.clone(),
            body: IrBlock { stmts: ctx.stmts, expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))) },
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
    params.extend(w_params.iter().cloned());
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
    let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.hat_names.iter().map(|h| var(h)).collect()));
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
            stmts: ctx.stmts,
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
    for k in 0..and_count {
        params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
        params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
        } else {
            for j in 0..w {
                params.push(IrParam { name: format!("w_{}_{}", i, j), ty: q_type() });
            }
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
            stmts: ctx.stmts,
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
    for k in 0..and_count {
        params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
        params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
        params.push(IrParam {
            name: format!("r_and_{}", k),
            ty: IrType::TypeParam(sink.fold_scalar_type_name().into()),
        });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
        } else {
            for j in 0..w {
                params.push(IrParam { name: format!("w_{}_{}", i, j), ty: q_type() });
            }
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
            stmts: ctx.stmts,
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
    for k in 0..and_count {
        params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    for i in 0..num_params {
        let w = cir_type_width(&block.params[i], types);
        if w <= 1 {
            params.push(IrParam { name: format!("w_{}", i), ty: q_type() });
        } else {
            for j in 0..w {
                params.push(IrParam { name: format!("w_{}_{}", i, j), ty: q_type() });
            }
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
    let q_and_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|n| var(n)).collect()));
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
            stmts: ctx.stmts,
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
            for j in 0..w {
                w_params.push(IrParam { name: format!("w_{}_{}", i, j), ty: q_type() });
            }
        }
    }
    let insert_w_wires = |ctx: &mut VoleIrCtx| {
        for i in 0..num_params {
            let w = cir_type_width(&block.params[i], types);
            if w <= 1 {
                ctx.wires.insert(i as u32, WireRepr::Scalar(format!("w_{}", i)));
            } else {
                let bits: Vec<String> = (0..w).map(|j| format!("w_{}_{}", i, j)).collect();
                ctx.wires.insert(i as u32, WireRepr::Vec(bits));
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
                let names: Vec<String> = (0..*n).map(|j| format!("{base_name}_{j}")).collect();
                for nm in &names {
                    params.push(IrParam { name: nm.clone(), ty: (**elem).clone() });
                }
                ctx.wires.insert(var_id, WireRepr::Vec(names));
            }
            _ => {
                params.push(IrParam { name: base_name.clone(), ty });
                ctx.wires.insert(var_id, WireRepr::Scalar(base_name));
            }
        }
    }

    let mut interfaces: Vec<SplitBlockInterface> = Vec::with_capacity(boundary.len());

    for (i, b) in boundary.iter().enumerate() {
        let start = (b.start - num_params as u32) as usize;
        let end = (b.end - num_params as u32) as usize;
        let local_stmts = &block.stmts[start..end];
        let local_and_count = count_ir_ands_no_storage_range(local_stmts, types);
        let local_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(local_stmts, types)
        } else { 0 };
        let local_ext = count_external_primitives_range(local_stmts, types);

        let mut params: Vec<IrParam> = vec![
            IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
        ];
        for k in 0..local_and_count {
            params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
        }
        params.push(IrParam { name: "q_one".into(), ty: q_type() });
        params.extend(w_params.iter().cloned());
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

        let mut ctx = VoleIrCtx::new_qsim();
        insert_w_wires(&mut ctx);
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
        let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|h| var(h)).collect()));

        let mut ret_tuple_tys = vec![is_active_ty.clone(), done_ty.clone()];
        ret_tuple_tys.extend(next_pc_bit_tys.iter().cloned());
        ret_tuple_tys.extend(next_state_tys.iter().cloned());
        ret_tuple_tys.extend(ret_val_tys.iter().cloned());
        ret_tuple_tys.push(hats_ty);

        let mut ret_tuple_exprs = vec![is_active_expr, done_expr];
        ret_tuple_exprs.extend(next_pc_exprs);
        ret_tuple_exprs.extend(next_state_exprs);
        ret_tuple_exprs.extend(ret_val_exprs);
        ret_tuple_exprs.push(hats_expr);

        let func = IrFunction { no_inline: true,
            name: format!("vole_qsim_ir_{}_block_{}", name, i),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause.clone(),
            body: IrBlock {
                stmts: ctx.stmts,
                expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
            },
            external_kind: ExternalKind::Normal,
        };
        emit_fn(func);

        interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
    }

    let (init_next_state_tys, init_ret_val_tys) = {
        let mut probe_ctx = VoleIrCtx::new_qsim();
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
        bind_scalar(ctx, params, done_acc, format!("{prefix}_done_acc"), q_type());
        for (j, &v) in next_pc.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_next_pc_{j}"), q_type());
        }
        for (k, &v) in next_state.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_next_state_{k}"), init_next_state_tys[k].clone());
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_ret_val_{m}"), init_ret_val_tys[m].clone());
        }
    };
    let running_tys = |pc_width: usize| -> Vec<IrType> {
        let mut tys = vec![q_type()];
        tys.extend((0..pc_width).map(|_| q_type()));
        tys.extend(init_next_state_tys.iter().cloned());
        tys.extend(init_ret_val_tys.iter().cloned());
        tys
    };
    let running_exprs = |ctx: &VoleIrCtx, done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| -> Vec<IrExpr> {
        let mut exprs = vec![ctx.slot_expr(&CirVar(done_acc))];
        exprs.extend(next_pc.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs.extend(next_state.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs.extend(ret_vals.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs
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
        for k in 0..chunk_and_count {
            params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
        }
        params.push(IrParam { name: "q_one".into(), ty: q_type() });
        params.extend(w_params.iter().cloned());
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

        let mut ctx = VoleIrCtx::new_qsim();
        insert_w_wires(&mut ctx);
        bind_running(&mut ctx, &mut params, "in", running_done_acc, &running_next_pc, &running_next_state, &running_ret_vals);
        for i in lo..hi {
            let b = &boundary[i];
            let iface = &interfaces[i];
            bind_scalar(&mut ctx, &mut params, b.is_active, format!("is_active_{i}"), iface.is_active_ty.clone());
            bind_scalar(&mut ctx, &mut params, b.done, format!("done_{i}"), iface.done_ty.clone());
            for (j, &v) in b.next_pc_bits.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("next_pc_{i}_{j}"), iface.next_pc_bit_tys[j].clone());
            }
            for (k, &v) in b.next_state.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("next_state_{i}_{k}"), iface.next_state_tys[k].clone());
            }
            for (m, &v) in b.ret_vals.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("ret_val_{i}_{m}"), iface.ret_val_tys[m].clone());
            }
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
        let mut ret_tuple_tys = running_tys(accum_info.init.next_pc.len());
        let hats_ty = q_and_array_type(ctx.q_and_names.len());
        ret_tuple_tys.push(hats_ty);
        let mut ret_tuple_exprs = running_exprs(&ctx, out_step.done_acc, &out_step.next_pc, &out_step.next_state, &out_step.ret_vals);
        let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|h| var(h)).collect()));
        ret_tuple_exprs.push(hats_expr);

        let chunk_func = IrFunction { no_inline: true,
            name: format!("vole_qsim_ir_{}_accum_chunk_{}", name, chunk_idx),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause.clone(),
            body: IrBlock { stmts: ctx.stmts, expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))) },
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
    for k in 0..finish_and_count {
        params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    params.extend(w_params.iter().cloned());
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
    let hats_expr = ir_expr(IrExprKind::FixedArray(ctx.q_and_names.iter().map(|h| var(h)).collect()));
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
            stmts: ctx.stmts,
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
            for j in 0..w {
                w_params.push(IrParam { name: format!("w_{}_{}", i, j), ty: q_type() });
            }
        }
    }
    let insert_w_wires = |ctx: &mut VoleIrCtx| {
        for i in 0..num_params {
            let w = cir_type_width(&block.params[i], types);
            if w <= 1 {
                ctx.wires.insert(i as u32, WireRepr::Scalar(format!("w_{}", i)));
            } else {
                let bits: Vec<String> = (0..w).map(|j| format!("w_{}_{}", i, j)).collect();
                ctx.wires.insert(i as u32, WireRepr::Vec(bits));
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
                let names: Vec<String> = (0..*n).map(|j| format!("{base_name}_{j}")).collect();
                for nm in &names {
                    params.push(IrParam { name: nm.clone(), ty: (**elem).clone() });
                }
                ctx.wires.insert(var_id, WireRepr::Vec(names));
            }
            _ => {
                params.push(IrParam { name: base_name.clone(), ty });
                ctx.wires.insert(var_id, WireRepr::Scalar(base_name));
            }
        }
    }

    let mut interfaces: Vec<SplitBlockInterface> = Vec::with_capacity(boundary.len());

    for (i, b) in boundary.iter().enumerate() {
        let start = (b.start - num_params as u32) as usize;
        let end = (b.end - num_params as u32) as usize;
        let local_stmts = &block.stmts[start..end];
        let local_and_count = count_ir_ands_no_storage_range(local_stmts, types);
        let local_oracle_reads = if matches!(mode, StorageMode::Commitment) {
            count_storage_reads_range(local_stmts, types)
        } else { 0 };
        let local_ext = count_external_primitives_range(local_stmts, types);

        let mut params: Vec<IrParam> = vec![
            IrParam { name: "delta".into(), ty: ref_to_vole(delta_type()) },
        ];
        for k in 0..local_and_count {
            params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
            params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
            params.push(IrParam {
                name: format!("r_and_{}", k),
                ty: IrType::TypeParam(sink.fold_scalar_type_name().into()),
            });
        }
        params.push(IrParam { name: "q_one".into(), ty: q_type() });
        params.extend(w_params.iter().cloned());
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

        let mut ctx = VoleIrCtx::new_verifier_with_trace_sink(sink);
        insert_w_wires(&mut ctx);
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

        let mut ret_tuple_tys = vec![is_active_ty.clone(), done_ty.clone()];
        ret_tuple_tys.extend(next_pc_bit_tys.iter().cloned());
        ret_tuple_tys.extend(next_state_tys.iter().cloned());
        ret_tuple_tys.extend(ret_val_tys.iter().cloned());
        ret_tuple_tys.push(IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool));
        ret_tuple_tys.push(IrType::TypeParam(sink.state_type_name().into()));

        let mut ret_tuple_exprs = vec![is_active_expr, done_expr];
        ret_tuple_exprs.extend(next_pc_exprs);
        ret_tuple_exprs.extend(next_state_exprs);
        ret_tuple_exprs.extend(ret_val_exprs);
        ret_tuple_exprs.push(var("all_ok"));
        ret_tuple_exprs.push(var("fold_state"));

        let func = IrFunction { no_inline: true,
            name: format!("vole_verify_ir_{}_block_{}", name, i),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause_for(sink),
            body: IrBlock {
                stmts: ctx.stmts,
                expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))),
            },
            external_kind: ExternalKind::Normal,
        };
        emit_fn(func);

        interfaces.push(SplitBlockInterface { is_active_ty, done_ty, next_pc_bit_tys, next_state_tys, ret_val_tys });
    }

    // ---- Chunked accumulation: fold blocks in groups, never all at once ----
    //
    // Determine the running accumulator's own next_state/ret_vals slot
    // types once, from `accum_info.init`'s zero-allocation stmts (a small,
    // gate-free range) -- done_acc/next_pc are always Bit-scalar (width 1),
    // needing no such lookup.
    let (init_next_state_tys, init_ret_val_tys) = {
        let mut probe_ctx = VoleIrCtx::new_verifier_with_trace_sink(sink);
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
        bind_scalar(ctx, params, done_acc, format!("{prefix}_done_acc"), q_type());
        for (j, &v) in next_pc.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_next_pc_{j}"), q_type());
        }
        for (k, &v) in next_state.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_next_state_{k}"), init_next_state_tys[k].clone());
        }
        for (m, &v) in ret_vals.iter().enumerate() {
            bind_scalar(ctx, params, v, format!("{prefix}_ret_val_{m}"), init_ret_val_tys[m].clone());
        }
    };
    let running_tys = |prefix: &str, pc_width: usize| -> Vec<IrType> {
        let mut tys = vec![q_type()];
        tys.extend((0..pc_width).map(|_| q_type()));
        tys.extend(init_next_state_tys.iter().cloned());
        tys.extend(init_ret_val_tys.iter().cloned());
        let _ = prefix;
        tys
    };
    let running_exprs = |ctx: &VoleIrCtx, done_acc: u32, next_pc: &[u32], next_state: &[u32], ret_vals: &[u32]| -> Vec<IrExpr> {
        let mut exprs = vec![ctx.slot_expr(&CirVar(done_acc))];
        exprs.extend(next_pc.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs.extend(next_state.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs.extend(ret_vals.iter().map(|&v| ctx.slot_expr(&CirVar(v))));
        exprs
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
        for k in 0..chunk_and_count {
            params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
            params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
            params.push(IrParam { name: format!("r_and_{}", k), ty: IrType::TypeParam(sink.fold_scalar_type_name().into()) });
        }
        params.push(IrParam { name: "q_one".into(), ty: q_type() });
        params.extend(w_params.iter().cloned());
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
            bind_scalar(&mut ctx, &mut params, b.is_active, format!("is_active_{i}"), iface.is_active_ty.clone());
            bind_scalar(&mut ctx, &mut params, b.done, format!("done_{i}"), iface.done_ty.clone());
            for (j, &v) in b.next_pc_bits.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("next_pc_{i}_{j}"), iface.next_pc_bit_tys[j].clone());
            }
            for (k, &v) in b.next_state.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("next_state_{i}_{k}"), iface.next_state_tys[k].clone());
            }
            for (m, &v) in b.ret_vals.iter().enumerate() {
                bind_scalar(&mut ctx, &mut params, v, format!("ret_val_{i}_{m}"), iface.ret_val_tys[m].clone());
            }
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
        let mut ret_tuple_tys = running_tys("out", accum_info.init.next_pc.len());
        ret_tuple_tys.push(IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool));
        ret_tuple_tys.push(IrType::TypeParam(sink.state_type_name().into()));
        let mut ret_tuple_exprs = running_exprs(&ctx, out_step.done_acc, &out_step.next_pc, &out_step.next_state, &out_step.ret_vals);
        ret_tuple_exprs.push(var("all_ok"));
        ret_tuple_exprs.push(var("fold_state"));

        let chunk_func = IrFunction { no_inline: true,
            name: format!("vole_verify_ir_{}_accum_chunk_{}", name, chunk_idx),
            module_path: vec![],
            generics: generics.clone(),
            receiver: None,
            params,
            return_type: Some(IrType::Tuple(ret_tuple_tys)),
            where_clause: where_clause_for(sink),
            body: IrBlock { stmts: ctx.stmts, expr: Some(Box::new(ir_expr(IrExprKind::Tuple(ret_tuple_exprs)))) },
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
    for k in 0..finish_and_count {
        params.push(IrParam { name: format!("q_and_{}", k), ty: q_type() });
        params.push(IrParam { name: format!("hat_{}", k), ty: array_t_n() });
        params.push(IrParam { name: format!("r_and_{}", k), ty: IrType::TypeParam(sink.fold_scalar_type_name().into()) });
    }
    params.push(IrParam { name: "q_one".into(), ty: q_type() });
    params.extend(w_params.iter().cloned());
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
            stmts: ctx.stmts,
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
            stmts: ctx.stmts,
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
            stmts: ctx.stmts,
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
        stmts: ctx.stmts,
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
        stmts: ctx.stmts,
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
        "use volar_spec::vole::setup::derive_and_q;\n",
        "use volar_spec::field::Invert;\n",
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
        let mut coeffs = alloc::collections::BTreeMap::new();
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
        assert!(func.params.iter().any(|p| p.name == "hat_0"), "QSim must take hat_0 as an input: {:?}", func.params.iter().map(|p| &p.name).collect::<std::vec::Vec<_>>());
        assert!(!func.params.iter().any(|p| p.name == "q_and_0"), "QSim must NOT take q_and_0 as an input (it derives it): {:?}", func.params.iter().map(|p| &p.name).collect::<std::vec::Vec<_>>());
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

        assert!(code.contains("r_and_0"), "missing r_and_0 param:\n{code}");
        assert!(!code.contains("r_and_1"), "unexpected r_and_1 for a single-AND-gate circuit:\n{code}");
        assert!(code.contains("IopChallenge"), "missing fold-challenge type:\n{code}");

        assert!(code.contains("hat_0"), "and_gate_step must reference the real hat_0, not a placeholder:\n{code}");
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
        let mut coeffs = alloc::collections::BTreeMap::new();
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
        assert!(code.contains("r_and_0"), "missing r_and_0 for the one AND gate:\n{code}");
        assert!(!code.contains("r_and_1"), "storage ops must not contribute extra fold params:\n{code}");
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
        let mut coeffs = alloc::collections::BTreeMap::new();
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
        // 8 lanes → 8 independent AND-gate folds, one r_and_k each.
        for k in 0..8 {
            assert!(code.contains(&format!("r_and_{k}")), "missing r_and_{k} for lane {k}:\n{code}");
        }
        assert!(!code.contains("r_and_8"), "8-lane AND must not produce a 9th fold param:\n{code}");
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
        assert!(func.params.iter().any(|p| p.name == "hat_0"), "QSim must take hat_0..7 as inputs: {:?}", func.params.iter().map(|p| &p.name).collect::<std::vec::Vec<_>>());
        assert!(func.params.iter().any(|p| p.name == "hat_7"));
        assert!(!func.params.iter().any(|p| p.name == "q_and_0"), "QSim must not take q_and_k as an input");
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
        let mut coeffs0 = alloc::collections::BTreeMap::new();
        coeffs0.insert(std::vec![CirVar(0), CirVar(3)], 1u8); // a AND (storage read)
        let mut coeffs1 = alloc::collections::BTreeMap::new();
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
            &circuit, &types, "split_test", &mode, &IopSink, &boundary, &accum_info, 1,
            |f| funcs.push(f),
        );

        assert_eq!(funcs.len(), 5, "2 blocks + 2 accumulator chunks (chunk_size=1) + 1 finish");
        assert_eq!(funcs[0].name, "vole_verify_ir_split_test_block_0");
        assert_eq!(funcs[1].name, "vole_verify_ir_split_test_block_1");
        assert_eq!(funcs[2].name, "vole_verify_ir_split_test_accum_chunk_0");
        assert_eq!(funcs[3].name, "vole_verify_ir_split_test_accum_chunk_1");
        assert_eq!(funcs[4].name, "vole_verify_ir_split_test_finish");

        // No chunk function's own params reference *every* block -- with
        // chunk_size=1, each accum_chunk function should reference only
        // its own one block's exported vars (is_active_N/done_N/...), not
        // both blocks'.
        assert!(funcs[2].params.iter().any(|p| p.name == "is_active_0"));
        assert!(!funcs[2].params.iter().any(|p| p.name == "is_active_1"));
        assert!(funcs[3].params.iter().any(|p| p.name == "is_active_1"));
        assert!(!funcs[3].params.iter().any(|p| p.name == "is_active_0"));

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
        let q_and_count = |f: &IrFunction| f.params.iter().filter(|p| p.name.starts_with("q_and_")).count();
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

        // Every function shares the same w_i entry-state params.
        for f in &funcs {
            assert!(f.params.iter().any(|p| p.name == "w_0"), "{} missing w_0", f.name);
            assert!(f.params.iter().any(|p| p.name == "w_1"), "{} missing w_1", f.name);
        }

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
            &circuit, &types, "split_test", &mode, &boundary, &accum_info, 1,
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
        weave_vole_prover_ir_split(&circuit, &types, "il", &mode, &boundary, &accum_info, 1, |f| prover_funcs.push(f));
        let mut verifier_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_verifier_ir_split_with_trace(&circuit, &types, "il", &mode, &IopSink, &boundary, &accum_info, 1, |f| verifier_funcs.push(f));

        assert_eq!(prover_funcs.len(), verifier_funcs.len());
        let and_count_of = |f: &IrFunction| f.params.iter().filter(|p| p.name.starts_with("q_and_")).count();
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
        weave_vole_prover_ir_split(&circuit, &types, "qs", &mode, &boundary, &accum_info, 1, |f| prover_funcs.push(f));
        let mut verifier_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        weave_vole_verifier_ir_split_with_trace(&circuit, &types, "qs", &mode, &IopSink, &boundary, &accum_info, 1, |f| verifier_funcs.push(f));
        let mut qsim_funcs: std::vec::Vec<IrFunction> = std::vec::Vec::new();
        let trace = weave_vole_qsim_ir_split(&circuit, &types, "qs", &mode, &boundary, &accum_info, 1, |f| qsim_funcs.push(f));

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
            assert!(!f.params.iter().any(|p| p.name.starts_with("q_and_")), "{} must not take q_and_k as input", f.name);
        }

        let hat_count_of = |f: &IrFunction| f.params.iter().filter(|p| p.name.starts_with("hat_")).count();
        let q_and_count_of = |f: &IrFunction| f.params.iter().filter(|p| p.name.starts_with("q_and_")).count();
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
