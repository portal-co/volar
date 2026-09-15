// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! `BinFheScheme` — FHE weaving for the Track-V2 `volar_spec::binfhe`
//! construction (plan: `docs/fhe/binfhe-v2-implementation-plan.md` §7).
//!
//! Two layers:
//!
//! 1. **[`BinFheScheme`]** implements the existing [`FheScheme`] trait,
//!    emitting typed IR calls into `volar_spec::binfhe`. All Boolean gates
//!    are single-bootstrap LUT evaluations (`binfhe_gate_*`); the AND/OR
//!    tables are the standard negation-free tables of the V2 selector
//!    construction, so XOR needs one bootstrap (not the legacy three).
//!    `cfg_capable()` is `false` for V2.0.
//! 2. **[`build_bootstrap_plan`]** is the cone-fusion pass (plan §7.1,
//!    §7.4): it collapses maximal Boolean cones of `And`/`Or`/`Xor`/`Not`
//!    gates into multi-input LUT ops — one blind rotation per cone —
//!    and emits a validated [`BootstrapPlan`] with topological layering
//!    and a failure budget checked against a caller-supplied bound.
//!    [`weave_binfhe_plan`] renders such a plan into an `IrModule` whose
//!    generated code calls the same executor (`binfhe_lut_read_dyn`) as
//!    the spec's `execute_plan`, so generated-code-vs-interpreter
//!    differential tests compare identical call paths.
//!
//! Non-fusable statements (storage, oracle, action, RNG) panic with
//! descriptive messages in V2.0, same contract as `FheScheme`'s defaults.

use alloc::boxed::Box;
use alloc::format;
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;

use volar_compiler::ir::{
    ExternalKind, IrBlock, IrExpr, IrExprKind, IrFunction, IrGenericParam, IrGenericParamKind,
    IrLit, IrModule, IrParam, IrPattern, IrStmt, IrStmtKind, IrType, PrimitiveType, StructKind,
};
use volar_discipline::{Tagged, Transparent};
use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir_common::Node;
use volar_spec::binfhe::plan::{BootstrapPlan, FailureBudget, LutSpec, PlanOp, ProfileId};

use crate::fhe::FheScheme;
use crate::{clone_expr, ir_expr, ref_expr, var};

// ============================================================================
// IR type helpers for the binfhe shapes
// ============================================================================

fn tp(name: &str) -> IrType {
    IrType::TypeParam(name.into())
}

fn custom(name: &str, args: Vec<IrType>) -> IrType {
    IrType::Struct {
        kind: StructKind::Custom(name.into()),
        type_args: args,
    }
}

fn lwe_ty() -> IrType {
    custom("LweCiphertext", vec![tp("N_LWE")])
}

fn rlwe_ty() -> IrType {
    custom("RlweCiphertext", vec![tp("BIG_N")])
}

fn bk_ty() -> IrType {
    custom(
        "BootstrappingKey",
        vec![tp("N_LWE"), tp("BIG_N"), tp("BS_ELL"), tp("KS_ELL")],
    )
}

fn cbk_ty() -> IrType {
    custom(
        "CircuitBootstrappingKey",
        vec![tp("N_LWE"), tp("BIG_N"), tp("BS_ELL"), tp("KS_ELL"), tp("PRIV_ELL")],
    )
}

fn const_generic(name: &str, is_u32: bool) -> IrGenericParam {
    IrGenericParam {
        name: name.into(),
        kind: IrGenericParamKind::Const,
        const_ty: is_u32.then(|| IrType::Primitive(PrimitiveType::U32)),
        bounds: vec![],
        default: None,
    }
}

/// The 12 const generics of the binfhe call surface. Modulus-log
/// parameters are `u32`; sizes are `usize`.
fn binfhe_generics() -> Vec<IrGenericParam> {
    [
        ("N_LWE", false),
        ("BIG_N", false),
        ("LOG_Q", true),
        ("LOG_Q_LWE", true),
        ("LOG_MOD_KS", true),
        ("BS_ELL", false),
        ("BS_BASE_LOG", true),
        ("KS_ELL", false),
        ("KS_BASE_LOG", true),
        ("PRIV_ELL", false),
        ("PRIV_BASE_LOG", true),
        ("K_MAX", false),
    ]
    .iter()
    .map(|(s, u)| const_generic(s, *u))
    .collect()
}

/// Type args for the 10-const gate surface (binfhe_gate_* / binfhe_cmux).
fn gate_tys() -> Vec<IrType> {
    [
        "N_LWE", "BIG_N", "LOG_Q", "LOG_Q_LWE", "LOG_MOD_KS",
        "BS_ELL", "BS_BASE_LOG", "KS_ELL", "KS_BASE_LOG", "K_MAX",
    ]
    .iter()
    .map(|s| tp(s))
    .collect()
}

/// Type args for the 9-const executor surface (binfhe_lut_read_dyn).
fn exec_tys() -> Vec<IrType> {
    [
        "N_LWE", "BIG_N", "LOG_Q", "LOG_Q_LWE", "LOG_MOD_KS",
        "BS_ELL", "BS_BASE_LOG", "KS_ELL", "KS_BASE_LOG",
    ]
    .iter()
    .map(|s| tp(s))
    .collect()
}

/// Function path for generated code: single-segment (turbofish binds
/// correctly; the module arg documents the origin and consumers import the
/// function by name — the same pattern as `TfheScheme`).
fn binfhe_path<Q: Clone + Default>(_module: &str, func: &str, type_args: Vec<IrType>) -> IrExpr<Q> {
    ir_expr(IrExprKind::Path {
        segments: vec![func.into()],
        type_args,
    })
}

/// `volar_spec::binfhe::lwe::wire_delta::<LOG_Q_LWE>(K_MAX)`
fn delta_expr<Q: Clone + Default>() -> IrExpr<Q> {
    ir_expr(IrExprKind::Call {
        func: Box::new(binfhe_path("lwe", "wire_delta", vec![tp("LOG_Q_LWE")])),
        args: vec![var("K_MAX")],
    })
}

fn call<Q: Clone + Default>(func: IrExpr<Q>, args: Vec<IrExpr<Q>>) -> IrExpr<Q> {
    ir_expr(IrExprKind::Call {
        func: Box::new(func),
        args,
    })
}

fn let_stmt<Q: Clone + Default>(name: &str, init: IrExpr<Q>) -> IrStmt<Q> {
    IrStmt::new(
        IrStmtKind::Let {
            pattern: IrPattern::ident(name),
            ty: None,
            init: Some(init),
        },
        Q::default(),
        None,
    )
}

// ============================================================================
// BinFheScheme — FheScheme impl over the V2 gate surface
// ============================================================================

/// Scheme configuration for the V2 `binfhe` construction.
///
/// The flat path lowers `IRBlocks` to a movfuscated Boolean circuit and
/// emits `binfhe_gate_*` calls. All gate calls take the `bk` parameter
/// (`&BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>`); the generated
/// function is generic over the 12 binfhe const parameters.
pub struct BinFheScheme;

impl BinFheScheme {
    pub fn new() -> Self {
        BinFheScheme
    }
}

impl Default for BinFheScheme {
    fn default() -> Self {
        Self::new()
    }
}

impl FheScheme for BinFheScheme {
    fn wire_type(&self) -> IrType {
        lwe_ty()
    }

    fn extra_params(&self) -> Vec<IrParam> {
        vec![IrParam {
            name: "bk".into(),
            ty: crate::ref_to(bk_ty()),
        }]
    }

    fn generics(&self) -> Vec<IrGenericParam> {
        binfhe_generics()
    }

    fn fn_name_suffix(&self) -> &str {
        "binfhe"
    }

    fn emit_zero<Q: Clone + Default>(&self) -> IrExpr<Q> {
        call(
            binfhe_path("lwe", "binfhe_trivial", vec![tp("N_LWE"), tp("LOG_Q_LWE")]),
            vec![ir_expr(IrExprKind::Lit(IrLit::Bool(false))), delta_expr()],
        )
    }

    fn emit_one<Q: Clone + Default>(&self) -> IrExpr<Q> {
        call(
            binfhe_path("lwe", "binfhe_trivial", vec![tp("N_LWE"), tp("LOG_Q_LWE")]),
            vec![ir_expr(IrExprKind::Lit(IrLit::Bool(true))), delta_expr()],
        )
    }

    fn emit_xor<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        call(
            binfhe_path("pbs", "binfhe_gate_xor", gate_tys()),
            vec![a, b, var("bk")],
        )
    }

    fn emit_not<Q: Clone + Default>(&self, a: IrExpr<Q>) -> IrExpr<Q> {
        call(
            binfhe_path("lwe", "binfhe_not", vec![tp("N_LWE"), tp("LOG_Q_LWE")]),
            vec![a, delta_expr()],
        )
    }

    fn emit_and<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>, _gate_idx: usize) -> IrExpr<Q> {
        call(
            binfhe_path("pbs", "binfhe_gate_and", gate_tys()),
            vec![a, b, var("bk")],
        )
    }

    fn emit_or<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        call(
            binfhe_path("pbs", "binfhe_gate_or", gate_tys()),
            vec![a, b, var("bk")],
        )
    }

    fn emit_cmux<Q: Clone + Default>(&self, sel: IrExpr<Q>, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        call(
            binfhe_path("pbs", "binfhe_cmux", gate_tys()),
            vec![sel, a, b, var("bk")],
        )
    }

    fn promote_to_wire<Q: Clone + Default>(&self, expr: IrExpr<Q>, width: usize) -> IrExpr<Q> {
        if width <= 1 {
            return call(
                binfhe_path("lwe", "binfhe_trivial", vec![tp("N_LWE"), tp("LOG_Q_LWE")]),
                vec![expr, delta_expr()],
            );
        }
        // [bool; width] → [LweCiphertext; width]: promote each element.
        ir_expr(IrExprKind::FixedArray(
            (0..width)
                .map(|bit| {
                    call(
                        binfhe_path("lwe", "binfhe_trivial", vec![tp("N_LWE"), tp("LOG_Q_LWE")]),
                        vec![
                            ir_expr(IrExprKind::Index {
                                base: Box::new(expr.clone()),
                                index: Box::new(ir_expr(IrExprKind::Lit(IrLit::Int(bit as i128)))),
                            }),
                            delta_expr(),
                        ],
                    )
                })
                .collect(),
        ))
    }
}

// ============================================================================
// Cone-fusion plan builder (plan §7.1, §7.4, §7.5)
// ============================================================================

/// Errors from [`build_bootstrap_plan`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanBuildError {
    /// The circuit must be a single block with a `Jmp(Return)` terminator
    /// (the post-movfuscation flat shape).
    NotAFlatCircuit,
    /// The circuit declares static storage, so it is not a pure region.
    HasPreInitialization,
    /// A statement kind that V2.0 plan building does not fuse.
    UnsupportedStmt(&'static str),
    /// The required failure budget exceeds the caller's bound.
    BudgetExceeded {
        /// `log2` total failure bound implied by the fused schedule.
        required_total_log2: u32,
    },
}

/// A fusible Boolean cone: canonical-input truth table.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Cone {
    /// Circuit var ids, sorted, deduplicated.
    inputs: Vec<u32>,
    /// Address-ordered truth table (`addr = sum_j bit_j 2^j`, LSB first).
    table: Vec<bool>,
}

impl Cone {
    fn identity(v: u32) -> Self {
        Cone {
            inputs: vec![v],
            table: vec![false, true],
        }
    }

    fn is_identity(&self) -> bool {
        self.inputs.len() == 1 && self.table == [false, true]
    }
}

/// Per-wire fusion state.
#[derive(Clone, Debug)]
enum ConeState {
    /// Cleartext constant.
    Const(bool),
    /// Fusible cone, not yet scheduled.
    Virtual(Cone),
    /// Already scheduled into the plan at this wire id.
    Materialized(u32),
    /// Cone that must be materialized before further use; as an input to
    /// further fusion it acts as a singleton.
    Pinned(Cone),
}

/// `cone(a) OP cone(b)` as a canonical-input table over the merged inputs.
fn combine_tables(a: &Cone, b: &Cone, op: fn(bool, bool) -> bool) -> Cone {
    let mut inputs = a.inputs.clone();
    for &v in &b.inputs {
        if let Err(pos) = inputs.binary_search(&v) {
            inputs.insert(pos, v);
        }
    }
    let k = inputs.len();
    let mut table = vec![false; 1usize << k];
    let a_index: Vec<Option<usize>> = a
        .inputs
        .iter()
        .map(|v| inputs.binary_search(v).ok())
        .collect();
    let b_index: Vec<Option<usize>> = b
        .inputs
        .iter()
        .map(|v| inputs.binary_search(v).ok())
        .collect();
    for addr in 0..(1usize << k) {
        let mut av = 0usize;
        for (j, g) in a_index.iter().enumerate() {
            av |= ((addr >> g.unwrap()) & 1) << j;
        }
        let mut bv = 0usize;
        for (j, g) in b_index.iter().enumerate() {
            bv |= ((addr >> g.unwrap()) & 1) << j;
        }
        table[addr] = op(a.table[av], b.table[bv]);
    }
    Cone { inputs, table }
}

/// Apply `op` pointwise to a table (NOT fusion).
fn not_table(table: &[bool]) -> Vec<bool> {
    table.iter().map(|b| !*b).collect()
}

/// The current cone a wire contributes as an input to further fusion.
fn input_cone(state: &ConeState) -> Option<Cone> {
    match state {
        ConeState::Virtual(c) => Some(c.clone()),
        _ => None, // constants/materialized/pinned handled by the caller
    }
}

/// Build a fused [`BootstrapPlan`] from a movfuscated Boolean circuit.
///
/// * `circuit`: single-block `BIrBlocks` (post-movfuscation shape).
/// * `k_max`: maximum LUT arity (cone input count) admitted.
/// * `budget`: (`per_bootstrap_log2`, `max_total_log2`); building fails
///   with [`PlanBuildError::BudgetExceeded`] if the fused schedule needs
///   more than `max_total_log2`.
/// Build a plan together with its exact pure-region source binding.
///
/// The binding preserves the original Boolar input and output variable ids.
/// Consumers that map a plan into another representation must use these lists
/// rather than assuming generated wire ids are source ids.
pub fn build_bootstrap_plan_with_binding<P: Clone>(
    circuit: &BIrBlocks<P>,
    k_max: usize,
    budget: (u32, u32),
    profile: ProfileId,
) -> Result<(BootstrapPlan, PlanRegionBinding), PlanBuildError> {
    if !circuit.pre_init.is_empty() {
        return Err(PlanBuildError::HasPreInitialization);
    }
    let plan = build_bootstrap_plan_inner(circuit, k_max, budget, profile)?;
    let block = &circuit.blocks[0];
    let volar_ir::boolar::BIrTerminator::Jmp(target) = &block.terminator else {
        unreachable!("build_bootstrap_plan_inner validated the terminator");
    };
    Ok((
        plan,
        PlanRegionBinding {
            input_vars: (0..block.params).collect(),
            output_vars: target.args.iter().map(|v| v.0).collect(),
        },
    ))
}

/// Build a fused [`BootstrapPlan`] from a pure movfuscated Boolean circuit.
///
/// This compatibility helper discards the source binding. New cross-IR
/// consumers should use [`build_bootstrap_plan_with_binding`].
pub fn build_bootstrap_plan<P: Clone>(
    circuit: &BIrBlocks<P>,
    k_max: usize,
    budget: (u32, u32),
    profile: ProfileId,
) -> Result<BootstrapPlan, PlanBuildError> {
    build_bootstrap_plan_with_binding(circuit, k_max, budget, profile).map(|(plan, _)| plan)
}

/// Ordered original Boolar variable ids bound to a pure plan region.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PlanRegionBinding {
    /// Source variables imported as `BootstrapPlan` wire inputs.
    pub input_vars: Vec<u32>,
    /// Source variables returned by the region in plan-output order.
    pub output_vars: Vec<u32>,
}

fn build_bootstrap_plan_inner<P: Clone>(
    circuit: &BIrBlocks<P>,
    k_max: usize,
    budget: (u32, u32),
    profile: ProfileId,
) -> Result<BootstrapPlan, PlanBuildError> {
    if circuit.blocks.len() != 1 {
        return Err(PlanBuildError::NotAFlatCircuit);
    }
    let block = &circuit.blocks[0];
    let volar_ir::boolar::BIrTerminator::Jmp(target) = &block.terminator else {
        return Err(PlanBuildError::NotAFlatCircuit);
    };
    let output_vars: Vec<u32> = target.args.iter().map(|v| v.0).collect();

    let num_inputs = block.params as u32;
    let mut states: Vec<ConeState> = (0..num_inputs)
        .map(|i| ConeState::Materialized(i))
        .collect();
    let mut ops: Vec<PlanOp> = Vec::new();
    let mut luts: Vec<LutSpec> = Vec::new();
    let mut wire_count = num_inputs;

    // Materialize a wire into the plan (deduplicating constants and
    // identity aliases). Returns the plan wire id.
    fn materialize(
        v: u32,
        states: &mut Vec<ConeState>,
        ops: &mut Vec<PlanOp>,
        luts: &mut Vec<LutSpec>,
        wire_count: &mut u32,
    ) -> u32 {
        match &states[v as usize] {
            ConeState::Materialized(id) => *id,
            ConeState::Const(b) => {
                let b = *b;
                let id = *wire_count;
                *wire_count += 1;
                ops.push(PlanOp::Const { out: id, value: b });
                states[v as usize] = ConeState::Materialized(id);
                id
            }
            ConeState::Virtual(_) | ConeState::Pinned(_) => {
                // Take the cone out to satisfy the borrow checker while we
                // recursively materialize its inputs.
                let cone = match core::mem::replace(
                    &mut states[v as usize],
                    ConeState::Const(false),
                ) {
                    ConeState::Virtual(c) | ConeState::Pinned(c) => c,
                    _ => unreachable!(),
                };
                let id = if cone.is_identity() {
                    materialize(cone.inputs[0], states, ops, luts, wire_count)
                } else if volar_spec::binfhe::lut::table_is_constant(&cone.table) {
                    let id = *wire_count;
                    *wire_count += 1;
                    ops.push(PlanOp::Const {
                        out: id,
                        value: cone.table[0],
                    });
                    id
                } else {
                    let input_ids: alloc::vec::Vec<u32> = cone
                        .inputs
                        .iter()
                        .map(|&i| materialize(i, states, ops, luts, wire_count))
                        .collect();
                    let table_id = luts.len() as u32;
                    luts.push(LutSpec {
                        entries: cone.table.clone(),
                    });
                    let id = *wire_count;
                    *wire_count += 1;
                    ops.push(PlanOp::Lut {
                        inputs: volar_spec::binfhe::plan::LutInputs::from_slice(&input_ids),
                        table: table_id,
                        out: id,
                    });
                    id
                };
                states[v as usize] = ConeState::Materialized(id);
                id
            }
        }
    }

    // Pin a wire: its cone must be materialized before it feeds further
    // fusion (fusion size bound hit).
    fn pin(v: u32, states: &mut Vec<ConeState>) {
        if let ConeState::Virtual(_) = &states[v as usize] {
            let c = match core::mem::replace(&mut states[v as usize], ConeState::Const(false)) {
                ConeState::Virtual(c) => c,
                _ => unreachable!(),
            };
            states[v as usize] = ConeState::Pinned(c);
        }
    }

    for stmt in &block.stmts {
        let result_var = (states.len()) as u32;
        let new_state = match &stmt.kind {
            BIrStmt::Zero => ConeState::Const(false),
            BIrStmt::One => ConeState::Const(true),
            BIrStmt::Not(a) => match &states[a.0 as usize] {
                ConeState::Const(b) => ConeState::Const(!*b),
                _ => {
                    let c = input_cone(&states[a.0 as usize]).unwrap_or_else(|| Cone::identity(a.0));
                    ConeState::Virtual(Cone {
                        inputs: c.inputs,
                        table: not_table(&c.table),
                    })
                }
            },
            BIrStmt::And(a, b) | BIrStmt::Or(a, b) | BIrStmt::Xor(a, b) => {
                let op: fn(bool, bool) -> bool = match &stmt.kind {
                    BIrStmt::And(..) => |x, y| x && y,
                    BIrStmt::Or(..) => |x, y| x || y,
                    _ => |x, y| x ^ y,
                };
                let sa = &states[a.0 as usize];
                let sb = &states[b.0 as usize];
                // Constant folding first.
                if let (ConeState::Const(x), ConeState::Const(y)) = (sa, sb) {
                    ConeState::Const(op(*x, *y))
                } else if let ConeState::Const(x) = sa {
                    // Merge constant into the other cone's table.
                    let cb = input_cone(sb).unwrap_or_else(|| Cone::identity(b.0));
                    let mut table = Vec::with_capacity(cb.table.len());
                    for addr in 0..cb.table.len() {
                        table.push(op(*x, cb.table[addr]));
                    }
                    ConeState::Virtual(Cone {
                        inputs: cb.inputs,
                        table,
                    })
                } else if let ConeState::Const(y) = sb {
                    let ca = input_cone(sa).unwrap_or_else(|| Cone::identity(a.0));
                    let mut table = Vec::with_capacity(ca.table.len());
                    for addr in 0..ca.table.len() {
                        table.push(op(ca.table[addr], *y));
                    }
                    ConeState::Virtual(Cone {
                        inputs: ca.inputs,
                        table,
                    })
                } else {
                    let ca = input_cone(sa).unwrap_or_else(|| Cone::identity(a.0));
                    let cb = input_cone(sb).unwrap_or_else(|| Cone::identity(b.0));
                    let merged = combine_tables(&ca, &cb, op);
                    if merged.inputs.len() <= k_max {
                        ConeState::Virtual(merged)
                    } else {
                        // Over budget: pin both inputs and start a fresh
                        // two-input cone at this wire.
                        pin(a.0, &mut states);
                        pin(b.0, &mut states);
                        let a_cone = Cone::identity(a.0);
                        let b_cone = Cone::identity(b.0);
                        ConeState::Virtual(combine_tables(&a_cone, &b_cone, op))
                    }
                }
            }
            BIrStmt::StorageRead { .. } | BIrStmt::StorageWrite { .. } => {
                return Err(PlanBuildError::UnsupportedStmt("storage"))
            }
            BIrStmt::OracleCall { .. } | BIrStmt::OracleBit { .. } => {
                return Err(PlanBuildError::UnsupportedStmt("oracle"))
            }
            BIrStmt::ActionCall { .. } | BIrStmt::ActionBit { .. } => {
                return Err(PlanBuildError::UnsupportedStmt("action"))
            }
            other => {
                let _ = other;
                return Err(PlanBuildError::UnsupportedStmt("other"));
            }
        };
        states.push(new_state);
        debug_assert_eq!(states.len() as u32, result_var + 1);
    }

    // Materialize outputs (and all pinned wires, in order).
    let mut outputs = Vec::with_capacity(output_vars.len());
    for v in 0..states.len() as u32 {
        if matches!(states[v as usize], ConeState::Pinned(_)) {
            materialize(v, &mut states, &mut ops, &mut luts, &mut wire_count);
        }
    }
    for &v in &output_vars {
        outputs.push(materialize(v, &mut states, &mut ops, &mut luts, &mut wire_count));
    }

    // Failure budget: one blind rotation per non-constant LUT op (plus one
    // per CB level, of which this pass schedules none).
    let (per_bootstrap_log2, max_total_log2) = budget;
    let bootstrap_count = ops
        .iter()
        .filter(|op| matches!(op, PlanOp::Lut { .. } | PlanOp::CircuitBootstrap { .. }))
        .count() as u64;
    let ceil_log2 = if bootstrap_count <= 1 {
        0
    } else {
        (64 - (bootstrap_count - 1).leading_zeros()) as u32
    };
    let required_total_log2 = per_bootstrap_log2 + ceil_log2;
    if required_total_log2 > max_total_log2 {
        return Err(PlanBuildError::BudgetExceeded { required_total_log2 });
    }

    // Topological layering: layer[op] = 1 + max(layer of its inputs).
    let mut wire_layer: Vec<usize> = vec![0; num_inputs as usize];
    let mut layers: Vec<Vec<PlanOp>> = Vec::new();
    for op in ops {
        let layer = match &op {
            PlanOp::Const { .. } => 0,
            PlanOp::Not { input, .. } => wire_layer[*input as usize] + 1,
            PlanOp::Lut { inputs, .. } => {
                inputs.as_slice().iter().map(|w| wire_layer[*w as usize]).max().unwrap_or(0) + 1
            }
            PlanOp::CircuitBootstrap { input, .. } => wire_layer[*input as usize] + 1,
            PlanOp::RgswMux { .. } => 0, // scheduled after its selector layer by construction
        };
        if layers.len() <= layer {
            layers.resize_with(layer + 1, Vec::new);
        }
        if let Some(out) = match &op {
            PlanOp::Const { out, .. } | PlanOp::Not { out, .. } | PlanOp::Lut { out, .. } => Some(*out),
            _ => None,
        } {
            if wire_layer.len() <= out as usize {
                wire_layer.resize(out as usize + 1, 0);
            }
            wire_layer[out as usize] = layer;
        }
        layers[layer].push(op);
    }
    // Drop empty leading/intermediate layers (constants at layer 0 may be
    // the only occupants of later-empty layers).
    layers.retain(|l| !l.is_empty());

    Ok(BootstrapPlan {
        profile,
        k_max: k_max as u32,
        luts,
        layers,
        num_inputs,
        num_cells: 0,
        outputs,
        cell_outputs: vec![],
        budget: FailureBudget {
            per_bootstrap_log2,
            total_log2: required_total_log2,
        },
    })
}

// ============================================================================
// Plan code generation (weaver consumer of the plan)
// ============================================================================

/// Render a validated [`BootstrapPlan`] into an `IrModule` calling
/// `volar_spec::binfhe`. The generated function is generic over the 12
/// binfhe const parameters; its signature is
///
/// ```rust,ignore
/// fn {name}_binfhe<...>(
///     bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
///     cbk: &CircuitBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL, PRIV_ELL>, // only if CB ops
///     input_0..: LweCiphertext<N_LWE>,
///     cell_0..: RlweCiphertext<BIG_N>,                                       // only if cell ops
/// ) -> ...
/// ```
///
/// Panics if the plan fails `validate()` — call it first for diagnosable
/// errors.
pub fn weave_binfhe_plan(plan: &BootstrapPlan, name: &str) -> Tagged<Transparent, IrModule<IrFunction>> {
    plan.validate().expect("weave_binfhe_plan: invalid plan");

    let has_cb = plan
        .layers
        .iter()
        .flatten()
        .any(|op| matches!(op, PlanOp::CircuitBootstrap { .. } | PlanOp::RgswMux { .. }));

    let mut params: Vec<IrParam> = vec![IrParam {
        name: "bk".into(),
        ty: crate::ref_to(bk_ty()),
    }];
    if has_cb {
        params.push(IrParam {
            name: "cbk".into(),
            ty: crate::ref_to(cbk_ty()),
        });
    }
    for i in 0..plan.num_inputs {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: lwe_ty(),
        });
    }
    for i in 0..plan.num_cells {
        params.push(IrParam {
            name: format!("cell_{}", i),
            ty: rlwe_ty(),
        });
    }

    let mut stmts: Vec<IrStmt> = Vec::new();
    // Bind plan arena ids to function parameters.
    for i in 0..plan.num_inputs {
        stmts.push(let_stmt(
            &format!("w_{}", i),
            var(&format!("input_{}", i)),
        ));
    }
    for i in 0..plan.num_cells {
        stmts.push(let_stmt(
            &format!("c_{}", i),
            var(&format!("cell_{}", i)),
        ));
    }
    for layer in &plan.layers {
        for op in layer {
            match op {
                PlanOp::Const { out, value } => {
                    let init = call(
                        binfhe_path("lwe", "binfhe_trivial", vec![tp("N_LWE"), tp("LOG_Q_LWE")]),
                        vec![ir_expr(IrExprKind::Lit(IrLit::Bool(*value))), delta_expr()],
                    );
                    stmts.push(let_stmt(&format!("w_{}", out), init));
                }
                PlanOp::Not { input, out } => {
                    let init = call(
                        binfhe_path("lwe", "binfhe_not", vec![tp("N_LWE"), tp("LOG_Q_LWE")]),
                        vec![var(&format!("w_{}", input)), delta_expr()],
                    );
                    stmts.push(let_stmt(&format!("w_{}", out), init));
                }
                PlanOp::Lut { inputs, table, out } => {
                    let spec = &plan.luts[*table as usize];
                    let input_array = ref_expr(ir_expr(IrExprKind::Array(
                        inputs
                            .as_slice()
                            .iter()
                            .map(|w| clone_expr(var(&format!("w_{}", w))))
                            .collect(),
                    )));
                    let table_array = ref_expr(ir_expr(IrExprKind::Array(
                        spec.entries
                            .iter()
                            .map(|b| ir_expr(IrExprKind::Lit(IrLit::Bool(*b))))
                            .collect(),
                    )));
                    let init = call(
                        binfhe_path("pbs", "binfhe_lut_read_dyn", exec_tys()),
                        vec![input_array, table_array, var("K_MAX"), var("bk")],
                    );
                    stmts.push(let_stmt(&format!("w_{}", out), init));
                }
                PlanOp::CircuitBootstrap { input, out } => {
                    let init = call(
                        binfhe_path(
                            "circuit_bs",
                            "circuit_bootstrap",
                            [
                                "N_LWE", "BIG_N", "LOG_Q", "LOG_Q_LWE", "BS_ELL", "BS_BASE_LOG",
                                "KS_ELL", "PRIV_ELL", "PRIV_BASE_LOG",
                            ]
                            .iter()
                            .map(|s| tp(s))
                            .collect(),
                        ),
                        vec![var(&format!("w_{}", input)), var("cbk"), var("K_MAX")],
                    );
                    stmts.push(let_stmt(&format!("r_{}", out), init));
                }
                PlanOp::RgswMux { sel, then_cell, else_cell, out } => {
                    let init = call(
                        binfhe_path(
                            "rgsw",
                            "cmux",
                            ["BIG_N", "LOG_Q", "BS_ELL", "BS_BASE_LOG"]
                                .iter()
                                .map(|s| tp(s))
                                .collect(),
                        ),
                        vec![
                            var(&format!("r_{}", sel)),
                            var(&format!("c_{}", then_cell)),
                            var(&format!("c_{}", else_cell)),
                        ],
                    );
                    stmts.push(let_stmt(&format!("c_{}", out), init));
                }
            }
        }
    }

    // Outputs: single wire → bare expr; multiple → tuple.
    let mut ret_elems: Vec<IrExpr> = plan
        .outputs
        .iter()
        .map(|w| var::<()>(&format!("w_{}", w)))
        .collect();
    ret_elems.extend(
        plan.cell_outputs
            .iter()
            .map(|c| var::<()>(&format!("c_{}", c))),
    );
    let (ret_expr, ret_type) = if ret_elems.len() == 1 {
        let ty = if !plan.outputs.is_empty() && plan.cell_outputs.is_empty() {
            lwe_ty()
        } else {
            rlwe_ty()
        };
        (ret_elems.pop().unwrap(), Some(ty))
    } else {
        let types = plan
            .outputs
            .iter()
            .map(|_| lwe_ty())
            .chain(plan.cell_outputs.iter().map(|_| rlwe_ty()))
            .collect();
        (ir_expr(IrExprKind::Tuple(ret_elems)), Some(IrType::Tuple(types)))
    };

    let fn_name = format!("{}_binfhe", name);
    let func = IrFunction {
        no_inline: false,
        name: fn_name.clone(),
        module_path: vec![],
        generics: binfhe_generics(),
        receiver: None,
        params,
        return_type: ret_type,
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    Tagged::seal(IrModule {
        name: format!("weaved_{}", fn_name),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    })
}

/// Convenience: fuse a movfuscated circuit and emit the plan function.
pub fn weave_binfhe_fused<P: Clone>(
    circuit: &BIrBlocks<P>,
    name: &str,
    k_max: usize,
    budget: (u32, u32),
    profile: ProfileId,
) -> Result<(BootstrapPlan, Tagged<Transparent, IrModule<IrFunction>>), PlanBuildError> {
    let plan = build_bootstrap_plan(circuit, k_max, budget, profile)?;
    let module = weave_binfhe_plan(&plan, name);
    Ok((plan, module))
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use std::string::ToString;

    use crate::tests_common::{build_and_circuit, build_xor_and_circuit};
    use volar_ir::boolar::{BIrBlock, BIrTerminator, BIrTarget};
    use volar_ir::ir::{IRBlockTargetId, IRVarId};
    use volar_spec::binfhe::plan::{BootstrapPlan, PlanOp, ProfileId};

    fn xor_and_or_circuit() -> BIrBlocks {
        // (a XOR b) AND (a OR b) with an internal shared subterm to
        // exercise cone merging over 3+ inputs.
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: 2,
                stmts: vec![
                    Node::new(BIrStmt::Xor(IRVarId(0), IRVarId(1)), (), None),
                    Node::new(BIrStmt::Or(IRVarId(0), IRVarId(1)), (), None),
                    Node::new(BIrStmt::And(IRVarId(2), IRVarId(3)), (), None),
                ],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(4)],
                }),
            }],
            pre_init: vec![],
        }
    }

    #[test]
    fn binding_preserves_source_variables_and_rejects_preinitialization() {
        let circuit = xor_and_or_circuit();
        let (plan, binding) = build_bootstrap_plan_with_binding(
            &circuit,
            4,
            (30, 34),
            ProfileId::Toy,
        )
        .unwrap();
        assert_eq!(binding.input_vars, vec![0, 1]);
        assert_eq!(binding.output_vars, vec![4]);
        assert_eq!(plan.num_inputs, binding.input_vars.len() as u32);
        assert_eq!(plan.outputs.len(), binding.output_vars.len());

        let mut with_preinit = circuit;
        with_preinit.pre_init.push(volar_ir::boolar::BIrPreInitSegment {
            storage: volar_ir_common::StorageId(0),
            lane: volar_ir::boolar::LaneId(0),
            addr: vec![],
            data: vec![true],
        });
        assert!(matches!(
            build_bootstrap_plan_with_binding(&with_preinit, 4, (30, 34), ProfileId::Toy),
            Err(PlanBuildError::HasPreInitialization)
        ));
    }

    #[test]
    fn fusion_collapses_cones_into_single_luts() {
        let plan = build_bootstrap_plan(&xor_and_or_circuit(), 4, (30, 34), ProfileId::Toy).unwrap();
        plan.validate().unwrap();
        // The whole DAG is one 2-input cone: one LUT op, one layer.
        let lut_ops: Vec<_> = plan
            .layers
            .iter()
            .flatten()
            .filter(|op| matches!(op, PlanOp::Lut { .. }))
            .collect();
        assert_eq!(lut_ops.len(), 1, "expected one fused LUT, plan: {plan:?}");
        assert_eq!(plan.luts.len(), 1);
        // Table for (a ^ b) && (a || b) = a ^ b: [false, true, true, false].
        assert_eq!(plan.luts[0].entries, vec![false, true, true, false]);
        assert_eq!(plan.bootstrap_op_count(), 1);
        // Budget: per=30 + ceil_log2(1)=0 -> 30; bound 34 ok.
        assert_eq!(plan.budget.total_log2, 30);
    }

    #[test]
    fn fusion_respects_arity_bound_and_budget() {
        // Chain of XORs over 4 inputs: inputs grow to arity 4; with
        // k_max = 2 the pass must pin and emit multiple LUTs.
        let c = BIrBlocks {
            blocks: vec![BIrBlock {
                params: 4,
                stmts: vec![
                    Node::new(BIrStmt::Xor(IRVarId(0), IRVarId(1)), (), None),
                    Node::new(BIrStmt::Xor(IRVarId(4), IRVarId(2)), (), None),
                    Node::new(BIrStmt::Xor(IRVarId(5), IRVarId(3)), (), None),
                ],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(6)],
                }),
            }],
            pre_init: vec![],
        };
        let plan = build_bootstrap_plan(&c, 2, (30, 40), ProfileId::Toy).unwrap();
        plan.validate().unwrap();
        // 3 two-input LUTs (pinned at arity 2).
        let lut_count = plan
            .layers
            .iter()
            .flatten()
            .filter(|op| matches!(op, PlanOp::Lut { .. }))
            .count();
        assert_eq!(lut_count, 3, "expected 3 LUTs, plan: {plan:?}");
        assert_eq!(plan.luts.len(), 3);
        // Budget accounting: 3 bootstraps -> total = 30 + 2 = 32.
        assert_eq!(plan.budget.total_log2, 32);
        // Over-tight bound fails.
        let err = build_bootstrap_plan(&c, 2, (30, 31), ProfileId::Toy).unwrap_err();
        assert!(matches!(
            err,
            PlanBuildError::BudgetExceeded {
                required_total_log2: 32
            }
        ));
    }

    #[test]
    fn constants_fold_and_not_fuses() {
        // out = NOT(a AND 1) XOR 0 → NAND(a) i.e. [true, false] 1-input LUT.
        let c = BIrBlocks {
            blocks: vec![BIrBlock {
                params: 1,
                stmts: vec![
                    Node::new(BIrStmt::One, (), None),
                    Node::new(BIrStmt::Zero, (), None),
                    Node::new(BIrStmt::And(IRVarId(0), IRVarId(1)), (), None),
                    Node::new(BIrStmt::Not(IRVarId(3)), (), None),
                    Node::new(BIrStmt::Xor(IRVarId(4), IRVarId(2)), (), None),
                ],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(5)],
                }),
            }],
            pre_init: vec![],
        };
        let plan = build_bootstrap_plan(&c, 4, (30, 34), ProfileId::Toy).unwrap();
        plan.validate().unwrap();
        assert_eq!(plan.luts.len(), 1, "plan: {plan:?}");
        assert_eq!(plan.luts[0].entries, vec![true, false]);
        let ops: Vec<_> = plan.layers.iter().flatten().collect();
        assert_eq!(ops.len(), 1, "only one LUT op expected: {plan:?}");
    }

    #[test]
    fn unsupported_stmt_is_an_error() {
        let c = build_and_circuit();
        let mut c = c.clone();
        c.blocks[0].stmts.push(Node::new(
            BIrStmt::OracleBit {
                name: "x".into(),
                args: vec![IRVarId(0)],
                bit: 0,
                occurrence: 0,
            },
            (),
            None,
        ));
        assert!(matches!(
            build_bootstrap_plan(&c, 4, (30, 34), ProfileId::Toy),
            Err(PlanBuildError::UnsupportedStmt("oracle"))
        ));
    }

    // ── Codegen + compile-and-run ─────────────────────────────────────────

    /// Prepend binfhe imports and cargo-check the generated module (same
    /// harness as the legacy TFHE compile checks).
    fn compile_check_binfhe(code: &str, test_name: &str) {
        let uses = "use volar_spec::binfhe::lwe::{LweCiphertext, binfhe_trivial, binfhe_not, wire_delta};\n\
                    use volar_spec::binfhe::rlwe::RlweCiphertext;\n\
                    use volar_spec::binfhe::rgsw::cmux;\n\
                    use volar_spec::binfhe::keys::BootstrappingKey;\n\
                    use volar_spec::binfhe::circuit_bs::{CircuitBootstrappingKey, circuit_bootstrap};\n\
                    use volar_spec::binfhe::pbs::{binfhe_gate_and, binfhe_gate_or, binfhe_gate_xor, binfhe_cmux, binfhe_lut_read_dyn};\n";
        let with_imports = if let Some(newline) = code.find('\n') {
            let (head, tail) = code.split_at(newline + 1);
            alloc::format!("{head}{uses}{tail}")
        } else {
            alloc::format!("{uses}{code}")
        };
        crate::tests_common::run_compile_check(&with_imports, test_name);
    }

    /// The weaver's emitted IR must itself be Vec-free (AGENTS.md Core
    /// Design Rule 11): the weaver emits presized calls, never a `Vec`.
    fn assert_module_vec_free(plan: &BootstrapPlan, name: &str) {
        let module = weave_binfhe_plan(plan, name);
        let errors = volar_compiler_passes::vec_lint::lint_module(module.inner(), &|_| false);
        assert!(
            errors.is_empty(),
            "weaver-emitted IR must be Vec-free: {:?}",
            errors
        );
    }

    #[test]
    fn fused_and_circuit_compiles_and_is_vec_free() {
        let plan = build_bootstrap_plan(&build_and_circuit(), 4, (30, 34), ProfileId::Toy).unwrap();
        assert_module_vec_free(&plan, "and_fused");
        let module = weave_binfhe_plan(&plan, "and_fused");
        let code = crate::fhe::print_fhe_flat_module(module.inner(), true);
        compile_check_binfhe(&code, "binfhe_fused_and");
    }

    #[test]
    fn fused_xor_and_plan_is_vec_free() {
        let plan = build_bootstrap_plan(&xor_and_or_circuit(), 4, (30, 34), ProfileId::Toy).unwrap();
        assert_module_vec_free(&plan, "xor_and_or");
    }

    #[test]
    fn fused_xor_and_circuit_compiles() {
        let plan = build_bootstrap_plan(&build_xor_and_circuit(), 4, (30, 34), ProfileId::Toy).unwrap();
        let module = weave_binfhe_plan(&plan, "xor_and_fused");
        let code = crate::fhe::print_fhe_flat_module(module.inner(), true);
        compile_check_binfhe(&code, "binfhe_fused_xor_and");
    }

    #[test]
    fn binfhe_scheme_flat_compiles() {
        let circuit = crate::tests_common::build_xor_and_circuit();
        let scheme = BinFheScheme::new();
        let output = crate::fhe::weave_fhe_flat_bir(
            &circuit,
            &scheme,
            "xor_and_scheme",
            &crate::NoProvenance,
            None,
        );
        let code = crate::fhe::print_fhe_flat_module(output.inner(), true);
        compile_check_binfhe(&code, "binfhe_scheme_xor_and");
    }

    // ── End-to-end: generated code runs and agrees with execute_plan ────

    /// Render a [`BootstrapPlan`] as a Rust expression (for the e2e
    /// fixture project, where both the generated function and
    /// `execute_plan` consume the same plan value).
    fn plan_to_rust(plan: &BootstrapPlan) -> std::string::String {
        use std::fmt::Write as _;
        let mut s = std::string::String::new();
        write!(
            s,
            "BootstrapPlan {{ profile: ProfileId::Toy, k_max: {}, luts: vec![",
            plan.k_max
        )
        .unwrap();
        for spec in &plan.luts {
            write!(s, "LutSpec {{ entries: vec![").unwrap();
            for e in &spec.entries {
                write!(s, "{},", e).unwrap();
            }
            write!(s, "] }},").unwrap();
        }
        write!(s, "], layers: vec![").unwrap();
        for layer in &plan.layers {
            write!(s, "vec![").unwrap();
            for op in layer {
                match op {
                    PlanOp::Const { out, value } => {
                        write!(s, "PlanOp::Const {{ out: {}, value: {} }},", out, value).unwrap();
                    }
                    PlanOp::Not { input, out } => {
                        write!(s, "PlanOp::Not {{ input: {}, out: {} }},", input, out).unwrap();
                    }
                    PlanOp::Lut { inputs, table, out } => {
                        write!(s, "PlanOp::Lut {{ inputs: volar_spec::binfhe::plan::LutInputs::from_slice(&[").unwrap();
                        for w in inputs.as_slice() {
                            write!(s, "{},", w).unwrap();
                        }
                        write!(s, "]), table: {}, out: {} }},", table, out).unwrap();
                    }
                    PlanOp::CircuitBootstrap { input, out } => {
                        write!(
                            s,
                            "PlanOp::CircuitBootstrap {{ input: {}, out: {} }},",
                            input, out
                        )
                        .unwrap();
                    }
                    PlanOp::RgswMux { sel, then_cell, else_cell, out } => {
                        write!(
                            s,
                            "PlanOp::RgswMux {{ sel: {}, then_cell: {}, else_cell: {}, out: {} }},",
                            sel, then_cell, else_cell, out
                        )
                        .unwrap();
                    }
                }
            }
            write!(s, "],").unwrap();
        }
        write!(
            s,
            "], num_inputs: {}, num_cells: {}, outputs: vec![",
            plan.num_inputs, plan.num_cells
        )
        .unwrap();
        for w in &plan.outputs {
            write!(s, "{},", w).unwrap();
        }
        write!(s, "], cell_outputs: vec![").unwrap();
        for c in &plan.cell_outputs {
            write!(s, "{},", c).unwrap();
        }
        write!(
            s,
            "], budget: FailureBudget {{ per_bootstrap_log2: {}, total_log2: {} }} }}",
            plan.budget.per_bootstrap_log2, plan.budget.total_log2
        )
        .unwrap();
        s
    }

    /// Write a fixture project whose `lib.rs` contains the generated
    /// function plus a `#[cfg(test)]` module that runs it against
    /// `execute_plan` on the toy profile, and run `cargo test`.
    /// `fn_stem` is the generated function name without the `_binfhe`
    /// suffix; `expected_expr` is the plaintext oracle expression over
    /// `a`/`b`.
    fn run_binfhe_e2e(code: &str, plan: &BootstrapPlan, fn_stem: &str, expected_expr: &str) {
        use std::process::Command;
        let test_name = alloc::format!("binfhe_e2e_{}", fn_stem);
        let root = crate::tests_common::workspace_root();
        let tmpdir = std::env::temp_dir().join(alloc::format!("volar_weaver_{}", test_name));
        let srcdir = tmpdir.join("src");
        std::fs::create_dir_all(&srcdir).unwrap();
        let cargo_toml = std::format!(
            "[package]\n\
             name = \"weave-e2e-{name}\"\n\
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
             elliptic-curve = {{ version = \"0.13.8\", features = [\"arithmetic\"], default-features = false }}\n",
            name = test_name,
            root = root,
        );
        std::fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).unwrap();

        let plan_src = plan_to_rust(plan);
        let harness = std::format!(
            r#"
#[cfg(test)]
mod e2e {{
    use super::*;
    use volar_spec::binfhe::plan::{{
        BootstrapPlan, FailureBudget, LutSpec, PlanOp, ProfileId, execute_plan,
    }};
    use volar_spec::binfhe::circuit_bs::gen_circuit_bootstrapping_key;
    use volar_spec::binfhe::keys::gen_bootstrapping_key;
    use volar_spec::binfhe::lwe::{{
        gen_lwe_secret_key, lwe_decrypt, lwe_encrypt, wire_delta,
    }};
    use volar_spec::binfhe::rlwe::gen_rlwe_secret_key;
    use volar_spec::SpecRng;

    const N_LWE: usize = 8;
    const BIG_N: usize = 64;
    const LOG_Q: u32 = 7;
    const LOG_Q_LWE: u32 = 7;
    const LOG_MOD_KS: u32 = 7;
    const BS_ELL: usize = 2;
    const BS_BASE_LOG: u32 = 4;
    const KS_ELL: usize = 2;
    const KS_BASE_LOG: u32 = 4;
    const PRIV_ELL: usize = 2;
    const PRIV_BASE_LOG: u32 = 4;
    const K_MAX: usize = {k_max};

    struct TestRng(u64);
    impl TestRng {{
        fn new(seed: u64) -> Self {{ Self(seed) }}
    }}
    impl SpecRng for TestRng {{
        fn next_u32(&mut self) -> u32 {{
            self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
            z = z ^ (z >> 31);
            z as u32
        }}
    }}

    #[test]
    fn generated_matches_execute_plan() {{
        let plan: BootstrapPlan = {plan_src};
        plan.validate().unwrap();
        let mut rng = TestRng::new(0xE2E0);
        let lwe_sk = gen_lwe_secret_key::<N_LWE, _>(&mut rng);
        let rlwe_sk = gen_rlwe_secret_key::<BIG_N, _>(&mut rng);
        let bk = gen_bootstrapping_key::<
            N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
            BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, 0, _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        let cbk = gen_circuit_bootstrapping_key::<
            N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
            BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG,
            PRIV_ELL, PRIV_BASE_LOG, 0, _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        let delta = wire_delta::<LOG_Q_LWE>(K_MAX);
        for a in [false, true] {{
            for b in [false, true] {{
                let mut ra = TestRng::new(1 + a as u64);
                let mut rb = TestRng::new(3 + b as u64);
                let ca = lwe_encrypt::<N_LWE, LOG_Q_LWE, 0, _>(a, delta, &lwe_sk, &mut ra);
                let cb = lwe_encrypt::<N_LWE, LOG_Q_LWE, 0, _>(b, delta, &lwe_sk, &mut rb);
                let generated = {fn_name}::<
                    N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
                    BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, PRIV_ELL, PRIV_BASE_LOG, K_MAX,
                >(&bk, ca, cb);
                let (wires, _cells) = execute_plan::<
                    N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
                    BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, PRIV_ELL, PRIV_BASE_LOG,
                >(&plan, &[ca, cb], &[], &bk, &cbk);
                let via_plan = wires[plan.outputs[0] as usize];
                assert_eq!(
                    generated, via_plan,
                    "generated fn must equal execute_plan bit-for-bit (a={{}}, b={{}})",
                    a, b
                );
                let expected = {expected_expr};
                assert_eq!(
                    lwe_decrypt::<N_LWE, LOG_Q_LWE>(&generated, &lwe_sk, delta),
                    expected,
                    "plaintext semantics (a={{}}, b={{}})",
                    a, b
                );
            }}
        }}
    }}
}}
"#,
            k_max = plan.k_max,
            plan_src = plan_src,
            fn_name = alloc::format!("{}_binfhe", fn_stem),
            expected_expr = expected_expr,
        );
        std::fs::write(srcdir.join("lib.rs"), alloc::format!("{}{}", code, harness)).unwrap();

        let output = Command::new("cargo")
            .args(["test", "--quiet"])
            .current_dir(&tmpdir)
            .env(
                "CARGO_TARGET_DIR",
                std::string::String::from(tmpdir.join("target").to_str().unwrap()),
            )
            .output()
            .expect("failed to run cargo test");
        let stderr = std::string::String::from_utf8_lossy(&output.stderr).into_owned();
        let stdout = std::string::String::from_utf8_lossy(&output.stdout).into_owned();
        if std::env::var("BINFHE_KEEP_TMP").is_err() { let _ = std::fs::remove_dir_all(&tmpdir); }
        if !output.status.success() {
            panic!(
                "binfhe e2e failed (test: {})\n--- stdout ---\n{}\n--- stderr ---\n{}",
                test_name, stdout, stderr
            );
        }
    }

    #[test]
    fn fused_xor_and_runs_and_matches_execute_plan() {
        // (a XOR b) AND (a OR b) fused to one LUT = XOR table.
        let plan = build_bootstrap_plan(&xor_and_or_circuit(), 4, (30, 34), ProfileId::Toy).unwrap();
        let module = weave_binfhe_plan(&plan, "xor_and_or");
        let code = crate::fhe::print_fhe_flat_module(module.inner(), true);
        // The generated function name must match the e2e harness call.
        assert!(code.contains("fn xor_and_or_binfhe"), "code:\n{}", code);
        // Insert imports after the leading `#![allow]` inner attribute.
        let uses = "use volar_spec::binfhe::lwe::{LweCiphertext, binfhe_trivial, binfhe_not, wire_delta};\n\
                    use volar_spec::binfhe::keys::BootstrappingKey;\n\
                    use volar_spec::binfhe::pbs::binfhe_lut_read_dyn;\n";
        let code = if let Some(newline) = code.find('\n') {
            let (head, tail) = code.split_at(newline + 1);
            alloc::format!("{head}{uses}{tail}")
        } else {
            alloc::format!("{uses}{code}")
        };
        run_binfhe_e2e(&code, &plan, "xor_and_or", "(a && b) ^ (a || b)");
    }
}
