//! Demand-driven VAFFLE-to-IR entry point built on `portal-lazy-transform`.
//!
//! `describe` performs a purely structural scan of a function's direct call
//! targets (`Value::Call`/`Terminator::ReturnCall`) to discover the reachable
//! closure from a set of requested roots -- no lowering happens during
//! discovery.
//!
//! Resolution, however, is **not** routed through `portal_lazy_transform`'s
//! generic `ResultRef`/`DemandExecutor` driver. `LowerCtx`'s block-index
//! bookkeeping (`plan_functions`'s precomputed `entry_block` offsets, and the
//! call-splitting `cont_block_idx` arithmetic in `lower_function`) both
//! depend on every function being processed in strict ascending `FuncId`
//! order -- a hard structural requirement of the flat, single `IRBlocks`
//! array this backend emits into, completely independent of the call graph.
//! A caller's lowering never reads a callee's already-lowered *blocks* (only
//! its precomputed *layout*, available for every function up front via
//! `plan_functions`), so dependency order carries no correctness weight here
//! -- only *ascending module order* does. The discovered closure is therefore
//! used only to decide, per function in that fixed order, whether to pay for
//! a real `lower_function` call or a cheap `reserve_placeholder_blocks` call;
//! it is not used to reorder resolution.
//!
//! This is a deliberate, documented deviation from the generic driver used by
//! the dreamcomp/cps-lir integrations -- see the cross-repo lazy-transform
//! plan's "Key open risks" section.

use core::convert::Infallible;

use alloc::collections::BTreeSet;
use alloc::vec::Vec;

use vaffle::{FuncDecl, FuncId, Module, Terminator, Value};
use volar_ir::ir::{IRBlocks, IRTypes};
use portal_lazy_transform::{
    AssemblyLimits, Demand, Fragment, NoopObserver, PlanSource, ResolvedInputs, SubElementCounter,
    SubElementId, SubElementObserver,
};

use crate::lower_to_ir::LowerCtx;

/// The single facet this plan resolves: lower one VAFFLE function to IR.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Emit;

/// One fine-grained tracepoint fired per emitted `IRStmt` while resolving a
/// `FuncId`'s `Emit` facet. Project-owned event payload for
/// `SubElementObserver` -- the shared crate never defines this shape.
///
/// Carries no statement content (only that one was emitted) because wiring
/// real per-statement payloads through `BlockEmitter::emit`'s call sites
/// (scattered across `lower_function`) would be a much larger, riskier
/// change to this bit-precise circuit backend; this still gives a real
/// tracepoint per emitted SSA-level IR statement, fired from the outer
/// driving loop by diffing block/statement counts before and after each
/// `lower_function` call.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VaffleIrTraceEvent;

/// Options for [`lower_vaffle_to_ir_planned`]/[`lower_vaffle_to_ir_planned_with_trace`].
#[derive(Clone, Copy, Debug)]
pub struct LowerPlanOptions {
    pub limits: AssemblyLimits,
}

impl Default for LowerPlanOptions {
    fn default() -> Self {
        Self {
            limits: AssemblyLimits::default(),
        }
    }
}

/// Direct callees of `func_decl` from `Value::Call` and `Terminator::ReturnCall`
/// sites -- a purely structural scan (no lowering). Non-`Body` (import)
/// declarations have no callees. Scanning every value/block in the function
/// (rather than only ones reachable through its own internal control flow)
/// is a deliberately conservative over-approximation, matching the RFC's
/// conservative-indirect-dependency rule: over-declaring a dependency is
/// safe, silently omitting one is not.
fn direct_callees<P: Clone>(func_decl: &FuncDecl<P>) -> Vec<FuncId> {
    let body = match func_decl {
        FuncDecl::Body(b) => b,
        _ => return Vec::new(),
    };
    let mut callees = BTreeSet::new();
    for node in body.values.iter() {
        if let Value::Call { func, .. } = &node.kind {
            callees.insert(*func);
        }
    }
    for block in &body.blocks {
        if let Terminator::ReturnCall { func, .. } = &block.terminator {
            callees.insert(*func);
        }
    }
    callees.into_iter().collect()
}

/// Stateless `PlanSource` functor used only for discovery (`describe`).
/// `resolve` is never invoked -- see the module doc comment for why
/// resolution is driven by an explicit ascending-`FuncId` loop instead of
/// `portal_lazy_transform::ResultRef`.
#[derive(Default)]
struct VaffleFuncSource;

impl<P: Clone> PlanSource<Module<P>> for VaffleFuncSource {
    type Node = FuncId;
    type Artifact = FuncId;
    type Facet = Emit;
    type Metadata = ();
    type Error = Infallible;

    fn root_for(
        &self,
        _context: &Module<P>,
        demand: &Demand<FuncId, Emit>,
    ) -> Result<FuncId, Infallible> {
        Ok(demand.artifact)
    }

    fn describe(
        &mut self,
        context: &Module<P>,
        node: &FuncId,
        _demand: &Demand<FuncId, Emit>,
    ) -> Result<Fragment<FuncId, FuncId, Emit, ()>, Infallible> {
        let callees = direct_callees(&context.funcs[node.0]);
        Ok(Fragment::new(
            *node,
            (),
            callees.into_iter().map(|c| Demand::new(c, Emit)).collect(),
        ))
    }

    fn resolve(
        &mut self,
        _context: &mut Module<P>,
        _node: &FuncId,
        _facet: &Emit,
        _inputs: &mut dyn ResolvedInputs<FuncId, Emit>,
    ) -> Result<(), Infallible> {
        Ok(())
    }
}

/// Lower exactly the conservative reachable closure of `roots` (and nothing
/// else) to IR, preserving the eager path's exact block-index layout for
/// every resolved function. New lazy entry point beside
/// [`crate::lower_vaffle_to_ir`]; does not change it.
///
/// Unresolved (unreached) functions leave placeholder/trap blocks in their
/// reserved index range rather than being compacted out -- this keeps every
/// *resolved* function's block indices byte-identical to what
/// `lower_vaffle_to_ir` would have produced for the same module.
pub fn lower_vaffle_to_ir_planned<P: Clone>(
    module: &Module<P>,
    roots: Vec<FuncId>,
    options: &LowerPlanOptions,
) -> (IRBlocks<P>, IRTypes) {
    lower_vaffle_to_ir_planned_with_trace(module, roots, options, None)
}

/// Like [`lower_vaffle_to_ir_planned`] but with an optional sub-element
/// tracepoint sink -- fired once per `IRStmt` emitted while resolving each
/// requested function's `Emit` facet.
pub fn lower_vaffle_to_ir_planned_with_trace<P: Clone>(
    module: &Module<P>,
    roots: Vec<FuncId>,
    options: &LowerPlanOptions,
    mut sub_observer: Option<&mut dyn SubElementObserver<FuncId, Emit, VaffleIrTraceEvent>>,
) -> (IRBlocks<P>, IRTypes) {
    let ssa_module = crate::vaffle_ssa::ssa_ify_module(module);

    // Discovery only -- decides which functions are real-lowered vs.
    // placeholder-reserved below; does NOT drive resolution order.
    let mut source = VaffleFuncSource;
    let demands: Vec<_> = roots.into_iter().map(|r| Demand::new(r, Emit)).collect();
    let plan = portal_lazy_transform::assemble_bfs(
        &mut source,
        &ssa_module,
        demands,
        options.limits,
        &mut NoopObserver,
    )
    .expect("VAFFLE call-graph discovery cannot fail: describe() is Infallible and never errors");
    let requested: BTreeSet<usize> = plan.nodes.iter().map(|n| n.node.0).collect();

    let mut ctx = LowerCtx::new(&ssa_module);
    ctx.plan_functions();
    ctx.emit_entry_and_exit();

    let mut counter = SubElementCounter::new();
    for (func_idx, func_decl) in ssa_module.funcs.iter().enumerate() {
        let body = match func_decl {
            FuncDecl::Body(b) => b,
            _ => continue,
        };
        if requested.contains(&func_idx) {
            let blocks_before = ctx.blocks_len();
            let extra_before = ctx.extra_blocks_len();
            ctx.lower_function(func_idx, body);
            if let Some(obs) = sub_observer.as_deref_mut() {
                let emitted = ctx.stmt_count_since(blocks_before, extra_before);
                for _ in 0..emitted {
                    let id: SubElementId = counter.next();
                    obs.sub_element(&FuncId(func_idx), &Emit, id, &VaffleIrTraceEvent);
                }
            }
        } else {
            ctx.reserve_placeholder_blocks(body.blocks.len());
        }
    }

    ctx.append_extra_blocks();
    ctx.finish()
}

#[cfg(test)]
mod tests {
    use super::*;
    use vaffle::{Block, BlockId, SigDecl, SigId, ValueId};
    use volar_ir_common::{IrType, Node as IrNode, Type, TypeTable};

    /// `root` calls `reachable`; `unrelated` shares no edge with either.
    fn three_function_module() -> (Module<()>, FuncId, FuncId, FuncId) {
        let mut types = TypeTable::new();
        let bit_tid = types.intern(IrType::Primitive(Type::Bit));
        let sig = SigDecl {
            params: alloc::vec![],
            results: alloc::vec![bit_tid],
        };

        let leaf_body = |ret: ValueId| vaffle::FuncBody {
            sig: SigId(0),
            blocks: alloc::vec![Block {
                params: alloc::vec![],
                stmts: alloc::vec![ret],
                terminator: Terminator::Return {
                    values: alloc::vec![ret],
                },
            }],
            values: alloc::vec![IrNode::new(
                Value::Op(volar_ir_common::Stmt::Const(
                    volar_ir_common::Constant { hi: 0, lo: 0 },
                    bit_tid,
                )),
                (),
                None,
            )],
            entry: BlockId(0),
        };

        let reachable_body = leaf_body(ValueId(0));
        let unrelated_body = leaf_body(ValueId(0));

        // root: the module's entry function (VAFFLE requires FuncId(0) to be
        // the entry, and forbids anything from calling into it), calls
        // `reachable` (FuncId(1)), returns its result.
        let root_body = vaffle::FuncBody {
            sig: SigId(0),
            blocks: alloc::vec![Block {
                params: alloc::vec![],
                stmts: alloc::vec![ValueId(0)],
                terminator: Terminator::Return {
                    values: alloc::vec![ValueId(0)],
                },
            }],
            values: alloc::vec![IrNode::new(
                Value::Call {
                    func: FuncId(1),
                    args: alloc::vec![],
                },
                (),
                None,
            )],
            entry: BlockId(0),
        };

        let module = Module {
            types,
            oracles: alloc::vec![],
            actions: alloc::vec![],
            funcs: alloc::vec![
                FuncDecl::Body(root_body),      // FuncId(0), module entry
                FuncDecl::Body(reachable_body), // FuncId(1)
                FuncDecl::Body(unrelated_body), // FuncId(2)
            ],
            sigs: alloc::vec![sig],
            exports: Default::default(),
            pre_init: alloc::vec![],
        };
        (module, FuncId(0), FuncId(1), FuncId(2))
    }

    #[test]
    fn direct_callees_finds_only_statically_known_call_targets() {
        let (module, root_id, reachable_id, unrelated_id) = three_function_module();
        let callees = direct_callees(&module.funcs[root_id.0]);
        assert_eq!(callees, alloc::vec![reachable_id]);
        assert!(direct_callees(&module.funcs[reachable_id.0]).is_empty());
        assert!(!callees.contains(&unrelated_id));
    }

    #[test]
    fn assemble_bfs_closure_excludes_unrelated_functions() {
        let (module, root_id, reachable_id, unrelated_id) = three_function_module();
        let mut source = VaffleFuncSource;
        let plan = portal_lazy_transform::assemble_bfs(
            &mut source,
            &module,
            alloc::vec![Demand::new(root_id, Emit)],
            AssemblyLimits::default(),
            &mut NoopObserver,
        )
        .unwrap();

        let discovered: BTreeSet<FuncId> = plan.nodes.iter().map(|n| n.node).collect();
        assert!(discovered.contains(&root_id));
        assert!(discovered.contains(&reachable_id));
        assert!(
            !discovered.contains(&unrelated_id),
            "unrelated function must never be discovered by the plan"
        );
    }

    #[test]
    fn planned_lowering_matches_eager_indices_for_resolved_functions_and_skips_unrelated_body() {
        let (module, root_id, _reachable_id, _unrelated_id) = three_function_module();

        let (eager_blocks, _eager_types) = crate::lower_vaffle_to_ir(&module);
        let (planned_blocks, _planned_types) = lower_vaffle_to_ir_planned(
            &module,
            alloc::vec![root_id],
            &LowerPlanOptions::default(),
        );

        // Total block count (and therefore every resolved function's index
        // range) is identical -- placeholder blocks preserve the layout
        // rather than compacting it out.
        assert_eq!(eager_blocks.blocks.len(), planned_blocks.blocks.len());

        // Layout: block 0/1 = module entry/exit, block 2 = root (FuncId 0),
        // block 3 = reachable (FuncId 1), block 4 = unrelated (FuncId 2) --
        // each function here has exactly one VAFFLE block (see
        // `three_function_module`).
        let unrelated_block_idx = 4;
        for (i, (eager, planned)) in eager_blocks
            .blocks
            .iter()
            .zip(planned_blocks.blocks.iter())
            .enumerate()
        {
            if i == unrelated_block_idx {
                // Skipped: eager has real (bit-packing-expanded) content;
                // planned has an empty placeholder -- this is the entire
                // point of the opt-in laziness, not a regression.
                assert!(
                    !eager.stmts.is_empty(),
                    "expected the eager path to have really lowered the unrelated function"
                );
                assert_eq!(planned.stmts.len(), 0, "expected an empty placeholder block");
                continue;
            }
            assert_eq!(
                eager.stmts.len(),
                planned.stmts.len(),
                "block {i} stmt count diverged between eager and planned lowering for a \
                 resolved function -- laziness must be byte-identical for anything requested"
            );
        }
    }

    #[test]
    fn planned_lowering_counts_sub_element_tracepoints_only_for_resolved_functions() {
        struct CountingObserver {
            events: alloc::vec::Vec<FuncId>,
        }
        impl SubElementObserver<FuncId, Emit, VaffleIrTraceEvent> for CountingObserver {
            fn sub_element(
                &mut self,
                node: &FuncId,
                _facet: &Emit,
                _id: SubElementId,
                _event: &VaffleIrTraceEvent,
            ) {
                self.events.push(*node);
            }
        }

        let (module, root_id, reachable_id, unrelated_id) = three_function_module();
        let mut observer = CountingObserver {
            events: alloc::vec::Vec::new(),
        };
        let _ = lower_vaffle_to_ir_planned_with_trace(
            &module,
            alloc::vec![root_id],
            &LowerPlanOptions::default(),
            Some(&mut observer),
        );

        assert!(
            !observer.events.is_empty(),
            "expected at least one sub-element tracepoint for resolved functions"
        );
        assert!(observer.events.iter().all(|f| *f == root_id || *f == reachable_id));
        assert!(!observer.events.iter().any(|f| *f == unrelated_id));
    }
}
