// @reliability: experimental
// @ai: assisted
//! Canonicalisation of IR / BIR blocks into a deduplication key.
//!
//! A handler key (either [`IrHandlerKey`] or [`BirHandlerKey`]) uniquely
//! identifies a handler equivalence class: two blocks with the same key
//! are guaranteed to be executable by a single handler body, given
//! appropriate immediate parameters.
//!
//! The key is produced by walking the block's statements and terminator
//! and replacing every *immediate* field (values that vary between
//! otherwise-identical blocks) with a placeholder.  The lifted values are
//! returned alongside the key in a [`BlockImmediates`] struct.

use alloc::{collections::BTreeMap, vec::Vec};

use volar_ir::{
    boolar::{BIrBlock, BIrStmt, BIrTarget, BIrTerminator},
    ir::{IRBlock, IRBlockId, IRBlockTargetId, IRStmt, IRTerminator, IRTypeId, IRVarId},
};
use volar_ir_common::Constant;

/// What kind of immediate a handler parameter feeds.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum ImmediateKind {
    /// A [`Constant`] value lifted out of `Stmt::Const` or the constant
    /// term of `Stmt::Poly`.
    Constant,
    /// An [`IRBlockId`] lifted out of a terminator target.
    BlockTarget,
}

/// Values lifted out of a single block during canonicalisation.
///
/// `consts` and `targets` are parallel to the placeholder slots in the
/// handler key — the k-th lifted constant in the key is fed from
/// `consts[k]`, and similarly for targets.
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct BlockImmediates {
    /// Lifted constants (from `Stmt::Const.value` and `Stmt::Poly.constant`).
    pub consts: Vec<Constant>,
    /// Lifted terminator block targets, in traversal order.
    pub targets: Vec<IRBlockId>,
}

/// Trait shared between IR and BIR handler keys.
pub trait HandlerKey: Ord + Clone {
    /// The schema of the immediate parameters this handler expects.
    fn immediate_schema(&self) -> Vec<ImmediateKind>;
}

/// Extended schema entry describing both the kind and the IR type of an
/// immediate slot.  Only produced by IR handler keys (BIR block target
/// addresses are `_32`-typed by convention).
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IrImmediateSlot {
    pub kind: ImmediateKind,
    /// IR type id of the slot value.  For `BlockTarget` this is the
    /// caller-provided address type (typically `_32`).
    pub ty: IRTypeId,
}

// ============================================================================
// IR handler key
// ============================================================================

/// Placeholder constant used in canonicalised `Stmt::Const` and
/// `Stmt::Poly.constant` slots.  Deliberately a public sentinel so that
/// callers reading canonicalised stmts know not to trust the value.
pub const ZERO_CONSTANT: Constant = Constant { hi: 0, lo: 0 };

/// Placeholder block id used in canonicalised terminator targets.
pub const ZERO_BLOCK_ID: IRBlockId = IRBlockId(0);

/// Handler key for a Volar IR block.
///
/// The key is a tuple of:
///   * the parameter type sequence,
///   * the block's `stmts` with every immediate field replaced by a
///     placeholder,
///   * the terminator with every concrete [`IRBlockId`] replaced by a
///     placeholder.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IrHandlerKey {
    pub params: Vec<IRTypeId>,
    pub stmts: Vec<IRStmt>,
    pub terminator: IRTerminator,
}

impl HandlerKey for IrHandlerKey {
    fn immediate_schema(&self) -> Vec<ImmediateKind> {
        let mut out = Vec::new();
        for s in &self.stmts {
            if let IRStmt::Const(_, _) = s {
                out.push(ImmediateKind::Constant);
            }
        }
        append_ir_terminator_schema(&self.terminator, &mut out);
        out
    }
}

impl IrHandlerKey {
    /// Typed immediate schema (constants carry their IR type, targets carry
    /// `addr_ty`).
    pub fn typed_immediate_schema(&self, addr_ty: IRTypeId) -> Vec<IrImmediateSlot> {
        let mut out = Vec::new();
        for s in &self.stmts {
            if let IRStmt::Const(_, ty) = s {
                out.push(IrImmediateSlot {
                    kind: ImmediateKind::Constant,
                    ty: *ty,
                });
            }
        }
        let mut tslots = Vec::new();
        append_ir_terminator_schema(&self.terminator, &mut tslots);
        for k in tslots {
            out.push(IrImmediateSlot { kind: k, ty: addr_ty });
        }
        out
    }
}

fn append_ir_terminator_schema(term: &IRTerminator, out: &mut Vec<ImmediateKind>) {
    match term {
        IRTerminator::Jmp { func, .. } => {
            if let IRBlockTargetId::Block(_) = func {
                out.push(ImmediateKind::BlockTarget);
            }
        }
        IRTerminator::JumpCond { true_block, false_block, .. } => {
            if let IRBlockTargetId::Block(_) = true_block {
                out.push(ImmediateKind::BlockTarget);
            }
            if let IRBlockTargetId::Block(_) = false_block {
                out.push(ImmediateKind::BlockTarget);
            }
        }
        IRTerminator::JumpTable { cases, .. } => {
            for (_, (target, _)) in cases {
                if let IRBlockTargetId::Block(_) = target {
                    out.push(ImmediateKind::BlockTarget);
                }
            }
        }
    }
}

/// Canonicalise an [`IRBlock`] and return its handler key plus the lifted
/// immediates.
pub fn canonicalize_ir_block<P: Clone + Default>(
    block: &IRBlock<P>,
) -> (IrHandlerKey, BlockImmediates) {
    let mut consts = Vec::new();
    let mut targets = Vec::new();

    let mut canon_stmts: Vec<IRStmt> = Vec::with_capacity(block.stmts.len());
    for s in &block.stmts {
        canon_stmts.push(canon_ir_stmt(s, &mut consts));
    }

    let canon_term = canon_ir_terminator(&block.terminator, &mut targets);

    let key = IrHandlerKey {
        params: block.params.clone(),
        stmts: canon_stmts,
        terminator: canon_term,
    };

    (key, BlockImmediates { consts, targets })
}

fn canon_ir_stmt(s: &IRStmt, consts: &mut Vec<Constant>) -> IRStmt {
    match s {
        IRStmt::Const(c, ty) => {
            consts.push(*c);
            IRStmt::Const(ZERO_CONSTANT, *ty)
        }
        other => other.clone(),
    }
}

fn canon_ir_target(t: &IRBlockTargetId, targets: &mut Vec<IRBlockId>) -> IRBlockTargetId {
    match t {
        IRBlockTargetId::Block(id) => {
            targets.push(*id);
            IRBlockTargetId::Block(ZERO_BLOCK_ID)
        }
        IRBlockTargetId::Return => IRBlockTargetId::Return,
        IRBlockTargetId::Dyn(v) => IRBlockTargetId::Dyn(*v),
    }
}

fn canon_ir_terminator(t: &IRTerminator, targets: &mut Vec<IRBlockId>) -> IRTerminator {
    match t {
        IRTerminator::Jmp { func, args } => IRTerminator::Jmp {
            func: canon_ir_target(func, targets),
            args: args.clone(),
        },
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => IRTerminator::JumpCond {
            condition: *condition,
            true_block: canon_ir_target(true_block, targets),
            true_args: true_args.clone(),
            false_block: canon_ir_target(false_block, targets),
            false_args: false_args.clone(),
        },
        IRTerminator::JumpTable { index, cases } => {
            let mut canon_cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)> =
                BTreeMap::new();
            for (k, (target, args)) in cases {
                canon_cases.insert(*k, (canon_ir_target(target, targets), args.clone()));
            }
            IRTerminator::JumpTable {
                index: *index,
                cases: canon_cases,
            }
        }
    }
}

// ============================================================================
// BIR handler key
// ============================================================================

/// Handler key for a Boolar IR block.
///
/// BIR has no `Stmt::Const` variant (boolean constants are the structural
/// `BIrStmt::Zero`/`BIrStmt::One` gates) so only terminator targets are
/// lifted in v1.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BirHandlerKey {
    pub params: u32,
    pub stmts: Vec<BIrStmt>,
    pub terminator: BIrTerminator,
}

impl HandlerKey for BirHandlerKey {
    fn immediate_schema(&self) -> Vec<ImmediateKind> {
        let mut out = Vec::new();
        append_bir_terminator_schema(&self.terminator, &mut out);
        out
    }
}

fn append_bir_terminator_schema(term: &BIrTerminator, out: &mut Vec<ImmediateKind>) {
    match term {
        BIrTerminator::Jmp(t) => {
            if let IRBlockTargetId::Block(_) = t.block {
                out.push(ImmediateKind::BlockTarget);
            }
        }
        BIrTerminator::CondJmp { then_target, else_target, .. } => {
            if let IRBlockTargetId::Block(_) = then_target.block {
                out.push(ImmediateKind::BlockTarget);
            }
            if let IRBlockTargetId::Block(_) = else_target.block {
                out.push(ImmediateKind::BlockTarget);
            }
        }
    }
}

/// Canonicalise a [`BIrBlock`] and return its handler key plus the lifted
/// immediates.
pub fn canonicalize_bir_block<P: Clone + Default>(
    block: &BIrBlock<P>,
) -> (BirHandlerKey, BlockImmediates) {
    let mut targets = Vec::new();

    let canon_term = canon_bir_terminator(&block.terminator, &mut targets);

    let key = BirHandlerKey {
        params: block.params,
        stmts: block.stmts.clone(),
        terminator: canon_term,
    };

    (
        key,
        BlockImmediates {
            consts: Vec::new(),
            targets,
        },
    )
}

fn canon_bir_target(t: &BIrTarget, targets: &mut Vec<IRBlockId>) -> BIrTarget {
    match t.block {
        IRBlockTargetId::Block(id) => {
            targets.push(id);
            BIrTarget {
                block: IRBlockTargetId::Block(ZERO_BLOCK_ID),
                args: t.args.clone(),
            }
        }
        IRBlockTargetId::Return => BIrTarget {
            block: IRBlockTargetId::Return,
            args: t.args.clone(),
        },
        IRBlockTargetId::Dyn(v) => BIrTarget {
            block: IRBlockTargetId::Dyn(v),
            args: t.args.clone(),
        },
    }
}

fn canon_bir_terminator(t: &BIrTerminator, targets: &mut Vec<IRBlockId>) -> BIrTerminator {
    match t {
        BIrTerminator::Jmp(tgt) => BIrTerminator::Jmp(canon_bir_target(tgt, targets)),
        BIrTerminator::CondJmp {
            val,
            then_target,
            else_target,
        } => BIrTerminator::CondJmp {
            val: *val,
            then_target: canon_bir_target(then_target, targets),
            else_target: canon_bir_target(else_target, targets),
        },
    }
}

