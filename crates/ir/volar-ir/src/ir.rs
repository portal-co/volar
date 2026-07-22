// @reliability: normal
//! @ai: assisted

// Volar IR: SSA block-based IR for VOLE-based computations.
// Pure data structure definitions; no cryptographic claims.
use super::*;

pub use volar_ir_common::{Constant, StorageId, Type as PrimType};
/// Re-export the shared `Stmt` enum so downstream crates can pattern-match
/// on `IRStmt` variants without depending on `volar-ir-common` directly.
pub use volar_ir_common::Stmt;

// ============================================================================
// Type system — unified with VAFFLE via volar_ir_common
// ============================================================================

/// Re-export the shared type ID under the legacy Volar IR name.
/// All downstream code that imports `IRTypeId` from this crate continues to
/// work; only variant-level patterns need updating (e.g. `IRType::Bit` →
/// `IRType::Primitive(Type::Bit)`).
pub use volar_ir_common::TypeId as IRTypeId;

/// Re-export the unified type enum.  Previously `IRType` was defined here;
/// it is now the shared [`volar_ir_common::IrType`] so that VAFFLE and
/// Volar IR cannot drift apart when new type forms are added.
pub use volar_ir_common::IrType as IRType;

/// Re-export the type intern table.
pub use volar_ir_common::TypeTable as IRTypes;

/// Re-export oracle/action/rng declaration types so callers only need `volar_ir`.
pub use volar_ir_common::{ActionDecl, MeasureSpec, OracleDecl, PreInitSegment, ReentryHint, RngDecl, StructRef};

// ============================================================================
// Blocks and control flow
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IRBlockId(pub u32);

/// A complete Volar IR circuit module — a set of blocks with their
/// oracle, action, and RNG declarations.
///
/// The type parameter `P` is an optional provenance annotation.  Each
/// statement in each block carries a `P` value recording where it originated.
/// Use `P = ()` (the default) when provenance is not needed.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IRBlocks<P: Clone = ()> {
    /// Oracles declared for this circuit (resolved by the execution environment).
    pub oracles: Vec<OracleDecl>,
    /// Actions declared for this circuit (resolved by the execution environment).
    pub actions: Vec<ActionDecl>,
    /// RNG sources declared for this circuit (resolved by the execution environment).
    pub rngs: Vec<RngDecl>,
    /// The blocks of the circuit, in order.  Block 0 is the entry.
    pub blocks: Vec<IRBlock<P>>,
    /// Pre-initialised storage segments propagated from WASM data sections.
    pub pre_init: alloc::vec::Vec<PreInitSegment>,
}
impl<P: Clone> IRBlocks<P> {
    /// Construct an `IRBlocks` with no oracle, action, or RNG declarations.
    pub fn new(blocks: Vec<IRBlock<P>>) -> Self {
        IRBlocks {
            oracles: alloc::vec![],
            actions: alloc::vec![],
            rngs: alloc::vec![],
            blocks,
            pre_init: alloc::vec![],
        }
    }

    pub fn is_movfuscated(&self) -> bool {
        self.blocks.len() == 1
    }
    pub fn is_circuit(&self) -> bool {
        self.is_movfuscated()
            && match &self.blocks[0].terminator {
                IRTerminator::Jmp {
                    target:
                        IRBranchTarget {
                            dest: IRBlockTargetId::Return,
                            ..
                        },
                } => true,
                _ => false,
            }
    }
}

/// A single block in a Volar IR circuit.
///
/// The type parameter `P` is an optional per-statement provenance annotation.
/// Each statement also carries an optional `SideId` (see `volar-side`) naming
/// which actor/party/role it belongs to; both annotations live together on
/// the [`Node`](volar_ir_common::Node) wrapping each statement, so they can
/// never drift out of sync with `stmts` the way two parallel `Vec`s could.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IRBlock<P: Clone = ()> {
    pub params: Vec<IRTypeId>,
    pub stmts: Vec<volar_ir_common::Node<IRStmt, P>>,
    pub terminator: IRTerminator,
}

impl<P: Clone> IRBlock<P> {
    /// Append a statement with an explicit provenance annotation and no side.
    /// Returns the [`IRVarId`] for this statement (= index in the block's var space).
    pub fn push_stmt(&mut self, stmt: IRStmt, prov: P) -> IRVarId {
        self.push_stmt_with_side(stmt, prov, None)
    }

    /// Append a statement with an explicit provenance annotation and side.
    /// Returns the [`IRVarId`] for this statement (= index in the block's var space).
    pub fn push_stmt_with_side(&mut self, stmt: IRStmt, prov: P, side: Option<volar_side::SideId>) -> IRVarId {
        let id = IRVarId(self.params.len() as u32 + self.stmts.len() as u32);
        #[cfg(feature = "log-trace")]
        log::trace!(target: "volar::ir", "push_stmt id={}", id.0);
        self.stmts.push(volar_ir_common::Node::new(stmt, prov, side));
        id
    }

    /// Map provenance annotations using a [`ProvenanceHandler`]. `side` is
    /// untouched — provenance and side are independent axes.
    pub fn map_prov_with_handler<H: volar_provenance::ProvenanceHandler<P>>(self, handler: &H) -> IRBlock<H::Output> {
        IRBlock {
            params: self.params,
            stmts: self.stmts.into_iter().map(|n| n.map_prov(|p| handler.map(&p))).collect(),
            terminator: self.terminator,
        }
    }
}

impl<P: Clone> IRBlocks<P> {
    /// Map provenance annotations using a [`ProvenanceHandler`].
    pub fn map_prov_with_handler<H: volar_provenance::ProvenanceHandler<P>>(self, handler: &H) -> IRBlocks<H::Output> {
        IRBlocks {
            oracles: self.oracles,
            actions: self.actions,
            rngs: self.rngs,
            blocks: self.blocks.into_iter().map(|b| b.map_prov_with_handler(handler)).collect(),
            pre_init: self.pre_init,
        }
    }
}

// ============================================================================
// Variable IDs
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IRVarId(pub u32);

// ============================================================================
// Statement type
// ============================================================================

/// Statement type for Volar IR blocks.
///
/// This is a specialisation of the shared [`volar_ir_common::Stmt`] with
/// [`IRVarId`] as the variable reference.  All operations — including
/// [`Shuffle`](volar_ir_common::Stmt::Shuffle) — are defined once in
/// `volar-ir-common` so that VAFFLE and Volar IR cannot drift apart when new
/// operations are added.  Type annotations use the shared [`IRTypeId`]
/// ([`volar_ir_common::TypeId`]) referencing the module's [`IRTypes`].
pub type IRStmt<Var = IRVarId, Addr = Var, Ty = IRTypeId, Stor = volar_ir_common::StorageId> =
    volar_ir_common::Stmt<Var, Addr, Ty, Stor>;

// ============================================================================
// Branch targets
// ============================================================================

/// A jump/branch destination with optional reentry complexity hint.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IRBranchTarget<Var = IRVarId> {
    pub dest: IRBlockTargetId<Var>,
    pub args: Vec<Var>,
    pub reentry: Option<ReentryHint>,
}

impl<Var> IRBranchTarget<Var> {
    pub fn new(dest: IRBlockTargetId<Var>, args: Vec<Var>) -> Self {
        IRBranchTarget { dest, args, reentry: None }
    }

    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        go: &mut impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
    ) -> Result<IRBranchTarget<NV>, E>
    where
        NV: Ord,
    {
        Ok(IRBranchTarget {
            dest: self.dest.map(ctx, go)?,
            args: self
                .args
                .into_iter()
                .map(|v| go(ctx, v))
                .collect::<Result<Vec<NV>, E>>()?,
            reentry: self.reentry,
        })
    }

    pub fn as_ref(&self) -> IRBranchTarget<&Var>
    where
        Var: Ord,
    {
        IRBranchTarget {
            dest: self.dest.as_ref(),
            args: self.args.iter().collect(),
            reentry: self.reentry.clone(),
        }
    }

    pub fn as_mut(&mut self) -> IRBranchTarget<&mut Var>
    where
        Var: Ord,
    {
        IRBranchTarget {
            dest: self.dest.as_mut(),
            args: self.args.iter_mut().collect(),
            reentry: self.reentry.clone(),
        }
    }
}

// ============================================================================
// Terminators
// ============================================================================

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum IRTerminator<Var = IRVarId> {
    Jmp {
        target: IRBranchTarget<Var>,
    },
    JumpCond {
        condition: Var,
        then_target: IRBranchTarget<Var>,
        else_target: IRBranchTarget<Var>,
    },
    JumpTable {
        index: Var,
        cases: BTreeMap<Constant, IRBranchTarget<Var>>,
    },
}

impl<Var> IRTerminator<Var> {
    /// Map the variable parameter, potentially fallibly.
    ///
    /// `ctx` is passed to `go` on every call so the callback can share
    /// mutable state without borrow conflicts.
    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        mut go: impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
    ) -> Result<IRTerminator<NV>, E>
    where
        NV: Ord,
    {
        Ok(match self {
            IRTerminator::Jmp { target } => IRTerminator::Jmp {
                target: target.map(ctx, &mut go)?,
            },
            IRTerminator::JumpCond {
                condition,
                then_target,
                else_target,
            } => IRTerminator::JumpCond {
                condition: go(ctx, condition)?,
                then_target: then_target.map(ctx, &mut go)?,
                else_target: else_target.map(ctx, &mut go)?,
            },
            IRTerminator::JumpTable { index, cases } => IRTerminator::JumpTable {
                index: go(ctx, index)?,
                cases: cases
                    .into_iter()
                    .map(|(k, target)| {
                        let target = target.map(ctx, &mut go)?;
                        Ok((k, target))
                    })
                    .collect::<Result<BTreeMap<Constant, IRBranchTarget<NV>>, E>>()?,
            },
        })
    }

    /// Borrow the variable parameter in place.
    pub fn as_ref(&self) -> IRTerminator<&Var>
    where
        Var: Ord,
    {
        match self {
            IRTerminator::Jmp { target } => IRTerminator::Jmp {
                target: target.as_ref(),
            },
            IRTerminator::JumpCond {
                condition,
                then_target,
                else_target,
            } => IRTerminator::JumpCond {
                condition,
                then_target: then_target.as_ref(),
                else_target: else_target.as_ref(),
            },
            IRTerminator::JumpTable { index, cases } => IRTerminator::JumpTable {
                index,
                cases: cases
                    .iter()
                    .map(|(k, target)| (*k, target.as_ref()))
                    .collect(),
            },
        }
    }

    /// Mutably borrow the variable parameter in place.
    pub fn as_mut(&mut self) -> IRTerminator<&mut Var>
    where
        Var: Ord,
    {
        match self {
            IRTerminator::Jmp { target } => IRTerminator::Jmp {
                target: target.as_mut(),
            },
            IRTerminator::JumpCond {
                condition,
                then_target,
                else_target,
            } => IRTerminator::JumpCond {
                condition,
                then_target: then_target.as_mut(),
                else_target: else_target.as_mut(),
            },
            IRTerminator::JumpTable { index, cases } => IRTerminator::JumpTable {
                index,
                cases: cases
                    .iter_mut()
                    .map(|(k, target)| (*k, target.as_mut()))
                    .collect(),
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum IRBlockTargetId<Var = IRVarId> {
    Block(IRBlockId),
    Return,
    Dyn(Var),
}

impl<Var> IRBlockTargetId<Var> {
    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        go: &mut impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
    ) -> Result<IRBlockTargetId<NV>, E> {
        Ok(match self {
            IRBlockTargetId::Block(b) => IRBlockTargetId::Block(b),
            IRBlockTargetId::Return => IRBlockTargetId::Return,
            IRBlockTargetId::Dyn(v) => IRBlockTargetId::Dyn(go(ctx, v)?),
        })
    }

    pub fn as_ref(&self) -> IRBlockTargetId<&Var> {
        match self {
            IRBlockTargetId::Block(b) => IRBlockTargetId::Block(*b),
            IRBlockTargetId::Return => IRBlockTargetId::Return,
            IRBlockTargetId::Dyn(v) => IRBlockTargetId::Dyn(v),
        }
    }

    pub fn as_mut(&mut self) -> IRBlockTargetId<&mut Var> {
        match self {
            IRBlockTargetId::Block(b) => IRBlockTargetId::Block(*b),
            IRBlockTargetId::Return => IRBlockTargetId::Return,
            IRBlockTargetId::Dyn(v) => IRBlockTargetId::Dyn(v),
        }
    }
}
