// @reliability: normal
//! @ai: none

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

/// Re-export oracle/action declaration types so callers only need `volar_ir`.
pub use volar_ir_common::{ActionDecl, OracleDecl};

// ============================================================================
// Blocks and control flow
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlockId(pub u32);

/// A complete Volar IR circuit module — a set of blocks with their
/// oracle and action declarations.
///
/// The type parameter `P` is an optional provenance annotation.  Each
/// statement in each block carries a `P` value recording where it originated.
/// Use `P = ()` (the default) when provenance is not needed.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlocks<P: Clone + Default = ()> {
    /// Oracles declared for this circuit (resolved by the execution environment).
    pub oracles: Vec<OracleDecl>,
    /// Actions declared for this circuit (resolved by the execution environment).
    pub actions: Vec<ActionDecl>,
    /// The blocks of the circuit, in order.  Block 0 is the entry.
    pub blocks: Vec<IRBlock<P>>,
}
impl<P: Clone + Default> IRBlocks<P> {
    /// Construct an `IRBlocks` with no oracle or action declarations.
    pub fn new(blocks: Vec<IRBlock<P>>) -> Self {
        IRBlocks { oracles: alloc::vec![], actions: alloc::vec![], blocks }
    }

    pub fn is_movfuscated(&self) -> bool {
        self.blocks.len() == 1
    }
    pub fn is_circuit(&self) -> bool {
        self.is_movfuscated()
            && match self.blocks[0].terminator {
                IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    ..
                } => true,
                _ => false,
            }
    }
}

/// A single block in a Volar IR circuit.
///
/// The type parameter `P` is an optional per-statement provenance annotation
/// (parallel to `stmts`).  Use `P = ()` when provenance is not needed.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlock<P: Clone + Default = ()> {
    pub params: Vec<IRTypeId>,
    pub stmts: Vec<IRStmt>,
    /// Per-statement provenance, same length as `stmts`.
    /// Index `i` is the provenance of `stmts[i]`.
    pub stmt_provs: Vec<P>,
    pub terminator: IRTerminator,
}

impl<P: Clone + Default> IRBlock<P> {
    /// Append a statement with an explicit provenance annotation.
    /// Returns the [`IRVarId`] for this statement (= index in the block's var space).
    pub fn push_stmt(&mut self, stmt: IRStmt, prov: P) -> IRVarId {
        let id = IRVarId(self.params.len() as u32 + self.stmts.len() as u32);
        self.stmts.push(stmt);
        self.stmt_provs.push(prov);
        id
    }

    /// Append a statement using `P::default()` as the provenance.
    pub fn push_stmt_default(&mut self, stmt: IRStmt) -> IRVarId {
        self.push_stmt(stmt, P::default())
    }

    /// Map provenance annotations using a [`ProvenanceHandler`].
    pub fn map_prov_with_handler<H: volar_provenance::ProvenanceHandler<P>>(self, handler: &H) -> IRBlock<H::Output> {
        IRBlock {
            params: self.params,
            stmts: self.stmts,
            stmt_provs: self.stmt_provs.into_iter().map(|p| handler.map(&p)).collect(),
            terminator: self.terminator,
        }
    }
}

impl<P: Clone + Default> IRBlocks<P> {
    /// Map provenance annotations using a [`ProvenanceHandler`].
    pub fn map_prov_with_handler<H: volar_provenance::ProvenanceHandler<P>>(self, handler: &H) -> IRBlocks<H::Output> {
        IRBlocks {
            oracles: self.oracles,
            actions: self.actions,
            blocks: self.blocks.into_iter().map(|b| b.map_prov_with_handler(handler)).collect(),
        }
    }
}

// ============================================================================
// Variable IDs
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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
pub type IRStmt<Var = IRVarId, Addr = Var> = volar_ir_common::Stmt<Var, Addr>;

// ============================================================================
// Terminators
// ============================================================================

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IRTerminator {
    Jmp {
        func: IRBlockTargetId,
        args: Vec<IRVarId>,
    },
    JumpCond {
        condition: IRVarId,
        true_block: IRBlockTargetId,
        true_args: Vec<IRVarId>,
        false_block: IRBlockTargetId,
        false_args: Vec<IRVarId>,
    },
    JumpTable {
        index: IRVarId,
        cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)>,
        // no default; must be exhaustive
    },
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IRBlockTargetId {
    Block(IRBlockId),
    Return,
    Dyn(IRVarId),
}
