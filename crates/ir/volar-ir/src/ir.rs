// @reliability: normal
//! @ai: none
use volar_ir_common::Constant;

// Volar IR: SSA block-based IR for VOLE-based computations.
// Pure data structure definitions; no cryptographic claims.
use super::*;

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

// ============================================================================
// Blocks and control flow
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlockId(pub u32);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlocks(pub Vec<IRBlock>);
impl IRBlocks {
    pub fn is_movfuscated(&self) -> bool {
        return self.0.len() == 1;
    }
    pub fn is_circuit(&self) -> bool {
        return self.is_movfuscated()
            && match self.0[0].terminator {
                IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    ..
                } => true,
                _ => false,
            };
    }
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlock {
    pub params: Vec<IRTypeId>,
    pub stmts: Vec<IRStmt>,
    pub terminator: IRTerminator,
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
