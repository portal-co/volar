// @reliability: normal
//! @ai: none
use volar_ir_common::Constant;

// Volar IR: SSA block-based IR for VOLE-based computations.
// Pure data structure definitions; no cryptographic claims.
use super::*;
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRTypeId(pub u32);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRTypes(pub Vec<IRType>);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IRType {
    Bit,
    Vec(usize, IRTypeId),
    Tuple(Vec<IRTypeId>),
    Galois8AES,
    Galois64,
    Block { params: Vec<IRTypeId> },
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRVarId(pub u32);
/// Statement type for Volar IR blocks.
///
/// This is a specialisation of the shared [`volar_ir_common::Stmt`] with
/// `IRTypeId` as the type annotation.  All operations — including
/// [`Shuffle`](volar_ir_common::Stmt::Shuffle) — are defined once in
/// `volar-ir-common` so that VAFFLE and Volar IR can never drift apart.
pub type IRStmt<Var = IRVarId, Addr = Var> = volar_ir_common::Stmt<Var, Addr, IRTypeId>;
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
    JumpTable{
        index: IRVarId,
        cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)>,
        // no default; must be exhaustive
    }
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IRBlockTargetId {
    Block(IRBlockId),
    Return,
    Dyn(IRVarId),
}
