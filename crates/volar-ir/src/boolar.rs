use super::{ir::*, *};
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BIrBlocks(pub Vec<BIrBlock>);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BIrBlock {
    pub params: u32,
    pub stmts: Vec<BIrStmt>,
    pub terminator: BIrTerminator,
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BIrStmt {
    Zero,
    One,
    And(IRVarId, IRVarId),
    Or(IRVarId, IRVarId),
    Xor(IRVarId, IRVarId),
    Not(IRVarId),
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BIrTerminator {
    Jmp(BIrTarget),
    CondJmp {
        val: IRVarId,
        then_target: BIrTarget,
        else_target: BIrTarget,
    },
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BIrTarget {
    pub block: IRBlockTargetId,
    pub args: Vec<IRVarId>,
}
