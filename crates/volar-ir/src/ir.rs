use super::*;
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlockId(pub usize);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlocks(pub Vec<IRBlock>);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRBlock {
    pub params: Vec<IRTypeId>,
    pub stmts: Vec<IRStmt>,
    pub terminator: IRTerminator,
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRTypeId(pub usize);
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
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRVarId(pub usize);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IRStmt<Var = IRVarId> {
    StorageRead {
        ty: IRTypeId,
        addr: Var,
    },
    StorageWrite {
        src: Var,
        ty: IRTypeId,
        addr: Var,
    },
    Const(Vec<u8>, IRTypeId),
    Transmute {
        src: Var,
        src_ty: IRTypeId,
        dst_ty: IRTypeId,
    },
    Poly {
        coeffs: BTreeMap<Vec<Var>, u8>,
        constant: Vec<u8>,
    },
    Rol {
        src: Var,
        ty: IRTypeId,
        n: usize,
    },
    Ror {
        src: Var,
        ty: IRTypeId,
        n: usize,
    },
    Merge {
        parts: Vec<Var>,
        ty: IRTypeId,
    },
    Splat {
        src: Var,
        ty: IRTypeId,
    },
}
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
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IRBlockTargetId {
    Block(IRBlockId),
    Return,
    Dyn(IRVarId),
}
