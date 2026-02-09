use super::*;
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IRVarId(pub u32);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IRStmt<Var = IRVarId, Addr = Var> {
    StorageRead {
        ty: IRTypeId,
        addr: Addr,
    },
    StorageWrite {
        src: Var,
        ty: IRTypeId,
        addr: Addr,
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
