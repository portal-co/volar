#![no_std]

use alloc::{collections::btree_map::BTreeMap, vec::Vec};
extern crate alloc;
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
pub enum IRStmt {
    StorageRead {
        ty: IRTypeId,
        addr: IRVarId,
    },
    StorageWrite {
        src: IRVarId,
        ty: IRTypeId,
        addr: IRVarId,
    },
    Const(Vec<u8>, IRTypeId),
    Transmute {
        src: IRVarId,
        src_ty: IRTypeId,
        dst_ty: IRTypeId,
    },
    Poly {
        coeffs: BTreeMap<Vec<IRVarId>,u8>,
        constant: Vec<u8>,
    },
    Rol {
        src: IRVarId,
        ty: IRTypeId,
        n: usize,
    },
    Ror {
        src: IRVarId,
        ty: IRTypeId,
        n: usize,
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
    }
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum IRBlockTargetId {
    Block(IRBlockId),
    Return,
    Dyn(IRVarId),
}
