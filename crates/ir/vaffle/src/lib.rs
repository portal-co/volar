#![no_std]

use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};

extern crate alloc;
pub struct Module {
    pub funcs: Vec<FuncDecl>,
    pub sigs: Vec<SigDecl>,
    pub exports: BTreeMap<String, FuncId>,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SigId(pub usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FuncId(pub usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct BlockId(pub usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ValueId(pub usize);
pub struct SigDecl {
    pub params: Vec<Type>,
    pub results: Vec<Type>,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[non_exhaustive]
pub enum Type {
    _32,
    _64,
    _16,
    _8,
    AES8,
}
pub enum FuncDecl {
    Import {
        module: String,
        name: String,
        sig: SigId,
    },
    Body(FuncBody),
}
pub struct FuncBody {
    pub sig: SigId,
    pub blocks: Vec<Block>,
    pub values: Vec<Value>,
    pub entry: BlockId,
}
pub struct Block {
    pub params: Vec<(ValueId, Type)>,
    pub stmts: Vec<ValueId>,
    pub terminator: Terminator,
}
pub struct Target {
    pub block: BlockId,
    pub args: Vec<ValueId>,
}
pub enum Terminator {
    Return { values: Vec<ValueId> },
    Jump(Target),
    ReturnCall { func: FuncId, args: Vec<ValueId> },
}
pub enum Value {
    Param {
        block: BlockId,
        ty: Type,
        idx: usize,
    },
    Call {
        func: FuncId,
        args: Vec<ValueId>,
    },
    Output {
        value: ValueId,
        idx: usize,
    },
}
