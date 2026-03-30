#![no_std]

use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};
use volar_ir_common::Type;

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
    IfNonzero {
        cond: ValueId,
        then_target: Target,
        else_target: Target,
    },
    Table{
        index: ValueId,
        targets: Vec<Target>,
        default_target: Target,
    },
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
