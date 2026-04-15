#![no_std]

use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};
use volar_ir_common::{Constant, Stmt, Type};

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
    Table {
        index: ValueId,
        targets: Vec<Target>,
        default_target: Target,
    },
}

/// A value in a VAFFLE function body.
///
/// Structural values (`Param`, `Call`, `Output`) describe where a value comes
/// from in the dataflow graph.  Pure computational results are expressed as
/// `Op`, whose payload is the shared [`Stmt`] type.  This means VAFFLE and
/// Volar IR share a single definition of every operation — including
/// [`Shuffle`](volar_ir_common::Stmt::Shuffle) — so the two can never drift.
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
    /// Select one output from a multi-result call by index.
    Output {
        value: ValueId,
        idx: usize,
    },
    /// A pure computation (constant, polynomial, shuffle, rotate, merge, …).
    Op(Stmt<ValueId>),
}
