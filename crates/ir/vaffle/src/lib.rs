#![no_std]

use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};
use volar_ir_common::{Constant, IrType, OracleDecl, ActionDecl, PreInitSegment, Stmt, Type, TypeId, TypeTable};

extern crate alloc;

/// A VAFFLE module: the top-level container for types, signatures, functions,
/// and the symbol table.
///
/// `types` is the shared [`TypeTable`] that all [`TypeId`] references within
/// this module index into.  Construct it with [`TypeTable::new`] and use
/// [`TypeTable::intern`] / [`TypeTable::primitive`] to populate it.
#[derive(Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct Module<P: Clone = ()> {
    /// Shared type intern table.
    pub types: TypeTable,
    /// Declared pure oracles available in this module.
    pub oracles: Vec<OracleDecl>,
    /// Declared conditional actions available in this module.
    pub actions: Vec<ActionDecl>,
    pub funcs: Vec<FuncDecl<P>>,
    pub sigs: Vec<SigDecl>,
    pub exports: BTreeMap<String, FuncId>,
    /// Pre-initialised storage segments (from WASM active data segments).
    pub pre_init: Vec<PreInitSegment>,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct SigId(pub usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct FuncId(pub usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct BlockId(pub usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct ValueId(pub usize);

/// A function signature: parameter types and result types, expressed as
/// [`TypeId`] references into the containing [`Module::types`] table.
///
/// For import declarations the same information is also available as
/// `IrType::Func` in the type table, allowing function types to be used as
/// first-class values in VAFFLE programs.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct SigDecl {
    pub params: Vec<TypeId>,
    pub results: Vec<TypeId>,
}

#[derive(Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum FuncDecl<P: Clone = ()> {
    Import {
        module: String,
        name: String,
        sig: SigId,
    },
    Body(FuncBody<P>),
}
#[derive(Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct FuncBody<P: Clone = ()> {
    pub sig: SigId,
    pub blocks: Vec<Block<P>>,
    pub values: Vec<Value>,
    pub entry: BlockId,
}
#[derive(Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct Block<P: Clone = ()> {
    /// Block parameters: `(value_id, type_id)` pairs.
    pub params: Vec<(ValueId, TypeId)>,
    pub stmts: Vec<ValueId>,
    pub stmt_provs: Vec<P>,
    pub terminator: Terminator,
}
#[derive(Clone, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct Target<V = ValueId> {
    pub block: BlockId,
    pub args: Vec<V>,
}

impl<V> Target<V> {
    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        go: &mut impl FnMut(&mut Ctx, V) -> Result<NV, E>,
    ) -> Result<Target<NV>, E> {
        Ok(Target {
            block: self.block,
            args: self.args.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
        })
    }

    pub fn as_ref(&self) -> Target<&V> {
        Target { block: self.block, args: self.args.iter().collect() }
    }

    pub fn as_mut(&mut self) -> Target<&mut V> {
        Target { block: self.block, args: self.args.iter_mut().collect() }
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum Terminator<V = ValueId> {
    Return { values: Vec<V> },
    Jump(Target<V>),
    ReturnCall { func: FuncId, args: Vec<V> },
    IfNonzero {
        cond: V,
        then_target: Target<V>,
        else_target: Target<V>,
    },
    Table {
        index: V,
        targets: Vec<Target<V>>,
        default_target: Target<V>,
    },
}

impl<V> Terminator<V> {
    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        mut go: impl FnMut(&mut Ctx, V) -> Result<NV, E>,
    ) -> Result<Terminator<NV>, E> {
        Ok(match self {
            Terminator::Return { values } => Terminator::Return {
                values: values.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
            },
            Terminator::Jump(t) => Terminator::Jump(t.map(ctx, &mut go)?),
            Terminator::ReturnCall { func, args } => Terminator::ReturnCall {
                func,
                args: args.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
            },
            Terminator::IfNonzero { cond, then_target, else_target } => Terminator::IfNonzero {
                cond: go(ctx, cond)?,
                then_target: then_target.map(ctx, &mut go)?,
                else_target: else_target.map(ctx, &mut go)?,
            },
            Terminator::Table { index, targets, default_target } => Terminator::Table {
                index: go(ctx, index)?,
                targets: targets
                    .into_iter()
                    .map(|t| t.map(ctx, &mut go))
                    .collect::<Result<Vec<Target<NV>>, E>>()?,
                default_target: default_target.map(ctx, &mut go)?,
            },
        })
    }

    pub fn as_ref(&self) -> Terminator<&V> {
        match self {
            Terminator::Return { values } => Terminator::Return { values: values.iter().collect() },
            Terminator::Jump(t) => Terminator::Jump(t.as_ref()),
            Terminator::ReturnCall { func, args } => {
                Terminator::ReturnCall { func: *func, args: args.iter().collect() }
            }
            Terminator::IfNonzero { cond, then_target, else_target } => Terminator::IfNonzero {
                cond,
                then_target: then_target.as_ref(),
                else_target: else_target.as_ref(),
            },
            Terminator::Table { index, targets, default_target } => Terminator::Table {
                index,
                targets: targets.iter().map(|t| t.as_ref()).collect(),
                default_target: default_target.as_ref(),
            },
        }
    }

    pub fn as_mut(&mut self) -> Terminator<&mut V> {
        match self {
            Terminator::Return { values } => {
                Terminator::Return { values: values.iter_mut().collect() }
            }
            Terminator::Jump(t) => Terminator::Jump(t.as_mut()),
            Terminator::ReturnCall { func, args } => {
                Terminator::ReturnCall { func: *func, args: args.iter_mut().collect() }
            }
            Terminator::IfNonzero { cond, then_target, else_target } => Terminator::IfNonzero {
                cond,
                then_target: then_target.as_mut(),
                else_target: else_target.as_mut(),
            },
            Terminator::Table { index, targets, default_target } => Terminator::Table {
                index,
                targets: targets.iter_mut().map(|t| t.as_mut()).collect(),
                default_target: default_target.as_mut(),
            },
        }
    }
}

/// A value in a VAFFLE function body.
///
/// Structural values (`Param`, `Call`, `Output`) describe where a value comes
/// from in the dataflow graph.  Pure computational results are expressed as
/// `Op`, whose payload is the shared [`Stmt`] type — the same set of
/// operations used by Volar IR, including
/// [`Shuffle`](volar_ir_common::Stmt::Shuffle).
///
/// Type annotations inside `Op` are [`TypeId`] references into the containing
/// [`Module::types`] table, consistent with [`Param`](Value::Param) and the
/// rest of the module.
#[derive(Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum Value<V = ValueId> {
    Param {
        block: BlockId,
        /// The parameter's type, as a [`TypeId`] into [`Module::types`].
        ty: TypeId,
        idx: usize,
    },
    Call {
        func: FuncId,
        args: Vec<V>,
    },
    /// Select one output from a multi-result call by index.
    Output {
        value: V,
        idx: usize,
    },
    /// A pure computation (constant, polynomial, shuffle, rotate, merge, …).
    ///
    /// Type annotations in the inner [`Stmt`] reference the same
    /// [`Module::types`] table as the rest of the module.
    Op(Stmt<V>),
    /// Allocate `count` elements of `elem_ty` on the function's stack frame.
    ///
    /// Returns a pointer (address bits) into `StorageId::STACK`.  The
    /// allocated region is valid for the lifetime of the enclosing function
    /// call.  `base_slot` is the compile-time slot offset assigned by the
    /// target when the allocation was emitted.
    StackAlloc {
        elem_ty: TypeId,
        count: usize,
        /// The first stack-storage slot assigned to this allocation.
        base_slot: u64,
    },
    /// Load a value through a stack pointer.
    ///
    /// `ptr` is a stack address (as emitted by `StackAlloc` or `PtrOffset`).
    /// `pointee_ty` is the type of the loaded value.
    PtrLoad {
        ptr: V,
        pointee_ty: TypeId,
    },
    /// Store `val` through a stack pointer.  No result value.
    PtrStore {
        ptr: V,
        val: V,
    },
    /// Element-wise pointer offset: `ptr + idx` elements (not bytes).
    ///
    /// `elem_bits` is the element width in storage slots so the lowering
    /// can compute the byte offset without re-inspecting the type table.
    PtrOffset {
        ptr: V,
        idx: V,
        elem_bits: usize,
    },
}

impl<V> Value<V> {
    /// Map the value-id parameter, potentially fallibly.
    ///
    /// For `Op(Stmt<V>)`, maps all `Var` and `Addr` positions using `go`,
    /// leaving `TypeId` and `StorageId` fields unchanged.
    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        mut go: impl FnMut(&mut Ctx, V) -> Result<NV, E>,
    ) -> Result<Value<NV>, E>
    where
        V: Ord,
        NV: Ord,
    {
        Ok(match self {
            Value::Param { block, ty, idx } => Value::Param { block, ty, idx },
            Value::Call { func, args } => Value::Call {
                func,
                args: args.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
            },
            Value::Output { value, idx } => Value::Output { value: go(ctx, value)?, idx },
            Value::Op(stmt) => {
                Value::Op(stmt.map_var(
                    ctx,
                    &mut go,
                    &mut |_, ty| Ok(ty),
                    &mut |_, s| Ok(s),
                )?)
            }
            Value::StackAlloc { elem_ty, count, base_slot } => {
                Value::StackAlloc { elem_ty, count, base_slot }
            }
            Value::PtrLoad { ptr, pointee_ty } => {
                Value::PtrLoad { ptr: go(ctx, ptr)?, pointee_ty }
            }
            Value::PtrStore { ptr, val } => {
                Value::PtrStore { ptr: go(ctx, ptr)?, val: go(ctx, val)? }
            }
            Value::PtrOffset { ptr, idx, elem_bits } => Value::PtrOffset {
                ptr: go(ctx, ptr)?,
                idx: go(ctx, idx)?,
                elem_bits,
            },
        })
    }
}
