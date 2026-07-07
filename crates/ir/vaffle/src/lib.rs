#![no_std]

use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};
use volar_ir_common::{Constant, IrType, Node, OracleDecl, ActionDecl, PreInitSegment, Stmt, Type, TypeId, TypeTable};

extern crate alloc;

/// A VAFFLE module: the top-level container for types, signatures, functions,
/// and the symbol table.
///
/// `types` is the shared [`TypeTable`] that all [`TypeId`] references within
/// this module index into.  Construct it with [`TypeTable::new`] and use
/// [`TypeTable::intern`] / [`TypeTable::primitive`] to populate it.
#[derive(Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct Module {
    /// Shared type intern table.
    pub types: TypeTable,
    /// Declared pure oracles available in this module.
    pub oracles: Vec<OracleDecl>,
    /// Declared conditional actions available in this module.
    pub actions: Vec<ActionDecl>,
    pub funcs: Vec<FuncDecl>,
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
pub enum FuncDecl {
    Import {
        module: String,
        name: String,
        sig: SigId,
    },
    Body(FuncBody),
}
/// `values` is a flat arena of every `Value` in the function, addressed by
/// [`ValueId`]. Each entry carries its own provenance and [`SideId`](volar_side::SideId)
/// via the [`Node`] wrapper — this is the single source of truth for both
/// annotations; [`Block::stmts`] is ordering-only and holds no metadata of
/// its own.
#[derive(Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct FuncBody {
    pub sig: SigId,
    pub blocks: Vec<Block>,
<<<<<<< HEAD
    pub values: Vec<Node<Value, P>>,
=======
    pub values: Vec<Value>,
>>>>>>> origin/main
    pub entry: BlockId,
}
/// `Block` carries no provenance/side metadata of its own — those annotations
/// live on the [`FuncBody::values`] arena entry that each [`ValueId`] in
/// `stmts` points to, so `Block` does not need to be generic over `P`.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct Block {
    /// Block parameters: `(value_id, type_id)` pairs.
    pub params: Vec<(ValueId, TypeId)>,
    pub stmts: Vec<ValueId>,
    pub terminator: Terminator,
}
use volar_ir_common::ReentryHint;

#[derive(Clone, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct Target {
    pub block: BlockId,
<<<<<<< HEAD
    pub args: Vec<V>,
    pub reentry: Option<ReentryHint>,
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
            reentry: self.reentry,
        })
    }

    pub fn as_ref(&self) -> Target<&V> {
        Target { block: self.block, args: self.args.iter().collect(), reentry: self.reentry.clone() }
    }

    pub fn as_mut(&mut self) -> Target<&mut V> {
        Target { block: self.block, args: self.args.iter_mut().collect(), reentry: self.reentry.clone() }
    }
}

=======
    pub args: Vec<ValueId>,
}
>>>>>>> origin/main
#[derive(Clone, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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
/// `Op`, whose payload is the shared [`Stmt`] type — the same set of
/// operations used by Volar IR, including
/// [`Shuffle`](volar_ir_common::Stmt::Shuffle).
///
/// Type annotations inside `Op` are [`TypeId`] references into the containing
/// [`Module::types`] table, consistent with [`Param`](Value::Param) and the
/// rest of the module.
#[derive(Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum Value {
    Param {
        block: BlockId,
        /// The parameter's type, as a [`TypeId`] into [`Module::types`].
        ty: TypeId,
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
    ///
    /// Type annotations in the inner [`Stmt`] reference the same
    /// [`Module::types`] table as the rest of the module.
    Op(Stmt<ValueId>),
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
        ptr: ValueId,
        pointee_ty: TypeId,
    },
    /// Store `val` through a stack pointer.  No result value.
    PtrStore {
        ptr: ValueId,
        val: ValueId,
    },
    /// Element-wise pointer offset: `ptr + idx` elements (not bytes).
    ///
    /// `elem_bits` is the element width in storage slots so the lowering
    /// can compute the byte offset without re-inspecting the type table.
    PtrOffset {
        ptr: ValueId,
        idx: ValueId,
        elem_bits: usize,
    },
}
