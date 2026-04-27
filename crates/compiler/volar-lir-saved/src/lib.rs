// @reliability: experimental
// @ai: assisted
//! Serializable "recorded LIR" format.
//!
//! [`RecordingTarget`] implements [`LirTarget`] (and [`StackAllocExt`]) by
//! recording every API call into a [`SavedLirModule`].  The saved module can
//! be serialized with `rkyv` (feature `"rkyv"`) and later replayed into any
//! target via [`SavedLirModule::replay`].
//!
//! # Value and Block handles
//!
//! Both are represented as `u32` indices assigned monotonically.  The
//! recording target returns handles in order; the replay engine maps them
//! back to the real handles produced by the downstream target.
//!
//! # StackAllocExt
//!
//! [`RecordingTarget`] also implements [`StackAllocExt`].  When replaying
//! into a target that does not support stack allocation (i.e. one whose
//! [`LirTarget::stack_alloc_ext`] returns `None`), the replay will panic at
//! runtime on any `Alloca` / `PtrLoad` / `PtrStore` / `PtrOffset` call.

#![no_std]
extern crate alloc;

use alloc::{vec, vec::Vec};
use volar_lir::{IcmpPred, LirAbi, LirTarget, LirType, StackAllocExt, StructDef, StructId};

// ============================================================================
// Call log
// ============================================================================

/// A single recorded [`LirTarget`] API call.
///
/// Values and blocks are referred to by their `u32` allocation index.
/// The special index `u32::MAX` is never assigned and can be used as a
/// sentinel.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum LirCall {
    // ---- Type registration --------------------------------------------------
    DefineStruct {
        def: StructDef,
        /// The `StructId` returned by the original `define_struct` call.
        id: StructId,
    },

    // ---- Function management ------------------------------------------------
    BeginFunction {
        name: alloc::string::String,
        params: Vec<LirType>,
        ret: Option<LirType>,
        /// The entry block index returned.
        entry_block: u32,
        /// For each parameter, the flat scalar value indices returned.
        param_vals: Vec<Vec<u32>>,
    },
    EndFunction,

    // ---- Block management ---------------------------------------------------
    CreateBlock {
        /// The block index assigned.
        block: u32,
    },
    AddBlockParam {
        block: u32,
        ty: LirType,
        /// The value index assigned.
        val: u32,
    },
    SwitchToBlock {
        block: u32,
    },

    // ---- Constants ----------------------------------------------------------
    Iconst {
        ty: LirType,
        val: i64,
        /// Output value index.
        out: u32,
    },

    // ---- Arithmetic ---------------------------------------------------------
    Add { lhs: u32, rhs: u32, out: u32 },
    Sub { lhs: u32, rhs: u32, out: u32 },
    Mul { lhs: u32, rhs: u32, out: u32 },
    Udiv { lhs: u32, rhs: u32, out: u32 },
    Sdiv { lhs: u32, rhs: u32, out: u32 },

    // ---- Bitwise ------------------------------------------------------------
    And { lhs: u32, rhs: u32, out: u32 },
    Or  { lhs: u32, rhs: u32, out: u32 },
    Xor { lhs: u32, rhs: u32, out: u32 },
    Not { val: u32, out: u32 },
    Shl  { val: u32, shift: u32, out: u32 },
    Lshr { val: u32, shift: u32, out: u32 },
    Ashr { val: u32, shift: u32, out: u32 },

    // ---- Comparison ---------------------------------------------------------
    Icmp { pred: IcmpPred, lhs: u32, rhs: u32, out: u32 },

    // ---- Conversions --------------------------------------------------------
    Zext { val: u32, dst_ty: LirType, out: u32 },
    Sext { val: u32, dst_ty: LirType, out: u32 },
    Trunc { val: u32, dst_ty: LirType, out: u32 },

    // ---- Select -------------------------------------------------------------
    Select { cond: u32, then_val: u32, else_val: u32, out: u32 },

    // ---- Terminators --------------------------------------------------------
    Jump { target: u32, args: Vec<u32> },
    Branch {
        cond: u32,
        then_block: u32,
        then_args: Vec<u32>,
        else_block: u32,
        else_args: Vec<u32>,
    },
    Ret { vals: Vec<u32> },

    // ---- Extern calls -------------------------------------------------------
    CallExtern {
        name: alloc::string::String,
        arg_tys: Vec<LirType>,
        args: Vec<u32>,
        ret_ty: Option<LirType>,
        /// Output scalar indices.
        outs: Vec<u32>,
    },

    // ---- Crypto primitives --------------------------------------------------
    Oracle {
        name: alloc::string::String,
        arg_tys: Vec<LirType>,
        args: Vec<u32>,
        ret_tys: Vec<LirType>,
        outs: Vec<u32>,
    },
    Action {
        name: alloc::string::String,
        guard: u32,
        arg_tys: Vec<LirType>,
        args: Vec<u32>,
        fallbacks: Vec<u32>,
        ret_tys: Vec<LirType>,
        outs: Vec<u32>,
    },
    Rng {
        ty: LirType,
        out: u32,
    },

    // ---- StackAllocExt ------------------------------------------------------
    Alloca {
        elem_ty: LirType,
        count: usize,
        out: u32,
    },
    PtrLoad {
        ptr: u32,
        ty: LirType,
        out: u32,
    },
    PtrStore {
        ptr: u32,
        val: u32,
    },
    PtrOffset {
        ptr: u32,
        idx: u32,
        out: u32,
    },

    // ---- Indexed pointer ops ------------------------------------------------
    PtrIndexLoad {
        ptr: u32,
        idx: u32,
        pointee_ty: LirType,
        outs: Vec<u32>,
    },
    PtrIndexStore {
        ptr: u32,
        idx: u32,
        vals: Vec<u32>,
        pointee_ty: LirType,
    },

    // ---- Value type storage -------------------------------------------------
    /// Records the type of a value so `value_scalar_type` can answer queries
    /// during replay.
    ValueType {
        val: u32,
        ty: LirType,
    },

    // ---- Provenance ---------------------------------------------------------
    SetProv,
}

// ============================================================================
// SavedLirModule
// ============================================================================

/// A recorded sequence of [`LirTarget`] API calls that can be replayed into
/// any target.
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct SavedLirModule {
    pub calls: Vec<LirCall>,
}

impl SavedLirModule {
    /// Replay this module into `target`.
    ///
    /// Values and blocks are mapped from their recorded `u32` indices to the
    /// handles produced by the downstream target.
    ///
    /// # Panics
    ///
    /// Panics if the call log contains `Alloca` / `PtrLoad` / `PtrStore` /
    /// `PtrOffset` and `target.stack_alloc_ext()` returns `None`.
    pub fn replay<T: LirTarget>(&self, target: &mut T) {
        let mut vals: Vec<T::Value> = Vec::new();
        let mut blocks: Vec<T::Block> = Vec::new();

        // Helper closures — defined as macros to avoid borrow issues.
        macro_rules! val {
            ($idx:expr) => {
                vals[*$idx as usize].clone()
            };
        }
        macro_rules! block {
            ($idx:expr) => {
                blocks[*$idx as usize].clone()
            };
        }
        macro_rules! push_val {
            ($v:expr) => {
                vals.push($v)
            };
        }
        macro_rules! push_block {
            ($b:expr) => {
                blocks.push($b)
            };
        }

        for call in &self.calls {
            match call {
                LirCall::DefineStruct { def, .. } => {
                    target.define_struct(def.clone());
                }

                LirCall::BeginFunction { name, params, ret, entry_block: _, param_vals: _ } => {
                    let (entry, pvs) = target.begin_function(name, params, ret.clone());
                    push_block!(entry);
                    for pv_group in pvs {
                        for v in pv_group {
                            push_val!(v);
                        }
                    }
                }

                LirCall::EndFunction => {
                    target.end_function();
                }

                LirCall::CreateBlock { .. } => {
                    let b = target.create_block();
                    push_block!(b);
                }

                LirCall::AddBlockParam { block, ty, .. } => {
                    let v = target.add_block_param(block!(block), ty.clone());
                    push_val!(v);
                }

                LirCall::SwitchToBlock { block } => {
                    target.switch_to_block(block!(block));
                }

                LirCall::Iconst { ty, val, .. } => {
                    let v = target.iconst(ty.clone(), *val);
                    push_val!(v);
                }

                LirCall::Add { lhs, rhs, .. } => {
                    let v = target.add(val!(lhs), val!(rhs));
                    push_val!(v);
                }
                LirCall::Sub { lhs, rhs, .. } => {
                    let v = target.sub(val!(lhs), val!(rhs));
                    push_val!(v);
                }
                LirCall::Mul { lhs, rhs, .. } => {
                    let v = target.mul(val!(lhs), val!(rhs));
                    push_val!(v);
                }
                LirCall::Udiv { lhs, rhs, .. } => {
                    let v = target.udiv(val!(lhs), val!(rhs));
                    push_val!(v);
                }
                LirCall::Sdiv { lhs, rhs, .. } => {
                    let v = target.sdiv(val!(lhs), val!(rhs));
                    push_val!(v);
                }

                LirCall::And { lhs, rhs, .. } => {
                    let v = target.and(val!(lhs), val!(rhs));
                    push_val!(v);
                }
                LirCall::Or { lhs, rhs, .. } => {
                    let v = target.or(val!(lhs), val!(rhs));
                    push_val!(v);
                }
                LirCall::Xor { lhs, rhs, .. } => {
                    let v = target.xor(val!(lhs), val!(rhs));
                    push_val!(v);
                }
                LirCall::Not { val, .. } => {
                    let v = target.not(val!(val));
                    push_val!(v);
                }
                LirCall::Shl { val, shift, .. } => {
                    let v = target.shl(val!(val), val!(shift));
                    push_val!(v);
                }
                LirCall::Lshr { val, shift, .. } => {
                    let v = target.lshr(val!(val), val!(shift));
                    push_val!(v);
                }
                LirCall::Ashr { val, shift, .. } => {
                    let v = target.ashr(val!(val), val!(shift));
                    push_val!(v);
                }

                LirCall::Icmp { pred, lhs, rhs, .. } => {
                    let v = target.icmp(*pred, val!(lhs), val!(rhs));
                    push_val!(v);
                }

                LirCall::Zext { val, dst_ty, .. } => {
                    let v = target.zext(val!(val), dst_ty.clone());
                    push_val!(v);
                }
                LirCall::Sext { val, dst_ty, .. } => {
                    let v = target.sext(val!(val), dst_ty.clone());
                    push_val!(v);
                }
                LirCall::Trunc { val, dst_ty, .. } => {
                    let v = target.trunc(val!(val), dst_ty.clone());
                    push_val!(v);
                }

                LirCall::Select { cond, then_val, else_val, .. } => {
                    let v = target.select(val!(cond), val!(then_val), val!(else_val));
                    push_val!(v);
                }

                LirCall::Jump { target: tgt, args } => {
                    let real_args: Vec<T::Value> = args.iter().map(|a| val!(a)).collect();
                    target.jump(block!(tgt), &real_args);
                }

                LirCall::Branch { cond, then_block, then_args, else_block, else_args } => {
                    let real_then: Vec<T::Value> = then_args.iter().map(|a| val!(a)).collect();
                    let real_else: Vec<T::Value> = else_args.iter().map(|a| val!(a)).collect();
                    target.branch(val!(cond), block!(then_block), &real_then, block!(else_block), &real_else);
                }

                LirCall::Ret { vals: ret_vals } => {
                    let real: Vec<T::Value> = ret_vals.iter().map(|v| val!(v)).collect();
                    target.ret(&real);
                }

                LirCall::CallExtern { name, arg_tys, args, ret_ty, .. } => {
                    let real_args: Vec<T::Value> = args.iter().map(|a| val!(a)).collect();
                    let outs = target.call_extern(name, arg_tys, &real_args, ret_ty.clone());
                    for v in outs {
                        push_val!(v);
                    }
                }

                LirCall::Oracle { name, arg_tys, args, ret_tys, .. } => {
                    let real_args: Vec<T::Value> = args.iter().map(|a| val!(a)).collect();
                    let outs = target.oracle(name, arg_tys, &real_args, ret_tys);
                    for v in outs {
                        push_val!(v);
                    }
                }

                LirCall::Action { name, guard, arg_tys, args, fallbacks, ret_tys, .. } => {
                    let real_args: Vec<T::Value> = args.iter().map(|a| val!(a)).collect();
                    let real_fallbacks: Vec<T::Value> = fallbacks.iter().map(|f| val!(f)).collect();
                    let outs = target.action(name, val!(guard), arg_tys, &real_args, &real_fallbacks, ret_tys);
                    for v in outs {
                        push_val!(v);
                    }
                }

                LirCall::Rng { ty, .. } => {
                    let v = target.rng(ty.clone());
                    push_val!(v);
                }

                LirCall::Alloca { elem_ty, count, .. } => {
                    let ext = target
                        .stack_alloc_ext()
                        .expect("SavedLirModule::replay: Alloca call requires StackAllocExt, but target returned None");
                    let v = ext.alloca(elem_ty.clone(), *count);
                    push_val!(v);
                }
                LirCall::PtrLoad { ptr, ty, .. } => {
                    let ext = target
                        .stack_alloc_ext()
                        .expect("SavedLirModule::replay: PtrLoad requires StackAllocExt");
                    let v = ext.ptr_load(val!(ptr), ty.clone());
                    push_val!(v);
                }
                LirCall::PtrStore { ptr, val } => {
                    let ext = target
                        .stack_alloc_ext()
                        .expect("SavedLirModule::replay: PtrStore requires StackAllocExt");
                    ext.ptr_store(val!(ptr), val!(val));
                }
                LirCall::PtrOffset { ptr, idx, .. } => {
                    let ext = target
                        .stack_alloc_ext()
                        .expect("SavedLirModule::replay: PtrOffset requires StackAllocExt");
                    let v = ext.ptr_offset(val!(ptr), val!(idx));
                    push_val!(v);
                }

                LirCall::PtrIndexLoad { ptr, idx, pointee_ty, .. } => {
                    let outs = target.ptr_index_load(val!(ptr), val!(idx), pointee_ty);
                    for v in outs {
                        push_val!(v);
                    }
                }
                LirCall::PtrIndexStore { ptr, idx, vals: store_vals, pointee_ty } => {
                    let real: Vec<T::Value> = store_vals.iter().map(|v| val!(v)).collect();
                    target.ptr_index_store(val!(ptr), val!(idx), &real, pointee_ty);
                }

                // These are metadata-only; no downstream target call needed.
                LirCall::ValueType { .. } | LirCall::SetProv => {}
            }
        }
    }
}

// ============================================================================
// RecordingTarget
// ============================================================================

/// Implements [`LirTarget`] by recording all calls into a [`SavedLirModule`].
///
/// Also implements [`StackAllocExt`] so that stack-allocation calls are
/// captured too.
pub struct RecordingTarget {
    pub module: SavedLirModule,
    next_val: u32,
    next_block: u32,
    /// Type map: `val_types[i]` = `LirType` of value index `i`.
    val_types: Vec<LirType>,
}

impl RecordingTarget {
    pub fn new() -> Self {
        RecordingTarget {
            module: SavedLirModule::default(),
            next_val: 0,
            next_block: 0,
            val_types: Vec::new(),
        }
    }

    fn alloc_val(&mut self, ty: LirType) -> u32 {
        let idx = self.next_val;
        self.next_val += 1;
        self.val_types.push(ty.clone());
        self.module.calls.push(LirCall::ValueType { val: idx, ty });
        idx
    }

    fn alloc_block(&mut self) -> u32 {
        let idx = self.next_block;
        self.next_block += 1;
        idx
    }

    /// Consume the recorder and return the finished [`SavedLirModule`].
    pub fn finish(self) -> SavedLirModule {
        self.module
    }
}

impl Default for RecordingTarget {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// LirTarget impl for RecordingTarget
// ============================================================================

impl LirTarget for RecordingTarget {
    type Value = u32;
    type Block = u32;

    fn define_struct(&mut self, def: StructDef) -> StructId {
        let id = self.module.calls.iter().filter(|c| matches!(c, LirCall::DefineStruct { .. })).count() as StructId;
        self.module.calls.push(LirCall::DefineStruct { def, id });
        id
    }

    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (u32, Vec<Vec<u32>>) {
        let entry_block = self.alloc_block();

        // Allocate value indices for each parameter (scalar: one val per param).
        let mut param_vals: Vec<Vec<u32>> = Vec::new();
        for ty in params {
            let v = self.alloc_val(ty.clone());
            param_vals.push(vec![v]);
        }

        self.module.calls.push(LirCall::BeginFunction {
            name: alloc::string::String::from(name),
            params: params.to_vec(),
            ret,
            entry_block,
            param_vals: param_vals.clone(),
        });

        (entry_block, param_vals)
    }

    fn end_function(&mut self) {
        self.module.calls.push(LirCall::EndFunction);
    }

    fn create_block(&mut self) -> u32 {
        let b = self.alloc_block();
        self.module.calls.push(LirCall::CreateBlock { block: b });
        b
    }

    fn add_block_param(&mut self, block: u32, ty: LirType) -> u32 {
        let v = self.alloc_val(ty.clone());
        self.module.calls.push(LirCall::AddBlockParam { block, ty, val: v });
        v
    }

    fn switch_to_block(&mut self, block: u32) {
        self.module.calls.push(LirCall::SwitchToBlock { block });
    }

    fn iconst(&mut self, ty: LirType, val: i64) -> u32 {
        let out = self.alloc_val(ty.clone());
        self.module.calls.push(LirCall::Iconst { ty, val, out });
        out
    }

    fn add(&mut self, lhs: u32, rhs: u32) -> u32 {
        let ty = self.val_types[lhs as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Add { lhs, rhs, out });
        out
    }

    fn sub(&mut self, lhs: u32, rhs: u32) -> u32 {
        let ty = self.val_types[lhs as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Sub { lhs, rhs, out });
        out
    }

    fn mul(&mut self, lhs: u32, rhs: u32) -> u32 {
        let ty = self.val_types[lhs as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Mul { lhs, rhs, out });
        out
    }

    fn udiv(&mut self, lhs: u32, rhs: u32) -> u32 {
        let ty = self.val_types[lhs as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Udiv { lhs, rhs, out });
        out
    }

    fn sdiv(&mut self, lhs: u32, rhs: u32) -> u32 {
        let ty = self.val_types[lhs as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Sdiv { lhs, rhs, out });
        out
    }

    fn and(&mut self, lhs: u32, rhs: u32) -> u32 {
        let ty = self.val_types[lhs as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::And { lhs, rhs, out });
        out
    }

    fn or(&mut self, lhs: u32, rhs: u32) -> u32 {
        let ty = self.val_types[lhs as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Or { lhs, rhs, out });
        out
    }

    fn xor(&mut self, lhs: u32, rhs: u32) -> u32 {
        let ty = self.val_types[lhs as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Xor { lhs, rhs, out });
        out
    }

    fn not(&mut self, val: u32) -> u32 {
        let ty = self.val_types[val as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Not { val, out });
        out
    }

    fn shl(&mut self, val: u32, shift: u32) -> u32 {
        let ty = self.val_types[val as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Shl { val, shift, out });
        out
    }

    fn lshr(&mut self, val: u32, shift: u32) -> u32 {
        let ty = self.val_types[val as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Lshr { val, shift, out });
        out
    }

    fn ashr(&mut self, val: u32, shift: u32) -> u32 {
        let ty = self.val_types[val as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Ashr { val, shift, out });
        out
    }

    fn icmp(&mut self, pred: IcmpPred, lhs: u32, rhs: u32) -> u32 {
        let out = self.alloc_val(LirType::Bool);
        self.module.calls.push(LirCall::Icmp { pred, lhs, rhs, out });
        out
    }

    fn zext(&mut self, val: u32, dst_ty: LirType) -> u32 {
        let out = self.alloc_val(dst_ty.clone());
        self.module.calls.push(LirCall::Zext { val, dst_ty, out });
        out
    }

    fn sext(&mut self, val: u32, dst_ty: LirType) -> u32 {
        let out = self.alloc_val(dst_ty.clone());
        self.module.calls.push(LirCall::Sext { val, dst_ty, out });
        out
    }

    fn trunc(&mut self, val: u32, dst_ty: LirType) -> u32 {
        let out = self.alloc_val(dst_ty.clone());
        self.module.calls.push(LirCall::Trunc { val, dst_ty, out });
        out
    }

    fn select(&mut self, cond: u32, then_val: u32, else_val: u32) -> u32 {
        let ty = self.val_types[then_val as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::Select { cond, then_val, else_val, out });
        out
    }

    fn value_scalar_type(&self, val: &u32) -> LirType {
        self.val_types[*val as usize].clone()
    }

    fn call_extern(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[u32],
        ret_ty: Option<LirType>,
    ) -> Vec<u32> {
        let mut outs = Vec::new();
        if let Some(ty) = &ret_ty {
            outs.push(self.alloc_val(ty.clone()));
        }
        self.module.calls.push(LirCall::CallExtern {
            name: alloc::string::String::from(name),
            arg_tys: arg_tys.to_vec(),
            args: args.to_vec(),
            ret_ty,
            outs: outs.clone(),
        });
        outs
    }

    fn jump(&mut self, target: u32, args: &[u32]) {
        self.module.calls.push(LirCall::Jump { target, args: args.to_vec() });
    }

    fn branch(
        &mut self,
        cond: u32,
        then_block: u32,
        then_args: &[u32],
        else_block: u32,
        else_args: &[u32],
    ) {
        self.module.calls.push(LirCall::Branch {
            cond,
            then_block,
            then_args: then_args.to_vec(),
            else_block,
            else_args: else_args.to_vec(),
        });
    }

    fn ret(&mut self, vals: &[u32]) {
        self.module.calls.push(LirCall::Ret { vals: vals.to_vec() });
    }

    fn oracle(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[u32],
        ret_tys: &[LirType],
    ) -> Vec<u32> {
        let outs: Vec<u32> = ret_tys.iter().map(|ty| self.alloc_val(ty.clone())).collect();
        self.module.calls.push(LirCall::Oracle {
            name: alloc::string::String::from(name),
            arg_tys: arg_tys.to_vec(),
            args: args.to_vec(),
            ret_tys: ret_tys.to_vec(),
            outs: outs.clone(),
        });
        outs
    }

    fn action(
        &mut self,
        name: &str,
        guard: u32,
        arg_tys: &[LirType],
        args: &[u32],
        fallbacks: &[u32],
        ret_tys: &[LirType],
    ) -> Vec<u32> {
        let outs: Vec<u32> = ret_tys.iter().map(|ty| self.alloc_val(ty.clone())).collect();
        self.module.calls.push(LirCall::Action {
            name: alloc::string::String::from(name),
            guard,
            arg_tys: arg_tys.to_vec(),
            args: args.to_vec(),
            fallbacks: fallbacks.to_vec(),
            ret_tys: ret_tys.to_vec(),
            outs: outs.clone(),
        });
        outs
    }

    fn rng(&mut self, ty: LirType) -> u32 {
        let out = self.alloc_val(ty.clone());
        self.module.calls.push(LirCall::Rng { ty, out });
        out
    }

    fn stack_alloc_ext(&mut self) -> Option<&mut dyn StackAllocExt<Value = u32>> {
        Some(self)
    }

    fn abi(&self) -> LirAbi {
        LirAbi::DEFAULT
    }

    fn ptr_index_load(&mut self, ptr: u32, idx: u32, pointee_ty: &LirType) -> Vec<u32> {
        let out = self.alloc_val(pointee_ty.clone());
        let outs = vec![out];
        self.module.calls.push(LirCall::PtrIndexLoad {
            ptr,
            idx,
            pointee_ty: pointee_ty.clone(),
            outs: outs.clone(),
        });
        outs
    }

    fn ptr_index_store(&mut self, ptr: u32, idx: u32, vals: &[u32], pointee_ty: &LirType) {
        self.module.calls.push(LirCall::PtrIndexStore {
            ptr,
            idx,
            vals: vals.to_vec(),
            pointee_ty: pointee_ty.clone(),
        });
    }
}

// ============================================================================
// StackAllocExt impl for RecordingTarget
// ============================================================================

impl StackAllocExt for RecordingTarget {
    type Value = u32;

    fn alloca(&mut self, elem_ty: LirType, count: usize) -> u32 {
        let out = self.alloc_val(LirType::Ptr(alloc::boxed::Box::new(elem_ty.clone())));
        self.module.calls.push(LirCall::Alloca { elem_ty, count, out });
        out
    }

    fn ptr_load(&mut self, ptr: u32, ty: LirType) -> u32 {
        let out = self.alloc_val(ty.clone());
        self.module.calls.push(LirCall::PtrLoad { ptr, ty, out });
        out
    }

    fn ptr_store(&mut self, ptr: u32, val: u32) {
        self.module.calls.push(LirCall::PtrStore { ptr, val });
    }

    fn ptr_offset(&mut self, ptr: u32, idx: u32) -> u32 {
        let ty = self.val_types[ptr as usize].clone();
        let out = self.alloc_val(ty);
        self.module.calls.push(LirCall::PtrOffset { ptr, idx, out });
        out
    }
}
