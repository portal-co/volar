// @reliability: normal
//! `LirTarget` — low-level IR builder trait with block parameters.
//!
//! Backends receive a stream of builder calls and produce whatever output
//! format they target (C text, machine code, etc.).
//!
//! # Block-parameter SSA
//!
//! Control-flow join points are block parameters (Cranelift / MLIR style).
//! A phi-node `%v = phi [%a, bb0], [%b, bb1]` becomes: block `bb_join(p0)`
//! with `jump bb_join(%a)` from `bb0` and `jump bb_join(%b)` from `bb1`.
//!
//! # Types
//!
//! `LirType` is `Clone` (not `Copy`) because `Arr` contains a boxed element
//! type. Use `.clone()` when you need multiple copies.

#![no_std]
extern crate alloc;

use alloc::{boxed::Box, string::String, vec::Vec};

// ============================================================================
// Types
// ============================================================================

/// Integer/boolean/aggregate types supported by LIR.
///
/// Note: `Clone`, not `Copy` — `Arr` boxes its element type.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum LirType {
    // ---- Scalars ------------------------------------------------------------
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    // ---- Aggregates ---------------------------------------------------------
    /// Fixed-size homogeneous array: `[elem; len]`.
    Arr(Box<LirType>, usize),
    /// Named struct registered via `LirTarget::define_struct`.
    Struct(StructId),
}

impl LirType {
    /// Bit width for scalar types. Panics on `Arr`/`Struct`.
    pub fn bit_width(&self) -> u32 {
        match self {
            LirType::Bool => 1,
            LirType::I8 | LirType::U8 => 8,
            LirType::I16 | LirType::U16 => 16,
            LirType::I32 | LirType::U32 => 32,
            LirType::I64 | LirType::U64 => 64,
            LirType::Arr(elem, len) => elem.bit_width() * (*len as u32),
            LirType::Struct(_) => panic!("bit_width not defined for Struct"),
        }
    }

    /// Whether this scalar type is signed. Panics on aggregates.
    pub fn is_signed(&self) -> bool {
        matches!(self, LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64)
    }

    /// Returns true if this is a scalar (non-aggregate) type.
    pub fn is_scalar(&self) -> bool {
        !matches!(self, LirType::Arr(_, _) | LirType::Struct(_))
    }
}

// ============================================================================
// Struct definitions
// ============================================================================

pub type StructId = u32;

/// One field in a struct definition.
#[derive(Clone, Debug)]
pub struct FieldDef {
    pub name: String,
    pub ty: LirType,
}

/// A named struct with an ordered list of fields.
#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
}

// ============================================================================
// Comparison predicates
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum IcmpPred {
    Eq,
    Ne,
    Ult,
    Ule,
    Ugt,
    Uge,
    Slt,
    Sle,
    Sgt,
    Sge,
}

// ============================================================================
// The trait
// ============================================================================

/// Builder trait for a low-level SSA IR with block parameters.
///
/// All methods take `&mut self`. The caller must maintain well-formedness:
/// - Call `switch_to_block` before emitting instructions or a terminator.
/// - Every block must end with exactly one terminator.
/// - Call `end_function` after the last terminator.
pub trait LirTarget {
    type Value: Copy + Eq + core::fmt::Debug;
    type Block: Copy + Eq + core::fmt::Debug;

    // ---- Type registration --------------------------------------------------

    /// Register a struct definition. Must be called before any use of the
    /// returned `StructId` in `LirType::Struct(id)` or `struct_new`.
    /// Can be called at any point (before or during functions).
    fn define_struct(&mut self, def: StructDef) -> StructId;

    // ---- Function management ------------------------------------------------

    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (Self::Block, Vec<Self::Value>);

    fn end_function(&mut self);

    // ---- Block management ---------------------------------------------------

    fn create_block(&mut self) -> Self::Block;
    fn add_block_param(&mut self, block: Self::Block, ty: LirType) -> Self::Value;
    fn switch_to_block(&mut self, block: Self::Block);

    // ---- Constants ----------------------------------------------------------

    fn iconst(&mut self, ty: LirType, val: i64) -> Self::Value;

    // ---- Arithmetic ---------------------------------------------------------

    fn add(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    fn sub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    fn mul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    fn udiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    fn sdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;

    // ---- Bitwise ------------------------------------------------------------

    fn and(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    fn or(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    fn xor(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    fn not(&mut self, val: Self::Value) -> Self::Value;
    fn shl(&mut self, val: Self::Value, shift: Self::Value) -> Self::Value;
    fn lshr(&mut self, val: Self::Value, shift: Self::Value) -> Self::Value;
    fn ashr(&mut self, val: Self::Value, shift: Self::Value) -> Self::Value;

    // ---- Comparison ---------------------------------------------------------

    /// Integer compare — result type is always `LirType::Bool`.
    fn icmp(&mut self, pred: IcmpPred, lhs: Self::Value, rhs: Self::Value) -> Self::Value;

    // ---- Conversions --------------------------------------------------------

    fn zext(&mut self, val: Self::Value, dst_ty: LirType) -> Self::Value;
    fn sext(&mut self, val: Self::Value, dst_ty: LirType) -> Self::Value;
    fn trunc(&mut self, val: Self::Value, dst_ty: LirType) -> Self::Value;

    // ---- Select -------------------------------------------------------------

    /// `cond ? then_val : else_val`. `cond` must be `LirType::Bool`.
    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value;

    // ---- Array operations ---------------------------------------------------

    /// Construct a fixed-size array from elements (all same type).
    fn arr_new(&mut self, elem_ty: LirType, elems: &[Self::Value]) -> Self::Value;

    /// Load one element at a runtime index; returns the element value.
    fn arr_get(&mut self, arr: Self::Value, idx: Self::Value) -> Self::Value;

    /// Return a new array with the element at `idx` replaced by `val` (functional update).
    fn arr_set(&mut self, arr: Self::Value, idx: Self::Value, val: Self::Value) -> Self::Value;

    // ---- Struct operations --------------------------------------------------

    /// Construct a struct from field values in declaration order.
    fn struct_new(&mut self, id: StructId, fields: &[Self::Value]) -> Self::Value;

    /// Extract one field by its declaration-order index.
    fn struct_get(&mut self, val: Self::Value, field_idx: usize) -> Self::Value;

    // ---- Extern calls -------------------------------------------------------

    /// Call an external function by name.
    ///
    /// Returns `Some(value)` if `ret_ty` is `Some`, `None` for void calls.
    fn call_extern(
        &mut self,
        name: &str,
        ret_ty: Option<LirType>,
        args: &[Self::Value],
    ) -> Option<Self::Value>;

    // ---- Terminators --------------------------------------------------------

    fn jump(&mut self, target: Self::Block, args: &[Self::Value]);

    fn branch(
        &mut self,
        cond: Self::Value,
        then_block: Self::Block,
        then_args: &[Self::Value],
        else_block: Self::Block,
        else_args: &[Self::Value],
    );

    fn ret(&mut self, val: Option<Self::Value>);
}
