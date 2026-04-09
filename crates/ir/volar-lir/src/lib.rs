// @reliability: normal
//! `LirTarget` — low-level IR builder trait with block parameters.
//!
//! Backends implementing this trait receive a stream of builder calls and
//! produce whatever output format they target (C text, machine code, etc.).
//!
//! # Block-parameter SSA
//!
//! Control-flow join points are represented by block parameters (like
//! Cranelift, MLIR, Swift SIL) rather than phi-nodes (LLVM). A phi-node
//! `%v = phi [%a, bb0], [%b, bb1]` becomes: block `bb_join(p0)` with
//! `jump bb_join(%a)` from `bb0` and `jump bb_join(%b)` from `bb1`.
//!
//! # Builder sequence
//!
//! ```text
//! let (entry, params) = target.begin_function("add", &[LirType::U32, LirType::U32], Some(LirType::U32));
//! target.switch_to_block(entry);
//! let sum = target.add(params[0], params[1]);
//! target.ret(Some(sum));
//! target.end_function();
//! ```

#![no_std]
extern crate alloc;

use alloc::vec::Vec;

// ============================================================================
// Types
// ============================================================================

/// Integer/boolean types supported by LIR.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum LirType {
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
}

impl LirType {
    /// Bit width of this type.
    pub fn bit_width(self) -> u32 {
        match self {
            LirType::Bool => 1,
            LirType::I8 | LirType::U8 => 8,
            LirType::I16 | LirType::U16 => 16,
            LirType::I32 | LirType::U32 => 32,
            LirType::I64 | LirType::U64 => 64,
        }
    }

    /// Whether this type is signed.
    pub fn is_signed(self) -> bool {
        matches!(self, LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64)
    }
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
/// All methods take `&mut self` so the backend can accumulate state.
/// The caller is responsible for maintaining well-formedness:
/// - Call `switch_to_block` before emitting instructions or a terminator.
/// - Every block must end with exactly one terminator call.
/// - Call `end_function` after the last block's terminator.
pub trait LirTarget {
    /// An SSA value produced by an instruction or a block parameter.
    type Value: Copy + Eq + core::fmt::Debug;

    /// A basic block identifier.
    type Block: Copy + Eq + core::fmt::Debug;

    // ---- Function management ------------------------------------------------

    /// Start a new function.
    ///
    /// Returns the entry block and the `Value`s for each parameter in order.
    /// The caller should call `switch_to_block(entry_block)` before emitting.
    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (Self::Block, Vec<Self::Value>);

    /// Finish the current function. Must be called after the last block's terminator.
    fn end_function(&mut self);

    // ---- Block management ---------------------------------------------------

    /// Create a new (empty) block in the current function.
    ///
    /// Not the insertion point until `switch_to_block` is called.
    fn create_block(&mut self) -> Self::Block;

    /// Add a parameter to `block` and return its `Value`.
    ///
    /// Must be called before any instructions are emitted into `block`.
    fn add_block_param(&mut self, block: Self::Block, ty: LirType) -> Self::Value;

    /// Set `block` as the current insertion point.
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

    /// Zero-extend `val` to `dst_ty`.
    fn zext(&mut self, val: Self::Value, dst_ty: LirType) -> Self::Value;
    /// Sign-extend `val` to `dst_ty`.
    fn sext(&mut self, val: Self::Value, dst_ty: LirType) -> Self::Value;
    /// Truncate `val` to `dst_ty`.
    fn trunc(&mut self, val: Self::Value, dst_ty: LirType) -> Self::Value;

    // ---- Select (ternary mux) -----------------------------------------------

    /// `cond ? then_val : else_val`. `cond` must be `LirType::Bool`.
    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value;

    // ---- Terminators --------------------------------------------------------

    /// Unconditional jump to `target` with `args` bound to its block parameters.
    fn jump(&mut self, target: Self::Block, args: &[Self::Value]);

    /// Conditional branch.
    ///
    /// If `cond` (Bool) is true, jump to `then_block` with `then_args`;
    /// otherwise jump to `else_block` with `else_args`.
    fn branch(
        &mut self,
        cond: Self::Value,
        then_block: Self::Block,
        then_args: &[Self::Value],
        else_block: Self::Block,
        else_args: &[Self::Value],
    );

    /// Return from the current function.
    fn ret(&mut self, val: Option<Self::Value>);
}
