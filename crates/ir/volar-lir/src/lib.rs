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
use volar_ir_common::Type as NativeType;

pub mod circuits;
pub use circuits::{BitCircuitBuilder, StorageEmitter, StackPtr, FrameLayout};

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
    /// An opaque Volar-IR-native field element, treated as a **single** value
    /// in both the LirTarget API and (for `VolarIrTarget`) the resulting IR.
    ///
    /// `VolarIrTarget` represents this as one `IRVarId` whose `IrType` is
    /// `IrType::Primitive(t)` — **not** as N individual `Bit` vars.  Other
    /// backends (e.g. C) can map it to the closest integer type or a custom
    /// field-element struct.
    ///
    /// Arithmetic on `Native` values in `VolarIrTarget` emits `IRStmt::Poly`
    /// with the native type, giving correct GF-field semantics automatically.
    Native(NativeType),
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
            LirType::Native(_) => panic!("bit_width not meaningful for Native field elements"),
        }
    }

    /// Whether this scalar type is signed. Panics on aggregates.
    pub fn is_signed(&self) -> bool {
        matches!(self, LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64)
    }

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
///
/// # Provenance
///
/// The optional `Prov` type parameter allows callers to attach provenance
/// annotations to emitted instructions.  Call [`set_prov`](LirTarget::set_prov)
/// before emitting one or more instructions; each instruction inherits the
/// most recently set provenance.  Backends that do not track provenance use
/// the default `Prov = ()` and the no-op default impl of `set_prov`.
pub trait LirTarget<Prov: Clone + Default = ()> {
    type Value: Clone + Eq + core::fmt::Debug;
    type Block: Clone + Eq + core::fmt::Debug;

    /// Set the provenance context for subsequently emitted instructions.
    ///
    /// Each call overrides the previous value.  Instructions emitted after
    /// this call (and before the next `set_prov`) are tagged with `prov`.
    ///
    /// The default implementation is a no-op — backends that do not track
    /// provenance need not override this.
    fn set_prov(&mut self, _prov: Prov) {}

    // ---- Type registration --------------------------------------------------

    /// Register a struct definition. Must be called before any use of the
    /// returned `StructId` in `LirType::Struct(id)` or `struct_new`.
    /// Can be called at any point (before or during functions).
    fn define_struct(&mut self, def: StructDef) -> StructId;

    // ---- Function management ------------------------------------------------

    /// Begin a new function.
    ///
    /// `params` holds the ABI type for each parameter (may be aggregate).
    /// Returns the entry block and one `Vec<Self::Value>` per parameter — each
    /// inner vec is the flat scalar values that represent that parameter.
    fn begin_function(
        &mut self,
        name: &str,
        params: &[LirType],
        ret: Option<LirType>,
    ) -> (Self::Block, Vec<Vec<Self::Value>>);

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

    // ---- Value type query ---------------------------------------------------

    /// Return the scalar `LirType` of a previously-emitted value.
    ///
    /// Used by the lowering pass to recover types for join-block parameters
    /// (e.g. in `lower_if`) without threading type information through every
    /// `lower_expr` return.  Panics if `val` was not produced by this target.
    fn value_scalar_type(&self, val: &Self::Value) -> LirType;

    // ---- Extern calls -------------------------------------------------------

    /// Call an external function by name.
    ///
    /// `arg_tys` holds the ABI type for each *logical* argument (may be
    /// `Arr`/`Struct`).  `args` is the flat list of scalars whose total count
    /// equals `sum(flatten_count(arg_tys[i]))`.  `ret_ty` is the ABI return
    /// type (may be aggregate).  Returns the flat scalar list for the return
    /// value (empty for void).
    fn call_extern(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[Self::Value],
        ret_ty: Option<LirType>,
    ) -> Vec<Self::Value>;

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

    /// Emit a return.  `vals` is the flat scalar list for the return value
    /// (empty slice for void functions).
    fn ret(&mut self, vals: &[Self::Value]);

    // ---- External access primitives ----------------------------------------

    /// Invoke a named pure oracle, returning all outputs as a flat scalar list.
    ///
    /// `ret_tys` holds the [`LirType`] of each output (length ≥ 1).
    /// Returns the concatenation of `flatten(output_i)` for each `i`.
    /// Callers split using `flatten_count(&ret_tys[i])`.
    fn oracle(
        &mut self,
        name: &str,
        arg_tys: &[LirType],
        args: &[Self::Value],
        ret_tys: &[LirType],
    ) -> Vec<Self::Value>;

    /// Invoke a named conditional action, returning all outputs as a flat
    /// scalar list.
    ///
    /// `guard` must be `LirType::Bool`.  `fallbacks` is the flat concatenation
    /// of all outputs' fallback scalar values.  `ret_tys` holds the [`LirType`]
    /// of each output (length ≥ 1).  The action executes iff `guard = 1`.
    fn action(
        &mut self,
        name: &str,
        guard: Self::Value,
        arg_tys: &[LirType],
        args: &[Self::Value],
        fallbacks: &[Self::Value],
        ret_tys: &[LirType],
    ) -> Vec<Self::Value>;

    /// Generate a fresh random value of `ty`.  Each call is an independent
    /// sample; implementations must not alias results.
    fn rng(&mut self, ty: LirType) -> Self::Value;
}
