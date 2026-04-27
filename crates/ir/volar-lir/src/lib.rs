// @reliability: normal
// @ai: assisted
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

use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use volar_ir_common::Type as NativeType;

pub mod circuits;
pub use circuits::{BitCircuitBuilder, StorageEmitter, StackPtr, FrameLayout, PACK_W, n_packs, pack_bits, unpack_words};

// ============================================================================
// Name configuration
// ============================================================================

/// Controls how a backend maps logical names to emitted names.
///
/// Applied to every name a backend **defines or calls**:
/// - Functions registered via [`LirTarget::begin_function`]
/// - External functions declared via [`LirTarget::call_extern`]
/// - Oracle calls (`oracle_<name>`) and action calls (`action_<name>`)
///
/// The backend's `rng_fn` field (where present) is **not** subject to
/// `NameConfig`; it is always used verbatim as an absolute name.
///
/// # Resolution order
/// 1. If `remap` contains the original (un-prefixed) `name`, return `remap[name]`.
/// 2. Otherwise, prepend `prefix` to `name` and return the result.
///
/// An empty `prefix` and empty `remap` (the default) is the identity.
#[derive(Clone, Debug, Default)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct NameConfig {
    /// Prefix prepended to all names not found in `remap`.
    pub prefix: String,
    /// Per-name overrides.  Keys are the original (un-prefixed) names; values
    /// are used verbatim (no prefix is applied to them).
    pub remap: BTreeMap<String, String>,
}

impl NameConfig {
    /// Apply this configuration to `name`.
    pub fn apply(&self, name: &str) -> String {
        if let Some(mapped) = self.remap.get(name) {
            return mapped.clone();
        }
        if self.prefix.is_empty() {
            String::from(name)
        } else {
            alloc::format!("{}{}", self.prefix, name)
        }
    }
}

// ============================================================================
// Types
// ============================================================================

/// Integer/boolean/aggregate types supported by LIR.
///
/// Note: `Clone`, not `Copy` — `Arr` boxes its element type.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[cfg_attr(feature = "rkyv", rkyv(serialize_bounds(
    __S: rkyv::ser::Writer + rkyv::ser::Allocator,
    __S::Error: rkyv::rancor::Source,
)))]
#[cfg_attr(feature = "rkyv", rkyv(bytecheck(bounds(__C: rkyv::validation::ArchiveContext))))]
#[cfg_attr(feature = "rkyv", rkyv(deserialize_bounds(__D::Error: rkyv::rancor::Source)))]
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
    Arr(#[cfg_attr(feature = "rkyv", rkyv(omit_bounds))] Box<LirType>, usize),
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
    /// A typed pointer to a value of the inner type.
    ///
    /// Only meaningful in backends that return `Some` from
    /// [`LirTarget::stack_alloc_ext`] (e.g. `CBackend`).  Circuit backends
    /// (`VolarIrTarget`, `VaffleTarget`) have no memory model and will panic
    /// if they encounter a `Ptr` type.
    Ptr(#[cfg_attr(feature = "rkyv", rkyv(omit_bounds))] Box<LirType>),
}

impl LirType {
    /// Bit width for scalar types. Panics on `Arr`/`Struct`/`Ptr`.
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
            LirType::Ptr(_) => panic!("bit_width not meaningful for Ptr (target-dependent size)"),
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
// ABI policy
// ============================================================================

/// ABI policy for a [`LirTarget`] backend.
///
/// Controls how values are passed between functions — inline vs. by-pointer,
/// aggregate packing thresholds, and whether the backend supports native
/// aggregate passing (structs/arrays as single C values).
///
/// Each [`LirTarget`] implementation returns its preferred ABI via
/// [`LirTarget::abi`].  The compiler codegen layer
/// (`volar-lir-codegen`) queries this to make passing-convention decisions.
///
/// # Variants
///
/// | Constant       | Backend          | Aggregates           | Pointer passing  |
/// |----------------|------------------|----------------------|------------------|
/// | `CIRCUIT`      | VolarIrTarget, VaffleTarget | Flattened to bits | N/A          |
/// | `C_NATIVE`     | CBackend         | Native C structs     | Above threshold  |
/// | `DEFAULT`      | Fallback         | Flattened to scalars | Disabled         |
#[derive(Clone, Debug)]
pub struct LirAbi {
    /// Maximum number of flat scalars to pass inline (by value) at a call
    /// site.  Aggregates whose [`flatten_count`] exceeds this are passed
    /// via `StackAllocExt` (alloca + pointer) when the backend supports it.
    ///
    /// Set to `usize::MAX` to disable pointer-passing entirely.
    pub aggregate_byval_limit: usize,

    /// Whether the backend can accept and return aggregate types directly
    /// in its native calling convention (e.g. C passes `struct` by value).
    ///
    /// When `true`, the codegen layer may skip flattening for types below
    /// the `aggregate_byval_limit` and rely on the backend's own
    /// pack/unpack logic (see `CBackend::pack_scalars`).
    pub native_aggregates: bool,
}

impl LirAbi {
    /// ABI for circuit backends (`VolarIrTarget`, `VaffleTarget`).
    ///
    /// Everything is decomposed to GF(2) bits; there is no concept of
    /// aggregate passing or pointer indirection.  `aggregate_byval_limit`
    /// is `usize::MAX` so the codegen layer never attempts pointer-passing.
    pub const CIRCUIT: LirAbi = LirAbi {
        aggregate_byval_limit: usize::MAX,
        native_aggregates: false,
    };

    /// ABI for the C backend.
    ///
    /// Aggregates up to 64 flat scalars are passed by value as C structs.
    /// Larger aggregates are passed by pointer via `StackAllocExt` when
    /// available.
    pub const C_NATIVE: LirAbi = LirAbi {
        aggregate_byval_limit: 64,
        native_aggregates: true,
    };

    /// ABI for the VAFFLE target with stack-based aggregate passing.
    ///
    /// Parameters whose bit-decomposition exceeds 64 bits are passed via
    /// `StorageId::STACK`: the caller writes the bits to a stack slot and
    /// passes a 32-bit address; the callee reads the bits back.  This
    /// dramatically reduces block-parameter counts (and therefore
    /// spill/reload cost in the `lower_to_ir` pass) for functions that
    /// accept AES-sized or larger aggregates.
    ///
    /// Enable via [`VaffleTarget::with_optimized_abi`].
    pub const VAFFLE_OPTIMIZED: LirAbi = LirAbi {
        aggregate_byval_limit: 64,
        native_aggregates: false,
    };

    /// Default ABI: unlimited inline passing, no native aggregates.
    pub const DEFAULT: LirAbi = LirAbi {
        aggregate_byval_limit: usize::MAX,
        native_aggregates: false,
    };

    /// Whether `scalar_count` exceeds the inline-passing limit.
    ///
    /// Returns `true` when the aggregate should be passed by pointer
    /// (assuming the backend has `StackAllocExt` support).
    #[inline]
    pub fn pass_by_ptr(&self, scalar_count: usize) -> bool {
        scalar_count > self.aggregate_byval_limit
    }
}

// ============================================================================
// Stack allocation extension trait
// ============================================================================

/// Extension trait for backends that support stack allocation and pointer
/// operations.
///
/// Access via [`LirTarget::stack_alloc_ext`], which returns `None` for
/// backends without a memory model (e.g. `VolarIrTarget`, `VaffleTarget`).
/// `CBackend` returns `Some(self)`.
///
/// # Pointer type
///
/// `alloca` returns a [`LirType::Ptr`] value.  Pointer arithmetic and
/// dereferencing use element indices, not byte offsets, so callers do not need
/// to know the element size.
pub trait StackAllocExt {
    type Value: Clone + Eq + core::fmt::Debug;

    /// Allocate a stack region for `count` elements of `elem_ty`.
    ///
    /// Returns a value of type `LirType::Ptr(Box::new(elem_ty))`.
    /// The region is live for the duration of the enclosing function.
    fn alloca(&mut self, elem_ty: LirType, count: usize) -> Self::Value;

    /// Load through a typed pointer. `ty` must match the pointee type.
    fn ptr_load(&mut self, ptr: Self::Value, ty: LirType) -> Self::Value;

    /// Store `val` through `ptr`. Returns `()` — emitted as a bare statement.
    fn ptr_store(&mut self, ptr: Self::Value, val: Self::Value);

    /// Element-wise pointer offset: `ptr + idx` elements (not bytes).
    ///
    /// Returns a pointer of the same type as `ptr`.
    fn ptr_offset(&mut self, ptr: Self::Value, idx: Self::Value) -> Self::Value;
}

// ============================================================================
// Struct definitions
// ============================================================================

pub type StructId = u32;

/// One field in a struct definition.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct FieldDef {
    pub name: String,
    pub ty: LirType,
}

/// A named struct with an ordered list of fields.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
}

// ============================================================================
// Comparison predicates
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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

    /// Return a mutable reference to the [`StackAllocExt`] implementation for
    /// this backend, if it supports stack allocation and pointer operations.
    ///
    /// Circuit backends (`VolarIrTarget`, `VaffleTarget`) return `None`.
    /// `CBackend` returns `Some(self)`.
    ///
    /// Callers should check for `Some` before using pointer-based patterns
    /// (e.g. passing large structs by pointer in the C ABI).
    fn stack_alloc_ext(&mut self) -> Option<&mut dyn StackAllocExt<Value = Self::Value>> {
        None
    }

    /// Return the ABI policy for this target.
    ///
    /// The compiler codegen layer (`volar-lir-codegen`) queries this to
    /// decide aggregate passing conventions, pointer-passing thresholds,
    /// and other target-specific calling convention details.
    ///
    /// The default returns [`LirAbi::DEFAULT`].  Override in backends with
    /// specialised ABIs (e.g. `CBackend` returns [`LirAbi::C_NATIVE`]).
    fn abi(&self) -> LirAbi {
        LirAbi::DEFAULT
    }

    // ---- Pointer-indexed memory operations ----------------------------------

    /// Load an element from a pointer at the given index, returning flat
    /// scalar values.
    ///
    /// Semantics: `ptr[idx]` — read the element at offset `idx` and
    /// decompose the aggregate result into flat scalar values.
    ///
    /// Only backends with a memory model (e.g. `CBackend`) need to implement
    /// this.  Circuit backends can leave the default, which panics.
    fn ptr_index_load(
        &mut self,
        _ptr: Self::Value,
        _idx: Self::Value,
        _pointee_ty: &LirType,
    ) -> Vec<Self::Value> {
        unimplemented!("ptr_index_load: not supported by this backend")
    }

    /// Store flat scalar values to a pointer at the given index.
    ///
    /// Semantics: `ptr[idx] = pack(vals)` — pack the flat scalars into an
    /// aggregate and write it at offset `idx`.
    ///
    /// Only backends with a memory model need to implement this.
    fn ptr_index_store(
        &mut self,
        _ptr: Self::Value,
        _idx: Self::Value,
        _vals: &[Self::Value],
        _pointee_ty: &LirType,
    ) {
        unimplemented!("ptr_index_store: not supported by this backend")
    }
}
