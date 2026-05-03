#![no_std]
// @reliability: normal
// @ai: assisted

extern crate alloc;

use alloc::{collections::btree_map::BTreeMap, vec::Vec};

/// Primitive (non-compound) types shared across Volar IR and VAFFLE.
///
/// Compound types (`Vec`, `Tuple`, `Block`, `Func`) are expressed by
/// [`IrType`] and referenced via [`TypeId`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum Type {
    /// Single GF(2) element (one bit).
    Bit,
    /// 8-bit integer / byte.
    _8,
    /// 16-bit integer.
    _16,
    /// 32-bit integer.
    _32,
    /// 64-bit integer.
    _64,
    /// 128-bit integer.
    _128,
    /// 256-bit value (e.g. full AES lane).
    _256,
    /// GF(2^8) element via the AES polynomial.
    AES8,
    /// GF(2^64) field element.
    Galois64,
}

/// A 256-bit compile-time constant, split into high and low 128-bit halves.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct Constant {
    pub hi: u128,
    pub lo: u128,
}

// ============================================================================
// Unified type system
// ============================================================================

/// An opaque index into a [`TypeTable`].
///
/// Both Volar IR (`IRTypeId`) and VAFFLE use this; the former is now just a
/// re-export alias in `volar-ir`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct TypeId(pub u32);

/// The full IR type language, shared between Volar IR and VAFFLE.
///
/// Primitive scalars are wrapped in [`Primitive`](IrType::Primitive) so that
/// the [`Type`] enum remains the single source of truth for leaf types.
/// Compound types are recursive via [`TypeId`] references into a [`TypeTable`].
///
/// # Variants unique to VAFFLE
/// [`Func`](IrType::Func) represents a first-class function type (used for
/// imports, exports, and higher-order values in VAFFLE modules).
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IrType {
    /// A primitive scalar type (bit, integer, or Galois-field element).
    Primitive(Type),
    /// A fixed-length homogeneous vector: `[element; len]`.
    Vec(usize, TypeId),
    /// A heterogeneous product type (tuple).
    Tuple(alloc::vec::Vec<TypeId>),
    /// A block / continuation type: a control-flow label that accepts the
    /// listed parameter types.  Used by Volar IR's `Block`-typed SSA params
    /// and dynamic jump targets.
    Block {
        params: alloc::vec::Vec<TypeId>,
    },
    /// A function type: a callable with the given parameter and result types.
    /// Present in VAFFLE for import/export declarations and first-class
    /// function values.  Not used by Volar IR (which represents functions via
    /// `IRBlocks` rather than typed values).
    Func {
        params: alloc::vec::Vec<TypeId>,
        results: alloc::vec::Vec<TypeId>,
    },
}

/// An interning table for [`IrType`] values.
///
/// Both Volar IR and VAFFLE carry one of these (Volar IR as `IRTypes`, VAFFLE
/// as `Module::types`).  [`TypeId`]s are valid only within the table that
/// produced them.
///
/// # Deduplication
/// [`intern`](TypeTable::intern) does a linear scan for an existing entry
/// before pushing.  Type tables are typically small (tens of entries), so
/// this is acceptable; for large tables consider a separate index.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct TypeTable(pub alloc::vec::Vec<IrType>);

impl TypeTable {
    /// Create an empty table.
    pub fn new() -> Self {
        TypeTable(alloc::vec::Vec::new())
    }

    /// Push `ty` without deduplication and return its new [`TypeId`].
    pub fn push(&mut self, ty: IrType) -> TypeId {
        let id = TypeId(self.0.len() as u32);
        self.0.push(ty);
        id
    }

    /// Look for an existing entry equal to `ty`; if not found, push it.
    /// Returns the [`TypeId`] of the (found or newly inserted) entry.
    pub fn intern(&mut self, ty: IrType) -> TypeId {
        if let Some(pos) = self.0.iter().position(|t| t == &ty) {
            TypeId(pos as u32)
        } else {
            self.push(ty)
        }
    }

    /// Convenience: intern `IrType::Primitive(ty)`.
    pub fn primitive(&mut self, ty: Type) -> TypeId {
        self.intern(IrType::Primitive(ty))
    }

    /// Convenience: intern the `Bit` primitive type.
    pub fn bit(&mut self) -> TypeId {
        self.primitive(Type::Bit)
    }

    /// Return `true` if `id` resolves to `IrType::Primitive(Type::Bit)`.
    pub fn is_bit(&self, id: TypeId) -> bool {
        matches!(self.0.get(id.0 as usize), Some(IrType::Primitive(Type::Bit)))
    }

    /// Return `true` if `id` resolves to `IrType::Block { .. }`.
    pub fn is_block(&self, id: TypeId) -> bool {
        matches!(self.0.get(id.0 as usize), Some(IrType::Block { .. }))
    }
}

impl Default for TypeTable {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// External-primitive declarations (shared by Volar IR and VAFFLE)
// ============================================================================

/// Declaration of a named pure oracle.
///
/// An oracle is a deterministic external function evaluated by all parties.
/// Its implementation is provided by the execution environment at protocol time.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct OracleDecl {
    pub name: alloc::string::String,
    /// Parameter types in order.
    pub params: alloc::vec::Vec<TypeId>,
    /// Return types in order (length ≥ 1).
    pub results: alloc::vec::Vec<TypeId>,
}

/// Declaration of a named conditional action.
///
/// An action is a side-effectful external function invoked by one party
/// (prover / evaluator) only when a boolean guard is 1.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct ActionDecl {
    pub name: alloc::string::String,
    /// Parameter types in order.
    pub params: alloc::vec::Vec<TypeId>,
    /// Return types in order (length ≥ 1).
    pub results: alloc::vec::Vec<TypeId>,
}

/// Declaration of a named RNG source.
///
/// An RNG source is a zero-argument external function that produces a fresh
/// random value of `ty` on each call.  Unlike `rand`, it is modeled as a named
/// external primitive so that the compiler can emit a call to a named function
/// rather than depending on any specific RNG crate.
///
/// Each [`Stmt::Rng`] references an `RngDecl` by name.  The execution
/// environment supplies the concrete implementation.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct RngDecl {
    pub name: alloc::string::String,
    /// Type of the fresh random value produced on each call.
    pub ty: TypeId,
}

// ============================================================================
// Shared statement type
// ============================================================================

/// Identifies one of potentially many independent storage spaces.
///
/// `StorageId(0)` is the "default" storage.  Higher IDs may be used for
/// separate stacks, heaps, or per-type scratch spaces.  The execution
/// environment maps each `StorageId` to a concrete address space.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct StorageId(pub u32);

impl StorageId {
    /// The default / "main" storage space.
    pub const DEFAULT: StorageId = StorageId(0);
    /// A dedicated call-stack storage space.
    pub const STACK: StorageId = StorageId(1);
    /// Storage space for the virtualisation pass bytecode table (see
    /// `volar-ir-virt`).  Chosen to be outside the WASM memory range so
    /// virtualised modules can still reference any `memory(i)`.
    pub const VIRT_BYTECODE: StorageId = StorageId(2);
    /// Base ID for WASM linear memories.  Memory `i` uses `StorageId(MEMORY_BASE + i)`.
    pub const MEMORY_BASE: u32 = 16;
    /// Convenience: StorageId for WASM memory index `i`.
    pub const fn memory(i: u32) -> StorageId { StorageId(Self::MEMORY_BASE + i) }
}

/// Shared computational statement type for Volar IR and VAFFLE.
///
/// Generic over two parameters:
///
/// | Parameter | Volar IR | VAFFLE |
/// |-----------|----------|--------|
/// | `Var`     | `IRVarId` | `ValueId` |
/// | `Addr`    | `IRVarId` (= `Var`) | `ValueId` (= `Var`) |
///
/// All type annotations are expressed as [`TypeId`] references into the
/// module's [`TypeTable`], unifying the type systems of both IRs.
///
/// # Invariants
/// * `Poly.coeffs` — monomial keys must be sorted (no duplicates within a key).
/// * `Shuffle.result_bits` — each `(bit_idx, var)` selects bit `bit_idx`
///   from `var`; together they define every bit of the output, LSB first.
///   Length equals the output bit-width.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Stmt<Var, Addr = Var> {
    /// Load a value from a storage location addressed by `addr`.
    StorageRead {
        storage: StorageId,
        ty: TypeId,
        addr: Addr,
    },
    /// Write `src` to the storage location addressed by `addr`.
    StorageWrite {
        storage: StorageId,
        src: Var,
        ty: TypeId,
        addr: Addr,
    },
    /// A compile-time constant value of type `ty`.
    Const(Constant, TypeId),
    /// Reinterpret `src` (of type `src_ty`) as `dst_ty` without changing bits.
    Transmute {
        src: Var,
        src_ty: TypeId,
        dst_ty: TypeId,
    },
    /// Multivariate polynomial over variables.
    ///
    /// Each `(monomial, coeff)` contributes `coeff * product(monomial)` to
    /// the sum; `constant` is the degree-0 term.  Monomial keys must be
    /// sorted.
    ///
    /// # Type semantics
    /// * If `ty` resolves to `Bit`: all variables must be `Bit`; arithmetic
    ///   is GF(2) (mod 2 on every coefficient bit).
    /// * If `ty` resolves to a bitvector or field element `T`: at most one
    ///   variable across all monomials may have type `T` (the "non-Bit slot");
    ///   all other variables in that monomial must be `Bit` and act as GF(2)
    ///   selectors.  The constant term occupies the lowest `bits(T)` bits of
    ///   `constant`.  Mixing two distinct non-GF(2) field types is prohibited.
    Poly {
        /// Output (and dominant operand) type.
        ty: TypeId,
        coeffs: BTreeMap<Vec<Var>, u8>,
        constant: Constant,
    },
    /// Rotate-left `src` (of type `ty`) by `n` bit positions.
    Rol {
        src: Var,
        ty: TypeId,
        n: usize,
    },
    /// Rotate-right `src` (of type `ty`) by `n` bit positions.
    Ror {
        src: Var,
        ty: TypeId,
        n: usize,
    },
    /// Concatenate `parts` in order (LSB-first) into a wider value of type `ty`.
    Merge {
        parts: Vec<Var>,
        ty: TypeId,
    },
    /// Broadcast a single-bit value across every bit position of type `ty`.
    Splat {
        src: Var,
        ty: TypeId,
    },
    /// Arbitrary bit shuffle: assemble an output from individually selected bits.
    ///
    /// `result_bits[i] = (bit_idx, var)` — bit `i` of the output is taken
    /// from bit `bit_idx` of `var`.  Length equals the output bit-width.
    Shuffle {
        result_bits: Vec<(u8, Var)>,
        ty: TypeId,
    },

    // ---- External access primitives ----------------------------------------

    /// Invoke a named pure oracle, producing a multi-output aggregate result.
    ///
    /// The result type is `IrType::Tuple(output_tys)`, pre-interned as
    /// `result_ty`.  Project individual outputs with [`OracleOutput`].
    ///
    /// **Ordering**: an `OracleCall` is pure and may be reordered or CSE'd
    /// freely.  It may only be DCE'd when every corresponding `OracleOutput`
    /// is also DCE'd.
    OracleCall {
        name: alloc::string::String,
        args: Vec<Var>,
        /// Return type of each output, in declaration order.  Non-empty.
        output_tys: Vec<TypeId>,
        /// Pre-interned `TypeId` of `IrType::Tuple(output_tys)`.
        /// Stored at construction time so type inference never mutates the table.
        result_ty: TypeId,
    },

    /// Project output `idx` from an [`OracleCall`] result var.
    ///
    /// `call` must be the SSA var produced by an `OracleCall` in the same block.
    /// `ty` must equal `oracle_call.output_tys[idx]`.
    OracleOutput {
        call: Var,
        idx: usize,
        ty: TypeId,
    },

    /// Conditionally invoke a named impure action, producing a multi-output aggregate result.
    ///
    /// The result type is `IrType::Tuple(output_tys)`, pre-interned as `result_ty`.
    /// Output `i` is `action(args)[i]` when `guard != 0`, `fallbacks[i]` otherwise.
    /// Project individual outputs with [`ActionOutput`].
    ///
    /// **Ordering**: an `ActionCall` has side effects and must not be
    /// reordered, CSE'd, or DCE'd.  All `ActionOutput` projections from it
    /// are kept alive as long as the call itself is live.
    ActionCall {
        name: alloc::string::String,
        guard: Var,
        args: Vec<Var>,
        /// Fallback vars — one per output (typed as `output_tys[i]`).  Used
        /// when `guard = 0` and the action is not invoked.
        fallbacks: Vec<Var>,
        /// Return type of each output, in declaration order.  Non-empty.
        output_tys: Vec<TypeId>,
        /// Pre-interned `TypeId` of `IrType::Tuple(output_tys)`.
        result_ty: TypeId,
    },

    /// Project output `idx` from an [`ActionCall`] result var.
    ///
    /// `call` must be the SSA var produced by an `ActionCall` in the same block.
    /// `ty` must equal `action_call.output_tys[idx]`.
    ActionOutput {
        call: Var,
        idx: usize,
        ty: TypeId,
    },

    /// Produce a fresh random value drawn uniformly from the type’s domain.
    ///
    /// `name` identifies the [`RngDecl`] in the enclosing [`IRBlocks::rngs`]
    /// that provides this source of randomness.  Each occurrence is an
    /// independent sample.  Optimisers must **not** deduplicate, CSE, or
    /// reorder `Rng` stmts.  An `Rng` may be DCE'd only when its output is
    /// demonstrably unused.
    Rng {
        /// Name of the declared RNG source (matches an [`RngDecl::name`]).
        name: alloc::string::String,
        ty: TypeId,
    },
}
