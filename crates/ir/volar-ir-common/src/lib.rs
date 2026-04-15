#![no_std]
// @reliability: normal
// @ai: none

extern crate alloc;

use alloc::{collections::btree_map::BTreeMap, vec::Vec};

/// Primitive (non-compound) types shared across Volar IR and VAFFLE.
///
/// Compound types (`Vec`, `Tuple`, `Block`, `Func`) are expressed by
/// [`IrType`] and referenced via [`TypeId`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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
// Shared statement type
// ============================================================================

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
        ty: TypeId,
        addr: Addr,
    },
    /// Write `src` to the storage location addressed by `addr`.
    StorageWrite {
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
    /// GF(2) multivariate polynomial over variables.
    ///
    /// Each `(monomial, coeff)` contributes `coeff * product(monomial)` to
    /// the sum; `constant` is the degree-0 term.  Monomial keys must be
    /// sorted.  All arithmetic is mod 2.
    Poly {
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
}
