#![no_std]
// @reliability: normal
// @ai: none

extern crate alloc;

use alloc::{collections::btree_map::BTreeMap, vec::Vec};

/// Flat value types used by VAFFLE and as the default type annotation
/// for [`Stmt`] when no type-ID indirection is needed.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[non_exhaustive]
pub enum Type {
    _32,
    _64,
    _16,
    _8,
    AES8,
    _128,
    _256,
}

/// A 256-bit compile-time constant split into high and low 128-bit halves.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Constant {
    pub hi: u128,
    pub lo: u128,
}

/// Shared computational statement type for Volar IR and VAFFLE.
///
/// Generic over three parameters so neither IR needs to duplicate the set of
/// operations or duplicate the logic that processes them:
///
/// | Parameter | Volar IR | VAFFLE |
/// |-----------|----------|--------|
/// | `Var`     | `IRVarId` | `ValueId` |
/// | `Addr`    | `IRVarId` (= `Var`) | `ValueId` (= `Var`) |
/// | `Ty`      | `IRTypeId` | [`Type`] |
///
/// `Addr` defaults to `Var` and `Ty` defaults to [`Type`] so that a
/// plain `Stmt<V>` works for VAFFLE and only volar-ir needs to spell out the
/// third parameter.
///
/// # Invariants
/// * `Poly.coeffs` — monomial keys must be sorted (no duplicates within a key).
/// * `Shuffle.result_bits` — each element `(bit_idx, var)` selects bit
///   `bit_idx` from `var`; together they define every bit of the output, LSB
///   first.  The number of elements determines the output bit-width.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Stmt<Var, Addr = Var, Ty = Type> {
    /// Load a value from a storage location addressed by `addr`.
    StorageRead {
        ty: Ty,
        addr: Addr,
    },
    /// Write `src` to the storage location addressed by `addr`.
    /// The result value is a unit/zero placeholder.
    StorageWrite {
        src: Var,
        ty: Ty,
        addr: Addr,
    },
    /// A compile-time constant value of type `ty`.
    Const(Constant, Ty),
    /// Reinterpret `src` (of type `src_ty`) as `dst_ty` without changing bits.
    Transmute {
        src: Var,
        src_ty: Ty,
        dst_ty: Ty,
    },
    /// GF(2) multivariate polynomial over variables.
    ///
    /// Each `(monomial, coeff)` entry contributes `coeff * product(monomial)`
    /// to the sum; `constant` is the degree-0 term.  All arithmetic is mod 2
    /// (only the low bit of each coefficient and of `constant.lo` is used in a
    /// pure-GF(2) context).  Monomial keys must be sorted.
    Poly {
        coeffs: BTreeMap<Vec<Var>, u8>,
        constant: Constant,
    },
    /// Rotate-left `src` (of type `ty`) by `n` bit positions.
    Rol {
        src: Var,
        ty: Ty,
        n: usize,
    },
    /// Rotate-right `src` (of type `ty`) by `n` bit positions.
    Ror {
        src: Var,
        ty: Ty,
        n: usize,
    },
    /// Concatenate `parts` in order (LSB-first) into a wider value of type `ty`.
    Merge {
        parts: Vec<Var>,
        ty: Ty,
    },
    /// Broadcast a single-bit value across every bit position of type `ty`.
    Splat {
        src: Var,
        ty: Ty,
    },
    /// Arbitrary bit shuffle: assemble an output from individually selected bits.
    ///
    /// `result_bits[i] = (bit_idx, var)` — bit `i` of the output is taken from
    /// bit `bit_idx` of `var`.  The length of `result_bits` equals the output
    /// bit-width (e.g. 256 for a full AES-lane shuffle).
    Shuffle {
        result_bits: Vec<(u8, Var)>,
        ty: Ty,
    },
}
