// @reliability: normal
// @ai: assisted
//! Compiler-IR-level pass: expand packed-bit types to arrays of `Z3` elements.
//!
//! `BitsInBytes` and `BitsInBytes64` are convenience packed types that store
//! 8 or 64 GF(2) bits in a single integer word.  Before the TFHE backend can
//! process them as individual ternary elements (using the `Z3` type with the
//! `None` sentinel), they must be *unpacked* into `[Z3; 8]` or `[Z3; 64]`.
//!
//! This pass operates on the compiler IR (`IrModule`) before lowering and
//! TFHE weaving.  It rewrites:
//!
//! - `PrimitiveType::BitsInBytes`   → `IrType::Array { elem: Z3, len: 8 }`
//! - `PrimitiveType::BitsInBytes64` → `IrType::Array { elem: Z3, len: 64 }`
//!
//! The rewrite is *structural*: it replaces type annotations in function
//! signatures, struct fields, and let bindings.  It does **not** generate
//! new bit-extraction expressions; that is the responsibility of the
//! subsequent lowering pass.
//!
//! # When to use
//! Call this pass as the first step in the TFHE compilation pipeline, before
//! `lowering_dyn` or any backend-specific weaving.  Do **not** call it in the
//! GF(2) / Boolar pipeline — `BitsInBytes` has an efficient packed
//! representation there.

#[cfg(feature = "std")]
use std::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use volar_compiler::ir::{
    ArrayKind, ArrayLength, IrType, PrimitiveType,
};

/// Rewrite `BitsInBytes` → `[Z3; 8]` and `BitsInBytes64` → `[Z3; 64]`
/// throughout a single `IrType`.
///
/// Recurses into array element types, vector element types, struct type args,
/// tuple elements, and reference targets.  All other types are returned
/// unchanged.
pub fn unpack_bits_in_type(ty: IrType) -> IrType {
    match ty {
        IrType::Primitive(PrimitiveType::BitsInBytes) => IrType::Array {
            kind: ArrayKind::FixedArray,
            elem: Box::new(IrType::Primitive(PrimitiveType::Z3)),
            len: ArrayLength::Const(8),
        },
        IrType::Primitive(PrimitiveType::BitsInBytes64) => IrType::Array {
            kind: ArrayKind::FixedArray,
            elem: Box::new(IrType::Primitive(PrimitiveType::Z3)),
            len: ArrayLength::Const(64),
        },
        // Recurse into compound types.
        IrType::Array { kind, elem, len } => IrType::Array {
            kind,
            elem: Box::new(unpack_bits_in_type(*elem)),
            len,
        },
        IrType::Vector { elem } => IrType::Vector {
            elem: Box::new(unpack_bits_in_type(*elem)),
        },
        IrType::Struct { kind, type_args } => IrType::Struct {
            kind,
            type_args: type_args.into_iter().map(unpack_bits_in_type).collect(),
        },
        IrType::Tuple(elems) => {
            IrType::Tuple(elems.into_iter().map(unpack_bits_in_type).collect())
        }
        IrType::Reference { mutable, elem } => IrType::Reference {
            mutable,
            elem: Box::new(unpack_bits_in_type(*elem)),
        },
        // Leaf or opaque types — pass through unchanged.
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bits_in_bytes_becomes_z3_array_8() {
        let input = IrType::Primitive(PrimitiveType::BitsInBytes);
        let output = unpack_bits_in_type(input);
        assert_eq!(
            output,
            IrType::Array {
                kind: ArrayKind::FixedArray,
                elem: Box::new(IrType::Primitive(PrimitiveType::Z3)),
                len: ArrayLength::Const(8),
            }
        );
    }

    #[test]
    fn test_bits_in_bytes64_becomes_z3_array_64() {
        let input = IrType::Primitive(PrimitiveType::BitsInBytes64);
        let output = unpack_bits_in_type(input);
        assert_eq!(
            output,
            IrType::Array {
                kind: ArrayKind::FixedArray,
                elem: Box::new(IrType::Primitive(PrimitiveType::Z3)),
                len: ArrayLength::Const(64),
            }
        );
    }

    #[test]
    fn test_z3_passes_through_unchanged() {
        let input = IrType::Primitive(PrimitiveType::Z3);
        let output = unpack_bits_in_type(input.clone());
        assert_eq!(output, input);
    }

    #[test]
    fn test_nested_array_recursed() {
        // Vec<BitsInBytes> → Vec<[Z3; 8]>
        let input = IrType::Vector {
            elem: Box::new(IrType::Primitive(PrimitiveType::BitsInBytes)),
        };
        let output = unpack_bits_in_type(input);
        assert_eq!(
            output,
            IrType::Vector {
                elem: Box::new(IrType::Array {
                    kind: ArrayKind::FixedArray,
                    elem: Box::new(IrType::Primitive(PrimitiveType::Z3)),
                    len: ArrayLength::Const(8),
                })
            }
        );
    }
}
