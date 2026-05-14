// @reliability: experimental
//! @ai: assisted
//! Shared scaffolding for VOLE-style weavers.
//!
//! Pulled out of [`crate::vole`] so that a degree-K variant (e.g. FAEST's
//! QuickSilver at K=3) and the existing K=1 AND-check weaver can share the
//! same type-builder helpers without forking the surface.
//!
//! ## What lives here
//!
//! - [`vope_type_k`] — `Vope<N, T, U{k}>` for any `k ≥ 0`. The existing K=1
//!   weaver calls `vope_type_k(1)`; the FAEST weaver will call
//!   `vope_type_k(3)` for S-box norm-constraint commitments.
//! - [`q_type`] / [`delta_type`] / [`array_t_n`] — verifier-side and shared
//!   helpers, all K-independent.
//! - [`typenum_uk_name`] — `U0`, `U1`, …, `U16` symbolic typenum names. The
//!   weaver emits these as `IrType::TypeParam` so they print as
//!   `Vope<N, T, U3>` not as a runtime value.
//!
//! ## What does NOT live here yet
//!
//! - The gate dispatch table (XOR/NOT/AND at K=1; S-box at K=3 in FAEST).
//!   That is gate-library territory and is per-weaver.
//! - The prover/verifier function-signature scaffolding. Threading K
//!   through those touches enough of [`crate::vole`] that we leave it for
//!   M5 when the FAEST weaver actually wants to emit K>1 gates. Until then
//!   the existing weaver continues calling these helpers at K=1.

use alloc::{boxed::Box, string::ToString, vec};

use volar_compiler::ir::{IrType, StructKind};

/// `Vope<N, T, U{k}>` — prover's degree-K VOLE wire commitment.
///
/// `k = 1` is the default Quicksilver AND-check commitment. `k = 2` and
/// `k = 3` are used by higher-degree constraint systems (e.g. FAEST's
/// degree-3 AES S-box norm constraint).
///
/// Currently supports `k ∈ [0, 16]`; beyond that the typenum encoding
/// would need a multi-character form (`UTerm`, etc.) which we'll add when
/// any consumer needs it.
pub fn vope_type_k(k: usize) -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Vope".into()),
        type_args: vec![
            IrType::TypeParam("N".into()),
            IrType::TypeParam("T".into()),
            IrType::TypeParam(typenum_uk_name(k).to_string()),
        ],
    }
}

/// `Q<N, T>` — verifier's VOLE wire share. K-independent.
pub fn q_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Q".into()),
        type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
    }
}

/// `Delta<N, T>` — verifier's global secret. K-independent.
pub fn delta_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Delta".into()),
        type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
    }
}

/// `Array<T, N>` — element-wise hat / field vector.
pub fn array_t_n() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Array".into()),
        type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
    }
}

/// Symbolic typenum identifier for `Vope`'s degree parameter.
///
/// Returns `"U0"`, `"U1"`, …, `"U16"`. Beyond 16 we'll need either a
/// composite typenum encoding or a const-generic refactor on `Vope`
/// itself; flag at the use site rather than silently wrapping.
pub fn typenum_uk_name(k: usize) -> &'static str {
    match k {
        0 => "U0",
        1 => "U1",
        2 => "U2",
        3 => "U3",
        4 => "U4",
        5 => "U5",
        6 => "U6",
        7 => "U7",
        8 => "U8",
        9 => "U9",
        10 => "U10",
        11 => "U11",
        12 => "U12",
        13 => "U13",
        14 => "U14",
        15 => "U15",
        16 => "U16",
        _ => panic!("typenum_uk_name: k={} exceeds supported range (0..=16)", k),
    }
}

/// `&T` reference helper. The existing weaver also defines this locally;
/// re-exporting from here lets future weavers share the same `IrType` shape
/// rather than re-deriving it.
pub fn ref_to_vole(ty: IrType) -> IrType {
    IrType::Reference {
        mutable: false,
        elem: Box::new(ty),
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use std::format;

    #[test]
    fn vope_type_k1_matches_legacy_layout() {
        // Sanity: K=1 still produces the structural shape the existing weaver
        // emitted before the refactor.
        let t = vope_type_k(1);
        match t {
            IrType::Struct { kind: StructKind::Custom(ref name), ref type_args } => {
                assert_eq!(name, "Vope");
                assert_eq!(type_args.len(), 3);
                if let IrType::TypeParam(ref s) = type_args[2] {
                    assert_eq!(s, "U1");
                } else {
                    panic!("expected TypeParam");
                }
            }
            _ => panic!("expected Vope struct, got {:?}", format!("{:?}", t)),
        }
    }

    #[test]
    fn vope_type_k3_emits_u3() {
        let t = vope_type_k(3);
        match t {
            IrType::Struct { type_args, .. } => {
                if let IrType::TypeParam(ref s) = type_args[2] {
                    assert_eq!(s, "U3");
                } else {
                    panic!("expected TypeParam");
                }
            }
            _ => panic!("expected Vope struct"),
        }
    }

    #[test]
    #[should_panic(expected = "exceeds supported range")]
    fn vope_type_k_panics_above_16() {
        let _ = vope_type_k(17);
    }
}
