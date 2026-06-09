// @reliability: normal
// @ai: assisted
//! IR pass: lift GF(2) `Bit`-typed polynomials to GF(3) `Z3` polynomials.
//!
//! Each `Stmt::Poly { ty: Bit, ... }` in the input `IRBlocks` is replaced by
//! an equivalent `Stmt::Poly { ty: Z3, ... }` whose coefficients are the GF(3)
//! Möbius lifting of the original GF(2) multilinear polynomial.
//!
//! # Algorithm — GF(2)→GF(3) Möbius inversion
//!
//! Given a GF(2) multilinear polynomial f over n variables:
//! 1. Enumerate all 2^n evaluation points S ⊆ vars.
//! 2. Evaluate F(S) = f at the 0/1 point where xᵢ = 1 iff vᵢ ∈ S, mod 2.
//! 3. Apply Möbius inversion over the Boolean lattice, coefficients mod 3:
//!       a_S = Σ_{T⊆S} (−1)^(|S|−|T|) F(T) mod 3
//! 4. Build a new GF(3) `Poly` statement with these coefficients.
//!
//! The result agrees with f on all {0,1}^n inputs (Bit semantics preserved)
//! and is defined over all of GF(3) for TFHE bootstrapping.
//!
//! # Lift table for common small polynomials
//! | GF(2) form   | GF(3) form        |
//! |--------------|-------------------|
//! | `x`          | `x`               |
//! | `1 + x`      | `1 + 2x`          |
//! | `xy`         | `xy`              |
//! | `x + y`      | `x + y + xy`      |
//! | `x + y + xy` | `x + y + 2xy`     |
//! | `1 + xy`     | `1 + 2xy`         |
//! | `1 + x + y`  | `1 + 2x + 2y + 2xy` |
//!
//! # Limitations
//! - Only rewrites `Stmt::Poly` nodes whose `ty` resolves to `Bit`.
//! - Polynomials with more than 20 distinct variables are rejected (panic).
//!   In practice Volar IR polynomials are bounded by gate fan-in.
//! - All other statements are cloned unchanged.

use alloc::{collections::BTreeMap, vec, vec::Vec};

use volar_ir::ir::{IRBlock, IRBlocks, IRStmt, IRTypeId, IRTypes, IRVarId};
use volar_ir_common::{Constant, IrType, Stmt, Type as PrimType};

// ============================================================================
// Public API
// ============================================================================

/// Lift every `Stmt::Poly { ty: Bit, ... }` in `blocks` to an equivalent
/// `Stmt::Poly { ty: Z3, ... }` via GF(2)→GF(3) Möbius inversion.
///
/// `types` is mutated to intern the `Z3` primitive type if it is not already
/// present.  All other type entries are unchanged.
///
/// # Panics
/// Panics if any Bit-typed polynomial has more than 20 distinct variables
/// (unreachable in practice — see module documentation).
pub fn raise_bits_to_z3<P: Clone>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
) -> IRBlocks<P> {
    let z3_ty_id = types.intern(IrType::Primitive(PrimType::Z3));
    let bit_ty_id = types.intern(IrType::Primitive(PrimType::Bit));

    IRBlocks {
        oracles: blocks.oracles.clone(),
        actions: blocks.actions.clone(),
        rngs: blocks.rngs.clone(),
        blocks: blocks
            .blocks
            .iter()
            .map(|b| lift_block(b, bit_ty_id, z3_ty_id))
            .collect(),
        pre_init: blocks.pre_init.clone(),
    }
}

// ============================================================================
// Block lifting
// ============================================================================

fn lift_block<P: Clone>(
    block: &IRBlock<P>,
    bit_ty: IRTypeId,
    z3_ty: IRTypeId,
) -> IRBlock<P> {
    let new_stmts = block
        .stmts
        .iter()
        .map(|s| lift_stmt(s, bit_ty, z3_ty))
        .collect();
    IRBlock {
        params: block.params.clone(),
        stmts: new_stmts,
        stmt_provs: block.stmt_provs.clone(),
        terminator: block.terminator.clone(),
    }
}

fn lift_stmt(stmt: &IRStmt, bit_ty: IRTypeId, z3_ty: IRTypeId) -> IRStmt {
    match stmt {
        Stmt::Poly { ty, coeffs, constant } if *ty == bit_ty => {
            let (new_coeffs, new_const) = mobius_lift(coeffs, *constant);
            Stmt::Poly {
                ty: z3_ty,
                coeffs: new_coeffs,
                constant: new_const,
            }
        }
        other => other.clone(),
    }
}

// ============================================================================
// Möbius lifting core
// ============================================================================

/// Lift a GF(2) multilinear polynomial (represented by `coeffs` and
/// `constant`) to an equivalent GF(3) multilinear polynomial.
///
/// Returns `(gf3_coeffs, gf3_constant)`.
fn mobius_lift(
    coeffs: &BTreeMap<Vec<IRVarId>, u8>,
    constant: Constant,
) -> (BTreeMap<Vec<IRVarId>, u8>, Constant) {
    // --- Collect and index all variables ---------------------------------
    let mut all_vars: Vec<IRVarId> = Vec::new();
    for mono in coeffs.keys() {
        for &v in mono {
            if all_vars.iter().all(|u| *u != v) {
                all_vars.push(v);
            }
        }
    }
    all_vars.sort();

    let n = all_vars.len();
    assert!(
        n <= 20,
        "raise_to_z3: polynomial has {n} distinct variables; \
         Möbius lift is limited to ≤ 20 (2^20 = 1 M subsets)"
    );

    let num_subsets = 1usize << n;

    // --- Evaluate F(S) for each S ⊆ vars (mod 2) ------------------------
    let mut f_val: Vec<u8> = vec![0u8; num_subsets];

    // Degree-0 constant contributes to every evaluation point.
    let const_bit = (constant.lo & 1) as u8;
    for fv in f_val.iter_mut() {
        *fv = const_bit;
    }

    // Each monomial contributes to evaluations where all its vars are in S.
    for (mono, &coeff) in coeffs {
        if coeff & 1 == 0 {
            continue; // GF(2) coefficient 0 — skip
        }
        // Build the bitmask for this monomial's variable set.
        let mut mono_mask: usize = 0;
        for &v in mono {
            let idx = all_vars.iter().position(|u| *u == v).unwrap();
            mono_mask |= 1 << idx;
        }
        // F(S) ^= 1 for every S that is a superset of mono_mask.
        // Enumerate supersets of mono_mask in {0..num_subsets}.
        let complement = (!mono_mask) & (num_subsets - 1);
        let mut extra = complement;
        loop {
            f_val[mono_mask | extra] ^= 1;
            if extra == 0 {
                break;
            }
            extra = (extra - 1) & complement;
        }
    }

    // --- Möbius inversion mod 3 -----------------------------------------
    // a_S = Σ_{T⊆S} (-1)^(|S|−|T|) F(T) mod 3
    let mut a_vals: Vec<i32> = vec![0i32; num_subsets];

    for s in 0..num_subsets {
        let s_size = s.count_ones() as usize;
        // Iterate over all T ⊆ S.
        let mut t = s;
        loop {
            let t_size = t.count_ones() as usize;
            let sign = if (s_size - t_size) % 2 == 0 { 1i32 } else { -1i32 };
            a_vals[s] += sign * f_val[t] as i32;
            if t == 0 {
                break;
            }
            t = (t - 1) & s;
        }
        a_vals[s] = ((a_vals[s] % 3) + 3) % 3;
    }

    // --- Build output polynomial ----------------------------------------
    // a_vals[0] is the degree-0 (constant) term.
    let new_const = Constant { hi: 0, lo: a_vals[0] as u128 };

    let mut new_coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
    for s in 1..num_subsets {
        if a_vals[s] == 0 {
            continue;
        }
        let mono: Vec<IRVarId> = (0..n)
            .filter(|&i| (s >> i) & 1 == 1)
            .map(|i| all_vars[i])
            .collect();
        new_coeffs.insert(mono, a_vals[s] as u8);
    }

    (new_coeffs, new_const)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use std::vec;

    use super::*;
    use volar_ir_common::{IrType, TypeTable};

    /// Convenience: build an `IRVarId` from a small integer.
    fn var(n: u32) -> IRVarId {
        IRVarId(n)
    }

    // Evaluate a GF(3) polynomial (BTreeMap<mono, coeff>, constant) at a
    // point given as a map var_index → value (0, 1, or 2).
    fn eval_gf3(
        coeffs: &BTreeMap<Vec<IRVarId>, u8>,
        constant: Constant,
        point: &[(IRVarId, u8)],
    ) -> u8 {
        let lookup = |v: &IRVarId| -> u8 {
            point.iter().find(|(u, _)| u == v).map(|(_, x)| *x).unwrap_or(0)
        };
        let add3 = |a: u8, b: u8| -> u8 { let s = a + b; if s >= 3 { s - 3 } else { s } };
        let mul3 = |a: u8, b: u8| -> u8 { let p = a * b; if p >= 3 { p - 3 } else { p } };
        let mulmod3 = |coeff: u8, prod: u8| -> u8 {
            // coeff * prod mod 3
            let mut r = 0u8;
            let mut c = coeff % 3;
            let mut p = prod;
            // manual mul mod 3 (coeff ≤ 2, prod ≤ 2)
            r = add3(r, if c >= 1 { p } else { 0 });
            r = add3(r, if c >= 2 { p } else { 0 });
            r
        };

        let mut result = (constant.lo % 3) as u8;
        for (mono, &coeff) in coeffs {
            let prod = mono.iter().fold(1u8, |acc, v| mul3(acc, lookup(v)));
            let term = mulmod3(coeff, prod);
            result = add3(result, term);
        }
        result
    }

    // Evaluate a GF(2) polynomial at a 0/1 point.
    fn eval_gf2(
        coeffs: &BTreeMap<Vec<IRVarId>, u8>,
        constant: Constant,
        point: &[(IRVarId, u8)],
    ) -> u8 {
        let lookup = |v: &IRVarId| -> u8 {
            point.iter().find(|(u, _)| u == v).map(|(_, x)| *x & 1).unwrap_or(0)
        };
        let mut result = (constant.lo & 1) as u8;
        for (mono, &coeff) in coeffs {
            if coeff & 1 == 0 { continue; }
            let prod = mono.iter().fold(1u8, |acc, v| acc & lookup(v));
            result ^= prod;
        }
        result
    }

    #[test]
    fn test_identity_lift() {
        // f = x  →  g = x
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![var(0)], 1u8);
        let constant = Constant { hi: 0, lo: 0 };
        let (g_coeffs, g_const) = mobius_lift(&coeffs, constant);
        // Check agreement on {0,1}
        for xval in 0u8..2 {
            let point = &[(var(0), xval)];
            assert_eq!(eval_gf2(&coeffs, constant, point),
                       eval_gf3(&g_coeffs, g_const, point),
                       "identity lift mismatch at x={xval}");
        }
    }

    #[test]
    fn test_not_lift() {
        // f = 1 + x  →  g = 1 + 2x
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![var(0)], 1u8);
        let constant = Constant { hi: 0, lo: 1 };
        let (g_coeffs, g_const) = mobius_lift(&coeffs, constant);
        for xval in 0u8..2 {
            let pt = &[(var(0), xval)];
            assert_eq!(eval_gf2(&coeffs, constant, pt),
                       eval_gf3(&g_coeffs, g_const, pt),
                       "NOT lift mismatch at x={xval}");
        }
    }

    #[test]
    fn test_xor_lift() {
        // f = x + y  →  g = x + y + xy
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![var(0)], 1u8);
        coeffs.insert(vec![var(1)], 1u8);
        let constant = Constant { hi: 0, lo: 0 };
        let (g_coeffs, g_const) = mobius_lift(&coeffs, constant);
        for xval in 0u8..2 { for yval in 0u8..2 {
            let pt = &[(var(0), xval), (var(1), yval)];
            assert_eq!(eval_gf2(&coeffs, constant, pt),
                       eval_gf3(&g_coeffs, g_const, pt),
                       "XOR lift mismatch at x={xval} y={yval}");
        }}
    }

    #[test]
    fn test_and_lift() {
        // f = xy  →  g = xy
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![var(0), var(1)], 1u8);
        let constant = Constant { hi: 0, lo: 0 };
        let (g_coeffs, g_const) = mobius_lift(&coeffs, constant);
        for xval in 0u8..2 { for yval in 0u8..2 {
            let pt = &[(var(0), xval), (var(1), yval)];
            assert_eq!(eval_gf2(&coeffs, constant, pt),
                       eval_gf3(&g_coeffs, g_const, pt),
                       "AND lift mismatch at x={xval} y={yval}");
        }}
    }

    #[test]
    fn test_or_lift() {
        // f = x + y + xy  →  g = x + y + 2xy
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![var(0)], 1u8);
        coeffs.insert(vec![var(1)], 1u8);
        coeffs.insert(vec![var(0), var(1)], 1u8);
        let constant = Constant { hi: 0, lo: 0 };
        let (g_coeffs, g_const) = mobius_lift(&coeffs, constant);
        for xval in 0u8..2 { for yval in 0u8..2 {
            let pt = &[(var(0), xval), (var(1), yval)];
            assert_eq!(eval_gf2(&coeffs, constant, pt),
                       eval_gf3(&g_coeffs, g_const, pt),
                       "OR lift mismatch at x={xval} y={yval}");
        }}
    }

    #[test]
    fn test_raise_bits_updates_type() {
        // Build a trivial one-block IRBlocks with a Poly{Bit} statement
        // and verify the output has a Poly{Z3} statement.
        use volar_ir::ir::{IRBlock, IRBlockTargetId, IRTerminator};

        let mut types = TypeTable::new();
        let bit_id = types.intern(IrType::Primitive(PrimType::Bit));
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![IRVarId(0)], 1u8); // "x"
        let poly_stmt = Stmt::Poly {
            ty: bit_id,
            coeffs,
            constant: Constant { hi: 0, lo: 0 },
        };
        let block = IRBlock {
            params: vec![bit_id],
            stmts: vec![poly_stmt],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(1)],
            },
        };
        let blocks: IRBlocks<()> = IRBlocks::new(vec![block]);
        let lifted = raise_bits_to_z3(&blocks, &mut types);

        let z3_id = types.intern(IrType::Primitive(PrimType::Z3));
        assert!(matches!(
            &lifted.blocks[0].stmts[0],
            Stmt::Poly { ty, .. } if *ty == z3_id
        ));
    }
}
