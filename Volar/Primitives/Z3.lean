/-
Mirrors the `Z3` type in crates/spec/volar-primitives/src/lib.rs (GF(3) with a
Möbius lifting used so AND-style products behave correctly). Abstract model:
GF(3) is `ZMod 3`. We prove it is a field and the inversion facts; the bit-level
Möbius encoding is an implementation detail not modeled here.

FULLY PROVEN; depends only on the standard axioms (never `Volar.Hash`).
-/
import Mathlib.Data.ZMod.Basic
import Mathlib.FieldTheory.Finite.Basic

namespace Volar.Primitives

/-- GF(3), modeled as `ZMod 3`. -/
abbrev Z3 := ZMod 3

theorem z3_mul_comm (a b : Z3) : a * b = b * a := mul_comm a b
theorem z3_left_distrib (a b c : Z3) : a * (b + c) = a * b + a * c := mul_add a b c

/-- Inversion correctness in GF(3). -/
theorem z3_mul_inv_cancel (a : Z3) (ha : a ≠ 0) : a * a⁻¹ = 1 := mul_inv_cancel₀ ha

/-- In GF(3) every nonzero element is its own inverse (`a^2 = 1`), the algebraic
    fact behind the Möbius lift. -/
theorem z3_self_inverse (a : Z3) (ha : a ≠ 0) : a * a = 1 := by
  have hcard : Fintype.card Z3 = 3 := by decide
  have h := FiniteField.pow_card_sub_one_eq_one a ha
  rw [hcard] at h
  simpa [pow_succ, pow_zero, one_mul] using h

end Volar.Primitives
