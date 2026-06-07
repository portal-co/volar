/-
Mirrors crates/spec/volar-primitives/src/lib.rs (GF(2^n) field arithmetic).

Abstract semantic model: GF(2^n) is modeled by Mathlib's `GaloisField 2 n`,
the field of order `2^n`. The Rust file implements `gf_mul_*` (carry-less
multiplication) and `gf_invert_*` (Itoh-Tsujii inversion); here we model those
as the field operations `(*)` and `(·⁻¹)` and prove the algebraic facts the
Rust code relies on. We do NOT port the bit-twiddling loops or prove a
Rust↔Lean bridge (abstract-model-only decision).

All theorems in this file are FULLY PROVEN and depend only on Lean/Mathlib's
standard axioms (propext, Classical.choice, Quot.sound) — never on `Volar.Hash`.
-/
import Mathlib.FieldTheory.Finite.GaloisField
import Mathlib.FieldTheory.Finite.Basic

namespace Volar.Primitives

instance : Fact (Nat.Prime 2) := ⟨Nat.prime_two⟩

/-- Binary extension field GF(2^n), modeled as the Galois field of order 2^n. -/
abbrev GF (n : ℕ) := GaloisField 2 n

section
variable {n : ℕ}

/-- `gf_mul` is field multiplication (carry-less mul modulo the reduction poly). -/
noncomputable def gfMul (a b : GF n) : GF n := a * b

/-- `gf_invert` is field inversion, with the Rust convention `invert 0 = 0`. -/
noncomputable def gfInvert (a : GF n) : GF n := a⁻¹

-- Field axioms (inherited from the `Field (GF n)` instance).
theorem gfMul_comm (a b : GF n) : gfMul a b = gfMul b a := mul_comm a b
theorem gfMul_assoc (a b c : GF n) : gfMul (gfMul a b) c = gfMul a (gfMul b c) :=
  mul_assoc a b c
theorem gfMul_one (a : GF n) : gfMul a 1 = a := mul_one a
theorem gf_left_distrib (a b c : GF n) : a * (b + c) = a * b + a * c := mul_add a b c

/-- Inversion correctness: `a * a⁻¹ = 1` for nonzero `a`. -/
theorem gfMul_invert_cancel (a : GF n) (ha : a ≠ 0) : gfMul a (gfInvert a) = 1 :=
  mul_inv_cancel₀ ha

/-- The Rust convention `gf_invert(0) = 0`. -/
theorem gfInvert_zero : gfInvert (0 : GF n) = 0 := inv_zero

/-- Fermat's little theorem for GF(2^n): `a^(2^n - 1) = 1` for nonzero `a`. -/
theorem gf_pow_card_sub_one (hn : n ≠ 0) (a : GF n) (ha : a ≠ 0) :
    a ^ (2 ^ n - 1) = 1 := by
  haveI : Fintype (GF n) := Fintype.ofFinite _
  have hcard : Fintype.card (GF n) = 2 ^ n := by
    rw [← Nat.card_eq_fintype_card]; exact GaloisField.card 2 n hn
  have h := FiniteField.pow_card_sub_one_eq_one a ha
  rwa [hcard] at h

/-- **Itoh–Tsujii correctness.** The exponent the Rust `gf_invert_*` raises to,
    `2^n - 2`, computes the multiplicative inverse: `a^(2^n - 2) = a⁻¹`. -/
theorem gf_invert_eq_pow (hn : n ≠ 0) (a : GF n) (ha : a ≠ 0) :
    a ^ (2 ^ n - 2) = gfInvert a := by
  have h2 : (2 : ℕ) ≤ 2 ^ n := by
    calc (2 : ℕ) = 2 ^ 1 := (pow_one 2).symm
      _ ≤ 2 ^ n := Nat.pow_le_pow_right (by norm_num) (Nat.one_le_iff_ne_zero.mpr hn)
  have hpow : a ^ (2 ^ n - 1) = 1 := gf_pow_card_sub_one hn a ha
  have hsplit : 2 ^ n - 1 = (2 ^ n - 2) + 1 := by omega
  rw [hsplit, pow_succ] at hpow
  -- hpow : a ^ (2^n - 2) * a = 1
  show a ^ (2 ^ n - 2) = a⁻¹
  calc a ^ (2 ^ n - 2)
      = a ^ (2 ^ n - 2) * a * a⁻¹ := by rw [mul_assoc, mul_inv_cancel₀ ha, mul_one]
    _ = 1 * a⁻¹ := by rw [hpow]
    _ = a⁻¹ := one_mul _

end

end Volar.Primitives
