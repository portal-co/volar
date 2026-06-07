/-
Mirrors the `Tropical` type in crates/spec/volar-primitives/src/lib.rs
(min-plus tropical semiring). Abstract model: Mathlib's `Tropical (WithTop ℕ)`,
where `+` is `min` and `*` is `+`. We prove the semiring laws the Rust type
provides.

FULLY PROVEN; depends only on the standard axioms (never `Volar.Hash`).
-/
import Mathlib.Algebra.Tropical.Basic
import Mathlib.Algebra.Order.Ring.WithTop

namespace Volar.Primitives

/-- Tropical (min-plus) semiring over `WithTop ℕ`. -/
abbrev Trop := Tropical (WithTop ℕ)

theorem trop_add_comm (a b : Trop) : a + b = b + a := add_comm a b
theorem trop_mul_comm (a b : Trop) : a * b = b * a := mul_comm a b
theorem trop_mul_assoc (a b c : Trop) : a * b * c = a * (b * c) := mul_assoc a b c
theorem trop_mul_one (a : Trop) : a * 1 = a := mul_one a

end Volar.Primitives
