/-
Mirrors crates/spec/volar-spec/src/ot/ideal_cot.rs:
    one correlated-OT instance returns (r0, v = r0 + b·Δ).

Abstract model over a field `K`; the correlation relation is FULLY PROVEN.
-/
import Mathlib.Algebra.Field.Basic

namespace Volar.Spec

variable {K : Type*} [Field K]

/-- A single ideal C-OT: on choice bit `b` and global secret `delta`, returns
    `(r0, r0 + b·Δ)`. -/
def cot (r0 : K) (b : Bool) (delta : K) : K × K :=
  (r0, r0 + (if b then delta else 0))

/-- The two outputs differ by exactly `b·Δ` (the correlation). -/
theorem cot_correlation (r0 delta : K) (b : Bool) :
    (cot r0 b delta).2 - (cot r0 b delta).1 = (if b then delta else 0) := by
  simp [cot]

/-- On `b = false` the receiver learns `r0` only. -/
theorem cot_false (r0 delta : K) : cot r0 false delta = (r0, r0) := by simp [cot]

/-- On `b = true` the second output is `r0 + Δ`. -/
theorem cot_true (r0 delta : K) : cot r0 true delta = (r0, r0 + delta) := by simp [cot]

end Volar.Spec
