/-
Mirrors the core VOLE relation in crates/spec/volar-spec/src/vole/ :
    u · Δ + v = q   over a binary extension field.

Abstract model over an arbitrary field `K`. Completeness and the binding
(soundness) lemma are FULLY PROVEN.
-/
import Mathlib.Algebra.Field.Basic

namespace Volar.Spec

variable {K : Type*} [Field K]

/-- A single VOLE wire: prover value `u`, prover mask `v`, verifier share `q`,
    global secret `delta`. -/
structure Vole (K : Type*) [Field K] where
  u : K
  v : K
  q : K
  delta : K

/-- The VOLE relation `u·Δ + v = q`. -/
def Vole.relation (w : Vole K) : Prop := w.u * w.delta + w.v = w.q

/-- Honest construction: the verifier share is computed as `u·Δ + v`. -/
def honest (u v delta : K) : Vole K := ⟨u, v, u * delta + v, delta⟩

/-- **Completeness:** an honestly generated VOLE satisfies the relation. -/
theorem honest_relation (u v delta : K) : (honest u v delta).relation := rfl

/-- **Soundness / binding:** for a fixed mask `v`, global secret `Δ ≠ 0`, and
    verifier share `q`, the relation pins down a unique prover value `u`. -/
theorem vole_binding {u u' v q delta : K} (hd : delta ≠ 0)
    (h : u * delta + v = q) (h' : u' * delta + v = q) : u = u' := by
  have hmul : u * delta = u' * delta := by
    have := h.trans h'.symm
    exact add_right_cancel this
  exact mul_right_cancel₀ hd hmul

end Volar.Spec
