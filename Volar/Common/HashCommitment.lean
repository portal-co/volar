/-
Mirrors crates/spec/volar-common/src/hash_commitment.rs:
    commit(message, rand) = H(message || rand).

Completeness is FULLY PROVEN. Binding is stated as a *conditional* theorem
reducing to a hash collision — it is NOT an axiom (per the axiom-whitelist
policy, only the external hash `Volar.Hash` may be axiomatized).
-/
import Volar.Axioms

namespace Volar.Common

open Volar

/-- `commit message rand = H(message ‖ rand)`. (Noncomputable: `Hash` is an
    opaque external primitive with no executable definition.) -/
noncomputable def commit (message rand : List UInt8) : List UInt8 := Hash (message ++ rand)

/-- `validate` recomputes the commitment and compares (mirrors `validate`). -/
noncomputable def validate (c message rand : List UInt8) : Bool := commit message rand == c

/-- **Completeness:** a commitment always validates against its own opening. -/
theorem validate_commit (message rand : List UInt8) :
    validate (commit message rand) message rand = true := by
  simp [validate]

/-- A hash collision on the committed pre-images. -/
def HashCollision (a b : List UInt8) : Prop := a ≠ b ∧ Hash a = Hash b

/-- **Binding (conditional):** two openings of the same commitment either agree
    on the concatenated pre-image, or exhibit a hash collision. This reduces
    binding to collision-resistance of the external hash; it introduces no axiom. -/
theorem commit_binding {m₁ r₁ m₂ r₂ : List UInt8}
    (h : commit m₁ r₁ = commit m₂ r₂) :
    (m₁ ++ r₁ = m₂ ++ r₂) ∨ HashCollision (m₁ ++ r₁) (m₂ ++ r₂) := by
  by_cases heq : m₁ ++ r₁ = m₂ ++ r₂
  · exact Or.inl heq
  · exact Or.inr ⟨heq, h⟩

end Volar.Common
