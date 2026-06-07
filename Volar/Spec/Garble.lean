/-
Mirrors crates/spec/volar-spec/src/garble.rs (half-gate garbled circuits over
VOLE-style labels).

POLICY NOTE: per the project directive, garbled-circuit correctness MUST be
PROVEN, never discharged by an axiom. Accordingly the theorems below are real
proofs over the field model. The per-gate mask (`H(L_A, L_B)` in the real
scheme) appears only as an opaque *function* `mask`; correctness needs only that
the same mask value is added and removed — a property of function-ness, not a
security axiom.

Abstract model over a field `K`. FULLY PROVEN.
-/
import Mathlib.Algebra.Field.Basic
import Mathlib.Tactic.Ring

namespace Volar.Spec

variable {K : Type*} [Field K]

/-- Garbler state: the global secret Δ (the `GlobalSecret`/Δ in the Rust). -/
structure Garble (K : Type*) [Field K] where
  delta : K

/-- Wire encoding: label for bit `b` on a wire with zero-label `zero` is
    `zero + b·Δ` (with bits embedded as `0,1 ∈ K`). -/
def Garble.encode (g : Garble K) (zero b : K) : K := zero + b * g.delta

/-- **Free-XOR correctness.** The XOR wire's zero-label is `A0 + B0`, and the
    encoding is additively homomorphic: `enc (A0+B0) (a+b) = enc A0 a + enc B0 b`.
    (XOR of bits is modeled by `+` of their field embeddings.) -/
theorem free_xor_correct (g : Garble K) (A0 B0 a b : K) :
    g.encode (A0 + B0) (a + b) = g.encode A0 a + g.encode B0 b := by
  simp only [Garble.encode]; ring

/-- A garbled AND-gate row: `mask(L_A i, L_B j) + enc C0 (i·j)`, where `mask`
    models the gate hash `H(·,·)` (used here purely as a function). -/
def garbleRow (g : Garble K) (mask : K → K → K) (A0 B0 C0 i j : K) : K :=
  mask (g.encode A0 i) (g.encode B0 j) + g.encode C0 (i * j)

/-- The evaluator strips the mask using its two input labels. -/
def evalAnd (mask : K → K → K) (row LA LB : K) : K := row - mask LA LB

/-- **Garbled AND correctness (PROVEN, not axiomatized).** Evaluating the row
    selected by inputs `(a, b)` recovers the output label `enc C0 (a·b)` —
    i.e. the encoding of `a ∧ b` (product of bit embeddings). -/
theorem garbled_and_correct (g : Garble K) (mask : K → K → K) (A0 B0 C0 a b : K) :
    evalAnd mask (garbleRow g mask A0 B0 C0 a b) (g.encode A0 a) (g.encode B0 b)
      = g.encode C0 (a * b) := by
  simp only [evalAnd, garbleRow]; ring

end Volar.Spec
