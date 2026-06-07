/-
Mirrors crates/spec/volar-common/src/length_doubling.rs:
    double(a) = [ H(a), H(a) XOR a ]   (the ViaDigest length-doubling PRG).

Structural/determinism facts are FULLY PROVEN. PRG *security* is NOT claimed
here and is NOT axiomatized.
-/
import Volar.Axioms

namespace Volar.Common

open Volar

/-- Byte-wise XOR (mirrors `Array::from_fn(|i| v[i] ^ a[i])`). -/
def xorBytes (a b : List UInt8) : List UInt8 := List.zipWith (· ^^^ ·) a b

/-- `double a = (H a, H a ⊕ a)`. (Noncomputable: `Hash` is an opaque external
    primitive with no executable definition.) -/
noncomputable def double (a : List UInt8) : List UInt8 × List UInt8 :=
  (Hash a, xorBytes (Hash a) a)

/-- The first output is the raw hash. -/
theorem double_fst (a : List UInt8) : (double a).1 = Hash a := rfl

/-- The second output is the hash XOR the input. -/
theorem double_snd (a : List UInt8) : (double a).2 = xorBytes (Hash a) a := rfl

/-- The PRG is deterministic: equal seeds give equal expansions. -/
theorem double_deterministic {a b : List UInt8} (h : a = b) : double a = double b := by
  rw [h]

end Volar.Common
