/-
The ONLY place external-primitive axioms may live.

Policy (see docs/lean.md and docs/reliability.md § AI Capability Tiers):
axioms are permitted SOLELY for the *functionality* of external primitives
that Volar does not implement itself — the Keccak/SHA-3 hash family.
Constructions Volar builds on top (hash commitments, length-doubling PRG,
VOLE, garbled circuits) must be *proven*, never axiomatized.
-/

namespace Volar

/-- Opaque external hash (Keccak/SHA-3 family), modeled by functionality only:
    a deterministic function of input bytes. This is the single trusted
    external primitive in the entire library. -/
axiom Hash : List UInt8 → List UInt8

end Volar
