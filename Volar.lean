/-
Volar — parallel Lean library mirroring the Tier-3 cryptographic crates
`volar-primitives`, `volar-common`, and `volar-spec`, with machine-checked
theorems about their completeness and soundness invariants.

See docs/lean.md for the library map and theorem catalog, and
docs/reliability.md § AI Capability Tiers for the proving/axiom policy.
-/

-- External-primitive axiom whitelist (the ONLY axioms in the library):
import Volar.Axioms

-- volar-primitives (fully proven):
import Volar.Primitives.Field
import Volar.Primitives.Z3
import Volar.Primitives.Tropical

-- volar-common:
import Volar.Common.HashCommitment
import Volar.Common.LengthDoubling

-- volar-spec:
import Volar.Spec.Vole
import Volar.Spec.IdealCot
import Volar.Spec.Garble
