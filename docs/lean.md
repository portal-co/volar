# The Volar Lean Library

A parallel Lean 4 + Mathlib library that mirrors the three Tier-3 cryptographic
crates and carries **machine-checked theorems** about their completeness and
soundness invariants. The Lean kernel is a second correctness oracle alongside
the Rust test suite: a wrong proof simply fails to compile, so it cannot
silently corrupt the load-bearing Rust.

> This library is an **abstract semantic model**: it models the math and protocol
> semantics and proves properties about them. It deliberately does **not** port
> the Rust bit-twiddling implementations or prove a Rust↔Lean equivalence bridge.

## Building

```sh
lake exe cache get      # fetch prebuilt Mathlib oleans (fast; avoids recompiling Mathlib)
lake build Volar        # build the library + check every proof
```

Mathlib is pinned to `v4.30.0` in [`lakefile.toml`](../lakefile.toml), matching
[`lean-toolchain`](../lean-toolchain). CI runs `lake build` via
`.github/workflows/lean_action_ci.yml`.

## Library map

The `Volar/` source tree mirrors `crates/spec/`:

| Lean module | Mirrors | Status |
|---|---|---|
| `Volar/Axioms.lean` | external Keccak/SHA-3 primitive | the **only** axioms in the library |
| `Volar/Primitives/Field.lean` | `volar-primitives/src/lib.rs` (GF(2^n)) | **fully proven** |
| `Volar/Primitives/Z3.lean` | `volar-primitives` `Z3` (GF(3)) | **fully proven** |
| `Volar/Primitives/Tropical.lean` | `volar-primitives` `Tropical` | **fully proven** |
| `Volar/Common/HashCommitment.lean` | `volar-common/src/hash_commitment.rs` | completeness proven; binding conditional |
| `Volar/Common/LengthDoubling.lean` | `volar-common/src/length_doubling.rs` | structural facts proven |
| `Volar/Spec/Vole.lean` | `volar-spec/src/vole/` | completeness + binding proven |
| `Volar/Spec/IdealCot.lean` | `volar-spec/src/ot/ideal_cot.rs` | correlation proven |
| `Volar/Spec/Garble.lean` | `volar-spec/src/garble.rs` | free-XOR + AND correctness proven |

## The axiom whitelist (critical policy)

Axioms are permitted **solely** for the *functionality* of external primitives
Volar does not implement itself — the Keccak/SHA-3 hash family, declared in
[`Volar/Axioms.lean`](../Volar/Axioms.lean) as `Volar.Hash`.

**Forbidden as axioms** — these are constructions Volar builds and must be
*proven*: garbled-circuit correctness, VOLE soundness, commitment binding/hiding,
PRG security. Security properties that genuinely rest on a hardness assumption
(e.g. binding ⇐ collision-resistance) are stated as **conditional theorems**
taking the hardness predicate as a hypothesis — never as axioms.

> Concretely: `Volar/Spec/Garble.lean` proves garbled-AND correctness as a real
> theorem (the gate hash appears only as an opaque *function*, never an axiom).

## Theorem catalog

**`volar-primitives` (fully proven):**
- `Primitives.gfMul_comm/assoc/one`, `gf_left_distrib` — field laws of GF(2^n).
- `Primitives.gfMul_invert_cancel` — `a * a⁻¹ = 1` for `a ≠ 0`.
- `Primitives.gfInvert_zero` — the Rust `invert 0 = 0` convention.
- `Primitives.gf_invert_eq_pow` — **Itoh–Tsujii**: `a^(2^n − 2) = a⁻¹` (the
  exponent the Rust `gf_invert_*` raises to).
- `Primitives.z3_self_inverse`, `z3_mul_inv_cancel` — GF(3) inversion.
- `Primitives.trop_*` — tropical semiring laws.

**`volar-common`:**
- `Common.validate_commit` — **completeness** of hash commitments.
- `Common.commit_binding` — **binding**, conditional on a hash collision.
- `Common.double_*` — length-doubling PRG structure/determinism.

**`volar-spec`:**
- `Spec.honest_relation` — VOLE **completeness**.
- `Spec.vole_binding` — VOLE **soundness** (the relation pins a unique value).
- `Spec.cot_correlation/false/true` — ideal C-OT correlation.
- `Spec.free_xor_correct`, `Spec.garbled_and_correct` — garbled-circuit
  **correctness** (proven, not axiomatized).

## Auditing axiom usage

Every theorem must depend only on the whitelisted hash axiom (where relevant)
plus Lean/Mathlib's standard axioms (`propext`, `Classical.choice`, `Quot.sound`):

```sh
grep -rn '^axiom\|^  axiom\| axiom ' Volar/      # only Volar/Axioms.lean may match
```

In an editor or via the `lean-lsp` MCP, use `#print axioms <name>` (or
`lean_verify Volar.…`) on any theorem to confirm its axiom dependencies. The
primitives theorems must show **no `sorryAx` and no `Volar.Hash`**.

## Who may edit this library (tier policy)

See [reliability.md § AI Capability Tiers](reliability.md#ai-capability-tiers).
In short: **filling in or editing a proof body is Tier 1** (the kernel checks
it), while **adding/changing an axiom or a theorem *statement* about Tier-3
content matches the underlying content tier**, because a statement or axiom is a
trust assertion the kernel does not validate.
