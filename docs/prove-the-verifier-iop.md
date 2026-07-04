# Prove-the-Verifier: IOP-based (Merkle + Fiat–Shamir) Backend

> **Reliability:** the whole construction (`volar-iop`) is
> `@reliability: experimental` — **Tier 3**, needs cryptographic review
> before production trust. Agents: read
> [`agent-context/discipline.md`](agent-context/discipline.md) before
> touching anything on the ZK ↔ non-ZK boundary.

This document describes **prove-the-verifier**: after the pre-ZK passes and
the ZK weave, the VOLE **verifier** is itself a computation — a stream of
per-AND-gate checks `K_a·K_b + V̂ = K_c·Δ` plus a memory-consistency
accumulator boundary — and this backend produces a succinct,
non-interactive, re-checkable proof that every check held, **without a
final zkSNARK on the *inner* proof**: the inner VOLE proof already accounts
for zero-knowledge, so the outer proof only needs soundness over public/
committed data (gate MACs, the verifier-only secret Δ, the prover-sent
openings, and the memory hash) — there is no secret left to hide here.

The construction stays **native** to the VOLE's own field (`GF(2^k)`, no
cross-field embedding into an unrelated prime field) and is a genuine
"traditional IOP-based zkSNARK" in the sense of Ligero/Aurora/STARKs:
oracle messages are committed via Merkle tree, verifier randomness is
replaced by Fiat–Shamir, and the result is an actual succinct, self-
contained proof artifact rather than a native `O(|F|)` opening.

---

## 1. Two phases

### 1.1 Phase 1 — the per-gate fold (`volar_iop::fold`, O(1) memory)

A fixed-size accumulator `(W: 7 slots, E: 3 slots, u: 1 scalar)` is threaded
through the woven verifier's loop, updated once per AND gate via relaxed-R1CS
cross-term folding algebra (`crates/iop/volar-iop/src/fold.rs`) — the
accumulator's size never grows with gate count, which is what makes this
succinct regardless of circuit size and avoids any storage blowup.

The relation folded each round is `and_check_r1cs`'s original
3-constraint/7-slot shape, used **directly, natively** — because the fold
never leaves `GF(2^k)`, no bit-expansion gadget is needed to make the check
sound over a different field (a cross-field embedding would otherwise cost
~174 constraints per gate for a naive `GF(2^8)`-sized check; native folding
needs none of that).

**Field**: `crates/iop/volar-iop/src/field.rs` builds a binary tower field
`GF(2^8) → GF(2^16) → GF(2^32) → GF(2^64) → GF(2^128)` by repeated
quadratic extension (`F_{i+1} = F_i[x]/(x^2+x+β_i)`) on top of the VOLE's
own `GF(2^8)` (`volar_primitives::Galois`) — chosen over a single
degree-`m` extension or a binary-tower/additive-NTT (classic FRI)
construction specifically because a *quadratic* extension in characteristic
2 has a simple, textbook irreducibility criterion (trace = 1), avoiding both
an arbitrary-degree irreducibility test and additive-NTT machinery. See that
file's module doc for the full derivation and the three independent
correctness checks (multiplicative-group order, distributivity, defining
relation) run across all five tower levels.

**Per-gate challenges**: see § Honest scope below for the precise,
non-overclaimed soundness posture. `crates/compiler/volar-weaver/src/vole.rs`'s
`IopSink` threads the per-gate challenge `r_and_{k}: IopChallenge` through
the woven verifier's loop via the generic `VerifierTraceSink` weave-time
extension point.

**`IopLift`**: `T` (the VOLE's own field, e.g. `Galois`) is a different Rust
type from the fold's tower field (`Gf128`), so a lift is still needed — a
**canonical, characteristic-preserving ring embedding** (`T` sits at the
tower's base level, zero above), sound by construction (it's the literal
definition of a field extension containing its base field), not a
cryptographic design decision. See
`crates/iop/volar-verifier-iop-runtime/src/lib.rs`'s module doc.

### 1.2 Phase 2 — finalization (`volar_iop::ligero`)

Once Phase 1 produces the final, fixed-size `(W, E, u)` — plus the
memory-accumulator boundary `(mem_acc_in, mem_acc_out)`, § 1.3 — Phase 2
proves the whole message satisfies `and_check_r1cs`'s relaxed relation via
a one-shot Merkle+Fiat–Shamir argument:

1. **Reveal, don't hide.** Classic Ligero hides the witness and proves a
   *quadratic* constraint about hidden values using few revealed positions
   — real machinery, needed because the witness must stay secret. This
   proof doesn't need to hide anything (§1's "no secret left to hide"), so
   it reveals the message directly and checks the relation on the revealed
   values. The genuinely new IOP part is a standard **Reed–Solomon
   proximity test**: systematically encode the message into a codeword,
   Merkle-commit it, Fiat–Shamir-sample query positions, and check the
   opened positions are consistent with *some* degree-`<k` polynomial
   (interpolate from `k` of the queries, cross-check the rest) before
   trusting the recovered values.
2. This is the standard "encode + spot-check" IOP of proximity underlying
   Ligero/Aurora/FRI, simplified to a single round because the message
   never needs BaseFold/FRI-style recursive folding-in-half — that
   machinery only pays for itself on a *growing* object, and Phase 1
   already prevents the object from growing.
3. Merkle leaf/node hashing is domain-separated (SHA3-256, one-byte
   prefix) — `crates/iop/volar-iop/src/merkle.rs`.
4. Fiat–Shamir transcript: `crates/iop/volar-iop/src/transcript.rs`'s
   `IopTranscript`, modeled directly on `volar_spec::faest::transcript`'s
   `FaestTranscript` shape (running SHAKE128 sponge, non-destructive
   squeeze) but generic rather than FAEST-specific.

**Message shape is generic, not hardcoded**: `k`/`n`/`q` (message length,
codeword length, query count) are all computed from the actual message
length (`crates/iop/volar-iop/src/ligero.rs`'s `sizes` function), not fixed
constants — specifically so a future extension (more rows for an oracle/
action statement, a different-length memory accumulator) doesn't need this
module's machinery redesigned, only a longer message. The concrete sizing
formula (`n` = next power of two `≥ 2k`, `q = k + 10`) is a reasonable first
choice, not a derived-and-proven soundness bound — flagged in § Honest
scope.

### 1.3 The memory-accumulator boundary

`mem_acc_in`/`mem_acc_out` (each a `Vec<F>` of any length — a circuit with
no committed storage passes empty slices) are carried through the **same**
Phase 2 message as `(W, E, u)`, committed and revealed via the same
Merkle+Fiat–Shamir mechanism — no separate commitment scheme needed. This
is not a per-gate folding concern: the in-circuit memory-consistency check
itself (the multiset-hash, `docs/memory-checking.md`) costs zero AND gates
and is checked independently of prove-the-verifier; `mem_acc_in`/
`mem_acc_out` here are a one-shot **boundary attestation** — supplied
wholesale by the caller (`prove_verifier_iop`'s extra parameters, mirroring
how a memory boundary is handled on any prove-the-verifier construction:
never re-derived gate-by-gate, only committed once at finalization).

`verify_iop` takes an optional `expected_mem_acc: Option<(&[F], &[F])>` —
if given, it checks the proof's recovered memory boundary against the
caller's own expectation (e.g. values a continuation/linking mechanism
independently committed to). This is a genuine capability, not just a
pass-through: because the values are Merkle-committed and Fiat–Shamir-bound
*before* being revealed, a caller can trust that the revealed
`mem_acc_in`/`mem_acc_out` are exactly what the prover committed to, not a
value chosen after the fact to match an expectation.

---

## 2. API

In [`volar-iop`](../crates/iop/volar-iop/):

| Item | Role |
|---|---|
| `field::{Gf128, Ext, BetaOf, Field}` | the native tower field and its trait |
| `fold::{and_check_r1cs, gate_witness, cross_term_z, IopAccumulator, fold_gate}` | Phase 1 |
| `merkle::{MerkleTree, verify}` | generic Merkle commitment |
| `transcript::{IopTranscript, FromBytes}` | Fiat–Shamir |
| `ligero::{prove, verify, LigeroProof}` | Phase 2, generic message length |
| `prove_verifier_iop<Z: NonZk>(Tagged<Z, IopAccumulator>, mem_acc_in, mem_acc_out) -> Tagged<Transparent, IopProof>` | fold's terminal |
| `verify_iop(&Tagged<Transparent, IopProof>, expected_mem_acc: Option<(&[F], &[F])>) -> bool` | check the finalization proof, optionally against an expected memory boundary |

### Discipline safety (compile-time)

`prove_verifier_iop` is bound `where Z: NonZk` (implemented only for
`Transparent`), so a `Tagged<Zk, _>` accumulator cannot reach it — a
compile error, pinned by a `compile_fail` doctest in `verifier.rs`. See
`docs/agent-context/discipline.md`.

---

## 3. Build wiring

- **Compile-time — `volar-verifier-fold`**: `emit_verifier_c`/
  `emit_verifier_rust` are generic over any
  `Tagged<Transparent, IrModule<IrFunction>>` and never reference any
  sink's bare names, so an `IopSink`-woven module lowers through them
  unmodified. Confirmed by
  `crates/fold/volar-verifier-fold/tests/e2e_iop_verifier.rs`. There is
  deliberately no separate `volar-verifier-iop-fold` crate — it would have
  been empty.

- **Runtime — [`volar-verifier-iop-runtime`](../crates/iop/volar-verifier-iop-runtime/)**
  (depends only on `volar-iop`, `volar-spec`, `volar-discipline`, `std` —
  never `volar-compiler`/`volar-lir-codegen`/`volar-c-backend`/
  `volar-weaver`, keeping the compiler-crate dependency graph
  one-directional): `IopLift`, `IopChallenge`, `IopAccumulator`,
  `iop_accumulator_fresh`, `iop_fold_gate` (the bare names an
  `IopSink`-woven verifier links against), `prove_and_verify_iop` (the
  pipeline terminal, including the memory boundary), and `run_iop_verifier`
  (the "print → temp Cargo project → real `cargo`/`rustc`" harness).

---

## 4. Weave-time plugin: `IopSink`

`crates/compiler/volar-weaver/src/vole.rs`'s `IopSink` implements the
`VerifierTraceSink<P>` weave-time extension point
(`weave_vole_verifier_with_trace`) — a new sink implementation is the
entire integration surface needed for a new prove-the-verifier backend; no
weaver-internals changes were needed for this one.

---

## 5. Actions and RNG: no special handling needed

Tracing `weave_vole_verifier_inner`'s statement lowering
(`crates/compiler/volar-weaver/src/vole.rs`, the `BIrStmt::ActionBit`/
`BIrStmt::Rng` arms) shows both simply bind a pre-supplied Q-share wire
variable (`q_action_{k}_bit_{j}` / `action_{k}_bit_{j}` if public,
`q_rng_{r}`) — exactly like an ordinary circuit input. **Neither introduces
a new algebraic check at the verifier level**: their VOLE-authentication is
established the same way any wire's is, and the only thing Phase 1's fold
ever checks is the AND-gate relation these wires eventually feed into. So
`IopAccumulator`/`fold_gate` need no Action/RNG-specific variant — they
already handle any circuit that produces AND gates, regardless of where the
gates' input wires came from. This was verified by inspection before
writing this doc, not assumed.

## 6. Oracles: deferred

Oracle outputs are *also* just Q-share-bound wires at this weaving layer
(`q_oracle_{k}_bit_{j}`, same mechanism as Actions/RNG above) — in the
common case, this repo's existing oracle-lowering infrastructure
(`docs/external-primitives-plan.md`) resolves an oracle call to concrete
circuit gates before this stage, so by the time a circuit reaches
`weave_vole_verifier_with_trace` there is often no "oracle" left to treat
specially. Whether a *future*, not-yet-built mode (an oracle left opaque
through weaving, needing its own attested check in the finalization proof)
would need a new constraint type in Phase 1's constraint set is an open
question, deliberately left for a future document rather than speculated on
here — this document only covers what the current pipeline actually
produces.

---

## Honest scope

- **Tier 3 / needs cryptographic review**: the tower field's soundness
  parameters, the native-field fold algebra, the Merkle domain separation,
  the Fiat–Shamir absorb ordering in the finalization IOP, and the `k`→`n`/`q`
  sizing formulas (§1.2) — all a reasonable first choice, not a
  derived-and-proven soundness bound.
- **Per-gate Fiat–Shamir soundness (Phase 1) is a real open question, not
  fully closed.** Making the per-gate fold challenges non-interactively
  sound against a third-party verifier, without a recursive/IVC augmented
  circuit, is a genuinely hard problem — the same shape of gap
  `docs/vcb-ivc-folding.md` §5 flags for its own (unrelated) Nova-based
  continuation-bridge folding. This document's fully-buildable-without-
  recursion contribution is Phase 2 (the finalization IOP over the small,
  fixed-size final accumulator, plus the memory-boundary attestation,
  §1.3) — a single, one-shot statement, not a chain of per-gate ones. An
  interactive verifier (who supplies `r_i` themselves) sidesteps this
  entirely and is always a sound fallback.
- **Not done here**: a FRI/binary-tower-field upgrade of the Phase 2
  finalization IOP for even smaller proofs, if the Ligero-shaped
  construction's concrete parameters prove worse than wanted in practice.
  A streaming/incremental Merkle-tree construction is *not* a needed
  refinement here (unlike it would be for a monolithic whole-trace design)
  — Phase 2's statement is always small by construction.
- **Scope boundary**: this covers the AND-gate check
  (`and_check_r1cs`) and the memory-accumulator boundary (§1.3) — the same
  scope a `VerifierTrace`-shaped arithmetization frontend would need.
  §5/§6 above record why Actions/RNG need no extension and why Oracles are
  deliberately left open rather than assumed.
