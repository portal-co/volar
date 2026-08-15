# Agent Context: ZK / non-ZK Proving Discipline

**Load this when:** touching any weaver, the folding crate
([`volar-fold`](../../crates/fold/volar-fold/)), the build pipeline, or anything
that produces, consumes, or moves an `IrModule` that represents a proof.

This brief **binds agents to the discipline system**. The discipline typestate is
a load-bearing safety boundary: it stops an agent — especially a lower-tier one —
from catastrophically mixing already-existing primitives (a zero-knowledge
prover with a non-ZK / transparent prover or a folding instance). The type system
enforces it; this document tells you **why you must not route around it**.

See also [`prove-the-verifier.md`](../prove-the-verifier.md) and the
`volar-ir` repo's `docs/provenance.md` (the discipline tag is orthogonal to
and composes with provenance `P`).

---

## The primitives you must not mix

| Marker (`volar_discipline`) | `IS_ZK` | Carries | Produced by |
|---|---|---|---|
| `Zk` | `true` | blinding MACs (`Vope.u`), Δ-coupling, a shared Fiat–Shamir transcript | `weave_vole_prover*`, `weave_faest_prover*` |
| `Transparent` | `false` | no ZK secrets — verifier-as-computation, folding instance, regular SNARK, garble/noop/fhe modules | `weave_vole_verifier*`, `weave_faest_verifier*`, `weave_evaluator/garbler/into_gc/eval_from_setup*`, `weave_noop*`, `weave_fhe_flat*` |

Mixing them is unsafe: revealing the prover's MAC blinding, leaking the
verifier-only secret Δ, or replaying one Fiat–Shamir transcript across
independent proofs each break soundness or zero-knowledge. The whole point of the
typestate is that these mistakes are **compile errors**, not runtime surprises.

## The mechanism

`Tagged<Z: ZkDiscipline, T>` wraps an artifact with a compile-time discipline.
`Tagged::seal` is the single audited construction point; `map` preserves
discipline; `into_inner` unwraps. `DynTagged::require::<Z>()` is the
runtime-checked recovery for genuinely dynamic boundaries. The subtrait `NonZk`
is implemented **only** for `Transparent`; folding and regular-SNARK entry points
bound `where Z: NonZk`, so a `Tagged<Zk, _>` cannot reach them.

## Rules (the binding)

1. **Weaver outputs are tagged — keep the tags truthful.** A prover is `Zk`; a
   verifier-as-a-computation is `Transparent` (the inner proof already accounts
   for ZK — see [`prove-the-verifier.md`](../prove-the-verifier.md)). Never seal a
   prover as `Transparent` or a verifier/garble/noop/fhe module as `Zk`. If you
   add a new weaver, tag its output by the table above; if you cannot justify the
   tag, stop and surface it.

2. **`into_inner()` is not an escape hatch across the boundary.** Use it only at a
   *local, same-discipline* boundary (e.g. handing a module to a printer or
   codegen that takes a raw `IrModule`). Never use it to push a `Zk` artifact into
   a non-ZK consumer, or to make a type error "go away." If a fold/SNARK API
   rejects your value, that rejection is correct — do not unwrap past it.

3. **Never weaken a `where Z: NonZk` bound.** `prove_verifier`,
   `compress_with_snark`, and every folding entry are gated on it deliberately.
   Do not replace it with a concrete `Transparent`, add a blanket impl of
   `NonZk`, or take a raw `IrModule`/`VerifierTrace` to dodge it.

4. **Passes generic over the tag thread it, never erase it.** A pass that does not
   inspect discipline stays `<Z: ZkDiscipline>` and forwards via `Tagged::map`.
   Do not collapse to a fixed tag for convenience.

5. **Dynamic boundaries use `DynTagged::require`, not `assert!` + unwrap.** When
   discipline is only known at runtime, recover the static tag through `require`
   and handle `DisciplineError`.

## Review note

The discipline boundary is a cross-cutting invariant that routine work can break
while wiring a backend consumer, adding a weaver variant, or fixing a build
error. The boundary itself (`volar-discipline`, the `NonZk` bounds, and the
verifier tag) is **load-bearing and cryptographically sensitive**: if a change
makes ZK and non-ZK artifacts assignable to one another, it is wrong. Record a
small reproducer and handoff rather than relaxing a tag or bound; retain the
paper/review evidence required by the pinnedness policy.

## Trip-wires (most common in review)

- A new `volar-fold` consumer that takes `IrModule`/`VerifierTrace` directly
  instead of `Tagged<Z, _>` with `Z: NonZk`. → Add the bound.
- `weave_*().into_inner()` feeding something that is *not* a printer/codegen
  sink. → Suspect a boundary violation.
- A test or pipeline that seals a module with a tag it copied from a nearby line
  without checking the role. → Verify prover⇒`Zk`, verifier/garble/noop/fhe⇒`Transparent`.
- `impl NonZk for …` anywhere other than the single `Transparent` impl. → Reject.
