# Agent Context: Side Tracking

**Load this when:** adding a `side`/`SideId` to a new IR container, writing
a `SideHandler` impl, replacing a hand-rolled publicness/witness config
(anything shaped like `ZkWitnessConfig`, `*ActionConfig`, `PublicSet`), or
wondering why a circuit input/action output needs an explicit side instead
of reading one off the IR.

See [`side.md`](../side.md) for the full design. This brief covers the
trip-wires.

## What Side is and isn't

Side answers "which actor/party does this value belong to" (ZK witness vs.
statement, FHE plaintext vs. ciphertext). It is **not** the
`volar-discipline` typestate (`Tagged<Zk/Transparent, T>`,
[discipline.md](discipline.md)) — that's a whole-module compile-time
boundary; side is a within-IR, per-value annotation. Don't conflate them:
a single `Tagged<Zk, IrModule<...>>` prover module can (and does) contain
both witness-side and statement-side values inside it.

Side is provenance's sibling, not its replacement. Both live on the same
`Node<T, P> { kind, prov, side: Option<SideId> }` wrapper used by every
block-based IR. If you're adding a new IR container or wrapper, give it
**both** fields together — there is no reason to add one without the other.

## Rules

1. **A `SideId`'s meaning is never invented.** `Option<SideId>` is always
   either propagated from operands (`volar_side::propagate`), copied
   verbatim from a source node a pass is transforming, or supplied
   explicitly at a true introduction point (a literal/param/oracle-or-action
   output, or — where the IR shape has no node to carry it, like a circuit
   input param — an explicit assignment table passed to the weaving
   function). Do not default a side to `Some(arbitrary_id)` to make a type
   error go away.

2. **`volar-side` itself stays policy-free.** It defines `SideId`,
   `SideHandler`, `propagate`, and the two generic handlers
   (`UniformProtection`, `TableProtection`) — and nothing else. Protection
   vocabulary (`VoleProtection`, `FheProtection`, or anything you add for a
   new weaver) belongs in the consumer crate, next to the code that uses it,
   not in `volar-side`. If you're tempted to add a ZK/FHE-specific variant
   to `volar-side`, stop — that breaks its zero-dependency, extractable
   design.

3. **A pass that doesn't inspect side just copies the `Node` wholesale.**
   Rewriting `.kind` while passing `.prov`/`.side` through unchanged is
   correct and requires no special-casing — this is the default behavior of
   nearly every pass already. Only special-case side in a pass that is
   itself about side resolution (a weaver's witness/statement decision) or
   about merging two differently-sided sources (and even then, check
   [`side.md`](../side.md)'s note on `substitute_vaffle` before reaching for
   a "dual side handler" — `SideId` is concrete, not per-module-generic, so
   there's usually nothing to merge).

4. **Don't delete `ZkWitnessConfig`/`ZkActionConfig`/`FheActionConfig`/
   `PublicSet` without checking `side.md`'s migration table first.** As of
   this writing only the non-bounded VOLE prover/verifier
   (`weave_vole_{prover,verifier}_with_side`) have a parity-tested
   side-based replacement. The bounded VOLE variants, the IR-based VOLE
   variants, and the entire FHE action-publicness path
   (`FheActionConfig`/`track_stmt_publicness`/`PublicSet`) still depend on
   the legacy types. Deleting them before those are migrated breaks the
   build — verify the table in `side.md` is fully ✅ first, and add a parity
   test (assert identical generated output between the legacy and
   side-based path for an equivalent assignment) for any new migration
   before considering its legacy counterpart removable.

5. **Don't add a `Default` bound to a function generic over arbitrary `P`
   provenance to make a side helper compile.** The provenance pipeline's
   "never invented" invariant deliberately has no `Default` bound on `P`
   anywhere (see
   [provenance-pipeline.md](provenance-pipeline.md)). The handful of
   existing exceptions — `volar-weaver`'s file-local `var`/`array_default`/
   `ir_expr` helpers, which construct genuinely-fresh scaffolding
   expressions with no source node at all (a bare variable reference, a
   default-value array) — are a narrow, deliberate, already-reviewed
   relaxation scoped to *that* category of zero-information introduction
   point. Don't extend the pattern to a node that *does* have a real source
   to inherit `.prov`/`.side` from; use `ir_expr_p`/`ir_stmt_p`-style
   explicit-provenance construction there instead.

## Trip-wires (most common in review)

- A new weaving function that takes `side: SideId` directly as a parameter
  instead of `Option<SideId>` or a `SideHandler` call. → A side's meaning
  must go through a handler; raw `SideId`s floating around untyped is what
  `volar-side` exists to replace.
- A `ZkActionConfig`/`FheActionConfig`-shaped struct added for a *new*
  weaver instead of a `YourProtection` enum + `SideHandler`. → Use the Side
  system from the start for new code; don't grow the thing Side 2 exists to
  retire.
- A pass touching `IrExpr`/`IrStmt` that drops `.side` while rewriting
  `.kind` (e.g. constructing a fresh `Node::new(new_kind, prov, None)` when
  the original had `Some(side)`). → Preserve `.side` unless you have a
  specific, documented reason to clear it.
- `var`/`array_default`-style `P: Clone + Default` helpers appearing outside
  `volar-weaver`'s existing scaffolding-construction helpers. → Suspect a
  shortcut around "never invented"; the source/target language extension
  points in `side.md` (`WaffleImportConfig`, `LirTarget::set_side`, weaver
  side-assignment tables) are the sanctioned ways to introduce a side.
