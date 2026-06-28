# Side Tracking

Side is a per-value annotation system that tracks **which actor, party, or
role a value belongs to** — and therefore what cryptographic protection it
needs (ZK witness vs. statement, FHE plaintext vs. ciphertext, garbler vs.
evaluator). It is the sibling of [provenance](provenance.md): both are
generic per-node metadata carried by the same `Node<T, P>` wrapper, but they
answer different questions. Provenance answers "where did this value come
from" (debugging/integration); side answers "who does this value belong to"
(protection policy).

Side is **independent of** the `volar-discipline` typestate
(`Tagged<Zk/Transparent, T>`, see
[agent-context/discipline.md](agent-context/discipline.md)). Discipline is a
whole-module, compile-time boundary that stops mixing already-distinct
artifacts (a prover module with a verifier module). Side is a within-IR,
per-value concept that decides what an individual value *inside* one of
those modules needs. Neither system is layered on or derived from the other.

## How it works

Every IR node in Volar is (or can be) wrapped in `volar_ir_common::Node<T, P>`:

```rust
pub struct Node<T, P: Clone = ()> {
    pub kind: T,
    pub prov: P,
    pub side: Option<SideId>,
}
```

`SideId` (defined in the **`volar-side`** crate) is an opaque numeric
identifier — it carries no meaning of its own, exactly like `IRVarId` or
`StorageId`. A [`SideHandler`] implementation is the single place that
decides what a given side (or its absence) means:

```rust
pub trait SideHandler {
    type Protection;
    fn protection(&self, side: Option<SideId>) -> Self::Protection;
}
```

`volar-side` ships no ZK/FHE/garbling-specific vocabulary. Each consumer
defines its own `Protection` enum next to the code that uses it —
`VoleProtection { Witness, Statement }` in `volar-weaver::vole`,
`FheProtection { Cleartext, Encrypted }` in `volar-weaver::fhe` — and
implements `SideHandler<Protection = TheirEnum>` however suits them.
`UniformProtection<T>` (every side maps to the same fixed value) and
`TableProtection<T>` (an explicit `SideId → T` map with a default fallback)
are built-in handlers for the common cases.

### Which IR types carry a side

Every block-based IR's per-statement/per-value `Node` carries `.side`
alongside `.prov`:

| Level | Container | Node type |
|---|---|---|
| VAFFLE | `Module<P>` / `FuncBody<P>` (`vaffle`) | `Node<Value, P>` in `FuncBody::values` |
| Volar field-level IR | `IRBlocks<P>` / `IRBlock<P>` (`volar-ir`) | `Node<IRStmt, P>` in `IRBlock::stmts` |
| Boolar (boolean circuit) IR | `BIrBlocks<P>` / `BIrBlock<P>` (`volar-ir`) | `Node<BIrStmt, P>` in `BIrBlock::stmts` |
| Compiler IR (tree-shaped) | `IrModule<IrFunction<P>, P>` (`volar-compiler`) | `IrExpr<P> = Node<IrExprKind<P>, P>`, `IrStmt<P> = Node<IrStmtKind<P>, P>` — **every** subexpression, not just top-level statements |

This is a structural change from the older parallel-array design: each `Node`
carries its own `prov`/`side` directly, so there is no `stmt_provs: Vec<P>`
array to keep in sync with `stmts` by index, for either axis. (Older docs —
[provenance.md](provenance.md) and
[agent-context/provenance-pipeline.md](agent-context/provenance-pipeline.md)
— still describe the parallel-array shape and predate this change; treat
this document and the current source as authoritative on the `Node<T, P>`
shape.)

The tree-shaped compiler IR (`IrExpr`/`IrStmt`) is the one case where side
(and provenance) reaches *every* node, not just top-level statements — `Lit`,
`Var`, `Binary`, every variant carries its own `Node` wrapper now.

### Default propagation

A value *derived* from operands (a binary op, a field projection, a cast)
should usually inherit its operands' side rather than require an explicit
assignment at every single node. `volar_side::propagate` implements this
rule once, generically, for any consumer:

```rust
pub fn propagate(operands: &[Option<SideId>]) -> Option<SideId>
```

It returns the operands' common side if they agree (including "all
unassigned"), or `None` if they disagree — disagreement is left to the
caller/handler to resolve explicitly rather than guessed at. Only true
**introduction points** — literals/`Const`, function/closure params, oracle
and action call outputs — need an explicit `Some(SideId)` from the frontend
or weaver config; everything else can propagate for free.

### Attachment points for source/target languages

- **WASM source**: `WaffleImportConfig`/`WaffleImportKind`
  (`crates/ir/volar-vaffle-target/src/import_config.rs`) — `Oracle`/`Action`
  variants carry an optional `side: Option<SideId>`, populated into the
  lowered `Node<Value, P>::side` for that arena entry by
  `lower_waffle_module`/`lower_waffle_function`.
- **VAFFLE → Volar IR**: `lower_vaffle_to_ir`
  (`crates/ir/volar-vaffle-target/src/lower_to_ir.rs`) carries each
  `Node<Value, P>::side` through into the corresponding output `Node`.
- **LIR targets**: the `LirTarget<Prov>` trait has `set_side(&mut self, side:
  Option<SideId>)` (default no-op) alongside `set_prov`. `VolarIrTarget<P>`
  and `VaffleTarget` override it to tag the `Node` they're currently
  emitting.
- **Weaver introduction points with no IR node of their own**: a circuit
  input param (`BIrBlock::params` is a bare count, not individually
  `Node`-wrapped) or an action's output bit (doesn't exist as a node until
  the call is lowered) can't carry a side via the IR directly. These take an
  explicit side from a small assignment table passed to the weaving
  function — e.g. `VoleSideAssignments` in `volar-weaver::vole` — mirroring
  how `WaffleImportConfig` is the explicit-assignment point for WASM
  imports. This is the same "introduction points need an explicit
  assignment" rule as propagation, just applied at a point where the IR
  shape itself has no node to attach the side to.

### Preservation through IR-to-IR conversion

Any pass that copies a whole `Node` and only rewrites `.kind` preserves
`.prov`/`.side` automatically — this is the existing shape of nearly every
pass in `volar-ir-opt`, `volar-ir-virt`, and `volar-weaver`. No special
handling is needed unless a pass is deliberately re-deriving or merging
sides (e.g. `substitute_vaffle` keeping whichever side a substituted value
already carried from its origin module, since `SideId` is concrete rather
than per-module-generic, there's no analogous "dual side handler" needed
the way `DualProvenanceHandler<P, Q>` exists for provenance).

## Weaving integration (`volar-weaver`)

`VoleProtection` and the `weave_vole_prover_with_side`/
`weave_vole_verifier_with_side` entry points
(`crates/compiler/volar-weaver/src/vole.rs`) are the reference
implementation of Side replacing a hand-rolled config:

```rust
pub enum VoleProtection { Witness, Statement }

pub fn weave_vole_prover_with_side<P: Clone, SH>(
    circuit: &BIrBlocks<P>,
    name: &str,
    assignments: &VoleSideAssignments,
    side_handler: &SH,
) -> Tagged<Zk, IrModule<IrFunction>>
where
    SH: SideHandler<Protection = VoleProtection>,
```

Internally, a `VoleWitnessSource` trait abstracts "is this input/action
output public" so the weaving logic itself doesn't know or care whether the
answer comes from the legacy `ZkWitnessConfig` or from `VoleSideAssignments`
+ a `SideHandler` — both implement the trait, and
`weave_vole_prover_inner`/`weave_vole_verifier_inner` are generic over it.
`TableProtection<VoleProtection>` is a ready-made `side_handler` for the
common case of a small explicit `SideId → VoleProtection` map.

**Current scope** — this is the reference implementation, not a full
migration:

| Entry point | Side-based equivalent |
|---|---|
| `weave_vole_prover` / `weave_vole_prover_with_config` | ✅ `weave_vole_prover_with_side` |
| `weave_vole_verifier` / `weave_vole_verifier_with_config` | ✅ `weave_vole_verifier_with_side` |
| `weave_vole_prover_bounded` / `*_bounded_with_config` | ❌ not yet migrated |
| `weave_vole_verifier_bounded` / `*_bounded_with_config` | ❌ not yet migrated |
| `weave_vole_prover_ir` / `weave_vole_verifier_ir` | ❌ not yet migrated |
| `FheActionConfig` (FHE weaver) | ❌ vocabulary only (`FheProtection { Cleartext, Encrypted }`); no `weave_fhe_*_with_side` entry point yet — `FheActionConfig` is configured per-`FheScheme` (e.g. `TfheScheme::with_action_config`), not as a single top-level function parameter like `ZkWitnessConfig`, so the migration shape differs and is tracked as follow-up work |

`ZkWitnessConfig`, `ZkActionConfig`, and `FheActionConfig` are **not**
deleted, and `volar_ir::public::PublicSet` is still used by the FHE weaver's
`track_stmt_publicness` dataflow tracker. Deleting them is only safe once
every entry point that depends on them has a parity-tested side-based
replacement (see the table above) — doing it now would break the
not-yet-migrated entry points.

## Crate structure

| Crate | Role |
|---|---|
| `volar-side` | `SideId`, `SideHandler` trait, `propagate`, `UniformProtection`, `TableProtection`, `SideTable` (no-std, zero dependency on the rest of the workspace — designed to be extractable into a standalone crate) |
| `volar-ir-common` | `Node<T, P>` wrapper + `MapKind<P, Q>` trait, used by every IR in the workspace |
| `volar-vaffle-target` | `WaffleImportConfig` side assignment; `lower_vaffle_to_ir` side preservation |
| `volar-lir` | `LirTarget::set_side` |
| `volar-weaver` | `VoleProtection`, `VoleSideAssignments`, `weave_vole_*_with_side`; `FheProtection` vocabulary |

## Design notes

- **Never invented, same as provenance** — there is no way to synthesize a
  `SideId` from nothing; `Option<SideId>` is always either propagated,
  copied from a source node, or supplied explicitly at a true introduction
  point.
- **`side` is `Option<SideId>`, not a second generic parameter.** Unlike
  provenance's free-form `P`, a side is always "which of N parties owns
  this value" — a single opaque id is enough, so it doesn't need to be
  threaded through every IR type's generic signature the way `P` does.
- **No runtime effect on emitted source.** Side is compile-time/weaving-time
  metadata. Printers ignore it by default, exactly like provenance.
- **Extractable by design.** `volar-side` has zero dependencies on the rest
  of the workspace, so lifting it into a standalone crate for reuse by other
  multi-actor/multi-party systems is a follow-on, not a redesign.
