# Plan: Multi-Input PBS and Direct-IR TFHE Fusion

**Status:** partially implemented — the two-address-bit LUT-first direct-IR XOR path is implemented and remains opt-in; wider address tables are explicitly deferred to the generalized selector/encoding review
**Primary scope:** `crates/spec/volar-spec/src/tfhe.rs`, `crates/compiler/volar-weaver/src/fhe.rs`, and the direct `IRBlocks` path
**Related work:** [spec-static-shapes-plan.md](spec-static-shapes-plan.md), [fhe-weaver.md](fhe-weaver.md), [pipeline.md](pipeline.md), [agent-context/boolar-ir-conflicts.md](agent-context/boolar-ir-conflicts.md), [agent-context/ast-to-ast-weaving.md](agent-context/ast-to-ast-weaving.md)
**Review boundary:** TFHE table/phase/encoding semantics and fusion equivalence
need paper-bound cryptographic review. Isolated IR/planner work begins only
after that semantic contract is fixed and must retain generated-code
compile-and-run coverage.


## Merged-tree update — 2026-07-22

Evidence: `08d1d33`, the [TFHE validation handoff](handoffs/merge-recovery/tfhe-ginx-validation.md), and the [static-shapes handoff](handoffs/merge-recovery/static-shapes-and-monomorphization.md).

**Dependency/supersession notice:** LUT-first/XOR and every generalized-PBS
proposal are blocked behind the core audit in `tfhe-pbs-rework-plan.md`. The
implemented opt-in two-address-bit surface is a regression target, not
validation to enable broader work. The rejected arbitrary-table/three-bit
selector prototype remains rejected; never infer a wider table or generic PBS
claim from the current API.

**Policy update:** the legacy TFHE marker maps to Unpinned and Very unstable
until evidence supports a documented reclassification. This does not relax the
core-audit dependency or any non-goal below.

## 0. Decision record and safety status

This plan has two deliberately separable tracks:

1. **LUT-first negacyclic fusion (available prior art).** The implemented
   `TfheBootstrapTable` / `tfhe_lut_read` path already accepts a fixed bundle
   of encrypted address bits, validates a fixed Boolean table against the
   current negacyclic-image rule, produces a fixed test polynomial, performs
   one programmable bootstrap, and restores the standard Boolean encoding.
   A direct-IR planner can therefore discover and emit **layers of existing,
   validated negacyclic-table operations** before a broader PBS construction is
   selected. This track is useful even when it recognizes only a strict subset
   of Boolean cones.
2. **Generalized multi-input PBS.** A later reviewed construction may enlarge
   the representable table family, change the selector/phase construction,
   establish a broader error analysis, and subsume the bespoke AND/OR paths.
   That is cryptographic work requiring an exact reference selection and must
   not be treated as a prerequisite for the LUT-first optimizer.

Both tracks remain Unpinned, Very unstable cryptographic work during the
legacy-marker migration, not merely compiler optimization:

- A table must correctly encode a Boolean function in the negacyclic test
  polynomial.
- Combining several input ciphertexts into one PBS selector must preserve the
  intended input-domain indexing and leave enough error margin for the chosen
  parameters.
- The output must be restored to the one documented, composable ciphertext
  encoding before it can feed another operation.
- A compiler pass that replaces a Boolean cone with a PBS must prove semantic
  equivalence with that cone, preserve ordering/effects/provenance, and never
  silently cross a control-flow or external-effect boundary.

The current TFHE module retains the legacy `@reliability: experimental` and
`@experimental-status: unreviewed` markers, so the migration policy treats it
as Unpinned and Very unstable. Nothing in this plan establishes a secure
parameter set, a concrete security level, a noise bound, or a production-ready
bootstrap implementation. Any non-default pinnedness or stability decision
remains a human decision under [reliability.md](reliability.md).

### 0.1 What “multi-input PBS” and “layered negacyclic” mean here

The existing LUT substrate already implements a restricted multi-input PBS:
a fixed encrypted address bundle selects one fixed Boolean-table result in one
blind rotation, producing **one encrypted Boolean output**. Its current
representability rule is deliberately narrow: nonconstant logical table entries
at positions separated by half the logical domain must be complements. Under
the current standard `{0, Q4}` Boolean-wire encoding and selector arithmetic,
the exact target-facing subset is additionally limited to **at most two
address bits**: a third least-significant selector weight would be `Q4/2`,
which the current per-coefficient integer scaling does not apply as an exact
ciphertext-linear operation. `TfheBootstrapTable::new` therefore rejects
`ADDR_BITS > 2` as `InputEncodingUnsupported`; this is a semantic constraint,
not merely a ring-capacity or negacyclic-table constraint. Wider tables require
the reviewed generalized selector/encoding construction in Track 2.

A **layered negacyclic plan** is a compile-time-generated, topologically
ordered schedule of those existing table operations. Each layer request has a
fixed list of input wires, a fixed logical Boolean table, and a fixed
`TfheBootstrapTable<ADDR_BITS, TABLE_LEN, BIG_N>` shape. A layer output is
standard-encoded by `tfhe_lut_read` and can therefore be a later layer’s input.
The planner may use several such PBS calls for one source cone when one table
cannot express the whole cone; it must never pretend that several layers are
one bootstrap.

“Compile-time generated” here means that the host-side weaver/planner derives,
canonicalizes, validates, and records logical table entries and layer
connectivity while compiling the circuit. Generated target code receives a
fixed-shape, prevalidated table artifact or a linkable static factory for it;
it must not discover a table at runtime from a `Vec` or `slice.len()`. Whether
the target materializes the corresponding `[u32; BIG_N]` test polynomial by
Rust `const` evaluation, a generated literal, or a reviewed fixed-table helper
is a later target-support/optimization choice. All choices must be
observationally equivalent to the existing `TfheBootstrapTable::new` mapping.

The later **generalized multi-input PBS** track may expand this substrate to a
larger family of tables or selector encodings. It still produces one encrypted
Boolean output per standard PBS:

```text
(LweCiphertext; INPUTS) + typed Boolean table
    → selector/phase construction
    → one blind rotation + extraction + key switch
    → one standard-encoded LweCiphertext
```

This does **not** claim that one ordinary PBS produces several unrelated output
ciphertexts, shares a blind rotation between unrelated table roots, performs
SIMD packing, or amortizes a bootstrapping key switch across multiple output
values. Those require separate constructions, representations, and review.

The word **batch** below therefore means either (a) fusing one entire
currently-representable cone into one LUT/PBS, or (b) scheduling a deterministic
layered sequence of compatible LUT/PBS requests. A batch may contain multiple
independent roots only as an ordered emission schedule; each root still
consumes one PBS unless and until a separately reviewed multi-output
construction is introduced.

### 0.2 Why this is needed

The current implementation has three interlocking problems:

1. The current `TfheBootstrapTable` / `tfhe_lut_read` path already contains
   a fixed-shape, multi-address-bit negacyclic table representation, but it is
   not yet a first-class weaver substrate. It is intentionally restrictive:
   not every Boolean truth table satisfies its current negacyclic relation.
   The legacy AND/OR helpers separately implement threshold offsets around
   blind rotation, creating two semantic paths.
2. `tfhe_xor` is a raw LWE addition. It decrypts to XOR for a final observation,
   but `true XOR true` has torus phase `2·Q4`, not the standard `{0, Q4}`
   Boolean encoding. It is not composable with bootstrap/PBS operations. The
   weaver therefore expands every encrypted XOR into a three-bootstrap
   `OR(AND(a, NOT(b)), AND(NOT(a), b))` construction, even where the existing
   LUT table for XOR—or a layered negacyclic plan—would avoid that expansion.
3. The flat FHE path currently takes `IRBlocks`, lowers them to `BIrBlocks`,
   movfuscates Boolar IR, expands ORs, and emits binary gates. Boolar support
   is explicitly backlogged; it is Bit-only and loses the typed,
   width-aware/aggregate structure that direct Volar IR preserves. The direct
   Volar-IR path is the project direction for new work.

The immediate outcome is a direct `IRBlocks` planner that recognizes the
existing table scheme and emits validated negacyclic layers. The eventual
outcome is a single, auditable generalized PBS semantic API plus that planner,
without making the former an all-or-nothing prerequisite for the latter.

## 1. Normative invariants

These invariants are binding for any implementation derived from this plan.

### 1.1 Standard Boolean ciphertext invariant

Every public Boolean operation that returns `LweCiphertext<N_LWE>` for further
homomorphic use returns the documented standard Boolean encoding:

```text
false → phase near 0
true  → phase near Q4
```

with an explicitly reviewed decoding interval and error margin. A result that
only decrypts correctly under final decryption but is not safe as input to the
next PBS is **not** a Boolean-wire API result.

A raw torus-linear operation may exist only if all of the following hold:

- its name makes the non-standard phase semantics explicit (for example,
  `*_raw_phase_*`, never `tfhe_xor`);
- it is private unless a reviewed expert-facing API and safety documentation
  justify public exposure;
- no normal weaver path emits it as a Boolean wire; and
- it has no route into a PBS/table/gate operation without a reviewed
  normalization step.

### 1.2 Typed table-shape and layer invariant

All table-relevant dimensions are compile-time facts at target-facing calls:

- input arity;
- input order and significance;
- logical table domain size;
- ring degree;
- input and output encoding identifiers; and
- output arity (one in the initial construction).

No table shape may be derived from `slice.len()`, `Vec::len()`, an unbounded
runtime loop, or a run-time-selected encoding. A runtime *index* into a
fixed-size table remains distinct from a runtime-selected table shape.

For the **LUT-first track**, the only accepted table is one that the existing
`TfheBootstrapTable::new` validation accepts: a non-empty power-of-two logical
domain, exact `TABLE_LEN == 2^ADDR_BITS`, ring capacity, and the current
negacyclic relation (with explicit constant-table handling). The planner treats
validation failure as an ordinary *no-fuse* result; it must not replace the
current validator with an approximate symbolic rule.

A `NegacyclicLayerRequest` is the fixed compile-time request form for this
track: it records the root, ordered leaf wires, `[bool; TABLE_LEN]` logical
table, table type parameters, standard output encoding, and provenance. Its
layers form a DAG/topological schedule: any layer can consume only entry wires
or outputs of earlier layers. A request is neither a raw polynomial nor a
claim that arbitrary Boolean functions are representable in one existing LUT.

The generalized PBS track may add a different reviewed table/encoding
representation only with a new explicit descriptor and validation path. It
must not silently broaden the meaning of `TfheBootstrapTable` or make an old
validated table represent a different function.

### 1.3 Fusion equivalence and layering invariant

A planner may replace an IR region only when it can prove, mechanically from
an explicit semantic model, that:

1. the region is a pure Boolean cone with one selected root;
2. all cone leaves are available before the root;
3. no internal result is live outside the cone except through the selected
   root, or all such escaping results are separately retained without changing
   their values;
4. every leaf/input mapping, table index bit order, and truth-table output is
   fixed and recorded in the plan; and
5. the replacement output has standard Boolean encoding.

For LUT-first fusion, it must additionally prove that the exact logical table
passes the existing table validator. If the whole cone fails that check, the
planner may partition it into a topological sequence of smaller closed cones
only when *each* layer independently satisfies every rule above and later
layers consume the earlier layers’ standard-encoded outputs. It may otherwise
leave the cone to the ordinary direct-IR fallback.

The planner must use a conservative fallback: if a cone cannot be classified,
validated, or partitioned safely, it remains ordinary direct-IR TFHE lowering.
Failure to fuse is a performance outcome, never a correctness failure.

### 1.4 Effect, control-flow, and discipline invariant

PBS fusion is transparent FHE computation. It must preserve the existing
`Tagged<Transparent, _>` discipline of FHE outputs. It must not alter ZK
prover/verifier tags, use `into_inner()` to bypass a discipline boundary, or
weaken any `NonZk` constraint.

A fusable cone may not include, reorder, duplicate, or remove:

- `StorageRead`/`StorageWrite`;
- `ActionCall`/`ActionOutput`;
- `OracleCall`/`OracleOutput`;
- `Rng`; or
- a terminator, block parameter, or value that crosses a CFG edge.

The initial fusion unit is wholly within one `IRBlocks` block and one lane of a
value. Cross-block, storage-aware, action-aware, and multi-output fusion are
future work with separate proofs and tests.

## 2. Current behavior to retire

### 2.1 Existing LUT-first wrappers, then generalized wrappers

The first optimizer milestone must be built on the currently implemented
`TfheBootstrapTable` / `tfhe_lut_read` semantics, rather than waiting for a
new generalized PBS reference selection. It may add named **compile-time table
factories** for the subset of AND, OR, XOR, NOT-derived, or program-fragment
functions that the current validator accepts. Their output must be emitted
through `tfhe_lut_read`, including its existing centering and post-bootstrap
normalization.

`tfhe_gate_bootstrapping_and` and `tfhe_gate_bootstrapping_or` currently select
fixed threshold behavior by manually adjusting an LWE phase and using blind
rotation. The LUT-first plan does not assume every one of those functions is
already equivalent to a given table under every configuration: each wrapper
migration requires a differential/composability test against the existing
implementation. A wrapper that has not passed this gate remains a legacy
fallback.

After a generalized PBS construction is reviewed, the intended migration is:

```text
named AND table ─┐
named OR table  ─┼─> generalized typed PBS core ─> standard Boolean wire
named XOR table ─┘
```

At that point compatibility wrappers should be thin wrappers over one
canonical semantic core; they must not retain a second phase-offset
implementation. Whether their public names remain is an API decision made at
the migration gate, not an assumption of the LUT-first work.

### 2.2 XOR resolution

The desired public API is **composable XOR**, not the current ambiguous
“free XOR” Boolean-wire operation.

The proposed migration is:

1. Introduce `tfhe_pbs_bool` and a named two-input XOR table.
2. Make the public `tfhe_xor` (if retained) a composable typed-PBS wrapper with
   the same standard output invariant as AND and OR.
3. Rename or privatize the current addition implementation as a raw-phase
   primitive. It is not a Boolean gate in the public/weaver API.
4. Remove `TfheScheme::emit_xor` as the default binary Boolean operation after
   the cone planner owns Boolean-function selection. During transition, it may
   call the composable XOR PBS wrapper only.
5. Remove the weaver’s global XOR-to-`OR(AND, AND)` expansion. It is a
   correctness workaround with a large, avoidable cost and should not become
   the target semantics.

This deliberately gives up the misleading “free XOR” label for general
composable Boolean evaluation. A future reviewed representation that supports
free **and composable** XOR may add an explicitly different wire encoding and
conversion protocol; it must not silently change this one.

### 2.3 OR expansion

`expand_ors` is useful for AND/XOR/NOT-only circuit consumers, but it destroys
Boolean-function structure that a PBS table could encode directly. The direct
TFHE PBS path must run before `expand_ors`, and must not normalize OR through
De Morgan merely because an older backend needs that basis.

The legacy Boolar FHE entry point remains compatibility-only. Its byte-for-byte
behavior is preserved until callers migrate; the new PBS planner is not added
to Boolar by default.

## 3. Target architecture

### 3.1 Separate layers

The implementation must keep the following three layers separate.

| Layer | Responsibility | Must not decide |
|---|---|---|
| **TFHE semantic layer** | Selector phase, table representability, polynomial construction, PBS output normalization, input/error constraints | Which IR cones are profitable to fuse |
| **Fusion planner** | Discover pure Boolean cones, form a deterministic table request, choose conservative roots/leaves, maintain provenance | Torus offsets or cryptographic validity by intuition |
| **Direct-IR emitter** | Convert a validated plan to typed `IrExpr`/`IrStmt` calls and emit ordinary unfused IR operations for the remainder | Re-derive table contents or encode raw Rust strings |

The semantic layer owns all plaintext truth-table generation/validation. The
planner supplies a small Boolean expression/operation descriptor, not a raw
`[u32; BIG_N]` polynomial and not independently generated phase arithmetic.

### 3.2 Two-stage typed API surface

The plan deliberately distinguishes the **implemented LUT-first surface** from
the later generalized surface.

#### LUT-first: use the existing descriptor, add compile-time factories

```rust
// Existing target-facing execution primitive:
pub fn tfhe_lut_read<..., const ADDR_BITS: usize, const TABLE_LEN: usize>(
    inputs_lsb_first: &[LweCiphertext<N_LWE>; ADDR_BITS],
    table: &TfheBootstrapTable<ADDR_BITS, TABLE_LEN, BIG_N>,
    bk: &BootstrappingKey<...>,
) -> LweCiphertext<N_LWE>;

// Proposed reviewed factory form; exact spelling is deferred:
pub const fn /* or reviewed static factory */ negacyclic_bool_table<
    const ADDR_BITS: usize,
    const TABLE_LEN: usize,
    const BIG_N: usize,
>(logical: [bool; TABLE_LEN])
    -> Result<TfheBootstrapTable<ADDR_BITS, TABLE_LEN, BIG_N>, TfheBootstrapTableError>;
```

The factory must have exactly the existing validation/mapping semantics. If the
current parser/target subset cannot compile `const fn` construction, the
weaver may generate a static fixed logical-table artifact and call one linked,
reviewed table-construction helper. It may not duplicate polynomial-generation
math in the weaver. A target capability test decides which of these equivalent
materialization strategies is usable.

#### Later generalized PBS: a new explicit descriptor

Only after a selected reference and review establish a broader selector/table
construction may a new API be proposed, for example:

```rust
pub struct TfheBooleanPbsTable<
    const INPUTS: usize,
    const TABLE_LEN: usize,
    const BIG_N: usize,
> { /* private, separately validated representation */ }

pub fn tfhe_pbs_bool<..., const INPUTS: usize, const TABLE_LEN: usize>(
    inputs_lsb_first: &[LweCiphertext<N_LWE>; INPUTS],
    table: &TfheBooleanPbsTable<INPUTS, TABLE_LEN, BIG_N>,
    bk: &BootstrappingKey<...>,
) -> LweCiphertext<N_LWE>;
```

`TfheBooleanPbsTable` must not be a cosmetic rename that silently changes the
meaning of `TfheBootstrapTable`. It is a distinct reviewed representation when
and only when the generalized construction materially differs.

Both surfaces return exactly one Boolean ciphertext. Multi-bit output tables,
packed result encodings, and encrypted table contents are explicitly out of
scope.

### 3.3 LUT-first table contract, then generalized selector contract

The **LUT-first track** adopts the current implementation as its narrow
semantic contract, subject to cryptographic review of each new factory and fusion
mapping:

1. Input bits are least-significant first, matching `tfhe_lut_read`.
2. The logical domain is exactly `TABLE_LEN == 2^ADDR_BITS`.
3. Capacity is `TABLE_LEN <= 2 * BIG_N`, `BIG_N` is nonzero and a power of two.
4. Address width is at most two under the current standard `{0, Q4}` wire
   encoding; a wider selector is a new reviewed construction, not a LUT-first
   planner option.
5. Nonconstant tables obey the existing half-domain complement/negacyclic
   rule; constant functions use their explicit trivial-ciphertext case.
6. Test polynomial coefficients, centering offset, and post-bootstrap
   `Q4/2` normalization are **exactly** those of the reviewed existing
   `TfheBootstrapTable::new` and `tfhe_lut_read` implementation.
7. Every layer consumes standard `{0, Q4}` Boolean ciphertext inputs and
   returns the same standard encoding before it can feed the next layer.

The corresponding planner work is allowed to recognize only tables that pass
this existing contract. This is enough to build and benchmark layered
negacyclic fusion now; it does not assert that this family includes every
Boolean cone or every desirable AND/OR/XOR wrapper.

Before a **generalized** construction lands, a companion design note must fix
all of the following against a cited reference construction:

1. Any changed input-order convention.
2. The generalized selector formula, torus coefficients, affine offset,
   rounding, and relation to blind-rotation exponent.
3. The broader accepted domain and negacyclic image.
4. Test-polynomial construction, centering, signed output representation, and
   output normalization.
5. The error budget from every input, selector scaling/rounding, blind
   rotation, extraction, and key switching.
6. All parameter constraints on arity, table length, ring degree, and
   decomposition.

The existing LUT is therefore a regression oracle and a practical initial
representation—not a proof by extrapolation for a generalized selector.

### 3.4 Table construction and compile-time layering boundary

For LUT-first fusion there are three distinct construction/materialization
steps; keeping them separate makes future test-polynomial optimization safe to
reason about:

1. **Host-side planning:** derive a canonical fixed `[bool; TABLE_LEN]` table
   from a pure direct-IR cone, choose ordered leaves, and test it with the
   exact existing descriptor validator.
2. **Validated table artifact:** construct the existing `TfheBootstrapTable`
   using the one reviewed logical-table → negacyclic-polynomial mapping. This
   is where representability, constants, and standard output encoding are
   established.
3. **Target materialization:** embed or link the resulting fixed artifact so
   generated code calls `tfhe_lut_read` with a fixed ciphertext array. This
   may be compile-time polynomial generation, generated static data, or a
   fixed helper, but is never a target runtime dynamic table construction.

The planner owns step 1 only. The semantic layer owns step 2. The emitter owns
step 3 using typed IR expressions. No layer may receive an unchecked raw
`[u32; BIG_N]` polynomial from the planner.

A future optimization may improve the representation of a **layered** table
schedule—for example, deduplicating identical logical tables, folding a
compile-time polynomial generator, sharing immutable static descriptors, or
selecting a cheaper valid partition. It must preserve the exact validated
logical-table → polynomial mapping per layer. It cannot merge two layers merely
because their polynomials look similar, and it cannot cache ciphertext outputs
across evaluations.

For a later generalized representation there are likewise two paths, both
using its own reviewed validator:

- **Host/test path:** a fallible constructor accepts a fixed Boolean array and
  reports a precise descriptor error.
- **Weaver/generated-code path:** a reviewed table factory accepts a canonical
  planner descriptor. It must not emit `.unwrap()`, runtime `Vec`s, an
  unchecked raw polynomial, or its own table coefficient logic.

A planner request must include a stable canonical function identity, input
order, table bits, table length, output encoding, and layer dependencies. This
makes table cache keys deterministic and makes a generated artifact auditable.

## 4. Direct Volar-IR fusion path

### 4.1 Why direct `IRBlocks`, not Boolar

`docs/agent-context/boolar-ir-conflicts.md` establishes that Boolar IR is
backlogged and Bit-only. The direct Volar-IR path already preserves typed
values, provenance, actions, storage declarations, and the wider statement
forms needed by current and future pipelines. It also avoids this obsolete flat
route:

```text
IRBlocks → lower_ir_to_boolar → BIrBlocks → movfuscate_biir → expand_ors
```

The new flat TFHE PBS route should instead be architected as:

```text
IRBlocks
  → [optional reviewed direct movfuscation / circuit lowering in IR]
  → IRBlocks carrying typed `Stmt::Poly` / Const / Transmute / Merge / ...
  → direct-IR eligibility + Boolean-cone fusion planner
  → validated PBS requests + unfused direct-IR operations
  → IrModule or IrCfgModule
```

The exact placement relative to `movfuscate_ir` and `lower_to_circuit_ir`
needs a benchmark-backed choice. The first direct implementation may accept
an already single-block circuit `IRBlocks`; it must not route through
`lower_ir_to_boolar` merely to reuse the old flat emitter.

The direct path must retain a clear compatibility boundary:

- Existing `weave_fhe_flat_bir` remains for current Boolar callers.
- New PBS fusion is implemented for `IRBlocks` first.
- A Boolar shim may translate a narrow `BIrStmt::{And, Or, Xor, Not}` subset
  into the canonical Boolean-cone representation at width one only if a real
  caller needs it. It must not force new direct-IR features back into Boolar.

### 4.2 Initial eligible IR subset

The first planner may only fuse a root whose transitive cone consists of
recognized, same-block, single-bit pure values:

- `Stmt::Const` of `Bit`;
- `Stmt::Transmute` that preserves an eligible Bit;
- `Stmt::Poly` whose output and non-constant operands are Bit and whose GF(2)
  polynomial semantics can be converted exactly to the canonical Boolean
  expression representation; and
- optional aliases/copies introduced by the direct circuit lowering.

`Not` must be represented according to the actual `Poly`/constant semantics,
not assumed from a syntactic source name. `Or`, `Xor`, and `And` appear as
Boolean polynomial structure or may be represented by a small planner IR;
the planner must preserve their *function*, not their old binary lowering.

The initial planner rejects/falls back at:

- any multi-bit `IRType`, `Merge`, `Shuffle`, `Rol`, `Ror`, or `Splat`;
- an escaping intermediate result;
- storage, action, oracle, RNG, or a terminator;
- an input count/table domain beyond the reviewed table limits;
- a cone with an expression representation that exceeds a deterministic
  resource limit; or
- any unknown/non-exhaustive statement form (`_ =>` fallback).

Wide values are a later lane-local extension. It must prove that each lane is
independent and preserve `IRType`/`Shuffle` semantics; widening by silently
splitting into Boolar is prohibited.

### 4.3 Planner representation and deterministic policy

Introduce a private, typed planner IR such as:

```text
BoolConeExpr = Input(leaf_index) | Const(bool) | Not(expr)
             | Xor(expr, expr) | And(expr, expr) | Or(expr, expr)

NegacyclicLayerRequest {
    root: IRVarId,
    leaves_lsb_first: [IRVarId; ADDR_BITS],
    logical_table: [bool; TABLE_LEN],
    table_shape: TfheBootstrapTable<ADDR_BITS, TABLE_LEN, BIG_N>,
    output_encoding: StandardBoolean,
    depends_on: earlier layer roots only,
    provenance: root provenance plus recorded source range,
}

// Later, only after its own construction review:
GeneralPbsFusionRequest { /* analogous, generalized descriptor */ }
```

The actual Rust representation may use `Vec` only inside the host-side planner;
it must resolve to fixed concrete table/input shapes before emitted code. It
must not become target/source IR data or a runtime target allocation.

The LUT-first policy is deliberately simple and deterministic:

1. Traverse each block in topological SSA order.
2. Consider eligible values in a stable order (root variable ID, then stable
   leaf order).
3. Enumerate a bounded candidate cone, derive its complete Boolean truth table,
   and submit that exact table to the existing `TfheBootstrapTable` validator.
4. If it is valid, prefer the closed candidate that eliminates the most
   eligible work, with a fixed tie-breaker; emit one `NegacyclicLayerRequest`
   and mark its private internal nodes consumed.
5. If a whole candidate is invalid, try only deterministic smaller closed
   candidates under the same resource limits. This produces a topological
   **layered** schedule when a later valid request consumes an earlier layer’s
   result. It must never split a shared/live value or use an invalid table.
6. Values for which no validated request exists remain ordinary direct-IR
   operations, including legacy reviewed gates during migration.

This makes the initial optimizer valuable without overclaiming table
expressiveness. More aggressive DAG covering, common-subexpression sharing,
or cost-model search is deferred until the basic transformation is reviewed.

### 4.4 What “PBS-compatible operations occur together” means

For the LUT-first track, a cone is **negacyclic-table-compatible** only when
all of the following are true:

- It has one root output.
- It is Boolean and pure.
- Its leaves can be ordered into the current LUT address domain.
- Its exact derived logical table passes `TfheBootstrapTable` validation.
- Its leaves fit the current two-address-bit standard-wire selector; a larger
  address bundle is rejected pending the generalized selector/encoding review.
- Its root is immediately materializable with the existing standard output
  encoding.
- It contains no externally observable intermediate needed elsewhere.

The existing relation is intentionally not universal. With the current
**two-address-bit** construction, XOR’s table `[false, true, true, false]`
satisfies the half-domain complement relation, whereas ordinary two-input AND
and OR tables do not. `ADDR_BITS > 2` is additionally rejected by the current
standard-wire selector contract, before planner fusion is considered. The
planner must obtain these results by running the validator, not by keeping a
hand-maintained function allowlist.

Examples:

| Source-level region | LUT-first planner result | Later generalized result |
|---|---|---|
| `xor(a, b)` | one validated two-bit XOR LUT/PBS; standard output | one two-input XOR PBS |
| `not(xor(a,b))` if its table validates | one validated LUT/PBS | one two-input table PBS |
| `or(xor(a,b), c)` | one XOR LUT layer then a legacy OR/gate fallback; a three-address-bit table is rejected by the current standard-wire selector | one three-input table PBS when reviewed |
| `and(xor(a,b), xor(c,d))` | deterministic valid sublayers plus legacy fallback where needed; never claim one table without validation | one four-input table PBS if arity/capacity/error review permits |
| `tmp = xor(a,b); use(tmp, c)` in two roots | do not fuse through shared `tmp`; emit one composable XOR LUT/PBS for it or leave it unfused | same initial liveness rule |
| storage MUX, action output, branch condition across block boundary | no initial fusion | no initial fusion |

Thus the immediate win is concrete: a raw XOR no longer needs the weaver’s
three-bootstrap `OR(AND, AND)` workaround, and valid surrounding fragments can
be scheduled as validated LUT layers. The broader one-PBS-per-large-cone result
waits for the reference-reviewed generalized table scheme.

### 4.5 Existing FHE APIs and migration

Add a new explicit direct-IR entry point rather than changing `weave_fhe`’s
meaning invisibly. Conceptually:

```rust
weave_fhe_tfhe_pbs_ir_with_handler(
    blocks: &IRBlocks<P>,
    types: &IRTypes,
    config: &TfhePbsFusionConfig,
    name: &str,
    linkage: Option<&LinkageSystem>,
    storage: Option<&FheStorageConfig>,
    handler: &H,
) -> Tagged<Transparent, IrModule<...>>
```

The exact output may be `IrModule` for a flat/circuit input and `IrCfgModule`
for a future CFG-aware direct emitter. It must preserve `ProvenanceHandler`
semantics and use typed `IrExpr`/`IrStmt` nodes only.

Migration stages:

1. New API is opt-in, test-only, Unpinned, and Very unstable.
2. TFHE’s flat scheme selects it only after direct-IR coverage demonstrates
   equivalence and the old Boolar path remains available as a reference.
3. CFG integration is considered separately; no CFG control-flow fusion lands
   as a side effect of the flat implementation.
4. `weave_fhe_flat_bir` stays compatible for existing callers and cannot gain
   new behavior by accident.

## 5. Cryptographic reference and review requirements

### 5.1 References: current LUT baseline versus generalized PBS

The **LUT-first implementation** must document and preserve the current
`TfheBootstrapTable`/`tfhe_lut_read` algorithm as its baseline. It needs a
cryptographic semantic review of the existing mapping, exact input ordering,
negacyclic validation, centering, and output normalization; it does **not**
wait for a newly selected paper merely to plan and emit tables already accepted
by that implementation. Existing module citations to Chillotti, Gama,
Georgieva, and Izabachène, *TFHE: Fast Fully Homomorphic Encryption over the
Torus* (J. Cryptology, 2020) and Micciancio–Polyakov’s FHEW-like work (ePrint
2020/086) must be checked against the code rather than treated as a blanket
proof.

Before any **generalized** construction lands, its companion note must cite the
exact version, section, and algorithm/equation number of the selected source
for:

- the current GINX/FHEW-like blind rotation and sample extraction/key switch;
- programmable-bootstrap test-vector/table encoding;
- the generalized multi-input selector/phase construction; and
- any noise/error estimate used to bound that selector.

If the selected construction diverges from the source, the note must state the
algebraic divergence, why it is required, and why it preserves the specified
Boolean semantics. If that argument is not available, generalized PBS work
stops, while the constrained LUT-first optimizer remains limited to its
existing validated contract.

### 5.2 Required correctness artifacts

#### LUT-first optimizer gate

Before enabling the LUT-first direct-IR optimization, produce:

1. A plaintext evaluator for the canonical Boolean cone/table descriptor.
2. Exhaustive tests over the whole domain of every emitted table shape.
3. Differential evidence that `TfheBootstrapTable::new` and the selected
   target materialization produce the same logical entries/test polynomial
   contract; the planner itself never derives polynomial coefficients.
4. Chained-layer tests showing each `tfhe_lut_read` output feeds the next LUT
   and decrypts to the composed plaintext function.
5. A negative suite for every rejected descriptor and invalid layer boundary.
6. A composable-XOR test that consumes a LUT-produced XOR in a later PBS/LUT,
   replacing the raw-XOR assumption.

#### Generalized PBS gate

Before a generalized multi-input instance becomes enabled, additionally
produce:

1. A deterministic reference selector/test-polynomial generator independent of
   the production function where practical, or a line-by-line reference
   comparison against the cited algorithm.
2. Exhaustive tests over all `2^INPUTS` inputs for every new arity and named
   function/table shape.
3. Tests with deterministic nonzero noise sufficient to exercise the claimed
   margin, without presenting the small fixture as a security proof.
4. Differential tests against the legacy AND/OR behavior where wrappers are
   migrated.
5. Negative tests for mismatched key/table parameters, invalid generalized
   arity/domain, and unrepresentable table descriptors.

### 5.3 Required compiler/weaver artifacts

Before enabling fusion by default:

1. A non-cryptographic planner evaluator compares the original eligible
   `IRBlocks` cone with its requested Boolean table on every plaintext input.
2. A generated-code E2E test uses real spec functions and a synthetic
   direct-IR caller, then generates Rust/C as applicable, compiles it, runs it,
   and checks results—not only IR shape or function names.
3. A differential direct-IR test runs both the no-fusion and forced-fusion
   paths on equivalent small circuits and checks decryption against the
   plaintext circuit.
4. A negative planner test confirms it never fuses a cone with an escaping
   intermediate, effectful statement, multi-block dependency, or unsupported
   type.
5. A cost regression test counts **validated LUT-layer requests** (a hard
   structural invariant) and proves that a direct XOR root requests one LUT/PBS
   rather than the legacy three-bootstrap workaround. A separate test may
   assert that `OR(XOR(a,b),c)` becomes a valid layered schedule or a safe
   fallback under the current validator; it must not require one PBS unless the
   table validates.
6. A real direct-`IRBlocks` test demonstrates that the new route does not call
   `lower_ir_to_boolar`, `movfuscate_biir`, or `expand_ors`.
7. Generalized one-PBS cone-count assertions are added only after the
   generalized table construction is reviewed.

## 6. Implementation phases

### Phase 0A — Freeze and review the implemented LUT baseline

**Review:** cryptographic review for the semantic baseline; any contributor may prepare inventory and test-harness scaffolding.

- [ ] Record current deterministic vectors for AND, OR, raw XOR, NOT, CMUX,
      one-bit LUTs, and two-bit LUTs.
- [ ] Make the current XOR defect explicit in a regression test: its direct
      decryption behavior may hold while a downstream PBS/AND composition
      fails or is outside the standard-wire invariant.
- [ ] Write a focused LUT-baseline note that records the implementation’s exact
      address-bit order, table validation, logical-table → test-polynomial
      algorithm, centering, trivial constant case, and output normalization.
- [ ] Review that note against the current code and existing cited literature;
      identify unknowns explicitly rather than silently changing behavior.
- [ ] Add a host-side test that derives tables for candidate functions and
      compares planner acceptance directly with `TfheBootstrapTable::new`.

**Exit gate:** a reviewer can state precisely what the current LUT accepts,
what standard Boolean encoding it returns, and why a planner rejection is the
safe outcome for every other table.

### Phase 0B — Generalized PBS reference selection (independent/deferred)

**Review:** cryptographic review.

- [ ] Select and cite the exact reference algorithm for a generalized
      multi-input selector. Record equation/algorithm identifiers.
- [ ] Write the companion note containing the changed selector equation, table
      mapping, torus encoding, error budget, and non-goals.
- [ ] Define any larger arity limit from the reference/error analysis; do not
      choose it because a table happens to fit in memory.

**Exit gate:** this phase gates only the generalized construction. It does not
block the LUT-first direct-IR optimizer from using the constrained existing
validator.

### Phase 1 — LUT-first direct-IR negacyclic layers

**Review:** cryptographic review for existing-table semantics and output encoding; isolated planner/emitter work may proceed after that contract is fixed.

- [ ] Add the pure, deterministic same-block `IRBlocks` analyzer and
      `NegacyclicLayerRequest` plan object.
- [ ] Define `BoolConeExpr`, leaf ordering, canonical truth-table generation,
      resource limits, liveness/escape analysis, and no-fuse `_ =>` fallback.
- [ ] Ask the actual `TfheBootstrapTable` validator whether every candidate is
      representable; never reproduce its negacyclic predicate in the planner.
- [ ] Implement deterministic validated partitioning into topological layers,
      preserving standard output encoding between layers.
- [ ] Add compile-time/static target materialization for tables where the
      target subset supports it, with an equivalent linked fixed helper as a
      documented fallback. Do not synthesize polynomial arithmetic in weaver
      code.
- [x] Introduce an opt-in direct-IR LUT weave API that emits typed
      `tfhe_lut_read` calls and does not traverse Boolar. The shipped first
      surface is the fixed two-address-bit XOR table wrapper.
- [x] Route a representable XOR through one LUT/PBS and stop applying the
      three-bootstrap XOR workaround on that direct path. Unsupported roots
      retain their legacy reviewed fallback during migration.
- [x] Reject `ADDR_BITS > 2` for the current `{0, Q4}` standard-wire selector:
      three-bit syntactic negacyclic tables require a `Q4/2` LSB selector
      weight, which the current scaling path cannot apply exactly. Wider-table
      batching is deferred to Phase 2's separately reviewed selector/encoding
      construction.

**Exit gate:** a real direct-IR generated-code E2E test executes a validated
XOR table and at least one layered schedule; every layer matches the plaintext
cone/table oracle, and the route avoids `lower_ir_to_boolar`,
`movfuscate_biir`, and `expand_ors`.

### Phase 2 — Generalized typed Boolean PBS core (optional later track)

**Review:** cryptographic review.

- [ ] Refactor existing raw blind rotation behind one private implementation
      only if doing so preserves Phase-1 LUT behavior byte-for-byte or by
      differential ciphertext/decryption evidence.
- [ ] Introduce a separately explicit generalized table descriptor and
      `tfhe_pbs_bool` only after Phase 0B’s reference/error review.
- [ ] Implement named AND, OR, and XOR factories/wrappers over that reviewed
      core where their table semantics are now representable.
- [ ] Privatize or explicitly rename raw phase addition. No public Boolean
      function may expose non-composable XOR under an ordinary gate name.
- [ ] Preserve fixed compile-time shape and all key decomposition type
      parameters.

**Exit gate:** AND, OR, XOR, and chained Boolean results pass exhaustive and
composability tests through the generalized shared core, without regressing the
LUT-first path.

### Phase 3 — Broader cone fusion and cost policy

**Review:** cryptographic review for generalized fusion equivalence/cost policy; compiler machinery follows that contract.

- [ ] Permit a generalized table request only under the reviewed arity/error
      limit.
- [ ] Prefer one generalized valid fused root over a layered LUT schedule only
      when the policy is deterministic and semantically equivalent.
- [ ] Add canonical table caching keyed by table kind, function bits/input
      order/shape, and layer dependencies; cache descriptors only, never
      mutable ciphertext results.
- [ ] Measure PBS request counts and reference runtime on representative
      direct-IR/movfuscated inputs, distinguishing one-table fusion from
      multi-layer schedules.
- [ ] Keep a configuration switch for `Off`, `LutOnly`, `ReviewedGeneral`, and
      `ForceForTest`; a production-default policy requires review.

**Exit gate:** targeted cones reduce PBS requests where a valid representation
exists; no unsupported cone is fused; fallback output remains equivalent.

### Phase 4 — Broader direct-IR and AST-to-AST readiness

**Review:** compiler infrastructure plus cryptographic review of each new operation class and semantic lowering.

- [ ] Extend from Bit-only cones to independently provable lane-local wide
      values where direct `IRBlocks` retains width.
- [ ] Decide the integration point for `movfuscate_ir` and
      `lower_to_circuit_ir`; resolve their documented `JumpTable`, dynamic
      target, and multi-block limitations rather than falling back to Boolar.
- [ ] Add a static-shape eligibility diagnostic shared with the future
      AST-to-AST track.
- [ ] Design CFG-local fusion only after same-block SSA fusion is stable.
- [ ] Keep AST-to-AST weaving separate: it may consume the same
      `NegacyclicLayerRequest`/generalized request contracts and table factory,
      but it must not depend on LIR or invent a second cryptographic encoder.

**Exit gate:** the direct-IR contract is reusable by a future AST-to-AST
weaver, while Boolar remains a compatibility shim rather than a blocker.

### Phase 5 — External review and reclassification decision

**Review:** independent human cryptographic review; an AI contribution alone cannot complete this phase.

- [ ] Internal implementation review of `tfhe.rs`, planner, and weaver.
- [ ] Review the LUT-first mapping against its intended reference and review
      any generalized selector/error/encoding derivation separately.
- [ ] Integration review of generated Rust/C output and the Transparent
      discipline boundary.
- [ ] Decide whether the feature remains opt-in, Unpinned, and Very unstable,
      is revised, or is quarantined if a correctness flaw is found.
- [ ] Do not change TFHE pinnedness or stability without the required human action
      and current evidence record.

## 7. Review matrix

| Review phase | Reviewer requirement | Must answer |
|---|---|---|
| LUT baseline / layer review | Cryptographic reviewer familiar with `tfhe.rs` plus human engineer familiar with the weaver | Does every emitted request use the exact existing validator/mapping, preserve input order/centering/output encoding, and keep raw-polynomial construction out of the planner? |
| Generalized PBS correctness review | Cryptographer who reads the exact selected PBS source | Does the new selector/table/negacyclic/output construction match the cited algorithm and error conditions, without changing LUT-first semantics? |
| XOR/encoding review | Same or separate TFHE reviewer | Does every LUT/generalized XOR emitted by the weaver return a composable standard wire? Is raw-phase addition contained and accurately named? |
| Fusion equivalence review | Cryptographic reviewer plus compiler reviewer | Does every accepted cone map to exactly the requested validated table or layer schedule without crossing liveness/effect/CFG boundaries? |
| Backend integration review | Backend reviewer | Does generated Rust/LIR/C preserve concrete table/key dimensions, use typed IR nodes, and compile/run? |
| ZK-discipline review | Cryptographic reviewer | Does the new transparent FHE route preserve tags and avoid ZK/non-ZK escape hatches? |
| External/reclassification review | Human cryptographic reviewer | Are any deployment/security claims warranted, and is a pinnedness or stability change justified? |

## 8. Explicit non-goals

- No claim of a secure production parameter set or a new security level.
- No arbitrary `Vec`-backed, runtime-sized table interface for target code.
- No arbitrary-DAG fusion, cross-block fusion, or fusion across effects.
- No multi-output PBS, ciphertext packing/SIMD batching, bootstrap-key
  compression, FFT/NTT acceleration, or GPU parallelism.
- No automatic conversion of every Boolar caller to direct IR.
- No change to ZK/non-ZK discipline tags or folding/SNARK bounds.
- No AST-to-AST backend implementation in this plan; this plan only defines
  the table/fusion contract that such a backend may later consume.

## 9. Acceptance criteria

The **LUT-first direct-IR milestone** is acceptable only when all of the
following hold:

- [ ] Every emitted `NegacyclicLayerRequest` is accepted by the actual
      `TfheBootstrapTable` validation/mapping or is conservatively rejected;
      the planner never hand-implements a broader representability rule.
- [ ] Table logical bits, input order, compile-time materialization, centering,
      and output normalization are equivalent to the existing reviewed LUT
      behavior for each emitted layer.
- [ ] A direct `IRBlocks` path emits a composable XOR through one validated LUT
      PBS, removing the three-bootstrap XOR workaround on that path while
      retaining a safe fallback for invalid/non-LUT roots.
- [ ] At least one multi-layer schedule is proven equal to its plaintext cone
      and each layer’s output is successfully consumed by a later LUT/PBS.
- [ ] The direct LUT route does not require Boolar lowering or `expand_ors`.
- [ ] Every fused/layered region is pure, same-block, single-root, statically
      shaped, and verified against a plaintext table oracle; unsupported
      regions fall back without semantic change.
- [ ] Tests cover exhaustive small-domain tables, descriptor rejection, two
      distinct non-deployable parameter fixtures, generated Rust execution,
      and LIR/C execution where the static target surface supports it.
- [ ] The feature remains Unpinned, Very unstable, and opt-in unless a documented
      reclassification justifies otherwise; no ZK/non-ZK boundary is weakened.

The **generalized PBS milestone** additionally requires:

- [ ] An exact cited selector/table reference or derivation and a reviewed
      error/encoding argument for every expanded table family.
- [ ] AND, OR, XOR, and other migrated wrappers use one canonical generalized
      typed PBS semantic core or have a documented reviewed reason not to; no
      duplicate gate-specific phase semantics remain.
- [ ] Public Boolean XOR is composable, or raw non-composable addition is
      removed/private/explicitly hazardous and never emitted as a Boolean wire.
- [ ] A direct `IRBlocks` path can fuse a reviewed large cone such as
      `OR(XOR(a,b),c)` into one generalized PBS and executes correctly through
      real generated code.
- [ ] The generalized path does not regress the constrained LUT-first path or
      silently broaden `TfheBootstrapTable`’s established meaning.
