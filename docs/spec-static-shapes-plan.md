# Plan: Static, Security-Parameterized `volar-spec`

**Status:** partially implemented — TFHE parameter binding and fixed-shape boolean LUTs landed; broader protocol audit and multi-input PBS remain proposed
**Primary scope:** `crates/spec/volar-spec/`
**Related work:** [`tfhe-multi-input-pbs-weaver-plan.md`](tfhe-multi-input-pbs-weaver-plan.md), [`agent-context/ast-to-ast-weaving.md`](agent-context/ast-to-ast-weaving.md), [`fhe-weaver.md`](fhe-weaver.md), [`lir-lowering-monomorphization-plan.md`](lir-lowering-monomorphization-plan.md)
**Review boundary:** cryptographic-spec API, parameter-binding, and PBS-semantic changes require paper-bound cryptographic review. Mechanical inventory, test harnesses, and compiler support still need reproducible behavior and generated-code evidence. No parameter set may be presented as secure without independent human cryptographic review.

## Merged-tree update — 2026-07-22

Evidence: `08d1d33`, [TFHE validation handoff](handoffs/merge-recovery/tfhe-ginx-validation.md), and [static-shapes handoff](handoffs/merge-recovery/static-shapes-and-monomorphization.md).

TFHE decomposition bases and fixed-shape LUT inputs/tables are static, and the
repaired focused `cargo test -p volar-spec` suite passes. Track B is gated by
`tfhe-pbs-rework-plan.md`; do not treat static shapes as a PBS/security result.
The dynamic generated crate has a pre-existing generated associated-type parse
failure for `cargo fmt --check`, and the LIR/C widening ring stops at unresolved
const parameter `L` in `encrypt_branch`. Neither backend remains verified for
these target-facing static paths. Under the pinnedness/stability policy, the
legacy TFHE marker maps to Unpinned and Very unstable until evidence supports a
documented reclassification.

## Implementation status

Implemented in the first vertical slice:

- TFHE bootstrapping and key-switching decomposition bases are part of the
  const-generic `BootstrappingKey` / `KeySwitchingKey` identity; the TFHE
  weaver carries the same six concrete parameters through emitted functions.
- `TfheBootstrapTable<ADDR_BITS, TABLE_LEN, BIG_N>` validates fixed boolean
  LUT shape/negacyclic compatibility and `tfhe_lut_read` consumes a fixed array
  of encrypted address bits rather than runtime slices.
- TFHE tests cover two distinct small, noiseless configurations, fixed one- and
  two-bit tables, constant tables, invalid descriptors, and the generated FHE
  execution path. The fixture parameters remain explicitly non-deployable.
- VOLE bridge bit-packing now takes paired fixed arrays sharing one
  `BITS` const parameter, removing its runtime equal-length assertion.

Still proposed: a reviewed multi-input PBS encoding, converting the remaining
fixed-shape FAEST/spec APIs, static-shape eligibility diagnostics for an
AST-to-AST target, and the full dynamic-mirror support matrix. In particular,
the dynamic generator parses the new TFHE surface and forwards its const
parameters, but the generated dynamic crate has pre-existing compilation gaps;
this change does not claim dynamic/LIR table construction support.

## Goal

Make `volar-spec` substantially more useful as a statically shaped source
language for Rust, LIR-backed targets, and a future direct AST-to-AST weaver.
The refactor has two linked objectives:

1. **Security-parameter genericity.** Protocol dimensions and numerical
   configuration that affect ciphertext/key layout, decomposition, noise, or
   correctness margins must be explicit compile-time parameters rather than
   being hidden in a particular correctness-test configuration or carried as
   runtime values. A test configuration remains deliberately small and
   explicitly non-secure; a production parameter selection is a reviewed,
   named instantiation rather than an undocumented collection of literals.
2. **Static shapes by default.** Replace `Vec` and slice use where their length
   is part of a protocol, circuit, key, encrypted-input, or table shape with
   arrays or fixed-size wrappers. This gives Rust monomorphization, LIR, C, and
   a future AST-to-AST weaver concrete storage layouts and bounded loops before
   code generation.

The immediate proof point is TFHE. Its main algebraic types carry the LWE/RLWE
dimensions and decomposition levels as const generics, and the implemented
slice extends that to decomposition bases and fixed table/input shapes. The
remaining audit covers the rest of the spec crate, especially protocol-shaped
slices in VOLE/FAEST, without forcing dynamically sized messages and
transcripts into an artificial fixed-size API.

This work prepares two later tracks without implementing either in this plan:

- **AST-to-AST weaving.** A direct weaver cannot reasonably lower a protocol
  body whose allocation and iteration bounds depend on runtime `len()` values.
  Its usable spec subset must expose the needed dimensions in types and const
  arguments.
- **Optimized TFHE bootstrapping.** A future bootstrap may combine multiple
  encrypted inputs and select among larger custom tables encoding compatible
  program segments, rather than exposing only binary AND/OR helpers. Static
  input/table dimensions allow each table layout to be checked, specialized,
  and emitted by Rust, LIR, and an AST-to-AST target.

## Current state

### What is already static

The TFHE core represents the largest layout-bearing dimensions with const
generics:

```rust
LweCiphertext<const N_LWE: usize>
RlweCiphertext<const BIG_N: usize>
RgswCiphertext<const BIG_N: usize, const BS_ELL: usize>
BootstrappingKey<const N_LWE: usize, const BIG_N: usize,
                 const BS_ELL: usize, const KS_ELL: usize>
```

Keys, ciphertext masks, RGSW rows, decomposition outputs, test polynomials,
and polynomial arithmetic use fixed arrays. This is the desired baseline:
these dimensions participate in storage layout and loop bounds, so a target
can monomorphize them.

### Remaining TFHE work

The initial TFHE slice deliberately leaves these concerns for a later reviewed
phase:

- Noise parameters are still explicit key-generation/encryption values; a
  reviewed production parameter-set representation must decide which belong in
  type identity.
- The low-level `tfhe_programmable_bootstrap` still accepts a raw fixed
  polynomial for reviewed specialized callers. Logical boolean tables should
  use `TfheBootstrapTable`; richer output encodings remain future work.
- The existing fixed table API is also a constrained, usable negacyclic PBS
  substrate: a future direct-IR optimizer may plan and emit compile-time fixed
  layers accepted by `TfheBootstrapTable`/`tfhe_lut_read` before a broader
  multi-input selector construction is selected. The layered contract and its
  separate generalized-PBS track are specified in
  [`tfhe-multi-input-pbs-weaver-plan.md`](tfhe-multi-input-pbs-weaver-plan.md).

The spec crate also retains dynamic collections where their role differs:

- **Likely fixed-shape protocol data:** `vole::bridge::{vope_bitpack,
  q_bitpack}` take paired slices; several FAEST aggregation/key-material paths
  own `Vec`s; and TFHE LUT input/table arguments above are directly
  layout-bearing.
- **Possibly variable external data:** hash/commitment input byte streams,
  transcript squeeze outputs, signatures or proofs whose size is deliberately
  negotiated, and test generators. A slice or `Vec` is appropriate until the
  calling protocol supplies a fixed dimension.
- **Tests:** proptest's dynamic vectors and host-only diagnostics do not by
  themselves block code generation. They may remain in `#[cfg(test)]`, though
  static test fixtures should be used for target-facing test coverage.

The inventory phase must classify every occurrence; it must not treat every
`&[T]` as a defect.

## Scope, non-goals, and boundaries

### In scope

- Establish a crate-wide vocabulary and audit for **security parameters**,
  **algorithmic constants**, **test fixtures**, and **runtime public inputs**.
- Make layout-affecting TFHE parameters explicit compile-time arguments and
  make a reviewed parameter bundle ergonomic without hiding its values.
- Add fixed-shape wrappers/APIs for TFHE bootstrap inputs and tables.
- Convert spec APIs whose slice/`Vec` length is semantically a fixed protocol
  dimension to const generic arrays or equivalent transparent fixed-size
  containers.
- Supply tests that exercise more than one concrete small configuration and
  lower representative spec functions through the real compilation path.
- Define the validation boundary necessary before table/program-segment
  specialization can be trusted by a future weaver.

### Explicit non-goals

- This is **not** a claim that the current suggested TFHE numbers provide a
  particular security level. Parameter selection, noise analysis, modulus
  switching, and error bounds require independent cryptographic review.
- Do not convert all byte-oriented APIs to arrays. Hash input, transcript
  absorption/squeezing, serialized messages, and intentionally variable proof
  artifacts may retain slices/`Vec`s.
- Do not introduce heap allocation merely to hide a static shape. The target
  subset should use arrays or transparent fixed-size structures; `Vec<T>` is
  not an acceptable replacement for a type-level length.
- Do not implement FFT/NTT acceleration, packing, key compression, a new
  bootstrap construction, or AST-to-AST weaving here. This plan creates the
  interfaces and invariants those efforts need.
- Do not silently make the test dimensions a production default. No public
  `Default` implementation should select a cryptographic parameter set.
- Do not rely on unstable Rust generic-const-expression features or associated
  const expressions in array lengths unless the compiler/parser/LIR support
  matrix explicitly accepts them. The initial representation must work in the
  supported total Rust subset.

## Terminology and invariants

### Four classes of numbers

Every literal or configuration field in `volar-spec` must be assigned one of
these classes in documentation and API names:

| Class | Examples | Representation rule |
|---|---|---|
| **Security/layout parameter** | LWE/RLWE dimensions, decomposition levels/base logs, noise distribution/bound, protocol repetition count | Explicit const generic or an immutable named parameter-set component that resolves to const values before lowering. |
| **Algorithmic constant** | Torus word width, `Q4`, AES block width, a reference-defined domain separator | A documented module constant; do not genericize merely for uniformity. |
| **Protocol shape** | bootstrap input arity, logical LUT entries, VOLE lane count, fixed proof field count | Const generic / fixed-size wrapper when the caller's protocol fixes it. |
| **Runtime public input** | absorbed byte message, serialized record stream, negotiated external payload | Slice/iterator/`Vec` is permitted; its dynamic length must not determine an emitted static layout. |

A correctness-only test tuple is a **test fixture**, never a security
parameter set. It belongs in a test support module with a name such as
`NoiselessTestParams`, and its documentation must say which real-world
security properties it does *not* model.

### Static-shape invariant

For any function intended to be parsed, woven, and lowered, all lengths that
control any of the following must be statically resolvable at the call site:

- stack/struct/array layout;
- loop trip count or unrolled iteration count;
- encrypted input arity, output arity, and table width;
- key, ciphertext, polynomial, gadget, or protocol-message field count; and
- an index calculation that chooses a member of a fixed protocol/table shape.

A runtime index into a known fixed-size array is acceptable when the target
supports it. A runtime-derived *length* that creates a layout, changes a loop
bound, or selects a different table encoding is not part of the static-spec
subset. Such a function must either receive a fixed shape, stay on a separate
dynamic-only API, or be redesigned before AST-to-AST weaving uses it.

### Parameter-binding invariant

Values that affect the interpretation of a TFHE key or ciphertext must agree
at compile time across all participants in an operation. In particular, a
key generated for one `(N_LWE, BIG_N, BS_ELL, KS_ELL, BS_BG_LOG, KS_BG_LOG)`
configuration must not type-check as an input to another configuration. Noise
parameters used only during key generation/encryption must nevertheless be
carried in the named parameter-set documentation and test evidence; whether
they need a type-level role is decided by the parameter audit and security
review, not by convenience.

## Proposed design

### 1. Make an explicit, flat TFHE parameter surface

Use a single canonical ordered list of TFHE const parameters for every type
or operation whose representation depends on them. The exact names may change
in implementation, but the first implementation should be equivalent to:

```rust
// Dimensions and decomposition layout.
const N_LWE: usize,
const BIG_N: usize,
const BS_ELL: usize,
const KS_ELL: usize,

// Decomposition bases are parameter-set facts, not runtime fields.
const BS_BG_LOG: u32,
const KS_BG_LOG: u32,
```

For example, `BootstrappingKey` and its nested key types should be parameterized
so a mismatched decomposition base cannot be paired with the key accidentally.
`blind_rotate`, `external_product`, `poly_decompose`, `key_switch`, and
key-generation APIs receive the same static values through their type/function
parameters, replacing stored `u32` base-log fields where no dynamic behavior
is intended.

Keep per-call randomness and messages as values. Treat noise configuration
carefully:

- If a noise bound/distribution selects a different cryptographic scheme or is
  guaranteed uniform for a parameter set, make it explicit in the named
  parameter-set definition and in key-generation/encryption APIs. It may be a
  const argument once this is supported by all target paths.
- If the reference API intentionally permits caller-selected testing noise,
  preserve that choice only in a clearly marked testing/experimental API.
  Production-facing key generation must not make an arbitrary runtime `u32`
  appear equivalent to a reviewed parameter set.

Do **not** begin with a trait whose associated constants appear in array
lengths, e.g. `[u32; P::BIG_N]`, unless the supported Rust/compiler/LIR subset
can fully parse, specialize, and emit it. Generic associated-const expressions
are a portability risk. The conservative source representation is flat const
generics; ergonomic named aliases or zero-sized markers can be added only when
they erase to the same concrete flat instantiation before lowering.

A named parameter declaration should publish, rather than conceal, its values
and provenance. Conceptually:

```rust
/// A reviewed named selection; not a `Default`.
pub mod parameters {
    pub type NoiselessTest = /* explicit small const instantiation */;
    // Future: a cited, independently reviewed parameter selection.
}
```

The actual API must avoid a type alias that loses the constants required by
LIR's monomorphization planner. A target-facing call must still expose a fully
concrete specialization.

### 2. Represent a programmable bootstrap's shape and encoding in types

Split raw blind rotation from public programmable-bootstrap APIs:

1. **Internal primitive:** a private, fixed-polynomial blind-rotation helper
   continues to consume `[u32; BIG_N]`. It has no claim about the logical
   table represented by that polynomial.
2. **Typed table builder:** introduce a fixed-size table description whose
   logical table length, input arity, ring degree, signed-output convention,
   and encoding policy are all explicit. At minimum this needs const generics
   equivalent to `INPUTS`, `TABLE_LEN`, and `BIG_N` and a transparent array
   payload; it may also need an output-width parameter when multi-bit results
   are introduced.
3. **Typed bootstrap entry point:** receive `&[LweCiphertext<N_LWE>; INPUTS]`
   (or by-value array where ownership is preferable), a table with the matching
   `INPUTS/TABLE_LEN/BIG_N`, and a matching bootstrapping key. It must not
   compute its shape from `slice.len()`.

The initial public replacement for `tfhe_lut_read` should therefore look
conceptually like:

```rust
fn tfhe_lut_read<
    const N_LWE: usize, const BIG_N: usize,
    const BS_ELL: usize, const KS_ELL: usize,
    const BS_BG_LOG: u32, const KS_BG_LOG: u32,
    const ADDR_BITS: usize, const TABLE_LEN: usize,
>(
    addr_bits: &[LweCiphertext<N_LWE>; ADDR_BITS],
    table: &TfheBootstrapTable<ADDR_BITS, TABLE_LEN, BIG_N>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL, BS_BG_LOG, KS_BG_LOG>,
) -> LweCiphertext<N_LWE>;
```

This is a shape sketch, not a settled encoding. It intentionally rejects the
old API's ambiguity: a `TABLE_LEN` is neither inferred from a slice nor padded
silently at runtime.

### 3. Make representability a checked table-construction contract

A table for a negacyclic PBS is not an arbitrary truth table. Its capacity,
logical domain, constant-function handling, signed encoding, centering, and
negacyclic compatibility determine whether it has a sound/correct polynomial
representation. These constraints must not remain implicit in a helper that
accepts arbitrary slices.

Define a construction/validation boundary with these rules:

- `TfheBootstrapTable` stores either the logical fixed table plus its
  precomputed `[u32; BIG_N]` polynomial, or only the polynomial with a
  separately retained fixed-shape descriptor. Decide based on whether the
  weaver needs the logical table for provenance/debugging.
- Table construction validates all size and encoding preconditions before a
  table can enter the bootstrap operation. Validation must use fixed const
  dimensions; where stable Rust cannot express a predicate in the type system,
  use a fallible constructor or a compiler/weaver preflight diagnostic, never
  an unchecked internal assumption.
- Constant functions remain an explicit representation case, because they
  cannot be represented by one ordinary negacyclic blind rotation. The typed
  API must encode whether it returns a trivial ciphertext rather than making
  this a hidden branch based on a runtime slice scan.
- A future weaver may only construct tables through this validated interface.
  It must not synthesize raw test-polynomial arrays from program fragments
  independently.
- A table specifies its output encoding (initially the existing boolean
  `{0, Q4}` convention after its required offset). Multi-valued or packed
  outputs require a separate reviewed descriptor and tests; they must not be
  smuggled into `bool` table entries.

This gives the later optimizer a clear route to combine inputs: it chooses a
fixed `INPUTS`, constructs a compatible segment table, proves/validates the
segment's input-domain and output encoding, then calls one typed PBS. The
optimizer's segmentation policy and any claim about bootstrap equivalence are
separate cryptographic work.

### 4. Separate general multi-input PBS from boolean gate wrappers

After the typed one-/multi-input primitive is established and verified,
re-express `tfhe_gate_bootstrapping_and` and `tfhe_gate_bootstrapping_or` as
small wrappers over named, static table/encoding constructors where that
preserves the existing arithmetic exactly. Retain specialized wrappers if they
provide clearer invariants or a more efficient known construction; the point
is one shared, auditable table representation, not forcing all code through a
slow abstraction.

The multi-input API must define, before implementation:

- input ordering and bit significance;
- the exact torus packing/centering rule for an `INPUTS`-ciphertext bundle;
- logical-table indexing and allowed table lengths;
- ring-capacity/negacyclic compatibility criteria;
- input noise/error budget and why combining inputs remains decryptable;
- output encoding and required post-bootstrap offset; and
- whether a custom table denotes a boolean operation, a finite program
  segment, or another explicitly named operation class.

The first implementation should cover a small, exhaustively testable arity
(e.g. two inputs) without generalizing from intuition. Higher arities and
larger program-segment tables require their own parameter/noise analysis,
reference comparison, and review notes.

### 5. Apply a disciplined `Vec`/slice reduction across the spec crate

For each existing dynamic collection, choose one of four destinations:

| Classification | Action |
|---|---|
| Fixed by a protocol/security parameter | Replace with `[T; N]`, a const-generic wrapper, or a `GenericArray` form already supported by the spec/compiler surface. |
| Fixed by a caller's circuit/table shape | Add a const-generic target-facing API; optionally retain a dynamic adapter outside the AST-to-AST subset. |
| Truly variable public input/output | Retain slice/`Vec`; document that it is dynamic-only and cannot determine generated layout. |
| Test-only host data | Keep in `#[cfg(test)]` if useful, but do not let it be the only coverage of target-facing APIs. |

Priority order:

1. **TFHE:** LUT address/table slices; runtime decomposition configuration;
   any bootstrap scratch/output collection introduced by future work.
2. **VOLE bridge and related fixed protocols:** paired bit/power slices in
   `vope_bitpack` and `q_bitpack`, where the number of bits is a circuit shape
   rather than a message length. Introduce matching fixed-array APIs first;
   preserve dynamic adapters only if a real dynamic caller needs them.
3. **FAEST fixed artifacts:** audit `Vec`-owning proof, commitment, PRG, and
   conversion paths. Replace only fields whose size follows the selected FAEST
   parameter set; preserve transcript/serialization interfaces where callers
   legitimately provide variable byte strings.
4. **Other spec modules:** audit curve/hash and fold APIs, documenting why
   fixed arrays already suffice or why a dynamic byte-domain is intentional.

Use existing `GenericArray`/`ArrayLength` conventions only where they are
already accepted across the Rust source, parser, dynamic lowering, and target
paths. Prefer Rust `[T; N]` for newly target-facing TFHE interfaces now that
LIR monomorphization supports concrete numeric const arguments. Do not add a
second generic-array abstraction merely to avoid a focused compiler gap; fix a
confirmed compiler representation gap in its own workstream.

## Implementation phases

Each phase should be separately reviewable. Phases that alter only tests,
metadata, or mechanical adapters can be prepared independently; any phase
that changes a cryptographic interpretation requires review and should retain
its Unpinned, Very unstable classification unless current evidence supports a
human-approved reclassification.

### Phase 0 — Baseline, inventory, and target-support matrix

**Files:** documentation, test-only support, and audit notes first; no
cryptographic behavior change.

1. Record all `Vec`, `vec!`, slice arguments, `.len()`-controlled loops, and
   runtime shape assertions in `volar-spec`, classifying them using the four
   categories above. Include a caller map: Rust-only, dynamic lowering, LIR,
   current weavers, and anticipated AST-to-AST use.
2. Inventory numerical values in TFHE and other protocols. For each, identify
   whether it is an algorithmic constant, layout/security parameter, test
   fixture, or runtime input; cite a source/reference where one exists.
3. Establish the supported source matrix with small standalone fixtures:
   flat const generics; nested arrays; numeric turbofish arguments; arrays of
   generic structs; const-generic fixed references; and `core::array::from_fn`.
   Test each through parser → IR → LIR → C compilation and execution where the
   feature is target-facing.
4. Capture current deterministic test vectors and differential fixtures for
   TFHE gates/PBS/LUT behavior before changing API shape. These characterize
   current behavior only; they do not validate security.
5. Publish an allowlist of dynamic APIs that remain outside the static target
   subset, including the reason each requires a runtime length.

**Exit criteria:** every in-scope dynamic length has an owner and a proposed
destination; target support is known before the spec API is designed around an
unsupported Rust feature.

### Phase 1 — Parameterize TFHE without changing bootstrap semantics

**Files:** primarily `crates/spec/volar-spec/src/tfhe.rs`, with cryptographic review.

1. Define the canonical const-parameter order and apply it to TFHE key types,
   private helpers, and public gate/PBS functions whose behavior depends on
   decomposition bases.
2. Move `bs_bg_log` and `ks_bg_log` from runtime operation/key fields to the
   canonical static configuration, preserving byte-for-byte or
   decrypt-and-compare behavior for the old test fixture.
3. Isolate test configuration in a named `#[cfg(test)]` fixture module. Give it
   explicit documentation: noiseless, small, correctness-only, and not a
   security recommendation.
4. Add an ergonomic *named* instantiation mechanism only after the compiler
   matrix proves it lowers to every intended target. It must be transparent to
   monomorphization and must not create a hidden default.
5. Keep public compatibility adapters only where a migration needs them. Mark
   them deprecated/dynamic-only, make their runtime validation explicit, and
   schedule removal once all known callers have migrated.

**Exit criteria:** no decomposition-layout fact can be accidentally changed at
runtime after key generation; two small distinct parameter instantiations
compile and execute without type/layout confusion.

### Phase 2 — Fixed-shape LUT and table construction

**Files:** `tfhe.rs`, TFHE tests, target-facing integration tests.

1. Introduce the fixed-size table descriptor and a validated constructor for
   the existing one-/two-bit boolean LUT semantics. Keep the raw polynomial
   helper private.
2. Replace `tfhe_lut_read`'s target-facing slices with const-generic fixed
   inputs/tables. Preserve the old behavior—including constant LUT behavior,
   signed test-polynomial encoding, centering, and output offset—under the
   new representation before expanding its capability.
3. Move capacity/power-of-two/negacyclic conditions out of ad hoc operation
   code into the table-construction/preflight boundary. Return a meaningful
   error or a compiler/weaver diagnostic for invalid static shapes.
4. Add a dynamic compatibility adapter only if an existing Rust-only consumer
   requires it. It must construct/validate a concrete table rather than call
   unchecked internals, and it is excluded from AST-to-AST target eligibility.
5. Make test fixtures exercise multiple `ADDR_BITS`, `TABLE_LEN`, and ring
   dimensions that are valid for their test parameter configurations.

**Exit criteria:** no target-facing LUT/PBS path allocates or derives a table
layout from runtime slice lengths, and every constructed table carries enough
shape/encoding information to be checked before bootstrapping.

### Phase 3 — Layered negacyclic optimization and generalized multi-input PBS

**Files:** direct-IR TFHE weaver work, TFHE specification/docs and `tfhe.rs`.
The work has two separately reviewable tracks.

**Track A — LUT-first layered negacyclic optimization:**

1. Define a direct-IR planner request whose input wires, logical Boolean table,
   `TfheBootstrapTable` shape, and dependencies are fixed at compile time.
2. Derive each candidate’s truth table from a pure same-block Boolean cone and
   accept it only by running the existing table validator; do not duplicate or
   approximate the negacyclic rule in the weaver.
3. Emit a topological sequence of valid table layers through `tfhe_lut_read`.
   Each layer returns the existing standard Boolean encoding and may feed a
   later layer. Invalid whole cones may be partitioned only into independently
   valid closed layers; otherwise they fall back unchanged.
4. Start with tables/operations that the existing implementation demonstrably
   accepts (including composable XOR where valid), exhaustively test each
   plaintext domain, and compile/run the generated target path.

This track does not claim arbitrary Boolean-table support or a new noise
analysis; it is bounded by the established `TfheBootstrapTable` contract.

**Track B — generalized multi-input programmable bootstrap:**

1. Write a focused companion design/review note citing the applicable TFHE/
   PBS reference algorithm. State the combined-input phase formula, error
   budget, domain indexing, polynomial construction, and output encoding.
2. Define a separate descriptor/validation path if the new construction
   materially expands the existing table family; do not silently change the
   meaning of `TfheBootstrapTable`.
3. Implement exactly one bounded generalized multi-input instance (normally
   the smallest useful arity) with an exhaustive plaintext oracle over its
   entire domain, deterministic reference vectors, and differential tests
   against prior AND/OR behavior where applicable.
4. Prove in tests that incompatible input count, table dimensions, ring degree,
   decomposition configuration, and output encoding cannot be mixed at a typed
   call site. Test invalid descriptor construction separately.
5. Only after the first generalized instance is reviewed, enable one-PBS
   program-segment fusion beyond the LUT-first table family. The weaver must
   pass a prevalidated descriptor, not a runtime `Vec<bool>`.

**Exit criteria:** Track A can ship a constrained direct-IR optimization with
no runtime-computed shape and no generalized-PBS claim. Track B can encode a
reviewed broader instance with no unsupported generality implied for arbitrary
program tables.

### Phase 4 — Convert remaining fixed protocol shapes

**Files:** targeted `volar-spec` modules, one protocol family per reviewable
change.

1. Convert VOLE bridge bit/power inputs to a fixed-shape API, preserving an
   adapter only for verified dynamic callers. Test the fixed and legacy paths
   against the same existing outputs before deprecating the latter.
2. Audit FAEST fields and functions. Replace proof/key/internal arrays fixed
   by a selected parameter set; leave transcript absorb/squeeze and serialized
   variable public messages dynamic when they genuinely are variable.
3. Audit remaining `Vec` fields with special attention to public structs:
   converting a public owned vector to an array is an ABI change and requires a
   migration note, test vectors, and target-compatibility testing.
4. Establish module-level documentation for every intentional dynamic API,
   including whether it is Rust-only, dynamic-lowering-only, or can be called
   by a static target after a caller-supplied fixed wrapper.

**Exit criteria:** protocol/circuit/table/key lengths no longer enter the
static target surface as unbounded slices or `Vec`s; remaining dynamic
collections have a documented semantic reason.

### Phase 5 — Compiler/weaver integration readiness

**Files:** compiler/weaver work plus spec-facing tests; load
`pipeline.md`, `agent-context/weaving.md`, and the ZK/non-ZK discipline note
before implementation.

1. Add linkable spec functions that call the new fixed-shape TFHE and VOLE
   APIs at multiple concrete specializations. Lower them through LIR to C and
   execute them; do not treat IR-text inspection as correctness evidence.
2. Ensure monomorphization plans every array/table/key specialization and
   rejects unresolved const parameters contextually. Concrete const arguments
   must reach the emitted C layout unchanged.
3. Define a target capability check for AST-to-AST weaving: reject a candidate
   spec call if its reachable signature/body contains an unclassified dynamic
   length or `Vec`-dependent layout. This is a diagnostic, not a fallback that
   silently chooses a runtime representation.
4. Add a weaver-facing table-construction hand-off API whose inputs are static
   program/circuit metadata. Its first consumer is the constrained
   LUT-first/direct-IR path specified in
   [`tfhe-multi-input-pbs-weaver-plan.md`](tfhe-multi-input-pbs-weaver-plan.md);
   generalized program-segment tables remain disabled until their separate
   construction and optimization policy receive review.
5. Preserve the ZK/non-ZK discipline boundary. TFHE is transparent; this
   refactor must not use `into_inner()`, change artifact tags, or loosen a
   `NonZk` bound to route a newly shaped module into another proving pipeline.

**Exit criteria:** two or more concrete static spec specializations compile
and run through the real backend; AST-to-AST feasibility is determined by an
explicit static-shape check rather than by accidental backend failure.

## Test and review plan

### Behavioral baseline and differential tests

Before every semantics-preserving phase, record existing outputs using the
current implementation. After the refactor, compare the old and new path for:

- LWE encryption/decryption under deterministic `SpecRng` seeds;
- AND, OR, NOT, XOR, and CMUX gate truth tables;
- one-bit and two-bit LUT/PBS behavior, including constant-table handling;
- test-polynomial construction and bootstrap output encoding; and
- VOLE/FAEST paths converted from a fixed semantic slice to an array.

A matching output proves only regression equivalence for the test fixture; it
does not establish cryptographic parameter security.

### Parameter matrix

Use at least two deliberately small, named, deterministic test configurations
with different dimensions/decomposition layouts where the reference code is
tractable. For each valid configuration:

- key generation, encryption/decryption, representative gates, typed LUTs,
  and the first multi-input PBS instance pass;
- incompatible configurations fail to type-check in compile-fail/API tests or
  fail descriptor validation before execution; and
- test documentation explains why the configuration is not deployable.

Do not run the schoolbook reference implementation at purported production
sizes merely to claim validation. A reviewed production parameter set needs
its own feasibility and security evidence.

### Real target tests

For APIs in the static target subset, add integration tests that:

1. parse/link real spec functions rather than hand-constructed string IR;
2. invoke them at multiple const specializations like a weaver would;
3. lower through the actual LIR path and C backend;
4. compile generated C with the system compiler; and
5. run an executable that checks semantic results.

Add focused diagnostics for invalid static table descriptors or unresolved
lengths. Do not assert generated variable names, statement counts, or an IR
text shape as the primary correctness signal.

### Review gates

| Change | Minimum review |
|---|---|
| Inventory, documentation, non-semantic test harness | Reproducible review appropriate to the changed behavior |
| Parser/IR/LIR representation or backend support | Generated-code compile-and-run coverage |
| Moving TFHE parameter binding from runtime to type-level | Cryptographic review and differential behavior evidence |
| New table encoding, multi-input packing, output encoding, or noise claim | Cited reference/correctness review; remains Unpinned and Very unstable unless reclassified with current evidence |
| Naming a production security parameter set or non-default pinnedness/stability | Independent cryptographic/human review per `reliability.md`; never AI-only |

## Acceptance criteria

- A target-facing spec function never needs a runtime slice length to determine
  a ciphertext/key/table/protocol layout or a generated loop bound.
- TFHE key and bootstrap operations carry matching compile-time layout and
  decomposition parameters; test-only values are not exposed as a security
  default.
- Fixed-address LUT/PBS calls use statically sized ciphertext-input arrays and
  validated statically described tables, not `&[Ct]`/`&[bool]` plus runtime
  length calculations.
- The raw test polynomial is not a general public escape hatch that bypasses
  table/encoding validation.
- At least one constrained, reviewed existing-LUT negacyclic layer can be
  represented with no runtime-computed input or table length; a broader
  multi-input PBS/program-segment representation remains deliberately deferred
  until its separate reference and error analysis.
- Every remaining `Vec`/slice in `volar-spec` is either test-only, a documented
  genuinely dynamic API, or has a scheduled fixed-shape replacement.
- Multiple small concrete parameter configurations and multiple fixed table
  shapes pass Rust behavior tests and real LIR → C compile-and-run tests.
- No change makes a non-default TFHE pinnedness/stability claim, a
  security-level claim for an unreviewed parameter set, or crosses the
  ZK/non-ZK artifact boundary.
