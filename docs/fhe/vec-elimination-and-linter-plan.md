# Plan: eliminate unnecessary `Vec` from the spec and lint `Vec` in compiler IR

**Status:** plan + research record. It authorizes no reclassification; the
linter is a compiler-policy mechanism whose enforcement defaults are set in
§7 (human decision). **@ai:** assisted.

**Scope:** (1) remove unnecessary `Vec` from `crates/spec/volar-spec` —
starting with the `binfhe` family, where every heap allocation is either
presizable at weave time or avoidable — and (2) add a linter over the
`volar-compiler` IR that errors on `Vec` usage unless a specially formatted
doc comment justifies it. The same rule is recorded in `AGENTS.md` (§8).

**Related records:** [binfhe V2 plan](binfhe-v2-implementation-plan.md) ·
[static-shapes plan](../spec-static-shapes-plan.md) ·
[metadata container plan](../metadata-container-plan.md) ·
[pipeline.md](../pipeline.md) · [reliability.md](../reliability.md)

---

## 1. Why

The spec crate is the source the compiler parses and lowers into generated
Rust / TypeScript / C / dyn mirrors. Every `Vec` in it has one of three
meanings, none of which belongs in the *program* being compiled:

1. **A size the weaver already knows.** Circuit shape, LUT arity, key
   dimensions, layer counts — all fixed at weave time. A `Vec` there is a
   lost static-shape fact; the static-shapes machinery
   (`spec-static-shapes-plan.md`) and the LIR/C backends have to
   re-derive what was thrown away.
2. **A size the *plan* already knows.** `BootstrapPlan` is a weaver
   artifact: its arena sizes, layer counts, and LUT arities are known when
   the plan is built, so its consumers should be presized.
3. **An allocation hiding in a helper.** `inputs.iter().map(..).collect()`
   inside `execute_plan`, `Vec::new()` in codecs. These are pure
   implementation detail and should be stack arrays or `core::array` calls.

Heap allocation in generated cryptographic code is also a portability and
constant-time hazard (several backends are `#![no_std]`-without-alloc
targets; the C/WASM backends have no allocator contract).

The one *honest* `Vec` is the runtime-data container at the **adapter
boundary** — e.g. `plan_codec`'s byte buffer, or the wire/cell arenas an
interpreter grows while executing a runtime-supplied plan. Those are
host-side runtime concerns, not compiled-program structure, and they get an
explicit documented exemption (§5) rather than pretending to be static.

## 2. Research: where `Vec` lives today

### 2.1 Spec crate, `binfhe` (the pilot)

Production (non-test) `Vec` usage, from `grep`:

| Site | What | Classification (§3) |
|---|---|---|
| `plan.rs` `BootstrapPlan.layers: Vec<Vec<PlanOp>>`, `luts: Vec<LutSpec>`, `outputs`, `cell_outputs`, `PlanOp::Lut { inputs: Vec<WireId> }`, `LutSpec.entries: Vec<bool>` | plan data | Weaver-known / adapter-boundary |
| `plan.rs` `execute_plan`/`execute_clear` arena `Vec`s, `cts: Vec<LweCiphertext>` per-LUT | interpreter arenas + temp | Runtime-boundary (arenas) / removable (temp) |
| `plan_codec.rs` `encode_plan -> Vec<u8>`, decode buffers | serialization | Runtime-boundary |
| `keys.rs` `bsk: Vec<RgswCiphertext>`, `ksk: Vec<[LweCiphertext; KS_ELL]>` | evaluation key storage | **Keep as `Vec` (exempt)** — at Std128 dims (~36 MB / ~18 MB) the arrays cannot live on the stack; sized by const generics but heap-resident by necessity |
| `pbs.rs` (tests only) | fixtures | Test-only |
| `circuit_bs.rs` (test only) `contents: Vec` | fixture | Test-only |

Every non-key `Vec` above is either weaver-presizable or an adapter
boundary. There is **no** spec `Vec` that is genuinely runtime-dynamic
inside the compiled program.

### 2.2 Rest of the spec crate

The heavy `Vec` users outside `binfhe` (`tinylabels/ring_lwe.rs` 40,
`ot/wire.rs` 32, `faest/*` ~120 total, `ot/*` ~80 total) are mostly
**protocol buffers and batched material** (OT pools, VOLE material, hash
transcripts) — same three classifications, but they are *not* all
weaver-known: some are genuinely runtime-sized protocol quantities. They
are out of scope for the pilot (§6) and get the linter's exemption path
until individually reviewed.

### 2.3 Compiler IR — what the linter can actually see

`IrType` has **no `Vec` variant**. `Vec` enters the IR three ways, each
lintable:

1. **Type position:** `IrType::Struct { kind: StructKind::Custom("Vec"),
   .. }`. The parser (`parser.rs:1589`) already *recognizes* `Vec` only to
   reject a bare `Vec::default()`; it otherwise falls through to
   `Custom("Vec")`. The TS printer special-cases it
   (`printer_ts.rs:3295,3327,3491`). So a `Vec<T>` in a signature/field is
   `Struct{Custom("Vec")}` — trivially detectable.
2. **Expression position:** `IrExprKind::IterPipeline` with terminal
   `IterStep::Collect` / `CollectTyped` (`ir.rs:214-216`) — "materializes
   into a Vec". Also `IrExprKind::Path` resolving to `Vec::new` /
   `Vec::with_capacity` (TS printer pattern-matches these at
   `printer_ts.rs:4630-4631`).
3. **Macro position:** the parser does not expand `vec![...]` (binfhe
   learned this the hard way in M7 — `vec!` in a parsed function body is a
   parse error today). So `vec![]` never reaches the IR from source; it can
   only be *constructed* by a pass/weaver, which is exactly what the linter
   should reject at the IR level.

There is an established **doc-comment annotation precedent**:
`@volar-native: <Type>` (`parser.rs:179-216`) is scanned from `///` doc
attributes into a typed IR field. The exemption mechanism (§5) follows that
exact pattern.

### 2.4 Existing validation-pass shape

`volar-compiler-passes` hosts analysis passes; the error-returning
validator precedent is `lowering.rs::validate_impl(&self, imp) ->
Vec<String>` (collects error strings, empty = valid). The linter follows
that shape: pure analysis, no transform, error list out.

## 3. Classification rules (the semantic core)

Every `Vec` in spec source or in IR is classified by **who knows its
length**:

- **Weaver-known (remove):** the length is a const generic, a plan
  dimension, a LUT arity, a layer count, or a fixed protocol width.
  Replace with `[T; N]`, `core::array::from_fn`, `hybrid_array::Array`, or
  a presized slice parameter. If the weaver can emit it, it is presized.
- **Runtime-boundary (exempt, documented):** the length is genuinely only
  known at host runtime *and* the value never crosses into generated/target
  code — adapter codecs, host interpreters executing a runtime-supplied
  plan, test fixtures, evaluation-key backing stores that exceed stack.
  These carry the exemption doc comment (§5) at the item or module.
- **Forbidden:** everything else. The linter errors.

## 4. Vec-elimination plan (binfhe pilot)

Ordered; each step keeps `cargo test -p volar-spec --lib` green.

### 4.1 `plan.rs` — presize the plan and its interpreter

This is the structural change; everything else is downstream.

- `PlanOp::Lut { inputs: Vec<WireId> }` → `inputs: SmallInputs` where
  `SmallInputs` is a fixed-capacity inline array bounded by the profile's
  `K_MAX` cap (`params::max_lut_arity(LOG_Q_LWE) <= LOG_Q_LWE - 2 <= 30`).
  A LUT arity is *definitionally* weaver-known (it is in `plan.k_max`), so
  a small inline array — not a `Vec` — is correct. Chosen representation:
  `arrayvec::ArrayVec<WireId, 32>` **or** a hand-rolled
  `{ ids: [WireId; MAX_LUT_ARITY], len: u8 }` to avoid a new dependency
  (decision in §6.1; prefer hand-rolled — no new dep, matches `no_std` and
  the crate-constraint table).
- `LutSpec.entries: Vec<bool>` → same inline-capacity treatment
  (`[bool; 1 << MAX_LUT_ARITY]` + `len`) **only if** the max table length
  is also capped by `1 << MAX_LUT_ARITY`; otherwise it stays a
  runtime-boundary `Vec` with an exemption, because a LUT's *entries* are
  data, not shape. **Decision: entries stay `Vec` (data), with the
  module-level exemption covering the plan as a runtime artifact; the
  `inputs` id-list (shape) becomes inline.** This is the key distinction:
  shape is weaver-known, data is not.
- `BootstrapPlan { luts, layers, outputs, cell_outputs }` → these *are* the
  runtime plan artifact an interpreter loads; they are runtime-boundary by
  design. They keep `Vec` **with the exemption doc comment**, because the
  whole point of `execute_plan` (vs. generated code) is to run a plan whose
  size was not known at spec-compile time. The *generated-code* consumer
  (weaver) never sees these `Vec`s — it emits presized calls.
- `execute_plan` / `execute_clear` arenas → stay `Vec` (interpreter is a
  runtime-boundary consumer), **but** the per-LUT temp
  `cts: Vec<LweCiphertext>` (plan.rs:426) → inline `ArrayVec`/array, since
  its length is the LUT arity.

### 4.2 `keys.rs` — dual storage: borrowed (zero-heap) + owned (`Vec`)

Evaluation keys are sized entirely by const generics (`N_LWE`, `BIG_N`,
`BS_ELL`, `KS_ELL`) — they are weaver-known, so a `Vec` is *not*
semantically required; it is a native-execution convenience. Two storage
modes, one operation surface:

- **Borrowed (zero-heap), for virtual targets.** `BootstrappingKeyRef`
  / `KeySwitchingKeyRef` hold `&'a [RgswCiphertext<..>]` /
  `&'a [[LweCiphertext; KS_ELL]]` slices into caller-managed storage
  (stack-allocated fixed arrays). Virtual targets with (virtually)
  unlimited stack — the Volar-IR LLVM target and similar — construct the
  key material in place on the stack and pass the borrowed view; no heap
  allocation anywhere. The bootstrap/CB operations are implemented
  generically over a `Borrow<..>`/trait view so the same `blind_rotate`,
  `pbs`, and `circuit_bootstrap` code runs on both storage modes.
- **Owned (`Vec`), for native execution.** The current
  `BootstrappingKey.bsk: Vec<..>` / `ksk: Vec<..>` stays, exempted with
  `/// @volar-allow-vec: eval-key-store: ...`. Native targets have a
  bounded stack (~8 MB) and ~36 MB of Std128 key material cannot live on
  it; heap is correct there. `BootstrappingKey::borrow()` / `.as_ref()`
  yields the `..Ref` view so native code reuses the same borrowed
  operation surface.

This removes the apparent tension between "keys are weaver-known (no
Vec)" and "keys exceed the native stack": the *shape* is static, the
*placement* is a target property. The exemption remains on the owned
variant only; the borrowed variant needs no exemption because it contains
no `Vec`.

### 4.3 `pbs.rs` / `circuit_bs.rs` — tests only

Test fixtures keep `Vec` freely; the linter (§5) does not scan
`#[cfg(test)]` modules. No change.

### 4.4 Weaver side — emit presized, not Vec

`fhe_binfhe.rs` already emits `IrExprKind::Array` (fixed) for LUT inputs
and tables. Confirm no emitted path uses `Collect`; the linter (§5) runs
on the weaver's output `IrModule` as a gate.

## 5. The IR linter (`volar-compiler-passes/src/vec_lint.rs`)

### 5.1 What it checks

A pure analysis pass over `IrModule`:

```rust
pub struct VecLintError { pub path: String, pub kind: VecUseKind, pub note: String }
pub enum VecUseKind {
    VecType,        // IrType::Struct{Custom("Vec")} in sig/field
    VecCollect,     // IterPipeline terminal Collect/CollectTyped
    VecCtorPath,    // Path resolving to Vec::new / Vec::with_capacity / vec![]
}
pub fn lint_module_no_vec(module: &IrModule<..>) -> Vec<VecLintError>;
```

Walk every function signature, struct field, let-binding type, and
expression tree. On a hit, check for the exemption; if none, push an error.
Empty result = clean.

### 5.2 The exemption doc comment

Following the `@volar-native:` precedent, an item is exempt iff it (or its
enclosing module) carries a doc comment of the exact form:

```text
/// @volar-allow-vec: <runtime-boundary | eval-key-store | host-interpreter | test-fixture>: <one-line reason>
```

The parser already scans `///` doc attributes (`parser.rs:179`); the linter
reuses that scan. The category is required so exemptions are auditable by
kind; a free-form `#[allow]` would defeat the purpose.

### 5.3 Enforcement points

1. **`volar-codegen` gate:** run `lint_module_no_vec` on the combined IR
   before `print_module_*`; non-empty errors fail codegen with the list.
   This is the default-on enforcement for the spec→target path.
2. **Weaver output gate:** `weave_binfhe_plan` (and later other weavers)
   runs the linter on its emitted `IrModule` in tests.
3. **Spec-source gate:** a `#![deny]`-style test in `volar-spec` that
   re-parses its own sources and lints (catches `vec!` in parsed bodies,
   which the parser already rejects, plus `Vec` types in signatures).

### 5.4 What it deliberately does not do

- It does not rewrite code; it is a gate, not a transform (matches
  `validate_impl`).
- It does not scan `#[cfg(test)]` modules.
- It does not flag `Vec` inside `volar-compiler`'s own *implementation*
  (the compiler uses `Vec` everywhere as its own data structure — that is
  compiler-host code, not compiled-program IR). The lint is over the IR
  graph's *payload*, not the graph's own containers.

## 6. Milestones

| # | Milestone | Gate |
|---|---|---|
| V1 | `vec_lint.rs` pass + exemption parser + unit tests over hand-built IR | lint tests green; detects all three `VecUseKind`s; honors exemptions |
| V2 | `plan.rs` LUT `inputs` → inline capacity; `cts` temp → inline; `keys.rs` dual storage (`BootstrappingKeyRef` borrowed + owned `Vec` + `.as_ref()`); bootstrap/CB ops generic over the key view; exemptions added | `cargo test -p volar-spec --lib` green; binfhe plan/dyn/e2e still pass; a borrowed-key test runs the full pipeline with zero heap key storage |
| V3 | codegen gate wired into `volar-codegen dyn`/`ts` (fail on non-exempt `Vec`) | `generate.sh` stays green on the current spec (exemptions cover plan/keys); a seeded non-exempt `Vec` fails the gate |
| V4 | weaver-output lint gate in `fhe_binfhe` tests + confirm no `Collect` emitted | weaver tests green incl. lint gate |
| V5 | rest-of-spec sweep: classify each `ot/`/`faest/`/`tinylabels/` `Vec`, remove weaver-known ones, exempt the rest with categories | spec tests green; lint report on the whole spec is empty-or-exempt |
| V6 | AGENTS.md rule + docs index | merged |

## 7. Open decisions (human)

1. **Default-on enforcement** in `volar-codegen` (V3) makes a non-exempt
   `Vec` a hard build error for the spec→target path. This is a
   build-behavior change; it is gated on a maintainer enabling it, exactly
   as non-default pinnedness is human-gated.
2. **New dependency vs hand-rolled** for inline capacity (`arrayvec` vs
   `[T; MAX] + len`). Recommendation: hand-rolled (no new dep, matches the
   crate-constraint table and the SWC-identifier-hygiene-style preference
   for explicit in-repo invariants).
3. **`LutSpec.entries`** stays a `Vec` (data, not shape) vs inline. Kept as
   `Vec` + exemption; revisit only if a backend needs static table
   materialization.

## 8. AGENTS.md rule (to add under "Core Design Rules")

> **No `Vec` in spec-compiled program structure.** A `Vec` whose length is
> known at weave time (const generic, plan dimension, LUT arity, layer
> count, fixed protocol width) is forbidden in `volar-spec` program code
> and in any IR a weaver emits. Use `[T; N]`, `core::array::from_fn`,
> `hybrid_array::Array`, or a presized inline capacity. A `Vec` is allowed
> only at a documented runtime boundary (adapter codecs, host interpreters
> running a runtime-supplied plan, evaluation-key backing stores that
> exceed stack, test fixtures) and only with a `/// @volar-allow-vec:
> <category>: <reason>` doc comment. The `volar-codegen` IR linter errors
> on any other `Vec` type, `Collect` pipeline, or `Vec` constructor path.
> Shape is weaver-known; data is not — classify by who knows the length.

## 9. Non-goals and risks

**Non-goals:** removing `Vec` from the compiler's own host implementation;
making `execute_plan` allocation-free (it is a host interpreter); a
generic smallvec dependency; retro-fitting `tfhe.rs` (Track S is frozen
experimental).

**Risks:** (a) Over-eager removal could break the dyn mirror — V2 keeps
`execute_plan` on `Vec` arenas deliberately. (b) The exemption mechanism
could be abused as a blanket `#[allow]` — mitigated by the required
category and by code review. (c) `PlanOp::Lut.inputs` inline capacity
bakes in a max arity (32) — matches `params::max_lut_arity`'s existing
profile cap, so it is not a new restriction.
