# LIR-native loops for the spec backend — plan

**Status:** Implemented (2026-08-28). Stages 0–4 landed; see §8 for the
commit record and measurements. Descent loops intentionally remain
concrete-unrolled (see §3.4).
**Scope:** `volar-lir-codegen` (flat + CFG paths), `volar-c-backend-spec-tests`
(regressions), optionally `volar-ir-lir-target` (interpreter parity).
**Relation to other work:** builds on the direct-to-LIR fast path
([`direct-to-lir-weaver-fast-path-plan.md`](direct-to-lir-weaver-fast-path-plan.md),
Phase 2 tranche 3). Does **not** touch the monomorphizer, the weaver/BIrStmt
level, or the parser.

---

## 1. Motivation

The flat spec backend (`volar-lir-codegen`) currently lowers every loop by
**concrete unrolling**: `lower_bounded_loop` requires `concrete_usize_expr`
bounds and re-invokes `lower_block` once per iteration, so all locals stay
ordinary SSA bindings in one block (`env: name → Vec<Value>`).

This model is simple and correct, but it has three real costs:

1. **Code size / compile time.** The TFHE component alone lowers to ~41 MB of
   C, dominated by unrolled loop bodies. Every added spec component multiplies
   this. Compile-and-run regressions (the repo's required testing discipline)
   get slower with every component.
2. **Concrete-bounds ceiling.** Loops whose trip count is only known at
   runtime cannot lower at all today. `IrExprKind::WhileLoop` has no flat-path
   arm, and the `while is_zero(&x) && tries < 64` retry pattern in
   `vole/setup.rs` (and grinding loops generally) is unreachable through LIR.
   This is a named blocker for widening the M1-path slice.
3. **Iterator pipelines are second-class.** `RawMap`/`RawFold`/`IterPipeline`
   are all unrolled by hand in `lower_expr`; a native-loop mode gives them one
   shared mechanism instead of three ad-hoc unrollers.

There is already an **abandoned native-loop fallback** inside
`lower_bounded_loop` (the non-concrete-bounds branch that builds a
`loop_header` with `counter`/`limit` block params). Its own doc comment
records why it is not the default:

> A CFG back-edge can only carry block parameters; the old path carried `i`
> but **silently discarded assignments to e.g. `result[i]`**.

That is the crux of this whole plan: in a native loop, the loop body runs in a
*different block*, and

- scalars that survive across iterations must become **loop-carried block
  params** (phi nodes), and
- aggregates (arrays/structs) written inside the body must live in **memory**
  (`alloca` + `ptr_store`), because the current indexed/field assign machinery
  mutates flat `env` value vectors that simply do not exist across a back-edge.

Any native-loop option that does not solve both is the "silently discarded
assignment" bug again. The design below therefore treats memory promotion as
the core deliverable, not the loop skeleton.

## 2. Current-state inventory (evidence)

| Piece | Where | State |
|---|---|---|
| CFG primitives on `LirTarget` | `volar-lir` `lib.rs`: `create_block`, `add_block_param`, `switch_to_block`, `jump`, `branch`, `switch`, `dyn_jump` | Complete; exercised by `LirTarget::call/switch` (Phase 1b) and `basic.rs` countdown test |
| Memory primitives | `LirTarget::{alloca, ptr_load, ptr_store, ptr_offset, ptr_index_load, ptr_index_store, stack_alloc_ext, heap_alloc_ext}` | Complete; `ptr_index_load/store` already used for slice refs in `lower_index`/`lower_assign` |
| Reentry hints | `BranchTarget::with_reentry`, `ReentryHint::bounded_loop_ascending()` | Defined and already emitted by the abandoned fallback; downstream consumption semantics must be confirmed before relying on it |
| C backend blocks | `volar-c-backend`: two-phase parallel assignment to block params, then `goto blockN` | Works (used by the countdown test) |
| VolarIrTarget interpreter | `volar-ir-lir-target` | Vars are **per-block**; cross-block values must flow through block params. Switch cascade tests prove the threading discipline |
| Flat loop lowering | `lower_bounded_loop`: concrete → unroll; non-concrete → abandoned native path | Non-concrete path is unsafe (see §1) and only reached when bounds are non-const |
| `WhileLoop` / `IterLoop` in flat path | `lower_expr` has no `WhileLoop` arm; `IterLoop` only supports descending concrete unroll | Not lowered |
| `continue`/`break` | Parser emits `continue;` as `IrStmtKind::Semi(Continue)`; the unrolled path treats Continue as "stop this block's lowering" | No jump-based semantics exist |
| Aggregate assignment | `lower_assign`: nested-index chains fold to a linear position and update flat `env` values via a select-mux tree; field assign overwrites at a flattened offset | Correct only while everything lives in one block |
| Options plumbing | `MonoPlanOptions { roots, max_instances, lenient, include_auxiliary }` → `lower_planned_module(module, target, &plan, lenient)` | Only `lenient` is threaded today; a loop option needs the same path |

## 3. Design

### 3.1 The option

```rust
pub enum LoopLowering {
    /// Today's behavior: concrete-bounds unrolling; non-concrete bounds error.
    Unroll,
    /// Bounded loops lower to CFG loops; unrolling only where a loop is
    /// deemed non-convertible (§3.4), with the reason recorded.
    Native,
    /// Native when the loop qualifies, unroll otherwise. Default in later
    /// stages once equivalence is proven; Unroll is the default at first.
    Auto,
}
```

- Lives on `MonoPlanOptions` (it is a lowering policy, not a planning policy —
  `plan_flat_module` is untouched).
- Threaded the same way `lenient` is: `lower_planned_module(module, target,
  &plan, options.lenient)` grows a `&MonoPlanOptions`-shaped parameter
  (mechanical signature change at 2 call sites), into `LowerCtx` as
  `loop_mode: LoopLowering` plus a `loop_stack: Vec<LoopFrame>`.
- Exposed to tests/driver via `lower_module_seeded` / `lower_module_monomorphized`
  callers (spec tests get a `native-loops` variant of each component).

### 3.2 Value model across blocks (the core invariant)

**Invariant (target-agnostic):** after a `jump`/`branch`, a value is
referencable only as (a) a block param of the successor, or (b) memory. Raw
`env` scalars are valid only within the block that defined them. The C backend
happens to tolerate raw C locals across labels; **VolarIrTarget does not**, and
the option must be backend-neutral, so we design to the strict target.

Loop skeleton (ascending, matching the existing fallback and
`ReentryHint::bounded_loop_ascending()`):

```
before:          start_val, end_val computed in the current block
                 jump header(start, limit)
header(i, n):    cmp = i < n          // or != for `while`
                 branch cmp → body, exit
body:            i and loop-carried values arrive as block params
                 <body lowered>
                 jump latch(…)
latch(i', …):    jump header(i+1, n)
exit:            loop-carried values arrive as block params (final iteration)
```

Scalars that are read after the loop or re-assigned across iterations become
**extra block params** on `header`/`latch`/`exit` (the phi). Everything else
stays in `env` for the body's own block only.

### 3.3 Aggregate assignment: alloca promotion

This is the piece the abandoned path lacked. Before lowering the body of a
native loop, run a **loop-body assignment analysis** over the IR body
(`BoundedLoop.body` / `WhileLoop.body`):

- Collect variables **assigned** inside the body by any of: `Assign{Var}`,
  `AssignOp{Var}`, indexed assign (`Assign{Index}` and nested chains), field
  assign (`Assign{Field}`).
- For each such variable whose declared type is an aggregate (`Array`,
  struct, tuple) **or** which is live after the loop: promote it to memory.
- Scalar loop-carried vars (`x = x + 1` where `x: u8`) become block params —
  cheaper and keeps the SSA discipline; an alloca is the fallback if the var
  is also indexed into.

Promotion mechanics (all target-primitives that already exist):

1. At loop entry (before `jump header`): `slot = alloca(elem_ty, count)` —
   element-wise alloca for flat arrays; the flattened-scalar layout already
   computed by `struct_field_scalar_offset`/`mono_len` gives the slot count.
2. Initialize the slot with the pre-loop value (`ptr_store` per scalar, or
   `ptr_index_store` bulk where available).
3. Inside the body:
   - reads (`Var` of a promoted name, `Index`, `Field`): lower as
     `ptr_index_load`/`ptr_load` at the same linear position the select-mux
     machinery computes today (reuse the nested-index chain → linear position
     folding from `lower_assign` — it is exactly the addressing math needed).
   - writes: `ptr_index_store`/`ptr_store` at that position.
4. After `exit`: promoted names in `env` map to their slot; a *read* after the
   loop lowers to loads of the final memory contents. (Simplest correct form:
   after the loop, load the whole aggregate back into `env` values once —
   O(width) loads, same as the select-mux tree would have cost, and downstream
   code keeps seeing ordinary `env` values.)

`ctx.env` entries for promoted names hold the **slot pointer** (a
`Vec<Value>` of length 1 whose scalar type is `Ptr<…>`), tagged in a new
`ctx.promoted: BTreeMap<String, PromotedSlot>` so `lower_index`/`lower_field`
/`lower_expr(Var)` can route to loads instead of the select-mux path.

### 3.4 Loop classification & fallback policy

A loop is **native-eligible** when:

- it is `BoundedLoop` (any bounds — concrete bounds are fine natively too) or
  `WhileLoop`;
- its body contains **no `oracle`/`action` external calls** (see §3.6) —
  else fall back to unroll with a recorded reason;
- descending loops / step≠1: supported by computing the direction and step in
  the header (`i += step` / `i -= step`); `IterLoop` descending unrolls stay
  as-is in `Auto` initially.

Anything else (e.g. a body containing `Try`, or an `oracle_bit` inside the
loop) falls back to `Unroll` **with the reason recorded** in the emitted
instance debug info. Never silently mix: a native loop whose body was too hard
must not degrade into a partial unroll.

### 3.5 `continue` / `break`

Add a `loop_stack: Vec<LoopFrame { latch: Block, exit: Block, carried: … }>`
to `LowerCtx`:

- `Continue` (both `IrStmtKind::Semi(Continue)` and `Expr(Continue)` — the
  parser uses Semi) → `jump latch(next_i, carried…)` with the *current*
  carried values.
- `Break` → `jump exit(carried…)`.
- Unrolled loops keep their existing "stop this block's lowering" semantics;
  the loop stack only changes behavior when the enclosing frame is native.
- Nested conditionals inside the body already emit blocks; a `continue` inside
  an `If` thread must forward the carried values through each intermediate
  block — the same threading discipline the switch cascade uses
  (`all values threaded through check-block params`).

### 3.6 Semantic risk: occurrence counters inside loops

`oracle_bit(name, bit, occurrence)` / `action_store_bit(…, occurrence)` use
occurrence counters as **stable call-site identity**. Under unrolling each
iteration is a distinct static site; under a native loop the occurrence would
become a dynamic `i`. That changes downstream proof semantics (weaver oracle
maps, IOP fold consumers), so:

- **Stage 1–3 policy:** a loop body containing `oracle`/`action`/`oracle_bit`
  calls is not native-eligible (falls back to unroll).
- **Future decision (out of scope here):** whether occurrence = f(i) is a
  sound extension, decided with the weaver/IOP owners.

### 3.7 Provenance

`P` (provenance) already threads through every `LirTarget` instruction
emitter; the new jump/branch/store emissions must carry the enclosing
statement's provenance like other emissions do (the `IrExpr<P>`/`IrStmt<P>`
nodes are at hand in `lower_iter_chain`-style helpers). No new provenance
obligations are created, but the preservation obligation applies: no
loop-restructuring may drop `P`.

## 4. Testing (per the generated-code testing rule)

Every stage lands with **compile-and-run** regressions in
`volar-c-backend-spec-tests` (the `basic.rs` harness pattern: emit C → `cc` →
run → check stdout), plus **interpreter parity** where applicable:

1. `native_loop_countdown` — scalar loop-carried var via block params; C and
   VolarIrTarget produce identical output to the unrolled build.
2. `native_loop_aggregate_write` — `for i in 0..N { result[i] = f(i) }` with
   `result` read after the loop (the exact case the abandoned path silently
   corrupted).
3. `native_loop_field_assign` — struct field written in a loop, read after.
4. `native_loop_dynamic_bounds` — bounds from a function parameter
   (impossible under unrolling; exercises `WhileLoop`-shaped bounds).
5. `native_loop_continue_break` — inside nested `if`s (threading check).
6. `native_loop_descending` — `IterLoop`-shaped descending loop.
7. **Dual-path equivalence harness** (Stage 4): for each component seed set,
   lower once with `Unroll` and once with `Native`, compile both, run both on
   the same inputs, diff stdout. This is the gate for flipping `Auto`'s
   default.

Interpreter parity note: VolarIrTarget's per-block vars make it the strictest
consumer; if a construction works on C but panics the interpreter, it is a
threading bug in our emission, not an interpreter bug (per the switch-cascade
precedent).

## 5. Implementation record (2026-08-28)

All four stages landed on `main` as single per-stage commits:

| Stage | Commit | Notes / deviations from this draft |
|---|---|---|
| 0 — plumbing | `76d5ca0` | `LoopLowering` on `MonoPlanOptions`; `LowerCtx.loop_mode`/`loop_stack`; legacy skeleton documented as unsound |
| 1 — native bounded loops | `36b629b` | Carried scalars as block params (phi); whitelist body scan with fallback. `lower_function_with_loop_lowering` test entry |
| 2 — alloca promotion | `58f6d27` | Promoted layouts: flat arrays get element-granularity slots (runtime-indexed `ptr_index_store`); other aggregates get one whole-value slot with load-splice-store for element/field writes. Mixing whole+part assigns of one name is a rejection reason. Promotion requires `StackAllocExt` |
| 3 — while/continue/break | `a6b8a4f` | `lower_while_loop` (condition evaluated in the header; **carried names must be rebound to header params before the condition lowers** — the initial draft omitted this and the while test caught it). `LoopFrame` distinguishes native/unrolled frames so `continue`/`break` bind to the innermost loop; unrolled `break` signals through an atomic flag the unroller observes per iteration |
| 4 — equivalence harness | (this stage) | `native_loop_equiv.rs`: identical instance sets, compile gating on the unrolled baseline, size recording |

Deviations discovered during implementation (evidence-first):

- **The flat path's If/match lowering never threaded values across
  blocks** (`lower_if` branches with empty args and lowers branch bodies
  referencing outer `env` values directly). The "strict target" §3.2
  stance therefore reduces to: loop-carried values must be threaded
  because their *values change per iteration* (a semantic requirement on
  every target), not for block purity. Free variables are raw-referenced,
  consistent with the rest of the path. The loop var's outer binding is
  saved/restored like the unroll path instead of threaded.
- **Descending loops stay concrete-unrolled** (`lower_bounded_loop_descending`
  unchanged); per §3.4's original note they remain unrolled in `Auto`
  mode initially.
- **Standalone-cc failures (pre-existing, both modes — fixed 2026-08-29):**
  compiling component outputs with `cc` exposed four bugs unrelated to
  loops: (1) the C backend emitted `Ptr(Arr)` struct fields (Vec
  fat-pointer `data`) without the referenced array typedef — fixed in
  volar-ir `7bcf716` (typedef recursion through pointers, alloca/heap
  element registration, extern-vs-definition reconciliation);
  (2) `StdMethod::Into` lowered to a call to an undefined extern — fixed
  as identity (T == O in all instances reaching it); (3) distinct
  instances of operator methods (`mul__<Struct>`) shared one emitted name —
  fixed with full-name dedup + `__dupN`; (4) `infer_expr_type` lacked an
  Index arm so `&a[j]` call args went unplanned — fixed. After the fixes:
  **tfhe native output compiles and links** (14.97 MB); vole_prover and
  vole_verifier compile and link in both modes. The 283 MB unrolled tfhe
  output is syntax-valid but exceeds any practical cc budget — itself the
  motivation for native loops; the harness skips it above a 64 MB budget.

### Measurements (component C output, unrolled vs native, post-fix)

| Component | Unrolled | Native | Ratio |
|---|---|---|---|
| tfhe | 283,409,389 B | 14,976,602 B | **18.9× smaller** |
| vole_verifier | 782,409 B | 620,850 B | 1.3× smaller |
| vole_prover | 2,509,023 B | 2,451,093 B | 1.02× (≈unchanged) |

(The earlier 6.3× tfhe ratio was depressed by the unplanned-call fallout;
18.9× is the true figure.)

Behavioral-parity tests (compile-and-run, `cc`): native `sum_to(10)=45`,
native concrete-bounds loops, aggregate index writes (`fill4`),
field-splice struct accumulator (`accumulate(4) = 6 4`), dynamic
`while` (`count_down(10)=55`), `continue`+`break` (`sum_selected(10)=25`),
each with an Unroll-parity counterpart where a baseline exists.

## 6. Stages (original plan — kept for review trail)

| Stage | Content | Exit criteria |
|---|---|---|
| **0** | `LoopLowering` enum + plumbing (`MonoPlanOptions` → `lower_planned_module` → `LowerCtx`); `loop_stack` scaffolding; doc comments on the abandoned fallback updated to point here. Zero behavior change. | `cargo check --workspace` clean; all suites unchanged |
| **1** | Native `BoundedLoop` (opt-in `Native`): header/latch/exit skeleton, scalar loop-carried vars as block params, **bodies rejected (fallback) if they assign aggregates**. Tests 1. | New tests compile-and-run on C *and* interpreter |
| **2** | Alloca promotion for aggregate assignment inside native loops (indexed, field, whole-var). Tests 2, 3. | Same; no select-mux regressions in `Unroll` mode |
| **3** | `WhileLoop` + dynamic bounds + `continue`/`break` + descending/step. Tests 4, 5, 6. | Same |
| **4** | Dual-path equivalence harness over the component seed table; code-size/compile-time measurement (TFHE before/after); `Auto` policy defined (native-eligible → native, else unroll). Decision memo on default flip. | Equivalence green on all green components; measurement recorded in this doc |

Each stage is one commit (or a small series), suites green before commit, and
the gap table in `direct-to-lir-weaver-fast-path-plan.md` gains a pointer here
when Stage 1 lands.

## 7. Risks & mitigations

| Risk | Mitigation |
|---|---|
| Silent value loss across the back-edge (the historical bug) | §3.2 invariant is checkable: after Stage 1, run every native-loop test on VolarIrTarget, whose per-block vars *panic* on raw cross-block references. The strict target is the guardrail. |
| Alloca promotion misses a write path (new assign shapes added later) | Classification collects assignments by IR kind; an **unknown assign shape inside a native loop body falls back to unroll**, never guesses. |
| Occurrence semantics drift (oracle/action in loops) | Hard fallback for loops containing external oracle/action calls (§3.6); revisit only with weaver/IOP owners. |
| Code bloat from per-element allocas where a block param would do | Scalar-carried → block params first; allocas only for aggregates. Measure in Stage 4. |
| `ReentryHint` consumers disagree on `bounded_loop_ascending` semantics | Before Stage 1 commits to emitting it on every latch, confirm with the downstream pass that consumes reentry hints (vaffle/IR passes); if unsettled, emit without the hint and record it as a follow-up. |
| Two divergent loop implementations drift over time | The dual-path harness (Stage 4) runs both modes on every component seed set as a standing regression. |

## 8. Non-goals

- No change to `plan_flat_module` / monomorphization keys (loops are a
  lowering concern; instance identity is unaffected).
- No new IR node shapes; the parser and `volar-ir` are untouched.
- `RawMap`/`RawFold`/`IterPipeline` keep their existing unrolled lowerings in
  Stages 0–3; migrating them onto the native skeleton is a Stage-5 candidate
  only after the equivalence harness exists.
- The weaver/BIrStmt level and CFG module *emission* (`lower_cfg_module`) are
  out of scope, though the shared `LowerCtx` means Stage 1+ benefits the CFG
  path for free where it uses the same bounded-loop lowering.
