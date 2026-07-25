# RISC-V interpreter honest end-to-end ZK proof: status and plan

**Status (2026-07-25): the movfuscation-level tunnelled-slot elimination is
implemented and landed, on top of a real top-level-parameter-threading fix
in the split-weave (the actual root cause of the earlier revert — see
"Attempt 1 → Attempt 2" below). Source size for the interpreter's largest
accumulator-chunk function dropped a further ~2x on top of the `chunk_size`
mitigation (chunk_size=8: 46.8MB→24.1MB; chunk_size=1: 7.7MB→3.15MB,
compiling in 47s instead of ~9min). Full regression sweep green. The
bitwise-op-widening optimization (`target.rs`, `and`/`or`/`xor`/`not` as one
wide `Poly` instead of per-bit) is also implemented and landed — see
"Bitwise-op-widening" below; measured no size change on *this specific*
circuit (the interpreter's own dispatch logic doesn't route much traffic
through wide AND/OR/XOR), but is a real, structurally-verified win for any
program that does. The generic honest driver (Gf128-based multi-storage
memory check, `memory_check_driver.rs`) is built and a real
`honest_interpreter_run_folds_and_finalizes_with_real_memory_boundary` test
is written (`wat_gen.rs`, `#[ignore]`d, real interpreter scale) — but a
**second, independent scale wall** was found and partially addressed; see
"`WireRepr::Array`: fixing the real print-time OOM" and "Beyond
`WireRepr::Array`: where the remaining size actually is" below for the full
story. The **C-backend path was explored and paused** — real, but deeper
and buggier than its own test suite's doc comments suggested; see
"C-backend path: explored, paused" for the full handoff. Rust-side
pre-weaving optimization landed two real wins: single-bit `Shuffle`
aliasing (`emit_shuffle`, modest ~0.11% but validated) and a new
`batch_ir_blocks` IR pass merging structurally-identical width-1 `Poly`s
(30% `Poly` reduction pre-movfuscation, semantically verified via exact
storage-map match; only ~2.2% post-movfuscation due to a real SSA-ordering
constraint — see "Poly batching" below for the full story and why it's
smaller than hoped at the scale that matters). Two real, independent bugs
were also found and fixed in `volar-ir-opt`'s own DCE (stripping variables
external boundary metadata still needed). A third pass, `cse_ir_blocks`,
landed with by far the largest structural win found this session
(post-movfuscation `total=327,453→225,431`, `Poly=152,734→117,499` with
CSE+DCE+batch combined) — but actually **weaving** the optimized circuit
hits a real, unresolved gap: CSE/batch can merge/insert across what used
to be a per-original-block boundary, producing a "no entry found for key"
panic in the split-weave. A region-constrained variant reduces but does
not eliminate the panic (moves from `vole.rs:2826` to `vole.rs:3935`);
the next step (user-directed) is a bigger fix — physically hoist
cross-chunk-shared statements into `shared_prefix` and extend the
boundary metadata accordingly, rather than excluding cross-region merges.
**Update**: hoisting was implemented, measured, and found to cause
~120x overcounting (every one of 241 functions computing a value only 2
of them need) — confirmed OOM at 93.6GB. The user redirected to a THIRD
design — thread cross-chunk values as packed parameters between exactly
the functions that need them, mirroring `split_driver.rs`'s own
already-built `all_ok`/`fold_state` linear-chain precedent — which was
then fully implemented across `movfuscate.rs`/`vole.rs`/`split_driver.rs`,
unit-tested, and confirmed **logically correct** at real scale (a
pre-weave validation pass reports zero reference-visibility failures,
after fixing two real bugs found along the way — see "Cross-chunk
locality" for the full story). It remains blocked on a THIRD, distinct
real-scale performance blowup (90.9GB at t=25s, confirmed unrelated to
both the synthetic-slot count and to `batch_ir_blocks` specifically —
CSE+DCE alone reproduces it identically) with a plausible but
unconfirmed cause (wide-value materialization triggered across function
boundaries by synthetic threading). See "Cross-chunk locality" below for
the complete, current state. Movfuscation block-finish fall-through
remains unstarted.**

## `chunk_size` mitigation (still useful, now stacks with the real fix)

`largest_chunk_function_compiles` (`crates/examples/volar-riscv-e2e/src/wat_gen.rs`)
confirmed printed source size scales ~linearly with `chunk_size`, both
before and after the tunnelled-slot elimination below:

| `chunk_size` | functions | largest chunk params | source (before fix) | source (after fix) |
|---|---|---|---|---|
| 8 (old default) | 136 | 5,512 | 46.8MB — OOMs rustc after ~28min | 24.1MB — **still OOMs, after ~50min (2983.40s, SIGKILL)** |
| 4 | 151 | 3,296 | 24.4MB | 10.8MB |
| 2 | 181 | 2,188 | 13.3MB | 8.7MB |
| **1** | 241 | 1,634 | 7.7MB — compiles in ~9min (536.59s) | **3.15MB — compiles in 47s** |

`largest_chunk_function_compiles` uses `chunk_size=1`. Re-probed
`chunk_size=8` after the movfuscation fix landed: halving the source size
(46.8MB→24.1MB) was **not enough** — it still OOMs `rustc`, just after
~50 minutes instead of ~28. `chunk_size=1` isn't a faster alternative to
`chunk_size=8`, it's the only one of the two confirmed to actually work.
**`chunk_size=1` is the recommendation**, not a fallback.

## Attempt 1 → Attempt 2: tunnelled-slot elimination in `movfuscate.rs`

**Attempt 1** (seed `next_state[k]` from `state_vars[k]`, skip a block's
own gate+add when it provably doesn't touch slot `k`) was implemented,
found to panic ("no entry found for key" in `crates/compiler/volar-weaver/src/vole.rs`,
`slot_type`/`emit_poly_wide`) once split-woven, and **fully reverted** —
`movfuscate.rs` was restored to its exact committed baseline before
re-attempting.

**Root cause, precisely located** (not the "deep structural wall" the
first writeup here concluded — that was an incomplete diagnosis): all
three split-weave functions (`weave_vole_prover_ir_split`,
`weave_vole_qsim_ir_split`, `weave_vole_verifier_ir_split_with_trace`)
build `init_next_state_tys`/`init_ret_val_tys` via a **separate,
throwaway `probe_ctx`** (`VoleIrCtx::new(true)` / `::new_qsim()` /
`::new_verifier_with_trace_sink(sink)`) used *only* to determine each
accumulator slot's own type — and none of the three ever called
`insert_w_wires(&mut probe_ctx)` on it. Every other `VoleIrCtx` used for
real statement emission (block functions, chunk functions) *does* call
`insert_w_wires` first. This lone omission meant the one-off type-probe
couldn't resolve the circuit's own top-level params (`state_vars`) as
operands — exactly, and only, when a statement referencing them got
replayed into it. The main per-chunk `ctx` (used for the *real* function
body, not just type discovery) already binds `accum_info.init.next_state[k]`
correctly via `bind_scalar`/`bind_running`, which treats *any* var id —
param or statement-produced — as a normal named parameter; no identity-copy
workaround is needed there at all.

**The fix** (`crates/compiler/volar-weaver/src/vole.rs`, ~3 lines × 3
functions): add `insert_w_wires(&mut probe_ctx);` right after each
`probe_ctx` is constructed, before it processes `accum_info.init`'s own
statement range. This is exactly the "top-level parameter threading should
be implemented anyway" fix the user pointed at: this circuit is
fundamentally a looped, return-to-parameter construction, so the circuit's
own top-level params should be uniformly resolvable everywhere the
split-weave touches `accum_info`'s own var ids, not specially available
only to ordinary block processing.

**Attempt 2** (this fix, `movfuscate.rs` re-applied): seed `next_state[k]`
directly from `state_vars[k]` (no identity-copy wrapper needed, given the
above), skip a block's own gate+add when `br.next_state[k] == state_vars[k]`.
`ret_vals` was **not** re-attempted — its own "zero" contribution for a
non-returning block is a *fresh* `emit_zero_slot` call every time (no
caching/dedup in `emit_zero_slot`), so a var-id-identity skip check would
be dead code there regardless of the weaver fix; a real `ret_vals`
optimization would need a content-based ("is this var a zero constant")
check instead, not attempted.

**A real, separate correctness gap this also surfaced and fixed**:
`crates/examples/volar-riscv-e2e/src/split_driver.rs`'s own
`generate_split_step` hardcoded chunk 0's own `in_next_state_{k}` inputs
as literal `vope_zero()`/`q_zero()` — correct when `accum_init.next_state[k]`
really was zero (before this fix), silently **wrong** once it became
`state_vars[k]` (the real per-step entry state, honest for step 0 only,
where entry state genuinely is zero — the exact case `mem_probe.rs`'s own
3-step test happens to landed on, meaning it would *not* have caught this
by itself for steps 1+). Fixed by reusing `entry_w[pc_width + k]`'s own
already-bound driver-side locals directly (no new binding needed, matching
`accum_init`'s own "no new statement" property) instead of binding a fresh
zero. `ret_vals`' own zero-init is untouched (correctly still zero,
unaffected by the `next_state`-only reseed).

**Verification**: new unit test
`test_ir_tunnelled_state_slot_skips_accumulation_for_untouched_blocks`
(`movfuscate.rs`) confirms the skip mechanism fires exactly where expected
and not elsewhere. Full regression sweep green: `volar-ir-passes` (80/80),
`volar-weaver` (112/123 — the 11 failures are all pre-existing
`fhe::tests::*` TFHE issues, unrelated, confirmed by diff scope),
`interpreter_ir_movfuscates_and_unrolls_to_a_circuit`,
`halted_flag_triggers_loop_exit_immediately`, and
`honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary` (back
to its own pre-existing, unrelated `split_driver.rs:241` index-out-of-bounds
failure — no new panic, confirming both the weaver and driver fixes are
compatible with the existing pre-existing-failure baseline).

## Goal

Continue Milestone 1 (`~/.claude/plans/plan-to-have-an-synthetic-pike.md`): drive
the real RISC-V interpreter (`crates/examples/volar-riscv-e2e/src/interp.rs`'s
sum-4-words program) through the actual weave → VOLE-prove → VOLE-verify →
IOP-fold pipeline, honestly (real witness values, real memory-boundary
multiset check, real proof verification), mirroring
`mem_probe.rs::honest_mem_probe_run_folds_and_finalizes_with_real_memory_boundary`
but at real interpreter scale.

## What's confirmed working

- **Correctness**: `trace_interpreter_plain_values_matches_native_reference`
  (release, ~570s) passes — the interpreter, with this session's vaffle_ssa
  7th-bug-fix + SP-threading redesign applied, correctly computes and halts.
  An earlier session's flagged concern ("circuit state never changes" /
  "loops ~10x more than its bound implies") appears resolved as a side
  effect of those fixes.
- **Split-weave compiles at reasonable size for block/finish functions**:
  `measure_split_weave_on_real_interpreter` (120 blocks, chunk_size=8)
  completes in ~2.5s with negligible RSS; block functions are small
  (669–4,529 params).
- **`q_and`/`hat`/`r_and` are already array-batched** in the split-weave path
  (`crates/compiler/volar-weaver/src/vole.rs`, e.g. lines ~4796-4797,
  ~4938-4939, ~5083, ~5834). The 65535-rustc-arg-limit concern documented in
  `largest_chunk_function_compiles`'s own doc comment (wat_gen.rs) is
  **stale** — that fix already landed (confirmed empirically: the largest
  accumulator chunk has 5,512 params, not ~236K).

## The real blocker: accumulator-chunk source size, not param count

`largest_chunk_function_compiles` (real run, network confirmed working):
`rustc` gets **SIGKILL'd (OOM)** after ~28 minutes compiling
`vole_verify_ir_riscv_step_accum_chunk_0` — **5,512 params but 44.6MB of
generated Rust source** for that one function.

### Root cause (confirmed by reading `movfuscate.rs`)

The cross-block accumulation loop (`crates/ir/volar-ir-passes/src/movfuscate.rs`,
~line 622-647) computes, for **every** block `i` and **every** state slot `k`
(and every return-value slot), unconditionally:

```rust
let g = ctx.emit_gate(br.is_active, br.next_state[k], &state_slot_types[k]);
next_state[k] = ctx.emit_field_add(next_state[k], g, &state_slot_types[k]);
```

— i.e. `next_state[k] = Σ_i is_active_i · br_i.next_state[k]`, seeded from
`emit_zero_slot`. This emits one AND-gate + one XOR-add **per (block, slot)
pair**, regardless of whether block `i` actually writes slot `k`. Since most
RISC-V instructions only touch a handful of the ~20-40 state slots
(registers/PC/pointers), the overwhelming majority of these terms are
degenerate: for a block that doesn't touch slot `k`,
`br_i.next_state[k]` is exactly `state_vars[k]` (the slot's own incoming
value, referenced unchanged) — the term contributes nothing new
algebraically, but the code still emits a real gate for it.

This exact issue is already documented as deferred, unimplemented work in
`docs/agent-context/circuit-size-optimization-backlog.md`'s "Deferred:
movfuscation-level dedup — Tunnelled/unchanged-state-slot elimination"
section, written from an earlier session. It was never implemented.

### The fix (algebraically verified, not yet implemented)

Since `Σ_i is_active_i = 1` (movfuscation's own mutual-exclusivity
invariant) and all arithmetic here is GF(2)-style (XOR = add, AND =
multiply, AND distributes over XOR), for any state slot `k`:

```
Σ_i is_active_i · br_i.next_state[k]
  = Σ_{i: touches k} is_active_i · new_i[k]  +  Σ_{i: doesn't touch k} is_active_i · state_vars[k]
  = Σ_{i: touches k} is_active_i · new_i[k]  +  (1 − Σ_{i: touches k} is_active_i) · state_vars[k]
  = state_vars[k]  ⊕  Σ_{i: touches k} is_active_i · (new_i[k] ⊕ state_vars[k])
```

This is an **exact identity**, not an approximation — derived purely from
`Σ_i is_active_i = 1` and GF(2) distributivity, both already true of the
existing formula. Restricting the sum to only the (typically few) blocks
that actually write slot `k` collapses the per-slot gate count from
`n_blocks` to `n_blocks_touching_that_slot`.

**Detecting "block `i` doesn't touch slot `k`" is cheap and exact**: block
`i`'s own `next_state[k]` (from `emit_block_terminator`) is *literally* the
var id `state_vars[k]` (the combined block's own incoming param for that
slot) whenever block `i`'s original logic never writes it — a plain `u32`
equality check (`br.next_state[k] == state_vars[k]`), no block-body
analysis needed. This is a **sound, conservative** test: it can never
misclassify a touching block as non-touching (false negatives impossible —
worst case it just misses an optimization opportunity if an equivalent-but
differently-numbered var slips through, which only costs unrealized
savings, never correctness).

### Implementation caution

`MovfuscAccumInit.next_state` (currently seeded via `ctx.emit_zero_slot`)
needs to change to seed from `state_vars[k]` directly for the restructured
formula to be internally consistent, OR the delta-accumulation needs to be
computed as a wrapper around the existing zero-seeded loop without touching
`MovfuscAccumInit`'s own recorded field (safer — avoids any question of
whether `weave_vole_*_ir_split`'s chunk-0 special-casing, which treats
`MovfuscAccumInit` as literally representing "zero" per
`split_driver.rs`'s own doc comment — "Chunk 0's own inputs are always
literal zero" — depends on that exact semantic). **This needs to be
verified against the split-weave's chunk-0 handling before changing
`MovfuscAccumInit`'s own seed**, not assumed safe from static reading alone.

## Required verification before trusting this fix

1. New unit test(s) in `movfuscate.rs`: hand-built multi-block example,
   assert the optimized accumulation produces **identical** final
   `next_state`/`ret_vals`/`done_acc` values to the current (unoptimized)
   formula, across cases including "no block touches a slot",
   "every block touches a slot", and "only the active block touches it".
2. A statement-count regression test proving the optimization actually
   reduces emitted gate count for a slot most blocks don't touch.
3. Full existing regression sweep must stay green:
   `movfuscate.rs`'s own extensive unit tests, `volar-ir-passes` full suite,
   `interpreter_ir_movfuscates_and_unrolls_to_a_circuit`,
   `halted_flag_triggers_loop_exit_immediately`,
   `mem_probe.rs`'s honest end-to-end test (uses the same accumulation
   machinery at small scale — the closest thing to a canary for this exact
   change).
4. Re-run `largest_chunk_function_compiles` (release, real interpreter,
   ~28+ min budget, watch RSS) to confirm both a real *and* a
   *sufficient* reduction in generated source size.

## Generic honest driver: built, e2e test running

Both pieces below are now implemented (not just designed):

- `crates/examples/volar-riscv-e2e/src/memory_check_driver.rs` (new file):
  `MemCheckAccounting`, the generic per-`(storage_id, type_id)` accounting
  struct described below. Declared as `#[cfg(test)] pub(crate) mod
  memory_check_driver;` in `lib.rs`. Compiles cleanly.
- `crates/examples/volar-riscv-e2e/src/wat_gen.rs`:
  `honest_interpreter_run_folds_and_finalizes_with_real_memory_boundary`
  (`#[ignore]`d, real interpreter scale) — drives the real interpreter's
  circuit for a small, fixed `RAW_STEPS` (currently 2, deliberately **not**
  full guest-program completion — see the test's own doc comment for why:
  at `chunk_size=1`'s 241 functions/role, unrolling ~1000+ real steps into
  one generated driver function is a different design problem, out of
  scope here) through the real split weave (all 3 roles) at
  `chunk_size=1`, `eval_ir_circuit_step_with_watch` for real plain values,
  `MemCheckAccounting` for the real Gf128 multiset boundary, and
  `generate_split_step` for the per-step calling glue, then
  `prove_and_verify_iop` + a corrupted-boundary rejection check, compiled
  via `run_iop_verifier`.

Below is the original design (kept for reference; now implemented as
described above):

- `weave_vole_prover_ir_split` returns a `MemoryTrace` (real order + identity
  of every `StorageRead`/`StorageWrite`, including internal
  `StorageId::VAFFLE_SSA_SPILL` traffic from this session's SP-threading
  work — every spill/reload statement fires on **every** raw step
  unconditionally, per movfuscation's own "always execute, gate by
  is_active" design, so the interpreter's per-step oracle/memory-check
  volume is large; exact count not yet measured post-dedup-fix).
- `eval_ir_circuit_step_with_watch(..., watch: &[addr_var, value_var, ...])`
  gives real plain values for every trace entry, per real step, in the
  trace's own order — this is how `oracle_bits` (for `split_driver.rs`'s
  `generate_split_step`) and the memory-check accounting both get their
  real values, without hand-deriving them (mem_probe.rs's approach, which
  only works because it has exactly 2 single-address storages).
- Generalize `mem_probe.rs`'s hand-written 2-storage `MemoryCheckState`
  accounting into a generic per-`(storage_id, type_id)` accounting struct
  that tracks per-real-address last-value/last-timestamp and emits
  `init()`/`read()`/`write()`/`drain()` calls mechanically — necessary since
  the real interpreter touches dozens of storages with many addresses each
  (register file, RAM, `VAFFLE_SSA_SPILL`), not 2 single-cell storages.
- **Field choice**: use `MemoryCheckState<Gf128>` directly, not `Galois`
  (`u8` — only 256 values, would alias distinct real addresses/values).
  `Gf128 = Ext<Gf64, Beta64>` (`crates/iop/volar-iop/src/field.rs`)
  implements `FromBytes::from_u64` (injective for any `u64`), and
  `produce()`/`consume()` are already the same field `prove_and_verify_iop`
  consumes directly — no separate `.iop_embed()` step needed (that trait,
  `IopLift`, is only implemented for `Galois`/`Bit`, not `Galois64`/`Gf128`
  directly — confirmed by reading
  `crates/iop/volar-verifier-iop-runtime/src/lib.rs`).

## `WireRepr::Array`: fixing the real print-time OOM

Running `honest_interpreter_run_folds_and_finalizes_with_real_memory_boundary`
for the first time (2 real steps, `chunk_size=1`, all 3 roles) hit a second,
independent scale wall — **before compilation even started**: weaving
succeeded fast (241 functions/role), but `print_weaved_vole_module` on the
full module drove RSS from ~5GB past 13GB+ and climbing, with system swap
dropping to <1GB free (heavy thrashing, `Swapins`/`Swapouts` in the tens of
millions) — killed manually before it could OOM the whole machine.

**Root cause**: `insert_w_wires` (`vole.rs`, one copy per role in each
`weave_vole_*_ir_split*` function) and the shared `bind_scalar` helper both
eagerly unpacked every wide top-level/incoming param into `width`
individually-`let`-bound locals, **unconditionally, for every one of the
~241 functions per role**, regardless of whether that specific function
ever referenced them. Before this session's top-level-parameter-threading
work, `num_params` (`circuit.blocks[0].params.len()`) only covered the
circuit's own original inputs; after it, `num_params` covers every state
slot too (measured: **520** for the real interpreter). Unpacking is
O(functions × total_param_width) — the actual driver of the blowup.

**Fix**: `WireRepr` gained a third variant, `Array(String, usize)` — a
wide value kept as a **lazy reference into an existing `[T; width]`
array** (the function's own param, e.g. `w_5`), indexed on demand
(`arr_index("w_5", "3")`) rather than eagerly unpacked. Any consumer that
only needs an `IrExpr` (not a bound name) — `slot_expr`, `emit_poly_wide`'s
operand bundling — indexes directly, no extra statement at all (and
`emit_poly_wide`'s bundling step skips its own bundling `let` entirely for
an already-array-shaped operand, reusing the param name directly). Any
consumer that genuinely needs real bound-local names (`Merge` combining,
oblivious-storage address composition, the unrolled per-lane `Poly`
fallback) calls a new `materialize(&mut self, v)` helper first — idempotent
and memoized (unpacks once, writes a real `WireRepr::Vec` back into
`self.wires`), so only vars a function *actually* touches pay the
real-locals cost, not the whole top-level param set.

Verified: `volar-weaver`'s own `vole::tests` (36 tests) all green.
Re-running the full-module print (`probe_full_module_print_size`,
`wat_gen.rs`, `#[ignore]`d) after the fix: **241 functions, printed in
28s, well under an 8GB `ulimit -v` safety net** (vs. the pre-fix run that
was still climbing past 13GB after ~2 minutes with no end in sight).
The crisis is fixed and confirmed at real scale.

## Beyond `WireRepr::Array`: where the remaining size actually is

The fix above solved the *crash*, but the full prover-role module is still
**746,002,390 bytes (~746MB)** printed — vs. `chunk_size=8`'s single
largest chunk (46.8MB) already OOM-ing `rustc` after ~28 minutes, i.e.
~16x bigger than an already-failing reference point. Two follow-up
investigations, both real (not blind optimization attempts):

- **Used-parameter filtering** (implemented, `vole.rs`, block-function
  sites only, all 3 roles): only declare the specific `w_i` a block
  function's own statement range + boundary-derived output ids actually
  reference, instead of unconditionally all `num_params`. Deliberately
  conservative (safe over-inclusion via `collect_used_top_level_params` —
  can only declare an unused-but-harmless extra param, never omit a
  needed one) given how subtle this exact area has already proven this
  session. **Result: negligible** (746,055,351 → 746,002,390 bytes,
  ~0.007%). Root cause: every block's own `next_state` output is a
  full state-width tuple (one entry per slot); for a slot the block
  doesn't touch, that entry is a *literal pass-through* of the top-level
  param `state_vars[k]` (movfuscate.rs's own tunnelled-slot elimination),
  so the block function's signature needs `w_k` regardless of whether real
  computation touches it. This is a hard floor at the current calling
  convention — not fixable by filtering alone; would need block functions
  to return only touched slots (sparse), pushing pass-through defaulting
  into the chunk-level caller — a real redesign, not attempted.
- **Statement-mix measurement** (`probe_full_module_print_size`, extended):
  327,550 total statements in the combined circuit. `Poly`: 152,830 (47%,
  dominant) — 83,125 are wide XOR-chains (mostly already using
  `emit_poly_wide`'s collapsed-loop encoding), 68,107 are AND-bearing and
  almost all **already width=1** (no unrolling possible, one statement is
  already minimal). `Shuffle`: 23,408 (7%), every one already a single bit
  (23,408 total `result_bits` / 23,408 shuffles = 1.0 avg) — nothing to
  fold per-statement, only a batching-into-loops opportunity across many
  shuffles, and even that only touches 7% of total statements. `Merge`:
  1,338 (0.4%). **No obvious redundant/wasteful category** — this reads as
  genuine, largely irreducible circuit complexity for a 120-block real
  RISC-V interpreter at the bit-circuit level, not a second blowup bug.

## C-backend path: explored, paused (handoff)

Given the Rust-side numbers above, further shrinking the *Rust* source
alone isn't likely to close a ~16x gap, so this session explored an
alternative: `crates/compiler/volar-c-backend/tests/vole_e2e.rs`'s own doc
comments describe an "already-working, tested, real-cryptography-verified"
pipeline — weave → `LinkageSystem` (parses real `volar_spec::vole` source
via `parse_sources`, merges structs+functions into the woven module) →
`lower_module_with_opts` with a `MonoEnv` → `CBackend` → C → `cc` → run,
claimed verified against 4/4 real OT-based VOLE AND-gate checks. **This
turned out to be materially less solid than advertised** — direct testing
surfaced three distinct, real bugs in `volar-lir-codegen`, none related to
this session's other work. Paused here; this needs dedicated debugging in
a fresh session, not squeezed into an already very long one.

### Bug 1 (fixed this session): struct registry built with a hardcoded empty `MonoEnv`

`lower_planned_module` (`volar-lir-codegen/src/lib.rs:441-450`, part of the
"monomorphization 1" refactor per `git log -S`) called
`structs::build_struct_registry(module, target, &MonoEnv::new(""))` — a
**brand-new, always-empty environment**, completely discarding whatever
`env` the caller passed to `lower_module_with_opts`. Confirmed via direct
reproduction: even `vole_e2e.rs`'s own `vole_prover_and_gate_to_c` (single
AND gate, the simplest possible case) currently panics with:

```
thread '...' panicked at crates/compiler/volar-lir-codegen/src/structs.rs:283:21:
unsubstituted TypeParam length 'N' — add it to MonoEnv
```

This is a real, pre-existing regression from the monomorphization refactor
(an "artifact of when there was only one `MonoEnv`" — the refactor moved to
per-call-site environments but never updated the struct-registry-building
step, which still needs *some* representative environment for struct
*definitions* even though those aren't call-site-specific). **Fixed**:
`lower_planned_module` now merges every planned instance's own
substitutions (`plan.instances.values()`) into one environment before
building the struct/enum registries, instead of using an always-empty one.
In the common case (one global `MonoEnv` shared by every root, e.g. via
`lower_module_with_opts` — true for every caller in this codebase today)
this exactly recovers the original global env. Verified: the "N"
unsubstituted-length panic is gone for both `vole_e2e.rs`'s own tests and
the real split-weave's own single-block probe.

### Bug 2 (found, not fixed): the merge is unsound for genuinely divergent per-instance envs

The merge fix above is a **last-write-wins union** across every planned
instance — safe only because every root in every caller today shares one
identical env. It broke as soon as instances genuinely diverge: after the
fix, `vole_e2e.rs`'s `vole_prover_xor_gate_to_c`/`vole_verifier_xor_gate_to_c`
started failing with:

```
thread '...' panicked at crates/compiler/volar-lir-codegen/src/lib.rs:898:17:
Binary Mul: operand widths 32 vs 16 — cannot apply element-wise
```

— i.e. some other, unrelated function's own generic substitution (a
different width, probably from a different named-the-same generic param in
a different scope) leaked into the merged struct-registry env and
corrupted a width that should have stayed 16. A **correct** fix needs to
distinguish *root* environments (the caller's explicit, uniform intent —
what `lower_module_with_opts` promises callers) from *discovered/derived*
per-callee environments (which can and should legitimately diverge): thread
`options.roots` (currently only available in `lower_module_monomorphized`,
one level up from `lower_planned_module`) down and merge only those, not
the full `plan.instances` map. Not attempted — needs `MonoRoot`'s own
definition studied first (not yet done).

### Bug 3 (found, not fixed): severe performance pathology in `mono_type`/`register_tuples_in_type`

Independent of both bugs above, and **present before any of this session's
changes**: running `vole_e2e.rs`'s full test suite (12 tests, mostly
single-gate circuits) hangs, burning **~500% CPU per test thread with no
termination seen after 15+ minutes**. Confirmed via macOS `sample`
profiling (not guessed): every hung thread's call stack is identical —
`lower_module_with_opts` → `register_tuples_in_type` → `mono_type`
recursing into itself, with `_platform_memcmp` dominating the profile
(6,216 of ~10,345 total samples across 5 threads) — strongly suggesting a
superlinear or unbounded recursive comparison, not legitimate work. This
was likely masked before Bug 1's fix landed, since every test used to
panic *earlier* in the pipeline (at struct-registry build time) before
ever reaching `register_tuples_in_type`'s own pathological path — fixing
Bug 1 let execution proceed further and exposed this. **Not investigated
further** — needs its own dedicated profiling/debugging session. Caution:
this pathology consumed real CPU for ~15+ minutes across two separate test
runs (one orphaned by an earlier `kill` of its parent shell, both
eventually force-killed) before being caught — anyone picking this up
should run under a tight timeout/resource limit from the very first
attempt, not discover the hang empirically.

### Net assessment

`volar-lir-codegen`'s VOLE-to-C pipeline is real (Bug 1's fix demonstrates
forward progress is possible) but meaningfully less mature than its own
test suite's doc comments claim, with at least one active performance
hang. Given the depth remaining (properly distinguish root vs. discovered
envs, then diagnose and fix the `mono_type` pathology, then re-attempt
`probe_split_weave_single_block_lowers_to_c`, then scale to all 241
functions, then attempt a real `cc` compile) this is parked here as a
scoped, ready-to-resume handoff rather than continued in this session.

## Known environment note

The `run_iop_verifier` harness (`crates/iop/volar-verifier-iop-runtime/src/lib.rs`)
spins up a brand-new temp Cargo project per call and needs to resolve
`cipher`/`hybrid-array` from crates.io fresh each time — this failed twice
this session on registry-timeout network flakiness before the sandbox's
network access was fixed. Not a code issue, but worth remembering as a
possible source of spurious failures in any `run_iop_verifier`-based test.

## WAFFLE→VAFFLE and `vaffle_ssa` disassemble/reassemble audit

Requested check: does WASM→WAFFLE→VAFFLE lowering, or `vaffle_ssa.rs`,
unnecessarily break wide values into bits and reassemble them later,
wasting statements the same way movfuscation's untouched-slot accumulation
does?

**Implemented and landed** (`crates/ir/volar-vaffle-target/src/target.rs`):
`and`/`or`/`xor`/`not` used to all delegate to the generic
`BitCircuitBuilder` trait defaults `bc_and_vec`/`bc_or_vec`/`bc_xor_vec`/
`bc_not_vec` (`crates/ir/volar-lir/src/circuits.rs`) — simple per-bit loops,
each lane becoming its own scalar `Poly` statement, `width` separate
statements per WASM `i32.and`/`.or`/`.xor`/`.and`. Now: `emit_wide_binop_poly`/
`emit_wide_not_poly` build one wide `Stmt::Poly` directly (`compose_address`
merges the operand bits, one `Poly` computes every lane, `extract_bit`
un-merges the result — the same three primitives `mem_load_bytes`/
`mem_store_bytes` already use, see below), falling back to the original
per-bit path for `width <= 1` (no benefit) or `width > 64` (outside
`emit_poly_wide`'s own scope). GF(2) identities used: `a XOR b = a+b`,
`a AND b = a·b`, `a OR b = a+b+a·b` (verified by truth table — **not**
degree ≤1 as an earlier version of this doc and the original backlog note
both claimed; OR is not an affine function of its inputs in GF(2), it
genuinely needs the `a·b` term, same shape as AND), `NOT a = a+1` with an
**all-`width`-bits-set** constant (every lane flips at runtime, not just
bit 0 — `emit_poly_wide` decodes the constant per-lane from the raw
literal). Confirmed `emit_poly_wide`'s own `operand_expr` closure resolves
each monomial operand independently by its own `WireRepr` (`Vec` → per-lane
index, `Scalar` → broadcast) by reading it directly, *not* assumed from
`Stmt::Poly`'s own doc comment (which describes existing producers' typical
shape — one wide operand, one Bit selector — not a hard constraint the
weaver enforces); a monomial with *two* wide, non-selector operands (this
AND/OR case) is exactly as supported.

**Verification**: two new tests in `target.rs`
(`test_wide_bitwise_ops_emit_correct_wide_poly`,
`test_narrow_bitwise_ops_skip_wide_poly`) structurally confirm each op's
own `Poly` has the exact intended `coeffs`/`constant` (not an independent
interpreter-based semantic check — would need a `volar-fuzz` dev-dependency
cycle on `volar-vaffle-target`, out of scope for this fix, though Cargo
does support dev-dependency cycles if this is worth revisiting later).
Full `volar-vaffle-target` suite green (55/55, including the corpus smoke
test). Real interpreter regression tests
(`interpreter_ir_movfuscates_and_unrolls_to_a_circuit`,
`halted_flag_triggers_loop_exit_immediately`) still pass. **Measured no
size change** on the real interpreter's own circuit specifically — its own
dispatch/decode logic apparently doesn't route much traffic through wide
AND/OR/XOR (likely equality comparisons instead) — so this is a real,
structurally-verified win that just isn't exercised much by *this*
program; still worth having for any program that does use wide bitwise ops.

**Confirmed necessary, not wasteful**: `mem_load_bytes`/`mem_store_bytes`
(`crates/ir/volar-vaffle-target/src/waffle_lower.rs`, ~line 1185-1271) read
one byte (`Vec(8,Bit)`) per `StorageRead`, then decompose it into 8
individual-bit `Stmt::Shuffle` calls (one per bit); the store path does the
reverse (8 bits → one `Merge` per byte). This looks like disassemble/
reassemble at first glance, but it's structurally required, not avoidable
waste: `VaffleValue.bits: Vec<ValueId>` represents every value as one
`ValueId` *per individual bit*, uniformly, throughout this whole target —
and a single `Stmt` can only ever produce one output `ValueId`, so
producing 8 independently-addressable bit values from one byte-typed read
genuinely requires 8 separate `Shuffle` statements; there's no cheaper
primitive for "extract N individual bits from one wide value" in the
current `Stmt` vocabulary. Changing this would mean redesigning
`VaffleValue`'s own representation to support mixed bit/byte-granularity
chunks — a much larger, more invasive change than anything else in this
doc, not recommended as a quick win.

**`vaffle_ssa.rs`**: no unnecessary disassemble/reassemble found. Spill/
reload addresses (`emit_spill_address`) already avoid a real adder by
reusing `SP`'s existing high bits directly (no new statements for them)
and only materializing the low `k = log2(sp_step)` bits as fresh consts —
see `project_vaffle_ssa_spilling_architecture.md` (session memory) for the
full design. This was already optimized earlier in the same session that
built it.

## Pre-weaving and post-movfuscation optimization: results

With the C-backend path parked (see above), focus shifted to reducing
real circuit size on the Rust/weaver path itself. Real statement-mix data
(`probe_full_module_print_size`, extended; see "Beyond `WireRepr::Array`"
above) for the real interpreter's combined circuit (327,550 total
statements): `Poly` 152,830 (47%, dominant), `Shuffle` 23,408 (7%, every
one already single-bit), `Merge` 1,338 (0.4%). Four ideas were scoped;
two landed, one is documented as a real architectural limit (not a bug),
one wasn't started.

### 1. Single-bit `Shuffle` aliasing — landed

Investigated "trivial bit-to-bit `Shuffle` removal" directly: measured
(not assumed) that **zero** of the 23,408 `Shuffle`s are identity no-ops
or directly Merge-extractable — all 23,408 are "genuine" single-bit
extractions from a wide, non-trivially-sourced value. But **all 23,408**
turned out to belong to tightly clustered groups sharing one source var
each (544 groups, ~99% of same-group statement pairs within 4 statements
of each other) — the classic "decompose one wide value into its
individual bits" pattern. `emit_shuffle` (`vole.rs`) always emitted a
fresh `let out = src_bit.clone();` statement even when the source bit was
already a real bound name; fixed to alias directly instead. Real,
validated (`volar-weaver`'s own `vole::tests`, 36/36), modest
(746,002,390 → 745,190,830 bytes, ~811KB / 0.11%) — confirms `Poly`
statements are the real dominant cost, not `Shuffle`.

### 2. `Poly` batching — landed, real but bounded by a genuine SSA constraint

`batch_ir_blocks` (`volar-ir-opt/src/ir.rs`, new pass): merges width-1
`Poly`s related by an exact single-variable substitution (e.g.
movfuscation's own `is_active_i · touched_slot_k`, repeated per
`(block, slot)` pair) into one wide `Poly` + a `Merge` bundling the
varying operands, preserving each original `Poly`'s own output var id via
a cheap-to-weave `Shuffle` (per fix #1 above). Two real bugs found and
fixed during development (an over-coarse grouping key that collided
different "hole" choices; a genuine SSA-ordering violation where a
non-earliest member's own hole var can be defined after the group's
earliest member) — see the commit message for the full story. Verified
via 3 unit tests plus a real-scale correctness probe
(`probe_batch_ir_blocks_on_real_interpreter`) that runs the real
interpreter's own pre-movfuscation CFG to completion twice (unbatched vs.
batched) via `eval_ir_with_storage` and asserts the final return value AND
full 16,694-entry storage map match exactly. They do.

**Measured effect, two very different numbers**:
- **Pre-movfuscation** (3,783 total `Poly`s): 3,783 → 2,660, **-30%**.
- **Post-movfuscation** (152,734 total `Poly`s, the scale that actually
  matters for the compile-size problem): 152,734 → 149,435, only **-2.2%**.

The gap is a **real architectural constraint, not a bug**: the new wide
`Poly` (and its feeding `Merge`) must be inserted at the group's own
*earliest* member's position, so every member's own `Shuffle` (at or
after that position) can reference it — but a non-earliest member's own
"hole" variable is only guaranteed defined before *that member's own*
position, which can be at or after the group's earliest member.
Post-movfuscation, each `touched_slot_k` is typically computed *fresh,
immediately before its own use* (not hoisted near the top of the
combined block), so most candidate members get filtered out by this
ordering constraint specifically in the regime where the `Poly` count is
largest. A design that inserted *later* (after the latest hole var,
rather than before the earliest member) could capture substantially more
— but that requires moving each member's own *consumption* points too,
not just adding a producer, a materially bigger redesign than what
landed here. Not attempted this session.

### 3. CSE (`cse_ir_blocks`) — landed, biggest single win, but not yet weavable

`cse_ir_blocks`/`cse_ir_block_once` (`volar-ir-opt/src/ir.rs`, new pass):
deduplicates statements with byte-for-byte identical kind+operands (pure
kinds only — `Poly`, `Merge`, `Shuffle`, `Rol`, `Ror`, `Splat`,
`Transmute`, `Const`; storage/side-effecting statements excluded, left to
`store_forward_ir_blocks`). One real bug found and fixed *before* running
any test, via design review: CSE's own remap is deliberately many-to-one
(that's the point of dedup), so a naive per-monomial remap-and-collect on
a `Poly`'s coeffs can silently **drop** a term when two different
monomials collide onto the same key after remapping, instead of
GF(2)-XOR-combining them (e.g. `a XOR b` where `b` dedups onto `a` must
become the constant `0`, not silently `a`). Fixed via a dedicated
`remap_stmt_operands` helper with Poly-specific GF(2)-safe handling,
confirmed via a targeted unit test
(`poly_remap_is_gf2_safe_on_monomial_collision`). Verified via a
real-scale exact-match probe (`probe_cse_ir_blocks_on_real_interpreter`,
`eval_ir_with_storage` on the real interpreter's pre-movfuscation CFG,
exact 16,694-entry storage match) plus structural-only post-movfuscation
measurements:

- Post-movfuscation, before CSE: `total=327,453 poly=152,734`.
- After CSE alone: `total=224,899 poly=122,096` (**-31% total, -20% Poly**)
  — by far the largest single win found this session, much bigger than
  `batch_ir_blocks`'s own post-movfuscation -2.2%. Makes sense: unlike
  batching (blocked by the SSA-ordering constraint above), CSE has no
  positional constraint — it just needs byte-identical operands, which
  movfuscation's per-block-duplicated dispatch logic produces in bulk
  (structurally identical `is_active_i · touched_slot_k`-style
  expressions recur near-verbatim across blocks).
- After CSE+DCE+batch combined: `total=225,431 poly=117,499` — DCE+batch
  add relatively little on top of CSE alone at this scale, confirming CSE
  is where the real opportunity was.

**Not yet actually weavable — see "Cross-chunk locality" below.** These
numbers are structural (statement counts on the merged block), not a real
printed-size measurement, because attempting to actually weave the
optimized circuit hits a real, unresolved gap in how the split-weave
locates statements.

### 4. Cross-chunk locality: CSE/batch can merge across a chunk boundary the split-weave still assumes — open, mid-investigation

The split-weave (`weave_vole_prover_ir_split` and its qsim/verifier
siblings, `vole.rs`) builds one Rust function per **original**
(pre-movfuscation) block, each emitting only `shared_prefix` (statements
`[0, boundary[0].start)`) plus its own `[boundary[i].start,
boundary[i].end)` range — a *static, contiguous-range* assumption about
where every statement it needs lives. CSE/batch, run post-movfuscation,
have no notion of this structure and freely merge/insert across whatever
was originally a per-block boundary, producing a statement whose new
position is outside the range(s) the chunk(s) that actually use it will
ever emit. This surfaces only at **weave time**, not pass-run time, as
`panicked at .../vole.rs:NNNN: no entry found for key` — silent until
then.

Two variants confirmed so far:

1. **Unconstrained CSE+DCE+batch** (the 225,431-statement number above):
   panics at `vole.rs:2826`.
2. **Region-constrained CSE+batch** (see below): panics at a *different*
   location, `vole.rs:3935`, inside `emit_shuffle` — fewer merges happen
   (268,259 statements, more conservative than 225,431, as expected) but
   the panic isn't eliminated, meaning the region model itself still has
   a gap.

**Region-constrained attempt** (in progress, not yet correct): added
`region_of: Option<&[u32]>` to both `cse_ir_block_once`/
`batch_ir_block_once` (their own `canon_map`/grouping keys now include a
region id, so two statements from different regions can never be
treated as duplicates/batchable) — `cse_ir_blocks_with_regions`/
`batch_ir_blocks_with_regions`, unit-tested at small scale
(`region_aware_cse_never_crosses_a_region_boundary`,
`region_aware_batch_never_crosses_a_region_boundary`, both pass). Wired
into a new probe, `probe_optimized_full_module_print_size` (`wat_gen.rs`),
which computes a `region_by_orig_var` map once from the *original*
`MovfuscBlockBoundary`/`MovfuscAccumInfo` ranges (shared_prefix = region
0, each `boundary[i]` = region `i+1`, `accum_info.init` and each
`accum_info.steps[i]` their own subsequent regions) and a
`derive_region_of` closure translating it through the running `cumulative`
remap before each pass. **Still panics** (see above) — the region model
as built evidently doesn't cover the *whole* combined block's own
statement space. Leading, not-yet-confirmed hypothesis: `derive_region_of`
falls back to a sentinel `u32::MAX` for any statement whose old-var
lookup misses `region_by_orig_var` (e.g. anything not covered by exactly
one of shared_prefix/boundary[i]/accum_info's ranges — possibly a "finish"
function's own space, or a gap in how accum_info's ranges relate to
boundary's ranges, neither audited directly yet) — every such statement
would share the *same* fallback region and could still be merged/batched
together across what are, in reality, different real chunks. Not
confirmed; the audit (compare `region_by_orig_var`'s coverage directly
against the split-weave's own per-function emission logic in `vole.rs`,
rather than guessing) was not done before this doc update.

**Implemented and tried at real scale: the hoist-to-shared-prefix design.**
Built exactly as sketched originally (kept below for the record), plus
one refinement the first sketch didn't anticipate needing:
`hoist_shared_statements`/`hoist_shared_statements_once`
(`volar-ir-opt/src/ir.rs`) physically reorders the block into `[group 0
(shared)] ++ [each remaining region's own statements, stable order]` via
a stable sort keyed on `(group_rank, original_index)`, and returns
`region_ranges: BTreeMap<u32,(u32,u32)>` — each region's own *contiguous*
`[start,end)` in the **new** numbering — because naively remapping the
*old* `start`/`end` var ids through the pass's own remap is wrong (the
specific var that used to sit at a region's old boundary may itself have
been hoisted away). Region provenance is reconstructed post-hoc from
`cumulative` (inverted: for each final var, the union of every
pre-optimization var's own known region that maps onto it) plus, for
batch's two brand-new vars per accepted group (no pre-optimization
identity), the union of their own members' region sets — exposed via a
new `batch_ir_blocks_with_remap_and_members` (the member list wasn't
otherwise recoverable from a plain remap). 2 new unit tests confirm the
reordering and the returned ranges are exactly right at small scale.

**Real-scale result: a real, different failure mode, and the original
"coverage gap" hypothesis was WRONG.** A diagnostic run confirmed
`region_by_orig_var` covers all 327,453 vars in `[n_params, n0)` with
**zero gaps** — `region_sets_array: empty=0`. So every statement has a
definite region provenance; nothing defaults to "unknown → shared" by
accident. But `multi(genuine-cross-region)=19,130` out of 225,431 total —
and hoisting **any** multi-region statement moves it into `shared_prefix`,
which **every one of the 241 split-weave functions emits unconditionally**
regardless of whether that specific function needs it. A statement that's
only actually shared between 2 specific chunks (the overwhelmingly common
case — movfuscation's per-block dispatch logic is structurally similar
block-to-block, so CSE finds mostly *pairwise* duplicates, not duplicates
shared across dozens of blocks) still gets computed by all 241 functions
once hoisted — roughly 120× more instantiations than the 2 chunks that
actually need it. Running the full probe at real scale confirmed this
concretely: **93.6GB physical footprint, SIGKILLed** — the same class of
blowup `WireRepr::Array` fixed earlier this session, but now from real
statement duplication rather than eager per-function param unpacking.

**This is a real architectural limit of "hoist to a single universal
shared_prefix," not a bug in the hoist pass itself** (the pass does
exactly what it says: it's *correct*, just not *scoped* finely enough).
The split-weave's own "every function gets shared_prefix + its own range"
structure has no notion of a value shared between exactly 2 (or a handful
of) specific chunks — only "owned by exactly one chunk" or "owned by
all of them." Fixing this properly needs either: (a) partitioning
`shared_prefix` further, so a statement only gets emitted into the
specific *set* of chunk functions that actually reference it (a real
change to the split-weave's own per-function emission logic in `vole.rs`,
not just the IR-side passes); or (b) accepting a coarser, still-useful
middle ground — e.g. only hoist a statement when its own region-set size
is *large* (shared across many chunks, where universal hoisting is
actually a good trade), falling back to the region-exclusion constraint
(no merge at all) for statements shared between just a few chunks, losing
some of CSE's win but staying within the split-weave's existing two-tier
structure.

Also worth revisiting given the coverage-gap hypothesis is now ruled out:
the **region-exclusion** attempt's own earlier panic (`vole.rs:3935`,
"Region-constrained CSE+batch" above) almost certainly has a different,
more mundane cause than a coverage gap — most likely the `derive_region_of`
closure used there silently overwrites (rather than detects) a
"multiple old vars, different regions, same new var" collision (BTreeMap
iteration order picks whichever old var has the largest id), which could
let two *actually*-different-region statements look like the same region
right before a later pass runs. Not yet re-investigated with this
corrected understanding.

**User-directed next design (confirmed grounded, not yet implemented):
thread cross-chunk values as packed parameters, don't hoist or move
statements at all.** Rather than relocating a shared statement's own
computation (hoist) or forbidding the merge (exclusion), leave every
statement exactly where CSE/batch put it and instead extend the
split-weave's own **function call interface** so a value computed by
one chunk function is passed as an *explicit extra parameter* to only
the specific chunk function(s) that need it — mirroring the mechanism
`MovfuscAccumStep`'s own `next_state`/`next_pc`/`ret_vals`/`done_acc`
*already* use to thread the accumulator's running state from chunk `c`
to chunk `c+1`. This has a **direct, already-built precedent**:
`split_driver.rs`'s own module doc (top of file) states the verifier's
`all_ok`/`fold_state` pair already "thread[s] linearly across *every*
verifier-role call in call order (block 0, block 1, ..., chunk 0, ...,
finish) -- unlike next_state, this is a single continuous chain, not
scoped to movfuscation's own block/chunk topology." That's exactly the
shape a synthetic CSE-shared value needs: a value flows from whichever
chunk function computes it earliest (in this same real, established
call order) through every intervening chunk (cheap pass-through, same
as `next_state`'s own tunnelled-slot skip) to the last chunk that
consumes it, then stops. **No physical statement movement, no universal
prefix, no per-statement duplication beyond exactly the intervening
chunks that must pass it through** — the actual fix.

Concrete design, spanning three files (not yet implemented):

1. **Discovery** (reuse what's already built): run CSE/batch
   unconstrained (as now), get `region_sets_final` per final statement.
   For each statement with `region_sets_final.len() > 1`, its "producer"
   region is whichever appears *earliest* in the real driver call order
   (`boundary[0..n)` then `accum_init` then `accum_step[0..n)`, per
   `split_driver.rs`'s own doc); every other region in its set is a
   "consumer." (Values whose only "sharing" is with `shared_prefix`
   itself, region 0, need no new mechanism at all — they're already
   universally visible; only genuinely inter-chunk sharing needs a new
   slot.)
2. **Metadata**: extend `MovfuscBlockBoundary`/`MovfuscAccumStep` (or a
   new parallel structure keyed the same way) with a `synthetic_out:
   Vec<u32>` (var ids this range's own function must additionally
   expose as outputs — either genuinely computed here, if this range is
   the producer, or passed through unchanged from `synthetic_in`, if
   it's an intervening/consumer range) and `synthetic_in: Vec<u32>` (var
   ids this range's own function receives as extra incoming params,
   bound in `vole.rs` as if they were ordinary top-level circuit params
   for statements inside this range that reference them). Every range
   strictly between producer and last-consumer (in call order) gets a
   pass-through pair; the producer only gets `synthetic_out`; the
   consumer(s) only need `synthetic_in` for the specific ones they use
   (they need NOT re-export past the last one they consume).
3. **Weaver** (`vole.rs`, `weave_vole_prover_ir_split`/
   `weave_vole_qsim_ir_split`/`weave_vole_verifier_ir_split_with_trace`):
   for each function, bind `synthetic_in`'s own var ids as real params
   (analogous to `insert_w_wires`) and emit `synthetic_out`'s own var ids
   as real return values, using a `synth_{k}`-style naming convention
   distinct from the existing `next_state_{i}_{k}` naming so the driver
   generator (below) can distinguish "movfuscation-native state" from
   "CSE-synthetic pass-through."
4. **Driver** (`split_driver.rs`): thread `synth_{k}` the same way
   `all_ok`/`fold_state` already thread linearly across every call in
   order — likely reusable as a THIRD instance of that exact pattern
   (a `BTreeMap<usize, String>` of "currently live synthetic value ->
   its own current local name", updated after each call, consulted when
   building the next call's own argument list), rather than needing a
   wholly new threading mechanism.
5. **Correctness discipline**: this touches the same self-looping
   movfuscation core that produced the earlier "Attempt 1 → Attempt 2"
   saga this session's predecessor hit — build with the same care (real
   witness-value tests via `eval_ir_with_storage`/`eval_ir_with_trace`
   before trusting any circuit-level result, small hand-built fixtures
   before real-interpreter scale, `(ulimit -v ...)` before any real-scale
   weave attempt given this exact investigation's own confirmed 93.6GB
   OOM history).

Also still open, lower priority than the above: the **region-exclusion**
attempt's own earlier panic (`vole.rs:3935`) is unexplained (coverage-gap
hypothesis ruled out with data; a `derive_region_of` "last-write-wins
collision" hypothesis was reasoned through and appears NOT to actually
manifest under region-constrained CSE specifically, since the region
constraint itself guarantees any two old vars merged onto the same new
var already agree on region — so that panic's real cause remains
unknown). Likely moot once the packed-parameter design above lands,
since it would supersede exclusion entirely, but worth a note in case
exclusion is ever revisited independently.

**Implemented (2026-07-25, follow-up session): fully built, correctness
confirmed, blocked on a THIRD distinct performance issue.**

All three files from the design above were built:

1. **`movfuscate.rs`**: `MovfuscBlockBoundary`/`MovfuscAccumStep`/
   `MovfuscAccumInit` gained `synthetic_out: Vec<u32>`/`synthetic_in:
   Vec<u32>` fields (always empty from `movfuscate()` itself —
   backward-compatible, zero behavior change for every existing caller,
   confirmed via the full `volar-weaver`/`volar-riscv-e2e` suites: only
   the same 2 pre-existing mem_probe failures plus a newly-confirmed
   THIRD pre-existing failure, `beq_equal_branch_fires_straight_line`
   ("circuit must halt within budget"), confirmed via `git stash` A/B to
   already fail identically on the unmodified tree — not a regression).
   New `thread_synthetic_slots(boundary, accum_info, region_sets_final)`
   implements the discovery step directly: for a var with region set `S`
   (`0 ∉ S`, `|S| > 1`), `producer = min(S)`, `last = max(S)`; every
   region in `[producer, last)` gets the var appended to its own
   `synthetic_out`, every region in `(producer, last]` gets it appended
   to `synthetic_in`. 4 new unit tests (adjacent sharing, distant sharing
   threading through every intervening range, shared_prefix-inclusion
   needing no slot, repeated calls extending rather than overwriting).
2. **`vole.rs`**: all three split-weave functions (`weave_vole_prover_ir_split`/
   `weave_vole_qsim_ir_split`/`weave_vole_verifier_ir_split_with_trace`)
   bind each range's own `synthetic_in` var ids as extra `synth_{v}`
   params (via the *existing* `bind_scalar` helper, reused as-is) right
   after `insert_w_wires`, and append `synthetic_out`'s own
   `ctx.slot_expr`/`ctx.slot_type` values to the return tuple after
   emission — for both the per-block loop and the accumulator chunk
   loop (the latter checks only the chunk's own *boundary* steps,
   `accum_info.steps[lo]`/`accum_info.steps[hi-1]`, which correctly
   answers "does the whole `[lo,hi)` chunk need this threaded" for any
   `chunk_size`, not just `chunk_size=1`). `batch_ir_blocks_with_remap`
   gained a sibling, `batch_ir_blocks_with_remap_and_members`, exposing
   each newly-created var's own pre-batch member var ids (needed to
   compute a batch-created var's own region provenance, since it has no
   pre-optimization identity of its own to look up directly).
3. **`split_driver.rs`**: `build_call` gained a `synth_exported: &BTreeMap<u32,
   Slot>` parameter and a `synth_` param-name branch; two new threading
   maps (`synth_exported_vope`/`synth_exported_q`, keyed by var id, mirroring
   `exported_vope`/`exported_q`) get populated after each call by slicing
   the trailing tuple positions the callee's own `synthetic_out` declared,
   and consulted before building each later call's own argument list —
   the prover's own output populates the vope map, the **verifier's**
   own output populates the q map (qsim's own synthetic outputs are
   discarded, same as its `is_active`/`done`/etc, mirroring how
   `exported_q` was already populated from `v_*` not `q_*`).

**Two real bugs found and fixed via a real-scale validation pass** (a
cheap, no-weave pre-flight check added to the probe: for every range,
confirm every var its own native fields reference is actually visible —
a top-level param, in `shared_prefix`, within its own `[start,end)`, or
in its own `synthetic_in` — catching exactly the class of bug that
otherwise only surfaces as an opaque "no entry found for key" panic deep
in a release-mode-inlined weaver call, with no line-level attribution):

1. **`start`/`end` naive-remap bug (real, found first).** Even though
   this design never physically moves any statement, naively remapping
   a region's own `start`/`end` var ids through `cumulative`
   (`remap_movfusc_boundaries`'s own existing behavior) is WRONG: CSE's
   own "keep the earliest occurrence" behavior can merge a region's own
   *boundary-marking* statement (its literal first or one-past-last
   statement) onto an EARLIER position from a completely different
   region — unconstrained CSE isn't limited to merging within a region
   the way the start/end proof assumed for the (abandoned)
   region-exclusion design. Confirmed concretely: `boundary[65]` ended up
   with `start=524, end=523` (inverted). Fixed the same way
   `hoist_shared_statements`'s own `region_ranges` fixed the analogous
   problem for the hoist design, but without needing an actual reorder
   pass: since CSE/DCE/batch never reorder surviving statements (only
   remove/insert), a region's own surviving members stay contiguous, so
   `start`/`end` can be recomputed directly as `min`/`max` over
   `cumulative[v]` for every original `v` with `region_by_orig_var[v] ==
   this region` that's still alive.
2. **Validator false positives (found second, not a real bug).**
   `accum_info.init`/`.steps[i]`'s own `done_acc`/`next_pc`/`next_state`/
   `ret_vals` are NOT statement-position references at all in the sense
   the validator assumed — they're threaded via the **pre-existing,
   unrelated** `bind_running` mechanism (each chunk receives the
   previous chunk's own running value as an `in_*` param, *regardless*
   of that var's own physical position — this is how "unchanged slot,
   same var id carried across many steps" already worked before any of
   this investigation). Fixed by narrowing the validator to check only
   `synthetic_out` for accum ranges (the one field that IS my own new,
   physical-position-dependent mechanism).

**With both fixed, the validation pass reports zero failures** — every
var reference in the optimized+synthetic-threaded circuit is confirmed
resolvable. This is real evidence the packed-parameter design is
**logically correct**.

**Still blocked: a third, distinct real-scale performance blowup,
unrelated to hoisting's overcounting and unrelated to the synthetic
slot mechanism's own size.** Attempting the actual weave (not just
validation) gets SIGKILLed consistently around 60s regardless of
`ulimit -v` level tried (8GB, 12GB, 16GB all killed at the same point) —
`sample`-profiled directly (same technique used earlier this session for
the original `WireRepr::Array` OOM): **90.9GB physical footprint at just
t=25s**, hot functions `emit_zero`/`emit_poly_wide`/`array_t_default`/
`materialize`/`arr_index`/`emit_poly_lane` — i.e. the *same function
family* `WireRepr::Array` was built to keep cheap, now expensive again
through some other path. Two things ruled out concretely:
- **Not the synthetic-slot count**: a cheap pre-weave diagnostic
  (`total_span` = sum over every cross-region var of its own
  producer-to-last-consumer distance, predicting exactly how many extra
  param+return pairs get added across all functions) measured only
  `19,129` vars, `total_span=137,327` (median distance 2, p90 17, max
  119) — far too small to explain a 90GB blowup on its own.
- **Not `batch_ir_blocks`**: re-ran with batch skipped entirely
  (`VOLAR_SKIP_BATCH` env-gated in the probe, CSE+DCE+synthetic-threading
  only) — circuit size barely changes (224,899 vs 225,431 statements,
  consistent with batch's own tiny post-movfuscation contribution
  documented earlier) and the **identical blowup still occurs**. So
  whatever's expensive is already present from CSE+DCE alone; batch
  isn't adding or fixing it.

**Leading, unconfirmed hypothesis**: `emit_poly_wide`'s bundling logic
already materializes a `WireRepr::Array` operand when needed (Merge,
address composition, unrolled-Poly fallback — see `WireRepr::Array`'s
own doc). A CSE-shared value threaded via `synth_{v}` crosses a function
boundary; if the underlying value is *wide* (a common case — this
interpreter's own registers/words are typically 32-64 bits, and
`VaffleTarget`'s own bitwise ops already emit one wide `Poly` per
op) and the consuming statement needs individual bits or combines it
with something else, materializing it in *every* function it threads
through could reproduce a variant of the exact eager-unpack cost class
`WireRepr::Array` was built to eliminate — just triggered via the
synthetic-threading path instead of top-level param binding. **Not
confirmed** — would need direct instrumentation (count actual
`materialize()` calls and their own array widths during a real run) to
verify, not yet done.

**Where this leaves the investigation**: the packed-parameter design
itself is complete, unit-tested (4 new tests, all passing), and
validated logically correct at real scale (zero reference-visibility
failures) — a real, durable piece of infrastructure. Getting an actual
printed-size number still requires diagnosing this third performance
issue, which is a distinct, self-contained follow-up (add materialize()
call-site instrumentation; if the wide-value hypothesis is confirmed,
likely fix is to only materialize the *specific bits* a consumer
actually needs rather than the whole array, or to avoid threading wide
values through function boundaries that don't need every bit). Natural
next step for a fresh session.

Not yet implemented as of this doc update — this is the concrete,
grounded plan for the next work on this investigation.

<details>
<summary>Original hoist-to-shared-prefix sketch (superseded by the "implemented and tried" section above, kept for the record)</summary>

1. Run CSE + batch **unconstrained** (no `region_of`) for maximum
   optimization — this is the 225,431-statement result.
2. For each surviving statement, determine whether it's "shared": either
   (a) it was formed by merging inputs whose own original regions weren't
   all the same (CSE deduped across regions, or a batch group's members
   spanned regions), or (b) it has at least one consumer (another
   statement's operand, or a boundary/accum_info reference) whose own
   region differs from its own.
3. Physically reorder the block: `[existing shared_prefix] ++ [newly
   "shared" statements, stable original relative order] ++ [each region's
   own remaining statements, stable original relative order, grouped by
   region]`.
4. Recompute a position remap from this reordering, rewrite every
   statement's operands through it, and recompute `MovfuscBlockBoundary`/
   `MovfuscAccumInfo` ranges as contiguous ranges over the new layout.
5. Re-verify weave correctness at real scale before trusting the printed
   size number.

</details>

### 5. Movfuscation block-finish fall-through — not started

Currently every block unconditionally routes through the full
`is_active`-gated dispatch/mux for every step, regardless of whether the
next block is statically known. An optimization: when a block's own
terminator target is a fixed, known next block (not data-dependent), fall
through directly — set the new block/arg variables and continue
processing under those newly-muxed values, rather than looping back
through the generic, full-width dispatch mux. Potentially the largest
remaining win (could reduce movfuscation's own per-step dispatch cost,
not just per-statement size), but also the riskiest/most invasive —
touches the same correctness-critical movfuscation core that produced
this session's "Attempt 1 → Attempt 2" saga earlier. Natural next step
for a fresh session.
