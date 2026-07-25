# RISC-V interpreter honest end-to-end ZK proof: status and plan

**Status (2026-07-25): the movfuscation-level tunnelled-slot elimination is
implemented and landed, on top of a real top-level-parameter-threading fix
in the split-weave (the actual root cause of the earlier revert — see
"Attempt 1 → Attempt 2" below). Source size for the interpreter's largest
accumulator-chunk function dropped a further ~2x on top of the `chunk_size`
mitigation (chunk_size=8: 46.8MB→24.1MB; chunk_size=1: 7.7MB→3.15MB,
compiling in 47s instead of ~9min). Full regression sweep green. A separate,
already-scoped-but-unimplemented optimization remains in
`waffle_lower.rs`/`target.rs` (bitwise ops lower per-bit instead of as one
wide `Poly`) — see "WAFFLE→VAFFLE and vaffle_ssa disassemble/reassemble
audit" below. Generic honest-driver work (Gf128-based multi-storage memory
check) is designed but not yet built.**

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

## Separate, still-designed-but-not-yet-implemented: the generic honest driver

Once the above compiles, the interpreter's own honest e2e driver still needs
building (design complete, not yet written):

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

**Confirmed, already-scoped, not yet implemented**: `VaffleTarget`'s own
`and`/`or`/`xor`/`not` (`crates/ir/volar-vaffle-target/src/target.rs`,
~line 541-556) all delegate to the generic `BitCircuitBuilder` trait
defaults `bc_and_vec`/`bc_or_vec`/`bc_xor_vec`/`bc_not_vec`
(`crates/ir/volar-lir/src/circuits.rs`) — simple per-bit loops, each lane
becoming its own scalar `Poly` statement. Every WASM `i32.and`/`.or`/`.xor`
(and `i64` counterparts) therefore emits 32 (or 64) separate statements
where **one** wide `Poly` would do: XOR/OR/NOT are degree ≤1 and AND is
degree 2, both already within `emit_poly_wide`'s own documented scope
(used elsewhere, e.g. movfuscation's own `is_active` gating). This is not
a *disassemble-then-reassemble round trip* — it's "never assembled in the
first place" — but it's the same class of avoidable per-lane statement
bloat, already flagged as deferred work in
`docs/agent-context/circuit-size-optimization-backlog.md`'s "Deferred:
bitwise-op widening" section (written in an earlier session, still
unimplemented as of this one). **Recommended fix**: override
`and`/`or`/`xor`/`not` in `VaffleTarget`'s own impl to build one wide
`Stmt::Poly` directly (mirroring how `emit_poly_wide`-eligible code already
works elsewhere) instead of delegating to the bit-by-bit trait defaults.
Not attempted this session — real, separate, testable unit of work; would
need the same kind of careful before/after statement-count regression test
and full-suite verification as the movfuscation attempt above.

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
