# Agent Context: circuit-size optimization backlog

**Load this when:** picking up further circuit-size/statement-count
optimization work on the movfuscated Volar-IR pipeline (movfuscation,
`lower_to_circuit_ir`, the VOLE weaver), after the width-at-rest fix.

## Status as of this log

Two optimization passes have shipped, in this order, each measured against
the same real benchmark (the Milestone-1 RISC-V interpreter's one-step
circuit, `crates/examples/volar-riscv-e2e`, `count_woven_statements_after_optimization`,
`#[ignore]`d, run manually):

1. **`emit_poly_wide`** (weaver-level AND-gate batching) —
   7,249,314 → 5,342,031 woven prover statements (~26%). See
   `docs/agent-context/boolar-ir-conflicts.md` conflict #3.
2. **Width-at-rest frontend fix** (`VaffleTarget::add_block_param`/`jump`/
   `branch` pack/unpack instead of decomposing every i32/i64 to individual
   bits across block boundaries) — 5,342,031 → **3,272,222** (~39% further,
   ~55% total from the original baseline). See conflict #4 in the same doc.

Per the user's own stated conditional ("unless the decreased state width
alone is that big of a win"), this was judged decisive, and the remaining
items below were **deferred, not implemented**, in favor of moving on to
Milestone 1's remaining steps (driving a real proof end-to-end). This doc
exists so whoever picks up further circuit-size work next doesn't have to
rediscover the plan from scratch.

Since then, Milestone 1.5 Step B (splitting the woven verifier/prover into
one function per block plus a *chunked* accumulator, dropping
post-movfuscation optimization) has been completed and validated at real
interpreter scale — see the two "Milestone 1.5 Step B" sections below.
The circuit that previously required 8GB+ RSS (climbing, killed before
completion) to weave now weaves in ~2.5s with negligible memory.

## Milestone 1.5 Step A: `virtualize_ir` dedup — attempted, reverted, real bugs found

**Status: not adopted.** Wiring `volar_ir_virt::virtualize_ir` (block-skeleton
dedup: "one body per unique handler rather than one body per original
block") in as a pre-movfuscation pass on the interpreter's `ir_blocks`
(`crates/examples/volar-riscv-e2e/src/wat_gen.rs`'s `lower_interpreter`,
between `optimize_to_fixpoint` and `movfuscate_ir`) surfaced two real,
non-trivial bugs before it could be measured for actual `and_count`
reduction. The wiring itself has been **fully reverted** — `lower_interpreter`
is back to calling `movfuscate_ir(&ir_blocks, ...)` directly, no `volar-ir-virt`
dependency in `volar-riscv-e2e`'s `Cargo.toml` — so this doesn't block
anything; it's parked here for whoever picks Step A back up.

### Bug 1 (found, real, but **not fixed** — fixing it in isolation broke something else)

`crates/ir/volar-vaffle-target/src/lower_to_ir.rs`'s `plan_functions`
interns the module-entry↔function-0 call continuation's `Block` type via
`intern_cont_block_type(total_ret_bits)`, where `total_ret_bits` comes from
`self.module.sigs[body.sig.0].results`. For the interpreter's WAT-authored
`(func (export "run") (result i32) ...)`, this signature's `results` field
came back **empty** (`sig.results.len() == 0`), even though the function's
actual `Terminator::Return { values: [..] }` genuinely returns one i32 —
confirmed by direct instrumentation (`panic!`-based, since both this crate
and `volar-ir-virt` are `#![no_std]` and can't use `std::eprintln!`).
This makes the interned continuation type one param too narrow (`[SP_word]`
instead of `[SP_word, ret_word]`), which `volar-ir-virt`'s
`fill_terminator_slots` (`crates/ir/volar-ir-virt/src/ir.rs:1102`) — the
first code in this pipeline to actually cross-check a `Dyn` jump's `args.len()`
against its `Block` type's declared `params.len()` — correctly flags as an
index-out-of-bounds (`args.len()=2` but `sig.len()=1`).

**Why it wasn't fixed**: the natural fix (compute `total_ret_bits` by
scanning the function body's own blocks for a real `Terminator::Return` and
using its actual value widths, since `translate_terminator`'s own
`Terminator::Return` handling already does exactly this and never consults
`sig.results` either) is *correct* in isolation, but re-sizes continuation
block 1 (the module's synthetic exit continuation) to 2 params instead of 1.
`movfuscate_ir`'s `compute_expanded_state_slot_types`
(`crates/ir/volar-ir-passes/src/movfuscate.rs:1607`) requires **every**
block's param at a given positional index to agree on type — and this
2-param exit-continuation block collides, at position 1, with an unrelated
block's own (differently-typed) param 1. In other words: the *original*,
too-narrow continuation type was accidentally load-bearing — its wrong
width happened to numerically match whatever the interpreter's own blocks
declare at that position, and `movfuscate_ir` (which has never itself
cross-checked `Dyn`-target arity against args) silently tolerated the
type-confusion. Fixing bug 1 in isolation regresses the *already-working*
`movfuscate_ir`/`lower_to_circuit_ir` baseline (confirmed directly: with the
fix applied and `virtualize_ir` *not* even in the picture,
`interpreter_ir_movfuscates_and_unrolls_to_a_circuit` fails with
`movfuscate_ir: block 3 has type Vec(32, TypeId(0)) at param 1, but an
earlier block had a different non-Block type there`). The fix was reverted.

**What this really means**: `movfuscate_ir` implicitly assumes the *entire*
block list it's given (including `lower_to_ir.rs`'s special module-entry
block 0 and exit-continuation block 1, which are logically one-shot
trampoline blocks, not part of the interpreter's own per-step execution
loop) shares one uniform per-position state-slot layout. That assumption is
currently only kept true by an unrelated, accidental type-narrowness bug.
Fixing this for real needs `movfuscate_ir` (or `lower_to_ir.rs`) to either
(a) genuinely unify block 0/1's param layout with the interpreter's own
loop-carried state layout, or (b) exclude the one-shot entry/exit blocks
from the uniform-slot-type invariant entirely (they execute at most once,
outside the interpreter's own repeating step). Both are real design work,
out of scope for a quick fix.

### Bug 2 (found, not fixed, deeper — this is why Step A itself is parked)

Independent of bug 1: even with `DispatchMode::Public` (per `docs/virt.md`,
the *correct* mode to combine with a subsequent `movfuscate_ir` call —
`DispatchMode::Oblivious` is listed there as a **deferred integration**, not
a working end-to-end path, so don't reach for it), `virtualize_ir`'s own
output (dispatcher + deduped handler blocks + setup block) does not
produce a uniform per-position param-type layout across all its blocks
either — hit the *same* `movfuscate_ir:1647` assertion (`block 50 has type
Vec(64, TypeId(0)) at param 1, but an earlier block had a different
non-Block type there`), this time from virt's own block shapes, not the
entry/exit trampoline. `virtualize_ir`'s dispatcher/handler/setup blocks
each have param shapes suited to their *own* individual purpose (matching
a per-handler register-file view, per `docs/virt.md`'s register-routing
description) — nothing in `virtualize_ir` currently guarantees these
compose into one positionally-uniform state vector across the whole output,
which is exactly what `movfuscate_ir` needs from *any* input it's given.

**Where this leaves Step A**: `virtualize_ir` and `movfuscate_ir` are both
real, both tested (in isolation), but **not yet compatible with each other**
for a program shaped like this interpreter, despite `docs/virt.md` explicitly
describing the combination as the intended way to get oblivious dispatch.
Making them compose would need either (a) `movfuscate_ir` accepting a
non-uniform-across-blocks input and unifying/padding it itself, or (b)
`virtualize_ir` gaining a "unify all output blocks to one canonical
positional state layout" mode. Neither is a quick fix; this needs dedicated
design work in `volar-ir-virt` and/or `volar-ir-passes`, ideally starting
from a *much* smaller repro (a 2-3-block virt-dedup case, not the full
~120-block interpreter) to isolate the invariant precisely before touching
either crate's real logic.

**Recommendation**: per the plan's own framing ("Step B — the primary,
general mechanism, preferred regardless of Step A's result"), Milestone 1.5
proceeds on Step B alone. Revisit Step A only if Step B's split-per-block
gains turn out insufficient on their own, or once RV32I's larger
instruction set (Milestone 2) makes the register-dispatch repetition (the
thing Step A specifically targets) numerically bigger and worth the
composability work above.

**Bug 1's real-world urgency, upgraded**: this isn't purely a virt-integration
tail risk — per direction received while writing this up, Bug 1
(`compute_expanded_state_slot_types`'s per-position type-uniformity
assumption, currently kept true only by an accidental narrow-continuation-type
bug) will very likely need real fixing **in Milestone 2**, independent of
whether `virtualize_ir` is ever revisited: Milestone 2's Rust-compiled
interpreter introduces genuinely **multiple WASM functions** with real
call/return continuations (unlike Milestone 1's single self-contained loop,
where the only continuation is the synthetic module-entry↔function-0
trampoline), plus more distinct types and more block shapes than a single
instruction-dispatch loop has. That's exactly the shape that stresses this
assumption harder than Milestone 1 does. See the plan's own Milestone 2
section ("Anticipated blocker, flag early") for the concrete recommendation
— budget real investigation time for this early in Milestone 2, starting
from a small two-function repro before scaling up.

## Milestone 1.5 Step B: `store_forward_ir_blocks` appears to *increase*
## and_count 24x post-movfuscation — deferred, dropped for now

**Status: not fixed — post-movfuscation optimization is simply skipped for
the split-weaving path.** While building and measuring Milestone 1.5's
split verifier/prover weave (`weave_vole_verifier_ir_split_with_trace`/
`weave_vole_prover_ir_split`, `crates/compiler/volar-weaver/src/vole.rs`)
against the real interpreter circuit, a direct A/B measurement (same
movfuscated starting point, and_count computed both ways via the *real*
weaver's own `q_and_`-param counting, not a re-implemented diagnostic
formula) found:

- **Without** the post-movfuscation `optimize_to_fixpoint` pass
  (`fold_ir_blocks`/`store_forward_ir_blocks`, run by `lower_interpreter`
  between `movfuscate_ir` and `lower_to_circuit_ir`): and_count = **115,780**.
- **With** it (the pipeline every earlier measurement in this doc used):
  and_count = **2,771,980** — **~24x larger**.

This is backwards from what "optimization" should do, and is the likely
explanation for why post-movfuscation optimization never actually helped
as much as expected (see the width-at-rest section above, which measured
*statement* count reduction, not and_count, and didn't isolate this
effect). The suspected mechanism (not yet confirmed by reading
`store_forward_ir_blocks`'s implementation closely): "store forwarding" —
substituting a computed value's full defining expression at its use
site(s) instead of referencing a shared intermediate — is a sound, common
optimization for ordinary imperative code (fewer live registers), but for
a *circuit*, forwarding an expression with **multiple uses** duplicates
whatever gates that expression contains at every use site. If the forwarded
expression contains AND-monomials (degree ≥ 2) and has more than one use,
each duplication is a real, extra Quicksilver check — a "fewer named
intermediates, more actual gates" trade that looks like an improvement by
statement count while making and_count (the thing that actually drives
verifier-weave cost) worse.

**Why this wasn't chased further right now:** per direction received,
"expensive" AND-heavy duplicated expressions are frequently the *easy* case
to optimize back down — especially when they're layered with memory
indirection (a `StorageRead`'s result gets forwarded to multiple AND sites,
each re-paying the read's own downstream gate cost) — but confirming and
fixing this precisely (likely: only forward single-use values, or CSE-dedupe
instead of forwarding when a value has multiple uses and contains any
AND-monomial) is real, separate investigation work, deferred here rather
than blocking Milestone 1.5's combiner-splitting work.

**What's actually done now:** the split-weaving pipeline
(`lower_interpreter` in `crates/examples/volar-riscv-e2e/src/wat_gen.rs`)
simply **skips post-movfuscation optimization** — both because it's the
regression's proximate cause and because
`movfuscate_ir_with_boundary`'s boundary metadata is invalid after
`fold_ir_blocks`/`store_forward_ir_blocks` run anyway (confirmed directly:
reusing boundary post-optimization panics with "no entry found for key" in
the weaver, since these passes renumber/delete statements). This is a
straightforward win with no known downside *for this pipeline* — the
smaller and_count came from *not* optimizing, so there's nothing lost by
skipping a pass that was hurting.

**Where to pick this up:** `crates/ir/volar-ir-opt/src/store_forward.rs`
(read the actual forwarding decision logic — does it check use-count
before forwarding at all, or forward unconditionally on any store/load
match?), measured against the same real interpreter circuit via
`weave_vole_verifier_ir_split_with_trace`'s q_and-param counting (the A/B
harness already exists as
`crates/examples/volar-riscv-e2e/src/wat_gen.rs`'s
`compare_and_count_with_and_without_post_movfuscation_optimization`,
`#[ignore]`d). If fixed, boundary-metadata compatibility with
post-movfuscation optimization would need re-establishing too (currently
just not attempted, since skipping the pass sidesteps the question
entirely).

## Milestone 1.5 Step B (continued): the combiner itself, chunked — done, validated at real scale

**Status: fixed and measured.** Splitting the verifier/prover into one
function per original (pre-movfuscation) block (above) still left a single
trailing "combiner" function that bound *every* block's exported state
simultaneously — on the real interpreter circuit (120 blocks) this combiner
alone needed ~2.5M Rust function parameters, i.e. the same order of
magnitude as the original unsplit problem, just moved to one function
instead of the whole verifier.

Fix: `movfuscate_ir`'s own accumulation loop (`Σ_i is_active_i · x_i`,
"movfuscate.rs") was restructured to be **block-major** — one contiguous
statement range per block covering all four accumulated quantities
(`done_acc`/`next_pc`/`next_state`/`ret_vals`) instead of four separate
all-blocks loops — and exposed as `MovfuscAccumInfo { init, steps }`
(`MovfuscAccumInit` + one `MovfuscAccumStep` per block), alongside the
existing per-block `MovfuscBlockBoundary`. The weaver
(`weave_vole_verifier_ir_split_with_trace` / `weave_vole_prover_ir_split`,
both now taking `accum_info: &MovfuscAccumInfo, chunk_size: usize`) folds
these accumulation steps in groups of `chunk_size` blocks at a time
("`..._accum_chunk_{i}`" functions), threading a running accumulator
(done_acc/next_pc/next_state/ret_vals — plus, prover-side, each chunk's own
local `hat`s, since the accumulation's own `is_active_i AND done_i`-style
selection logic is itself real AND-gates) between chunks, followed by one
final "`..._finish`" function for whatever trails the last accumulation
step (the terminator-select/return-padding `lower_to_circuit_ir` adds).

**Real-scale measurement** (`measure_split_weave_on_real_interpreter`,
`crates/examples/volar-riscv-e2e/src/wat_gen.rs`, `#[ignore]`d,
`chunk_size = 8`, 120 blocks): the whole weave — 120 block functions + 15
accumulator chunks + 1 finish function, both prover and verifier — now
**completes in ~2.5s with negligible RSS** (previously: 8GB+ RSS and
climbing, killed before completion, on the unsplit combiner). Per-function
param counts: the 120 block functions are all small (669–4,529 params);
the 15 chunk functions and 1 finish function are the new bottleneck, up to
**~236K params** (verifier) / **~220K params** (prover) for the
largest chunk — a real ~10-35x reduction from the prior ~2.5M-8.3M-param
single combiner, and easily tractable (fits comfortably in memory, no
special handling needed). This variance comes from block-to-block
and_count skew (most blocks measure 6-1,282 AND gates; a run of ~17 blocks
near the end of the circuit each measure ~5,000-6,000, so any chunk
containing several of those is proportionally larger) — `chunk_size` is a
tunable knob (smaller chunk_size → smaller max chunk, more functions) if
finer bounding is ever needed; 8 was not specially tuned and already
suffices.

**Verified consistent**: `verifier_and_counts == prover_hats_counts`
per-function (same chunk boundaries on both sides, required for
interleaved driving) — asserted directly in the measurement test, passes.

## Deferred: bitwise-op widening

**Idea:** AND/OR/XOR/NOT on now-wide (`Vec(k,Bit)`-typed) values currently
lower through `BitCircuitBuilder`'s `bc_and_vec`/`bc_or_vec`/`bc_xor_vec`/
`bc_not_vec` (`crates/ir/volar-lir/src/circuits.rs`), which are simple
per-bit loops — each bit becomes its own scalar `Poly` statement, same as
before the width-at-rest fix. Only `Merge`/`Shuffle` (structural
pack/unpack, free) and block-boundary crossings benefit from the fix as
shipped. A wide bitwise op could instead emit **one** wide `Poly`
statement directly (`emit_poly_wide`-eligible: XOR/OR/NOT are degree ≤ 1,
AND is degree 2 — both already within `poly_wide_supported`'s scope),
cutting per-bit statement count for arithmetic/bitwise-heavy code the same
way block-boundary packing cut it for loop-carried state.

**Where:** `bc_and_vec`/`bc_or_vec`/`bc_xor_vec`/`bc_not_vec`'s default
impls in `volar-lir/src/circuits.rs`, or a new width-aware override for
`VaffleTarget`'s own `and`/`or`/`xor`/`not` (`target.rs`) that builds one
wide `Stmt::Poly` per call instead of delegating to the bit-by-bit trait
default.

**Why deferred:** the width-at-rest fix alone already got block-boundary
state down to one packed slot per logical value; whether wide bitwise ops
are *also* worth widening depends on how much of the remaining statement
count (20,182 Poly statements post-fix) is bitwise-op output vs. other
structure (decode/dispatch compares, address arithmetic, movfuscation's
own `is_active` accumulation). Not measured yet — re-run
`count_woven_statements_after_optimization`'s histograms with a
bitwise-op-specific breakdown before implementing, to confirm this is
worth it before spending the effort.

## Deferred: movfuscation-level dedup

Original subplan (from the session that introduced `emit_poly_wide`),
still not implemented:

- **Tunnelled/unchanged-state-slot elimination.** Movfuscation's
  accumulate-every-slot-in-every-block scheme computes
  `is_active · val + (1 - is_active) · old_val` (via `emit_select_slot`,
  `dispatch_accumulator.rs`) for *every* state slot in *every* block, even
  when a given block provably doesn't touch that slot at all (the common
  case — most instructions touch a handful of registers, not all 18+ state
  slots). For a slot a block never writes, this degenerates to
  `is_active · old_val + (1 - is_active) · old_val = old_val` — exploitable
  algebraically (`1·n + 0·x = n`) without needing to touch the general
  case. Since `Σ_k is_active_k = 1` (movfuscation's own invariant — exactly
  one block is active per step), a slot untouched by ALL blocks except the
  identity pass-through could skip the accumulation entirely for that slot
  in that block.
- **`is_active`-bit-pattern-check dedup across blocks.** Each block's
  `is_active` is itself a (possibly large) equality/comparison circuit
  against a dispatch value (e.g. "is opcode == this block's opcode").
  Structurally-identical comparisons across blocks (e.g. two different
  instruction handlers checking the same opcode-field bits against
  different constants) currently re-derive shared subexpressions from
  scratch; standard CSE across the combined movfuscated block would
  dedup these.
- **Block-body CSE / fallthrough merging.** Adjacent blocks with identical
  or near-identical bodies (e.g. two arithmetic ops differing only in
  which ALU function they call) could share structure via ordinary
  common-subexpression elimination once combined into one block by
  movfuscation.
- **Combined fixpoint pass with plugin abstraction**, so these dedup
  passes (plus the existing `fold_ir_blocks`/`store_forward_ir_blocks`)
  run to a shared fixpoint rather than being separate one-shot passes,
  each exposed as a pluggable "plugin" the fixpoint driver iterates over.
- All of the above should be **fuzz-tested** (this repo's established
  `proptest`/`volar-fuzz` idiom) given how easy it is for a dedup pass to
  silently break the `Σ_k is_active_k = 1` invariant or an `is_active`
  gating subtlety.

**Where:** `crates/ir/volar-ir-passes/src/movfuscate.rs`,
`dispatch_accumulator.rs`, and the `optimize_to_fixpoint` driver pattern
already used pre/post movfuscation in
`crates/examples/volar-riscv-e2e/src/wat_gen.rs`'s `lower_interpreter`.

## Deferred: polynomial merging

Remove known-zero terms and deduplicate multiplications for bitvectors —
i.e., a genuine algebraic simplification pass over `Stmt::Poly`'s
`BTreeMap<Vec<CirVar>, u8>` coefficient representation itself (not just
the weaver's codegen of it): cancel monomials whose coefficient is even
(GF(2), so coefficient parity is what matters — already partly exploited
by `emit_poly`'s dispatch, but not as a standalone simplification pass
over the IR), and recognize when two `Poly` statements compute the same
monomial set and can share one computation. This is a different, lower
layer than movfuscation-level dedup (operates on individual `Poly`
statements' algebra, not on movfuscation's block-combination structure)
and could apply independently of it.

**Where:** likely a new pass in `volar-ir-passes`, run in the same
pre/post-movfuscation fixpoint slot as `fold_ir_blocks`.

## Deferred (older, lower priority): sharding + cold-state-to-memory

From the same original subgoal list, not revisited this session:

- **Sharding woven output across multiple functions** for parallel
  `rustc` compilation of the generated Rust source — a codegen-time
  build-speed concern, not a circuit-size concern; matters once circuits
  are large enough that a single `rustc` invocation on one giant function
  becomes the bottleneck (this was the original trigger: a 46MB/117K-gate
  single-function dump was infeasible to compile at all). With the
  width-at-rest fix's ~55% reduction, this may no longer be as urgent —
  re-measure real `rustc` compile time on the reduced circuit before
  prioritizing this.
- **IR pass moving cold/register state back to memory** — the inverse of
  this session's fix, for state that's touched rarely enough that keeping
  it as a movfuscation state slot (paid every step, every block) costs
  more than a `StorageRead`/`StorageWrite` pair would (paid only when
  actually touched). Requires a cost model (slot-carry cost across N
  blocks vs. read/write cost) to know when this trade is actually worth
  it — not attempted.

## Recommended next step if this backlog is picked up

Re-run `count_woven_statements_after_optimization` (extend its histograms
with a per-`Stmt`-kind breakdown, not just Poly width/degree) against
Milestone 2's expanded RV32I circuit once that exists — a fuller
instruction set and larger register file will change which of the above
deferred items has the most leverage, and re-measuring against a more
representative circuit is cheaper than guessing.
