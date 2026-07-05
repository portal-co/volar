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
