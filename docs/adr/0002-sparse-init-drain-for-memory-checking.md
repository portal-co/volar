# ADR 0002: Sparse (non-exponential) init/drain for committed memory checking

**Status:** **Implemented (Option A).** `weave_ts_storage_loop_*` and
`weave_hybrid_net_vole_*` now use the touched-cell witness list with `emit_lt`
sortedness + the `r0` constant-term `encode` hardening; the `addr_bits` cap is
lifted to `1..=64` and init/drain are `Θ(K)`. Option B (sorted-trace permutation)
remains the future gold-standard for fully uniform RAM.
**Tier of recommendations:** Tier 2 (touches `volar-weaver`; reuses existing
`volar-spec` primitives — no new spec surface required for the recommended path).
**Prepared by:** Opus 4.8, session 2026-06-03, VCB timestamp-soundness arc.

---

## Why this ADR exists

The timestamp-sound storage-loop weaver
([`weave_ts_storage_loop_{prover,verifier}`](../../crates/compiler/volar-weaver/src/storage_loop.rs))
and the continuation-bridge memory model
([`docs/vole-continuation-bridge.md`](../vole-continuation-bridge.md) §9)
make multiset memory checking *sound* — a read provably returns the last value
written, enforced by per-access timestamp ordering (`emit_lt`) plus a
produce/consume **drain**.

The drain (and the matching **init**) currently iterate **every** cell:

```
init  (entry): produce (addr=c, val=0, ts=0)        for c in 0 .. 2^addr_bits
drain (exit):  consume (addr=c, final_val_c, ts_c)  for c in 0 .. 2^addr_bits
```

This is `Θ(2^addr_bits)` gates **and** function parameters (the `final_val{c}` /
`final_ts{c}_{j}` witnesses, the `q_final_*` mirrors). The current weaver
asserts `addr_bits ∈ 1..=8` precisely because the construction explodes beyond
that.

**This is fine — and intended — for small-scale uses:** unit tests, fixtures,
retro-emulation address spaces (a handful of registers / flags), and any circuit
whose addressable memory is genuinely tiny. Those callers should keep using the
dense path; it is simpler and its soundness argument is self-evidently a closed
multiset.

**It does not scale.** A realistic RAM (say `2^20`–`2^32` cells) cannot
materialise a produce/consume per cell. The number of init/drain entries must be
**linear in the number of accesses**, independent of the address width. That is
what this ADR specifies.

The toolkit to do this **already exists** (multiset hash + `emit_lt` ordering +
`emit_incr` counter + bit-pack); the work is a new init/drain *shape*, not new
cryptography.

---

## Background: the invariant we must preserve

Multiset memory checking (Blum et al. 1991; Spice/vSQL/TinyRAM style) maintains
two multisets over `(addr, value, ts)` tuples:

- **produce** (`mem_prod`): every write of a value into a cell, including the
  cell's initial state.
- **consume** (`mem_cons`): every read of a cell's current state, including a
  final drain of every live cell.

Per access to address `a`: `consume(a, old, t_last)`, prove `t_last < counter`
(ordering — closes the "consume a future write" attack), `produce(a, new,
counter)`, `counter += 1`.

**Soundness theorem (informal):** if `mem_prod == mem_cons` as multisets AND
every consumed tuple was ordered strictly before the global counter at consume
time AND the counter is strictly monotone, then every read returned the value of
the most recent write to that address. The drain check
([`mem_drain_open`/`mem_drain_check`](../../crates/spec/volar-spec/src/vole/bridge.rs))
proves the multiset equality succinctly (one field element + one opening); the
ordering is the `order_ok` AND-fold opened via `assert_one_check`.

The init/drain exist only to *close the multiset*: the first access to a cell
consumes a tuple that must have been produced (→ init produces it); the last
value produced for a cell is never re-consumed by an access (→ drain consumes
it). **Sparsity is the observation that only *touched* cells need closing** —
untouched cells produce and consume nothing.

---

## Options

### Option A — Touched-cell witness list (simple, sound, a good first step)

The prover supplies a witness list of the `K` *distinct touched addresses*
`a_0 < a_1 < … < a_{K-1}`, each with an initial value and a final `(value, ts)`:

- **init:** `produce(a_i, init_val_i, 0)` for `i in 0..K`.
- **drain:** `consume(a_i, final_val_i, final_ts_i)` for `i in 0..K`.
- **distinctness/sortedness:** prove `a_i < a_{i+1}` with `emit_lt` on the
  committed address bit-vectors (we already lower this gadget on both sides).
  Sortedness ⇒ the `a_i` are distinct, so no cell is double-initialised.

`K ≤ #accesses` (a cell touched many times appears once), so init/drain are now
linear in the trace, not in `2^addr_bits`. `K` is a public per-segment bound
(the witness slice length), like `read_count`.

**Why it's sound.** The multiset still closes: any produced tuple not matched by
a consume (or vice-versa) makes `mem_prod − mem_cons ≠ 0`, caught by the drain.
A cheating prover who omits a touched cell from the list leaves that cell's last
produce unconsumed (imbalance). One who lists an untouched/duplicate address
either creates an unconsumed init (imbalance) or violates `a_i < a_{i+1}`
(ordering check fails). The sortedness gadget is the only addition over the
dense path.

**Caveat.** The prover must declare `K` (or an upper bound, padding with a
sentinel "no-op" address that produces and consumes the *same* zero tuple so it
nets out). Padding to a fixed `K_max` keeps the circuit shape static while
hiding the true touched-count if that matters for ZK.

### Option B — Sorted-trace permutation (the scalable gold standard)

The classic offline-memory-checking construction. Build a second, **address-then-
timestamp-sorted** copy of the access trace as a witness, and:

1. Prove the sorted trace is a **permutation** of the access trace — this is
   exactly multiset equality, which our additive hash already provides (hash the
   access-order tuples into one accumulator, the sorted-order tuples into
   another, drain-check equality).
2. Prove **local consistency** between consecutive sorted entries:
   - same address ⇒ `value` carries forward and `ts` strictly increases
     (`emit_lt`);
   - address increases (`emit_lt` on the address fields) ⇒ a new cell begins, so
     its first entry must be an init `(a, init_val, 0)`.

Init/drain become *boundary conditions of the sorted trace* (first/last entry
per address run) rather than a per-cell sweep. Cost is `Θ(#accesses)` and fully
independent of address width. This is what TinyRAM/Spice use and is the right
end state for arbitrary RAM.

**Cost vs A.** B needs a full sorted-trace witness (another `(addr,val,ts)` per
access) and a consecutive-consistency gadget per step, but it is *uniform* — no
per-cell parameters at all, and it directly generalises to multi-bit addresses
and to the continuation bridge (the sorted trace is itself succinctly carried by
its multiset hash).

### Option C — Keep dense, cap `addr_bits` (status quo)

Do nothing; document the cap. Correct for tests/retro-emulation only. Listed for
completeness — this ADR exists because C does not scale.

---

## Recommendation

**Adopt Option A first, then Option B.**

- **A** is a small, self-contained delta over the shipped dense path: replace the
  `for c in 0..cells` init/drain loops with a `for i in 0..K` loop over a
  touched-address witness slice, and add one `emit_lt(a_i, a_{i+1})` sortedness
  check per consecutive pair (lowered to hats via the existing
  `lower_gadget_{prover,verifier}`). It removes the exponential blow-up
  immediately and unblocks realistic address widths, with a soundness argument
  that is a direct extension of the one already in §9.

- **B** is the principled end state and should follow once A is in and the sorted-
  trace witness plumbing is worth building. B subsumes A (A is "B with the sort
  given implicitly by the touched-list order"). Do B when a real RAM workload
  needs it; until then A covers everything short of that.

Both reuse the existing toolkit with **zero new `volar-spec` surface**:
`mem_acc_absorb_{vope,q}`, `vope_bitpack`/`q_bitpack`, `emit_lt`, `emit_incr`,
`lower_gadget_{prover,verifier}`, `mem_drain_{open,check}`, `assert_one_check`.

---

## Implementation sketch (Option A)

In [`storage_loop.rs`](../../crates/compiler/volar-weaver/src/storage_loop.rs):

1. **Signature.** Drop the `2^addr_bits` `final_val{c}`/`final_ts{c}_*` params.
   Add `touched_count: usize` (`K`) and witness slices:
   - prover: `touched_addr: &[Vope]` (the `K·addr_bits` committed address bits,
     strided like `read_last_ts`), `touched_init_val: &[Vope]`,
     `touched_final_val: &[Vope]`, `touched_final_ts: &[Vope]` (`K·ts_bits`).
   - verifier: the `q_*` mirrors.
2. **Entry (init).** `for i in 0..K`: bit-pack `touched_addr[i]` →
   `addr_field_i`; `produce(addr_field_i, touched_init_val[i], 0)`.
3. **Exit (drain).** `for i in 0..K`: `consume(addr_field_i, touched_final_val[i],
   bitpack(touched_final_ts[i]))`; and for `i in 1..K` fold
   `order_ok &= emit_lt(addr_{i-1}, addr_i)` (strict sort ⇒ distinct). Open
   `order_ok` (already opened) — the sort ANDs join the existing per-iteration
   hat stream **or** a final single-shot batch (decide: simplest is to emit the
   sort checks in the exit block and extend the drain/order opening to cover
   them; the exit block is not in the loop so a one-shot `send_hats` batch is
   cleanest — note the exit currently only opens, so add a small fixed
   `send_iteration`/`recv_iteration` for the `K-1` sort ANDs, or reuse
   `send_hats`/`recv_hats`).
4. **Per-access body** is unchanged (it already consumes `t_last`, orders, and
   produces — none of that depends on init/drain shape).
5. Mirror all of the above in the verifier (`q_bitpack`, `mem_acc_absorb_q`,
   `lower_gadget_verifier` for the sort ANDs, fold into `all_ok`).

**`AND_COUNT` accounting.** The sort adds `(K−1)` `emit_lt` gadgets' worth of
ANDs at the boundary. Keep prover/verifier agreement by counting them with the
same `ts_iter_and_count`-style helper (extend it, or count the boundary batch
separately). The per-iteration loop AND count is unchanged.

**Hazard to watch.** The zero-tuple corner case (`encode(0,0,0) = 0`, since
`encode` has no constant term) interacts with sparse init: a touched cell at
address 0 with init value 0 contributes nothing to the produce hash, so its
"closing" is invisible. The dense path dodged this by starting the counter at 1;
under sparse init, prefer the **constant-term `encode`** hardening (add a public
`r0 ≠ 0`: `encode = r0 + addr·r1 + value·r2 + ts·r3`) so *every* tuple — including
`(0,0,0)` — is visible in the multiset. This is a one-line change in
[`bridge.rs`](../../crates/spec/volar-spec/src/vole/bridge.rs) `mem_acc_absorb*`
plus a public `r0` param, and it removes the corner case for both A and B. Do it
as part of A.

---

## Affected files

- [`crates/compiler/volar-weaver/src/storage_loop.rs`](../../crates/compiler/volar-weaver/src/storage_loop.rs)
  — `weave_ts_storage_loop_{prover,verifier}` init/drain (and the resume variant,
  if present); `ts_iter_and_count` accounting; remove the `addr_bits ≤ 8` cap.
- [`crates/compiler/volar-weaver/src/hybrid_net.rs`](../../crates/compiler/volar-weaver/src/hybrid_net.rs)
  — once its storage absorbs adopt the committed-counter scheme, the same sparse
  init/drain applies (its init is in B0, drain at the B3 exit).
- [`crates/spec/volar-spec/src/vole/bridge.rs`](../../crates/spec/volar-spec/src/vole/bridge.rs)
  — optional but recommended constant-term `encode` (`r0`) hardening.
- [`docs/vole-continuation-bridge.md`](../vole-continuation-bridge.md) §9 — update
  the init/drain description from dense to sparse.

## Verification

- Compile-checks (existing pattern): weave a multi-touch loop, assert the
  generated init/drain are `Θ(K)` not `Θ(2^addr_bits)` (e.g. param count grows
  with `touched_count`, not `addr_bits`).
- Soundness regression (when an executable harness exists): a forged read on an
  untouched / mis-sorted cell must make either the drain or the sort-order check
  reject.
- Keep the dense path (or A with `K = 2^addr_bits`) green for the small-scale
  callers.

---

## Non-goals

- Gate-succinctness in the *gap length* `m` (VCB-IVC folding) — separate frontier
  (`volar-fold`), see [`vole-continuation-bridge.md`](../vole-continuation-bridge.md) §3.3.
- ORAM-style access-pattern hiding — orthogonal; this ADR is about *correctness at
  scale*, not pattern privacy. (The committed counter already hides access
  *timing*; addresses in the trace are still visible to the extent the circuit
  reveals them.)
