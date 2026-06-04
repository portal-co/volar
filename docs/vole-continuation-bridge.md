# VOLE Continuation Bridge (VCB)

> **Status: design-of-record. Specified, not yet implemented.**
>
> The hybrid network VOLE weaver (`volar-weaver/src/hybrid_net.rs`) ships with a
> **best-effort** resumption bridge by default (the gap runs in the clear and the
> verifier's verdict is *qualified*: "valid except iterations `[k, k+m]`"). This
> document fully specifies the **sound, succinct** alternative — the VOLE
> Continuation Bridge — which slots in behind the same
> `volar_net::ResilientVoleTransport` / `BridgeStrategy` interface without any
> change to generated code.

---

## 1. Motivation

The streaming network VOLE weaver (`weave_net_vole_prover_loop`,
`weave_net_vole_verifier_loop`) runs a Quicksilver VOLE-in-the-head proof one
loop iteration at a time. A crucial property of the protocol:

> **The prover computes every AND-gate hat `V̂_g` locally** (via
> `vole_and_prover_step`). It never needs the verifier to make progress. Only
> `send_iteration` / `recv_iteration` / `recv_verdict` touch the network.

Therefore, if the network drops, the prover *could* simply buffer hats and flush
on reconnect — fully sound, no new crypto. The reason that is **not always
acceptable**, and the reason this bridge exists, is resource exhaustion:

- **VOLE correlation budget.** The pre-generated VOLE correlations (from the
  out-of-scope OT-based setup) are a *finite* one-time resource. Every AND gate
  consumes one. A long outage during which the prover keeps proving can exhaust
  the budget provisioned for the expected proof length.
- **Hat-buffer memory.** Buffered hats grow without bound across a long outage.

The hybrid weaver's answer: during an outage, drop to the **no-op (cleartext)
path** — compute `F` in the clear, consuming **no** correlations and **no**
unbounded buffer. The cost is that the gap iterations are unauthenticated. The
**continuation bridge** is what restores authentication on reconnect.

---

## 2. Setting and notation

- `F` — the public per-iteration transition circuit:
  `F : {0,1}^ℓ → ({0,1}^ℓ, done)`, mapping the live state to the next state plus
  a termination flag. This is exactly the single-block loop circuit the net loop
  weavers consume.
- `ℓ` — the number of **live state wires** crossing the loop back-edge. These are
  the only wires that persist across iterations; all internal gate wires are
  ephemeral. **The bridge only ever handles these `ℓ` boundary wires.**
- VOLE relation (Quicksilver): a committed wire bit `x` is authenticated by
  prover MAC `M ∈ T^N` and verifier key `K ∈ T^N` with
  `K = M + x·Δ`, where `Δ ∈ T^N` is the verifier's fixed session secret and `T`
  is the extension field (`Galois64` / `GF(2^128)`). In `volar-spec` types: the
  prover wire is `Vope<N, T, U1>`, the verifier wire is `Q<N, T>`, and `Δ` is
  `Delta<N, T>`.
- **Cut** at iteration `k`: the network drops. Live state is `S_k ∈ {0,1}^ℓ`.
  Prover holds `{M_i}_{i<ℓ}`, verifier holds `{K_i}_{i<ℓ}` with
  `K_i = M_i + S_k[i]·Δ`.
- During the outage the prover computes, in the clear,
  `S_{k+1}, …, S_{k+m} = F^m(S_k)` (where `m` is the gap length).
- **Reconnect**: the verifier must accept that `S_{k+m} = F^m(S_k)` and the
  streaming proof must resume from a freshly-committed `S_{k+m}`, subject to:
  - **(a)** no per-iteration streaming correlations consumed *during* the gap, and
  - **(b)** ideally, reconnect cost **independent of `m`** (succinct) — otherwise
    the no-op gap saved nothing over having stayed in ZK.

---

## 3. Construction

Three components: **anchor**, **re-commitment**, **bridge proof**.

### 3.1 Anchor (no new message at cut time)

The cut point is named by a transcript hash `τ_cut = H(transcript ‖ k)`. When the
verifier's transport classifies an error as a disconnect, it **retains** the live
keys `{K_i}` and the index `k`. The prover symmetrically retains `{M_i}` and the
cleartext `S_k`. These pre-existing VOLE shares *are* the anchor — no extra
commitment is transmitted at cut time (the link drops; there may be nothing to
transmit to).

### 3.2 Re-commitment at resume (fresh VOLE)

On reconnect the prover draws `ℓ` **fresh** VOLE correlations and commits the
resumed state `S_{k+m}`: fresh MACs `{M'_j}`, verifier keys
`{K'_j} = M'_j + S_{k+m}[j]·Δ`. The same `Δ` is reused — VOLE authentication is
secure under a fixed `Δ` across *independent* correlations within one session.
Cost `O(ℓ)`. This is a standard "commit `ℓ` new wires" step; the streaming loop
then resumes from `{K'_j}`.

> **Soundness hinge.** Re-commitment alone is *not* sound: a cheating prover could
> commit any `S_{k+m}` it likes. The bridge proof (§3.3) is what forces
> `S_{k+m} = F^m(S_k)`.

### 3.3 Bridge proof: `S_{k+m} = F^m(S_k)`

Two instantiations, both behind the `BridgeStrategy` interface.

#### VCB-RX — re-execution (sound, **not** succinct)

Re-run `F` for the `m` gap iterations *under VOLE*, starting from the anchor MACs
`{M_i}` plus fresh correlations for the internal AND gates of those iterations.
The prover produces hats; the verifier runs the ordinary per-gate Quicksilver
checks and finally checks that the produced final-state wires equal the
re-committed `{K'_j}` (a free VOLE linear-equality check).

- **Soundness:** exactly that of the base streaming protocol.
- **Cost:** `m × (AND gates per iteration)` fresh correlations + messages — it
  **defers** the budget spend to reconnect rather than **saving** it. Use this as
  the guaranteed-sound fallback when the succinct variant's embedding (below) is
  impractical. Trivial to implement given the existing per-iteration weaving.

#### VCB-IVC — succinct folding + VOLE-MAC linking (**novel target**)

The win — reconnect cost independent of `m`, zero streaming correlations spent
during the gap.

1. **Incremental folding during the gap.** As it computes each cleartext step,
   the prover maintains an incremental proof `π_t` of
   "`S_t = F^{t-k}(S_k)`" using a Nova-style folding / IVC scheme over the
   relaxed-R1CS arithmetization of `F`. Each step folds one instance:
   `π_t = Fold(π_{t-1}, w_t)`, where `w_t` witnesses `F(S_{t-1}) = S_t`. After the
   gap, `π_{k+m}` has size `O(|F|)`, **independent of `m`**, and folding consumes
   **no VOLE correlations**.

2. **Boundary commitments.** The folding instance exposes two homomorphic vector
   commitments to the boundary states:
   - `C_in  = Commit(S_k;     r_in)`
   - `C_out = Commit(S_{k+m}; r_out)`

   using a homomorphic vector commitment, e.g. Pedersen
   `Commit(x; r) = Σ_i x_i·G_i + r·H`.

3. **Linking argument (the crux).** The verifier must be convinced that the bits
   inside `C_in` equal the bits authenticated by the anchor `{K_i}`, and the bits
   inside `C_out` equal the re-committed `{K'_j}`. Because VOLE authenticates
   **linear combinations for free**, the prover proves *in VOLE* the two linear
   predicates:

   ```
   Σ_i  S_k[i]    · g_i  =  open(C_in)      // boundary-in  link  (vs anchor {K_i})
   Σ_j  S_{k+m}[j]· g_j   =  open(C_out)     // boundary-out link  (vs fresh  {K'_j})
   ```

   where `g_i` are public field encodings of the commitment generators. Each is a
   single VOLE-authenticated linear sum (`O(ℓ)` free operations) plus one opening
   check against the cleartext `C_in` / `C_out`. The IVC's own knowledge-soundness
   covers the `m` internal steps succinctly.

**Reconnect cost (VCB-IVC):** one folding verification `O(|F|)` + `2·O(ℓ)` VOLE
linear boundary links + `O(1)` opening checks — independent of `m`, zero
streaming correlations consumed during the gap.

---

## 4. Message flow (VCB-IVC, on reconnect)

```
Prover → Verifier:  τ_cut, gap_len = m
Prover → Verifier:  C_in, C_out, π_{k+m}                       (folding proof + boundaries)
Prover → Verifier:  fresh VOLE commitments {K'_j} for S_{k+m}  (re-commitment)
Prover ⇄ Verifier:  VOLE linear-link check for C_in  (against anchor {K_i})
Prover ⇄ Verifier:  VOLE linear-link check for C_out (against fresh  {K'_j})
Verifier:           verify π_{k+m}; if all checks pass → GapVerdict::Proven
Resume streaming VOLE loop from {K'_j}.
```

For the **best-effort default** strategy this collapses to:

```
Prover → Verifier:  gap_len = m, fresh VOLE commitments {K'_j} for S_{k+m}
Verifier:           record GapVerdict::Unproven { start: k, end: k+m }
Resume streaming VOLE loop from {K'_j}.
```

---

## 5. Security argument (sketch — VCB-IVC)

- **Completeness.** An honest prover's folded proof verifies, and both linear
  links hold because the same bit-vectors `S_k` / `S_{k+m}` define both the VOLE
  MACs and the commitments.

- **Soundness.** To make the verifier accept a wrong `S_{k+m} ≠ F^m(S_k)`, a
  cheating prover must either
  1. break IVC knowledge-soundness for the statement `S_{k+m} = F^m(S_k)`
     (reduces to the folding scheme's soundness under its commitment's binding
     property), **or**
  2. pass a boundary link with inconsistent bits — i.e. open a boundary
     commitment to bits differing from the VOLE-authenticated bits while the
     *free* VOLE linear check still accepts. That requires either guessing `Δ`
     (probability `≤ |T|^{-1}` per the standard Quicksilver linear-check
     soundness bound) or breaking the commitment's binding.

  Union-bound over the two boundary links and the IVC gives negligible soundness
  error.

- **Zero-knowledge.** The gap transcript reveals only `C_in`, `C_out`, `π` (hiding
  under the commitment blinders `r_in`, `r_out` and the IVC's ZK property) and the
  gap length `m`. It leaks `m`; pad the gap to a fixed granularity if `m` must be
  hidden.

---

## 6. Assumptions and open problems (stated honestly)

- **Field/group embedding (main open task).** VCB-IVC needs a folding/IVC scheme
  whose boundary commitment is a homomorphic vector commitment over a field that
  **embeds into the VOLE field `T`** (`Galois64` / `GF(2^128)`), so the linking
  predicate is a free VOLE linear combination. Pairing a concrete IVC (e.g. Nova
  over a pairing-/curve-friendly field) with the binary-extension VOLE field is
  non-trivial; a literal Pedersen opening may need to be replaced by a small
  in-circuit hash-to-field gadget that *is* expressible in the VOLE field. This is
  the principal piece of novel cryptographic engineering and should be prototyped
  before committing to VCB-IVC over VCB-RX.
- **`Δ`-reuse.** Reusing `Δ` across the gap is sound within a single session but
  `Δ` must never be exposed; the re-commitment must use fresh, independent
  correlations.
- **Gap-length leakage.** `m` leaks unless padded.
- **Fallback.** VCB-RX is the guaranteed-sound, no-novel-crypto fallback if the
  VCB-IVC embedding proves impractical. The hybrid weaver's `BridgeStrategy`
  interface lets the three strategies (best-effort, VCB-RX, VCB-IVC) be swapped
  with **no change to generated code**.

---

## 7. Interface (`volar-net`)

The generated hybrid code is strategy-agnostic; it calls into the runtime trait:

```rust
pub enum SendOutcome<E> { Continue, Disconnect, Fatal(E) }   // documentation enum

pub struct ResumeToken<N: ArraySize, T> {
    pub resume_state: Vec<Vope<N, T, U1>>,  // anchor (replay) or fresh (re-commit) wires
    pub mem_acc:      Vec<Vope<N, T, U1>>,  // carried multiset-hash accumulator wires
}

pub enum GapVerdict { Proven, Unproven { start: u32, end: u32 } }

pub trait ResilientVoleTransport<N: ArraySize, T>: VoleTransport<N, T> {
    // Result<bool>: Ok(true)=sent/continue, Ok(false)=disconnect, Err=fatal.
    // (Returns Result<bool> rather than the SendOutcome enum because CFG
    //  terminators are only Return/Goto/CondGoto — see hybrid_net.rs.)
    fn try_send_iteration(&mut self, hats: &[Array<T, N>], is_sentinel: bool) -> Result<bool, Self::Error>;
    fn try_reconnect(&mut self) -> Result<bool, Self::Error>;
    fn prover_bridge(&mut self, token: &ResumeToken<N, T>, gap_len: u32) -> Result<(), Self::Error>;
    fn verifier_bridge(&mut self, gap_len: u32) -> Result<GapVerdict, Self::Error>;
    fn fresh_commit(&mut self, bits: &[bool]) -> Result<Vec<Vope<N, T, U1>>, Self::Error>;
}
```

- **Best-effort default:** `prover_bridge` no-op; `verifier_bridge` returns
  `GapVerdict::Unproven`. All methods have defaults so a bare
  `impl ResilientVoleTransport for X {}` works.
- **VCB-RX (shipped path):** generated code replays from the anchor, so the
  transport's `prover_bridge` only re-synchronises the channel; a sound transport
  returns `GapVerdict::Proven` from `verifier_bridge`.

---

## 8. Implementation status & remaining integration (this repo)

**Built and tested:**
- `volar_net::ResilientVoleTransport` (+ `SendOutcome`, `ResumeToken { resume_state,
  mem_acc }`, `GapVerdict`, `fresh_commit`, placeholder `recommit_bit`).
- `hybrid_net.rs` prover CFG implements **sound replay-from-anchor (VCB-RX)**: the
  pre-gap anchor wires are carried through the gap blocks and the gap is re-proven
  by re-entering the ZK loop body from them on reconnect — no placeholder
  re-commitment in the bridge path.
- `volar_spec::vole::bridge`: `vole_rekey_prover` / `vole_rekey_verifier_check`
  (XOR-key re-commitment, free linear check), `mem_acc_absorb` (plain-field
  additive multiset-hash accumulation), and `vope_scale_const` +
  `mem_acc_absorb_vope` (the **in-circuit** encode/accumulate on VOPE wires via
  free public-scalar scaling) — all unit-tested.
- **Storage-aware loop weaver** `volar_weaver::weave_storage_commit_loop_prover`
  (`storage_loop.rs`): the first weaver that threads a **loop-carried in-circuit
  multiset accumulator** (`mem_prod`/`mem_cons`/`ts`) through a dynamic loop,
  fed by `StorageRead`/`StorageWrite`, exiting with `(output, mem_prod, mem_cons)`.
  Single-bit address; linear gates only (no AND/transport in this slice).
  Compile-checked against a Commitment-mode loop fixture. This is the
  infrastructure that unblocks A1/C2/C3.

- **Accumulator carried through the hybrid gap (A1 — DONE).** The hybrid prover
  (`hybrid_net.rs`) now threads `mem_prod`/`mem_cons`/`ts` as part of the **anchor
  bundle** (`[w_0..w_{ℓ-1}, mem_prod, mem_cons, ts]`, suffixes `_mp`/`_mc`/`_ts`)
  through every gap block (B4–B8) and restores it to the ZK body on replay — so
  the memory accumulator **provably survives a network cut**. Each ZK iteration
  folds its output into `mem_prod` via `mem_acc_absorb_vope`; the prover returns
  `(output, mem_prod, mem_cons)`. (The per-iteration absorb here digests the
  output as a stand-in; `storage_loop.rs` shows the same accumulator fed by real
  `StorageRead`/`StorageWrite`.)

- **Unified prover (DONE).** `hybrid_net.rs` now does it all in one weaver: the
  ZK body (B1) performs **real** storage absorbs (`StorageRead`/`StorageWrite`
  fed by per-iteration `read_vals`/`write_olds` slices indexed by a carried
  `iter`) alongside AND/hats + transport, while the accumulator `mem_prod`/
  `mem_cons`/`ts` **and** `iter` are carried through the gap as the anchor bundle
  and restored on replay (so replay re-reads the same slices and re-absorbs).
  The cleartext gap path tolerates storage as a *provisional* placeholder (reads
  → `false`), which is sound because the gap is replayed from the anchor.
  Single-bit address. Tested by `test_hybrid_storage_unified`.

- **Verifier-side accumulator + drain check (DONE).** The hybrid verifier now
  carries Q-share accumulators `mem_prod_q`/`mem_cons_q`/`mem_ts_q` through its
  loop, mirrors the prover's storage absorbs in Q-space
  (`mem_acc_absorb_q`, fed by `read_vals_q`/`write_olds_q`), and at exit runs the
  **drain check**: the prover sends `M_diff = mem_prod.v ⊕ mem_cons.v`
  (`mem_drain_open` + `send_mem_opening`); the verifier checks
  `K_prod ⊕ K_cons == M_diff` (`recv_mem_opening` + `mem_drain_check`), which
  holds iff the committed produce/consume multisets match, and folds the result
  into the verdict (`verdict = all_ok && mem_ok`). Two-party memory consistency
  is now actually *checked*, not just carried. Tested by
  `test_hybrid_storage_verifier`.

**Not yet built — remaining integration (specified here):**
- **Full read-consistency soundness — see §9.** The multiset balance + drain check
  are wired and sound *for what they check*, but the **timestamp-ordering** that
  makes the multiset a genuine memory-consistency proof needs the comparison
  **gadget** (§9). The gadget itself (`emit_lt`) is **built and exhaustively
  tested** (`gadgets.rs`); the weaver integration (bit-vector timestamps,
  per-cell last-write-ts consume, init/drain) is specified in §9.
- **C2/C3 — SHIPPED.** `volar_weaver::glue::weave_continuation_glue_{prover,verifier}`
  (`GlueMode::{AdditiveHashCarry, XorRekey}`) is the shared "bind pre-skip →
  post-skip committed state" helper, compile-checked:
  - `XorRekey` (dynamic skip): per boundary wire `rekeyed = vole_rekey_prover(snapshot,
    key)` (free `Add`/XOR), bound by `vole_rekey_verifier_check`
    (`q_rekeyed == q_snapshot + q_key`, no `Δ`). Keys are fresh commitments from
    `ResilientVoleTransport::fresh_commit`.
  - `AdditiveHashCarry` (bridge / replay-from-anchor): identity carry — nothing to
    re-commit (the same anchor is replayed; consistency closed by the drain).
  - **Layering (important):** the binding is VOLE-level, so it is **not** injected
    into the Boolar-only `lower_to_circuit`. That pass instead exposes
    `lower_to_circuit_with_boundary` → `SkipBoundary { state_width, has_done_flag }`,
    handing the glue its `ell` (= carried-state width). Static lowering unchanged.
- **VCB-IVC gate succinctness (§3.3) — IMPLEMENTED (scaffold).** The folding
  bridge now exists as the [`volar-fold`](../crates/fold/volar-fold) crate —
  *Nova minus the zkSNARK* (native interactive final check ⇒ single Ed25519
  curve, no cycle), with `F_ℓ`, Pedersen, (relaxed) R1CS, NIFS folding, native
  verify, the gap-folding IVC driver, and a `FoldingBridge<L>` strategy (25 unit
  tests). The reconnect cost is `O(|F|)`, **independent of `m`**. The
  VOLE-MAC ↔ commitment **embedding** is abstracted behind the `BoundaryLink`
  trait (shipped `DummyLink` is a non-sound placeholder); a sound embedding +
  binding (hash-to-curve) generators are the isolated open tasks. Full design:
  [`vcb-ivc-folding.md`](vcb-ivc-folding.md). VCB-RX remains the
  ready-to-use sound fallback while those are discharged.

---

## 9. Timestamp soundness & the comparison gadget

### The attack (multiset balance is not enough)

The in-circuit multiset memory check accumulates `(addr, value, ts)` tuples into
`mem_prod` / `mem_cons` and checks `mem_prod == mem_cons` at the drain (§8).
**Multiset balance alone does not prove read-consistency.** Concretely, a
cheating prover can make a read return a *future* write's value:

- op 2 (read `a`): consume `(a, v, t=100)`, produce `(a, v, cur=2)`.
- op 100 (write `a=v`): produce `(a, v, 100)`, consume the then-current entry.

Over the whole trace `(a, v, 100)` is produced once (op 100) and consumed once
(op 2), so the multisets **still balance** — yet the op-2 read returned a value
only written at op 100. The fix is a per-access **timestamp-ordering check**:
the consumed timestamp must be strictly less than the current timestamp,
`t_last < cur_ts`, which rules out consuming not-yet-produced entries.

### The gadget (built + tested)

`volar_weaver::gadgets::emit_lt` synthesises an unsigned less-than over
committed bit-vectors using the equal-prefix recurrence (MSB→LSB):

```text
lt = 0; eq = 1
for i = n-1 .. 0:  lt |= eq & ¬a_i & b_i ;  eq &= ¬(a_i ^ b_i)
```

emitted as `And`/`Or`/`Xor`/`Not` `BIrStmt` gates, so the VOLE weaver lowers them
normally (XOR/NOT free; AND → `vole_and_prover_step` + hat). It is verified by an
**exhaustive** test (`lt_gadget_exhaustive`: all `a, b` for widths 1–4) against
the integer reference. This keeps timestamps **private** (committed), unlike a
public-timestamp scheme which would leak the access-timing pattern.

### Built toolkit (all tested)

- `gadgets::emit_lt` — committed less-than (exhaustive test, widths 1–4).
- `gadgets::emit_incr` — committed `+1` ripple counter (exhaustive test).
- `storage_loop::lower_gadget_prover` — lowers a boolean gadget to VOLE prover IR
  (XOR/NOT free; AND/OR → `vole_and_prover_step` + hat). Validated end-to-end by
  `weave_lt_check` + `test_lt_check_lowers_to_vole` (gadget → hats, compiles).
- `bridge::{vope_bitpack, q_bitpack}` — link committed timestamp bits to the
  multiset field value (`Σ bits·pow2`), tested.
- `bridge::{mem_acc_absorb_vope, mem_acc_absorb_q, mem_drain_open, mem_drain_check}`
  — the multiset absorb + drain (tested).
- `bridge::{vope_open_mask, assert_one_check}` — value-1 opening: prover reveals a
  committed wire's MAC mask, verifier checks `K + opening == Δ` (holds iff the
  wire is `1`). Used to enforce the AND-folded `order_ok` (tested).
- `storage_loop::lower_gadget_verifier` — `Q`-side mirror of the gadget lowering
  (ANDs → `vole_and_verifier_check`, hats consumed from a slice; validated by
  `weave_lt_check_verifier`).

So every cryptographic primitive for sound, private timestamp ordering exists
and is tested.

### Integration — SHIPPED (`weave_ts_storage_loop_{prover,verifier}`)

`storage_loop::weave_ts_storage_loop_prover` / `_verifier` are the
**timestamp-sound** storage loop (single-bit address, `ts_bits`-wide counter),
both compile-checked (`test_ts_storage_loop_{prover,verifier}_compiles`).
Mechanism:

- **counter** is a *committed* `ts_bits`-bit bit-vector carried through the loop,
  initialised to `1` and incremented per access with `emit_incr` (its ANDs
  streamed as hats). The produce-side timestamp is `bitpack(counter)` (free
  linear `Σ bit_i · 2^i`).
- **t_last** (the addressed cell's last-write timestamp) is *committed*,
  prover-supplied per access as a bit-vector slice (`read_last_ts` /
  `write_last_ts`). The consume-side timestamp is `bitpack(t_last)`.
- Per access: `consume(addr, value, bitpack(t_last))`,
  `order_ok &= emit_lt(t_last, counter)`, `produce(addr, value, bitpack(counter))`,
  `counter += 1`. All gadget hats stream via `transport.send_iteration` and are
  checked verifier-side via `recv_iteration` + `vole_and_verifier_check`.
- **Init** (entry block): produce both single-bit cells at `ts = 0`. **Drain**
  (exit block): consume each cell's committed final `(addr, value, ts)` witness.
- The exit opens the drain (`mem_drain_open` → `mem_drain_check`) and the ordering
  result (`vope_open_mask` → `assert_one_check`); the verdict is
  `all_ok && drain_ok && order_ok`. Both halves agree on AND ordering by
  construction (identical `emit_lt`/`emit_incr` traversal, shared
  `ts_iter_and_count` for the per-iteration AND count / `q_ands` indexing).

The counter is committed (private) rather than the public op-index scheme, so it
does **not** leak the access-timing pattern.

**Multi-bit addresses — SHIPPED.** `weave_ts_storage_loop_{prover,verifier}` take
an `addr_bits` parameter (`1..=8`). The per-access address is the free linear
bit-pack `Σ addr_i · 2^i` of the committed address wires (`vope_bitpack` /
`q_bitpack`); init/drain materialise all `2^addr_bits` cells, whose addresses are
public constants encoded from the public-`1`/`0` wires (`const_addr_bits`). The
`pow2` argument is `max(addr_bits, ts_bits)` wide and sliced per use. Cost is
**exponential in `addr_bits`** (init + drain emit `2^addr_bits` absorbs each), so
it is practical only for small address widths — wider memories want a sparse
init/drain (only touched cells) instead, which is the natural follow-up.

**Skip-resume entry — SHIPPED.** `glue::weave_skip_resume_{prover,verifier}` is the
composition point a driver weaves between a fast-forwarded segment and the resumed
segment: it re-keys the carried state wires (`XorRekey`) into the resumed segment's
committed inputs **and** carries `mem_prod`/`mem_cons` unchanged
(`AdditiveHashCarry`), so the driver feeds the returned wires straight into the
resumed loop body's `init_w*` + accumulators.

**Skip-resume ⊕ ts-loop composition — SHIPPED.** `weave_ts_storage_loop_{prover,
verifier}` take a `resume: bool`. When set, the loop's initial state is the
re-keyed snapshot computed **inline** — prover `init_w_i = vole_rekey_prover(
snapshot_i, key_i)`, verifier `init_q_i = q_snapshot_i + q_key_i`. Because both
sides compute the same free linear combination, the rekey relation is enforced
for free (like an XOR gate) and needs **no separate
`vole_rekey_verifier_check`** — the binding is automatic. This unifies the
dynamic-skip continuation with the ts-sound storage loop in one weaver
(`test_ts_storage_loop_resume_composes_skip_and_loop`).

**hybrid_net integration — SHIPPED.** `weave_hybrid_net_vole_{prover,verifier}`
take a `ts_bits` parameter and now use the **committed-counter** ts-sound scheme
in their storage absorbs (replacing the old vacuous single-`ts` +1), so the
network-resilient weaver closes the same future-consumption gap as the standalone
loop. How it was done:

1. **Shared per-access helpers.** The standalone loop's per-access logic was
   extracted to `pub(crate)` `storage_loop::emit_ts_access_{prover,verifier}` and
   is now called by both the standalone loop and the hybrid weaver — one
   implementation, no divergence.
2. **Gap threading via the suffix list.** The `aw_params`/`aw_clone_args`/
   `aw_move_args` closures iterate a computed `acc_sufs =
   ["_mp","_mc","_ts0".."_ts{B-1}","_ord"]`, so the committed counter bits +
   `order_ok` are carried through every gap block (B4–B8) and restored to B1 on
   replay automatically. Replay-from-anchor soundness is unchanged.
3. **B1 (prover ZK body)** calls `emit_ts_access_prover` (bit-pack produce/consume
   timestamps, `order_ok &= emit_lt(t_last, counter)`, `counter += emit_incr`);
   the gadget hats join the `try_send_iteration` batch. `B0` init-produces both
   single-bit cells at ts 0; `B3` drains both cells, opens the drain
   (`mem_drain_open`/`send_mem_opening`) and the ordering result
   (`vope_open_mask`/`send_opening`).
4. **Verifier** `emit_verifier_loop_gates` calls `emit_ts_access_verifier`; the
   3-block CFG carries the counter/order bundle slots, init-produces in B0, and at
   the sentinel runs `mem_drain_check` + `assert_one_check`, folding both into the
   verdict (`final_ok && mem_ok && order_ok`).
5. **AND_COUNT** is `count_and_gates(circuit) + ts_iter_and_count(b, accesses)`;
   prover hat order and verifier `and_counter`/`q_ands` stride agree by
   construction (same expanded-stmt traversal).

Both weavers take `addr_bits` too (multi-bit addresses): the per-access address
is the free linear bit-pack `Σ addr_i·2^i` of the committed address wires, and
init/drain materialise all `2^addr_bits` cells (`const_addr_bits` for the public
cell addresses; `pow2` is `max(addr_bits, ts_bits)` wide, sliced per use). The
`Weaver::HybridNetVole{Prover,Verifier}` variants now carry `{ name, ts_bits,
addr_bits }`, so the dispatch threads both (prover and verifier must use matching
values). Sparse init/drain (ADR 0002) remains the follow-up to remove the
`2^addr_bits` blow-up for large memories.

The committed counter starts at `1`, so the all-zero `(0,0,0)` tuple (which the
no-constant-term `encode` maps to the field zero) cannot be consumed as a phantom
"future" read: any forged read leaves the cell's true live tuple unconsumed,
breaking the drain balance. (A constant-term `encode` would remove this corner
case entirely; tracked as a hardening follow-up.)
