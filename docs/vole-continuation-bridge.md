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
pub enum SendOutcome { Continue, Disconnect, Fatal }

pub struct ResumeToken<N: ArraySize, T> { /* re-committed boundary state + proof material */ }

pub enum GapVerdict { Proven, Unproven { start: u32, end: u32 } }

pub trait ResilientVoleTransport<N: ArraySize, T>: VoleTransport<N, T> {
    fn try_send_iteration(&mut self, hats: &[Array<T, N>], is_sentinel: bool) -> SendOutcome;
    fn try_reconnect(&mut self) -> Result<bool, Self::Error>;
    fn prover_bridge(&mut self, token: &ResumeToken<N, T>, gap_len: u32) -> Result<(), Self::Error>;
    fn verifier_bridge(&mut self, gap_len: u32) -> Result<GapVerdict, Self::Error>;
}
```

- **Best-effort default:** `prover_bridge` transmits `(ResumeToken, gap_len)`;
  `verifier_bridge` returns `GapVerdict::Unproven { k, k+gap_len }` and records it.
- **VCB-RX / VCB-IVC:** implement `prover_bridge` / `verifier_bridge` to run the
  re-execution or the folding + linking protocol of §3.3, returning
  `GapVerdict::Proven` on success.
