# Steel-in-Volar: full on-chain verify + decide path (multi-transaction)

**Goal.** Host a fork of [Steel](https://github.com/risc0/risc0-ethereum/tree/main/steel)
(RISC Zero's EVM view-call coprocessor) on the Volar pipeline: prove an EVM
execution (a view call / `eth_call` against historical state) **off-chain**
through Volar — Rust source → VAFFLE → virtualization → movfuscation → PESAT →
WARP — and verify it **on-chain** with the succinct WARP verifier, split across
**multiple transactions** where a single proof's verifier exceeds the block gas
limit. Target **Base** (L2) first, Ethereum mainnet if the gas allows, and
prove at an **acceptable speed** (FFT-based RS encoding, optimized recursion).

This builds on the landed WARP→EVM pipeline
(`volar-ir/docs/evm-e2e-pipeline-plan.md`, milestones P1–P4: real proofs, a
native-Solidity succinct verifier, L1/L2 gas analysis, and Reed–Solomon codes
for real soundness). This plan is the *application* of that pipeline to a real
program (the Steel view-call logic) and the *multi-transaction* verifier the
larger proofs need.

## End-state

A Volar-hosted Steel fork where:

1. A developer writes a Solidity view call they want proven (e.g. an ERC-20
   `balanceOf` at a historical block, a governance vote tally, an oracle read).
2. Off-chain, Volar proves the EVM execution of that call (the revm-style
   stepper + the state-access Merkle proofs) as a WARP accumulation proof.
3. On-chain, a deployed **multi-transaction WARP verifier** contract checks the
   proof across one or more transactions and exposes the verified view-call
   result to consuming contracts.

## Architecture

```text
Rust source (the Steel view-call stepper + state-access logic)
  │  cargo build --target wasm32…  AND  --emit=llvm-ir   (same compile, both forms)
  ├─ WASM ──▶ volar-wasm-circuit-import ──┐   (both frontends on the table;
  └─ LLVM ──▶ volar-llvm-vaffle-import ───┤    assess completeness/efficiency)
                                          ▼
                                        VAFFLE
  │  volar-ir-opt (inline, const-fold, DCE, CSE)
  ▼
  │  volar-ir-virt::virtualize_ir   (DispatchMode, per-PC bytecode commitment)
  ▼
virtualized step circuit
  │  movfuscate_ir + lower_to_circuit (E1 step granularity)
  ▼
Volar IR step block
  │  volar-pesat-emit  (E1–E5: per-step PESAT + memory accumulator, H5 FS-bound)
  ▼
per-step PESAT instances
  │  volar-iop::warp::warp_prove
  │    · ReedSolomonCode with an *additive FFT* encoder (S4 — replaces the
  │      naive O(n·k) Horner, the landed prover bottleneck)
  │    · resumable accumulation (E4) for the long EVM-execution stream
  ▼
WARP accumulation proof
  │  on-chain, multi-transaction:
  ▼
multi-tx succinct WARP verifier (S5)  ──▶  decide  ──▶  verified view-call result
```

The on-chain artifact is the **succinct verifier** (`warp_verify`), not the
linear `warp_decide` (which stays off-chain or is itself proven, per
`docs/prove-the-verifier-iop.md`). The transcript + Merkle use the native
`KECCAK256` extern (~519 gas/hash); the field arithmetic is `Gf128`/`Gf256`
words (the dominant verifier cost, per the P2 measurements).

## Why both frontends are on the table

A single `cargo build` of the Rust source can emit **both** the
`wasm32-unknown-unknown` WASM *and* the LLVM IR (`--emit=llvm-ir` /
`-C emit=llvm-ir`). Volar has a frontend for each:

| Frontend | Crate | Status / notes |
|---|---|---|
| WASM → circuit | `volar-wasm-circuit-import` | WASM operator coverage in `docs/wasm-feature-support.md`; the natural target for a `wasm32` build. |
| LLVM → VAFFLE | `volar-llvm-vaffle-import` | rustc-guest oriented; `alloca`/stack `load`/`store`/`gep`, `llvm.memcpy`/`memset`, `noalias.scope.decl`, `null` pointers — see the `docs/llvm-*.md` series. |
| LLVM → Volar IR | `volar-llvm-ir-import` | Lower-level LLVM import. |

S1 runs the reference program through **both** and picks (or supports both) on
completeness (which handles the revm-style code without unsupported-construct
gaps) and circuit size. The LLVM path is attractive because the Steel stepper
is pointer/memory-heavy (the `llvm-*.md` series is exactly the hard part), and
the WASM path is attractive because `wasm32` is the more stable rustc target —
the plan keeps both live until S1 measures them.

## Milestones

| M | Workstream | Gate |
|---|---|---|
| S1 | **Frontend selection + reference program.** Take a minimal Steel-subset view call (a single `balanceOf`-style storage read) as a Rust program; compile to WASM *and* LLVM IR from the same build; run both frontends to VAFFLE; measure completeness + circuit size. | A reference program compiles through ≥1 frontend to VAFFLE; a frontend decision (or dual-support) is recorded. |
| S2 | **Compilation.** VAFFLE → `virtualize_ir` → `movfuscate_ir` → `lower_to_circuit` for the reference program. Land the full compile; resolve any virtualization/movfuscation gaps the larger program hits. | The reference program reaches a Volar IR step block; the per-step trace evaluates correctly (differential vs native). |
| S3 | **PESAT emission.** `volar-pesat-emit` over the step block: per-step instances + the H5 FS-bound memory accumulator. | Per-step PESAT instances for the reference program; the emitter's wire trace matches the native run. |
| S4 | **Prover speed: FFT + recursion.** Replace the naive `O(n·k)` RS `encode` with an **additive FFT** (`O(n log n)`, the landed bottleneck — the `pipeline` test's naive encoding dominates the ~1356 s runtime); keep resumable accumulation (E4) for the long EVM stream; assess recursion (fold many steps, prove the folding) for amortization. | A measurable prover speedup (target: the reference program proves in minutes, not ~20 min); the FFT encoder is bit-exact vs the naive one. |
| S5 | **Multi-transaction on-chain verifier.** Extend the native-Solidity succinct verifier (`WarpVerifyFull.sol`) into a **resumable, multi-transaction** verifier: split the Fiat-Shamir absorption + the `ℓ·t` Merkle-opening checks + the two sumchecks across transactions, with the transcript sponge state + running sums + verified-count persisted on-chain between txs. | The multi-tx verifier accepts a real proof across ≥2 transactions on a local fork; per-tx gas is capped below the target chain's block limit. |
| S6 | **Steel fork application.** Port the Steel view-call logic (a revm-subset stepper + state-access Merkle proofs against a block's state root) to the pipeline; prove a real view call end-to-end. The state-access Merkle proofs ride the native-`KECCAK256` extern (keccak-heavy, so the extern is load-bearing). | A real EVM view call (e.g. `balanceOf`) is proven off-chain and verified on-chain on a local fork. |
| S7 | **Deployment: Base, then mainnet.** Deploy the multi-tx verifier + a consuming contract to **Base** (L2 — the higher block gas limit absorbs the verifier); measure the real per-proof cost; assess mainnet (tighter limit → more transactions per proof, or a smaller-`t` config). | A verified view call on Base testnet; a mainnet gas/cost report. |

## The multi-transaction verifier (S5) — design

The single-transaction succinct verifier is ~21–30 M gas for the sound RS
configs (P3). A Steel proof over a *large* EVM execution pushes the codeword
(`log n`) and possibly `t` up, exceeding even an L2 block. The multi-tx
verifier splits the work while preserving the Fiat-Shamir soundness (the
transcript is sequential, so the split is **along** the transcript order):

1. **Stage 0 — commitment.** Post the proof's Merkle roots + the instance;
   the contract absorbs them and derives the batching challenge. State saved:
   the transcript sponge state.
2. **Stages 1..k — sumchecks + openings.** Each transaction: load the saved
   sponge state, absorb the next chunk (sumcheck polys / OOD answers /
   per-query openings), derive the next challenges, and check the next chunk of
   Merkle openings (`log n` hashes each) and sumcheck rounds. Persist the
   sponge state + the running sumcheck sums + the verified-opening count.
3. **Final stage — tail checks.** The MLE/eq-eval consistency checks and the
   verdict; on success, write the verified view-call result to storage for
   consuming contracts.

Each transaction's gas is capped by the chunk size (the number of
openings/rounds it processes); the on-chain state between txs is small (the
sponge state, a few field running-sums, a counter). **Security note:** the
split is sound because no challenge is *used* before the absorption that
derives it is complete — the resumable transcript enforces the sequential
Fiat-Shamir order across transactions. This is the on-chain analogue of the
off-chain resumable accumulation (E4).

## Prover speed (S4) — the levers

The WARP prover over a large EVM execution is the speed risk. Levers, in
priority order:

1. **Additive-FFT RS encoding** (the landed bottleneck): the naive
   `O(n·k)` Horner `encode` dominates; an additive FFT over `GF(2^m)` is
   `O(n log n)`. This is the single largest prover speedup.
2. **Resumable accumulation** (E4, landed): the long EVM-execution stream is
   folded incrementally; memory-bounded.
3. **Recursion**: prove the per-step folding itself (a WARP proof of a WARP
   verifier step) to amortize many steps into one succinct proof — the
   in-circuit hash here is **Ascon** (~830× cheaper than Keccak in-circuit, per
   the H7 cost report), which is what makes recursion affordable.
4. **Parallelism / hardware**: the codeword encoding + the Merkle trees are
   embarrassingly parallel; noted as a follow-up (out of scope for the
   correctness-first milestones).

## The Steel fork (S6) — scope discipline

A *full* revm execution is millions of gates — infeasible to prove directly at
first. The Steel fork scopes up:

1. **S6a (first):** a *single storage read* view call (no general EVM stepping)
   — the state-access Merkle proof against the block's state root + the slot
   decode. Small circuit; validates the state-access + on-chain path.
2. **S6b:** a *bounded* EVM stepper (a fixed opcode subset / gas cap) for
   simple view calls (e.g. `balanceOf` = a few SLOADs + arithmetic).
3. **S6c (stretch):** a fuller revm port, gated on S4 prover speed.

The keccak-heavy state-access proofs are the in-circuit cost driver; the
native-`KECCAK256` extern (already the native path's win) is load-bearing, and
an Ascon-based *in-circuit* commitment of the accessed slots (where the proof
must be hash-checked in-circuit) keeps the recursion affordable.

## Risks / open questions

- **Prover scale.** A full EVM execution may stay too large even with the FFT;
  S6's scoped subsets + S4's recursion are the mitigation. **Gate:** S4's
  speedup measurement decides whether S6c is reachable.
- **Verifier gas vs chain limits.** L1 fits only the smallest configs (P3);
  Base's higher limit is the deployment target, and S5's multi-tx split is the
  fallback for larger proofs. Mainnet is opportunistic ("if we're lucky").
- **Frontend completeness.** The revm-style code is pointer/memory-heavy; the
  LLVM frontend's `alloca`/`memcpy` support (the `docs/llvm-*.md` series) and
  the WASM frontend's operator coverage (`docs/wasm-feature-support.md`) are
  both unproven on real revm code until S1.
- **Soundness parameters.** The RS parameters are *derived* (Theorem 9.1), not
  certified (see `evm-e2e-pipeline-plan.md`'s caveat); a production deployment
  needs the parameter-selection non-goal revisited and, eventually, an audit.

## References

- `volar-ir/docs/evm-e2e-pipeline-plan.md` — the landed WARP→EVM pipeline
  (P1–P4): real proofs, the native-Solidity verifier, L1/L2 gas, RS codes.
- `volar-ir/docs/pipeline.md` — the IR/lowering pipeline this rides.
- `docs/hash-cipher-generic-plan.md` — the hash backends (Ascon for in-circuit
  recursion + accumulators; Keccak for the on-chain transcript/Merkle).
- `docs/prove-the-verifier-iop.md` — `warp_decide` stays off-chain / is itself
  proven.
- `docs/memory-checking.md`, the `docs/llvm-*.md` series — the memory-model +
  LLVM-import details the Steel stepper hits.
- Steel upstream: https://github.com/risc0/risc0-ethereum/tree/main/steel
  (the framework being forked).
