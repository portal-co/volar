# Multiset Memory Checking for VOLE-Authenticated Storage

> **Referenced by**: `volar-spec/src/vole/memory.rs`, `volar-weaver/src/vole.rs`
>
> **Status**: @reliability: experimental

## Overview

This document specifies how Volar verifies memory (storage) consistency in
VOLE-based zero-knowledge proofs.  Two modes are supported:

| Mode | AND gates | Verifier state | Mechanism |
|------|-----------|----------------|-----------|
| **Tree** (in-circuit) | O(N) per access | O(N) Q-values | MUX/demux tree |
| **Commitment** (external) | **0** | **O(1)** | Multiset hash |

The **commitment** mode is the subject of this document.

## Protocol: Offline Memory Checking

Based on Blum–Evans–Gemmell–Kannan–Naor (1991), adapted for the
VOLE/Quicksilver framework.

### Memory model

Storage is a map `addr → value` that starts with all cells zero.  Operations:

- **Write(addr, value, t)**: set `storage[addr] = value` at timestamp `t`.
- **Read(addr, t) → value**: return `storage[addr]` at timestamp `t`.

Every write implicitly *consumes* the previous entry at that address and
*produces* a new one.  Every read *consumes* the current entry and
*re-produces* it (the value persists).  A final *drain* at the end of the
proof consumes every surviving entry.

### Multiset accounting

We maintain two accumulators:

```
H_produce  ← accumulates all entries that are "produced"
H_consume  ← accumulates all entries that are "consumed"
```

| Event | H_produce | H_consume |
|-------|-----------|-----------|
| **Init** cell (a, 0, t=0) | +encode(a, 0, 0) | — |
| **Write** (a, v_new, t), overwriting (a, v_old, t_old) | +encode(a, v_new, t) | +encode(a, v_old, t_old) |
| **Read** (a, v, t), matching write at t_w | +encode(a, v, t) | +encode(a, v, t_w) |
| **Drain** final cell (a, v_final, t_final) | — | +encode(a, v_final, t_final) |

**Invariant**: if all reads return correct values, `H_produce == H_consume`.

### Encoding

Each memory tuple is encoded as a field element using a random challenge
`r ∈ T` provided by the verifier (or derived via Fiat-Shamir):

```
encode(addr, value, timestamp) = addr · r  +  value · r²  +  timestamp · r³
```

For multi-bit addresses (K bits): `addr = Σ_j  addr_bit_j · 2^j`.  This is
computed as a linear combination with public coefficients.

### Why this is free in VOLE

Every multiplication in the encoding is **public constant × authenticated
value** — the scalar `r^k` or `2^j · r` is known to both parties:

- **Prover**: `(x, M) → (x·c, M·c)` for public `c ∈ T`
- **Verifier**: `K → K·c`

These are free operations (no AND gate, no hat).  Additions are also free.
The entire multiset hash computation costs **zero AND gates**.

### Soundness

**Theorem**: A cheating prover that causes any read to return an incorrect
value is detected with probability ≥ 1 − M/|T|, where M is the total number
of memory operations and |T| is the extension field size.

**Proof sketch** (Schwartz-Zippel):

1. `H_produce − H_consume` is a polynomial of degree ≤ 3 in `r` (per
   memory operation).  If any read returns a wrong value, the polynomial
   is not identically zero.
2. A non-zero polynomial of degree ≤ 3M over T has at most 3M roots.
3. The probability that the random `r` is a root is ≤ 3M / |T|.
4. For |T| = 2^128 and M ≤ 2^40, this is negligible (≤ 2^{-88}).

### Prover's auxiliary hints

For each **read**, the prover must supply:
- The read **value** (as a VOLE-authenticated oracle wire).
- The **matching write timestamp** `t_w` (public hint, verified by the hash).

For each **write** that overwrites a previous value, the prover must supply:
- The **previous value** `v_old` and **previous timestamp** `t_old`
  (public hints, verified by the hash).

These hints are verified implicitly: if any hint is wrong, the multiset
hash won't balance.

## State machine API (`MemoryCheckState`)

The spec provides a generic state machine parameterized by a hash function:

```rust
trait MemoryHasher<T> {
    type State;
    fn new_state() -> Self::State;
    fn absorb(state: &mut Self::State, encoded: T);
    fn finalize_eq(produce: &Self::State, consume: &Self::State) -> bool;
}
```

The `MemoryCheckState<H>` struct wraps two accumulators and exposes:

- `init(addr, zero_value)` — produce an initial cell.
- `write(addr, new_value, timestamp, old_value, old_timestamp)` — produce
  new entry, consume old entry.
- `read(addr, value, timestamp, matching_write_timestamp)` — produce and
  consume entries.
- `drain(addr, final_value, final_timestamp)` — consume surviving entry.
- `verify() → bool` — check `H_produce == H_consume`.

Both prover and verifier call the same API; the only difference is the type
of the values (Vope vs Q).

## Integration with the weaver

The weaver's `StorageMode::Commitment` generates code that:

1. Takes oracle parameters for each read value.
2. Calls `MemoryCheckState` methods for each storage operation.
3. Returns `(output, hats, verify_ok)` where `verify_ok` includes the
   multiset check result.

The `MemoryTrace` returned alongside the `IrModule` maps circuit var IDs
to memory operations so the runtime can supply the correct oracle values
and hints.
