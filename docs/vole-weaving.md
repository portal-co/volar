# VOLE Weaving Plan

## Overview

This document describes the design and implementation of VOLE proving and verifying
passes in `volar-weaver`. These passes lower a boolean circuit (`BIrBlocks`) into
an `IrModule` implementing a one-round interactive ZK proof based on the Quicksilver
VOLE-in-the-head (VOLEitH) protocol.

The garble passes already in `volar-weaver` are moved to a `garble` submodule as
part of this refactor. The new `vole` submodule adds `weave_vole_prover` and
`weave_vole_verifier`.

---

## Background: Quicksilver AND Check

VOLE-based ZK proofs authenticate wire values via the VOLE relation:

```
K_w = M_w + x_w · Δ
```

where:
- `x_w ∈ GF(2)` is the wire value (a bit).
- `M_w ∈ GF(2^k)^N` is the prover's per-wire share (a vector).
- `K_w ∈ GF(2^k)^N` is the verifier's per-wire share.
- `Δ ∈ GF(2^k)^N` is the verifier's global secret.

In `volar-spec` terms, each wire is a `Vope<N, T, U1>` (prover) or `Q<N, T>` (verifier),
where `T` is the extension field element type (e.g. `Galois64`, `GF(2^128)`).

### XOR gate (free)

```
x_c = x_a ⊕ x_b
M_c = M_a + M_b           →  vope_c = vope_a + vope_b
K_c = K_a + K_b           →  q_c.q[i] = q_a.q[i] + q_b.q[i]
```

No communication needed.

### NOT gate (free)

The public constant `1` is committed as `Vope { u: [[1..1]], v: [0..0] }` with
verifier share `K_1 = Δ`. NOT is XOR with the public one:

```
x_c = 1 ⊕ x_a
M_c = M_a + 0 = M_a       →  vope_c = vope_a + vope_one
K_c = K_a + Δ             →  q_c.q[i] = q_a.q[i] + delta.delta[i]
```

No communication needed.

### AND gate (requires one message: V̂)

For gate `c = a AND b`, the prover sends `V̂_g = M_a · M_b` (element-wise product
in the extension field). The verifier checks:

```
K_a · K_b + V̂_g = K_c · Δ   (element-wise in T, for all N lanes)
```

**Derivation:** Expand `K_a · K_b`:
```
K_a · K_b = (M_a + x_a·Δ)(M_b + x_b·Δ)
          = M_a·M_b + (M_a·x_b + x_a·M_b)·Δ + x_a·x_b·Δ²
          = V̂ + (v_a·u_b + u_a·v_b)·Δ + u_c·Δ²
```

The prover sets:
- `u_c = u_a · u_b` (AND of the bit values, lifted to T).
- `v_c = v_a · u_b + u_a · v_b` (cross-term of masks and bits).
- `V̂ = v_a · v_b` (product of constant terms).

Then:
```
K_c · Δ = (v_c + u_c·Δ) · Δ
        = v_c·Δ + u_c·Δ²
        = (v_a·u_b + u_a·v_b)·Δ + u_a·u_b·Δ²
```

So `K_a · K_b + V̂ = K_c · Δ` ✓.

The verifier takes `K_c` (Q share for the AND output wire) as input — it comes
from the VOLE setup phase for that wire.

---

## Module Structure

```
volar-weaver/src/
├── lib.rs      — public API, shared helpers (expand_ors, var_names, type helpers)
├── garble.rs   — garble weaving passes (moved from lib.rs)
└── vole.rs     — new VOLE proving/verifying passes
```

---

## Function Signatures (Generated Code)

### Prover: `vole_prove_<name>`

```rust
fn vole_prove_<name><N: ArraySize, T>(
    vope_one: Vope<N, T, U1>,      // committed constant bit 1: u=[1..1], v=[0..0]
    vope_input_0: Vope<N, T, U1>, // prover's committed wire for input 0
    vope_input_1: Vope<N, T, U1>,
    ...
) -> (Vope<N, T, U1>, [Array<T, N>; AND_COUNT])
//    ^^^                ^^^^^^^^^^^^^^^^^^^^
//    output wire        V̂ per AND gate (fixed-size, in circuit order)
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T> + Mul<Output = T> + Default,
```

Gate lowering:
| Gate | IR |
|---|---|
| Zero | `Vope { u: [zeros], v: zeros }` |
| One  | `vope_one.clone()` |
| Xor(a,b) | `wire_a.clone() + wire_b.clone()` |
| Not(a) | `wire_a.clone() + vope_one.clone()` |
| And(a,b) | `let (wire_c, hat) = vole_and_prover_step::<N,T>(wire_a.clone(), wire_b.clone());` |

Returns `(wire_output, [hat_0, hat_1, ...])` (fixed-size array).

### Verifier: `vole_verify_<name>`

```rust
fn vole_verify_<name><N: ArraySize, T>(
    delta: &Delta<N, T>,

    // One (q_and, hat) pair per AND gate, in circuit order:
    q_and_0: Q<N, T>, hat_0: Array<T, N>,
    q_and_1: Q<N, T>, hat_1: Array<T, N>,
    ...

    // Verifier's VOLE shares for input wires:
    q_input_0: Q<N, T>,
    q_input_1: Q<N, T>,
    ...
) -> (Q<N, T>, bool)
//    ^^^        ^^^
//    output Q   all AND checks passed
where
    N: ArraySize,
    T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default,
```

Gate lowering:
| Gate | IR |
|---|---|
| Zero | `Q { q: Array::<T, N>::default() }` |
| One  | `Q { q: delta.delta.clone() }` |
| Xor(a,b) | `Q { q: Array::from_fn(\|i\| q_a.q[i].clone() + q_b.q[i].clone()) }` |
| Not(a) | `Q { q: Array::from_fn(\|i\| q_a.q[i].clone() + delta.delta[i].clone()) }` |
| And(a,b) | `let (wire_c, ok_k) = vole_and_verifier_check::<N,T>(delta, &wire_a, &wire_b, &q_and_k, &hat_k); all_ok = all_ok && ok_k;` |

Returns `(wire_output, all_ok)`.

---

## Helper Functions

The AND gate primitives live in `volar-spec/src/vole/prove.rs` and are
imported by the weaver's print preamble:

```rust
use volar_spec::vole::prove::{vole_and_prover_step, vole_and_verifier_check};
```

These functions are in the total-Rust subset and are parseable by
`volar-compiler` (verified by `test_prove_module_static_print_roundtrip`).

See `volar-spec/src/vole/prove.rs` for the implementations and security
rationale.

---

## Bounded Wrappers

`weave_vole_prover_bounded` and `weave_vole_verifier_bounded` apply
`lower_to_circuit` before delegating, mirroring the garble bounded wrappers.

---

## Printer

`print_weaved_vole_module(module) -> String` emits:
1. The standard `#![allow(...)]`, `extern crate alloc`, imports.
2. VOLE-specific imports: `volar_spec::vole::{Delta, Q, Vope}`, typenum `U1`.
3. Import of `vole_and_prover_step` / `vole_and_verifier_check` from
   `volar_spec::vole::prove`.
4. The IR-generated circuit body.

---

## Tests

All tests lower and compile the generated output (real Rust via `cargo check`
or `cargo test`), following the project invariant. Test cases:

1. `test_weave_vole_prover_compiles` — 2-input XOR+AND circuit, prover variant.
2. `test_weave_vole_verifier_compiles` — same circuit, verifier variant.
3. `test_weave_vole_prover_bounded_compiles` — bounded loop circuit.
4. `test_weave_vole_verifier_bounded_compiles` — same.

A runtime correctness test is deferred until a concrete field type with `Mul`
is available in test scope (the type `T` must implement field multiplication).

---

## Interaction with Existing Code

- The garble passes are moved verbatim to `garble.rs`; their public API is
  re-exported from `lib.rs`. No behavioral changes.
- The `expand_ors` and shared type/expression helpers remain in `lib.rs`
  (private, shared by both submodules).
- The `print_weaved_module` function remains in `garble.rs`; the new
  `print_weaved_vole_module` is in `vole.rs`.

---

## Open Issues

- **Field type**: The VOLE weave is fully generic over `T`, but the runtime
  correctness test needs a concrete `T: Mul`. `Galois64` (GF(2^64) from
  `volar-primitives`) is a natural candidate once its `Mul` impl is available
  in the weaver's test scope.
- **Batching**: The current protocol checks each AND gate independently. A
  batched random-challenge version (Quicksilver Section 4) would reduce prover
  message size from O(AND_gates × N × field_size) to O(N × field_size). This
  is a future optimization.
- **VOLE setup**: The weaver generates the *online proof* phase only. The VOLE
  setup (OT-based correlation generation) is out of scope.
