# Issue: transport-backed split-key ORAM material store

**Status:** narrowed and still open, updated 2026-09-13.

## Completed transport seam

`volar-mpc::strict_chain` now has a role-local, transport-aware material
backing seam:

```rust
trait HeldMaterialStore<T, N> {
    fn load(&mut self, slot, transport, ot) -> Result<Option<T>, MpcError>;
    fn store(&mut self, slot, role_local_value: Option<T>, transport, ot)
        -> Result<(), MpcError>;
}
```

The `Option<T>` write is intentional. For a material output:

- the garbler writes `Some(Garble<N>)` only for `GarblerMaterial(slot)`;
- the evaluator writes `Some(Eval<N>)` only for `EvaluatorMaterial(slot)`;
- the other role receives `None` but still executes the matching store
  transaction, keeping a network ORAM protocol lockstep without receiving the
  other role's label/base.

`GarblerMaterial` and `EvaluatorMaterial` do not decode a Boolean value. The
framed-TCP test (`tests/strict_material_tcp.rs`) uses real Chou--Orlandi OT,
persists both role-local parts, and consumes them in a later strict round.
This establishes the material-conversion protocol independently of loopback
ORAM execution.

## Blocking lower-layer implementation issue

`volar-vc::oram_2pc::SharedKeyOramAdapter` is not usable as the backing yet.
It is an in-process combined-role harness over `LoopbackOt`, and its internal
`HeldState` contains both evaluator labels and garbler bases. Reusing it as a
`HeldMaterialStore` would hide the same material registry under a new name and
would not exercise the real transport.

This is a concrete implementation issue, not an acceptable deployment gap:
create two network role adapters with no combined-role state:

```text
GarblerSplitKeyMaterialOramStore<Garble<N>>
    own AES key half + garbler false-label-base ORAM client state

EvaluatorSplitKeyMaterialOramStore<Eval<N>>
    own AES key half + evaluator active-label ORAM client state
```

They must execute the existing begin/access/evict circuits as two strict roles
over the same `Transport` / `NetOtChannel`, not through `LoopbackOt`.
The physical tree protocol needs explicit framed requests for:

1. the public old leaf;
2. evaluator-hosted ciphertext path read/write; and
3. garbler/evaluator key-half inputs to every AES-pad access circuit.

The current `Oram2pc::run_circuit` combines both role inputs and directly
calls `DynGarbledExec::eval_labels_multi`; that is the exact code path to
replace. It cannot be wrapped safely.

## Required acceptance test

Replace `MemoryHeldStore` in `strict_material_tcp.rs` with the paired network
ORAM stores. Over loopback TCP, with real `NetOtChannel`, prove all of:

- material persists across a strict round and a later `Held` input preserves
  the garbling relation;
- no role's process contains the other role's `Garble`/`Eval` material or the
  full AES tree key;
- every evaluator-private input of an ORAM access uses network OT;
- a malformed/tampered ORAM response aborts before it can become an input
  label; and
- no `LoopbackOt`, `DynGarbledExec::eval_labels_multi`, or combined-role
  `HeldState` is reachable from either production store implementation.

Do not remove `MemoryHeldStore` until this acceptance test exists and passes.
