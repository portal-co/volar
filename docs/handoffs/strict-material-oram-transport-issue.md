# Issue: transport-backed split-key ORAM material store

**Status:** open implementation issue, recorded 2026-09-13.

## What is complete

`volar-mpc::strict_chain` now has a role-local `HeldMaterialStore<T>` seam:

- garblers persist only `Garble<N>` false-label bases;
- evaluators persist only `Eval<N>` active labels;
- `ChainOut::GarblerMaterial` and `ChainOut::EvaluatorMaterial` convert raw
  garbled-circuit outputs into those role-local materials without decoding a
  logical bit; and
- the complete conversion and subsequent re-use run over framed TCP with real
  Chou--Orlandi OT in `tests/strict_material_tcp.rs`.

The material store's `load` takes `&mut self`, deliberately allowing a durable
network adapter to perform a request/response fetch before the next strict
round builds its input bases/labels. The prior `&self` shape could not support
networked storage and would have forced an in-memory cache.

## What remains

`volar-vc::oram_2pc::SharedKeyOramAdapter` proves the split-key AES ORAM
circuit shape, but its current execution engine is `LoopbackOt`; it is not a
network transport implementation. It therefore cannot be installed as a
production `HeldMaterialStore` yet.

The required adapter is two role-local stores, backed by the same physical
ciphertext tree:

```text
ChainGarbler<Garble<N>, GarblerOramMaterialStore>
    store/load false-label base bytes

ChainEvaluator<Eval<N>, EvaluatorOramMaterialStore>
    store/load active-label bytes
```

Each store operation must drive one split-key encrypted ORAM access over the
strict session transport, using `NetOtChannel` (or a bulk extension), with:

1. the AES tree key split 64/64 as in `SharedOramKey`;
2. a material encoding that spans `N` label bytes, rather than ORAM's current
   one-bit payload; and
3. no output disposition that decodes or reveals the stored material bit
   values.

## Acceptance test

Replace the in-memory `MemoryHeldStore` in `strict_material_tcp.rs` with the
paired ORAM stores. The test must still run over loopback TCP, use `NetOtChannel`
for every evaluator-private input, and demonstrate:

- `Hold` material persists across a completed strict round;
- a later `Held` input reuses the exact garbler/evaluator material relation;
- neither role receives the other role's material or a decoded logical value;
- a tampered ORAM response aborts rather than becoming a valid input label.

This is an implementation issue, not a security claim about the present
loopback ORAM harness. Do not delete the compatibility `MemoryHeldStore` until
this test exists and passes over the network transport.
