# Held-material encapsulation and deferred-remap prototype

**Status:** explored 2026-09-14. This note records both the working circuit
shapes and the limits discovered before wiring them into the durable adapter.

## Question

Can fixed-size packed material blocks, a repeated-CFG material step, or a
label-to-label remapping protocol avoid the per-held-bit AES cost measured by
the Wasm/LLVM probes?

## What works

### Fixed public packing and cache planning

`MaterialBlockLayout` maps a public held slot to a 16-byte AES block and byte
offset. It never splits a fixed-width value across blocks. Its associated
`MaterialBlockCachePlan` tracks resident slots and public dirty block indices:

- a resident read does not request another material opening;
- writes to several slots in one packed block produce one flush candidate;
- the final partial block is zero-padded deterministically.

The packing tests show that three 4-byte values in slots 0, 1, and 3 share one
material block, and four 8-byte values use exactly two blocks.

This is a viable cache-policy seam. It does **not** by itself shrink a current
`U16` garbling label: those labels are 16 bytes, exactly one AES block each.
A later representation/encapsulation format must produce a narrower opaque
value before this packing yields a material-AES reduction.

### Multi-block material circuit

`build_material_block_cipher_n(n)` accepts one split 128-bit key, `n` public
tweaks, and `n` role-owned material blocks, producing `n` AES-XOR blocks in
one split invocation:

```text
[key: 128 | tweaks: n*128 | blocks: n*128] -> n*128 outputs
```

This combines protocol framing and OT scheduling at one public invocation
boundary. The test confirms its I/O geometry for `n = 4`.

The current implementation inlines a complete AES circuit per block. Its AND
count is exactly linear: the four-block schedule is four times the one-block
schedule. Thus it does not yet amortize AES key expansion or nonlinear gate
cost. It is useful as a fixed-shape batching baseline, not the final scaling
answer.

### CFG-style repeated material step

`build_material_block_loop_step(counter_bits)` processes one material block
and threads a remaining-block counter:

```text
[key | tweak | block | remaining]
  -> [block' | remaining - 1 | done]
```

`done` becomes true precisely at `remaining == 1`. A driver can reveal that
single termination flag at a circuit boundary and feed the counter state into
the next circuit as held material. The concrete test confirms the `2 -> 1`
nonterminal step and the `1 -> 0` terminal step.

This provides an `n` option without fully materializing the `n`-block circuit.
It caps peak circuit/table memory and permits cache-driven early stop, but it
does not reduce total AES nonlinearity absent a shared-key-schedule gadget.
It also deliberately reveals only termination, so a caller must define whether
iteration count itself is public/acceptable for its persistence protocol.

### Deferred opaque remapping

`DeferredLabelRemapPlan` describes a safe post-restoration protocol shape:

```text
[source held label | fresh garbler target encoding | evaluator selector]
  -> opaque remapped label
```

The target is an opaque split output. Neither host decodes the Boolean first.
This is compatible with strict-chain's normal zero-cost rebasing: whenever the
next circuit can adopt the previous output false base, that direct threading
remains superior and requires no remap circuit.

## What did not work / remains intentionally absent

1. **Raw 16-byte label packing does not reduce blocks.** Current `U16` labels
   consume all 16 bytes. Packing helps only after a new fixed-width compressed
   or encapsulated representation is defined and proven to preserve garbling
   semantics.
2. **The multi-block circuit does not share AES key expansion.** It has one
   public key input but currently inlines independent AES instances, so AND
   count remains linear in `n`.
3. **The CFG loop does not lower total cryptographic cost.** It trades peak
   memory for sequential circuit invocations. It needs the revealed
   termination policy audited for the concrete guest protocol.
4. **Deferred remapping is a public protocol plan, not a landed remap
   circuit.** A real circuit must establish the fresh target wire encodings
   while retaining the source Boolean as opaque held state; a host-side copy
   or decode/re-encode is not acceptable.
5. **ORAM return batching is conservative.** Repeated public-leaf reads may
   reuse a path. Writes sharing any non-root physical bucket remain serial;
   even leaves that diverge below root still require ordered root commits.
   No network tree transport consumes the plan yet.

## Next implementation sequence

1. Add an AES multi-block gadget with shared expanded round keys, then compare
   AND count against the current `n × 16,000` baseline.
2. Choose a fixed, narrower opaque durable representation (or an
   encapsulation construction) and prove pack/unpack plus deferred remapping
   preserve the garbling relation.
3. Integrate `MaterialBlockCachePlan` into the paired durable adapters, with
   block-granular dirty flush and explicit cache eviction at persistence
   boundaries.
4. Drive a real storage-bearing module through the networked adapter using
   both `NetOtChannel` and a Ferret-backed `OtChannel`; report table bytes, OT
   bytes, number of material blocks, cache hit rate, and termination rounds.
