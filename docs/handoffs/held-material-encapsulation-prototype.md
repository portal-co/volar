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

The initial implementation inlined a complete AES circuit per block. This is
now superseded by `build_aes128_multi(blocks)`: it expands the 128-bit split
key once and reuses the 11 round keys for every block. The multi-block
material gadget now uses that shared-key-schedule AES circuit. Concrete AES
vectors confirm each output block matches independent AES-128 encryption, and
the schedule test confirms four blocks cost strictly less than four one-block
circuits (while still costing more than one block). This removes duplicated
key-expansion S-box work, but SubBytes/MixColumns remain necessarily linear in
the number of plaintext blocks.

### Adapter cache boundary and public accounting

The paired split-key adapters now own a `MaterialBlockCachePlan` for the
current label width. A `MaterialRole::Both` load is one cacheable, paired
protocol operation: a repeated load of a resident slot starts neither AES nor
OT. A flush explicitly evicts the slot after sealing, so later loads cannot
mistake a stale role-local label for the newly persisted material. The chain
storage dispatcher invokes a paired load once rather than independently
asking each role stream; this preserves lockstep framing when the complete
paired operation is skipped.

`MaterialStoreMetrics` reports only public execution shape: material blocks,
opens, seals, and paired-load cache hits. The TCP round-trip test performs one
paired seal, two identical paired loads, and a held round; each role reports
four material circuits (two seals, two opens) and one cache hit out of two
paired requests. This is intentionally **not** a packed-block writeback yet:
current `U16` labels remain exactly one AES block, so a narrower proven opaque
representation is required before dirty slots can collapse into one physical
seal.

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

`deferred_remap_schedule(bits)` is now a concrete post-restoration rebase
circuit. For each bit it evaluates the free-XOR relation:

```text
[held label | fresh garbler-base public-zero label] -> opaque remapped label
```

The garbler provides a fresh false-label base for the public-zero wire while
the evaluator receives only its zero label. XOR therefore changes the output
false base without changing the hidden Boolean. Neither role decodes the bit,
and the output remains `SplitOutput::Opaque`. The TCP test verifies that a true
held label is accepted under the new base while no reveal disposition is
present. Direct strict-chain threading is still superior whenever the next
circuit can adopt the prior output base, because it needs no circuit at all.

## What did not work / remains intentionally absent

1. **Raw 16-byte label packing does not reduce blocks.** Current `U16` labels
   consume all 16 bytes. Packing helps only after a new fixed-width compressed
   or encapsulated representation is defined and proven to preserve garbling
   semantics.
2. **Shared key expansion does not make AES rounds sublinear.** The multi-block
   circuit removes duplicated key-schedule S-boxes, but its per-block
   SubBytes/MixColumns work (and therefore most table work) remains linear.
3. **The CFG loop does not lower total cryptographic cost.** It trades peak
   memory for sequential circuit invocations. It needs the revealed
   termination policy audited for the concrete guest protocol.
4. **Deferred remapping is a rebase, not compression.** It preserves the full
   opaque label and only gives it a fresh false base. It does not define a
   narrower durable representation or authorize host-side copy/decode/re-
   encode.
5. **ORAM return batching is conservative.** Repeated public-leaf reads may
   reuse a path. Writes sharing any non-root physical bucket remain serial;
   even leaves that diverge below root still require ordered root commits.
   No network tree transport consumes the plan yet.

## Next implementation sequence

1. Measure `build_aes128_multi(n)` AND/table bytes for representative `n` and
   select a bounded material-batch size that fits the strict-table stream.
2. Choose a fixed, narrower opaque durable representation (or an
   encapsulation construction) and prove pack/unpack plus deferred remapping
   preserve the garbling relation.
3. Extend the now-landed resident cache to block-granular packed writeback once
   that narrower representation exists; retain explicit eviction at each
   persistence boundary.
4. Measure a real storage-bearing module through both `NetOtChannel` and the
   now-landed Ferret-backed channel with production Ferret parameters; report
   table bytes, OT bytes, number of material blocks, cache hit rate, and
   termination rounds. The `FERRET_REG_TOY` test parameters are correctness
   fixtures only and must never be deployed.
5. Evaluate whole-construction alternatives for garbled-table or input-label
   communication reductions while retaining a reviewed durable-label security
   level; see [`../research/garbled-label-size-options.md`](../research/garbled-label-size-options.md).
