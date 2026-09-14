# Split ORAM runner and held-material encryption report

## Implemented lower-level seam

`volar-mpc::strict_split` now exposes the actual role split required below the
combined `Oram2pc` harness:

- `SplitGarbler<N>` receives input false-label bases and returns only output
  false-label bases;
- `SplitEvaluator<N>` receives evaluator-owned input bits and returns only
  active output labels;
- evaluator-private inputs use the supplied `OtChannel`;
- table transfer uses the normal strict framed transport;
- neither role calls `LoopbackOt` or shares a `HeldState`.
- `SplitOutput::{Reveal, Opaque}` now makes output disposition explicit:
  opaque output labels are never transmitted to the garbler, while each role
  retains its own corresponding material for later threading.
- `SplitInput::{Public, Garbler, Evaluator, Held}` and the paired
  `run_with_state` methods now provide that later threading directly: garbler
  bases and evaluator labels enter separate role-local vectors, with no frame
  or OT for `Held` wires. The TCP test executes an opaque round followed by a
  distinct round that consumes its output as `Held`.

This is deliberately a small, deep module: a future begin/access/evict ORAM
adapter need only build its public input partition and role-local inputs. It
owns path I/O, key-half ownership, and state threading internally.

`strict_chain` additionally has `ChainStoragePhase` and a public
`StorageOperation` script. It identifies the correct scheduling seam: durable
ORAM work must run at a chain-round boundary, never recursively from a
`HeldMaterialStore::load/store` while the outer strict round is exchanging
frames. `MaterialRole` makes the intended owner of a material write public, including `Both` for ordinary
threaded `Hold` outputs, which is necessary for matching the garbler/evaluator
private partitions.

## Held-value encryption assessment

**Yes, encrypting packed held values under the same split AES key works and is
recommended for material that crosses a role/process or durable-storage seam.**
It must be done as a circuit operation, not by either role reconstructing the
key or encrypting locally.

### Construction

1. Pack a fixed, public count of held bits into 128-bit blocks. The block
   count and padding are protocol shape, not secret data.
2. In the split access circuit, take the 64 garbler AES-key bits as garbler
   inputs and the 64 evaluator bits as evaluator inputs (the latter through
   OT).
3. Derive a unique AES tweak/counter from a public domain separator,
   material-region ID, block index, and monotonically increasing version.
4. XOR the packed held bits with `AES_K(tweak)` in-circuit. Store/transmit only
   the resulting ciphertext block; unpack only within a subsequent split
   circuit.
5. Bind ciphertext freshness: use the existing versioned-pad work when
   available, or add a circuit-verified counter/MAC before treating a returned
   ciphertext as an input label. Encryption alone hides content but does not
   prevent replay/substitution.

### What it protects

- An evaluator-hosted durable tree, transport transcript, or serialized
  material cache sees ciphertext rather than a useful raw garbling-label
  representation.
- Neither party obtains the complete AES key.
- Packing amortizes AES work and removes a per-label ciphertext layout leak.

### Important limit

A garbler false-label base and evaluator active label are *not interchangeable
plaintext shares*. They must be encrypted in separate role-local ciphertext
streams or as a jointly garbled computation that preserves the relation. Do
not combine both into one host-visible block: that would recreate combined
held state. Also do not encrypt a label and later decode it outside MPC; the
ciphertext must re-enter as evaluator-private path data and be decrypted in
the split circuit.

## Next implementation step

Migrate `Oram2pc::run_access` in this order:

1. Split the current `Oram2pc` combined harness into a paired
   `SplitOramGarbler` / `SplitOramEvaluator` driver and replace its
   `run_circuit` calls with `run_with_state`, using `SplitOutput::Opaque` for
   tape/posmap/stash. The new `oram_split` module has migrated the
   tape-to-tape `Compute` segment with separate `GarblerOramState` and
   `EvaluatorOramState`, validated over TCP/OT. The legacy one-process harness
   remains unchanged until the begin/access/evict path is migrated.
2. Extend those role-local types with posmap/stash initialization and migrate
   `build_begin`; its old leaf must be a deliberate revealed output while the
   replacement posmap is opaque. **Completed for non-keyed leaves:**
   `SplitOram*::run_begin` has TCP/OT coverage, returns only the public old
   physical leaf, and retains its updated posmap in distinct role-local state.
   The keyed-leaf variant remains with the later split-key migration.
3. Give the evaluator a ciphertext-tree adapter and explicit path
   request/read/write frames, validating widths/version before circuit input.
4. Feed the separate AES halves to each access and use packed held-material
   encryption for the durable material regions.
5. Build the paired `HeldMaterialStore` adapters on top of the completed
   boundary phase, then replace `MemoryHeldStore` in the TCP acceptance test.
