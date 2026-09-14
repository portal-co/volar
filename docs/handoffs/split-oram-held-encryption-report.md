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
3. Migrate the plaintext `build_access` circuit. **Completed:**
   `SplitOram*::run_access` accepts the evaluator-owned physical path as OT
   inputs, threads stash/address/write-data as opaque state, reveals only
   overflow plus the physical write-back path, and has TCP/OT coverage. Logical
   read data remains opaque and is not exposed by this migration API.
4. Add the role-local key-bearing drivers. **Completed as phase one:**
   `GarblerEncryptedOramDriver` owns only garbler bases and its 64 key bits;
   `EvaluatorEncryptedOramDriver` owns only evaluator labels and its 64 key
   bits. They independently advance a public success-only epoch, and neither
   public interface exposes the other half. The still-private garbler helper
   retains all 128 key input bases, as required by garbling, but not evaluator
   labels or key bits.
5. Add the evaluator-owned ciphertext tree. **Completed as phase two:**
   `oram_ciphertext_tree::CiphertextTree` owns only evaluator physical tree
   bytes and a public epoch; an `OpenedCiphertextPath`/prepared access is
   single-use, width-checked, leaf-bound, and epoch-bound before mutation.
   Unsupported formatter/version modes fail closed.
6. Bind both key-bearing drivers and the prepared tree access to the split AES
   access circuit. **Completed:** role-local drivers invoke `build_access`
   with 128 garbler input bases, garbler 64-bit key input, evaluator 64-bit OT
   input, evaluator ciphertext-path OT inputs, and public path-version inputs.
   The evaluator commits its consuming prepared path before acknowledging
   success; the garbler advances only after that acknowledgement. TCP coverage
   proves both key epochs, tree epoch, and versioned path counters advance
   together.
7. Encrypted-valid formatting is now a separate split formatter circuit:
   `build_tree_node_formatter` consumes the same 64/64 key partition and emits
   ciphertext dummy buckets. `CiphertextTree` refuses encrypted-valid opens
   until every public tree node has a formatter output installed. Versioned
   pads use evaluator-owned public node counters, validated on prepare and
   bumped only on successful commit.
8. Build the paired `HeldMaterialStore` adapters on top of the completed
   boundary phase, then replace `MemoryHeldStore` in the TCP acceptance test.
   **Started with the durable block-format protocol:**
   `build_material_block_cipher` is a fixed `[key:128, tweak:128,
   material:128] -> material XOR AES_K(tweak)` circuit. The public tweak
   names the material role, slot, version, and block, using independently
   encoded fields rather than XOR folding. Concrete circuit coverage confirms
   that it matches the AES reference and that any role/slot/version/block
   change yields a different pad. The next slice must add the paired
   `HeldMaterialStore` adapters and keep their sealed/opened blocks inside
   split-circuit input/output handling; do not expose block plaintext to a
   role host.

## Current durable-material runner seam

`strict_split::SplitOutput::EvaluatorReveal` now supplies the required narrow
primitive: an output label stays evaluator-local, while the garbler sends the
two output encodings in a dedicated `OutputDecodes` frame. The garbler never
receives that active label or decoded bit. TCP/OT coverage exercises this
shape independently. This is the correct output disposition for a material
ciphertext block produced by the split AES circuit; the next adapter slice can
store that evaluator-decoded ciphertext without adding a host-visible
plaintext channel.

`HeldMaterialStore::{load,store}` are now generic over the chain circuit's
`Digest`. The injected backing therefore uses the same digest type when it
invokes the material AES circuit, rather than baking a second hash choice into
durable material format. `ChainGarbler` and `ChainEvaluator` thread that type
through ordinary held loads/stores and explicit `ChainStoragePhase` scripts;
the compatibility memory store remains digest-independent.

The inverse private-output primitive is now also present:
`SplitOutput::GarblerReveal` gives the garbler an exact-match decoded output
while the evaluator receives no decoded verdict bit. Together with
`EvaluatorReveal`, the material transaction has the two directions it needs:
seal produces evaluator-owned ciphertext; opening an evaluator-supplied
ciphertext produces a garbler-owned false-label base. Both directions have
separate TCP/OT tests.

The directional circuit/partition API is now explicit:
`MaterialBlockDirection::{SealGarbler, OpenGarbler, SealEvaluator,
OpenEvaluator}` names the only valid ownership transitions.
`MaterialBlockProtocol::for_direction` provides the exact 384-input
`SplitInput` partition and private `SplitOutput` disposition for each. This
means adapters cannot mistakenly feed a garbler base as evaluator input, or
leak an opened garbler base to the evaluator. The arithmetic remains one fixed
AES-XOR circuit; the direction-specific constructors document and test the
ownership contract. `material_block_tweak_checked` rejects slot/version/block
values that cannot fit in the fixed public tweak encoding, instead of silently
aliasing AES pads.

`ChainStoragePhase` now invokes explicit `HeldMaterialStore::prefetch` and
`flush` hooks rather than calling `load`/`store` inside the storage phase.
This gives a durable adapter an unambiguous, non-nested transport/OT seam:
ordinary strict rounds only consume a local cache, while the public storage
script drives material AES transactions at a round boundary. The hooks default
to no-ops for `MemoryHeldStore`.

**Completed adapter implementation:** `GarblerSplitKeyMaterialStore` and
`EvaluatorSplitKeyMaterialStore` are paired role-local adapters. They stage
strict outputs, use the direction-specific AES circuit at `flush`, persist
ciphertext only evaluator-side by `(region, slot)`, and recover local material
at `prefetch`. `MaterialRole::Both` is expanded internally into two sequential
transactions—garbler stream then evaluator stream—without ever combining a
false-label base and active label. The TCP acceptance test stages a private
output, flushes and prefetches encrypted material through real network OT, and
uses it in a later `Held` circuit round. The adapter accepts any `OtChannel`;
a Ferret-backed channel applies its lower-communication extension without a
second adapter path.

## Real module storage probes

Two concrete real-front-end probes now exercise the same ORAM lowering seam:

- **Wasm:** a WAT module storing then loading an `i32` from linear memory;
- **LLVM:** an imported LLVM IR module using `alloca`, `store`, and `load` on
  an `i32` stack slot.

Both pass through their normal frontend → VAFFLE → IR → boolar pipeline and
then `storage_to_oram`, which proves the storage accesses are not a
hand-assembled boolar fixture. With the current small, safe probe geometry
(`levels=4`, `Z=2`, `64` narrowed cells, `stash=96`), the probes report 128
bit-cell ORAM accesses: Wasm has a 1,250-bit tape and LLVM has a 972-bit tape.
They preserve the whole input `i32` through store/load. This is a correctness
and sizing baseline, **not** a performance claim for rustls: the next
measurement needs to drive a realistic storage-bearing guest through the
networked durable material adapters and compare base OT/Ferret channel bytes
and round count.

## Split-key and held-encryption sizing probes

The Wasm and LLVM storage probes now also compile the shared-key encrypted ORAM
shape and all four directional held-material AES circuits. The probes use a
byte store/load to avoid conflating the measurement with 32 independent byte
lanes. At the current 64-cell, `levels=4`, `Z=2`, stash-96 geometry:

| probe | tape bits | bit-cell ORAM accesses | shared access ANDs | material AES ANDs per direction |
| --- | ---: | ---: | ---: | ---: |
| Wasm linear-memory byte store/load | 767 | 80 | 90,516 | 16,000 |
| LLVM `alloca i8` / store / load | 700 | 80 | 90,516 | 16,000 |

A complete role-pair material save+open has four 16,000-AND material
invocations (garbler seal/open plus evaluator seal/open), or 64,000 ANDs per
held bit. Naively wrapping the entire tape at every durable boundary would
therefore cost 49,088,000 ANDs for the Wasm tape and 44,800,000 for LLVM,
versus 7,241,280 ANDs for the 80 shared-key memory accesses themselves. The
probes assert that this unfavorable ratio exists. **Conclusion:** material
must be encapsulated/cached in coarser fixed-size blocks and flushed only at
true persistence boundaries; per-bit tape re-encryption is not viable for
memory-heavy modules such as rustls. These figures are static circuit sizing,
not an OT byte benchmark: a next networked benchmark must compare Net OT and a
Ferret-backed `OtChannel` with the chosen block/cache policy.

## Encapsulation, cache, and return planning

`MaterialBlockLayout` now packs fixed-width role-local values into 16-byte AES
blocks without splitting a value across blocks. `MaterialBlockCachePlan` tracks
public resident slots and dirty block indices: repeated resident reads require
no reopening, and several writes to slots in the same packed block produce one
flush candidate. The layout is immediately useful for narrower opaque material
representations; current `U16` garbling labels are themselves 16 bytes and
therefore occupy one block each, so a separate label-compression/encapsulation
format is required before those labels can share a physical AES block.

`oram_batch` provides conservative public path-return planning. Consecutive
reads of the same public physical leaf share one fetched path. Writes are
batchable only for non-root buckets with no common public node; root commits
remain ordered because every Path ORAM path shares root. This intentionally
avoids claiming unsafe whole-path parallel commits. Integrating the planner
with a network tree transport is the next layer; the current ORAM driver
remains sequential.
