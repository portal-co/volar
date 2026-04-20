# ORAM & Channel Protocol

> Load when working on ORAM crates, channel protocol, or ORAM weaver integration.

## Channel Crate (`volar-channel`)

`volar-channel` provides a transport-agnostic protocol abstraction for interactive cryptographic protocols. It is `#![no_std] + alloc`.

Core types:
- **`Yield<D, O>`**: result of a protocol step — either `Done(D)` (protocol complete) or `Send { msg: O, next: S }` (send a message and continue with updated state).
- **`Protocol`** trait: `fn step(state: Self::State, incoming: Self::Incoming) -> (Self::State, Yield<Self::Done, Self::Outgoing>)` — pure function, serializable state.
- **`run_protocol`**: driver function that executes two `Protocol` implementations against each other in lockstep (useful for local simulation and testing).

Design invariants:
- No `Transport` trait. The caller decides how messages move (TCP, shared memory, FHE ciphertext, VOLE commitment).
- Deterministic: no hidden state, no randomness. RNG is passed in via `State` or `Incoming` if needed.
- State is passed by value, not `&mut self`, so it can be serialized for checkpointing.

## ORAM Crate (`volar-oram`)

`volar-oram` implements Recursive Path ORAM (Stefanov et al. 2018) for oblivious memory access in FHE/MPC circuits. It is `#![no_std] + alloc`.

Core types:
- **`OramTree<const Z: usize, const B: usize>`**: the server-side binary tree of buckets, each holding Z blocks of B bytes.
- **`OramClient<const Z: usize, const B: usize>`**: the client-side state holding the position map and stash.
- **`OramBlock<const B: usize>`**: a `(logical_address, leaf_label, data: [u8; B])` triple, or `DUMMY`.
- **`RecursiveOram<const Z: usize, const B: usize, const RZ: usize, const RB: usize>`**: wraps `OramClient` + `OramTree` with a recursive sub-ORAM for the position map when it exceeds the base-case threshold.

Algorithm:
1. Client looks up `pos_map[addr]` to get the current leaf.
2. Client picks a `new_leaf` uniformly at random.
3. Server reads the entire path from root to `old_leaf` and sends it.
4. Client absorbs the path into the stash, performs the read/write, assigns `new_leaf` to the accessed address.
5. Client runs deterministic reverse-lexicographic eviction (two paths per access).
6. Client writes back evicted buckets to the server.

Key design points:
- **Deterministic eviction**: reverse-lexicographic order via `G(cnt)` — bit-reversal of the access counter. Two eviction paths per access for O(log N) stash size.
- **Recursive position map**: when `num_leaves > base_case`, the position map is stored in a sub-ORAM with smaller blocks (packing multiple position entries per block).
- **Protocol integration**: both `OramAccessProtocol` and `RecursiveOramProtocol` implement the `volar-channel::Protocol` trait for interactive client-server execution.

## ORAM Access Protocol (Weaver Transformation)

A single ORAM access (read or write) in the woven circuit maps to this IR sequence:

1. **Begin action**: circuit sends encrypted addr -> client returns plaintext leaf (1 ActionCall, `output_public=[true]`)
2. **Tree read**: circuit reads path at plaintext leaf from tree storage using `StorageId(1000 + storage_id)` (direct indexed, no MUX)
3. **Process action**: circuit sends `(path_buckets, write_data, is_write)` -> client returns `(write_back_path, read_data, evict_leaf_1, evict_leaf_2)` (1 ActionCall, `output_public=[false, false, true, true]`)
4. **Tree write-back**: circuit writes `write_back_path` to tree storage at leaf
5. **Eviction** (×2): for each eviction leaf, read tree path → `evict` action → write back evicted path

Each access emits 4 ActionCalls (begin + process + 2 evicts) and uses 3 action declarations per storage config (begin, process, evict).

The ORAM rewrite pass (`oram.rs`) transforms `StorageRead`/`StorageWrite` IR statements into this sequence of ActionCalls and public-address storage operations.

## Three-Phase Handler API

The single-call `oram_access_local` has been decomposed into three handler methods on `OramClient` to match the circuit's action boundary:

### `handle_begin(addr, levels, rng) -> ActionBeginState`
- Looks up the position map to get `old_leaf`
- Assigns a random `new_leaf`
- Returns `ActionBeginState { old_leaf, new_leaf, addr }`

### `handle_process(begin_state, path, write_data, is_write, levels) -> (Vec<Bucket>, [u8; B], u64, u64)`
- Absorbs the path from the tree into the stash
- Performs the read/write operation
- Packs stash entries back into buckets along the path
- Returns `(write_back_path, read_data, evict_leaf_1, evict_leaf_2)`

### `handle_evict(path, evict_leaf, levels) -> Vec<Bucket>`
- Absorbs an eviction path into the stash
- Packs stash entries back along the eviction path
- Returns the evicted path for tree write-back

The `test_three_phase_matches_local_oracle` unit test and `prop_o_three_phase_equivalence` proptest verify that this decomposition produces identical results to `oram_access_local`.

## OramConfig Automation

`OramConfig` (in `oram.rs`) centralizes all per-storage ORAM parameter management:

- **`tree_cell_count(sid)`**: returns `Some(num_nodes)` if `sid` is this config's tree storage, else `None`
- **`oram_cell_count_fn(configs)`**: builds the closure for `derive_ir_storage_config` from a slice of configs
- **`apply_mono(env)`**: adds Z, B, L, N to a `MonoEnv` for monomorphization
- **`configure_scheme(scheme)`**: registers action configs on a `TfheScheme`

## ORAM Weaver Integration Approach

**Compile the ORAM runtime through the volar-compiler pipeline** and stitch it into the circuit. Do NOT add special modes to the existing FHE weaver. The spec source (`volar-oram-core`) is linked alongside the woven circuit code via the linkage system.

Phase 2b (StorageStrategy/PBS) was **skipped** — went directly to Phase 4 (ORAM). PBS is only suitable for cleartext/read-only LUT lookups, NOT for encrypted mutable storage.

## ORAM Fuzzing (Properties I-P)

ORAM property tests use **proptest** in-crate (`#[cfg(test)] mod proptests`), continuing the letter-based naming from IR fuzzing (A-H):

| Property | Name | What it checks |
|---|---|---|
| I | HashMap equivalence | Sequence of read/write ops matches a `BTreeMap` oracle |
| J | Stash boundedness | Stash size stays <= `Z * tree_height` after every operation |
| K | Protocol equivalence | `run_protocol` path produces same results as local `access()` path |
| L | Path invariant | Every node on a root-to-leaf path has the correct depth and index |
| M | Recursive posmap equivalence | `RecursiveOram` read/write matches `BTreeMap` oracle |
| N | No duplicate addresses | After a sequence of writes, no logical address appears twice in tree + stash |
| O | Three-phase equivalence | `handle_begin/process/evict` decomposition matches `oram_access_local` |
| P | Three-phase no duplicates | Three-phase protocol maintains no-duplicate-addresses invariant |

## Relevant Files

- `crates/oram/volar-oram/src/lib.rs` — full ORAM implementation + 26 unit tests + 8 proptests
- `crates/oram/volar-oram-core/src/lib.rs` — server-side ORAM source (linked spec)
- `crates/channel/volar-channel/src/lib.rs` — `Protocol` trait + `run_protocol` + tests
- `crates/compiler/volar-weaver/src/oram.rs` — ORAM rewrite pass + 21 config tests + 13 rewrite tests + 7 linking tests
- `docs/oram-channel-plan.md` — approved design plan
- `docs/oram-fuzzing.md` — property documentation
