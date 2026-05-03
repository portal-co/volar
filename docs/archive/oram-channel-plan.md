# Recursive Path ORAM + Unified Communication Plan

> Supersedes Phase 4 (square-root ORAM) in `fhe-memory-enhancement-plan.md`.
> @reliability: experimental
> @ai: assisted

## Overview

Two new crates implement oblivious RAM and a generic protocol abstraction:

| Crate | Path | `std` | Purpose |
|---|---|---|---|
| `volar-channel` | `crates/channel/volar-channel/` | `no_std + alloc` | Packet-based protocol abstraction |
| `volar-oram` | `crates/oram/volar-oram/` | `no_std + alloc` | Recursive Path ORAM runtime |

Plus weaver integration: `StorageStrategy::Oram` on `TfheScheme`.

## Design Decisions

| Decision | Choice | Rationale |
|---|---|---|
| ORAM variant | Recursive Path ORAM | Standard, well-analyzed, O(log^2 N) amortized |
| Block type | `[u8; B]` const generic | Zero-cost, no heap for block data |
| Protocol model | `fn step(state, msg) -> (state', Yield)` | Serializable state enables checkpointing + ZK nesting |
| Protocol init | Separate `init()` function | Clean type distinction from step messages |
| Server impl | Both client + server | Self-contained testing, full protocol in one crate |
| Address modes | Client-knows + encrypted | Optimization path for single-party FHE |
| Eviction | Deterministic reverse-lexicographic | No randomness needed, proven safe for Z>=4 |
| Communication | Unified `Protocol` trait | Same abstraction for ORAM + future interactive VOLE-ZK |

---

## Part 1: `volar-channel`

### Purpose

Generic coroutine/CPS protocol abstraction. No crypto, no ORAM -- just the
pure-function state-machine pattern. Future interactive VOLE-ZK will also use
this trait.

### API

```rust
#![no_std]
extern crate alloc;

/// Result of a protocol step.
pub enum Yield<Done, Msg> {
    /// Protocol completed with this result.
    Done(Done),
    /// Send this message; caller will invoke step() again with the response.
    Send(Msg),
}

/// Pure-function protocol with serializable state.
///
/// State must be rkyv-serializable for checkpointing and circuit compilation.
pub trait Protocol {
    type State;
    type Incoming;
    type Outgoing;
    type Done;

    /// Start the protocol -- returns initial state + first action.
    fn init(params: Self::State) -> (Self::State, Yield<Self::Done, Self::Outgoing>);

    /// Process one incoming message, advance the state.
    fn step(state: Self::State, msg: Self::Incoming)
        -> (Self::State, Yield<Self::Done, Self::Outgoing>);
}
```

### Design Notes

- **State-passing (not `&mut self`)**: Makes serialization explicit. The state
  is the full snapshot -- checkpoint it, transfer to another machine, or
  compile the step function to a circuit.
- **No Transport trait**: The caller decides how messages move (TCP, shared
  memory, FHE ciphertext, VOLE commitment). Protocol is transport-agnostic.
- **rkyv serialization**: Consistent with existing codebase (all IR types
  already have rkyv derives).

### Estimated Size

~100-150 lines including doc comments and tests.

---

## Part 2: `volar-oram`

### Purpose

Recursive Path ORAM implementation as a `Protocol`. Spec-level crate --
reference implementation in concrete types, deterministic, no `rand`.
Randomness via `FnMut() -> u64` parameter.

### 2a: Core Data Structures

```rust
/// Single ORAM entry (block + metadata).
pub struct OramEntry<const B: usize> {
    pub addr: u64,          // u64::MAX = dummy/empty
    pub leaf: u64,          // assigned leaf in the tree
    pub data: [u8; B],      // payload block
}

/// Bucket of Z entries.
pub struct Bucket<const Z: usize, const B: usize> {
    pub entries: [OramEntry<B>; Z],
}

/// Binary tree of buckets (server-side state).
pub struct OramTree<const Z: usize, const B: usize> {
    pub levels: usize,                // L = tree depth
    pub buckets: Vec<Bucket<Z, B>>,   // 2^L - 1 nodes, heap-indexed
}

/// ORAM client state.
pub struct OramClientState<const Z: usize, const B: usize> {
    pub stash: Vec<OramEntry<B>>,
    pub max_stash: usize,
    pub position_map: PosMap,
    pub access_counter: u64,
}

/// Position map -- recursive or base case.
pub enum PosMap {
    /// Client holds the full map (<= BASE_CASE entries).
    Local { map: Vec<u64>, num_leaves: u64 },
    /// Positions stored in a sub-ORAM.
    Recursive {
        sub_client: Box<OramClientState<4, 16>>,
        entries_per_block: usize,
    },
}
```

### 2b: Tree Operations

```rust
impl<const Z: usize, const B: usize> OramTree<Z, B> {
    /// Create a new tree with L levels (2^(L-1) leaves, capacity 2^(L-1) blocks).
    pub fn new(levels: usize) -> Self;

    /// Read all buckets on the path from root to leaf.
    pub fn read_path(&self, leaf: u64) -> Vec<Bucket<Z, B>>;

    /// Write buckets back to the path from root to leaf.
    pub fn write_path(&mut self, leaf: u64, buckets: &[Bucket<Z, B>]);

    /// Compute heap indices for the path from root to leaf.
    pub fn path_indices(leaf: u64, levels: usize) -> Vec<usize>;
}
```

Tree layout: standard heap indexing. Root = index 0. Children of node `i` are
`2*i + 1` (left) and `2*i + 2` (right). Leaves are indices `2^(L-1) - 1` through
`2^L - 2`.

### 2c: Core Algorithm

```rust
pub enum AccessOp<const B: usize> {
    Read,
    Write([u8; B]),
}

pub enum AccessResult<const B: usize> {
    ReadValue([u8; B]),
    WriteAck,
}

/// Perform one ORAM access (given path buckets from server).
pub fn oram_access<const Z: usize, const B: usize>(
    client: &mut OramClientState<Z, B>,
    path_buckets: &[Bucket<Z, B>],
    addr: u64,
    op: AccessOp<B>,
    rng: &mut dyn FnMut() -> u64,
) -> (AccessResult<B>, Vec<Bucket<Z, B>>);
```

Algorithm:
1. Move all real entries from path buckets into stash.
2. Find block with `addr` in stash -- read or update.
3. Assign new random leaf for `addr` via `rng`.
4. Deterministic eviction: for each level from leaf to root, push stash
   entries whose assigned leaf is compatible with that subtree.
5. Pack remaining entries into new path buckets (Z per bucket, pad with dummies).
6. Return result + new buckets.

### 2d: Deterministic Eviction

```rust
fn eviction_target(access_counter: u64, num_leaves: u64) -> u64 {
    bit_reverse(access_counter % num_leaves, log2(num_leaves))
}
```

After each access at path `leaf`, perform one additional eviction pass along
the path to `eviction_target(counter)`. The counter-based target selection
covers all leaves uniformly over time.

Properties:
- No randomness needed (critical for ZK nesting).
- Proven safe for Z >= 4 with stash bound O(log N).
- Default stash bound: `2 * L + Z`.

### 2e: Recursive Position Map

```rust
impl PosMap {
    pub fn lookup(&self, addr: u64) -> u64;
    pub fn update(&mut self, addr: u64, new_leaf: u64, rng: &mut dyn FnMut() -> u64);
}
```

For `PosMap::Recursive`: one ORAM access on the sub-ORAM to read/write the
position entry. Multiple positions packed per block:
- Position = `ceil(log2(num_leaves))` bits.
- Pack `floor(B * 8 / position_bits)` entries per block.
- Recurse until sub-ORAM's position map has <= `BASE_CASE` entries (default 64).
- Typically 2-3 levels for practical N.

### 2f: Protocol Implementation

**Messages:**

```rust
pub enum ClientToServer<const Z: usize, const B: usize> {
    ReadPath { leaf: u64 },
    WritePath { leaf: u64, buckets: Vec<Bucket<Z, B>> },
}

pub enum ServerToClient<const Z: usize, const B: usize> {
    PathBuckets { buckets: Vec<Bucket<Z, B>> },
    Ack,
}
```

**Client protocol (2 rounds per access):**

```
  Client                              Server
    |                                    |
    |  [posmap lookup]                   |
    |  leaf = posmap[addr]               |
    |                                    |
    |-- ReadPath { leaf } ------------->|
    |<-- PathBuckets { buckets } -------|
    |                                    |
    |  [oram_access: scan, evict]        |
    |                                    |
    |-- WritePath { leaf, buckets } --->|
    |<-- Ack --------------------------|
    |                                    |
    |  => Done(result)                   |
```

**Client state machine:**

```rust
enum ClientStep { Start, WaitPath, WaitAck }

impl Protocol for OramClientProtocol<Z, B> {
    type State = (OramClientState<Z, B>, ClientStep, AccessOp<B>, u64);
    type Incoming = ServerToClient<Z, B>;
    type Outgoing = ClientToServer<Z, B>;
    type Done = AccessResult<B>;

    fn init(state) {
        // posmap lookup, send ReadPath { leaf }
    }
    fn step(state, msg) {
        // WaitPath: call oram_access, send WritePath
        // WaitAck: Done(result)
    }
}
```

**Server protocol:**

```rust
impl Protocol for OramServerProtocol<Z, B> {
    type State = OramTree<Z, B>;
    type Incoming = ClientToServer<Z, B>;
    type Outgoing = ServerToClient<Z, B>;
    type Done = ();

    fn step(state, msg) {
        // ReadPath: read path, send PathBuckets
        // WritePath: write path, send Ack, Done
    }
}
```

### 2g: Tests

1. **Correctness**: N random read/write ops, verify reads return last written value.
2. **Stash bound**: Many operations, verify stash never exceeds bound.
3. **Protocol round-trip**: Wire client + server, verify end-to-end correctness.
4. **Recursive posmap**: N > BASE_CASE to exercise recursion.
5. **Deterministic eviction**: Verify eviction targets cover all leaves uniformly.

Default test parameters: Z=4, B=16, L=4 (8 leaves), BASE_CASE=4.

---

## Part 3: Weaver Integration

### StorageStrategy

```rust
pub enum StorageStrategy {
    /// O(N) MUX tree (current default).
    Linear,
    /// O(log N) Path ORAM.
    Oram { bucket_size: usize, base_case: usize },
}
```

Added as a field on `TfheScheme`:

```rust
pub struct TfheScheme {
    pub use_cfg: bool,
    pub action_configs: BTreeMap<String, FheActionConfig>,
    pub storage_strategy: StorageStrategy,  // NEW
}
```

### Dispatch

`FheStorageCtx::emit_read` / `emit_write` check the strategy:

- **Linear**: current behavior (delegate to `scheme.emit_oblivious_read/write`).
- **Oram** + cell_count > threshold: generate code calling ORAM runtime.

Initial implementation generates direct function calls to `volar_oram` (Option A).
Full CPS state-machine generation (Option B) deferred to ZK nesting work.

### Threshold

- `cell_count <= 64` -> Linear (MUX tree).
- `cell_count > 64` -> Oram.
- Configurable via `StorageStrategy::Oram` fields.

---

## Part 4: ZK Nesting (Design Only -- Implementation Deferred)

Each ORAM step is a pure function `step(state, msg) -> (state', Yield)`.
This can be compiled to a VOLE-ZK circuit:

1. **State commitment**: ORAM client state committed via VOLE (one Vope per
   bit of state).
2. **Step verification**: Prover executes step(), verifier checks via
   Quicksilver AND checks.
3. **State transition**: Output state links to input state of next step via
   `MemoryCheckState` multiset check (zero AND gates, already implemented).
4. **Server messages**: Oracle inputs (fresh VOLE-authenticated values).

### Why It Works

- `MemoryCheckState` checks memory consistency via multiset hashing (zero AND
  gates, Schwartz-Zippel). Already exists in `volar-spec/src/vole/memory.rs`.
- `StorageMode::Commitment` already uses oracle inputs for storage reads.
- The ORAM step function is a pure function -- no I/O, no randomness beyond
  SpecRng (which can be committed).
- CPS transform already exists in VAFFLE lowering.

Implementation deferred until interactive VOLE-ZK is built.

---

## Implementation Order

| # | Task | Lines | Deps |
|---|---|---|---|
| 1 | Workspace setup: crate dirs + Cargo.toml | ~50 | -- |
| 2 | `volar-channel`: Yield, Protocol trait | ~100 | 1 |
| 3 | `volar-oram`: data structures | ~200 | 2 |
| 4 | `volar-oram`: tree operations | ~100 | 3 |
| 5 | `volar-oram`: core algorithm + eviction | ~350 | 4 |
| 6 | `volar-oram`: recursive position map | ~150 | 5 |
| 7 | `volar-oram`: Protocol impls | ~200 | 5-6 |
| 8 | `volar-oram`: tests | ~300 | 3-7 |
| 9 | Weaver: StorageStrategy + TfheScheme | ~50 | -- |
| 10 | Weaver: ORAM emit hooks | ~200 | 7, 9 |
| 11 | Integration tests | ~150 | 10 |

**Total: ~1,450 lines code + ~450 lines tests = ~1,900 lines**

---

## Relevant Files

### New
- `crates/channel/volar-channel/src/lib.rs`
- `crates/oram/volar-oram/src/lib.rs`

### Modified
- `Cargo.toml` (workspace members)
- `crates/compiler/volar-weaver/src/fhe.rs` (StorageStrategy, emit hooks)

### Reference (read-only)
- `crates/spec/volar-spec/src/vole/memory.rs` -- MemoryCheckState
- `crates/compiler/volar-weaver/src/vole.rs` -- StorageMode, VoleIrCtx
- `crates/spec/volar-spec/src/lib.rs` -- SpecRng
