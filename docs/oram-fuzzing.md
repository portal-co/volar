# ORAM Fuzzing Standard

> **Referenced by**: `crates/oram/volar-oram/src/lib.rs` (proptest module)
>
> **Status**: @reliability: experimental

---

## Overview

The `volar-oram` crate includes property-based fuzz tests using `proptest`.
These test the core ORAM data structure for correctness, safety invariants,
and protocol equivalence. The tests live in-crate (in a `#[cfg(test)] mod
proptests` block) following the same pattern as `volar-spec`'s TFHE tests.

Unlike the IR-level fuzzing in `volar-fuzz` (which uses a raw-data →
interpretation → differential-oracle pipeline), ORAM fuzzing uses a simpler
approach: random operation sequences against a HashMap reference oracle.

---

## Property Tests

Run with:

```
cargo test -p volar-oram
```

### Property I — HashMap equivalence

```
for each random (Read|Write) to random addr:
    oram_result == hashmap_result
```

The primary correctness property. ORAM should behave identically to a
`HashMap<u64, [u8; B]>`. Uninitialized reads return zeros. Writes are
acknowledged. Subsequent reads return the last written value.

### Property J — Stash boundedness

```
after every access: client.stash.len() <= client.max_stash
```

Verifies the stash overflow invariant holds statistically across many random
operation sequences. The stash bound is `2 * levels + Z`.

### Property K — Protocol equivalence

```
run_oram_protocol(ops) produces same read values as oram_access_local(ops)
```

Compares the interactive 2-round protocol path (`OramClientProtocol` +
`OramServerProtocol`) against the standalone `oram_access_local` function.
Both paths should produce identical read values for the same operation
sequence.

### Property L — Path invariant (tree integrity)

```
after all accesses:
    for every real block in tree:
        leaf_in_subtree(block.leaf, block.node_idx, levels) == true
```

Every real block in the tree must reside at a node that is on the path to
the block's assigned leaf. A block at the wrong node indicates a placement
or eviction bug.

### Property M — Recursive posmap equivalence

```
for each (addr, new_leaf) update:
    local_posmap.lookup(addr) == recursive_posmap.lookup(addr)
```

A `PosMap::Recursive` backed by a sub-ORAM should produce the same
lookup/update results as a `PosMap::Local` for the same operation sequence.

### Property N — No duplicate addresses

```
after all accesses:
    each addr appears at most once across tree + stash
```

Duplicates in the tree or stash indicate a bug in absorb/evict logic.
Each logical address should exist at most once in the entire ORAM system.

---

## Test Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| Z (bucket size) | 4 | Standard Path ORAM bucket size |
| B (block bytes) | 16 | Small for fast tests |
| Tree levels | 3–5 | 4–16 leaves |
| Max addresses | 4–16 | Matches leaf count |
| Ops per test | 30–200 | Varies by property |
| proptest cases | 50–200 | Property-dependent |

---

## Generator Design

ORAM tests use a simple generator rather than the two-layer raw → interpret
pattern used for IR tests:

```rust
struct RawOramOp {
    is_write: bool,   // Read or Write
    addr: u64,        // clamped to [0, max_addr)
    data_byte: u8,    // first byte of write payload
}
```

A proptest strategy generates `Vec<RawOramOp>` with addresses clamped to
the valid range. A `u64` seed initializes the deterministic RNG.

---

## Oracle

The reference oracle is a `BTreeMap<u64, [u8; 16]>`:
- **Write(addr, data)** → insert into map.
- **Read(addr)** → lookup in map (default: `[0u8; 16]`).

This is the simplest possible oracle: an ORAM should behave like a plain
key-value store.

---

## Adding New Properties

When adding a new ORAM property test:

1. Add it to the `proptests` module in `crates/oram/volar-oram/src/lib.rs`.
2. Use the letter-based naming convention: `prop_X_descriptive_name`.
3. Document it in this file with the property formula.
4. Update the count in `AGENTS.md`.

The letter sequence continues from the IR fuzzing properties (A–H), starting
at I for ORAM properties.

---

## See Also

- `crates/oram/volar-oram/src/lib.rs` — source of truth for ORAM tests
- `docs/fuzzing.md` — IR-level fuzzing infrastructure
- `docs/oram-channel-plan.md` — ORAM design decisions
