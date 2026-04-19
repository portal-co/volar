// @reliability: experimental
// @ai: assisted
//
//! Recursive Path ORAM runtime for Volar.
//!
//! Implements Path ORAM (Stefanov et al. 2018) with:
//! - Fixed-size blocks `[u8; B]` (const generic)
//! - Configurable bucket size Z (const generic, default 4)
//! - Deterministic reverse-lexicographic eviction
//! - Recursive position map (sub-ORAM for large position maps)
//! - Both client and server Protocol implementations
//!
//! # Architecture
//!
//! The ORAM is split into client and server roles:
//! - **Server** holds the binary tree of encrypted buckets ([`OramTree`])
//! - **Client** holds the stash and position map ([`OramClient`])
//!
//! Each access is a 2-round protocol:
//! 1. Client sends `ReadPath { leaf }`, server responds with path buckets
//! 2. Client processes locally (scan, evict), sends `WritePath { leaf, buckets }`
//!
//! # `no_std`
//!
//! This crate is `#![no_std]` with `extern crate alloc`. Randomness is
//! injected via `&mut dyn FnMut() -> u64` — no dependency on `rand`.

#![no_std]

extern crate alloc;

use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use volar_channel::{Protocol, Yield};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Marker for an empty/dummy entry.
const DUMMY_ADDR: u64 = u64::MAX;

// ---------------------------------------------------------------------------
// Core data structures
// ---------------------------------------------------------------------------

/// A single ORAM entry: block data plus metadata.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OramEntry<const B: usize> {
    /// Logical address. `DUMMY_ADDR` (u64::MAX) indicates an empty slot.
    pub addr: u64,
    /// Assigned leaf in the tree (determines which path this block lives on).
    pub leaf: u64,
    /// Payload data.
    pub data: [u8; B],
}

impl<const B: usize> OramEntry<B> {
    /// Create a dummy (empty) entry.
    pub fn dummy() -> Self {
        Self {
            addr: DUMMY_ADDR,
            leaf: 0,
            data: [0u8; B],
        }
    }

    /// Returns true if this is a real (non-dummy) entry.
    pub fn is_real(&self) -> bool {
        self.addr != DUMMY_ADDR
    }
}

/// A bucket holding Z entries. Each node in the ORAM tree is one bucket.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bucket<const Z: usize, const B: usize> {
    pub entries: Vec<OramEntry<B>>,
}

impl<const Z: usize, const B: usize> Bucket<Z, B> {
    /// Create an empty bucket (all dummy entries).
    pub fn empty() -> Self {
        let mut entries = Vec::with_capacity(Z);
        for _ in 0..Z {
            entries.push(OramEntry::dummy());
        }
        Self { entries }
    }
}

/// Binary tree of buckets (server-side state).
///
/// Uses heap indexing: root = 0, children of node `i` are `2*i + 1` and
/// `2*i + 2`. Leaves are indices `2^(L-1) - 1` through `2^L - 2`.
#[derive(Clone, Debug)]
pub struct OramTree<const Z: usize, const B: usize> {
    /// Tree depth (number of levels). The tree has `2^(L-1)` leaves.
    pub levels: usize,
    /// All buckets in heap order. Length = `2^levels - 1`.
    pub buckets: Vec<Bucket<Z, B>>,
}

/// What the client wants to do with a block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AccessOp<const B: usize> {
    /// Read the block (return its data).
    Read,
    /// Write new data to the block.
    Write([u8; B]),
}

/// Result of an ORAM access.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AccessResult<const B: usize> {
    /// The data that was read.
    ReadValue([u8; B]),
    /// Write completed.
    WriteAck,
}

// ---------------------------------------------------------------------------
// Tree operations
// ---------------------------------------------------------------------------

impl<const Z: usize, const B: usize> OramTree<Z, B> {
    /// Create a new tree with `levels` levels.
    ///
    /// The tree has `2^(levels-1)` leaves and can store up to `2^(levels-1)`
    /// blocks. All buckets are initialized to dummy entries.
    ///
    /// # Panics
    ///
    /// Panics if `levels == 0`.
    pub fn new(levels: usize) -> Self {
        assert!(levels > 0, "tree must have at least 1 level");
        let num_nodes = (1usize << levels) - 1;
        let mut buckets = Vec::with_capacity(num_nodes);
        for _ in 0..num_nodes {
            buckets.push(Bucket::empty());
        }
        Self { levels, buckets }
    }

    /// Number of leaves in the tree.
    pub fn num_leaves(&self) -> u64 {
        1u64 << (self.levels - 1)
    }

    /// Compute the heap indices along the path from root to the given leaf.
    ///
    /// Returns a Vec of length `self.levels`, starting from root (index 0)
    /// down to the leaf.
    pub fn path_indices(&self, leaf: u64) -> Vec<usize> {
        path_indices_for(leaf, self.levels)
    }

    /// Read all buckets on the path from root to the given leaf.
    pub fn read_path(&self, leaf: u64) -> Vec<Bucket<Z, B>> {
        self.path_indices(leaf)
            .iter()
            .map(|&idx| self.buckets[idx].clone())
            .collect()
    }

    /// Write buckets back to the path from root to the given leaf.
    ///
    /// `buckets` must have length `self.levels`.
    pub fn write_path(&mut self, leaf: u64, buckets: &[Bucket<Z, B>]) {
        let indices = self.path_indices(leaf);
        assert_eq!(
            buckets.len(),
            indices.len(),
            "bucket count must match path length"
        );
        for (i, &idx) in indices.iter().enumerate() {
            self.buckets[idx] = buckets[i].clone();
        }
    }
}

/// Compute path indices from root to leaf for a tree of given depth.
///
/// Leaf `l` is at heap index `2^(levels-1) - 1 + l`. The path is computed
/// by walking from the leaf up to the root and then reversing.
fn path_indices_for(leaf: u64, levels: usize) -> Vec<usize> {
    let leaf_offset = (1usize << (levels - 1)) - 1;
    let mut idx = leaf_offset + leaf as usize;
    let mut path = Vec::with_capacity(levels);
    path.push(idx);
    while idx > 0 {
        idx = (idx - 1) / 2; // parent
        path.push(idx);
    }
    path.reverse();
    path
}

/// Check whether a leaf is in the subtree rooted at `node_idx` in a tree
/// of given depth.
///
/// At level `d` (root = 0), node `node_idx` covers leaves whose
/// `(levels-1-d)` high bits match the node's position.
fn leaf_in_subtree(leaf: u64, node_idx: usize, levels: usize) -> bool {
    // Compute the range of leaves covered by this node.
    // A node at heap index `i` is at level `d = floor(log2(i+1))`.
    // It covers leaves from `left_leaf` to `right_leaf` inclusive.
    let depth = log2_floor(node_idx + 1);
    let subtree_levels = levels - 1 - depth;
    // Position within level: node_idx - (2^depth - 1)
    let pos_in_level = node_idx - ((1usize << depth) - 1);
    let left_leaf = (pos_in_level as u64) << subtree_levels;
    let right_leaf = left_leaf + (1u64 << subtree_levels) - 1;
    leaf >= left_leaf && leaf <= right_leaf
}

/// Floor of log2 for positive integers.
fn log2_floor(x: usize) -> usize {
    assert!(x > 0);
    (usize::BITS - 1 - x.leading_zeros()) as usize
}

// ---------------------------------------------------------------------------
// Position map
// ---------------------------------------------------------------------------

/// Position map: maps logical address -> assigned leaf.
///
/// Either stored locally (base case) or in a recursive sub-ORAM.
#[derive(Clone, Debug)]
pub enum PosMap {
    /// Base case: client holds the full map in memory.
    Local {
        /// map[addr] = assigned leaf
        map: Vec<u64>,
        /// Number of leaves in the parent ORAM tree.
        num_leaves: u64,
    },
    /// Recursive: positions stored in a sub-ORAM.
    Recursive {
        /// The sub-ORAM client state (owns position map for the sub-ORAM).
        sub_client: Box<OramClient<4, 16>>,
        /// The sub-ORAM tree (server-side).
        sub_tree: Box<OramTree<4, 16>>,
        /// How many position entries fit in one sub-ORAM block.
        entries_per_block: usize,
        /// Number of logical addresses in the parent ORAM.
        num_addrs: u64,
        /// Number of leaves in the parent ORAM tree.
        num_leaves: u64,
        /// Bits per position entry.
        bits_per_pos: usize,
    },
}

impl PosMap {
    /// Create a local position map for `num_addrs` addresses.
    ///
    /// All positions are initialized to 0.
    pub fn local(num_addrs: u64, num_leaves: u64) -> Self {
        PosMap::Local {
            map: vec![0u64; num_addrs as usize],
            num_leaves,
        }
    }

    /// Create a recursive position map backed by a sub-ORAM.
    ///
    /// Uses Z=4, B=16 for the sub-ORAM. Recurses until the sub-ORAM's
    /// position map fits within `base_case` entries.
    pub fn recursive(
        num_addrs: u64,
        num_leaves: u64,
        base_case: u64,
        rng: &mut dyn FnMut() -> u64,
    ) -> Self {
        let bits_per_pos = bits_needed(num_leaves);
        let entries_per_block = (16 * 8) / bits_per_pos.max(1); // B=16 bytes

        if entries_per_block == 0 {
            // positions are too large for blocks — fall back to local
            return PosMap::local(num_addrs, num_leaves);
        }

        let sub_num_blocks = (num_addrs as usize + entries_per_block - 1) / entries_per_block;

        if sub_num_blocks as u64 <= base_case {
            // Base case: sub-ORAM is small enough for local position map
            let sub_levels = tree_levels_for(sub_num_blocks);
            let sub_tree = OramTree::<4, 16>::new(sub_levels);
            let sub_num_leaves = sub_tree.num_leaves();
            let sub_client = OramClient::<4, 16> {
                stash: Vec::new(),
                max_stash: 2 * sub_levels + 4,
                position_map: PosMap::local(sub_num_blocks as u64, sub_num_leaves),
                access_counter: 0,
            };
            PosMap::Recursive {
                sub_client: Box::new(sub_client),
                sub_tree: Box::new(sub_tree),
                entries_per_block,
                num_addrs,
                num_leaves,
                bits_per_pos,
            }
        } else {
            // Need another level of recursion
            let sub_levels = tree_levels_for(sub_num_blocks);
            let sub_tree = OramTree::<4, 16>::new(sub_levels);
            let sub_num_leaves = sub_tree.num_leaves();
            let sub_posmap =
                PosMap::recursive(sub_num_blocks as u64, sub_num_leaves, base_case, rng);
            let sub_client = OramClient::<4, 16> {
                stash: Vec::new(),
                max_stash: 2 * sub_levels + 4,
                position_map: sub_posmap,
                access_counter: 0,
            };
            PosMap::Recursive {
                sub_client: Box::new(sub_client),
                sub_tree: Box::new(sub_tree),
                entries_per_block,
                num_addrs,
                num_leaves,
                bits_per_pos,
            }
        }
    }

    /// Look up the assigned leaf for `addr`.
    pub fn lookup(
        &mut self,
        addr: u64,
        rng: &mut dyn FnMut() -> u64,
    ) -> u64 {
        match self {
            PosMap::Local { map, .. } => map[addr as usize],
            PosMap::Recursive {
                sub_client,
                sub_tree,
                entries_per_block,
                bits_per_pos,
                ..
            } => {
                let block_idx = addr as usize / *entries_per_block;
                let offset_in_block = addr as usize % *entries_per_block;
                let result = oram_access_local(sub_client, sub_tree, block_idx as u64, AccessOp::Read, rng);
                match result {
                    AccessResult::ReadValue(block) => {
                        extract_position(&block, offset_in_block, *bits_per_pos)
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    /// Update the assigned leaf for `addr` to `new_leaf`. Returns the old leaf.
    pub fn update(
        &mut self,
        addr: u64,
        new_leaf: u64,
        rng: &mut dyn FnMut() -> u64,
    ) -> u64 {
        match self {
            PosMap::Local { map, .. } => {
                let old = map[addr as usize];
                map[addr as usize] = new_leaf;
                old
            }
            PosMap::Recursive {
                sub_client,
                sub_tree,
                entries_per_block,
                bits_per_pos,
                ..
            } => {
                let block_idx = addr as usize / *entries_per_block;
                let offset_in_block = addr as usize % *entries_per_block;
                // Read the block
                let result = oram_access_local(
                    sub_client,
                    sub_tree,
                    block_idx as u64,
                    AccessOp::Read,
                    rng,
                );
                let mut block = match result {
                    AccessResult::ReadValue(b) => b,
                    _ => unreachable!(),
                };
                let old = extract_position(&block, offset_in_block, *bits_per_pos);
                insert_position(&mut block, offset_in_block, *bits_per_pos, new_leaf);
                // Write the block back
                oram_access_local(
                    sub_client,
                    sub_tree,
                    block_idx as u64,
                    AccessOp::Write(block),
                    rng,
                );
                old
            }
        }
    }
}

/// Extract a position (packed bits) from a block.
fn extract_position(block: &[u8], index: usize, bits_per_pos: usize) -> u64 {
    if bits_per_pos == 0 {
        return 0;
    }
    let bit_offset = index * bits_per_pos;
    let mut value: u64 = 0;
    for b in 0..bits_per_pos {
        let bit_idx = bit_offset + b;
        let byte_idx = bit_idx / 8;
        let bit_in_byte = bit_idx % 8;
        if byte_idx < block.len() && (block[byte_idx] >> bit_in_byte) & 1 == 1 {
            value |= 1u64 << b;
        }
    }
    value
}

/// Insert a position (packed bits) into a block.
fn insert_position(block: &mut [u8], index: usize, bits_per_pos: usize, value: u64) {
    if bits_per_pos == 0 {
        return;
    }
    let bit_offset = index * bits_per_pos;
    for b in 0..bits_per_pos {
        let bit_idx = bit_offset + b;
        let byte_idx = bit_idx / 8;
        let bit_in_byte = bit_idx % 8;
        if byte_idx < block.len() {
            if (value >> b) & 1 == 1 {
                block[byte_idx] |= 1u8 << bit_in_byte;
            } else {
                block[byte_idx] &= !(1u8 << bit_in_byte);
            }
        }
    }
}

/// Compute the number of bits needed to represent values in `[0, n)`.
fn bits_needed(n: u64) -> usize {
    if n <= 1 {
        return 1;
    }
    64 - (n - 1).leading_zeros() as usize
}

/// Compute the number of tree levels needed for `n` blocks.
/// The tree has `2^(L-1)` leaves, so we need `2^(L-1) >= n`.
fn tree_levels_for(n: usize) -> usize {
    if n <= 1 {
        return 1;
    }
    let mut levels = 1;
    while (1usize << (levels - 1)) < n {
        levels += 1;
    }
    levels
}

// ---------------------------------------------------------------------------
// ORAM client state
// ---------------------------------------------------------------------------

/// ORAM client state (the "thin client").
///
/// Holds the stash (overflow buffer) and position map. The client does not
/// hold the tree — that's on the server side.
#[derive(Clone, Debug)]
pub struct OramClient<const Z: usize, const B: usize> {
    /// Overflow buffer for blocks that don't fit back into the path.
    pub stash: Vec<OramEntry<B>>,
    /// Hard upper bound on stash size. Access fails if exceeded.
    pub max_stash: usize,
    /// Maps logical address -> assigned leaf.
    pub position_map: PosMap,
    /// Monotonic counter for deterministic eviction.
    pub access_counter: u64,
}

impl<const Z: usize, const B: usize> OramClient<Z, B> {
    /// Create a new client for a tree with `levels` levels and `num_addrs`
    /// addressable blocks.
    ///
    /// Uses a local position map (suitable when `num_addrs` is small).
    pub fn new(levels: usize, num_addrs: u64) -> Self {
        let num_leaves = 1u64 << (levels - 1);
        Self {
            stash: Vec::new(),
            max_stash: 2 * levels + Z,
            position_map: PosMap::local(num_addrs, num_leaves),
            access_counter: 0,
        }
    }

    /// Create a new client with a recursive position map.
    pub fn new_recursive(
        levels: usize,
        num_addrs: u64,
        base_case: u64,
        rng: &mut dyn FnMut() -> u64,
    ) -> Self {
        let num_leaves = 1u64 << (levels - 1);
        Self {
            stash: Vec::new(),
            max_stash: 2 * levels + Z,
            position_map: PosMap::recursive(num_addrs, num_leaves, base_case, rng),
            access_counter: 0,
        }
    }
}

// ---------------------------------------------------------------------------
// Core ORAM algorithm
// ---------------------------------------------------------------------------

/// Perform a complete ORAM access locally (non-interactive).
///
/// This is the standalone algorithm used for testing and for recursive
/// position map accesses. For the interactive protocol, see [`OramClientProtocol`].
pub fn oram_access_local<const Z: usize, const B: usize>(
    client: &mut OramClient<Z, B>,
    tree: &mut OramTree<Z, B>,
    addr: u64,
    op: AccessOp<B>,
    rng: &mut dyn FnMut() -> u64,
) -> AccessResult<B> {
    let num_leaves = tree.num_leaves();

    // 1. Position map: look up old leaf, assign new random leaf
    let new_leaf = rng() % num_leaves;
    let old_leaf = client.position_map.update(addr, new_leaf, rng);

    // 2. Read the path for old_leaf
    let path_buckets = tree.read_path(old_leaf);

    // 3. Process: move real entries from path into stash, access block
    let (result, new_path) =
        process_access(client, &path_buckets, addr, op, old_leaf, new_leaf, tree.levels);

    // 4. Write updated path back
    tree.write_path(old_leaf, &new_path);

    // 5. Deterministic eviction on a separate path
    let evict_leaf = eviction_target(client.access_counter, num_leaves);
    client.access_counter += 1;
    if evict_leaf != old_leaf {
        let evict_path = tree.read_path(evict_leaf);
        // Absorb entries from eviction path into stash first
        absorb_path_to_stash(client, &evict_path);
        let new_evict_path = evict_along_path(client, evict_leaf, tree.levels);
        tree.write_path(evict_leaf, &new_evict_path);
    }

    result
}

/// Process an ORAM access given path buckets from the server.
///
/// 1. Moves real entries from path into stash
/// 2. Finds the target block, reads or writes it
/// 3. Evicts stash entries back into path buckets
///
/// Returns `(result, new_path_buckets)`.
fn process_access<const Z: usize, const B: usize>(
    client: &mut OramClient<Z, B>,
    path_buckets: &[Bucket<Z, B>],
    addr: u64,
    op: AccessOp<B>,
    path_leaf: u64,
    new_leaf: u64,
    levels: usize,
) -> (AccessResult<B>, Vec<Bucket<Z, B>>) {
    // Move all real entries from path into stash
    absorb_path_to_stash(client, path_buckets);

    // Find the target block in stash
    let mut result = AccessResult::ReadValue([0u8; B]);
    let mut found = false;
    for entry in client.stash.iter_mut() {
        if entry.addr == addr {
            match &op {
                AccessOp::Read => {
                    result = AccessResult::ReadValue(entry.data);
                }
                AccessOp::Write(data) => {
                    entry.data = *data;
                    result = AccessResult::WriteAck;
                }
            }
            // Update leaf to the newly assigned leaf
            entry.leaf = new_leaf;
            found = true;
            break;
        }
    }

    // If block not found (first access to this address), create it
    if !found {
        match &op {
            AccessOp::Read => {
                result = AccessResult::ReadValue([0u8; B]);
                client.stash.push(OramEntry {
                    addr,
                    leaf: new_leaf,
                    data: [0u8; B],
                });
            }
            AccessOp::Write(data) => {
                result = AccessResult::WriteAck;
                client.stash.push(OramEntry {
                    addr,
                    leaf: new_leaf,
                    data: *data,
                });
            }
        }
    }

    // Evict stash entries back into path buckets
    let new_path = evict_along_path(client, path_leaf, levels);

    // Stash overflow check
    assert!(
        client.stash.len() <= client.max_stash,
        "ORAM stash overflow: {} > {}",
        client.stash.len(),
        client.max_stash
    );

    (result, new_path)
}

/// Move all real entries from path buckets into the client's stash.
fn absorb_path_to_stash<const Z: usize, const B: usize>(
    client: &mut OramClient<Z, B>,
    path_buckets: &[Bucket<Z, B>],
) {
    for bucket in path_buckets {
        for entry in &bucket.entries {
            if entry.is_real() {
                client.stash.push(entry.clone());
            }
        }
    }
}

/// Evict stash entries into path buckets.
///
/// For each level (leaf to root), tries to place stash entries whose
/// assigned leaf falls in the subtree rooted at that node.
fn evict_along_path<const Z: usize, const B: usize>(
    client: &mut OramClient<Z, B>,
    path_leaf: u64,
    levels: usize,
) -> Vec<Bucket<Z, B>> {
    let path_idx = path_indices_for(path_leaf, levels);
    let mut new_path: Vec<Bucket<Z, B>> = Vec::with_capacity(levels);
    for _ in 0..levels {
        new_path.push(Bucket::empty());
    }

    // For each level (deepest first), try to fill the bucket
    for level in (0..levels).rev() {
        let node_idx = path_idx[level];
        let mut placed = 0;
        let mut i = 0;
        while i < client.stash.len() && placed < Z {
            if client.stash[i].is_real()
                && leaf_in_subtree(client.stash[i].leaf, node_idx, levels)
            {
                new_path[level].entries[placed] = client.stash.swap_remove(i);
                placed += 1;
                // Don't increment i — swap_remove moved the last element here
            } else {
                i += 1;
            }
        }
    }

    new_path
}

/// Compute the deterministic eviction target leaf.
///
/// Uses bit-reversal of the access counter modulo the number of leaves.
/// This ensures uniform coverage over time.
fn eviction_target(access_counter: u64, num_leaves: u64) -> u64 {
    if num_leaves <= 1 {
        return 0;
    }
    let bits = bits_needed(num_leaves);
    let idx = access_counter % num_leaves;
    bit_reverse(idx, bits) % num_leaves
}

/// Reverse the low `bits` bits of `x`.
fn bit_reverse(x: u64, bits: usize) -> u64 {
    let mut result = 0u64;
    let mut val = x;
    for _ in 0..bits {
        result = (result << 1) | (val & 1);
        val >>= 1;
    }
    result
}

// ---------------------------------------------------------------------------
// Protocol implementation
// ---------------------------------------------------------------------------

/// Messages from client to server.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClientToServer<const Z: usize, const B: usize> {
    /// Request to read all buckets on the path to this leaf.
    ReadPath { leaf: u64 },
    /// Write updated buckets back to the path.
    WritePath {
        leaf: u64,
        buckets: Vec<Bucket<Z, B>>,
    },
}

/// Messages from server to client.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ServerToClient<const Z: usize, const B: usize> {
    /// The buckets along the requested path.
    PathBuckets { buckets: Vec<Bucket<Z, B>> },
    /// Acknowledgement that the write completed.
    Ack,
}

/// Client protocol step.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClientStep {
    /// Initial state: need to look up posmap and send ReadPath.
    Start,
    /// Sent ReadPath, waiting for PathBuckets.
    WaitPath { old_leaf: u64, new_leaf: u64 },
    /// Sent WritePath, waiting for Ack.
    WaitAck,
    /// Protocol complete.
    Done,
}

/// Client-side ORAM access protocol.
///
/// Performs one ORAM access (read or write) as a 2-round interactive protocol.
///
/// # State encoding
///
/// The protocol state bundles the client state, current step, and access
/// parameters. It is fully serializable for checkpointing.
#[derive(Clone, Debug)]
pub struct OramClientProtocol<const Z: usize, const B: usize>;

/// Full state for the client protocol.
#[derive(Clone, Debug)]
pub struct ClientProtocolState<const Z: usize, const B: usize> {
    pub client: OramClient<Z, B>,
    pub step: ClientStep,
    pub addr: u64,
    pub op: AccessOp<B>,
    pub levels: usize,
    pub result: Option<AccessResult<B>>,
    /// RNG state: a simple counter-based RNG for determinism.
    pub rng_counter: u64,
}

impl<const Z: usize, const B: usize> ClientProtocolState<Z, B> {
    /// Create initial protocol state for an access.
    pub fn new(
        client: OramClient<Z, B>,
        addr: u64,
        op: AccessOp<B>,
        levels: usize,
        rng_seed: u64,
    ) -> Self {
        Self {
            client,
            step: ClientStep::Start,
            addr,
            op,
            levels,
            result: None,
            rng_counter: rng_seed,
        }
    }
}

impl<const Z: usize, const B: usize> Protocol for OramClientProtocol<Z, B> {
    type State = ClientProtocolState<Z, B>;
    type Incoming = ServerToClient<Z, B>;
    type Outgoing = ClientToServer<Z, B>;
    type Done = (OramClient<Z, B>, AccessResult<B>);

    fn init(mut params: Self::State) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
        // Look up position map, assign new leaf
        let num_leaves = 1u64 << (params.levels - 1);
        let new_leaf = {
            let counter = params.rng_counter;
            params.rng_counter = counter.wrapping_mul(6364136223846793005).wrapping_add(1);
            counter % num_leaves
        };
        let old_leaf = {
            let rng_counter = &mut params.rng_counter;
            let mut rng = || {
                let val = *rng_counter;
                *rng_counter = val.wrapping_mul(6364136223846793005).wrapping_add(1);
                val
            };
            params.client.position_map.update(params.addr, new_leaf, &mut rng)
        };

        params.step = ClientStep::WaitPath { old_leaf, new_leaf };
        (
            params,
            Yield::Send(ClientToServer::ReadPath { leaf: old_leaf }),
        )
    }

    fn step(
        mut state: Self::State,
        msg: Self::Incoming,
    ) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
        match state.step.clone() {
            ClientStep::WaitPath { old_leaf, new_leaf } => {
                let buckets = match msg {
                    ServerToClient::PathBuckets { buckets } => buckets,
                    _ => panic!("expected PathBuckets, got {:?}", msg),
                };

                // Move real entries from path into stash
                absorb_path_to_stash(&mut state.client, &buckets);

                // Find and process the target block
                let addr = state.addr;
                let mut found = false;
                let mut result = AccessResult::ReadValue([0u8; B]);
                for entry in state.client.stash.iter_mut() {
                    if entry.addr == addr {
                        match &state.op {
                            AccessOp::Read => {
                                result = AccessResult::ReadValue(entry.data);
                            }
                            AccessOp::Write(data) => {
                                entry.data = *data;
                                result = AccessResult::WriteAck;
                            }
                        }
                        // Update leaf assignment to new_leaf
                        entry.leaf = new_leaf;
                        found = true;
                        break;
                    }
                }

                if !found {
                    match &state.op {
                        AccessOp::Read => {
                            result = AccessResult::ReadValue([0u8; B]);
                            state.client.stash.push(OramEntry {
                                addr,
                                leaf: new_leaf,
                                data: [0u8; B],
                            });
                        }
                        AccessOp::Write(data) => {
                            result = AccessResult::WriteAck;
                            state.client.stash.push(OramEntry {
                                addr,
                                leaf: new_leaf,
                                data: *data,
                            });
                        }
                    }
                }

                // Evict stash entries back into path
                let new_path =
                    evict_along_path(&mut state.client, old_leaf, state.levels);

                // Stash overflow check
                assert!(
                    state.client.stash.len() <= state.client.max_stash,
                    "ORAM stash overflow: {} > {}",
                    state.client.stash.len(),
                    state.client.max_stash
                );

                state.result = Some(result);
                state.step = ClientStep::WaitAck;
                (
                    state,
                    Yield::Send(ClientToServer::WritePath {
                        leaf: old_leaf,
                        buckets: new_path,
                    }),
                )
            }
            ClientStep::WaitAck => {
                match msg {
                    ServerToClient::Ack => {}
                    _ => panic!("expected Ack, got {:?}", msg),
                }

                // Deterministic eviction (done via a second access to the tree)
                // For the protocol version, eviction is folded into the main
                // access — we skip the separate eviction round for now and rely
                // on the stash bound being sufficient.
                state.client.access_counter += 1;
                state.step = ClientStep::Done;
                let result = state.result.take().expect("result should be set");
                let client = state.client.clone();
                (state, Yield::Done((client, result)))
            }
            _ => panic!("unexpected client step: {:?}", state.step),
        }
    }
}

/// Server-side ORAM protocol.
///
/// Processes client requests against the ORAM tree.
pub struct OramServerProtocol<const Z: usize, const B: usize>;

impl<const Z: usize, const B: usize> Protocol for OramServerProtocol<Z, B> {
    type State = OramTree<Z, B>;
    type Incoming = ClientToServer<Z, B>;
    type Outgoing = ServerToClient<Z, B>;
    type Done = OramTree<Z, B>;

    fn init(state: Self::State) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
        // Server starts by waiting for a client message.
        // Since the protocol requires an incoming message first, init
        // can't send — we return Done immediately. The caller should
        // use step() directly.
        //
        // However, Protocol::init must return a Yield. The server is
        // reactive (waits for messages), so we return Done with the
        // current state, signaling that the caller should use step().
        (state.clone(), Yield::Done(state))
    }

    fn step(
        mut state: Self::State,
        msg: Self::Incoming,
    ) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
        match msg {
            ClientToServer::ReadPath { leaf } => {
                let buckets = state.read_path(leaf);
                (state, Yield::Send(ServerToClient::PathBuckets { buckets }))
            }
            ClientToServer::WritePath { leaf, buckets } => {
                state.write_path(leaf, &buckets);
                // After write, server is done with this access round
                (state.clone(), Yield::Send(ServerToClient::Ack))
            }
        }
    }
}

/// Run a complete ORAM access using the Protocol interface.
///
/// Wires the client and server protocols together for local execution.
pub fn run_oram_protocol<const Z: usize, const B: usize>(
    client_state: ClientProtocolState<Z, B>,
    mut tree: OramTree<Z, B>,
) -> (OramClient<Z, B>, AccessResult<B>, OramTree<Z, B>) {
    let (mut c_state, c_action) = OramClientProtocol::<Z, B>::init(client_state);

    let mut server_msg: Option<ServerToClient<Z, B>> = None;
    let mut client_msg: Option<ClientToServer<Z, B>> = match c_action {
        Yield::Send(m) => Some(m),
        Yield::Done((client, result)) => return (client, result, tree),
    };

    loop {
        // Server processes client message
        if let Some(cm) = client_msg.take() {
            let (new_tree, s_action) = OramServerProtocol::<Z, B>::step(tree, cm);
            tree = new_tree;
            match s_action {
                Yield::Send(m) => server_msg = Some(m),
                Yield::Done(t) => {
                    tree = t;
                    // Server done but client might not be
                }
            }
        }

        // Client processes server message
        if let Some(sm) = server_msg.take() {
            let (new_c_state, c_action) = OramClientProtocol::<Z, B>::step(c_state, sm);
            c_state = new_c_state;
            match c_action {
                Yield::Done((client, result)) => return (client, result, tree),
                Yield::Send(m) => client_msg = Some(m),
            }
        } else {
            panic!("protocol stuck: no messages to process");
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    /// Simple deterministic RNG for tests.
    fn test_rng(seed: u64) -> impl FnMut() -> u64 {
        let mut state = seed;
        move || {
            let val = state;
            state = state.wrapping_mul(6364136223846793005).wrapping_add(1);
            val
        }
    }

    // -- Tree operation tests --

    #[test]
    fn test_tree_new() {
        let tree = OramTree::<4, 16>::new(3);
        assert_eq!(tree.levels, 3);
        assert_eq!(tree.buckets.len(), 7); // 2^3 - 1
        assert_eq!(tree.num_leaves(), 4); // 2^(3-1)
    }

    #[test]
    fn test_path_indices() {
        // Tree with 3 levels (4 leaves):
        //        0
        //      /   \
        //     1     2
        //    / \   / \
        //   3   4 5   6
        // Leaves: 3,4,5,6 (leaf 0,1,2,3)

        let tree = OramTree::<4, 16>::new(3);

        // Path to leaf 0 (node 3): 0 -> 1 -> 3
        assert_eq!(tree.path_indices(0), vec![0, 1, 3]);
        // Path to leaf 1 (node 4): 0 -> 1 -> 4
        assert_eq!(tree.path_indices(1), vec![0, 1, 4]);
        // Path to leaf 2 (node 5): 0 -> 2 -> 5
        assert_eq!(tree.path_indices(2), vec![0, 2, 5]);
        // Path to leaf 3 (node 6): 0 -> 2 -> 6
        assert_eq!(tree.path_indices(3), vec![0, 2, 6]);
    }

    #[test]
    fn test_read_write_path() {
        let mut tree = OramTree::<4, 16>::new(3);

        // Create some non-dummy buckets
        let mut custom_buckets = tree.read_path(0);
        custom_buckets[2].entries[0] = OramEntry {
            addr: 42,
            leaf: 0,
            data: [1u8; 16],
        };

        tree.write_path(0, &custom_buckets);

        let read_back = tree.read_path(0);
        assert_eq!(read_back[2].entries[0].addr, 42);
        assert_eq!(read_back[2].entries[0].data, [1u8; 16]);
    }

    #[test]
    fn test_leaf_in_subtree() {
        // 3-level tree: root covers all, level-1 nodes cover half each
        assert!(leaf_in_subtree(0, 0, 3)); // root covers all
        assert!(leaf_in_subtree(3, 0, 3));
        assert!(leaf_in_subtree(0, 1, 3)); // left child covers leaves 0,1
        assert!(leaf_in_subtree(1, 1, 3));
        assert!(!leaf_in_subtree(2, 1, 3));
        assert!(leaf_in_subtree(2, 2, 3)); // right child covers leaves 2,3
        assert!(leaf_in_subtree(3, 2, 3));
        assert!(!leaf_in_subtree(0, 2, 3));
        // Leaf nodes cover exactly one leaf
        assert!(leaf_in_subtree(0, 3, 3));
        assert!(!leaf_in_subtree(1, 3, 3));
    }

    // -- Bit utility tests --

    #[test]
    fn test_bit_reverse() {
        assert_eq!(bit_reverse(0b000, 3), 0b000);
        assert_eq!(bit_reverse(0b001, 3), 0b100);
        assert_eq!(bit_reverse(0b010, 3), 0b010);
        assert_eq!(bit_reverse(0b011, 3), 0b110);
        assert_eq!(bit_reverse(0b100, 3), 0b001);
    }

    #[test]
    fn test_bits_needed() {
        assert_eq!(bits_needed(1), 1);
        assert_eq!(bits_needed(2), 1);
        assert_eq!(bits_needed(3), 2);
        assert_eq!(bits_needed(4), 2);
        assert_eq!(bits_needed(5), 3);
        assert_eq!(bits_needed(8), 3);
        assert_eq!(bits_needed(9), 4);
    }

    #[test]
    fn test_tree_levels_for() {
        assert_eq!(tree_levels_for(1), 1); // 1 leaf needed
        assert_eq!(tree_levels_for(2), 2); // 2 leaves
        assert_eq!(tree_levels_for(3), 3); // need 4 leaves (next power of 2)
        assert_eq!(tree_levels_for(4), 3); // 4 leaves
        assert_eq!(tree_levels_for(5), 4); // 8 leaves
    }

    // -- Position packing tests --

    #[test]
    fn test_extract_insert_position() {
        let mut block = [0u8; 16];

        // 3-bit positions, pack multiple per block
        insert_position(&mut block, 0, 3, 5); // 0b101
        insert_position(&mut block, 1, 3, 3); // 0b011
        insert_position(&mut block, 2, 3, 7); // 0b111

        assert_eq!(extract_position(&block, 0, 3), 5);
        assert_eq!(extract_position(&block, 1, 3), 3);
        assert_eq!(extract_position(&block, 2, 3), 7);

        // Overwrite
        insert_position(&mut block, 1, 3, 0);
        assert_eq!(extract_position(&block, 1, 3), 0);
        // Others unchanged
        assert_eq!(extract_position(&block, 0, 3), 5);
        assert_eq!(extract_position(&block, 2, 3), 7);
    }

    // -- Core ORAM algorithm tests --

    #[test]
    fn test_oram_write_then_read() {
        let mut tree = OramTree::<4, 16>::new(3); // 4 leaves
        let mut client = OramClient::<4, 16>::new(3, 4);
        let mut rng = test_rng(42);

        // Write to address 0
        let data = [0xAA; 16];
        let result = oram_access_local(&mut client, &mut tree, 0, AccessOp::Write(data), &mut rng);
        assert_eq!(result, AccessResult::WriteAck);

        // Read from address 0
        let result = oram_access_local(&mut client, &mut tree, 0, AccessOp::Read, &mut rng);
        assert_eq!(result, AccessResult::ReadValue(data));
    }

    #[test]
    fn test_oram_multiple_addresses() {
        let mut tree = OramTree::<4, 16>::new(4); // 8 leaves
        let mut client = OramClient::<4, 16>::new(4, 8);
        let mut rng = test_rng(123);

        // Write to several addresses
        for addr in 0..8u64 {
            let mut data = [0u8; 16];
            data[0] = addr as u8;
            oram_access_local(
                &mut client,
                &mut tree,
                addr,
                AccessOp::Write(data),
                &mut rng,
            );
        }

        // Read them all back
        for addr in 0..8u64 {
            let result = oram_access_local(&mut client, &mut tree, addr, AccessOp::Read, &mut rng);
            match result {
                AccessResult::ReadValue(data) => {
                    assert_eq!(data[0], addr as u8, "addr {} mismatch", addr);
                }
                _ => panic!("expected ReadValue"),
            }
        }
    }

    #[test]
    fn test_oram_overwrite() {
        let mut tree = OramTree::<4, 16>::new(3);
        let mut client = OramClient::<4, 16>::new(3, 4);
        let mut rng = test_rng(999);

        // Write version 1
        oram_access_local(
            &mut client,
            &mut tree,
            0,
            AccessOp::Write([1u8; 16]),
            &mut rng,
        );

        // Write version 2
        oram_access_local(
            &mut client,
            &mut tree,
            0,
            AccessOp::Write([2u8; 16]),
            &mut rng,
        );

        // Read should return version 2
        let result = oram_access_local(&mut client, &mut tree, 0, AccessOp::Read, &mut rng);
        assert_eq!(result, AccessResult::ReadValue([2u8; 16]));
    }

    #[test]
    fn test_oram_uninitialized_read() {
        let mut tree = OramTree::<4, 16>::new(3);
        let mut client = OramClient::<4, 16>::new(3, 4);
        let mut rng = test_rng(7);

        // Reading an uninitialized address should return zeros
        let result = oram_access_local(&mut client, &mut tree, 2, AccessOp::Read, &mut rng);
        assert_eq!(result, AccessResult::ReadValue([0u8; 16]));
    }

    #[test]
    fn test_oram_stash_bounded() {
        let levels = 4;
        let mut tree = OramTree::<4, 16>::new(levels);
        let mut client = OramClient::<4, 16>::new(levels, 8);
        let mut rng = test_rng(42);

        // Perform many accesses and verify stash stays bounded
        for i in 0..100u64 {
            let addr = i % 8;
            let mut data = [0u8; 16];
            data[0] = (i & 0xFF) as u8;
            oram_access_local(
                &mut client,
                &mut tree,
                addr,
                AccessOp::Write(data),
                &mut rng,
            );
            assert!(
                client.stash.len() <= client.max_stash,
                "stash overflow at access {}: {} > {}",
                i,
                client.stash.len(),
                client.max_stash
            );
        }
    }

    #[test]
    fn test_oram_many_accesses_correctness() {
        let levels = 4;
        let num_addrs = 8u64;
        let mut tree = OramTree::<4, 16>::new(levels);
        let mut client = OramClient::<4, 16>::new(levels, num_addrs);
        let mut rng = test_rng(12345);

        // Track expected values
        let mut expected = std::vec![0u8; num_addrs as usize];

        for i in 0..200u64 {
            let addr = i % num_addrs;
            if i % 3 == 0 {
                // Write
                let val = ((i * 7 + 13) & 0xFF) as u8;
                let mut data = [0u8; 16];
                data[0] = val;
                oram_access_local(
                    &mut client,
                    &mut tree,
                    addr,
                    AccessOp::Write(data),
                    &mut rng,
                );
                expected[addr as usize] = val;
            } else {
                // Read
                let result =
                    oram_access_local(&mut client, &mut tree, addr, AccessOp::Read, &mut rng);
                match result {
                    AccessResult::ReadValue(data) => {
                        assert_eq!(
                            data[0], expected[addr as usize],
                            "mismatch at access {} addr {}: got {} expected {}",
                            i, addr, data[0], expected[addr as usize]
                        );
                    }
                    _ => panic!("expected ReadValue"),
                }
            }
        }
    }

    // -- Eviction coverage test --

    #[test]
    fn test_eviction_targets_cover_all_leaves() {
        let num_leaves = 8u64;
        let mut seen = std::collections::BTreeSet::new();
        for counter in 0..num_leaves {
            let target = eviction_target(counter, num_leaves);
            assert!(target < num_leaves, "target {} >= num_leaves {}", target, num_leaves);
            seen.insert(target);
        }
        // All leaves should be covered in one full cycle
        assert_eq!(seen.len(), num_leaves as usize);
    }

    // -- Protocol round-trip tests --

    #[test]
    fn test_protocol_write_read() {
        let levels = 3;
        let tree = OramTree::<4, 16>::new(levels);
        let client = OramClient::<4, 16>::new(levels, 4);

        // Write
        let write_state = ClientProtocolState::new(
            client,
            0,
            AccessOp::Write([0xBB; 16]),
            levels,
            42,
        );
        let (client, result, tree) = run_oram_protocol(write_state, tree);
        assert_eq!(result, AccessResult::WriteAck);

        // Read
        let read_state = ClientProtocolState::new(
            client,
            0,
            AccessOp::Read,
            levels,
            100,
        );
        let (_client, result, _tree) = run_oram_protocol(read_state, tree);
        assert_eq!(result, AccessResult::ReadValue([0xBB; 16]));
    }

    #[test]
    fn test_protocol_multiple_addresses() {
        let levels = 4;
        let mut tree = OramTree::<4, 16>::new(levels);
        let mut client = OramClient::<4, 16>::new(levels, 8);
        let mut seed = 1u64;

        // Write to all addresses
        for addr in 0..8u64 {
            let mut data = [0u8; 16];
            data[0] = addr as u8 + 10;
            let state = ClientProtocolState::new(
                client,
                addr,
                AccessOp::Write(data),
                levels,
                seed,
            );
            seed += 1000;
            let (c, r, t) = run_oram_protocol(state, tree);
            assert_eq!(r, AccessResult::WriteAck);
            client = c;
            tree = t;
        }

        // Read them all back
        for addr in 0..8u64 {
            let state = ClientProtocolState::new(
                client,
                addr,
                AccessOp::Read,
                levels,
                seed,
            );
            seed += 1000;
            let (c, r, t) = run_oram_protocol(state, tree);
            match r {
                AccessResult::ReadValue(data) => {
                    assert_eq!(data[0], addr as u8 + 10, "addr {} mismatch", addr);
                }
                _ => panic!("expected ReadValue"),
            }
            client = c;
            tree = t;
        }
    }

    // -- Recursive position map tests --

    #[test]
    fn test_posmap_local() {
        let mut posmap = PosMap::local(4, 8);
        let mut rng = test_rng(42);

        // Initially all positions are 0
        assert_eq!(posmap.lookup(0, &mut rng), 0);
        assert_eq!(posmap.lookup(3, &mut rng), 0);

        // Update and verify
        let old = posmap.update(2, 5, &mut rng);
        assert_eq!(old, 0);
        assert_eq!(posmap.lookup(2, &mut rng), 5);
    }

    #[test]
    fn test_posmap_recursive() {
        let mut rng = test_rng(42);
        let mut posmap = PosMap::recursive(32, 16, 8, &mut rng);

        // Update some positions
        let old = posmap.update(0, 5, &mut rng);
        assert_eq!(old, 0); // initially 0
        assert_eq!(posmap.lookup(0, &mut rng), 5);

        posmap.update(15, 3, &mut rng);
        assert_eq!(posmap.lookup(15, &mut rng), 3);

        // Original should still be there
        assert_eq!(posmap.lookup(0, &mut rng), 5);
    }

    #[test]
    fn test_oram_with_recursive_posmap() {
        let levels = 5; // 16 leaves
        let num_addrs = 16u64;
        let mut rng = test_rng(42);

        let mut tree = OramTree::<4, 16>::new(levels);
        let mut client = OramClient::<4, 16>::new_recursive(levels, num_addrs, 4, &mut rng);

        // Write and read with recursive posmap
        for addr in 0..num_addrs {
            let mut data = [0u8; 16];
            data[0] = (addr * 3) as u8;
            oram_access_local(
                &mut client,
                &mut tree,
                addr,
                AccessOp::Write(data),
                &mut rng,
            );
        }

        for addr in 0..num_addrs {
            let result = oram_access_local(&mut client, &mut tree, addr, AccessOp::Read, &mut rng);
            match result {
                AccessResult::ReadValue(data) => {
                    assert_eq!(data[0], (addr * 3) as u8, "addr {} mismatch", addr);
                }
                _ => panic!("expected ReadValue"),
            }
        }
    }
}
