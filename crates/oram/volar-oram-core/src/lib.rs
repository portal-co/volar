// @reliability: experimental
// @ai: assisted
//
//! Server-side Path ORAM operations for Volar circuit compilation.
//!
//! This crate contains the ORAM tree operations (read path, write path)
//! in "total Rust" — the subset of Rust that `volar-compiler` can parse
//! and lower to circuits. The client-side logic (stash, position map,
//! eviction) remains in `volar-oram`.
//!
//! All types use fixed-size arrays (no `Vec`, no dynamic allocation).
//! Loops are bounded. No `while`/`loop`/`assert!`/`panic!`.
//!
//! # Const generics
//!
//! - `Z`: bucket size (entries per bucket, typically 4)
//! - `B`: block data size in bytes (typically 16)
//! - `L`: number of tree levels (depth). The tree has `2^(L-1)` leaves.
//! - `N`: total number of tree nodes. Must equal `2^L - 1`.
//!
//! The weaver monomorphizes these to concrete values when compiling
//! ORAM circuits.

#![no_std]

// ---------------------------------------------------------------------------
// Core types
// ---------------------------------------------------------------------------

/// A single ORAM entry: block data plus metadata.
///
/// An entry with `addr == !0u64` (u64::MAX) is a dummy (empty) slot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OramEntry<const B: usize> {
    /// Logical address. `!0u64` indicates an empty slot.
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
            addr: !0u64,
            leaf: 0,
            data: [0u8; B],
        }
    }

    /// Returns true if this is a real (non-dummy) entry.
    pub fn is_real(&self) -> bool {
        self.addr != !0u64
    }
}

/// A bucket holding Z entries. Each node in the ORAM tree is one bucket.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bucket<const Z: usize, const B: usize> {
    pub entries: [OramEntry<B>; Z],
}

impl<const Z: usize, const B: usize> Bucket<Z, B> {
    /// Create an empty bucket (all dummy entries).
    pub fn empty() -> Self {
        Self {
            entries: [OramEntry::dummy(); Z],
        }
    }
}

// ---------------------------------------------------------------------------
// Protocol messages (server side)
// ---------------------------------------------------------------------------

/// Request from the ORAM client to the server.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ServerRequest<const Z: usize, const B: usize, const L: usize> {
    /// Read all buckets on the path from root to this leaf.
    ReadPath { leaf: u64 },
    /// Write updated buckets back to the path.
    WritePath {
        leaf: u64,
        buckets: [Bucket<Z, B>; L],
    },
}

/// Response from the server to the ORAM client.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ServerResponse<const Z: usize, const B: usize, const L: usize> {
    /// The buckets along the requested path.
    PathBuckets { buckets: [Bucket<Z, B>; L] },
    /// Acknowledgement that the write completed.
    Ack,
}

// ---------------------------------------------------------------------------
// Path computation
// ---------------------------------------------------------------------------

/// Compute heap indices for the path from root to a given leaf
/// in a binary tree with `L` levels.
///
/// The tree uses heap indexing: root = 0, children of node `i` are
/// `2*i + 1` (left) and `2*i + 2` (right). Leaf `l` is at heap index
/// `2^(L-1) - 1 + l`.
///
/// Returns an array of `L` indices, from root (index 0) down to the leaf.
///
/// # Requires
///
/// `L >= 1` and `leaf < 2^(L-1)`.
pub fn path_indices<const L: usize>(leaf: u64) -> [usize; L] {
    let mut path = [0usize; L];
    // Level 0 is always the root (index 0).
    // For each subsequent level, go left or right based on the
    // corresponding bit of the leaf index. Bit (L-1-level) of the
    // leaf determines the direction at that level.
    let mut idx = 0usize;
    for level in 1..L {
        let bit_pos = L - 1 - level;
        if (leaf >> bit_pos) & 1 == 0 {
            idx = 2 * idx + 1; // left child
        } else {
            idx = 2 * idx + 2; // right child
        }
        path[level] = idx;
    }
    path
}

/// Number of leaves in a tree with `L` levels.
pub fn num_leaves<const L: usize>() -> u64 {
    1u64 << (L - 1)
}

/// Total number of nodes in a tree with `L` levels.
///
/// This is the required value of `N` for tree arrays.
pub fn num_nodes<const L: usize>() -> usize {
    (1usize << L) - 1
}

// ---------------------------------------------------------------------------
// Tree operations (server side)
// ---------------------------------------------------------------------------

/// Read all buckets along the path from root to the given leaf.
///
/// `tree_buckets` is the flat heap-indexed array of all tree nodes.
/// Returns `L` buckets (one per level, root first).
pub fn read_path<const Z: usize, const B: usize, const L: usize, const N: usize>(
    tree_buckets: &[Bucket<Z, B>; N],
    leaf: u64,
) -> [Bucket<Z, B>; L] {
    let path = path_indices::<L>(leaf);
    let mut result = [Bucket::empty(); L];
    for i in 0..L {
        result[i] = tree_buckets[path[i]];
    }
    result
}

/// Write buckets back along the path from root to the given leaf.
///
/// `tree_buckets` is the flat heap-indexed array of all tree nodes.
/// `new_buckets` has `L` buckets (one per level, root first).
pub fn write_path<const Z: usize, const B: usize, const L: usize, const N: usize>(
    tree_buckets: &mut [Bucket<Z, B>; N],
    leaf: u64,
    new_buckets: &[Bucket<Z, B>; L],
) {
    let path = path_indices::<L>(leaf);
    for i in 0..L {
        tree_buckets[path[i]] = new_buckets[i];
    }
}

// ---------------------------------------------------------------------------
// Server protocol step
// ---------------------------------------------------------------------------

/// Process a single server request against the ORAM tree.
///
/// This is the core function that gets compiled into circuit code.
/// The tree is a flat array of `N` buckets in heap order.
pub fn server_step<const Z: usize, const B: usize, const L: usize, const N: usize>(
    tree_buckets: &mut [Bucket<Z, B>; N],
    request: ServerRequest<Z, B, L>,
) -> ServerResponse<Z, B, L> {
    match request {
        ServerRequest::ReadPath { leaf } => {
            let buckets = read_path::<Z, B, L, N>(tree_buckets, leaf);
            ServerResponse::PathBuckets { buckets }
        }
        ServerRequest::WritePath { leaf, buckets } => {
            write_path::<Z, B, L, N>(tree_buckets, leaf, &buckets);
            ServerResponse::Ack
        }
    }
}

// ---------------------------------------------------------------------------
// Subtree helpers (used by client for eviction decisions)
// ---------------------------------------------------------------------------

/// Check whether a leaf is in the subtree rooted at the node at heap
/// index `node_idx` in a tree with `L` levels.
///
/// At depth `d` (root = 0), a node covers `2^(L-1-d)` consecutive
/// leaves. This function checks whether `leaf` falls in that range.
pub fn leaf_in_subtree<const L: usize>(leaf: u64, node_idx: usize) -> bool {
    // Depth of node_idx in the heap: floor(log2(node_idx + 1))
    let depth = log2_floor(node_idx + 1);
    let subtree_levels = L - 1 - depth;
    // Position within level: node_idx - (2^depth - 1)
    let pos_in_level = node_idx - ((1usize << depth) - 1);
    let left_leaf = (pos_in_level as u64) << subtree_levels;
    let right_leaf = left_leaf + (1u64 << subtree_levels) - 1;
    leaf >= left_leaf && leaf <= right_leaf
}

/// Floor of log2 for positive integers, computed with a bounded loop.
///
/// Returns 0 for inputs <= 1. For x > 0, returns the position of the
/// highest set bit (0-indexed).
fn log2_floor(x: usize) -> usize {
    // Bounded loop: usize is at most 64 bits
    let mut result = 0usize;
    let mut val = x >> 1;
    for _ in 0..63 {
        if val > 0 {
            result = result + 1;
            val = val >> 1;
        }
    }
    result
}

/// Reverse the low `bits` bits of `x`.
///
/// Used for deterministic eviction target computation.
pub fn bit_reverse(x: u64, bits: usize) -> u64 {
    let mut result = 0u64;
    let mut val = x;
    for _ in 0..bits {
        result = (result << 1) | (val & 1);
        val = val >> 1;
    }
    result
}

/// Number of bits needed to represent values in `[0, n)`.
pub fn bits_needed(n: u64) -> usize {
    if n <= 1 {
        return 1;
    }
    // Count leading zeros to find the highest set bit
    // For n > 1: bits_needed = 64 - leading_zeros(n - 1)
    let v = n - 1;
    let mut bits = 0usize;
    let mut tmp = v;
    for _ in 0..64 {
        if tmp > 0 {
            bits = bits + 1;
            tmp = tmp >> 1;
        }
    }
    bits
}

/// Compute the deterministic eviction target leaf.
///
/// Uses bit-reversal of the access counter modulo the number of leaves.
/// This ensures uniform coverage over time.
pub fn eviction_target(access_counter: u64, num_leaves_val: u64) -> u64 {
    if num_leaves_val <= 1 {
        return 0;
    }
    let bits = bits_needed(num_leaves_val);
    let idx = access_counter % num_leaves_val;
    bit_reverse(idx, bits) % num_leaves_val
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dummy_entry() {
        let e = OramEntry::<16>::dummy();
        assert!(!e.is_real());
        assert_eq!(e.addr, u64::MAX);
        assert_eq!(e.data, [0u8; 16]);
    }

    #[test]
    fn test_empty_bucket() {
        let b = Bucket::<4, 16>::empty();
        for entry in &b.entries {
            assert!(!entry.is_real());
        }
    }

    #[test]
    fn test_path_indices_3_levels() {
        // Tree with 3 levels (4 leaves):
        //        0
        //      /   \
        //     1     2
        //    / \   / \
        //   3   4 5   6
        // Leaves: 0→3, 1→4, 2→5, 3→6

        assert_eq!(path_indices::<3>(0), [0, 1, 3]);
        assert_eq!(path_indices::<3>(1), [0, 1, 4]);
        assert_eq!(path_indices::<3>(2), [0, 2, 5]);
        assert_eq!(path_indices::<3>(3), [0, 2, 6]);
    }

    #[test]
    fn test_path_indices_4_levels() {
        // 4 levels, 8 leaves (indices 7..14)
        // Leaf 0: root → left → left → left = 0, 1, 3, 7
        assert_eq!(path_indices::<4>(0), [0, 1, 3, 7]);
        // Leaf 7: root → right → right → right = 0, 2, 6, 14
        assert_eq!(path_indices::<4>(7), [0, 2, 6, 14]);
    }

    #[test]
    fn test_num_leaves() {
        assert_eq!(num_leaves::<1>(), 1);
        assert_eq!(num_leaves::<2>(), 2);
        assert_eq!(num_leaves::<3>(), 4);
        assert_eq!(num_leaves::<4>(), 8);
    }

    #[test]
    fn test_num_nodes() {
        assert_eq!(num_nodes::<1>(), 1);
        assert_eq!(num_nodes::<2>(), 3);
        assert_eq!(num_nodes::<3>(), 7);
        assert_eq!(num_nodes::<4>(), 15);
    }

    #[test]
    fn test_read_write_path() {
        // 3-level tree: 7 nodes, 4 leaves
        let mut tree = [Bucket::<4, 16>::empty(); 7];

        // Write some data via write_path
        let mut buckets_to_write = [Bucket::<4, 16>::empty(); 3];
        buckets_to_write[2].entries[0] = OramEntry {
            addr: 42,
            leaf: 0,
            data: [0xAA; 16],
        };
        write_path::<4, 16, 3, 7>(&mut tree, 0, &buckets_to_write);

        // Read it back
        let read_back = read_path::<4, 16, 3, 7>(&tree, 0);
        assert_eq!(read_back[2].entries[0].addr, 42);
        assert_eq!(read_back[2].entries[0].data, [0xAA; 16]);

        // Other path should still be empty
        let other = read_path::<4, 16, 3, 7>(&tree, 3);
        assert!(!other[2].entries[0].is_real());
    }

    #[test]
    fn test_server_step_read() {
        let mut tree = [Bucket::<4, 16>::empty(); 7];

        // Plant a real entry at node 3 (leaf 0's deepest bucket)
        tree[3].entries[0] = OramEntry {
            addr: 10,
            leaf: 0,
            data: [0xBB; 16],
        };

        let response = server_step::<4, 16, 3, 7>(
            &mut tree,
            ServerRequest::ReadPath { leaf: 0 },
        );
        match response {
            ServerResponse::PathBuckets { buckets } => {
                // Path to leaf 0: [0, 1, 3]
                assert_eq!(buckets[2].entries[0].addr, 10);
                assert_eq!(buckets[2].entries[0].data, [0xBB; 16]);
            }
            ServerResponse::Ack => panic!("expected PathBuckets"),
        }
    }

    #[test]
    fn test_server_step_write() {
        let mut tree = [Bucket::<4, 16>::empty(); 7];

        let mut new_buckets = [Bucket::<4, 16>::empty(); 3];
        new_buckets[1].entries[0] = OramEntry {
            addr: 5,
            leaf: 1,
            data: [0xCC; 16],
        };

        let response = server_step::<4, 16, 3, 7>(
            &mut tree,
            ServerRequest::WritePath {
                leaf: 0,
                buckets: new_buckets,
            },
        );
        assert_eq!(response, ServerResponse::Ack);

        // Verify the tree was updated (node 1 is path[1] for leaf 0)
        assert_eq!(tree[1].entries[0].addr, 5);
        assert_eq!(tree[1].entries[0].data, [0xCC; 16]);
    }

    #[test]
    fn test_leaf_in_subtree() {
        // 3-level tree
        assert!(leaf_in_subtree::<3>(0, 0)); // root covers all
        assert!(leaf_in_subtree::<3>(3, 0));
        assert!(leaf_in_subtree::<3>(0, 1)); // left child covers 0,1
        assert!(leaf_in_subtree::<3>(1, 1));
        assert!(!leaf_in_subtree::<3>(2, 1));
        assert!(leaf_in_subtree::<3>(2, 2)); // right child covers 2,3
        assert!(leaf_in_subtree::<3>(3, 2));
        assert!(!leaf_in_subtree::<3>(0, 2));
        // Leaf nodes cover exactly one leaf
        assert!(leaf_in_subtree::<3>(0, 3));
        assert!(!leaf_in_subtree::<3>(1, 3));
    }

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
        assert_eq!(bits_needed(8), 3);
        assert_eq!(bits_needed(9), 4);
    }

    #[test]
    fn test_eviction_targets_cover_all_leaves() {
        let num_leaves_val = 8u64;
        let mut seen = [false; 8];
        for counter in 0..8u64 {
            let target = eviction_target(counter, num_leaves_val);
            assert!(target < num_leaves_val);
            seen[target as usize] = true;
        }
        for i in 0..8 {
            assert!(seen[i], "leaf {} not covered", i);
        }
    }

    #[test]
    fn test_log2_floor() {
        assert_eq!(log2_floor(1), 0);
        assert_eq!(log2_floor(2), 1);
        assert_eq!(log2_floor(3), 1);
        assert_eq!(log2_floor(4), 2);
        assert_eq!(log2_floor(7), 2);
        assert_eq!(log2_floor(8), 3);
        assert_eq!(log2_floor(16), 4);
    }
}
