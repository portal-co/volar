// @reliability: experimental
// @ai: assisted
//! Generic binary Merkle tree over byte-vector leaves, hashed with SHA3-256
//! — the same hash primitive already used elsewhere in this repo for
//! commitment purposes (`crates/fold/volar-fold/src/keccak_r1cs.rs`,
//! `docs/vcb-ivc-folding.md` §4's Keccak boundary link). Leaf and internal
//! nodes are domain-separated (distinct one-byte prefixes) so a leaf hash
//! can never be confused with an internal-node hash — standard practice,
//! prevents a class of second-preimage attacks against the tree.
//!
//! Only ever used here on small, fixed-size vectors (the Phase 2
//! finalization IOP's codeword, `crate::ligero`) — no streaming/incremental
//! construction is needed anywhere in this design (see
//! `docs/prove-the-verifier-iop.md`).

use alloc::vec;
use alloc::vec::Vec;

use sha3::{
    Sha3_256,
    digest::{Digest, FixedOutput},
};

const LEAF_PREFIX: u8 = 0x00;
const NODE_PREFIX: u8 = 0x01;

/// A 32-byte SHA3-256 digest.
pub type Digest32 = [u8; 32];

fn hash_leaf(data: &[u8]) -> Digest32 {
    let mut h = Sha3_256::new();
    h.update([LEAF_PREFIX]);
    h.update(data);
    h.finalize_fixed().into()
}

fn hash_node(left: &Digest32, right: &Digest32) -> Digest32 {
    let mut h = Sha3_256::new();
    h.update([NODE_PREFIX]);
    h.update(left);
    h.update(right);
    h.finalize_fixed().into()
}

/// A committed Merkle tree: the full level-by-level layer structure (kept —
/// this design never has more than a few hundred leaves, § module doc), so
/// [`MerkleTree::open`] is a cheap array lookup, not a recomputation.
#[derive(Clone, Debug)]
pub struct MerkleTree {
    /// `layers[0]` = leaf hashes; `layers.last()` = `[root]`.
    layers: Vec<Vec<Digest32>>,
}

/// An authentication path: sibling hashes from the leaf up to (excluding)
/// the root, in bottom-up order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AuthPath {
    pub siblings: Vec<Digest32>,
}

impl MerkleTree {
    /// Build a tree over `leaves` (byte-vectors), padding to the next power
    /// of two by duplicating the last leaf (a standard, simple padding
    /// convention — documented so it can be reviewed/changed if a different
    /// one is preferred).
    pub fn commit(leaves: &[Vec<u8>]) -> Self {
        assert!(
            !leaves.is_empty(),
            "MerkleTree::commit: at least one leaf required"
        );
        let mut level: Vec<Digest32> = leaves.iter().map(|l| hash_leaf(l)).collect();
        let padded_len = level.len().next_power_of_two();
        while level.len() < padded_len {
            level.push(*level.last().unwrap());
        }
        let mut layers = vec![level.clone()];
        while level.len() > 1 {
            let mut next = Vec::with_capacity(level.len() / 2);
            for pair in level.chunks(2) {
                next.push(hash_node(&pair[0], &pair[1]));
            }
            layers.push(next.clone());
            level = next;
        }
        MerkleTree { layers }
    }

    pub fn root(&self) -> Digest32 {
        self.layers.last().unwrap()[0]
    }

    pub fn num_leaves(&self) -> usize {
        self.layers[0].len()
    }

    /// Build the authentication path for leaf `index`.
    pub fn open(&self, index: usize) -> AuthPath {
        assert!(
            index < self.num_leaves(),
            "MerkleTree::open: index out of range"
        );
        let mut siblings = Vec::new();
        let mut idx = index;
        for layer in &self.layers[..self.layers.len() - 1] {
            let sib_idx = idx ^ 1;
            siblings.push(layer[sib_idx]);
            idx /= 2;
        }
        AuthPath { siblings }
    }
}

/// Verify that `leaf` (raw bytes, not yet hashed) is committed at `index`
/// under `root`, given `path`.
pub fn verify(root: &Digest32, leaf: &[u8], index: usize, path: &AuthPath) -> bool {
    let mut cur = hash_leaf(leaf);
    let mut idx = index;
    for sib in &path.siblings {
        cur = if idx % 2 == 0 {
            hash_node(&cur, sib)
        } else {
            hash_node(sib, &cur)
        };
        idx /= 2;
    }
    cur == *root
}

#[cfg(test)]
mod tests {
    use super::*;

    fn leaves(n: usize) -> Vec<Vec<u8>> {
        (0..n)
            .map(|i| alloc::vec![i as u8, (i * 3) as u8])
            .collect()
    }

    #[test]
    fn commit_open_verify_round_trips_power_of_two() {
        let ls = leaves(8);
        let tree = MerkleTree::commit(&ls);
        for (i, l) in ls.iter().enumerate() {
            let path = tree.open(i);
            assert!(verify(&tree.root(), l, i, &path), "leaf {i} must verify");
        }
    }

    #[test]
    fn commit_open_verify_round_trips_non_power_of_two() {
        let ls = leaves(5);
        let tree = MerkleTree::commit(&ls);
        for (i, l) in ls.iter().enumerate() {
            let path = tree.open(i);
            assert!(verify(&tree.root(), l, i, &path));
        }
    }

    #[test]
    fn single_leaf_tree_works() {
        let ls = leaves(1);
        let tree = MerkleTree::commit(&ls);
        assert_eq!(tree.root(), hash_leaf(&ls[0]));
        assert!(verify(&tree.root(), &ls[0], 0, &tree.open(0)));
    }

    #[test]
    fn tampered_leaf_fails_verification() {
        let ls = leaves(8);
        let tree = MerkleTree::commit(&ls);
        let path = tree.open(3);
        let tampered = alloc::vec![9u8, 9u8];
        assert!(!verify(&tree.root(), &tampered, 3, &path));
    }

    #[test]
    fn tampered_path_fails_verification() {
        let ls = leaves(8);
        let tree = MerkleTree::commit(&ls);
        let mut path = tree.open(3);
        path.siblings[0][0] ^= 0xff;
        assert!(!verify(&tree.root(), &ls[3], 3, &path));
    }

    #[test]
    fn wrong_index_fails_verification() {
        let ls = leaves(8);
        let tree = MerkleTree::commit(&ls);
        let path = tree.open(3);
        assert!(!verify(&tree.root(), &ls[3], 4, &path));
    }

    #[test]
    fn leaf_and_node_hashes_are_domain_separated() {
        // A two-byte leaf that happens to look like a hash_node's input
        // shape must not collide with an actual internal node hash.
        let l0 = hash_leaf(&[1, 2, 3]);
        let l1 = hash_leaf(&[4, 5, 6]);
        let node = hash_node(&l0, &l1);
        // Domain-separation means re-hashing the concatenation as a "leaf"
        // of the raw bytes gives a different result than hashing as a node.
        let mut raw = alloc::vec::Vec::new();
        raw.extend_from_slice(&l0);
        raw.extend_from_slice(&l1);
        assert_ne!(hash_leaf(&raw), node);
    }
}
