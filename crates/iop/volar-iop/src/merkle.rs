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

use core::marker::PhantomData;

use sha3::{
    Sha3_256,
    digest::{Digest, typenum::U32},
};

const LEAF_PREFIX: u8 = 0x00;
const NODE_PREFIX: u8 = 0x01;

/// A 32-byte digest (SHA3-256 by default; any 32-byte-output `Digest` —
/// Blake2s-256, Ascon-Hash — is a drop-in, see
/// `docs/hash-cipher-generic-plan.md`).
pub type Digest32 = [u8; 32];

/// The Merkle hash backend: any fixed-output 32-byte `Digest` — SHA3-256
/// (default), Blake2s-256, or Ascon-Hash. (A named bound, not a new trait —
/// the `Digest` trait is the interface.)
fn hash_leaf<D: Digest<OutputSize = U32>>(data: &[u8]) -> Digest32 {
    let mut h = D::new();
    h.update([LEAF_PREFIX]);
    h.update(data);
    h.finalize().into()
}

fn hash_node<D: Digest<OutputSize = U32>>(left: &Digest32, right: &Digest32) -> Digest32 {
    let mut h = D::new();
    h.update([NODE_PREFIX]);
    h.update(left);
    h.update(right);
    h.finalize().into()
}

/// A committed Merkle tree: the full level-by-level layer structure (kept —
/// this design never has more than a few hundred leaves, § module doc), so
/// [`MerkleTree::open`] is a cheap array lookup, not a recomputation.
#[derive(Clone, Debug)]
pub struct MerkleTreeImpl<D: Digest<OutputSize = U32>> {
    /// `layers[0]` = leaf hashes; `layers.last()` = `[root]`.
    layers: Vec<Vec<Digest32>>,
    /// The hash backend is phantom — the tree stores only the computed hashes.
    _digest: PhantomData<fn() -> D>,
}

/// The default Merkle tree: SHA3-256 (today's behavior). Other backends are
/// selected by naming `MerkleTreeImpl<Blake2s256>` / `MerkleTreeImpl<AsconHash>`
/// directly.
pub type MerkleTree = MerkleTreeImpl<Sha3_256>;

/// An authentication path: sibling hashes from the leaf up to (excluding)
/// the root, in bottom-up order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AuthPath {
    pub siblings: Vec<Digest32>,
}

impl<D: Digest<OutputSize = U32>> MerkleTreeImpl<D> {
    /// Build a tree over `leaves` (byte-vectors), padding to the next power
    /// of two by duplicating the last leaf (a standard, simple padding
    /// convention — documented so it can be reviewed/changed if a different
    /// one is preferred).
    pub fn commit(leaves: &[Vec<u8>]) -> Self {
        assert!(
            !leaves.is_empty(),
            "MerkleTree::commit: at least one leaf required"
        );
        let mut level: Vec<Digest32> = leaves.iter().map(|l| hash_leaf::<D>(l)).collect();
        let padded_len = level.len().next_power_of_two();
        while level.len() < padded_len {
            level.push(*level.last().unwrap());
        }
        let mut layers = vec![level.clone()];
        while level.len() > 1 {
            let mut next = Vec::with_capacity(level.len() / 2);
            for pair in level.chunks(2) {
                next.push(hash_node::<D>(&pair[0], &pair[1]));
            }
            layers.push(next.clone());
            level = next;
        }
        MerkleTreeImpl {
            layers,
            _digest: PhantomData,
        }
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
    verify_with::<Sha3_256>(root, leaf, index, path)
}

/// [`verify`] with an explicit hash backend `D` (which must match the one the
/// tree was committed with — it is not recorded in the path). This is the
/// generic entry point; [`verify`] is the SHA3-256 default.
pub fn verify_with<D: Digest<OutputSize = U32>>(
    root: &Digest32,
    leaf: &[u8],
    index: usize,
    path: &AuthPath,
) -> bool {
    let mut cur = hash_leaf::<D>(leaf);
    let mut idx = index;
    for sib in &path.siblings {
        cur = if idx % 2 == 0 {
            hash_node::<D>(&cur, sib)
        } else {
            hash_node::<D>(sib, &cur)
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
        assert_eq!(tree.root(), hash_leaf::<Sha3_256>(&ls[0]));
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
        let l0 = hash_leaf::<Sha3_256>(&[1, 2, 3]);
        let l1 = hash_leaf::<Sha3_256>(&[4, 5, 6]);
        let node = hash_node::<Sha3_256>(&l0, &l1);
        // Domain-separation means re-hashing the concatenation as a "leaf"
        // of the raw bytes gives a different result than hashing as a node.
        let mut raw = alloc::vec::Vec::new();
        raw.extend_from_slice(&l0);
        raw.extend_from_slice(&l1);
        assert_ne!(hash_leaf::<Sha3_256>(&raw), node);
    }
}
