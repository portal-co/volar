//! Evaluator-owned ciphertext-tree half of an atomic split-key ORAM access.
//!
//! This module owns only physical ciphertext bytes, public per-node versions,
//! and public operation/formatting state. It contains neither AES key half nor
//! garbling material. The formatter and access circuits run in the paired
//! role-local drivers; this module accepts only their public ciphertext output.

use alloc::vec;
use alloc::vec::Vec;

use volar_mpc::MpcError;
use volar_oram::{Bucket, OramEntry, OramTree};

use crate::oram_gadget::OramGadgetConfig;

/// Evaluator-owned physical ciphertext tree.
pub struct CiphertextTree<const Z: usize> {
    tree: OramTree<Z, 1>,
    cfg: OramGadgetConfig,
    epoch: u64,
    /// Public per-node versions, heap-indexed. They enter versioned pad
    /// circuits as public wires and advance only after a bound path commit.
    versions: Vec<u64>,
    /// Formatter completion for `encrypt_valid`; avoids treating an all-zero
    /// raw tree as ciphertext dummies before the split formatter has run.
    formatted_nodes: Vec<bool>,
}

/// Single-use evaluator-local authority to commit one exact path read.
pub struct OpenedCiphertextPath {
    leaf: u64,
    epoch: u64,
    versions: Vec<u64>,
    bits: Vec<bool>,
}

impl<const Z: usize> CiphertextTree<Z> {
    /// Create evaluator-only ciphertext state. `encrypt_valid` and versioned
    /// pads are supported, but encrypted-valid paths cannot open until the
    /// split formatter installs every node.
    pub fn new(cfg: &OramGadgetConfig) -> Result<Self, MpcError> {
        if !cfg.encrypted || cfg.bucket_size != Z || cfg.tree_block_bytes() > 1 {
            return Err(MpcError::BadPartition);
        }
        let tree = OramTree::new(cfg.levels);
        Ok(Self {
            versions: vec![0; tree.buckets.len()],
            formatted_nodes: vec![!cfg.encrypt_valid; tree.buckets.len()],
            tree,
            cfg: cfg.clone(),
            epoch: 0,
        })
    }

    /// Public operation ordinal paired with role-local key-driver epochs.
    pub fn epoch(&self) -> u64 {
        self.epoch
    }

    /// Public ciphertext width for a complete physical path.
    pub fn path_width(&self) -> usize {
        self.cfg.path_entries() * self.cfg.entry_bits()
    }

    /// Install one output of the split tree-node formatter circuit. The caller
    /// supplies the public depth/prefix named by that formatter invocation;
    /// there is no AES key or plaintext value in this interface.
    pub fn install_formatted_node(
        &mut self,
        depth: usize,
        prefix: usize,
        ciphertext: &[bool],
    ) -> Result<(), MpcError> {
        let width = self.cfg.bucket_size * self.cfg.entry_bits();
        if !self.cfg.encrypt_valid
            || depth >= self.cfg.levels
            || prefix >= (1usize << depth)
            || ciphertext.len() != width
        {
            return Err(MpcError::BadPartition);
        }
        let index = (1usize << depth) - 1 + prefix;
        let entries = core::array::from_fn(|slot| {
            let mut data = [0u8; 1];
            for bit in 0..self.cfg.entry_bits() {
                if ciphertext[slot * self.cfg.entry_bits() + bit] {
                    data[bit / 8] |= 1 << (bit % 8);
                }
            }
            OramEntry {
                addr: 0,
                leaf: 0,
                data,
            }
        });
        // Keep the assignment explicit rather than relying on a tree-layout
        // helper: `index` is the public heap position named by the formatter.
        self.tree.buckets[index] = Bucket { entries };
        self.formatted_nodes[index] = true;
        Ok(())
    }

    /// Whether all public tree nodes have outputs from the split formatter.
    pub fn is_formatted(&self) -> bool {
        self.formatted_nodes.iter().all(|&done| done)
    }

    /// Public versions for `leaf`, in the root-to-leaf input order expected by
    /// `build_access` when `versioned_pads` is enabled.
    pub fn path_versions(&self, leaf: u64) -> Result<Vec<u64>, MpcError> {
        if leaf >= self.cfg.num_leaves() as u64 {
            return Err(MpcError::BadPartition);
        }
        Ok(self
            .tree
            .path_indices(leaf)
            .iter()
            .map(|&index| self.versions[index])
            .collect())
    }

    /// Open one fixed-width ciphertext path. Before encrypted-valid formatter
    /// completion this fails closed rather than interpreting raw zeros as a
    /// formatted ciphertext tree.
    pub fn open(&self, leaf: u64) -> Result<OpenedCiphertextPath, MpcError> {
        if leaf >= self.cfg.num_leaves() as u64 || !self.is_formatted() {
            return Err(MpcError::MalformedSchedule);
        }
        let indices = self.tree.path_indices(leaf);
        Ok(OpenedCiphertextPath {
            leaf,
            epoch: self.epoch,
            versions: indices.iter().map(|&index| self.versions[index]).collect(),
            bits: flatten_path(&self.tree.read_path(leaf), &self.cfg),
        })
    }

    /// Atomically write the ciphertext response for this exact opening. A
    /// stale epoch, changed version, wrong width, or reused opening aborts
    /// before tree mutation. Versioned paths advance once per node only after
    /// the write-back is accepted.
    pub fn commit(
        &mut self,
        opening: OpenedCiphertextPath,
        writeback: &[bool],
    ) -> Result<(), MpcError> {
        let indices = self.tree.path_indices(opening.leaf);
        if opening.epoch != self.epoch
            || writeback.len() != self.path_width()
            || opening.versions.len() != indices.len()
            || !indices
                .iter()
                .zip(&opening.versions)
                .all(|(&index, &version)| self.versions[index] == version)
        {
            return Err(MpcError::MalformedSchedule);
        }
        self.tree
            .write_path(opening.leaf, &unflatten_path::<Z>(writeback, &self.cfg));
        if self.cfg.versioned_pads {
            for index in indices {
                self.versions[index] = self.versions[index]
                    .checked_add(1)
                    .ok_or(MpcError::MalformedSchedule)?;
            }
        }
        self.epoch = self
            .epoch
            .checked_add(1)
            .ok_or(MpcError::MalformedSchedule)?;
        Ok(())
    }

    /// Start an access bound to the caller-supplied public protocol epoch.
    pub fn prepare(
        &mut self,
        epoch: u64,
        leaf: u64,
    ) -> Result<PreparedCiphertextAccess<'_, Z>, MpcError> {
        if epoch != self.epoch {
            return Err(MpcError::MalformedSchedule);
        }
        let opening = self.open(leaf)?;
        Ok(PreparedCiphertextAccess {
            tree: self,
            opening: Some(opening),
            epoch,
        })
    }
}

/// Evaluator-side prepared access. It consumes the path capability on commit,
/// preventing any second write from the same formatter/access response.
pub struct PreparedCiphertextAccess<'a, const Z: usize> {
    tree: &'a mut CiphertextTree<Z>,
    opening: Option<OpenedCiphertextPath>,
    epoch: u64,
}

impl<'a, const Z: usize> PreparedCiphertextAccess<'a, Z> {
    pub fn path_bits(&self) -> &[bool] {
        self.opening.as_ref().expect("prepared path").bits()
    }

    pub fn leaf(&self) -> u64 {
        self.opening.as_ref().expect("prepared path").leaf()
    }

    pub fn epoch(&self) -> u64 {
        self.epoch
    }

    pub fn path_versions(&self) -> &[u64] {
        self.opening.as_ref().expect("prepared path").versions()
    }

    pub fn commit(mut self, epoch: u64, writeback: &[bool]) -> Result<(), MpcError> {
        if epoch != self.epoch {
            return Err(MpcError::MalformedSchedule);
        }
        self.tree
            .commit(self.opening.take().expect("prepared path"), writeback)
    }
}

impl OpenedCiphertextPath {
    pub fn bits(&self) -> &[bool] {
        &self.bits
    }
    pub fn leaf(&self) -> u64 {
        self.leaf
    }
    pub fn epoch(&self) -> u64 {
        self.epoch
    }
    pub fn versions(&self) -> &[u64] {
        &self.versions
    }
}

fn flatten_path<const Z: usize>(path: &[Bucket<Z, 1>], cfg: &OramGadgetConfig) -> Vec<bool> {
    let mut bits = Vec::with_capacity(cfg.path_entries() * cfg.entry_bits());
    for bucket in path {
        for entry in &bucket.entries {
            for bit in 0..cfg.entry_bits() {
                bits.push((entry.data[bit / 8] >> (bit % 8)) & 1 != 0);
            }
        }
    }
    bits
}

fn unflatten_path<const Z: usize>(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, 1>> {
    (0..cfg.levels)
        .map(|level| Bucket {
            entries: core::array::from_fn(|slot| {
                let entry = level * Z + slot;
                let mut data = [0u8; 1];
                for bit in 0..cfg.entry_bits() {
                    if bits[entry * cfg.entry_bits() + bit] {
                        data[bit / 8] |= 1 << (bit % 8);
                    }
                }
                OramEntry {
                    addr: 0,
                    leaf: 0,
                    data,
                }
            }),
        })
        .collect()
}
