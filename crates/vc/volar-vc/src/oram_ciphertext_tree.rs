//! Evaluator-owned ciphertext-tree half of an atomic split-key ORAM access.
//!
//! This module contains no garbler labels, false-label bases, or AES key
//! material. Its narrow interface makes physical path I/O atomic with respect
//! to the public access epoch: an opened path can be committed exactly once,
//! only at the same leaf and tree epoch. The later split AES access driver
//! consumes [`OpenedCiphertextPath::bits`] as evaluator-private inputs and
//! returns the ciphertext bits accepted by [`CiphertextTree::commit`].

use alloc::vec::Vec;

use volar_mpc::MpcError;
use volar_oram::{Bucket, OramEntry, OramTree};

use crate::oram_gadget::OramGadgetConfig;

/// Evaluator-owned physical ciphertext tree.
///
/// The current split-key access circuit supports its lazy, non-versioned
/// encrypted mode: `encrypt_valid = false` and `versioned_pads = false`.
/// Rejecting the other configurations here is fail-closed: they need a
/// circuit-only formatter/version commit protocol before a split key can use
/// them safely.
pub struct CiphertextTree<const Z: usize> {
    tree: OramTree<Z, 1>,
    cfg: OramGadgetConfig,
    epoch: u64,
}

/// Single-use evaluator-local authority to commit one exact path read.
///
/// Its fields remain private so callers cannot fabricate a commit for a
/// different leaf or stale epoch.
pub struct OpenedCiphertextPath {
    leaf: u64,
    epoch: u64,
    bits: Vec<bool>,
}

impl<const Z: usize> CiphertextTree<Z> {
    /// Create the evaluator's independent tree module.
    pub fn new(cfg: &OramGadgetConfig) -> Result<Self, MpcError> {
        if !cfg.encrypted
            || cfg.bucket_size != Z
            || cfg.tree_block_bytes() > 1
            || cfg.encrypt_valid
            || cfg.versioned_pads
        {
            return Err(MpcError::BadPartition);
        }
        Ok(Self {
            tree: OramTree::new(cfg.levels),
            cfg: cfg.clone(),
            epoch: 0,
        })
    }

    /// The public operation ordinal to bind to the paired garbler driver's
    /// success-only epoch.
    pub fn epoch(&self) -> u64 {
        self.epoch
    }

    /// Open one fixed-width ciphertext path for `leaf`.
    pub fn open(&self, leaf: u64) -> Result<OpenedCiphertextPath, MpcError> {
        if leaf >= self.cfg.num_leaves() as u64 {
            return Err(MpcError::BadPartition);
        }
        Ok(OpenedCiphertextPath {
            leaf,
            epoch: self.epoch,
            bits: flatten_path(&self.tree.read_path(leaf), &self.cfg),
        })
    }

    /// Atomically write the ciphertext response for this exact opening.
    ///
    /// A wrong-width response, a stale opening, or an attempt to reuse a
    /// previous opening returns an error before tree mutation. On success the
    /// epoch advances exactly once, which the role-local key drivers bind to
    /// their `complete_access` calls.
    pub fn commit(
        &mut self,
        opening: OpenedCiphertextPath,
        writeback: &[bool],
    ) -> Result<(), MpcError> {
        if opening.epoch != self.epoch || writeback.len() != self.path_width() {
            return Err(MpcError::MalformedSchedule);
        }
        self.tree
            .write_path(opening.leaf, &unflatten_path::<Z>(writeback, &self.cfg));
        self.epoch = self
            .epoch
            .checked_add(1)
            .ok_or(MpcError::MalformedSchedule)?;
        Ok(())
    }

    /// Physical ciphertext width, public from ORAM geometry.
    pub fn path_width(&self) -> usize {
        self.cfg.path_entries() * self.cfg.entry_bits()
    }
}

/// Evaluator-side prepared access. The opening cannot be committed except
/// through [`Self::commit`], which checks the public paired epoch first.
pub struct PreparedCiphertextAccess<'a, const Z: usize> {
    tree: &'a mut CiphertextTree<Z>,
    opening: Option<OpenedCiphertextPath>,
    epoch: u64,
}

impl<'a, const Z: usize> PreparedCiphertextAccess<'a, Z> {
    /// Evaluator-private path bits for the split AES circuit.
    pub fn path_bits(&self) -> &[bool] {
        self.opening.as_ref().expect("prepared path").bits()
    }

    /// Commit a same-epoch circuit write-back. Consuming `self` prevents a
    /// second commit from the same opening even in one process.
    pub fn commit(mut self, epoch: u64, writeback: &[bool]) -> Result<(), MpcError> {
        if epoch != self.epoch {
            return Err(MpcError::MalformedSchedule);
        }
        self.tree
            .commit(self.opening.take().expect("prepared path"), writeback)
    }
}

impl<const Z: usize> CiphertextTree<Z> {
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

impl OpenedCiphertextPath {
    /// Evaluator-private ciphertext bits for the pending split circuit.
    pub fn bits(&self) -> &[bool] {
        &self.bits
    }

    /// Public physical leaf selected by the begin circuit.
    pub fn leaf(&self) -> u64 {
        self.leaf
    }

    /// Public tree epoch paired with the access operation.
    pub fn epoch(&self) -> u64 {
        self.epoch
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
