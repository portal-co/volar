//! Pre-FHE storage-boundary planning.
//!
//! This module deliberately contains **no FHE implementation, key material,
//! ciphertext format, or encryption call**. It prepares the scheduling facts
//! that are safe and useful today, so a future reviewed FHE provider can swap
//! a long chunk in at the one explicit seam:
//!
//! ```text
//! secret-address ORAM reads -> opaque results -> provider encrypt -> FHE chunk
//! durable held ciphertext -> scheduled split-AES open -> strict GC/ORAM chunk
//! ```
//!
//! The planner never sees a logical ORAM address, a held label, a plaintext,
//! or an FHE ciphertext. An ORAM request is named only by a public schedule
//! token; actual address handling remains in the strict GC/ORAM driver.
//!
//! ## V2 verification log
//!
//! No test in this module may attach an insecure, legacy, or unreviewed FHE
//! implementation merely to exercise this schedule. The exact deferred test
//! matrix is maintained in
//! `docs/handoffs/hybrid-storage-pre-fhe-infrastructure-v1.md` in the parent
//! repository. Every public constructor/method below has a corresponding
//! `TODO(v2-test)` marker at its semantic seam.

use alloc::collections::BTreeSet;
use alloc::vec::Vec;

use volar_mpc::strict_chain::{MaterialRole, StorageOperation};

use crate::oram_batch::{
    DisjointBelowRootWrites, PublicPathOp, ReadPathReuse, plan_disjoint_below_root_writes,
    plan_read_reuse,
};

/// Public identifier of a computation chunk which may later be replaced by an
/// FHE provider invocation. It conveys no cryptographic identity or provider
/// parameter and is deliberately not an FHE handle.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeferredChunkId(pub u32);

/// Public schedule token for one secret-address ORAM read. It is not an ORAM
/// address, physical leaf, ciphertext, or cache key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OramReadToken(pub u64);

/// Public position at which an opaque pre-run ORAM result becomes an input to
/// the deferred chunk's future provider adapter.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeferredInputSlot(pub u32);

/// One secret-address ORAM read that must run before the named deferred chunk.
///
/// `expected_oram_epoch` is the epoch *before* this ORAM access. A successful
/// access consumes exactly one epoch. The plan validates this public sequence
/// so a caller cannot accidentally use a result from a stale ORAM state.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OramReadPreRun {
    pub token: OramReadToken,
    pub expected_oram_epoch: u64,
    pub output: DeferredInputSlot,
}

/// One lazy opening demand for a split-AES durable held-material stream.
///
/// It lowers to [`StorageOperation::Prefetch`] at a chain-round boundary. The
/// operation opens role-local opaque material only; it never decrypts material
/// in either host process.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HeldMaterialPreOpen {
    pub slot: usize,
    pub owner: MaterialRole,
}

/// Public path operations eligible for existing public-address optimizations.
/// These are deliberately separate from [`OramReadPreRun`]: a secret-address
/// ORAM access must never be recast as a public host lookup.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PublicAddressStoragePlan {
    pub operations: Vec<PublicPathOp>,
}

impl PublicAddressStoragePlan {
    /// Build the already-available repeated-read and non-root-disjoint-write
    /// candidates. The caller still performs the physical root commits in
    /// order; no whole-Path-ORAM write parallelism is implied.
    // TODO(v2-test): exercise repeated reads, write barriers, shared
    // descendants, and root-commit ordering through the actual tree transport.
    pub fn read_reuse(&self) -> Vec<ReadPathReuse> {
        plan_read_reuse(&self.operations)
    }

    /// Return a write grouping only if all supplied public leaves are disjoint
    /// below the root. `None` means the normal sequential path is mandatory.
    // TODO(v2-test): drive grouped non-root I/O against a durable tree and
    // prove that roots still commit in original order.
    pub fn disjoint_below_root_writes(
        &self,
        levels: usize,
        leaves: &[u64],
    ) -> Option<DisjointBelowRootWrites> {
        plan_disjoint_below_root_writes(levels, leaves)
    }
}

/// The complete public plan immediately preceding one deferred computation
/// chunk. It is executable today only up to the strict-GC/ORAM and split-AES
/// storage seams. The future provider consumes `oram_reads`' opaque outputs
/// and must be added separately after review.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreFheStoragePlan {
    pub chunk: DeferredChunkId,
    pub oram_reads: Vec<OramReadPreRun>,
    pub held_material: Vec<HeldMaterialPreOpen>,
    pub public_address: PublicAddressStoragePlan,
    final_oram_epoch: u64,
}

/// Public schedule-shape errors. These fail closed before a protocol run;
/// callers must schedule a fresh strict boundary instead of repairing a plan
/// dynamically inside a movfuscated computation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PreFheStoragePlanError {
    /// A schedule token identified more than one ORAM request.
    DuplicateOramToken(OramReadToken),
    /// Two opaque ORAM results were assigned the same future input position.
    DuplicateDeferredInput(DeferredInputSlot),
    /// A read did not name the public epoch produced by its predecessor.
    NonContiguousOramEpoch { expected: u64, actual: u64 },
    /// Public epoch arithmetic overflowed; wrapping could bind stale state.
    OramEpochOverflow,
}

impl PreFheStoragePlan {
    /// Validate and construct a pre-FHE plan.
    ///
    /// ORAM reads retain the caller's declared order. Their expected epochs
    /// must be contiguous from `initial_oram_epoch`, because a normal Path
    /// ORAM read is itself a state-changing access. Held demands are
    /// canonicalized by slot: duplicate equal owners collapse; a garbler and
    /// evaluator demand for the same slot becomes one `Both` prefetch.
    // TODO(v2-test): run a reviewed provider after a real split ORAM pre-run;
    // assert opaque results map to the declared input slots without exposing
    // addresses, labels, plaintexts, or provider ciphertext frames.
    // TODO(v2-test): verify rejection of stale/non-contiguous epochs after a
    // failed ORAM transaction and a replayed manifest.
    pub fn new(
        chunk: DeferredChunkId,
        initial_oram_epoch: u64,
        oram_reads: Vec<OramReadPreRun>,
        held_material: Vec<HeldMaterialPreOpen>,
        public_address: PublicAddressStoragePlan,
    ) -> Result<Self, PreFheStoragePlanError> {
        let mut expected_epoch = initial_oram_epoch;
        let mut tokens = BTreeSet::new();
        let mut outputs = BTreeSet::new();
        for read in &oram_reads {
            if !tokens.insert(read.token) {
                return Err(PreFheStoragePlanError::DuplicateOramToken(read.token));
            }
            if !outputs.insert(read.output) {
                return Err(PreFheStoragePlanError::DuplicateDeferredInput(read.output));
            }
            if read.expected_oram_epoch != expected_epoch {
                return Err(PreFheStoragePlanError::NonContiguousOramEpoch {
                    expected: expected_epoch,
                    actual: read.expected_oram_epoch,
                });
            }
            expected_epoch = expected_epoch
                .checked_add(1)
                .ok_or(PreFheStoragePlanError::OramEpochOverflow)?;
        }

        Ok(Self {
            chunk,
            oram_reads,
            held_material: canonicalize_held_material(held_material),
            public_address,
            final_oram_epoch: expected_epoch,
        })
    }

    /// Epoch expected after every scheduled ORAM pre-run succeeds.
    // TODO(v2-test): bind this value to the strict ORAM driver's success-only
    // epoch and prove that aborts do not advance either side.
    pub const fn final_oram_epoch(&self) -> u64 {
        self.final_oram_epoch
    }

    /// Emit the explicit chain-boundary operations for lazy split-AES held
    /// material opening. They are safe to run now via `ChainStoragePhase`;
    /// the adapter owns all AES/OT work and ordinary strict rounds only read
    /// the resulting role-local cache.
    // TODO(v2-test): execute this script with the paired durable material
    // adapters, including `Both`, stale versions, eviction, and a later Held
    // feed. Do not use a host-side AES or an insecure FHE implementation.
    pub fn held_prefetch_operations(&self) -> Vec<StorageOperation> {
        self.held_material
            .iter()
            .map(|demand| StorageOperation::Prefetch {
                slot: demand.slot,
                owner: demand.owner,
            })
            .collect()
    }
}

fn canonicalize_held_material(demands: Vec<HeldMaterialPreOpen>) -> Vec<HeldMaterialPreOpen> {
    let mut owners = alloc::collections::BTreeMap::<usize, MaterialRole>::new();
    for demand in demands {
        owners
            .entry(demand.slot)
            .and_modify(|owner| *owner = merge_material_owner(*owner, demand.owner))
            .or_insert(demand.owner);
    }
    owners
        .into_iter()
        .map(|(slot, owner)| HeldMaterialPreOpen { slot, owner })
        .collect()
}

fn merge_material_owner(left: MaterialRole, right: MaterialRole) -> MaterialRole {
    match (left, right) {
        (MaterialRole::Both, _) | (_, MaterialRole::Both) => MaterialRole::Both,
        (MaterialRole::Garbler, MaterialRole::Garbler) => MaterialRole::Garbler,
        (MaterialRole::Evaluator, MaterialRole::Evaluator) => MaterialRole::Evaluator,
        (MaterialRole::Garbler, MaterialRole::Evaluator)
        | (MaterialRole::Evaluator, MaterialRole::Garbler) => MaterialRole::Both,
    }
}
