//! Read-only storage planning for storage-capable circuit providers.
//!
//! This is the first storage layer beneath the circuit-provider ABI. It
//! describes only public storage geometry and deterministic source manifests;
//! it contains no plaintext/ciphertext storage value and cannot mutate a base
//! snapshot. A later cache-composition layer consumes the manifest to build a
//! fresh invocation-local cache.
//!
//! A provider storage access is never a host lookup. Static/public accesses
//! are resolved or coalesced from public layout facts; secret accesses remain
//! explicit bounded scan / pre-run-ORAM demands for the existing strict
//! storage seams.

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;

use volar_ir::circuit::BCircuit;
use volar_ir::ir::IRVarId;
use volar_mpc::strict_chain::{HeldRange, HeldSlots, MaterialRole, StorageOperation};

/// Stable public identity of one immutable storage snapshot.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BaseStorageId(pub u64);

/// Public version of a [`BaseStorageId`].
///
/// A future explicit cache export creates a new version; it never mutates the
/// previous base in place.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BaseStorageEpoch(pub u64);

/// One zero-based cell in a base storage layout.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BaseStorageCell(pub usize);

/// Public representation of base cells. It intentionally carries no value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReadonlyStorageSource {
    /// The cell is constructed from public constants by the eventual circuit.
    PublicConstant,
    /// The cell's opaque role-local labels live in the base held range.
    HeldMaterial { owner: MaterialRole },
    /// The cell becomes available only from the declared pre-run ORAM output.
    OramPreRun,
}

/// Permitted access form, declared by public provider storage geometry.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReadonlyAccessKind {
    /// A finite set of statically named cells.
    StaticSet,
    /// An ordinary public cell index or public interval.
    Public,
    /// A secret index retained as a bounded MUX/ORAM operation.
    SecretBounded,
    /// A full sequential scan of the entire base.
    Sequential,
}

/// Public, immutable geometry for one provider base snapshot.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReadonlyStorageLayout {
    pub base: BaseStorageId,
    pub epoch: BaseStorageEpoch,
    /// Number of Boolean wires in each logical cell.
    pub cell_bits: usize,
    /// Number of logical cells in the immutable snapshot.
    pub cells: usize,
    pub permitted: Vec<ReadonlyAccessKind>,
    pub source: ReadonlyStorageSource,
}

/// One requested read, which contains only public shape and never a value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReadonlyStorageRequest {
    StaticCell(BaseStorageCell),
    PublicCell(BaseStorageCell),
    PublicRange {
        start: BaseStorageCell,
        len: usize,
    },
    /// The public bound is the number of cells considered by an eventual
    /// existing MUX/ORAM lowering; it must equal this base layout's capacity.
    SecretBounded {
        bound: usize,
    },
    Sequential,
}

/// A canonical base range copied into a future invocation cache.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BaseCopyRange {
    pub start: BaseStorageCell,
    pub len: usize,
}

/// Explicit secret-address demand retained for the strict storage planner.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SecretBaseRead {
    pub bound: usize,
}

/// The canonical public result of planning readonly provider accesses.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReadonlyStorageManifest {
    pub base: BaseStorageId,
    pub epoch: BaseStorageEpoch,
    pub copies: Vec<BaseCopyRange>,
    pub secret_reads: Vec<SecretBaseRead>,
    /// Held material range reserved in canonical copy order, when the base
    /// source is held material. It has exactly `copied_cells * cell_bits`
    /// slots; its values remain opaque labels to both roles.
    pub source_slots: Option<HeldRange>,
    pub prefetch: Vec<StorageOperation>,
}

/// Exact ordinary Boolean wires copied into one fresh provider invocation.
///
/// The map identifies cache positions, rather than base positions, so a later
/// cache write can never alias/mutate the caller-owned base vector.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InvocationCache {
    pub base: BaseStorageId,
    pub base_epoch: BaseStorageEpoch,
    /// Base cell -> contiguous cache wires in canonical copy order.
    pub copied: BTreeMap<BaseStorageCell, Vec<IRVarId>>,
}

/// Fail-closed public-shape errors for readonly provider storage.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReadonlyStorageError {
    ZeroCellBits,
    ZeroCells,
    GeometryOverflow,
    AccessNotPermitted(ReadonlyAccessKind),
    CellOutOfRange {
        cell: BaseStorageCell,
        cells: usize,
    },
    RangeOutOfRange {
        start: BaseStorageCell,
        len: usize,
        cells: usize,
    },
    SecretBoundMismatch {
        expected: usize,
        actual: usize,
    },
    /// A caller held an old manifest for a different snapshot version.
    StaleEpoch {
        expected: BaseStorageEpoch,
        actual: BaseStorageEpoch,
    },
    BaseWireCount {
        expected: usize,
        actual: usize,
    },
    CacheWireCount {
        expected: usize,
        actual: usize,
    },
}

impl ReadonlyStorageLayout {
    /// Construct a validated immutable base layout.
    // TODO(provider-ledger: FHE-PLUMB-CIRCUIT-STORAGE-01): bind a real strict
    // held/ORAM fixture after a provider-neutral clear evaluator exists.
    pub fn new(
        base: BaseStorageId,
        epoch: BaseStorageEpoch,
        cell_bits: usize,
        cells: usize,
        permitted: Vec<ReadonlyAccessKind>,
        source: ReadonlyStorageSource,
    ) -> Result<Self, ReadonlyStorageError> {
        if cell_bits == 0 {
            return Err(ReadonlyStorageError::ZeroCellBits);
        }
        if cells == 0 {
            return Err(ReadonlyStorageError::ZeroCells);
        }
        cell_bits
            .checked_mul(cells)
            .ok_or(ReadonlyStorageError::GeometryOverflow)?;
        Ok(Self {
            base,
            epoch,
            cell_bits,
            cells,
            permitted,
            source,
        })
    }

    /// Plan requests against this exact immutable epoch and reserve any
    /// durable held-material slots through the one strict-chain registry.
    pub fn plan_reads(
        &self,
        expected_epoch: BaseStorageEpoch,
        requests: &[ReadonlyStorageRequest],
        held_slots: &mut HeldSlots,
    ) -> Result<ReadonlyStorageManifest, ReadonlyStorageError> {
        if expected_epoch != self.epoch {
            return Err(ReadonlyStorageError::StaleEpoch {
                expected: self.epoch,
                actual: expected_epoch,
            });
        }

        let mut cells = BTreeSet::new();
        let mut secret_reads = Vec::new();
        for request in requests {
            match *request {
                ReadonlyStorageRequest::StaticCell(cell) => {
                    self.require(ReadonlyAccessKind::StaticSet)?;
                    self.check_cell(cell)?;
                    cells.insert(cell.0);
                }
                ReadonlyStorageRequest::PublicCell(cell) => {
                    self.require(ReadonlyAccessKind::Public)?;
                    self.check_cell(cell)?;
                    cells.insert(cell.0);
                }
                ReadonlyStorageRequest::PublicRange { start, len } => {
                    self.require(ReadonlyAccessKind::Public)?;
                    self.check_range(start, len)?;
                    cells.extend(start.0..start.0 + len);
                }
                ReadonlyStorageRequest::SecretBounded { bound } => {
                    self.require(ReadonlyAccessKind::SecretBounded)?;
                    if bound != self.cells {
                        return Err(ReadonlyStorageError::SecretBoundMismatch {
                            expected: self.cells,
                            actual: bound,
                        });
                    }
                    secret_reads.push(SecretBaseRead { bound });
                }
                ReadonlyStorageRequest::Sequential => {
                    self.require(ReadonlyAccessKind::Sequential)?;
                    cells.extend(0..self.cells);
                }
            }
        }
        let copies = coalesce_cells(&cells);
        let copied_bits = cells
            .len()
            .checked_mul(self.cell_bits)
            .ok_or(ReadonlyStorageError::GeometryOverflow)?;
        let (source_slots, prefetch) = match self.source {
            ReadonlyStorageSource::HeldMaterial { owner } => {
                let range = held_slots.reserve(copied_bits);
                let mut prefetch = Vec::with_capacity(copied_bits);
                for index in 0..copied_bits {
                    prefetch.push(StorageOperation::Prefetch {
                        slot: range.slot(index).expect("reserved held range covers index"),
                        owner,
                    });
                }
                (Some(range), prefetch)
            }
            ReadonlyStorageSource::PublicConstant | ReadonlyStorageSource::OramPreRun => {
                (None, Vec::new())
            }
        };
        Ok(ReadonlyStorageManifest {
            base: self.base,
            epoch: self.epoch,
            copies,
            secret_reads,
            source_slots,
            prefetch,
        })
    }

    /// Materialize a fresh invocation cache from the canonical public copy
    /// manifest. The base is only read; every returned vector is independently
    /// allocated and a later cache write cannot alter the base input vector.
    ///
    /// This is intentionally wire-level copying, not a host value lookup. A
    /// provider can later replace individual cache wires with transformed
    /// circuit outputs while the original base wires remain intact.
    pub fn copy_into_cache(
        &self,
        manifest: &ReadonlyStorageManifest,
        base_wires: &[IRVarId],
    ) -> Result<InvocationCache, ReadonlyStorageError> {
        if manifest.base != self.base || manifest.epoch != self.epoch {
            return Err(ReadonlyStorageError::StaleEpoch {
                expected: self.epoch,
                actual: manifest.epoch,
            });
        }
        let expected = self
            .cells
            .checked_mul(self.cell_bits)
            .ok_or(ReadonlyStorageError::GeometryOverflow)?;
        if base_wires.len() != expected {
            return Err(ReadonlyStorageError::BaseWireCount {
                expected,
                actual: base_wires.len(),
            });
        }
        let mut copied = BTreeMap::new();
        for range in &manifest.copies {
            for cell in range.start.0..range.start.0 + range.len {
                let start = cell * self.cell_bits;
                copied.insert(
                    BaseStorageCell(cell),
                    base_wires[start..start + self.cell_bits].to_vec(),
                );
            }
        }
        Ok(InvocationCache {
            base: self.base,
            base_epoch: self.epoch,
            copied,
        })
    }

    /// Compose the manifest's cached source wires into a fresh fused circuit
    /// cache. Each bit is `Xor(base, Zero)`: after normal fold/CSE the values
    /// keep the same logical value but carry explicit cache-result identities
    /// for the storage planner. A later provider write replaces cache mappings
    /// only, never the original base wire ids.
    pub fn compose_cache_copy<P: Clone + Default>(
        &self,
        manifest: &ReadonlyStorageManifest,
        base_wires: &[IRVarId],
        into: &mut BCircuit<P>,
    ) -> Result<InvocationCache, ReadonlyStorageError> {
        let source = self.copy_into_cache(manifest, base_wires)?;
        let zero = into.push_stmt(volar_ir::boolar::BIrStmt::Zero, P::default());
        let mut copied = BTreeMap::new();
        for (cell, wires) in source.copied {
            let wires = wires
                .into_iter()
                .map(|wire| {
                    into.push_stmt(volar_ir::boolar::BIrStmt::Xor(wire, zero), P::default())
                })
                .collect();
            copied.insert(cell, wires);
        }
        Ok(InvocationCache {
            base: source.base,
            base_epoch: source.base_epoch,
            copied,
        })
    }

    fn require(&self, kind: ReadonlyAccessKind) -> Result<(), ReadonlyStorageError> {
        if self.permitted.contains(&kind) {
            Ok(())
        } else {
            Err(ReadonlyStorageError::AccessNotPermitted(kind))
        }
    }

    fn check_cell(&self, cell: BaseStorageCell) -> Result<(), ReadonlyStorageError> {
        if cell.0 < self.cells {
            Ok(())
        } else {
            Err(ReadonlyStorageError::CellOutOfRange {
                cell,
                cells: self.cells,
            })
        }
    }

    fn check_range(&self, start: BaseStorageCell, len: usize) -> Result<(), ReadonlyStorageError> {
        let Some(end) = start.0.checked_add(len) else {
            return Err(ReadonlyStorageError::RangeOutOfRange {
                start,
                len,
                cells: self.cells,
            });
        };
        if end <= self.cells {
            Ok(())
        } else {
            Err(ReadonlyStorageError::RangeOutOfRange {
                start,
                len,
                cells: self.cells,
            })
        }
    }
}

fn coalesce_cells(cells: &BTreeSet<usize>) -> Vec<BaseCopyRange> {
    let mut ranges = Vec::new();
    let Some(&first) = cells.first() else {
        return ranges;
    };
    let mut start = first;
    let mut previous = first;
    for &cell in cells.iter().skip(1) {
        if cell == previous + 1 {
            previous = cell;
        } else {
            ranges.push(BaseCopyRange {
                start: BaseStorageCell(start),
                len: previous - start + 1,
            });
            start = cell;
            previous = cell;
        }
    }
    ranges.push(BaseCopyRange {
        start: BaseStorageCell(start),
        len: previous - start + 1,
    });
    ranges
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;

    fn layout(source: ReadonlyStorageSource) -> ReadonlyStorageLayout {
        ReadonlyStorageLayout::new(
            BaseStorageId(4),
            BaseStorageEpoch(9),
            2,
            8,
            vec![
                ReadonlyAccessKind::StaticSet,
                ReadonlyAccessKind::Public,
                ReadonlyAccessKind::SecretBounded,
                ReadonlyAccessKind::Sequential,
            ],
            source,
        )
        .unwrap()
    }

    #[test]
    fn static_reads_deduplicate_and_public_ranges_coalesce() {
        let mut slots = HeldSlots::new();
        let manifest = layout(ReadonlyStorageSource::PublicConstant)
            .plan_reads(
                BaseStorageEpoch(9),
                &[
                    ReadonlyStorageRequest::StaticCell(BaseStorageCell(1)),
                    ReadonlyStorageRequest::StaticCell(BaseStorageCell(1)),
                    ReadonlyStorageRequest::PublicRange {
                        start: BaseStorageCell(2),
                        len: 2,
                    },
                    ReadonlyStorageRequest::PublicCell(BaseStorageCell(5)),
                ],
                &mut slots,
            )
            .unwrap();
        assert_eq!(
            manifest.copies,
            vec![
                BaseCopyRange {
                    start: BaseStorageCell(1),
                    len: 3,
                },
                BaseCopyRange {
                    start: BaseStorageCell(5),
                    len: 1,
                },
            ]
        );
        assert!(manifest.prefetch.is_empty());
        assert!(slots.is_empty());
    }

    #[test]
    fn held_base_reserves_canonical_prefetch_slots() {
        let mut slots = HeldSlots::new();
        let manifest = layout(ReadonlyStorageSource::HeldMaterial {
            owner: MaterialRole::Both,
        })
        .plan_reads(
            BaseStorageEpoch(9),
            &[ReadonlyStorageRequest::PublicRange {
                start: BaseStorageCell(3),
                len: 2,
            }],
            &mut slots,
        )
        .unwrap();
        assert_eq!(manifest.source_slots.unwrap().len(), 4);
        assert_eq!(slots.len(), 4);
        assert_eq!(manifest.prefetch.len(), 4);
        assert!(matches!(
            manifest.prefetch[0],
            StorageOperation::Prefetch {
                slot: 0,
                owner: MaterialRole::Both
            }
        ));
    }

    #[test]
    fn cache_copy_isolated_from_base_and_composes_new_wires() {
        let mut slots = HeldSlots::new();
        let base = layout(ReadonlyStorageSource::PublicConstant);
        let manifest = base
            .plan_reads(
                BaseStorageEpoch(9),
                &[ReadonlyStorageRequest::PublicRange {
                    start: BaseStorageCell(1),
                    len: 2,
                }],
                &mut slots,
            )
            .unwrap();
        let base_wires: Vec<_> = (0..16).map(IRVarId).collect();
        let copied = base.copy_into_cache(&manifest, &base_wires).unwrap();
        assert_eq!(
            copied.copied[&BaseStorageCell(1)],
            vec![IRVarId(2), IRVarId(3)]
        );
        // The cache map is its own container: replacing a cache line leaves
        // the source base wire vector and another fresh invocation untouched.
        let mut changed = copied.clone();
        changed
            .copied
            .insert(BaseStorageCell(1), vec![IRVarId(99), IRVarId(100)]);
        assert_eq!(base_wires[2], IRVarId(2));
        assert_eq!(
            base.copy_into_cache(&manifest, &base_wires).unwrap().copied[&BaseStorageCell(1)],
            vec![IRVarId(2), IRVarId(3)]
        );

        let mut circuit = BCircuit::<()>::new(16);
        let composed = base
            .compose_cache_copy(&manifest, &base_wires, &mut circuit)
            .unwrap();
        assert_ne!(composed.copied[&BaseStorageCell(1)][0], IRVarId(2));
        assert_eq!(circuit.stmts.len(), 5); // one zero plus four cache copies
    }

    #[test]
    fn secret_reads_remain_explicit_and_stale_epoch_fails() {
        let mut slots = HeldSlots::new();
        let base = layout(ReadonlyStorageSource::OramPreRun);
        let manifest = base
            .plan_reads(
                BaseStorageEpoch(9),
                &[ReadonlyStorageRequest::SecretBounded { bound: 8 }],
                &mut slots,
            )
            .unwrap();
        assert!(manifest.copies.is_empty());
        assert_eq!(manifest.secret_reads, vec![SecretBaseRead { bound: 8 }]);
        assert!(matches!(
            base.plan_reads(BaseStorageEpoch(8), &[], &mut slots),
            Err(ReadonlyStorageError::StaleEpoch { .. })
        ));
    }
}
