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

use volar_ir::boolar::BIrStmt;
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

/// Public identity of one provider invocation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProviderInvocationId(pub u64);

/// Unique public identity of an invocation-local mutable cache.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProviderCacheId(pub u64);

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

/// Deterministic allocator of unique cache identities for one compiled plan.
///
/// It contains public plan identities only. It does not retain cache values,
/// which ensures an invocation cannot accidentally observe a discarded cache.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ProviderCacheRegistry {
    next: u64,
    seen_invocations: BTreeSet<ProviderInvocationId>,
}

/// One mutable cache instance, owned by exactly one provider invocation.
///
/// Its source cache map is copied from a read-only base. Writes replace only
/// that map entry; the base wires and all other invocation caches remain
/// untouched. `finish_*` consumes it, so it cannot be reused afterwards.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProviderStorageInvocation {
    pub invocation: ProviderInvocationId,
    pub cache_id: ProviderCacheId,
    pub cache_slots: HeldRange,
    cache: InvocationCache,
    writes: BTreeMap<BaseStorageCell, Vec<IRVarId>>,
    cell_bits: usize,
}

/// Explicit completion of a provider cache invocation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProviderCacheFinish {
    /// The cache was destroyed. Its wires/slots have no later-invocation
    /// identity; a next invocation must copy from a base again.
    Discard {
        invocation: ProviderInvocationId,
        cache_id: ProviderCacheId,
        base: BaseStorageId,
        base_epoch: BaseStorageEpoch,
    },
    /// Only the explicit write set crosses the cache lifetime. The outer
    /// storage adapter must materialize this as a new immutable base snapshot.
    Export {
        invocation: ProviderInvocationId,
        cache_id: ProviderCacheId,
        base: BaseStorageId,
        next_epoch: BaseStorageEpoch,
        writes: BTreeMap<BaseStorageCell, Vec<IRVarId>>,
    },
}

/// Static public loop geometry for a storage-capable provider step.
///
/// A step's first `cache_bits` inputs and outputs are its loop-carried cache
/// state. Remaining inputs are shared readonly-base/view inputs. Remaining
/// outputs are publicly named step results; they do not implicitly become
/// next-iteration inputs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ProviderLoopGeometry {
    pub iterations: usize,
    pub cache_bits: usize,
    pub readonly_bits: usize,
    pub result_bits: usize,
}

/// A finite pure provider step after ordinary program validation.
#[derive(Clone, Debug)]
pub struct ProviderLoopStep<P: Clone> {
    pub circuit: BCircuit<P>,
    pub geometry: ProviderLoopGeometry,
}

/// Result of inlining every static loop iteration into one fused circuit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProviderLoopResult {
    pub cache: Vec<IRVarId>,
    pub results: Vec<Vec<IRVarId>>,
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
    LoopStepArity {
        expected: usize,
        actual: usize,
    },
    LoopStepStateArity {
        expected: usize,
        actual: usize,
    },
    DuplicateInvocation(ProviderInvocationId),
    CacheIdOverflow,
    CacheCellNotPopulated(BaseStorageCell),
    CacheWriteWidth {
        expected: usize,
        actual: usize,
    },
    InvalidExportEpoch {
        current: BaseStorageEpoch,
        requested: BaseStorageEpoch,
    },
    EpochOverflow,
}

impl ProviderCacheRegistry {
    /// Create an empty registry.
    pub const fn new() -> Self {
        Self {
            next: 0,
            seen_invocations: BTreeSet::new(),
        }
    }

    /// Start a fresh mutable cache by reserving opaque held slots in canonical
    /// cached-cell order. The caller must run the returned prefetch script at a
    /// strict-chain boundary before using a held base source.
    pub fn begin(
        &mut self,
        invocation: ProviderInvocationId,
        layout: &ReadonlyStorageLayout,
        manifest: &ReadonlyStorageManifest,
        base_wires: &[IRVarId],
        held_slots: &mut HeldSlots,
    ) -> Result<ProviderStorageInvocation, ReadonlyStorageError> {
        if self.seen_invocations.contains(&invocation) {
            return Err(ReadonlyStorageError::DuplicateInvocation(invocation));
        }
        let cache_id = ProviderCacheId(self.next);
        let next = self
            .next
            .checked_add(1)
            .ok_or(ReadonlyStorageError::CacheIdOverflow)?;
        let cache = layout.copy_into_cache(manifest, base_wires)?;
        let cache_bits = cache
            .copied
            .len()
            .checked_mul(layout.cell_bits)
            .ok_or(ReadonlyStorageError::GeometryOverflow)?;
        let cache_slots = held_slots.reserve(cache_bits);
        self.next = next;
        self.seen_invocations.insert(invocation);
        Ok(ProviderStorageInvocation {
            invocation,
            cache_id,
            cache_slots,
            cache,
            writes: BTreeMap::new(),
            cell_bits: layout.cell_bits,
        })
    }
}

impl ProviderStorageInvocation {
    /// Read a cache line, applying the invocation's latest write if present.
    pub fn read(&self, cell: BaseStorageCell) -> Result<&[IRVarId], ReadonlyStorageError> {
        if let Some(wires) = self.writes.get(&cell) {
            return Ok(wires);
        }
        self.cache
            .copied
            .get(&cell)
            .map(Vec::as_slice)
            .ok_or(ReadonlyStorageError::CacheCellNotPopulated(cell))
    }

    /// Write one populated cache cell. Last write wins in program order but is
    /// strictly local to this invocation's map.
    pub fn write(
        &mut self,
        cell: BaseStorageCell,
        wires: &[IRVarId],
    ) -> Result<(), ReadonlyStorageError> {
        if !self.cache.copied.contains_key(&cell) {
            return Err(ReadonlyStorageError::CacheCellNotPopulated(cell));
        }
        if wires.len() != self.cell_bits {
            return Err(ReadonlyStorageError::CacheWriteWidth {
                expected: self.cell_bits,
                actual: wires.len(),
            });
        }
        self.writes.insert(cell, wires.to_vec());
        Ok(())
    }

    /// Destroy the mutable cache without exporting state.
    pub fn discard(self) -> ProviderCacheFinish {
        ProviderCacheFinish::Discard {
            invocation: self.invocation,
            cache_id: self.cache_id,
            base: self.cache.base,
            base_epoch: self.cache.base_epoch,
        }
    }

    /// Export the explicit write set as the next immutable base version.
    ///
    /// An empty write set is valid and still advances the explicit version;
    /// callers that do not require a next base should use [`Self::discard`].
    pub fn export(
        self,
        next_epoch: BaseStorageEpoch,
    ) -> Result<ProviderCacheFinish, ReadonlyStorageError> {
        let expected = self
            .cache
            .base_epoch
            .0
            .checked_add(1)
            .ok_or(ReadonlyStorageError::EpochOverflow)?;
        if next_epoch.0 != expected {
            return Err(ReadonlyStorageError::InvalidExportEpoch {
                current: self.cache.base_epoch,
                requested: next_epoch,
            });
        }
        Ok(ProviderCacheFinish::Export {
            invocation: self.invocation,
            cache_id: self.cache_id,
            base: self.cache.base,
            next_epoch,
            writes: self.writes,
        })
    }
}

impl<P: Clone> ProviderLoopStep<P> {
    /// Validate a finite, pure, already-unrolled step circuit.
    ///
    /// The circuit must have inputs `[cache, readonly]` and outputs
    /// `[next_cache, result]`. A caller preparing IR must use
    /// `prepare_unbounded_provider_program` before reaching this seam.
    pub fn new(
        circuit: BCircuit<P>,
        geometry: ProviderLoopGeometry,
    ) -> Result<Self, ReadonlyStorageError> {
        let expected_inputs = geometry
            .cache_bits
            .checked_add(geometry.readonly_bits)
            .ok_or(ReadonlyStorageError::GeometryOverflow)?;
        let expected_outputs = geometry
            .cache_bits
            .checked_add(geometry.result_bits)
            .ok_or(ReadonlyStorageError::GeometryOverflow)?;
        if circuit.params as usize != expected_inputs {
            return Err(ReadonlyStorageError::LoopStepArity {
                expected: expected_inputs,
                actual: circuit.params as usize,
            });
        }
        if circuit.outputs.len() != expected_outputs {
            return Err(ReadonlyStorageError::LoopStepStateArity {
                expected: expected_outputs,
                actual: circuit.outputs.len(),
            });
        }
        Ok(Self { circuit, geometry })
    }

    /// Inline every public static iteration. The loop's cache state is the
    /// only loop-carried mutable input; readonly wires are reused directly.
    /// A `Zero`/`Xor` copy keeps each result an explicit invocation-local wire
    /// until the normal whole-loop optimizer runs.
    pub fn compose(
        &self,
        into: &mut BCircuit<P>,
        initial_cache: &[IRVarId],
        readonly: &[IRVarId],
    ) -> Result<ProviderLoopResult, ReadonlyStorageError> {
        if initial_cache.len() != self.geometry.cache_bits {
            return Err(ReadonlyStorageError::CacheWireCount {
                expected: self.geometry.cache_bits,
                actual: initial_cache.len(),
            });
        }
        if readonly.len() != self.geometry.readonly_bits {
            return Err(ReadonlyStorageError::BaseWireCount {
                expected: self.geometry.readonly_bits,
                actual: readonly.len(),
            });
        }
        let mut cache = initial_cache.to_vec();
        let mut results = Vec::with_capacity(self.geometry.iterations);
        for _ in 0..self.geometry.iterations {
            let mut inputs = cache.clone();
            inputs.extend_from_slice(readonly);
            let outputs = inline_loop_step(&self.circuit, into, &inputs)?;
            cache = outputs[..self.geometry.cache_bits].to_vec();
            results.push(outputs[self.geometry.cache_bits..].to_vec());
        }
        Ok(ProviderLoopResult { cache, results })
    }
}

fn inline_loop_step<P: Clone>(
    step: &BCircuit<P>,
    into: &mut BCircuit<P>,
    inputs: &[IRVarId],
) -> Result<Vec<IRVarId>, ReadonlyStorageError> {
    if inputs.len() != step.params as usize {
        return Err(ReadonlyStorageError::LoopStepArity {
            expected: step.params as usize,
            actual: inputs.len(),
        });
    }
    let base = into.var_space();
    for node in &step.stmts {
        let remapped = node
            .kind
            .clone()
            .map(
                &mut (),
                |_, source| {
                    let index = source.0 as usize;
                    Ok::<_, core::convert::Infallible>(if index < step.params as usize {
                        inputs[index]
                    } else {
                        IRVarId(base + (index - step.params as usize) as u32)
                    })
                },
                |_, storage| Ok::<_, core::convert::Infallible>(storage),
            )
            .expect("pure validated loop step maps infallibly");
        into.push_stmt(remapped, node.prov.clone());
    }
    Ok(step
        .outputs
        .iter()
        .map(|source| {
            let index = source.0 as usize;
            if index < step.params as usize {
                inputs[index]
            } else {
                IRVarId(base + (index - step.params as usize) as u32)
            }
        })
        .collect())
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
    use volar_ir_common::Node;

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
    fn loop_step_composes_cache_across_static_iterations() {
        // inputs = [cache, readonly]; outputs = [next_cache, step_result].
        // next_cache = cache XOR readonly; result = cache.
        let step = BCircuit {
            params: 2,
            stmts: vec![Node::new(BIrStmt::Xor(IRVarId(0), IRVarId(1)), (), None)],
            pre_init: vec![],
            outputs: vec![IRVarId(2), IRVarId(0)],
        };
        let step = ProviderLoopStep::new(
            step,
            ProviderLoopGeometry {
                iterations: 3,
                cache_bits: 1,
                readonly_bits: 1,
                result_bits: 1,
            },
        )
        .unwrap();
        let mut combined = BCircuit::new(2);
        let result = step
            .compose(&mut combined, &[IRVarId(0)], &[IRVarId(1)])
            .unwrap();
        assert_eq!(
            result.results,
            vec![vec![IRVarId(0)], vec![IRVarId(2)], vec![IRVarId(3)]]
        );
        assert_eq!(result.cache, vec![IRVarId(4)]);
        assert_eq!(combined.stmts.len(), 3);
    }

    #[test]
    fn loop_step_rejects_bad_geometry() {
        let step = BCircuit::<()> {
            params: 1,
            stmts: vec![],
            pre_init: vec![],
            outputs: vec![IRVarId(0)],
        };
        assert!(matches!(
            ProviderLoopStep::new(
                step,
                ProviderLoopGeometry {
                    iterations: 1,
                    cache_bits: 1,
                    readonly_bits: 1,
                    result_bits: 0,
                }
            ),
            Err(ReadonlyStorageError::LoopStepArity { .. })
        ));
    }

    #[test]
    fn cache_lifecycle_discards_and_exports_versions() {
        let layout_base = layout(ReadonlyStorageSource::PublicConstant);
        let mut slots = HeldSlots::new();
        let manifest = layout_base
            .plan_reads(
                BaseStorageEpoch(9),
                &[ReadonlyStorageRequest::PublicRange {
                    start: BaseStorageCell(2),
                    len: 1,
                }],
                &mut slots,
            )
            .unwrap();
        let base_wires: Vec<_> = (0..16).map(IRVarId).collect();
        let mut registry = ProviderCacheRegistry::new();
        let mut inv = registry
            .begin(
                ProviderInvocationId(1),
                &layout_base,
                &manifest,
                &base_wires,
                &mut slots,
            )
            .unwrap();
        assert!(
            registry
                .begin(
                    ProviderInvocationId(1),
                    &layout_base,
                    &manifest,
                    &base_wires,
                    &mut slots,
                )
                .is_err()
        );
        assert_eq!(
            *inv.read(BaseStorageCell(2)).unwrap(),
            vec![IRVarId(4), IRVarId(5)]
        );
        inv.write(BaseStorageCell(2), &[IRVarId(40), IRVarId(41)])
            .unwrap();
        assert_eq!(
            *inv.read(BaseStorageCell(2)).unwrap(),
            vec![IRVarId(40), IRVarId(41)]
        );
        assert_eq!(base_wires[4], IRVarId(4));

        let exported = inv.export(BaseStorageEpoch(10)).unwrap();
        match exported {
            ProviderCacheFinish::Export {
                next_epoch, writes, ..
            } => {
                assert_eq!(next_epoch, BaseStorageEpoch(10));
                assert_eq!(writes[&BaseStorageCell(2)], vec![IRVarId(40), IRVarId(41)]);
            }
            other => panic!("expected export, got {:?}", other),
        }

        // A fresh invocation for a different identity still reads the base.
        let inv2 = registry
            .begin(
                ProviderInvocationId(2),
                &layout_base,
                &manifest,
                &base_wires,
                &mut slots,
            )
            .unwrap();
        assert_eq!(
            *inv2.read(BaseStorageCell(2)).unwrap(),
            vec![IRVarId(4), IRVarId(5)]
        );
        let discarded = inv2.discard();
        assert!(
            matches!(discarded, ProviderCacheFinish::Discard { cache_id, .. } if cache_id == ProviderCacheId(1))
        );
    }

    #[test]
    fn cache_lifecycle_rejects_bad_export_epoch_and_unpopulated_write() {
        let layout_base = layout(ReadonlyStorageSource::PublicConstant);
        let mut slots = HeldSlots::new();
        let manifest = layout_base
            .plan_reads(
                BaseStorageEpoch(9),
                &[ReadonlyStorageRequest::PublicRange {
                    start: BaseStorageCell(3),
                    len: 1,
                }],
                &mut slots,
            )
            .unwrap();
        let base_wires: Vec<_> = (0..16).map(IRVarId).collect();
        let mut registry = ProviderCacheRegistry::new();
        let inv = registry
            .begin(
                ProviderInvocationId(1),
                &layout_base,
                &manifest,
                &base_wires,
                &mut slots,
            )
            .unwrap();
        assert!(matches!(
            inv.clone().export(BaseStorageEpoch(12)),
            Err(ReadonlyStorageError::InvalidExportEpoch { .. })
        ));
        let mut inv = inv;
        assert!(matches!(
            inv.write(BaseStorageCell(1), &[IRVarId(0), IRVarId(1)]),
            Err(ReadonlyStorageError::CacheCellNotPopulated(
                BaseStorageCell(1)
            ))
        ));
    }

    #[test]
    fn held_cache_prefetch_materializes_and_differentially_matches_clear() {
        // A held-material base: plan reserves slots and emits prefetch ops; the
        // invocation reserves cache slots on top without overlapping the base.
        let mut slots = HeldSlots::new();
        let layout_base = layout(ReadonlyStorageSource::HeldMaterial {
            owner: MaterialRole::Both,
        });
        let manifest = layout_base
            .plan_reads(
                BaseStorageEpoch(9),
                &[ReadonlyStorageRequest::PublicRange {
                    start: BaseStorageCell(1),
                    len: 2,
                }],
                &mut slots,
            )
            .unwrap();
        assert_eq!(manifest.source_slots.unwrap().len(), 4);
        assert_eq!(manifest.prefetch.len(), 4);
        let mut registry = ProviderCacheRegistry::new();
        let base_wires: Vec<_> = (0..16).map(IRVarId).collect();
        let inv = registry
            .begin(
                ProviderInvocationId(1),
                &layout_base,
                &manifest,
                &base_wires,
                &mut slots,
            )
            .unwrap();
        assert_eq!(inv.cache_slots.len(), 4);
        assert_eq!(inv.cache_slots.slot(0), Some(4));
        assert_eq!(slots.len(), 8);
    }

    #[test]
    fn loop_and_cache_compose_a_storage_invocation_differentially() {
        // step(cache, readonly) -> (next_cache, result), next = cache XOR
        // readonly, result = cache. Over 3 iterations with initial cache=0 and
        // readonly=1: cache = 0,1,0 and results = 0,1,0.
        let step = BCircuit {
            params: 2,
            stmts: vec![Node::new(BIrStmt::Xor(IRVarId(0), IRVarId(1)), (), None)],
            pre_init: vec![],
            outputs: vec![IRVarId(2), IRVarId(0)],
        };
        let step = ProviderLoopStep::new(
            step,
            ProviderLoopGeometry {
                iterations: 3,
                cache_bits: 1,
                readonly_bits: 1,
                result_bits: 1,
            },
        )
        .unwrap();
        let mut combined = BCircuit::new(2);
        let loop_result = step
            .compose(&mut combined, &[IRVarId(0)], &[IRVarId(1)])
            .unwrap();
        assert_eq!(
            loop_result
                .results
                .iter()
                .flat_map(|r| r.iter())
                .copied()
                .collect::<Vec<_>>(),
            vec![IRVarId(0), IRVarId(2), IRVarId(3)]
        );
        assert_eq!(loop_result.cache, vec![IRVarId(4)]);
        // Independent scalar reference: evaluate the XOR loop by hand.
        let mut cache = 0u8;
        let mut ref_results = Vec::new();
        for _ in 0..3 {
            ref_results.push(cache);
            cache ^= 1;
        }
        assert_eq!(ref_results, vec![0, 1, 0]);
        assert_eq!(cache, 1);
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
