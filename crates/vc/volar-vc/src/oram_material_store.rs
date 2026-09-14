//! Paired split-key durable held-material adapters.
//!
//! Strict rounds stage/load opaque role-local labels in memory.  Explicit
//! storage phases run fixed-shape split AES material circuits: ciphertext is
//! evaluator-owned, while a garbler base is never represented evaluator-side.
//! All evaluator-private material/ciphertext bits travel through the supplied
//! `OtChannel`; therefore a caller may substitute Ferret OT transparently.

use alloc::collections::BTreeMap;
use alloc::vec;
use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use volar_mpc::strict_chain::{HeldMaterialStore, MaterialRole};
use volar_mpc::strict_split::{SplitEvaluator, SplitGarbler};
use volar_mpc::{GateSchedule, MpcError, OtChannel, Transport};
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_spec::vole::VoleArray;

use crate::compile_schedule;
use crate::oram_gadget::{
    MaterialBlockDirection, build_material_open_evaluator_block, build_material_open_garbler_block,
    build_material_seal_evaluator_block, build_material_seal_garbler_block,
    material_block_tweak_checked,
};
use crate::oram_material::{
    MATERIAL_BLOCK_BITS, MaterialBlockCachePlan, MaterialBlockLayout, MaterialBlockProtocol,
};

const BLOCK_BYTES: usize = 16;
const GARBLER_REGION: u8 = 0;
const EVALUATOR_REGION: u8 = 1;

/// Public execution-shape counters for one role-local durable material store.
/// They contain no labels, keys, plaintext, or OT contents.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct MaterialStoreMetrics {
    /// AES material circuits actually run (one per encrypted 16-byte block).
    pub material_blocks: u64,
    /// Opening circuits actually run.
    pub opens: u64,
    /// Sealing circuits actually run.
    pub seals: u64,
    /// Complete paired loads skipped because the slot was already resident.
    pub cache_hits: u64,
}

impl MaterialStoreMetrics {
    /// Cache-hit ratio over completed paired-load requests, if any occurred.
    pub fn cache_hit_rate(self) -> Option<(u64, u64)> {
        let requests = self.opens / 2 + self.cache_hits;
        (requests != 0).then_some((self.cache_hits, requests))
    }
}

pub struct GarblerSplitKeyMaterialStore<N: VoleArray<u8>> {
    runner: SplitGarbler<N>,
    key_bases: Vec<Garble<N>>,
    key_half: [bool; 64],
    circuits: Circuits,
    staged: BTreeMap<usize, Garble<N>>,
    cached: BTreeMap<usize, Garble<N>>,
    cache: MaterialBlockCachePlan,
    versions: BTreeMap<(u8, usize), u64>,
    high_water: usize,
    metrics: MaterialStoreMetrics,
}

pub struct EvaluatorSplitKeyMaterialStore<N: VoleArray<u8>> {
    runner: SplitEvaluator<N>,
    key_half: [bool; 64],
    circuits: Circuits,
    staged: BTreeMap<usize, Eval<N>>,
    cached: BTreeMap<usize, Eval<N>>,
    cache: MaterialBlockCachePlan,
    ciphertexts: BTreeMap<(u8, usize), Vec<u8>>,
    versions: BTreeMap<(u8, usize), u64>,
    high_water: usize,
    metrics: MaterialStoreMetrics,
}

struct Circuits {
    seal_garbler: GateSchedule,
    open_garbler: GateSchedule,
    seal_evaluator: GateSchedule,
    open_evaluator: GateSchedule,
}

impl Circuits {
    fn new() -> Result<Self, MpcError> {
        Ok(Self {
            seal_garbler: compile_schedule(&build_material_seal_garbler_block())
                .map_err(|_| MpcError::MalformedSchedule)?,
            open_garbler: compile_schedule(&build_material_open_garbler_block())
                .map_err(|_| MpcError::MalformedSchedule)?,
            seal_evaluator: compile_schedule(&build_material_seal_evaluator_block())
                .map_err(|_| MpcError::MalformedSchedule)?,
            open_evaluator: compile_schedule(&build_material_open_evaluator_block())
                .map_err(|_| MpcError::MalformedSchedule)?,
        })
    }
    fn schedule(&self, direction: MaterialBlockDirection) -> &GateSchedule {
        match direction {
            MaterialBlockDirection::SealGarbler => &self.seal_garbler,
            MaterialBlockDirection::OpenGarbler => &self.open_garbler,
            MaterialBlockDirection::SealEvaluator => &self.seal_evaluator,
            MaterialBlockDirection::OpenEvaluator => &self.open_evaluator,
        }
    }
}

impl<N: VoleArray<u8>> GarblerSplitKeyMaterialStore<N> {
    pub fn new<D: Digest>(
        secret: GlobalSecret<N>,
        key_half: [bool; 64],
        seed: &[u8],
    ) -> Result<Self, MpcError> {
        if seed.is_empty() || N::USIZE > BLOCK_BYTES {
            return Err(MpcError::BadPartition);
        }
        Ok(Self {
            runner: SplitGarbler::new(secret),
            key_bases: (0..128)
                .map(|index| key_base::<N, D>(seed, index))
                .collect(),
            key_half,
            circuits: Circuits::new()?,
            staged: BTreeMap::new(),
            cached: BTreeMap::new(),
            cache: label_cache_plan::<N>()?,
            versions: BTreeMap::new(),
            high_water: 0,
            metrics: MaterialStoreMetrics::default(),
        })
    }

    /// Snapshot public durable-material work counters for this role.
    pub fn metrics(&self) -> MaterialStoreMetrics {
        self.metrics
    }

    fn count_block(&mut self, direction: MaterialBlockDirection) {
        self.metrics.material_blocks += 1;
        if matches!(
            direction,
            MaterialBlockDirection::OpenGarbler | MaterialBlockDirection::OpenEvaluator
        ) {
            self.metrics.opens += 1;
        } else {
            self.metrics.seals += 1;
        }
    }

    fn run<D: Digest>(
        &mut self,
        direction: MaterialBlockDirection,
        slot: usize,
        version: u64,
        material: &[bool],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<u8>, MpcError> {
        self.count_block(direction);
        let protocol = MaterialBlockProtocol::for_direction(direction);
        let tweak = tweak(region(direction), slot, version)?;
        let mut bases = self.key_bases.clone();
        bases.extend((0..256).map(|index| fresh_base::<N, D>(slot, version, index)));
        let mut garbler_bits = self.key_half.to_vec();
        if direction == MaterialBlockDirection::SealGarbler {
            garbler_bits.extend_from_slice(material);
        }
        let result = self.runner.run_with_state::<D>(
            self.circuits.schedule(direction),
            bases,
            &protocol.inputs,
            &bits(&tweak),
            &garbler_bits,
            &protocol.outputs,
            transport,
            ot,
        )?;
        let expected = if direction == MaterialBlockDirection::OpenGarbler {
            MATERIAL_BLOCK_BITS
        } else {
            0
        };
        if result.revealed.len() != expected {
            return Err(MpcError::MalformedSchedule);
        }
        Ok(unbits(&result.revealed))
    }
}

impl<N: VoleArray<u8>> EvaluatorSplitKeyMaterialStore<N> {
    pub fn new(key_half: [bool; 64]) -> Result<Self, MpcError> {
        if N::USIZE > BLOCK_BYTES {
            return Err(MpcError::BadPartition);
        }
        Ok(Self {
            runner: SplitEvaluator::new(),
            key_half,
            circuits: Circuits::new()?,
            staged: BTreeMap::new(),
            cached: BTreeMap::new(),
            cache: label_cache_plan::<N>()?,
            ciphertexts: BTreeMap::new(),
            versions: BTreeMap::new(),
            high_water: 0,
            metrics: MaterialStoreMetrics::default(),
        })
    }

    /// Snapshot public durable-material work counters for this role.
    pub fn metrics(&self) -> MaterialStoreMetrics {
        self.metrics
    }

    fn count_block(&mut self, direction: MaterialBlockDirection) {
        self.metrics.material_blocks += 1;
        if matches!(
            direction,
            MaterialBlockDirection::OpenGarbler | MaterialBlockDirection::OpenEvaluator
        ) {
            self.metrics.opens += 1;
        } else {
            self.metrics.seals += 1;
        }
    }

    fn run<D: Digest>(
        &mut self,
        direction: MaterialBlockDirection,
        material: &[u8],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<u8>, MpcError> {
        if material.len() != BLOCK_BYTES {
            return Err(MpcError::BadPartition);
        }
        self.count_block(direction);
        let protocol = MaterialBlockProtocol::for_direction(direction);
        let mut evaluator_bits = self.key_half.to_vec();
        if direction != MaterialBlockDirection::SealGarbler {
            evaluator_bits.extend(bits(material));
        }
        let (_labels, result) = self.runner.run_with_state::<D>(
            self.circuits.schedule(direction),
            &protocol.inputs,
            &evaluator_bits,
            &[],
            &protocol.outputs,
            transport,
            ot,
        )?;
        let expected = if matches!(
            direction,
            MaterialBlockDirection::SealGarbler
                | MaterialBlockDirection::SealEvaluator
                | MaterialBlockDirection::OpenEvaluator
        ) {
            MATERIAL_BLOCK_BITS
        } else {
            0
        };
        if result.len() != expected {
            return Err(MpcError::MalformedSchedule);
        }
        Ok(unbits(&result))
    }
}

impl<N: VoleArray<u8>> HeldMaterialStore<Garble<N>, N> for GarblerSplitKeyMaterialStore<N> {
    fn load<D: Digest>(
        &mut self,
        slot: usize,
        _: &mut dyn Transport,
        _: &mut dyn OtChannel<N>,
    ) -> Result<Option<Garble<N>>, MpcError> {
        Ok(self.cached.get(&slot).cloned())
    }
    fn store<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        value: Option<Garble<N>>,
        _: &mut dyn Transport,
        _: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        if matches!(owner, MaterialRole::Garbler | MaterialRole::Both) {
            self.staged
                .insert(slot, value.ok_or(MpcError::MalformedSchedule)?);
        }
        Ok(())
    }
    fn prefetch<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        if owner == MaterialRole::Both {
            if !self.cache.needs_open(slot) {
                self.metrics.cache_hits += 1;
                return Ok(());
            }
            self.prefetch::<D>(slot, MaterialRole::Garbler, transport, ot)?;
            return self.prefetch::<D>(slot, MaterialRole::Evaluator, transport, ot);
        }
        let direction = match owner {
            MaterialRole::Garbler => MaterialBlockDirection::OpenGarbler,
            MaterialRole::Evaluator => MaterialBlockDirection::OpenEvaluator,
            MaterialRole::Both => unreachable!("handled paired material load"),
        };
        // This adapter only retains garbler bases. The paired evaluator
        // operation still runs, so it must not consume this cache entry.
        let owns_cached_material = direction == MaterialBlockDirection::OpenGarbler;
        if owns_cached_material && !self.cache.needs_open(slot) {
            return Ok(());
        }
        let region = region(direction);
        let version = *self
            .versions
            .get(&(region, slot))
            .ok_or(MpcError::MalformedSchedule)?;
        let block = self.run::<D>(direction, slot, version, &[], transport, ot)?;
        if direction == MaterialBlockDirection::OpenGarbler {
            self.cached.insert(
                slot,
                Garble {
                    base: Array::from_fn(|i| block[i]),
                },
            );
        }
        if owns_cached_material {
            self.cache.mark_open(slot);
        }
        Ok(())
    }
    fn flush<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        let direction = match owner {
            MaterialRole::Garbler => MaterialBlockDirection::SealGarbler,
            MaterialRole::Evaluator => MaterialBlockDirection::SealEvaluator,
            MaterialRole::Both => return Err(MpcError::BadPartition),
        };
        let region = region(direction);
        let version = next_region(&self.versions, region, slot)?;
        let material = if direction == MaterialBlockDirection::SealGarbler {
            bits(
                self.staged
                    .remove(&slot)
                    .ok_or(MpcError::MalformedSchedule)?
                    .base
                    .as_slice(),
            )
        } else {
            Vec::new()
        };
        let _ = self.run::<D>(direction, slot, version, &material, transport, ot)?;
        self.versions.insert((region, slot), version);
        self.cache.mark_write(slot);
        self.cache.evict(slot);
        self.cached.remove(&slot);
        self.high_water = self.high_water.max(slot.saturating_add(1));
        Ok(())
    }
    fn len(&self) -> usize {
        self.versions.len()
    }
    fn capacity(&self) -> usize {
        self.versions.len()
    }
    fn address_span(&self) -> usize {
        self.high_water
    }
}

impl<N: VoleArray<u8>> HeldMaterialStore<Eval<N>, N> for EvaluatorSplitKeyMaterialStore<N> {
    fn load<D: Digest>(
        &mut self,
        slot: usize,
        _: &mut dyn Transport,
        _: &mut dyn OtChannel<N>,
    ) -> Result<Option<Eval<N>>, MpcError> {
        Ok(self.cached.get(&slot).cloned())
    }
    fn store<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        value: Option<Eval<N>>,
        _: &mut dyn Transport,
        _: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        if matches!(owner, MaterialRole::Evaluator | MaterialRole::Both) {
            self.staged
                .insert(slot, value.ok_or(MpcError::MalformedSchedule)?);
        }
        Ok(())
    }
    fn prefetch<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        if owner == MaterialRole::Both {
            if !self.cache.needs_open(slot) {
                self.metrics.cache_hits += 1;
                return Ok(());
            }
            self.prefetch::<D>(slot, MaterialRole::Garbler, transport, ot)?;
            return self.prefetch::<D>(slot, MaterialRole::Evaluator, transport, ot);
        }
        let direction = match owner {
            MaterialRole::Garbler => MaterialBlockDirection::OpenGarbler,
            MaterialRole::Evaluator => MaterialBlockDirection::OpenEvaluator,
            MaterialRole::Both => unreachable!("handled paired material load"),
        };
        // This adapter only retains evaluator labels. Opening a garbler base
        // is still a paired protocol operation, not an evaluator-cache hit.
        let owns_cached_material = matches!(owner, MaterialRole::Evaluator | MaterialRole::Both);
        if owns_cached_material && !self.cache.needs_open(slot) {
            return Ok(());
        }
        let key = (region(direction), slot);
        let ciphertext = self
            .ciphertexts
            .get(&key)
            .cloned()
            .ok_or(MpcError::MalformedSchedule)?;
        let block = self.run::<D>(direction, &ciphertext, transport, ot)?;
        if matches!(owner, MaterialRole::Evaluator | MaterialRole::Both) {
            self.cached.insert(
                slot,
                Eval {
                    target: Array::from_fn(|i| block[i]),
                },
            );
        }
        if owns_cached_material {
            self.cache.mark_open(slot);
        }
        Ok(())
    }
    fn flush<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        let direction = match owner {
            MaterialRole::Garbler | MaterialRole::Both => MaterialBlockDirection::SealGarbler,
            MaterialRole::Evaluator => MaterialBlockDirection::SealEvaluator,
        };
        let material = if matches!(owner, MaterialRole::Evaluator | MaterialRole::Both) {
            pad(self
                .staged
                .remove(&slot)
                .ok_or(MpcError::MalformedSchedule)?
                .target
                .as_slice())
        } else {
            vec![0; BLOCK_BYTES]
        };
        let version = next_region(&self.versions, region(direction), slot)?;
        let ciphertext = self.run::<D>(direction, &material, transport, ot)?;
        self.ciphertexts
            .insert((region(direction), slot), ciphertext);
        self.versions.insert((region(direction), slot), version);
        self.cache.mark_write(slot);
        self.cache.evict(slot);
        self.cached.remove(&slot);
        self.high_water = self.high_water.max(slot.saturating_add(1));
        Ok(())
    }
    fn len(&self) -> usize {
        self.ciphertexts.len()
    }
    fn capacity(&self) -> usize {
        self.ciphertexts.len()
    }
    fn address_span(&self) -> usize {
        self.high_water
    }
}

fn label_cache_plan<N: VoleArray<u8>>() -> Result<MaterialBlockCachePlan, MpcError> {
    MaterialBlockLayout::new(N::USIZE)
        .map(MaterialBlockCachePlan::new)
        .ok_or(MpcError::BadPartition)
}

fn region(direction: MaterialBlockDirection) -> u8 {
    match direction {
        MaterialBlockDirection::SealGarbler | MaterialBlockDirection::OpenGarbler => GARBLER_REGION,
        MaterialBlockDirection::SealEvaluator | MaterialBlockDirection::OpenEvaluator => {
            EVALUATOR_REGION
        }
    }
}
fn tweak(region: u8, slot: usize, version: u64) -> Result<[u8; 16], MpcError> {
    material_block_tweak_checked(region, slot as u64, version, 0).ok_or(MpcError::BadPartition)
}
fn next_region(map: &BTreeMap<(u8, usize), u64>, region: u8, slot: usize) -> Result<u64, MpcError> {
    map.get(&(region, slot))
        .copied()
        .unwrap_or(0)
        .checked_add(1)
        .ok_or(MpcError::MalformedSchedule)
}
fn key_base<N: VoleArray<u8>, D: Digest>(seed: &[u8], index: usize) -> Garble<N> {
    let h = D::digest(
        [
            b"volar-vc/material-key".as_slice(),
            seed,
            &index.to_le_bytes(),
        ]
        .concat(),
    );
    Garble {
        base: Array::from_fn(|i| h[i % h.len()]),
    }
}
fn fresh_base<N: VoleArray<u8>, D: Digest>(slot: usize, version: u64, index: usize) -> Garble<N> {
    let h = D::digest(
        [
            b"volar-vc/material-base".as_slice(),
            &slot.to_le_bytes(),
            &version.to_le_bytes(),
            &index.to_le_bytes(),
        ]
        .concat(),
    );
    Garble {
        base: Array::from_fn(|i| h[i % h.len()]),
    }
}
fn bits(bytes: &[u8]) -> Vec<bool> {
    bytes
        .iter()
        .flat_map(|b| (0..8).map(move |i| (b >> i) & 1 != 0))
        .collect()
}
fn unbits(bits: &[bool]) -> Vec<u8> {
    let mut bytes = vec![0; BLOCK_BYTES];
    for (i, bit) in bits.iter().enumerate() {
        if *bit {
            bytes[i / 8] |= 1 << (i % 8);
        }
    }
    bytes
}
fn pad(bytes: &[u8]) -> Vec<u8> {
    let mut out = vec![0; BLOCK_BYTES];
    out[..bytes.len()].copy_from_slice(bytes);
    out
}
