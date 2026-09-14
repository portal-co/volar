//! Role-local state drivers for the transport-backed ORAM migration.
//!
//! This module is deliberately separate from the legacy [`crate::oram_2pc`]
//! combined harness. It establishes the state ownership used by the networked
//! begin/access/evict driver: garbler state contains only false-label bases;
//! evaluator state contains only active labels. The first migrated operation is
//! a tape-to-tape compute segment, which already exercises opaque split output
//! threading over the normal strict transport.

use alloc::vec;
use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use volar_mpc::strict_split::{SplitEvaluator, SplitGarbler, SplitInput, SplitOutput};
use volar_mpc::{GateSchedule, MpcError, OtChannel, Transport};

use crate::oram_gadget::OramGadgetConfig;
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_spec::vole::VoleArray;

/// Garbler-only threaded ORAM material. It never contains evaluator labels.
#[derive(Clone)]
pub struct GarblerOramState<N: VoleArray<u8>> {
    tape: Vec<Garble<N>>,
    posmap: Vec<Garble<N>>,
    stash: Vec<Garble<N>>,
}

impl<N: VoleArray<u8>> GarblerOramState<N> {
    /// Construct role-local state from false-label bases produced by an earlier
    /// split invocation. No evaluator material is accepted here.
    pub fn new(tape: Vec<Garble<N>>, posmap: Vec<Garble<N>>, stash: Vec<Garble<N>>) -> Self {
        Self {
            tape,
            posmap,
            stash,
        }
    }

    /// The garbler's tape bases, for public accounting and test inspection.
    pub fn tape(&self) -> &[Garble<N>] {
        &self.tape
    }

    /// The garbler's posmap bases.
    pub fn posmap(&self) -> &[Garble<N>] {
        &self.posmap
    }

    /// The garbler's stash bases.
    pub fn stash(&self) -> &[Garble<N>] {
        &self.stash
    }
}

/// Evaluator-only threaded ORAM material. It never contains garbler bases or
/// a free-XOR delta.
#[derive(Clone)]
pub struct EvaluatorOramState<N: VoleArray<u8>> {
    tape: Vec<Eval<N>>,
    posmap: Vec<Eval<N>>,
    stash: Vec<Eval<N>>,
}

impl<N: VoleArray<u8>> EvaluatorOramState<N> {
    /// Construct role-local state from active labels produced by an earlier
    /// split invocation. No garbler material is accepted here.
    pub fn new(tape: Vec<Eval<N>>, posmap: Vec<Eval<N>>, stash: Vec<Eval<N>>) -> Self {
        Self {
            tape,
            posmap,
            stash,
        }
    }

    /// The evaluator's tape labels, for test inspection and the next circuit.
    pub fn tape(&self) -> &[Eval<N>] {
        &self.tape
    }

    /// The evaluator's posmap labels.
    pub fn posmap(&self) -> &[Eval<N>] {
        &self.posmap
    }

    /// The evaluator's stash labels.
    pub fn stash(&self) -> &[Eval<N>] {
        &self.stash
    }
}

/// Garbler-only half of the split AES-128 tree key. The type intentionally
/// has no operation for reconstructing a full key.
#[derive(Clone, Copy)]
pub struct GarblerTreeKeyHalf([bool; 64]);

impl GarblerTreeKeyHalf {
    pub const fn new(bits: [bool; 64]) -> Self {
        Self(bits)
    }

    fn bits(&self) -> &[bool; 64] {
        &self.0
    }
}

/// Evaluator-only half of the split AES-128 tree key.
#[derive(Clone, Copy)]
pub struct EvaluatorTreeKeyHalf([bool; 64]);

impl EvaluatorTreeKeyHalf {
    pub const fn new(bits: [bool; 64]) -> Self {
        Self(bits)
    }

    fn bits(&self) -> &[bool; 64] {
        &self.0
    }
}

/// Garbler role of the migrated ORAM driver.
pub struct SplitOramGarbler<N: VoleArray<u8>> {
    runner: SplitGarbler<N>,
    state: GarblerOramState<N>,
    fresh: u64,
}

/// Public result of one plaintext-path access. `new_path` is intentionally
/// revealed: the evaluator must write it to its physical tree. Logical read
/// data and the replacement stash remain opaque role-local state.
pub struct SplitAccessResult {
    pub overflow: bool,
    pub new_path: Vec<bool>,
}

impl<N: VoleArray<u8>> SplitOramGarbler<N> {
    pub fn new(secret: GlobalSecret<N>, state: GarblerOramState<N>) -> Self {
        Self {
            runner: SplitGarbler::new(secret),
            state,
            fresh: 0,
        }
    }

    pub fn state(&self) -> &GarblerOramState<N> {
        &self.state
    }

    /// Run the ORAM posmap-update circuit. The old physical leaf is explicitly
    /// revealed; the replacement posmap remains opaque role-local state.
    #[allow(clippy::too_many_arguments)]
    pub fn run_begin<D: Digest>(
        &mut self,
        cfg: &OramGadgetConfig,
        schedule: &GateSchedule,
        addr_slots: &[usize],
        new_leaf_bases: Vec<Garble<N>>,
        new_leaf_bits: &[bool],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<u64, MpcError> {
        if cfg.keyed_leaf
            || self.state.posmap.len() != cfg.num_addrs * cfg.leaf_bits()
            || addr_slots.len() != cfg.addr_bits()
            || new_leaf_bases.len() != cfg.leaf_bits()
            || new_leaf_bits.len() != cfg.leaf_bits()
            || schedule.num_inputs != cfg.begin_params()
        {
            return Err(MpcError::BadPartition);
        }
        let mut bases = self.state.posmap.clone();
        bases.extend(addr_slots.iter().map(|&slot| self.state.tape[slot].clone()));
        bases.extend(new_leaf_bases);
        let mut inputs = vec![SplitInput::Held; self.state.posmap.len() + addr_slots.len()];
        inputs.extend(vec![SplitInput::Garbler; cfg.leaf_bits()]);
        let mut outputs = vec![SplitOutput::Reveal; cfg.leaf_bits()];
        outputs.extend(vec![SplitOutput::Opaque; self.state.posmap.len()]);
        let result = self.runner.run_with_state::<D>(
            schedule,
            bases,
            &inputs,
            &[],
            new_leaf_bits,
            &outputs,
            transport,
            ot,
        )?;
        self.state.posmap = result.output_bases[cfg.leaf_bits()..].to_vec();
        Ok(bits_to_u64(&result.revealed))
    }

    /// Run the plaintext-path access circuit. The path comes from the
    /// evaluator's physical tree as evaluator-private bits; stash/address/data
    /// thread as opaque split material. The API deliberately returns only the
    /// public host write-back shape, never logical read data.
    #[allow(clippy::too_many_arguments)]
    pub fn run_access<D: Digest>(
        &mut self,
        cfg: &OramGadgetConfig,
        schedule: &GateSchedule,
        path_bits: &[bool],
        write: bool,
        addr_slots: &[usize],
        wdata_slot: Option<usize>,
        path_leaf: u64,
        new_leaf: u64,
        evict_only: bool,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<SplitAccessResult, MpcError> {
        let (ab, lb, eb, db) = (
            cfg.addr_bits(),
            cfg.leaf_bits(),
            cfg.entry_bits(),
            cfg.data_bits,
        );
        let path_width = cfg.path_entries() * eb;
        if cfg.encrypted
            || cfg.versioned_pads
            || cfg.keyed_leaf
            || db != 1
            || self.state.stash.len() != cfg.max_stash * eb
            || path_bits.len() != path_width
            || addr_slots.len() != ab
            || write && wdata_slot.is_none()
            || schedule.num_inputs != cfg.access_params()
        {
            return Err(MpcError::BadPartition);
        }
        let mut bases = self.state.stash.clone();
        bases.extend((0..path_width).map(|_| self.fresh_base::<D>()));
        bases.extend(addr_slots.iter().map(|&slot| self.state.tape[slot].clone()));
        bases.push(self.fresh_base::<D>()); // op_write public
        if write {
            bases.push(self.state.tape[wdata_slot.expect("checked")].clone());
        } else {
            bases.push(self.fresh_base::<D>());
        }
        bases.extend((0..lb).map(|_| self.fresh_base::<D>())); // path leaf
        bases.extend((0..lb).map(|_| self.fresh_base::<D>())); // new leaf
        bases.push(self.fresh_base::<D>()); // evict_only

        let mut inputs = vec![SplitInput::Held; self.state.stash.len()];
        inputs.extend(vec![SplitInput::Evaluator; path_width]);
        inputs.extend(vec![SplitInput::Held; ab]);
        inputs.push(SplitInput::Public);
        inputs.push(if write {
            SplitInput::Held
        } else {
            SplitInput::Public
        });
        inputs.extend(vec![SplitInput::Public; lb * 2 + 1]);
        let mut public = Vec::with_capacity(1 + usize::from(!write) + lb * 2 + 1);
        public.push(write);
        if !write {
            public.push(false);
        }
        public.extend(u64_bits(path_leaf, lb));
        public.extend(u64_bits(new_leaf, lb));
        public.push(evict_only);

        let path_off = 1 + db;
        let stash_off = path_off + path_width;
        let mut outputs = vec![SplitOutput::Reveal; 1]; // overflow
        outputs.push(SplitOutput::Opaque); // logical rdata
        outputs.extend(vec![SplitOutput::Reveal; path_width]);
        outputs.extend(vec![SplitOutput::Opaque; self.state.stash.len()]);
        let result = self.runner.run_with_state::<D>(
            schedule,
            bases,
            &inputs,
            &public,
            &[],
            &outputs,
            transport,
            ot,
        )?;
        self.state.stash = result.output_bases[stash_off..].to_vec();
        Ok(SplitAccessResult {
            overflow: result.revealed[0],
            new_path: result.revealed[1..].to_vec(),
        })
    }

    fn fresh_base<D: Digest>(&mut self) -> Garble<N> {
        self.fresh += 1;
        let hash = D::digest(
            &[
                b"volar-vc/oram-split-base".as_slice(),
                &self.fresh.to_le_bytes(),
            ]
            .concat(),
        );
        Garble {
            base: Array::from_fn(|i| hash[i % hash.len()]),
        }
    }

    /// Replace one legacy `Stage::Compute` invocation. All tape inputs and
    /// outputs are opaque held wires: the evaluator never returns labels to
    /// the garbler, and the next stage reuses the exact relation.
    pub fn run_compute<D: Digest>(
        &mut self,
        schedule: &GateSchedule,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        if self.state.tape.len() != schedule.num_inputs
            || schedule.output_wires().len() != self.state.tape.len()
        {
            return Err(MpcError::BadPartition);
        }
        let inputs = vec![SplitInput::Held; self.state.tape.len()];
        let outputs = vec![SplitOutput::Opaque; self.state.tape.len()];
        let result = self.runner.run_with_state::<D>(
            schedule,
            self.state.tape.clone(),
            &inputs,
            &[],
            &[],
            &outputs,
            transport,
            ot,
        )?;
        self.state.tape = result.output_bases;
        Ok(())
    }
}

/// Garbler-only persistent key-bearing driver for the encrypted migration.
/// It owns all AES input bases (as every garbling input needs a base) but only
/// the garbler's 64 Boolean key bits. It has no evaluator labels or tree.
pub struct GarblerEncryptedOramDriver<N: VoleArray<u8>> {
    driver: SplitOramGarbler<N>,
    key_half: GarblerTreeKeyHalf,
    key_input_bases: Vec<Garble<N>>,
    epoch: u64,
}

impl<N: VoleArray<u8>> GarblerEncryptedOramDriver<N> {
    /// Bind a role-local garbler driver to its AES half. `base_seed` is
    /// garbler-local entropy used only to derive false-label bases; it is not
    /// an AES key and must not be shared with the evaluator.
    pub fn new<D: Digest>(
        driver: SplitOramGarbler<N>,
        key_half: GarblerTreeKeyHalf,
        base_seed: &[u8],
    ) -> Result<Self, MpcError> {
        if base_seed.is_empty() {
            return Err(MpcError::BadPartition);
        }
        let key_input_bases = (0..128)
            .map(|index| key_base::<N, D>(base_seed, index as u64))
            .collect();
        Ok(Self {
            driver,
            key_half,
            key_input_bases,
            epoch: 0,
        })
    }

    /// Public access ordinal. The peer independently maintains the same
    /// ordinal; later encrypted access framing binds it to tree commits.
    pub fn epoch(&self) -> u64 {
        self.epoch
    }

    /// Advance only after the access protocol succeeds.
    pub fn complete_access(&mut self, expected_epoch: u64) -> Result<(), MpcError> {
        if expected_epoch != self.epoch {
            return Err(MpcError::MalformedSchedule);
        }
        self.epoch = self
            .epoch
            .checked_add(1)
            .ok_or(MpcError::MalformedSchedule)?;
        Ok(())
    }

    /// Internal encrypted-access inputs: 128 garbler-only bases and just this
    /// role's 64 private bits. The evaluator half cannot be obtained here.
    pub(crate) fn key_inputs(&self) -> (&[Garble<N>], &[bool]) {
        (&self.key_input_bases, self.key_half.bits())
    }

    pub(crate) fn driver_mut(&mut self) -> &mut SplitOramGarbler<N> {
        &mut self.driver
    }
}

/// Evaluator-only persistent key-bearing driver. It owns only the evaluator
/// key half and labels/state. It cannot obtain garbler bases or input bases.
pub struct EvaluatorEncryptedOramDriver<N: VoleArray<u8>> {
    driver: SplitOramEvaluator<N>,
    key_half: EvaluatorTreeKeyHalf,
    epoch: u64,
}

impl<N: VoleArray<u8>> EvaluatorEncryptedOramDriver<N> {
    pub fn new(driver: SplitOramEvaluator<N>, key_half: EvaluatorTreeKeyHalf) -> Self {
        Self {
            driver,
            key_half,
            epoch: 0,
        }
    }

    pub fn epoch(&self) -> u64 {
        self.epoch
    }

    pub fn complete_access(&mut self, expected_epoch: u64) -> Result<(), MpcError> {
        if expected_epoch != self.epoch {
            return Err(MpcError::MalformedSchedule);
        }
        self.epoch = self
            .epoch
            .checked_add(1)
            .ok_or(MpcError::MalformedSchedule)?;
        Ok(())
    }

    pub(crate) fn key_input_bits(&self) -> &[bool] {
        self.key_half.bits()
    }

    pub(crate) fn driver_mut(&mut self) -> &mut SplitOramEvaluator<N> {
        &mut self.driver
    }
}

/// Evaluator role of the migrated ORAM driver.
pub struct SplitOramEvaluator<N: VoleArray<u8>> {
    runner: SplitEvaluator<N>,
    state: EvaluatorOramState<N>,
}

impl<N: VoleArray<u8>> SplitOramEvaluator<N> {
    pub fn new(state: EvaluatorOramState<N>) -> Self {
        Self {
            runner: SplitEvaluator::new(),
            state,
        }
    }

    pub fn state(&self) -> &EvaluatorOramState<N> {
        &self.state
    }

    /// Evaluator half of [`SplitOramGarbler::run_begin`]. Only the physical
    /// old leaf is returned; replacement posmap labels remain evaluator-local.
    pub fn run_begin<D: Digest>(
        &mut self,
        cfg: &OramGadgetConfig,
        schedule: &GateSchedule,
        addr_slots: &[usize],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<u64, MpcError> {
        if cfg.keyed_leaf
            || self.state.posmap.len() != cfg.num_addrs * cfg.leaf_bits()
            || addr_slots.len() != cfg.addr_bits()
            || schedule.num_inputs != cfg.begin_params()
        {
            return Err(MpcError::BadPartition);
        }
        let mut held = self.state.posmap.clone();
        held.extend(addr_slots.iter().map(|&slot| self.state.tape[slot].clone()));
        let mut inputs = vec![SplitInput::Held; held.len()];
        inputs.extend(vec![SplitInput::Garbler; cfg.leaf_bits()]);
        let mut outputs = vec![SplitOutput::Reveal; cfg.leaf_bits()];
        outputs.extend(vec![SplitOutput::Opaque; self.state.posmap.len()]);
        let (labels, revealed) = self.runner.run_with_state::<D>(
            schedule,
            &inputs,
            &[],
            &held,
            &outputs,
            transport,
            ot,
        )?;
        self.state.posmap = labels[cfg.leaf_bits()..].to_vec();
        Ok(bits_to_u64(&revealed))
    }

    /// Evaluator half of [`SplitOramGarbler::run_access`]. The evaluator owns
    /// physical path bytes and returns the revealed write-back path, but never
    /// receives a garbler base for the opaque stash/read-data outputs.
    #[allow(clippy::too_many_arguments)]
    pub fn run_access<D: Digest>(
        &mut self,
        cfg: &OramGadgetConfig,
        schedule: &GateSchedule,
        path_bits: &[bool],
        write: bool,
        addr_slots: &[usize],
        wdata_slot: Option<usize>,
        path_leaf: u64,
        new_leaf: u64,
        evict_only: bool,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<SplitAccessResult, MpcError> {
        let (ab, lb, eb, db) = (
            cfg.addr_bits(),
            cfg.leaf_bits(),
            cfg.entry_bits(),
            cfg.data_bits,
        );
        let path_width = cfg.path_entries() * eb;
        if cfg.encrypted
            || cfg.versioned_pads
            || cfg.keyed_leaf
            || db != 1
            || self.state.stash.len() != cfg.max_stash * eb
            || path_bits.len() != path_width
            || addr_slots.len() != ab
            || write && wdata_slot.is_none()
            || schedule.num_inputs != cfg.access_params()
        {
            return Err(MpcError::BadPartition);
        }
        let mut held = self.state.stash.clone();
        held.extend(addr_slots.iter().map(|&slot| self.state.tape[slot].clone()));
        if write {
            held.push(self.state.tape[wdata_slot.expect("checked")].clone());
        }
        let mut inputs = vec![SplitInput::Held; self.state.stash.len()];
        inputs.extend(vec![SplitInput::Evaluator; path_width]);
        inputs.extend(vec![SplitInput::Held; ab]);
        inputs.push(SplitInput::Public);
        inputs.push(if write {
            SplitInput::Held
        } else {
            SplitInput::Public
        });
        inputs.extend(vec![SplitInput::Public; lb * 2 + 1]);
        let mut public = Vec::with_capacity(1 + usize::from(!write) + lb * 2 + 1);
        public.push(write);
        if !write {
            public.push(false);
        }
        public.extend(u64_bits(path_leaf, lb));
        public.extend(u64_bits(new_leaf, lb));
        public.push(evict_only);
        let path_off = 1 + db;
        let stash_off = path_off + path_width;
        let mut outputs = vec![SplitOutput::Reveal; 1];
        outputs.push(SplitOutput::Opaque);
        outputs.extend(vec![SplitOutput::Reveal; path_width]);
        outputs.extend(vec![SplitOutput::Opaque; self.state.stash.len()]);
        let (labels, revealed) = self
            .runner
            .run_with_state::<D>(schedule, &inputs, path_bits, &held, &outputs, transport, ot)?;
        self.state.stash = labels[stash_off..].to_vec();
        Ok(SplitAccessResult {
            overflow: revealed[0],
            new_path: revealed[1..].to_vec(),
        })
    }

    /// Evaluator half of [`SplitOramGarbler::run_compute`].
    pub fn run_compute<D: Digest>(
        &mut self,
        schedule: &GateSchedule,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        if self.state.tape.len() != schedule.num_inputs
            || schedule.output_wires().len() != self.state.tape.len()
        {
            return Err(MpcError::BadPartition);
        }
        let inputs = vec![SplitInput::Held; self.state.tape.len()];
        let outputs = vec![SplitOutput::Opaque; self.state.tape.len()];
        let (labels, _revealed) = self.runner.run_with_state::<D>(
            schedule,
            &inputs,
            &[],
            &self.state.tape,
            &outputs,
            transport,
            ot,
        )?;
        self.state.tape = labels;
        Ok(())
    }
}

fn key_base<N: VoleArray<u8>, D: Digest>(seed: &[u8], index: u64) -> Garble<N> {
    let digest = D::digest(
        &[
            b"volar-vc/encrypted-oram-key-base".as_slice(),
            seed,
            &index.to_le_bytes(),
        ]
        .concat(),
    );
    Garble {
        base: Array::from_fn(|i| digest[i % digest.len()]),
    }
}

fn u64_bits(value: u64, width: usize) -> Vec<bool> {
    (0..width).map(|bit| (value >> bit) & 1 != 0).collect()
}

fn bits_to_u64(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |value, (bit, set)| value | ((*set as u64) << bit))
}
