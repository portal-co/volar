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
use volar_mpc::strict_split::{SplitEvaluator, SplitGarbler, SplitInput, SplitOutput};
use volar_mpc::{GateSchedule, MpcError, OtChannel, Transport};
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

/// Garbler role of the migrated ORAM driver.
pub struct SplitOramGarbler<N: VoleArray<u8>> {
    runner: SplitGarbler<N>,
    state: GarblerOramState<N>,
}

impl<N: VoleArray<u8>> SplitOramGarbler<N> {
    pub fn new(secret: GlobalSecret<N>, state: GarblerOramState<N>) -> Self {
        Self {
            runner: SplitGarbler::new(secret),
            state,
        }
    }

    pub fn state(&self) -> &GarblerOramState<N> {
        &self.state
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
        self.state.tape = self.runner.run_with_state::<D>(
            schedule,
            &inputs,
            &[],
            &self.state.tape,
            &outputs,
            transport,
            ot,
        )?;
        Ok(())
    }
}
