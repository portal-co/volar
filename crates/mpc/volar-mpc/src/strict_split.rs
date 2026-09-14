//! Lower-level role-split strict-circuit runner.
//!
//! This module is the seam used by durable ORAM adapters. It exposes the
//! garbler's false-label bases and evaluator's active labels separately, so a
//! begin/access/evict driver can thread them without a combined-role state.
//! It deliberately delegates table streaming and OT to the strict protocol;
//! it never uses loopback OT or a local evaluator walk.

use alloc::vec;
use alloc::vec::Vec;

use digest::Digest;
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_spec::vole::VoleArray;

use crate::strict::{
    STRICT_TABLE_CHUNK, decode_output_label, eliminate_nots, eval_strict_table_stream,
    garble_schedule_strict_dyn_full,
};
use crate::{
    GateSchedule, InputOwner, MpcError, OtChannel, SessionFrame, Transport, arr_to_vec, vec_to_arr,
};

struct TransportRef<'a>(&'a mut dyn Transport);

impl Transport for TransportRef<'_> {
    fn send(&mut self, frame: &[u8]) {
        self.0.send(frame);
    }

    fn recv(&mut self) -> Vec<u8> {
        self.0.recv()
    }
}

/// Disposition of one lower-level circuit output. The script is public and
/// shared by both roles; an opaque output never crosses the role seam.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SplitOutput {
    /// Return the evaluator label to the garbler for exact-match decoding.
    Reveal,
    /// Keep the evaluator label and garbler base in their respective roles.
    /// This is the disposition for threaded posmap/stash/material wires.
    Opaque,
}

/// Garbler-local result of a split invocation.
pub struct SplitGarblerResult<N: VoleArray<u8>> {
    /// One false-label base per circuit output. No evaluator labels occur here.
    pub output_bases: Vec<Garble<N>>,
    /// Authenticated bits for the `Reveal` entries, in output order restricted
    /// to revealed outputs.
    pub revealed: Vec<bool>,
}

/// Garbler role of a single lower-level strict circuit invocation.
pub struct SplitGarbler<N: VoleArray<u8>> {
    secret: GlobalSecret<N>,
}

impl<N: VoleArray<u8>> SplitGarbler<N> {
    pub fn new(secret: GlobalSecret<N>) -> Self {
        Self { secret }
    }

    /// Execute a circuit with all outputs revealed. New durable-state callers
    /// should use [`Self::run_with_outputs`] and mark threaded wires opaque.
    pub fn run<D: Digest>(
        &self,
        schedule: &GateSchedule,
        input_bases: Vec<Garble<N>>,
        partition: &[InputOwner],
        public_bits: &[bool],
        garbler_bits: &[bool],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<Garble<N>>, MpcError> {
        let outputs = vec![SplitOutput::Reveal; schedule.output_wires().len()];
        Ok(self
            .run_with_outputs::<D>(
                schedule,
                input_bases,
                partition,
                public_bits,
                garbler_bits,
                &outputs,
                transport,
                ot,
            )?
            .output_bases)
    }

    /// Execute a circuit and retain opaque outputs in separate role-local
    /// state. The evaluator sends labels only for `Reveal` entries, so this
    /// role never receives an evaluator label for an opaque output.
    #[allow(clippy::too_many_arguments)]
    pub fn run_with_outputs<D: Digest>(
        &self,
        schedule: &GateSchedule,
        input_bases: Vec<Garble<N>>,
        partition: &[InputOwner],
        public_bits: &[bool],
        garbler_bits: &[bool],
        outputs: &[SplitOutput],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<SplitGarblerResult<N>, MpcError> {
        if partition.len() != schedule.num_inputs
            || input_bases.len() != schedule.num_inputs
            || outputs.len() != schedule.output_wires().len()
        {
            return Err(MpcError::BadPartition);
        }
        let eliminated = eliminate_nots(schedule)?;
        let full =
            garble_schedule_strict_dyn_full::<N, D>(&eliminated, self.secret.clone(), input_bases)?;
        let mut owned = Vec::new();
        let mut public_i = 0;
        let mut garbler_i = 0;
        for (wire, owner) in full.exec.circuit.input_labels.iter().zip(partition) {
            match owner {
                InputOwner::Public => {
                    let bit = *public_bits.get(public_i).ok_or(MpcError::BadPartition)?;
                    public_i += 1;
                    owned.push(arr_to_vec(&self.secret.encode(wire, bit).target));
                }
                InputOwner::Garbler => {
                    let bit = *garbler_bits.get(garbler_i).ok_or(MpcError::BadPartition)?;
                    garbler_i += 1;
                    owned.push(arr_to_vec(&self.secret.encode(wire, bit).target));
                }
                InputOwner::Evaluator => {}
            }
        }
        if public_i != public_bits.len() || garbler_i != garbler_bits.len() {
            return Err(MpcError::BadPartition);
        }
        transport.send(&SessionFrame::OwnedInputs(owned).encode());
        for (wire, owner) in full.exec.circuit.input_labels.iter().zip(partition) {
            if *owner == InputOwner::Evaluator {
                let zero = self.secret.encode(wire, false);
                let one = self.secret.encode(wire, true);
                ot.send([&zero.target, &one.target]);
            }
        }
        for tables in full.exec.circuit.tables.chunks(STRICT_TABLE_CHUNK) {
            transport.send(
                &SessionFrame::SetupStrictChunk {
                    tables: tables
                        .iter()
                        .map(|table| core::array::from_fn(|row| arr_to_vec(&table.table[row])))
                        .collect(),
                }
                .encode(),
            );
        }
        transport.send(
            &SessionFrame::SetupStrictEnd {
                table_count: full.exec.circuit.tables.len() as u32,
            }
            .encode(),
        );
        let labels = match SessionFrame::decode(&transport.recv()) {
            Some(SessionFrame::OutputLabels(labels)) => labels,
            _ => return Err(MpcError::UnexpectedMessage),
        };
        let reveal_count = outputs
            .iter()
            .filter(|output| **output == SplitOutput::Reveal)
            .count();
        if labels.len() != reveal_count {
            return Err(MpcError::UnexpectedMessage);
        }
        let mut revealed = Vec::with_capacity(reveal_count);
        let mut label_i = 0usize;
        for (index, output) in outputs.iter().enumerate() {
            if *output == SplitOutput::Reveal {
                let label = vec_to_arr::<N>(&labels[label_i]).ok_or(MpcError::MalformedSchedule)?;
                label_i += 1;
                let base = &full.exec.output_labels[index];
                revealed.push(
                    decode_output_label(
                        &self.secret,
                        base,
                        eliminated.output_polarity[index],
                        &label,
                    )
                    .ok_or(MpcError::DecodeFailure)?,
                );
            }
        }
        transport.send(&SessionFrame::VerdictBits(revealed.clone()).encode());
        Ok(SplitGarblerResult {
            output_bases: full.exec.output_labels,
            revealed,
        })
    }
}

/// Evaluator role of a single lower-level strict circuit invocation.
pub struct SplitEvaluator<N: VoleArray<u8>>(core::marker::PhantomData<N>);

impl<N: VoleArray<u8>> SplitEvaluator<N> {
    pub const fn new() -> Self {
        Self(core::marker::PhantomData)
    }

    /// Execute a circuit with all outputs revealed. New durable-state callers
    /// should use [`Self::run_with_outputs`] and mark threaded wires opaque.
    pub fn run<D: Digest>(
        &self,
        schedule: &GateSchedule,
        partition: &[InputOwner],
        evaluator_bits: &[bool],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<Eval<N>>, MpcError> {
        let output_script = vec![SplitOutput::Reveal; schedule.output_wires().len()];
        self.run_with_outputs::<D>(
            schedule,
            partition,
            evaluator_bits,
            &output_script,
            transport,
            ot,
        )
    }

    /// Execute a circuit, transmitting only output labels the shared script
    /// explicitly marks `Reveal`.
    pub fn run_with_outputs<D: Digest>(
        &self,
        schedule: &GateSchedule,
        partition: &[InputOwner],
        evaluator_bits: &[bool],
        outputs: &[SplitOutput],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<Eval<N>>, MpcError> {
        if partition.len() != schedule.num_inputs || outputs.len() != schedule.output_wires().len()
        {
            return Err(MpcError::BadPartition);
        }
        let eliminated = eliminate_nots(schedule)?;
        let owned = match SessionFrame::decode(&transport.recv()) {
            Some(SessionFrame::OwnedInputs(labels)) => labels,
            _ => return Err(MpcError::UnexpectedMessage),
        };
        let mut labels = Vec::with_capacity(schedule.num_inputs);
        let mut owned_i = 0;
        let mut evaluator_i = 0;
        for owner in partition {
            match owner {
                InputOwner::Public | InputOwner::Garbler => {
                    labels.push(Eval {
                        target: vec_to_arr(owned.get(owned_i).ok_or(MpcError::BadPartition)?)
                            .ok_or(MpcError::MalformedSchedule)?,
                    });
                    owned_i += 1;
                }
                InputOwner::Evaluator => {
                    let bit = *evaluator_bits
                        .get(evaluator_i)
                        .ok_or(MpcError::BadPartition)?;
                    evaluator_i += 1;
                    labels.push(Eval {
                        target: ot.receive(bit),
                    });
                }
            }
        }
        if owned_i != owned.len() || evaluator_i != evaluator_bits.len() {
            return Err(MpcError::BadPartition);
        }
        let mut link = TransportRef(transport);
        let output_labels =
            eval_strict_table_stream::<N, D, _>(&eliminated.schedule, &labels, &mut link)?;
        let revealed_labels: Vec<Vec<u8>> = output_labels
            .iter()
            .zip(outputs)
            .filter_map(|(label, disposition)| {
                (*disposition == SplitOutput::Reveal).then(|| arr_to_vec(&label.target))
            })
            .collect();
        transport.send(&SessionFrame::OutputLabels(revealed_labels).encode());
        match SessionFrame::decode(&transport.recv()) {
            Some(SessionFrame::VerdictBits(_)) => Ok(output_labels),
            Some(SessionFrame::Verdict(Err(()))) => Err(MpcError::DecodeFailure),
            _ => Err(MpcError::UnexpectedMessage),
        }
    }
}

impl<N: VoleArray<u8>> Default for SplitEvaluator<N> {
    fn default() -> Self {
        Self::new()
    }
}
