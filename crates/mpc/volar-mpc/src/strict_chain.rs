//! Strict multi-circuit sessions ("chains"): the protocol-merge layer.
//!
//! A chain is a sequence of strict two-party circuit invocations over one
//! transport. Secrets cross invocations as **threaded re-based labels**:
//! the garbler aligns the next circuit's input base to the previous
//! circuit's output base under the single global delta, so the evaluator's
//! held output label IS the next input label and the value never decodes
//! (the S4 mechanism, here on the strict session: the evaluator never sees
//! the delta). Values that must leave the chain (record ciphertexts, the
//! final verdict) are revealed via the strict OutputLabels/VerdictBits
//! round (exact-match decode: a forged label aborts).
//!
//! Every round rides one transport, so a transcript hash over the session
//! binds all rounds — this is how independent checks (a TLS Turnstile
//! verification and a licensing predicate) are **correlated** rather than
//! standalone: the final circuit ANDs their threaded verdict wires, and
//! neither verdict can be replayed or swapped independently of the other.
//!
//! The polarity subtlety: a strict (Not-eliminated) output's logical value
//! is `raw XOR output_polarity`. Threading registers the RAW evaluator
//! label, so the garbler registers the held slot's false-label base as
//! `raw_base XOR (polarity ? delta : 0)` — then the held label encodes the
//! LOGICAL value under that base (free-XOR: L = raw_base XOR raw*delta =
//! base' XOR (raw XOR pol)*delta).

use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::{Array, ArraySize};
use volar_spec::garble::{Garble, GarbleTable, GlobalSecret};
use volar_spec::vole::VoleArray;

use crate::strict::{decode_output_label, eliminate_nots, garble_schedule_strict_dyn_full};
use crate::{
    DynEvalSetup, Eval, GateSchedule, MpcError, OtChannel, SessionFrame, Transport, arr_to_vec,
    vec_to_arr,
};

/// A chain input feed, per circuit-input bit. The feed script is shared by
/// both parties; each role uses only its own secret slice.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChainFeed {
    /// Public constant: the garbler encodes the value (from the shared
    /// `const_bits` slice) and ships the label in OwnedInputs.
    Const,
    /// Garbler-private input: the garbler encodes from its secret slice.
    Garbler,
    /// Evaluator-private input: 1-of-2 OT (the evaluator chooses on its
    /// secret slice).
    Eval,
    /// Threaded secret: reuse the held slot's label/base. Neither party
    /// decodes; the garbler aligns this input's base to the held base.
    Held(usize),
}

/// What happens to one circuit output bit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChainOut {
    /// Reveal via the strict OutputLabels/VerdictBits round (exact-match
    /// decode; a forged label aborts the session).
    Reveal,
    /// Thread into the held-slot registry (never decoded).
    Hold(usize),
}

/// A contiguous allocation in the strict-chain held-label registry.
///
/// This is deliberately a small value object: TLS and interpreter drivers name
/// data regions once, then derive their input feeds/output dispositions from
/// the range instead of scattering manually coordinated slot arithmetic.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HeldRange {
    start: usize,
    len: usize,
}

impl HeldRange {
    /// Number of labels in this allocation.
    pub const fn len(self) -> usize {
        self.len
    }

    /// Whether this allocation contains no slots.
    pub const fn is_empty(self) -> bool {
        self.len == 0
    }

    /// The registry slot at `index`, or `None` when it is outside this range.
    pub const fn slot(self, index: usize) -> Option<usize> {
        if index < self.len {
            Some(self.start + index)
        } else {
            None
        }
    }

    /// Build held input feeds in range order.
    pub fn feeds(self) -> Vec<ChainFeed> {
        (0..self.len)
            .map(|i| ChainFeed::Held(self.start + i))
            .collect()
    }

    /// Build held output dispositions in range order.
    pub fn holds(self) -> Vec<ChainOut> {
        (0..self.len)
            .map(|i| ChainOut::Hold(self.start + i))
            .collect()
    }
}

/// Deterministic allocator for a chain driver's secret-to-neither registry.
///
/// Allocation is purely script construction: it does not allocate labels or
/// reveal values. Both parties use the same allocation sequence, which makes
/// held-state shape auditable before a session begins.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct HeldSlots {
    next: usize,
}

impl HeldSlots {
    /// Start with no allocated slots.
    pub const fn new() -> Self {
        Self { next: 0 }
    }

    /// Reserve `len` contiguous held slots. Panics on `usize` overflow rather
    /// than wrapping two independent secret regions onto the same labels.
    pub fn reserve(&mut self, len: usize) -> HeldRange {
        let start = self.next;
        self.next = self.next.checked_add(len).expect("held slot overflow");
        HeldRange { start, len }
    }

    /// Number of slots allocated so far.
    pub const fn len(&self) -> usize {
        self.next
    }

    /// Whether no slots have been allocated.
    pub const fn is_empty(&self) -> bool {
        self.next == 0
    }
}

/// One party's chain-round driver. Implemented by [`ChainGarbler`] and
/// [`ChainEvaluator`]; a session script (e.g. the TLS 1.3 client driver) is
/// written once against this trait and executed by both parties with their
/// role's driver and secret inputs.
pub trait ChainParty<N: VoleArray<u8>> {
    /// Run one circuit of the chain. `feeds` has one entry per schedule
    /// input bit; `const_bits` the values of the `Const` feeds in order
    /// (ignored by the evaluator, which consumes the labels); `secret_bits`
    /// the caller's own secret values (`Garbler` feeds on the garbler side,
    /// `Eval` feeds on the evaluator side) in order; `outs` one entry per
    /// output wire. Returns the revealed output bits in output order.
    fn run_round<D: Digest, T: Transport>(
        &mut self,
        schedule: &GateSchedule,
        feeds: &[ChainFeed],
        const_bits: &[bool],
        secret_bits: &[bool],
        outs: &[ChainOut],
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<bool>, MpcError>;
}

/// The garbler's chain driver: holds the global secret (delta), the held
/// slots' false-label bases, and a fresh-base counter.
pub struct ChainGarbler<N: VoleArray<u8>> {
    secret: GlobalSecret<N>,
    held: Vec<Option<Garble<N>>>,
    fresh: u64,
}

impl<N: VoleArray<u8>> ChainGarbler<N> {
    /// A new driver over `secret` with an empty held registry.
    pub fn new(secret: GlobalSecret<N>) -> Self {
        Self {
            secret,
            held: Vec::new(),
            fresh: 0,
        }
    }

    /// Count of currently held labels. This exposes storage shape for
    /// instrumentation without exposing labels or their logical values.
    pub fn held_len(&self) -> usize {
        self.held.iter().filter(|slot| slot.is_some()).count()
    }

    /// Allocated slots, including unwritten holes; this is the peak-state
    /// shape relevant to large jointly-secret ORAM/key material.
    pub fn held_capacity(&self) -> usize {
        self.held.len()
    }

    fn held_get(&self, slot: usize) -> Result<Garble<N>, MpcError> {
        self.held
            .get(slot)
            .and_then(Option::as_ref)
            .cloned()
            .ok_or(MpcError::MalformedSchedule)
    }

    fn held_set(&mut self, slot: usize, value: Garble<N>) {
        if self.held.len() <= slot {
            self.held.resize(slot + 1, None);
        }
        self.held[slot] = Some(value);
    }

    fn fresh_base<D: Digest>(&mut self) -> Garble<N> {
        self.fresh += 1;
        let h = D::new()
            .chain_update(b"volar-mpc/strict-chain-base")
            .chain_update(self.fresh.to_le_bytes())
            .finalize();
        let mut base = Array::<u8, N>::default();
        base.as_mut_slice().copy_from_slice(&h[..N::USIZE]);
        Garble { base }
    }
}

impl<N: VoleArray<u8>> ChainParty<N> for ChainGarbler<N> {
    fn run_round<D: Digest, T: Transport>(
        &mut self,
        schedule: &GateSchedule,
        feeds: &[ChainFeed],
        const_bits: &[bool],
        secret_bits: &[bool],
        outs: &[ChainOut],
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<bool>, MpcError> {
        let elim = eliminate_nots(schedule)?;
        let sched = &elim.schedule;
        if feeds.len() != sched.num_inputs || outs.len() != sched.output_wires().len() {
            return Err(MpcError::BadPartition);
        }

        // Input bases: fresh for Const/Garbler/Eval, aligned for Held.
        let mut bases: Vec<Garble<N>> = Vec::with_capacity(sched.num_inputs);
        let mut n_const = 0usize;
        let mut n_garbler = 0usize;
        for feed in feeds {
            match feed {
                ChainFeed::Const => {
                    n_const += 1;
                    bases.push(self.fresh_base::<D>());
                }
                ChainFeed::Garbler => {
                    n_garbler += 1;
                    bases.push(self.fresh_base::<D>());
                }
                ChainFeed::Eval => bases.push(self.fresh_base::<D>()),
                ChainFeed::Held(slot) => bases.push(self.held_get(*slot)?),
            }
        }
        if const_bits.len() != n_const || secret_bits.len() != n_garbler {
            return Err(MpcError::BadPartition);
        }

        let full = garble_schedule_strict_dyn_full::<N, D>(&elim, self.secret.clone(), bases)?;

        // SetupStrict: tables only.
        transport.send(
            &SessionFrame::SetupStrict {
                tables: full
                    .exec
                    .circuit
                    .tables
                    .iter()
                    .map(|t| {
                        let mut rows: [Vec<u8>; 4] = Default::default();
                        for (r, row) in t.table.iter().enumerate() {
                            rows[r] = arr_to_vec(row);
                        }
                        rows
                    })
                    .collect(),
            }
            .encode(),
        );

        // OwnedInputs: Const + Garbler labels in circuit-input order.
        let mut owned: Vec<Vec<u8>> = Vec::new();
        let mut const_i = 0usize;
        let mut gb_i = 0usize;
        for (idx, feed) in feeds.iter().enumerate() {
            let wire = &full.exec.circuit.input_labels[idx];
            match feed {
                ChainFeed::Const => {
                    let b = const_bits[const_i];
                    const_i += 1;
                    owned.push(arr_to_vec(&full.exec.circuit.secret.encode(wire, b).target));
                }
                ChainFeed::Garbler => {
                    let b = secret_bits[gb_i];
                    gb_i += 1;
                    owned.push(arr_to_vec(&full.exec.circuit.secret.encode(wire, b).target));
                }
                ChainFeed::Eval | ChainFeed::Held(_) => {}
            }
        }
        transport.send(&SessionFrame::OwnedInputs(owned).encode());

        // OT per Eval feed.
        for (idx, feed) in feeds.iter().enumerate() {
            if *feed == ChainFeed::Eval {
                let wire = &full.exec.circuit.input_labels[idx];
                let f = full.exec.circuit.secret.encode(wire, false);
                let t = full.exec.circuit.secret.encode(wire, true);
                ot.send([&f.target, &t.target]);
            }
        }

        // Revealed outputs: exact-match decode.
        let n_reveal = outs.iter().filter(|o| **o == ChainOut::Reveal).count();
        let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
        let labels = match frame {
            SessionFrame::OutputLabels(labels) => labels,
            _ => return Err(MpcError::UnexpectedMessage),
        };
        if labels.len() != n_reveal {
            return Err(MpcError::UnexpectedMessage);
        }
        let mut revealed: Vec<bool> = Vec::with_capacity(n_reveal);
        let mut rev_i = 0usize;
        for (o, out) in outs.iter().enumerate() {
            match out {
                ChainOut::Reveal => {
                    let label =
                        vec_to_arr::<N>(&labels[rev_i]).ok_or(MpcError::MalformedSchedule)?;
                    rev_i += 1;
                    let bit = decode_output_label(
                        &full.exec.circuit.secret,
                        &full.exec.output_labels[o],
                        elim.output_polarity[o],
                        &label,
                    )
                    .ok_or(MpcError::DecodeFailure)?;
                    revealed.push(bit);
                }
                ChainOut::Hold(slot) => {
                    // Thread: register the polarity-adjusted base so the held
                    // label encodes the LOGICAL value.
                    let raw = &full.exec.output_labels[o];
                    let base = if elim.output_polarity[o] {
                        Garble {
                            base: full.exec.circuit.secret.encode(raw, true).target,
                        }
                    } else {
                        raw.clone()
                    };
                    self.held_set(*slot, base);
                }
            }
        }
        transport.send(&SessionFrame::VerdictBits(revealed.clone()).encode());
        Ok(revealed)
    }
}

/// The evaluator's chain driver: holds only the held slots' labels (never
/// the delta).
pub struct ChainEvaluator<N: VoleArray<u8>> {
    held: Vec<Option<Eval<N>>>,
}

impl<N: VoleArray<u8>> ChainEvaluator<N> {
    /// A new driver with an empty held registry.
    pub fn new() -> Self {
        Self { held: Vec::new() }
    }

    /// Number of initialized jointly-secret labels.
    pub fn held_len(&self) -> usize {
        self.held.iter().filter(|slot| slot.is_some()).count()
    }

    /// Allocated evaluator-label slots, including unwritten holes.
    pub fn held_capacity(&self) -> usize {
        self.held.len()
    }

    fn held_get(&self, slot: usize) -> Result<Eval<N>, MpcError> {
        self.held
            .get(slot)
            .and_then(Option::as_ref)
            .cloned()
            .ok_or(MpcError::MalformedSchedule)
    }

    fn held_set(&mut self, slot: usize, value: Eval<N>) {
        if self.held.len() <= slot {
            self.held.resize(slot + 1, None);
        }
        self.held[slot] = Some(value);
    }
}

impl<N: VoleArray<u8>> Default for ChainEvaluator<N> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn held_slots_are_contiguous_and_scriptable_at_scale() {
        let mut slots = HeldSlots::new();
        let key_schedule = slots.reserve(1_024 * 8);
        let record_window = slots.reserve(64 * 1_024 * 8);
        let heap_image = slots.reserve(1_024 * 1_024 * 8);

        assert_eq!(key_schedule.slot(0), Some(0));
        assert_eq!(key_schedule.slot(key_schedule.len()), None);
        assert_eq!(record_window.slot(0), Some(key_schedule.len()));
        assert_eq!(
            heap_image.slot(0),
            Some(key_schedule.len() + record_window.len())
        );
        assert_eq!(
            slots.len(),
            key_schedule.len() + record_window.len() + heap_image.len()
        );
        assert_eq!(heap_image.feeds().len(), heap_image.len());
        assert_eq!(heap_image.holds().len(), heap_image.len());
        assert!(matches!(heap_image.feeds()[0], ChainFeed::Held(_)));
        assert!(matches!(heap_image.holds()[0], ChainOut::Hold(_)));
    }
}

impl<N: VoleArray<u8>> ChainParty<N> for ChainEvaluator<N> {
    fn run_round<D: Digest, T: Transport>(
        &mut self,
        schedule: &GateSchedule,
        feeds: &[ChainFeed],
        _const_bits: &[bool],
        secret_bits: &[bool],
        outs: &[ChainOut],
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<bool>, MpcError> {
        let elim = eliminate_nots(schedule)?;
        let sched = &elim.schedule;
        if feeds.len() != sched.num_inputs || outs.len() != sched.output_wires().len() {
            return Err(MpcError::BadPartition);
        }
        let n_eval = feeds.iter().filter(|f| **f == ChainFeed::Eval).count();
        if secret_bits.len() != n_eval {
            return Err(MpcError::BadPartition);
        }

        let setup_frame =
            SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
        let tables_raw = match setup_frame {
            SessionFrame::SetupStrict { tables } => tables,
            _ => return Err(MpcError::UnexpectedMessage),
        };
        let tables: Vec<GarbleTable<N>> = tables_raw
            .into_iter()
            .map(|rows| {
                let mut t: [Array<u8, N>; 4] = Default::default();
                for (r, row) in rows.iter().enumerate() {
                    t[r] = vec_to_arr(row).ok_or(MpcError::MalformedSchedule)?;
                }
                Ok(GarbleTable { table: t })
            })
            .collect::<Result<Vec<_>, MpcError>>()?;

        let owned_frame =
            SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
        let owned: Vec<Eval<N>> = match owned_frame {
            SessionFrame::OwnedInputs(labels) => labels
                .iter()
                .map(|l| {
                    Ok(Eval {
                        target: vec_to_arr(l).ok_or(MpcError::MalformedSchedule)?,
                    })
                })
                .collect::<Result<Vec<_>, MpcError>>()?,
            _ => return Err(MpcError::UnexpectedMessage),
        };

        let mut labels: Vec<Eval<N>> = Vec::with_capacity(sched.num_inputs);
        let mut owned_i = 0usize;
        let mut ev_i = 0usize;
        for feed in feeds.iter() {
            match feed {
                ChainFeed::Const | ChainFeed::Garbler => {
                    let l = owned.get(owned_i).ok_or(MpcError::BadPartition)?;
                    owned_i += 1;
                    labels.push(l.clone());
                }
                ChainFeed::Eval => {
                    let b = secret_bits[ev_i];
                    ev_i += 1;
                    labels.push(Eval {
                        target: ot.receive(b),
                    });
                }
                ChainFeed::Held(slot) => labels.push(self.held_get(*slot)?),
            }
        }

        let setup = DynEvalSetup {
            one_wire: Eval::zero(),
            tables,
            output_label: Garble::zero(),
        };
        let out_labels =
            crate::DynGarbledExec::<N>::eval_labels_multi::<D>(&setup, sched, &labels)?;

        let mut send_labels: Vec<Vec<u8>> = Vec::new();
        for (o, out) in outs.iter().enumerate() {
            match out {
                ChainOut::Reveal => send_labels.push(arr_to_vec(&out_labels[o].target)),
                ChainOut::Hold(slot) => {
                    self.held_set(*slot, out_labels[o].clone());
                }
            }
        }
        transport.send(&SessionFrame::OutputLabels(send_labels).encode());
        let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
        match frame {
            SessionFrame::VerdictBits(bits) => Ok(bits),
            SessionFrame::Verdict(Err(())) => Err(MpcError::DecodeFailure),
            _ => Err(MpcError::UnexpectedMessage),
        }
    }
}
