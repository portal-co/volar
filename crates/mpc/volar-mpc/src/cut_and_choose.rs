//! M6: authenticated PDE — evaluator-side malicious security for cut-and-choose.
//!
//! Cut-and-choose (the M2–M4 machinery) makes a malicious **garbler**
//! detectable: the garbler commits to N garbled copies, the evaluator opens a
//! random subset and checks them. But on its own it does not bind a malicious
//! **evaluator**. This module adds the two evaluator-side guarantees
//! ("authenticated private-data evaluation"):
//!
//! - **Input consistency** — the evaluator's input bit for a wire is ONE global
//!   choice shared across **all** eval copies, delivered by a *batched OT*
//!   ([`BatchOtChannel`]): one choice bit yields that bit's label in every eval
//!   copy. With independent per-copy OTs the evaluator could feed different
//!   inputs to different eval copies and pick whichever output it likes; the
//!   batched OT makes that impossible.
//! - **Output authenticity** — the evaluator reveals the output wire **labels**
//!   it obtained by evaluating; the garbler verifies each is exactly one of the
//!   copy's two valid output labels (matching the committed garbled circuit)
//!   before decoding, so the evaluator cannot report a forged output. Combined
//!   with the cross-copy agreement check, the output is authentic.
//!
//! This is the in-process reference orchestration (the interpreter path); a
//! transport-driven split is a follow-up. It composes the runtime-sized
//! two-party path ([`garble_schedule_dyn`] / [`DynGarbledExec`]).

use alloc::collections::VecDeque;
use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_spec::vole::VoleArray;

use crate::{DynGarbledExec, GateSchedule, InputOwner, MpcError, garble_schedule_dyn};

// ============================================================================
// Batched OT — evaluator input consistency
// ============================================================================

/// A batched OT channel enforcing evaluator **input consistency**: one choice
/// bit delivers that bit's label in **every** eval copy, so the evaluator's
/// input for a wire is a single global choice.
///
/// A production instantiation runs one base OT per wire whose two messages
/// carry the per-copy label encryptions (the evaluator's choice bit is bound to
/// all copies at once). The in-process [`InProcessBatchOt`] is the test driver.
pub trait BatchOtChannel<N: VoleArray<u8>> {
    /// Sender: offer, for one evaluator-input wire, the per-eval-copy label
    /// pairs (`pairs[i]` = the (false, true) labels for eval copy `i`).
    fn send_batch(&mut self, pairs: &[[Array<u8, N>; 2]]);
    /// Receiver: one choice bit, get the per-eval-copy labels for that bit.
    fn receive_batch(&mut self, bit: bool) -> Vec<Array<u8, N>>;
}

/// An in-process [`BatchOtChannel`] (queue of the garbler's offers; the
/// evaluator selects by its choice bit). For tests.
pub struct InProcessBatchOt<N: VoleArray<u8>> {
    queue: VecDeque<Vec<[Array<u8, N>; 2]>>,
}

impl<N: VoleArray<u8>> Default for InProcessBatchOt<N> {
    fn default() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }
}

impl<N: VoleArray<u8>> BatchOtChannel<N> for InProcessBatchOt<N> {
    fn send_batch(&mut self, pairs: &[[Array<u8, N>; 2]]) {
        self.queue.push_back(pairs.to_vec());
    }

    fn receive_batch(&mut self, bit: bool) -> Vec<Array<u8, N>> {
        let pairs = self.queue.pop_front().expect("batch OT: underflow");
        pairs
            .into_iter()
            .map(|pair| pair[bit as usize].clone())
            .collect()
    }
}

// ============================================================================
// Errors
// ============================================================================

/// A cut-and-choose failure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CutAndChooseError {
    /// A check copy failed to verify against its commitment (malicious garbler).
    CheckFailed(usize),
    /// An eval copy's revealed output label was not a valid output label
    /// (malicious evaluator forging an output).
    InvalidOutputLabel {
        /// Which eval copy.
        copy: usize,
        /// Which output wire.
        wire: usize,
    },
    /// The eval copies' decoded outputs disagree.
    OutputDisagree,
    /// An underlying MPC error.
    Mpc(MpcError),
}

impl From<MpcError> for CutAndChooseError {
    fn from(e: MpcError) -> Self {
        CutAndChooseError::Mpc(e)
    }
}

// ============================================================================
// Deterministic garbling + commitments
// ============================================================================

/// Derive a label base from a per-copy seed and a tag.
fn derive_label<N: VoleArray<u8>, D: Digest>(seed: &[u8; 16], tag: &[u8]) -> Array<u8, N> {
    let mut h = D::new();
    h.update(seed);
    h.update(tag);
    let d = h.finalize();
    let mut a = Array::<u8, N>::default();
    let n = a.len();
    a.copy_from_slice(&d[..n]);
    a
}

/// Garble one copy of `schedule` deterministically from a per-copy `seed` (so a
/// check copy can be re-garbled and verified against its commitment).
pub fn garble_copy<N, D>(
    schedule: &GateSchedule,
    seed: &[u8; 16],
) -> Result<DynGarbledExec<N>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let mut delta = derive_label::<N, D>(seed, b"cut-and-choose/delta");
    delta[0] |= 1; // free-XOR point-and-permute convention.
    let secret = GlobalSecret::new(delta);
    let input_labels: Vec<Garble<N>> = (0..schedule.num_inputs)
        .map(|i| Garble {
            base: derive_label::<N, D>(seed, &u64::try_from(i).unwrap_or(0).to_le_bytes()),
        })
        .collect();
    garble_schedule_dyn::<N, D>(schedule, secret, input_labels)
}

/// Commit to a garbled copy: H(tables ‖ input-label bases ‖ output-label bases).
pub fn commit_copy<N: VoleArray<u8>, D: Digest>(exec: &DynGarbledExec<N>) -> Vec<u8> {
    let mut h = D::new();
    for t in &exec.circuit.tables {
        for row in &t.table {
            h.update(&row[..]);
        }
    }
    for l in &exec.circuit.input_labels {
        h.update(&l.base[..]);
    }
    for l in &exec.output_labels {
        h.update(&l.base[..]);
    }
    h.finalize().to_vec()
}

/// Re-garble a check copy from its seed and verify it against the commitment.
pub fn verify_check_copy<N: VoleArray<u8>, D: Digest>(
    schedule: &GateSchedule,
    seed: &[u8; 16],
    commitment: &[u8],
) -> bool
where
    N: VoleArray<u8>,
{
    match garble_copy::<N, D>(schedule, seed) {
        Ok(re) => commit_copy::<N, D>(&re) == commitment,
        Err(_) => false,
    }
}

// ============================================================================
// Output authenticity
// ============================================================================

/// Verify the evaluator's revealed output labels for one eval copy and decode
/// them. Each revealed label must be **exactly** one of the copy's two valid
/// output labels (the false base or the true label), else the evaluator is
/// forging an output. Returns the decoded bits on success.
pub fn verify_output_labels<N: VoleArray<u8>>(
    exec: &DynGarbledExec<N>,
    revealed: &[Eval<N>],
) -> Option<Vec<bool>> {
    let mut out = Vec::with_capacity(revealed.len());
    for (w, label) in revealed.iter().enumerate() {
        let base = exec
            .output_labels
            .get(w)
            .unwrap_or(&exec.circuit.output_label);
        let false_l = exec.circuit.secret.encode(base, false);
        let true_l = exec.circuit.secret.encode(base, true);
        if label.target == false_l.target {
            out.push(false);
        } else if label.target == true_l.target {
            out.push(true);
        } else {
            return None;
        }
    }
    Some(out)
}

// ============================================================================
// Orchestration (in-process)
// ============================================================================

/// Run the full cut-and-choose protocol in-process with M6 evaluator-side
/// security. `seeds` are the per-copy garbler seeds (`seeds.len()` copies);
/// `eval_set` is the eval-copy subset (the rest are opened and checked). The
/// evaluator's input is delivered by the batched OT (consistent across eval
/// copies); the eval copies' outputs are verified for authenticity and
/// agreement. Returns the circuit output bits.
pub fn run_cut_and_choose<N, D, OT: BatchOtChannel<N>>(
    schedule: &GateSchedule,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    evaluator_bits: &[bool],
    seeds: &[[u8; 16]],
    eval_set: &[usize],
    batch_ot: &mut OT,
) -> Result<Vec<bool>, CutAndChooseError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let total = seeds.len();
    if partition.len() != schedule.num_inputs
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Public)
            .count()
            != public_bits.len()
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Garbler)
            .count()
            != garbler_bits.len()
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Evaluator)
            .count()
            != evaluator_bits.len()
    {
        return Err(CutAndChooseError::Mpc(MpcError::BadPartition));
    }
    let is_eval: Vec<bool> = (0..total).map(|i| eval_set.contains(&i)).collect();

    // 1. Garble + commit to every copy.
    let copies: Vec<DynGarbledExec<N>> = seeds
        .iter()
        .map(|s| garble_copy::<N, D>(schedule, s))
        .collect::<Result<_, _>>()?;
    let commitments: Vec<Vec<u8>> = copies.iter().map(commit_copy::<N, D>).collect();

    // 2. Open + verify the check copies.
    for i in 0..total {
        if !is_eval[i] && !verify_check_copy::<N, D>(schedule, &seeds[i], &commitments[i]) {
            return Err(CutAndChooseError::CheckFailed(i));
        }
    }

    // 3. Input consistency: one batched OT per evaluator-input wire, delivering
    //    the shared choice bit's label in every eval copy.
    let eval_wires: Vec<usize> = (0..schedule.num_inputs)
        .filter(|&w| partition[w] == InputOwner::Evaluator)
        .collect();
    let mut batch_labels: Vec<Vec<Array<u8, N>>> = Vec::with_capacity(eval_wires.len());
    for (ew, &w) in eval_wires.iter().enumerate() {
        let pairs: Vec<[Array<u8, N>; 2]> = eval_set
            .iter()
            .map(|&i| {
                let base = &copies[i].circuit.input_labels[w];
                let f = copies[i].circuit.secret.encode(base, false);
                let t = copies[i].circuit.secret.encode(base, true);
                [f.target, t.target]
            })
            .collect();
        batch_ot.send_batch(&pairs);
        batch_labels.push(batch_ot.receive_batch(evaluator_bits[ew]));
    }

    // 4. Evaluate each eval copy; 5. verify + decode its output (authenticity).
    let mut agreed: Option<Vec<bool>> = None;
    for (j, &i) in eval_set.iter().enumerate() {
        let mut labels: Vec<Eval<N>> = Vec::with_capacity(schedule.num_inputs);
        let mut pub_i = 0usize;
        let mut gb_i = 0usize;
        let mut ev_i = 0usize;
        for (w, owner) in partition.iter().enumerate() {
            let base = &copies[i].circuit.input_labels[w];
            match owner {
                InputOwner::Public => {
                    labels.push(copies[i].circuit.secret.encode(base, public_bits[pub_i]));
                    pub_i += 1;
                }
                InputOwner::Garbler => {
                    labels.push(copies[i].circuit.secret.encode(base, garbler_bits[gb_i]));
                    gb_i += 1;
                }
                InputOwner::Evaluator => {
                    labels.push(Eval {
                        target: batch_labels[ev_i][j].clone(),
                    });
                    ev_i += 1;
                }
            }
        }
        let setup = copies[i].circuit.eval_setup();
        let out_labels = DynGarbledExec::<N>::eval_labels_multi::<D>(&setup, schedule, &labels)?;
        let decoded = verify_output_labels(&copies[i], &out_labels).ok_or(
            CutAndChooseError::InvalidOutputLabel {
                copy: i,
                wire: usize::MAX,
            },
        )?;
        match &agreed {
            None => agreed = Some(decoded),
            Some(a) if *a == decoded => {}
            Some(_) => return Err(CutAndChooseError::OutputDisagree),
        }
    }
    agreed.ok_or(CutAndChooseError::OutputDisagree)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Gate;
    use hybrid_array::typenum::U16;
    use sha2::Sha256;

    type N = U16;
    type D = Sha256;

    fn four_input_schedule() -> GateSchedule {
        GateSchedule {
            num_inputs: 4,
            gates: alloc::vec![
                Gate::And(0, 1),
                Gate::Xor(2, 3),
                Gate::Not(5),
                Gate::And(4, 6),
                Gate::Xor(7, 2),
            ],
            output: 8,
            outputs: None,
            actions: Vec::new(),
            storages: alloc::vec![],
        }
    }

    fn eval_concrete(schedule: &GateSchedule, inputs: &[bool]) -> bool {
        let mut wires = inputs.to_vec();
        for g in &schedule.gates {
            let v = match g {
                Gate::Zero => false,
                Gate::One => true,
                Gate::Xor(a, b) => wires[*a] ^ wires[*b],
                Gate::And(a, b) => wires[*a] & wires[*b],
                Gate::Not(a) => !wires[*a],
                _ => panic!("storage gate in pure circuit"),
            };
            wires.push(v);
        }
        wires[schedule.output]
    }

    /// Honest cut-and-choose over all 2^4 inputs: the M6 orchestration (batched
    /// OT for input consistency + output-label verification) matches concrete
    /// eval, with both parties holding private inputs.
    #[test]
    fn cut_and_choose_honest_all_inputs() {
        let schedule = four_input_schedule();
        let partition = [
            InputOwner::Public,
            InputOwner::Garbler,
            InputOwner::Evaluator,
            InputOwner::Evaluator,
        ];
        let seeds = [[0x11; 16], [0x22; 16], [0x33; 16], [0x44; 16]];
        let eval_set = [1usize, 3usize];
        for bits in 0u32..16 {
            let b = [
                (bits >> 0) & 1 == 1,
                (bits >> 1) & 1 == 1,
                (bits >> 2) & 1 == 1,
                (bits >> 3) & 1 == 1,
            ];
            let public = [b[0]];
            let garbler = [b[1]];
            let evaluator = [b[2], b[3]];
            let mut ot = InProcessBatchOt::<N>::default();
            let out = run_cut_and_choose::<N, D, _>(
                &schedule, &partition, &public, &garbler, &evaluator, &seeds, &eval_set, &mut ot,
            )
            .expect("honest cut-and-choose");
            assert_eq!(
                out,
                alloc::vec![eval_concrete(&schedule, &b)],
                "inputs {b:?}"
            );
        }
    }

    /// The batched OT delivers the evaluator's single choice bit's label in
    /// EVERY eval copy (input consistency): the two eval copies yield the same
    /// chosen bit's labels, so the evaluator cannot split its input.
    #[test]
    fn batched_ot_binds_choice_across_copies() {
        let schedule = four_input_schedule();
        let seeds = [[0x11; 16], [0x22; 16], [0x33; 16], [0x44; 16]];
        let copies: Vec<_> = seeds
            .iter()
            .map(|s| garble_copy::<N, D>(&schedule, s).unwrap())
            .collect();
        // Wire 2 is evaluator-owned; offer the per-copy pairs for eval copies 1, 3.
        let w = 2;
        let eval_set = [1usize, 3usize];
        let pairs: Vec<[Array<u8, N>; 2]> = eval_set
            .iter()
            .map(|&i| {
                let base = &copies[i].circuit.input_labels[w];
                let f = copies[i].circuit.secret.encode(base, false);
                let t = copies[i].circuit.secret.encode(base, true);
                [f.target, t.target]
            })
            .collect();
        let mut ot = InProcessBatchOt::<N>::default();
        ot.send_batch(&pairs);
        // Choosing bit=1 yields the true label in BOTH copies.
        let got = ot.receive_batch(true);
        for (k, &i) in eval_set.iter().enumerate() {
            let base = &copies[i].circuit.input_labels[w];
            let t = copies[i].circuit.secret.encode(base, true);
            assert_eq!(got[k], t.target, "copy {i} must get the chosen bit's label");
        }
    }

    /// Output authenticity: a forged output label (not one of the copy's two
    /// valid output labels) is rejected.
    #[test]
    fn forged_output_label_rejected() {
        let schedule = four_input_schedule();
        let exec = garble_copy::<N, D>(&schedule, &[0x77; 16]).unwrap();
        // A forged label: neither the false base nor the true label.
        let forged = Eval {
            target: derive_label::<N, D>(&[0x99; 16], b"forged"),
        };
        assert_eq!(verify_output_labels(&exec, &[forged]), None);
        // The genuine true label is accepted and decodes to true.
        let base = &exec.output_labels[0];
        let true_l = exec.circuit.secret.encode(base, true);
        assert_eq!(
            verify_output_labels(&exec, &[true_l]),
            Some(alloc::vec![true])
        );
    }

    /// A corrupted check copy (commitment mismatch) is caught.
    #[test]
    fn corrupted_check_copy_caught() {
        let schedule = four_input_schedule();
        let exec = garble_copy::<N, D>(&schedule, &[0x55; 16]).unwrap();
        let mut bad_commit = commit_copy::<N, D>(&exec);
        bad_commit[0] ^= 1;
        assert!(!verify_check_copy::<N, D>(
            &schedule,
            &[0x55; 16],
            &bad_commit
        ));
        // The honest commitment verifies.
        let good_commit = commit_copy::<N, D>(&exec);
        assert!(verify_check_copy::<N, D>(
            &schedule,
            &[0x55; 16],
            &good_commit
        ));
    }
}
