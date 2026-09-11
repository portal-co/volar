// @reliability: experimental
// @ai: assisted
//! Two-party driver for an [`OramProgram`] (symbolic-ORAM S4): run the
//! per-access segments produced by [`crate::oram_lower::storage_to_oram`]
//! **two-party**, threading the guest tape and the ORAM client state (position
//! map + stash) as garbled re-based labels that are never decoded to cleartext.
//!
//! This is the general form of the mechanism pinned by
//! `tests/oram_threaded.rs`, lifted to a whole `OramProgram`:
//!
//! - The **guest tape** (live guest values) threads between `Compute` segments
//!   and is *patched* by each read `Access` at its result slot.
//! - The **ORAM state** (posmap + stash) threads across every access, persisting
//!   in the driver so it also threads across *steps* (the loop-guest work).
//! - The only values revealed per access are the oblivious `old_leaf` plus the
//!   physical path bits the evaluator writes back into its own tree; the
//!   logical address and read/write data stay on the (secret) tape.
//!
//! Runtime-sized: uses [`volar_mpc::garble_schedule_dyn`] /
//! [`volar_mpc::DynGarbledExec`], so the per-stage circuits may differ in input
//! and AND counts (a const-generic driver could not mix them).
//!
//! Scope / honesty: single-process harness drives both parties over a loopback
//! OT (mechanism and label-consistency are pinned, not transport). Plaintext
//! tree is the S1–S4 scaffold; the encrypted tree is S5.

use alloc::vec::Vec;

use hybrid_array::Array;
use volar_mpc::ot::LoopbackOt;
use volar_mpc::{DynGarbledExec, GateSchedule, OtChannel, garble_schedule_dyn};
use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};
use digest::Digest;
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_spec::vole::VoleArray;

use crate::oram_gadget::OramGadgetConfig;
use crate::oram_lower::{OramProgram, Stage};

/// A bundle of threaded wires: the evaluator holds `labels`, the garbler knows
/// the matching false-label `bases`. The two describe the same secret bits;
/// neither party alone can decode them.
#[derive(Clone)]
pub struct HeldState<N: VoleArray<u8>> {
    /// Evaluator-held labels, one per wire.
    pub labels: Vec<Eval<N>>,
    /// Garbler-known false-label bases, one per wire (parallel to `labels`).
    pub bases: Vec<Garble<N>>,
}

impl<N: VoleArray<u8>> HeldState<N> {
    /// An all-`bit` state of `width` wires with fresh bases.
    fn constant<D: Digest>(
        width: usize,
        bit: bool,
        secret: &GlobalSecret<N>,
        fresh: &mut u64,
    ) -> Self {
        let bases: Vec<Garble<N>> = (0..width)
            .map(|_| {
                *fresh += 1;
                fresh_base::<N, D>(*fresh)
            })
            .collect();
        let labels = bases.iter().map(|b| secret.encode(b, bit)).collect();
        HeldState { labels, bases }
    }
}

/// A fresh input false-label base (garbler-picked).
fn fresh_base<N: VoleArray<u8>, D: Digest>(counter: u64) -> Garble<N> {
    let h = D::digest(&[b"oram2pc-base".as_slice(), &counter.to_le_bytes()].concat());
    let h = h.as_slice();
    Garble {
        base: Array::from_fn(|i| h[i % h.len()]),
    }
}

/// Decode a reveal output label against its base.
fn reveal<N: VoleArray<u8>>(label: &Eval<N>, base: &Garble<N>) -> bool {
    label.open(base)[0] & 1 != 0
}

/// How each input bit of one circuit invocation is supplied.
enum Feed {
    /// Public bit, encoded locally by both parties.
    Const(bool),
    /// Garbler's private fresh bit (e.g. the new leaf).
    Garbler(bool),
    /// Evaluator's private bit, delivered by 1-of-2 OT (the physical path).
    Eval(bool),
    /// Reuse the tape wire at this slot.
    Tape(usize),
    /// Reuse the posmap wire at this offset.
    Posmap(usize),
    /// Reuse the stash wire at this offset.
    Stash(usize),
}

/// Run one circuit two-party with a mix of fresh and threaded inputs. Free
/// function over disjoint borrows so the driver can pass `&self.begin_sched`
/// alongside `&mut self.fresh`. Returns the evaluator's output labels and the
/// garbler's per-output false-label bases.
#[allow(clippy::too_many_arguments)]
fn run_circuit<N: VoleArray<u8>, D: Digest>(
    secret: &GlobalSecret<N>,
    tape: &HeldState<N>,
    posmap: &HeldState<N>,
    stash: &HeldState<N>,
    fresh: &mut u64,
    schedule: &GateSchedule,
    feeds: &[Feed],
    ot: &mut LoopbackOt<N>,
) -> (Vec<Eval<N>>, Vec<Garble<N>>) {
    assert_eq!(feeds.len(), schedule.num_inputs, "feed width");
    // Garbler: align threaded input bases to the held bases; fresh otherwise.
    let input_bases: Vec<Garble<N>> = feeds
        .iter()
        .map(|f| match f {
            Feed::Tape(s) => tape.bases[*s].clone(),
            Feed::Posmap(o) => posmap.bases[*o].clone(),
            Feed::Stash(o) => stash.bases[*o].clone(),
            Feed::Const(_) | Feed::Garbler(_) | Feed::Eval(_) => {
                *fresh += 1;
                fresh_base::<N, D>(*fresh)
            }
        })
        .collect();
    let exec =
        garble_schedule_dyn::<N, D>(schedule, secret.clone(), input_bases).expect("garbles");
    let setup = exec.circuit.eval_setup();
    // Evaluator: threaded wires reuse the held label; fresh wires are encoded
    // (public/garbler) or OT-delivered (evaluator).
    let labels: Vec<Eval<N>> = feeds
        .iter()
        .enumerate()
        .map(|(i, f)| match f {
            Feed::Const(b) | Feed::Garbler(b) => secret.encode(&exec.circuit.input_labels[i], *b),
            Feed::Eval(b) => {
                let f_l = secret.encode(&exec.circuit.input_labels[i], false);
                let t_l = secret.encode(&exec.circuit.input_labels[i], true);
                ot.send([&f_l.target, &t_l.target]);
                Eval {
                    target: ot.receive(*b),
                }
            }
            Feed::Tape(s) => tape.labels[*s].clone(),
            Feed::Posmap(o) => posmap.labels[*o].clone(),
            Feed::Stash(o) => stash.labels[*o].clone(),
        })
        .collect();
    let out_labels = DynGarbledExec::<N>::eval_labels_multi::<D>(&setup, schedule, &labels)
        .expect("evals");
    (out_labels, exec.output_labels)
}

/// Two-party `OramProgram` driver. Holds the persistent ORAM state (position
/// map + stash + access counter) plus the current guest tape, all as threaded
/// labels. Reused across steps so the ORAM state threads through a whole loop.
pub struct Oram2pc<N: VoleArray<u8>> {
    secret: GlobalSecret<N>,
    posmap: HeldState<N>,
    stash: HeldState<N>,
    tape: HeldState<N>,
    counter: u64,
    fresh: u64,
    leaf_rng: u64,
    // Compiled schedules, cached so repeated begin/access invocations don't
    // recompile. (Compute-segment schedules are compiled per use; small.)
    begin_sched: GateSchedule,
    access_sched: GateSchedule,
}

fn splitmix_next(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

impl<N: VoleArray<u8>> Oram2pc<N> {
    /// Bootstrap a driver for `program` with an all-zero ORAM state and an
    /// all-zero tape (slots are filled per step by [`Self::run_program`]).
    pub fn new<D: Digest>(program: &OramProgram, secret: GlobalSecret<N>) -> Self {
        let cfg = &program.oram;
        let lb = cfg.leaf_bits();
        let eb = cfg.entry_bits();
        let mut fresh = 0u64;
        let posmap = HeldState::constant::<D>(cfg.num_addrs * lb, false, &secret, &mut fresh);
        let stash = HeldState::constant::<D>(cfg.max_stash * eb, false, &secret, &mut fresh);
        let tape = HeldState::constant::<D>(program.tape_width, false, &secret, &mut fresh);
        let begin_sched = crate::compile_schedule(&program.begin).expect("begin schedules");
        let access_sched = crate::compile_schedule(&program.access).expect("access schedules");
        Oram2pc {
            secret,
            posmap,
            stash,
            tape,
            counter: 0,
            fresh,
            leaf_rng: 0x5EED,
            begin_sched,
            access_sched,
        }
    }

    /// Run one ORAM access (begin → main → evict) against `tree`, threading the
    /// posmap/stash and patching the tape's `result_slot` with the read data.
    #[allow(clippy::too_many_arguments)]
    fn run_access<D: Digest, const Z: usize>(
        &mut self,
        cfg: &OramGadgetConfig,
        write: bool,
        addr_slots: &[usize],
        wdata_slot: usize,
        result_slot: Option<usize>,
        tree: &mut OramTree<Z, 1>,
        ot: &mut LoopbackOt<N>,
    ) {
        let (ab, lb, db, eb) = (
            cfg.addr_bits(),
            cfg.leaf_bits(),
            cfg.data_bits,
            cfg.entry_bits(),
        );
        let n_path = cfg.levels * Z;
        let num_leaves = cfg.num_leaves() as u64;
        debug_assert_eq!(db, 1, "bit-level storage");
        debug_assert_eq!(addr_slots.len(), ab);

        let new_leaf = splitmix_next(&mut self.leaf_rng) % num_leaves;

        // --- begin: posmap.update(addr, new_leaf) -> old_leaf, new_posmap ---
        let mut feeds: Vec<Feed> = (0..cfg.num_addrs * lb).map(Feed::Posmap).collect();
        feeds.extend(addr_slots.iter().map(|&s| Feed::Tape(s)));
        feeds.extend(enc(new_leaf, lb).into_iter().map(Feed::Garbler));
        let (bl, bb) = run_circuit::<N, D>(
            &self.secret,
            &self.tape,
            &self.posmap,
            &self.stash,
            &mut self.fresh,
            &self.begin_sched,
            &feeds,
            ot,
        );
        let old_leaf = dec(
            &bl[..lb]
                .iter()
                .zip(&bb[..lb])
                .map(|(l, b)| reveal(l, b))
                .collect::<Vec<_>>(),
        );
        self.posmap = HeldState {
            labels: bl[lb..].to_vec(),
            bases: bb[lb..].to_vec(),
        };

        // --- extern + main access (evict_only = 0) ---
        let main_path = tree.read_path(old_leaf);
        let path_bits = flatten_path::<Z>(&main_path, cfg);
        let mut feeds: Vec<Feed> = (0..cfg.max_stash * eb).map(Feed::Stash).collect();
        feeds.extend(path_bits.iter().copied().map(Feed::Eval));
        feeds.extend(addr_slots.iter().map(|&s| Feed::Tape(s)));
        feeds.push(Feed::Const(write));
        feeds.push(if write {
            Feed::Tape(wdata_slot)
        } else {
            Feed::Const(false)
        });
        feeds.extend(enc(old_leaf, lb).into_iter().map(Feed::Const));
        feeds.extend(enc(new_leaf, lb).into_iter().map(Feed::Garbler));
        feeds.push(Feed::Const(false)); // evict_only
        let (al, ab_) = run_circuit::<N, D>(
            &self.secret,
            &self.tape,
            &self.posmap,
            &self.stash,
            &mut self.fresh,
            &self.access_sched,
            &feeds,
            ot,
        );
        assert!(!reveal(&al[0], &ab_[0]), "ORAM stash overflow");
        let np_off = 1 + db;
        let new_path_bits: Vec<bool> = (0..n_path * eb)
            .map(|k| reveal(&al[np_off + k], &ab_[np_off + k]))
            .collect();
        tree.write_path(old_leaf, &unflatten_path::<Z>(&new_path_bits, cfg));
        // Patch the tape's result slot with the (threaded) read-data wire.
        if let Some(slot) = result_slot {
            self.tape.labels[slot] = al[1].clone();
            self.tape.bases[slot] = ab_[1].clone();
        }
        let stash_off = np_off + n_path * eb;
        self.stash = HeldState {
            labels: al[stash_off..].to_vec(),
            bases: ab_[stash_off..].to_vec(),
        };

        // --- deterministic eviction (one pass, skipped on collision) ---
        let evict_leaf = eviction_target(self.counter, num_leaves);
        self.counter += 1;
        if evict_leaf != old_leaf {
            let epath = tree.read_path(evict_leaf);
            let epath_bits = flatten_path::<Z>(&epath, cfg);
            let mut feeds: Vec<Feed> = (0..cfg.max_stash * eb).map(Feed::Stash).collect();
            feeds.extend(epath_bits.iter().copied().map(Feed::Eval));
            feeds.extend((0..ab).map(|_| Feed::Const(false))); // dummy addr
            feeds.push(Feed::Const(false)); // op_write
            feeds.push(Feed::Const(false)); // dummy wdata
            feeds.extend(enc(evict_leaf, lb).into_iter().map(Feed::Const));
            feeds.extend(enc(0, lb).into_iter().map(Feed::Const)); // dummy new_leaf
            feeds.push(Feed::Const(true)); // evict_only
            let (el, eb_) = run_circuit::<N, D>(
                &self.secret,
                &self.tape,
                &self.posmap,
                &self.stash,
                &mut self.fresh,
                &self.access_sched,
                &feeds,
                ot,
            );
            assert!(!reveal(&el[0], &eb_[0]), "ORAM stash overflow (evict)");
            let np_off = 1 + db;
            let new_epath_bits: Vec<bool> = (0..n_path * eb)
                .map(|k| reveal(&el[np_off + k], &eb_[np_off + k]))
                .collect();
            tree.write_path(evict_leaf, &unflatten_path::<Z>(&new_epath_bits, cfg));
            let stash_off = np_off + n_path * eb;
            self.stash = HeldState {
                labels: el[stash_off..].to_vec(),
                bases: eb_[stash_off..].to_vec(),
            };
        }
    }

    /// Run one `OramProgram` (one step) two-party. `param_inputs` supplies the
    /// guest params as `(evaluator label, garbler base)` pairs — fresh-encoded
    /// for a single-shot program (see [`Self::fresh_input`]), threaded from the
    /// previous step's outputs for a loop. Returns the `(label, base)` pair for
    /// each output slot; the caller reveals the public ones (done flag, result)
    /// and threads the loop state.
    pub fn run_program<D: Digest, const Z: usize>(
        &mut self,
        program: &OramProgram,
        param_inputs: &[(Eval<N>, Garble<N>)],
        tree: &mut OramTree<Z, 1>,
        ot: &mut LoopbackOt<N>,
    ) -> Vec<(Eval<N>, Garble<N>)> {
        assert_eq!(
            param_inputs.len(),
            program.input_slots.len(),
            "param count"
        );
        // Init the tape: params into their slots, everything else fresh-zero.
        let mut tape =
            HeldState::constant::<D>(program.tape_width, false, &self.secret, &mut self.fresh);
        for (i, &(ref l, ref b)) in param_inputs.iter().enumerate() {
            tape.labels[program.input_slots[i]] = l.clone();
            tape.bases[program.input_slots[i]] = b.clone();
        }
        self.tape = tape;

        for stage in &program.stages {
            match stage {
                Stage::Compute(circuit) => {
                    let sched = crate::compile_schedule(circuit).expect("segment schedules");
                    let feeds: Vec<Feed> = (0..program.tape_width).map(Feed::Tape).collect();
                    let (ol, ob) = run_circuit::<N, D>(
                        &self.secret,
                        &self.tape,
                        &self.posmap,
                        &self.stash,
                        &mut self.fresh,
                        &sched,
                        &feeds,
                        ot,
                    );
                    self.tape = HeldState {
                        labels: ol,
                        bases: ob,
                    };
                }
                Stage::Access(info) => {
                    self.run_access::<D, Z>(
                        &program.oram,
                        info.write,
                        &info.addr_slots,
                        info.wdata_slot,
                        info.result_slot,
                        tree,
                        ot,
                    );
                }
            }
        }

        program
            .output_slots
            .iter()
            .map(|&s| (self.tape.labels[s].clone(), self.tape.bases[s].clone()))
            .collect()
    }

    /// Build a fresh-encoded `(label, base)` pair for a single guest param bit —
    /// the single-shot (non-looping) way to feed inputs to [`Self::run_program`].
    pub fn fresh_input<D: Digest>(&mut self, bit: bool) -> (Eval<N>, Garble<N>) {
        self.fresh += 1;
        let base = fresh_base::<N, D>(self.fresh);
        (self.secret.encode(&base, bit), base)
    }
}

// --- bit helpers (LSB-first, matching the gadget) --------------------------
fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}

fn flatten_path<const Z: usize>(path: &[Bucket<Z, 1>], cfg: &OramGadgetConfig) -> Vec<bool> {
    let mut v = Vec::new();
    for bucket in path {
        for e in &bucket.entries {
            let real = e.is_real();
            v.push(real);
            v.extend(enc(if real { e.addr } else { 0 }, cfg.addr_bits()));
            v.extend(enc(if real { e.leaf } else { 0 }, cfg.leaf_bits()));
            v.push(if real { e.data[0] & 1 == 1 } else { false });
        }
    }
    v
}
fn unflatten_path<const Z: usize>(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, 1>> {
    let eb = cfg.entry_bits();
    let mut out = Vec::new();
    for level in 0..cfg.levels {
        let mut entries = [OramEntry::dummy(); Z];
        for (s, slot) in entries.iter_mut().enumerate() {
            let k = level * Z + s;
            let e = &bits[k * eb..(k + 1) * eb];
            *slot = if e[0] {
                OramEntry {
                    addr: dec(&e[1..1 + cfg.addr_bits()]),
                    leaf: dec(&e[1 + cfg.addr_bits()..1 + cfg.addr_bits() + cfg.leaf_bits()]),
                    data: [dec(&e[1 + cfg.addr_bits() + cfg.leaf_bits()..]) as u8],
                }
            } else {
                OramEntry::dummy()
            };
        }
        out.push(Bucket { entries });
    }
    out
}
