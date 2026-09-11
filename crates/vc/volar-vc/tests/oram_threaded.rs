// @reliability: experimental
// @ai: assisted
//! S4: ORAM **state threaded across accesses as re-based labels, never decoded
//! to cleartext**, two-party. Builds on S2 (`oram_two_party.rs`), which threaded
//! the position map and stash through the trusted harness as decoded public
//! inputs — a correctness scaffold. Here the posmap/stash are *secret*: they
//! cross begin/access circuit invocations only as garbled labels.
//!
//! # The re-basing mechanism
//!
//! Free-XOR gives every wire a label `base ^ (v·Δ)` with a single global `Δ`.
//! To thread a wire from circuit A's output to circuit B's input **without
//! decoding it**, the garbler simply *aligns bases*: it garbles B with B's
//! input base for that wire set equal to A's output base (`garble_schedule`
//! takes the input bases, so the driver controls this). The evaluator, holding
//! A's output label `L = base ^ (v·Δ)`, then uses `L` directly as B's input
//! label — no translation offset, no decode, and `v` stays secret from both
//! parties. The driver assembles B's input labels itself: threaded wires reuse
//! the held label, fresh secret wires are encoded, the evaluator's physical
//! path is delivered by OT.
//!
//! What is *revealed* per access is only the oblivious `old_leaf` (a uniform
//! random remapped leaf) plus the physical path bits the evaluator writes back
//! into its own tree. The posmap and stash — whose secrecy is what stops the
//! evaluator inverting `old_leaf = posmap[addr]` to recover the logical
//! address — are never decoded.
//!
//! Scope / honesty: single-process harness drives both parties over a loopback
//! OT (the mechanism and label-consistency are what's pinned, not transport).
//! Plaintext tree remains the S1–S4 correctness scaffold; the encrypted tree
//! that hides block contents from the evaluator is S5.

use hybrid_array::Array;
use sha2::{Digest, Sha256};
use typenum::U16;
use volar_mpc::ot::LoopbackOt;
use volar_mpc::{GarbledExec, OtChannel, garble_schedule};
use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin};
use volar_vc::compile_schedule;

type N = U16;
type D = Sha256;

// S2 geometry (counts probed from the compiled schedules).
const NUM_ADDRS: usize = 4;
const LEVELS: usize = 3;
const Z: usize = 2;
const B: usize = 1;
const MAX_STASH: usize = 8;
const BEGIN_I: usize = 12;
const BEGIN_A: usize = 20;
const ACCESS_I: usize = 198;
const ACCESS_A: usize = 1999;

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: NUM_ADDRS,
        levels: LEVELS,
        bucket_size: Z,
        data_bits: B * 8,
        max_stash: MAX_STASH,
        encrypted: false,
        tree_key_bits: 0,
        keyed_leaf: false,
    }
}

fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}

fn flatten_path(path: &[Bucket<Z, B>], cfg: &OramGadgetConfig) -> Vec<bool> {
    let mut v = Vec::new();
    for bucket in path {
        for e in &bucket.entries {
            let real = e.is_real();
            v.push(real);
            v.extend(enc(if real { e.addr } else { 0 }, cfg.addr_bits()));
            v.extend(enc(if real { e.leaf } else { 0 }, cfg.leaf_bits()));
            v.extend(enc(if real { e.data[0] as u64 } else { 0 }, cfg.data_bits));
        }
    }
    v
}
fn unflatten_path(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, B>> {
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

/// A fresh input false-label base (garbler-picked, deterministic for the test).
fn fresh_base(counter: u64) -> Garble<N> {
    let h = Sha256::digest([b"s4-base".as_slice(), &counter.to_le_bytes()].concat());
    Garble {
        base: Array::clone_from_slice(&h[..16]),
    }
}

/// A bundle of threaded state wires: the evaluator holds `labels`, the garbler
/// knows the matching false-label `bases`. The two describe the same secret
/// bits; neither party alone can decode them.
struct HeldState {
    labels: Vec<Eval<N>>,
    bases: Vec<Garble<N>>,
}

/// How each input bit of one circuit invocation is supplied.
enum Feed {
    /// Public bit, encoded locally by both parties.
    Const(bool),
    /// Garbler's private bit, encoded by the garbler.
    Garbler(bool),
    /// Evaluator's private bit, delivered by 1-of-2 OT.
    Eval(bool),
    /// Reuse the held label/base at `posmap[offset]` (re-based thread).
    ThreadPosmap(usize),
    /// Reuse the held label/base at `stash[offset]` (re-based thread).
    ThreadStash(usize),
}

/// Run one circuit two-party with a mix of fresh and threaded inputs. Returns
/// the evaluator's output labels and the garbler's per-output false-label bases
/// (so the caller can decode the reveal outputs and thread the state outputs).
fn run_threaded<const I: usize, const A: usize>(
    circuit: &volar_ir::boolar::BIrBlocks,
    feeds: &[Feed],
    posmap: &HeldState,
    stash: &HeldState,
    secret: &GlobalSecret<N>,
    fresh: &mut u64,
    ot: &mut LoopbackOt<N>,
) -> (Vec<Eval<N>>, Vec<Garble<N>>) {
    assert_eq!(feeds.len(), I, "feed width");
    let schedule = compile_schedule(circuit).expect("circuit schedules");
    // Garbler: align threaded input bases to the held state bases; fresh bases
    // for everything else.
    let input_bases: [Garble<N>; I] = core::array::from_fn(|i| match feeds[i] {
        Feed::ThreadPosmap(off) => posmap.bases[off].clone(),
        Feed::ThreadStash(off) => stash.bases[off].clone(),
        _ => {
            *fresh += 1;
            fresh_base(*fresh)
        }
    });
    let exec =
        garble_schedule::<N, D, I, A>(&schedule, secret.clone(), input_bases).expect("garbles");
    let setup = exec.circuit.eval_setup();
    // Evaluator: assemble input labels — threaded wires reuse the held label,
    // fresh wires are encoded (public/garbler) or OT-delivered (evaluator).
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
            Feed::ThreadPosmap(off) => posmap.labels[*off].clone(),
            Feed::ThreadStash(off) => stash.labels[*off].clone(),
        })
        .collect();
    let out_labels =
        GarbledExec::<N, I, A>::eval_labels_multi::<D>(&setup, &schedule, &labels).expect("evals");
    (out_labels, exec.output_labels)
}

/// Decode a reveal output label against its base.
fn reveal(label: &Eval<N>, base: &Garble<N>) -> bool {
    label.open(base)[0] & 1 != 0
}

struct Splitmix(u64);
impl Splitmix {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
}

#[test]
fn s4_oram_state_threaded_two_party() {
    let cfg = cfg();
    let (ab, lb, db, eb) = (
        cfg.addr_bits(),
        cfg.leaf_bits(),
        cfg.data_bits,
        cfg.entry_bits(),
    );
    let n_path = cfg.path_entries();
    let num_leaves = cfg.num_leaves() as u64;
    let begin = build_begin(&cfg);
    let access = build_access(&cfg);
    assert_eq!(begin.blocks[0].params as usize, BEGIN_I);
    assert_eq!(access.blocks[0].params as usize, ACCESS_I);

    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[7u8; 16]));
    let mut ot = LoopbackOt::<N>::new();
    let mut tree = OramTree::<Z, B>::new(LEVELS);
    let mut counter: u64 = 0;
    let mut fresh: u64 = 0;
    let mut leaf_rng = Splitmix(0xdead_beef_cafe_f00d); // garbler's leaf randomness
    let mut model = [0u8; NUM_ADDRS];

    // Bootstrap the secret ORAM state: fresh bases, all-zero labels (empty
    // posmap / stash). The posmap/stash are NEVER decoded after this — they
    // cross accesses only as re-based labels.
    let mut posmap_st = {
        let bases: Vec<Garble<N>> = (0..NUM_ADDRS * lb)
            .map(|_| {
                fresh += 1;
                fresh_base(fresh)
            })
            .collect();
        let labels = bases.iter().map(|b| secret.encode(b, false)).collect();
        HeldState { labels, bases }
    };
    let mut stash_st = {
        let bases: Vec<Garble<N>> = (0..MAX_STASH * eb)
            .map(|_| {
                fresh += 1;
                fresh_base(fresh)
            })
            .collect();
        let labels = bases.iter().map(|b| secret.encode(b, false)).collect();
        HeldState { labels, bases }
    };

    // Ops: (addr, Some(write) / None(read)). Addresses and data are the
    // garbler's private inputs; the tree host (evaluator) never learns them.
    let ops: [(u64, Option<u8>); 5] = [
        (1, Some(0xA5)),
        (1, None),
        (2, Some(0x3C)),
        (1, None),
        (2, None),
    ];

    for (addr, wdata) in ops {
        let new_leaf = leaf_rng.next() % num_leaves; // garbler's fresh leaf

        // --- begin: posmap.update(addr, new_leaf) -> old_leaf, new_posmap ---
        let mut feeds: Vec<Feed> = (0..NUM_ADDRS * lb).map(Feed::ThreadPosmap).collect();
        feeds.extend(enc(addr, ab).into_iter().map(Feed::Garbler));
        feeds.extend(enc(new_leaf, lb).into_iter().map(Feed::Garbler));
        let (bl, bb) = run_threaded::<BEGIN_I, BEGIN_A>(
            &begin, &feeds, &posmap_st, &stash_st, &secret, &mut fresh, &mut ot,
        );
        // Reveal only old_leaf; thread the new posmap.
        let old_leaf = dec(
            &bl[..lb]
                .iter()
                .zip(&bb[..lb])
                .map(|(l, b)| reveal(l, b))
                .collect::<Vec<_>>(),
        );
        posmap_st = HeldState {
            labels: bl[lb..].to_vec(),
            bases: bb[lb..].to_vec(),
        };

        // --- extern: read the main path, then access (main) ---
        let main_path = tree.read_path(old_leaf);
        let path_bits = flatten_path(&main_path, &cfg);
        let mut feeds: Vec<Feed> = (0..MAX_STASH * eb).map(Feed::ThreadStash).collect();
        feeds.extend(path_bits.iter().copied().map(Feed::Eval)); // evaluator's tree
        feeds.extend(enc(addr, ab).into_iter().map(Feed::Garbler));
        feeds.push(Feed::Const(wdata.is_some())); // op_write (public)
        feeds.extend(
            enc(wdata.unwrap_or(0) as u64, db)
                .into_iter()
                .map(Feed::Garbler),
        );
        feeds.extend(enc(old_leaf, lb).into_iter().map(Feed::Const)); // path_leaf (revealed)
        feeds.extend(enc(new_leaf, lb).into_iter().map(Feed::Garbler));
        feeds.push(Feed::Const(false)); // evict_only
        let (al, ab_) = run_threaded::<ACCESS_I, ACCESS_A>(
            &access, &feeds, &posmap_st, &stash_st, &secret, &mut fresh, &mut ot,
        );
        assert!(!reveal(&al[0], &ab_[0]), "ORAM stash overflow");
        let rdata = dec(
            &(1..1 + db)
                .map(|k| reveal(&al[k], &ab_[k]))
                .collect::<Vec<_>>(),
        ) as u8;
        let np_off = 1 + db;
        let new_path_bits: Vec<bool> = (0..n_path * eb)
            .map(|k| reveal(&al[np_off + k], &ab_[np_off + k]))
            .collect();
        tree.write_path(old_leaf, &unflatten_path(&new_path_bits, &cfg));
        let stash_off = np_off + n_path * eb;
        stash_st = HeldState {
            labels: al[stash_off..].to_vec(),
            bases: ab_[stash_off..].to_vec(),
        };

        // --- deterministic eviction pass (skipped on collision) ---
        let evict_leaf = eviction_target(counter, num_leaves);
        counter += 1;
        if evict_leaf != old_leaf {
            let epath = tree.read_path(evict_leaf);
            let epath_bits = flatten_path(&epath, &cfg);
            let mut feeds: Vec<Feed> = (0..MAX_STASH * eb).map(Feed::ThreadStash).collect();
            feeds.extend(epath_bits.iter().copied().map(Feed::Eval));
            feeds.extend(enc(0, ab).into_iter().map(Feed::Const)); // dummy addr
            feeds.push(Feed::Const(false)); // op_write
            feeds.extend(enc(0, db).into_iter().map(Feed::Const)); // dummy wdata
            feeds.extend(enc(evict_leaf, lb).into_iter().map(Feed::Const)); // path_leaf
            feeds.extend(enc(0, lb).into_iter().map(Feed::Const)); // dummy new_leaf
            feeds.push(Feed::Const(true)); // evict_only
            let (el, eb_) = run_threaded::<ACCESS_I, ACCESS_A>(
                &access, &feeds, &posmap_st, &stash_st, &secret, &mut fresh, &mut ot,
            );
            assert!(!reveal(&el[0], &eb_[0]), "ORAM stash overflow (evict)");
            let np_off = 1 + db;
            let new_epath_bits: Vec<bool> = (0..n_path * eb)
                .map(|k| reveal(&el[np_off + k], &eb_[np_off + k]))
                .collect();
            tree.write_path(evict_leaf, &unflatten_path(&new_epath_bits, &cfg));
            let stash_off = np_off + n_path * eb;
            stash_st = HeldState {
                labels: el[stash_off..].to_vec(),
                bases: eb_[stash_off..].to_vec(),
            };
        }

        // Check the read result against the model.
        match wdata {
            Some(w) => {
                model[addr as usize] = w;
            }
            None => {
                let want = model[addr as usize];
                assert_eq!(rdata, want, "read addr {addr}: got {rdata:#x}, want {want:#x}");
            }
        }
    }
}
