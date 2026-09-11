// @reliability: experimental
// @ai: assisted
//! S5c follow-up: the recursive position map **two-party, with garbled
//! intermediates**. The S5c base (`oram_recursive.rs`) demonstrates the
//! recursion concretely, decoding the intermediate posmap blocks in the trusted
//! harness. Here the intermediates stay **garbled**: a posmap block holds `c`
//! addresses' leaves, so it cannot be revealed — the level-below access returns
//! it as re-based labels, [`build_extract`] selects the accessed entry's leaf
//! in-circuit (only that leaf is revealed: it is the physical path), and
//! [`build_update`] writes the remapped leaf back — so the tree-hosting
//! evaluator never learns any *other* address's leaf.
//!
//! Composes the proven pieces: S4 re-based label threading (`oram_threaded.rs`)
//! for the never-decoded posmap/stash/block state, and the S5c recursive
//! structure (`oram_recursive.rs`). Two-party over a loopback OT; the harness
//! drives both parties (the mechanism and the never-decoded intermediates are
//! what's pinned, not transport). Plaintext tree remains the scaffold.

use hybrid_array::Array;
use sha2::{Digest, Sha256};
use typenum::U16;
use volar_mpc::ot::LoopbackOt;
use volar_mpc::{DynGarbledExec, GateSchedule, OtChannel, garble_schedule_dyn};
use volar_oram::{Bucket, OramEntry, OramTree, eviction_target};
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_vc::oram_gadget::{OramGadgetConfig, build_access, build_begin, build_extract, build_update};
use volar_vc::compile_schedule;

type N = U16;
type D = Sha256;

const Z: usize = 2;
const C: usize = 2; // posmap entries per block

fn enc(value: u64, bits: usize) -> Vec<bool> {
    (0..bits).map(|j| (value >> j) & 1 == 1).collect()
}
fn dec(bits: &[bool]) -> u64 {
    bits.iter().enumerate().fold(0u64, |a, (j, &b)| a | if b { 1u64 << j } else { 0 })
}
fn enc_entry<const B: usize>(e: &OramEntry<B>, cfg: &OramGadgetConfig) -> Vec<bool> {
    let real = e.is_real();
    let mut v = vec![real];
    v.extend(enc(if real { e.addr } else { 0 }, cfg.addr_bits()));
    v.extend(enc(if real { e.leaf } else { 0 }, cfg.leaf_bits()));
    let data = e.data.iter().take(B).fold(0u64, |a, &b| (a << 8) | b as u64);
    v.extend(enc(if real { data } else { 0 }, cfg.data_bits));
    v
}
fn dec_entry<const B: usize>(bits: &[bool], cfg: &OramGadgetConfig) -> OramEntry<B> {
    let (ab, lb, db) = (cfg.addr_bits(), cfg.leaf_bits(), cfg.data_bits);
    if !bits[0] {
        return OramEntry::dummy();
    }
    let addr = dec(&bits[1..1 + ab]);
    let leaf = dec(&bits[1 + ab..1 + ab + lb]);
    let data = dec(&bits[1 + ab + lb..1 + ab + lb + db]);
    let mut bytes = [0u8; B];
    for (i, byte) in bytes.iter_mut().enumerate() {
        *byte = (data >> (8 * (B - 1 - i))) as u8;
    }
    OramEntry { addr, leaf, data: bytes }
}
fn flatten_path<const B: usize>(path: &[Bucket<Z, B>], cfg: &OramGadgetConfig) -> Vec<bool> {
    let mut v = Vec::new();
    for bucket in path {
        for e in &bucket.entries {
            v.extend(enc_entry(e, cfg));
        }
    }
    v
}
fn unflatten_path<const B: usize>(bits: &[bool], cfg: &OramGadgetConfig) -> Vec<Bucket<Z, B>> {
    let eb = cfg.entry_bits();
    (0..cfg.levels)
        .map(|level| Bucket {
            entries: core::array::from_fn(|s| dec_entry(&bits[(level * Z + s) * eb..(level * Z + s + 1) * eb], cfg)),
        })
        .collect()
}

fn fresh_base(counter: u64) -> Garble<N> {
    let h = Sha256::digest([b"s5c-2pc".as_slice(), &counter.to_le_bytes()].concat());
    Garble { base: Array::clone_from_slice(&h[..16]) }
}
fn reveal(label: &Eval<N>, base: &Garble<N>) -> bool {
    label.open(base)[0] & 1 != 0
}
fn reveal_word(labels: &[Eval<N>], bases: &[Garble<N>]) -> u64 {
    dec(&labels.iter().zip(bases).map(|(l, b)| reveal(l, b)).collect::<Vec<_>>())
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

/// A bundle of threaded state wires (evaluator holds `labels`, garbler holds
/// `bases`), never decoded.
#[derive(Clone)]
struct HeldState {
    labels: Vec<Eval<N>>,
    bases: Vec<Garble<N>>,
}

/// One two-party input: a threaded (held) wire, a public constant, a
/// garbler-private bit, or an evaluator-private (OT-delivered) bit.
#[derive(Clone)]
enum In {
    Held(Eval<N>, Garble<N>),
    Const(bool),
    Garbler(bool),
    Eval(bool),
}

fn held(h: &HeldState) -> Vec<In> {
    h.labels.iter().zip(&h.bases).map(|(l, b)| In::Held(l.clone(), b.clone())).collect()
}
fn consts(bits: &[bool]) -> Vec<In> {
    bits.iter().map(|&b| In::Const(b)).collect()
}
fn garblers(bits: &[bool]) -> Vec<In> {
    bits.iter().map(|&b| In::Garbler(b)).collect()
}
fn evals(bits: &[bool]) -> Vec<In> {
    bits.iter().map(|&b| In::Eval(b)).collect()
}

/// Run one circuit two-party (dyn path) with a mix of held/fresh inputs.
/// Returns the output `(labels, bases)`.
fn run_2pc(
    secret: &GlobalSecret<N>,
    fresh: &mut u64,
    schedule: &GateSchedule,
    inputs: Vec<In>,
    ot: &mut LoopbackOt<N>,
) -> (Vec<Eval<N>>, Vec<Garble<N>>) {
    assert_eq!(inputs.len(), schedule.num_inputs, "input width");
    let input_bases: Vec<Garble<N>> = inputs
        .iter()
        .map(|i| match i {
            In::Held(_, b) => b.clone(),
            _ => {
                *fresh += 1;
                fresh_base(*fresh)
            }
        })
        .collect();
    let exec = garble_schedule_dyn::<N, D>(schedule, secret.clone(), input_bases).expect("garbles");
    let setup = exec.circuit.eval_setup();
    let labels: Vec<Eval<N>> = inputs
        .iter()
        .enumerate()
        .map(|(i, inp)| match inp {
            In::Held(l, _) => l.clone(),
            In::Const(b) | In::Garbler(b) => secret.encode(&exec.circuit.input_labels[i], *b),
            In::Eval(b) => {
                let f = secret.encode(&exec.circuit.input_labels[i], false);
                let t = secret.encode(&exec.circuit.input_labels[i], true);
                ot.send([&f.target, &t.target]);
                Eval { target: ot.receive(*b) }
            }
        })
        .collect();
    let out = DynGarbledExec::<N>::eval_labels_multi::<D>(&setup, schedule, &labels).expect("evals");
    (out, exec.output_labels)
}

/// A two-level recursive ORAM driven two-party. Level 1 is the base (its own
/// posmap linear-scanned); level 0's leaf is resolved through level 1 with the
/// posmap block held as garbled labels.
struct Rec2pc {
    secret: GlobalSecret<N>,
    l1_begin: GateSchedule,
    l1_access: GateSchedule,
    l0_access: GateSchedule,
    extract: GateSchedule,
    update: GateSchedule,
    l0_cfg: OramGadgetConfig,
    l1_cfg: OramGadgetConfig,
    posmap_1: HeldState,
    stash_0: HeldState,
    stash_1: HeldState,
    tree_0: OramTree<Z, 1>,
    tree_1: OramTree<Z, 1>,
    counter_0: u64,
    counter_1: u64,
    rng_0: Splitmix,
    rng_1: Splitmix,
    fresh: u64,
}

/// Run one level's *access* circuit (no begin) two-party: absorb path,
/// select/read/write, evict, then the deterministic eviction pass. Returns the
/// rdata HeldState (threaded, not decoded). A free function taking disjoint
/// field borrows to keep the borrow checker happy across the recursion.
#[allow(clippy::too_many_arguments)]
fn access_level(
    cfg: &OramGadgetConfig,
    sched: &GateSchedule,
    secret: &GlobalSecret<N>,
    fresh: &mut u64,
    stash: &mut HeldState,
    tree: &mut OramTree<Z, 1>,
    counter: &mut u64,
    addr: u64,
    op_write: bool,
    wdata: Vec<In>,
    old_leaf: u64,
    new_leaf: u64,
    ot: &mut LoopbackOt<N>,
) -> HeldState {
    let (ab, lb, db, eb) = (cfg.addr_bits(), cfg.leaf_bits(), cfg.data_bits, cfg.entry_bits());
    let n_path = cfg.path_entries();

    let path = tree.read_path(old_leaf);
    let mut inputs = held(stash);
    inputs.extend(evals(&flatten_path::<1>(&path, cfg)));
    inputs.extend(garblers(&enc(addr, ab)));
    inputs.push(In::Const(op_write));
    inputs.extend(wdata);
    inputs.extend(consts(&enc(old_leaf, lb)));
    inputs.extend(garblers(&enc(new_leaf, lb)));
    inputs.push(In::Const(false));
    let (out_l, out_b) = run_2pc(secret, fresh, sched, inputs, ot);
    assert!(!reveal(&out_l[0], &out_b[0]), "ORAM stash overflow");
    let rdata = HeldState {
        labels: out_l[1..1 + db].to_vec(),
        bases: out_b[1..1 + db].to_vec(),
    };
    let new_path: Vec<bool> = (0..n_path * eb)
        .map(|i| reveal(&out_l[1 + db + i], &out_b[1 + db + i]))
        .collect();
    *stash = HeldState {
        labels: out_l[1 + db + n_path * eb..].to_vec(),
        bases: out_b[1 + db + n_path * eb..].to_vec(),
    };
    tree.write_path(old_leaf, &unflatten_path::<1>(&new_path, cfg));

    // Deterministic eviction pass.
    let evict_leaf = eviction_target(*counter, cfg.num_leaves() as u64);
    *counter += 1;
    if evict_leaf != old_leaf {
        let epath = tree.read_path(evict_leaf);
        let mut ev_in = held(stash);
        ev_in.extend(evals(&flatten_path::<1>(&epath, cfg)));
        ev_in.extend(consts(&enc(0, ab)));
        ev_in.push(In::Const(false));
        ev_in.extend(consts(&enc(0, db)));
        ev_in.extend(consts(&enc(evict_leaf, lb)));
        ev_in.extend(consts(&enc(0, lb)));
        ev_in.push(In::Const(true));
        let (evo_l, evo_b) = run_2pc(secret, fresh, sched, ev_in, ot);
        assert!(!reveal(&evo_l[0], &evo_b[0]), "ORAM stash overflow (evict)");
        let nepath: Vec<bool> = (0..n_path * eb)
            .map(|i| reveal(&evo_l[1 + db + i], &evo_b[1 + db + i]))
            .collect();
        *stash = HeldState {
            labels: evo_l[1 + db + n_path * eb..].to_vec(),
            bases: evo_b[1 + db + n_path * eb..].to_vec(),
        };
        tree.write_path(evict_leaf, &unflatten_path::<1>(&nepath, cfg));
    }
    rdata
}

impl Rec2pc {
    fn new(secret: GlobalSecret<N>, n0: usize) -> Self {
        let n1 = n0.div_ceil(C);
        let l0_cfg = OramGadgetConfig {
            num_addrs: n0,
            levels: 4,
            bucket_size: Z,
            data_bits: 1,
            max_stash: 2 * 4 + Z + n0,
            encrypted: false,
            tree_key_bits: 0,
        };
        let l1_cfg = OramGadgetConfig {
            num_addrs: n1,
            levels: 3,
            bucket_size: Z,
            data_bits: C * l0_cfg.leaf_bits(),
            max_stash: 2 * 3 + Z + n1,
            encrypted: false,
            tree_key_bits: 0,
        };
        let mut fresh = 0u64;
        let mk = |bits: usize, secret: &GlobalSecret<N>, fresh: &mut u64| {
            let bases: Vec<Garble<N>> = (0..bits)
                .map(|_| {
                    *fresh += 1;
                    fresh_base(*fresh)
                })
                .collect();
            let labels = bases.iter().map(|b| secret.encode(b, false)).collect();
            HeldState { labels, bases }
        };
        let posmap_1 = mk(n1 * l1_cfg.leaf_bits(), &secret, &mut fresh);
        let stash_0 = mk(l0_cfg.max_stash * l0_cfg.entry_bits(), &secret, &mut fresh);
        let stash_1 = mk(l1_cfg.max_stash * l1_cfg.entry_bits(), &secret, &mut fresh);
        Rec2pc {
            l1_begin: compile_schedule(&build_begin(&l1_cfg)).expect("l1 begin"),
            l1_access: compile_schedule(&build_access(&l1_cfg)).expect("l1 access"),
            l0_access: compile_schedule(&build_access(&l0_cfg)).expect("l0 access"),
            extract: compile_schedule(&build_extract(C, l0_cfg.leaf_bits())).expect("extract"),
            update: compile_schedule(&build_update(C, l0_cfg.leaf_bits())).expect("update"),
            l0_cfg,
            l1_cfg,
            posmap_1,
            stash_0,
            stash_1,
            tree_0: OramTree::new(4),
            tree_1: OramTree::new(3),
            counter_0: 0,
            counter_1: 0,
            rng_0: Splitmix(0xAAAA),
            rng_1: Splitmix(0xBBBB),
            fresh,
            secret,
        }
    }

    /// A base-level (L1) access: linear-scan begin + access. Returns rdata.
    fn l1_access(&mut self, addr1: u64, op_write: bool, wdata: Vec<In>, ot: &mut LoopbackOt<N>) -> HeldState {
        let lb1 = self.l1_cfg.leaf_bits();
        let ab1 = self.l1_cfg.addr_bits();
        let new_leaf_1 = self.rng_1.next() % self.l1_cfg.num_leaves() as u64;
        let secret = self.secret.clone();
        let l1_cfg = self.l1_cfg;
        let l1_begin = self.l1_begin.clone();
        let l1_access = self.l1_access.clone();
        // begin: old_leaf_1 = posmap_1[addr1]; posmap_1[addr1] = new_leaf_1.
        let mut begin_in = held(&self.posmap_1);
        begin_in.extend(garblers(&enc(addr1, ab1)));
        begin_in.extend(garblers(&enc(new_leaf_1, lb1)));
        let (bl, bb) = run_2pc(&secret, &mut self.fresh, &l1_begin, begin_in, ot);
        let old_leaf_1 = reveal_word(&bl[..lb1], &bb[..lb1]);
        self.posmap_1 = HeldState {
            labels: bl[lb1..].to_vec(),
            bases: bb[lb1..].to_vec(),
        };
        access_level(
            &l1_cfg,
            &l1_access,
            &secret,
            &mut self.fresh,
            &mut self.stash_1,
            &mut self.tree_1,
            &mut self.counter_1,
            addr1,
            op_write,
            wdata,
            old_leaf_1,
            new_leaf_1,
            ot,
        )
    }

    /// A recursive level-0 access. Returns the read bit (if a read).
    fn access0(&mut self, addr0: u64, op_write: bool, wdata_bit: bool, ot: &mut LoopbackOt<N>) -> u64 {
        let lb0 = self.l0_cfg.leaf_bits();
        let new_leaf_0 = self.rng_0.next() % self.l0_cfg.num_leaves() as u64;
        let c = C as u64;
        let b1 = addr0 / c;
        let off = (addr0 % c) as usize;
        let secret = self.secret.clone();
        let extract = self.extract.clone();
        let update = self.update.clone();

        // 1. Read the L1 posmap block for `b1` — returns the block (c leaves of
        //    level 0) as GARBLED labels, never decoded.
        let block = self.l1_access(b1, false, consts(&enc(0, self.l1_cfg.data_bits)), ot);
        // 2. Extract entry `off` in-circuit; only this leaf is revealed.
        let mut ex_in = held(&block);
        ex_in.extend(garblers(&enc(off as u64, 1)));
        let (exl, exb) = run_2pc(&secret, &mut self.fresh, &extract, ex_in, ot);
        let leaf_0 = reveal_word(&exl[..lb0], &exb[..lb0]);
        // 3. Replace entry `off` with new_leaf_0 in-circuit (stays garbled).
        let mut up_in = held(&block);
        up_in.extend(garblers(&enc(off as u64, 1)));
        up_in.extend(garblers(&enc(new_leaf_0, lb0)));
        let (upl, upb) = run_2pc(&secret, &mut self.fresh, &update, up_in, ot);
        let new_block = HeldState {
            labels: upl.clone(),
            bases: upb.clone(),
        };
        // 4. Write the updated posmap block back to level 1.
        self.l1_access(b1, true, held(&new_block), ot);
        // 5. The level-0 access itself, with the resolved leaf.
        let l0_cfg = self.l0_cfg;
        let l0_access = self.l0_access.clone();
        let rdata = access_level(
            &l0_cfg,
            &l0_access,
            &self.secret,
            &mut self.fresh,
            &mut self.stash_0,
            &mut self.tree_0,
            &mut self.counter_0,
            addr0,
            op_write,
            vec![In::Const(wdata_bit)],
            leaf_0,
            new_leaf_0,
            ot,
        );
        reveal_word(&rdata.labels, &rdata.bases)
    }
}

#[test]
fn s5c_recursive_two_party_garbled_intermediates() {
    let secret: GlobalSecret<N> = GlobalSecret::new(Array::clone_from_slice(&[9u8; 16]));
    let mut r = Rec2pc::new(secret, 8);
    let mut ot = LoopbackOt::<N>::new();
    let mut model = [0u8; 8];

    // write 1,0,1,1 to addrs 1,3,5,7; read them back; a few mixed ops.
    let ops: [(u64, Option<u8>); 10] = [
        (1, Some(1)),
        (3, Some(0)),
        (5, Some(1)),
        (7, Some(1)),
        (1, None),
        (3, None),
        (5, None),
        (7, None),
        (2, Some(1)),
        (2, None),
    ];
    for (step, &(addr, w)) in ops.iter().enumerate() {
        let got = r.access0(addr, w.is_some(), w.unwrap_or(0) != 0, &mut ot);
        match w {
            Some(d) => model[addr as usize] = d,
            None => assert_eq!(got as u8, model[addr as usize], "step {step}: read({addr})"),
        }
    }
}
