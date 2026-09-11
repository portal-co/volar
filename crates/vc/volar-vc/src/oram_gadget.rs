// @reliability: experimental
// @ai: assisted
//! In-circuit Path ORAM access gadget (symbolic-ORAM workstream, increment S1).
//!
//! This builds the **ORAM client logic** as a concrete-evaluable boolar
//! circuit, keeping the physical tree *external* — the architectural split at
//! the heart of symbolic garbled RAM (see `GRAM_SYMBOLIC_ORAM.md`):
//!
//! - **In-circuit (garbled in real deployment):** the position map, the stash,
//!   the target-block select/read/write, and the eviction packing. The logical
//!   address stays a circuit value end-to-end.
//! - **External (the "only oblivious accesses are revealed" seam):** the
//!   physical `read_path(leaf)` / `write_path(leaf, …)` against the
//!   evaluator-hosted tree. The circuit emits the leaf as an output; the host
//!   reads the path and feeds the bucket bits back as inputs.
//!
//! Because the leaf is uniform and re-randomized per access, revealing it leaks
//! nothing about the logical address (standard Path ORAM argument).
//!
//! # Decomposition
//!
//! One logical access splits at the extern boundary into two circuit kinds:
//!
//! - [`build_begin`]: `posmap.update(addr, new_leaf) -> old_leaf`, updating the
//!   position map. Run first; the revealed `old_leaf` drives `read_path`.
//! - [`build_access`]: absorb a path into the stash, (optionally) select and
//!   read/write the target block, then greedily evict along the path, emitting
//!   the buckets to write back. This one circuit serves *both* the main access
//!   (`evict_only = 0`) and the deterministic post-access eviction
//!   (`evict_only = 1`), which differ only in whether the select/read/write
//!   fires.
//!
//! The host interleaves these with tree I/O exactly as
//! `volar_oram::oram_access_local` does (the semantic oracle the S1 test checks
//! against): begin → read main path → access → write main path → read evict
//! path → access → write evict path.
//!
//! # Circuit encoding
//!
//! An ORAM entry is a flat little word, LSB-first per field:
//! `[valid:1, addr:addr_bits, leaf:leaf_bits, data:data_bits]`. Validity is an
//! explicit bit (the host maps `OramEntry`'s `u64::MAX`-dummy to `valid = 0`).
//! The position map is `num_addrs × leaf_bits`; the stash is
//! `max_stash × entry_bits`; a path is `levels × bucket_size × entry_bits`.
//!
//! # Correctness notes
//!
//! - Eviction uses **prefix equality**, not node-index arithmetic: an entry
//!   with leaf `e` may sit in the depth-`d` bucket of the path to leaf `L` iff
//!   the top-`d` bits of `e` and `L` agree (that bucket is an ancestor of
//!   `e`'s leaf). This keeps the placed block on its *own* path, preserving
//!   the Path ORAM invariant, and needs no division/shift on garbled indices.
//! - The stash is a fixed `max_stash`-slot array; an `overflow` output flag is
//!   raised if a real block could not be placed (stash exhaustion), which the
//!   host must treat as a protocol abort. With `max_stash ≥ num_addrs` no
//!   in-range access can overflow.

use alloc::vec;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;

/// Compile-time geometry for the in-circuit ORAM gadget.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OramGadgetConfig {
    /// Number of distinct logical addresses the ORAM can serve.
    pub num_addrs: usize,
    /// Path-ORAM tree depth. `num_leaves = 2^(levels-1)`.
    pub levels: usize,
    /// Bucket size `Z` (entries per tree node).
    pub bucket_size: usize,
    /// Payload width of one block, in bits (`= B * 8` for byte blocks).
    pub data_bits: usize,
    /// Stash capacity in entries. `>= num_addrs` is always safe.
    pub max_stash: usize,
    /// When set, the physical tree is **encrypted**: `read_path` returns
    /// ciphertext and the access circuit decrypts on absorb / re-encrypts on
    /// evict under a secret `tree_key` input, so the tree-hosting evaluator
    /// cannot read block tags (the S5 obliviousness upgrade). The whole entry
    /// (`valid ++ addr ++ leaf ++ data`) is XORed with a per-node pad, so the
    /// evaluator cannot even distinguish real blocks from dummies.
    #[doc(hidden)]
    pub encrypted: bool,
    /// Width of the `tree_key` secret input (used only when `encrypted`).
    #[doc(hidden)]
    pub tree_key_bits: usize,
}

impl OramGadgetConfig {
    /// Bits to name a logical address.
    pub fn addr_bits(&self) -> usize {
        // Smallest k with 2^k >= num_addrs (integer ceil-log2; no_std-safe).
        let n = self.num_addrs.max(2);
        let mut k = 0;
        while (1usize << k) < n {
            k += 1;
        }
        k
    }
    /// Bits to name a leaf: `log2(num_leaves) = levels - 1`.
    pub fn leaf_bits(&self) -> usize {
        self.levels - 1
    }
    /// Number of leaves in the tree.
    pub fn num_leaves(&self) -> usize {
        1usize << self.leaf_bits()
    }
    /// Bits in one packed entry `[valid, addr, leaf, data]`.
    pub fn entry_bits(&self) -> usize {
        1 + self.addr_bits() + self.leaf_bits() + self.data_bits
    }
    /// Entries on one path (`levels * Z`).
    pub fn path_entries(&self) -> usize {
        self.levels * self.bucket_size
    }

    // --- begin-circuit layout -------------------------------------------
    /// Input width of the begin circuit.
    pub fn begin_params(&self) -> usize {
        self.num_addrs * self.leaf_bits() + self.addr_bits() + self.leaf_bits()
    }
    /// Output width of the begin circuit (`old_leaf ++ posmap'`).
    pub fn begin_outputs(&self) -> usize {
        self.leaf_bits() + self.num_addrs * self.leaf_bits()
    }

    // --- access-circuit layout ------------------------------------------
    /// Input width of the access circuit.
    pub fn access_params(&self) -> usize {
        let eb = self.entry_bits();
        let base = self.max_stash * eb            // stash
            + self.path_entries() * eb // path
            + self.addr_bits()         // addr
            + 1                        // op_write
            + self.data_bits           // wdata
            + self.leaf_bits()         // path_leaf
            + self.leaf_bits()         // new_leaf
            + 1; // evict_only
        // Encrypted tree: the secret `tree_key` is an additional input.
        if self.encrypted {
            base + self.tree_key_bits
        } else {
            base
        }
    }
    /// Output width of the access circuit
    /// (`overflow ++ rdata ++ new_path ++ stash'`).
    pub fn access_outputs(&self) -> usize {
        let eb = self.entry_bits();
        1 + self.data_bits + self.path_entries() * eb + self.max_stash * eb
    }
}

/// Offset of an entry's `valid` bit.
fn off_valid() -> usize {
    0
}
/// Offset of an entry's `addr` field.
fn off_addr() -> usize {
    1
}
/// Offset of an entry's `leaf` field.
fn off_leaf(cfg: &OramGadgetConfig) -> usize {
    1 + cfg.addr_bits()
}
/// Offset of an entry's `data` field.
fn off_data(cfg: &OramGadgetConfig) -> usize {
    1 + cfg.addr_bits() + cfg.leaf_bits()
}

/// A boolar circuit under construction: params occupy var ids `0..params`,
/// each pushed gate takes the next id.
struct Builder {
    params: u32,
    stmts: Vec<Node<BIrStmt, ()>>,
}

impl Builder {
    fn new(params: u32) -> Self {
        Self {
            params,
            stmts: Vec::new(),
        }
    }
    fn gate(&mut self, s: BIrStmt) -> u32 {
        let id = self.params + self.stmts.len() as u32;
        self.stmts.push(Node::new(s, (), None));
        id
    }
    fn const0(&mut self) -> u32 {
        self.gate(BIrStmt::Zero)
    }
    fn const1(&mut self) -> u32 {
        self.gate(BIrStmt::One)
    }
    fn and(&mut self, a: u32, b: u32) -> u32 {
        self.gate(BIrStmt::And(IRVarId(a), IRVarId(b)))
    }
    fn or(&mut self, a: u32, b: u32) -> u32 {
        self.gate(BIrStmt::Or(IRVarId(a), IRVarId(b)))
    }
    fn xor(&mut self, a: u32, b: u32) -> u32 {
        self.gate(BIrStmt::Xor(IRVarId(a), IRVarId(b)))
    }
    fn not(&mut self, a: u32) -> u32 {
        self.gate(BIrStmt::Not(IRVarId(a)))
    }
    /// `MUX(c, a, b) = AND(c, XOR(a, b)) XOR b` (the `storage_to_mux_boolar`
    /// primitive).
    fn mux(&mut self, c: u32, a: u32, b: u32) -> u32 {
        let x = self.xor(a, b);
        let y = self.and(c, x);
        self.xor(y, b)
    }
    /// Bitwise [`Builder::mux`] over equal-length words.
    fn mux_word(&mut self, c: u32, a: &[u32], b: &[u32]) -> Vec<u32> {
        a.iter().zip(b).map(|(&ai, &bi)| self.mux(c, ai, bi)).collect()
    }
    /// `1` iff the LSB-first `bits` equal the constant `value`.
    fn eq_const(&mut self, bits: &[u32], value: u64) -> u32 {
        let mut acc: Option<u32> = None;
        for (j, &bit) in bits.iter().enumerate() {
            let set = (value >> j) & 1 == 1;
            let t = if set { bit } else { self.not(bit) };
            acc = Some(match acc {
                None => t,
                Some(a) => self.and(a, t),
            });
        }
        acc.unwrap_or_else(|| self.const1())
    }
    /// `1` iff LSB-first words `a` and `b` are equal (AND-fold of per-bit XNOR).
    fn eq_bits(&mut self, a: &[u32], b: &[u32]) -> u32 {
        let mut acc: Option<u32> = None;
        for (&ai, &bi) in a.iter().zip(b) {
            let x = self.xor(ai, bi);
            let xn = self.not(x);
            acc = Some(match acc {
                None => xn,
                Some(p) => self.and(p, xn),
            });
        }
        acc.unwrap_or_else(|| self.const1())
    }
    /// `1` iff the top-`depth` bits of LSB-first leaves `a` and `b` agree.
    /// `depth = 0` (the root) matches everything.
    fn prefix_eq(&mut self, a: &[u32], b: &[u32], depth: usize) -> u32 {
        let leaf_bits = a.len();
        let mut acc: Option<u32> = None;
        for j in 0..depth {
            let idx = leaf_bits - 1 - j;
            let x = self.xor(a[idx], b[idx]);
            let xn = self.not(x);
            acc = Some(match acc {
                None => xn,
                Some(p) => self.and(p, xn),
            });
        }
        acc.unwrap_or_else(|| self.const1())
    }
    /// 128-bit AES plaintext tweak naming one path slot: `(depth, zslot)` as
    /// constant bytes plus the top-`depth` bits of `path_leaf` (the node
    /// prefix). Only the prefix is included — not the full leaf — so every
    /// leaf whose path reaches a given physical node produces the *same* tweak
    /// for that node's slot, keeping the pad consistent across accesses.
    /// `zc`/`oc` are shared constant wires. AES keyed on the secret `tree_key`
    /// turns this public tweak into a pad the evaluator cannot reproduce.
    fn slot_tweak(&mut self, path_leaf: &[u32], depth: usize, zslot: usize, zc: u32, oc: u32) -> Vec<u32> {
        let lb = path_leaf.len();
        let mut tw = Vec::with_capacity(128);
        for i in 0..16 {
            let val = if i < 8 {
                (depth >> i) & 1 == 1
            } else {
                (zslot >> (i - 8)) & 1 == 1
            };
            tw.push(if val { oc } else { zc });
        }
        for j in 0..depth {
            tw.push(path_leaf[lb - 1 - j]);
        }
        while tw.len() < 128 {
            tw.push(zc);
        }
        tw
    }
    /// Inline a single-block sub-circuit: append its gates with var ids
    /// remapped (`inputs[i]` is the parent wire for sub-param `i`), returning
    /// the sub-circuit's output wires in the parent. Used to instantiate the
    /// AES PRF per path slot.
    fn inline_sub(&mut self, sub: &BIrBlocks, inputs: &[u32]) -> Vec<u32> {
        let block = &sub.blocks[0];
        assert_eq!(sub.blocks.len(), 1, "inline_sub: single-block circuit");
        assert_eq!(block.params as usize, inputs.len(), "inline_sub: input arity");
        let mut remap: Vec<u32> = Vec::with_capacity(block.params as usize + block.stmts.len());
        remap.extend_from_slice(inputs);
        for stmt in &block.stmts {
            let s = remap_bir_stmt(&stmt.kind, &remap);
            let id = self.gate(s);
            remap.push(id);
        }
        match &block.terminator {
            BIrTerminator::Jmp(t) if t.block == IRBlockTargetId::Return => {
                t.args.iter().map(|a| remap[a.0 as usize]).collect()
            }
            _ => panic!("inline_sub: sub-circuit must end in a Return"),
        }
    }
    /// Bitwise-XOR a word with a pad (used for both decrypt and encrypt).
    fn xor_word(&mut self, a: &[u32], pad: &[u32]) -> Vec<u32> {
        a.iter().zip(pad).map(|(&ai, &pi)| self.xor(ai, pi)).collect()
    }
    fn finish(self, outputs: Vec<u32>) -> BIrBlocks {
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: self.params,
                stmts: self.stmts,
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: outputs.into_iter().map(IRVarId).collect(),
                }),
            }],
            pre_init: vec![],
        }
    }
}

/// Remap a boolean gate's operand var ids through `remap` (used by
/// [`Builder::inline_sub`]; only the pure-boolean variants an inlined gadget
/// uses are supported).
fn remap_bir_stmt(s: &BIrStmt, remap: &[u32]) -> BIrStmt {
    let r = |v: &IRVarId| IRVarId(remap[v.0 as usize]);
    match s {
        BIrStmt::Zero => BIrStmt::Zero,
        BIrStmt::One => BIrStmt::One,
        BIrStmt::And(a, b) => BIrStmt::And(r(a), r(b)),
        BIrStmt::Or(a, b) => BIrStmt::Or(r(a), r(b)),
        BIrStmt::Xor(a, b) => BIrStmt::Xor(r(a), r(b)),
        BIrStmt::Not(a) => BIrStmt::Not(r(a)),
        other => panic!("inline_sub: unsupported stmt {other:?}"),
    }
}

/// Build the **begin** circuit: `posmap.update(addr, new_leaf) -> old_leaf`.
///
/// Inputs:  `[posmap: num_addrs×leaf_bits, addr: addr_bits, new_leaf: leaf_bits]`
/// Outputs: `[old_leaf: leaf_bits, posmap': num_addrs×leaf_bits]`
pub fn build_begin(cfg: &OramGadgetConfig) -> BIrBlocks {
    let (ab, lb, num_addrs) = (cfg.addr_bits(), cfg.leaf_bits(), cfg.num_addrs);
    let mut b = Builder::new(cfg.begin_params() as u32);

    let mut posmap: Vec<Vec<u32>> = (0..num_addrs)
        .map(|i| (0..lb).map(|j| (i * lb + j) as u32).collect())
        .collect();
    let addr: Vec<u32> = (0..ab).map(|j| (num_addrs * lb + j) as u32).collect();
    let new_leaf: Vec<u32> = (0..lb).map(|j| (num_addrs * lb + ab + j) as u32).collect();

    let zc = b.const0();
    let mut old_leaf: Vec<u32> = vec![zc; lb];
    for (i, cell) in posmap.iter_mut().enumerate() {
        let eq = b.eq_const(&addr, i as u64);
        let new_old = b.mux_word(eq, cell, &old_leaf);
        old_leaf = new_old;
        let new_cell = b.mux_word(eq, &new_leaf, cell);
        *cell = new_cell;
    }

    let mut out = old_leaf;
    for cell in &posmap {
        out.extend_from_slice(cell);
    }
    b.finish(out)
}

/// Build the **access** circuit: absorb a path, optionally select/read/write
/// the target block, then evict along the path. Used for both the main access
/// (`evict_only = 0`) and the post-access eviction (`evict_only = 1`).
///
/// Inputs:  `[stash: max_stash×eb, path: (levels·Z)×eb, addr: ab, op_write: 1,
///           wdata: data_bits, path_leaf: lb, new_leaf: lb, evict_only: 1]`
/// Outputs: `[overflow: 1, rdata: data_bits, new_path: (levels·Z)×eb,
///           stash': max_stash×eb]`
pub fn build_access(cfg: &OramGadgetConfig) -> BIrBlocks {
    let (ab, lb, eb, db) = (
        cfg.addr_bits(),
        cfg.leaf_bits(),
        cfg.entry_bits(),
        cfg.data_bits,
    );
    let (z, levels, ms) = (cfg.bucket_size, cfg.levels, cfg.max_stash);
    let n_path = cfg.path_entries();

    // Input layout offsets.
    let stash_off = 0usize;
    let path_off = stash_off + ms * eb;
    let addr_off = path_off + n_path * eb;
    let op_off = addr_off + ab;
    let wdata_off = op_off + 1;
    let pleaf_off = wdata_off + db;
    let nleaf_off = pleaf_off + lb;
    let evonly_off = nleaf_off + lb;

    let mut b = Builder::new(cfg.access_params() as u32);

    let mut stash: Vec<Vec<u32>> = (0..ms)
        .map(|i| (0..eb).map(|j| (stash_off + i * eb + j) as u32).collect())
        .collect();
    let mut path: Vec<Vec<u32>> = (0..n_path)
        .map(|k| (0..eb).map(|j| (path_off + k * eb + j) as u32).collect())
        .collect();
    let addr: Vec<u32> = (0..ab).map(|j| (addr_off + j) as u32).collect();
    let op_write = op_off as u32;
    let wdata: Vec<u32> = (0..db).map(|j| (wdata_off + j) as u32).collect();
    let path_leaf: Vec<u32> = (0..lb).map(|j| (pleaf_off + j) as u32).collect();
    let new_leaf: Vec<u32> = (0..lb).map(|j| (nleaf_off + j) as u32).collect();
    let evict_only = evonly_off as u32;
    let tree_key: Vec<u32> = (0..cfg.tree_key_bits)
        .map(|j| (evonly_off + 1 + j) as u32)
        .collect();

    let zc = b.const0();
    let oc = b.const1();
    let not_evict_only = b.not(evict_only);
    let mut overflow = zc;

    // Encrypted tree: precompute each path slot's pad, then decrypt the path the
    // host just read (the tree stores ciphertext). The same pad re-encrypts the
    // slot on write-back below, so a physical node has one consistent pad.
    //
    // The pad is `AES-128(tree_key, slot_tweak)` — a real PRF keyed on the
    // garbler-held `tree_key`, so the tree-hosting evaluator cannot recover
    // block tags from the ciphertext. One AES instance is inlined per path slot.
    //
    // Only the payload `[addr, leaf, data]` is encrypted; the `valid` bit stays
    // plaintext so an untouched (all-zero) tree slot reads back as a dummy
    // without any garbler-side formatting pass. (The evaluator therefore sees
    // the tree *occupancy* pattern — a partial leak hardened in S6 — but not the
    // address tags, leaf assignments, or data.)
    let mut pads: Vec<Vec<u32>> = Vec::new();
    if cfg.encrypted {
        assert_eq!(cfg.tree_key_bits, 128, "encrypted tree uses an AES-128 key");
        let aes = crate::aes_gadget::build_aes128();
        for k in 0..n_path {
            let tweak = b.slot_tweak(&path_leaf, k / z, k % z, zc, oc);
            let mut aes_in = tree_key.clone();
            aes_in.extend_from_slice(&tweak);
            let aes_out = b.inline_sub(&aes, &aes_in);
            // eb-1 pad bits: cover the payload, leave `valid` (bit 0) plaintext.
            pads.push(aes_out[0..eb - 1].to_vec());
        }
        for k in 0..n_path {
            let mut e = vec![path[k][0]]; // valid stays plaintext
            e.extend(b.xor_word(&path[k][1..], &pads[k]));
            path[k] = e;
        }
    }

    // 1. Absorb every real path entry into the first free stash slot.
    for pe in &path {
        let pe_valid = pe[off_valid()];
        let mut placed = zc;
        for slot in stash.iter_mut() {
            let slot_free = b.not(slot[off_valid()]);
            let not_placed = b.not(placed);
            let place = b.and(pe_valid, slot_free);
            let place = b.and(place, not_placed);
            let updated = b.mux_word(place, pe, slot);
            *slot = updated;
            placed = b.or(placed, place);
        }
        let not_placed = b.not(placed);
        let dropped = b.and(pe_valid, not_placed);
        overflow = b.or(overflow, dropped);
    }

    // 2. Select / read / write the target block (skipped when evict_only).
    let mut rdata: Vec<u32> = vec![zc; db];
    let mut found = zc;
    for s in 0..ms {
        let e = stash[s].clone();
        let eq = b.eq_bits(&e[off_addr()..off_leaf(cfg)], &addr);
        let nf = b.not(found);
        let m0 = b.and(e[off_valid()], eq);
        let m1 = b.and(m0, nf);
        let m = b.and(m1, not_evict_only);
        // read
        rdata = b.mux_word(m, &e[off_data(cfg)..eb], &rdata);
        // write
        let mw = b.and(m, op_write);
        let new_data = b.mux_word(mw, &wdata, &e[off_data(cfg)..eb]);
        // retag leaf to new_leaf
        let new_lf = b.mux_word(m, &new_leaf, &e[off_leaf(cfg)..off_data(cfg)]);
        let mut e2 = e.clone();
        e2[off_leaf(cfg)..off_data(cfg)].copy_from_slice(&new_lf);
        e2[off_data(cfg)..eb].copy_from_slice(&new_data);
        stash[s] = e2;
        found = b.or(found, m);
    }

    // 2b. First touch of this address: create the block in a free slot.
    let nf = b.not(found);
    let need_create = b.and(nf, not_evict_only);
    let create_data = b.mux_word(op_write, &wdata, &vec![zc; db]);
    let mut centry: Vec<u32> = Vec::with_capacity(eb);
    centry.push(oc);
    centry.extend_from_slice(&addr);
    centry.extend_from_slice(&new_leaf);
    centry.extend_from_slice(&create_data);
    let mut placed = zc;
    for slot in stash.iter_mut() {
        let slot_free = b.not(slot[off_valid()]);
        let not_placed = b.not(placed);
        let place0 = b.and(need_create, slot_free);
        let place = b.and(place0, not_placed);
        let updated = b.mux_word(place, &centry, slot);
        *slot = updated;
        placed = b.or(placed, place);
    }
    let not_placed = b.not(placed);
    let cdropped = b.and(need_create, not_placed);
    overflow = b.or(overflow, cdropped);

    // 3. Evict along `path_leaf`, deepest level first, filling each bucket.
    let mut placed_in: Vec<u32> = vec![zc; ms];
    let mut new_path: Vec<Vec<u32>> = (0..n_path).map(|_| vec![zc; eb]).collect();
    for level in (0..levels).rev() {
        for slot in 0..z {
            let bucket = level * z + slot;
            let mut found_slot = zc;
            for i in 0..ms {
                let e = stash[i].clone();
                let peq = b.prefix_eq(&e[off_leaf(cfg)..off_data(cfg)], &path_leaf, level);
                let nplaced = b.not(placed_in[i]);
                let e0 = b.and(e[off_valid()], nplaced);
                let e1 = b.and(e0, peq);
                let nfs = b.not(found_slot);
                let elig = b.and(e1, nfs);
                let cur = new_path[bucket].clone();
                let updated = b.mux_word(elig, &e, &cur);
                new_path[bucket] = updated;
                placed_in[i] = b.or(placed_in[i], elig);
                found_slot = b.or(found_slot, elig);
            }
        }
    }

    // 4. Whatever was not evicted stays in the stash (clear the placed bits).
    for i in 0..ms {
        let e = stash[i].clone();
        let nplaced = b.not(placed_in[i]);
        let new_valid = b.and(e[off_valid()], nplaced);
        let mut e2 = e;
        e2[off_valid()] = new_valid;
        stash[i] = e2;
    }

    // Encrypted tree: re-encrypt the evicted path's payload before the host
    // writes it back (same per-slot pad as the decrypt above; `valid` stays
    // plaintext).
    if cfg.encrypted {
        for k in 0..n_path {
            let mut e = vec![new_path[k][0]];
            e.extend(b.xor_word(&new_path[k][1..], &pads[k]));
            new_path[k] = e;
        }
    }

    // Outputs: overflow, rdata, new_path, stash'.
    let mut out = vec![overflow];
    out.extend_from_slice(&rdata);
    for e in &new_path {
        out.extend_from_slice(e);
    }
    for e in &stash {
        out.extend_from_slice(e);
    }
    b.finish(out)
}
