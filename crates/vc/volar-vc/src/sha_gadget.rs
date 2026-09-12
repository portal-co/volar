//! Bit-level SHA-256 / HMAC-SHA256 / HKDF-SHA256 boolar gadgets (P4c-ii).
//!
//! The TLS 1.3 key schedule (HKDF) and the handshake transcript hash both
//! run on SHA-256; for MPC-TLS they must execute on garbled values, so this
//! module builds them as fixed-geometry pure-boolean circuits (no symbolic
//! lengths — padding constants are compile-time, like
//! [`crate::aes_gadget::build_aes128_gcm`]).
//!
//! Layout convention (matching the AES gadget): inputs/outputs are bytes in
//! order, **LSB-first within each byte**. Inside, SHA-256 words are handled
//! as 32-bit LSB-first value bits with the big-endian regrouping done at the
//! input/output boundary.
//!
//! Cost: ~40k ANDs per 512-bit compression block (the classic SHA-256
//! circuit size), so an HMAC (~4 blocks incl. pads) is ~170k ANDs and a TLS
//! 1.3 HKDF step ~2 HMACs. Affine gates are free.

use alloc::vec;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;

/// Minimal boolar builder (same pattern as `aes_gadget`'s local builder).
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
    fn const_bits(&mut self, value: u32) -> [u32; 32] {
        let mut out = [0; 32];
        for (j, b) in out.iter_mut().enumerate() {
            *b = if (value >> j) & 1 == 1 {
                self.const1()
            } else {
                self.const0()
            };
        }
        out
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
    fn xor3(&mut self, a: u32, b: u32, c: u32) -> u32 {
        let x = self.xor(a, b);
        self.xor(x, c)
    }
    fn xor_word(&mut self, a: &[u32; 32], b: &[u32; 32]) -> [u32; 32] {
        let mut out = [0; 32];
        for j in 0..32 {
            out[j] = self.xor(a[j], b[j]);
        }
        out
    }
    /// 32-bit ripple-carry addition, LSB-first in/out.
    fn add32(&mut self, a: &[u32; 32], b: &[u32; 32]) -> [u32; 32] {
        let mut out = [0; 32];
        let mut carry: Option<u32> = None;
        for j in 0..32 {
            let axb = self.xor(a[j], b[j]);
            let (sum, cout) = match carry {
                None => (axb, Some(self.and(a[j], b[j]))),
                Some(c) => {
                    let s = self.xor(axb, c);
                    let ab = self.and(a[j], b[j]);
                    let caxb = self.and(c, axb);
                    (s, Some(self.or(ab, caxb)))
                }
            };
            out[j] = sum;
            carry = cout;
        }
        out
    }
    fn add_many(&mut self, words: &[&[u32; 32]]) -> [u32; 32] {
        assert!(!words.is_empty());
        let mut acc = *words[0];
        for w in &words[1..] {
            acc = self.add32(&acc, w);
        }
        acc
    }
}

/// Rotate-right is a free wire permutation (LSB-first words).
fn rotr(w: &[u32; 32], n: usize) -> [u32; 32] {
    let mut out = [0; 32];
    for j in 0..32 {
        out[j] = w[(j + n) % 32];
    }
    out
}

/// Shift-right (zero-fill), free wire permutation.
fn shr(w: &[u32; 32], n: usize) -> [u32; 32] {
    let mut out = [0; 32];
    for j in 0..32 - n {
        out[j] = w[j + n];
    }
    // The vacated high bits are filled by the caller's `zero` wire.
    out
}

const K256: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

const H0_256: [u32; 8] = [
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
];

/// One SHA-256 compression over a 512-bit block. `block` is the block's 512
/// bits as 16 big-endian words, each 32 LSB-first value bits; `h` is the
/// 8-word chaining state. Returns the new state.
fn compress(b: &mut Builder, h: &[[u32; 32]; 8], block: &[[u32; 32]; 16]) -> [[u32; 32]; 8] {
    let zero = b.const0();
    // Message schedule.
    let mut w: Vec<[u32; 32]> = block.to_vec();
    for t in 16..64 {
        // σ0(x) = rotr7 ^ rotr18 ^ shr3 ; σ1(x) = rotr17 ^ rotr19 ^ shr10
        // (shifts zero-filled with the shared `zero` wire).
        let w15 = w[t - 15];
        let w2 = w[t - 2];
        let mut s0 = [zero; 32];
        let mut s1 = [zero; 32];
        {
            let (r7, r18) = (rotr(&w15, 7), rotr(&w15, 18));
            let mut s3 = shr(&w15, 3);
            for bit in s3.iter_mut().skip(32 - 3) {
                *bit = zero;
            }
            let (r17, r19) = (rotr(&w2, 17), rotr(&w2, 19));
            let mut s10 = shr(&w2, 10);
            for bit in s10.iter_mut().skip(32 - 10) {
                *bit = zero;
            }
            for j in 0..32 {
                s0[j] = b.xor3(r7[j], r18[j], s3[j]);
                s1[j] = b.xor3(r17[j], r19[j], s10[j]);
            }
        }
        // w[t] = w[t-16] + σ0 + w[t-7] + σ1 (three 32-bit adds).
        let t1 = b.add32(&w[t - 16], &s0);
        let t2 = b.add32(&t1, &w[t - 7]);
        w.push(b.add32(&t2, &s1));
    }
    let mut st = *h;
    for t in 0..64 {
        let (a, bb, c, d, e, f, g, hh) = (st[0], st[1], st[2], st[3], st[4], st[5], st[6], st[7]);
        // Σ1(e) = rotr6 ^ rotr11 ^ rotr25
        let (r6, r11, r25) = (rotr(&e, 6), rotr(&e, 11), rotr(&e, 25));
        let mut sig1 = [zero; 32];
        for j in 0..32 {
            sig1[j] = b.xor3(r6[j], r11[j], r25[j]);
        }
        // Ch(e,f,g) = (e&f) ^ (!e&g)
        let mut ch = [zero; 32];
        for j in 0..32 {
            let ef = b.and(e[j], f[j]);
            let ne = b.not(e[j]);
            let neg = b.and(ne, g[j]);
            ch[j] = b.xor(ef, neg);
        }
        let kbits = b.const_bits(K256[t]);
        let kw = b.add32(&kbits, &w[t]);
        let t1 = b.add_many(&[&hh, &sig1, &ch, &kw]);
        // Σ0(a) and Maj.
        let (r2, r13, r22) = (rotr(&a, 2), rotr(&a, 13), rotr(&a, 22));
        let mut sig0 = [zero; 32];
        for j in 0..32 {
            sig0[j] = b.xor3(r2[j], r13[j], r22[j]);
        }
        let mut maj = [zero; 32];
        for j in 0..32 {
            let ab = b.and(a[j], bb[j]);
            let ac = b.and(a[j], c[j]);
            let bc = b.and(bb[j], c[j]);
            let x = b.xor(ab, ac);
            maj[j] = b.xor(x, bc);
        }
        let t2 = b.add32(&sig0, &maj);
        st[7] = g;
        st[6] = f;
        st[5] = e;
        st[4] = b.add32(&d, &t1);
        st[3] = c;
        st[2] = bb;
        st[1] = a;
        st[0] = b.add32(&t1, &t2);
    }
    let mut out = [[zero; 32]; 8];
    for i in 0..8 {
        out[i] = b.add32(&h[i], &st[i]);
    }
    out
}

/// Build the block boundary: `msg_bytes` of message become the padded
/// 512-bit blocks (padding constants included), each returned as 16 words
/// of 32 LSB-first value bits. `msg_wire` must return the wire holding
/// message byte `i` bit `j` (LSB-first).
fn pad_blocks<F: FnMut(&mut Builder, usize, usize) -> u32>(
    b: &mut Builder,
    msg_bytes: usize,
    mut msg_wire: F,
) -> Vec<[[u32; 32]; 16]> {
    let total = msg_bytes + 1 + 8;
    let n_blocks = total.div_ceil(64);
    let len_bits = (msg_bytes as u64) * 8;
    let mut blocks = Vec::with_capacity(n_blocks);
    for blk in 0..n_blocks {
        let mut words = [[0u32; 32]; 16];
        for (wi, word) in words.iter_mut().enumerate() {
            for k in 0..32 {
                // value bit k of a BE word = bit (k%8) of byte (3 - k/8).
                let byte_idx = blk * 64 + wi * 4 + (3 - k / 8);
                let bit = k % 8;
                let wire = if byte_idx < msg_bytes {
                    msg_wire(b, byte_idx, bit)
                } else if byte_idx == msg_bytes {
                    if bit == 7 {
                        b.const1()
                    } else {
                        b.const0()
                    }
                } else if byte_idx >= blk * 64 + 56 && blk == n_blocks - 1 {
                    // 64-bit big-endian length at the final block's tail.
                    let len_byte = byte_idx - (blk * 64 + 56);
                    let lv = (len_bits >> (8 * (7 - len_byte))) & 0xff;
                    if (lv >> bit) & 1 == 1 {
                        b.const1()
                    } else {
                        b.const0()
                    }
                } else {
                    b.const0()
                };
                word[k] = wire;
            }
        }
        blocks.push(words);
    }
    blocks
}

/// Byte-boundary helpers: LSB-first bits of each byte in order.
fn emit_digest(b: &mut Builder, h: &[[u32; 32]; 8]) -> Vec<u32> {
    // Output byte (4i + m) bit j = h[i] value bit (8*(3-m) + j).
    let mut out = Vec::with_capacity(256);
    for word in h.iter() {
        for m in 0..4 {
            for j in 0..8 {
                out.push(word[8 * (3 - m) + j]);
            }
        }
    }
    let _ = b;
    out
}

fn finish(b: Builder, params: u32, outputs: Vec<u32>) -> BIrBlocks {
    BIrBlocks {
        blocks: vec![BIrBlock {
            params,
            stmts: b.stmts,
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: outputs.into_iter().map(IRVarId).collect(),
            }),
        }],
        pre_init: vec![],
    }
}

/// SHA-256 of a fixed-length message.
///
/// Inputs: `msg_bytes * 8` bits (message bytes in order, LSB-first per
/// byte). Outputs: 256 bits (the digest, same byte layout).
pub fn build_sha256(msg_bytes: usize) -> BIrBlocks {
    let params = (msg_bytes * 8) as u32;
    let mut b = Builder::new(params);
    let blocks = pad_blocks(&mut b, msg_bytes, |_b, byte, bit| (byte * 8 + bit) as u32);
    let mut h: [[u32; 32]; 8] = [[0; 32]; 8];
    for (i, word) in h.iter_mut().enumerate() {
        *word = b.const_bits(H0_256[i]);
    }
    for blk in &blocks {
        h = compress(&mut b, &h, blk);
    }
    let outputs = emit_digest(&mut b, &h);
    finish(b, params, outputs)
}

/// HMAC-SHA256 with fixed key/message geometry.
///
/// Inputs: `key_bytes*8 ++ msg_bytes*8` bits. Output: 256-bit tag. Keys of
/// any length ≤ 64 bytes hash directly into the pad block (RFC 2104); keys
/// longer than 64 bytes are rejected (TLS uses ≤ 32-byte keys).
pub fn build_hmac_sha256(key_bytes: usize, msg_bytes: usize) -> BIrBlocks {
    assert!(key_bytes <= 64, "HMAC gadget keys must fit one block");
    let params = ((key_bytes + msg_bytes) * 8) as u32;
    let key_bits = key_bytes * 8;
    let mut b = Builder::new(params);
    let tag = hmac_wires(
        &mut b,
        key_bytes,
        msg_bytes,
        |byte, bit| (byte * 8 + bit) as u32,
        |byte, bit| (key_bits + (byte * 8 + bit)) as u32,
    );
    finish(b, params, tag)
}

/// HKDF-SHA256 (RFC 5869) with fixed geometry: `extract(salt, ikm)` then
/// `expand(info, out_bytes)` (out_bytes ≤ 255*32).
///
/// Inputs: `salt_bytes*8 ++ ikm_bytes*8 ++ info_bytes*8` bits, in that
/// order. Outputs: `out_bytes*8` bits (the OKM). The intermediate PRK stays
/// internal (garbled) — it is never an output.
pub fn build_hkdf_sha256(salt_bytes: usize, ikm_bytes: usize, info_bytes: usize, out_bytes: usize) -> BIrBlocks {
    assert!(out_bytes <= 255 * 32 && out_bytes > 0);
    let n_t = out_bytes.div_ceil(32);
    let params = ((salt_bytes + ikm_bytes + info_bytes) * 8) as u32;
    let mut b = Builder::new(params);

    // To compose the fixed-geometry HMAC circuits inline we inline each
    // HMAC invocation: extract = HMAC(salt, ikm), then the expand chain.
    // Rather than re-emitting HMAC sub-circuits, we build the whole HKDF
    // directly with the same pad_blocks/compress primitives so intermediate
    // digests stay as wires.
    let salt_off = 0usize;
    let ikm_off = salt_bytes;
    let info_off = salt_bytes + ikm_bytes;

    // HMAC-Extract: prk = HMAC(key=salt, msg=ikm).
    let prk = hmac_wires(
        &mut b,
        salt_bytes,
        ikm_bytes,
        |byte, bit| ((salt_off + byte) * 8 + bit) as u32,
        |byte, bit| ((ikm_off + byte) * 8 + bit) as u32,
    );
    // Expand: T(0) = empty; T(i) = HMAC(prk, T(i-1) || info || i).
    let mut t_prev: Vec<u32> = Vec::new();
    let mut okm: Vec<u32> = Vec::new();
    for i in 1..=n_t {
        let t_len = t_prev.len() / 8;
        let msg_len = t_len + info_bytes + 1;
        // Counter byte materialized before the closures so they needn't
        // touch the builder.
        let ctr: [u32; 8] = core::array::from_fn(|bit| {
            if (i >> bit) & 1 == 1 {
                b.const1()
            } else {
                b.const0()
            }
        });
        let t_i = hmac_wires(
            &mut b,
            32,
            msg_len,
            |byte, bit| prk[byte * 8 + bit],
            |byte, bit| {
                if byte < t_len {
                    t_prev[byte * 8 + bit]
                } else if byte < t_len + info_bytes {
                    ((info_off + byte - t_len) * 8 + bit) as u32
                } else {
                    ctr[bit]
                }
            },
        );
        okm.extend_from_slice(&t_i);
        t_prev = t_i;
    }
    okm.truncate(out_bytes * 8);
    finish(b, params, okm)
}

/// HMAC-SHA256 over wire sources: `key(byte, bit)` and `msg(byte, bit)`
/// return input/intermediate wires; returns the 256-bit tag as wires
/// (byte-major, LSB-first).
fn hmac_wires<K: FnMut(usize, usize) -> u32, M: FnMut(usize, usize) -> u32>(
    b: &mut Builder,
    key_bytes: usize,
    msg_bytes: usize,
    mut key: K,
    mut msg: M,
) -> Vec<u32> {
    assert!(key_bytes <= 64);
    let mut h: [[u32; 32]; 8] = [[0; 32]; 8];
    for (i, word) in h.iter_mut().enumerate() {
        *word = b.const_bits(H0_256[i]);
    }
    // Inner: (k ^ ipad) || msg.
    let inner = pad_blocks(b, 64 + msg_bytes, |b, byte, bit| {
        if byte < 64 {
            let w = if byte < key_bytes {
                key(byte, bit)
            } else {
                b.const0()
            };
            if (0x36u8 >> bit) & 1 == 1 {
                b.not(w)
            } else {
                w
            }
        } else {
            msg(byte - 64, bit)
        }
    });
    for blk in &inner {
        h = compress(b, &h, blk);
    }
    let inner_digest = emit_digest(b, &h);
    // Outer: (k ^ opad) || inner_digest.
    let mut h: [[u32; 32]; 8] = [[0; 32]; 8];
    for (i, word) in h.iter_mut().enumerate() {
        *word = b.const_bits(H0_256[i]);
    }
    let outer = pad_blocks(b, 64 + 32, |b, byte, bit| {
        if byte < 64 {
            let w = if byte < key_bytes {
                key(byte, bit)
            } else {
                b.const0()
            };
            if (0x5cu8 >> bit) & 1 == 1 {
                b.not(w)
            } else {
                w
            }
        } else {
            inner_digest[(byte - 64) * 8 + bit]
        }
    });
    for blk in &outer {
        h = compress(b, &h, blk);
    }
    emit_digest(b, &h)
}
