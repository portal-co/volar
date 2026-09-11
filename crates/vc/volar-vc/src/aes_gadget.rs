// @reliability: experimental
// @ai: assisted
//! Bit-level (boolar) **AES-128 gadget** — the PRF for the S5 encrypted ORAM
//! tree, built to mirror `volar_spec::faest::aes` (the FAEST reference) and
//! validated against its FIPS-197 known-answer vectors.
//!
//! Per the user direction, the tree pads use AES as the PRF (a fixed-length
//! gadget), with FAEST-style cost reduction as the S6 hardening target. The
//! S-box here is `affine(x^254)` in GF(2^8) built by square-and-multiply (6
//! GF multiplies of ≤ 64 ANDs each); the linear maps (mix-columns ×2/×3, the
//! affine, squaring) are XOR-only. The gate networks for the GF(2^8) operations
//! are *derived from the reference* `gf_mul`/`SBOX` at build time (each output
//! bit's contributing terms are read off the scalar function), so the circuit
//! is correct by construction and checked against the FIPS KATs.
//!
//! Layout: params = `[key: 128, plaintext: 128]` (byte-major, LSB-first within
//! a byte); outputs = the 128-bit ciphertext block.

use alloc::vec;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;

/// Reference GF(2^8) multiply under the AES polynomial (0x11b / 0x1b),
/// identical to `volar_spec::faest::aes::gf_mul`. Used at build time to derive
/// the gate network.
fn gf_mul_scalar(mut a: u8, mut b: u8) -> u8 {
    let mut p = 0u8;
    for _ in 0..8 {
        if b & 1 != 0 {
            p ^= a;
        }
        let hi = a & 0x80 != 0;
        a <<= 1;
        if hi {
            a ^= 0x1b;
        }
        b >>= 1;
    }
    p
}

/// A boolar circuit under construction.
struct B {
    params: u32,
    stmts: Vec<Node<BIrStmt, ()>>,
}

impl B {
    fn new(params: u32) -> Self {
        Self {
            params,
            stmts: Vec::new(),
        }
    }
    fn g(&mut self, s: BIrStmt) -> u32 {
        let id = self.params + self.stmts.len() as u32;
        self.stmts.push(Node::new(s, (), None));
        id
    }
    fn c0(&mut self) -> u32 {
        self.g(BIrStmt::Zero)
    }
    fn c1(&mut self) -> u32 {
        self.g(BIrStmt::One)
    }
    fn and(&mut self, a: u32, b: u32) -> u32 {
        self.g(BIrStmt::And(IRVarId(a), IRVarId(b)))
    }
    fn xor(&mut self, a: u32, b: u32) -> u32 {
        self.g(BIrStmt::Xor(IRVarId(a), IRVarId(b)))
    }
    fn not(&mut self, a: u32) -> u32 {
        self.g(BIrStmt::Not(IRVarId(a)))
    }
    /// XOR-fold of `bits`, or the constant-0 wire when empty.
    fn xor_fold(&mut self, bits: &[u32]) -> u32 {
        match bits.len() {
            0 => self.c0(),
            1 => bits[0],
            _ => {
                let mut acc = bits[0];
                for &x in &bits[1..] {
                    acc = self.xor(acc, x);
                }
                acc
            }
        }
    }
    fn xor_byte(&mut self, a: &[u32; 8], b: &[u32; 8]) -> [u32; 8] {
        let mut out = [0u32; 8];
        for i in 0..8 {
            out[i] = self.xor(a[i], b[i]);
        }
        out
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

/// The set of `(i, j)` AND terms feeding each output bit of GF(2^8) multiply,
/// read off the bilinear reference: term `(i, j)` contributes to output `k`
/// iff `gf_mul(1<<i, 1<<j)` has bit `k` set.
fn gf_mul_terms() -> [Vec<(usize, usize)>; 8] {
    let mut t: [Vec<(usize, usize)>; 8] = Default::default();
    for i in 0..8 {
        for j in 0..8 {
            let v = gf_mul_scalar(1 << i, 1 << j);
            for k in 0..8 {
                if (v >> k) & 1 == 1 {
                    t[k].push((i, j));
                }
            }
        }
    }
    t
}

/// The set of input bits feeding each output bit of a *linear* GF(2^8) map
/// `x -> gf_mul(x, c)` for a constant `c`.
fn gf_mul_const_terms(c: u8) -> [Vec<usize>; 8] {
    let mut t: [Vec<usize>; 8] = Default::default();
    for i in 0..8 {
        let v = gf_mul_scalar(1 << i, c);
        for k in 0..8 {
            if (v >> k) & 1 == 1 {
                t[k].push(i);
            }
        }
    }
    t
}

/// Squaring in GF(2^8) is linear: `x -> gf_mul(x, x)` restricted to the
/// diagonal, i.e. the linear map derived from the reference.
fn gf_square_terms() -> [Vec<usize>; 8] {
    let mut t: [Vec<usize>; 8] = Default::default();
    for i in 0..8 {
        let v = gf_mul_scalar(1 << i, 1 << i);
        for k in 0..8 {
            if (v >> k) & 1 == 1 {
                t[k].push(i);
            }
        }
    }
    t
}

/// GF(2^8) multiply of two bytes as a boolar circuit (≤ 64 ANDs).
fn gf_mul_c(b: &mut B, a: &[u32; 8], y: &[u32; 8]) -> [u32; 8] {
    let terms = gf_mul_terms();
    let mut out = [0u32; 8];
    for k in 0..8 {
        let ands: Vec<u32> = terms[k].iter().map(|&(i, j)| b.and(a[i], y[j])).collect();
        out[k] = b.xor_fold(&ands);
    }
    out
}

/// `x -> gf_mul(x, c)` for constant `c`, XOR-only.
fn gf_mul_const_c(b: &mut B, a: &[u32; 8], c: u8) -> [u32; 8] {
    let terms = gf_mul_const_terms(c);
    let mut out = [0u32; 8];
    for k in 0..8 {
        let bits: Vec<u32> = terms[k].iter().map(|&i| a[i]).collect();
        out[k] = b.xor_fold(&bits);
    }
    out
}

/// GF(2^8) squaring, XOR-only.
fn gf_square_c(b: &mut B, a: &[u32; 8]) -> [u32; 8] {
    let terms = gf_square_terms();
    let mut out = [0u32; 8];
    for k in 0..8 {
        let bits: Vec<u32> = terms[k].iter().map(|&i| a[i]).collect();
        out[k] = b.xor_fold(&bits);
    }
    out
}

/// The AES S-box: `affine(x^254)` in GF(2^8). `0^254 = 0`, and `affine(0) =
/// 0x63`, so the zero input needs no special-casing. Square-and-multiply for
/// the exponent `254 = 0b1111_1110`: 6 GF multiplies + 7 (free) squarings.
fn sbox_c(b: &mut B, x: &[u32; 8]) -> [u32; 8] {
    let mut r: Option<[u32; 8]> = None;
    for bit in (0..8).rev() {
        if let Some(prev) = r {
            r = Some(gf_square_c(b, &prev));
        }
        if (254u16 >> bit) & 1 == 1 {
            r = Some(match r {
                None => *x,
                Some(prev) => gf_mul_c(b, &prev, x),
            });
        }
    }
    let inv = r.expect("254 has a set bit");
    // Affine: s_i = b_i ^ b_{i+4} ^ b_{i+5} ^ b_{i+6} ^ b_{i+7} ^ c_i (indices
    // mod 8), c = 0x63 — the FIPS-197 affine transform.
    let c = 0x63u8;
    let mut out = [0u32; 8];
    for i in 0..8 {
        let terms = [
            inv[i],
            inv[(i + 4) % 8],
            inv[(i + 5) % 8],
            inv[(i + 6) % 8],
            inv[(i + 7) % 8],
        ];
        let mut v = b.xor_fold(&terms);
        if (c >> i) & 1 == 1 {
            let one = b.c1();
            v = b.xor(v, one);
        }
        out[i] = v;
    }
    out
}

/// Byte-level helpers over the boolar state (16 bytes × 8 bits, column-major
/// like the reference).
type State = [[u32; 8]; 16];

fn sub_bytes_c(b: &mut B, s: &mut State) {
    for i in 0..16 {
        s[i] = sbox_c(b, &s[i]);
    }
}

fn shift_rows_c(s: &mut State) {
    // Pure rewiring (no gates), mirroring the reference `shift_rows`.
    let t = s[1];
    s[1] = s[5];
    s[5] = s[9];
    s[9] = s[13];
    s[13] = t;
    let t = s[2];
    s[2] = s[10];
    s[10] = t;
    let t = s[6];
    s[6] = s[14];
    s[14] = t;
    let t = s[15];
    s[15] = s[11];
    s[11] = s[7];
    s[7] = s[3];
    s[3] = t;
}

fn mix_columns_c(b: &mut B, s: &mut State) {
    for c in 0..4 {
        let i = 4 * c;
        let (s0, s1, s2, s3) = (s[i], s[i + 1], s[i + 2], s[i + 3]);
        let two0 = gf_mul_const_c(b, &s0, 2);
        let three0 = gf_mul_const_c(b, &s0, 3);
        let two1 = gf_mul_const_c(b, &s1, 2);
        let three1 = gf_mul_const_c(b, &s1, 3);
        let two2 = gf_mul_const_c(b, &s2, 2);
        let three2 = gf_mul_const_c(b, &s2, 3);
        let two3 = gf_mul_const_c(b, &s3, 2);
        let three3 = gf_mul_const_c(b, &s3, 3);
        // col = (2·s0 ^ 3·s1 ^ s2 ^ s3, s0 ^ 2·s1 ^ 3·s2 ^ s3, ...)
        let col0 = xor4(b, &two0, &three1, &s2, &s3);
        let col1 = xor4(b, &s0, &two1, &three2, &s3);
        let col2 = xor4(b, &s0, &s1, &two2, &three3);
        let col3 = xor4(b, &three0, &s1, &s2, &two3);
        s[i] = col0;
        s[i + 1] = col1;
        s[i + 2] = col2;
        s[i + 3] = col3;
    }
}

fn xor4(b: &mut B, a: &[u32; 8], c: &[u32; 8], d: &[u32; 8], e: &[u32; 8]) -> [u32; 8] {
    let mut out = [0u32; 8];
    for i in 0..8 {
        let x = b.xor(a[i], c[i]);
        let y = b.xor(d[i], e[i]);
        out[i] = b.xor(x, y);
    }
    out
}

fn add_round_key_c(b: &mut B, s: &mut State, rk: &State) {
    for i in 0..16 {
        s[i] = b.xor_byte(&s[i], &rk[i]);
    }
}

const RCON: [u8; 11] = [
    0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36,
];

/// Key expansion producing the 11 round keys as boolar states.
fn key_expansion_c(b: &mut B, key: &State) -> [State; 11] {
    // words[i] = 4-byte word i (i in 0..44).
    let mut words: Vec<[[u32; 8]; 4]> = Vec::with_capacity(44);
    for i in 0..4 {
        words.push([key[4 * i], key[4 * i + 1], key[4 * i + 2], key[4 * i + 3]]);
    }
    for i in 4..44 {
        let mut temp = words[i - 1];
        if i % 4 == 0 {
            // RotWord.
            temp = [temp[1], temp[2], temp[3], temp[0]];
            // SubWord.
            for j in 0..4 {
                temp[j] = sbox_c(b, &temp[j]);
            }
            // RCON on byte 0.
            let rc = RCON[i / 4];
            for bit in 0..8 {
                if (rc >> bit) & 1 == 1 {
                    let one = b.c1();
                    temp[0][bit] = b.xor(temp[0][bit], one);
                }
            }
        }
        let mut w = [[0u32; 8]; 4];
        for j in 0..4 {
            w[j] = b.xor_byte(&words[i - 4][j], &temp[j]);
        }
        words.push(w);
    }
    let mut rks: Vec<State> = Vec::with_capacity(11);
    for r in 0..11 {
        let mut st: State = [[0u32; 8]; 16];
        for c in 0..4 {
            let w = words[4 * r + c];
            st[4 * c] = w[0];
            st[4 * c + 1] = w[1];
            st[4 * c + 2] = w[2];
            st[4 * c + 3] = w[3];
        }
        rks.push(st);
    }
    rks.try_into().unwrap_or_else(|_| unreachable!("11 round keys"))
}

/// Build the AES-128 block-encryption circuit.
/// params = `[key: 128, plaintext: 128]` (byte-major, LSB-first in a byte);
/// output = 128-bit ciphertext.
pub fn build_aes128() -> BIrBlocks {
    let mut b = B::new(256);
    let mut key: State = [[0u32; 8]; 16];
    let mut plain: State = [[0u32; 8]; 16];
    for i in 0..16 {
        for j in 0..8 {
            key[i][j] = (i * 8 + j) as u32;
            plain[i][j] = (128 + i * 8 + j) as u32;
        }
    }
    let rks = key_expansion_c(&mut b, &key);
    let mut state = plain;
    add_round_key_c(&mut b, &mut state, &rks[0]);
    for r in 1..10 {
        sub_bytes_c(&mut b, &mut state);
        shift_rows_c(&mut state);
        mix_columns_c(&mut b, &mut state);
        add_round_key_c(&mut b, &mut state, &rks[r]);
    }
    sub_bytes_c(&mut b, &mut state);
    shift_rows_c(&mut state);
    add_round_key_c(&mut b, &mut state, &rks[10]);

    let mut out = Vec::with_capacity(128);
    for i in 0..16 {
        out.extend_from_slice(&state[i]);
    }
    b.finish(out)
}
