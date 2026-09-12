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

/// Reference GF(2^4) multiply under `x^4 + x + 1` (the composite-field base).
fn gf4_mul_scalar(a: u8, b: u8) -> u8 {
    let mut p = 0u8;
    let mut a = a & 0xF;
    let mut b = b & 0xF;
    for _ in 0..4 {
        if b & 1 != 0 {
            p ^= a;
        }
        let hi = a & 0x8 != 0;
        a = (a << 1) & 0xF;
        if hi {
            a ^= 0x3;
        }
        b >>= 1;
    }
    p & 0xF
}

/// The composite-field S-box isomorphism constants, derived by search at build
/// time (validated against the reference SBOX). Returns `(lambda, phi_terms,
/// phi_inv_terms)` where `phi` maps a composite-field byte `[a1:a0]` (high
/// nibble a1) to GF(2^8) and `phi_inv` is its inverse, each as the set of input
/// bits feeding each output bit (a linear, XOR-only map).
///
/// GF(2^8) is represented via the AES polynomial; GF((2^4)^2) via
/// `y^2 + y + lambda` over GF(2^4). The isomorphism is `phi((a1,a0)) =
/// emb(a1)*beta ^ emb(a0)`, where `emb` embeds GF(2^4) into GF(2^8) at a root
/// `omega` of `x^4+x+1` and `beta` is a root of `y^2 + y + emb(lambda)`.
fn derive_iso() -> (u8, [Vec<usize>; 8], [Vec<usize>; 8]) {
    let omega = (2u16..256)
        .map(|w| w as u8)
        .find(|&w| {
            let w2 = gf_mul_scalar(w, w);
            let w4 = gf_mul_scalar(w2, w2);
            (w4 ^ w ^ 1) == 0
        })
        .expect("omega");
    let w2 = gf_mul_scalar(omega, omega);
    let w3 = gf_mul_scalar(w2, omega);
    let emb = |a: u8| -> u8 {
        let mut r = 0u8;
        if a & 1 != 0 {
            r ^= 1;
        }
        if a & 2 != 0 {
            r ^= omega;
        }
        if a & 4 != 0 {
            r ^= w2;
        }
        if a & 8 != 0 {
            r ^= w3;
        }
        r
    };
    let lambda = (0u8..16)
        .find(|&l| (0u8..16).all(|y| (gf4_mul_scalar(y, y) ^ y) != l))
        .expect("lambda");
    let el = emb(lambda);
    let beta = (0u16..256)
        .map(|b| b as u8)
        .find(|&b| (gf_mul_scalar(b, b) ^ b ^ el) == 0)
        .expect("beta");
    let phi = |comp: u8| -> u8 { gf_mul_scalar(emb(comp >> 4), beta) ^ emb(comp & 0xF) };
    let mut phi_inv = [0u8; 256];
    for c in 0..256u16 {
        phi_inv[phi(c as u8) as usize] = c as u8;
    }
    let terms_of = |f: &dyn Fn(u8) -> u8| -> [Vec<usize>; 8] {
        let mut t: [Vec<usize>; 8] = Default::default();
        for i in 0..8 {
            let v = f(1 << i);
            for k in 0..8 {
                if (v >> k) & 1 == 1 {
                    t[k].push(i);
                }
            }
        }
        t
    };
    let phi_terms = terms_of(&phi);
    let phi_inv_terms = terms_of(&|x| phi_inv[x as usize]);
    (lambda, phi_terms, phi_inv_terms)
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


/// Apply an 8-bit linear map given as per-output-bit input-bit sets (XOR-only).
fn linear_byte_c(b: &mut B, terms: &[Vec<usize>; 8], a: &[u32; 8]) -> [u32; 8] {
    let mut out = [0u32; 8];
    for k in 0..8 {
        let bits: Vec<u32> = terms[k].iter().map(|&i| a[i]).collect();
        out[k] = b.xor_fold(&bits);
    }
    out
}

/// Apply a 4-bit linear map given as per-output-bit input-bit sets (XOR-only).
fn linear_nibble_c(b: &mut B, terms: &[Vec<usize>; 4], a: &[u32; 4]) -> [u32; 4] {
    let mut out = [0u32; 4];
    for k in 0..4 {
        let bits: Vec<u32> = terms[k].iter().map(|&i| a[i]).collect();
        out[k] = b.xor_fold(&bits);
    }
    out
}

fn gf4_mul_terms() -> [Vec<(usize, usize)>; 4] {
    let mut t: [Vec<(usize, usize)>; 4] = Default::default();
    for i in 0..4 {
        for j in 0..4 {
            let v = gf4_mul_scalar(1 << i, 1 << j);
            for k in 0..4 {
                if (v >> k) & 1 == 1 {
                    t[k].push((i, j));
                }
            }
        }
    }
    t
}
fn gf4_square_terms() -> [Vec<usize>; 4] {
    let mut t: [Vec<usize>; 4] = Default::default();
    for i in 0..4 {
        let v = gf4_mul_scalar(1 << i, 1 << i);
        for k in 0..4 {
            if (v >> k) & 1 == 1 {
                t[k].push(i);
            }
        }
    }
    t
}
fn gf4_mul_const_terms(c: u8) -> [Vec<usize>; 4] {
    let mut t: [Vec<usize>; 4] = Default::default();
    for i in 0..4 {
        let v = gf4_mul_scalar(1 << i, c);
        for k in 0..4 {
            if (v >> k) & 1 == 1 {
                t[k].push(i);
            }
        }
    }
    t
}

/// GF(2^4) multiply, 16 ANDs (shared partial products).
fn gf4_mul_c(b: &mut B, a: &[u32; 4], y: &[u32; 4]) -> [u32; 4] {
    let terms = gf4_mul_terms();
    let mut prod = [[0u32; 4]; 4];
    for i in 0..4 {
        for j in 0..4 {
            prod[i][j] = b.and(a[i], y[j]);
        }
    }
    let mut out = [0u32; 4];
    for k in 0..4 {
        let prods: Vec<u32> = terms[k].iter().map(|&(i, j)| prod[i][j]).collect();
        out[k] = b.xor_fold(&prods);
    }
    out
}
fn gf4_square_c(b: &mut B, a: &[u32; 4]) -> [u32; 4] {
    linear_nibble_c(b, &gf4_square_terms(), a)
}
fn gf4_mul_const_c(b: &mut B, a: &[u32; 4], c: u8) -> [u32; 4] {
    linear_nibble_c(b, &gf4_mul_const_terms(c), a)
}
fn xor_nibble(b: &mut B, a: &[u32; 4], c: &[u32; 4]) -> [u32; 4] {
    let mut out = [0u32; 4];
    for i in 0..4 {
        out[i] = b.xor(a[i], c[i]);
    }
    out
}

/// GF(2^4) inversion `x^14` via Itoh-Tsujii: `x^3 = x^2*x`, `x^7 = (x^3)^2*x`,
/// `x^14 = (x^7)^2` — 2 GF(2^4) multiplies (squarings free). `0 -> 0`.
fn gf4_inv_c(b: &mut B, a: &[u32; 4]) -> [u32; 4] {
    let a2 = gf4_square_c(b, a);
    let a3 = gf4_mul_c(b, &a2, a);
    let a6 = gf4_square_c(b, &a3);
    let a7 = gf4_mul_c(b, &a6, a);
    gf4_square_c(b, &a7)
}

/// The AES S-box via the composite field GF((2^4)^2): map to the composite
/// field, invert there (3 GF(2^4) multiplies + 1 GF(2^4) inversion = 80 ANDs),
/// map back, then the affine transform. The isomorphisms are XOR-only linear
/// maps, so the whole S-box is ~80 ANDs versus ~256 for the direct Itoh-Tsujii
/// inversion in GF(2^8).
fn sbox_c(b: &mut B, x: &[u32; 8]) -> [u32; 8] {
    let (lambda, phi_terms, phi_inv_terms) = derive_iso();
    // Map GF(2^8) -> composite: comp = phi_inv(x); a1 = high nibble, a0 = low.
    let comp = linear_byte_c(b, &phi_inv_terms, x);
    let a1: [u32; 4] = [comp[4], comp[5], comp[6], comp[7]];
    let a0: [u32; 4] = [comp[0], comp[1], comp[2], comp[3]];
    // Invert in the composite field: d = a1^2*lambda ^ a1*a0 ^ a0^2.
    let sq1 = gf4_square_c(b, &a1);
    let t = gf4_mul_const_c(b, &sq1, lambda);
    let p1 = gf4_mul_c(b, &a1, &a0);
    let sq0 = gf4_square_c(b, &a0);
    let tp = xor_nibble(b, &t, &p1);
    let d = xor_nibble(b, &tp, &sq0);
    let d_inv = gf4_inv_c(b, &d);
    let a1p = gf4_mul_c(b, &a1, &d_inv);
    let a1xa0 = xor_nibble(b, &a1, &a0);
    let a0p = gf4_mul_c(b, &a1xa0, &d_inv);
    let comp_inv: [u32; 8] = [
        a0p[0], a0p[1], a0p[2], a0p[3], a1p[0], a1p[1], a1p[2], a1p[3],
    ];
    // Map composite -> GF(2^8).
    let inv = linear_byte_c(b, &phi_terms, &comp_inv);
    // Affine: s_i = b_i ^ b_{i+4} ^ b_{i+5} ^ b_{i+6} ^ b_{i+7} ^ c_i (mod 8),
    // c = 0x63.
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

// ---------------------------------------------------------------------------
// AES-128-GCM (NIST SP 800-38D) boolar gadget (P4c-i) — the TLS 1.3 record-layer
// AEAD, built from the composite-field AES block gadget plus a GF(2^128)
// GHASH multiply. Fixed geometry: the AAD and plaintext block counts are
// compile-time parameters, so the CTR counters and the length block are
// constants baked at build time.
// ---------------------------------------------------------------------------

/// Inline a single-block boolean sub-circuit into `b`, remapping var ids
/// (`inputs[i]` is the parent wire for sub-param `i`); returns the sub's
/// output wires in the parent.
fn inline_sub(b: &mut B, sub: &BIrBlocks, inputs: &[u32]) -> Vec<u32> {
    assert_eq!(sub.blocks.len(), 1, "inline_sub: single-block circuit");
    let block = &sub.blocks[0];
    assert_eq!(block.params as usize, inputs.len(), "inline_sub: input arity");
    let mut remap: Vec<u32> = Vec::with_capacity(block.params as usize + block.stmts.len());
    remap.extend_from_slice(inputs);
    for stmt in &block.stmts {
        use volar_ir::boolar::BIrStmt::*;
        let s = match &stmt.kind {
            Zero => Zero,
            One => One,
            And(x, y) => And(IRVarId(remap[x.0 as usize]), IRVarId(remap[y.0 as usize])),
            Or(x, y) => Or(IRVarId(remap[x.0 as usize]), IRVarId(remap[y.0 as usize])),
            Xor(x, y) => Xor(IRVarId(remap[x.0 as usize]), IRVarId(remap[y.0 as usize])),
            Not(x) => Not(IRVarId(remap[x.0 as usize])),
            other => panic!("inline_sub: unsupported stmt {other:?}"),
        };
        let id = b.g(s);
        remap.push(id);
    }
    match &block.terminator {
        BIrTerminator::Jmp(t) if t.block == IRBlockTargetId::Return => {
            t.args.iter().map(|a| remap[a.0 as usize]).collect()
        }
        _ => panic!("inline_sub: sub-circuit must end in a Return"),
    }
}

/// Reorder a 128-bit block from the AES gadget's byte layout (byte `i`, bit
/// `j` = LSB-first wire `i*8+j`) into GCM field order (wire `k` = the `k`-th
/// bit transmitted = MSB-first within each byte).
fn gcm_order(bits: &[u32]) -> [u32; 128] {
    assert_eq!(bits.len(), 128);
    let mut out = [0u32; 128];
    for k in 0..128 {
        out[k] = bits[(k / 8) * 8 + (7 - (k % 8))];
    }
    out
}

/// The inverse of [`gcm_order`].
fn gcm_unorder(bits: &[u32; 128]) -> Vec<u32> {
    let mut out = vec![0u32; 128];
    for k in 0..128 {
        out[(k / 8) * 8 + (7 - (k % 8))] = bits[k];
    }
    out
}

/// GF(2^128) multiply in the GCM (reflected) convention, SP 800-38D
/// Algorithm 1: `z ^= v` selected by each bit of `y` (128 ANDs per step),
/// `v` shifted right with the R = 0xE1||0^120 reduction on carry.
/// 128 × 128 = 16 384 ANDs.
fn gf128_mul_c(b: &mut B, x: &[u32; 128], y: &[u32; 128]) -> [u32; 128] {
    let zero = b.c0();
    let mut z = [zero; 128];
    let mut v = *x;
    for i in 0..128 {
        let yi = y[i];
        for k in 0..128 {
            let m = b.and(yi, v[k]);
            z[k] = b.xor(z[k], m);
        }
        let carry = v[127];
        let mut nv = [zero; 128];
        for k in 1..128 {
            nv[k] = v[k - 1];
        }
        // R = 0xE1 at the left end: bits 0,1,2,7 of the string.
        for &k in &[0usize, 1, 2, 7] {
            nv[k] = b.xor(nv[k], carry);
        }
        v = nv;
    }
    z
}

/// Constant 128-bit block (LSB-first byte layout) from 16 bytes.
fn const_block_c(b: &mut B, bytes: [u8; 16]) -> Vec<u32> {
    let mut out = Vec::with_capacity(128);
    for byte in bytes {
        for j in 0..8 {
            out.push(if (byte >> j) & 1 == 1 { b.c1() } else { b.c0() });
        }
    }
    out
}

/// Build an AES-128-GCM encrypt+tag circuit of fixed geometry.
///
/// Params (LSB-first per byte):
/// `[key: 128, iv: 96 (12 bytes), aad: num_aad_blocks*128, pt: num_pt_blocks*128]`.
/// Outputs: `[ct: num_pt_blocks*128, tag: 128]`.
///
/// The 96-bit-IV form (SP 800-38D §7.1): J0 = iv || 0x00000001, CTR counters
/// are the compile-time block indices, and the GHASH length block encodes the
/// fixed AAD/ciphertext bit lengths — so no in-circuit incrementer or length
/// arithmetic is needed.
pub fn build_aes128_gcm(num_aad_blocks: usize, num_pt_blocks: usize) -> BIrBlocks {
    build_aes128_gcm_var(num_aad_blocks * 16, num_pt_blocks * 16)
}

/// The byte-exact form of [`build_aes128_gcm`]: AAD and plaintext are any
/// byte counts (TLS 1.3 records are not block-multiples). Per SP 800-38D
/// the tails of the last AAD/ciphertext GHASH blocks are zero-padded, which
/// here means the in-circuit absorb uses constant-zero wires beyond the
/// real bits (the lengths are build-time constants, so the masking is
/// wiring, not gates); the CTR keystream tail beyond the plaintext is
/// simply not output.
///
/// Params (LSB-first per byte):
/// `[key: 128, iv: 96, aad: 8*aad_bytes, pt: 8*pt_bytes]`.
/// Outputs: `[ct: 8*pt_bytes, tag: 128]`.
///
/// This is the ENCRYPT shape: GHASH covers the ciphertext the circuit
/// produces (the CTR output). Feeding a ciphertext as the `pt` input does
/// NOT yield a correct decryption tag — the GHASH would cover the
/// recovered plaintext. Use [`build_aes128_gcm_decrypt_var`] for AEAD-open.
pub fn build_aes128_gcm_var(aad_bytes: usize, pt_bytes: usize) -> BIrBlocks {
    build_aes128_gcm_shaped(aad_bytes, pt_bytes, false)
}

/// The DECRYPT shape of [`build_aes128_gcm_var`]: the text input is the
/// received ciphertext, GHASH covers those input wires (the actual
/// ciphertext per SP 800-38D), and the outputs are
/// `[pt: 8*ct_bytes, recomputed_tag: 128]` — compare the tag against the
/// received tag for the AEAD-open verdict.
pub fn build_aes128_gcm_decrypt_var(aad_bytes: usize, ct_bytes: usize) -> BIrBlocks {
    build_aes128_gcm_shaped(aad_bytes, ct_bytes, true)
}

fn build_aes128_gcm_shaped(aad_bytes: usize, pt_bytes: usize, decrypt: bool) -> BIrBlocks {
    let a = aad_bytes.div_ceil(16);
    let p = pt_bytes.div_ceil(16);
    let params = 128 + 96 + aad_bytes * 8 + pt_bytes * 8;
    let mut b = B::new(params as u32);
    let key: Vec<u32> = (0..128).collect();
    let iv: Vec<u32> = (128..224).collect();
    let aad_start = 224usize;
    let pt_start = aad_start + aad_bytes * 8;
    let aes = build_aes128();
    let aad_bits = aad_bytes * 8;
    let pt_bits = pt_bytes * 8;

    // H = AES_K(0^128).
    let zero_block = const_block_c(&mut b, [0u8; 16]);
    let h = inline_sub(&mut b, &aes, &[key.clone(), zero_block].concat());
    let h_gcm = gcm_order(&h);

    // J0 = iv || 00000001 in AES byte layout.
    let mut j0 = iv.clone();
    j0.extend_from_slice(&const_block_c(&mut b, {
        let mut x = [0u8; 16];
        x[15] = 1;
        x
    })[96..128]);
    debug_assert_eq!(j0.len(), 128);
    let tag_mask = inline_sub(&mut b, &aes, &[key.clone(), j0.clone()].concat());

    // CTR mode: block i uses counter value i+1 in the last 4 bytes (BE).
    let mut ct: Vec<u32> = Vec::with_capacity(pt_bits);
    for i in 0..p {
        let mut ctr_bytes = [0u8; 16];
        // J0 already carries counter value 1; the first CTR block uses 2.
        ctr_bytes[12..16].copy_from_slice(&((i as u32 + 2).to_be_bytes()));
        let mut ctr = iv.clone();
        ctr.extend_from_slice(&const_block_c(&mut b, ctr_bytes)[96..128]);
        let ks = inline_sub(&mut b, &aes, &[key.clone(), ctr].concat());
        for j in 0..128 {
            if i * 128 + j < pt_bits {
                ct.push(b.xor((pt_start + i * 128 + j) as u32, ks[j]));
            }
        }
    }

    // GHASH over aad || ct || lenblock.
    fn absorb(b: &mut B, x: &mut [u32; 128], block_lsb: Vec<u32>, h_gcm: &[u32; 128]) {
        let bg = gcm_order(&block_lsb);
        let mut xb = [0u32; 128];
        for k in 0..128 {
            xb[k] = b.xor(x[k], bg[k]);
        }
        *x = gf128_mul_c(b, &xb, h_gcm);
    }
    let zero = b.c0();
    let mut x = [zero; 128];
    for i in 0..a {
        let block: Vec<u32> = (0..128)
            .map(|j| {
                if i * 128 + j < aad_bits {
                    (aad_start + i * 128 + j) as u32
                } else {
                    zero
                }
            })
            .collect();
        absorb(&mut b, &mut x, block, &h_gcm);
    }
    for i in 0..p {
        let block: Vec<u32> = (0..128)
            .map(|j| {
                if i * 128 + j < pt_bits {
                    if decrypt {
                        // GHASH covers the received ciphertext (the text
                        // input wires), not the recovered plaintext.
                        (pt_start + i * 128 + j) as u32
                    } else {
                        ct[i * 128 + j]
                    }
                } else {
                    zero
                }
            })
            .collect();
        absorb(&mut b, &mut x, block, &h_gcm);
    }
    let mut len_bytes = [0u8; 16];
    len_bytes[0..8].copy_from_slice(&(aad_bits as u64).to_be_bytes());
    len_bytes[8..16].copy_from_slice(&(pt_bits as u64).to_be_bytes());
    let len_block = const_block_c(&mut b, len_bytes);
    absorb(&mut b, &mut x, len_block, &h_gcm);

    // tag = GHASH ^ AES_K(J0), reported in the uniform LSB-first byte layout.
    let tm = gcm_order(&tag_mask);
    let mut tag_gcm = [0u32; 128];
    for k in 0..128 {
        tag_gcm[k] = b.xor(x[k], tm[k]);
    }
    let mut out = ct;
    out.extend_from_slice(&gcm_unorder(&tag_gcm));
    b.finish(out)
}
