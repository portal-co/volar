//! Bit-level X25519 (RFC 7748) boolar gadget (P4c-ii) — the TLS 1.3 key
//! exchange in-circuit, so the handshake shared secret stays split between
//! the MPC parties (neither side alone holds a key it could use to forge
//! records).
//!
//! Construction: the Montgomery ladder over GF(2^255 - 19), field elements
//! as 255-bit LSB-first wires. Multiplication is a schoolbook AND-matrix
//! (~65k ANDs) plus a `2^255 ≡ 19` folding reduction; squaring collapses
//! the symmetric half of the matrix (~33k ANDs); inversion uses the short
//! addition chain for `p - 2 = 2^255 - 21` (254 squarings, 11 multiplies).
//!
//! Cost note: the full ladder is one straight-line circuit of ~0.5-1G ANDs
//! dominated by carry propagation in the schoolbook accumulation. That is
//! fine for concrete validation (the full-vector test is `#[ignore]`d), and
//! two-party it runs as a step loop (one ladder iteration per circuit, the
//! ladder state threaded as re-based labels per the S4 machinery). For
//! production the KEX is a candidate for the VOLE backend, where ANDs carry
//! no per-gate communication.
//!
//! Byte layout matches the other gadgets: inputs are the 32-byte scalar
//! then the 32-byte u-coordinate (bytes in order, LSB-first per byte); the
//! RFC 7748 clamping and the u-coordinate high-bit mask are applied by the
//! input wiring. Output is the 32-byte canonical little-endian result.

use alloc::vec;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;

const F: usize = 255; // field width in bits

/// Minimal boolar builder with cached constants (same pattern as the other
/// gadgets; the cache matters here because field ops emit thousands of
/// constant-free folds).
struct Builder {
    params: u32,
    stmts: Vec<Node<BIrStmt, ()>>,
    zero: Option<u32>,
    one: Option<u32>,
}

impl Builder {
    fn new(params: u32) -> Self {
        Self {
            params,
            stmts: Vec::new(),
            zero: None,
            one: None,
        }
    }
    fn gate(&mut self, s: BIrStmt) -> u32 {
        let id = self.params + self.stmts.len() as u32;
        self.stmts.push(Node::new(s, (), None));
        id
    }
    fn const0(&mut self) -> u32 {
        if let Some(z) = self.zero {
            return z;
        }
        let z = self.gate(BIrStmt::Zero);
        self.zero = Some(z);
        z
    }
    fn const1(&mut self) -> u32 {
        if let Some(o) = self.one {
            return o;
        }
        let o = self.gate(BIrStmt::One);
        self.one = Some(o);
        o
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
    fn mux(&mut self, c: u32, a: u32, b: u32) -> u32 {
        let x = self.xor(a, b);
        let y = self.and(c, x);
        self.xor(y, b)
    }

    /// `a + b` LSB-first, returning `max(len) + 1` bits (full carry kept).
    fn add(&mut self, a: &[u32], b: &[u32]) -> Vec<u32> {
        let n = a.len().max(b.len());
        let zero = self.const0();
        let get = |v: &[u32], i| if i < v.len() { v[i] } else { zero };
        let mut out = Vec::with_capacity(n + 1);
        let mut carry: Option<u32> = None;
        for i in 0..n {
            let (ai, bi) = (get(a, i), get(b, i));
            let axb = self.xor(ai, bi);
            let (sum, cout) = match carry {
                None => (axb, Some(self.and(ai, bi))),
                Some(c) => {
                    let s = self.xor(axb, c);
                    let ab = self.and(ai, bi);
                    let caxb = self.and(c, axb);
                    (s, Some(self.or(ab, caxb)))
                }
            };
            out.push(sum);
            carry = cout;
        }
        out.push(carry.unwrap_or(zero));
        out
    }

    /// `a - b` over `n = a.len()` bits as `(low n bits, ge)` where `ge`
    /// holds iff `a >= b` (carry-out of `a + !b + 1`).
    fn sub_cmp(&mut self, a: &[u32], b: &[u32]) -> (Vec<u32>, u32) {
        let n = a.len();
        let zero = self.const0();
        let one = self.const1();
        let get = |v: &[u32], i| if i < v.len() { v[i] } else { zero };
        let mut out = Vec::with_capacity(n);
        let mut carry = one;
        for i in 0..n {
            let nb = self.not(get(b, i));
            let axb = self.xor(a[i], nb);
            let s = self.xor(axb, carry);
            let ab = self.and(a[i], nb);
            let caxb = self.and(carry, axb);
            carry = self.or(ab, caxb);
            out.push(s);
        }
        (out, carry)
    }

    /// Constant value (up to 64 bits) as fresh-or-cached const wires.
    fn const_bits(&mut self, mut k: u64, width: usize) -> Vec<u32> {
        let mut out = Vec::with_capacity(width);
        for _ in 0..width {
            if k & 1 == 1 {
                let o = self.const1();
                out.push(o);
            } else {
                let z = self.const0();
                out.push(z);
            }
            k >>= 1;
        }
        out
    }

    /// `value << n` as wires (leading zeros are the shared const0 wire).
    fn shl(&mut self, v: &[u32], n: usize) -> Vec<u32> {
        let zero = self.const0();
        let mut out = vec![zero; n];
        out.extend_from_slice(v);
        out
    }

    /// Conditional swap of two 255-bit words on `c`.
    fn cswap(&mut self, c: u32, a: &mut Fe, b: &mut Fe) {
        for i in 0..F {
            let (ai, bi) = (a[i], b[i]);
            a[i] = self.mux(c, bi, ai);
            b[i] = self.mux(c, ai, bi);
        }
    }
}

/// Field element: 255-bit LSB-first wires, kept < 2^255 + small (not
/// necessarily canonical mod p).
type Fe = [u32; F];

/// Fold bits ≥ 255 of a wide value via `2^255 ≡ 19 (mod p)`, looping until
/// the value fits 258 bits, then return 255 bits (a final fold happens in
/// the callers' next operation).
fn fold_wide(b: &mut Builder, wide: &[u32]) -> Fe {
    // Each fold replaces `hi * 2^255` with `19 * hi`. The bit-length is not
    // a reliable progress measure (every adder appends a carry wire), but
    // the VALUE is: from <= 520 bits, after fold 1 the excess over 2^255 is
    // < 2^6, after fold 2 < 2 (a single bit), and folds 3-5 absorb it into
    // the low word. Six rounds is a proven margin, so the truncation is
    // sound: any dropped wire carries value 0.
    let mut cur: Vec<u32> = wide.to_vec();
    for _ in 0..6 {
        let lo: Vec<u32> = cur[..F.min(cur.len())].to_vec();
        if cur.len() <= F {
            break;
        }
        let hi: Vec<u32> = cur[F..].to_vec();
        // 19 * hi = hi + 2*hi + 16*hi
        let s1 = b.shl(&hi, 1);
        let s4 = b.shl(&hi, 4);
        let h1 = b.add(&hi, &s1);
        let h2 = b.add(&h1, &s4);
        cur = b.add(&lo, &h2);
    }
    let zero = b.const0();
    let mut out = [zero; F];
    out[..F].copy_from_slice(&cur[..F]);
    out
}

/// Field add.
fn fadd(b: &mut Builder, x: &Fe, y: &Fe) -> Fe {
    let t = b.add(x, y);
    fold_wide(b, &t)
}

/// Field sub: `x - y mod p` as `x + (2^256 - 38) - y` (`2p = 2^256 - 38`),
/// which stays non-negative as an integer and is then folded.
fn fsub(b: &mut Builder, x: &Fe, y: &Fe) -> Fe {
    // 2^256 - 38 = 0xFF..FFDA (32 bytes).
    let mut c2p = Vec::with_capacity(256);
    for i in 0..256 {
        let byte = if i / 8 == 0 { 0xdau8 } else { 0xffu8 };
        if (byte >> (i % 8)) & 1 == 1 {
            let o = b.const1();
            c2p.push(o);
        } else {
            let z = b.const0();
            c2p.push(z);
        }
    }
    let t1 = b.add(x, &c2p); // <= 257 bits, always >= y as an integer
    let mut yp = y.to_vec();
    yp.resize(t1.len(), b.const0());
    let (d, _ge) = b.sub_cmp(&t1, &yp);
    fold_wide(b, &d)
}

/// Field multiply: schoolbook AND-matrix, full-width accumulation (no
/// truncation — rows extend the accumulator), then fold.
fn fmul(b: &mut Builder, x: &Fe, y: &Fe) -> Fe {
    let zero = b.const0();
    let mut acc: Vec<u32> = vec![zero; F];
    for j in 0..F {
        // row = (x & y[j]) << j
        let mut row = vec![zero; j];
        for i in 0..F {
            let a = b.and(x[i], y[j]);
            row.push(a);
        }
        acc = b.add(&acc, &row);
    }
    fold_wide(b, &acc)
}

/// Field square: the symmetric AND-matrix halves (terms `i < j` fold into
/// one row shifted by `i + j + 1`; the diagonal is just `x[i] << 2i`).
fn fsquare(b: &mut Builder, x: &Fe) -> Fe {
    let zero = b.const0();
    // Diagonal: x[i]^2 = x[i] at bit 2i.
    let mut acc: Vec<u32> = Vec::with_capacity(F * 2);
    for i in 0..F {
        acc.push(x[i]);
        acc.push(zero);
    }
    // Off-diagonal pairs (x[i]&x[j]), i < j, counted twice -> bit i+j+1.
    for j in 0..F {
        let mut row = Vec::with_capacity(F * 2);
        for _ in 0..j + 1 {
            row.push(zero);
        }
        for i in 0..j {
            let a = b.and(x[i], x[j]);
            row.push(a);
        }
        if row.len() <= 1 {
            continue;
        }
        acc = b.add(&acc, &row);
    }
    fold_wide(b, &acc)
}

/// `z^(p-2)`: the standard short addition chain for `2^255 - 21`.
fn invert(b: &mut Builder, z: &Fe) -> Fe {
    let t0 = fsquare(b, z); // z^2
    let mut t1 = fsquare(b, &t0); // z^4
    t1 = fsquare(b, &t1); // z^8
    t1 = fmul(b, z, &t1); // z^9
    let t0 = fmul(b, &t0, &t1); // z^11
    let mut t2 = fsquare(b, &t0); // z^22
    t2 = fmul(b, &t1, &t2); // z^31 = z^(2^5-1)
    let mut t1 = t2;
    for _ in 0..5 {
        t1 = fsquare(b, &t1);
    }
    let t1 = fmul(b, &t1, &t2); // z^(2^10-1)
    let mut t2 = t1;
    for _ in 0..10 {
        t2 = fsquare(b, &t2);
    }
    let t2 = fmul(b, &t2, &t1); // z^(2^20-1)
    let mut t3 = t2;
    for _ in 0..20 {
        t3 = fsquare(b, &t3);
    }
    let t2 = fmul(b, &t3, &t2); // z^(2^40-1)
    let mut t3 = t2;
    for _ in 0..10 {
        t3 = fsquare(b, &t3);
    }
    let t1 = fmul(b, &t3, &t1); // z^(2^50-1)
    let mut t3 = t1;
    for _ in 0..50 {
        t3 = fsquare(b, &t3);
    }
    let t2 = fmul(b, &t3, &t1); // z^(2^100-1)
    let mut t3 = t2;
    for _ in 0..100 {
        t3 = fsquare(b, &t3);
    }
    let mut t2 = fmul(b, &t3, &t2); // z^(2^200-1)
    for _ in 0..50 {
        t2 = fsquare(b, &t2);
    }
    let t1 = fmul(b, &t2, &t1); // z^(2^250-1)
    let mut t1 = t1;
    for _ in 0..5 {
        t1 = fsquare(b, &t1);
    }
    // z^(2^255-32) * z^11 = z^(2^255-21) = z^(p-2)
    fmul(b, &t1, &t0)
}

/// Canonicalize: conditional subtraction of `p = 2^255 - 19`, twice (the
/// gadget's internal bound `< 2^255 + small` makes two rounds sufficient).
fn canonical(b: &mut Builder, x: &Fe) -> Fe {
    // p = 0x7FFF...FFED (32 bytes).
    let mut pvec = Vec::with_capacity(F);
    for i in 0..F {
        let byte = match i / 8 {
            0 => 0xedu8,
            31 => 0x7fu8,
            _ => 0xffu8,
        };
        if (byte >> (i % 8)) & 1 == 1 {
            let o = b.const1();
            pvec.push(o);
        } else {
            let z = b.const0();
            pvec.push(z);
        }
    }
    let mut cur: Vec<u32> = x.to_vec();
    for _ in 0..2 {
        let (diff, ge) = b.sub_cmp(&cur, &pvec);
        cur = (0..F).map(|i| b.mux(ge, diff[i], cur[i])).collect();
    }
    let mut out = [b.const0(); F];
    out.copy_from_slice(&cur[..F]);
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

/// One Montgomery ladder iteration as a circuit (the X25519 step
/// function). Params (1277 bits): `x2 ++ z2 ++ x3 ++ z3 ++ x1 ++ swap ++
/// kt` — four 255-bit ladder coordinates, the 255-bit base point, the
/// pending swap flag, and this iteration's scalar bit. Outputs (1021
/// bits): the updated `x2 ++ z2 ++ x3 ++ z3 ++ swap'`. The scalar bit
/// stays an *input* (garbled in MPC) so the circuit is key-agnostic and
/// reusable across all 255 iterations; the ladder init/final swap/inversion
/// are the caller's job (see the test driver).
pub fn build_x25519_step() -> BIrBlocks {
    let params: u32 = (4 * F + F + 2) as u32;
    let mut b = Builder::new(params);
    let zero = b.const0();
    let one = b.const1();
    let mut x2: Fe = [zero; F];
    let mut z2: Fe = [zero; F];
    let mut x3: Fe = [zero; F];
    let mut z3: Fe = [zero; F];
    let mut x1: Fe = [zero; F];
    for i in 0..F {
        x2[i] = i as u32;
        z2[i] = (F + i) as u32;
        x3[i] = (2 * F + i) as u32;
        z3[i] = (3 * F + i) as u32;
        x1[i] = (4 * F + i) as u32;
    }
    let swap = (5 * F) as u32;
    let kt = (5 * F + 1) as u32;

    let a24: Fe = {
        let mut v = [zero; F];
        for (i, vb) in v.iter_mut().enumerate().take(17) {
            // 121665 = 0x1DB41 (17 bits).
            *vb = if (121665u32 >> i) & 1 == 1 { one } else { zero };
        }
        v
    };

    let s_new = b.xor(swap, kt);
    b.cswap(s_new, &mut x2, &mut x3);
    b.cswap(s_new, &mut z2, &mut z3);

    let a = fadd(&mut b, &x2, &z2);
    let aa = fsquare(&mut b, &a);
    let bb = fsub(&mut b, &x2, &z2);
    let bbb = fsquare(&mut b, &bb);
    let e = fsub(&mut b, &aa, &bbb);
    let c = fadd(&mut b, &x3, &z3);
    let d = fsub(&mut b, &x3, &z3);
    let da = fmul(&mut b, &d, &a);
    let cb = fmul(&mut b, &c, &bb);
    let da_cb = fadd(&mut b, &da, &cb);
    let nx3 = fsquare(&mut b, &da_cb);
    let da_mb = fsub(&mut b, &da, &cb);
    let sq = fsquare(&mut b, &da_mb);
    let nz3 = fmul(&mut b, &x1, &sq);
    let nx2 = fmul(&mut b, &aa, &bbb);
    let a24e = fmul(&mut b, &a24, &e);
    let aa_a24e = fadd(&mut b, &aa, &a24e);
    let nz2 = fmul(&mut b, &e, &aa_a24e);

    let mut outputs = Vec::with_capacity(4 * F + 1);
    outputs.extend_from_slice(&nx2);
    outputs.extend_from_slice(&nz2);
    outputs.extend_from_slice(&nx3);
    outputs.extend_from_slice(&nz3);
    outputs.push(kt); // swap' = kt (per the RFC 7748 swap bookkeeping)
    finish(b, params, outputs)
}

/// The final ladder conditional swap as a circuit (the 2PC KX's last
/// ladder round). Params (1021 bits): `swap ++ x2 ++ z2 ++ x3 ++ z3`;
/// outputs (510 bits): the maybe-swapped `x2' ++ z2'` — the only
/// coordinates the finish chain consumes. The swap bit is a held secret
/// wire two-party, so this runs in-circuit rather than natively.
#[doc(hidden)]
pub fn build_final_cswap() -> BIrBlocks {
    let params: u32 = (1 + 4 * F) as u32;
    let mut b = Builder::new(params);
    let zero = b.const0();
    let swap = 0u32;
    let mut x2: Fe = [zero; F];
    let mut z2: Fe = [zero; F];
    let mut x3: Fe = [zero; F];
    let mut z3: Fe = [zero; F];
    for i in 0..F {
        x2[i] = (1 + i) as u32;
        z2[i] = (1 + F + i) as u32;
        x3[i] = (1 + 2 * F + i) as u32;
        z3[i] = (1 + 3 * F + i) as u32;
    }
    b.cswap(swap, &mut x2, &mut x3);
    b.cswap(swap, &mut z2, &mut z3);
    let mut outputs = Vec::with_capacity(2 * F);
    outputs.extend_from_slice(&x2);
    outputs.extend_from_slice(&z2);
    finish(b, params, outputs)
}

/// One field squaring (params: 255, output: 255 canonical) — the stepped
/// finish chain's square step.
#[doc(hidden)]
pub fn build_fe_square() -> BIrBlocks {
    let params = F as u32;
    let mut b = Builder::new(params);
    let zero = b.const0();
    let mut x = [zero; F];
    for (i, xb) in x.iter_mut().enumerate() {
        *xb = i as u32;
    }
    let r = fsquare(&mut b, &x);
    let r = canonical(&mut b, &r);
    finish(b, params, r.to_vec())
}

/// Test-only: one field multiplication (params: a 255 ++ b 255, output 255
/// bits folded but not canonicalized).
#[doc(hidden)]
pub fn build_fe_mul() -> BIrBlocks {
    let params = (F * 2) as u32;
    let mut b = Builder::new(params);
    let zero = b.const0();
    let mut x = [zero; F];
    let mut y = [zero; F];
    for i in 0..F {
        x[i] = i as u32;
        y[i] = (F + i) as u32;
    }
    let r = fmul(&mut b, &x, &y);
    let r = canonical(&mut b, &r);
    finish(b, params, r.to_vec())
}

/// Test-only: one field inversion (params: z 255, output 255 canonical).
#[doc(hidden)]
pub fn build_fe_invert() -> BIrBlocks {
    let params = F as u32;
    let mut b = Builder::new(params);
    let zero = b.const0();
    let mut z = [zero; F];
    for (i, zb) in z.iter_mut().enumerate() {
        *zb = i as u32;
    }
    let r = invert(&mut b, &z);
    let r = canonical(&mut b, &r);
    finish(b, params, r.to_vec())
}

/// Test-only: `a - b` (params: a 255 ++ b 255, output 255 canonical).
#[doc(hidden)]
pub fn build_fe_sub() -> BIrBlocks {
    let params = (F * 2) as u32;
    let mut b = Builder::new(params);
    let zero = b.const0();
    let mut x = [zero; F];
    let mut y = [zero; F];
    for i in 0..F {
        x[i] = i as u32;
        y[i] = (F + i) as u32;
    }
    let r = fsub(&mut b, &x, &y);
    let r = canonical(&mut b, &r);
    finish(b, params, r.to_vec())
}

// ------------------------------------------------------- scalar reference (test drivers)

/// Scalar GF(2^255-19) reference, independent of the circuit, exposed
/// doc-hidden for test drivers (the X25519 and TLS-composition tests).
#[doc(hidden)]
pub mod scalar_ref {
    use alloc::vec;
    use alloc::vec::Vec;
    /// 256-bit little-endian limb value, kept < 2^256 between ops.
    pub type Fp = [u64; 4];

    pub const P: Fp = [
        0xffffffffffffffed,
        0xffffffffffffffff,
        0xffffffffffffffff,
        0x7fffffffffffffff,
    ];

    pub fn fp_from_bytes(b: &[u8; 32]) -> Fp {
        let mut out = [0u64; 4];
        for i in 0..4 {
            out[i] = u64::from_le_bytes(b[i * 8..i * 8 + 8].try_into().unwrap());
        }
        // Mask bit 255 (the RFC 7748 u mask); the circuit does the same.
        out[3] &= 0x7fffffffffffffff;
        out
    }

    pub fn fp_to_bytes(a: &Fp) -> [u8; 32] {
        let a = canon(*a);
        let mut out = [0u8; 32];
        for i in 0..4 {
            out[i * 8..i * 8 + 8].copy_from_slice(&a[i].to_le_bytes());
        }
        out
    }

    pub fn ge(a: &Fp, b: &Fp) -> bool {
        for i in (0..4).rev() {
            if a[i] != b[i] {
                return a[i] > b[i];
            }
        }
        true
    }

    pub fn sub_raw(a: &Fp, b: &Fp) -> Fp {
        let mut out = [0u64; 4];
        let mut borrow = 0i128;
        for i in 0..4 {
            let t = a[i] as i128 - b[i] as i128 - borrow;
            if t < 0 {
                out[i] = (t + (1i128 << 64)) as u64;
                borrow = 1;
            } else {
                out[i] = t as u64;
                borrow = 0;
            }
        }
        out
    }

    pub fn canon(mut a: Fp) -> Fp {
        while ge(&a, &P) {
            a = sub_raw(&a, &P);
        }
        a
    }

    /// Reduce a wider little-endian limb vector into the field.
    pub fn reduce(mut v: Vec<u64>) -> Fp {
        while v.len() > 4 {
            let hi = v.split_off(4);
            let mut carry = 0u128;
            for i in 0..4 {
                let h = if i < hi.len() { hi[i] } else { 0 };
                // 2^256 = 38 (mod p)
                let t = v[i] as u128 + h as u128 * 38 + carry;
                v[i] = t as u64;
                carry = t >> 64;
            }
            if carry > 0 {
                v.push(carry as u64);
            }
        }
        let mut out = [0u64; 4];
        out.copy_from_slice(&v[..4]);
        // Fold bit 255 (2^255 = 19) until clear; the +19 ripple can re-set it,
        // so loop.
        while out[3] >> 63 == 1 {
            out[3] &= 0x7fffffffffffffff;
            let mut carry = 19u128;
            for limb in out.iter_mut() {
                let t = *limb as u128 + carry;
                *limb = t as u64;
                carry = t >> 64;
                if carry == 0 {
                    break;
                }
            }
        }
        canon(out)
    }

    pub fn fp_add(a: &Fp, b: &Fp) -> Fp {
        let mut v = vec![0u64; 5];
        let mut c = 0u128;
        for i in 0..4 {
            let t = a[i] as u128 + b[i] as u128 + c;
            v[i] = t as u64;
            c = t >> 64;
        }
        v[4] = c as u64;
        reduce(v)
    }

    pub fn fp_sub(a: &Fp, b: &Fp) -> Fp {
        // Canonicalize first so a, b < p; then a - b (or a + p - b when a < b)
        // fits 256 bits with no carry gymnastics.
        let a = &canon(*a);
        let b = &canon(*b);
        if ge(a, b) {
            reduce(sub_raw(a, b).to_vec())
        } else {
            // a + p - b; the raw add must NOT canonicalize (fp_add would
            // reduce mod p and destroy the +p guard). a + p < 2^256, so no
            // carry escapes.
            let mut ap = [0u64; 4];
            let mut c = 0u128;
            for i in 0..4 {
                let t = a[i] as u128 + P[i] as u128 + c;
                ap[i] = t as u64;
                c = t >> 64;
            }
            debug_assert_eq!(c, 0);
            reduce(sub_raw(&ap, b).to_vec())
        }
    }

    pub fn fp_mul(a: &Fp, b: &Fp) -> Fp {
        let mut v = [0u64; 9];
        for i in 0..4 {
            let mut c = 0u128;
            for j in 0..4 {
                let t = a[i] as u128 * b[j] as u128 + v[i + j] as u128 + c;
                v[i + j] = t as u64;
                c = t >> 64;
            }
            let mut k = i + 4;
            while c > 0 {
                let t = v[k] as u128 + c;
                v[k] = t as u64;
                c = t >> 64;
                k += 1;
            }
        }
        reduce(v.to_vec())
    }

    pub fn fp_square(a: &Fp) -> Fp {
        fp_mul(a, a)
    }

    pub fn fp_invert(z: &Fp) -> Fp {
        // p - 2 = 2^255 - 21; MSB-first square-and-multiply (scalar side is
        // fast; the circuit uses the short addition chain).
        let mut e = [0u64; 4];
        e.copy_from_slice(&P);
        e = sub_raw(&e, &[2, 0, 0, 0]);
        let mut r: Fp = [1, 0, 0, 0];
        for i in (0..255).rev() {
            r = fp_square(&r);
            if (e[i / 64] >> (i % 64)) & 1 == 1 {
                r = fp_mul(&r, z);
            }
        }
        r
    }

    pub fn x25519_scalar(k_bytes: &[u8; 32], u_bytes: &[u8; 32]) -> [u8; 32] {
        // Clamp the scalar.
        let mut k = *k_bytes;
        k[0] &= 248;
        k[31] &= 127;
        k[31] |= 64;
        let x1 = fp_from_bytes(u_bytes);
        let mut x2: Fp = [1, 0, 0, 0];
        let mut z2: Fp = [0; 4];
        let mut x3 = x1;
        let mut z3: Fp = [1, 0, 0, 0];
        let mut swap = 0u64;
        let a24: Fp = [121665, 0, 0, 0];
        for t in (0..255).rev() {
            let kt = (k[t / 8] >> (t % 8)) & 1;
            swap ^= kt as u64;
            if swap == 1 {
                core::mem::swap(&mut x2, &mut x3);
                core::mem::swap(&mut z2, &mut z3);
            }
            swap = kt as u64;
            let a = fp_add(&x2, &z2);
            let aa = fp_square(&a);
            let bb = fp_sub(&x2, &z2);
            let bbb = fp_square(&bb);
            let e = fp_sub(&aa, &bbb);
            let c = fp_add(&x3, &z3);
            let d = fp_sub(&x3, &z3);
            let da = fp_mul(&d, &a);
            let cb = fp_mul(&c, &bb);
            x3 = fp_square(&fp_add(&da, &cb));
            z3 = fp_mul(&x1, &fp_square(&fp_sub(&da, &cb)));
            x2 = fp_mul(&aa, &bbb);
            z2 = fp_mul(&e, &fp_add(&aa, &fp_mul(&a24, &e)));
        }
        if swap == 1 {
            core::mem::swap(&mut x2, &mut x3);
            core::mem::swap(&mut z2, &mut z3);
        }
        fp_to_bytes(&fp_mul(&x2, &fp_invert(&z2)))
    }

    // --- bit helpers ----------------------------------------------------------
}
