//! P4c-ii: the X25519 gadget. Fast field-op tests (mul / sub / invert)
//! cross-check against an independent scalar GF(2^255-19) reference; the
//! Montgomery ladder is checked against the RFC 7748 vectors via the
//! STEPPED form (one iteration per circuit, 255 evals with threaded state —
//! the shape the two-party run will use) in an `#[ignore]`d heavyweight
//! test; a monolithic single-circuit ladder would be ~30GB of gates.

use volar_vc::x25519_gadget::{
    build_fe_invert, build_fe_mul, build_fe_square, build_fe_sub, build_x25519_step,
};

// --- scalar GF(2^255-19) reference (independent of the circuit) ----------

/// 256-bit little-endian limb value, kept < 2^256 between ops.
type Fp = [u64; 4];

const P: Fp = [
    0xffffffffffffffed,
    0xffffffffffffffff,
    0xffffffffffffffff,
    0x7fffffffffffffff,
];

fn fp_from_bytes(b: &[u8; 32]) -> Fp {
    let mut out = [0u64; 4];
    for i in 0..4 {
        out[i] = u64::from_le_bytes(b[i * 8..i * 8 + 8].try_into().unwrap());
    }
    // Mask bit 255 (the RFC 7748 u mask); the circuit does the same.
    out[3] &= 0x7fffffffffffffff;
    out
}

fn fp_to_bytes(a: &Fp) -> [u8; 32] {
    let a = canon(*a);
    let mut out = [0u8; 32];
    for i in 0..4 {
        out[i * 8..i * 8 + 8].copy_from_slice(&a[i].to_le_bytes());
    }
    out
}

fn ge(a: &Fp, b: &Fp) -> bool {
    for i in (0..4).rev() {
        if a[i] != b[i] {
            return a[i] > b[i];
        }
    }
    true
}

fn sub_raw(a: &Fp, b: &Fp) -> Fp {
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

fn canon(mut a: Fp) -> Fp {
    while ge(&a, &P) {
        a = sub_raw(&a, &P);
    }
    a
}

/// Reduce a wider little-endian limb vector into the field.
fn reduce(mut v: Vec<u64>) -> Fp {
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

fn fp_add(a: &Fp, b: &Fp) -> Fp {
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

fn fp_sub(a: &Fp, b: &Fp) -> Fp {
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

fn fp_mul(a: &Fp, b: &Fp) -> Fp {
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

fn fp_square(a: &Fp) -> Fp {
    fp_mul(a, a)
}

fn fp_invert(z: &Fp) -> Fp {
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

fn x25519_scalar(k_bytes: &[u8; 32], u_bytes: &[u8; 32]) -> [u8; 32] {
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

fn bits_of(bytes: &[u8]) -> Vec<bool> {
    bytes.iter().flat_map(|b| (0..8).map(move |j| (b >> j) & 1 == 1)).collect()
}

fn bytes_of(bits: &[bool]) -> Vec<u8> {
    bits.chunks(8)
        .map(|c| c.iter().enumerate().fold(0u8, |a, (j, &b)| a|((b as u8) << j)))
        .collect()
}

fn eval_bits(circuit: &volar_ir::boolar::BIrBlocks, inputs: &[bool]) -> Vec<bool> {
    volar_fuzz::interpreter::biir::eval_biir(circuit, inputs).expect("concrete eval")
}

fn eval(circuit: &volar_ir::boolar::BIrBlocks, inputs: &[bool]) -> Vec<u8> {
    bytes_of(&eval_bits(circuit, inputs))
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len() / 2)
        .map(|i| u8::from_str_radix(&s[2 * i..2 * i + 2], 16).unwrap())
        .collect()
}

fn fe_inputs(a: &Fp, b: &Fp) -> Vec<bool> {
    // The test circuits take 255 bits per side (bit 255 is the RFC mask
    // position, always 0 after canonicalization); truncate BEFORE joining.
    let mut v = bits_of(&fp_to_bytes(a));
    v.truncate(255);
    let mut vb = bits_of(&fp_to_bytes(b));
    vb.truncate(255);
    v.extend(vb);
    v
}

#[test]
fn fe_mul_matches_scalar() {
    let c = build_fe_mul();
    let cases: [(Fp, Fp); 4] = [
        ([0; 4], [1, 0, 0, 0]),
        ([1, 0, 0, 0], [1, 0, 0, 0]),
        (P, [2, 0, 0, 0]), // p ≡ 0
        (
            [0x0123456789abcdef, 0xfedcba9876543210, 0x0badf00d12345678, 0x5a5a5a5a5a5a5a5a],
            [0xdeadbeefcafebabe, 0x123456789abcdef0, 0x0f0f0f0f0f0f0f0f, 0x3333333333333333],
        ),
    ];
    for (a, b) in cases {
        let out = eval(&c, &fe_inputs(&a, &b));
        let expect = fp_to_bytes(&fp_mul(&a, &b));
        assert_eq!(out, expect, "fe_mul({a:x?}, {b:x?})");
    }
}

#[test]
fn fe_sub_matches_scalar() {
    let c = build_fe_sub();
    let cases: [(Fp, Fp); 4] = [
        ([5, 0, 0, 0], [3, 0, 0, 0]),
        ([3, 0, 0, 0], [5, 0, 0, 0]), // wraps mod p
        ([0; 4], [0; 4]),
        ([0; 4], P), // 0 - p ≡ 0... input p is itself ≡ 0
    ];
    for (a, b) in cases {
        let out = eval(&c, &fe_inputs(&a, &b));
        let expect = fp_to_bytes(&fp_sub(&a, &b));
        assert_eq!(out, expect, "fe_sub({a:x?}, {b:x?})");
    }
}

/// Inversion is ~60M gates — heavyweight.
#[test]
#[ignore = "heavyweight: ~60M-gate inversion circuit"]
fn fe_invert_matches_scalar() {
    let c = build_fe_invert();
    let z: Fp = [0x0123456789abcdef, 0xfedcba9876543210, 0x0badf00d12345678, 0x5a5a5a5a5a5a5a5a];
    let mut inputs = bits_of(&fp_to_bytes(&z));
    inputs.truncate(255);
    let out = eval(&c, &inputs);
    assert_eq!(out, fp_to_bytes(&fp_invert(&z)).to_vec(), "fe_invert");
    // And the defining property: z * z^-1 = 1.
    let m = build_fe_mul();
    let zinv = fp_invert(&z);
    let out2 = eval(&m, &fe_inputs(&z, &zinv));
    assert_eq!(out2, {
        let mut e = [0u8; 32];
        e[0] = 1;
        e.to_vec()
    });
}

/// Drive `steps` ladder iterations through the step circuit, threading the
/// decoded state (concrete validation of the shape the two-party step loop
/// will run with label threading). Returns (x2, z2, x3, z3, swap).
fn ladder_steps(
    step: &volar_ir::boolar::BIrBlocks,
    u: &Fp,
    k: &[u8; 32],
    steps: usize,
) -> (Fp, Fp, Fp, Fp, u64) {
    let mut x2: Fp = [1, 0, 0, 0];
    let mut z2: Fp = [0; 4];
    let mut x3: Fp = *u;
    let mut z3: Fp = [1, 0, 0, 0];
    let mut swap = 0u64;
    let mut k = *k;
    k[0] &= 248;
    k[31] &= 127;
    k[31] |= 64; // RFC 7748 clamping (the circuit keeps its input raw)
    for i in 0..steps {
        let t = 254 - i; // iteration index (254 down to 0)
        let kt = (k[t / 8] >> (t % 8)) & 1;
        let mut inputs = Vec::with_capacity(1277);
        for w in [&x2, &z2, &x3, &z3, u] {
            let mut bits = bits_of(&fp_to_bytes(w));
            bits.truncate(255);
            inputs.extend(bits);
        }
        inputs.push(swap == 1);
        inputs.push(kt == 1);
        let out = eval_bits(step, &inputs);
        assert_eq!(out.len(), 4 * 255 + 1);
        let mut take = |off: usize| -> Fp {
            let mut bytes = [0u8; 32];
            for m in 0..32 {
                let mut byte = 0u8;
                for j in 0..8 {
                    let local = 8 * m + j;
                    if local < 255 && out[off + local] {
                        byte |= 1 << j;
                    }
                }
                bytes[m] = byte;
            }
            bytes[31] &= 0x7f;
            let mut v = [0u64; 4];
            for i in 0..4 {
                v[i] = u64::from_le_bytes(bytes[i * 8..i * 8 + 8].try_into().unwrap());
            }
            v
        };
        x2 = take(0);
        z2 = take(255);
        x3 = take(510);
        z3 = take(765);
        swap = out[1020] as u64;
    }
    (x2, z2, x3, z3, swap)
}

/// Scalar ladder mid-state for cross-checking (non-canonical-tolerant:
/// compared mod p).
fn scalar_ladder_steps(u: &Fp, k: &[u8; 32], steps: usize) -> (Fp, Fp, Fp, Fp, u64) {
    let mut kc = *k;
    kc[0] &= 248;
    kc[31] &= 127;
    kc[31] |= 64;
    let mut x2: Fp = [1, 0, 0, 0];
    let mut z2: Fp = [0; 4];
    let mut x3: Fp = *u;
    let mut z3: Fp = [1, 0, 0, 0];
    let mut swap = 0u64;
    let a24: Fp = [121665, 0, 0, 0];
    for i in 0..steps {
        let t = 254 - i;
        let kt = (kc[t / 8] >> (t % 8)) & 1;
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
        z3 = fp_mul(u, &fp_square(&fp_sub(&da, &cb)));
        x2 = fp_mul(&aa, &bbb);
        z2 = fp_mul(&e, &fp_add(&aa, &fp_mul(&a24, &e)));
    }
    (x2, z2, x3, z3, swap)
}

/// The stepped ladder agrees with the scalar reference after the first few
/// iterations (fast default-suite coverage of the step circuit).
#[test]
fn x25519_step_matches_scalar_midstate() {
    let step = build_x25519_step();
    let mut u_bytes = [0u8; 32];
    u_bytes[0] = 9;
    let u = fp_from_bytes(&u_bytes);
    let mut k = [0u8; 32];
    k[0] = 0x35;
    k[1] = 0xc3;
    let (cx2, cz2, cx3, cz3, cswap) = ladder_steps(&step, &u, &k, 3);
    let (sx2, sz2, sx3, sz3, sswap) = scalar_ladder_steps(&u, &k, 3);
    assert_eq!(cswap, sswap, "swap");
    for (c, s, name) in [(cx2, sx2, "x2"), (cz2, sz2, "z2"), (cx3, sx3, "x3"), (cz3, sz3, "z3")] {
        assert_eq!(
            canon(c),
            canon(s),
            "{name} mismatch after 3 steps"
        );
    }
}

/// The full stepped X25519: 255 ladder iterations + the stepped inversion
/// chain + final multiply, against the RFC 7748 vectors.
#[test]
#[ignore = "heavyweight: 255 step-circuit evals plus a 265-op stepped inversion"]
fn x25519_stepped_rfc7748_vectors() {
    let step = build_x25519_step();
    let sq = build_fe_square();
    let mul = build_fe_mul();

    let run = |k: &[u8; 32], u_bytes: &[u8; 32]| -> [u8; 32] {
        let u = fp_from_bytes(u_bytes);
        let (mut x2, mut z2, mut x3, mut z3, swap) = ladder_steps(&step, &u, k, 255);
        // Final cswap.
        if swap == 1 {
            core::mem::swap(&mut x2, &mut x3);
            core::mem::swap(&mut z2, &mut z3);
        }
        // Stepped inversion chain for z2^(p-2) (the circuit's `invert`
        // structure), driving build_fe_square / build_fe_mul.
        let fev = |c: &volar_ir::boolar::BIrBlocks, args: &[&Fp]| -> Fp {
            let mut inputs = Vec::new();
            for a in args {
                let mut bits = bits_of(&fp_to_bytes(a));
                bits.truncate(255);
                inputs.extend(bits);
            }
            let out = eval_bits(c, &inputs);
            let mut bytes = [0u8; 32];
            for m in 0..32 {
                let mut byte = 0u8;
                for j in 0..8 {
                    if 8 * m + j < 255 && out[8 * m + j] {
                        byte |= 1 << j;
                    }
                }
                bytes[m] = byte;
            }
            bytes[31] &= 0x7f;
            let mut v = [0u64; 4];
            for i in 0..4 {
                v[i] = u64::from_le_bytes(bytes[i * 8..i * 8 + 8].try_into().unwrap());
            }
            v
        };
        let square = |a: &Fp| fev(&sq, &[a]);
        let mult = |a: &Fp, b: &Fp| fev(&mul, &[a, b]);
        let t0 = square(&z2);
        let mut t1 = square(&t0);
        t1 = square(&t1);
        t1 = mult(&z2, &t1);
        let t0 = mult(&t0, &t1);
        let mut t2 = square(&t0);
        t2 = mult(&t1, &t2);
        let mut t1 = t2;
        for _ in 0..5 {
            t1 = square(&t1);
        }
        let t1 = mult(&t1, &t2);
        let mut t2 = t1;
        for _ in 0..10 {
            t2 = square(&t2);
        }
        let t2 = mult(&t2, &t1);
        let mut t3 = t2;
        for _ in 0..20 {
            t3 = square(&t3);
        }
        let t2 = mult(&t3, &t2);
        let mut t3 = t2;
        for _ in 0..10 {
            t3 = square(&t3);
        }
        let t1 = mult(&t3, &t1);
        let mut t3 = t1;
        for _ in 0..50 {
            t3 = square(&t3);
        }
        let t2 = mult(&t3, &t1);
        let mut t3 = t2;
        for _ in 0..100 {
            t3 = square(&t3);
        }
        let mut t2 = mult(&t3, &t2);
        for _ in 0..50 {
            t2 = square(&t2);
        }
        let t1 = mult(&t2, &t1);
        let mut t1 = t1;
        for _ in 0..5 {
            t1 = square(&t1);
        }
        let zinv = mult(&t1, &t0);
        let res = mult(&x2, &zinv);
        fp_to_bytes(&res)
    };

    // RFC 7748 §5.2 vector 1.
    let k1: [u8; 32] = hex("a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4")
        .try_into()
        .unwrap();
    let u1: [u8; 32] = hex("e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c")
        .try_into()
        .unwrap();
    assert_eq!(
        run(&k1, &u1).to_vec(),
        hex("c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552"),
        "RFC 7748 vector 1"
    );
    // Base point (scalar 9, u 9).
    let mut k9 = [0u8; 32];
    k9[0] = 9;
    assert_eq!(
        run(&k9, &k9).to_vec(),
        hex("422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079"),
        "RFC 7748 base point"
    );
}

/// The scalar reference itself against the RFC vectors (guards against a
/// shared-circuit/reference bug).
#[test]
fn scalar_reference_matches_rfc7748() {
    let k: [u8; 32] = hex("a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4")
        .try_into()
        .unwrap();
    let u: [u8; 32] = hex("e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c")
        .try_into()
        .unwrap();
    assert_eq!(
        x25519_scalar(&k, &u).to_vec(),
        hex("c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552"),
        "scalar ref vector 1"
    );
    let k9 = {
        let mut k = [0u8; 32];
        k[0] = 9;
        k
    };
    assert_eq!(
        x25519_scalar(&k9, &k9).to_vec(),
        hex("422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079"),
        "scalar ref base point"
    );
}
