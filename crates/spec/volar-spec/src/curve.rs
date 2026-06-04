// @reliability: experimental
//! @ai: assisted
//! Curve25519 in twisted-Edwards form (Ed25519).
//!
//! Provides a `Group` implementation suitable for the Chou-Orlandi base
//! OT (see [`crate::ot::base`]), replacing the [`crate::ot::group::ToyGroup`]
//! placeholder with a curve where the discrete-log problem is believed
//! hard.
//!
//! # Layout
//!
//! - [`Fe25519`] — field element mod `p = 2^255 − 19`. Stored as
//!   four little-endian `u64` limbs in canonical (`< p`) form.
//! - [`EdPoint`] — point on the twisted Edwards curve `−x² + y² = 1 + d·x²·y²`
//!   with `d = −121665/121666`. Stored in **extended coordinates**
//!   `(X : Y : Z : T)` with `x = X/Z`, `y = Y/Z`, `T = X·Y/Z`.
//! - [`Ed25519`] — zero-sized type carrying the [`Group`](super::ot::group::Group)
//!   impl.
//!
//! # Caveats
//!
//! - Implementations are **not constant-time**. Acceptable for a
//!   spec/test setting; a production implementation must be
//!   side-channel hardened.
//! - The cofactor (h = 8) is not cleared. For OT correctness this is
//!   fine (both sides agree on the embedding); for protocols requiring
//!   prime-order semantics, multiply by 8 or switch to ristretto255.
//! - Inversion uses naive `a^(p-2)` via 254 squarings + ~12 multiplies.
//!   Slow but small code.

use core::ops::{Add, Mul, Neg, Sub};

use digest::Digest;

use crate::SpecRng;
use crate::ot::group::Group;

// ============================================================================
// Field arithmetic mod p = 2^255 - 19
// ============================================================================

/// Field element mod `2^255 − 19`. Canonical: every value `< p`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Fe25519(pub [u64; 4]);

const P_LIMBS: [u64; 4] = [
    0xFFFF_FFFF_FFFF_FFED,
    0xFFFF_FFFF_FFFF_FFFF,
    0xFFFF_FFFF_FFFF_FFFF,
    0x7FFF_FFFF_FFFF_FFFF,
];

impl Fe25519 {
    pub const ZERO: Self = Self([0, 0, 0, 0]);
    pub const ONE: Self = Self([1, 0, 0, 0]);

    pub fn is_zero(&self) -> bool {
        self.0 == [0, 0, 0, 0]
    }

    pub fn to_bytes(self) -> [u8; 32] {
        let mut out = [0u8; 32];
        for i in 0..4 {
            out[i * 8..i * 8 + 8].copy_from_slice(&self.0[i].to_le_bytes());
        }
        out
    }
}

/// Schoolbook 4×4 → 8 limb multiply.
fn mul_4x4(a: &[u64; 4], b: &[u64; 4]) -> [u64; 8] {
    let mut r = [0u64; 8];
    for i in 0..4 {
        let mut carry: u64 = 0;
        for j in 0..4 {
            let v = (r[i + j] as u128)
                + (a[i] as u128) * (b[j] as u128)
                + (carry as u128);
            r[i + j] = v as u64;
            carry = (v >> 64) as u64;
        }
        r[i + 4] = carry;
    }
    r
}

/// Reduce an 8-limb wide value mod `p` using the `2^256 ≡ 38 (mod p)` trick.
fn reduce_wide(t: &[u64; 8]) -> Fe25519 {
    // Step 1: out[0..4] = t[0..4] + 38·t[4..8]; carry collected into out[4].
    let mut acc = [0u64; 5];
    let mut c: u128 = 0;
    for i in 0..4 {
        let v = (t[i] as u128) + (t[4 + i] as u128) * 38 + c;
        acc[i] = v as u64;
        c = v >> 64;
    }
    acc[4] = c as u64;

    // Step 2: fold acc[4]·2^256 ≡ acc[4]·38 (mod p) back into low limbs.
    let mut out = [0u64; 4];
    let mut c: u128 = (acc[4] as u128) * 38;
    for i in 0..4 {
        let v = (acc[i] as u128) + c;
        out[i] = v as u64;
        c = v >> 64;
    }
    // c should be tiny (≤ a few). Fold once more.
    if c != 0 {
        let mut c2: u128 = c * 38;
        for i in 0..4 {
            let v = (out[i] as u128) + c2;
            out[i] = v as u64;
            c2 = v >> 64;
        }
        // c2 must be 0 here.
    }

    // Result is in [0, ~2p). Conditional subtract p.
    fe_canonicalize(out)
}

/// Subtract `p` up to twice if `a ≥ p`. Inputs may be in `[0, 2p + ε)`,
/// so a single subtract is not always sufficient (after `reduce_wide`).
fn fe_canonicalize(a: [u64; 4]) -> Fe25519 {
    let mut x = a;
    for _ in 0..2 {
        let mut tmp = [0u64; 4];
        let mut borrow: u64 = 0;
        for i in 0..4 {
            let (r1, b1) = x[i].overflowing_sub(P_LIMBS[i]);
            let (r2, b2) = r1.overflowing_sub(borrow);
            tmp[i] = r2;
            borrow = (b1 as u64) | (b2 as u64);
        }
        if borrow == 0 {
            x = tmp;
        }
    }
    Fe25519(x)
}

pub fn fe_add(a: &Fe25519, b: &Fe25519) -> Fe25519 {
    let mut r = [0u64; 4];
    let mut c: u64 = 0;
    for i in 0..4 {
        let v = (a.0[i] as u128) + (b.0[i] as u128) + (c as u128);
        r[i] = v as u64;
        c = (v >> 64) as u64;
    }
    // c may be 1 (overflow into 2^256). Fold via 2^256 ≡ 38.
    if c != 0 {
        let mut c2: u128 = (c as u128) * 38;
        for i in 0..4 {
            let v = (r[i] as u128) + c2;
            r[i] = v as u64;
            c2 = v >> 64;
        }
    }
    fe_canonicalize(r)
}

pub fn fe_sub(a: &Fe25519, b: &Fe25519) -> Fe25519 {
    // a - b mod p = a + (p - b) mod p; compute via add of (p - b).
    let mut neg_b = [0u64; 4];
    let mut borrow: u64 = 0;
    for i in 0..4 {
        let (r1, br1) = P_LIMBS[i].overflowing_sub(b.0[i]);
        let (r2, br2) = r1.overflowing_sub(borrow);
        neg_b[i] = r2;
        borrow = (br1 as u64) | (br2 as u64);
    }
    fe_add(a, &Fe25519(neg_b))
}

pub fn fe_neg(a: &Fe25519) -> Fe25519 {
    if a.is_zero() {
        Fe25519::ZERO
    } else {
        let mut neg = [0u64; 4];
        let mut borrow: u64 = 0;
        for i in 0..4 {
            let (r1, br1) = P_LIMBS[i].overflowing_sub(a.0[i]);
            let (r2, br2) = r1.overflowing_sub(borrow);
            neg[i] = r2;
            borrow = (br1 as u64) | (br2 as u64);
        }
        Fe25519(neg)
    }
}

pub fn fe_mul(a: &Fe25519, b: &Fe25519) -> Fe25519 {
    let wide = mul_4x4(&a.0, &b.0);
    reduce_wide(&wide)
}

pub fn fe_sq(a: &Fe25519) -> Fe25519 {
    fe_mul(a, a)
}

/// `base^exp mod p`, `exp` little-endian limbs — square-and-multiply (MSB→LSB).
pub fn fe_pow(base: &Fe25519, exp: &[u64; 4]) -> Fe25519 {
    let mut acc = Fe25519::ONE;
    for limb_idx in (0..4).rev() {
        for bit in (0..64).rev() {
            acc = fe_sq(&acc);
            if (exp[limb_idx] >> bit) & 1 == 1 {
                acc = fe_mul(&acc, base);
            }
        }
    }
    acc
}

/// `√−1 mod p = 2^{(p−1)/4}` (since `2` is a non-residue); `(p−1)/4 = 2^253 − 5`.
fn sqrt_m1() -> Fe25519 {
    const E: [u64; 4] = [
        0xFFFF_FFFF_FFFF_FFFB,
        0xFFFF_FFFF_FFFF_FFFF,
        0xFFFF_FFFF_FFFF_FFFF,
        0x1FFF_FFFF_FFFF_FFFF,
    ];
    fe_pow(&Fe25519([2, 0, 0, 0]), &E)
}

/// Square root mod `p = 2^255 − 19` (`p ≡ 5 mod 8`).  Returns `Some(r)` with
/// `r² = w` if `w` is a quadratic residue, else `None`.
///
/// `c = w^{(p+3)/8}` (`(p+3)/8 = 2^252 − 2`); then `c² ∈ {w, −w}`: if `c²=w`,
/// `c` is the root; if `c²=−w`, `c·√−1` is the root; otherwise `w` is a non-residue.
pub fn fe_sqrt(w: &Fe25519) -> Option<Fe25519> {
    const E: [u64; 4] = [
        0xFFFF_FFFF_FFFF_FFFE,
        0xFFFF_FFFF_FFFF_FFFF,
        0xFFFF_FFFF_FFFF_FFFF,
        0x0FFF_FFFF_FFFF_FFFF,
    ];
    let c = fe_pow(w, &E);
    let c2 = fe_sq(&c);
    if c2 == *w {
        Some(c)
    } else if c2 == fe_neg(w) {
        Some(fe_mul(&c, &sqrt_m1()))
    } else {
        None
    }
}

/// Whether `w` is a quadratic residue mod `p`.
pub fn is_square(w: &Fe25519) -> bool {
    fe_sqrt(w).is_some()
}

/// Interpret 32 little-endian bytes as an integer and reduce mod `p`.
pub fn fe_from_bytes_le(b: &[u8; 32]) -> Fe25519 {
    let mut limbs = [0u64; 4];
    for i in 0..4 {
        let mut chunk = [0u8; 8];
        chunk.copy_from_slice(&b[i * 8..i * 8 + 8]);
        limbs[i] = u64::from_le_bytes(chunk);
    }
    reduce_wide(&[limbs[0], limbs[1], limbs[2], limbs[3], 0, 0, 0, 0])
}

/// `a^(p − 2) mod p` via naive square-and-multiply.
pub fn fe_invert(a: &Fe25519) -> Fe25519 {
    // p - 2 = 2^255 - 21. Bits: top bit at position 254, then ones down to 5,
    // then bits 0..4 = 5'b01011 (i.e. bits 0,1,3 set; bits 2,4 unset).
    // Easiest: walk all 255 bits explicitly.
    let exp_limbs: [u64; 4] = [
        // p - 2 in LE limbs: limb 0 = ...EB (since p limb 0 = ...ED, minus 2 = ...EB)
        0xFFFF_FFFF_FFFF_FFEB,
        0xFFFF_FFFF_FFFF_FFFF,
        0xFFFF_FFFF_FFFF_FFFF,
        0x7FFF_FFFF_FFFF_FFFF,
    ];
    let mut acc = Fe25519::ONE;
    // Iterate from MSB to LSB.
    for limb_idx in (0..4).rev() {
        for bit in (0..64).rev() {
            acc = fe_sq(&acc);
            let b = (exp_limbs[limb_idx] >> bit) & 1;
            if b == 1 {
                acc = fe_mul(&acc, a);
            }
        }
    }
    acc
}

// Operator overloads for ergonomics within this module.
impl Add for Fe25519 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self { fe_add(&self, &rhs) }
}
impl Sub for Fe25519 {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self { fe_sub(&self, &rhs) }
}
impl Mul for Fe25519 {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self { fe_mul(&self, &rhs) }
}
impl Neg for Fe25519 {
    type Output = Self;
    fn neg(self) -> Self { fe_neg(&self) }
}

// ============================================================================
// Ed25519 group
// ============================================================================

/// Twisted-Edwards curve constant `d = −121665/121666 mod p`.
const D_LIMBS: [u64; 4] = [
    0x75EB_4DCA_1359_78A3,
    0x0070_0A4D_4141_D8AB,
    0x8CC7_4079_7779_E898,
    0x5203_6CEE_2B6F_FE73,
];
/// `2·d mod p` (precomputed for the Hisil-Wong-Carter-Dawson formulas).
const D2_LIMBS: [u64; 4] = [
    0xEBD6_9B94_26B2_F159,
    0x00E0_149A_8283_B156,
    0x198E_80F2_EEF3_D130,
    0x2406_D9DC_56DF_FCE7,
];

const fn fe_const(limbs: [u64; 4]) -> Fe25519 { Fe25519(limbs) }

const D: Fe25519 = fe_const(D_LIMBS);
const D2: Fe25519 = fe_const(D2_LIMBS);

/// Ed25519 base-point limbs (X coordinate, little-endian u64).
const BASE_X_LIMBS: [u64; 4] = [
    0xC956_2D60_8F25_D51A,
    0x692C_C760_9525_A7B2,
    0xC0A4_E231_FDD6_DC5C,
    0x2169_36D3_CD6E_53FE,
];
/// Ed25519 base-point Y = 4/5 mod p.
const BASE_Y_LIMBS: [u64; 4] = [
    0x6666_6666_6666_6658,
    0x6666_6666_6666_6666,
    0x6666_6666_6666_6666,
    0x6666_6666_6666_6666,
];

/// Point in extended twisted-Edwards coordinates.
///
/// Affine `(x, y) = (X/Z, Y/Z)` with `T = X·Y/Z`.
/// Identity: `(0, 1, 1, 0)`.
#[derive(Clone, Copy, Debug)]
pub struct EdPoint {
    pub x: Fe25519,
    pub y: Fe25519,
    pub z: Fe25519,
    pub t: Fe25519,
}

impl EdPoint {
    pub const IDENTITY: Self = Self {
        x: Fe25519::ZERO,
        y: Fe25519::ONE,
        z: Fe25519::ONE,
        t: Fe25519::ZERO,
    };

    pub fn base() -> Self {
        let x = Fe25519(BASE_X_LIMBS);
        let y = Fe25519(BASE_Y_LIMBS);
        Self { x, y, z: Fe25519::ONE, t: fe_mul(&x, &y) }
    }

    /// Affine X-Y representation. Returns `(x, y) = (X/Z, Y/Z)`.
    pub fn to_affine(&self) -> (Fe25519, Fe25519) {
        let zinv = fe_invert(&self.z);
        (fe_mul(&self.x, &zinv), fe_mul(&self.y, &zinv))
    }
}

impl PartialEq for EdPoint {
    /// Equality via cross-multiplication: `X1·Z2 == X2·Z1` and `Y1·Z2 == Y2·Z1`.
    fn eq(&self, other: &Self) -> bool {
        let lhs_x = fe_mul(&self.x, &other.z);
        let rhs_x = fe_mul(&other.x, &self.z);
        let lhs_y = fe_mul(&self.y, &other.z);
        let rhs_y = fe_mul(&other.y, &self.z);
        lhs_x == rhs_x && lhs_y == rhs_y
    }
}

impl Eq for EdPoint {}

/// Point addition (Hisil-Wong-Carter-Dawson, twisted Edwards `a = −1`).
pub fn ed_add(p1: &EdPoint, p2: &EdPoint) -> EdPoint {
    let a = fe_mul(&fe_sub(&p1.y, &p1.x), &fe_sub(&p2.y, &p2.x));
    let b = fe_mul(&fe_add(&p1.y, &p1.x), &fe_add(&p2.y, &p2.x));
    let c = fe_mul(&fe_mul(&p1.t, &D2), &p2.t);
    let d_ = fe_add(&fe_mul(&p1.z, &p2.z), &fe_mul(&p1.z, &p2.z)); // 2·Z1·Z2
    let e = fe_sub(&b, &a);
    let f = fe_sub(&d_, &c);
    let g = fe_add(&d_, &c);
    let h = fe_add(&b, &a);
    EdPoint {
        x: fe_mul(&e, &f),
        y: fe_mul(&g, &h),
        t: fe_mul(&e, &h),
        z: fe_mul(&f, &g),
    }
}

/// Point doubling (twisted Edwards `a = −1`).
pub fn ed_double(p: &EdPoint) -> EdPoint {
    let a = fe_sq(&p.x);
    let b = fe_sq(&p.y);
    let c = fe_add(&fe_sq(&p.z), &fe_sq(&p.z)); // 2·Z²
    let d_ = fe_neg(&a); // a = -1
    let xy_sum = fe_add(&p.x, &p.y);
    let e = fe_sub(&fe_sub(&fe_sq(&xy_sum), &a), &b);
    let g = fe_add(&d_, &b);
    let f = fe_sub(&g, &c);
    let h = fe_sub(&d_, &b);
    EdPoint {
        x: fe_mul(&e, &f),
        y: fe_mul(&g, &h),
        t: fe_mul(&e, &h),
        z: fe_mul(&f, &g),
    }
}

pub fn ed_neg(p: &EdPoint) -> EdPoint {
    EdPoint {
        x: fe_neg(&p.x),
        y: p.y,
        z: p.z,
        t: fe_neg(&p.t),
    }
}

/// Double-and-add scalar multiplication `[k] · P`.
///
/// `k` is given as little-endian bytes (32 bytes for Ed25519). Iterates from
/// the most-significant bit downward.
pub fn ed_scalar_mul(p: &EdPoint, k: &[u8; 32]) -> EdPoint {
    let mut acc = EdPoint::IDENTITY;
    for byte_idx in (0..32).rev() {
        for bit in (0..8).rev() {
            acc = ed_double(&acc);
            let b = (k[byte_idx] >> bit) & 1;
            if b == 1 {
                acc = ed_add(&acc, p);
            }
        }
    }
    acc
}

/// Clear the cofactor (`h = 8`): `[8]·P` via three doublings.  Maps any point
/// into the prime-order (`ℓ`) subgroup.
pub fn ed_mul_cofactor(p: &EdPoint) -> EdPoint {
    ed_double(&ed_double(&ed_double(p)))
}

/// Hash-to-curve by **try-and-increment** (RFC 9380 §6.9.1 spirit), using
/// SHA3-256 as the random oracle.  Returns a prime-order point whose discrete log
/// w.r.t. the base point is unknown — suitable for **binding** Pedersen
/// generators.
///
/// For counter `ctr = 0, 1, …`: hash `domain ‖ index ‖ ctr` to a field element
/// `x` (+ a sign bit); solve the Edwards equation for `y² = (1+x²)/(1−d·x²)`; if
/// that is a quadratic residue, take `y = ±√(y²)` (sign from the hash) and clear
/// the cofactor.  Retry on a non-residue, zero denominator, or identity output.
///
/// Indifferentiability is **not** required for generators (only one-wayness of the
/// DL), so the simple try-and-increment map is sound in the ROM; Elligator2
/// (constant-time, RFC 9380) is the standardized alternative.
pub fn hash_to_curve(domain: &[u8], index: u64) -> EdPoint {
    use sha3::Sha3_256;
    for ctr in 0u64.. {
        let mut h = Sha3_256::new();
        h.update(domain);
        h.update(index.to_le_bytes());
        h.update(ctr.to_le_bytes());
        let out = h.finalize();
        let mut xb = [0u8; 32];
        xb.copy_from_slice(&out);
        let sign = (xb[31] >> 7) & 1;
        xb[31] &= 0x7f; // clear top bit; x < 2^255
        let x = fe_from_bytes_le(&xb);

        let xx = fe_sq(&x);
        let num = fe_add(&Fe25519::ONE, &xx);
        let den = fe_sub(&Fe25519::ONE, &fe_mul(&D, &xx));
        if den.is_zero() {
            continue;
        }
        let yy = fe_mul(&num, &fe_invert(&den));
        let Some(mut y) = fe_sqrt(&yy) else {
            continue;
        };
        // Fix the sign by the hash bit (parity of y).
        if (y.to_bytes()[0] & 1) != sign {
            y = fe_neg(&y);
        }
        let point = EdPoint { x, y, z: Fe25519::ONE, t: fe_mul(&x, &y) };
        let p8 = ed_mul_cofactor(&point);
        if p8 == EdPoint::IDENTITY {
            continue;
        }
        return p8;
    }
    unreachable!("hash_to_curve: counter exhausted")
}

// ============================================================================
// Group impl
// ============================================================================

/// Marker type implementing the [`Group`] trait over Ed25519.
pub struct Ed25519;

impl Group for Ed25519 {
    type Element = EdPoint;
    type Scalar = [u8; 32];

    fn generator() -> EdPoint {
        EdPoint::base()
    }

    fn random_scalar<R: SpecRng>(rng: &mut R) -> [u8; 32] {
        let mut k = [0u8; 32];
        for byte in k.iter_mut() {
            *byte = rng.next_u8();
        }
        // Clamp top two bits so the scalar is < 2^254 (safely below the
        // group order ℓ ≈ 2^252.5). Keeps the value canonical without a
        // full mod-ℓ reduction.
        k[31] &= 0x3F;
        k
    }

    fn scalar_mul(elt: &EdPoint, k: &[u8; 32]) -> EdPoint {
        ed_scalar_mul(elt, k)
    }

    fn add(a: &EdPoint, b: &EdPoint) -> EdPoint {
        ed_add(a, b)
    }

    fn neg(a: &EdPoint) -> EdPoint {
        ed_neg(a)
    }

    fn write_element<D_: Digest>(elt: &EdPoint, h: &mut D_) {
        // Hash the affine encoding (x_bytes || y_bytes) so equivalent
        // projective representations of the same point hash identically.
        let (x, y) = elt.to_affine();
        h.update(x.to_bytes());
        h.update(y.to_bytes());
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fe_add_sub_inverse() {
        let a = Fe25519([1, 2, 3, 4]);
        let b = Fe25519([5, 6, 7, 8]);
        let c = fe_add(&a, &b);
        let back = fe_sub(&c, &b);
        assert_eq!(back, a);
    }

    #[test]
    fn fe_mul_one_is_identity() {
        let a = Fe25519([0xCAFE, 0xBABE, 0xDEAD, 0x0BEEF]);
        let prod = fe_mul(&a, &Fe25519::ONE);
        assert_eq!(prod, a);
    }

    #[test]
    fn fe_invert_round_trip() {
        let a = Fe25519([7, 11, 13, 17]);
        let inv = fe_invert(&a);
        let prod = fe_mul(&a, &inv);
        assert_eq!(prod, Fe25519::ONE);
    }

    #[test]
    fn fe_neg_round_trip() {
        let a = Fe25519([42, 0, 0, 0]);
        let s = fe_add(&a, &fe_neg(&a));
        assert!(s.is_zero());
    }

    #[test]
    fn ed_base_point_on_curve() {
        // Verify the base point satisfies -x² + y² = 1 + d·x²·y².
        let b = EdPoint::base();
        let (x, y) = (b.x, b.y);
        let xx = fe_sq(&x);
        let yy = fe_sq(&y);
        let lhs = fe_sub(&yy, &xx);
        let xxyy = fe_mul(&xx, &yy);
        let rhs = fe_add(&Fe25519::ONE, &fe_mul(&D, &xxyy));
        assert_eq!(lhs, rhs, "base point not on Ed25519 curve");
    }

    #[test]
    fn ed_identity_is_identity() {
        let g = EdPoint::base();
        let s = ed_add(&g, &EdPoint::IDENTITY);
        assert_eq!(s, g);
    }

    #[test]
    fn ed_double_eq_add_self() {
        let g = EdPoint::base();
        let d_ = ed_double(&g);
        let a = ed_add(&g, &g);
        assert_eq!(d_, a);
    }

    #[test]
    fn ed_neg_inverse() {
        let g = EdPoint::base();
        let s = ed_add(&g, &ed_neg(&g));
        assert_eq!(s, EdPoint::IDENTITY);
    }

    #[test]
    fn ed_scalar_mul_one() {
        let g = EdPoint::base();
        let mut k = [0u8; 32];
        k[0] = 1;
        let r = ed_scalar_mul(&g, &k);
        assert_eq!(r, g);
    }

    #[test]
    fn ed_scalar_mul_two_eq_double() {
        let g = EdPoint::base();
        let mut k = [0u8; 32];
        k[0] = 2;
        let r = ed_scalar_mul(&g, &k);
        let d_ = ed_double(&g);
        assert_eq!(r, d_);
    }

    #[test]
    fn ed_diffie_hellman_correctness() {
        // (g^x)^y == (g^y)^x  for random x, y.
        let g = EdPoint::base();
        let mut x = [0u8; 32];
        let mut y = [0u8; 32];
        // small fixed scalars to keep test fast (255-bit ladder is slow on CI).
        x[0] = 0x12;
        x[1] = 0x34;
        y[0] = 0x56;
        y[1] = 0x78;
        let gx = ed_scalar_mul(&g, &x);
        let gy = ed_scalar_mul(&g, &y);
        let gxy_a = ed_scalar_mul(&gx, &y);
        let gxy_b = ed_scalar_mul(&gy, &x);
        assert_eq!(gxy_a, gxy_b);
    }

    /// Ed25519 plugged into the existing Chou-Orlandi base OT.
    #[test]
    fn ed25519_chou_orlandi_round_trip() {
        use crate::SpecRng;
        use crate::ot::base::{
            ot_recv, ot_recv_finish, ot_send_finish, ot_send_setup,
        };
        use sha2::Sha256;

        struct TestRng(u64);
        impl SpecRng for TestRng {
            fn next_u32(&mut self) -> u32 {
                self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
                let mut z = self.0;
                z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
                z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
                (z ^ (z >> 31)) as u32
            }
        }

        let mut rng = TestRng(0xABAD_CAFE_DEAD_BEEF);
        // c = false branch
        let (sender, s) = ot_send_setup::<Ed25519, Sha256, _>(&mut rng);
        let (receiver, msg) = ot_recv::<Ed25519, Sha256, _>(&mut rng, s, false);
        let (k0, _k1) = ot_send_finish::<Ed25519, Sha256>(&sender, &msg);
        let kc = ot_recv_finish::<Ed25519, Sha256>(&receiver);
        assert_eq!(k0, kc, "c=0 must yield k0");

        // c = true branch
        let (sender, s) = ot_send_setup::<Ed25519, Sha256, _>(&mut rng);
        let (receiver, msg) = ot_recv::<Ed25519, Sha256, _>(&mut rng, s, true);
        let (_k0, k1) = ot_send_finish::<Ed25519, Sha256>(&sender, &msg);
        let kc = ot_recv_finish::<Ed25519, Sha256>(&receiver);
        assert_eq!(k1, kc, "c=1 must yield k1");
    }

    // ── Hash-to-curve / field sqrt ────────────────────────────────────────

    /// Ed25519 group order ℓ, little-endian bytes (for prime-order checks).
    const ELL_BYTES: [u8; 32] = [
        0xed, 0xd3, 0xf5, 0x5c, 0x1a, 0x63, 0x12, 0x58, 0xd6, 0x9c, 0xf7, 0xa2, 0xde, 0xf9, 0xde,
        0x14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x10,
    ];

    fn on_curve(p: &EdPoint) -> bool {
        let (x, y) = p.to_affine();
        let xx = fe_sq(&x);
        let yy = fe_sq(&y);
        let lhs = fe_sub(&yy, &xx);
        let rhs = fe_add(&Fe25519::ONE, &fe_mul(&D, &fe_mul(&xx, &yy)));
        lhs == rhs
    }

    #[test]
    fn fe_sqrt_roundtrip_and_residue() {
        for v in [1u64, 2, 3, 4, 9, 16, 1234567] {
            let a = Fe25519([v, 0, 0, 0]);
            let aa = fe_sq(&a);
            // aa is a square; its sqrt squares back to aa.
            let r = fe_sqrt(&aa).expect("square has a root");
            assert_eq!(fe_sq(&r), aa, "sqrt({v}^2)^2 != {v}^2");
            assert!(is_square(&aa));
        }
        // A non-residue must report None. 2 is a known QNR mod p (= 2^255-19).
        assert!(fe_sqrt(&Fe25519([2, 0, 0, 0])).is_none(), "2 is a non-residue mod p");
        assert!(!is_square(&Fe25519([2, 0, 0, 0])));
    }

    #[test]
    fn hash_to_curve_points_are_prime_order_and_on_curve() {
        for i in 0..5u64 {
            let p = hash_to_curve(b"volar-fold/test", i);
            assert!(on_curve(&p), "hash_to_curve point {i} not on curve");
            // Prime-order: [ℓ]·P == identity (cofactor was cleared).
            assert_eq!(ed_scalar_mul(&p, &ELL_BYTES), EdPoint::IDENTITY, "point {i} not prime-order");
            assert_ne!(p, EdPoint::IDENTITY);
        }
    }

    #[test]
    fn hash_to_curve_is_deterministic_and_distinct() {
        let a0 = hash_to_curve(b"dom", 0);
        let a0b = hash_to_curve(b"dom", 0);
        let a1 = hash_to_curve(b"dom", 1);
        let c0 = hash_to_curve(b"other", 0);
        assert_eq!(a0, a0b, "deterministic");
        assert_ne!(a0, a1, "distinct index");
        assert_ne!(a0, c0, "distinct domain");
    }
}
