// @reliability: experimental
//! @ai: assisted
//! Post-quantum 1-out-of-2 OT from Learning With Errors.
//!
//! Replaces the discrete-log-based [`super::base`] (Chou-Orlandi over
//! Ed25519) with an LWE construction whose security reduces to the
//! standard-LWE assumption — believed hard against quantum adversaries.
//!
//! # Protocol (semi-honest, public CRS `A`, public reference `h`)
//!
//! Hat-style "two-public-keys" OT (Frodo / PVW lineage):
//!
//! ```text
//! CRS:  A ∈ Z_q^{n×n}, h ∈ Z_q^n  (sampled uniformly)
//! ```
//!
//! Receiver (choice bit `c`):
//! 1. Sample LWE secret `s ←$ Z_q^n` and noise `e ←$ χ`.
//! 2. Compute `pk' = A·s + e`.
//! 3. Set `pk_c = pk'`, `pk_{1-c} = h - pk'`. Send `pk_0` to sender.
//!
//! Sender (messages `m_0, m_1 ∈ {0,1}^L`, with `L · 8 ≤ n`):
//! 1. Receive `pk_0`, derive `pk_1 = h - pk_0`.
//! 2. For `i ∈ {0,1}`: pick `r_i ←$ χ^n`, `e_i ←$ χ`, `e'_i ←$ χ`.
//! 3. `u_i = A^T · r_i + e_i ∈ Z_q^n`,
//!    `v_i = pk_i^T · r_i + e'_i + ⌊q/2⌋ · m_i ∈ Z_q^L`
//!    (treating each bit of `m_i` as one Z_q coordinate of `v_i`).
//! 4. Send `(u_0, v_0, u_1, v_1)`.
//!
//! Receiver:
//! 5. Decrypt: `m̃_c = v_c − s^T · u_c ∈ Z_q^L`. Round each coord:
//!    bit = `1` if `m̃_c[k] ∈ (q/4, 3q/4]`, else `0`. The other branch
//!    `m̃_{1-c}` is a uniform-looking decoy because `pk_{1-c} = h − A·s −
//!    e` is not in the LWE language for the receiver's `s`.
//!
//! # Security
//!
//! - LWE hides `s` and `pk' = A·s + e` is pseudorandom; therefore
//!   `pk_{1-c} = h − pk'` is also pseudorandom and the sender cannot
//!   distinguish which branch is "live".
//! - Decryption correctness requires the noise budget
//!   `B · (n · ‖r‖∞ + 1) + ‖e'‖∞ < q/4` (per coordinate of `v`).
//!
//! # Parameters
//!
//! For the unit tests below: `n = 16`, `q = 65536` (= 2^16), uniform noise
//! in `[-1, 1]`. **Insecure** sizing — for real PQ security use
//! `n ≥ 640`, `q ≈ 2^15`, discrete-Gaussian noise (Frodo-640 parameters).

use core::ops::Add;

use crate::SpecRng;

/// Lattice dimension for the test instantiation.
pub const LWE_N: usize = 16;
/// Modulus `q`. Power of two so rounding is "top bit of the residue".
pub const LWE_Q_BITS: u32 = 16;
pub const LWE_Q: u32 = 1u32 << LWE_Q_BITS;
const LWE_Q_MASK: u32 = LWE_Q - 1;
/// Maximum noise magnitude. Tight for the test parameters above.
pub const LWE_NOISE_BOUND: u32 = 1;

/// One element of `Z_q`, stored canonically in `[0, q)` as `u32`.
pub type Zq = u32;

#[inline]
fn zq_add(a: Zq, b: Zq) -> Zq { (a.wrapping_add(b)) & LWE_Q_MASK }
#[inline]
fn zq_sub(a: Zq, b: Zq) -> Zq { (a.wrapping_sub(b)) & LWE_Q_MASK }
#[inline]
fn zq_mul(a: Zq, b: Zq) -> Zq { (a.wrapping_mul(b)) & LWE_Q_MASK }
#[inline]
fn zq_neg(a: Zq) -> Zq { (LWE_Q.wrapping_sub(a)) & LWE_Q_MASK }

/// Sample a noise value uniformly in `[-B, B]`, returned as a canonical `Zq`.
fn sample_noise<R: SpecRng>(rng: &mut R) -> Zq {
    // Range size: 2·B + 1.
    let span = 2 * LWE_NOISE_BOUND + 1;
    let raw = rng.next_u32() % span;
    // Map [0, span) → [-B, B] then to Z_q.
    if raw <= LWE_NOISE_BOUND {
        raw
    } else {
        zq_neg(raw - LWE_NOISE_BOUND)
    }
}

/// Sample a uniform Z_q element.
fn sample_zq<R: SpecRng>(rng: &mut R) -> Zq {
    rng.next_u32() & LWE_Q_MASK
}

/// CRS — public matrix `A` and public reference vector `h`.
pub struct LweOtCrs {
    pub a: [[Zq; LWE_N]; LWE_N], // A[i][j] = (A)_{i,j}
    pub h: [Zq; LWE_N],
}

impl LweOtCrs {
    pub fn sample<R: SpecRng>(rng: &mut R) -> Self {
        let mut a = [[0u32; LWE_N]; LWE_N];
        for i in 0..LWE_N {
            for j in 0..LWE_N {
                a[i][j] = sample_zq(rng);
            }
        }
        let mut h = [0u32; LWE_N];
        for i in 0..LWE_N { h[i] = sample_zq(rng); }
        Self { a, h }
    }
}

// ============================================================================
// Receiver
// ============================================================================

/// Receiver state: knows secret `s` for `pk_c` only.
pub struct LweOtReceiver {
    pub s: [Zq; LWE_N],
    pub c: bool,
}

/// Receiver-to-sender message: the chosen `pk_0` (sender derives `pk_1 = h - pk_0`).
pub struct LweOtRecvMsg {
    pub pk0: [Zq; LWE_N],
}

/// Step 1: receiver samples `s, e`, computes `pk_c = A·s + e`, derives
/// `pk_{1-c} = h − pk_c`, returns `pk_0` to the sender.
pub fn lwe_ot_recv<R: SpecRng>(
    rng: &mut R,
    crs: &LweOtCrs,
    c: bool,
) -> (LweOtReceiver, LweOtRecvMsg) {
    // Small-secret LWE: sample `s` from the noise distribution rather than
    // uniformly. Required for decryption correctness — under uniform-`s`,
    // the cross term `s^T · e_u` dwarfs the `q/4` decoding margin.
    let mut s = [0u32; LWE_N];
    for i in 0..LWE_N { s[i] = sample_noise(rng); }

    // pk' = A·s + e
    let mut pk_real = [0u32; LWE_N];
    for i in 0..LWE_N {
        let mut acc: Zq = 0;
        for j in 0..LWE_N {
            acc = zq_add(acc, zq_mul(crs.a[i][j], s[j]));
        }
        acc = zq_add(acc, sample_noise(rng));
        pk_real[i] = acc;
    }

    let pk0 = if c {
        // c = 1 ⇒ pk_1 = pk', pk_0 = h - pk'.
        let mut pk0 = [0u32; LWE_N];
        for i in 0..LWE_N { pk0[i] = zq_sub(crs.h[i], pk_real[i]); }
        pk0
    } else {
        // c = 0 ⇒ pk_0 = pk'.
        pk_real
    };

    (LweOtReceiver { s, c }, LweOtRecvMsg { pk0 })
}

// ============================================================================
// Sender
// ============================================================================

/// Sender's encrypted payload. `u_i ∈ Z_q^n`, `v_i ∈ Z_q^L`.
pub struct LweOtSenderMsg<const L: usize> {
    pub u0: [Zq; LWE_N],
    pub v0: [Zq; L],
    pub u1: [Zq; LWE_N],
    pub v1: [Zq; L],
}

fn encrypt_branch<R: SpecRng, const L: usize>(
    rng: &mut R,
    crs: &LweOtCrs,
    pk: &[Zq; LWE_N],
    msg: &[u8; L],
) -> ([Zq; LWE_N], [Zq; L]) {
    // r ←$ χ^n
    let mut r = [0u32; LWE_N];
    for i in 0..LWE_N { r[i] = sample_noise(rng); }

    // u = A^T · r + e
    let mut u = [0u32; LWE_N];
    for j in 0..LWE_N {
        let mut acc: Zq = 0;
        for i in 0..LWE_N {
            acc = zq_add(acc, zq_mul(crs.a[i][j], r[i]));
        }
        acc = zq_add(acc, sample_noise(rng));
        u[j] = acc;
    }

    // base = pk^T · r ∈ Z_q
    let mut base: Zq = 0;
    for i in 0..LWE_N { base = zq_add(base, zq_mul(pk[i], r[i])); }

    // v_k = base + e' + ⌊q/2⌋ · msg[k]
    // API: msg[k] ∈ {0, 1} per coord (one bit per byte, not bit-packed).
    let half_q = LWE_Q / 2;
    let mut v = [0u32; L];
    for k in 0..L {
        let plain = if msg[k] & 1 == 1 { half_q } else { 0 };
        v[k] = zq_add(zq_add(base, sample_noise(rng)), plain);
    }
    (u, v)
}

/// Step 2: sender encrypts `(m_0, m_1)` under `(pk_0, pk_1 = h − pk_0)`.
pub fn lwe_ot_send<R: SpecRng, const L: usize>(
    rng: &mut R,
    crs: &LweOtCrs,
    recv_msg: &LweOtRecvMsg,
    m0: &[u8; L],
    m1: &[u8; L],
) -> LweOtSenderMsg<L> {
    let pk0 = recv_msg.pk0;
    let mut pk1 = [0u32; LWE_N];
    for i in 0..LWE_N { pk1[i] = zq_sub(crs.h[i], pk0[i]); }

    let (u0, v0) = encrypt_branch::<R, L>(rng, crs, &pk0, m0);
    let (u1, v1) = encrypt_branch::<R, L>(rng, crs, &pk1, m1);
    LweOtSenderMsg { u0, v0, u1, v1 }
}

// ============================================================================
// Receiver decrypt
// ============================================================================

/// Step 3: receiver decrypts the chosen branch using `s`.
///
/// Output: recovered `L`-coordinate message bits, packed into `L/8` bytes.
pub fn lwe_ot_recv_decrypt<const L: usize>(
    receiver: &LweOtReceiver,
    sender_msg: &LweOtSenderMsg<L>,
) -> [u8; L] {
    let (u, v) = if receiver.c {
        (&sender_msg.u1, &sender_msg.v1)
    } else {
        (&sender_msg.u0, &sender_msg.v0)
    };
    let mut s_dot_u: Zq = 0;
    for i in 0..LWE_N { s_dot_u = zq_add(s_dot_u, zq_mul(receiver.s[i], u[i])); }

    let quarter = LWE_Q / 4;
    let three_quarter = 3 * quarter;
    let mut out = [0u8; L];
    for k in 0..L {
        let raw = zq_sub(v[k], s_dot_u);
        // Round to bit.
        out[k] = if raw > quarter && raw <= three_quarter { 1 } else { 0 };
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

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

    fn run_ot<const L: usize>(c: bool, m0: &[u8; L], m1: &[u8; L]) -> [u8; L] {
        let mut rng = TestRng(0xDEAD_BEEF_CAFE_F00D);
        let crs = LweOtCrs::sample(&mut rng);
        let (recv, msg) = lwe_ot_recv(&mut rng, &crs, c);
        let send_msg = lwe_ot_send(&mut rng, &crs, &msg, m0, m1);
        lwe_ot_recv_decrypt(&recv, &send_msg)
    }

    #[test]
    fn lwe_ot_short_message_c0() {
        // L = 2 coords: [bit_0, bit_1] one bit per byte.
        let m0 = [1u8, 1u8];
        let m1 = [0u8, 0u8];
        let recovered = run_ot::<2>(false, &m0, &m1);
        assert_eq!(recovered, [1, 1]);
    }

    /// Full message round-trip: every coord recovered.
    #[test]
    fn lwe_ot_full_message_c0() {
        const L: usize = 16;
        let mut m0 = [0u8; L];
        let mut m1 = [0u8; L];
        for k in 0..L {
            m0[k] = (k as u8 & 1);  // alternating 0/1
            m1[k] = !(k as u8 & 1) & 1;
        }
        let recovered = run_ot::<L>(false, &m0, &m1);
        assert_eq!(recovered, m0, "c=0 must recover m_0 exactly");
    }

    #[test]
    fn lwe_ot_full_message_c1() {
        const L: usize = 16;
        let mut m0 = [0u8; L];
        let mut m1 = [0u8; L];
        for k in 0..L {
            m0[k] = (k as u8 & 1);
            m1[k] = !(k as u8 & 1) & 1;
        }
        let recovered = run_ot::<L>(true, &m0, &m1);
        assert_eq!(recovered, m1, "c=1 must recover m_1 exactly");
    }

    /// Many trials: every coord decoded correctly across multiple seeds.
    #[test]
    fn lwe_ot_stress_multiple_seeds() {
        const L: usize = 16;
        for seed in 0u64..16 {
            let mut rng = TestRng(seed.wrapping_mul(0xDEAD_BEEF));
            let crs = LweOtCrs::sample(&mut rng);
            let mut m0 = [0u8; L];
            let mut m1 = [0u8; L];
            for k in 0..L {
                m0[k] = ((seed >> k) & 1) as u8;
                m1[k] = !((seed >> k) & 1) as u8 & 1;
            }
            for c in [false, true] {
                let (recv, msg) = lwe_ot_recv(&mut rng, &crs, c);
                let send_msg = lwe_ot_send(&mut rng, &crs, &msg, &m0, &m1);
                let recovered = lwe_ot_recv_decrypt(&recv, &send_msg);
                let expected = if c { m1 } else { m0 };
                assert_eq!(recovered, expected, "seed {seed} c={c} mismatch");
            }
        }
    }

    /// Across many seeds, attempting to decrypt the *other* branch with the
    /// receiver's `s` produces approximately uniform output — it should NOT
    /// reliably recover `m_{1−c}`. We assert that across 8 seeds, at least
    /// one cheat-decoded coordinate differs from `m_{1−c}`. (This is a
    /// sanity check, not a security proof.)
    #[test]
    fn lwe_ot_other_branch_is_noise() {
        const L: usize = 16;
        let m0 = [1u8; L];
        let m1 = [0u8; L];
        let mut at_least_one_mismatch = false;
        for seed in 0u64..8 {
            let mut rng = TestRng(seed.wrapping_mul(0xABCD_EF01));
            let crs = LweOtCrs::sample(&mut rng);
            let (mut recv, msg) = lwe_ot_recv(&mut rng, &crs, false);
            let send_msg = lwe_ot_send(&mut rng, &crs, &msg, &m0, &m1);
            // Honest c=0 must always recover m0.
            let honest = lwe_ot_recv_decrypt(&recv, &send_msg);
            assert_eq!(honest, m0, "seed {seed} honest decode failed");
            // Cheat: claim c=1, decrypt other branch.
            recv.c = true;
            let cheat = lwe_ot_recv_decrypt(&recv, &send_msg);
            if cheat != m1 {
                at_least_one_mismatch = true;
            }
        }
        assert!(at_least_one_mismatch,
                "cheat decode matched m_1 across all 8 seeds — privacy bug?");
    }

    // Allow `Add` import to be technically unused in this module.
    #[allow(dead_code)]
    fn _unused_marker(_: impl Add<u32, Output = u32>) {}
}
