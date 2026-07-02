// @reliability: experimental
// @ai: assisted
//! The spaced-packing `GF(2^k) → F_ℓ` embedding: R1CS + native-witness builder
//! for the woven-VOLE AND-gate/Quicksilver check
//! `K_a·K_b + V̂ + K_c·Δ = 0` in `GF(2^k)`, lowered to R1CS over the folding
//! scalar field.  See `docs/fold-lift-expansion.md` for the full derivation
//! (§2 parameters, §3 relation, §4 completeness, §5 soundness) — this module
//! is a mechanical, pinned implementation of that spec; the math is not
//! re-derived here.
//!
//! Reuses the [`crate::r1cs_builder`] gadget kit (shared with
//! [`crate::keccak_r1cs`]): [`Lc`](crate::r1cs_builder) linear combinations and
//! a [`Builder`](crate::r1cs_builder) that accumulates R1CS rows *and* the
//! satisfying witness together.

use alloc::vec::Vec;

use crate::r1cs::R1CS;
use crate::r1cs_builder::{Builder, Lc};
use crate::scalar::Scalar;

/// Parameters for the `GF(2^k)` embedding: spacing, per-column reduction
/// masks, and per-output-column half-bit widths.  Depends only on `(k, poly)`
/// — the same `Gf2kParams` is reused across every gate for a fixed field.
///
/// Fully pinned by `docs/fold-lift-expansion.md` §2; see that doc for the
/// derivation of `s`, `red`, and `m`.
#[derive(Clone, Debug)]
pub struct Gf2kParams {
    /// `GF(2^k)`'s degree.
    pub k: usize,
    /// Irreducible polynomial's low bits `p(x)`, `deg p < k`, `x^k` implicit
    /// (`P(x) = x^k + p(x)`).
    pub poly: u128,
    /// Spacing `s = ⌈log2(2k + 2)⌉` — the width of each bit-packed slot.
    pub s: usize,
    /// `red[j - k] = x^j mod P(x)` as a `k`-bit mask, for `j ∈ [k, 2k−1)`.
    pub red: Vec<u128>,
    /// `m[i]` — half-bit width for output column `i ∈ [0, k)`.
    pub m: Vec<usize>,
}

/// `⌈log2(bound)⌉` — smallest `e` with `2^e >= bound` (`bound >= 1`).
fn ceil_log2(bound: usize) -> usize {
    let mut e = 0usize;
    while (1usize << e) < bound {
        e += 1;
    }
    e
}

impl Gf2kParams {
    /// Builds the params for `GF(2^k)` with irreducible `P(x) = x^k + poly`.
    ///
    /// Panics on the §2 static soundness asserts — deliberately: this is the
    /// "single-limb only" boundary (`k` up to 16; `k=32` and beyond need a
    /// future multi-limb variant, out of scope here).
    pub fn new(k: usize, poly: u128) -> Self {
        assert!(k >= 1, "Gf2kParams::new: k must be >= 1, got {k}");
        assert!(
            poly < (1u128 << k),
            "Gf2kParams::new: poly {poly:#x} must be < 2^{k} (deg p < k)"
        );

        // Assert 2: 2k + 1 < 2^s, by definition of s (checked anyway).
        let s = ceil_log2(2 * k + 2);
        assert!(
            2 * k + 1 < (1usize << s),
            "Gf2kParams::new: spacing s={s} too small for k={k}"
        );

        // Assert 3: packed products never wrap mod ℓ.
        assert!(
            2 * k * s <= 250,
            "Gf2kParams::new: packed products would wrap mod ℓ (2*k*s = {} > 250; k={k} too \
             large for the single-limb embedding)",
            2 * k * s
        );
        // Assert 4: the packed sum S and its bit decomposition never wrap.
        assert!(
            (2 * k - 1) * s <= 250,
            "Gf2kParams::new: packed sum S would wrap mod ℓ ((2k-1)*s = {} > 250; k={k} too \
             large for the single-limb embedding)",
            (2 * k - 1) * s
        );

        // red[j-k] = x^j mod P(x), j in [k, 2k-1)  (k-1 entries; empty for k=1).
        let mut red: Vec<u128> = Vec::with_capacity(k.saturating_sub(1));
        let mut cur = poly; // x^k mod P(x) = poly (since x^k + poly = P(x), char 2).
        for _ in 0..k.saturating_sub(1) {
            red.push(cur);
            // x^{j+1} mod P = reduce(x · x^j mod P): shift, fold back if bit k set.
            let shifted = cur << 1;
            cur = if (shifted >> k) & 1 == 1 { (shifted ^ (1u128 << k)) ^ poly } else { shifted };
        }

        // weight_i, τmax_i, m_i per output column.
        let mut m = alloc::vec![0usize; k];
        for (i, mi) in m.iter_mut().enumerate() {
            let weight = red.iter().filter(|r| (*r >> i) & 1 == 1).count();
            let tau_max = (1 + weight) * (2 * k + 1);
            *mi = ceil_log2(tau_max / 2 + 1);
        }

        Gf2kParams { k, poly, s, red, m }
    }
}

/// `2^e` as a `Scalar` for every `e` in `0..=max_exp`, built by iterated
/// doubling (safe for `e >= 64`, where `1u64 << e` would overflow; exponents
/// here run up to ~250).
fn pow2_table(max_exp: usize) -> Vec<Scalar> {
    let mut table = Vec::with_capacity(max_exp + 1);
    table.push(Scalar::ONE);
    for i in 1..=max_exp {
        let prev = table[i - 1];
        table.push(prev.add(&prev));
    }
    table
}

/// Build the expanded AND-check R1CS *and* its satisfying witness together.
/// `a, b, c, d, v` are LSB-first `GF(2)` coefficient bits of `K_a, K_b, K_c,
/// Δ, V̂`; each must have length `params.k`.  The R1CS shape depends only on
/// `params`, never on the bit values (§6 contract).
///
/// Implements `docs/fold-lift-expansion.md` §3 exactly: witness allocation
/// order `a‖b‖c‖d‖v, P_ab, P_cd, e(j-major,t-minor), h(i-major,t-minor)`;
/// constraint emission order `C1..C6`.
pub fn and_check_gf2k(
    params: &Gf2kParams,
    a: &[bool],
    b: &[bool],
    c: &[bool],
    d: &[bool],
    v: &[bool],
) -> (R1CS, Vec<Scalar>) {
    let k = params.k;
    let s = params.s;
    assert_eq!(a.len(), k, "a must have length k");
    assert_eq!(b.len(), k, "b must have length k");
    assert_eq!(c.len(), k, "c must have length k");
    assert_eq!(d.len(), k, "d must have length k");
    assert_eq!(v.len(), k, "v must have length k");

    let num_cols = 2 * k - 1; // j ∈ [0, 2k-1)

    let mut bld = Builder::new();

    // ── C1: 5k input bits (also allocates a_0..v_{k-1} in that order) ──────
    let a_lc: Vec<Lc> = a.iter().map(|&x| bld.input_bit(x)).collect();
    let b_lc: Vec<Lc> = b.iter().map(|&x| bld.input_bit(x)).collect();
    let c_lc: Vec<Lc> = c.iter().map(|&x| bld.input_bit(x)).collect();
    let d_lc: Vec<Lc> = d.iter().map(|&x| bld.input_bit(x)).collect();
    let v_lc: Vec<Lc> = v.iter().map(|&x| bld.input_bit(x)).collect();

    // Table covers every power of two this gate needs: packing (up to
    // (k-1)*s), e-bit slots (up to s-1), the C4 column scale (up to
    // (num_cols-1)*s), and h-bit slots (up to max(m)-1).
    let max_exp = [
        (k.saturating_sub(1)) * s,
        s.saturating_sub(1),
        (num_cols.saturating_sub(1)) * s,
        params.m.iter().copied().max().unwrap_or(1).saturating_sub(1),
    ]
    .into_iter()
    .max()
    .unwrap_or(0);
    let pow2 = pow2_table(max_exp);

    let pack = |bits_lc: &[Lc]| -> Lc {
        let mut acc = Lc::zero();
        for (i, lc) in bits_lc.iter().enumerate() {
            acc = acc.add(&lc.scale(&pow2[i * s]));
        }
        acc
    };
    let a_packed = pack(&a_lc);
    let b_packed = pack(&b_lc);
    let c_packed = pack(&c_lc);
    let d_packed = pack(&d_lc);
    let v_packed = pack(&v_lc);

    // ── C2: the two spaced products A·B = P_ab, C·D = P_cd ─────────────────
    // Assert 3 (2*k*s <= 250) guarantees the F_ℓ product equals the true
    // integer spaced product, so evaluating in the field gives the honest
    // witness value directly.
    let p_ab_val = bld.eval(&a_packed).mul(&bld.eval(&b_packed));
    let p_ab_var = bld.alloc(p_ab_val);
    let p_ab_lc = Lc::from_var(p_ab_var);
    bld.enforce(&a_packed, &b_packed, &p_ab_lc);

    let p_cd_val = bld.eval(&c_packed).mul(&bld.eval(&d_packed));
    let p_cd_var = bld.alloc(p_cd_val);
    let p_cd_lc = Lc::from_var(p_cd_var);
    bld.enforce(&c_packed, &d_packed, &p_cd_lc);

    // ── Native column sums σ_j (small integers; never assembled into one
    // wide S — each σ_j is its own bounded count, per the brief's warning
    // that a single u128 S doesn't fit for larger k). ─────────────────────
    let mut sigma_native = alloc::vec![0u32; num_cols];
    for (j, sj) in sigma_native.iter_mut().enumerate() {
        let mut sum = 0u32;
        for i in 0..k {
            if j >= i && (j - i) < k {
                if a[i] && b[j - i] {
                    sum += 1;
                }
                if c[i] && d[j - i] {
                    sum += 1;
                }
            }
        }
        if j < k && v[j] {
            sum += 1;
        }
        *sj = sum;
    }

    // ── C3: e-bits, j-major t-minor; also build σ_j as an Lc. ──────────────
    let mut sigma_lc: Vec<Lc> = Vec::with_capacity(num_cols);
    for &sigma_j in &sigma_native {
        let mut sj_lc = Lc::zero();
        for t in 0..s {
            let bit = (sigma_j >> t) & 1 == 1;
            let e_lc = bld.input_bit(bit);
            sj_lc = sj_lc.add(&e_lc.scale(&pow2[t]));
        }
        sigma_lc.push(sj_lc);
    }

    // ── C4: (P_ab + P_cd + V − Σ_{j,t} e_{j,t}·2^{js+t}) · 1 = 0 ────────────
    let mut sum_e = Lc::zero();
    for (j, sj_lc) in sigma_lc.iter().enumerate() {
        sum_e = sum_e.add(&sj_lc.scale(&pow2[j * s]));
    }
    let c4_lc = p_ab_lc.add(&p_cd_lc).add(&v_packed).sub(&sum_e);
    bld.enforce_linear(&c4_lc);

    // Native τ_i = σ_i + Σ_{j>=k, red[j] bit i} σ_j.
    let mut tau_native = alloc::vec![0u32; k];
    for (i, ti) in tau_native.iter_mut().enumerate() {
        let mut t = sigma_native[i];
        for (jj, &red_val) in params.red.iter().enumerate() {
            let j = k + jj;
            if (red_val >> i) & 1 == 1 {
                t += sigma_native[j];
            }
        }
        *ti = t;
    }

    // ── C5 + C6: h-bits per output column, then τ_i − 2·h_i = 0. ───────────
    let two = Scalar::from_u64(2);
    for i in 0..k {
        let mi = params.m[i];
        let h_native = tau_native[i] / 2; // honest values: exact; adversarial: floors down.
        let mut h_lc = Lc::zero();
        for t in 0..mi {
            let bit = (h_native >> t) & 1 == 1;
            let h_bit_lc = bld.input_bit(bit);
            h_lc = h_lc.add(&h_bit_lc.scale(&pow2[t]));
        }

        let mut tau_i_lc = sigma_lc[i].clone();
        for (jj, &red_val) in params.red.iter().enumerate() {
            let j = k + jj;
            if (red_val >> i) & 1 == 1 {
                tau_i_lc = tau_i_lc.add(&sigma_lc[j]);
            }
        }
        let c6_lc = tau_i_lc.sub(&h_lc.scale(&two));
        bld.enforce_linear(&c6_lc);
    }

    bld.finish()
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use std::time::Instant;

    // ── Local GF(2^8) reference arithmetic (test-only; volar-fold does not
    // depend on volar-primitives, so this mirrors `gf_mul_u8`/`gf_invert_u8`
    // in ~10 lines rather than adding a crate dependency). ──────────────────
    fn gf_mul(a: u8, b: u8, poly: u8) -> u8 {
        let mut p = 0u8;
        let mut a = a;
        let mut b = b;
        for _ in 0..8 {
            if b & 1 != 0 {
                p ^= a;
            }
            let hi = a & 0x80;
            a <<= 1;
            if hi != 0 {
                a ^= poly;
            }
            b >>= 1;
        }
        p
    }

    fn gf_invert(a: u8, poly: u8) -> u8 {
        // a^(2^8 - 2) via square-and-multiply (Fermat inverse).
        let mut result = 1u8;
        let mut base = a;
        let mut e = 254u8;
        while e > 0 {
            if e & 1 == 1 {
                result = gf_mul(result, base, poly);
            }
            base = gf_mul(base, base, poly);
            e >>= 1;
        }
        result
    }

    fn bits_lsb(x: u64, k: usize) -> std::vec::Vec<bool> {
        (0..k).map(|i| (x >> i) & 1 == 1).collect()
    }

    fn next_byte(state: &mut u64) -> u8 {
        *state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        (*state >> 56) as u8
    }

    /// A random honest `(K_a, K_b, K_c, Δ, V̂)` tuple satisfying
    /// `K_c = (K_a·K_b + V̂)·Δ⁻¹` in `GF(2^8)`, `Δ ≠ 0`.
    fn honest_tuple(state: &mut u64) -> (u8, u8, u8, u8, u8) {
        let ka = next_byte(state);
        let kb = next_byte(state);
        let vv = next_byte(state);
        let mut delta = next_byte(state);
        while delta == 0 {
            delta = next_byte(state);
        }
        let sum = gf_mul(ka, kb, 0x1b) ^ vv;
        let kc = gf_mul(sum, gf_invert(delta, 0x1b), 0x1b);
        (ka, kb, kc, delta, vv)
    }

    fn build_and_z(
        params: &Gf2kParams,
        ka: u8,
        kb: u8,
        kc: u8,
        delta: u8,
        vv: u8,
    ) -> (R1CS, std::vec::Vec<Scalar>, std::vec::Vec<Scalar>) {
        let (r1cs, w) = and_check_gf2k(
            params,
            &bits_lsb(ka as u64, 8),
            &bits_lsb(kb as u64, 8),
            &bits_lsb(kc as u64, 8),
            &bits_lsb(delta as u64, 8),
            &bits_lsb(vv as u64, 8),
        );
        let z = r1cs.full_z(&w, &Scalar::ONE);
        (r1cs, w, z)
    }

    // ── Item 1: pin constants (k=8 and k=1). ────────────────────────────────
    #[test]
    fn item1_pin_constants_k8() {
        let p = Gf2kParams::new(8, 0x1b);
        assert_eq!(p.s, 5);
        assert_eq!(p.red, std::vec![0x1bu128, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a]);
        assert_eq!(p.m, std::vec![6usize; 8]);

        let (r1cs, w, _) = build_and_z(&p, 1, 1, gf_mul(1, 1, 0x1b), 1, 0);
        assert_eq!(r1cs.num_cons, 174);
        assert_eq!(r1cs.num_vars, 166);
        assert_eq!(w.len(), 165);
    }

    #[test]
    fn item1_pin_constants_k1() {
        let p = Gf2kParams::new(1, 0);
        assert_eq!(p.s, 2);
        assert!(p.red.is_empty());
        let (r1cs, w) = and_check_gf2k(&p, &[false], &[false], &[false], &[false], &[false]);
        assert_eq!(r1cs.num_cons, 12);
        assert_eq!(r1cs.num_vars, 11);
        assert_eq!(w.len(), 10);
    }

    // ── Item 2: exhaustive k=8 completeness (all 2^16 (K_a,K_b)). ───────────
    #[test]
    fn item2_exhaustive_k8_completeness() {
        let params = Gf2kParams::new(8, 0x1b);
        let start = Instant::now();
        for ka in 0u32..=255 {
            for kb in 0u32..=255 {
                let ka8 = ka as u8;
                let kb8 = kb as u8;
                let kc8 = gf_mul(ka8, kb8, 0x1b);
                let (r1cs, _w, z) = build_and_z(&params, ka8, kb8, kc8, 1, 0);
                assert!(r1cs.is_satisfied(&z), "Ka={ka8:#x} Kb={kb8:#x}");
            }
        }
        std::eprintln!("item2_exhaustive_k8_completeness: {:?}", start.elapsed());
    }

    // ── Item 3: randomized honest tuples, deterministic LCG, >=256, plus
    // edge values. ───────────────────────────────────────────────────────────
    #[test]
    fn item3_randomized_honest_tuples() {
        let params = Gf2kParams::new(8, 0x1b);
        let mut state = 0x243F_6A88_85A3_08D3u64;

        let mut cases: std::vec::Vec<(u8, u8, u8, u8)> = std::vec::Vec::new();
        for &edge in &[0u8, 1, 0xff] {
            cases.push((edge, edge, edge, 1));
        }
        while cases.len() < 256 {
            let ka = next_byte(&mut state);
            let kb = next_byte(&mut state);
            let vv = next_byte(&mut state);
            let mut delta = next_byte(&mut state);
            while delta == 0 {
                delta = next_byte(&mut state);
            }
            cases.push((ka, kb, vv, delta));
        }

        for (ka, kb, vv, delta) in cases {
            let sum = gf_mul(ka, kb, 0x1b) ^ vv;
            let kc = gf_mul(sum, gf_invert(delta, 0x1b), 0x1b);
            assert_eq!(gf_mul(kc, delta, 0x1b), sum, "sanity: Kc·Δ == Ka·Kb + V̂");
            let (r1cs, _w, z) = build_and_z(&params, ka, kb, kc, delta, vv);
            assert!(
                r1cs.is_satisfied(&z),
                "ka={ka:#x} kb={kb:#x} kc={kc:#x} delta={delta:#x} v={vv:#x}"
            );
        }
    }

    // ── Item 4: negatives. ───────────────────────────────────────────────────
    #[test]
    fn item4_negatives() {
        let params = Gf2kParams::new(8, 0x1b);
        let ka = 0x37u8;
        let kb = 0x82u8;
        let delta = 1u8;
        let vv = 0u8;
        let kc = gf_mul(ka, kb, 0x1b);

        // Honest baseline is satisfied.
        let (r1cs, _w, z) = build_and_z(&params, ka, kb, kc, delta, vv);
        assert!(r1cs.is_satisfied(&z));

        // Flip a bit of K_c.
        let (r1cs, _w, z) = build_and_z(&params, ka, kb, kc ^ 0x01, delta, vv);
        assert!(!r1cs.is_satisfied(&z), "flipped K_c bit must be unsatisfied");

        // Flip a bit of V̂.
        let (r1cs, _w, z) = build_and_z(&params, ka, kb, kc, delta, vv ^ 0x01);
        assert!(!r1cs.is_satisfied(&z), "flipped V̂ bit must be unsatisfied");

        // Naive-embedding counterexample family: an F_ℓ-integer product on the
        // packed bytes that disagrees with the true GF(2^8) product.
        let naive_cases: [(u8, u8); 3] = [(0x10, 0x10), (0x53, 0xCA), (0x02, 0x80)];
        for (a8, b8) in naive_cases {
            let gf_c = gf_mul(a8, b8, 0x1b);
            let naive_c = ((a8 as u16 * b8 as u16) & 0xff) as u8;
            assert_ne!(gf_c, naive_c, "chosen pair must differ under GF vs naive product");
            let (r1cs, _w, z) = build_and_z(&params, a8, b8, naive_c, 1, 0);
            assert!(
                !r1cs.is_satisfied(&z),
                "naive-embedding false GF tuple must be unsatisfied: a={a8:#x} b={b8:#x}"
            );
        }
    }

    // ── Item 5: k=1 exhaustive, both directions. ────────────────────────────
    #[test]
    fn item5_k1_exhaustive() {
        let params = Gf2kParams::new(1, 0);
        for bits in 0u32..32 {
            let a = (bits & 1) != 0;
            let b = (bits & 2) != 0;
            let c = (bits & 4) != 0;
            let d = (bits & 8) != 0;
            let v = (bits & 16) != 0;
            let (r1cs, w) = and_check_gf2k(&params, &[a], &[b], &[c], &[d], &[v]);
            let z = r1cs.full_z(&w, &Scalar::ONE);
            let sat = r1cs.is_satisfied(&z);
            let expected = !((a & b) ^ v ^ (c & d));
            assert_eq!(sat, expected, "a={a} b={b} c={c} d={d} v={v}");
        }
    }

    // ── Item 6: tampered-witness soundness probes — every one of the 165
    // witness variables, for 3 honest k=8 witnesses. ────────────────────────
    #[test]
    fn item6_tampered_witness_probes() {
        let params = Gf2kParams::new(8, 0x1b);
        let mut state = 0xD134_2543_DE82_EF95u64;
        let idx_p_ab = 5 * 8;
        let idx_p_cd = 5 * 8 + 1;

        for _ in 0..3 {
            let (ka, kb, kc, delta, vv) = honest_tuple(&mut state);
            let (r1cs, w, z) = build_and_z(&params, ka, kb, kc, delta, vv);
            assert!(r1cs.is_satisfied(&z), "baseline honest witness must satisfy");

            for i in 0..w.len() {
                let mut tw = w.clone();
                if i == idx_p_ab || i == idx_p_cd {
                    tw[i] = tw[i].add(&Scalar::ONE);
                } else {
                    tw[i] = Scalar::ONE.sub(&tw[i]);
                }
                let tz = r1cs.full_z(&tw, &Scalar::ONE);
                assert!(
                    !r1cs.is_satisfied(&tz),
                    "tamper of witness var {i} must break satisfaction (ka={ka:#x} kb={kb:#x} \
                     kc={kc:#x} delta={delta:#x} v={vv:#x})"
                );
            }
        }
    }

    // ── Item 7: fold two honest gate witnesses via `cross_term_z`. ─────────
    #[test]
    fn item7_fold_via_cross_term_z() {
        let params = Gf2kParams::new(8, 0x1b);
        let mut state = 0x9E37_79B9_7F4A_7C15u64;
        let (ka1, kb1, kc1, d1, v1) = honest_tuple(&mut state);
        let (ka2, kb2, kc2, d2, v2) = honest_tuple(&mut state);

        let (r1cs, w1, _) = build_and_z(&params, ka1, kb1, kc1, d1, v1);
        let (_r1cs2, w2, _) = build_and_z(&params, ka2, kb2, kc2, d2, v2);

        let u1 = Scalar::ONE;
        let u2 = Scalar::ONE;
        let t = crate::nifs::cross_term_z(&r1cs, &w1, &u1, &w2, &u2);
        let r = Scalar::from_u64(0xabcd);

        let w_folded: std::vec::Vec<Scalar> =
            w1.iter().zip(w2.iter()).map(|(x, y)| x.add(&r.mul(y))).collect();
        // Fresh instances: e1 = e2 = 0, so e' = r·t.
        let e_folded: std::vec::Vec<Scalar> = t.iter().map(|ti| r.mul(ti)).collect();
        let u_folded = u1.add(&r.mul(&u2));

        assert!(r1cs.is_satisfied_relaxed(&w_folded, &e_folded, &u_folded));
    }

    // ── Item 8: shape determinism across two different honest inputs. ──────
    #[test]
    fn item8_shape_determinism() {
        let params = Gf2kParams::new(8, 0x1b);
        let (r1cs_a, _, _) = build_and_z(&params, 0x11, 0x22, gf_mul(0x11, 0x22, 0x1b), 1, 0);
        let (r1cs_b, _, _) = build_and_z(&params, 0xAB, 0xCD, gf_mul(0xAB, 0xCD, 0x1b), 1, 0);
        assert_eq!(r1cs_a.num_cons, r1cs_b.num_cons);
        assert_eq!(r1cs_a.num_vars, r1cs_b.num_vars);
        assert_eq!(r1cs_a.a, r1cs_b.a);
        assert_eq!(r1cs_a.b, r1cs_b.b);
        assert_eq!(r1cs_a.c, r1cs_b.c);
    }

    // ── Item 9: panic tests (out-of-scope params). ──────────────────────────
    #[test]
    #[should_panic]
    fn item9_panic_k_too_large() {
        let _ = Gf2kParams::new(32, 0x1);
    }

    #[test]
    #[should_panic]
    fn item9_panic_k_zero() {
        let _ = Gf2kParams::new(0, 0);
    }

    #[test]
    #[should_panic]
    fn item9_panic_poly_too_big() {
        let _ = Gf2kParams::new(8, 0x100);
    }
}
