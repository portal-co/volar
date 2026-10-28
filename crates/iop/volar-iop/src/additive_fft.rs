//! Additive FFT over `GF(2^m)` for the Reed–Solomon encoder (S4).
//!
//! The naive RS `encode` is `O(n·k)` Horner (per-point power recomputation).
//! The Gao–Mateer additive FFT evaluates the message polynomial at the
//! `n = 2^s` integer points `0..n` — the subspace `span(e_0..e_{s-1})` with
//! `e_j = from_u128(2^j)` — in `O(n log n)`.
//!
//! The algorithm (the recursive coset split). For a subspace
//! `W_k = span(β_0..β_{k-1})`, the subspace-vanishing polynomial
//! `s_{k-1}(X) = prod_{a ∈ W_{k-1}} (X − a)` is **linearized**
//! (`s(X+Y) = s(X)+s(Y)`), so it's constant on each coset of `W_{k-1}`.
//! Reducing `p = p0 + s_{k-1}·p1` (poly division, deg < 2^{k-1} each), the
//! evaluation of `p` on `shift + W_k` splits into the two `W_{k-1}`-cosets
//! with `s_{k-1}` equal to the constants `s_{k-1}(shift)` and
//! `s_{k-1}(shift) + s_{k-1}(β_{k-1})` respectively — halving the degree each
//! level, hence `O(n log n)`.

use alloc::vec::Vec;
use alloc::vec;

use crate::field::Field;

/// The subspace-vanishing polynomial of `span(beta[0..i])`, as a coefficient
/// vector (lowest-degree first). `s_0(X) = X` (vanishes on `{0}`);
/// `s_i(X) = s_{i-1}(X)·(s_{i-1}(X) − s_{i-1}(β_{i-1}))` (vanishes on
/// `W_{i-1} ∪ (β_{i-1} + W_{i-1}) = W_i`). Linearized: only `X^{2^j}` terms.
fn subspace_vanishing_polys<F: Field>(beta: &[F]) -> Vec<Vec<F>> {
    let k = beta.len();
    let mut s: Vec<Vec<F>> = Vec::with_capacity(k + 1);
    s.push(vec![F::ZERO, F::ONE]); // s_0(X) = X
    for i in 1..=k {
        let prev = &s[i - 1];
        let c = eval_poly(prev, beta[i - 1]); // s_{i-1}(β_{i-1})
        // s_i(X) = prev(X)^2 - c·prev(X)  (char 2: - = +, but written as
        // prev·prev + c·prev since we subtract).
        let sq = poly_mul(prev, prev);
        let cprev = poly_scalar_mul(prev, c);
        s.push(poly_add(&sq, &cprev));
    }
    s
}

/// Evaluate `p` (coeffs, lowest-degree first) at `point` via Horner.
fn eval_poly<F: Field>(p: &[F], point: F) -> F {
    let mut acc = F::ZERO;
    for &c in p.iter().rev() {
        acc = acc.mul(&point).add(&c);
    }
    acc
}

/// Polynomial addition (char 2: coefficientwise XOR/add), lowest-degree first.
fn poly_add<F: Field>(a: &[F], b: &[F]) -> Vec<F> {
    let n = a.len().max(b.len());
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        let x = a.get(i).copied().unwrap_or_default();
        let y = b.get(i).copied().unwrap_or_default();
        out.push(x.add(&y));
    }
    trim(&mut out);
    out
}

/// Polynomial scalar multiplication.
fn poly_scalar_mul<F: Field>(a: &[F], c: F) -> Vec<F> {
    let mut out: Vec<F> = a.iter().map(|x| x.mul(&c)).collect();
    trim(&mut out);
    out
}

/// Polynomial multiplication (naive, lowest-degree first). Used only for the
/// subspace-vanishing polynomials (small — `log n` of them, degree `2^i`).
fn poly_mul<F: Field>(a: &[F], b: &[F]) -> Vec<F> {
    if a.is_empty() || b.is_empty() {
        return Vec::new();
    }
    let mut out = vec![F::ZERO; a.len() + b.len() - 1];
    for (i, &x) in a.iter().enumerate() {
        for (j, &y) in b.iter().enumerate() {
            out[i + j] = out[i + j].add(&x.mul(&y));
        }
    }
    trim(&mut out);
    out
}

/// Trim trailing zeros (keep the canonical coefficient length).
fn trim<F: Field>(p: &mut Vec<F>) {
    while p.len() > 1 && p.last().is_some_and(|x| x.is_zero()) {
        p.pop();
    }
}

/// Divide `p` by the monic linearized `s` (degree `d = 2^i`), returning
/// `(quotient, remainder)` with `deg(remainder) < d`. Naive long division —
/// the vanishing polynomials are linearized (sparse), and this runs `log n`
/// times per level, so the total division cost is `O(n log n)`.
fn poly_divmod<F: Field>(p: &[F], s: &[F]) -> (Vec<F>, Vec<F>) {
    let d = s.len() - 1; // deg(s); s is monic
    let mut rem = p.to_vec();
    trim(&mut rem);
    let mut quot = vec![F::ZERO; rem.len().saturating_sub(d)];
    if rem.len() <= d {
        return (vec![F::ZERO], rem);
    }
    // The vanishing polynomials are LINEARIZED (only `X^{2^j}` terms are
    // nonzero), so iterate only over the nonzero coefficients — this is what
    // makes the per-level division `O(n · log n)` instead of `O(n^2)`.
    let nonzero: Vec<usize> = (0..=d).filter(|&j| !s[j].is_zero()).collect();
    // Long division from the top.
    for k in (0..quot.len()).rev() {
        let lead = rem[d + k];
        if lead.is_zero() {
            continue;
        }
        quot[k] = lead; // s is monic, so the quotient term is the leading coeff
        for &j in &nonzero {
            rem[j + k] = rem[j + k].add(&lead.mul(&s[j]));
        }
    }
    trim(&mut quot);
    trim(&mut rem);
    // rem now has degree < d.
    rem.truncate(d);
    (quot, rem)
}

/// The additive FFT: evaluate `p` (coeffs, degree < `1 << beta.len()`) at
/// `shift + span(beta)`, returning the evaluations in binary-counting order
/// (`shift + Σ b_j β_j` for `b` ranging over `0..2^k`).
fn additive_fft_rec<F: Field>(p: &[F], beta: &[F], s_polys: &[Vec<F>], shift: F) -> Vec<F> {
    let k = beta.len();
    if k == 0 {
        return vec![p.first().copied().unwrap_or_default()];
    }
    let n = 1usize << k;
    // Reduce p = p0 + s_{k-1}·p1 (deg(p0), deg(p1) < 2^{k-1}).
    let s = &s_polys[k - 1];
    let (p1, p0) = poly_divmod(p, s);
    // s_{k-1}(shift): constant on the coset shift + W_{k-1}.
    let s_shift = eval_poly(s, shift);
    // s_{k-1}(shift + β_{k-1}) = s_{k-1}(shift) + s_{k-1}(β_{k-1}).
    let s_shift_beta = s_shift.add(&eval_poly(s, beta[k - 1]));
    // Left coset (shift + W_{k-1}): p = p0 + s_{k-1}(shift)·p1.
    let left_poly = poly_add(&p0, &poly_scalar_mul(&p1, s_shift));
    // Right coset (shift + β_{k-1} + W_{k-1}): p = p0 + s_shift_beta·p1.
    let right_poly = poly_add(&p0, &poly_scalar_mul(&p1, s_shift_beta));
    let mut out = Vec::with_capacity(n);
    out.extend(additive_fft_rec(&left_poly, &beta[..k - 1], s_polys, shift));
    out.extend(additive_fft_rec(
        &right_poly,
        &beta[..k - 1],
        s_polys,
        shift.add(&beta[k - 1]),
    ));
    out
}

/// Evaluate the message polynomial `p(X) = Σ_i p[i]·X^i` at the `n` integer
/// points `0..n` via the additive FFT (`O(n log n)`). `p.len()` and `n` must
/// be powers of two with `p.len() <= n`.
pub fn additive_fft_evaluate<F: Field>(p: &[F], n: usize) -> Vec<F> {
    let s = n.trailing_zeros() as usize;
    assert_eq!(1usize << s, n, "n must be a power of two");
    assert!(p.len() <= n, "message longer than the evaluation domain");
    // Pad p to length n.
    let mut p = p.to_vec();
    p.resize(n, F::ZERO);
    trim(&mut p);
    // The basis: e_j = from_u128(2^j), so span(e_0..e_{s-1}) = {0,1,…,2^s−1}
    // (the integer points, as field elements via from_u128).
    let beta: Vec<F> = (0..s).map(|j| F::from_u128(1u128 << j)).collect();
    let s_polys = subspace_vanishing_polys(&beta);
    additive_fft_rec(&p, &beta, &s_polys, F::ZERO)
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_primitives::Galois128;

    /// The naive Horner reference (the current RS `encode` body).
    fn naive_evaluate<F: Field>(p: &[F], n: usize) -> Vec<F> {
        (0..n)
            .map(|j| {
                let point = F::from_u128(j as u128);
                let mut acc = F::ZERO;
                for i in (0..p.len()).rev() {
                    acc = acc.mul(&point).add(&p[i]);
                }
                acc
            })
            .collect()
    }

    #[test]
    fn additive_fft_matches_naive_horner() {
        for log_n in 1..=6 {
            let n = 1usize << log_n;
            for log_k in 0..=log_n {
                let mlen = 1usize << log_k;
                let msg: Vec<Galois128> = (0..mlen)
                    .map(|i| Galois128(0x9e3779b9u128.wrapping_mul(i as u128 + 1)))
                    .collect();
                let fft = additive_fft_evaluate(&msg, n);
                let naive = naive_evaluate(&msg, n);
                assert_eq!(fft, naive, "log_n={log_n} log_k={log_k} mismatch");
            }
        }
    }

    #[test]
    fn additive_fft_constant_and_linear() {
        // Constant polynomial.
        let msg = vec![Galois128(42)];
        assert_eq!(additive_fft_evaluate(&msg, 8), vec![Galois128(42); 8]);
        // p(X) = X: p(j) = j.
        let msg = vec![Galois128(0), Galois128(1)];
        let out = additive_fft_evaluate(&msg, 8);
        for (j, &v) in out.iter().enumerate() {
            assert_eq!(v, Galois128(j as u128), "p(X)=X at {j}");
        }
    }
}
