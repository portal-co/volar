// @reliability: experimental
//! @ai: assisted
//! ConvertToVOLE — turn a vector of N seeds into one small-field VOLE
//! correlation over GF(2^k) where N = 2^k.
//!
//! FAEST v2 spec §5.4 (Figure 5.6). The divide-and-conquer XOR tree from
//! Proposition 5.2 — for `N = 2^k` seeds, produces:
//!
//! - `u ∈ {0,1}^ℓhat` — XOR of all seed expansions.
//! - `v_0, …, v_{k-1} ∈ {0,1}^ℓhat` — one mask per bit of the verifier's
//!   eventual challenge `Δ_i ∈ [0..N)`.
//!
//! The prover runs this with all seeds present; the verifier runs the same
//! algorithm with one seed nulled to `None`. Proposition 5.2 gives:
//!
//! ```text
//! q_j = v_j ⊕ δ_j · u    for j ∈ [0..k)
//! ```
//!
//! where `(δ_0, …, δ_{k-1})` is the little-endian bit decomposition of the
//! hidden index `Δ_i`. This is the "small VOLE" per FAEST §2.1; τ of them
//! concatenate via [`concat_small_voles`] into the big VOLE over F_{2^λ}.

use alloc::vec;
use alloc::vec::Vec;

use super::prg::aes_ctr_prg;

/// λ in bytes for FAEST-128. Re-declared here rather than imported from
/// `bavc` to keep this module independent of the BAVC layout.
const LAMBDA_BYTES: usize = 16;

/// Output of [`convert_to_vole`].
///
/// `u` is the XOR of all expanded seeds. `v` has `log2(N)` entries; entry
/// `j` corresponds to the `j`-th bit of the eventual challenge index.
/// Each `v[j]` and `u` is `l_hat_bytes` bytes long.
pub struct ConvertOutput {
    pub u: Vec<u8>,
    pub v: Vec<Vec<u8>>,
}

/// FAEST §5.4 ConvertToVOLE.
///
/// `seeds[i] = None` indicates the verifier-side "missing seed" (the
/// hidden leaf). All present seeds are expanded via [`aes_ctr_prg`] under
/// the given `iv`/`tweak`. `seeds.len()` must be a power of two.
///
/// # Panics
///
/// - If `seeds.len()` is not a power of two or is zero.
/// - If `seeds.len() == 1` (no challenge bits to commit to).
pub fn convert_to_vole(
    seeds: &[Option<[u8; LAMBDA_BYTES]>],
    iv: &[u8; 16],
    tweak: u32,
    l_hat_bytes: usize,
) -> ConvertOutput {
    let n = seeds.len();
    assert!(n.is_power_of_two() && n >= 2, "ConvertToVOLE: N must be power of 2, ≥ 2");
    let d = (n.trailing_zeros()) as usize; // log2(N)

    // r[0..N]: the expanded seeds. Missing seed → zero vector.
    let zero_block = vec![0u8; l_hat_bytes];
    let mut r: Vec<Vec<u8>> = Vec::with_capacity(n);
    for s in seeds {
        match s {
            Some(seed) => r.push(aes_ctr_prg(seed, iv, tweak, l_hat_bytes)),
            None => r.push(zero_block.clone()),
        }
    }

    // v_0 = ... = v_{d-1} = 0.
    let mut v: Vec<Vec<u8>> = (0..d).map(|_| vec![0u8; l_hat_bytes]).collect();

    // Divide-and-conquer XOR pass.
    // At level j, half the buffer is updated in place and v[j] absorbs
    // every odd-indexed input from that level. After level d-1 there is
    // a single entry left in `r`, which is `u`.
    let mut level: Vec<Vec<u8>> = r;
    for j in 0..d {
        let half = level.len() / 2;
        let mut next: Vec<Vec<u8>> = Vec::with_capacity(half);
        for i in 0..half {
            // v_j ⊕= r_{j, 2i+1}
            xor_in_place(&mut v[j], &level[2 * i + 1]);
            // r_{j+1, i} = r_{j, 2i} ⊕ r_{j, 2i+1}
            let mut new_entry = level[2 * i].clone();
            xor_in_place(&mut new_entry, &level[2 * i + 1]);
            next.push(new_entry);
        }
        level = next;
    }
    debug_assert_eq!(level.len(), 1);
    let u = level.into_iter().next().unwrap();

    ConvertOutput { u, v }
}

/// Output of [`concat_small_voles`].
///
/// `u` is the common witness mask (= u_0 from the first small VOLE).
/// `c[i]` for `i ∈ [1..τ)` is the correction `u_i ⊕ u_0` that the
/// verifier applies to align the i-th small VOLE to the common `u`.
/// `v_columns[bit]` is the `bit`-th column of the big-VOLE V matrix —
/// concatenation of all small-VOLE `v_j`s across `i`. Index range is
/// `[0..λ)` with `λ = Σ_i k_i` where `k_i = log2(N_i)`.
pub struct BigVoleProver {
    pub u: Vec<u8>,
    pub c: Vec<Vec<u8>>,
    pub v_columns: Vec<Vec<u8>>,
}

/// FAEST §5.5 small→big VOLE concatenation (prover side).
///
/// Given τ [`ConvertOutput`]s (one per BAVC vector), the first sets the
/// common `u`; subsequent ones produce correction vectors `c_i = u_i ⊕ u_0`
/// and contribute their `v_j` columns to the big-VOLE matrix.
///
/// # Panics
///
/// - If `outs.is_empty()`.
/// - If any pair of `outs` disagrees on `l_hat_bytes` (the `u.len()` of
///   the first entry is taken as ground truth).
pub fn concat_small_voles(outs: Vec<ConvertOutput>) -> BigVoleProver {
    assert!(!outs.is_empty(), "concat_small_voles: need at least one VOLE");
    let l_hat = outs[0].u.len();
    for o in &outs {
        assert_eq!(o.u.len(), l_hat, "small VOLEs must agree on l_hat_bytes");
        for vj in &o.v {
            assert_eq!(vj.len(), l_hat, "small VOLE v_j must agree on l_hat_bytes");
        }
    }

    let u = outs[0].u.clone();

    // Correction vectors c_i = u_i ⊕ u_0 for i ∈ [1..τ).
    let mut c: Vec<Vec<u8>> = Vec::with_capacity(outs.len() - 1);
    for o in outs.iter().skip(1) {
        let mut ci = o.u.clone();
        xor_in_place(&mut ci, &u);
        c.push(ci);
    }

    // V columns: concatenate all v_j from each small VOLE in order.
    let mut v_columns: Vec<Vec<u8>> = Vec::new();
    for o in outs {
        for vj in o.v {
            v_columns.push(vj);
        }
    }

    BigVoleProver { u, c, v_columns }
}

/// Output of [`concat_small_voles_verifier`].
///
/// `q_columns[bit]` is the `bit`-th column of the big-VOLE Q matrix the
/// verifier has assembled from its τ small-VOLE outputs (one of which has
/// a hidden seed). Index range is `[0..λ)`.
pub struct BigVoleVerifier {
    pub q_columns: Vec<Vec<u8>>,
}

/// FAEST §5.5 small→big VOLE concatenation (verifier side).
///
/// The verifier ran [`convert_to_vole`] on each of its τ partial seed
/// vectors, getting `(u'_i, v'_{i,0}, …, v'_{i,k-1})`. It then applies
/// the per-bit correction `q_{i,j} = v'_{i,j} ⊕ δ_{i,j} · c_i` (where
/// `c_0 = 0` because the first VOLE defines the baseline `u_0`) and
/// concatenates the resulting q-columns.
///
/// `outs[i]` is the verifier's `convert_to_vole` output for vector i.
/// `deltas[i]` is the hidden challenge index for vector i.
/// `corrections[i-1]` is `c_i` from the prover for i ∈ [1..τ).
///
/// # Panics
///
/// - If `outs.is_empty()`.
/// - If `deltas.len() != outs.len()` or `corrections.len() != outs.len()-1`.
pub fn concat_small_voles_verifier(
    outs: Vec<ConvertOutput>,
    deltas: &[usize],
    corrections: &[Vec<u8>],
) -> BigVoleVerifier {
    assert!(!outs.is_empty());
    assert_eq!(deltas.len(), outs.len());
    assert_eq!(corrections.len(), outs.len() - 1);

    let mut q_columns: Vec<Vec<u8>> = Vec::new();
    for (i, o) in outs.into_iter().enumerate() {
        let k = o.v.len();
        let delta_i = deltas[i];
        for (bit, vj_raw) in o.v.into_iter().enumerate() {
            // Verifier's raw v_j from convert_to_vole. The verifier knows
            // its own u'_i = 0 (because seed was missing). For i ≥ 1, it
            // also needs to fold in the correction c_i scaled by δ_{i,bit}.
            let mut q = vj_raw;
            if i >= 1 {
                let delta_bit = (delta_i >> bit) & 1 == 1;
                if delta_bit {
                    xor_in_place(&mut q, &corrections[i - 1]);
                }
            }
            q_columns.push(q);
        }
        let _ = k;
    }
    BigVoleVerifier { q_columns }
}

/// Byte-wise XOR: `a ⊕= b`. Panics if lengths differ (caller's bug).
fn xor_in_place(a: &mut [u8], b: &[u8]) {
    assert_eq!(a.len(), b.len(), "xor_in_place: length mismatch");
    for i in 0..a.len() {
        a[i] ^= b[i];
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    /// Generate `n` random-looking seeds deterministically from a base.
    fn make_seeds(base: u8, n: usize) -> Vec<[u8; LAMBDA_BYTES]> {
        (0..n)
            .map(|i| {
                let mut s = [base; LAMBDA_BYTES];
                s[0] = i as u8;
                s[1] = (i >> 8) as u8;
                s
            })
            .collect()
    }

    fn xor(a: &[u8], b: &[u8]) -> Vec<u8> {
        a.iter().zip(b.iter()).map(|(x, y)| x ^ y).collect()
    }

    /// Proposition 5.2: with the verifier feeding permuted seeds
    /// `sd'_i = sd_{i ⊕ Δ}` (and `sd'_0 = ⊥`), the prover's and verifier's
    /// `convert_to_vole` outputs are related by `q_j = v_j ⊕ δ_j · u`.
    ///
    /// The XOR permutation is what makes the "the missing seed always
    /// lives at slot 0 in the verifier's view" framing work — see the
    /// spec proof in §5.4. Without the permutation, the two algorithms
    /// produce structurally unrelated divide-and-conquer trees.
    #[test]
    fn proposition_5_2_holds() {
        let n = 8usize;
        let l_hat_bytes = 32;
        let iv = [0u8; 16];
        let tweak = 0;
        let seeds = make_seeds(0xa5, n);

        // Try every possible hidden index Δ ∈ [0..N).
        for delta in 0..n {
            let prover_seeds: Vec<Option<[u8; LAMBDA_BYTES]>> =
                seeds.iter().map(|s| Some(*s)).collect();
            // Build verifier seeds: position 0 → None (the hidden seed,
            // which would have been seeds[delta]); position i > 0 →
            // seeds[i ⊕ Δ].
            let verifier_seeds: Vec<Option<[u8; LAMBDA_BYTES]>> = (0..n)
                .map(|i| if i == 0 { None } else { Some(seeds[i ^ delta]) })
                .collect();

            let p = convert_to_vole(&prover_seeds, &iv, tweak, l_hat_bytes);
            let v = convert_to_vole(&verifier_seeds, &iv, tweak, l_hat_bytes);

            let d = n.trailing_zeros() as usize;
            assert_eq!(p.v.len(), d);
            assert_eq!(v.v.len(), d);

            for j in 0..d {
                let delta_j = (delta >> j) & 1 == 1;
                let expected = if delta_j { xor(&p.v[j], &p.u) } else { p.v[j].clone() };
                assert_eq!(
                    v.v[j], expected,
                    "Proposition 5.2 mismatch at Δ={}, j={}",
                    delta, j
                );
            }
        }
    }

    /// τ=2 small VOLEs concatenate to a coherent big VOLE.
    ///
    /// After correction, both small VOLEs share u = u_0. Verifier-side
    /// q_columns satisfy q[bit] = V[bit] ⊕ u · Δ[bit] where Δ packs
    /// (δ_{0,0..k}, δ_{1,0..k}).
    #[test]
    fn big_vole_concat_round_trip() {
        let tau = 2usize;
        let n: usize = 4;
        let k_per = n.trailing_zeros() as usize; // 2
        let l_hat_bytes = 32;
        let iv = [0u8; 16];

        let seeds0 = make_seeds(0x10, n);
        let seeds1 = make_seeds(0x20, n);

        let delta0 = 1usize;
        let delta1 = 3usize;

        // Prover side: all seeds.
        let p0 = convert_to_vole(
            &seeds0.iter().map(|s| Some(*s)).collect::<Vec<_>>(),
            &iv,
            0,
            l_hat_bytes,
        );
        let p1 = convert_to_vole(
            &seeds1.iter().map(|s| Some(*s)).collect::<Vec<_>>(),
            &iv,
            1, // distinct tweak per vector
            l_hat_bytes,
        );
        let big_p = concat_small_voles(vec![p0, p1]);
        assert_eq!(big_p.c.len(), tau - 1);
        assert_eq!(big_p.v_columns.len(), tau * k_per);

        // Verifier side: per Proposition 5.2, slot 0 = None and slot i > 0
        // is the prover's seed at index `i ⊕ Δ`.
        let vs0: Vec<Option<[u8; 16]>> = (0..n)
            .map(|i| if i == 0 { None } else { Some(seeds0[i ^ delta0]) })
            .collect();
        let vs1: Vec<Option<[u8; 16]>> = (0..n)
            .map(|i| if i == 0 { None } else { Some(seeds1[i ^ delta1]) })
            .collect();
        let v0 = convert_to_vole(&vs0, &iv, 0, l_hat_bytes);
        let v1 = convert_to_vole(&vs1, &iv, 1, l_hat_bytes);
        let big_v = concat_small_voles_verifier(
            vec![v0, v1],
            &[delta0, delta1],
            &big_p.c,
        );
        assert_eq!(big_v.q_columns.len(), tau * k_per);

        // Big-VOLE relation: for each bit position β ∈ [0..λ_eff),
        //   q[β] = V[β] ⊕ Δ_bit_β · u
        // where Δ_bit_β = bit β of the packed Δ = (δ_{0,0}, δ_{0,1}, δ_{1,0}, δ_{1,1}).
        let packed_delta: Vec<bool> = (0..tau)
            .flat_map(|i| {
                let d = if i == 0 { delta0 } else { delta1 };
                (0..k_per).map(move |j| (d >> j) & 1 == 1)
            })
            .collect();

        for (beta, (q, v)) in big_v
            .q_columns
            .iter()
            .zip(big_p.v_columns.iter())
            .enumerate()
        {
            let expected = if packed_delta[beta] {
                xor(v, &big_p.u)
            } else {
                v.clone()
            };
            assert_eq!(
                q, &expected,
                "big-VOLE relation broken at β={} (packed Δ-bit = {})",
                beta, packed_delta[beta]
            );
        }
    }

    /// N=2 (k=1) edge case: degenerate but valid.
    #[test]
    fn convert_to_vole_n2() {
        let l_hat_bytes = 16;
        let iv = [0u8; 16];
        let seeds = make_seeds(0x42, 2);
        let prover: Vec<Option<_>> = seeds.iter().map(|s| Some(*s)).collect();

        let p = convert_to_vole(&prover, &iv, 0, l_hat_bytes);
        assert_eq!(p.v.len(), 1, "k = log2(2) = 1");
        assert_eq!(p.u.len(), l_hat_bytes);
        // u = r_0 ⊕ r_1, v_0 = r_1
        let r0 = aes_ctr_prg(&seeds[0], &iv, 0, l_hat_bytes);
        let r1 = aes_ctr_prg(&seeds[1], &iv, 0, l_hat_bytes);
        assert_eq!(p.u, xor(&r0, &r1));
        assert_eq!(p.v[0], r1);
    }

    /// l_hat_bytes can be arbitrary; XOR is byte-aligned.
    #[test]
    fn convert_to_vole_arbitrary_l_hat() {
        let n = 4;
        let iv = [0u8; 16];
        let seeds = make_seeds(0, n);
        let prover: Vec<Option<_>> = seeds.iter().map(|s| Some(*s)).collect();
        for &l_hat_bytes in &[1, 7, 15, 16, 17, 100, 210] {
            let p = convert_to_vole(&prover, &iv, 0, l_hat_bytes);
            assert_eq!(p.u.len(), l_hat_bytes);
            for vj in &p.v {
                assert_eq!(vj.len(), l_hat_bytes);
            }
        }
    }

    /// Concat with τ=1: no corrections, v_columns equal the single small
    /// VOLE's v.
    #[test]
    fn big_vole_concat_tau_1() {
        let l_hat_bytes = 16;
        let iv = [0u8; 16];
        let seeds = make_seeds(0, 4);
        let prover: Vec<Option<_>> = seeds.iter().map(|s| Some(*s)).collect();
        let p = convert_to_vole(&prover, &iv, 0, l_hat_bytes);
        let v_expected = p.v.clone();
        let u_expected = p.u.clone();
        let big = concat_small_voles(vec![p]);
        assert_eq!(big.c.len(), 0);
        assert_eq!(big.u, u_expected);
        assert_eq!(big.v_columns, v_expected);
    }
}
