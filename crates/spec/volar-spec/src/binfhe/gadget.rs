// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Exact covering gadget decomposition over power-of-two moduli.
//!
//! For modulus `2^LOG`, base `2^BASE_LOG`, and `ELL = ceil(LOG / BASE_LOG)`
//! levels (a profile invariant), level `j` extracts the digit occupying
//! bits `[shift_j, shift_j + bits_j)` where
//!
//! ```text
//! shift_j = max(LOG - BASE_LOG * (j + 1), 0)
//! bits_j  = min(BASE_LOG, LOG - BASE_LOG * j)
//! ```
//!
//! Because the levels exactly cover the `LOG` bits, the decomposition is
//! **exact**: `x = sum_j digit_j << shift_j` for every `x < 2^LOG`, with no
//! rounding term. (Real-world TFHE parameter sets sometimes truncate the
//! lowest level for noise control; the `binfhe` profiles choose covering
//! decompositions so decomposition error is identically zero, simplifying
//! the failure analysis to RGSW/KSK noise only.)

use crate::binfhe::torus;

/// First bit position of level `j`'s digit.
pub const fn level_shift(log: u32, base_log: u32, j: usize) -> u32 {
    log.saturating_sub(base_log * (j as u32 + 1))
}

/// Number of bits in level `j`'s digit (the last level may be narrower).
pub const fn level_bits(log: u32, base_log: u32, j: usize) -> u32 {
    let remaining = log.saturating_sub(base_log * j as u32);
    if remaining < base_log { remaining } else { base_log }
}

/// Gadget factor of level `j`: `2^shift_j` as an element of `Z_{2^LOG}`.
pub const fn level_factor<const LOG: u32>(base_log: u32, j: usize) -> u32 {
    torus::reduce::<LOG>(1u32 << level_shift(LOG, base_log, j))
}

/// Decompose `x` (already reduced mod `2^LOG`) into `ELL` digits.
///
/// Returns digits ordered most-significant first (level 0 = top bits).
/// Exact: `sum_j digits[j] << shift_j == x` whenever
/// `ELL * BASE_LOG >= LOG`.
pub fn decompose<const LOG: u32, const ELL: usize, const BASE_LOG: u32>(x: u32) -> [u32; ELL] {
    debug_assert!(
        ELL as u32 * BASE_LOG >= LOG,
        "gadget decomposition must cover the modulus"
    );
    let mut digits = [0u32; ELL];
    for (j, d) in digits.iter_mut().enumerate() {
        let shift = level_shift(LOG, BASE_LOG, j);
        let bits = level_bits(LOG, BASE_LOG, j);
        let m = if bits >= 32 { u32::MAX } else { (1u32 << bits) - 1 };
        *d = (x >> shift) & m;
    }
    digits
}

/// Coefficient-wise decomposition of a polynomial.
pub fn poly_decompose<const N: usize, const LOG: u32, const ELL: usize, const BASE_LOG: u32>(
    p: &[u32; N],
) -> [[u32; N]; ELL] {
    let mut out = [[0u32; N]; ELL];
    for i in 0..N {
        let digits = decompose::<LOG, ELL, BASE_LOG>(p[i]);
        for j in 0..ELL {
            out[j][i] = digits[j];
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn level_layout_covers_the_modulus() {
        // std128 bootstrapping decomposition: LOG=27, base 2^7, 4 levels.
        assert_eq!(level_shift(27, 7, 0), 20);
        assert_eq!(level_shift(27, 7, 3), 0);
        assert_eq!(level_bits(27, 7, 0), 7);
        assert_eq!(level_bits(27, 7, 3), 6); // last level narrower
        // toy: LOG=8, base 2^4, 2 levels.
        assert_eq!(level_shift(8, 4, 0), 4);
        assert_eq!(level_shift(8, 4, 1), 0);
        assert_eq!(level_bits(8, 4, 1), 4);
    }

    #[test]
    fn decomposition_reconstructs_exactly() {
        // Exhaustive on the toy 8-bit modulus, sampled on larger ones.
        for x in 0u32..=255 {
            let d = decompose::<8, 2, 4>(x);
            assert_eq!(d[0] * 16 + d[1], x);
        }
        // std128 shape: 4 levels of 7 bits over 27 bits.
        for &x in &[0u32, 1, 127, 128, 65_535, (1 << 26) + 12345, (1 << 27) - 1] {
            let d = decompose::<27, 4, 7>(x);
            let recon = (d[0] << 20) + (d[1] << 13) + (d[2] << 6) + d[3];
            assert_eq!(recon, x, "exact reconstruction of {x}");
            assert!(d[3] < 64, "last level holds 6 bits");
        }
        // key-switching shape: LOG_MOD_KS=15, base 2^5, 3 levels.
        for &x in &[0u32, 31, 32, 1023, (1 << 15) - 1] {
            let d = decompose::<15, 3, 5>(x);
            let recon = (d[0] << 10) + (d[1] << 5) + d[2];
            assert_eq!(recon, x);
        }
    }

    #[test]
    fn level_factor_matches_shift() {
        assert_eq!(level_factor::<8>(4, 0), 16);
        assert_eq!(level_factor::<8>(4, 1), 1);
        assert_eq!(level_factor::<27>(7, 0), 1 << 20);
        assert_eq!(level_factor::<27>(7, 3), 1);
    }
}
