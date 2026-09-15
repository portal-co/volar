// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Deterministic integer noise sampling.
//!
//! Noise is a centered binomial distribution (CBD): the difference of two
//! popcounts of `ETA`-bit fresh random strings. This is the FIPS 203
//! §4.2.2 sampling mechanic — integer-only, constant-shape, and fully
//! determined by the caller's [`SpecRng`]. The variance is exactly
//! `ETA/2`, so `ETA = 16` gives `sigma = sqrt(8) ~= 2.828` (the `std128`
//! profile's recorded deviation from OpenFHE's `sigma = 3.19`), and
//! `ETA = 0` disables noise entirely (the `toy` profile).
//!
//! The sampler consumes whole `next_u32` words from the RNG and uses each
//! word for at most one sample, so the consumption pattern is simple to
//! audit and to reproduce across implementations.

use crate::SpecRng;

/// Sample a centered binomial value in `[-ETA, ETA]` with variance `ETA/2`.
///
/// Returns 0 when `ETA = 0`. The result is a signed integer; reduce it into
/// a modulus with [`super::torus::reduce`] after widening as needed.
pub fn cbd<const ETA: u32, R: SpecRng>(rng: &mut R) -> i32 {
    if ETA == 0 {
        return 0;
    }
    debug_assert!(ETA <= 32, "one RNG word supplies at most 32 bits per half");
    let mask = if ETA >= 32 { u32::MAX } else { (1u32 << ETA) - 1 };
    let a = (rng.next_u32() & mask).count_ones() as i32;
    let b = (rng.next_u32() & mask).count_ones() as i32;
    a - b
}

/// Sample a centered binomial error term reduced modulo `2^LOG`.
#[inline]
pub fn sample_error<const LOG: u32, const ETA: u32, R: SpecRng>(rng: &mut R) -> u32 {
    super::torus::reduce::<LOG>(cbd::<ETA, R>(rng) as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Splitmix64-backed RNG (same construction as the legacy spec tests).
    struct TestRng(u64);

    impl TestRng {
        fn new(seed: u64) -> Self {
            Self(seed)
        }
    }

    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
            z = z ^ (z >> 31);
            z as u32
        }
    }

    #[test]
    fn cbd_zero_eta_is_exactly_zero() {
        let mut rng = TestRng::new(0xC0FFEE);
        for _ in 0..1024 {
            assert_eq!(cbd::<0, _>(&mut rng), 0);
            assert_eq!(sample_error::<8, 0, _>(&mut rng), 0);
        }
    }

    #[test]
    fn cbd_is_deterministic_per_seed() {
        let mut a = TestRng::new(42);
        let mut b = TestRng::new(42);
        let mut c = TestRng::new(43);
        let mut same = true;
        let mut differ = false;
        for _ in 0..256 {
            let (x, y, z) = (cbd::<16, _>(&mut a), cbd::<16, _>(&mut b), cbd::<16, _>(&mut c));
            same &= x == y;
            differ |= x != z;
        }
        assert!(same, "same seed must give identical streams");
        assert!(differ, "different seed must diverge");
    }

    #[test]
    fn cbd_supports_and_moments_match_eta() {
        // Support: |sample| <= ETA always.
        let mut rng = TestRng::new(7);
        for _ in 0..4096 {
            let v = cbd::<16, _>(&mut rng);
            assert!((-16..=16).contains(&v), "support violation: {v}");
        }
        // Moments over a fixed seeded corpus: mean ~ 0, variance ~ ETA/2.
        // Deterministic seeds make this an exact regression fixture, not a
        // statistical flake: the ranges below are wide engineering
        // tolerances around the theoretical values.
        let mut rng = TestRng::new(0x5EED);
        let n = 20_000i64;
        let mut sum = 0i64;
        let mut sum_sq = 0i64;
        for _ in 0..n {
            let v = cbd::<16, _>(&mut rng) as i64;
            sum += v;
            sum_sq += v * v;
        }
        let mean_x1000 = (sum * 1000) / n;
        let var_x100 = (sum_sq * 100) / n; // mean is ~0
        assert!(
            (-200..=200).contains(&mean_x1000),
            "mean/1000 = {mean_x1000}, expected ~0"
        );
        // ETA/2 = 8 -> var_x100 ~ 800.
        assert!(
            (720..=880).contains(&var_x100),
            "variance/100 = {var_x100}, expected ~800 (ETA/2 = 8)"
        );
    }

    #[test]
    fn sample_error_reduces_into_modulus() {
        let mut rng = TestRng::new(99);
        for _ in 0..1024 {
            let e = sample_error::<8, 16, _>(&mut rng);
            // Reduced representative: small positive or near-2^8 negative.
            assert!(e <= 16 || e >= 256 - 16, "unexpected reduced error {e}");
        }
    }
}
