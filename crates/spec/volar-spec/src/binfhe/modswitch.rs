// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Modulus switching between power-of-two moduli.
//!
//! `mod_switch<FROM, TO>(x)` computes `round(x * 2^TO / 2^FROM)` modulo
//! `2^TO`. Upscaling (`FROM <= TO`) is exact; downscaling rounds to the
//! nearest representative (ties away from zero, which is immaterial for a
//! rounding error budget that already assumes `|e| <= 1/2` in the target
//! modulus).
//!
//! The pipeline uses it twice: `Q -> modKS` after sample extraction (so key
//! switching noise is generated against a small modulus), and `modKS -> q`
//! after key switching (so the output wire re-enters the `q = 2N` domain).

/// Modulus-switch one value.
#[inline]
pub const fn mod_switch<const FROM: u32, const TO: u32>(x: u32) -> u32 {
    if TO >= FROM {
        // Exact embedding.
        super::torus::embed_up::<FROM, TO>(x)
    } else {
        let shift = FROM - TO;
        let half = 1u32 << (shift - 1);
        // u64 intermediate so the rounding increment cannot wrap at FROM=32.
        let rounded = ((x as u64 + half as u64) >> shift) as u32;
        super::torus::reduce::<TO>(rounded)
    }
}

/// Modulus-switch an LWE ciphertext component-wise.
#[inline]
pub fn mod_switch_lwe<const N: usize, const FROM: u32, const TO: u32>(
    ct: &super::lwe::BinfheLweCiphertext<N>,
) -> super::lwe::BinfheLweCiphertext<N> {
    let mut a = [0u32; N];
    for i in 0..N {
        a[i] = mod_switch::<FROM, TO>(ct.a[i]);
    }
    super::lwe::BinfheLweCiphertext {
        a,
        b: mod_switch::<FROM, TO>(ct.b),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn upscaling_is_exact() {
        assert_eq!(mod_switch::<7, 8>(1), 2);
        assert_eq!(mod_switch::<11, 27>(2047), 2047 << 16);
    }

    #[test]
    fn downscaling_rounds_to_nearest() {
        // 8 -> 7: divide by 2 with rounding.
        assert_eq!(mod_switch::<8, 7>(0), 0);
        assert_eq!(mod_switch::<8, 7>(1), 1); // 0.5 rounds up
        assert_eq!(mod_switch::<8, 7>(2), 1);
        assert_eq!(mod_switch::<8, 7>(3), 2);
        assert_eq!(mod_switch::<8, 7>(255), 0); // rounds to 128, wraps mod 2^7
    }

    #[test]
    fn scaled_values_round_trip_exactly() {
        // A value that is an exact multiple of Q/q survives Q -> q.
        // q = 2^7, Q = 2^8: exact multiples of 2 at Q scale.
        for v in 0..128u32 {
            assert_eq!(mod_switch::<8, 7>(2 * v), v);
        }
        // std128 chain: Q=2^27 -> modKS=2^15 -> q=2^11.
        // Exact multiples of 2^12 at Q scale map to multiples of 1 at
        // modKS scale and stay integral down to q when multiples of 2^16.
        for v in [0u32, 1, 15, 2047] {
            assert_eq!(mod_switch::<27, 11>(v << 16), v);
            assert_eq!(mod_switch::<15, 11>(v << 4), v);
        }
    }

    #[test]
    fn full_width_downscale_does_not_overflow() {
        // FROM = 32 -> TO = 8 divides by 2^24; the u64 rounding increment
        // must not wrap even at the top of the source modulus.
        assert_eq!(mod_switch::<32, 8>(1 << 24), 1);
        assert_eq!(mod_switch::<32, 8>((1 << 24) + (1 << 23)), 2); // 1.5 rounds up
        assert_eq!(mod_switch::<32, 8>(u32::MAX), 0); // rounds to 256, reduces mod 2^8
        assert_eq!(mod_switch::<32, 8>(0xFF00_0000), 0xFF);
    }
}
