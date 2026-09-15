// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Power-of-two modular arithmetic over `u32`.
//!
//! All `binfhe` moduli are powers of two at most `2^32`, so a modulus is
//! represented by its log2 (`LOG`) and reduction is a mask. Values are
//! stored **reduced**: every public constructor/operation in `binfhe`
//! reduces before storing, so arithmetic here may assume `x < 2^LOG` and
//! must restore that invariant.

/// Bit mask of the modulus `2^LOG`: `2^LOG - 1`, saturating at `u32::MAX`.
#[inline]
pub const fn mask<const LOG: u32>() -> u32 {
    if LOG >= 32 { u32::MAX } else { (1u32 << LOG) - 1 }
}

/// Reduce `x` modulo `2^LOG`.
#[inline]
pub const fn reduce<const LOG: u32>(x: u32) -> u32 {
    x & mask::<LOG>()
}

/// `(a + b) mod 2^LOG`.
#[inline]
pub const fn add<const LOG: u32>(a: u32, b: u32) -> u32 {
    reduce::<LOG>(a.wrapping_add(b))
}

/// `(a - b) mod 2^LOG`.
#[inline]
pub const fn sub<const LOG: u32>(a: u32, b: u32) -> u32 {
    reduce::<LOG>(a.wrapping_sub(b))
}

/// `-a mod 2^LOG`.
#[inline]
pub const fn neg<const LOG: u32>(a: u32) -> u32 {
    reduce::<LOG>(a.wrapping_neg())
}

/// `c * a mod 2^LOG` for an exact (small, non-modular) integer `c`.
///
/// Ciphertext-linear scaling: `c` is a cleartext exact integer (e.g. a
/// multi-input selector weight `2^j`), never a torus element.
#[inline]
pub const fn mul_exact<const LOG: u32>(a: u32, c: u32) -> u32 {
    reduce::<LOG>(a.wrapping_mul(c))
}

/// Exact embedding `Z_{2^FROM} -> Z_{2^TO}` (`FROM <= TO`): multiply by
/// `2^(TO - FROM)`. Used to embed LWE-domain values into the accumulator
/// domain; exact because both moduli are powers of two.
#[inline]
pub const fn embed_up<const FROM: u32, const TO: u32>(x: u32) -> u32 {
    debug_assert!(FROM <= TO);
    reduce::<TO>(x << (TO - FROM))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mask_covers_full_width_at_32() {
        assert_eq!(mask::<32>(), u32::MAX);
        assert_eq!(mask::<8>(), 0xFF);
        assert_eq!(reduce::<8>(0x1FF), 0xFF);
    }

    #[test]
    fn arithmetic_reduces_and_wraps_exactly() {
        assert_eq!(add::<8>(200, 100), 44);
        assert_eq!(sub::<8>(10, 20), 246);
        assert_eq!(neg::<8>(1), 255);
        assert_eq!(neg::<8>(0), 0);
        assert_eq!(mul_exact::<8>(100, 4), 144); // 400 mod 256
        // Full-width path.
        assert_eq!(add::<32>(u32::MAX, 1), 0);
        assert_eq!(sub::<32>(0, 1), u32::MAX);
    }

    #[test]
    fn embed_up_is_exact_scaling() {
        assert_eq!(embed_up::<7, 8>(1), 2);
        assert_eq!(embed_up::<11, 27>(1), 1 << 16);
        // Values that overflow the target modulus reduce.
        assert_eq!(embed_up::<7, 8>(200), 400 - 256);
    }
}
