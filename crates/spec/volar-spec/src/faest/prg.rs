// @reliability: experimental
//! @ai: assisted
//! FAEST AES-CTR PRG (FAEST v2 spec §3.3).
//!
//! ## Two surfaces
//!
//! [`AesCtrLengthDoubler`] is the textbook GGM length-doubler: 16-byte seed
//! in, 32 bytes (= two children) out. It implements
//! [`crate::byte_gen::LengthDoubler`] so existing tree machinery
//! ([`crate::byte_gen::gen_abo`], etc.) can drive it directly. Tweak/IV are
//! fixed to zero in this surface — the GGM tree position is taken care of by
//! the seed propagation, which is sufficient for any GGM-style construction
//! that doesn't need cross-tree domain separation.
//!
//! [`aes_ctr_prg`] is the explicit-IV/tweak surface that FAEST's BAVC code
//! consumes. It matches the spec §3.3 algorithm literally:
//!
//! ```text
//! k  := AES.KeyExpansion(sd)
//! iv' := AddToUpperWord(iv, tweak)   (32-bit add into upper word)
//! for i in 0..ceil(m / 128):
//!     out[i*16..(i+1)*16] := AES.Encrypt(k, AddToLowerWord(iv', i))
//! ```
//!
//! The two surfaces are deliberately distinct: the `LengthDoubler` impl is
//! the reusable PRG (`base.rs`-style — any GGM consumer can use it), while
//! [`aes_ctr_prg`] is the FAEST-specific entry point that the BAVC consumer
//! needs for per-leaf domain separation via tweak.

use hybrid_array::{Array, sizes::U16};

use super::aes::{encrypt_block, BLOCK};
use crate::byte_gen::LengthDoubler;

/// AES-CTR-based length doubler.
///
/// `double(sd)` returns the AES encryption of counters 0 and 1 under key
/// `sd`. The IV is implicitly all-zeroes; the counters occupy the low 4
/// bytes of the AES input block (little-endian, matching FAEST §3.3
/// `AddToLowerWord`).
pub struct AesCtrLengthDoubler;

impl LengthDoubler for AesCtrLengthDoubler {
    type OutputSize = U16;

    fn double(a: Array<u8, U16>) -> [Array<u8, U16>; 2] {
        // Counter 0: lower 4 bytes are zero, so the AES input is the
        // all-zero block.
        let mut block0 = [0u8; BLOCK];
        // Counter 1: lower 4 bytes = 1 (little-endian).
        let mut block1 = [0u8; BLOCK];
        block1[0] = 1;

        let key: [u8; BLOCK] = a.0;
        let c0 = encrypt_block(&key, &block0);
        let c1 = encrypt_block(&key, &block1);

        // Defensive zeroize of stack scratch — spec-level, no actual
        // secret-protection guarantees but matches the rest of the crate.
        block0.fill(0);
        block1.fill(0);

        [Array(c0), Array(c1)]
    }
}

/// Add `tweak` (32-bit unsigned) into the upper word of a 128-bit IV
/// (little-endian: upper word = bytes 12..16).
///
/// Wrap on overflow — matches FAEST §3.3 `AddToUpperWord`.
#[inline]
fn add_to_upper_word(iv: &mut [u8; BLOCK], tweak: u32) {
    let upper = u32::from_le_bytes([iv[12], iv[13], iv[14], iv[15]]);
    let new = upper.wrapping_add(tweak);
    let bytes = new.to_le_bytes();
    iv[12] = bytes[0];
    iv[13] = bytes[1];
    iv[14] = bytes[2];
    iv[15] = bytes[3];
}

/// Add `counter` (32-bit unsigned) into the lower word of a 128-bit IV
/// (little-endian: lower word = bytes 0..4).
///
/// Wrap on overflow — matches FAEST §3.3 `AddToLowerWord`. Returns a new
/// block rather than mutating in place because each PRG counter step
/// starts from the same `iv'` baseline.
#[inline]
fn add_to_lower_word(iv: &[u8; BLOCK], counter: u32) -> [u8; BLOCK] {
    let mut out = *iv;
    let lower = u32::from_le_bytes([out[0], out[1], out[2], out[3]]);
    let new = lower.wrapping_add(counter);
    let bytes = new.to_le_bytes();
    out[0] = bytes[0];
    out[1] = bytes[1];
    out[2] = bytes[2];
    out[3] = bytes[3];
    out
}

/// FAEST §3.3 PRG: produce `out_bytes` bytes of pseudorandom output from
/// (seed, iv, tweak).
///
/// `out_bytes` may be any non-negative value; the last block is truncated
/// if `out_bytes` is not a multiple of 16. The seed is the AES-128 key,
/// the iv is the 128-bit counter base, and `tweak` is added into the upper
/// 32 bits of the iv before the counter starts incrementing the lower 32
/// bits. This separates two PRG calls with the same `(seed, iv)` but
/// distinct tweaks — used by FAEST to domain-separate per-node BAVC PRG
/// expansions.
///
/// Returns a `Vec<u8>` of length `out_bytes`. The caller is responsible
/// for converting to `Array<u8, _>` if a typenum length is needed.
pub fn aes_ctr_prg(
    seed: &[u8; BLOCK],
    iv: &[u8; BLOCK],
    tweak: u32,
    out_bytes: usize,
) -> alloc::vec::Vec<u8> {
    // Spec line: iv' := AddToUpperWord(iv, tweak).
    let mut iv_tweaked = *iv;
    add_to_upper_word(&mut iv_tweaked, tweak);

    let n_full = out_bytes / BLOCK;
    let rem = out_bytes % BLOCK;

    let mut out = alloc::vec::Vec::with_capacity(out_bytes);
    for i in 0..n_full {
        let block_in = add_to_lower_word(&iv_tweaked, i as u32);
        let ct = encrypt_block(seed, &block_in);
        out.extend_from_slice(&ct);
    }
    if rem > 0 {
        let block_in = add_to_lower_word(&iv_tweaked, n_full as u32);
        let ct = encrypt_block(seed, &block_in);
        out.extend_from_slice(&ct[..rem]);
    }
    out
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    /// `AesCtrLengthDoubler::double(0)` matches AES_0(0) || AES_0(counter=1).
    /// Pins the trait impl to the algorithm spec.
    #[test]
    fn length_doubler_matches_aes_ctr() {
        let seed = Array::<u8, U16>([0u8; 16]);
        let [c0, c1] = AesCtrLengthDoubler::double(seed);
        let want_c0 = encrypt_block(&[0u8; 16], &[0u8; 16]);
        let mut block1 = [0u8; 16];
        block1[0] = 1;
        let want_c1 = encrypt_block(&[0u8; 16], &block1);
        assert_eq!(c0.0, want_c0);
        assert_eq!(c1.0, want_c1);
    }

    /// Two distinct seeds produce distinct children (overwhelming
    /// probability — this is just a smoke test on the AES PRP property,
    /// not a real statistical test).
    #[test]
    fn length_doubler_distinct_seeds_produce_distinct_children() {
        let seed_a = Array::<u8, U16>([0u8; 16]);
        let mut seed_b_bytes = [0u8; 16];
        seed_b_bytes[0] = 1;
        let seed_b = Array::<u8, U16>(seed_b_bytes);
        let [a0, a1] = AesCtrLengthDoubler::double(seed_a);
        let [b0, b1] = AesCtrLengthDoubler::double(seed_b);
        assert_ne!(a0.0, b0.0);
        assert_ne!(a1.0, b1.0);
    }

    /// `aes_ctr_prg` with tweak = 0 and iv = 0 reproduces
    /// `AesCtrLengthDoubler::double(seed)` for the first 32 bytes.
    /// Confirms the two surfaces line up at the trivial tweak.
    #[test]
    fn aes_ctr_prg_aligns_with_length_doubler_at_zero_tweak() {
        let seed = [0u8; 16];
        let iv = [0u8; 16];
        let prg_out = aes_ctr_prg(&seed, &iv, 0, 32);

        let seed_arr = Array::<u8, U16>(seed);
        let [c0, c1] = AesCtrLengthDoubler::double(seed_arr);

        assert_eq!(&prg_out[..16], &c0.0[..]);
        assert_eq!(&prg_out[16..32], &c1.0[..]);
    }

    /// Output length is exactly what was requested, even when not a
    /// multiple of the block size.
    #[test]
    fn aes_ctr_prg_truncates_correctly() {
        let seed = [0xaau8; 16];
        let iv = [0u8; 16];
        for n in [0, 1, 15, 16, 17, 31, 32, 33, 100] {
            let out = aes_ctr_prg(&seed, &iv, 0, n);
            assert_eq!(out.len(), n, "expected {} bytes, got {}", n, out.len());
        }
    }

    /// Distinct tweaks produce distinct outputs for the same (seed, iv).
    /// This is the FAEST domain-separation property the BAVC consumer
    /// will rely on.
    #[test]
    fn aes_ctr_prg_tweak_changes_output() {
        let seed = [0x42u8; 16];
        let iv = [0x17u8; 16];
        let out_a = aes_ctr_prg(&seed, &iv, 0, 32);
        let out_b = aes_ctr_prg(&seed, &iv, 1, 32);
        assert_ne!(out_a, out_b);
    }

    /// IV-as-counter-base test: setting iv[0] = 1 with tweak=0 is the
    /// same as iv = 0 with the PRG asked for an offset block —
    /// specifically, the AES input for counter=0 under iv[0]=1 is the
    /// same as counter=1 under iv=0.
    #[test]
    fn aes_ctr_prg_iv_lower_word_acts_as_offset() {
        let seed = [0x11u8; 16];
        let mut iv_one = [0u8; 16];
        iv_one[0] = 1;
        let out_iv1 = aes_ctr_prg(&seed, &iv_one, 0, 16);

        let zero_iv = [0u8; 16];
        let out_offset = aes_ctr_prg(&seed, &zero_iv, 0, 32);

        // First block under iv_one[0]=1 == second block under iv=0
        assert_eq!(&out_iv1[..16], &out_offset[16..32]);
    }
}
