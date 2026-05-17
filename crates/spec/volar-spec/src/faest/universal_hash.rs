// @reliability: experimental
//! @ai: assisted
//! FAEST v2 universal hash functions — §4.3.3 (VOLEHash) and §4.3.4 (ZKHash).
//!
//! Both are evaluated as Horner-style polynomials over GF(2^λ) × GF(2^64):
//!
//! - **VOLEHash**: takes a *byte* input (bits of the VOLE witness / correction
//!   vectors), parses it into λ-bit blocks and an optional 64-bit tail, and
//!   evaluates them as a polynomial in the key `r0 ∈ GF(2^λ)` and `r1 ∈ GF(2^64)`.
//!   Used in `chall_1` expansion to check VOLE consistency.
//!
//! - **ZKHash**: takes a list of field elements in `GF(2^λ)` and evaluates them
//!   as a polynomial in the same key pair. Used in `chall_2` expansion to check
//!   the QuickSilver constraint system (the A-hat / B-hat / C-hat values).
//!
//! Both functions are linear over F_2 in the input (for fixed key), which is
//! required by the ε_v / ε_zk almost-universality proofs in the spec.

use volar_primitives::{Galois128, Galois64};

/// Shared key for both hash functions: `(r0, r1) ∈ GF(2^128) × GF(2^64)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UniversalHashKey {
    /// GF(2^λ) component (λ=128 for FAEST-128).
    pub r0: Galois128,
    /// GF(2^64) component.
    pub r1: Galois64,
}

/// Output of both hash functions: `(h0, h1) ∈ GF(2^128) × GF(2^64)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UniversalHashOutput {
    pub h0: Galois128,
    pub h1: Galois64,
}

/// VOLEHash — FAEST §4.3.3.
///
/// Evaluates the input byte slice as a polynomial over GF(2^128) × GF(2^64):
///
/// 1. Parse `input` as consecutive 16-byte (128-bit) blocks `s_0, …, s_{m-1}`,
///    then an optional trailing block of up to 8 bytes treated as `t ∈ GF(2^64)`.
/// 2. `h0 = Σ_i s_i · r0^{i+1}` in GF(2^128) (Horner order: highest power first
///    inside the Horner loop, or equivalently accumulate with multiply-and-add).
/// 3. `h1 = Σ_i low64(s_i) · r1^{i+1}  +  t · r1^{m+1}` in GF(2^64).
///
/// The output is `(h0, h1)`.
///
/// # Parameters
///
/// - `key`: the hash key `(r0, r1)`.
/// - `input`: the raw byte slice. Must be a multiple of 16 bytes optionally
///   followed by up to 8 bytes.  Any bytes beyond the first `16·m + 8` are
///   ignored (the caller is responsible for sizing the buffer correctly).
pub fn vole_hash(key: &UniversalHashKey, input: &[u8]) -> UniversalHashOutput {
    let n_full = input.len() / 16;
    let tail = &input[n_full * 16..];

    let mut h0 = Galois128(0);
    let mut h1 = Galois64(0);
    let mut pow0 = key.r0;
    let mut pow1 = key.r1;

    for i in 0..n_full {
        let block = &input[i * 16..(i + 1) * 16];
        let mut bytes = [0u8; 16];
        bytes.copy_from_slice(block);
        let s = Galois128(u128::from_le_bytes(bytes));
        h0 = h0 + s * pow0;

        let s64 = Galois64(s.0 as u64); // low 64 bits
        h1 = h1 + s64 * pow1;

        pow0 = pow0 * key.r0;
        pow1 = pow1 * key.r1;
    }

    // Optional trailing 64-bit block (spec §4.3.3: last B=64-bit element).
    if !tail.is_empty() {
        let mut bytes = [0u8; 8];
        let n = tail.len().min(8);
        bytes[..n].copy_from_slice(&tail[..n]);
        let t = Galois64(u64::from_le_bytes(bytes));
        h1 = h1 + t * pow1;
    }

    UniversalHashOutput { h0, h1 }
}

/// ZKHash — FAEST §4.3.4.
///
/// Evaluates a slice of `GF(2^128)` elements as a polynomial:
///
/// 1. `h0 = Σ_i x_i · r0^{i+1}` in GF(2^128).
/// 2. `h1 = Σ_i low64(x_i) · r1^{i+1}` in GF(2^64).
///
/// Used by the QuickSilver verifier to batch-check the A-hat / B-hat / C-hat
/// sent by the prover (one element per gate constraint).
pub fn zk_hash(key: &UniversalHashKey, elements: &[Galois128]) -> UniversalHashOutput {
    let mut h0 = Galois128(0);
    let mut h1 = Galois64(0);
    let mut pow0 = key.r0;
    let mut pow1 = key.r1;

    for &x in elements {
        h0 = h0 + x * pow0;
        let x64 = Galois64(x.0 as u64);
        h1 = h1 + x64 * pow1;
        pow0 = pow0 * key.r0;
        pow1 = pow1 * key.r1;
    }

    UniversalHashOutput { h0, h1 }
}

/// Consistency check used by the FAEST verifier for the VOLE relation.
///
/// Given:
/// - Prover's hash of u: `hu = vole_hash(key, u_bytes)`.
/// - Verifier's hash of q: `hq = vole_hash(key, q_bytes)`.
/// - The verifier's global secret `Δ ∈ GF(2^λ)`.
/// - The hash of the correction `c`: `hc = vole_hash(key, c_bytes)`.
///
/// The VOLE relation `q = v + Δ·u` (in the big field) implies:
/// `hq = hv + Δ · (hu + hc)`.
/// This function checks that `hq + Δ·hu = hv + Δ·hc`
/// (i.e., the verifier's hash matches the prover-committed hash after correction).
///
/// Returns `true` iff the check passes.
pub fn vole_hash_consistency_check(
    key: &UniversalHashKey,
    hu: UniversalHashOutput,
    hq: UniversalHashOutput,
    hv: UniversalHashOutput,
    hc: UniversalHashOutput,
    delta: Galois128,
) -> bool {
    // Check: hq = hv + Δ · (hu + hc)
    let lhs0 = hq.h0;
    let rhs0 = hv.h0 + delta * (hu.h0 + hc.h0);

    let delta64 = Galois64(delta.0 as u64);
    let lhs1 = hq.h1;
    let rhs1 = hv.h1 + delta64 * (hu.h1 + hc.h1);

    lhs0 == rhs0 && lhs1 == rhs1
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    fn zero_key() -> UniversalHashKey {
        UniversalHashKey {
            r0: Galois128(0),
            r1: Galois64(0),
        }
    }

    fn unit_key() -> UniversalHashKey {
        UniversalHashKey {
            r0: Galois128(1),
            r1: Galois64(1),
        }
    }

    #[test]
    fn vole_hash_zero_key_gives_zero() {
        // Any polynomial evaluated at 0 is zero (all powers of 0 are 0).
        let input = [0xFFu8; 48];
        let out = vole_hash(&zero_key(), &input);
        assert_eq!(out.h0, Galois128(0));
        assert_eq!(out.h1, Galois64(0));
    }

    #[test]
    fn vole_hash_unit_key_sums_blocks() {
        // r0 = r1 = 1  →  h = x_0 + x_1 + ... (XOR in GF(2^128) and GF(2^64))
        let mut input = [0u8; 32];
        input[..16].fill(0xAA);
        input[16..].fill(0x55);
        let out = vole_hash(&unit_key(), &input);

        let a = Galois128(u128::from_le_bytes([0xAAu8; 16]));
        let b = Galois128(u128::from_le_bytes([0x55u8; 16]));
        // h0 = a*1 + b*1 = a XOR b
        assert_eq!(out.h0, a + b);
    }

    #[test]
    fn vole_hash_empty_is_zero() {
        let out = vole_hash(&unit_key(), &[]);
        assert_eq!(out.h0, Galois128(0));
        assert_eq!(out.h1, Galois64(0));
    }

    #[test]
    fn vole_hash_tail_only() {
        // 8-byte tail, no full blocks: only h1 gets a contribution.
        let input = [0x01u8; 8];
        let key = UniversalHashKey {
            r0: Galois128(2),
            r1: Galois64(3),
        };
        let out = vole_hash(&key, &input);
        // h0 should be 0 (no 128-bit blocks)
        assert_eq!(out.h0, Galois128(0));
        // h1 = t * r1^1 = Galois64(0x0101010101010101) * Galois64(3)
        let t = Galois64(u64::from_le_bytes([0x01u8; 8]));
        let expected = t * Galois64(3);
        assert_eq!(out.h1, expected);
    }

    #[test]
    fn vole_hash_linearity_over_f2() {
        // VOLEHash must be linear in the input: hash(a XOR b) = hash(a) XOR hash(b)
        let key = UniversalHashKey {
            r0: Galois128(0x123456789ABCDEF0123456789ABCDEF0),
            r1: Galois64(0xDEADBEEFCAFEBABE),
        };
        let a = [0xAAu8; 32];
        let b = [0x55u8; 32];
        let mut ab = [0u8; 32];
        for i in 0..32 {
            ab[i] = a[i] ^ b[i];
        }

        let ha = vole_hash(&key, &a);
        let hb = vole_hash(&key, &b);
        let hab = vole_hash(&key, &ab);

        // Linearity: hash(a XOR b) == hash(a) XOR hash(b)
        assert_eq!(hab.h0, ha.h0 + hb.h0);
        assert_eq!(hab.h1, ha.h1 + hb.h1);
    }

    #[test]
    fn zk_hash_zero_elements_zero_output() {
        let key = unit_key();
        let elems: &[Galois128] = &[];
        let out = zk_hash(&key, elems);
        assert_eq!(out.h0, Galois128(0));
        assert_eq!(out.h1, Galois64(0));
    }

    #[test]
    fn zk_hash_single_element() {
        let key = UniversalHashKey {
            r0: Galois128(5),
            r1: Galois64(7),
        };
        let x = Galois128(0x100);
        let out = zk_hash(&key, &[x]);
        // h0 = x * r0^1 = Galois128(0x100) * Galois128(5)
        assert_eq!(out.h0, x * Galois128(5));
        // h1 = low64(x) * r1^1 = Galois64(0x100) * Galois64(7)
        assert_eq!(out.h1, Galois64(0x100) * Galois64(7));
    }

    #[test]
    fn zk_hash_linearity() {
        // ZKHash is linear over GF(2^λ) in the input.
        let key = UniversalHashKey {
            r0: Galois128(0xDEAD),
            r1: Galois64(0xBEEF),
        };
        let a = [Galois128(0x111), Galois128(0x222)];
        let b = [Galois128(0x333), Galois128(0x444)];
        let ab: std::vec::Vec<Galois128> = a.iter().zip(b.iter()).map(|(&x, &y)| x + y).collect();

        let ha = zk_hash(&key, &a);
        let hb = zk_hash(&key, &b);
        let hab = zk_hash(&key, &ab);

        assert_eq!(hab.h0, ha.h0 + hb.h0);
        assert_eq!(hab.h1, ha.h1 + hb.h1);
    }

    #[test]
    fn vole_hash_consistency_check_passes_honest() {
        // Simulate an honest VOLE: q = v + Δ·u
        let delta = Galois128(0x1234_5678_9ABC_DEF0_FEDC_BA98_7654_3210);
        let key = UniversalHashKey {
            r0: Galois128(0xAAAA_BBBB_CCCC_DDDD_EEEE_FFFF_0000_1111),
            r1: Galois64(0x1111_2222_3333_4444),
        };

        // u: 32 bytes, v: 32 bytes, c: 32 bytes (correction, all-zero for simplicity)
        let u_bytes = [0x7Fu8; 32];
        let c_bytes = [0u8; 32];

        // Compute q = v + Δ·u for a simple case where v = 0
        // (In GF(2^128) the product Δ·u_block for each block)
        let v_bytes = [0u8; 32];

        // Hash everything
        let hu = vole_hash(&key, &u_bytes);
        let hc = vole_hash(&key, &c_bytes); // zero correction
        let hv = vole_hash(&key, &v_bytes); // zero v

        // Honest q = Δ·u (since v=0), so hq = Δ·hu
        // Compute hq directly:
        let hq_h0 = delta * hu.h0;
        let hq_h1 = Galois64(delta.0 as u64) * hu.h1;
        let hq = UniversalHashOutput { h0: hq_h0, h1: hq_h1 };

        assert!(vole_hash_consistency_check(&key, hu, hq, hv, hc, delta));
    }

    #[test]
    fn vole_hash_consistency_check_rejects_tampered() {
        let delta = Galois128(3);
        let key = UniversalHashKey {
            r0: Galois128(7),
            r1: Galois64(11),
        };
        let u_bytes = [0xCCu8; 16];
        let v_bytes = [0u8; 16];
        let c_bytes = [0u8; 16];

        let hu = vole_hash(&key, &u_bytes);
        let hc = vole_hash(&key, &c_bytes);
        let hv = vole_hash(&key, &v_bytes);

        // Honest hq:
        let good_hq = UniversalHashOutput {
            h0: delta * hu.h0,
            h1: Galois64(delta.0 as u64) * hu.h1,
        };
        assert!(vole_hash_consistency_check(&key, hu, good_hq, hv, hc, delta));

        // Tampered hq (flip a bit):
        let bad_hq = UniversalHashOutput {
            h0: good_hq.h0 + Galois128(1),
            h1: good_hq.h1,
        };
        assert!(!vole_hash_consistency_check(&key, hu, bad_hq, hv, hc, delta));
    }
}
