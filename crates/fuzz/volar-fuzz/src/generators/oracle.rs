//! Deterministic hash oracle for fuzz testing.
//!
//! Approximates a random oracle by computing FNV-1a over the oracle index and
//! input bits.  The same input always produces the same output, so pass
//! correctness (before == after) can be checked without any external state.

// FNV-1a 64-bit constants.
const FNV_OFFSET: u64 = 14_695_981_039_346_656_037;
const FNV_PRIME: u64 = 1_099_511_628_211;

/// Evaluate oracle `oracle_idx` on `flat_inputs` (LSB-first bit vector),
/// returning `output_width` output bits.
///
/// Implementation: FNV-1a over `oracle_idx` bytes + input bits packed into
/// bytes, then the hash is extended to the required width by repeatedly
/// mixing with `FNV_PRIME`.
pub fn hash_oracle(oracle_idx: u32, flat_inputs: &[bool], output_width: usize) -> Vec<bool> {
    // Seed with oracle index bytes.
    let mut h = FNV_OFFSET;
    for byte in oracle_idx.to_le_bytes() {
        h ^= byte as u64;
        h = h.wrapping_mul(FNV_PRIME);
    }

    // Mix in input bits packed into bytes (LSB first within each byte).
    let mut pending: u8 = 0;
    let mut pending_bits: u32 = 0;
    for &b in flat_inputs {
        pending |= (b as u8) << pending_bits;
        pending_bits += 1;
        if pending_bits == 8 {
            h ^= pending as u64;
            h = h.wrapping_mul(FNV_PRIME);
            pending = 0;
            pending_bits = 0;
        }
    }
    if pending_bits > 0 {
        h ^= pending as u64;
        h = h.wrapping_mul(FNV_PRIME);
    }

    // Expand to output_width bits by iteratively re-mixing.
    let mut out = Vec::with_capacity(output_width);
    let mut seed = h;
    while out.len() < output_width {
        seed = seed.wrapping_mul(FNV_PRIME) ^ FNV_OFFSET;
        for bit in 0u32..64 {
            if out.len() < output_width {
                out.push((seed >> bit) & 1 == 1);
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deterministic() {
        let a = hash_oracle(0, &[true, false, true], 16);
        let b = hash_oracle(0, &[true, false, true], 16);
        assert_eq!(a, b);
    }

    #[test]
    fn different_inputs_differ() {
        let a = hash_oracle(0, &[false; 8], 64);
        let b = hash_oracle(0, &[true; 8], 64);
        assert_ne!(a, b);
    }

    #[test]
    fn different_oracle_ids_differ() {
        let a = hash_oracle(0, &[false; 8], 64);
        let b = hash_oracle(1, &[false; 8], 64);
        assert_ne!(a, b);
    }

    #[test]
    fn correct_length() {
        for w in [0, 1, 7, 8, 63, 64, 65, 128, 200] {
            assert_eq!(hash_oracle(0, &[], w).len(), w);
        }
    }
}
