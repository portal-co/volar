// @reliability: experimental
// @ai: assisted
//
// Runtime helper functions for FHE-woven circuits.
//
// This file is NOT a Rust module — it is parsed by volar-compiler and
// linked into woven circuit code as a `LinkedSpec`. The functions here
// run on the evaluator side (plaintext) and are called from the woven
// circuit for address conversion and other utility operations.
//
// All code must be "total Rust": bounded loops, no while/loop/assert!/panic!.
// The volar-compiler parser must be able to handle every construct used here.

/// Convert a bool-slice address representation to a `usize` index.
///
/// In the woven circuit, encrypted addresses are decrypted to `[bool; N]`
/// at action boundaries. Storage arrays use `usize` indices, so this
/// helper folds the bit vector into a single `usize`.
///
/// Bit 0 of the result is `bits[0]`, bit 1 is `bits[1]`, etc. (little-endian).
/// Bits beyond position 63 are ignored (usize is at most 64 bits).
pub fn bools_to_usize(bits: &[bool]) -> usize {
    let mut result: usize = 0;
    // Bounded loop: usize is at most 64 bits.
    for i in 0..64 {
        if i < bits.len() {
            if bits[i] {
                result = result | (1usize << i);
            }
        }
    }
    result
}
