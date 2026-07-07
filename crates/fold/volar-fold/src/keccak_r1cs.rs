// @reliability: experimental
// @ai: assisted
//! **Keccak-256 arithmetized in R1CS** over the folding scalar field `F_ℓ`.
//!
//! This is the *folding side* of the dual-preimage boundary embedding
//! (`docs/boundary-link-embedding.md`): the folded instance carries constraints
//! proving `Keccak256(S_boundary) = d` for a public digest `d`, so the existing
//! native check ([`crate::verify`]) covers the preimage proof.  The VOLE side
//! proves a preimage of the *same* `d` (woven in `volar-weaver`); collision
//! resistance then forces the two boundaries to be the same bit-string — with no
//! cross-field algebra.
//!
//! ## Why this is large
//! Over a prime field **XOR is quadratic** (`a⊕b = a+b−2ab`), so unlike a binary
//! circuit where only χ is nonlinear, here θ/χ XORs *all* cost a constraint.  One
//! Keccak-f[1600] ⇒ ~155k constraints.  That is `O(1)` in the gap length `m`, and
//! tractable only because `F_ℓ` is now Montgomery and the MSM is Pippenger.
//!
//! ## Method (generic boolean-circuit → R1CS lowering)
//! Every wire is a [`Lc`] (linear combination over the witness).  Gates:
//! - **NOT** `1 − a` — free (an `Lc`).
//! - **AND** `p = a·b` — one constraint `(a)·(b) = p`, fresh boolean var `p`.
//! - **XOR** `r = a⊕b` — one constraint `(a)·(b) = ½·(a + b − r)`, fresh var `r`.
//!   (For boolean `a,b`: `a·b = (a+b−r)/2 ⟺ r = a+b−2ab = a⊕b`, and `r` is then
//!   forced boolean, so only the *primary inputs* need an explicit `b²=b`.)
//! - **ρ/π** lane rotation/permutation — free wire re-indexing.
//! - **ι** XOR with the public round constant — free (constant XOR = NOT or
//!   identity).
//!
//! Materializing every AND/XOR result as a fresh single-variable wire keeps each
//! constraint row tiny (no linear-combination blow-up across 24 rounds).  The
//! gate logic is cross-checked against [`crate::keccak::keccak256_bits`] (itself
//! anchored to the `sha3` crate).

use alloc::vec::Vec;

use crate::keccak::{PILN, RNDC, ROTC};
use crate::r1cs::R1CS;
use crate::r1cs_builder::{bit_scalar, Builder, Lc};
use crate::scalar::Scalar;

// ── Keccak-f[1600] over Lc wires (state = 25 lanes × 64 bits, flat index l*64+i) ─

fn lane_bit(l: usize, i: usize) -> usize {
    l * 64 + i
}

fn keccakf_circuit(bld: &mut Builder, st: &mut [Lc]) {
    for round in 0..24 {
        // θ: column parities, then fold into every lane of the column.
        let mut bc: Vec<Vec<Lc>> = Vec::with_capacity(5);
        for col in 0..5 {
            let mut lane = Vec::with_capacity(64);
            for i in 0..64 {
                let mut acc = st[lane_bit(col, i)].clone();
                for k in 1..5 {
                    let other = st[lane_bit(col + 5 * k, i)].clone();
                    acc = bld.xor(&acc, &other);
                }
                lane.push(acc);
            }
            bc.push(lane);
        }
        for col in 0..5 {
            let left = bc[(col + 4) % 5].clone();
            let right = bc[(col + 1) % 5].clone();
            // t = bc[col-1] ⊕ rotl(bc[col+1], 1):  out bit i = right[(i-1) mod 64]
            let mut t = Vec::with_capacity(64);
            for i in 0..64 {
                let r = right[(i + 63) % 64].clone();
                t.push(bld.xor(&left[i], &r));
            }
            for jj in 0..5 {
                let l = jj * 5 + col; // lanes col, col+5, col+10, col+15, col+20
                for i in 0..64 {
                    let cur = st[lane_bit(l, i)].clone();
                    st[lane_bit(l, i)] = bld.xor(&cur, &t[i]);
                }
            }
        }

        // ρ + π: rotate/permute lanes (pure re-indexing of wires).
        let mut cur: Vec<Lc> = (0..64).map(|i| st[lane_bit(1, i)].clone()).collect();
        for idx in 0..24 {
            let j = PILN[idx];
            let tmp: Vec<Lc> = (0..64).map(|i| st[lane_bit(j, i)].clone()).collect();
            let rot = ROTC[idx] as usize % 64;
            for i in 0..64 {
                // st[j] = rotl(cur, rot):  out bit i = cur[(i-rot) mod 64]
                st[lane_bit(j, i)] = cur[(i + 64 - rot) % 64].clone();
            }
            cur = tmp;
        }

        // χ: per row, st[i] ^= (¬st[i+1]) & st[i+2]  (read from a snapshot).
        let mut j = 0;
        while j < 25 {
            let row: Vec<Vec<Lc>> = (0..5)
                .map(|i| (0..64).map(|bit| st[lane_bit(j + i, bit)].clone()).collect())
                .collect();
            for i in 0..5 {
                for bit in 0..64 {
                    let nb = bld.not(&row[(i + 1) % 5][bit]);
                    let andt = bld.and(&nb, &row[(i + 2) % 5][bit]);
                    st[lane_bit(j + i, bit)] = bld.xor(&row[i][bit], &andt);
                }
            }
            j += 5;
        }

        // ι: XOR the public round constant into lane 0 (free).
        for bit in 0..64 {
            let cbit = (RNDC[round] >> bit) & 1 == 1;
            let cur = st[bit].clone();
            st[bit] = bld.xor_const(&cur, cbit);
        }
    }
}

/// Number of 136-byte sponge blocks for a `num_input_bits`-bit message (after
/// SHA3 `pad10*1`).  Mirrors [`crate::keccak::sha3_256`]'s block accounting.
fn num_blocks(num_input_bits: usize) -> usize {
    let nbytes = num_input_bits.div_ceil(8);
    nbytes / crate::keccak::RATE_BYTES + 1
}

/// Build the padded sponge message as a flat bit-`Lc` vector (length
/// `num_blocks·1088`), exactly reproducing the byte-level `0x06`/`0x80` padding
/// of [`crate::keccak::sha3_256`] at bit granularity.  Padding bits are constants
/// (free), so this allocates no constraints.
fn padded_message_bits(bld: &Builder, input: &[Lc]) -> Vec<Lc> {
    let num_input_bits = input.len();
    let nbytes = num_input_bits.div_ceil(8);
    let rate = crate::keccak::RATE_BYTES;
    let stripped = nbytes / rate;
    let rem = nbytes - stripped * rate;
    let total_bytes = (stripped + 1) * rate;
    let fin06_byte = stripped * rate + rem; // SHA3 domain byte `0x06`
    let fin80_byte = total_bytes - 1; // pad10*1 final `0x80`

    let total_bits = total_bytes * 8;
    let mut out = Vec::with_capacity(total_bits);
    for i in 0..total_bits {
        let byte = i / 8;
        let j = i % 8;
        let base = if byte < nbytes && byte * 8 + j < num_input_bits {
            input[byte * 8 + j].clone()
        } else {
            Lc::zero()
        };
        let mut cbyte = 0u8;
        if byte == fin06_byte {
            cbyte ^= 0x06;
        }
        if byte == fin80_byte {
            cbyte ^= 0x80;
        }
        out.push(bld.xor_const(&base, (cbyte >> j) & 1 == 1));
    }
    out
}

/// The R1CS + satisfying witness proving `Keccak256(input) = digest`.
pub struct KeccakInstance {
    pub r1cs: R1CS,
    /// The witness `W` (`z = [W ‖ 1]`).
    pub witness: Vec<Scalar>,
    /// Witness-variable indices of the (boolean-constrained) input bits.
    pub input_vars: Vec<usize>,
    /// The digest the circuit is bound to (`= keccak256_bits(input)` for
    /// [`keccak256_r1cs`]).
    pub digest_bits: [bool; 256],
}

/// Lower a full SHA3-256 (Keccak-256) over `num_input_bits` boolean inputs to
/// R1CS, binding the squeezed 256-bit output to the public `digest`.  The
/// concrete `input` bits also produce the satisfying assignment.  The instance is
/// satisfiable iff `digest == keccak256_bits(input)`.
pub fn keccak256_r1cs_with_digest(input: &[bool], digest: &[bool; 256]) -> KeccakInstance {
    let mut bld = Builder::new();

    // 1. Boolean-constrained input bits.
    let input_lcs: Vec<Lc> = input.iter().map(|&b| bld.input_bit(b)).collect();
    let input_vars: Vec<usize> =
        input_lcs.iter().map(|lc| lc.terms[0].0).collect();

    // 2. Sponge: absorb the padded message block-by-block, permuting each block.
    let msg = padded_message_bits(&bld, &input_lcs);
    let mut st: Vec<Lc> = (0..1600).map(|_| Lc::zero()).collect();
    let blocks = num_blocks(input.len());
    for b in 0..blocks {
        for g in 0..1088 {
            let cur = st[g].clone();
            st[g] = bld.xor(&cur, &msg[b * 1088 + g]);
        }
        keccakf_circuit(&mut bld, &mut st);
    }

    // 3. Bind the squeezed 256 output bits to the public digest:  st[i]·1 = d_i.
    for i in 0..256 {
        let d_lc = Lc { terms: Vec::new(), constant: bit_scalar(digest[i]) };
        let st_i = st[i].clone();
        bld.enforce(&st_i, &Lc::one(), &d_lc);
    }

    let (r1cs, witness) = bld.finish();
    KeccakInstance { r1cs, witness, input_vars, digest_bits: *digest }
}

/// Convenience: bind to the *correct* digest `keccak256_bits(input)`, so the
/// resulting instance is satisfiable by construction.
pub fn keccak256_r1cs(input: &[bool]) -> KeccakInstance {
    let digest = crate::keccak::keccak256_bits(input);
    keccak256_r1cs_with_digest(input, &digest)
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::keccak::{digest_bits_to_bytes, keccak256_bits, sha3_256};

    /// `z = [W ‖ 1]`.
    fn full_z(inst: &KeccakInstance) -> Vec<Scalar> {
        let mut z = inst.witness.clone();
        z.push(Scalar::ONE);
        z
    }

    #[test]
    fn r1cs_satisfied_and_digest_matches_reference() {
        let input: std::vec::Vec<bool> =
            [0xa5u8, 0x3c].iter().flat_map(|b| (0..8).map(move |i| (b >> i) & 1 == 1)).collect();
        let inst = keccak256_r1cs(&input);
        // The circuit's digest equals the lane-based reference (and the sha3 crate
        // via keccak.rs's own anchor), confirming the gate logic.
        assert_eq!(inst.digest_bits, keccak256_bits(&input));
        assert_eq!(digest_bits_to_bytes(&inst.digest_bits), sha3_256(&[0xa5, 0x3c]));
        // The arithmetization is satisfied by the honest witness.
        assert!(inst.r1cs.is_satisfied(&full_z(&inst)), "honest Keccak witness must satisfy R1CS");
    }

    #[test]
    fn wrong_digest_is_unsatisfiable() {
        let input: std::vec::Vec<bool> = (0..13).map(|i| i % 3 == 0).collect();
        // Bind to a digest with one bit flipped from the true one.
        let mut d = keccak256_bits(&input);
        d[100] = !d[100];
        let inst = keccak256_r1cs_with_digest(&input, &d);
        // The honest witness produces the *true* output bits, which now disagree
        // with the bound digest ⇒ the output-equality constraints fail.
        assert!(!inst.r1cs.is_satisfied(&full_z(&inst)), "wrong digest must be unsatisfiable");
    }

    #[test]
    fn tampered_input_bit_breaks_satisfaction() {
        let input: std::vec::Vec<bool> = (0..16).map(|i| i % 2 == 0).collect();
        let inst = keccak256_r1cs(&input);
        let mut z = full_z(&inst);
        // Flip an input wire (stays boolean, so b²=b still holds, but every
        // downstream gate constraint that consumed it now breaks).
        let v = inst.input_vars[0];
        z[v] = if z[v] == Scalar::ONE { Scalar::ZERO } else { Scalar::ONE };
        assert!(!inst.r1cs.is_satisfied(&z), "tampered input must break the circuit");
    }
}
