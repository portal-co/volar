// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! IKNP-style correlated-OT extension (Ishai-Kilian-Nielsen-Petrank, 2003).
//!
//! Amortises κ base OTs into `M` correlated OTs at the cost of `O(M·κ)`
//! local work. Each extended OT outputs an `L`-byte row with the standard
//! C-OT correlation `v_j = r0_j ⊕ b_j · Δ_msg`.
//!
//! # Protocol (semi-honest, single-shot in-process API)
//!
//! Notation: extension *sender* (S) plays base-OT *receiver*; extension
//! *receiver* (R) plays base-OT *sender*. `H` is a hash modelled as a
//! random oracle, used both as PRG (seed → m bits) and as KDF (κ-bit Q
//! row → L-byte output).
//!
//! 1. **S** picks `Δ_ot ∈ {0,1}^κ`.
//! 2. **R** picks κ pairs of seeds `(k_0^i, k_1^i) ∈ {0,1}^{κ}` (i=1..κ).
//! 3. **κ base OTs**: for each i, S learns `k_{Δ_ot[i]}^i` from R via base
//!    OT. (Roles are reversed: R is base-OT-sender, S is base-OT-receiver.)
//! 4. **R** computes column vectors `t^i = PRG(k_0^i)`, `u^i = t^i ⊕
//!    PRG(k_1^i) ⊕ r` and sends `u^i` to S.
//! 5. **S** computes `q^i` from `k_{Δ_ot[i]}^i` and `u^i`:
//!    - `Δ_ot[i] = 0` → `q^i = PRG(k_0^i) = t^i`,
//!    - `Δ_ot[i] = 1` → `q^i = PRG(k_1^i) ⊕ u^i = t^i ⊕ r`.
//!
//!    So `q^i = t^i ⊕ Δ_ot[i] · r` element-wise.
//! 6. **Transpose**: row `Q[j] = (q^1[j], …, q^κ[j])`, and similarly `T[j]`.
//!    The relation is `Q[j] = T[j] ⊕ r_j · Δ_ot`.
//! 7. **Random-OT keys** (length `L`):
//!    - `k0_j = H(j, Q[j])`,
//!    - `k1_j = H(j, Q[j] ⊕ Δ_ot)`,
//!    - receiver knows `k_{r_j}_j = H(j, T[j])` (which equals `k0_j` when
//!      `r_j = 0` and `k1_j` when `r_j = 1`).
//! 8. **C-OT correction**: caller-chosen `Δ_msg ∈ {0,1}^L`. S computes
//!    `c_j = k0_j ⊕ k1_j ⊕ Δ_msg` and sends to R. Final outputs:
//!    - sender: `r0_j = k0_j`,
//!    - receiver: `v_j = k_{r_j}_j ⊕ r_j · c_j` which simplifies to
//!      `v_j = r0_j ⊕ r_j · Δ_msg`.
//!
//! # Caveats
//!
//! - Semi-honest only (the original IKNP construction). For malicious
//!   security one needs an additional consistency check (KOS / SoftSpoken).
//! - `κ` is fixed to 128 (16 bytes). Adjust [`IKNP_KAPPA`] in concert with
//!   the chosen security level.
//! - Role-separated steps live in [`iknp_cot_extend_base`]; the const-generic
//!   [`iknp_cot_extend`] is an in-process driver over Chou-Orlandi.

use alloc::vec::Vec;
use digest::Digest;

use super::base_ot::{BaseOt, ChouOrlandi};
use super::group::Group;
use crate::SpecRng;

/// Security parameter: number of base OTs and width of `Δ_ot`.
pub const IKNP_KAPPA: usize = 128;
/// `IKNP_KAPPA / 8`.
pub const IKNP_KAPPA_BYTES: usize = IKNP_KAPPA / 8;

/// PRG: hash-based stream-cipher style, `H(seed || idx || counter)`.
fn prg_with_index<D: Digest>(seed: &[u8], idx: u32, out: &mut [u8]) {
    let mut counter: u32 = 0;
    let mut pos = 0;
    while pos < out.len() {
        let mut h = D::new();
        h.update(seed);
        h.update(idx.to_le_bytes());
        h.update(counter.to_le_bytes());
        let block = h.finalize();
        let block_bytes: &[u8] = block.as_ref();
        let take = (out.len() - pos).min(block_bytes.len());
        out[pos..pos + take].copy_from_slice(&block_bytes[..take]);
        pos += take;
        counter += 1;
    }
}

/// PRG: expand a κ-bit seed into `M` output bits.
fn prg_to_bools<D: Digest>(seed: &[u8], out: &mut [bool]) {
    let mut counter: u32 = 0;
    let mut pos = 0;
    while pos < out.len() {
        let mut h = D::new();
        h.update(seed);
        h.update(counter.to_le_bytes());
        let block = h.finalize();
        let block_bytes: &[u8] = block.as_ref();
        for &byte in block_bytes.iter() {
            for bit in 0..8 {
                if pos >= out.len() {
                    return;
                }
                out[pos] = ((byte >> bit) & 1) == 1;
                pos += 1;
            }
        }
        counter += 1;
    }
}

/// Pack `κ` bits into `κ/8` bytes (LSB of each byte first).
pub fn pack_kappa(bits: &[bool; IKNP_KAPPA]) -> [u8; IKNP_KAPPA_BYTES] {
    let mut out = [0u8; IKNP_KAPPA_BYTES];
    for i in 0..IKNP_KAPPA {
        if bits[i] {
            out[i / 8] |= 1 << (i % 8);
        }
    }
    out
}

/// Run an IKNP correlated-OT extension producing `M` C-OTs of `L` bytes each.
///
/// In-process driver: Chou-Orlandi base OT, two RNG streams.
pub fn iknp_cot_extend<G, D, R, const M: usize, const L: usize>(
    rng_s: &mut R,
    rng_r: &mut R,
    receiver_bits: &[bool; M],
    delta_msg: &[u8; L],
) -> ([[u8; L]; M], [[u8; L]; M])
where
    G: Group,
    D: Digest,
    R: SpecRng,
{
    let (r0, v) = iknp_cot_extend_base::<ChouOrlandi<G, D>, D, R, L>(
        rng_s,
        rng_r,
        receiver_bits.as_slice(),
        delta_msg,
    );
    let mut sender_r0 = [[0u8; L]; M];
    let mut receiver_v = [[0u8; L]; M];
    for j in 0..M {
        sender_r0[j] = r0[j];
        receiver_v[j] = v[j];
    }
    (sender_r0, receiver_v)
}

/// IKNP extension with a pluggable [`BaseOt`] and a runtime output length.
///
/// Base OT transfers [`IKNP_KAPPA_BYTES`]-byte seeds. `delta_msg` is the
/// C-OT correlation (length `L`).
pub fn iknp_cot_extend_base<B, D, R, const L: usize>(
    rng_s: &mut R,
    rng_r: &mut R,
    receiver_bits: &[bool],
    delta_msg: &[u8; L],
) -> (Vec<[u8; L]>, Vec<[u8; L]>)
where
    B: BaseOt<IKNP_KAPPA_BYTES>,
    D: Digest,
    R: SpecRng,
{
    let m = receiver_bits.len();

    let mut delta_ot = [false; IKNP_KAPPA];
    for i in 0..IKNP_KAPPA {
        delta_ot[i] = (rng_s.next_u32() & 1) == 1;
    }
    let delta_ot_bytes = pack_kappa(&delta_ot);

    let mut seeds_0 = [[0u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    let mut seeds_1 = [[0u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0..IKNP_KAPPA {
        for b in 0..IKNP_KAPPA_BYTES {
            seeds_0[i][b] = (rng_r.next_u32() & 0xFF) as u8;
            seeds_1[i][b] = (rng_r.next_u32() & 0xFF) as u8;
        }
    }

    // κ base OTs: ext receiver is base sender, ext sender is base receiver.
    let mut chosen_seeds = [[0u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0..IKNP_KAPPA {
        let (s_state, setup) = B::sender_setup(rng_r);
        let (r_state, recv_msg) = B::recv_start(rng_s, &setup, delta_ot[i]);
        let payload = B::sender_payload(rng_r, &s_state, &recv_msg, &seeds_0[i], &seeds_1[i]);
        chosen_seeds[i] = B::recv_finish(&r_state, &payload);
    }

    let (t_cols, u_msg) =
        iknp_receiver_u_cols::<D>(m, receiver_bits, &seeds_0, &seeds_1);
    let (sender_r0, corrections) = iknp_sender_from_u::<D, L>(
        m,
        delta_msg,
        &delta_ot,
        &delta_ot_bytes,
        &chosen_seeds,
        &u_msg,
    );
    let receiver_v = iknp_receiver_finish::<D, L>(receiver_bits, &t_cols, &corrections);
    (sender_r0, receiver_v)
}

/// Receiver → sender: `u^i = PRG(k0^i) ⊕ PRG(k1^i) ⊕ r` (IKNP step 4).
#[derive(Clone)]
pub struct IknpUMsg {
    pub u_cols: Vec<Vec<bool>>,
}

/// Receiver expands `t` columns and the `u` message from both seed pairs.
pub fn iknp_receiver_u_cols<D: Digest>(
    m: usize,
    receiver_bits: &[bool],
    seeds_0: &[[u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA],
    seeds_1: &[[u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA],
) -> (Vec<Vec<bool>>, IknpUMsg) {
    debug_assert_eq!(receiver_bits.len(), m);
    let mut t_cols = Vec::with_capacity(IKNP_KAPPA);
    let mut u_cols = Vec::with_capacity(IKNP_KAPPA);
    for i in 0..IKNP_KAPPA {
        let mut t_col = alloc::vec![false; m];
        prg_to_bools::<D>(&seeds_0[i], &mut t_col);
        let mut prg1 = alloc::vec![false; m];
        prg_to_bools::<D>(&seeds_1[i], &mut prg1);
        let mut u_col = alloc::vec![false; m];
        for j in 0..m {
            u_col[j] = t_col[j] ^ prg1[j] ^ receiver_bits[j];
        }
        t_cols.push(t_col);
        u_cols.push(u_col);
    }
    (t_cols, IknpUMsg { u_cols })
}

/// Sender consumes `u` and produces `r0` rows plus C-OT correction strings.
pub fn iknp_sender_from_u<D: Digest, const L: usize>(
    m: usize,
    delta_msg: &[u8; L],
    delta_ot: &[bool; IKNP_KAPPA],
    delta_ot_bytes: &[u8; IKNP_KAPPA_BYTES],
    chosen_seeds: &[[u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA],
    u_msg: &IknpUMsg,
) -> (Vec<[u8; L]>, Vec<[u8; L]>) {
    debug_assert_eq!(u_msg.u_cols.len(), IKNP_KAPPA);
    let mut q_cols = Vec::with_capacity(IKNP_KAPPA);
    for i in 0..IKNP_KAPPA {
        let mut prg_chosen = alloc::vec![false; m];
        prg_to_bools::<D>(&chosen_seeds[i], &mut prg_chosen);
        let mut q_col = alloc::vec![false; m];
        for j in 0..m {
            if delta_ot[i] {
                q_col[j] = prg_chosen[j] ^ u_msg.u_cols[i][j];
            } else {
                q_col[j] = prg_chosen[j];
            }
        }
        q_cols.push(q_col);
    }

    let mut sender_r0 = Vec::with_capacity(m);
    let mut corrections = Vec::with_capacity(m);
    let mut q_row = [false; IKNP_KAPPA];
    for j in 0..m {
        for i in 0..IKNP_KAPPA {
            q_row[i] = q_cols[i][j];
        }
        let q_bytes = pack_kappa(&q_row);
        let mut r0 = [0u8; L];
        prg_with_index::<D>(&q_bytes, j as u32, &mut r0);

        let mut q_xor_delta = q_bytes;
        for b in 0..IKNP_KAPPA_BYTES {
            q_xor_delta[b] ^= delta_ot_bytes[b];
        }
        let mut r1 = [0u8; L];
        prg_with_index::<D>(&q_xor_delta, j as u32, &mut r1);

        let mut correction = [0u8; L];
        for b in 0..L {
            correction[b] = r0[b] ^ r1[b] ^ delta_msg[b];
        }
        sender_r0.push(r0);
        corrections.push(correction);
    }
    (sender_r0, corrections)
}

/// Receiver applies C-OT corrections to the `t` rows.
pub fn iknp_receiver_finish<D: Digest, const L: usize>(
    receiver_bits: &[bool],
    t_cols: &[Vec<bool>],
    corrections: &[[u8; L]],
) -> Vec<[u8; L]> {
    let m = receiver_bits.len();
    debug_assert_eq!(t_cols.len(), IKNP_KAPPA);
    debug_assert_eq!(corrections.len(), m);
    let mut receiver_v = Vec::with_capacity(m);
    let mut t_row = [false; IKNP_KAPPA];
    for j in 0..m {
        for i in 0..IKNP_KAPPA {
            t_row[i] = t_cols[i][j];
        }
        let t_bytes = pack_kappa(&t_row);
        let mut v_pre = [0u8; L];
        prg_with_index::<D>(&t_bytes, j as u32, &mut v_pre);
        let mut vj = [0u8; L];
        if receiver_bits[j] {
            for b in 0..L {
                vj[b] = v_pre[b] ^ corrections[j][b];
            }
        } else {
            vj = v_pre;
        }
        receiver_v.push(vj);
    }
    receiver_v
}

#[cfg(test)]
mod tests {
    use super::super::group::ToyGroup;
    use super::*;
    use sha2::Sha256;

    struct TestRng(u64);
    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
            (z ^ (z >> 31)) as u32
        }
    }

    /// Each output row satisfies the C-OT relation `v = r0 ⊕ b · Δ_msg`.
    #[test]
    fn iknp_cot_relation_holds() {
        const M: usize = 16;
        const L: usize = 16;
        let mut rng_s = TestRng(0x1111_2222_3333_4444);
        let mut rng_r = TestRng(0xAAAA_BBBB_CCCC_DDDD);

        let mut bits = [false; M];
        for j in 0..M {
            bits[j] = (j & 1) == 1;
        }
        let mut delta = [0u8; L];
        for b in 0..L {
            delta[b] = (b as u8).wrapping_mul(17).wrapping_add(3);
        }

        let (r0, v) =
            iknp_cot_extend::<ToyGroup, Sha256, _, M, L>(&mut rng_s, &mut rng_r, &bits, &delta);

        for j in 0..M {
            for b in 0..L {
                let expected = if bits[j] {
                    r0[j][b] ^ delta[b]
                } else {
                    r0[j][b]
                };
                assert_eq!(v[j][b], expected, "row {j} byte {b}: C-OT relation broken");
            }
        }
    }

    /// All-zero choice bits: every receiver row equals the sender row.
    #[test]
    fn iknp_all_zero_choices_equal_r0() {
        const M: usize = 8;
        const L: usize = 8;
        let mut rng_s = TestRng(0x5555_5555_5555_5555);
        let mut rng_r = TestRng(0x6666_6666_6666_6666);

        let bits = [false; M];
        let delta = [0xFFu8; L];
        let (r0, v) =
            iknp_cot_extend::<ToyGroup, Sha256, _, M, L>(&mut rng_s, &mut rng_r, &bits, &delta);
        for j in 0..M {
            assert_eq!(r0[j], v[j]);
        }
    }

    /// All-one choice bits: every receiver row equals `r0 ⊕ Δ_msg`.
    #[test]
    fn iknp_all_one_choices_xor_delta() {
        const M: usize = 8;
        const L: usize = 8;
        let mut rng_s = TestRng(0xC0DE_C0DE_C0DE_C0DE);
        let mut rng_r = TestRng(0xD00D_D00D_D00D_D00D);

        let bits = [true; M];
        let mut delta = [0u8; L];
        for b in 0..L {
            delta[b] = 0x5A ^ (b as u8);
        }
        let (r0, v) =
            iknp_cot_extend::<ToyGroup, Sha256, _, M, L>(&mut rng_s, &mut rng_r, &bits, &delta);
        for j in 0..M {
            for b in 0..L {
                assert_eq!(v[j][b], r0[j][b] ^ delta[b]);
            }
        }
    }

    /// Outputs are non-trivial — neither all zero nor all equal to delta.
    #[test]
    fn iknp_outputs_look_random() {
        const M: usize = 16;
        const L: usize = 16;
        let mut rng_s = TestRng(0x1234_5678_9ABC_DEF0);
        let mut rng_r = TestRng(0x0FED_CBA9_8765_4321);

        let bits = [false; M]; // worst case — receiver_v == sender_r0
        let delta = [0u8; L];
        let (r0, _v) =
            iknp_cot_extend::<ToyGroup, Sha256, _, M, L>(&mut rng_s, &mut rng_r, &bits, &delta);
        let all_zero = r0.iter().all(|row| row.iter().all(|&b| b == 0));
        assert!(
            !all_zero,
            "sender output is all-zero — PRG/transpose likely broken"
        );
    }
}
