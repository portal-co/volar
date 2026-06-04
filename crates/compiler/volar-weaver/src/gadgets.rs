// @reliability: experimental
// @ai: assisted
//! Boolean-circuit gadgets synthesised by the memory-checking weavers.
//!
//! These emit `BIrStmt` gates (And/Or/Xor/Not/Zero/One) that the VOLE weaver
//! then lowers normally (XOR/NOT free, AND → `vole_and_prover_step` + hat).  The
//! key gadget is [`emit_lt`] — an unsigned less-than over committed bit-vectors,
//! used for the **timestamp-ordering check** that makes multiset memory checking
//! sound (it prevents the "consume a future write" attack that multiset balance
//! alone does not catch; see `docs/vole-continuation-bridge.md`).

use alloc::vec::Vec;
use volar_ir::boolar::BIrStmt;
use volar_ir::ir::IRVarId;

/// A tiny gate sink: appends `(result_id, stmt)` pairs and hands out fresh ids.
pub(crate) struct GateBuf {
    pub next: u32,
    pub gates: Vec<(IRVarId, BIrStmt)>,
}

impl GateBuf {
    pub(crate) fn new(first_free: u32) -> Self {
        GateBuf { next: first_free, gates: Vec::new() }
    }
    fn push(&mut self, stmt: BIrStmt) -> u32 {
        let id = self.next;
        self.next += 1;
        self.gates.push((IRVarId(id), stmt));
        id
    }
    fn zero(&mut self) -> u32 { self.push(BIrStmt::Zero) }
    fn one(&mut self) -> u32 { self.push(BIrStmt::One) }
    fn and(&mut self, a: u32, b: u32) -> u32 { self.push(BIrStmt::And(IRVarId(a), IRVarId(b))) }
    fn or(&mut self, a: u32, b: u32) -> u32 { self.push(BIrStmt::Or(IRVarId(a), IRVarId(b))) }
    fn xor(&mut self, a: u32, b: u32) -> u32 { self.push(BIrStmt::Xor(IRVarId(a), IRVarId(b))) }
    fn not(&mut self, a: u32) -> u32 { self.push(BIrStmt::Not(IRVarId(a))) }
}

/// Emit gates computing unsigned `a < b` for equal-length bit-vectors (bit 0 =
/// least-significant).  Returns the var-id of the 1-bit result.
///
/// Uses the equal-prefix recurrence, scanning MSB→LSB:
/// ```text
/// lt = 0; eq = 1
/// for i = n-1 .. 0:
///     lt = lt OR (eq AND ¬a_i AND b_i)
///     eq = eq AND ¬(a_i XOR b_i)
/// ```
/// `lt` flips to 1 only at the highest differing bit where `a_i < b_i`; once a
/// difference is found (`eq = 0`) lower bits cannot change the verdict.
pub(crate) fn emit_lt(a: &[u32], b: &[u32], buf: &mut GateBuf) -> u32 {
    assert_eq!(a.len(), b.len(), "emit_lt: bit-vectors must be equal length");
    let mut lt = buf.zero();
    let mut eq = buf.one();
    for i in (0..a.len()).rev() {
        let (ai, bi) = (a[i], b[i]);
        // lt |= eq & ¬a_i & b_i
        let nai = buf.not(ai);
        let ai_lt_bi = buf.and(nai, bi);
        let gated = buf.and(eq, ai_lt_bi);
        lt = buf.or(lt, gated);
        // eq &= ¬(a_i ^ b_i)
        let x = buf.xor(ai, bi);
        let xn = buf.not(x);
        eq = buf.and(eq, xn);
    }
    lt
}

/// Emit gates computing `a + 1 mod 2^n` for a little-endian bit-vector.
/// Returns the `n` result-bit var-ids (LSB first).  Ripple carry:
/// ```text
/// carry = 1
/// for i in 0..n:  out_i = a_i XOR carry ;  carry = a_i AND carry
/// ```
pub(crate) fn emit_incr(a: &[u32], buf: &mut GateBuf) -> Vec<u32> {
    let mut carry = buf.one();
    let mut out = Vec::with_capacity(a.len());
    for &ai in a {
        out.push(buf.xor(ai, carry));
        carry = buf.and(ai, carry);
    }
    out
}

// ── Keccak-256 boolean gadget (the VOLE leg of the dual-preimage boundary link) ──
//
// The folding side arithmetizes the same Keccak in R1CS (`volar-fold::keccak_r1cs`);
// here the *VOLE*-committed boundary is hashed by weaving these gates, lowered
// normally (XOR/NOT free, χ's ANDs → hats), and the squeezed output is checked
// against the public digest `d`.  Collision-resistance then forces the two
// boundaries (folding and VOLE) to be the same bit-string.  See
// `docs/boundary-link-embedding.md`.
//
// Standard Keccak-f[1600] tables (LSB-first lanes).  Duplicated from the lane
// reference in `volar-fold::keccak`; the sha3-anchored gadget test catches any
// transcription error.
const KECCAK_RNDC: [u64; 24] = [
    0x0000_0000_0000_0001, 0x0000_0000_0000_8082, 0x8000_0000_0000_808a, 0x8000_0000_8000_8000,
    0x0000_0000_0000_808b, 0x0000_0000_8000_0001, 0x8000_0000_8000_8081, 0x8000_0000_0000_8009,
    0x0000_0000_0000_008a, 0x0000_0000_0000_0088, 0x0000_0000_8000_8009, 0x0000_0000_8000_000a,
    0x0000_0000_8000_808b, 0x8000_0000_0000_008b, 0x8000_0000_0000_8089, 0x8000_0000_0000_8003,
    0x8000_0000_0000_8002, 0x8000_0000_0000_0080, 0x0000_0000_0000_800a, 0x8000_0000_8000_000a,
    0x8000_0000_8000_8081, 0x8000_0000_0000_8080, 0x0000_0000_8000_0001, 0x8000_0000_8000_8008,
];
const KECCAK_ROTC: [u32; 24] =
    [1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 2, 14, 27, 41, 56, 8, 25, 43, 62, 18, 39, 61, 20, 44];
const KECCAK_PILN: [usize; 24] =
    [10, 7, 11, 17, 18, 3, 5, 16, 8, 21, 24, 4, 15, 23, 19, 13, 12, 2, 20, 14, 22, 9, 6, 1];

/// SHA3-256 rate in bytes (1088 bits = 17 lanes).
const KECCAK_RATE_BYTES: usize = 136;

/// XOR with the constant-`0` wire elided (keeps the first absorb block + padding
/// zeros gate-free, mirroring the folding-side static-zero shortcut).
fn kxor(buf: &mut GateBuf, a: u32, b: u32, zero: u32) -> u32 {
    if a == zero {
        b
    } else if b == zero {
        a
    } else {
        buf.xor(a, b)
    }
}

/// The Keccak-f[1600] permutation over gate wires (state = 25 lanes × 64 bits,
/// flat index `lane*64 + bit`).  θ/ι are XORs (free), χ contributes the ANDs.
/// `rounds` is 24 for real Keccak; a smaller count is used only by the
/// lowering/codegen compile-check (the lowering path is round-independent, so a
/// reduced circuit validates the same machinery without a 150k-statement
/// `cargo check`).
fn keccakf_gates(buf: &mut GateBuf, st: &mut [u32], zero: u32, rounds: usize) {
    for round in 0..rounds {
        // θ: column parities, folded into every lane of the column.
        let mut bc = [[0u32; 64]; 5];
        for col in 0..5 {
            for i in 0..64 {
                let mut acc = st[col * 64 + i];
                for k in 1..5 {
                    acc = kxor(buf, acc, st[(col + 5 * k) * 64 + i], zero);
                }
                bc[col][i] = acc;
            }
        }
        for col in 0..5 {
            let left = bc[(col + 4) % 5];
            let right = bc[(col + 1) % 5];
            let mut t = [0u32; 64];
            for i in 0..64 {
                t[i] = kxor(buf, left[i], right[(i + 63) % 64], zero); // ⊕ rotl(·,1)
            }
            for jj in 0..5 {
                let lane = jj * 5 + col;
                for i in 0..64 {
                    st[lane * 64 + i] = kxor(buf, st[lane * 64 + i], t[i], zero);
                }
            }
        }

        // ρ + π: rotate/permute lanes — pure wire re-indexing.
        let mut cur = [0u32; 64];
        cur[..].copy_from_slice(&st[64..128]); // lane 1
        for idx in 0..24 {
            let j = KECCAK_PILN[idx];
            let mut tmp = [0u32; 64];
            tmp[..].copy_from_slice(&st[j * 64..j * 64 + 64]);
            let rot = (KECCAK_ROTC[idx] as usize) % 64;
            for i in 0..64 {
                st[j * 64 + i] = cur[(i + 64 - rot) % 64];
            }
            cur = tmp;
        }

        // χ: per row, st[i] ^= (¬st[i+1]) & st[i+2]  (from a snapshot of the row).
        let mut j = 0;
        while j < 25 {
            let mut row = [[0u32; 64]; 5];
            for (i, r) in row.iter_mut().enumerate() {
                r.copy_from_slice(&st[(j + i) * 64..(j + i) * 64 + 64]);
            }
            for i in 0..5 {
                for bit in 0..64 {
                    let nb = buf.not(row[(i + 1) % 5][bit]);
                    let andt = buf.and(nb, row[(i + 2) % 5][bit]);
                    st[(j + i) * 64 + bit] = kxor(buf, row[i][bit], andt, zero);
                }
            }
            j += 5;
        }

        // ι: XOR the public round constant into lane 0 (each set bit = a free NOT).
        for bit in 0..64 {
            if (KECCAK_RNDC[round] >> bit) & 1 == 1 {
                st[bit] = buf.not(st[bit]);
            }
        }
    }
}

/// Build the SHA3-padded sponge message as gate wires (length `num_blocks·1088`),
/// reproducing [`volar-fold`'s] byte-level `0x06`/`0x80` padding at bit
/// granularity.  Padding bits are constants ⇒ no ANDs.
fn padded_message_gates(buf: &mut GateBuf, input: &[u32], zero: u32) -> Vec<u32> {
    let num_input_bits = input.len();
    let nbytes = num_input_bits.div_ceil(8);
    let rate = KECCAK_RATE_BYTES;
    let stripped = nbytes / rate;
    let rem = nbytes - stripped * rate;
    let total_bytes = (stripped + 1) * rate;
    let fin06_byte = stripped * rate + rem;
    let fin80_byte = total_bytes - 1;

    let total_bits = total_bytes * 8;
    let mut out = Vec::with_capacity(total_bits);
    for i in 0..total_bits {
        let byte = i / 8;
        let j = i % 8;
        let base = if byte < nbytes && byte * 8 + j < num_input_bits {
            input[byte * 8 + j]
        } else {
            zero
        };
        let mut cbyte = 0u8;
        if byte == fin06_byte {
            cbyte ^= 0x06;
        }
        if byte == fin80_byte {
            cbyte ^= 0x80;
        }
        // ⊕ constant: identity, or a free NOT.
        out.push(if (cbyte >> j) & 1 == 1 { buf.not(base) } else { base });
    }
    out
}

/// Emit gates computing **Keccak-256** (SHA3-256) of the committed `input` bits
/// (LSB-first), returning the 256 squeezed output-bit var-ids.  This is the
/// VOLE-side preimage circuit of the dual-preimage boundary link; the woven
/// verifier constrains the outputs to the public digest `d` (see
/// [`emit_keccak256_digest_match`]).
pub(crate) fn emit_keccak256(input: &[u32], buf: &mut GateBuf) -> [u32; 256] {
    emit_keccak256_rounds(input, buf, 24)
}

/// As [`emit_keccak256`] but with a configurable Keccak-f round count.  Only the
/// `rounds = 24` form is real Keccak; smaller counts exist solely so the
/// VOLE-lowering compile-check can run on a tractable circuit.
pub(crate) fn emit_keccak256_rounds(input: &[u32], buf: &mut GateBuf, rounds: usize) -> [u32; 256] {
    let zero = buf.zero();
    let msg = padded_message_gates(buf, input, zero);
    let mut st = alloc::vec![zero; 1600];
    let blocks = msg.len() / 1088;
    for b in 0..blocks {
        for g in 0..1088 {
            st[g] = kxor(buf, st[g], msg[b * 1088 + g], zero);
        }
        keccakf_gates(buf, &mut st, zero, rounds);
    }
    core::array::from_fn(|i| st[i])
}

/// Emit Keccak-256 over `input` and the **public-digest equality** that the
/// dual-preimage link enforces: `match = ⋀_i (out_i XNOR d_i)`, a single wire
/// that is `1` iff `Keccak256(input) == digest`.  The prover opens `match` and
/// the verifier `assert_one_check`s it (exactly as the storage loop does for its
/// ordering bit).  `digest` is a 256-bit public value baked as constant gate
/// structure (XNOR with a constant is identity or a free NOT) — the established
/// way these weavers encode public values.  Returns the `match` var-id.
pub(crate) fn emit_keccak256_digest_match(
    input: &[u32],
    digest: &[bool],
    buf: &mut GateBuf,
    rounds: usize,
) -> u32 {
    assert_eq!(digest.len(), 256, "keccak digest must be 256 bits");
    let out = emit_keccak256_rounds(input, buf, rounds);
    // eq_i = out_i XNOR d_i = (out_i) if d_i else (¬out_i).
    let eq: Vec<u32> =
        (0..256).map(|i| if digest[i] { out[i] } else { buf.not(out[i]) }).collect();
    let mut acc = eq[0];
    for &e in &eq[1..] {
        acc = buf.and(acc, e);
    }
    acc
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrTarget, BIrTerminator};
    use volar_ir::ir::IRBlockTargetId;

    /// Reference: unsigned less-than over little-endian bit-vectors.
    fn lt_ref(a: u32, b: u32) -> bool { a < b }

    /// Minimal single-block cleartext evaluator for And/Or/Xor/Not/Zero/One.
    fn eval(circuit: &BIrBlocks, inputs: &[bool]) -> bool {
        let block = &circuit.blocks[0];
        let np = block.params as usize;
        let mut vals: Vec<bool> = inputs.to_vec();
        debug_assert_eq!(vals.len(), np);
        for stmt in &block.stmts {
            let v = match stmt {
                BIrStmt::Zero => false,
                BIrStmt::One => true,
                BIrStmt::And(a, b) => vals[a.0 as usize] && vals[b.0 as usize],
                BIrStmt::Or(a, b) => vals[a.0 as usize] || vals[b.0 as usize],
                BIrStmt::Xor(a, b) => vals[a.0 as usize] ^ vals[b.0 as usize],
                BIrStmt::Not(a) => !vals[a.0 as usize],
                _ => panic!("unexpected gate"),
            };
            vals.push(v);
        }
        match &block.terminator {
            BIrTerminator::Jmp(t) => vals[t.args[0].0 as usize],
            _ => panic!("expected Jmp"),
        }
    }

    /// Build a single-block circuit: params = a[0..n], b[0..n]; output = (a < b).
    fn build_lt_circuit(n: usize) -> BIrBlocks {
        let a: Vec<u32> = (0..n as u32).collect();
        let b: Vec<u32> = (n as u32..2 * n as u32).collect();
        let mut buf = GateBuf::new(2 * n as u32);
        let result = emit_lt(&a, &b, &mut buf);
        let stmts: Vec<BIrStmt> = buf.gates.iter().map(|(_, s)| s.clone()).collect();
        let stmt_provs = vec![(); stmts.len()];
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: 2 * n as u32,
                stmts,
                stmt_provs,
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(result)],
                }),
            }],
            pre_init: vec![],
        }
    }

    fn bits_le(x: u32, n: usize) -> Vec<bool> {
        (0..n).map(|i| (x >> i) & 1 == 1).collect()
    }

    /// Build a single-block circuit computing `(a + 1) mod 2^n`, output bit `out_bit`.
    fn build_incr_circuit(n: usize, out_bit: usize) -> BIrBlocks {
        let a: Vec<u32> = (0..n as u32).collect();
        let mut buf = GateBuf::new(n as u32);
        let outs = emit_incr(&a, &mut buf);
        let stmts: Vec<BIrStmt> = buf.gates.iter().map(|(_, s)| s.clone()).collect();
        let stmt_provs = vec![(); stmts.len()];
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: n as u32,
                stmts,
                stmt_provs,
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(outs[out_bit])],
                }),
            }],
            pre_init: vec![],
        }
    }

    #[test]
    fn incr_gadget_exhaustive() {
        for n in 1..=4usize {
            for av in 0..(1u32 << n) {
                let expect = (av + 1) % (1u32 << n);
                for bit in 0..n {
                    let circuit = build_incr_circuit(n, bit);
                    let got = eval(&circuit, &bits_le(av, n));
                    assert_eq!(got, (expect >> bit) & 1 == 1, "incr({av}) bit {bit} n={n}");
                }
            }
        }
    }

    #[test]
    fn lt_gadget_exhaustive() {
        for n in 1..=4usize {
            let circuit = build_lt_circuit(n);
            for av in 0..(1u32 << n) {
                for bv in 0..(1u32 << n) {
                    let mut inputs = bits_le(av, n);
                    inputs.extend(bits_le(bv, n));
                    let got = eval(&circuit, &inputs);
                    assert_eq!(got, lt_ref(av, bv), "lt({av},{bv}) n={n}");
                }
            }
        }
    }

    /// Evaluate every gate once over `inputs` (the `nparams` boundary bits),
    /// returning the full var-id → value map (gate ids are sequential from
    /// `nparams`, so `vals[id]` reads any wire — params or gate result).
    fn eval_all(gates: &[(IRVarId, BIrStmt)], inputs: &[bool], nparams: usize) -> Vec<bool> {
        let mut vals = inputs.to_vec();
        debug_assert_eq!(vals.len(), nparams);
        for (id, stmt) in gates {
            let v = match stmt {
                BIrStmt::Zero => false,
                BIrStmt::One => true,
                BIrStmt::And(a, b) => vals[a.0 as usize] && vals[b.0 as usize],
                BIrStmt::Or(a, b) => vals[a.0 as usize] || vals[b.0 as usize],
                BIrStmt::Xor(a, b) => vals[a.0 as usize] ^ vals[b.0 as usize],
                BIrStmt::Not(a) => !vals[a.0 as usize],
                _ => panic!("unexpected gate in keccak gadget"),
            };
            debug_assert_eq!(id.0 as usize, vals.len(), "gate ids must be sequential");
            vals.push(v);
        }
        vals
    }

    /// Reference SHA3-256 over a bit-vector (LSB-first packing), via the `sha3`
    /// crate — the same bit interface the gadget reproduces.
    fn keccak_ref_bits(input: &[bool]) -> [bool; 256] {
        use digest::Digest;
        let nbytes = input.len().div_ceil(8);
        let mut bytes = vec![0u8; nbytes];
        for (i, &b) in input.iter().enumerate() {
            if b {
                bytes[i / 8] |= 1 << (i % 8);
            }
        }
        let mut h = sha3::Sha3_256::new();
        h.update(&bytes);
        let o = h.finalize();
        core::array::from_fn(|i| (o[i / 8] >> (i % 8)) & 1 == 1)
    }

    #[test]
    fn keccak_gadget_matches_sha3() {
        // Empty, one absorb block, and a multi-byte case.
        for input_bytes in [vec![], vec![0xa5u8, 0x3c], vec![1u8, 2, 3, 4, 5, 6, 7]] {
            let nbits = input_bytes.len() * 8;
            let inbits: Vec<bool> =
                (0..nbits).map(|i| (input_bytes[i / 8] >> (i % 8)) & 1 == 1).collect();
            let params: Vec<u32> = (0..nbits as u32).collect();
            let mut buf = GateBuf::new(nbits as u32);
            let out = emit_keccak256(&params, &mut buf);
            let vals = eval_all(&buf.gates, &inbits, nbits);
            let got: [bool; 256] = core::array::from_fn(|i| vals[out[i] as usize]);
            assert_eq!(got, keccak_ref_bits(&inbits), "keccak gadget mismatch for {nbits} bits");
        }
    }
}
