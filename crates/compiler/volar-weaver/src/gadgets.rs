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
}
