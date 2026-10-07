// @reliability: experimental
// @ai: assisted
//! AX4: `expand_aes_oracles` realizes the `aes128_encrypt_block` extern by
//! inlining the AES boolar gadget per call site. The tests build the exact
//! boolar shape the IR→boolar lowering emits (128 consecutive
//! `BIrStmt::OracleBit` stmts sharing one 256-wide arg vector per call),
//! expand, and evaluate concretely against the FIPS-197 KATs via
//! `volar_fuzz::interpreter::biir::eval_biir`.

use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_ir_common::aes_extern;
use volar_vc::aes_extern::expand_aes_oracles;

const KEY: [u8; 16] = [
    0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c,
];
const PT: [u8; 16] = [
    0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37, 0x07, 0x34,
];
const CT: [u8; 16] = [
    0x39, 0x25, 0x84, 0x1d, 0x02, 0xdc, 0x09, 0xfb, 0xdc, 0x11, 0x85, 0x97, 0x19, 0x6a, 0x0b, 0x32,
];

fn bits_of_bytes(bytes: &[u8; 16]) -> Vec<bool> {
    let mut v = Vec::with_capacity(128);
    for b in bytes {
        for j in 0..8 {
            v.push((b >> j) & 1 == 1);
        }
    }
    v
}

fn bytes_of_bits(bits: &[bool]) -> [u8; 16] {
    let mut out = [0u8; 16];
    for i in 0..16 {
        let mut b = 0u8;
        for j in 0..8 {
            if bits[i * 8 + j] {
                b |= 1 << j;
            }
        }
        out[i] = b;
    }
    out
}

/// Append the lowering's exact call-site shape for one
/// `aes128_encrypt_block(key_wires, pt_wires)` call: 128 consecutive
/// `OracleBit` stmts; returns their result var ids.
fn emit_aes_call(
    stmts: &mut Vec<Node<BIrStmt>>,
    params: u32,
    arg_wires: &[IRVarId],
) -> Vec<IRVarId> {
    assert_eq!(arg_wires.len(), aes_extern::ARG_BITS);
    let mut outs = Vec::with_capacity(aes_extern::CT_BITS);
    for bit in 0..aes_extern::CT_BITS {
        let id = IRVarId(params + stmts.len() as u32);
        stmts.push(Node::new(
            BIrStmt::OracleBit {
                name: aes_extern::ORACLE_NAME.into(),
                args: arg_wires.to_vec(),
                bit,
                occurrence: bit as u64,
            },
            (),
            None,
        ));
        outs.push(id);
    }
    outs
}

fn single_block(params: u32, stmts: Vec<Node<BIrStmt>>, outputs: Vec<IRVarId>) -> BIrBlocks {
    BIrBlocks {
        blocks: vec![BIrBlock {
            params,
            stmts,
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: outputs,
            }),
        }],
        pre_init: vec![],
    }
}

#[test]
fn aes_extern_expands_to_gadget_and_matches_fips197() {
    // params: key[128] ++ pt[128]; one oracle call; return its 128 bits.
    let params = aes_extern::ARG_BITS as u32;
    let args: Vec<IRVarId> = (0..params).map(IRVarId).collect();
    let mut stmts = Vec::new();
    let outs = emit_aes_call(&mut stmts, params, &args);
    let host = single_block(params, stmts, outs);

    let expanded = expand_aes_oracles(&host);
    assert!(
        expanded.blocks[0]
            .stmts
            .iter()
            .all(|s| !matches!(&s.kind, BIrStmt::OracleBit { .. })),
        "no oracle bits remain after expansion"
    );

    let mut inp = bits_of_bytes(&KEY);
    inp.extend_from_slice(&bits_of_bytes(&PT));
    let out = eval_biir(&expanded, &inp).expect("expanded circuit evaluates");
    assert_eq!(bytes_of_bits(&out), CT, "FIPS-197 appendix B");
}

#[test]
fn aes_extern_two_call_sites_group_independently() {
    // Two calls with different args: pt' = aes(key, pt), out = aes(key, pt')
    // XOR'd against the first ciphertext (a cheap endomorphism check).
    let params = aes_extern::ARG_BITS as u32;
    let args: Vec<IRVarId> = (0..params).map(IRVarId).collect();
    let mut stmts: Vec<Node<BIrStmt>> = Vec::new();
    let ct1 = emit_aes_call(&mut stmts, params, &args);

    // second call: same key, pt = ct1 (chained)
    let mut args2: Vec<IRVarId> = (0..128).map(IRVarId).collect();
    args2.extend_from_slice(&ct1);
    let ct2 = emit_aes_call(&mut stmts, params, &args2);

    // out = ct1 XOR ct2
    let mut outs = Vec::with_capacity(128);
    for i in 0..128 {
        let id = IRVarId(params + stmts.len() as u32);
        stmts.push(Node::new(BIrStmt::Xor(ct1[i], ct2[i]), (), None));
        outs.push(id);
    }
    let host = single_block(params, stmts, outs);
    let expanded = expand_aes_oracles(&host);

    let mut inp = bits_of_bytes(&KEY);
    inp.extend_from_slice(&bits_of_bytes(&PT));
    let out = eval_biir(&expanded, &inp).expect("expanded circuit evaluates");

    // Reference: ct2 = AES(key, ct1)
    let ct2_ref = volar_spec::faest::aes::encrypt_block(&KEY, &CT);
    let want: Vec<bool> = bits_of_bytes(&CT)
        .iter()
        .zip(bits_of_bytes(&ct2_ref).iter())
        .map(|(a, b)| a ^ b)
        .collect();
    assert_eq!(out, want, "chained AES matches the scalar reference");
}

#[test]
fn other_oracles_pass_through() {
    // A differently-named oracle is left untouched.
    let params = 4u32;
    let args: Vec<IRVarId> = (0..params).map(IRVarId).collect();
    let stmts: Vec<Node<BIrStmt>> = (0..2)
        .map(|bit| {
            Node::new(
                BIrStmt::OracleBit {
                    name: "someone_else".into(),
                    args: args.clone(),
                    bit,
                    occurrence: bit as u64,
                },
                (),
                None,
            )
        })
        .collect();
    let host = single_block(params, stmts, vec![IRVarId(params), IRVarId(params + 1)]);
    let expanded = expand_aes_oracles(&host);
    assert_eq!(expanded.blocks[0].stmts.len(), 2, "untouched");
    assert!(
        matches!(&expanded.blocks[0].stmts[0].kind, BIrStmt::OracleBit { name, .. } if name == "someone_else")
    );
}
