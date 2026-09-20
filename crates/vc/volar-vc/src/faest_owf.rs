//! AX5: the FAEST OWF consistency check — the first consumer of the
//! `aes128_encrypt_block` circuit extern, running identically natively and
//! in-circuit.
//!
//! FAEST's one-way-function statement is `pk = (owf_input, owf_output)` with
//! `sk = owf_key` such that `AES-128(sk, owf_input) = owf_output`; every
//! FAEST signature is (at its core) a proof of VOLE consistency over the AES
//! circuit computing that encryption. This module provides the circuit for
//! the OWF statement itself, built **against the extern** (the
//! [`BIrStmt::OracleBit`] form of the `aes128_encrypt_block` contract), so:
//!
//! - natively, [`faest_owf_check_native`] evaluates the same statement with
//!   volar-spec's byte-level FAEST AES reference;
//! - in-circuit, [`expand_aes_oracles`](crate::aes_extern::expand_aes_oracles)
//!   realizes the extern as the 16k-AND AES gadget and the result is a pure
//!   boolean circuit.
//!
//! The full FAEST *verify* port (SHA-3 transcript, BAVC reconstruct,
//! convert-to-VOLE) composes on top of this statement; that is site P1's
//! `license_check` work. The Keccak-heavy transcript is the dominant cost
//! there — see the plan's open question on an AES-based transcript.

use alloc::vec;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_ir_common::aes_extern;

/// Input layout of [`build_faest_owf_check`]: `key || owf_input || owf_output`
/// — `3 * 128` bits, byte-major LSB-first within each byte (the extern
/// contract's layout).
pub const OWF_INPUT_BITS: usize = 3 * aes_extern::CT_BITS;

/// Build the FAEST OWF check circuit:
/// `ok = (aes128_encrypt_block(key, owf_input) == owf_output)`.
///
/// Params: `[key: 128, owf_input: 128, owf_output: 128]`. Output: `[ok: 1]`.
/// The AES call is emitted as the extern (128 `OracleBit` stmts over the
/// 256-bit `key || owf_input` arg vector) — realize it with
/// [`expand_aes_oracles`](crate::aes_extern::expand_aes_oracles) before
/// concrete evaluation or scheduling.
pub fn build_faest_owf_check() -> BIrBlocks {
    let params = OWF_INPUT_BITS as u32;
    let key = 0..aes_extern::KEY_BITS as u32;
    let pt = (aes_extern::KEY_BITS as u32)..(aes_extern::ARG_BITS as u32);
    let expected = (aes_extern::ARG_BITS as u32)..(OWF_INPUT_BITS as u32);

    let mut stmts: Vec<Node<BIrStmt>> = Vec::new();
    let mut push = |s: BIrStmt| -> IRVarId {
        let id = IRVarId(params + stmts.len() as u32);
        stmts.push(Node::new(s, (), None));
        id
    };

    // The extern call: 128 OracleBit stmts over key || pt.
    let args: Vec<IRVarId> = key.chain(pt).map(IRVarId).collect();
    let mut ct: Vec<IRVarId> = Vec::with_capacity(aes_extern::CT_BITS);
    for bit in 0..aes_extern::CT_BITS {
        ct.push(push(BIrStmt::OracleBit {
            name: aes_extern::ORACLE_NAME.into(),
            args: args.clone(),
            bit,
            occurrence: bit as u64,
        }));
    }

    // ok = AND_b !(ct[b] ^ expected[b])
    let mut acc: Option<IRVarId> = None;
    for (i, exp) in expected.enumerate() {
        let x = push(BIrStmt::Xor(ct[i], IRVarId(exp)));
        let nx = push(BIrStmt::Not(x));
        acc = Some(match acc {
            None => nx,
            Some(a) => push(BIrStmt::And(a, nx)),
        });
    }
    let ok = acc.expect("128 bits compared");

    BIrBlocks {
        blocks: vec![BIrBlock {
            params,
            stmts,
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: alloc::vec![ok],
            }),
        }],
        pre_init: vec![],
    }
}

/// The native twin of [`build_faest_owf_check`]: the same statement evaluated
/// with volar-spec's byte-level AES-128 reference.
pub fn faest_owf_check_native(key: &[u8; 16], owf_input: &[u8; 16], owf_output: &[u8; 16]) -> bool {
    volar_spec::faest::aes::encrypt_block(key, owf_input) == *owf_output
}
