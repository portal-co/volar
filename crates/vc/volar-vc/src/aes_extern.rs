//! AX4: schedule-time realization of the `aes128_encrypt_block` circuit
//! extern (the `volar_ir_common::aes_extern` contract).
//!
//! Frontends surface AES as `BIrStmt::OracleBit` calls named
//! [`aes_extern::ORACLE_NAME`]; this pass inlines the fixed-length AES boolar
//! gadget ([`crate::aes_gadget::build_aes128`]) per call site, leaving a pure
//! boolean circuit any downstream consumer (concrete eval, scheduling,
//! garbling) handles without knowing the extern exists.
//!
//! Call-site shape (what the IR→boolar lowering emits for one
//! `IRStmt::OracleCall`): `CT_BITS` consecutive `OracleBit` stmts sharing one
//! 256-wide `args` vector, with `bit` ascending `0..CT_BITS`. The pass groups
//! on the exact (remapped) arg vector, inlining the gadget once per group and
//! mapping output bit `b` to the gadget's output wire `b`. Oracles with any
//! other name pass through untouched.

use alloc::collections::BTreeMap;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_ir_common::aes_extern;

use crate::aes_gadget::build_aes128;

/// Replace every `aes128_encrypt_block` oracle call in `blocks` with an
/// inlined copy of the AES-128 boolar gadget.
///
/// Panics on a malformed call site: a first-seen `bit` other than 0, a break
/// in the ascending bit sequence, an arg vector that is not
/// [`aes_extern::ARG_BITS`] wide, or a call site left with fewer than
/// `CT_BITS` bits at the end of the block.
pub fn expand_aes_oracles<P: Clone>(blocks: &BIrBlocks<P>) -> BIrBlocks<P> {
    // Build the gadget once; every call site inlines the same circuit.
    let gadget = build_aes128();
    let gadget_block = &gadget.blocks[0];
    assert_eq!(gadget.blocks.len(), 1, "aes gadget is single-block");
    assert_eq!(gadget_block.params as usize, aes_extern::ARG_BITS);

    BIrBlocks {
        blocks: blocks
            .blocks
            .iter()
            .map(|b| expand_block(b, gadget_block))
            .collect(),
        pre_init: blocks.pre_init.clone(),
    }
}

fn expand_block<P: Clone>(block: &BIrBlock<P>, gadget_block: &BIrBlock) -> BIrBlock<P> {
    let params = block.params;
    let mut stmts: Vec<Node<BIrStmt, P>> = Vec::with_capacity(block.stmts.len());
    // old var id -> new var id (identity for params).
    let mut remap: Vec<u32> = (0..params).map(|i| i as u32).collect();
    // Open call sites: remapped arg vector -> (gadget output wires, next bit).
    let mut open: BTreeMap<Vec<u32>, (Vec<u32>, usize)> = BTreeMap::new();

    for node in &block.stmts {
        if let BIrStmt::OracleBit { name, args, bit, .. } = &node.kind {
            if name == aes_extern::ORACLE_NAME {
                let args: Vec<u32> = args.iter().map(|a| remap[a.0 as usize]).collect();
                assert_eq!(
                    args.len(),
                    aes_extern::ARG_BITS,
                    "aes128_encrypt_block: arg vector must be {} bits",
                    aes_extern::ARG_BITS
                );
                let wire = match open.get_mut(&args) {
                    Some((outputs, next)) => {
                        assert_eq!(
                            *next, *bit,
                            "aes128_encrypt_block: broken output-bit sequence"
                        );
                        *next += 1;
                        outputs[*bit]
                    }
                    None => {
                        assert_eq!(
                            *bit, 0,
                            "aes128_encrypt_block: first output bit of a call must be bit 0"
                        );
                        let outputs =
                            inline_gadget(&mut stmts, params, gadget_block, &args, node.prov.clone());
                        let out0 = outputs[0];
                        open.insert(args, (outputs, 1));
                        out0
                    }
                };
                // The oracle bit's own var aliases the gadget output wire: no
                // new stmt, just extend the remap.
                remap.push(wire);
                continue;
            }
        }
        let kind: BIrStmt = remap_host_stmt(&node.kind, &remap);
        let id = params + stmts.len() as u32;
        stmts.push(Node::new(kind, node.prov.clone(), node.side));
        remap.push(id);
    }

    // Seal any call site left mid-sequence.
    for (_, (_, next)) in &open {
        assert_eq!(
            *next,
            aes_extern::CT_BITS,
            "aes128_encrypt_block: call site incomplete ({next} of {} bits)",
            aes_extern::CT_BITS
        );
    }

    let r = |v: &IRVarId| IRVarId(remap[v.0 as usize]);
    let terminator = match &block.terminator {
        BIrTerminator::Jmp(t) => BIrTerminator::Jmp(BIrTarget {
            block: t.block.clone(),
            args: t.args.iter().map(r).collect(),
        }),
        BIrTerminator::CondJmp {
            val,
            then_target,
            else_target,
        } => BIrTerminator::CondJmp {
            val: r(val),
            then_target: BIrTarget {
                block: then_target.block.clone(),
                args: then_target.args.iter().map(r).collect(),
            },
            else_target: BIrTarget {
                block: else_target.block.clone(),
                args: else_target.args.iter().map(r).collect(),
            },
        },
        // `BIrTerminator` is `#[non_exhaustive]`; future variants pass
        // through unmodified only if they carry no vars, which cannot be
        // checked here — so fail loudly instead.
        other => panic!("expand_aes_oracles: unsupported terminator {other:?}"),
    };

    BIrBlock {
        params,
        stmts,
        terminator,
    }
}

/// Inline one copy of the AES gadget into the block being rebuilt, feeding
/// gadget param `i` from host wire `args[i]`. Returns the gadget's
/// `CT_BITS` output wires (host var ids). Inlined gates carry the first
/// `OracleBit`'s provenance; side metadata is dropped (the gadget is shared
/// infrastructure whose inputs' sides already flowed through the host's own
/// gates).
fn inline_gadget<P: Clone>(
    stmts: &mut Vec<Node<BIrStmt, P>>,
    params: u32,
    gadget_block: &BIrBlock,
    args: &[u32],
    prov: P,
) -> Vec<u32> {
    // Gadget var ids: params 0..ARG_BITS map to `args`; gadget stmt i maps to
    // the next free host wire, `params + stmts.len()`.
    let mut remap: Vec<u32> =
        Vec::with_capacity(gadget_block.params as usize + gadget_block.stmts.len());
    remap.extend_from_slice(args);
    for stmt in &gadget_block.stmts {
        let kind = remap_pure(&stmt.kind, &remap);
        let id = params + stmts.len() as u32;
        stmts.push(Node::new(kind, prov.clone(), None));
        remap.push(id);
    }
    match &gadget_block.terminator {
        BIrTerminator::Jmp(t) if t.block == IRBlockTargetId::Return => {
            t.args.iter().map(|a| remap[a.0 as usize]).collect()
        }
        _ => panic!("aes gadget must end in a Return"),
    }
}

/// Remap a gadget stmt's var references. The AES gadget is pure boolean, so
/// only the boolean variants are supported (any other stmt is a gadget bug,
/// not a host-program bug).
fn remap_pure(s: &BIrStmt, remap: &[u32]) -> BIrStmt {
    let r = |v: &IRVarId| IRVarId(remap[v.0 as usize]);
    match s {
        BIrStmt::Zero => BIrStmt::Zero,
        BIrStmt::One => BIrStmt::One,
        BIrStmt::And(a, b) => BIrStmt::And(r(a), r(b)),
        BIrStmt::Or(a, b) => BIrStmt::Or(r(a), r(b)),
        BIrStmt::Xor(a, b) => BIrStmt::Xor(r(a), r(b)),
        BIrStmt::Not(a) => BIrStmt::Not(r(a)),
        other => panic!("aes gadget inlining: unsupported stmt {other:?}"),
    }
}

/// Remap every var reference of a host stmt, covering the full `BIrStmt`
/// surface (non-AES oracles, actions, RNG, and storage all pass through).
fn remap_host_stmt(s: &BIrStmt, remap: &[u32]) -> BIrStmt {
    let r = |v: &IRVarId| IRVarId(remap[v.0 as usize]);
    match s {
        BIrStmt::Zero => BIrStmt::Zero,
        BIrStmt::One => BIrStmt::One,
        BIrStmt::And(a, b) => BIrStmt::And(r(a), r(b)),
        BIrStmt::Or(a, b) => BIrStmt::Or(r(a), r(b)),
        BIrStmt::Xor(a, b) => BIrStmt::Xor(r(a), r(b)),
        BIrStmt::Not(a) => BIrStmt::Not(r(a)),
        BIrStmt::OracleCall { name, args, num_bits } => BIrStmt::OracleCall {
            name: name.clone(),
            args: args.iter().map(r).collect(),
            num_bits: *num_bits,
        },
        BIrStmt::OracleBit {
            name,
            args,
            bit,
            occurrence,
        } => BIrStmt::OracleBit {
            name: name.clone(),
            args: args.iter().map(r).collect(),
            bit: *bit,
            occurrence: *occurrence,
        },
        BIrStmt::OracleProjectedBit { call, bit } => BIrStmt::OracleProjectedBit {
            call: r(call),
            bit: *bit,
        },
        BIrStmt::ActionCall {
            name,
            guard,
            args,
            fallback,
            num_bits,
        } => BIrStmt::ActionCall {
            name: name.clone(),
            guard: r(guard),
            args: args.iter().map(r).collect(),
            fallback: fallback.iter().map(r).collect(),
            num_bits: *num_bits,
        },
        BIrStmt::ActionBit { call, bit } => BIrStmt::ActionBit {
            call: r(call),
            bit: *bit,
        },
        BIrStmt::ActionStoreBit {
            name,
            guard,
            args,
            fallback,
            storage,
            lane,
            addr,
            bit,
            occurrence,
        } => BIrStmt::ActionStoreBit {
            name: name.clone(),
            guard: r(guard),
            args: args.iter().map(r).collect(),
            fallback: r(fallback),
            storage: *storage,
            lane: *lane,
            addr: addr.iter().map(r).collect(),
            bit: *bit,
            occurrence: *occurrence,
        },
        BIrStmt::Rng { name } => BIrStmt::Rng { name: name.clone() },
        BIrStmt::RngBit {
            name,
            bit,
            occurrence,
        } => BIrStmt::RngBit {
            name: name.clone(),
            bit: *bit,
            occurrence: *occurrence,
        },
        BIrStmt::StorageRead { storage, lane, addr } => BIrStmt::StorageRead {
            storage: *storage,
            lane: *lane,
            addr: addr.iter().map(r).collect(),
        },
        BIrStmt::StorageWrite {
            storage,
            lane,
            src,
            addr,
        } => BIrStmt::StorageWrite {
            storage: *storage,
            lane: *lane,
            src: r(src),
            addr: addr.iter().map(r).collect(),
        },
        other => panic!("expand_aes_oracles: unsupported stmt {other:?}"),
    }
}
