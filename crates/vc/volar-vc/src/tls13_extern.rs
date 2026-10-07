//! Realization of the fixed-shape TLS 1.3 circuit imports.
//!
//! The imports in [`volar_ir_common::tls13_extern`] are pure circuit oracles,
//! never strict actions: actions disclose decoded arguments to an evaluator
//! host, while these arguments include TLS traffic secrets. This pass replaces
//! each complete oracle-bit group with the existing SHA-256/HMAC/X25519 boolar
//! gadget. A strict-chain caller feeds each gadget from `ChainFeed::Held` and
//! retains its outputs with `ChainOut::Hold`, so neither party can decode the
//! derived key material.
//!
//! @pinnedness: unpinned
//! @stability: very-unstable
//! @ai: assisted

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_ir_common::tls13_extern;

use crate::sha_gadget::{build_hmac_sha256, build_sha256};
use crate::x25519_gadget::build_x25519_step;

/// Replace complete fixed-shape TLS oracle calls with their boolar gadgets.
/// Unknown oracles are retained unchanged for their own realization pass.
pub fn expand_tls13_oracles<P: Clone>(blocks: &BIrBlocks<P>) -> BIrBlocks<P> {
    BIrBlocks {
        blocks: blocks.blocks.iter().map(expand_block).collect(),
        pre_init: blocks.pre_init.clone(),
    }
}

#[derive(Clone, Copy)]
struct Contract {
    args: usize,
    results: usize,
    build: fn() -> BIrBlocks,
}

fn contract(name: &str) -> Option<Contract> {
    match name {
        tls13_extern::sha256_64::ORACLE_NAME => Some(Contract {
            args: tls13_extern::sha256_64::MSG_BITS,
            results: tls13_extern::sha256_64::RESULT_BITS,
            build: || build_sha256(64),
        }),
        tls13_extern::hmac_sha256_32_32::ORACLE_NAME => Some(Contract {
            args: tls13_extern::hmac_sha256_32_32::ARG_BITS,
            results: tls13_extern::hmac_sha256_32_32::RESULT_BITS,
            build: || build_hmac_sha256(32, 32),
        }),
        tls13_extern::x25519_step::ORACLE_NAME => Some(Contract {
            args: tls13_extern::x25519_step::ARG_BITS,
            results: tls13_extern::x25519_step::RESULT_BITS,
            build: build_x25519_step,
        }),
        _ => None,
    }
}

fn expand_block<P: Clone>(block: &BIrBlock<P>) -> BIrBlock<P> {
    let params = block.params;
    let mut stmts = Vec::with_capacity(block.stmts.len());
    let mut remap: Vec<u32> = (0..params).collect();
    let mut calls: BTreeMap<(String, Vec<u32>), (Vec<u32>, usize, usize)> = BTreeMap::new();

    for node in &block.stmts {
        if let BIrStmt::OracleBit {
            name, args, bit, ..
        } = &node.kind
        {
            if let Some(spec) = contract(name) {
                let args: Vec<u32> = args.iter().map(|arg| remap[arg.0 as usize]).collect();
                assert_eq!(args.len(), spec.args, "{name}: wrong argument width");
                let key = (name.clone(), args.clone());
                let wire = match calls.get_mut(&key) {
                    Some((outputs, next, results)) => {
                        assert_eq!(*results, spec.results, "{name}: inconsistent call shape");
                        assert_eq!(*next, *bit as usize, "{name}: broken output-bit sequence");
                        *next += 1;
                        outputs[*bit as usize]
                    }
                    None => {
                        assert_eq!(*bit, 0, "{name}: first output bit must be zero");
                        let gadget = (spec.build)();
                        let outputs =
                            inline_gadget(&mut stmts, params, &gadget, &args, node.prov.clone());
                        assert_eq!(outputs.len(), spec.results, "{name}: gadget result width");
                        let out0 = outputs[0];
                        calls.insert(key, (outputs, 1, spec.results));
                        out0
                    }
                };
                remap.push(wire);
                continue;
            }
        }

        let id = params + stmts.len() as u32;
        stmts.push(Node::new(
            remap_stmt(&node.kind, &remap),
            node.prov.clone(),
            node.side,
        ));
        remap.push(id);
    }

    for ((name, _), (_, next, results)) in calls {
        assert_eq!(
            next, results,
            "{name}: incomplete call ({next} of {results})"
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
        other => panic!("expand_tls13_oracles: unsupported terminator {other:?}"),
    };

    BIrBlock {
        params,
        stmts,
        terminator,
    }
}

fn inline_gadget<P: Clone>(
    stmts: &mut Vec<Node<BIrStmt, P>>,
    params: u32,
    gadget: &BIrBlocks,
    args: &[u32],
    prov: P,
) -> Vec<u32> {
    assert_eq!(
        gadget.blocks.len(),
        1,
        "TLS oracle gadget must be one block"
    );
    let block = &gadget.blocks[0];
    assert_eq!(block.params as usize, args.len(), "TLS oracle gadget ABI");
    let mut remap = args.to_vec();
    for stmt in &block.stmts {
        let id = params + stmts.len() as u32;
        stmts.push(Node::new(
            remap_stmt(&stmt.kind, &remap),
            prov.clone(),
            None,
        ));
        remap.push(id);
    }
    match &block.terminator {
        BIrTerminator::Jmp(target) if target.block == IRBlockTargetId::Return => target
            .args
            .iter()
            .map(|arg| remap[arg.0 as usize])
            .collect(),
        _ => panic!("TLS oracle gadget must return"),
    }
}

fn remap_stmt(stmt: &BIrStmt, remap: &[u32]) -> BIrStmt {
    let r = |v: &IRVarId| IRVarId(remap[v.0 as usize]);
    match stmt {
        BIrStmt::Zero => BIrStmt::Zero,
        BIrStmt::One => BIrStmt::One,
        BIrStmt::And(a, b) => BIrStmt::And(r(a), r(b)),
        BIrStmt::Or(a, b) => BIrStmt::Or(r(a), r(b)),
        BIrStmt::Xor(a, b) => BIrStmt::Xor(r(a), r(b)),
        BIrStmt::Not(a) => BIrStmt::Not(r(a)),
        other => panic!("TLS oracle gadget contains unsupported statement {other:?}"),
    }
}
