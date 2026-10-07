//! Fixed-shape TLS import realization: no secret TLS primitive remains an
//! evaluator action, and every import lowers to the established boolar gadget.

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_ir_common::tls13_extern;
use volar_vc::{compile_schedule_optimized, tls13_extern::expand_tls13_oracles};

fn oracle_call(name: &str, args: usize, results: usize) -> BIrBlocks {
    let inputs: Vec<IRVarId> = (0..args as u32).map(IRVarId).collect();
    let mut stmts = Vec::with_capacity(results);
    let mut out = Vec::with_capacity(results);
    for bit in 0..results {
        let id = IRVarId(args as u32 + stmts.len() as u32);
        stmts.push(Node::new(
            BIrStmt::OracleBit {
                name: name.into(),
                args: inputs.clone(),
                bit,
                occurrence: 0,
            },
            (),
            None,
        ));
        out.push(id);
    }
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: args as u32,
            stmts,
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: out,
            }),
        }],
        pre_init: vec![],
    }
}

fn lower_oracle(name: &str, args: usize, results: usize) -> (usize, usize) {
    let circuit = oracle_call(name, args, results);
    let lowered = expand_tls13_oracles(&circuit);
    assert!(
        lowered.blocks[0].stmts.iter().all(|stmt| !matches!(
            stmt.kind,
            BIrStmt::OracleCall { .. }
                | BIrStmt::OracleProjectedBit { .. }
                | BIrStmt::OracleBit { .. }
        )),
        "{name} must be a circuit, not a host action"
    );
    let schedule = compile_schedule_optimized(&lowered).expect("realized TLS oracle schedules");
    assert_eq!(schedule.num_inputs, args);
    assert_eq!(schedule.output_wires().len(), results);
    (lowered.blocks[0].stmts.len(), schedule.and_count())
}

#[test]
fn tls_imports_realize_to_existing_secret_safe_gadgets() {
    let (sha_stmts, sha_ands) = lower_oracle(
        tls13_extern::sha256_64::ORACLE_NAME,
        tls13_extern::sha256_64::MSG_BITS,
        tls13_extern::sha256_64::RESULT_BITS,
    );
    let (hmac_stmts, hmac_ands) = lower_oracle(
        tls13_extern::hmac_sha256_32_32::ORACLE_NAME,
        tls13_extern::hmac_sha256_32_32::ARG_BITS,
        tls13_extern::hmac_sha256_32_32::RESULT_BITS,
    );
    let (kx_stmts, kx_ands) = lower_oracle(
        tls13_extern::x25519_step::ORACLE_NAME,
        tls13_extern::x25519_step::ARG_BITS,
        tls13_extern::x25519_step::RESULT_BITS,
    );

    eprintln!(
        "tls13 oracle lowering: sha256_64 stmts={sha_stmts} ands={sha_ands}; hmac_sha256_32_32 stmts={hmac_stmts} ands={hmac_ands}; x25519_step stmts={kx_stmts} ands={kx_ands}"
    );
    assert!(sha_stmts > 0 && sha_ands > 0);
    assert!(hmac_stmts > sha_stmts && hmac_ands > sha_ands);
    assert!(kx_stmts > hmac_stmts && kx_ands > hmac_ands);
}
