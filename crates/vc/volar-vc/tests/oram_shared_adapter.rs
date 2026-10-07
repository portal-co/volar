//! The shared-key adapter executes an encrypted ORAM program without ever
//! constructing the 128-bit AES tree key in either party's driver state.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator, LaneId};
use volar_ir::ir::{IRBlockTargetId, IRVarId, StorageId};
use volar_ir_common::Node;
use volar_mpc::ot::LoopbackOt;
use volar_oram::OramTree;
use volar_spec::garble::GlobalSecret;
use volar_vc::oram_2pc::{SharedKeyOramAdapter, SharedOramKey};
use volar_vc::oram_lower::{OramLowerConfig, storage_to_oram};

const LEVELS: usize = 2;
const Z: usize = 2;
const MAX_STASH: usize = 6;
type N = U16;
type D = Sha256;

fn guest() -> BIrBlocks<()> {
    let storage = StorageId(0);
    let lane = LaneId(0);
    let (address, value) = (IRVarId(0), IRVarId(1));
    let read = IRVarId(3);
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: 2,
            stmts: vec![
                Node::new(
                    BIrStmt::StorageWrite {
                        storage,
                        lane,
                        src: value,
                        addr: vec![address],
                    },
                    (),
                    None,
                ),
                Node::new(
                    BIrStmt::StorageRead {
                        storage,
                        lane,
                        addr: vec![address],
                    },
                    (),
                    None,
                ),
            ],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![read],
            }),
        }],
        pre_init: vec![],
    }
}

#[test]
fn split_key_adapter_runs_encrypted_oram_without_full_key() {
    let program = storage_to_oram(
        &guest(),
        &OramLowerConfig {
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
            secure: true,
            shared_tree_key: true,
            narrow_bits: None,
        },
    )
    .expect("lowers guest storage to ORAM");
    assert!(program.oram.encrypted);
    // Shared-key mode retains circuit-only (non-versioned, lazy) initialization.
    assert!(!program.oram.encrypt_valid && !program.oram.versioned_pads);

    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x7au8; 16]));
    let key = SharedOramKey::new(*b"garbkey1", *b"evalkey2");
    let mut driver = SharedKeyOramAdapter::<N>::new::<D>(&program.oram, secret, key);
    assert!(driver.uses_shared_key());

    let mut tree = OramTree::<Z, 1>::new(LEVELS);
    let mut ot = LoopbackOt::<N>::new();
    let inputs = [true, true];
    let params = inputs
        .iter()
        .map(|&bit| driver.fresh_input::<D>(bit))
        .collect::<Vec<_>>();
    let out = driver.run_program::<D, Z>(&program, &params, &mut tree, &mut ot);
    let result = out[0].0.open(&out[0].1)[0] & 1 != 0;
    assert!(result, "read observes the just-written secret bit");

    assert!(
        tree.buckets
            .iter()
            .flat_map(|bucket| bucket.entries.iter())
            .flat_map(|entry| entry.data)
            .any(|byte| byte != 0),
        "external tree stores ciphertext after the access"
    );
}
