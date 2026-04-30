// @reliability: experimental
// @ai: assisted
//! Round-trip tests for `volar-ir-text`.

#![cfg(all(test, feature = "parse"))]
extern crate std;

use std::collections::BTreeMap;
use std::vec;

use volar_ir_common::{
    ActionDecl, Constant, IrType, OracleDecl, RngDecl, StorageId, Stmt, Type, TypeId, TypeTable,
};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRTerminator, IRVarId,
};
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};

use crate::{ParseText, WriteText, SavedIrBlocks, SavedBIrBlocks};

// ============================================================================
// Helper builders
// ============================================================================

fn ty(n: u32) -> TypeId { TypeId(n) }
fn v(n: u32) -> IRVarId { IRVarId(n) }
fn storage(n: u32) -> StorageId { StorageId(n) }
fn c(hi: u128, lo: u128) -> Constant { Constant { hi, lo } }
fn block_id(n: u32) -> IRBlockId { IRBlockId(n) }

fn simple_ir_module() -> SavedIrBlocks {
    // Type table: 0=bit, 1=u8, 2=vec(4,u8)
    let types = TypeTable(vec![
        IrType::Primitive(Type::Bit),
        IrType::Primitive(Type::_8),
        IrType::Vec(4, ty(1)),
        IrType::Tuple(vec![ty(0), ty(1)]),
        IrType::Block { params: vec![ty(0)] },
        IrType::Func { params: vec![ty(0), ty(1)], results: vec![ty(1)] },
    ]);

    // One block: param v0:bit, v1 = const 0:255 ty=1, jmp return args=[v1]
    let block = IRBlock {
        params:     vec![ty(0)],
        stmts:      vec![Stmt::Const(c(0, 255), ty(1))],
        stmt_provs: vec![()],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(1)] },
    };

    SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block] },
    }
}

fn round_trip_ir(m: SavedIrBlocks) {
    let text = m.to_text_string();
    let parsed = SavedIrBlocks::parse_text(&text).unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let text2 = parsed.to_text_string();
    assert_eq!(text, text2, "IR round-trip text mismatch");
}

fn round_trip_bir(m: SavedBIrBlocks) {
    let text = m.to_text_string();
    let parsed = SavedBIrBlocks::parse_text(&text).unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let text2 = parsed.to_text_string();
    assert_eq!(text, text2, "BIR round-trip text mismatch");
}

// ============================================================================
// IR tests
// ============================================================================

#[test]
fn ir_type_table() {
    let types = TypeTable(vec![
        IrType::Primitive(Type::Bit),
        IrType::Primitive(Type::_8),
        IrType::Primitive(Type::_16),
        IrType::Primitive(Type::_32),
        IrType::Primitive(Type::_64),
        IrType::Primitive(Type::_128),
        IrType::Primitive(Type::_256),
        IrType::Primitive(Type::AES8),
        IrType::Primitive(Type::Galois64),
        IrType::Vec(8, ty(0)),
        IrType::Tuple(vec![ty(0), ty(1)]),
        IrType::Block { params: vec![ty(0), ty(1)] },
        IrType::Func { params: vec![ty(0)], results: vec![ty(1)] },
    ]);
    let m = SavedIrBlocks {
        types,
        blocks: IRBlocks {
            oracles: vec![], actions: vec![], rngs: vec![],
            blocks: vec![IRBlock {
                params:     vec![],
                stmts:      vec![],
                stmt_provs: vec![],
                terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![] },
            }],
        },
    };
    round_trip_ir(m);
}

#[test]
fn ir_simple_round_trip() {
    round_trip_ir(simple_ir_module());
}

#[test]
fn ir_decls() {
    let types = TypeTable(vec![
        IrType::Primitive(Type::Bit),
        IrType::Primitive(Type::_8),
    ]);
    let m = SavedIrBlocks {
        types,
        blocks: IRBlocks {
            oracles: vec![OracleDecl { name: "my_oracle".into(), params: vec![ty(0)], results: vec![ty(1)] }],
            actions: vec![ActionDecl { name: "my_action".into(), params: vec![ty(1)], results: vec![ty(0)] }],
            rngs:    vec![RngDecl { name: "my_rng".into(), ty: ty(0) }],
            blocks: vec![IRBlock {
                params:     vec![],
                stmts:      vec![],
                stmt_provs: vec![],
                terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![] },
            }],
        },
    };
    round_trip_ir(m);
}

#[test]
fn ir_stmt_storage_read_write() {
    let types = TypeTable(vec![IrType::Primitive(Type::_8)]);
    let block = IRBlock {
        params:     vec![ty(0)],  // v0
        stmts:      vec![
            // v1 = storage_read storage=0 ty=0 addr=v0
            Stmt::StorageRead { storage: storage(0), ty: ty(0), addr: v(0) },
            // v2 = storage_write storage=1 src=v1 ty=0 addr=v0
            Stmt::StorageWrite { storage: storage(1), src: v(1), ty: ty(0), addr: v(0) },
        ],
        stmt_provs: vec![(), ()],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(1)] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block] },
    });
}

#[test]
fn ir_stmt_transmute() {
    let types = TypeTable(vec![IrType::Primitive(Type::Bit), IrType::Primitive(Type::_8)]);
    let block = IRBlock {
        params:     vec![ty(0)],
        stmts:      vec![Stmt::Transmute { src: v(0), src_ty: ty(0), dst_ty: ty(1) }],
        stmt_provs: vec![()],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(1)] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block] },
    });
}

#[test]
fn ir_stmt_poly() {
    let types = TypeTable(vec![IrType::Primitive(Type::Bit)]);
    let mut coeffs = BTreeMap::new();
    coeffs.insert(vec![v(0), v(1)], 1u8);
    coeffs.insert(vec![v(0)], 1u8);
    let block = IRBlock {
        params:     vec![ty(0), ty(0)],  // v0, v1
        stmts:      vec![Stmt::Poly { ty: ty(0), coeffs, constant: c(0, 0) }],
        stmt_provs: vec![()],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(2)] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block] },
    });
}

#[test]
fn ir_stmt_rot_merge_splat_shuffle() {
    let types = TypeTable(vec![IrType::Primitive(Type::_8)]);
    let block = IRBlock {
        params:     vec![ty(0), ty(0)],  // v0, v1
        stmts:      vec![
            Stmt::Rol { src: v(0), ty: ty(0), n: 3 },                            // v2
            Stmt::Ror { src: v(1), ty: ty(0), n: 2 },                            // v3
            Stmt::Merge { parts: vec![v(0), v(1)], ty: ty(0) },                  // v4
            Stmt::Splat { src: v(0), ty: ty(0) },                                // v5
            Stmt::Shuffle { result_bits: vec![(0, v(0)), (1, v(1))], ty: ty(0) }, // v6
        ],
        stmt_provs: vec![(), (), (), (), ()],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(6)] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block] },
    });
}

#[test]
fn ir_stmt_oracle_action_rng() {
    let types = TypeTable(vec![IrType::Primitive(Type::Bit), IrType::Primitive(Type::_8)]);
    let block = IRBlock {
        params:     vec![ty(0), ty(0)],  // v0, v1
        stmts:      vec![
            // v2 = oracle_call "oc" args=[v0] out_tys=[0] result_ty=1
            Stmt::OracleCall {
                name: "oc".into(), args: vec![v(0)],
                output_tys: vec![ty(0)], result_ty: ty(1),
            },
            // v3 = oracle_output call=v2 idx=0 ty=0
            Stmt::OracleOutput { call: v(2), idx: 0, ty: ty(0) },
            // v4 = action_call "ac" guard=v0 args=[v1] fallbacks=[v0] out_tys=[0] result_ty=1
            Stmt::ActionCall {
                name: "ac".into(), guard: v(0), args: vec![v(1)],
                fallbacks: vec![v(0)], output_tys: vec![ty(0)], result_ty: ty(1),
            },
            // v5 = action_output call=v4 idx=0 ty=0
            Stmt::ActionOutput { call: v(4), idx: 0, ty: ty(0) },
            // v6 = rng "rng1" ty=0
            Stmt::Rng { name: "rng1".into(), ty: ty(0) },
        ],
        stmt_provs: vec![(), (), (), (), ()],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(6)] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block] },
    });
}

#[test]
fn ir_terminator_jmp_cond() {
    let types = TypeTable(vec![IrType::Primitive(Type::Bit), IrType::Primitive(Type::_8)]);
    let block0 = IRBlock {
        params:     vec![ty(0)],   // v0
        stmts:      vec![],
        stmt_provs: vec![],
        terminator: IRTerminator::JumpCond {
            condition: v(0),
            true_block:  IRBlockTargetId::Block(block_id(1)),
            true_args:   vec![v(0)],
            false_block: IRBlockTargetId::Return,
            false_args:  vec![],
        },
    };
    let block1 = IRBlock {
        params:     vec![ty(0)],
        stmts:      vec![],
        stmt_provs: vec![],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(0)] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block0, block1] },
    });
}

#[test]
fn ir_terminator_jmp_table() {
    let types = TypeTable(vec![IrType::Primitive(Type::_8)]);
    let mut cases = BTreeMap::new();
    cases.insert(c(0, 1), (IRBlockTargetId::Block(block_id(1)), vec![]));
    cases.insert(c(0, 2), (IRBlockTargetId::Return, vec![]));
    let block0 = IRBlock {
        params:     vec![ty(0)],
        stmts:      vec![],
        stmt_provs: vec![],
        terminator: IRTerminator::JumpTable { index: v(0), cases },
    };
    let block1 = IRBlock {
        params:     vec![],
        stmts:      vec![],
        stmt_provs: vec![],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block0, block1] },
    });
}

#[test]
fn ir_string_escaping() {
    let types = TypeTable(vec![IrType::Primitive(Type::Bit)]);
    let block = IRBlock {
        params:     vec![],
        stmts:      vec![Stmt::Rng { name: "a\"b\\c\nd".into(), ty: ty(0) }],
        stmt_provs: vec![()],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(0)] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block] },
    });
}

#[test]
fn ir_constant_large() {
    let types = TypeTable(vec![IrType::Primitive(Type::_128)]);
    let big = c(0xdeadbeef_cafebabe_12345678_90abcdef_u128, u128::MAX);
    let block = IRBlock {
        params:     vec![],
        stmts:      vec![Stmt::Const(big, ty(0))],
        stmt_provs: vec![()],
        terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![v(0)] },
    };
    round_trip_ir(SavedIrBlocks {
        types,
        blocks: IRBlocks { oracles: vec![], actions: vec![], rngs: vec![], blocks: vec![block] },
    });
}

// ============================================================================
// BIR tests
// ============================================================================

#[test]
fn bir_simple_round_trip() {
    let block = BIrBlock {
        params:     2,   // v0, v1
        stmts:      vec![
            BIrStmt::Zero,                   // v2
            BIrStmt::One,                    // v3
            BIrStmt::And(v(0), v(1)),        // v4
            BIrStmt::Or(v(0), v(1)),         // v5
            BIrStmt::Xor(v(0), v(1)),        // v6
            BIrStmt::Not(v(0)),              // v7
        ],
        stmt_provs: vec![(), (), (), (), (), ()],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return, args: vec![v(6)],
        }),
    };
    round_trip_bir(SavedBIrBlocks { blocks: BIrBlocks(vec![block]) });
}

#[test]
fn bir_oracle_action_rng() {
    let block = BIrBlock {
        params:     4,   // v0..v3
        stmts:      vec![
            BIrStmt::OracleCall { name: "oc".into(), args: vec![v(0), v(1)], num_bits: 4 }, // v4
            BIrStmt::OracleBit  { call: v(4), bit: 2 },                                     // v5
            BIrStmt::ActionCall { name: "ac".into(), guard: v(0), args: vec![v(1)],
                                   fallback: vec![v(2)], num_bits: 2 },                      // v6
            BIrStmt::ActionBit  { call: v(6), bit: 0 },                                     // v7
            BIrStmt::Rng        { name: "rng1".into() },                                    // v8
        ],
        stmt_provs: vec![(), (), (), (), ()],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return, args: vec![v(8)],
        }),
    };
    round_trip_bir(SavedBIrBlocks { blocks: BIrBlocks(vec![block]) });
}

#[test]
fn bir_storage() {
    let block = BIrBlock {
        params:     4,   // v0..v3 (addr bits)
        stmts:      vec![
            BIrStmt::StorageRead  { storage: storage(0), bit_width: 8, addr: vec![v(0), v(1)] }, // v4
            BIrStmt::StorageWrite { storage: storage(1), src: v(4), bit_width: 8, addr: vec![v(2), v(3)] }, // v5
        ],
        stmt_provs: vec![(), ()],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return, args: vec![v(4)],
        }),
    };
    round_trip_bir(SavedBIrBlocks { blocks: BIrBlocks(vec![block]) });
}

#[test]
fn bir_cond_jmp() {
    let block0 = BIrBlock {
        params:     1,  // v0
        stmts:      vec![],
        stmt_provs: vec![],
        terminator: BIrTerminator::CondJmp {
            val: v(0),
            then_target: BIrTarget { block: IRBlockTargetId::Block(IRBlockId(1)), args: vec![v(0)] },
            else_target: BIrTarget { block: IRBlockTargetId::Return,              args: vec![] },
        },
    };
    let block1 = BIrBlock {
        params:     1,
        stmts:      vec![],
        stmt_provs: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget { block: IRBlockTargetId::Return, args: vec![v(0)] }),
    };
    round_trip_bir(SavedBIrBlocks { blocks: BIrBlocks(vec![block0, block1]) });
}

#[test]
fn bir_multi_block() {
    let block0 = BIrBlock {
        params:     2,
        stmts:      vec![BIrStmt::And(v(0), v(1))],
        stmt_provs: vec![()],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Block(IRBlockId(1)), args: vec![v(2)],
        }),
    };
    let block1 = BIrBlock {
        params:     1,
        stmts:      vec![BIrStmt::Not(v(0))],
        stmt_provs: vec![()],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return, args: vec![v(1)],
        }),
    };
    round_trip_bir(SavedBIrBlocks { blocks: BIrBlocks(vec![block0, block1]) });
}

// ============================================================================
// Parser error cases
// ============================================================================

#[test]
fn error_ir_missing_version_line() {
    let err = match SavedIrBlocks::parse_text("begin_block 0\n") { Err(e) => e, Ok(_) => panic!("expected Err") };
    assert!(matches!(err, crate::ParseError::MissingVersionLine),
        "expected MissingVersionLine, got {:?}", err);
}

#[test]
fn error_ir_wrong_version() {
    let err = match SavedIrBlocks::parse_text("volar-ir v99\n") { Err(e) => e, Ok(_) => panic!("expected Err") };
    assert!(matches!(err, crate::ParseError::UnsupportedVersion(_)),
        "expected UnsupportedVersion, got {:?}", err);
}

#[test]
fn error_bir_missing_version_line() {
    let err = match SavedBIrBlocks::parse_text("begin_block 0\n") { Err(e) => e, Ok(_) => panic!("expected Err") };
    assert!(matches!(err, crate::ParseError::MissingVersionLine),
        "expected MissingVersionLine, got {:?}", err);
}

#[test]
fn error_bir_wrong_version() {
    let err = match SavedBIrBlocks::parse_text("volar-bir v99\n") { Err(e) => e, Ok(_) => panic!("expected Err") };
    assert!(matches!(err, crate::ParseError::UnsupportedVersion(_)),
        "expected UnsupportedVersion, got {:?}", err);
}

#[test]
fn comments_and_blank_lines_ir() {
    let text = std::format!(
        "volar-ir v1\n; type table\n\ntype 0 prim bit\n\n; blocks\nbegin_block 0\nparams []\njmp return args=[]\nend_block\n"
    );
    let parsed = SavedIrBlocks::parse_text(&text).unwrap_or_else(|e| panic!("parse with comments should work: {:?}", e));
    let text2 = parsed.to_text_string();
    // Re-parse the canonical form
    let parsed2 = SavedIrBlocks::parse_text(&text2).unwrap_or_else(|e| panic!("re-parse: {:?}", e));
    assert_eq!(text2, parsed2.to_text_string());
}
