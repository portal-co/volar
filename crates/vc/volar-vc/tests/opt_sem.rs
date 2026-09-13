#[test]
fn opt_preserves_semantics() {
    use volar_fuzz::interpreter::biir::eval_biir;
    // A small nontrivial circuit: x XOR y, (x AND y) OR (x AND y) [dup for CSE].
    use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
    use volar_ir::ir::{IRBlockTargetId, IRVarId};
    use volar_ir_common::Node;
    let node = |k| Node::new(k, (), None);
    let mut c = BIrBlocks {
        blocks: vec![BIrBlock {
            params: 2,
            stmts: vec![
                node(BIrStmt::Xor(IRVarId(0), IRVarId(1))), // rv2
                node(BIrStmt::And(IRVarId(0), IRVarId(1))), // rv3
                node(BIrStmt::And(IRVarId(1), IRVarId(0))), // rv4 (dup)
                node(BIrStmt::Or(IRVarId(3), IRVarId(4))),  // rv5
            ],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(2), IRVarId(5)],
            }),
        }],
        pre_init: vec![],
    };
    for inp in [[false, false], [false, true], [true, false], [true, true]] {
        let want = eval_biir(&c, &inp).unwrap();
        let mut cc = c.clone();
        volar_ir_opt::biir::fold_biir_blocks(&mut cc);
        volar_ir_opt::biir::cse_biir_blocks(&mut cc);
        volar_ir_opt::biir::dce_biir_blocks(&mut cc);
        let got = eval_biir(&cc, &inp).unwrap();
        assert_eq!(got, want, "input {inp:?}");
    }
}

#[test]
fn opt_preserves_fe_square_semantics() {
    use volar_fuzz::interpreter::biir::eval_biir;
    use volar_vc::x25519_gadget::scalar_ref::*;
    let c = volar_vc::x25519_gadget::build_fe_square();
    let mut cc = c.clone();
    volar_ir_opt::biir::fold_biir_blocks(&mut cc);
    let stmts_after_fold = cc.blocks[0].stmts.len();
    volar_ir_opt::biir::cse_biir_blocks(&mut cc);
    let stmts_after_cse = cc.blocks[0].stmts.len();
    let z: Fp = fp_from_bytes(&[0x2Bu8; 32]);
    let mut inp = volar_vc::tls13_2pc::bits_of(&fp_to_bytes(&z));
    inp.truncate(255);
    let want = eval_biir(&c, &inp).unwrap();
    let got_fc = eval_biir(&cc, &inp).unwrap();
    assert_eq!(
        got_fc.len(),
        want.len(),
        "fold+cse changed output arity ({stmts_after_fold} stmts after fold, {stmts_after_cse} after cse)"
    );
    assert_eq!(got_fc, want, "fold+cse changed fe_square semantics");
    let n_before_dce = cc.blocks[0].stmts.len();
    volar_ir_opt::biir::dce_biir_blocks(&mut cc);
    let n_after_dce = cc.blocks[0].stmts.len();
    let got_dce = eval_biir(&cc, &inp).unwrap();
    assert_eq!(
        got_dce, want,
        "dce changed fe_square semantics ({n_before_dce} -> {n_after_dce} stmts)"
    );
}
