// @reliability: experimental
// @ai: assisted
//! Storage-aware VOLE loop weaver (Commitment mode).
//!
//! This is the first weaver that threads a **loop-carried, in-circuit multiset
//! memory accumulator** through a dynamic loop, rather than accumulating an
//! offline `MemoryTrace` over a flat circuit (as `vole.rs` Commitment mode
//! does).  It unblocks the VOLE Continuation Bridge's memory continuation: the
//! accumulator is a single field element per side (`mem_prod`, `mem_cons`), so
//! it survives a gap in O(1) and can be carried into `ResumeToken.mem_acc`.
//!
//! See `docs/vole-continuation-bridge.md` §8.
//!
//! ## Scope (this slice)
//!
//! - Gates: `Zero`, `One`, `Xor`, `Not` (linear — no AND/hat/transport here, so
//!   the loop is self-contained and compile-checkable without `volar-net`).
//!   `And`/`Or`/oracle/action/rng panic at weave time.
//! - Storage: `StorageRead` / `StorageWrite`, **single-bit address**
//!   (`addr.len() == 1`).  Per access the running accumulators are updated via
//!   `volar_spec::vole::bridge::mem_acc_absorb_vope` (free public-scalar
//!   scaling).  Read values and write old-values are supplied per iteration via
//!   `read_vals` / `write_olds` slices (indexed `iter * COUNT + k`, mirroring the
//!   net verifier's `q_ands` convention).
//! - The exit returns `(output, mem_prod, mem_cons)`; the verifier checks
//!   `mem_prod == mem_cons` offline after a final drain.
//!
//! ## Soundness note (honest)
//!
//! The accumulator-**carry** mechanism (additive multiset hash threaded through
//! the loop) is sound and is the piece the bridge needs.  Full read-consistency
//! soundness additionally requires tracking each cell's last-write timestamp so
//! a read consumes the exact `(addr, value, write_ts)` tuple, plus a final drain
//! of live cells.  Here timestamps are a monotonic counter and the drain is
//! left to the caller — i.e. this models *what to carry*, not yet the complete
//! *what to absorb* bookkeeping.  That bookkeeping is about the absorbed tuples,
//! not the carry, and layers on top without changing this CFG's shape.

use alloc::{boxed::Box, collections::BTreeMap, format, string::String, vec, vec::Vec};

use volar_compiler::{
    ir::{
        ExternalKind, IrAnyFunction, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgJump,
        IrCfgModule, IrCfgTerminator, IrExpr, IrGenericParam, IrGenericParamKind, IrLit,
        IrModule, IrParam, IrPattern, IrStmt, IrType, IrWherePredicate, SpecBinOp, StructKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt, BIrTerminator};
use volar_ir::ir::IRVarId;

use crate::{clone_expr, expand_ors, ref_expr, var};
use crate::gadgets::{emit_lt, GateBuf};
use crate::net::{
    add_output_t, array_size_bound, bool_type, clone_t, default_t, emit_prover_and_gate,
    mul_output_t, ref_to, usize_type, vole_array_t_bound, vope_bit_call, vope_type,
};

// ── Gadget → VOLE lowering bridge ───────────────────────────────────────────

/// Lower a boolean gadget (`BIrStmt` gates over abstract var-ids, e.g. from
/// `gadgets::emit_lt`) to VOLE **prover** IR.  `in_map` pre-binds the gadget's
/// input var-ids to existing `Vope` wire names.  `Xor`/`Not`/`Zero`/`One` are
/// free; each `And` becomes a `vole_and_prover_step` (its hat name pushed to
/// `hats`); `Or(a,b) = a + b + (a·b)` (one AND).  Returns the wire bound to
/// `result_id`.
fn lower_gadget_prover(
    gates: &[(IRVarId, BIrStmt)],
    in_map: &BTreeMap<u32, String>,
    result_id: u32,
    tag: &str,
    hats: &mut Vec<String>,
    stmts: &mut Vec<IrStmt>,
) -> String {
    let addv = |a: &str, b: &str| IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(clone_expr(var(a))),
        right: Box::new(clone_expr(var(b))),
    };
    let mut map = in_map.clone();
    for (rid, stmt) in gates {
        let name = format!("g{tag}_{}", rid.0);
        match stmt {
            BIrStmt::Zero => stmts.push(let_stmt(&name, zero_vope_expr())),
            BIrStmt::One => stmts.push(let_stmt(&name, clone_expr(var("vope_one")))),
            BIrStmt::Xor(a, b) => stmts.push(let_stmt(&name, addv(&map[&a.0], &map[&b.0]))),
            BIrStmt::Not(a) => stmts.push(let_stmt(&name, addv(&map[&a.0], "vope_one"))),
            BIrStmt::And(a, b) => {
                let hat = format!("gh{tag}_{}", rid.0);
                hats.push(hat.clone());
                emit_prover_and_gate(&map[&a.0], &map[&b.0], &name, &hat, stmts);
            }
            BIrStmt::Or(a, b) => {
                let hat = format!("gh{tag}_{}", rid.0);
                hats.push(hat.clone());
                let ab = format!("{name}_ab");
                emit_prover_and_gate(&map[&a.0], &map[&b.0], &ab, &hat, stmts);
                let t = format!("{name}_t");
                stmts.push(let_stmt(&t, addv(&map[&a.0], &map[&b.0])));
                stmts.push(let_stmt(&name, addv(&t, &ab)));
            }
            _ => panic!("lower_gadget_prover: unexpected gate {stmt:?}"),
        }
        map.insert(rid.0, name);
    }
    map[&result_id].clone()
}

// ── Local helpers ───────────────────────────────────────────────────────────

/// `<N: ArraySize, T>` where `N: VoleArray<T>, T: Clone + Add + Mul + Default +
/// PartialEq` — no transport generic (unlike the net weavers).
fn storage_loop_generics_and_where() -> (Vec<IrGenericParam>, Vec<IrWherePredicate>) {
    use volar_compiler::ir::{IrTraitBound, MathTrait, TraitKind};
    let generics = vec![
        IrGenericParam {
            name: "N".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![array_size_bound()],
            default: None,
        },
        IrGenericParam {
            name: "T".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
            bounds: vec![],
            default: None,
        },
    ];
    let partial_eq_t =
        IrTraitBound { trait_kind: TraitKind::Math(MathTrait::PartialEq), type_args: vec![], assoc_bindings: vec![] };
    let where_clause = vec![
        IrWherePredicate::TypeBound {
            ty: IrType::TypeParam("N".into()),
            bounds: vec![vole_array_t_bound()],
        },
        IrWherePredicate::TypeBound {
            ty: IrType::TypeParam("T".into()),
            bounds: vec![clone_t(), add_output_t(), mul_output_t(), default_t(), partial_eq_t],
        },
    ];
    (generics, where_clause)
}

/// `&[Vope<N, T, U1>]`
pub(crate) fn vope_slice_type() -> IrType {
    ref_to(IrType::Array {
        kind: volar_compiler::ir::ArrayKind::Slice,
        elem: Box::new(vope_type()),
        len: volar_compiler::ir::ArrayLength::Const(0),
    })
}

/// `Vope { u: Array::<Array<T,N>, U1>::default(), v: Array::<T,N>::default() }`
pub(crate) fn zero_vope_expr() -> IrExpr {
    let array_default = |type_args: Vec<IrType>| IrExpr::Call {
        func: Box::new(IrExpr::Path { segments: vec!["Array".into(), "default".into()], type_args }),
        args: vec![],
    };
    IrExpr::StructExpr {
        kind: StructKind::Custom("Vope".into()),
        type_args: vec![],
        fields: vec![
            // u : Array<Array<T, N>, U1>
            ("u".into(), array_default(vec![crate::net::array_t_n(), IrType::TypeParam("U1".into())])),
            // v : Array<T, N>
            ("v".into(), array_default(vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())])),
        ],
        rest: None,
    }
}

/// `volar_spec::vole::bridge::mem_acc_absorb_vope(acc.clone(), &addr, &value, &ts, &r1, &r2, &r3)`
///
/// The accumulator is **cloned** so the input value remains available — the
/// hybrid weaver needs it as the gap replay anchor (the extra clone is free in
/// the storage-only loop, which has a single path).
pub(crate) fn absorb_call(acc: &str, addr: &str, value: &str, ts: &str) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec![
                "volar_spec".into(),
                "vole".into(),
                "bridge".into(),
                "mem_acc_absorb_vope".into(),
            ],
            type_args: vec![],
        }),
        args: vec![
            clone_expr(var(acc)),
            ref_expr(var(addr)),
            ref_expr(var(value)),
            ref_expr(var(ts)),
            ref_expr(var("r1")),
            ref_expr(var("r2")),
            ref_expr(var("r3")),
        ],
    }
}

/// `slice[iter * count + k].clone()`
pub(crate) fn slice_index_clone(slice: &str, count: usize, k: usize) -> IrExpr {
    let idx = IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(IrExpr::Binary {
            op: SpecBinOp::Mul,
            left: Box::new(var("iter")),
            right: Box::new(IrExpr::Lit(IrLit::Int(count as i128))),
        }),
        right: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
    };
    clone_expr(IrExpr::Index { base: Box::new(var(slice)), index: Box::new(idx) })
}

fn let_stmt(name: &str, init: IrExpr) -> IrStmt {
    IrStmt::Let { pattern: IrPattern::ident(name), ty: None, init: Some(init) }
}

pub(crate) fn count_storage(expanded: &[(IRVarId, BIrStmt, ())]) -> (usize, usize) {
    let mut reads = 0;
    let mut writes = 0;
    for (_, s, _) in expanded {
        match s {
            BIrStmt::StorageRead { .. } => reads += 1,
            BIrStmt::StorageWrite { .. } => writes += 1,
            _ => {}
        }
    }
    (reads, writes)
}

// ============================================================================
// weave_storage_commit_loop_prover
// ============================================================================

/// Weave a single-block loop circuit with Commitment-mode storage into an
/// `IrCfgModule` that threads the multiset-hash accumulator through the loop.
///
/// Generated signature:
/// ```text
/// fn storage_commit_loop_<NAME><N, T>(
///     vope_one: Vope<N, T, U1>,
///     r1: T, r2: T, r3: T,                 // public challenge powers
///     init_w0: Vope<N, T, U1>, ...,        // initial loop state
///     read_vals:  &[Vope<N, T, U1>],       // committed read values  (iter*R + k)
///     write_olds: &[Vope<N, T, U1>],       // committed old values    (iter*W + k)
/// ) -> (Vope<N, T, U1>, Vope<N, T, U1>, Vope<N, T, U1>)   // (output, mem_prod, mem_cons)
/// ```
pub fn weave_storage_commit_loop_prover(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(
        circuit.is_movfuscated(),
        "weave_storage_commit_loop_prover: circuit must be single-block"
    );
    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let (read_count, write_count) = count_storage(&expanded);

    let (generics, where_clause) = storage_loop_generics_and_where();

    // ── Function params ────────────────────────────────────────────────────
    let mut func_params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    for r in ["r1", "r2", "r3"] {
        func_params.push(IrParam { name: r.into(), ty: IrType::TypeParam("T".into()) });
    }
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_w{i}"), ty: vope_type() });
    }
    func_params.push(IrParam { name: "read_vals".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "write_olds".into(), ty: vope_slice_type() });

    let ret_type = IrType::Tuple(vec![vope_type(), vope_type(), vope_type()]);

    // ── Block 0: entry ─────────────────────────────────────────────────────
    // Goto B1 [init_w.clone()..., mem_prod=0, mem_cons=0, ts=0, iter=0]
    let mut b0_args: Vec<IrExpr> =
        (0..num_params).map(|i| clone_expr(var(&format!("init_w{i}")))).collect();
    b0_args.push(zero_vope_expr()); // mem_prod
    b0_args.push(zero_vope_expr()); // mem_cons
    b0_args.push(zero_vope_expr()); // ts
    b0_args.push(IrExpr::Lit(IrLit::Int(0))); // iter
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: loop body ─────────────────────────────────────────────────
    let mut b1_params: Vec<IrParam> =
        (0..num_params).map(|i| IrParam { name: format!("w{i}"), ty: vope_type() }).collect();
    b1_params.push(IrParam { name: "mem_prod".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "mem_cons".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "ts".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "iter".into(), ty: usize_type() });

    let mut b1_stmts: Vec<IrStmt> = Vec::new();
    let mut wnames = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        wnames.insert(i as u32, format!("w{i}"));
    }
    // Current SSA names for the carried accumulators / timestamp.
    let mut cur_prod = String::from("mem_prod");
    let mut cur_cons = String::from("mem_cons");
    let mut cur_ts = String::from("ts");
    let mut ssa = 0usize; // fresh-name counter
    let mut read_k = 0usize;
    let mut write_k = 0usize;

    for (result_id, stmt, _) in &expanded {
        let let_name = format!("lw_{}", result_id.0);
        match stmt {
            BIrStmt::Zero => b1_stmts.push(let_stmt(&let_name, zero_vope_expr())),
            BIrStmt::One => b1_stmts.push(let_stmt(&let_name, clone_expr(var("vope_one")))),
            BIrStmt::Xor(a, b) => {
                let e = IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(clone_expr(var(&wnames[&a.0]))),
                    right: Box::new(clone_expr(var(&wnames[&b.0]))),
                };
                b1_stmts.push(let_stmt(&let_name, e));
            }
            BIrStmt::Not(a) => {
                let e = IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(clone_expr(var(&wnames[&a.0]))),
                    right: Box::new(clone_expr(var("vope_one"))),
                };
                b1_stmts.push(let_stmt(&let_name, e));
            }
            BIrStmt::StorageRead { addr, .. } => {
                assert_eq!(addr.len(), 1, "storage_loop: only single-bit addresses supported");
                let addr_w = wnames[&addr[0].0].clone();
                let val_name = format!("rd_{ssa}");
                ssa += 1;
                b1_stmts.push(let_stmt(&val_name, slice_index_clone("read_vals", read_count, read_k)));
                read_k += 1;
                // consume (addr, val, ts)
                let nc = format!("cons_{ssa}");
                ssa += 1;
                b1_stmts.push(let_stmt(&nc, absorb_call(&cur_cons, &addr_w, &val_name, &cur_ts)));
                cur_cons = nc;
                // ts += 1
                let nts = format!("ts_{ssa}");
                ssa += 1;
                b1_stmts.push(let_stmt(
                    &nts,
                    IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(var(&cur_ts)),
                        right: Box::new(clone_expr(var("vope_one"))),
                    },
                ));
                cur_ts = nts;
                // produce (addr, val, ts)
                let np = format!("prod_{ssa}");
                ssa += 1;
                b1_stmts.push(let_stmt(&np, absorb_call(&cur_prod, &addr_w, &val_name, &cur_ts)));
                cur_prod = np;
                // the read result wire is the committed value
                wnames.insert(result_id.0, val_name.clone());
                continue;
            }
            BIrStmt::StorageWrite { src, addr, .. } => {
                assert_eq!(addr.len(), 1, "storage_loop: only single-bit addresses supported");
                let addr_w = wnames[&addr[0].0].clone();
                let new_w = wnames[&src.0].clone();
                let old_name = format!("old_{ssa}");
                ssa += 1;
                b1_stmts.push(let_stmt(&old_name, slice_index_clone("write_olds", write_count, write_k)));
                write_k += 1;
                // consume (addr, old, ts)
                let nc = format!("cons_{ssa}");
                ssa += 1;
                b1_stmts.push(let_stmt(&nc, absorb_call(&cur_cons, &addr_w, &old_name, &cur_ts)));
                cur_cons = nc;
                // ts += 1
                let nts = format!("ts_{ssa}");
                ssa += 1;
                b1_stmts.push(let_stmt(
                    &nts,
                    IrExpr::Binary {
                        op: SpecBinOp::Add,
                        left: Box::new(var(&cur_ts)),
                        right: Box::new(clone_expr(var("vope_one"))),
                    },
                ));
                cur_ts = nts;
                // produce (addr, new, ts)
                let np = format!("prod_{ssa}");
                ssa += 1;
                b1_stmts.push(let_stmt(&np, absorb_call(&cur_prod, &addr_w, &new_w, &cur_ts)));
                cur_prod = np;
                // StorageWrite yields a dummy zero wire
                b1_stmts.push(let_stmt(&let_name, zero_vope_expr()));
            }
            BIrStmt::And(..) | BIrStmt::Or(..) => {
                panic!("storage_loop: AND/OR gates not supported (linear subset only)")
            }
            _ => panic!("storage_loop: unsupported statement {stmt:?}"),
        }
        wnames.insert(result_id.0, let_name);
    }

    // Terminator analysis (same convention as the net loop weaver).
    let (done_id, out_id, next_ids) = match &block.terminator {
        BIrTerminator::Jmp(t) => {
            assert!(!t.args.is_empty());
            (t.args.last().unwrap().0, t.args[0].0, t.args[..t.args.len() - 1].to_vec())
        }
        BIrTerminator::CondJmp { val, then_target, else_target } => (
            val.0,
            then_target.args.get(0).map(|id| id.0).unwrap_or(val.0),
            else_target.args.clone(),
        ),
    };
    let done_wire = wnames[&done_id].clone();
    let out_wire = wnames[&out_id].clone();

    b1_stmts.push(let_stmt("done_bit", vope_bit_call(&done_wire)));

    // then (done) -> B2 [out, mem_prod, mem_cons]
    let b2_args = vec![
        clone_expr(var(&out_wire)),
        clone_expr(var(&cur_prod)),
        clone_expr(var(&cur_cons)),
    ];
    // else -> B1 [next..., mem_prod, mem_cons, ts, iter+1]
    let mut back_args: Vec<IrExpr> =
        next_ids.iter().map(|id| clone_expr(var(&wnames[&id.0]))).collect();
    back_args.push(var(&cur_prod));
    back_args.push(var(&cur_cons));
    back_args.push(var(&cur_ts));
    back_args.push(IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(var("iter")),
        right: Box::new(IrExpr::Lit(IrLit::Int(1))),
    });

    let block1 = IrCfgBlock {
        params: b1_params,
        stmts: b1_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("done_bit"),
            then_: IrCfgJump { target: 2, args: b2_args },
            else_: IrCfgJump { target: 1, args: back_args },
        },
    };

    // ── Block 2: exit — return (output, mem_prod, mem_cons) ────────────────
    let block2 = IrCfgBlock {
        params: vec![
            IrParam { name: "output".into(), ty: vope_type() },
            IrParam { name: "fprod".into(), ty: vope_type() },
            IrParam { name: "fcons".into(), ty: vope_type() },
        ],
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(IrExpr::Tuple(vec![
            var("output"),
            var("fprod"),
            var("fcons"),
        ]))),
    };

    let func = IrCfgFunction {
        name: format!("storage_commit_loop_{name}"),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0, block1, block2] },
    };

    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_storage_loop_{name}"),
        functions: vec![IrAnyFunction::Cfg(func)],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply_cfg(&mut module);
    }
    module
}

/// Weave a VOLE **prover** function computing the committed less-than
/// `a < b` over two `n`-bit committed timestamps, via the `emit_lt` gadget
/// lowered to VOLE (XOR/NOT free, AND → `vole_and_prover_step` + hat).
///
/// This validates the gadget → VOLE lowering bridge end-to-end (the
/// timestamp-ordering primitive that fixes the memory-checking soundness gap;
/// see `docs/vole-continuation-bridge.md` §9).  Generated signature:
/// ```text
/// fn lt_check_<NAME><N, T>(vope_one: Vope<N,T,U1>,
///     a0: Vope<N,T,U1>, .., b0: Vope<N,T,U1>, ..) -> Vope<N,T,U1>   // = (a < b)
/// ```
pub fn weave_lt_check(n: usize, name: &str, linkage: Option<&LinkageSystem>) -> IrCfgModule {
    let (generics, where_clause) = storage_loop_generics_and_where();

    let mut func_params = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    for i in 0..n {
        func_params.push(IrParam { name: format!("a{i}"), ty: vope_type() });
    }
    for i in 0..n {
        func_params.push(IrParam { name: format!("b{i}"), ty: vope_type() });
    }

    // Build the gadget over abstract ids: a = 0..n, b = n..2n.
    let a_ids: Vec<u32> = (0..n as u32).collect();
    let b_ids: Vec<u32> = (n as u32..2 * n as u32).collect();
    let mut buf = GateBuf::new(2 * n as u32);
    let result_id = emit_lt(&a_ids, &b_ids, &mut buf);

    let mut in_map = BTreeMap::<u32, String>::new();
    for i in 0..n {
        in_map.insert(i as u32, format!("a{i}"));
        in_map.insert((n + i) as u32, format!("b{i}"));
    }
    let mut hats: Vec<String> = Vec::new();
    let mut stmts: Vec<IrStmt> = Vec::new();
    let result = lower_gadget_prover(&buf.gates, &in_map, result_id, "lt", &mut hats, &mut stmts);

    let block0 = IrCfgBlock {
        params: vec![],
        stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(var(&result))),
    };
    let func = IrCfgFunction {
        name: format!("lt_check_{name}"),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(vope_type()),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0] },
    };
    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_lt_check_{name}"),
        functions: vec![IrAnyFunction::Cfg(func)],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply_cfg(&mut module);
    }
    module
}

/// Print a storage-loop CFG module to self-contained Rust source.
pub fn print_storage_loop_module(module: &IrCfgModule) -> String {
    use volar_compiler::printer::{CfgModuleWriter, DisplayRust};
    use alloc::fmt::Write as _;

    let mut body = String::new();
    let _ = write!(body, "{}", DisplayRust(CfgModuleWriter { module, emit_async: false }));

    let preamble = concat!(
        "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
        "extern crate alloc;\n",
        "use alloc::vec::Vec;\n",
        "use alloc::vec;\n",
        "use core::ops::{Add, Mul};\n",
        "use hybrid_array::{Array, ArraySize};\n",
        "use cipher::consts::U1;\n",
        "use volar_spec::vole::{Q, Vope, VoleArray};\n",
        "use volar_spec::vole::bridge::mem_acc_absorb_vope;\n",
        "use volar_spec::vole::prove::vole_and_prover_step;\n",
        "use volar_net::vope_bit;\n",
        "\n",
    );

    let mut out = String::with_capacity(preamble.len() + body.len());
    out.push_str(preamble);
    out.push_str(&body);
    out
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::tests_common::run_compile_check_net;
    use volar_ir::boolar::{BIrBlock, BIrTarget, BIrTerminator};
    use volar_ir::ir::{IRBlockTargetId, StorageId};

    /// Single-block loop with one storage write then read at 1-bit address.
    /// params (ids 0,1,2): w0 = address bit, w1 = value to store, w2 = done flag.
    /// stmts: Write(src=w1, addr=[w0]) -> id 3;  v = Read(addr=[w0]) -> id 4.
    /// terminator (Jmp convention: last arg = done, the rest = next state, which
    /// must be `num_params` wide): next = [w0, w1, v(id4)], done = w2.
    fn build_storage_loop() -> BIrBlocks {
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: 3,
                stmts: vec![
                    BIrStmt::StorageWrite {
                        storage: StorageId(0),
                        src: IRVarId(1),
                        bit_width: 1,
                        addr: vec![IRVarId(0)],
                    },
                    BIrStmt::StorageRead {
                        storage: StorageId(0),
                        bit_width: 1,
                        addr: vec![IRVarId(0)],
                    },
                ],
                stmt_provs: vec![(), ()],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    // next state = [w0, w1, read-result(id 4)], done flag = w2
                    args: vec![IRVarId(0), IRVarId(1), IRVarId(4), IRVarId(2)],
                }),
            }],
            pre_init: vec![],
        }
    }

    #[test]
    fn test_storage_loop_compiles() {
        let circuit = build_storage_loop();
        let module = weave_storage_commit_loop_prover(&circuit, "test", None);
        let code = print_storage_loop_module(&module);
        run_compile_check_net(&code, "storage_commit_loop");
    }

    #[test]
    fn test_storage_loop_threads_accumulator() {
        let circuit = build_storage_loop();
        let module = weave_storage_commit_loop_prover(&circuit, "test", None);
        let code = print_storage_loop_module(&module);
        // The accumulator is carried as loop state and updated via the absorb.
        assert!(code.contains("mem_prod"), "expected carried produce accumulator");
        assert!(code.contains("mem_cons"), "expected carried consume accumulator");
        assert!(code.contains("mem_acc_absorb_vope"), "expected in-circuit absorb");
        assert!(code.contains("read_vals"), "expected per-iteration read-value slice");
        assert!(code.contains("write_olds"), "expected per-iteration old-value slice");
    }

    #[test]
    fn test_lt_check_lowers_to_vole() {
        // The timestamp-ordering gadget (emit_lt) lowers to VOLE and compiles:
        // ANDs become vole_and_prover_step + hat; XOR/NOT are field adds.
        let module = weave_lt_check(4, "ts", None);
        let code = print_storage_loop_module(&module);
        run_compile_check_net(&code, "lt_check_gadget");
        assert!(code.contains("vole_and_prover_step"), "expected gadget ANDs lowered to hats");
    }
}
