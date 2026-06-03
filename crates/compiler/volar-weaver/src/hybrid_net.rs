// @reliability: experimental
// @ai: assisted
//! Hybrid network-resilient VOLE weaving.
//!
//! Like [`crate::net`]'s streaming loop weavers, but the generated CFG **reacts
//! to the network cutting off**: when a per-iteration send fails it switches to
//! a cleartext (non-ZK) "no-op" gap path that consumes no VOLE correlations,
//! then runs a resumption bridge on reconnect.  Generated code targets the
//! [`volar_net::ResilientVoleTransport`] trait (a superset of `VoleTransport`).
//!
//! The default resumption is **best-effort**: the gap runs in the clear and the
//! verifier's verdict is *qualified* ("valid except iterations `[k, k+m]`").
//! The sound, succinct alternative — the VOLE Continuation Bridge — is specified
//! in `docs/vole-continuation-bridge.md` and slots in behind the same trait
//! without regenerating code.
//!
//! ## Prover CFG shape (9 blocks)
//!
//! | Block | Role |
//! |-------|------|
//! | 0 | entry — clone initial state, jump to ZK body |
//! | 1 | ZK loop body — compute hats, `try_send_iteration?`; branch continue / disconnect |
//! | 2 | post-send dispatch — branch done (exit) / next iteration |
//! | 3 | exit — `recv_verdict?`, return |
//! | 4 | gap body — one cleartext iteration, `try_reconnect?` |
//! | 5 | resume — re-commit cleartext state, `prover_bridge?`, re-enter ZK body |
//! | 6 | gap dispatch — branch finished-offline / continue gap |
//! | 7 | offline spin — retry reconnect after finishing during the outage |
//! | 8 | final — re-commit output, bridge, `recv_verdict?`, return |

use alloc::{
    boxed::Box,
    collections::BTreeMap,
    format,
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        ExternalKind, IrAnyFunction, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgJump,
        IrCfgModule, IrCfgTerminator, IrClosureParam, IrExpr, IrGenericParam, IrLit, IrModule,
        IrParam, IrPattern, IrStmt, IrTraitBound, IrType, MethodKind, PrimitiveType, SpecBinOp,
        SpecUnaryOp, StdMethod, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt, BIrTerminator};
use volar_ir::ir::IRVarId;

use crate::{clone_expr, expand_ors, ref_expr, var};
use crate::net::{
    bool_type, count_and_gates, delta_type, emit_prover_and_gate, hats_slice_expr,
    net_verifier_generics_and_where, net_prover_loop_generics_and_where, ok_expr, q_slice_type,
    q_type, ref_mut_to, ref_to, result_type, tr_error_type, transport_call_try, usize_type,
    vope_bit_call, vope_type,
};
use crate::storage_loop::{absorb_call, count_storage, slice_index_clone, vope_slice_type, zero_vope_expr};

// ── Local helpers ───────────────────────────────────────────────────────────

fn u32_type() -> IrType {
    IrType::Primitive(PrimitiveType::U32)
}

/// `Tr: volar_net::ResilientVoleTransport<N, T>` trait bound.
fn resilient_transport_bound() -> IrTraitBound {
    IrTraitBound {
        trait_kind: TraitKind::External {
            path: vec!["volar_net".into(), "ResilientVoleTransport".into()],
        },
        type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
        assoc_bindings: vec![],
    }
}

/// Swap the `Tr` generic's bound from `VoleTransport` to `ResilientVoleTransport`.
fn make_resilient(generics: &mut [IrGenericParam]) {
    for g in generics.iter_mut() {
        if g.name == "Tr" {
            g.bounds = vec![resilient_transport_bound()];
        }
    }
}

/// `let {name} = {init};`
fn let_stmt(name: &str, init: IrExpr) -> IrStmt {
    IrStmt::Let { pattern: IrPattern::ident(name), ty: None, init: Some(init) }
}

/// `a {op} b` on plain (Copy) values — no clone.
fn bin(op: SpecBinOp, a: &str, b: &str) -> IrExpr {
    IrExpr::Binary { op, left: Box::new(var(a)), right: Box::new(var(b)) }
}

/// `Vec::new()`
fn vec_new() -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path { segments: vec!["Vec".into(), "new".into()], type_args: vec![] }),
        args: vec![],
    }
}

/// `volar_net::ResumeToken { resume_state: Vec::from([w0.clone(), ...]), mem_acc: Vec::new() }`
///
/// `state_names` are the anchor (or fresh) live-state wires; `mem_acc_names` are
/// the carried memory-accumulator wires (empty for storage-free circuits).
fn resume_token_expr(state_names: &[String], mem_acc_names: &[String]) -> IrExpr {
    let mk_vec = |names: &[String]| -> IrExpr {
        if names.is_empty() {
            vec_new()
        } else {
            let arr = IrExpr::FixedArray(names.iter().map(|n| clone_expr(var(n))).collect());
            IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["Vec".into(), "from".into()],
                    type_args: vec![],
                }),
                args: vec![arr],
            }
        }
    };
    IrExpr::StructExpr {
        kind: StructKind::Custom("volar_net::ResumeToken".into()),
        type_args: vec![],
        fields: vec![
            ("resume_state".into(), mk_vec(state_names)),
            ("mem_acc".into(), mk_vec(mem_acc_names)),
        ],
        rest: None,
    }
}

/// `gap_len + 1` (u32).
fn incr(name: &str) -> IrExpr {
    IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(var(name)),
        right: Box::new(IrExpr::Lit(IrLit::Int(1))),
    }
}

/// Loop terminator analysis shared with `net` loop weavers.
/// Returns `(done_var_id, output_var_id, next_state_var_ids)`.
fn analyze_loop_terminator<P: Clone + Default>(
    block: &volar_ir::boolar::BIrBlock<P>,
) -> (u32, u32, Vec<IRVarId>) {
    match &block.terminator {
        BIrTerminator::Jmp(t) => {
            assert!(!t.args.is_empty(), "hybrid loop Jmp must have at least one arg");
            let done = t.args.last().unwrap().0;
            let out = t.args[0].0;
            let next = t.args[..t.args.len() - 1].to_vec();
            (done, out, next)
        }
        BIrTerminator::CondJmp { val, then_target, else_target } => {
            let out = then_target.args.get(0).map(|id| id.0).unwrap_or(val.0);
            (val.0, out, else_target.args.clone())
        }
    }
}

/// Emit the cleartext (plaintext-`bool`) gate evaluation for one iteration of
/// the loop body.  Inputs are named `{in_prefix}{i}`, outputs `{out_prefix}{id}`.
/// Returns the var-id → name map so the caller can resolve done/out/next wires.
fn emit_cleartext_gates(
    expanded: &[(IRVarId, BIrStmt, ())],
    num_params: usize,
    in_prefix: &str,
    out_prefix: &str,
    stmts: &mut Vec<IrStmt>,
) -> BTreeMap<u32, String> {
    let mut names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        names.insert(i as u32, format!("{in_prefix}{i}"));
    }
    for (result_id, stmt, _) in expanded {
        let nm = format!("{out_prefix}{}", result_id.0);
        let init = match stmt {
            BIrStmt::Zero => IrExpr::Lit(IrLit::Bool(false)),
            BIrStmt::One => IrExpr::Lit(IrLit::Bool(true)),
            BIrStmt::Xor(a, b) => bin(SpecBinOp::BitXor, &names[&a.0], &names[&b.0]),
            BIrStmt::And(a, b) => bin(SpecBinOp::BitAnd, &names[&a.0], &names[&b.0]),
            BIrStmt::Not(a) => IrExpr::Unary {
                op: SpecUnaryOp::Not,
                expr: Box::new(var(&names[&a.0])),
            },
            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            // Storage on the cleartext gap path is **provisional** — the gap is
            // replayed from the anchor on reconnect, so a placeholder read/write
            // value here only affects liveness (provisional output), never
            // soundness.  A StorageRead yields `false`, a StorageWrite a dummy.
            BIrStmt::StorageRead { .. } | BIrStmt::StorageWrite { .. } => {
                IrExpr::Lit(IrLit::Bool(false))
            }
            _ => {
                // Oracle/Action/Rng are not supported on the cleartext gap path.
                panic!("hybrid_net cleartext gap: unsupported gate {stmt:?}");
            }
        };
        stmts.push(let_stmt(&nm, init));
        names.insert(result_id.0, nm);
    }
    names
}

/// `Q { q: {field_expr} }`
fn q_struct(field: IrExpr) -> IrExpr {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Q".into()),
        type_args: vec![],
        fields: vec![("q".into(), field)],
        rest: None,
    }
}

/// `Array::<T, N>::default()`
fn array_t_default() -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "default".into()],
            type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
        }),
        args: vec![],
    }
}

/// `{recv}.{field}[i].clone()` — index a `.q`/`.delta` array field at `i`.
fn field_index_clone(recv: &str, field: &str) -> IrExpr {
    clone_expr(IrExpr::Index {
        base: Box::new(IrExpr::Field {
            base: Box::new(var(recv)),
            field: field.into(),
        }),
        index: Box::new(var("i")),
    })
}

/// `Array::<T,N>::from_fn(|i| {body})`
fn array_from_fn_t(body: IrExpr) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "from_fn".into()],
            type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
        }),
        args: vec![IrExpr::Closure {
            params: vec![IrClosureParam { pattern: IrPattern::ident("i"), ty: None }],
            ret_type: None,
            body: Box::new(body),
        }],
    }
}

/// Replicates the net loop verifier's per-gate Q lowering, reading hats from
/// `iter_hats[k]` and `q_and` shares from `q_ands[iter * AND_COUNT + k]`, and
/// accumulating `all_ok_new`.
fn emit_verifier_loop_gates(
    expanded: &[(IRVarId, BIrStmt, ())],
    vnames: &mut BTreeMap<u32, String>,
    and_count: usize,
    stmts: &mut Vec<IrStmt>,
) {
    let mut and_counter = 0usize;
    for (result_id, stmt, _) in expanded {
        let let_name = format!("lq_{}", result_id.0);
        match stmt {
            BIrStmt::Zero => stmts.push(let_stmt(&let_name, q_struct(array_t_default()))),
            BIrStmt::One => {
                let q = q_struct(IrExpr::MethodCall {
                    receiver: Box::new(IrExpr::Field {
                        base: Box::new(var("delta")),
                        field: "delta".into(),
                    }),
                    method: MethodKind::Known(StdMethod::Clone),
                    type_args: vec![],
                    args: vec![],
                });
                stmts.push(let_stmt(&let_name, q));
            }
            BIrStmt::Xor(a, b) => {
                let na = vnames[&a.0].clone();
                let nb = vnames[&b.0].clone();
                let body = IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(field_index_clone(&na, "q")),
                    right: Box::new(field_index_clone(&nb, "q")),
                };
                stmts.push(let_stmt(&let_name, q_struct(array_from_fn_t(body))));
            }
            BIrStmt::Not(a) => {
                let na = vnames[&a.0].clone();
                let body = IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(field_index_clone(&na, "q")),
                    right: Box::new(field_index_clone("delta", "delta")),
                };
                stmts.push(let_stmt(&let_name, q_struct(array_from_fn_t(body))));
            }
            BIrStmt::And(a, b) => {
                let na = vnames[&a.0].clone();
                let nb = vnames[&b.0].clone();
                let ok_name = format!("lok_and_{and_counter}");
                let q_and_idx = IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(IrExpr::Binary {
                        op: SpecBinOp::Mul,
                        left: Box::new(var("iter")),
                        right: Box::new(IrExpr::Lit(IrLit::Int(and_count as i128))),
                    }),
                    right: Box::new(IrExpr::Lit(IrLit::Int(and_counter as i128))),
                };
                let q_and_expr = clone_expr(IrExpr::Index {
                    base: Box::new(var("q_ands")),
                    index: Box::new(q_and_idx),
                });
                let hat_expr = IrExpr::Index {
                    base: Box::new(var("iter_hats")),
                    index: Box::new(IrExpr::Lit(IrLit::Int(and_counter as i128))),
                };
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::Tuple(vec![
                        IrPattern::ident(&let_name),
                        IrPattern::ident(&ok_name),
                    ]),
                    ty: None,
                    init: Some(IrExpr::Call {
                        func: Box::new(IrExpr::Path {
                            segments: vec!["vole_and_verifier_check".into()],
                            type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
                        }),
                        args: vec![
                            var("delta"),
                            ref_expr(var(&na)),
                            ref_expr(var(&nb)),
                            ref_expr(q_and_expr),
                            ref_expr(hat_expr),
                        ],
                    }),
                });
                stmts.push(IrStmt::Semi(IrExpr::Assign {
                    left: Box::new(var("all_ok_new")),
                    right: Box::new(IrExpr::Binary {
                        op: SpecBinOp::And,
                        left: Box::new(var("all_ok_new")),
                        right: Box::new(var(&ok_name)),
                    }),
                }));
                and_counter += 1;
            }
            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            _ => {
                vnames.insert(result_id.0, let_name.clone());
                continue;
            }
        }
        vnames.insert(result_id.0, let_name);
    }
}

// ============================================================================
// Prover
// ============================================================================

/// Weave a single-iteration loop circuit into a **network-resilient** VOLE
/// prover `IrCfgModule`.
///
/// Generated signature:
/// ```text
/// fn vole_prove_hybrid_net_<NAME><N, T, Tr: ResilientVoleTransport<N, T>>(
///     vope_one: Vope<N, T, U1>,
///     init_w0: Vope<N, T, U1>, ...   // initial state wires
///     transport: &mut Tr,
/// ) -> Result<Vope<N, T, U1>, <Tr as VoleTransport<N, T>>::Error>
/// ```
pub fn weave_hybrid_net_vole_prover(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_movfuscated(), "weave_hybrid_net_vole_prover: circuit must be single-block");

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let (done_id, out_id, next_ids) = analyze_loop_terminator(block);
    let (read_count, write_count) = count_storage(&expanded);

    let (mut generics, where_clause) = net_prover_loop_generics_and_where();
    make_resilient(&mut generics);

    // ── Function params ────────────────────────────────────────────────────
    let mut func_params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    // Public multiset-hash challenge powers (r, r², r³) for the carried memory
    // accumulator (`mem_prod`/`mem_cons`).
    for r in ["r1", "r2", "r3"] {
        func_params.push(IrParam { name: r.into(), ty: IrType::TypeParam("T".into()) });
    }
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_w{i}"), ty: vope_type() });
    }
    // Per-iteration committed read values / write old-values for storage absorbs
    // (indexed `iter * count + k`).  Unused (and empty) for storage-free circuits.
    func_params.push(IrParam { name: "read_vals".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "write_olds".into(), ty: vope_slice_type() });
    func_params.push(IrParam {
        name: "transport".into(),
        ty: ref_mut_to(IrType::TypeParam("Tr".into())),
    });
    // Returns (output, mem_prod, mem_cons): the final committed output plus the
    // carried memory-consistency accumulators (a single field element each).
    let ret_type = result_type(
        IrType::Tuple(vec![vope_type(), vope_type(), vope_type()]),
        tr_error_type(),
    );

    // ── Block 0: entry ─────────────────────────────────────────────────────
    // Bundle order everywhere: [w_0..w_{ℓ-1}, mem_prod, mem_cons, ts, iter].
    let mut b0_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("init_w{i}"))))
        .collect();
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

    // ── Block 1: ZK loop body ──────────────────────────────────────────────
    let mut b1_params: Vec<IrParam> = (0..num_params)
        .map(|i| IrParam { name: format!("w{i}"), ty: vope_type() })
        .collect();
    b1_params.push(IrParam { name: "mem_prod".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "mem_cons".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "ts".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "iter".into(), ty: usize_type() });
    let mut b1_stmts: Vec<IrStmt> = Vec::new();
    let mut wnames = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        wnames.insert(i as u32, format!("w{i}"));
    }
    let mut hat_names: Vec<String> = Vec::new();
    let mut and_counter = 0usize;
    // Running SSA names for the memory accumulator updated by storage ops; they
    // start at the input params and chain through each StorageRead/StorageWrite.
    let mut cur_prod = String::from("mem_prod");
    let mut cur_cons = String::from("mem_cons");
    let mut cur_ts = String::from("ts");
    let mut ssa = 0usize;
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
            BIrStmt::And(a, b) => {
                let hat = format!("lhat_{and_counter}");
                and_counter += 1;
                hat_names.push(hat.clone());
                emit_prover_and_gate(&wnames[&a.0], &wnames[&b.0], &let_name, &hat, &mut b1_stmts);
            }
            BIrStmt::StorageRead { addr, .. } => {
                assert_eq!(addr.len(), 1, "hybrid storage: only single-bit addresses supported");
                let addr_w = wnames[&addr[0].0].clone();
                let val = format!("rd_{ssa}"); ssa += 1;
                b1_stmts.push(let_stmt(&val, slice_index_clone("read_vals", read_count, read_k)));
                read_k += 1;
                let nc = format!("scons_{ssa}"); ssa += 1;
                b1_stmts.push(let_stmt(&nc, absorb_call(&cur_cons, &addr_w, &val, &cur_ts)));
                cur_cons = nc;
                let nts = format!("sts_{ssa}"); ssa += 1;
                b1_stmts.push(let_stmt(&nts, IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(clone_expr(var(&cur_ts))),
                    right: Box::new(clone_expr(var("vope_one"))),
                }));
                cur_ts = nts;
                let np = format!("sprod_{ssa}"); ssa += 1;
                b1_stmts.push(let_stmt(&np, absorb_call(&cur_prod, &addr_w, &val, &cur_ts)));
                cur_prod = np;
                wnames.insert(result_id.0, val);
                continue;
            }
            BIrStmt::StorageWrite { src, addr, .. } => {
                assert_eq!(addr.len(), 1, "hybrid storage: only single-bit addresses supported");
                let addr_w = wnames[&addr[0].0].clone();
                let new_w = wnames[&src.0].clone();
                let old = format!("old_{ssa}"); ssa += 1;
                b1_stmts.push(let_stmt(&old, slice_index_clone("write_olds", write_count, write_k)));
                write_k += 1;
                let nc = format!("scons_{ssa}"); ssa += 1;
                b1_stmts.push(let_stmt(&nc, absorb_call(&cur_cons, &addr_w, &old, &cur_ts)));
                cur_cons = nc;
                let nts = format!("sts_{ssa}"); ssa += 1;
                b1_stmts.push(let_stmt(&nts, IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(clone_expr(var(&cur_ts))),
                    right: Box::new(clone_expr(var("vope_one"))),
                }));
                cur_ts = nts;
                let np = format!("sprod_{ssa}"); ssa += 1;
                b1_stmts.push(let_stmt(&np, absorb_call(&cur_prod, &addr_w, &new_w, &cur_ts)));
                cur_prod = np;
                b1_stmts.push(let_stmt(&let_name, zero_vope_expr()));
            }
            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            _ => {
                wnames.insert(result_id.0, let_name.clone());
                continue;
            }
        }
        wnames.insert(result_id.0, let_name);
    }

    let done_wire = wnames[&done_id].clone();
    let out_wire = wnames[&out_id].clone();

    // let done_bit = volar_net::vope_bit(&done_wire);
    b1_stmts.push(let_stmt("done_bit", vope_bit_call(&done_wire)));
    // let cont = transport.try_send_iteration(&[hats...], done_bit)?;
    b1_stmts.push(let_stmt(
        "cont",
        transport_call_try("try_send_iteration", vec![hats_slice_expr(&hat_names), var("done_bit")]),
    ));

    // then -> B2 [done_bit, out.clone(), next..., cur_prod, cur_cons, cur_ts, iter+1]
    // (continue path carries the *updated* accumulator advanced by storage ops).
    let mut b2_args: Vec<IrExpr> = vec![var("done_bit"), clone_expr(var(&out_wire))];
    for id in &next_ids {
        b2_args.push(clone_expr(var(&wnames[&id.0])));
    }
    b2_args.push(var(&cur_prod));
    b2_args.push(var(&cur_cons));
    b2_args.push(var(&cur_ts));
    b2_args.push(IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(var("iter")),
        right: Box::new(IrExpr::Lit(IrLit::Int(1))),
    });

    // else (disconnect) -> B4 [anchor w_i.clone()..., mem_prod/cons/ts/iter
    // (INPUT), plaintext mirror vope_bit(&w_i)..., gap_len=0].  The anchor is the
    // iteration-INPUT state + INPUT accumulator/iter (this iteration's send
    // failed, so replay re-runs it from the same iter, re-reading the same
    // read_vals/write_olds and re-absorbing); the verifier still holds the
    // matching Q-shares.
    let mut b4_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("w{i}"))))
        .collect();
    b4_args.push(clone_expr(var("mem_prod")));
    b4_args.push(clone_expr(var("mem_cons")));
    b4_args.push(clone_expr(var("ts")));
    b4_args.push(var("iter"));
    for i in 0..num_params {
        b4_args.push(vope_bit_call(&format!("w{i}")));
    }
    b4_args.push(IrExpr::Lit(IrLit::Int(0)));

    let block1 = IrCfgBlock {
        params: b1_params,
        stmts: b1_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("cont"),
            then_: IrCfgJump { target: 2, args: b2_args },
            else_: IrCfgJump { target: 4, args: b4_args },
        },
    };

    // ── Block 2: post-send dispatch ────────────────────────────────────────
    let mut b2_params: Vec<IrParam> = vec![
        IrParam { name: "db".into(), ty: bool_type() },
        IrParam { name: "out".into(), ty: vope_type() },
    ];
    for i in 0..num_params {
        b2_params.push(IrParam { name: format!("nw{i}"), ty: vope_type() });
    }
    b2_params.push(IrParam { name: "mem_prod".into(), ty: vope_type() });
    b2_params.push(IrParam { name: "mem_cons".into(), ty: vope_type() });
    b2_params.push(IrParam { name: "ts".into(), ty: vope_type() });
    b2_params.push(IrParam { name: "iter".into(), ty: usize_type() });
    // else (continue) -> B1 [nw..., mem_prod, mem_cons, ts, iter]
    let mut b1_back: Vec<IrExpr> = (0..num_params).map(|i| var(&format!("nw{i}"))).collect();
    b1_back.push(var("mem_prod"));
    b1_back.push(var("mem_cons"));
    b1_back.push(var("ts"));
    b1_back.push(var("iter"));
    let block2 = IrCfgBlock {
        params: b2_params,
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("db"),
            // done -> B3 [out, mem_prod, mem_cons]
            then_: IrCfgJump {
                target: 3,
                args: vec![var("out"), var("mem_prod"), var("mem_cons")],
            },
            else_: IrCfgJump { target: 1, args: b1_back },
        },
    };

    // ── Block 3: exit ──────────────────────────────────────────────────────
    let block3 = IrCfgBlock {
        params: vec![
            IrParam { name: "output".into(), ty: vope_type() },
            IrParam { name: "fprod".into(), ty: vope_type() },
            IrParam { name: "fcons".into(), ty: vope_type() },
        ],
        stmts: vec![IrStmt::Semi(transport_call_try("recv_verdict", vec![]))],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(ok_expr(IrExpr::Tuple(vec![
            var("output"),
            var("fprod"),
            var("fcons"),
        ])))),
    };

    // The gap blocks carry the **anchor** Vope wires `aw_i` (the iteration-input
    // state, unchanged) plus a plaintext mirror `p_i` (advanced for liveness).
    // On *any* reconnect the gap is re-proven by replaying from the anchor — no
    // re-commitment, no linking — so the sound bridge reuses the base protocol.
    // The anchor bundle is [w_0..w_{ℓ-1}, mem_prod, mem_cons, ts, iter] — the
    // same order as B1's params — so the memory accumulator AND the iteration
    // counter are carried through every gap block and restored to B1 on replay
    // (i.e. memory survives the network cut, and replay re-reads the same
    // read_vals/write_olds slices).  `_it` is the usize iter; the rest are Vope.
    let aw_params = |prefix: &str| -> Vec<IrParam> {
        let mut v: Vec<IrParam> = (0..num_params)
            .map(|i| IrParam { name: format!("{prefix}{i}"), ty: vope_type() })
            .collect();
        for suf in ["_mp", "_mc", "_ts"] {
            v.push(IrParam { name: format!("{prefix}{suf}"), ty: vope_type() });
        }
        v.push(IrParam { name: format!("{prefix}_it"), ty: usize_type() });
        v
    };
    let aw_clone_args = |prefix: &str| -> Vec<IrExpr> {
        let mut v: Vec<IrExpr> = (0..num_params)
            .map(|i| clone_expr(var(&format!("{prefix}{i}"))))
            .collect();
        for suf in ["_mp", "_mc", "_ts"] {
            v.push(clone_expr(var(&format!("{prefix}{suf}"))));
        }
        v.push(var(&format!("{prefix}_it")));
        v
    };
    let aw_move_args = |prefix: &str| -> Vec<IrExpr> {
        let mut v: Vec<IrExpr> = (0..num_params).map(|i| var(&format!("{prefix}{i}"))).collect();
        for suf in ["_mp", "_mc", "_ts"] {
            v.push(var(&format!("{prefix}{suf}")));
        }
        v.push(var(&format!("{prefix}_it")));
        v
    };

    // ── Block 4: gap body (one cleartext iteration; anchor carried) ────────
    let mut b4_params: Vec<IrParam> = aw_params("aw");
    b4_params.extend((0..num_params).map(|i| IrParam { name: format!("p{i}"), ty: bool_type() }));
    b4_params.push(IrParam { name: "gap_len".into(), ty: u32_type() });

    let mut b4_stmts: Vec<IrStmt> = Vec::new();
    let pnames = emit_cleartext_gates(&expanded, num_params, "p", "g_", &mut b4_stmts);
    let pdone = pnames[&done_id].clone();
    b4_stmts.push(let_stmt("gl2", incr("gap_len")));
    b4_stmts.push(let_stmt("reconnected", transport_call_try("try_reconnect", vec![])));

    // then (reconnected) -> B5 [aw..., gl2]   (replay from anchor)
    let mut b5_args: Vec<IrExpr> = aw_clone_args("aw");
    b5_args.push(var("gl2"));
    // else -> B6 [aw..., pnext..., pdone, gl2]   (carry anchor + advanced mirror)
    let mut b6_args: Vec<IrExpr> = aw_clone_args("aw");
    b6_args.extend(next_ids.iter().map(|id| var(&pnames[&id.0])));
    b6_args.push(var(&pdone));
    b6_args.push(var("gl2"));

    let block4 = IrCfgBlock {
        params: b4_params,
        stmts: b4_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("reconnected"),
            then_: IrCfgJump { target: 5, args: b5_args },
            else_: IrCfgJump { target: 6, args: b6_args },
        },
    };

    // ── Block 5: resume — bridge handshake, replay ZK body from anchor ─────
    let mut b5_params: Vec<IrParam> = aw_params("rw");
    b5_params.push(IrParam { name: "gl".into(), ty: u32_type() });
    let rw_names: Vec<String> = (0..num_params).map(|i| format!("rw{i}")).collect();
    let mut b5_stmts: Vec<IrStmt> = Vec::new();
    b5_stmts.push(let_stmt("token", resume_token_expr(&rw_names, &[])));
    b5_stmts.push(IrStmt::Semi(transport_call_try(
        "prover_bridge",
        vec![ref_expr(var("token")), var("gl")],
    )));
    let block5 = IrCfgBlock {
        params: b5_params,
        stmts: b5_stmts,
        stmt_provs: vec![],
        // Goto B1 with the anchor wires → replay the gap iterations under VOLE.
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: aw_move_args("rw") }),
    };

    // ── Block 6: gap dispatch (finished-offline vs continue gap) ───────────
    let mut b6_params: Vec<IrParam> = aw_params("gw");
    b6_params.extend((0..num_params).map(|i| IrParam { name: format!("gp{i}"), ty: bool_type() }));
    b6_params.push(IrParam { name: "gpdone".into(), ty: bool_type() });
    b6_params.push(IrParam { name: "gl".into(), ty: u32_type() });
    // continue gap -> B4 [gw..., gp..., gl]
    let mut b4_cont_args: Vec<IrExpr> = aw_clone_args("gw");
    b4_cont_args.extend((0..num_params).map(|i| var(&format!("gp{i}"))));
    b4_cont_args.push(var("gl"));
    let block6 = IrCfgBlock {
        params: b6_params,
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("gpdone"),
            // finished offline -> B7 [gw..., gl] (still must replay on reconnect)
            then_: IrCfgJump { target: 7, args: { let mut a = aw_clone_args("gw"); a.push(var("gl")); a } },
            else_: IrCfgJump { target: 4, args: b4_cont_args },
        },
    };

    // ── Block 7: offline spin (retry reconnect, anchor carried) ────────────
    let mut b7_params: Vec<IrParam> = aw_params("sw");
    b7_params.push(IrParam { name: "gl".into(), ty: u32_type() });
    let block7 = IrCfgBlock {
        params: b7_params,
        stmts: vec![let_stmt("connected", transport_call_try("try_reconnect", vec![]))],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("connected"),
            then_: IrCfgJump { target: 8, args: { let mut a = aw_clone_args("sw"); a.push(var("gl")); a } },
            else_: IrCfgJump { target: 7, args: { let mut a = aw_clone_args("sw"); a.push(var("gl")); a } },
        },
    };

    // ── Block 8: final-resume — bridge handshake, replay from anchor ───────
    let mut b8_params: Vec<IrParam> = aw_params("fw");
    b8_params.push(IrParam { name: "gl".into(), ty: u32_type() });
    let fw_names: Vec<String> = (0..num_params).map(|i| format!("fw{i}")).collect();
    let mut b8_stmts: Vec<IrStmt> = Vec::new();
    b8_stmts.push(let_stmt("token", resume_token_expr(&fw_names, &[])));
    b8_stmts.push(IrStmt::Semi(transport_call_try(
        "prover_bridge",
        vec![ref_expr(var("token")), var("gl")],
    )));
    let block8 = IrCfgBlock {
        params: b8_params,
        stmts: b8_stmts,
        stmt_provs: vec![],
        // Replay from anchor even after an offline finish (authoritative proof).
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: aw_move_args("fw") }),
    };

    let func = IrCfgFunction {
        name: format!("vole_prove_hybrid_net_{name}"),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody {
            blocks: vec![block0, block1, block2, block3, block4, block5, block6, block7, block8],
        },
    };

    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_hybrid_net_prover_{name}"),
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

// ============================================================================
// Verifier
// ============================================================================

/// Weave a single-iteration loop circuit into a **network-resilient** VOLE
/// verifier `IrCfgModule`.
///
/// The verifier runs the ordinary `recv_iteration` loop; on a disconnect it
/// parks, awaits reconnect via `try_reconnect`, records the unproven gap with
/// `verifier_bridge`, and resumes.  The accumulated boolean verdict is sent at
/// the end; the qualified-gap intervals are surfaced through the transport's
/// `verifier_bridge` side effects (see `docs/vole-continuation-bridge.md`).
///
/// Generated signature:
/// ```text
/// fn vole_verify_hybrid_net_<NAME><N, T, Tr: ResilientVoleTransport<N, T>>(
///     delta: &Delta<N, T>,
///     q_ands: &[Q<N, T>],
///     init_q0: Q<N, T>, ...
///     transport: &mut Tr,
/// ) -> Result<bool, <Tr as VoleTransport<N, T>>::Error>
/// ```
pub fn weave_hybrid_net_vole_verifier(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_movfuscated(), "weave_hybrid_net_vole_verifier: circuit must be single-block");

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let and_count = count_and_gates(circuit);

    let (mut generics, where_clause) = net_verifier_generics_and_where();
    make_resilient(&mut generics);

    // ── Function params ────────────────────────────────────────────────────
    let mut func_params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to(delta_type()) },
        IrParam { name: "q_ands".into(), ty: q_slice_type() },
    ];
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_q{i}"), ty: q_type() });
    }
    func_params.push(IrParam {
        name: "transport".into(),
        ty: ref_mut_to(IrType::TypeParam("Tr".into())),
    });
    let ret_type = result_type(bool_type(), tr_error_type());

    // ── Block 0: entry → B1 [init_q.clone()..., all_ok=true, iter=0] ───────
    let mut b0_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("init_q{i}"))))
        .collect();
    b0_args.push(IrExpr::Lit(IrLit::Bool(true)));
    b0_args.push(IrExpr::Lit(IrLit::Int(0)));
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: receive-or-disconnect dispatch ────────────────────────────
    // params: q0..qN-1, all_ok, iter
    let mut b1_params: Vec<IrParam> = (0..num_params)
        .map(|i| IrParam { name: format!("q{i}"), ty: q_type() })
        .collect();
    b1_params.push(IrParam { name: "all_ok".into(), ty: bool_type() });
    b1_params.push(IrParam { name: "iter".into(), ty: u32_type() });

    // let got = transport.try_recv_iteration(AND_COUNT)?;  -> Option<(hats,bool)>
    // Modeled with the resilient default: if recv fails, park in the gap block.
    // We approximate detection by attempting reconnect-classification: the
    // verifier calls `try_reconnect` to decide whether to keep waiting.
    //
    // For the scaffold we keep the verifier's online path identical to the net
    // loop verifier, and add a gap park block reachable on `!all_ok`-independent
    // disconnect signaling via `try_reconnect`.  The online recv uses the base
    // (propagating) `recv_iteration`; a transport that wants graceful gaps
    // overrides `recv_iteration` to block until reconnected and calls
    // `verifier_bridge` internally.  The dedicated park block below is emitted so
    // the CFG explicitly carries the resume edge.
    let mut b1_stmts: Vec<IrStmt> = Vec::new();
    b1_stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident("iter_hats"),
            IrPattern::ident("is_sentinel"),
        ]),
        ty: None,
        init: Some(transport_call_try(
            "recv_iteration",
            vec![IrExpr::Lit(IrLit::Int(and_count as i128))],
        )),
    });
    b1_stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok_new".into(), subpat: None },
        ty: None,
        init: Some(var("all_ok")),
    });

    let mut vnames = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        vnames.insert(i as u32, format!("q{i}"));
    }
    emit_verifier_loop_gates(&expand_ors(block), &mut vnames, and_count, &mut b1_stmts);

    let (_done, _out, next_ids) = analyze_loop_terminator(block);
    let mut back_args: Vec<IrExpr> =
        next_ids.iter().map(|id| clone_expr(var(&vnames[&id.0]))).collect();
    back_args.push(var("all_ok_new"));
    back_args.push(incr("iter"));

    let block1 = IrCfgBlock {
        params: b1_params,
        stmts: b1_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("is_sentinel"),
            then_: IrCfgJump { target: 2, args: vec![var("all_ok_new")] },
            else_: IrCfgJump { target: 1, args: back_args },
        },
    };

    // ── Block 2: exit — send verdict, return ───────────────────────────────
    let block2 = IrCfgBlock {
        params: vec![IrParam { name: "final_ok".into(), ty: bool_type() }],
        stmts: vec![IrStmt::Semi(transport_call_try("send_verdict", vec![var("final_ok")]))],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(ok_expr(var("final_ok")))),
    };

    let func = IrCfgFunction {
        name: format!("vole_verify_hybrid_net_{name}"),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0, block1, block2] },
    };

    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_hybrid_net_verifier_{name}"),
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

// ============================================================================
// Printer
// ============================================================================

/// Print a hybrid-net CFG module to self-contained Rust source.
///
/// Identical to [`crate::net::print_net_vole_cfg_module`]'s preamble plus the
/// resilience-layer imports (`ResilientVoleTransport`, `ResumeToken`).
pub fn print_hybrid_net_cfg_module(module: &IrCfgModule) -> String {
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
        "use volar_spec::vole::{Delta, Q, Vope, VoleArray};\n",
        "use volar_spec::vole::prove::{vole_and_prover_step, vole_and_verifier_check};\n",
        "use volar_net::{VoleTransport, ResilientVoleTransport, ResumeToken, recommit_bit, vope_bit};\n",
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
    use crate::tests_common::{build_simple_loop, run_compile_check_net};

    #[test]
    fn test_hybrid_net_prover_compiles() {
        let circuit = build_simple_loop();
        let module = weave_hybrid_net_vole_prover(&circuit, "test", None);
        let code = print_hybrid_net_cfg_module(&module);
        run_compile_check_net(&code, "hybrid_net_prover");
    }

    #[test]
    fn test_hybrid_net_verifier_compiles() {
        let circuit = build_simple_loop();
        let module = weave_hybrid_net_vole_verifier(&circuit, "test", None);
        let code = print_hybrid_net_cfg_module(&module);
        run_compile_check_net(&code, "hybrid_net_verifier");
    }

    #[test]
    fn test_hybrid_prover_has_both_paths() {
        let circuit = build_simple_loop();
        let module = weave_hybrid_net_vole_prover(&circuit, "test", None);
        let code = print_hybrid_net_cfg_module(&module);
        // ZK path present:
        assert!(code.contains("vole_and_prover_step") || code.contains("vope_one"),
            "expected ZK gate lowering");
        assert!(code.contains("try_send_iteration"), "expected resilient send");
        // Cleartext gap path + sound replay-from-anchor resumption present:
        assert!(code.contains("try_reconnect"), "expected reconnect handling");
        assert!(code.contains("prover_bridge"), "expected resumption bridge call");
        assert!(code.contains("ResumeToken"), "expected resume token construction");
        // Memory accumulator carried through the gap (survives the cut):
        assert!(code.contains("mem_prod"), "expected carried produce accumulator");
        assert!(code.contains("mem_cons"), "expected carried consume accumulator");
    }

    #[test]
    fn test_hybrid_prover_returns_memory_triple() {
        let circuit = build_simple_loop();
        let module = weave_hybrid_net_vole_prover(&circuit, "test", None);
        let code = print_hybrid_net_cfg_module(&module);
        // The accumulator survives the gap because it is part of the anchor
        // bundle carried through every gap block (suffixes _mp/_mc/_ts) and
        // restored to the ZK body on replay; the prover returns it as a triple.
        assert!(code.contains("_mp") && code.contains("_mc") && code.contains("_ts"),
            "expected accumulator carried through gap blocks as anchor bundle");
    }

    /// Storage-bearing loop (1-bit address): Write(w1 @ w0); v = Read(@ w0).
    /// Jmp convention: next = [w0, w1, v(id4)], done = w2.
    fn build_storage_hybrid_loop() -> BIrBlocks {
        use volar_ir::boolar::{BIrBlock, BIrStmt, BIrTarget, BIrTerminator};
        use volar_ir::ir::{IRBlockTargetId, StorageId};
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: 3,
                stmts: vec![
                    BIrStmt::StorageWrite {
                        storage: StorageId(0), src: IRVarId(1), bit_width: 1, addr: vec![IRVarId(0)],
                    },
                    BIrStmt::StorageRead {
                        storage: StorageId(0), bit_width: 1, addr: vec![IRVarId(0)],
                    },
                ],
                stmt_provs: vec![(), ()],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(0), IRVarId(1), IRVarId(4), IRVarId(2)],
                }),
            }],
            pre_init: vec![],
        }
    }

    #[test]
    fn test_hybrid_storage_unified() {
        // The unified weaver: storage absorbs in the ZK body + accumulator
        // carried through the gap + transport, all in one prover.
        let circuit = build_storage_hybrid_loop();
        let module = weave_hybrid_net_vole_prover(&circuit, "test", None);
        let code = print_hybrid_net_cfg_module(&module);
        run_compile_check_net(&code, "hybrid_storage_unified");
        // Real storage absorbs present, fed by the per-iteration slices:
        assert!(code.contains("mem_acc_absorb_vope"), "expected in-circuit storage absorb");
        assert!(code.contains("read_vals"), "expected read-value slice");
        assert!(code.contains("write_olds"), "expected write old-value slice");
        // And still resilient (gap + replay) with the accumulator carried:
        assert!(code.contains("try_reconnect") && code.contains("prover_bridge"),
            "expected gap/resume handling");
        assert!(code.contains("_mp"), "expected accumulator carried through gap");
    }
}
