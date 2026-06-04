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
/// `volar_spec::vole::bridge::mem_acc_absorb_q(acc.clone(), &addr, &value, &ts, &r1, &r2, &r3)`
fn absorb_q_call(acc: &str, addr: &str, value: &str, ts: &str) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["volar_spec".into(), "vole".into(), "bridge".into(), "mem_acc_absorb_q".into()],
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

/// Verifier `Q` timestamp increment: `Q { q: from_fn(|i| cur_ts.q[i] + delta.delta[i]) }`
/// (adds the verifier's committed-1 share `Δ`, mirroring the prover's `ts + vope_one`).
fn q_ts_incr(cur_ts: &str) -> IrExpr {
    q_struct(array_from_fn_t(IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(field_index_clone(cur_ts, "q")),
        right: Box::new(field_index_clone("delta", "delta")),
    }))
}

#[allow(clippy::too_many_arguments)]
fn emit_verifier_loop_gates(
    expanded: &[(IRVarId, BIrStmt, ())],
    vnames: &mut BTreeMap<u32, String>,
    andc: usize,
    read_count: usize,
    write_count: usize,
    ts_bits: usize,
    addr_bits: usize,
    pow2_ts: &[String],
    pow2_addr: &[String],
    cur_prod: &mut String,
    cur_cons: &mut String,
    cur_order: &mut String,
    cur_cnt: &mut Vec<String>,
    stmts: &mut Vec<IrStmt>,
) {
    let and_count = andc; // total per-iteration ANDs (circuit + ts gadgets)
    let b = ts_bits;
    let mut and_counter = 0usize;
    let mut ssa = 0usize;
    let mut read_k = 0usize;
    let mut write_k = 0usize;
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
            BIrStmt::StorageRead { addr, .. } => {
                assert_eq!(addr.len(), addr_bits, "hybrid storage: address width must equal addr_bits");
                let addr_q = format!("vaf_{ssa}");
                let bits: Vec<String> = addr.iter().map(|a| vnames[&a.0].clone()).collect();
                stmts.push(let_stmt(&addr_q, crate::storage_loop::q_bitpack_call(&bits, pow2_addr)));
                let val = format!("vrd_{ssa}_v");
                stmts.push(let_stmt(&val, slice_index_clone("read_vals_q", read_count, read_k)));
                crate::storage_loop::emit_ts_access_verifier(
                    &addr_q, &val, &val, "q_read_last_ts", read_count * b, read_k, b, pow2_ts,
                    and_count, &mut ssa, &mut and_counter, cur_prod, cur_cons, cur_order, cur_cnt,
                    stmts,
                );
                read_k += 1;
                vnames.insert(result_id.0, val);
                continue;
            }
            BIrStmt::StorageWrite { src, addr, .. } => {
                assert_eq!(addr.len(), addr_bits, "hybrid storage: address width must equal addr_bits");
                let addr_q = format!("vaf_{ssa}");
                let bits: Vec<String> = addr.iter().map(|a| vnames[&a.0].clone()).collect();
                stmts.push(let_stmt(&addr_q, crate::storage_loop::q_bitpack_call(&bits, pow2_addr)));
                let new_q = vnames[&src.0].clone();
                let old = format!("vold_{ssa}_old");
                stmts.push(let_stmt(&old, slice_index_clone("write_olds_q", write_count, write_k)));
                crate::storage_loop::emit_ts_access_verifier(
                    &addr_q, &old, &new_q, "q_write_last_ts", write_count * b, write_k, b, pow2_ts,
                    and_count, &mut ssa, &mut and_counter, cur_prod, cur_cons, cur_order, cur_cnt,
                    stmts,
                );
                write_k += 1;
                stmts.push(let_stmt(&let_name, q_struct(array_t_default())));
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
    ts_bits: usize,
    addr_bits: usize,
    touched_count: usize,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_movfuscated(), "weave_hybrid_net_vole_prover: circuit must be single-block");
    assert!(ts_bits >= 1, "ts_bits must be >= 1");
    assert!((1..=64).contains(&addr_bits), "addr_bits must be 1..=64");

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let (done_id, out_id, next_ids) = analyze_loop_terminator(block);
    let (read_count, write_count) = count_storage(&expanded);
    let b = ts_bits;
    let k = touched_count;
    // Committed timestamp counter bits + ordering accumulator carried in the bundle.
    let cnt_names: Vec<String> = (0..b).map(|j| format!("cnt{j}")).collect();
    let npow = b.max(addr_bits);
    let pow2_names: Vec<String> = (0..npow).map(|j| format!("pow2_{j}")).collect();
    let pow2_ts: Vec<String> = pow2_names[..b].to_vec();
    let pow2_addr: Vec<String> = pow2_names[..addr_bits].to_vec();
    // Accumulator suffixes threaded through the gap bundle (after the ℓ state wires,
    // before `_it`): produce/consume hashes, the `b` counter bits, ordering wire.
    let acc_sufs: Vec<String> = {
        let mut v = vec![String::from("_mp"), String::from("_mc")];
        for j in 0..b {
            v.push(format!("_ts{j}"));
        }
        v.push(String::from("_ord"));
        v
    };

    let (mut generics, where_clause) = net_prover_loop_generics_and_where();
    make_resilient(&mut generics);

    // ── Function params ────────────────────────────────────────────────────
    let mut func_params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    // Public multiset-hash challenge powers (r, r², r³) for the carried memory
    // accumulator (`mem_prod`/`mem_cons`).
    for r in ["r0", "r1", "r2", "r3"] {
        func_params.push(IrParam { name: r.into(), ty: IrType::TypeParam("T".into()) });
    }
    // Public bit-weights for the committed-timestamp bit-pack (`Σ bit·2^j`).
    for n in &pow2_names {
        func_params.push(IrParam { name: n.clone(), ty: IrType::TypeParam("T".into()) });
    }
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_w{i}"), ty: vope_type() });
    }
    // Per-iteration committed read values / write old-values for storage absorbs
    // (indexed `iter * count + k`).  Unused (and empty) for storage-free circuits.
    func_params.push(IrParam { name: "read_vals".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "write_olds".into(), ty: vope_slice_type() });
    // Committed per-access last-write timestamp bit-vectors (ts-soundness ordering).
    func_params.push(IrParam { name: "read_last_ts".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "write_last_ts".into(), ty: vope_slice_type() });
    // Sparse touched-cell witnesses (ADR 0002 Option A): K cells, not 2^addr_bits.
    func_params.push(IrParam { name: "touched_addr".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "touched_init_val".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "touched_final_val".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "touched_final_ts".into(), ty: vope_slice_type() });
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
    // Bundle order everywhere:
    //   [w_0..w_{ℓ-1}, mem_prod, mem_cons, cnt_0..cnt_{B-1}, order_ok, iter].
    // Sparse init: produce the K touched cells at ts=0 (ADR 0002 Option A).
    let mut b0_stmts: Vec<IrStmt> = Vec::new();
    let init_cur = crate::storage_loop::emit_touched_init_prover(k, addr_bits, &pow2_addr, &mut b0_stmts);
    let mut b0_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("init_w{i}"))))
        .collect();
    b0_args.push(clone_expr(var(&init_cur))); // mem_prod (cells initialised)
    b0_args.push(zero_vope_expr()); // mem_cons
    b0_args.push(clone_expr(var("vope_one"))); // cnt bit 0 = 1 (init ts 0 < first produce 1)
    for _ in 1..b {
        b0_args.push(zero_vope_expr()); // cnt bits 1.. = 0
    }
    b0_args.push(clone_expr(var("vope_one"))); // order_ok = true
    b0_args.push(IrExpr::Lit(IrLit::Int(0))); // iter
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: b0_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: ZK loop body ──────────────────────────────────────────────
    let mut b1_params: Vec<IrParam> = (0..num_params)
        .map(|i| IrParam { name: format!("w{i}"), ty: vope_type() })
        .collect();
    b1_params.push(IrParam { name: "mem_prod".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "mem_cons".into(), ty: vope_type() });
    for n in &cnt_names {
        b1_params.push(IrParam { name: n.clone(), ty: vope_type() });
    }
    b1_params.push(IrParam { name: "order_ok".into(), ty: vope_type() });
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
    let mut cur_order = String::from("order_ok");
    let mut cur_cnt: Vec<String> = cnt_names.clone();
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
                assert_eq!(addr.len(), addr_bits, "hybrid storage: address width must equal addr_bits");
                let addr_w = format!("af_{ssa}");
                let bits: Vec<String> = addr.iter().map(|a| wnames[&a.0].clone()).collect();
                b1_stmts.push(let_stmt(&addr_w, crate::storage_loop::bitpack_call(&bits, &pow2_addr)));
                let val = format!("rd_{ssa}_v");
                b1_stmts.push(let_stmt(&val, slice_index_clone("read_vals", read_count, read_k)));
                crate::storage_loop::emit_ts_access_prover(
                    &addr_w, &val, &val, "read_last_ts", read_count * b, read_k, b, &pow2_ts,
                    &mut ssa, &mut cur_prod, &mut cur_cons, &mut cur_order, &mut cur_cnt,
                    &mut hat_names, &mut b1_stmts,
                );
                read_k += 1;
                wnames.insert(result_id.0, val);
                continue;
            }
            BIrStmt::StorageWrite { src, addr, .. } => {
                assert_eq!(addr.len(), addr_bits, "hybrid storage: address width must equal addr_bits");
                let addr_w = format!("af_{ssa}");
                let bits: Vec<String> = addr.iter().map(|a| wnames[&a.0].clone()).collect();
                b1_stmts.push(let_stmt(&addr_w, crate::storage_loop::bitpack_call(&bits, &pow2_addr)));
                let new_w = wnames[&src.0].clone();
                let old = format!("wr_{ssa}_old");
                b1_stmts.push(let_stmt(&old, slice_index_clone("write_olds", write_count, write_k)));
                crate::storage_loop::emit_ts_access_prover(
                    &addr_w, &old, &new_w, "write_last_ts", write_count * b, write_k, b, &pow2_ts,
                    &mut ssa, &mut cur_prod, &mut cur_cons, &mut cur_order, &mut cur_cnt,
                    &mut hat_names, &mut b1_stmts,
                );
                write_k += 1;
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
    for n in &cur_cnt {
        b2_args.push(clone_expr(var(n)));
    }
    b2_args.push(var(&cur_order));
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
    for n in &cnt_names {
        b4_args.push(clone_expr(var(n))); // INPUT counter bits (anchor)
    }
    b4_args.push(clone_expr(var("order_ok"))); // INPUT order_ok (anchor)
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
    for n in &cnt_names {
        b2_params.push(IrParam { name: n.clone(), ty: vope_type() });
    }
    b2_params.push(IrParam { name: "order_ok".into(), ty: vope_type() });
    b2_params.push(IrParam { name: "iter".into(), ty: usize_type() });
    // else (continue) -> B1 [nw..., mem_prod, mem_cons, cnt..., order_ok, iter]
    let mut b1_back: Vec<IrExpr> = (0..num_params).map(|i| var(&format!("nw{i}"))).collect();
    b1_back.push(var("mem_prod"));
    b1_back.push(var("mem_cons"));
    for n in &cnt_names {
        b1_back.push(var(n));
    }
    b1_back.push(var("order_ok"));
    b1_back.push(var("iter"));
    let block2 = IrCfgBlock {
        params: b2_params,
        stmts: vec![],
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("db"),
            // done -> B3 [out, mem_prod, mem_cons, order_ok]
            then_: IrCfgJump {
                target: 3,
                args: vec![var("out"), var("mem_prod"), var("mem_cons"), var("order_ok")],
            },
            else_: IrCfgJump { target: 1, args: b1_back },
        },
    };

    // ── Block 3: exit ──────────────────────────────────────────────────────
    // Drain both single-bit cells (consume each cell's committed final
    // `(addr, value, ts)`), open the memory-consistency drain (mask of
    // mem_prod − mem_cons) for `mem_drain_check`, open the timestamp-ordering
    // result for `assert_one_check`, then receive the verdict.
    let bridge_fn = |fname: &str, args: Vec<IrExpr>| IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["volar_spec".into(), "vole".into(), "bridge".into(), fname.into()],
            type_args: vec![],
        }),
        args,
    };
    let mut b3_stmts: Vec<IrStmt> = Vec::new();
    let mut cur_order_x = String::from("forder");
    let mut sort_hats: Vec<String> = Vec::new();
    let drain_cur = crate::storage_loop::emit_touched_drain_prover(
        k, addr_bits, b, &pow2_addr, &pow2_ts, "fcons", &mut cur_order_x, &mut sort_hats,
        &mut b3_stmts,
    );
    b3_stmts.extend([
        // one-shot sortedness-gadget hats (separate from the streaming loop)
        IrStmt::Semi(transport_call_try("send_hats", vec![hats_slice_expr(&sort_hats)])),
        let_stmt("mem_opening", bridge_fn("mem_drain_open", vec![ref_expr(var("fprod")), ref_expr(var(&drain_cur))])),
        IrStmt::Semi(transport_call_try("send_mem_opening", vec![ref_expr(var("mem_opening"))])),
        let_stmt("order_opening", bridge_fn("vope_open_mask", vec![ref_expr(var(&cur_order_x))])),
        IrStmt::Semi(transport_call_try("send_opening", vec![ref_expr(var("order_opening"))])),
        IrStmt::Semi(transport_call_try("recv_verdict", vec![])),
    ]);
    let block3 = IrCfgBlock {
        params: vec![
            IrParam { name: "output".into(), ty: vope_type() },
            IrParam { name: "fprod".into(), ty: vope_type() },
            IrParam { name: "fcons".into(), ty: vope_type() },
            IrParam { name: "forder".into(), ty: vope_type() },
        ],
        stmts: core::mem::take(&mut b3_stmts),
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(ok_expr(IrExpr::Tuple(vec![
            var("output"),
            var("fprod"),
            var(&drain_cur),
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
        for suf in &acc_sufs {
            v.push(IrParam { name: format!("{prefix}{suf}"), ty: vope_type() });
        }
        v.push(IrParam { name: format!("{prefix}_it"), ty: usize_type() });
        v
    };
    let aw_clone_args = |prefix: &str| -> Vec<IrExpr> {
        let mut v: Vec<IrExpr> = (0..num_params)
            .map(|i| clone_expr(var(&format!("{prefix}{i}"))))
            .collect();
        for suf in &acc_sufs {
            v.push(clone_expr(var(&format!("{prefix}{suf}"))));
        }
        v.push(var(&format!("{prefix}_it")));
        v
    };
    let aw_move_args = |prefix: &str| -> Vec<IrExpr> {
        let mut v: Vec<IrExpr> = (0..num_params).map(|i| var(&format!("{prefix}{i}"))).collect();
        for suf in &acc_sufs {
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
    ts_bits: usize,
    addr_bits: usize,
    touched_count: usize,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_movfuscated(), "weave_hybrid_net_vole_verifier: circuit must be single-block");
    assert!(ts_bits >= 1, "ts_bits must be >= 1");
    assert!((1..=64).contains(&addr_bits), "addr_bits must be 1..=64");

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let circuit_ands = count_and_gates(circuit);
    let (read_count, write_count) = count_storage(&expand_ors(block));
    let b = ts_bits;
    let k = touched_count;
    // Total per-iteration ANDs = circuit ANDs + ts-gadget ANDs (ordering + counter).
    let andc = circuit_ands
        + crate::storage_loop::ts_iter_and_count(b, read_count + write_count);
    let cnt_names: Vec<String> = (0..b).map(|j| format!("qcnt{j}")).collect();
    let npow = b.max(addr_bits);
    let pow2_names: Vec<String> = (0..npow).map(|j| format!("pow2_{j}")).collect();
    let pow2_ts: Vec<String> = pow2_names[..b].to_vec();
    let pow2_addr: Vec<String> = pow2_names[..addr_bits].to_vec();

    let (mut generics, where_clause) = net_verifier_generics_and_where();
    make_resilient(&mut generics);

    // ── Function params ────────────────────────────────────────────────────
    let mut func_params: Vec<IrParam> = vec![
        IrParam { name: "delta".into(), ty: ref_to(delta_type()) },
        IrParam { name: "q_ands".into(), ty: q_slice_type() },
        IrParam { name: "q_ands_sort".into(), ty: q_slice_type() },
    ];
    for r in ["r0", "r1", "r2", "r3"] {
        func_params.push(IrParam { name: r.into(), ty: IrType::TypeParam("T".into()) });
    }
    for n in &pow2_names {
        func_params.push(IrParam { name: n.clone(), ty: IrType::TypeParam("T".into()) });
    }
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_q{i}"), ty: q_type() });
    }
    // Verifier Q-shares of the per-iteration committed read values / write
    // old-values (mirror of the prover's `read_vals`/`write_olds`).
    func_params.push(IrParam { name: "read_vals_q".into(), ty: q_slice_type() });
    func_params.push(IrParam { name: "write_olds_q".into(), ty: q_slice_type() });
    func_params.push(IrParam { name: "q_read_last_ts".into(), ty: q_slice_type() });
    func_params.push(IrParam { name: "q_write_last_ts".into(), ty: q_slice_type() });
    // Sparse touched-cell witnesses (Q mirror).
    for s in ["q_touched_addr", "q_touched_init_val", "q_touched_final_val", "q_touched_final_ts"] {
        func_params.push(IrParam { name: s.into(), ty: q_slice_type() });
    }
    func_params.push(IrParam {
        name: "transport".into(),
        ty: ref_mut_to(IrType::TypeParam("Tr".into())),
    });
    let ret_type = result_type(bool_type(), tr_error_type());

    // Bundle order: [q_0..q_{ℓ-1}, all_ok, iter, mem_prod_q, mem_cons_q,
    //                qcnt_0..qcnt_{B-1}, q_order].
    let q_zero = || q_struct(array_t_default());
    let q_one = || {
        q_struct(IrExpr::MethodCall {
            receiver: Box::new(IrExpr::Field { base: Box::new(var("delta")), field: "delta".into() }),
            method: MethodKind::Known(StdMethod::Clone),
            type_args: vec![],
            args: vec![],
        })
    };

    // ── Block 0: entry — sparse init: produce the K touched cells at ts=0 → B1 ─
    let mut b0_stmts: Vec<IrStmt> = vec![let_stmt("q_one_const", q_one())];
    let init_cur = crate::storage_loop::emit_touched_init_verifier(k, addr_bits, &pow2_addr, &mut b0_stmts);
    let mut b0_args: Vec<IrExpr> = (0..num_params)
        .map(|i| clone_expr(var(&format!("init_q{i}"))))
        .collect();
    b0_args.push(IrExpr::Lit(IrLit::Bool(true)));
    b0_args.push(IrExpr::Lit(IrLit::Int(0)));
    b0_args.push(clone_expr(var(&init_cur))); // mem_prod_q (cells initialised)
    b0_args.push(q_zero()); // mem_cons_q
    b0_args.push(q_one()); // qcnt bit 0 = Q(1) = Δ
    for _ in 1..b {
        b0_args.push(q_zero()); // qcnt bits 1.. = 0
    }
    b0_args.push(q_one()); // q_order = Q(1) = Δ
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: b0_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: receive-or-disconnect dispatch ────────────────────────────
    // params: q0..qN-1, all_ok, iter, mem_prod_q, mem_cons_q, mem_ts_q
    let mut b1_params: Vec<IrParam> = (0..num_params)
        .map(|i| IrParam { name: format!("q{i}"), ty: q_type() })
        .collect();
    b1_params.push(IrParam { name: "all_ok".into(), ty: bool_type() });
    b1_params.push(IrParam { name: "iter".into(), ty: usize_type() });
    b1_params.push(IrParam { name: "mem_prod_q".into(), ty: q_type() });
    b1_params.push(IrParam { name: "mem_cons_q".into(), ty: q_type() });
    for n in &cnt_names {
        b1_params.push(IrParam { name: n.clone(), ty: q_type() });
    }
    b1_params.push(IrParam { name: "q_order".into(), ty: q_type() });

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
            vec![IrExpr::Lit(IrLit::Int(andc as i128))],
        )),
    });
    b1_stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok_new".into(), subpat: None },
        ty: None,
        init: Some(var("all_ok")),
    });
    // Q-share of public 1 (= Δ), carrying the constant encode term r0.
    b1_stmts.push(let_stmt("q_one_const", q_one()));

    let mut vnames = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        vnames.insert(i as u32, format!("q{i}"));
    }
    let mut cur_prod = String::from("mem_prod_q");
    let mut cur_cons = String::from("mem_cons_q");
    let mut cur_order = String::from("q_order");
    let mut cur_cnt: Vec<String> = cnt_names.clone();
    emit_verifier_loop_gates(
        &expand_ors(block),
        &mut vnames,
        andc,
        read_count,
        write_count,
        b,
        addr_bits,
        &pow2_ts,
        &pow2_addr,
        &mut cur_prod,
        &mut cur_cons,
        &mut cur_order,
        &mut cur_cnt,
        &mut b1_stmts,
    );

    let (_done, _out, next_ids) = analyze_loop_terminator(block);
    let mut back_args: Vec<IrExpr> =
        next_ids.iter().map(|id| clone_expr(var(&vnames[&id.0]))).collect();
    back_args.push(var("all_ok_new"));
    back_args.push(incr("iter"));
    back_args.push(var(&cur_prod));
    back_args.push(var(&cur_cons));
    for n in &cur_cnt {
        back_args.push(var(n));
    }
    back_args.push(var(&cur_order));

    let block1 = IrCfgBlock {
        params: b1_params,
        stmts: b1_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::CondGoto {
            cond: var("is_sentinel"),
            // sentinel -> B2 [all_ok_new, mem_prod_q, mem_cons_q, q_order]
            then_: IrCfgJump {
                target: 2,
                args: vec![var("all_ok_new"), var(&cur_prod), var(&cur_cons), var(&cur_order)],
            },
            else_: IrCfgJump { target: 1, args: back_args },
        },
    };

    // ── Block 2: exit — drain both cells, check drain + ordering, verdict ──
    let bridge_fn = |fname: &str, args: Vec<IrExpr>| IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["volar_spec".into(), "vole".into(), "bridge".into(), fname.into()],
            type_args: vec![],
        }),
        args,
    };
    let mut b2_stmts: Vec<IrStmt> = vec![
        let_stmt("q_one_const", q_one()),
        IrStmt::Let {
            pattern: IrPattern::Ident { mutable: true, name: "all_ok_d".into(), subpat: None },
            ty: None,
            init: Some(var("final_ok")),
        },
        let_stmt("sort_hats", transport_call_try("recv_hats", vec![IrExpr::Lit(IrLit::Int(crate::storage_loop::sort_and_count(k, addr_bits) as i128))])),
    ];
    let mut cur_order_x = String::from("forder_q");
    let mut sort_and_counter = 0usize;
    let mk_qand_sort = |kk: usize| IrExpr::Index {
        base: Box::new(var("q_ands_sort")),
        index: Box::new(IrExpr::Lit(IrLit::Int(kk as i128))),
    };
    let mk_hat_sort = |kk: usize| IrExpr::Index {
        base: Box::new(var("sort_hats")),
        index: Box::new(IrExpr::Lit(IrLit::Int(kk as i128))),
    };
    let drain_cur = crate::storage_loop::emit_touched_drain_verifier(
        k, addr_bits, b, &pow2_addr, &pow2_ts, "fcons_q", &mut cur_order_x, "all_ok_d",
        &mut sort_and_counter, &mk_qand_sort, &mk_hat_sort, &mut b2_stmts,
    );
    b2_stmts.extend([
        let_stmt("mem_opening", transport_call_try("recv_mem_opening", vec![])),
        let_stmt(
            "mem_ok",
            bridge_fn(
                "mem_drain_check",
                vec![ref_expr(var("fprod_q")), ref_expr(var(&drain_cur)), ref_expr(var("mem_opening"))],
            ),
        ),
        let_stmt("order_opening", transport_call_try("recv_opening", vec![])),
        let_stmt(
            "order_ok2",
            bridge_fn(
                "assert_one_check",
                vec![ref_expr(var(&cur_order_x)), ref_expr(var("order_opening")), var("delta")],
            ),
        ),
        let_stmt(
            "verdict",
            IrExpr::Binary {
                op: SpecBinOp::And,
                left: Box::new(IrExpr::Binary {
                    op: SpecBinOp::And,
                    left: Box::new(var("all_ok_d")),
                    right: Box::new(var("mem_ok")),
                }),
                right: Box::new(var("order_ok2")),
            },
        ),
        IrStmt::Semi(transport_call_try("send_verdict", vec![clone_expr(var("verdict"))])),
    ]);
    let block2 = IrCfgBlock {
        params: vec![
            IrParam { name: "final_ok".into(), ty: bool_type() },
            IrParam { name: "fprod_q".into(), ty: q_type() },
            IrParam { name: "fcons_q".into(), ty: q_type() },
            IrParam { name: "forder_q".into(), ty: q_type() },
        ],
        stmts: core::mem::take(&mut b2_stmts),
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(ok_expr(var("verdict")))),
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
        "use volar_spec::vole::bridge::{mem_acc_absorb_vope, mem_acc_absorb_q, vope_bitpack, q_bitpack, mem_drain_open, mem_drain_check, vope_open_mask, assert_one_check};\n",
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
        let module = weave_hybrid_net_vole_prover(&circuit, 4, 1, 2, "test", None);
        let code = print_hybrid_net_cfg_module(&module);
        run_compile_check_net(&code, "hybrid_net_prover");
    }

    #[test]
    fn test_hybrid_net_verifier_compiles() {
        let circuit = build_simple_loop();
        let module = weave_hybrid_net_vole_verifier(&circuit, 4, 1, 2, "test", None);
        let code = print_hybrid_net_cfg_module(&module);
        run_compile_check_net(&code, "hybrid_net_verifier");
    }

    #[test]
    fn test_hybrid_prover_has_both_paths() {
        let circuit = build_simple_loop();
        let module = weave_hybrid_net_vole_prover(&circuit, 4, 1, 2, "test", None);
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
        let module = weave_hybrid_net_vole_prover(&circuit, 4, 1, 2, "test", None);
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

    /// Hybrid storage loop with a 2-bit address (4 cells).
    fn build_storage_hybrid_loop_2bit() -> BIrBlocks {
        use volar_ir::boolar::{BIrBlock, BIrStmt, BIrTarget, BIrTerminator};
        use volar_ir::ir::{IRBlockTargetId, StorageId};
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: 4,
                stmts: vec![
                    BIrStmt::StorageWrite {
                        storage: StorageId(0), src: IRVarId(2), bit_width: 1,
                        addr: vec![IRVarId(0), IRVarId(1)],
                    },
                    BIrStmt::StorageRead {
                        storage: StorageId(0), bit_width: 1, addr: vec![IRVarId(0), IRVarId(1)],
                    },
                ],
                stmt_provs: vec![(), ()],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(0), IRVarId(1), IRVarId(2), IRVarId(5), IRVarId(3)],
                }),
            }],
            pre_init: vec![],
        }
    }

    #[test]
    fn test_hybrid_storage_multibit_addr() {
        // 2-bit address ⇒ 4 init/drain cells; ts-sound + network-resilient.
        let circuit = build_storage_hybrid_loop_2bit();
        let mp = weave_hybrid_net_vole_prover(&circuit, 3, 2, 4, "mb", None);
        let codep = print_hybrid_net_cfg_module(&mp);
        run_compile_check_net(&codep, "hybrid_storage_prover_2bit");
        assert!(codep.contains("touched_final_val"), "sparse drain witnesses");
        assert!(codep.contains("vope_bitpack"), "address + timestamp bit-packed");
        let mv = weave_hybrid_net_vole_verifier(&circuit, 3, 2, 4, "mb", None);
        let codev = print_hybrid_net_cfg_module(&mv);
        run_compile_check_net(&codev, "hybrid_storage_verifier_2bit");
        assert!(codev.contains("q_touched_final_val"), "verifier mirrors sparse drain");
        assert!(codev.contains("assert_one_check"), "ordering asserted");
    }

    #[test]
    fn test_hybrid_storage_unified() {
        // The unified weaver: storage absorbs in the ZK body + accumulator
        // carried through the gap + transport, all in one prover.
        let circuit = build_storage_hybrid_loop();
        let module = weave_hybrid_net_vole_prover(&circuit, 4, 1, 2, "test", None);
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
        // Prover opens the memory-consistency drain for the verifier:
        assert!(code.contains("mem_drain_open") && code.contains("send_mem_opening"),
            "expected prover drain opening");
        // Timestamp-SOUND: committed counter bit-pack + ordering gadget hats +
        // ordering opening (closes the "consume a future write" attack).
        assert!(code.contains("vope_bitpack"), "expected committed-timestamp bit-pack");
        assert!(code.contains("vole_and_prover_step"), "expected ordering/counter gadget hats");
        assert!(code.contains("vope_open_mask") && code.contains("send_opening"),
            "expected ordering-result opening");
    }

    #[test]
    fn test_hybrid_storage_verifier() {
        // The verifier mirrors the prover's storage absorbs in Q-space and runs
        // the drain check (mem_prod == mem_cons), folding it into the verdict.
        let circuit = build_storage_hybrid_loop();
        let module = weave_hybrid_net_vole_verifier(&circuit, 4, 1, 2, "test", None);
        let code = print_hybrid_net_cfg_module(&module);
        run_compile_check_net(&code, "hybrid_storage_verifier");
        assert!(code.contains("mem_acc_absorb_q"), "expected Q-side storage absorb");
        assert!(code.contains("read_vals_q") && code.contains("write_olds_q"),
            "expected verifier Q-share slices");
        assert!(code.contains("recv_mem_opening"), "expected verifier receives drain opening");
        assert!(code.contains("mem_drain_check"), "expected verifier drain check");
        // Timestamp-SOUND verifier: Q-side bit-pack, ordering gadget checks, and
        // the ordering-result assertion folded into the verdict.
        assert!(code.contains("q_bitpack"), "expected Q-side committed-timestamp bit-pack");
        assert!(code.contains("vole_and_verifier_check"), "expected ordering/counter gadget checks");
        assert!(code.contains("assert_one_check"), "expected ordering-result assertion");
    }
}
