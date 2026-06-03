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
        ArrayKind, ArrayLength, ExternalKind, IrAnyFunction, IrCfgBlock, IrCfgBody, IrCfgFunction,
        IrCfgJump, IrClosureParam, IrCfgModule, IrCfgTerminator, IrExpr, IrGenericParam,
        IrGenericParamKind, IrLit, IrModule, IrParam, IrPattern, IrStmt, IrType, IrWherePredicate,
        SpecBinOp, StructKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt, BIrTerminator};
use volar_ir::ir::IRVarId;

use crate::{clone_expr, expand_ors, ref_expr, var};
use crate::gadgets::{emit_lt, GateBuf};
use crate::net::{
    add_output_t, array_size_bound, array_t_n, bool_type, clone_t, default_t, delta_type,
    emit_prover_and_gate, hats_slice_expr, mul_output_t, net_prover_loop_generics_and_where,
    net_verifier_generics_and_where, ok_expr, q_slice_type, q_type, ref_mut_to, ref_to,
    result_type, tr_error_type, transport_call_try, usize_type, vole_array_t_bound, vope_bit_call,
    vope_type,
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
    let map = lower_gadget_prover_map(gates, in_map, tag, hats, stmts);
    map[&result_id].clone()
}

/// As [`lower_gadget_prover`] but returns the full gadget-id → wire-name map (so
/// multi-output gadgets such as [`crate::gadgets::emit_incr`] can recover every
/// result bit).
fn lower_gadget_prover_map(
    gates: &[(IRVarId, BIrStmt)],
    in_map: &BTreeMap<u32, String>,
    tag: &str,
    hats: &mut Vec<String>,
    stmts: &mut Vec<IrStmt>,
) -> BTreeMap<u32, String> {
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
    map
}

// ── Verifier-side (Q) gadget lowering ────────────────────────────────────────

/// `Array::<T, N>::default()`
fn array_t_n_default() -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "default".into()],
            type_args: vec![IrType::TypeParam("T".into()), IrType::TypeParam("N".into())],
        }),
        args: vec![],
    }
}

/// `Q { q: <field> }`
fn q_struct(q_field: IrExpr) -> IrExpr {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Q".into()),
        type_args: vec![],
        fields: vec![("q".into(), q_field)],
        rest: None,
    }
}

/// `Array::<T, N>::from_fn(|i| <body>)` — body references the closure index `i`.
fn array_from_fn_t_n(body: IrExpr) -> IrExpr {
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

/// `<name>.q[i]`
fn q_idx(name: &str) -> IrExpr {
    IrExpr::Index {
        base: Box::new(IrExpr::Field { base: Box::new(var(name)), field: "q".into() }),
        index: Box::new(var("i")),
    }
}

/// `delta.delta[i]`
fn delta_idx() -> IrExpr {
    IrExpr::Index {
        base: Box::new(IrExpr::Field { base: Box::new(var("delta")), field: "delta".into() }),
        index: Box::new(var("i")),
    }
}

fn add(a: IrExpr, b: IrExpr) -> IrExpr {
    IrExpr::Binary { op: SpecBinOp::Add, left: Box::new(a), right: Box::new(b) }
}

/// Verifier (`Q`-side) mirror of [`lower_gadget_prover`].  Lowers the same
/// boolean gadget to verifier IR: `Zero`→`Q{0}`, `One`→`Q{Δ}`, `Xor`/`Not` are
/// free `Q` combinations, each `And`/`Or` runs `vole_and_verifier_check` (its
/// per-gate `ok` is `&&`-folded into the mutable accumulator named `ok_acc`).
///
/// `mk_qand(k)` / `mk_hat(k)` produce the index expressions for the `k`-th AND's
/// verifier share and received hat (so callers control flat vs. per-iteration
/// `iter * AND_COUNT + base` indexing); `and_counter` is advanced in lockstep
/// with the prover's hat ordering.  Returns the wire bound to `result_id`.
#[allow(clippy::too_many_arguments)]
fn lower_gadget_verifier(
    gates: &[(IRVarId, BIrStmt)],
    in_map: &BTreeMap<u32, String>,
    tag: &str,
    and_counter: &mut usize,
    mk_qand: &dyn Fn(usize) -> IrExpr,
    mk_hat: &dyn Fn(usize) -> IrExpr,
    ok_acc: &str,
    stmts: &mut Vec<IrStmt>,
) -> BTreeMap<u32, String> {
    let mut map = in_map.clone();
    // `let (qname, ok_k) = vole_and_verifier_check::<N,T>(delta, &qa, &qb, &q_and, &hat);`
    // then `ok_acc = ok_acc & ok_k;`
    let mut emit_and = |qa: &str, qb: &str, qname: &str, stmts: &mut Vec<IrStmt>| {
        let k = *and_counter;
        *and_counter += 1;
        let ok_name = format!("ok_{qname}");
        stmts.push(IrStmt::Let {
            pattern: IrPattern::Tuple(vec![
                IrPattern::ident(qname),
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
                    ref_expr(var(qa)),
                    ref_expr(var(qb)),
                    ref_expr(mk_qand(k)),
                    ref_expr(mk_hat(k)),
                ],
            }),
        });
        stmts.push(IrStmt::Semi(IrExpr::Assign {
            left: Box::new(var(ok_acc)),
            right: Box::new(IrExpr::Binary {
                op: SpecBinOp::And,
                left: Box::new(var(ok_acc)),
                right: Box::new(var(&ok_name)),
            }),
        }));
    };

    for (rid, stmt) in gates {
        let name = format!("qg{tag}_{}", rid.0);
        match stmt {
            BIrStmt::Zero => stmts.push(let_stmt(&name, q_struct(array_t_n_default()))),
            BIrStmt::One => stmts.push(let_stmt(
                &name,
                q_struct(clone_expr(IrExpr::Field {
                    base: Box::new(var("delta")),
                    field: "delta".into(),
                })),
            )),
            BIrStmt::Xor(a, b) => {
                let (qa, qb) = (map[&a.0].clone(), map[&b.0].clone());
                let _ = (&qa, &qb);
                stmts.push(let_stmt(
                    &name,
                    q_struct(array_from_fn_t_n(add(
                        clone_expr(q_idx(&qa)),
                        clone_expr(q_idx(&qb)),
                    ))),
                ));
            }
            BIrStmt::Not(a) => {
                let qa = map[&a.0].clone();
                stmts.push(let_stmt(
                    &name,
                    q_struct(array_from_fn_t_n(add(
                        clone_expr(q_idx(&qa)),
                        clone_expr(delta_idx()),
                    ))),
                ));
            }
            BIrStmt::And(a, b) => {
                let (qa, qb) = (map[&a.0].clone(), map[&b.0].clone());
                emit_and(&qa, &qb, &name, stmts);
            }
            BIrStmt::Or(a, b) => {
                let (qa, qb) = (map[&a.0].clone(), map[&b.0].clone());
                let ab = format!("{name}_ab");
                emit_and(&qa, &qb, &ab, stmts);
                // qg = qa + qb + qab  (free Q combination)
                stmts.push(let_stmt(
                    &name,
                    q_struct(array_from_fn_t_n(add(
                        add(clone_expr(q_idx(&qa)), clone_expr(q_idx(&qb))),
                        clone_expr(q_idx(&ab)),
                    ))),
                ));
            }
            _ => panic!("lower_gadget_verifier: unexpected gate {stmt:?}"),
        }
        map.insert(rid.0, name);
    }
    map
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

/// `volar_spec::vole::bridge::vope_bitpack(&[b0.clone(), ..], &[pow2_0.clone(), ..])`
fn bitpack_call(bits: &[String], pow2: &[String]) -> IrExpr {
    let bit_arr = ref_expr(IrExpr::FixedArray(bits.iter().map(|b| clone_expr(var(b))).collect()));
    let pow_arr = ref_expr(IrExpr::FixedArray(pow2.iter().map(|p| clone_expr(var(p))).collect()));
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec![
                "volar_spec".into(), "vole".into(), "bridge".into(), "vope_bitpack".into(),
            ],
            type_args: vec![],
        }),
        args: vec![bit_arr, pow_arr],
    }
}

/// `volar_spec::vole::bridge::q_bitpack(&[q0.clone(), ..], &[pow2_0.clone(), ..])`
fn q_bitpack_call(bits: &[String], pow2: &[String]) -> IrExpr {
    let bit_arr = ref_expr(IrExpr::FixedArray(bits.iter().map(|b| clone_expr(var(b))).collect()));
    let pow_arr = ref_expr(IrExpr::FixedArray(pow2.iter().map(|p| clone_expr(var(p))).collect()));
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec![
                "volar_spec".into(), "vole".into(), "bridge".into(), "q_bitpack".into(),
            ],
            type_args: vec![],
        }),
        args: vec![bit_arr, pow_arr],
    }
}

/// Wire names for the little-endian bits of a **public** cell address `c` over
/// `addr_bits` bits: bit `1` → `one_name` (the committed public-1 wire), bit `0`
/// → `zero_name`.  Used to encode the init/drain cell addresses, which are public
/// constants (cells `0..2^addr_bits`).
fn const_addr_bits(c: usize, addr_bits: usize, one_name: &str, zero_name: &str) -> Vec<String> {
    (0..addr_bits)
        .map(|j| if (c >> j) & 1 == 1 { one_name.into() } else { zero_name.into() })
        .collect()
}

/// Number of AND/OR gates a gadget contributes (Or lowers to one AND).
fn gadget_and_count(gates: &[(IRVarId, BIrStmt)]) -> usize {
    gates.iter().filter(|(_, s)| matches!(s, BIrStmt::And(..) | BIrStmt::Or(..))).count()
}

/// Per-iteration AND-gate count for the ts-aware loop: each storage access runs
/// `emit_lt` (ordering) + one `order &= lt` AND + `emit_incr` (counter).
fn ts_iter_and_count(ts_bits: usize, num_access: usize) -> usize {
    let mut lt_buf = GateBuf::new(2 * ts_bits as u32);
    let a: Vec<u32> = (0..ts_bits as u32).collect();
    let b: Vec<u32> = (ts_bits as u32..2 * ts_bits as u32).collect();
    let _ = emit_lt(&a, &b, &mut lt_buf);
    let lt_ands = gadget_and_count(&lt_buf.gates);
    let mut incr_buf = GateBuf::new(ts_bits as u32);
    let _ = crate::gadgets::emit_incr(&(0..ts_bits as u32).collect::<Vec<_>>(), &mut incr_buf);
    let incr_ands = gadget_and_count(&incr_buf.gates);
    num_access * (lt_ands + 1 + incr_ands)
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

// ============================================================================
// weave_ts_storage_loop_prover  —  timestamp-SOUND storage loop
// ============================================================================

/// `volar_spec::vole::bridge::FN(&a, &b)` two-ref bridge call.
fn bridge_call2(fname: &str, a: IrExpr, b: IrExpr) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["volar_spec".into(), "vole".into(), "bridge".into(), fname.into()],
            type_args: vec![],
        }),
        args: vec![ref_expr(a), ref_expr(b)],
    }
}

/// Weave a single-block loop circuit with Commitment-mode storage into a
/// **timestamp-sound** VOLE prover `IrCfgModule`.
///
/// Unlike [`weave_storage_commit_loop_prover`] (which used a single monotonic
/// field `ts` and so could not catch the "consume a future write" attack), this
/// weaver carries a **committed `ts_bits`-bit global counter** and, per storage
/// access, (1) consumes the cell's prior `(addr, value, t_last)` tuple, (2)
/// proves `t_last < counter` with the [`emit_lt`] gadget (its ANDs streamed as
/// hats), (3) produces `(addr, value, counter)`, then (4) increments the counter
/// with [`emit_incr`].  The all-accesses-ordered AND-fold `order_ok` and the
/// produce/consume drain are opened at exit so the verifier rejects any
/// out-of-order (forged) read.  Memory is initialised (both single-bit cells at
/// `ts = 0`) in the entry block and drained at exit using committed
/// final-state witnesses.
///
/// Generated signature (B = `ts_bits`):
/// ```text
/// fn ts_storage_loop_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     vope_one: Vope<N,T,U1>, r1: T, r2: T, r3: T,
///     pow2_0: T, .., pow2_{B-1}: T,          // bit weights for the ts bitpack
///     init_w0: Vope, ..,                     // initial loop state
///     read_vals: &[Vope], write_olds: &[Vope],            // committed values
///     read_last_ts: &[Vope], write_last_ts: &[Vope],      // committed t_last bits
///     final_val0: Vope, final_val1: Vope,                 // drain witnesses
///     final_ts0_0: Vope, .., final_ts1_{B-1}: Vope,
///     transport: &mut Tr,
/// ) -> Result<Vope<N,T,U1>, <Tr as VoleTransport<N,T>>::Error>
/// ```
pub fn weave_ts_storage_loop_prover(
    circuit: &BIrBlocks,
    ts_bits: usize,
    addr_bits: usize,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_movfuscated(), "weave_ts_storage_loop_prover: circuit must be single-block");
    assert!(ts_bits >= 1, "ts_bits must be >= 1");
    assert!(
        (1..=8).contains(&addr_bits),
        "addr_bits must be 1..=8 (init/drain materialises 2^addr_bits cells)"
    );
    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let (read_count, write_count) = count_storage(&expanded);
    let b = ts_bits;
    let cells = 1usize << addr_bits;

    let (generics, where_clause) = net_prover_loop_generics_and_where();
    let npow = b.max(addr_bits);
    let pow2_names: Vec<String> = (0..npow).map(|j| format!("pow2_{j}")).collect();
    let pow2_ts: Vec<String> = pow2_names[..b].to_vec();
    let pow2_addr: Vec<String> = pow2_names[..addr_bits].to_vec();

    // ── Function params ────────────────────────────────────────────────────
    let mut func_params: Vec<IrParam> = vec![IrParam { name: "vope_one".into(), ty: vope_type() }];
    for r in ["r1", "r2", "r3"] {
        func_params.push(IrParam { name: r.into(), ty: IrType::TypeParam("T".into()) });
    }
    for n in &pow2_names {
        func_params.push(IrParam { name: n.clone(), ty: IrType::TypeParam("T".into()) });
    }
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_w{i}"), ty: vope_type() });
    }
    func_params.push(IrParam { name: "read_vals".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "write_olds".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "read_last_ts".into(), ty: vope_slice_type() });
    func_params.push(IrParam { name: "write_last_ts".into(), ty: vope_slice_type() });
    for cell in 0..cells {
        func_params.push(IrParam { name: format!("final_val{cell}"), ty: vope_type() });
    }
    for cell in 0..cells {
        for j in 0..b {
            func_params.push(IrParam { name: format!("final_ts{cell}_{j}"), ty: vope_type() });
        }
    }
    func_params.push(IrParam { name: "transport".into(), ty: ref_mut_to(IrType::TypeParam("Tr".into())) });

    let ret_type = result_type(vope_type(), tr_error_type());

    // ── Block 0: entry — initialise memory (all 2^addr_bits cells at ts=0) ──
    let mut b0_stmts: Vec<IrStmt> = vec![
        let_stmt("zw", zero_vope_expr()),
        let_stmt("mp_acc0", zero_vope_expr()),
    ];
    let mut init_cur = String::from("mp_acc0");
    for c in 0..cells {
        let bits = const_addr_bits(c, addr_bits, "vope_one", "zw");
        let af = format!("initaddr_{c}");
        b0_stmts.push(let_stmt(&af, bitpack_call(&bits, &pow2_addr)));
        let np = format!("init_p{c}");
        // produce (addr=c, val=0, ts=0)
        b0_stmts.push(let_stmt(&np, absorb_call(&init_cur, &af, "zw", "zw")));
        init_cur = np;
    }

    let mut b0_args: Vec<IrExpr> =
        (0..num_params).map(|i| clone_expr(var(&format!("init_w{i}")))).collect();
    b0_args.push(clone_expr(var(&init_cur))); // mem_prod
    b0_args.push(zero_vope_expr()); // mem_cons
    // counter initial value = 1  (so init ts=0 < first produce ts=1)
    b0_args.push(clone_expr(var("vope_one"))); // cnt bit 0 = 1
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

    // ── Block 1: loop body ─────────────────────────────────────────────────
    let mut b1_params: Vec<IrParam> =
        (0..num_params).map(|i| IrParam { name: format!("w{i}"), ty: vope_type() }).collect();
    b1_params.push(IrParam { name: "mem_prod".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "mem_cons".into(), ty: vope_type() });
    let cnt_names: Vec<String> = (0..b).map(|j| format!("cnt{j}")).collect();
    for n in &cnt_names {
        b1_params.push(IrParam { name: n.clone(), ty: vope_type() });
    }
    b1_params.push(IrParam { name: "order_ok".into(), ty: vope_type() });
    b1_params.push(IrParam { name: "iter".into(), ty: usize_type() });

    let mut b1_stmts: Vec<IrStmt> = vec![let_stmt("zwb", zero_vope_expr())];
    let mut wnames = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        wnames.insert(i as u32, format!("w{i}"));
    }
    let mut cur_prod = String::from("mem_prod");
    let mut cur_cons = String::from("mem_cons");
    let mut cur_order = String::from("order_ok");
    let mut cur_cnt: Vec<String> = cnt_names.clone();
    let mut hat_names: Vec<String> = Vec::new();
    let mut ssa = 0usize;
    let mut read_k = 0usize;
    let mut write_k = 0usize;

    // Emit one storage access's ts-sound bookkeeping. `consume_val`/`produce_val`
    // are wire names; `last_slice`/`last_count`/`k_base` locate this access's
    // committed `t_last` bit-vector at `last_slice[iter*last_count + k_base*B + j]`.
    let mut emit_access = |addr_w: &str,
                           consume_val: &str,
                           produce_val: &str,
                           last_slice: &str,
                           last_count: usize,
                           k_base: usize,
                           ssa: &mut usize,
                           cur_prod: &mut String,
                           cur_cons: &mut String,
                           cur_order: &mut String,
                           cur_cnt: &mut Vec<String>,
                           hat_names: &mut Vec<String>,
                           stmts: &mut Vec<IrStmt>| {
        let tag = *ssa;
        *ssa += 1;
        // t_last witness bits
        let tl: Vec<String> = (0..b)
            .map(|j| {
                let nm = format!("tl{tag}_{j}");
                stmts.push(let_stmt(&nm, slice_index_clone(last_slice, last_count, k_base * b + j)));
                nm
            })
            .collect();
        // consume (addr, consume_val, bitpack(t_last))
        let ts_cons = format!("tscons_{tag}");
        stmts.push(let_stmt(&ts_cons, bitpack_call(&tl, &pow2_ts)));
        let nc = format!("cons_{tag}");
        stmts.push(let_stmt(&nc, absorb_call(cur_cons, addr_w, consume_val, &ts_cons)));
        *cur_cons = nc;
        // ordering: order_ok &= (t_last < counter)
        let mut lt_buf = GateBuf::new(2 * b as u32);
        let a_ids: Vec<u32> = (0..b as u32).collect();
        let b_ids: Vec<u32> = (b as u32..2 * b as u32).collect();
        let lt_res = emit_lt(&a_ids, &b_ids, &mut lt_buf);
        let mut lt_in = BTreeMap::<u32, String>::new();
        for j in 0..b {
            lt_in.insert(j as u32, tl[j].clone());
            lt_in.insert((b + j) as u32, cur_cnt[j].clone());
        }
        let lt_name = lower_gadget_prover(
            &lt_buf.gates, &lt_in, lt_res, &format!("lt{tag}"), hat_names, stmts,
        );
        let new_order = format!("ord_{tag}");
        let ord_hat = format!("ordh_{tag}");
        hat_names.push(ord_hat.clone());
        emit_prover_and_gate(cur_order, &lt_name, &new_order, &ord_hat, stmts);
        *cur_order = new_order;
        // produce (addr, produce_val, bitpack(counter))
        let ts_prod = format!("tsprod_{tag}");
        stmts.push(let_stmt(&ts_prod, bitpack_call(cur_cnt, &pow2_ts)));
        let np = format!("prod_{tag}");
        stmts.push(let_stmt(&np, absorb_call(cur_prod, addr_w, produce_val, &ts_prod)));
        *cur_prod = np;
        // counter += 1  (committed ripple-carry increment)
        let mut ic_buf = GateBuf::new(b as u32);
        let ic_outs = crate::gadgets::emit_incr(&a_ids, &mut ic_buf);
        let mut ic_in = BTreeMap::<u32, String>::new();
        for j in 0..b {
            ic_in.insert(j as u32, cur_cnt[j].clone());
        }
        let ic_map = lower_gadget_prover_map(
            &ic_buf.gates, &ic_in, &format!("ic{tag}"), hat_names, stmts,
        );
        *cur_cnt = ic_outs.iter().map(|id| ic_map[id].clone()).collect();
    };

    for (result_id, stmt, _) in &expanded {
        let let_name = format!("lw_{}", result_id.0);
        match stmt {
            BIrStmt::Zero => b1_stmts.push(let_stmt(&let_name, zero_vope_expr())),
            BIrStmt::One => b1_stmts.push(let_stmt(&let_name, clone_expr(var("vope_one")))),
            BIrStmt::Xor(a, bb) => {
                let e = IrExpr::Binary {
                    op: SpecBinOp::Add,
                    left: Box::new(clone_expr(var(&wnames[&a.0]))),
                    right: Box::new(clone_expr(var(&wnames[&bb.0]))),
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
                assert_eq!(addr.len(), addr_bits, "ts_storage_loop: address width must equal addr_bits");
                let addr_w = format!("af_{ssa}");
                let bits: Vec<String> = addr.iter().map(|a| wnames[&a.0].clone()).collect();
                b1_stmts.push(let_stmt(&addr_w, bitpack_call(&bits, &pow2_addr)));
                let val_name = format!("rd_{ssa}_v");
                b1_stmts.push(let_stmt(&val_name, slice_index_clone("read_vals", read_count, read_k)));
                emit_access(
                    &addr_w, &val_name, &val_name, "read_last_ts", read_count * b, read_k,
                    &mut ssa, &mut cur_prod, &mut cur_cons, &mut cur_order, &mut cur_cnt,
                    &mut hat_names, &mut b1_stmts,
                );
                read_k += 1;
                wnames.insert(result_id.0, val_name);
                continue;
            }
            BIrStmt::StorageWrite { src, addr, .. } => {
                assert_eq!(addr.len(), addr_bits, "ts_storage_loop: address width must equal addr_bits");
                let addr_w = format!("af_{ssa}");
                let bits: Vec<String> = addr.iter().map(|a| wnames[&a.0].clone()).collect();
                b1_stmts.push(let_stmt(&addr_w, bitpack_call(&bits, &pow2_addr)));
                let new_w = wnames[&src.0].clone();
                let old_name = format!("wr_{ssa}_old");
                b1_stmts.push(let_stmt(&old_name, slice_index_clone("write_olds", write_count, write_k)));
                emit_access(
                    &addr_w, &old_name, &new_w, "write_last_ts", write_count * b, write_k,
                    &mut ssa, &mut cur_prod, &mut cur_cons, &mut cur_order, &mut cur_cnt,
                    &mut hat_names, &mut b1_stmts,
                );
                write_k += 1;
                b1_stmts.push(let_stmt(&let_name, zero_vope_expr()));
            }
            BIrStmt::And(..) | BIrStmt::Or(..) => {
                panic!("ts_storage_loop: AND/OR circuit gates not supported (linear subset only)")
            }
            _ => panic!("ts_storage_loop: unsupported statement {stmt:?}"),
        }
        wnames.insert(result_id.0, let_name);
    }

    // Terminator analysis.
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
    // transport.send_iteration(&hats, done_bit)?
    b1_stmts.push(IrStmt::Semi(transport_call_try(
        "send_iteration",
        vec![hats_slice_expr(&hat_names), var("done_bit")],
    )));

    // then (done) -> B2 [out, mem_prod, mem_cons, order_ok]
    let b2_args = vec![
        clone_expr(var(&out_wire)),
        clone_expr(var(&cur_prod)),
        clone_expr(var(&cur_cons)),
        clone_expr(var(&cur_order)),
    ];
    // else -> B1 [next..., mem_prod, mem_cons, cnt.., order_ok, iter+1]
    let mut back_args: Vec<IrExpr> =
        next_ids.iter().map(|id| clone_expr(var(&wnames[&id.0]))).collect();
    back_args.push(clone_expr(var(&cur_prod)));
    back_args.push(clone_expr(var(&cur_cons)));
    for n in &cur_cnt {
        back_args.push(clone_expr(var(n)));
    }
    back_args.push(clone_expr(var(&cur_order)));
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

    // ── Block 2: exit — drain all cells, open drain + ordering, recv verdict ─
    let mut b2_stmts: Vec<IrStmt> = vec![let_stmt("zwx", zero_vope_expr())];
    let mut drain_cur = String::from("fcons");
    for c in 0..cells {
        let bits = const_addr_bits(c, addr_bits, "vope_one", "zwx");
        let af = format!("draddr_{c}");
        b2_stmts.push(let_stmt(&af, bitpack_call(&bits, &pow2_addr)));
        let ts_bits_c: Vec<String> = (0..b).map(|j| format!("final_ts{c}_{j}")).collect();
        let tsd = format!("ts_d{c}");
        b2_stmts.push(let_stmt(&tsd, bitpack_call(&ts_bits_c, &pow2_ts)));
        let nc = format!("fc{c}");
        // consume (addr=c, final_val_c, bitpack(final_ts_c))
        b2_stmts.push(let_stmt(&nc, absorb_call(&drain_cur, &af, &format!("final_val{c}"), &tsd)));
        drain_cur = nc;
    }
    // open drain: M_diff = mem_prod.v ^ mem_cons.v
    b2_stmts.push(let_stmt("mem_open", bridge_call2("mem_drain_open", var("fprod"), var(&drain_cur))));
    b2_stmts.push(IrStmt::Semi(transport_call_try("send_opening", vec![ref_expr(var("mem_open"))])));
    // open ordering result (must be 1)
    b2_stmts.push(let_stmt(
        "order_open",
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec![
                    "volar_spec".into(), "vole".into(), "bridge".into(), "vope_open_mask".into(),
                ],
                type_args: vec![],
            }),
            args: vec![ref_expr(var("forder"))],
        },
    ));
    b2_stmts.push(IrStmt::Semi(transport_call_try("send_opening", vec![ref_expr(var("order_open"))])));
    b2_stmts.push(IrStmt::Semi(transport_call_try("recv_verdict", vec![])));
    let block2 = IrCfgBlock {
        params: vec![
            IrParam { name: "output".into(), ty: vope_type() },
            IrParam { name: "fprod".into(), ty: vope_type() },
            IrParam { name: "fcons".into(), ty: vope_type() },
            IrParam { name: "forder".into(), ty: vope_type() },
        ],
        stmts: b2_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(ok_expr(var("output")))),
    };

    let func = IrCfgFunction {
        name: format!("ts_storage_loop_{name}"),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0, block1, block2] },
    };
    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_ts_storage_loop_{name}"),
        functions: vec![IrAnyFunction::Cfg(func)],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    let _ = ts_iter_and_count(b, read_count + write_count); // documents per-iter AND count
    if let Some(ls) = linkage {
        ls.apply_cfg(&mut module);
    }
    module
}

// ============================================================================
// weave_ts_storage_loop_verifier  —  Q-side mirror
// ============================================================================

/// `q_ands[iter * andc + k]`
fn qand_index_expr(andc: usize, k: usize) -> IrExpr {
    let idx = IrExpr::Binary {
        op: SpecBinOp::Add,
        left: Box::new(IrExpr::Binary {
            op: SpecBinOp::Mul,
            left: Box::new(var("iter")),
            right: Box::new(IrExpr::Lit(IrLit::Int(andc as i128))),
        }),
        right: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
    };
    IrExpr::Index { base: Box::new(var("q_ands")), index: Box::new(idx) }
}

/// `iter_hats[k]`
fn iter_hat_index_expr(k: usize) -> IrExpr {
    IrExpr::Index {
        base: Box::new(var("iter_hats")),
        index: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
    }
}

/// `q[iter * count + k].clone()` — verifier-side per-iteration slice index.
fn q_slice_index_clone(slice: &str, count: usize, k: usize) -> IrExpr {
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

/// `mem_acc_absorb_q(acc.clone(), &addr, &value, &ts, &r1, &r2, &r3)`
fn q_absorb_call(acc: &str, addr: &str, value: &str, ts: &str) -> IrExpr {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec![
                "volar_spec".into(), "vole".into(), "bridge".into(), "mem_acc_absorb_q".into(),
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

/// `Q { q: <delta.delta.clone() or Array::default()> }` for public-constant Q.
fn q_const(is_one: bool) -> IrExpr {
    if is_one {
        q_struct(clone_expr(IrExpr::Field { base: Box::new(var("delta")), field: "delta".into() }))
    } else {
        q_struct(array_t_n_default())
    }
}

/// Verifier (`Q`-side) mirror of [`weave_ts_storage_loop_prover`].  Receives one
/// iteration's gadget hats via `transport.recv_iteration(AND_COUNT)`, mirrors the
/// committed-counter ordering checks and the multiset absorbs in `Q`-space, and
/// at the sentinel checks (a) the produce/consume drain (`mem_drain_check`) and
/// (b) that every access was ordered (`assert_one_check` on the opened
/// `order_ok`).  The verdict is `all_ok && drain_ok && order_ok`.
///
/// Generated signature (B = `ts_bits`, ANDC = per-iteration AND count):
/// ```text
/// fn ts_storage_verify_<NAME><N, T, Tr: VoleTransport<N, T>>(
///     delta: &Delta<N,T>, r1: T, r2: T, r3: T, pow2_0: T, .., pow2_{B-1}: T,
///     q_ands: &[Q<N,T>], init_q0: Q, ..,
///     q_read_vals: &[Q], q_write_olds: &[Q], q_read_last_ts: &[Q], q_write_last_ts: &[Q],
///     q_final_val0: Q, q_final_val1: Q, q_final_ts0_0: Q, .., q_final_ts1_{B-1}: Q,
///     transport: &mut Tr,
/// ) -> Result<bool, <Tr as VoleTransport<N,T>>::Error>
/// ```
pub fn weave_ts_storage_loop_verifier(
    circuit: &BIrBlocks,
    ts_bits: usize,
    addr_bits: usize,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    assert!(circuit.is_movfuscated(), "weave_ts_storage_loop_verifier: circuit must be single-block");
    assert!(ts_bits >= 1, "ts_bits must be >= 1");
    assert!(
        (1..=8).contains(&addr_bits),
        "addr_bits must be 1..=8 (init/drain materialises 2^addr_bits cells)"
    );
    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);
    let (read_count, write_count) = count_storage(&expanded);
    let b = ts_bits;
    let cells = 1usize << addr_bits;
    let andc = ts_iter_and_count(b, read_count + write_count);

    let (generics, where_clause) = net_verifier_generics_and_where();
    let npow = b.max(addr_bits);
    let pow2_names: Vec<String> = (0..npow).map(|j| format!("pow2_{j}")).collect();
    let pow2_ts: Vec<String> = pow2_names[..b].to_vec();
    let pow2_addr: Vec<String> = pow2_names[..addr_bits].to_vec();

    // ── Function params ────────────────────────────────────────────────────
    let mut func_params: Vec<IrParam> = vec![IrParam { name: "delta".into(), ty: ref_to(delta_type()) }];
    for r in ["r1", "r2", "r3"] {
        func_params.push(IrParam { name: r.into(), ty: IrType::TypeParam("T".into()) });
    }
    for n in &pow2_names {
        func_params.push(IrParam { name: n.clone(), ty: IrType::TypeParam("T".into()) });
    }
    func_params.push(IrParam { name: "q_ands".into(), ty: q_slice_type() });
    for i in 0..num_params {
        func_params.push(IrParam { name: format!("init_q{i}"), ty: q_type() });
    }
    for s in ["q_read_vals", "q_write_olds", "q_read_last_ts", "q_write_last_ts"] {
        func_params.push(IrParam { name: s.into(), ty: q_slice_type() });
    }
    for cell in 0..cells {
        func_params.push(IrParam { name: format!("q_final_val{cell}"), ty: q_type() });
    }
    for cell in 0..cells {
        for j in 0..b {
            func_params.push(IrParam { name: format!("q_final_ts{cell}_{j}"), ty: q_type() });
        }
    }
    func_params.push(IrParam { name: "transport".into(), ty: ref_mut_to(IrType::TypeParam("Tr".into())) });

    let ret_type = result_type(bool_type(), tr_error_type());

    // ── Block 0: entry — init mem_prod_q (all 2^addr_bits cells, ts=0) ──────
    let mut b0_stmts: Vec<IrStmt> = vec![
        let_stmt("qz", q_const(false)),
        let_stmt("qone", q_const(true)),
        let_stmt("mp_acc0", q_const(false)),
    ];
    let mut init_cur = String::from("mp_acc0");
    for c in 0..cells {
        let bits = const_addr_bits(c, addr_bits, "qone", "qz");
        let af = format!("qinitaddr_{c}");
        b0_stmts.push(let_stmt(&af, q_bitpack_call(&bits, &pow2_addr)));
        let np = format!("iq{c}");
        // produce (addr=c, val=0, ts=0)
        b0_stmts.push(let_stmt(&np, q_absorb_call(&init_cur, &af, "qz", "qz")));
        init_cur = np;
    }

    let mut b0_args: Vec<IrExpr> =
        (0..num_params).map(|i| clone_expr(var(&format!("init_q{i}")))).collect();
    b0_args.push(clone_expr(var(&init_cur))); // mem_prod_q
    b0_args.push(q_const(false)); // mem_cons_q = 0
    b0_args.push(q_const(true)); // qcnt bit0 = Q(1) = Δ
    for _ in 1..b {
        b0_args.push(q_const(false)); // qcnt bits = 0
    }
    b0_args.push(q_const(true)); // q_order init = Q(1) = Δ
    b0_args.push(IrExpr::Lit(IrLit::Bool(true))); // all_ok
    b0_args.push(IrExpr::Lit(IrLit::Int(0))); // iter
    let block0 = IrCfgBlock {
        params: vec![],
        stmts: b0_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Goto(IrCfgJump { target: 1, args: b0_args }),
    };

    // ── Block 1: loop body ─────────────────────────────────────────────────
    let qcnt_names: Vec<String> = (0..b).map(|j| format!("qcnt{j}")).collect();
    let mut b1_params: Vec<IrParam> =
        (0..num_params).map(|i| IrParam { name: format!("q{i}"), ty: q_type() }).collect();
    b1_params.push(IrParam { name: "mem_prod_q".into(), ty: q_type() });
    b1_params.push(IrParam { name: "mem_cons_q".into(), ty: q_type() });
    for n in &qcnt_names {
        b1_params.push(IrParam { name: n.clone(), ty: q_type() });
    }
    b1_params.push(IrParam { name: "q_order".into(), ty: q_type() });
    b1_params.push(IrParam { name: "all_ok".into(), ty: bool_type() });
    b1_params.push(IrParam { name: "iter".into(), ty: usize_type() });

    let mut b1_stmts: Vec<IrStmt> = Vec::new();
    // let (iter_hats, is_sentinel) = transport.recv_iteration(ANDC)?;
    b1_stmts.push(IrStmt::Let {
        pattern: IrPattern::Tuple(vec![
            IrPattern::ident("iter_hats"),
            IrPattern::ident("is_sentinel"),
        ]),
        ty: None,
        init: Some(transport_call_try("recv_iteration", vec![IrExpr::Lit(IrLit::Int(andc as i128))])),
    });
    b1_stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok_new".into(), subpat: None },
        ty: None,
        init: Some(var("all_ok")),
    });

    let mut qwnames = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        qwnames.insert(i as u32, format!("q{i}"));
    }
    let mut cur_prod = String::from("mem_prod_q");
    let mut cur_cons = String::from("mem_cons_q");
    let mut cur_order = String::from("q_order");
    let mut cur_cnt: Vec<String> = qcnt_names.clone();
    let mut and_counter = 0usize;
    let mut ssa = 0usize;
    let mut read_k = 0usize;
    let mut write_k = 0usize;

    let mut emit_access_q = |addr_q: &str,
                             consume_q: &str,
                             produce_q: &str,
                             last_slice: &str,
                             last_count: usize,
                             k_base: usize,
                             ssa: &mut usize,
                             and_counter: &mut usize,
                             cur_prod: &mut String,
                             cur_cons: &mut String,
                             cur_order: &mut String,
                             cur_cnt: &mut Vec<String>,
                             stmts: &mut Vec<IrStmt>| {
        let tag = *ssa;
        *ssa += 1;
        let mk_qand = |k: usize| qand_index_expr(andc, k);
        let mk_hat = |k: usize| iter_hat_index_expr(k);
        // q t_last bits
        let tl: Vec<String> = (0..b)
            .map(|j| {
                let nm = format!("qtl{tag}_{j}");
                stmts.push(let_stmt(&nm, q_slice_index_clone(last_slice, last_count, k_base * b + j)));
                nm
            })
            .collect();
        // consume
        let ts_cons = format!("qtscons_{tag}");
        stmts.push(let_stmt(&ts_cons, q_bitpack_call(&tl, &pow2_ts)));
        let nc = format!("qcons_{tag}");
        stmts.push(let_stmt(&nc, q_absorb_call(cur_cons, addr_q, consume_q, &ts_cons)));
        *cur_cons = nc;
        // ordering gadget: order_ok &= (t_last < counter)
        let mut lt_buf = GateBuf::new(2 * b as u32);
        let a_ids: Vec<u32> = (0..b as u32).collect();
        let b_ids: Vec<u32> = (b as u32..2 * b as u32).collect();
        let lt_res = emit_lt(&a_ids, &b_ids, &mut lt_buf);
        let mut lt_in = BTreeMap::<u32, String>::new();
        for j in 0..b {
            lt_in.insert(j as u32, tl[j].clone());
            lt_in.insert((b + j) as u32, cur_cnt[j].clone());
        }
        let lt_map = lower_gadget_verifier(
            &lt_buf.gates, &lt_in, &format!("lt{tag}"), and_counter, &mk_qand, &mk_hat,
            "all_ok_new", stmts,
        );
        let q_lt = lt_map[&lt_res].clone();
        // order AND
        let k = *and_counter;
        *and_counter += 1;
        let new_order = format!("qord_{tag}");
        let ok_ord = format!("okord_{tag}");
        stmts.push(IrStmt::Let {
            pattern: IrPattern::Tuple(vec![IrPattern::ident(&new_order), IrPattern::ident(&ok_ord)]),
            ty: None,
            init: Some(IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["vole_and_verifier_check".into()],
                    type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("T".into())],
                }),
                args: vec![
                    var("delta"),
                    ref_expr(var(cur_order)),
                    ref_expr(var(&q_lt)),
                    ref_expr(mk_qand(k)),
                    ref_expr(mk_hat(k)),
                ],
            }),
        });
        stmts.push(IrStmt::Semi(IrExpr::Assign {
            left: Box::new(var("all_ok_new")),
            right: Box::new(IrExpr::Binary {
                op: SpecBinOp::And,
                left: Box::new(var("all_ok_new")),
                right: Box::new(var(&ok_ord)),
            }),
        }));
        *cur_order = new_order;
        // produce
        let ts_prod = format!("qtsprod_{tag}");
        stmts.push(let_stmt(&ts_prod, q_bitpack_call(cur_cnt, &pow2_ts)));
        let np = format!("qprod_{tag}");
        stmts.push(let_stmt(&np, q_absorb_call(cur_prod, addr_q, produce_q, &ts_prod)));
        *cur_prod = np;
        // counter += 1
        let mut ic_buf = GateBuf::new(b as u32);
        let ic_outs = crate::gadgets::emit_incr(&a_ids, &mut ic_buf);
        let mut ic_in = BTreeMap::<u32, String>::new();
        for j in 0..b {
            ic_in.insert(j as u32, cur_cnt[j].clone());
        }
        let ic_map = lower_gadget_verifier(
            &ic_buf.gates, &ic_in, &format!("ic{tag}"), and_counter, &mk_qand, &mk_hat,
            "all_ok_new", stmts,
        );
        *cur_cnt = ic_outs.iter().map(|id| ic_map[id].clone()).collect();
    };

    for (result_id, stmt, _) in &expanded {
        let let_name = format!("lq_{}", result_id.0);
        match stmt {
            BIrStmt::Zero => b1_stmts.push(let_stmt(&let_name, q_const(false))),
            BIrStmt::One => b1_stmts.push(let_stmt(&let_name, q_const(true))),
            BIrStmt::Xor(a, bb) => {
                let (qa, qb) = (qwnames[&a.0].clone(), qwnames[&bb.0].clone());
                b1_stmts.push(let_stmt(
                    &let_name,
                    q_struct(array_from_fn_t_n(add(clone_expr(q_idx(&qa)), clone_expr(q_idx(&qb))))),
                ));
            }
            BIrStmt::Not(a) => {
                let qa = qwnames[&a.0].clone();
                b1_stmts.push(let_stmt(
                    &let_name,
                    q_struct(array_from_fn_t_n(add(clone_expr(q_idx(&qa)), clone_expr(delta_idx())))),
                ));
            }
            BIrStmt::StorageRead { addr, .. } => {
                assert_eq!(addr.len(), addr_bits, "ts_storage_loop: address width must equal addr_bits");
                let addr_q = format!("qaf_{ssa}");
                let bits: Vec<String> = addr.iter().map(|a| qwnames[&a.0].clone()).collect();
                b1_stmts.push(let_stmt(&addr_q, q_bitpack_call(&bits, &pow2_addr)));
                let val_q = format!("qrd_{ssa}_v");
                b1_stmts.push(let_stmt(&val_q, q_slice_index_clone("q_read_vals", read_count, read_k)));
                emit_access_q(
                    &addr_q, &val_q, &val_q, "q_read_last_ts", read_count * b, read_k,
                    &mut ssa, &mut and_counter, &mut cur_prod, &mut cur_cons, &mut cur_order,
                    &mut cur_cnt, &mut b1_stmts,
                );
                read_k += 1;
                qwnames.insert(result_id.0, val_q);
                continue;
            }
            BIrStmt::StorageWrite { src, addr, .. } => {
                assert_eq!(addr.len(), addr_bits, "ts_storage_loop: address width must equal addr_bits");
                let addr_q = format!("qaf_{ssa}");
                let bits: Vec<String> = addr.iter().map(|a| qwnames[&a.0].clone()).collect();
                b1_stmts.push(let_stmt(&addr_q, q_bitpack_call(&bits, &pow2_addr)));
                let new_q = qwnames[&src.0].clone();
                let old_q = format!("qwr_{ssa}_old");
                b1_stmts.push(let_stmt(&old_q, q_slice_index_clone("q_write_olds", write_count, write_k)));
                emit_access_q(
                    &addr_q, &old_q, &new_q, "q_write_last_ts", write_count * b, write_k,
                    &mut ssa, &mut and_counter, &mut cur_prod, &mut cur_cons, &mut cur_order,
                    &mut cur_cnt, &mut b1_stmts,
                );
                write_k += 1;
                b1_stmts.push(let_stmt(&let_name, q_const(false)));
            }
            BIrStmt::And(..) | BIrStmt::Or(..) => {
                panic!("ts_storage_loop verifier: AND/OR circuit gates not supported")
            }
            _ => panic!("ts_storage_loop verifier: unsupported statement {stmt:?}"),
        }
        qwnames.insert(result_id.0, let_name);
    }

    // Terminator: next-state ids (all but last for Jmp; else_target for CondJmp).
    let next_ids: Vec<IRVarId> = match &block.terminator {
        BIrTerminator::Jmp(t) => t.args[..t.args.len().saturating_sub(1)].to_vec(),
        BIrTerminator::CondJmp { else_target, .. } => else_target.args.clone(),
    };

    // then (sentinel) -> B2 [mem_prod_q, mem_cons_q, q_order, all_ok_new]
    let b2_args = vec![
        clone_expr(var(&cur_prod)),
        clone_expr(var(&cur_cons)),
        clone_expr(var(&cur_order)),
        var("all_ok_new"),
    ];
    // else -> B1 [next.., mem_prod_q, mem_cons_q, qcnt.., q_order, all_ok_new, iter+1]
    let mut back_args: Vec<IrExpr> =
        next_ids.iter().map(|id| clone_expr(var(&qwnames[&id.0]))).collect();
    back_args.push(clone_expr(var(&cur_prod)));
    back_args.push(clone_expr(var(&cur_cons)));
    for n in &cur_cnt {
        back_args.push(clone_expr(var(n)));
    }
    back_args.push(clone_expr(var(&cur_order)));
    back_args.push(var("all_ok_new"));
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
            cond: var("is_sentinel"),
            then_: IrCfgJump { target: 2, args: b2_args },
            else_: IrCfgJump { target: 1, args: back_args },
        },
    };

    // ── Block 2: exit — drain all cells, check drain + ordering, send verdict ─
    let mut b2_stmts: Vec<IrStmt> = vec![
        let_stmt("qzx", q_const(false)),
        let_stmt("qone_x", q_const(true)),
    ];
    let mut drain_cur = String::from("fcons");
    for c in 0..cells {
        let bits = const_addr_bits(c, addr_bits, "qone_x", "qzx");
        let af = format!("qdraddr_{c}");
        b2_stmts.push(let_stmt(&af, q_bitpack_call(&bits, &pow2_addr)));
        let qts_c: Vec<String> = (0..b).map(|j| format!("q_final_ts{c}_{j}")).collect();
        let tsd = format!("qts_d{c}");
        b2_stmts.push(let_stmt(&tsd, q_bitpack_call(&qts_c, &pow2_ts)));
        let nc = format!("qfc{c}");
        // consume (addr=c, q_final_val_c, bitpack(q_final_ts_c))
        b2_stmts.push(let_stmt(&nc, q_absorb_call(&drain_cur, &af, &format!("q_final_val{c}"), &tsd)));
        drain_cur = nc;
    }
    // recv drain opening + check
    b2_stmts.push(let_stmt("mem_open", transport_call_try("recv_opening", vec![])));
    b2_stmts.push(let_stmt(
        "drain_ok",
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec![
                    "volar_spec".into(), "vole".into(), "bridge".into(), "mem_drain_check".into(),
                ],
                type_args: vec![],
            }),
            args: vec![ref_expr(var("fprod")), ref_expr(var(&drain_cur)), ref_expr(var("mem_open"))],
        },
    ));
    // recv ordering opening + check (order_ok must be committed to 1)
    b2_stmts.push(let_stmt("order_open", transport_call_try("recv_opening", vec![])));
    b2_stmts.push(let_stmt(
        "order_ok2",
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec![
                    "volar_spec".into(), "vole".into(), "bridge".into(), "assert_one_check".into(),
                ],
                type_args: vec![],
            }),
            args: vec![ref_expr(var("forder")), ref_expr(var("order_open")), var("delta")],
        },
    ));
    // verdict = all_ok && drain_ok && order_ok2
    b2_stmts.push(let_stmt(
        "verdict",
        IrExpr::Binary {
            op: SpecBinOp::And,
            left: Box::new(IrExpr::Binary {
                op: SpecBinOp::And,
                left: Box::new(var("final_ok")),
                right: Box::new(var("drain_ok")),
            }),
            right: Box::new(var("order_ok2")),
        },
    ));
    b2_stmts.push(IrStmt::Semi(transport_call_try("send_verdict", vec![clone_expr(var("verdict"))])));
    let block2 = IrCfgBlock {
        params: vec![
            IrParam { name: "fprod".into(), ty: q_type() },
            IrParam { name: "fcons".into(), ty: q_type() },
            IrParam { name: "forder".into(), ty: q_type() },
            IrParam { name: "final_ok".into(), ty: bool_type() },
        ],
        stmts: b2_stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(ok_expr(var("verdict")))),
    };

    let func = IrCfgFunction {
        name: format!("ts_storage_verify_{name}"),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(ret_type),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0, block1, block2] },
    };
    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_ts_storage_verify_{name}"),
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

/// Verifier (`Q`-side) mirror of [`weave_lt_check`]: checks the AND-gate hats of
/// the committed less-than gadget and returns the accumulated `all_ok`.  This
/// validates [`lower_gadget_verifier`] end-to-end (hats consumed via
/// `vole_and_verifier_check`, `Xor`/`Not` lowered to free `Q` combinations).
/// Generated signature:
/// ```text
/// fn lt_check_verify_<NAME><N, T>(delta: &Delta<N,T>, q_ands: &[Q<N,T>],
///     a0: Q<N,T>, .., b0: Q<N,T>, .., hats: &[Array<T,N>]) -> bool
/// ```
pub fn weave_lt_check_verifier(n: usize, name: &str, linkage: Option<&LinkageSystem>) -> IrCfgModule {
    let (generics, where_clause) = storage_loop_generics_and_where();

    let q_slice = ref_to(IrType::Array {
        kind: ArrayKind::Slice,
        elem: Box::new(q_type()),
        len: ArrayLength::Const(0),
    });
    let hats_slice = ref_to(IrType::Array {
        kind: ArrayKind::Slice,
        elem: Box::new(array_t_n()),
        len: ArrayLength::Const(0),
    });

    let mut func_params = vec![
        IrParam { name: "delta".into(), ty: ref_to(delta_type()) },
        IrParam { name: "q_ands".into(), ty: q_slice },
    ];
    for i in 0..n {
        func_params.push(IrParam { name: format!("a{i}"), ty: q_type() });
    }
    for i in 0..n {
        func_params.push(IrParam { name: format!("b{i}"), ty: q_type() });
    }
    func_params.push(IrParam { name: "hats".into(), ty: hats_slice });

    // Build the gadget over abstract ids (same as the prover): a = 0..n, b = n..2n.
    let a_ids: Vec<u32> = (0..n as u32).collect();
    let b_ids: Vec<u32> = (n as u32..2 * n as u32).collect();
    let mut buf = GateBuf::new(2 * n as u32);
    let result_id = emit_lt(&a_ids, &b_ids, &mut buf);

    let mut in_map = BTreeMap::<u32, String>::new();
    for i in 0..n {
        in_map.insert(i as u32, format!("a{i}"));
        in_map.insert((n + i) as u32, format!("b{i}"));
    }

    let mut stmts: Vec<IrStmt> = vec![IrStmt::Let {
        pattern: IrPattern::Ident { mutable: true, name: "all_ok".into(), subpat: None },
        ty: None,
        init: Some(IrExpr::Lit(IrLit::Bool(true))),
    }];
    let mut and_counter = 0usize;
    let mk_qand = |k: usize| IrExpr::Index {
        base: Box::new(var("q_ands")),
        index: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
    };
    let mk_hat = |k: usize| IrExpr::Index {
        base: Box::new(var("hats")),
        index: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
    };
    let vmap = lower_gadget_verifier(
        &buf.gates, &in_map, "lt", &mut and_counter, &mk_qand, &mk_hat, "all_ok", &mut stmts,
    );
    let _ = (&vmap, result_id);

    let block0 = IrCfgBlock {
        params: vec![],
        stmts,
        stmt_provs: vec![],
        terminator: IrCfgTerminator::Return(Some(var("all_ok"))),
    };
    let func = IrCfgFunction {
        name: format!("lt_check_verify_{name}"),
        generics,
        receiver: None,
        params: func_params,
        return_type: Some(bool_type()),
        where_clause,
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: vec![block0] },
    };
    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_lt_check_verify_{name}"),
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
        "use volar_spec::vole::{Delta, Q, Vope, VoleArray};\n",
        "use volar_spec::vole::bridge::{mem_acc_absorb_vope, mem_acc_absorb_q, vope_bitpack, q_bitpack, mem_drain_open, mem_drain_check, vope_open_mask, assert_one_check};\n",
        "use volar_spec::vole::prove::{vole_and_prover_step, vole_and_verifier_check};\n",
        "use volar_net::{vope_bit, VoleTransport};\n",
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

    /// Single-block loop with a 2-bit address: params (ids 0,1)=addr bits,
    /// (id 2)=value, (id 3)=done. Write(src=w2, addr=[w0,w1]); Read(addr=[w0,w1])
    /// → id 5. Jmp next state = [w0,w1,w2,read(id5)] (width = num_params 4),
    /// done = w3.
    fn build_storage_loop_2bit() -> BIrBlocks {
        BIrBlocks {
            blocks: vec![BIrBlock {
                params: 4,
                stmts: vec![
                    BIrStmt::StorageWrite {
                        storage: StorageId(0),
                        src: IRVarId(2),
                        bit_width: 1,
                        addr: vec![IRVarId(0), IRVarId(1)],
                    },
                    BIrStmt::StorageRead {
                        storage: StorageId(0),
                        bit_width: 1,
                        addr: vec![IRVarId(0), IRVarId(1)],
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
    fn test_storage_loop_compiles() {
        let circuit = build_storage_loop();
        let module = weave_storage_commit_loop_prover(&circuit, "test", None);
        let code = print_storage_loop_module(&module);
        run_compile_check_net(&code, "storage_commit_loop");
    }

    #[test]
    fn test_ts_storage_loop_multibit_addr_compiles() {
        // 2-bit address ⇒ 4 cells of init/drain; ts_bits=3.
        let circuit = build_storage_loop_2bit();
        let mp = weave_ts_storage_loop_prover(&circuit, 3, 2, "mb", None);
        let codep = print_storage_loop_module(&mp);
        run_compile_check_net(&codep, "ts_storage_loop_prover_2bit");
        // 4 cells (final_val0..3) materialised; address bit-packed.
        assert!(codep.contains("final_val3"), "2^addr_bits=4 drain cells");
        let mv = weave_ts_storage_loop_verifier(&circuit, 3, 2, "mb", None);
        let codev = print_storage_loop_module(&mv);
        run_compile_check_net(&codev, "ts_storage_loop_verifier_2bit");
        assert!(codev.contains("q_final_val3"), "verifier mirrors 4 drain cells");
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

    #[test]
    fn test_ts_storage_loop_prover_compiles() {
        let circuit = build_storage_loop();
        let module = weave_ts_storage_loop_prover(&circuit, 4, 1, "test", None);
        let code = print_storage_loop_module(&module);
        run_compile_check_net(&code, "ts_storage_loop_prover");
    }

    #[test]
    fn test_ts_storage_loop_prover_is_sound_shaped() {
        let circuit = build_storage_loop();
        let module = weave_ts_storage_loop_prover(&circuit, 4, 1, "test", None);
        let code = print_storage_loop_module(&module);
        // committed-counter ordering gadget + increment lower to hats
        assert!(code.contains("vole_and_prover_step"), "ordering/incr ANDs → hats");
        assert!(code.contains("vope_bitpack"), "timestamps bitpacked into the multiset value");
        assert!(code.contains("send_iteration"), "hats streamed per iteration");
        assert!(code.contains("mem_drain_open"), "produce/consume drain opened at exit");
        assert!(code.contains("vope_open_mask"), "ordering result opened at exit");
        assert!(code.contains("read_last_ts"), "per-access committed t_last witness");
    }

    #[test]
    fn test_ts_storage_loop_verifier_compiles() {
        let circuit = build_storage_loop();
        let module = weave_ts_storage_loop_verifier(&circuit, 4, 1, "test", None);
        let code = print_storage_loop_module(&module);
        run_compile_check_net(&code, "ts_storage_loop_verifier");
    }

    #[test]
    fn test_ts_storage_loop_verifier_checks_drain_and_order() {
        let circuit = build_storage_loop();
        let module = weave_ts_storage_loop_verifier(&circuit, 4, 1, "test", None);
        let code = print_storage_loop_module(&module);
        assert!(code.contains("vole_and_verifier_check"), "gadget ANDs checked");
        assert!(code.contains("mem_acc_absorb_q"), "Q-side multiset absorb");
        assert!(code.contains("mem_drain_check"), "produce/consume drain checked");
        assert!(code.contains("assert_one_check"), "ordering result asserted == 1");
        assert!(code.contains("recv_iteration"), "hats received per iteration");
    }

    #[test]
    fn test_lt_check_verifier_lowers_to_vole() {
        // Verifier mirror: gadget ANDs lower to vole_and_verifier_check, hats
        // consumed from the slice, Xor/Not are free Q combinations.
        let module = weave_lt_check_verifier(4, "ts", None);
        let code = print_storage_loop_module(&module);
        run_compile_check_net(&code, "lt_check_verify_gadget");
        assert!(code.contains("vole_and_verifier_check"), "expected gadget ANDs checked");
    }
}
