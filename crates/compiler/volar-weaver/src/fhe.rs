// @reliability: experimental
// @ai: assisted
//! Generic FHE scheme abstraction and weaver.
//!
//! Provides the [`FheScheme`] trait, which decouples scheme-specific gate
//! emission from the shared weaving infrastructure, and [`weave_fhe`], the
//! single entry point that dispatches to the correct path based on the scheme's
//! capabilities.
//!
//! ## Two weaving paths
//!
//! ### Flat path (`cfg_capable() == false`, default)
//!
//! The weaver:
//! 1. Lowers `IRBlocks` → `BIrBlocks` via `lower_ir_to_boolar`.
//! 2. Movfuscates the result into a single flat circuit.
//! 3. Expands OR gates via De Morgan.
//! 4. Iterates the flat gate list, calling the binary gate methods on the scheme.
//! 5. Returns [`FheOutput::Flat`] wrapping an [`IrModule`].
//!
//! Use [`weave_fhe_flat_bir`] directly when you already hold a `BIrBlocks`
//! (e.g. from a previous lowering step) and need provenance threading.
//!
//! ### CFG path (`cfg_capable() == true`)
//!
//! The weaver:
//! 1. Iterates the original `IRBlocks` block-by-block.
//! 2. For each [`IRStmt`], calls [`FheScheme::emit_ir_stmt`].
//!    - `Some(expr)` → emits a `let var_N = expr;` binding.
//!    - `None` → **panics** with a clear error. Schemes that cannot handle all
//!      `IRStmt` variants must use `cfg_capable() == false`.
//! 3. Maps each [`IRTerminator`] to an [`IrCfgTerminator`].
//! 4. Returns [`FheOutput::Cfg`] wrapping an [`IrCfgModule`].
//!
//! > **Future work:** A per-stmt binary fallback (lowering individual unhandled
//! > stmts to `BIrStmt`s) requires bridging two variable namespaces (IRBlocks
//! > typed vars vs. BIrBlocks bit vars) and scheme-specific bit reconstruction.
//! > This is deferred until a concrete CFG-capable scheme (e.g., TFHE-CFG)
//! > is being implemented.

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
        ExternalKind, IrBlock, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgJump, IrCfgModule,
        IrCfgTerminator, IrExpr, IrFunction, IrGenericParam, IrGenericParamKind, IrModule,
        IrParam, IrPattern, IrStmt, IrTraitBound, IrType, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::{
    boolar::{BIrBlocks, BIrStmt},
    ir::{IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRTypes, IRVarId},
};
use volar_ir_passes::{lower_ir_to_boolar, movfuscate_biir};

use crate::{
    build_return, clone_expr, expand_ors, ref_expr, var, NoProvenance, ProvenanceHandler,
};

// ============================================================================
// FheScheme trait
// ============================================================================

/// Trait implemented by each FHE scheme to guide the weaver.
///
/// # Flat path (default, `cfg_capable = false`)
///
/// The weaver lowers `IRBlocks` to boolean gates, movfuscates into a single
/// circuit, then calls the binary gate and oracle/action/rng methods in order.
/// Implement all `emit_*` methods that may appear in the input; any unimplemented
/// variant causes a panic with a descriptive message at weave time.
///
/// # CFG path (`cfg_capable = true`)
///
/// Implement [`emit_ir_stmt`] to handle typed `IRStmt`s without flattening.
/// Returning `None` from `emit_ir_stmt` panics the weaver.
///
/// # Provenance
///
/// Emit methods are generic over the output provenance type `Q`.  This lets the
/// weaver thread provenance from the input circuit through to the output
/// `IrModule<Q>` without any work in the scheme implementation — the expression
/// constructors (`IrExpr::Call`, `IrExpr::Path`, …) carry Q as a phantom type.
pub trait FheScheme {
    // ── Control flow ─────────────────────────────────────────────────────────

    /// Whether this scheme operates on `IRBlocks` directly (CFG path).
    ///
    /// `false` (default): lowers to `BIrBlocks`, movfuscates, then calls the
    /// binary gate methods.  Returns [`FheOutput::Flat`].
    ///
    /// `true`: passes each [`IRStmt`] to [`emit_ir_stmt`].  Returns
    /// [`FheOutput::Cfg`].  Every stmt variant that may appear in the input
    /// MUST be handled; returning `None` panics.
    fn cfg_capable(&self) -> bool {
        false
    }

    // ── Type and signature ────────────────────────────────────────────────────

    /// The Rust type of an owned gate-output wire in this scheme.
    ///
    /// Used as the return type and the type of intermediate `let wire_N` bindings.
    fn wire_type(&self) -> IrType;

    /// The Rust type of a circuit *input* parameter.
    ///
    /// Defaults to [`wire_type`].  Override when inputs have a different type
    /// (e.g. GRAFHEN passes inputs by reference: `&GrafhenWord<WBOUND>`).
    fn input_type(&self) -> IrType {
        self.wire_type()
    }

    /// Additional function parameters prepended before the circuit inputs.
    ///
    /// Examples: public key (`pk`), bootstrapping key (`bk`), global secret Δ.
    fn extra_params(&self) -> Vec<IrParam>;

    /// Generic type parameters on the emitted function.
    ///
    /// Example: GRAFHEN emits `<R: WordReducer<WBOUND>>`.
    /// Default: no generics.
    fn generics(&self) -> Vec<IrGenericParam> {
        vec![]
    }

    /// Suffix appended to the circuit name to form the emitted function name.
    ///
    /// The function is named `{circuit_name}_{suffix}`.
    /// Default: `"fhe"`.  GRAFHEN overrides to `"grafhen"`.
    fn fn_name_suffix(&self) -> &str {
        "fhe"
    }

    // ── Binary gate path ─────────────────────────────────────────────────────

    /// Emit a constant-zero wire.
    fn emit_zero<Q: Clone + Default>(&self) -> IrExpr<Q>;

    /// Emit a constant-one wire.
    fn emit_one<Q: Clone + Default>(&self) -> IrExpr<Q>;

    /// Emit an XOR of two wires.
    fn emit_xor<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q>;

    /// Emit a NOT of a wire.
    fn emit_not<Q: Clone + Default>(&self, a: IrExpr<Q>) -> IrExpr<Q>;

    /// Emit an AND of two wires.
    ///
    /// `gate_idx` is a 0-based count of AND gates already emitted, allowing
    /// schemes that need per-gate material (bootstrapping keys, garble tables)
    /// to index into a parameter array.
    fn emit_and<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>, gate_idx: usize) -> IrExpr<Q>;

    // ── Oracle calls ──────────────────────────────────────────────────────────

    /// Emit a call to a named pure oracle.
    ///
    /// `arg_exprs` are the homomorphic arguments, already resolved from
    /// var_names.  `num_bits` is the total number of output bits.
    ///
    /// Default: panics at weave time.
    #[allow(unused_variables)]
    fn emit_oracle_call<Q: Clone + Default>(
        &self,
        oracle_name: &str,
        arg_exprs: Vec<IrExpr<Q>>,
        num_bits: usize,
    ) -> IrExpr<Q> {
        panic!(
            "FheScheme: emit_oracle_call not implemented (oracle: '{}').  \
             Either implement this method or avoid circuits with oracle calls.",
            oracle_name
        )
    }

    /// Emit the projection of bit `bit` from an oracle call result variable.
    ///
    /// `call_var` is the variable name bound to the [`emit_oracle_call`] result.
    ///
    /// Default: panics at weave time.
    #[allow(unused_variables)]
    fn emit_oracle_bit<Q: Clone + Default>(&self, call_var: &str, bit: usize) -> IrExpr<Q> {
        panic!(
            "FheScheme: emit_oracle_bit not implemented (call_var: '{}', bit: {}).  \
             Either implement this method or avoid circuits with oracle calls.",
            call_var, bit
        )
    }

    // ── Action calls ──────────────────────────────────────────────────────────

    /// Emit a conditional action call.
    ///
    /// `guard_expr` is the homomorphic guard bit.
    /// `arg_exprs` are the homomorphic arguments.
    /// `fallback_exprs` are wire expressions to use when `guard = 0` (one per
    /// output bit); schemes that multiplex guard/no-guard paths need these.
    /// `num_bits` is the total number of output bits.
    ///
    /// Default: panics at weave time.
    #[allow(unused_variables)]
    fn emit_action_call<Q: Clone + Default>(
        &self,
        action_name: &str,
        guard_expr: IrExpr<Q>,
        arg_exprs: Vec<IrExpr<Q>>,
        fallback_exprs: Vec<IrExpr<Q>>,
        num_bits: usize,
    ) -> IrExpr<Q> {
        panic!(
            "FheScheme: emit_action_call not implemented (action: '{}').  \
             Either implement this method or avoid circuits with action calls.",
            action_name
        )
    }

    /// Emit the projection of bit `bit` from an action call result variable.
    ///
    /// `call_var` is the variable name bound to the [`emit_action_call`] result.
    ///
    /// Default: panics at weave time.
    #[allow(unused_variables)]
    fn emit_action_bit<Q: Clone + Default>(&self, call_var: &str, bit: usize) -> IrExpr<Q> {
        panic!(
            "FheScheme: emit_action_bit not implemented (call_var: '{}', bit: {}).  \
             Either implement this method or avoid circuits with action calls.",
            call_var, bit
        )
    }

    // ── RNG ───────────────────────────────────────────────────────────────────

    /// Emit a fresh random wire from the named RNG source.
    ///
    /// Default: panics at weave time.
    #[allow(unused_variables)]
    fn emit_rng<Q: Clone + Default>(&self, rng_name: &str) -> IrExpr<Q> {
        panic!(
            "FheScheme: emit_rng not implemented (rng: '{}').  \
             Either implement this method or avoid circuits with RNG sources.",
            rng_name
        )
    }

    // ── CFG path ─────────────────────────────────────────────────────────────

    /// Try to emit a typed [`IRStmt`] natively (CFG path only).
    ///
    /// - `Some(expr)` → the scheme handles this stmt.
    /// - `None` → unhandled.  Panics the weaver when `cfg_capable` returns `true`.
    ///
    /// `var_map` maps each in-scope `IRVarId.0` to its variable name string.
    #[allow(unused_variables)]
    fn emit_ir_stmt(
        &self,
        stmt: &IRStmt,
        var_map: &BTreeMap<u32, String>,
    ) -> Option<IrExpr> {
        None
    }

    /// Build the return expression for a CFG-path function's Return terminator.
    ///
    /// Default: single output → bare name; multiple outputs → tuple.
    fn emit_cfg_return(&self, output_vars: &[String]) -> IrExpr {
        match output_vars {
            [] => IrExpr::Tuple(vec![]),
            [single] => var(single),
            many => IrExpr::Tuple(many.iter().map(|s| var(s.as_str())).collect()),
        }
    }
}

// ============================================================================
// Output type
// ============================================================================

/// The output of [`weave_fhe`], parameterised by which weaving path was taken.
pub enum FheOutput {
    /// Produced by the flat path (`cfg_capable == false`).
    /// The circuit was movfuscated and emitted using binary gate methods.
    Flat(IrModule),
    /// Produced by the CFG path (`cfg_capable == true`).
    /// IRBlocks were processed directly, preserving control-flow structure.
    Cfg(IrCfgModule),
}

// ============================================================================
// Entry point
// ============================================================================

/// Weave an `IRBlocks` circuit using a specific FHE scheme.
///
/// Dispatches to the flat or CFG path based on [`FheScheme::cfg_capable`].
/// Provenance is erased (uses [`NoProvenance`]).  For provenance threading
/// on the flat path use [`weave_fhe_flat_bir`] directly.
pub fn weave_fhe<S: FheScheme>(
    blocks: &IRBlocks,
    types: &IRTypes,
    scheme: &S,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> FheOutput {
    if scheme.cfg_capable() {
        FheOutput::Cfg(weave_fhe_cfg(blocks, types, scheme, name, linkage))
    } else {
        FheOutput::Flat(weave_fhe_flat(blocks, types, scheme, name, linkage))
    }
}

// ============================================================================
// Flat path — high-level entry (lowers IRBlocks first)
// ============================================================================

fn weave_fhe_flat<S: FheScheme>(
    blocks: &IRBlocks,
    types: &IRTypes,
    scheme: &S,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    let bir_blocks = lower_ir_to_boolar(blocks, types);
    let circuit = movfuscate_biir(&bir_blocks);
    assert!(
        circuit.is_circuit(),
        "weave_fhe_flat: circuit after movfuscation must satisfy is_circuit()"
    );
    weave_fhe_flat_bir(&circuit, scheme, name, linkage, &NoProvenance)
}

// ============================================================================
// Flat path — core (takes BIrBlocks, threads provenance)
// ============================================================================

/// Core flat weaver: takes an already-movfuscated `BIrBlocks` circuit plus a
/// [`ProvenanceHandler`] and produces an `IrModule<H::Output>`.
///
/// This is the low-level entry point used by both [`weave_fhe`] and by
/// callers (such as `weave_grafhen_with_handler`) that already hold a
/// `BIrBlocks` and want to preserve per-statement provenance.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()` (single block, Return
/// terminator), or if the scheme does not implement a required emit method for
/// a gate variant present in the circuit.
pub fn weave_fhe_flat_bir<P, H, S>(
    circuit: &BIrBlocks<P>,
    scheme: &S,
    name: &str,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
    S: FheScheme,
{
    assert!(
        circuit.is_circuit(),
        "weave_fhe_flat_bir: circuit must satisfy is_circuit() \
         (single block with Return terminator)"
    );

    let block = &circuit.0[0];
    let expanded = expand_ors(block);
    let num_inputs = block.params as u32;
    let wire_ty = scheme.wire_type();
    let input_ty = scheme.input_type();

    // Build parameter list: extra scheme params + one input per circuit param.
    let mut params: Vec<IrParam> = scheme.extra_params();
    for i in 0..num_inputs {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: input_ty.clone(),
        });
    }

    // Map input vars to their names.
    let mut var_names: BTreeMap<u32, String> = BTreeMap::new();
    for i in 0..num_inputs {
        var_names.insert(i, format!("input_{}", i));
    }

    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut stmt_provs: Vec<H::Output> = Vec::new();
    let mut and_gate_idx: usize = 0;

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        let init_expr: IrExpr<H::Output> = match stmt {
            BIrStmt::Zero => scheme.emit_zero(),
            BIrStmt::One => scheme.emit_one(),

            BIrStmt::Xor(a, b) => scheme.emit_xor(
                var(var_names[&a.0].as_str()),
                var(var_names[&b.0].as_str()),
            ),

            BIrStmt::Not(a) => scheme.emit_not(var(var_names[&a.0].as_str())),

            BIrStmt::And(a, b) => {
                let out = scheme.emit_and(
                    var(var_names[&a.0].as_str()),
                    var(var_names[&b.0].as_str()),
                    and_gate_idx,
                );
                and_gate_idx += 1;
                out
            }

            BIrStmt::Or(..) => {
                unreachable!("weave_fhe_flat_bir: Or gates should have been expanded")
            }

            BIrStmt::OracleCall { name: oracle_name, args, num_bits } => {
                let arg_exprs: Vec<IrExpr<H::Output>> = args
                    .iter()
                    .map(|id| var(var_names[&id.0].as_str()))
                    .collect();
                scheme.emit_oracle_call(oracle_name, arg_exprs, *num_bits)
            }

            BIrStmt::OracleBit { call, bit } => {
                scheme.emit_oracle_bit(var_names[&call.0].as_str(), *bit)
            }

            BIrStmt::ActionCall { name: action_name, guard, args, fallback, num_bits } => {
                let guard_expr: IrExpr<H::Output> = var(var_names[&guard.0].as_str());
                let arg_exprs: Vec<IrExpr<H::Output>> = args
                    .iter()
                    .map(|id| var(var_names[&id.0].as_str()))
                    .collect();
                let fallback_exprs: Vec<IrExpr<H::Output>> = fallback
                    .iter()
                    .map(|id| var(var_names[&id.0].as_str()))
                    .collect();
                scheme.emit_action_call(action_name, guard_expr, arg_exprs, fallback_exprs, *num_bits)
            }

            BIrStmt::ActionBit { call, bit } => {
                scheme.emit_action_bit(var_names[&call.0].as_str(), *bit)
            }

            BIrStmt::Rng { name: rng_name } => scheme.emit_rng(rng_name),

            BIrStmt::StorageRead { .. } | BIrStmt::StorageWrite { .. } => {
                unimplemented!("weave_fhe_flat_bir: StorageRead/Write not yet supported")
            }
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        stmt_provs.push(q);
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, wire_ty);

    let fn_name = format!("{}_{}", name, scheme.fn_name_suffix());
    let func = IrFunction {
        name: fn_name.clone(),
        generics: scheme.generics(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: format!("weaved_{}", fn_name),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// CFG path implementation
// ============================================================================

fn weave_fhe_cfg<S: FheScheme>(
    blocks: &IRBlocks,
    _types: &IRTypes,
    scheme: &S,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrCfgModule {
    let wire_ty = scheme.wire_type();

    let mut cfg_blocks: Vec<IrCfgBlock> = Vec::new();

    for (bidx, ir_block) in blocks.blocks.iter().enumerate() {
        // Block parameters.
        // Block 0's params are the function params (no block-param slot needed).
        // Other blocks carry their params via IrCfgBlock::params.
        let block_params: Vec<IrParam> = ir_block
            .params
            .iter()
            .enumerate()
            .map(|(pidx, _ty)| IrParam {
                name: format!("blk{}_p{}", bidx, pidx),
                ty: wire_ty.clone(),
            })
            .collect();

        // Variable name map for this block.
        let num_params = ir_block.params.len() as u32;
        let mut var_map: BTreeMap<u32, String> = BTreeMap::new();

        // Params: block 0 uses function param names; others use blk-param names.
        if bidx == 0 {
            for i in 0..num_params {
                var_map.insert(i, format!("input_{}", i));
            }
        } else {
            for (pidx, p) in block_params.iter().enumerate() {
                var_map.insert(pidx as u32, p.name.clone());
            }
        }

        let mut stmts: Vec<IrStmt> = Vec::new();
        let mut stmt_provs: Vec<()> = Vec::new();

        for (stmt_idx, ir_stmt) in ir_block.stmts.iter().enumerate() {
            let result_ir_vid = num_params + stmt_idx as u32;
            let let_name = format!("var_{}", result_ir_vid);

            let init_expr = scheme
                .emit_ir_stmt(ir_stmt, &var_map)
                .unwrap_or_else(|| {
                    panic!(
                        "weave_fhe_cfg: scheme cannot handle IRStmt variant in block {} stmt {}: {:?}. \
                         Either implement emit_ir_stmt for this variant, or set cfg_capable = false \
                         to use the automatic movfuscation path.",
                        bidx, stmt_idx, ir_stmt
                    )
                });

            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&let_name),
                ty: None,
                init: Some(init_expr),
            });
            stmt_provs.push(());
            var_map.insert(result_ir_vid, let_name);
        }

        let terminator = map_ir_terminator(&ir_block.terminator, &var_map, scheme, &wire_ty);

        cfg_blocks.push(IrCfgBlock {
            params: if bidx == 0 { vec![] } else { block_params },
            stmts,
            stmt_provs,
            terminator,
        });
    }

    let return_type = Some(wire_ty.clone());

    let mut func_params: Vec<IrParam> = scheme.extra_params();
    if let Some(entry) = blocks.blocks.first() {
        for i in 0..entry.params.len() {
            func_params.push(IrParam {
                name: format!("input_{}", i),
                ty: wire_ty.clone(),
            });
        }
    }

    let fn_name = format!("{}_{}_cfg", name, scheme.fn_name_suffix());
    let func = IrCfgFunction {
        name: fn_name.clone(),
        generics: vec![],
        receiver: None,
        params: func_params,
        return_type,
        where_clause: vec![],
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: cfg_blocks },
    };

    let module = IrCfgModule {
        name: format!("weaved_{}", fn_name),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    };
    let _ = linkage; // CFG linkage is future work
    module
}

/// Map an [`IRTerminator`] to an [`IrCfgTerminator`] using the current var_map.
fn map_ir_terminator<S: FheScheme>(
    term: &IRTerminator,
    var_map: &BTreeMap<u32, String>,
    scheme: &S,
    _wire_ty: &IrType,
) -> IrCfgTerminator {
    match term {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => {
            let output_vars: Vec<String> = args
                .iter()
                .map(|id| {
                    var_map
                        .get(&id.0)
                        .cloned()
                        .unwrap_or_else(|| format!("var_{}", id.0))
                })
                .collect();
            let ret_expr = scheme.emit_cfg_return(&output_vars);
            IrCfgTerminator::Return(Some(ret_expr))
        }
        IRTerminator::Jmp { func: IRBlockTargetId::Block(bid), args } => {
            IrCfgTerminator::Goto(IrCfgJump {
                target: bid.0 as usize,
                args: args
                    .iter()
                    .map(|id| {
                        var(var_map
                            .get(&id.0)
                            .map(|s| s.as_str())
                            .unwrap_or("__unknown"))
                    })
                    .collect(),
            })
        }
        IRTerminator::Jmp { func: IRBlockTargetId::Dyn(_), .. } => {
            panic!("weave_fhe_cfg: dynamic jump targets are not supported in CFG path")
        }
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => {
            let cond_name = var_map
                .get(&condition.0)
                .cloned()
                .unwrap_or_else(|| format!("var_{}", condition.0));

            let map_jump = |bid: &IRBlockTargetId, args: &[IRVarId]| -> IrCfgJump {
                let target = match bid {
                    IRBlockTargetId::Block(b) => b.0 as usize,
                    IRBlockTargetId::Return => usize::MAX, // sentinel
                    IRBlockTargetId::Dyn(_) => panic!("weave_fhe_cfg: dynamic jump in CondJmp"),
                };
                IrCfgJump {
                    target,
                    args: args
                        .iter()
                        .map(|id| {
                            var(var_map
                                .get(&id.0)
                                .map(|s| s.as_str())
                                .unwrap_or("__unknown"))
                        })
                        .collect(),
                }
            };

            IrCfgTerminator::CondGoto {
                cond: var(&cond_name),
                then_: map_jump(true_block, true_args),
                else_: map_jump(false_block, false_args),
            }
        }
        IRTerminator::JumpTable { .. } => {
            panic!("weave_fhe_cfg: JumpTable terminators are not supported in CFG path")
        }
    }
}

// ============================================================================
// Reference implementation: GrafhenScheme
// ============================================================================

/// [`FheScheme`] implementation for GRAFHEN homomorphic evaluation.
///
/// Delegates gate emission to the `grafhen_*` family of functions from
/// `volar-spec`.  Inputs are passed as `&GrafhenWord<WBOUND>` references;
/// gate outputs are owned `GrafhenWord<WBOUND>` values.
///
/// The emitted function has signature:
/// ```text
/// fn {name}_grafhen<R: WordReducer<WBOUND>>(
///     pk: &GrafhenPublic<R, WBOUND>,
///     input_0: &GrafhenWord<WBOUND>,
///     ...
/// ) -> GrafhenWord<WBOUND>
/// ```
///
/// **WARNING: IND-CPA BROKEN.** See `docs/grafhen.md` and ePrint 2026/700.
/// Use only inside a ZK proof for correctness; never as a confidentiality
/// primitive.
///
/// Supports oracle calls (`{oracle}_grafhen(pk, &arg, ...)`),
/// action calls (`{action}_action_grafhen(pk, &guard, &arg, ...)`),
/// and RNG (`grafhen_encrypt({rng}(), pk)`).
pub struct GrafhenScheme {
    /// Concrete bit-width baked into `GrafhenWord<WBOUND>` and
    /// `GrafhenPublic<R, WBOUND>`.
    pub word_bound: usize,
}

impl GrafhenScheme {
    pub fn new(word_bound: usize) -> Self {
        GrafhenScheme { word_bound }
    }

    fn word_ty(&self) -> IrType {
        IrType::Struct {
            kind: StructKind::Custom("GrafhenWord".into()),
            type_args: vec![IrType::TypeParam(format!("{}", self.word_bound))],
        }
    }

    fn public_ty(&self) -> IrType {
        IrType::Struct {
            kind: StructKind::Custom("GrafhenPublic".into()),
            type_args: vec![
                IrType::TypeParam("R".into()),
                IrType::TypeParam(format!("{}", self.word_bound)),
            ],
        }
    }
}

impl FheScheme for GrafhenScheme {
    fn wire_type(&self) -> IrType {
        self.word_ty()
    }

    fn input_type(&self) -> IrType {
        IrType::Reference {
            mutable: false,
            elem: Box::new(self.word_ty()),
        }
    }

    fn extra_params(&self) -> Vec<IrParam> {
        vec![IrParam {
            name: "pk".into(),
            ty: IrType::Reference {
                mutable: false,
                elem: Box::new(self.public_ty()),
            },
        }]
    }

    fn generics(&self) -> Vec<IrGenericParam> {
        vec![IrGenericParam {
            name: "R".into(),
            kind: IrGenericParamKind::Type,
            bounds: vec![IrTraitBound {
                trait_kind: TraitKind::Custom("WordReducer".into()),
                type_args: vec![IrType::TypeParam(format!("{}", self.word_bound))],
                assoc_bindings: vec![],
            }],
            default: None,
        }]
    }

    fn fn_name_suffix(&self) -> &str {
        "grafhen"
    }

    fn emit_zero<Q: Clone + Default>(&self) -> IrExpr<Q> {
        // GrafhenWord::identity() — the additive identity (all-zero ciphertext).
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["GrafhenWord".into(), "identity".into()],
                type_args: vec![],
            }),
            args: vec![],
        }
    }

    fn emit_one<Q: Clone + Default>(&self) -> IrExpr<Q> {
        // pk.enc_one.clone()
        clone_expr(IrExpr::Field {
            base: Box::new(var("pk")),
            field: "enc_one".into(),
        })
    }

    fn emit_xor<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        // grafhen_xor(&a.clone(), &b.clone())
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["grafhen_xor".into()],
                type_args: vec![],
            }),
            args: vec![ref_expr(clone_expr(a)), ref_expr(clone_expr(b))],
        }
    }

    fn emit_not<Q: Clone + Default>(&self, a: IrExpr<Q>) -> IrExpr<Q> {
        // grafhen_not(&a.clone(), pk)
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["grafhen_not".into()],
                type_args: vec![],
            }),
            args: vec![ref_expr(clone_expr(a)), var("pk")],
        }
    }

    fn emit_and<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>, _gate_idx: usize) -> IrExpr<Q> {
        // grafhen_and(&a.clone(), &b.clone(), pk)
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["grafhen_and".into()],
                type_args: vec![],
            }),
            args: vec![ref_expr(clone_expr(a)), ref_expr(clone_expr(b)), var("pk")],
        }
    }

    fn emit_oracle_call<Q: Clone + Default>(
        &self,
        oracle_name: &str,
        arg_exprs: Vec<IrExpr<Q>>,
        _num_bits: usize,
    ) -> IrExpr<Q> {
        // {oracle_name}_grafhen(pk, &arg0.clone(), &arg1.clone(), ...)
        let mut args: Vec<IrExpr<Q>> = vec![var("pk")];
        args.extend(arg_exprs.into_iter().map(|a| ref_expr(clone_expr(a))));
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec![format!("{}_grafhen", oracle_name)],
                type_args: vec![],
            }),
            args,
        }
    }

    fn emit_oracle_bit<Q: Clone + Default>(&self, call_var: &str, bit: usize) -> IrExpr<Q> {
        // call_var.{bit}.clone()
        clone_expr(IrExpr::Field {
            base: Box::new(var(call_var)),
            field: format!("{}", bit),
        })
    }

    fn emit_action_call<Q: Clone + Default>(
        &self,
        action_name: &str,
        guard_expr: IrExpr<Q>,
        arg_exprs: Vec<IrExpr<Q>>,
        _fallback_exprs: Vec<IrExpr<Q>>,
        _num_bits: usize,
    ) -> IrExpr<Q> {
        // {action_name}_action_grafhen(pk, &guard.clone(), &arg0.clone(), ...)
        let mut args: Vec<IrExpr<Q>> = vec![var("pk"), ref_expr(clone_expr(guard_expr))];
        args.extend(arg_exprs.into_iter().map(|a| ref_expr(clone_expr(a))));
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec![format!("{}_action_grafhen", action_name)],
                type_args: vec![],
            }),
            args,
        }
    }

    fn emit_action_bit<Q: Clone + Default>(&self, call_var: &str, bit: usize) -> IrExpr<Q> {
        // call_var.{bit}.clone()
        clone_expr(IrExpr::Field {
            base: Box::new(var(call_var)),
            field: format!("{}", bit),
        })
    }

    fn emit_rng<Q: Clone + Default>(&self, rng_name: &str) -> IrExpr<Q> {
        // grafhen_encrypt({rng_name}(), pk)
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["grafhen_encrypt".into()],
                type_args: vec![],
            }),
            args: vec![
                IrExpr::Call {
                    func: Box::new(IrExpr::Path {
                        segments: vec![rng_name.into()],
                        type_args: vec![],
                    }),
                    args: vec![],
                },
                var("pk"),
            ],
        }
    }
}

// ============================================================================
// Reference implementation: TfheScheme (stub)
// ============================================================================

// @reliability: experimental
/// Stub implementation of [`FheScheme`] for TFHE (fully homomorphic encryption
/// over binary circuits via gate bootstrapping).
///
/// This is a **skeleton only** — the type names and function calls are
/// placeholders for a real TFHE library integration.  Do not use in production.
///
/// TFHE characteristics:
/// - `wire_type`: single-bit LWE ciphertext (`LweCiphertext`)
/// - XOR / NOT: free (no bootstrapping required)
/// - AND: requires gate bootstrapping via the bootstrapping key (`bk`)
/// - `cfg_capable = false`: uses the flat movfuscation path; CFG-capable TFHE
///   (via programmable bootstrapping) is future work.
///
/// See the `goals.md` TFHE entry for the full implementation plan.
pub struct TfheScheme;

impl FheScheme for TfheScheme {
    fn wire_type(&self) -> IrType {
        IrType::Struct {
            kind: StructKind::Custom("LweCiphertext".into()),
            type_args: vec![],
        }
    }

    fn extra_params(&self) -> Vec<IrParam> {
        vec![IrParam {
            name: "bk".into(),
            ty: IrType::Reference {
                mutable: false,
                elem: Box::new(IrType::Struct {
                    kind: StructKind::Custom("BootstrappingKey".into()),
                    type_args: vec![],
                }),
            },
        }]
    }

    fn emit_zero<Q: Clone + Default>(&self) -> IrExpr<Q> {
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["tfhe_trivial_zero".into()],
                type_args: vec![],
            }),
            args: vec![],
        }
    }

    fn emit_one<Q: Clone + Default>(&self) -> IrExpr<Q> {
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["tfhe_trivial_one".into()],
                type_args: vec![],
            }),
            args: vec![],
        }
    }

    fn emit_xor<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["tfhe_xor".into()],
                type_args: vec![],
            }),
            args: vec![a, b],
        }
    }

    fn emit_not<Q: Clone + Default>(&self, a: IrExpr<Q>) -> IrExpr<Q> {
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["tfhe_not".into()],
                type_args: vec![],
            }),
            args: vec![a],
        }
    }

    fn emit_and<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>, _gate_idx: usize) -> IrExpr<Q> {
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["tfhe_gate_bootstrapping_and".into()],
                type_args: vec![],
            }),
            args: vec![a, b, var("bk")],
        }
    }
}

// ============================================================================
// Printing
// ============================================================================

/// Render a CFG-weaved FHE `IrCfgModule` to Rust source.
pub fn print_fhe_cfg_module(module: &IrCfgModule, self_contained: bool) -> String {
    use volar_compiler::printer::{CfgModuleWriter, DisplayRust};
    use alloc::fmt::Write as _;

    let mut out = String::new();
    if self_contained {
        let _ = writeln!(
            out,
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, \
             non_snake_case, unused_parens)]"
        );
    }
    let _ = write!(out, "{}", DisplayRust(CfgModuleWriter { module }));
    out
}

/// Render a flat FHE `IrModule` to Rust source.
pub fn print_fhe_flat_module(module: &IrModule, self_contained: bool) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut out = String::new();
    if self_contained {
        let _ = writeln!(
            out,
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, \
             non_snake_case, unused_parens)]"
        );
    }
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module }));
    out
}
