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
//! This is the path used by [`GrafhenScheme`] and [`TfheScheme`].
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
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        ExternalKind, IrBlock, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgJump, IrCfgModule,
        IrCfgTerminator, IrExpr, IrFunction, IrModule, IrParam, IrStmt, IrType,
        PrimitiveType,
    },
    linkage::LinkageSystem,
};
use volar_ir::ir::{
    IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRTypes, IRVarId,
};
use volar_ir_passes::{lower_ir_to_boolar, movfuscate_biir};

use crate::{build_return, expand_ors, var};

// ============================================================================
// FheScheme trait
// ============================================================================

/// Trait implemented by each FHE scheme to guide the weaver.
///
/// Implement the binary gate methods for the **flat path** (`cfg_capable = false`).
/// Implement [`emit_ir_stmt`] and related methods for the **CFG path**.
///
/// # Choosing a path
///
/// - Binary circuits (TFHE gate-by-gate, GRAFHEN): implement binary gate methods,
///   leave `cfg_capable` as `false`.
/// - Arithmetic schemes (CKKS, BFV/BGV): implement `emit_ir_stmt` for typed
///   operations, set `cfg_capable = true`, and return `Some(...)` for every
///   `IRStmt` variant that may appear in inputs.  Returning `None` from
///   `emit_ir_stmt` in CFG mode causes a panic; use `cfg_capable = false` if
///   any stmt variant cannot be handled.
pub trait FheScheme {
    // ── Control flow capability ───────────────────────────────────────────────

    /// Whether this scheme can operate on `IRBlocks` directly (CFG path).
    ///
    /// - `false` (default): the weaver lowers to `BIrBlocks`, movfuscates, then
    ///   calls the binary gate methods.  Returns [`FheOutput::Flat`].
    /// - `true`: the weaver passes each [`IRStmt`] to [`emit_ir_stmt`] without
    ///   flattening.  Returns [`FheOutput::Cfg`].  The scheme MUST handle every
    ///   stmt variant that appears in the input; returning `None` panics.
    fn cfg_capable(&self) -> bool {
        false
    }

    // ── Common ────────────────────────────────────────────────────────────────

    /// The Rust type of a single "wire" value in this scheme.
    ///
    /// In the flat path this is the type of every gate output variable.
    /// In the CFG path this is the type of every variable in the emitted function.
    fn wire_type(&self) -> IrType;

    /// Additional function parameters the emitted function requires.
    ///
    /// These are prepended to (or appended after, at the scheme's discretion)
    /// the circuit input parameters.  Examples: public key, evaluation key, Δ.
    fn extra_params(&self) -> Vec<IrParam>;

    // ── Binary gate path (flat) ───────────────────────────────────────────────

    /// Emit a constant-zero wire.
    fn emit_zero(&self) -> IrExpr;

    /// Emit a constant-one wire.
    fn emit_one(&self) -> IrExpr;

    /// Emit an XOR of two wires.
    fn emit_xor(&self, a: IrExpr, b: IrExpr) -> IrExpr;

    /// Emit a NOT of a wire.
    fn emit_not(&self, a: IrExpr) -> IrExpr;

    /// Emit an AND of two wires.
    ///
    /// `gate_idx` is a 0-based count of AND gates already emitted, allowing
    /// schemes that need per-gate material (bootstrapping keys, garble tables)
    /// to index into a parameter array.
    fn emit_and(&self, a: IrExpr, b: IrExpr, gate_idx: usize) -> IrExpr;

    // ── CFG path ─────────────────────────────────────────────────────────────

    /// Try to emit a typed [`IRStmt`] natively.
    ///
    /// - `Some(expr)` → the scheme handles this stmt; the weaver emits
    ///   `let var_N = expr;`.
    /// - `None` → unhandled.  In CFG mode (`cfg_capable = true`) this **panics**.
    ///   Override this method only when `cfg_capable` returns `true` AND the
    ///   scheme can handle every stmt variant that may appear.
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
    /// `output_vars` are the variable names of the values being returned.
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
/// Both paths produce semantically equivalent output; the CFG path may
/// be more efficient for schemes that support native control flow.
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
// Flat path implementation
// ============================================================================

fn weave_fhe_flat<S: FheScheme>(
    blocks: &IRBlocks,
    types: &IRTypes,
    scheme: &S,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    // Lower to boolean, then movfuscate into a single circuit block.
    let bir_blocks = lower_ir_to_boolar(blocks, types);
    let circuit = movfuscate_biir(&bir_blocks);
    assert!(
        circuit.is_circuit(),
        "weave_fhe_flat: circuit after movfuscation must satisfy is_circuit()"
    );

    let block = &circuit.0[0];
    let expanded = expand_ors(block);
    let num_inputs = block.params as u32;
    let wire_ty = scheme.wire_type();

    // Build parameter list: extra scheme params + one input per circuit param.
    let mut params: Vec<IrParam> = scheme.extra_params();
    for i in 0..num_inputs {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: wire_ty.clone(),
        });
    }

    // Map input vars to their names.
    let mut var_names: BTreeMap<u32, String> = BTreeMap::new();
    for i in 0..num_inputs {
        var_names.insert(i, format!("input_{}", i));
    }

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut stmt_provs: Vec<()> = Vec::new();
    let mut and_gate_idx: usize = 0;

    for (result_id, stmt, _prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let init_expr = match stmt {
            volar_ir::boolar::BIrStmt::Zero => scheme.emit_zero(),
            volar_ir::boolar::BIrStmt::One => scheme.emit_one(),
            volar_ir::boolar::BIrStmt::Xor(a, b) => {
                let ea = var(var_names[&a.0].as_str());
                let eb = var(var_names[&b.0].as_str());
                scheme.emit_xor(ea, eb)
            }
            volar_ir::boolar::BIrStmt::Not(a) => {
                let ea = var(var_names[&a.0].as_str());
                scheme.emit_not(ea)
            }
            volar_ir::boolar::BIrStmt::And(a, b) => {
                let ea = var(var_names[&a.0].as_str());
                let eb = var(var_names[&b.0].as_str());
                let out = scheme.emit_and(ea, eb, and_gate_idx);
                and_gate_idx += 1;
                out
            }
            volar_ir::boolar::BIrStmt::Or(_, _) => {
                unreachable!("weave_fhe_flat: Or gates should have been expanded")
            }
            volar_ir::boolar::BIrStmt::OracleCall { name: oracle_name, args, num_bits } => {
                panic!(
                    "weave_fhe_flat: OracleCall '{}' not supported by scheme '{}'. \
                     Implement oracle handling or use a scheme that supports it.",
                    oracle_name, name
                );
            }
            volar_ir::boolar::BIrStmt::OracleBit { call, bit } => {
                panic!(
                    "weave_fhe_flat: OracleBit not supported by scheme '{}'. \
                     Implement oracle handling or use a scheme that supports it.",
                    name
                );
            }
            volar_ir::boolar::BIrStmt::ActionCall { .. } => {
                panic!(
                    "weave_fhe_flat: ActionCall not supported by scheme '{}'. \
                     Implement action handling or use a scheme that supports it.",
                    name
                );
            }
            volar_ir::boolar::BIrStmt::ActionBit { .. } => {
                panic!(
                    "weave_fhe_flat: ActionBit not supported by scheme '{}'. \
                     Implement action handling or use a scheme that supports it.",
                    name
                );
            }
            volar_ir::boolar::BIrStmt::Rng { name: rng_name } => {
                panic!(
                    "weave_fhe_flat: Rng '{}' not supported by scheme '{}'. \
                     Implement Rng handling or use a scheme that supports it.",
                    rng_name, name
                );
            }
            volar_ir::boolar::BIrStmt::StorageRead { .. }
            | volar_ir::boolar::BIrStmt::StorageWrite { .. } => {
                unimplemented!("weave_fhe_flat: StorageRead/Write not yet supported")
            }
        };

        stmts.push(IrStmt::Let {
            pattern: volar_compiler::ir::IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        stmt_provs.push(());
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, wire_ty);

    let func = IrFunction {
        name: format!("{}_{}", name, "fhe"),
        generics: vec![],
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(alloc::boxed::Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: format!("weaved_{}_fhe", name),
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
    types: &IRTypes,
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
            // Function params are named "input_0", "input_1", ...
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
                pattern: volar_compiler::ir::IrPattern::ident(&let_name),
                ty: None,
                init: Some(init_expr),
            });
            stmt_provs.push(());
            var_map.insert(result_ir_vid, let_name);
        }

        // Map the IRTerminator to an IrCfgTerminator.
        let terminator = map_ir_terminator(&ir_block.terminator, &var_map, scheme, &wire_ty);

        cfg_blocks.push(IrCfgBlock {
            params: if bidx == 0 { vec![] } else { block_params },
            stmts,
            stmt_provs,
            terminator,
        });
    }

    // Build the return type from the entry block's terminator if it's a Return.
    // For CFG functions we use the scheme's wire_type as the return type.
    // The printer handles the specific return expression.
    let return_type = Some(wire_ty.clone());

    // Build function params from entry block (block 0).
    let mut func_params: Vec<IrParam> = scheme.extra_params();
    if let Some(entry) = blocks.blocks.first() {
        for i in 0..entry.params.len() {
            func_params.push(IrParam {
                name: format!("input_{}", i),
                ty: wire_ty.clone(),
            });
        }
    }

    let func = IrCfgFunction {
        name: format!("{}_{}", name, "fhe_cfg"),
        generics: vec![],
        receiver: None,
        params: func_params,
        return_type,
        where_clause: vec![],
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: cfg_blocks },
    };

    let mut module = IrCfgModule {
        name: format!("weaved_{}_fhe_cfg", name),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    };
    // Note: LinkageSystem currently operates on IrModule; CFG linkage is future work.
    // If linkage is Some, we log a warning (no-std: just ignore for now).
    let _ = linkage;
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
                    IRBlockTargetId::Return => usize::MAX, // sentinel; handled below
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

/// A wrapper that implements [`FheScheme`] by delegating to the GRAFHEN gate functions.
///
/// This is provided as a reference implementation demonstrating the flat path.
/// The underlying cryptographic security of GRAFHEN is experimental (IND-CPA broken);
/// see `grafhen.rs` for details.
///
/// # Usage
///
/// ```ignore
/// let scheme = GrafhenScheme::new(4);
/// let output = weave_fhe(&ir_blocks, &types, &scheme, "my_circuit", None);
/// ```
pub struct GrafhenScheme {
    /// Bit width of a GRAFHEN word (controls noise budget).
    pub word_bound: u8,
}

impl GrafhenScheme {
    pub fn new(word_bound: u8) -> Self {
        GrafhenScheme { word_bound }
    }
}

impl FheScheme for GrafhenScheme {
    fn wire_type(&self) -> IrType {
        IrType::Struct {
            kind: volar_compiler::ir::StructKind::Custom(
                format!("GrafhenWord<{}>", self.word_bound),
            ),
            type_args: vec![],
        }
    }

    fn extra_params(&self) -> Vec<IrParam> {
        vec![IrParam {
            name: "pk".into(),
            ty: IrType::Reference {
                mutable: false,
                elem: alloc::boxed::Box::new(IrType::Struct {
                    kind: volar_compiler::ir::StructKind::Custom("GrafhenPublic".into()),
                    type_args: vec![],
                }),
            },
        }]
    }

    fn emit_zero(&self) -> IrExpr {
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["grafhen_zero".into()],
                type_args: vec![],
            }),
            args: vec![var("pk")],
        }
    }

    fn emit_one(&self) -> IrExpr {
        // GRAFHEN one = NOT(zero)
        let zero = self.emit_zero();
        self.emit_not(zero)
    }

    fn emit_xor(&self, a: IrExpr, b: IrExpr) -> IrExpr {
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["grafhen_xor".into()],
                type_args: vec![],
            }),
            args: vec![a, b, var("pk")],
        }
    }

    fn emit_not(&self, a: IrExpr) -> IrExpr {
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["grafhen_not".into()],
                type_args: vec![],
            }),
            args: vec![a, var("pk")],
        }
    }

    fn emit_and(&self, a: IrExpr, b: IrExpr, _gate_idx: usize) -> IrExpr {
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["grafhen_and".into()],
                type_args: vec![],
            }),
            args: vec![a, b, var("pk")],
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
        // Placeholder: real TFHE ciphertexts would be LWE ciphertexts.
        IrType::Struct {
            kind: volar_compiler::ir::StructKind::Custom("LweCiphertext".into()),
            type_args: vec![],
        }
    }

    fn extra_params(&self) -> Vec<IrParam> {
        // Bootstrapping key required for AND gates.
        vec![IrParam {
            name: "bk".into(),
            ty: IrType::Reference {
                mutable: false,
                elem: alloc::boxed::Box::new(IrType::Struct {
                    kind: volar_compiler::ir::StructKind::Custom("BootstrappingKey".into()),
                    type_args: vec![],
                }),
            },
        }]
    }

    fn emit_zero(&self) -> IrExpr {
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["tfhe_trivial_zero".into()],
                type_args: vec![],
            }),
            args: vec![],
        }
    }

    fn emit_one(&self) -> IrExpr {
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["tfhe_trivial_one".into()],
                type_args: vec![],
            }),
            args: vec![],
        }
    }

    fn emit_xor(&self, a: IrExpr, b: IrExpr) -> IrExpr {
        // XOR is a free gate in TFHE (no bootstrapping needed).
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["tfhe_xor".into()],
                type_args: vec![],
            }),
            args: vec![a, b],
        }
    }

    fn emit_not(&self, a: IrExpr) -> IrExpr {
        // NOT is free in TFHE.
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["tfhe_not".into()],
                type_args: vec![],
            }),
            args: vec![a],
        }
    }

    fn emit_and(&self, a: IrExpr, b: IrExpr, _gate_idx: usize) -> IrExpr {
        // AND requires gate bootstrapping.
        IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
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
