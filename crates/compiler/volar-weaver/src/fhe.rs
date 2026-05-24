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
    collections::{BTreeMap, BTreeSet},
    format,
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        ArrayKind, ArrayLength,
        ExternalKind, IrAnyFunction, IrBlock, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgJump,
        IrCfgModule, IrCfgTerminator, IrExpr, IrFunction, IrGenericParam, IrGenericParamKind,
        IrLit, IrModule,
        IrParam, IrPattern, IrStmt, IrTraitBound, IrType, PrimitiveType, SpecBinOp, StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::{
    boolar::{BIrBlocks, BIrStmt},
    ir::{IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId, IRTypes, IRVarId, PrimType, StorageId},
    public::PublicSet,
};
use volar_ir_passes::{lower_ir_to_boolar, movfuscate_biir};

use crate::{
    build_return, clone_expr, expand_ors, ref_expr, var, NoProvenance, ProvenanceHandler,
};

// ============================================================================
// Action configuration types
// ============================================================================

/// Whether the guard condition for a conditional action is cleartext or encrypted.
/// Per-action configuration for the FHE weaver.
///
/// Specifies how the weaver should emit an `ActionCall` for a particular named
/// action function, including which outputs are public cleartext versus encrypted.
///
/// Guard mode is now auto-detected from the publicness of the guard variable at
/// the call site (via `public_set`). No explicit `guard_mode` is needed:
/// - Public guard (cleartext `bool`) → `action(guard, fallbacks..., args...)`
/// - Encrypted guard → `action(true, fallbacks..., guard_enc, args...)`
#[derive(Clone, Debug)]
pub struct FheActionConfig {
    /// Per-output publicness flags.
    ///
    /// `output_public[i] = true` means output `i` is a cleartext value (e.g.,
    /// `bool`).  `false` means it is an encrypted value (e.g., `LweCiphertext`).
    ///
    /// An **empty** vec means *all* outputs are public — this is the
    /// backward-compatible behaviour for actions whose name ends with `_pub`.
    pub output_public: Vec<bool>,
}

impl FheActionConfig {
    /// Returns `true` if output `idx` is public (cleartext).
    ///
    /// When `output_public` is empty (all-public sentinel), always returns `true`.
    pub fn is_output_public(&self, idx: usize) -> bool {
        self.output_public.is_empty() || self.output_public.get(idx).copied().unwrap_or(false)
    }

    /// Returns `true` if ALL outputs are public.
    pub fn all_outputs_public(&self) -> bool {
        self.output_public.is_empty() || self.output_public.iter().all(|&p| p)
    }
}

// ============================================================================
// Helpers used by FheScheme default implementations
// ============================================================================

/// Compute `ceil(log2(cell_count))` — the number of address bits needed to
/// address `cell_count` cells.  Returns 0 for counts ≤ 1.
fn effective_addr_width(cell_count: usize) -> usize {
    if cell_count <= 1 { return 0; }
    usize::BITS as usize - (cell_count - 1).leading_zeros() as usize
}

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

    /// The encrypted Rust type for a circuit-level [`IRTypeId`].
    ///
    /// - `Bit` → `wire_type()` (single ciphertext).
    /// - `Vec(N, Bit)` or packed bitvector of width W → `[wire_type(); W]`.
    /// - Field elements (`AES8`, `Galois64`) → unsupported; panics.
    ///
    /// Default implementation uses `ir_type_bit_width` to compute W.
    fn wire_type_for_ir(&self, ir_ty: IRTypeId, types: &IRTypes) -> IrType {
        let w = ir_type_bit_width(ir_ty, types);
        if w == 1 {
            self.wire_type()
        } else {
            IrType::Array {
                kind: ArrayKind::FixedArray,
                elem: Box::new(self.wire_type()),
                len: ArrayLength::Const(w),
            }
        }
    }

    /// The cleartext (public) Rust type for a circuit-level [`IRTypeId`].
    ///
    /// - `Bit` → `bool`.
    /// - `Vec(N, Bit)` or packed bitvector of width W → `[bool; W]`.
    fn public_type_for_ir(&self, ir_ty: IRTypeId, types: &IRTypes) -> IrType {
        let bool_ty = IrType::Primitive(PrimitiveType::Bool);
        let w = ir_type_bit_width(ir_ty, types);
        if w == 1 {
            bool_ty
        } else {
            IrType::Array {
                kind: ArrayKind::FixedArray,
                elem: Box::new(bool_ty),
                len: ArrayLength::Const(w),
            }
        }
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

    /// Emit an OR of two wires.
    ///
    /// Default: De Morgan — `NOT(AND(NOT(a), NOT(b)))`.
    /// TFHE overrides with `tfhe_gate_bootstrapping_or` for composability.
    fn emit_or<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        self.emit_not(self.emit_and(self.emit_not(a), self.emit_not(b), 0))
    }

    /// Emit a composable MUX: `sel ? a : b`.
    ///
    /// **TFHE default**: `OR(AND(sel, a), AND(NOT(sel), b))` — fully composable
    /// because it avoids non-composable XOR.
    ///
    /// GRAFHEN may override with the efficient `sel · (a ⊕ b) ⊕ b` since
    /// garbled-circuit XOR is free and composable.
    fn emit_cmux<Q: Clone + Default>(&self, sel: IrExpr<Q>, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        // Default: OR(AND(sel, a), AND(NOT(sel), b))
        // Uses clone_expr so the generated code calls .clone() on the sel wire.
        let sel_clone = clone_expr(sel.clone());
        let branch_a = self.emit_and(sel_clone, a, 0);
        let not_sel = self.emit_not(sel);
        let branch_b = self.emit_and(not_sel, b, 0);
        self.emit_or(branch_a, branch_b)
    }

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
    /// `type_map` maps each in-scope `IRVarId.0` to its [`IRTypeId`].
    /// `types` is the type intern table for the current circuit.
    #[allow(unused_variables)]
    fn emit_ir_stmt(
        &self,
        stmt: &IRStmt,
        var_map: &BTreeMap<u32, String>,
        type_map: &BTreeMap<u32, IRTypeId>,
        types: &IRTypes,
        public_set: &PublicSet,
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

    /// Return action-specific configuration for a named action.
    ///
    /// - `None` → the action is not handled by this scheme (weaver panics on
    ///   `ActionCall` for this action name).
    /// - `Some(config)` → emit using the provided guard mode and per-output
    ///   publicness.
    ///
    /// Default: returns `None` (no action support).
    ///
    /// **Backward compatibility**: [`TfheScheme`] overrides this to return
    /// `Some(FheActionConfig { guard_mode: Public, output_public: [] })` for
    /// any action whose name ends with `_pub`, matching the prior heuristic.
    #[allow(unused_variables)]
    fn action_config(&self, name: &str) -> Option<FheActionConfig> {
        None
    }

    /// Lift a cleartext (public) value to the scheme's wire type.
    ///
    /// Called at jump sites and return sites when a public variable (e.g.,
    /// `bool`) must be passed to a position that expects an encrypted wire
    /// (e.g., `LweCiphertext<N_LWE>`).
    ///
    /// `width` is the number of wire bits the value represents:
    /// - `width == 1`: the expression has type `bool`; wrap it in a single
    ///   encryption call.
    /// - `width > 1`: the expression has type `[bool; width]`; wrap each
    ///   element individually to produce `[wire_type(); width]`.
    ///
    /// Default: returns `expr` unchanged (identity — for schemes where public
    /// values already have the wire type, or where no promotion is needed).
    fn promote_to_wire<Q: Clone + Default>(&self, expr: IrExpr<Q>, _width: usize) -> IrExpr<Q> {
        expr
    }

    /// Return scheme-specific helper function stubs for type inference.
    ///
    /// These stubs carry return-type information needed by the LIR codegen (C
    /// backend).  They are tagged `ExternalKind::TypeStub` so that the Rust and
    /// TS printers skip them (the real implementations are imported via `use`).
    ///
    /// Default: empty (no scheme-specific helpers).
    fn helper_type_stubs(&self) -> Vec<IrFunction> {
        vec![]
    }

    // ── Oblivious storage access ─────────────────────────────────────────────

    /// Emit an oblivious read circuit that selects one of `cells` based on
    /// the encrypted `addr_wires`.
    ///
    /// `cells` contains the current wire name for each storage cell (at least 2;
    /// the caller handles the 0-cell and 1-cell edge cases before dispatching).
    ///
    /// `addr_wires` contains one wire name per address bit (bit 0 = LSB).
    /// `tag` is a unique prefix for naming intermediate wires.
    ///
    /// Returns the wire name of the read result.
    ///
    /// **Default**: recursive binary MUX tree.  Cost: `(next_pow2(N) − 1)` AND
    /// gates.  Override in scheme implementations to use alternative strategies
    /// (e.g. PBS-based lookup).
    fn emit_oblivious_read<Q: Clone + Default>(
        &self,
        cells: &[String],
        addr_wires: &[&str],
        tag: &str,
        stmts: &mut Vec<IrStmt<Q>>,
        stmt_provs: &mut Vec<Q>,
    ) -> String {
        match cells.len() {
            0 => {
                let z = format!("{}_z", tag);
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&z),
                    ty: None,
                    init: Some(self.emit_zero::<Q>()),
                });
                stmt_provs.push(Q::default());
                z
            }
            1 => cells[0].clone(),
            2 => {
                // MUX(sel, cell[0], cell[1]): sel=0 → cell[0], sel=1 → cell[1]
                let sel = addr_wires.first().copied().unwrap_or("_zero");
                let result = format!("{}_r", tag);
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&result),
                    ty: None,
                    init: Some(self.emit_cmux::<Q>(
                        var(sel),
                        clone_expr(var(&cells[1])),
                        clone_expr(var(&cells[0])),
                    )),
                });
                stmt_provs.push(Q::default());
                result
            }
            _ => {
                // Recursive split — pad to power-of-2.
                let n_pad = cells.len().next_power_of_two();
                let mid = n_pad / 2;
                let left = if mid <= cells.len() { &cells[..mid] } else { cells };
                let right = if mid < cells.len() { &cells[mid..] } else { &[] as &[String] };
                let remaining_addr = if addr_wires.len() > 1 { &addr_wires[1..] } else { &[] as &[&str] };

                let left_r = self.emit_oblivious_read(
                    left, remaining_addr, &format!("{}l", tag), stmts, stmt_provs,
                );
                let right_cells: Vec<String> = if right.is_empty() {
                    let zn = format!("{}rz", tag);
                    stmts.push(IrStmt::Let {
                        pattern: IrPattern::ident(&zn),
                        ty: None,
                        init: Some(self.emit_zero::<Q>()),
                    });
                    stmt_provs.push(Q::default());
                    vec![zn]
                } else {
                    right.to_vec()
                };
                let right_r = self.emit_oblivious_read(
                    &right_cells, remaining_addr, &format!("{}r", tag), stmts, stmt_provs,
                );

                // MUX the two halves: sel=0 → left, sel=1 → right.
                let sel = addr_wires.first().copied().unwrap_or("_zero");
                let result = format!("{}_r", tag);
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&result),
                    ty: None,
                    init: Some(self.emit_cmux::<Q>(
                        var(sel),
                        clone_expr(var(&right_r)),
                        clone_expr(var(&left_r)),
                    )),
                });
                stmt_provs.push(Q::default());
                result
            }
        }
    }

    /// Emit an oblivious write circuit that updates all `cells` so that the
    /// cell at the encrypted `addr_wires` address receives `src_wire` while
    /// all other cells retain their previous values.
    ///
    /// `cells` contains the current wire name for each storage cell (at least 2;
    /// the caller handles the 0-cell and 1-cell edge cases before dispatching).
    ///
    /// Returns a `Vec<String>` of new cell wire names, same length as `cells`.
    ///
    /// **Default**: full binary demux — for each cell, compute a one-hot
    /// selector (AND of address bit matches) then MUX(sel, old, src).
    /// Cost: roughly `addr_width * N` AND gates.  Override in scheme
    /// implementations to use alternative strategies.
    fn emit_oblivious_write<Q: Clone + Default>(
        &self,
        cells: &[String],
        addr_wires: &[&str],
        src_wire: &str,
        tag: &str,
        stmts: &mut Vec<IrStmt<Q>>,
        stmt_provs: &mut Vec<Q>,
    ) -> Vec<String> {
        let count = cells.len();
        let aw = effective_addr_width(count);

        // Pre-compute NOT of each address bit.
        let not_addr: Vec<String> = (0..aw)
            .map(|level| {
                let name = format!("{}_not{}", tag, level);
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&name),
                    ty: None,
                    init: Some(self.emit_not::<Q>(var(addr_wires[level]))),
                });
                stmt_provs.push(Q::default());
                name
            })
            .collect();

        let mut new_cells = Vec::with_capacity(count);
        for ci in 0..count {
            let old_cell = &cells[ci];

            // Build the one-hot selector for cell `ci`.
            let sel = if aw == 1 {
                if ci & 1 == 0 { not_addr[0].clone() } else { String::from(addr_wires[0]) }
            } else {
                let first_bit_wire = if ci & 1 == 0 { &not_addr[0] } else { addr_wires[0] };
                let mut acc = String::from(first_bit_wire);
                for k in 1..aw {
                    let bit_wire = if (ci >> k) & 1 == 0 { &not_addr[k] } else { addr_wires[k] };
                    let and_name = format!("{}_s{}b{}", tag, ci, k);
                    stmts.push(IrStmt::Let {
                        pattern: IrPattern::ident(&and_name),
                        ty: None,
                        init: Some(self.emit_and::<Q>(var(&acc), var(bit_wire), 0)),
                    });
                    stmt_provs.push(Q::default());
                    acc = and_name;
                }
                acc
            };

            // MUX(sel, old_cell, src): sel=0 → old, sel=1 → src
            let new_cell = format!("{}_c{}", tag, ci);
            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&new_cell),
                ty: None,
                init: Some(self.emit_cmux::<Q>(
                    var(&sel),
                    clone_expr(var(src_wire)),
                    clone_expr(var(old_cell)),
                )),
            });
            stmt_provs.push(Q::default());

            new_cells.push(new_cell);
        }
        new_cells
    }
}

// ============================================================================
// Loop-based oblivious access helpers
// ============================================================================

/// Emit an oblivious read as a runtime loop rather than an unrolled MUX tree.
///
/// Produces a `for` loop that iterates over all cells, computing a one-hot
/// selector for each cell index by matching plaintext bits of the loop variable
/// against the encrypted address bits, then conditionally accumulating the
/// cell value into a mutable result via `MUX(sel, result, cell) =
/// sel · (result ⊕ cell) ⊕ result`.
///
/// **Same asymptotic cost** as the unrolled MUX tree — roughly `N * (aw + 1)`
/// AND gates (where `aw = ceil(log2(N))`) — but the generated code is O(1) in
/// size instead of O(N).
///
/// Call this from an [`FheScheme::emit_oblivious_read`] override when the
/// target supports loops and compact code is preferred over fully unrolled
/// trees.
pub fn oblivious_read_loop<S: FheScheme, Q: Clone + Default>(
    scheme: &S,
    cells: &[String],
    addr_wires: &[&str],
    tag: &str,
    stmts: &mut Vec<IrStmt<Q>>,
    stmt_provs: &mut Vec<Q>,
) -> String {
    let count = cells.len();
    let aw = effective_addr_width(count);

    // Pre-compute NOT of each address bit (outside the loop).
    let not_addr: Vec<String> = (0..aw)
        .map(|k| {
            let name = format!("{}_not{}", tag, k);
            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&name),
                ty: None,
                init: Some(scheme.emit_not::<Q>(var(addr_wires[k]))),
            });
            stmt_provs.push(Q::default());
            name
        })
        .collect();

    // Build the cells array: let {tag}_arr = [c0.clone(), c1.clone(), ...];
    let arr_name = format!("{}_arr", tag);
    stmts.push(IrStmt::Let {
        pattern: IrPattern::ident(&arr_name),
        ty: None,
        init: Some(IrExpr::FixedArray(
            cells.iter().map(|c| clone_expr(var(c))).collect(),
        )),
    });
    stmt_provs.push(Q::default());

    // Initialize mutable result: let mut {tag}_result = emit_zero();
    let result_name = format!("{}_result", tag);
    stmts.push(IrStmt::Let {
        pattern: IrPattern::ident(&result_name).as_mut(),
        ty: None,
        init: Some(scheme.emit_zero::<Q>()),
    });
    stmt_provs.push(Q::default());

    // ── Loop body ────────────────────────────────────────────────────────
    let loop_var = format!("{}_i", tag);
    let mut body_stmts: Vec<IrStmt<Q>> = Vec::new();
    let mut body_provs: Vec<Q> = Vec::new();

    // For each address bit, select the matching wire based on the plaintext
    // bit of the loop variable:
    //   let bk = if (i >> k) & 1 == 0 { not_addr_k.clone() } else { addr_k.clone() };
    let mut bit_names: Vec<String> = Vec::new();
    for k in 0..aw {
        let bit_name = format!("{}_b{}", tag, k);
        let cond = IrExpr::Binary {
            op: SpecBinOp::Eq,
            left: Box::new(IrExpr::Binary {
                op: SpecBinOp::BitAnd,
                left: Box::new(IrExpr::Binary {
                    op: SpecBinOp::Shr,
                    left: Box::new(var(&loop_var)),
                    right: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
                }),
                right: Box::new(IrExpr::Lit(IrLit::Int(1))),
            }),
            right: Box::new(IrExpr::Lit(IrLit::Int(0))),
        };
        body_stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&bit_name),
            ty: None,
            init: Some(IrExpr::If {
                cond: Box::new(cond),
                then_branch: IrBlock {
                    stmts: vec![],
                    stmt_provs: vec![],
                    expr: Some(Box::new(clone_expr(var(&not_addr[k])))),
                },
                else_branch: Some(Box::new(IrExpr::Block(IrBlock {
                    stmts: vec![],
                    stmt_provs: vec![],
                    expr: Some(Box::new(clone_expr(var(addr_wires[k])))),
                }))),
            }),
        });
        body_provs.push(Q::default());
        bit_names.push(bit_name);
    }

    // Build the one-hot selector by chaining ANDs of all bit-match wires.
    let sel_name = if aw == 0 {
        // No address bits — always match (edge case, shouldn't reach here
        // since the caller handles count ≤ 1).
        let s = format!("{}_sel", tag);
        body_stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&s),
            ty: None,
            init: Some(scheme.emit_one::<Q>()),
        });
        body_provs.push(Q::default());
        s
    } else if aw == 1 {
        bit_names[0].clone()
    } else {
        let mut acc = bit_names[0].clone();
        for k in 1..aw {
            let and_name = format!("{}_sel{}", tag, k);
            body_stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&and_name),
                ty: None,
                init: Some(scheme.emit_and::<Q>(var(&acc), var(&bit_names[k]), 0)),
            });
            body_provs.push(Q::default());
            acc = and_name;
        }
        acc
    };

    // let cv = cells_arr[i].clone();
    let cv_name = format!("{}_cv", tag);
    body_stmts.push(IrStmt::Let {
        pattern: IrPattern::ident(&cv_name),
        ty: None,
        init: Some(clone_expr(IrExpr::Index {
            base: Box::new(var(&arr_name)),
            index: Box::new(var(&loop_var)),
        })),
    });
    body_provs.push(Q::default());

    // MUX(sel, result, cell_val): sel=0 → result, sel=1 → cell_val
    // result = cmux(sel, cell_val, result.clone());
    body_stmts.push(IrStmt::Semi(IrExpr::Assign {
        left: Box::new(var(&result_name)),
        right: Box::new(scheme.emit_cmux::<Q>(
            var(&sel_name),
            var(&cv_name),
            clone_expr(var(&result_name)),
        )),
    }));
    body_provs.push(Q::default());

    // ── Emit the loop ────────────────────────────────────────────────────
    stmts.push(IrStmt::Expr(IrExpr::BoundedLoop {
        var: loop_var,
        start: Box::new(IrExpr::Cast {
            expr: Box::new(IrExpr::Lit(IrLit::Int(0))),
            ty: Box::new(IrType::Primitive(PrimitiveType::Usize)),
        }),
        end: Box::new(IrExpr::Cast {
            expr: Box::new(IrExpr::Lit(IrLit::Int(count as i128))),
            ty: Box::new(IrType::Primitive(PrimitiveType::Usize)),
        }),
        inclusive: false,
        body: IrBlock {
            stmts: body_stmts,
            stmt_provs: body_provs,
            expr: None,
        },
    }));
    stmt_provs.push(Q::default());

    result_name
}

/// Emit an oblivious write as a runtime loop rather than unrolled demux+MUX.
///
/// Produces a `for` loop that iterates over all cells, computing a one-hot
/// selector for each cell index and conditionally updating the cell to
/// `src_wire` (via MUX) when the selector is active.
///
/// Returns a `Vec<String>` of new cell wire names — one per cell — extracted
/// from the mutable array after the loop completes.
///
/// **Same asymptotic cost** as the unrolled demux+MUX: roughly
/// `N * (aw + 1)` AND gates.  Generated code is O(1) instead of O(N).
///
/// Call this from an [`FheScheme::emit_oblivious_write`] override when the
/// target supports loops and compact code is preferred.
pub fn oblivious_write_loop<S: FheScheme, Q: Clone + Default>(
    scheme: &S,
    cells: &[String],
    addr_wires: &[&str],
    src_wire: &str,
    tag: &str,
    stmts: &mut Vec<IrStmt<Q>>,
    stmt_provs: &mut Vec<Q>,
) -> Vec<String> {
    let count = cells.len();
    let aw = effective_addr_width(count);

    // Pre-compute NOT of each address bit (outside the loop).
    let not_addr: Vec<String> = (0..aw)
        .map(|k| {
            let name = format!("{}_not{}", tag, k);
            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&name),
                ty: None,
                init: Some(scheme.emit_not::<Q>(var(addr_wires[k]))),
            });
            stmt_provs.push(Q::default());
            name
        })
        .collect();

    // Build mutable cells array: let mut {tag}_arr = [c0.clone(), ...];
    let arr_name = format!("{}_arr", tag);
    stmts.push(IrStmt::Let {
        pattern: IrPattern::ident(&arr_name).as_mut(),
        ty: None,
        init: Some(IrExpr::FixedArray(
            cells.iter().map(|c| clone_expr(var(c))).collect(),
        )),
    });
    stmt_provs.push(Q::default());

    // ── Loop body ────────────────────────────────────────────────────────
    let loop_var = format!("{}_i", tag);
    let mut body_stmts: Vec<IrStmt<Q>> = Vec::new();
    let mut body_provs: Vec<Q> = Vec::new();

    // Bit-match selection (same pattern as read loop).
    let mut bit_names: Vec<String> = Vec::new();
    for k in 0..aw {
        let bit_name = format!("{}_b{}", tag, k);
        let cond = IrExpr::Binary {
            op: SpecBinOp::Eq,
            left: Box::new(IrExpr::Binary {
                op: SpecBinOp::BitAnd,
                left: Box::new(IrExpr::Binary {
                    op: SpecBinOp::Shr,
                    left: Box::new(var(&loop_var)),
                    right: Box::new(IrExpr::Lit(IrLit::Int(k as i128))),
                }),
                right: Box::new(IrExpr::Lit(IrLit::Int(1))),
            }),
            right: Box::new(IrExpr::Lit(IrLit::Int(0))),
        };
        body_stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&bit_name),
            ty: None,
            init: Some(IrExpr::If {
                cond: Box::new(cond),
                then_branch: IrBlock {
                    stmts: vec![],
                    stmt_provs: vec![],
                    expr: Some(Box::new(clone_expr(var(&not_addr[k])))),
                },
                else_branch: Some(Box::new(IrExpr::Block(IrBlock {
                    stmts: vec![],
                    stmt_provs: vec![],
                    expr: Some(Box::new(clone_expr(var(addr_wires[k])))),
                }))),
            }),
        });
        body_provs.push(Q::default());
        bit_names.push(bit_name);
    }

    // Build one-hot selector (same chaining as read loop).
    let sel_name = if aw == 0 {
        let s = format!("{}_sel", tag);
        body_stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&s),
            ty: None,
            init: Some(scheme.emit_one::<Q>()),
        });
        body_provs.push(Q::default());
        s
    } else if aw == 1 {
        bit_names[0].clone()
    } else {
        let mut acc = bit_names[0].clone();
        for k in 1..aw {
            let and_name = format!("{}_sel{}", tag, k);
            body_stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&and_name),
                ty: None,
                init: Some(scheme.emit_and::<Q>(var(&acc), var(&bit_names[k]), 0)),
            });
            body_provs.push(Q::default());
            acc = and_name;
        }
        acc
    };

    // MUX(sel, old, src) = sel · (old ⊕ src) ⊕ old
    // MUX(sel, old, src): sel=0 → old, sel=1 → src
    // Clone old from the array, then cmux.
    let old_name = format!("{}_old", tag);
    body_stmts.push(IrStmt::Let {
        pattern: IrPattern::ident(&old_name),
        ty: None,
        init: Some(clone_expr(IrExpr::Index {
            base: Box::new(var(&arr_name)),
            index: Box::new(var(&loop_var)),
        })),
    });
    body_provs.push(Q::default());

    // cells_arr[i] = cmux(sel, src, old);
    body_stmts.push(IrStmt::Semi(IrExpr::Assign {
        left: Box::new(IrExpr::Index {
            base: Box::new(var(&arr_name)),
            index: Box::new(var(&loop_var)),
        }),
        right: Box::new(scheme.emit_cmux::<Q>(
            var(&sel_name),
            clone_expr(var(src_wire)),
            var(&old_name),
        )),
    }));
    body_provs.push(Q::default());

    // ── Emit the loop ────────────────────────────────────────────────────
    stmts.push(IrStmt::Expr(IrExpr::BoundedLoop {
        var: loop_var,
        start: Box::new(IrExpr::Cast {
            expr: Box::new(IrExpr::Lit(IrLit::Int(0))),
            ty: Box::new(IrType::Primitive(PrimitiveType::Usize)),
        }),
        end: Box::new(IrExpr::Cast {
            expr: Box::new(IrExpr::Lit(IrLit::Int(count as i128))),
            ty: Box::new(IrType::Primitive(PrimitiveType::Usize)),
        }),
        inclusive: false,
        body: IrBlock {
            stmts: body_stmts,
            stmt_provs: body_provs,
            expr: None,
        },
    }));
    stmt_provs.push(Q::default());

    // Extract updated cells from the array into individual variables.
    let mut new_cells = Vec::with_capacity(count);
    for ci in 0..count {
        let new_name = format!("{}_c{}", tag, ci);
        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&new_name),
            ty: None,
            init: Some(clone_expr(IrExpr::Index {
                base: Box::new(var(&arr_name)),
                index: Box::new(IrExpr::Lit(IrLit::Int(ci as i128))),
            })),
        });
        stmt_provs.push(Q::default());
        new_cells.push(new_name);
    }

    new_cells
}

// ============================================================================
// Storage configuration
// ============================================================================

/// Maps `(StorageId.0, bit_width)` → cell count.
///
/// Tells the weaver how many cells each storage space contains so that it can
/// allocate per-cell wires and size the MUX/demux trees correctly.
pub type FheStorageSizes = BTreeMap<(u32, usize), usize>;

/// Configuration for storage support in the FHE weaver.
///
/// When storage is enabled, each `(StorageId, bit_width)` pair becomes a
/// `&mut [WireType]` parameter on the emitted function, and `StorageRead`/
/// `StorageWrite` stmts are lowered to oblivious MUX-tree reads and
/// demux+MUX writes using the scheme's binary gate primitives.
#[derive(Clone, Debug, Default)]
pub struct FheStorageConfig {
    /// Number of cells per `(StorageId.0, bit_width)` key.
    ///
    /// Missing keys are treated as 0 cells (reads produce zero wires,
    /// writes are no-ops).
    pub sizes: FheStorageSizes,
}

// ============================================================================
// Output type
// ============================================================================

/// The output of [`weave_fhe`], parameterised by which weaving path was taken.
pub enum FheOutput {
    /// Produced by the flat path (`cfg_capable == false`).
    /// The circuit was movfuscated and emitted using binary gate methods.
    Flat(IrModule<IrFunction>),
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
    storage: Option<&FheStorageConfig>,
) -> FheOutput {
    if scheme.cfg_capable() {
        FheOutput::Cfg(weave_fhe_cfg(blocks, types, scheme, name, linkage, storage))
    } else {
        FheOutput::Flat(weave_fhe_flat(blocks, types, scheme, name, linkage, storage))
    }
}

// ============================================================================
// Auto-derive storage configuration from BIR circuit
// ============================================================================

/// Derive storage configuration by inspecting `StorageRead`/`StorageWrite`
/// statements in a BIR circuit.
///
/// For each `(StorageId, bit_width)` pair found, the cell count is set to
/// `2^N` where `N` is the maximum address length (number of address bits)
/// seen across all accesses to that storage pair.  A zero-length address
/// vector (no address bits) yields 1 cell.
///
/// Returns an empty config when the circuit contains no storage operations.
pub fn derive_storage_config<P: Clone + Default>(circuit: &BIrBlocks<P>) -> FheStorageConfig {
    let mut max_addr_len: BTreeMap<(u32, usize), usize> = BTreeMap::new();
    for block in &circuit.blocks {
        for stmt in &block.stmts {
            match stmt {
                BIrStmt::StorageRead { storage, bit_width, addr } => {
                    let key = (storage.0, *bit_width);
                    let entry = max_addr_len.entry(key).or_insert(0);
                    *entry = (*entry).max(addr.len());
                }
                BIrStmt::StorageWrite { storage, bit_width, addr, .. } => {
                    let key = (storage.0, *bit_width);
                    let entry = max_addr_len.entry(key).or_insert(0);
                    *entry = (*entry).max(addr.len());
                }
                _ => {}
            }
        }
    }
    let sizes = max_addr_len
        .into_iter()
        .map(|(key, alen)| (key, 1_usize << alen))
        .collect();
    FheStorageConfig { sizes }
}

/// Derive storage configuration by inspecting `StorageRead`/`StorageWrite`
/// statements in an IR-level circuit.
///
/// Unlike [`derive_storage_config`] which operates on BIR (bit-level), this
/// scans `IRBlocks` and keys storage by `(StorageId.0, IRTypeId.0)`.
///
/// `cell_count_fn` maps each `(StorageId, IRTypeId)` to a cell count.
/// If `None`, uses `1` (single-cell) as a conservative default.
pub fn derive_ir_storage_config<P: Clone + Default>(
    blocks: &IRBlocks<P>,
    cell_count_fn: Option<&dyn Fn(StorageId, IRTypeId) -> usize>,
) -> FheStorageConfig {
    let mut seen: BTreeSet<(u32, usize)> = BTreeSet::new();
    for block in &blocks.blocks {
        for stmt in &block.stmts {
            match stmt {
                IRStmt::StorageRead { storage, ty, .. }
                | IRStmt::StorageWrite { storage, ty, .. } => {
                    seen.insert((storage.0, ty.0 as usize));
                }
                _ => {}
            }
        }
    }
    let sizes = seen
        .into_iter()
        .map(|(sid, tid)| {
            let count = cell_count_fn
                .map(|f| f(StorageId(sid), IRTypeId(tid as u32)))
                .unwrap_or(1);
            ((sid, tid), count)
        })
        .collect();
    FheStorageConfig { sizes }
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
    storage: Option<&FheStorageConfig>,
) -> IrModule<IrFunction> {
    let bir_blocks = lower_ir_to_boolar(blocks, types);
    let circuit = movfuscate_biir(&bir_blocks);
    assert!(
        circuit.is_circuit(),
        "weave_fhe_flat: circuit after movfuscation must satisfy is_circuit()"
    );
    // When no storage config is provided, auto-derive from the circuit.
    // This scans StorageRead/Write stmts and infers cell counts from address
    // bit widths, so callers don't need to supply FheStorageSizes manually.
    let derived;
    let effective_storage = match storage {
        Some(cfg) => cfg,
        None => {
            derived = derive_storage_config(&circuit);
            &derived
        }
    };
    weave_fhe_flat_bir(&circuit, scheme, name, linkage, &NoProvenance, Some(effective_storage))
}

// ============================================================================
// Oblivious storage context for flat FHE weaving
// ============================================================================

/// Tracks per-cell wire names and emits oblivious MUX-tree reads / demux writes
/// using a scheme's binary gate primitives.
struct FheStorageCtx {
    /// Current wire name for each cell: `(storage_id, bit_width, cell_index)` → wire name.
    cells: BTreeMap<(u32, usize, usize), String>,
    /// Cell count per `(storage_id, bit_width)` key.
    counts: BTreeMap<(u32, usize), usize>,
    /// Running counter for unique MUX/demux names.
    mux_counter: usize,
}

impl FheStorageCtx {
    fn new(sizes: &FheStorageSizes) -> Self {
        let mut counts = BTreeMap::new();
        for (&(sid, bw), &count) in sizes {
            if count > 0 {
                counts.insert((sid, bw), count);
            }
        }
        FheStorageCtx {
            cells: BTreeMap::new(),
            counts,
            mux_counter: 0,
        }
    }

    /// Initialise all storage cells from the function parameters.
    ///
    /// For each `(sid, bw)` with `count` cells, emits `let _sinit_{sid}_{bw}_{i} =
    /// storage_{sid}_{bw}[{i}].clone();` bindings and tracks the cell names.
    fn init_cells<Q: Clone + Default>(&mut self, stmts: &mut Vec<IrStmt<Q>>, stmt_provs: &mut Vec<Q>) {
        for (&(sid, bw), &count) in &self.counts {
            let param_name = format!("storage_{}_{}", sid, bw);
            for ci in 0..count {
                let name = format!("_sinit_{}_{}_{}", sid, bw, ci);
                // storage_param[ci].clone()
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&name),
                    ty: None,
                    init: Some(clone_expr(IrExpr::Index {
                        base: Box::new(var(&param_name)),
                        index: Box::new(IrExpr::Lit(IrLit::Int(ci as i128))),
                    })),
                });
                stmt_provs.push(Q::default());
                self.cells.insert((sid, bw, ci), name);
            }
        }
    }

    /// Write-back all storage cells to the parameter slices at the end.
    fn writeback_cells<Q: Clone + Default>(&self, stmts: &mut Vec<IrStmt<Q>>, stmt_provs: &mut Vec<Q>) {
        for (&(sid, bw), &count) in &self.counts {
            let param_name = format!("storage_{}_{}", sid, bw);
            for ci in 0..count {
                let cell_name = &self.cells[&(sid, bw, ci)];
                // storage_param[ci] = cell.clone();
                stmts.push(IrStmt::Expr(IrExpr::Assign {
                    left: Box::new(IrExpr::Index {
                        base: Box::new(var(&param_name)),
                        index: Box::new(IrExpr::Lit(IrLit::Int(ci as i128))),
                    }),
                    right: Box::new(clone_expr(var(cell_name))),
                }));
                stmt_provs.push(Q::default());
            }
        }
    }

    /// Oblivious read — delegates to [`FheScheme::emit_oblivious_read`].
    ///
    /// Handles edge cases (0 cells → zero, 1 cell → clone) before dispatching
    /// to the scheme's oblivious read implementation for ≥ 2 cells.
    fn emit_read<S: FheScheme, Q: Clone + Default>(
        &mut self,
        out_name: &str,
        storage_id: u32,
        bit_width: usize,
        addr_wires: &[&str],
        scheme: &S,
        stmts: &mut Vec<IrStmt<Q>>,
        stmt_provs: &mut Vec<Q>,
        _var_names: &mut BTreeMap<u32, String>,
    ) -> String {
        let count = self.counts.get(&(storage_id, bit_width)).copied().unwrap_or(0);
        if count == 0 {
            // No cells — return zero.
            let z = scheme.emit_zero::<Q>();
            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(out_name),
                ty: None,
                init: Some(z),
            });
            stmt_provs.push(Q::default());
            return String::from(out_name);
        }

        let cells: Vec<String> = (0..count)
            .map(|ci| self.cells[&(storage_id, bit_width, ci)].clone())
            .collect();

        // Simple 1-cell case: just clone the cell.
        if count == 1 {
            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(out_name),
                ty: None,
                init: Some(clone_expr(var(&cells[0]))),
            });
            stmt_provs.push(Q::default());
            return String::from(out_name);
        }

        // Delegate to the scheme's oblivious read (default: MUX tree).
        let tag = format!("_sr_{}", out_name);
        let result = scheme.emit_oblivious_read(
            &cells, addr_wires, &tag, stmts, stmt_provs,
        );
        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(out_name),
            ty: None,
            init: Some(clone_expr(var(&result))),
        });
        stmt_provs.push(Q::default());
        String::from(out_name)
    }

    /// Oblivious write — delegates to [`FheScheme::emit_oblivious_write`].
    ///
    /// Handles edge cases (0 cells → no-op, 1 cell → unconditional write)
    /// before dispatching to the scheme's oblivious write for ≥ 2 cells.
    fn emit_write<S: FheScheme, Q: Clone + Default>(
        &mut self,
        storage_id: u32,
        bit_width: usize,
        src_wire: &str,
        addr_wires: &[&str],
        scheme: &S,
        stmts: &mut Vec<IrStmt<Q>>,
        stmt_provs: &mut Vec<Q>,
    ) {
        let count = self.counts.get(&(storage_id, bit_width)).copied().unwrap_or(0);
        if count == 0 { return; }
        if count == 1 {
            // Only one cell — unconditional write.
            let new_name = format!("_sw_{}_{}", storage_id, self.mux_counter);
            self.mux_counter += 1;
            stmts.push(IrStmt::Let {
                pattern: IrPattern::ident(&new_name),
                ty: None,
                init: Some(clone_expr(var(src_wire))),
            });
            stmt_provs.push(Q::default());
            self.cells.insert((storage_id, bit_width, 0), new_name);
            return;
        }

        let cells: Vec<String> = (0..count)
            .map(|ci| self.cells[&(storage_id, bit_width, ci)].clone())
            .collect();

        let tag = format!("_sdm_{}_{}", storage_id, self.mux_counter);
        self.mux_counter += 1;

        // Delegate to the scheme's oblivious write (default: demux + MUX).
        let new_cells = scheme.emit_oblivious_write(
            &cells, addr_wires, src_wire, &tag, stmts, stmt_provs,
        );

        // Update tracked cell names.
        for (ci, new_name) in new_cells.into_iter().enumerate() {
            self.cells.insert((storage_id, bit_width, ci), new_name);
        }
    }
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
/// # Storage
///
/// When `storage` is `Some`, `StorageRead`/`StorageWrite` stmts in the circuit
/// are lowered to oblivious MUX-tree reads and demux+MUX writes using the
/// scheme's binary gates.  Each `(StorageId, bit_width)` pair with at least
/// one cell in [`FheStorageConfig::sizes`] becomes a `&mut [WireType]`
/// parameter on the emitted function.
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
    storage: Option<&FheStorageConfig>,
) -> IrModule<IrFunction<H::Output>, H::Output>
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

    let block = &circuit.blocks[0];
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

    // ---- Storage parameter setup -------------------------------------------
    // For each (StorageId, bit_width) with cells > 0, add a &mut [WireType] param.
    let empty_config = FheStorageConfig::default();
    let stor_cfg = storage.unwrap_or(&empty_config);
    // Sorted keys for deterministic parameter order.
    let stor_keys: Vec<(u32, usize)> = stor_cfg.sizes.keys().copied().collect();
    for &(sid, bw) in &stor_keys {
        let count = stor_cfg.sizes[&(sid, bw)];
        if count == 0 { continue; }
        params.push(IrParam {
            name: format!("storage_{}_{}", sid, bw),
            ty: IrType::Reference {
                mutable: true,
                elem: Box::new(IrType::Array { kind: ArrayKind::Slice, elem: Box::new(wire_ty.clone()), len: ArrayLength::Const(0) }),
            },
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

    // Initialize storage cells from parameters.
    let mut stor_ctx = FheStorageCtx::new(&stor_cfg.sizes);
    stor_ctx.init_cells::<H::Output>(&mut stmts, &mut stmt_provs);

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

            BIrStmt::Or(a, b) => {
                scheme.emit_or(
                    var(var_names[&a.0].as_str()),
                    var(var_names[&b.0].as_str()),
                )
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

            BIrStmt::StorageRead { storage, bit_width, addr } => {
                let addr_names: Vec<String> = addr
                    .iter()
                    .map(|v| var_names
                        .get(&v.0)
                        .cloned()
                        .unwrap_or_else(|| format!("wire_{}", v.0)))
                    .collect();
                let addr_refs: Vec<&str> = addr_names.iter().map(|s| s.as_str()).collect();
                stor_ctx.emit_read::<S, H::Output>(
                    &let_name,
                    storage.0,
                    *bit_width,
                    &addr_refs,
                    scheme,
                    &mut stmts,
                    &mut stmt_provs,
                    &mut var_names,
                );
                var_names.insert(result_id.0, let_name);
                continue; // stmts already emitted by emit_read
            }

            BIrStmt::StorageWrite { storage, src, bit_width, addr } => {
                let src_name = var_names
                    .get(&src.0)
                    .cloned()
                    .unwrap_or_else(|| format!("wire_{}", src.0));
                let addr_names: Vec<String> = addr
                    .iter()
                    .map(|v| var_names
                        .get(&v.0)
                        .cloned()
                        .unwrap_or_else(|| format!("wire_{}", v.0)))
                    .collect();
                let addr_refs: Vec<&str> = addr_names.iter().map(|s| s.as_str()).collect();
                stor_ctx.emit_write::<S, H::Output>(
                    storage.0,
                    *bit_width,
                    &src_name,
                    &addr_refs,
                    scheme,
                    &mut stmts,
                    &mut stmt_provs,
                );
                // StorageWrite produces a dummy zero.
                let z: IrExpr<H::Output> = scheme.emit_zero();
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&let_name),
                    ty: None,
                    init: Some(z),
                });
                stmt_provs.push(q);
                var_names.insert(result_id.0, let_name);
                continue;
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

    // Write back storage cells to the parameter slices.
    stor_ctx.writeback_cells::<H::Output>(&mut stmts, &mut stmt_provs);

    let (ret_expr, ret_type) = build_return(block, &var_names, wire_ty);

    let fn_name = format!("{}_{}", name, scheme.fn_name_suffix());
    let func = IrFunction {
        name: fn_name.clone(),
        module_path: vec![],
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
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],

        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// CFG path implementation
// ============================================================================

// ── Type helpers ─────────────────────────────────────────────────────────────

/// Return the total bit-width of a type.
///
/// Primitive types return their natural width.  `Vec(n, elem)` returns
/// `n * elem_width`.  `Tuple(fields)` returns the sum of field widths.
/// Other forms (Block, Func) are unsupported.
fn ir_type_bit_width(ty_id: IRTypeId, types: &IRTypes) -> usize {
    match &types.0[ty_id.0 as usize] {
        IRType::Primitive(PrimType::Bit)    => 1,
        IRType::Primitive(PrimType::_8)
        | IRType::Primitive(PrimType::AES8) => 8,
        IRType::Primitive(PrimType::_16)    => 16,
        IRType::Primitive(PrimType::_32)    => 32,
        IRType::Primitive(PrimType::_64)
        | IRType::Primitive(PrimType::Galois64) => 64,
        IRType::Primitive(PrimType::_128)   => 128,
        IRType::Primitive(PrimType::_256)   => 256,
        IRType::Vec(n, elem_ty) => n * ir_type_bit_width(*elem_ty, types),
        IRType::Tuple(fields) => fields.iter().map(|f| ir_type_bit_width(*f, types)).sum(),
        other => panic!("ir_type_bit_width: unsupported IR type {:?}", other),
    }
}

/// Return the bit-width of SSA variable `vid`, defaulting to 1 if unknown.
fn var_bit_width(vid: IRVarId, type_map: &BTreeMap<u32, IRTypeId>, types: &IRTypes) -> usize {
    if let Some(&ty_id) = type_map.get(&vid.0) {
        ir_type_bit_width(ty_id, types)
    } else {
        1
    }
}

/// Extract the output [`IRTypeId`] from a statement (best-effort).
///
/// Returns `None` for stmts whose output type cannot be determined without
/// external context (e.g. `StorageWrite`, `Poly`).
fn ir_stmt_output_ty(stmt: &IRStmt) -> Option<IRTypeId> {
    use volar_ir::ir::Stmt;
    match stmt {
        Stmt::Const(_, ty)           => Some(*ty),
        Stmt::Transmute { dst_ty, .. } => Some(*dst_ty),
        Stmt::Rol { ty, .. }
        | Stmt::Ror { ty, .. }
        | Stmt::Merge { ty, .. }
        | Stmt::Splat { ty, .. }
        | Stmt::Shuffle { ty, .. }   => Some(*ty),
        Stmt::StorageRead { ty, .. } => Some(*ty),
        Stmt::OracleCall { result_ty, .. }
        | Stmt::ActionCall { result_ty, .. } => Some(*result_ty),
        Stmt::OracleOutput { ty, .. }
        | Stmt::ActionOutput { ty, .. } => Some(*ty),
        Stmt::Rng { ty, .. }         => Some(*ty),
        // Poly output type is now carried in the ty field.
        Stmt::Poly { ty, .. } => Some(*ty),
        // StorageWrite result is a dummy zero (no typed output).
        Stmt::StorageWrite { .. } => None,
    }
}

// ============================================================================
// CFG publicity pre-analysis
// ============================================================================

/// Update `public_set` and `action_output_public` for a single `IRStmt`.
///
/// This is the single source of truth for publicness propagation rules.  It is
/// called by both the pre-analysis pass ([`analyze_cfg_publicity`]) and the
/// main codegen loop in [`weave_fhe_cfg`].
fn track_stmt_publicness<S: FheScheme>(
    ir_stmt: &IRStmt,
    result_ir_vid: u32,
    public_set: &mut PublicSet,
    action_output_public: &mut BTreeMap<u32, Vec<bool>>,
    scheme: &S,
) {
    use volar_ir::ir::Stmt;
    match ir_stmt {
        Stmt::Const(..) => {
            public_set.mark_public(IRVarId(result_ir_vid));
        }
        Stmt::Transmute { src, .. } => {
            public_set.propagate_if_all_public(&[*src], IRVarId(result_ir_vid));
        }
        Stmt::Rol { src, .. } | Stmt::Ror { src, .. } | Stmt::Splat { src, .. } => {
            public_set.propagate_if_all_public(&[*src], IRVarId(result_ir_vid));
        }
        Stmt::Merge { parts, .. } => {
            public_set.propagate_if_all_public(parts, IRVarId(result_ir_vid));
        }
        Stmt::Shuffle { result_bits, .. } => {
            let vars: Vec<IRVarId> = result_bits.iter().map(|(_, v)| *v).collect();
            public_set.propagate_if_all_public(&vars, IRVarId(result_ir_vid));
        }
        Stmt::Poly { coeffs, .. } => {
            let vars: Vec<IRVarId> = coeffs
                .keys()
                .flat_map(|m| m.iter().copied())
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect();
            if vars.is_empty() {
                public_set.mark_public(IRVarId(result_ir_vid));
            } else {
                public_set.propagate_if_all_public(&vars, IRVarId(result_ir_vid));
            }
        }
        Stmt::ActionCall { name, .. } => {
            if let Some(cfg) = scheme.action_config(name) {
                if cfg.all_outputs_public() {
                    public_set.mark_public(IRVarId(result_ir_vid));
                }
                action_output_public.insert(result_ir_vid, cfg.output_public.clone());
            }
        }
        Stmt::ActionOutput { call, idx, .. } => {
            if let Some(out_pub) = action_output_public.get(&call.0) {
                // We have per-output config for this call.
                let is_pub = out_pub.is_empty() || out_pub.get(*idx).copied().unwrap_or(false);
                if is_pub {
                    public_set.mark_public(IRVarId(result_ir_vid));
                }
            } else {
                // Legacy: propagate publicness from the call result.
                public_set.propagate_if_all_public(&[*call], IRVarId(result_ir_vid));
            }
        }
        _ => {}
    }
}

/// Pre-analysis result: which block params are public, and the full per-block
/// `PublicSet`s (needed for predecessor checking).
struct CfgPublicityAnalysis {
    /// `block_param_public[bidx][pidx]` — true if block `bidx`'s param at
    /// position `pidx` is always public (every predecessor passes a public var).
    block_param_public: Vec<Vec<bool>>,
}

/// Analyse publicness across all blocks to determine which block parameters
/// can be typed as `bool` rather than the scheme's encrypted wire type.
///
/// A block parameter is public iff *every* predecessor block that jumps to it
/// passes a variable that is itself public at that point in the CFG.
///
/// **Limitation**: back-edges (loops) are treated conservatively — a parameter
/// reachable via a back-edge from an unprocessed block is assumed encrypted.
/// This is correct but may miss some public parameters in loops.
fn analyze_cfg_publicity<S: FheScheme>(
    blocks: &IRBlocks,
    scheme: &S,
) -> CfgPublicityAnalysis {
    let n = blocks.blocks.len();

    // Build predecessor info: pred_info[tgt][param_idx] = Vec<(src_block, src_var)>
    let mut pred_info: Vec<Vec<Vec<(usize, IRVarId)>>> = vec![vec![]; n];

    let add_pred = |pred_info: &mut Vec<Vec<Vec<(usize, IRVarId)>>>,
                    bidx: usize,
                    tgt: &IRBlockTargetId,
                    args: &[IRVarId]| {
        if let IRBlockTargetId::Block(bid) = tgt {
            let tgt_idx = bid.0 as usize;
            if tgt_idx < pred_info.len() {
                while pred_info[tgt_idx].len() < args.len() {
                    pred_info[tgt_idx].push(vec![]);
                }
                for (pidx, &arg) in args.iter().enumerate() {
                    pred_info[tgt_idx][pidx].push((bidx, arg));
                }
            }
        }
    };

    for (bidx, ir_block) in blocks.blocks.iter().enumerate() {
        match &ir_block.terminator {
            IRTerminator::Jmp { func, args } => {
                add_pred(&mut pred_info, bidx, func, args);
            }
            IRTerminator::JumpCond {
                true_block, true_args,
                false_block, false_args, ..
            } => {
                add_pred(&mut pred_info, bidx, true_block, true_args);
                add_pred(&mut pred_info, bidx, false_block, false_args);
            }
            IRTerminator::JumpTable { .. } => {}
        }
    }

    // Forward pass: compute publicness for each block in order.
    let mut per_block_public_sets: Vec<PublicSet> = Vec::with_capacity(n);
    let mut block_param_public: Vec<Vec<bool>> = Vec::with_capacity(n);

    for (bidx, ir_block) in blocks.blocks.iter().enumerate() {
        // Block 0's params are function inputs — always encrypted.
        let bpp: Vec<bool> = if bidx == 0 {
            vec![false; ir_block.params.len()]
        } else {
            (0..ir_block.params.len())
                .map(|pidx| {
                    let preds = match pred_info[bidx].get(pidx) {
                        Some(p) => p,
                        None => return false,
                    };
                    // Public iff ALL predecessors that have been processed
                    // (index < bidx) pass a public var.  Unprocessed predecessors
                    // (back edges) are treated as encrypted.
                    !preds.is_empty()
                        && preds.iter().all(|(src, var_id)| {
                            per_block_public_sets
                                .get(*src)
                                .map(|ps| ps.is_public(*var_id))
                                .unwrap_or(false)
                        })
                })
                .collect()
        };

        // Initialise public_set with the public params.
        let mut public_set = PublicSet::new();
        for (pidx, &is_pub) in bpp.iter().enumerate() {
            if is_pub {
                public_set.mark_public(IRVarId(pidx as u32));
            }
        }

        // Track publicness across all stmts.
        let num_params = ir_block.params.len() as u32;
        let mut action_output_public: BTreeMap<u32, Vec<bool>> = BTreeMap::new();
        for (stmt_idx, ir_stmt) in ir_block.stmts.iter().enumerate() {
            let result_ir_vid = num_params + stmt_idx as u32;
            track_stmt_publicness(ir_stmt, result_ir_vid, &mut public_set, &mut action_output_public, scheme);
        }

        block_param_public.push(bpp);
        per_block_public_sets.push(public_set);
    }

    CfgPublicityAnalysis { block_param_public }
}

/// Compute the emitted return type for a CFG function.
///
/// Scans all blocks for a `Return` terminator and derives the output type
/// from the returned variable types.  All return values are assumed to be
/// lifted to the wire type (public values are promoted before returning).
///
/// Returns `None` (→ `()`) if no `Return` terminator is found.
fn cfg_return_type<S: FheScheme>(blocks: &IRBlocks, types: &IRTypes, scheme: &S) -> Option<IrType> {
    use volar_ir::ir::IRTerminator;
    /// Look up the `IRTypeId` of a variable by its index within a block.
    fn block_var_ty(block: &volar_ir::ir::IRBlock, var_id: IRVarId) -> Option<IRTypeId> {
        let idx = var_id.0 as usize;
        if idx < block.params.len() {
            return Some(block.params[idx]);
        }
        let stmt_idx = idx - block.params.len();
        block.stmts.get(stmt_idx).and_then(ir_stmt_output_ty)
    }

    for block in &blocks.blocks {
        let ret_args = match &block.terminator {
            IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => args,
            _ => continue,
        };
        if ret_args.is_empty() {
            return Some(IrType::Tuple(vec![]));
        }
        if ret_args.len() == 1 {
            let ty_id = block_var_ty(block, ret_args[0])?;
            return Some(scheme.wire_type_for_ir(ty_id, types));
        }
        let tys: Vec<IrType> = ret_args
            .iter()
            .filter_map(|&v| block_var_ty(block, v).map(|t| scheme.wire_type_for_ir(t, types)))
            .collect();
        return Some(IrType::Tuple(tys));
    }
    None
}

fn weave_fhe_cfg<S: FheScheme>(
    blocks: &IRBlocks,
    types: &IRTypes,
    scheme: &S,
    name: &str,
    linkage: Option<&LinkageSystem>,
    storage: Option<&FheStorageConfig>,
) -> IrCfgModule {
    let wire_ty = scheme.wire_type();
    let bool_ty = IrType::Primitive(PrimitiveType::Bool);

    // Pre-analysis: determine which block params are public (cleartext bool).
    let analysis = analyze_cfg_publicity(blocks, scheme);

    let mut cfg_blocks: Vec<IrCfgBlock> = Vec::new();
    // Track whether any storage access uses a public address (bool-array),
    // requiring a `bools_to_usize` helper function in the output module.
    let mut needs_bools_to_usize = false;

    for (bidx, ir_block) in blocks.blocks.iter().enumerate() {
        // Block parameters.
        // Block 0's params are the function params (no block-param slot needed).
        // Other blocks carry their params via IrCfgBlock::params.
        // A block param is typed `bool` if the pre-analysis determined it is always
        // public (every predecessor passes a cleartext value for it).
        let block_params: Vec<IrParam> = ir_block
            .params
            .iter()
            .enumerate()
            .map(|(pidx, ty_id)| {
                let is_pub = analysis.block_param_public
                    .get(bidx)
                    .and_then(|v| v.get(pidx))
                    .copied()
                    .unwrap_or(false);
                IrParam {
                    name: format!("blk{}_p{}", bidx, pidx),
                    ty: if is_pub {
                        scheme.public_type_for_ir(*ty_id, types)
                    } else {
                        scheme.wire_type_for_ir(*ty_id, types)
                    },
                }
            })
            .collect();

        // Variable name map for this block.
        let num_params = ir_block.params.len() as u32;
        let mut var_map: BTreeMap<u32, String> = BTreeMap::new();
        // Type map: IRVarId.0 → IRTypeId, built incrementally.
        let mut type_map: BTreeMap<u32, IRTypeId> = BTreeMap::new();
        // Tracks which variables are known-cleartext (public).
        let mut public_set: PublicSet = PublicSet::new();

        // Params: block 0 uses function param names; others use blk-param names.
        if bidx == 0 {
            for i in 0..num_params {
                var_map.insert(i, format!("input_{}", i));
            }
        } else {
            // Seed public_set with params that the pre-analysis determined are public.
            if let Some(bpp) = analysis.block_param_public.get(bidx) {
                for (pidx, &is_pub) in bpp.iter().enumerate() {
                    if is_pub {
                        public_set.mark_public(IRVarId(pidx as u32));
                    }
                }
            }
            for (pidx, p) in block_params.iter().enumerate() {
                var_map.insert(pidx as u32, p.name.clone());
            }
        }
        // Record param types.
        for (pidx, &ty_id) in ir_block.params.iter().enumerate() {
            type_map.insert(pidx as u32, ty_id);
        }

        let mut stmts: Vec<IrStmt> = Vec::new();
        let mut stmt_provs: Vec<()> = Vec::new();
        // Tracks per-output publicness for ActionCall results in this block.
        let mut action_output_public: BTreeMap<u32, Vec<bool>> = BTreeMap::new();

        for (stmt_idx, ir_stmt) in ir_block.stmts.iter().enumerate() {
            let result_ir_vid = num_params + stmt_idx as u32;
            let let_name = format!("var_{}", result_ir_vid);

            // Track output type for this stmt.
            if let Some(ty_id) = ir_stmt_output_ty(ir_stmt) {
                type_map.insert(result_ir_vid, ty_id);
            }
            // Track publicness via the shared helper (single source of truth).
            track_stmt_publicness(ir_stmt, result_ir_vid, &mut public_set, &mut action_output_public, scheme);

            // Handle storage stmts generically (array-indexed parameter access).
            // When the address variable is public (e.g. ORAM leaf from an action),
            // it is typed as `[bool; N]` — we wrap it in `bools_to_usize(&addr)`
            // so it can index a Rust slice.
            match ir_stmt {
                IRStmt::StorageRead { storage, ty, addr } => {
                    let addr_name = var_map
                        .get(&addr.0)
                        .cloned()
                        .unwrap_or_else(|| format!("var_{}", addr.0));
                    let param_name = format!("storage_{}_{}", storage.0, ty.0);
                    let index_expr = if public_set.is_public(*addr) {
                        needs_bools_to_usize = true;
                        IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["bools_to_usize".into()],
                                type_args: vec![],
                            }),
                            args: vec![ref_expr(var(&addr_name))],
                        }
                    } else {
                        var(&addr_name)
                    };
                    // let var_N = storage_S_T[addr].clone();
                    stmts.push(IrStmt::Let {
                        pattern: IrPattern::ident(&let_name),
                        ty: None,
                        init: Some(clone_expr(IrExpr::Index {
                            base: Box::new(var(&param_name)),
                            index: Box::new(index_expr),
                        })),
                    });
                    stmt_provs.push(());
                    var_map.insert(result_ir_vid, let_name);
                    continue;
                }
                IRStmt::StorageWrite { storage, src, ty, addr } => {
                    let src_name = var_map
                        .get(&src.0)
                        .cloned()
                        .unwrap_or_else(|| format!("var_{}", src.0));
                    let addr_name = var_map
                        .get(&addr.0)
                        .cloned()
                        .unwrap_or_else(|| format!("var_{}", addr.0));
                    let param_name = format!("storage_{}_{}", storage.0, ty.0);
                    let index_expr = if public_set.is_public(*addr) {
                        needs_bools_to_usize = true;
                        IrExpr::Call {
                            func: Box::new(IrExpr::Path {
                                segments: vec!["bools_to_usize".into()],
                                type_args: vec![],
                            }),
                            args: vec![ref_expr(var(&addr_name))],
                        }
                    } else {
                        var(&addr_name)
                    };
                    // storage_S_T[addr] = src.clone();
                    stmts.push(IrStmt::Semi(IrExpr::Assign {
                        left: Box::new(IrExpr::Index {
                            base: Box::new(var(&param_name)),
                            index: Box::new(index_expr),
                        }),
                        right: Box::new(clone_expr(var(&src_name))),
                    }));
                    stmt_provs.push(());
                    // Dummy zero result for the write.
                    stmts.push(IrStmt::Let {
                        pattern: IrPattern::ident(&let_name),
                        ty: None,
                        init: Some(scheme.emit_zero()),
                    });
                    stmt_provs.push(());
                    var_map.insert(result_ir_vid, let_name);
                    continue;
                }
                _ => {}
            }

            let init_expr = scheme
                .emit_ir_stmt(ir_stmt, &var_map, &type_map, types, &public_set)
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

        let (extra_stmts, terminator) = map_ir_terminator(
            &ir_block.terminator,
            &var_map,
            &type_map,
            types,
            &public_set,
            scheme,
            &wire_ty,
            bidx,
            &analysis.block_param_public,
        );
        for s in extra_stmts {
            stmts.push(s);
            stmt_provs.push(());
        }

        cfg_blocks.push(IrCfgBlock {
            params: if bidx == 0 { vec![] } else { block_params },
            stmts,
            stmt_provs,
            terminator,
        });
    }

    let return_type = cfg_return_type(blocks, types, scheme);

    let mut func_params: Vec<IrParam> = scheme.extra_params();
    if let Some(entry) = blocks.blocks.first() {
        for (i, ty_id) in entry.params.iter().enumerate() {
            func_params.push(IrParam {
                name: format!("input_{}", i),
                ty: scheme.wire_type_for_ir(*ty_id, types),
            });
        }
    }

    // Add storage parameters.
    // In the CFG path, the key is (StorageId.0, IRTypeId.0 as usize).
    // Each storage cell holds the full wire type for that IR type, not a
    // single wire — e.g. an ORAM path of 4096 bits becomes
    // `[LweCiphertext<N_LWE>; 4096]` per cell.
    let empty_config = FheStorageConfig::default();
    let stor_cfg = storage.unwrap_or(&empty_config);
    for &(sid, bw) in stor_cfg.sizes.keys() {
        let count = stor_cfg.sizes[&(sid, bw)];
        if count == 0 { continue; }
        let elem_ty = scheme.wire_type_for_ir(IRTypeId(bw as u32), types);
        func_params.push(IrParam {
            name: format!("storage_{}_{}", sid, bw),
            ty: IrType::Reference {
                mutable: true,
                elem: Box::new(IrType::Array { kind: ArrayKind::Slice, elem: Box::new(elem_ty), len: ArrayLength::Const(0) }),
            },
        });
    }

    let fn_name = format!("{}_{}_cfg", name, scheme.fn_name_suffix());
    let func = IrCfgFunction {
        name: fn_name.clone(),
        generics: scheme.generics(),
        receiver: None,
        params: func_params,
        return_type,
        where_clause: vec![],
        external_kind: ExternalKind::Normal,
        body: IrCfgBody { blocks: cfg_blocks },
    };

    let mut module: IrCfgModule = IrModule {
        name: format!("weaved_{}", fn_name),
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

    // ── Emit action function stubs ────────────────────────────────────────
    //
    // For each ActionDecl in the IR, generate a function with:
    //   - `#[volar_action]` attribute (via ExternalKind::Action)
    //   - Public-guard convention parameters:
    //       guard: bool, fallback_0: (pub|wire)_ty, ..., arg_0: wire_ty, ...
    //     Fallback types match the return element type (public or wire per
    //     is_output_public).  Promotion from public to wire happens at the
    //     *call site*, not inside the action body.
    //   - Return type: tuple of (pub_ty | wire_ty) per output_public flags
    //   - Body: suppress unused args, return fallback values
    //
    // The fallback body returns the fallback parameter values directly,
    // providing type-correct default behavior. The `#[volar_action]` proc
    // macro can replace this with context-dependent dispatch in the future.
    for action_decl in &blocks.actions {
        let action_cfg = scheme.action_config(&action_decl.name);
        let mut params: Vec<IrParam> = Vec::new();

        // First param: guard (always bool).
        params.push(IrParam {
            name: "guard".into(),
            ty: IrType::Primitive(PrimitiveType::Bool),
        });

        // Fallback params: one per result, typed to match the return element.
        // Public outputs get public-typed fallbacks; wire outputs get wire-typed
        // fallbacks.  Promotion from public to wire happens at the *call site*,
        // not inside the action body.
        for (i, &res_ty) in action_decl.results.iter().enumerate() {
            let is_pub = action_cfg
                .as_ref()
                .map(|c| c.is_output_public(i))
                .unwrap_or(true);
            params.push(IrParam {
                name: format!("fallback_{}", i),
                ty: if is_pub {
                    scheme.public_type_for_ir(res_ty, types)
                } else {
                    scheme.wire_type_for_ir(res_ty, types)
                },
            });
        }

        // Argument params: one per input, always wire-typed.
        for (i, &param_ty) in action_decl.params.iter().enumerate() {
            params.push(IrParam {
                name: format!("arg_{}", i),
                ty: scheme.wire_type_for_ir(param_ty, types),
            });
        }

        // Return type: tuple of results.  Each element is public or wire
        // depending on FheActionConfig::is_output_public.
        let ret_elems: Vec<IrType> = action_decl.results.iter().enumerate().map(|(i, &res_ty)| {
            let is_pub = action_cfg
                .as_ref()
                .map(|c| c.is_output_public(i))
                .unwrap_or(true);
            if is_pub {
                scheme.public_type_for_ir(res_ty, types)
            } else {
                scheme.wire_type_for_ir(res_ty, types)
            }
        }).collect();

        let return_type = match ret_elems.len() {
            0 => None,
            // Always wrap in a tuple — ActionOutput uses `.0`, `.1`, etc.
            _ => Some(IrType::Tuple(ret_elems)),
        };

        // Build fallback body:
        //   let _ = (guard, arg_0, arg_1, ...);   // suppress unused warnings
        //   (fallback_0, fallback_1, ...)          // return fallback values
        let mut suppress_vars: Vec<IrExpr> = Vec::new();
        suppress_vars.push(IrExpr::Var("guard".into()));
        for i in 0..action_decl.params.len() {
            suppress_vars.push(IrExpr::Var(format!("arg_{}", i)));
        }

        let mut body_stmts: Vec<IrStmt> = Vec::new();
        let mut body_provs: Vec<()> = Vec::new();
        body_stmts.push(IrStmt::Let {
            pattern: IrPattern::Wild,
            ty: None,
            init: Some(IrExpr::Tuple(suppress_vars)),
        });
        body_provs.push(());

        let fallback_exprs: Vec<IrExpr> = (0..action_decl.results.len())
            .map(|i| IrExpr::Var(format!("fallback_{}", i)))
            .collect();
        let body_expr = if fallback_exprs.is_empty() {
            None
        } else {
            Some(Box::new(IrExpr::Tuple(fallback_exprs)))
        };

        let stub_fn = IrFunction {
            name: action_decl.name.clone(),
            module_path: vec![],
            generics: scheme.generics(),
            receiver: None,
            params,
            return_type,
            where_clause: vec![],
            body: IrBlock {
                stmts: body_stmts,
                stmt_provs: body_provs,
                expr: body_expr,
            },
            external_kind: ExternalKind::Action,
        };
        module.functions.push(IrAnyFunction::Flat(stub_fn));
    }

    // ── Emit bools_to_usize helper if any storage access uses a public address ──
    //
    // When the runtime linked spec is available (via linkage), the real
    // implementation is already in `functions` as a Flat entry; skip the stub.
    // Otherwise emit a stub with `unreachable!()` for compile-checking.
    if needs_bools_to_usize {
        let already_linked = module.functions.iter().any(|f| {
            matches!(f, IrAnyFunction::Flat(f) if f.name == "bools_to_usize")
        });
        if !already_linked {
            let helper = IrFunction {
                name: "bools_to_usize".into(),
                module_path: vec![],
                generics: vec![],
                receiver: None,
                params: vec![IrParam {
                    name: "bits".into(),
                    ty: IrType::Reference {
                        mutable: false,
                        elem: Box::new(IrType::Array {
                            kind: ArrayKind::Slice,
                            elem: Box::new(IrType::Primitive(PrimitiveType::Bool)),
                            len: ArrayLength::Const(0),
                        }),
                    },
                }],
                return_type: Some(IrType::Primitive(PrimitiveType::Usize)),
                where_clause: vec![],
                body: IrBlock {
                    stmts: vec![],
                    stmt_provs: vec![],
                    expr: Some(Box::new(IrExpr::Unreachable)),
                },
                external_kind: ExternalKind::Normal,
            };
            module.functions.push(IrAnyFunction::Flat(helper));
        }
    }

    // ── FHE helper type stubs ─────────────────────────────────────────────────
    // Add scheme-specific helper stubs (e.g. tfhe_trivial_encrypt, tfhe_cmux)
    // for LIR codegen return-type inference.  Tagged ExternalKind::TypeStub so
    // the Rust/TS printers skip them.
    module.functions.extend(scheme.helper_type_stubs().into_iter().map(IrAnyFunction::Flat));

    module
}

/// Map an [`IRTerminator`] to an [`IrCfgTerminator`], optionally prepending
/// CMUX statements for encrypted branch merges.
///
/// Returns `(extra_stmts, terminator)`.  `extra_stmts` must be appended to
/// the block's statement list before `terminator`.
///
/// # JumpCond handling
///
/// - **Public condition** (in `public_set`): emits `CondGoto` directly — the
///   condition is cleartext and a real Rust `if/else` is safe.
/// - **Encrypted condition, same target** (`true_block == false_block`): for
///   each argument position `i`, emits
///   `let __cmux_arg_{bidx}_{i} = tfhe_cmux(cond, true_arg_i, false_arg_i, bk);`
///   then `Goto(target, [__cmux_arg_{bidx}_0, ...])`.
/// - **Encrypted condition, different targets**: panics.  Oblivious control
///   flow across distinct basic blocks requires serialising both paths and is
///   not yet supported.
fn map_ir_terminator<S: FheScheme>(
    term: &IRTerminator,
    var_map: &BTreeMap<u32, String>,
    type_map: &BTreeMap<u32, IRTypeId>,
    types: &IRTypes,
    public_set: &PublicSet,
    scheme: &S,
    _wire_ty: &IrType,
    bidx: usize,
    block_param_public: &[Vec<bool>],
) -> (Vec<IrStmt>, IrCfgTerminator) {
    // Helper: resolve a variable name from var_map.
    let resolve = |id: &IRVarId| -> String {
        var_map
            .get(&id.0)
            .cloned()
            .unwrap_or_else(|| format!("var_{}", id.0))
    };

    // Helper: build a jump argument expression, applying promote_to_wire when a
    // source-public variable is passed to an encrypted target parameter.
    let jump_arg = |id: &IRVarId, target_bidx: usize, pidx: usize| -> IrExpr {
        let name = resolve(id);
        let src_public = public_set.is_public(*id);
        let tgt_public = block_param_public
            .get(target_bidx)
            .and_then(|v| v.get(pidx))
            .copied()
            .unwrap_or(false);
        if src_public && !tgt_public {
            // Target expects encrypted wire; promote the cleartext value.
            let width = var_bit_width(*id, type_map, types);
            scheme.promote_to_wire(var(&name), width)
        } else {
            var(&name)
        }
    };

    match term {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => {
            // Function return type is wire type (encrypted). Promote public vars.
            let output_exprs: Vec<IrExpr> = args
                .iter()
                .map(|id| {
                    let name = resolve(id);
                    if public_set.is_public(*id) {
                        let width = var_bit_width(*id, type_map, types);
                        scheme.promote_to_wire(var(&name), width)
                    } else {
                        var(&name)
                    }
                })
                .collect();
            let ret_expr = match output_exprs.len() {
                0 => IrExpr::Tuple(vec![]),
                1 => output_exprs.into_iter().next().unwrap(),
                _ => IrExpr::Tuple(output_exprs),
            };
            (vec![], IrCfgTerminator::Return(Some(ret_expr)))
        }
        IRTerminator::Jmp { func: IRBlockTargetId::Block(bid), args } => {
            let target_bidx = bid.0 as usize;
            let jump = IrCfgJump {
                target: target_bidx,
                args: args
                    .iter()
                    .enumerate()
                    .map(|(pidx, id)| jump_arg(id, target_bidx, pidx))
                    .collect(),
            };
            (vec![], IrCfgTerminator::Goto(jump))
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
            let cond_name = resolve(condition);

            // Public condition → direct Rust if/else via CondGoto.
            if public_set.is_public(*condition) {
                let map_jump = |bid: &IRBlockTargetId, args: &[IRVarId]| -> IrCfgJump {
                    let target = match bid {
                        IRBlockTargetId::Block(b) => b.0 as usize,
                        IRBlockTargetId::Return    => usize::MAX,
                        IRBlockTargetId::Dyn(_)   =>
                            panic!("weave_fhe_cfg: dynamic jump in CondJmp"),
                    };
                    IrCfgJump {
                        target,
                        args: args
                            .iter()
                            .enumerate()
                            .map(|(pidx, id)| jump_arg(id, target, pidx))
                            .collect(),
                    }
                };
                return (
                    vec![],
                    IrCfgTerminator::CondGoto {
                        cond: var(&cond_name),
                        then_: map_jump(true_block, true_args),
                        else_: map_jump(false_block, false_args),
                    },
                );
            }

            // Encrypted condition: only same-target phi-merge is supported.
            let true_target_id  = match true_block  {
                IRBlockTargetId::Block(b) => b.0,
                _ => u32::MAX,
            };
            let false_target_id = match false_block {
                IRBlockTargetId::Block(b) => b.0,
                _ => u32::MAX,
            };
            if true_target_id != false_target_id {
                panic!(
                    "weave_fhe_cfg block {bidx}: encrypted JumpCond with different targets \
                     (true→{true_target_id}, false→{false_target_id}) is not supported. \
                     Oblivious CFG across distinct basic blocks requires serialising both \
                     paths; use the flat (movfuscation) path or restructure the circuit."
                );
            }

            // Same target: merge args with CMUX.
            // For each arg position i: __cmux_arg_{bidx}_{i} = tfhe_cmux(cond, t_i, f_i, bk)
            assert_eq!(
                true_args.len(), false_args.len(),
                "weave_fhe_cfg block {bidx}: JumpCond same-target branches have different arg counts"
            );

            let mut extra_stmts: Vec<IrStmt> = Vec::new();
            let mut merged_args: Vec<IrExpr>  = Vec::new();

            for (i, (t_id, f_id)) in true_args.iter().zip(false_args.iter()).enumerate() {
                let t_name = var_map.get(&t_id.0).cloned()
                    .unwrap_or_else(|| format!("var_{}", t_id.0));
                let f_name = var_map.get(&f_id.0).cloned()
                    .unwrap_or_else(|| format!("var_{}", f_id.0));

                // Helper: produce an encrypted wire expr for a var, lifting public ones.
                // In the single-bit CMUX path this is always width=1.
                let lift_to_ct = |vid: &IRVarId, name: &str| -> IrExpr {
                    if public_set.is_public(*vid) {
                        scheme.promote_to_wire(var(name), 1)
                    } else {
                        clone_expr(var(name))
                    }
                };

                // Determine width of this argument; emit per-bit CMUX if > 1.
                let width = var_bit_width(*t_id, type_map, types);
                let merged_name = format!("__cmux_arg_{}_{}", bidx, i);

                if width == 1 {
                    // Single-bit CMUX.
                    let cmux_expr: IrExpr = IrExpr::Call {
                        func: Box::new(IrExpr::Path {
                            segments: vec!["tfhe_cmux".into()],
                            type_args: vec![],
                        }),
                        args: vec![
                            clone_expr(var(&cond_name)),
                            lift_to_ct(t_id, &t_name),
                            lift_to_ct(f_id, &f_name),
                            var("bk"),
                        ],
                    };
                    extra_stmts.push(IrStmt::Let {
                        pattern: IrPattern::ident(&merged_name),
                        ty: None,
                        init: Some(cmux_expr),
                    });
                } else {
                    // Multi-bit: element-wise CMUX → fixed array.
                    // Public multi-bit vars are [bool; N]; they can't be indexed as LweCiphertext.
                    // We only support encrypting single-bit public vars in the CMUX path.
                    if public_set.is_public(*t_id) || public_set.is_public(*f_id) {
                        panic!(
                            "weave_fhe_cfg block {bidx}: cannot CMUX-merge a multi-bit public \
                             variable (arg {i}). Multi-bit public vars cannot be trivially \
                             encrypted element-wise in the CMUX path. Restructure the circuit \
                             to avoid multi-bit public block arguments under encrypted conditions."
                        );
                    }
                    let bit_exprs: Vec<IrExpr> = (0..width)
                        .map(|bit| {
                            IrExpr::Call {
                                func: Box::new(IrExpr::Path {
                                    segments: vec!["tfhe_cmux".into()],
                                    type_args: vec![],
                                }),
                                args: vec![
                                    clone_expr(var(&cond_name)),
                                    clone_expr(IrExpr::Index {
                                        base:  Box::new(var(&t_name)),
                                        index: Box::new(IrExpr::Lit(IrLit::Int(bit as i128))),
                                    }),
                                    clone_expr(IrExpr::Index {
                                        base:  Box::new(var(&f_name)),
                                        index: Box::new(IrExpr::Lit(IrLit::Int(bit as i128))),
                                    }),
                                    var("bk"),
                                ],
                            }
                        })
                        .collect();
                    extra_stmts.push(IrStmt::Let {
                        pattern: IrPattern::ident(&merged_name),
                        ty: None,
                        init: Some(IrExpr::FixedArray(bit_exprs)),
                    });
                }

                merged_args.push(var(&merged_name));
            }

            let term = IrCfgTerminator::Goto(IrCfgJump {
                target: true_target_id as usize,
                args: merged_args,
            });
            (extra_stmts, term)
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
            const_ty: None,
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

    fn emit_or<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        // De Morgan: NOT(AND(NOT(a), NOT(b)))
        // GRAFHEN NOT and XOR are free, so only 1 AND gate cost.
        self.emit_not(self.emit_and(self.emit_not(a), self.emit_not(b), 0))
    }

    fn emit_cmux<Q: Clone + Default>(&self, sel: IrExpr<Q>, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        // GRAFHEN has free composable XOR, so use the efficient formula:
        // MUX(sel, a, b) = sel · (a ⊕ b) ⊕ b = XOR(AND(sel, XOR(a, b)), b)
        // Only 1 AND gate cost (XOR and NOT are free).
        let diff = self.emit_xor(a, clone_expr(b.clone()));
        let masked = self.emit_and(sel, diff, 0);
        self.emit_xor(masked, b)
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
// TFHE scheme
// ============================================================================

/// [`FheScheme`] implementation for TFHE (Torus Fully Homomorphic Encryption).
///
/// Generates code that calls the functions from `volar_spec::tfhe`:
/// - Free gates: `tfhe_xor`, `tfhe_not`
/// - AND gate: `tfhe_gate_bootstrapping_and` (full GINX blind rotation + key switching)
/// - CMUX: `tfhe_cmux` (used for encrypted branch merging in CFG path)
///
/// The generated function is generic over four const usize parameters:
/// `N_LWE`, `BIG_N`, `BS_ELL`, `KS_ELL` — corresponding to the TFHE parameters
/// in `volar_spec::tfhe::BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>`.
///
/// Wire type: `LweCiphertext<N_LWE>` (a single-bit LWE ciphertext).
/// Extra parameter: `bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>`.
///
/// # Paths
///
/// - [`TfheScheme::flat()`] — use the flat (movfuscation) path.  Suitable
///   for circuits without control flow.
/// - [`TfheScheme::cfg()`] — preserve the input `IRBlocks` CFG structure.
///   Encrypted branch conditions are merged with CMUX; public conditions use
///   direct `if/else`.
///
/// # Example generated signature (flat)
/// ```rust,ignore
/// fn my_circuit_tfhe<
///     const N_LWE: usize,
///     const BIG_N: usize,
///     const BS_ELL: usize,
///     const KS_ELL: usize,
/// >(
///     input: LweCiphertext<N_LWE>,
///     bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
/// ) -> LweCiphertext<N_LWE> { ... }
/// ```
pub struct TfheScheme {
    /// When `true`, use the CFG path (preserve `IRBlocks` structure).
    /// When `false`, use the flat movfuscation path.
    pub use_cfg: bool,
    /// Per-action configuration overrides.
    ///
    /// Takes precedence over the `_pub`-suffix heuristic.  Use
    /// [`TfheScheme::with_action_config`] to populate this.
    pub action_configs: BTreeMap<String, FheActionConfig>,
}

impl TfheScheme {
    /// Flat path: lower to a single-block boolean circuit (movfuscation).
    pub fn flat() -> Self {
        TfheScheme { use_cfg: false, action_configs: BTreeMap::new() }
    }

    /// CFG path: preserve the input `IRBlocks` control-flow structure.
    ///
    /// Encrypted branch conditions are merged with CMUX.
    /// Public (constant-propagated) conditions use direct `if/else`.
    pub fn cfg() -> Self {
        TfheScheme { use_cfg: true, action_configs: BTreeMap::new() }
    }

    /// Builder: register an action-specific configuration.
    ///
    /// This overrides the `_pub`-suffix heuristic for the named action.
    pub fn with_action_config(mut self, name: impl Into<String>, config: FheActionConfig) -> Self {
        self.action_configs.insert(name.into(), config);
        self
    }
}

impl FheScheme for TfheScheme {
    fn cfg_capable(&self) -> bool {
        self.use_cfg
    }

    fn wire_type(&self) -> IrType {
        IrType::Struct {
            kind: StructKind::Custom("LweCiphertext".into()),
            type_args: vec![IrType::TypeParam("N_LWE".into())],
        }
    }

    fn extra_params(&self) -> Vec<IrParam> {
        vec![IrParam {
            name: "bk".into(),
            ty: IrType::Reference {
                mutable: false,
                elem: Box::new(IrType::Struct {
                    kind: StructKind::Custom("BootstrappingKey".into()),
                    type_args: vec![
                        IrType::TypeParam("N_LWE".into()),
                        IrType::TypeParam("BIG_N".into()),
                        IrType::TypeParam("BS_ELL".into()),
                        IrType::TypeParam("KS_ELL".into()),
                    ],
                }),
            },
        }]
    }

    fn generics(&self) -> Vec<IrGenericParam> {
        vec![
            IrGenericParam {
                name: "N_LWE".into(),
                kind: IrGenericParamKind::Const,
                const_ty: None, // defaults to usize in printer
                bounds: vec![],
                default: None,
            },
            IrGenericParam {
                name: "BIG_N".into(),
                kind: IrGenericParamKind::Const,
                const_ty: None,
                bounds: vec![],
                default: None,
            },
            IrGenericParam {
                name: "BS_ELL".into(),
                kind: IrGenericParamKind::Const,
                const_ty: None,
                bounds: vec![],
                default: None,
            },
            IrGenericParam {
                name: "KS_ELL".into(),
                kind: IrGenericParamKind::Const,
                const_ty: None,
                bounds: vec![],
                default: None,
            },
        ]
    }

    fn fn_name_suffix(&self) -> &str {
        "tfhe"
    }

    fn action_config(&self, name: &str) -> Option<FheActionConfig> {
        // Explicit per-action override takes precedence.
        if let Some(cfg) = self.action_configs.get(name) {
            return Some(cfg.clone());
        }
        // Backward-compatible heuristic: `_pub` suffix → all-public.
        if name.ends_with("_pub") {
            return Some(FheActionConfig {
                output_public: vec![], // empty = all public
            });
        }
        None
    }

    fn promote_to_wire<Q: Clone + Default>(&self, expr: IrExpr<Q>, width: usize) -> IrExpr<Q> {
        // Lift a cleartext value to LweCiphertext<N_LWE>.
        let encrypt = |e: IrExpr<Q>| -> IrExpr<Q> {
            IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["tfhe_trivial_encrypt".into()],
                    type_args: vec![],
                }),
                args: vec![e],
            }
        };
        if width <= 1 {
            encrypt(expr)
        } else {
            // [bool; width] → [LweCiphertext; width]: encrypt each element.
            IrExpr::FixedArray(
                (0..width)
                    .map(|bit| {
                        encrypt(IrExpr::Index {
                            base: Box::new(expr.clone()),
                            index: Box::new(IrExpr::Lit(IrLit::Int(bit as i128))),
                        })
                    })
                    .collect(),
            )
        }
    }

    fn emit_zero<Q: Clone + Default>(&self) -> IrExpr<Q> {
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["tfhe_trivial_zero".into()],
                type_args: vec![IrType::TypeParam("N_LWE".into())],
            }),
            args: vec![],
        }
    }

    fn emit_one<Q: Clone + Default>(&self) -> IrExpr<Q> {
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["tfhe_trivial_one".into()],
                type_args: vec![IrType::TypeParam("N_LWE".into())],
            }),
            args: vec![],
        }
    }

    fn emit_xor<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        // TFHE XOR is non-composable ({0, Q4} encoding breaks chaining).
        // Decompose: XOR(a, b) = OR(AND(a, NOT(b)), AND(NOT(a), b))
        // = 2 AND + 1 OR + 2 free NOT = 3 bootstrapping rounds.
        //
        // Each input is used twice in the generated code, so the first use
        // gets a .clone() via clone_expr.
        let not_b = self.emit_not(clone_expr(b.clone()));
        let not_a = self.emit_not(clone_expr(a.clone()));
        let a_and_not_b = self.emit_and(a, not_b, 0);
        let not_a_and_b = self.emit_and(not_a, b, 0);
        self.emit_or(a_and_not_b, not_a_and_b)
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

    fn emit_or<Q: Clone + Default>(&self, a: IrExpr<Q>, b: IrExpr<Q>) -> IrExpr<Q> {
        IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["tfhe_gate_bootstrapping_or".into()],
                type_args: vec![],
            }),
            args: vec![a, b, var("bk")],
        }
    }

    // ── CFG path ─────────────────────────────────────────────────────────────

    fn emit_ir_stmt(
        &self,
        stmt: &IRStmt,
        var_map: &BTreeMap<u32, String>,
        type_map: &BTreeMap<u32, IRTypeId>,
        types: &IRTypes,
        public_set: &PublicSet,
    ) -> Option<IrExpr> {
        use volar_ir::ir::Stmt;

        // Helper: look up variable name.
        let vname = |vid: &IRVarId| -> String {
            var_map.get(&vid.0).cloned().unwrap_or_else(|| format!("var_{}", vid.0))
        };

        // Helper: emit tfhe_trivial_zero() or tfhe_trivial_one() based on bit `b`.
        let trivial = |bit: bool| -> IrExpr {
            IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec![if bit { "tfhe_trivial_one" } else { "tfhe_trivial_zero" }.into()],
                    type_args: vec![IrType::TypeParam("N_LWE".into())],
                }),
                args: vec![],
            }
        };

        // Helper: emit tfhe_not(a).
        let not1 = |a: IrExpr| -> IrExpr {
            IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["tfhe_not".into()],
                    type_args: vec![],
                }),
                args: vec![a],
            }
        };

        // Helper: emit tfhe_gate_bootstrapping_and(a, b, bk).
        let and2 = |a: IrExpr, b: IrExpr| -> IrExpr {
            IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["tfhe_gate_bootstrapping_and".into()],
                    type_args: vec![],
                }),
                args: vec![a, b, var("bk")],
            }
        };

        // Helper: emit tfhe_gate_bootstrapping_or(a, b, bk).
        let or2 = |a: IrExpr, b: IrExpr| -> IrExpr {
            IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["tfhe_gate_bootstrapping_or".into()],
                    type_args: vec![],
                }),
                args: vec![a, b, var("bk")],
            }
        };

        // Helper: composable XOR decomposition.
        // XOR(a, b) = OR(AND(a, NOT(b)), AND(NOT(a), b))
        // Each input is used twice, so the first use gets clone_expr.
        let xor2 = |a: IrExpr, b: IrExpr| -> IrExpr {
            let not_b = not1(clone_expr(b.clone()));
            let not_a = not1(clone_expr(a.clone()));
            let a_and_not_b = and2(a, not_b);
            let not_a_and_b = and2(not_a, b);
            or2(a_and_not_b, not_a_and_b)
        };

        // Helper: index into a multi-bit variable at position `bit`.
        let index_bit = |name: &str, bit: usize| -> IrExpr {
            IrExpr::Index {
                base:  Box::new(var(name)),
                index: Box::new(IrExpr::Lit(IrLit::Int(bit as i128))),
            }
        };

        match stmt {
            // ── Const ─────────────────────────────────────────────────────────
            // Emit cleartext bool literal(s).  Constants are always public;
            // they are promoted to LweCiphertext at use sites where needed.
            Stmt::Const(constant, ty_id) => {
                let width = ir_type_bit_width(*ty_id, types);
                if width == 1 {
                    let bit = (constant.lo & 1) != 0;
                    Some(IrExpr::Lit(IrLit::Bool(bit)))
                } else {
                    let elems: Vec<IrExpr> = (0..width)
                        .map(|i| {
                            let bit = if i < 128 {
                                (constant.lo >> i) & 1 != 0
                            } else if i < 256 {
                                (constant.hi >> (i - 128)) & 1 != 0
                            } else {
                                // Constant only has 256 bits of storage;
                                // bits beyond position 255 are always 0.
                                false
                            };
                            IrExpr::Lit(IrLit::Bool(bit))
                        })
                        .collect();
                    Some(IrExpr::FixedArray(elems))
                }
            }

            // ── Transmute ─────────────────────────────────────────────────────
            // Reinterpret bits without change — just clone the source binding.
            Stmt::Transmute { src, .. } => {
                let name = vname(src);
                Some(clone_expr(var(&name)))
            }

            // ── Poly ──────────────────────────────────────────────────────────
            // GF(2) multivariate polynomial.  Output is always single-bit.
            // Variables in monomials must be single-bit values.
            //
            // Three paths:
            //   1. All variables are public → emit bool arithmetic (BitXor/BitAnd).
            //   2. Mixed (some public, some encrypted) → lift public vars via
            //      tfhe_trivial_encrypt, then use TFHE gates.
            //   3. All encrypted → use TFHE gates directly (original path).
            Stmt::Poly { coeffs, constant, .. } => {
                let const_bit = (constant.lo & 1) != 0;

                // Determine if every variable in the polynomial is public.
                let all_public = coeffs
                    .keys()
                    .flat_map(|m| m.iter())
                    .all(|v| public_set.is_public(*v));

                if all_public {
                    // ── All-public: bool arithmetic ──────────────────────────
                    let mut acc: IrExpr = IrExpr::Lit(IrLit::Bool(const_bit));
                    for (monomial, &coeff) in coeffs.iter() {
                        if coeff == 0 { continue; }
                        let term: IrExpr = match monomial.as_slice() {
                            [] => IrExpr::Lit(IrLit::Bool(true)), // empty product = 1
                            [first, rest @ ..] => {
                                let mut t: IrExpr = clone_expr(var(&vname(first)));
                                for v in rest {
                                    t = IrExpr::Binary {
                                        op: SpecBinOp::BitAnd,
                                        left: Box::new(t),
                                        right: Box::new(clone_expr(var(&vname(v)))),
                                    };
                                }
                                t
                            }
                        };
                        acc = IrExpr::Binary {
                            op: SpecBinOp::BitXor,
                            left: Box::new(acc),
                            right: Box::new(term),
                        };
                    }
                    Some(acc)
                } else {
                    // ── Mixed or all-encrypted: TFHE gates ───────────────────
                    // Public variables are promoted via tfhe_trivial_encrypt.
                    let lift = |vid: &IRVarId| -> IrExpr {
                        let name = vname(vid);
                        if public_set.is_public(*vid) {
                            IrExpr::Call {
                                func: Box::new(IrExpr::Path {
                                    segments: vec!["tfhe_trivial_encrypt".into()],
                                    type_args: vec![],
                                }),
                                args: vec![var(&name)],
                            }
                        } else {
                            clone_expr(var(&name))
                        }
                    };

                    let mut acc: IrExpr = trivial(const_bit);
                    for (monomial, &coeff) in coeffs.iter() {
                        if coeff == 0 { continue; }
                        let term: IrExpr = match monomial.as_slice() {
                            [] => trivial(true), // empty product = 1
                            [first, rest @ ..] => {
                                let w = var_bit_width(*first, type_map, types);
                                if w != 1 {
                                    panic!(
                                        "emit_ir_stmt: Poly references a {}-bit variable {:?}; \
                                         only single-bit variables are supported in the CFG path Poly handler.",
                                        w, first
                                    );
                                }
                                let mut t: IrExpr = lift(first);
                                for v in rest {
                                    let wv = var_bit_width(*v, type_map, types);
                                    if wv != 1 {
                                        panic!(
                                            "emit_ir_stmt: Poly references a {}-bit variable {:?}; \
                                             only single-bit variables are supported.",
                                            wv, v
                                        );
                                    }
                                    t = and2(t, lift(v));
                                }
                                t
                            }
                        };
                        acc = xor2(acc, term);
                    }
                    Some(acc)
                }
            }

            // ── Rol / Ror ─────────────────────────────────────────────────────
            // Circular bit rotation.  Implemented as a permutation of array slots.
            Stmt::Rol { src, ty, n } => {
                let width = ir_type_bit_width(*ty, types);
                let n = n % width;
                let src_name = vname(src);
                if width == 1 {
                    // Single-bit: rotation is identity.
                    return Some(clone_expr(var(&src_name)));
                }
                let elems: Vec<IrExpr> = (0..width)
                    .map(|i| clone_expr(index_bit(&src_name, (i + width - n) % width)))
                    .collect();
                Some(IrExpr::FixedArray(elems))
            }
            Stmt::Ror { src, ty, n } => {
                let width = ir_type_bit_width(*ty, types);
                let n = n % width;
                let src_name = vname(src);
                if width == 1 {
                    return Some(clone_expr(var(&src_name)));
                }
                let elems: Vec<IrExpr> = (0..width)
                    .map(|i| clone_expr(index_bit(&src_name, (i + n) % width)))
                    .collect();
                Some(IrExpr::FixedArray(elems))
            }

            // ── Merge ─────────────────────────────────────────────────────────
            // Concatenate parts LSB-first.
            Stmt::Merge { parts, .. } => {
                let mut elems: Vec<IrExpr> = Vec::new();
                for part in parts {
                    let part_name = vname(part);
                    let part_width = var_bit_width(*part, type_map, types);
                    if part_width == 1 {
                        elems.push(clone_expr(var(&part_name)));
                    } else {
                        for bit in 0..part_width {
                            elems.push(clone_expr(index_bit(&part_name, bit)));
                        }
                    }
                }
                Some(IrExpr::FixedArray(elems))
            }

            // ── Splat ─────────────────────────────────────────────────────────
            // Broadcast a single bit across every position.
            Stmt::Splat { src, ty } => {
                let width = ir_type_bit_width(*ty, types);
                let src_name = vname(src);
                if width == 1 {
                    return Some(clone_expr(var(&src_name)));
                }
                let elems: Vec<IrExpr> = (0..width)
                    .map(|_| clone_expr(var(&src_name)))
                    .collect();
                Some(IrExpr::FixedArray(elems))
            }

            // ── Shuffle ───────────────────────────────────────────────────────
            // Arbitrary bit selection.  `result_bits[i] = (bit_idx, src_var)`.
            Stmt::Shuffle { result_bits, .. } => {
                let elems: Vec<IrExpr> = result_bits
                    .iter()
                    .map(|(bit_idx, src_var)| {
                        let src_name = vname(src_var);
                        let src_width = var_bit_width(*src_var, type_map, types);
                        if src_width == 1 {
                            clone_expr(var(&src_name))
                        } else {
                            clone_expr(index_bit(&src_name, *bit_idx as usize))
                        }
                    })
                    .collect();
                Some(IrExpr::FixedArray(elems))
            }

            // ── OracleCall / OracleOutput / ActionCall / ActionOutput / Rng ──
            // OracleCall/OracleOutput are not yet supported.
            // ActionCall is supported for any action that has a config via
            // `action_config(name)`, including the backward-compatible `_pub` heuristic.
            Stmt::OracleCall { name, .. } => {
                panic!(
                    "TfheScheme CFG path: OracleCall '{}' is not yet supported. \
                     Use the flat path (TfheScheme::flat()) or implement the oracle \
                     as a homomorphic function.",
                    name
                )
            }
            Stmt::OracleOutput { .. } => {
                panic!("TfheScheme CFG path: OracleOutput is not yet supported.")
            }
            Stmt::ActionCall { name, guard, args, fallbacks, .. } => {
                // Require an explicit config or the `_pub` heuristic.
                match self.action_config(name) {
                    Some(_) => {}
                    None => panic!(
                        "TfheScheme CFG path: ActionCall '{}' has no configuration. \
                         Register it with TfheScheme::with_action_config, or give it \
                         a '_pub' suffix for the backward-compatible all-public heuristic.",
                        name
                    ),
                }

                let guard_name = vname(guard);
                let action_cfg = self.action_config(name);

                // Build fallback expressions, promoting public fallbacks to wire
                // type when the action output is not public.  This mirrors the
                // arg_exprs promotion below: single-bit values use
                // `tfhe_trivial_encrypt`, multi-bit arrays use `RawMap`.
                let fb_exprs: Vec<IrExpr> = fallbacks.iter().enumerate().map(|(i, f)| {
                    let is_pub_output = action_cfg.as_ref()
                        .map(|c| c.is_output_public(i))
                        .unwrap_or(true);
                    if !is_pub_output && public_set.is_public(*f) {
                        // Fallback is public but action expects wire type — promote.
                        let w = type_map.get(&f.0)
                            .map(|tid| ir_type_bit_width(*tid, types))
                            .unwrap_or(1);
                        let fname = vname(f);
                        if w == 1 {
                            IrExpr::Call {
                                func: Box::new(IrExpr::Path {
                                    segments: vec!["tfhe_trivial_encrypt".into()],
                                    type_args: vec![IrType::TypeParam("N_LWE".into())],
                                }),
                                args: vec![var(&fname)],
                            }
                        } else {
                            IrExpr::RawMap {
                                receiver: Box::new(var(&fname)),
                                elem_var: IrPattern::ident("b"),
                                body: Box::new(IrExpr::Call {
                                    func: Box::new(IrExpr::Path {
                                        segments: vec!["tfhe_trivial_encrypt".into()],
                                        type_args: vec![IrType::TypeParam("N_LWE".into())],
                                    }),
                                    args: vec![var("b")],
                                }),
                            }
                        }
                    } else {
                        clone_expr(var(&vname(f)))
                    }
                }).collect();

                // Build arg expressions, promoting public args to wire type
                // via tfhe_trivial_encrypt (single bit) or .map(|b| tfhe_trivial_encrypt(b)) (array).
                let arg_exprs: Vec<IrExpr> = args.iter().map(|a| {
                    if public_set.is_public(*a) {
                        let w = type_map.get(&a.0)
                            .map(|tid| ir_type_bit_width(*tid, types))
                            .unwrap_or(1);
                        let aname = vname(a);
                        if w == 1 {
                            // tfhe_trivial_encrypt::<N_LWE>(var)
                            IrExpr::Call {
                                func: Box::new(IrExpr::Path {
                                    segments: vec!["tfhe_trivial_encrypt".into()],
                                    type_args: vec![IrType::TypeParam("N_LWE".into())],
                                }),
                                args: vec![var(&aname)],
                            }
                        } else {
                            // var.map(|b| tfhe_trivial_encrypt::<N_LWE>(b))
                            IrExpr::RawMap {
                                receiver: Box::new(var(&aname)),
                                elem_var: IrPattern::ident("b"),
                                body: Box::new(IrExpr::Call {
                                    func: Box::new(IrExpr::Path {
                                        segments: vec!["tfhe_trivial_encrypt".into()],
                                        type_args: vec![IrType::TypeParam("N_LWE".into())],
                                    }),
                                    args: vec![var("b")],
                                }),
                            }
                        }
                    } else {
                        clone_expr(var(&vname(a)))
                    }
                }).collect();

                let mut call_args: Vec<IrExpr> = Vec::new();

                if public_set.is_public(*guard) {
                    // Public guard — flat call: action(guard_bool, fallbacks..., args...)
                    call_args.push(clone_expr(var(&guard_name)));
                    call_args.extend(fb_exprs.clone());
                    call_args.extend(arg_exprs);
                } else {
                    // Encrypted guard — flat call:
                    //   action(true, fallbacks..., guard_enc, fallbacks..., args...)
                    //
                    // The leading `true` means "always execute"; the action function
                    // receives the encrypted guard before the args and performs
                    // oblivious selection internally.  The fallbacks appear twice so
                    // that stripping the outer `true, fallbacks...` prefix yields a
                    // second valid action-call suffix, enabling compositional unwrapping.
                    call_args.push(IrExpr::Lit(IrLit::Bool(true)));
                    call_args.extend(fb_exprs.clone());
                    call_args.push(clone_expr(var(&guard_name)));
                    call_args.extend(fb_exprs.clone());
                    call_args.extend(arg_exprs);
                }

                // Build turbofish type_args from the scheme's generics so Rust
                // can resolve const generic parameters on the action function.
                let turbofish_args: Vec<IrType> = self.generics().iter().map(|g| {
                    IrType::TypeParam(g.name.clone())
                }).collect();

                Some(IrExpr::Call {
                    func: Box::new(IrExpr::Path {
                        segments: vec![name.clone()],
                        type_args: turbofish_args,
                    }),
                    args: call_args,
                })
            }
            Stmt::ActionOutput { call, idx, .. } => {
                // Always emit a field projection from the ActionCall result tuple.
                // Publicness is tracked separately via `public_set`; the expression
                // is the same regardless of whether the output is clear or encrypted.
                let call_name = vname(call);
                Some(IrExpr::Field {
                    base: Box::new(var(&call_name)),
                    field: format!("{}", idx),
                })
            }
            Stmt::Rng { name, .. } => {
                panic!(
                    "TfheScheme CFG path: Rng source '{}' is not yet supported.",
                    name
                )
            }

            // StorageRead / StorageWrite are handled before emit_ir_stmt is called.
            Stmt::StorageRead { .. } | Stmt::StorageWrite { .. } => {
                unreachable!("StorageRead/StorageWrite should be handled before emit_ir_stmt")
            }
        }
    }

    fn helper_type_stubs(&self) -> Vec<IrFunction> {
        let wire_ty = self.wire_type();
        let n_lwe_gen: Vec<IrGenericParam> = self.generics().into_iter()
            .filter(|g| g.name == "N_LWE")
            .collect();
        let all_gens = self.generics();
        let bk_params = self.extra_params();
        let stub_body = || IrBlock {
            stmts: vec![],
            stmt_provs: vec![],
            expr: Some(Box::new(IrExpr::Unreachable)),
        };

        let mut stubs = Vec::new();

        // tfhe_trivial_encrypt(b: bool) -> wire_type
        stubs.push(IrFunction {
            name: "tfhe_trivial_encrypt".into(),
            module_path: vec![],
            generics: n_lwe_gen.clone(),
            receiver: None,
            params: vec![IrParam {
                name: "b".into(),
                ty: IrType::Primitive(PrimitiveType::Bool),
            }],
            return_type: Some(wire_ty.clone()),
            where_clause: vec![],
            body: stub_body(),
            external_kind: ExternalKind::TypeStub,
        });

        // tfhe_trivial_zero() -> wire_type
        stubs.push(IrFunction {
            name: "tfhe_trivial_zero".into(),
            module_path: vec![],
            generics: n_lwe_gen.clone(),
            receiver: None,
            params: vec![],
            return_type: Some(wire_ty.clone()),
            where_clause: vec![],
            body: stub_body(),
            external_kind: ExternalKind::TypeStub,
        });

        // tfhe_trivial_one() -> wire_type
        stubs.push(IrFunction {
            name: "tfhe_trivial_one".into(),
            module_path: vec![],
            generics: n_lwe_gen,
            receiver: None,
            params: vec![],
            return_type: Some(wire_ty.clone()),
            where_clause: vec![],
            body: stub_body(),
            external_kind: ExternalKind::TypeStub,
        });

        // tfhe_cmux(cond, a, b, bk) -> wire_type
        let mut cmux_params = vec![
            IrParam { name: "cond".into(), ty: wire_ty.clone() },
            IrParam { name: "a".into(), ty: wire_ty.clone() },
            IrParam { name: "b".into(), ty: wire_ty.clone() },
        ];
        cmux_params.extend(bk_params);
        stubs.push(IrFunction {
            name: "tfhe_cmux".into(),
            module_path: vec![],
            generics: all_gens,
            receiver: None,
            params: cmux_params,
            return_type: Some(wire_ty),
            where_clause: vec![],
            body: stub_body(),
            external_kind: ExternalKind::TypeStub,
        });

        stubs
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
pub fn print_fhe_flat_module(module: &IrModule<IrFunction>, self_contained: bool) -> String {
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

/// Render a CFG FHE `IrCfgModule` to TypeScript source.
pub fn print_fhe_cfg_module_ts(module: &IrCfgModule) -> String {
    volar_compiler::printer_ts::print_cfg_module_ts(module)
}

/// Render a CFG FHE `IrCfgModule` to C source via the LIR -> C backend pipeline.
///
/// Pass a [`MonoEnv`](volar_lir_codegen::mono::MonoEnv) with concrete values for all
/// const generic parameters; monomorphization is applied on the fly during lowering.
///
/// Auxiliary (spec) functions are skipped — they contain Rust-specific
/// constructs (enums, match, etc.) that the LIR/C pipeline does not support.
/// The FHE helper type stubs (`ExternalKind::TypeStub`) are included in the
/// module from `weave_fhe_cfg` and provide return-type info to the LIR codegen.
pub fn print_fhe_cfg_module_c(module: &IrCfgModule, env: &volar_lir_codegen::mono::MonoEnv) -> String {
    use volar_c_backend::CBackend;

    let mut backend = CBackend::new();
    volar_lir_codegen::lower_cfg_module_with_opts(module, &mut backend, env, false);
    backend.finish()
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use std::{vec, vec::Vec, string::String, collections::BTreeMap, format};

    use super::*;
    use volar_ir::{
        boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
        ir::{
            IRBlockId, IRBlockTargetId, IRVarId,
            IRBlocks, IRBlock, IRTerminator, IRTypes, IRType, PrimType, Constant, Stmt as IRStmt_,
        },
    };
    use crate::tests_common::run_compile_check;

    // ---- CFG circuit builders -----------------------------------------------

    /// Single-block AND circuit: params=[Bit, Bit], var_2 = a*b (Poly), Return var_2.
    fn build_ir_and_cfg() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let mut coeffs = BTreeMap::new();
        coeffs.insert(vec![IRVarId(0), IRVarId(1)], 1u8);
        let block = IRBlock {
            params: vec![bit, bit],
            stmts: vec![IRStmt_::Poly { ty: bit, coeffs, constant: Constant { hi: 0, lo: 0 } }],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            },
        };
        (IRBlocks::new(vec![block]), types)
    }

    /// Two-block circuit with a **public** condition.
    ///
    /// ```text
    /// Block 0: params=[Bit, Bit]
    ///   var_2 = Const(0, Bit)   // public
    ///   JumpCond(cond=var_2,
    ///     true  → Block(1)[var_0],
    ///     false → Block(1)[var_1])
    /// Block 1: params=[Bit]
    ///   Return blk1_p0
    /// ```
    fn build_ir_two_block_public_branch() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let zero = Constant { hi: 0, lo: 0 };

        let block0 = IRBlock {
            params: vec![bit, bit],
            stmts: vec![IRStmt_::Const(zero, bit)],
            stmt_provs: vec![()],
            terminator: IRTerminator::JumpCond {
                condition: IRVarId(2), // the Const — public
                true_block:  IRBlockTargetId::Block(IRBlockId(1)),
                true_args:   vec![IRVarId(0)],
                false_block: IRBlockTargetId::Block(IRBlockId(1)),
                false_args:  vec![IRVarId(1)],
            },
        };
        let block1 = IRBlock {
            params: vec![bit],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        };
        (IRBlocks::new(vec![block0, block1]), types)
    }

    /// Two-block circuit with an **encrypted** condition and same target (CMUX path).
    ///
    /// ```text
    /// Block 0: params=[Bit, Bit]
    ///   JumpCond(cond=var_0,
    ///     true  → Block(1)[var_0],
    ///     false → Block(1)[var_1])
    /// Block 1: params=[Bit]
    ///   Return blk1_p0
    /// ```
    fn build_ir_two_block_encrypted_same_target() -> (IRBlocks, IRTypes) {
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));

        let block0 = IRBlock {
            params: vec![bit, bit],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::JumpCond {
                condition: IRVarId(0), // encrypted — not in public_set
                true_block:  IRBlockTargetId::Block(IRBlockId(1)),
                true_args:   vec![IRVarId(0)],
                false_block: IRBlockTargetId::Block(IRBlockId(1)),
                false_args:  vec![IRVarId(1)],
            },
        };
        let block1 = IRBlock {
            params: vec![bit],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        };
        (IRBlocks::new(vec![block0, block1]), types)
    }

    // ---- CFG tests ----------------------------------------------------------

    /// Compile-check TFHE-CFG generated code by prepending the necessary `use` glob.
    fn run_compile_check_tfhe_cfg(code: &str, test_name: &str) {
        crate::tests_common::run_compile_check_tfhe_cfg(code, test_name);
    }

    #[test]
    fn test_tfhe_cfg_single_block_and_compiles() {
        let (circuit, types) = build_ir_and_cfg();
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "and_cfg", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg from TfheScheme::cfg()"),
        };
        let code = print_fhe_cfg_module(&module, true);
        run_compile_check_tfhe_cfg(&code, "tfhe_cfg_and");
    }

    #[test]
    fn test_tfhe_cfg_two_block_public_branch_emits_cond_goto() {
        // In TFHE every wire — including Const-derived "public" values — is an
        // LweCiphertext, so the generated `if var_2 { ... }` is not valid Rust.
        // This test verifies the structural property (CondGoto is chosen over
        // CMUX) without attempting a full compile check.
        let (circuit, types) = build_ir_two_block_public_branch();
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "pub_branch", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg from TfheScheme::cfg()"),
        };
        let code = print_fhe_cfg_module(&module, true);
        // Public condition → CondGoto (Rust if/else), NOT a CMUX.
        assert!(
            !code.contains("tfhe_cmux"),
            "public-condition branch should use CondGoto, not tfhe_cmux:\n{code}"
        );
        assert!(
            code.contains("if "),
            "public-condition branch should emit a Rust if/else:\n{code}"
        );
    }

    #[test]
    fn test_tfhe_cfg_two_block_encrypted_same_target_compiles() {
        let (circuit, types) = build_ir_two_block_encrypted_same_target();
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "enc_cmux", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg from TfheScheme::cfg()"),
        };
        let code = print_fhe_cfg_module(&module, true);
        // Encrypted same-target condition → CMUX stmts emitted before Goto.
        assert!(
            code.contains("tfhe_cmux"),
            "encrypted same-target branch should emit tfhe_cmux:\n{code}"
        );
        run_compile_check_tfhe_cfg(&code, "tfhe_cfg_enc_cmux");
    }

    #[test]
    fn test_tfhe_cfg_const_emits_bool_literal() {
        // Circuit: single block, params=[], var_0 = Const(1, Bit), Return var_0.
        // Expected: generated code contains `true` or `false` as bool literal,
        // NOT `tfhe_trivial_one()` or `tfhe_trivial_zero()`.
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let one = Constant { hi: 0, lo: 1 };
        let block = IRBlock {
            params: vec![],
            stmts: vec![IRStmt_::Const(one, bit)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        };
        let circuit = IRBlocks::new(vec![block]);
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "const_bool", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg"),
        };
        let code = print_fhe_cfg_module(&module, true);
        assert!(
            code.contains("true") || code.contains("false"),
            "Const should emit a bool literal:\n{code}"
        );
        assert!(
            !code.contains("tfhe_trivial_one") && !code.contains("tfhe_trivial_zero"),
            "Const should NOT emit trivial encryption:\n{code}"
        );
    }

    #[test]
    fn test_tfhe_cfg_all_public_poly_emits_bool_arithmetic() {
        // Circuit: block, params=[], c0 = Const(1), c1 = Const(0),
        // var_2 = Poly(c0 * c1), Return var_2.
        // All vars are public → generated code uses `^` / `&`, not tfhe_xor/and.
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let one = Constant { hi: 0, lo: 1 };
        let zero = Constant { hi: 0, lo: 0 };
        let mut coeffs = BTreeMap::new();
        // monomial: var_0 AND var_1 (coefficient 1)
        coeffs.insert(vec![IRVarId(0), IRVarId(1)], 1u8);
        let block = IRBlock {
            params: vec![],
            stmts: vec![
                IRStmt_::Const(one,  bit), // var_0 = 1 (public)
                IRStmt_::Const(zero, bit), // var_1 = 0 (public)
                IRStmt_::Poly { ty: bit, coeffs, constant: Constant { hi: 0, lo: 0 } },
            ],
            stmt_provs: vec![(), (), ()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            },
        };
        let circuit = IRBlocks::new(vec![block]);
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "pub_poly", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg"),
        };
        let code = print_fhe_cfg_module(&module, true);
        assert!(
            !code.contains("tfhe_xor") && !code.contains("tfhe_gate_bootstrapping_and"),
            "all-public Poly should NOT use TFHE gates:\n{code}"
        );
        assert!(
            !code.contains("tfhe_trivial_one") && !code.contains("tfhe_trivial_zero"),
            "all-public Poly should NOT use trivial encryptions:\n{code}"
        );
    }

    #[test]
    fn test_tfhe_cfg_mixed_poly_wraps_public_in_trivial_encrypt() {
        // Circuit: params=[Bit], var_1 = Const(1, Bit), var_2 = Poly(input_0 * var_1).
        // input_0 is encrypted; var_1 is public → var_1 must be wrapped in
        // tfhe_trivial_encrypt before the AND gate.
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let one = Constant { hi: 0, lo: 1 };
        let mut coeffs = BTreeMap::new();
        // monomial: var_0 (encrypted input) AND var_1 (public const)
        coeffs.insert(vec![IRVarId(0), IRVarId(1)], 1u8);
        let block = IRBlock {
            params: vec![bit],
            stmts: vec![
                IRStmt_::Const(one, bit), // var_1 = true (public)
                IRStmt_::Poly { ty: bit, coeffs, constant: Constant { hi: 0, lo: 0 } },
            ],
            stmt_provs: vec![(), ()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            },
        };
        let circuit = IRBlocks::new(vec![block]);
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "mixed_poly", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg"),
        };
        let code = print_fhe_cfg_module(&module, true);
        assert!(
            code.contains("tfhe_trivial_encrypt"),
            "mixed Poly should wrap public var with tfhe_trivial_encrypt:\n{code}"
        );
        assert!(
            code.contains("tfhe_gate_bootstrapping_and"),
            "mixed Poly should use TFHE AND gate:\n{code}"
        );
        run_compile_check_tfhe_cfg(&code, "tfhe_cfg_mixed_poly");
    }

    #[test]
    fn test_tfhe_cfg_public_var_promoted_at_jmp_site() {
        // Two-block circuit where Block 0 passes a Const (public) as a jump arg.
        // Block 1's param is LweCiphertext, so the public value must be promoted
        // via tfhe_trivial_encrypt at the jump site.
        //
        // Block 0: params=[Bit]
        //   var_1 = Const(0, Bit)  // public
        //   Jmp → Block(1)[var_1]
        // Block 1: params=[Bit]
        //   Return blk1_p0
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let zero = Constant { hi: 0, lo: 0 };
        let block0 = IRBlock {
            params: vec![bit],
            stmts: vec![IRStmt_::Const(zero, bit)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(1)], // public Const passed as block arg
            },
        };
        let block1 = IRBlock {
            params: vec![bit],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        };
        let circuit = IRBlocks::new(vec![block0, block1]);
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "pub_jmp_promote", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg"),
        };
        let code = print_fhe_cfg_module(&module, true);
        assert!(
            code.contains("tfhe_trivial_encrypt"),
            "public var passed to block jump should be promoted via tfhe_trivial_encrypt:\n{code}"
        );
        run_compile_check_tfhe_cfg(&code, "tfhe_cfg_pub_jmp_promote");
    }
    ///
    /// ```text
    /// params: 2 (input_0, input_1)
    /// wire_2 = StorageWrite(storage=5, src=input_0, bit_width=1, addr=[input_1])
    /// wire_3 = StorageRead(storage=5, bit_width=1, addr=[input_1])
    /// Return wire_3
    /// ```
    fn build_storage_circuit() -> BIrBlocks {
        BIrBlocks { blocks: vec![BIrBlock {
            params: 2,
            stmts: vec![
                BIrStmt::StorageWrite {
                    storage: StorageId(5),
                    src: IRVarId(0),
                    bit_width: 1,
                    addr: vec![IRVarId(1)],
                },
                BIrStmt::StorageRead {
                    storage: StorageId(5),
                    bit_width: 1,
                    addr: vec![IRVarId(1)],
                },
            ],
            stmt_provs: vec![(), ()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(3)],
            }),
        }], pre_init: vec![] }
    }

    fn storage_config_2cells() -> FheStorageConfig {
        let mut sizes = FheStorageSizes::new();
        sizes.insert((5, 1), 2);
        FheStorageConfig { sizes }
    }

    #[test]
    fn test_tfhe_flat_with_storage_does_not_panic() {
        let circuit = build_storage_circuit();
        let scheme = TfheScheme::flat();
        let config = storage_config_2cells();
        let module = weave_fhe_flat_bir(
            &circuit, &scheme, "stor_test", None, &NoProvenance, Some(&config),
        );
        // Should produce a function with storage parameters.
        assert_eq!(module.functions.len(), 1);
        let func = &module.functions[0];
        // Extra params (bk) + 2 inputs + 1 storage param.
        assert!(
            func.params.len() >= 4,
            "expected >=4 params (bk + 2 inputs + storage), got {}",
            func.params.len()
        );
        // The last param should be the storage slice.
        let stor_param = func.params.last().unwrap();
        assert_eq!(stor_param.name, "storage_5_1");
    }

    #[test]
    fn test_grafhen_flat_with_storage_does_not_panic() {
        let circuit = build_storage_circuit();
        let scheme = GrafhenScheme::new(64);
        let config = storage_config_2cells();
        let module = weave_fhe_flat_bir(
            &circuit, &scheme, "stor_grafhen", None, &NoProvenance, Some(&config),
        );
        assert_eq!(module.functions.len(), 1);
        let func = &module.functions[0];
        // pk + 2 inputs + 1 storage param.
        assert!(
            func.params.len() >= 4,
            "expected >=4 params, got {}",
            func.params.len()
        );
        let stor_param = func.params.last().unwrap();
        assert_eq!(stor_param.name, "storage_5_1");
    }

    #[test]
    fn test_flat_storage_no_config_panics_gracefully() {
        // Without storage config, storage ops produce zero wires (empty cells).
        let circuit = build_storage_circuit();
        let scheme = TfheScheme::flat();
        // Pass None => 0 cells => reads produce zeros, writes are no-ops.
        let module = weave_fhe_flat_bir(
            &circuit, &scheme, "no_stor", None, &NoProvenance, None,
        );
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn test_flat_storage_writeback_present() {
        let circuit = build_storage_circuit();
        let scheme = TfheScheme::flat();
        let config = storage_config_2cells();
        let module = weave_fhe_flat_bir(
            &circuit, &scheme, "wb", None, &NoProvenance, Some(&config),
        );
        let code = print_fhe_flat_module(&module, true);
        // The write-back assigns to storage_5_1[0] and storage_5_1[1].
        assert!(
            code.contains("storage_5_1"),
            "generated code should reference storage parameter"
        );
    }

    // ---- Auto-derive storage tests ------------------------------------------

    #[test]
    fn test_derive_storage_config_matches_manual() {
        let circuit = build_storage_circuit();
        let derived = derive_storage_config(&circuit);
        let manual = storage_config_2cells();
        assert_eq!(
            derived.sizes, manual.sizes,
            "auto-derived storage config should match manual config"
        );
    }

    #[test]
    fn test_derive_storage_config_empty_circuit() {
        // A circuit with no storage stmts should produce an empty config.
        let circuit = BIrBlocks { blocks: vec![BIrBlock {
            params: 2,
            stmts: vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            }),
        }], pre_init: vec![] };
        let derived = derive_storage_config(&circuit);
        assert!(
            derived.sizes.is_empty(),
            "circuit without storage ops should yield empty config"
        );
    }

    #[test]
    fn test_derive_storage_config_multi_bit_addr() {
        // 2-bit address should yield 4 cells.
        let circuit = BIrBlocks { blocks: vec![BIrBlock {
            params: 3,
            stmts: vec![
                BIrStmt::StorageRead {
                    storage: StorageId(0),
                    bit_width: 1,
                    addr: vec![IRVarId(0), IRVarId(1)],
                },
            ],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(3)],
            }),
        }], pre_init: vec![] };
        let derived = derive_storage_config(&circuit);
        assert_eq!(
            derived.sizes.get(&(0, 1)),
            Some(&4),
            "2-bit address should yield 4 cells"
        );
    }

    #[test]
    fn test_auto_derive_weave_produces_storage_params() {
        // Call weave_fhe_flat_bir with the auto-derived config and verify
        // it produces the same storage parameter as the manual config.
        let circuit = build_storage_circuit();
        let scheme = TfheScheme::flat();

        let manual_config = storage_config_2cells();
        let manual_module = weave_fhe_flat_bir(
            &circuit, &scheme, "manual", None, &NoProvenance, Some(&manual_config),
        );

        let derived_config = derive_storage_config(&circuit);
        let derived_module = weave_fhe_flat_bir(
            &circuit, &scheme, "derived", None, &NoProvenance, Some(&derived_config),
        );

        let manual_func = &manual_module.functions[0];
        let derived_func = &derived_module.functions[0];
        assert_eq!(
            manual_func.params.len(),
            derived_func.params.len(),
            "auto-derived should produce same number of params as manual"
        );
        // The last param should be the storage slice in both cases.
        assert_eq!(
            manual_func.params.last().unwrap().name,
            derived_func.params.last().unwrap().name,
            "storage param name should match"
        );
    }

    // ---- Loop-based oblivious access tests ---------------------------------

    /// Build an `IrModule` whose single function calls `oblivious_read_loop`
    /// with `cell_count` cells and `addr_width` address bits.  The function
    /// takes `bk`, `cell_count` cell params, and `addr_width` addr params,
    /// returning a single `LweCiphertext<N_LWE>`.
    fn build_loop_read_module(cell_count: usize, addr_width: usize) -> IrModule<IrFunction> {
        let scheme = TfheScheme::flat();
        let wire_ty = scheme.wire_type();

        let mut params: Vec<IrParam> = scheme.extra_params();
        let mut cell_names = Vec::new();
        for i in 0..cell_count {
            let name = format!("c{}", i);
            params.push(IrParam { name: name.clone(), ty: wire_ty.clone() });
            cell_names.push(name);
        }
        let mut addr_names = Vec::new();
        for i in 0..addr_width {
            let name = format!("a{}", i);
            params.push(IrParam { name: name.clone(), ty: wire_ty.clone() });
            addr_names.push(name);
        }

        let addr_refs: Vec<&str> = addr_names.iter().map(|s| s.as_str()).collect();
        let mut stmts: Vec<IrStmt> = Vec::new();
        let mut provs: Vec<()> = Vec::new();

        let result = oblivious_read_loop(
            &scheme, &cell_names, &addr_refs, "rd", &mut stmts, &mut provs,
        );

        IrModule {
            name: "loop_read_test".into(),
            structs: vec![],
            enums: vec![],
            traits: vec![],
            impls: vec![],
            type_aliases: vec![],
            functions: vec![IrFunction {
                name: "loop_read_tfhe".into(),
                module_path: vec![],
                generics: scheme.generics(),
                receiver: None,
                params,
                return_type: Some(wire_ty),
                where_clause: vec![],
                body: IrBlock {
                    stmts,
                    stmt_provs: provs,
                    expr: Some(Box::new(var(&result))),
                },
                external_kind: ExternalKind::Normal,
            }],
        }
    }

    /// Build an `IrModule` whose single function calls `oblivious_write_loop`
    /// with `cell_count` cells, `addr_width` address bits, and a `src` wire.
    /// Returns a tuple of the new cell values.
    fn build_loop_write_module(cell_count: usize, addr_width: usize) -> IrModule<IrFunction> {
        let scheme = TfheScheme::flat();
        let wire_ty = scheme.wire_type();

        let mut params: Vec<IrParam> = scheme.extra_params();
        let mut cell_names = Vec::new();
        for i in 0..cell_count {
            let name = format!("c{}", i);
            params.push(IrParam { name: name.clone(), ty: wire_ty.clone() });
            cell_names.push(name);
        }
        let mut addr_names = Vec::new();
        for i in 0..addr_width {
            let name = format!("a{}", i);
            params.push(IrParam { name: name.clone(), ty: wire_ty.clone() });
            addr_names.push(name);
        }
        params.push(IrParam { name: "src".into(), ty: wire_ty.clone() });

        let addr_refs: Vec<&str> = addr_names.iter().map(|s| s.as_str()).collect();
        let mut stmts: Vec<IrStmt> = Vec::new();
        let mut provs: Vec<()> = Vec::new();

        let new_cells = oblivious_write_loop(
            &scheme, &cell_names, &addr_refs, "src", "wr", &mut stmts, &mut provs,
        );

        // Return a tuple of the new cells.
        let ret_exprs: Vec<IrExpr> = new_cells.iter().map(|n| var(n)).collect();
        let ret_tys: Vec<IrType> = (0..cell_count).map(|_| wire_ty.clone()).collect();
        let (ret_expr, ret_ty) = if cell_count == 1 {
            (ret_exprs.into_iter().next().unwrap(), wire_ty)
        } else {
            (IrExpr::Tuple(ret_exprs), IrType::Tuple(ret_tys))
        };

        IrModule {
            name: "loop_write_test".into(),
            structs: vec![],
            enums: vec![],
            traits: vec![],
            impls: vec![],
            type_aliases: vec![],
            functions: vec![IrFunction {
                name: "loop_write_tfhe".into(),
                module_path: vec![],
                generics: scheme.generics(),
                receiver: None,
                params,
                return_type: Some(ret_ty),
                where_clause: vec![],
                body: IrBlock {
                    stmts,
                    stmt_provs: provs,
                    expr: Some(Box::new(ret_expr)),
                },
                external_kind: ExternalKind::Normal,
            }],
        }
    }

    /// Compile-check helper for loop-based codegen tests — needs `tfhe_not`
    /// in addition to the standard TFHE imports.
    fn run_compile_check_tfhe_loop(code: &str, test_name: &str) {
        let uses = "use volar_spec::tfhe::{BootstrappingKey, LweCiphertext, \
                    tfhe_gate_bootstrapping_and, tfhe_gate_bootstrapping_or, \
                    tfhe_xor, tfhe_not, tfhe_trivial_zero, \
                    tfhe_trivial_one, tfhe_trivial_encrypt};\n";
        let with_imports = if let Some(newline) = code.find('\n') {
            let (head, tail) = code.split_at(newline + 1);
            format!("{head}{uses}{tail}")
        } else {
            format!("{uses}{code}")
        };
        run_compile_check(&with_imports, test_name);
    }

    #[test]
    fn test_oblivious_read_loop_2cells_compiles() {
        let module = build_loop_read_module(2, 1);
        let code = print_fhe_flat_module(&module, true);
        run_compile_check_tfhe_loop(&code, "loop_read_2cells");
    }

    #[test]
    fn test_oblivious_read_loop_4cells_compiles() {
        let module = build_loop_read_module(4, 2);
        let code = print_fhe_flat_module(&module, true);
        run_compile_check_tfhe_loop(&code, "loop_read_4cells");
    }

    #[test]
    fn test_oblivious_write_loop_2cells_compiles() {
        let module = build_loop_write_module(2, 1);
        let code = print_fhe_flat_module(&module, true);
        run_compile_check_tfhe_loop(&code, "loop_write_2cells");
    }

    #[test]
    fn test_oblivious_write_loop_4cells_compiles() {
        let module = build_loop_write_module(4, 2);
        let code = print_fhe_flat_module(&module, true);
        run_compile_check_tfhe_loop(&code, "loop_write_4cells");
    }

    // ---- New feature tests --------------------------------------------------

    /// Block param is typed `bool` when every predecessor passes a public value.
    ///
    /// Circuit:
    /// ```text
    /// Block 0: params=[Bit]
    ///   var_1 = Const(1, Bit)    // public
    ///   Jmp → Block(1)[var_1]
    /// Block 1: params=[Bit]     ← only predecessor passes a public Const
    ///   Return blk1_p0
    /// ```
    ///
    /// Expected: `blk1_p0: bool` in the emitted code, NOT `LweCiphertext<N_LWE>`.
    /// The return site should promote `blk1_p0` via `tfhe_trivial_encrypt`.
    #[test]
    fn test_tfhe_cfg_public_block_param_typed_bool() {
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let one = Constant { hi: 0, lo: 1 };
        let block0 = IRBlock {
            params: vec![bit],
            stmts: vec![IRStmt_::Const(one, bit)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(1)], // Const — public
            },
        };
        let block1 = IRBlock {
            params: vec![bit],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)], // blk1_p0
            },
        };
        let circuit = IRBlocks::new(vec![block0, block1]);
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "pub_param_bool", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg"),
        };
        let code = print_fhe_cfg_module(&module, true);

        // Block 1's param must be typed `bool`, not an LweCiphertext.
        assert!(
            code.contains("bool"),
            "public block param should be typed `bool`:\n{code}"
        );
        assert!(
            !code.contains("LweCiphertext") || code.contains("tfhe_trivial_encrypt"),
            "if LweCiphertext appears, it must be from a promote call:\n{code}"
        );
        // The return site must promote the bool param to LweCiphertext.
        assert!(
            code.contains("tfhe_trivial_encrypt"),
            "return site should promote public param via tfhe_trivial_encrypt:\n{code}"
        );
        // Jump site should NOT promote (target param is already bool).
        // The call to tfhe_trivial_encrypt should only appear at the return, not in the jump args.
        run_compile_check_tfhe_cfg(&code, "tfhe_cfg_pub_param_bool");
    }

    /// `with_action_config` builder sets config that overrides the `_pub` heuristic.
    ///
    /// Circuit:
    /// ```text
    /// Block 0: params=[Bit]
    ///   var_1 = Const(1, Bit)              // guard — public
    ///   var_2 = ActionCall("my_action", guard=var_1, args=[input_0], fallbacks=[var_1])
    ///   var_3 = ActionOutput(var_2, 0)
    ///   Return var_3
    /// ```
    ///
    /// `"my_action"` has no `_pub` suffix; it must be registered explicitly.
    /// The guard (`var_1`) is a public const, so the emitted call is flat:
    /// `my_action(var_1, var_1, input_0)` — no `if/else`.
    #[test]
    fn test_tfhe_cfg_action_config_builder() {
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let tuple_ty = types.intern(IRType::Primitive(PrimType::Bit)); // reuse bit as result type
        let one = Constant { hi: 0, lo: 1 };

        let block = IRBlock {
            params: vec![bit],
            stmts: vec![
                IRStmt_::Const(one, bit),  // var_1 = true (public guard)
                IRStmt_::ActionCall {
                    name: "my_action".into(),
                    guard: IRVarId(1),
                    args: vec![IRVarId(0)],
                    fallbacks: vec![IRVarId(1)],
                    output_tys: vec![bit],
                    result_ty: tuple_ty,
                },
                IRStmt_::ActionOutput { call: IRVarId(2), idx: 0, ty: bit },
            ],
            stmt_provs: vec![(), (), ()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(3)],
            },
        };
        let circuit = IRBlocks::new(vec![block]);
        let scheme = TfheScheme::cfg().with_action_config(
            "my_action",
            FheActionConfig { output_public: vec![] },
        );
        let output = weave_fhe(&circuit, &types, &scheme, "action_cfg_builder", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg"),
        };
        let code = print_fhe_cfg_module(&module, true);

        assert!(
            code.contains("my_action"),
            "generated code should call my_action:\n{code}"
        );
        // Public guard → flat call, no if/else wrapper.
        assert!(
            !code.contains("if "),
            "public guard should emit a flat call with no if/else:\n{code}"
        );
    }

    /// Encrypted guard emits a flat call with `true` + double fallbacks prepended.
    ///
    /// Circuit: same as above but the guard (`input_0`) is encrypted.
    /// Config: `output_public: vec![false]`.
    ///
    /// Expected: `private_action(true, var_1, input_0, var_1, input_0)` — no `if`, no `else`.
    /// The leading `true` means "always execute"; stripping `true, var_1` leaves a
    /// second valid action-call suffix `input_0, var_1, input_0` (encrypted guard,
    /// fallback, arg), enabling compositional unwrapping.
    #[test]
    fn test_tfhe_cfg_private_guard_action() {
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let tuple_ty = types.intern(IRType::Primitive(PrimType::Bit));
        let one = Constant { hi: 0, lo: 1 };

        // The guard is `input_0` (encrypted). Fallback is a Const(1).
        let block = IRBlock {
            params: vec![bit],
            stmts: vec![
                IRStmt_::Const(one, bit),  // var_1 = true (fallback, public)
                IRStmt_::ActionCall {
                    name: "private_action".into(),
                    guard: IRVarId(0),     // encrypted guard
                    args: vec![IRVarId(0)],
                    fallbacks: vec![IRVarId(1)],
                    output_tys: vec![bit],
                    result_ty: tuple_ty,
                },
                IRStmt_::ActionOutput { call: IRVarId(2), idx: 0, ty: bit },
            ],
            stmt_provs: vec![(), (), ()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(3)],
            },
        };
        let circuit = IRBlocks::new(vec![block]);
        let scheme = TfheScheme::cfg().with_action_config(
            "private_action",
            FheActionConfig { output_public: vec![false] },
        );
        let output = weave_fhe(&circuit, &types, &scheme, "priv_guard_action", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg"),
        };
        let code = print_fhe_cfg_module(&module, true);

        assert!(
            code.contains("private_action"),
            "generated code should call private_action:\n{code}"
        );
        // Encrypted guard → flat call with leading `true`, no Rust if/else.
        assert!(
            !code.contains("if "),
            "Encrypted guard should emit a flat call with no if/else:\n{code}"
        );
        assert!(
            code.contains("true"),
            "Encrypted guard call should contain the leading `true` outer guard:\n{code}"
        );
    }

    /// Per-output publicness: one output public (`bool`), one encrypted.
    ///
    /// Circuit: ActionCall `mixed_action` with `output_public: [true, false]`.
    ///   var_3 = ActionOutput(var_2, 0) → public (bool field)
    ///   var_4 = ActionOutput(var_2, 1) → encrypted (LweCiphertext field)
    ///
    /// Both should emit field projections (`.0` and `.1`).
    #[test]
    fn test_tfhe_cfg_mixed_action_output_publicness() {
        let mut types = IRTypes::new();
        let bit = types.intern(IRType::Primitive(PrimType::Bit));
        let tuple_ty = types.intern(IRType::Primitive(PrimType::Bit));
        let one = Constant { hi: 0, lo: 1 };

        let block = IRBlock {
            params: vec![bit],
            stmts: vec![
                IRStmt_::Const(one, bit), // var_1 = true (public guard)
                IRStmt_::ActionCall {
                    name: "mixed_action".into(),
                    guard: IRVarId(1),
                    args: vec![IRVarId(0)],
                    fallbacks: vec![IRVarId(1), IRVarId(0)],
                    output_tys: vec![bit, bit],
                    result_ty: tuple_ty,
                },
                IRStmt_::ActionOutput { call: IRVarId(2), idx: 0, ty: bit }, // public output
                IRStmt_::ActionOutput { call: IRVarId(2), idx: 1, ty: bit }, // encrypted output
            ],
            stmt_provs: vec![(), (), (), ()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(3)], // return the public output
            },
        };
        let circuit = IRBlocks::new(vec![block]);
        let scheme = TfheScheme::cfg().with_action_config(
            "mixed_action",
            FheActionConfig { output_public: vec![true, false] },
        );
        let output = weave_fhe(&circuit, &types, &scheme, "mixed_action_out", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg"),
        };
        let code = print_fhe_cfg_module(&module, true);

        // Both outputs should use field projection syntax (.0 and .1).
        assert!(
            code.contains(".0"),
            "ActionOutput idx=0 should emit field projection .0:\n{code}"
        );
        assert!(
            code.contains(".1"),
            "ActionOutput idx=1 should emit field projection .1:\n{code}"
        );
        // The public output (var_3) should be promoted at the return site.
        assert!(
            code.contains("tfhe_trivial_encrypt"),
            "public ActionOutput returned should be promoted at return site:\n{code}"
        );
    }

    // ── E2E execution tests ─────────────────────────────────────────────

    /// End-to-end test: weave a simple AND circuit through TFHE, compile
    /// the result, run it with real key-generation / encryption / decryption,
    /// and verify the output matches `a && b` for all input combinations.
    ///
    /// This is the first test that actually *runs* generated FHE code rather
    /// than just compile-checking it.
    #[test]
    fn test_tfhe_cfg_and_executes_correctly() {
        let (circuit, types) = build_ir_and_cfg();
        let scheme = TfheScheme::cfg();
        let output = weave_fhe(&circuit, &types, &scheme, "and_cfg", None, None);
        let module = match output {
            FheOutput::Cfg(m) => m,
            _ => panic!("expected FheOutput::Cfg from TfheScheme::cfg()"),
        };
        let code = print_fhe_cfg_module(&module, true);

        // Prepend the use-imports the same way run_compile_check_tfhe_cfg does.
        let uses = "\
use volar_spec::tfhe::{\
    BootstrappingKey, LweCiphertext, \
    tfhe_gate_bootstrapping_and, tfhe_gate_bootstrapping_or, \
    tfhe_xor, tfhe_not, tfhe_trivial_zero, \
    tfhe_trivial_one, tfhe_trivial_encrypt, tfhe_cmux, \
    gen_lwe_secret_key, gen_rlwe_secret_key, gen_bootstrapping_key, \
    lwe_encrypt, lwe_decrypt};\n\
use volar_spec::SpecRng;\n\
use volar_macros::volar_action;\n";

        let code_with_imports = if let Some(nl) = code.find('\n') {
            let (head, tail) = code.split_at(nl + 1);
            format!("{head}{uses}{tail}")
        } else {
            format!("{uses}{code}")
        };

        // Append test harness: deterministic RNG + truth-table test.
        let test_harness = r#"

// ── Test harness ────────────────────────────────────────────────────────

struct TestRng(u64);
impl TestRng {
    fn new(seed: u64) -> Self { Self(seed) }
}
impl SpecRng for TestRng {
    fn next_u32(&mut self) -> u32 {
        self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
        z = z ^ (z >> 31);
        z as u32
    }
}

const N: usize = 8;
const BN: usize = 64;
const BSE: usize = 2;
const KSE: usize = 2;

#[test]
fn and_truth_table() {
    let mut rng = TestRng::new(42);
    let lwe_sk = gen_lwe_secret_key::<N, _>(&mut rng);
    let rlwe_sk = gen_rlwe_secret_key::<BN, _>(&mut rng);
    let bk = gen_bootstrapping_key::<N, BN, BSE, KSE, _>(
        &lwe_sk, &rlwe_sk, 16, 16, 0, 0, &mut rng,
    );

    for a in [false, true] {
        for b in [false, true] {
            let ct_a = lwe_encrypt(a, &lwe_sk, 0, &mut TestRng::new(100));
            let ct_b = lwe_encrypt(b, &lwe_sk, 0, &mut TestRng::new(200));
            let result = and_cfg_tfhe_cfg::<N, BN, BSE, KSE>(&bk, ct_a, ct_b);
            let decrypted = lwe_decrypt(&result, &lwe_sk);
            assert_eq!(
                decrypted, a && b,
                "AND({},{}) = {}, expected {}", a, b, decrypted, a && b,
            );
        }
    }
}
"#;

        let test_code = format!("{code_with_imports}{test_harness}");

        // Set up temp crate as a [[test]] target (like garble::test_multi_eval_and).
        let root = crate::tests_common::workspace_root();
        let tmpdir = std::env::temp_dir().join("volar_weaver_tfhe_e2e_and");
        let srcdir = tmpdir.join("src");
        std::fs::create_dir_all(&srcdir).unwrap();

        let cargo_toml = std::format!(
            "[package]\n\
             name = \"weave-exec-tfhe-and\"\n\
             version = \"0.1.0\"\n\
             edition = \"2024\"\n\
             \n\
             [[test]]\n\
             name = \"tfhe_and\"\n\
             path = \"src/lib.rs\"\n\
             \n\
             [dependencies]\n\
             volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
             volar-primitives = {{ path = \"{root}/crates/spec/volar-primitives\" }}\n\
             volar-common = {{ path = \"{root}/crates/spec/volar-common\" }}\n\
             volar-macros = {{ path = \"{root}/crates/macros/volar-macros\" }}\n\
             hybrid-array = \"0.4.8\"\n\
             digest = {{ version = \"0.11.2\", default-features = false }}\n\
             cipher = {{ version = \"0.5.1\", default-features = false }}\n\
             rand = {{ version = \"0.9.2\", default-features = false }}\n\
             typenum = {{ version = \"1.17\", default-features = false }}\n\
             elliptic-curve = {{ version = \"0.13.8\", features = [\"arithmetic\"], default-features = false }}\n",
            root = root,
        );

        std::fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).unwrap();
        std::fs::write(srcdir.join("lib.rs"), &test_code).unwrap();

        let output = std::process::Command::new("cargo")
            .args(["test", "--quiet", "--test", "tfhe_and"])
            .current_dir(&tmpdir)
            .env(
                "CARGO_TARGET_DIR",
                String::from(tmpdir.join("target").to_str().unwrap()),
            )
            .output()
            .expect("failed to run cargo test");

        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let _ = std::fs::remove_dir_all(&tmpdir);

        if !output.status.success() {
            panic!(
                "TFHE E2E AND test failed\n--- code ---\n{}\n--- stdout ---\n{}\n--- stderr ---\n{}",
                test_code, stdout, stderr
            );
        }
    }
}
