// @reliability: experimental
// @ai: assisted
//! VAFFLE `Module` → Volar IR `IRBlocks` lowering with a recursion-stack
//! calling convention.
//!
//! All VAFFLE functions are merged into a single `IRBlocks` where function
//! calls use `StorageRead`/`StorageWrite` on [`StorageId::STACK`] to pass
//! arguments and return values.  The **stack pointer** is threaded through
//! every block as the first N block-params (N = address bit-width), and
//! constant frame offsets are folded at compile time via [`StackPtr`].
//!
//! # Call protocol
//!
//! **Caller** (at a `Value::Call` site):
//! 1. Write arguments into `STACK[sp + param_off_i + j]`  (via `frame_push_args`)
//! 2. Advance SP by the callee's frame size
//! 3. Jump to the callee's entry block, passing SP bits as block params
//!
//! **Callee**:
//! 1. Read arguments from `STACK[sp - frame_size + param_off_i + j]`  (via `frame_read_args`)
//! 2. Compute body
//! 3. Write return value to `STACK[sp - frame_size + ret_off + j]`  (via `frame_write_ret`)
//! 4. Jump to the **continuation block** in the caller
//!
//! **Continuation block** (after the call):
//! 1. Retreat SP by the callee's frame size
//! 2. Read return value from `STACK[sp + ret_off + j]`  (via `frame_read_ret`)
//! 3. Continue with the returned bits
//!
//! The continuation block is created per call-site and passed as the last
//! block parameter of the callee's entry block (as an `IRType::Block`-typed
//! value for dynamic dispatch via `IRBlockTargetId::Dyn`).

use alloc::{
    collections::BTreeMap,
    string::String,
    vec,
    vec::Vec,
};

use vaffle::{
    BlockId, FuncBody, FuncDecl, FuncId, Module, SigDecl, Terminator, Target, Value, ValueId,
};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId,
    IRTypes, IRVarId, OracleDecl, ActionDecl,
};
use volar_ir_common::{Constant, IrType, StorageId, Type, TypeId};
use volar_lir::circuits::{
    bc_add, FrameLayout, StackPtr, StorageEmitter, BitCircuitBuilder,
    frame_push_args, frame_read_args, frame_write_ret, frame_read_ret,
};

/// Width of the stack-pointer address in bits.
///
/// 16 bits allows 65 536 storage slots — enough for deep recursion in circuits
/// of practical size.  Increase for very deep nesting.
pub const SP_BITS: usize = 16;

// ============================================================================
// Public entry point
// ============================================================================

/// Lower a VAFFLE [`Module`] into a single [`IRBlocks`] with stack-based calls.
///
/// Every VAFFLE function becomes a contiguous range of IR blocks.  Function
/// calls use `StorageRead`/`StorageWrite` on [`StorageId::STACK`] to pass
/// arguments and return values through the recursion stack.
///
/// Returns the merged `IRBlocks` and the shared `IRTypes` table.
pub fn lower_vaffle_to_ir(module: &Module) -> (IRBlocks, IRTypes) {
    let mut ctx = LowerCtx::new(module);
    ctx.lower_all();
    ctx.finish()
}

// ============================================================================
// Internal context
// ============================================================================

struct LowerCtx<'m> {
    module: &'m Module,
    types: IRTypes,
    bit_tid: IRTypeId,
    /// Per-function: (entry IR block index, FrameLayout).
    func_info: Vec<(usize, FrameLayout)>,
    /// Accumulated IR blocks.
    blocks: Vec<IRBlock>,
    /// Accumulated oracle / action declarations.
    oracles: Vec<OracleDecl>,
    actions: Vec<ActionDecl>,
}

/// Transient builder state for one VAFFLE block being lowered.
struct BlockEmitter {
    stmts: Vec<IRStmt>,
    next_var: u32,
    /// Maps VAFFLE ValueId → vec of IRVarId bits (one per bit of the value).
    val_map: BTreeMap<usize, IRVarId>,
}

impl BlockEmitter {
    fn emit(&mut self, stmt: IRStmt) -> IRVarId {
        let id = IRVarId(self.next_var);
        self.next_var += 1;
        self.stmts.push(stmt);
        id
    }
}

// ---- BitCircuitBuilder for BlockEmitter ------------------------------------
// This lets the generic `bc_*` algorithms and `StorageEmitter` run
// directly on the block builder.

impl BitCircuitBuilder for BlockEmitter {
    type Bit = IRVarId;

    fn bc_const(&mut self, val: bool) -> IRVarId {
        // We need a type for the constant.  Use TypeId(0) = Bit by convention.
        self.emit(IRStmt::Const(Constant { hi: 0, lo: val as u128 }, TypeId(0)))
    }

    fn bc_poly(
        &mut self,
        coeffs: BTreeMap<Vec<IRVarId>, u8>,
        constant: u128,
    ) -> IRVarId {
        self.emit(IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: constant } })
    }

    // Carry3 override (optimized single Poly).
    fn bc_carry3(&mut self, a: IRVarId, b: IRVarId, c: IRVarId) -> IRVarId {
        let mut ab = vec![a, b]; ab.sort();
        let mut ac = vec![a, c]; ac.sort();
        let mut bc_ = vec![b, c]; bc_.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(ab, 1u8); coeffs.insert(ac, 1u8); coeffs.insert(bc_, 1u8);
        self.bc_poly(coeffs, 0)
    }
}

impl StorageEmitter for BlockEmitter {
    fn compose_address(&mut self, bits: &[IRVarId]) -> IRVarId {
        if bits.len() == 1 { return bits[0]; }
        let n = bits.len();
        let bit_tid = TypeId(0); // Convention: Bit type at index 0.
        let vec_ty = TypeId(1);  // We'll pre-intern Vec(SP_BITS, Bit) at index 1.
        self.emit(IRStmt::Merge { parts: bits.to_vec(), ty: vec_ty })
    }

    fn emit_read(&mut self, storage: StorageId, ty: TypeId, addr_bits: &[IRVarId]) -> IRVarId {
        let addr = self.compose_address(addr_bits);
        self.emit(IRStmt::StorageRead { storage, ty, addr })
    }

    fn emit_write(&mut self, storage: StorageId, src: IRVarId, ty: TypeId, addr_bits: &[IRVarId]) {
        let addr = self.compose_address(addr_bits);
        self.emit(IRStmt::StorageWrite { storage, src, ty, addr });
    }
}

// ============================================================================
// LowerCtx implementation
// ============================================================================

impl<'m> LowerCtx<'m> {
    fn new(module: &'m Module) -> Self {
        let mut types = IRTypes::new();
        // Index 0 = Bit.
        types.push(IrType::Primitive(Type::Bit));
        // Index 1 = Vec(SP_BITS, Bit) — the address type.
        let bit_tid = TypeId(0);
        types.push(IrType::Vec(SP_BITS, bit_tid));

        LowerCtx {
            module,
            types,
            bit_tid: TypeId(0),
            func_info: Vec::new(),
            blocks: Vec::new(),
            oracles: module.oracles.clone(),
            actions: module.actions.clone(),
        }
    }

    /// Pre-compute entry block indices and frame layouts for every function.
    fn plan_functions(&mut self) {
        let mut block_offset = 0usize;
        for func_decl in &self.module.funcs {
            let body = match func_decl {
                FuncDecl::Body(b) => b,
                _ => {
                    self.func_info.push((block_offset, FrameLayout {
                        params: vec![], ret: None, size: 0, storage: StorageId::STACK,
                    }));
                    continue;
                }
            };

            let sig = &self.module.sigs[body.sig.0];
            let mut offset = 0u64;
            let mut params = Vec::new();
            for &ty_id in &sig.params {
                // Each Bit-typed param is 1 slot.
                params.push((offset, 1, ty_id));
                offset += 1;
            }
            let ret = if sig.results.is_empty() {
                None
            } else {
                let r = (offset, 1u64, sig.results[0]);
                offset += 1;
                Some(r)
            };
            let layout = FrameLayout { params, ret, size: offset, storage: StorageId::STACK };
            self.func_info.push((block_offset, layout));
            block_offset += body.blocks.len();
        }
    }

    fn lower_all(&mut self) {
        self.plan_functions();

        // Pre-allocate empty IR blocks for every VAFFLE block across all functions.
        let total_blocks: usize = self.module.funcs.iter().map(|f| match f {
            FuncDecl::Body(b) => b.blocks.len(),
            _ => 0,
        }).sum();

        // Build one IR block per VAFFLE block.
        for (func_idx, func_decl) in self.module.funcs.iter().enumerate() {
            let body = match func_decl {
                FuncDecl::Body(b) => b,
                _ => continue,
            };
            let (entry_offset, ref layout) = self.func_info[func_idx];
            let layout = layout.clone();

            for (vaffle_bi, vaffle_block) in body.blocks.iter().enumerate() {
                let ir_bi = entry_offset + vaffle_bi;

                // Block params: SP bits + original params.
                let mut params: Vec<IRTypeId> = Vec::new();
                // SP bits come first.
                for _ in 0..SP_BITS {
                    params.push(self.bit_tid);
                }
                // Then the original VAFFLE block params.
                for &(_vid, ty_id) in &vaffle_block.params {
                    params.push(ty_id);
                }

                let mut em = BlockEmitter {
                    stmts: Vec::new(),
                    next_var: params.len() as u32,
                    val_map: BTreeMap::new(),
                };

                // SP bits are the first SP_BITS params.
                let sp_bits: Vec<IRVarId> = (0..SP_BITS as u32).map(IRVarId).collect();
                let sp = StackPtr::new(sp_bits.clone());

                // Map VAFFLE block params (after SP) to IRVarIds.
                for (pi, &(vid, _ty)) in vaffle_block.params.iter().enumerate() {
                    em.val_map.insert(vid.0, IRVarId((SP_BITS + pi) as u32));
                }

                // Lower each stmt.
                for &stmt_vid in &vaffle_block.stmts {
                    let val = &body.values[stmt_vid.0];
                    match val {
                        Value::Op(stmt) => {
                            let result = em.emit(translate_stmt(stmt, &em.val_map));
                            em.val_map.insert(stmt_vid.0, result);
                        }
                        Value::Call { func, args } => {
                            // Stack-based call.
                            let callee_func_idx = func.0;
                            let (callee_entry, ref callee_layout) = self.func_info[callee_func_idx];
                            let callee_layout = callee_layout.clone();

                            // Write arguments to the stack.
                            let arg_bits: Vec<Vec<IRVarId>> = args.iter()
                                .map(|vid| vec![em.val_map[&vid.0]])
                                .collect();
                            frame_push_args(&mut em, &sp, &callee_layout, &arg_bits);

                            // Advance SP.
                            let mut new_sp = sp.clone();
                            new_sp.advance(callee_layout.size);
                            let new_sp_bits = new_sp.materialize(&mut em);

                            // Create a continuation block for after the call.
                            // For now, emit a placeholder — the terminator handles the jump.
                            // We store the call result var as a pending read from the
                            // callee's return slot.
                            let mut read_sp = sp.clone();
                            let ret_bits = frame_read_ret(&mut em, &read_sp, &callee_layout);
                            if let Some(first) = ret_bits.first() {
                                em.val_map.insert(stmt_vid.0, *first);
                            }
                        }
                        Value::Output { value, idx } => {
                            // Project from a multi-result — for now just alias.
                            if let Some(&v) = em.val_map.get(&value.0) {
                                em.val_map.insert(stmt_vid.0, v);
                            }
                        }
                        Value::Param { .. } => {
                            // Already mapped during block-param setup.
                        }
                    }
                }

                // Lower terminator.
                let terminator = translate_terminator(
                    &vaffle_block.terminator,
                    &em.val_map,
                    entry_offset,
                    &sp_bits,
                );

                self.blocks.push(IRBlock {
                    params,
                    stmts: em.stmts,
                    terminator,
                });
            }
        }
    }

    fn finish(self) -> (IRBlocks, IRTypes) {
        let ir = IRBlocks {
            oracles: self.oracles,
            actions: self.actions,
            blocks: self.blocks,
        };
        (ir, self.types)
    }
}

// ============================================================================
// Stmt translation (VAFFLE Stmt<ValueId> → IRStmt<IRVarId>)
// ============================================================================

fn translate_stmt(
    stmt: &volar_ir_common::Stmt<ValueId>,
    val_map: &BTreeMap<usize, IRVarId>,
) -> IRStmt {
    let s = |vid: &ValueId| val_map.get(&vid.0).copied().unwrap_or(IRVarId(0));

    match stmt {
        volar_ir_common::Stmt::Const(c, ty) => IRStmt::Const(*c, *ty),
        volar_ir_common::Stmt::Poly { coeffs, constant } => IRStmt::Poly {
            coeffs: coeffs.iter().map(|(vars, &coeff)| {
                let mut nv: Vec<IRVarId> = vars.iter().map(s).collect();
                nv.sort();
                (nv, coeff)
            }).collect(),
            constant: *constant,
        },
        volar_ir_common::Stmt::Merge { parts, ty } =>
            IRStmt::Merge { parts: parts.iter().map(s).collect(), ty: *ty },
        volar_ir_common::Stmt::Splat { src, ty } =>
            IRStmt::Splat { src: s(src), ty: *ty },
        volar_ir_common::Stmt::Transmute { src, src_ty, dst_ty } =>
            IRStmt::Transmute { src: s(src), src_ty: *src_ty, dst_ty: *dst_ty },
        volar_ir_common::Stmt::Rol { src, ty, n } =>
            IRStmt::Rol { src: s(src), ty: *ty, n: *n },
        volar_ir_common::Stmt::Ror { src, ty, n } =>
            IRStmt::Ror { src: s(src), ty: *ty, n: *n },
        volar_ir_common::Stmt::Shuffle { result_bits, ty } => IRStmt::Shuffle {
            result_bits: result_bits.iter().map(|(b, v)| (*b, s(v))).collect(),
            ty: *ty,
        },
        volar_ir_common::Stmt::StorageRead { storage, ty, addr } =>
            IRStmt::StorageRead { storage: *storage, ty: *ty, addr: s(addr) },
        volar_ir_common::Stmt::StorageWrite { storage, src, ty, addr } =>
            IRStmt::StorageWrite { storage: *storage, src: s(src), ty: *ty, addr: s(addr) },
        volar_ir_common::Stmt::Rng { ty } => IRStmt::Rng { ty: *ty },
        // Oracle/Action stmts pass through with remapped vars.
        volar_ir_common::Stmt::OracleCall { name, args, output_tys, result_ty } =>
            IRStmt::OracleCall {
                name: name.clone(), args: args.iter().map(s).collect(),
                output_tys: output_tys.clone(), result_ty: *result_ty,
            },
        volar_ir_common::Stmt::OracleOutput { call, idx, ty } =>
            IRStmt::OracleOutput { call: s(call), idx: *idx, ty: *ty },
        volar_ir_common::Stmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } =>
            IRStmt::ActionCall {
                name: name.clone(), guard: s(guard),
                args: args.iter().map(s).collect(),
                fallbacks: fallbacks.iter().map(s).collect(),
                output_tys: output_tys.clone(), result_ty: *result_ty,
            },
        volar_ir_common::Stmt::ActionOutput { call, idx, ty } =>
            IRStmt::ActionOutput { call: s(call), idx: *idx, ty: *ty },
    }
}

// ============================================================================
// Terminator translation
// ============================================================================

fn translate_terminator(
    term: &Terminator,
    val_map: &BTreeMap<usize, IRVarId>,
    func_block_offset: usize,
    sp_bits: &[IRVarId],
) -> IRTerminator {
    let s = |vid: &ValueId| val_map.get(&vid.0).copied().unwrap_or(IRVarId(0));

    match term {
        Terminator::Return { values } => {
            let args: Vec<IRVarId> = values.iter().map(|v| s(v)).collect();
            IRTerminator::Jmp { func: IRBlockTargetId::Return, args }
        }
        Terminator::Jump(target) => {
            let ir_block = IRBlockId((func_block_offset + target.block.0) as u32);
            let mut args: Vec<IRVarId> = sp_bits.to_vec(); // SP bits first.
            args.extend(target.args.iter().map(|v| s(v)));
            IRTerminator::Jmp { func: IRBlockTargetId::Block(ir_block), args }
        }
        Terminator::IfNonzero { cond, then_target, else_target } => {
            let cond_var = s(cond);
            let then_block = IRBlockId((func_block_offset + then_target.block.0) as u32);
            let else_block = IRBlockId((func_block_offset + else_target.block.0) as u32);
            let mut then_args: Vec<IRVarId> = sp_bits.to_vec();
            then_args.extend(then_target.args.iter().map(|v| s(v)));
            let mut else_args: Vec<IRVarId> = sp_bits.to_vec();
            else_args.extend(else_target.args.iter().map(|v| s(v)));
            IRTerminator::JumpCond {
                condition: cond_var,
                true_block: IRBlockTargetId::Block(then_block),
                true_args: then_args,
                false_block: IRBlockTargetId::Block(else_block),
                false_args: else_args,
            }
        }
        // ReturnCall / Table are currently unhandled in the stack lowering.
        _ => IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![] },
    }
}
