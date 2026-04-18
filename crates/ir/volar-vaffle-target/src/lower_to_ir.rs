// @reliability: experimental
// @ai: assisted
//! VAFFLE `Module` → Volar IR `IRBlocks` lowering with a Dyn-based recursion
//! stack.
//!
//! # Storage model
//!
//! Storages are keyed by `(StorageId, TypeId, addr)`.  The `Block`-typed
//! continuation lives in its own type lane at the same address as the
//! `Bit`-typed parameters — zero extra frame space.
//!
//! # Call protocol
//!
//! **Caller** at a `Value::Call` site:
//! 1. Spill all values defined so far to own frame's spill slots.
//! 2. Write callee's arguments into `STACK[sp + param_off]` (Bit lane).
//! 3. Write continuation block ref into `STACK[sp]` (Block lane).
//! 4. Advance SP by callee frame size.
//! 5. Jump to callee's entry block with new SP bits.
//!
//! **Callee** entry:
//! 1. Read arguments from `STACK[sp - frame_size + param_off]` (Bit lane).
//! 2. Execute body.
//!
//! **Callee** return:
//! 1. Read continuation from `STACK[sp - frame_size]` (Block lane).
//! 2. Retreat SP by frame size.
//! 3. `Dyn(continuation)` with args `[sp_bits…, ret_bits…]`.
//!
//! **Continuation** block (in caller):
//! 1. Receives `[sp_bits…, ret_bits…]` as block params.
//! 2. Reloads spilled values from own frame's spill slots.
//! 3. Continues with remaining stmts.

use alloc::{
    collections::BTreeMap,
    vec,
    vec::Vec,
};

use vaffle::{
    BlockId, FuncBody, FuncDecl, FuncId, Module, Terminator, Target, Value, ValueId,
};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator,
    IRTypeId, IRTypes, IRVarId, OracleDecl, ActionDecl,
};
use volar_ir_common::{Constant, IrType, StorageId, Type, TypeId};
use volar_lir::circuits::{
    bc_add, FrameLayout, StackPtr,
    frame_push_args, frame_read_args, frame_write_cont, frame_read_cont,
    frame_write_ret, frame_spill, frame_reload,
    BitCircuitBuilder, StorageEmitter,
};

/// Width of the stack-pointer address in bits.
pub const SP_BITS: usize = 16;

// ============================================================================
// Public entry point
// ============================================================================

/// Lower a VAFFLE [`Module`] into a single [`IRBlocks`] with a Dyn-based
/// recursion stack.
///
/// Every VAFFLE function becomes a contiguous range of IR blocks (possibly
/// expanded by call-site splitting).  The first function in the module is
/// treated as the entry point.
pub fn lower_vaffle_to_ir(module: &Module) -> (IRBlocks, IRTypes) {
    let mut ctx = LowerCtx::new(module);
    ctx.lower_all();
    ctx.finish()
}

// ============================================================================
// Well-known type IDs (indices into the type table)
// ============================================================================

const BIT_TID: TypeId = TypeId(0);
const ADDR_TID: TypeId = TypeId(1); // Vec(SP_BITS, Bit)

// ============================================================================
// BlockEmitter — implements BitCircuitBuilder + StorageEmitter
// ============================================================================

struct BlockEmitter {
    params: Vec<IRTypeId>,
    stmts: Vec<IRStmt>,
    next_var: u32,
}

impl BlockEmitter {
    fn new(params: Vec<IRTypeId>) -> Self {
        let next_var = params.len() as u32;
        BlockEmitter { params, stmts: Vec::new(), next_var }
    }
    fn emit(&mut self, stmt: IRStmt) -> IRVarId {
        let id = IRVarId(self.next_var);
        self.next_var += 1;
        self.stmts.push(stmt);
        id
    }
    fn finish(self, terminator: IRTerminator) -> IRBlock {
        IRBlock { params: self.params, stmts: self.stmts, stmt_provs: vec![], terminator }
    }
}

impl BitCircuitBuilder for BlockEmitter {
    type Bit = IRVarId;
    fn bc_const(&mut self, val: bool) -> IRVarId {
        self.emit(IRStmt::Const(Constant { hi: 0, lo: val as u128 }, BIT_TID))
    }
    fn bc_poly(&mut self, coeffs: BTreeMap<Vec<IRVarId>, u8>, constant: u128) -> IRVarId {
        self.emit(IRStmt::Poly { coeffs, constant: Constant { hi: 0, lo: constant } })
    }
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
        self.emit(IRStmt::Merge { parts: bits.to_vec(), ty: ADDR_TID })
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
// LowerCtx
// ============================================================================

struct FuncInfo {
    /// Index of the function's entry block in the global blocks array.
    entry_block: usize,
    /// Frame layout for calls *into* this function.
    callee_layout: FrameLayout,
    /// Frame layout of this function's own frame (for spilling).
    own_layout: FrameLayout,
}

struct LowerCtx<'m> {
    module: &'m Module,
    types: IRTypes,
    func_info: Vec<FuncInfo>,
    blocks: Vec<IRBlock>,
    oracles: Vec<OracleDecl>,
    actions: Vec<ActionDecl>,
    /// Extra blocks generated by call-site splitting (appended after all
    /// function blocks).  Each entry is `(block_def)`.
    extra_blocks: Vec<IRBlock>,
}

impl<'m> LowerCtx<'m> {
    fn new(module: &'m Module) -> Self {
        let mut types = IRTypes::new();
        types.push(IrType::Primitive(Type::Bit));           // index 0
        types.push(IrType::Vec(SP_BITS, BIT_TID));          // index 1
        LowerCtx {
            module, types,
            func_info: Vec::new(),
            blocks: Vec::new(),
            oracles: module.oracles.clone(),
            actions: module.actions.clone(),
            extra_blocks: Vec::new(),
        }
    }

    /// Intern a Block type for a continuation that receives `[sp_bits, ret_bits]`.
    fn intern_cont_block_type(&mut self, n_ret_bits: usize) -> TypeId {
        let params = vec![BIT_TID; SP_BITS + n_ret_bits];
        self.types.intern(IrType::Block { params })
    }

    // ---- Planning ----------------------------------------------------------

    fn plan_functions(&mut self) {
        // Reserve block 0 for the module entry; block 1 for the exit continuation.
        let mut block_offset = 2usize;

        for func_decl in &self.module.funcs {
            let body = match func_decl {
                FuncDecl::Body(b) => b,
                _ => {
                    self.func_info.push(FuncInfo {
                        entry_block: block_offset,
                        callee_layout: FrameLayout {
                            params: vec![], ret: None, cont_ty: None,
                            spill_base: 0, n_spill: 0, size: 0,
                            storage: StorageId::STACK,
                        },
                        own_layout: FrameLayout {
                            params: vec![], ret: None, cont_ty: None,
                            spill_base: 0, n_spill: 0, size: 0,
                            storage: StorageId::STACK,
                        },
                    });
                    continue;
                }
            };

            let sig = &self.module.sigs[body.sig.0];
            let n_params = sig.params.len();
            let n_ret = if sig.results.is_empty() { 0 } else { 1 };
            let n_values = body.values.len();

            // Callee layout: what the caller pushes for this function.
            //   Bit-lane: params[0..n_params], ret[n_params..n_params+n_ret]
            //   Block-lane: continuation at addr sp (free).
            let mut offset = 0u64;
            let params: Vec<(u64, u64, TypeId)> = sig.params.iter().map(|&ty_id| {
                let o = offset; offset += 1; (o, 1, ty_id)
            }).collect();
            let ret = if n_ret > 0 {
                let o = offset; offset += 1;
                Some((o, 1u64, sig.results[0]))
            } else { None };
            let cont_ty = Some(self.intern_cont_block_type(n_ret));
            let callee_size = offset; // Bit-lane frame size.

            let callee_layout = FrameLayout {
                params, ret, cont_ty,
                spill_base: 0, n_spill: 0,
                size: callee_size, storage: StorageId::STACK,
            };

            // Own layout: this function's spill slots (appended after the
            // callee-visible slots in the same frame region).
            let spill_base = callee_size;
            let n_spill = n_values as u64;
            let own_size = callee_size + n_spill;

            let own_layout = FrameLayout {
                params: callee_layout.params.clone(),
                ret: callee_layout.ret,
                cont_ty,
                spill_base, n_spill,
                size: own_size,
                storage: StorageId::STACK,
            };

            self.func_info.push(FuncInfo {
                entry_block: block_offset,
                callee_layout,
                own_layout,
            });
            block_offset += body.blocks.len();
        }
    }

    // ---- Lowering ----------------------------------------------------------

    fn lower_all(&mut self) {
        self.plan_functions();

        // Block 0: module entry — SP=0, jump to first function's entry.
        // Block 1: exit continuation — receives [SP, ret], emits Return.
        self.emit_entry_and_exit();

        for (func_idx, func_decl) in self.module.funcs.iter().enumerate() {
            let body = match func_decl {
                FuncDecl::Body(b) => b,
                _ => continue,
            };
            self.lower_function(func_idx, body);
        }

        // Append extra blocks (continuations created by call splitting).
        self.blocks.append(&mut self.extra_blocks);
    }

    fn emit_entry_and_exit(&mut self) {
        // Block 0: entry.  Params: none.  Body: SP = const 0.  Jump to func 0.
        let mut em = BlockEmitter::new(vec![]);
        let sp = StackPtr::<IRVarId>::from_const(&mut em, 0, SP_BITS);

        if self.func_info.is_empty() {
            // No functions — just return.
            self.blocks.push(em.finish(IRTerminator::Jmp {
                func: IRBlockTargetId::Return, args: vec![],
            }));
            // Dummy exit block.
            self.blocks.push(IRBlock {
                params: vec![], stmts: vec![], stmt_provs: vec![],
                terminator: IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![] },
            });
            return;
        }

        let info = &self.func_info[0];
        let callee_layout = &info.callee_layout;

        // Write the exit continuation (block 1) into the callee's Block lane.
        let exit_block_idx = 1u32;
        let cont_ty = callee_layout.cont_ty.unwrap_or(BIT_TID);
        let cont_var = em.emit(IRStmt::Const(
            Constant { hi: 0, lo: exit_block_idx as u128 }, cont_ty,
        ));
        frame_write_cont(&mut em, &sp, callee_layout, cont_var);

        // Advance SP by callee's frame size.
        let mut new_sp = sp.clone();
        new_sp.advance(callee_layout.size);
        let new_sp_bits = new_sp.materialize(&mut em);

        // Jump to function 0's entry.
        let entry_target = IRBlockId(info.entry_block as u32);
        let mut args = new_sp_bits;
        self.blocks.push(em.finish(IRTerminator::Jmp {
            func: IRBlockTargetId::Block(entry_target), args,
        }));

        // Block 1: exit continuation.  Params: [SP bits, ret bits].
        let n_ret = self.func_info[0].callee_layout.ret.map_or(0, |(_, c, _)| c as usize);
        let mut exit_params: Vec<IRTypeId> = vec![BIT_TID; SP_BITS + n_ret];
        let ret_args: Vec<IRVarId> = (SP_BITS as u32 .. (SP_BITS + n_ret) as u32)
            .map(IRVarId).collect();
        self.blocks.push(IRBlock {
            params: exit_params,
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return, args: ret_args,
            },
        });
    }

    fn lower_function(&mut self, func_idx: usize, body: &FuncBody) {
        let info = &self.func_info[func_idx];
        let entry_block_offset = info.entry_block;
        let own_layout = info.own_layout.clone();
        let callee_layout = info.callee_layout.clone();

        for (vaffle_bi, vaffle_block) in body.blocks.iter().enumerate() {
            let ir_bi = entry_block_offset + vaffle_bi;
            let is_entry = vaffle_bi == body.entry.0;

            // Every block takes SP bits as the first params.
            let mut params: Vec<IRTypeId> = vec![BIT_TID; SP_BITS];
            // Non-entry blocks also take the original VAFFLE block params.
            if !is_entry {
                for &(_vid, ty_id) in &vaffle_block.params {
                    params.push(ty_id);
                }
            }

            let mut em = BlockEmitter::new(params);
            let sp_bits: Vec<IRVarId> = (0..SP_BITS as u32).map(IRVarId).collect();

            // SP points past the frame.  Our frame base is SP - own_layout.size.
            let mut frame_sp = StackPtr::new(sp_bits.clone());
            frame_sp.retreat(own_layout.size);

            // Value map: VAFFLE ValueId → IRVarId.
            let mut val_map: BTreeMap<usize, IRVarId> = BTreeMap::new();

            // Entry block: read arguments from the frame.
            if is_entry {
                let arg_bits = frame_read_args(&mut em, &frame_sp, &callee_layout);
                for (pi, bits) in arg_bits.iter().enumerate() {
                    if let Some(first) = bits.first() {
                        // Map the VAFFLE param ValueId.
                        if pi < vaffle_block.params.len() {
                            val_map.insert(vaffle_block.params[pi].0.0, *first);
                        }
                    }
                }
            } else {
                // Non-entry: map block params (after SP).
                for (pi, &(vid, _ty)) in vaffle_block.params.iter().enumerate() {
                    val_map.insert(vid.0, IRVarId((SP_BITS + pi) as u32));
                }
            }

            // Process stmts.  When we hit a Call, split the block.
            let mut remaining_stmts: &[ValueId] = &vaffle_block.stmts;
            let mut current_em = em;
            let mut current_sp_bits = sp_bits.clone();
            let mut current_frame_sp = frame_sp.clone();

            while !remaining_stmts.is_empty() {
                let (before_call, at_call, after_call) = find_call(remaining_stmts, body);

                // Emit stmts before the call.
                for &svid in before_call {
                    if let Value::Op(stmt) = &body.values[svid.0] {
                        let ir_stmt = translate_stmt(stmt, &val_map);
                        let id = current_em.emit(ir_stmt);
                        val_map.insert(svid.0, id);
                    }
                }

                match at_call {
                    Some(call_vid) => {
                        if let Value::Call { func: callee_fid, args: call_args } = &body.values[call_vid.0] {
                            let callee_idx = callee_fid.0;
                            let callee_info = &self.func_info[callee_idx];
                            let cl = callee_info.callee_layout.clone();

                            // 1. Spill all defined values.
                            for (&vid_key, &ir_var) in &val_map {
                                frame_spill(&mut current_em, &current_frame_sp,
                                    &own_layout, vid_key as u64, ir_var, BIT_TID);
                            }

                            // 2. Write callee args.
                            let arg_bits: Vec<Vec<IRVarId>> = call_args.iter()
                                .map(|vid| vec![val_map[&vid.0]])
                                .collect();
                            // Write to SP (top of stack = base of callee's frame).
                            let callee_frame_sp = StackPtr::new(current_sp_bits.clone());
                            frame_push_args(&mut current_em, &callee_frame_sp, &cl, &arg_bits);

                            // 3. Write continuation.
                            let n_ret = cl.ret.map_or(0, |(_, c, _)| c as usize);
                            let cont_block_idx = self.blocks.len() + self.extra_blocks.len() + 1; // +1 for current block
                            // We'll create the continuation block as an extra block.
                            let cont_ir_idx = cont_block_idx as u32;
                            let cont_ty = cl.cont_ty.unwrap_or(BIT_TID);
                            let cont_var = current_em.emit(IRStmt::Const(
                                Constant { hi: 0, lo: cont_ir_idx as u128 }, cont_ty,
                            ));
                            frame_write_cont(&mut current_em, &callee_frame_sp, &cl, cont_var);

                            // 4. Advance SP.
                            let mut new_sp = StackPtr::new(current_sp_bits.clone());
                            new_sp.advance(cl.size);
                            let new_sp_bits = new_sp.materialize(&mut current_em);

                            // 5. Jump to callee entry.
                            let callee_entry = IRBlockId(callee_info.entry_block as u32);
                            let block = current_em.finish(IRTerminator::Jmp {
                                func: IRBlockTargetId::Block(callee_entry),
                                args: new_sp_bits,
                            });
                            // If this is the original block, push to self.blocks at the right index.
                            if self.blocks.len() == ir_bi {
                                self.blocks.push(block);
                            } else {
                                self.extra_blocks.push(block);
                            }

                            // 6. Create continuation block.
                            let mut cont_params: Vec<IRTypeId> = vec![BIT_TID; SP_BITS + n_ret];
                            let mut cont_em = BlockEmitter::new(cont_params);
                            let cont_sp_bits: Vec<IRVarId> = (0..SP_BITS as u32).map(IRVarId).collect();

                            // Map return value.
                            if n_ret > 0 {
                                let ret_var = IRVarId(SP_BITS as u32); // first param after SP
                                val_map.insert(call_vid.0, ret_var);
                            }

                            // Reload spilled values.
                            let mut cont_frame_sp = StackPtr::new(cont_sp_bits.clone());
                            cont_frame_sp.retreat(own_layout.size);
                            let keys: Vec<usize> = val_map.keys().copied().collect();
                            for vid_key in keys {
                                if vid_key == call_vid.0 { continue; } // ret already a param
                                let reloaded = frame_reload(&mut cont_em, &cont_frame_sp,
                                    &own_layout, vid_key as u64, BIT_TID);
                                val_map.insert(vid_key, reloaded);
                            }

                            current_em = cont_em;
                            current_sp_bits = cont_sp_bits;
                            current_frame_sp = StackPtr::new(current_sp_bits.clone());
                            current_frame_sp.retreat(own_layout.size);
                        }
                        remaining_stmts = after_call;
                    }
                    None => {
                        remaining_stmts = &[];
                    }
                }
            }

            // If no call was encountered, emit remaining non-call stmts.
            // (Already handled above if there were calls.)

            // Terminator.
            let terminator = self.translate_terminator(
                &vaffle_block.terminator, &val_map, func_idx,
                &current_sp_bits, &current_frame_sp, &own_layout,
                &mut current_em,
            );

            let block = current_em.finish(terminator);
            if self.blocks.len() == ir_bi {
                self.blocks.push(block);
            } else {
                self.extra_blocks.push(block);
            }
        }
    }

    fn translate_terminator(
        &self,
        term: &Terminator,
        val_map: &BTreeMap<usize, IRVarId>,
        func_idx: usize,
        sp_bits: &[IRVarId],
        frame_sp: &StackPtr<IRVarId>,
        own_layout: &FrameLayout,
        em: &mut BlockEmitter,
    ) -> IRTerminator {
        let s = |vid: &ValueId| val_map.get(&vid.0).copied().unwrap_or(IRVarId(0));
        let entry_off = self.func_info[func_idx].entry_block;

        match term {
            Terminator::Return { values } => {
                // Write return value to frame's ret slot.
                let ret_bits: Vec<IRVarId> = values.iter().map(|v| s(v)).collect();
                frame_write_ret(em, frame_sp, own_layout, &ret_bits);

                // Read continuation from Block lane.
                let cont_var = frame_read_cont(em, frame_sp, own_layout)
                    .expect("return without continuation");

                // Retreat SP by own frame size.
                let mut retreated_sp = StackPtr::new(sp_bits.to_vec());
                retreated_sp.retreat(own_layout.size);
                let retreated_bits = retreated_sp.materialize(em);

                // Read back the return value (to pass as Dyn args).
                // Actually, we already have the ret_bits — just pass them.
                let mut dyn_args = retreated_bits;
                dyn_args.extend(ret_bits);

                IRTerminator::Jmp {
                    func: IRBlockTargetId::Dyn(cont_var),
                    args: dyn_args,
                }
            }
            Terminator::Jump(target) => {
                let ir_block = IRBlockId((entry_off + target.block.0) as u32);
                let mut args: Vec<IRVarId> = sp_bits.to_vec();
                args.extend(target.args.iter().map(|v| s(v)));
                IRTerminator::Jmp { func: IRBlockTargetId::Block(ir_block), args }
            }
            Terminator::IfNonzero { cond, then_target, else_target } => {
                let then_block = IRBlockId((entry_off + then_target.block.0) as u32);
                let else_block = IRBlockId((entry_off + else_target.block.0) as u32);
                let mut then_args: Vec<IRVarId> = sp_bits.to_vec();
                then_args.extend(then_target.args.iter().map(|v| s(v)));
                let mut else_args: Vec<IRVarId> = sp_bits.to_vec();
                else_args.extend(else_target.args.iter().map(|v| s(v)));
                IRTerminator::JumpCond {
                    condition: s(cond),
                    true_block: IRBlockTargetId::Block(then_block),
                    true_args: then_args,
                    false_block: IRBlockTargetId::Block(else_block),
                    false_args: else_args,
                }
            }
            _ => IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![] },
        }
    }

    fn finish(self) -> (IRBlocks, IRTypes) {
        (IRBlocks {
            oracles: self.oracles,
            actions: self.actions,
            rngs: alloc::vec![],
            blocks: self.blocks,
        }, self.types)
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Find the first `Value::Call` in `stmts`, returning (before, Some(call), after).
/// If no call, returns (stmts, None, &[]).
fn find_call<'a>(
    stmts: &'a [ValueId],
    body: &FuncBody,
) -> (&'a [ValueId], Option<ValueId>, &'a [ValueId]) {
    for (i, &svid) in stmts.iter().enumerate() {
        if matches!(&body.values[svid.0], Value::Call { .. }) {
            return (&stmts[..i], Some(svid), &stmts[i + 1..]);
        }
    }
    (stmts, None, &[])
}

/// Translate a VAFFLE `Stmt<ValueId>` to an `IRStmt<IRVarId>` using `val_map`.
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
        volar_ir_common::Stmt::Shuffle { result_bits, ty } =>
            IRStmt::Shuffle { result_bits: result_bits.iter().map(|(b, v)| (*b, s(v))).collect(), ty: *ty },
        volar_ir_common::Stmt::StorageRead { storage, ty, addr } =>
            IRStmt::StorageRead { storage: *storage, ty: *ty, addr: s(addr) },
        volar_ir_common::Stmt::StorageWrite { storage, src, ty, addr } =>
            IRStmt::StorageWrite { storage: *storage, src: s(src), ty: *ty, addr: s(addr) },
        volar_ir_common::Stmt::Rng { name, ty } => IRStmt::Rng { name: name.clone(), ty: *ty },
        volar_ir_common::Stmt::OracleCall { name, args, output_tys, result_ty } =>
            IRStmt::OracleCall { name: name.clone(), args: args.iter().map(s).collect(),
                output_tys: output_tys.clone(), result_ty: *result_ty },
        volar_ir_common::Stmt::OracleOutput { call, idx, ty } =>
            IRStmt::OracleOutput { call: s(call), idx: *idx, ty: *ty },
        volar_ir_common::Stmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } =>
            IRStmt::ActionCall { name: name.clone(), guard: s(guard),
                args: args.iter().map(s).collect(), fallbacks: fallbacks.iter().map(s).collect(),
                output_tys: output_tys.clone(), result_ty: *result_ty },
        volar_ir_common::Stmt::ActionOutput { call, idx, ty } =>
            IRStmt::ActionOutput { call: s(call), idx: *idx, ty: *ty },
    }
}
