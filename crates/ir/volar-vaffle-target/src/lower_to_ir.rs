// @reliability: experimental
// @ai: assisted
//! VAFFLE `Module` → Volar IR `IRBlocks` lowering with a Dyn-based recursion
//! stack.
//!
//! # Bit packing
//!
//! Individual GF(2) bits are packed into `Vec(PACK_W, Bit)` words (default
//! `PACK_W = 64`) at every boundary:
//!
//! * **Block parameters**: `ceil(SP_BITS / PACK_W)` packed words instead of
//!   `SP_BITS` individual `Bit` params.
//! * **Spill / reload**: `ceil(N / PACK_W)` `StorageWrite` / `StorageRead`
//!   ops instead of `N`.
//! * **Frame arguments / return**: packed word slots.
//! * **Jump / Dyn arguments**: packed SP + values.
//!
//! Packing is performed via `Merge` (pack) and `Shuffle` (unpack).
//! Computation within a block operates on individual `Bit`-typed vars as
//! before — only boundary crossings use packed words.
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

/// Number of bits packed into a single `Vec(PACK_W, Bit)` word at block
/// boundaries, spill/reload slots, and frame argument slots.
///
/// Choosing 64 means that a 128-bit parameter occupies 2 packed block
/// params instead of 128 individual `Bit` params, and spilling 200
/// values costs 4 storage ops instead of 200.
pub const PACK_W: usize = 64;

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
/// `Vec(PACK_W, Bit)` — the packed word type.  Index 2 in the type table.
const PACK_TID: TypeId = TypeId(2);

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

    // ---- Packing helpers ---------------------------------------------------

    /// Pack `bits` into `ceil(bits.len() / PACK_W)` words of type `PACK_TID`.
    ///
    /// Short final words are zero-padded.
    fn pack_bits(&mut self, bits: &[IRVarId]) -> Vec<IRVarId> {
        bits.chunks(PACK_W)
            .map(|chunk| {
                let mut parts: Vec<IRVarId> = chunk.to_vec();
                while parts.len() < PACK_W {
                    parts.push(self.bc_const(false));
                }
                self.emit(IRStmt::Merge { parts, ty: PACK_TID })
            })
            .collect()
    }

    /// Unpack `words` (each `Vec(PACK_W, Bit)`) back into `n` individual
    /// `Bit`-typed vars, discarding padding.
    fn unpack_words(&mut self, words: &[IRVarId], n: usize) -> Vec<IRVarId> {
        let mut out = Vec::with_capacity(n);
        for (wi, &word) in words.iter().enumerate() {
            let base = wi * PACK_W;
            let end = (base + PACK_W).min(n);
            for bit_j in base..end {
                let local_j = (bit_j - base) as u8;
                let v = self.emit(IRStmt::Shuffle {
                    result_bits: vec![(local_j, word)],
                    ty: BIT_TID,
                });
                out.push(v);
            }
        }
        out
    }

    /// Number of packed words needed for `n` bits.
    fn n_packs(n: usize) -> usize {
        (n + PACK_W - 1) / PACK_W
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
        types.push(IrType::Primitive(Type::Bit));           // index 0 = BIT_TID
        types.push(IrType::Vec(SP_BITS, BIT_TID));          // index 1 = ADDR_TID
        types.push(IrType::Vec(PACK_W, BIT_TID));           // index 2 = PACK_TID
        LowerCtx {
            module, types,
            func_info: Vec::new(),
            blocks: Vec::new(),
            oracles: module.oracles.clone(),
            actions: module.actions.clone(),
            extra_blocks: Vec::new(),
        }
    }

    /// Intern a Block type for a continuation that receives packed
    /// `[sp_words…, ret_words…]`.
    fn intern_cont_block_type(&mut self, n_ret_bits: usize) -> TypeId {
        let sp_words = BlockEmitter::n_packs(SP_BITS);
        let ret_words = BlockEmitter::n_packs(n_ret_bits);
        let params = vec![PACK_TID; sp_words + ret_words];
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
            //   Packed lane: params packed into ceil(n_params/PACK_W) words,
            //                ret packed into ceil(n_ret/PACK_W) words.
            //   Block-lane: continuation at addr sp (free).
            let n_param_words = BlockEmitter::n_packs(n_params);
            let n_ret_words = BlockEmitter::n_packs(n_ret);

            let mut offset = 0u64;
            let params: Vec<(u64, u64, TypeId)> = (0..n_param_words).map(|_| {
                let o = offset; offset += 1; (o, 1, PACK_TID)
            }).collect();
            let ret = if n_ret > 0 {
                let o = offset; offset += n_ret_words as u64;
                Some((o, n_ret_words as u64, PACK_TID))
            } else { None };
            let cont_ty = Some(self.intern_cont_block_type(n_ret));
            let callee_size = offset;

            let callee_layout = FrameLayout {
                params, ret, cont_ty,
                spill_base: 0, n_spill: 0,
                size: callee_size, storage: StorageId::STACK,
            };

            // Own layout: this function's spill slots (appended after the
            // callee-visible slots in the same frame region).
            // Spill is also packed: ceil(n_values / PACK_W) packed words.
            let n_spill_words = BlockEmitter::n_packs(n_values) as u64;
            let spill_base = callee_size;
            let own_size = callee_size + n_spill_words;

            let own_layout = FrameLayout {
                params: callee_layout.params.clone(),
                ret: callee_layout.ret,
                cont_ty,
                spill_base, n_spill: n_spill_words,
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
            self.blocks.push(em.finish(IRTerminator::Jmp {
                func: IRBlockTargetId::Return, args: vec![],
            }));
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

        // Pack SP bits into words for the jump.
        let sp_words = em.pack_bits(&new_sp_bits);

        let entry_target = IRBlockId(info.entry_block as u32);
        self.blocks.push(em.finish(IRTerminator::Jmp {
            func: IRBlockTargetId::Block(entry_target), args: sp_words,
        }));

        // Block 1: exit continuation.  Packed params: [sp_words, ret_words].
        let n_ret = self.func_info[0].callee_layout.ret.map_or(0, |(_, c, _)| c as usize);
        // n_ret is in packed words for the new layout; actual ret bits
        // were ceil(original_ret_bits / PACK_W) words, but the exit
        // continuation just forwards them.
        let sp_packs = BlockEmitter::n_packs(SP_BITS);
        let n_ret_bits = if n_ret > 0 { 1 } else { 0 }; // original ret is 1 bit
        let ret_packs = BlockEmitter::n_packs(n_ret_bits);
        let exit_params: Vec<IRTypeId> = vec![PACK_TID; sp_packs + ret_packs];
        let mut exit_em = BlockEmitter::new(exit_params);

        // Unpack return words to bits and return them.
        let ret_word_ids: Vec<IRVarId> = (sp_packs as u32 .. (sp_packs + ret_packs) as u32)
            .map(IRVarId).collect();
        let ret_bits = exit_em.unpack_words(&ret_word_ids, n_ret_bits);

        self.blocks.push(exit_em.finish(IRTerminator::Jmp {
            func: IRBlockTargetId::Return, args: ret_bits,
        }));
    }

    fn lower_function(&mut self, func_idx: usize, body: &FuncBody) {
        let info = &self.func_info[func_idx];
        let entry_block_offset = info.entry_block;
        let own_layout = info.own_layout.clone();
        let callee_layout = info.callee_layout.clone();

        let sp_packs = BlockEmitter::n_packs(SP_BITS);

        for (vaffle_bi, vaffle_block) in body.blocks.iter().enumerate() {
            let ir_bi = entry_block_offset + vaffle_bi;
            let is_entry = vaffle_bi == body.entry.0;

            // Every block takes SP as packed words.
            let mut params: Vec<IRTypeId> = vec![PACK_TID; sp_packs];
            // Non-entry blocks also take the original VAFFLE block params
            // (individual bits — these are internal and stay unpacked).
            if !is_entry {
                for &(_vid, ty_id) in &vaffle_block.params {
                    params.push(ty_id);
                }
            }

            let mut em = BlockEmitter::new(params);

            // Unpack SP from packed words.
            let sp_word_ids: Vec<IRVarId> = (0..sp_packs as u32).map(IRVarId).collect();
            let sp_bits: Vec<IRVarId> = em.unpack_words(&sp_word_ids, SP_BITS);

            let mut frame_sp = StackPtr::new(sp_bits.clone());
            frame_sp.retreat(own_layout.size);

            let mut val_map: BTreeMap<usize, IRVarId> = BTreeMap::new();

            // Entry block: read packed args from the frame, unpack.
            if is_entry {
                let arg_words = frame_read_args(&mut em, &frame_sp, &callee_layout);
                // arg_words: Vec<Vec<IRVarId>> — one inner vec per param slot,
                // each containing one packed word.
                // Flatten and unpack back to individual bits.
                let sig = &self.module.sigs[body.sig.0];
                let n_params = sig.params.len();
                let all_words: Vec<IRVarId> = arg_words.into_iter().flat_map(|w| w).collect();
                let all_bits = em.unpack_words(&all_words, n_params);
                for (pi, &bit) in all_bits.iter().enumerate() {
                    if pi < vaffle_block.params.len() {
                        val_map.insert(vaffle_block.params[pi].0.0, bit);
                    }
                }
            } else {
                // Non-entry: map block params (after packed SP words).
                for (pi, &(vid, _ty)) in vaffle_block.params.iter().enumerate() {
                    val_map.insert(vid.0, IRVarId((sp_packs + pi) as u32));
                }
            }

            let mut remaining_stmts: &[ValueId] = &vaffle_block.stmts;
            let mut current_em = em;
            let mut current_sp_bits = sp_bits.clone();
            let mut current_frame_sp = frame_sp.clone();

            while !remaining_stmts.is_empty() {
                let (before_call, at_call, after_call) = find_call(remaining_stmts, body);

                for &svid in before_call {
                    match &body.values[svid.0] {
                        Value::Op(stmt) => {
                            let ir_stmt = translate_stmt(stmt, &val_map);
                            let id = current_em.emit(ir_stmt);
                            val_map.insert(svid.0, id);
                        }
                        Value::StackAlloc { base_slot, .. } => {
                            let addr = current_em.emit(IRStmt::Const(
                                Constant { hi: 0, lo: *base_slot as u128 }, BIT_TID,
                            ));
                            val_map.insert(svid.0, addr);
                        }
                        Value::PtrLoad { ptr, .. } => {
                            let s = val_map.get(&ptr.0).copied().unwrap_or(IRVarId(0));
                            val_map.insert(svid.0, s);
                        }
                        Value::PtrStore { .. } => {}
                        Value::PtrOffset { idx, .. } => {
                            let s = val_map.get(&idx.0).copied().unwrap_or(IRVarId(0));
                            val_map.insert(svid.0, s);
                        }
                        _ => {}
                    }
                }

                match at_call {
                    Some(call_vid) => {
                        if let Value::Call { func: callee_fid, args: call_args } = &body.values[call_vid.0] {
                            let callee_idx = callee_fid.0;
                            let callee_info = &self.func_info[callee_idx];
                            let cl = callee_info.callee_layout.clone();

                            // 1. Packed spill: collect all defined values,
                            //    pack into words, write to spill slots.
                            let spill_keys: Vec<usize> = val_map.keys().copied().collect();
                            let spill_bits: Vec<IRVarId> = spill_keys.iter()
                                .map(|k| val_map[k])
                                .collect();
                            let spill_words = current_em.pack_bits(&spill_bits);
                            for (wi, &word) in spill_words.iter().enumerate() {
                                frame_spill(&mut current_em, &current_frame_sp,
                                    &own_layout, wi as u64, word, PACK_TID);
                            }

                            // 2. Write callee args (packed).
                            let arg_bits: Vec<IRVarId> = call_args.iter()
                                .map(|vid| val_map[&vid.0])
                                .collect();
                            let arg_words = current_em.pack_bits(&arg_bits);
                            let arg_word_vecs: Vec<Vec<IRVarId>> = arg_words.iter()
                                .map(|&w| vec![w])
                                .collect();
                            let callee_frame_sp = StackPtr::new(current_sp_bits.clone());
                            frame_push_args(&mut current_em, &callee_frame_sp, &cl, &arg_word_vecs);

                            // 3. Write continuation.
                            let n_ret = cl.ret.map_or(0, |(_, c, _)| c as usize);
                            let n_ret_bits_orig = if n_ret > 0 { 1 } else { 0 };
                            let cont_block_idx = self.blocks.len() + self.extra_blocks.len() + 1;
                            let cont_ir_idx = cont_block_idx as u32;
                            let cont_ty = cl.cont_ty.unwrap_or(BIT_TID);
                            let cont_var = current_em.emit(IRStmt::Const(
                                Constant { hi: 0, lo: cont_ir_idx as u128 }, cont_ty,
                            ));
                            frame_write_cont(&mut current_em, &callee_frame_sp, &cl, cont_var);

                            // 4. Advance SP, pack, jump.
                            let mut new_sp = StackPtr::new(current_sp_bits.clone());
                            new_sp.advance(cl.size);
                            let new_sp_bits = new_sp.materialize(&mut current_em);
                            let sp_words = current_em.pack_bits(&new_sp_bits);

                            let callee_entry = IRBlockId(callee_info.entry_block as u32);
                            let block = current_em.finish(IRTerminator::Jmp {
                                func: IRBlockTargetId::Block(callee_entry),
                                args: sp_words,
                            });
                            if self.blocks.len() == ir_bi {
                                self.blocks.push(block);
                            } else {
                                self.extra_blocks.push(block);
                            }

                            // 5. Create continuation block.
                            //    Params: [packed_sp_words, packed_ret_words].
                            let ret_packs = BlockEmitter::n_packs(n_ret_bits_orig);
                            let cont_params: Vec<IRTypeId> = vec![PACK_TID; sp_packs + ret_packs];
                            let mut cont_em = BlockEmitter::new(cont_params);

                            // Unpack SP.
                            let cont_sp_word_ids: Vec<IRVarId> = (0..sp_packs as u32).map(IRVarId).collect();
                            let cont_sp_bits = cont_em.unpack_words(&cont_sp_word_ids, SP_BITS);

                            // Unpack return value.
                            if n_ret_bits_orig > 0 {
                                let ret_word_ids: Vec<IRVarId> = (sp_packs as u32..(sp_packs + ret_packs) as u32)
                                    .map(IRVarId).collect();
                                let ret_bits = cont_em.unpack_words(&ret_word_ids, n_ret_bits_orig);
                                val_map.insert(call_vid.0, ret_bits[0]);
                            }

                            // Packed reload.
                            let mut cont_frame_sp = StackPtr::new(cont_sp_bits.clone());
                            cont_frame_sp.retreat(own_layout.size);
                            let n_spill_words = spill_words.len();
                            let mut reloaded_words = Vec::with_capacity(n_spill_words);
                            for wi in 0..n_spill_words {
                                let w = frame_reload(&mut cont_em, &cont_frame_sp,
                                    &own_layout, wi as u64, PACK_TID);
                                reloaded_words.push(w);
                            }
                            let reloaded_bits = cont_em.unpack_words(&reloaded_words, spill_keys.len());
                            for (ki, &key) in spill_keys.iter().enumerate() {
                                if key == call_vid.0 { continue; }
                                val_map.insert(key, reloaded_bits[ki]);
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
                let ret_bits: Vec<IRVarId> = values.iter().map(|v| s(v)).collect();
                // Write packed return value.
                let ret_words = em.pack_bits(&ret_bits);
                // frame_write_ret expects one value per ret slot;
                // our layout has ceil(ret_bits/PACK_W) slots of PACK_TID.
                frame_write_ret(em, frame_sp, own_layout, &ret_words);

                let cont_var = frame_read_cont(em, frame_sp, own_layout)
                    .expect("return without continuation");

                let mut retreated_sp = StackPtr::new(sp_bits.to_vec());
                retreated_sp.retreat(own_layout.size);
                let retreated_bits = retreated_sp.materialize(em);

                // Pack SP + ret words for the Dyn jump.
                let sp_words = em.pack_bits(&retreated_bits);
                let mut dyn_args = sp_words;
                dyn_args.extend(ret_words);

                IRTerminator::Jmp {
                    func: IRBlockTargetId::Dyn(cont_var),
                    args: dyn_args,
                }
            }
            Terminator::Jump(target) => {
                let ir_block = IRBlockId((entry_off + target.block.0) as u32);
                // Pack SP, append unpacked VAFFLE block args (internal).
                let sp_words = em.pack_bits(sp_bits);
                let mut args: Vec<IRVarId> = sp_words;
                args.extend(target.args.iter().map(|v| s(v)));
                IRTerminator::Jmp { func: IRBlockTargetId::Block(ir_block), args }
            }
            Terminator::IfNonzero { cond, then_target, else_target } => {
                let then_block = IRBlockId((entry_off + then_target.block.0) as u32);
                let else_block = IRBlockId((entry_off + else_target.block.0) as u32);
                let sp_words = em.pack_bits(sp_bits);
                let mut then_args: Vec<IRVarId> = sp_words.clone();
                then_args.extend(then_target.args.iter().map(|v| s(v)));
                let mut else_args: Vec<IRVarId> = sp_words;
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

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::target::VaffleTarget;
    use volar_lir::{LirTarget, LirType, StackAllocExt};

    #[test]
    fn test_lower_vaffle_with_stack_alloc() {
        let mut t = VaffleTarget::new();
        let (entry, params) = t.begin_function("test_alloc", &[LirType::U32], Some(LirType::U32));
        t.switch_to_block(entry);

        let input = params[0][0].clone();

        // Allocate, store the input, load it back.
        let ptr = t.alloca(LirType::U32, 1);
        t.ptr_store(ptr.clone(), input);
        let loaded = t.ptr_load(ptr, LirType::U32);

        t.ret(&[loaded]);
        t.end_function();

        // Lower to IR — this should not panic.
        let (ir_blocks, _ir_types) = lower_vaffle_to_ir(&t.module);

        // Should have at least the entry block, exit continuation, and function blocks.
        assert!(
            ir_blocks.blocks.len() >= 3,
            "expected at least 3 IR blocks, got {}",
            ir_blocks.blocks.len()
        );

        // Verify that STACK StorageRead/Write appear in the lowered IR.
        let has_stack_ops = ir_blocks.blocks.iter().any(|block| {
            block.stmts.iter().any(|stmt| matches!(
                stmt,
                IRStmt::StorageRead { storage, .. } | IRStmt::StorageWrite { storage, .. }
                    if *storage == StorageId::STACK
            ))
        });
        assert!(has_stack_ops, "lowered IR should contain STACK storage ops");
    }

    /// Verify that block params use packed words (PACK_TID) instead of
    /// individual Bit types.
    #[test]
    fn test_packed_block_params() {
        let mut t = VaffleTarget::new();
        let (entry, _params) = t.begin_function("packed", &[LirType::Bool], None);
        t.switch_to_block(entry);
        t.ret(&[]);
        t.end_function();

        let (ir_blocks, ir_types) = lower_vaffle_to_ir(&t.module);

        // Block 0 is the module entry (no params).
        // Block 2 is the function's entry block.  Its params should be
        // ceil(SP_BITS / PACK_W) = 1 packed word (for SP) + the function param.
        assert!(ir_blocks.blocks.len() >= 3);
        let func_entry = &ir_blocks.blocks[2];
        // SP is packed: ceil(16/64) = 1 word.
        // Plus 1 bit-level param for the Bool arg (read from frame, not a block param).
        // The block only has the SP words as params; function params are read
        // from the frame via StorageRead.
        let sp_packs = (SP_BITS + PACK_W - 1) / PACK_W;
        assert_eq!(
            func_entry.params.len(), sp_packs,
            "function entry block should have {} packed SP params, got {}",
            sp_packs, func_entry.params.len()
        );
        // All params should be PACK_TID.
        for (i, &tid) in func_entry.params.iter().enumerate() {
            assert_eq!(tid, PACK_TID,
                "param {} should be PACK_TID (Vec({}, Bit))", i, PACK_W);
        }
    }

    /// Verify that the Merge and Shuffle stmts appear in the lowered IR
    /// (evidence of pack/unpack operations).
    #[test]
    fn test_pack_unpack_stmts_present() {
        let mut t = VaffleTarget::new();
        let (entry, _params) = t.begin_function("pu", &[], None);
        t.switch_to_block(entry);
        t.ret(&[]);
        t.end_function();

        let (ir_blocks, _) = lower_vaffle_to_ir(&t.module);

        // The entry block (block 0) should contain at least one Merge
        // (packing SP bits for the jump to the function entry).
        let has_merge = ir_blocks.blocks[0].stmts.iter()
            .any(|s| matches!(s, IRStmt::Merge { ty, .. } if *ty == PACK_TID));
        assert!(has_merge, "entry block should contain a Merge (pack) with PACK_TID");

        // The function entry block should contain Shuffle stmts (unpacking SP).
        let func_entry = &ir_blocks.blocks[2];
        let has_shuffle = func_entry.stmts.iter()
            .any(|s| matches!(s, IRStmt::Shuffle { ty, .. } if *ty == BIT_TID));
        assert!(has_shuffle, "function entry should contain Shuffle (unpack) stmts");
    }

    /// Verify that spill/reload uses packed words when there is a call.
    #[test]
    fn test_packed_spill_reload() {
        use vaffle::*;
        use volar_ir_common::Stmt;

        // Build a VAFFLE module with two functions; func0 calls func1.
        let mut types = volar_ir_common::TypeTable::new();
        let bit_tid = types.intern(volar_ir_common::IrType::Primitive(
            volar_ir_common::Type::Bit,
        ));

        let sig0 = SigDecl { params: vec![bit_tid], results: vec![bit_tid] };
        let sig1 = SigDecl { params: vec![bit_tid], results: vec![bit_tid] };

        // func1: identity (return param)
        let mut vals1 = std::vec::Vec::new();
        vals1.push(Value::Param { block: BlockId(0), ty: bit_tid, idx: 0 });
        let body1 = FuncBody {
            sig: SigId(1),
            blocks: std::vec![Block {
                params: std::vec![(ValueId(0), bit_tid)],
                stmts: std::vec![],
                terminator: Terminator::Return { values: std::vec![ValueId(0)] },
            }],
            values: vals1,
            entry: BlockId(0),
        };

        // func0: call func1 with its param
        let mut vals0 = std::vec::Vec::new();
        vals0.push(Value::Param { block: BlockId(0), ty: bit_tid, idx: 0 });
        vals0.push(Value::Call { func: FuncId(1), args: std::vec![ValueId(0)] });
        vals0.push(Value::Output { value: ValueId(1), idx: 0 });
        let body0 = FuncBody {
            sig: SigId(0),
            blocks: std::vec![Block {
                params: std::vec![(ValueId(0), bit_tid)],
                stmts: std::vec![ValueId(1), ValueId(2)],
                terminator: Terminator::Return { values: std::vec![ValueId(2)] },
            }],
            values: vals0,
            entry: BlockId(0),
        };

        let module = vaffle::Module {
            types,
            oracles: std::vec![],
            actions: std::vec![],
            funcs: std::vec![
                vaffle::FuncDecl::Body(body0),
                vaffle::FuncDecl::Body(body1),
            ],
            sigs: std::vec![sig0, sig1],
            exports: alloc::collections::BTreeMap::new(),
        };

        let (ir_blocks, ir_types) = lower_vaffle_to_ir(&module);

        // The lowering should succeed and produce blocks.
        assert!(
            ir_blocks.blocks.len() >= 4,
            "expected ≥4 blocks (entry + exit + func0 + func1), got {}",
            ir_blocks.blocks.len()
        );

        // Spill writes should use PACK_TID (packed words), not BIT_TID.
        let spill_writes: std::vec::Vec<_> = ir_blocks.blocks.iter()
            .flat_map(|b| b.stmts.iter())
            .filter(|s| matches!(s, IRStmt::StorageWrite { storage, ty, .. }
                if *storage == StorageId::STACK && *ty == PACK_TID))
            .collect();
        assert!(
            !spill_writes.is_empty(),
            "spill should use PACK_TID-typed StorageWrite"
        );

        // Reload reads should also use PACK_TID.
        let reload_reads: std::vec::Vec<_> = ir_blocks.blocks.iter()
            .flat_map(|b| b.stmts.iter())
            .filter(|s| matches!(s, IRStmt::StorageRead { storage, ty, .. }
                if *storage == StorageId::STACK && *ty == PACK_TID))
            .collect();
        assert!(
            !reload_reads.is_empty(),
            "reload should use PACK_TID-typed StorageRead"
        );
    }
}
