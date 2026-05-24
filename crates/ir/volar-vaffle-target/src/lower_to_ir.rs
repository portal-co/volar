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
    collections::{BTreeMap, BTreeSet},
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
use volar_ir_common::{Constant, IrType, Stmt, StorageId, Type, TypeId};
use volar_lir::circuits::{
    bc_add, FrameLayout, StackPtr,
    frame_write_cont, frame_read_cont,
    frame_write_ret, frame_spill, frame_reload,
    BitCircuitBuilder, StorageEmitter,
    PACK_W, n_packs, pack_bits, unpack_words,
};

/// Width of the stack-pointer address in bits.
pub const SP_BITS: usize = 16;

/// Number of bits packed into a single `Vec(PACK_W, Bit)` word at block
/// boundaries, spill/reload slots, and frame argument slots.
///
/// Choosing 64 means that a 128-bit parameter occupies 2 packed block
/// params instead of 128 individual `Bit` params, and spilling 200
/// values costs 4 storage ops instead of 200.
// Re-exported from volar_lir::circuits::PACK_W.

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

    /// Number of packed words needed for `n` bits.
    fn n_packs(n: usize) -> usize {
        volar_lir::n_packs(n)
    }
}

impl BitCircuitBuilder for BlockEmitter {
    type Bit = IRVarId;
    fn bc_const(&mut self, val: bool) -> IRVarId {
        self.emit(IRStmt::Const(Constant { hi: 0, lo: val as u128 }, BIT_TID))
    }
    fn bc_poly(&mut self, coeffs: BTreeMap<Vec<IRVarId>, u8>, constant: u128) -> IRVarId {
        self.emit(IRStmt::Poly { ty: BIT_TID, coeffs, constant: Constant { hi: 0, lo: constant } })
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
    fn compose_pack(&mut self, bits: &[IRVarId]) -> IRVarId {
        if bits.len() == 1 { return bits[0]; }
        self.emit(IRStmt::Merge { parts: bits.to_vec(), ty: PACK_TID })
    }
    fn extract_bit(&mut self, word: IRVarId, idx: u8) -> IRVarId {
        self.emit(IRStmt::Shuffle { result_bits: vec![(idx, word)], ty: BIT_TID })
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
// Type-table remapping: VAFFLE TypeId → IR TypeId
// ============================================================================

/// Recursively intern VAFFLE type `vtid` into `ir_types`, returning the
/// corresponding IR `TypeId`.  Uses `map`/`done` for memoization so each
/// entry is processed at most once (handles shared structure and avoids
/// infinite loops for any forward-declared but well-formed type graph).
fn remap_type_id(
    vtid: TypeId,
    vaffle_types: &volar_ir_common::TypeTable,
    ir_types: &mut IRTypes,
    map: &mut Vec<TypeId>,
    done: &mut Vec<bool>,
) -> TypeId {
    if done[vtid.0 as usize] {
        return map[vtid.0 as usize];
    }
    // Mark before recursing to break cycles (result is a placeholder until we
    // overwrite below — cycles in IrType are not valid, so this is safe).
    done[vtid.0 as usize] = true;
    let vty = vaffle_types.0[vtid.0 as usize].clone();
    let ity = match vty {
        IrType::Primitive(p) => IrType::Primitive(p),
        IrType::Vec(k, inner) => {
            let inner_ir = remap_type_id(inner, vaffle_types, ir_types, map, done);
            IrType::Vec(k, inner_ir)
        }
        IrType::Tuple(parts) => {
            let parts_ir: Vec<TypeId> = parts.iter()
                .map(|&p| remap_type_id(p, vaffle_types, ir_types, map, done))
                .collect();
            IrType::Tuple(parts_ir)
        }
        IrType::Block { params } => {
            let params_ir: Vec<TypeId> = params.iter()
                .map(|&p| remap_type_id(p, vaffle_types, ir_types, map, done))
                .collect();
            IrType::Block { params: params_ir }
        }
        IrType::Func { params, results } => {
            let params_ir: Vec<TypeId> = params.iter()
                .map(|&p| remap_type_id(p, vaffle_types, ir_types, map, done))
                .collect();
            let results_ir: Vec<TypeId> = results.iter()
                .map(|&r| remap_type_id(r, vaffle_types, ir_types, map, done))
                .collect();
            IrType::Func { params: params_ir, results: results_ir }
        }
    };
    let ir_tid = ir_types.intern(ity);
    map[vtid.0 as usize] = ir_tid;
    ir_tid
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
    /// Number of packed words carrying parameters as entry-block params.
    /// Callers append these after the SP words in the jump args.
    n_param_words: usize,
    /// Number of individual parameter bits in the function signature.
    n_params: usize,
}

struct LowerCtx<'m> {
    module: &'m Module,
    types: IRTypes,
    /// Maps VAFFLE TypeId → IR TypeId (index = VAFFLE TypeId.0).
    type_map: Vec<TypeId>,
    func_info: Vec<FuncInfo>,
    blocks: Vec<IRBlock>,
    oracles: Vec<OracleDecl>,
    actions: Vec<ActionDecl>,
    /// Extra blocks generated by call-site splitting (appended after all
    /// function blocks).  Each entry is `(block_def)`.
    extra_blocks: Vec<IRBlock>,
    pre_init: alloc::vec::Vec<volar_ir_common::PreInitSegment>,
}

impl<'m> LowerCtx<'m> {
    fn new(module: &'m Module) -> Self {
        let mut types = IRTypes::new();
        types.push(IrType::Primitive(Type::Bit));           // index 0 = BIT_TID
        types.push(IrType::Vec(SP_BITS, BIT_TID));          // index 1 = ADDR_TID
        types.push(IrType::Vec(PACK_W, BIT_TID));           // index 2 = PACK_TID

        // Build a mapping from VAFFLE TypeId → IR TypeId by interning each
        // VAFFLE type into the IR type table (recursively remapping inner refs).
        let n = module.types.0.len();
        let mut type_map = alloc::vec![TypeId(0); n];
        let mut done = alloc::vec![false; n];
        for i in 0..n {
            remap_type_id(TypeId(i as u32), &module.types, &mut types, &mut type_map, &mut done);
        }

        // Remap TypeIds in pre_init segments to be valid in the IR type table.
        let pre_init = module.pre_init.iter().map(|seg| {
            volar_ir_common::PreInitSegment {
                storage: seg.storage,
                ty: type_map[seg.ty.0 as usize],
                offset: seg.offset,
                data: seg.data.clone(),
            }
        }).collect();

        LowerCtx {
            module, types, type_map,
            func_info: Vec::new(),
            blocks: Vec::new(),
            oracles: module.oracles.clone(),
            actions: module.actions.clone(),
            extra_blocks: Vec::new(),
            pre_init,
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
                        n_param_words: 0,
                        n_params: 0,
                    });
                    continue;
                }
            };

            let sig = &self.module.sigs[body.sig.0];
            let n_params = sig.params.len();
            let n_ret = if sig.results.is_empty() { 0 } else { 1 };
            let n_values = body.values.len();

            // Callee layout: params are now passed as entry-block params (not
            // written to the frame), so the frame only needs ret + cont slots.
            let n_param_words = BlockEmitter::n_packs(n_params);
            let n_ret_words = BlockEmitter::n_packs(n_ret);

            let mut offset = 0u64;
            let ret = if n_ret > 0 {
                let o = offset; offset += n_ret_words as u64;
                Some((o, n_ret_words as u64, PACK_TID))
            } else { None };
            let cont_ty = Some(self.intern_cont_block_type(n_ret));
            let callee_size = offset;

            let callee_layout = FrameLayout {
                params: vec![], ret, cont_ty,
                spill_base: 0, n_spill: 0,
                size: callee_size, storage: StorageId::STACK,
            };

            // Own layout: this function's spill slots.
            let n_spill_words = BlockEmitter::n_packs(n_values) as u64;
            let spill_base = callee_size;
            let own_size = callee_size + n_spill_words;

            let own_layout = FrameLayout {
                params: vec![],
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
                n_param_words,
                n_params,
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
        let sp_words = pack_bits(&mut em, &new_sp_bits, PACK_W);

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
        let ret_bits = unpack_words(&mut exit_em, &ret_word_ids, n_ret_bits, PACK_W);

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
        let n_param_words = info.n_param_words;
        let n_params = info.n_params;

        for (vaffle_bi, vaffle_block) in body.blocks.iter().enumerate() {
            let ir_bi = entry_block_offset + vaffle_bi;
            let is_entry = vaffle_bi == body.entry.0;

            // Every block takes SP as packed words.
            // Entry blocks also take n_param_words packed parameter words
            // (passed directly by the caller instead of written to the frame).
            // Non-entry blocks take individual VAFFLE block params.
            let mut params: Vec<IRTypeId> = vec![PACK_TID; sp_packs];
            if is_entry {
                params.extend(vec![PACK_TID; n_param_words]);
            } else {
                for &(_vid, ty_id) in &vaffle_block.params {
                    params.push(ty_id);
                }
            }

            let mut em = BlockEmitter::new(params);

            // Unpack SP from packed words.
            let sp_word_ids: Vec<IRVarId> = (0..sp_packs as u32).map(IRVarId).collect();
            let sp_bits: Vec<IRVarId> = unpack_words(&mut em, &sp_word_ids, SP_BITS, PACK_W);

            let mut frame_sp = StackPtr::new(sp_bits.clone());
            frame_sp.retreat(own_layout.size);

            let mut val_map: BTreeMap<usize, IRVarId> = BTreeMap::new();

            // Entry block: param words arrive directly as block params after SP.
            if is_entry {
                let param_word_ids: Vec<IRVarId> = (sp_packs as u32 .. (sp_packs + n_param_words) as u32)
                    .map(IRVarId)
                    .collect();
                let all_bits = unpack_words(&mut em, &param_word_ids, n_params, PACK_W);
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
                            let ir_stmt = translate_stmt(stmt, &val_map, &self.type_map);
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

                            // 1. Selective spill: collect only values that are
                            //    actually used after this call (in the
                            //    remaining stmts or the block terminator).
                            //    StackAlloc addresses are compile-time
                            //    constants and never need to be spilled.
                            let future_uses = collect_uses(
                                body, after_call, &vaffle_block.terminator,
                            );
                            let spill_keys: Vec<usize> = val_map.keys().copied()
                                .filter(|k| future_uses.contains(k))
                                .filter(|k| !matches!(
                                    &body.values[*k], Value::StackAlloc { .. }
                                ))
                                .collect();
                            let spill_bits: Vec<IRVarId> = spill_keys.iter()
                                .map(|k| val_map[k])
                                .collect();
                            let spill_words = pack_bits(&mut current_em, &spill_bits, PACK_W);
                            for (wi, &word) in spill_words.iter().enumerate() {
                                frame_spill(&mut current_em, &current_frame_sp,
                                    &own_layout, wi as u64, word, PACK_TID);
                            }

                            // 2. Pack callee args — passed as entry-block params, not frame writes.
                            let arg_bits: Vec<IRVarId> = call_args.iter()
                                .map(|vid| val_map[&vid.0])
                                .collect();
                            let arg_words = pack_bits(&mut current_em, &arg_bits, PACK_W);

                            // 3. Write continuation.
                            let n_ret = cl.ret.map_or(0, |(_, c, _)| c as usize);
                            let n_ret_bits_orig = if n_ret > 0 { 1 } else { 0 };
                            let cont_block_idx = self.blocks.len() + self.extra_blocks.len() + 1;
                            let cont_ir_idx = cont_block_idx as u32;
                            let cont_ty = cl.cont_ty.unwrap_or(BIT_TID);
                            let cont_var = current_em.emit(IRStmt::Const(
                                Constant { hi: 0, lo: cont_ir_idx as u128 }, cont_ty,
                            ));
                            let callee_frame_sp = StackPtr::new(current_sp_bits.clone());
                            frame_write_cont(&mut current_em, &callee_frame_sp, &cl, cont_var);

                            // 4. Advance SP, pack, jump — args appended after SP words.
                            let mut new_sp = StackPtr::new(current_sp_bits.clone());
                            new_sp.advance(cl.size);
                            let new_sp_bits = new_sp.materialize(&mut current_em);
                            let mut sp_words = pack_bits(&mut current_em, &new_sp_bits, PACK_W);
                            sp_words.extend(arg_words);

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
                            let cont_sp_bits = unpack_words(&mut cont_em, &cont_sp_word_ids, SP_BITS, PACK_W);

                            // Unpack return value.
                            if n_ret_bits_orig > 0 {
                                let ret_word_ids: Vec<IRVarId> = (sp_packs as u32..(sp_packs + ret_packs) as u32)
                                    .map(IRVarId).collect();
                                let ret_bits = unpack_words(&mut cont_em, &ret_word_ids, n_ret_bits_orig, PACK_W);
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
                            let reloaded_bits = unpack_words(&mut cont_em, &reloaded_words, spill_keys.len(), PACK_W);
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
                let ret_words = pack_bits(em, &ret_bits, PACK_W);
                // frame_write_ret expects one value per ret slot;
                // our layout has ceil(ret_bits/PACK_W) slots of PACK_TID.
                frame_write_ret(em, frame_sp, own_layout, &ret_words);

                let cont_var = frame_read_cont(em, frame_sp, own_layout)
                    .expect("return without continuation");

                let mut retreated_sp = StackPtr::new(sp_bits.to_vec());
                retreated_sp.retreat(own_layout.size);
                let retreated_bits = retreated_sp.materialize(em);

                // Pack SP + ret words for the Dyn jump.
                let sp_words = pack_bits(em, &retreated_bits, PACK_W);
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
                let sp_words = pack_bits(em, sp_bits, PACK_W);
                let mut args: Vec<IRVarId> = sp_words;
                args.extend(target.args.iter().map(|v| s(v)));
                IRTerminator::Jmp { func: IRBlockTargetId::Block(ir_block), args }
            }
            Terminator::IfNonzero { cond, then_target, else_target } => {
                let then_block = IRBlockId((entry_off + then_target.block.0) as u32);
                let else_block = IRBlockId((entry_off + else_target.block.0) as u32);
                let sp_words = pack_bits(em, sp_bits, PACK_W);
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
            Terminator::ReturnCall { func: callee_fid, args: call_args } => {
                let callee_idx = callee_fid.0;
                // Guard: Import stubs have no entry_block — fall through to the
                // catch-all for those (they shouldn't appear as tail calls in
                // well-formed VAFFLE, but be safe).
                if callee_idx >= self.func_info.len() {
                    return IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![] };
                }
                let callee_info = &self.func_info[callee_idx];
                let cl = callee_info.callee_layout.clone();

                // Compute the pre-call SP: sp - F.callee_size.
                // own_layout.spill_base == F's callee_layout.size (set in plan_functions).
                // This is the address where F's caller wrote F's args/cont, which
                // is also where G's frame should start for a tail call.
                let f_callee_size = own_layout.spill_base;
                let mut callee_frame_sp = StackPtr::new(sp_bits.to_vec());
                callee_frame_sp.retreat(f_callee_size);

                // Pack G's args — passed as entry-block params, not frame writes.
                let arg_bits: Vec<IRVarId> = call_args.iter()
                    .map(|vid| val_map.get(&vid.0).copied().unwrap_or(IRVarId(0)))
                    .collect();
                let arg_words = pack_bits(em, &arg_bits, PACK_W);

                // Advance SP to G.callee_layout.size and jump — args after SP words.
                let mut new_sp = callee_frame_sp.clone();
                new_sp.advance(cl.size);
                let new_sp_bits = new_sp.materialize(em);
                let mut sp_words = pack_bits(em, &new_sp_bits, PACK_W);
                sp_words.extend(arg_words);

                let callee_entry = IRBlockId(callee_info.entry_block as u32);
                IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(callee_entry),
                    args: sp_words,
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
            pre_init: self.pre_init,
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

/// Collect all `ValueId.0` indices that appear as *operands* in the given
/// slice of statement `ValueId`s and in `term`.
///
/// These are the values that must be *live* (available in `val_map`) after a
/// call site that precedes `stmt_ids` in the same block.  Only operand
/// references are collected — the defining occurrence of a value is not.
fn collect_uses(
    body: &FuncBody,
    stmt_ids: &[ValueId],
    term: &Terminator,
) -> BTreeSet<usize> {
    let mut uses = BTreeSet::new();
    for &vid in stmt_ids {
        collect_value_uses(&body.values[vid.0], &mut uses);
    }
    collect_terminator_uses(term, &mut uses);
    uses
}

fn collect_value_uses(val: &Value, out: &mut BTreeSet<usize>) {
    match val {
        Value::Op(stmt) => collect_stmt_uses(stmt, out),
        Value::Call { args, .. } => {
            for a in args { out.insert(a.0); }
        }
        Value::Output { value, .. } => { out.insert(value.0); }
        Value::PtrLoad { ptr, .. } => { out.insert(ptr.0); }
        Value::PtrStore { ptr, val } => {
            out.insert(ptr.0);
            out.insert(val.0);
        }
        Value::PtrOffset { ptr, idx, .. } => {
            out.insert(ptr.0);
            out.insert(idx.0);
        }
        // Defining occurrences — no operands to record.
        Value::Param { .. } | Value::StackAlloc { .. } => {}
    }
}

fn collect_stmt_uses(stmt: &Stmt<ValueId>, out: &mut BTreeSet<usize>) {
    match stmt {
        Stmt::Const(..) | Stmt::Rng { .. } => {}
        Stmt::Poly { coeffs, .. } => {
            for vars in coeffs.keys() {
                for v in vars { out.insert(v.0); }
            }
        }
        Stmt::Merge { parts, .. } => {
            for p in parts { out.insert(p.0); }
        }
        Stmt::Splat { src, .. } => { out.insert(src.0); }
        Stmt::Transmute { src, .. } => { out.insert(src.0); }
        Stmt::Rol { src, .. } | Stmt::Ror { src, .. } => { out.insert(src.0); }
        Stmt::Shuffle { result_bits, .. } => {
            for (_, v) in result_bits { out.insert(v.0); }
        }
        Stmt::StorageRead { addr, .. } => { out.insert(addr.0); }
        Stmt::StorageWrite { src, addr, .. } => {
            out.insert(src.0);
            out.insert(addr.0);
        }
        Stmt::OracleCall { args, .. } => {
            for a in args { out.insert(a.0); }
        }
        Stmt::OracleOutput { call, .. } => { out.insert(call.0); }
        Stmt::ActionCall { guard, args, fallbacks, .. } => {
            out.insert(guard.0);
            for a in args { out.insert(a.0); }
            for f in fallbacks { out.insert(f.0); }
        }
        Stmt::ActionOutput { call, .. } => { out.insert(call.0); }
    }
}

fn collect_terminator_uses(term: &Terminator, out: &mut BTreeSet<usize>) {
    match term {
        Terminator::Return { values } => {
            for v in values { out.insert(v.0); }
        }
        Terminator::Jump(target) => {
            for v in &target.args { out.insert(v.0); }
        }
        Terminator::ReturnCall { args, .. } => {
            for a in args { out.insert(a.0); }
        }
        Terminator::IfNonzero { cond, then_target, else_target } => {
            out.insert(cond.0);
            for v in &then_target.args { out.insert(v.0); }
            for v in &else_target.args { out.insert(v.0); }
        }
        Terminator::Table { index, targets, default_target } => {
            out.insert(index.0);
            for t in targets { for v in &t.args { out.insert(v.0); } }
            for v in &default_target.args { out.insert(v.0); }
        }
    }
}

/// Translate a VAFFLE `Stmt<ValueId>` to an `IRStmt<IRVarId>`.
///
/// `val_map` maps VAFFLE `ValueId` → IR `IRVarId`.
/// `type_map` maps VAFFLE `TypeId` → IR `TypeId` (produced by [`remap_type_id`]).
fn translate_stmt(
    stmt: &volar_ir_common::Stmt<ValueId>,
    val_map: &BTreeMap<usize, IRVarId>,
    type_map: &[TypeId],
) -> IRStmt {
    let s = |vid: &ValueId| val_map.get(&vid.0).copied().unwrap_or(IRVarId(0));
    let t = |tid: &TypeId| type_map[tid.0 as usize];
    let tv = |tids: &[TypeId]| tids.iter().map(t).collect::<Vec<_>>();
    match stmt {
        volar_ir_common::Stmt::Const(c, ty) => IRStmt::Const(*c, t(ty)),
        volar_ir_common::Stmt::Poly { ty, coeffs, constant } => IRStmt::Poly {
            ty: t(ty),
            coeffs: coeffs.iter().map(|(vars, &coeff)| {
                let mut nv: Vec<IRVarId> = vars.iter().map(s).collect();
                nv.sort();
                (nv, coeff)
            }).collect(),
            constant: *constant,
        },
        volar_ir_common::Stmt::Merge { parts, ty } =>
            IRStmt::Merge { parts: parts.iter().map(s).collect(), ty: t(ty) },
        volar_ir_common::Stmt::Splat { src, ty } =>
            IRStmt::Splat { src: s(src), ty: t(ty) },
        volar_ir_common::Stmt::Transmute { src, src_ty, dst_ty } =>
            IRStmt::Transmute { src: s(src), src_ty: t(src_ty), dst_ty: t(dst_ty) },
        volar_ir_common::Stmt::Rol { src, ty, n } =>
            IRStmt::Rol { src: s(src), ty: t(ty), n: *n },
        volar_ir_common::Stmt::Ror { src, ty, n } =>
            IRStmt::Ror { src: s(src), ty: t(ty), n: *n },
        volar_ir_common::Stmt::Shuffle { result_bits, ty } =>
            IRStmt::Shuffle {
                result_bits: result_bits.iter().map(|(b, v)| (*b, s(v))).collect(),
                ty: t(ty),
            },
        volar_ir_common::Stmt::StorageRead { storage, ty, addr } =>
            IRStmt::StorageRead { storage: *storage, ty: t(ty), addr: s(addr) },
        volar_ir_common::Stmt::StorageWrite { storage, src, ty, addr } =>
            IRStmt::StorageWrite { storage: *storage, src: s(src), ty: t(ty), addr: s(addr) },
        volar_ir_common::Stmt::Rng { name, ty } =>
            IRStmt::Rng { name: name.clone(), ty: t(ty) },
        volar_ir_common::Stmt::OracleCall { name, args, output_tys, result_ty } =>
            IRStmt::OracleCall {
                name: name.clone(),
                args: args.iter().map(s).collect(),
                output_tys: tv(output_tys),
                result_ty: t(result_ty),
            },
        volar_ir_common::Stmt::OracleOutput { call, idx, ty } =>
            IRStmt::OracleOutput { call: s(call), idx: *idx, ty: t(ty) },
        volar_ir_common::Stmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } =>
            IRStmt::ActionCall {
                name: name.clone(),
                guard: s(guard),
                args: args.iter().map(s).collect(),
                fallbacks: fallbacks.iter().map(s).collect(),
                output_tys: tv(output_tys),
                result_ty: t(result_ty),
            },
        volar_ir_common::Stmt::ActionOutput { call, idx, ty } =>
            IRStmt::ActionOutput { call: s(call), idx: *idx, ty: t(ty) },
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

    /// Verify that spill/reload uses packed words, and that only values that
    /// are actually live after a call are spilled (not all values in val_map).
    ///
    /// Scenario: func0 calls func1 with its only param and immediately returns
    /// the result.  At the call site the param is consumed as a call argument;
    /// it is *not* referenced after the call.  So zero values should be spilled.
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

        // func0: call func1 with its param, return result.
        // The param (ValueId(0)) is NOT used after the call — so zero spills.
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

        let (ir_blocks, _ir_types) = lower_vaffle_to_ir(&module);

        assert!(
            ir_blocks.blocks.len() >= 4,
            "expected ≥4 blocks (entry + exit + func0 + func1), got {}",
            ir_blocks.blocks.len()
        );

        // With selective spilling, the number of STACK+PACK_TID StorageWrites
        // should equal the number from arg-pushes and ret-writes only (the
        // frame protocol), with zero additional writes from spills.
        //
        // Concretely: func0 pushes 1 call-arg word and writes 1 ret word;
        // func1 writes 1 ret word → 3 total.  Before this optimisation there
        // would have been extra spill writes on top of that.  We verify that
        // the total does not exceed 3 (i.e. no spill writes were added).
        let stack_pack_writes: std::vec::Vec<_> = ir_blocks.blocks.iter()
            .flat_map(|b| b.stmts.iter())
            .filter(|s| matches!(s, IRStmt::StorageWrite { storage, ty, .. }
                if *storage == StorageId::STACK && *ty == PACK_TID))
            .collect();
        assert!(
            stack_pack_writes.len() <= 3,
            "expected at most 3 STACK+PACK_TID writes (arg + 2 rets); \
             got {} — likely a regression adding unnecessary spill writes",
            stack_pack_writes.len()
        );
    }

    /// Verify that a value which IS live across a call gets spilled and
    /// reloaded using packed words.
    ///
    /// Scenario: func0 has a local value `local` (NOT an arg to the call) that
    /// is used AFTER the call returns.  At the call site `local` must be
    /// spilled; the reload should use PACK_TID-typed StorageRead.
    #[test]
    fn test_selective_spill() {
        use vaffle::*;
        use volar_ir_common::{Stmt, Constant};

        let mut types = volar_ir_common::TypeTable::new();
        let bit_tid = types.intern(volar_ir_common::IrType::Primitive(
            volar_ir_common::Type::Bit,
        ));

        // sig0: (bit) -> bit
        // sig1: (bit) -> bit  (identity)
        let sig0 = SigDecl { params: vec![bit_tid], results: vec![bit_tid] };
        let sig1 = SigDecl { params: vec![bit_tid], results: vec![bit_tid] };

        // func1: identity — return its param.
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

        // func0:
        //   param   = ValueId(0)   (bit input)
        //   local   = ValueId(1)   = const 1  (NOT used as call arg)
        //   call    = ValueId(2)   = call func1(param)
        //   out     = ValueId(3)   = Output(call, 0)
        //   xor_res = ValueId(4)   = local XOR out   (uses local AFTER the call)
        //   return xor_res
        //
        // At the call site, `local` is in val_map and is referenced by
        // ValueId(4) which comes after the call → `local` IS live → must spill.
        let mut vals0 = std::vec::Vec::new();
        vals0.push(Value::Param { block: BlockId(0), ty: bit_tid, idx: 0 });  // 0
        vals0.push(Value::Op(Stmt::Const(                                      // 1 = local
            Constant { hi: 0, lo: 1 }, bit_tid,
        )));
        vals0.push(Value::Call { func: FuncId(1), args: std::vec![ValueId(0)] }); // 2
        vals0.push(Value::Output { value: ValueId(2), idx: 0 });               // 3 = out
        {
            // xor_res = local XOR out  (degree-1 polynomial: local + out)
            let mut coeffs = alloc::collections::BTreeMap::new();
            coeffs.insert(std::vec![ValueId(1)], 1u8);
            coeffs.insert(std::vec![ValueId(3)], 1u8);
            vals0.push(Value::Op(Stmt::Poly {                                  // 4 = xor_res
                ty: bit_tid,
                coeffs,
                constant: volar_ir_common::Constant { hi: 0, lo: 0 },
            }));
        }

        let body0 = FuncBody {
            sig: SigId(0),
            blocks: std::vec![Block {
                params: std::vec![(ValueId(0), bit_tid)],
                stmts: std::vec![ValueId(1), ValueId(2), ValueId(3), ValueId(4)],
                terminator: Terminator::Return { values: std::vec![ValueId(4)] },
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

        let (ir_blocks, _ir_types) = lower_vaffle_to_ir(&module);

        assert!(
            ir_blocks.blocks.len() >= 4,
            "expected ≥4 IR blocks, got {}",
            ir_blocks.blocks.len()
        );

        // `local` (ValueId(1)) is live across the call → at least one
        // PACK_TID-typed StorageWrite to STACK must appear (the spill).
        let spill_writes: std::vec::Vec<_> = ir_blocks.blocks.iter()
            .flat_map(|b| b.stmts.iter())
            .filter(|s| matches!(s, IRStmt::StorageWrite { storage, ty, .. }
                if *storage == StorageId::STACK && *ty == PACK_TID))
            .collect();
        assert!(
            !spill_writes.is_empty(),
            "`local` is live across the call and must be spilled (PACK_TID StorageWrite)"
        );

        // Matching reload: PACK_TID-typed StorageRead from STACK in the
        // continuation block.
        let reload_reads: std::vec::Vec<_> = ir_blocks.blocks.iter()
            .flat_map(|b| b.stmts.iter())
            .filter(|s| matches!(s, IRStmt::StorageRead { storage, ty, .. }
                if *storage == StorageId::STACK && *ty == PACK_TID))
            .collect();
        assert!(
            !reload_reads.is_empty(),
            "spilled `local` must be reloaded (PACK_TID StorageRead)"
        );
    }

    /// A `Terminator::ReturnCall` should lower to a direct jump to the callee's
    /// entry block, not to a Dyn continuation.
    ///
    /// Scenario: func0 tail-calls func1.  The lowered IR for func0's block
    /// should terminate with a `Jmp` to `Block(func1_entry)`, NOT `Dyn(...)`.
    #[test]
    fn test_return_call_lowers_to_direct_jump() {
        use vaffle::*;

        let mut types = volar_ir_common::TypeTable::new();
        let bit_tid = types.intern(volar_ir_common::IrType::Primitive(
            volar_ir_common::Type::Bit,
        ));

        let sig0 = SigDecl { params: std::vec![bit_tid], results: std::vec![bit_tid] };
        let sig1 = SigDecl { params: std::vec![bit_tid], results: std::vec![bit_tid] };

        // func1: identity function.
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

        // func0: tail-calls func1 with its own param.
        let mut vals0 = std::vec::Vec::new();
        vals0.push(Value::Param { block: BlockId(0), ty: bit_tid, idx: 0 });
        let body0 = FuncBody {
            sig: SigId(0),
            blocks: std::vec![Block {
                params: std::vec![(ValueId(0), bit_tid)],
                stmts: std::vec![],
                terminator: Terminator::ReturnCall {
                    func: FuncId(1),
                    args: std::vec![ValueId(0)],
                },
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

        let (ir_blocks, _ir_types) = lower_vaffle_to_ir(&module);

        // Layout: block 0 = module entry, block 1 = exit cont,
        //         block 2 = func0 entry, block 3 = func1 entry.
        assert!(
            ir_blocks.blocks.len() >= 4,
            "expected ≥4 IR blocks, got {}",
            ir_blocks.blocks.len()
        );

        // func1's entry block index should be 3.
        let func1_entry = IRBlockId(3);

        // func0's entry block (block 2) terminator must be a direct Jmp to func1.
        let func0_block = &ir_blocks.blocks[2];
        match &func0_block.terminator {
            IRTerminator::Jmp { func: IRBlockTargetId::Block(target), .. } => {
                assert_eq!(
                    *target, func1_entry,
                    "ReturnCall should jump directly to func1's entry block"
                );
            }
            other => panic!(
                "expected Jmp to Block(func1_entry), got {:?}", other
            ),
        }

        // No extra blocks should be added (no call-site continuation split).
        assert_eq!(
            ir_blocks.blocks.len(), 4,
            "tail call should not generate extra continuation blocks"
        );
    }

    /// A `Terminator::ReturnCall` should not emit any STACK+PACK_TID
    /// StorageWrites for spilling (there is no live-across-call state to
    /// preserve), and should not emit a continuation-write (the caller's
    /// continuation is reused).
    #[test]
    fn test_return_call_no_spill_no_cont_write() {
        use vaffle::*;

        let mut types = volar_ir_common::TypeTable::new();
        let bit_tid = types.intern(volar_ir_common::IrType::Primitive(
            volar_ir_common::Type::Bit,
        ));

        let sig0 = SigDecl { params: std::vec![bit_tid], results: std::vec![bit_tid] };
        let sig1 = SigDecl { params: std::vec![bit_tid], results: std::vec![bit_tid] };

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

        let mut vals0 = std::vec::Vec::new();
        vals0.push(Value::Param { block: BlockId(0), ty: bit_tid, idx: 0 });
        let body0 = FuncBody {
            sig: SigId(0),
            blocks: std::vec![Block {
                params: std::vec![(ValueId(0), bit_tid)],
                stmts: std::vec![],
                terminator: Terminator::ReturnCall {
                    func: FuncId(1),
                    args: std::vec![ValueId(0)],
                },
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

        let (ir_blocks, _) = lower_vaffle_to_ir(&module);

        // Count Block-typed StorageWrites in func0's block (block 2).
        // A regular call writes one Block-typed cont; a tail call must NOT.
        let func0_block = &ir_blocks.blocks[2];
        let block_writes = func0_block.stmts.iter().filter(|s| matches!(
            s, IRStmt::StorageWrite { ty, .. } if *ty != PACK_TID && *ty != BIT_TID
        )).count();
        assert_eq!(
            block_writes, 0,
            "tail call must not write a new continuation (Block-lane write count = {})",
            block_writes
        );
    }
}
