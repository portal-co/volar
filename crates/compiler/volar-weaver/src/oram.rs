// @reliability: experimental
//! @ai: assisted
//!
//! ORAM weaver: compiles `volar-oram-core` server-side ORAM operations
//! through the volar-compiler pipeline so they can be lowered to circuits.
//!
//! The ORAM server code (tree operations: `read_path`, `write_path`,
//! `server_step`) is written in "total Rust" — the subset that the
//! compiler can parse. This module provides:
//!
//! - [`OramConfig`]: describes how a storage region maps to an ORAM
//!   instance, including the Path ORAM parameters (Z, B, L) and the
//!   corresponding IR types, action declarations, and FHE action configs.
//!
//! - [`oram_linked_spec`]: parses the `volar-oram-core` source into a
//!   [`LinkedSpec`] that can be merged into any target `IrModule`.
//!
//! The client-side logic (stash, position map, eviction, Protocol
//! impls) stays in normal Rust in `volar-oram`.
//!
//! # ORAM ↔ ActionCall mapping
//!
//! Each ORAM access involves two round-trips between circuit and client:
//!
//! 1. **Begin** (`oram_begin_{id}`): The circuit sends the encrypted
//!    application address. The client decrypts it, looks up the position
//!    map, and returns a **plaintext leaf** index. The ORAM guarantee
//!    ensures the leaf is uniformly random and independent of the address.
//!
//! 2. **Process** (`oram_process_{id}`): The circuit reads the tree path
//!    at the plaintext leaf (direct indexed storage — no MUX tree needed),
//!    then sends the path buckets to the client. The client absorbs them
//!    into its stash, performs the read/write, does deterministic eviction,
//!    and returns: updated write-back buckets, the read data, and two
//!    plaintext eviction leaf indices.
//!
//! The plaintext outputs use [`FheActionConfig`] with `output_public`
//! flags, which the FHE weaver uses to track public values through the
//! `PublicSet` and avoid unnecessary ciphertext operations on leaf indices.

use alloc::{collections::BTreeMap, format, string::String, vec, vec::Vec};

#[cfg(feature = "linking")]
use volar_compiler::linkage::LinkedSpec;
use volar_ir::ir::{
    ActionDecl, Constant, IRBlock, IRBlocks, IRBlockTargetId, IRStmt, IRTerminator,
    IRType, IRTypeId, IRTypes, IRVarId, PrimType, StorageId,
};

use crate::fhe::FheActionConfig;

// ============================================================================
// OramConfig — per-storage ORAM parameters
// ============================================================================

/// Configuration for an ORAM-backed storage region.
///
/// Describes how a logical storage region in the original circuit should be
/// backed by a Path ORAM construction. The parameters match `volar-oram-core`:
///
/// - `Z`: bucket size (entries per bucket, typically 4)
/// - `B`: block data size in bytes (e.g. 16 or 32)
/// - `L`: number of tree levels (the tree has `2^(L-1)` leaves)
///
/// Each `OramConfig` generates a pair of [`ActionDecl`]s (begin + process)
/// and their corresponding [`FheActionConfig`]s for the FHE weaver.
#[derive(Clone, Debug)]
pub struct OramConfig {
    /// The `StorageId` index in the original IR that this ORAM replaces.
    pub storage_id: u32,
    /// Bucket size: entries per bucket. Typically 4.
    pub z: usize,
    /// Block data size in bytes.
    pub b: usize,
    /// Number of tree levels (depth). The tree has `2^(L-1)` leaves
    /// and `2^L - 1` total nodes.
    pub l: usize,
}

impl OramConfig {
    /// Total number of tree nodes: `2^L - 1`.
    pub fn num_nodes(&self) -> usize {
        (1usize << self.l) - 1
    }

    /// Number of leaves: `2^(L-1)`.
    pub fn num_leaves(&self) -> usize {
        1usize << (self.l - 1)
    }

    /// Bits per ORAM entry: `addr(64) + leaf(64) + data(8*B)`.
    pub fn entry_bits(&self) -> usize {
        64 + 64 + 8 * self.b
    }

    /// Bits per bucket: `Z * entry_bits`.
    pub fn bucket_bits(&self) -> usize {
        self.z * self.entry_bits()
    }

    /// Bits for a full root-to-leaf path: `L * bucket_bits`.
    pub fn path_bits(&self) -> usize {
        self.l * self.bucket_bits()
    }

    // -- Action naming -------------------------------------------------------

    /// Name of the "begin" action for this storage.
    pub fn begin_action_name(&self) -> String {
        format!("oram_begin_{}", self.storage_id)
    }

    /// Name of the "process path" action for this storage.
    pub fn process_action_name(&self) -> String {
        format!("oram_process_{}", self.storage_id)
    }

    /// Name of the "evict path" action for this storage.
    pub fn evict_action_name(&self) -> String {
        format!("oram_evict_{}", self.storage_id)
    }

    // -- IR type construction ------------------------------------------------

    /// Intern the ORAM entry type: `Tuple(u64, u64, Vec(B, u8))`.
    ///
    /// Represents `OramEntry<B>` in the IR type system:
    /// - field 0: `addr` (u64)
    /// - field 1: `leaf` (u64)
    /// - field 2: `data` ([u8; B])
    pub fn entry_type(&self, types: &mut IRTypes) -> IRTypeId {
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let u8_ty = types.intern(IRType::Primitive(PrimType::_8));
        let data_ty = types.intern(IRType::Vec(self.b, u8_ty));
        types.intern(IRType::Tuple(vec![u64_ty, u64_ty, data_ty]))
    }

    /// Intern the bucket type: `Vec(Z, entry_type)`.
    ///
    /// Represents `Bucket<Z, B>` — a fixed-size array of Z entries.
    pub fn bucket_type(&self, types: &mut IRTypes) -> IRTypeId {
        let entry_ty = self.entry_type(types);
        types.intern(IRType::Vec(self.z, entry_ty))
    }

    /// Intern the path type: `Vec(L, bucket_type)`.
    ///
    /// Represents the L buckets along a root-to-leaf path.
    pub fn path_type(&self, types: &mut IRTypes) -> IRTypeId {
        let bucket_ty = self.bucket_type(types);
        types.intern(IRType::Vec(self.l, bucket_ty))
    }

    /// Intern the block data type: `Vec(B, u8)`.
    pub fn data_type(&self, types: &mut IRTypes) -> IRTypeId {
        let u8_ty = types.intern(IRType::Primitive(PrimType::_8));
        types.intern(IRType::Vec(self.b, u8_ty))
    }

    // -- ActionDecl construction ---------------------------------------------

    /// Create the [`ActionDecl`] for the "begin" action.
    ///
    /// The begin action is the first round-trip of an ORAM access:
    /// - **Params**: `[u64]` — the encrypted application-level address
    /// - **Results**: `[u64]` — the plaintext leaf index from the position map
    pub fn begin_action_decl(&self, types: &mut IRTypes) -> ActionDecl {
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        ActionDecl {
            name: self.begin_action_name(),
            params: vec![u64_ty],
            results: vec![u64_ty],
        }
    }

    /// Create the [`ActionDecl`] for the "process path" action.
    ///
    /// The process action is the second round-trip. After the circuit reads
    /// the tree path using the plaintext leaf, it sends the path buckets
    /// along with the write data and operation flag:
    ///
    /// - **Params**: `[path_ty, data_ty, Bit]` — path buckets, write data,
    ///   is_write flag
    /// - **Results**: `[path_ty, data_ty, u64, u64]` — write-back buckets,
    ///   read data, eviction leaf 1, eviction leaf 2
    pub fn process_action_decl(&self, types: &mut IRTypes) -> ActionDecl {
        let path_ty = self.path_type(types);
        let data_ty = self.data_type(types);
        let bit_ty = types.bit();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));

        ActionDecl {
            name: self.process_action_name(),
            params: vec![path_ty, data_ty, bit_ty],
            results: vec![path_ty, data_ty, u64_ty, u64_ty],
        }
    }

    /// Create the [`ActionDecl`] for the "evict path" action.
    ///
    /// The evict action is used for deterministic reverse-lexicographic
    /// eviction. After the main access, the circuit reads the tree path at
    /// an eviction leaf (plaintext — from the process action output), sends
    /// the path to the client, and the client moves stash entries into
    /// buckets whose subtree covers the entry's assigned leaf.
    ///
    /// - **Params**: `[path_ty]` — the path buckets read from the tree
    /// - **Results**: `[path_ty]` — the updated path after eviction
    ///
    /// Two eviction passes occur per access (the process action returns two
    /// eviction leaf indices).
    pub fn evict_action_decl(&self, types: &mut IRTypes) -> ActionDecl {
        let path_ty = self.path_type(types);
        ActionDecl {
            name: self.evict_action_name(),
            params: vec![path_ty],
            results: vec![path_ty],
        }
    }

    /// Create both [`ActionDecl`]s for this ORAM instance.
    pub fn action_decls(&self, types: &mut IRTypes) -> (ActionDecl, ActionDecl, ActionDecl) {
        let begin = self.begin_action_decl(types);
        let process = self.process_action_decl(types);
        let evict = self.evict_action_decl(types);
        (begin, process, evict)
    }

    // -- FheActionConfig construction ----------------------------------------

    /// Create the [`FheActionConfig`] for the "begin" action.
    ///
    /// The single output (leaf index) is **plaintext** — the ORAM guarantee
    /// ensures the leaf is uniformly random and independent of the address.
    pub fn begin_action_config(&self) -> FheActionConfig {
        FheActionConfig {
            output_public: vec![true],
        }
    }

    /// Create the [`FheActionConfig`] for the "process path" action.
    ///
    /// - Output 0 (write-back path buckets): **encrypted**
    /// - Output 1 (read data): **encrypted**
    /// - Output 2 (eviction leaf 1): **plaintext**
    /// - Output 3 (eviction leaf 2): **plaintext**
    pub fn process_action_config(&self) -> FheActionConfig {
        FheActionConfig {
            output_public: vec![false, false, true, true],
        }
    }

    /// Create the [`FheActionConfig`] for the "evict path" action.
    ///
    /// The single output (updated eviction path) is **encrypted**.
    pub fn evict_action_config(&self) -> FheActionConfig {
        FheActionConfig {
            output_public: vec![false],
        }
    }

    /// Create all action configs as `(name, config)` pairs, suitable for
    /// registering with [`TfheScheme::with_action_config`].
    pub fn action_configs(&self) -> [(String, FheActionConfig); 3] {
        [
            (self.begin_action_name(), self.begin_action_config()),
            (self.process_action_name(), self.process_action_config()),
            (self.evict_action_name(), self.evict_action_config()),
        ]
    }

    /// Register this ORAM instance's action configs with a [`TfheScheme`].
    ///
    /// Convenience method that calls `with_action_config` for the
    /// begin, process, and evict actions.
    pub fn configure_scheme(&self, scheme: crate::fhe::TfheScheme) -> crate::fhe::TfheScheme {
        let [(n1, c1), (n2, c2), (n3, c3)] = self.action_configs();
        scheme
            .with_action_config(n1, c1)
            .with_action_config(n2, c2)
            .with_action_config(n3, c3)
    }

    /// Return the tree storage cell count for a given `StorageId`.
    ///
    /// Returns `Some(num_nodes)` if `sid` is this ORAM's tree storage
    /// (`ORAM_TREE_BASE + storage_id`), `None` otherwise.
    pub fn tree_cell_count(&self, sid: StorageId) -> Option<usize> {
        if sid.0 == ORAM_TREE_BASE + self.storage_id {
            Some(self.num_nodes())
        } else {
            None
        }
    }

    /// Add this ORAM configuration's const-generic parameters to a [`MonoEnv`].
    ///
    /// Inserts `Z`, `B`, `L`, and `N` (computed as `2^L − 1`) so that
    /// monomorphization can resolve the corresponding `ArrayLength::TypeParam`
    /// entries in linked `volar-oram-core` types.
    ///
    /// ```ignore
    /// let env = MonoEnv::new("")
    ///     .with_len("N_LWE", 630)  // TFHE params…
    ///     .with_len("BIG_N", 2048);
    /// let env = config.apply_mono(env);
    /// // env now also has Z=4, B=16, L=4, N=15
    /// ```
    pub fn apply_mono(&self, env: volar_lir_codegen::mono::MonoEnv) -> volar_lir_codegen::mono::MonoEnv {
        env.with_len("Z", self.z)
            .with_len("B", self.b)
            .with_len("L", self.l)
            .with_len("N", self.num_nodes())
    }
}

/// Build the `cell_count_fn` closure for [`derive_ir_storage_config`]
/// from a slice of ORAM configurations.
///
/// For ORAM tree storages (`StorageId >= ORAM_TREE_BASE`), returns the
/// corresponding tree's node count. For all other storages, returns 1.
///
/// # Example
///
/// ```ignore
/// let configs = &[OramConfig { storage_id: 0, z: 4, b: 16, l: 4 }];
/// let storage_config = derive_ir_storage_config(&rewritten,
///     Some(&oram_cell_count_fn(configs)));
/// ```
pub fn oram_cell_count_fn(configs: &[OramConfig]) -> impl Fn(StorageId, IRTypeId) -> usize + '_ {
    move |sid: StorageId, _ty: IRTypeId| {
        for c in configs {
            if let Some(count) = c.tree_cell_count(sid) {
                return count;
            }
        }
        1
    }
}

// ============================================================================
// ORAM circuit construction helpers
// ============================================================================

/// Construct a minimal `IRBlocks` that performs an ORAM begin action.
///
/// The circuit takes a single `u64` input (the encrypted application address),
/// invokes the `oram_begin` action to get a plaintext leaf from the position
/// map, and returns the leaf.
///
/// This is primarily for testing the ActionCall wiring — it exercises the
/// action declaration, the FheActionConfig, and the publicness propagation
/// through the weaver.
///
/// Returns `(ir_blocks, types)`.
pub fn oram_begin_circuit(config: &OramConfig) -> (IRBlocks, IRTypes) {

    let mut types = IRTypes::new();
    let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
    let bit_ty = types.bit();

    let begin_decl = config.begin_action_decl(&mut types);
    let result_ty = types.intern(IRType::Tuple(vec![u64_ty]));

    // Block layout (1 param + 4 stmts):
    //   v0 = param: u64          (the encrypted address)
    //   v1 = Const(1, Bit)       (guard — always execute)
    //   v2 = Const(0, u64)       (fallback leaf value)
    //   v3 = ActionCall("oram_begin_N", guard=v1, args=[v0], fallbacks=[v2])
    //   v4 = ActionOutput(call=v3, idx=0)  (the plaintext leaf)
    //   return v4
    let stmts = vec![
        IRStmt::Const(Constant { hi: 0, lo: 1 }, bit_ty),
        IRStmt::Const(Constant { hi: 0, lo: 0 }, u64_ty),
        IRStmt::ActionCall {
            name: config.begin_action_name(),
            guard: IRVarId(1),
            args: vec![IRVarId(0)],
            fallbacks: vec![IRVarId(2)],
            output_tys: vec![u64_ty],
            result_ty,
        },
        IRStmt::ActionOutput {
            call: IRVarId(3),
            idx: 0,
            ty: u64_ty,
        },
    ];

    let block = IRBlock {
        params: vec![u64_ty],
        stmts,
        stmt_provs: vec![(); 4],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: vec![IRVarId(4)],
        },
    };

    let ir = IRBlocks {
        oracles: vec![],
        actions: vec![begin_decl],
        rngs: vec![],
        blocks: vec![block],
    };

    (ir, types)
}

// ============================================================================
// Storage-to-ORAM transformation
// ============================================================================

/// Pre-interned IR type IDs for a single ORAM configuration.
struct ConfigTypes {
    path_ty: IRTypeId,
    data_ty: IRTypeId,
    begin_result_ty: IRTypeId,
    process_result_ty: IRTypeId,
    evict_result_ty: IRTypeId,
}

/// Rewrite `StorageRead`/`StorageWrite` on ORAM-backed regions into
/// ActionCall sequences with deterministic eviction.
///
/// For each storage region described by an [`OramConfig`], this
/// transformation replaces every `StorageRead`/`StorageWrite` that
/// targets that region with the following IR sequence:
///
/// **StorageRead(S, T, addr) →**
/// 1. `guard  = Const(1, Bit)`
/// 2. `fb_leaf = Const(0, u64)`
/// 3. `begin  = ActionCall("oram_begin_S", guard, [addr], [fb_leaf])`
/// 4. `leaf   = ActionOutput(begin, 0, u64)` — plaintext leaf
/// 5. `path   = StorageRead(ORAM_TREE_S, path_ty, leaf)` — direct indexed
/// 6. `zero_data = Const(0, data_ty)`
/// 7. `zero_bit  = Const(0, Bit)`
/// 8. `fb_path  = Const(0, path_ty)` … (fallbacks)
/// 9. `process = ActionCall("oram_process_S", guard, [path, zero_data, zero_bit], [fb_*])`
/// 10. `wb_path = ActionOutput(process, 0, path_ty)` — write-back buckets
/// 11. `rd_data = ActionOutput(process, 1, data_ty)` — **the read result**
/// 12. `evict1  = ActionOutput(process, 2, u64)`
/// 13. `evict2  = ActionOutput(process, 3, u64)`
/// 14. `StorageWrite(ORAM_TREE_S, wb_path, path_ty, leaf)` — write back
/// 15–19. Eviction pass 1 (read/evict/write at evict1)
/// 20–24. Eviction pass 2 (read/evict/write at evict2)
///
/// Each eviction pass emits 5 statements:
/// - `evict_path = StorageRead(ORAM_TREE_S, path_ty, evict_leaf)`
/// - `fb = Const(0, path_ty)`
/// - `evict_call = ActionCall("oram_evict_S", guard, [evict_path], [fb])`
/// - `evict_result = ActionOutput(evict_call, 0, path_ty)`
/// - `StorageWrite(ORAM_TREE_S, evict_result, path_ty, evict_leaf)`
///
/// **StorageWrite(S, src, T, addr) →**
/// Same as above, but with `is_write = Const(1, Bit)` and `src` as the
/// write data. The original statement's result is the dummy zero.
///
/// The original storage operations on non-ORAM regions are left untouched.
/// All subsequent `IRVarId` references in the block are remapped to
/// account for the expanded statement count.
///
/// # Tree storage convention
///
/// ORAM tree storage uses `StorageId(1000 + storage_id)` to avoid
/// collisions with the original storage namespace. The `path_ty` type
/// acts as the element type for tree reads/writes.
///
/// # Eviction
///
/// Two deterministic eviction passes per access, using reverse-
/// lexicographic order. The eviction leaf indices are computed by the
/// client and returned as plaintext outputs of the process action.
/// Each eviction pass reads the tree path at the eviction leaf, sends
/// it to the client via the `oram_evict_S` action, and writes back
/// the updated path.
///
/// # Returns
///
/// A new `IRBlocks` with ORAM-backed storage ops replaced, and the
/// `types` table updated with any newly interned types. The action
/// declarations from each `OramConfig` are merged into the output.
pub fn rewrite_storage_to_oram<P: Clone + Default>(
    ir: &IRBlocks<P>,
    types: &mut IRTypes,
    configs: &[OramConfig],
) -> IRBlocks<P> {
    // Build a lookup from StorageId → OramConfig index.
    let oram_map: BTreeMap<u32, usize> = configs
        .iter()
        .enumerate()
        .map(|(i, c)| (c.storage_id, i))
        .collect();

    // If no configs, return a clone unchanged (no-op).
    if oram_map.is_empty() {
        return ir.clone();
    }

    // Pre-intern shared types.
    let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
    let bit_ty = types.bit();

    // Pre-intern per-config types.
    let config_types: Vec<ConfigTypes> = configs
        .iter()
        .map(|c| {
            let path_ty = c.path_type(types);
            let data_ty = c.data_type(types);
            let begin_result_ty = types.intern(IRType::Tuple(vec![u64_ty]));
            let process_result_ty =
                types.intern(IRType::Tuple(vec![path_ty, data_ty, u64_ty, u64_ty]));
            let evict_result_ty = types.intern(IRType::Tuple(vec![path_ty]));
            ConfigTypes { path_ty, data_ty, begin_result_ty, process_result_ty, evict_result_ty }
        })
        .collect();

    // Collect action declarations from all configs.
    let mut new_actions: Vec<ActionDecl> = ir.actions.clone();
    for c in configs {
        let (begin_decl, process_decl, evict_decl) = c.action_decls(types);
        // Avoid duplicates if actions were already declared.
        if !new_actions.iter().any(|a| a.name == begin_decl.name) {
            new_actions.push(begin_decl);
        }
        if !new_actions.iter().any(|a| a.name == process_decl.name) {
            new_actions.push(process_decl);
        }
        if !new_actions.iter().any(|a| a.name == evict_decl.name) {
            new_actions.push(evict_decl);
        }
    }

    // Rewrite each block.
    let new_blocks: Vec<IRBlock<P>> = ir
        .blocks
        .iter()
        .map(|block| {
            rewrite_block(
                block, &oram_map, configs, &config_types, types, u64_ty, bit_ty,
            )
        })
        .collect();

    IRBlocks {
        oracles: ir.oracles.clone(),
        actions: new_actions,
        rngs: ir.rngs.clone(),
        blocks: new_blocks,
    }
}

/// ORAM tree storage uses `StorageId(1000 + storage_id)`.
const ORAM_TREE_BASE: u32 = 1000;

/// Rewrite a single IR block, replacing ORAM-backed storage ops.
fn rewrite_block<P: Clone + Default>(
    block: &IRBlock<P>,
    oram_map: &BTreeMap<u32, usize>,
    configs: &[OramConfig],
    config_types: &[ConfigTypes],
    types: &mut IRTypes,
    u64_ty: IRTypeId,
    bit_ty: IRTypeId,
) -> IRBlock<P> {
    let num_params = block.params.len() as u32;

    // Phase 1: Build the new statement list and a var-remap table.
    //
    // For each original statement at index `i` (producing IRVarId(num_params + i)),
    // we emit one or more new statements. The var-remap maps old IRVarId → new IRVarId.
    let mut new_stmts: Vec<IRStmt> = Vec::new();
    let mut new_provs: Vec<P> = Vec::new();
    let mut var_remap: BTreeMap<u32, u32> = BTreeMap::new();

    // Block params keep their IDs (0..num_params).
    for p in 0..num_params {
        var_remap.insert(p, p);
    }

    for (stmt_idx, (stmt, prov)) in block.stmts.iter().zip(block.stmt_provs.iter()).enumerate() {
        let old_var = num_params + stmt_idx as u32;

        match stmt {
            IRStmt::StorageRead { storage, ty: _data_elem_ty, addr }
                if oram_map.contains_key(&storage.0) =>
            {
                let cfg_idx = oram_map[&storage.0];
                let c = &configs[cfg_idx];
                let ct = &config_types[cfg_idx];

                let remapped_addr = IRVarId(var_remap[&addr.0]);

                // Emit the ORAM read expansion. Returns the new IRVarId
                // for the read data result.
                let result_var = emit_oram_access(
                    &mut new_stmts,
                    &mut new_provs,
                    prov,
                    c,
                    ct,
                    types,
                    u64_ty,
                    bit_ty,
                    num_params,
                    remapped_addr,
                    None, // No write data → read mode
                );

                var_remap.insert(old_var, result_var);
            }

            IRStmt::StorageWrite { storage, src, ty: _data_elem_ty, addr }
                if oram_map.contains_key(&storage.0) =>
            {
                let cfg_idx = oram_map[&storage.0];
                let c = &configs[cfg_idx];
                let ct = &config_types[cfg_idx];

                let remapped_addr = IRVarId(var_remap[&addr.0]);
                let remapped_src = IRVarId(var_remap[&src.0]);

                // Emit the ORAM write expansion. Returns a dummy var
                // (the StorageWrite result is unused / zero).
                let result_var = emit_oram_access(
                    &mut new_stmts,
                    &mut new_provs,
                    prov,
                    c,
                    ct,
                    types,
                    u64_ty,
                    bit_ty,
                    num_params,
                    remapped_addr,
                    Some(remapped_src), // Write data → write mode
                );

                var_remap.insert(old_var, result_var);
            }

            // Non-ORAM statement — emit as-is with remapped vars.
            other => {
                let remapped = remap_stmt(other, &var_remap);
                new_stmts.push(remapped);
                new_provs.push(prov.clone());
                var_remap.insert(old_var, num_params + new_stmts.len() as u32 - 1);
            }
        }
    }

    // Phase 2: Remap the terminator.
    let new_terminator = remap_terminator(&block.terminator, &var_remap);

    IRBlock {
        params: block.params.clone(),
        stmts: new_stmts,
        stmt_provs: new_provs,
        terminator: new_terminator,
    }
}

/// Emit the ORAM ActionCall sequence for a single storage access,
/// including two deterministic eviction passes.
///
/// If `write_data` is `None`, this is a read; otherwise a write.
///
/// Returns the `IRVarId` for the **read data** result (for reads) or
/// a **dummy zero** (for writes).
///
/// The sequence emitted is:
/// 1. **Begin**: send address → get plaintext leaf (4 stmts)
/// 2. **Read tree path** at plaintext leaf (1 stmt)
/// 3. **Process**: send path + data + is_write → get write-back path,
///    read data, eviction leaf 1, eviction leaf 2 (8–9 stmts)
/// 4. **Write-back**: store updated path at leaf (1 stmt)
/// 5. **Eviction pass 1**: read tree at evict_leaf_1, send to client,
///    write back evicted path (5 stmts)
/// 6. **Eviction pass 2**: same for evict_leaf_2 (5 stmts)
///
/// Total: 27 statements for a read, 26 for a write (write reuses
/// the caller-provided data var instead of emitting a zero constant).
fn emit_oram_access<P: Clone + Default>(
    stmts: &mut Vec<IRStmt>,
    provs: &mut Vec<P>,
    prov: &P,
    config: &OramConfig,
    ct: &ConfigTypes,
    _types: &mut IRTypes,
    u64_ty: IRTypeId,
    bit_ty: IRTypeId,
    num_params: u32,
    addr_var: IRVarId,
    write_data: Option<IRVarId>,
) -> u32 {
    let push = |stmt: IRStmt, provs_vec: &mut Vec<P>, stmts_vec: &mut Vec<IRStmt>| -> u32 {
        let id = num_params + stmts_vec.len() as u32;
        stmts_vec.push(stmt);
        provs_vec.push(prov.clone());
        id
    };

    let tree_storage = StorageId(ORAM_TREE_BASE + config.storage_id);

    // --- Begin action: addr → leaf ---

    // v_guard = Const(1, Bit)
    let v_guard = push(IRStmt::Const(Constant { hi: 0, lo: 1 }, bit_ty), provs, stmts);

    // v_fb_leaf = Const(0, u64) — fallback
    let v_fb_leaf = push(IRStmt::Const(Constant { hi: 0, lo: 0 }, u64_ty), provs, stmts);

    // v_begin = ActionCall("oram_begin_S", ...)
    let v_begin = push(
        IRStmt::ActionCall {
            name: config.begin_action_name(),
            guard: IRVarId(v_guard),
            args: vec![addr_var],
            fallbacks: vec![IRVarId(v_fb_leaf)],
            output_tys: vec![u64_ty],
            result_ty: ct.begin_result_ty,
        },
        provs,
        stmts,
    );

    // v_leaf = ActionOutput(begin, 0, u64) — plaintext leaf
    let v_leaf = push(
        IRStmt::ActionOutput { call: IRVarId(v_begin), idx: 0, ty: u64_ty },
        provs,
        stmts,
    );

    // --- Read tree path at plaintext leaf ---

    // v_path = StorageRead(ORAM_TREE_S, path_ty, leaf)
    let v_path = push(
        IRStmt::StorageRead {
            storage: tree_storage,
            ty: ct.path_ty,
            addr: IRVarId(v_leaf),
        },
        provs,
        stmts,
    );

    // --- Process action: path, data, is_write → wb_path, data, evict1, evict2 ---

    // Write data or zero (for reads)
    let v_data_arg = match write_data {
        Some(src) => src.0,
        None => push(
            IRStmt::Const(Constant { hi: 0, lo: 0 }, ct.data_ty),
            provs,
            stmts,
        ),
    };

    // is_write flag
    let is_write_val = if write_data.is_some() { 1u128 } else { 0u128 };
    let v_is_write = push(
        IRStmt::Const(Constant { hi: 0, lo: is_write_val }, bit_ty),
        provs,
        stmts,
    );

    // Fallbacks for process action (4 outputs: path, data, u64, u64)
    let v_fb_path = push(
        IRStmt::Const(Constant { hi: 0, lo: 0 }, ct.path_ty),
        provs,
        stmts,
    );
    let v_fb_data = push(
        IRStmt::Const(Constant { hi: 0, lo: 0 }, ct.data_ty),
        provs,
        stmts,
    );
    let v_fb_ev1 = push(
        IRStmt::Const(Constant { hi: 0, lo: 0 }, u64_ty),
        provs,
        stmts,
    );
    let v_fb_ev2 = push(
        IRStmt::Const(Constant { hi: 0, lo: 0 }, u64_ty),
        provs,
        stmts,
    );

    // v_process = ActionCall("oram_process_S", ...)
    let v_process = push(
        IRStmt::ActionCall {
            name: config.process_action_name(),
            guard: IRVarId(v_guard),
            args: vec![IRVarId(v_path), IRVarId(v_data_arg), IRVarId(v_is_write)],
            fallbacks: vec![
                IRVarId(v_fb_path),
                IRVarId(v_fb_data),
                IRVarId(v_fb_ev1),
                IRVarId(v_fb_ev2),
            ],
            output_tys: vec![ct.path_ty, ct.data_ty, u64_ty, u64_ty],
            result_ty: ct.process_result_ty,
        },
        provs,
        stmts,
    );

    // Project process outputs
    let v_wb_path = push(
        IRStmt::ActionOutput { call: IRVarId(v_process), idx: 0, ty: ct.path_ty },
        provs,
        stmts,
    );
    let v_rd_data = push(
        IRStmt::ActionOutput { call: IRVarId(v_process), idx: 1, ty: ct.data_ty },
        provs,
        stmts,
    );
    let v_evict1 = push(
        IRStmt::ActionOutput { call: IRVarId(v_process), idx: 2, ty: u64_ty },
        provs,
        stmts,
    );
    let v_evict2 = push(
        IRStmt::ActionOutput { call: IRVarId(v_process), idx: 3, ty: u64_ty },
        provs,
        stmts,
    );

    // --- Write back updated path ---

    // StorageWrite(ORAM_TREE_S, wb_path, path_ty, leaf)
    let _v_wb = push(
        IRStmt::StorageWrite {
            storage: tree_storage,
            src: IRVarId(v_wb_path),
            ty: ct.path_ty,
            addr: IRVarId(v_leaf),
        },
        provs,
        stmts,
    );

    // --- Eviction pass 1: read path at evict_leaf_1, evict, write back ---
    emit_eviction_pass(
        stmts, provs, prov, config, ct, num_params,
        tree_storage, v_guard, v_evict1,
    );

    // --- Eviction pass 2: read path at evict_leaf_2, evict, write back ---
    emit_eviction_pass(
        stmts, provs, prov, config, ct, num_params,
        tree_storage, v_guard, v_evict2,
    );

    // The result for the original StorageRead is the read data.
    // For StorageWrite, the result is the read data too (caller discards it
    // since StorageWrite produces a dummy zero — but we return a meaningful
    // var ID so the remap table is valid).
    v_rd_data
}

/// Emit a single eviction pass: read tree path → evict action → write back.
///
/// 5 statements:
/// 1. `evict_path = StorageRead(ORAM_TREE_S, path_ty, evict_leaf)`
/// 2. `fb_evict = Const(0, path_ty)` — fallback
/// 3. `evict_call = ActionCall("oram_evict_S", guard, [evict_path], [fb_evict])`
/// 4. `evict_result = ActionOutput(evict_call, 0, path_ty)`
/// 5. `StorageWrite(ORAM_TREE_S, evict_result, path_ty, evict_leaf)`
fn emit_eviction_pass<P: Clone + Default>(
    stmts: &mut Vec<IRStmt>,
    provs: &mut Vec<P>,
    prov: &P,
    config: &OramConfig,
    ct: &ConfigTypes,
    num_params: u32,
    tree_storage: StorageId,
    v_guard: u32,
    v_evict_leaf: u32,
) {
    let push = |stmt: IRStmt, provs_vec: &mut Vec<P>, stmts_vec: &mut Vec<IRStmt>| -> u32 {
        let id = num_params + stmts_vec.len() as u32;
        stmts_vec.push(stmt);
        provs_vec.push(prov.clone());
        id
    };

    // Read tree path at eviction leaf
    let v_evict_path = push(
        IRStmt::StorageRead {
            storage: tree_storage,
            ty: ct.path_ty,
            addr: IRVarId(v_evict_leaf),
        },
        provs,
        stmts,
    );

    // Fallback for evict action
    let v_fb_evict = push(
        IRStmt::Const(Constant { hi: 0, lo: 0 }, ct.path_ty),
        provs,
        stmts,
    );

    // Evict action call
    let v_evict_call = push(
        IRStmt::ActionCall {
            name: config.evict_action_name(),
            guard: IRVarId(v_guard),
            args: vec![IRVarId(v_evict_path)],
            fallbacks: vec![IRVarId(v_fb_evict)],
            output_tys: vec![ct.path_ty],
            result_ty: ct.evict_result_ty,
        },
        provs,
        stmts,
    );

    // Project evict result
    let v_evict_result = push(
        IRStmt::ActionOutput { call: IRVarId(v_evict_call), idx: 0, ty: ct.path_ty },
        provs,
        stmts,
    );

    // Write back evicted path
    let _v_evict_wb = push(
        IRStmt::StorageWrite {
            storage: tree_storage,
            src: IRVarId(v_evict_result),
            ty: ct.path_ty,
            addr: IRVarId(v_evict_leaf),
        },
        provs,
        stmts,
    );
}

// ============================================================================
// Var remapping helpers
// ============================================================================

/// Remap variable references in a statement using the given lookup table.
fn remap_stmt(stmt: &IRStmt, remap: &BTreeMap<u32, u32>) -> IRStmt {
    let rv = |v: &IRVarId| -> IRVarId { IRVarId(remap[&v.0]) };

    match stmt {
        IRStmt::StorageRead { storage, ty, addr } => IRStmt::StorageRead {
            storage: *storage,
            ty: *ty,
            addr: rv(addr),
        },
        IRStmt::StorageWrite { storage, src, ty, addr } => IRStmt::StorageWrite {
            storage: *storage,
            src: rv(src),
            ty: *ty,
            addr: rv(addr),
        },
        IRStmt::Const(c, t) => IRStmt::Const(*c, *t),
        IRStmt::Transmute { src, src_ty, dst_ty } => IRStmt::Transmute {
            src: rv(src),
            src_ty: *src_ty,
            dst_ty: *dst_ty,
        },
        IRStmt::Poly { ty, coeffs, constant } => {
            let new_coeffs = coeffs
                .iter()
                .map(|(vars, coeff)| {
                    let new_vars: Vec<IRVarId> = vars.iter().map(|v| rv(v)).collect();
                    (new_vars, *coeff)
                })
                .collect();
            IRStmt::Poly { ty: *ty, coeffs: new_coeffs, constant: *constant }
        }
        IRStmt::Rol { src, ty, n } => IRStmt::Rol {
            src: rv(src),
            ty: *ty,
            n: *n,
        },
        IRStmt::Ror { src, ty, n } => IRStmt::Ror {
            src: rv(src),
            ty: *ty,
            n: *n,
        },
        IRStmt::Merge { parts, ty } => {
            let new_parts: Vec<IRVarId> = parts.iter().map(|v| rv(v)).collect();
            IRStmt::Merge { parts: new_parts, ty: *ty }
        }
        IRStmt::Splat { src, ty } => IRStmt::Splat {
            src: rv(src),
            ty: *ty,
        },
        IRStmt::Shuffle { result_bits, ty } => {
            let new_bits: Vec<(u8, IRVarId)> =
                result_bits.iter().map(|(bit, v)| (*bit, rv(v))).collect();
            IRStmt::Shuffle { result_bits: new_bits, ty: *ty }
        }
        IRStmt::OracleCall { name, args, output_tys, result_ty } => {
            IRStmt::OracleCall {
                name: name.clone(),
                args: args.iter().map(|v| rv(v)).collect(),
                output_tys: output_tys.clone(),
                result_ty: *result_ty,
            }
        }
        IRStmt::OracleOutput { call, idx, ty } => IRStmt::OracleOutput {
            call: rv(call),
            idx: *idx,
            ty: *ty,
        },
        IRStmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } => {
            IRStmt::ActionCall {
                name: name.clone(),
                guard: rv(guard),
                args: args.iter().map(|v| rv(v)).collect(),
                fallbacks: fallbacks.iter().map(|v| rv(v)).collect(),
                output_tys: output_tys.clone(),
                result_ty: *result_ty,
            }
        }
        IRStmt::ActionOutput { call, idx, ty } => IRStmt::ActionOutput {
            call: rv(call),
            idx: *idx,
            ty: *ty,
        },
        IRStmt::Rng { name, ty } => IRStmt::Rng {
            name: name.clone(),
            ty: *ty,
        },
    }
}

/// Remap variable references in a terminator.
fn remap_terminator(term: &IRTerminator, remap: &BTreeMap<u32, u32>) -> IRTerminator {
    let rv = |v: &IRVarId| -> IRVarId { IRVarId(remap[&v.0]) };
    let rargs = |args: &[IRVarId]| -> Vec<IRVarId> { args.iter().map(|v| rv(v)).collect() };

    match term {
        IRTerminator::Jmp { func, args } => IRTerminator::Jmp {
            func: func.clone(),
            args: rargs(args),
        },
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => IRTerminator::JumpCond {
            condition: rv(condition),
            true_block: true_block.clone(),
            true_args: rargs(true_args),
            false_block: false_block.clone(),
            false_args: rargs(false_args),
        },
        IRTerminator::JumpTable { index, cases } => IRTerminator::JumpTable {
            index: rv(index),
            cases: cases
                .iter()
                .map(|(k, (target, args))| (*k, (target.clone(), rargs(args))))
                .collect(),
        },
    }
}

/// Parse the `volar-oram-core` server-side source and return a [`LinkedSpec`].
///
/// This uses `include_str!` to embed the source at compile time, then
/// parses it with the volar-compiler parser. The resulting `LinkedSpec`
/// can be merged into any target `IrModule` via the linkage system.
///
/// Inner attributes (`#![...]`) are stripped before parsing since the
/// compiler parser handles outer attributes only.
///
/// Requires the `linking` feature.
#[cfg(feature = "linking")]
pub fn oram_linked_spec() -> LinkedSpec {
    use volar_compiler::parser::{SourceInput, parse_sources};
    let source = include_str!("../../../oram/volar-oram-core/src/lib.rs");
    let filtered = strip_inner_attributes(source);
    let module = parse_sources(&[SourceInput { source: &filtered, name: "oram_core.rs" }], "oram_core")
        .expect("oram_linked_spec: failed to parse volar-oram-core/src/lib.rs");
    LinkedSpec {
        name: "oram_core".into(),
        module,
    }
}

/// Strip inner attributes (`#![...]`) and inner doc comments (`//!`)
/// from source code.
///
/// The volar-compiler parser handles outer attributes (`#[derive(...)]`)
/// but not inner attributes. This function removes them line-by-line
/// so the source can be parsed.
#[cfg(feature = "linking")]
fn strip_inner_attributes(source: &str) -> alloc::string::String {
    use alloc::string::String;
    let mut result = String::new();
    for line in source.split('\n') {
        let trimmed = line.trim_start();
        if trimmed.starts_with("#![") || trimmed.starts_with("//!") {
            // Skip inner attribute and inner doc comment lines
            continue;
        }
        result.push_str(line);
        result.push('\n');
    }
    result
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests_config {
    use super::*;
    use volar_ir::ir::{IRType, IRTypes, PrimType};

    /// Standard test config: Z=4, B=16, L=4 (8 leaves, 15 nodes).
    fn test_config() -> OramConfig {
        OramConfig { storage_id: 0, z: 4, b: 16, l: 4 }
    }

    #[test]
    fn derived_values() {
        let c = test_config();
        assert_eq!(c.num_nodes(), 15);
        assert_eq!(c.num_leaves(), 8);
        // entry = 64 + 64 + 8*16 = 256 bits
        assert_eq!(c.entry_bits(), 256);
        // bucket = 4 * 256 = 1024 bits
        assert_eq!(c.bucket_bits(), 1024);
        // path = 4 * 1024 = 4096 bits
        assert_eq!(c.path_bits(), 4096);
    }

    #[test]
    fn action_names() {
        let c = test_config();
        assert_eq!(c.begin_action_name(), "oram_begin_0");
        assert_eq!(c.process_action_name(), "oram_process_0");

        let c2 = OramConfig { storage_id: 3, ..c };
        assert_eq!(c2.begin_action_name(), "oram_begin_3");
        assert_eq!(c2.process_action_name(), "oram_process_3");
    }

    #[test]
    fn ir_types_are_well_formed() {
        let c = test_config();
        let mut types = IRTypes::new();

        let entry_ty = c.entry_type(&mut types);
        let bucket_ty = c.bucket_type(&mut types);
        let path_ty = c.path_type(&mut types);
        let data_ty = c.data_type(&mut types);

        // entry = Tuple(u64, u64, Vec(16, u8))
        match &types.0[entry_ty.0 as usize] {
            IRType::Tuple(fields) => {
                assert_eq!(fields.len(), 3);
                assert_eq!(types.0[fields[0].0 as usize], IRType::Primitive(PrimType::_64));
                assert_eq!(types.0[fields[1].0 as usize], IRType::Primitive(PrimType::_64));
                match &types.0[fields[2].0 as usize] {
                    IRType::Vec(len, elem) => {
                        assert_eq!(*len, 16);
                        assert_eq!(types.0[elem.0 as usize], IRType::Primitive(PrimType::_8));
                    }
                    other => panic!("expected Vec for data field, got {:?}", other),
                }
            }
            other => panic!("expected Tuple for entry, got {:?}", other),
        }

        // bucket = Vec(4, entry)
        match &types.0[bucket_ty.0 as usize] {
            IRType::Vec(len, elem) => {
                assert_eq!(*len, 4);
                assert_eq!(*elem, entry_ty);
            }
            other => panic!("expected Vec for bucket, got {:?}", other),
        }

        // path = Vec(4, bucket)
        match &types.0[path_ty.0 as usize] {
            IRType::Vec(len, elem) => {
                assert_eq!(*len, 4);
                assert_eq!(*elem, bucket_ty);
            }
            other => panic!("expected Vec for path, got {:?}", other),
        }

        // data = Vec(16, u8)
        match &types.0[data_ty.0 as usize] {
            IRType::Vec(len, elem) => {
                assert_eq!(*len, 16);
                assert_eq!(types.0[elem.0 as usize], IRType::Primitive(PrimType::_8));
            }
            other => panic!("expected Vec for data, got {:?}", other),
        }
    }

    #[test]
    fn begin_action_decl_shape() {
        let c = test_config();
        let mut types = IRTypes::new();
        let decl = c.begin_action_decl(&mut types);

        assert_eq!(decl.name, "oram_begin_0");
        assert_eq!(decl.params.len(), 1, "begin takes one param (address)");
        assert_eq!(decl.results.len(), 1, "begin returns one result (leaf)");
        // Both should be u64
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        assert_eq!(decl.params[0], u64_ty);
        assert_eq!(decl.results[0], u64_ty);
    }

    #[test]
    fn process_action_decl_shape() {
        let c = test_config();
        let mut types = IRTypes::new();
        let decl = c.process_action_decl(&mut types);

        assert_eq!(decl.name, "oram_process_0");
        assert_eq!(decl.params.len(), 3, "process takes 3 params (path, data, is_write)");
        assert_eq!(decl.results.len(), 4, "process returns 4 results (path, data, leaf, leaf)");

        let path_ty = c.path_type(&mut types);
        let data_ty = c.data_type(&mut types);
        let bit_ty = types.bit();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));

        // Params
        assert_eq!(decl.params[0], path_ty);
        assert_eq!(decl.params[1], data_ty);
        assert_eq!(decl.params[2], bit_ty);

        // Results
        assert_eq!(decl.results[0], path_ty);
        assert_eq!(decl.results[1], data_ty);
        assert_eq!(decl.results[2], u64_ty);
        assert_eq!(decl.results[3], u64_ty);
    }

    #[test]
    fn begin_action_config_all_public() {
        let c = test_config();
        let cfg = c.begin_action_config();
        assert!(cfg.is_output_public(0), "leaf output must be public");
        assert!(cfg.all_outputs_public());
    }

    #[test]
    fn process_action_config_mixed_public() {
        let c = test_config();
        let cfg = c.process_action_config();
        assert!(!cfg.is_output_public(0), "write-back path must be encrypted");
        assert!(!cfg.is_output_public(1), "read data must be encrypted");
        assert!(cfg.is_output_public(2), "eviction leaf 1 must be public");
        assert!(cfg.is_output_public(3), "eviction leaf 2 must be public");
        assert!(!cfg.all_outputs_public());
    }

    #[test]
    fn action_configs_pair() {
        let c = test_config();
        let configs = c.action_configs();
        assert_eq!(configs[0].0, "oram_begin_0");
        assert_eq!(configs[1].0, "oram_process_0");
        assert_eq!(configs[2].0, "oram_evict_0");
        assert!(configs[0].1.all_outputs_public());
        assert!(!configs[1].1.all_outputs_public());
        assert!(!configs[2].1.all_outputs_public());
    }

    #[test]
    fn type_interning_is_deduplicated() {
        let c = test_config();
        let mut types = IRTypes::new();

        // Calling entry_type twice should return the same TypeId
        let e1 = c.entry_type(&mut types);
        let e2 = c.entry_type(&mut types);
        assert_eq!(e1, e2, "entry_type should be deduplicated by intern");

        let b1 = c.bucket_type(&mut types);
        let b2 = c.bucket_type(&mut types);
        assert_eq!(b1, b2, "bucket_type should be deduplicated");
    }

    #[test]
    fn different_storage_ids_give_different_names() {
        let c0 = OramConfig { storage_id: 0, z: 4, b: 16, l: 4 };
        let c1 = OramConfig { storage_id: 1, z: 4, b: 16, l: 4 };

        assert_ne!(c0.begin_action_name(), c1.begin_action_name());
        assert_ne!(c0.process_action_name(), c1.process_action_name());
    }

    #[test]
    fn small_oram_config() {
        // Minimal: Z=1, B=1, L=2 (2 leaves, 3 nodes)
        let c = OramConfig { storage_id: 0, z: 1, b: 1, l: 2 };
        assert_eq!(c.num_nodes(), 3);
        assert_eq!(c.num_leaves(), 2);
        assert_eq!(c.entry_bits(), 136); // 64 + 64 + 8
        assert_eq!(c.bucket_bits(), 136);
        assert_eq!(c.path_bits(), 272);

        let mut types = IRTypes::new();
        let (begin, process, evict) = c.action_decls(&mut types);
        assert_eq!(begin.params.len(), 1);
        assert_eq!(process.results.len(), 4);
        assert_eq!(evict.params.len(), 1, "evict takes 1 arg (path)");
        assert_eq!(evict.results.len(), 1, "evict returns 1 output (path)");
    }

    #[test]
    fn oram_begin_circuit_is_well_formed() {
        let c = test_config();
        let (ir, mut types) = oram_begin_circuit(&c);

        // One block, one action, no oracles/rngs
        assert_eq!(ir.blocks.len(), 1);
        assert_eq!(ir.actions.len(), 1);
        assert_eq!(ir.oracles.len(), 0);
        assert_eq!(ir.rngs.len(), 0);

        // Action declaration matches config
        assert_eq!(ir.actions[0].name, "oram_begin_0");
        assert_eq!(ir.actions[0].params.len(), 1);
        assert_eq!(ir.actions[0].results.len(), 1);

        // Block has 1 param (addr) and 4 stmts
        let block = &ir.blocks[0];
        assert_eq!(block.params.len(), 1);
        assert_eq!(block.stmts.len(), 4);
        assert_eq!(block.stmt_provs.len(), 4);

        // Types are well-formed
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        assert_eq!(block.params[0], u64_ty);
    }

    #[test]
    fn oram_begin_circuit_weaves_through_cfg_path() {
        use crate::fhe::{weave_fhe, FheOutput, TfheScheme};

        let c = test_config();
        let (ir, types) = oram_begin_circuit(&c);

        let scheme = c.configure_scheme(TfheScheme::cfg());
        let output = weave_fhe(&ir, &types, &scheme, "oram_begin_test", None, None);

        match output {
            FheOutput::Cfg(module) => {
                // The output module should have at least one function
                assert!(!module.functions.is_empty(), "CFG output should have functions");
            }
            FheOutput::Flat(_) => panic!("expected CFG output, got Flat"),
        }
    }

    #[test]
    #[should_panic(expected = "emit_action_call not implemented")]
    fn oram_begin_circuit_flat_path_panics() {
        use crate::fhe::{weave_fhe, TfheScheme};

        let c = test_config();
        let (ir, types) = oram_begin_circuit(&c);

        // The flat path uses BIR-level ActionCall emission, which is not
        // implemented for TfheScheme. ORAM actions require the CFG path.
        let scheme = c.configure_scheme(TfheScheme::flat());
        let _output = weave_fhe(&ir, &types, &scheme, "oram_begin_flat_test", None, None);
    }

    #[test]
    fn configure_scheme_registers_both_actions() {
        use crate::fhe::{FheScheme, TfheScheme};

        let c = test_config();
        let scheme = c.configure_scheme(TfheScheme::cfg());

        // Both actions should be registered
        assert!(scheme.action_config("oram_begin_0").is_some());
        assert!(scheme.action_config("oram_process_0").is_some());

        // Begin action: all outputs public
        let begin_cfg = scheme.action_config("oram_begin_0").unwrap();
        assert!(begin_cfg.all_outputs_public());

        // Process action: mixed publicness
        let process_cfg = scheme.action_config("oram_process_0").unwrap();
        assert!(!process_cfg.all_outputs_public());
        assert!(process_cfg.is_output_public(2));
        assert!(process_cfg.is_output_public(3));
    }

    #[test]
    fn tree_cell_count_matches_own_storage() {
        let c = test_config();
        let sid = StorageId(super::ORAM_TREE_BASE + c.storage_id);
        assert_eq!(c.tree_cell_count(sid), Some(c.num_nodes()));
    }

    #[test]
    fn tree_cell_count_returns_none_for_other_storage() {
        let c = test_config();
        // Original storage id should not match the tree storage.
        assert_eq!(c.tree_cell_count(StorageId(c.storage_id)), None);
        // A different tree storage id should not match either.
        assert_eq!(c.tree_cell_count(StorageId(super::ORAM_TREE_BASE + 99)), None);
    }

    #[test]
    fn oram_cell_count_fn_returns_nodes_for_tree_storage() {
        let c = test_config();
        let configs = [c.clone()];
        let f = super::oram_cell_count_fn(&configs);
        let mut types = IRTypes::new();
        let dummy_ty = types.intern(IRType::Primitive(PrimType::_64));

        // Tree storage → num_nodes.
        let tree_sid = StorageId(super::ORAM_TREE_BASE + c.storage_id);
        assert_eq!(f(tree_sid, dummy_ty), c.num_nodes());

        // Non-tree storage → 1.
        assert_eq!(f(StorageId(c.storage_id), dummy_ty), 1);
    }

    #[test]
    fn oram_cell_count_fn_handles_multiple_configs() {
        let c0 = OramConfig { storage_id: 0, z: 4, b: 16, l: 3 };
        let c1 = OramConfig { storage_id: 1, z: 2, b: 8, l: 5 };
        let configs = [c0.clone(), c1.clone()];
        let f = super::oram_cell_count_fn(&configs);
        let mut types = IRTypes::new();
        let dummy_ty = types.intern(IRType::Primitive(PrimType::_64));

        assert_eq!(f(StorageId(super::ORAM_TREE_BASE + 0), dummy_ty), c0.num_nodes());
        assert_eq!(f(StorageId(super::ORAM_TREE_BASE + 1), dummy_ty), c1.num_nodes());
        // Unknown tree storage → 1.
        assert_eq!(f(StorageId(super::ORAM_TREE_BASE + 2), dummy_ty), 1);
    }

    #[test]
    fn apply_mono_adds_z_b_l_n() {
        use volar_lir_codegen::mono::MonoEnv;

        let c = test_config(); // z=4, b=16, l=4
        let env = c.apply_mono(MonoEnv::new(""));

        assert_eq!(env.const_params.get("Z"), Some(&4));
        assert_eq!(env.const_params.get("B"), Some(&16));
        assert_eq!(env.const_params.get("L"), Some(&4));
        assert_eq!(env.const_params.get("N"), Some(&15)); // 2^4 - 1
    }

    #[test]
    fn apply_mono_preserves_existing_params() {
        use volar_lir_codegen::mono::MonoEnv;

        let c = test_config();
        let env = MonoEnv::new("sha3").with_len("N_LWE", 630);
        let env = c.apply_mono(env);

        // Pre-existing param preserved.
        assert_eq!(env.const_params.get("N_LWE"), Some(&630));
        assert_eq!(env.hash_suffix, "sha3");
        // ORAM params present.
        assert_eq!(env.const_params.get("Z"), Some(&4));
        assert_eq!(env.const_params.get("N"), Some(&15));
    }
}

#[cfg(test)]
mod tests_rewrite {
    use super::*;
    use volar_ir::ir::{IRType, IRTypes, PrimType};

    /// Standard test config: Z=4, B=16, L=4.
    fn test_config() -> OramConfig {
        OramConfig { storage_id: 0, z: 4, b: 16, l: 4 }
    }

    /// Build a minimal single-block IR with the given stmts, params, and terminator.
    fn make_ir(
        params: Vec<IRTypeId>,
        stmts: Vec<IRStmt>,
        terminator: IRTerminator,
    ) -> IRBlocks<()> {
        let num_stmts = stmts.len();
        IRBlocks {
            oracles: vec![],
            actions: vec![],
            rngs: vec![],
            blocks: vec![IRBlock {
                params,
                stmts,
                stmt_provs: vec![(); num_stmts],
                terminator,
            }],
        }
    }

    fn return_jmp(args: Vec<IRVarId>) -> IRTerminator {
        IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args,
        }
    }

    // -- No-op cases --

    #[test]
    fn empty_configs_returns_clone() {
        let mut types = IRTypes::new();
        let bit_ty = types.bit();
        let ir = make_ir(
            vec![bit_ty],
            vec![IRStmt::Const(Constant { hi: 0, lo: 1 }, bit_ty)],
            return_jmp(vec![IRVarId(1)]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[]);
        assert_eq!(result.blocks.len(), 1);
        assert_eq!(result.blocks[0].stmts.len(), 1);
        assert_eq!(result.actions.len(), 0);
    }

    #[test]
    fn non_oram_storage_passes_through() {
        let mut types = IRTypes::new();
        let bit_ty = types.bit();
        let u8_ty = types.intern(IRType::Primitive(PrimType::_8));

        // StorageId(5) is not in any OramConfig
        let ir = make_ir(
            vec![bit_ty], // param 0: address
            vec![
                IRStmt::StorageRead {
                    storage: StorageId(5),
                    ty: u8_ty,
                    addr: IRVarId(0),
                },
            ],
            return_jmp(vec![IRVarId(1)]),
        );

        let c = test_config(); // storage_id=0
        let result = rewrite_storage_to_oram(&ir, &mut types, &[c]);

        // The StorageRead on StorageId(5) should pass through unchanged.
        assert_eq!(result.blocks[0].stmts.len(), 1);
        match &result.blocks[0].stmts[0] {
            IRStmt::StorageRead { storage, .. } => {
                assert_eq!(storage.0, 5);
            }
            other => panic!("expected StorageRead, got {:?}", other),
        }
    }

    // -- StorageRead expansion --

    #[test]
    fn storage_read_expands_to_action_sequence() {
        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = test_config();
        let data_ty = c.data_type(&mut types);

        // Build: param 0 is address (u64), stmt 0 is StorageRead(0, data_ty, addr)
        let ir = make_ir(
            vec![u64_ty],
            vec![IRStmt::StorageRead {
                storage: StorageId(0),
                ty: data_ty,
                addr: IRVarId(0),
            }],
            return_jmp(vec![IRVarId(1)]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c]);

        // The single StorageRead should expand into many statements.
        let block = &result.blocks[0];
        assert!(
            block.stmts.len() > 1,
            "expected expansion, got {} stmts",
            block.stmts.len()
        );

        // Count ActionCalls — should have exactly 4 (begin + process + 2 evicts).
        let action_calls: Vec<_> = block
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::ActionCall { .. }))
            .collect();
        assert_eq!(
            action_calls.len(),
            4,
            "expected 4 ActionCalls (begin + process + 2 evicts)"
        );

        // First ActionCall should be "oram_begin_0"
        match &action_calls[0] {
            IRStmt::ActionCall { name, args, output_tys, .. } => {
                assert_eq!(name, "oram_begin_0");
                assert_eq!(args.len(), 1, "begin takes 1 arg (address)");
                assert_eq!(output_tys.len(), 1, "begin returns 1 output (leaf)");
            }
            _ => unreachable!(),
        }

        // Second ActionCall should be "oram_process_0"
        match &action_calls[1] {
            IRStmt::ActionCall { name, args, output_tys, .. } => {
                assert_eq!(name, "oram_process_0");
                assert_eq!(args.len(), 3, "process takes 3 args (path, data, is_write)");
                assert_eq!(output_tys.len(), 4, "process returns 4 outputs");
            }
            _ => unreachable!(),
        }

        // Should have StorageReads on tree storage (1000 + 0 = 1000):
        // 1 main read + 2 eviction reads = 3
        let tree_reads: Vec<_> = block
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::StorageRead { storage, .. } if storage.0 == 1000))
            .collect();
        assert_eq!(tree_reads.len(), 3, "expected 3 tree StorageReads (1 main + 2 evictions)");

        // Should have StorageWrites on tree storage (write-back + 2 evictions)
        let tree_writes: Vec<_> = block
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::StorageWrite { storage, .. } if storage.0 == 1000))
            .collect();
        assert_eq!(tree_writes.len(), 3, "expected 3 tree StorageWrites (1 write-back + 2 evictions)");

        // Action declarations should be added.
        assert!(result.actions.len() >= 3, "expected at least 3 action declarations");
        let action_names: Vec<_> = result.actions.iter().map(|a| a.name.as_str()).collect();
        assert!(action_names.contains(&"oram_begin_0"));
        assert!(action_names.contains(&"oram_process_0"));
        assert!(action_names.contains(&"oram_evict_0"));
    }

    // -- StorageWrite expansion --

    #[test]
    fn storage_write_expands_with_is_write_flag() {
        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = test_config();
        let data_ty = c.data_type(&mut types);

        // Build: param 0 = address (u64), param 1 = data
        // stmt 0 = StorageWrite(0, src=1, data_ty, addr=0)
        let ir = make_ir(
            vec![u64_ty, data_ty],
            vec![IRStmt::StorageWrite {
                storage: StorageId(0),
                src: IRVarId(1),
                ty: data_ty,
                addr: IRVarId(0),
            }],
            return_jmp(vec![]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c]);
        let block = &result.blocks[0];

        // Should have 4 ActionCalls (begin + process + 2 evicts)
        let action_calls: Vec<_> = block
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::ActionCall { .. }))
            .collect();
        assert_eq!(action_calls.len(), 4);

        // The is_write constant should be 1 for writes.
        // It's a Const(1, Bit) somewhere in the expansion.
        let bit_ty = types.bit();
        let write_consts: Vec<_> = block
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::Const(Constant { hi: 0, lo: 1 }, ty) if *ty == bit_ty))
            .collect();
        // We expect at least 2 Const(1, Bit): the guard and the is_write flag.
        assert!(
            write_consts.len() >= 2,
            "expected at least 2 Const(1, Bit) for guard + is_write, got {}",
            write_consts.len()
        );
    }

    // -- Read: is_write = 0 --

    #[test]
    fn storage_read_has_is_write_zero() {
        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = test_config();
        let data_ty = c.data_type(&mut types);

        let ir = make_ir(
            vec![u64_ty],
            vec![IRStmt::StorageRead {
                storage: StorageId(0),
                ty: data_ty,
                addr: IRVarId(0),
            }],
            return_jmp(vec![IRVarId(1)]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c]);
        let block = &result.blocks[0];
        let bit_ty = types.bit();

        // For reads, we should have exactly 1 Const(1, Bit) — just the guard.
        // The is_write flag should be Const(0, Bit).
        let guard_consts: Vec<_> = block
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::Const(Constant { hi: 0, lo: 1 }, ty) if *ty == bit_ty))
            .collect();
        assert_eq!(
            guard_consts.len(),
            1,
            "expected exactly 1 Const(1, Bit) for guard (is_write should be 0), got {}",
            guard_consts.len()
        );
    }

    // -- Var remapping correctness --

    #[test]
    fn subsequent_stmts_use_remapped_vars() {
        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let bit_ty = types.bit();
        let c = test_config();
        let data_ty = c.data_type(&mut types);

        // Build:
        //   param 0 = addr (u64)
        //   stmt 0 (var 1) = StorageRead(0, data_ty, addr=0) → ORAM-backed
        //   stmt 1 (var 2) = Const(42, u64) — should pass through
        //   terminator: return [var1, var2]
        let ir = make_ir(
            vec![u64_ty],
            vec![
                IRStmt::StorageRead {
                    storage: StorageId(0),
                    ty: data_ty,
                    addr: IRVarId(0),
                },
                IRStmt::Const(Constant { hi: 0, lo: 42 }, u64_ty),
            ],
            return_jmp(vec![IRVarId(1), IRVarId(2)]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c]);
        let block = &result.blocks[0];

        // The last stmt should be Const(42, u64), unchanged.
        let last = block.stmts.last().unwrap();
        match last {
            IRStmt::Const(Constant { hi: 0, lo: 42 }, ty) => {
                assert_eq!(*ty, u64_ty);
            }
            other => panic!("expected Const(42, u64) as last stmt, got {:?}", other),
        }

        // The terminator should reference the remapped vars.
        // var1 (StorageRead result) should map to the rd_data ActionOutput.
        // var2 (Const) should map to the last statement index.
        match &block.terminator {
            IRTerminator::Jmp { args, .. } => {
                assert_eq!(args.len(), 2, "terminator should have 2 return args");
                // Both args should be valid var IDs (< num_params + num_stmts)
                let max_var = 1 + block.stmts.len() as u32;
                assert!(
                    args[0].0 < max_var,
                    "first return arg {} out of range (max {})",
                    args[0].0,
                    max_var
                );
                assert!(
                    args[1].0 < max_var,
                    "second return arg {} out of range (max {})",
                    args[1].0,
                    max_var
                );
                // The two args should be different (read data vs const).
                assert_ne!(args[0], args[1], "return args should differ");
            }
            other => panic!("expected Jmp terminator, got {:?}", other),
        }
    }

    // -- Provenance tracking --

    #[test]
    fn provenance_length_matches_stmts() {
        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = test_config();
        let data_ty = c.data_type(&mut types);

        let ir = make_ir(
            vec![u64_ty],
            vec![IRStmt::StorageRead {
                storage: StorageId(0),
                ty: data_ty,
                addr: IRVarId(0),
            }],
            return_jmp(vec![IRVarId(1)]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c]);
        let block = &result.blocks[0];
        assert_eq!(
            block.stmts.len(),
            block.stmt_provs.len(),
            "stmts and stmt_provs must have equal length"
        );
    }

    // -- Multiple configs --

    #[test]
    fn multiple_configs_rewrite_different_storages() {
        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c0 = OramConfig { storage_id: 0, z: 4, b: 16, l: 4 };
        let c1 = OramConfig { storage_id: 1, z: 2, b: 8, l: 3 };
        let data0_ty = c0.data_type(&mut types);
        let data1_ty = c1.data_type(&mut types);

        let ir = make_ir(
            vec![u64_ty],
            vec![
                IRStmt::StorageRead {
                    storage: StorageId(0),
                    ty: data0_ty,
                    addr: IRVarId(0),
                },
                IRStmt::StorageRead {
                    storage: StorageId(1),
                    ty: data1_ty,
                    addr: IRVarId(0),
                },
            ],
            return_jmp(vec![]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c0, c1]);
        let block = &result.blocks[0];

        // Should have 8 ActionCalls (4 per ORAM config: begin + process + 2 evicts).
        let action_calls: Vec<_> = block
            .stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::ActionCall { .. }))
            .collect();
        assert_eq!(action_calls.len(), 8, "expected 8 ActionCalls for 2 ORAM configs");

        // Tree storage reads: StorageId(1000) and StorageId(1001), 3 each
        let tree_reads: Vec<u32> = block
            .stmts
            .iter()
            .filter_map(|s| match s {
                IRStmt::StorageRead { storage, .. } if storage.0 >= 1000 => Some(storage.0),
                _ => None,
            })
            .collect();
        assert!(tree_reads.contains(&1000));
        assert!(tree_reads.contains(&1001));

        // Action declarations: 6 total (begin+process+evict for each config)
        assert_eq!(result.actions.len(), 6);
    }

    // -- Action declarations are not duplicated --

    #[test]
    fn action_decls_not_duplicated_when_pre_existing() {
        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = test_config();
        let data_ty = c.data_type(&mut types);

        // Pre-populate actions in the IR.
        let (begin_decl, _, _) = c.action_decls(&mut types);
        let mut ir = make_ir(
            vec![u64_ty],
            vec![IRStmt::StorageRead {
                storage: StorageId(0),
                ty: data_ty,
                addr: IRVarId(0),
            }],
            return_jmp(vec![IRVarId(1)]),
        );
        ir.actions.push(begin_decl);

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c]);

        // begin_0 should not be duplicated. Total should be 3 (1 pre-existing begin + 1 new process + 1 new evict).
        let begin_count = result
            .actions
            .iter()
            .filter(|a| a.name == "oram_begin_0")
            .count();
        assert_eq!(begin_count, 1, "oram_begin_0 should not be duplicated");
        assert_eq!(result.actions.len(), 3);
    }

    // -- Statement count sanity --

    #[test]
    fn read_expansion_statement_count() {
        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = test_config();
        let data_ty = c.data_type(&mut types);

        let ir = make_ir(
            vec![u64_ty],
            vec![IRStmt::StorageRead {
                storage: StorageId(0),
                ty: data_ty,
                addr: IRVarId(0),
            }],
            return_jmp(vec![IRVarId(1)]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c]);
        let block = &result.blocks[0];

        // Expected expansion for a read:
        // 1. guard = Const(1, Bit)
        // 2. fb_leaf = Const(0, u64)
        // 3. begin = ActionCall
        // 4. leaf = ActionOutput
        // 5. path = StorageRead (tree)
        // 6. zero_data = Const(0, data_ty)
        // 7. is_write = Const(0, Bit)
        // 8. fb_path = Const(0, path_ty)
        // 9. fb_data = Const(0, data_ty)
        // 10. fb_ev1 = Const(0, u64)
        // 11. fb_ev2 = Const(0, u64)
        // 12. process = ActionCall
        // 13. wb_path = ActionOutput
        // 14. rd_data = ActionOutput
        // 15. evict1_leaf = ActionOutput
        // 16. evict2_leaf = ActionOutput
        // 17. StorageWrite (tree write-back)
        // 18. evict1_path = StorageRead (tree at evict1_leaf)
        // 19. fb_evict1 = Const(0, path_ty)
        // 20. evict1_call = ActionCall ("oram_evict_0")
        // 21. evict1_result = ActionOutput
        // 22. StorageWrite (tree evict1 write-back)
        // 23. evict2_path = StorageRead (tree at evict2_leaf)
        // 24. fb_evict2 = Const(0, path_ty)
        // 25. evict2_call = ActionCall ("oram_evict_0")
        // 26. evict2_result = ActionOutput
        // 27. StorageWrite (tree evict2 write-back)
        assert_eq!(
            block.stmts.len(),
            27,
            "a single StorageRead should expand to exactly 27 statements"
        );
    }

    // -- Weave the rewritten IR through CFG path --

    #[test]
    fn rewritten_ir_weaves_through_cfg() {
        use crate::fhe::{weave_fhe, FheOutput, TfheScheme};

        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = test_config();
        let data_ty = c.data_type(&mut types);

        let ir = make_ir(
            vec![u64_ty],
            vec![IRStmt::StorageRead {
                storage: StorageId(0),
                ty: data_ty,
                addr: IRVarId(0),
            }],
            return_jmp(vec![IRVarId(1)]),
        );

        let result = rewrite_storage_to_oram(&ir, &mut types, &[c.clone()]);

        let scheme = c.configure_scheme(TfheScheme::cfg());
        let output = weave_fhe(&result, &types, &scheme, "oram_rewrite_test", None, None);

        match output {
            FheOutput::Cfg(module) => {
                assert!(!module.functions.is_empty(), "CFG output should have functions");
            }
            FheOutput::Flat(_) => panic!("expected CFG output, got Flat"),
        }
    }
}

#[cfg(all(test, feature = "linking"))]
mod tests_linking {
    extern crate std;
    extern crate alloc;
    use alloc::vec;
    use alloc::vec::Vec;

    use volar_compiler::parser::{SourceInput, parse_sources};
    use volar_compiler::linkage::LinkageSystem;

    #[test]
    fn oram_core_parses_from_include() {
        let source = include_str!("../../../oram/volar-oram-core/src/lib.rs");
        let filtered = super::strip_inner_attributes(source);
        let module = parse_sources(&[SourceInput { source: &filtered, name: "oram_core.rs" }], "oram_core")
            .expect("failed to parse oram-core source");

        // Verify expected structure
        assert!(module.structs.len() >= 2, "expected at least OramEntry, Bucket structs");
        assert!(module.enums.len() >= 2, "expected at least ServerRequest, ServerResponse enums");
        assert!(module.functions.len() >= 4, "expected at least path_indices, read_path, write_path, server_step");
        assert!(module.impls.len() >= 2, "expected at least OramEntry, Bucket impls");
    }

    #[test]
    fn oram_core_round_trips_through_printer() {
        use volar_compiler::printer::{DisplayRust, ModuleWriter};

        let source = include_str!("../../../oram/volar-oram-core/src/lib.rs");
        let filtered = super::strip_inner_attributes(source);
        let module = parse_sources(&[SourceInput { source: &filtered, name: "oram_core.rs" }], "oram_core")
            .expect("failed to parse oram-core source");

        let out = std::format!("{}", DisplayRust(ModuleWriter { module: &module }));

        // Key types present
        assert!(out.contains("struct OramEntry"), "missing OramEntry in output");
        assert!(out.contains("struct Bucket"), "missing Bucket in output");
        assert!(out.contains("enum ServerRequest"), "missing ServerRequest in output");
        assert!(out.contains("enum ServerResponse"), "missing ServerResponse in output");

        // Key functions present
        assert!(out.contains("fn path_indices"), "missing path_indices in output");
        assert!(out.contains("fn read_path"), "missing read_path in output");
        assert!(out.contains("fn write_path"), "missing write_path in output");
        assert!(out.contains("fn server_step"), "missing server_step in output");

        // Derives preserved
        assert!(out.contains("Clone"), "missing Clone derive in output");
        assert!(out.contains("Copy"), "missing Copy derive in output");
    }

    #[test]
    fn oram_core_compile_check() {
        use crate::tests_common::run_compile_check;
        use volar_compiler::printer::{DisplayRust, ModuleWriter};

        let source = include_str!("../../../oram/volar-oram-core/src/lib.rs");
        let filtered = super::strip_inner_attributes(source);
        let module = parse_sources(&[SourceInput { source: &filtered, name: "oram_core.rs" }], "oram_core")
            .expect("failed to parse oram-core source");

        let code = std::format!("{}", DisplayRust(ModuleWriter { module: &module }));
        run_compile_check(&code, "oram_core_roundtrip");
    }

    // -- End-to-end integration: rewrite + link + weave --

    #[test]
    fn rewrite_link_weave_integration() {
        use crate::fhe::print_fhe_cfg_module;

        let module = build_oram_cfg_module();

        // Verify structural properties — circuit code present
        assert!(!module.functions.is_empty(), "should have at least one CFG function");

        // Verify linked spec content is merged
        assert!(
            module.auxiliary_functions.len() >= 4,
            "expected >= 4 linked functions (path_indices, read_path, write_path, server_step), got {}",
            module.auxiliary_functions.len()
        );
        assert!(
            module.structs.len() >= 2,
            "expected >= 2 linked structs (OramEntry, Bucket), got {}",
            module.structs.len()
        );
        assert!(
            module.enums.len() >= 2,
            "expected >= 2 linked enums (ServerRequest, ServerResponse), got {}",
            module.enums.len()
        );

        // Print and verify text output contains both circuit and spec content
        let code = print_fhe_cfg_module(&module, true);

        // Circuit function
        assert!(code.contains("fn oram_e2e_tfhe_cfg"), "missing circuit function in output");

        // Linked ORAM types
        assert!(code.contains("struct OramEntry"), "missing OramEntry struct in output");
        assert!(code.contains("struct Bucket"), "missing Bucket struct in output");
        assert!(code.contains("enum ServerRequest"), "missing ServerRequest enum in output");
        assert!(code.contains("enum ServerResponse"), "missing ServerResponse enum in output");

        // Linked ORAM functions
        assert!(code.contains("fn path_indices"), "missing path_indices in output");
        assert!(code.contains("fn read_path"), "missing read_path in output");
        assert!(code.contains("fn write_path"), "missing write_path in output");
        assert!(code.contains("fn server_step"), "missing server_step in output");

        // Derives preserved in linked types
        assert!(code.contains("Clone"), "missing Clone derive in output");

        // Eviction action stubs should be present (3 action types)
        assert!(code.contains("fn oram_begin_0"), "missing oram_begin_0 action stub");
        assert!(code.contains("fn oram_process_0"), "missing oram_process_0 action stub");
        assert!(code.contains("fn oram_evict_0"), "missing oram_evict_0 action stub");

        // Compile check — the generated code (with action stubs,
        // linked ORAM spec, and circuit function) should pass `cargo check`.
        crate::tests_common::run_compile_check_tfhe_cfg(&code, "oram_e2e");
    }

    #[test]
    #[cfg(feature = "linking")]
    fn rewrite_link_weave_integration_ts() {
        use crate::fhe::print_fhe_cfg_module_ts;

        let module = build_oram_cfg_module();
        let ts_code = print_fhe_cfg_module_ts(&module);

        // Sanity: output is non-empty and contains the circuit function name
        assert!(!ts_code.is_empty(), "TS output should be non-empty");
        assert!(ts_code.contains("oram_e2e_tfhe_cfg"), "missing circuit function in TS output");

        // Check for linked ORAM types (classes/tagged unions in TS)
        assert!(ts_code.contains("OramEntry"), "missing OramEntry in TS output");
        assert!(ts_code.contains("Bucket"), "missing Bucket in TS output");
        assert!(ts_code.contains("ServerRequest"), "missing ServerRequest in TS output");
        assert!(ts_code.contains("ServerResponse"), "missing ServerResponse in TS output");

        // Check for linked functions
        assert!(ts_code.contains("path_indices"), "missing path_indices in TS output");

        crate::tests_common::run_compile_check_ts(&ts_code, "oram_e2e_ts");
    }

    #[test]
    #[cfg(feature = "linking")]
    fn rewrite_link_weave_integration_c() {
        use super::OramConfig;
        use crate::fhe::print_fhe_cfg_module_c;
        use volar_lir_codegen::mono::MonoEnv;

        let c = OramConfig { storage_id: 0, z: 4, b: 16, l: 4 };
        let module = build_oram_cfg_module();

        // C has no generics — monomorphize all const params to concrete values.
        let env = MonoEnv::new("")
            .with_len("N_LWE", 630)
            .with_len("BIG_N", 2048)
            .with_len("BS_ELL", 3)
            .with_len("KS_ELL", 4);
        let env = c.apply_mono(env);

        let c_code = print_fhe_cfg_module_c(&module, &env);

        // Sanity: output is non-empty
        assert!(!c_code.is_empty(), "C output should be non-empty");

        crate::tests_common::run_compile_check_c(&c_code, "oram_e2e_c");
    }

    /// End-to-end: circuit with both StorageRead and StorageWrite on the
    /// same ORAM-backed storage. Exercises eviction sequences for both
    /// access types in a single function.
    #[test]
    #[cfg(feature = "linking")]
    fn rewrite_link_weave_read_write_e2e() {
        use super::{OramConfig, rewrite_storage_to_oram, oram_linked_spec, oram_cell_count_fn};
        use crate::fhe::{weave_fhe, FheOutput, TfheScheme, derive_ir_storage_config, print_fhe_cfg_module};
        use volar_ir::ir::{
            IRType, IRTypes, IRBlocks, IRBlock, IRStmt, IRVarId,
            IRTerminator, IRBlockTargetId, PrimType, StorageId, Constant,
        };
        use volar_compiler::linkage::LinkageSystem;

        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = OramConfig { storage_id: 0, z: 4, b: 16, l: 4 };
        let data_ty = c.data_type(&mut types);

        // Circuit: param 0 = addr (u64), param 1 = write_data (data_ty)
        //   stmt 0 = StorageWrite(0, src=param1, data_ty, addr=param0)
        //   stmt 1 = StorageRead(0, data_ty, addr=param0)
        //   return [stmt1_result]
        let ir = IRBlocks {
            oracles: vec![],
            actions: vec![],
            rngs: vec![],
            blocks: vec![IRBlock {
                params: vec![u64_ty, data_ty],
                stmts: vec![
                    IRStmt::StorageWrite {
                        storage: StorageId(0),
                        src: IRVarId(1),
                        ty: data_ty,
                        addr: IRVarId(0),
                    },
                    IRStmt::StorageRead {
                        storage: StorageId(0),
                        ty: data_ty,
                        addr: IRVarId(0),
                    },
                ],
                stmt_provs: vec![(); 2],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: vec![IRVarId(3)], // result of StorageRead (var 2 = StorageWrite dummy, var 3 = StorageRead result)
                },
            }],
        };

        let rewritten = rewrite_storage_to_oram(&ir, &mut types, &[c.clone()]);

        // Should have expanded both storage ops.
        assert!(
            rewritten.blocks[0].stmts.len() > 4,
            "expected expansion for both read and write, got {} stmts",
            rewritten.blocks[0].stmts.len()
        );

        // 3 action declarations: begin, process, evict
        assert_eq!(rewritten.actions.len(), 3, "expected 3 action declarations");

        // 8 ActionCalls: 4 per access (begin + process + 2 evicts) x 2 accesses
        let action_calls: Vec<_> = rewritten.blocks[0].stmts
            .iter()
            .filter(|s| matches!(s, IRStmt::ActionCall { .. }))
            .collect();
        assert_eq!(action_calls.len(), 8, "expected 8 ActionCalls for 2 ORAM accesses");

        // Link and weave.
        let mut linkage = LinkageSystem::new();
        linkage.add(oram_linked_spec());

        let configs = [c.clone()];
        let cell_count = oram_cell_count_fn(&configs);
        let storage_config = derive_ir_storage_config(&rewritten, Some(&cell_count));

        let scheme = c.configure_scheme(TfheScheme::cfg());
        let output = weave_fhe(&rewritten, &types, &scheme, "oram_rw_e2e", Some(&linkage), Some(&storage_config));

        let module = match output {
            FheOutput::Cfg(m) => m,
            FheOutput::Flat(_) => panic!("expected CFG output"),
        };

        let code = print_fhe_cfg_module(&module, true);

        // All 3 action stubs present.
        assert!(code.contains("fn oram_begin_0"), "missing oram_begin_0");
        assert!(code.contains("fn oram_process_0"), "missing oram_process_0");
        assert!(code.contains("fn oram_evict_0"), "missing oram_evict_0");

        // Circuit function present.
        assert!(code.contains("fn oram_rw_e2e_tfhe_cfg"), "missing circuit function");

        // Compile check.
        crate::tests_common::run_compile_check_tfhe_cfg(&code, "oram_rw_e2e");
    }

    /// Shared helper: build a rewritten+linked+woven ORAM CFG module for testing.
    #[cfg(feature = "linking")]
    fn build_oram_cfg_module() -> volar_compiler::IrCfgModule {
        use super::{OramConfig, rewrite_storage_to_oram, oram_linked_spec, oram_cell_count_fn};
        use crate::fhe::{weave_fhe, FheOutput, TfheScheme, derive_ir_storage_config};
        use volar_ir::ir::{
            IRType, IRTypes, IRBlocks, IRBlock, IRStmt, IRVarId,
            IRTerminator, IRBlockTargetId, PrimType, StorageId, Constant,
        };

        let mut types = IRTypes::new();
        let u64_ty = types.intern(IRType::Primitive(PrimType::_64));
        let c = OramConfig { storage_id: 0, z: 4, b: 16, l: 4 };
        let data_ty = c.data_type(&mut types);

        // Build minimal IR: param 0 = address (u64), stmt 0 = StorageRead
        let ir = IRBlocks {
            oracles: vec![],
            actions: vec![],
            rngs: vec![],
            blocks: vec![IRBlock {
                params: vec![u64_ty],
                stmts: vec![IRStmt::StorageRead {
                    storage: StorageId(0),
                    ty: data_ty,
                    addr: IRVarId(0),
                }],
                stmt_provs: vec![()],
                terminator: IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: vec![IRVarId(1)],
                },
            }],
        };

        // Step 1: Rewrite storage to ORAM
        let rewritten = rewrite_storage_to_oram(&ir, &mut types, &[c.clone()]);
        assert!(rewritten.blocks[0].stmts.len() > 1, "rewrite should expand storage ops");

        // Step 2: Build linkage system with ORAM core
        let mut linkage = LinkageSystem::new();
        linkage.add(oram_linked_spec());

        // Step 3: Derive storage configuration from the rewritten IR
        let configs = [c.clone()];
        let cell_count = oram_cell_count_fn(&configs);
        let storage_config = derive_ir_storage_config(&rewritten, Some(&cell_count));

        // Step 4: Weave through CFG with linkage and storage config
        let scheme = c.configure_scheme(TfheScheme::cfg());
        let output = weave_fhe(&rewritten, &types, &scheme, "oram_e2e", Some(&linkage), Some(&storage_config));

        match output {
            FheOutput::Cfg(m) => m,
            FheOutput::Flat(_) => panic!("expected CFG output, got Flat"),
        }
    }
}
