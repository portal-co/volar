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

use alloc::{format, string::String, vec};

use volar_compiler::linkage::LinkedSpec;
use volar_ir::ir::{ActionDecl, IRType, IRTypeId, IRTypes, PrimType};

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

    /// Create both [`ActionDecl`]s for this ORAM instance.
    pub fn action_decls(&self, types: &mut IRTypes) -> (ActionDecl, ActionDecl) {
        let begin = self.begin_action_decl(types);
        let process = self.process_action_decl(types);
        (begin, process)
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

    /// Create all action configs as `(name, config)` pairs, suitable for
    /// registering with [`TfheScheme::with_action_config`].
    pub fn action_configs(&self) -> [(String, FheActionConfig); 2] {
        [
            (self.begin_action_name(), self.begin_action_config()),
            (self.process_action_name(), self.process_action_config()),
        ]
    }

    /// Register this ORAM instance's action configs with a [`TfheScheme`].
    ///
    /// Convenience method that calls `with_action_config` for both the
    /// begin and process actions.
    pub fn configure_scheme(&self, scheme: crate::fhe::TfheScheme) -> crate::fhe::TfheScheme {
        let [(n1, c1), (n2, c2)] = self.action_configs();
        scheme.with_action_config(n1, c1).with_action_config(n2, c2)
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
    use volar_ir::ir::{Constant, IRBlock, IRTerminator, IRVarId, IRBlockTargetId};

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

use volar_ir::ir::{IRBlocks, IRStmt};

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
        assert!(configs[0].1.all_outputs_public());
        assert!(!configs[1].1.all_outputs_public());
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
        let (begin, process) = c.action_decls(&mut types);
        assert_eq!(begin.params.len(), 1);
        assert_eq!(process.results.len(), 4);
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
}

#[cfg(all(test, feature = "linking"))]
mod tests_linking {
    extern crate std;

    use volar_compiler::parser::{SourceInput, parse_sources};

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
}
