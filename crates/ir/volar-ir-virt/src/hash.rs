// @reliability: experimental
// @ai: assisted
//! Hash algorithm interface for bytecode commitment in the virtualisation pass.
//!
//! Two paired implementations are required for every algorithm:
//!
//! * **Native** — [`IrHashAlgorithm::hash_bytes_native`] runs in plain Rust at
//!   setup time.  It consumes the canonical little-endian byte encoding of the
//!   per-block bytecode words and returns the commitment bytes.
//!
//! * **IR** — [`IrHashAlgorithm::emit_ir`] lowers the same computation into a
//!   sequence of [`IRStmt`] nodes appended to the block under construction via
//!   an [`IrEmitter`].  The emitter must use IR primitives (`Poly`, `Rol`,
//!   `Ror`, `Merge`, `Splat`, `Shuffle`) for the core computation; named
//!   `OracleCall` statements are permitted for sub-operations the IR cannot
//!   express natively (e.g. 32-bit integer addition).  The top-level hash call
//!   must not be a single wrapping oracle.
//!
//! [`XorFoldHash32`] is a zero-oracle reference implementation that uses only
//! `Const`, `Transmute`, `Poly` (XOR), and `Rol`.
//!
//! # How the commitment is used
//!
//! At **setup time** the pass computes `commitment[pc] =
//! H(handler_idx, slot_0, …, slot_n)` natively for every program counter and
//! writes the result as a `Const` into `CommitmentConfig::commitment_storage`.
//!
//! At **every execution step** each handler re-reads its bytecode slots, emits
//! the same hash computation via `emit_ir`, and XOR-diffs the result against the
//! stored commitment.  The diff is XOR-injected into `next_pc`:
//!
//! ```text
//! next_pc_protected = next_pc XOR diff
//! ```
//!
//! When the commitment is valid `diff == 0`, so `next_pc_protected == next_pc`
//! and semantics are unchanged.  When the bytecode has been tampered with
//! `diff != 0` and the program jumps to a wrong address, diverging or crashing.
//! This makes the commitment structurally binding without requiring a separate
//! assertion oracle.

use alloc::{collections::BTreeMap, string::String, vec, vec::Vec};

use volar_ir::ir::{IRStmt, IRType, IRTypeId, IRTypes, IRVarId};
use volar_ir_common::{Constant, OracleDecl, StorageId, Type as PrimType};

// ============================================================================
// IrEmitter — minimal block-builder interface
// ============================================================================

/// The minimal IR block-builder interface exposed to hash algorithm
/// implementations.
///
/// Implementations are provided by the virtualisation pass and forward to
/// the `IRBlockUnfinished` being built plus the module's [`IRTypes`] table.
pub trait IrEmitter {
    /// Append `stmt` to the current block and return the new SSA variable id.
    fn emit(&mut self, stmt: IRStmt) -> IRVarId;
    /// Intern `ty` in the module type table (deduplicates).
    fn intern_type(&mut self, ty: IRType) -> IRTypeId;
    /// Pre-interned address type (`_32`).
    fn addr_ty(&self) -> IRTypeId;
    /// Pre-interned bit type (`Bit`).
    fn bit_ty(&self) -> IRTypeId;
}

// ============================================================================
// IrHashAlgorithm
// ============================================================================

/// A hash algorithm with paired native and IR implementations.
///
/// # Semantic contract
///
/// Let `words = [(v_0, ty_0), …, (v_{n-1}, ty_{n-1})]`.  The SSA value
/// produced by `emit_ir(emitter, words)` when evaluated on concrete inputs
/// **must equal** the value produced by
/// `hash_bytes_native(&[le_bytes(v_0), …, le_bytes(v_{n-1})])`, where
/// `le_bytes` is the little-endian byte encoding of the concrete word value.
///
/// # Internal oracle calls
///
/// Operations unavailable as native IR primitives may be expressed via named
/// `OracleCall` statements.  Declare them by returning [`OracleDecl`]s from
/// [`internal_oracle_decls`](IrHashAlgorithm::internal_oracle_decls); the
/// virtualisation pass adds them to the output module's `oracles` list.
pub trait IrHashAlgorithm {
    /// Emit IR that computes `H(inputs)` into `emitter`.
    ///
    /// `inputs` is a slice of `(var, type_id)` pairs in serialisation order.
    /// Returns the SSA variable holding the hash output; its IR type must
    /// equal `output_type_id(types)`.
    fn emit_ir(&self, emitter: &mut dyn IrEmitter, inputs: &[(IRVarId, IRTypeId)]) -> IRVarId;

    /// The IR type of the value returned by `emit_ir`.
    fn output_type_id(&self, types: &mut IRTypes) -> IRTypeId;

    /// Compute the same hash natively.
    ///
    /// `words[i]` is the little-endian byte slice for the i-th input word.
    /// The return value is the little-endian byte encoding of the hash output;
    /// its length determines how many bytes are stored in `commitment_storage`.
    fn hash_bytes_native(&self, words: &[&[u8]]) -> Vec<u8>;

    /// Short algorithm name used for oracle sub-names and debug output.
    fn name(&self) -> &str;

    /// Oracle declarations that must be added to the module's `oracles` list
    /// when this algorithm is active.  Return an empty vec if none.
    fn internal_oracle_decls(&self, types: &mut IRTypes) -> Vec<OracleDecl> {
        let _ = types;
        Vec::new()
    }
}

// ============================================================================
// CommitmentConfig
// ============================================================================

/// Configuration for per-PC bytecode commitment.
///
/// Pass this to [`virtualize_ir_committed`](crate::ir::virtualize_ir_committed)
/// in addition to the usual [`VirtualizeConfig`](crate::VirtualizeConfig).
pub struct CommitmentConfig<H: IrHashAlgorithm> {
    /// Hash algorithm to use.
    pub algorithm: H,
    /// [`StorageId`] where per-PC commitment hashes are written at setup
    /// and read at runtime.  Must not overlap with
    /// [`VirtualizeConfig::bytecode_storage`](crate::VirtualizeConfig::bytecode_storage)
    /// or the register-file range.
    pub commitment_storage: StorageId,
}

// ============================================================================
// XorFoldHash32 — reference implementation
// ============================================================================

/// Oracle-free 32-bit hash: XOR fold + rotate.
///
/// ```text
/// h₀ = seed
/// hᵢ = Rol( hᵢ₋₁  XOR  truncate_u32(wᵢ₋₁),  13 )
/// ```
///
/// Emits only `Const`, `Transmute`, `Poly` (XOR), and `Rol` IR statements.
/// No oracle calls are needed.
///
/// **Security note:** Not a cryptographic hash.  Suitable for lightweight
/// structural binding; use a stronger algorithm in security-sensitive contexts.
#[derive(Clone, Copy, Debug)]
pub struct XorFoldHash32 {
    /// Starting accumulator value.
    pub seed: u32,
}

impl Default for XorFoldHash32 {
    fn default() -> Self {
        Self { seed: 0xDEAD_BEEF }
    }
}

impl IrHashAlgorithm for XorFoldHash32 {
    fn emit_ir(&self, emitter: &mut dyn IrEmitter, inputs: &[(IRVarId, IRTypeId)]) -> IRVarId {
        let u32_ty = emitter.intern_type(IRType::Primitive(PrimType::_32));
        let mut h = emitter.emit(IRStmt::Const(
            Constant { hi: 0, lo: self.seed as u128 },
            u32_ty,
        ));
        for &(var, ty) in inputs {
            // Reinterpret input as _32 (truncate/zero-extend bitwise).
            let w = if ty == u32_ty {
                var
            } else {
                emitter.emit(IRStmt::Transmute { src: var, src_ty: ty, dst_ty: u32_ty })
            };
            // h = h XOR w  (degree-1 Poly over _32 = GF(2^32) addition)
            let mut xor: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
            xor.insert(vec![h], 1);
            xor.insert(vec![w], 1);
            h = emitter.emit(IRStmt::Poly {
                ty: u32_ty,
                coeffs: xor,
                constant: Constant { hi: 0, lo: 0 },
            });
            // h = Rol(h, 13)
            h = emitter.emit(IRStmt::Rol { src: h, ty: u32_ty, n: 13 });
        }
        h
    }

    fn output_type_id(&self, types: &mut IRTypes) -> IRTypeId {
        types.intern(IRType::Primitive(PrimType::_32))
    }

    fn hash_bytes_native(&self, words: &[&[u8]]) -> Vec<u8> {
        let mut h = self.seed;
        for word in words {
            let mut buf = [0u8; 4];
            let n = word.len().min(4);
            buf[..n].copy_from_slice(&word[..n]);
            let w = u32::from_le_bytes(buf);
            h ^= w;
            h = h.rotate_left(13);
        }
        h.to_le_bytes().to_vec()
    }

    fn name(&self) -> &str {
        "xorfold32"
    }
}

// ============================================================================
// Serialisation helpers
// ============================================================================

/// Canonical little-endian byte encoding for a typed constant.
///
/// Returns the byte slice that `emit_ir` would see when the constant is
/// materialised as a word and `Transmute`d to `_32` (or kept as-is for `_32`
/// inputs).  The length equals the natural byte width of the type.
pub fn constant_to_le_bytes(c: &Constant, ty: &IRType) -> Vec<u8> {
    match ty {
        IRType::Primitive(PrimType::Bit) => vec![(c.lo & 1) as u8],
        IRType::Primitive(PrimType::_8) => vec![(c.lo & 0xFF) as u8],
        IRType::Primitive(PrimType::_16) => (c.lo as u16).to_le_bytes().to_vec(),
        IRType::Primitive(PrimType::_32) => (c.lo as u32).to_le_bytes().to_vec(),
        IRType::Primitive(PrimType::_64) => (c.lo as u64).to_le_bytes().to_vec(),
        IRType::Primitive(PrimType::_128) => c.lo.to_le_bytes().to_vec(),
        IRType::Primitive(PrimType::_256) => {
            let mut b = c.lo.to_le_bytes().to_vec();
            b.extend_from_slice(&c.hi.to_le_bytes());
            b
        }
        // AES8, Galois64, compound types: fall back to 4-byte LE encoding.
        _ => (c.lo as u32).to_le_bytes().to_vec(),
    }
}

/// Convert hash output bytes (little-endian) to a [`Constant`].
///
/// At most 32 bytes (256 bits) are used; any excess is silently truncated.
pub fn bytes_to_constant(bytes: &[u8]) -> Constant {
    let mut lo_buf = [0u8; 16];
    let lo_n = bytes.len().min(16);
    lo_buf[..lo_n].copy_from_slice(&bytes[..lo_n]);
    let lo = u128::from_le_bytes(lo_buf);

    let hi = if bytes.len() > 16 {
        let mut hi_buf = [0u8; 16];
        let hi_n = (bytes.len() - 16).min(16);
        hi_buf[..hi_n].copy_from_slice(&bytes[16..16 + hi_n]);
        u128::from_le_bytes(hi_buf)
    } else {
        0
    };

    Constant { hi, lo }
}
