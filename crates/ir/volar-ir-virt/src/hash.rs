// @reliability: experimental
// @ai: assisted
//! Hash algorithm interface for bytecode commitment, with keyed-hash support.
//!
//! # Interface
//!
//! [`IrHashAlgorithm`] requires two paired implementations:
//!
//! * **Native** — [`IrHashAlgorithm::hash_bytes_native`] runs in plain Rust
//!   at setup time to compute the commitment constants baked into the IR.
//! * **IR** — [`IrHashAlgorithm::emit_ir`] lowers the same computation into
//!   a stream of [`IRStmt`] nodes appended to the block under construction.
//!   Core IR primitives (`Poly`, `Rol`, `Ror`, `Shuffle`, `Merge`) are used
//!   where possible; the existing ripple-carry integer-addition emulation
//!   (from `volar_lir::circuits::bc_add`) handles modular 64-bit addition.
//!
//! # Keyed hashes
//!
//! An algorithm declares its key shape via [`IrHashAlgorithm::key_schema`].
//! The key is a compile-time constant stored in [`CommitmentConfig::key`]; its
//! values are added as the **first parameters** of the virtualised module's
//! entry block.  This parameter framing means:
//!
//! * At setup time the pass writes the compile-time constants as `Stmt::Const`
//!   into a dedicated [`CommitmentConfig::key_storage`].
//! * At every execution step each handler reads the key back from storage and
//!   feeds it into `emit_ir`.
//! * Future ZK work can mark those entry-block parameters as *private*,
//!   turning the commitment into a MAC without changing the IR structure.
//!
//! # Provided algorithms
//!
//! | Algorithm | Oracle-free? | Output | Use case |
//! |-----------|-------------|--------|----------|
//! | [`XorFoldHash32`] | ✅ | `_32` | Lightweight testing; not secure |
//! | [`SipHash48`] | ✅ | `_64` | Real use; SipHash-4-8 via ripple-carry |
//!
//! `SipHash48` is SipHash with `c = 4` compression rounds and `d = 8`
//! finalization rounds, keyed with a 128-bit key (`k0 ∥ k1`, each 64 bits).
//! It requires no oracle calls — all SipRound operations are lowered directly:
//! XOR and rotation via `Poly`/`Rol`, 64-bit addition via the ripple-carry
//! adder from `volar_lir::circuits::bc_add`.

use alloc::{collections::BTreeMap, vec, vec::Vec};

use volar_ir::ir::{IRStmt, IRType, IRTypeId, IRTypes, IRVarId};
use volar_ir_common::{Constant, OracleDecl, StorageId, Type as PrimType};
use volar_lir::circuits::{bc_add, BitCircuitBuilder};

// ============================================================================
// IrEmitter
// ============================================================================

/// Minimal IR block-builder interface exposed to hash algorithm implementations.
pub trait IrEmitter {
    /// Append `stmt` to the current block and return the new SSA variable id.
    fn emit(&mut self, stmt: IRStmt) -> IRVarId;
    /// Intern `ty` in the module type table.
    fn intern_type(&mut self, ty: IRType) -> IRTypeId;
    /// Pre-interned `_32` address type.
    fn addr_ty(&self) -> IRTypeId;
    /// Pre-interned `Bit` type.
    fn bit_ty(&self) -> IRTypeId;
}

// ============================================================================
// IrHashAlgorithm
// ============================================================================

/// A hash algorithm with paired native and IR implementations.
///
/// # Semantic contract
///
/// The value produced by `emit_ir(emitter, key_vars, inputs)` on concrete
/// data **must equal** the value produced by
/// `hash_bytes_native(key_words, input_words)` where each word is the
/// little-endian byte encoding of the corresponding concrete value.
///
/// # Keyed algorithms
///
/// Return the key word types from [`key_schema`](Self::key_schema).  The
/// framework passes the pre-loaded key words to `emit_ir` as `key_vars` and
/// the serialised key bytes to `hash_bytes_native` as `key_words`.
/// Unkeyed algorithms return an empty `key_schema` and receive empty slices.
pub trait IrHashAlgorithm {
    /// Emit IR that computes `H(key_vars, inputs)` into `emitter`.
    ///
    /// `key_vars` contains pre-loaded IR variables for each key word
    /// (type parallel to [`key_schema`](Self::key_schema)).
    /// `inputs` are the bytecode-entry words in serialisation order.
    /// Returns the SSA variable holding the hash output.
    fn emit_ir(
        &self,
        emitter: &mut dyn IrEmitter,
        key_vars: &[(IRVarId, IRTypeId)],
        inputs: &[(IRVarId, IRTypeId)],
    ) -> IRVarId;

    /// The IR type of the value returned by `emit_ir`.
    fn output_type_id(&self, types: &mut IRTypes) -> IRTypeId;

    /// Compute the same hash natively.
    ///
    /// `key_words[i]` is the LE byte slice for the i-th key word.
    /// `inputs[i]` is the LE byte slice for the i-th input word.
    /// Returns the LE byte encoding of the hash output.
    fn hash_bytes_native(&self, key_words: &[&[u8]], inputs: &[&[u8]]) -> Vec<u8>;

    /// IR types of the key words, in order.  Empty for unkeyed algorithms.
    fn key_schema(&self) -> Vec<IRType> {
        Vec::new()
    }

    /// Short algorithm name (for debug output and oracle sub-names).
    fn name(&self) -> &str;

    /// Oracle declarations needed by this algorithm (empty for oracle-free ones).
    fn internal_oracle_decls(&self, types: &mut IRTypes) -> Vec<OracleDecl> {
        let _ = types;
        Vec::new()
    }
}

// ============================================================================
// CommitmentConfig
// ============================================================================

/// Configuration for per-PC bytecode commitment.
pub struct CommitmentConfig<H: IrHashAlgorithm> {
    /// Hash algorithm to use.
    pub algorithm: H,
    /// [`StorageId`] where per-PC commitment hashes are written at setup
    /// and read by handlers.
    pub commitment_storage: StorageId,
    /// Compile-time key values, one per word in [`IrHashAlgorithm::key_schema`].
    /// Empty for unkeyed algorithms.
    pub key: Vec<Constant>,
    /// Storage for key words.  Required when `!key.is_empty()`; the setup
    /// block writes the key params here so handlers can read them back.
    /// Must not overlap with `commitment_storage` or the register-file range.
    pub key_storage: Option<StorageId>,
}

// ============================================================================
// BitEmitter — bridges dyn IrEmitter → BitCircuitBuilder
// ============================================================================

/// Adaptor that implements [`BitCircuitBuilder`] over a `dyn IrEmitter`,
/// emitting Bit-typed Poly/Const statements.
///
/// Used by [`SipHash48`] to call `volar_lir::circuits::bc_add` (the existing
/// ripple-carry adder) without depending on `VolarIrTarget`.
pub(crate) struct BitEmitter<'a> {
    pub(crate) inner: &'a mut dyn IrEmitter,
    pub(crate) bit_ty: IRTypeId,
}

impl BitCircuitBuilder for BitEmitter<'_> {
    type Bit = IRVarId;

    fn bc_const(&mut self, val: bool) -> IRVarId {
        self.inner.emit(IRStmt::Const(Constant { hi: 0, lo: val as u128 }, self.bit_ty))
    }

    fn bc_poly(
        &mut self,
        coeffs: BTreeMap<Vec<IRVarId>, u8>,
        constant: u128,
    ) -> IRVarId {
        self.inner.emit(IRStmt::Poly {
            ty: self.bit_ty,
            coeffs,
            constant: Constant { hi: 0, lo: constant },
        })
    }

    /// Override with the single-Poly degree-2 majority (1 stmt vs 5).
    fn bc_carry3(&mut self, a: IRVarId, b: IRVarId, c: IRVarId) -> IRVarId {
        let mut ab = vec![a, b]; ab.sort();
        let mut ac = vec![a, c]; ac.sort();
        let mut bc = vec![b, c]; bc.sort();
        let mut coeffs = BTreeMap::new();
        coeffs.insert(ab, 1u8);
        coeffs.insert(ac, 1u8);
        coeffs.insert(bc, 1u8);
        self.inner.emit(IRStmt::Poly {
            ty: self.bit_ty,
            coeffs,
            constant: Constant { hi: 0, lo: 0 },
        })
    }
}

// ============================================================================
// XorFoldHash32 — oracle-free reference (testing only)
// ============================================================================

/// Oracle-free 32-bit hash: XOR fold + rotate.
///
/// ```text
/// h₀ = seed
/// hᵢ = Rol( hᵢ₋₁  XOR  truncate_u32(wᵢ₋₁),  13 )
/// ```
///
/// Unkeyed.  Not cryptographically secure — for testing and lightweight
/// structural binding only.
#[derive(Clone, Copy, Debug)]
pub struct XorFoldHash32 {
    pub seed: u32,
}

impl Default for XorFoldHash32 {
    fn default() -> Self {
        Self { seed: 0xDEAD_BEEF }
    }
}

impl IrHashAlgorithm for XorFoldHash32 {
    fn emit_ir(
        &self,
        emitter: &mut dyn IrEmitter,
        _key_vars: &[(IRVarId, IRTypeId)],
        inputs: &[(IRVarId, IRTypeId)],
    ) -> IRVarId {
        let u32_ty = emitter.intern_type(IRType::Primitive(PrimType::_32));
        let mut h = emitter.emit(IRStmt::Const(
            Constant { hi: 0, lo: self.seed as u128 },
            u32_ty,
        ));
        for &(var, ty) in inputs {
            let w = if ty == u32_ty {
                var
            } else {
                emitter.emit(IRStmt::Transmute { src: var, src_ty: ty, dst_ty: u32_ty })
            };
            let mut xor: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
            xor.insert(vec![h], 1);
            xor.insert(vec![w], 1);
            h = emitter.emit(IRStmt::Poly {
                ty: u32_ty,
                coeffs: xor,
                constant: Constant { hi: 0, lo: 0 },
            });
            h = emitter.emit(IRStmt::Rol { src: h, ty: u32_ty, n: 13 });
        }
        h
    }

    fn output_type_id(&self, types: &mut IRTypes) -> IRTypeId {
        types.intern(IRType::Primitive(PrimType::_32))
    }

    fn hash_bytes_native(&self, _key_words: &[&[u8]], inputs: &[&[u8]]) -> Vec<u8> {
        let mut h = self.seed;
        for word in inputs {
            let mut buf = [0u8; 4];
            let n = word.len().min(4);
            buf[..n].copy_from_slice(&word[..n]);
            h ^= u32::from_le_bytes(buf);
            h = h.rotate_left(13);
        }
        h.to_le_bytes().to_vec()
    }

    fn name(&self) -> &str { "xorfold32" }
}

// ============================================================================
// SipHash48 — SipHash-4-8, oracle-free, 128-bit key
// ============================================================================

/// SipHash-4-8: 4 compression rounds, 8 finalization rounds, 128-bit key.
///
/// The key is `k0 ∥ k1` (two 64-bit words).  At runtime the key words are
/// read from the entry-block parameters via `key_storage`; they are always
/// the same compile-time constant value but may be marked as a private
/// witness by a future ZK backend.
///
/// All operations lower directly to IR primitives:
/// * XOR/constant-XOR → `Poly { ty: _64 }`
/// * Rotate-left → `Rol { ty: _64 }`
/// * 64-bit addition → `Shuffle` (decompose) + ripple-carry `bc_add` + `Merge`
///
/// No oracle calls are emitted.  Each call to `emit_ir` emits approximately
/// 12 × 1 042 ≈ 12 500 IR statements (12 SipRounds × 4 adds × ~257 stmts
/// per 64-bit addition, plus rotations and XORs).
#[derive(Clone, Copy, Debug, Default)]
pub struct SipHash48;

// SipHash initialization constants (from the SipHash paper, §2.1).
const SIP_C0: u64 = 0x736f_6d65_7073_6575;
const SIP_C1: u64 = 0x646f_7261_6e64_6f6d;
const SIP_C2: u64 = 0x6c79_6765_6e65_7261;
const SIP_C3: u64 = 0x7465_6462_7974_6573;

impl IrHashAlgorithm for SipHash48 {
    fn key_schema(&self) -> Vec<IRType> {
        vec![
            IRType::Primitive(PrimType::_64),
            IRType::Primitive(PrimType::_64),
        ]
    }

    fn output_type_id(&self, types: &mut IRTypes) -> IRTypeId {
        types.intern(IRType::Primitive(PrimType::_64))
    }

    fn emit_ir(
        &self,
        emitter: &mut dyn IrEmitter,
        key_vars: &[(IRVarId, IRTypeId)],
        inputs: &[(IRVarId, IRTypeId)],
    ) -> IRVarId {
        assert_eq!(key_vars.len(), 2, "SipHash48 requires exactly two 64-bit key words");
        let u64_ty = emitter.intern_type(IRType::Primitive(PrimType::_64));
        let k0 = key_vars[0].0;
        let k1 = key_vars[1].0;

        // Initialise state: vᵢ = kᵢ XOR cᵢ
        let mut v = sip_init_ir(emitter, k0, k1, u64_ty);

        // Serialise inputs: each word is truncated/extended to _64 via Transmute.
        // The last block gets the message-length byte XOR'd into its MSB, per
        // the SipHash spec (§2.4, "b = b || (|m| mod 256)").
        let n = inputs.len();
        let len_byte = (((if n == 0 { 0 } else { n } * 8) & 0xFF) as u64) << 56;

        if n == 0 {
            // Empty message: one all-zero block with just the length byte.
            let zero = emitter.emit(IRStmt::Const(Constant { hi: 0, lo: 0 }, u64_ty));
            let block = if len_byte != 0 {
                sip_xor_const(emitter, zero, len_byte, u64_ty)
            } else {
                zero
            };
            sip_compress_ir(emitter, &mut v, block, 4, u64_ty);
        } else {
            for (i, &(var, ty)) in inputs.iter().enumerate() {
                let m = if ty == u64_ty {
                    var
                } else {
                    emitter.emit(IRStmt::Transmute { src: var, src_ty: ty, dst_ty: u64_ty })
                };
                let block = if i + 1 == n && len_byte != 0 {
                    sip_xor_const(emitter, m, len_byte, u64_ty)
                } else {
                    m
                };
                sip_compress_ir(emitter, &mut v, block, 4, u64_ty);
            }
        }

        // Finalise: v2 ^= 0xff, then 8 SipRounds, then v0^v1^v2^v3.
        sip_finalize_ir(emitter, &mut v, 8, u64_ty)
    }

    fn hash_bytes_native(&self, key_words: &[&[u8]], inputs: &[&[u8]]) -> Vec<u8> {
        assert_eq!(key_words.len(), 2, "SipHash48 requires exactly two key-word slices");
        let k0 = le_bytes_to_u64(key_words[0]);
        let k1 = le_bytes_to_u64(key_words[1]);

        let mut v = [
            k0 ^ SIP_C0,
            k1 ^ SIP_C1,
            k0 ^ SIP_C2,
            k1 ^ SIP_C3,
        ];

        let n = inputs.len();
        let len_byte = (((if n == 0 { 0 } else { n * 8 }) & 0xFF) as u64) << 56;

        if n == 0 {
            let block = len_byte; // 0 ^ len_byte
            sip_compress_native(&mut v, block, 4);
        } else {
            for (i, word) in inputs.iter().enumerate() {
                let m = le_bytes_to_u64(word);
                let block = if i + 1 == n { m ^ len_byte } else { m };
                sip_compress_native(&mut v, block, 4);
            }
        }

        // Finalise.
        v[2] ^= 0xff;
        for _ in 0..8 { sip_round_native(&mut v); }
        let result = v[0] ^ v[1] ^ v[2] ^ v[3];
        result.to_le_bytes().to_vec()
    }

    fn name(&self) -> &str { "siphash48" }
}

// ============================================================================
// SipHash IR helpers
// ============================================================================

/// Initialise the four SipHash state words from key vars.
fn sip_init_ir(
    emitter: &mut dyn IrEmitter,
    k0: IRVarId,
    k1: IRVarId,
    u64_ty: IRTypeId,
) -> [IRVarId; 4] {
    [
        sip_xor_const(emitter, k0, SIP_C0, u64_ty),
        sip_xor_const(emitter, k1, SIP_C1, u64_ty),
        sip_xor_const(emitter, k0, SIP_C2, u64_ty),
        sip_xor_const(emitter, k1, SIP_C3, u64_ty),
    ]
}

/// One SipRound: the mixing permutation applied to the four state words.
fn sip_round_ir(emitter: &mut dyn IrEmitter, v: &mut [IRVarId; 4], u64_ty: IRTypeId) {
    // v0 += v1; v1 <<<= 13; v1 ^= v0; v0 <<<= 32
    v[0] = sip_add64(emitter, v[0], v[1], u64_ty);
    v[1] = sip_rol64(emitter, v[1], 13, u64_ty);
    v[1] = sip_xor(emitter, v[1], v[0], u64_ty);
    v[0] = sip_rol64(emitter, v[0], 32, u64_ty);
    // v2 += v3; v3 <<<= 16; v3 ^= v2
    v[2] = sip_add64(emitter, v[2], v[3], u64_ty);
    v[3] = sip_rol64(emitter, v[3], 16, u64_ty);
    v[3] = sip_xor(emitter, v[3], v[2], u64_ty);
    // v0 += v3; v3 <<<= 21; v3 ^= v0
    v[0] = sip_add64(emitter, v[0], v[3], u64_ty);
    v[3] = sip_rol64(emitter, v[3], 21, u64_ty);
    v[3] = sip_xor(emitter, v[3], v[0], u64_ty);
    // v2 += v1; v1 <<<= 17; v1 ^= v2; v2 <<<= 32
    v[2] = sip_add64(emitter, v[2], v[1], u64_ty);
    v[1] = sip_rol64(emitter, v[1], 17, u64_ty);
    v[1] = sip_xor(emitter, v[1], v[2], u64_ty);
    v[2] = sip_rol64(emitter, v[2], 32, u64_ty);
}

/// Compress one 64-bit message block `m` into the state with `c` SipRounds.
fn sip_compress_ir(
    emitter: &mut dyn IrEmitter,
    v: &mut [IRVarId; 4],
    m: IRVarId,
    c: usize,
    u64_ty: IRTypeId,
) {
    v[3] = sip_xor(emitter, v[3], m, u64_ty);
    for _ in 0..c { sip_round_ir(emitter, v, u64_ty); }
    v[0] = sip_xor(emitter, v[0], m, u64_ty);
}

/// Finalise: XOR `v2 ^= 0xff`, apply `d` SipRounds, return `v0^v1^v2^v3`.
fn sip_finalize_ir(
    emitter: &mut dyn IrEmitter,
    v: &mut [IRVarId; 4],
    d: usize,
    u64_ty: IRTypeId,
) -> IRVarId {
    v[2] = sip_xor_const(emitter, v[2], 0xff, u64_ty);
    for _ in 0..d { sip_round_ir(emitter, v, u64_ty); }
    let r01 = sip_xor(emitter, v[0], v[1], u64_ty);
    let r23 = sip_xor(emitter, v[2], v[3], u64_ty);
    sip_xor(emitter, r01, r23, u64_ty)
}

// ---- Primitive IR word operations ----------------------------------------

/// `a XOR b` over `_64` using `Poly`.
fn sip_xor(emitter: &mut dyn IrEmitter, a: IRVarId, b: IRVarId, u64_ty: IRTypeId) -> IRVarId {
    let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
    coeffs.insert(vec![a], 1);
    coeffs.insert(vec![b], 1);
    emitter.emit(IRStmt::Poly { ty: u64_ty, coeffs, constant: Constant { hi: 0, lo: 0 } })
}

/// `a XOR k` where `k` is a compile-time `u64` constant.
/// Returns `a` unchanged when `k == 0`.
fn sip_xor_const(emitter: &mut dyn IrEmitter, a: IRVarId, k: u64, u64_ty: IRTypeId) -> IRVarId {
    if k == 0 { return a; }
    let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
    coeffs.insert(vec![a], 1);
    emitter.emit(IRStmt::Poly {
        ty: u64_ty,
        coeffs,
        constant: Constant { hi: 0, lo: k as u128 },
    })
}

/// Rotate `a` left by `n` bits over `_64`.
fn sip_rol64(emitter: &mut dyn IrEmitter, a: IRVarId, n: usize, u64_ty: IRTypeId) -> IRVarId {
    emitter.emit(IRStmt::Rol { src: a, ty: u64_ty, n })
}

/// 64-bit modular addition using the ripple-carry emulation from
/// `volar_lir::circuits::bc_add`.
///
/// Decompose both operands into 64 `Bit`-typed vars via `Shuffle`, run the
/// ripple-carry adder, then repack the result bits with `Merge`.
fn sip_add64(emitter: &mut dyn IrEmitter, a: IRVarId, b: IRVarId, u64_ty: IRTypeId) -> IRVarId {
    let bit_ty = emitter.bit_ty();
    // Decompose: bit i of word = Shuffle { result_bits: [(i, word)], ty: Bit }
    let bits_a: Vec<IRVarId> = (0u8..64)
        .map(|i| emitter.emit(IRStmt::Shuffle { result_bits: vec![(i, a)], ty: bit_ty }))
        .collect();
    let bits_b: Vec<IRVarId> = (0u8..64)
        .map(|i| emitter.emit(IRStmt::Shuffle { result_bits: vec![(i, b)], ty: bit_ty }))
        .collect();
    // Ripple-carry add (from volar_lir::circuits).
    let sum_bits = {
        let mut bce = BitEmitter { inner: emitter, bit_ty };
        bc_add(&mut bce, &bits_a, &bits_b, false)
    };
    // Repack bits into a single _64 word.
    emitter.emit(IRStmt::Merge { parts: sum_bits, ty: u64_ty })
}

// ============================================================================
// SipHash native helpers
// ============================================================================

fn sip_round_native(v: &mut [u64; 4]) {
    v[0] = v[0].wrapping_add(v[1]);
    v[1] = v[1].rotate_left(13);
    v[1] ^= v[0];
    v[0] = v[0].rotate_left(32);
    v[2] = v[2].wrapping_add(v[3]);
    v[3] = v[3].rotate_left(16);
    v[3] ^= v[2];
    v[0] = v[0].wrapping_add(v[3]);
    v[3] = v[3].rotate_left(21);
    v[3] ^= v[0];
    v[2] = v[2].wrapping_add(v[1]);
    v[1] = v[1].rotate_left(17);
    v[1] ^= v[2];
    v[2] = v[2].rotate_left(32);
}

fn sip_compress_native(v: &mut [u64; 4], m: u64, c: u8) {
    v[3] ^= m;
    for _ in 0..c { sip_round_native(v); }
    v[0] ^= m;
}

/// Read up to 8 LE bytes from `bytes`, zero-padding to form a `u64`.
/// This matches the behaviour of `Transmute(src → _64)` in the IR, which
/// reinterprets the source bits with zero in the upper positions.
fn le_bytes_to_u64(bytes: &[u8]) -> u64 {
    let mut buf = [0u8; 8];
    let n = bytes.len().min(8);
    buf[..n].copy_from_slice(&bytes[..n]);
    u64::from_le_bytes(buf)
}

// ============================================================================
// Serialisation helpers (used by build_commitment_ctx in ir.rs)
// ============================================================================

/// Canonical little-endian byte encoding for a typed constant.
pub fn constant_to_le_bytes(c: &Constant, ty: &IRType) -> Vec<u8> {
    match ty {
        IRType::Primitive(PrimType::Bit)  => vec![(c.lo & 1) as u8],
        IRType::Primitive(PrimType::_8)   => vec![(c.lo & 0xFF) as u8],
        IRType::Primitive(PrimType::_16)  => (c.lo as u16).to_le_bytes().to_vec(),
        IRType::Primitive(PrimType::_32)  => (c.lo as u32).to_le_bytes().to_vec(),
        IRType::Primitive(PrimType::_64)  => (c.lo as u64).to_le_bytes().to_vec(),
        IRType::Primitive(PrimType::_128) => c.lo.to_le_bytes().to_vec(),
        IRType::Primitive(PrimType::_256) => {
            let mut b = c.lo.to_le_bytes().to_vec();
            b.extend_from_slice(&c.hi.to_le_bytes());
            b
        }
        _ => (c.lo as u32).to_le_bytes().to_vec(),
    }
}

/// Convert hash output bytes (little-endian) to a [`Constant`].
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
    } else { 0 };
    Constant { hi, lo }
}
