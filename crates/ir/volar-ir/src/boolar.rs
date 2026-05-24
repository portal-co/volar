// @reliability: normal
//! @ai: assisted
// Boolar IR: boolean circuit IR (AND/XOR/NOT basis).
// Pure data structure definitions; no cryptographic claims.
use super::{ir::*, *};
use volar_ir_common::{PreInitSegment, StorageId};

/// A complete Boolar circuit — a set of boolean-gate blocks.
///
/// The type parameter `P` is an optional per-statement provenance annotation.
/// Use `P = ()` (the default) when provenance is not needed.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct BIrBlocks<P: Clone + Default = ()> {
    /// The blocks of the circuit, in order. Block 0 is the entry.
    pub blocks: Vec<BIrBlock<P>>,
    /// Pre-initialised storage segments propagated from WASM data sections.
    pub pre_init: alloc::vec::Vec<PreInitSegment>,
}

impl<P: Clone + Default> BIrBlocks<P> {
    pub fn is_movfuscated(&self) -> bool {
        return self.blocks.len() == 1;
    }
    pub fn is_circuit(&self) -> bool {
        return self.is_movfuscated()
            && match &self.blocks[0].terminator {
                BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    ..
                }) => true,
                _ => false,
            };
    }
}

/// A single block in a Boolar circuit.
///
/// The type parameter `P` is an optional per-statement provenance annotation
/// (parallel to `stmts`).  Use `P = ()` when provenance is not needed.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct BIrBlock<P: Clone + Default = ()> {
    pub params: u32,
    pub stmts: Vec<BIrStmt>,
    /// Per-statement provenance, same length as `stmts`.
    /// Index `i` is the provenance of `stmts[i]`.
    pub stmt_provs: Vec<P>,
    pub terminator: BIrTerminator,
}

impl<P: Clone + Default> BIrBlock<P> {
    /// Append a statement with an explicit provenance annotation.
    pub fn push_stmt(&mut self, stmt: BIrStmt, prov: P) {
        self.stmts.push(stmt);
        self.stmt_provs.push(prov);
    }

    /// Append a statement using `P::default()` as the provenance.
    pub fn push_stmt_default(&mut self, stmt: BIrStmt) {
        self.push_stmt(stmt, P::default());
    }

    /// Map provenance annotations using a [`ProvenanceHandler`].
    pub fn map_prov_with_handler<H: volar_provenance::ProvenanceHandler<P>>(self, handler: &H) -> BIrBlock<H::Output> {
        BIrBlock {
            params: self.params,
            stmts: self.stmts,
            stmt_provs: self.stmt_provs.into_iter().map(|p| handler.map(&p)).collect(),
            terminator: self.terminator,
        }
    }
}

impl<P: Clone + Default> BIrBlocks<P> {
    /// Map provenance annotations using a [`ProvenanceHandler`].
    pub fn map_prov_with_handler<H: volar_provenance::ProvenanceHandler<P>>(self, handler: &H) -> BIrBlocks<H::Output> {
        BIrBlocks {
            blocks: self.blocks.into_iter().map(|b| b.map_prov_with_handler(handler)).collect(),
            pre_init: self.pre_init,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum BIrStmt {
    // ---- Boolean primitives ------------------------------------------------
    Zero,
    One,
    And(IRVarId, IRVarId),
    Or(IRVarId, IRVarId),
    Xor(IRVarId, IRVarId),
    Not(IRVarId),

    // ---- External primitives -----------------------------------------------

    /// Invoke a named pure oracle.  Produces a call-handle var; individual
    /// output bits are projected with [`OracleBit`].
    ///
    /// `num_bits` is the total number of output bits (sum of the bit-widths of
    /// all oracle result types, expanded to the bit level by the lowering pass).
    OracleCall {
        name: alloc::string::String,
        args: alloc::vec::Vec<IRVarId>,
        num_bits: usize,
    },

    /// Project bit `bit` from an [`OracleCall`] call-handle var.
    OracleBit {
        call: IRVarId,
        bit: usize,
    },

    /// Conditionally invoke a named action (guard is a separate boolean wire).
    /// Produces a call-handle var; individual output bits are projected with
    /// [`ActionBit`].
    ///
    /// `fallback` contains one `IRVarId` per output bit; these are used when
    /// `guard = 0` and the action is not invoked.
    ActionCall {
        name: alloc::string::String,
        guard: IRVarId,
        args: alloc::vec::Vec<IRVarId>,
        fallback: alloc::vec::Vec<IRVarId>,
        num_bits: usize,
    },

    /// Project bit `bit` from an [`ActionCall`] call-handle var.
    ActionBit {
        call: IRVarId,
        bit: usize,
    },

    /// Fresh independent random bit from the named RNG source.
    ///
    /// `name` matches an [`RngDecl`] in the enclosing circuit.  Each
    /// occurrence is an independent sample; optimisers must not CSE or
    /// reorder `Rng` stmts.
    Rng {
        name: alloc::string::String,
    },

    /// Read `bit_width` bits from a storage space, addressed by a bit-vector.
    ///
    /// `addr` is an N-bit address represented as a `Vec` of single-bit BIR
    /// variables (bit 0 = index 0 = least-significant), giving 2^N distinct
    /// locations per `(StorageId, bit_width)` pair.
    ///
    /// The result var represents the entire `bit_width`-wide value as a single
    /// Boolar handle; individual bits are not separately addressable at this
    /// level.  The lowering pass or weaver expands this as needed.
    StorageRead {
        storage: StorageId,
        bit_width: usize,
        addr: Vec<IRVarId>,
    },

    /// Write a `bit_width`-wide value `src` to storage, addressed by `addr`.
    ///
    /// `addr` is an N-bit address represented as a `Vec` of single-bit BIR
    /// variables (bit 0 = index 0 = least-significant), giving 2^N distinct
    /// locations per `(StorageId, bit_width)` pair.
    ///
    /// Produces a dummy zero bit (no useful value).
    StorageWrite {
        storage: StorageId,
        src: IRVarId,
        bit_width: usize,
        addr: Vec<IRVarId>,
    },
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum BIrTerminator {
    Jmp(BIrTarget),
    CondJmp {
        val: IRVarId,
        then_target: BIrTarget,
        else_target: BIrTarget,
    },
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct BIrTarget {
    pub block: IRBlockTargetId,
    pub args: Vec<IRVarId>,
}
