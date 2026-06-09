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
pub struct BIrBlocks<P: Clone = ()> {
    /// The blocks of the circuit, in order. Block 0 is the entry.
    pub blocks: Vec<BIrBlock<P>>,
    /// Pre-initialised storage segments propagated from WASM data sections.
    pub pre_init: alloc::vec::Vec<PreInitSegment>,
}

impl<P: Clone> BIrBlocks<P> {
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
pub struct BIrBlock<P: Clone = ()> {
    pub params: u32,
    pub stmts: Vec<BIrStmt>,
    /// Per-statement provenance, same length as `stmts`.
    /// Index `i` is the provenance of `stmts[i]`.
    pub stmt_provs: Vec<P>,
    pub terminator: BIrTerminator,
}

impl<P: Clone> BIrBlock<P> {
    /// Append a statement with an explicit provenance annotation.
    pub fn push_stmt(&mut self, stmt: BIrStmt, prov: P) {
        self.stmts.push(stmt);
        self.stmt_provs.push(prov);
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

impl<P: Clone> BIrBlocks<P> {
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
#[non_exhaustive]
pub enum BIrStmt<Var = IRVarId, Stor = StorageId> {
    // ---- Boolean primitives ------------------------------------------------
    Zero,
    One,
    And(Var, Var),
    Or(Var, Var),
    Xor(Var, Var),
    Not(Var),

    // ---- External primitives -----------------------------------------------

    /// Invoke a named pure oracle.  Produces a call-handle var; individual
    /// output bits are projected with [`OracleBit`].
    ///
    /// `num_bits` is the total number of output bits (sum of the bit-widths of
    /// all oracle result types, expanded to the bit level by the lowering pass).
    OracleCall {
        name: alloc::string::String,
        args: alloc::vec::Vec<Var>,
        num_bits: usize,
    },

    /// Project bit `bit` from an [`OracleCall`] call-handle var.
    OracleBit {
        call: Var,
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
        guard: Var,
        args: alloc::vec::Vec<Var>,
        fallback: alloc::vec::Vec<Var>,
        num_bits: usize,
    },

    /// Project bit `bit` from an [`ActionCall`] call-handle var.
    ActionBit {
        call: Var,
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
        storage: Stor,
        bit_width: usize,
        addr: Vec<Var>,
    },

    /// Write a `bit_width`-wide value `src` to storage, addressed by `addr`.
    ///
    /// `addr` is an N-bit address represented as a `Vec` of single-bit BIR
    /// variables (bit 0 = index 0 = least-significant), giving 2^N distinct
    /// locations per `(StorageId, bit_width)` pair.
    ///
    /// Produces a dummy zero bit (no useful value).
    StorageWrite {
        storage: Stor,
        src: Var,
        bit_width: usize,
        addr: Vec<Var>,
    },
}

impl<Var, Stor> BIrStmt<Var, Stor> {
    /// Map variable and storage parameters, potentially fallibly.
    pub fn map<Ctx, NV, NS, E>(
        self,
        ctx: &mut Ctx,
        mut var_fn: impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
        mut stor_fn: impl FnMut(&mut Ctx, Stor) -> Result<NS, E>,
    ) -> Result<BIrStmt<NV, NS>, E> {
        Ok(match self {
            BIrStmt::Zero => BIrStmt::Zero,
            BIrStmt::One => BIrStmt::One,
            BIrStmt::And(a, b) => BIrStmt::And(var_fn(ctx, a)?, var_fn(ctx, b)?),
            BIrStmt::Or(a, b) => BIrStmt::Or(var_fn(ctx, a)?, var_fn(ctx, b)?),
            BIrStmt::Xor(a, b) => BIrStmt::Xor(var_fn(ctx, a)?, var_fn(ctx, b)?),
            BIrStmt::Not(v) => BIrStmt::Not(var_fn(ctx, v)?),
            BIrStmt::OracleCall { name, args, num_bits } => BIrStmt::OracleCall {
                name,
                args: args.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<_, E>>()?,
                num_bits,
            },
            BIrStmt::OracleBit { call, bit } => BIrStmt::OracleBit { call: var_fn(ctx, call)?, bit },
            BIrStmt::ActionCall { name, guard, args, fallback, num_bits } => BIrStmt::ActionCall {
                name,
                guard: var_fn(ctx, guard)?,
                args: args.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<_, E>>()?,
                fallback: fallback.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<_, E>>()?,
                num_bits,
            },
            BIrStmt::ActionBit { call, bit } => BIrStmt::ActionBit { call: var_fn(ctx, call)?, bit },
            BIrStmt::Rng { name } => BIrStmt::Rng { name },
            BIrStmt::StorageRead { storage, bit_width, addr } => BIrStmt::StorageRead {
                storage: stor_fn(ctx, storage)?,
                bit_width,
                addr: addr.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<_, E>>()?,
            },
            BIrStmt::StorageWrite { storage, src, bit_width, addr } => BIrStmt::StorageWrite {
                storage: stor_fn(ctx, storage)?,
                src: var_fn(ctx, src)?,
                bit_width,
                addr: addr.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<_, E>>()?,
            },
        })
    }

    /// Borrow variable and storage parameters in place.
    ///
    /// `name` fields in `OracleCall`, `ActionCall`, `Rng` are cloned (they
    /// are not generic parameters).
    pub fn as_ref(&self) -> BIrStmt<&Var, &Stor> {
        match self {
            BIrStmt::Zero => BIrStmt::Zero,
            BIrStmt::One => BIrStmt::One,
            BIrStmt::And(a, b) => BIrStmt::And(a, b),
            BIrStmt::Or(a, b) => BIrStmt::Or(a, b),
            BIrStmt::Xor(a, b) => BIrStmt::Xor(a, b),
            BIrStmt::Not(v) => BIrStmt::Not(v),
            BIrStmt::OracleCall { name, args, num_bits } => BIrStmt::OracleCall {
                name: name.clone(),
                args: args.iter().collect(),
                num_bits: *num_bits,
            },
            BIrStmt::OracleBit { call, bit } => BIrStmt::OracleBit { call, bit: *bit },
            BIrStmt::ActionCall { name, guard, args, fallback, num_bits } => BIrStmt::ActionCall {
                name: name.clone(),
                guard,
                args: args.iter().collect(),
                fallback: fallback.iter().collect(),
                num_bits: *num_bits,
            },
            BIrStmt::ActionBit { call, bit } => BIrStmt::ActionBit { call, bit: *bit },
            BIrStmt::Rng { name } => BIrStmt::Rng { name: name.clone() },
            BIrStmt::StorageRead { storage, bit_width, addr } => BIrStmt::StorageRead {
                storage,
                bit_width: *bit_width,
                addr: addr.iter().collect(),
            },
            BIrStmt::StorageWrite { storage, src, bit_width, addr } => BIrStmt::StorageWrite {
                storage,
                src,
                bit_width: *bit_width,
                addr: addr.iter().collect(),
            },
        }
    }

    /// Mutably borrow variable and storage parameters in place.
    ///
    /// `name` fields in `OracleCall`, `ActionCall`, `Rng` are cloned.
    pub fn as_mut(&mut self) -> BIrStmt<&mut Var, &mut Stor> {
        match self {
            BIrStmt::Zero => BIrStmt::Zero,
            BIrStmt::One => BIrStmt::One,
            BIrStmt::And(a, b) => BIrStmt::And(a, b),
            BIrStmt::Or(a, b) => BIrStmt::Or(a, b),
            BIrStmt::Xor(a, b) => BIrStmt::Xor(a, b),
            BIrStmt::Not(v) => BIrStmt::Not(v),
            BIrStmt::OracleCall { name, args, num_bits } => BIrStmt::OracleCall {
                name: name.clone(),
                args: args.iter_mut().collect(),
                num_bits: *num_bits,
            },
            BIrStmt::OracleBit { call, bit } => BIrStmt::OracleBit { call, bit: *bit },
            BIrStmt::ActionCall { name, guard, args, fallback, num_bits } => BIrStmt::ActionCall {
                name: name.clone(),
                guard,
                args: args.iter_mut().collect(),
                fallback: fallback.iter_mut().collect(),
                num_bits: *num_bits,
            },
            BIrStmt::ActionBit { call, bit } => BIrStmt::ActionBit { call, bit: *bit },
            BIrStmt::Rng { name } => BIrStmt::Rng { name: name.clone() },
            BIrStmt::StorageRead { storage, bit_width, addr } => BIrStmt::StorageRead {
                storage,
                bit_width: *bit_width,
                addr: addr.iter_mut().collect(),
            },
            BIrStmt::StorageWrite { storage, src, bit_width, addr } => BIrStmt::StorageWrite {
                storage,
                src,
                bit_width: *bit_width,
                addr: addr.iter_mut().collect(),
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum BIrTerminator<Var = IRVarId> {
    Jmp(BIrTarget<Var>),
    CondJmp {
        val: Var,
        then_target: BIrTarget<Var>,
        else_target: BIrTarget<Var>,
    },
}

impl<Var> BIrTerminator<Var> {
    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        mut go: impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
    ) -> Result<BIrTerminator<NV>, E> {
        Ok(match self {
            BIrTerminator::Jmp(t) => BIrTerminator::Jmp(t.map(ctx, &mut go)?),
            BIrTerminator::CondJmp { val, then_target, else_target } => BIrTerminator::CondJmp {
                val: go(ctx, val)?,
                then_target: then_target.map(ctx, &mut go)?,
                else_target: else_target.map(ctx, &mut go)?,
            },
        })
    }

    pub fn as_ref(&self) -> BIrTerminator<&Var> {
        match self {
            BIrTerminator::Jmp(t) => BIrTerminator::Jmp(t.as_ref()),
            BIrTerminator::CondJmp { val, then_target, else_target } => BIrTerminator::CondJmp {
                val,
                then_target: then_target.as_ref(),
                else_target: else_target.as_ref(),
            },
        }
    }

    pub fn as_mut(&mut self) -> BIrTerminator<&mut Var> {
        match self {
            BIrTerminator::Jmp(t) => BIrTerminator::Jmp(t.as_mut()),
            BIrTerminator::CondJmp { val, then_target, else_target } => BIrTerminator::CondJmp {
                val,
                then_target: then_target.as_mut(),
                else_target: else_target.as_mut(),
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct BIrTarget<Var = IRVarId> {
    pub block: IRBlockTargetId<Var>,
    pub args: Vec<Var>,
}

impl<Var> BIrTarget<Var> {
    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        go: &mut impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
    ) -> Result<BIrTarget<NV>, E> {
        Ok(BIrTarget {
            block: self.block.map(ctx, go)?,
            args: self.args.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
        })
    }

    pub fn as_ref(&self) -> BIrTarget<&Var> {
        BIrTarget {
            block: self.block.as_ref(),
            args: self.args.iter().collect(),
        }
    }

    pub fn as_mut(&mut self) -> BIrTarget<&mut Var> {
        BIrTarget {
            block: self.block.as_mut(),
            args: self.args.iter_mut().collect(),
        }
    }
}
