// @reliability: normal
//! @ai: none
// Boolar IR: boolean circuit IR (AND/XOR/NOT basis).
// Pure data structure definitions; no cryptographic claims.
use super::{ir::*, *};

/// A complete Boolar circuit — a set of boolean-gate blocks.
///
/// The type parameter `P` is an optional per-statement provenance annotation.
/// Use `P = ()` (the default) when provenance is not needed.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BIrBlocks<P: Clone + Default = ()>(pub Vec<BIrBlock<P>>);

impl<P: Clone + Default> BIrBlocks<P> {
    pub fn is_movfuscated(&self) -> bool {
        return self.0.len() == 1;
    }
    pub fn is_circuit(&self) -> bool {
        return self.is_movfuscated()
            && match &self.0[0].terminator {
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
        BIrBlocks(self.0.into_iter().map(|b| b.map_prov_with_handler(handler)).collect())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BIrStmt {
    Zero,
    One,
    And(IRVarId, IRVarId),
    Or(IRVarId, IRVarId),
    Xor(IRVarId, IRVarId),
    Not(IRVarId),
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BIrTerminator {
    Jmp(BIrTarget),
    CondJmp {
        val: IRVarId,
        then_target: BIrTarget,
        else_target: BIrTarget,
    },
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BIrTarget {
    pub block: IRBlockTargetId,
    pub args: Vec<IRVarId>,
}
