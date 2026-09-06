// @reliability: experimental
//! @ai: assisted
//! Intra-region function splitting for the VOLE split-weave
//! (`crate::vole`).
//!
//! Real `rustc -Z time-passes` profiling (documented in
//! `~/.claude/plans/tidy-exploring-duckling.md`) found that a single
//! ~6.4MB woven function costs ~74s / ~19GB peak RSS to compile, with
//! `MIR_borrow_checking` — a per-function-item rustc pass — responsible
//! for the overwhelming majority of both. Since it's per-function-item,
//! splitting one oversized region's own statement range into several
//! smaller Rust functions directly bounds the cost, without touching
//! `movfuscate.rs`'s core loop, `thread_synthetic_slots`, or
//! `split_driver.rs`'s one-call-per-boundary-entry structure at all: the
//! split is entirely internal to how `crate::vole` turns ONE region into
//! Rust functions -- externally there is still exactly one function per
//! `MovfuscBlockBoundary`/`MovfuscAccumStep` entry (a thin "wrapper" that
//! keeps the region's original name/params/return-tuple shape and calls
//! the split-out "piece" functions in sequence).
//!
//! This module contains only the pure, IR-emission-free planning step:
//! given a region's own `[start, end)` statement-var-id range, decide
//! where to cut it into pieces and which vars need threading across a
//! cut. It does not touch `VoleIrCtx` or build any `IrFunction` --
//! `crate::vole`'s own per-region loops consume [`PieceSpec`] to do that.
//!
//! ## Algorithm
//!
//! Pieces are strictly sequential slices of one already-CSE'd,
//! already-topologically-ordered statement range (no reordering happens
//! at this stage -- that already ran earlier in the pipeline), so a
//! var's producer piece and its last-using piece always satisfy
//! `producer <= last` trivially: simpler than
//! [`volar_ir_passes::thread_synthetic_slots`]'s general cross-region
//! case, which has no such ordering guarantee.
//!
//! For each var `v` defined within `[start, end)`:
//! - its `producer` piece is whichever piece's own sub-range contains it;
//! - its `last`-use piece is the greatest piece index whose own
//!   statements reference `v` as an operand.
//!
//! If `last > producer`, `v` needs threading: `producer`'s own piece
//! exports it (`extra_out`), every intervening piece
//! `producer+1..=last` receives it (`extra_in`), and every piece
//! `producer+1..last` (exclusive of `last`) re-exports it onward
//! (`extra_out`) -- the same "producer..last" pass-through shape
//! `thread_synthetic_slots` already uses, just scoped to one region's
//! own pieces instead of the whole circuit's regions.
//!
//! `outputs` (the region's own must-produce fields -- `is_active`/
//! `done`/`next_pc_bits`/`next_state`/`ret_vals`/`synthetic_out`) are
//! handled directly rather than via last-use threading: the wrapper
//! reads each output straight from whichever piece produced it, with no
//! relay needed, so an output var just needs to be present in its own
//! producer's `extra_out` -- even if no *other* piece ever references it
//! internally (a pure pass-through field with zero interior consumers,
//! the one case last-use tracking alone would silently miss -- this is
//! deliberately NOT folded into the last-use bookkeeping above).
//! An output that lives OUTSIDE `[start, end)` entirely (e.g. a
//! tunnelled top-level-param pass-through, or a shared-prefix var) is
//! resolvable by ANY piece -- every piece binds the region's full
//! shared-prefix/top-level-params/synthetic_in preamble, identically to
//! what the original unsplit function bound -- so it's assigned to piece
//! 0 by convention, keeping the wrapper itself free of any `VoleIrCtx`
//! of its own (pure call-and-destructure, cheap to borrowck by
//! construction).

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;
use volar_ir::ir::{IRBlock, IRVarId};

/// One contiguous sub-range ("piece") of an oversized region, plus the
/// extra values it needs threaded in/out relative to its sibling pieces
/// within the same region. `extra_in`/`extra_out` are sorted ascending
/// (deterministic tuple-element order between the piece-function-builder
/// and the wrapper-builder, both driven by this same list).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PieceSpec {
    pub start: u32,
    pub end: u32,
    /// Vars defined by an earlier piece (or, for piece 0 only, possibly
    /// unused -- piece 0 never has cross-piece inputs) that this piece's
    /// own statements reference, or must pass through to a later piece.
    pub extra_in: Vec<u32>,
    /// Vars this piece must return: needed by a later piece, and/or a
    /// member of the region's own `outputs` list produced here.
    pub extra_out: Vec<u32>,
}

/// Plan how to split one region's own `[start, end)` statement range
/// into multiple Rust functions. Returns a single piece (with empty
/// `extra_in`/`extra_out`) unchanged when the region is already small
/// enough -- callers can treat the "no split" and "split" cases
/// uniformly by always consuming the returned `Vec<PieceSpec>`.
///
/// `block.stmts[i]` corresponds to var id `num_params + i` -- the same
/// convention used throughout `crate::vole`/`movfuscate.rs`.
/// `outputs` must be the FULL set of vars the region's own external
/// interface must produce (native `is_active`/`done`/`next_pc_bits`/
/// `next_state`/`ret_vals` AND `synthetic_out`) -- omitting any of these
/// silently drops that value from the wrapper's own final tuple.
pub(crate) fn split_region_into_pieces<P: Clone>(
    block: &IRBlock<P>,
    num_params: usize,
    start: u32,
    end: u32,
    outputs: &[u32],
    max_stmts_per_piece: usize,
) -> Vec<PieceSpec> {
    assert!(max_stmts_per_piece > 0, "split_region_into_pieces: max_stmts_per_piece must be positive");
    assert!(end >= start, "split_region_into_pieces: end must be >= start (start={start}, end={end})");

    let total = (end - start) as usize;
    if total <= max_stmts_per_piece {
        return alloc::vec![PieceSpec { start, end, extra_in: Vec::new(), extra_out: Vec::new() }];
    }

    let k = total.div_ceil(max_stmts_per_piece);
    let piece_len = total.div_ceil(k) as u32;
    let mut ranges: Vec<(u32, u32)> = Vec::new();
    let mut cur = start;
    while cur < end {
        let nxt = core::cmp::min(cur + piece_len, end);
        ranges.push((cur, nxt));
        cur = nxt;
    }
    let k = ranges.len();

    let piece_of = |v: u32| -> Option<usize> {
        if v < start || v >= end {
            return None;
        }
        let idx = (((v - start) / piece_len) as usize).min(k - 1);
        debug_assert!(ranges[idx].0 <= v && v < ranges[idx].1, "split_region_into_pieces: piece_of arithmetic mismatch for var {v}");
        Some(idx)
    };

    // last_use[v] = greatest piece index (> its producer) whose own
    // statements reference v as an operand.
    let mut last_use: BTreeMap<u32, usize> = BTreeMap::new();
    for (i, &(lo, hi)) in ranges.iter().enumerate() {
        let stmt_lo = (lo as usize) - num_params;
        let stmt_hi = (hi as usize) - num_params;
        for node in &block.stmts[stmt_lo..stmt_hi] {
            let mut ops: Vec<u32> = Vec::new();
            let _ = node.kind.clone().map_var(
                &mut ops,
                &mut |ops: &mut Vec<u32>, v: IRVarId| -> Result<IRVarId, core::convert::Infallible> {
                    ops.push(v.0);
                    Ok(v)
                },
                &mut |_, ty| Ok(ty),
                &mut |_, s| Ok(s),
            );
            for v in ops {
                if let Some(p) = piece_of(v) {
                    if p < i {
                        let e = last_use.entry(v).or_insert(p);
                        if i > *e {
                            *e = i;
                        }
                    }
                }
            }
        }
    }

    let mut extra_out: Vec<BTreeSet<u32>> = alloc::vec![BTreeSet::new(); k];
    let mut extra_in: Vec<BTreeSet<u32>> = alloc::vec![BTreeSet::new(); k];

    for (&v, &last) in &last_use {
        let p = piece_of(v).expect("split_region_into_pieces: var in last_use must belong to a piece");
        extra_out[p].insert(v);
        for j in (p + 1)..=last {
            extra_in[j].insert(v);
            if j != last {
                extra_out[j].insert(v);
            }
        }
    }

    for &o in outputs {
        match piece_of(o) {
            Some(p) => {
                extra_out[p].insert(o);
            }
            // Outside [start,end) entirely -- every piece binds the same
            // shared-prefix/top-level-params/synthetic_in preamble the
            // original unsplit function did, so piece 0 can resolve it
            // directly; assigned here by convention so the wrapper needs
            // no VoleIrCtx of its own.
            None => {
                extra_out[0].insert(o);
            }
        }
    }

    ranges
        .into_iter()
        .enumerate()
        .map(|(i, (s, e))| PieceSpec {
            start: s,
            end: e,
            extra_in: extra_in[i].iter().copied().collect(),
            extra_out: extra_out[i].iter().copied().collect(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    extern crate std;

    use super::*;
    use volar_ir::ir::{IRBlockTargetId, IRBranchTarget, IRStmt, IRTerminator, IRTypeId};
    use volar_ir_common::{Constant, Node, PolyCoeffs, Stmt};

    /// Builds an `IRBlock<()>` with `num_params` dummy params and pushes
    /// a `Poly` statement referencing `refs` (as a degree-1 monomial per
    /// ref, summed) for each entry of `stmt_refs` -- enough for
    /// `split_region_into_pieces`'s own operand-graph analysis, which
    /// only inspects `map_var`, not statement semantics/types. Returns
    /// the block plus the starting var id (`= num_params`).
    fn build_block(num_params: usize, stmt_refs: &[&[u32]]) -> (IRBlock<()>, u32) {
        let dummy_ty = IRTypeId(0);
        let mut block: IRBlock<()> = IRBlock {
            params: alloc::vec![dummy_ty; num_params],
            stmts: Vec::new(),
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget { dest: IRBlockTargetId::Return, args: Vec::new(), reentry: None },
            },
        };
        for refs in stmt_refs {
            let coeffs: PolyCoeffs<_> = refs
                .iter()
                .map(|&v| (alloc::vec![volar_ir::ir::IRVarId(v)], 1u8))
                .collect();
            let stmt: IRStmt = Stmt::Poly { ty: dummy_ty, coeffs, constant: Constant { hi: 0, lo: 0 } };
            block.stmts.push(Node::new(stmt, (), None));
        }
        (block, num_params as u32)
    }

    #[test]
    fn no_split_when_under_threshold() {
        let (block, start) = build_block(2, &[&[0], &[1], &[2]]);
        let end = start + block.stmts.len() as u32;
        let pieces = split_region_into_pieces(&block, 2, start, end, &[], 100);
        assert_eq!(pieces.len(), 1);
        assert_eq!(pieces[0].start, start);
        assert_eq!(pieces[0].end, end);
        assert!(pieces[0].extra_in.is_empty());
        assert!(pieces[0].extra_out.is_empty());
    }

    #[test]
    fn adjacent_dependency_threads_directly() {
        // num_params=0, 4 statements -> vars 0,1,2,3. Split into 2
        // pieces of 2 stmts each: piece0=[0,2), piece1=[2,4). Statement
        // 3 (var 3) references var 1 (defined in piece 0) -- an
        // adjacent-piece dependency, no intervening pieces.
        let (block, start) = build_block(0, &[&[], &[], &[1], &[]]);
        let end = start + block.stmts.len() as u32;
        let pieces = split_region_into_pieces(&block, 0, start, end, &[], 2);
        assert_eq!(pieces.len(), 2);
        assert_eq!(pieces[0].extra_out, alloc::vec![1]);
        assert_eq!(pieces[1].extra_in, alloc::vec![1]);
        assert!(pieces[1].extra_out.is_empty());
    }

    #[test]
    fn distant_dependency_threads_through_intervening_pieces() {
        // 6 statements, split into 3 pieces of 2: [0,2) [2,4) [4,6).
        // Var 0 (piece 0) is referenced only by statement 5 (var 5,
        // piece 2) -- piece 1 never touches it directly and must
        // pass it through unchanged.
        let (block, start) = build_block(0, &[&[], &[], &[], &[], &[], &[0]]);
        let end = start + block.stmts.len() as u32;
        let pieces = split_region_into_pieces(&block, 0, start, end, &[], 2);
        assert_eq!(pieces.len(), 3);
        assert_eq!(pieces[0].extra_out, alloc::vec![0], "producer piece must export");
        assert_eq!(pieces[1].extra_in, alloc::vec![0], "intervening piece must receive");
        assert_eq!(pieces[1].extra_out, alloc::vec![0], "intervening piece must re-export (pass-through)");
        assert_eq!(pieces[2].extra_in, alloc::vec![0], "consuming piece must receive");
        assert!(pieces[2].extra_out.is_empty(), "last consumer needs no further re-export");
    }

    #[test]
    fn output_with_zero_interior_consumers_still_exported() {
        // The critical correctness gotcha this module's own doc warns
        // about: a pure pass-through `outputs` member with NO statement
        // anywhere in the region ever referencing it as an operand must
        // still be exported by its own producer piece, purely because
        // it's a member of `outputs` -- last-use tracking alone would
        // never see it (nothing consumes it internally).
        let (block, start) = build_block(0, &[&[], &[], &[], &[]]);
        let end = start + block.stmts.len() as u32;
        // var 0 (defined by stmt 0, in piece 0 given max_stmts_per_piece=2)
        // is a region output but is never referenced by any statement.
        let pieces = split_region_into_pieces(&block, 0, start, end, &[0], 2);
        assert_eq!(pieces.len(), 2);
        assert_eq!(pieces[0].extra_out, alloc::vec![0], "output var must be exported by its producer piece even with zero interior consumers");
        assert!(pieces[1].extra_in.is_empty(), "no other piece needs it -- the wrapper reads it straight from piece 0");
    }

    #[test]
    fn output_living_outside_region_assigned_to_piece_zero() {
        // An output that's a top-level param / shared-prefix var (i.e.
        // physically outside [start,end) -- var id < start) must be
        // resolvable without any piece needing extra threading for it;
        // by convention it's assigned to piece 0 (every piece binds the
        // same preamble, so piece 0 can resolve it exactly like the
        // original unsplit function did).
        let (block, start) = build_block(5, &[&[], &[], &[], &[]]);
        let end = start + block.stmts.len() as u32;
        // var 2 is a top-level param (< num_params=5, definitely < start=5).
        let pieces = split_region_into_pieces(&block, 5, start, end, &[2], 2);
        assert_eq!(pieces.len(), 2);
        assert_eq!(pieces[0].extra_out, alloc::vec![2]);
        assert!(pieces[1].extra_out.is_empty());
        assert!(pieces[0].extra_in.is_empty());
        assert!(pieces[1].extra_in.is_empty());
    }

    #[test]
    fn output_in_non_final_piece_with_no_consumers_still_exported() {
        // Same gotcha as above, but confirms it holds when the
        // zero-consumer output lives in a piece that ISN'T the last one
        // (3 pieces, output in piece 0, no other piece references it).
        let (block, start) = build_block(0, &[&[], &[], &[], &[], &[], &[]]);
        let end = start + block.stmts.len() as u32;
        let pieces = split_region_into_pieces(&block, 0, start, end, &[0], 2);
        assert_eq!(pieces.len(), 3);
        assert_eq!(pieces[0].extra_out, alloc::vec![0]);
        assert!(pieces[1].extra_in.is_empty());
        assert!(pieces[2].extra_in.is_empty());
    }

    #[test]
    fn multiple_vars_threaded_independently() {
        // 3 pieces; var 0 (piece 0) used in piece 2 only; var 3 (piece 1)
        // used in piece 2 only -- two independent threading chains of
        // different lengths, must not interfere with each other.
        let (block, start) = build_block(0, &[&[], &[], &[], &[], &[0], &[3]]);
        let end = start + block.stmts.len() as u32;
        let pieces = split_region_into_pieces(&block, 0, start, end, &[], 2);
        assert_eq!(pieces.len(), 3);
        assert_eq!(pieces[0].extra_out, alloc::vec![0]);
        assert_eq!(pieces[1].extra_in, alloc::vec![0]);
        assert_eq!(pieces[1].extra_out, alloc::vec![0, 3]);
        assert_eq!(pieces[2].extra_in, alloc::vec![0, 3]);
    }
}
