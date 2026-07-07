// @reliability: experimental
// @ai: assisted
//! Unified bytecode layout for outer program rows and appended regions.

use alloc::vec::Vec;

use crate::bytecode::{AppendedRegionKind, AppendedRegionMeta, OperandMode, TripCount};

/// Flat bytecode map: outer rows first, appended regions after.
#[derive(Clone, Debug, Default)]
pub struct UnifiedBytecodeLayout {
    /// Number of original blocks (= outer row count).
    pub outer_block_count: usize,
    /// Appended region metadata in pc order.
    pub regions: Vec<AppendedRegionMeta>,
    /// Total bytecode rows (outer + all appended opcode/descriptor rows).
    pub total_rows: usize,
}

impl UnifiedBytecodeLayout {
    /// First global pc of appended regions.
    pub fn appended_base(&self) -> u32 {
        self.outer_block_count as u32
    }

    pub fn push_region(
        &mut self,
        kind: AppendedRegionKind,
        row_count: usize,
    ) -> AppendedRegionMeta {
        let pc_start = self.total_rows as u32;
        let pc_end = pc_start + row_count as u32;
        self.total_rows = pc_end as usize;
        let meta = AppendedRegionMeta {
            pc_start,
            pc_end,
            kind,
        };
        self.regions.push(meta.clone());
        meta
    }
}

/// Per-block composite execution plan after merging cross-block and reroll planners.
#[derive(Clone, Debug, Default)]
pub struct BlockCompositePlan {
    pub prologue: core::ops::Range<usize>,
    pub segments: alloc::vec::Vec<SegmentInvoke>,
    pub epilogue: core::ops::Range<usize>,
}

#[derive(Clone, Debug)]
pub enum SegmentInvoke {
    SharedCore {
        region_index: usize,
        entry_offset: u32,
    },
    RerollLoop {
        region_index: usize,
    },
}

/// Merged adaptive split output consumed by emission.
#[derive(Clone, Debug, Default)]
pub struct AdaptiveSplitPlan {
    pub block_plans: alloc::vec::Vec<BlockCompositePlan>,
    pub layout: UnifiedBytecodeLayout,
    /// SharedCore regions: stmt range members before micro-block expansion.
    pub shared_cores: alloc::vec::Vec<SharedCoreSpec>,
    pub reroll_loops: alloc::vec::Vec<RerollLoopSpec>,
}

#[derive(Clone, Debug)]
pub struct SharedCoreSpec {
    pub members: alloc::vec::Vec<(usize, core::ops::Range<usize>)>,
}

#[derive(Clone, Debug)]
pub struct RerollLoopSpec {
    pub owner_block: usize,
    pub body_range: core::ops::Range<usize>,
    pub trip_count: TripCount,
    pub operand_mode: OperandMode,
    /// Total stmt span replaced by reroll (= body_len × trips).
    pub covered_range: core::ops::Range<usize>,
}
