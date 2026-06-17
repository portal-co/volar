// @reliability: experimental
// @ai: assisted
//! Configuration for adaptive split / specialized regions.
//!
//! See [`docs/agent-context/virt-adaptive-split-adr.md`](../../docs/agent-context/virt-adaptive-split-adr.md).

/// Knobs for cross-block SharedCore and intra-block RerollLoop planners.
///
/// Default: disabled — [`crate::virtualize_ir`] behavior is unchanged.
#[derive(Clone, Debug)]
pub struct AdaptiveSplitConfig {
    /// Master switch. When `false`, planners and emission hooks are skipped.
    pub enabled: bool,
    /// Cross-block shared stmt sequence extraction.
    pub cross_block: bool,
    pub min_sequence_len: usize,
    pub min_reuse_count: usize,
    /// Intra-block loop rerolling via descriptor + trip count.
    pub loop_reroll: bool,
    pub min_reroll_iterations: usize,
    pub min_reroll_body_len: usize,
    /// Cost model: minimum stmt savings to accept a region.
    pub sub_interp_entry_cost: usize,
    pub max_appended_regions: usize,
    /// Prefer reentry-hint CFG loops over structural intra-block reroll search.
    pub prefer_reentry_hints: bool,
}

impl Default for AdaptiveSplitConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            cross_block: true,
            min_sequence_len: 4,
            min_reuse_count: 2,
            loop_reroll: true,
            min_reroll_iterations: 3,
            min_reroll_body_len: 2,
            sub_interp_entry_cost: 4,
            max_appended_regions: 64,
            prefer_reentry_hints: true,
        }
    }
}

impl AdaptiveSplitConfig {
    pub fn enabled(cfg: &Self) -> bool {
        cfg.enabled
    }
}
