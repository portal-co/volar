//! Conservative public ORAM return planning.
//!
//! Logical addresses and labels are never inspected here.  The planner only
//! sees already-public physical leaves. It coalesces repeated read-path fetches
//! and identifies writes whose *non-root* physical buckets are disjoint. Path
//! ORAM roots always overlap, so root commits remain serial; callers must not
//! claim an atomic whole-path multi-write merely because two leaves differ.

use alloc::collections::BTreeSet;
use alloc::vec;
use alloc::vec::Vec;

/// One evaluator-hosted physical-path operation, named only by public leaf.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PublicPathOp {
    pub leaf: u64,
    pub write: bool,
}

/// A reusable evaluator path fetch. Every member is read-only and names the
/// same public leaf; one ciphertext-path read can feed all consumers that do
/// not mutate it in between.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReadPathReuse {
    pub leaf: u64,
    pub consumers: usize,
}

/// Non-root bucket writes that do not overlap. `root_commits` still needs the
/// original sequential order, because every Path ORAM path contains the root.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DisjointBelowRootWrites {
    pub leaves: Vec<u64>,
    pub non_root_nodes: Vec<usize>,
    pub root_commits: Vec<u64>,
}

/// Coalesce consecutive repeated public reads. A write is a cache barrier: it
/// invalidates a prior read-path fetch, even if its leaf is the same.
pub fn plan_read_reuse(ops: &[PublicPathOp]) -> Vec<ReadPathReuse> {
    let mut planned = Vec::new();
    let mut current: Option<ReadPathReuse> = None;
    for op in ops {
        if op.write {
            if let Some(read) = current.take() {
                planned.push(read);
            }
            continue;
        }
        match &mut current {
            Some(read) if read.leaf == op.leaf => read.consumers += 1,
            Some(_) => {
                planned.push(
                    current
                        .replace(ReadPathReuse {
                            leaf: op.leaf,
                            consumers: 1,
                        })
                        .expect("current"),
                );
            }
            None => {
                current = Some(ReadPathReuse {
                    leaf: op.leaf,
                    consumers: 1,
                })
            }
        }
    }
    if let Some(read) = current {
        planned.push(read);
    }
    planned
}

/// Compute public heap indices below the root for a Path ORAM leaf. `levels`
/// includes the root; `leaf` must fit in the `levels - 1` bit leaf domain.
pub fn non_root_path_nodes(levels: usize, leaf: u64) -> Option<Vec<usize>> {
    if levels == 0 || leaf >= (1u64 << levels.saturating_sub(1)) {
        return None;
    }
    Some(
        (1..levels)
            .map(|depth| {
                let prefix = (leaf >> (levels - 1 - depth)) as usize;
                (1usize << depth) - 1 + prefix
            })
            .collect(),
    )
}

/// Return a batch only when every pair is disjoint below the root. This lets a
/// transport return those buckets in one grouped I/O phase, while preserving
/// root writes in `root_commits` order. Repeated leaves or common descendants
/// return `None` and must remain fully serial.
pub fn plan_disjoint_below_root_writes(
    levels: usize,
    leaves: &[u64],
) -> Option<DisjointBelowRootWrites> {
    let mut seen = BTreeSet::new();
    let mut non_root_nodes = Vec::new();
    for &leaf in leaves {
        let nodes = non_root_path_nodes(levels, leaf)?;
        if nodes.iter().any(|node| !seen.insert(*node)) {
            return None;
        }
        non_root_nodes.extend(nodes);
    }
    Some(DisjointBelowRootWrites {
        leaves: leaves.to_vec(),
        non_root_nodes,
        root_commits: leaves.to_vec(),
    })
}

/// A different leaf means two paths diverge somewhere below the root, but they
/// may still share a depth-one ancestor. Use [`plan_disjoint_below_root_writes`]
/// rather than this predicate to authorize grouped physical write I/O.
pub const fn leaves_diverge_somewhere_below_root(a: u64, b: u64) -> bool {
    a != b
}
