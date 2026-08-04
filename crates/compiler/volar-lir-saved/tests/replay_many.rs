// @reliability: experimental
// @ai: assisted
//! Record-once / replay-many fan-out for multi-backend parallelism.

use volar_lir::{LirTarget, LirType};
use volar_lir_saved::{RecordingTarget, SavedLirModule};

fn build_add() -> SavedLirModule {
    let mut rec = RecordingTarget::new();
    let (entry, params) = rec.begin_function(
        "add",
        &[LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    rec.switch_to_block(entry);
    let sum = rec.add(params[0][0], params[1][0]);
    rec.ret(&[sum]);
    rec.end_function();
    rec.finish()
}

#[test]
fn replay_into_many_identical_targets() {
    let saved = build_add();
    let mut a = [RecordingTarget::new(), RecordingTarget::new(), RecordingTarget::new()];
    saved.replay_into_many(&mut a);
    let finished: Vec<SavedLirModule> = a.into_iter().map(|t| t.finish()).collect();
    assert_eq!(finished[0], finished[1]);
    assert_eq!(finished[1], finished[2]);
    assert_eq!(finished[0], saved);
}

#[test]
fn replay_pair_heterogeneous_recordings() {
    let saved = build_add();
    let mut left = RecordingTarget::new();
    let mut right = RecordingTarget::new();
    saved.replay_pair(&mut left, &mut right);
    assert_eq!(left.finish(), right.finish());
}
