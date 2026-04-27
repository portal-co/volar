// @reliability: experimental
// @ai: assisted
//! Round-trip serialization test for [`SavedLirModule`].
//!
//! Records a simple `add(u32, u32) -> u32` function, serializes with rkyv,
//! deserializes, replays into a fresh [`RecordingTarget`], and asserts the
//! resulting [`SavedLirModule`] is identical to the original.
//!
//! Only compiled when the `rkyv` feature is enabled.

#![cfg(feature = "rkyv")]

use volar_lir::{LirTarget, LirType};
use volar_lir_saved::{RecordingTarget, SavedLirModule};

/// Build a module with a single function:
/// ```text
/// fn add(a: u32, b: u32) -> u32 { return a + b; }
/// ```
fn build_add_module() -> SavedLirModule {
    let mut rec = RecordingTarget::new();
    let (entry, params) = rec.begin_function(
        "add",
        &[LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    rec.switch_to_block(entry);
    let a = params[0][0];
    let b = params[1][0];
    let sum = rec.add(a, b);
    rec.ret(&[sum]);
    rec.end_function();
    rec.finish()
}

/// Build a more complex module exercising branches and block params:
/// ```text
/// fn max(a: u32, b: u32) -> u32 {
///     if a >= b { return a; } else { return b; }
/// }
/// ```
fn build_max_module() -> SavedLirModule {
    use volar_lir::IcmpPred;

    let mut rec = RecordingTarget::new();
    let (entry, params) = rec.begin_function(
        "max",
        &[LirType::U32, LirType::U32],
        Some(LirType::U32),
    );
    let a = params[0][0];
    let b = params[1][0];

    let then_block = rec.create_block();
    let else_block = rec.create_block();
    let merge_block = rec.create_block();
    let result_param = rec.add_block_param(merge_block, LirType::U32);

    rec.switch_to_block(entry);
    let cond = rec.icmp(IcmpPred::Uge, a, b);
    rec.branch(cond, then_block, &[], else_block, &[]);

    rec.switch_to_block(then_block);
    rec.jump(merge_block, &[a]);

    rec.switch_to_block(else_block);
    rec.jump(merge_block, &[b]);

    rec.switch_to_block(merge_block);
    rec.ret(&[result_param]);

    rec.end_function();
    rec.finish()
}

fn round_trip(original: &SavedLirModule) -> SavedLirModule {
    // Serialize.
    let bytes = rkyv::to_bytes::<rkyv::rancor::Error>(original)
        .expect("rkyv::to_bytes failed");
    assert!(!bytes.is_empty(), "serialized bytes must be non-empty");

    // Deserialize.  We trust bytes we just serialized, so unchecked is fine.
    let deserialized: SavedLirModule = unsafe {
        rkyv::from_bytes_unchecked::<SavedLirModule, rkyv::rancor::Error>(bytes.as_slice())
            .expect("rkyv::from_bytes_unchecked failed")
    };

    // Replay into a fresh RecordingTarget.
    let mut replay_rec = RecordingTarget::new();
    deserialized.replay(&mut replay_rec);
    replay_rec.finish()
}

#[test]
fn add_module_round_trip() {
    let original = build_add_module();
    let replayed = round_trip(&original);
    assert_eq!(
        original, replayed,
        "round-tripped `add` module does not match original"
    );
}

#[test]
fn max_module_round_trip() {
    let original = build_max_module();
    let replayed = round_trip(&original);
    assert_eq!(
        original, replayed,
        "round-tripped `max` module does not match original"
    );
}

#[test]
fn empty_module_round_trip() {
    let original = SavedLirModule::default();
    let replayed = round_trip(&original);
    assert_eq!(original, replayed, "round-tripped empty module does not match");
}
