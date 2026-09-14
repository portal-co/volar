//! Packing/caching planner for encrypted durable held material.

use volar_vc::oram_material::{MaterialBlockCachePlan, MaterialBlockLayout};

#[test]
fn narrow_labels_share_a_single_fixed_encryption_block() {
    let layout = MaterialBlockLayout::new(4).expect("four-byte label layout");
    assert_eq!(layout.labels_per_block(), 4);
    assert_eq!(layout.block_for(0), 0);
    assert_eq!(layout.block_for(3), 0);
    assert_eq!(layout.block_for(4), 1);
    assert_eq!(layout.offset_in_block(3), 12);

    let mut cache = MaterialBlockCachePlan::new(layout);
    cache.mark_write(0);
    cache.mark_write(1);
    cache.mark_write(3);
    assert_eq!(
        cache.take_flush_blocks(),
        vec![0],
        "three disjoint slots share one AES block"
    );
}

#[test]
fn packed_values_round_trip_at_public_offsets() {
    let layout = MaterialBlockLayout::new(4).expect("four-byte label layout");
    let packed = layout
        .pack_block(0, [(0, &[1, 2, 3, 4][..]), (3, &[5, 6, 7, 8][..])])
        .expect("same public block and fixed width");
    assert_eq!(layout.value_from_block(0, &packed), &[1, 2, 3, 4]);
    assert_eq!(layout.value_from_block(3, &packed), &[5, 6, 7, 8]);
    assert_eq!(layout.value_from_block(1, &packed), &[0, 0, 0, 0]);
    assert!(layout.pack_block(0, [(4, &[1, 2, 3, 4][..])]).is_none());
}

#[test]
fn resident_reads_do_not_need_another_open() {
    let layout = MaterialBlockLayout::new(16).expect("garbling-label layout");
    let mut cache = MaterialBlockCachePlan::new(layout);
    assert!(cache.needs_open(7));
    cache.mark_open(7);
    assert!(!cache.needs_open(7));
    assert_eq!(cache.resident_slots(), 1);
    cache.evict(7);
    assert!(
        cache.needs_open(7),
        "a persistence boundary invalidates cache"
    );
    assert_eq!(cache.resident_slots(), 0);
}

#[test]
fn writes_to_disjoint_blocks_flush_once_per_block() {
    let layout = MaterialBlockLayout::new(8).expect("two labels per block");
    let mut cache = MaterialBlockCachePlan::new(layout);
    cache.mark_write(0);
    cache.mark_write(1);
    cache.mark_write(2);
    cache.mark_write(3);
    assert_eq!(cache.take_flush_blocks(), vec![0, 1]);
}
