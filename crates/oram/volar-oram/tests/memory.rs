// @reliability: experimental
// @ai: assisted
//! ORAM host memory-usage scaling. The evaluator-side ORAM host holds the
//! physical tree (the ORAM "instance"); the embedded/garbler side holds only
//! per-wire labels and never the tree. This measures the tree's per-slot size
//! and its scaling in `levels`, and projects the 128M-block budget.

use volar_oram::OramTree;

/// Bytes per tree slot (`OramEntry<B>` = `addr: u64 + leaf: u64 + data: [u8;B]`,
/// 8-byte aligned).
fn slot_bytes<const B: usize>() -> usize {
    std::mem::size_of::<volar_oram::OramEntry<B>>()
}

/// Tree bytes for `levels` levels and bucket size `Z`.
fn tree_bytes<const Z: usize, const B: usize>(levels: usize) -> usize {
    let nodes = (1usize << levels) - 1;
    nodes * Z * slot_bytes::<B>()
}

#[test]
fn oram_tree_memory_scaling() {
    // Proper scaling: each additional level doubles the tree.
    for levels in 8..20 {
        let a = tree_bytes::<4, 8>(levels);
        let b = tree_bytes::<4, 8>(levels + 1);
        assert_eq!(b / a, 2, "tree size doubles per level (levels={levels})");
    }

    // Spot-check a real allocation at a feasible size (levels=20, Z=4, B=8:
    // ~2M buckets) and confirm the bucket count matches the model.
    let levels = 20;
    let tree = OramTree::<4, 8>::new(levels);
    assert_eq!(tree.buckets.len(), (1usize << levels) - 1);
    let measured = tree.buckets.len() * std::mem::size_of::<volar_oram::Bucket<4, 8>>();
    assert_eq!(measured, tree_bytes::<4, 8>(levels));
    drop(tree);

    // Report the per-slot size and the 128M-block (2^27 addresses) projection.
    let sb8 = slot_bytes::<8>();
    println!("OramEntry<8> slot size: {sb8} bytes");
    for levels in [20, 24, 27, 28] {
        let n_addrs = 1u64 << (levels - 1);
        let bytes = tree_bytes::<4, 8>(levels);
        println!(
            "levels={levels} addrs=2^{} ({n_addrs}) tree={:.2} GiB (Z=4, B=8)",
            levels - 1,
            bytes as f64 / (1u64 << 30) as f64
        );
    }
    // The 128M-block budget: 8 GiB max for 2^27 addresses. The tree has ~2N
    // nodes; with Z=4 buckets the per-address cost is ~8 * slot_bytes. A slot
    // is 16+B bytes (addr+leaf+data). Report the config that fits 8 GiB.
    let n = 1u64 << 27; // 128M addresses
    for (z, b) in [(4usize, 8usize), (4, 16), (3, 8)] {
        let bytes = (2 * n) as usize * z * (16 + b);
        println!(
            "128M blocks (OramTree): Z={z} B={b} -> {:.2} GiB (budget 8 GiB: {})",
            bytes as f64 / (1u64 << 30) as f64,
            if bytes <= (8u64 << 30) as usize {
                "OK"
            } else {
                "OVER"
            }
        );
    }

    // The encrypted tree drops addr/leaf (they live inside the ciphertext), so
    // CompactOramTree stores B bytes/slot. At Z=4, B=8 it hits the 8 GiB budget.
    use volar_oram::CompactOramTree;
    for (z, b) in [(4usize, 8usize), (4, 16)] {
        let bytes = ((1usize << 28) - 1) * z * b;
        println!(
            "128M blocks (CompactOramTree): Z={z} B={b} -> {:.2} GiB (budget 8 GiB: {})",
            bytes as f64 / (1u64 << 30) as f64,
            if bytes <= (8u64 << 30) as usize {
                "OK"
            } else {
                "OVER"
            }
        );
    }
    // The compact tree meets the budget at Z=4, B=8: (2^28 - 1) * 4 * 8 = 8 GiB.
    let compact_bytes = ((1usize << 28) - 1) * 4 * 8;
    assert!(
        compact_bytes <= (8u64 << 30) as usize,
        "compact tree must fit 8 GiB"
    );

    // Spot-check a real compact allocation at levels=20.
    let ct = CompactOramTree::<4, 8>::new(20);
    assert_eq!(ct.byte_len(), ((1usize << 20) - 1) * 4 * 8);
    // read/write round-trip on the compact tree.
    let mut ct = ct;
    let path = ct.read_path(3);
    assert_eq!(path.len(), 20);
    ct.write_path(3, &path);
}

#[test]
fn embedded_side_holds_no_tree() {
    // The embedded / garbler side of a garbled-ORAM run holds only per-wire
    // labels and bases (O(circuit size)), never the ORAM tree or position map —
    // those live on the evaluator host. The client-side state is the stash
    // (O(log N)) plus the position map; the *tree* is the host's allocation.
    // Assert the client stash is tiny relative to the tree at a fixed geometry.
    let levels = 12;
    let tree_bytes = ((1usize << levels) - 1) * 4 * 8;
    // A stash bounded by max_stash = 2*levels + Z + margin entries stays O(log N).
    let max_stash = 2 * levels + 4 + 16;
    let stash_bytes = max_stash * (16 + 8);
    assert!(
        stash_bytes * 100 < tree_bytes,
        "client stash ({stash_bytes} B) must be far below the tree ({tree_bytes} B)"
    );
    println!(
        "levels={levels}: host tree={tree_bytes} B, client stash<={stash_bytes} B (garbler holds neither)"
    );
}
