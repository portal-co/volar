use volar_vc::oram_batch::{
    PublicPathOp, non_root_path_nodes, plan_disjoint_below_root_writes, plan_read_reuse,
};

#[test]
fn consecutive_same_leaf_reads_share_one_path_fetch() {
    let reads = plan_read_reuse(&[
        PublicPathOp {
            leaf: 3,
            write: false,
        },
        PublicPathOp {
            leaf: 3,
            write: false,
        },
        PublicPathOp {
            leaf: 5,
            write: false,
        },
        PublicPathOp {
            leaf: 5,
            write: true,
        },
        PublicPathOp {
            leaf: 5,
            write: false,
        },
    ]);
    assert_eq!(reads.len(), 3);
    assert_eq!(reads[0].leaf, 3);
    assert_eq!(reads[0].consumers, 2);
    assert_eq!(reads[1].leaf, 5);
    assert_eq!(reads[1].consumers, 1);
    assert_eq!(reads[2].leaf, 5);
}

#[test]
fn only_paths_with_no_common_non_root_bucket_are_groupable() {
    // Four-level tree: leaves 0 (000) and 4 (100) diverge immediately below
    // root, whereas 0 and 1 share their depth-one node.
    let grouped = plan_disjoint_below_root_writes(4, &[0, 4]).expect("disjoint below root");
    assert_eq!(grouped.root_commits, vec![0, 4]);
    assert_eq!(grouped.non_root_nodes.len(), 6);
    assert!(plan_disjoint_below_root_writes(4, &[0, 1]).is_none());
    assert_eq!(non_root_path_nodes(4, 0).expect("path").len(), 3);
}
