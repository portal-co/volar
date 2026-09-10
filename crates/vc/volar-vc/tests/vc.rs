// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! End-to-end vc embedder tests: a lowered guest circuit (`BIrBlocks`) is
//! compiled to a `GateSchedule`, partitioned by argument visibility, and run
//! through the two-party session — asserting the vc-spec outcome (concrete
//! result revealed) matches concrete evaluation for every visibility
//! combination.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_mpc::InputOwner;
use volar_mpc::ot::LoopbackOt;
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::{VcEmbedder, VcOutcome, partition_from_sides};
use volar_side::SideId;

type N = U16;
type D = Sha256;

fn det_bytes(seed: u8) -> Array<u8, N> {
    Array::<u8, N>::from_fn(|i| seed.wrapping_mul(31).wrapping_add(i as u8))
}
fn det_label(seed: u8) -> Garble<N> {
    Garble { base: det_bytes(seed) }
}

/// `(x0 ^ x1) & x2` — a 3-input, 1-AND circuit: wires 0,1,2 inputs;
/// wire 3 = Xor(0,1); wire 4 = And(3,2); Return wire 4. In `BIrBlocks`,
/// stmt `i` (0-indexed) produces `IRVarId(params + i)`, so the Xor result is
/// `IRVarId(3)` and the And reads it plus input `IRVarId(2)` (x2).
fn xor_and_circuit() -> BIrBlocks {
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: 3,
            stmts: vec![
                Node::new(BIrStmt::Xor(IRVarId(0), IRVarId(1)), (), None),
                Node::new(BIrStmt::And(IRVarId(3), IRVarId(2)), (), None),
            ],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(4)],
            }),
        }],
        pre_init: vec![],
    }
}

fn concrete(inputs: &[bool; 3]) -> bool {
    (inputs[0] ^ inputs[1]) & inputs[2]
}

fn embedder() -> VcEmbedder<N, 3, 1> {
    let secret = GlobalSecret::<N>::new(det_bytes(13));
    let labels = [det_label(7), det_label(91), det_label(33)];
    VcEmbedder::with_secret(secret, labels)
}

/// The schedule compiler produces the right gate structure.
#[test]
fn compile_xor_and_schedule() {
    let schedule = VcEmbedder::<N, 3, 1>::compile(&xor_and_circuit()).expect("compiles");
    assert_eq!(schedule.num_inputs, 3);
    assert_eq!(schedule.and_count(), 1);
    assert_eq!(schedule.output, 4);
    // Xor then And → two gates.
    assert_eq!(schedule.gates.len(), 2);
}

/// `Or` is expanded by De Morgan into Not/And/Not.
#[test]
fn compile_or_expands() {
    use volar_mpc::Gate;
    let circuit = BIrBlocks {
        blocks: vec![BIrBlock {
            params: 2,
            stmts: vec![Node::new(BIrStmt::Or(IRVarId(0), IRVarId(1)), (), None)],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            }),
        }],
        pre_init: vec![],
    };
    let schedule = VcEmbedder::<N, 2, 1>::compile(&circuit).expect("compiles");
    // Or(a,b) → Not, Not, And, Not: 4 gates, exactly 1 AND.
    assert_eq!(schedule.gates.len(), 4);
    assert_eq!(schedule.and_count(), 1);
    assert!(matches!(schedule.gates[2], Gate::And(_, _)));
}

/// Full vc invoke: every visibility assignment of the 3 inputs runs through
/// the embedder and matches concrete evaluation, across all 8 input combos.
#[test]
fn vc_invoke_all_visibilities() {
    let circuit = xor_and_circuit();
    let owners = [
        volar_vc::VcVisibility::Public,
        volar_vc::VcVisibility::Private,
        volar_vc::VcVisibility::Blind,
    ];
    // Intern side ids 0/1/2 = public/local/remote, mirroring the lowering.
    let (public, local, remote) = (SideId(0), SideId(1), SideId(2));

    for combo in 0u32..27 {
        // Assign each input a visibility.
        let mut vis = [volar_vc::VcVisibility::Public; 3];
        let mut c = combo;
        for v in vis.iter_mut() {
            *v = owners[(c % 3) as usize];
            c /= 3;
        }
        // Map visibility → per-bit side, then to InputOwner.
        let side_of = |i: usize| match vis[i] {
            volar_vc::VcVisibility::Public => Some(public),
            volar_vc::VcVisibility::Private => Some(local),
            volar_vc::VcVisibility::Blind => Some(remote),
        };
        let partition = partition_from_sides(3, public, local, remote, side_of);

        for inputs in 0u32..8 {
            let b = [
                (inputs >> 0) & 1 == 1,
                (inputs >> 1) & 1 == 1,
                (inputs >> 2) & 1 == 1,
            ];
            // Split inputs into the three visibility vectors.
            let mut public_b = Vec::new();
            let mut private_b = Vec::new();
            let mut blind_b = Vec::new();
            for (i, &bit) in b.iter().enumerate() {
                match vis[i] {
                    volar_vc::VcVisibility::Public => public_b.push(bit),
                    volar_vc::VcVisibility::Private => private_b.push(bit),
                    volar_vc::VcVisibility::Blind => blind_b.push(bit),
                }
            }
            let mut ot = LoopbackOt::<N>::new();
            let out = embedder().invoke::<D, _>(
                &circuit,
                &partition,
                &public_b,
                &private_b,
                &blind_b,
                &mut ot,
            );
            match out {
                VcOutcome::Value(bits) => {
                    assert_eq!(bits.len(), 1);
                    assert_eq!(bits[0], concrete(&b), "vis {vis:?} inputs {b:?}");
                }
                other => panic!("expected Value, got {other:?} for vis {vis:?} inputs {b:?}"),
            }
        }
    }
}

/// Mutual privacy: a blind (remote) input bit must cross only via OT — the
/// embedder never sees it in cleartext. Structural assertion via the
/// partition: blind ⇒ `InputOwner::Evaluator`, which the session delivers by
/// OT, never as a garbler-sent label.
#[test]
fn vc_blind_input_is_ot_delivered() {
    let (public, local, remote) = (SideId(0), SideId(1), SideId(2));
    let partition = partition_from_sides(3, public, local, remote, |i| match i {
        0 => Some(public),
        1 => Some(local),
        2 => Some(remote),
        _ => None,
    });
    assert_eq!(
        partition,
        [
            InputOwner::Public,
            InputOwner::Garbler,
            InputOwner::Evaluator
        ]
    );
}

/// A non-circuit (multi-block) input is rejected as an embedder error, not a
/// panic — vc-spec `Error`, not a crash.
#[test]
fn vc_rejects_non_circuit() {
    // Two blocks → not a fused single-block circuit.
    let bad = BIrBlocks {
        blocks: vec![
            BIrBlock::<()> {
                params: 1,
                stmts: vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Block(volar_ir::ir::IRBlockId(1)),
                    args: vec![IRVarId(0)],
                }),
            },
            BIrBlock::<()> {
                params: 1,
                stmts: vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(0)],
                }),
            },
        ],
        pre_init: vec![],
    };
    let mut ot = LoopbackOt::<N>::new();
    let out = embedder().invoke::<D, _>(
        &bad,
        &[InputOwner::Public],
        &[true],
        &[],
        &[],
        &mut ot,
    );
    assert!(matches!(out, VcOutcome::Error(_)), "got {out:?}");
}

// ---------------------------------------------------------------------------
// Workstream A: Garbled RAM baseline — storage circuit via the MUX floor,
// compiled to a schedule and run through the two-party MPC session.
// ---------------------------------------------------------------------------

use volar_ir::boolar::LaneId;
use volar_ir_common::StorageId;
use volar_ir_passes::{StorageToMuxBoolarConfig, storage_to_mux_boolar};

/// Build a storage circuit with parametric (private) inputs, lower storage to
/// a MUX/demux register file, and return the resulting pure-boolean circuit.
///
/// Layout (params = wires 0..1): `data_in` (bit 0) and `addr` (bit 1) are
/// circuit inputs. The circuit writes `data_in` to cell `addr`, then reads
/// cell 0 and cell 1, returning `read0 ^ read1`. With 2 cells this is the
/// smallest memory that exercises both an oblivious write (demux) and an
/// oblivious read (MUX) through the MPC session.
fn storage_circuit_mux() -> BIrBlocks {
    use volar_ir::boolar::BIrPreInitSegment;
    let storage = StorageId(0);
    let lane = LaneId(0);
    let mut block: BIrBlock<()> = BIrBlock {
        params: 2, // 0 = data_in, 1 = addr
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = 2u32;
    let mut push = |s: BIrStmt, b: &mut BIrBlock<()>| {
        b.stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let data_in = IRVarId(0);
    let addr = IRVarId(1);
    // Write data_in to cell addr.
    push(
        BIrStmt::StorageWrite {
            storage,
            lane,
            src: data_in,
            addr: vec![addr],
        },
        &mut block,
    );
    // Read cell 0 (addr bit = Zero) and cell 1 (addr bit = One).
    let zero = push(BIrStmt::Zero, &mut block);
    let one = push(BIrStmt::One, &mut block);
    let read0 = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: vec![zero],
        },
        &mut block,
    );
    let read1 = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: vec![one],
        },
        &mut block,
    );
    let out = push(BIrStmt::Xor(read0, read1), &mut block);
    block.terminator = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        args: vec![out],
    });
    let raw: BIrBlocks = BIrBlocks {
        blocks: vec![block],
        pre_init: vec![],
    };
    // Lower storage to a 2-cell MUX register file (the A3 linear-scan floor).
    let cfg = StorageToMuxBoolarConfig {
        storage,
        lane,
        num_cells: 2,
    };
    let lowered: BIrBlocks =
        storage_to_mux_boolar(&raw, &cfg).expect("MUX lowering should succeed");
    assert!(lowered.is_circuit());
    lowered
}

/// Concrete reference: write `d` to cell `a` (cells start 0), then read both.
/// cell0' = (a==0)? d : 0 ; cell1' = (a==1)? d : 0. Output = cell0' ^ cell1'.
fn storage_concrete(d: bool, a: bool) -> bool {
    let cell0 = if !a { d } else { false };
    let cell1 = if a { d } else { false };
    cell0 ^ cell1
}

/// Workstream A baseline: a storage circuit lowered through the MUX floor and
/// run through the two-party MPC session recovers the concrete result for
/// every input, with `data_in` held as the evaluator's (blind) private bit.
#[test]
fn gram_mux_baseline_through_mpc() {
    let circuit = storage_circuit_mux();
    // After MUX lowering the circuit is pure boolean gates; compile it.
    let and_count = circuit
        .blocks[0]
        .stmts
        .iter()
        .filter(|n| matches!(n.kind, volar_ir::boolar::BIrStmt::And(..)))
        .count();
    let num_inputs = circuit.blocks[0].params as usize;
    assert_eq!(num_inputs, 2);

    // Both inputs private to the evaluator (blind): the whole memory access
    // pattern is driven by the remote party's data.
    let partition = [InputOwner::Evaluator, InputOwner::Evaluator];

    for d in [false, true] {
        for a in [false, true] {
            let inputs = [d, a];
            let mut ot = LoopbackOt::<N>::new();
            let want = storage_concrete(d, a);
            // Build the embedder with the right const-generic AND count.
            let out = run_gram(&circuit, and_count, &partition, &inputs, &mut ot);
            match out {
                VcOutcome::Value(bits) => assert_eq!(bits[0], want, "d={d} a={a}"),
                other => panic!("expected Value, got {other:?}"),
            }
        }
    }
}

/// Helper: garble + evaluate the MUX-lowered storage circuit. AND count is
/// circuit-dependent; this test's circuit has a fixed shape so we monomorphize
/// on the observed count.
fn run_gram(
    circuit: &BIrBlocks,
    and_count: usize,
    partition: &[InputOwner],
    inputs: &[bool; 2],
    ot: &mut LoopbackOt<N>,
) -> VcOutcome {
    match and_count {
        6 => run_gram_typed::<6>(circuit, partition, inputs, ot),
        other => panic!("unexpected AND count {other} — update the test monomorphization"),
    }
}

fn run_gram_typed<const A: usize>(
    circuit: &BIrBlocks,
    partition: &[InputOwner],
    inputs: &[bool; 2],
    ot: &mut LoopbackOt<N>,
) -> VcOutcome {
    let secret = GlobalSecret::<N>::new(det_bytes(29));
    let labels = [det_label(11), det_label(53)];
    let embedder: VcEmbedder<N, 2, A> = VcEmbedder::with_secret(secret, labels);
    embedder.invoke::<D, _>(circuit, partition, &[], &[], inputs, ot)
}

// ---------------------------------------------------------------------------
// Multi-output schedules: a circuit returning several bits reveals each one
// correctly, decoded against its own output false-label base.
// ---------------------------------------------------------------------------

/// `(x0 & x1, x0 ^ x1, !x2)` — a 3-input, 3-output circuit. Wires 0,1,2 are
/// inputs; wire 3 = And(0,1); wire 4 = Xor(0,1); wire 5 = Not(2); the Return
/// terminator carries all three result wires, in order.
fn three_output_circuit() -> BIrBlocks {
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: 3,
            stmts: vec![
                Node::new(BIrStmt::And(IRVarId(0), IRVarId(1)), (), None),
                Node::new(BIrStmt::Xor(IRVarId(0), IRVarId(1)), (), None),
                Node::new(BIrStmt::Not(IRVarId(2)), (), None),
            ],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(3), IRVarId(4), IRVarId(5)],
            }),
        }],
        pre_init: vec![],
    }
}

fn three_output_concrete(inputs: &[bool; 3]) -> [bool; 3] {
    [
        inputs[0] & inputs[1],
        inputs[0] ^ inputs[1],
        !inputs[2],
    ]
}

/// The schedule compiler surfaces all three output wires.
#[test]
fn compile_multi_output_schedule() {
    let schedule = VcEmbedder::<N, 3, 1>::compile(&three_output_circuit()).expect("compiles");
    assert_eq!(schedule.num_inputs, 3);
    assert_eq!(schedule.and_count(), 1);
    assert_eq!(schedule.output_wires(), vec![3, 4, 5]);
}

/// A multi-output invoke reveals every output bit correctly across all inputs
/// and visibility assignments — each decoded against its own output base.
#[test]
fn vc_invoke_multi_output_all_visibilities() {
    let circuit = three_output_circuit();
    let owners = [
        volar_vc::VcVisibility::Public,
        volar_vc::VcVisibility::Private,
        volar_vc::VcVisibility::Blind,
    ];
    let (public, local, remote) = (SideId(0), SideId(1), SideId(2));

    for combo in 0u32..27 {
        let mut vis = [volar_vc::VcVisibility::Public; 3];
        let mut c = combo;
        for v in vis.iter_mut() {
            *v = owners[(c % 3) as usize];
            c /= 3;
        }
        let side_of = |i: usize| match vis[i] {
            volar_vc::VcVisibility::Public => Some(public),
            volar_vc::VcVisibility::Private => Some(local),
            volar_vc::VcVisibility::Blind => Some(remote),
        };
        let partition = partition_from_sides(3, public, local, remote, side_of);

        for inputs in 0u32..8 {
            let b = [
                (inputs >> 0) & 1 == 1,
                (inputs >> 1) & 1 == 1,
                (inputs >> 2) & 1 == 1,
            ];
            let mut public_b = Vec::new();
            let mut private_b = Vec::new();
            let mut blind_b = Vec::new();
            for (i, &bit) in b.iter().enumerate() {
                match vis[i] {
                    volar_vc::VcVisibility::Public => public_b.push(bit),
                    volar_vc::VcVisibility::Private => private_b.push(bit),
                    volar_vc::VcVisibility::Blind => blind_b.push(bit),
                }
            }
            let secret = GlobalSecret::<N>::new(det_bytes(13));
            let labels = [det_label(7), det_label(91), det_label(33)];
            let embedder: VcEmbedder<N, 3, 1> = VcEmbedder::with_secret(secret, labels);
            let mut ot = LoopbackOt::<N>::new();
            let out = embedder.invoke::<D, _>(
                &circuit,
                &partition,
                &public_b,
                &private_b,
                &blind_b,
                &mut ot,
            );
            match out {
                VcOutcome::Value(bits) => {
                    assert_eq!(bits.len(), 3);
                    assert_eq!(bits.as_slice(), &three_output_concrete(&b), "vis {vis:?} inputs {b:?}");
                }
                other => panic!("expected Value, got {other:?} for vis {vis:?} inputs {b:?}"),
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Workstream G1: GRAM storage circuits through the two-party session.
// A storage circuit (concrete cells) is scheduled to GRAM storage gates
// (not the linear-scan MUX floor) and driven through an ORAM host on the
// evaluator side, with the garbler pinning bases. Sub-linear in the memory.
// ---------------------------------------------------------------------------

use volar_oram::OramTree;
use volar_vc::GramEvalDrive;

/// A GRAM storage circuit: write `data_in` to cell `0` (concrete address),
/// then read cells 0 and 1 and XOR them. Addresses are constant wires, so
/// the schedule resolves concrete cells. This is `storage_circuit_mux`'s
/// logic but left in storage form (no MUX lowering) so it schedules to GRAM.
fn storage_circuit_gram() -> BIrBlocks {
    let storage = StorageId(0);
    let lane = LaneId(0);
    let mut block: BIrBlock<()> = BIrBlock {
        params: 1, // 0 = data_in
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = 1u32;
    let mut push = |s: BIrStmt, b: &mut BIrBlock<()>| {
        b.stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let data_in = IRVarId(0);
    // Write data_in to cell 0 (constant address bit Zero).
    let zero = push(BIrStmt::Zero, &mut block);
    let one = push(BIrStmt::One, &mut block);
    push(
        BIrStmt::StorageWrite {
            storage,
            lane,
            src: data_in,
            addr: vec![zero],
        },
        &mut block,
    );
    // Read cell 0 and cell 1 (constant addresses), XOR them.
    let read0 = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: vec![zero],
        },
        &mut block,
    );
    let read1 = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: vec![one],
        },
        &mut block,
    );
    let out = push(BIrStmt::Xor(read0, read1), &mut block);
    block.terminator = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        args: vec![out],
    });
    BIrBlocks {
        blocks: vec![block],
        pre_init: vec![],
    }
}

/// Concrete reference for `storage_circuit_gram`: cell 0 gets data_in, cell 1
/// stays 0, so the result is `data_in ^ false = data_in`.
fn gram_concrete(data_in: bool) -> bool {
    data_in
}

/// The schedule compiler resolves concrete storage cells to GRAM gates.
#[test]
fn gram_storage_schedule_has_concrete_cells() {
    let schedule = VcEmbedder::<N, 1, 0>::compile(&storage_circuit_gram()).expect("compiles");
    assert_eq!(schedule.num_inputs, 1);
    assert_eq!(schedule.and_count(), 0, "no AND gates (only XOR + storage)");
    assert_eq!(schedule.storages.len(), 1, "one storage space");
    // 2 cells touched (0 and 1); num_cells = 2, levels sized to cover it.
    assert_eq!(schedule.storages[0].num_cells, 2);
    let n_storage = schedule
        .gates
        .iter()
        .filter(|g| matches!(g, volar_mpc::Gate::StorageRead { .. } | volar_mpc::Gate::StorageWrite { .. }))
        .count();
    assert_eq!(n_storage, 3, "one write + two reads");
}

/// The G1 capstone: a GRAM storage circuit runs two-party — the evaluator
/// drives an ORAM host, the garbler pins bases — and the result matches
/// concrete evaluation. Sub-linear (ORAM), unlike the MUX floor.
#[test]
fn gram_storage_circuit_two_party() {
    const Z: usize = 4;
    const B: usize = 8;
    let circuit = storage_circuit_gram();
    let schedule = VcEmbedder::<N, 1, 0>::compile(&circuit).expect("compiles");
    let spec = schedule.storages[0];
    let partition = [InputOwner::Evaluator]; // data_in is the remote party's

    for d in [false, true] {
        let inputs = [d];
        let mut ot = LoopbackOt::<N>::new();
        // Build the ORAM driver for the single storage space.
        let mut tree = OramTree::<Z, B>::new(spec.levels);
        let drive_secret = GlobalSecret::<N>::new(det_bytes(29));
        let mut drive: GramEvalDrive<D, N, Z, B> = GramEvalDrive::new(
            &drive_secret,
            &mut tree,
            spec.levels,
            spec.num_cells,
            0x5EED,
        );
        let labels = [det_label(11)];
        let embedder: VcEmbedder<N, 1, 0> = VcEmbedder::with_secret(drive_secret.clone(), labels);
        let mut gram: [&mut dyn volar_mpc::GramDrive<N>; 1] = [&mut drive];
        let out = embedder.invoke_schedule_with_gram::<D>(
            &schedule,
            &partition,
            &[],
            &[],
            &inputs,
            &mut ot,
            &mut gram,
        );
        match out {
            VcOutcome::Value(bits) => assert_eq!(bits[0], gram_concrete(d), "d={d}"),
            other => panic!("expected Value, got {other:?} for d={d}"),
        }
    }
}
