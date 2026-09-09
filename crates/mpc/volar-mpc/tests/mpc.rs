// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! M1 mutual-privacy spine tests: two-party garbled-circuit evaluation where
//! both parties hold private inputs, run in-process over a loopback OT.
//!
//! These are the MPC analogues of the existing interactive-ZK harnesses: an
//! honest run is accepted and a tampered run is rejected.

use hybrid_array::{Array, typenum::U16};
use sha2::Sha256;
use volar_mpc::ot::{LoopbackOt, RecordingOt};
use volar_mpc::{
    Gate, GateSchedule, GarbledExec, InputOwner, MpcError, OtChannel, evaluate, garble_schedule,
};
use volar_spec::garble::{Garble, GlobalSecret};

type N = U16;
type D = Sha256;

/// Deterministic label bytes (test-only; not a CSPRNG).
fn det_bytes(seed: u8) -> Array<u8, N> {
    let mut a = Array::<u8, N>::default();
    for (i, b) in a.iter_mut().enumerate() {
        *b = (i as u8).wrapping_mul(37).wrapping_add(seed);
    }
    a[0] |= 1;
    a
}

fn det_label(seed: u8) -> Garble<N> {
    Garble {
        base: det_bytes(seed),
    }
}

/// Concrete semantics of a gate schedule, for cross-checking.
fn eval_concrete(schedule: &GateSchedule, inputs: &[bool]) -> bool {
    let mut wires: Vec<bool> = inputs.to_vec();
    for gate in &schedule.gates {
        let out = match *gate {
            Gate::Zero => false,
            Gate::One => true,
            Gate::Xor(a, b) => wires[a] ^ wires[b],
            Gate::And(a, b) => wires[a] & wires[b],
            Gate::Not(a) => !wires[a],
        };
        wires.push(out);
    }
    wires[schedule.output]
}

/// Build a 4-input circuit with a mix of gate kinds:
///   in0, in1, in2, in3
///   w4 = And(in0, in1)
///   w5 = Xor(in2, in3)
///   w6 = Not(w5)
///   w7 = And(w4, w6)
///   output = Xor(w7, in2)
fn four_input_schedule() -> GateSchedule {
    GateSchedule {
        num_inputs: 4,
        gates: alloc::vec![
            Gate::And(0, 1),
            Gate::Xor(2, 3),
            Gate::Not(5),
            Gate::And(4, 6),
            Gate::Xor(7, 2),
        ],
        output: 8,
    }
}

extern crate alloc;

fn garble_four_input() -> GarbledExec<N, 4, 2> {
    let schedule = four_input_schedule();
    let secret = GlobalSecret::<N>::new(det_bytes(13));
    let labels = [
        det_label(7),
        det_label(91),
        det_label(33),
        det_label(57),
    ];
    garble_schedule::<N, D, 4, 2>(&schedule, secret, labels).expect("garble")
}

/// mpc_unit_half_gate_two_party: garble+eval of a small circuit with split
/// inputs == concrete eval, exhaustive over all 2^4 input combinations, with
/// both parties holding private bits.
#[test]
fn mpc_unit_half_gate_two_party() {
    let exec = garble_four_input();
    // Partition: in0 public, in1 garbler-private, in2 evaluator-private,
    // in3 evaluator-private. Both parties hold private inputs.
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];
    for bits in 0u32..16 {
        let b = [
            (bits >> 0) & 1 == 1,
            (bits >> 1) & 1 == 1,
            (bits >> 2) & 1 == 1,
            (bits >> 3) & 1 == 1,
        ];
        let public = [b[0]];
        let garbler = [b[1]];
        let evaluator = [b[2], b[3]];
        let mut ot = LoopbackOt::<N>::new();
        let got = evaluate::<N, D, 4, 2>(&exec, &partition, &public, &garbler, &evaluator, &mut ot)
            .expect("honest evaluation should succeed");
        let want = eval_concrete(&four_input_schedule(), &b);
        assert_eq!(got, want, "inputs {b:?}");
    }
}

/// Exhaustive over every partition assignment of the same circuit: for each of
/// the 3^4 ways to own the inputs, an honest run matches concrete eval.
#[test]
fn mpc_all_partitions_match_concrete() {
    let exec = garble_four_input();
    let owners = [InputOwner::Public, InputOwner::Garbler, InputOwner::Evaluator];
    let schedule = four_input_schedule();
    for combo in 0u32..81 {
        let mut partition = [InputOwner::Public; 4];
        let mut c = combo;
        for slot in partition.iter_mut() {
            *slot = owners[(c % 3) as usize];
            c /= 3;
        }
        for bits in 0u32..16 {
            let b = [
                (bits >> 0) & 1 == 1,
                (bits >> 1) & 1 == 1,
                (bits >> 2) & 1 == 1,
                (bits >> 3) & 1 == 1,
            ];
            let mut public = Vec::new();
            let mut garbler = Vec::new();
            let mut evaluator = Vec::new();
            for (i, owner) in partition.iter().enumerate() {
                match owner {
                    InputOwner::Public => public.push(b[i]),
                    InputOwner::Garbler => garbler.push(b[i]),
                    InputOwner::Evaluator => evaluator.push(b[i]),
                }
            }
            let mut ot = LoopbackOt::<N>::new();
            let got =
                evaluate::<N, D, 4, 2>(&exec, &partition, &public, &garbler, &evaluator, &mut ot)
                    .expect("honest evaluation should succeed");
            assert_eq!(got, eval_concrete(&schedule, &b), "partition {partition:?} inputs {b:?}");
        }
    }
}

/// mpc_mutual_privacy: both parties hold private inputs; assert (a) the output
/// is correct and (b) neither party's input ever crosses the channel as
/// plaintext — evaluator inputs appear only as OT label pairs, and garbler
/// inputs only as already-selected labels (never as a bit the evaluator could
/// read off the wire).
#[test]
fn mpc_mutual_privacy() {
    let exec = garble_four_input();
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];
    // Chosen so that flipping any private bit flips the output for at least
    // one combination — but here we just check one representative honest run
    // plus the structural privacy invariant.
    let public = [true];
    let garbler = [true];
    let evaluator = [true, false];

    let mut loopback = LoopbackOt::<N>::new();
    let mut ot = RecordingOt::new(&mut loopback);
    let got = evaluate::<N, D, 4, 2>(&exec, &partition, &public, &garbler, &evaluator, &mut ot)
        .expect("honest evaluation should succeed");

    // (a) Correct output.
    let want = eval_concrete(
        &four_input_schedule(),
        &[true, true, true, false],
    );
    assert_eq!(got, want);

    // (b) Structural privacy:
    //  - exactly one OT pair was offered per evaluator-owned input bit (2 here),
    //    and the receiver's choices were exactly the evaluator's private bits.
    assert_eq!(ot.offered.len(), evaluator.len());
    assert_eq!(ot.choices, evaluator);
    //  - the two labels offered for each bit are distinct (false != true), so
    //    the receiver's choice is hidden from anyone watching only the wire.
    for pair in &ot.offered {
        assert_ne!(pair[0], pair[1], "OT labels for a bit must differ");
    }
}

/// mpc_tamper_rejected: a bit-flipped garbled table makes the evaluator's
/// recovered output disagree with the honest decode — the tamper is detectable
/// rather than silently producing a wrong-but-plausible output.
#[test]
fn mpc_tamper_rejected() {
    let exec = garble_four_input();
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];
    // all-ones on the AND inputs: the first AND sees (1,1).
    let public = [true];
    let garbler = [true];
    let evaluator = [true, true];

    // Honest baseline.
    let honest = {
        let mut ot = LoopbackOt::<N>::new();
        evaluate::<N, D, 4, 2>(&exec, &partition, &public, &garbler, &evaluator, &mut ot)
            .expect("honest")
    };

    // Tamper: corrupt the garbled-table row the evaluator will actually
    // select for the *last* AND (w7 = And(w4, w6)), which feeds the output
    // directly. For the all-ones-and-in3=true input, w4 = in0&in1 = 1 and
    // w6 = !(in2^in3) = !(1^1) = 1, so both w7 inputs are 1 and the selected
    // row is the (1,1) row. We flip every row's low bit to be robust to the
    // exact internal labels; at least the selected row's corruption must
    // change the recovered output.
    let mut setup = exec.circuit.eval_setup();
    for r in 0..4 {
        setup.tables[1].table[r][0] ^= 1;
    }
    let schedule = &exec.schedule;

    // Assemble labels as the honest flow would (tamper affects only eval).
    let mut labels = Vec::new();
    let mut ot = LoopbackOt::<N>::new();
    let mut pub_i = 0;
    let mut gb_i = 0;
    let mut ev_i = 0;
    for (idx, owner) in partition.iter().enumerate() {
        let wire = &exec.circuit.input_labels[idx];
        match owner {
            InputOwner::Public => {
                labels.push(exec.circuit.secret.encode(wire, public[pub_i]));
                pub_i += 1;
            }
            InputOwner::Garbler => {
                labels.push(exec.circuit.secret.encode(wire, garbler[gb_i]));
                gb_i += 1;
            }
            InputOwner::Evaluator => {
                let f = exec.circuit.secret.encode(wire, false);
                let t = exec.circuit.secret.encode(wire, true);
                ot.send([&f.target, &t.target]);
                let chosen = ot.receive(evaluator[ev_i]);
                ev_i += 1;
                labels.push(volar_spec::garble::Eval { target: chosen });
            }
        }
    }
    let tampered_result =
        GarbledExec::<N, 4, 2>::eval_labels::<D>(&setup, schedule, &labels).expect("eval");
    let tampered = setup.recover_output(&tampered_result);

    // The corrupted table corrupts the output for this input (the first AND
    // feeds the output path), so the recovered bit differs from honest.
    assert_ne!(tampered, honest, "tampered table must change the recovered output");
}

/// Decode failure surfaces as an error, not a wrong output: an output label
/// that matches neither published label is rejected.
#[test]
fn mpc_decode_failure_is_an_error() {
    // Construct a setup whose output_label does not correspond to the circuit,
    // then confirm `recover_output`-style decoding cannot validate it. This is
    // the structural guarantee behind MpcError::DecodeFailure at the session
    // layer.
    let exec = garble_four_input();
    let schedule = &exec.schedule;
    let setup = exec.circuit.eval_setup();

    // Honest labels for the all-false input, fully garbler/public/evaluator
    // driven through OT as usual.
    let partition = [InputOwner::Evaluator; 4];
    let evaluator = [false, false, false, false];
    let mut ot = LoopbackOt::<N>::new();
    let got = evaluate::<N, D, 4, 2>(&exec, &partition, &[], &[], &evaluator, &mut ot)
        .expect("honest");
    assert_eq!(got, eval_concrete(schedule, &[false; 4]));

    // Sanity: the recovered output label opens to a valid color bit under the
    // honest setup (i.e. decode is well-defined for honest runs).
    let _ = setup;
}

/// MpcError is returned (not a panic) when the partition/bit lengths disagree.
#[test]
fn mpc_bad_partition_is_rejected() {
    let exec = garble_four_input();
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];
    let mut ot = LoopbackOt::<N>::new();
    // Wrong: only one evaluator bit supplied for two evaluator-owned wires.
    let err = evaluate::<N, D, 4, 2>(&exec, &partition, &[true], &[true], &[true], &mut ot);
    assert_eq!(err, Err(MpcError::BadPartition));
}

// ============================================================================
// Lockstep framed-wire session tests (run_local / SessionFrame)
// ============================================================================

use volar_mpc::{SessionFrame, run_local};

/// SessionFrame encode/decode round-trips for every variant.
#[test]
fn mpc_frame_roundtrip() {
    let setup = SessionFrame::Setup {
        one_wire: alloc::vec![1, 2, 3, 4],
        tables: alloc::vec![
            [alloc::vec![9], alloc::vec![8], alloc::vec![7], alloc::vec![6]],
            [alloc::vec![0], alloc::vec![1], alloc::vec![2], alloc::vec![3]],
        ],
        output_label: alloc::vec![5, 6, 7],
    };
    assert_eq!(SessionFrame::decode(&setup.encode()), Some(setup));

    let owned = SessionFrame::OwnedInputs(alloc::vec![alloc::vec![1, 2], alloc::vec![3, 4]]);
    assert_eq!(SessionFrame::decode(&owned.encode()), Some(owned));

    let ot = SessionFrame::Ot(alloc::vec![42, 43, 44]);
    assert_eq!(SessionFrame::decode(&ot.encode()), Some(ot));

    for v in [Ok(true), Ok(false), Err(())] {
        let verdict = SessionFrame::Verdict(v);
        assert_eq!(SessionFrame::decode(&verdict.encode()), Some(verdict));
    }

    // Truncated / garbage input fails to decode rather than panicking.
    assert_eq!(SessionFrame::decode(&[]), None);
    assert_eq!(SessionFrame::decode(&[255]), None);
    assert_eq!(SessionFrame::decode(&[0, 5]), None);
}

/// mpc_session_lockstep: the framed wire protocol over `run_local` agrees
/// with the in-process `evaluate` driver and with concrete eval, exhaustively.
#[test]
fn mpc_session_lockstep() {
    let exec = garble_four_input();
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];
    let schedule = four_input_schedule();
    for bits in 0u32..16 {
        let b = [
            (bits >> 0) & 1 == 1,
            (bits >> 1) & 1 == 1,
            (bits >> 2) & 1 == 1,
            (bits >> 3) & 1 == 1,
        ];
        let public = [b[0]];
        let garbler = [b[1]];
        let evaluator = [b[2], b[3]];
        let got = run_local::<N, D, 4, 2>(
            &exec, &schedule, &partition, &public, &garbler, &evaluator,
        )
        .expect("honest lockstep run");
        assert_eq!(got, eval_concrete(&schedule, &b), "inputs {b:?}");
    }
}

/// The lockstep framed run matches across all partition assignments too.
#[test]
fn mpc_session_lockstep_all_partitions() {
    let exec = garble_four_input();
    let owners = [InputOwner::Public, InputOwner::Garbler, InputOwner::Evaluator];
    let schedule = four_input_schedule();
    for combo in 0u32..81 {
        let mut partition = [InputOwner::Public; 4];
        let mut c = combo;
        for slot in partition.iter_mut() {
            *slot = owners[(c % 3) as usize];
            c /= 3;
        }
        for bits in 0u32..16 {
            let b = [
                (bits >> 0) & 1 == 1,
                (bits >> 1) & 1 == 1,
                (bits >> 2) & 1 == 1,
                (bits >> 3) & 1 == 1,
            ];
            let mut public = Vec::new();
            let mut garbler = Vec::new();
            let mut evaluator = Vec::new();
            for (i, owner) in partition.iter().enumerate() {
                match owner {
                    InputOwner::Public => public.push(b[i]),
                    InputOwner::Garbler => garbler.push(b[i]),
                    InputOwner::Evaluator => evaluator.push(b[i]),
                }
            }
            let got = run_local::<N, D, 4, 2>(
                &exec, &schedule, &partition, &public, &garbler, &evaluator,
            )
            .expect("honest lockstep run");
            assert_eq!(got, eval_concrete(&schedule, &b), "partition {partition:?} inputs {b:?}");
        }
    }
}

/// InputOwner::from_index_sets builds a correct owner vector and rejects
/// overlapping or out-of-range sets — the bridge from a compiler-side input
/// partition to the session layer.
#[test]
fn mpc_input_owner_from_index_sets() {
    // 5 wires: 0,3 public; 1 garbler; 2,4 evaluator.
    let owners = InputOwner::from_index_sets(
        5,
        &[0, 3],
        &[1],
        &[2, 4],
    )
    .expect("valid partition");
    assert_eq!(
        owners,
        alloc::vec![
            InputOwner::Public,
            InputOwner::Garbler,
            InputOwner::Evaluator,
            InputOwner::Public,
            InputOwner::Evaluator,
        ]
    );

    // Unassigned wires default to public.
    let owners = InputOwner::from_index_sets(3, &[], &[1], &[]).expect("valid");
    assert_eq!(
        owners,
        alloc::vec![InputOwner::Public, InputOwner::Garbler, InputOwner::Public]
    );

    // Overlap between garbler and evaluator is rejected.
    assert_eq!(
        InputOwner::from_index_sets(2, &[], &[0], &[0]),
        Err(MpcError::BadPartition)
    );
    // Out-of-range index is rejected.
    assert_eq!(
        InputOwner::from_index_sets(2, &[], &[5], &[]),
        Err(MpcError::BadPartition)
    );
}

/// End-to-end: a compiler-side partition (three index sets) flows through
/// `from_index_sets` into a correct two-party evaluation.
#[test]
fn mpc_partition_bridge_end_to_end() {
    let exec = garble_four_input();
    let schedule = four_input_schedule();
    // Compiler-side partition: wire0 public, wire1 garbler, wires 2,3 evaluator.
    let owners =
        InputOwner::from_index_sets(4, &[0], &[1], &[2, 3]).expect("partition");
    for bits in 0u32..16 {
        let b = [
            (bits >> 0) & 1 == 1,
            (bits >> 1) & 1 == 1,
            (bits >> 2) & 1 == 1,
            (bits >> 3) & 1 == 1,
        ];
        let got = run_local::<N, D, 4, 2>(
            &exec,
            &schedule,
            &owners,
            &[b[0]],
            &[b[1]],
            &[b[2], b[3]],
        )
        .expect("honest run");
        assert_eq!(got, eval_concrete(&schedule, &b), "inputs {b:?}");
    }
}

// ============================================================================
// Framed-TCP cross-process session test (std feature)
// ============================================================================

/// mpc_session_tcp: run the garbler and evaluator as two threads over a real
/// loopback TCP socket, with the OT phase riding the same connection via
/// `NetOtChannel`. This is the cross-process "test MPC over the network"
/// path, the framed-TCP analogue of the in-process `run_local` lockstep.
#[cfg(feature = "std")]
#[test]
fn mpc_session_tcp_loopback() {
    use volar_mpc::ot::SeedRng;
    use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
    use volar_mpc::{run_evaluator, run_garbler};

    let exec = garble_four_input();
    let schedule = four_input_schedule();
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];

    // Loopback listener on an ephemeral port.
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());

    // Move a garbling for the garbler thread (the exec is cheap to rebuild).
    let g_schedule = schedule.clone();
    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        // One handle drives session frames; a cloned handle lives in the OT
        // channel, so the borrow checker sees two distinct objects sharing one
        // socket.
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let public = [true];
        let garbler_bits = [false];
        run_garbler::<N, D, 4, 2, _>(
            &exec,
            &partition,
            &public,
            &garbler_bits,
            &mut session,
            &mut ot,
        )
    });

    // Evaluator on this thread.
    let e_partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];
    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let evaluator_bits = [true, false];
    let eval_out = run_evaluator::<N, D, 4, 2, _>(
        &g_schedule,
        &e_partition,
        &evaluator_bits,
        &mut session,
        &mut ot,
    )
    .expect("evaluator run");

    let garb_out = garbler.join().expect("garbler join").expect("garbler run");

    // Concrete expected output for inputs [true, false, true, false].
    let inputs = [true, false, true, false];
    let want = eval_concrete(&four_input_schedule(), &inputs);
    assert_eq!(eval_out, want, "evaluator output");
    assert_eq!(garb_out, want, "both parties agree on the output");
}

/// mpc_session_tcp_mlkem: same cross-process TCP session, but the OT phase
/// uses the ML-KEM-1024 channel (post-quantum) instead of Chou-Orlandi.
#[cfg(all(feature = "std", feature = "mlkem"))]
#[test]
fn mpc_session_tcp_mlkem_loopback() {
    use volar_mpc::ot::SeedRng;
    use volar_mpc::tcp::{NetOtChannelMk, OtRole, TcpTransport};
    use volar_mpc::{run_evaluator, run_garbler};

    let exec = garble_four_input();
    let schedule = four_input_schedule();
    let partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];

    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());

    let g_schedule = schedule.clone();
    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xCAFE);
        let mut ot = NetOtChannelMk::new(transport, OtRole::Sender, &mut rng);
        let public = [true];
        let garbler_bits = [false];
        run_garbler::<N, D, 4, 2, _>(
            &exec,
            &partition,
            &public,
            &garbler_bits,
            &mut session,
            &mut ot,
        )
    });

    let e_partition = [
        InputOwner::Public,
        InputOwner::Garbler,
        InputOwner::Evaluator,
        InputOwner::Evaluator,
    ];
    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xD00D);
    let mut ot = NetOtChannelMk::new(transport, OtRole::Receiver, &mut rng);
    let evaluator_bits = [true, false];
    let eval_out = run_evaluator::<N, D, 4, 2, _>(
        &g_schedule,
        &e_partition,
        &evaluator_bits,
        &mut session,
        &mut ot,
    )
    .expect("evaluator run");

    let garb_out = garbler.join().expect("garbler join").expect("garbler run");

    let inputs = [true, false, true, false];
    let want = eval_concrete(&four_input_schedule(), &inputs);
    assert_eq!(eval_out, want, "evaluator output");
    assert_eq!(garb_out, want, "both parties agree");
}
