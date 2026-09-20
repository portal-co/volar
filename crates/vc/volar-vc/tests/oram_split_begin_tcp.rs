//! The migrated ORAM begin step reveals only the physical old leaf and keeps
//! the position map in split opaque state.

use std::thread;

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::compile_schedule;
use volar_vc::oram_gadget::{OramGadgetConfig, build_begin};
use volar_vc::oram_split::{
    EvaluatorOramState, GarblerOramState, SplitOramEvaluator, SplitOramGarbler,
};

type N = U16;
type D = Sha256;

fn cfg() -> OramGadgetConfig {
    OramGadgetConfig {
        num_addrs: 4,
        levels: 3,
        bucket_size: 2,
        data_bits: 1,
        max_stash: 8,
        encrypted: false,
        tree_key_bits: 0,
        versioned_pads: false,
        version_bits: 0,
        encrypt_valid: false,
        keyed_leaf: false,
    }
}

#[test]
fn split_begin_reveals_old_leaf_and_threads_posmap_over_tcp() {
    let cfg = cfg();
    let schedule = compile_schedule(&build_begin(&cfg)).expect("compile begin");
    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[0x42; 16]));
    let posmap_bases: Vec<Garble<N>> = (0..cfg.num_addrs * cfg.leaf_bits())
        .map(|i| Garble {
            base: Array::clone_from_slice(&[i as u8 + 1; 16]),
        })
        .collect();
    let posmap_labels = posmap_bases
        .iter()
        .map(|base| secret.encode(base, false))
        .collect::<Vec<_>>();
    let tape_bases = vec![
        Garble {
            base: Array::clone_from_slice(&[0x61; 16]),
        },
        Garble {
            base: Array::clone_from_slice(&[0x62; 16]),
        },
    ];
    let tape_labels = vec![
        secret.encode(&tape_bases[0], true),
        secret.encode(&tape_bases[1], false),
    ];
    let new_leaf_bases = vec![
        Garble {
            base: Array::clone_from_slice(&[0x71; 16]),
        },
        Garble {
            base: Array::clone_from_slice(&[0x72; 16]),
        },
    ];
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let address = listener.local_addr().expect("address");
    let gschedule = schedule.clone();
    let gcfg = cfg.clone();
    let gposmap = posmap_bases.clone();
    let gtape = tape_bases.clone();
    let gnew = new_leaf_bases.clone();
    let garbler = thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xC1_11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut driver =
            SplitOramGarbler::new(secret, GarblerOramState::new(gtape, gposmap, vec![]));
        let old = driver
            .run_begin::<D>(
                &gcfg,
                &gschedule,
                &[0, 1],
                gnew,
                &[true, false],
                &mut session,
                &mut ot,
            )
            .expect("garbler begin");
        (old, driver.state().posmap().to_vec())
    });

    let transport = TcpTransport::connect(&address.to_string()).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xC2_11CE);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut driver =
        SplitOramEvaluator::new(EvaluatorOramState::new(tape_labels, posmap_labels, vec![]));
    let old = driver
        .run_begin::<D>(&cfg, &schedule, &[0, 1], &mut session, &mut ot)
        .expect("evaluator begin");
    let (garbler_old, bases) = garbler.join().expect("garbler join");
    assert_eq!(old, 0);
    assert_eq!(garbler_old, old);
    assert_eq!(driver.state().posmap().len(), bases.len());
    // Address 1 received new_leaf=1; the rest of the initial posmap remains 0.
    let posmap = driver.state().posmap();
    assert!(posmap[2].open(&bases[2])[0] & 1 != 0);
    assert!(!(posmap[3].open(&bases[3])[0] & 1 != 0));
}
