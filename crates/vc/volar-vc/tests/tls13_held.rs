//! Held-only routing for the TLS circuit imports: no input or output is a
//! garbler/evaluator/reveal feed, so traffic keys cannot cross the host seam.

use volar_mpc::strict_chain::{ChainFeed, ChainOut, HeldSlots};
use volar_vc::tls13_held::{Tls13OracleSchedules, Tls13OracleStorage};

#[test]
fn tls_import_storage_has_only_jointly_secret_routes() {
    let mut slots = HeldSlots::new();
    let storage = Tls13OracleStorage::reserve(&mut slots);

    for call in [
        storage.sha256_64(),
        storage.hmac_sha256_32_32(),
        storage.x25519_step(),
    ] {
        assert!(
            call.feeds()
                .iter()
                .all(|feed| matches!(feed, ChainFeed::Held(_))),
            "TLS import arguments must remain held"
        );
        assert!(
            call.holds()
                .iter()
                .all(|out| matches!(out, ChainOut::Hold(_))),
            "TLS import results must remain held"
        );
    }

    assert_eq!(storage.sha256_64().input_bits(), 512);
    assert_eq!(storage.sha256_64().output_bits(), 256);
    assert_eq!(storage.hmac_sha256_32_32().input_bits(), 512);
    assert_eq!(storage.hmac_sha256_32_32().output_bits(), 256);
    assert_eq!(storage.x25519_step().input_bits(), 1277);
    assert_eq!(storage.x25519_step().output_bits(), 1021);
    assert_eq!(slots.len(), 3834, "all regions have stable, distinct slots");
}

#[test]
fn held_routing_matches_each_realized_schedule_abi() {
    let mut slots = HeldSlots::new();
    let storage = Tls13OracleStorage::reserve(&mut slots);
    let schedules = Tls13OracleSchedules::new();

    for (call, schedule) in [
        (storage.sha256_64(), &schedules.sha256_64),
        (storage.hmac_sha256_32_32(), &schedules.hmac_sha256_32_32),
        (storage.x25519_step(), &schedules.x25519_step),
    ] {
        assert_eq!(call.input_bits(), schedule.num_inputs);
        assert_eq!(call.output_bits(), schedule.output_wires().len());
        assert!(
            schedule.actions.is_empty(),
            "TLS import is not a host action"
        );
    }
}
