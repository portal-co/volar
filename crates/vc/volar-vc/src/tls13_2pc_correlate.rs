//! Shared correlated Turnstile + licensing session script (`std` only).
//!
//! This is the consumer-facing composition built from
//! [`crate::tls13_2pc`]'s per-round chain driver: the two-party TLS client
//! session yields four held verdict wires, a licensing circuit yields a
//! fifth, and one final AND-fold reveals only the correlated verdict. The
//! TLS record ciphertexts remain the only TLS values revealed.
//!
//! Keeping the composition here, rather than in each downstream repository,
//! makes the slot layout and round ordering part of the reusable protocol
//! surface. The caller supplies its concrete TLS script, secrets, licensing
//! circuit, and a blocking [`Transport`] pair (session clone plus OT clone).

extern crate std;

use alloc::vec::Vec;

use hybrid_array::typenum::U16;
use sha2::Sha256;
use volar_ir::boolar::BIrBlocks;
use volar_mpc::strict_chain::{ChainFeed, ChainOut, ChainParty};
use volar_mpc::tcp::{NetOtChannel, OtRole};
use volar_mpc::{MpcError, Transport};
use volar_spec::SpecRng;
use volar_spec::garble::GlobalSecret;

use crate::tls13_2pc::{
    TurnstileTlsOutcome, TurnstileTlsScript, TurnstileTlsSecrets, correlation_circuit,
    run_turnstile_tls_session,
};

/// The garbled-label width used by the shipped correlated sessions.
pub type N = U16;
/// The digest used by the shipped correlated sessions.
pub type D = Sha256;
/// The held slot for the licensing predicate verdict.
pub const LICENSE_SLOT: usize = 32 << 16;

/// The reveal-only outcome of a correlated session.
pub struct CorrelatedSessionOutcome {
    /// The client Finished record (revealed ciphertext + tag).
    pub client_finished_record: Vec<bool>,
    /// The siteverify request record (revealed ciphertext + tag).
    pub siteverify_request_record: Vec<bool>,
    /// The single correlated verdict: all four TLS verdict wires AND the
    /// licensing predicate wire.
    pub verdict: bool,
}

fn correlated_verdict<C, T>(
    chain: &mut C,
    tls: &TurnstileTlsOutcome,
    transport: &mut T,
    ot: &mut dyn volar_mpc::OtChannel<N>,
) -> Result<bool, MpcError>
where
    C: ChainParty<N>,
    T: Transport,
{
    let sched = crate::compile_schedule(&correlation_circuit(5)).expect("correlation schedules");
    let bits = chain.run_round::<D, T>(
        &sched,
        &[
            ChainFeed::Held(tls.vt_flight),
            ChainFeed::Held(tls.vf_server),
            ChainFeed::Held(tls.vt_response),
            ChainFeed::Held(tls.vs_success),
            ChainFeed::Held(LICENSE_SLOT),
        ],
        &[],
        &[],
        &[ChainOut::Reveal],
        transport,
        ot,
    )?;
    Ok(bits[0])
}

/// Run the garbler (site server) half of the correlated session.
///
/// `license_circuit` must have two inputs in `[garbler, evaluator]` order
/// and one boolean output. `session` and `ot_session` must be two handles
/// to the same ordered transport.
pub fn run_correlated_garbler<T>(
    tls_script: &TurnstileTlsScript,
    tls_secrets: &TurnstileTlsSecrets,
    license_circuit: &BIrBlocks,
    license_server_bit: bool,
    session: &mut T,
    ot_session: T,
    rng: &mut dyn SpecRng,
    secret: GlobalSecret<N>,
) -> Result<CorrelatedSessionOutcome, MpcError>
where
    T: Transport,
{
    let mut ot = NetOtChannel::new(ot_session, OtRole::Sender, rng);
    let mut chain = volar_mpc::strict_chain::ChainGarbler::<N>::new(secret);
    let tls = run_turnstile_tls_session::<N, D, _, _>(
        &mut chain,
        tls_script,
        tls_secrets,
        session,
        &mut ot,
    )?;

    let sched = crate::compile_schedule(license_circuit).expect("license circuit schedules");
    let _ = chain.run_round::<D, T>(
        &sched,
        &[ChainFeed::Garbler, ChainFeed::Eval],
        &[],
        &[license_server_bit],
        &[ChainOut::Hold(LICENSE_SLOT)],
        session,
        &mut ot,
    )?;

    let verdict = correlated_verdict(&mut chain, &tls, session, &mut ot)?;
    Ok(CorrelatedSessionOutcome {
        client_finished_record: tls.client_finished_record,
        siteverify_request_record: tls.siteverify_request_record,
        verdict,
    })
}

/// Run the evaluator (client) half of the correlated session.
pub fn run_correlated_evaluator<T>(
    tls_script: &TurnstileTlsScript,
    tls_secrets: &TurnstileTlsSecrets,
    license_circuit: &BIrBlocks,
    license_claim_bit: bool,
    session: &mut T,
    ot_session: T,
    rng: &mut dyn SpecRng,
) -> Result<CorrelatedSessionOutcome, MpcError>
where
    T: Transport,
{
    let mut ot = NetOtChannel::new(ot_session, OtRole::Receiver, rng);
    let mut chain = volar_mpc::strict_chain::ChainEvaluator::<N>::new();
    let tls = run_turnstile_tls_session::<N, D, _, _>(
        &mut chain,
        tls_script,
        tls_secrets,
        session,
        &mut ot,
    )?;

    let sched = crate::compile_schedule(license_circuit).expect("license circuit schedules");
    let _ = chain.run_round::<D, T>(
        &sched,
        &[ChainFeed::Garbler, ChainFeed::Eval],
        &[],
        &[license_claim_bit],
        &[ChainOut::Hold(LICENSE_SLOT)],
        session,
        &mut ot,
    )?;

    let verdict = correlated_verdict(&mut chain, &tls, session, &mut ot)?;
    Ok(CorrelatedSessionOutcome {
        client_finished_record: tls.client_finished_record,
        siteverify_request_record: tls.siteverify_request_record,
        verdict,
    })
}
