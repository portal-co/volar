#![cfg(feature = "std")]
//! P4: the two-party TLS 1.3 + Turnstile session over a strict chain
//! (volar-mpc strict_chain + volar-vc tls13_2pc), correlated with a
//! licensing predicate in the same session — the merged-protocol shape
//! (not standalone): the final revealed verdict ANDs the TLS record-tag
//! verdicts, the server Finished verdict, the Turnstile success verdict,
//! and a licensing predicate verdict, all threaded.

use hybrid_array::typenum::U16;
use sha2::Sha256;
use volar_fuzz::interpreter::biir::eval_biir;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_chain::{ChainEvaluator, ChainFeed, ChainGarbler, ChainOut, ChainParty};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::GlobalSecret;
use volar_vc::tls13::{record_open_circuit, record_seal_circuit};
use volar_vc::tls13_2pc::{
    TurnstileTlsScript, TurnstileTlsSecrets, bits_of, bytes_of, correlation_circuit,
    run_turnstile_tls_session,
};

type N = U16;
type D = Sha256;

fn hex(s: &str) -> Vec<u8> {
    let s: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap())
        .collect()
}

// RFC 8448 simple 1-RTT handshake (section 3) anchors.
const SHARED: &str = "8bd4054fb55b9d63fdfbacf9f04b9f0d35e6d63f537563efd46272900f89492d";
const CH: &str = "010000c00303cb34ecb1e78163ba1c38c6dacb196a6dffa21a8d9912ec18a2ef62\
    83024dece7000006130113031302010000910000000b0009000006736572766572ff0100010000\
    0a00140012001d0017001800190100010101020103010400230000003300260024001d00209938\
    1de560e4bd43d23d8e435a7dbafeb3c06e51c13cae4d5413691e529aaf2c002b0003020304000d\
    0020001e040305030603020308040805080604010501060102010402050206020202002d000201\
    01001c00024001";
const SH: &str = "0200005603 03a6af06a4121860dc5e6e60249cd34c95930c8ac5cb1434dac155772ed3e26928\
    00130100002e00330024001d0020c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd\
    69b1b04e751f0f002b00020304";
const SERVER_RECORD: &str = include_str!("tls13_2pc_server_record.txt");
const CLIENT_FIN_RECORD: &str = "170303003575ec4dc238cce60b298044a71e219c56cc77b0517fe9b93c7a4bfc\
    44d87f38f80338ac98fc46deb384bd1caeacab6867d726c40546";

// RFC 8448 application traffic keys (anchored in tests/tls_session.rs).
const CK_AP: &str = "17422dda596ed5d9acd890e3c63f5051";
const CIV_AP: &str = "5b78923dee08579033e523d9";
const SK_AP: &str = "9f02283b6c9c07efc26bb9f2ac92e356";
const SIV_AP: &str = "cf782b88dd83549aadf1e984";

/// The licensing predicate's held verdict slot (caller-side correlation).
const VL: usize = 31 << 16;

fn seal_native(key: &[u8; 16], iv: &[u8; 12], pt: &[u8]) -> Vec<u8> {
    let c = record_seal_circuit(pt.len());
    let aad = [
        0x17u8,
        0x03,
        0x03,
        ((pt.len() + 16) >> 8) as u8,
        (pt.len() + 16) as u8,
    ];
    let mut inputs = bits_of(key);
    inputs.extend(bits_of(iv));
    inputs.extend(bits_of(&aad));
    inputs.extend(bits_of(pt));
    let out = bytes_of(&eval_biir(&c, &inputs).expect("eval"));
    let mut record = aad.to_vec();
    record.extend_from_slice(&out);
    record
}

struct SessionOut {
    verdict: bool,
    cf_record: Vec<u8>,
    req_record: Vec<u8>,
}

fn run_two_party(flight_tamper: bool, response_success: bool) -> SessionOut {
    let turnstile_secret = [0x53u8; 16];
    let token = [0x74u8; 16];
    let script = TurnstileTlsScript {
        ch_len: 196,
        sh_len: 90,
        flight_len: 679,
        req_prefix: b"POST /siteverify HTTP/1.1\r\nHost: c\r\nContent-Length: 49\r\n\r\nsecret="
            .to_vec(),
        req_mid: b"&response=".to_vec(),
        req_suffix: Vec::new(),
        secret_len: 16,
        token_len: 16,
        response_len: 5 + 36 + 16,
        success_marker: b"\"success\":true".to_vec(),
    };

    // The scripted siteverify response, sealed under the server app keys.
    let resp_json: &[u8] = if response_success {
        b"HTTP/1.1 200 OK\r\n\r\n{\"success\":true}"
    } else {
        b"HTTP/1.1 200 OK\r\n\r\n{\"success\":fals}"
    };
    let mut resp_inner = resp_json.to_vec();
    resp_inner.push(0x17);
    let sk_ap: [u8; 16] = hex(SK_AP).try_into().unwrap();
    let siv_ap: [u8; 12] = hex(SIV_AP).try_into().unwrap();
    let response = seal_native(&sk_ap, &siv_ap, &resp_inner);
    assert_eq!(response.len(), script.response_len);

    let mut flight = hex(SERVER_RECORD);
    if flight_tamper {
        flight[100] ^= 1;
    }
    let shared: [u8; 32] = hex(SHARED).try_into().unwrap();
    let ch = hex(CH);
    let sh = hex(SH);

    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let addr = format!("{}", listener.local_addr().unwrap());
    let g_script = script.clone();
    let g_secret = turnstile_secret;

    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut chain = ChainGarbler::<N>::new(GlobalSecret::<N>::new(hybrid_array::Array::<u8, N>::from([0x77u8; 16])));
        let out = run_turnstile_tls_session::<N, D, _, _>(
            &mut chain,
            &g_script,
            &TurnstileTlsSecrets::Garbler {
                secret: g_secret.to_vec(),
            },
            &mut session,
            &mut ot,
        )
        .expect("garbler session");
        // Correlated licensing predicate: server policy bit AND the
        // client's claim bit.
        let lic = volar_vc::tls13_2pc::correlation_circuit(2);
        let lic_sched = volar_vc::compile_schedule(&lic).expect("license schedules");
        let _ = chain
            .run_round::<D, _>(
                &lic_sched,
                &[ChainFeed::Garbler, ChainFeed::Eval],
                &[],
                &[true], // server policy: license valid
                &[ChainOut::Hold(VL)],
                &mut session,
                &mut ot,
            )
            .expect("license round");
        // Final correlation: all verdicts ANDed, revealed.
        let fin = volar_vc::tls13_2pc::correlation_circuit(5);
        let fin_sched = volar_vc::compile_schedule(&fin).expect("correlation schedules");
        let verdict = chain
            .run_round::<D, _>(
                &fin_sched,
                &[
                    ChainFeed::Held(out.vt_flight),
                    ChainFeed::Held(out.vf_server),
                    ChainFeed::Held(out.vt_response),
                    ChainFeed::Held(out.vs_success),
                    ChainFeed::Held(VL),
                ],
                &[],
                &[],
                &[ChainOut::Reveal],
                &mut session,
                &mut ot,
            )
            .expect("correlation round");
        verdict[0]
    });

    let transport = TcpTransport::connect(&addr).expect("connect");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut chain = ChainEvaluator::<N>::new();
    let out = run_turnstile_tls_session::<N, D, _, _>(
        &mut chain,
        &script,
        &TurnstileTlsSecrets::Evaluator {
            ch,
            sh,
            flight,
            shared_secret: shared,
            token: token.to_vec(),
            response,
        },
        &mut session,
        &mut ot,
    )
    .expect("evaluator session");
    let lic = volar_vc::tls13_2pc::correlation_circuit(2);
    let lic_sched = volar_vc::compile_schedule(&lic).expect("license schedules");
    let _ = chain
        .run_round::<D, _>(
            &lic_sched,
            &[ChainFeed::Garbler, ChainFeed::Eval],
            &[],
            &[true], // client claim: holds a valid license
            &[ChainOut::Hold(VL)],
            &mut session,
            &mut ot,
        )
        .expect("license round");
    let fin = volar_vc::tls13_2pc::correlation_circuit(5);
    let fin_sched = volar_vc::compile_schedule(&fin).expect("correlation schedules");
    let eval_verdict = chain
        .run_round::<D, _>(
            &fin_sched,
            &[
                ChainFeed::Held(out.vt_flight),
                ChainFeed::Held(out.vf_server),
                ChainFeed::Held(out.vt_response),
                ChainFeed::Held(out.vs_success),
                ChainFeed::Held(VL),
            ],
            &[],
            &[],
            &[ChainOut::Reveal],
            &mut session,
            &mut ot,
        )
        .expect("correlation round");

    let garb_verdict = garbler.join().expect("garbler join");
    assert_eq!(garb_verdict, eval_verdict[0], "verdicts agree");

    SessionOut {
        verdict: eval_verdict[0],
        cf_record: bytes_of(&out.client_finished_record),
        req_record: bytes_of(&out.siteverify_request_record),
    }
}

#[test]
fn tls13_turnstile_two_party_correlated_honest() {
    let out = run_two_party(false, true);
    assert!(out.verdict, "honest session verdict");
    // cf_record is the sealed ciphertext || tag (the record body); the
    // RFC trace's record additionally carries the 5-byte header.
    assert_eq!(
        out.cf_record,
        hex(CLIENT_FIN_RECORD)[5..],
        "client Finished byte-exact"
    );
    // The revealed request record opens natively under the client app keys
    // and carries the secret + token.
    let ck_ap: [u8; 16] = hex(CK_AP).try_into().unwrap();
    let civ_ap: [u8; 12] = hex(CIV_AP).try_into().unwrap();
    // req_record is the record BODY (ct || tag); the header is the
    // deterministic TLS 1.3 application-data header for the body length.
    let body_ct_len = out.req_record.len() - 16;
    // The TLS record header's length covers ct + tag (the full body).
    let aad = [
        0x17u8,
        0x03,
        0x03,
        (out.req_record.len() >> 8) as u8,
        out.req_record.len() as u8,
    ];
    let c = record_open_circuit(body_ct_len);
    let mut inputs = bits_of(&ck_ap);
    inputs.extend(bits_of(&civ_ap));
    inputs.extend(bits_of(&aad));
    inputs.extend(bits_of(&out.req_record[..body_ct_len]));
    let opened = bytes_of(&eval_biir(&c, &inputs).expect("eval"));
    let tag_ok = opened[body_ct_len..] == out.req_record[body_ct_len..];
    assert!(tag_ok, "request record tag verifies");
    let body = &opened[..body_ct_len - 1];
    assert!(
        body.windows(16).any(|w| w == [0x53u8; 16]) && body.windows(16).any(|w| w == [0x74u8; 16]),
        "request carries the secret and token"
    );
}

#[test]
fn tls13_turnstile_two_party_tampered_flight_rejected() {
    let out = run_two_party(true, true);
    assert!(!out.verdict, "a tampered flight record must fail the verdict");
}

#[test]
fn tls13_turnstile_two_party_failed_turnstile_rejected() {
    let out = run_two_party(false, false);
    assert!(!out.verdict, "a success:false response must fail the verdict");
}
