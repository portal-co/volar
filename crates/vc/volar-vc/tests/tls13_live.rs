//! Live two-party TLS 1.3 session against a REAL rustls server.
//!
//! The evaluator (client) runs a real TCP handshake with a local rustls
//! server (TLS 1.3, TLS_AES_128_GCM_SHA256, rcgen self-signed cert) while
//! every secret-touching operation runs as strict-chain rounds with the
//! garbler (site server) over the loopback MPC transport: key schedule,
//! record crypto, tag/Finished verdicts, and the siteverify request seal
//! (garbler's secret + client's token inside). The final correlation round
//! reveals one AND-fold verdict.
//!
//! Assertions: the verdict holds, the decrypted flight parses to a real
//! Certificate/CertVerify/Finished flight, and the rustls server received
//! EXACTLY the request body with the secret and token inside.

#![cfg(feature = "std")]

use std::io::Read;
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_mpc::ot::SeedRng;
use volar_mpc::strict_chain::{ChainFeed, ChainOut, ChainParty};
use volar_mpc::tcp::{NetOtChannel, OtRole, TcpTransport};
use volar_spec::garble::GlobalSecret;
use volar_vc::tls13_2pc::{bits_of, correlation_circuit};
use volar_vc::tls13_live::{
    LiveRequestTemplate, LiveSecrets, NativeKx, NativeStream, run_live_tls_session,
    walk_handshake,
};

type N = U16;
type D = Sha256;

struct DalekKx {
    secret: x25519_dalek::StaticSecret,
}
impl NativeKx for DalekKx {
    fn public_key(&self) -> [u8; 32] {
        x25519_dalek::PublicKey::from(&self.secret).to_bytes()
    }
    fn shared_secret(&self, server_public: &[u8; 32]) -> [u8; 32] {
        self.secret
            .diffie_hellman(&x25519_dalek::PublicKey::from(*server_public))
            .to_bytes()
    }
}

const TURNSTILE_SECRET: [u8; 16] = [0x53u8; 16];
const TOKEN: [u8; 16] = [0x74u8; 16];
const RESPONSE_BODY: &[u8] = b"HTTP/1.1 200 OK\r\n\r\n{\"success\":true}";

fn template() -> LiveRequestTemplate {
    LiveRequestTemplate {
        prefix: b"POST /siteverify HTTP/1.1\r\nHost: c\r\nContent-Length: 49\r\n\r\nsecret="
            .to_vec(),
        secret_len: 16,
        mid: b"&response=".to_vec(),
        token_len: 16,
        suffix: Vec::new(),
        success_marker: b"\"success\":true".to_vec(),
    }
}

/// The exact plaintext HTTP request the server must receive.
fn expected_request() -> Vec<u8> {
    let t = template();
    let mut r = t.prefix.clone();
    r.extend_from_slice(&TURNSTILE_SECRET);
    r.extend_from_slice(&t.mid);
    r.extend_from_slice(&TOKEN);
    r.extend_from_slice(&t.suffix);
    r
}

fn make_cert() -> (Vec<rustls::pki_types::CertificateDer<'static>>, rustls::pki_types::PrivateKeyDer<'static>) {
    let key = rcgen::KeyPair::generate().expect("keygen");
    let params = rcgen::CertificateParams::new(vec![
        "localhost".to_string(),
        "challenges.cloudflare.com".to_string(),
    ])
    .expect("params");
    let cert = params.self_signed(&key).expect("self signed");
    (
        vec![cert.der().clone()],
        rustls::pki_types::PrivatePkcs8KeyDer::from(key.serialize_der()).into(),
    )
}

/// Run a real rustls TLS 1.3 server: accept, read the request, assert it
/// matches, write the scripted response.
fn serve(listener: TcpListener, received: Arc<Mutex<Vec<u8>>>) {
    let (certs, key) = make_cert();
    let mut config = rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .expect("server config");
    config.send_tls13_tickets = 0; // no NewSessionTicket app records
    let mut sock = listener.accept().expect("accept").0;
    sock.set_nonblocking(false).expect("blocking");
    let mut conn = rustls::ServerConnection::new(Arc::new(config)).expect("conn");
    let mut stream = rustls::Stream::new(&mut conn, &mut sock);
    let want = expected_request();
    let mut buf = Vec::new();
    while buf.len() < want.len() {
        let mut chunk = [0u8; 2048];
        let n = stream.read(&mut chunk).expect("server read");
        assert!(n > 0, "server read EOF before full request");
        buf.extend_from_slice(&chunk[..n]);
    }
    received.lock().unwrap().extend_from_slice(&buf);
    assert_eq!(
        &buf[..want.len()],
        &want[..],
        "rustls server must receive the exact sealed request"
    );
    use std::io::Write;
    stream.write_all(RESPONSE_BODY).expect("write response");
    stream.conn.send_close_notify();
    stream.flush().expect("flush");
}

/// Heavyweight: the full two-party crypto chain against a real TLS stack
/// (~400s debug / ~40s release). Run with:
/// `cargo test -p volar-vc --features std --test tls13_live -- --ignored`.
#[test]
#[ignore]
fn live_two_party_tls_against_real_rustls_server() {
    let tls_listener = TcpListener::bind("127.0.0.1:0").expect("bind tls");
    let tls_addr = tls_listener.local_addr().unwrap();
    let received = Arc::new(Mutex::new(Vec::new()));
    let received2 = Arc::clone(&received);
    let tls_server = std::thread::spawn(move || serve(tls_listener, received2));

    let mpc_listener = TcpListener::bind("127.0.0.1:0").expect("bind mpc");
    let mpc_addr = format!("{}", mpc_listener.local_addr().unwrap());

    let garbler = std::thread::spawn(move || {
        let transport = TcpTransport::accept(&mpc_listener).expect("accept");
        let mut session = transport.try_clone().expect("clone");
        let mut rng = SeedRng::new(0xA11CE);
        let mut ot = NetOtChannel::new(transport, OtRole::Sender, &mut rng);
        let mut chain = volar_mpc::strict_chain::ChainGarbler::<N>::new(
            GlobalSecret::<N>::new(Array::<u8, N>::from([0x77u8; 16])),
        );
        let tmpl = template();
        let out = run_live_tls_session::<N, D, _, _, NativeStream<TcpStream>>(
            &mut chain,
            "challenges.cloudflare.com",
            &tmpl,
            &LiveSecrets::Garbler {
                secret: &TURNSTILE_SECRET,
            },
            None,
            &[0u8; 32],
            None,
            &mut session,
            &mut ot,
        )
        .expect("garbler session");
        // Final correlation over the per-record tag verdicts + the three
        // held verdicts.
        let n = out.vt_records.len() + 3;
        let fin = correlation_circuit(n);
        let fin_sched = volar_vc::compile_schedule(&fin).expect("correlation schedules");
        let mut feeds: Vec<ChainFeed> = out
            .vt_records
            .iter()
            .map(|&s| ChainFeed::Held(s))
            .collect();
        feeds.push(ChainFeed::Held(out.vf_server));
        feeds.push(ChainFeed::Held(out.vt_response));
        feeds.push(ChainFeed::Held(out.vs_success));
        let verdict = chain
            .run_round::<D, _>(
                &fin_sched,
                &feeds,
                &[],
                &[],
                &[ChainOut::Reveal],
                &mut session,
                &mut ot,
            )
            .expect("correlation round");
        (verdict[0], out)
    });

    // Evaluator (client): real TCP to the rustls server + MPC transport.
    let tls_sock = TcpStream::connect(tls_addr).expect("connect tls");
    tls_sock.set_nonblocking(false).expect("blocking");
    let mut io = NativeStream { stream: tls_sock };
    let transport = TcpTransport::connect(&mpc_addr).expect("connect mpc");
    let mut session = transport.try_clone().expect("clone");
    let mut rng = SeedRng::new(0xB0B);
    let mut ot = NetOtChannel::new(transport, OtRole::Receiver, &mut rng);
    let mut chain = volar_mpc::strict_chain::ChainEvaluator::<N>::new();
    let kx = DalekKx {
        secret: x25519_dalek::StaticSecret::from([0x07u8; 32]),
    };
    let tmpl = template();
    let out = run_live_tls_session::<N, D, _, _, _>(
        &mut chain,
        "challenges.cloudflare.com",
        &tmpl,
        &LiveSecrets::Evaluator { token: &TOKEN },
        Some(&kx),
        &[0x42u8; 32],
        Some(&mut io),
        &mut session,
        &mut ot,
    )
    .expect("evaluator session");
    let n = out.vt_records.len() + 3;
    let fin = correlation_circuit(n);
    let fin_sched = volar_vc::compile_schedule(&fin).expect("correlation schedules");
    let mut feeds: Vec<ChainFeed> = out
        .vt_records
        .iter()
        .map(|&s| ChainFeed::Held(s))
        .collect();
    feeds.push(ChainFeed::Held(out.vf_server));
    feeds.push(ChainFeed::Held(out.vt_response));
    feeds.push(ChainFeed::Held(out.vs_success));
    let eval_verdict = chain
        .run_round::<D, _>(
            &fin_sched,
            &feeds,
            &[],
            &[],
            &[ChainOut::Reveal],
            &mut session,
            &mut ot,
        )
        .expect("correlation round");

    let (garb_verdict, garb_out) = garbler.join().expect("garbler join");
    tls_server.join().expect("tls server join");

    assert!(garb_verdict, "all verdicts must hold (garbler)");
    assert!(eval_verdict[0], "all verdicts must hold (evaluator)");

    // The flight really was a Certificate + CertVerify + Finished flight.
    let msgs = walk_handshake(&out.flight_stream);
    let types: Vec<u8> = msgs.iter().map(|m| m.hs_type).collect();
    assert!(
        types.contains(&11) && types.contains(&15) && types.contains(&20),
        "expected Certificate+CertVerify+Finished, got {types:?}"
    );
    // The response plaintext matches.
    assert_eq!(out.response_inner, garb_out.response_inner);
    assert!(out
        .response_inner
        .windows(b"\"success\":true".len())
        .any(|w| w == b"\"success\":true"));
    // The server received exactly the request (asserted in-serve too).
    assert_eq!(
        *received.lock().unwrap(),
        expected_request(),
        "server saw the exact request body with secret + token"
    );
}
