//! P4c: the minimal TLS 1.3 client session driver, driven end-to-end
//! against the RFC 8448 simple 1-RTT handshake as a scripted peer.
//!
//! The driver interleaves in-circuit crypto (transcript SHA-256, HKDF key
//! schedule, AES-128-GCM record open/seal) with native record framing and
//! transcript management — the exact interleaving the two-party session
//! driver runs with socket ActionCall host externs and threaded-secret
//! Feeds; here the socket is the RFC's scripted bytes and the secrets are
//! threaded as plain values between circuit invocations.
//!
//! The final stage is the Turnstile siteverify exchange the site server
//! performs: the POST (carrying the server-private Turnstile secret and
//! the client-private token — both private inputs in the MPC) sealed
//! under the client application keys, and the scripted JSON response
//! opened under the server application keys, with the verdict extracted
//! from the decrypted body.

use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir::boolar::BIrBlocks;
use volar_vc::sha_gadget::build_sha256;
use volar_vc::tls13::{expand_label_circuit, extract_circuit, nonce_xor, record_open_circuit, record_seal_circuit, transcript_circuit};

fn hex(s: &str) -> Vec<u8> {
    let s: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap())
        .collect()
}

fn bits_of(bytes: &[u8]) -> Vec<bool> {
    bytes
        .iter()
        .flat_map(|b| (0..8).map(move |i| (b >> i) & 1 == 1))
        .collect()
}

fn bytes_of(bits: &[bool]) -> Vec<u8> {
    bits.chunks(8)
        .map(|c| {
            c.iter()
                .enumerate()
                .fold(0u8, |a, (i, &b)| a | ((b as u8) << i))
        })
        .collect()
}

fn eval(c: &BIrBlocks<()>, inputs: &[bool]) -> Vec<u8> {
    bytes_of(&eval_biir(c, inputs).expect("circuit"))
}

fn hmac(key: &[u8], msg: &[u8]) -> Vec<u8> {
    let c = extract_circuit(32, msg.len());
    let mut inputs = bits_of(key);
    inputs.extend(bits_of(msg));
    eval(&c, &inputs)
}

fn expand_label(secret: &[u8], label: &[u8], context: &[u8], out_len: usize) -> Vec<u8> {
    let (c, prefix) = expand_label_circuit(out_len as u16, label, context.len());
    let mut msg = prefix;
    msg.extend_from_slice(context);
    msg.push(0x01);
    let mut inputs = bits_of(secret);
    inputs.extend(bits_of(&msg));
    let out = eval(&c, &inputs);
    out[..out_len].to_vec()
}

fn sha256(data: &[u8]) -> Vec<u8> {
    eval(&build_sha256(data.len()), &bits_of(data))
}

/// AEAD-open one TLS 1.3 record (header included in `record`); returns
/// (inner plaintext, tag_ok).
fn open_record(key: &[u8], iv: &[u8; 12], seq: u64, record: &[u8]) -> (Vec<u8>, bool) {
    let (aad, body) = record.split_at(5);
    let (ct, tag) = body.split_at(body.len() - 16);
    let c = record_open_circuit(ct.len());
    let nonce = nonce_xor(iv, seq);
    let mut inputs = bits_of(key);
    inputs.extend(bits_of(&nonce));
    inputs.extend(bits_of(aad));
    inputs.extend(bits_of(ct));
    let out = eval(&c, &inputs);
    let tag_ok = out[ct.len()..] == *tag;
    (out[..ct.len()].to_vec(), tag_ok)
}

/// AEAD-seal one TLS 1.3 inner plaintext into a complete record.
fn seal_record(key: &[u8], iv: &[u8; 12], seq: u64, inner: &[u8]) -> Vec<u8> {
    let c = record_seal_circuit(inner.len());
    let nonce = nonce_xor(iv, seq);
    let mut record = Vec::new();
    let body_len = inner.len() + 16;
    record.extend_from_slice(&[0x17, 0x03, 0x03, (body_len >> 8) as u8, body_len as u8]);
    let mut inputs = bits_of(key);
    inputs.extend(bits_of(&nonce));
    inputs.extend(bits_of(&record));
    inputs.extend(bits_of(inner));
    let out = eval(&c, &inputs);
    record.extend_from_slice(&out);
    record
}

// --- RFC 8448 simple 1-RTT handshake (section 3) -------------------------

const SHARED: &str = "8bd4054fb55b9d63fdfbacf9f04b9f0d35e6d63f537563efd46272900f89492d";

const CH: &str = "010000c00303cb34ecb1e78163ba1c38c6dacb196a6dffa21a8d9912ec18a2ef62\
    83024dece7000006130113031302010000910000000b0009000006736572766572ff0100010000\
    0a00140012001d0017001800190100010101020103010400230000003300260024001d00209938\
    1de560e4bd43d23d8e435a7dbafeb3c06e51c13cae4d5413691e529aaf2c002b0003020304000d\
    0020001e040305030603020308040805080604010501060102010402050206020202002d000201\
    01001c00024001";

/// The server's complete 679-octet encrypted handshake record
/// (EE + Certificate + CertificateVerify + Finished, inner type 0x16).
const SERVER_RECORD: &str = "17030302a2d1ff334a56f5bff6594a07cc87b580233f500f45e489e7f33af35e\
    df7869fcf40aa40aa2b8ea73f848a7ca07612ef9f945cb960b4068905123ea78\
    b111b429ba9191cd05d2a389280f526134aadc7fc78c4b729df828b5ecf7b13b\
    d9aefb0e57f271585b8ea9bb355c7c79020716cfb9b1183ef3ab20e37d57a6b9\
    d7477609aee6e122a4cf51427325250c7d0e509289444c9b3a648f1d71035d2e\
    d65b0e3cdd0cbae8bf2d0b227812cbb360987255cc744110c453baa4fcd61092\
    8d809810e4b7ed1a8fd991f06aa6248204797e36a6a73b70a2559c09ead68694\
    5ba246ab66e5edd8044b4c6de3fcf2a89441ac66272fd8fb330ef8190579b368\
    4596c960bd596eea520a56a8d650f563aad27409960dca63d3e688611ea5e22f\
    4415cf9538d51a200c27034272968a264ed6540c84838d89f72c24461aad6d26\
    f59ecaba9acbbb317b66d902f4f292a36ac1b639c637ce343117b65962224531\
    7b49eeda0c6258f100d7d961ffb138647e92ea330faeea6dfa31c7a84dc3bd7e\
    1b7a6c7178af36879018e3f252107f243d243dc7339d5684c8b0378bf30244da\
    8c87c843f5e56eb4c5e8280a2b48052cf93b16499a66db7cca71e4599426f7d4\
    61e66f99882bd89fc50800becca62d6c74116dbd2972fda1fa80f85df881edbe\
    5a37668936b335583b599186dc5c6918a396fa48a181d6b6fa4f9d62d513afbb\
    992f2b992f67f8afe67f76913fa388cb5630c8ca01e0c65d11c66a1e2ac4c859\
    77b7c7a6999bbf10dc35ae69f5515614636c0b9b68c19ed2e31c0b3b66763038\
    ebba42f3b38edc0399f3a9f23faa63978c317fc9fa66a73f60f0504de93b5b84\
    5e275592c12335ee340bbc4fddd502784016e4b3be7ef04dda49f4b440a30cb5\
    d2af939828fd4ae3794e44f94df5a631ede42c1719bfdabf0253fe5175be898e\
    750edc53370d2b";

/// The server's SH (payload of its plaintext record).
const SH: &str = "0200005603 03a6af06a4121860dc5e6e60249cd34c95930c8ac5cb1434dac155772ed3e26928\
    00130100002e00330024001d0020c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd\
    69b1b04e751f0f002b00020304";

/// The client's exact 58-octet Finished record.
const CLIENT_FIN_RECORD: &str = "170303003575ec4dc238cce60b298044a71e219c56cc77b0517fe9b93c7a4bfc\
    44d87f38f80338ac98fc46deb384bd1caeacab6867d726c40546";

/// The full session against the scripted RFC 8448 peer.
fn run_session(shared: &[u8; 32]) {
    // ---- handshake secrets (transcript + schedule, all in-circuit) ----
    let mut transcript = hex(CH);
    transcript.extend_from_slice(&hex(SH));
    let th1 = eval(&transcript_circuit(transcript.len()), &bits_of(&transcript));
    let empty_hash = sha256(&[]);
    let early = hmac(&[0u8; 32], &[0u8; 32]);
    let derived = expand_label(&early, b"derived", &empty_hash, 32);
    let hs = hmac(&derived, shared);
    let c_hs = expand_label(&hs, b"c hs traffic", &th1, 32);
    let s_hs = expand_label(&hs, b"s hs traffic", &th1, 32);
    let s_key: [u8; 16] = expand_label(&s_hs, b"key", &[], 16).try_into().unwrap();
    let s_iv: [u8; 12] = expand_label(&s_hs, b"iv", &[], 12).try_into().unwrap();
    assert_eq!(s_key, hex("3fce516009c21727d0f2e4e86ee403bc")[..], "s hs key");
    assert_eq!(s_iv, hex("5d313eb2671276ee13000b30")[..], "s hs iv");

    // ---- open the server's encrypted handshake flight (seq 0) ----
    let record = hex(SERVER_RECORD);
    let (inner, tag_ok) = open_record(&s_key, &s_iv, 0, &record);
    assert!(tag_ok, "server record tag");
    let (payload, ctype) = inner.split_at(inner.len() - 1);
    assert_eq!(ctype, &[0x16], "inner content type handshake");
    // The payload is EE || Certificate || CertificateVerify || Finished.
    assert_eq!(payload.len(), 657, "server flight length");
    transcript.extend_from_slice(payload);

    // ---- verify the server's Finished (over the transcript WITHOUT it) --
    let th5 = sha256(&transcript[..transcript.len() - 36]);
    let finished_key_s = expand_label(&s_hs, b"finished", &[], 32);
    assert_eq!(
        finished_key_s,
        hex("008d3b66f816ea559f96b537e885c31fc068bf492c652f01f288a1d8cdc19fc8"),
        "server finished_key"
    );
    let vd_s = hmac(&finished_key_s, &th5);
    assert_eq!(
        vd_s,
        hex("9b9b141d906337fbd2cbdce71df4deda4ab42c309572cb7fffee5454b78f0718"),
        "server verify_data"
    );
    assert_eq!(&payload[payload.len() - 32..], &vd_s[..], "Finished body matches");

    // ---- master + application secrets (transcript through server Fin) --
    let th6 = sha256(&transcript);
    assert_eq!(
        th6,
        hex("9608102a0f1ccc6db6250b7b7e417b1a000eaada3daae4777a7686c9ff83df13"),
        "transcript through server Finished"
    );
    let derived2 = expand_label(&hs, b"derived", &empty_hash, 32);
    let ms = hmac(&derived2, &[0u8; 32]);
    assert_eq!(
        ms,
        hex("18df06843d13a08bf2a449844c5f8a478001bc4d4c627984d5a41da8d0402919"),
        "master secret"
    );
    let c_ap = expand_label(&ms, b"c ap traffic", &th6, 32);
    assert_eq!(
        c_ap,
        hex("9e40646ce79a7f9dc05af8889bce6552875afa0b06df0087f792ebb7c17504a5"),
        "c ap traffic"
    );
    let s_ap = expand_label(&ms, b"s ap traffic", &th6, 32);
    assert_eq!(
        s_ap,
        hex("a11af9f05531f856ad47116b45a950328 204b4f44bfb6b3a4b4f1f3fcb631643"),
        "s ap traffic"
    );

    // ---- client Finished, byte-exact against the trace -----------------
    let c_key: [u8; 16] = expand_label(&c_hs, b"key", &[], 16).try_into().unwrap();
    let c_iv: [u8; 12] = expand_label(&c_hs, b"iv", &[], 12).try_into().unwrap();
    let finished_key_c = expand_label(&c_hs, b"finished", &[], 32);
    assert_eq!(
        finished_key_c,
        hex("b80ad01015fb2f0bd65ff7d4da5d6bf83f84821d1f87fdc7d3c75b5a7b42d9c4"),
        "client finished_key"
    );
    let vd_c = hmac(&finished_key_c, &th6);
    assert_eq!(
        vd_c,
        hex("a8ec436d677634ae525ac1fcebe11a039ec17694fac6e98527b642f2edd5ce61"),
        "client verify_data"
    );
    let mut fin_inner = Vec::new();
    fin_inner.extend_from_slice(&[0x14, 0x00, 0x00, 0x20]);
    fin_inner.extend_from_slice(&vd_c);
    fin_inner.push(0x16);
    let fin_record = seal_record(&c_key, &c_iv, 0, &fin_inner);
    assert_eq!(fin_record, hex(CLIENT_FIN_RECORD), "client Finished record byte-exact");

    // ---- application traffic keys --------------------------------------
    let ck_ap: [u8; 16] = expand_label(&c_ap, b"key", &[], 16).try_into().unwrap();
    let civ_ap: [u8; 12] = expand_label(&c_ap, b"iv", &[], 12).try_into().unwrap();
    assert_eq!(ck_ap, hex("17422dda596ed5d9acd890e3c63f5051")[..], "client app key");
    assert_eq!(civ_ap, hex("5b78923dee08579033e523d9")[..], "client app iv");
    let sk_ap: [u8; 16] = expand_label(&s_ap, b"key", &[], 16).try_into().unwrap();
    let siv_ap: [u8; 12] = expand_label(&s_ap, b"iv", &[], 12).try_into().unwrap();
    assert_eq!(sk_ap, hex("9f02283b6c9c07efc26bb9f2ac92e356")[..], "server app key");
    assert_eq!(siv_ap, hex("cf782b88dd83549aadf1e984")[..], "server app iv");

    // ---- Turnstile siteverify over the application channel -------------
    // The POST body carries the server-private secret and the
    // client-private token (both private inputs in the MPC driver).
    let post = b"{\"secret\":\"1x0000000000000000000000000000000AA\",\"response\":\"03AFcWeA4...\"}";
    let req_inner = {
        let mut v = Vec::new();
        v.extend_from_slice(post);
        v.push(0x17); // inner type application_data
        v
    };
    let req_record = seal_record(&ck_ap, &civ_ap, 0, &req_inner);
    let (req_back, req_ok) = open_record(&ck_ap, &civ_ap, 0, &req_record);
    assert!(req_ok && req_back == req_inner, "client app record round-trip");

    // The scripted siteverify response, sealed with the server app keys.
    let resp_json = b"{\"success\":true,\"challenge_ts\":\"2026-09-12T00:00:00Z\",\"hostname\":\"portalsolutions.com\"}";
    let resp_inner = {
        let mut v = Vec::new();
        v.extend_from_slice(resp_json);
        v.push(0x17);
        v
    };
    let resp_record = seal_record(&sk_ap, &siv_ap, 0, &resp_inner);
    let (resp_open, resp_ok) = open_record(&sk_ap, &siv_ap, 0, &resp_record);
    assert!(resp_ok, "siteverify response tag");
    let body = &resp_open[..resp_open.len() - 1];
    // Verdict extraction (native over the decrypted body in the concrete
    // driver; an in-circuit JSON compare with a verdict reveal in the MPC).
    let verdict = body
        .windows(b"\"success\":true".len())
        .any(|w| w == b"\"success\":true");
    assert!(verdict, "turnstile verdict from the decrypted response");
}

#[test]
fn tls13_session_driver_rfc8448() {
    run_session(&hex(SHARED).try_into().unwrap());
}
