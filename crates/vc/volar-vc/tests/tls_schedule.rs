//! P4c-ii: the TLS 1.3 handshake key-schedule composition, anchored to the
//! RFC 8448 simple 1-RTT handshake trace.
//!
//! Every stage runs in-circuit: the transcript SHA-256 over the
//! ClientHello/ServerHello messages, HKDF-Extract for the early/handshake
//! secrets, and HKDF-Expand-Label for the "derived" secret and the
//! client/server handshake traffic secrets plus the client write key/iv.
//! The default test feeds the RFC 8448 shared secret as an input; the
//! `#[ignore]`d heavyweight test derives it in-circuit with the stepped
//! X25519 ladder first (the full end-to-end path).

use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir::boolar::BIrBlocks;
use volar_vc::sha_gadget::build_sha256;
use volar_vc::tls13::{expand_label_circuit, extract_circuit, transcript_circuit};
use volar_vc::x25519_gadget::scalar_ref::{Fp, fp_from_bytes, fp_to_bytes};
use volar_vc::x25519_gadget::{build_fe_invert, build_fe_mul, build_x25519_step};

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

/// HMAC(key, msg) in-circuit (32-byte key).
fn hmac(key: &[u8], msg: &[u8]) -> Vec<u8> {
    let c = extract_circuit(32, msg.len());
    let mut inputs = bits_of(key);
    inputs.extend(bits_of(msg));
    eval(&c, &inputs)
}

/// HKDF-Expand-Label(secret, label, context) in-circuit; returns out_len
/// bytes (the context may itself be in-circuit bits — here threaded as
/// values between circuit invocations).
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

// --- RFC 8448 simple 1-RTT handshake (section 3) -------------------------

const CLIENT_PRIV: &str = "49af42ba7f7994852d713ef2784bcbcaa7911de26adc5642cb634540e7ea5005";
const SERVER_PUB: &str = "c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd69b1b04e751f0f";
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

/// The full key schedule given the shared secret, asserting every RFC 8448
/// intermediate. Returns nothing; panics on any mismatch.
fn key_schedule_rfc8448(shared: &[u8; 32]) {
    // Transcript over CH||SH (hello messages are plaintext on the wire —
    // public inputs in deployment).
    let tr_c = transcript_circuit(286);
    let mut tr_in = bits_of(&hex(CH));
    tr_in.extend(bits_of(&hex(SH)));
    let transcript = eval(&tr_c, &tr_in);
    assert_eq!(
        transcript,
        hex("860c06edc07858ee8e78f0e7428c58edd6b43f2ca3e6e95f02ed063cf0e1cad8"),
        "transcript CH..SH"
    );

    // SHA-256("") in-circuit — the "derived" context.
    let empty = eval(&build_sha256(0), &[]);
    assert_eq!(
        empty,
        hex("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
        "sha256 empty"
    );

    // Early Secret = HKDF-Extract(0, 0).
    let early = hmac(&[0u8; 32], &[0u8; 32]);
    assert_eq!(
        early,
        hex("33ad0a1c607ec03b09e6cd9893680ce210adf300aa1f2660e1b22e10f170f92a"),
        "early secret"
    );

    // Derive-Secret(Early, "derived", "") = Expand-Label(early, "derived", Hash("")).
    let derived = expand_label(&early, b"derived", &empty, 32);
    assert_eq!(
        derived,
        hex("6f2615a108c702c5678f54fc9dbab69716c076189c48250cebeac3576c3611ba"),
        "tls13 derived"
    );

    // Handshake Secret = HKDF-Extract(derived, shared).
    let hs = hmac(&derived, shared);
    assert_eq!(
        hs,
        hex("1dc826e93606aa6fdc0aadc12f741b01046aa6b99f691ed221a9f0ca043fbeac"),
        "handshake secret"
    );

    // Traffic secrets (context = transcript CH..SH, threaded from above).
    let c_hs = expand_label(&hs, b"c hs traffic", &transcript, 32);
    assert_eq!(
        c_hs,
        hex("b3eddb126e067f35a780b3abf45e2d8f3b1a950738f52e9600746a0e27a55a21"),
        "c hs traffic"
    );
    let s_hs = expand_label(&hs, b"s hs traffic", &transcript, 32);
    assert_eq!(
        s_hs,
        hex("b67b7d690cc16c4e75e54213cb2d37b4e9c912bcded9105d42befd59d391ad38"),
        "s hs traffic"
    );

    // Client handshake write key/iv (from c_hs_traffic; the server reads
    // these — RFC 8448 {server} derive read traffic keys for handshake).
    let key = expand_label(&c_hs, b"key", &[], 16);
    assert_eq!(
        key,
        hex("dbfaa693d1762c5b666af5d950258d01"),
        "client hs key"
    );
    let iv = expand_label(&c_hs, b"iv", &[], 12);
    assert_eq!(iv, hex("5bd3c71b836e0b76bb73265f"), "client hs iv");
}

/// The key-schedule composition with the RFC 8448 shared secret fed as an
/// input — the default-suite version (X25519 itself is covered separately).
#[test]
fn tls13_key_schedule_rfc8448() {
    key_schedule_rfc8448(&hex(SHARED).try_into().unwrap());
}

/// The shared secret computed by the STEPPED in-circuit ladder (one
/// iteration per circuit eval, threaded state) plus the stepped inversion
/// chain — the same driver as the X25519 heavyweight test, with the RFC
/// 8448 client/server ephemeral keys.
fn x25519_shared_circuit() -> [u8; 32] {
    let step = build_x25519_step();
    let mul = build_fe_mul();
    let inv = build_fe_invert();

    let fe_bits = |w: &Fp| {
        let mut b = bits_of(&fp_to_bytes(w));
        b.truncate(255);
        b
    };
    let fe_out = |out: &[bool]| -> Fp {
        let mut bytes = [0u8; 32];
        for (i, &b) in out.iter().take(255).enumerate() {
            if b {
                bytes[i / 8] |= 1 << (i % 8);
            }
        }
        bytes[31] &= 0x7f;
        let mut v = [0u64; 4];
        for i in 0..4 {
            v[i] = u64::from_le_bytes(bytes[i * 8..i * 8 + 8].try_into().unwrap());
        }
        v
    };

    // Ladder (255 stepped iterations).
    let mut k: [u8; 32] = hex(CLIENT_PRIV).try_into().unwrap();
    k[0] &= 248;
    k[31] &= 127;
    k[31] |= 64;
    let u: Fp = {
        let ub: [u8; 32] = hex(SERVER_PUB).try_into().unwrap();
        fp_from_bytes(&ub)
    };
    let mut x2: Fp = [1, 0, 0, 0];
    let mut z2: Fp = [0; 4];
    let mut x3: Fp = u;
    let mut z3: Fp = [1, 0, 0, 0];
    let mut swap = false;
    for i in 0..255 {
        let t = 254 - i;
        let kt = (k[t / 8] >> (t % 8)) & 1 == 1;
        let mut inputs = Vec::with_capacity(1277);
        for w in [&x2, &z2, &x3, &z3, &u] {
            inputs.extend(fe_bits(w));
        }
        inputs.push(swap);
        inputs.push(kt);
        let out = eval_biir(&step, &inputs).expect("step");
        x2 = fe_out(&out[0..255]);
        z2 = fe_out(&out[255..510]);
        x3 = fe_out(&out[510..765]);
        z3 = fe_out(&out[765..1020]);
        swap = out[1020];
    }
    // Final conditional swap.
    if swap {
        core::mem::swap(&mut x2, &mut x3);
        core::mem::swap(&mut z2, &mut z3);
    }
    // Affine u = x2 * z2^(p-2): the inversion chain as one circuit, the
    // final multiply as another.
    let zinv = {
        let inputs = fe_bits(&z2);
        fe_out(&eval_biir(&inv, &inputs).expect("invert"))
    };
    let mut inputs = fe_bits(&x2);
    inputs.extend(fe_bits(&zinv));
    let res = fe_out(&eval_biir(&mul, &inputs).expect("mul"));
    fp_to_bytes(&res)
}

/// The full end-to-end composition: stepped X25519 in-circuit (RFC 8448
/// client private x server public) then the key schedule. Heavyweight.
#[test]
#[ignore]
fn tls13_full_composition_rfc8448() {
    let shared = x25519_shared_circuit();
    assert_eq!(shared.to_vec(), hex(SHARED), "RFC 8448 shared secret");
    key_schedule_rfc8448(&shared);
}
