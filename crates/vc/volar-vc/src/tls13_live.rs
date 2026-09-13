//! Live two-party TLS 1.3 client driver over a real socket (`std` only).
//!
//! Where [`crate::tls13_2pc`] drives a scripted session from captured wire
//! bytes, this module drives a REAL TLS 1.3 handshake against a real server
//! (rustls, Cloudflare, ...) through the strict chain. The split follows
//! the P4d architecture decision:
//!
//! - **Public-data native**: record framing, handshake-message parsing,
//!   and the decrypted flight inner (revealed after its tag verifies) are
//!   handled natively on both parties.
//! - **Secret-mixing in-circuit**: the key schedule, record crypto, and
//!   verdict comparisons run as strict-chain rounds identical in shape to
//!   the scripted session's.
//!
//! Circuit shapes depend only on byte LENGTHS, which are public; the
//! client (evaluator) discovers them from the wire and ships them to the
//! garbler as small length frames over the same transport between rounds.
//!
//! ## KX status (security-critical placeholder)
//!
//! The X25519 shared secret is computed NATIVELY by the client
//! ([`NativeKx`]), so the client learns the session keys. This is fine
//! for local-server integration testing but MUST NOT be used with a real
//! garbler secret: the client could decrypt the request record. The 2PC
//! KX (Edwards partial-addition, one round, ~1–2M ANDs) is the follow-up
//! that removes this hole.

extern crate std;

use alloc::vec::Vec;
use std::io::{Read, Write};

use digest::Digest;
use volar_mpc::strict_chain::{ChainFeed, ChainOut, ChainParty};
use volar_mpc::{MpcError, OtChannel, Transport};
use volar_spec::vole::VoleArray;

use crate::sha_gadget::build_hmac_sha256;
use crate::tls13::{
    extract_circuit, record_open_circuit, record_seal_circuit, transcript_circuit,
};
use crate::tls13_2pc::{
    Expand, bits_of, bytes_of, expand, hold_range, sched, slots, take,
};

/// Byte-stream socket seam. Implemented by a native blocking-stream
/// adapter and by the browser relay adapter (site side).
pub trait TlsRecordIo {
    /// Write all bytes to the stream.
    fn write_all(&mut self, bytes: &[u8]) -> Result<(), MpcError>;
    /// Read exactly `n` bytes.
    fn read_exact(&mut self, n: usize) -> Result<Vec<u8>, MpcError>;
}

/// A native blocking stream (e.g. `std::net::TcpStream`).
pub struct NativeStream<S> {
    /// The wrapped blocking stream.
    pub stream: S,
}

impl<S: Read + Write> TlsRecordIo for NativeStream<S> {
    fn write_all(&mut self, bytes: &[u8]) -> Result<(), MpcError> {
        self.stream.write_all(bytes).map_err(|_| MpcError::UnexpectedMessage)
    }
    fn read_exact(&mut self, n: usize) -> Result<Vec<u8>, MpcError> {
        let mut buf = alloc::vec![0u8; n];
        self.stream
            .read_exact(&mut buf)
            .map_err(|_| MpcError::UnexpectedMessage)?;
        Ok(buf)
    }
}

/// Read one full TLS record (5-byte header + payload).
pub fn read_record<Io: TlsRecordIo + ?Sized>(io: &mut Io) -> Result<Vec<u8>, MpcError> {
    let hdr = io.read_exact(5)?;
    let len = ((hdr[3] as usize) << 8) | hdr[4] as usize;
    let mut rec = hdr;
    rec.extend(io.read_exact(len)?);
    Ok(rec)
}

/// A handshake-message view parsed from the clean (content-type-stripped,
/// records concatenated) flight message stream.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HsMsg {
    /// Handshake type (8 EE, 11 Certificate, 15 CertVerify, 20 Finished).
    pub hs_type: u8,
    /// Offset of the 4-byte handshake header in the clean stream.
    pub off: usize,
    /// Total message length (4-byte header + body).
    pub len: usize,
}

/// Walk handshake messages in the clean flight stream.
pub fn walk_handshake(stream: &[u8]) -> Vec<HsMsg> {
    let mut out = Vec::new();
    let mut off = 0;
    while off + 4 <= stream.len() {
        let hs_type = stream[off];
        let len = ((stream[off + 1] as usize) << 16)
            | ((stream[off + 2] as usize) << 8)
            | stream[off + 3] as usize;
        let total = 4 + len;
        if off + total > stream.len() {
            break;
        }
        out.push(HsMsg {
            hs_type,
            off,
            len: total,
        });
        off += total;
    }
    out
}

/// Parse a ServerHello handshake message, returning the server's x25519
/// key-share public key. TLS_AES_128_GCM_SHA256 only; HelloRetryRequest
/// is not supported.
pub fn parse_sh_key_share(sh: &[u8]) -> Result<[u8; 32], MpcError> {
    // sh: hs header(4) | legacy_version(2) | random(32) | sid | suite(2)
    // | compression(1) | extensions.
    let mut p = 4 + 2 + 32;
    if p >= sh.len() {
        return Err(MpcError::UnexpectedMessage);
    }
    let sid_len = sh[p] as usize;
    p += 1 + sid_len;
    if p + 2 + 1 + 2 > sh.len() {
        return Err(MpcError::UnexpectedMessage);
    }
    let suite = ((sh[p] as u16) << 8) | sh[p + 1] as u16;
    if suite != 0x1301 {
        return Err(MpcError::UnsupportedStorage);
    }
    p += 2 + 1;
    let ext_len = ((sh[p] as usize) << 8) | sh[p + 1] as usize;
    p += 2;
    let end = p + ext_len;
    if end > sh.len() {
        return Err(MpcError::UnexpectedMessage);
    }
    while p + 4 <= end {
        let et = ((sh[p] as u16) << 8) | sh[p + 1] as u16;
        let el = ((sh[p + 2] as usize) << 8) | sh[p + 3] as usize;
        p += 4;
        if p + el > end {
            return Err(MpcError::UnexpectedMessage);
        }
        if et == 0x0033 {
            // KeyShareEntry = group(2) || key_exchange<1..2^16-1> (2-byte
            // length prefix + data).
            if el < 6 || sh[p] != 0x00 || sh[p + 1] != 0x1d {
                return Err(MpcError::UnsupportedStorage);
            }
            let kex = &sh[p + 4..p + el];
            if kex.len() != 32 {
                return Err(MpcError::UnexpectedMessage);
            }
            let mut out = [0u8; 32];
            out.copy_from_slice(kex);
            return Ok(out);
        }
        p += el;
    }
    Err(MpcError::UnexpectedMessage)
}

/// The client-side key exchange (placeholder, see module docs).
pub trait NativeKx {
    /// The client's ephemeral public key (key_share bytes for the CH).
    fn public_key(&self) -> [u8; 32];
    /// Compute the shared secret against the server's public key.
    fn shared_secret(&self, server_public: &[u8; 32]) -> [u8; 32];
}

/// Build a minimal TLS 1.3 ClientHello handshake message (4-byte header
/// included): one cipher suite (TLS_AES_128_GCM_SHA256), SNI,
/// supported_versions, supported_groups, and an x25519 key share.
pub fn build_client_hello(
    server_name: &str,
    client_public: &[u8; 32],
    random: &[u8; 32],
) -> Vec<u8> {
    let mut body = Vec::new();
    body.extend_from_slice(&[0x03, 0x03]);
    body.extend_from_slice(random);
    body.push(0);
    body.extend_from_slice(&[0x00, 0x02, 0x13, 0x01]);
    body.extend_from_slice(&[0x01, 0x00]);

    let mut exts = Vec::new();
    {
        let mut list = Vec::new();
        list.push(0);
        list.extend_from_slice(&(server_name.len() as u16).to_be_bytes());
        list.extend_from_slice(server_name.as_bytes());
        exts.extend_from_slice(&[0x00, 0x00]);
        exts.extend_from_slice(&((list.len() + 2) as u16).to_be_bytes());
        exts.extend_from_slice(&(list.len() as u16).to_be_bytes());
        exts.extend_from_slice(&list);
    }
    exts.extend_from_slice(&[0x00, 0x2b, 0x00, 0x03, 0x02, 0x03, 0x04]);
    exts.extend_from_slice(&[0x00, 0x0a, 0x00, 0x04, 0x00, 0x02, 0x00, 0x1d]);
    // signature_algorithms: ecdsa_secp256r1_sha256, ed25519,
    // rsa_pss_rsae_sha256 (required when the server authenticates by cert).
    exts.extend_from_slice(&[
        0x00, 0x0d, 0x00, 0x08, 0x00, 0x06, 0x04, 0x03, 0x08, 0x07, 0x08, 0x04,
    ]);
    {
        let mut ks = Vec::new();
        ks.extend_from_slice(&[0x00, 0x1d, 0x00, 0x20]);
        ks.extend_from_slice(client_public);
        exts.extend_from_slice(&[0x00, 0x33]);
        exts.extend_from_slice(&((ks.len() + 2) as u16).to_be_bytes());
        exts.extend_from_slice(&(ks.len() as u16).to_be_bytes());
        exts.extend_from_slice(&ks);
    }

    body.extend_from_slice(&(exts.len() as u16).to_be_bytes());
    body.extend_from_slice(&exts);

    let mut msg = Vec::new();
    msg.push(0x01);
    msg.extend_from_slice(&(body.len() as u32).to_be_bytes()[1..]);
    msg.extend_from_slice(&body);
    msg
}

/// Wrap a handshake message in a TLS handshake record.
pub fn record_of(hs_msg: &[u8]) -> Vec<u8> {
    let mut rec = Vec::new();
    rec.extend_from_slice(&[0x16, 0x03, 0x01]);
    rec.extend_from_slice(&(hs_msg.len() as u16).to_be_bytes());
    rec.extend_from_slice(hs_msg);
    rec
}

/// The request-body template (the live analogue of the scripted
/// session's `req_prefix/mid/suffix` + lengths).
pub struct LiveRequestTemplate {
    /// Bytes before the Turnstile secret.
    pub prefix: Vec<u8>,
    /// Turnstile secret byte length (public, part of the template).
    pub secret_len: usize,
    /// Between secret and token.
    pub mid: Vec<u8>,
    /// Turnstile token byte length (public).
    pub token_len: usize,
    /// After the token.
    pub suffix: Vec<u8>,
    /// The success marker searched in the decrypted response body.
    pub success_marker: Vec<u8>,
}

impl LiveRequestTemplate {
    /// The sealed inner plaintext length (body + content-type byte).
    pub fn inner_len(&self) -> usize {
        self.prefix.len() + self.secret_len + self.mid.len() + self.token_len + self.suffix.len() + 1
    }
}

/// Per-role secret inputs.
pub enum LiveSecrets<'a> {
    /// The site server (garbler): the Turnstile secret.
    Garbler {
        /// The Turnstile secret key bytes.
        secret: &'a [u8],
    },
    /// The client (evaluator): the Turnstile token.
    Evaluator {
        /// The Turnstile token bytes.
        token: &'a [u8],
    },
}

/// What the live driver returns on success.
pub struct LiveTlsOutcome {
    /// The clean flight message stream (all flight records decrypted,
    /// content types stripped, concatenated; revealed to both parties).
    /// The garbler's native pinned-root check consumes this.
    pub flight_stream: Vec<u8>,
    /// Held slot ids of the per-record flight tag verdicts.
    pub vt_records: Vec<usize>,
    /// Held slot of the server-Finished verdict.
    pub vf_server: usize,
    /// Held slot of the response tag verdict.
    pub vt_response: usize,
    /// Held slot of the success-marker verdict.
    pub vs_success: usize,
    /// The decrypted response inner (revealed; public HTTP response).
    pub response_inner: Vec<u8>,
}

// Live-session held-slot regions (distinct from the scripted driver's).
const LIVE_INNER: usize = 40 << 16; // + rec * (1<<17)
const LIVE_TAG: usize = 42 << 16; // + rec * 256
const LIVE_NONCE: usize = 43 << 16; // + rec * 256
const LIVE_VT: usize = 44 << 16; // + rec

fn inner_slot(rec: usize) -> usize {
    LIVE_INNER + rec * (1 << 17)
}

struct Asm {
    feeds: Vec<ChainFeed>,
    consts: Vec<bool>,
    secrets: Vec<bool>,
}
impl Asm {
    fn new() -> Self {
        Self {
            feeds: Vec::new(),
            consts: Vec::new(),
            secrets: Vec::new(),
        }
    }
    fn held(&mut self, base: usize, n: usize) {
        self.feeds
            .extend((0..n).map(|i| ChainFeed::Held(base + i)));
    }
    fn konst(&mut self, bits: &[bool]) {
        self.feeds
            .extend(core::iter::repeat_n(ChainFeed::Const, bits.len()));
        self.consts.extend_from_slice(bits);
    }
    fn eval(&mut self, count: usize, bits: &[bool]) {
        debug_assert!(bits.len() == count || bits.is_empty());
        self.feeds.extend(core::iter::repeat_n(ChainFeed::Eval, count));
        self.secrets.extend_from_slice(bits);
    }
    fn garb(&mut self, count: usize, bits: &[bool]) {
        debug_assert!(bits.len() == count || bits.is_empty());
        self.feeds
            .extend(core::iter::repeat_n(ChainFeed::Garbler, count));
        self.secrets.extend_from_slice(bits);
    }
    fn expand(&mut self, e: &Expand, secret_slot: usize, ctx: Ctx) {
        self.held(secret_slot, 256);
        self.konst(&bits_of(&e.prefix));
        match ctx {
            Ctx::Held(slot) => self.held(slot, e.ctx_len * 8),
            Ctx::Const(bytes) => self.konst(&bits_of(bytes)),
        }
        self.konst(&bits_of(&[0x01u8]));
    }
}
enum Ctx<'a> {
    Held(usize),
    Const(&'a [u8]),
}

fn send_lens<T: Transport>(transport: &mut T, lens: &[usize]) {
    let mut f = Vec::new();
    for &l in lens {
        f.extend_from_slice(&(l as u32).to_be_bytes());
    }
    transport.send(&f);
}
fn recv_lens<T: Transport>(transport: &mut T, n: usize) -> Result<Vec<usize>, MpcError> {
    let f = transport.recv();
    if f.len() != 4 * n {
        return Err(MpcError::UnexpectedMessage);
    }
    Ok((0..n)
        .map(|i| u32::from_be_bytes(f[4 * i..4 * i + 4].try_into().unwrap()) as usize)
        .collect())
}

/// Strip trailing zero padding and the inner content-type byte from a
/// decrypted record's inner plaintext.
fn strip_inner(inner: &[u8]) -> &[u8] {
    let mut end = inner.len();
    while end > 0 && inner[end - 1] == 0 {
        end -= 1;
    }
    if end > 0 {
        end -= 1; // the content-type byte
    }
    &inner[..end]
}

/// Locate the (record, intra-record offset) of `clean_off` bytes into the
/// clean stream, given per-record inner lengths.
fn locate(inner_lens: &[usize], clean_off: usize) -> Result<(usize, usize), MpcError> {
    let mut base = 0usize;
    for (r, &il) in inner_lens.iter().enumerate() {
        let clean = il.saturating_sub(1);
        if clean_off < base + clean {
            return Ok((r, clean_off - base));
        }
        base += clean;
    }
    Err(MpcError::UnexpectedMessage)
}

/// Run the live two-party TLS 1.3 session as a strict chain.
///
/// Both parties execute this same function with their role's chain driver;
/// the evaluator additionally performs the socket IO (`io`, `kx`) between
/// phases. The garbler passes `None` for both.
pub fn run_live_tls_session<N, D, C, T, Io>(
    chain: &mut C,
    server_name: &str,
    template: &LiveRequestTemplate,
    secrets: &LiveSecrets,
    kx: Option<&dyn NativeKx>,
    ch_random: &[u8; 32],
    mut io: Option<&mut Io>,
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
) -> Result<LiveTlsOutcome, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
    C: ChainParty<N>,
    T: Transport,
    Io: TlsRecordIo + ?Sized,
{
    let empty_hash: [u8; 32] = [
        0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f,
        0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b,
        0x78, 0x52, 0xb8, 0x55,
    ];
    let is_eval = kx.is_some();

    macro_rules! round {
        ($sched:expr, $asm:expr, $outs:expr) => {
            chain.run_round::<D, T>(
                $sched,
                &$asm.feeds,
                &$asm.consts,
                &$asm.secrets,
                &$outs,
                transport,
                ot,
            )?
        };
    }

    // ---- Phase 0: native CH build/send + SH read/parse (evaluator) ----
    let mut ch: Vec<u8> = Vec::new();
    let mut sh: Vec<u8> = Vec::new();
    let mut server_pub = [0u8; 32];
    if is_eval {
        let kx = kx.expect("checked");
        let io = io.as_deref_mut().expect("evaluator io");
        let ch_msg = build_client_hello(server_name, &kx.public_key(), ch_random);
        io.write_all(&record_of(&ch_msg))?;
        ch = ch_msg;
        // Read records until the ServerHello handshake record (skip CCS).
        loop {
            let rec = read_record(io)?;
            match rec[0] {
                0x14 => continue, // change_cipher_spec
                0x16 => {
                    sh = rec[5..].to_vec();
                    break;
                }
                0x15 => return Err(MpcError::UnexpectedMessage), // alert
                _ => return Err(MpcError::UnexpectedMessage),
            }
        }
        server_pub = parse_sh_key_share(&sh)?;
        send_lens(transport, &[ch.len(), sh.len()]);
    }
    let (ch_len, sh_len) = if is_eval {
        (ch.len(), sh.len())
    } else {
        let l = recv_lens(transport, 2)?;
        (l[0], l[1])
    };

    // ---- Key-schedule rounds (identical shapes to the scripted driver) ----
    let t1 = sched(&transcript_circuit(ch_len + sh_len));
    let mut a = Asm::new();
    a.eval(ch_len * 8, &bits_of(&ch));
    a.eval(sh_len * 8, &bits_of(&sh));
    let _ = round!(&t1, a, hold_range(slots::T_HASH, 256));

    let early = sched(&extract_circuit(32, 32));
    let mut a = Asm::new();
    a.konst(&alloc::vec![false; 512]);
    let _ = round!(&early, a, hold_range(slots::EARLY, 256));

    let derived = expand(32, b"derived", 32);
    let mut a = Asm::new();
    a.expand(&derived, slots::EARLY, Ctx::Const(&empty_hash));
    let _ = round!(&derived.sched, a, hold_range(slots::DERIVED, 256));

    // The shared secret is an evaluator-private input; the garbler passes
    // an empty secret slice (it never learns the value).
    let ss_bits: Vec<bool> = match kx {
        Some(kx) => bits_of(&kx.shared_secret(&server_pub)),
        None => Vec::new(),
    };
    let hs = sched(&extract_circuit(32, 32));
    let mut a = Asm::new();
    a.held(slots::DERIVED, 256);
    a.eval(256, &ss_bits);
    let _ = round!(&hs, a, hold_range(slots::HS, 256));

    let c_hs = expand(32, b"c hs traffic", 32);
    let mut a = Asm::new();
    a.expand(&c_hs, slots::HS, Ctx::Held(slots::T_HASH));
    let _ = round!(&c_hs.sched, a, hold_range(slots::C_HS, 256));
    let s_hs = expand(32, b"s hs traffic", 32);
    let mut a = Asm::new();
    a.expand(&s_hs, slots::HS, Ctx::Held(slots::T_HASH));
    let _ = round!(&s_hs.sched, a, hold_range(slots::S_HS, 256));

    for (label, src, dst) in [
        (&b"key"[..], slots::C_HS, slots::C_KEY),
        (b"iv", slots::C_HS, slots::C_IV),
        (b"key", slots::S_HS, slots::S_KEY),
        (b"iv", slots::S_HS, slots::S_IV),
    ] {
        let out_len: u16 = if label == b"key" { 16 } else { 12 };
        let e = expand(out_len, label, 0);
        let mut a = Asm::new();
        a.expand(&e, src, Ctx::Const(&[]));
        let _ = round!(&e.sched, a, hold_range(dst, 256));
    }

    // ---- Flight loop: per-record open + tag verdict + inner reveal ----
    let mut stream: Vec<u8> = Vec::new();
    let mut inner_lens: Vec<usize> = Vec::new();
    let mut vt_records: Vec<usize> = Vec::new();
    let mut saw_finished = false;
    while !saw_finished {
        let rec_idx = inner_lens.len();
        let rec = if is_eval {
            let io = io.as_deref_mut().expect("evaluator io");
            loop {
                let r = read_record(io)?;
                if r[0] == 0x14 {
                    continue; // CCS
                }
                if r[0] == 0x15 {
                    return Err(MpcError::UnexpectedMessage);
                }
                if r[0] != 0x17 {
                    return Err(MpcError::UnexpectedMessage);
                }
                break r;
            }
        } else {
            Vec::new()
        };
        let inner = if is_eval { rec.len() - 5 - 16 } else { 0 };
        if is_eval {
            send_lens(transport, &[inner]);
        }
        let inner = if is_eval {
            inner
        } else {
            recv_lens(transport, 1)?[0]
        };

        // nonce = s_iv XOR seq (const seq).
        let nonce_sched = sched(&crate::tls13_2pc::xor_const_circuit(96));
        let mut a = Asm::new();
        a.held(slots::S_IV, 96);
        let seq_bits: Vec<bool> = (0..96)
            .map(|i| {
                let shift = 96 - 1 - i;
                if shift < 64 {
                    ((rec_idx as u64) >> shift) & 1 == 1
                } else {
                    false
                }
            })
            .collect();
        a.konst(&seq_bits);
        let _ = round!(
            &nonce_sched,
            a,
            hold_range(LIVE_NONCE + rec_idx * 256, 96)
        );

        let open = sched(&record_open_circuit(inner));
        let rec_bits = bits_of(&rec);
        let mut a = Asm::new();
        a.held(slots::S_KEY, 128);
        a.held(LIVE_NONCE + rec_idx * 256, 96);
        a.eval(40, take(&rec_bits, 0, 40));
        a.eval(inner * 8, take(&rec_bits, 40, inner * 8));
        let mut outs = hold_range(inner_slot(rec_idx), inner * 8);
        outs.extend(hold_range(LIVE_TAG + rec_idx * 256, 128));
        let _ = round!(&open, a, outs);

        let eq_tag = sched(&crate::tls13_2pc::eq_bits_circuit(128));
        let mut a = Asm::new();
        a.held(LIVE_TAG + rec_idx * 256, 128);
        a.eval(128, take(&rec_bits, 40 + inner * 8, 128));
        let _ = round!(
            &eq_tag,
            a,
            alloc::vec![ChainOut::Hold(LIVE_VT + rec_idx)]
        );

        // Reveal the inner (public data; both parties parse it, and the
        // garbler's native cert check consumes it).
        let reveal = sched(&crate::tls13_2pc::identity_circuit(inner * 8));
        let mut a = Asm::new();
        a.held(inner_slot(rec_idx), inner * 8);
        let inner_revealed =
            round!(&reveal, a, alloc::vec![ChainOut::Reveal; inner * 8]);
        let inner_bytes = bytes_of(&inner_revealed);
        // Alerts arrive as type-23 records whose inner content type is 21.
        let ct = inner_bytes
            .iter()
            .rposition(|&b| b != 0)
            .map(|i| inner_bytes[i]);
        if ct == Some(0x15) {
            return Err(MpcError::UnexpectedMessage);
        }
        stream.extend_from_slice(strip_inner(&inner_bytes));
        inner_lens.push(inner);
        vt_records.push(LIVE_VT + rec_idx);
        saw_finished = walk_handshake(&stream).iter().any(|m| m.hs_type == 20);
        if rec_idx >= 16 && !saw_finished {
            return Err(MpcError::UnexpectedMessage);
        }
    }

    let msgs = walk_handshake(&stream);
    let fin_msg = msgs.iter().find(|m| m.hs_type == 20).expect("finished");
    let t2_len = fin_msg.off;
    let t_th_len = fin_msg.off + fin_msg.len;

    // Feed the clean-stream prefix from held per-record slots, skipping
    // each record's content-type byte.
    let feed_clean_prefix = |a: &mut Asm, len: usize| {
        let mut remaining = len;
        for (r, &il) in inner_lens.iter().enumerate() {
            let clean = il - 1;
            let take_n = remaining.min(clean);
            a.held(inner_slot(r), take_n * 8);
            remaining -= take_n;
            if remaining == 0 {
                break;
            }
        }
        debug_assert_eq!(remaining, 0);
    };

    // ---- Transcript2 + server-Finished verification ----
    let t2 = sched(&transcript_circuit(ch_len + sh_len + t2_len));
    let mut a = Asm::new();
    a.eval(ch_len * 8, &bits_of(&ch));
    a.eval(sh_len * 8, &bits_of(&sh));
    feed_clean_prefix(&mut a, t2_len);
    let _ = round!(&t2, a, hold_range(slots::T2, 256));

    let fk_s = expand(32, b"finished", 0);
    let mut a = Asm::new();
    a.expand(&fk_s, slots::S_HS, Ctx::Const(&[]));
    let _ = round!(&fk_s.sched, a, hold_range(slots::FK_S, 256));

    let server_fin = sched(&build_hmac_sha256(32, 32));
    let mut a = Asm::new();
    a.held(slots::FK_S, 256);
    a.held(slots::T2, 256);
    let _ = round!(&server_fin, a, hold_range(slots::SCRATCH, 256));

    // verify_data is the 32-byte body of the Finished message, at clean
    // offset fin_msg.off + 4. Locate it within one record.
    let (fin_rec, fin_off) = locate(&inner_lens, fin_msg.off + 4)?;
    if fin_off + 32 > inner_lens[fin_rec] - 1 {
        return Err(MpcError::UnexpectedMessage); // spans records (unsupported)
    }
    let eq_fin = sched(&crate::tls13_2pc::eq_bits_circuit(256));
    let mut a = Asm::new();
    a.held(slots::SCRATCH, 256);
    a.held(inner_slot(fin_rec) + fin_off * 8, 256);
    let _ = round!(&eq_fin, a, alloc::vec![ChainOut::Hold(slots::VF_SERVER)]);

    // ---- Transcript3 (through the server Finished) ----
    let t3 = sched(&transcript_circuit(ch_len + sh_len + t_th_len));
    let mut a = Asm::new();
    a.eval(ch_len * 8, &bits_of(&ch));
    a.eval(sh_len * 8, &bits_of(&sh));
    feed_clean_prefix(&mut a, t_th_len);
    let _ = round!(&t3, a, hold_range(slots::T_TH, 256));

    // ---- Master + application keys ----
    let derived2 = expand(32, b"derived", 32);
    let mut a = Asm::new();
    a.expand(&derived2, slots::HS, Ctx::Const(&empty_hash));
    let _ = round!(&derived2.sched, a, hold_range(slots::D2, 256));
    let master = sched(&extract_circuit(32, 32));
    let mut a = Asm::new();
    a.held(slots::D2, 256);
    a.konst(&alloc::vec![false; 256]);
    let _ = round!(&master, a, hold_range(slots::MASTER, 256));

    let c_ap = expand(32, b"c ap traffic", 32);
    let mut a = Asm::new();
    a.expand(&c_ap, slots::MASTER, Ctx::Held(slots::T_TH));
    let _ = round!(&c_ap.sched, a, hold_range(slots::C_AP, 256));
    let s_ap = expand(32, b"s ap traffic", 32);
    let mut a = Asm::new();
    a.expand(&s_ap, slots::MASTER, Ctx::Held(slots::T_TH));
    let _ = round!(&s_ap.sched, a, hold_range(slots::S_AP, 256));
    for (label, src, dst) in [
        (&b"key"[..], slots::C_AP, slots::C_AP_KEY),
        (b"iv", slots::C_AP, slots::C_AP_IV),
        (b"key", slots::S_AP, slots::S_AP_KEY),
        (b"iv", slots::S_AP, slots::S_AP_IV),
    ] {
        let out_len: u16 = if label == b"key" { 16 } else { 12 };
        let e = expand(out_len, label, 0);
        let mut a = Asm::new();
        a.expand(&e, src, Ctx::Const(&[]));
        let _ = round!(&e.sched, a, hold_range(dst, 256));
    }

    // ---- Client Finished (handshake keys, seq 0) ----
    let cfk = expand(32, b"finished", 0);
    let mut a = Asm::new();
    a.expand(&cfk, slots::C_HS, Ctx::Const(&[]));
    let _ = round!(&cfk.sched, a, hold_range(slots::CFK, 256));
    let client_fin = sched(&build_hmac_sha256(32, 32));
    let mut a = Asm::new();
    a.held(slots::CFK, 256);
    a.held(slots::T_TH, 256);
    let _ = round!(&client_fin, a, hold_range(slots::VFD, 256));
    let seal_fin = sched(&record_seal_circuit(37));
    let mut a = Asm::new();
    a.held(slots::C_KEY, 128);
    a.held(slots::C_IV, 96);
    a.konst(&bits_of(&[0x17u8, 0x03, 0x03, 0x00, 0x35]));
    a.konst(&bits_of(&[0x14u8, 0x00, 0x00, 0x20]));
    a.held(slots::VFD, 256);
    a.konst(&bits_of(&[0x16u8]));
    let cf_record = round!(&seal_fin, a, alloc::vec![ChainOut::Reveal; 37 * 8 + 128]);
    if is_eval {
        // The reveal is ct||tag; the wire record needs its 5-byte header
        // (which is exactly the AAD).
        let mut wire = alloc::vec![0x17u8, 0x03, 0x03, 0x00, 0x35];
        wire.extend_from_slice(&bytes_of(&cf_record));
        io.as_deref_mut()
            .expect("evaluator io")
            .write_all(&wire)?;
    }

    // ---- The siteverify request (application keys, seq 0) ----
    let (gsecret, token): (&[u8], &[u8]) = match secrets {
        LiveSecrets::Garbler { secret } => (secret, &[]),
        LiveSecrets::Evaluator { token } => (&[], token),
    };
    debug_assert!(gsecret.is_empty() || gsecret.len() == template.secret_len);
    debug_assert!(token.is_empty() || token.len() == template.token_len);
    let pt_len = template.inner_len();
    let aad = [
        0x17u8,
        0x03,
        0x03,
        ((pt_len + 16) >> 8) as u8,
        (pt_len + 16) as u8,
    ];
    let seal_req = sched(&record_seal_circuit(pt_len));
    let mut a = Asm::new();
    a.held(slots::C_AP_KEY, 128);
    a.held(slots::C_AP_IV, 96);
    a.konst(&bits_of(&aad));
    a.konst(&bits_of(&template.prefix));
    a.garb(template.secret_len * 8, &bits_of(gsecret));
    a.konst(&bits_of(&template.mid));
    a.eval(template.token_len * 8, &bits_of(token));
    a.konst(&bits_of(&template.suffix));
    a.konst(&bits_of(&[0x17u8]));
    let req_record = round!(
        &seal_req,
        a,
        alloc::vec![ChainOut::Reveal; pt_len * 8 + 128]
    );
    if is_eval {
        let mut wire = aad.to_vec();
        wire.extend_from_slice(&bytes_of(&req_record));
        io.as_deref_mut()
            .expect("evaluator io")
            .write_all(&wire)?;
    }

    // ---- The response (server app data, seq 0) ----
    let resp_rec = if is_eval {
        let io = io.as_deref_mut().expect("evaluator io");
        loop {
            let r = read_record(io)?;
            if r[0] == 0x14 {
                continue;
            }
            if r[0] == 0x15 {
                return Err(MpcError::UnexpectedMessage);
            }
            if r[0] != 0x17 {
                return Err(MpcError::UnexpectedMessage);
            }
            break r;
        }
    } else {
        Vec::new()
    };
    let resp_inner = if is_eval {
        resp_rec.len() - 5 - 16
    } else {
        0
    };
    if is_eval {
        send_lens(transport, &[resp_inner]);
    }
    let resp_inner = if is_eval {
        resp_inner
    } else {
        recv_lens(transport, 1)?[0]
    };

    let open_resp = sched(&record_open_circuit(resp_inner));
    let resp_bits = bits_of(&resp_rec);
    let mut a = Asm::new();
    a.held(slots::S_AP_KEY, 128);
    a.held(slots::S_AP_IV, 96); // seq 0: nonce == iv
    a.eval(40, take(&resp_bits, 0, 40));
    a.eval(resp_inner * 8, take(&resp_bits, 40, resp_inner * 8));
    let mut outs = hold_range(slots::RESP, resp_inner * 8);
    outs.extend(hold_range(slots::R_TAG, 128));
    let _ = round!(&open_resp, a, outs);

    let eq_rtag = sched(&crate::tls13_2pc::eq_bits_circuit(128));
    let mut a = Asm::new();
    a.held(slots::R_TAG, 128);
    a.eval(128, take(&resp_bits, 40 + resp_inner * 8, 128));
    let _ = round!(&eq_rtag, a, alloc::vec![ChainOut::Hold(slots::VT_RESP)]);

    let success = sched(&crate::tls13_2pc::contains_bytes_circuit(
        resp_inner,
        &template.success_marker,
    ));
    let mut a = Asm::new();
    a.held(slots::RESP, resp_inner * 8);
    let _ = round!(&success, a, alloc::vec![ChainOut::Hold(slots::VS_SUCCESS)]);

    let reveal_resp = sched(&crate::tls13_2pc::identity_circuit(resp_inner * 8));
    let mut a = Asm::new();
    a.held(slots::RESP, resp_inner * 8);
    let resp_revealed = round!(
        &reveal_resp,
        a,
        alloc::vec![ChainOut::Reveal; resp_inner * 8]
    );

    Ok(LiveTlsOutcome {
        flight_stream: stream,
        vt_records,
        vf_server: slots::VF_SERVER,
        vt_response: slots::VT_RESP,
        vs_success: slots::VS_SUCCESS,
        response_inner: bytes_of(&resp_revealed),
    })
}
