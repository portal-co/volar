//! The two-party TLS 1.3 client session driver (P4): the scripted-peer
//! session of `tls13`'s concrete driver, run as a strict multi-circuit
//! **chain** (`volar_mpc::strict_chain`).
//!
//! Key-schedule secrets and record keys cross circuit invocations as
//! threaded re-based labels — no intermediate key, hash, or secret is ever
//! decoded by either party. Record ciphertexts are revealed between rounds
//! (they are on the wire anyway) so the evaluator's host does the socket IO
//! natively; only the ciphertexts and the caller's final verdict are ever
//! revealed. The Turnstile secret is a garbler-private input, the token an
//! evaluator-private input, and the success verdict is threaded out as a
//! held bit — the caller correlates it with another MPC component (e.g.
//! the licensing predicate) by running that circuit in the same chain and
//! AND-ing the threaded verdict wires, so neither verdict can be replayed
//! or swapped independently.
//!
//! Roles: the TLS client is the **evaluator** (its host holds the socket;
//! the handshake bytes, records, shared secret, and token are its private
//! inputs); the site's server is the **garbler** (the Turnstile secret is
//! its private input). The shared-secret input is the scripted-test
//! stand-in for the production joint-ephemeral X25519 ladder output (a
//! client-generated ephemeral would let the evaluator derive the channel
//! keys natively and read the request plaintext — the two-party ladder is
//! the documented production replacement).

use alloc::vec;
use alloc::vec::Vec;

use digest::Digest;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_mpc::strict_chain::{ChainFeed, ChainOut, ChainParty};
use volar_mpc::{GateSchedule, MpcError, OtChannel, Transport};
use volar_spec::vole::VoleArray;

use crate::sha_gadget::build_hmac_sha256;
use crate::tls13::{
    expand_label_circuit, extract_circuit, record_open_circuit, record_seal_circuit,
    transcript_circuit,
};

/// Held-slot bases (per-bit slots are `base + bit_index`). Spaced so every
/// threaded value fits. HKDF-Expand-Label rounds hold the full 256-bit
/// HMAC output; consumers read only the first `out_len` bytes (the RFC
/// 8446 truncation), matching the concrete driver.
pub(crate) mod slots {
    /// Transcript SHA-256 over CH||SH (256).
    pub const T_HASH: usize = 0;
    /// Early secret (256).
    pub const EARLY: usize = 1 << 16;
    /// "derived" secret (256).
    pub const DERIVED: usize = 2 << 16;
    /// Handshake secret (256).
    pub const HS: usize = 3 << 16;
    /// c_hs_traffic (256).
    pub const C_HS: usize = 4 << 16;
    /// s_hs_traffic (256).
    pub const S_HS: usize = 5 << 16;
    /// Client handshake key (128).
    pub const C_KEY: usize = 6 << 16;
    /// Client handshake iv (96).
    pub const C_IV: usize = 7 << 16;
    /// Server handshake key (128).
    pub const S_KEY: usize = 8 << 16;
    /// Server handshake iv (96).
    pub const S_IV: usize = 9 << 16;
    /// Decrypted server-flight inner plaintext incl. content type.
    pub const INNER: usize = 10 << 16;
    /// Recomputed server-flight tag (128).
    pub const S_TAG: usize = 11 << 16;
    /// Transcript through CertVerify (256).
    pub const T2: usize = 12 << 16;
    /// Server finished key (256).
    pub const FK_S: usize = 13 << 16;
    /// Transcript through the server Finished (256).
    pub const T_TH: usize = 14 << 16;
    /// Master-secret "derived" (256).
    pub const D2: usize = 15 << 16;
    /// Master secret (256).
    pub const MASTER: usize = 16 << 16;
    /// c_ap_traffic (256).
    pub const C_AP: usize = 17 << 16;
    /// s_ap_traffic (256).
    pub const S_AP: usize = 18 << 16;
    /// Client application key (128).
    pub const C_AP_KEY: usize = 19 << 16;
    /// Client application iv (96).
    pub const C_AP_IV: usize = 20 << 16;
    /// Server application key (128).
    pub const S_AP_KEY: usize = 21 << 16;
    /// Server application iv (96).
    pub const S_AP_IV: usize = 22 << 16;
    /// Client finished key (256).
    pub const CFK: usize = 23 << 16;
    /// Client Finished verify_data (256).
    pub const VFD: usize = 24 << 16;
    /// Decrypted siteverify-response inner plaintext incl. content type.
    pub const RESP: usize = 25 << 16;
    /// Recomputed response tag (128).
    pub const R_TAG: usize = 26 << 16;
    /// Scratch (the computed server verify_data before the eq check).
    pub const SCRATCH: usize = 30 << 16;
    /// Verdict bit: the server flight's tag verified.
    pub const VT_FLIGHT: usize = 27 << 16;
    /// Verdict bit: the server Finished verified.
    pub const VF_SERVER: usize = (27 << 16) + 1;
    /// Verdict bit: the siteverify response's tag verified.
    pub const VT_RESP: usize = (27 << 16) + 2;
    /// Verdict bit: the response body carries the success marker.
    pub const VS_SUCCESS: usize = (27 << 16) + 3;
}

/// `bits[off..off + len]`, or the empty slice when `bits` is empty (the
/// non-owning party's stand-in).
pub(crate) fn take(bits: &[bool], off: usize, len: usize) -> &[bool] {
    if bits.is_empty() {
        &[]
    } else {
        &bits[off..off + len]
    }
}

pub(crate) fn hold_range(base: usize, n: usize) -> Vec<ChainOut> {
    (0..n).map(|i| ChainOut::Hold(base + i)).collect()
}

pub(crate) fn held_feeds(base: usize, n: usize) -> Vec<ChainFeed> {
    (0..n).map(|i| ChainFeed::Held(base + i)).collect()
}

/// Byte vector to LSB-first-per-byte bit vector.
pub fn bits_of(b: &[u8]) -> Vec<bool> {
    b.iter()
        .flat_map(|x| (0..8).map(move |i| (x >> i) & 1 == 1))
        .collect()
}

/// Bit vector to bytes (LSB-first per byte).
pub fn bytes_of(b: &[bool]) -> Vec<u8> {
    b.chunks(8)
        .map(|c| {
            c.iter()
                .enumerate()
                .fold(0u8, |a, (i, &x)| a | ((x as u8) << i))
        })
        .collect()
}

fn finish(params: usize, stmts: Vec<Node<BIrStmt, ()>>, args: Vec<IRVarId>) -> BIrBlocks {
    let params = params as u32;
    BIrBlocks {
        blocks: vec![BIrBlock {
            params,
            stmts,
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args,
            }),
        }],
        pre_init: vec![],
    }
}


/// A tiny circuit: bitwise XOR of two n-bit inputs (with a constant
/// second operand this folds to polarity flips under Not-elimination).
pub(crate) fn xor_const_circuit(n: usize) -> BIrBlocks {
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    let mut args = Vec::new();
    for i in 0..n {
        stmts.push(Node::new(
            BIrStmt::Xor(IRVarId(i as u32), IRVarId((n + i) as u32)),
            (),
            None,
        ));
        args.push(IRVarId((2 * n + i) as u32));
    }
    finish(2 * n, stmts, args)
}

/// A tiny circuit: the identity on n input bits (free pass-through;
/// used to reveal held slot contents via the strict-chain reveal round).
pub(crate) fn identity_circuit(n: usize) -> BIrBlocks {
    let args: Vec<IRVarId> = (0..n).map(|i| IRVarId(i as u32)).collect();
    finish(n, Vec::new(), args)
}

/// A tiny hand-built pure-boolean circuit: bitwise-XNOR equality of two
/// n-bit inputs, output 1 bit.
pub(crate) fn eq_bits_circuit(n: usize) -> BIrBlocks {
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    for i in 0..n {
        // xnor_i = Not(Xor(a_i, b_i)); var ids: params = 2n, stmt j = 2n + j.
        stmts.push(Node::new(
            BIrStmt::Xor(IRVarId(i as u32), IRVarId((n + i) as u32)),
            (),
            None,
        ));
        stmts.push(Node::new(
            BIrStmt::Not(IRVarId((2 * n + 2 * i) as u32)),
            (),
            None,
        ));
    }
    let xn = |i: usize| IRVarId((2 * n + 2 * i + 1) as u32);
    let mut acc = xn(0);
    for i in 1..n {
        let id = IRVarId((2 * n + stmts.len()) as u32);
        stmts.push(Node::new(BIrStmt::And(acc, xn(i)), (), None));
        acc = id;
    }
    finish(2 * n, stmts, vec![acc])
}

/// A tiny circuit: AND-fold of n input bits, output 1 bit.
pub(crate) fn and_fold_circuit(n: usize) -> BIrBlocks {
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    let mut acc = IRVarId(0);
    for i in 1..n {
        let id = IRVarId((n + stmts.len()) as u32);
        stmts.push(Node::new(BIrStmt::And(acc, IRVarId(i as u32)), (), None));
        acc = id;
    }
    finish(n, stmts, vec![acc])
}

/// A tiny circuit: 1 iff the `hay_bytes`-byte input contains `needle` (a
/// compile-time constant) at any byte offset — OR over offsets of the
/// per-offset bit match (XNOR-with-constant folds to Not where the needle
/// bit is 0).
pub(crate) fn contains_bytes_circuit(hay_bytes: usize, needle: &[u8]) -> BIrBlocks {
    let nb = needle.len();
    assert!(nb > 0 && hay_bytes >= nb);
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    let n_in = hay_bytes * 8;
    let mut next_var = n_in as u32;
    let mut hits: Vec<u32> = Vec::new();
    for off in 0..=(hay_bytes - nb) {
        let mut acc: Option<u32> = None;
        for (j, &byte) in needle.iter().enumerate() {
            for b in 0..8 {
                let in_bit = ((off + j) * 8 + b) as u32;
                let matched = if (byte >> b) & 1 == 1 {
                    in_bit
                } else {
                    stmts.push(Node::new(BIrStmt::Not(IRVarId(in_bit)), (), None));
                    let v = next_var;
                    next_var += 1;
                    v
                };
                acc = Some(match acc {
                    None => matched,
                    Some(a) => {
                        stmts.push(Node::new(
                            BIrStmt::And(IRVarId(a), IRVarId(matched)),
                            (),
                            None,
                        ));
                        let v = next_var;
                        next_var += 1;
                        v
                    }
                });
            }
        }
        hits.push(acc.expect("nonempty needle"));
    }
    let mut acc = hits[0];
    for &h in &hits[1..] {
        stmts.push(Node::new(BIrStmt::Or(IRVarId(acc), IRVarId(h)), (), None));
        let v = next_var;
        next_var += 1;
        acc = v;
    }
    finish(n_in, stmts, vec![IRVarId(acc)])
}

/// The shared session script: byte lengths and the request-body template.
/// Both parties build identical circuits from it (circuits depend only on
/// lengths, never on secret values).
#[derive(Clone)]
pub struct TurnstileTlsScript {
    /// ClientHello handshake-message length.
    pub ch_len: usize,
    /// ServerHello length.
    pub sh_len: usize,
    /// The server flight record length (5 header + ct + 16 tag).
    pub flight_len: usize,
    /// Request body bytes before the Turnstile secret.
    pub req_prefix: Vec<u8>,
    /// Between secret and token.
    pub req_mid: Vec<u8>,
    /// After the token.
    pub req_suffix: Vec<u8>,
    /// Turnstile secret byte length.
    pub secret_len: usize,
    /// Turnstile token byte length.
    pub token_len: usize,
    /// The siteverify response record length.
    pub response_len: usize,
    /// The success marker searched in the decrypted response body.
    pub success_marker: Vec<u8>,
}

/// Role-specific secret inputs.
pub enum TurnstileTlsSecrets {
    /// The site's server (garbler): the Turnstile secret.
    Garbler {
        /// The Turnstile secret key bytes.
        secret: Vec<u8>,
    },
    /// The client (evaluator): wire bytes and its private inputs.
    Evaluator {
        /// ClientHello handshake-message bytes.
        ch: Vec<u8>,
        /// ServerHello bytes.
        sh: Vec<u8>,
        /// The server flight record bytes.
        flight: Vec<u8>,
        /// The X25519 shared secret (scripted stand-in for the
        /// joint-ephemeral ladder output).
        shared_secret: [u8; 32],
        /// The Turnstile token bytes.
        token: Vec<u8>,
        /// The siteverify response record bytes.
        response: Vec<u8>,
    },
}

/// What the session reveals: the two client-sent records (ciphertext + tag,
/// which the evaluator's host writes to its socket), and the held verdict
/// slot ids for the caller's correlation circuit.
pub struct TurnstileTlsOutcome {
    /// The client Finished record bits (revealed).
    pub client_finished_record: Vec<bool>,
    /// The siteverify request record bits (revealed).
    pub siteverify_request_record: Vec<bool>,
    /// Held slot of the server-flight tag verdict.
    pub vt_flight: usize,
    /// Held slot of the server Finished verdict.
    pub vf_server: usize,
    /// Held slot of the response tag verdict.
    pub vt_response: usize,
    /// Held slot of the success verdict.
    pub vs_success: usize,
}

/// One compiled HKDF-Expand-Label stage: the circuit plus the constant
/// HkdfLabel prefix the driver feeds before the context.
pub(crate) struct Expand {
    pub(crate) sched: GateSchedule,
    pub(crate) prefix: Vec<u8>,
    pub(crate) ctx_len: usize,
}

pub(crate) fn expand(out_len: u16, label: &[u8], ctx_len: usize) -> Expand {
    let (c, prefix) = expand_label_circuit(out_len, label, ctx_len);
    Expand {
        sched: crate::compile_schedule(&c).expect("expand label schedules"),
        prefix,
        ctx_len,
    }
}

pub(crate) fn sched(c: &BIrBlocks) -> GateSchedule {
    crate::compile_schedule_optimized(c).expect("TLS chain circuit schedules")
}

/// Pre-compiled session circuits (identical on both parties).
pub(crate) struct Circuits {
    pub(crate) transcript1: GateSchedule,
    pub(crate) early: GateSchedule,
    pub(crate) derived: Expand,
    pub(crate) hs: GateSchedule,
    pub(crate) c_hs: Expand,
    pub(crate) s_hs: Expand,
    pub(crate) c_key: Expand,
    pub(crate) c_iv: Expand,
    pub(crate) s_key: Expand,
    pub(crate) s_iv: Expand,
    pub(crate) open_flight: GateSchedule,
    pub(crate) eq_tag: GateSchedule,
    pub(crate) transcript2: GateSchedule,
    pub(crate) fk_s: Expand,
    pub(crate) server_fin: GateSchedule,
    pub(crate) eq_fin: GateSchedule,
    pub(crate) transcript3: GateSchedule,
    pub(crate) derived2: Expand,
    pub(crate) master: GateSchedule,
    pub(crate) c_ap: Expand,
    pub(crate) s_ap: Expand,
    pub(crate) c_ap_key: Expand,
    pub(crate) c_ap_iv: Expand,
    pub(crate) s_ap_key: Expand,
    pub(crate) s_ap_iv: Expand,
    pub(crate) cfk: Expand,
    pub(crate) client_fin: GateSchedule,
    pub(crate) seal_fin: GateSchedule,
    pub(crate) seal_req: GateSchedule,
    pub(crate) open_resp: GateSchedule,
    pub(crate) eq_rtag: GateSchedule,
    pub(crate) success: GateSchedule,
}

impl TurnstileTlsScript {
    /// The inner plaintext length of the flight record (payload + content
    /// type).
    pub(crate) fn inner_len(&self) -> usize {
        self.flight_len - 5 - 16
    }

    pub(crate) fn resp_inner_len(&self) -> usize {
        self.response_len - 5 - 16
    }

    pub(crate) fn body_len(&self) -> usize {
        self.req_prefix.len()
            + self.secret_len
            + self.req_mid.len()
            + self.token_len
            + self.req_suffix.len()
    }

    pub(crate) fn circuits(&self) -> Circuits {
        let inner = self.inner_len();
        Circuits {
            transcript1: sched(&transcript_circuit(self.ch_len + self.sh_len)),
            early: sched(&extract_circuit(32, 32)),
            derived: expand(32, b"derived", 32),
            hs: sched(&extract_circuit(32, 32)),
            c_hs: expand(32, b"c hs traffic", 32),
            s_hs: expand(32, b"s hs traffic", 32),
            c_key: expand(16, b"key", 0),
            c_iv: expand(12, b"iv", 0),
            s_key: expand(16, b"key", 0),
            s_iv: expand(12, b"iv", 0),
            open_flight: sched(&record_open_circuit(inner)),
            eq_tag: sched(&eq_bits_circuit(128)),
            transcript2: sched(&transcript_circuit(self.ch_len + self.sh_len + inner - 37)),
            fk_s: expand(32, b"finished", 0),
            server_fin: sched(&build_hmac_sha256(32, 32)),
            eq_fin: sched(&eq_bits_circuit(256)),
            transcript3: sched(&transcript_circuit(self.ch_len + self.sh_len + inner - 1)),
            derived2: expand(32, b"derived", 32),
            master: sched(&extract_circuit(32, 32)),
            c_ap: expand(32, b"c ap traffic", 32),
            s_ap: expand(32, b"s ap traffic", 32),
            c_ap_key: expand(16, b"key", 0),
            c_ap_iv: expand(12, b"iv", 0),
            s_ap_key: expand(16, b"key", 0),
            s_ap_iv: expand(12, b"iv", 0),
            cfk: expand(32, b"finished", 0),
            client_fin: sched(&build_hmac_sha256(32, 32)),
            seal_fin: sched(&record_seal_circuit(37)),
            seal_req: sched(&record_seal_circuit(self.body_len() + 1)),
            open_resp: sched(&record_open_circuit(self.resp_inner_len())),
            eq_rtag: sched(&eq_bits_circuit(128)),
            success: sched(&contains_bytes_circuit(
                self.resp_inner_len(),
                &self.success_marker,
            )),
        }
    }
}

/// Run the scripted Turnstile-TLS session as a strict chain. Both parties
/// execute this same function with their role's [`ChainParty`] driver and
/// [`TurnstileTlsSecrets`]; the returned verdict slots are the correlation
/// points (the caller ANDs them with e.g. the licensing predicate's
/// threaded verdict in a final [`correlation_circuit`] round).
pub fn run_turnstile_tls_session<N, D, C, T>(
    chain: &mut C,
    script: &TurnstileTlsScript,
    secrets: &TurnstileTlsSecrets,
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
) -> Result<TurnstileTlsOutcome, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
    C: ChainParty<N>,
    T: Transport,
{
    let circuits = script.circuits();
    let inner = script.inner_len();
    let resp_inner = script.resp_inner_len();
    let empty_hash: [u8; 32] = [
        0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f,
        0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b,
        0x78, 0x52, 0xb8, 0x55,
    ];

    let (ch, sh, flight, shared_secret, token, response, gsecret): (
        &[u8],
        &[u8],
        &[u8],
        &[u8],
        &[u8],
        &[u8],
        &[u8],
    ) = match secrets {
        TurnstileTlsSecrets::Garbler { secret } => {
            (&[], &[], &[], &[], &[], &[], secret.as_slice())
        }
        TurnstileTlsSecrets::Evaluator {
            ch,
            sh,
            flight,
            shared_secret,
            token,
            response,
        } => (
            ch.as_slice(),
            sh.as_slice(),
            flight.as_slice(),
            shared_secret.as_slice(),
            token.as_slice(),
            response.as_slice(),
            &[],
        ),
    };
    let ch_bits = bits_of(ch);
    let sh_bits = bits_of(sh);
    let flight_bits = bits_of(flight);
    let ss_bits = bits_of(shared_secret);
    let token_bits = bits_of(token);
    let resp_bits = bits_of(response);
    let gsecret_bits = bits_of(gsecret);

    /// Feed assembly: both parties build the identical `feeds` vector; the
    /// `consts`/`secrets` slices are consumed by the role that owns them.
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
            self.feeds.extend(held_feeds(base, n));
        }
        fn konst(&mut self, bits: &[bool]) {
            self.feeds
                .extend(core::iter::repeat_n(ChainFeed::Const, bits.len()));
            self.consts.extend_from_slice(bits);
        }
        /// `count` Eval feeds; the evaluator passes its `count` bits, the
        /// garbler an empty slice (the feed count is script-driven, not
        /// slice-driven — the non-owner has no value).
        fn eval(&mut self, count: usize, bits: &[bool]) {
            assert!(bits.len() == count || bits.is_empty());
            self.feeds.extend(core::iter::repeat_n(ChainFeed::Eval, count));
            self.secrets.extend_from_slice(bits);
        }
        /// `count` Garbler feeds; the garbler passes its `count` bits, the
        /// evaluator an empty slice.
        fn garb(&mut self, count: usize, bits: &[bool]) {
            assert!(bits.len() == count || bits.is_empty());
            self.feeds
                .extend(core::iter::repeat_n(ChainFeed::Garbler, count));
            self.secrets.extend_from_slice(bits);
        }
        /// One HKDF-Expand-Label stage: [secret(held) , prefix(const) ,
        /// ctx , 0x01(const)].
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

    // 1. Transcript(CH||SH) -> T_HASH.
    let mut a = Asm::new();
    a.eval(script.ch_len * 8, &ch_bits);
    a.eval(script.sh_len * 8, &sh_bits);
    let _ = round!(&circuits.transcript1, a, hold_range(slots::T_HASH, 256));

    // 2. Early secret = Extract(salt = 0^32, IKM = 0^32) — RFC 8446's
    // "0" is Hash.length zero BYTES, fed as public constants.
    let mut a = Asm::new();
    a.konst(&vec![false; 512]);
    let _ = round!(&circuits.early, a, hold_range(slots::EARLY, 256));

    // 3. derived = Expand-Label(early, "derived", empty_hash).
    let mut a = Asm::new();
    a.expand(&circuits.derived, slots::EARLY, Ctx::Const(&empty_hash));
    let _ = round!(
        &circuits.derived.sched,
        a,
        hold_range(slots::DERIVED, 256)
    );

    // 4. handshake secret = Extract(derived, shared secret).
    let mut a = Asm::new();
    a.held(slots::DERIVED, 256);
    a.eval(256, &ss_bits);
    let _ = round!(&circuits.hs, a, hold_range(slots::HS, 256));

    // 5-6. c/s hs traffic.
    let mut a = Asm::new();
    a.expand(&circuits.c_hs, slots::HS, Ctx::Held(slots::T_HASH));
    let _ = round!(&circuits.c_hs.sched, a, hold_range(slots::C_HS, 256));
    let mut a = Asm::new();
    a.expand(&circuits.s_hs, slots::HS, Ctx::Held(slots::T_HASH));
    let _ = round!(&circuits.s_hs.sched, a, hold_range(slots::S_HS, 256));

    // 7-10. Handshake keys/ivs.
    let mut a = Asm::new();
    a.expand(&circuits.c_key, slots::C_HS, Ctx::Const(&[]));
    let _ = round!(&circuits.c_key.sched, a, hold_range(slots::C_KEY, 256));
    let mut a = Asm::new();
    a.expand(&circuits.c_iv, slots::C_HS, Ctx::Const(&[]));
    let _ = round!(&circuits.c_iv.sched, a, hold_range(slots::C_IV, 256));
    let mut a = Asm::new();
    a.expand(&circuits.s_key, slots::S_HS, Ctx::Const(&[]));
    let _ = round!(&circuits.s_key.sched, a, hold_range(slots::S_KEY, 256));
    let mut a = Asm::new();
    a.expand(&circuits.s_iv, slots::S_HS, Ctx::Const(&[]));
    let _ = round!(&circuits.s_iv.sched, a, hold_range(slots::S_IV, 256));

    // 11. Open the server flight (seq 0: nonce == iv). Inputs: key, nonce,
    // aad(5), ct — the tag is NOT an input (compared in round 12).
    let mut a = Asm::new();
    a.held(slots::S_KEY, 128);
    a.held(slots::S_IV, 96);
    a.eval(40, take(&flight_bits, 0, 40));
    a.eval(inner * 8, take(&flight_bits, 40, inner * 8));
    let mut outs = hold_range(slots::INNER, inner * 8);
    outs.extend(hold_range(slots::S_TAG, 128));
    let _ = round!(&circuits.open_flight, a, outs);

    // 12. Server-flight tag verdict.
    let mut a = Asm::new();
    a.held(slots::S_TAG, 128);
    a.eval(128, take(&flight_bits, 40 + inner * 8, 128));
    let _ = round!(
        &circuits.eq_tag,
        a,
        vec![ChainOut::Hold(slots::VT_FLIGHT)]
    );

    // 13. Transcript through CertVerify (drop the 36-byte Finished message
    // and the inner content type).
    let mut a = Asm::new();
    a.eval(script.ch_len * 8, &ch_bits);
    a.eval(script.sh_len * 8, &sh_bits);
    a.held(slots::INNER, (inner - 37) * 8);
    let _ = round!(&circuits.transcript2, a, hold_range(slots::T2, 256));

    // 14-16. Server Finished verification: verify_data = HMAC(fk_s, t2),
    // compared against the last 32 bytes before the content type.
    let mut a = Asm::new();
    a.expand(&circuits.fk_s, slots::S_HS, Ctx::Const(&[]));
    let _ = round!(&circuits.fk_s.sched, a, hold_range(slots::FK_S, 256));
    let mut a = Asm::new();
    a.held(slots::FK_S, 256);
    a.held(slots::T2, 256);
    let _ = round!(
        &circuits.server_fin,
        a,
        hold_range(slots::SCRATCH, 256)
    );
    let mut a = Asm::new();
    a.held(slots::SCRATCH, 256);
    a.held(slots::INNER + (inner - 33) * 8, 256);
    let _ = round!(
        &circuits.eq_fin,
        a,
        vec![ChainOut::Hold(slots::VF_SERVER)]
    );

    // 17. Transcript through the server Finished (drop the content type).
    let mut a = Asm::new();
    a.eval(script.ch_len * 8, &ch_bits);
    a.eval(script.sh_len * 8, &sh_bits);
    a.held(slots::INNER, (inner - 1) * 8);
    let _ = round!(&circuits.transcript3, a, hold_range(slots::T_TH, 256));

    // 18-19. Master secret.
    let mut a = Asm::new();
    a.expand(&circuits.derived2, slots::HS, Ctx::Const(&empty_hash));
    let _ = round!(&circuits.derived2.sched, a, hold_range(slots::D2, 256));
    let mut a = Asm::new();
    a.held(slots::D2, 256);
    a.konst(&vec![false; 256]);
    let _ = round!(&circuits.master, a, hold_range(slots::MASTER, 256));

    // 20-25. Application secrets, keys, ivs.
    let mut a = Asm::new();
    a.expand(&circuits.c_ap, slots::MASTER, Ctx::Held(slots::T_TH));
    let _ = round!(&circuits.c_ap.sched, a, hold_range(slots::C_AP, 256));
    let mut a = Asm::new();
    a.expand(&circuits.s_ap, slots::MASTER, Ctx::Held(slots::T_TH));
    let _ = round!(&circuits.s_ap.sched, a, hold_range(slots::S_AP, 256));
    let mut a = Asm::new();
    a.expand(&circuits.c_ap_key, slots::C_AP, Ctx::Const(&[]));
    let _ = round!(
        &circuits.c_ap_key.sched,
        a,
        hold_range(slots::C_AP_KEY, 256)
    );
    let mut a = Asm::new();
    a.expand(&circuits.c_ap_iv, slots::C_AP, Ctx::Const(&[]));
    let _ = round!(
        &circuits.c_ap_iv.sched,
        a,
        hold_range(slots::C_AP_IV, 256)
    );
    let mut a = Asm::new();
    a.expand(&circuits.s_ap_key, slots::S_AP, Ctx::Const(&[]));
    let _ = round!(
        &circuits.s_ap_key.sched,
        a,
        hold_range(slots::S_AP_KEY, 256)
    );
    let mut a = Asm::new();
    a.expand(&circuits.s_ap_iv, slots::S_AP, Ctx::Const(&[]));
    let _ = round!(
        &circuits.s_ap_iv.sched,
        a,
        hold_range(slots::S_AP_IV, 256)
    );

    // 26-28. Seal the client Finished: the Finished handshake message
    // (0x14 00 00 20 || verify_data) plus the inner content type.
    let mut a = Asm::new();
    a.expand(&circuits.cfk, slots::C_HS, Ctx::Const(&[]));
    let _ = round!(&circuits.cfk.sched, a, hold_range(slots::CFK, 256));
    // The client Finished's verify_data covers the transcript through the
    // SERVER Finished (RFC 8446 section 4.4.4) — T_TH, not T2.
    let mut a = Asm::new();
    a.held(slots::CFK, 256);
    a.held(slots::T_TH, 256);
    let _ = round!(&circuits.client_fin, a, hold_range(slots::VFD, 256));
    let mut a = Asm::new();
    a.held(slots::C_KEY, 128);
    a.held(slots::C_IV, 96);
    a.konst(&bits_of(&[0x17u8, 0x03, 0x03, 0x00, 0x35]));
    a.konst(&bits_of(&[0x14u8, 0x00, 0x00, 0x20]));
    a.held(slots::VFD, 256);
    a.konst(&bits_of(&[0x16u8]));
    let cf_record = round!(&circuits.seal_fin, a, vec![ChainOut::Reveal; 37 * 8 + 128]);

    // 29. Seal the siteverify request under the application keys.
    let mut a = Asm::new();
    a.held(slots::C_AP_KEY, 128);
    a.held(slots::C_AP_IV, 96);
    let body_len = script.body_len();
    // The inner plaintext is the request body plus the 0x17
    // application-data content-type byte.
    let pt_len = body_len + 1;
    let aad = [
        0x17u8,
        0x03,
        0x03,
        ((pt_len + 16) >> 8) as u8,
        (pt_len + 16) as u8,
    ];
    a.konst(&bits_of(&aad));
    a.konst(&bits_of(&script.req_prefix));
    a.garb(script.secret_len * 8, &gsecret_bits);
    a.konst(&bits_of(&script.req_mid));
    a.eval(script.token_len * 8, &token_bits);
    a.konst(&bits_of(&script.req_suffix));
    a.konst(&bits_of(&[0x17u8]));
    let req_record = round!(
        &circuits.seal_req,
        a,
        vec![ChainOut::Reveal; pt_len * 8 + 128]
    );

    // 30. Open the siteverify response.
    let mut a = Asm::new();
    a.held(slots::S_AP_KEY, 128);
    a.held(slots::S_AP_IV, 96);
    a.eval(40, take(&resp_bits, 0, 40));
    a.eval(resp_inner * 8, take(&resp_bits, 40, resp_inner * 8));
    let mut outs = hold_range(slots::RESP, resp_inner * 8);
    outs.extend(hold_range(slots::R_TAG, 128));
    let _ = round!(&circuits.open_resp, a, outs);

    // 31. Response tag verdict.
    let mut a = Asm::new();
    a.held(slots::R_TAG, 128);
    a.eval(128, take(&resp_bits, 40 + resp_inner * 8, 128));
    let _ = round!(
        &circuits.eq_rtag,
        a,
        vec![ChainOut::Hold(slots::VT_RESP)]
    );

    // 32. Success extraction over the decrypted body.
    let mut a = Asm::new();
    a.held(slots::RESP, resp_inner * 8);
    let _ = round!(
        &circuits.success,
        a,
        vec![ChainOut::Hold(slots::VS_SUCCESS)]
    );

    Ok(TurnstileTlsOutcome {
        client_finished_record: cf_record,
        siteverify_request_record: req_record,
        vt_flight: slots::VT_FLIGHT,
        vf_server: slots::VF_SERVER,
        vt_response: slots::VT_RESP,
        vs_success: slots::VS_SUCCESS,
    })
}

/// The caller's final correlation round: an AND-fold of `n` threaded
/// verdict bits, revealed — binding every check in the chain into one
/// verdict that cannot be replayed or swapped independently.
pub fn correlation_circuit(n: usize) -> BIrBlocks {
    and_fold_circuit(n)
}
