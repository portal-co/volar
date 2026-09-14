// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Framed-TCP transport for the two-party session (the `std` feature).
//!
//! [`TcpTransport`] implements the session [`crate::Transport`] over a
//! blocking `TcpStream`, using the same length-prefixed framing the
//! [`crate::SessionFrame`] encoder emits (one `u32` little-endian length
//! prefix per frame). It lets the garbler and evaluator run as **separate
//! processes** — the real "test MPC over the network" path, mirroring how the
//! ZK C-interaction harness splits prover/verifier.
//!
//! The OT phase rides the same stream: [`ot_io`] adapts a `TcpTransport` to
//! the byte-channel shape the Chou–Orlandi step machines
//! ([`crate::ot::CoSender`]/[`crate::ot::CoReceiver`]) consume.

extern crate std;

use std::io::{Read, Write};
use std::net::TcpStream;
use std::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use sha2::Sha256;
use volar_spec::ot::ferret::FerretParams;
use volar_spec::ot::ferret::pool::{CotPoolReceiver, CotPoolSender};
use volar_spec::ot::two_party::{
    StackIo, stack_bea95_receiver, stack_bea95_sender, stack_setup_receiver,
    stack_setup_receiver_malicious, stack_setup_sender, stack_setup_sender_malicious,
};

use crate::Transport;

/// A framed, blocking-TCP [`Transport`].
///
/// Frames are `u32` little-endian length-prefixed byte strings, matching
/// [`crate::SessionFrame::encode`]'s framing expectations. `send`/`recv` map
/// one-to-one onto framed writes/reads, so the strict request/response
/// session ordering (setup → owned inputs → OTs → verdict) never deadlocks on
/// a single socket.
pub struct TcpTransport {
    stream: TcpStream,
}

impl TcpTransport {
    /// Wrap an established, blocking stream.
    pub fn new(stream: TcpStream) -> Self {
        Self { stream }
    }

    /// Connect to a listening peer at `addr`.
    pub fn connect(addr: &str) -> std::io::Result<Self> {
        Ok(Self::new(TcpStream::connect(addr)?))
    }

    /// Accept one inbound connection on a bound listener.
    pub fn accept(listener: &std::net::TcpListener) -> std::io::Result<Self> {
        let (stream, _) = listener.accept()?;
        Ok(Self::new(stream))
    }

    /// Borrow the underlying stream (e.g. to set timeouts).
    pub fn stream(&mut self) -> &mut TcpStream {
        &mut self.stream
    }

    /// Create a second, independent handle to the same connection.
    ///
    /// The two handles share the socket; because the session protocol is
    /// strictly request/response (setup → owned inputs → OTs → verdict) and
    /// both halves run on one logical role, the handles never race. This lets
    /// a caller hold one handle as the session [`Transport`] and another
    /// inside the OT channel, satisfying the borrow checker.
    pub fn try_clone(&self) -> std::io::Result<Self> {
        Ok(Self::new(self.stream.try_clone()?))
    }
}

fn read_exact_n(stream: &mut TcpStream, n: usize) -> std::io::Result<Vec<u8>> {
    let mut buf = alloc::vec![0u8; n];
    stream.read_exact(&mut buf)?;
    Ok(buf)
}

impl Transport for TcpTransport {
    fn send(&mut self, frame: &[u8]) {
        let len = (frame.len() as u32).to_le_bytes();
        self.stream
            .write_all(&len)
            .and_then(|()| self.stream.write_all(frame))
            .and_then(|()| self.stream.flush())
            .expect("TcpTransport: send failed");
    }

    fn recv(&mut self) -> Vec<u8> {
        let mut len = [0u8; 4];
        self.stream
            .read_exact(&mut len)
            .expect("TcpTransport: recv len failed");
        let n = u32::from_le_bytes(len) as usize;
        read_exact_n(&mut self.stream, n).expect("TcpTransport: recv body failed")
    }
}

/// An [`crate::OtChannel`] that runs the Chou–Orlandi OT over a framed
/// transport, for cross-process sessions.
///
/// One `NetOtChannel` is used per role: `Sender` for the garbler, `Receiver`
/// for the evaluator. Each `send`/`receive` call performs one full 1-of-2 OT
/// over the transport (setup → reply → masked labels), so the session's
/// per-input-bit OT loop runs unchanged — only the wire is real.
///
/// The channel *owns* the transport and re-exposes it via
/// [`NetOtChannel::transport`], so the caller drives both the session frames
/// and the OT phase over the single owned connection without a double borrow.
pub struct NetOtChannel<'a, T: Transport> {
    transport: T,
    role: OtRole,
    rng: &'a mut dyn volar_spec::SpecRng,
}

/// Which side of the OT this channel plays.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum OtRole {
    /// Garbler: offers the two labels.
    Sender,
    /// Evaluator: chooses one label.
    Receiver,
}

impl<'a, T: Transport> NetOtChannel<'a, T> {
    /// Create a channel for `role` over `transport`, drawing OT scheme
    /// randomness from `rng`.
    pub fn new(transport: T, role: OtRole, rng: &'a mut dyn volar_spec::SpecRng) -> Self {
        Self {
            transport,
            role,
            rng,
        }
    }

    /// Borrow the underlying transport for driving session frames.
    pub fn transport(&mut self) -> &mut T {
        &mut self.transport
    }

    /// Consume the channel and return the transport.
    pub fn into_transport(self) -> T {
        self.transport
    }
}

impl<N: volar_spec::vole::VoleArray<u8>, T: Transport> crate::OtChannel<N> for NetOtChannel<'_, T> {
    fn send(&mut self, labels: [&hybrid_array::Array<u8, N>; 2]) {
        assert_eq!(self.role, OtRole::Sender, "only the garbler sends OTs");
        let (sender, s_msg) = crate::ot::CoSender::<N>::setup_dyn(self.rng);
        self.transport.send(&s_msg);
        let r_msg = self.transport.recv();
        let frame = sender.finish(&r_msg, labels).expect("OT sender finish");
        self.transport.send(&frame);
    }

    fn receive(&mut self, bit: bool) -> hybrid_array::Array<u8, N> {
        assert_eq!(
            self.role,
            OtRole::Receiver,
            "only the evaluator receives OTs"
        );
        let s_msg = self.transport.recv();
        let (receiver, r_msg) = crate::ot::CoReceiver::<N>::setup_dyn(self.rng, &s_msg, bit)
            .expect("OT receiver setup");
        self.transport.send(&r_msg);
        let frame = self.transport.recv();
        receiver.finish(&frame).expect("OT receiver finish")
    }
}

// ============================================================================
// Ferret chosen-bit OT channel (std)

/// Public transport accounting for one Ferret-backed OT role. Counts include
/// the tagged Ferret stack frames and label-mask frames sent or received by
/// this role, but never their contents.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct FerretOtMetrics {
    pub sent_frames: u64,
    pub sent_bytes: u64,
    pub received_frames: u64,
    pub received_bytes: u64,
}

struct FerretIo<'a, T: Transport> {
    transport: &'a mut T,
    metrics: &'a mut FerretOtMetrics,
}

impl<T: Transport> StackIo for FerretIo<'_, T> {
    fn send(&mut self, tag: u8, payload: &[u8]) {
        let mut frame = Vec::with_capacity(payload.len() + 1);
        frame.push(tag);
        frame.extend_from_slice(payload);
        self.metrics.sent_frames += 1;
        self.metrics.sent_bytes += frame.len() as u64;
        self.transport.send(&frame);
    }

    fn recv(&mut self, expected_tag: u8) -> Vec<u8> {
        let frame = self.transport.recv();
        self.metrics.received_frames += 1;
        self.metrics.received_bytes += frame.len() as u64;
        assert!(
            !frame.is_empty() && frame[0] == expected_tag,
            "unexpected Ferret stack frame"
        );
        frame[1..].to_vec()
    }
}

struct RngRef<'a>(&'a mut dyn volar_spec::SpecRng);

impl volar_spec::SpecRng for RngRef<'_> {
    fn next_u32(&mut self) -> u32 {
        self.0.next_u32()
    }
}

/// Ferret-Reg extension backed 1-of-2 OT over a framed transport.
///
/// The channel bootstraps the repository's LWE → SoftSpoken → Ferret stack at
/// construction. Each [`crate::OtChannel`] call converts one buffered random
/// COT with Bea95 and masks the offered pair using SHA-256 domain-separated
/// pads. The receiver gets exactly one pad, selected by its private bit.
///
/// `params` must be a production Ferret parameter set in deployments;
/// `FERRET_REG_TOY` is appropriate only for tests.
pub struct FerretOtChannel<'a, T: Transport> {
    transport: T,
    role: OtRole,
    rng: &'a mut dyn volar_spec::SpecRng,
    sender: Option<CotPoolSender>,
    receiver: Option<CotPoolReceiver>,
    metrics: FerretOtMetrics,
}

impl<'a, T: Transport> FerretOtChannel<'a, T> {
    /// Construct a semi-honest Ferret-Reg channel and run its one-time setup.
    pub fn new(
        mut transport: T,
        role: OtRole,
        rng: &'a mut dyn volar_spec::SpecRng,
        params: FerretParams,
    ) -> Self {
        let mut metrics = FerretOtMetrics::default();
        let (sender, receiver) = {
            let mut io = FerretIo {
                transport: &mut transport,
                metrics: &mut metrics,
            };
            match role {
                OtRole::Sender => {
                    let mut rng = RngRef(rng);
                    (Some(stack_setup_sender(&mut rng, params, &mut io)), None)
                }
                OtRole::Receiver => {
                    let mut rng = RngRef(rng);
                    (None, Some(stack_setup_receiver(&mut rng, params, &mut io)))
                }
            }
        };
        Self {
            transport,
            role,
            rng,
            sender,
            receiver,
            metrics,
        }
    }

    /// Construct a Ferret channel whose refills use the stack's batched SPCOT
    /// consistency check. This does not make garbling malicious-secure by
    /// itself; it authenticates the Ferret COT extension only.
    pub fn new_malicious(
        mut transport: T,
        role: OtRole,
        rng: &'a mut dyn volar_spec::SpecRng,
        params: FerretParams,
    ) -> Self {
        let mut metrics = FerretOtMetrics::default();
        let (sender, receiver) = {
            let mut io = FerretIo {
                transport: &mut transport,
                metrics: &mut metrics,
            };
            match role {
                OtRole::Sender => {
                    let mut rng = RngRef(rng);
                    (
                        Some(stack_setup_sender_malicious(&mut rng, params, &mut io)),
                        None,
                    )
                }
                OtRole::Receiver => {
                    let mut rng = RngRef(rng);
                    (
                        None,
                        Some(stack_setup_receiver_malicious(&mut rng, params, &mut io)),
                    )
                }
            }
        };
        Self {
            transport,
            role,
            rng,
            sender,
            receiver,
            metrics,
        }
    }

    pub fn transport(&mut self) -> &mut T {
        &mut self.transport
    }

    pub fn metrics(&self) -> FerretOtMetrics {
        self.metrics
    }

    pub fn into_transport(self) -> T {
        self.transport
    }

    fn send_masked(&mut self, frame: &[u8]) {
        self.metrics.sent_frames += 1;
        self.metrics.sent_bytes += frame.len() as u64;
        self.transport.send(frame);
    }

    fn receive_masked(&mut self) -> Vec<u8> {
        let frame = self.transport.recv();
        self.metrics.received_frames += 1;
        self.metrics.received_bytes += frame.len() as u64;
        frame
    }
}

fn ferret_pad<N: volar_spec::vole::VoleArray<u8>>(block: &[u8; 16], choice: bool) -> Array<u8, N> {
    let mut out = Vec::with_capacity(N::USIZE);
    let mut counter = 0u32;
    while out.len() < N::USIZE {
        let mut hash = Sha256::new();
        hash.update(b"volar-mpc/ferret-ot-pad-v1");
        hash.update(block);
        hash.update([choice as u8]);
        hash.update(counter.to_le_bytes());
        out.extend_from_slice(&hash.finalize());
        counter += 1;
    }
    Array::from_fn(|i| out[i])
}

impl<N: volar_spec::vole::VoleArray<u8>, T: Transport> crate::OtChannel<N>
    for FerretOtChannel<'_, T>
{
    fn send(&mut self, labels: [&Array<u8, N>; 2]) {
        assert_eq!(self.role, OtRole::Sender, "only the garbler sends OTs");
        let r0 = {
            let sender = self.sender.as_mut().expect("sender Ferret state");
            let mut io = FerretIo {
                transport: &mut self.transport,
                metrics: &mut self.metrics,
            };
            let mut rng = RngRef(self.rng);
            stack_bea95_sender(&mut rng, sender, &mut io)
        };
        let delta = self
            .sender
            .as_ref()
            .expect("sender Ferret state")
            .seed
            .delta;
        let pad0 = ferret_pad::<N>(&r0, false);
        let mut r1 = r0;
        for (byte, delta_byte) in r1.iter_mut().zip(delta) {
            *byte ^= delta_byte;
        }
        let pad1 = ferret_pad::<N>(&r1, true);
        let mut masked = Vec::with_capacity(N::USIZE * 2);
        masked.extend((0..N::USIZE).map(|i| labels[0][i] ^ pad0[i]));
        masked.extend((0..N::USIZE).map(|i| labels[1][i] ^ pad1[i]));
        self.send_masked(&masked);
    }

    fn receive(&mut self, bit: bool) -> Array<u8, N> {
        assert_eq!(
            self.role,
            OtRole::Receiver,
            "only the evaluator receives OTs"
        );
        let z = {
            let receiver = self.receiver.as_mut().expect("receiver Ferret state");
            let mut io = FerretIo {
                transport: &mut self.transport,
                metrics: &mut self.metrics,
            };
            let mut rng = RngRef(self.rng);
            stack_bea95_receiver(&mut rng, receiver, &mut io, bit)
        };
        let masked = self.receive_masked();
        assert_eq!(
            masked.len(),
            N::USIZE * 2,
            "malformed Ferret OT label frame"
        );
        let pad = ferret_pad::<N>(&z, bit);
        let start = if bit { N::USIZE } else { 0 };
        Array::from_fn(|i| masked[start + i] ^ pad[i])
    }
}

// ============================================================================
// ML-KEM OT channel (mlkem + std)
// ============================================================================

/// An [`crate::OtChannel`] that runs the ML-KEM-1024 1-of-2 OT over a framed
/// transport, for cross-process sessions (the post-quantum OT option).
///
/// Same ownership model as [`NetOtChannel`]. ML-KEM OT is receiver-initiated
/// and non-interactive (receiver sends its two encapsulation keys, sender
/// replies with the masked frame — no round trip), so it needs fewer messages
/// than Chou–Orlandi and avoids the blocking-receive-in-sender ordering
/// constraint.
#[cfg(feature = "mlkem")]
pub struct NetOtChannelMk<'a, T: Transport> {
    transport: T,
    role: OtRole,
    rng: &'a mut dyn volar_spec::SpecRng,
}

#[cfg(feature = "mlkem")]
impl<'a, T: Transport> NetOtChannelMk<'a, T> {
    /// Create an ML-KEM OT channel for `role` over `transport`.
    pub fn new(transport: T, role: OtRole, rng: &'a mut dyn volar_spec::SpecRng) -> Self {
        Self {
            transport,
            role,
            rng,
        }
    }

    /// Borrow the underlying transport for driving session frames.
    pub fn transport(&mut self) -> &mut T {
        &mut self.transport
    }
}

#[cfg(feature = "mlkem")]
impl<N: volar_spec::vole::VoleArray<u8>, T: Transport> crate::OtChannel<N>
    for NetOtChannelMk<'_, T>
{
    fn send(&mut self, labels: [&hybrid_array::Array<u8, N>; 2]) {
        assert_eq!(self.role, OtRole::Sender, "only the garbler sends OTs");
        // Receiver-initiated: first read the two encapsulation keys.
        let eks = self.transport.recv();
        let frame =
            crate::ot_mlkem::MkSender::finish::<N>(self.rng, &eks, labels).expect("ML-KEM sender");
        self.transport.send(&frame);
    }

    fn receive(&mut self, bit: bool) -> hybrid_array::Array<u8, N> {
        assert_eq!(
            self.role,
            OtRole::Receiver,
            "only the evaluator receives OTs"
        );
        let (receiver, eks) = crate::ot_mlkem::MkReceiver::setup(self.rng, bit);
        self.transport.send(&eks);
        let frame = self.transport.recv();
        receiver.finish::<N>(&frame).expect("ML-KEM receiver")
    }
}
