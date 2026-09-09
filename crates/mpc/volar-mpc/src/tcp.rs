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
        assert_eq!(self.role, OtRole::Receiver, "only the evaluator receives OTs");
        let s_msg = self.transport.recv();
        let (receiver, r_msg) =
            crate::ot::CoReceiver::<N>::setup_dyn(self.rng, &s_msg, bit).expect("OT receiver setup");
        self.transport.send(&r_msg);
        let frame = self.transport.recv();
        receiver.finish(&frame).expect("OT receiver finish")
    }
}
