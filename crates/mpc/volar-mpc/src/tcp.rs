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

/// An OT byte channel over the same framed TCP stream.
///
/// The Chou–Orlandi step machines exchange raw byte messages; this adapts a
/// `&mut TcpTransport` to the send/recv shape they need by framing each OT
/// message exactly like a session frame. Use one per role; within a single OT
/// the sender only sends and the receiver sends once then receives, so one
/// socket carries both directions without confusion (the session's strict
/// ordering keeps the two roles' OT messages from interleaving mid-OT).
pub struct OtTcp<'a> {
    transport: &'a mut TcpTransport,
}

impl<'a> OtTcp<'a> {
    pub fn new(transport: &'a mut TcpTransport) -> Self {
        Self { transport }
    }
}

impl OtTcp<'_> {
    /// Send one OT message.
    pub fn send(&mut self, msg: &[u8]) {
        self.transport.send(msg);
    }
    /// Receive one OT message.
    pub fn recv(&mut self) -> Vec<u8> {
        self.transport.recv()
    }
}

/// Run one 1-of-2 OT as the sender over TCP.
pub fn ot_send_tcp<N: volar_spec::vole::VoleArray<u8>>(
    io: &mut OtTcp,
    labels: [&hybrid_array::Array<u8, N>; 2],
    rng: &mut dyn volar_spec::SpecRng,
) -> Result<(), crate::ot::OtError> {
    let (sender, s_msg) = crate::ot::CoSender::<N>::setup_dyn(rng);
    io.send(&s_msg);
    let r_msg = io.recv();
    let frame = sender.finish(&r_msg, labels)?;
    io.send(&frame);
    Ok(())
}

/// Run one 1-of-2 OT as the receiver over TCP for choice `bit`.
pub fn ot_receive_tcp<N: volar_spec::vole::VoleArray<u8>>(
    io: &mut OtTcp,
    bit: bool,
    rng: &mut dyn volar_spec::SpecRng,
) -> Result<hybrid_array::Array<u8, N>, crate::ot::OtError> {
    let s_msg = io.recv();
    let (receiver, r_msg) = crate::ot::CoReceiver::<N>::setup_dyn(rng, &s_msg, bit)?;
    io.send(&r_msg);
    let frame = io.recv();
    receiver.finish(&frame)
}
