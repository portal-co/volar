// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Oblivious-transfer channels for evaluator-input label delivery.
//!
//! The session layer ([`crate::OtChannel`]) is generic over the OT scheme.
//! This module ships:
//!
//! - [`LoopbackOt`]: a deterministic in-process OT for tests and local
//!   simulation. The sender queues both labels; the receiver pops the one
//!   matching its choice bit. It provides **no cryptographic privacy** — it
//!   exists so `volar_channel::run_protocol`-style in-memory tests and the
//!   reference driver can exercise the full message flow without sockets or a
//!   real OT stack.
//!
//! Production wiring replaces `LoopbackOt` with a channel backed by
//! `volar-spec`'s OT stack (Chou–Orlandi base OT + IKNP/Ferret extension);
//! the session logic is unchanged.

use alloc::collections::VecDeque;
use alloc::vec::Vec;

use hybrid_array::Array;
use volar_spec::vole::VoleArray;

use crate::OtChannel;

/// A deterministic in-process 1-of-2 OT for tests.
///
/// The garbler (sender) pushes label pairs; the evaluator (receiver) pops the
/// label for its choice bit. Both halves share one queue, so this is only
/// meaningful inside a single process driving both roles — exactly the
/// in-memory test harness shape.
///
/// Privacy: none. The sender technically "sees" both labels and the receiver
/// could see both too; it is a test double, not a cryptographic OT.
#[derive(Default)]
pub struct LoopbackOt<N: VoleArray<u8>> {
    queue: VecDeque<[Array<u8, N>; 2]>,
}

impl<N: VoleArray<u8>> LoopbackOt<N> {
    /// Create an empty loopback OT.
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    /// Number of unclaimed label pairs (diagnostic for tests).
    pub fn pending(&self) -> usize {
        self.queue.len()
    }
}

impl<N: VoleArray<u8>> OtChannel<N> for LoopbackOt<N> {
    fn send(&mut self, labels: [&Array<u8, N>; 2]) {
        self.queue
            .push_back([labels[0].clone(), labels[1].clone()]);
    }

    fn receive(&mut self, bit: bool) -> Array<u8, N> {
        let pair = self
            .queue
            .pop_front()
            .expect("LoopbackOt: receive called before send");
        pair[bit as usize].clone()
    }
}

/// A recording OT wrapper: passes through to an inner channel while logging
/// every label the sender offered and every choice the receiver made.
///
/// Used by the mutual-privacy test to assert (structurally) that evaluator
/// inputs only ever traverse the channel as OT label pairs — never as
/// plaintext bits — and vice versa for garbler inputs.
pub struct RecordingOt<'a, N: VoleArray<u8>, C: OtChannel<N>> {
    inner: &'a mut C,
    /// Every label pair the sender offered (both labels, in order).
    pub offered: Vec<[Array<u8, N>; 2]>,
    /// Every choice bit the receiver made.
    pub choices: Vec<bool>,
}

impl<'a, N: VoleArray<u8>, C: OtChannel<N>> RecordingOt<'a, N, C> {
    pub fn new(inner: &'a mut C) -> Self {
        Self {
            inner,
            offered: Vec::new(),
            choices: Vec::new(),
        }
    }
}

impl<N: VoleArray<u8>, C: OtChannel<N>> OtChannel<N> for RecordingOt<'_, N, C> {
    fn send(&mut self, labels: [&Array<u8, N>; 2]) {
        self.offered.push([labels[0].clone(), labels[1].clone()]);
        self.inner.send(labels);
    }

    fn receive(&mut self, bit: bool) -> Array<u8, N> {
        self.choices.push(bit);
        self.inner.receive(bit)
    }
}
