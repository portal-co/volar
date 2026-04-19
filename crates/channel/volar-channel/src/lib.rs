// @reliability: experimental
// @ai: assisted
//
//! Packet-based protocol abstraction for Volar.
//!
//! Provides a generic pure-function state-machine pattern for interactive
//! protocols. The core abstraction is [`Protocol`]: a trait where each step
//! takes `(State, Incoming)` and produces `(State', Yield<Done, Outgoing>)`.
//!
//! State is passed by value (not `&mut self`) so it can be serialized for
//! checkpointing, transferred to another machine, or compiled to a circuit
//! for ZK nesting.
//!
//! # Design
//!
//! - **Transport-agnostic**: no `Transport` trait. The caller decides how
//!   messages move (TCP, shared memory, FHE ciphertext, VOLE commitment).
//! - **Deterministic**: no hidden state, no randomness. RNG is passed in
//!   via the protocol's `State` or `Incoming` types if needed.
//! - **`no_std + alloc`**: consistent with the rest of the Volar codebase.

#![no_std]

extern crate alloc;

/// Result of a protocol step.
///
/// A protocol step either completes with a final value ([`Yield::Done`]) or
/// sends a message and waits for a response ([`Yield::Send`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Yield<Done, Msg> {
    /// Protocol completed with this result.
    Done(Done),
    /// Send this message to the counterparty; the caller will invoke
    /// [`Protocol::step`] again with the response.
    Send(Msg),
}

impl<Done, Msg> Yield<Done, Msg> {
    /// Returns `true` if the protocol is done.
    pub fn is_done(&self) -> bool {
        matches!(self, Yield::Done(_))
    }

    /// Returns `true` if the protocol wants to send a message.
    pub fn is_send(&self) -> bool {
        matches!(self, Yield::Send(_))
    }

    /// Unwrap the `Done` value, panicking if the protocol wants to send.
    pub fn unwrap_done(self) -> Done {
        match self {
            Yield::Done(d) => d,
            Yield::Send(_) => panic!("called unwrap_done on Yield::Send"),
        }
    }

    /// Unwrap the `Send` value, panicking if the protocol is done.
    pub fn unwrap_send(self) -> Msg {
        match self {
            Yield::Send(m) => m,
            Yield::Done(_) => panic!("called unwrap_send on Yield::Done"),
        }
    }

    /// Map the `Done` value.
    pub fn map_done<F, D2>(self, f: F) -> Yield<D2, Msg>
    where
        F: FnOnce(Done) -> D2,
    {
        match self {
            Yield::Done(d) => Yield::Done(f(d)),
            Yield::Send(m) => Yield::Send(m),
        }
    }

    /// Map the `Send` value.
    pub fn map_send<F, M2>(self, f: F) -> Yield<Done, M2>
    where
        F: FnOnce(Msg) -> M2,
    {
        match self {
            Yield::Done(d) => Yield::Done(d),
            Yield::Send(m) => Yield::Send(f(m)),
        }
    }
}

/// A pure-function protocol with serializable state.
///
/// Each protocol is a state machine driven by incoming messages. The state is
/// passed by value so it can be serialized (e.g. via rkyv) for checkpointing
/// or circuit compilation.
///
/// # Lifecycle
///
/// 1. Call [`Protocol::init`] with initial parameters (encoded in `State`).
///    This returns `(State', Yield)`.
/// 2. If `Yield::Send(msg)`, deliver `msg` to the counterparty and obtain a
///    response.
/// 3. Call [`Protocol::step`] with the updated state and the response.
/// 4. Repeat from step 2 until `Yield::Done(result)`.
///
/// # Example
///
/// ```rust,ignore
/// let (mut state, mut action) = MyProtocol::init(initial_state);
/// loop {
///     match action {
///         Yield::Done(result) => break result,
///         Yield::Send(msg) => {
///             let response = transport.send_and_receive(msg);
///             let (s, a) = MyProtocol::step(state, response);
///             state = s;
///             action = a;
///         }
///     }
/// }
/// ```
pub trait Protocol {
    /// The full serializable state of this protocol.
    type State;

    /// Message type received from the counterparty.
    type Incoming;

    /// Message type sent to the counterparty.
    type Outgoing;

    /// Final result when the protocol completes.
    type Done;

    /// Start the protocol with initial parameters.
    ///
    /// The initial parameters are encoded in `State` (e.g. the access
    /// operation to perform). Returns the initial state and the first
    /// action (either `Send` a message or `Done` immediately).
    fn init(params: Self::State) -> (Self::State, Yield<Self::Done, Self::Outgoing>);

    /// Process one incoming message and advance the state.
    fn step(
        state: Self::State,
        msg: Self::Incoming,
    ) -> (Self::State, Yield<Self::Done, Self::Outgoing>);
}

/// Drive a protocol to completion using a closure for message exchange.
///
/// This is a convenience function for testing and local execution.
/// The `exchange` closure sends an outgoing message and returns the
/// incoming response.
pub fn run_protocol<P, F>(params: P::State, mut exchange: F) -> (P::State, P::Done)
where
    P: Protocol,
    F: FnMut(P::Outgoing) -> P::Incoming,
{
    let (mut state, mut action) = P::init(params);
    loop {
        match action {
            Yield::Done(result) => return (state, result),
            Yield::Send(msg) => {
                let response = exchange(msg);
                let (s, a) = P::step(state, response);
                state = s;
                action = a;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    /// Trivial echo protocol for testing: sends one message, receives it
    /// back, and completes with the echoed value.
    struct EchoProtocol;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum EchoStep {
        Init(u64),
        WaitEcho,
    }

    impl Protocol for EchoProtocol {
        type State = EchoStep;
        type Incoming = u64;
        type Outgoing = u64;
        type Done = u64;

        fn init(params: Self::State) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
            match params {
                EchoStep::Init(val) => (EchoStep::WaitEcho, Yield::Send(val)),
                EchoStep::WaitEcho => panic!("init called with WaitEcho"),
            }
        }

        fn step(
            state: Self::State,
            msg: Self::Incoming,
        ) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
            match state {
                EchoStep::WaitEcho => (EchoStep::WaitEcho, Yield::Done(msg)),
                EchoStep::Init(_) => panic!("step called with Init"),
            }
        }
    }

    #[test]
    fn test_echo_protocol() {
        let (_, result) = run_protocol::<EchoProtocol, _>(EchoStep::Init(42), |msg| {
            assert_eq!(msg, 42);
            msg // echo back
        });
        assert_eq!(result, 42);
    }

    #[test]
    fn test_yield_helpers() {
        let y: Yield<u32, &str> = Yield::Done(10);
        assert!(y.is_done());
        assert!(!y.is_send());
        assert_eq!(y.unwrap_done(), 10);

        let y: Yield<u32, &str> = Yield::Send("hello");
        assert!(y.is_send());
        assert!(!y.is_done());
        assert_eq!(y.unwrap_send(), "hello");
    }

    #[test]
    fn test_yield_map() {
        let y: Yield<u32, u32> = Yield::Done(5);
        let y2 = y.map_done(|x| x * 2);
        assert_eq!(y2.unwrap_done(), 10);

        let y: Yield<u32, u32> = Yield::Send(3);
        let y2 = y.map_send(|x| x + 1);
        assert_eq!(y2.unwrap_send(), 4);
    }
}
