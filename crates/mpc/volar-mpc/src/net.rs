//! Evaluator-hosted network-socket actions for the strict-actions session
//! (P4b): a [`StrictActionHost`] over a real TCP stream.
//!
//! The circuit drives the connection through three byte-oriented actions
//! (WAT imports under `portal_net.*`, see `volar-vc/tests/action_socket.rs`):
//!
//! All lanes are 64-bit (the WAT imports declare i64s); payloads sit in the
//! low bits, zero-extended, LSB-first per the bit-order convention:
//!
//! - `net_connect(port) -> status`: args are 64 bits (the port in the low
//!   16); connects to `127.0.0.1:port`. Output: 64 bits, status in the low 8
//!   (0 = ok, 1 = error).
//! - `net_send(byte) -> status`: args are 64 bits (the byte in the low 8);
//!   output 64 bits, status in the low 8.
//! - `net_recv() -> (status, byte)`: no args; 128 output bits — the first
//!   i64 carries the status in its low 8 bits (0 = ok, 1 = error/EOF), the
//!   second the received byte in its low 8 bits.
//!
//! The argument bits are *revealed to both parties* by the strict-actions
//! protocol (the garbler exact-match decodes the arg labels), so only ever
//! pass data that may be public — for TLS the circuit encrypts records
//! in-circuit before `net_send`, and `net_recv` carries ciphertext.
//!
//! Status-bit failures abort the session as [`MpcError::ActionHost`] on the
//! evaluator side; the garbler sees the session's normal abort.

extern crate std;

use alloc::string::String;
use alloc::vec::Vec;

use crate::MpcError;
use crate::strict::StrictActionHost;

/// A TCP-backed action host for the strict-actions session.
///
/// One host serves one connection at a time (`net_connect` drops any
/// previous connection). Not re-entrant; a guest that needs two sockets
/// should instantiate one host per connection and route by action name.
#[derive(Default)]
pub struct SocketHost {
    stream: Option<std::net::TcpStream>,
}

impl SocketHost {
    pub fn new() -> Self {
        Self { stream: None }
    }

    fn set_stream(&mut self, port: u16) -> bool {
        match std::net::TcpStream::connect((std::net::Ipv4Addr::LOCALHOST, port)) {
            Ok(s) => {
                let _ = s.set_nodelay(true);
                self.stream = Some(s);
                true
            }
            Err(_) => false,
        }
    }

    fn send_byte(&mut self, byte: u8) -> bool {
        use std::io::Write;
        match self.stream.as_mut() {
            Some(s) => s.write_all(&[byte]).is_ok() && s.flush().is_ok(),
            None => false,
        }
    }

    fn recv_byte(&mut self) -> Option<u8> {
        use std::io::Read;
        let s = self.stream.as_mut()?;
        let mut b = [0u8; 1];
        s.read_exact(&mut b).ok()?;
        Some(b[0])
    }
}

/// Bits (LSB-first) -> u64.
fn bits_to_u64(bits: &[bool]) -> u64 {
    bits.iter()
        .enumerate()
        .fold(0u64, |acc, (i, &b)| acc | ((b as u64) << i))
}

/// u64 -> `n` bits (LSB-first).
fn u64_bits(v: u64, n: usize) -> Vec<bool> {
    (0..n).map(|i| (v >> i) & 1 == 1).collect()
}

impl StrictActionHost for SocketHost {
    fn action(&mut self, name: &str, args: &[bool]) -> Result<Vec<bool>, MpcError> {
        match name {
            "net_connect" => {
                if args.len() != 64 {
                    return Err(MpcError::ActionHost);
                }
                let port = bits_to_u64(args) as u16;
                Ok(u64_bits(self.set_stream(port) as u64 ^ 1, 64).to_vec())
            }
            "net_send" => {
                if args.len() != 64 {
                    return Err(MpcError::ActionHost);
                }
                let byte = bits_to_u64(args) as u8;
                Ok(u64_bits(self.send_byte(byte) as u64 ^ 1, 64).to_vec())
            }
            "net_recv" => {
                if !args.is_empty() {
                    return Err(MpcError::ActionHost);
                }
                let (status, byte) = match self.recv_byte() {
                    Some(b) => (0u64, b as u64),
                    None => (1u64, 0u64),
                };
                let mut out = u64_bits(status, 64);
                out.extend(u64_bits(byte, 64));
                Ok(out)
            }
            _ => Err(MpcError::ActionHost),
        }
    }
}

/// The WASM import names this host answers (the `portal_net` module).
pub const SOCKET_IMPORTS: [(&str, &str); 3] = [
    ("portal_net.connect", "net_connect"),
    ("portal_net.send", "net_send"),
    ("portal_net.recv", "net_recv"),
];

/// Look up the action name for a WASM import, if it is a socket import.
pub fn socket_action_name(waffle_import: &str) -> Option<&'static str> {
    SOCKET_IMPORTS
        .iter()
        .find(|(w, _)| *w == waffle_import)
        .map(|(_, a)| *a)
}

/// Convenience: register all three socket imports on a
/// `volar_vaffle_target::WaffleImportConfig`-shaped map without a volar-ir
/// dependency — returns (waffle_name, action_name, n_args) triples for the
/// caller to feed its import config.
pub fn socket_import_specs() -> [(&'static str, &'static str, usize); 3] {
    [
        // connect(guard, port, fallback_status)
        ("portal_net.connect", "net_connect", 1),
        // send(guard, byte, fallback_status)
        ("portal_net.send", "net_send", 1),
        // recv(guard, fallback_status, fallback_byte) — 0 args, 2 results
        ("portal_net.recv", "net_recv", 0),
    ]
}

/// Handy for tests: the guest WAT skeleton declaring the three socket
/// imports (guard + args + fallbacks, i64 lanes).
pub fn socket_imports_wat() -> String {
    alloc::format!(
        "(import \"portal_net\" \"connect\" (func $connect (param i64 i64 i64) (result i64)))\n\
         (import \"portal_net\" \"send\" (func $send (param i64 i64 i64) (result i64)))\n\
         (import \"portal_net\" \"recv\" (func $recv (param i64 i64 i64) (result i64 i64)))"
    )
}
