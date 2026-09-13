// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Byte codecs for role-separated OT / Ferret messages (framed transports).

use alloc::vec::Vec;

use super::ferret::mpcot_reg::MpcotRegSenderMsg;
use super::ferret::spcot::{Block, SpcotSenderMsg};
use super::iknp::{IKNP_KAPPA, IKNP_KAPPA_BYTES, IknpUMsg};
use super::lwe::{LweOtCrs, LweOtRecvMsg, LweOtSenderMsgDyn, Zq};

/// Sender → receiver: VOLE/Ferret `Δ` (16 bytes).
pub const TAG_DELTA: u8 = 1;
/// Base-OT sender setup (LWE CRS).
pub const TAG_LWE_SETUP: u8 = 2;
/// Base-OT receiver message.
pub const TAG_LWE_RECV: u8 = 3;
/// Base-OT payload.
pub const TAG_LWE_PAYLOAD: u8 = 4;
/// IKNP `u` columns.
pub const TAG_IKNP_U: u8 = 5;
/// IKNP C-OT corrections.
pub const TAG_IKNP_CORR: u8 = 6;
/// SoftSpoken sender tag.
pub const TAG_SSP_S: u8 = 7;
/// SoftSpoken receiver tag.
pub const TAG_SSP_R: u8 = 8;
/// Ferret receiver opening (LPN seed + SPCOT choices).
pub const TAG_FERRET_OPEN: u8 = 9;
/// Ferret sender MPCOT ciphertexts.
pub const TAG_FERRET_MPCOT: u8 = 10;
/// Bea95 correction bit.
pub const TAG_BEA95: u8 = 11;
/// Quicksilver AND `hat`.
pub const TAG_HAT: u8 = 12;
/// Ferret malicious consistency check: receiver's masked extra choice `x*′`.
pub const TAG_FERRET_CHECK_MASK: u8 = 13;
/// Ferret malicious consistency check: sender's `H'(V)`.
pub const TAG_FERRET_CHECK_HV: u8 = 14;

fn push_u32(buf: &mut Vec<u8>, x: u32) {
    buf.extend_from_slice(&x.to_le_bytes());
}

fn take_u32(bytes: &[u8], off: &mut usize) -> u32 {
    let x = u32::from_le_bytes(bytes[*off..*off + 4].try_into().unwrap());
    *off += 4;
    x
}

fn push_block(buf: &mut Vec<u8>, b: &Block) {
    buf.extend_from_slice(b);
}

fn take_block(bytes: &[u8], off: &mut usize) -> Block {
    let mut b = [0u8; 16];
    b.copy_from_slice(&bytes[*off..*off + 16]);
    *off += 16;
    b
}

/// Encode a bool slice as `len || bytes`.
pub fn encode_bools(bits: &[bool]) -> Vec<u8> {
    let mut buf = Vec::with_capacity(4 + bits.len());
    push_u32(&mut buf, bits.len() as u32);
    buf.extend(bits.iter().map(|&b| b as u8));
    buf
}

/// Decode a bool slice; returns `(bits, bytes_consumed)`.
pub fn decode_bools(bytes: &[u8]) -> (Vec<bool>, usize) {
    let mut off = 0usize;
    let n = take_u32(bytes, &mut off) as usize;
    let bits = bytes[off..off + n].iter().map(|&b| b != 0).collect();
    off += n;
    (bits, off)
}

/// Encode [`SpcotSenderMsg`].
pub fn encode_spcot(msg: &SpcotSenderMsg) -> Vec<u8> {
    let mut buf = Vec::new();
    push_u32(&mut buf, msg.ms.len() as u32);
    for pair in &msg.ms {
        push_block(&mut buf, &pair[0]);
        push_block(&mut buf, &pair[1]);
    }
    push_block(&mut buf, &msg.c);
    push_u32(&mut buf, msg.hash_v.len() as u32);
    buf.extend_from_slice(&msg.hash_v);
    buf
}

/// Decode [`SpcotSenderMsg`].
pub fn decode_spcot(bytes: &[u8]) -> (SpcotSenderMsg, usize) {
    let mut off = 0usize;
    let h = take_u32(bytes, &mut off) as usize;
    let mut ms = Vec::with_capacity(h);
    for _ in 0..h {
        let a = take_block(bytes, &mut off);
        let b = take_block(bytes, &mut off);
        ms.push([a, b]);
    }
    let c = take_block(bytes, &mut off);
    let hv_len = take_u32(bytes, &mut off) as usize;
    let hash_v = bytes[off..off + hv_len].to_vec();
    off += hv_len;
    (SpcotSenderMsg { ms, c, hash_v }, off)
}

/// Encode regular MPCOT sender message.
pub fn encode_mpcot_reg(msg: &MpcotRegSenderMsg) -> Vec<u8> {
    let mut buf = Vec::new();
    push_u32(&mut buf, msg.blocks.len() as u32);
    for b in &msg.blocks {
        let inner = encode_spcot(b);
        push_u32(&mut buf, inner.len() as u32);
        buf.extend_from_slice(&inner);
    }
    buf
}

/// Decode regular MPCOT sender message.
pub fn decode_mpcot_reg(bytes: &[u8]) -> MpcotRegSenderMsg {
    let mut off = 0usize;
    let n = take_u32(bytes, &mut off) as usize;
    let mut blocks = Vec::with_capacity(n);
    for _ in 0..n {
        let len = take_u32(bytes, &mut off) as usize;
        let (msg, used) = decode_spcot(&bytes[off..off + len]);
        debug_assert_eq!(used, len);
        off += len;
        blocks.push(msg);
    }
    MpcotRegSenderMsg { blocks }
}

/// Encode Ferret receiver opening (`lpn_seed || choices`).
pub fn encode_ferret_open(lpn_seed: &[u8; 16], choices: &[bool]) -> Vec<u8> {
    let mut buf = Vec::new();
    buf.extend_from_slice(lpn_seed);
    buf.extend(encode_bools(choices));
    buf
}

/// Decode Ferret receiver opening.
pub fn decode_ferret_open(bytes: &[u8]) -> ([u8; 16], Vec<bool>) {
    let mut seed = [0u8; 16];
    seed.copy_from_slice(&bytes[..16]);
    let (choices, _) = decode_bools(&bytes[16..]);
    (seed, choices)
}

/// Encode LWE CRS.
pub fn encode_lwe_crs<const N: usize>(crs: &LweOtCrs<N>) -> Vec<u8> {
    let mut buf = Vec::with_capacity(4 + 4 * (N * N + N));
    push_u32(&mut buf, N as u32);
    for i in 0..N {
        for j in 0..N {
            buf.extend_from_slice(&crs.a[i][j].to_le_bytes());
        }
    }
    for i in 0..N {
        buf.extend_from_slice(&crs.h[i].to_le_bytes());
    }
    buf
}

/// Decode LWE CRS.
pub fn decode_lwe_crs<const N: usize>(bytes: &[u8]) -> LweOtCrs<N> {
    let mut off = 0usize;
    let n = take_u32(bytes, &mut off) as usize;
    debug_assert_eq!(n, N);
    let mut a = [[0u32; N]; N];
    for i in 0..N {
        for j in 0..N {
            a[i][j] = u32::from_le_bytes(bytes[off..off + 4].try_into().unwrap());
            off += 4;
        }
    }
    let mut h = [0u32; N];
    for i in 0..N {
        h[i] = u32::from_le_bytes(bytes[off..off + 4].try_into().unwrap());
        off += 4;
    }
    LweOtCrs { a, h }
}

/// Encode LWE receiver message.
pub fn encode_lwe_recv<const N: usize>(msg: &LweOtRecvMsg<N>) -> Vec<u8> {
    let mut buf = Vec::with_capacity(4 * N);
    for i in 0..N {
        buf.extend_from_slice(&msg.pk0[i].to_le_bytes());
    }
    buf
}

/// Decode LWE receiver message.
pub fn decode_lwe_recv<const N: usize>(bytes: &[u8]) -> LweOtRecvMsg<N> {
    let mut pk0 = [0u32; N];
    for i in 0..N {
        pk0[i] = u32::from_le_bytes(bytes[i * 4..i * 4 + 4].try_into().unwrap());
    }
    LweOtRecvMsg { pk0 }
}

fn encode_zq_vec(v: &[Zq]) -> Vec<u8> {
    let mut buf = Vec::new();
    push_u32(&mut buf, v.len() as u32);
    for x in v {
        buf.extend_from_slice(&x.to_le_bytes());
    }
    buf
}

fn decode_zq_vec(bytes: &[u8], off: &mut usize) -> Vec<Zq> {
    let n = take_u32(bytes, off) as usize;
    let mut v = Vec::with_capacity(n);
    for _ in 0..n {
        v.push(u32::from_le_bytes(
            bytes[*off..*off + 4].try_into().unwrap(),
        ));
        *off += 4;
    }
    v
}

/// Encode LWE dynamic payload.
pub fn encode_lwe_payload(msg: &LweOtSenderMsgDyn) -> Vec<u8> {
    let mut buf = Vec::new();
    buf.extend(encode_zq_vec(&msg.u0));
    buf.extend(encode_zq_vec(&msg.v0));
    buf.extend(encode_zq_vec(&msg.u1));
    buf.extend(encode_zq_vec(&msg.v1));
    buf
}

/// Decode LWE dynamic payload.
pub fn decode_lwe_payload(bytes: &[u8]) -> LweOtSenderMsgDyn {
    let mut off = 0usize;
    let u0 = decode_zq_vec(bytes, &mut off);
    let v0 = decode_zq_vec(bytes, &mut off);
    let u1 = decode_zq_vec(bytes, &mut off);
    let v1 = decode_zq_vec(bytes, &mut off);
    LweOtSenderMsgDyn { u0, v0, u1, v1 }
}

/// Encode IKNP `u` columns.
pub fn encode_iknp_u(msg: &IknpUMsg) -> Vec<u8> {
    let mut buf = Vec::new();
    push_u32(&mut buf, msg.u_cols.len() as u32);
    for col in &msg.u_cols {
        buf.extend(encode_bools(col));
    }
    buf
}

/// Decode IKNP `u` columns.
pub fn decode_iknp_u(bytes: &[u8]) -> IknpUMsg {
    let mut off = 0usize;
    let n = take_u32(bytes, &mut off) as usize;
    debug_assert_eq!(n, IKNP_KAPPA);
    let mut u_cols = Vec::with_capacity(n);
    for _ in 0..n {
        let (col, used) = decode_bools(&bytes[off..]);
        off += used;
        u_cols.push(col);
    }
    IknpUMsg { u_cols }
}

/// Encode IKNP correction rows (`L = 16`).
pub fn encode_iknp_corr(rows: &[[u8; IKNP_KAPPA_BYTES]]) -> Vec<u8> {
    let mut buf = Vec::new();
    push_u32(&mut buf, rows.len() as u32);
    for r in rows {
        buf.extend_from_slice(r);
    }
    buf
}

/// Decode IKNP correction rows.
pub fn decode_iknp_corr(bytes: &[u8]) -> Vec<[u8; IKNP_KAPPA_BYTES]> {
    let mut off = 0usize;
    let n = take_u32(bytes, &mut off) as usize;
    let mut rows = Vec::with_capacity(n);
    for _ in 0..n {
        let mut r = [0u8; IKNP_KAPPA_BYTES];
        r.copy_from_slice(&bytes[off..off + IKNP_KAPPA_BYTES]);
        off += IKNP_KAPPA_BYTES;
        rows.push(r);
    }
    rows
}
