// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//
//! Bit-level ORAM host: drives the evaluator-side [`OramClient`] against
//! **flattened boolar bits**, the shared engine behind both the volar-vc GRAM
//! action extern and the cirrus GRAM interpreter gadget.
//!
//! A GRAM-enabled garbled circuit emits `begin` / `process` / `evict`
//! action calls whose arguments and results are flattened bit vectors. This
//! host decodes those bits into structured ORAM values, runs the
//! [`OramClient`] handler (`handle_begin` / `handle_process` /
//! `handle_evict`), and serializes each result bit back. It is deliberately
//! **label-free**: the garbled-circuit label decode/re-garble layer lives in
//! the consuming crate (volar-vc's `OramHostShim`, cirrus's `GramActionHost`),
//! so this module stays usable from any `no_std + alloc` runtime.
//!
//! # Bit layout (matches the boolar lowering)
//!
//! The IR→boolar lowering flattens every value **LSB-first**: bit `0` of a
//! `u64` is its least-significant bit, and `Vec(n, T)` / `Tuple` concatenate
//! element bit-lists in order. [`OramHost`] reads and writes that exact
//! layout:
//!
//! - `begin`: args = `addr(64)`; results = `leaf(64)` (cleartext).
//! - `process`: args = `path(L × Z × entry) ‖ data(8B) ‖ is_write(1)` where
//!   `entry = addr(64) ‖ leaf(64) ‖ data(8B)`; results =
//!   `wb_path(L × Z × entry) ‖ read_data(8B) ‖ evict_leaf_1(64) ‖
//!   evict_leaf_2(64)`.
//! - `evict`: args = `path(L × Z × entry)`; results =
//!   `path(L × Z × entry)`.
//!
//! # Single-access lifetime
//!
//! `begin` carries an [`ActionBeginState`] that `process` consumes, and
//! `process` queues the two cleartext eviction leaves that the two `evict`
//! calls consume in order. One host drives one access at a time.

use alloc::vec::Vec;

use crate::{ActionBeginState, Bucket, OramClient, OramEntry};

/// Errors the ORAM host can report while parsing flattened action bits.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OramHostError {
    /// The flattened argument bit-slice had the wrong length for the action.
    BadArgBits {
        /// Which action was being parsed.
        action: &'static str,
        /// Expected bit count.
        expected: usize,
        /// Actual bit count.
        got: usize,
    },
    /// `process` was called without a preceding `begin` (no pending state).
    ProcessWithoutBegin,
}

/// The evaluator-side ORAM host: drives one [`OramClient`] access per
/// begin/process/evict action sequence against flattened boolar bits.
///
/// `Z` is the bucket size, `B` the block data size in bytes — the same const
/// generics the [`OramClient`] and [`OramTree`](crate::OramTree) use.
/// `levels` is the tree depth (the ORAM has `2^(levels-1)` leaves).
///
/// Randomness for position-map leaf assignment comes from the supplied `rng`
/// closure (a `&mut dyn FnMut() -> u64`, matching the [`OramClient`] handler
/// signature). For a deterministic test harness this is a seeded
/// counter/splitmix; a production embedder supplies a CSPRNG.
pub struct OramHost<const Z: usize, const B: usize> {
    client: OramClient<Z, B>,
    levels: usize,
    /// Pending begin state, set by `begin` and consumed by `process`.
    pending: Option<ActionBeginState>,
    /// Eviction leaves produced by `process`, consumed one per `evict` call.
    /// The two deterministic eviction targets are cleartext (data-independent),
    /// so the host queues them and feeds them back to the client on each evict.
    evict_leaves: Vec<u64>,
}

impl<const Z: usize, const B: usize> OramHost<Z, B> {
    /// Bits in one ORAM entry: `addr(64) + leaf(64) + data(8B)`.
    pub fn entry_bits(&self) -> usize {
        64 + 64 + 8 * B
    }
    /// Bits in one bucket: `Z × entry_bits`.
    pub fn bucket_bits(&self) -> usize {
        Z * self.entry_bits()
    }
    /// Bits in a full root-to-leaf path: `levels × bucket_bits`.
    pub fn path_bits(&self) -> usize {
        self.levels * self.bucket_bits()
    }

    /// Construct a host over a fresh [`OramClient`] for a tree of `levels`
    /// levels addressing `num_addrs` blocks (local position map).
    pub fn new(levels: usize, num_addrs: u64) -> Self {
        Self {
            client: OramClient::new(levels, num_addrs),
            levels,
            pending: None,
            evict_leaves: Vec::new(),
        }
    }

    /// Borrow the underlying client (e.g. to inspect the stash in tests).
    pub fn client(&self) -> &OramClient<Z, B> {
        &self.client
    }

    // -- Bit (de)serialization, LSB-first ------------------------------------

    /// Read `width` bits starting at `*off` as a little-endian `u64`
    /// (bit 0 = LSB), advancing `*off`. Widths above 64 are truncated to the
    /// low 64 bits; callers only use widths ≤ 64.
    pub fn take_u64(bits: &[bool], off: &mut usize, width: usize) -> u64 {
        let mut v = 0u64;
        for i in 0..width.min(64) {
            if bits[*off + i] {
                v |= 1u64 << i;
            }
        }
        *off += width;
        v
    }

    /// Push the low `width` bits of `v` onto `out`, LSB-first.
    pub fn push_u64(out: &mut Vec<bool>, v: u64, width: usize) {
        for i in 0..width {
            out.push((v >> i) & 1 == 1);
        }
    }

    /// Read one entry (`addr ‖ leaf ‖ data`) at `*off`, advancing it.
    pub fn take_entry(bits: &[bool], off: &mut usize) -> OramEntry<B> {
        let addr = Self::take_u64(bits, off, 64);
        let leaf = Self::take_u64(bits, off, 64);
        let mut data = [0u8; B];
        for byte in data.iter_mut() {
            *byte = Self::take_u64(bits, off, 8) as u8;
        }
        OramEntry { addr, leaf, data }
    }

    /// Push one entry onto `out`.
    pub fn push_entry(out: &mut Vec<bool>, e: &OramEntry<B>) {
        Self::push_u64(out, e.addr, 64);
        Self::push_u64(out, e.leaf, 64);
        for byte in e.data.iter() {
            Self::push_u64(out, *byte as u64, 8);
        }
    }

    /// Read a full path (`levels × Z` entries) at the start of `bits`.
    pub fn take_path(
        &self,
        bits: &[bool],
        action: &'static str,
    ) -> Result<Vec<Bucket<Z, B>>, OramHostError> {
        let expected = self.path_bits();
        if bits.len() != expected {
            return Err(OramHostError::BadArgBits {
                action,
                expected,
                got: bits.len(),
            });
        }
        let mut off = 0usize;
        let mut path = Vec::with_capacity(self.levels);
        for _ in 0..self.levels {
            let mut entries: Vec<OramEntry<B>> = Vec::with_capacity(Z);
            for _ in 0..Z {
                entries.push(Self::take_entry(bits, &mut off));
            }
            // `entries` has exactly Z elements by construction.
            let entries: [OramEntry<B>; Z] =
                entries.try_into().map_err(|_| OramHostError::BadArgBits {
                    action,
                    expected,
                    got: bits.len(),
                })?;
            path.push(Bucket { entries });
        }
        Ok(path)
    }

    /// Push a full path onto `out`.
    pub fn push_path(&self, out: &mut Vec<bool>, path: &[Bucket<Z, B>]) {
        for bucket in path {
            for entry in bucket.entries.iter() {
                Self::push_entry(out, entry);
            }
        }
    }

    // -- Action handlers -----------------------------------------------------

    /// **Begin**: `addr(64)` → `leaf(64)` (cleartext). Sets the pending
    /// state consumed by [`Self::process`].
    ///
    /// `args` must be exactly 64 bits (the address). Returns the 64-bit
    /// old-leaf index, LSB-first.
    pub fn begin(
        &mut self,
        args: &[bool],
        rng: &mut dyn FnMut() -> u64,
    ) -> Result<Vec<bool>, OramHostError> {
        if args.len() != 64 {
            return Err(OramHostError::BadArgBits {
                action: "begin",
                expected: 64,
                got: args.len(),
            });
        }
        let mut off = 0usize;
        let addr = Self::take_u64(args, &mut off, 64);
        let state = self.client.handle_begin(addr, self.levels, rng);
        let old_leaf = state.old_leaf;
        self.pending = Some(state);
        let mut out = Vec::with_capacity(64);
        Self::push_u64(&mut out, old_leaf, 64);
        Ok(out)
    }

    /// **Process**: `path ‖ data(8B) ‖ is_write(1)` →
    /// `wb_path ‖ read_data(8B) ‖ evict1(64) ‖ evict2(64)`.
    ///
    /// Consumes the pending [`ActionBeginState`] from [`Self::begin`].
    pub fn process(&mut self, args: &[bool]) -> Result<Vec<bool>, OramHostError> {
        let path_bits = self.path_bits();
        let data_bits = 8 * B;
        let expected = path_bits + data_bits + 1;
        if args.len() != expected {
            return Err(OramHostError::BadArgBits {
                action: "process",
                expected,
                got: args.len(),
            });
        }
        let state = self
            .pending
            .take()
            .ok_or(OramHostError::ProcessWithoutBegin)?;

        // Parse: path, then write data, then is_write.
        let path = {
            // take_path expects the whole slice; give it the path prefix.
            let path_slice = &args[..path_bits];
            self.take_path(path_slice, "process")?
        };
        let mut off = path_bits;
        let mut write_data = [0u8; B];
        for byte in write_data.iter_mut() {
            *byte = Self::take_u64(args, &mut off, 8) as u8;
        }
        let is_write = args[off];

        let (wb_path, read_data, evict1, evict2) =
            self.client
                .handle_process(&state, &path, write_data, is_write, self.levels);

        // Queue the two eviction leaves for the two `evict` calls that follow.
        self.evict_leaves.clear();
        self.evict_leaves.push(evict1);
        self.evict_leaves.push(evict2);

        let mut out = Vec::with_capacity(path_bits + data_bits + 128);
        self.push_path(&mut out, &wb_path);
        for byte in read_data.iter() {
            Self::push_u64(&mut out, *byte as u64, 8);
        }
        Self::push_u64(&mut out, evict1, 64);
        Self::push_u64(&mut out, evict2, 64);
        Ok(out)
    }

    /// **Evict**: `path` → `path` (the updated eviction path).
    ///
    /// The eviction leaf is **not** an action argument (the circuit knows it
    /// but does not pass it back) — the host dequeues the next leaf produced
    /// by the preceding `process` call, in order (eviction pass 1 then 2).
    pub fn evict(&mut self, args: &[bool]) -> Result<Vec<bool>, OramHostError> {
        let path = self.take_path(args, "evict")?;
        if self.evict_leaves.is_empty() {
            return Err(OramHostError::ProcessWithoutBegin);
        }
        let leaf = self.evict_leaves.remove(0);
        let new_path = self.client.handle_evict(&path, leaf, self.levels);
        let mut out = Vec::with_capacity(self.path_bits());
        self.push_path(&mut out, &new_path);
        Ok(out)
    }
}
