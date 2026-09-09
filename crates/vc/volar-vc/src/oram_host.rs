// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//
//! The ORAM glue driver (workstream A, increment 3): the **host** that the
//! woven evaluator's GRAM action extern calls into during the MPC session.
//!
//! The config-carrying weaver (`weave_evaluator_with_gram`) emits, for each
//! registered ORAM action, an extern `fn(guard: bool, args: &[bool]) ->
//! Vec<Eval<N>>`. [`OramHost`] is the cleartext-bit half of that extern: it
//! decodes the flattened argument bits into structured ORAM values, runs the
//! evaluator-side [`OramClient`] handler (`handle_begin` / `handle_process` /
//! `handle_evict`), and serializes each result bit LSB-first. The embedder
//! glue that owns the [`GlobalSecret`] then re-encodes every result bit to a
//! fresh [`Eval`] label via `gram_regarble` — the evaluator cannot re-garble
//! itself, so the returned `Vec<Eval<N>>` is what flows back into the circuit.
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
//! The host is the evaluator's trusted local party: per the
//! [`GramActionConfig`] output contract, only the *cleartext* outputs (the
//! `begin` leaf and the two `process` eviction leaves — data-independent
//! values) may be learned in the clear; the secret path/data bits are produced
//! here only to be re-garbled by the embedder shim before returning.
//!
//! # Single-access lifetime
//!
//! `begin` carries an [`ActionBeginState`] that `process` consumes, so one
//! host drives one access at a time: `begin` → (circuit reads the tree path)
//! → `process` → `evict` × 2. The state is held in the host between the
//! `begin` and `process` calls.

use alloc::vec::Vec;

use volar_oram::{ActionBeginState, Bucket, OramClient, OramEntry};
use volar_spec::garble::{Eval, Garble, GlobalSecret, gram_regarble};
use volar_spec::vole::VoleArray;

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
/// generics the [`OramClient`] and [`OramTree`] use. `levels` is the tree
/// depth (the ORAM has `2^(levels-1)` leaves).
///
/// Randomness for position-map leaf assignment comes from the supplied `rng`
/// closure (a `&mut dyn FnMut() -> u64`, matching the [`OramClient`] handler
/// signature). For the deterministic test harness this is a seeded
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
    fn entry_bits(&self) -> usize {
        64 + 64 + 8 * B
    }
    /// Bits in one bucket: `Z × entry_bits`.
    fn bucket_bits(&self) -> usize {
        Z * self.entry_bits()
    }
    /// Bits in a full root-to-leaf path: `levels × bucket_bits`.
    fn path_bits(&self) -> usize {
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
    fn take_u64(bits: &[bool], off: &mut usize, width: usize) -> u64 {
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
    fn push_u64(out: &mut Vec<bool>, v: u64, width: usize) {
        for i in 0..width {
            out.push((v >> i) & 1 == 1);
        }
    }

    /// Read one entry (`addr ‖ leaf ‖ data`) at `*off`, advancing it.
    fn take_entry(bits: &[bool], off: &mut usize) -> OramEntry<B> {
        let addr = Self::take_u64(bits, off, 64);
        let leaf = Self::take_u64(bits, off, 64);
        let mut data = [0u8; B];
        for byte in data.iter_mut() {
            *byte = Self::take_u64(bits, off, 8) as u8;
        }
        OramEntry { addr, leaf, data }
    }

    /// Push one entry onto `out`.
    fn push_entry(out: &mut Vec<bool>, e: &OramEntry<B>) {
        Self::push_u64(out, e.addr, 64);
        Self::push_u64(out, e.leaf, 64);
        for byte in e.data.iter() {
            Self::push_u64(out, *byte as u64, 8);
        }
    }

    /// Read a full path (`levels × Z` entries) at the start of `bits`.
    fn take_path(&self, bits: &[bool], action: &'static str) -> Result<Vec<Bucket<Z, B>>, OramHostError> {
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
    fn push_path(&self, out: &mut Vec<bool>, path: &[Bucket<Z, B>]) {
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
        let state = self.pending.take().ok_or(OramHostError::ProcessWithoutBegin)?;

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

// ============================================================================
// Re-garble shim
// ============================================================================

/// The re-garble shim: wraps the cleartext-bit [`OramHost`] with the
/// garbler's [`GlobalSecret`] and a per-result-wire base supply, producing the
/// `Vec<Eval<N>>` the woven evaluator's GRAM action extern returns
/// (`fn(guard: bool, args: &[bool]) -> Vec<Eval<N>>`).
///
/// The evaluator cannot re-garble — only the garbler knows the secret. So the
/// host extern is linked against this shim, which runs the cleartext-bit
/// driver and re-encodes every result bit to a fresh label via
/// [`gram_regarble`]. The **base for each result wire** must be the *same*
/// false-label the garbler assigned that wire when it garbled the circuit —
/// otherwise the half-gates AND tables downstream won't decode. The shim
/// therefore takes a `base_for` closure that yields the [`Garble`] false-label
/// for the `i`-th result bit of the current call; the embedder wires this to
/// the same deterministic base supply the garbler used for the action's
/// result wires (the increment-4 embedder integration contract).
///
/// The shim is generic over the base supply rather than owning it, so the
/// embedder can back it by a seeded RNG, an OT-delivered table, or a fixed
/// test vector without changing this type.
pub struct OramHostShim<'a, N: VoleArray<u8>, F>
where
    F: FnMut(usize) -> Garble<N>,
{
    secret: &'a GlobalSecret<N>,
    base_for: F,
}

impl<'a, N: VoleArray<u8>, F> OramHostShim<'a, N, F>
where
    F: FnMut(usize) -> Garble<N>,
{
    /// Create a shim from the garbler's secret and a base supply.
    /// `base_for(i)` must return the false-label for the `i`-th result bit of
    /// the next call.
    pub fn new(secret: &'a GlobalSecret<N>, base_for: F) -> Self {
        Self { secret, base_for }
    }

    /// Re-garble a cleartext result-bit vector to fresh labels: `out[i] =
    /// gram_regarble(secret, base_for(i), bits[i])`. This is the extern's
    /// return value.
    pub fn regarble(&mut self, bits: &[bool]) -> Vec<Eval<N>> {
        bits.iter()
            .enumerate()
            .map(|(i, &bit)| gram_regarble(self.secret, &(self.base_for)(i), bit))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use volar_oram::{AccessOp, OramTree};

    // A deterministic splitmix64-style RNG for the test harness.
    struct DetRng(u64);
    impl DetRng {
        fn next(&mut self) -> u64 {
            self.0 = self.0.wrapping_add(0x9E3779B97F4A7C15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
            z ^ (z >> 31)
        }
    }

    // Run one full access through the host against `tree`, returning the
    // read-data bits. Mirrors the circuit's begin → read_path → process →
    // evict × 2 sequence, moving paths through the host as flattened bits.
    fn host_access<const Z: usize, const B: usize>(
        host: &mut OramHost<Z, B>,
        tree: &mut OramTree<Z, B>,
        addr: u64,
        write: Option<[u8; B]>,
        rng: &mut DetRng,
    ) -> [u8; B] {
        // begin: addr(64) → old_leaf(64).
        let mut addr_bits = Vec::new();
        OramHost::<Z, B>::push_u64(&mut addr_bits, addr, 64);
        let leaf_bits = host
            .begin(&addr_bits, &mut || rng.next())
            .expect("begin");
        let mut off = 0usize;
        let old_leaf = OramHost::<Z, B>::take_u64(&leaf_bits, &mut off, 64);

        // Circuit reads the tree path at old_leaf, flattens it.
        let path = tree.read_path(old_leaf);
        let mut path_bits = Vec::new();
        host.push_path(&mut path_bits, &path);

        // process: path ‖ data ‖ is_write → wb_path ‖ read_data ‖ e1 ‖ e2.
        let mut proc_args = path_bits.clone();
        let wd = write.unwrap_or([0u8; B]);
        for byte in wd.iter() {
            OramHost::<Z, B>::push_u64(&mut proc_args, *byte as u64, 8);
        }
        proc_args.push(write.is_some());
        let proc_out = host.process(&proc_args).expect("process");

        // Parse process output: wb_path ‖ read_data ‖ e1 ‖ e2.
        let path_bits_len = path_bits.len();
        let data_bits_len = 8 * B;
        let wb_bits = &proc_out[..path_bits_len];
        let mut roff = path_bits_len;
        let mut read_data = [0u8; B];
        for byte in read_data.iter_mut() {
            *byte = OramHost::<Z, B>::take_u64(&proc_out, &mut roff, 8) as u8;
        }
        let evict1 = OramHost::<Z, B>::take_u64(&proc_out, &mut roff, 64);
        let _evict2 = OramHost::<Z, B>::take_u64(&proc_out, &mut roff, 64);
        assert_eq!(proc_out.len(), path_bits_len + data_bits_len + 128);

        // Circuit writes back the updated path at old_leaf.
        let wb_path = host.take_path(wb_bits, "wb").expect("wb path");
        tree.write_path(old_leaf, &wb_path);

        // Two eviction passes: read path at each evict leaf, evict, write back.
        for evict_leaf in [evict1, _evict2] {
            let ep = tree.read_path(evict_leaf);
            let mut ep_bits = Vec::new();
            host.push_path(&mut ep_bits, &ep);
            let new_ep_bits = host.evict(&ep_bits).expect("evict");
            let new_ep = host.take_path(&new_ep_bits, "evict_out").expect("evict path");
            tree.write_path(evict_leaf, &new_ep);
        }

        read_data
    }

    #[test]
    fn oram_host_write_then_read_roundtrip() {
        const Z: usize = 4;
        const B: usize = 8;
        let levels = 4;
        let num_addrs = 8u64;
        let mut host = OramHost::<Z, B>::new(levels, num_addrs);
        let mut tree = OramTree::<Z, B>::new(levels);
        let mut rng = DetRng(0xDEADBEEF);

        // Write addr 3.
        let data = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
        host_access(&mut host, &mut tree, 3, Some(data), &mut rng);

        // Read it back.
        let got = host_access::<Z, B>(&mut host, &mut tree, 3, None, &mut rng);
        assert_eq!(got, data, "read must return the written block");
    }

    #[test]
    fn oram_host_multiple_addrs() {
        const Z: usize = 4;
        const B: usize = 4;
        let levels = 4;
        let mut host = OramHost::<Z, B>::new(levels, 8);
        let mut tree = OramTree::<Z, B>::new(levels);
        let mut rng = DetRng(0x1234);

        let d0 = [0xAA; B];
        let d1 = [0xBB; B];
        host_access(&mut host, &mut tree, 0, Some(d0), &mut rng);
        host_access(&mut host, &mut tree, 1, Some(d1), &mut rng);
        assert_eq!(host_access(&mut host, &mut tree, 0, None, &mut rng), d0);
        assert_eq!(host_access(&mut host, &mut tree, 1, None, &mut rng), d1);
        // Overwrite addr 0.
        let d0b = [0xCC; B];
        host_access(&mut host, &mut tree, 0, Some(d0b), &mut rng);
        assert_eq!(host_access(&mut host, &mut tree, 0, None, &mut rng), d0b);
        assert_eq!(host_access(&mut host, &mut tree, 1, None, &mut rng), d1);
    }

    #[test]
    fn oram_host_matches_local_reference() {
        const Z: usize = 4;
        const B: usize = 8;
        let levels = 4;
        let num_addrs = 8u64;
        // Host path.
        let mut host = OramHost::<Z, B>::new(levels, num_addrs);
        let mut host_tree = OramTree::<Z, B>::new(levels);
        let mut rng_h = DetRng(0xABC);

        // Reference path: a separate client + tree driven by oram_access_local
        // with the same seed stream.
        let mut ref_client = OramClient::<Z, B>::new(levels, num_addrs);
        let mut ref_tree = OramTree::<Z, B>::new(levels);
        let mut rng_r = DetRng(0xABC);

        let data = [0x5u8; B];
        // Write via both.
        host_access(&mut host, &mut host_tree, 2, Some(data), &mut rng_h);
        let _ = volar_oram::oram_access_local(
            &mut ref_client,
            &mut ref_tree,
            2,
            AccessOp::Write(data),
            &mut || rng_r.next(),
        );
        // Read via both and compare.
        let h = host_access(&mut host, &mut host_tree, 2, None, &mut rng_h);
        let r = volar_oram::oram_access_local(
            &mut ref_client,
            &mut ref_tree,
            2,
            AccessOp::Read,
            &mut || rng_r.next(),
        );
        let volar_oram::AccessResult::ReadValue(rd) = r else {
            panic!("reference read must return a value");
        };
        assert_eq!(h, rd, "host-driven read must match the local reference");
    }

    #[test]
    fn oram_host_bit_roundtrip() {
        // u64 LSB-first round-trip.
        let mut bits = Vec::new();
        OramHost::<4, 8>::push_u64(&mut bits, 0x0123_4567_89AB_CDEF, 64);
        assert_eq!(bits.len(), 64);
        let mut off = 0usize;
        assert_eq!(
            OramHost::<4, 8>::take_u64(&bits, &mut off, 64),
            0x0123_4567_89AB_CDEF
        );
        assert_eq!(off, 64);
        // Bit 0 is the LSB.
        assert_eq!(bits[0], true); // 0x...F, LSB = 1
    }

    #[test]
    fn oram_host_process_without_begin_errors() {
        let mut host = OramHost::<4, 8>::new(4, 8);
        let path_bits = 4 * 4 * (64 + 64 + 64);
        let mut args = vec![false; path_bits + 64 + 1];
        args[path_bits + 64] = false; // is_write = false
        assert_eq!(
            host.process(&args),
            Err(OramHostError::ProcessWithoutBegin)
        );
    }

    #[test]
    fn oram_host_bad_arg_bits_rejected() {
        let mut host = OramHost::<4, 8>::new(4, 8);
        // begin expects exactly 64 bits.
        assert!(matches!(
            host.begin(&[false; 32], &mut || 0),
            Err(OramHostError::BadArgBits { .. })
        ));
    }

    #[test]
    fn shim_regarble_roundtrip() {
        use typenum::U16;
        use hybrid_array::Array;
        use volar_spec::garble::{Garble, GlobalSecret, gram_decode_label};

        let secret =
            GlobalSecret::<U16>::new(Array::<u8, U16>::from_fn(|i| (i as u8).wrapping_mul(37) | 1));
        // Deterministic per-result bases.
        let bases: Vec<Garble<U16>> = (0..4)
            .map(|s| Garble {
                base: Array::<u8, U16>::from_fn(|i| (s as u8).wrapping_add(i as u8) | 2),
            })
            .collect();
        let mut idx = 0usize;
        let mut shim = OramHostShim::new(&secret, |i| {
            idx = i;
            bases[i].clone()
        });

        let bits = [true, false, true, true];
        let labels = shim.regarble(&bits);
        assert_eq!(labels.len(), 4);
        let _ = idx;
        // The host can verify each label decodes back to the bit against its base.
        for (i, bit) in bits.iter().enumerate() {
            assert_eq!(gram_decode_label(&labels[i], &bases[i]), *bit);
        }
        // Distinct bases → the evaluator cannot correlate labels across wires.
        assert_ne!(labels[0].target, labels[2].target);
    }
}
