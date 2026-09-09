// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//
//! The ORAM glue driver (workstream A, increment 3): the **host** that the
//! woven evaluator's GRAM action extern calls into during the MPC session.
//!
//! The bit-level driver ([`OramHost`], re-exported from
//! [`volar_oram::bit_host`]) decodes flattened boolar bits into structured
//! ORAM values, runs the evaluator-side [`OramClient`] handler, and
//! serializes each result bit LSB-first. That driver is label-free and lives
//! in `volar-oram` so the cirrus GRAM interpreter shares it. This module adds
//! the **label layer**: [`OramHostShim`] re-encodes every result bit to a
//! fresh [`Eval`] label via `gram_regarble`, producing the `Vec<Eval<N>>` the
//! woven evaluator's GRAM action extern returns (`fn(guard: bool, args:
//! &[bool]) -> Vec<Eval<N>>`) — the evaluator cannot re-garble itself.
//!
//! See `volar_oram::bit_host` for the bit-layout contract and the
//! single-access begin → process → evict×2 lifetime.

use alloc::vec::Vec;

use volar_spec::garble::{Eval, Garble, GlobalSecret, gram_regarble};
use volar_spec::vole::VoleArray;

// Re-export the shared bit-level driver so volar-vc consumers get one path.
pub use volar_oram::bit_host::{OramHost, OramHostError};

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
    use volar_oram::{AccessOp, OramClient, OramTree};

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
        let leaf_bits = host.begin(&addr_bits, &mut || rng.next()).expect("begin");
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
        let evict2 = OramHost::<Z, B>::take_u64(&proc_out, &mut roff, 64);
        assert_eq!(proc_out.len(), path_bits_len + data_bits_len + 128);

        // Circuit writes back the updated path at old_leaf.
        let wb_path = host.take_path(wb_bits, "wb").expect("wb path");
        tree.write_path(old_leaf, &wb_path);

        // Two eviction passes: read path at each evict leaf, evict, write back.
        for evict_leaf in [evict1, evict2] {
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

        let data = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
        host_access(&mut host, &mut tree, 3, Some(data), &mut rng);
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
        let mut host = OramHost::<Z, B>::new(levels, num_addrs);
        let mut host_tree = OramTree::<Z, B>::new(levels);
        let mut rng_h = DetRng(0xABC);

        let mut ref_client = OramClient::<Z, B>::new(levels, num_addrs);
        let mut ref_tree = OramTree::<Z, B>::new(levels);
        let mut rng_r = DetRng(0xABC);

        let data = [0x5u8; B];
        host_access(&mut host, &mut host_tree, 2, Some(data), &mut rng_h);
        let _ = volar_oram::oram_access_local(
            &mut ref_client,
            &mut ref_tree,
            2,
            AccessOp::Write(data),
            &mut || rng_r.next(),
        );
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
        let mut bits = Vec::new();
        OramHost::<4, 8>::push_u64(&mut bits, 0x0123_4567_89AB_CDEF, 64);
        assert_eq!(bits.len(), 64);
        let mut off = 0usize;
        assert_eq!(
            OramHost::<4, 8>::take_u64(&bits, &mut off, 64),
            0x0123_4567_89AB_CDEF
        );
        assert_eq!(off, 64);
        assert_eq!(bits[0], true);
    }

    #[test]
    fn oram_host_process_without_begin_errors() {
        let mut host = OramHost::<4, 8>::new(4, 8);
        let path_bits = 4 * 4 * (64 + 64 + 64);
        let mut args = vec![false; path_bits + 64 + 1];
        args[path_bits + 64] = false;
        assert_eq!(
            host.process(&args),
            Err(OramHostError::ProcessWithoutBegin)
        );
    }

    #[test]
    fn oram_host_bad_arg_bits_rejected() {
        let mut host = OramHost::<4, 8>::new(4, 8);
        assert!(matches!(
            host.begin(&[false; 32], &mut || 0),
            Err(OramHostError::BadArgBits { .. })
        ));
    }

    #[test]
    fn shim_regarble_roundtrip() {
        use hybrid_array::Array;
        use typenum::U16;
        use volar_spec::garble::{Garble, GlobalSecret, gram_decode_label};

        let secret = GlobalSecret::<U16>::new(Array::<u8, U16>::from_fn(|i| {
            (i as u8).wrapping_mul(37) | 1
        }));
        let bases: Vec<Garble<U16>> = (0..4)
            .map(|s| Garble {
                base: Array::<u8, U16>::from_fn(|i| (s as u8).wrapping_add(i as u8) | 2),
            })
            .collect();
        let mut shim = OramHostShim::new(&secret, |i| bases[i].clone());

        let bits = [true, false, true, true];
        let labels = shim.regarble(&bits);
        assert_eq!(labels.len(), 4);
        for (i, bit) in bits.iter().enumerate() {
            assert_eq!(gram_decode_label(&labels[i], &bases[i]), *bit);
        }
        assert_ne!(labels[0].target, labels[2].target);
    }
}
