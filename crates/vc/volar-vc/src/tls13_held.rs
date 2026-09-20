//! Shared strict-chain storage for the fixed-shape TLS 1.3 oracle imports.
//!
//! A [`Tls13OracleStorage`] reserves labels through [`HeldSlots`] and produces
//! only [`ChainFeed::Held`] inputs and [`ChainOut::Hold`] outputs. It has no
//! decode API. Consequently, routing a secret-mixing oracle through this
//! storage cannot expose a TLS key, traffic IV, HMAC key, or X25519 state to
//! either party; the garbler holds bases and the evaluator holds labels, and
//! neither representation decodes without the other party's secret delta.
//!
//! @pinnedness: unpinned
//! @stability: very-unstable
//! @ai: assisted

use alloc::vec;
use alloc::vec::Vec;

use digest::Digest;
use volar_mpc::strict_chain::{ChainFeed, ChainOut, ChainParty, HeldRange, HeldSlots};
use volar_mpc::{MpcError, OtChannel, Transport};
use volar_spec::vole::VoleArray;

use crate::compile_schedule_optimized;
use crate::tls13_extern::expand_tls13_oracles;
use volar_mpc::GateSchedule;

/// A fully held strict-chain invocation. Its fields contain only protocol
/// routing metadata—logical values never leave the chain registry.
#[derive(Clone, Debug)]
pub struct HeldOracleCall {
    feeds: Vec<ChainFeed>,
    holds: Vec<ChainOut>,
}

impl HeldOracleCall {
    /// Number of held input bits.
    pub fn input_bits(&self) -> usize {
        self.feeds.len()
    }

    /// Number of held result bits.
    pub fn output_bits(&self) -> usize {
        self.holds.len()
    }

    /// Execute the realized oracle with no public or party-owned input. The
    /// sole result disposition is `Hold`; callers cannot accidentally reveal
    /// key material through this API.
    pub fn run<N, D, C, T>(
        &self,
        party: &mut C,
        schedule: &GateSchedule,
        session: &mut T,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError>
    where
        N: VoleArray<u8>,
        D: Digest,
        C: ChainParty<N>,
        T: Transport,
    {
        debug_assert_eq!(schedule.num_inputs, self.feeds.len());
        debug_assert_eq!(schedule.output_wires().len(), self.holds.len());
        let revealed =
            party.run_round::<D, T>(schedule, &self.feeds, &[], &[], &self.holds, session, ot)?;
        debug_assert!(revealed.is_empty(), "held oracle call must not reveal");
        Ok(())
    }

    /// Protocol-routing metadata for auditing. Every entry is `Held`; this
    /// exposes slot identities, never labels or values.
    #[doc(hidden)]
    pub fn feeds(&self) -> &[ChainFeed] {
        &self.feeds
    }

    /// Protocol-routing metadata for auditing. Every entry is `Hold`; this
    /// exposes slot identities, never labels or values.
    #[doc(hidden)]
    pub fn holds(&self) -> &[ChainOut] {
        &self.holds
    }
}

/// Precompiled schedules for the fixed-shape TLS imports.
#[derive(Clone, Debug)]
pub struct Tls13OracleSchedules {
    /// SHA-256 over one padded block (64-byte message input).
    pub sha256_64: GateSchedule,
    /// HMAC-SHA-256 with a 32-byte key and message.
    pub hmac_sha256_32_32: GateSchedule,
    /// One RFC 7748 X25519 Montgomery-ladder step.
    pub x25519_step: GateSchedule,
}

impl Tls13OracleSchedules {
    /// Realize the import contracts and optimize their boolar schedules.
    pub fn new() -> Self {
        use volar_ir::boolar::BIrBlocks;
        use volar_ir::boolar::{BIrBlock, BIrStmt, BIrTarget, BIrTerminator};
        use volar_ir::ir::{IRBlockTargetId, IRVarId};
        use volar_ir_common::Node;
        use volar_ir_common::tls13_extern;

        fn import(name: &str, args: usize, results: usize) -> BIrBlocks {
            let inputs: Vec<IRVarId> = (0..args as u32).map(IRVarId).collect();
            let stmts = (0..results)
                .map(|bit| {
                    Node::new(
                        BIrStmt::OracleBit {
                            name: name.into(),
                            args: inputs.clone(),
                            bit,
                            occurrence: 0,
                        },
                        (),
                        None,
                    )
                })
                .collect();
            BIrBlocks {
                blocks: vec![BIrBlock {
                    params: args as u32,
                    stmts,
                    terminator: BIrTerminator::Jmp(BIrTarget {
                        block: IRBlockTargetId::Return,
                        args: (0..results)
                            .map(|bit| IRVarId(args as u32 + bit as u32))
                            .collect(),
                    }),
                }],
                pre_init: vec![],
            }
        }

        let schedule = |name, args, results| {
            compile_schedule_optimized(&expand_tls13_oracles(&import(name, args, results)))
                .expect("fixed-shape TLS oracle realizes to a schedule")
        };
        Self {
            sha256_64: schedule(
                tls13_extern::sha256_64::ORACLE_NAME,
                tls13_extern::sha256_64::MSG_BITS,
                tls13_extern::sha256_64::RESULT_BITS,
            ),
            hmac_sha256_32_32: schedule(
                tls13_extern::hmac_sha256_32_32::ORACLE_NAME,
                tls13_extern::hmac_sha256_32_32::ARG_BITS,
                tls13_extern::hmac_sha256_32_32::RESULT_BITS,
            ),
            x25519_step: schedule(
                tls13_extern::x25519_step::ORACLE_NAME,
                tls13_extern::x25519_step::ARG_BITS,
                tls13_extern::x25519_step::RESULT_BITS,
            ),
        }
    }
}

impl Default for Tls13OracleSchedules {
    fn default() -> Self {
        Self::new()
    }
}

/// Held regions for one X25519 ladder step and its alternate output bank.
#[derive(Clone, Copy, Debug)]
pub struct X25519HeldState {
    /// The invariant input Montgomery u-coordinate.
    pub x1: HeldRange,
    pub x2: HeldRange,
    pub z2: HeldRange,
    pub x3: HeldRange,
    pub z3: HeldRange,
    pub swap: HeldRange,
    /// One clamped scalar bit for the current step.
    pub scalar_bit: HeldRange,
    /// Alternate state bank for the next ladder step.
    pub next_x2: HeldRange,
    pub next_z2: HeldRange,
    pub next_x3: HeldRange,
    pub next_z3: HeldRange,
    pub next_swap: HeldRange,
}

impl X25519HeldState {
    fn call(&self) -> HeldOracleCall {
        let mut feeds = Vec::with_capacity(5 * 255 + 2);
        for range in [
            self.x2,
            self.z2,
            self.x3,
            self.z3,
            self.x1,
            self.swap,
            self.scalar_bit,
        ] {
            feeds.extend(range.feeds());
        }
        let mut holds = Vec::with_capacity(4 * 255 + 1);
        for range in [
            self.next_x2,
            self.next_z2,
            self.next_x3,
            self.next_z3,
            self.next_swap,
        ] {
            holds.extend(range.holds());
        }
        HeldOracleCall { feeds, holds }
    }
}

/// All secret storage needed by the initial fixed-shape TLS import surface.
#[derive(Clone, Copy, Debug)]
pub struct Tls13OracleStorage {
    /// 64-byte SHA input and its 32-byte digest.
    pub sha_message: HeldRange,
    pub sha_digest: HeldRange,
    /// 32-byte HMAC key, 32-byte message, and 32-byte tag.
    pub hmac_key: HeldRange,
    pub hmac_message: HeldRange,
    pub hmac_tag: HeldRange,
    /// The X25519 current and next state banks.
    pub x25519: X25519HeldState,
}

impl Tls13OracleStorage {
    /// Reserve all TLS secret regions from a chain's allocator. Allocation
    /// changes only public routing shape; it allocates no labels and reveals
    /// no values.
    pub fn reserve(slots: &mut HeldSlots) -> Self {
        let sha_message = slots.reserve(64 * 8);
        let sha_digest = slots.reserve(32 * 8);
        let hmac_key = slots.reserve(32 * 8);
        let hmac_message = slots.reserve(32 * 8);
        let hmac_tag = slots.reserve(32 * 8);
        let x1 = slots.reserve(255);
        let x2 = slots.reserve(255);
        let z2 = slots.reserve(255);
        let x3 = slots.reserve(255);
        let z3 = slots.reserve(255);
        let swap = slots.reserve(1);
        let scalar_bit = slots.reserve(1);
        let next_x2 = slots.reserve(255);
        let next_z2 = slots.reserve(255);
        let next_x3 = slots.reserve(255);
        let next_z3 = slots.reserve(255);
        let next_swap = slots.reserve(1);
        Self {
            sha_message,
            sha_digest,
            hmac_key,
            hmac_message,
            hmac_tag,
            x25519: X25519HeldState {
                x1,
                x2,
                z2,
                x3,
                z3,
                swap,
                scalar_bit,
                next_x2,
                next_z2,
                next_x3,
                next_z3,
                next_swap,
            },
        }
    }

    /// Held-only SHA-256 invocation.
    pub fn sha256_64(&self) -> HeldOracleCall {
        HeldOracleCall {
            feeds: self.sha_message.feeds(),
            holds: self.sha_digest.holds(),
        }
    }

    /// Held-only HMAC-SHA-256 invocation.
    pub fn hmac_sha256_32_32(&self) -> HeldOracleCall {
        let mut feeds = self.hmac_key.feeds();
        feeds.extend(self.hmac_message.feeds());
        HeldOracleCall {
            feeds,
            holds: self.hmac_tag.holds(),
        }
    }

    /// Held-only X25519 step invocation. The next state is written to the
    /// alternate bank, avoiding read/write aliasing in one strict round.
    pub fn x25519_step(&self) -> HeldOracleCall {
        self.x25519.call()
    }
}
