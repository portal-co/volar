//! Strict multi-circuit sessions ("chains"): the protocol-merge layer.
//!
//! A chain is a sequence of strict two-party circuit invocations over one
//! transport. Secrets cross invocations as **threaded re-based labels**:
//! the garbler aligns the next circuit's input base to the previous
//! circuit's output base under the single global delta, so the evaluator's
//! held output label IS the next input label and the value never decodes
//! (the S4 mechanism, here on the strict session: the evaluator never sees
//! the delta). Values that must leave the chain (record ciphertexts, the
//! final verdict) are revealed via the strict OutputLabels/VerdictBits
//! round (exact-match decode: a forged label aborts).
//!
//! Every round rides one transport, so a transcript hash over the session
//! binds all rounds — this is how independent checks (a TLS Turnstile
//! verification and a licensing predicate) are **correlated** rather than
//! standalone: the final circuit ANDs their threaded verdict wires, and
//! neither verdict can be replayed or swapped independently of the other.
//!
//! The polarity subtlety: a strict (Not-eliminated) output's logical value
//! is `raw XOR output_polarity`. Threading registers the RAW evaluator
//! label, so the garbler registers the held slot's false-label base as
//! `raw_base XOR (polarity ? delta : 0)` — then the held label encodes the
//! LOGICAL value under that base (free-XOR: L = raw_base XOR raw*delta =
//! base' XOR (raw XOR pol)*delta).

use alloc::collections::BTreeMap;
use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use volar_spec::garble::{Garble, GlobalSecret};
use volar_spec::vole::VoleArray;

use crate::strict::{
    STRICT_TABLE_CHUNK, decode_output_label, eliminate_nots, eval_strict_table_stream,
    garble_schedule_strict_dyn_full,
};
use crate::{
    Eval, ExternalBatchManifest, GateSchedule, MpcError, OtChannel, SessionFrame, Transport,
    arr_to_vec, vec_to_arr,
};

/// A chain input feed, per circuit-input bit. The feed script is shared by
/// both parties; each role uses only its own secret slice.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChainFeed {
    /// Public constant: the garbler encodes the value (from the shared
    /// `const_bits` slice) and ships the label in OwnedInputs.
    Const,
    /// Garbler-private input: the garbler encodes from its secret slice.
    Garbler,
    /// Evaluator-private input: 1-of-2 OT (the evaluator chooses on its
    /// secret slice).
    Eval,
    /// Threaded secret: reuse the held slot's label/base. Neither party
    /// decodes; the garbler aligns this input's base to the held base.
    Held(usize),
}

/// What happens to one circuit output bit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChainOut {
    /// Reveal via the strict OutputLabels/VerdictBits round (exact-match
    /// decode; a forged label aborts the session).
    Reveal,
    /// Thread through the configured role-local material store (never decoded).
    Hold(usize),
    /// Persist only the garbler's raw false-label base at `slot`. The
    /// evaluator retains its matching active label locally; it is not returned
    /// to the garbler, so neither party decodes the logical bit.
    GarblerMaterial(usize),
    /// Persist only the evaluator's active output label at `slot`. Nothing is
    /// sent to the garbler; no logical bit is decoded.
    EvaluatorMaterial(usize),
}

/// Public owner of a role-local material write.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MaterialRole {
    Garbler,
    Evaluator,
    /// Both roles persist their corresponding half of a threaded material.
    Both,
}

/// A contiguous allocation in the strict-chain role-local material store.
///
/// This is deliberately a small value object: TLS and interpreter drivers name
/// data regions once, then derive their input feeds/output dispositions from
/// the range instead of scattering manually coordinated slot arithmetic.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HeldRange {
    start: usize,
    len: usize,
}

impl HeldRange {
    /// Number of labels in this allocation.
    pub const fn len(self) -> usize {
        self.len
    }

    /// Whether this allocation contains no slots.
    pub const fn is_empty(self) -> bool {
        self.len == 0
    }

    /// The registry slot at `index`, or `None` when it is outside this range.
    pub const fn slot(self, index: usize) -> Option<usize> {
        if index < self.len {
            Some(self.start + index)
        } else {
            None
        }
    }

    /// Build held input feeds in range order.
    pub fn feeds(self) -> Vec<ChainFeed> {
        (0..self.len)
            .map(|i| ChainFeed::Held(self.start + i))
            .collect()
    }

    /// Build held output dispositions in range order.
    pub fn holds(self) -> Vec<ChainOut> {
        (0..self.len)
            .map(|i| ChainOut::Hold(self.start + i))
            .collect()
    }
}

/// Deterministic allocator for a chain driver's role-local material store.
///
/// Allocation is purely script construction: it does not allocate labels or
/// reveal values. Both parties use the same allocation sequence, which makes
/// held-state shape auditable before a session begins.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct HeldSlots {
    next: usize,
}

/// Per-chain execution counters for sizing jointly-secret state and strict
/// table streaming. Values are public protocol shape, never wire labels or
/// logical secret values.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ChainMetrics {
    /// Strict circuit rounds executed.
    pub rounds: u64,
    /// AND tables scheduled across all rounds.
    pub and_tables: u64,
    /// Garbler-to-evaluator strict table frames emitted (garbler side only).
    pub table_frames: u64,
    /// Payload bytes in those table frames, excluding transport framing.
    pub table_bytes: u64,
    /// Inputs loaded from role-local opaque material storage.
    pub held_inputs: u64,
    /// Outputs persisted as role-local opaque material.
    pub held_outputs: u64,
}

impl HeldSlots {
    /// Start with no allocated slots.
    pub const fn new() -> Self {
        Self { next: 0 }
    }

    /// Reserve `len` contiguous held slots. Panics on `usize` overflow rather
    /// than wrapping two independent secret regions onto the same labels.
    pub fn reserve(&mut self, len: usize) -> HeldRange {
        let start = self.next;
        self.next = self.next.checked_add(len).expect("held slot overflow");
        HeldRange { start, len }
    }

    /// Number of slots allocated so far.
    pub const fn len(&self) -> usize {
        self.next
    }

    /// Whether no slots have been allocated.
    pub const fn is_empty(&self) -> bool {
        self.next == 0
    }
}

/// A role-local storage phase. It runs only at an explicit chain round
/// boundary, before ordinary strict frames begin or after the verdict frame
/// completes. Implementations may therefore use the same transport and OT
/// channel for a multi-round durable-storage protocol without nesting frames.
pub trait ChainStoragePhase<N: VoleArray<u8>> {
    /// Run the public operation script in role-local storage state.
    fn run_storage_phase<D: Digest>(
        &mut self,
        operations: &[StorageOperation],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError>;
}

/// One explicit strict-chain boundary script.
///
/// Storage and external sections remain distinct: planner ordering decides
/// their relation, while this value only gives the chain one atomic admission
/// point before any role-local storage transaction begins.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ChainBoundaryScript {
    pub storage: Vec<StorageOperation>,
    pub external: Vec<ExternalBatchManifest>,
}

/// Opaque held-slot allocation reserved for one future external request's
/// reinserted result bits. It is public script geometry only: the garbler and
/// evaluator keep their respective opaque bases/labels in the allocated range.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExternalHeldResult {
    pub request_id: u64,
    pub slots: HeldRange,
}

/// Reserve result material for validated manifests without decoding any value.
///
/// Callers must run this while constructing the public chain script, before a
/// round begins. A malformed manifest allocates nothing; result slots are
/// contiguous in canonical manifest/action/output order.
pub fn reserve_external_held_results(
    slots: &mut HeldSlots,
    manifests: &[ExternalBatchManifest],
) -> Result<Vec<ExternalHeldResult>, MpcError> {
    let mut results = Vec::new();
    for manifest in manifests {
        manifest
            .validate()
            .map_err(|_| MpcError::MalformedSchedule)?;
        for action in &manifest.actions {
            results.push(ExternalHeldResult {
                request_id: action.request_id,
                slots: slots.reserve(action.output_bits),
            });
        }
    }
    Ok(results)
}

/// Execute an admitted chain boundary before an ordinary strict round.
///
/// Current chain drivers accept storage-only scripts. Any external manifest is
/// rejected *before* a storage prefetch/store executes, so an unavailable
/// action/oracle adapter cannot advance durable material state and then abort.
pub trait ChainBoundaryPhase<N: VoleArray<u8>>: ChainStoragePhase<N> {
    fn run_boundary_phase<D: Digest>(
        &mut self,
        script: &ChainBoundaryScript,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError>;
}

/// Admit a boundary script before it can mutate role-local durable state.
///
/// TODO(mpc-external-ledger: MPC-EXT-CHAIN-01): replace this storage-only
/// admission with the reviewed external batch executor. Until then, rejecting
/// before the storage phase preserves retry/epoch safety.
pub fn validate_chain_boundary_script(script: &ChainBoundaryScript) -> Result<(), MpcError> {
    if script.external.is_empty() {
        Ok(())
    } else {
        Err(MpcError::UnsupportedExternalPolicy)
    }
}

/// One public, fixed-shape storage operation. The role-local material is
/// supplied by the adapter; this script carries neither labels nor bases.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageOperation {
    /// Fetch the material at `slot` into the adapter's role-local cache.
    ///
    /// New callers should prefer [`Self::Prefetch`], whose name makes the
    /// required scheduling property explicit. `Load` is retained for existing
    /// scripts and has identical boundary-only semantics.
    Load { slot: usize, owner: MaterialRole },
    /// Pre-open durable split-AES held material into the role-local cache at
    /// an explicit chain-round boundary. This is not a host-side decrypt and
    /// must never be issued recursively during an ordinary strict round.
    ///
    /// This variant is intentionally provider-neutral: it prepares a later
    /// deferred/FHE chunk without naming an FHE scheme, key, or ciphertext.
    // TODO(v2-test): execute a `Prefetch` script through paired durable
    // adapters before a reviewed FHE provider consumes the subsequent chunk;
    // cover both material roles, `Both`, stale versions, and abort/retry.
    Prefetch { slot: usize, owner: MaterialRole },
    /// Persist the role-local material at `slot`, whose owning role is public.
    Store { slot: usize, owner: MaterialRole },
}

/// One party's chain-round driver. Implemented by [`ChainGarbler`] and
/// [`ChainEvaluator`]; a session script (e.g. the TLS 1.3 client driver) is
/// written once against this trait and executed by both parties with their
/// role's driver and secret inputs.
pub trait ChainParty<N: VoleArray<u8>> {
    /// Run one circuit of the chain. `feeds` has one entry per schedule
    /// input bit; `const_bits` the values of the `Const` feeds in order
    /// (ignored by the evaluator, which consumes the labels); `secret_bits`
    /// the caller's own secret values (`Garbler` feeds on the garbler side,
    /// `Eval` feeds on the evaluator side) in order; `outs` one entry per
    /// output wire. Returns the revealed output bits in output order.
    fn run_round<D: Digest, T: Transport>(
        &mut self,
        schedule: &GateSchedule,
        feeds: &[ChainFeed],
        const_bits: &[bool],
        secret_bits: &[bool],
        outs: &[ChainOut],
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<bool>, MpcError>;
}

/// Role-local backing for strict-chain wire material. A held bit is not a
/// Boolean value: the garbler stores its false-label base and the evaluator
/// stores its active label. A backing must preserve those opaque bytes exactly;
/// it must never decode, re-encode, or combine the two roles' materials.
///
/// This is the seam for the split-key encrypted ORAM material adapter. The
/// default [`MemoryHeldStore`] exists only for compatibility and small tests;
/// network deployments must inject a durable role-local backing with
/// [`ChainGarbler::with_held_store`] / [`ChainEvaluator::with_held_store`].
pub trait HeldMaterialStore<T, N: VoleArray<u8>> {
    /// Load one opaque role-local material item. Both roles invoke this before
    /// a strict round's ordinary frames begin, so a durable adapter may run a
    /// request/response fetch on the strict transport and its OT channel.
    fn load<D: Digest>(
        &mut self,
        slot: usize,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Option<T>, MpcError>;
    /// Persist one opaque role-local material item. Both roles invoke this at
    /// the corresponding output position, so a durable adapter may run a
    /// split-key encrypted ORAM write before the next strict round.
    fn store<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        value: Option<T>,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError>;
    /// Run a durable read at an explicit chain storage boundary. Implementors
    /// populate their local cache here; ordinary [`Self::load`] calls during a
    /// strict round must not start a nested transport protocol.
    fn prefetch<D: Digest>(
        &mut self,
        _slot: usize,
        _owner: MaterialRole,
        _transport: &mut dyn Transport,
        _ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        Ok(())
    }
    /// Persist staged role-local output at an explicit chain storage boundary.
    /// `store` only stages data so strict output framing is never nested with
    /// AES/OT material storage framing.
    fn flush<D: Digest>(
        &mut self,
        _slot: usize,
        _owner: MaterialRole,
        _transport: &mut dyn Transport,
        _ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        Ok(())
    }
    /// Number of initialized logical slots, for public resource accounting.
    fn len(&self) -> usize;
    /// Physical capacity consumed by the backing, for public accounting.
    fn capacity(&self) -> usize;
    /// Largest logical slot extent touched by this backing.
    fn address_span(&self) -> usize;
}

/// Compatibility backing for test/small-session material. It is deliberately
/// a narrow adapter, not part of strict-chain's protocol implementation.
#[derive(Clone, Debug)]
pub struct MemoryHeldStore<T> {
    values: BTreeMap<usize, T>,
    high_water: usize,
}

impl<T> Default for MemoryHeldStore<T> {
    fn default() -> Self {
        Self {
            values: BTreeMap::new(),
            high_water: 0,
        }
    }
}

impl<T: Clone, N: VoleArray<u8>> HeldMaterialStore<T, N> for MemoryHeldStore<T> {
    fn load<D: Digest>(
        &mut self,
        slot: usize,
        _: &mut dyn Transport,
        _: &mut dyn OtChannel<N>,
    ) -> Result<Option<T>, MpcError> {
        Ok(self.values.get(&slot).cloned())
    }

    fn store<D: Digest>(
        &mut self,
        slot: usize,
        _: MaterialRole,
        value: Option<T>,
        _: &mut dyn Transport,
        _: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        if let Some(value) = value {
            self.values.insert(slot, value);
            self.high_water = self.high_water.max(slot.saturating_add(1));
        }
        Ok(())
    }

    fn len(&self) -> usize {
        self.values.len()
    }

    fn capacity(&self) -> usize {
        self.values.len()
    }

    fn address_span(&self) -> usize {
        self.high_water
    }
}

fn material_load<N: VoleArray<u8>, S: HeldMaterialStore<T, N>, T, D: Digest>(
    held: &mut S,
    slot: usize,
    owner: MaterialRole,
    transport: &mut dyn Transport,
    ot: &mut dyn OtChannel<N>,
) -> Result<(), MpcError> {
    match owner {
        // A paired backing may cache a complete logical slot only when it sees
        // both role-local streams together. Invoke it once so it can skip the
        // entire paired protocol on a cache hit; splitting this into two calls
        // can leave one peer waiting for the other's AES/OT frames.
        MaterialRole::Both => held.prefetch::<D>(slot, MaterialRole::Both, transport, ot),
        role => held.prefetch::<D>(slot, role, transport, ot),
    }
}

fn material_store<N: VoleArray<u8>, S: HeldMaterialStore<T, N>, T, D: Digest>(
    held: &mut S,
    slot: usize,
    owner: MaterialRole,
    transport: &mut dyn Transport,
    ot: &mut dyn OtChannel<N>,
) -> Result<(), MpcError> {
    match owner {
        MaterialRole::Both => {
            held.flush::<D>(slot, MaterialRole::Garbler, transport, ot)?;
            held.flush::<D>(slot, MaterialRole::Evaluator, transport, ot)
        }
        role => held.flush::<D>(slot, role, transport, ot),
    }
}

/// The garbler's strict-chain role. Its backing contains only false-label
/// bases; it never contains evaluator labels or decoded logical values.
pub struct ChainGarbler<N: VoleArray<u8>, S = MemoryHeldStore<Garble<N>>> {
    secret: GlobalSecret<N>,
    held: S,
    fresh: u64,
    metrics: ChainMetrics,
}

impl<N: VoleArray<u8>> ChainGarbler<N> {
    /// A new driver using the compatibility memory backing. Production callers
    /// should use [`Self::with_held_store`] with a split-key ORAM material
    /// adapter instead.
    pub fn new(secret: GlobalSecret<N>) -> Self {
        Self::with_held_store(secret, MemoryHeldStore::default())
    }
}

impl<N: VoleArray<u8>, S: HeldMaterialStore<Garble<N>, N>> ChainGarbler<N, S> {
    /// Construct a garbler role with an injected role-local material backing.
    pub fn with_held_store(secret: GlobalSecret<N>, held: S) -> Self {
        Self {
            secret,
            held,
            fresh: 0,
            metrics: ChainMetrics::default(),
        }
    }

    /// Count of stored opaque false-label bases.
    pub fn held_len(&self) -> usize {
        self.held.len()
    }

    /// Physical backing capacity reported by the injected material adapter.
    pub fn held_capacity(&self) -> usize {
        self.held.capacity()
    }

    /// Largest logical held slot touched by the injected material adapter.
    pub fn held_address_span(&self) -> usize {
        self.held.address_span()
    }

    /// Consume the driver and return its role-local material backing. This is
    /// useful for public post-session accounting; it does not expose opaque
    /// values through the chain protocol.
    pub fn into_held(self) -> S {
        self.held
    }

    fn held_get<D: Digest>(
        &mut self,
        slot: usize,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Garble<N>, MpcError> {
        self.held
            .load::<D>(slot, transport, ot)?
            .ok_or(MpcError::MalformedSchedule)
    }

    fn held_set<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        value: Option<Garble<N>>,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        self.held.store::<D>(slot, owner, value, transport, ot)
    }

    /// Snapshot public execution-shape counters.
    pub fn metrics(&self) -> ChainMetrics {
        self.metrics
    }

    fn fresh_base<D: Digest>(&mut self) -> Garble<N> {
        self.fresh += 1;
        let h = D::new()
            .chain_update(b"volar-mpc/strict-chain-base")
            .chain_update(self.fresh.to_le_bytes())
            .finalize();
        let mut base = Array::<u8, N>::default();
        base.as_mut_slice().copy_from_slice(&h[..N::USIZE]);
        Garble { base }
    }
}

impl<N: VoleArray<u8>, S: HeldMaterialStore<Garble<N>, N>> ChainStoragePhase<N>
    for ChainGarbler<N, S>
{
    fn run_storage_phase<D: Digest>(
        &mut self,
        operations: &[StorageOperation],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        for operation in operations {
            match *operation {
                StorageOperation::Load { slot, owner }
                | StorageOperation::Prefetch { slot, owner } => {
                    material_load::<N, S, Garble<N>, D>(&mut self.held, slot, owner, transport, ot)?
                }
                StorageOperation::Store { slot, owner } => material_store::<N, S, Garble<N>, D>(
                    &mut self.held,
                    slot,
                    owner,
                    transport,
                    ot,
                )?,
            }
        }
        Ok(())
    }
}

impl<N: VoleArray<u8>, S: HeldMaterialStore<Garble<N>, N>> ChainBoundaryPhase<N>
    for ChainGarbler<N, S>
{
    fn run_boundary_phase<D: Digest>(
        &mut self,
        script: &ChainBoundaryScript,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        validate_chain_boundary_script(script)?;
        self.run_storage_phase::<D>(&script.storage, transport, ot)
    }
}

impl<N: VoleArray<u8>, S: HeldMaterialStore<Garble<N>, N>> ChainParty<N> for ChainGarbler<N, S> {
    fn run_round<D: Digest, T: Transport>(
        &mut self,
        schedule: &GateSchedule,
        feeds: &[ChainFeed],
        const_bits: &[bool],
        secret_bits: &[bool],
        outs: &[ChainOut],
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<bool>, MpcError> {
        let elim = eliminate_nots(schedule)?;
        let sched = &elim.schedule;
        self.metrics.rounds += 1;
        self.metrics.and_tables += sched.and_count() as u64;
        self.metrics.held_inputs += feeds
            .iter()
            .filter(|f| matches!(f, ChainFeed::Held(_)))
            .count() as u64;
        self.metrics.held_outputs += outs
            .iter()
            .filter(|o| matches!(o, ChainOut::Hold(_)))
            .count() as u64;
        if feeds.len() != sched.num_inputs || outs.len() != sched.output_wires().len() {
            return Err(MpcError::BadPartition);
        }

        // Input bases: fresh for Const/Garbler/Eval, aligned for Held.
        let mut bases: Vec<Garble<N>> = Vec::with_capacity(sched.num_inputs);
        let mut n_const = 0usize;
        let mut n_garbler = 0usize;
        for feed in feeds {
            match feed {
                ChainFeed::Const => {
                    n_const += 1;
                    bases.push(self.fresh_base::<D>());
                }
                ChainFeed::Garbler => {
                    n_garbler += 1;
                    bases.push(self.fresh_base::<D>());
                }
                ChainFeed::Eval => bases.push(self.fresh_base::<D>()),
                ChainFeed::Held(slot) => {
                    bases.push(self.held_get::<D>(*slot, transport, ot)?);
                }
            }
        }
        if const_bits.len() != n_const || secret_bits.len() != n_garbler {
            return Err(MpcError::BadPartition);
        }

        let full = garble_schedule_strict_dyn_full::<N, D>(&elim, self.secret.clone(), bases)?;

        // OwnedInputs: Const + Garbler labels in circuit-input order.
        let mut owned: Vec<Vec<u8>> = Vec::new();
        let mut const_i = 0usize;
        let mut gb_i = 0usize;
        for (idx, feed) in feeds.iter().enumerate() {
            let wire = &full.exec.circuit.input_labels[idx];
            match feed {
                ChainFeed::Const => {
                    let b = const_bits[const_i];
                    const_i += 1;
                    owned.push(arr_to_vec(&full.exec.circuit.secret.encode(wire, b).target));
                }
                ChainFeed::Garbler => {
                    let b = secret_bits[gb_i];
                    gb_i += 1;
                    owned.push(arr_to_vec(&full.exec.circuit.secret.encode(wire, b).target));
                }
                ChainFeed::Eval | ChainFeed::Held(_) => {}
            }
        }
        transport.send(&SessionFrame::OwnedInputs(owned).encode());

        // OT per Eval feed.
        for (idx, feed) in feeds.iter().enumerate() {
            if *feed == ChainFeed::Eval {
                let wire = &full.exec.circuit.input_labels[idx];
                let f = full.exec.circuit.secret.encode(wire, false);
                let t = full.exec.circuit.secret.encode(wire, true);
                ot.send([&f.target, &t.target]);
            }
        }

        // Stream after labels/OTs are available. The evaluator consumes each
        // chunk immediately, so a large jointly-secret ORAM/TLS round does
        // not materialize all tables at once.
        for tables in full.exec.circuit.tables.chunks(STRICT_TABLE_CHUNK) {
            self.metrics.table_frames += 1;
            self.metrics.table_bytes += (tables.len() * 4 * N::USIZE) as u64;
            transport.send(
                &SessionFrame::SetupStrictChunk {
                    tables: tables
                        .iter()
                        .map(|t| {
                            let mut rows: [Vec<u8>; 4] = Default::default();
                            for (r, row) in t.table.iter().enumerate() {
                                rows[r] = arr_to_vec(row);
                            }
                            rows
                        })
                        .collect(),
                }
                .encode(),
            );
        }
        transport.send(
            &SessionFrame::SetupStrictEnd {
                table_count: full.exec.circuit.tables.len() as u32,
            }
            .encode(),
        );

        // Revealed outputs: exact-match decode.
        let n_reveal = outs.iter().filter(|out| **out == ChainOut::Reveal).count();
        let n_labels_to_garbler = n_reveal;
        let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
        let labels = match frame {
            SessionFrame::OutputLabels(labels) => labels,
            _ => return Err(MpcError::UnexpectedMessage),
        };
        if labels.len() != n_labels_to_garbler {
            return Err(MpcError::UnexpectedMessage);
        }
        let mut revealed: Vec<bool> = Vec::with_capacity(n_reveal);
        let mut rev_i = 0usize;
        for (o, out) in outs.iter().enumerate() {
            match out {
                ChainOut::Reveal => {
                    let label =
                        vec_to_arr::<N>(&labels[rev_i]).ok_or(MpcError::MalformedSchedule)?;
                    rev_i += 1;
                    let bit = decode_output_label(
                        &full.exec.circuit.secret,
                        &full.exec.output_labels[o],
                        elim.output_polarity[o],
                        &label,
                    )
                    .ok_or(MpcError::DecodeFailure)?;
                    revealed.push(bit);
                }
                ChainOut::GarblerMaterial(slot) => {
                    // No evaluator label comes back on this path: returning it
                    // would let the garbler decode the material bit using its
                    // false base and delta. The label remains evaluator-local.
                    let raw = &full.exec.output_labels[o];
                    let base = if elim.output_polarity[o] {
                        Garble {
                            base: full.exec.circuit.secret.encode(raw, true).target,
                        }
                    } else {
                        raw.clone()
                    };
                    self.held_set::<D>(*slot, MaterialRole::Garbler, Some(base), transport, ot)?;
                }
                ChainOut::EvaluatorMaterial(slot) => {
                    // Advance the paired durable adapter at the same logical
                    // operation without giving the garbler an evaluator label.
                    self.held_set::<D>(*slot, MaterialRole::Evaluator, None, transport, ot)?;
                }
                ChainOut::Hold(slot) => {
                    // Thread: register the polarity-adjusted base so the held
                    // label encodes the LOGICAL value.
                    let raw = &full.exec.output_labels[o];
                    let base = if elim.output_polarity[o] {
                        Garble {
                            base: full.exec.circuit.secret.encode(raw, true).target,
                        }
                    } else {
                        raw.clone()
                    };
                    self.held_set::<D>(*slot, MaterialRole::Both, Some(base), transport, ot)?;
                }
            }
        }
        transport.send(&SessionFrame::VerdictBits(revealed.clone()).encode());
        Ok(revealed)
    }
}

/// The evaluator's chain driver: holds only the held slots' labels (never
/// the delta).
/// The evaluator's strict-chain role. Its backing contains only active labels;
/// it never contains garbler bases, the free-XOR delta, or decoded values.
pub struct ChainEvaluator<N: VoleArray<u8>, S = MemoryHeldStore<Eval<N>>> {
    held: S,
    _label: core::marker::PhantomData<N>,
}

impl<N: VoleArray<u8>> ChainEvaluator<N> {
    /// A new driver using the compatibility memory backing. Production callers
    /// should use [`Self::with_held_store`] with a split-key ORAM material
    /// adapter instead.
    pub fn new() -> Self {
        Self::with_held_store(MemoryHeldStore::default())
    }
}

impl<N: VoleArray<u8>, S: HeldMaterialStore<Eval<N>, N>> ChainEvaluator<N, S> {
    /// Construct an evaluator role with an injected role-local material backing.
    pub fn with_held_store(held: S) -> Self {
        Self {
            held,
            _label: core::marker::PhantomData,
        }
    }

    /// Number of stored opaque active labels.
    pub fn held_len(&self) -> usize {
        self.held.len()
    }

    /// Physical backing capacity reported by the injected material adapter.
    pub fn held_capacity(&self) -> usize {
        self.held.capacity()
    }

    /// Largest logical held slot touched by the injected material adapter.
    pub fn held_address_span(&self) -> usize {
        self.held.address_span()
    }

    /// Consume the driver and return its role-local material backing. This is
    /// useful for public post-session accounting; it does not expose opaque
    /// values through the chain protocol.
    pub fn into_held(self) -> S {
        self.held
    }

    fn held_get<D: Digest>(
        &mut self,
        slot: usize,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Eval<N>, MpcError> {
        self.held
            .load::<D>(slot, transport, ot)?
            .ok_or(MpcError::MalformedSchedule)
    }

    fn held_set<D: Digest>(
        &mut self,
        slot: usize,
        owner: MaterialRole,
        value: Option<Eval<N>>,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        self.held.store::<D>(slot, owner, value, transport, ot)
    }
}

impl<N: VoleArray<u8>> Default for ChainEvaluator<N> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;
    use crate::ActionSpec;

    #[test]
    fn external_results_reserve_contiguous_opaque_held_slots() {
        let mut slots = HeldSlots::new();
        let manifest = ExternalBatchManifest::from_actions(
            crate::ExternalBoundaryId(8),
            &[
                ActionSpec {
                    name: "one".into(),
                    request_id: 10,
                    action_ordinal: 0,
                    execution: crate::ActionExecutionPolicy::legacy_evaluator(),
                    guard: 0,
                    arg_wires: Vec::new(),
                    fallback_wires: vec![0],
                    num_bits: 1,
                    guard_polarity: false,
                    arg_polarity: Vec::new(),
                    fallback_polarity: Vec::new(),
                },
                ActionSpec {
                    name: "two".into(),
                    request_id: 11,
                    action_ordinal: 1,
                    execution: crate::ActionExecutionPolicy::legacy_evaluator(),
                    guard: 0,
                    arg_wires: Vec::new(),
                    fallback_wires: vec![0, 0],
                    num_bits: 2,
                    guard_polarity: false,
                    arg_polarity: Vec::new(),
                    fallback_polarity: Vec::new(),
                },
            ],
        )
        .unwrap();
        let results = reserve_external_held_results(&mut slots, &[manifest]).unwrap();
        assert_eq!(results[0].request_id, 10);
        assert_eq!(results[0].slots.slot(0), Some(0));
        assert_eq!(results[1].request_id, 11);
        assert_eq!(results[1].slots.slot(0), Some(1));
        assert_eq!(results[1].slots.slot(1), Some(2));
        assert_eq!(slots.len(), 3);
    }

    #[test]
    fn external_boundary_is_rejected_before_storage_admission() {
        let script = ChainBoundaryScript {
            storage: vec![StorageOperation::Prefetch {
                slot: 9,
                owner: MaterialRole::Both,
            }],
            external: vec![ExternalBatchManifest {
                boundary: crate::ExternalBoundaryId(0),
                actions: vec![],
            }],
        };
        assert_eq!(
            validate_chain_boundary_script(&script),
            Err(MpcError::UnsupportedExternalPolicy)
        );
    }

    #[test]
    fn held_slots_are_contiguous_and_scriptable_at_scale() {
        let mut slots = HeldSlots::new();
        let key_schedule = slots.reserve(1_024 * 8);
        let record_window = slots.reserve(64 * 1_024 * 8);
        let heap_image = slots.reserve(1_024 * 1_024 * 8);

        assert_eq!(key_schedule.slot(0), Some(0));
        assert_eq!(key_schedule.slot(key_schedule.len()), None);
        assert_eq!(record_window.slot(0), Some(key_schedule.len()));
        assert_eq!(
            heap_image.slot(0),
            Some(key_schedule.len() + record_window.len())
        );
        assert_eq!(
            slots.len(),
            key_schedule.len() + record_window.len() + heap_image.len()
        );
        assert_eq!(heap_image.feeds().len(), heap_image.len());
        assert_eq!(heap_image.holds().len(), heap_image.len());
        assert!(matches!(heap_image.feeds()[0], ChainFeed::Held(_)));
        assert!(matches!(heap_image.holds()[0], ChainOut::Hold(_)));
    }
}

impl<N: VoleArray<u8>, S: HeldMaterialStore<Eval<N>, N>> ChainStoragePhase<N>
    for ChainEvaluator<N, S>
{
    fn run_storage_phase<D: Digest>(
        &mut self,
        operations: &[StorageOperation],
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        for operation in operations {
            match *operation {
                StorageOperation::Load { slot, owner }
                | StorageOperation::Prefetch { slot, owner } => {
                    material_load::<N, S, Eval<N>, D>(&mut self.held, slot, owner, transport, ot)?
                }
                StorageOperation::Store { slot, owner } => {
                    material_store::<N, S, Eval<N>, D>(&mut self.held, slot, owner, transport, ot)?
                }
            }
        }
        Ok(())
    }
}

impl<N: VoleArray<u8>, S: HeldMaterialStore<Eval<N>, N>> ChainBoundaryPhase<N>
    for ChainEvaluator<N, S>
{
    fn run_boundary_phase<D: Digest>(
        &mut self,
        script: &ChainBoundaryScript,
        transport: &mut dyn Transport,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        validate_chain_boundary_script(script)?;
        self.run_storage_phase::<D>(&script.storage, transport, ot)
    }
}

impl<N: VoleArray<u8>, S: HeldMaterialStore<Eval<N>, N>> ChainParty<N> for ChainEvaluator<N, S> {
    fn run_round<D: Digest, T: Transport>(
        &mut self,
        schedule: &GateSchedule,
        feeds: &[ChainFeed],
        _const_bits: &[bool],
        secret_bits: &[bool],
        outs: &[ChainOut],
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<Vec<bool>, MpcError> {
        let elim = eliminate_nots(schedule)?;
        let sched = &elim.schedule;
        if feeds.len() != sched.num_inputs || outs.len() != sched.output_wires().len() {
            return Err(MpcError::BadPartition);
        }
        let n_eval = feeds.iter().filter(|f| **f == ChainFeed::Eval).count();
        if secret_bits.len() != n_eval {
            return Err(MpcError::BadPartition);
        }

        let owned_frame =
            SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
        let owned: Vec<Eval<N>> = match owned_frame {
            SessionFrame::OwnedInputs(labels) => labels
                .iter()
                .map(|l| {
                    Ok(Eval {
                        target: vec_to_arr(l).ok_or(MpcError::MalformedSchedule)?,
                    })
                })
                .collect::<Result<Vec<_>, MpcError>>()?,
            _ => return Err(MpcError::UnexpectedMessage),
        };

        let mut labels: Vec<Eval<N>> = Vec::with_capacity(sched.num_inputs);
        let mut owned_i = 0usize;
        let mut ev_i = 0usize;
        for feed in feeds.iter() {
            match feed {
                ChainFeed::Const | ChainFeed::Garbler => {
                    let l = owned.get(owned_i).ok_or(MpcError::BadPartition)?;
                    owned_i += 1;
                    labels.push(l.clone());
                }
                ChainFeed::Eval => {
                    let b = secret_bits[ev_i];
                    ev_i += 1;
                    labels.push(Eval {
                        target: ot.receive(b),
                    });
                }
                ChainFeed::Held(slot) => {
                    labels.push(self.held_get::<D>(*slot, transport, ot)?);
                }
            }
        }

        let out_labels = eval_strict_table_stream::<N, D, T>(sched, &labels, transport)?;

        let mut send_labels: Vec<Vec<u8>> = Vec::new();
        for (o, out) in outs.iter().enumerate() {
            match out {
                ChainOut::Reveal => {
                    send_labels.push(arr_to_vec(&out_labels[o].target));
                }
                ChainOut::GarblerMaterial(slot) => {
                    // Advance the paired durable adapter at the same logical
                    // operation without giving the evaluator a false base.
                    self.held_set::<D>(*slot, MaterialRole::Garbler, None, transport, ot)?;
                }
                ChainOut::EvaluatorMaterial(slot) => {
                    self.held_set::<D>(
                        *slot,
                        MaterialRole::Evaluator,
                        Some(out_labels[o].clone()),
                        transport,
                        ot,
                    )?;
                }
                ChainOut::Hold(slot) => {
                    self.held_set::<D>(
                        *slot,
                        MaterialRole::Both,
                        Some(out_labels[o].clone()),
                        transport,
                        ot,
                    )?;
                }
            }
        }
        transport.send(&SessionFrame::OutputLabels(send_labels).encode());
        let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
        match frame {
            SessionFrame::VerdictBits(bits) => Ok(bits),
            SessionFrame::Verdict(Err(())) => Err(MpcError::DecodeFailure),
            _ => Err(MpcError::UnexpectedMessage),
        }
    }
}
