//! Pre-FHE storage-boundary planning.
//!
//! This module deliberately contains **no FHE implementation, key material,
//! ciphertext format, or encryption call**. It prepares the scheduling facts
//! that are safe and useful today, so a future reviewed FHE provider can swap
//! a long chunk in at the one explicit seam:
//!
//! ```text
//! secret-address ORAM reads -> opaque results -> provider encrypt -> FHE chunk
//! durable held ciphertext -> scheduled split-AES open -> strict GC/ORAM chunk
//! ```
//!
//! The planner never sees a logical ORAM address, a held label, a plaintext,
//! or an FHE ciphertext. An ORAM request is named only by a public schedule
//! token; actual address handling remains in the strict GC/ORAM driver.
//!
//! ## V2 verification log
//!
//! No test in this module may attach an insecure, legacy, or unreviewed FHE
//! implementation merely to exercise this schedule. The exact deferred test
//! matrix is maintained in
//! `docs/handoffs/hybrid-storage-pre-fhe-infrastructure-v1.md` in the parent
//! repository. Every public constructor/method below has a corresponding
//! `TODO(v2-test)` marker at its semantic seam.

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;

use volar_ir::ir::{IRBlocks, IRStmt, IRType, IRTypeId, IRTypes, IRVarId};
use volar_ir_common::{Constant, PolyCoeffs, Type};
use volar_ir_passes::movfuscate_ir_with_boundary_and_watch;
use volar_mpc::strict_chain::{MaterialRole, StorageOperation};

use crate::oram_batch::{
    DisjointBelowRootWrites, PublicPathOp, ReadPathReuse, plan_disjoint_below_root_writes,
    plan_read_reuse,
};

/// Public identifier of a computation chunk which may later be replaced by an
/// FHE provider invocation. It conveys no cryptographic identity or provider
/// parameter and is deliberately not an FHE handle.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeferredChunkId(pub u32);

/// Public schedule token for one secret-address ORAM read. It is not an ORAM
/// address, physical leaf, ciphertext, or cache key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OramReadToken(pub u64);

/// Public position at which an opaque pre-run ORAM result becomes an input to
/// the deferred chunk's future provider adapter.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeferredInputSlot(pub u32);

/// One secret-address ORAM read that must run before the named deferred chunk.
///
/// `expected_oram_epoch` is the epoch *before* this ORAM access. A successful
/// access consumes exactly one epoch. The plan validates this public sequence
/// so a caller cannot accidentally use a result from a stale ORAM state.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OramReadPreRun {
    pub token: OramReadToken,
    pub expected_oram_epoch: u64,
    pub output: DeferredInputSlot,
}

/// One lazy opening demand for a split-AES durable held-material stream.
///
/// It lowers to [`StorageOperation::Prefetch`] at a chain-round boundary. The
/// operation opens role-local opaque material only; it never decrypts material
/// in either host process.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HeldMaterialPreOpen {
    pub slot: usize,
    pub owner: MaterialRole,
}

/// Public path operations eligible for existing public-address optimizations.
/// These are deliberately separate from [`OramReadPreRun`]: a secret-address
/// ORAM access must never be recast as a public host lookup.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PublicAddressStoragePlan {
    pub operations: Vec<PublicPathOp>,
}

impl PublicAddressStoragePlan {
    /// Build the already-available repeated-read and non-root-disjoint-write
    /// candidates. The caller still performs the physical root commits in
    /// order; no whole-Path-ORAM write parallelism is implied.
    // TODO(v2-test): exercise repeated reads, write barriers, shared
    // descendants, and root-commit ordering through the actual tree transport.
    pub fn read_reuse(&self) -> Vec<ReadPathReuse> {
        plan_read_reuse(&self.operations)
    }

    /// Return a write grouping only if all supplied public leaves are disjoint
    /// below the root. `None` means the normal sequential path is mandatory.
    // TODO(v2-test): drive grouped non-root I/O against a durable tree and
    // prove that roots still commit in original order.
    pub fn disjoint_below_root_writes(
        &self,
        levels: usize,
        leaves: &[u64],
    ) -> Option<DisjointBelowRootWrites> {
        plan_disjoint_below_root_writes(levels, leaves)
    }
}

/// The complete public plan immediately preceding one deferred computation
/// chunk. It is executable today only up to the strict-GC/ORAM and split-AES
/// storage seams. The future provider consumes `oram_reads`' opaque outputs
/// and must be added separately after review.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreFheStoragePlan {
    pub chunk: DeferredChunkId,
    pub oram_reads: Vec<OramReadPreRun>,
    pub held_material: Vec<HeldMaterialPreOpen>,
    pub public_address: PublicAddressStoragePlan,
    final_oram_epoch: u64,
}

/// Public schedule-shape errors. These fail closed before a protocol run;
/// callers must schedule a fresh strict boundary instead of repairing a plan
/// dynamically inside a movfuscated computation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PreFheStoragePlanError {
    /// A schedule token identified more than one ORAM request.
    DuplicateOramToken(OramReadToken),
    /// Two opaque ORAM results were assigned the same future input position.
    DuplicateDeferredInput(DeferredInputSlot),
    /// A read did not name the public epoch produced by its predecessor.
    NonContiguousOramEpoch { expected: u64, actual: u64 },
    /// Public epoch arithmetic overflowed; wrapping could bind stale state.
    OramEpochOverflow,
}

impl PreFheStoragePlan {
    /// Validate and construct a pre-FHE plan.
    ///
    /// ORAM reads retain the caller's declared order. Their expected epochs
    /// must be contiguous from `initial_oram_epoch`, because a normal Path
    /// ORAM read is itself a state-changing access. Held demands are
    /// canonicalized by slot: duplicate equal owners collapse; a garbler and
    /// evaluator demand for the same slot becomes one `Both` prefetch.
    // TODO(v2-test): run a reviewed provider after a real split ORAM pre-run;
    // assert opaque results map to the declared input slots without exposing
    // addresses, labels, plaintexts, or provider ciphertext frames.
    // TODO(v2-test): verify rejection of stale/non-contiguous epochs after a
    // failed ORAM transaction and a replayed manifest.
    pub fn new(
        chunk: DeferredChunkId,
        initial_oram_epoch: u64,
        oram_reads: Vec<OramReadPreRun>,
        held_material: Vec<HeldMaterialPreOpen>,
        public_address: PublicAddressStoragePlan,
    ) -> Result<Self, PreFheStoragePlanError> {
        let mut expected_epoch = initial_oram_epoch;
        let mut tokens = BTreeSet::new();
        let mut outputs = BTreeSet::new();
        for read in &oram_reads {
            if !tokens.insert(read.token) {
                return Err(PreFheStoragePlanError::DuplicateOramToken(read.token));
            }
            if !outputs.insert(read.output) {
                return Err(PreFheStoragePlanError::DuplicateDeferredInput(read.output));
            }
            if read.expected_oram_epoch != expected_epoch {
                return Err(PreFheStoragePlanError::NonContiguousOramEpoch {
                    expected: expected_epoch,
                    actual: read.expected_oram_epoch,
                });
            }
            expected_epoch = expected_epoch
                .checked_add(1)
                .ok_or(PreFheStoragePlanError::OramEpochOverflow)?;
        }

        Ok(Self {
            chunk,
            oram_reads,
            held_material: canonicalize_held_material(held_material),
            public_address,
            final_oram_epoch: expected_epoch,
        })
    }

    /// Epoch expected after every scheduled ORAM pre-run succeeds.
    // TODO(v2-test): bind this value to the strict ORAM driver's success-only
    // epoch and prove that aborts do not advance either side.
    pub const fn final_oram_epoch(&self) -> u64 {
        self.final_oram_epoch
    }

    /// Emit the explicit chain-boundary operations for lazy split-AES held
    /// material opening. They are safe to run now via `ChainStoragePhase`;
    /// the adapter owns all AES/OT work and ordinary strict rounds only read
    /// the resulting role-local cache.
    // TODO(v2-test): execute this script with the paired durable material
    // adapters, including `Both`, stale versions, eviction, and a later Held
    // feed. Do not use a host-side AES or an insecure FHE implementation.
    pub fn held_prefetch_operations(&self) -> Vec<StorageOperation> {
        self.held_material
            .iter()
            .map(|demand| StorageOperation::Prefetch {
                slot: demand.slot,
                owner: demand.owner,
            })
            .collect()
    }
}

fn canonicalize_held_material(demands: Vec<HeldMaterialPreOpen>) -> Vec<HeldMaterialPreOpen> {
    let mut owners = alloc::collections::BTreeMap::<usize, MaterialRole>::new();
    for demand in demands {
        owners
            .entry(demand.slot)
            .and_modify(|owner| *owner = merge_material_owner(*owner, demand.owner))
            .or_insert(demand.owner);
    }
    owners
        .into_iter()
        .map(|(slot, owner)| HeldMaterialPreOpen { slot, owner })
        .collect()
}

fn merge_material_owner(left: MaterialRole, right: MaterialRole) -> MaterialRole {
    match (left, right) {
        (MaterialRole::Both, _) | (_, MaterialRole::Both) => MaterialRole::Both,
        (MaterialRole::Garbler, MaterialRole::Garbler) => MaterialRole::Garbler,
        (MaterialRole::Evaluator, MaterialRole::Evaluator) => MaterialRole::Evaluator,
        (MaterialRole::Garbler, MaterialRole::Evaluator)
        | (MaterialRole::Evaluator, MaterialRole::Garbler) => MaterialRole::Both,
    }
}

/// A pre-movfuscation marker for one lazy split-AES held-material opening.
///
/// This is deliberately sidecar metadata rather than a new IR statement. The
/// marker names a concrete SSA value whose later consumer needs role-local held
/// material. The compiler is responsible for making that value/slot relation
/// before flattening; this module preserves the identity through the existing
/// movfuscation watch seam. It has no provider, key, plaintext, or ciphertext.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HeldMaterialMarker {
    pub block: usize,
    pub value: IRVarId,
    pub slot: usize,
    pub owner: MaterialRole,
}

/// A marker after 5a1 has tunneled its selected SSA value through
/// movfuscation. `movfuscated_value` is an IR variable identity, not a label
/// or runtime material value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TunneledHeldMaterialMarker {
    pub source: HeldMaterialMarker,
    pub movfuscated_value: IRVarId,
}

/// Fail-closed errors for the marker-based 5a1 pass.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HeldMarkerTunnelError {
    /// The source marker was not a statement result in the named block.
    InvalidSource { block: usize, value: IRVarId },
    /// Two markers named the same exact source SSA result.
    DuplicateSource { block: usize, value: IRVarId },
    /// The movfuscator did not return a watch result for one marker.
    MissingTunnel { block: usize, value: IRVarId },
}

/// Output of the marker-consuming 5a1 pass.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HeldMarkerTunnel<P: Clone> {
    pub movfuscated: IRBlocks<P>,
    pub markers: Vec<TunneledHeldMaterialMarker>,
}

/// 5a1: consume pre-movfuscation held-material markers and tunnel the marked
/// SSA values through movfuscation using its existing watch facility.
///
/// The markers are deliberately consumed at this seam: callers receive the
/// movfuscated program plus the exact renamed values and cannot accidentally
/// leave a marker to be rediscovered from flattened dataflow. The caller must
/// lower `markers` to a `PreFheStoragePlan`/`StorageOperation::Prefetch` phase
/// before executing the relevant strict/FHE chunk.
///
/// This is a tunnel only. It does not infer dataflow, execute a storage action,
/// emit an FHE call, or test an unreviewed FHE implementation.
// TODO(v2-test): construct markers from stable front-end instruction groups,
// then verify a reviewed-provider chunk consumes each tunneled result only
// after its paired split-AES prefetch has succeeded.
// TODO(v2-test): confirm pipeline-level rejection when a required marker has
// no registered consumer before movfuscation.
pub fn tunnel_held_material_markers<P: Clone>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    markers: &[HeldMaterialMarker],
) -> Result<HeldMarkerTunnel<P>, HeldMarkerTunnelError> {
    let mut watched = Vec::with_capacity(markers.len());
    let mut seen = BTreeSet::new();
    for marker in markers {
        let Some(block) = blocks.blocks.get(marker.block) else {
            return Err(HeldMarkerTunnelError::InvalidSource {
                block: marker.block,
                value: marker.value,
            });
        };
        let first_stmt = block.params.len() as u32;
        let end = first_stmt + block.stmts.len() as u32;
        if marker.value.0 < first_stmt || marker.value.0 >= end {
            return Err(HeldMarkerTunnelError::InvalidSource {
                block: marker.block,
                value: marker.value,
            });
        }
        if !seen.insert((marker.block, marker.value)) {
            return Err(HeldMarkerTunnelError::DuplicateSource {
                block: marker.block,
                value: marker.value,
            });
        }
        watched.push((marker.block, marker.value.0));
    }

    let (movfuscated, _boundaries, _accumulation, renames) =
        movfuscate_ir_with_boundary_and_watch(blocks, types, &watched);
    // The generic movfuscator returns a single-block program unchanged and
    // intentionally has no per-block emission/watch pass in that case. The
    // tunnel is therefore identity for a valid marker in its sole block.
    let renamed: BTreeMap<(usize, u32), IRVarId> = if blocks.blocks.len() == 1 {
        watched
            .iter()
            .map(|&(block, source)| ((block, source), IRVarId(source)))
            .collect()
    } else {
        renames
            .into_iter()
            .map(|(block, source, result)| ((block, source), IRVarId(result)))
            .collect()
    };
    let mut tunneled = Vec::with_capacity(markers.len());
    for marker in markers {
        let Some(&movfuscated_value) = renamed.get(&(marker.block, marker.value.0)) else {
            return Err(HeldMarkerTunnelError::MissingTunnel {
                block: marker.block,
                value: marker.value,
            });
        };
        tunneled.push(TunneledHeldMaterialMarker {
            source: *marker,
            movfuscated_value,
        });
    }
    Ok(HeldMarkerTunnel {
        movfuscated,
        markers: tunneled,
    })
}

/// One select recovered from a Boolean `Poly` statement. The representation is
/// normalized to `selector ? when_true : when_false`, where a source polynomial
/// must be exactly `selector * when_true + (selector + 1) * when_false` over
/// GF(2), i.e. the three nonzero monomials `{s,t}`, `{s,f}`, and `{f}`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InferredSelect {
    pub selector: IRVarId,
    pub when_true: IRVarId,
    pub when_false: IRVarId,
}

/// 5a2 output for one inferred select, retaining nested select dependencies.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InferredSelectNode {
    pub value: IRVarId,
    pub select: InferredSelect,
    /// Direct child select results. Callers can traverse this graph to find a
    /// maximal select region without assuming the entire polynomial is a MUX.
    pub nested_children: Vec<IRVarId>,
}

/// A select-shaped subset of a larger Boolean polynomial. `containing_value`
/// is still an ordinary arithmetic result: this record does **not** claim the
/// whole value can be replaced with a MUX.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InferredSelectFragment {
    pub containing_value: IRVarId,
    pub select: InferredSelect,
}

/// Conservative recovered select graph for one raw IR module. Statements that
/// mix a select polynomial with other arithmetic remain untouched but can
/// expose both existing select-producing inputs and inline select fragments.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InferredSelectGraph {
    pub selects: Vec<InferredSelectNode>,
    /// Earlier exact select results consumed by a non-select polynomial.
    pub embedded_selects: Vec<IRVarId>,
    /// Inline `x*a + x*b + b` subsets found within a larger polynomial.
    pub inline_select_fragments: Vec<InferredSelectFragment>,
}

/// 5a2: inspect raw Volar IR and recover exact GF(2) select identities from
/// `Poly` monomials, including nested selects and select results embedded in a
/// larger polynomial expression.
///
/// This recognises structure only. It never rewrites a polynomial, assumes
/// plaintext equality, opens material, or creates a storage request. A later
/// consumer may use its graph to form bounded lazy-held-material prefetch
/// regions; if it cannot prove a bounded public shape it must leave the
/// ordinary strict boundary intact.
// TODO(v2-test): run the recovery result through a reviewed provider/strict
// chain integration, compare an independently evaluated IR program before and
// after the consumer's eventual rewrite, and include nested/mixed polynomials.
// TODO(v2-test): fuzz canonical and noncanonical polynomial encodings after
// the provider-independent IR evaluator test seam is approved.
pub fn infer_selects_from_poly<P: Clone>(
    blocks: &IRBlocks<P>,
    types: &IRTypes,
) -> InferredSelectGraph {
    let mut exact = BTreeMap::<IRVarId, InferredSelect>::new();
    let mut embedded = BTreeSet::<IRVarId>::new();
    let mut inline_fragments = Vec::new();

    for block in &blocks.blocks {
        for (index, node) in block.stmts.iter().enumerate() {
            let value = IRVarId(block.params.len() as u32 + index as u32);
            if let IRStmt::Poly {
                ty,
                coeffs,
                constant,
            } = &node.kind
            {
                if let Some(select) = select_from_boolean_poly(*ty, types, coeffs, constant) {
                    exact.insert(value, select);
                } else {
                    for variable in polynomial_variables(coeffs) {
                        if exact.contains_key(&variable) {
                            embedded.insert(variable);
                        }
                    }
                    if is_boolean_poly(*ty, types, constant) {
                        for select in select_fragments(coeffs) {
                            inline_fragments.push(InferredSelectFragment {
                                containing_value: value,
                                select,
                            });
                        }
                    }
                }
            }
        }
    }

    let selects = exact
        .iter()
        .map(|(&value, &select)| {
            let nested_children = [select.selector, select.when_true, select.when_false]
                .into_iter()
                .filter(|input| exact.contains_key(input))
                .collect();
            InferredSelectNode {
                value,
                select,
                nested_children,
            }
        })
        .collect();
    InferredSelectGraph {
        selects,
        embedded_selects: embedded.into_iter().collect(),
        inline_select_fragments: inline_fragments,
    }
}

fn polynomial_variables(coeffs: &PolyCoeffs<IRVarId>) -> BTreeSet<IRVarId> {
    coeffs
        .iter()
        .flat_map(|(monomial, _)| monomial.iter().copied())
        .collect()
}

fn select_from_boolean_poly(
    ty: IRTypeId,
    types: &IRTypes,
    coeffs: &PolyCoeffs<IRVarId>,
    constant: &Constant,
) -> Option<InferredSelect> {
    if !is_boolean_poly(ty, types, constant) || coeffs.len() != 3 {
        return None;
    }
    let fragments = select_fragments(coeffs);
    (fragments.len() == 1).then_some(fragments[0])
}

fn is_boolean_poly(ty: IRTypeId, types: &IRTypes, constant: &Constant) -> bool {
    matches!(
        types.0.get(ty.0 as usize),
        Some(IRType::Primitive(Type::Bit))
    ) && constant_is_zero(constant)
}

/// Recover every unambiguous `x*a + x*b + b` subset with coefficient one.
/// This is intentionally syntactic: coefficient cancellation, type coercions,
/// and noncanonical encodings are not guessed at this planning seam.
fn select_fragments(coeffs: &PolyCoeffs<IRVarId>) -> Vec<InferredSelect> {
    let terms: Vec<&Vec<IRVarId>> = coeffs
        .iter()
        .filter_map(|(monomial, coefficient)| (*coefficient == 1).then_some(monomial))
        .collect();
    let singles: Vec<IRVarId> = terms
        .iter()
        .filter_map(|term| (term.len() == 1).then_some(term[0]))
        .collect();
    let pairs: Vec<&Vec<IRVarId>> = terms
        .iter()
        .copied()
        .filter(|term| term.len() == 2 && term[0] != term[1])
        .collect();
    let mut found = Vec::new();
    for when_false in singles {
        for false_pair in &pairs {
            let selector = if false_pair[0] == when_false {
                false_pair[1]
            } else if false_pair[1] == when_false {
                false_pair[0]
            } else {
                continue;
            };
            for true_pair in &pairs {
                if true_pair.as_slice() == false_pair.as_slice() || !true_pair.contains(&selector) {
                    continue;
                }
                let when_true = if true_pair[0] == selector {
                    true_pair[1]
                } else {
                    true_pair[0]
                };
                if when_true == when_false {
                    continue;
                }
                let select = InferredSelect {
                    selector,
                    when_true,
                    when_false,
                };
                if !found.contains(&select) {
                    found.push(select);
                }
            }
        }
    }
    found
}

fn constant_is_zero(constant: &Constant) -> bool {
    constant.hi == 0 && constant.lo == 0
}
