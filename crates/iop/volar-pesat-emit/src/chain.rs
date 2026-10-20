// @reliability: experimental
// @ai: assisted
//! Chain manifest: the inter-step linking constraints for a stream of emitted
//! step instances (plan §3.2). Each step's `(pc, state)` output boundary is
//! the next step's input boundary; the link is a literal field equality per
//! boundary wire (B5), recorded here as a pairing of PESAT variable indices.

use volar_ir::ir::{IRBlock, IRBlockTargetId, IRTerminator, IRVarId};
use volar_iop::warp::PesatWitness;
use volar_primitives::Galois128;

use crate::emit::StepEmission;

/// The boundary pairing for one step-instance chain.
///
/// `boundary_in[k]` is the PESAT variable index (into `x || w`) of the k-th
/// boundary input bit of every step (the `(pc, state)` params — the public
/// prefix); `boundary_out[k]` is the index of the k-th boundary output bit
/// (the `(pc', state')` return values, in the witness region). The link
/// constraint is `out_i[k] == in_{i+1}[k]` for every step `i` and wire `k`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ChainManifest {
    /// PESAT variable count of the public (boundary-in) region.
    pub public_len: usize,
    /// Boundary-input variable indices (into the public region).
    pub boundary_in: Vec<usize>,
    /// Boundary-output variable indices (into `x || w`; these land in the
    /// witness region).
    pub boundary_out: Vec<usize>,
}

/// A link failure between two adjacent steps.
#[derive(Debug)]
pub enum LinkError {
    /// Step `step`'s boundary-out wire `wire` disagrees with step `step + 1`'s
    /// boundary-in.
    Mismatch {
        /// The earlier step's index in the chain.
        step: usize,
        /// The boundary wire position within the manifest.
        wire: usize,
    },
    /// A witness's public/witness lengths don't match the manifest.
    Shape {
        /// The step index in the chain.
        step: usize,
    },
}

impl core::fmt::Display for LinkError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Mismatch { step, wire } => {
                write!(f, "link mismatch at step {step}, boundary wire {wire}")
            }
            Self::Shape { step } => write!(f, "witness shape mismatch at step {step}"),
        }
    }
}

impl std::error::Error for LinkError {}

/// Derive the chain manifest for an emitted step circuit.
///
/// The boundary-in is the step block's whole parameter region (the `(pc,
/// state)` inputs — the public prefix). The boundary-out is the `(pc',
/// state')` portion of the step's return values: the movfuscated step circuit
/// returns `[done, pc', state'..., out...]` (see
/// `lower_to_circuit_ir`'s `WithTerminationFlag`), so the boundary-out return
/// args are positions `1..=n_params` — the done flag (position 0) and the
/// step's own outputs (positions after the state) are not fed back.
pub fn chain_manifest(emission: &StepEmission, step_block: &IRBlock<()>) -> ChainManifest {
    let n_params = step_block.params.len();
    let boundary_in: Vec<usize> = (0..emission.public_len).collect();
    let ret_args: &[IRVarId] = match &step_block.terminator {
        IRTerminator::Jmp { target } if target.dest == IRBlockTargetId::Return => &target.args,
        other => panic!("step block must return; got {other:?}"),
    };
    let mut boundary_out = Vec::new();
    for arg in ret_args.iter().skip(1).take(n_params) {
        let base = emission.var_base[arg.0 as usize];
        let width = emission.var_width[arg.0 as usize];
        boundary_out.extend(base..base + width);
    }
    assert_eq!(
        boundary_in.len(),
        boundary_out.len(),
        "boundary-in and boundary-out widths must match"
    );
    ChainManifest {
        public_len: emission.public_len,
        boundary_in,
        boundary_out,
    }
}

impl ChainManifest {
    /// A canonical text form (`public_len;boundary_in...;boundary_out...`,
    /// comma-separated indices), deterministic so a chain's manifest can be
    /// compared across runs and serialized with the chain. (The manifest is
    /// emitter metadata, not an IRBlocks, so the IR text format is not a fit;
    /// this is its canonical serialization.)
    pub fn to_canonical(&self) -> String {
        let join = |v: &[usize]| {
            v.iter()
                .map(usize::to_string)
                .collect::<Vec<_>>()
                .join(",")
        };
        format!(
            "{};{};{}",
            self.public_len,
            join(&self.boundary_in),
            join(&self.boundary_out)
        )
    }

    /// Parse the canonical form produced by [`ChainManifest::to_canonical`].
    pub fn from_canonical(s: &str) -> Option<Self> {
        let mut parts = s.split(';');
        let public_len = parts.next()?.parse().ok()?;
        let parse_list = |p: &str| -> Option<Vec<usize>> {
            if p.is_empty() {
                return Some(Vec::new());
            }
            p.split(',').map(|s| s.parse().ok()).collect()
        };
        let boundary_in = parse_list(parts.next()?)?;
        let boundary_out = parse_list(parts.next()?)?;
        Some(ChainManifest {
            public_len,
            boundary_in,
            boundary_out,
        })
    }
}

/// Verify every adjacent link in a chain of per-step witnesses (the native
/// traces). Each step's boundary-out must equal the next step's boundary-in.
pub fn verify_chain(
    manifest: &ChainManifest,
    traces: &[PesatWitness<Galois128>],
) -> Result<(), LinkError> {
    for i in 0..traces.len().saturating_sub(1) {
        let current = &traces[i];
        let next = &traces[i + 1];
        for (k, (&out_idx, &in_idx)) in manifest
            .boundary_out
            .iter()
            .zip(manifest.boundary_in.iter())
            .enumerate()
        {
            // boundary-out indices land in the witness region (offset by the
            // public prefix); boundary-in indices are in the public region.
            let out_val = current
                .witness
                .get(out_idx - manifest.public_len)
                .ok_or(LinkError::Shape { step: i })?;
            let in_val = next.public.get(in_idx).ok_or(LinkError::Shape { step: i + 1 })?;
            if out_val != in_val {
                return Err(LinkError::Mismatch { step: i, wire: k });
            }
        }
    }
    Ok(())
}
