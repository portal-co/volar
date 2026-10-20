// @reliability: experimental
// @ai: assisted
//! Chain manifest: the inter-step linking constraints for a stream of emitted
//! step instances (plan §3.2). Each step's `(pc, state)` output boundary is
//! the next step's input boundary; the link is a literal field equality per
//! boundary wire (B5), recorded here as a pairing of PESAT variable indices.

use volar_ir::ir::IRBlock;
use volar_iop::warp::PesatWitness;
use volar_primitives::Galois128;

use crate::emit::StepEmission;
use crate::eval::{Access, bits_to_field};

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

/// Derive the chain manifest for an emitted step circuit. The boundary
/// (which variables link across steps) is computed at emission time and
/// carried on the [`StepEmission`]; this just packages it.
pub fn chain_manifest(emission: &StepEmission, _step_block: &IRBlock<()>) -> ChainManifest {
    ChainManifest {
        public_len: emission.public_len,
        boundary_in: emission.boundary_in.clone(),
        boundary_out: emission.boundary_out.clone(),
    }
}

/// Compute one step's memory-accumulator chain from its recorded accesses.
///
/// Returns `(acc_chain, mem_prod_out, mem_cons_out)`: one accumulator value
/// per access (in statement order, writes chaining `mem_prod` and reads
/// chaining `mem_cons`), then the boundary-out pair. The absorption shape is
/// `acc' = acc + r0 + addr·r1 + value·r2 + ts·r3` (the `mem_acc_absorb`
/// shape), matching the emitter's constraints.
pub fn step_accumulators(
    challenges: [Galois128; 4],
    mem_prod_in: Galois128,
    mem_cons_in: Galois128,
    ts: Galois128,
    accesses: &[Access],
) -> (Vec<Galois128>, Galois128, Galois128) {
    let [r0, r1, r2, r3] = challenges;
    let mut chain = Vec::with_capacity(accesses.len());
    let mut prod = mem_prod_in;
    let mut cons = mem_cons_in;
    for access in accesses {
        let addr = bits_to_field(&u64_to_bits(access.addr));
        let value = bits_to_field(&access.value);
        let next = if access.is_write {
            prod = prod + r0 + addr * r1 + value * r2 + ts * r3;
            prod
        } else {
            cons = cons + r0 + addr * r1 + value * r2 + ts * r3;
            cons
        };
        chain.push(next);
    }
    (chain, prod, cons)
}

/// `u64` → 64 bits, LSB-first.
fn u64_to_bits(v: u64) -> Vec<bool> {
    (0..64).map(|i| (v >> i) & 1 != 0).collect()
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
