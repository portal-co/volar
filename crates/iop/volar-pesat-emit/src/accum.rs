// @reliability: experimental
// @ai: assisted
//! Endless-stream accumulation driver (plan §4, E4). A possibly-unbounded step
//! stream is folded into a running WARP accumulator, one step instance per
//! `P_ACC`, resumable across calls via a continuation token (the
//! `ResumeToken` analogue): each call folds a segment and returns a token
//! carrying the running accumulator, so a later call resumes exactly where the
//! stream paused.

use volar_iop::warp::{
    LinearCode, PesatIndex, PesatWitness, WarpAccumulator, WarpProof, WarpResult, WarpSampling,
    warp_genesis, warp_prove,
};
use volar_primitives::Galois128;

/// A resumable accumulation token: the running WARP accumulator plus the
/// number of steps folded so far. This is the continuation a caller holds
/// between segments of an endless stream (plan §4's "multiple verifier
/// calls").
#[derive(Clone, Debug)]
pub struct StreamAccumulator {
    /// The running WARP accumulator after `steps` steps.
    pub acc: WarpAccumulator<Galois128>,
    /// How many step instances have been folded in.
    pub steps: usize,
}

/// Start a stream: `warp_genesis` over the first step's instance.
pub fn stream_start<C>(
    index: &PesatIndex<Galois128>,
    code: &C,
    first: &PesatWitness<Galois128>,
) -> WarpResult<StreamAccumulator>
where
    C: LinearCode<Galois128>,
{
    let acc = warp_genesis(index, code, first)?;
    Ok(StreamAccumulator { acc, steps: 1 })
}

/// Fold one more step instance into the running accumulator
/// (`acc = P_ACC(acc, step)`). Returns the new token and the step's proof.
pub fn stream_fold<C>(
    index: &PesatIndex<Galois128>,
    code: &C,
    sampling: WarpSampling,
    token: StreamAccumulator,
    next: &PesatWitness<Galois128>,
) -> WarpResult<(StreamAccumulator, WarpProof<Galois128>)>
where
    C: LinearCode<Galois128>,
{
    let steps = token.steps;
    let (acc, proof) = warp_prove(index, code, sampling, &[next.clone()], &[token.acc])?;
    Ok((StreamAccumulator { acc, steps: steps + 1 }, proof))
}

/// Fold a whole segment of steps, returning the new token. This is the
/// per-call entry point: a call processes `segment` and returns the token a
/// later call resumes from.
pub fn stream_fold_segment<'a, C>(
    index: &PesatIndex<Galois128>,
    code: &C,
    sampling: WarpSampling,
    mut token: StreamAccumulator,
    segment: impl IntoIterator<Item = &'a PesatWitness<Galois128>>,
) -> WarpResult<StreamAccumulator>
where
    C: LinearCode<Galois128>,
    Galois128: 'a,
{
    for witness in segment {
        token = stream_fold(index, code, sampling, token, witness)?.0;
    }
    Ok(token)
}
