//! `Arbitrary` impl for `BIrBlocks<()>`.

use arbitrary::{Arbitrary, Result, Unstructured};
use volar_ir::boolar::BIrBlocks;

use crate::generators::biir::{RawBlock, RawTarget, RawTerm, interpret_biir};

/// A newtype wrapper that implements [`Arbitrary`] for use in cargo-fuzz targets.
#[derive(Debug)]
pub struct ArbitraryBIir {
    pub blocks: BIrBlocks<()>,
    pub inputs: Vec<bool>,
}

impl<'a> Arbitrary<'a> for ArbitraryBIir {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        // Choose number of blocks (1–3) and param counts.
        let n_blocks = u.int_in_range(1usize..=3)?;
        let param_counts: Vec<u32> = (0..n_blocks)
            .map(|_| u.int_in_range(0u32..=4u32))
            .collect::<Result<_>>()?;

        let n_entry_params = param_counts[0] as usize;

        // Build raw blocks.
        let raw_blocks: Vec<RawBlock> = (0..n_blocks)
            .map(|_| {
                let n_stmts = u.int_in_range(0usize..=8)?;
                let raw_stmts = (0..n_stmts)
                    .map(|_| {
                        Ok((
                            u8::arbitrary(u)?,
                            u32::arbitrary(u)?,
                            u32::arbitrary(u)?,
                        ))
                    })
                    .collect::<Result<_>>()?;
                let raw_term = arb_raw_term(u)?;
                Ok((raw_stmts, raw_term))
            })
            .collect::<Result<_>>()?;

        let inputs: Vec<bool> = (0..n_entry_params)
            .map(|_| bool::arbitrary(u))
            .collect::<Result<_>>()?;

        let raw_ret_arity = u8::arbitrary(u)?;

        Ok(ArbitraryBIir {
            blocks: interpret_biir(param_counts, raw_blocks, raw_ret_arity),
            inputs,
        })
    }
}

fn arb_raw_term(u: &mut Unstructured<'_>) -> arbitrary::Result<RawTerm> {
    Ok((
        u8::arbitrary(u)?,
        u32::arbitrary(u)?,
        arb_raw_target(u)?,
        arb_raw_target(u)?,
    ))
}

fn arb_raw_target(u: &mut Unstructured<'_>) -> arbitrary::Result<RawTarget> {
    let n = u.int_in_range(0usize..=4)?;
    let args: Vec<u32> = (0..n)
        .map(|_| u32::arbitrary(u))
        .collect::<arbitrary::Result<_>>()?;
    Ok((u8::arbitrary(u)?, args))
}
