//! `Arbitrary` impls for `IRBlocks<()>`.

use arbitrary::{Arbitrary, Result, Unstructured};
use volar_ir::ir::IRBlocks;
use volar_ir_common::TypeTable;

use crate::generators::ir::{interpret_ir, interpret_ir_extended, interpret_ir_multiblock, PRIM_TYPES};
use crate::interpreter::ir::{IrValue, primitive_width};

/// A newtype wrapper that implements [`Arbitrary`] for use in cargo-fuzz targets.
#[derive(Debug)]
pub struct ArbitraryIr {
    pub blocks: IRBlocks<()>,
    pub types: TypeTable,
    pub inputs: Vec<IrValue>,
}

impl<'a> Arbitrary<'a> for ArbitraryIr {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let n_params = u.int_in_range(0usize..=4)?;
        let raw_param_types: Vec<u8> = (0..n_params)
            .map(|_| u.int_in_range(0u8..=(PRIM_TYPES.len() as u8 - 1)))
            .collect::<Result<_>>()?;

        let n_stmts = u.int_in_range(0usize..=8)?;
        let raw_stmts: Vec<(u8, u32, u32, u128, u128)> = (0..n_stmts)
            .map(|_| {
                Ok((
                    u8::arbitrary(u)?,
                    u32::arbitrary(u)?,
                    u32::arbitrary(u)?,
                    u128::arbitrary(u)?,
                    u128::arbitrary(u)?,
                ))
            })
            .collect::<Result<_>>()?;

        let (blocks, types, param_widths) = interpret_ir(&raw_param_types, &raw_stmts);

        // Generate inputs matching param widths.
        let inputs: Vec<IrValue> = param_widths
            .iter()
            .map(|&w| {
                (0..w)
                    .map(|_| bool::arbitrary(u))
                    .collect::<Result<Vec<bool>>>()
            })
            .collect::<Result<_>>()?;

        Ok(ArbitraryIr { blocks, types, inputs })
    }
}

/// Like [`ArbitraryIr`] but uses [`interpret_ir_extended`] so the generated IR
/// may contain `StorageRead`/`StorageWrite` and `OracleCall`/`OracleOutput` stmts.
#[derive(Debug)]
pub struct ArbitraryIrExtended {
    pub blocks: IRBlocks<()>,
    pub types: TypeTable,
    pub inputs: Vec<IrValue>,
}

impl<'a> Arbitrary<'a> for ArbitraryIrExtended {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let n_params = u.int_in_range(0usize..=4)?;
        let raw_param_types: Vec<u8> = (0..n_params)
            .map(|_| u.int_in_range(0u8..=(PRIM_TYPES.len() as u8 - 1)))
            .collect::<Result<_>>()?;

        let n_stmts = u.int_in_range(0usize..=10)?;
        let raw_stmts: Vec<(u8, u32, u32, u128, u128)> = (0..n_stmts)
            .map(|_| {
                Ok((
                    u8::arbitrary(u)?,
                    u32::arbitrary(u)?,
                    u32::arbitrary(u)?,
                    u128::arbitrary(u)?,
                    u128::arbitrary(u)?,
                ))
            })
            .collect::<Result<_>>()?;

        let (blocks, types, param_widths) = interpret_ir_extended(&raw_param_types, &raw_stmts);

        let inputs: Vec<IrValue> = param_widths
            .iter()
            .map(|&w| {
                (0..w)
                    .map(|_| bool::arbitrary(u))
                    .collect::<Result<Vec<bool>>>()
            })
            .collect::<Result<_>>()?;

        Ok(ArbitraryIrExtended { blocks, types, inputs })
    }
}

/// Multi-block IR (two-block) with cross-block store-to-load forwarding support.
#[derive(Debug)]
pub struct ArbitraryIrMultiblock {
    pub blocks: IRBlocks<()>,
    pub types: TypeTable,
    pub inputs: Vec<IrValue>,
}

impl<'a> Arbitrary<'a> for ArbitraryIrMultiblock {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let n_params = u.int_in_range(0usize..=4)?;
        let raw_param_types: Vec<u8> = (0..n_params)
            .map(|_| u.int_in_range(0u8..=(PRIM_TYPES.len() as u8 - 1)))
            .collect::<Result<_>>()?;

        let n_stmts_b0 = u.int_in_range(0usize..=6)?;
        let raw_stmts_b0: Vec<(u8, u32, u32, u128, u128)> = (0..n_stmts_b0)
            .map(|_| Ok((u8::arbitrary(u)?, u32::arbitrary(u)?, u32::arbitrary(u)?, u128::arbitrary(u)?, u128::arbitrary(u)?)))
            .collect::<Result<_>>()?;

        let n_stmts_b1 = u.int_in_range(0usize..=6)?;
        let raw_stmts_b1: Vec<(u8, u32, u32, u128, u128)> = (0..n_stmts_b1)
            .map(|_| Ok((u8::arbitrary(u)?, u32::arbitrary(u)?, u32::arbitrary(u)?, u128::arbitrary(u)?, u128::arbitrary(u)?)))
            .collect::<Result<_>>()?;

        let (blocks, types, param_widths) =
            interpret_ir_multiblock(&raw_param_types, &raw_stmts_b0, &raw_stmts_b1);

        let inputs: Vec<IrValue> = param_widths
            .iter()
            .map(|&w| {
                (0..w)
                    .map(|_| bool::arbitrary(u))
                    .collect::<Result<Vec<bool>>>()
            })
            .collect::<Result<_>>()?;

        Ok(ArbitraryIrMultiblock { blocks, types, inputs })
    }
}
