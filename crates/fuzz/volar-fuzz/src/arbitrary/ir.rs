//! `Arbitrary` impl for `IRBlocks<()>`.

use arbitrary::{Arbitrary, Result, Unstructured};
use volar_ir::ir::IRBlocks;
use volar_ir_common::TypeTable;

use crate::generators::ir::{interpret_ir, PRIM_TYPES};
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
