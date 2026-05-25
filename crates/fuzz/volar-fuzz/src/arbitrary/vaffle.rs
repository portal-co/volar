//! `Arbitrary` impls for VAFFLE `Module`s.

use arbitrary::{Arbitrary, Result, Unstructured};
use vaffle::{FuncId, Module};

use crate::generators::ir::PRIM_TYPES;
use crate::generators::vaffle::{
    interpret_vaffle, interpret_vaffle_extended, interpret_vaffle_two_func,
};
use crate::interpreter::ir::{IrValue, primitive_width};
use crate::generators::ir::PRIM_TYPES as _PRIM_TYPES;

fn raw_stmt(u: &mut Unstructured<'_>) -> Result<(u8, u32, u32, u128, u128)> {
    Ok((
        u8::arbitrary(u)?,
        u32::arbitrary(u)?,
        u32::arbitrary(u)?,
        u128::arbitrary(u)?,
        u128::arbitrary(u)?,
    ))
}

/// A valid single-function, single-block VAFFLE module with matching inputs.
#[derive(Debug)]
pub struct ArbitraryVaffle {
    pub module: Module,
    pub func_id: FuncId,
    pub inputs: Vec<IrValue>,
}

impl<'a> Arbitrary<'a> for ArbitraryVaffle {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let n_params = u.int_in_range(0usize..=4)?;
        let raw_param_types: Vec<u8> = (0..n_params)
            .map(|_| u.int_in_range(0u8..=(PRIM_TYPES.len() as u8 - 1)))
            .collect::<Result<_>>()?;

        let n_stmts = u.int_in_range(0usize..=8)?;
        let raw_stmts: Vec<_> = (0..n_stmts).map(|_| raw_stmt(u)).collect::<Result<_>>()?;

        let (module, func_id, param_widths) = interpret_vaffle(&raw_param_types, &raw_stmts);

        let inputs: Vec<IrValue> = param_widths
            .iter()
            .map(|&w| (0..w).map(|_| bool::arbitrary(u)).collect::<Result<Vec<bool>>>())
            .collect::<Result<_>>()?;

        Ok(ArbitraryVaffle { module, func_id, inputs })
    }
}

/// Like [`ArbitraryVaffle`] but uses `interpret_vaffle_extended` so the module
/// may contain `StorageRead`/`StorageWrite` and `OracleCall`/`OracleOutput` values.
#[derive(Debug)]
pub struct ArbitraryVaffleExt {
    pub module: Module,
    pub func_id: FuncId,
    pub inputs: Vec<IrValue>,
}

impl<'a> Arbitrary<'a> for ArbitraryVaffleExt {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let n_params = u.int_in_range(0usize..=4)?;
        let raw_param_types: Vec<u8> = (0..n_params)
            .map(|_| u.int_in_range(0u8..=(PRIM_TYPES.len() as u8 - 1)))
            .collect::<Result<_>>()?;

        let n_stmts = u.int_in_range(0usize..=10)?;
        let raw_stmts: Vec<_> = (0..n_stmts).map(|_| raw_stmt(u)).collect::<Result<_>>()?;

        let (module, func_id, param_widths) = interpret_vaffle_extended(&raw_param_types, &raw_stmts);

        let inputs: Vec<IrValue> = param_widths
            .iter()
            .map(|&w| (0..w).map(|_| bool::arbitrary(u)).collect::<Result<Vec<bool>>>())
            .collect::<Result<_>>()?;

        Ok(ArbitraryVaffleExt { module, func_id, inputs })
    }
}

/// Two-function VAFFLE module exercising `Value::Call` + `Value::Output`.
///
/// func_0 calls func_1 and returns both its own values and the call outputs.
#[derive(Debug)]
pub struct ArbitraryVaffleTwoFunc {
    pub module: Module,
    pub func_id: FuncId,
    pub inputs: Vec<IrValue>,
}

impl<'a> Arbitrary<'a> for ArbitraryVaffleTwoFunc {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let n_params_f0 = u.int_in_range(0usize..=4)?;
        let raw_params_f0: Vec<u8> = (0..n_params_f0)
            .map(|_| u.int_in_range(0u8..=(PRIM_TYPES.len() as u8 - 1)))
            .collect::<Result<_>>()?;

        let n_stmts_f0 = u.int_in_range(0usize..=8)?;
        let raw_stmts_f0: Vec<_> = (0..n_stmts_f0).map(|_| raw_stmt(u)).collect::<Result<_>>()?;

        let n_params_f1 = u.int_in_range(0usize..=3)?;
        let raw_params_f1: Vec<u8> = (0..n_params_f1)
            .map(|_| u.int_in_range(0u8..=(PRIM_TYPES.len() as u8 - 1)))
            .collect::<Result<_>>()?;

        let n_stmts_f1 = u.int_in_range(0usize..=6)?;
        let raw_stmts_f1: Vec<_> = (0..n_stmts_f1).map(|_| raw_stmt(u)).collect::<Result<_>>()?;

        let (module, func_id, param_widths) = interpret_vaffle_two_func(
            &raw_params_f0,
            &raw_stmts_f0,
            &raw_params_f1,
            &raw_stmts_f1,
        );

        let inputs: Vec<IrValue> = param_widths
            .iter()
            .map(|&w| (0..w).map(|_| bool::arbitrary(u)).collect::<Result<Vec<bool>>>())
            .collect::<Result<_>>()?;

        Ok(ArbitraryVaffleTwoFunc { module, func_id, inputs })
    }
}
