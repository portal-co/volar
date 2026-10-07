// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Canonical bytes for [`BootstrapPlan`](crate::binfhe::plan::BootstrapPlan).
//!
//! This format is an adapter boundary, not an archive format. It uses a fixed
//! field order, little-endian fixed-width integers, packed least-significant-
//! first LUT bits, and rejects trailing bytes. The generic Cirrus variant
//! format deliberately has its own envelope and source binding; this module
//! provides the stable Volar-side plan view it consumes.
//!
//! @volar-allow-vec: runtime-boundary: the whole module is a byte adapter
//! for host-side plan (de)serialization; its buffers never cross into
//! generated target code. This module-level exemption applies to every item
//! below (encode/decode buffers and `put_*`/`Reader` helpers).

use alloc::vec;
use alloc::vec::Vec;

use crate::binfhe::plan::{
    BootstrapPlan, CellId, FailureBudget, LutId, LutSpec, PlanError, PlanOp, ProfileId, RgswId,
    WireId,
};

const MAGIC: &[u8; 4] = b"VBP1";
const VERSION: u8 = 1;
const MAX_ITEMS: usize = 1 << 20;

/// Why canonical plan encoding could not be produced.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EncodeError {
    /// The supplied schedule violates the plan's structural contracts.
    InvalidPlan(PlanError),
    /// A collection exceeds the bounded canonical adapter format.
    TooLarge,
}

/// Why canonical plan bytes were rejected.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DecodeError {
    /// The bytes do not begin with the canonical `VBP1` magic.
    BadMagic,
    /// The byte stream uses an unsupported canonical-plan version.
    UnsupportedVersion,
    /// A profile or operation tag is unknown.
    UnknownTag,
    /// A field or packed table extends past the input slice.
    Truncated,
    /// A declared count is too large for this bounded adapter format.
    TooLarge,
    /// The canonical encoding contains trailing bytes.
    TrailingBytes,
    /// The decoded structure violates [`BootstrapPlan::validate`].
    InvalidPlan(PlanError),
}

/// Encode a validated bootstrap plan into its unique canonical representation.
pub fn encode_plan(plan: &BootstrapPlan) -> Result<Vec<u8>, EncodeError> {
    plan.validate().map_err(EncodeError::InvalidPlan)?;
    if plan.luts.len() > MAX_ITEMS
        || plan.layers.len() > MAX_ITEMS
        || plan.outputs.len() > MAX_ITEMS
        || plan.cell_outputs.len() > MAX_ITEMS
        || plan.layers.iter().any(|layer| layer.len() > MAX_ITEMS)
        || plan.luts.iter().any(|lut| lut.entries.len() > MAX_ITEMS)
    {
        return Err(EncodeError::TooLarge);
    }

    let mut bytes = Vec::new();
    bytes.extend_from_slice(MAGIC);
    bytes.push(VERSION);
    bytes.push(profile_tag(plan.profile));
    put_u32(&mut bytes, plan.k_max);
    put_u32(&mut bytes, plan.num_inputs);
    put_u32(&mut bytes, plan.num_cells);
    put_u32(&mut bytes, plan.budget.per_bootstrap_log2);
    put_u32(&mut bytes, plan.budget.total_log2);
    put_u32(&mut bytes, plan.luts.len() as u32);
    for lut in &plan.luts {
        put_u32(&mut bytes, lut.entries.len() as u32);
        for chunk in lut.entries.chunks(8) {
            let mut packed = 0u8;
            for (bit, entry) in chunk.iter().enumerate() {
                packed |= (*entry as u8) << bit;
            }
            bytes.push(packed);
        }
    }
    put_u32(&mut bytes, plan.layers.len() as u32);
    for layer in &plan.layers {
        put_u32(&mut bytes, layer.len() as u32);
        for op in layer {
            match op {
                PlanOp::Const { out, value } => {
                    bytes.push(0);
                    put_u32(&mut bytes, *out);
                    bytes.push(*value as u8);
                }
                PlanOp::Not { input, out } => {
                    bytes.push(1);
                    put_u32(&mut bytes, *input);
                    put_u32(&mut bytes, *out);
                }
                PlanOp::Lut { inputs, table, out } => {
                    bytes.push(2);
                    put_u32(&mut bytes, inputs.len() as u32);
                    for input in inputs.as_slice() {
                        put_u32(&mut bytes, *input);
                    }
                    put_u32(&mut bytes, *table);
                    put_u32(&mut bytes, *out);
                }
                PlanOp::CircuitBootstrap { input, out } => {
                    bytes.push(3);
                    put_u32(&mut bytes, *input);
                    put_u32(&mut bytes, *out);
                }
                PlanOp::RgswMux {
                    sel,
                    then_cell,
                    else_cell,
                    out,
                } => {
                    bytes.push(4);
                    put_u32(&mut bytes, *sel);
                    put_u32(&mut bytes, *then_cell);
                    put_u32(&mut bytes, *else_cell);
                    put_u32(&mut bytes, *out);
                }
            }
        }
    }
    put_ids(&mut bytes, &plan.outputs);
    put_ids(&mut bytes, &plan.cell_outputs);
    Ok(bytes)
}

/// Decode exactly one canonical bootstrap plan.
pub fn decode_plan(bytes: &[u8]) -> Result<BootstrapPlan, DecodeError> {
    let mut reader = Reader { bytes, offset: 0 };
    if reader.take(4)? != MAGIC {
        return Err(DecodeError::BadMagic);
    }
    if reader.byte()? != VERSION {
        return Err(DecodeError::UnsupportedVersion);
    }
    let profile = parse_profile(reader.byte()?)?;
    let k_max = reader.u32()?;
    let num_inputs = reader.u32()?;
    let num_cells = reader.u32()?;
    let budget = FailureBudget {
        per_bootstrap_log2: reader.u32()?,
        total_log2: reader.u32()?,
    };
    let lut_count = reader.count()?;
    let mut luts = Vec::with_capacity(lut_count);
    for _ in 0..lut_count {
        let bit_count = reader.count()?;
        let packed_len = bit_count.div_ceil(8);
        let packed = reader.take(packed_len)?;
        if bit_count % 8 != 0
            && packed
                .last()
                .is_some_and(|byte| *byte >> (bit_count % 8) != 0)
        {
            return Err(DecodeError::UnknownTag);
        }
        let mut entries = Vec::with_capacity(bit_count);
        for bit in 0..bit_count {
            entries.push((packed[bit / 8] >> (bit % 8)) & 1 != 0);
        }
        luts.push(LutSpec { entries });
    }
    let layer_count = reader.count()?;
    let mut layers = Vec::with_capacity(layer_count);
    for _ in 0..layer_count {
        let op_count = reader.count()?;
        let mut layer = Vec::with_capacity(op_count);
        for _ in 0..op_count {
            layer.push(read_op(&mut reader)?);
        }
        layers.push(layer);
    }
    let outputs = reader.ids()?;
    let cell_outputs = reader.ids()?;
    if reader.offset != bytes.len() {
        return Err(DecodeError::TrailingBytes);
    }
    let plan = BootstrapPlan {
        profile,
        k_max,
        luts,
        layers,
        num_inputs,
        num_cells,
        outputs,
        cell_outputs,
        budget,
    };
    plan.validate().map_err(DecodeError::InvalidPlan)?;
    Ok(plan)
}

fn profile_tag(profile: ProfileId) -> u8 {
    match profile {
        ProfileId::Toy => 0,
        ProfileId::ToyNoisy => 1,
        ProfileId::Std128 => 2,
        ProfileId::Custom => 3,
    }
}

fn parse_profile(tag: u8) -> Result<ProfileId, DecodeError> {
    match tag {
        0 => Ok(ProfileId::Toy),
        1 => Ok(ProfileId::ToyNoisy),
        2 => Ok(ProfileId::Std128),
        3 => Ok(ProfileId::Custom),
        _ => Err(DecodeError::UnknownTag),
    }
}

fn put_u32(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn put_ids(bytes: &mut Vec<u8>, ids: &[u32]) {
    put_u32(bytes, ids.len() as u32);
    for id in ids {
        put_u32(bytes, *id);
    }
}

fn read_op(reader: &mut Reader<'_>) -> Result<PlanOp, DecodeError> {
    match reader.byte()? {
        0 => {
            let out = reader.u32()?;
            let value = match reader.byte()? {
                0 => false,
                1 => true,
                _ => return Err(DecodeError::UnknownTag),
            };
            Ok(PlanOp::Const { out, value })
        }
        1 => Ok(PlanOp::Not {
            input: reader.u32()?,
            out: reader.u32()?,
        }),
        2 => {
            let count = reader.count()?;
            let mut ids = alloc::vec::Vec::with_capacity(count);
            for _ in 0..count {
                ids.push(reader.u32()?);
            }
            let mut inputs = vec![0u32; ids.len()];
            for (i, id) in ids.iter().enumerate() {
                inputs[i] = *id;
            }
            let table: LutId = reader.u32()?;
            let out: WireId = reader.u32()?;
            Ok(PlanOp::Lut { inputs, table, out })
        }
        3 => Ok(PlanOp::CircuitBootstrap {
            input: reader.u32()?,
            out: reader.u32()?,
        }),
        4 => {
            let sel: RgswId = reader.u32()?;
            let then_cell = reader.u32()?;
            let else_cell = reader.u32()?;
            let out = reader.u32()?;
            Ok(PlanOp::RgswMux {
                sel,
                then_cell,
                else_cell,
                out,
            })
        }
        _ => Err(DecodeError::UnknownTag),
    }
}

struct Reader<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> Reader<'a> {
    fn take(&mut self, count: usize) -> Result<&'a [u8], DecodeError> {
        let end = self
            .offset
            .checked_add(count)
            .ok_or(DecodeError::Truncated)?;
        let bytes = self
            .bytes
            .get(self.offset..end)
            .ok_or(DecodeError::Truncated)?;
        self.offset = end;
        Ok(bytes)
    }

    fn byte(&mut self) -> Result<u8, DecodeError> {
        Ok(self.take(1)?[0])
    }

    fn u32(&mut self) -> Result<u32, DecodeError> {
        let bytes: [u8; 4] = self
            .take(4)?
            .try_into()
            .map_err(|_| DecodeError::Truncated)?;
        Ok(u32::from_le_bytes(bytes))
    }

    fn count(&mut self) -> Result<usize, DecodeError> {
        let count = self.u32()? as usize;
        if count > MAX_ITEMS {
            return Err(DecodeError::TooLarge);
        }
        Ok(count)
    }

    fn ids(&mut self) -> Result<Vec<u32>, DecodeError> {
        let count = self.count()?;
        let mut ids = Vec::with_capacity(count);
        for _ in 0..count {
            ids.push(self.u32()?);
        }
        Ok(ids)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn plan() -> BootstrapPlan {
        BootstrapPlan {
            profile: ProfileId::Toy,
            k_max: 2,
            luts: vec![LutSpec {
                entries: vec![false, true, true, false],
            }],
            layers: vec![vec![PlanOp::Lut {
                inputs: [0, 1].to_vec(),
                table: 0,
                out: 2,
            }]],
            num_inputs: 2,
            num_cells: 0,
            outputs: vec![2],
            cell_outputs: vec![],
            budget: FailureBudget {
                per_bootstrap_log2: 30,
                total_log2: 30,
            },
        }
    }

    #[test]
    fn round_trip_is_canonical() {
        let original = plan();
        let bytes = encode_plan(&original).unwrap();
        let decoded = decode_plan(&bytes).unwrap();
        assert_eq!(decoded, original);
        assert_eq!(encode_plan(&decoded).unwrap(), bytes);
    }

    #[test]
    fn rejects_trailing_and_non_boolean_bits() {
        let mut bytes = encode_plan(&plan()).unwrap();
        bytes.push(0);
        assert_eq!(decode_plan(&bytes), Err(DecodeError::TrailingBytes));

        let mut bytes = encode_plan(&plan()).unwrap();
        // The only 4-bit LUT byte immediately follows its length field.
        let table_offset = 4 + 1 + 1 + 5 * 4 + 4 + 4;
        bytes[table_offset] |= 0x80;
        assert_eq!(decode_plan(&bytes), Err(DecodeError::UnknownTag));
    }
}
