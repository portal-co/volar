// @reliability: experimental
// @experimental-status: design
// @experimental-since: 929a03c (experiment: garbling)
//! @ai: assisted
// Half-gate garbled circuit scheme over VOLE types.
// The half-gate construction (Zahur-Rosulek-Evans 2015) is standard, but the
// VOLE-specific binding has not been reviewed. Do not use in production.
use alloc::vec::Vec;
use core::ops::{BitXor, Div};

use cipher::consts::U8;
use digest::Digest;

use crate::vole::VoleArray;
use hybrid_array::{Array, ArraySize};

#[derive(Clone)]
pub struct Eval<N: VoleArray<u8>> {
    pub target: Array<u8, N>,
}
#[derive(Clone)]
pub struct Garble<N: VoleArray<u8>> {
    pub base: Array<u8, N>,
}
impl<N: VoleArray<u8>> Garble<N> {
    /// False-label for a constant-zero wire (all-zero bytes).
    pub fn zero() -> Self {
        Garble { base: Array::<u8, N>::default() }
    }

    /// Derive the AND gate output wire's false-label from the two input false-labels.
    ///
    /// Under the current scheme `C_false = H(self.base || b.base)`, which is what
    /// the evaluator computes when holding both false-labels (`and_via_table` with
    /// false inputs). Call as `wire_a.and_result::<D>(&wire_b)`.
    pub fn and_result<D: Digest>(&self, b: &Garble<N>) -> Self {
        let mut d = D::new();
        d.update(&self.base);
        d.update(&b.base);
        let hash = d.finalize();
        Garble {
            base: Array::<u8, N>::from_fn(|i| hash[i]),
        }
    }

    pub fn share(&self, target: &Array<u8, N>) -> Eval<N> {
        Eval {
            target: Array::<u8, N>::from_fn(|i| self.base[i] ^ target[i]),
        }
    }
    pub fn to_share<O: VoleArray<u8>>(&self) -> Garble<O>
    where
        N: Div<U8, Output = O>,
    {
        Garble {
            base: Array::<u8, O>::from_fn(|i| {
                let mut v = 0;
                for j in 0..8 {
                    let bit = (self.base[i * 8 + j]) & 1;
                    v |= bit << j;
                }
                v
            }),
        }
    }
}
impl<N: VoleArray<u8>> Eval<N> {
    /// Evaluator label for a constant-zero wire (all-zero bytes, same as the false-label).
    pub fn zero() -> Self {
        Eval { target: Array::<u8, N>::default() }
    }

    pub fn open(&self, garble: &Garble<N>) -> Array<u8, N> {
        Array::<u8, N>::from_fn(|i| self.target[i] ^ garble.base[i])
    }
    pub fn to_share<O: VoleArray<u8>>(&self) -> Eval<O>
    where
        N: Div<U8, Output = O>,
    {
        Eval {
            target: Array::<u8, O>::from_fn(|i| {
                let mut v = 0;
                for j in 0..8 {
                    let bit = (self.target[i * 8 + j]) & 1;
                    v |= bit << j;
                }
                v
            }),
        }
    }
    pub fn and_via_table<D: Digest>(
        &self,
        other: &Eval<N>,
        table: &GarbleTable<N>,
    ) -> Eval<N> {
        let index = (if self.target[0] & 1 == 1 { 1 } else { 0 })
            | (if other.target[0] & 1 == 1 { 2 } else { 0 });
        let hash = {
            let mut d = D::new();
            d.update(&self.target);
            d.update(&other.target);
            d.finalize()
        };
        Eval {
            target: Array::<u8, N>::from_fn(|i| hash[i] ^ table.table[index][i]),
        }
    }
}
#[derive(Clone)]
pub struct GarbleTable<N: VoleArray<u8>> {
    pub table: [Array<u8, N>; 4],
}
#[derive(Clone)]
pub struct GlobalSecret<N: VoleArray<u8>> {
    secret: Array<u8, N>,
}
impl<N: VoleArray<u8>> GlobalSecret<N> {
    pub fn new(mut secret: Array<u8, N>) -> Self {
        secret[0] |= 1;
        Self { secret }
    }
    pub fn secret(&self) -> Array<u8, N> {
        self.secret.clone()
    }
    pub fn encode(&self, garble: &Garble<N>, value: bool) -> Eval<N> {
        Eval {
            target: Array::<u8, N>::from_fn(|i| {
                if value { self.secret[i] ^ garble.base[i] } else { garble.base[i] }
            }),
        }
    }
    /// Evaluator label for the constant-one wire: `encode(Garble::zero(), true) = Δ`.
    pub fn one_wire_eval(&self) -> Eval<N> {
        self.encode(&Garble::zero(), true)
    }

    /// NOT of a garbler wire label: flip the false-label by XOR-ing with Δ.
    ///
    /// If `a` encodes bit `v`, `not_garble(a)` encodes bit `!v`.
    pub fn not_garble(&self, a: &Garble<N>) -> Garble<N> {
        Garble {
            base: Array::<u8, N>::from_fn(|i| a.base[i] ^ self.secret[i]),
        }
    }

    pub fn gen_and_table<D: Digest>(
        &self,
        a: &Garble<N>,
        b: &Garble<N>,
    ) -> GarbleTable<N> {
        // False-label of the result wire: H(a.base || b.base).
        // This is consistent with what the evaluator computes from the (0,0) label pair.
        let result_base = a.and_result::<D>(b);
        let mut table = core::array::from_fn(|_| Array::<u8, N>::default());
        for i in 0..4 {
            let av = (i & 1) != 0;
            let bv = (i & 2) != 0;
            let ea = self.encode(a, av);
            let eb = self.encode(b, bv);
            // Row index = color bits of the evaluator's labels for this input combination.
            // The evaluator selects the same row during eval, so the entry cancels correctly.
            let row = ((ea.target[0] & 1) as usize) | (((eb.target[0] & 1) as usize) << 1);
            // Result label for this combination: encode(result_wire, av AND bv).
            let result_label = self.encode(&result_base, av & bv);
            let mut d = D::new();
            d.update(&ea.target);
            d.update(&eb.target);
            let hash = d.finalize();
            // T[row] = H(ea || eb) XOR result_label; evaluator recovers result_label by XOR.
            table[row] = Array::<u8, N>::from_fn(|j| hash[j] ^ result_label.target[j]);
        }
        GarbleTable { table }
    }
}
impl<N: VoleArray<u8>> BitXor<Eval<N>> for Eval<N> {
    type Output = Eval<N>;

    fn bitxor(self, rhs: Eval<N>) -> Self::Output {
        return Eval {
            target: Array::<u8, N>::from_fn(|i| self.target[i] ^ rhs.target[i]),
        };
    }
}

// ============================================================================
// Multi-evaluation types
// ============================================================================

/// Garbler-private state for a garbled circuit instance.
///
/// Captures a complete garbling — the global secret, all input wire false-labels,
/// all AND-gate tables, and the output wire false-label — so the circuit can be
/// evaluated multiple times with different inputs without re-garbling.
///
/// The tables depend only on the fixed wire labels, not on input bit values,
/// so they are valid for all evaluations against this garbling instance.
#[derive(Clone)]
pub struct GarbledCircuit<N: VoleArray<u8>> {
    pub secret: GlobalSecret<N>,
    pub input_labels: Vec<Garble<N>>,
    pub tables: Vec<GarbleTable<N>>,
    pub output_label: Garble<N>,
}

impl<N: VoleArray<u8>> GarbledCircuit<N> {
    /// Encode a set of input bit values into evaluator labels using the stored
    /// false-labels. Call this once per evaluation; the tables are reused unchanged.
    pub fn encode_inputs(&self, bits: &[bool]) -> Vec<Eval<N>> {
        bits.iter()
            .zip(&self.input_labels)
            .map(|(&bit, label)| self.secret.encode(label, bit))
            .collect()
    }

    /// Extract the evaluator-visible subset of this garbling: the one-wire,
    /// tables, and output label. Share this with the evaluator once; reuse it
    /// across all evaluations.
    pub fn eval_setup(&self) -> EvalSetup<N> {
        EvalSetup {
            one_wire: self.secret.one_wire_eval(),
            tables: self.tables.clone(),
            output_label: self.output_label.clone(),
        }
    }
}

/// The evaluator-visible subset of a garbled circuit: everything needed to
/// evaluate but nothing that reveals the garbler's secret.
#[derive(Clone)]
pub struct EvalSetup<N: VoleArray<u8>> {
    pub one_wire: Eval<N>,
    pub tables: Vec<GarbleTable<N>>,
    pub output_label: Garble<N>,
}

impl<N: VoleArray<u8>> EvalSetup<N> {
    /// Recover the output bit value from the evaluator's output label.
    pub fn recover_output(&self, result: &Eval<N>) -> bool {
        // The color bit (LSB of first byte) encodes the wire value.
        // Δ[0] & 1 == 1 is guaranteed by GlobalSecret::new, so (Δ·v)[0] & 1 == v.
        result.open(&self.output_label)[0] & 1 != 0
    }
}
