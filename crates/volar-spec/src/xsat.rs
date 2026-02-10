use core::{mem::take, ops::BitXor};

use cipher::{
    ArrayLength,
    generic_array::{GenericArray, functional::FunctionalSequence, sequence::GenericSequence},
};
use digest::Digest;
#[derive(Clone, Copy)]
pub struct Item {
    pub target: usize,
    pub negated: bool,
}
pub struct SatProblem<K: ArrayLength<Item>, N: ArrayLength<GenericArray<Item, K>>> {
    pub clauses: GenericArray<GenericArray<Item, K>, N>,
}
impl<K: ArrayLength<Item>, N: ArrayLength<GenericArray<Item, K>>> SatProblem<K, N> {
    pub fn seal<D: Digest>(
        &self,
        mut seed: GenericArray<u8, D::OutputSize>,
    ) -> SealedSatProblem<K, N, D>
    where
        K: ArrayLength<GenericArray<u8, D::OutputSize>>,
        N: ArrayLength<OutClause<D, K>> + ArrayLength<GenericArray<u8, D::OutputSize>>,
    {
        let roots = self.clauses.clone().map(|a| {
            let root = seed.clone();
            seed = seed.clone().zip(D::digest(&seed), |a, b| a.bitxor(b));
            root
        });
        let troot = seed.clone();
        seed = D::digest(seed);
        SealedSatProblem {
            sealed_clauses: GenericArray::generate(|i| {
                let clause = self.clauses[i].clone();
                let mut p = seed.clone().map(|a| a ^ 0xff);
                seed = seed.clone().zip(D::digest(&seed), |a, b| a.bitxor(b));

                OutClause {
                    ixor_roots: clause.clone().map(|i| {
                        let root = roots[i.target].clone().zip(p.clone(), |a, b| a.bitxor(b));
                        if i.negated {
                            let trash = seed.clone();
                            seed = seed.clone().zip(D::digest(&seed), |a, b| a.bitxor(b));
                            return trash;
                        } else {
                            root
                        }
                    }),
                    xor_roots: clause.clone().map(|i| {
                        let root = roots[i.target].clone().zip(p.clone(), |a, b| a.bitxor(b));
                        if i.negated {
                            let q = p.clone();
                            p = p.clone().zip(D::digest(&p), |a, b| a.bitxor(b));
                            return q;
                        } else {
                            let trash = seed.clone();
                            seed = seed.clone().zip(D::digest(&seed), |a, b| a.bitxor(b));
                            return trash;
                        }
                    }),
                    target: p,
                }
            }),
            troot,
        }
    }
}
impl<
    K: ArrayLength<Item> + ArrayLength<GenericArray<u8, D::OutputSize>>,
    N: ArrayLength<OutClause<D, K>> + ArrayLength<GenericArray<Item, K>>,
    D: Digest,
> SealedSatProblem<K, N, D>
{
    pub fn open(
        &self,
        sat: &SatProblem<K, N>,
        targets: &[bool],
    ) -> GenericArray<u8, D::OutputSize> {
        self.sealed_clauses
            .iter()
            .zip(sat.clauses.iter())
            .fold(GenericArray::default(), |a, (clause, iclause)| {
                let target = clause
                    .xor_roots
                    .clone()
                    .zip(iclause, |a, b| {
                        if targets[b.target] {
                            GenericArray::generate(|_| 0)
                        } else {
                            a
                        }
                    })
                    .iter()
                    .fold(clause.target.clone(), |a, b| a.zip(b, |a, b| a.bitxor(b)));
                let mut seal = true;
                let target = clause
                    .ixor_roots
                    .clone()
                    .zip(iclause, |a, b| {
                        if targets[b.target] {
                            if take(&mut seal) {
                                a
                            } else {
                                GenericArray::generate(|_| 0)
                            }
                        } else {
                            GenericArray::generate(|_| 0)
                        }
                    })
                    .iter()
                    .fold(target, |a, b| a.zip(b, |a, b| a.bitxor(b)));
                a.zip(target, |a: u8, b: u8| a.bitxor(b))
            })
            .zip(self.troot.clone(), |a, b| a.bitxor(b))
    }
}
pub struct SealedSatProblem<
    K: ArrayLength<Item> + ArrayLength<GenericArray<u8, D::OutputSize>>,
    N: ArrayLength<OutClause<D, K>>,
    D: Digest,
> {
    pub sealed_clauses: GenericArray<OutClause<D, K>, N>,
    pub troot: GenericArray<u8, D::OutputSize>,
}
pub struct OutClause<D: Digest, K: ArrayLength<Item> + ArrayLength<GenericArray<u8, D::OutputSize>>>
{
    /// The i-th element is the root of the i-th variable if it appears non-negated, and a random value if it appears negated.
    pub xor_roots: GenericArray<GenericArray<u8, D::OutputSize>, K>,
    /// The i-th element is the root of the i-th variable if it appears negated, and a random value if it appears non-negated.
    pub ixor_roots: GenericArray<GenericArray<u8, D::OutputSize>, K>,

    pub target: GenericArray<u8, D::OutputSize>,
}
