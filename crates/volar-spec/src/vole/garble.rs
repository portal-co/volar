use core::ops::{BitXor, Div};

use cipher::{
    consts::U8,
    generic_array::{GenericArray, functional::FunctionalSequence, sequence::GenericSequence},
};
use digest::Digest;

use crate::vole::{Delta, Q, VoleArray, Vope};

pub struct Eval<N: VoleArray<u8>> {
    pub target: GenericArray<u8, N>,
}
pub struct Garble<N: VoleArray<u8>> {
    pub base: GenericArray<u8, N>,
}
impl<N: VoleArray<u8>> Garble<N> {
    pub fn share(&self, target: &GenericArray<u8, N>) -> Eval<N> {
        Eval {
            target: self.base.clone().zip(target.clone(), |a, b| a ^ b),
        }
    }
    pub fn to_share<O: VoleArray<u8>>(&self) -> Garble<O>
    where
        N: Div<U8, Output = O>,
    {
        Garble {
            base: GenericArray::<u8, O>::generate(|i| {
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
    pub fn open(&self, garble: &Garble<N>) -> GenericArray<u8, N> {
        self.target.clone().zip(garble.base.clone(), |a, b| a ^ b)
    }
    pub fn to_share<O: VoleArray<u8>>(&self) -> Eval<O>
    where
        N: Div<U8, Output = O>,
    {
        Eval {
            target: GenericArray::<u8, O>::generate(|i| {
                let mut v = 0;
                for j in 0..8 {
                    let bit = (self.target[i * 8 + j]) & 1;
                    v |= bit << j;
                }
                v
            }),
        }
    }
    pub fn and_via_table<D: Digest<OutputSize = N>>(
        &self,
        other: &Eval<N>,
        table: &[GenericArray<u8, N>; 4],
    ) -> Eval<N> {
        let index = (if self.target[0] & 1 == 1 { 1 } else { 0 })
            | (if other.target[0] & 1 == 1 { 2 } else { 0 });
        Eval {
            target: {
                let mut d = D::new();
                d.update(&self.target);
                d.update(&other.target);
                d.finalize()
            }
            .zip(table[index].clone(), |a, b| a ^ b),
        }
    }
}

pub struct GlobalSecret<N: VoleArray<u8>> {
    secret: GenericArray<u8, N>,
}
impl<N: VoleArray<u8>> GlobalSecret<N> {
    pub fn new(mut secret: GenericArray<u8, N>) -> Self {
        secret[0] |= 1;
        Self { secret }
    }
    pub fn secret(&self) -> GenericArray<u8, N> {
        self.secret.clone()
    }
    pub fn encode(&self, garble: &Garble<N>, value: bool) -> Eval<N> {
        Eval {
            target: self
                .secret
                .clone()
                .zip(garble.base.clone(), |a, b| if value { a ^ b } else { b }),
        }
    }
    pub fn gen_and_table<D: Digest<OutputSize = N>>(
        &self,
        a: &Garble<N>,
        b: &Garble<N>,
    ) -> [GenericArray<u8, N>; 4] {
        let mut table = core::array::from_fn(|_| GenericArray::<u8, N>::default());
        for i in 0..4 {
            let av = (i & 1) != 0;
            let bv = (i & 2) != 0;
            let mut d = D::new();
            d.update(&self.encode(a, av).target);
            d.update(&self.encode(b, bv).target);
            let target = d.finalize();
            let index =
                (if a.base[0] & 1 != 1 { 1 } else { 0 }) | (if b.base[0] & 1 != 1 { 2 } else { 0 });
            table[index] = table[index].clone().zip(target, |a, b| a ^ b);
        }
        table
    }
}
impl<N: VoleArray<u8>> BitXor<Eval<N>> for Eval<N> {
    type Output = Eval<N>;

    fn bitxor(self, rhs: Eval<N>) -> Self::Output {
        return Eval {
            target: self.target.zip(rhs.target, |a, b| a ^ b),
        };
    }
}
