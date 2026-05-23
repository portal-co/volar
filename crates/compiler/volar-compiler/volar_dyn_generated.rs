//! Auto-generated dynamic types from volar-spec
//! Type-level lengths have been converted to runtime usize witnesses

#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]
extern crate alloc;
use alloc::vec::Vec;
use alloc::vec;
use core::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr};
use core::marker::PhantomData;
use typenum::Unsigned;
use cipher::BlockCipherEncrypt;
use digest::Digest;
use volar_common::hash_commitment::commit;
use volar_common::length_doubling::LengthDoubler;
use volar_primitives::{Bit, BitsInBytes, BitsInBytes64, Galois, Galois64};
use hybrid_array::Array;

/// Compute integer log2
#[inline]
pub fn ilog2(x: usize) -> u32 {
    usize::BITS - x.leading_zeros() - 1
}

/// Bridge: call LengthDoubler::double on a Vec<u8>, converting to/from Array
#[inline]
pub fn double_vec<B: LengthDoubler>(v: Vec<u8>) -> [Vec<u8>; 2] {
    let arr = hybrid_array::Array::try_from(v.as_slice()).expect("double_vec: length mismatch");
    let [a, b] = B::double(arr);
    [a.to_vec(), b.to_vec()]
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct U256(pub [u64; 4]);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Bit(pub bool);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois(pub u8);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BitsInBytes(pub u8);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois64(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BitsInBytes64(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois128(pub u128);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois256(pub U256);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Z3(pub u8);

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct TropicalDyn<T>(pub T);

#[derive(Debug, Default)]
pub struct ViaDigestPuncturableRandomizerDyn<D: Digest> {
    pub digest: PhantomData<D>,
}

#[derive(Debug, Default)]
pub struct CommitmentCoreDyn<D: Digest>(pub Vec<u8>, pub PhantomData<D>);

#[derive(Clone, Copy, Default)]
pub struct NoReduction {
}

#[derive(Clone, Copy)]
pub struct GrafhenWordDyn {
    pub wbound: usize,
    pub data: [u8; WBOUND],
    pub len: usize,
}

#[derive(Clone)]
pub struct GrafhenKeyDyn {
    pub n: usize,
    pub d: usize,
    pub gens: [[u8; N]; D],
    pub inv_gens: [[u8; N]; D],
}

#[derive(Clone)]
pub struct GrafhenPublicDyn<R> {
    pub wbound: usize,
    pub enc_one: GrafhenWordDyn,
    pub and_w1: GrafhenWordDyn,
    pub and_w2: GrafhenWordDyn,
    pub reducer: R,
}

#[derive(Debug, Default)]
pub struct DeltaDyn<T> {
    pub n: usize,
    pub delta: Vec<T>,
}

#[derive(Debug, Default)]
pub struct QDyn<T> {
    pub n: usize,
    pub q: Vec<T>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UniversalHashKey {
    pub r0: Galois128,
    pub r1: Galois64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UniversalHashOutput {
    pub h0: Galois128,
    pub h1: Galois64,
}

#[derive(Debug, Default)]
pub struct AesCtrLengthDoubler {
}

#[derive(Debug, Default)]
pub struct RoLeafCommitDyn<D: Digest> {
    pub _d: PhantomData<D>,
}

#[derive(Debug, Default)]
pub struct EmLeafCommit {
}

#[derive(Debug, Default)]
pub struct FaestTranscript {
    pub sponge: Sponge,
}

#[derive(Debug, Default)]
pub struct ConvertOutput {
    pub u: Vec<u8>,
    pub v: Vec<Vec<u8>>,
}

#[derive(Debug, Default)]
pub struct BigVoleProver {
    pub u: Vec<u8>,
    pub c: Vec<Vec<u8>>,
    pub v_columns: Vec<Vec<u8>>,
}

#[derive(Debug, Default)]
pub struct BigVoleVerifier {
    pub q_columns: Vec<Vec<u8>>,
}

#[derive(Debug, Default)]
pub struct BavcCommitmentDyn {
    pub com_bytes: usize,
    pub root: Vec<u8>,
    pub vec_hashes: Vec<Vec<u8>>,
    pub seeds: Vec<[u8; LAMBDA_BYTES]>,
    pub commitments: Vec<[u8; COM_BYTES]>,
}

#[derive(Debug, Default)]
pub struct BavcOpeningDyn {
    pub com_bytes: usize,
    pub hidden_commits: Vec<[u8; COM_BYTES]>,
    pub nodes: Vec<(usize, [u8; LAMBDA_BYTES])>,
}

#[derive(Debug, Default)]
pub struct BavcDyn<L> {
    pub com_bytes: usize,
    pub _l: PhantomData<L>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FaestSecretKey(pub [u8; LAMBDA_BYTES]);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FaestPublicKey(pub [u8; LAMBDA_BYTES]);

#[derive(Clone, Debug)]
pub struct FaestSignature {
    pub iv: [u8; LAMBDA_BYTES],
    pub bavc_root: Vec<u8>,
    pub hidden_commits: Vec<[u8; COM_BYTES]>,
    pub nodes: Vec<(usize, [u8; LAMBDA_BYTES])>,
    pub corrections: Vec<Vec<u8>>,
    pub vole_u: Vec<u8>,
    pub qs_proof: QuickSilverProof,
    pub c_hat_with_counter: Vec<u8>,
    pub chall_3: Vec<u8>,
    pub counter: u32,
}

#[derive(Clone, Debug)]
pub struct QuickSilverProof {
    pub a_hat: Vec<u8>,
    pub b_hat: Vec<u8>,
    pub c_hat_base: Vec<u8>,
}

#[derive(Debug, Default)]
pub struct StubFaestAesProver {
}

#[derive(Debug, Default)]
pub struct ABODyn<B: LengthDoubler, D: Digest> {
    pub k: usize,
    pub n: usize,
    pub commit: Vec<u8>,
    pub per_byte: Vec<Vec<Vec<u8>>>,
    pub _phantom: PhantomData<(B, D)>,
}

#[derive(Debug, Default)]
pub struct ABOOpeningDyn<B: LengthDoubler, D: Digest> {
    pub t: usize,
    pub u: usize,
    pub n: usize,
    pub bad: Vec<u64>,
    pub openings: Vec<Vec<Vec<Vec<u8>>>>,
    pub _phantom: PhantomData<(B, D)>,
}

#[derive(Debug, Default)]
pub struct BSplitDyn<B: LengthDoubler, D: Digest> {
    pub split: Vec<[Vec<u8>; 2]>,
    pub _phantom: PhantomData<(B, D)>,
}

#[derive(Debug, Default)]
pub struct PolyDyn<T> {
    pub n: usize,
    pub c0: T,
    pub c1: Vec<T>,
}

#[derive(Debug, Default)]
pub struct PolyInputPoolDyn<T> {
    pub n: usize,
    pub x: usize,
    pub inputs: Vec<T>,
    pub indices: Vec<Vec<usize>>,
}

#[derive(Debug, Default)]
pub struct AdditiveHasher {
}

#[derive(Clone)]
pub struct ChallengeKeyDyn<T> {
    pub r1: T,
    pub r2: T,
    pub r3: T,
}

#[derive(Clone)]
pub struct MemoryCheckStateDyn<T, H: MemoryHasher<T>> {
    pub key: ChallengeKeyDyn<T>,
    pub produce: <H as _>::State,
    pub consume: <H as _>::State,
    pub _phantom: PhantomData<H>,
}

#[derive(Debug, Default)]
pub struct BitVoleDyn<T> {
    pub n: usize,
    pub u: Vec<Bit>,
    pub v: Vec<T>,
}

#[derive(Debug, Default)]
pub struct VopeDyn<T> {
    pub n: usize,
    pub k: usize,
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
}

#[derive(Debug, Default)]
pub struct LweSampleDyn<T, U> {
    pub n: usize,
    pub m: usize,
    pub matrix: Vec<Vec<T>>,
    pub b: Vec<U>,
}

#[derive(Clone)]
pub struct EvalDyn {
    pub n: usize,
    pub target: Vec<u8>,
}

#[derive(Clone)]
pub struct GarbleDyn {
    pub n: usize,
    pub base: Vec<u8>,
}

#[derive(Clone)]
pub struct GarbleTableDyn {
    pub n: usize,
    pub table: [Vec<u8>; 4],
}

#[derive(Clone)]
pub struct GlobalSecretDyn {
    pub n: usize,
    pub secret: Vec<u8>,
}

#[derive(Clone)]
pub struct GarbledCircuitDyn {
    pub n: usize,
    pub i: usize,
    pub a: usize,
    pub secret: GlobalSecretDyn,
    pub input_labels: [GarbleDyn; I],
    pub tables: [GarbleTableDyn; A],
    pub output_label: GarbleDyn,
}

#[derive(Clone)]
pub struct EvalSetupDyn {
    pub n: usize,
    pub a: usize,
    pub one_wire: EvalDyn,
    pub tables: [GarbleTableDyn; A],
    pub output_label: GarbleDyn,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LweCiphertextDyn {
    pub n_lwe: usize,
    pub a: [u32; N_LWE],
    pub b: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RlweCiphertextDyn {
    pub big_n: usize,
    pub a: [u32; BIG_N],
    pub b: [u32; BIG_N],
}

#[derive(Clone, Copy, Debug)]
pub struct RgswRowDyn {
    pub big_n: usize,
    pub rlwe0: RlweCiphertextDyn,
    pub rlwe1: RlweCiphertextDyn,
}

#[derive(Clone, Copy, Debug)]
pub struct RgswCiphertextDyn {
    pub big_n: usize,
    pub bs_ell: usize,
    pub rows: [RgswRowDyn; BS_ELL],
}

#[derive(Clone, Debug)]
pub struct KeySwitchingKeyDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub ks_ell: usize,
    pub ksk: [[LweCiphertextDyn; KS_ELL]; BIG_N],
    pub ks_bg_log: u32,
}

#[derive(Clone, Debug)]
pub struct BootstrappingKeyDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub bs_ell: usize,
    pub ks_ell: usize,
    pub bsk: [RgswCiphertextDyn; N_LWE],
    pub ksk: KeySwitchingKeyDyn,
    pub bs_bg_log: u32,
}

#[derive(Clone, Debug)]
pub struct LweSecretKeyDyn {
    pub n_lwe: usize,
    pub key: [u8; N_LWE],
}

#[derive(Clone, Debug)]
pub struct RlweSecretKeyDyn {
    pub big_n: usize,
    pub key: [u32; BIG_N],
}

#[derive(Debug, Default)]
pub struct AllPartiesDyn<T> {
    pub n: usize,
    pub other_parties: OtherPartiesDyn<T>,
    pub self_party: T,
}

#[derive(Debug, Default)]
pub struct OtherPartiesDyn<T> {
    pub n: usize,
    pub other_parties: Vec<T>,
}

#[derive(Debug, Default)]
pub struct SoftSpokenOutDyn<D: Digest> {
    pub m: usize,
    pub l: usize,
    pub sender_r0: [[u8; L]; M],
    pub receiver_v: [[u8; L]; M],
    pub sender_tag: Output<D>,
    pub receiver_tag: Output<D>,
}

#[derive(Debug, Default)]
pub struct LweOtCrs {
    pub a: [[Zq; LWE_N]; LWE_N],
    pub h: [Zq; LWE_N],
}

#[derive(Debug, Default)]
pub struct LweOtReceiver {
    pub s: [Zq; LWE_N],
    pub c: bool,
}

#[derive(Debug, Default)]
pub struct LweOtRecvMsg {
    pub pk0: [Zq; LWE_N],
}

#[derive(Debug, Default)]
pub struct LweOtSenderMsgDyn {
    pub l: usize,
    pub u0: [Zq; LWE_N],
    pub v0: [Zq; L],
    pub u1: [Zq; LWE_N],
    pub v1: [Zq; L],
}

#[derive(Debug, Default)]
pub struct BaseOtSenderDyn<G: Group, D: Digest> {
    pub y: <G as _>::Scalar,
    pub s: <G as _>::Element,
    pub t: <G as _>::Element,
    pub _d: PhantomData<D>,
    pub _phantom: PhantomData<G>,
}

#[derive(Debug, Default)]
pub struct BaseOtReceiverDyn<G: Group, D: Digest> {
    pub x: <G as _>::Scalar,
    pub s: <G as _>::Element,
    pub c: bool,
    pub _d: PhantomData<D>,
    pub _phantom: PhantomData<G>,
}

#[derive(Debug, Default)]
pub struct OtReceiverMsgDyn<G: Group> {
    pub r: <G as _>::Element,
    pub _phantom: PhantomData<G>,
}

#[derive(Debug, Default)]
pub struct IdealCotDyn<T> {
    pub n: usize,
    pub delta: DeltaDyn<T>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ToyElement(pub u64);

#[derive(Debug, Default)]
pub struct ToyGroup {
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Fe25519(pub [u64; 4]);

#[derive(Clone, Copy, Debug)]
pub struct EdPoint {
    pub x: Fe25519,
    pub y: Fe25519,
    pub z: Fe25519,
    pub t: Fe25519,
}

#[derive(Debug, Default)]
pub struct Ed25519 {
}

#[derive(Debug)]
pub enum Sponge {
    Shake128(Shake128),
    Shake256(Shake256),
}

pub trait FieldMulBackend: ShlAssign<u32> + ShrAssign<u32> + Default + BitXor<Output = Self> + BitXorAssign + Shl<u32, Output = Self> + BitAnd<Output = Self> + PartialEq + From<u8> + Clone {
}

pub trait Invert {
    fn invert(&self) -> Self;
}

pub trait PuncturableLengthDoubler: LengthDoubler {
}

pub trait WordReducer: Clone {
    fn reduce(&self, word: &mut GrafhenWordDyn);
}

pub trait LeafCommit {
    fn commit(r: &[u8; SD_BYTES], iv: &[u8; 16], tweak: u32) -> ([u8; SD_BYTES], [u8; COM_BYTES]);
}

pub trait FaestAesProver {
    fn prove_aes_witness(&self, big_vole: &BigVoleProver, hash_key: &UniversalHashKey) -> QuickSilverProof;
    fn verify_aes_proof(&self, _proof: &QuickSilverProof, _hash_key: &UniversalHashKey, _q_vec: &[u8], _delta: &[u8]) -> bool;
}

pub trait PartyIndex {
    fn party_index(requested: usize) -> usize;
}

pub trait MemoryHasher<T> {
    type State: Clone;
    fn new_state() -> Self::State;
    fn absorb(state: &mut Self::State, encoded: T);
    fn finalize_eq(produce: &Self::State, consume: &Self::State) -> bool;
}

pub trait SpecRng {
    fn next_u32(&mut self) -> u32;
    fn next_u8(&mut self) -> u8;
}

pub trait Group {
    type Element: Clone + PartialEq;
    type Scalar: Clone;
    fn generator() -> Self::Element;
    fn random_scalar<R: SpecRng>(rng: &mut R) -> Self::Scalar;
    fn scalar_mul(elt: &Self::Element, k: &Self::Scalar) -> Self::Element;
    fn add(a: &Self::Element, b: &Self::Element) -> Self::Element;
    fn neg(a: &Self::Element) -> Self::Element;
    fn write_element<D: Digest>(elt: &Self::Element, h: &mut D);
}

impl <T: ShlAssign<u32> + ShrAssign<u32> + Default + BitXor<Output = T> + BitXorAssign + Shl<u32, Output = T> + BitAnd<Output = T> + PartialEq + From<u8> + Clone> FieldMulBackend for T {
}

impl  U256 {
    pub fn bit(&self, mut n: u32) -> bool
    {
        let word = ((n / 64) as usize);
        let bit = (n % 64);
        if (word < 4){
    (((self.0[word] >> bit) & 1) != 0)
} else {
    false
}
    }
    pub fn high_bit(&self) -> bool
    {
        ((self.0[3] >> 63) != 0)
    }
    pub fn shl1(&self) -> Self
    {
        let mut out = [0; 4];
        out[0] = (self.0[0] << 1);
        out[1] = ((self.0[1] << 1) | (self.0[0] >> 63));
        out[2] = ((self.0[2] << 1) | (self.0[1] >> 63));
        out[3] = ((self.0[3] << 1) | (self.0[2] >> 63));
        U256(out)
    }
    pub fn shr1(&self) -> Self
    {
        let mut out = [0; 4];
        out[0] = ((self.0[0] >> 1) | (self.0[1] << 63));
        out[1] = ((self.0[1] >> 1) | (self.0[2] << 63));
        out[2] = ((self.0[2] >> 1) | (self.0[3] << 63));
        out[3] = (self.0[3] >> 1);
        U256(out)
    }
    pub fn xor(&self, mut other: &Self) -> Self
    {
        U256(vec![(self.0[0] ^ other.0[0]), (self.0[1] ^ other.0[1]), (self.0[2] ^ other.0[2]), (self.0[3] ^ other.0[3])])
    }
    pub fn is_zero(&self) -> bool
    {
        ((((self.0[0] == 0) && (self.0[1] == 0)) && (self.0[2] == 0)) && (self.0[3] == 0))
    }
}

impl  BitXor<u8> for Bit {
    type Output = Self;
    fn bitxor(self, mut rhs: u8) -> Self::Output
    {
        Bit((self.0 ^ ((rhs & 1) != 0)))
    }
}

impl  BitXor<u8> for Galois {
    type Output = Self;
    fn bitxor(self, mut rhs: u8) -> Self::Output
    {
        Galois((self.0 ^ rhs))
    }
}

impl  Add<Galois> for Galois {
    type Output = Galois;
    fn add(self, mut rhs: Galois) -> Self::Output
    {
        Galois((self.0 ^ rhs.0))
    }
}

impl  Mul<Galois> for Galois {
    type Output = Galois;
    fn mul(self, mut rhs: Galois) -> Self::Output
    {
        Galois(gf_mul_u8(self.0, rhs.0, GF8_POLY))
    }
}

impl  Sub<Galois> for Galois {
    type Output = Galois;
    fn sub(self, mut rhs: Galois) -> Self::Output
    {
        Galois((self.0 ^ rhs.0))
    }
}

impl  Invert for Galois {
    fn invert(&self) -> Self
    {
        Galois(gf_invert_u8(self.0, GF8_POLY))
    }
}

impl  BitXor<u8> for BitsInBytes {
    type Output = Self;
    fn bitxor(self, mut rhs: u8) -> Self::Output
    {
        BitsInBytes((self.0 ^ rhs))
    }
}

impl  Add<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn add(self, mut rhs: BitsInBytes) -> Self::Output
    {
        BitsInBytes((self.0 ^ rhs.0))
    }
}

impl  Mul<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn mul(self, mut rhs: BitsInBytes) -> Self::Output
    {
        BitsInBytes((self.0 & rhs.0))
    }
}

impl  Sub<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn sub(self, mut rhs: BitsInBytes) -> Self::Output
    {
        BitsInBytes((self.0 ^ rhs.0))
    }
}

impl  BitXor<u8> for Galois64 {
    type Output = Self;
    fn bitxor(self, mut rhs: u8) -> Self::Output
    {
        Galois64((self.0 ^ ((rhs as u64) * 72340172838076673)))
    }
}

impl  Add<Galois64> for Galois64 {
    type Output = Galois64;
    fn add(self, mut rhs: Galois64) -> Self::Output
    {
        Galois64((self.0 ^ rhs.0))
    }
}

impl  Mul<Galois64> for Galois64 {
    type Output = Galois64;
    fn mul(self, mut rhs: Galois64) -> Self::Output
    {
        Galois64(gf_mul_u64(self.0, rhs.0, GF64_POLY))
    }
}

impl  Sub<Galois64> for Galois64 {
    type Output = Galois64;
    fn sub(self, mut rhs: Galois64) -> Self::Output
    {
        Galois64((self.0 ^ rhs.0))
    }
}

impl  Invert for Galois64 {
    fn invert(&self) -> Self
    {
        Galois64(gf_invert_u64(self.0, GF64_POLY))
    }
}

impl  BitXor<u8> for BitsInBytes64 {
    type Output = Self;
    fn bitxor(self, mut rhs: u8) -> Self::Output
    {
        BitsInBytes64((self.0 ^ ((rhs as u64) * 72340172838076673)))
    }
}

impl  Add<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn add(self, mut rhs: BitsInBytes64) -> Self::Output
    {
        BitsInBytes64((self.0 ^ rhs.0))
    }
}

impl  Mul<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn mul(self, mut rhs: BitsInBytes64) -> Self::Output
    {
        BitsInBytes64((self.0 & rhs.0))
    }
}

impl  Sub<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn sub(self, mut rhs: BitsInBytes64) -> Self::Output
    {
        BitsInBytes64((self.0 ^ rhs.0))
    }
}

impl  BitXor<u8> for Galois128 {
    type Output = Self;
    fn bitxor(self, mut rhs: u8) -> Self::Output
    {
        Galois128((self.0 ^ (rhs as u128)))
    }
}

impl  Add<Galois128> for Galois128 {
    type Output = Galois128;
    fn add(self, mut rhs: Galois128) -> Self::Output
    {
        Galois128((self.0 ^ rhs.0))
    }
}

impl  Mul<Galois128> for Galois128 {
    type Output = Galois128;
    fn mul(self, mut rhs: Galois128) -> Self::Output
    {
        Galois128(gf_mul_u128(self.0, rhs.0, GF128_POLY))
    }
}

impl  Sub<Galois128> for Galois128 {
    type Output = Galois128;
    fn sub(self, mut rhs: Galois128) -> Self::Output
    {
        Galois128((self.0 ^ rhs.0))
    }
}

impl  Invert for Galois128 {
    fn invert(&self) -> Self
    {
        Galois128(gf_invert_u128(self.0, GF128_POLY))
    }
}

impl  Add<Galois256> for Galois256 {
    type Output = Galois256;
    fn add(self, mut rhs: Galois256) -> Self::Output
    {
        Galois256(self.0.xor(&rhs.0))
    }
}

impl  Mul<Galois256> for Galois256 {
    type Output = Galois256;
    fn mul(self, mut rhs: Galois256) -> Self::Output
    {
        Galois256(gf_mul_256(self.0, rhs.0, GF256_POLY))
    }
}

impl  Sub<Galois256> for Galois256 {
    type Output = Galois256;
    fn sub(self, mut rhs: Galois256) -> Self::Output
    {
        Galois256(self.0.xor(&rhs.0))
    }
}

impl  Invert for Galois256 {
    fn invert(&self) -> Self
    {
        Galois256(gf_invert_256(self.0, GF256_POLY))
    }
}

impl  Z3 {
    pub fn add3(mut a: u8, mut b: u8) -> u8
    {
        let s = (a + b);
        if (s >= 3){
    (s - 3)
} else {
    s
}
    }
    pub fn neg3(mut a: u8) -> u8
    {
        if (a == 0){
    0
} else {
    (3 - a)
}
    }
    pub fn mul3(mut a: u8, mut b: u8) -> u8
    {
        let p = (a * b);
        if (p >= 3){
    (p - 3)
} else {
    p
}
    }
}

impl  Add<Z3> for Z3 {
    type Output = Z3;
    fn add(self, mut rhs: Z3) -> Self::Output
    {
        Z3(Self::add3(self.0, rhs.0))
    }
}

impl  Sub<Z3> for Z3 {
    type Output = Z3;
    fn sub(self, mut rhs: Z3) -> Self::Output
    {
        Z3(Self::add3(self.0, Self::neg3(rhs.0)))
    }
}

impl  Mul<Z3> for Z3 {
    type Output = Z3;
    fn mul(self, mut rhs: Z3) -> Self::Output
    {
        Z3(Self::mul3(self.0, rhs.0))
    }
}

impl <T: Ord> Add<TropicalDyn<T>> for TropicalDyn<T> {
    type Output = TropicalDyn<T>;
    fn add(self, mut rhs: TropicalDyn<T>) -> Self::Output
    {
        TropicalDyn(self.0.min(rhs.0))
    }
}

impl <T: Add<U>, U> Mul<TropicalDyn<U>> for TropicalDyn<T> {
    type Output = TropicalDyn<<T as Add<U>>::Output>;
    fn mul(self, mut rhs: TropicalDyn<U>) -> Self::Output
    {
        TropicalDyn((self.0 + rhs.0))
    }
}

impl <D: Digest> LengthDoubler for ViaDigestPuncturableRandomizerDyn<D> {
    type OutputSize = <D as _>::OutputSize;
    fn double(mut a: Vec<u8>) -> [Vec<u8>; 2]
    {
        let v = D::digest(&a);
        vec![v.clone(), (0..n).map(|i| (v[i] ^ a[i])).collect::<Vec<_>>()]
    }
}

impl <D: Digest> PuncturableLengthDoubler for ViaDigestPuncturableRandomizerDyn<D> {
}

impl <D: Digest> AsRef<[u8]> for CommitmentCoreDyn<D> {
    fn as_ref(&self) -> &[u8]
    {
        &self.0
    }
}

impl <D: Digest> Default for CommitmentCoreDyn<D> {
    fn default() -> Self
    {
        CommitmentCoreDyn((0..<<D>::OutputSize as Unsigned>::to_usize()).map(|_| 0).collect::<Vec<u8>>(), PhantomData)
    }
}

impl <D: Digest> Clone for CommitmentCoreDyn<D> {
    fn clone(&self) -> Self
    {
        CommitmentCoreDyn(self.0.clone(), PhantomData)
    }
}

impl <D: Digest> PartialEq for CommitmentCoreDyn<D> {
    fn eq(&self, mut other: &Self) -> bool
    {
        (self.0.as_slice() == other.0.as_slice())
    }
}

impl <D: Digest> Eq for CommitmentCoreDyn<D> {
}

impl <D: Digest> CommitmentCoreDyn<D> {
    pub fn validate(&self, mut opened_message: &impl AsRef<[u8]>, mut opened_rand: &impl AsRef<[u8]>) -> bool
    {
        let recomputed: CommitmentCoreDyn<D> = commit::<D>(opened_message, opened_rand);
        (&recomputed.0 == &self.0)
    }
}

impl  WordReducer<WBOUND> for NoReduction {
    fn reduce(&self, mut _word: &mut GrafhenWordDyn)
    {
    }
}

impl  GrafhenWordDyn {
    pub fn identity(mut wbound: usize) -> Self
    {
        GrafhenWordDyn { data: [0; wbound], len: 0, wbound: 0 }
    }
}

impl <T> DeltaDyn<T> {
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> DeltaDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let Self { delta: delta, .. } = self;
        DeltaDyn { delta: (0..m).map(|i| delta[(f(i) % n)].clone()).collect::<Vec<T>>(), n: 0 }
    }
    pub fn rotate_left(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(self.n, |a| a.wrapping_sub(n_param))
    }
    pub fn rotate_right(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(self.n, |a| a.wrapping_add(n_param))
    }
    pub fn r#static<U: Mul<T, Output = O> + Clone, O>(&self, mut val: Vec<U>) -> QDyn<O> where T: Clone
    {
        let n: usize = self.n;
        QDyn { q: (0..n).map(|i| (val[i].clone() * self.delta[i].clone())).collect::<Vec<O>>(), n: 0 }
    }
}

impl <T> QDyn<T> {
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> QDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let Self { q: q, .. } = self;
        QDyn { q: (0..m).map(|i| q[(f(i) % n)].clone()).collect::<Vec<T>>(), n: 0 }
    }
    pub fn rotate_left(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(self.n, |a| a.wrapping_sub(n_param))
    }
    pub fn rotate_right(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(self.n, |a| a.wrapping_add(n_param))
    }
}

impl  LengthDoubler for AesCtrLengthDoubler {
    type OutputSize = U16;
    fn double(mut a: Vec<u8>) -> [Vec<u8>; 2]
    {
        let mut block0 = [0; BLOCK];
        let mut block1 = [0; BLOCK];
        block1[0] = 1;
        let key: [u8; BLOCK] = a.0;
        let c0 = encrypt_block(&key, &block0);
        let c1 = encrypt_block(&key, &block1);
        block0.fill(0);
        block1.fill(0);
        vec![Vec(c0), Vec(c1)]
    }
}

impl <D: Digest + Digest> LeafCommit<SD_BYTES, COM_BYTES> for RoLeafCommitDyn<D> where D: Digest {
    fn commit(mut sd_bytes: usize, mut com_bytes: usize, mut r: &[u8; SD_BYTES], mut iv: &[u8; 16], mut tweak: u32) -> ([u8; SD_BYTES], [u8; COM_BYTES])
    {
        let out_size = D::output_size();
        let mut h = D::new();
        h.update(r);
        h.update(iv);
        h.update(&tweak.to_le_bytes());
        let digest = h.finalize().to_vec();
        let mut sd = [0; sd_bytes];
        let mut com = [0; com_bytes];
        sd.copy_from_slice(&digest[..sd_bytes]);
        com.copy_from_slice(&digest[sd_bytes..(sd_bytes + com_bytes)]);
        (sd, com)
    }
}

impl  LeafCommit<SD_BYTES, COM_BYTES> for EmLeafCommit {
    fn commit(mut sd_bytes: usize, mut com_bytes: usize, mut r: &[u8; SD_BYTES], mut iv: &[u8; 16], mut tweak: u32) -> ([u8; SD_BYTES], [u8; COM_BYTES])
    {
        let mut seed_buf = [0; 16];
        seed_buf.copy_from_slice(&r[..]);
        let com_vec = aes_ctr_prg(&seed_buf, iv, tweak, com_bytes);
        let mut sd = [0; sd_bytes];
        sd.copy_from_slice(r);
        let mut com = [0; com_bytes];
        com.copy_from_slice(&com_vec[..com_bytes]);
        (sd, com)
    }
}

impl  Sponge {
    pub fn absorb(&mut self, mut data: &[u8])
    {
        match self {
    Sponge::Shake128(h) => h.update(data),
    Sponge::Shake256(h) => h.update(data),
}
    }
    pub fn squeeze(&self, mut n: usize) -> Vec<u8>
    {
        let mut out = vec![];
        match self {
    Sponge::Shake128(h) => {
    let mut r = h.clone().finalize_xof();
    r.read(&mut out);
},
    Sponge::Shake256(h) => {
    let mut r = h.clone().finalize_xof();
    r.read(&mut out);
},
};
        out
    }
}

impl  FaestTranscript {
    pub fn new_shake128() -> Self
    {
        FaestTranscript { sponge: Sponge::Shake128(Shake128::default()) }
    }
    pub fn new_shake256() -> Self
    {
        FaestTranscript { sponge: Sponge::Shake256(Shake256::default()) }
    }
    pub fn absorb(&mut self, mut data: &[u8])
    {
        self.sponge.absorb(data);
    }
    pub fn squeeze(&self, mut n: usize) -> Vec<u8>
    {
        self.sponge.squeeze(n)
    }
}

impl <L: LeafCommit<LAMBDA_BYTES, COM_BYTES>> BavcDyn<L> {
    pub fn commit<D: Digest>(mut com_bytes: usize, mut r: [u8; LAMBDA_BYTES], mut iv: &[u8; 16], mut tau: usize, mut n: usize) -> BavcCommitmentDyn
    {
        let leaf_count = (tau * n);
        let total_nodes = ((2 * leaf_count) - 1);
        let mut tree: Vec<[u8; LAMBDA_BYTES]> = vec![];
        tree[0] = r;
        for node in 0.. (leaf_count - 1){
    let parent = Vec::<u8, U16>(tree[node]);
    let .. = AesCtrLengthDoubler::double(parent);
    tree[((2 * node) + 1)] = left.0;
    tree[((2 * node) + 2)] = right.0;
};
        let mut seeds = Vec::with_capacity(leaf_count);
        let mut commitments = Vec::with_capacity(leaf_count);
        for i in 0.. tau{
    for j in 0.. n{
    let leaf_k = ((i * n) + j);
    let tree_pos = ((leaf_count - 1) + leaf_k);
    let r_leaf = tree[tree_pos];
    let tweak = (leaf_k as u32);
    let (sd, com) = commit::<D>(&r_leaf, iv, tweak);
    seeds.push(sd);
    commitments.push(com);
}
};
        let mut vec_hashes: Vec<Vec<u8>> = Vec::with_capacity(tau);
        for i in 0.. tau{
    let mut h = D::new();
    for j in 0.. n{
    h.update(&commitments[((i * n) + j)]);
};
    vec_hashes.push(h.finalize().to_vec().to_vec());
};
        let mut h = D::new();
        for hi in &vec_hashes{
    h.update(hi);
};
        let root = h.finalize().to_vec().to_vec();
        BavcCommitmentDyn { root: root, vec_hashes: vec_hashes, seeds: seeds, commitments: commitments, com_bytes: 0 }
    }
    pub fn open(mut com_bytes: usize, mut commitment: &BavcCommitmentDyn, mut deltas: &[usize], mut tau: usize, mut n: usize) -> BavcOpeningDyn
    {
        for (i, ..) in deltas.iter().enumerate(){
};
        let leaf_count = (tau * n);
        let total_nodes = ((2 * leaf_count) - 1);
        let mut hidden = vec![];
        for (i, ..) in deltas.iter().enumerate(){
    let leaf_k = ((i * n) + d);
    let tree_pos = ((leaf_count - 1) + leaf_k);
    hidden[tree_pos] = true;
};
        for node in 0..(leaf_count - 1).rev(){
    let left = ((2 * node) + 1);
    let right = ((2 * node) + 2);
    hidden[node] = (hidden[left] || hidden[right]);
};
        let mut tree: Vec<[u8; LAMBDA_BYTES]> = vec![];
        let _ = tree;
        BavcOpeningDyn { hidden_commits: deltas.iter().enumerate().map(|(i, d)| commitment.commitments[((i * n) + d)]).collect::<Vec<_>>(), nodes: Vec::new(), com_bytes: 0 }
    }
    pub fn collect_open_nodes(mut com_bytes: usize, mut deltas: &[usize], mut tree: &[[u8; LAMBDA_BYTES]], mut tau: usize, mut n: usize) -> Vec<(usize, [u8; LAMBDA_BYTES])>
    {
        let leaf_count = (tau * n);
        let total_nodes = ((2 * leaf_count) - 1);
        let mut hidden = vec![];
        for (i, ..) in deltas.iter().enumerate(){
    let leaf_k = ((i * n) + d);
    let tree_pos = ((leaf_count - 1) + leaf_k);
    hidden[tree_pos] = true;
};
        for node in 0..(leaf_count - 1).rev(){
    hidden[node] = (hidden[((2 * node) + 1)] || hidden[((2 * node) + 2)]);
};
        let mut out: Vec<(usize, [u8; LAMBDA_BYTES])> = Vec::new();
        walk(0, &hidden, tree, leaf_count, &mut out);
        out
    }
    pub fn reconstruct<D: Digest>(mut com_bytes: usize, mut nodes: &[(usize, [u8; LAMBDA_BYTES])], mut hidden_commits: &[[u8; COM_BYTES]], mut deltas: &[usize], mut iv: &[u8; 16], mut expected_root: &[u8], mut tau: usize, mut n: usize) -> Option<Vec<[u8; LAMBDA_BYTES]>>
    {
        let leaf_count = (tau * n);
        let total_nodes = ((2 * leaf_count) - 1);
        let mut hidden = vec![];
        for (i, ..) in deltas.iter().enumerate(){
    let leaf_k = ((i * n) + d);
    let tree_pos = ((leaf_count - 1) + leaf_k);
    hidden[tree_pos] = true;
};
        for node in 0..(leaf_count - 1).rev(){
    hidden[node] = (hidden[((2 * node) + 1)] || hidden[((2 * node) + 2)]);
};
        let mut tree: Vec<Option<[u8; LAMBDA_BYTES]>> = vec![];
        for (idx, seed) in nodes{
    tree[*idx] = Some(*seed);
};
        for node in 0.. (leaf_count - 1){
    match tree[node] {
    Some(parent_seed) => {
    let parent = Vec::<u8, U16>(parent_seed);
    let .. = AesCtrLengthDoubler::double(parent);
    if tree[((2 * node) + 1)].is_none(){
    tree[((2 * node) + 1)] = Some(left.0);
};
    if tree[((2 * node) + 2)].is_none(){
    tree[((2 * node) + 2)] = Some(right.0);
}
},
    _ => {
},
}
};
        let mut leaf_seeds: Vec<[u8; LAMBDA_BYTES]> = Vec::with_capacity(leaf_count);
        let mut leaf_coms: Vec<[u8; COM_BYTES]> = Vec::with_capacity(leaf_count);
        for i in 0.. tau{
    for j in 0.. n{
    let leaf_k = ((i * n) + j);
    let tree_pos = ((leaf_count - 1) + leaf_k);
    if (j == deltas[i]){
    leaf_seeds.push([0; LAMBDA_BYTES]);
    leaf_coms.push(hidden_commits[i]);
} else {
    let r_leaf = tree[tree_pos]?;
    let tweak = (leaf_k as u32);
    let (sd, com) = commit::<D>(&r_leaf, iv, tweak);
    leaf_seeds.push(sd);
    leaf_coms.push(com);
}
}
};
        let mut vec_hashes: Vec<Vec<u8>> = Vec::with_capacity(tau);
        for i in 0.. tau{
    let mut h = D::new();
    for j in 0.. n{
    h.update(&leaf_coms[((i * n) + j)]);
};
    vec_hashes.push(h.finalize().to_vec().to_vec());
};
        let mut h = D::new();
        for hi in &vec_hashes{
    h.update(hi);
};
        let root = h.finalize().to_vec().to_vec();
        if (root.as_slice() == expected_root){
    Some(leaf_seeds)
} else {
    None
}
    }
}

impl  FaestAesProver for StubFaestAesProver {
    fn prove_aes_witness(&self, mut big_vole: &BigVoleProver, mut hash_key: &UniversalHashKey) -> QuickSilverProof
    {
        let a_hat_out: UniversalHashOutput = vole_hash(hash_key, &big_vole.u);
        let a_hat: Vec<u8> = a_hat_out.h0.0.to_le_bytes().iter().chain(a_hat_out.h1.0.to_le_bytes().iter()).cloned().collect();
        let len = a_hat.len();
        QuickSilverProof { a_hat: a_hat, b_hat: vec![], c_hat_base: vec![] }
    }
}

impl  PartyIndex for U1 {
    fn party_index(mut _: usize) -> usize
    {
        0
    }
}

impl  PartyIndex for N {
    fn party_index(mut n: usize, mut requested: usize) -> usize
    {
        requested
    }
}

impl <T> PolyDyn<T> {
    pub fn get_qs_pool<Q: Clone + Mul<A, Output = A>, A: Add<A, Output = A>>(&self, mut m: usize, mut x: usize, mut root: DeltaDyn<Q>, mut inputs: PolyInputPoolDyn<QDyn<Q>>, mut reduction: usize) -> QDyn<A> where T: Clone + Into<A>
    {
        let n: usize = self.n;
        super::Q { q: (0..m).map(|i| {
    let mut sum: A = self.c0.clone().into();
    for _ in 0.. n{
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. n{
    let mut b: A = self.c1[j].clone().into();
    for i2 in inputs.indices.iter(){
    for _ in 0.. reduction{
    b = (inputs.inputs[i2[j]].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<A>>() }
    }
    pub fn get_qs<Q: Clone + Mul<A, Output = A>, A: Add<A, Output = A>>(&self, mut m: usize, mut x: usize, mut root: DeltaDyn<Q>, mut inputs: Vec<Vec<QDyn<Q>>>, mut reduction: usize) -> QDyn<A> where T: Clone + Into<A>
    {
        let n: usize = self.n;
        super::Q { q: (0..m).map(|i| {
    let mut sum: A = self.c0.clone().into();
    for _ in 0.. n{
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. n{
    let mut b: A = self.c1[j].clone().into();
    for i2 in inputs.iter(){
    for _ in 0.. reduction{
    b = (i2[j].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<A>>() }
    }
    pub fn apply_pool<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(&self, mut m: usize, mut x: usize, mut x2: usize, mut xs: usize, mut s: usize, mut voles: &PolyInputPoolDyn<VopeDyn<T>>) -> VopeDyn<O> where T: Into<O> + Clone
    {
        let n: usize = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    let mut b: O = self.c1[k].clone().into();
    for v in &voles.indices{
    b = (b * voles.inputs[v[k]].v[i].clone().into());
};
    sum = (sum + b);
};
    let c0: O = self.c0.clone().into();
    (sum + c0)
}).collect::<Vec<O>>();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    for n in 0.. x{
    let mut b: O = self.c1[k].clone().into();
    for m in 0.. s{
    let l = ((l * s) + m);
    for (idx, v) in voles.indices.iter().enumerate(){
    b = (b * if (idx == n){
    voles.inputs[v[k]].u[l][i].clone().into()
} else {
    voles.inputs[v[k]].v[i].clone().into()
});
}
};
    sum = (sum + b);
}
};
    sum
}).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>();
        return VopeDyn { u: u, v: v, n: 0, k: 1 };
    }
    pub fn apply<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(&self, mut m: usize, mut x: usize, mut x2: usize, mut xs: usize, mut s: usize, mut voles: Vec<Vec<VopeDyn<T>>>) -> VopeDyn<O> where T: Into<O> + Clone
    {
        let n: usize = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    let mut b: O = self.c1[k].clone().into();
    for v in &voles{
    b = (b * v[k].v[i].clone().into());
};
    sum = (sum + b);
};
    let c0: O = self.c0.clone().into();
    (sum + c0)
}).collect::<Vec<O>>();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    for n in 0.. x{
    let mut b: O = self.c1[k].clone().into();
    for m in 0.. s{
    let l = ((l * s) + m);
    for (idx, v) in voles.iter().enumerate(){
    b = (b * if (idx == n){
    v[k].u[l][i].clone().into()
} else {
    v[k].v[i].clone().into()
});
}
};
    sum = (sum + b);
}
};
    sum
}).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>();
        return VopeDyn { u: u, v: v, n: 0, k: 1 };
    }
}

impl <T: Clone + Default + Add<Output = T> + PartialEq> MemoryHasher<T> for AdditiveHasher where T: Clone + Default + Add<Output = T> + PartialEq {
    type State = T;
    fn new_state() -> T
    {
        T::default()
    }
    fn absorb(mut state: &mut T, mut encoded: T)
    {
        let old = core::mem::take(state);
        *state = (old + encoded);
    }
    fn finalize_eq(mut produce: &T, mut consume: &T) -> bool
    {
        (*produce == *consume)
    }
}

impl <T: Clone + Mul<Output = T>> ChallengeKeyDyn<T> {
    pub fn from_challenge(mut r: T) -> Self
    {
        let r2 = (r.clone() * r.clone());
        let r3 = (r2.clone() * r.clone());
        ChallengeKeyDyn { r1: r, r2: r2, r3: r3 }
    }
}

impl <T: Clone + Default + Add<Output = T> + Mul<Output = T>, H: MemoryHasher<T>> MemoryCheckStateDyn<T, H> where T: Clone + Default + Add<Output = T> + Mul<Output = T> {
    pub fn new(mut key: ChallengeKeyDyn<T>) -> Self
    {
        MemoryCheckStateDyn { key: key, produce: H::new_state(), consume: H::new_state(), _phantom: PhantomData }
    }
    pub fn encode(&self, mut addr: T, mut value: T, mut timestamp: u64) -> T
    {
        let a = (addr * self.key.r1.clone());
        let v = (value * self.key.r2.clone());
        let t = self.scale_by_u64(self.key.r3.clone(), timestamp);
        ((a + v) + t)
    }
    pub fn init(&mut self, mut addr: T, mut zero_value: T)
    {
        let enc = self.encode(addr, zero_value, 0);
        H::absorb(&mut self.produce, enc);
    }
    pub fn write(&mut self, mut addr: T, mut new_value: T, mut timestamp: u64, mut old_value: T, mut old_timestamp: u64) where T: Clone
    {
        let enc_new = self.encode(addr.clone(), new_value, timestamp);
        H::absorb(&mut self.produce, enc_new);
        let enc_old = self.encode(addr, old_value, old_timestamp);
        H::absorb(&mut self.consume, enc_old);
    }
    pub fn read(&mut self, mut addr: T, mut value: T, mut timestamp: u64, mut write_timestamp: u64) where T: Clone
    {
        let enc_produce = self.encode(addr.clone(), value.clone(), timestamp);
        H::absorb(&mut self.produce, enc_produce);
        let enc_consume = self.encode(addr, value, write_timestamp);
        H::absorb(&mut self.consume, enc_consume);
    }
    pub fn drain(&mut self, mut addr: T, mut final_value: T, mut final_timestamp: u64)
    {
        let enc = self.encode(addr, final_value, final_timestamp);
        H::absorb(&mut self.consume, enc);
    }
    pub fn verify(&self) -> bool
    {
        H::finalize_eq(&self.produce, &self.consume)
    }
    pub fn scale_by_u64(&self, mut x: T, mut n: u64) -> T
    {
        if (n == 0){
    return T::default();
};
        if (n == 1){
    return x;
};
        let mut acc = T::default();
        let mut base = x;
        for bit in 0.. 64{
    if (((n >> bit) & 1) == 1){
    acc = (acc + base.clone());
};
    base = (base.clone() + base);
};
        acc
    }
}

impl <T: Add<Output = T> + Mul<Output = T> + Default + Clone> VopeDyn<T> where T: Add<Output = T> + Mul<Output = T> + Default + Clone {
    pub fn mul_generalized(&self, mut k2: usize, mut other: &VopeDyn<T>) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let mut res_u = (0..(k2 + k)).map(|_| <Vec<T>>::default()).collect::<Vec<Vec<T>>>();
        let mut res_v = (0..n).map(|_| <T>::default()).collect::<Vec<T>>();
        for i in 0..= k{
    for j in 0..= k2{
    let k = (i + j);
    let a_coeff = if (i == 0){
    &self.v
} else {
    &self.u[(i - 1)]
};
    let b_coeff = if (j == 0){
    &other.v
} else {
    &other.u[(j - 1)]
};
    if (k == 0){
    for lane in 0.. n{
    res_v[lane] = (res_v[lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
} else {
    for lane in 0.. n{
    res_u[(k - 1)][lane] = (res_u[(k - 1)][lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
}
}
};
        VopeDyn { u: res_u, v: res_v, n: 0, k: 1 }
    }
}

impl  VopeDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    let BitsInBytes(next) = u[l][((i + 1) % n_param)].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>()
}).collect::<Vec<Vec<BitsInBytes>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    let BitsInBytes(next) = v[((i + 1) % n_param)].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>(), n: 0, k: 1 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes(prev) = u[l][(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes(b) = u[l][i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>()
}).collect::<Vec<Vec<BitsInBytes>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes(prev) = v[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes(b) = v[i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>(), n: 0, k: 1 }
    }
    pub fn bit(&self, mut n_param: u8) -> VopeDyn<Bit>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>()
}).collect::<Vec<Vec<Bit>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0, k: 1 }
    }
}

impl  VopeDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    let BitsInBytes64(next) = u[l][((i + 1) % n_param)].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>()
}).collect::<Vec<Vec<BitsInBytes64>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    let BitsInBytes64(next) = v[((i + 1) % n_param)].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>(), n: 0, k: 1 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes64(prev) = u[l][(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes64(b) = u[l][i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>()
}).collect::<Vec<Vec<BitsInBytes64>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = v[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes64(b) = v[i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>(), n: 0, k: 1 }
    }
    pub fn bit(&self, mut n_param: u8) -> VopeDyn<Bit>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>()
}).collect::<Vec<Vec<Bit>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0, k: 1 }
    }
}

impl <T: Clone> Clone for VopeDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| (0..n).map(|i| u[l][i].clone()).collect::<Vec<T>>()).collect::<Vec<Vec<T>>>(), v: (0..n).map(|i| v[i].clone()).collect::<Vec<T>>(), n: 0, k: 1 }
    }
}

impl <T: Clone> Clone for QDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| q[i].clone()).collect::<Vec<T>>(), n: 0 }
    }
}

impl <T: Clone> Clone for DeltaDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| delta[i].clone()).collect::<Vec<T>>(), n: 0 }
    }
}

impl <T: PartialEq> PartialEq for VopeDyn<T> {
    fn eq(&self, mut other: &Self) -> bool
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u1, v: v1, .. } = self;
        let VopeDyn { u: u2, v: v2, .. } = other;
        for l in 0.. k{
    for i in 0.. n{
    if (u1[l][i] != u2[l][i]){
    return false;
}
}
};
        for i in 0.. n{
    if (v1[i] != v2[i]){
    return false;
}
};
        true
    }
}

impl <T: PartialEq> PartialEq for QDyn<T> {
    fn eq(&self, mut other: &Self) -> bool
    {
        let n: usize = self.n;
        let QDyn { q: q1, .. } = self;
        let QDyn { q: q2, .. } = other;
        for i in 0.. n{
    if (q1[i] != q2[i]){
    return false;
}
};
        true
    }
}

impl <T: PartialEq> PartialEq for DeltaDyn<T> {
    fn eq(&self, mut other: &Self) -> bool
    {
        let n: usize = self.n;
        let DeltaDyn { delta: d1, .. } = self;
        let DeltaDyn { delta: d2, .. } = other;
        for i in 0.. n{
    if (d1[i] != d2[i]){
    return false;
}
};
        true
    }
}

impl <T: Eq> Eq for VopeDyn<T> {
}

impl <T: Eq> Eq for QDyn<T> {
}

impl <T: Eq> Eq for DeltaDyn<T> {
}

impl  QDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    let BitsInBytes(next) = q[((i + 1) % n_param)].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes(prev) = q[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes(b) = q[i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> QDyn<Bit>
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl  QDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    let BitsInBytes64(next) = q[((i + 1) % n_param)].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = q[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes64(b) = q[i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> QDyn<Bit>
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl  DeltaDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    let BitsInBytes(next) = delta[((i + 1) % n_param)].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(prev) = delta[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes(b) = delta[i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> DeltaDyn<Bit>
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl  DeltaDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    let BitsInBytes64(next) = delta[((i + 1) % n_param)].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = delta[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes64(b) = delta[i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> DeltaDyn<Bit>
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl <T> VopeDyn<T> {
    pub fn constant(mut n: usize, mut v: Vec<T>) -> Self
    {
        let k: usize = 0;
        VopeDyn { u: (0..0).map(|_| unreachable!()).collect::<Vec<Vec<T>>>(), v: v, n: 0, k: 1 }
    }
}

impl <T: Add<U, Output = O> + Clone, U: Clone, O> Add<VopeDyn<U>> for VopeDyn<T> {
    type Output = VopeDyn<O>;
    fn add(self, mut rhs: VopeDyn<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| (self.u[l][i].clone() + rhs.u[l][i].clone())).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>(), v: (0..n).map(|i| (self.v[i].clone() + rhs.v[i].clone())).collect::<Vec<O>>(), n: 0, k: 1 }
    }
}

impl <T: BitXor<U, Output = O> + Clone + Into<O>, U: Clone, O> BitXor<Vec<U>> for VopeDyn<T> where T: Into<O> {
    type Output = VopeDyn<O>;
    fn bitxor(self, mut rhs: Vec<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        VopeDyn { u: (0..k).map(|i| {
    (0..n).map(|j| {
    let o: O = self.u[i][j].clone().bitxor(rhs[((i * k) + j)].clone());
    o
}).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>(), v: self.v.into_iter().map(|a| a.into()).collect::<Vec<_>>(), n: 0, k: 1 }
    }
}

impl <T: Mul<U, Output = O> + Into<O> + Clone, U: Mul<U, Output = U> + Clone, O: Add<O, Output = O> + Clone> Mul<DeltaDyn<U>> for VopeDyn<T> {
    type Output = QDyn<O>;
    fn mul(self, mut rhs: DeltaDyn<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        QDyn { q: self.u.iter().enumerate().fold(self.v.into_iter().map(|a| a.into()).collect::<Vec<_>>(), |mut a, (i, b)| {
    (0..n).map(|j| {
    let mut x = rhs.delta[i].clone();
    for _ in 0.. i{
    x = (x * rhs.delta[i].clone());
};
    let m: O = (b[j].clone() * x);
    (m + a[j].clone())
}).collect::<Vec<O>>()
}), n: 0 }
    }
}

impl <T> VopeDyn<T> {
    pub fn expand(&self, mut l: usize) -> VopeDyn<T> where T: Clone + Default
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Self { u: u, v: v, .. } = self;
        VopeDyn { u: (0..l).map(|l| {
    (0..n).map(|i| u.get(l).map_or(T::default(), |a| a[i].clone())).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: v.clone(), n: 0, k: 1 }
    }
    pub fn rotate_left(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        self.remap(self.n, |a| a.wrapping_sub(n_param))
    }
    pub fn rotate_right(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        self.remap(self.n, |a| a.wrapping_add(n_param))
    }
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> VopeDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Self { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..m).map(|i| u[l][(f(i) % n)].clone()).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: (0..m).map(|i| v[(f(i) % n)].clone()).collect::<Vec<T>>(), n: 0, k: 1 }
    }
}

impl  VopeDyn<Bit> {
    pub fn scale<T>(self, mut f: impl FnMut(bool) -> T) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let Bit(b) = u[l][i].clone();
    f(b)
}).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: (0..n).map(|i| {
    let Bit(b) = v[i].clone();
    f(b)
}).collect::<Vec<T>>(), n: 0, k: 1 }
    }
}

impl <T: Clone, U: Clone> LweSampleDyn<T, U> {
    pub fn new(mut n: usize, mut m: usize, mut matrix: Vec<Vec<T>>, mut b: Vec<U>) -> Self
    {
        Self { matrix: matrix, b: b }
    }
    pub fn sample<S: Clone + Mul<T, Output = A>, A: Add<P, Output = U> + Add<A, Output = A> + Default, P: Clone>(mut n: usize, mut m: usize, mut matrix: Vec<Vec<T>>, mut s: Vec<S>, mut e: Vec<P>) -> Self
    {
        Self { b: (0..m).map(|i| {
    (s.iter().enumerate().map(|(a, b)| (b.clone() * matrix[i][a].clone())).fold(A::default(), |mut a, b| (a + b)) + e[i].clone())
}).collect::<Vec<U>>(), matrix: matrix }
    }
}

impl <B: LengthDoubler, D: Digest> ABOOpeningDyn<B, D> {
    pub fn to_vole_material(&self, mut m: usize, mut party: usize) -> [VopeDyn<u8>; M]
    {
        let t: usize = self.t;
        let u: usize = self.u;
        let n: usize = self.n;
        (0..n).map(|i| {
    let s = &self.openings[N::party_index(party)][i];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, mut m: usize, mut party: usize) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        let n: usize = self.n;
        (0..m).map(|i| {
    let s = &self.openings[N::party_index(party)][i];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<VopeDyn<u8>>>()
    }
    pub fn to_vole_material_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut m: usize, mut party: usize, mut f: F) -> [VopeDyn<u8>; M]
    {
        let t: usize = self.t;
        let u: usize = self.u;
        let n: usize = self.n;
        (0..n).map(|i| {
    let s = &self.openings[N::party_index(party)][i];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut m: usize, mut party: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        let n: usize = self.n;
        (0..m).map(|i| {
    let s = &self.openings[N::party_index(party)][i];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<VopeDyn<u8>>>()
    }
    pub fn split_bit_typenum(&self, mut m: usize, mut party: usize) -> Vec<BSplitDyn<B, D>> where D: Digest
    {
        let t: usize = self.t;
        let u: usize = self.u;
        let n: usize = self.n;
        (0..m).map(|i| {
    let s = &self.openings[N::party_index(party)][i];
    BSplitDyn { split: (0..ilog2(<<D>::OutputSize as Unsigned>::to_usize())).map(|j| {
    (0..n).map(|b| {
    s.iter().enumerate().filter_map(|(a, c)| {
    if (((a >> j) & 1) == b){
    Some(c.clone())
} else {
    None
}
}).fold((0..<<B>::OutputSize as Unsigned>::to_usize()).map(|_| 0).collect::<Vec<u8>>(), |mut a, b| {
    (0..<<B>::OutputSize as Unsigned>::to_usize()).map(|i| a[i].bitxor(b[i])).collect::<Vec<u8>>()
})
}).collect::<Vec<_>>()
}).collect::<Vec<[Vec<u8>; 2]>>(), _phantom: PhantomData }
}).collect::<Vec<BSplitDyn<B, D>>>()
    }
}

impl <B: LengthDoubler, D: Digest> ABOOpeningDyn<B, D> {
    pub fn validate<R: AsRef<[u8]>>(&self, mut commit_: &Vec<u8>, mut rand: &R) -> bool
    {
        let t: usize = self.t;
        let u: usize = self.u;
        let n: usize = self.n;
        let mut h = D::new();
        for i in 0.. t{
    for b in 0.. u{
    let i2 = (i | ((b as usize) << t.ilog2()));
    if self.bad.contains(&(i2 as u64)){
    h.update(&self.openings[0][i][b][..<<D>::OutputSize as Unsigned>::to_usize()]);
} else {
    h.update(&commit::<D>(&&self.openings[0][i][b][..<<B>::OutputSize as Unsigned>::to_usize()], rand));
}
}
};
        (h.finalize().to_vec().as_slice() == commit_.as_slice())
    }
}

impl <B: LengthDoubler, D: Digest> ABOOpeningDyn<B, D> {
    pub fn validate<R: AsRef<[u8]>>(mut t: usize, mut u: usize, mut nothers: usize, mut this: Vec<&Self>, mut me: &ABODyn<B, D>, mut commit_: Vec<&Vec<u8>>, mut rand: &R) -> bool
    {
        commit_.iter().enumerate().all(|(ci, commit_)| {
    let mut h = D::new();
    if (ci != 0){
    for i in 0.. t{
    for b in 0.. u{
    let i2 = (i | ((b as usize) << t.ilog2()));
    h.update(&commit::<D>(&me.per_byte[ci][i2], rand));
}
}
};
    for (idx, this) in this.iter().enumerate(){
    if ((idx + 1) == ci){
    continue;
};
    for i in 0.. t{
    for b in 0.. u{
    let i2 = (i | ((b as usize) << t.ilog2()));
    if this.bad.contains(&(i2 as u64)){
    h.update(&this.openings[ci][i][b][..<<D>::OutputSize as Unsigned>::to_usize()]);
} else {
    h.update(&commit::<D>(&&this.openings[ci][i][b][..<<B>::OutputSize as Unsigned>::to_usize()], rand));
}
}
}
};
    (h.finalize().to_vec().as_slice() == commit_.as_slice())
})
    }
}

impl <B: LengthDoubler, D: Digest> ABODyn<B, D> {
    pub fn open<R: AsRef<[u8]>>(&self, mut t: usize, mut u: usize, mut m: usize, mut bad: Vec<u64>, mut rand: &R) -> ABOOpeningDyn<B, D>
    {
        let k: usize = self.k;
        let n: usize = self.n;
        ABOOpeningDyn { bad: bad.clone(), openings: (0..n).map(|ni| {
    let bad = bad.clone();
    (0..t).map(|i| {
    let bad = bad.clone();
    (0..u).map(|j| {
    let i2 = (i | ((j as usize) << t.ilog2()));
    if bad.contains(&(i2 as u64)){
    let h = commit::<D>(&self.per_byte[ni][i2], rand);
    (0..m).map(|j| AsRef::<[u8]>::as_ref(&h).get(j).cloned().unwrap_or_default()).collect::<Vec<u8>>()
} else {
    (0..m).map(|j| {
    self.per_byte[ni][i2].get(j).cloned().unwrap_or_default()
}).collect::<Vec<u8>>()
}
}).collect::<Vec<Vec<u8>>>()
}).collect::<Vec<Vec<Vec<u8>>>>()
}).collect::<Vec<Vec<Vec<Vec<u8>>>>>(), t: 0, u: 0, n: 0, _phantom: PhantomData }
    }
}

impl <B: LengthDoubler, D: Digest> ABODyn<B, D> {
    pub fn to_vole_material(&self, mut m: usize, mut target: usize) -> [VopeDyn<u8>; M]
    {
        let k: usize = self.k;
        let n: usize = self.n;
        (0..n).map(|i| {
    let s = &self.per_byte[N::party_index(target)][(i * m)..][..m];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, mut m: usize, mut target: usize) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        let n: usize = self.n;
        (0..m).map(|i| {
    let s = &self.per_byte[N::party_index(target)][(i * m)..][..m];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<VopeDyn<u8>>>()
    }
    pub fn to_vole_material_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut m: usize, mut target: usize, mut f: F) -> [VopeDyn<u8>; M]
    {
        let k: usize = self.k;
        let n: usize = self.n;
        (0..n).map(|i| {
    let s = &self.per_byte[N::party_index(target)][(i * m)..][..m];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut m: usize, mut target: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        let n: usize = self.n;
        (0..m).map(|i| {
    let s = &self.per_byte[N::party_index(target)][(i * m)..][..m];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<VopeDyn<u8>>>()
    }
    pub fn split_bit_typenum(&self, mut m: usize, mut target: usize) -> Vec<BSplitDyn<B, D>> where D: Digest
    {
        let k: usize = self.k;
        let n: usize = self.n;
        (0..m).map(|i| {
    let s = &self.per_byte[N::party_index(target)][(i * m)..][..m];
    BSplitDyn { split: (0..ilog2(<<D>::OutputSize as Unsigned>::to_usize())).map(|j| {
    (0..n).map(|b| {
    s.iter().enumerate().filter_map(|(a, c)| {
    if (((a >> j) & 1) == b){
    Some(c.clone())
} else {
    None
}
}).fold((0..<<B>::OutputSize as Unsigned>::to_usize()).map(|_| 0).collect::<Vec<u8>>(), |mut a, b| {
    (0..<<B>::OutputSize as Unsigned>::to_usize()).map(|i| a[i].bitxor(b[i])).collect::<Vec<u8>>()
})
}).collect::<Vec<_>>()
}).collect::<Vec<[Vec<u8>; 2]>>(), _phantom: PhantomData }
}).collect::<Vec<BSplitDyn<B, D>>>()
    }
}

impl  GarbleDyn {
    pub fn zero(mut n: usize) -> Self
    {
        GarbleDyn { base: (0..n).map(|_| 0).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn and_result<D: Digest>(&self, mut b: &GarbleDyn) -> Self
    {
        let n: usize = self.n;
        let mut d = D::new();
        d.update(&self.base);
        d.update(&b.base);
        let hash = d.finalize().to_vec();
        GarbleDyn { base: (0..n).map(|i| hash[i]).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn share(&self, mut target: &Vec<u8>) -> EvalDyn
    {
        let n: usize = self.n;
        EvalDyn { target: (0..n).map(|i| (self.base[i] ^ target[i])).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn to_share(&self, mut o: usize) -> GarbleDyn
    {
        let n: usize = self.n;
        GarbleDyn { base: (0..o).map(|i| {
    let mut v = 0;
    for j in 0.. 8{
    let bit = (self.base[((i * 8) + j)] & 1);
    v |= (bit << j);
};
    v
}).collect::<Vec<u8>>(), n: 0 }
    }
}

impl  EvalDyn {
    pub fn zero(mut n: usize) -> Self
    {
        EvalDyn { target: (0..n).map(|_| 0).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn open(&self, mut garble: &GarbleDyn) -> Vec<u8>
    {
        let n: usize = self.n;
        (0..n).map(|i| (self.target[i] ^ garble.base[i])).collect::<Vec<u8>>()
    }
    pub fn to_share(&self, mut o: usize) -> EvalDyn
    {
        let n: usize = self.n;
        EvalDyn { target: (0..o).map(|i| {
    let mut v = 0;
    for j in 0.. 8{
    let bit = (self.target[((i * 8) + j)] & 1);
    v |= (bit << j);
};
    v
}).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn and_via_table<D: Digest>(&self, mut other: &EvalDyn, mut table: &GarbleTableDyn) -> EvalDyn
    {
        let n: usize = self.n;
        let index = (if ((self.target[0] & 1) == 1){
    1
} else {
    0
} | if ((other.target[0] & 1) == 1){
    2
} else {
    0
});
        let hash = {
    let mut d = D::new();
    d.update(&self.target);
    d.update(&other.target);
    d.finalize().to_vec()
};
        EvalDyn { target: (0..n).map(|i| (hash[i] ^ table.table[index][i])).collect::<Vec<u8>>(), n: 0 }
    }
}

impl  GlobalSecretDyn {
    pub fn new(mut n: usize, mut secret: Vec<u8>) -> Self
    {
        secret[0] |= 1;
        Self { secret: secret }
    }
    pub fn secret(&self) -> Vec<u8>
    {
        let n: usize = self.n;
        self.secret.clone()
    }
    pub fn encode(&self, mut garble: &GarbleDyn, mut value: bool) -> EvalDyn
    {
        let n: usize = self.n;
        EvalDyn { target: (0..n).map(|i| {
    if value{
    (self.secret[i] ^ garble.base[i])
} else {
    garble.base[i]
}
}).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn one_wire_eval(&self) -> EvalDyn
    {
        let n: usize = self.n;
        self.encode(&Garble::zero(), true)
    }
    pub fn not_garble(&self, mut a: &GarbleDyn) -> GarbleDyn
    {
        let n: usize = self.n;
        GarbleDyn { base: (0..n).map(|i| (a.base[i] ^ self.secret[i])).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn gen_and_table<D: Digest>(&self, mut a: &GarbleDyn, mut b: &GarbleDyn) -> GarbleTableDyn
    {
        let n: usize = self.n;
        let result_base = a.and_result(b);
        let mut table = (0..n).map(|_| (0..n).map(|_| 0).collect::<Vec<u8>>()).collect::<Vec<_>>();
        for i in 0.. 4{
    let av = ((i & 1) != 0);
    let bv = ((i & 2) != 0);
    let ea = self.encode(a, av);
    let eb = self.encode(b, bv);
    let row = (((ea.target[0] & 1) as usize) | (((eb.target[0] & 1) as usize) << 1));
    let result_label = self.encode(&result_base, (av & bv));
    let mut d = D::new();
    d.update(&ea.target);
    d.update(&eb.target);
    let hash = d.finalize().to_vec();
    table[row] = (0..n).map(|j| (hash[j] ^ result_label.target[j])).collect::<Vec<u8>>();
};
        GarbleTableDyn { table: table, n: 0 }
    }
}

impl  BitXor<EvalDyn> for EvalDyn {
    type Output = EvalDyn;
    fn bitxor(self, mut rhs: EvalDyn) -> Self::Output
    {
        let n: usize = self.n;
        return EvalDyn { target: (0..n).map(|i| (self.target[i] ^ rhs.target[i])).collect::<Vec<u8>>(), n: 0 };
    }
}

impl  GarbledCircuitDyn {
    pub fn encode_inputs(&self, mut bits: &[bool; I]) -> [EvalDyn; I]
    {
        let n: usize = self.n;
        let i: usize = self.i;
        let a: usize = self.a;
        (0..n).map(|i| self.secret.encode(&self.input_labels[i], bits[i])).collect::<Vec<_>>()
    }
    pub fn eval_setup(&self) -> EvalSetupDyn
    {
        let n: usize = self.n;
        let i: usize = self.i;
        let a: usize = self.a;
        EvalSetupDyn { one_wire: self.secret.one_wire_eval(), tables: self.tables.clone(), output_label: self.output_label.clone(), n: 0, a: 0 }
    }
}

impl  EvalSetupDyn {
    pub fn recover_output(&self, mut result: &EvalDyn) -> bool
    {
        let n: usize = self.n;
        let a: usize = self.a;
        ((result.open(&self.output_label)[0] & 1) != 0)
    }
}

impl <D: Digest> SoftSpokenOutDyn<D> {
    pub fn check(&self) -> bool
    {
        let m: usize = self.m;
        let l: usize = self.l;
        (self.sender_tag == self.receiver_tag)
    }
}

impl  LweOtCrs {
    pub fn sample<R: SpecRng>(mut rng: &mut R) -> Self
    {
        let mut a = [[0; LWE_N]; LWE_N];
        for i in 0.. LWE_N{
    for j in 0.. LWE_N{
    a[i][j] = sample_zq(rng);
}
};
        let mut h = [0; LWE_N];
        for i in 0.. LWE_N{
    h[i] = sample_zq(rng);
};
        Self { a: a, h: h }
    }
}

impl <T: Clone + Add<Output = T> + Mul<Output = T> + Default> IdealCotDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T> + Default {
    pub fn new(mut n: usize, mut delta: DeltaDyn<T>) -> Self
    {
        Self { delta: delta }
    }
    pub fn cot<R: SpecRng>(&self, mut rng: &mut R, mut sample_t: impl Fn, mut b: bool) -> (Vec<T>, Vec<T>)
    {
        let n: usize = self.n;
        let r0 = (0..n).map(|_| sample_t(rng)).collect::<Vec<T>>();
        let v = if b{
    (0..n).map(|i| (r0[i].clone() + self.delta.delta[i].clone())).collect::<Vec<T>>()
} else {
    (0..n).map(|i| r0[i].clone()).collect::<Vec<T>>()
};
        (r0, v)
    }
}

impl  Group for ToyGroup {
    type Element = ToyElement;
    type Scalar = u64;
    fn generator() -> ToyElement
    {
        ToyElement(TOY_G)
    }
    fn random_scalar<R: SpecRng>(mut rng: &mut R) -> u64
    {
        let lo = (rng.next_u32() as u64);
        let hi = (rng.next_u32() as u64);
        (((hi << 32) | lo) % (TOY_P - 1))
    }
    fn scalar_mul(mut elt: &ToyElement, mut k: &u64) -> ToyElement
    {
        ToyElement(toy_pow(elt.0, *k))
    }
    fn add(mut a: &ToyElement, mut b: &ToyElement) -> ToyElement
    {
        ToyElement(toy_mul(a.0, b.0))
    }
    fn neg(mut a: &ToyElement) -> ToyElement
    {
        ToyElement(toy_pow(a.0, (TOY_P - 2)))
    }
    fn write_element<D: Digest>(mut elt: &ToyElement, mut h: &mut D)
    {
        h.update(elt.0.to_le_bytes());
    }
}

impl  Fe25519 {
    pub fn is_zero(&self) -> bool
    {
        (self.0 == vec![0, 0, 0, 0])
    }
    pub fn to_bytes(self) -> [u8; 32]
    {
        let mut out = [0; 32];
        for i in 0.. 4{
    out[(i * 8)..((i * 8) + 8)].copy_from_slice(&self.0[i].to_le_bytes());
};
        out
    }
}

impl  Add for Fe25519 {
    type Output = Self;
    fn add(self, mut rhs: Self) -> Self
    {
        fe_add(&self, &rhs)
    }
}

impl  Sub for Fe25519 {
    type Output = Self;
    fn sub(self, mut rhs: Self) -> Self
    {
        fe_sub(&self, &rhs)
    }
}

impl  Mul for Fe25519 {
    type Output = Self;
    fn mul(self, mut rhs: Self) -> Self
    {
        fe_mul(&self, &rhs)
    }
}

impl  Neg for Fe25519 {
    type Output = Self;
    fn neg(self) -> Self
    {
        fe_neg(&self)
    }
}

impl  EdPoint {
    pub fn base() -> Self
    {
        let x = Fe25519(BASE_X_LIMBS);
        let y = Fe25519(BASE_Y_LIMBS);
        Self { x: x, y: y, z: Fe25519::ONE, t: fe_mul(&x, &y) }
    }
    pub fn to_affine(&self) -> (Fe25519, Fe25519)
    {
        let zinv = fe_invert(&self.z);
        (fe_mul(&self.x, &zinv), fe_mul(&self.y, &zinv))
    }
}

impl  PartialEq for EdPoint {
    fn eq(&self, mut other: &Self) -> bool
    {
        let lhs_x = fe_mul(&self.x, &other.z);
        let rhs_x = fe_mul(&other.x, &self.z);
        let lhs_y = fe_mul(&self.y, &other.z);
        let rhs_y = fe_mul(&other.y, &self.z);
        ((lhs_x == rhs_x) && (lhs_y == rhs_y))
    }
}

impl  Eq for EdPoint {
}

impl  Group for Ed25519 {
    type Element = EdPoint;
    type Scalar = [u8; 32];
    fn generator() -> EdPoint
    {
        EdPoint::base()
    }
    fn random_scalar<R: SpecRng>(mut rng: &mut R) -> [u8; 32]
    {
        let mut k = [0; 32];
        for byte in k.iter_mut(){
    *byte = rng.next_u8();
};
        k[31] &= 63;
        k
    }
    fn scalar_mul(mut elt: &EdPoint, mut k: &[u8; 32]) -> EdPoint
    {
        ed_scalar_mul(elt, k)
    }
    fn add(mut a: &EdPoint, mut b: &EdPoint) -> EdPoint
    {
        ed_add(a, b)
    }
    fn neg(mut a: &EdPoint) -> EdPoint
    {
        ed_neg(a)
    }
    fn write_element<D_: Digest>(mut elt: &EdPoint, mut h: &mut D_)
    {
        let (x, y) = elt.to_affine();
        h.update(x.to_bytes());
        h.update(y.to_bytes());
    }
}

pub fn field_mul<T: FieldMulBackend>(mut a: T, mut b: T, mut c: T) -> T
{
    let mut p: T = T::default();
    let mut a = a;
    let mut b = b;
    let h = (T::from(1) << (((size_of_val(&p) << 3) - 1) as u32));
    for _ in 0.. (size_of_val(&p) << 3){
    if ((b.clone() & T::from(1)) != T::default()){
    p ^= a.clone();
};
    let high_bit = (a.clone() & h.clone());
    a <<= 1;
    if (high_bit != T::default()){
    a ^= c.clone();
};
    b >>= 1;
};
    p
}

pub fn field_square<T: FieldMulBackend>(mut a: T, mut c: T) -> T
{
    field_mul(a.clone(), a, c)
}

pub fn field_invert<T: FieldMulBackend>(mut a: T, mut c: T, mut w: u32) -> T
{
    if (a == T::default()){
    return T::default();
};
    let e = (w - 1);
    let mut r = a.clone();
    let mut k: u32 = 1;
    let msb = (31 - e.leading_zeros());
    for bit_pos in 0..msb.rev(){
    let mut tmp = r.clone();
    for _ in 0.. k{
    tmp = field_square(tmp, c.clone());
};
    r = field_mul(tmp, r, c.clone());
    k *= 2;
    if (((e >> bit_pos) & 1) == 1){
    r = field_mul(field_square(r, c.clone()), a.clone(), c.clone());
    k += 1;
}
};
    field_square(r, c)
}

pub fn gf_mul_u8(mut a: u8, mut b: u8, mut c: u8) -> u8
{
    let mut p: u8 = 0;
    let mut a = a;
    let mut b = b;
    for _ in 0.. 8{
    if ((b & 1) != 0){
    p ^= a;
};
    let high = (a & 128);
    a <<= 1;
    if (high != 0){
    a ^= c;
};
    b >>= 1;
};
    p
}

pub fn gf_mul_u64(mut a: u64, mut b: u64, mut c: u64) -> u64
{
    let mut p: u64 = 0;
    let mut a = a;
    let mut b = b;
    let h: u64 = (1 << 63);
    for _ in 0.. 64{
    if ((b & 1) != 0){
    p ^= a;
};
    let high = (a & h);
    a <<= 1;
    if (high != 0){
    a ^= c;
};
    b >>= 1;
};
    p
}

pub fn gf_mul_u128(mut a: u128, mut b: u128, mut c: u128) -> u128
{
    let mut p: u128 = 0;
    let mut a = a;
    let mut b = b;
    let h: u128 = (1 << 127);
    for _ in 0.. 128{
    if ((b & 1) != 0){
    p ^= a;
};
    let high = (a & h);
    a <<= 1;
    if (high != 0){
    a ^= c;
};
    b >>= 1;
};
    p
}

pub fn gf_invert_u8(mut a: u8, mut c: u8) -> u8
{
    if (a == 0){
    return 0;
};
    let e: u32 = 7;
    let msb: u32 = 2;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0.. msb{
    let bit_pos = ((msb - 1) - bit_pos_rev);
    let mut tmp = r;
    for _ in 0.. k{
    tmp = gf_mul_u8(tmp, tmp, c);
};
    r = gf_mul_u8(tmp, r, c);
    k *= 2;
    if (((e >> bit_pos) & 1) == 1){
    r = gf_mul_u8(gf_mul_u8(r, r, c), a, c);
    k += 1;
}
};
    gf_mul_u8(r, r, c)
}

pub fn gf_invert_u64(mut a: u64, mut c: u64) -> u64
{
    if (a == 0){
    return 0;
};
    let e: u32 = 63;
    let msb: u32 = 5;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0.. msb{
    let bit_pos = ((msb - 1) - bit_pos_rev);
    let mut tmp = r;
    for _ in 0.. k{
    tmp = gf_mul_u64(tmp, tmp, c);
};
    r = gf_mul_u64(tmp, r, c);
    k *= 2;
    if (((e >> bit_pos) & 1) == 1){
    r = gf_mul_u64(gf_mul_u64(r, r, c), a, c);
    k += 1;
}
};
    gf_mul_u64(r, r, c)
}

pub fn gf_invert_u128(mut a: u128, mut c: u128) -> u128
{
    if (a == 0){
    return 0;
};
    let e: u32 = 127;
    let msb: u32 = 6;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0.. msb{
    let bit_pos = ((msb - 1) - bit_pos_rev);
    let mut tmp = r;
    for _ in 0.. k{
    tmp = gf_mul_u128(tmp, tmp, c);
};
    r = gf_mul_u128(tmp, r, c);
    k *= 2;
    if (((e >> bit_pos) & 1) == 1){
    r = gf_mul_u128(gf_mul_u128(r, r, c), a, c);
    k += 1;
}
};
    gf_mul_u128(r, r, c)
}

pub fn gf_mul_256(mut a: U256, mut b: U256, mut c: U256) -> U256
{
    let mut p = U256::ZERO;
    let mut a = a;
    let mut b = b;
    for _ in 0.. 256{
    if b.bit(0){
    p = p.xor(&a);
};
    let high = a.high_bit();
    a = a.shl1();
    if high{
    a = a.xor(&c);
};
    b = b.shr1();
};
    p
}

pub fn gf_invert_256(mut a: U256, mut c: U256) -> U256
{
    if a.is_zero(){
    return U256::ZERO;
};
    let e: u32 = 255;
    let msb: u32 = 7;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0.. msb{
    let bit_pos = ((msb - 1) - bit_pos_rev);
    let mut tmp = r;
    for _ in 0.. k{
    tmp = gf_mul_256(tmp, tmp, c);
};
    r = gf_mul_256(tmp, r, c);
    k *= 2;
    if (((e >> bit_pos) & 1) == 1){
    r = gf_mul_256(gf_mul_256(r, r, c), a, c);
    k += 1;
}
};
    gf_mul_256(r, r, c)
}

pub fn commit<D: Digest>(mut message: &impl AsRef<[u8]>, mut rand: &impl AsRef<[u8]>) -> CommitmentCoreDyn<D>
{
    let mut hasher = D::new();
    hasher.update(AsRef::<[u8]>::as_ref(&message));
    hasher.update(AsRef::<[u8]>::as_ref(&rand));
    CommitmentCoreDyn(hasher.finalize().to_vec(), PhantomData)
}

pub fn concat_words(mut wbound: usize, mut a: &GrafhenWordDyn, mut b: &GrafhenWordDyn) -> Option<GrafhenWordDyn>
{
    let new_len = match a.len.checked_add(b.len) {
    Some(n) => n,
    _ => return None,
};
    let mut result = GrafhenWord::identity();
    result.data[..a.len].copy_from_slice(&a.data[..a.len]);
    result.data[a.len..new_len].copy_from_slice(&b.data[..b.len]);
    result.len = new_len;
    Some(result)
}

pub fn grafhen_zero(mut wbound: usize) -> GrafhenWordDyn
{
    GrafhenWord::identity()
}

pub fn grafhen_xor(mut wbound: usize, mut a: &GrafhenWordDyn, mut b: &GrafhenWordDyn) -> GrafhenWordDyn
{
    concat_words(wbound, a, b).expect("grafhen_xor: combined word length exceeds WBOUND")
}

pub fn grafhen_not<R>(mut wbound: usize, mut a: &GrafhenWordDyn, mut pk: &GrafhenPublicDyn<R>) -> GrafhenWordDyn
{
    grafhen_xor(wbound, a, &pk.enc_one)
}

pub fn grafhen_and<R>(mut wbound: usize, mut enc_a: &GrafhenWordDyn, mut enc_b: &GrafhenWordDyn, mut pk: &GrafhenPublicDyn<R>) -> GrafhenWordDyn
{
    let w1 = enc_a;
    let w2 = enc_b;
    let a = &pk.and_w1;
    let b = &pk.and_w2;
    let segs: [&GrafhenWordDyn; 12] = vec![w1, a, w1, w2, b, w2, w1, a, w1, w2, b, w2];
    let total_len: usize = segs.iter().map(|s| s.len).sum();
    let mut result = GrafhenWord::identity();
    let mut pos = 0;
    for seg in &segs{
    result.data[pos..(pos + seg.len)].copy_from_slice(&seg.data[..seg.len]);
    pos += seg.len;
};
    result.len = total_len;
    pk.reducer.reduce(&mut result);
    result
}

pub fn grafhen_encrypt<R>(mut wbound: usize, mut bit: bool, mut zero_cipher: &GrafhenWordDyn, mut pk: &GrafhenPublicDyn<R>) -> GrafhenWordDyn
{
    if bit{
    grafhen_xor(wbound, zero_cipher, &pk.enc_one)
} else {
    *zero_cipher
}
}

pub fn eval_word_to_perm(mut n: usize, mut d: usize, mut wbound: usize, mut key: &GrafhenKeyDyn, mut word: &GrafhenWordDyn) -> [u8; N]
{
    let mut perm: [u8; N] = (0..n).map(|i| (i as u8)).collect::<Vec<_>>();
    for .. in &word.data[..word.len]{
    let g = (g as usize);
    let generator: &[u8; N] = if (g < d){
    &key.gens[g]
} else {
    &key.inv_gens[(g - d)]
};
    perm = (0..n).map(|i| generator[(perm[i] as usize)]).collect::<Vec<_>>();
};
    perm
}

pub fn grafhen_decrypt(mut n: usize, mut d: usize, mut wbound: usize, mut key: &GrafhenKeyDyn, mut word: &GrafhenWordDyn) -> Option<bool>
{
    let perm = eval_word_to_perm(n, d, wbound, key, word);
    match perm[0] {
    .. => Some(false),
    .. => Some(true),
    _ => None,
}
}

pub fn gf_mul(mut a: u8, mut b: u8) -> u8
{
    volar_primitives::gf_mul_u8(a, b, GF8_AES_POLY)
}

pub fn sub_bytes(mut state: &mut [u8; BLOCK])
{
    for i in 0.. BLOCK{
    state[i] = SBOX[(state[i] as usize)];
}
}

pub fn shift_rows(mut state: &mut [u8; BLOCK])
{
    let t = state[1];
    state[1] = state[5];
    state[5] = state[9];
    state[9] = state[13];
    state[13] = t;
    let t = state[2];
    state[2] = state[10];
    state[10] = t;
    let t = state[6];
    state[6] = state[14];
    state[14] = t;
    let t = state[15];
    state[15] = state[11];
    state[11] = state[7];
    state[7] = state[3];
    state[3] = t;
}

pub fn mix_columns(mut state: &mut [u8; BLOCK])
{
    for c in 0.. 4{
    let i = (4 * c);
    let s0 = state[i];
    let s1 = state[(i + 1)];
    let s2 = state[(i + 2)];
    let s3 = state[(i + 3)];
    state[i] = (((gf_mul(s0, 2) ^ gf_mul(s1, 3)) ^ s2) ^ s3);
    state[(i + 1)] = (((s0 ^ gf_mul(s1, 2)) ^ gf_mul(s2, 3)) ^ s3);
    state[(i + 2)] = (((s0 ^ s1) ^ gf_mul(s2, 2)) ^ gf_mul(s3, 3));
    state[(i + 3)] = (((gf_mul(s0, 3) ^ s1) ^ s2) ^ gf_mul(s3, 2));
}
}

pub fn add_round_key(mut state: &mut [u8; BLOCK], mut round_key: &[u8; BLOCK])
{
    for i in 0.. BLOCK{
    state[i] ^= round_key[i];
}
}

pub fn key_expansion(mut key: &[u8; BLOCK]) -> [[u8; BLOCK]; NK_ROUND_KEYS]
{
    let mut words = [[0; 4]; (4 * NK_ROUND_KEYS)];
    for i in 0.. 4{
    words[i] = vec![key[(4 * i)], key[((4 * i) + 1)], key[((4 * i) + 2)], key[((4 * i) + 3)]];
};
    for i in 4.. (4 * NK_ROUND_KEYS){
    let mut temp = words[(i - 1)];
    if ((i % 4) == 0){
    let t0 = temp[0];
    temp[0] = temp[1];
    temp[1] = temp[2];
    temp[2] = temp[3];
    temp[3] = t0;
    for b in 0.. 4{
    temp[b] = SBOX[(temp[b] as usize)];
};
    temp[0] ^= RCON[(i / 4)];
};
    for b in 0.. 4{
    words[i][b] = (words[(i - 4)][b] ^ temp[b]);
}
};
    let mut round_keys = [[0; BLOCK]; NK_ROUND_KEYS];
    for r in 0.. NK_ROUND_KEYS{
    for c in 0.. 4{
    let w = words[((4 * r) + c)];
    round_keys[r][(4 * c)] = w[0];
    round_keys[r][((4 * c) + 1)] = w[1];
    round_keys[r][((4 * c) + 2)] = w[2];
    round_keys[r][((4 * c) + 3)] = w[3];
}
};
    round_keys
}

pub fn encrypt_block(mut key: &[u8; BLOCK], mut plain: &[u8; BLOCK]) -> [u8; BLOCK]
{
    let round_keys = key_expansion(key);
    let mut state = *plain;
    add_round_key(&mut state, &round_keys[0]);
    for r in 1.. NR{
    sub_bytes(&mut state);
    shift_rows(&mut state);
    mix_columns(&mut state);
    add_round_key(&mut state, &round_keys[r]);
};
    sub_bytes(&mut state);
    shift_rows(&mut state);
    add_round_key(&mut state, &round_keys[NR]);
    state
}

pub fn vole_hash(mut key: &UniversalHashKey, mut input: &[u8]) -> UniversalHashOutput
{
    let n_full = (input.len() / 16);
    let tail = &input[(n_full * 16)..];
    let mut h0 = Galois128(0);
    let mut h1 = Galois64(0);
    let mut pow0 = key.r0;
    let mut pow1 = key.r1;
    for i in 0.. n_full{
    let block = &input[(i * 16)..((i + 1) * 16)];
    let mut bytes = [0; 16];
    bytes.copy_from_slice(block);
    let s = Galois128(u128::from_le_bytes(bytes));
    h0 = (h0 + (s * pow0));
    let s64 = Galois64((s.0 as u64));
    h1 = (h1 + (s64 * pow1));
    pow0 = (pow0 * key.r0);
    pow1 = (pow1 * key.r1);
};
    if !tail.is_empty(){
    let mut bytes = [0; 8];
    let n = tail.len().min(8);
    bytes[..n].copy_from_slice(&tail[..n]);
    let t = Galois64(u64::from_le_bytes(bytes));
    h1 = (h1 + (t * pow1));
};
    UniversalHashOutput { h0: h0, h1: h1 }
}

pub fn zk_hash(mut key: &UniversalHashKey, mut elements: &[Galois128]) -> UniversalHashOutput
{
    let mut h0 = Galois128(0);
    let mut h1 = Galois64(0);
    let mut pow0 = key.r0;
    let mut pow1 = key.r1;
    for .. in elements{
    h0 = (h0 + (x * pow0));
    let x64 = Galois64((x.0 as u64));
    h1 = (h1 + (x64 * pow1));
    pow0 = (pow0 * key.r0);
    pow1 = (pow1 * key.r1);
};
    UniversalHashOutput { h0: h0, h1: h1 }
}

pub fn vole_hash_consistency_check(mut key: &UniversalHashKey, mut hu: UniversalHashOutput, mut hq: UniversalHashOutput, mut hv: UniversalHashOutput, mut hc: UniversalHashOutput, mut delta: Galois128) -> bool
{
    let lhs0 = hq.h0;
    let rhs0 = (hv.h0 + (delta * (hu.h0 + hc.h0)));
    let delta64 = Galois64((delta.0 as u64));
    let lhs1 = hq.h1;
    let rhs1 = (hv.h1 + (delta64 * (hu.h1 + hc.h1)));
    ((lhs0 == rhs0) && (lhs1 == rhs1))
}

pub fn add_to_upper_word(mut iv: &mut [u8; BLOCK], mut tweak: u32)
{
    let upper = u32::from_le_bytes(vec![iv[12], iv[13], iv[14], iv[15]]);
    let new = upper.wrapping_add(tweak);
    let bytes = new.to_le_bytes();
    iv[12] = bytes[0];
    iv[13] = bytes[1];
    iv[14] = bytes[2];
    iv[15] = bytes[3];
}

pub fn add_to_lower_word(mut iv: &[u8; BLOCK], mut counter: u32) -> [u8; BLOCK]
{
    let mut out = *iv;
    let lower = u32::from_le_bytes(vec![out[0], out[1], out[2], out[3]]);
    let new = lower.wrapping_add(counter);
    let bytes = new.to_le_bytes();
    out[0] = bytes[0];
    out[1] = bytes[1];
    out[2] = bytes[2];
    out[3] = bytes[3];
    out
}

pub fn aes_ctr_prg(mut seed: &[u8; BLOCK], mut iv: &[u8; BLOCK], mut tweak: u32, mut out_bytes: usize) -> Vec<u8>
{
    let mut iv_tweaked = *iv;
    add_to_upper_word(&mut iv_tweaked, tweak);
    let n_full = (out_bytes / BLOCK);
    let rem = (out_bytes % BLOCK);
    let mut out = alloc::vec::Vec::with_capacity(out_bytes);
    for i in 0.. n_full{
    let block_in = add_to_lower_word(&iv_tweaked, (i as u32));
    let ct = encrypt_block(seed, &block_in);
    out.extend_from_slice(&ct);
};
    if (rem > 0){
    let block_in = add_to_lower_word(&iv_tweaked, (n_full as u32));
    let ct = encrypt_block(seed, &block_in);
    out.extend_from_slice(&ct[..rem]);
};
    out
}

pub fn chall1(mut mu: &[u8], mut iv: &[u8; 16], mut com_bytes: &[u8], mut lambda_plus_b: usize, mut use_shake256: bool) -> Vec<u8>
{
    let mut t = if use_shake256{
    FaestTranscript::new_shake256()
} else {
    FaestTranscript::new_shake128()
};
    t.absorb(mu);
    t.absorb(AsRef::<[u8]>::as_ref(&iv));
    t.absorb(com_bytes);
    t.squeeze(lambda_plus_b)
}

pub fn chall2(mut chall_1: &[u8], mut u_hat: &[u8], mut d: &[u8], mut lambda_plus_b: usize, mut use_shake256: bool) -> Vec<u8>
{
    let mut t = if use_shake256{
    FaestTranscript::new_shake256()
} else {
    FaestTranscript::new_shake128()
};
    t.absorb(chall_1);
    t.absorb(u_hat);
    t.absorb(d);
    t.squeeze(lambda_plus_b)
}

pub fn chall3(mut chall_2: &[u8], mut a_hat: &[u8], mut b_hat: &[u8], mut c_hat: &[u8], mut lambda: usize, mut use_shake256: bool) -> Vec<u8>
{
    let mut t = if use_shake256{
    FaestTranscript::new_shake256()
} else {
    FaestTranscript::new_shake128()
};
    t.absorb(chall_2);
    t.absorb(a_hat);
    t.absorb(b_hat);
    t.absorb(c_hat);
    t.squeeze(lambda)
}

pub fn grind_chall3(mut chall_2: &[u8], mut a_hat: &[u8], mut b_hat: &[u8], mut c_hat_base: &[u8], mut lambda: usize, mut w_grind: u32, mut use_shake256: bool, mut max_iters: u32) -> Option<(Vec<u8>, u32)>
{
    for counter in 0.. max_iters{
    let counter_bytes = counter.to_le_bytes();
    let mut c_hat_grind = alloc::vec::Vec::from(c_hat_base);
    c_hat_grind.extend_from_slice(&counter_bytes);
    let candidate = chall3(chall_2, a_hat, b_hat, &c_hat_grind, lambda, use_shake256);
    if has_trailing_zero_bits(&candidate, w_grind){
    return Some((candidate, counter));
}
};
    None
}

pub fn has_trailing_zero_bits(mut bytes: &[u8], mut n: u32) -> bool
{
    if (n == 0){
    return true;
};
    let n = (n as usize);
    let full_bytes = (n / 8);
    let rem_bits = (n % 8);
    if (bytes.len() < (full_bytes + if (rem_bits > 0){
    1
} else {
    0
})){
    return false;
};
    for i in (bytes.len() - full_bytes).. bytes.len(){
    if (bytes[i] != 0){
    return false;
}
};
    if (rem_bits > 0){
    let mask = ((1 << rem_bits) - 1);
    let byte_idx = ((bytes.len() - full_bytes) - 1);
    if ((bytes[byte_idx] & mask) != 0){
    return false;
}
};
    true
}

pub fn convert_to_vole(mut seeds: &[Option<[u8; LAMBDA_BYTES]>], mut iv: &[u8; 16], mut tweak: u32, mut l_hat_bytes: usize) -> ConvertOutput
{
    let n = seeds.len();
    let d = (n.trailing_zeros() as usize);
    let zero_block = vec![];
    let mut r: Vec<Vec<u8>> = Vec::with_capacity(n);
    for s in seeds{
    match s {
    Some(seed) => r.push(aes_ctr_prg(seed, iv, tweak, l_hat_bytes)),
    None => r.push(zero_block.clone()),
}
};
    let mut v: Vec<Vec<u8>> = (0..d).map(|_| vec![]).collect::<Vec<_>>();
    let mut level: Vec<Vec<u8>> = r;
    for j in 0.. d{
    let half = (level.len() / 2);
    let mut next: Vec<Vec<u8>> = Vec::with_capacity(half);
    for i in 0.. half{
    xor_in_place(&mut v[j], &level[((2 * i) + 1)]);
    let mut new_entry = level[(2 * i)].clone();
    xor_in_place(&mut new_entry, &level[((2 * i) + 1)]);
    next.push(new_entry);
};
    level = next;
};
    let u = level.into_iter().next().unwrap();
    ConvertOutput { u: u, v: v }
}

pub fn concat_small_voles(mut outs: Vec<ConvertOutput>) -> BigVoleProver
{
    let l_hat = outs[0].u.len();
    for o in &outs{
    for vj in &o.v{
}
};
    let u = outs[0].u.clone();
    let mut c: Vec<Vec<u8>> = Vec::with_capacity((outs.len() - 1));
    for o in outs.iter().skip(1){
    let mut ci = o.u.clone();
    xor_in_place(&mut ci, &u);
    c.push(ci);
};
    let mut v_columns: Vec<Vec<u8>> = Vec::new();
    for o in outs{
    for vj in o.v{
    v_columns.push(vj);
}
};
    BigVoleProver { u: u, c: c, v_columns: v_columns }
}

pub fn concat_small_voles_verifier(mut outs: Vec<ConvertOutput>, mut deltas: &[usize], mut corrections: &[Vec<u8>]) -> BigVoleVerifier
{
    let mut q_columns: Vec<Vec<u8>> = Vec::new();
    for (i, o) in outs.into_iter().enumerate(){
    let k = o.v.len();
    let delta_i = deltas[i];
    for (bit, vj_raw) in o.v.into_iter().enumerate(){
    let mut q = vj_raw;
    if (i >= 1){
    let delta_bit = (((delta_i >> bit) & 1) == 1);
    if delta_bit{
    xor_in_place(&mut q, &corrections[(i - 1)]);
}
};
    q_columns.push(q);
};
    let _ = k;
};
    BigVoleVerifier { q_columns: q_columns }
}

pub fn xor_in_place(mut a: &mut [u8], mut b: &[u8])
{
    for i in 0.. a.len(){
    a[i] ^= b[i];
}
}

pub fn keygen(mut rng: &mut impl SpecRng) -> (FaestSecretKey, FaestPublicKey)
{
    let mut sk = [0; LAMBDA_BYTES];
    for b in sk.iter_mut(){
    *b = rng.next_u8();
};
    let pk = aes128_encrypt(&sk, &[0; LAMBDA_BYTES]);
    (FaestSecretKey(sk), FaestPublicKey(pk))
}

pub fn sign(mut sk: &FaestSecretKey, mut pk: &FaestPublicKey, mut message: &[u8], mut iv_seed: [u8; LAMBDA_BYTES], mut prover: &impl FaestAesProver) -> FaestSignature
{
    let iv: [u8; LAMBDA_BYTES] = aes128_encrypt(&iv_seed, &[0; LAMBDA_BYTES]);
    let r: [u8; LAMBDA_BYTES] = aes128_encrypt(&sk.0, &iv);
    let commitment: BavcCommitmentDyn = Bavc::<EmLeafCommit, COM_BYTES, Sha3_256>::commit(r, &iv, TAU, SUB_VOLE_N);
    let mu: Vec<u8> = {
    let mut h = Sha3_256::new();
    DigestUpdate::update(&mut h, &pk.0);
    DigestUpdate::update(&mut h, message);
    Digest::finalize(h).to_vec()
};
    let chall_1 = chall1(&mu, &iv, &commitment.root, (LAMBDA_BYTES + 8), false);
    let deltas = expand_challenge_to_deltas(&chall_1, TAU, SUB_VOLE_N);
    let nodes = Bavc::<EmLeafCommit, COM_BYTES>::collect_open_nodes(&deltas, &recompute_tree(r, (TAU * SUB_VOLE_N)), TAU, SUB_VOLE_N);
    let hidden_commits: Vec<[u8; COM_BYTES]> = deltas.iter().enumerate().map(|(i, d)| commitment.commitments[((i * SUB_VOLE_N) + d)]).collect::<Vec<_>>();
    let opening = BavcOpeningDyn { hidden_commits: hidden_commits.clone(), nodes: nodes.clone(), com_bytes: 0 };
    let _ = opening;
    let mut sub_voles = Vec::with_capacity(TAU);
    for i in 0.. TAU{
    let seeds_i: Vec<Option<[u8; LAMBDA_BYTES]>> = (0..SUB_VOLE_N).map(|j| Some(commitment.seeds[((i * SUB_VOLE_N) + j)])).collect::<Vec<_>>();
    sub_voles.push(convert_to_vole(&seeds_i, &iv, (i as u32), L_HAT_BYTES));
};
    let big_vole: BigVoleProver = concat_small_voles(sub_voles);
    let corrections_flat: Vec<u8> = big_vole.c.iter().flatten().cloned().collect();
    let chall_2 = chall2(&chall_1, &big_vole.u, &corrections_flat, (LAMBDA_BYTES + 8), false);
    let hash_key = hash_key_from_chall(&chall_2);
    let qs_proof = prover.prove_aes_witness(&big_vole, &hash_key);
    let (chall_3, counter) = grind_chall3(&chall_2, &qs_proof.a_hat, &qs_proof.b_hat, &qs_proof.c_hat_base, LAMBDA_BYTES, W_GRIND, false, 1000000).expect("grinding must terminate within 1M iterations");
    let mut c_hat_with_counter = qs_proof.c_hat_base.clone();
    c_hat_with_counter.extend_from_slice(&counter.to_le_bytes());
    FaestSignature { iv: iv, bavc_root: commitment.root.clone(), hidden_commits: hidden_commits, nodes: nodes, corrections: big_vole.c.clone(), vole_u: big_vole.u.clone(), qs_proof: qs_proof, c_hat_with_counter: c_hat_with_counter, chall_3: chall_3, counter: counter }
}

pub fn verify(mut pk: &FaestPublicKey, mut message: &[u8], mut sig: &FaestSignature) -> bool
{
    let iv = &sig.iv;
    let mu: Vec<u8> = {
    let mut h = Sha3_256::new();
    DigestUpdate::update(&mut h, &pk.0);
    DigestUpdate::update(&mut h, message);
    Digest::finalize(h).to_vec()
};
    let chall_1 = chall1(&mu, iv, &sig.bavc_root, (LAMBDA_BYTES + 8), false);
    let deltas = expand_challenge_to_deltas(&chall_1, TAU, SUB_VOLE_N);
    let reconstructed_seeds_opt = Bavc::<EmLeafCommit, COM_BYTES, Sha3_256>::reconstruct(&sig.nodes, &sig.hidden_commits, &deltas, iv, &sig.bavc_root, TAU, SUB_VOLE_N);
    let reconstructed_seeds = match reconstructed_seeds_opt {
    Some(s) => s,
    None => return false,
};
    let mut sub_voles_v = Vec::with_capacity(TAU);
    for i in 0.. TAU{
    let d = deltas[i];
    let verifier_seeds: Vec<Option<[u8; LAMBDA_BYTES]>> = (0..SUB_VOLE_N).map(|j| {
    if (j == 0){
    None
} else {
    Some(reconstructed_seeds[((i * SUB_VOLE_N) + (j ^ d))])
}
}).collect::<Vec<_>>();
    sub_voles_v.push(convert_to_vole(&verifier_seeds, iv, (i as u32), L_HAT_BYTES));
};
    let corrections = &sig.corrections;
    if (corrections.len() != (TAU - 1)){
    return false;
};
    let big_q: Vec<u8> = {
    let q_out = concat_small_voles_verifier(sub_voles_v, &deltas, corrections);
    q_out.q_columns.into_iter().flatten().collect::<Vec<_>>()
};
    let corrections_flat: Vec<u8> = sig.corrections.iter().flatten().cloned().collect();
    let chall_2 = chall2(&chall_1, &sig.vole_u, &corrections_flat, (LAMBDA_BYTES + 8), false);
    let hash_key = hash_key_from_chall(&chall_2);
    let derived_chall_3 = chall3(&chall_2, &sig.qs_proof.a_hat, &sig.qs_proof.b_hat, &sig.c_hat_with_counter, LAMBDA_BYTES, false);
    if (derived_chall_3 != sig.chall_3){
    return false;
};
    if !has_trailing_zero_bits(&sig.chall_3, W_GRIND){
    return false;
};
    let _ = hash_key;
    true
}

pub fn expand_challenge_to_deltas(mut chall_1: &[u8], mut tau: usize, mut n: usize) -> Vec<usize>
{
    (0..tau).map(|i| {
    let byte = (chall_1[(i % chall_1.len())] as usize);
    (byte % n)
}).collect::<Vec<_>>()
}

pub fn recompute_tree(mut r: [u8; LAMBDA_BYTES], mut total_leaves: usize) -> Vec<[u8; LAMBDA_BYTES]>
{
    let total_nodes = ((2 * total_leaves) - 1);
    let mut tree = vec![];
    tree[0] = r;
    for node in 0.. (total_leaves - 1){
    let parent = Vec::<u8, U16>(tree[node]);
    let .. = AesCtrLengthDoubler::double(parent);
    tree[((2 * node) + 1)] = left.0;
    tree[((2 * node) + 2)] = right.0;
};
    tree
}

pub fn hash_key_from_chall(mut chall: &[u8]) -> UniversalHashKey
{
    let mut r0_bytes = [0; 16];
    let n = chall.len().min(16);
    r0_bytes[..n].copy_from_slice(&chall[..n]);
    let mut r1_bytes = [0; 8];
    let off = n;
    let m = (chall.len() - off).min(8);
    r1_bytes[..m].copy_from_slice(&chall[off..(off + m)]);
    UniversalHashKey { r0: Galois128(u128::from_le_bytes(r0_bytes)), r1: Galois64(u64::from_le_bytes(r1_bytes)) }
}

pub fn has_trailing_zero_bits(mut bytes: &[u8], mut n: u32) -> bool
{
    if (n == 0){
    return true;
};
    let n = (n as usize);
    let full_bytes = (n / 8);
    let rem = (n % 8);
    if (bytes.len() < (full_bytes + if (rem > 0){
    1
} else {
    0
})){
    return false;
};
    for i in (bytes.len() - full_bytes).. bytes.len(){
    if (bytes[i] != 0){
    return false;
}
};
    if (rem > 0){
    let mask = ((1 << rem) - 1);
    let idx = ((bytes.len() - full_bytes) - 1);
    if ((bytes[idx] & mask) != 0){
    return false;
}
};
    true
}

pub fn gen_abo<B: LengthDoubler, D: Digest>(mut k: usize, mut n: usize, mut a: Vec<u8>, mut rand: &impl AsRef<[u8]>) -> ABODyn<B, D> where B: Sized
{
    let mut h = D::new();
    let per_byte = (0..n).map(|_ni| {
    let mut per_byte = (0..k).map(|_| <Vec<u8>>::default()).collect::<Vec<Vec<u8>>>();
    for i in 0.. k{
    let core = (0..k.ilog2()).fold(a.clone(), |mut acc, b| {
    if (((i >> b) & 1) != 0){
    let doubled = B::double(acc);
    acc = doubled[1].clone();
} else {
    let doubled = B::double(acc);
    acc = doubled[0].clone();
};
    acc
});
    h.update(&commit::<D>(&core, rand));
    per_byte[i] = core;
};
    return per_byte;
}).collect::<Vec<Vec<Vec<u8>>>>();
    ABODyn { commit: h.finalize().to_vec(), per_byte: per_byte, k: 0, n: 0, _phantom: PhantomData }
}

pub fn create_vole_from_material<B: LengthDoubler, X: AsRef<[u8]>>(mut s: &[X]) -> VopeDyn<u8>
{
    let u: Vec<u8> = s.iter().fold((0..<<B>::OutputSize as Unsigned>::to_usize()).map(|_| 0).collect::<Vec<u8>>(), |mut a, b| {
    (0..<<B>::OutputSize as Unsigned>::to_usize()).map(|i| a[i].bitxor(AsRef::<[u8]>::as_ref(&b)[i])).collect::<Vec<u8>>()
});
    let v: Vec<u8> = s.iter().enumerate().fold((0..<<B>::OutputSize as Unsigned>::to_usize()).map(|_| 0).collect::<Vec<u8>>(), |mut a, (i, b)| {
    (0..<<B>::OutputSize as Unsigned>::to_usize()).map(|j| a[j].bitxor(AsRef::<[u8]>::as_ref(&b)[j]).bitxor((i as u8))).collect::<Vec<u8>>()
});
    VopeDyn { u: (0..1).map(|_| u.clone()).collect::<Vec<Vec<u8>>>(), v: v, n: 0, k: 1 }
}

pub fn create_vole_from_material_expanded<B: LengthDoubler, X: AsRef<[u8]>, Y: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(mut s: &[Y], mut f: F) -> VopeDyn<u8>
{
    let u: Vec<u8> = s.iter().map(|b| f(&AsRef::<[u8]>::as_ref(&b)[..<<B>::OutputSize as Unsigned>::to_usize()])).fold((0..<<B>::OutputSize as Unsigned>::to_usize()).map(|_| 0).collect::<Vec<u8>>(), |mut a, b| {
    (0..<<B>::OutputSize as Unsigned>::to_usize()).map(|i| a[i].bitxor(AsRef::<[u8]>::as_ref(&b)[i])).collect::<Vec<u8>>()
});
    let v: Vec<u8> = s.iter().map(|b| f(&AsRef::<[u8]>::as_ref(&b)[..<<B>::OutputSize as Unsigned>::to_usize()])).enumerate().fold((0..<<B>::OutputSize as Unsigned>::to_usize()).map(|_| 0).collect::<Vec<u8>>(), |mut a, (i, b)| {
    (0..<<B>::OutputSize as Unsigned>::to_usize()).map(|j| a[j].bitxor(AsRef::<[u8]>::as_ref(&b)[j]).bitxor((i as u8))).collect::<Vec<u8>>()
});
    VopeDyn { u: (0..1).map(|_| u.clone()).collect::<Vec<Vec<u8>>>(), v: v, n: 0, k: 1 }
}

pub fn memory_check_per_lane<N, T>(mut n: usize, mut challenges: Vec<T>) -> Vec<MemoryCheckStateDyn<T, AdditiveHasher>> where T: Clone + Default + Add<Output = T> + Mul<Output = T> + PartialEq
{
    (0..n).map(|i| {
    let key = ChallengeKey::from_challenge(challenges[i].clone());
    MemoryCheckState::new(key)
}).collect::<Vec<_>>()
}

pub fn vole_and_prover_step<N, T>(mut n: usize, mut vope_a: VopeDyn<T>, mut vope_b: VopeDyn<T>) -> (VopeDyn<T>, Vec<T>) where T: Clone + Add<Output = T> + Mul<Output = T> + Default
{
    let u_c_inner = (0..n).map(|i| {
    (vope_a.u[0][i].clone() * vope_b.u[0][i].clone())
}).collect::<Vec<T>>();
    let u_c = (0..1).map(|_| u_c_inner.clone()).collect::<Vec<Vec<T>>>();
    let v_c = (0..n).map(|i| {
    ((vope_a.v[i].clone() * vope_b.u[0][i].clone()) + (vope_b.v[i].clone() * vope_a.u[0][i].clone()))
}).collect::<Vec<T>>();
    let hat = (0..n).map(|i| {
    (vope_a.v[i].clone() * vope_b.v[i].clone())
}).collect::<Vec<T>>();
    (VopeDyn { u: u_c, v: v_c, n: 0, k: 1 }, hat)
}

pub fn vole_and_verifier_check<N, T>(mut n: usize, mut delta: &DeltaDyn<T>, mut q_a: &QDyn<T>, mut q_b: &QDyn<T>, mut q_and: &QDyn<T>, mut hat: &Vec<T>) -> (QDyn<T>, bool) where T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default
{
    let mut ok = true;
    for i in 0.. n{
    let lhs = ((q_a.q[i].clone() * q_b.q[i].clone()) + hat[i].clone());
    let rhs = (q_and.q[i].clone() * delta.delta[i].clone());
    ok = (ok && (lhs == rhs));
};
    (QDyn { q: q_and.q.clone(), n: 0 }, ok)
}

pub fn vole_sbox_prover_step<N, T>(mut n: usize, mut vope_a: VopeDyn<T>, mut vope_b: VopeDyn<T>) -> (VopeDyn<T>, VopeDyn<T>) where T: Add<Output = T> + Mul<Output = T> + Default + Clone
{
    let k2: VopeDyn<T> = vope_a.mul_generalized(&vope_b);
    let k1 = VopeDyn { u: (0..1).map(|_| k2.u[1].clone()).collect::<Vec<Vec<T>>>(), v: k2.u[0].clone(), n: 0, k: 1 };
    (k1, k2)
}

pub fn vole_sbox_verifier_check<N, T>(mut n: usize, mut delta: &DeltaDyn<T>, mut q_a: &QDyn<T>, mut q_b: &QDyn<T>, mut vope_k2: VopeDyn<T>) -> (QDyn<T>, bool) where T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default + Into<T>
{
    let q_c = (vope_k2 * delta.clone());
    let mut ok = true;
    for i in 0.. n{
    ok = (ok && ((q_a.q[i].clone() * q_b.q[i].clone()) == q_c.q[i]));
};
    (q_c, ok)
}

pub fn vole_mul3_prover_step<N, T>(mut n: usize, mut vope_a: &VopeDyn<T>, mut vope_b: &VopeDyn<T>, mut vope_d: &VopeDyn<T>) -> VopeDyn<T> where T: Add<Output = T> + Mul<Output = T> + Default + Clone
{
    let ab: VopeDyn<T> = vope_a.mul_generalized(vope_b);
    ab.mul_generalized(vope_d)
}

pub fn vole_mul3_verifier_check<N, T>(mut n: usize, mut delta: &DeltaDyn<T>, mut q_a: &QDyn<T>, mut q_b: &QDyn<T>, mut q_d: &QDyn<T>, mut vope_abd: VopeDyn<T>) -> (QDyn<T>, bool) where T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default + Into<T>
{
    let q_abd = (vope_abd * delta.clone());
    let mut ok = true;
    for i in 0.. n{
    let lhs = ((q_a.q[i].clone() * q_b.q[i].clone()) * q_d.q[i].clone());
    ok = (ok && (lhs == q_abd.q[i]));
};
    (q_abd, ok)
}

pub fn random_nonzero_delta<N, T, R>(mut n: usize, mut rng: &mut R, mut sample_t: impl Fn, mut is_zero: impl Fn) -> DeltaDyn<T>
{
    DeltaDyn { delta: (0..n).map(|_| {
    let mut x = sample_t(rng);
    let mut tries = 0;
    while (is_zero(&x) && (tries < 64)){
    x = sample_t(rng);
    tries += 1;
};
    x
}).collect::<Vec<T>>(), n: 0 }
}

pub fn lift_bit<T: Clone>(mut n: usize, mut bit_t: T) -> Vec<T>
{
    (0..n).map(|_| bit_t.clone()).collect::<Vec<T>>()
}

pub fn vole_commit_bit<N, T, R>(mut n: usize, mut cot: &IdealCotDyn<T>, mut rng: &mut R, mut sample_t: impl Fn, mut bit_to_t: impl FnMut(bool) -> T, mut bit: bool) -> (VopeDyn<T>, QDyn<T>) where T: Clone + Add<Output = T> + Mul<Output = T> + Default
{
    let (r0, v) = cot.cot(rng, sample_t, bit);
    let u_t = bit_to_t(bit);
    let u_row: Vec<T> = lift_bit(n, u_t);
    let u: Vec<Vec<T>> = (0..1).map(|_| {
    (0..n).map(|i| u_row[i].clone()).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>();
    let q = (0..n).map(|i| r0[i].clone()).collect::<Vec<T>>();
    (VopeDyn { u: u, v: v, n: 0, k: 1 }, QDyn { q: q, n: 0 })
}

pub fn derive_and_q<N, T>(mut n: usize, mut delta: &DeltaDyn<T>, mut q_a: &QDyn<T>, mut q_b: &QDyn<T>, mut hat: &Vec<T>) -> QDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T> + Invert + Default
{
    QDyn { q: (0..n).map(|i| {
    let lhs = ((q_a.q[i].clone() * q_b.q[i].clone()) + hat[i].clone());
    (lhs * delta.delta[i].invert())
}).collect::<Vec<T>>(), n: 0 }
}

pub fn tfhe_trivial_zero(mut n_lwe: usize) -> LweCiphertextDyn
{
    LweCiphertextDyn { a: [0; n_lwe], b: 0, n_lwe: 0 }
}

pub fn tfhe_trivial_one(mut n_lwe: usize) -> LweCiphertextDyn
{
    LweCiphertextDyn { a: [0; n_lwe], b: Q4, n_lwe: 0 }
}

pub fn tfhe_trivial_encrypt(mut n_lwe: usize, mut b: bool) -> LweCiphertextDyn
{
    if b{
    tfhe_trivial_one(n_lwe)
} else {
    tfhe_trivial_zero(n_lwe)
}
}

pub fn tfhe_xor(mut n_lwe: usize, mut a: LweCiphertextDyn, mut b: LweCiphertextDyn) -> LweCiphertextDyn
{
    let mut out_a = [0; n_lwe];
    for i in 0.. n_lwe{
    out_a[i] = a.a[i].wrapping_add(b.a[i]);
};
    LweCiphertextDyn { a: out_a, b: a.b.wrapping_add(b.b), n_lwe: 0 }
}

pub fn tfhe_not(mut n_lwe: usize, mut a: LweCiphertextDyn) -> LweCiphertextDyn
{
    let mut out_a = [0; n_lwe];
    for i in 0.. n_lwe{
    out_a[i] = a.a[i].wrapping_neg();
};
    LweCiphertextDyn { a: out_a, b: Q4.wrapping_sub(a.b), n_lwe: 0 }
}

pub fn tfhe_gate_bootstrapping_and(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut ct_a: LweCiphertextDyn, mut ct_b: LweCiphertextDyn, mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let mut ct = lwe_add(n_lwe, ct_a, ct_b);
    ct.b = ct.b.wrapping_sub((Q4 >> 1));
    let acc = blind_rotate(n_lwe, big_n, bs_ell, ks_ell, &ct, bk);
    let lwe_big = sample_extract(big_n, &acc);
    let mut ct_out = key_switch(n_lwe, big_n, ks_ell, &lwe_big, &bk.ksk);
    ct_out.b = ct_out.b.wrapping_add((Q4 >> 1));
    ct_out
}

pub fn tfhe_gate_bootstrapping_or(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut ct_a: LweCiphertextDyn, mut ct_b: LweCiphertextDyn, mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let mut ct = lwe_add(n_lwe, ct_a, ct_b);
    ct.b = ct.b.wrapping_add((Q4 >> 1));
    let acc = blind_rotate(n_lwe, big_n, bs_ell, ks_ell, &ct, bk);
    let lwe_big = sample_extract(big_n, &acc);
    let mut ct_out = key_switch(n_lwe, big_n, ks_ell, &lwe_big, &bk.ksk);
    ct_out.b = ct_out.b.wrapping_add((Q4 >> 1));
    ct_out
}

pub fn tfhe_cmux(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut sel: LweCiphertextDyn, mut a: LweCiphertextDyn, mut b: LweCiphertextDyn, mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let not_sel = tfhe_not(n_lwe, sel);
    let sel_and_a = tfhe_gate_bootstrapping_and(n_lwe, big_n, bs_ell, ks_ell, sel, a, bk);
    let nsel_and_b = tfhe_gate_bootstrapping_and(n_lwe, big_n, bs_ell, ks_ell, not_sel, b, bk);
    tfhe_gate_bootstrapping_or(n_lwe, big_n, bs_ell, ks_ell, sel_and_a, nsel_and_b, bk)
}

pub fn tfhe_programmable_bootstrap(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut ct: LweCiphertextDyn, mut test_poly: [u32; BIG_N], mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let acc = blind_rotate_with_poly(n_lwe, big_n, bs_ell, ks_ell, &ct, test_poly, bk);
    let lwe_big = sample_extract(big_n, &acc);
    key_switch(n_lwe, big_n, ks_ell, &lwe_big, &bk.ksk)
}

pub fn tfhe_lut_read(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut addr_bits: &[LweCiphertextDyn], mut lut: &[bool], mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let two_n = (2 * big_n);
    let k = lut.len().max(1).next_power_of_two();
    if (!lut.is_empty() && lut.iter().all(|..| (v == lut[0]))){
    let msg = if lut[0]{
    Q4
} else {
    0
};
    return LweCiphertextDyn { a: [0; n_lwe], b: msg, n_lwe: 0 };
};
    let half_q4 = (Q4 >> 1);
    let step = (two_n / k);
    let half_k = (k / 2);
    let poly_step = (big_n / half_k);
    let mut test_poly = [0; big_n];
    for j in 0.. big_n{
    let entry_idx = (j / poly_step);
    let val = if ((entry_idx < lut.len()) && lut[entry_idx]){
    half_q4
} else {
    half_q4.wrapping_neg()
};
    test_poly[j] = val;
};
    let delta = ((1 << 32) / (k as u64));
    let mut combined = LweCiphertextDyn { a: [0; n_lwe], b: 0, n_lwe: 0 };
    for (j, addr_ct) in addr_bits.iter().enumerate(){
    let target = ((1 << j) * delta);
    for i in 0.. n_lwe{
    let scaled = ((addr_ct.a[i] as u64).wrapping_mul(target) / (Q4 as u64));
    combined.a[i] = combined.a[i].wrapping_add((scaled as u32));
};
    let scaled_b = ((addr_ct.b as u64).wrapping_mul(target) / (Q4 as u64));
    combined.b = combined.b.wrapping_add((scaled_b as u32));
};
    let centering = ((delta / 2) as u32);
    combined.b = combined.b.wrapping_add(centering);
    let mut ct_out = tfhe_programmable_bootstrap(n_lwe, big_n, bs_ell, ks_ell, combined, test_poly, bk);
    ct_out.b = ct_out.b.wrapping_add(half_q4);
    ct_out
}

pub fn lwe_encrypt<R: SpecRng>(mut n_lwe: usize, mut m: bool, mut sk: &LweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> LweCiphertextDyn
{
    let mut a = [0; n_lwe];
    for ai in a.iter_mut(){
    *ai = rng.next_u32();
};
    let mut dot: u32 = 0;
    for i in 0.. n_lwe{
    dot = dot.wrapping_add(a[i].wrapping_mul((sk.key[i] as u32)));
};
    let e: u32 = small_noise(noise_bits, rng);
    let msg = if m{
    Q4
} else {
    0
};
    let b = dot.wrapping_add(e).wrapping_add(msg);
    LweCiphertextDyn { a: a, b: b, n_lwe: 0 }
}

pub fn lwe_decrypt(mut n_lwe: usize, mut ct: &LweCiphertextDyn, mut sk: &LweSecretKeyDyn) -> bool
{
    let mut dot: u32 = 0;
    for i in 0.. n_lwe{
    dot = dot.wrapping_add(ct.a[i].wrapping_mul((sk.key[i] as u32)));
};
    let phase = ct.b.wrapping_sub(dot);
    let half = (Q4 >> 1);
    let shifted = phase.wrapping_sub(half);
    (shifted < Q4)
}

pub fn gen_lwe_secret_key<R: SpecRng>(mut n_lwe: usize, mut rng: &mut R) -> LweSecretKeyDyn
{
    let mut key = [0; n_lwe];
    for k in key.iter_mut(){
    *k = ((rng.next_u8() & 1) as u8);
};
    LweSecretKeyDyn { key: key, n_lwe: 0 }
}

pub fn gen_rlwe_secret_key<R: SpecRng>(mut big_n: usize, mut rng: &mut R) -> RlweSecretKeyDyn
{
    let mut key = [0; big_n];
    for k in key.iter_mut(){
    *k = ((rng.next_u8() & 1) as u32);
};
    RlweSecretKeyDyn { key: key, big_n: 0 }
}

pub fn gen_bootstrapping_key<R: SpecRng>(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut lwe_sk: &LweSecretKeyDyn, mut rlwe_sk: &RlweSecretKeyDyn, mut bs_bg_log: u32, mut ks_bg_log: u32, mut bs_noise_bits: u32, mut ks_noise_bits: u32, mut rng: &mut R) -> BootstrappingKeyDyn
{
    let bsk = (0..n).map(|i| {
    let bit = (lwe_sk.key[i] != 0);
    rgsw_encrypt(big_n, bs_ell, bit, rlwe_sk, bs_bg_log, bs_noise_bits, rng)
}).collect::<Vec<_>>();
    let rlwe_as_lwe = LweSecretKeyDyn { key: (0..n).map(|i| (rlwe_sk.key[i] as u8)).collect::<Vec<_>>(), n_lwe: 0 };
    let ksk_array: [[LweCiphertextDyn; KS_ELL]; BIG_N] = (0..n).map(|i| {
    let s_bit = rlwe_sk.key[i];
    (0..n).map(|j| {
    let shift = 32.saturating_sub(ks_bg_log.saturating_mul(((j as u32) + 1)));
    let msg_val = s_bit.wrapping_shl(shift);
    lwe_encrypt_raw(n_lwe, msg_val, lwe_sk, ks_noise_bits, rng)
}).collect::<Vec<_>>()
}).collect::<Vec<_>>();
    let _ = rlwe_as_lwe;
    let ksk = KeySwitchingKeyDyn { ksk: ksk_array, ks_bg_log: ks_bg_log, n_lwe: 0, big_n: 0, ks_ell: 0 };
    BootstrappingKeyDyn { bsk: bsk, ksk: ksk, bs_bg_log: bs_bg_log, n_lwe: 0, big_n: 0, bs_ell: 0, ks_ell: 0 }
}

pub fn rlwe_encrypt_scalar<R: SpecRng>(mut big_n: usize, mut m: u32, mut sk: &RlweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> RlweCiphertextDyn
{
    let a: [u32; BIG_N] = (0..big_n).map(|_| rng.next_u32()).collect::<Vec<_>>();
    let mut b = poly_mul_neg(&a, &sk.key);
    b[0] = b[0].wrapping_add(small_noise(noise_bits, rng)).wrapping_add(m);
    RlweCiphertextDyn { a: a, b: b, big_n: 0 }
}

pub fn rlwe_encrypt_poly<R: SpecRng>(mut big_n: usize, mut msg_poly: &[u32; BIG_N], mut sk: &RlweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> RlweCiphertextDyn
{
    let a: [u32; BIG_N] = (0..big_n).map(|_| rng.next_u32()).collect::<Vec<_>>();
    let mut b = poly_mul_neg(&a, &sk.key);
    for i in 0.. big_n{
    b[i] = b[i].wrapping_add(small_noise(noise_bits, rng)).wrapping_add(msg_poly[i]);
};
    RlweCiphertextDyn { a: a, b: b, big_n: 0 }
}

pub fn rgsw_encrypt<R: SpecRng>(mut big_n: usize, mut bs_ell: usize, mut m: bool, mut sk: &RlweSecretKeyDyn, mut bs_bg_log: u32, mut noise_bits: u32, mut rng: &mut R) -> RgswCiphertextDyn
{
    let msg_bit = if m{
    1
} else {
    0
};
    let rows = (0..n).map(|j| {
    let shift = 32.saturating_sub(bs_bg_log.saturating_mul(((j as u32) + 1)));
    let g_factor = 1.wrapping_shl(shift);
    let contrib = msg_bit.wrapping_mul(g_factor);
    let mut rlwe0 = rlwe_encrypt_scalar(big_n, 0, sk, noise_bits, rng);
    rlwe0.a[0] = rlwe0.a[0].wrapping_add(contrib);
    let rlwe1 = rlwe_encrypt_scalar(big_n, contrib, sk, noise_bits, rng);
    RgswRowDyn { rlwe0: rlwe0, rlwe1: rlwe1, big_n: 0 }
}).collect::<Vec<_>>();
    RgswCiphertextDyn { rows: rows, big_n: 0, bs_ell: 0 }
}

pub fn blind_rotate_with_poly(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut ct: &LweCiphertextDyn, mut test_poly: [u32; BIG_N], mut bk: &BootstrappingKeyDyn) -> RlweCiphertextDyn
{
    let mut acc = RlweCiphertextDyn { a: [0; big_n], b: test_poly, big_n: 0 };
    let two_n = (2 * big_n);
    let log2_two_n = two_n.trailing_zeros();
    let scale_shift = 32.saturating_sub(log2_two_n);
    let b_exp = torus_to_exp(ct.b, scale_shift, two_n);
    if (b_exp != 0){
    acc = rlwe_rotate(big_n, &acc, (two_n - b_exp));
};
    for i in 0.. n_lwe{
    let a_exp = torus_to_exp(ct.a[i], scale_shift, two_n);
    if (a_exp != 0){
    let acc_rotated = rlwe_rotate(big_n, &acc, a_exp);
    acc = cmux(big_n, bs_ell, &bk.bsk[i], &acc_rotated, &acc, bk.bs_bg_log);
}
};
    acc
}

pub fn and_test_poly(mut big_n: usize) -> [u32; BIG_N]
{
    let mut v = [0; big_n];
    let half_q4 = (Q4 >> 1);
    for k in 0.. (big_n / 2){
    v[k] = half_q4.wrapping_neg();
};
    for k in (big_n / 2).. big_n{
    v[k] = half_q4;
};
    v
}

pub fn blind_rotate(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut ct: &LweCiphertextDyn, mut bk: &BootstrappingKeyDyn) -> RlweCiphertextDyn
{
    blind_rotate_with_poly(n_lwe, big_n, bs_ell, ks_ell, ct, and_test_poly(big_n), bk)
}

pub fn torus_to_exp(mut x: u32, mut scale_shift: u32, mut two_n: usize) -> usize
{
    let half = if (scale_shift > 0){
    (1 << (scale_shift - 1))
} else {
    0
};
    let exp = ((x.wrapping_add(half) >> scale_shift) as usize);
    (exp & (two_n - 1))
}

pub fn sample_extract(mut big_n: usize, mut rlwe: &RlweCiphertextDyn) -> LweCiphertextDyn
{
    let mut a_lwe = [0; big_n];
    a_lwe[0] = rlwe.a[0];
    for i in 1.. big_n{
    a_lwe[i] = rlwe.a[(big_n - i)].wrapping_neg();
};
    LweCiphertextDyn { a: a_lwe, b: rlwe.b[0], n_lwe: 0 }
}

pub fn key_switch(mut n_lwe: usize, mut big_n: usize, mut ks_ell: usize, mut ct_big: &LweCiphertextDyn, mut ksk: &KeySwitchingKeyDyn) -> LweCiphertextDyn
{
    let mut out_a = [0; n_lwe];
    let mut out_b = ct_big.b;
    for i in 0.. big_n{
    let digits = ks_decompose::<KS_ELL>(ct_big.a[i], ksk.ks_bg_log);
    for j in 0.. ks_ell{
    let d = (digits[j] as u32);
    if (d == 0){
    continue;
};
    let ksk_ct = &ksk.ksk[i][j];
    for k in 0.. n_lwe{
    out_a[k] = out_a[k].wrapping_sub(d.wrapping_mul(ksk_ct.a[k]));
};
    out_b = out_b.wrapping_sub(d.wrapping_mul(ksk_ct.b));
}
};
    LweCiphertextDyn { a: out_a, b: out_b, n_lwe: 0 }
}

pub fn ks_decompose(mut ks_ell: usize, mut x: u32, mut bg_log: u32) -> [u32; KS_ELL]
{
    let bg = (1 << bg_log);
    let mask = (bg - 1);
    let mut rem = (x as u64);
    let tail_shift = 32.saturating_sub(bg_log.saturating_mul((ks_ell as u32)));
    if ((tail_shift > 0) && (tail_shift < 32)){
    let half_tail = (1 << (tail_shift - 1));
    rem = rem.wrapping_add(half_tail);
};
    let mut digits = [0; ks_ell];
    for j in 0..ks_ell.rev(){
    let shift = 32.saturating_sub(bg_log.saturating_mul(((j as u32) + 1)));
    if (shift < 32){
    digits[j] = (((rem >> shift) & mask) as u32);
}
};
    digits
}

pub fn cmux(mut big_n: usize, mut bs_ell: usize, mut c: &RgswCiphertextDyn, mut d1: &RlweCiphertextDyn, mut d0: &RlweCiphertextDyn, mut bs_bg_log: u32) -> RlweCiphertextDyn
{
    let diff = rlwe_sub(big_n, d1, d0);
    let prod = external_product(big_n, bs_ell, c, &diff, bs_bg_log);
    rlwe_add(big_n, d0, &prod)
}

pub fn external_product(mut big_n: usize, mut bs_ell: usize, mut rgsw: &RgswCiphertextDyn, mut rlwe: &RlweCiphertextDyn, mut bs_bg_log: u32) -> RlweCiphertextDyn
{
    let a_decomp = poly_decompose::<BIG_N, BS_ELL>(&rlwe.a, bs_bg_log);
    let b_decomp = poly_decompose::<BIG_N, BS_ELL>(&rlwe.b, bs_bg_log);
    let mut out_a = [0; big_n];
    let mut out_b = [0; big_n];
    for j in 0.. bs_ell{
    let row = &rgsw.rows[j];
    let prod_a0 = poly_mul_neg(&a_decomp[j], &row.rlwe0.a);
    let prod_a1 = poly_mul_neg(&a_decomp[j], &row.rlwe0.b);
    let prod_b0 = poly_mul_neg(&b_decomp[j], &row.rlwe1.a);
    let prod_b1 = poly_mul_neg(&b_decomp[j], &row.rlwe1.b);
    for k in 0.. big_n{
    out_a[k] = out_a[k].wrapping_add(prod_a0[k]).wrapping_add(prod_b0[k]);
    out_b[k] = out_b[k].wrapping_add(prod_a1[k]).wrapping_add(prod_b1[k]);
}
};
    RlweCiphertextDyn { a: out_a, b: out_b, big_n: 0 }
}

pub fn poly_decompose(mut big_n: usize, mut bs_ell: usize, mut p: &[u32; BIG_N], mut bg_log: u32) -> [[u32; BIG_N]; BS_ELL]
{
    let bg = (1 << bg_log);
    let mask = ((bg - 1) as u32);
    let mut result = [[0; big_n]; bs_ell];
    for i in 0.. big_n{
    let x = p[i];
    let tail_bits = 32.saturating_sub(bg_log.saturating_mul((bs_ell as u32)));
    let rounded = if ((tail_bits > 0) && (tail_bits < 32)){
    x.wrapping_add((1 << (tail_bits - 1)))
} else {
    x
};
    for j in 0.. bs_ell{
    let shift = 32.saturating_sub(bg_log.saturating_mul(((j as u32) + 1)));
    result[j][i] = if (shift < 32){
    ((rounded >> shift) & mask)
} else {
    0
};
}
};
    result
}

pub fn rlwe_add(mut big_n: usize, mut a: &RlweCiphertextDyn, mut b: &RlweCiphertextDyn) -> RlweCiphertextDyn
{
    RlweCiphertextDyn { a: poly_add_neg(&a.a, &b.a), b: poly_add_neg(&a.b, &b.b), big_n: 0 }
}

pub fn rlwe_sub(mut big_n: usize, mut a: &RlweCiphertextDyn, mut b: &RlweCiphertextDyn) -> RlweCiphertextDyn
{
    RlweCiphertextDyn { a: poly_sub_neg(&a.a, &b.a), b: poly_sub_neg(&a.b, &b.b), big_n: 0 }
}

pub fn rlwe_rotate(mut big_n: usize, mut ct: &RlweCiphertextDyn, mut exp: usize) -> RlweCiphertextDyn
{
    RlweCiphertextDyn { a: poly_rotate(&ct.a, exp), b: poly_rotate(&ct.b, exp), big_n: 0 }
}

pub fn lwe_add(mut n_lwe: usize, mut a: LweCiphertextDyn, mut b: LweCiphertextDyn) -> LweCiphertextDyn
{
    let mut out_a = [0; n_lwe];
    for i in 0.. n_lwe{
    out_a[i] = a.a[i].wrapping_add(b.a[i]);
};
    LweCiphertextDyn { a: out_a, b: a.b.wrapping_add(b.b), n_lwe: 0 }
}

pub fn lwe_encrypt_raw<R: SpecRng>(mut n_lwe: usize, mut msg: u32, mut sk: &LweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> LweCiphertextDyn
{
    let mut a = [0; n_lwe];
    for ai in a.iter_mut(){
    *ai = rng.next_u32();
};
    let mut dot: u32 = 0;
    for i in 0.. n_lwe{
    dot = dot.wrapping_add(a[i].wrapping_mul((sk.key[i] as u32)));
};
    let e = small_noise(noise_bits, rng);
    let b = dot.wrapping_add(e).wrapping_add(msg);
    LweCiphertextDyn { a: a, b: b, n_lwe: 0 }
}

pub fn poly_mul_neg(mut n: usize, mut a: &[u32; N], mut b: &[u32; N]) -> [u32; N]
{
    let mut result = [0; n];
    for i in 0.. n{
    for j in 0.. n{
    let deg = (i + j);
    if (deg < n){
    result[deg] = result[deg].wrapping_add(a[i].wrapping_mul(b[j]));
} else {
    result[(deg - n)] = result[(deg - n)].wrapping_sub(a[i].wrapping_mul(b[j]));
}
}
};
    result
}

pub fn poly_add_neg(mut n: usize, mut a: &[u32; N], mut b: &[u32; N]) -> [u32; N]
{
    let mut result = [0; n];
    for i in 0.. n{
    result[i] = a[i].wrapping_add(b[i]);
};
    result
}

pub fn poly_sub_neg(mut n: usize, mut a: &[u32; N], mut b: &[u32; N]) -> [u32; N]
{
    let mut result = [0; n];
    for i in 0.. n{
    result[i] = a[i].wrapping_sub(b[i]);
};
    result
}

pub fn poly_rotate(mut n: usize, mut p: &[u32; N], mut exp: usize) -> [u32; N]
{
    let exp = (exp % (2 * n));
    if (exp == 0){
    return *p;
};
    let mut result = [0; n];
    for i in 0.. n{
    let new_pos = (i + exp);
    if (new_pos < n){
    result[new_pos] = result[new_pos].wrapping_add(p[i]);
} else if (new_pos < (2 * n)){
    result[(new_pos - n)] = result[(new_pos - n)].wrapping_sub(p[i]);
} else {
    result[(new_pos - (2 * n))] = result[(new_pos - (2 * n))].wrapping_add(p[i]);
}
};
    result
}

pub fn small_noise<R: SpecRng>(mut noise_bits: u32, mut rng: &mut R) -> u32
{
    if (noise_bits >= 32){
    return rng.next_u32();
};
    let raw: u32 = rng.next_u32();
    let mask = (1 << noise_bits).wrapping_sub(1);
    let small = (raw & mask);
    if ((noise_bits > 0) && ((small >> (noise_bits - 1)) != 0)){
    (small | !mask)
} else {
    small
}
}

pub fn prg_with_index<D: Digest>(mut seed: &[u8], mut idx: u32, mut out: &mut [u8])
{
    let mut counter: u32 = 0;
    let mut pos = 0;
    while (pos < out.len()){
    let mut h = D::new();
    h.update(seed);
    h.update(idx.to_le_bytes());
    h.update(counter.to_le_bytes());
    let block = h.finalize().to_vec();
    let block_bytes: &[u8] = AsRef::<[u8]>::as_ref(&block);
    let take = (out.len() - pos).min(block_bytes.len());
    out[pos..(pos + take)].copy_from_slice(&block_bytes[..take]);
    pos += take;
    counter += 1;
}
}

pub fn prg_to_bools<D: Digest>(mut seed: &[u8], mut out: &mut [bool])
{
    let mut counter: u32 = 0;
    let mut pos = 0;
    while (pos < out.len()){
    let mut h = D::new();
    h.update(seed);
    h.update(counter.to_le_bytes());
    let block = h.finalize().to_vec();
    let block_bytes: &[u8] = AsRef::<[u8]>::as_ref(&block);
    for .. in block_bytes.iter(){
    for bit in 0.. 8{
    if (pos >= out.len()){
    return;
};
    out[pos] = (((byte >> bit) & 1) == 1);
    pos += 1;
}
};
    counter += 1;
}
}

pub fn pack_kappa(mut bits: &[bool; IKNP_KAPPA]) -> [u8; IKNP_KAPPA_BYTES]
{
    let mut out = [0; IKNP_KAPPA_BYTES];
    for i in 0.. IKNP_KAPPA{
    if bits[i]{
    out[(i / 8)] |= (1 << (i % 8));
}
};
    out
}

pub fn iknp_cot_extend<G, D, R>(mut m: usize, mut l: usize, mut rng_s: &mut R, mut rng_r: &mut R, mut receiver_bits: &[bool; M], mut delta_msg: &[u8; L]) -> ([[u8; L]; M], [[u8; L]; M]) where D: Digest
{
    let mut delta_ot = [false; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    delta_ot[i] = ((rng_s.next_u32() & 1) == 1);
};
    let delta_ot_bytes = pack_kappa(&delta_ot);
    let mut seeds_0 = [[0; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    let mut seeds_1 = [[0; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    for b in 0.. IKNP_KAPPA_BYTES{
    seeds_0[i][b] = ((rng_r.next_u32() & 255) as u8);
    seeds_1[i][b] = ((rng_r.next_u32() & 255) as u8);
}
};
    let mut chosen_seeds = [[0; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    let (s_state, s_msg) = ot_send_setup::<G, D, _>(rng_r);
    let (r_state, r_msg) = ot_recv::<G, D, _>(rng_s, s_msg, delta_ot[i]);
    let (key_0, key_1) = ot_send_finish::<G, D>(&s_state, &r_msg);
    let kc = ot_recv_finish::<G, D>(&r_state);
    let mut e0 = [0; IKNP_KAPPA_BYTES];
    let mut e1 = [0; IKNP_KAPPA_BYTES];
    ot_send_payload::<D>(&key_0, &key_1, &seeds_0[i], &seeds_1[i], &mut e0, &mut e1);
    let chosen_e: &[u8] = if delta_ot[i]{
    &e1
} else {
    &e0
};
    ot_recv_payload::<D>(&kc, chosen_e, &mut chosen_seeds[i]);
};
    let mut t_cols = [[false; m]; IKNP_KAPPA];
    let mut q_cols = [[false; m]; IKNP_KAPPA];
    {
    let mut prg1 = [false; m];
    for i in 0.. IKNP_KAPPA{
    prg_to_bools::<D>(&seeds_0[i], &mut t_cols[i]);
    prg_to_bools::<D>(&seeds_1[i], &mut prg1);
    let mut u_col = [false; m];
    for j in 0.. m{
    u_col[j] = ((t_cols[i][j] ^ prg1[j]) ^ receiver_bits[j]);
};
    let mut prg_chosen = [false; m];
    prg_to_bools::<D>(&chosen_seeds[i], &mut prg_chosen);
    for j in 0.. m{
    if delta_ot[i]{
    q_cols[i][j] = (prg_chosen[j] ^ u_col[j]);
} else {
    q_cols[i][j] = prg_chosen[j];
}
}
}
};
    let mut sender_r0 = [[0; l]; m];
    let mut receiver_v = [[0; l]; m];
    let mut q_row = [false; IKNP_KAPPA];
    let mut t_row = [false; IKNP_KAPPA];
    for j in 0.. m{
    for i in 0.. IKNP_KAPPA{
    q_row[i] = q_cols[i][j];
    t_row[i] = t_cols[i][j];
};
    let q_bytes = pack_kappa(&q_row);
    let t_bytes = pack_kappa(&t_row);
    let mut r0 = [0; l];
    prg_with_index::<D>(&q_bytes, (j as u32), &mut r0);
    let mut q_xor_delta = q_bytes;
    for b in 0.. IKNP_KAPPA_BYTES{
    q_xor_delta[b] ^= delta_ot_bytes[b];
};
    let mut r1 = [0; l];
    prg_with_index::<D>(&q_xor_delta, (j as u32), &mut r1);
    let mut v_pre = [0; l];
    prg_with_index::<D>(&t_bytes, (j as u32), &mut v_pre);
    let mut correction = [0; l];
    for b in 0.. l{
    correction[b] = ((r0[b] ^ r1[b]) ^ delta_msg[b]);
};
    sender_r0[j] = r0;
    if receiver_bits[j]{
    for b in 0.. l{
    receiver_v[j][b] = (v_pre[b] ^ correction[b]);
}
} else {
    receiver_v[j] = v_pre;
}
};
    (sender_r0, receiver_v)
}

pub fn softspoken_cot_extend<G, D, R>(mut k: usize, mut m: usize, mut l: usize, mut rng_s: &mut R, mut rng_r: &mut R, mut receiver_bits: &[bool; M], mut delta_msg: &[u8; L]) -> SoftSpokenOutDyn<D> where D: Digest
{
    let (sender_r0, receiver_v) = iknp_cot_extend::<G, D, _, M, L>(rng_s, rng_r, receiver_bits, delta_msg);
    let mut hs = D::new();
    hs.update(TAG_DOMAIN);
    hs.update(delta_msg);
    for row in sender_r0.iter(){
    hs.update(row);
};
    let sender_tag = hs.finalize().to_vec();
    let mut hr = D::new();
    hr.update(TAG_DOMAIN);
    hr.update(delta_msg);
    for j in 0.. m{
    let mut r0_reconstructed = [0; l];
    if receiver_bits[j]{
    for b in 0.. l{
    r0_reconstructed[b] = (receiver_v[j][b] ^ delta_msg[b]);
}
} else {
    r0_reconstructed = receiver_v[j];
};
    hr.update(&r0_reconstructed);
};
    let receiver_tag = hr.finalize().to_vec();
    SoftSpokenOutDyn { sender_r0: sender_r0, receiver_v: receiver_v, sender_tag: sender_tag, receiver_tag: receiver_tag, m: 0, l: 0 }
}

pub fn zq_add(mut a: Zq, mut b: Zq) -> Zq
{
    (a.wrapping_add(b) & LWE_Q_MASK)
}

pub fn zq_sub(mut a: Zq, mut b: Zq) -> Zq
{
    (a.wrapping_sub(b) & LWE_Q_MASK)
}

pub fn zq_mul(mut a: Zq, mut b: Zq) -> Zq
{
    (a.wrapping_mul(b) & LWE_Q_MASK)
}

pub fn zq_neg(mut a: Zq) -> Zq
{
    (LWE_Q.wrapping_sub(a) & LWE_Q_MASK)
}

pub fn sample_noise<R: SpecRng>(mut rng: &mut R) -> Zq
{
    let span = ((2 * LWE_NOISE_BOUND) + 1);
    let raw = (rng.next_u32() % span);
    if (raw <= LWE_NOISE_BOUND){
    raw
} else {
    zq_neg((raw - LWE_NOISE_BOUND))
}
}

pub fn sample_zq<R: SpecRng>(mut rng: &mut R) -> Zq
{
    (rng.next_u32() & LWE_Q_MASK)
}

pub fn lwe_ot_recv<R: SpecRng>(mut rng: &mut R, mut crs: &LweOtCrs, mut c: bool) -> (LweOtReceiver, LweOtRecvMsg)
{
    let mut s = [0; LWE_N];
    for i in 0.. LWE_N{
    s[i] = sample_noise(rng);
};
    let mut pk_real = [0; LWE_N];
    for i in 0.. LWE_N{
    let mut acc: Zq = 0;
    for j in 0.. LWE_N{
    acc = zq_add(acc, zq_mul(crs.a[i][j], s[j]));
};
    acc = zq_add(acc, sample_noise(rng));
    pk_real[i] = acc;
};
    let pk0 = if c{
    let mut pk0 = [0; LWE_N];
    for i in 0.. LWE_N{
    pk0[i] = zq_sub(crs.h[i], pk_real[i]);
};
    pk0
} else {
    pk_real
};
    (LweOtReceiver { s: s, c: c }, LweOtRecvMsg { pk0: pk0 })
}

pub fn encrypt_branch<R: SpecRng>(mut l: usize, mut rng: &mut R, mut crs: &LweOtCrs, mut pk: &[Zq; LWE_N], mut msg: &[u8; L]) -> ([Zq; LWE_N], [Zq; L])
{
    let mut r = [0; LWE_N];
    for i in 0.. LWE_N{
    r[i] = sample_noise(rng);
};
    let mut u = [0; LWE_N];
    for j in 0.. LWE_N{
    let mut acc: Zq = 0;
    for i in 0.. LWE_N{
    acc = zq_add(acc, zq_mul(crs.a[i][j], r[i]));
};
    acc = zq_add(acc, sample_noise(rng));
    u[j] = acc;
};
    let mut base: Zq = 0;
    for i in 0.. LWE_N{
    base = zq_add(base, zq_mul(pk[i], r[i]));
};
    let half_q = (LWE_Q / 2);
    let mut v = [0; l];
    for k in 0.. l{
    let plain = if ((msg[k] & 1) == 1){
    half_q
} else {
    0
};
    v[k] = zq_add(zq_add(base, sample_noise(rng)), plain);
};
    (u, v)
}

pub fn lwe_ot_send<R: SpecRng>(mut l: usize, mut rng: &mut R, mut crs: &LweOtCrs, mut recv_msg: &LweOtRecvMsg, mut m0: &[u8; L], mut m1: &[u8; L]) -> LweOtSenderMsgDyn
{
    let pk0 = recv_msg.pk0;
    let mut pk1 = [0; LWE_N];
    for i in 0.. LWE_N{
    pk1[i] = zq_sub(crs.h[i], pk0[i]);
};
    let (u0, v0) = encrypt_branch::<R, L>(rng, crs, &pk0, m0);
    let (u1, v1) = encrypt_branch::<R, L>(rng, crs, &pk1, m1);
    LweOtSenderMsgDyn { u0: u0, v0: v0, u1: u1, v1: v1, l: 0 }
}

pub fn lwe_ot_recv_decrypt(mut l: usize, mut receiver: &LweOtReceiver, mut sender_msg: &LweOtSenderMsgDyn) -> [u8; L]
{
    let (u, v) = if receiver.c{
    (&sender_msg.u1, &sender_msg.v1)
} else {
    (&sender_msg.u0, &sender_msg.v0)
};
    let mut s_dot_u: Zq = 0;
    for i in 0.. LWE_N{
    s_dot_u = zq_add(s_dot_u, zq_mul(receiver.s[i], u[i]));
};
    let quarter = (LWE_Q / 4);
    let three_quarter = (3 * quarter);
    let mut out = [0; l];
    for k in 0.. l{
    let raw = zq_sub(v[k], s_dot_u);
    out[k] = if ((raw > quarter) && (raw <= three_quarter)){
    1
} else {
    0
};
};
    out
}

pub fn ot_send_setup<G: Group, D: Digest, R: SpecRng>(mut rng: &mut R) -> (BaseOtSenderDyn<G, D>, <G as _>::Element)
{
    let y = G::random_scalar(rng);
    let g = G::generator();
    let s = G::scalar_mul(&g, &y);
    let t = G::scalar_mul(&s, &y);
    (BaseOtSenderDyn { y: y, s: s.clone(), t: t, _d: PhantomData, _phantom: PhantomData }, s)
}

pub fn ot_recv<G: Group, D: Digest, R: SpecRng>(mut rng: &mut R, mut s: <G as _>::Element, mut c: bool) -> (BaseOtReceiverDyn<G, D>, OtReceiverMsgDyn<G>)
{
    let x = G::random_scalar(rng);
    let g = G::generator();
    let gx = G::scalar_mul(&g, &x);
    let r = if c{
    G::add(&s, &gx)
} else {
    gx
};
    (BaseOtReceiverDyn { x: x, s: s, c: c, _d: PhantomData, _phantom: PhantomData }, OtReceiverMsgDyn { r: r, _phantom: PhantomData })
}

pub fn ot_send_finish<G: Group, D: Digest>(mut state: &BaseOtSenderDyn<G, D>, mut msg: &OtReceiverMsgDyn<G>) -> (Output<D>, Output<D>)
{
    let ry = G::scalar_mul(&msg.r, &state.y);
    let s_inv = G::neg(&state.s);
    let r_minus_s = G::add(&msg.r, &s_inv);
    let r_minus_s_y = G::scalar_mul(&r_minus_s, &state.y);
    let mut h0 = D::new();
    G::<D>::write_element(&ry, &mut h0);
    let mut h1 = D::new();
    G::<D>::write_element(&r_minus_s_y, &mut h1);
    (h0.finalize().to_vec(), h1.finalize().to_vec())
}

pub fn ot_recv_finish<G: Group, D: Digest>(mut state: &BaseOtReceiverDyn<G, D>) -> Output<D>
{
    let sx = G::scalar_mul(&state.s, &state.x);
    let mut h = D::new();
    G::<D>::write_element(&sx, &mut h);
    h.finalize().to_vec()
}

pub fn ot_recv_choice<G: Group, D: Digest>(mut state: &BaseOtReceiverDyn<G, D>) -> bool
{
    state.c
}

pub fn ot_send_payload<D: Digest>(mut k0: &Output<D>, mut k1: &Output<D>, mut m0: &[u8], mut m1: &[u8], mut e0: &mut [u8], mut e1: &mut [u8])
{
    for i in 0.. m0.len(){
    e0[i] = (m0[i] ^ k0[i]);
};
    for i in 0.. m1.len(){
    e1[i] = (m1[i] ^ k1[i]);
}
}

pub fn ot_recv_payload<D: Digest>(mut kc: &Output<D>, mut ec: &[u8], mut mc: &mut [u8])
{
    for i in 0.. ec.len(){
    mc[i] = (ec[i] ^ kc[i]);
}
}

pub fn toy_mul(mut a: u64, mut b: u64) -> u64
{
    ((a * b) % TOY_P)
}

pub fn toy_pow(mut base: u64, mut exp: u64) -> u64
{
    let mut acc: u64 = 1;
    let mut b = (base % TOY_P);
    while (exp > 0){
    if ((exp & 1) == 1){
    acc = toy_mul(acc, b);
};
    b = toy_mul(b, b);
    exp >>= 1;
};
    acc
}

pub fn mul_4x4(mut a: &[u64; 4], mut b: &[u64; 4]) -> [u64; 8]
{
    let mut r = [0; 8];
    for i in 0.. 4{
    let mut carry: u64 = 0;
    for j in 0.. 4{
    let v = (((r[(i + j)] as u128) + ((a[i] as u128) * (b[j] as u128))) + (carry as u128));
    r[(i + j)] = (v as u64);
    carry = ((v >> 64) as u64);
};
    r[(i + 4)] = carry;
};
    r
}

pub fn reduce_wide(mut t: &[u64; 8]) -> Fe25519
{
    let mut acc = [0; 5];
    let mut c: u128 = 0;
    for i in 0.. 4{
    let v = (((t[i] as u128) + ((t[(4 + i)] as u128) * 38)) + c);
    acc[i] = (v as u64);
    c = (v >> 64);
};
    acc[4] = (c as u64);
    let mut out = [0; 4];
    let mut c: u128 = ((acc[4] as u128) * 38);
    for i in 0.. 4{
    let v = ((acc[i] as u128) + c);
    out[i] = (v as u64);
    c = (v >> 64);
};
    if (c != 0){
    let mut c2: u128 = (c * 38);
    for i in 0.. 4{
    let v = ((out[i] as u128) + c2);
    out[i] = (v as u64);
    c2 = (v >> 64);
}
};
    fe_canonicalize(out)
}

pub fn fe_canonicalize(mut a: [u64; 4]) -> Fe25519
{
    let mut x = a;
    for _ in 0.. 2{
    let mut tmp = [0; 4];
    let mut borrow: u64 = 0;
    for i in 0.. 4{
    let (r1, b1) = x[i].overflowing_sub(P_LIMBS[i]);
    let (r2, b2) = r1.overflowing_sub(borrow);
    tmp[i] = r2;
    borrow = ((b1 as u64) | (b2 as u64));
};
    if (borrow == 0){
    x = tmp;
}
};
    Fe25519(x)
}

pub fn fe_add(mut a: &Fe25519, mut b: &Fe25519) -> Fe25519
{
    let mut r = [0; 4];
    let mut c: u64 = 0;
    for i in 0.. 4{
    let v = (((a.0[i] as u128) + (b.0[i] as u128)) + (c as u128));
    r[i] = (v as u64);
    c = ((v >> 64) as u64);
};
    if (c != 0){
    let mut c2: u128 = ((c as u128) * 38);
    for i in 0.. 4{
    let v = ((r[i] as u128) + c2);
    r[i] = (v as u64);
    c2 = (v >> 64);
}
};
    fe_canonicalize(r)
}

pub fn fe_sub(mut a: &Fe25519, mut b: &Fe25519) -> Fe25519
{
    let mut neg_b = [0; 4];
    let mut borrow: u64 = 0;
    for i in 0.. 4{
    let (r1, br1) = P_LIMBS[i].overflowing_sub(b.0[i]);
    let (r2, br2) = r1.overflowing_sub(borrow);
    neg_b[i] = r2;
    borrow = ((br1 as u64) | (br2 as u64));
};
    fe_add(a, &Fe25519(neg_b))
}

pub fn fe_neg(mut a: &Fe25519) -> Fe25519
{
    if a.is_zero(){
    Fe25519::ZERO
} else {
    let mut neg = [0; 4];
    let mut borrow: u64 = 0;
    for i in 0.. 4{
    let (r1, br1) = P_LIMBS[i].overflowing_sub(a.0[i]);
    let (r2, br2) = r1.overflowing_sub(borrow);
    neg[i] = r2;
    borrow = ((br1 as u64) | (br2 as u64));
};
    Fe25519(neg)
}
}

pub fn fe_mul(mut a: &Fe25519, mut b: &Fe25519) -> Fe25519
{
    let wide = mul_4x4(&a.0, &b.0);
    reduce_wide(&wide)
}

pub fn fe_sq(mut a: &Fe25519) -> Fe25519
{
    fe_mul(a, a)
}

pub fn fe_invert(mut a: &Fe25519) -> Fe25519
{
    let exp_limbs: [u64; 4] = vec![18446744073709551595, 18446744073709551615, 18446744073709551615, 9223372036854775807];
    let mut acc = Fe25519::ONE;
    for limb_idx in 0..4.rev(){
    for bit in 0..64.rev(){
    acc = fe_sq(&acc);
    let b = ((exp_limbs[limb_idx] >> bit) & 1);
    if (b == 1){
    acc = fe_mul(&acc, a);
}
}
};
    acc
}

pub fn fe_const(mut limbs: [u64; 4]) -> Fe25519
{
    Fe25519(limbs)
}

pub fn ed_add(mut p1: &EdPoint, mut p2: &EdPoint) -> EdPoint
{
    let a = fe_mul(&fe_sub(&p1.y, &p1.x), &fe_sub(&p2.y, &p2.x));
    let b = fe_mul(&fe_add(&p1.y, &p1.x), &fe_add(&p2.y, &p2.x));
    let c = fe_mul(&fe_mul(&p1.t, &D2), &p2.t);
    let d_ = fe_add(&fe_mul(&p1.z, &p2.z), &fe_mul(&p1.z, &p2.z));
    let e = fe_sub(&b, &a);
    let f = fe_sub(&d_, &c);
    let g = fe_add(&d_, &c);
    let h = fe_add(&b, &a);
    EdPoint { x: fe_mul(&e, &f), y: fe_mul(&g, &h), t: fe_mul(&e, &h), z: fe_mul(&f, &g) }
}

pub fn ed_double(mut p: &EdPoint) -> EdPoint
{
    let a = fe_sq(&p.x);
    let b = fe_sq(&p.y);
    let c = fe_add(&fe_sq(&p.z), &fe_sq(&p.z));
    let d_ = fe_neg(&a);
    let xy_sum = fe_add(&p.x, &p.y);
    let e = fe_sub(&fe_sub(&fe_sq(&xy_sum), &a), &b);
    let g = fe_add(&d_, &b);
    let f = fe_sub(&g, &c);
    let h = fe_sub(&d_, &b);
    EdPoint { x: fe_mul(&e, &f), y: fe_mul(&g, &h), t: fe_mul(&e, &h), z: fe_mul(&f, &g) }
}

pub fn ed_neg(mut p: &EdPoint) -> EdPoint
{
    EdPoint { x: fe_neg(&p.x), y: p.y, z: p.z, t: fe_neg(&p.t) }
}

pub fn ed_scalar_mul(mut p: &EdPoint, mut k: &[u8; 32]) -> EdPoint
{
    let mut acc = EdPoint::IDENTITY;
    for byte_idx in 0..32.rev(){
    for bit in 0..8.rev(){
    acc = ed_double(&acc);
    let b = ((k[byte_idx] >> bit) & 1);
    if (b == 1){
    acc = ed_add(&acc, p);
}
}
};
    acc
}

