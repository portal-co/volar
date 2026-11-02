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
pub struct CommitmentCoreDyn<D: Digest>(pub Vec<u8>, pub PhantomData<D>);

#[derive(Debug, Default)]
pub struct ViaDigestPuncturableRandomizerDyn<D: Digest> {
    pub digest: PhantomData<D>,
}

#[derive(Debug, Default)]
pub struct RoleKeys {
    pub enc: [[u8; BLOCK_LEN]; NK_ROUND_KEYS],
    pub mac: [[u8; BLOCK_LEN]; NK_ROUND_KEYS],
}

#[derive(Clone, Copy, Debug)]
pub struct PlanBoundary<'a> {
    pub plan: &BootstrapPlan,
    pub delta: u32,
}

#[derive(Debug, Default)]
pub struct CiphertextInputBufferDyn {
    pub n_lwe: usize,
    pub ciphertexts: Vec<BinfheLweCiphertextDyn>,
}

#[derive(Clone, Debug)]
pub struct PrivateKeySwitchingKeyDyn {
    pub big_n: usize,
    pub priv_ell: usize,
    pub a_col: Vec<[BinfheRlweCiphertextDyn; PRIV_ELL]>,
    pub b_col: Vec<[BinfheRlweCiphertextDyn; PRIV_ELL]>,
    pub a_body: [BinfheRlweCiphertextDyn; PRIV_ELL],
    pub b_body: [BinfheRlweCiphertextDyn; PRIV_ELL],
}

#[derive(Clone, Debug)]
pub struct CircuitBootstrappingKeyDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub bs_ell: usize,
    pub ks_ell: usize,
    pub priv_ell: usize,
    pub bk: BinfheBootstrappingKeyDyn,
    pub privksk: PrivateKeySwitchingKeyDyn,
}

#[derive(Clone, Copy, Debug)]
pub struct BinfheKeySwitchingKeyRefDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub ks_ell: usize,
    pub ksk: Vec<[BinfheLweCiphertextDyn; KS_ELL]>,
}

#[derive(Clone, Copy, Debug)]
pub struct BinfheBootstrappingKeyRefDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub bs_ell: usize,
    pub ks_ell: usize,
    pub bsk: Vec<BinfheRgswCiphertextDyn>,
    pub ksk: BinfheKeySwitchingKeyRefDyn,
}

#[derive(Clone, Debug)]
pub struct BinfheKeySwitchingKeyDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub ks_ell: usize,
    pub ksk: Vec<[BinfheLweCiphertextDyn; KS_ELL]>,
}

#[derive(Clone, Debug)]
pub struct BinfheBootstrappingKeyDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub bs_ell: usize,
    pub ks_ell: usize,
    pub bsk: Vec<BinfheRgswCiphertextDyn>,
    pub ksk: BinfheKeySwitchingKeyDyn,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LutDyn {
    pub addr_bits: usize,
    pub table_len: usize,
    pub big_n: usize,
    pub log_q: usize,
    pub log_q_lwe: usize,
    pub k_max: usize,
    pub logical: [bool; TABLE_LEN],
    pub test_poly: [u32; BIG_N],
    pub is_constant: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinfheLweSecretKeyDyn {
    pub n: usize,
    pub key: [u8; N],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BinfheLweCiphertextDyn {
    pub n: usize,
    pub a: [u32; N],
    pub b: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PlanWorkspaceRequirements {
    pub wires: usize,
    pub rgsws: usize,
    pub cells: usize,
    pub lut_inputs: usize,
}

#[derive(Debug, Default)]
pub struct PlanWorkspaceDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub bs_ell: usize,
    pub wires: Vec<BinfheLweCiphertextDyn>,
    pub rgsws: Vec<BinfheRgswCiphertextDyn>,
    pub cells: Vec<BinfheRlweCiphertextDyn>,
    pub lut_inputs: Vec<BinfheLweCiphertextDyn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LutSpec {
    pub entries: Vec<bool>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FailureBudget {
    pub per_bootstrap_log2: u32,
    pub total_log2: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BootstrapPlan {
    pub profile: ProfileId,
    pub k_max: u32,
    pub luts: Vec<LutSpec>,
    pub layers: Vec<Vec<PlanOp>>,
    pub num_inputs: u32,
    pub num_cells: u32,
    pub outputs: Vec<WireId>,
    pub cell_outputs: Vec<CellId>,
    pub budget: FailureBudget,
}

#[derive(Debug, Default)]
pub struct Reader<'a> {
    pub bytes: &[u8],
    pub offset: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct BinfheRgswRowDyn {
    pub n: usize,
    pub rlwe0: BinfheRlweCiphertextDyn,
    pub rlwe1: BinfheRlweCiphertextDyn,
}

#[derive(Clone, Debug)]
pub struct BinfheRgswCiphertextDyn {
    pub n: usize,
    pub ell: usize,
    pub rows: [BinfheRgswRowDyn; ELL],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinfheRlweSecretKeyDyn {
    pub n: usize,
    pub key: [u32; N],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BinfheRlweCiphertextDyn {
    pub n: usize,
    pub a: [u32; N],
    pub b: [u32; N],
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct WireState {
    pub a0: u64,
    pub pi: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GlobalDelta {
    pub delta: u64,
}

#[derive(Clone, Debug)]
pub struct ExpandedWire {
    pub ct_a0: Ciphertext,
    pub ct_pi: Ciphertext,
    pub slot_a0: usize,
    pub slot_pi: usize,
}

#[derive(Clone, Debug)]
pub struct ExpandedGate {
    pub a: ExpandedWire,
    pub b: ExpandedWire,
    pub out: ExpandedWire,
}

#[derive(Clone, Debug)]
pub struct Circuit {
    pub inputs: usize,
    pub gates: Vec<Gate>,
    pub outputs: Vec<usize>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ReferenceEquation {
    pub xor_charged: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LatticeRowCt {
    pub u1: RqPoly,
    pub u2: RqPoly,
}

#[derive(Clone, Debug)]
pub struct LatticeDd {
    pub n: usize,
    pub p: u64,
    pub q: u64,
    pub delta: RpPoly,
    pub delta_pinned: Delta,
    pub triple: (RqPoly, RqPoly, RqPoly),
    pub triple_rp: (RpPoly, RpPoly, RpPoly),
    pub ntt_q: Option<Ntt>,
    pub ntt_p: Option<Ntt>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rns2 {
    pub l0: Limb,
    pub l1: Limb,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RnsRowCt {
    pub u1: Rns2,
    pub u2: Rns2,
}

#[derive(Clone, Debug)]
pub struct LatticeRnsDd {
    pub n: usize,
    pub p: u64,
    pub q0: u64,
    pub q1: u64,
    pub delta: LabelPoly,
    pub delta_pinned: Delta,
    pub triple: (Rns2, Rns2, Rns2),
    pub triple_rp: (LabelPoly, LabelPoly, LabelPoly),
    pub ntt0: Option<Ntt>,
    pub ntt1: Option<Ntt>,
    pub ntt_p: Option<Ntt>,
}

#[derive(Clone, Debug)]
pub struct DeltaLoweredDyn {
    pub n: usize,
    pub coeffs: LabelPoly,
    pub p: u64,
}

#[derive(Clone, Debug)]
pub struct WireLabel {
    pub w0: LabelPoly,
    pub pi: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RowCt {
    pub c0: LabelPoly,
    pub c1: LabelPoly,
}

#[derive(Clone, Debug)]
pub struct ReferenceDd {
    pub p: u64,
    pub delta: Delta,
    pub triple: (LabelPoly, LabelPoly, LabelPoly),
}

#[derive(Clone, Debug)]
pub struct PrfKey(pub [u64; 4]);

#[derive(Clone, Debug)]
pub struct PinnedGarbling {
    pub labels: Vec<WireLabel>,
    pub delta_ct: RowCt,
    pub and_rows: Vec<AndRows>,
    pub records: Vec<GateRecord>,
}

#[derive(Clone, Copy, Debug)]
pub struct ReferencePrg {
    pub slots: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExpansionPlan {
    pub seed_ciphertexts: usize,
    pub slots_per_seed: usize,
    pub depth: usize,
    pub required_bits: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Parameters {
    pub degree: usize,
    pub modulus: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PolyLoweredDyn {
    pub n: usize,
    pub slots: Vec<u64>,
}

#[derive(Clone, Debug)]
pub struct Ring {
    pub parameters: Parameters,
    pub ntt: Ntt,
}

#[derive(Clone, Debug)]
pub struct Ntt {
    pub modulus: u64,
    pub degree: usize,
    pub psi_powers: Vec<u64>,
    pub inv_psi_powers_scaled: Vec<u64>,
    pub fwd_twiddles: Vec<u64>,
    pub inv_twiddles: Vec<u64>,
}

#[derive(Clone, Debug)]
pub struct OfflineGarbler {
    pub secret: SecretKey,
    pub ksks: KeySwitchKeySet,
    pub plan: ExpansionPlan,
}

#[derive(Clone, Debug)]
pub struct OfflineEvaluator {
    pub plan: ExpansionPlan,
    pub wires: Vec<ExpandedWire>,
}

#[derive(Clone, Debug)]
pub struct ExpansionStep {
    pub wire_state: WireState,
    pub eval: ExpandedWire,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ZeroNoise {
}

#[derive(Clone, Debug)]
pub struct Parameters {
    pub degree: usize,
    pub plaintext_modulus: u64,
    pub chain: Vec<u64>,
    pub noise_bound: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RnsPoly {
    pub limbs: Vec<Poly>,
}

#[derive(Clone, Debug)]
pub struct SecretKey {
    pub coeffs: Vec<i64>,
}

#[derive(Clone, Debug)]
pub struct KeySwitchKey {
    pub level: usize,
    pub base: u64,
    pub parts: Vec<(RnsPoly, RnsPoly)>,
}

#[derive(Clone, Debug)]
pub struct KeySwitchKeySet {
    pub keys: Vec<KeySwitchKey>,
    pub base: u64,
}

#[derive(Clone, Debug)]
pub struct Ciphertext {
    pub level: usize,
    pub parts: Vec<RnsPoly>,
}

#[derive(Clone, Debug)]
pub struct Context {
    pub parameters: Parameters,
    pub level_rings: Vec<Ring>,
    pub plaintext_ring: Ring,
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
pub struct RoLeafCommitDyn<D: Digest> {
    pub _d: PhantomData<D>,
}

#[derive(Debug, Default)]
pub struct EmLeafCommit {
}

#[derive(Debug, Default)]
pub struct AesCtrLengthDoubler {
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
pub struct FaestTranscript {
    pub sponge: Sponge,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Gf2_8 {
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Gf2_64 {
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Gf2_128 {
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Gf2_256 {
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Fp3 {
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

#[derive(Clone, Debug)]
pub struct SpecSponge {
    pub state: [u8; 200],
    pub rate: usize,
    pub pos: usize,
}

#[derive(Clone, Debug)]
pub struct SpecXofReader {
    pub state: [u8; 200],
    pub rate: usize,
    pub pos: usize,
}

#[derive(Debug, Default)]
pub struct LweSampleDyn<T, U> {
    pub n: usize,
    pub m: usize,
    pub matrix: Vec<Vec<T>>,
    pub b: Vec<U>,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NttParams {
    pub degree: usize,
    pub modulus: u64,
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

#[derive(Clone)]
pub struct OtConsistencyProofDyn<S> {
    pub e0: S,
    pub e1: S,
    pub z0: S,
    pub z1: S,
}

#[derive(Debug, Default)]
pub struct ChouOrlandiDyn<G, D> {
    pub _g: PhantomData<G>,
    pub _d: PhantomData<D>,
}

#[derive(Debug, Default)]
pub struct ChouOrlandiRecvDyn<G: Group, D: Digest> {
    pub inner: BaseOtReceiverDyn<G, D>,
}

#[derive(Clone)]
pub struct FerretIterMsg {
    pub lpn_seed: [u8; 16],
    pub choices: Vec<bool>,
    pub mpcot: MpcotRegSenderMsg,
}

#[derive(Clone)]
pub struct FerretSenderSeed {
    pub delta: Block,
    pub q: Vec<Block>,
}

#[derive(Clone)]
pub struct FerretReceiverSeed {
    pub u: Vec<bool>,
    pub w: Vec<Block>,
}

#[derive(Debug, Default)]
pub struct FerretExtendOut {
    pub sender_out: Vec<Block>,
    pub recv_x: Vec<bool>,
    pub recv_z: Vec<Block>,
    pub sender_seed: FerretSenderSeed,
    pub receiver_seed: FerretReceiverSeed,
}

#[derive(Clone)]
pub struct FerretPrep {
    pub alphas: Vec<usize>,
    pub e: Vec<bool>,
    pub lpn_seed: [u8; 16],
    pub choices: Vec<bool>,
}

#[derive(Clone)]
pub struct MpcotRegSenderMsg {
    pub blocks: Vec<SpcotSenderMsg>,
}

#[derive(Clone)]
pub struct MpcotUniSenderMsg {
    pub hash_seed: [u8; 16],
    pub blocks: Vec<SpcotSenderMsg>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FerretParams {
    pub n: usize,
    pub k: usize,
    pub t: usize,
}

#[derive(Debug, Default)]
pub struct CotPoolSender {
    pub params: FerretParams,
    pub seed: FerretSenderSeed,
    pub out: VecDeque<Block>,
    pub raise_n: Option<usize>,
    pub malicious: bool,
    pub refill_count: u64,
}

#[derive(Debug, Default)]
pub struct CotPoolReceiver {
    pub params: FerretParams,
    pub seed: FerretReceiverSeed,
    pub out_x: VecDeque<bool>,
    pub out_z: VecDeque<Block>,
    pub malicious: bool,
}

#[derive(Clone)]
pub struct SpcotSenderMsg {
    pub ms: Vec<[Block; 2]>,
    pub c: Block,
    pub hash_v: Vec<u8>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ToyElement(pub u64);

#[derive(Debug, Default)]
pub struct ToyGroup {
}

#[derive(Debug, Default)]
pub struct IdealCotDyn<T> {
    pub n: usize,
    pub delta: DeltaLoweredDyn<T>,
}

#[derive(Clone)]
pub struct IknpUMsg {
    pub u_cols: Vec<Vec<bool>>,
}

#[derive(Clone)]
pub struct LweOtCrsDyn {
    pub n: usize,
    pub a: [[Zq; N]; N],
    pub h: [Zq; N],
}

#[derive(Debug, Default)]
pub struct LweOtReceiverDyn {
    pub n: usize,
    pub s: [Zq; N],
    pub c: bool,
}

#[derive(Clone)]
pub struct LweOtRecvMsgDyn {
    pub n: usize,
    pub pk0: [Zq; N],
}

#[derive(Debug, Default)]
pub struct LweOtSenderMsgLoweredDyn {
    pub n: usize,
    pub l: usize,
    pub u0: [Zq; N],
    pub v0: [Zq; L],
    pub u1: [Zq; N],
    pub v1: [Zq; L],
}

#[derive(Debug, Default)]
pub struct LweOtSenderMsgSecureLoweredDyn {
    pub n: usize,
    pub l: usize,
    pub u0: [[Zq; N]; L],
    pub v0: [Zq; L],
    pub u1: [[Zq; N]; L],
    pub v1: [Zq; L],
}

#[derive(Clone)]
pub struct LweOtSenderMsgDyn {
    pub u0: Vec<Zq>,
    pub v0: Vec<Zq>,
    pub u1: Vec<Zq>,
    pub v1: Vec<Zq>,
}

#[derive(Debug, Default)]
pub struct LweBaseOtDyn(pub usize, pub PhantomData<[(); N]>);

#[derive(Debug, Default)]
pub struct LweOtSenderMsgSecureDyn {
    pub u0: Vec<Vec<Zq>>,
    pub v0: Vec<Zq>,
    pub u1: Vec<Vec<Zq>>,
    pub v1: Vec<Zq>,
}

#[derive(Debug, Default)]
pub struct LweBaseOtSecureDyn(pub usize, pub PhantomData<[(); N]>);

#[derive(Debug, Default)]
pub struct SoftSpokenOutLoweredDyn<D: Digest> {
    pub m: usize,
    pub l: usize,
    pub sender_r0: [[u8; L]; M],
    pub receiver_v: [[u8; L]; M],
    pub sender_tag: Output<D>,
    pub receiver_tag: Output<D>,
}

#[derive(Debug, Default)]
pub struct SoftSpokenOutDynDyn<D: Digest> {
    pub l: usize,
    pub sender_r0: Vec<[u8; L]>,
    pub receiver_v: Vec<[u8; L]>,
    pub sender_tag: Output<D>,
    pub receiver_tag: Output<D>,
}

#[derive(Debug, Default)]
pub struct OtStack {
    pub sender: CotPoolSender,
    pub receiver: CotPoolReceiver,
}

#[derive(Debug, Default)]
pub struct SenderLpn {
    pub seed_q: Vec<[u8; 16]>,
    pub emit: VecDeque<[u8; 16]>,
}

#[derive(Debug, Default)]
pub struct RecvLpn {
    pub seed: FerretReceiverSeed,
    pub x: Vec<bool>,
    pub z: Vec<[u8; 16]>,
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
    pub ks_bg_log: usize,
    pub ksk: [[LweCiphertextDyn; KS_ELL]; BIG_N],
}

#[derive(Clone, Debug)]
pub struct BootstrappingKeyDyn {
    pub n_lwe: usize,
    pub big_n: usize,
    pub bs_ell: usize,
    pub ks_ell: usize,
    pub bs_bg_log: usize,
    pub ks_bg_log: usize,
    pub bsk: [RgswCiphertextDyn; N_LWE],
    pub ksk: KeySwitchingKeyDyn,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TfheBootstrapTableDyn {
    pub addr_bits: usize,
    pub table_len: usize,
    pub big_n: usize,
    pub logical: [bool; TABLE_LEN],
    pub test_poly: [u32; BIG_N],
    pub is_constant: bool,
}

#[derive(Debug, Default)]
pub struct GateCertificate {
    pub name: &str,
    pub arity: usize,
    pub prepare: _,
    pub interval_true: (u64, u64),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FrameBinding {
    pub parameter_fingerprint: [u8; 32],
    pub session_id: [u8; 32],
    pub manifest_digest: [u8; 32],
    pub use_counter: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Frame {
    pub stage: Stage,
    pub binding: FrameBinding,
    pub payload: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PaperProfile {
    pub security_bits: usize,
    pub ring_degree: usize,
    pub modulus_bits: usize,
    pub batch_messages: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EncodedLabelBatch {
    pub differences: Vec<u64>,
    pub zeroes: Vec<u64>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PaddedLabelBatch {
    pub label_count: usize,
    pub differences: Vec<u64>,
    pub zeroes: Vec<u64>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LabelPairDyn {
    pub n: usize,
    pub zero: [u8; N],
    pub one: [u8; N],
}

#[derive(Debug, Default)]
pub struct LabelBatchDyn {
    pub n: usize,
    pub pairs: Vec<LabelPairDyn>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Parameters {
    pub degree: usize,
    pub width: usize,
    pub plaintext_modulus: u64,
    pub delta: u64,
    pub gadget_base: u64,
    pub gadget_digits: usize,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ZeroNoise {
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Polynomial {
    pub first: Vec<u64>,
    pub second: Vec<u64>,
}

#[derive(Clone, Debug)]
pub struct PublicParameters {
    pub parameters: Parameters,
    pub lhe_a: Vec<Polynomial>,
    pub lenc_b: Vec<Polynomial>,
}

#[derive(Clone, Debug)]
pub struct FirstCiphertext {
    pub lhe_state: Vec<Polynomial>,
    pub lhe_ciphertext: Vec<Polynomial>,
    pub lenc_ciphertext: Vec<Polynomial>,
}

#[derive(Clone, Debug)]
pub struct SecondCiphertext {
    pub lhe_state: Polynomial,
    pub lhe_ciphertext: Vec<Polynomial>,
}

#[derive(Clone, Debug)]
pub struct SelectionKey {
    pub key: Polynomial,
}

#[derive(Clone, Debug)]
pub struct BatchSelect {
    pub ring: Ring,
    pub public: PublicParameters,
}

#[derive(Clone, Debug)]
pub struct Tree {
    pub tree: Vec<Polynomial>,
    pub digest: Polynomial,
}

#[derive(Clone, Debug)]
pub struct Ring {
    pub parameters: Parameters,
    pub first_ntt: Ntt,
    pub second_ntt: Ntt,
    pub inverse_plaintext_mod_delta: u64,
    pub inverse_delta_mod_plaintext: u64,
}

#[derive(Clone, Debug)]
pub struct Ntt {
    pub modulus: u64,
    pub degree: usize,
    pub psi: u64,
    pub omega: u64,
    pub inverse_psi: u64,
    pub inverse_omega: u64,
    pub inverse_degree: u64,
}

#[derive(Debug, Default)]
pub struct BitVoleDyn<T> {
    pub n: usize,
    pub u: Vec<Bit>,
    pub v: Vec<T>,
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
pub struct PolyLoweredDyn<T> {
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
pub struct VopeDyn<T> {
    pub n: usize,
    pub k: usize,
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
}

#[derive(Debug, Default)]
pub struct DeltaLoweredDyn<T> {
    pub n: usize,
    pub delta: Vec<T>,
}

#[derive(Debug, Default)]
pub struct QDyn<T> {
    pub n: usize,
    pub q: Vec<T>,
}

#[derive(Clone)]
pub struct WarpTranscript {
    pub sponge: SpecSponge,
}

#[derive(Clone, Debug)]
pub struct WarpMerkleTreeDyn {
    pub nodes: usize,
    pub leaves: usize,
    pub nodes: Vec<WarpDigest>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpAuthPathDyn {
    pub log2_leaves: usize,
    pub siblings: Vec<WarpDigest>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpTermDyn {
    pub deg: usize,
    pub coeff: Galois128,
    pub vars: Vec<usize>,
    pub n_vars: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpConstraintDyn {
    pub terms: usize,
    pub deg: usize,
    pub terms: Vec<WarpTermDyn>,
    pub n_terms: u32,
}

#[derive(Clone, Debug)]
pub struct WarpIndexDyn {
    pub m: usize,
    pub terms: usize,
    pub deg: usize,
    pub constraints: Vec<WarpConstraintDyn>,
    pub public_len: usize,
    pub witness_len: usize,
    pub degree_bound: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpWitnessDyn {
    pub k: usize,
    pub x: usize,
    pub public: Vec<Galois128>,
    pub witness: Vec<Galois128>,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct WarpIdentityCode {
}

#[derive(Clone, Copy, Debug, Default)]
pub struct WarpRepetitionCode {
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpUniDyn {
    pub cap: usize,
    pub evaluations: Vec<Galois128>,
    pub count: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WarpAccInstanceDyn {
    pub logn: usize,
    pub beta: usize,
    pub root: WarpDigest,
    pub alpha: Vec<Galois128>,
    pub mu: Galois128,
    pub beta: Vec<Galois128>,
    pub eta: Galois128,
}

#[derive(Clone, Debug)]
pub struct WarpAccWitnessDyn {
    pub n: usize,
    pub k: usize,
    pub nodes: usize,
    pub tree: WarpMerkleTreeDyn,
    pub codeword: Vec<Galois128>,
    pub witness: Vec<Galois128>,
}

#[derive(Clone, Debug)]
pub struct WarpAccDyn {
    pub n: usize,
    pub k: usize,
    pub logn: usize,
    pub beta: usize,
    pub nodes: usize,
    pub instance: WarpAccInstanceDyn,
    pub witness: WarpAccWitnessDyn,
}

#[derive(Clone, Debug)]
pub struct WarpProofDyn {
    pub fresh: usize,
    pub ell: usize,
    pub a: usize,
    pub s: usize,
    pub shifts: usize,
    pub sc: usize,
    pub logn: usize,
    pub initial_root: WarpDigest,
    pub fresh_mus: Vec<Galois128>,
    pub first_sumcheck: Vec<WarpUniDyn>,
    pub nu0: Galois128,
    pub ood_answers: Vec<Galois128>,
    pub second_sumcheck: Vec<WarpUniDyn>,
    pub initial_paths: Vec<WarpAuthPathDyn>,
    pub accumulator_paths: Vec<Vec<WarpAuthPathDyn>>,
    pub openings: Vec<Vec<Galois128>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoundaryError {
    InvalidPlan(PlanError),
    InvalidWireEncoding {
        k_max: u32,
        log_q_lwe: u32,
    },
    InputCount {
        expected: usize,
        actual: usize,
    },
    CiphertextInputCount {
        expected: usize,
        actual: usize,
    },
    InputBufferCapacity {
        required: usize,
        actual: usize,
    },
    WireArenaTooShort {
        required: usize,
        actual: usize,
    },
    CellCount {
        expected: usize,
        actual: usize,
    },
    Workspace(PlanWorkspaceError),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LutError {
    AddressShapeInvalid,
    ArityExceedsCircuitMax,
    ShapeUnsupported,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanWorkspaceError {
    InvalidPlan(PlanError),
    CapacityOverflow,
    InputCount {
        expected: usize,
        actual: usize,
    },
    CellCount {
        expected: usize,
        actual: usize,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanOp {
    Const {
        out: WireId,
        value: bool,
    },
    Not {
        input: WireId,
        out: WireId,
    },
    Lut {
        inputs: Vec<u32>,
        table: LutId,
        out: WireId,
    },
    CircuitBootstrap {
        input: WireId,
        out: RgswId,
    },
    RgswMux {
        sel: RgswId,
        then_cell: CellId,
        else_cell: CellId,
        out: CellId,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProfileId {
    Toy,
    ToyNoisy,
    Std128,
    Custom,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanError {
    BadTableShape {
        table: LutId,
    },
    ArityExceedsKMax {
        table: LutId,
    },
    BadReference,
    BadOutput,
    BudgetInconsistent,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EncodeError {
    InvalidPlan(PlanError),
    TooLarge,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DecodeError {
    BadMagic,
    UnsupportedVersion,
    UnknownTag,
    Truncated,
    TooLarge,
    TrailingBytes,
    InvalidPlan(PlanError),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Stitch {
    OneBit(bool),
    FiveBits(u8),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StitchError {
    Exhausted,
    WrongVariant,
    NonCanonicalBit,
    PayloadOverflow,
    Crypto(SwheError),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Gate {
    And {
        a: usize,
        b: usize,
    },
    Xor {
        a: usize,
        b: usize,
    },
    Not {
        a: usize,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LatticeError {
    LengthMismatch,
    InvalidParameters,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RnsError {
    LengthMismatch,
    InvalidParameters,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Variant {
    Small,
    Fast,
    Tiny,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GateRecord {
    SmallAnd(bool),
    SmallXor(bool),
    FastAnd {
        masks: [bool; 4],
        det: bool,
    },
    FastXor,
}

#[derive(Clone, Debug)]
pub enum AndRows {
    Small([RowCt; 3]),
    Fast([RowCt; 4]),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DriverError {
    BadCircuit,
    BadInputCount,
    MissingRecord,
    WrongRecord,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    InvalidParameters,
    LengthMismatch,
    Arithmetic,
    Randomness,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    InvalidParameters,
    LengthMismatch,
    LevelMismatch,
    Noise,
    NoiseSource,
    Randomness,
    Arithmetic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CirrusHashBackend {
    Blake2s,
    Ascon,
    Keccak,
}

#[derive(Debug)]
pub enum Sponge {
    Shake128(Shake128),
    Shake256(Shake256),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GramOutput {
    Cleartext,
    Regarble,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PoolSeedError {
    InvalidParameters,
    SenderSeedCount {
        expected: usize,
        found: usize,
    },
    ReceiverChoiceCount {
        expected: usize,
        found: usize,
    },
    ReceiverSeedCount {
        expected: usize,
        found: usize,
    },
    InsufficientCotMaterial {
        required: usize,
        available: usize,
    },
    ReceiverOutputCountMismatch {
        choices: usize,
        values: usize,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TfheBootstrapTableError {
    AddressWidthOutOfRange,
    TableLengthMismatch,
    RingCapacityExceeded,
    NegacyclicIncompatible,
    InputEncodingUnsupported,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Stage {
    PublicParameters,
    ReusableCiphertext,
    PerUseCiphertext,
    SelectionKey,
    Complete,
    Error,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FrameError {
    Truncated,
    UnsupportedFormat,
    UnknownStage,
    PayloadTooLarge,
    LengthMismatch,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LabelEncodingError {
    FieldElementOutOfRange {
        index: usize,
    },
    NonCanonicalElement {
        index: usize,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LabelBatchPaddingError {
    TooFewSlots {
        slots: usize,
        used: usize,
    },
    ChoiceLengthMismatch {
        expected: usize,
        actual: usize,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LabelBatchDecodeError {
    LengthMismatch,
    NonCanonicalLabel(LabelEncodingError),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BatchError {
    EvenOffset,
    MismatchedPair {
        index: usize,
    },
    LengthMismatch,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    InvalidParameters,
    LengthMismatch,
    NonCanonicalPlaintext,
    Randomness,
    Noise,
    Arithmetic,
}

pub trait Invert {
    fn invert(&self) -> Self;
}

pub trait PuncturableLengthDoubler: LengthDoubler {
}

pub trait AsKeySwitchingKey {
    fn ksk_rows(&self) -> &[[BinfheLweCiphertextDyn; KS_ELL]];
}

pub trait AsBinfheBootstrappingKey {
    fn bsk_rows(&self) -> &[BinfheRgswCiphertextDyn];
    fn ksk_ref(&self) -> BinfheKeySwitchingKeyRefDyn;
}

pub trait GateEquation {
    fn garble_and(&self, gate_index: usize, a: &WireState, b: &WireState, out: &WireState) -> Stitch;
    fn eval_and(&self, ctx: &Context, la: &Ciphertext, lb: &Ciphertext, expanded: &ExpandedGate, delta: &GlobalDelta, ksk: &KeySwitchKeySet, stitch: &Stitch) -> Result<Ciphertext, StitchError>;
    fn garble_xor(&self, p: u64, a: &WireState, b: &WireState, out: &WireState) -> Option<Stitch>;
    fn eval_xor(&self, ctx: &Context, la: &Ciphertext, lb: &Ciphertext, expanded: &ExpandedGate, delta: &GlobalDelta, ksk: &KeySwitchKeySet, stitch: Option<&Stitch>) -> Result<Ciphertext, StitchError>;
    fn xor_is_charged(&self) -> bool;
}

pub trait DdGadget {
    fn p(&self) -> u64;
    fn delta(&self) -> &Delta;
    fn row(&self, t: i64, random: &mut impl SpecRng) -> RowCt;
    fn dd0(&self, a: &LabelPoly, tau: &RowCt) -> LabelPoly;
    fn dd_i(&self, a: &LabelPoly, i: bool, tau: &RowCt) -> LabelPoly;
    fn dd_ij(&self, a: &LabelPoly, i: bool, b: &LabelPoly, j: bool, tau: &RowCt) -> LabelPoly;
    fn eval(&self, a: &LabelPoly, i: bool, b: &LabelPoly, j: bool, taus: &[RowCt; 3]) -> LabelPoly;
}

pub trait ExpansionPrg {
    fn depth(&self) -> usize;
    fn slots(&self) -> usize;
    fn expand(&self, ctx: &Context, seed: &Ciphertext, ksk: &KeySwitchKeySet) -> Result<Vec<Ciphertext>, Error>;
}

pub trait RandomSource {
    fn fill_bytes(&mut self, output: &mut [u8]) -> Result<(), Error>;
}

pub trait NoiseSource {
    fn sample(&mut self, bound: u64, output: &mut [i64]) -> Result<(), Error>;
}

pub trait PartyIndex {
    fn party_index(requested: usize) -> usize;
}

pub trait LeafCommit {
    fn commit(r: &[u8; SD_BYTES], iv: &[u8; 16], tweak: u32) -> ([u8; SD_BYTES], [u8; COM_BYTES]);
}

pub trait FaestAesProver {
    fn prove_aes_witness(&self, big_vole: &BigVoleProver, hash_key: &UniversalHashKey) -> QuickSilverProof;
    fn verify_aes_proof(&self, _proof: &QuickSilverProof, _hash_key: &UniversalHashKey, _q_vec: &[u8], _delta: &[u8]) -> bool;
}

pub trait ExtFieldMarker: Copy + core::fmt::Debug {
}

pub trait PrimeFieldMarker: Copy + core::fmt::Debug {
}

pub trait SpecRng {
    fn next_u32(&mut self) -> u32;
    fn next_u8(&mut self) -> u8;
}

pub trait BaseOt {
    type SenderState;
    type ReceiverState;
    type SetupMsg: Clone;
    type RecvMsg;
    type PayloadMsg;
    fn sender_setup<R: SpecRng>(rng: &mut R) -> (Self::SenderState, Self::SetupMsg);
    fn recv_start<R: SpecRng>(rng: &mut R, setup: &Self::SetupMsg, c: bool) -> (Self::ReceiverState, Self::RecvMsg);
    fn sender_payload<R: SpecRng>(rng: &mut R, state: &Self::SenderState, recv_msg: &Self::RecvMsg, m0: &[u8; L], m1: &[u8; L]) -> Self::PayloadMsg;
    fn recv_finish(state: &Self::ReceiverState, payload: &Self::PayloadMsg) -> [u8; L];
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

pub trait ScalarOps: Group {
    fn scalar_zero() -> Self::Scalar;
    fn scalar_add(a: &Self::Scalar, b: &Self::Scalar) -> Self::Scalar;
    fn scalar_sub(a: &Self::Scalar, b: &Self::Scalar) -> Self::Scalar;
    fn scalar_mul_scalar(a: &Self::Scalar, b: &Self::Scalar) -> Self::Scalar;
    fn scalar_eq(a: &Self::Scalar, b: &Self::Scalar) -> bool;
    fn scalar_from_hash<D: Digest>(h: D) -> Self::Scalar;
    fn random_mod_order<R: SpecRng>(rng: &mut R) -> Self::Scalar;
}

pub trait StackIo {
    fn send(&mut self, tag: u8, payload: &[u8]);
    fn recv(&mut self, expected_tag: u8) -> Vec<u8>;
}

pub trait RandomSource {
    fn fill_bytes(&mut self, output: &mut [u8]) -> Result<(), Error>;
}

pub trait NoiseSource {
    fn sample(&mut self, standard_deviation: u64, bound: u64, output: &mut [i64]) -> Result<(), Error>;
}

pub trait MemoryHasher<T> {
    type State: Clone;
    fn new_state() -> Self::State;
    fn absorb(state: &mut Self::State, encoded: T);
    fn finalize_eq(produce: &Self::State, consume: &Self::State) -> bool;
}

pub trait CotSource<T> {
    fn cot<R: SpecRng>(&mut self, rng: &mut R, sample_t: impl Fn, bit: bool) -> (Vec<T>, Vec<T>);
}

pub trait WarpCode {
    fn encode(&self, message: &Vec<Galois128>) -> Vec<Galois128>;
}

impl  U256 {
    pub fn bit(&self, mut n: u32) -> bool
    {
        let word = (n / 64) as usize;
        let bit = n % 64;
        if word < 4{
    ((self.0[word] >> bit) & 1) != 0
} else {
    false
}
    }
    pub fn high_bit(&self) -> bool
    {
        (self.0[3] >> 63) != 0
    }
    pub fn shl1(&self) -> Self
    {
        let mut out = [(0 as u64); 4];
        out[0] = (self.0[0] << 1);
        out[1] = ((self.0[1] << 1) | (self.0[0] >> 63));
        out[2] = ((self.0[2] << 1) | (self.0[1] >> 63));
        out[3] = ((self.0[3] << 1) | (self.0[2] >> 63));
        U256(out)
    }
    pub fn shr1(&self) -> Self
    {
        let mut out = [(0 as u64); 4];
        out[0] = ((self.0[0] >> 1) | (self.0[1] << 63));
        out[1] = ((self.0[1] >> 1) | (self.0[2] << 63));
        out[2] = ((self.0[2] >> 1) | (self.0[3] << 63));
        out[3] = (self.0[3] >> 1);
        U256(out)
    }
    pub fn xor(&self, mut other: &Self) -> Self
    {
        U256([(self.0[0] ^ other.0[0]), (self.0[1] ^ other.0[1]), (self.0[2] ^ other.0[2]), (self.0[3] ^ other.0[3])])
    }
    pub fn is_zero(&self) -> bool
    {
        (self.0[0] == 0) && (self.0[1] == 0) && (self.0[2] == 0) && (self.0[3] == 0)
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
        Galois64((self.0 ^ ((rhs as u64) * (72340172838076673 as u64))))
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
        BitsInBytes64((self.0 ^ ((rhs as u64) * (72340172838076673 as u64))))
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
        let s = a + b;
        if s >= 3{
    s - 3
} else {
    s
}
    }
    pub fn neg3(mut a: u8) -> u8
    {
        if a == 0{
    0
} else {
    3 - a
}
    }
    pub fn mul3(mut a: u8, mut b: u8) -> u8
    {
        let p = a * b;
        if p >= 3{
    p - 3
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
        self.0.as_slice() == other.0.as_slice()
    }
}

impl <D: Digest> Eq for CommitmentCoreDyn<D> {
}

impl <D: Digest> CommitmentCoreDyn<D> {
    pub fn validate(&self, mut opened_message: &impl AsRef<[u8]>, mut opened_rand: &impl AsRef<[u8]>) -> bool
    {
        let recomputed: CommitmentCoreDyn<D> = commit::<D>(opened_message, opened_rand);
        &recomputed.0 == &self.0
    }
}

impl <D: Digest> LengthDoubler for ViaDigestPuncturableRandomizerDyn<D> {
    type OutputSize = <D as _>::OutputSize;
    fn double(mut a: Vec<u8>) -> [Vec<u8>; 2]
    {
        let v = D::digest(&a);
        [v.clone(), (0..<<D>::OutputSize as Unsigned>::to_usize()).map(|i| (v[i] ^ a[i])).collect::<Vec<_>>()]
    }
}

impl <D: Digest> PuncturableLengthDoubler for ViaDigestPuncturableRandomizerDyn<D> {
}

impl  CiphertextInputBufferDyn {
    pub fn as_slice(&self) -> &[BinfheLweCiphertextDyn]
    {
        let n_lwe: usize = self.n_lwe;
        &self.ciphertexts
    }
    pub fn capacity(&self) -> usize
    {
        let n_lwe: usize = self.n_lwe;
        self.ciphertexts.capacity()
    }
}

impl  PlanBoundary {
    pub fn new(mut log_q_lwe: usize, mut plan: &BootstrapPlan) -> Result<Self, BoundaryError>
    {
        plan.validate().map_err(BoundaryError::InvalidPlan)?;
        let delta = wire_delta::<LOG_Q_LWE>(log_q_lwe, (plan.k_max as usize));
        Ok(Self { plan: plan, delta: delta })
    }
    pub fn plan(&self) -> &BootstrapPlan
    {
        self.plan
    }
    pub fn delta(&self) -> u32
    {
        self.delta
    }
    pub fn input_buffer(&self, mut n_lwe: usize) -> CiphertextInputBufferDyn
    {
        CiphertextInputBufferDyn { ciphertexts: Vec::with_capacity((self.plan.num_inputs as usize)), n_lwe: 0 }
    }
    pub fn encrypt_inputs_into<R: SpecRng>(&self, mut n_lwe: usize, mut log_q_lwe: usize, mut eta: usize, mut inputs: &[bool], mut sk: &BinfheLweSecretKeyDyn, mut rng: &mut R, mut output: &mut CiphertextInputBufferDyn) -> Result<(), BoundaryError>
    {
        self.require_input_count(inputs.len())?;
        if output.ciphertexts.capacity() < inputs.len(){
    return Err(BoundaryError::InputBufferCapacity { required: inputs.len(), actual: output.ciphertexts.capacity() });
};
        output.ciphertexts.clear();
        output.ciphertexts.extend(inputs.iter().map(|bit| {
    binfhe_lwe_encrypt::<N_LWE, LOG_Q_LWE, ETA, R>(n_lwe, log_q_lwe, eta, bit, self.delta, sk, rng)
}));
        Ok(())
    }
    pub fn encrypt_inputs<R: SpecRng>(&self, mut n_lwe: usize, mut log_q_lwe: usize, mut eta: usize, mut inputs: &[bool], mut sk: &BinfheLweSecretKeyDyn, mut rng: &mut R) -> Result<Vec<BinfheLweCiphertextDyn>, BoundaryError>
    {
        let mut output = self.input_buffer(n_lwe);
        self.encrypt_inputs_into(n_lwe, log_q_lwe, eta, inputs, sk, rng, &mut output)?;
        Ok(output.ciphertexts)
    }
    pub fn decrypt_outputs(&self, mut n_lwe: usize, mut log_q_lwe: usize, mut wire_arena: &[BinfheLweCiphertextDyn], mut sk: &BinfheLweSecretKeyDyn) -> Result<Vec<bool>, BoundaryError>
    {
        let required = self.plan.outputs.iter().copied().max().into_iter().map(|wire| ((wire as usize) + 1)).collect::<Vec<_>>().unwrap_or(0);
        if wire_arena.len() < required{
    return Err(BoundaryError::WireArenaTooShort { required: required, actual: wire_arena.len() });
};
        Ok(self.plan.outputs.iter().map(|wire| {
    binfhe_lwe_decrypt::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, &wire_arena[wire as usize], sk, self.delta)
}).collect::<Vec<_>>())
    }
    pub fn execute(&self, mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut priv_ell: usize, mut priv_base_log: usize, mut inputs: &[BinfheLweCiphertextDyn], mut cells: &[BinfheRlweCiphertextDyn], mut bk: &BinfheBootstrappingKeyDyn, mut cbk: &CircuitBootstrappingKeyDyn) -> Result<(Vec<BinfheLweCiphertextDyn>, Vec<BinfheRlweCiphertextDyn>), BoundaryError>
    {
        self.require_ciphertext_input_count(inputs.len())?;
        self.require_cell_count(cells.len())?;
        Ok(execute_plan::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, PRIV_ELL, PRIV_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, priv_ell, priv_base_log, self.plan, inputs, cells, bk, cbk))
    }
    pub fn execute_in_workspace(&self, mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut priv_ell: usize, mut priv_base_log: usize, mut inputs: &[BinfheLweCiphertextDyn], mut cells: &[BinfheRlweCiphertextDyn], mut bk: &BinfheBootstrappingKeyDyn, mut cbk: &CircuitBootstrappingKeyDyn, mut workspace: &mut PlanWorkspaceDyn) -> Result<(&[BinfheLweCiphertextDyn], &[BinfheRlweCiphertextDyn]), BoundaryError>
    {
        self.require_ciphertext_input_count(inputs.len())?;
        self.require_cell_count(cells.len())?;
        execute_plan_in_workspace::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, PRIV_ELL, PRIV_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, priv_ell, priv_base_log, self.plan, inputs, cells, bk, cbk, workspace).map_err(BoundaryError::Workspace)
    }
    pub fn require_input_count(&self, mut actual: usize) -> Result<(), BoundaryError>
    {
        let expected = self.plan.num_inputs as usize;
        if actual == expected{
    Ok(())
} else {
    Err(BoundaryError::InputCount { expected: expected, actual: actual })
}
    }
    pub fn require_ciphertext_input_count(&self, mut actual: usize) -> Result<(), BoundaryError>
    {
        let expected = self.plan.num_inputs as usize;
        if actual == expected{
    Ok(())
} else {
    Err(BoundaryError::CiphertextInputCount { expected: expected, actual: actual })
}
    }
    pub fn require_cell_count(&self, mut actual: usize) -> Result<(), BoundaryError>
    {
        let expected = self.plan.num_cells as usize;
        if actual == expected{
    Ok(())
} else {
    Err(BoundaryError::CellCount { expected: expected, actual: actual })
}
    }
}

impl  AsKeySwitchingKey<N_LWE, BIG_N, KS_ELL> for BinfheKeySwitchingKeyDyn {
    fn ksk_rows(&self) -> &[[BinfheLweCiphertextDyn; KS_ELL]]
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let ks_ell: usize = self.ks_ell;
        &self.ksk
    }
}

impl  AsKeySwitchingKey<N_LWE, BIG_N, KS_ELL> for BinfheKeySwitchingKeyRefDyn {
    fn ksk_rows(&self) -> &[[BinfheLweCiphertextDyn; KS_ELL]]
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let ks_ell: usize = self.ks_ell;
        self.ksk
    }
}

impl  AsBinfheBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL> for BinfheBootstrappingKeyDyn {
    fn bsk_rows(&self) -> &[BinfheRgswCiphertextDyn]
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let bs_ell: usize = self.bs_ell;
        let ks_ell: usize = self.ks_ell;
        &self.bsk
    }
    fn ksk_ref(&self) -> BinfheKeySwitchingKeyRefDyn
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let bs_ell: usize = self.bs_ell;
        let ks_ell: usize = self.ks_ell;
        BinfheKeySwitchingKeyRefDyn { ksk: &self.ksk.ksk, n_lwe: 0, big_n: 0, ks_ell: 0 }
    }
}

impl  AsBinfheBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL> for BinfheBootstrappingKeyRefDyn {
    fn bsk_rows(&self) -> &[BinfheRgswCiphertextDyn]
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let bs_ell: usize = self.bs_ell;
        let ks_ell: usize = self.ks_ell;
        self.bsk
    }
    fn ksk_ref(&self) -> BinfheKeySwitchingKeyRefDyn
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let bs_ell: usize = self.bs_ell;
        let ks_ell: usize = self.ks_ell;
        self.ksk
    }
}

impl  BinfheBootstrappingKeyDyn {
    pub fn as_ref(&self) -> BinfheBootstrappingKeyRefDyn
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let bs_ell: usize = self.bs_ell;
        let ks_ell: usize = self.ks_ell;
        BinfheBootstrappingKeyRefDyn { bsk: &self.bsk, ksk: BinfheKeySwitchingKeyRefDyn { ksk: &self.ksk.ksk, n_lwe: 0, big_n: 0, ks_ell: 0 }, n_lwe: 0, big_n: 0, bs_ell: 0, ks_ell: 0 }
    }
}

impl  LutDyn {
    pub fn new(mut addr_bits: usize, mut table_len: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut k_max: usize, mut logical: [bool; TABLE_LEN]) -> Result<Self, LutError>
    {
        match check_lut_shape(addr_bits, table_len, big_n, log_q, log_q_lwe, k_max) {
    Err(e) => Err(e),
    Ok(_) => Ok(Self { logical: logical, test_poly: fill_test_poly::<BIG_N>(big_n, &logical, addr_bits, k_max, log_q, log_q_lwe), is_constant: table_is_constant(&logical) }),
}
    }
    pub fn entries(&self) -> &[bool; TABLE_LEN]
    {
        let addr_bits: usize = self.addr_bits;
        let table_len: usize = self.table_len;
        let big_n: usize = self.big_n;
        let log_q: usize = self.log_q;
        let log_q_lwe: usize = self.log_q_lwe;
        let k_max: usize = self.k_max;
        &self.logical
    }
    pub fn test_polynomial(&self) -> &[u32; BIG_N]
    {
        let addr_bits: usize = self.addr_bits;
        let table_len: usize = self.table_len;
        let big_n: usize = self.big_n;
        let log_q: usize = self.log_q;
        let log_q_lwe: usize = self.log_q_lwe;
        let k_max: usize = self.k_max;
        &self.test_poly
    }
    pub fn is_constant(&self) -> bool
    {
        let addr_bits: usize = self.addr_bits;
        let table_len: usize = self.table_len;
        let big_n: usize = self.big_n;
        let log_q: usize = self.log_q;
        let log_q_lwe: usize = self.log_q_lwe;
        let k_max: usize = self.k_max;
        self.is_constant
    }
    pub fn constant_value(&self) -> bool
    {
        let addr_bits: usize = self.addr_bits;
        let table_len: usize = self.table_len;
        let big_n: usize = self.big_n;
        let log_q: usize = self.log_q;
        let log_q_lwe: usize = self.log_q_lwe;
        let k_max: usize = self.k_max;
        self.logical[0]
    }
    pub fn output_delta(&self) -> u32
    {
        let addr_bits: usize = self.addr_bits;
        let table_len: usize = self.table_len;
        let big_n: usize = self.big_n;
        let log_q: usize = self.log_q;
        let log_q_lwe: usize = self.log_q_lwe;
        let k_max: usize = self.k_max;
        (1 as u32) << ((log_q_lwe - 1) - (k_max as u32))
    }
}

impl  LutDyn {
}

impl  LutDyn {
}

impl  BootstrapPlan {
    pub fn workspace_requirements(&self) -> Result<PlanWorkspaceRequirements, PlanWorkspaceError>
    {
        self.validate().map_err(PlanWorkspaceError::InvalidPlan)?;
        let mut wires = self.num_inputs as usize;
        let mut rgsws = 0 as usize;
        let mut cells = self.num_cells as usize;
        let mut lut_inputs = 0 as usize;
        for layer in &self.layers{
    for op in layer{
    match op {
    .. => {
    wires = wires.checked_add(1).ok_or(PlanWorkspaceError::CapacityOverflow)?;
},
    PlanOp::CircuitBootstrap { .. } => {
    rgsws = rgsws.checked_add(1).ok_or(PlanWorkspaceError::CapacityOverflow)?;
},
    PlanOp::RgswMux { .. } => {
    cells = cells.checked_add(1).ok_or(PlanWorkspaceError::CapacityOverflow)?;
},
};
    match op {
    PlanOp::Lut { inputs: inputs, .. } => {
    lut_inputs = lut_inputs.max(inputs.len());
},
    _ => {
},
};
};
};
        Ok(PlanWorkspaceRequirements { wires: wires, rgsws: rgsws, cells: cells, lut_inputs: lut_inputs })
    }
}

impl  PlanWorkspaceDyn {
    pub fn new(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut plan: &BootstrapPlan) -> Result<Self, PlanWorkspaceError>
    {
        let required = plan.workspace_requirements()?;
        Ok(Self { wires: Vec::with_capacity(required.wires), rgsws: Vec::with_capacity(required.rgsws), cells: Vec::with_capacity(required.cells), lut_inputs: Vec::with_capacity(required.lut_inputs) })
    }
    pub fn reset(&mut self)
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let bs_ell: usize = self.bs_ell;
        self.wires.clear();
        self.rgsws.clear();
        self.cells.clear();
        self.lut_inputs.clear();
    }
    pub fn requirements(&self) -> PlanWorkspaceRequirements
    {
        let n_lwe: usize = self.n_lwe;
        let big_n: usize = self.big_n;
        let bs_ell: usize = self.bs_ell;
        PlanWorkspaceRequirements { wires: self.wires.capacity(), rgsws: self.rgsws.capacity(), cells: self.cells.capacity(), lut_inputs: self.lut_inputs.capacity() }
    }
}

impl  BootstrapPlan {
    pub fn execute_clear(&self, mut inputs: &[bool], mut cells: &[bool]) -> (Vec<bool>, Vec<bool>)
    {
        let mut wires = inputs.to_vec();
        let mut rgsws = Vec::new();
        let mut cell_arena = cells.to_vec();
        for layer in &self.layers{
    for op in layer{
    match op {
    PlanOp::Const { out: out, value: value } => {
    wires.push(*value);
},
    PlanOp::Not { input: input, out: out } => {
    wires.push(!wires[*input as usize]);
},
    PlanOp::Lut { inputs: inputs, table: table, out: out } => {
    let mut address = 0 as usize;
    for (bit, input) in inputs.iter().enumerate(){
    address |= ((wires[*input as usize] as usize) << bit);
};
    wires.push(self.luts[*table as usize].entries[address]);
},
    PlanOp::CircuitBootstrap { input: input, out: out } => {
    rgsws.push(wires[*input as usize]);
},
    PlanOp::RgswMux { sel: sel, then_cell: then_cell, else_cell: else_cell, out: out } => {
    cell_arena.push(if rgsws[*sel as usize]{
    cell_arena[*then_cell as usize]
} else {
    cell_arena[*else_cell as usize]
});
},
};
};
};
        (wires, cell_arena)
    }
    pub fn bootstrap_op_count(&self) -> u64
    {
        let mut count = 0 as u64;
        for layer in &self.layers{
    for op in layer{
    match op {
    PlanOp::Lut { table: table, .. } => {
    if !table_is_constant(&self.luts[*table as usize].entries){
    count += 1;
}
},
    PlanOp::CircuitBootstrap { .. } => count += 1,
    _ => {
},
};
};
};
        count
    }
    pub fn validate(&self) -> Result<(), PlanError>
    {
        for (i, spec) in self.luts.iter().enumerate(){
    let len = spec.entries.len();
    if (len == 0) || !len.is_power_of_two(){
    return Err(PlanError::BadTableShape { table: (i as LutId) });
};
    let arity = len.trailing_zeros() as usize;
    if arity > (self.k_max as usize){
    return Err(PlanError::ArityExceedsKMax { table: (i as LutId) });
};
};
        let mut wires = self.num_inputs;
        let mut rgsws = 0 as u32;
        let mut cells = self.num_cells;
        for layer in &self.layers{
    for op in layer{
    match op {
    PlanOp::Const { out: out, .. } => {
    if *out != wires{
    return Err(PlanError::BadReference);
};
    wires += 1;
},
    PlanOp::Not { input: input, out: out } => {
    if (*input >= wires) || (*out != wires){
    return Err(PlanError::BadReference);
};
    wires += 1;
},
    PlanOp::Lut { inputs: inputs, table: table, out: out } => {
    if (*table as usize) >= self.luts.len(){
    return Err(PlanError::BadReference);
};
    let arity = self.luts[*table as usize].entries.len().trailing_zeros();
    if (inputs.len() != (arity as usize)) || inputs.iter().any(|w| (*w >= wires)) || (*out != wires){
    return Err(PlanError::BadReference);
};
    wires += 1;
},
    PlanOp::CircuitBootstrap { input: input, out: out } => {
    if (*input >= wires) || (*out != rgsws){
    return Err(PlanError::BadReference);
};
    rgsws += 1;
},
    PlanOp::RgswMux { sel: sel, then_cell: then_cell, else_cell: else_cell, out: out } => {
    if (*sel >= rgsws) || (*then_cell >= cells) || (*else_cell >= cells) || (*out != cells){
    return Err(PlanError::BadReference);
};
    cells += 1;
},
};
};
};
        if self.outputs.iter().any(|w| (*w >= wires)) || self.cell_outputs.iter().any(|c| (*c >= cells)){
    return Err(PlanError::BadOutput);
};
        let count = self.bootstrap_op_count();
        if count > 0{
    let log2_count = 64 - count.leading_zeros();
    if (self.budget.total_log2 < self.budget.per_bootstrap_log2) || (((self.budget.total_log2 - self.budget.per_bootstrap_log2) + 1) < log2_count){
    return Err(PlanError::BudgetInconsistent);
}
};
        Ok(())
    }
    pub fn plan_hash(&self) -> u64
    {
        let mut h = 14695981039346656037 as u64;
        for spec in &self.luts{
    for (i, chunk) in spec.entries.chunks(8).enumerate(){
    let mut byte = 0 as u8;
    for (j, ..) in chunk.iter().enumerate(){
    byte |= ((e as u8) << j);
};
    let _ = i;
};
};
        for layer in &self.layers{
    for op in layer{
    match op {
    PlanOp::Const { out: out, value: value } => {
},
    PlanOp::Not { input: input, out: out } => {
},
    PlanOp::Lut { inputs: inputs, table: table, out: out } => {
    for w in inputs{
};
},
    PlanOp::CircuitBootstrap { input: input, out: out } => {
},
    PlanOp::RgswMux { sel: sel, then_cell: then_cell, else_cell: else_cell, out: out } => {
},
};
};
};
        for w in &self.outputs{
};
        for c in &self.cell_outputs{
};
        h
    }
}

impl  Reader {
    pub fn take(&mut self, mut count: usize) -> Result<&[u8], DecodeError>
    {
        let end = self.offset.checked_add(count).ok_or(DecodeError::Truncated)?;
        let bytes = self.bytes.get(self.offset..end).ok_or(DecodeError::Truncated)?;
        self.offset = end;
        Ok(bytes)
    }
    pub fn byte(&mut self) -> Result<u8, DecodeError>
    {
        Ok(self.take(1)?[0])
    }
    pub fn u32(&mut self) -> Result<u32, DecodeError>
    {
        let bytes: [u8; 4] = self.take(4)?.try_into().map_err(|_| DecodeError::Truncated)?;
        Ok(u32::from_le_bytes(bytes))
    }
    pub fn count(&mut self) -> Result<usize, DecodeError>
    {
        let count = self.u32()? as usize;
        if count > MAX_ITEMS{
    return Err(DecodeError::TooLarge);
};
        Ok(count)
    }
    pub fn ids(&mut self) -> Result<Vec<u32>, DecodeError>
    {
        let count = self.count()?;
        let mut ids = Vec::with_capacity(count);
        for _ in 0.. count{
    ids.push(self.u32()?);
};
        Ok(ids)
    }
}

impl  WireState {
    pub fn active_label(&self, mut masked_bit: bool, mut delta: &GlobalDelta, mut p: u64) -> u64
    {
        if masked_bit{
    (self.a0 + delta.value()) % p
} else {
    self.a0
}
    }
}

impl  GlobalDelta {
    pub fn new(mut delta: u64, mut plaintext_modulus: u64) -> Option<Self>
    {
        if (delta == 0) || (delta >= plaintext_modulus) || ((delta & 1) == 0){
    return None;
};
        Some(Self { delta: delta })
    }
    pub fn value(&self) -> u64
    {
        self.delta
    }
    pub fn inverse(&self, mut plaintext_modulus: u64) -> Option<u64>
    {
        ring::inverse_mod(self.delta, plaintext_modulus)
    }
}

impl  Stitch {
    pub fn bits(&self) -> usize
    {
        match self {
    Self::OneBit(_) => 1,
    Self::FiveBits(_) => 5,
}
    }
    pub fn correction(&self) -> bool
    {
        match self {
    Self::OneBit(b) => *b,
    Self::FiveBits(b) => ((b & 1) == 1),
}
    }
    pub fn five(mut correction: bool) -> Self
    {
        let s = u8::from(correction);
        let pattern = (s << 1) | (u8::from((!correction & true)) << 2) | (s << 3) | (u8::from((!correction & true)) << 4);
        Self::FiveBits((s | pattern))
    }
    pub fn check_five(&self) -> Result<bool, StitchError>
    {
        match *self {
    Self::OneBit(_) => Err(StitchError::WrongVariant),
    Self::FiveBits(b) => {
    if (b & !31) != 0{
    return Err(StitchError::NonCanonicalBit);
};
    let s = b & 1;
    let expected = Self::five((s == 1));
    if *self != expected{
    return Err(StitchError::NonCanonicalBit);
};
    Ok((s == 1))
},
}
    }
}

impl  core::fmt::Display for StitchError {
    fn fmt(&self, mut f: &mut Formatter) -> Result
    {
        match self {
    _ => f.write_str("stitching record stream is exhausted"),
    _ => f.write_str("stitching record variant mismatch"),
    _ => f.write_str("stitching record is not canonical"),
    _ => f.write_str("free-XOR payload exceeds reduction bound"),
    Self::Crypto(_) => f.write_str("underlying SWHE error"),
}
    }
}

impl  core::error::Error for StitchError {
}

impl  From<SwheError> for StitchError {
    fn from(mut e: SwheError) -> Self
    {
        Self::Crypto(e)
    }
}

impl  Circuit {
    pub fn wires(&self) -> usize
    {
        self.inputs + self.gates.len()
    }
    pub fn validate(&self) -> Result<(), StitchError>
    {
        let wires = self.wires();
        for (i, gate) in self.gates.iter().enumerate(){
    let out = self.inputs + i;
    let (a, b) = gate.operands();
    if (a >= out) || (b >= out) || (out >= wires){
    return Err(StitchError::WrongVariant);
};
};
        if self.outputs.iter().any(|..| (o >= wires)){
    return Err(StitchError::WrongVariant);
};
        Ok(())
    }
    pub fn eval_plain(&self, mut inputs: &[bool]) -> Result<Vec<bool>, StitchError>
    {
        if inputs.len() != self.inputs{
    return Err(StitchError::WrongVariant);
};
        self.validate()?;
        let mut values = vec![false; self.wires()];
        values[..self.inputs].copy_from_slice(inputs);
        for (i, gate) in self.gates.iter().enumerate(){
    let (a, b) = gate.operands();
    values[self.inputs + i] = match gate {
    Gate::And { .. } => (values[a] & values[b]),
    Gate::Xor { .. } => (values[a] ^ values[b]),
    Gate::Not { .. } => !values[a],
};
};
        Ok(self.outputs.iter().map(|o| values[o]).collect::<Vec<_>>())
    }
}

impl  Gate {
    pub fn operands(&self) -> (usize, usize)
    {
        match *self {
    .. => (a, b),
    Self::Not { a: a } => (a, a),
}
    }
    pub fn out_at(&self, mut inputs: usize, mut index: usize) -> usize
    {
        inputs + index
    }
}

impl  ReferenceEquation {
    pub fn unwrap(&self, mut ctx: &Context, mut label: &Ciphertext, mut expanded: &ExpandedWire, mut delta: &GlobalDelta) -> Result<Ciphertext, StitchError>
    {
        let p = ctx.parameters().plaintext_modulus;
        let inv = delta.inverse(p).ok_or(StitchError::NonCanonicalBit)?;
        let ct_a0 = ctx.switch_to(&expanded.ct_a0, label.level())?;
        let diff = ctx.sub(label, &ct_a0)?;
        Ok(ctx.mul_plain(&diff, &ctx.constant_slots(inv))?)
    }
    pub fn wrap(&self, mut ctx: &Context, mut masked: &Ciphertext, mut expanded: &ExpandedWire, mut delta: &GlobalDelta, mut online_base: usize) -> Result<Ciphertext, StitchError>
    {
        let scaled = ctx.mul_plain(masked, &ctx.constant_slots(delta.value()))?;
        let scaled = ctx.switch_to(&scaled, online_base)?;
        let ct_a0 = ctx.switch_to(&expanded.ct_a0, online_base)?;
        Ok(ctx.add(&scaled, &ct_a0)?)
    }
    pub fn xor_bits(&self, mut ctx: &Context, mut a: &Ciphertext, mut b: &Ciphertext, mut ksk: &KeySwitchKeySet) -> Result<Ciphertext, StitchError>
    {
        let top = ctx.max_level();
        let a = ctx.switch_to(a, top)?;
        let b = ctx.switch_to(b, top)?;
        let level = top;
        let sum = ctx.add(&a, &b)?;
        let product = ctx.mul(&a, &b, ksk)?;
        let doubled = ctx.mul_plain(&product, &ctx.constant_slots(2))?;
        let out = ctx.sub(&sum, &doubled)?;
        Ok(ctx.switch_to(&out, level)?)
    }
    pub fn complement(&self, mut ctx: &Context, mut x: &Ciphertext) -> Result<Ciphertext, StitchError>
    {
        let p = ctx.parameters().plaintext_modulus;
        let neg = ctx.mul_plain(x, &ctx.constant_slots((p - 1)))?;
        Ok(ctx.add_plain(&neg, &ctx.constant_slots(1))?)
    }
    pub fn parity(&self, mut ctx: &Context, mut m: &Ciphertext, mut k_max: usize, mut ksk: &KeySwitchKeySet) -> Result<Ciphertext, StitchError>
    {
        if k_max == 1{
    return Ok(m.clone());
};
        let p = ctx.parameters().plaintext_modulus;
        let coeffs = parity_coeffs(p, k_max)?;
        let level = m.level();
        let mut acc = ctx.mul_plain(&ctx.encrypt_one_like(m)?, &ctx.constant_slots(coeffs[k_max]))?;
        for i in 0..k_max.rev(){
    acc = ctx.switch_to(&ctx.mul(&acc, m, ksk)?, level)?;
    if coeffs[i] != 0{
    acc = ctx.add_plain(&acc, &ctx.constant_slots(coeffs[i]))?;
};
};
        Ok(acc)
    }
}

impl  GateEquation for ReferenceEquation {
    fn garble_and(&self, mut gate_index: usize, mut a: &WireState, mut b: &WireState, mut out: &WireState) -> Stitch
    {
        let bit = (((gate_index as u64).wrapping_mul(2654435769).wrapping_add(a.a0).wrapping_add(b.a0).wrapping_add(u64::from(out.pi)) >> 63) & 1) == 1;
        if self.xor_charged{
    Stitch::OneBit(bit)
} else {
    Stitch::five(bit)
}
    }
    fn eval_and(&self, mut ctx: &Context, mut la: &Ciphertext, mut lb: &Ciphertext, mut expanded: &ExpandedGate, mut delta: &GlobalDelta, mut ksk: &KeySwitchKeySet, mut stitch: &Stitch) -> Result<Ciphertext, StitchError>
    {
        let correction = match (self.xor_charged, stitch) {
    (.., Stitch::OneBit(b)) => *b,
    (.., s) => s.check_five()?,
    _ => return Err(StitchError::WrongVariant),
};
        let k_max = if self.xor_charged{
    1
} else {
    MAX_PAYLOAD
};
        let m_a = self.unwrap(ctx, la, &expanded.a, delta)?;
        let m_b = self.unwrap(ctx, lb, &expanded.b, delta)?;
        let b_a = self.parity(ctx, &m_a, k_max, ksk)?;
        let b_b = self.parity(ctx, &m_b, k_max, ksk)?;
        let x_a = self.xor_bits(ctx, &b_a, &expanded.a.ct_pi, ksk)?;
        let x_b = self.xor_bits(ctx, &b_b, &expanded.b.ct_pi, ksk)?;
        let top = ctx.max_level();
        let x_a = ctx.switch_to(&x_a, top)?;
        let x_b = ctx.switch_to(&x_b, top)?;
        let v_ab = ctx.switch_to(&ctx.mul(&x_a, &x_b, ksk)?, top)?;
        let mut b_c = self.xor_bits(ctx, &v_ab, &expanded.out.ct_pi, ksk)?;
        if correction{
    b_c = self.complement(ctx, &b_c)?;
};
        self.wrap(ctx, &b_c, &expanded.out, delta, ctx.max_level())
    }
    fn garble_xor(&self, mut p: u64, mut a: &WireState, mut b: &WireState, mut out: &WireState) -> Option<Stitch>
    {
        if self.xor_charged{
    Some(Stitch::OneBit(((a.pi ^ b.pi) ^ out.pi)))
} else {
    None
}
    }
    fn eval_xor(&self, mut ctx: &Context, mut la: &Ciphertext, mut lb: &Ciphertext, mut expanded: &ExpandedGate, mut delta: &GlobalDelta, mut ksk: &KeySwitchKeySet, mut stitch: Option<&Stitch>) -> Result<Ciphertext, StitchError>
    {
        if self.xor_charged{
    let bit = match stitch {
    Some(Stitch::OneBit(b)) => *b,
    _ => return Err(StitchError::WrongVariant),
};
    let b_a = self.unwrap(ctx, la, &expanded.a, delta)?;
    let b_b = self.unwrap(ctx, lb, &expanded.b, delta)?;
    let mut b_c = self.xor_bits(ctx, &b_a, &b_b, ksk)?;
    if bit{
    b_c = self.complement(ctx, &b_c)?;
};
    self.wrap(ctx, &b_c, &expanded.out, delta, ctx.max_level())
} else {
    if stitch.is_some(){
    return Err(StitchError::WrongVariant);
};
    Ok(ctx.add(la, lb)?)
}
    }
    fn xor_is_charged(&self) -> bool
    {
        self.xor_charged
    }
}

impl  DdGadget for LatticeDd {
    fn p(&self) -> u64
    {
        self.p
    }
    fn delta(&self) -> &Delta
    {
        &self.delta_pinned
    }
    fn row(&self, mut t: i64, mut random: &mut impl SpecRng) -> RowCt
    {
        let tau = LatticeDd::row(self, t, random);
        rowct_from_lattice(&tau, self.p)
    }
    fn dd0(&self, mut a: &LabelPoly, mut tau: &RowCt) -> LabelPoly
    {
        self.mul_p(&tau.c1, a)
    }
    fn dd_i(&self, mut a: &LabelPoly, mut i: bool, mut tau: &RowCt) -> LabelPoly
    {
        let base = DdGadget::dd0(self, a, tau);
        if i{
    ring::add_coeffs(&base, &tau.c0, self.p)
} else {
    base
}
    }
    fn dd_ij(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut tau: &RowCt) -> LabelPoly
    {
        let share = self.hss_mul_rp(a, i, b, j);
        let base = self.mul_p(&tau.c1, &share);
        if i && j{
    ring::add_coeffs(&base, &tau.c0, self.p)
} else {
    base
}
    }
    fn eval(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut taus: &[RowCt; 3]) -> LabelPoly
    {
        let mut out = DdGadget::dd_i(self, a, i, &taus[0]);
        out = ring::add_coeffs(&out, &DdGadget::dd_i(self, b, j, &taus[1]), self.p);
        ring::add_coeffs(&out, &DdGadget::dd_ij(self, a, i, b, j, &taus[2]), self.p)
    }
}

impl  LatticeDd {
    pub fn new(mut n: usize, mut p: u64, mut q: u64, mut random: &mut impl SpecRng) -> Result<Self, LatticeError>
    {
        if (n == 0) || (p < 3) || ((p % 2) == 0) || ((q % p) != 0) || ((q % 2) == 0){
    return Err(LatticeError::InvalidParameters);
};
        let mut delta = vec![(0 as u64); n];
        for c in delta.iter_mut(){
    let r = random.next_u32() % 3;
    *c = match r {
    .. => 0,
    .. => 1,
    _ => (p - 1),
};
};
        delta[0] = 1;
        let delta_q: RqPoly = delta.iter().map(|c| {
    if c == 0{
    0
} else if c == 1{
    1
} else {
    q - 1
}
}).collect::<Vec<_>>();
        let u = sample_uniform_rq(n, q, random);
        let ud = negacyclic_mul_rq(&u, &delta_q, q);
        let e1 = sample_noise_rq(n, p, q, random);
        let v = add_rq(&negate_rq(&ud, q), &e1, q);
        let d2_q = negacyclic_mul_rq(&delta_q, &delta_q, q);
        let ud2 = negacyclic_mul_rq(&u, &d2_q, q);
        let e2 = sample_noise_rq(n, p, q, random);
        let neg_delta_q = negate_rq(&delta_q, q);
        let w = add_rq(&sub_rq(&neg_delta_q, &ud2, q), &e2, q);
        let red = |poly| {
    poly.iter().map(|c| (c % p)).collect::<Vec<_>>()
};
        let triple_rp = (red(&u), red(&v), red(&w));
        let delta_pinned = DeltaLoweredDyn { coeffs: delta.clone(), p: p, n: 0 };
        let ntt_q = ring::Ntt::new(n, q).ok();
        let ntt_p = ring::Ntt::new(n, p).ok();
        Ok(Self { n: n, p: p, q: q, delta: delta, delta_pinned: delta_pinned, triple: (u, v, w), triple_rp: triple_rp, ntt_q: ntt_q, ntt_p: ntt_p })
    }
    pub fn mul_q(&self, mut a: &[u64], mut b: &[u64]) -> RqPoly
    {
        match &self.ntt_q {
    Some(ntt) => ring::negacyclic_mul_ntt(a, b, ntt),
    None => negacyclic_mul_rq(a, b, self.q),
}
    }
    pub fn mul_p(&self, mut a: &[u64], mut b: &[u64]) -> RpPoly
    {
        match &self.ntt_p {
    Some(ntt) => ring::negacyclic_mul_ntt(a, b, ntt),
    None => ring::negacyclic_mul_coeffs(a, b, self.p),
}
    }
    pub fn hss_mul_rp(&self, mut a: &RpPoly, mut i: bool, mut b: &RpPoly, mut j: bool) -> RpPoly
    {
        let p = self.p;
        let (u, v, w) = &self.triple_rp;
        let ab = self.mul_p(a, b);
        let mut out = self.mul_p(&ab, u);
        if i{
    let bv = self.mul_p(b, v);
    out = ring::add_coeffs(&out, &bv, p);
};
        if j{
    let av = self.mul_p(a, v);
    out = ring::add_coeffs(&out, &av, p);
};
        if i && j{
    out = ring::sub_coeffs(&out, w, p);
};
        out
    }
    pub fn degree(&self) -> usize
    {
        self.n
    }
    pub fn p(&self) -> u64
    {
        self.p
    }
    pub fn q(&self) -> u64
    {
        self.q
    }
    pub fn delta(&self) -> &RpPoly
    {
        &self.delta
    }
    pub fn row(&self, mut t: i64, mut random: &mut impl SpecRng) -> LatticeRowCt
    {
        let (n, p, q) = (self.n, self.p, self.q);
        let u2 = sample_uniform_rq(n, q, random);
        let delta_q: RqPoly = self.delta.iter().map(|c| {
    if c == 0{
    0
} else if c == 1{
    1
} else {
    q - 1
}
}).collect::<Vec<_>>();
        let u2d = self.mul_q(&u2, &delta_q);
        let td: RqPoly = delta_q.iter().map(|c| {
    let c = c as i128;
    let v = c * (t as i128);
    v.rem_euclid((q as i128)) as u64
}).collect::<Vec<_>>();
        let e = sample_noise_rq(n, p, q, random);
        let u1 = add_rq(&sub_rq(&td, &u2d, q), &e, q);
        LatticeRowCt { u1: u1, u2: u2 }
    }
    pub fn dec_row(&self, mut tau: &LatticeRowCt) -> RpPoly
    {
        let q = self.q;
        let delta_q: RqPoly = self.delta.iter().map(|c| {
    if c == 0{
    0
} else if c == 1{
    1
} else {
    q - 1
}
}).collect::<Vec<_>>();
        let u2d = self.mul_q(&tau.u2, &delta_q);
        let phase = add_rq(&tau.u1, &u2d, q);
        round_to_rp(&phase, q, self.p)
    }
    pub fn dd0(&self, mut a: &RpPoly, mut tau: &LatticeRowCt) -> RpPoly
    {
        let a_q: RqPoly = a.iter().map(|c| (c % self.q)).collect::<Vec<_>>();
        let prod = self.mul_q(&tau.u2, &a_q);
        round_to_rp(&prod, self.q, self.p)
    }
    pub fn dd_i(&self, mut a: &RpPoly, mut i: bool, mut tau: &LatticeRowCt) -> RpPoly
    {
        let mut acc = self.dd0_raw(a, tau);
        if i{
    acc = add_rq(&acc, &tau.u1, self.q);
};
        round_to_rp(&acc, self.q, self.p)
    }
    pub fn dd0_raw(&self, mut a: &RpPoly, mut tau: &LatticeRowCt) -> RqPoly
    {
        let a_q: RqPoly = a.iter().map(|c| (c % self.q)).collect::<Vec<_>>();
        self.mul_q(&tau.u2, &a_q)
    }
    pub fn hss_mul(&self, mut a: &RpPoly, mut i: bool, mut b: &RpPoly, mut j: bool) -> RpPoly
    {
        let q = self.q;
        let (u, v, w) = &self.triple;
        let a_q: RqPoly = a.iter().map(|c| (c % q)).collect::<Vec<_>>();
        let b_q: RqPoly = b.iter().map(|c| (c % q)).collect::<Vec<_>>();
        let ab = self.mul_q(&a_q, &b_q);
        let mut acc = self.mul_q(&ab, u);
        if i{
    let bv = self.mul_q(&b_q, v);
    acc = add_rq(&acc, &bv, q);
};
        if j{
    let av = self.mul_q(&a_q, v);
    acc = add_rq(&acc, &av, q);
};
        if i && j{
    acc = sub_rq(&acc, w, q);
};
        round_to_rp(&acc, q, self.p)
    }
    pub fn dd_ij(&self, mut a: &RpPoly, mut i: bool, mut b: &RpPoly, mut j: bool, mut tau: &LatticeRowCt) -> RpPoly
    {
        let q = self.q;
        let share = self.hss_mul_raw(a, i, b, j);
        let mut acc = self.mul_q(&tau.u2, &share);
        if i && j{
    acc = add_rq(&acc, &tau.u1, q);
};
        round_to_rp(&acc, q, self.p)
    }
    pub fn hss_mul_raw(&self, mut a: &RpPoly, mut i: bool, mut b: &RpPoly, mut j: bool) -> RqPoly
    {
        let q = self.q;
        let (u, v, w) = &self.triple;
        let a_q: RqPoly = a.iter().map(|c| (c % q)).collect::<Vec<_>>();
        let b_q: RqPoly = b.iter().map(|c| (c % q)).collect::<Vec<_>>();
        let ab = self.mul_q(&a_q, &b_q);
        let mut acc = self.mul_q(&ab, u);
        if i{
    let bv = self.mul_q(&b_q, v);
    acc = add_rq(&acc, &bv, q);
};
        if j{
    let av = self.mul_q(&a_q, v);
    acc = add_rq(&acc, &av, q);
};
        if i && j{
    acc = sub_rq(&acc, w, q);
};
        acc
    }
    pub fn eval(&self, mut a: &RpPoly, mut i: bool, mut b: &RpPoly, mut j: bool, mut taus: &[LatticeRowCt; 3]) -> RpPoly
    {
        let p = self.p;
        let mut out = self.dd_i(a, i, &taus[0]);
        out = add_rp(&out, &self.dd_i(b, j, &taus[1]), p);
        add_rp(&out, &self.dd_ij(a, i, b, j, &taus[2]), p)
    }
}

impl  LatticeRnsDd {
    pub fn new(mut n: usize, mut p: u64, mut q0: u64, mut q1: u64, mut random: &mut impl SpecRng) -> Result<Self, RnsError>
    {
        if (n == 0) || (p < 3) || ((p % 2) == 0) || ((q0 % p) != 0) || (q1 == q0){
    return Err(RnsError::InvalidParameters);
};
        let mut delta = vec![(0 as u64); n];
        for c in delta.iter_mut(){
    let r = random.next_u32() % 3;
    *c = match r {
    .. => 0,
    .. => 1,
    _ => (p - 1),
};
};
        delta[0] = 1;
        let dq0 = to_limb(&delta, p, q0);
        let dq1 = to_limb(&delta, p, q1);
        let u0 = uniform_limb(n, q0, random);
        let u1 = uniform_limb(n, q1, random);
        let ud0 = mul_limb(&u0, &dq0, q0, &ntt_ctx(n, q0));
        let ud1 = mul_limb(&u1, &dq1, q1, &ntt_ctx(n, q1));
        let v0 = add_limb(&negate_limb(&ud0, q0), &noise_limb(n, p, q0, random), q0);
        let v1 = add_limb(&negate_limb(&ud1, q1), &noise_limb(n, p, q1, random), q1);
        let d2q0 = mul_limb(&dq0, &dq0, q0, &ntt_ctx(n, q0));
        let d2q1 = mul_limb(&dq1, &dq1, q1, &ntt_ctx(n, q1));
        let ud20 = mul_limb(&u0, &d2q0, q0, &ntt_ctx(n, q0));
        let ud21 = mul_limb(&u1, &d2q1, q1, &ntt_ctx(n, q1));
        let w0 = add_limb(&sub_limb(&negate_limb(&dq0, q0), &ud20, q0), &noise_limb(n, p, q0, random), q0);
        let w1 = add_limb(&sub_limb(&negate_limb(&dq1, q1), &ud21, q1), &noise_limb(n, p, q1, random), q1);
        let red = |limb| {
    limb.iter().map(|c| (c % p)).collect::<Vec<_>>()
};
        let triple_rp = (red(&u0), red(&v0), red(&w0));
        let delta_pinned = DeltaLoweredDyn { coeffs: delta.clone(), p: p, n: 0 };
        Ok(Self { n: n, p: p, q0: q0, q1: q1, delta: delta, delta_pinned: delta_pinned, triple: (Rns2 { l0: u0, l1: u1 }, Rns2 { l0: v0, l1: v1 }, Rns2 { l0: w0, l1: w1 }), triple_rp: triple_rp, ntt0: ring::Ntt::new(n, q0).ok(), ntt1: ring::Ntt::new(n, q1).ok(), ntt_p: ring::Ntt::new(n, p).ok() })
    }
    pub fn degree(&self) -> usize
    {
        self.n
    }
    pub fn p(&self) -> u64
    {
        self.p
    }
    pub fn modulus_bits(&self) -> u32
    {
        ((64 - self.q0.leading_zeros()) + (64 - self.q1.leading_zeros())) - 2
    }
    pub fn delta(&self) -> &LabelPoly
    {
        &self.delta
    }
    pub fn delta_pinned_ref(&self) -> &Delta
    {
        &self.delta_pinned
    }
    pub fn row(&self, mut t: i64, mut random: &mut impl SpecRng) -> RnsRowCt
    {
        let (p, q0, q1) = (self.p, self.q0, self.q1);
        let u2 = Rns2 { l0: uniform_limb(self.n, q0, random), l1: uniform_limb(self.n, q1, random) };
        let dq0 = to_limb(&self.delta, p, q0);
        let dq1 = to_limb(&self.delta, p, q1);
        let u2d0 = mul_limb(&u2.l0, &dq0, q0, &self.ntt0);
        let u2d1 = mul_limb(&u2.l1, &dq1, q1, &self.ntt1);
        let td0 = scale_limb(&dq0, t, q0);
        let td1 = scale_limb(&dq1, t, q1);
        let e0 = noise_limb(self.n, p, q0, random);
        let e1 = noise_limb(self.n, p, q1, random);
        RnsRowCt { u1: Rns2 { l0: add_limb(&sub_limb(&td0, &u2d0, q0), &e0, q0), l1: add_limb(&sub_limb(&td1, &u2d1, q1), &e1, q1) }, u2: u2 }
    }
    pub fn dec_row(&self, mut tau: &RnsRowCt) -> LabelPoly
    {
        let q0 = self.q0;
        let dq0 = to_limb(&self.delta, self.p, q0);
        let u2d0 = mul_limb(&tau.u2.l0, &dq0, q0, &self.ntt0);
        let phase0 = add_limb(&tau.u1.l0, &u2d0, q0);
        round_limb0_to_rp(&phase0, q0, self.p)
    }
    pub fn dd0(&self, mut a: &LabelPoly, mut tau: &RnsRowCt) -> LabelPoly
    {
        let (p, q0) = (self.p, self.q0);
        let a_q0: Limb = a.iter().map(|c| (c % q0)).collect::<Vec<_>>();
        let prod0 = mul_limb(&tau.u2.l0, &a_q0, q0, &self.ntt0);
        round_limb0_to_rp(&prod0, q0, p)
    }
    pub fn dd_i(&self, mut a: &LabelPoly, mut i: bool, mut tau: &RnsRowCt) -> LabelPoly
    {
        let (p, q0) = (self.p, self.q0);
        let a_q0: Limb = a.iter().map(|c| (c % q0)).collect::<Vec<_>>();
        let mut acc0 = mul_limb(&tau.u2.l0, &a_q0, q0, &self.ntt0);
        if i{
    acc0 = add_limb(&acc0, &tau.u1.l0, q0);
};
        round_limb0_to_rp(&acc0, q0, p)
    }
    pub fn hss_mul_rp(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool) -> LabelPoly
    {
        let p = self.p;
        let (u, v, w) = &self.triple_rp;
        let ab = self.mul_p(a, b);
        let mut out = self.mul_p(&ab, u);
        if i{
    let bv = self.mul_p(b, v);
    out = ring::add_coeffs(&out, &bv, p);
};
        if j{
    let av = self.mul_p(a, v);
    out = ring::add_coeffs(&out, &av, p);
};
        if i && j{
    out = ring::sub_coeffs(&out, w, p);
};
        out
    }
    pub fn mul_p(&self, mut a: &[u64], mut b: &[u64]) -> LabelPoly
    {
        match &self.ntt_p {
    Some(ntt) => ring::negacyclic_mul_ntt(a, b, ntt),
    None => ring::negacyclic_mul_coeffs(a, b, self.p),
}
    }
    pub fn dd_ij(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut tau: &RnsRowCt) -> LabelPoly
    {
        let (p, q0) = (self.p, self.q0);
        let share = self.hss_mul_rp(a, i, b, j);
        let share_q0: Limb = share.iter().map(|c| (c % q0)).collect::<Vec<_>>();
        let mut acc0 = mul_limb(&tau.u2.l0, &share_q0, q0, &self.ntt0);
        if i && j{
    acc0 = add_limb(&acc0, &tau.u1.l0, q0);
};
        round_limb0_to_rp(&acc0, q0, p)
    }
    pub fn eval(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut taus: &[RnsRowCt; 3]) -> LabelPoly
    {
        let p = self.p;
        let mut out = self.dd_i(a, i, &taus[0]);
        out = ring::add_coeffs(&out, &self.dd_i(b, j, &taus[1]), p);
        ring::add_coeffs(&out, &self.dd_ij(a, i, b, j, &taus[2]), p)
    }
}

impl  DdGadget for LatticeRnsDd {
    fn p(&self) -> u64
    {
        self.p
    }
    fn delta(&self) -> &Delta
    {
        &self.delta_pinned
    }
    fn row(&self, mut t: i64, mut random: &mut impl SpecRng) -> RowCt
    {
        let tau = LatticeRnsDd::row(self, t, random);
        RowCt { c0: tau.u1.l0.iter().map(|c| (c % self.p)).collect::<Vec<_>>(), c1: tau.u2.l0.iter().map(|c| (c % self.p)).collect::<Vec<_>>() }
    }
    fn dd0(&self, mut a: &LabelPoly, mut tau: &RowCt) -> LabelPoly
    {
        self.mul_p(&tau.c1, a)
    }
    fn dd_i(&self, mut a: &LabelPoly, mut i: bool, mut tau: &RowCt) -> LabelPoly
    {
        let base = DdGadget::dd0(self, a, tau);
        if i{
    ring::add_coeffs(&base, &tau.c0, self.p)
} else {
    base
}
    }
    fn dd_ij(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut tau: &RowCt) -> LabelPoly
    {
        let share = self.hss_mul_rp(a, i, b, j);
        let base = self.mul_p(&tau.c1, &share);
        if i && j{
    ring::add_coeffs(&base, &tau.c0, self.p)
} else {
    base
}
    }
    fn eval(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut taus: &[RowCt; 3]) -> LabelPoly
    {
        let mut out = DdGadget::dd_i(self, a, i, &taus[0]);
        out = ring::add_coeffs(&out, &DdGadget::dd_i(self, b, j, &taus[1]), self.p);
        ring::add_coeffs(&out, &DdGadget::dd_ij(self, a, i, b, j, &taus[2]), self.p)
    }
}

impl  Variant {
    pub fn bits_per_and(self) -> usize
    {
        match self {
    .. => 1,
    _ => 5,
}
    }
    pub fn bits_per_xor(self) -> usize
    {
        match self {
    _ => 1,
    .. => 0,
}
    }
}

impl  Delta {
    pub fn from_ternary(mut coeffs: &[i64], mut p: u64) -> Self
    {
        let mut c: Vec<u64> = coeffs.iter().map(|v| if v < 0{
    p - 1
} else {
    (v as u64) % p
}).collect::<Vec<_>>();
        if c.is_empty(){
    c.push(1);
};
        c[0] = 1;
        Self { coeffs: c, p: p }
    }
    pub fn neg(&self) -> LabelPoly
    {
        self.coeffs.iter().map(|c| if c == 0{
    0
} else {
    self.p - c
}).collect::<Vec<_>>()
    }
}

impl  WireLabel {
    pub fn base(&self, mut delta: &Delta) -> LabelPoly
    {
        if self.pi{
    ring::add_coeffs(&self.w0, &delta.coeffs, delta.p)
} else {
    self.w0.clone()
}
    }
    pub fn active(&self, mut masked: bool, mut delta: &Delta) -> LabelPoly
    {
        let base = self.base(delta);
        if masked{
    ring::add_coeffs(&base, &delta.coeffs, delta.p)
} else {
    base
}
    }
    pub fn w0_lsb(&self) -> bool
    {
        (self.w0[0] % 2) == 1
    }
}

impl  ReferenceDd {
    pub fn new(mut delta: Delta, mut degree: usize, mut random: &mut impl FnMut) -> Self
    {
        let p = delta.p;
        let mut uniform = || {
    let mut v = vec![(0 as u64); degree];
    random(&mut v);
    v.iter_mut().for_each(|x| *x %= p);
    v
};
        let u = uniform();
        let du = ring::negacyclic_mul_coeffs(&delta.coeffs, &u, p);
        let v = du.iter().map(|x| if x == 0{
    0
} else {
    p - x
}).collect::<Vec<_>>();
        let d2 = ring::negacyclic_mul_coeffs(&delta.coeffs, &delta.coeffs, p);
        let d2u = ring::negacyclic_mul_coeffs(&d2, &u, p);
        let w = ring::sub_coeffs(&delta.neg(), &d2u, p);
        Self { p: p, delta: delta, triple: (u, v, w) }
    }
    pub fn p(&self) -> u64
    {
        self.p
    }
    pub fn delta(&self) -> &Delta
    {
        &self.delta
    }
    pub fn row(&self, mut t: i64, mut random: &mut impl FnMut) -> RowCt
    {
        let p = self.p;
        let degree = self.delta.coeffs.len();
        let mut c1 = vec![(0 as u64); degree];
        random(&mut c1);
        c1.iter_mut().for_each(|x| *x %= p);
        let td = if t >= 0{
    delta_scaled(&self.delta, (t as u64), p)
} else {
    delta_scaled(&self.delta, (-t as u64), p).iter().map(|x| if x == 0{
    0
} else {
    p - x
}).collect::<Vec<_>>()
};
        let c1d = ring::negacyclic_mul_coeffs(&c1, &self.delta.coeffs, p);
        let c0 = ring::sub_coeffs(&td, &c1d, p);
        RowCt { c0: c0, c1: c1 }
    }
    pub fn dec_row(&self, mut tau: &RowCt) -> LabelPoly
    {
        let c1d = ring::negacyclic_mul_coeffs(&tau.c1, &self.delta.coeffs, self.p);
        ring::add_coeffs(&tau.c0, &c1d, self.p)
    }
    pub fn dd0(&self, mut a: &LabelPoly, mut tau: &RowCt) -> LabelPoly
    {
        ring::negacyclic_mul_coeffs(&tau.c1, a, self.p)
    }
    pub fn dd_i(&self, mut a: &LabelPoly, mut i: bool, mut tau: &RowCt) -> LabelPoly
    {
        let base = self.dd0(a, tau);
        if i{
    ring::add_coeffs(&base, &tau.c0, self.p)
} else {
    base
}
    }
    pub fn hss_mul(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool) -> LabelPoly
    {
        let p = self.p;
        let (u, v, w) = &self.triple;
        let ab = ring::negacyclic_mul_coeffs(a, b, p);
        let mut out = ring::negacyclic_mul_coeffs(&ab, u, p);
        if i{
    let bv = ring::negacyclic_mul_coeffs(b, v, p);
    out = ring::add_coeffs(&out, &bv, p);
};
        if j{
    let av = ring::negacyclic_mul_coeffs(a, v, p);
    out = ring::add_coeffs(&out, &av, p);
};
        if i && j{
    out = ring::sub_coeffs(&out, w, p);
};
        out
    }
    pub fn dd_ij(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut tau: &RowCt) -> LabelPoly
    {
        let share = self.hss_mul(a, i, b, j);
        let base = ring::negacyclic_mul_coeffs(&tau.c1, &share, self.p);
        if i && j{
    ring::add_coeffs(&base, &tau.c0, self.p)
} else {
    base
}
    }
    pub fn eval(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut taus: &[RowCt; 3]) -> LabelPoly
    {
        let mut out = self.dd_i(a, i, &taus[0]);
        out = ring::add_coeffs(&out, &self.dd_i(b, j, &taus[1]), self.p);
        ring::add_coeffs(&out, &self.dd_ij(a, i, b, j, &taus[2]), self.p)
    }
}

impl  DdGadget for ReferenceDd {
    fn p(&self) -> u64
    {
        self.p
    }
    fn delta(&self) -> &Delta
    {
        &self.delta
    }
    fn row(&self, mut t: i64, mut random: &mut impl SpecRng) -> RowCt
    {
        self.row(t, &mut |buf| {
    for v in buf.iter_mut(){
    *v = ((random.next_u32() as u64) % self.p);
}
})
    }
    fn dd0(&self, mut a: &LabelPoly, mut tau: &RowCt) -> LabelPoly
    {
        self.dd0(a, tau)
    }
    fn dd_i(&self, mut a: &LabelPoly, mut i: bool, mut tau: &RowCt) -> LabelPoly
    {
        self.dd_i(a, i, tau)
    }
    fn dd_ij(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut tau: &RowCt) -> LabelPoly
    {
        self.dd_ij(a, i, b, j, tau)
    }
    fn eval(&self, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut taus: &[RowCt; 3]) -> LabelPoly
    {
        self.eval(a, i, b, j, taus)
    }
}

impl  GateRecord {
    pub fn bits(&self) -> usize
    {
        match self {
    .. => 1,
    Self::FastAnd { .. } => 5,
    _ => 0,
}
    }
}

impl  PrfKey {
    pub fn offset(&self, mut wire: usize, mut degree: usize, mut p: u64) -> LabelPoly
    {
        let mut out = vec![(0 as u64); degree];
        let mut s0 = self.0[0] ^ (wire as u64).wrapping_mul(11400714819323198485);
        let mut s1 = self.0[1].wrapping_add((wire as u64));
        let mut s2 = self.0[2] ^ self.0[0];
        let mut s3 = self.0[3].wrapping_add(self.0[1]);
        for c in out.iter_mut(){
    let t = s1 << 17;
    s2 ^= s0;
    s3 ^= s1;
    s1 ^= s2;
    s0 ^= s3;
    s2 ^= t;
    s3 = s3.rotateleft(45);
    *c = (s3.wrapping_add(s0) % p);
};
        out
    }
}

impl  ReferencePrg {
    pub fn new(mut slots: usize) -> Self
    {
        Self { slots: slots }
    }
}

impl  ExpansionPrg for ReferencePrg {
    fn depth(&self) -> usize
    {
        1
    }
    fn slots(&self) -> usize
    {
        self.slots
    }
    fn expand(&self, mut ctx: &Context, mut seed: &Ciphertext, mut ksk: &KeySwitchKeySet) -> Result<Vec<Ciphertext>, Error>
    {
        let degree = ctx.parameters().degree;
        if self.slots > degree{
    return Err(Error::InvalidParameters);
};
        let s2 = ctx.mul(seed, seed, ksk)?;
        let mut out = Vec::with_capacity(self.slots);
        for i in 0.. self.slots{
    let mut mask = vec![(0 as u64); degree];
    mask[i] = 1;
    out.push(ctx.mul_plain(&s2, &mask)?);
};
        Ok(out)
    }
}

impl  ExpansionPlan {
    pub fn plan(mut required_bits: usize, mut prg_depth: usize, mut slots_per_seed: usize, mut depth_budget: usize) -> Result<Self, Error>
    {
        if (slots_per_seed == 0) || (prg_depth > depth_budget){
    return Err(Error::InvalidParameters);
};
        let seed_ciphertexts = required_bits.div_ceil(slots_per_seed);
        Ok(Self { seed_ciphertexts: seed_ciphertexts, slots_per_seed: slots_per_seed, depth: prg_depth, required_bits: required_bits })
    }
    pub fn total_slots(&self) -> usize
    {
        self.seed_ciphertexts * self.slots_per_seed
    }
}

impl  Parameters {
    pub fn validate(self) -> Result<(), Error>
    {
        if (self.degree < 2) || !self.degree.is_power_of_two() || (self.modulus < 3){
    return Err(Error::InvalidParameters);
};
        if ((self.modulus - 1) % (2 * (self.degree as u64))) != 0{
    return Err(Error::InvalidParameters);
};
        Ok(())
    }
}

impl  Poly {
    pub fn slots(&self) -> &[u64]
    {
        &self.slots
    }
}

impl  Ring {
    pub fn new(mut parameters: Parameters) -> Result<Self, Error>
    {
        parameters.validate()?;
        Ok(Self { ntt: Ntt::new(parameters.degree, parameters.modulus)?, parameters: parameters })
    }
    pub fn parameters(&self) -> Parameters
    {
        self.parameters
    }
    pub fn zero(&self) -> Poly
    {
        PolyLoweredDyn { slots: vec![0; self.parameters.degree], n: 0 }
    }
    pub fn constant(&self, mut value: u64) -> Poly
    {
        PolyLoweredDyn { slots: vec![(value % self.parameters.modulus); self.parameters.degree], n: 0 }
    }
    pub fn from_slots(&self, mut slots: &[u64]) -> Result<Poly, Error>
    {
        if slots.len() != self.parameters.degree{
    return Err(Error::LengthMismatch);
};
        let q = self.parameters.modulus;
        Ok(PolyLoweredDyn { slots: slots.iter().map(|v| (v % q)).collect::<Vec<_>>(), n: 0 })
    }
    pub fn uniform(&self, mut random: &mut impl FnMut(&[u8]) -> Result<(), Error>) -> Result<Poly, Error>
    {
        let mut value = self.zero();
        sample_uniform(random, self.parameters.modulus, &mut value.slots)?;
        Ok(value)
    }
    pub fn to_coeffs(&self, mut value: &Poly) -> Vec<u64>
    {
        let mut coeffs = value.slots.clone();
        self.ntt.inverse(&mut coeffs);
        coeffs
    }
    pub fn from_coeffs(&self, mut coeffs: &[u64]) -> Result<Poly, Error>
    {
        if coeffs.len() != self.parameters.degree{
    return Err(Error::LengthMismatch);
};
        let mut slots = coeffs.to_vec();
        self.ntt.forward(&mut slots);
        Ok(PolyLoweredDyn { slots: slots, n: 0 })
    }
    pub fn add(&self, mut left: &Poly, mut right: &Poly) -> Poly
    {
        let mut out = left.clone();
        self.add_assign(&mut out, right);
        out
    }
    pub fn sub(&self, mut left: &Poly, mut right: &Poly) -> Poly
    {
        let mut out = left.clone();
        self.sub_assign(&mut out, right);
        out
    }
    pub fn mul(&self, mut left: &Poly, mut right: &Poly) -> Poly
    {
        let q = self.parameters.modulus;
        PolyLoweredDyn { slots: left.slots.iter().zip(right.slots.iter()).map(|(l, r)| mul_mod(l, r, q)).collect::<Vec<_>>(), n: 0 }
    }
    pub fn scalar_mul(&self, mut value: &Poly, mut scalar: u64) -> Poly
    {
        let q = self.parameters.modulus;
        let scalar = scalar % q;
        PolyLoweredDyn { slots: value.slots.iter().map(|v| mul_mod(v, scalar, q)).collect::<Vec<_>>(), n: 0 }
    }
    pub fn add_assign(&self, mut left: &mut Poly, mut right: &Poly)
    {
        let q = self.parameters.modulus;
        for (l, ..) in left.slots.iter_mut().zip(right.slots.iter()){
    *l = add_mod(*l, r, q);
}
    }
    pub fn sub_assign(&self, mut left: &mut Poly, mut right: &Poly)
    {
        let q = self.parameters.modulus;
        for (l, ..) in left.slots.iter_mut().zip(right.slots.iter()){
    *l = sub_mod(*l, r, q);
}
    }
    pub fn gadget_decompose_coeffs(&self, mut coeffs: &[u64], mut base: u64, mut digits: usize) -> Result<Vec<Vec<u64>>, Error>
    {
        if (coeffs.len() != self.parameters.degree) || !base.is_power_of_two(){
    return Err(Error::LengthMismatch);
};
        let mut out = vec![vec![(0 as u64); self.parameters.degree]; digits];
        let bits = base.trailing_zeros() as usize;
        for (i, ..) in coeffs.iter().enumerate(){
    for d in 0.. digits{
    out[d][i] = ((c >> (bits * d)) & (base - 1));
};
};
        Ok(out)
    }
    pub fn negate_assign(&self, mut value: &mut Poly)
    {
        let q = self.parameters.modulus;
        for v in value.slots.iter_mut(){
    if *v != 0{
    *v = (q - *v);
};
}
    }
}

impl  Ntt {
    pub fn new(mut degree: usize, mut modulus: u64) -> Result<Self, Error>
    {
        if (degree == 0) || !degree.is_power_of_two() || (((modulus - 1) % (2 * (degree as u64))) != 0){
    return Err(Error::Arithmetic);
};
        let psi = find_negacyclic_root(degree, modulus).ok_or(Error::Arithmetic)?;
        let omega = mul_mod(psi, psi, modulus);
        let inverse_psi = inverse_mod(psi, modulus).ok_or(Error::Arithmetic)?;
        let inverse_omega = inverse_mod(omega, modulus).ok_or(Error::Arithmetic)?;
        let inverse_degree = inverse_mod((degree as u64), modulus).ok_or(Error::Arithmetic)?;
        let mut psi_powers = Vec::with_capacity(degree);
        let mut acc = 1 as u64;
        for _ in 0.. degree{
    psi_powers.push(acc);
    acc = mul_mod(acc, psi, modulus);
};
        let mut inv_psi_powers_scaled = Vec::with_capacity(degree);
        let mut acc = 1 as u64;
        for _ in 0.. degree{
    inv_psi_powers_scaled.push(mul_mod(acc, inverse_degree, modulus));
    acc = mul_mod(acc, inverse_psi, modulus);
};
        let mut fwd_twiddles = Vec::new();
        let mut inv_twiddles = Vec::new();
        let mut length = 2;
        while (length <= degree){
    fwd_twiddles.push(pow_mod(omega, (degree / length), modulus));
    inv_twiddles.push(pow_mod(inverse_omega, (degree / length), modulus));
    length *= 2;
};
        Ok(Self { modulus: modulus, degree: degree, psi_powers: psi_powers, inv_psi_powers_scaled: inv_psi_powers_scaled, fwd_twiddles: fwd_twiddles, inv_twiddles: inv_twiddles })
    }
    pub fn modulus(&self) -> u64
    {
        self.modulus
    }
    pub fn degree(&self) -> usize
    {
        self.degree
    }
    pub fn forward(&self, mut values: &mut [u64])
    {
        for (value, ..) in values.iter_mut().zip(self.psi_powers.iter()){
    *value = mul_mod(*value, s, self.modulus);
};
        self.cyclic(values, &self.fwd_twiddles);
    }
    pub fn inverse(&self, mut values: &mut [u64])
    {
        self.cyclic(values, &self.inv_twiddles);
        for (value, ..) in values.iter_mut().zip(self.inv_psi_powers_scaled.iter()){
    *value = mul_mod(*value, s, self.modulus);
}
    }
    pub fn cyclic(&self, mut values: &mut [u64], mut twiddles: &[u64])
    {
        bit_reverse(values);
        let mut length = 2;
        let mut stage = 0;
        while (length <= self.degree){
    let step = twiddles[stage];
    for start in 0..self.degree.step_by(length){
    let mut twiddle = 1;
    for offset in 0.. length / 2{
    let left = values[start + offset];
    let right = mul_mod(values[start + offset + (length / 2)], twiddle, self.modulus);
    values[start + offset] = add_mod(left, right, self.modulus);
    values[start + offset + (length / 2)] = sub_mod(left, right, self.modulus);
    twiddle = mul_mod(twiddle, step, self.modulus);
};
};
    length *= 2;
    stage += 1;
}
    }
}

impl  OfflineGarbler {
    pub fn wire_state(&self, mut ctx: &Context, mut plan: &ExpansionPlan, mut ct_a0: &Ciphertext, mut ct_pi: &Ciphertext, mut index: usize) -> Result<WireState, Error>
    {
        let degree = ctx.parameters().degree;
        let slot = |i| ((i % plan.slots_per_seed) % degree);
        let a0 = ctx.decrypt(&self.secret, ct_a0)?[slot((2 * index))] % 2;
        let pi = (ctx.decrypt(&self.secret, ct_pi)?[slot(((2 * index) + 1))] % 2) == 1;
        let p = ctx.parameters().plaintext_modulus;
        Ok(super::common::WireState { a0: (a0 % p), pi: pi })
    }
}

impl  OfflineEvaluator {
    pub fn wire(&self, mut wire: usize) -> Option<&ExpandedWire>
    {
        self.wires.get(wire)
    }
    pub fn wires(&self) -> usize
    {
        self.wires.len()
    }
}

impl  NoiseSource for ZeroNoise {
    fn sample(&mut self, mut _bound: u64, mut output: &mut [i64]) -> Result<(), Error>
    {
        output.fill(0);
        Ok(())
    }
}

impl  From<Error> for Error {
    fn from(mut e: Error) -> Self
    {
        match e {
    _ => Self::InvalidParameters,
    _ => Self::LengthMismatch,
    _ => Self::Arithmetic,
    _ => Self::Randomness,
}
    }
}

impl  Parameters {
    pub fn validate(&self) -> Result<(), Error>
    {
        if (self.degree < 2) || !self.degree.is_power_of_two() || self.chain.is_empty() || (self.noise_bound == 0){
    return Err(Error::InvalidParameters);
};
        let n2 = 2 * (self.degree as u64);
        let p = self.plaintext_modulus;
        if (p < 3) || (((p - 1) % n2) != 0){
    return Err(Error::InvalidParameters);
};
        let mut seen = alloc::vec::Vec::new();
        for .. in &self.chain{
    if (q < 3) || (((q - 1) % n2) != 0) || (((q - 1) % p) != 0) || (q == p) || seen.contains(&q){
    return Err(Error::InvalidParameters);
};
    seen.push(q);
};
        Ok(())
    }
}

impl  RnsPoly {
    pub fn limbs(&self) -> &[Poly]
    {
        &self.limbs
    }
}

impl  KeySwitchKeySet {
    pub fn max_level(&self) -> usize
    {
        self.keys.len() - 1
    }
    pub fn at(&self, mut level: usize) -> Option<&KeySwitchKey>
    {
        self.keys.get(level)
    }
    pub fn base(&self) -> u64
    {
        self.base
    }
}

impl  Ciphertext {
    pub fn level(&self) -> usize
    {
        self.level
    }
    pub fn parts(&self) -> &[RnsPoly]
    {
        &self.parts
    }
}

impl  Context {
    pub fn new(mut parameters: Parameters) -> Result<Self, Error>
    {
        parameters.validate()?;
        let level_rings = parameters.chain.iter().map(|q| {
    Ring::new(ring::Parameters { degree: parameters.degree, modulus: q })
}).collect::<Vec<_>>()?;
        let plaintext_ring = Ring::new(ring::Parameters { degree: parameters.degree, modulus: parameters.plaintext_modulus })?;
        Ok(Self { parameters: parameters, level_rings: level_rings, plaintext_ring: plaintext_ring })
    }
    pub fn parameters(&self) -> &Parameters
    {
        &self.parameters
    }
    pub fn constant_slots(&self, mut value: u64) -> Vec<u64>
    {
        vec![(value % self.parameters.plaintext_modulus); self.parameters.degree]
    }
    pub fn max_level(&self) -> usize
    {
        self.parameters.chain.len() - 1
    }
    pub fn limbs(&self, mut level: usize) -> usize
    {
        level + 1
    }
    pub fn check_level(&self, mut level: usize) -> Result<(), Error>
    {
        if level > self.max_level(){
    return Err(Error::InvalidParameters);
};
        Ok(())
    }
    pub fn zero(&self, mut level: usize) -> RnsPoly
    {
        RnsPoly { limbs: self.level_rings[..=level].iter().map(Ring::zero).collect() }
    }
    pub fn add_rns(&self, mut left: &RnsPoly, mut right: &RnsPoly) -> RnsPoly
    {
        RnsPoly { limbs: left.limbs.iter().zip(right.limbs.iter()).enumerate().map(|(i, (l, r))| self.level_rings[i].add(l, r)).collect::<Vec<_>>() }
    }
    pub fn mul_rns(&self, mut left: &RnsPoly, mut right: &RnsPoly) -> RnsPoly
    {
        RnsPoly { limbs: left.limbs.iter().zip(right.limbs.iter()).enumerate().map(|(i, (l, r))| self.level_rings[i].mul(l, r)).collect::<Vec<_>>() }
    }
    pub fn scalar_mul_rns(&self, mut value: &RnsPoly, mut scalar: u64) -> RnsPoly
    {
        RnsPoly { limbs: value.limbs.iter().enumerate().map(|(i, l)| self.level_rings[i].scalar_mul(l, scalar)).collect::<Vec<_>>() }
    }
    pub fn uniform_rns(&self, mut level: usize, mut random: &mut impl RandomSource) -> Result<RnsPoly, Error>
    {
        let mut limbs = Vec::with_capacity(self.limbs(level));
        for ring in &self.level_rings[..=level]{
    let mut slots = vec![(0 as u64); self.parameters.degree];
    let modulus = ring.parameters().modulus;
    let mut rng = |out| {
    random.fill_bytes(out).map_err(|_| ring::Error::Randomness)
};
    ring::sample_uniform(&mut rng, modulus, &mut slots)?;
    limbs.push(ring.from_slots(&slots)?);
};
        Ok(RnsPoly { limbs: limbs })
    }
    pub fn noise_rns(&self, mut level: usize, mut noise: &mut impl NoiseSource) -> Result<RnsPoly, Error>
    {
        let mut coeffs_i = vec![(0 as i64); self.parameters.degree];
        noise.sample(&mut coeffs_i.n, self.parameters.noise_bound, &mut coeffs_i)?;
        if coeffs_i.iter().any(|v| (v.unsigned_abs() > self.parameters.noise_bound)){
    return Err(Error::NoiseSource);
};
        let mut limbs = Vec::with_capacity(self.limbs(level));
        for ring in &self.level_rings[..=level]{
    let q = ring.parameters().modulus;
    let coeffs: Vec<u64> = coeffs_i.iter().map(|v| {
    if v < 0{
    q - (v.unsigned_abs() % q)
} else {
    (v as u64) % q
}
}).collect::<Vec<_>>();
    limbs.push(ring.from_coeffs(&coeffs)?);
};
        Ok(RnsPoly { limbs: limbs })
    }
    pub fn embed(&self, mut level: usize, mut slots: &[u64]) -> Result<RnsPoly, Error>
    {
        if slots.len() != self.parameters.degree{
    return Err(Error::LengthMismatch);
};
        if (level == 0) && slots.iter().all(|..| (v == 0)){
    return Err(Error::InvalidParameters);
};
        let p = self.parameters.plaintext_modulus;
        if slots.iter().any(|..| (v >= p)){
    return Err(Error::InvalidParameters);
};
        let packed = self.plaintext_ring.from_slots(slots)?;
        let m_coeffs = self.plaintext_ring.to_coeffs(&packed);
        let mut limbs = Vec::with_capacity(self.limbs(level));
        for ring in &self.level_rings[..=level]{
    let q = ring.parameters().modulus;
    let coeffs: Vec<u64> = m_coeffs.iter().map(|c| (c % q)).collect::<Vec<_>>();
    limbs.push(ring.from_coeffs(&coeffs)?);
};
        Ok(RnsPoly { limbs: limbs })
    }
    pub fn keygen(&self, mut random: &mut impl RandomSource) -> Result<SecretKey, Error>
    {
        let mut coeffs = vec![(0 as i64); self.parameters.degree];
        let mut byte = [(0 as u8); 1];
        for c in coeffs.iter_mut(){
    random.fill_bytes(&mut byte[..])?;
    *c = (((byte[0] % 3) as i64) - 1);
};
        Ok(SecretKey { coeffs: coeffs })
    }
    pub fn encrypt(&self, mut sk: &SecretKey, mut level: usize, mut slots: &[u64], mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<Ciphertext, Error>
    {
        self.check_level(level)?;
        if sk.coeffs.len() != self.parameters.degree{
    return Err(Error::LengthMismatch);
};
        let p = self.parameters.plaintext_modulus;
        let m = self.embed(level, slots)?;
        let a = self.uniform_rns(level, random)?;
        let e = self.noise_rns(level, noise)?;
        let s = self.secret_rns(level, sk);
        let mut c0 = self.mul_rns(&a, &s);
        let pe = self.scalar_mul_rns(&e, p);
        self.add_assign_rns(&mut c0, &m);
        self.add_assign_rns(&mut c0, &pe);
        let mut c1 = a;
        for (i, limb) in c1.limbs.iter_mut().enumerate(){
    self.level_rings[i].negate_assign(limb);
};
        Ok(Ciphertext { level: level, parts: vec![c0, c1] })
    }
    pub fn add_assign_rns(&self, mut left: &mut RnsPoly, mut right: &RnsPoly)
    {
        for (i, (l, r)) in left.limbs.iter_mut().zip(right.limbs.iter()).enumerate(){
    self.level_rings[i].add_assign(l, r);
}
    }
    pub fn secret_rns(&self, mut level: usize, mut sk: &SecretKey) -> RnsPoly
    {
        let limbs = self.level_rings[..=level].iter().map(|ring| {
    let q = ring.parameters().modulus;
    let coeffs: Vec<u64> = sk.coeffs.iter().map(|v| {
    if v < 0{
    q - (v.unsigned_abs() % q)
} else {
    (v as u64) % q
}
}).collect::<Vec<_>>();
    ring.from_coeffs(&coeffs).expect("degree checked at keygen use")
}).collect::<Vec<_>>();
        RnsPoly { limbs: limbs }
    }
    pub fn decrypt(&self, mut sk: &SecretKey, mut ct: &Ciphertext) -> Result<Vec<u64>, Error>
    {
        if sk.coeffs.len() != self.parameters.degree{
    return Err(Error::LengthMismatch);
};
        let p = self.parameters.plaintext_modulus;
        let s = self.secret_rns(ct.level, sk);
        let coeffs = self.level_rings[0].to_coeffs(&{
    let ring0 = &self.level_rings[0];
    let mut limb = ring0.mul(&ct.parts[1].limbs[0], &s.limbs[0]);
    ring0.add_assign(&mut limb, &ct.parts[0].limbs[0]);
    limb
});
        let q0 = self.parameters.chain[0];
        let m_coeffs: Vec<u64> = coeffs.iter().map(|c| (ring::center(c, q0).rem_euclid((p as i128)) as u64)).collect::<Vec<_>>();
        let packed = self.plaintext_ring.from_coeffs(&m_coeffs)?;
        Ok(packed.slots().to_vec())
    }
    pub fn add(&self, mut left: &Ciphertext, mut right: &Ciphertext) -> Result<Ciphertext, Error>
    {
        let (left_owned, right_owned);
        let mut l = left;
        let mut r = right;
        if l.level > r.level{
    left_owned = self.switch_to(l, r.level)?;
    l = &left_owned;
} else if r.level > l.level{
    right_owned = self.switch_to(r, l.level)?;
    r = &right_owned;
};
        let parts = l.parts.iter().zip(r.parts.iter()).map(|(a, b)| self.add_rns(a, b)).collect::<Vec<_>>();
        Ok(Ciphertext { level: l.level, parts: parts })
    }
    pub fn sub(&self, mut left: &Ciphertext, mut right: &Ciphertext) -> Result<Ciphertext, Error>
    {
        let (left_owned, right_owned);
        let mut l = left;
        let mut r = right;
        if l.level > r.level{
    left_owned = self.switch_to(l, r.level)?;
    l = &left_owned;
} else if r.level > l.level{
    right_owned = self.switch_to(r, l.level)?;
    r = &right_owned;
};
        let mut neg = r.clone();
        for part in neg.parts.iter_mut(){
    for (i, limb) in part.limbs.iter_mut().enumerate(){
    self.level_rings[i].negate_assign(limb);
};
};
        self.add(l, &neg)
    }
    pub fn switch_to(&self, mut ct: &Ciphertext, mut target: usize) -> Result<Ciphertext, Error>
    {
        let mut out = ct.clone();
        while (out.level > target){
    out = self.mod_switch_down(&out)?;
};
        Ok(out)
    }
    pub fn add_plain(&self, mut ct: &Ciphertext, mut slots: &[u64]) -> Result<Ciphertext, Error>
    {
        let m = self.embed(ct.level, slots)?;
        let mut parts = ct.parts.clone();
        self.add_assign_rns(&mut parts[0], &m);
        Ok(Ciphertext { level: ct.level, parts: parts })
    }
    pub fn encrypt_one_like(&self, mut like: &Ciphertext) -> Result<Ciphertext, Error>
    {
        let mut one = self.zero(like.level.n, like.level);
        for (i, limb) in one.limbs.iter_mut().enumerate(){
    *limb = self.level_rings[i].constant(1.n, 1);
};
        Ok(Ciphertext { level: like.level, parts: vec![one, self.zero(like.level.n, like.level)] })
    }
    pub fn mul_plain(&self, mut ct: &Ciphertext, mut slots: &[u64]) -> Result<Ciphertext, Error>
    {
        let m = self.embed(ct.level, slots)?;
        let parts = ct.parts.iter().map(|c| self.mul_rns(c, &m)).collect::<Vec<_>>();
        Ok(Ciphertext { level: ct.level, parts: parts })
    }
    pub fn mul(&self, mut left: &Ciphertext, mut right: &Ciphertext, mut ksk: &KeySwitchKeySet) -> Result<Ciphertext, Error>
    {
        let (left_owned, right_owned);
        let mut l = left;
        let mut r = right;
        if l.level > r.level{
    left_owned = self.switch_to(l, r.level)?;
    l = &left_owned;
} else if r.level > l.level{
    right_owned = self.switch_to(r, l.level)?;
    r = &right_owned;
};
        let (left, right) = (l, r);
        let level = left.level;
        let ksk = ksk.at(level).ok_or(Error::LevelMismatch)?;
        if ksk.level != level{
    return Err(Error::LevelMismatch);
};
        let t0 = self.mul_rns(&left.parts[0], &right.parts[0]);
        let mut t1 = self.mul_rns(&left.parts[0], &right.parts[1]);
        let t1b = self.mul_rns(&left.parts[1], &right.parts[0]);
        self.add_assign_rns_mut(&mut t1, &t1b);
        let t2 = self.mul_rns(&left.parts[1], &right.parts[1]);
        let mut c0 = t0;
        let mut c1 = t1;
        let t2_coeffs: Vec<Vec<u64>> = t2.limbs.iter().enumerate().map(|(i, limb)| self.level_rings[i].to_coeffs(limb)).collect::<Vec<_>>();
        let base_bits = ksk.base.trailing_zeros() as usize;
        if !ksk.base.is_power_of_two() || (ksk.base < 2){
    return Err(Error::InvalidParameters);
};
        let _ = base_bits;
        for (digit_index, (neg_a, b)) in ksk.parts.iter().enumerate(){
    let shift = base_bits * digit_index;
    let mut digit = self.zero(level.n, level);
    if shift < 64{
    for (i, ring_i) in self.level_rings[..=level].iter().enumerate(){
    let coeffs: Vec<u64> = t2_coeffs[i].iter().map(|c| ((c >> shift) & (ksk.base - 1))).collect::<Vec<_>>();
    digit.limbs[i] = ring_i.from_coeffs(&coeffs)?;
}
};
    self.add_assign_rns_mut(&mut c0, &self.mul_rns(&digit, b));
    self.add_assign_rns_mut(&mut c1, &self.mul_rns(&digit, neg_a));
};
        let product = Ciphertext { level: level, parts: vec![c0, c1] };
        if level == 0{
    Ok(product)
} else {
    self.mod_switch_down(&product)
}
    }
    pub fn add_assign_rns_mut(&self, mut left: &mut RnsPoly, mut right: &RnsPoly)
    {
        self.add_assign_rns(left, right);
    }
    pub fn key_switch_keyset(&self, mut sk: &SecretKey, mut base: u64, mut digits: usize, mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<KeySwitchKeySet, Error>
    {
        let mut keys = Vec::with_capacity((self.max_level() + 1));
        for level in 0..= self.max_level(){
    keys.push(self.key_switch_keygen(sk, level, base, digits, random, noise)?);
};
        Ok(KeySwitchKeySet { keys: keys, base: base })
    }
    pub fn key_switch_keygen(&self, mut sk: &SecretKey, mut level: usize, mut base: u64, mut digits: usize, mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<KeySwitchKey, Error>
    {
        self.check_level(level)?;
        if !base.is_power_of_two() || (base < 2) || (digits == 0){
    return Err(Error::InvalidParameters);
};
        if ((base.trailing_zeros() as usize) * digits) < 64{
    return Err(Error::InvalidParameters);
};
        let p = self.parameters.plaintext_modulus;
        let s = self.secret_rns(level, sk);
        let s2 = self.mul_rns(&s, &s);
        let mut parts = Vec::with_capacity(digits);
        for i in 0.. digits{
    let a = self.uniform_rns(level, random)?;
    let e = self.noise_rns(level, noise)?;
    let mut b = self.mul_rns(&a, &s);
    let pe = self.scalar_mul_rns(&e, p);
    self.add_assign_rns(&mut b, &pe);
    let mut scaled = s2.clone();
    for (j, limb) in scaled.limbs.iter_mut().enumerate(){
    let q = self.level_rings[j].parameters().modulus;
    let factor = ring::pow_mod((base % q), i, q);
    *limb = self.level_rings[j].scalar_mul(limb, factor);
};
    self.add_assign_rns(&mut b, &scaled);
    let mut neg_a = a;
    for (j, limb) in neg_a.limbs.iter_mut().enumerate(){
    self.level_rings[j].negate_assign(limb);
};
    parts.push((neg_a, b));
};
        Ok(KeySwitchKey { level: level, base: base, parts: parts })
    }
    pub fn mod_switch_down(&self, mut ct: &Ciphertext) -> Result<Ciphertext, Error>
    {
        if (ct.level == 0) || (ct.level > self.max_level()){
    return Err(Error::LevelMismatch);
};
        let p = self.parameters.plaintext_modulus;
        let from = ct.level;
        let q_top = self.parameters.chain[from];
        let inv_p_top = ring::inverse_mod((p % q_top), q_top).ok_or(Error::Arithmetic)?;
        let parts = ct.parts.iter().map(|part| {
    let top_coeffs = self.level_rings[from].to_coeffs(&part.limbs[from]);
    let deltas: Vec<u64> = top_coeffs.iter().map(|c| ring::mul_mod(ring::mul_mod(c, inv_p_top, q_top), 1, q_top)).collect::<Vec<_>>();
    let mut limbs = Vec::with_capacity(from);
    for (i, ring_i) in self.level_rings[..from].iter().enumerate(){
    let q_i = ring_i.parameters().modulus;
    let inv_q_top = ring::inverse_mod((q_top % q_i), q_i).ok_or(Error::Arithmetic)?;
    let ci = ring_i.to_coeffs(&part.limbs[i]);
    let p_mod_qi = p % q_i;
    let out: Vec<u64> = ci.iter().zip(deltas.iter()).map(|(c, d)| {
    let pd = ring::mul_mod(p_mod_qi, d, q_i);
    let diff = ring::sub_mod(c, pd, q_i);
    ring::mul_mod(diff, inv_q_top, q_i)
}).collect::<Vec<_>>();
    limbs.push(ring_i.from_coeffs(&out)?);
};
    Ok(RnsPoly { limbs: limbs })
}).collect::<Vec<_>>()?;
        Ok(Ciphertext { level: (from - 1), parts: parts })
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
    let i2 = i | ((j as usize) << t.ilog2());
    if bad.contains(&(i2 as u64)){
    let h = commit::<D>(&self.per_byte[ni][i2], rand);
    (0..m).map(|j| {
    AsRef::<[u8]>::as_ref(&h).get(j).cloned().unwrap_or_default()
}).collect::<Vec<u8>>()
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
    if ((a >> j) & 1) == b{
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
    if ((a >> j) & 1) == b{
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
    let i2 = i | ((b as usize) << t.ilog2());
    if self.bad.contains(&(i2 as u64)){
    h.update(&self.openings[0][i][b][..<<D>::OutputSize as Unsigned>::to_usize()]);
} else {
    h.update(&commit::<D>(&&self.openings[0][i][b][..<<B>::OutputSize as Unsigned>::to_usize()], rand));
};
};
};
        h.finalize().to_vec().as_slice() == commit_.as_slice()
    }
}

impl <B: LengthDoubler, D: Digest> ABOOpeningDyn<B, D> {
    pub fn validate<R: AsRef<[u8]>>(mut t: usize, mut u: usize, mut nothers: usize, mut this: Vec<&Self>, mut me: &ABODyn<B, D>, mut commit_: Vec<&Vec<u8>>, mut rand: &R) -> bool
    {
        commit_.iter().enumerate().all(|(ci, commit_)| {
    let mut h = D::new();
    if ci != 0{
    for i in 0.. t{
    for b in 0.. u{
    let i2 = i | ((b as usize) << t.ilog2());
    h.update(&commit::<D>(&me.per_byte[ci][i2], rand));
};
}
};
    for (idx, this) in this.iter().enumerate(){
    if (idx + 1) == ci{
    continue;
};
    for i in 0.. t{
    for b in 0.. u{
    let i2 = i | ((b as usize) << t.ilog2());
    if this.bad.contains(&(i2 as u64)){
    h.update(&this.openings[ci][i][b][..<<D>::OutputSize as Unsigned>::to_usize()]);
} else {
    h.update(&commit::<D>(&&this.openings[ci][i][b][..<<B>::OutputSize as Unsigned>::to_usize()], rand));
};
};
};
};
    h.finalize().to_vec().as_slice() == commit_.as_slice()
})
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

impl  Fe25519 {
    pub fn is_zero(&self) -> bool
    {
        self.0 == [0, 0, 0, 0]
    }
    pub fn to_bytes(self) -> [u8; 32]
    {
        let mut out = [(0 as u8); 32];
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
        (lhs_x == rhs_x) && (lhs_y == rhs_y)
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
        let mut k = [(0 as u8); 32];
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

impl  ScalarOps for Ed25519 {
    fn scalar_zero() -> [u8; 32]
    {
        [(0 as u8); 32]
    }
    fn scalar_add(mut a: &[u8; 32], mut b: &[u8; 32]) -> [u8; 32]
    {
        limbs_to_bytes(&add_mod_l(&bytes_to_limbs(a), &bytes_to_limbs(b)))
    }
    fn scalar_sub(mut a: &[u8; 32], mut b: &[u8; 32]) -> [u8; 32]
    {
        limbs_to_bytes(&sub_mod_l(&bytes_to_limbs(a), &bytes_to_limbs(b)))
    }
    fn scalar_mul_scalar(mut a: &[u8; 32], mut b: &[u8; 32]) -> [u8; 32]
    {
        let prod = mul4(&bytes_to_limbs(a), &bytes_to_limbs(b));
        limbs_to_bytes(&reduce512(&prod))
    }
    fn scalar_eq(mut a: &[u8; 32], mut b: &[u8; 32]) -> bool
    {
        let ra = reduce512(&[{
    let l = bytes_to_limbs(a);
    l[0]
}, bytes_to_limbs(a)[1], bytes_to_limbs(a)[2], bytes_to_limbs(a)[3], 0, 0, 0, 0]);
        let rb = reduce512(&[{
    let l = bytes_to_limbs(b);
    l[0]
}, bytes_to_limbs(b)[1], bytes_to_limbs(b)[2], bytes_to_limbs(b)[3], 0, 0, 0, 0]);
        cmp4(&ra, &rb) == core::cmp::Ordering::Equal
    }
    fn scalar_from_hash<D: Digest>(mut h: D) -> [u8; 32]
    {
        let out = h.finalize().to_vec();
        let mut wide = [(0 as u64); 8];
        let n = out.len().min(64);
        for (i, chunk) in out[..n].chunks(8).enumerate(){
    let mut le = [(0 as u8); 8];
    le[..chunk.len()].copy_from_slice(chunk);
    wide[i] = u64::from_le_bytes(le);
};
        limbs_to_bytes(&reduce512(&wide))
    }
    fn random_mod_order<R: SpecRng>(mut rng: &mut R) -> [u8; 32]
    {
        let mut k = [(0 as u8); 32];
        for byte in k.iter_mut(){
    *byte = rng.next_u8();
};
        k[31] &= 15;
        k
    }
}

impl <L: LeafCommit<LAMBDA_BYTES, COM_BYTES>> BavcDyn<L> {
    pub fn commit<D: Digest>(mut com_bytes: usize, mut r: [u8; LAMBDA_BYTES], mut iv: &[u8; 16], mut tau: usize, mut n: usize) -> BavcCommitmentDyn
    {
        let leaf_count = tau * n;
        let total_nodes = (2 * leaf_count) - 1;
        let mut tree: Vec<[u8; LAMBDA_BYTES]> = vec![[(0 as u8); LAMBDA_BYTES]; total_nodes];
        tree[0] = r;
        for node in 0.. leaf_count - 1{
    let parent = Vec::<u8, U16>(tree[node]);
    let .. = AesCtrLengthDoubler::double(parent);
    tree[(2 * node) + 1] = left.0;
    tree[(2 * node) + 2] = right.0;
};
        let mut seeds: Vec<[u8; LAMBDA_BYTES]> = Vec::with_capacity(leaf_count);
        let mut commitments: Vec<[u8; COM_BYTES]> = Vec::with_capacity(leaf_count);
        for i in 0.. tau{
    for j in 0.. n{
    let leaf_k = (i * n) + j;
    let tree_pos = (leaf_count - 1) + leaf_k;
    let r_leaf = tree[tree_pos];
    let tweak = leaf_k as u32;
    let (sd, com) = L::commit(&r_leaf, iv, tweak);
    seeds.push(sd);
    commitments.push(com);
};
};
        let mut vec_hashes: Vec<Vec<u8>> = Vec::with_capacity(tau);
        for i in 0.. tau{
    let mut h = D::new();
    for j in 0.. n{
    h.update(&commitments[(i * n) + j]);
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
        for i in 0.. tau{
    let d = deltas[i];
};
        let leaf_count = tau * n;
        let total_nodes = (2 * leaf_count) - 1;
        let mut hidden = vec![false; total_nodes];
        for i in 0.. tau{
    let leaf_k = (i * n) + deltas[i];
    let tree_pos = (leaf_count - 1) + leaf_k;
    hidden[tree_pos] = true;
};
        let mut node = leaf_count - 1;
        while (node > 0){
    node -= 1;
    let left = (2 * node) + 1;
    let right = (2 * node) + 2;
    hidden[node] = (hidden[left] || hidden[right]);
};
        let mut tree: Vec<[u8; LAMBDA_BYTES]> = vec![[(0 as u8); LAMBDA_BYTES]; total_nodes];
        let _ = tree;
        let mut hidden_commits: Vec<[u8; COM_BYTES]> = Vec::with_capacity(deltas.len());
        for i in 0.. deltas.len(){
    let d = deltas[i];
    hidden_commits.push(commitment.commitments[(i * n) + d]);
};
        BavcOpeningDyn { hidden_commits: hidden_commits, nodes: Vec::new(), com_bytes: 0 }
    }
    pub fn collect_open_nodes(mut com_bytes: usize, mut deltas: &[usize], mut tree: &[[u8; LAMBDA_BYTES]], mut tau: usize, mut n: usize) -> Vec<(usize, [u8; LAMBDA_BYTES])>
    {
        let leaf_count = tau * n;
        let total_nodes = (2 * leaf_count) - 1;
        let mut hidden = vec![false; total_nodes];
        for i in 0.. tau{
    let leaf_k = (i * n) + deltas[i];
    let tree_pos = (leaf_count - 1) + leaf_k;
    hidden[tree_pos] = true;
};
        let mut node = leaf_count - 1;
        while (node > 0){
    node -= 1;
    hidden[node] = (hidden[(2 * node) + 1] || hidden[(2 * node) + 2]);
};
        let mut out: Vec<(usize, [u8; LAMBDA_BYTES])> = Vec::new();
        collect_open_nodes_walk(0, &hidden, tree, leaf_count, &mut out);
        out
    }
    pub fn reconstruct<D: Digest>(mut com_bytes: usize, mut nodes: &Vec<(usize, [u8; LAMBDA_BYTES])>, mut hidden_commits: &[[u8; COM_BYTES]], mut deltas: &[usize], mut iv: &[u8; 16], mut expected_root: &[u8], mut tau: usize, mut n: usize) -> Option<Vec<[u8; LAMBDA_BYTES]>>
    {
        let leaf_count = tau * n;
        let total_nodes = (2 * leaf_count) - 1;
        let mut hidden = vec![false; total_nodes];
        for i in 0.. tau{
    let leaf_k = (i * n) + deltas[i];
    let tree_pos = (leaf_count - 1) + leaf_k;
    hidden[tree_pos] = true;
};
        let mut node = leaf_count - 1;
        while (node > 0){
    node -= 1;
    hidden[node] = (hidden[(2 * node) + 1] || hidden[(2 * node) + 2]);
};
        let mut tree: Vec<Option<[u8; LAMBDA_BYTES]>> = vec![None; total_nodes];
        for (idx, seed) in nodes{
    tree[*idx] = Some(*seed);
};
        for node in 0.. leaf_count - 1{
    match tree[node] {
    Some(parent_seed) => {
    let parent = Vec::<u8, U16>(parent_seed);
    let .. = AesCtrLengthDoubler::double(parent);
    if tree[(2 * node) + 1].is_none(){
    tree[(2 * node) + 1] = Some(left.0);
};
    if tree[(2 * node) + 2].is_none(){
    tree[(2 * node) + 2] = Some(right.0);
}
},
    _ => {
},
};
};
        let mut leaf_seeds: Vec<[u8; LAMBDA_BYTES]> = Vec::with_capacity(leaf_count);
        let mut leaf_coms: Vec<[u8; COM_BYTES]> = Vec::with_capacity(leaf_count);
        for i in 0.. tau{
    for j in 0.. n{
    let leaf_k = (i * n) + j;
    let tree_pos = (leaf_count - 1) + leaf_k;
    if j == deltas[i]{
    leaf_seeds.push([(0 as u8); LAMBDA_BYTES]);
    leaf_coms.push(hidden_commits[i]);
} else {
    let r_leaf = tree[tree_pos]?;
    let tweak = leaf_k as u32;
    let (sd, com) = L::commit(&r_leaf, iv, tweak);
    leaf_seeds.push(sd);
    leaf_coms.push(com);
};
};
};
        let mut vec_hashes: Vec<Vec<u8>> = Vec::with_capacity(tau);
        for i in 0.. tau{
    let mut h = D::new();
    for j in 0.. n{
    h.update(&leaf_coms[(i * n) + j]);
};
    vec_hashes.push(h.finalize().to_vec().to_vec());
};
        let mut h = D::new();
        for hi in &vec_hashes{
    h.update(hi);
};
        let root = h.finalize().to_vec().to_vec();
        let mut matches = root.len() == expected_root.len();
        let mut k = 0;
        while (k < root.len()){
    if root[k] != expected_root[k]{
    matches = false;
};
    k += 1;
};
        if matches{
    Some(leaf_seeds)
} else {
    None
}
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
        let mut sd = [(0 as u8); sd_bytes];
        let mut com = [(0 as u8); com_bytes];
        sd.copy_from_slice(&digest[..sd_bytes]);
        com.copy_from_slice(&digest[sd_bytes..(sd_bytes + com_bytes)]);
        (sd, com)
    }
}

impl  LeafCommit<SD_BYTES, COM_BYTES> for EmLeafCommit {
    fn commit(mut sd_bytes: usize, mut com_bytes: usize, mut r: &[u8; SD_BYTES], mut iv: &[u8; 16], mut tweak: u32) -> ([u8; SD_BYTES], [u8; COM_BYTES])
    {
        let mut seed_buf = [(0 as u8); 16];
        seed_buf.copy_from_slice(&r[..]);
        let com_vec = aes_ctr_prg(&seed_buf, iv, tweak, com_bytes);
        let mut sd = [(0 as u8); sd_bytes];
        sd.copy_from_slice(r);
        let mut com = [(0 as u8); com_bytes];
        com.copy_from_slice(&com_vec[..com_bytes]);
        (sd, com)
    }
}

impl  LengthDoubler for AesCtrLengthDoubler {
    type OutputSize = U16;
    fn double(mut a: Vec<u8>) -> [Vec<u8>; 2]
    {
        let mut block0 = [(0 as u8); BLOCK];
        let mut block1 = [(0 as u8); BLOCK];
        block1[0] = 1;
        let key: [u8; BLOCK] = a.0;
        let c0 = encrypt_block(&key, &block0);
        let c1 = encrypt_block(&key, &block1);
        block0.fill(0);
        block1.fill(0);
        [Vec(c0), Vec(c1)]
    }
}

impl  FaestAesProver for StubFaestAesProver {
    fn prove_aes_witness(&self, mut big_vole: &BigVoleProver, mut hash_key: &UniversalHashKey) -> QuickSilverProof
    {
        let a_hat_out: UniversalHashOutput = vole_hash(hash_key, &big_vole.u);
        let h0_bytes = a_hat_out.h0.0.to_le_bytes();
        let h1_bytes = a_hat_out.h1.0.to_le_bytes();
        let mut a_hat: Vec<u8> = Vec::with_capacity((h0_bytes.len() + h1_bytes.len()));
        for i in 0.. h0_bytes.len(){
    a_hat.push(h0_bytes[i]);
};
        for i in 0.. h1_bytes.len(){
    a_hat.push(h1_bytes[i]);
};
        let len = a_hat.len();
        QuickSilverProof { a_hat: a_hat, b_hat: vec![(0 as u8); len], c_hat_base: vec![(0 as u8); len] }
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
        let mut out = vec![(0 as u8); n];
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
    pub fn action_result_base<D: Digest>(&self, mut args: &[&GarbleDyn], mut bit: usize) -> Self
    {
        let n: usize = self.n;
        let mut d = D::new();
        let domain_separator = [(172 as u8)];
        d.update(&domain_separator);
        d.update(&self.base);
        let mut d = absorb_args_bases(n, d, args);
        let bit_bytes = (bit as u64).to_le_bytes();
        d.update(&bit_bytes);
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
    let bit = self.base[(i * 8) + j] & 1;
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
    let bit = self.target[(i * 8) + j] & 1;
    v |= (bit << j);
};
    v
}).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn and_via_table<D: Digest>(&self, mut other: &EvalDyn, mut table: &GarbleTableDyn) -> EvalDyn
    {
        let n: usize = self.n;
        let index = if (self.target[0] & 1) == 1{
    1
} else {
    0
} | if (other.target[0] & 1) == 1{
    2
} else {
    0
};
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
    self.secret[i] ^ garble.base[i]
} else {
    garble.base[i]
}
}).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn one_wire_eval(&self) -> EvalDyn
    {
        let n: usize = self.n;
        self.encode(&GarbleDyn::zero(n), true)
    }
    pub fn not_garble(&self, mut a: &GarbleDyn) -> GarbleDyn
    {
        let n: usize = self.n;
        GarbleDyn { base: (0..n).map(|i| (a.base[i] ^ self.secret[i])).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn gen_and_table<D: Digest>(&self, mut a: &GarbleDyn, mut b: &GarbleDyn) -> GarbleTableDyn
    {
        let n: usize = self.n;
        self.gen_and_table_pol(a, b, false, false)
    }
    pub fn gen_and_table_pol<D: Digest>(&self, mut a: &GarbleDyn, mut b: &GarbleDyn, mut pa: bool, mut pb: bool) -> GarbleTableDyn
    {
        let n: usize = self.n;
        let result_base = a.and_result(b);
        let mut table = (0..n).map(|_| (0..n).map(|_| 0).collect::<Vec<u8>>()).collect::<Vec<_>>();
        for i in 0.. 4{
    let av = (i & 1) != 0;
    let bv = (i & 2) != 0;
    let ea = self.encode(a, av);
    let eb = self.encode(b, bv);
    let row = ((ea.target[0] & 1) as usize) | (((eb.target[0] & 1) as usize) << 1);
    let result_label = self.encode(&result_base, ((av ^ pa) & (bv ^ pb)));
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
        (result.open(&self.output_label.com_bytes, &self.output_label)[0] & 1) != 0
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
    s.iter().enumerate().map(|(a, b)| (b.clone() * matrix[i][a].clone())).fold(A::default(), |mut a, b| (a + b)) + e[i].clone()
}).collect::<Vec<U>>(), matrix: matrix }
    }
}

impl  NttParams {
    pub fn new(mut degree: usize, mut modulus: u64) -> Option<Self>
    {
        if (degree == 0) || ((degree & (degree - 1)) != 0){
    return None;
};
        if (modulus % 2) == 0{
    return None;
};
        if ((modulus - 1) % (2 * (degree as u64))) != 0{
    return None;
};
        Some(Self { degree: degree, modulus: modulus })
    }
    pub fn modulus_bits(&self) -> u32
    {
        64 - self.modulus.leading_zeros()
    }
}

impl <G: Group, D: Digest> BaseOt<L> for ChouOrlandiDyn<G, D> {
    type SenderState = BaseOtSenderDyn<G, D>;
    type ReceiverState = ChouOrlandiRecvDyn<G, D>;
    type SetupMsg = <G as _>::Element;
    type RecvMsg = OtReceiverMsgDyn<G>;
    type PayloadMsg = ([u8; L], [u8; L]);
    fn sender_setup<R: SpecRng>(mut l: usize, mut rng: &mut R) -> (Self::SenderState, Self::SetupMsg)
    {
        ot_send_setup::<G, D, R>(rng)
    }
    fn recv_start<R: SpecRng>(mut l: usize, mut rng: &mut R, mut setup: &Self::SetupMsg, mut c: bool) -> (Self::ReceiverState, Self::RecvMsg)
    {
        let (inner, msg) = ot_recv::<G, D, R>(rng, setup.clone(), c);
        (ChouOrlandiRecvDyn { inner: inner }, msg)
    }
    fn sender_payload<R: SpecRng>(mut l: usize, mut _rng: &mut R, mut state: &Self::SenderState, mut recv_msg: &Self::RecvMsg, mut m0: &[u8; L], mut m1: &[u8; L]) -> Self::PayloadMsg
    {
        let (k0, k1) = ot_send_finish::<G, D>(state, recv_msg);
        let mut e0 = [(0 as u8); l];
        let mut e1 = [(0 as u8); l];
        ot_send_payload::<D>(&k0, &k1, m0, m1, &mut e0, &mut e1);
        (e0, e1)
    }
    fn recv_finish(mut l: usize, mut state: &Self::ReceiverState, mut payload: &Self::PayloadMsg) -> [u8; L]
    {
        let kc = ot_recv_finish::<G, D>(&state.inner);
        let chosen = if state.inner_choice(){
    &payload.1
} else {
    &payload.0
};
        let mut mc = [(0 as u8); l];
        ot_recv_payload::<D>(&kc, chosen, &mut mc);
        mc
    }
}

impl <G: Group, D: Digest> ChouOrlandiRecvDyn<G, D> {
    pub fn inner_choice(&self) -> bool
    {
        super::base::ot_recv_choice(&self.inner)
    }
}

impl  FerretParams {
    pub fn splen(&self) -> usize
    {
        self.n.div_ceil(self.t).checked_next_power_of_two().expect("Ferret SPCOT interval length overflow")
    }
    pub fn log_splen(&self) -> usize
    {
        self.splen().trailing_zeros() as usize
    }
    pub fn interval_bounds(&self, mut index: usize) -> Option<(usize, usize)>
    {
        if (self.t == 0) || (index >= self.t){
    return None;
};
        let n = self.n as u128;
        let t = self.t as u128;
        let start = (((index as u128) * n) / t) as usize;
        let end = ((((index as u128) + 1) * n) / t) as usize;
        Some((start, end))
    }
    pub fn seed_cot_count(&self, mut malicious: bool) -> usize
    {
        let body = self.t.checked_mul(self.log_splen()).and_then(|noise| self.k.checked_add(noise)).expect("Ferret seed-COT count must fit usize");
        let seed_count = if malicious{
    body.checked_add(super::KAPPA_BITS).expect("malicious Ferret seed-COT count must fit usize")
} else {
    body
};
        seed_count
    }
    pub fn output_cot_count(&self, mut malicious: bool) -> usize
    {
        self.n.saturating_sub(self.seed_cot_count(malicious))
    }
}

impl  CotPoolSender {
    pub fn from_seed(mut params: FerretParams, mut seed: FerretSenderSeed) -> Result<Self, PoolSeedError>
    {
        Self::from_seed_mode(params, seed, false)
    }
    pub fn from_seed_malicious(mut params: FerretParams, mut seed: FerretSenderSeed) -> Result<Self, PoolSeedError>
    {
        Self::from_seed_mode(params, seed, true)
    }
    pub fn from_seed_mode(mut params: FerretParams, mut seed: FerretSenderSeed, mut malicious: bool) -> Result<Self, PoolSeedError>
    {
        let expected = checked_seed_cot_count_mode(params, malicious)?;
        if seed.q.len() != expected{
    return Err(PoolSeedError::SenderSeedCount { expected: expected, found: seed.q.len() });
};
        Ok(Self { params: params, seed: seed, out: VecDeque::new(), raise_n: None, malicious: malicious, refill_count: 0 })
    }
    pub fn remaining(&self) -> usize
    {
        self.out.len()
    }
    pub fn reprofile(self, mut params: FerretParams) -> Result<Self, PoolSeedError>
    {
        let old_seed_count = checked_seed_cot_count_mode(self.params, self.malicious)?;
        if self.seed.q.len() != old_seed_count{
    return Err(PoolSeedError::SenderSeedCount { expected: old_seed_count, found: self.seed.q.len() });
};
        let required = checked_seed_cot_count_mode(params, self.malicious)?;
        let available = self.seed.q.len().checked_add(self.out.len()).ok_or(PoolSeedError::InvalidParameters)?;
        if available < required{
    return Err(PoolSeedError::InsufficientCotMaterial { required: required, available: available });
};
        let delta = self.seed.delta;
        let mut cots = self.seed.q;
        cots.extend(self.out);
        let out = cots.split_off(required);
        self.params = params;
        self.seed = FerretSenderSeed { delta: delta, q: cots };
        self.out = out.into();
        self.raise_n = None;
        Ok(self)
    }
}

impl  CotPoolReceiver {
    pub fn from_seed(mut params: FerretParams, mut seed: FerretReceiverSeed) -> Result<Self, PoolSeedError>
    {
        Self::from_seed_mode(params, seed, false)
    }
    pub fn from_seed_malicious(mut params: FerretParams, mut seed: FerretReceiverSeed) -> Result<Self, PoolSeedError>
    {
        Self::from_seed_mode(params, seed, true)
    }
    pub fn from_seed_mode(mut params: FerretParams, mut seed: FerretReceiverSeed, mut malicious: bool) -> Result<Self, PoolSeedError>
    {
        let expected = checked_seed_cot_count_mode(params, malicious)?;
        if seed.u.len() != expected{
    return Err(PoolSeedError::ReceiverChoiceCount { expected: expected, found: seed.u.len() });
};
        if seed.w.len() != expected{
    return Err(PoolSeedError::ReceiverSeedCount { expected: expected, found: seed.w.len() });
};
        Ok(Self { params: params, seed: seed, out_x: VecDeque::new(), out_z: VecDeque::new(), malicious: malicious })
    }
    pub fn remaining(&self) -> usize
    {
        self.out_x.len()
    }
    pub fn reprofile(self, mut params: FerretParams) -> Result<Self, PoolSeedError>
    {
        let old_seed_count = checked_seed_cot_count_mode(self.params, self.malicious)?;
        if self.seed.u.len() != old_seed_count{
    return Err(PoolSeedError::ReceiverChoiceCount { expected: old_seed_count, found: self.seed.u.len() });
};
        if self.seed.w.len() != old_seed_count{
    return Err(PoolSeedError::ReceiverSeedCount { expected: old_seed_count, found: self.seed.w.len() });
};
        if self.out_x.len() != self.out_z.len(){
    return Err(PoolSeedError::ReceiverOutputCountMismatch { choices: self.out_x.len(), values: self.out_z.len() });
};
        let required = checked_seed_cot_count_mode(params, self.malicious)?;
        let available = self.seed.u.len().checked_add(self.out_x.len()).ok_or(PoolSeedError::InvalidParameters)?;
        if available < required{
    return Err(PoolSeedError::InsufficientCotMaterial { required: required, available: available });
};
        let mut choices = self.seed.u;
        choices.extend(self.out_x);
        let mut values = self.seed.w;
        values.extend(self.out_z);
        let output_choices = choices.split_off(required);
        let output_values = values.split_off(required);
        self.params = params;
        self.seed = FerretReceiverSeed { u: choices, w: values };
        self.out_x = output_choices.into();
        self.out_z = output_values.into();
        Ok(self)
    }
}

impl  ScalarOps for ToyGroup {
    fn scalar_zero() -> u64
    {
        0
    }
    fn scalar_add(mut a: &u64, mut b: &u64) -> u64
    {
        let q = TOY_P - 1;
        ((a % q) + (b % q)) % q
    }
    fn scalar_sub(mut a: &u64, mut b: &u64) -> u64
    {
        let q = TOY_P - 1;
        (((a % q) + q) - (b % q)) % q
    }
    fn scalar_mul_scalar(mut a: &u64, mut b: &u64) -> u64
    {
        let q = TOY_P - 1;
        ((a % q) * (b % q)) % q
    }
    fn scalar_eq(mut a: &u64, mut b: &u64) -> bool
    {
        (a % (TOY_P - 1)) == (b % (TOY_P - 1))
    }
    fn scalar_from_hash<D: Digest>(mut h: D) -> u64
    {
        let out = h.finalize().to_vec();
        let mut le = [(0 as u8); 8];
        le.copy_from_slice(&out[..8]);
        u64::from_le_bytes(le) % (TOY_P - 1)
    }
    fn random_mod_order<R: SpecRng>(mut rng: &mut R) -> u64
    {
        let v = ((rng.next_u32() as u64) << 32) | (rng.next_u32() as u64);
        v % (TOY_P - 1)
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
        let lo = rng.next_u32() as u64;
        let hi = rng.next_u32() as u64;
        ((hi << 32) | lo) % (TOY_P - 1)
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

impl <T: Clone + Add<Output = T> + Mul<Output = T> + Default> IdealCotDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T> + Default {
    pub fn new(mut n: usize, mut delta: DeltaLoweredDyn<T>) -> Self
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

impl  LweOtCrsDyn {
    pub fn sample<R: SpecRng>(mut n: usize, mut rng: &mut R) -> Self
    {
        let mut a = [[(0 as u32); n]; n];
        for i in 0.. n{
    for j in 0.. n{
    a[i][j] = sample_zq(rng);
};
};
        let mut h = [(0 as u32); n];
        for i in 0.. n{
    h[i] = sample_zq(rng);
};
        Self { a: a, h: h }
    }
}

impl  BaseOt<L> for LweBaseOtDyn {
    type SenderState = LweOtCrsDyn;
    type ReceiverState = LweOtReceiverDyn;
    type SetupMsg = LweOtCrsDyn;
    type RecvMsg = LweOtRecvMsgDyn;
    type PayloadMsg = LweOtSenderMsgDyn;
    fn sender_setup<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R) -> (Self::SenderState, Self::SetupMsg)
    {
        let crs = LweOtCrsDyn::<N>::sample(n, rng);
        (crs.clone(), crs)
    }
    fn recv_start<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R, mut setup: &Self::SetupMsg, mut c: bool) -> (Self::ReceiverState, Self::RecvMsg)
    {
        lwe_ot_recv(n, rng, setup, c)
    }
    fn sender_payload<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R, mut state: &Self::SenderState, mut recv_msg: &Self::RecvMsg, mut m0: &[u8; L], mut m1: &[u8; L]) -> Self::PayloadMsg
    {
        lwe_ot_send_bytes(n, rng, state, recv_msg, m0, m1)
    }
    fn recv_finish(mut n: usize, mut l: usize, mut state: &Self::ReceiverState, mut payload: &Self::PayloadMsg) -> [u8; L]
    {
        let bytes = lwe_ot_recv_decrypt_bytes(n, state, payload, l);
        let mut out = [(0 as u8); l];
        out.copy_from_slice(&bytes);
        out
    }
}

impl  BaseOt<L> for LweBaseOtSecureDyn {
    type SenderState = LweOtCrsDyn;
    type ReceiverState = LweOtReceiverDyn;
    type SetupMsg = LweOtCrsDyn;
    type RecvMsg = LweOtRecvMsgDyn;
    type PayloadMsg = LweOtSenderMsgSecureDyn;
    fn sender_setup<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R) -> (Self::SenderState, Self::SetupMsg)
    {
        let crs = LweOtCrsDyn::<N>::sample(n, rng);
        (crs.clone(), crs)
    }
    fn recv_start<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R, mut setup: &Self::SetupMsg, mut c: bool) -> (Self::ReceiverState, Self::RecvMsg)
    {
        lwe_ot_recv(n, rng, setup, c)
    }
    fn sender_payload<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R, mut state: &Self::SenderState, mut recv_msg: &Self::RecvMsg, mut m0: &[u8; L], mut m1: &[u8; L]) -> Self::PayloadMsg
    {
        lwe_ot_send_bytes_secure(n, rng, state, recv_msg, m0, m1)
    }
    fn recv_finish(mut n: usize, mut l: usize, mut state: &Self::ReceiverState, mut payload: &Self::PayloadMsg) -> [u8; L]
    {
        let bytes = lwe_ot_recv_decrypt_bytes_secure(n, state, payload, l);
        let mut out = [(0 as u8); l];
        out.copy_from_slice(&bytes);
        out
    }
}

impl <D: Digest> SoftSpokenOutLoweredDyn<D> {
    pub fn check(&self) -> bool
    {
        let m: usize = self.m;
        let l: usize = self.l;
        self.sender_tag == self.receiver_tag
    }
}

impl <D: Digest> SoftSpokenOutDynDyn<D> {
    pub fn check(&self) -> bool
    {
        let l: usize = self.l;
        self.sender_tag == self.receiver_tag
    }
}

impl  OtStack {
    pub fn setup<D, R>(mut rng_s: &mut R, mut rng_r: &mut R, mut params: FerretParams) -> Self where D: Digest
    {
        Self::<D, R, LweBaseOtDyn>::setup_impl(rng_s, rng_r, params, false)
    }
    pub fn setup_with_base<D, R, B>(mut rng_s: &mut R, mut rng_r: &mut R, mut params: FerretParams) -> Self where D: Digest, B: BaseOt
    {
        Self::<D, R, B>::setup_impl(rng_s, rng_r, params, false)
    }
    pub fn setup_malicious<D, R>(mut rng_s: &mut R, mut rng_r: &mut R, mut params: FerretParams) -> Self where D: Digest
    {
        Self::<D, R, LweBaseOtSecureDyn>::setup_impl(rng_s, rng_r, params, true)
    }
    pub fn setup_impl<D, R, B>(mut rng_s: &mut R, mut rng_r: &mut R, mut params: FerretParams, mut malicious: bool) -> Self where D: Digest, B: BaseOt
    {
        let m = params.seed_cot_count(malicious);
        let mut bits = vec![false; m];
        for b in &mut bits{
    *b = ((rng_r.next_u32() & 1) == 1);
};
        let mut delta_msg = [(0 as u8); 16];
        for chunk in delta_msg.chunks_mut(4){
    chunk.copy_from_slice(&rng_s.next_u32().to_le_bytes()[..chunk.len()]);
};
        let out = softspoken_cot_extend_base::<B, D, R, 16>(16, rng_s, rng_r, &bits, &delta_msg);
        let mut q = Vec::with_capacity(m);
        let mut w = Vec::with_capacity(m);
        for j in 0.. m{
    q.push(out.sender_r0[j]);
    w.push(out.receiver_v[j]);
};
        let mut stack = Self { sender: CotPoolSender { params: params, seed: crate::ot::ferret::cot::FerretSenderSeed { delta: delta_msg, q: q }, out: alloc::collections::VecDeque::new(), raise_n: None, malicious: malicious, refill_count: 0 }, receiver: CotPoolReceiver { params: params, seed: crate::ot::ferret::cot::FerretReceiverSeed { u: bits, w: w }, out_x: alloc::collections::VecDeque::new(), out_z: alloc::collections::VecDeque::new(), malicious: malicious } };
        let _ = rng_r;
        stack
    }
    pub fn from_ideal_seed<R: SpecRng>(mut rng: &mut R, mut params: FerretParams) -> Self
    {
        let (sender, receiver) = new_pool(rng, params);
        Self { sender: sender, receiver: receiver }
    }
    pub fn commit_bits<R: SpecRng>(&mut self, mut rng: &mut R, mut bits: &[bool]) -> Vec<(VopeDyn<Galois128>, QDyn<Galois128>)>
    {
        let (r0s, xs, zs) = take_random(rng, &mut self.sender, &mut self.receiver, bits.len()).expect("Ferret pool refill failed; abort protocol");
        let delta = self.sender.seed.delta;
        let mut out = Vec::with_capacity(bits.len());
        for j in 0.. bits.len(){
    let (r0, z, _d) = bea95_chosen_bit(&delta, r0s[j], xs[j], zs[j], bits[j]);
    let r0_t = (0..1).map(|_| Galois128(u128::from_le_bytes(r0))).collect::<Vec<Galois128>>();
    let v_t = (0..1).map(|_| Galois128(u128::from_le_bytes(z))).collect::<Vec<Galois128>>();
    out.push(vole_commit_bit_shares(r0_t, v_t, bit_to_g128, bits[j]));
};
        out
    }
}

impl  CotSource<U1, Galois128> for OtStack {
    fn cot<R: SpecRng>(&mut self, mut rng: &mut R, mut _sample_t: impl Fn, mut bit: bool) -> (Vec<Galois128>, Vec<Galois128>)
    {
        let (r0s, xs, zs) = take_random(rng, &mut self.sender, &mut self.receiver, 1).expect("Ferret pool refill failed; abort protocol");
        let (r0, z, _d) = bea95_chosen_bit(&self.sender.seed.delta, r0s[0], xs[0], zs[0], bit);
        let r0_t = (0..1).map(|_| Galois128(u128::from_le_bytes(r0))).collect::<Vec<Galois128>>();
        let v_t = (0..1).map(|_| Galois128(u128::from_le_bytes(z))).collect::<Vec<Galois128>>();
        (r0_t, v_t)
    }
}

impl  TfheBootstrapTableDyn {
    pub fn new(mut addr_bits: usize, mut table_len: usize, mut big_n: usize, mut logical: [bool; TABLE_LEN]) -> Result<Self, TfheBootstrapTableError>
    {
        let max_addr_bits = (usize::BITS as usize) - 1;
        if (addr_bits == 0) || (addr_bits > max_addr_bits){
    return Err(TfheBootstrapTableError::AddressWidthOutOfRange);
};
        if addr_bits > 2{
    return Err(TfheBootstrapTableError::InputEncodingUnsupported);
};
        let domain = (1 as usize) << addr_bits;
        if table_len != domain{
    return Err(TfheBootstrapTableError::TableLengthMismatch);
};
        let capacity = match big_n.checked_mul(2) {
    Some(capacity) => capacity,
    None => return Err(TfheBootstrapTableError::RingCapacityExceeded),
};
        if (table_len > capacity) || (big_n == 0) || !big_n.is_power_of_two(){
    return Err(TfheBootstrapTableError::RingCapacityExceeded);
};
        let mut is_constant = true;
        let mut index = 1;
        while (index < table_len){
    if logical[index] != logical[0]{
    is_constant = false;
    break;
};
    index += 1;
};
        if !is_constant{
    let half = table_len / 2;
    index = 0;
    while (index < half){
    if logical[index] == logical[index + half]{
    return Err(TfheBootstrapTableError::NegacyclicIncompatible);
};
    index += 1;
}
};
        let half_q4 = Q4 >> 1;
        let poly_step = big_n / (table_len / 2);
        let mut test_poly = [(0 as u32); big_n];
        index = 0;
        while (index < big_n){
    let entry = index / poly_step;
    test_poly[index] = if logical[entry]{
    half_q4
} else {
    half_q4.wrapping_neg()
};
    index += 1;
};
        Ok(Self { logical: logical, test_poly: test_poly, is_constant: is_constant })
    }
    pub fn entries(&self) -> &[bool; TABLE_LEN]
    {
        let addr_bits: usize = self.addr_bits;
        let table_len: usize = self.table_len;
        let big_n: usize = self.big_n;
        &self.logical
    }
}

impl  TfheBootstrapTableDyn {
}

impl  TryFrom<u8> for Stage {
    type Error = FrameError;
    fn try_from(mut value: u8) -> Result<Self, FrameError>
    {
        match value {
    .. => Ok(Self::PublicParameters),
    .. => Ok(Self::ReusableCiphertext),
    .. => Ok(Self::PerUseCiphertext),
    .. => Ok(Self::SelectionKey),
    .. => Ok(Self::Complete),
    .. => Ok(Self::Error),
    _ => Err(FrameError::UnknownStage),
}
    }
}

impl  Frame {
    pub fn encode(&self) -> Vec<u8>
    {
        let mut out = Vec::with_capacity((HEADER_BYTES + self.payload.len()));
        out.extend_from_slice(MAGIC);
        out.extend_from_slice(&VERSION.to_le_bytes());
        out.push((self.stage as u8));
        out.extend_from_slice(&self.binding.parameter_fingerprint);
        out.extend_from_slice(&self.binding.session_id);
        out.extend_from_slice(&self.binding.manifest_digest);
        out.extend_from_slice(&self.binding.use_counter.to_le_bytes());
        out.extend_from_slice(&(self.payload.len() as u32).to_le_bytes());
        out.extend_from_slice(&self.payload);
        out
    }
    pub fn decode(mut input: &[u8], mut max_payload: usize) -> Result<Self, FrameError>
    {
        if input.len() < HEADER_BYTES{
    return Err(FrameError::Truncated);
};
        if (&input[..8] != MAGIC) || (u16::from_le_bytes([input[8], input[9]]) != VERSION){
    return Err(FrameError::UnsupportedFormat);
};
        let stage = Stage::try_from(input[10])?;
        let mut offset: usize = 11;
        let mut take_32 = || {
    let mut value = [(0 as u8); 32];
    value.copy_from_slice(&input[offset..(offset + 32)]);
    offset += 32;
    value
};
        let parameter_fingerprint = take_32();
        let session_id = take_32();
        let manifest_digest = take_32();
        let use_counter = u64::from_le_bytes(input[offset..(offset + 8)].try_into().unwrap());
        offset += 8;
        let payload_len = u32::from_le_bytes(input[offset..(offset + 4)].try_into().unwrap()) as usize;
        offset += 4;
        if payload_len > max_payload{
    return Err(FrameError::PayloadTooLarge);
};
        if input.len() != offset.saturating_add(payload_len){
    return Err(FrameError::LengthMismatch);
};
        Ok(Self { stage: stage, binding: FrameBinding { parameter_fingerprint: parameter_fingerprint, session_id: session_id, manifest_digest: manifest_digest, use_counter: use_counter }, payload: input[offset..].to_vec() })
    }
}

impl  EncodedLabelBatch {
    pub fn from_pairs(mut pairs: &[LabelPairDyn], mut offset: [u8; 16]) -> Result<Self, BatchError>
    {
        let batch = LabelBatchDyn::new(16, pairs, offset)?;
        let mut differences = alloc::vec::Vec::with_capacity((pairs.len() * 3));
        let mut zeroes = alloc::vec::Vec::with_capacity((pairs.len() * 3));
        let modulus = ring_lwe::REFERENCE_PLAINTEXT_MODULUS;
        for pair in batch.pairs{
    let zero = encode_label_16(pair.zero);
    let one = encode_label_16(pair.one);
    for (one, zero) in one.into_iter().zip(zero.into_iter()){
    differences.push(if one >= zero{
    one - zero
} else {
    modulus - (zero - one)
});
    zeroes.push(zero);
};
};
        Ok(Self { differences: differences, zeroes: zeroes })
    }
    pub fn differences(&self) -> &[u64]
    {
        &self.differences
    }
    pub fn zeroes(&self) -> &[u64]
    {
        &self.zeroes
    }
    pub fn label_count(&self) -> usize
    {
        self.zeroes.len() / 3
    }
    pub fn pad_to_slots(&self, mut slots: usize) -> Result<PaddedLabelBatch, LabelBatchPaddingError>
    {
        let used = self.zeroes.len();
        if slots < used{
    return Err(LabelBatchPaddingError::TooFewSlots { slots: slots, used: used });
};
        let mut differences = self.differences.clone();
        let mut zeroes = self.zeroes.clone();
        differences.resize(slots, 0);
        zeroes.resize(slots, 0);
        Ok(PaddedLabelBatch { label_count: self.label_count(), differences: differences, zeroes: zeroes })
    }
    pub fn expanded_choices(mut choices: &[bool]) -> Vec<bool>
    {
        let mut out = alloc::vec::Vec::with_capacity((choices.len() * 3));
        for .. in choices{
    out.extend_from_slice(&[choice; 3]);
};
        out
    }
    pub fn decode_selected(mut selected: &[u64]) -> Result<Vec<[u8; 16]>, LabelBatchDecodeError>
    {
        if (selected.len() % 3) != 0{
    return Err(LabelBatchDecodeError::LengthMismatch);
};
        selected.chunks_exact(3).into_iter().map(|chunk| {
    decode_label_16([chunk[0], chunk[1], chunk[2]]).map_err(LabelBatchDecodeError::NonCanonicalLabel)
}).collect::<Vec<_>>().collect()
    }
}

impl  PaddedLabelBatch {
    pub fn slots(&self) -> usize
    {
        self.zeroes.len()
    }
    pub fn label_count(&self) -> usize
    {
        self.label_count
    }
    pub fn differences(&self) -> &[u64]
    {
        &self.differences
    }
    pub fn zeroes(&self) -> &[u64]
    {
        &self.zeroes
    }
    pub fn expanded_choices(&self, mut choices: &[bool]) -> Result<Vec<bool>, LabelBatchPaddingError>
    {
        if choices.len() != self.label_count{
    return Err(LabelBatchPaddingError::ChoiceLengthMismatch { expected: self.label_count, actual: choices.len() });
};
        let mut out = EncodedLabelBatch::expanded_choices(choices);
        out.resize(self.slots(), false);
        Ok(out)
    }
    pub fn decode_selected(&self, mut selected: &[u64]) -> Result<Vec<[u8; 16]>, LabelBatchDecodeError>
    {
        if selected.len() != self.slots(){
    return Err(LabelBatchDecodeError::LengthMismatch);
};
        EncodedLabelBatch::decode_selected(&selected[..(self.label_count * 3)])
    }
}

impl  LabelBatchDyn {
    pub fn new(mut n: usize, mut pairs: &[LabelPairDyn], mut offset: [u8; N]) -> Result<Self, BatchError>
    {
        if (n == 0) || ((offset[0] & 1) == 0){
    return Err(BatchError::EvenOffset);
};
        for (index, pair) in pairs.iter().enumerate(){
    if pair.one != (0..n).map(|byte| (pair.zero[byte] ^ offset[byte])).collect::<Vec<_>>(){
    return Err(BatchError::MismatchedPair { index: index });
};
};
        Ok(Self { pairs: pairs })
    }
    pub fn len(&self) -> usize
    {
        let n: usize = self.n;
        self.pairs.len()
    }
    pub fn is_empty(&self) -> bool
    {
        let n: usize = self.n;
        self.pairs.is_empty()
    }
    pub fn select(&self, mut choices: &[bool], mut output: &mut [[u8; N]]) -> Result<(), BatchError>
    {
        let n: usize = self.n;
        if (choices.len() != self.pairs.len()) || (output.len() != self.pairs.len()){
    return Err(BatchError::LengthMismatch);
};
        for ((pair, choice), selected) in self.pairs.iter().zip(choices.iter().copied().into_iter()).zip(output.iter_mut().into_iter()){
    *selected = if choice{
    pair.one
} else {
    pair.zero
};
};
        Ok(())
    }
}

impl  Parameters {
    pub fn scaled_reference(mut degree: usize, mut width: usize) -> Self
    {
        Self { degree: degree, width: width }
    }
    pub fn slots(self) -> Result<usize, Error>
    {
        self.degree.checked_mul(self.width).ok_or(Error::InvalidParameters)
    }
    pub fn levels(self) -> Result<usize, Error>
    {
        if (self.width < 2) || !self.width.is_power_of_two(){
    return Err(Error::InvalidParameters);
};
        Ok((self.width.ilog2() as usize))
    }
    pub fn validate(self) -> Result<(), Error>
    {
        if (self.degree < 2) || !self.degree.is_power_of_two() || (self.width < 2) || !self.width.is_power_of_two() || (self.plaintext_modulus < 3) || (self.delta < 3) || (self.gadget_base < 2) || (self.gadget_digits == 0) || (((self.plaintext_modulus - 1) % (2 * (self.degree as u64))) != 0) || (((self.delta - 1) % (2 * (self.degree as u64))) != 0){
    return Err(Error::InvalidParameters);
};
        self.slots()?;
        let modulus = (self.plaintext_modulus as u128) * (self.delta as u128);
        let mut capacity = 1 as u128;
        for _ in 0.. self.gadget_digits{
    capacity = capacity.checked_mul((self.gadget_base as u128)).ok_or(Error::InvalidParameters)?;
};
        if capacity <= modulus{
    return Err(Error::InvalidParameters);
};
        Ok(())
    }
}

impl  NoiseSource for ZeroNoise {
    fn sample(&mut self, mut _standard_deviation: u64, mut _bound: u64, mut output: &mut [i64]) -> Result<(), Error>
    {
        output.fill(0);
        Ok(())
    }
}

impl  Polynomial {
    pub fn degree(&self) -> usize
    {
        self.first.len()
    }
    pub fn rns_limbs(&self) -> (&[u64], &[u64])
    {
        (&self.first, &self.second)
    }
}

impl  PublicParameters {
    pub fn parameters(&self) -> Parameters
    {
        self.parameters
    }
    pub fn lhe_polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial>
    {
        self.lhe_a.iter()
    }
    pub fn lenc_polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial>
    {
        self.lenc_b.iter()
    }
}

impl  FirstCiphertext {
    pub fn lhe_polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial>
    {
        self.lhe_ciphertext.iter()
    }
    pub fn lenc_polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial>
    {
        self.lenc_ciphertext.iter()
    }
}

impl  SecondCiphertext {
    pub fn polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial>
    {
        self.lhe_ciphertext.iter()
    }
}

impl  SelectionKey {
    pub fn polynomial(&self) -> &Polynomial
    {
        &self.key
    }
}

impl  BatchSelect {
    pub fn setup(mut parameters: Parameters, mut random: &mut impl RandomSource) -> Result<Self, Error>
    {
        let ring = Ring::new(parameters)?;
        let mut lhe_a = Vec::with_capacity(parameters.width);
        for _ in 0.. parameters.width{
    lhe_a.push(ring.uniform(random)?);
};
        let mut lenc_b = Vec::with_capacity((2 * parameters.gadget_digits));
        for _ in 0.. 2 * parameters.gadget_digits{
    lenc_b.push(ring.uniform(random)?);
};
        Ok(Self { ring: ring, public: PublicParameters { parameters: parameters, lhe_a: lhe_a, lenc_b: lenc_b } })
    }
    pub fn public_parameters(&self) -> &PublicParameters
    {
        &self.public
    }
    pub fn enc1(&self, mut l1: &[u64], mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<FirstCiphertext, Error>
    {
        let messages = self.ring.encode_messages(l1)?;
        let (random_vector, lenc_ciphertext) = self.lenc_enc(&messages, random, noise)?;
        let (lhe_state, lhe_ciphertext) = self.lhe_enc1(&random_vector, random, noise)?;
        Ok(FirstCiphertext { lhe_state: lhe_state, lhe_ciphertext: lhe_ciphertext, lenc_ciphertext: lenc_ciphertext })
    }
    pub fn enc2(&self, mut l2: &[u64], mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<SecondCiphertext, Error>
    {
        let mut messages = self.ring.encode_messages(l2)?;
        for message in &mut messages{
    self.ring.add_noise(message, noise, LARGE_NOISE_STANDARD_DEVIATION, LARGE_NOISE_MAX_DEVIATION)?;
};
        let (lhe_state, lhe_ciphertext) = self.lhe_enc2(&messages, random, noise)?;
        Ok(SecondCiphertext { lhe_state: lhe_state, lhe_ciphertext: lhe_ciphertext })
    }
    pub fn keygen(&self, mut first: &FirstCiphertext, mut second: &SecondCiphertext, mut choices: &[bool]) -> Result<SelectionKey, Error>
    {
        self.check_first(first)?;
        self.check_second(second)?;
        let choice = self.ring.encode_choices(choices)?;
        let tree = self.lenc_digest(&choice)?;
        let digits = self.ring.decompose(&tree.digest)?;
        let mut key = second.lhe_state.clone();
        for (state, digit) in first.lhe_state.iter().zip(digits.iter()){
    key.add_assign(&self.ring, &state.product(&self.ring, digit));
};
        Ok(SelectionKey { key: key })
    }
    pub fn dec(&self, mut first: &FirstCiphertext, mut second: &SecondCiphertext, mut key: &SelectionKey, mut choices: &[bool]) -> Result<Vec<u64>, Error>
    {
        self.check_first(first)?;
        self.check_second(second)?;
        if key.key.degree() != self.ring.parameters.degree{
    return Err(Error::LengthMismatch);
};
        let choice = self.ring.encode_choices(choices)?;
        let tree = self.lenc_digest(&choice)?;
        let digits = self.ring.decompose(&tree.digest)?;
        let mut result = Vec::with_capacity(self.ring.parameters.width);
        for index in 0.. self.ring.parameters.width{
    let row = &first.lhe_ciphertext[(index * self.ring.parameters.gadget_digits)..((index + 1) * self.ring.parameters.gadget_digits)];
    let mut value = inner_product(&self.ring, row, &digits)?;
    value.add_assign(&self.ring, &second.lhe_ciphertext[index]);
    value.sub_assign(&self.ring, &self.public.lhe_a[index].product(&self.ring, &key.key));
    result.push(value);
};
        let correction = self.lenc_eval(&first.lenc_ciphertext, &tree)?;
        for (value, delta) in result.iter_mut().zip(correction.iter()){
    value.sub_assign(&self.ring, delta);
};
        self.ring.decode_messages(&result)
    }
    pub fn check_first(&self, mut first: &FirstCiphertext) -> Result<(), Error>
    {
        let p = self.ring.parameters;
        if (first.lhe_state.len() != p.gadget_digits) || (first.lhe_ciphertext.len() != (p.width * p.gadget_digits)) || (first.lenc_ciphertext.len() != (((p.levels()? * p.width) * 2) * p.gadget_digits)) || first.lhe_state.iter().chain(first.lhe_ciphertext.iter()).chain(first.lenc_ciphertext.iter()).any(|poly| (poly.degree() != p.degree)){
    return Err(Error::LengthMismatch);
};
        Ok(())
    }
    pub fn check_second(&self, mut second: &SecondCiphertext) -> Result<(), Error>
    {
        let p = self.ring.parameters;
        if (second.lhe_ciphertext.len() != p.width) || (second.lhe_state.degree() != p.degree) || second.lhe_ciphertext.iter().any(|poly| (poly.degree() != p.degree)){
    return Err(Error::LengthMismatch);
};
        Ok(())
    }
    pub fn lhe_enc1(&self, mut messages: &[Polynomial], mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<(Vec<Polynomial>, Vec<Polynomial>), Error>
    {
        if messages.len() != self.ring.parameters.width{
    return Err(Error::LengthMismatch);
};
        let p = self.ring.parameters;
        let mut state = Vec::with_capacity(p.gadget_digits);
        for _ in 0.. p.gadget_digits{
    state.push(self.ring.uniform(random)?);
};
        let mut ciphertext = Vec::with_capacity((p.width * p.gadget_digits));
        for (index, message) in messages.iter().enumerate(){
    for digit in 0.. p.gadget_digits{
    let mut value = self.public.lhe_a[index].product(&self.ring, &state[digit]);
    value.add_assign(&self.ring, &message.scaled(&self.ring, p.gadget_base, digit));
    self.ring.add_noise(&mut value, noise, SMALL_NOISE_STANDARD_DEVIATION, SMALL_NOISE_MAX_DEVIATION)?;
    ciphertext.push(value);
};
};
        Ok((state, ciphertext))
    }
    pub fn lhe_enc2(&self, mut messages: &[Polynomial], mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<(Polynomial, Vec<Polynomial>), Error>
    {
        if messages.len() != self.ring.parameters.width{
    return Err(Error::LengthMismatch);
};
        let state = self.ring.uniform(random)?;
        let mut ciphertext = Vec::with_capacity(self.ring.parameters.width);
        for (a, message) in self.public.lhe_a.iter().zip(messages.iter()){
    let mut value = a.product(&self.ring, &state);
    value.add_assign(&self.ring, message);
    self.ring.add_noise(&mut value, noise, LARGE_NOISE_STANDARD_DEVIATION, LARGE_NOISE_MAX_DEVIATION)?;
    ciphertext.push(value);
};
        Ok((state, ciphertext))
    }
    pub fn lenc_enc(&self, mut message: &[Polynomial], mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<(Vec<Polynomial>, Vec<Polynomial>), Error>
    {
        let p = self.ring.parameters;
        if message.len() != p.width{
    return Err(Error::LengthMismatch);
};
        let levels = p.levels()?;
        let mut random_vector = Vec::with_capacity((levels * p.width));
        for _ in 0.. levels * p.width{
    random_vector.push(self.ring.uniform(random)?);
};
        let mut ciphertext = Vec::with_capacity((((levels * p.width) * 2) * p.gadget_digits));
        for level in 0.. levels{
    for row in 0.. p.width{
    for public in &self.public.lenc_b{
    ciphertext.push(random_vector[(level * p.width) + row].product(&self.ring, public));
};
    let half = if (row & (1 << ((levels - level) - 1))) == 0{
    0
} else {
    p.gadget_digits
};
    let next = if (level + 1) == levels{
    &message[row]
} else {
    &random_vector[((level + 1) * p.width) + row]
};
    let base = ((((level * p.width) + row) * 2) * p.gadget_digits) + half;
    for digit in 0.. p.gadget_digits{
    let extra = next.scaled(&self.ring, p.gadget_base, digit);
    ciphertext[base + digit].add_assign(&self.ring, &extra);
};
};
};
        for value in &mut ciphertext{
    self.ring.add_noise(value, noise, SMALL_NOISE_STANDARD_DEVIATION, SMALL_NOISE_MAX_DEVIATION)?;
};
        Ok((random_vector, ciphertext))
    }
    pub fn lenc_digest(&self, mut choice: &[Polynomial]) -> Result<Tree, Error>
    {
        let p = self.ring.parameters;
        if choice.len() != p.width{
    return Err(Error::LengthMismatch);
};
        let mut tree = vec![self.ring.zero(); (((2 * p.width) - 1) * p.gadget_digits)];
        for (index, value) in choice.iter().enumerate(){
    let digits = self.ring.decompose(value)?;
    let base = ((p.width - 1) + index) * p.gadget_digits;
    tree[base..(base + p.gadget_digits)].clone_from_slice(&digits);
};
        let mut digest = self.ring.zero();
        for node in 0..(p.width - 1).rev(){
    let children = ((2 * node) + 1) * p.gadget_digits;
    let mut parent = inner_product(&self.ring, &self.public.lenc_b, &tree[children..(children + (2 * p.gadget_digits))])?;
    parent.negate_assign(&self.ring);
    if node == 0{
    digest = parent;
} else {
    let digits = self.ring.decompose(&parent)?;
    let base = node * p.gadget_digits;
    tree[base..(base + p.gadget_digits)].clone_from_slice(&digits);
};
};
        Ok(Tree { tree: tree, digest: digest })
    }
    pub fn lenc_eval(&self, mut ciphertext: &[Polynomial], mut tree: &Tree) -> Result<Vec<Polynomial>, Error>
    {
        let p = self.ring.parameters;
        let levels = p.levels()?;
        if (ciphertext.len() != (((levels * p.width) * 2) * p.gadget_digits)) || (tree.tree.len() != (((2 * p.width) - 1) * p.gadget_digits)){
    return Err(Error::LengthMismatch);
};
        let mut delta = Vec::with_capacity(p.width);
        for row in 0.. p.width{
    let mut value = inner_product(&self.ring, &ciphertext[((row * 2) * p.gadget_digits)..(((row + 1) * 2) * p.gadget_digits)], &tree.tree[p.gadget_digits..(3 * p.gadget_digits)])?;
    for level in 1.. levels{
    let ciphertext_base = ((level * p.width) + row) * 2 * p.gadget_digits;
    let tree_base = (((((row >> (levels - level)) + (1 << level)) - 1) * 2) + 1) * p.gadget_digits;
    let term = inner_product(&self.ring, &ciphertext[ciphertext_base..(ciphertext_base + (2 * p.gadget_digits))], &tree.tree[tree_base..(tree_base + (2 * p.gadget_digits))])?;
    value.add_assign(&self.ring, &term);
};
    value.negate_assign(&self.ring);
    delta.push(value);
};
        Ok(delta)
    }
}

impl  Ring {
    pub fn new(mut parameters: Parameters) -> Result<Self, Error>
    {
        parameters.validate()?;
        Ok(Self { first_ntt: Ntt::new(parameters.degree, parameters.plaintext_modulus)?, second_ntt: Ntt::new(parameters.degree, parameters.delta)?, inverse_plaintext_mod_delta: inverse_mod((parameters.plaintext_modulus % parameters.delta), parameters.delta).ok_or(Error::Arithmetic)?, inverse_delta_mod_plaintext: inverse_mod((parameters.delta % parameters.plaintext_modulus), parameters.plaintext_modulus).ok_or(Error::Arithmetic)?, parameters: parameters })
    }
    pub fn zero(&self) -> Polynomial
    {
        Polynomial { first: vec![0; self.parameters.degree], second: vec![0; self.parameters.degree] }
    }
    pub fn uniform(&self, mut random: &mut impl RandomSource) -> Result<Polynomial, Error>
    {
        let mut value = self.zero();
        sample_uniform(random, self.parameters.plaintext_modulus, &mut value.first)?;
        sample_uniform(random, self.parameters.delta, &mut value.second)?;
        Ok(value)
    }
    pub fn encode_messages(&self, mut input: &[u64]) -> Result<Vec<Polynomial>, Error>
    {
        if input.len() != self.parameters.slots()?{
    return Err(Error::LengthMismatch);
};
        if input.iter().any(|..| (value >= self.parameters.plaintext_modulus)){
    return Err(Error::NonCanonicalPlaintext);
};
        let mut output = Vec::with_capacity(self.parameters.width);
        for chunk in input.chunks_exact(self.parameters.degree){
    let mut value = self.zero();
    for (slot, ..) in value.first.iter_mut().zip(chunk.iter()){
    *slot = mul_mod(message, (self.parameters.delta % self.parameters.plaintext_modulus), self.parameters.plaintext_modulus);
};
    output.push(value);
};
        Ok(output)
    }
    pub fn encode_choices(&self, mut input: &[bool]) -> Result<Vec<Polynomial>, Error>
    {
        if input.len() != self.parameters.slots()?{
    return Err(Error::LengthMismatch);
};
        let mut output = Vec::with_capacity(self.parameters.width);
        for chunk in input.chunks_exact(self.parameters.degree){
    let mut value = self.zero();
    for ((first, second), ..) in value.first.iter_mut().zip(value.second.iter_mut()).zip(chunk.iter()){
    let choice = u64::from(choice);
    *first = choice;
    *second = choice;
};
    self.first_ntt.inverse(&mut value.second);
    self.second_ntt.forward(&mut value.second);
    output.push(value);
};
        Ok(output)
    }
    pub fn add_noise(&self, mut value: &mut Polynomial, mut noise: &mut impl NoiseSource, mut standard_deviation: u64, mut bound: u64) -> Result<(), Error>
    {
        let mut coefficients = vec![(0 as i64); self.parameters.degree];
        noise.sample(&mut coefficients.n, standard_deviation, bound, &mut coefficients)?;
        if coefficients.iter().any(|sample| (sample.unsigned_abs() > bound)){
    return Err(Error::Noise);
};
        let mut first = Vec::with_capacity(self.parameters.degree);
        let mut second = Vec::with_capacity(self.parameters.degree);
        for sample in coefficients{
    first.push(signed_to_mod(sample, self.parameters.plaintext_modulus));
    second.push(signed_to_mod(sample, self.parameters.delta));
};
        self.first_ntt.forward(&mut first);
        self.second_ntt.forward(&mut second);
        for (destination, error) in value.first.iter_mut().zip(first){
    *destination = add_mod(*destination, error, self.parameters.plaintext_modulus);
};
        for (destination, error) in value.second.iter_mut().zip(second){
    *destination = add_mod(*destination, error, self.parameters.delta);
};
        Ok(())
    }
    pub fn decompose(&self, mut value: &Polynomial) -> Result<Vec<Polynomial>, Error>
    {
        if value.degree() != self.parameters.degree{
    return Err(Error::LengthMismatch);
};
        let mut first = value.first.clone();
        let mut second = value.second.clone();
        self.first_ntt.inverse(&mut first);
        self.second_ntt.inverse(&mut second);
        let mut digits: Vec<Polynomial> = (0..self.parameters.gadget_digits).map(|_| self.zero()).collect::<Vec<_>>();
        for index in 0.. self.parameters.degree{
    let mut combined = self.combine(first[index], second[index]);
    for digit in &mut digits{
    let part = (combined % (self.parameters.gadget_base as u128)) as u64;
    digit.first[index] = (part % self.parameters.plaintext_modulus);
    digit.second[index] = (part % self.parameters.delta);
    combined /= (self.parameters.gadget_base as u128);
};
    if combined != 0{
    return Err(Error::Arithmetic);
};
};
        for digit in &mut digits{
    self.first_ntt.forward(&mut digit.first);
    self.second_ntt.forward(&mut digit.second);
};
        Ok(digits)
    }
    pub fn decode_messages(&self, mut values: &[Polynomial]) -> Result<Vec<u64>, Error>
    {
        if (values.len() != self.parameters.width) || values.iter().any(|value| (value.degree() != self.parameters.degree)){
    return Err(Error::LengthMismatch);
};
        let mut output = Vec::with_capacity(self.parameters.slots()?);
        for value in values{
    let mut noise = value.second.clone();
    self.second_ntt.inverse(&mut noise);
    let mut correction = Vec::with_capacity(self.parameters.degree);
    for error in noise{
    let signed = if error > (self.parameters.delta / 2){
    let magnitude = self.parameters.delta - error;
    if magnitude >= self.parameters.plaintext_modulus{
    return Err(Error::Noise);
};
    if magnitude == 0{
    0
} else {
    self.parameters.plaintext_modulus - magnitude
}
} else {
    if error >= self.parameters.plaintext_modulus{
    return Err(Error::Noise);
};
    error
};
    correction.push(signed);
};
    self.first_ntt.forward(&mut correction);
    for (message, correction) in value.first.iter().zip(correction.into_iter()){
    let no_error = sub_mod(*message, correction, self.parameters.plaintext_modulus);
    output.push(mul_mod(no_error, self.inverse_delta_mod_plaintext, self.parameters.plaintext_modulus));
};
};
        Ok(output)
    }
    pub fn combine(&self, mut first: u64, mut second: u64) -> u128
    {
        let first_mod_delta = first % self.parameters.delta;
        let offset = mul_mod(sub_mod(second, first_mod_delta, self.parameters.delta), self.inverse_plaintext_mod_delta, self.parameters.delta);
        (first as u128) + ((self.parameters.plaintext_modulus as u128) * (offset as u128))
    }
}

impl  Polynomial {
    pub fn product(&self, mut ring: &Ring, mut other: &Self) -> Self
    {
        let mut output = ring.zero();
        for ((output, left), right) in output.first.iter_mut().zip(self.first.iter()).zip(other.first.iter()){
    *output = mul_mod(*left, *right, ring.parameters.plaintext_modulus);
};
        for ((output, left), right) in output.second.iter_mut().zip(self.second.iter()).zip(other.second.iter()){
    *output = mul_mod(*left, *right, ring.parameters.delta);
};
        output
    }
    pub fn add_assign(&mut self, mut ring: &Ring, mut other: &Self)
    {
        for (left, right) in self.first.iter_mut().zip(other.first.iter()){
    *left = add_mod(*left, *right, ring.parameters.plaintext_modulus);
};
        for (left, right) in self.second.iter_mut().zip(other.second.iter()){
    *left = add_mod(*left, *right, ring.parameters.delta);
}
    }
    pub fn sub_assign(&mut self, mut ring: &Ring, mut other: &Self)
    {
        for (left, right) in self.first.iter_mut().zip(other.first.iter()){
    *left = sub_mod(*left, *right, ring.parameters.plaintext_modulus);
};
        for (left, right) in self.second.iter_mut().zip(other.second.iter()){
    *left = sub_mod(*left, *right, ring.parameters.delta);
}
    }
    pub fn negate_assign(&mut self, mut ring: &Ring)
    {
        for value in &mut self.first{
    if *value != 0{
    *value = (ring.parameters.plaintext_modulus - *value);
};
};
        for value in &mut self.second{
    if *value != 0{
    *value = (ring.parameters.delta - *value);
};
}
    }
    pub fn scaled(&self, mut ring: &Ring, mut base: u64, mut power: usize) -> Self
    {
        let first = pow_mod((base % ring.parameters.plaintext_modulus), power, ring.parameters.plaintext_modulus);
        let second = pow_mod((base % ring.parameters.delta), power, ring.parameters.delta);
        let mut output = self.clone();
        for value in &mut output.first{
    *value = mul_mod(*value, first, ring.parameters.plaintext_modulus);
};
        for value in &mut output.second{
    *value = mul_mod(*value, second, ring.parameters.delta);
};
        output
    }
}

impl  Ntt {
    pub fn new(mut degree: usize, mut modulus: u64) -> Result<Self, Error>
    {
        let psi = find_negacyclic_root(degree, modulus).ok_or(Error::Arithmetic)?;
        let omega = mul_mod(psi, psi, modulus);
        Ok(Self { modulus: modulus, degree: degree, psi: psi, omega: omega, inverse_psi: inverse_mod(psi, modulus).ok_or(Error::Arithmetic)?, inverse_omega: inverse_mod(omega, modulus).ok_or(Error::Arithmetic)?, inverse_degree: inverse_mod((degree as u64), modulus).ok_or(Error::Arithmetic)? })
    }
    pub fn forward(&self, mut values: &mut [u64])
    {
        for (index, value) in values.iter_mut().enumerate(){
    *value = mul_mod(*value, pow_mod(self.psi, index, self.modulus), self.modulus);
};
        self.cyclic(values, self.omega);
    }
    pub fn inverse(&self, mut values: &mut [u64])
    {
        self.cyclic(values, self.inverse_omega);
        for (index, value) in values.iter_mut().enumerate(){
    *value = mul_mod(*value, self.inverse_degree, self.modulus);
    *value = mul_mod(*value, pow_mod(self.inverse_psi, index, self.modulus), self.modulus);
}
    }
    pub fn cyclic(&self, mut values: &mut [u64], mut root: u64)
    {
        bit_reverse(values);
        let mut length = 2;
        while (length <= self.degree){
    let step = pow_mod(root, (self.degree / length), self.modulus);
    for start in 0..self.degree.step_by(length){
    let mut twiddle = 1;
    for offset in 0.. length / 2{
    let left = values[start + offset];
    let right = mul_mod(values[start + offset + (length / 2)], twiddle, self.modulus);
    values[start + offset] = add_mod(left, right, self.modulus);
    values[start + offset + (length / 2)] = sub_mod(left, right, self.modulus);
    twiddle = mul_mod(twiddle, step, self.modulus);
};
};
    length *= 2;
}
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
    let BitsInBytes(next) = u[l][(i + 1) % n_param].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>()
}).collect::<Vec<Vec<BitsInBytes>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    let BitsInBytes(next) = v[(i + 1) % n_param].clone();
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
    let BitsInBytes(prev) = u[l][((i + n_param) - 1) % n_param].clone();
    let BitsInBytes(b) = u[l][i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>()
}).collect::<Vec<Vec<BitsInBytes>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes(prev) = v[((i + n_param) - 1) % n_param].clone();
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
    let BitsInBytes64(next) = u[l][(i + 1) % n_param].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>()
}).collect::<Vec<Vec<BitsInBytes64>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    let BitsInBytes64(next) = v[(i + 1) % n_param].clone();
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
    let BitsInBytes64(prev) = u[l][((i + n_param) - 1) % n_param].clone();
    let BitsInBytes64(b) = u[l][i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>()
}).collect::<Vec<Vec<BitsInBytes64>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = v[((i + n_param) - 1) % n_param].clone();
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

impl  QDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    let BitsInBytes(next) = q[(i + 1) % n_param].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes(prev) = q[((i + n_param) - 1) % n_param].clone();
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
    let BitsInBytes64(next) = q[(i + 1) % n_param].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = q[((i + n_param) - 1) % n_param].clone();
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

impl  DeltaLoweredDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaLoweredDyn { delta: delta, .. } = self;
        DeltaLoweredDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    let BitsInBytes(next) = delta[(i + 1) % n_param].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaLoweredDyn { delta: delta, .. } = self;
        DeltaLoweredDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(prev) = delta[((i + n_param) - 1) % n_param].clone();
    let BitsInBytes(b) = delta[i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> DeltaLoweredDyn<Bit>
    {
        let n: usize = self.n;
        let DeltaLoweredDyn { delta: delta, .. } = self;
        DeltaLoweredDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl  DeltaLoweredDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaLoweredDyn { delta: delta, .. } = self;
        DeltaLoweredDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    let BitsInBytes64(next) = delta[(i + 1) % n_param].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaLoweredDyn { delta: delta, .. } = self;
        DeltaLoweredDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = delta[((i + n_param) - 1) % n_param].clone();
    let BitsInBytes64(b) = delta[i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> DeltaLoweredDyn<Bit>
    {
        let n: usize = self.n;
        let DeltaLoweredDyn { delta: delta, .. } = self;
        DeltaLoweredDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
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

impl <T: Clone> Clone for DeltaLoweredDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let DeltaLoweredDyn { delta: delta, .. } = self;
        DeltaLoweredDyn { delta: (0..n).map(|i| delta[i].clone()).collect::<Vec<T>>(), n: 0 }
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
    if u1[l][i] != u2[l][i]{
    return false;
};
};
};
        for i in 0.. n{
    if v1[i] != v2[i]{
    return false;
};
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
    if q1[i] != q2[i]{
    return false;
};
};
        true
    }
}

impl <T: PartialEq> PartialEq for DeltaLoweredDyn<T> {
    fn eq(&self, mut other: &Self) -> bool
    {
        let n: usize = self.n;
        let DeltaLoweredDyn { delta: d1, .. } = self;
        let DeltaLoweredDyn { delta: d2, .. } = other;
        for i in 0.. n{
    if d1[i] != d2[i]{
    return false;
};
};
        true
    }
}

impl <T: Eq> Eq for VopeDyn<T> {
}

impl <T: Eq> Eq for QDyn<T> {
}

impl <T: Eq> Eq for DeltaLoweredDyn<T> {
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
        *produce == *consume
    }
}

impl <T: Clone + Mul<Output = T>> ChallengeKeyDyn<T> {
    pub fn from_challenge(mut r: T) -> Self
    {
        let r2 = r.clone() * r.clone();
        let r3 = r2.clone() * r.clone();
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
        let a = addr * self.key.r1.clone();
        let v = value * self.key.r2.clone();
        let t = self.scale_by_u64(self.key.r3.clone(), timestamp);
        a + v + t
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
    pub fn produce(&self) -> &<H as _>::State
    {
        &self.produce
    }
    pub fn consume(&self) -> &<H as _>::State
    {
        &self.consume
    }
    pub fn scale_by_u64(&self, mut x: T, mut n: u64) -> T
    {
        if n == 0{
    return T::default();
};
        if n == 1{
    return x;
};
        let mut acc = T::default();
        let mut base = x;
        let mut remaining = n;
        while (remaining != 0){
    if (remaining & 1) == 1{
    acc = (acc + base.clone());
};
    remaining >>= 1;
    if remaining != 0{
    base = (base.clone() + base);
};
};
        acc
    }
}

impl <T> PolyLoweredDyn<T> {
    pub fn get_qs_pool<Q: Clone + Mul<A, Output = A>, A: Add<A, Output = A>>(&self, mut m: usize, mut x: usize, mut root: DeltaLoweredDyn<Q>, mut inputs: PolyInputPoolDyn<QDyn<Q>>, mut reduction: usize) -> QDyn<A> where T: Clone + Into<A>
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
};
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<A>>() }
    }
    pub fn get_qs<Q: Clone + Mul<A, Output = A>, A: Add<A, Output = A>>(&self, mut m: usize, mut x: usize, mut root: DeltaLoweredDyn<Q>, mut inputs: Vec<Vec<QDyn<Q>>>, mut reduction: usize) -> QDyn<A> where T: Clone + Into<A>
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
};
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
    sum + c0
}).collect::<Vec<O>>();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    for n in 0.. x{
    let mut b: O = self.c1[k].clone().into();
    for m in 0.. s{
    let l = (l * s) + m;
    for (idx, v) in voles.indices.iter().enumerate(){
    b = (b * if idx == n{
    voles.inputs[v[k]].u[l][i].clone().into()
} else {
    voles.inputs[v[k]].v[i].clone().into()
});
};
};
    sum = (sum + b);
};
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
    sum + c0
}).collect::<Vec<O>>();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    for n in 0.. x{
    let mut b: O = self.c1[k].clone().into();
    for m in 0.. s{
    let l = (l * s) + m;
    for (idx, v) in voles.iter().enumerate(){
    b = (b * if idx == n{
    v[k].u[l][i].clone().into()
} else {
    v[k].v[i].clone().into()
});
};
};
    sum = (sum + b);
};
};
    sum
}).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>();
        return VopeDyn { u: u, v: v, n: 0, k: 1 };
    }
}

impl <T: Clone + Add<Output = T> + Mul<Output = T> + Default> CotSource<N, T> for IdealCotDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T> + Default {
    fn cot<R: SpecRng>(&mut self, mut rng: &mut R, mut sample_t: impl Fn, mut bit: bool) -> (Vec<T>, Vec<T>)
    {
        let n: usize = self.n;
        IdealCotDyn::cot(self, rng, sample_t, bit)
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
    let k = i + j;
    let a_coeff = if i == 0{
    &self.v
} else {
    &self.u[i - 1]
};
    let b_coeff = if j == 0{
    &other.v
} else {
    &other.u[j - 1]
};
    if k == 0{
    for lane in 0.. n{
    res_v[lane] = (res_v[lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
} else {
    for lane in 0.. n{
    res_u[k - 1][lane] = (res_u[k - 1][lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
};
};
};
        VopeDyn { u: res_u, v: res_v, n: 0, k: 1 }
    }
}

impl <T: Default> Default for VopeDyn<T> {
    fn default(mut n: usize, mut k: usize) -> Self
    {
        VopeDyn { u: Default::default(), v: Default::default(), n: 0, k: 1 }
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
    let o: O = self.u[i][j].clone().bitxor(rhs[(i * k) + j].clone());
    o
}).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>(), v: self.v.into_iter().map(|a| a.into()).collect::<Vec<_>>(), n: 0, k: 1 }
    }
}

impl <T: Mul<U, Output = O> + Into<O> + Clone, U: Mul<U, Output = U> + Clone, O: Add<O, Output = O> + Clone> Mul<DeltaLoweredDyn<U>> for VopeDyn<T> {
    type Output = QDyn<O>;
    fn mul(self, mut rhs: DeltaLoweredDyn<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        QDyn { q: self.u.iter().enumerate().fold(self.v.into_iter().map(|a| a.into()).collect::<Vec<_>>(), |mut a, (i, b)| {
    (0..n).map(|j| {
    let mut x = rhs.delta[i].clone();
    for _ in 0.. i{
    x = (x * rhs.delta[i].clone());
};
    let m: O = b[j].clone() * x;
    m + a[j].clone()
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
    (0..m).map(|i| u[l][f(i) % n].clone()).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: (0..m).map(|i| v[f(i) % n].clone()).collect::<Vec<T>>(), n: 0, k: 1 }
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

impl <T: Default> Default for QDyn<T> {
    fn default(mut n: usize) -> Self
    {
        QDyn { q: Default::default(), n: 0 }
    }
}

impl <T> DeltaLoweredDyn<T> {
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> DeltaLoweredDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let Self { delta: delta, .. } = self;
        DeltaLoweredDyn { delta: (0..m).map(|i| delta[f(i) % n].clone()).collect::<Vec<T>>(), n: 0 }
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
        QDyn { q: (0..m).map(|i| q[f(i) % n].clone()).collect::<Vec<T>>(), n: 0 }
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

impl  Default for WarpTranscript {
    fn default() -> Self
    {
        warp_transcript_new()
    }
}

impl  WarpTranscript {
    pub fn domain_sep(self, mut n: usize, mut label: &Vec<u8>) -> Self
    {
        let mut sponge = self.sponge.clone();
        spec_sponge_absorb_array(n, &mut sponge, label);
        self.sponge = sponge;
        self
    }
    pub fn absorb_array(&mut self, mut n: usize, mut data: &Vec<u8>)
    {
        let mut sponge = self.sponge.clone();
        spec_sponge_absorb_array(n, &mut sponge, data);
        self.sponge = sponge;
    }
    pub fn absorb_len_prefixed(&mut self, mut n: usize, mut data: &Vec<u8>)
    {
        let len_bytes = (n as u64).to_le_bytes();
        self.absorb_array(n, &(0..8).map(|i| len_bytes[i]).collect::<Vec<u8>>());
        self.absorb_array(n, data);
    }
    pub fn absorb_usize(&mut self, mut value: usize)
    {
        let bytes = (value as u64).to_le_bytes();
        self.absorb_array(&(0..8).map(|i| bytes[i]).collect::<Vec<u8>>().n, &(0..8).map(|i| bytes[i]).collect::<Vec<u8>>());
    }
    pub fn absorb_field(&mut self, mut value: Galois128)
    {
        self.absorb_len_prefixed(&le16(value).n, &le16(value));
    }
    pub fn absorb_fields(&mut self, mut n: usize, mut values: &Vec<Galois128>)
    {
        self.absorb_usize(n);
        for i in 0.. n{
    self.absorb_field(values[i]);
}
    }
    pub fn absorb_poly(&mut self, mut sc: usize, mut cnt: usize, mut poly: &WarpUniDyn)
    {
        self.absorb_usize(cnt);
        for i in 0.. cnt{
    self.absorb_field(poly.evaluations[i]);
}
    }
    pub fn squeeze_fields(&self, mut n: usize) -> Vec<Galois128>
    {
        let mut reader = spec_sponge_finalize_xof(&self.sponge, 31);
        (0..n).map(|_| {
    let mut bytes: Vec<u8> = (0..16).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read_array(n, &mut reader, &mut bytes);
    let mut b16 = [(0 as u8); 16];
    for i in 0.. 16{
    b16[i] = bytes[i];
};
    Galois128(u128::from_le_bytes(b16))
}).collect::<Vec<_>>()
    }
    pub fn squeeze_field_grid(&self, mut r: usize, mut c: usize) -> Vec<Vec<Galois128>>
    {
        let mut reader = spec_sponge_finalize_xof(&self.sponge, 31);
        (0..r).map(|_| {
    (0..c).map(|_| {
    let mut bytes: Vec<u8> = (0..16).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read_array(&mut reader, &mut bytes);
    let mut b16 = [(0 as u8); 16];
    for i in 0.. 16{
    b16[i] = bytes[i];
};
    Galois128(u128::from_le_bytes(b16))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>()
    }
    pub fn squeeze_shift_and_xi(&self, mut shifts: usize, mut logr: usize, mut logdom: usize) -> (Vec<u32>, Vec<Galois128>)
    {
        let mut reader = spec_sponge_finalize_xof(&self.sponge, 31);
        let shifts = (0..shifts).map(|_| {
    let mut value = 0 as usize;
    for position in 0.. (logdom + 7) / 8{
    let mut byte: Vec<u8> = (0..1).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read_array(&mut reader, &mut byte);
    value |= ((byte[0] as usize) << (8 * position));
};
    (value & (((1 as usize) << logdom) - 1)) as u32
}).collect::<Vec<u32>>();
        let xi = (0..logr).map(|_| {
    let mut bytes: Vec<u8> = (0..16).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read_array(&mut reader, &mut bytes);
    let mut b16 = [(0 as u8); 16];
    for i in 0.. 16{
    b16[i] = bytes[i];
};
    Galois128(u128::from_le_bytes(b16))
}).collect::<Vec<Galois128>>();
        (shifts, xi)
    }
}

impl  WarpCode<N, N> for WarpIdentityCode {
    fn encode(&self, mut message: &Vec<Galois128>) -> Vec<Galois128>
    {
        message.clone()
    }
}

impl  WarpCode<K, N> for WarpRepetitionCode {
    fn encode(&self, mut message: &Vec<Galois128>) -> Vec<Galois128>
    {
        (0..n).map(|i| message[i % k]).collect::<Vec<_>>()
    }
}

pub fn gf_mul_u8(mut a: u8, mut b: u8, mut c: u8) -> u8
{
    let mut p: u8 = 0;
    let mut a = a;
    let mut b = b;
    for _ in 0.. 8{
    if (b & 1) != 0{
    p ^= a;
};
    let high = a & 128;
    a <<= 1;
    if high != 0{
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
    let h: u64 = (1 as u64) << 63;
    for _ in 0.. 64{
    if (b & 1) != 0{
    p ^= a;
};
    let high = a & h;
    a <<= 1;
    if high != 0{
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
    let h: u128 = (1 as u128) << 127;
    for _ in 0.. 128{
    if (b & 1) != 0{
    p ^= a;
};
    let high = a & h;
    a <<= 1;
    if high != 0{
    a ^= c;
};
    b >>= 1;
};
    p
}

pub fn gf_invert_u8(mut a: u8, mut c: u8) -> u8
{
    if a == 0{
    return 0;
};
    let e: u32 = 7;
    let msb: u32 = 2;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0.. msb{
    let bit_pos = (msb - 1) - bit_pos_rev;
    let mut tmp = r;
    for _ in 0.. k{
    tmp = gf_mul_u8(tmp, tmp, c);
};
    r = gf_mul_u8(tmp, r, c);
    k *= 2;
    if ((e >> bit_pos) & 1) == 1{
    r = gf_mul_u8(gf_mul_u8(r, r, c), a, c);
    k += 1;
};
};
    gf_mul_u8(r, r, c)
}

pub fn gf_invert_u64(mut a: u64, mut c: u64) -> u64
{
    if a == 0{
    return 0;
};
    let e: u32 = 63;
    let msb: u32 = 5;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0.. msb{
    let bit_pos = (msb - 1) - bit_pos_rev;
    let mut tmp = r;
    for _ in 0.. k{
    tmp = gf_mul_u64(tmp, tmp, c);
};
    r = gf_mul_u64(tmp, r, c);
    k *= 2;
    if ((e >> bit_pos) & 1) == 1{
    r = gf_mul_u64(gf_mul_u64(r, r, c), a, c);
    k += 1;
};
};
    gf_mul_u64(r, r, c)
}

pub fn gf_invert_u128(mut a: u128, mut c: u128) -> u128
{
    if a == 0{
    return 0;
};
    let e: u32 = 127;
    let msb: u32 = 6;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0.. msb{
    let bit_pos = (msb - 1) - bit_pos_rev;
    let mut tmp = r;
    for _ in 0.. k{
    tmp = gf_mul_u128(tmp, tmp, c);
};
    r = gf_mul_u128(tmp, r, c);
    k *= 2;
    if ((e >> bit_pos) & 1) == 1{
    r = gf_mul_u128(gf_mul_u128(r, r, c), a, c);
    k += 1;
};
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
    let bit_pos = (msb - 1) - bit_pos_rev;
    let mut tmp = r;
    for _ in 0.. k{
    tmp = gf_mul_256(tmp, tmp, c);
};
    r = gf_mul_256(tmp, r, c);
    k *= 2;
    if ((e >> bit_pos) & 1) == 1{
    r = gf_mul_256(gf_mul_256(r, r, c), a, c);
    k += 1;
};
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

pub fn derive_roles(mut key: &[u8; BLOCK_LEN]) -> RoleKeys
{
    let zero = [(0 as u8); BLOCK_LEN];
    let mut one = [(0 as u8); BLOCK_LEN];
    one[BLOCK_LEN - 1] = 1;
    let enc_key = encrypt_block_with_keys(&key_expansion(key), &zero);
    let mac_key = encrypt_block_with_keys(&key_expansion(key), &one);
    RoleKeys { enc: key_expansion(&enc_key), mac: key_expansion(&mac_key) }
}

pub fn dbl(mut block: &[u8; BLOCK_LEN]) -> [u8; BLOCK_LEN]
{
    let mut out = [(0 as u8); BLOCK_LEN];
    let mut carry = 0 as u8;
    for i in 0..BLOCK_LEN.rev(){
    let next_carry = block[i] >> 7;
    out[i] = ((block[i] << 1) | carry);
    carry = next_carry;
};
    if carry == 1{
    out[BLOCK_LEN - 1] ^= 135;
};
    out
}

pub fn cmac(mut mac_keys: &[[u8; BLOCK_LEN]; NK_ROUND_KEYS], mut msg: &[u8]) -> [u8; BLOCK_LEN]
{
    let l = encrypt_block_with_keys(mac_keys, &[(0 as u8); BLOCK_LEN]);
    let k1 = dbl(&l);
    let k2 = dbl(&k1);
    let complete = !msg.is_empty() && ((msg.len() % BLOCK_LEN) == 0);
    let n_blocks = if complete{
    msg.len() / BLOCK_LEN
} else {
    (msg.len() / BLOCK_LEN) + 1
};
    let mut state = [(0 as u8); BLOCK_LEN];
    for block_idx in 0.. n_blocks{
    let is_last = (block_idx + 1) == n_blocks;
    let mut block = [(0 as u8); BLOCK_LEN];
    if is_last{
    if complete{
    block.copy_from_slice(&msg[(block_idx * BLOCK_LEN)..((block_idx + 1) * BLOCK_LEN)]);
    for i in 0.. BLOCK_LEN{
    block[i] ^= k1[i];
}
} else {
    let rest = msg.len() - (block_idx * BLOCK_LEN);
    block[..rest].copy_from_slice(&msg[(block_idx * BLOCK_LEN)..]);
    block[rest] = 128;
    for i in 0.. BLOCK_LEN{
    block[i] ^= k2[i];
}
}
} else {
    block.copy_from_slice(&msg[(block_idx * BLOCK_LEN)..((block_idx + 1) * BLOCK_LEN)]);
};
    for i in 0.. BLOCK_LEN{
    state[i] ^= block[i];
};
    state = encrypt_block_with_keys(mac_keys, &state);
};
    state
}

pub fn ctr_block(mut enc_keys: &[[u8; BLOCK_LEN]; NK_ROUND_KEYS], mut nonce: &[u8; BLOCK_LEN], mut i: u64) -> [u8; BLOCK_LEN]
{
    let counter = u128::from_be_bytes(*nonce).wrapping_add((i as u128));
    encrypt_block_with_keys(enc_keys, &counter.to_be_bytes())
}

pub fn aes128_etm_seal(mut key: &[u8; BLOCK_LEN], mut nonce: &[u8; BLOCK_LEN], mut pt: &[u8]) -> (Vec<u8>, [u8; BLOCK_LEN])
{
    let roles = derive_roles(key);
    let mut ct = Vec::with_capacity(pt.len());
    for (i, chunk) in pt.chunks(BLOCK_LEN).enumerate(){
    let ks = ctr_block(&roles.enc, nonce, (i as u64));
    ct.extend(chunk.iter().zip(ks.iter()).map(|(p, k)| (p ^ k)));
};
    let mut msg = Vec::with_capacity(((BLOCK_LEN + ct.len()) + 8));
    msg.extend_from_slice(nonce);
    msg.extend_from_slice(&ct);
    msg.extend_from_slice(&((pt.len() as u64) * 8).to_be_bytes());
    let tag = cmac(&roles.mac, &msg);
    (ct, tag)
}

pub fn aes128_etm_open(mut key: &[u8; BLOCK_LEN], mut nonce: &[u8; BLOCK_LEN], mut ct: &[u8], mut tag: &[u8; BLOCK_LEN]) -> Option<Vec<u8>>
{
    let roles = derive_roles(key);
    let mut msg = Vec::with_capacity(((BLOCK_LEN + ct.len()) + 8));
    msg.extend_from_slice(nonce);
    msg.extend_from_slice(ct);
    msg.extend_from_slice(&((ct.len() as u64) * 8).to_be_bytes());
    let computed = cmac(&roles.mac, &msg);
    if computed != *tag{
    return None;
};
    let mut pt = Vec::with_capacity(ct.len());
    for (i, chunk) in ct.chunks(BLOCK_LEN).enumerate(){
    let ks = ctr_block(&roles.enc, nonce, (i as u64));
    pt.extend(chunk.iter().zip(ks.iter()).map(|(c, k)| (c ^ k)));
};
    Some(pt)
}

pub fn ascon_round(mut x: [u64; 5], mut c: u64) -> [u64; 5]
{
    let x0 = x[0] ^ x[4];
    let x2 = x[2] ^ x[1] ^ c;
    let x4 = x[4] ^ x[3];
    let tx0 = x0 ^ (!x[1] & x2);
    let tx1 = x[1] ^ (!x2 & x[3]);
    let tx2 = x2 ^ (!x[3] & x4);
    let tx3 = x[3] ^ (!x4 & x0);
    let tx4 = x4 ^ (!x0 & x[1]);
    let tx1 = tx1 ^ tx0;
    let tx3 = tx3 ^ tx2;
    let tx0 = tx0 ^ tx4;
    let y0 = tx0 ^ ((tx0 >> 9) | (tx0 << 55));
    let y1 = tx1 ^ ((tx1 >> 22) | (tx1 << 42));
    let y2 = tx2 ^ ((tx2 >> 5) | (tx2 << 59));
    let y3 = tx3 ^ ((tx3 >> 7) | (tx3 << 57));
    let y4 = tx4 ^ ((tx4 >> 34) | (tx4 << 30));
    [(tx0 ^ ((y0 >> 19) | (y0 << 45))), (tx1 ^ ((y1 >> 39) | (y1 << 25))), !(tx2 ^ ((y2 >> 1) | (y2 << 63))), (tx3 ^ ((y3 >> 10) | (y3 << 54))), (tx4 ^ ((y4 >> 7) | (y4 << 57)))]
}

pub fn ascon_p8(mut state: &mut [u64; 5])
{
    for round in 4.. 12{
    let new = ascon_round(*state, ASCON_RC[round]);
    for i in 0.. 5{
    state[i] = new[i];
};
}
}

pub fn ascon_p12(mut state: &mut [u64; 5])
{
    for round in 0.. 12{
    let new = ascon_round(*state, ASCON_RC[round]);
    for i in 0.. 5{
    state[i] = new[i];
};
}
}

pub fn ascon_hash256(mut input: &[u8]) -> [u8; 32]
{
    let mut state = ASCON_HASH256_IV;
    let n = input.len();
    let mut i = 0;
    while ((n - i) >= 8){
    let mut w = [(0 as u8); 8];
    w.copy_from_slice(&input[i..(i + 8)]);
    state[0] ^= u64::from_le_bytes(w);
    ascon_p12(&mut state);
    i += 8;
};
    let rem = n - i;
    let mut last = [(0 as u8); 8];
    last[..rem].copy_from_slice(&input[i..]);
    state[0] ^= u64::from_le_bytes(last);
    state[0] ^= ((1 as u64) << (8 * rem));
    ascon_p12(&mut state);
    let mut out = [(0 as u8); 32];
    for j in 0.. 4{
    out[(j * 8)..((j * 8) + 8)].copy_from_slice(&state[0].to_le_bytes());
    if j < 3{
    ascon_p12(&mut state);
};
};
    out
}

pub fn ascon_hash256_array(mut n: usize, mut input: &Vec<u8>) -> [u8; 32]
{
    let mut state = ASCON_HASH256_IV;
    let n = n;
    let num_full = n / 8;
    for i in 0.. num_full{
    let mut w = [(0 as u8); 8];
    for j in 0.. 8{
    w[j] = input[(i * 8) + j];
};
    state[0] ^= u64::from_le_bytes(w);
    ascon_p12(&mut state);
};
    let rem = n - (num_full * 8);
    let mut last = [(0 as u8); 8];
    for j in 0.. 8{
    let idx = (num_full * 8) + j;
    if idx < n{
    last[j] = input[idx];
};
};
    state[0] ^= u64::from_le_bytes(last);
    state[0] ^= ((1 as u64) << (8 * rem));
    ascon_p12(&mut state);
    let mut out = [(0 as u8); 32];
    for j in 0.. 4{
    out[(j * 8)..((j * 8) + 8)].copy_from_slice(&state[0].to_le_bytes());
    if j < 3{
    ascon_p12(&mut state);
};
};
    out
}

pub fn aead_pad(mut n: usize) -> u64
{
    (128 as u64) << (56 - (8 * n))
}

pub fn be_u64(mut input: &[u8]) -> u64
{
    let mut b = [(0 as u8); 8];
    b.copy_from_slice(input);
    u64::from_be_bytes(b)
}

pub fn be_u64_partial(mut input: &[u8]) -> u64
{
    let mut b = [(0 as u8); 8];
    b[..input.len()].copy_from_slice(input);
    u64::from_be_bytes(b)
}

pub fn ascon128a_encrypt(mut key: &[u8; 16], mut nonce: &[u8; 16], mut ad: &[u8], mut plaintext: &[u8]) -> (Vec<u8>, [u8; 16])
{
    let k1 = be_u64(&key[..8]);
    let k2 = be_u64(&key[8..16]);
    let mut state = [ASCON_128A_IV, k1, k2, be_u64(&nonce[..8]), be_u64(&nonce[8..16])];
    ascon_p12(&mut state);
    state[3] ^= k1;
    state[4] ^= k2;
    if !ad.is_empty(){
    let mut i = 0;
    while ((ad.len() - i) >= 16){
    state[0] ^= be_u64(&ad[i..(i + 8)]);
    state[1] ^= be_u64(&ad[(i + 8)..(i + 16)]);
    ascon_p8(&mut state);
    i += 16;
};
    let rem = ad.len() - i;
    let (sidx, lastlen) = if rem >= 8{
    state[0] ^= be_u64(&ad[i..(i + 8)]);
    (1, (rem - 8))
} else {
    (0, rem)
};
    state[sidx] ^= aead_pad(lastlen);
    if lastlen > 0{
    let off = i + if sidx == 1{
    8
} else {
    0
};
    state[sidx] ^= be_u64_partial(&ad[off..(off + lastlen)]);
};
    ascon_p8(&mut state);
};
    state[4] ^= 1;
    let mut ciphertext = alloc::vec::Vec::with_capacity(plaintext.len());
    let mut i = 0;
    while ((plaintext.len() - i) >= 16){
    state[0] ^= be_u64(&plaintext[i..(i + 8)]);
    ciphertext.extend_from_slice(&state[0].to_be_bytes());
    state[1] ^= be_u64(&plaintext[(i + 8)..(i + 16)]);
    ciphertext.extend_from_slice(&state[1].to_be_bytes());
    ascon_p8(&mut state);
    i += 16;
};
    let rem = plaintext.len() - i;
    if rem >= 8{
    state[0] ^= be_u64(&plaintext[i..(i + 8)]);
    ciphertext.extend_from_slice(&state[0].to_be_bytes());
    let rest = rem - 8;
    state[1] ^= aead_pad(rest);
    if rest > 0{
    state[1] ^= be_u64_partial(&plaintext[(i + 8)..((i + 8) + rest)]);
    let keystream = state[1].to_be_bytes();
    ciphertext.extend_from_slice(&keystream[..rest]);
}
} else {
    state[0] ^= aead_pad(rem);
    if rem > 0{
    state[0] ^= be_u64_partial(&plaintext[i..(i + rem)]);
    let keystream = state[0].to_be_bytes();
    ciphertext.extend_from_slice(&keystream[..rem]);
}
};
    state[2] ^= k1;
    state[3] ^= k2;
    ascon_p12(&mut state);
    state[3] ^= k1;
    state[4] ^= k2;
    let mut tag = [(0 as u8); 16];
    tag[..8].copy_from_slice(&state[3].to_be_bytes());
    tag[8..].copy_from_slice(&state[4].to_be_bytes());
    (ciphertext, tag)
}

pub fn ascon128a_decrypt(mut key: &[u8; 16], mut nonce: &[u8; 16], mut ad: &[u8], mut ciphertext: &[u8], mut tag: &[u8; 16]) -> Option<Vec<u8>>
{
    let k1 = be_u64(&key[..8]);
    let k2 = be_u64(&key[8..16]);
    let mut state = [ASCON_128A_IV, k1, k2, be_u64(&nonce[..8]), be_u64(&nonce[8..16])];
    ascon_p12(&mut state);
    state[3] ^= k1;
    state[4] ^= k2;
    if !ad.is_empty(){
    let mut i = 0;
    while ((ad.len() - i) >= 16){
    state[0] ^= be_u64(&ad[i..(i + 8)]);
    state[1] ^= be_u64(&ad[(i + 8)..(i + 16)]);
    ascon_p8(&mut state);
    i += 16;
};
    let rem = ad.len() - i;
    let (sidx, lastlen) = if rem >= 8{
    state[0] ^= be_u64(&ad[i..(i + 8)]);
    (1, (rem - 8))
} else {
    (0, rem)
};
    state[sidx] ^= aead_pad(lastlen);
    if lastlen > 0{
    let off = i + if sidx == 1{
    8
} else {
    0
};
    state[sidx] ^= be_u64_partial(&ad[off..(off + lastlen)]);
};
    ascon_p8(&mut state);
};
    state[4] ^= 1;
    let mut plaintext = alloc::vec::Vec::with_capacity(ciphertext.len());
    let mut i = 0;
    while ((ciphertext.len() - i) >= 16){
    plaintext.extend_from_slice(&(state[0] ^ be_u64(&ciphertext[i..(i + 8)])).to_be_bytes());
    state[0] = be_u64(&ciphertext[i..(i + 8)]);
    plaintext.extend_from_slice(&(state[1] ^ be_u64(&ciphertext[(i + 8)..(i + 16)])).to_be_bytes());
    state[1] = be_u64(&ciphertext[(i + 8)..(i + 16)]);
    ascon_p8(&mut state);
    i += 16;
};
    let rem = ciphertext.len() - i;
    if rem >= 8{
    plaintext.extend_from_slice(&(state[0] ^ be_u64(&ciphertext[i..(i + 8)])).to_be_bytes());
    state[0] = be_u64(&ciphertext[i..(i + 8)]);
    let rest = rem - 8;
    state[1] ^= aead_pad(rest);
    if rest > 0{
    let pt1 = state[1] ^ be_u64_partial(&ciphertext[(i + 8)..((i + 8) + rest)]);
    plaintext.extend_from_slice(&pt1.to_be_bytes()[..rest]);
    let mut s1 = state[1].to_be_bytes();
    s1[..rest].copy_from_slice(&ciphertext[(i + 8)..((i + 8) + rest)]);
    state[1] = be_u64(&s1);
}
} else {
    state[0] ^= aead_pad(rem);
    if rem > 0{
    let pt0 = state[0] ^ be_u64_partial(&ciphertext[i..(i + rem)]);
    plaintext.extend_from_slice(&pt0.to_be_bytes()[..rem]);
    let mut s0 = state[0].to_be_bytes();
    s0[..rem].copy_from_slice(&ciphertext[i..(i + rem)]);
    state[0] = be_u64(&s0);
}
};
    state[2] ^= k1;
    state[3] ^= k2;
    ascon_p12(&mut state);
    state[3] ^= k1;
    state[4] ^= k2;
    let mut expected = [(0 as u8); 16];
    expected[..8].copy_from_slice(&state[3].to_be_bytes());
    expected[8..].copy_from_slice(&state[4].to_be_bytes());
    if expected == *tag{
    Some(plaintext)
} else {
    None
}
}

pub fn exponent(mut log_q_lwe: usize, mut big_n: usize, mut x: u32) -> usize
{
    (x as usize) & ((2 * big_n) - 1)
}

pub fn binfhe_blind_rotate(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ct: &BinfheLweCiphertextDyn, mut test_poly: &[u32; BIG_N], mut bsk: &[BinfheRgswCiphertextDyn]) -> BinfheRlweCiphertextDyn
{
    let two_n = 2 * big_n;
    let b_exp = exponent::<LOG_Q_LWE, BIG_N>(log_q_lwe, big_n, ct.b);
    let mut acc = binfhe_rlwe_trivial::<BIG_N, LOG_Q>(big_n, log_q, test_poly);
    if b_exp != 0{
    acc = binfhe_rlwe_rotate::<BIG_N, LOG_Q>(big_n, log_q, &acc, (two_n - b_exp));
};
    for (i, row) in bsk.iter().enumerate(){
    let a_exp = exponent::<LOG_Q_LWE, BIG_N>(log_q_lwe, big_n, ct.a[i]);
    if a_exp != 0{
    let rotated = binfhe_rlwe_rotate::<BIG_N, LOG_Q>(big_n, log_q, &acc, a_exp);
    acc = binfhe_rgsw_cmux::<BIG_N, LOG_Q, BS_ELL, BS_BASE_LOG>(row, &rotated, &acc);
};
};
    acc
}

pub fn encrypt_scaled_poly<R: SpecRng>(mut big_n: usize, mut log_q: usize, mut eta: usize, mut msg: &[u32; BIG_N], mut level: usize, mut base_log: u32, mut sk: &BinfheRlweSecretKeyDyn, mut rng: &mut R) -> BinfheRlweCiphertextDyn
{
    let g = gadget::<LOG_Q>::level_factor(log_q, base_log, level);
    let mut scaled = [(0 as u32); big_n];
    for i in 0.. big_n{
    scaled[i] = torus::<LOG_Q>::mul_exact(log_q, msg[i], g);
};
    binfhe_rlwe_encrypt_poly::<BIG_N, LOG_Q, ETA, R>(big_n, log_q, eta, &scaled, sk, rng)
}

pub fn gen_circuit_bootstrapping_key<R: SpecRng>(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut priv_ell: usize, mut priv_base_log: usize, mut eta: usize, mut lwe_sk: &BinfheLweSecretKeyDyn, mut rlwe_sk: &BinfheRlweSecretKeyDyn, mut rng: &mut R) -> CircuitBootstrappingKeyDyn
{
    let bk = binfhe_gen_bootstrapping_key::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, ETA, R>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, eta, lwe_sk, rlwe_sk, rng);
    let zero = [(0 as u32); big_n];
    let neg_one_const: [u32; BIG_N] = {
    let mut p = [(0 as u32); big_n];
    p[0] = torus::<LOG_Q>::torus_neg(log_q, 1);
    p
};
    let neg_sk: [u32; BIG_N] = (0..big_n).map(|i| torus::<LOG_Q>::torus_neg(log_q, rlwe_sk.key[i])).collect::<Vec<_>>();
    let one_const: [u32; BIG_N] = {
    let mut p = [(0 as u32); big_n];
    p[0] = 1;
    p
};
    let mut a_col = alloc::vec::Vec::with_capacity(big_n);
    for i in 0.. big_n{
    let msg = if rlwe_sk.key[i] == 1{
    &rlwe_sk.key
} else {
    &zero
};
    a_col.push((0..priv_ell).map(|l| {
    encrypt_scaled_poly::<BIG_N, LOG_Q, ETA, R>(big_n, log_q, eta, msg, l, priv_base_log, rlwe_sk, rng)
}).collect::<Vec<_>>());
};
    let mut b_col = alloc::vec::Vec::with_capacity(big_n);
    for i in 0.. big_n{
    let msg = if rlwe_sk.key[i] == 1{
    &neg_one_const
} else {
    &zero
};
    b_col.push((0..priv_ell).map(|l| {
    encrypt_scaled_poly::<BIG_N, LOG_Q, ETA, R>(big_n, log_q, eta, msg, l, priv_base_log, rlwe_sk, rng)
}).collect::<Vec<_>>());
};
    let a_body = (0..priv_ell).map(|l| {
    encrypt_scaled_poly::<BIG_N, LOG_Q, ETA, R>(big_n, log_q, eta, &neg_sk, l, priv_base_log, rlwe_sk, rng)
}).collect::<Vec<_>>();
    let b_body = (0..priv_ell).map(|l| {
    encrypt_scaled_poly::<BIG_N, LOG_Q, ETA, R>(big_n, log_q, eta, &one_const, l, priv_base_log, rlwe_sk, rng)
}).collect::<Vec<_>>();
    CircuitBootstrappingKeyDyn { bk: bk, privksk: PrivateKeySwitchingKeyDyn { a_col: a_col, b_col: b_col, a_body: a_body, b_body: b_body, big_n: 0, priv_ell: 0 }, n_lwe: 0, big_n: 0, bs_ell: 0, ks_ell: 0, priv_ell: 0 }
}

pub fn priv_ks(mut big_n: usize, mut log_q: usize, mut priv_ell: usize, mut priv_base_log: usize, mut src: &BinfheLweCiphertextDyn, mut col: &[[BinfheRlweCiphertextDyn; PRIV_ELL]], mut body: &[BinfheRlweCiphertextDyn; PRIV_ELL]) -> BinfheRlweCiphertextDyn
{
    let mut out = BinfheRlweCiphertextDyn { a: [(0 as u32); big_n], b: [(0 as u32); big_n], n: 0 };
    for i in 0.. big_n{
    let digits = gadget::<LOG_Q, PRIV_ELL, PRIV_BASE_LOG>::gadget_decompose(log_q, priv_ell, priv_base_log, src.a[i]);
    for (l, ..) in digits.iter().enumerate(){
    if d == 0{
    continue;
};
    let entry = &col[i][l];
    for k in 0.. big_n{
    out.a[k] = out.a[k].wrapping_add(d.wrapping_mul(entry.a[k]));
    out.b[k] = out.b[k].wrapping_add(d.wrapping_mul(entry.b[k]));
};
};
};
    let digits = gadget::<LOG_Q, PRIV_ELL, PRIV_BASE_LOG>::gadget_decompose(log_q, priv_ell, priv_base_log, src.b);
    for (l, ..) in digits.iter().enumerate(){
    if d == 0{
    continue;
};
    let entry = &body[l];
    for k in 0.. big_n{
    out.a[k] = out.a[k].wrapping_add(d.wrapping_mul(entry.a[k]));
    out.b[k] = out.b[k].wrapping_add(d.wrapping_mul(entry.b[k]));
};
};
    for k in 0.. big_n{
    out.a[k] = torus::<LOG_Q>::reduce(log_q, out.a[k]);
    out.b[k] = torus::<LOG_Q>::reduce(log_q, out.b[k]);
};
    out
}

pub fn level_test_poly(mut big_n: usize, mut log_q: usize, mut level: usize, mut bs_base_log: u32, mut k_max: usize) -> [u32; BIG_N]
{
    let width = big_n >> k_max;
    let g = gadget::<LOG_Q>::level_factor(log_q, bs_base_log, level);
    let mut poly = [(0 as u32); big_n];
    for p in width.. 2 * width{
    poly[p] = g;
};
    poly
}

pub fn circuit_bootstrap(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut priv_ell: usize, mut priv_base_log: usize, mut ct: &BinfheLweCiphertextDyn, mut cbk: &CircuitBootstrappingKeyDyn, mut k_max: usize) -> BinfheRgswCiphertextDyn
{
    let delta = wire_delta::<LOG_Q_LWE>(log_q_lwe, (k_max as usize));
    let centered = binfhe_lwe_add_const::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, ct, (delta / 2));
    let rows = (0..ell).map(|j| {
    let test_poly = level_test_poly::<BIG_N, LOG_Q>(big_n, log_q, j, bs_base_log, k_max);
    let acc = binfhe_blind_rotate::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, BS_ELL, BS_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, &centered, &test_poly, &cbk.bk.bsk);
    let extracted = binfhe_sample_extract::<BIG_N, LOG_Q>(big_n, log_q, &acc);
    let rlwe0 = priv_ks::<BIG_N, LOG_Q, PRIV_ELL, PRIV_BASE_LOG>(big_n, log_q, priv_ell, priv_base_log, &extracted, &cbk.privksk.a_col, &cbk.privksk.a_body);
    let rlwe1 = priv_ks::<BIG_N, LOG_Q, PRIV_ELL, PRIV_BASE_LOG>(big_n, log_q, priv_ell, priv_base_log, &extracted, &cbk.privksk.b_col, &cbk.privksk.b_body);
    BinfheRgswRowDyn { rlwe0: rlwe0, rlwe1: rlwe1, n: 0 }
}).collect::<Vec<_>>();
    BinfheRgswCiphertextDyn { rows: rows, n: 0, ell: 0 }
}

pub fn level_shift(mut log: u32, mut base_log: u32, mut j: usize) -> u32
{
    log.saturating_sub((base_log * ((j as u32) + 1)))
}

pub fn level_bits(mut log: u32, mut base_log: u32, mut j: usize) -> u32
{
    let remaining = log.saturating_sub((base_log * (j as u32)));
    if remaining < base_log{
    remaining
} else {
    base_log
}
}

pub fn level_factor(mut log: usize, mut base_log: u32, mut j: usize) -> u32
{
    torus::<LOG>::reduce(log, ((1 as u32) << level_shift(log, base_log, j)))
}

pub fn gadget_decompose(mut log: usize, mut ell: usize, mut base_log: usize, mut x: u32) -> [u32; ELL]
{
    let mut digits = [(0 as u32); ell];
    for (j, d) in digits.iter_mut().enumerate(){
    let shift = level_shift(log, base_log, j);
    let bits = level_bits(log, base_log, j);
    let m = if bits >= 32{
    u32::MAX
} else {
    ((1 as u32) << bits) - 1
};
    *d = ((x >> shift) & m);
};
    digits
}

pub fn gadget_poly_decompose(mut n: usize, mut log: usize, mut ell: usize, mut base_log: usize, mut p: &[u32; N]) -> [[u32; N]; ELL]
{
    let mut out = [[(0 as u32); n]; ell];
    for i in 0.. n{
    let digits = gadget_decompose::<LOG, ELL, BASE_LOG>(log, ell, base_log, p[i]);
    for j in 0.. ell{
    out[j][i] = digits[j];
};
};
    out
}

pub fn binfhe_gen_bootstrapping_key<R: SpecRng>(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut eta: usize, mut lwe_sk: &BinfheLweSecretKeyDyn, mut rlwe_sk: &BinfheRlweSecretKeyDyn, mut rng: &mut R) -> BinfheBootstrappingKeyDyn
{
    let bsk = (0..n_lwe).map(|i| {
    binfhe_rgsw_encrypt::<BIG_N, LOG_Q, BS_ELL, BS_BASE_LOG, ETA, R>(big_n, log_q, bs_ell, bs_base_log, eta, (lwe_sk.key[i] != 0), rlwe_sk, rng)
}).collect::<Vec<_>>();
    let ksk = BinfheKeySwitchingKeyDyn { ksk: (0..big_n).map(|i| {
    (0..ks_ell).map(|j| {
    let msg = rlwe_sk.key[i].wrapping_mul(gadget::<LOG_MOD_KS>::level_factor(log_mod_ks, ks_base_log, j));
    binfhe_lwe_encrypt_raw::<N_LWE, LOG_MOD_KS, ETA, R>(n_lwe, log_mod_ks, eta, torus::<LOG_MOD_KS>::reduce(log_mod_ks, msg), lwe_sk, rng)
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), n_lwe: 0, big_n: 0, ks_ell: 0 };
    BinfheBootstrappingKeyDyn { bsk: bsk, ksk: ksk, n_lwe: 0, big_n: 0, bs_ell: 0, ks_ell: 0 }
}

pub fn binfhe_key_switch<K: AsKeySwitchingKey<N_LWE, BIG_N, KS_ELL> + Sized>(mut n_lwe: usize, mut big_n: usize, mut log_mod_ks: usize, mut ks_ell: usize, mut ks_base_log: usize, mut ct: &BinfheLweCiphertextDyn, mut ksk: &K) -> BinfheLweCiphertextDyn
{
    let ksk_rows = ksk.ksk_rows();
    let mut out_a = [(0 as u32); n_lwe];
    let mut out_b = ct.b;
    for i in 0.. big_n{
    let digits = gadget::<LOG_MOD_KS, KS_ELL, KS_BASE_LOG>::gadget_decompose(log_mod_ks, ks_ell, ks_base_log, ct.a[i]);
    for j in 0.. ks_ell{
    let d = digits[j];
    if d == 0{
    continue;
};
    let entry = &ksk_rows[i][j];
    for k in 0.. n_lwe{
    out_a[k] = out_a[k].wrapping_sub(d.wrapping_mul(entry.a[k]));
};
    out_b = out_b.wrapping_sub(d.wrapping_mul(entry.b));
};
};
    for k in 0.. n_lwe{
    out_a[k] = torus::<LOG_MOD_KS>::reduce(log_mod_ks, out_a[k]);
};
    BinfheLweCiphertextDyn { a: out_a, b: torus::<LOG_MOD_KS>::reduce(log_mod_ks, out_b), n: 0 }
}

pub fn check_lut_shape(mut addr_bits: usize, mut table_len: usize, mut big_n: usize, mut log_q: u32, mut log_q_lwe: u32, mut k_max: usize) -> Result<usize, LutError>
{
    if (addr_bits == 0) || (addr_bits >= (usize::BITS as usize)){
    return Err(LutError::AddressShapeInvalid);
};
    if table_len != ((1 as usize) << addr_bits){
    return Err(LutError::AddressShapeInvalid);
};
    if addr_bits > k_max{
    return Err(LutError::ArityExceedsCircuitMax);
};
    if (((k_max as u32) + 2) > log_q_lwe) || !big_n.is_power_of_two() || (((1 as usize) << k_max) > big_n) || (((1 as usize) << log_q_lwe) != (2 * big_n)) || (log_q_lwe > log_q) || (log_q > 32){
    return Err(LutError::ShapeUnsupported);
};
    Ok((big_n >> k_max))
}

pub fn table_is_constant(mut logical: &[bool]) -> bool
{
    let mut i = 1;
    while (i < logical.len()){
    if logical[i] != logical[0]{
    return false;
};
    i += 1;
};
    true
}

pub fn fill_test_poly(mut big_n: usize, mut logical: &[bool], mut addr_bits: usize, mut k_max: usize, mut log_q: u32, mut log_q_lwe: u32) -> [u32; BIG_N]
{
    let table_len = (1 as usize) << addr_bits;
    let is_constant = table_is_constant(logical);
    let delta_shift = (log_q_lwe - 1) - (k_max as u32);
    let value = if log_q >= 32{
    delta_out_full_width(delta_shift, (log_q - log_q_lwe))
} else {
    ((1 as u32) << (delta_shift + (log_q - log_q_lwe))) & (((1 as u32) << log_q) - 1)
};
    let width = big_n >> k_max;
    let used = if is_constant{
    0
} else {
    table_len * width
};
    let mut test_poly = [(0 as u32); big_n];
    let mut p = 0;
    while (p < used){
    test_poly[p] = if logical[p / width]{
    value
} else {
    0
};
    p += 1;
};
    test_poly
}

pub fn delta_out_full_width(mut delta_shift: u32, mut upscale: u32) -> u32
{
    (1 as u32) << (delta_shift + upscale)
}

pub fn wire_delta(mut log_q_lwe: usize, mut k_max: usize) -> u32
{
    (1 as u32) << ((log_q_lwe - 1) - (k_max as u32))
}

pub fn binfhe_gen_lwe_secret_key<R: SpecRng>(mut n: usize, mut rng: &mut R) -> BinfheLweSecretKeyDyn
{
    let mut key = [(0 as u8); n];
    let mut i = 0;
    while (i < n){
    let mut word = rng.next_u32();
    let take = (n - i).min(32);
    for _ in 0.. take{
    key[i] = ((word & 1) as u8);
    word >>= 1;
    i += 1;
};
};
    BinfheLweSecretKeyDyn { key: key, n: 0 }
}

pub fn binfhe_lwe_encrypt<R: SpecRng>(mut n: usize, mut log_m: usize, mut eta: usize, mut m: bool, mut delta: u32, mut sk: &BinfheLweSecretKeyDyn, mut rng: &mut R) -> BinfheLweCiphertextDyn
{
    let msg = if m{
    delta
} else {
    0
};
    binfhe_lwe_encrypt_raw::<N, LOG_M, ETA, R>(n, log_m, eta, msg, sk, rng)
}

pub fn binfhe_lwe_encrypt_raw<R: SpecRng>(mut n: usize, mut log_m: usize, mut eta: usize, mut msg: u32, mut sk: &BinfheLweSecretKeyDyn, mut rng: &mut R) -> BinfheLweCiphertextDyn
{
    let mut a = [(0 as u32); n];
    for ai in a.iter_mut(){
    *ai = torus::<LOG_M>::reduce(log_m, rng.next_u32());
};
    let mut dot = 0 as u32;
    for i in 0.. n{
    dot = dot.wrapping_add(a[i].wrapping_mul((sk.key[i] as u32)));
};
    let e = sampler::<LOG_M, ETA, R>::sample_error(log_m, eta, rng);
    let b = torus::<LOG_M>::reduce(log_m, dot.wrapping_add(e).wrapping_add(msg));
    BinfheLweCiphertextDyn { a: a, b: b, n: 0 }
}

pub fn lwe_phase(mut n: usize, mut log_m: usize, mut ct: &BinfheLweCiphertextDyn, mut sk: &BinfheLweSecretKeyDyn) -> u32
{
    let mut dot = 0 as u32;
    for i in 0.. n{
    dot = dot.wrapping_add(ct.a[i].wrapping_mul((sk.key[i] as u32)));
};
    torus::<LOG_M>::reduce(log_m, ct.b.wrapping_sub(dot))
}

pub fn lwe_decode(mut log_m: usize, mut phase: u32, mut delta: u32) -> bool
{
    torus::<LOG_M>::reduce(log_m, phase.wrapping_sub((delta / 2))) < delta
}

pub fn binfhe_lwe_decrypt(mut n: usize, mut log_m: usize, mut ct: &BinfheLweCiphertextDyn, mut sk: &BinfheLweSecretKeyDyn, mut delta: u32) -> bool
{
    lwe_decode::<LOG_M>(log_m, lwe_phase::<N, LOG_M>(n, log_m, ct, sk), delta)
}

pub fn binfhe_lwe_add(mut n: usize, mut log_m: usize, mut x: &BinfheLweCiphertextDyn, mut y: &BinfheLweCiphertextDyn) -> BinfheLweCiphertextDyn
{
    let mut a = [(0 as u32); n];
    for i in 0.. n{
    a[i] = torus::<LOG_M>::torus_add(log_m, x.a[i], y.a[i]);
};
    BinfheLweCiphertextDyn { a: a, b: torus::<LOG_M>::torus_add(log_m, x.b, y.b), n: 0 }
}

pub fn binfhe_lwe_sub(mut n: usize, mut log_m: usize, mut x: &BinfheLweCiphertextDyn, mut y: &BinfheLweCiphertextDyn) -> BinfheLweCiphertextDyn
{
    let mut a = [(0 as u32); n];
    for i in 0.. n{
    a[i] = torus::<LOG_M>::torus_sub(log_m, x.a[i], y.a[i]);
};
    BinfheLweCiphertextDyn { a: a, b: torus::<LOG_M>::torus_sub(log_m, x.b, y.b), n: 0 }
}

pub fn binfhe_lwe_neg(mut n: usize, mut log_m: usize, mut x: &BinfheLweCiphertextDyn) -> BinfheLweCiphertextDyn
{
    let mut a = [(0 as u32); n];
    for i in 0.. n{
    a[i] = torus::<LOG_M>::torus_neg(log_m, x.a[i]);
};
    BinfheLweCiphertextDyn { a: a, b: torus::<LOG_M>::torus_neg(log_m, x.b), n: 0 }
}

pub fn binfhe_lwe_scale(mut n: usize, mut log_m: usize, mut x: &BinfheLweCiphertextDyn, mut c: u32) -> BinfheLweCiphertextDyn
{
    let mut a = [(0 as u32); n];
    for i in 0.. n{
    a[i] = torus::<LOG_M>::mul_exact(log_m, x.a[i], c);
};
    BinfheLweCiphertextDyn { a: a, b: torus::<LOG_M>::mul_exact(log_m, x.b, c), n: 0 }
}

pub fn binfhe_lwe_add_const(mut n: usize, mut log_m: usize, mut x: &BinfheLweCiphertextDyn, mut c: u32) -> BinfheLweCiphertextDyn
{
    BinfheLweCiphertextDyn { a: x.a, b: torus::<LOG_M>::torus_add(log_m, x.b, c), n: 0 }
}

pub fn binfhe_not(mut n: usize, mut log_m: usize, mut x: &BinfheLweCiphertextDyn, mut delta: u32) -> BinfheLweCiphertextDyn
{
    let mut out = binfhe_lwe_neg::<N, LOG_M>(n, log_m, x);
    out.b = torus::<LOG_M>::torus_add(log_m, out.b, delta);
    out
}

pub fn binfhe_trivial(mut n: usize, mut log_m: usize, mut m: bool, mut delta: u32) -> BinfheLweCiphertextDyn
{
    BinfheLweCiphertextDyn { a: [(0 as u32); n], b: if m{
    delta
} else {
    0
}, n: 0 }
}

pub fn mod_switch(mut from: usize, mut to: usize, mut x: u32) -> u32
{
    if to >= from{
    super::torus::<FROM, TO>::embed_up(from, to, x)
} else {
    let shift = from - to;
    let half = (1 as u32) << (shift - 1);
    let rounded = (((x as u64) + (half as u64)) >> shift) as u32;
    super::torus::<TO>::reduce(to, rounded)
}
}

pub fn mod_switch_lwe(mut n: usize, mut from: usize, mut to: usize, mut ct: &BinfheLweCiphertextDyn) -> BinfheLweCiphertextDyn
{
    let mut a = [(0 as u32); n];
    for i in 0.. n{
    a[i] = mod_switch::<FROM, TO>(from, to, ct.a[i]);
};
    super::lwe::BinfheLweCiphertext { a: a, b: mod_switch::<FROM, TO>(from, to, ct.b) }
}

pub fn max_lut_arity(mut log_q_lwe: u32) -> u32
{
    log_q_lwe.saturating_sub(2)
}

pub fn selector_margin(mut log_q_lwe: u32, mut k: usize, mut input_noise_bound: u32) -> bool
{
    if ((k as u32) + 2) >= log_q_lwe{
    return false;
};
    let margin = (1 as u32) << ((log_q_lwe - (k as u32)) - 2);
    let weight = (((1 as u64) << k) - 1) as u32;
    weight.saturating_mul(input_noise_bound) < margin
}

pub fn check_profile(mut n_lwe: usize, mut big_n: usize, mut log_q: u32, mut log_q_lwe: u32, mut log_mod_ks: u32, mut bs_base_log: u32, mut bs_ell: usize, mut ks_base_log: u32, mut ks_ell: usize, mut priv_base_log: u32, mut priv_ell: usize)
{
}

pub fn binfhe_pbs_core<BK: crate::binfhe::keys::AsBinfheBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL> + Sized>(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut ct: &BinfheLweCiphertextDyn, mut test_poly: &[u32; BIG_N], mut bk: &BK) -> BinfheLweCiphertextDyn
{
    let acc = binfhe_blind_rotate::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, BS_ELL, BS_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, ct, test_poly, bk.bsk_rows());
    let extracted = binfhe_sample_extract::<BIG_N, LOG_Q>(big_n, log_q, &acc);
    let at_ks = mod_switch_lwe::<BIG_N, LOG_Q, LOG_MOD_KS>(big_n, log_q, log_mod_ks, &extracted);
    let switched = binfhe_key_switch::<N_LWE, BIG_N, LOG_MOD_KS, KS_ELL, KS_BASE_LOG, _>(n_lwe, big_n, log_mod_ks, ks_ell, ks_base_log, &at_ks, &bk.ksk_ref());
    mod_switch_lwe::<N_LWE, LOG_MOD_KS, LOG_Q_LWE>(n_lwe, log_mod_ks, log_q_lwe, &switched)
}

pub fn binfhe_lut_read(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut addr_bits: usize, mut table_len: usize, mut k_max: usize, mut addr: &[BinfheLweCiphertextDyn; ADDR_BITS], mut lut: &LutDyn, mut bk: &BinfheBootstrappingKeyDyn) -> BinfheLweCiphertextDyn
{
    let delta = wire_delta::<LOG_Q_LWE>(log_q_lwe, k_max);
    if lut.is_constant(){
    return binfhe_trivial::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, lut.constant_value(), delta);
};
    let mut combined = binfhe_trivial::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, false, 0);
    for (j, bit) in addr.iter().enumerate(){
    let scaled = binfhe_lwe_scale::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, bit, ((1 as u32) << j));
    combined = binfhe_lwe_add::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, &combined, &scaled);
};
    combined = binfhe_lwe_add_const::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, &combined, (delta / 2));
    binfhe_pbs_core::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, _>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, &combined, lut.test_polynomial(), bk)
}

pub fn binfhe_lut_read_dyn(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut inputs: &[BinfheLweCiphertextDyn], mut table: &[bool], mut k_max: usize, mut bk: &BinfheBootstrappingKeyDyn) -> BinfheLweCiphertextDyn
{
    let delta = wire_delta::<LOG_Q_LWE>(log_q_lwe, (k_max as usize));
    if table_is_constant(table){
    return binfhe_trivial::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, table[0], delta);
};
    let arity = table.len().trailing_zeros() as usize;
    let test_poly = fill_test_poly::<BIG_N>(big_n, table, arity, (k_max as usize), log_q, log_q_lwe);
    let mut combined = binfhe_trivial::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, false, 0);
    for (j, bit) in inputs.iter().enumerate(){
    let scaled = binfhe_lwe_scale::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, bit, ((1 as u32) << j));
    combined = binfhe_lwe_add::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, &combined, &scaled);
};
    combined = binfhe_lwe_add_const::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, &combined, (delta / 2));
    binfhe_pbs_core::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, _>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, &combined, &test_poly, bk)
}

pub fn binfhe_gate_and(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut k_max: usize, mut a: BinfheLweCiphertextDyn, mut b: BinfheLweCiphertextDyn, mut bk: &BinfheBootstrappingKeyDyn) -> BinfheLweCiphertextDyn
{
    binfhe_lut_read_dyn::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, &[a, b], &[false, false, false, true], k_max, bk)
}

pub fn binfhe_gate_or(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut k_max: usize, mut a: BinfheLweCiphertextDyn, mut b: BinfheLweCiphertextDyn, mut bk: &BinfheBootstrappingKeyDyn) -> BinfheLweCiphertextDyn
{
    binfhe_lut_read_dyn::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, &[a, b], &[false, true, true, true], k_max, bk)
}

pub fn binfhe_gate_xor(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut k_max: usize, mut a: BinfheLweCiphertextDyn, mut b: BinfheLweCiphertextDyn, mut bk: &BinfheBootstrappingKeyDyn) -> BinfheLweCiphertextDyn
{
    binfhe_lut_read_dyn::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, &[a, b], &[false, true, true, false], k_max, bk)
}

pub fn binfhe_rgsw_cmux(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut k_max: usize, mut sel: BinfheLweCiphertextDyn, mut a: BinfheLweCiphertextDyn, mut b: BinfheLweCiphertextDyn, mut bk: &BinfheBootstrappingKeyDyn) -> BinfheLweCiphertextDyn
{
    let TABLE: [bool; 8] = [false, false, false, true, true, false, true, true];
    binfhe_lut_read_dyn::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, &[sel, a, b], &TABLE, k_max, bk)
}

pub fn execute_plan(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut priv_ell: usize, mut priv_base_log: usize, mut plan: &BootstrapPlan, mut inputs: &[BinfheLweCiphertextDyn], mut cells: &[BinfheRlweCiphertextDyn], mut bk: &BinfheBootstrappingKeyDyn, mut cbk: &CircuitBootstrappingKeyDyn) -> (Vec<BinfheLweCiphertextDyn>, Vec<BinfheRlweCiphertextDyn>)
{
    let mut workspace = PlanWorkspaceDyn::<N_LWE, BIG_N, BS_ELL>::new(n_lwe, big_n, bs_ell, plan).expect("execute_plan: invalid or overflowing workspace requirements");
    execute_plan_in_workspace::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG, PRIV_ELL, PRIV_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, priv_ell, priv_base_log, plan, inputs, cells, bk, cbk, &mut workspace).into_iter().map(|(wires, cells)| (wires.to_vec(), cells.to_vec())).collect::<Vec<_>>().expect("execute_plan: invalid input/cell shape")
}

pub fn execute_plan_in_workspace(mut n_lwe: usize, mut big_n: usize, mut log_q: usize, mut log_q_lwe: usize, mut log_mod_ks: usize, mut bs_ell: usize, mut bs_base_log: usize, mut ks_ell: usize, mut ks_base_log: usize, mut priv_ell: usize, mut priv_base_log: usize, mut plan: &BootstrapPlan, mut inputs: &[BinfheLweCiphertextDyn], mut cells: &[BinfheRlweCiphertextDyn], mut bk: &BinfheBootstrappingKeyDyn, mut cbk: &CircuitBootstrappingKeyDyn, mut workspace: &mut PlanWorkspaceDyn) -> Result<(&[BinfheLweCiphertextDyn], &[BinfheRlweCiphertextDyn]), PlanWorkspaceError>
{
    plan.validate().map_err(PlanWorkspaceError::InvalidPlan)?;
    if inputs.len() != (plan.num_inputs as usize){
    return Err(PlanWorkspaceError::InputCount { expected: (plan.num_inputs as usize), actual: inputs.len() });
};
    if cells.len() != (plan.num_cells as usize){
    return Err(PlanWorkspaceError::CellCount { expected: (plan.num_cells as usize), actual: cells.len() });
};
    let required = plan.workspace_requirements()?;
    if (workspace.wires.capacity() < required.wires) || (workspace.rgsws.capacity() < required.rgsws) || (workspace.cells.capacity() < required.cells) || (workspace.lut_inputs.capacity() < required.lut_inputs){
    return Err(PlanWorkspaceError::CapacityOverflow);
};
    workspace.reset();
    workspace.wires.extend_from_slice(inputs);
    workspace.cells.extend_from_slice(cells);
    let delta = wire_delta::<LOG_Q_LWE>(log_q_lwe, (plan.k_max as usize));
    for layer in &plan.layers{
    for op in layer{
    match op {
    PlanOp::Const { out: out, value: value } => {
    workspace.wires.push(binfhe_trivial::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, *value, delta));
},
    PlanOp::Not { input: input, out: out } => {
    let input = workspace.wires[*input as usize];
    workspace.wires.push(binfhe_not::<N_LWE, LOG_Q_LWE>(n_lwe, log_q_lwe, &input, delta));
},
    PlanOp::Lut { inputs: inputs, table: table, out: out } => {
    let spec = &plan.luts[*table as usize];
    let arity = spec.entries.len().trailing_zeros() as usize;
    workspace.lut_inputs.clear();
    workspace.lut_inputs.extend(inputs.iter().map(|w| workspace.wires[*w as usize]));
    workspace.wires.push(binfhe_lut_read_dyn::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS, BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, log_mod_ks, bs_ell, bs_base_log, ks_ell, ks_base_log, &workspace.lut_inputs, &spec.entries, (plan.k_max as usize), bk));
},
    PlanOp::CircuitBootstrap { input: input, out: out } => {
    let input = workspace.wires[*input as usize];
    workspace.rgsws.push(circuit_bootstrap::<N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, BS_ELL, BS_BASE_LOG, KS_ELL, PRIV_ELL, PRIV_BASE_LOG>(n_lwe, big_n, log_q, log_q_lwe, bs_ell, bs_base_log, ks_ell, priv_ell, priv_base_log, &input, cbk, (plan.k_max as usize)));
},
    PlanOp::RgswMux { sel: sel, then_cell: then_cell, else_cell: else_cell, out: out } => {
    let out_cell = binfhe_rgsw_cmux::<BIG_N, LOG_Q, BS_ELL, BS_BASE_LOG>(&workspace.rgsws[*sel as usize], &workspace.cells[*then_cell as usize], &workspace.cells[*else_cell as usize]);
    workspace.cells.push(out_cell);
},
};
};
};
    Ok((&workspace.wires, &workspace.cells))
}

pub fn encode_plan(mut plan: &BootstrapPlan) -> Result<Vec<u8>, EncodeError>
{
    plan.validate().map_err(EncodeError::InvalidPlan)?;
    if (plan.luts.len() > MAX_ITEMS) || (plan.layers.len() > MAX_ITEMS) || (plan.outputs.len() > MAX_ITEMS) || (plan.cell_outputs.len() > MAX_ITEMS) || plan.layers.iter().any(|layer| (layer.len() > MAX_ITEMS)) || plan.luts.iter().any(|lut| (lut.entries.len() > MAX_ITEMS)){
    return Err(EncodeError::TooLarge);
};
    let mut bytes = Vec::new();
    bytes.extend_from_slice(MAGIC);
    bytes.push(VERSION);
    bytes.push(profile_tag(plan.profile));
    put_u32(&mut bytes, plan.k_max);
    put_u32(&mut bytes, plan.num_inputs);
    put_u32(&mut bytes, plan.num_cells);
    put_u32(&mut bytes, plan.budget.per_bootstrap_log2);
    put_u32(&mut bytes, plan.budget.total_log2);
    put_u32(&mut bytes, (plan.luts.len() as u32));
    for lut in &plan.luts{
    put_u32(&mut bytes, (lut.entries.len() as u32));
    for chunk in lut.entries.chunks(8){
    let mut packed = 0 as u8;
    for (bit, entry) in chunk.iter().enumerate(){
    packed |= ((*entry as u8) << bit);
};
    bytes.push(packed);
};
};
    put_u32(&mut bytes, (plan.layers.len() as u32));
    for layer in &plan.layers{
    put_u32(&mut bytes, (layer.len() as u32));
    for op in layer{
    match op {
    PlanOp::Const { out: out, value: value } => {
    bytes.push(0);
    put_u32(&mut bytes, *out);
    bytes.push((*value as u8));
},
    PlanOp::Not { input: input, out: out } => {
    bytes.push(1);
    put_u32(&mut bytes, *input);
    put_u32(&mut bytes, *out);
},
    PlanOp::Lut { inputs: inputs, table: table, out: out } => {
    bytes.push(2);
    put_u32(&mut bytes, (inputs.len() as u32));
    for input in inputs.as_slice(){
    put_u32(&mut bytes, *input);
};
    put_u32(&mut bytes, *table);
    put_u32(&mut bytes, *out);
},
    PlanOp::CircuitBootstrap { input: input, out: out } => {
    bytes.push(3);
    put_u32(&mut bytes, *input);
    put_u32(&mut bytes, *out);
},
    PlanOp::RgswMux { sel: sel, then_cell: then_cell, else_cell: else_cell, out: out } => {
    bytes.push(4);
    put_u32(&mut bytes, *sel);
    put_u32(&mut bytes, *then_cell);
    put_u32(&mut bytes, *else_cell);
    put_u32(&mut bytes, *out);
},
};
};
};
    put_ids(&mut bytes, &plan.outputs);
    put_ids(&mut bytes, &plan.cell_outputs);
    Ok(bytes)
}

pub fn decode_plan(mut bytes: &[u8]) -> Result<BootstrapPlan, DecodeError>
{
    let mut reader = Reader { bytes: bytes, offset: 0 };
    if reader.take(4)? != MAGIC{
    return Err(DecodeError::BadMagic);
};
    if reader.byte()? != VERSION{
    return Err(DecodeError::UnsupportedVersion);
};
    let profile = parse_profile(reader.byte()?)?;
    let k_max = reader.u32()?;
    let num_inputs = reader.u32()?;
    let num_cells = reader.u32()?;
    let budget = FailureBudget { per_bootstrap_log2: reader.u32()?, total_log2: reader.u32()? };
    let lut_count = reader.count()?;
    let mut luts = Vec::with_capacity(lut_count);
    for _ in 0.. lut_count{
    let bit_count = reader.count()?;
    let packed_len = bit_count.div_ceil(8);
    let packed = reader.take(packed_len)?;
    if ((bit_count % 8) != 0) && packed.last().is_some_and(|byte| ((*byte >> (bit_count % 8)) != 0)){
    return Err(DecodeError::UnknownTag);
};
    let mut entries = Vec::with_capacity(bit_count);
    for bit in 0.. bit_count{
    entries.push((((packed[bit / 8] >> (bit % 8)) & 1) != 0));
};
    luts.push(LutSpec { entries: entries });
};
    let layer_count = reader.count()?;
    let mut layers = Vec::with_capacity(layer_count);
    for _ in 0.. layer_count{
    let op_count = reader.count()?;
    let mut layer = Vec::with_capacity(op_count);
    for _ in 0.. op_count{
    layer.push(read_op(&mut reader)?);
};
    layers.push(layer);
};
    let outputs = reader.ids()?;
    let cell_outputs = reader.ids()?;
    if reader.offset != bytes.len(){
    return Err(DecodeError::TrailingBytes);
};
    let plan = BootstrapPlan { profile: profile, k_max: k_max, luts: luts, layers: layers, num_inputs: num_inputs, num_cells: num_cells, outputs: outputs, cell_outputs: cell_outputs, budget: budget };
    plan.validate().map_err(DecodeError::InvalidPlan)?;
    Ok(plan)
}

pub fn profile_tag(mut profile: ProfileId) -> u8
{
    match profile {
    _ => 0,
    _ => 1,
    _ => 2,
    _ => 3,
}
}

pub fn parse_profile(mut tag: u8) -> Result<ProfileId, DecodeError>
{
    match tag {
    .. => Ok(ProfileId::Toy),
    .. => Ok(ProfileId::ToyNoisy),
    .. => Ok(ProfileId::Std128),
    .. => Ok(ProfileId::Custom),
    _ => Err(DecodeError::UnknownTag),
}
}

pub fn put_u32(mut bytes: &mut Vec<u8>, mut value: u32)
{
    bytes.extend_from_slice(&value.to_le_bytes());
}

pub fn put_ids(mut bytes: &mut Vec<u8>, mut ids: &[u32])
{
    put_u32(bytes, (ids.len() as u32));
    for id in ids{
    put_u32(bytes, *id);
}
}

pub fn read_op(mut reader: &mut Reader) -> Result<PlanOp, DecodeError>
{
    match reader.byte()? {
    .. => {
    let out = reader.u32()?;
    let value = match reader.byte()? {
    .. => false,
    .. => true,
    _ => return Err(DecodeError::UnknownTag),
};
    Ok(PlanOp::Const { out: out, value: value })
},
    .. => Ok(PlanOp::Not { input: reader.u32()?, out: reader.u32()? }),
    .. => {
    let count = reader.count()?;
    let mut ids = alloc::vec::Vec::with_capacity(count);
    for _ in 0.. count{
    ids.push(reader.u32()?);
};
    let mut inputs = vec![(0 as u32); ids.len()];
    for (i, id) in ids.iter().enumerate(){
    inputs[i] = *id;
};
    let table: LutId = reader.u32()?;
    let out: WireId = reader.u32()?;
    Ok(PlanOp::Lut { inputs: inputs, table: table, out: out })
},
    .. => Ok(PlanOp::CircuitBootstrap { input: reader.u32()?, out: reader.u32()? }),
    .. => {
    let sel: RgswId = reader.u32()?;
    let then_cell = reader.u32()?;
    let else_cell = reader.u32()?;
    let out = reader.u32()?;
    Ok(PlanOp::RgswMux { sel: sel, then_cell: then_cell, else_cell: else_cell, out: out })
},
    _ => Err(DecodeError::UnknownTag),
}
}

pub fn binfhe_rgsw_encrypt<R: SpecRng>(mut n: usize, mut log: usize, mut ell: usize, mut base_log: usize, mut eta: usize, mut m: bool, mut sk: &BinfheRlweSecretKeyDyn, mut rng: &mut R) -> BinfheRgswCiphertextDyn
{
    let rows = (0..n).map(|j| {
    let g = gadget::<LOG>::level_factor(log, base_log, j);
    let contrib = if m{
    g
} else {
    0
};
    let mut rlwe0 = binfhe_rlwe_encrypt_scalar::<N, LOG, ETA, R>(n, log, eta, 0, sk, rng);
    rlwe0.a[0] = torus::<LOG>::torus_add(log, rlwe0.a[0], contrib);
    let rlwe1 = binfhe_rlwe_encrypt_scalar::<N, LOG, ETA, R>(n, log, eta, contrib, sk, rng);
    BinfheRgswRowDyn { rlwe0: rlwe0, rlwe1: rlwe1, n: 0 }
}).collect::<Vec<_>>();
    BinfheRgswCiphertextDyn { rows: rows, n: 0, ell: 0 }
}

pub fn binfhe_external_product(mut n: usize, mut log: usize, mut ell: usize, mut base_log: usize, mut c: &BinfheRgswCiphertextDyn, mut ct: &BinfheRlweCiphertextDyn) -> BinfheRlweCiphertextDyn
{
    let a_dec = gadget::<N, LOG, ELL, BASE_LOG>::gadget_poly_decompose(n, log, ell, base_log, &ct.a);
    let b_dec = gadget::<N, LOG, ELL, BASE_LOG>::gadget_poly_decompose(n, log, ell, base_log, &ct.b);
    let mut out_a = [(0 as u32); n];
    let mut out_b = [(0 as u32); n];
    for j in 0.. ell{
    let row = &c.rows[j];
    let a0 = binfhe_poly_mul_neg::<N, LOG>(n, log, &a_dec[j], &row.rlwe0.a);
    let a1 = binfhe_poly_mul_neg::<N, LOG>(n, log, &a_dec[j], &row.rlwe0.b);
    let b0 = binfhe_poly_mul_neg::<N, LOG>(n, log, &b_dec[j], &row.rlwe1.a);
    let b1 = binfhe_poly_mul_neg::<N, LOG>(n, log, &b_dec[j], &row.rlwe1.b);
    for k in 0.. n{
    out_a[k] = out_a[k].wrapping_add(a0[k]).wrapping_add(b0[k]);
    out_b[k] = out_b[k].wrapping_add(a1[k]).wrapping_add(b1[k]);
};
};
    for k in 0.. n{
    out_a[k] = torus::<LOG>::reduce(log, out_a[k]);
    out_b[k] = torus::<LOG>::reduce(log, out_b[k]);
};
    BinfheRlweCiphertextDyn { a: out_a, b: out_b, n: 0 }
}

pub fn binfhe_rgsw_cmux(mut n: usize, mut log: usize, mut ell: usize, mut base_log: usize, mut c: &BinfheRgswCiphertextDyn, mut d1: &BinfheRlweCiphertextDyn, mut d0: &BinfheRlweCiphertextDyn) -> BinfheRlweCiphertextDyn
{
    let diff = binfhe_rlwe_sub::<N, LOG>(n, log, d1, d0);
    let prod = binfhe_external_product::<N, LOG, ELL, BASE_LOG>(n, log, ell, base_log, c, &diff);
    binfhe_rlwe_add::<N, LOG>(n, log, d0, &prod)
}

pub fn binfhe_gen_rlwe_secret_key<R: SpecRng>(mut n: usize, mut rng: &mut R) -> BinfheRlweSecretKeyDyn
{
    let mut key = [(0 as u32); n];
    for k in key.iter_mut(){
    *k = ((rng.next_u32() & 1) as u32);
};
    BinfheRlweSecretKeyDyn { key: key, n: 0 }
}

pub fn binfhe_poly_mul_neg(mut n: usize, mut log: usize, mut a: &[u32; N], mut b: &[u32; N]) -> [u32; N]
{
    let mut result = [(0 as u32); n];
    for i in 0.. n{
    for j in 0.. n{
    let deg = i + j;
    let term = a[i].wrapping_mul(b[j]);
    if deg < n{
    result[deg] = result[deg].wrapping_add(term);
} else {
    result[deg - n] = result[deg - n].wrapping_sub(term);
};
};
};
    for r in result.iter_mut(){
    *r = torus::<LOG>::reduce(log, *r);
};
    result
}

pub fn binfhe_poly_rotate(mut n: usize, mut log: usize, mut p: &[u32; N], mut exp: usize) -> [u32; N]
{
    let exp = exp % (2 * n);
    let mut result = [(0 as u32); n];
    for (i, ..) in p.iter().enumerate(){
    let dest = i + exp;
    if dest < n{
    result[dest] = result[dest].wrapping_add(coeff);
} else if dest < (2 * n){
    result[dest - n] = result[dest - n].wrapping_sub(coeff);
} else {
    result[dest - (2 * n)] = result[dest - (2 * n)].wrapping_add(coeff);
};
};
    for r in result.iter_mut(){
    *r = torus::<LOG>::reduce(log, *r);
};
    result
}

pub fn binfhe_rlwe_encrypt_poly<R: SpecRng>(mut n: usize, mut log: usize, mut eta: usize, mut msg: &[u32; N], mut sk: &BinfheRlweSecretKeyDyn, mut rng: &mut R) -> BinfheRlweCiphertextDyn
{
    let a: [u32; N] = (0..n).map(|_| torus::<LOG>::reduce(log, rng.next_u32())).collect::<Vec<_>>();
    let mut b = binfhe_poly_mul_neg::<N, LOG>(n, log, &a, &sk.key);
    for i in 0.. n{
    b[i] = torus::<LOG>::reduce(log, b[i].wrapping_add(sampler::<LOG, ETA, R>::sample_error(log, eta, rng)).wrapping_add(msg[i]));
};
    BinfheRlweCiphertextDyn { a: a, b: b, n: 0 }
}

pub fn binfhe_rlwe_encrypt_scalar<R: SpecRng>(mut n: usize, mut log: usize, mut eta: usize, mut m: u32, mut sk: &BinfheRlweSecretKeyDyn, mut rng: &mut R) -> BinfheRlweCiphertextDyn
{
    let mut msg = [(0 as u32); n];
    msg[0] = m;
    binfhe_rlwe_encrypt_poly::<N, LOG, ETA, R>(n, log, eta, &msg, sk, rng)
}

pub fn binfhe_rlwe_phase(mut n: usize, mut log: usize, mut ct: &BinfheRlweCiphertextDyn, mut sk: &BinfheRlweSecretKeyDyn) -> [u32; N]
{
    let product = binfhe_poly_mul_neg::<N, LOG>(n, log, &ct.a, &sk.key);
    let mut phase = [(0 as u32); n];
    for i in 0.. n{
    phase[i] = torus::<LOG>::torus_sub(log, ct.b[i], product[i]);
};
    phase
}

pub fn binfhe_sample_extract(mut n: usize, mut log: usize, mut ct: &BinfheRlweCiphertextDyn) -> BinfheLweCiphertextDyn
{
    let mut a_lwe = [(0 as u32); n];
    a_lwe[0] = ct.a[0];
    for i in 1.. n{
    a_lwe[i] = torus::<LOG>::torus_neg(log, ct.a[n - i]);
};
    BinfheLweCiphertextDyn { a: a_lwe, b: ct.b[0], n: 0 }
}

pub fn binfhe_rlwe_add(mut n: usize, mut log: usize, mut x: &BinfheRlweCiphertextDyn, mut y: &BinfheRlweCiphertextDyn) -> BinfheRlweCiphertextDyn
{
    let mut out = *x;
    for i in 0.. n{
    out.a[i] = torus::<LOG>::torus_add(log, out.a[i], y.a[i]);
    out.b[i] = torus::<LOG>::torus_add(log, out.b[i], y.b[i]);
};
    out
}

pub fn binfhe_rlwe_sub(mut n: usize, mut log: usize, mut x: &BinfheRlweCiphertextDyn, mut y: &BinfheRlweCiphertextDyn) -> BinfheRlweCiphertextDyn
{
    let mut out = *x;
    for i in 0.. n{
    out.a[i] = torus::<LOG>::torus_sub(log, out.a[i], y.a[i]);
    out.b[i] = torus::<LOG>::torus_sub(log, out.b[i], y.b[i]);
};
    out
}

pub fn binfhe_rlwe_rotate(mut n: usize, mut log: usize, mut ct: &BinfheRlweCiphertextDyn, mut exp: usize) -> BinfheRlweCiphertextDyn
{
    BinfheRlweCiphertextDyn { a: binfhe_poly_rotate::<N, LOG>(n, log, &ct.a, exp), b: binfhe_poly_rotate::<N, LOG>(n, log, &ct.b, exp), n: 0 }
}

pub fn binfhe_rlwe_trivial(mut n: usize, mut log: usize, mut msg: &[u32; N]) -> BinfheRlweCiphertextDyn
{
    BinfheRlweCiphertextDyn { a: [(0 as u32); n], b: *msg, n: 0 }
}

pub fn cbd<R: SpecRng>(mut eta: usize, mut rng: &mut R) -> i32
{
    if eta == 0{
    return 0;
};
    let mask = if eta >= 32{
    u32::MAX
} else {
    ((1 as u32) << eta) - 1
};
    let a = (rng.next_u32() & mask).count_ones() as i32;
    let b = (rng.next_u32() & mask).count_ones() as i32;
    a - b
}

pub fn sample_error<R: SpecRng>(mut log: usize, mut eta: usize, mut rng: &mut R) -> u32
{
    super::torus::<LOG>::reduce(log, (cbd::<ETA, R>(eta, rng) as u32))
}

pub fn mask(mut log: usize) -> u32
{
    if log >= 32{
    u32::MAX
} else {
    ((1 as u32) << log) - 1
}
}

pub fn reduce(mut log: usize, mut x: u32) -> u32
{
    x & mask::<LOG>(log)
}

pub fn torus_add(mut log: usize, mut a: u32, mut b: u32) -> u32
{
    reduce::<LOG>(log, a.wrapping_add(b))
}

pub fn torus_sub(mut log: usize, mut a: u32, mut b: u32) -> u32
{
    reduce::<LOG>(log, a.wrapping_sub(b))
}

pub fn torus_neg(mut log: usize, mut a: u32) -> u32
{
    reduce::<LOG>(log, a.wrapping_neg())
}

pub fn mul_exact(mut log: usize, mut a: u32, mut c: u32) -> u32
{
    reduce::<LOG>(log, a.wrapping_mul(c))
}

pub fn embed_up(mut from: usize, mut to: usize, mut x: u32) -> u32
{
    reduce::<TO>(to, (x << (to - from)))
}

pub fn parity_coeffs(mut p: u64, mut k_max: usize) -> Result<Vec<u64>, StitchError>
{
    if (k_max == 0) || (k_max > ((p - 1) as usize)){
    return Err(StitchError::PayloadOverflow);
};
    let mut coeffs = vec![(0 as u64); (k_max + 1)];
    let points: Vec<u64> = (0..=(k_max as u64)).collect::<Vec<_>>();
    for (j, ..) in points.iter().enumerate(){
    if (x_j % 2) == 0{
    continue;
};
    let mut numerator = vec![(1 as u64)];
    let mut denominator = 1 as u64;
    for (k, ..) in points.iter().enumerate(){
    if k == j{
    continue;
};
    let mut next = vec![(0 as u64); (numerator.len() + 1)];
    for (i, ..) in numerator.iter().enumerate(){
    next[i + 1] = ring::add_mod(next[i + 1], c, p);
    next[i] = ring::sub_mod(next[i], ring::mul_mod(c, x_k, p), p);
};
    numerator = next;
    let diff = if x_j > x_k{
    x_j - x_k
} else {
    p - (x_k - x_j)
};
    denominator = ring::mul_mod(denominator, (diff % p), p);
};
    let inv = ring::inverse_mod(denominator, p).ok_or(StitchError::PayloadOverflow)?;
    for (i, ..) in numerator.iter().enumerate(){
    coeffs[i] = ring::add_mod(coeffs[i], ring::mul_mod(c, inv, p), p);
};
};
    Ok(coeffs)
}

pub fn rowct_from_lattice(mut tau: &LatticeRowCt, mut p: u64) -> RowCt
{
    let red = |poly| {
    poly.iter().map(|c| (c % p)).collect::<Vec<_>>()
};
    RowCt { c0: red(&tau.u1), c1: red(&tau.u2) }
}

pub fn negacyclic_mul_rq(mut a: &[u64], mut b: &[u64], mut q: u64) -> RqPoly
{
    let n = a.len();
    let mut out = vec![(0 as i128); n];
    for (i, ..) in a.iter().enumerate(){
    if ai == 0{
    continue;
};
    let ai = ring::center(ai, q);
    for (j, ..) in b.iter().enumerate(){
    if bj == 0{
    continue;
};
    let bj = ring::center(bj, q);
    let prod = ai * bj;
    if (i + j) < n{
    out[i + j] += prod;
} else {
    out[(i + j) - n] -= prod;
};
};
};
    out.iter().map(|c| (c.rem_euclid((q as i128)) as u64)).collect::<Vec<_>>()
}

pub fn add_rq(mut a: &[u64], mut b: &[u64], mut q: u64) -> RqPoly
{
    a.iter().zip(b.iter()).map(|(x, y)| ring::add_mod(x, y, q)).collect::<Vec<_>>()
}

pub fn sub_rq(mut a: &[u64], mut b: &[u64], mut q: u64) -> RqPoly
{
    a.iter().zip(b.iter()).map(|(x, y)| ring::sub_mod(x, y, q)).collect::<Vec<_>>()
}

pub fn negate_rq(mut a: &[u64], mut q: u64) -> RqPoly
{
    a.iter().map(|x| if x == 0{
    0
} else {
    q - x
}).collect::<Vec<_>>()
}

pub fn add_rp(mut a: &[u64], mut b: &[u64], mut p: u64) -> RpPoly
{
    a.iter().zip(b.iter()).map(|(x, y)| ring::add_mod(x, y, p)).collect::<Vec<_>>()
}

pub fn round_to_rp(mut x: &RqPoly, mut q: u64, mut p: u64) -> RpPoly
{
    x.iter().map(|c| {
    let centered = ring::center(c, q);
    centered.rem_euclid((p as i128)) as u64
}).collect::<Vec<_>>()
}

pub fn sample_uniform_rq(mut n: usize, mut q: u64, mut random: &mut impl SpecRng) -> RqPoly
{
    let mut out = vec![(0 as u64); n];
    for c in out.iter_mut(){
    *c = ((random.next_u32() as u64) % q);
};
    out
}

pub fn sample_noise_rq(mut n: usize, mut p: u64, mut q: u64, mut random: &mut impl SpecRng) -> RqPoly
{
    let mut out = vec![(0 as u64); n];
    for c in out.iter_mut(){
    let r = ((random.next_u32() % 3) as i64) - 1;
    let pe = if r < 0{
    q - (p % q)
} else {
    ((r as u64) * p) % q
};
    *c = pe;
};
    out
}

pub fn ntt_ctx(mut n: usize, mut q: u64) -> Option<Ntt>
{
    ring::Ntt::new(n, q).ok()
}

pub fn mul_limb(mut a: &Limb, mut b: &Limb, mut q: u64, mut ntt: &Option<Ntt>) -> Limb
{
    match ntt {
    Some(nt) => ring::negacyclic_mul_ntt(a, b, nt),
    None => ring::negacyclic_mul_coeffs(a, b, q),
}
}

pub fn add_limb(mut a: &Limb, mut b: &Limb, mut q: u64) -> Limb
{
    a.iter().zip(b.iter()).map(|(x, y)| ring::add_mod(x, y, q)).collect::<Vec<_>>()
}

pub fn sub_limb(mut a: &Limb, mut b: &Limb, mut q: u64) -> Limb
{
    a.iter().zip(b.iter()).map(|(x, y)| ring::sub_mod(x, y, q)).collect::<Vec<_>>()
}

pub fn negate_limb(mut a: &Limb, mut q: u64) -> Limb
{
    a.iter().map(|x| if x == 0{
    0
} else {
    q - x
}).collect::<Vec<_>>()
}

pub fn scale_limb(mut delta_limb: &Limb, mut t: i64, mut q: u64) -> Limb
{
    delta_limb.iter().map(|c| {
    let c = c as i128;
    (c * (t as i128)).rem_euclid((q as i128)) as u64
}).collect::<Vec<_>>()
}

pub fn to_limb(mut delta: &LabelPoly, mut p: u64, mut q: u64) -> Limb
{
    delta.iter().map(|c| {
    if c == 0{
    0
} else if c == 1{
    1
} else {
    q - 1
}
}).collect::<Vec<_>>()
}

pub fn uniform_limb(mut n: usize, mut q: u64, mut random: &mut impl SpecRng) -> Limb
{
    let mut out = vec![(0 as u64); n];
    for c in out.iter_mut(){
    *c = ((random.next_u32() as u64) % q);
};
    out
}

pub fn noise_limb(mut n: usize, mut p: u64, mut q: u64, mut random: &mut impl SpecRng) -> Limb
{
    let mut out = vec![(0 as u64); n];
    for c in out.iter_mut(){
    let r = ((random.next_u32() % 3) as i64) - 1;
    *c = if r < 0{
    q - (p % q)
} else {
    ((r as u64) * p) % q
};
};
    out
}

pub fn round_limb0_to_rp(mut x: &Limb, mut q0: u64, mut p: u64) -> LabelPoly
{
    x.iter().map(|c| {
    let centered = ring::center(c, q0);
    centered.rem_euclid((p as i128)) as u64
}).collect::<Vec<_>>()
}

pub fn masked_table(mut and: bool, mut pi_a: bool, mut pi_b: bool, mut pi_c: bool) -> [bool; 4]
{
    let mut out = [false; 4];
    for j in 0.. 2{
    for i in 0.. 2{
    let (ii, jj) = ((i == 1), (j == 1));
    out[i + (2 * j)] = if and{
    ((pi_a ^ ii) & (pi_b ^ jj)) ^ pi_c
} else {
    pi_a ^ ii ^ pi_b ^ jj ^ pi_c
};
};
};
    out
}

pub fn row_multipliers(mut z: &[bool; 4]) -> [i64; 3]
{
    let b = |x| i64::from(x);
    let t1 = b(z[1]) - b(z[0]);
    let t2 = b(z[2]) - b(z[0]);
    let t3 = ((b(z[3]) - b(z[0])) - t1) - t2;
    [t1, t2, t3]
}

pub fn fast_mask_bits(mut z: &[bool; 4], mut r: &[bool; 4]) -> [bool; 4]
{
    let mut out = [false; 4];
    for k in 0.. 4{
    out[k] = (z[k] ^ r[k]);
};
    out
}

pub fn fast_unmask_coeffs(mut d: bool) -> (i64, i64)
{
    if d{
    (1, -1)
} else {
    (0, 1)
}
}

pub fn determination_bit(mut w0_lsb: bool, mut pi: bool) -> bool
{
    w0_lsb ^ pi
}

pub fn label_lsb(mut w: &LabelPoly) -> bool
{
    !w.is_empty() && ((w[0] % 2) == 1)
}

pub fn delta_scaled(mut delta: &Delta, mut k: u64, mut p: u64) -> LabelPoly
{
    delta.coeffs.iter().map(|c| ring::mul_mod(c, k, p)).collect::<Vec<_>>()
}

pub fn scale_row(mut row: &RowCt, mut k: i64, mut p: u64) -> RowCt
{
    let scale = |poly, k| {
    poly.iter().map(|c| ring::mul_mod(c, k, p)).collect::<Vec<_>>()
};
    let neg = |poly| {
    poly.iter().map(|c| if c == 0{
    0
} else {
    p - c
}).collect::<Vec<_>>()
};
    if k >= 0{
    RowCt { c0: scale(&row.c0, (k as u64)), c1: scale(&row.c1, (k as u64)) }
} else {
    RowCt { c0: neg(scale(&row.c0, (-k as u64))), c1: neg(scale(&row.c1, (-k as u64))) }
}
}

pub fn sub_rows(mut a: &RowCt, mut b: &RowCt, mut p: u64) -> RowCt
{
    RowCt { c0: ring::sub_coeffs(&a.c0, &b.c0, p), c1: ring::sub_coeffs(&a.c1, &b.c1, p) }
}

pub fn xor_rows(mut delta_ct: &RowCt, mut d: bool, mut p: u64) -> [RowCt; 3]
{
    let m: i64 = if d{
    -1
} else {
    1
};
    [scale_row(delta_ct, m, p), scale_row(delta_ct, m, p), scale_row(delta_ct, (-2 * m), p)]
}

pub fn fast_and_rows(mut delta_ct: &RowCt, mut r: &[RowCt; 4], mut d: &[bool; 4], mut p: u64) -> [RowCt; 3]
{
    let mut z: Vec<RowCt> = Vec::with_capacity(4);
    for k in 0.. 4{
    z.push(if d[k]{
    sub_rows(delta_ct, &r[k], p)
} else {
    r[k].clone()
});
};
    let t1 = sub_rows(&z[1], &z[0], p);
    let t2 = sub_rows(&z[2], &z[0], p);
    let t3_pre = sub_rows(&z[3], &z[0], p);
    let t3 = sub_rows(&sub_rows(&t3_pre, &t1, p), &t2, p);
    [t1, t2, t3]
}

pub fn sample_poly(mut degree: usize, mut p: u64, mut random: &mut impl SpecRng) -> LabelPoly
{
    let mut out = vec![(0 as u64); degree];
    for c in out.iter_mut(){
    *c = ((random.next_u32() as u64) % p);
};
    out
}

pub fn gate_output_label<D: DdGadget>(mut dd: &D, mut wa_base: &LabelPoly, mut wb_base: &LabelPoly, mut taus: &[RowCt; 3], mut z00: bool, mut prf: &PrfKey, mut wire: usize, mut out_pi: bool) -> WireLabel
{
    let delta = dd.delta();
    let p = dd.p();
    let degree = delta.coeffs.len();
    let mut out = dd.eval(wa_base, false, wb_base, false, taus);
    if z00{
    out = ring::sub_coeffs(&out, &delta.coeffs, p);
};
    out = ring::add_coeffs(&out, &prf.offset(wire, degree, p), p);
    let w0 = if out_pi{
    ring::sub_coeffs(&out, &delta.coeffs, p)
} else {
    out
};
    WireLabel { w0: w0, pi: out_pi }
}

pub fn garble_circuit<D: DdGadget>(mut variant: Variant, mut circuit: &Circuit, mut dd: &D, mut pis: &[bool], mut prf: &PrfKey, mut prg_bits: &[bool], mut random: &mut impl SpecRng) -> Result<PinnedGarbling, DriverError>
{
    circuit.validate().map_err(|_| DriverError::BadCircuit)?;
    let delta = dd.delta();
    let p = dd.p();
    let degree = delta.coeffs.len();
    if pis.len() != circuit.wires(){
    return Err(DriverError::BadInputCount);
};
    let delta_ct = dd.row(1, random);
    let mut pi: Vec<bool> = pis.to_vec();
    let mut labels: Vec<WireLabel> = Vec::with_capacity(circuit.wires());
    let mut and_rows: Vec<AndRows> = Vec::new();
    let mut records = Vec::new();
    let mut fast_and = 0 as usize;
    for w in 0.. circuit.wires(){
    if labels.len() <= w{
    labels.push(WireLabel { w0: sample_poly(degree, p, random), pi: pi[w] });
};
    if w < circuit.inputs{
    continue;
};
    let gate = circuit.gates[w - circuit.inputs];
    let (a, b) = gate.operands();
    let wa = labels[a].base(delta);
    let wb = labels[b].base(delta);
    match (variant, gate) {
    .. => {
    let z = masked_table(true, pi[a], pi[b], pi[w]);
    let t = row_multipliers(&z);
    let taus = [dd.row(t[0], random), dd.row(t[1], random), dd.row(t[2], random)];
    let out = gate_output_label(dd, &wa, &wb, &taus, z[0], prf, w, pi[w]);
    let det = determination_bit(out.w0_lsb(), pi[w]);
    labels[w] = out;
    and_rows.push(AndRows::Small(taus));
    records.push(GateRecord::SmallAnd(det));
},
    .. => {
    let d = pi[a] ^ pi[b] ^ pi[w];
    let taus = xor_rows(&delta_ct, d, p);
    let out = gate_output_label(dd, &wa, &wb, &taus, d, prf, w, pi[w]);
    labels[w] = out;
    records.push(GateRecord::SmallXor(d));
},
    (_, Gate::And { .. }) => {
    let out_pi = (random.next_u32() & 1) == 1;
    pi[w] = out_pi;
    let z = masked_table(true, pi[a], pi[b], out_pi);
    let r: [bool; 4] = [prg_bits.get((4 * fast_and)).copied().unwrap_or(false), prg_bits.get(((4 * fast_and) + 1)).copied().unwrap_or(false), prg_bits.get(((4 * fast_and) + 2)).copied().unwrap_or(false), prg_bits.get(((4 * fast_and) + 3)).copied().unwrap_or(false)];
    fast_and += 1;
    let masks = fast_mask_bits(&z, &r);
    let r_rows = [dd.row(i64::from(r[0]), random), dd.row(i64::from(r[1]), random), dd.row(i64::from(r[2]), random), dd.row(i64::from(r[3]), random)];
    let taus = fast_and_rows(&delta_ct, &r_rows, &masks, p);
    let out = gate_output_label(dd, &wa, &wb, &taus, z[0], prf, w, out_pi);
    let det = determination_bit(out.w0_lsb(), out_pi);
    labels[w] = out;
    and_rows.push(AndRows::Fast(r_rows));
    records.push(GateRecord::FastAnd { masks: masks, det: det });
},
    (_, Gate::Xor { .. }) => {
    let out_pi = pi[a] ^ pi[b];
    pi[w] = out_pi;
    let taus = xor_rows(&delta_ct, false, p);
    let out = gate_output_label(dd, &wa, &wb, &taus, false, prf, w, out_pi);
    labels[w] = out;
    records.push(GateRecord::FastXor);
},
    (_, Gate::Not { .. }) => {
    let out_pi = !pi[a];
    pi[w] = out_pi;
    let base = labels[a].base(delta);
    let w0 = if out_pi{
    ring::sub_coeffs(&base, &delta.coeffs, p)
} else {
    base
};
    labels[w] = WireLabel { w0: w0, pi: out_pi };
},
};
};
    Ok(PinnedGarbling { labels: labels, delta_ct: delta_ct, and_rows: and_rows, records: records })
}

pub fn encode_inputs(mut garbling: &PinnedGarbling, mut delta: &Delta, mut inputs: &[bool], mut input_count: usize) -> Result<Vec<(LabelPoly, bool)>, DriverError>
{
    if inputs.len() != input_count{
    return Err(DriverError::BadInputCount);
};
    let mut out = Vec::with_capacity(input_count);
    for (i, ..) in inputs.iter().enumerate(){
    let masked = x ^ garbling.labels[i].pi;
    out.push((garbling.labels[i].active(masked, delta), masked));
};
    Ok(out)
}

pub fn eval_gate<D: DdGadget>(mut dd: &D, mut a: &LabelPoly, mut i: bool, mut b: &LabelPoly, mut j: bool, mut taus: &[RowCt; 3], mut prf: &PrfKey, mut wire: usize, mut degree: usize, mut p: u64) -> LabelPoly
{
    let out = dd.eval(a, i, b, j, taus);
    ring::add_coeffs(&out, &prf.offset(wire, degree, p), p)
}

pub fn evaluate_circuit<D: DdGadget>(mut variant: Variant, mut circuit: &Circuit, mut dd: &D, mut delta_ct: &RowCt, mut and_rows: &[AndRows], mut records: &[GateRecord], mut prf: &PrfKey, mut inputs: &[(LabelPoly, bool)]) -> Result<Vec<bool>, DriverError>
{
    circuit.validate().map_err(|_| DriverError::BadCircuit)?;
    if inputs.len() != circuit.inputs{
    return Err(DriverError::BadInputCount);
};
    let delta = dd.delta();
    let p = dd.p();
    let degree = delta.coeffs.len();
    let mut labels: Vec<LabelPoly> = Vec::with_capacity(circuit.wires());
    let mut masked: Vec<bool> = Vec::with_capacity(circuit.wires());
    for (w, m) in inputs{
    labels.push(w.clone());
    masked.push(*m);
};
    let mut rec = records.iter();
    let mut and_iter = and_rows.iter();
    for (gi, gate) in circuit.gates.iter().enumerate(){
    let w = circuit.inputs + gi;
    let (a, b) = gate.operands();
    match (variant, gate) {
    .. => {
    let d = match rec.next() {
    Some(GateRecord::SmallAnd(d)) => *d,
    Some(_) => return Err(DriverError::WrongRecord),
    None => return Err(DriverError::MissingRecord),
};
    let taus = match and_iter.next() {
    Some(AndRows::Small(rows)) => rows,
    Some(_) => return Err(DriverError::WrongRecord),
    None => return Err(DriverError::MissingRecord),
};
    let out = eval_gate(dd, &labels[a], masked[a], &labels[b], masked[b], taus, prf, w, degree, p);
    let m = label_lsb(&out) ^ d;
    labels.push(out);
    masked.push(m);
},
    .. => {
    let d = match rec.next() {
    Some(GateRecord::SmallXor(d)) => *d,
    Some(_) => return Err(DriverError::WrongRecord),
    None => return Err(DriverError::MissingRecord),
};
    let taus = xor_rows(delta_ct, d, p);
    let out = eval_gate(dd, &labels[a], masked[a], &labels[b], masked[b], &taus, prf, w, degree, p);
    labels.push(out);
    masked.push(((masked[a] ^ masked[b]) ^ d));
},
    (_, Gate::And { .. }) => {
    let (masks, det) = match rec.next() {
    Some(GateRecord::FastAnd { masks: masks, det: det }) => (*masks, *det),
    Some(_) => return Err(DriverError::WrongRecord),
    None => return Err(DriverError::MissingRecord),
};
    let r_rows = match and_iter.next() {
    Some(AndRows::Fast(rows)) => rows,
    Some(_) => return Err(DriverError::WrongRecord),
    None => return Err(DriverError::MissingRecord),
};
    let taus = fast_and_rows(delta_ct, r_rows, &masks, p);
    let out = eval_gate(dd, &labels[a], masked[a], &labels[b], masked[b], &taus, prf, w, degree, p);
    let m = label_lsb(&out) ^ det;
    labels.push(out);
    masked.push(m);
},
    (_, Gate::Xor { .. }) => {
    match rec.next() {
    Some(_) => {
},
    Some(_) => return Err(DriverError::WrongRecord),
    None => return Err(DriverError::MissingRecord),
};
    let taus = xor_rows(delta_ct, false, p);
    let out = eval_gate(dd, &labels[a], masked[a], &labels[b], masked[b], &taus, prf, w, degree, p);
    labels.push(out);
    masked.push((masked[a] ^ masked[b]));
},
    (_, Gate::Not { .. }) => {
    labels.push(labels[a].clone());
    masked.push(masked[a]);
},
};
};
    Ok(circuit.outputs.iter().map(|o| masked[o]).collect::<Vec<_>>())
}

pub fn open_output(mut masked: bool, mut pi: bool) -> bool
{
    masked ^ pi
}

pub fn add_mod(mut left: u64, mut right: u64, mut modulus: u64) -> u64
{
    (((left as u128) + (right as u128)) % (modulus as u128)) as u64
}

pub fn sub_mod(mut left: u64, mut right: u64, mut modulus: u64) -> u64
{
    if left >= right{
    left - right
} else {
    modulus - (right - left)
}
}

pub fn mul_mod(mut left: u64, mut right: u64, mut modulus: u64) -> u64
{
    (((left as u128) * (right as u128)) % (modulus as u128)) as u64
}

pub fn pow_mod(mut base: u64, mut exponent: usize, mut modulus: u64) -> u64
{
    let mut result = 1 as u64;
    let mut base = base % modulus;
    while (exponent > 0){
    if (exponent & 1) == 1{
    result = mul_mod(result, base, modulus);
};
    base = mul_mod(base, base, modulus);
    exponent >>= 1;
};
    result
}

pub fn inverse_mod(mut value: u64, mut modulus: u64) -> Option<u64>
{
    let (mut t, mut new_t) = ((0 as i128), (1 as i128));
    let (mut r, mut new_r) = ((modulus as i128), ((value % modulus) as i128));
    while (new_r != 0){
    let quotient = r / new_r;
    (t, new_t) = (new_t, (t - (quotient * new_t)));
    (r, new_r) = (new_r, (r - (quotient * new_r)));
};
    if r != 1{
    return None;
};
    let mut t = t % (modulus as i128);
    if t < 0{
    t += (modulus as i128);
};
    Some((t as u64))
}

pub fn center(mut value: u64, mut modulus: u64) -> i128
{
    let value = value % modulus;
    if value > (modulus / 2){
    (value as i128) - (modulus as i128)
} else {
    value as i128
}
}

pub fn sample_uniform(mut random: &mut impl FnMut(&[u8]) -> Result<(), Error>, mut modulus: u64, mut output: &mut [u64]) -> Result<(), Error>
{
    let threshold = u64::MAX - (u64::MAX % modulus);
    for value in output{
    while true{
    let mut bytes = [(0 as u8); 8];
    random(&mut bytes)?;
    let candidate = u64::from_le_bytes(bytes);
    if candidate < threshold{
    *value = (candidate % modulus);
    break;
};
};
};
    Ok(())
}

pub fn bit_reverse(mut values: &mut [u64])
{
    let bits = values.len().ilog2();
    for index in 0.. values.len(){
    let reversed = index.reverse_bits() >> (usize::BITS - bits);
    if index < reversed{
    values.swap(index, reversed);
};
}
}

pub fn find_negacyclic_root(mut degree: usize, mut modulus: u64) -> Option<u64>
{
    let exponent = (modulus - 1) / (2 * (degree as u64));
    for candidate in 2.. modulus{
    let root = pow_mod(candidate, (exponent as usize), modulus);
    if pow_mod(root, degree, modulus) == (modulus - 1){
    return Some(root);
};
};
    None
}

pub fn negacyclic_mul_ntt(mut a: &[u64], mut b: &[u64], mut ntt: &Ntt) -> Vec<u64>
{
    let mut fa = a.to_vec();
    let mut fb = b.to_vec();
    ntt.forward(&mut fa);
    ntt.forward(&mut fb);
    let q = ntt.modulus;
    for (x, ..) in fa.iter_mut().zip(fb.iter()){
    *x = mul_mod(*x, y, q);
};
    ntt.inverse(&mut fa);
    fa
}

pub fn negacyclic_mul_coeffs(mut a: &[u64], mut b: &[u64], mut modulus: u64) -> Vec<u64>
{
    let n = a.len();
    let mut out = vec![(0 as u64); n];
    for (i, ..) in a.iter().enumerate(){
    for (j, ..) in b.iter().enumerate(){
    let k = i + j;
    if k < n{
    out[k] = add_mod(out[k], mul_mod(ai, bj, modulus), modulus);
} else {
    out[k - n] = sub_mod(out[k - n], mul_mod(ai, bj, modulus), modulus);
};
};
};
    out
}

pub fn sub_coeffs(mut a: &[u64], mut b: &[u64], mut modulus: u64) -> Vec<u64>
{
    a.iter().zip(b.into_iter()).map(|(x, y)| sub_mod(x, y, modulus)).collect::<Vec<_>>()
}

pub fn add_coeffs(mut a: &[u64], mut b: &[u64], mut modulus: u64) -> Vec<u64>
{
    a.iter().zip(b.into_iter()).map(|(x, y)| add_mod(x, y, modulus)).collect::<Vec<_>>()
}

pub fn offline_setup(mut ctx: &Context, mut wire_count: usize, mut prg: &impl ExpansionPrg, mut ksk_base: u64, mut ksk_digits: usize, mut online_depth_budget: usize, mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<OfflineGarbler, Error>
{
    let required_bits = (2 as usize).checked_mul(wire_count).ok_or(Error::InvalidParameters)?;
    let plan = ExpansionPlan::plan(required_bits, prg.depth(), prg.slots(), (ctx.max_level() + 1))?;
    let top = ctx.max_level();
    if top < (prg.depth() + online_depth_budget){
    return Err(Error::InvalidParameters);
};
    let secret = ctx.keygen(random)?;
    let ksks = ctx.key_switch_keyset(&secret, ksk_base, ksk_digits, random, noise)?;
    Ok(OfflineGarbler { secret: secret, ksks: ksks, plan: plan })
}

pub fn stream_expansion(mut ctx: &Context, mut wire_count: usize, mut prg: &impl ExpansionPrg, mut garbler: &OfflineGarbler, mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<Vec<ExpansionStep>, Error>
{
    let plan = garbler.plan;
    let top = ctx.max_level();
    let degree = ctx.parameters().degree;
    let online_base = top - prg.depth();
    let required_bits = 2 * wire_count;
    let mut top_expanded: Vec<Ciphertext> = Vec::with_capacity(required_bits);
    let prg_slots = prg.slots();
    for _ in 0.. plan.seed_ciphertexts{
    if top_expanded.len() >= required_bits{
    break;
};
    let mut seed_slots = vec![(0 as u64); degree];
    let mut byte = [(0 as u8); 1];
    for slot in seed_slots.iter_mut().take(prg_slots){
    random.fill_bytes(&mut byte[..])?;
    *slot = u64::from((byte[0] & 1));
};
    let ct = ctx.encrypt(&garbler.secret, top, &seed_slots, random, noise)?;
    top_expanded.extend(prg.expand(&garbler.ksks.l, ctx, &ct, &garbler.ksks)?);
};
    top_expanded.truncate(required_bits);
    let mut steps = Vec::with_capacity(wire_count);
    for (w, pair) in top_expanded.chunks_exact(2).enumerate(){
    let wire_state = garbler.wire_state(ctx, &plan, &pair[0], &pair[1], w)?;
    steps.push(ExpansionStep { wire_state: wire_state, eval: ExpandedWire { ct_a0: ctx.switch_to(&pair[0], online_base)?, ct_pi: ctx.switch_to(&pair[1], online_base)?, slot_a0: (((2 * w) % plan.slots_per_seed) % degree), slot_pi: ((((2 * w) + 1) % plan.slots_per_seed) % degree) } });
};
    Ok(steps)
}

pub fn offline_phase(mut ctx: &Context, mut wire_count: usize, mut prg: &impl ExpansionPrg, mut ksk_base: u64, mut ksk_digits: usize, mut online_depth_budget: usize, mut random: &mut impl RandomSource, mut noise: &mut impl NoiseSource) -> Result<(OfflineGarbler, OfflineEvaluator, WireStates), Error>
{
    let garbler = offline_setup(ctx, wire_count, prg, ksk_base, ksk_digits, online_depth_budget, random, noise)?;
    let plan = garbler.plan;
    let steps = stream_expansion(ctx, wire_count, prg, &garbler, random, noise)?;
    let mut states = Vec::with_capacity(wire_count);
    let mut wires = Vec::with_capacity(wire_count);
    for step in steps{
    states.push(step.wire_state);
    wires.push(step.eval);
};
    Ok((garbler, OfflineEvaluator { plan: plan, wires: wires }, states))
}

pub fn garble(mut ctx: &Context, mut equation: &impl GateEquation, mut circuit: &Circuit, mut states: &[WireState], mut delta: &GlobalDelta) -> Result<Vec<Stitch>, StitchError>
{
    circuit.validate()?;
    let p = ctx.parameters().plaintext_modulus;
    if delta.value() >= p{
    return Err(StitchError::NonCanonicalBit);
};
    if states.len() != circuit.wires(){
    return Err(StitchError::Exhausted);
};
    let mut wires: Vec<WireState> = states.to_vec();
    let mut records = Vec::new();
    for (index, gate) in circuit.gates.iter().enumerate(){
    let out_wire = circuit.inputs + index;
    let (ai, bi) = gate.operands();
    let (a, b) = (wires[ai], wires[bi]);
    match gate {
    Gate::And { .. } => {
    let out = wires[out_wire];
    records.push(equation.garble_and(index, &a, &b, &out));
},
    Gate::Xor { .. } => {
    let mut out = wires[out_wire];
    if !equation.xor_is_charged(){
    out = WireState { a0: ((a.a0 + b.a0) % p), pi: (a.pi ^ b.pi) };
    wires[out_wire] = out;
};
    match equation.garble_xor(p, &a, &b, &out) {
    Some(stitch) => {
    records.push(stitch);
},
    _ => {
},
}
},
    Gate::Not { .. } => {
    wires[out_wire] = WireState { a0: a.a0, pi: !a.pi };
},
};
};
    Ok(records)
}

pub fn evaluate(mut ctx: &Context, mut equation: &impl GateEquation, mut circuit: &Circuit, mut offline: &OfflineEvaluator, mut delta: &GlobalDelta, mut ksk: &KeySwitchKeySet, mut input_labels: &[Ciphertext], mut records: &[Stitch], mut output_key: &SecretKey) -> Result<Vec<bool>, StitchError>
{
    circuit.validate()?;
    if input_labels.len() != circuit.inputs{
    return Err(StitchError::Exhausted);
};
    let top = offline.wires.first().ok_or(StitchError::Exhausted)?.ct_a0.level();
    let mut wires: Vec<Ciphertext> = Vec::with_capacity(circuit.wires());
    for label in input_labels{
    wires.push(ctx.switch_to(label, top)?);
};
    let mut record_iter = records.iter();
    for (index, gate) in circuit.gates.iter().enumerate(){
    let out_wire = circuit.inputs + index;
    let (ai, bi) = gate.operands();
    let expanded = ExpandedGate { a: align_wire(ctx, offline.wire(ai).ok_or(StitchError::Exhausted)?, top)?, b: align_wire(ctx, offline.wire(bi).ok_or(StitchError::Exhausted)?, top)?, out: align_wire(ctx, offline.wire(out_wire).ok_or(StitchError::Exhausted)?, top)? };
    let la = &wires[ai];
    let lb = &wires[bi];
    let out = match gate {
    Gate::And { .. } => {
    let stitch = record_iter.next().ok_or(StitchError::Exhausted)?;
    equation.eval_and(ctx, la, lb, &expanded, delta, ksk, stitch)?
},
    Gate::Xor { .. } => {
    let stitch = if equation.xor_is_charged(){
    Some(record_iter.next().ok_or(StitchError::Exhausted)?)
} else {
    None
};
    equation.eval_xor(ctx, la, lb, &expanded, delta, ksk, stitch)?
},
    Gate::Not { .. } => {
    la.clone()
},
};
    wires.push(out);
};
    if record_iter.next().is_some(){
    return Err(StitchError::NonCanonicalBit);
};
    let p = ctx.parameters().plaintext_modulus;
    let inv_delta = delta.inverse(p).ok_or(StitchError::NonCanonicalBit)?;
    let mut outputs = Vec::with_capacity(circuit.outputs.len());
    for .. in &circuit.outputs{
    let expanded = offline.wire(wire).ok_or(StitchError::Exhausted)?;
    let label = &wires[wire];
    let diff = ctx.sub(label, &expanded.ct_a0)?;
    let payload = ctx.mul_plain(&diff, &ctx.constant_slots(inv_delta))?;
    let payload_bits = ctx.decrypt(output_key, &payload)?;
    let ct_pi = ctx.switch_to(&expanded.ct_pi, payload.level())?;
    let pi_bits = ctx.decrypt(output_key, &ct_pi)?;
    let masked = payload_bits[expanded.slot_a0] % 2;
    let pi = pi_bits[expanded.slot_pi] % 2;
    outputs.push(((masked ^ pi) == 1));
};
    Ok(outputs)
}

pub fn align_wire(mut ctx: &Context, mut wire: &ExpandedWire, mut level: usize) -> Result<ExpandedWire, StitchError>
{
    Ok(super::common::ExpandedWire { ct_a0: ctx.switch_to(&wire.ct_a0, level)?, ct_pi: ctx.switch_to(&wire.ct_pi, level)?, slot_a0: wire.slot_a0, slot_pi: wire.slot_pi })
}

pub fn blake2s_g(mut v: &mut [u32; 16], mut a: usize, mut b: usize, mut c: usize, mut d: usize, mut mx: u32, mut my: u32)
{
    v[a] = v[a].wrapping_add(v[b]).wrapping_add(mx);
    let t0 = v[d] ^ v[a];
    v[d] = ((t0 >> 16) | (t0 << 16));
    v[c] = v[c].wrapping_add(v[d]);
    let t1 = v[b] ^ v[c];
    v[b] = ((t1 >> 12) | (t1 << 20));
    v[a] = v[a].wrapping_add(v[b]).wrapping_add(my);
    let t2 = v[d] ^ v[a];
    v[d] = ((t2 >> 8) | (t2 << 24));
    v[c] = v[c].wrapping_add(v[d]);
    let t3 = v[b] ^ v[c];
    v[b] = ((t3 >> 7) | (t3 << 25));
}

pub fn blake2s_compress(mut h: &mut [u32; 8], mut block: &[u8; BLAKE2S_BLOCK], mut t: u64, mut last: bool)
{
    let mut m = [(0 as u32); 16];
    for i in 0.. 16{
    let mut chunk = [(0 as u8); 4];
    chunk.copy_from_slice(&block[(i * 4)..((i * 4) + 4)]);
    m[i] = u32::from_le_bytes(chunk);
};
    let mut v = [(0 as u32); 16];
    for i in 0.. 8{
    v[i] = h[i];
};
    for i in 0.. 8{
    v[i + 8] = BLAKE2S_IV[i];
};
    v[12] ^= (t as u32);
    v[13] ^= ((t >> 32) as u32);
    if last{
    v[14] ^= 4294967295;
};
    for round in 0.. 10{
    let base = round * 16;
    blake2s_g(&mut v, 0, 4, 8, 12, m[BLAKE2S_SIGMA[base]], m[BLAKE2S_SIGMA[base + 1]]);
    blake2s_g(&mut v, 1, 5, 9, 13, m[BLAKE2S_SIGMA[base + 2]], m[BLAKE2S_SIGMA[base + 3]]);
    blake2s_g(&mut v, 2, 6, 10, 14, m[BLAKE2S_SIGMA[base + 4]], m[BLAKE2S_SIGMA[base + 5]]);
    blake2s_g(&mut v, 3, 7, 11, 15, m[BLAKE2S_SIGMA[base + 6]], m[BLAKE2S_SIGMA[base + 7]]);
    blake2s_g(&mut v, 0, 5, 10, 15, m[BLAKE2S_SIGMA[base + 8]], m[BLAKE2S_SIGMA[base + 9]]);
    blake2s_g(&mut v, 1, 6, 11, 12, m[BLAKE2S_SIGMA[base + 10]], m[BLAKE2S_SIGMA[base + 11]]);
    blake2s_g(&mut v, 2, 7, 8, 13, m[BLAKE2S_SIGMA[base + 12]], m[BLAKE2S_SIGMA[base + 13]]);
    blake2s_g(&mut v, 3, 4, 9, 14, m[BLAKE2S_SIGMA[base + 14]], m[BLAKE2S_SIGMA[base + 15]]);
};
    for i in 0.. 8{
    h[i] ^= (v[i] ^ v[i + 8]);
}
}

pub fn blake2s_init() -> [u32; 8]
{
    let mut h = BLAKE2S_IV;
    h[0] ^= 16842784;
    h
}

pub fn blake2s_output(mut h: &[u32; 8]) -> [u8; BLAKE2S_OUT]
{
    let mut out = [(0 as u8); BLAKE2S_OUT];
    for i in 0.. 8{
    out[(i * 4)..((i * 4) + 4)].copy_from_slice(&h[i].to_le_bytes());
};
    out
}

pub fn blake2s_256(mut input: &[u8]) -> [u8; BLAKE2S_OUT]
{
    let mut h = blake2s_init();
    let n = input.len();
    let num_blocks = if n == 0{
    1
} else {
    n.div_ceil(BLAKE2S_BLOCK)
};
    let mut t = 0 as u64;
    for i in 0.. num_blocks{
    let mut block = [(0 as u8); BLAKE2S_BLOCK];
    let start = i * BLAKE2S_BLOCK;
    let take = (n - start).min(BLAKE2S_BLOCK);
    block[..take].copy_from_slice(&input[start..(start + take)]);
    t += (take as u64);
    blake2s_compress(&mut h, &block, t, (i == (num_blocks - 1)));
};
    blake2s_output(&h)
}

pub fn blake2s_256_array(mut n: usize, mut input: &Vec<u8>) -> [u8; BLAKE2S_OUT]
{
    let mut h = blake2s_init();
    let n = n;
    let num_blocks = if n == 0{
    1
} else {
    ((n + BLAKE2S_BLOCK) - 1) / BLAKE2S_BLOCK
};
    let mut t = 0 as u64;
    for i in 0.. num_blocks{
    let mut block = [(0 as u8); BLAKE2S_BLOCK];
    let start = i * BLAKE2S_BLOCK;
    for j in 0.. BLAKE2S_BLOCK{
    let idx = start + j;
    if idx < n{
    block[j] = input[idx];
};
};
    let take = if (start + BLAKE2S_BLOCK) <= n{
    BLAKE2S_BLOCK
} else {
    n - start
};
    t += (take as u64);
    blake2s_compress(&mut h, &block, t, (i == (num_blocks - 1)));
};
    blake2s_output(&h)
}

pub fn gen_abo<B: LengthDoubler, D: Digest>(mut k: usize, mut n: usize, mut a: Vec<u8>, mut rand: &impl AsRef<[u8]>) -> ABODyn<B, D> where B: Sized
{
    let mut h = D::new();
    let per_byte = (0..n).map(|_ni| {
    let mut per_byte = (0..k).map(|_| <Vec<u8>>::default()).collect::<Vec<Vec<u8>>>();
    for i in 0.. k{
    let core = (0..k.ilog2()).fold(a.clone(), |mut acc, b| {
    if ((i >> b) & 1) != 0{
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

pub fn mul_4x4(mut a: &[u64; 4], mut b: &[u64; 4]) -> [u64; 8]
{
    let mut r = [(0 as u64); 8];
    for i in 0.. 4{
    let mut carry: u64 = 0;
    for j in 0.. 4{
    let v = (r[i + j] as u128) + ((a[i] as u128) * (b[j] as u128)) + (carry as u128);
    r[i + j] = (v as u64);
    carry = ((v >> 64) as u64);
};
    r[i + 4] = carry;
};
    r
}

pub fn reduce_wide(mut t: &[u64; 8]) -> Fe25519
{
    let mut acc = [(0 as u64); 5];
    let mut c: u128 = 0;
    for i in 0.. 4{
    let v = (t[i] as u128) + ((t[4 + i] as u128) * 38) + c;
    acc[i] = (v as u64);
    c = (v >> 64);
};
    acc[4] = (c as u64);
    let mut out = [(0 as u64); 4];
    let mut c: u128 = (acc[4] as u128) * 38;
    for i in 0.. 4{
    let v = (acc[i] as u128) + c;
    out[i] = (v as u64);
    c = (v >> 64);
};
    if c != 0{
    let mut c2: u128 = c * 38;
    for i in 0.. 4{
    let v = (out[i] as u128) + c2;
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
    let mut tmp = [(0 as u64); 4];
    let mut borrow: u64 = 0;
    for i in 0.. 4{
    let (r1, b1) = x[i].overflowing_sub(P_LIMBS[i]);
    let (r2, b2) = r1.overflowing_sub(borrow);
    tmp[i] = r2;
    borrow = ((b1 as u64) | (b2 as u64));
};
    if borrow == 0{
    x = tmp;
};
};
    Fe25519(x)
}

pub fn fe_add(mut a: &Fe25519, mut b: &Fe25519) -> Fe25519
{
    let mut r = [(0 as u64); 4];
    let mut c: u64 = 0;
    for i in 0.. 4{
    let v = (a.0[i] as u128) + (b.0[i] as u128) + (c as u128);
    r[i] = (v as u64);
    c = ((v >> 64) as u64);
};
    if c != 0{
    let mut c2: u128 = (c as u128) * 38;
    for i in 0.. 4{
    let v = (r[i] as u128) + c2;
    r[i] = (v as u64);
    c2 = (v >> 64);
}
};
    fe_canonicalize(r)
}

pub fn fe_sub(mut a: &Fe25519, mut b: &Fe25519) -> Fe25519
{
    let mut neg_b = [(0 as u64); 4];
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
    let mut neg = [(0 as u64); 4];
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

pub fn fe_pow(mut base: &Fe25519, mut exp: &[u64; 4]) -> Fe25519
{
    let mut acc = Fe25519::ONE;
    for limb_idx in 0..4.rev(){
    for bit in 0..64.rev(){
    acc = fe_sq(&acc);
    if ((exp[limb_idx] >> bit) & 1) == 1{
    acc = fe_mul(&acc, base);
};
};
};
    acc
}

pub fn sqrt_m1() -> Fe25519
{
    let E: [u64; 4] = [18446744073709551611, 18446744073709551615, 18446744073709551615, 2305843009213693951];
    fe_pow(&Fe25519([2, 0, 0, 0]), &E)
}

pub fn fe_sqrt(mut w: &Fe25519) -> Option<Fe25519>
{
    let E: [u64; 4] = [18446744073709551614, 18446744073709551615, 18446744073709551615, 1152921504606846975];
    let c = fe_pow(w, &E);
    let c2 = fe_sq(&c);
    if c2 == *w{
    Some(c)
} else if c2 == fe_neg(w){
    Some(fe_mul(&c, &sqrt_m1()))
} else {
    None
}
}

pub fn is_square(mut w: &Fe25519) -> bool
{
    fe_sqrt(w).is_some()
}

pub fn fe_from_bytes_le(mut b: &[u8; 32]) -> Fe25519
{
    let mut limbs = [(0 as u64); 4];
    for i in 0.. 4{
    let mut chunk = [(0 as u8); 8];
    chunk.copy_from_slice(&b[(i * 8)..((i * 8) + 8)]);
    limbs[i] = u64::from_le_bytes(chunk);
};
    reduce_wide(&[limbs[0], limbs[1], limbs[2], limbs[3], 0, 0, 0, 0])
}

pub fn fe_invert(mut a: &Fe25519) -> Fe25519
{
    let exp_limbs: [u64; 4] = [18446744073709551595, 18446744073709551615, 18446744073709551615, 9223372036854775807];
    let mut acc = Fe25519::ONE;
    for limb_idx in 0..4.rev(){
    for bit in 0..64.rev(){
    acc = fe_sq(&acc);
    let b = (exp_limbs[limb_idx] >> bit) & 1;
    if b == 1{
    acc = fe_mul(&acc, a);
};
};
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
    let b = (k[byte_idx] >> bit) & 1;
    if b == 1{
    acc = ed_add(&acc, p);
};
};
};
    acc
}

pub fn ed_mul_cofactor(mut p: &EdPoint) -> EdPoint
{
    ed_double(&ed_double(&ed_double(p)))
}

pub fn hash_to_curve(mut domain: &[u8], mut index: u64) -> EdPoint
{
    for ctr in 0 as u64.. 0{
    let mut h = Sha3_256::new();
    h.update(domain);
    h.update(index.to_le_bytes());
    h.update(ctr.to_le_bytes());
    let out = h.finalize().to_vec();
    let mut xb = [(0 as u8); 32];
    xb.copy_from_slice(&out);
    let sign = (xb[31] >> 7) & 1;
    xb[31] &= 127;
    let x = fe_from_bytes_le(&xb);
    let xx = fe_sq(&x);
    let num = fe_add(&Fe25519::ONE, &xx);
    let den = fe_sub(&Fe25519::ONE, &fe_mul(&D, &xx));
    if den.is_zero(){
    continue;
};
    let yy = fe_mul(&num, &fe_invert(&den));
    let Some(mut y) = fe_sqrt(&yy) else {
    continue;
};
    if (y.to_bytes()[0] & 1) != sign{
    y = fe_neg(&y);
};
    let point = EdPoint { x: x, y: y, z: Fe25519::ONE, t: fe_mul(&x, &y) };
    let p8 = ed_mul_cofactor(&point);
    if p8 == EdPoint::IDENTITY{
    continue;
};
    return p8;
};
    unreachable!()
}

pub fn bytes_to_limbs(mut b: &[u8; 32]) -> [u64; 4]
{
    let mut out = [(0 as u64); 4];
    for i in 0.. 4{
    let mut le = [(0 as u8); 8];
    le.copy_from_slice(&b[(i * 8)..((i * 8) + 8)]);
    out[i] = u64::from_le_bytes(le);
};
    out
}

pub fn limbs_to_bytes(mut l: &[u64; 4]) -> [u8; 32]
{
    let mut out = [(0 as u8); 32];
    for i in 0.. 4{
    out[(i * 8)..((i * 8) + 8)].copy_from_slice(&l[i].to_le_bytes());
};
    out
}

pub fn cmp4(mut a: &[u64; 4], mut b: &[u64; 4]) -> Ordering
{
    for i in 0..4.rev(){
    if a[i] != b[i]{
    return a[i].cmp(&b[i]);
};
};
    core::cmp::Ordering::Equal
}

pub fn sub4(mut a: &[u64; 4], mut b: &[u64; 4]) -> [u64; 4]
{
    let mut out = [(0 as u64); 4];
    let mut borrow = 0 as u128;
    for i in 0.. 4{
    let d = ((((1 as u128) << 64) + (a[i] as u128)) - (b[i] as u128)) - borrow;
    out[i] = (d as u64);
    borrow = (1 - (d >> 64));
};
    out
}

pub fn add4(mut a: &[u64; 4], mut b: &[u64; 4]) -> [u64; 4]
{
    let mut out = [(0 as u64); 4];
    let mut carry = 0 as u128;
    for i in 0.. 4{
    let s = (a[i] as u128) + (b[i] as u128) + carry;
    out[i] = (s as u64);
    carry = (s >> 64);
};
    out
}

pub fn add_mod_l(mut a: &[u64; 4], mut b: &[u64; 4]) -> [u64; 4]
{
    let mut s = add4(a, b);
    if cmp4(&s, &L_LIMBS) != core::cmp::Ordering::Less{
    s = sub4(&s, &L_LIMBS);
};
    s
}

pub fn sub_mod_l(mut a: &[u64; 4], mut b: &[u64; 4]) -> [u64; 4]
{
    if cmp4(a, b) != core::cmp::Ordering::Less{
    sub4(a, b)
} else {
    sub4(&add4(a, &L_LIMBS), b)
}
}

pub fn mul4(mut a: &[u64; 4], mut b: &[u64; 4]) -> [u64; 8]
{
    let mut out = [(0 as u64); 8];
    for i in 0.. 4{
    let mut carry = 0 as u128;
    for j in 0.. 4{
    let idx = i + j;
    let cur = (out[idx] as u128) + ((a[i] as u128) * (b[j] as u128)) + carry;
    out[idx] = (cur as u64);
    carry = (cur >> 64);
};
    let mut k = i + 4;
    while ((carry > 0) && (k < 8)){
    let cur = (out[k] as u128) + carry;
    out[k] = (cur as u64);
    carry = (cur >> 64);
    k += 1;
};
};
    out
}

pub fn reduce512(mut x: &[u64; 8]) -> [u64; 4]
{
    let mut r = [(0 as u64); 4];
    for bit in 0..512.rev(){
    let xbit = (x[bit / 64] >> (bit % 64)) & 1;
    let mut carry = xbit;
    for limb in r.iter_mut(){
    let new_carry = *limb >> 63;
    *limb = ((*limb << 1) | carry);
    carry = new_carry;
};
    if cmp4(&r, &L_LIMBS) != core::cmp::Ordering::Less{
    r = sub4(&r, &L_LIMBS);
};
};
    r
}

pub fn gf_mul(mut a: u8, mut b: u8) -> u8
{
    volar_primitives::gf_mul_u8(a, b, GF8_AES_POLY)
}

pub fn sub_bytes(mut state: &mut [u8; BLOCK])
{
    for i in 0.. BLOCK{
    state[i] = SBOX[state[i] as usize];
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
    let i = 4 * c;
    let s0 = state[i];
    let s1 = state[i + 1];
    let s2 = state[i + 2];
    let s3 = state[i + 3];
    state[i] = (((gf_mul(s0, 2) ^ gf_mul(s1, 3)) ^ s2) ^ s3);
    state[i + 1] = (((s0 ^ gf_mul(s1, 2)) ^ gf_mul(s2, 3)) ^ s3);
    state[i + 2] = (((s0 ^ s1) ^ gf_mul(s2, 2)) ^ gf_mul(s3, 3));
    state[i + 3] = (((gf_mul(s0, 3) ^ s1) ^ s2) ^ gf_mul(s3, 2));
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
    let mut words = [[(0 as u8); 4]; (4 * NK_ROUND_KEYS)];
    for i in 0.. 4{
    words[i] = [key[4 * i], key[(4 * i) + 1], key[(4 * i) + 2], key[(4 * i) + 3]];
};
    for i in 4.. 4 * NK_ROUND_KEYS{
    let mut temp = words[i - 1];
    if (i % 4) == 0{
    let t0 = temp[0];
    temp[0] = temp[1];
    temp[1] = temp[2];
    temp[2] = temp[3];
    temp[3] = t0;
    for b in 0.. 4{
    temp[b] = SBOX[temp[b] as usize];
};
    temp[0] ^= RCON[i / 4];
};
    for b in 0.. 4{
    words[i][b] = (words[i - 4][b] ^ temp[b]);
};
};
    let mut round_keys = [[(0 as u8); BLOCK]; NK_ROUND_KEYS];
    for r in 0.. NK_ROUND_KEYS{
    for c in 0.. 4{
    let w = words[(4 * r) + c];
    round_keys[r][4 * c] = w[0];
    round_keys[r][(4 * c) + 1] = w[1];
    round_keys[r][(4 * c) + 2] = w[2];
    round_keys[r][(4 * c) + 3] = w[3];
};
};
    round_keys
}

pub fn encrypt_block(mut key: &[u8; BLOCK], mut plain: &[u8; BLOCK]) -> [u8; BLOCK]
{
    let round_keys = key_expansion(key);
    encrypt_block_with_keys(&round_keys, plain)
}

pub fn encrypt_block_with_keys(mut round_keys: &[[u8; BLOCK]; NK_ROUND_KEYS], mut plain: &[u8; BLOCK]) -> [u8; BLOCK]
{
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

pub fn collect_open_nodes_walk(mut node: usize, mut hidden: &[bool], mut tree: &[[u8; LAMBDA_BYTES]], mut leaf_count: usize, mut out: &mut Vec<(usize, [u8; LAMBDA_BYTES])>)
{
    if !hidden[node]{
    out.push((node, tree[node]));
    return;
};
    if node >= (leaf_count - 1){
    return;
};
    collect_open_nodes_walk(((2 * node) + 1), hidden, tree, leaf_count, out);
    collect_open_nodes_walk(((2 * node) + 2), hidden, tree, leaf_count, out);
}

pub fn convert_to_vole(mut seeds: &[Option<[u8; LAMBDA_BYTES]>], mut iv: &[u8; 16], mut tweak: u32, mut l_hat_bytes: usize) -> ConvertOutput
{
    let n = seeds.len();
    let d = n.trailing_zeros() as usize;
    let zero_block = vec![(0 as u8); l_hat_bytes];
    let mut r: Vec<Vec<u8>> = Vec::with_capacity(n);
    for i in 0.. seeds.len(){
    match seeds[i] {
    Some(seed) => r.push(aes_ctr_prg(&seed, iv, tweak, l_hat_bytes)),
    None => r.push(zero_block.clone()),
};
};
    let mut v: Vec<Vec<u8>> = Vec::with_capacity(d);
    for _j in 0.. d{
    v.push(vec![(0 as u8); l_hat_bytes]);
};
    let mut level: Vec<Vec<u8>> = r;
    for j in 0.. d{
    let half = level.len() / 2;
    let mut next: Vec<Vec<u8>> = Vec::with_capacity(half);
    for i in 0.. half{
    xor_in_place(&mut v[j], &level[(2 * i) + 1]);
    let mut new_entry = level[2 * i].clone();
    xor_in_place(&mut new_entry, &level[(2 * i) + 1]);
    next.push(new_entry);
};
    level = next;
};
    let u = level[0].clone();
    ConvertOutput { u: u, v: v }
}

pub fn concat_small_voles(mut outs: Vec<ConvertOutput>) -> BigVoleProver
{
    let l_hat = outs[0].u.len();
    for oi in 0.. outs.len(){
    let o = &outs[oi];
    for vi in 0.. o.v.len(){
};
};
    let u = outs[0].u.clone();
    let mut c: Vec<Vec<u8>> = Vec::with_capacity((outs.len() - 1));
    for oi in 1.. outs.len(){
    let mut ci = outs[oi].u.clone();
    xor_in_place(&mut ci, &u);
    c.push(ci);
};
    let mut v_columns: Vec<Vec<u8>> = Vec::new();
    for oi in 0.. outs.len(){
    for vi in 0.. outs[oi].v.len(){
    v_columns.push(outs[oi].v[vi].clone());
};
};
    BigVoleProver { u: u, c: c, v_columns: v_columns }
}

pub fn concat_small_voles_verifier(mut outs: Vec<ConvertOutput>, mut deltas: &[usize], mut corrections: &[Vec<u8>]) -> BigVoleVerifier
{
    let mut q_columns: Vec<Vec<u8>> = Vec::new();
    for i in 0.. outs.len(){
    let delta_i = deltas[i];
    for bit in 0.. outs[i].v.len(){
    let mut q = outs[i].v[bit].clone();
    if i >= 1{
    let delta_bit = ((delta_i >> bit) & 1) == 1;
    if delta_bit{
    xor_in_place(&mut q, &corrections[i - 1]);
}
};
    q_columns.push(q);
};
};
    BigVoleVerifier { q_columns: q_columns }
}

pub fn xor_in_place(mut a: &mut [u8], mut b: &[u8])
{
    for i in 0.. a.len(){
    a[i] ^= b[i];
}
}

pub fn add_to_upper_word(mut iv: &mut [u8; BLOCK], mut tweak: u32)
{
    let upper = u32::from_le_bytes([iv[12], iv[13], iv[14], iv[15]]);
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
    let lower = u32::from_le_bytes([out[0], out[1], out[2], out[3]]);
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
    let n_full = out_bytes / BLOCK;
    let rem = out_bytes % BLOCK;
    let mut out: Vec<u8> = alloc::vec::Vec::with_capacity(out_bytes);
    for i in 0.. n_full{
    let block_in = add_to_lower_word(&iv_tweaked, (i as u32));
    let ct = encrypt_block(seed, &block_in);
    for b in 0.. BLOCK{
    out.push(ct[b]);
};
};
    if rem > 0{
    let block_in = add_to_lower_word(&iv_tweaked, (n_full as u32));
    let ct = encrypt_block(seed, &block_in);
    let mut b = 0;
    while (b < rem){
    out.push(ct[b]);
    b += 1;
}
};
    out
}

pub fn keygen(mut rng: &mut impl SpecRng) -> (FaestSecretKey, FaestPublicKey)
{
    let mut sk = [(0 as u8); LAMBDA_BYTES];
    for i in 0.. LAMBDA_BYTES{
    sk[i] = rng.next_u8();
};
    let pk = encrypt_block(&sk, &[(0 as u8); LAMBDA_BYTES]);
    (FaestSecretKey(sk), FaestPublicKey(pk))
}

pub fn sign(mut sk: &FaestSecretKey, mut pk: &FaestPublicKey, mut message: &[u8], mut iv_seed: [u8; LAMBDA_BYTES], mut prover: &impl FaestAesProver) -> FaestSignature
{
    let iv: [u8; LAMBDA_BYTES] = encrypt_block(&iv_seed, &[(0 as u8); LAMBDA_BYTES]);
    let r: [u8; LAMBDA_BYTES] = encrypt_block(&sk.0, &iv);
    let commitment: BavcCommitmentDyn = BavcDyn::<EmLeafCommit, COM_BYTES, Sha3_256>::commit(32, r, &iv, TAU, SUB_VOLE_N);
    let mu: Vec<u8> = {
    let mut h = Sha3_256::new();
    DigestUpdate::update(&mut h, &pk.0);
    DigestUpdate::update(&mut h, message);
    Digest::finalize(h).to_vec()
};
    let chall_1 = chall1(&mu, &iv, &commitment.root, (LAMBDA_BYTES + 8), false);
    let deltas = expand_challenge_to_deltas(&chall_1, TAU, SUB_VOLE_N);
    let nodes = BavcDyn::<EmLeafCommit, COM_BYTES>::collect_open_nodes(32, &deltas, &recompute_tree(r, (TAU * SUB_VOLE_N)), TAU, SUB_VOLE_N);
    let mut hidden_commits: Vec<[u8; COM_BYTES]> = Vec::with_capacity(deltas.len());
    for i in 0.. deltas.len(){
    let d = deltas[i];
    hidden_commits.push(commitment.commitments[(i * SUB_VOLE_N) + d]);
};
    let opening = BavcOpeningDyn { hidden_commits: hidden_commits.clone(), nodes: nodes.clone(), com_bytes: 0 };
    let _ = opening;
    let mut sub_voles: Vec<ConvertOutput> = Vec::with_capacity(TAU);
    for i in 0.. TAU{
    let mut seeds_i: Vec<Option<[u8; LAMBDA_BYTES]>> = Vec::with_capacity(SUB_VOLE_N);
    for j in 0.. SUB_VOLE_N{
    seeds_i.push(Some(commitment.seeds[(i * SUB_VOLE_N) + j]));
};
    sub_voles.push(convert_to_vole(&seeds_i, &iv, (i as u32), L_HAT_BYTES));
};
    let big_vole: BigVoleProver = concat_small_voles(sub_voles);
    let mut corrections_flat: Vec<u8> = Vec::new();
    for i in 0.. big_vole.c.len(){
    for j in 0.. big_vole.c[i].len(){
    corrections_flat.push(big_vole.c[i][j]);
};
};
    let chall_2 = chall2(&chall_1, &big_vole.u, &corrections_flat, (LAMBDA_BYTES + 8), false);
    let hash_key = hash_key_from_chall(&chall_2);
    let qs_proof = prover.prove_aes_witness(&big_vole, &hash_key);
    let (chall_3, counter) = grind_chall3(&chall_2, &qs_proof.a_hat, &qs_proof.b_hat, &qs_proof.c_hat_base, LAMBDA_BYTES, W_GRIND, false, 1000000).expect("grinding must terminate within 1M iterations");
    let mut c_hat_with_counter = qs_proof.c_hat_base.clone();
    let counter_bytes = counter.to_le_bytes();
    for i in 0.. counter_bytes.len(){
    c_hat_with_counter.push(counter_bytes[i]);
};
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
    let reconstructed_seeds_opt = BavcDyn::<EmLeafCommit, COM_BYTES, Sha3_256>::reconstruct(32, &sig.nodes, &sig.hidden_commits, &deltas, iv, &sig.bavc_root, TAU, SUB_VOLE_N);
    let reconstructed_seeds = match reconstructed_seeds_opt {
    Some(s) => s,
    None => return false,
};
    let mut sub_voles_v: Vec<ConvertOutput> = Vec::with_capacity(TAU);
    for i in 0.. TAU{
    let d = deltas[i];
    let mut verifier_seeds: Vec<Option<[u8; LAMBDA_BYTES]>> = Vec::with_capacity(SUB_VOLE_N);
    for j in 0.. SUB_VOLE_N{
    if j == 0{
    verifier_seeds.push(None);
} else {
    verifier_seeds.push(Some(reconstructed_seeds[(i * SUB_VOLE_N) + (j ^ d)]));
};
};
    sub_voles_v.push(convert_to_vole(&verifier_seeds, iv, (i as u32), L_HAT_BYTES));
};
    let corrections = &sig.corrections;
    if corrections.len() != (TAU - 1){
    return false;
};
    let big_q: Vec<u8> = {
    let q_out = concat_small_voles_verifier(sub_voles_v, &deltas, corrections);
    let mut big_q: Vec<u8> = Vec::new();
    for i in 0.. q_out.q_columns.len(){
    for j in 0.. q_out.q_columns[i].len(){
    big_q.push(q_out.q_columns[i][j]);
};
};
    big_q
};
    let mut corrections_flat: Vec<u8> = Vec::new();
    for i in 0.. sig.corrections.len(){
    for j in 0.. sig.corrections[i].len(){
    corrections_flat.push(sig.corrections[i][j]);
};
};
    let chall_2 = chall2(&chall_1, &sig.vole_u, &corrections_flat, (LAMBDA_BYTES + 8), false);
    let hash_key = hash_key_from_chall(&chall_2);
    let derived_chall_3 = chall3(&chall_2, &sig.qs_proof.a_hat, &sig.qs_proof.b_hat, &sig.c_hat_with_counter, LAMBDA_BYTES, false);
    if derived_chall_3 != sig.chall_3{
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
    let mut out: Vec<usize> = Vec::with_capacity(tau);
    for i in 0.. tau{
    let byte = chall_1[i % chall_1.len()] as usize;
    out.push((byte % n));
};
    out
}

pub fn recompute_tree(mut r: [u8; LAMBDA_BYTES], mut total_leaves: usize) -> Vec<[u8; LAMBDA_BYTES]>
{
    let total_nodes = (2 * total_leaves) - 1;
    let mut tree = vec![[(0 as u8); LAMBDA_BYTES]; total_nodes];
    tree[0] = r;
    for node in 0.. total_leaves - 1{
    let parent = Vec::<u8, U16>(tree[node]);
    let .. = AesCtrLengthDoubler::double(parent);
    tree[(2 * node) + 1] = left.0;
    tree[(2 * node) + 2] = right.0;
};
    tree
}

pub fn hash_key_from_chall(mut chall: &[u8]) -> UniversalHashKey
{
    let mut r0_bytes = [(0 as u8); 16];
    let n = chall.len().min(16);
    r0_bytes[..n].copy_from_slice(&chall[..n]);
    let mut r1_bytes = [(0 as u8); 8];
    let off = n;
    let m = (chall.len() - off).min(8);
    r1_bytes[..m].copy_from_slice(&chall[off..(off + m)]);
    UniversalHashKey { r0: Galois128(u128::from_le_bytes(r0_bytes)), r1: Galois64(u64::from_le_bytes(r1_bytes)) }
}

pub fn has_trailing_zero_bits(mut bytes: &[u8], mut n: u32) -> bool
{
    if n == 0{
    return true;
};
    let n = n as usize;
    let full_bytes = n / 8;
    let rem = n % 8;
    if bytes.len() < (full_bytes + if rem > 0{
    1
} else {
    0
}){
    return false;
};
    for i in bytes.len() - full_bytes.. bytes.len(){
    if bytes[i] != 0{
    return false;
};
};
    if rem > 0{
    let mask = ((1 as u8) << rem) - 1;
    let idx = (bytes.len() - full_bytes) - 1;
    if (bytes[idx] & mask) != 0{
    return false;
}
};
    true
}

pub fn chall1(mut mu: &[u8], mut iv: &[u8; 16], mut com_bytes: &[u8], mut lambda_plus_b: usize, mut use_shake256: bool) -> Vec<u8>
{
    let mut t = if use_shake256{
    FaestTranscript::new_shake256()
} else {
    FaestTranscript::new_shake128()
};
    t.absorb(mu);
    t.absorb(iv);
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
    let mut c_hat_grind: Vec<u8> = alloc::vec::Vec::with_capacity((c_hat_base.len() + 4));
    for i in 0.. c_hat_base.len(){
    c_hat_grind.push(c_hat_base[i]);
};
    for i in 0.. counter_bytes.len(){
    c_hat_grind.push(counter_bytes[i]);
};
    let candidate = chall3(chall_2, a_hat, b_hat, &c_hat_grind, lambda, use_shake256);
    if has_trailing_zero_bits(&candidate, w_grind){
    return Some((candidate, counter));
};
};
    None
}

pub fn has_trailing_zero_bits(mut bytes: &[u8], mut n: u32) -> bool
{
    if n == 0{
    return true;
};
    let n = n as usize;
    let full_bytes = n / 8;
    let rem_bits = n % 8;
    if bytes.len() < (full_bytes + if rem_bits > 0{
    1
} else {
    0
}){
    return false;
};
    for i in bytes.len() - full_bytes.. bytes.len(){
    if bytes[i] != 0{
    return false;
};
};
    if rem_bits > 0{
    let mask = ((1 as u8) << rem_bits) - 1;
    let byte_idx = (bytes.len() - full_bytes) - 1;
    if (bytes[byte_idx] & mask) != 0{
    return false;
}
};
    true
}

pub fn vole_hash(mut key: &UniversalHashKey, mut input: &[u8]) -> UniversalHashOutput
{
    let n_full = input.len() / 16;
    let tail = &input[(n_full * 16)..];
    let mut h0 = Galois128(0);
    let mut h1 = Galois64(0);
    let mut pow0 = key.r0;
    let mut pow1 = key.r1;
    for i in 0.. n_full{
    let block = &input[(i * 16)..((i + 1) * 16)];
    let mut bytes = [(0 as u8); 16];
    bytes.copy_from_slice(block);
    let s = Galois128(u128::from_le_bytes(bytes));
    h0 = (h0 + (s * pow0));
    let s64 = Galois64((s.0 as u64));
    h1 = (h1 + (s64 * pow1));
    pow0 = (pow0 * key.r0);
    pow1 = (pow1 * key.r1);
};
    if !tail.is_empty(){
    let mut bytes = [(0 as u8); 8];
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
    let rhs0 = hv.h0 + (delta * (hu.h0 + hc.h0));
    let delta64 = Galois64((delta.0 as u64));
    let lhs1 = hq.h1;
    let rhs1 = hv.h1 + (delta64 * (hu.h1 + hc.h1));
    (lhs0 == rhs0) && (lhs1 == rhs1)
}

pub fn full_z<S: Clone>(mut w: &[S; AND_VARS], mut u: &S) -> [S; 8]
{
    [w[0].clone(), w[1].clone(), w[2].clone(), w[3].clone(), w[4].clone(), w[5].clone(), w[6].clone(), u.clone()]
}

pub fn eval_abc<S>(mut z: &[S; 8]) -> ([S; AND_CONS], [S; AND_CONS], [S; AND_CONS])
{
    let az = [z[K_A].clone(), z[K_C].clone(), ((z[P1].clone() + z[V_HAT].clone()) - z[P2].clone())];
    let bz = [z[K_B].clone(), z[DELTA].clone(), z[U].clone()];
    let cz = [z[P1].clone(), z[P2].clone(), S::default()];
    (az, bz, cz)
}

pub fn is_satisfied_relaxed<S>(mut w: &[S; AND_VARS], mut e: &[S; AND_CONS], mut u: &S) -> bool
{
    let z = full_z(w, u);
    let (az, bz, cz) = eval_abc(&z);
    let mut ok = true;
    for i in 0.. AND_CONS{
    let lhs = az[i].clone() * bz[i].clone();
    let rhs = (u.clone() * cz[i].clone()) + e[i].clone();
    ok = (ok && (lhs == rhs));
};
    ok
}

pub fn gate_witness<S>(mut k_a: S, mut k_b: S, mut k_c: S, mut delta: S, mut v_hat: S) -> [S; AND_VARS]
{
    let p1 = k_a.clone() * k_b.clone();
    let p2 = k_c.clone() * delta.clone();
    [k_a, k_b, k_c, delta, v_hat, p1, p2]
}

pub fn cross_term<S>(mut w1: &[S; AND_VARS], mut u1: &S, mut w2: &[S; AND_VARS], mut u2: &S) -> [S; AND_CONS]
{
    let z1 = full_z(w1, u1);
    let z2 = full_z(w2, u2);
    let (az1, bz1, cz1) = eval_abc(&z1);
    let (az2, bz2, cz2) = eval_abc(&z2);
    let mut t = [S::default(), S::default(), S::default()];
    for i in 0.. AND_CONS{
    let cross = (az1[i].clone() * bz2[i].clone()) + (az2[i].clone() * bz1[i].clone());
    let sub = (u1.clone() * cz2[i].clone()) + (u2.clone() * cz1[i].clone());
    t[i] = (cross - sub);
};
    t
}

pub fn fold_witness<S>(mut w1: &[S; AND_VARS], mut e1: &[S; AND_CONS], mut w2: &[S; AND_VARS], mut e2: &[S; AND_CONS], mut t: &[S; AND_CONS], mut r: &S) -> ([S; AND_VARS], [S; AND_CONS])
{
    let r2 = r.clone() * r.clone();
    let mut w = [S::default(), S::default(), S::default(), S::default(), S::default(), S::default(), S::default()];
    for i in 0.. AND_VARS{
    w[i] = (w1[i].clone() + (r.clone() * w2[i].clone()));
};
    let mut e = [S::default(), S::default(), S::default()];
    for i in 0.. AND_CONS{
    e[i] = ((e1[i].clone() + (r.clone() * t[i].clone())) + (r2.clone() * e2[i].clone()));
};
    (w, e)
}

pub fn fold_u<S>(mut u1: &S, mut u2: &S, mut r: &S) -> S
{
    u1.clone() + (r.clone() * u2.clone())
}

pub fn fold_blinder<S>(mut rho1: &S, mut rho2: &S, mut r: &S) -> S
{
    rho1.clone() + (r.clone() * rho2.clone())
}

pub fn fold_error_blinder<S>(mut re1: &S, mut rt: &S, mut re2: &S, mut r: &S) -> S
{
    let r2 = r.clone() * r.clone();
    re1.clone() + (r.clone() * rt.clone()) + (r2 * re2.clone())
}

pub fn fold_commit_w(mut comm_w1: &EdPoint, mut comm_w2: &EdPoint, mut r: &[u8; 32]) -> EdPoint
{
    ed_add(comm_w1, &ed_scalar_mul(comm_w2, r))
}

pub fn fold_commit_e(mut comm_e1: &EdPoint, mut comm_t: &EdPoint, mut comm_e2: &EdPoint, mut r: &[u8; 32], mut r2: &[u8; 32]) -> EdPoint
{
    ed_add(&ed_add(comm_e1, &ed_scalar_mul(comm_t, r)), &ed_scalar_mul(comm_e2, r2))
}

pub fn pedersen_commit(mut gens: &[EdPoint], mut h: &EdPoint, mut x: &[[u8; 32]], mut blind: &[u8; 32]) -> EdPoint
{
    let mut acc = ed_scalar_mul(h, blind);
    let mut i = 0 as usize;
    while (i < x.len()){
    acc = ed_add(&acc, &ed_scalar_mul(&gens[i], &x[i]));
    i += 1;
};
    acc
}

pub fn absorb_args_bases<D: Digest>(mut n: usize, mut d: D, mut args: &[&GarbleDyn]) -> D
{
    match args.split_first() {
    None => d,
    Some((first, rest)) => {
    d.update(&first.base);
    absorb_args_bases(n, d, rest)
},
}
}

pub fn gram_decode_label(mut n: usize, mut label: &EvalDyn, mut base: &GarbleDyn) -> bool
{
    (label.open(base.com_bytes, base)[0] & 1) != 0
}

pub fn gram_regarble(mut n: usize, mut secret: &GlobalSecretDyn, mut base: &GarbleDyn, mut bit: bool) -> EvalDyn
{
    secret.encode(base, bit)
}

pub fn keccak_f1600(mut st: &mut [u64; 25])
{
    for round in 0.. 24{
    let mut bc = [(0 as u64); 5];
    for i in 0.. 5{
    bc[i] = ((((st[i] ^ st[i + 5]) ^ st[i + 10]) ^ st[i + 15]) ^ st[i + 20]);
};
    for i in 0.. 5{
    let t = bc[(i + 4) % 5] ^ ((bc[(i + 1) % 5] << 1) | (bc[(i + 1) % 5] >> 63));
    for jj in 0.. 5{
    st[(jj * 5) + i] ^= t;
};
};
    let mut t = st[1];
    for i in 0.. 24{
    let j = KECCAK_PILN[i];
    let k = KECCAK_ROTC[i];
    let tmp = st[j];
    st[j] = ((t << k) | (t >> ((64 as u32) - k)));
    t = tmp;
};
    for jj in 0.. 5{
    let j = jj * 5;
    let mut row = [(0 as u64); 5];
    for i in 0.. 5{
    row[i] = st[j + i];
};
    for i in 0.. 5{
    st[j + i] ^= (!row[(i + 1) % 5] & row[(i + 2) % 5]);
};
};
    st[0] ^= KECCAK_RNDC[round];
}
}

pub fn bytes_to_lanes(mut b: &[u8; 200], mut st: &mut [u64; 25])
{
    for i in 0.. 25{
    let mut chunk = [(0 as u8); 8];
    chunk.copy_from_slice(&b[(i * 8)..((i * 8) + 8)]);
    st[i] = u64::from_le_bytes(chunk);
}
}

pub fn lanes_to_bytes(mut st: &[u64; 25], mut b: &mut [u8; 200])
{
    for i in 0.. 25{
    b[(i * 8)..((i * 8) + 8)].copy_from_slice(&st[i].to_le_bytes());
}
}

pub fn permute_bytes(mut state_bytes: &mut [u8; 200])
{
    let mut st = [(0 as u64); 25];
    bytes_to_lanes(state_bytes, &mut st);
    keccak_f1600(&mut st);
    lanes_to_bytes(&st, state_bytes);
}

pub fn spec_sponge_new(mut rate: usize) -> SpecSponge
{
    SpecSponge { state: [(0 as u8); 200], rate: rate, pos: 0 }
}

pub fn spec_sponge_absorb(mut sponge: &mut SpecSponge, mut data: &[u8])
{
    let mut state = sponge.state;
    let mut pos = sponge.pos;
    let rate = sponge.rate;
    for i in 0.. data.len(){
    state[pos] ^= data[i];
    pos += 1;
    if pos == rate{
    permute_bytes(&mut state);
    pos = 0;
};
};
    sponge.state = state;
    sponge.pos = pos;
}

pub fn spec_sponge_absorb_array(mut n: usize, mut sponge: &mut SpecSponge, mut data: &Vec<u8>)
{
    let mut state = sponge.state;
    let mut pos = sponge.pos;
    let rate = sponge.rate;
    for i in 0.. n{
    state[pos] ^= data[i];
    pos += 1;
    if pos == rate{
    permute_bytes(&mut state);
    pos = 0;
};
};
    sponge.state = state;
    sponge.pos = pos;
}

pub fn spec_sponge_finalize_xof(mut sponge: &SpecSponge, mut suffix: u8) -> SpecXofReader
{
    let mut state = sponge.state;
    state[sponge.pos] ^= suffix;
    state[sponge.rate - 1] ^= 128;
    permute_bytes(&mut state);
    SpecXofReader { state: state, rate: sponge.rate, pos: 0 }
}

pub fn spec_xof_read(mut reader: &mut SpecXofReader, mut out: &mut [u8])
{
    let mut state = reader.state;
    let mut pos = reader.pos;
    let rate = reader.rate;
    for i in 0.. out.len(){
    if pos == rate{
    permute_bytes(&mut state);
    pos = 0;
};
    out[i] = state[pos];
    pos += 1;
};
    reader.state = state;
    reader.pos = pos;
}

pub fn spec_xof_read_array(mut n: usize, mut reader: &mut SpecXofReader, mut out: &mut Vec<u8>)
{
    let mut state = reader.state;
    let mut pos = reader.pos;
    let rate = reader.rate;
    for i in 0.. n{
    if pos == rate{
    permute_bytes(&mut state);
    pos = 0;
};
    out[i] = state[pos];
    pos += 1;
};
    reader.state = state;
    reader.pos = pos;
}

pub fn spec_sha3_256(mut input: &[u8]) -> [u8; 32]
{
    let mut sponge = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb(&mut sponge, input);
    let mut reader = spec_sponge_finalize_xof(&sponge, 6);
    let mut out = [(0 as u8); 32];
    spec_xof_read(&mut reader, &mut out);
    out
}

pub fn spec_shake128(mut n: usize, mut input: &[u8]) -> Vec<u8>
{
    let mut sponge = spec_sponge_new(SHAKE128_RATE);
    spec_sponge_absorb(&mut sponge, input);
    let mut reader = spec_sponge_finalize_xof(&sponge, 31);
    let mut out = (0..n).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read(&mut reader, &mut out);
    out
}

pub fn context(mut params: NttParams) -> Option<Context>
{
    ntt_context::Context::new(params.degree, params.modulus)
}

pub fn negacyclic_mul(mut params: NttParams, mut a: &[u64], mut b: &[u64]) -> Vec<u64>
{
    ntt_context::negacyclic_mul(params.degree, params.modulus, a, b)
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

pub fn or_proof_challenge<G: ScalarOps, D: Digest>(mut s: &<G as _>::Element, mut r: &<G as _>::Element, mut a0: &<G as _>::Element, mut a1: &<G as _>::Element) -> <G as _>::Scalar
{
    let g = G::generator();
    let mut h = D::new();
    h.update(b"[118, 111, 108, 97, 114, 45, 99, 104, 111, 117, 45, 111, 114, 108, 97, 110, 100, 105, 45, 111, 114, 45, 112, 114, 111, 111, 102, 45, 118, 49]");
    G::<D>::write_element(&g, &mut h);
    G::<D>::write_element(s, &mut h);
    G::<D>::write_element(r, &mut h);
    G::<D>::write_element(a0, &mut h);
    G::<D>::write_element(a1, &mut h);
    G::<D>::scalar_from_hash(h)
}

pub fn ot_recv_prove<G: ScalarOps, D: Digest, R: SpecRng>(mut rng: &mut R, mut s: &<G as _>::Element, mut r: &<G as _>::Element, mut x: &<G as _>::Scalar, mut c: bool) -> OtConsistencyProofDyn<<G as _>::Scalar>
{
    let g = G::generator();
    let r1_stmt = G::add(r, &G::neg(s));
    let rr = G::random_mod_order(rng);
    let e_sim = G::random_mod_order(rng);
    let z_sim = G::random_mod_order(rng);
    let a_real = G::scalar_mul(&g, &rr);
    let r_sim_stmt = if c{
    r
} else {
    &r1_stmt
};
    let a_sim = G::add(&G::scalar_mul(&g, &z_sim), &G::neg(&G::scalar_mul(r_sim_stmt, &e_sim)));
    let (a0, a1) = if c{
    (a_sim, a_real)
} else {
    (a_real, a_sim)
};
    let e = or_proof_challenge::<G, D>(s, r, &a0, &a1);
    let e_real = G::scalar_sub(&e, &e_sim);
    let z_real = G::scalar_add(&rr, &G::scalar_mul_scalar(&e_real, x));
    let (e0, z0, e1, z1) = if c{
    (e_sim, z_sim, e_real, z_real)
} else {
    (e_real, z_real, e_sim, z_sim)
};
    OtConsistencyProofDyn { e0: e0, e1: e1, z0: z0, z1: z1 }
}

pub fn ot_send_verify<G: ScalarOps, D: Digest>(mut s: &<G as _>::Element, mut r: &<G as _>::Element, mut proof: &OtConsistencyProofDyn<<G as _>::Scalar>) -> bool
{
    let g = G::generator();
    let r1_stmt = G::add(r, &G::neg(s));
    let a0 = G::add(&G::scalar_mul(&g, &proof.z0), &G::neg(&G::scalar_mul(r, &proof.e0)));
    let a1 = G::add(&G::scalar_mul(&g, &proof.z1), &G::neg(&G::scalar_mul(&r1_stmt, &proof.e1)));
    let e = or_proof_challenge::<G, D>(s, r, &a0, &a1);
    G::scalar_eq(&e, &G::scalar_add(&proof.e0, &proof.e1))
}

pub fn ot_recv_malicious<G: ScalarOps, D: Digest, R: SpecRng>(mut rng: &mut R, mut s: <G as _>::Element, mut c: bool) -> (BaseOtReceiverDyn<G, D>, OtReceiverMsgDyn<G>, OtConsistencyProofDyn<<G as _>::Scalar>)
{
    let (state, msg) = ot_recv::<G, D, R>(rng, s, c);
    let proof = ot_recv_prove::<G, D, R>(rng, &state.s, &msg.r, &state.x, state.c);
    (state, msg, proof)
}

pub fn ot_send_finish_malicious<G: ScalarOps, D: Digest>(mut state: &BaseOtSenderDyn<G, D>, mut msg: &OtReceiverMsgDyn<G>, mut proof: &OtConsistencyProofDyn<<G as _>::Scalar>) -> Option<(Output<D>, Output<D>)>
{
    if !ot_send_verify::<G, D>(&state.s, &msg.r, proof){
    return None;
};
    Some(ot_send_finish::<G, D>(state, msg))
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

pub fn xor_block(mut a: &Block, mut b: &Block) -> Block
{
    let mut o = [(0 as u8); 16];
    for i in 0.. 16{
    o[i] = (a[i] ^ b[i]);
};
    o
}

pub fn sample_seed<R: SpecRng>(mut rng: &mut R) -> [u8; 16]
{
    let mut s = [(0 as u8); 16];
    for chunk in s.chunks_mut(4){
    chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
};
    s
}

pub fn ferret_prepare_receiver<R: SpecRng>(mut rng: &mut R, mut params: FerretParams, mut receiver_seed: &FerretReceiverSeed) -> FerretPrep
{
    let n = params.n;
    let t = params.t;
    let k = params.k;
    let alphas = sample_regular_noise(rng, n, t);
    let mut e = vec![false; n];
    for (i, ..) in alphas.iter().enumerate(){
    let (start, end) = params.interval_bounds(i).expect("regular noise interval index must be valid");
    e[start + a] = true;
};
    let lpn_seed = sample_seed(rng);
    let cot_r = &receiver_seed.u[k..];
    let choices = mpcot_reg_choice_bits(n, t, &alphas, cot_r);
    FerretPrep { alphas: alphas, e: e, lpn_seed: lpn_seed, choices: choices }
}

pub fn ferret_sender_mpcot<R: SpecRng>(mut rng: &mut R, mut params: FerretParams, mut sender_seed: &FerretSenderSeed, mut choices: &[bool]) -> (Vec<Block>, MpcotRegSenderMsg)
{
    let cot_q = &sender_seed.q[params.k..];
    mpcot_reg_sender(rng, &sender_seed.delta, params.n, params.t, cot_q, choices)
}

pub fn ferret_receiver_mpcot(mut params: FerretParams, mut prep: &FerretPrep, mut receiver_seed: &FerretReceiverSeed, mut mpcot: &MpcotRegSenderMsg) -> Vec<Block>
{
    let cot_t = &receiver_seed.w[params.k..];
    mpcot_reg_receiver(params.n, params.t, &prep.alphas, cot_t, mpcot)
}

pub fn ferret_finish(mut params: FerretParams, mut sender_seed: &FerretSenderSeed, mut receiver_seed: &FerretReceiverSeed, mut prep: &FerretPrep, mut s: &[Block], mut r: &[Block]) -> FerretExtendOut
{
    ferret_finish_m(params, sender_seed, receiver_seed, prep, s, r, params.seed_cot_count(false))
}

pub fn ferret_finish_m(mut params: FerretParams, mut sender_seed: &FerretSenderSeed, mut receiver_seed: &FerretReceiverSeed, mut prep: &FerretPrep, mut s: &[Block], mut r: &[Block], mut m: usize) -> FerretExtendOut
{
    let n = params.n;
    let k = params.k;
    let v_lpn = &sender_seed.q[..k];
    let u_lpn = &receiver_seed.u[..k];
    let w_lpn = &receiver_seed.w[..k];
    let (mut y, mut x, mut z) = encode_ferret_outputs(&prep.lpn_seed, k, n, v_lpn, u_lpn, w_lpn, s, &prep.e, r);
    let sender_out = y.split_off(m);
    let recv_x = x.split_off(m);
    let recv_z = z.split_off(m);
    let sender_seed = FerretSenderSeed { delta: sender_seed.delta, q: y };
    let receiver_seed = FerretReceiverSeed { u: x, w: z };
    FerretExtendOut { sender_out: sender_out, recv_x: recv_x, recv_z: recv_z, sender_seed: sender_seed, receiver_seed: receiver_seed }
}

pub fn ferret_extend<R: SpecRng>(mut rng: &mut R, mut params: FerretParams, mut sender_seed: &FerretSenderSeed, mut receiver_seed: &FerretReceiverSeed) -> FerretExtendOut
{
    let m = params.seed_cot_count(false);
    let prep = ferret_prepare_receiver(rng, params, receiver_seed);
    let (s, mpcot) = ferret_sender_mpcot(rng, params, sender_seed, &prep.choices);
    let r = ferret_receiver_mpcot(params, &prep, receiver_seed, &mpcot);
    ferret_finish(params, sender_seed, receiver_seed, &prep, &s, &r)
}

pub fn ferret_extend_malicious<R: SpecRng>(mut rng: &mut R, mut params: FerretParams, mut sender_seed: &FerretSenderSeed, mut receiver_seed: &FerretReceiverSeed, mut transcript: &[u8]) -> (FerretExtendOut, bool)
{
    let m = params.seed_cot_count(true);
    let k = params.k;
    let n = params.n;
    let t = params.t;
    let spcot_cots = t * params.log_splen();
    let extra_off = k + spcot_cots;
    let alphas = sample_regular_noise(rng, n, t);
    let mut e = vec![false; n];
    for (i, ..) in alphas.iter().enumerate(){
    let (start, end) = params.interval_bounds(i).expect("regular noise interval index must be valid");
    e[start + a] = true;
};
    let lpn_seed = sample_seed(rng);
    let choices = mpcot_reg_choice_bits(n, t, &alphas, &receiver_seed.u[k..extra_off]);
    let (s_padded, mpcot) = mpcot_reg_sender_padded(rng, &sender_seed.delta, n, t, &sender_seed.q[k..extra_off], &choices);
    let r_padded = mpcot_reg_receiver_padded(n, t, &alphas, &receiver_seed.w[k..extra_off], &mpcot);
    let extra_q = &sender_seed.q[extra_off..(extra_off + KAPPA_BITS)];
    let extra_r = &receiver_seed.u[extra_off..(extra_off + KAPPA_BITS)];
    let extra_t = &receiver_seed.w[extra_off..(extra_off + KAPPA_BITS)];
    let check = mpcot_reg_consistency_check_padded(&sender_seed.delta, &s_padded, &r_padded, &alphas, extra_q, extra_r, extra_t, transcript);
    let s = mpcot_reg_flatten_outputs(n, t, &s_padded);
    let r = mpcot_reg_flatten_outputs(n, t, &r_padded);
    let prep = FerretPrep { alphas: alphas, e: e, lpn_seed: lpn_seed, choices: Vec::new() };
    let out = ferret_finish_m(params, sender_seed, receiver_seed, &prep, &s, &r, m);
    (out, check)
}

pub fn split_cot_chunks<T: Clone>(mut flat: &[T], mut heights: &[usize]) -> Vec<Vec<T>>
{
    let mut out = Vec::with_capacity(heights.len());
    let mut off = 0 as usize;
    for .. in heights{
    out.push(flat[off..(off + h)].to_vec());
    off += h;
};
    out
}

pub fn ferret_extend_uni<R: SpecRng>(mut rng: &mut R, mut params: FerretParams, mut hash_seed: [u8; 16], mut sender_seed: &FerretSenderSeed, mut receiver_seed: &FerretReceiverSeed) -> FerretExtendOut
{
    let m_seed = uni_seed_cot_count(&hash_seed, params);
    let k = params.k;
    let heights = uni_spcot_heights(&hash_seed, params.n, params.t);
    let points = sample_uniform_points(rng, params.n, params.t);
    let table = super::mpcot_uni::cuckoo_insert(&hash_seed, params.n, params.t, &points);
    let mut e = vec![false; params.n];
    for slot in &table{
    match slot {
    Some(x) => {
    e[*x] = true;
},
    _ => {
},
};
};
    let cot_r = split_cot_chunks(&receiver_seed.u[k..], &heights);
    let choices = mpcot_uni_choice_bits(params, &hash_seed, &table, &cot_r);
    let cot_q = split_cot_chunks(&sender_seed.q[k..], &heights);
    let (s, _s_bins, mpcot) = mpcot_uni_sender(rng, &sender_seed.delta, params, hash_seed, &cot_q, &choices);
    let cot_t = split_cot_chunks(&receiver_seed.w[k..], &heights);
    let (r, _r_bins) = mpcot_uni_receiver(params, &table, &cot_t, &mpcot);
    let prep = FerretPrep { alphas: points, e: e, lpn_seed: sample_seed(rng), choices: Vec::new() };
    let n = params.n;
    let v_lpn = &sender_seed.q[..k];
    let u_lpn = &receiver_seed.u[..k];
    let w_lpn = &receiver_seed.w[..k];
    let y_lpn = encode_blocks(&prep.lpn_seed, k, n, v_lpn);
    let x_bits = encode_bits(&prep.lpn_seed, k, n, u_lpn);
    let z_lpn = encode_blocks(&prep.lpn_seed, k, n, w_lpn);
    let mut y = Vec::with_capacity(n);
    let mut x = Vec::with_capacity(n);
    let mut z = Vec::with_capacity(n);
    for j in 0.. n{
    y.push(xor_block(&y_lpn[j], &s[j]));
    x.push((x_bits[j] ^ prep.e[j]));
    z.push(xor_block(&z_lpn[j], &r[j]));
};
    FerretExtendOut { sender_out: y[m_seed..].to_vec(), recv_x: x[m_seed..].to_vec(), recv_z: z[m_seed..].to_vec(), sender_seed: FerretSenderSeed { delta: sender_seed.delta, q: y[..m_seed].to_vec() }, receiver_seed: FerretReceiverSeed { u: x[..m_seed].to_vec(), w: z[..m_seed].to_vec() } }
}

pub fn ferret_extend_uni_malicious<R: SpecRng>(mut rng: &mut R, mut params: FerretParams, mut hash_seed: [u8; 16], mut sender_seed: &FerretSenderSeed, mut receiver_seed: &FerretReceiverSeed, mut transcript: &[u8]) -> (FerretExtendOut, bool)
{
    let m_seed = uni_seed_cot_count_malicious(&hash_seed, params);
    let k = params.k;
    let heights = uni_spcot_heights(&hash_seed, params.n, params.t);
    let spcot_cots: usize = heights.iter().sum();
    let extra_off = k + spcot_cots;
    let points = sample_uniform_points(rng, params.n, params.t);
    let table = super::mpcot_uni::cuckoo_insert(&hash_seed, params.n, params.t, &points);
    let mut e = vec![false; params.n];
    for slot in &table{
    match slot {
    Some(x) => {
    e[*x] = true;
},
    _ => {
},
};
};
    let cot_r = split_cot_chunks(&receiver_seed.u[k..extra_off], &heights);
    let choices = mpcot_uni_choice_bits(params, &hash_seed, &table, &cot_r);
    let cot_q = split_cot_chunks(&sender_seed.q[k..extra_off], &heights);
    let (s, s_bins, mpcot) = mpcot_uni_sender(rng, &sender_seed.delta, params, hash_seed, &cot_q, &choices);
    let cot_t = split_cot_chunks(&receiver_seed.w[k..extra_off], &heights);
    let (r, r_bins) = mpcot_uni_receiver(params, &table, &cot_t, &mpcot);
    let (alphas, lens) = mpcot_uni_bucket_params(params, &hash_seed, &table);
    let extra_q = &sender_seed.q[extra_off..(extra_off + KAPPA_BITS)];
    let extra_r = &receiver_seed.u[extra_off..(extra_off + KAPPA_BITS)];
    let extra_t = &receiver_seed.w[extra_off..(extra_off + KAPPA_BITS)];
    let check = mpcot_uni_consistency_check(&sender_seed.delta, &s_bins, &r_bins, &alphas, &lens, extra_q, extra_r, extra_t, transcript);
    let prep = FerretPrep { alphas: points, e: e, lpn_seed: sample_seed(rng), choices: Vec::new() };
    let n = params.n;
    let v_lpn = &sender_seed.q[..k];
    let u_lpn = &receiver_seed.u[..k];
    let w_lpn = &receiver_seed.w[..k];
    let y_lpn = encode_blocks(&prep.lpn_seed, k, n, v_lpn);
    let x_bits = encode_bits(&prep.lpn_seed, k, n, u_lpn);
    let z_lpn = encode_blocks(&prep.lpn_seed, k, n, w_lpn);
    let mut y = Vec::with_capacity(n);
    let mut x = Vec::with_capacity(n);
    let mut z = Vec::with_capacity(n);
    for j in 0.. n{
    y.push(xor_block(&y_lpn[j], &s[j]));
    x.push((x_bits[j] ^ prep.e[j]));
    z.push(xor_block(&z_lpn[j], &r[j]));
};
    let out = FerretExtendOut { sender_out: y[m_seed..].to_vec(), recv_x: x[m_seed..].to_vec(), recv_z: z[m_seed..].to_vec(), sender_seed: FerretSenderSeed { delta: sender_seed.delta, q: y[..m_seed].to_vec() }, receiver_seed: FerretReceiverSeed { u: x[..m_seed].to_vec(), w: z[..m_seed].to_vec() } };
    (out, check)
}

pub fn sample_seed_cots<R: SpecRng>(mut rng: &mut R, mut m: usize) -> (FerretSenderSeed, FerretReceiverSeed)
{
    let mut delta = [(0 as u8); 16];
    for chunk in delta.chunks_mut(4){
    chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
};
    let mut q = Vec::with_capacity(m);
    let mut u = Vec::with_capacity(m);
    let mut w = Vec::with_capacity(m);
    for _ in 0.. m{
    let mut row = [(0 as u8); 16];
    for chunk in row.chunks_mut(4){
    chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
};
    let bit = (rng.next_u32() & 1) == 1;
    let t = if bit{
    xor_block(&row, &delta)
} else {
    row
};
    q.push(row);
    u.push(bit);
    w.push(t);
};
    (FerretSenderSeed { delta: delta, q: q }, FerretReceiverSeed { u: u, w: w })
}

pub fn column_rows(mut seed: &[u8; 16], mut k: usize, mut j: usize) -> [usize; LOCALITY]
{
    column_rows_for_valid_k(seed, k, j)
}

pub fn column_rows_for_valid_k(mut seed: &[u8; 16], mut k: usize, mut j: usize) -> [usize; LOCALITY]
{
    let mut rows = [(0 as usize); LOCALITY];
    let mut fill = 0 as usize;
    let mut counter = 0 as u32;
    while (fill < LOCALITY){
    let mut h = Sha3_256::new();
    h.update(b"[102, 101, 114, 114, 101, 116, 45, 108, 112, 110, 45, 49, 48, 108, 111, 99, 97, 108, 45, 118, 49]");
    h.update(seed);
    h.update((j as u64).to_le_bytes());
    h.update(counter.to_le_bytes());
    let out = h.finalize().to_vec();
    for chunk in out.chunks_exact(4){
    if fill >= LOCALITY{
    break;
};
    let raw = u32::from_le_bytes(chunk.try_into().unwrap()) as usize;
    let row = raw % k;
    if !rows[..fill].contains(&row){
    rows[fill] = row;
    fill += 1;
};
};
    counter += 1;
};
    rows
}

pub fn encode_blocks(mut seed: &[u8; 16], mut k: usize, mut n: usize, mut v: &[Block]) -> Vec<Block>
{
    let mut y = vec![[(0 as u8); 16]; n];
    for j in 0.. n{
    let rows = column_rows_for_valid_k(seed, k, j);
    let mut acc = [(0 as u8); 16];
    for row in rows{
    for b in 0.. 16{
    acc[b] ^= v[row][b];
};
};
    y[j] = acc;
};
    y
}

pub fn encode_bits(mut seed: &[u8; 16], mut k: usize, mut n: usize, mut u: &[bool]) -> Vec<bool>
{
    let mut x = vec![false; n];
    for j in 0.. n{
    let rows = column_rows_for_valid_k(seed, k, j);
    let mut acc = false;
    for row in rows{
    acc ^= u[row];
};
    x[j] = acc;
};
    x
}

pub fn encode_ferret_outputs(mut seed: &[u8; 16], mut k: usize, mut n: usize, mut v: &[Block], mut u: &[bool], mut w: &[Block], mut s: &[Block], mut e: &[bool], mut r: &[Block]) -> (Vec<Block>, Vec<bool>, Vec<Block>)
{
    let mut y = Vec::with_capacity(n);
    let mut x = Vec::with_capacity(n);
    let mut z = Vec::with_capacity(n);
    for j in 0.. n{
    let rows = column_rows_for_valid_k(seed, k, j);
    let mut y_col = [(0 as u8); 16];
    let mut x_bit = false;
    let mut z_col = [(0 as u8); 16];
    for row in rows{
    x_bit ^= u[row];
    for b in 0.. 16{
    y_col[b] ^= v[row][b];
    z_col[b] ^= w[row][b];
};
};
    for b in 0.. 16{
    y_col[b] ^= s[j][b];
    z_col[b] ^= r[j][b];
};
    y.push(y_col);
    x.push((x_bit ^ e[j]));
    z.push(z_col);
};
    (y, x, z)
}

pub fn interval_bounds(mut n: usize, mut t: usize, mut index: usize) -> (usize, usize)
{
    let n = n as u128;
    let t = t as u128;
    let index = index as u128;
    ((((index * n) / t) as usize), ((((index + 1) * n) / t) as usize))
}

pub fn padded_interval_len(mut n: usize, mut t: usize) -> usize
{
    n.div_ceil(t).checked_next_power_of_two().expect("regular MPCOT interval length overflow")
}

pub fn sample_below<R: SpecRng>(mut rng: &mut R, mut upper: usize) -> usize
{
    let upper = u32::try_from(upper).expect("regular MPCOT interval exceeds RNG range");
    let zone = u32::MAX - (u32::MAX % upper);
    while true{
    let value = rng.next_u32();
    if value < zone{
    return ((value % upper) as usize);
};
}
}

pub fn mpcot_reg_sender<R: SpecRng>(mut rng: &mut R, mut delta: &Block, mut n: usize, mut t: usize, mut cot_q: &[Block], mut choices: &[bool]) -> (Vec<Block>, MpcotRegSenderMsg)
{
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    let mut s = Vec::with_capacity(n);
    let mut blocks = Vec::with_capacity(t);
    for i in 0.. t{
    let q = &cot_q[(i * h)..((i + 1) * h)];
    let ch = &choices[(i * h)..((i + 1) * h)];
    let (start, end) = interval_bounds(n, t, i);
    let (v, msg) = spcot_sender_extend(rng, delta, splen, q, ch);
    s.extend(v.into_iter().take((end - start)));
    blocks.push(msg);
};
    (s, MpcotRegSenderMsg { blocks: blocks })
}

pub fn mpcot_reg_sender_padded<R: SpecRng>(mut rng: &mut R, mut delta: &Block, mut n: usize, mut t: usize, mut cot_q: &[Block], mut choices: &[bool]) -> (Vec<Vec<Block>>, MpcotRegSenderMsg)
{
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    let mut outputs = Vec::with_capacity(t);
    let mut blocks = Vec::with_capacity(t);
    for i in 0.. t{
    let q = &cot_q[(i * h)..((i + 1) * h)];
    let ch = &choices[(i * h)..((i + 1) * h)];
    let (v, msg) = spcot_sender_extend(rng, delta, splen, q, ch);
    outputs.push(v);
    blocks.push(msg);
};
    (outputs, MpcotRegSenderMsg { blocks: blocks })
}

pub fn mpcot_reg_receiver(mut n: usize, mut t: usize, mut alphas: &[usize], mut cot_t: &[Block], mut msg: &MpcotRegSenderMsg) -> Vec<Block>
{
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    let mut r = Vec::with_capacity(n);
    for i in 0.. t{
    let (start, end) = interval_bounds(n, t, i);
    let t_rows = &cot_t[(i * h)..((i + 1) * h)];
    let w = spcot_receiver_extend(alphas[i], splen, t_rows, &msg.blocks[i]);
    r.extend(w.into_iter().take((end - start)));
};
    r
}

pub fn mpcot_reg_receiver_padded(mut n: usize, mut t: usize, mut alphas: &[usize], mut cot_t: &[Block], mut msg: &MpcotRegSenderMsg) -> Vec<Vec<Block>>
{
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    let mut outputs = Vec::with_capacity(t);
    for i in 0.. t{
    let t_rows = &cot_t[(i * h)..((i + 1) * h)];
    outputs.push(spcot_receiver_extend(alphas[i], splen, t_rows, &msg.blocks[i]));
};
    outputs
}

pub fn mpcot_reg_flatten_outputs(mut n: usize, mut t: usize, mut buckets: &[Vec<Block>]) -> Vec<Block>
{
    let mut output = Vec::with_capacity(n);
    for (index, bucket) in buckets.iter().enumerate(){
    let (start, end) = interval_bounds(n, t, index);
    output.extend(bucket.iter().take((end - start)).copied());
};
    output
}

pub fn mpcot_reg_choice_bits(mut n: usize, mut t: usize, mut alphas: &[usize], mut cot_r: &[bool]) -> Vec<bool>
{
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    let mut out = Vec::with_capacity((t * h));
    for i in 0.. t{
    let r = &cot_r[(i * h)..((i + 1) * h)];
    out.extend(spcot_choice_bits(alphas[i], h, r));
};
    out
}

pub fn sample_regular_noise<R: SpecRng>(mut rng: &mut R, mut n: usize, mut t: usize) -> Vec<usize>
{
    (0..t).map(|i| {
    let (start, end) = interval_bounds(n, t, i);
    sample_below(rng, (end - start))
}).collect::<Vec<_>>()
}

pub fn mpcot_reg_consistency_check(mut delta: &Block, mut s: &[Block], mut r: &[Block], mut alphas: &[usize], mut extra_q: &[Block], mut extra_r: &[bool], mut extra_t: &[Block], mut transcript: &[u8]) -> bool
{
    let t = alphas.len();
    if (t == 0) || (s.len() != r.len()) || (s.len() < t){
    return false;
};
    let n = s.len();
    let mut vs = Vec::with_capacity(t);
    let mut ws = Vec::with_capacity(t);
    for (index, ..) in alphas.iter().enumerate(){
    let (start, end) = interval_bounds(n, t, index);
    if alpha >= (end - start){
    return false;
};
    vs.push(&s[start..end]);
    ws.push(&r[start..end]);
};
    mpcot_reg_consistency_check_slices(delta, &vs, &ws, alphas, extra_q, extra_r, extra_t, transcript)
}

pub fn mpcot_reg_consistency_check_padded(mut delta: &Block, mut s: &[Vec<Block>], mut r: &[Vec<Block>], mut alphas: &[usize], mut extra_q: &[Block], mut extra_r: &[bool], mut extra_t: &[Block], mut transcript: &[u8]) -> bool
{
    let Some(first) = s.first() else {
    return false;
};
    if !first.len().is_power_of_two() || s.iter().any(|bucket| (bucket.len() != first.len())) || r.iter().any(|bucket| (bucket.len() != first.len())){
    return false;
};
    let vs: Vec<&[Block]> = s.iter().map(Vec::as_slice).collect();
    let ws: Vec<&[Block]> = r.iter().map(Vec::as_slice).collect();
    mpcot_reg_consistency_check_slices(delta, &vs, &ws, alphas, extra_q, extra_r, extra_t, transcript)
}

pub fn mpcot_reg_consistency_check_slices(mut delta: &Block, mut vs: &[&[Block]], mut ws: &[&[Block]], mut alphas: &[usize], mut extra_q: &[Block], mut extra_r: &[bool], mut extra_t: &[Block], mut transcript: &[u8]) -> bool
{
    let t = alphas.len();
    if (t == 0) || (vs.len() != t) || (ws.len() != t) || (extra_q.len() != KAPPA_BITS) || (extra_r.len() != KAPPA_BITS) || (extra_t.len() != KAPPA_BITS){
    return false;
};
    let mut lens = Vec::with_capacity(t);
    for (.., (v, w)) in alphas.iter().zip(vs.iter().zip(ws.into_iter())){
    if (v.len() != w.len()) || (alpha >= v.len()){
    return false;
};
    lens.push(v.len());
};
    let chis = spcot_batched_fs_chis(&lens, transcript);
    let x_star_prime = spcot_batched_masked_choice(alphas, extra_r, &chis);
    let hv = spcot_batched_sender_hash_v(delta, vs, extra_q, &x_star_prime, &chis);
    let hw = spcot_batched_receiver_hash_w(ws, extra_t, &chis);
    hv == hw
}

pub fn cuckoo_table_size(mut t: usize) -> usize
{
    t.saturating_mul(3).div_ceil(2).max((t + 1))
}

pub fn hash_i(mut seed: &[u8; 16], mut i: usize, mut x: usize, mut m: usize) -> usize
{
    let mut h = Sha3_256::new();
    h.update(b"[102, 101, 114, 114, 101, 116, 45, 99, 117, 99, 107, 111, 111, 45, 104, 45, 118, 49]");
    h.update(seed);
    h.update((i as u64).to_le_bytes());
    h.update((x as u64).to_le_bytes());
    let out = h.finalize().to_vec();
    let raw = u64::from_le_bytes(out[..8].try_into().unwrap());
    (raw as usize) % m
}

pub fn unique_bins(mut seed: &[u8; 16], mut x: usize, mut m: usize) -> Vec<usize>
{
    let mut js = Vec::with_capacity(TAU);
    for i in 0.. TAU{
    let j = hash_i(seed, i, x, m);
    if !js.contains(&j){
    js.push(j);
};
};
    js
}

pub fn cuckoo_insert(mut seed: &[u8; 16], mut n: usize, mut t: usize, mut points: &[usize]) -> Vec<Option<usize>>
{
    let m = cuckoo_table_size(t);
    let mut table = vec![None; m];
    let max_kicks = 8 * m.max(16);
    for .. in points{
    let mut x = item;
    for kick in 0.. max_kicks{
    let mut placed = false;
    for i in 0.. TAU{
    let j = hash_i(seed, i, x, m);
    if table[j].is_none(){
    table[j] = Some(x);
    placed = true;
    break;
};
};
    if placed{
    break;
};
    let i = kick % TAU;
    let j = hash_i(seed, i, x, m);
    x = table[j].replace(x).unwrap();
};
};
    table
}

pub fn build_buckets(mut seed: &[u8; 16], mut n: usize, mut m: usize) -> Vec<Vec<usize>>
{
    let mut buckets = vec![Vec::new(); m];
    for x in 0.. n{
    for i in 0.. TAU{
    let j = hash_i(seed, i, x, m);
    buckets[j].push(x);
};
};
    for b in &mut buckets{
    b.sort_unstable();
    b.dedup();
};
    buckets
}

pub fn next_pow2(mut x: usize) -> usize
{
    x.next_power_of_two().max(2)
}

pub fn mpcot_uni_sender<R: SpecRng>(mut rng: &mut R, mut delta: &Block, mut params: FerretParams, mut hash_seed: [u8; 16], mut cot_q_chunks: &[Vec<Block>], mut choices_chunks: &[Vec<bool>]) -> (Vec<Block>, Vec<Vec<Block>>, MpcotUniSenderMsg)
{
    let n = params.n;
    let m = cuckoo_table_size(params.t);
    let buckets = build_buckets(&hash_seed, n, m);
    let mut s_bins = Vec::with_capacity(m);
    let mut blocks = Vec::with_capacity(m);
    for j in 0.. m{
    let need = buckets[j].len() + 1;
    let splen = next_pow2(need);
    let (v, msg) = spcot_sender_extend(rng, delta, splen, &cot_q_chunks[j], &choices_chunks[j]);
    s_bins.push(v);
    blocks.push(msg);
};
    let mut s = vec![[(0 as u8); 16]; n];
    for x in 0.. n{
    let mut acc = [(0 as u8); 16];
    for j in unique_bins(&hash_seed, x, m){
    let pos = buckets[j].iter().position(|..| (y == x)).unwrap();
    for b in 0.. 16{
    acc[b] ^= s_bins[j][pos][b];
};
};
    s[x] = acc;
};
    (s, s_bins, MpcotUniSenderMsg { hash_seed: hash_seed, blocks: blocks })
}

pub fn mpcot_uni_receiver(mut params: FerretParams, mut table: &[Option<usize>], mut cot_t_chunks: &[Vec<Block>], mut msg: &MpcotUniSenderMsg) -> (Vec<Block>, Vec<Vec<Block>>)
{
    let n = params.n;
    let m = cuckoo_table_size(params.t);
    let buckets = build_buckets(&msg.hash_seed, n, m);
    let mut r_bins = Vec::with_capacity(m);
    for j in 0.. m{
    let need = buckets[j].len() + 1;
    let splen = next_pow2(need);
    let p = match table[j] {
    None => buckets[j].len(),
    Some(val) => buckets[j].iter().position(|..| (y == val)).unwrap(),
};
    let w = spcot_receiver_extend(p, splen, &cot_t_chunks[j], &msg.blocks[j]);
    r_bins.push(w);
};
    let mut r = vec![[(0 as u8); 16]; n];
    for x in 0.. n{
    let mut acc = [(0 as u8); 16];
    for j in unique_bins(&msg.hash_seed, x, m){
    let pos = buckets[j].iter().position(|..| (y == x)).unwrap();
    for b in 0.. 16{
    acc[b] ^= r_bins[j][pos][b];
};
};
    r[x] = acc;
};
    (r, r_bins)
}

pub fn mpcot_uni_bucket_params(mut params: FerretParams, mut hash_seed: &[u8; 16], mut table: &[Option<usize>]) -> (Vec<usize>, Vec<usize>)
{
    let n = params.n;
    let m = cuckoo_table_size(params.t);
    let buckets = build_buckets(hash_seed, n, m);
    let mut alphas = Vec::with_capacity(m);
    let mut lens = Vec::with_capacity(m);
    for j in 0.. m{
    let need = buckets[j].len() + 1;
    let splen = next_pow2(need);
    let p = match table[j] {
    None => buckets[j].len(),
    Some(val) => buckets[j].iter().position(|..| (y == val)).unwrap(),
};
    alphas.push(p);
    lens.push(splen);
};
    (alphas, lens)
}

pub fn mpcot_uni_consistency_check(mut delta: &Block, mut s_bins: &[Vec<Block>], mut r_bins: &[Vec<Block>], mut alphas: &[usize], mut lens: &[usize], mut extra_q: &[Block], mut extra_r: &[bool], mut extra_t: &[Block], mut transcript: &[u8]) -> bool
{
    let chis = spcot_batched_fs_chis(lens, transcript);
    let x_star_prime = spcot_batched_masked_choice(alphas, extra_r, &chis);
    let vs: Vec<&[Block]> = s_bins.iter().map(|b| b.as_slice()).collect::<Vec<_>>();
    let ws: Vec<&[Block]> = r_bins.iter().map(|b| b.as_slice()).collect::<Vec<_>>();
    let hv = spcot_batched_sender_hash_v(delta, &vs, extra_q, &x_star_prime, &chis);
    let hw = spcot_batched_receiver_hash_w(&ws, extra_t, &chis);
    hv == hw
}

pub fn mpcot_uni_choice_bits(mut params: FerretParams, mut hash_seed: &[u8; 16], mut table: &[Option<usize>], mut cot_r_chunks: &[Vec<bool>]) -> Vec<Vec<bool>>
{
    let n = params.n;
    let m = cuckoo_table_size(params.t);
    let buckets = build_buckets(hash_seed, n, m);
    let mut out = Vec::with_capacity(m);
    for j in 0.. m{
    let need = buckets[j].len() + 1;
    let splen = next_pow2(need);
    let h = splen.trailing_zeros() as usize;
    let p = match table[j] {
    None => buckets[j].len(),
    Some(val) => buckets[j].iter().position(|..| (y == val)).unwrap(),
};
    out.push(spcot_choice_bits(p, h, &cot_r_chunks[j]));
};
    out
}

pub fn uni_spcot_heights(mut hash_seed: &[u8; 16], mut n: usize, mut t: usize) -> Vec<usize>
{
    let m = cuckoo_table_size(t);
    let buckets = build_buckets(hash_seed, n, m);
    buckets.iter().map(|b| (next_pow2((b.len() + 1)).trailing_zeros() as usize)).collect::<Vec<_>>()
}

pub fn uni_seed_cot_count(mut hash_seed: &[u8; 16], mut params: FerretParams) -> usize
{
    params.k + uni_spcot_heights(hash_seed, params.n, params.t).iter().copied().sum()
}

pub fn uni_seed_cot_count_malicious(mut hash_seed: &[u8; 16], mut params: FerretParams) -> usize
{
    uni_seed_cot_count(hash_seed, params) + KAPPA_BITS
}

pub fn sample_uniform_points<R: SpecRng>(mut rng: &mut R, mut n: usize, mut t: usize) -> Vec<usize>
{
    let mut pts = Vec::with_capacity(t);
    while (pts.len() < t){
    let x = (rng.next_u32() as usize) % n;
    if !pts.contains(&x){
    pts.push(x);
};
};
    pts.sort_unstable();
    pts
}

pub fn xor_block(mut a: &Block, mut b: &Block) -> Block
{
    let mut o = [(0 as u8); 16];
    for i in 0.. 16{
    o[i] = (a[i] ^ b[i]);
};
    o
}

pub fn checked_seed_cot_count(mut params: FerretParams) -> Result<usize, PoolSeedError>
{
    checked_seed_cot_count_mode(params, false)
}

pub fn checked_seed_cot_count_malicious(mut params: FerretParams) -> Result<usize, PoolSeedError>
{
    checked_seed_cot_count_mode(params, true)
}

pub fn checked_seed_cot_count_mode(mut params: FerretParams, mut malicious: bool) -> Result<usize, PoolSeedError>
{
    if (params.t == 0) || (params.n < params.t) || (params.k < super::lpn::LOCALITY){
    return Err(PoolSeedError::InvalidParameters);
};
    let max_interval_len = params.n.div_ceil(params.t);
    let splen = max_interval_len.checked_next_power_of_two().ok_or(PoolSeedError::InvalidParameters)?;
    let body = params.t.checked_mul((splen.trailing_zeros() as usize)).and_then(|extra| params.k.checked_add(extra)).ok_or(PoolSeedError::InvalidParameters)?;
    let seed_count = if malicious{
    body.checked_add(KAPPA_BITS).ok_or(PoolSeedError::InvalidParameters)?
} else {
    body
};
    if seed_count >= params.n{
    return Err(PoolSeedError::InvalidParameters);
};
    Ok(seed_count)
}

pub fn new_pool<R: SpecRng>(mut rng: &mut R, mut params: FerretParams) -> (CotPoolSender, CotPoolReceiver)
{
    new_pool_mode(rng, params, false)
}

pub fn new_pool_malicious<R: SpecRng>(mut rng: &mut R, mut params: FerretParams) -> (CotPoolSender, CotPoolReceiver)
{
    new_pool_mode(rng, params, true)
}

pub fn new_pool_mode<R: SpecRng>(mut rng: &mut R, mut params: FerretParams, mut malicious: bool) -> (CotPoolSender, CotPoolReceiver)
{
    let m = params.seed_cot_count(malicious);
    let (seed_s, seed_r) = sample_seed_cots(rng, m);
    (CotPoolSender { params: params, seed: seed_s, out: VecDeque::new(), raise_n: None, malicious: malicious, refill_count: 0 }, CotPoolReceiver { params: params, seed: seed_r, out_x: VecDeque::new(), out_z: VecDeque::new(), malicious: malicious })
}

pub fn refill<R: SpecRng>(mut rng: &mut R, mut sender: &mut CotPoolSender, mut receiver: &mut CotPoolReceiver) -> bool
{
    let malicious = sender.malicious;
    if (malicious != receiver.malicious) || (sender.params != receiver.params){
    return false;
};
    let mut params = sender.params;
    let current_seed_count = match checked_seed_cot_count_mode(params, malicious) {
    Ok(count) => count,
    Err(_) => return false,
};
    if (sender.seed.q.len() != current_seed_count) || (receiver.seed.u.len() != current_seed_count) || (receiver.seed.w.len() != current_seed_count) || (sender.out.len() != receiver.out_x.len()) || (sender.out.len() != receiver.out_z.len()){
    return false;
};
    match sender.raise_n.take() {
    Some(n) => {
    let mut raised = params;
    raised.n = n;
    let raised_seed_count = match checked_seed_cot_count_mode(raised, malicious) {
    Ok(count) => count,
    Err(_) => return false,
};
    if (raised_seed_count != current_seed_count) || (raised_seed_count != sender.seed.q.len()) || (raised_seed_count != receiver.seed.u.len()) || (raised_seed_count != receiver.seed.w.len()){
    return false;
};
    params = raised;
    sender.params = params;
    receiver.params = params;
},
    _ => {
},
};
    if malicious{
    let Some(next_refill_count) = sender.refill_count.checked_add(1) else {
    return false;
};
    let mut transcript = b"[118, 111, 108, 97, 114, 45, 102, 101, 114, 114, 101, 116, 45, 112, 111, 111, 108, 45, 114, 101, 102, 105, 108, 108, 45, 118, 49]".to_vec();
    transcript.extend_from_slice(&sender.refill_count.to_le_bytes());
    let (out, check) = ferret_extend_malicious(rng, params, &sender.seed, &receiver.seed, &transcript);
    if !check{
    return false;
};
    sender.refill_count = next_refill_count;
    sender.seed = out.sender_seed;
    receiver.seed = out.receiver_seed;
    sender.out.extend(out.sender_out);
    receiver.out_x.extend(out.recv_x);
    receiver.out_z.extend(out.recv_z);
    true
} else {
    let out = ferret_extend(rng, params, &sender.seed, &receiver.seed);
    sender.seed = out.sender_seed;
    receiver.seed = out.receiver_seed;
    sender.out.extend(out.sender_out);
    receiver.out_x.extend(out.recv_x);
    receiver.out_z.extend(out.recv_z);
    true
}
}

pub fn ensure<R: SpecRng>(mut rng: &mut R, mut sender: &mut CotPoolSender, mut receiver: &mut CotPoolReceiver, mut need: usize) -> bool
{
    let watermark = sender.params.seed_cot_count(sender.malicious);
    while ((sender.remaining() < need) || (sender.remaining().saturating_sub(need) < watermark)){
    let before = sender.remaining();
    if !refill(rng, sender, receiver){
    return false;
};
};
    true
}

pub fn take_random<R: SpecRng>(mut rng: &mut R, mut sender: &mut CotPoolSender, mut receiver: &mut CotPoolReceiver, mut need: usize) -> Option<(Vec<Block>, Vec<bool>, Vec<Block>)>
{
    if !ensure(rng, sender, receiver, need){
    return None;
};
    let mut r0 = Vec::with_capacity(need);
    let mut x = Vec::with_capacity(need);
    let mut z = Vec::with_capacity(need);
    for _ in 0.. need{
    r0.push(sender.out.pop_front().unwrap());
    x.push(receiver.out_x.pop_front().unwrap());
    z.push(receiver.out_z.pop_front().unwrap());
};
    Some((r0, x, z))
}

pub fn bea95_chosen_bit(mut delta: &Block, mut r0: Block, mut x: bool, mut z: Block, mut b: bool) -> (Block, Block, bool)
{
    let d = b ^ x;
    let r0_chosen = if d{
    xor_block(&r0, delta)
} else {
    r0
};
    (r0_chosen, z, d)
}

pub fn xor_block(mut a: &Block, mut b: &Block) -> Block
{
    let mut o = [(0 as u8); KAPPA_BYTES];
    for i in 0.. KAPPA_BYTES{
    o[i] = (a[i] ^ b[i]);
};
    o
}

pub fn g_double(mut seed: Block) -> (Block, Block)
{
    let .. = AesCtrLengthDoubler::double(Vec::<u8, U16>(seed));
    (left.0, right.0)
}

pub fn crhf(mut input: &Block, mut tweak: u64) -> Block
{
    let mut h = Sha3_256::new();
    h.update(b"[102, 101, 114, 114, 101, 116, 45, 115, 112, 99, 111, 116, 45, 104, 45, 118, 49]");
    h.update(input);
    h.update(tweak.to_le_bytes());
    let out = h.finalize().to_vec();
    let mut b = [(0 as u8); KAPPA_BYTES];
    b.copy_from_slice(&out[..KAPPA_BYTES]);
    b
}

pub fn hash_prime(mut v: &Block) -> [u8; 32]
{
    let mut h = Sha3_256::new();
    h.update(b"[102, 101, 114, 114, 101, 116, 45, 115, 112, 99, 111, 116, 45, 104, 112, 45, 118, 49]");
    h.update(v);
    let out = h.finalize().to_vec();
    let mut b = [(0 as u8); 32];
    b.copy_from_slice(&out);
    b
}

pub fn sample_block<R: SpecRng>(mut rng: &mut R) -> Block
{
    let mut b = [(0 as u8); KAPPA_BYTES];
    for chunk in b.chunks_mut(4){
    let x = rng.next_u32().to_le_bytes();
    chunk.copy_from_slice(&x[..chunk.len()]);
};
    b
}

pub fn bit_msb(mut alpha: usize, mut h: usize, mut i: usize) -> bool
{
    ((alpha >> ((h - 1) - i)) & 1) == 1
}

pub fn spcot_choice_bits(mut alpha: usize, mut h: usize, mut cot_r: &[bool]) -> Vec<bool>
{
    (0..h).map(|i| ((cot_r[i] ^ bit_msb(alpha, h, i)) ^ true)).collect::<Vec<_>>()
}

pub fn expand_full(mut depth: usize, mut seed: Block) -> (Vec<Block>, Vec<[Block; 2]>)
{
    let mut level: Vec<Block> = vec![seed];
    let mut sums = Vec::with_capacity(depth);
    for _d in 0.. depth{
    let mut next = Vec::with_capacity((level.len() * 2));
    let mut k0 = [(0 as u8); KAPPA_BYTES];
    let mut k1 = [(0 as u8); KAPPA_BYTES];
    for node in &level{
    let (l, r) = g_double(*node);
    for i in 0.. KAPPA_BYTES{
    k0[i] ^= l[i];
    k1[i] ^= r[i];
};
    next.push(l);
    next.push(r);
};
    sums.push([k0, k1]);
    level = next;
};
    (level, sums)
}

pub fn recover_sibling(mut layer: &mut [Block], mut sum: Block, mut offset: usize, mut select: bool)
{
    let on = offset + (select as usize);
    let off = offset + (!select as usize);
    if on < layer.len(){
    layer[on] = [(0 as u8); KAPPA_BYTES];
};
    if off < layer.len(){
    layer[off] = [(0 as u8); KAPPA_BYTES];
};
    let mut value = sum;
    let mut i = !select as usize;
    while (i < layer.len()){
    value = xor_block(&value, &layer[i]);
    i += 2;
};
    layer[off] = value;
}

pub fn expand_partial(mut depth: usize, mut sums: &[Block], mut alpha: usize) -> Vec<Block>
{
    let n = (1 as usize) << depth;
    let mut level: Vec<Block> = vec![[(0 as u8); KAPPA_BYTES]; 2];
    let mut offset = 0 as usize;
    for d in 0.. depth{
    let select = bit_msb(alpha, depth, d);
    recover_sibling(&mut level, sums[d], offset, select);
    if (d + 1) == depth{
    break;
};
    let mut next = vec![[(0 as u8); KAPPA_BYTES]; (level.len() * 2)];
    for (j, node) in level.iter().enumerate(){
    let (l, r) = g_double(*node);
    next[2 * j] = l;
    next[(2 * j) + 1] = r;
};
    offset = ((offset + (select as usize)) << 1);
    level = next;
};
    level
}

pub fn spcot_sender_extend<R: SpecRng>(mut rng: &mut R, mut delta: &Block, mut n: usize, mut cot_q: &[Block], mut choices: &[bool]) -> (Vec<Block>, SpcotSenderMsg)
{
    let h = n.trailing_zeros() as usize;
    let seed = sample_block(rng);
    let (leaves, layer_sums) = expand_full(h, seed);
    let mut ms = Vec::with_capacity(h);
    for i in 0.. h{
    let b = choices[i];
    let q = cot_q[i];
    let q_xor_delta = xor_block(&q, delta);
    let (k0, k1) = if b{
    (q_xor_delta, q)
} else {
    (q, q_xor_delta)
};
    let tweak = i as u64;
    ms.push([xor_block(&layer_sums[i][0], &crhf(&k0, tweak)), xor_block(&layer_sums[i][1], &crhf(&k1, tweak))]);
};
    let mut c = *delta;
    for leaf in &leaves{
    c = xor_block(&c, leaf);
};
    (leaves, SpcotSenderMsg { ms: ms, c: c, hash_v: Vec::new() })
}

pub fn spcot_receiver_extend(mut alpha: usize, mut n: usize, mut cot_t: &[Block], mut msg: &SpcotSenderMsg) -> Vec<Block>
{
    let h = n.trailing_zeros() as usize;
    let mut off_sums = Vec::with_capacity(h);
    for i in 0.. h{
    let select = bit_msb(alpha, h, i);
    let t = cot_t[i];
    let tweak = i as u64;
    let ht = crhf(&t, tweak);
    let m = if select{
    msg.ms[i][0]
} else {
    msg.ms[i][1]
};
    off_sums.push(xor_block(&m, &ht));
};
    let mut w = expand_partial(h, &off_sums, alpha);
    let mut acc = msg.c;
    for (i, wi) in w.iter().enumerate(){
    if i != alpha{
    acc = xor_block(&acc, wi);
};
};
    w[alpha] = acc;
    w
}

pub fn spcot_in_process<R: SpecRng>(mut rng: &mut R, mut delta: &Block, mut n: usize, mut alpha: usize) -> (Vec<Block>, Vec<Block>)
{
    let h = n.trailing_zeros() as usize;
    let mut cot_q = Vec::with_capacity(h);
    let mut cot_r = Vec::with_capacity(h);
    let mut cot_t = Vec::with_capacity(h);
    for _ in 0.. h{
    let q = sample_block(rng);
    let r = (rng.next_u32() & 1) == 1;
    let t = if r{
    xor_block(&q, delta)
} else {
    q
};
    cot_q.push(q);
    cot_r.push(r);
    cot_t.push(t);
};
    let choices = spcot_choice_bits(alpha, h, &cot_r);
    let (v, msg) = spcot_sender_extend(rng, delta, n, &cot_q, &choices);
    let w = spcot_receiver_extend(alpha, n, &cot_t, &msg);
    (v, w)
}

pub fn spcot_fs_chis(mut n: usize, mut transcript: &[u8]) -> Vec<Block>
{
    let mut h = Sha3_256::new();
    h.update(b"[102, 101, 114, 114, 101, 116, 45, 115, 112, 99, 111, 116, 45, 102, 115, 45, 118, 49]");
    h.update(transcript);
    let seed = h.finalize().to_vec();
    let mut chi = [(0 as u8); KAPPA_BYTES];
    chi.copy_from_slice(&seed[..KAPPA_BYTES]);
    let mut chi_pow = field_from_block(&chi);
    let mut chis = Vec::with_capacity(n);
    for _ in 0.. n{
    chis.push(block_from_field(&chi_pow));
    chi_pow = field_mul(&chi_pow, &field_from_block(&chi));
};
    chis
}

pub fn poly_eval_at_x(mut blocks: &[Block]) -> Galois128
{
    let x_elem = crate::field::Galois128(2);
    let mut acc = crate::field::Galois128(0);
    let mut x_pow = crate::field::Galois128(1);
    for b in blocks{
    acc = (acc + field_mul(&field_from_block(b), &x_pow));
    x_pow = field_mul(&x_pow, &x_elem);
};
    acc
}

pub fn spcot_masked_choice(mut alpha: usize, mut extra_r: &[bool], mut chis: &[Block]) -> Vec<bool>
{
    let phi_bits = u128::from_le_bytes(chis[alpha]);
    (0..KAPPA_BITS).map(|i| (extra_r[i] ^ (((phi_bits >> i) & 1) == 1))).collect::<Vec<_>>()
}

pub fn spcot_sender_hash_v(mut delta: &Block, mut v: &[Block], mut extra_q: &[Block], mut x_star_prime: &[bool], mut chis: &[Block]) -> [u8; 32]
{
    let yp: Vec<Block> = (0..KAPPA_BITS).map(|i| {
    if x_star_prime[i]{
    xor_block(&extra_q[i], delta)
} else {
    extra_q[i]
}
}).collect::<Vec<_>>();
    let y = poly_eval_at_x(&yp);
    let mut ip = crate::field::Galois128(0);
    for i in 0.. v.len(){
    ip = (ip + field_mul(&field_from_block(&chis[i]), &field_from_block(&v[i])));
};
    hash_prime(&block_from_field(&(ip + y)))
}

pub fn spcot_receiver_hash_w(mut w: &[Block], mut extra_t: &[Block], mut chis: &[Block]) -> [u8; 32]
{
    let z = poly_eval_at_x(extra_t);
    let mut ip = crate::field::Galois128(0);
    for i in 0.. w.len(){
    ip = (ip + field_mul(&field_from_block(&chis[i]), &field_from_block(&w[i])));
};
    hash_prime(&block_from_field(&(ip + z)))
}

pub fn spcot_consistency_check(mut delta: &Block, mut v: &[Block], mut w: &[Block], mut extra_q: &[Block], mut extra_r: &[bool], mut extra_t: &[Block], mut transcript: &[u8]) -> bool
{
    let n = v.len();
    let chis = spcot_fs_chis(n, transcript);
    let mut alpha = None;
    for i in 0.. n{
    if v[i] != w[i]{
    if alpha.is_some(){
    return false;
};
    alpha = Some(i);
};
};
    let Some(a) = alpha else {
    return false
};
    if xor_block(&v[a], &w[a]) != *delta{
    return false;
};
    let x_star_prime = spcot_masked_choice(a, extra_r, &chis);
    let hv = spcot_sender_hash_v(delta, v, extra_q, &x_star_prime, &chis);
    let hw = spcot_receiver_hash_w(w, extra_t, &chis);
    hv == hw
}

pub fn spcot_batched_fs_chis(mut lens: &[usize], mut transcript: &[u8]) -> Vec<Vec<Block>>
{
    lens.iter().enumerate().map(|(l, n)| {
    let mut t = transcript.to_vec();
    t.extend_from_slice(&(l as u64).to_le_bytes());
    spcot_fs_chis(n, &t)
}).collect::<Vec<_>>()
}

pub fn spcot_batched_masked_choice(mut alphas: &[usize], mut extra_r: &[bool], mut chis: &[Vec<Block>]) -> Vec<bool>
{
    let mut phi = crate::field::Galois128(0);
    for (l, ..) in alphas.iter().enumerate(){
    phi = (phi + field_from_block(&chis[l][a]));
};
    let phi_bits = phi.0;
    (0..KAPPA_BITS).map(|i| (extra_r[i] ^ (((phi_bits >> i) & 1) == 1))).collect::<Vec<_>>()
}

pub fn spcot_batched_sender_hash_v(mut delta: &Block, mut vs: &[&[Block]], mut extra_q: &[Block], mut x_star_prime: &[bool], mut chis: &[Vec<Block>]) -> [u8; 32]
{
    let yp: Vec<Block> = (0..KAPPA_BITS).map(|i| {
    if x_star_prime[i]{
    xor_block(&extra_q[i], delta)
} else {
    extra_q[i]
}
}).collect::<Vec<_>>();
    let mut acc = poly_eval_at_x(&yp);
    for (l, v) in vs.iter().enumerate(){
    for i in 0.. v.len(){
    acc = (acc + field_mul(&field_from_block(&chis[l][i]), &field_from_block(&v[i])));
};
};
    hash_prime(&block_from_field(&acc))
}

pub fn spcot_batched_receiver_hash_w(mut ws: &[&[Block]], mut extra_t: &[Block], mut chis: &[Vec<Block>]) -> [u8; 32]
{
    let mut acc = poly_eval_at_x(extra_t);
    for (l, w) in ws.iter().enumerate(){
    for i in 0.. w.len(){
    acc = (acc + field_mul(&field_from_block(&chis[l][i]), &field_from_block(&w[i])));
};
};
    hash_prime(&block_from_field(&acc))
}

pub fn field_from_block(mut b: &Block) -> Galois128
{
    crate::field::Galois128(u128::from_le_bytes(*b))
}

pub fn block_from_field(mut g: &Galois128) -> Block
{
    g.0.to_le_bytes()
}

pub fn field_mul(mut a: &Galois128, mut b: &Galois128) -> Galois128
{
    *a * *b
}

pub fn field_mul_block(mut a: &Block, mut b: &Block) -> Block
{
    block_from_field(&field_mul(&field_from_block(a), &field_from_block(b)))
}

pub fn toy_mul(mut a: u64, mut b: u64) -> u64
{
    (a * b) % TOY_P
}

pub fn toy_pow(mut base: u64, mut exp: u64) -> u64
{
    let mut acc: u64 = 1;
    let mut b = base % TOY_P;
    while (exp > 0){
    if (exp & 1) == 1{
    acc = toy_mul(acc, b);
};
    b = toy_mul(b, b);
    exp >>= 1;
};
    acc
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
    if pos >= out.len(){
    return;
};
    out[pos] = (((byte >> bit) & 1) == 1);
    pos += 1;
};
};
    counter += 1;
}
}

pub fn pack_kappa(mut bits: &[bool; IKNP_KAPPA]) -> [u8; IKNP_KAPPA_BYTES]
{
    let mut out = [(0 as u8); IKNP_KAPPA_BYTES];
    for i in 0.. IKNP_KAPPA{
    if bits[i]{
    out[i / 8] |= (1 << (i % 8));
};
};
    out
}

pub fn iknp_cot_extend<G, D, R>(mut m: usize, mut l: usize, mut rng_s: &mut R, mut rng_r: &mut R, mut receiver_bits: &[bool; M], mut delta_msg: &[u8; L]) -> ([[u8; L]; M], [[u8; L]; M]) where D: Digest
{
    let (r0, v) = iknp_cot_extend_base::<ChouOrlandiDyn<G, D>, D, R, L>(l, rng_s, rng_r, receiver_bits.as_slice(), delta_msg);
    let mut sender_r0 = [[(0 as u8); l]; m];
    let mut receiver_v = [[(0 as u8); l]; m];
    for j in 0.. m{
    sender_r0[j] = r0[j];
    receiver_v[j] = v[j];
};
    (sender_r0, receiver_v)
}

pub fn iknp_cot_extend_base<B, D, R>(mut l: usize, mut rng_s: &mut R, mut rng_r: &mut R, mut receiver_bits: &[bool], mut delta_msg: &[u8; L]) -> (Vec<[u8; L]>, Vec<[u8; L]>) where B: BaseOt<IKNP_KAPPA_BYTES>, D: Digest
{
    let m = receiver_bits.len();
    let mut delta_ot = [false; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    delta_ot[i] = ((rng_s.next_u32() & 1) == 1);
};
    let delta_ot_bytes = pack_kappa(&delta_ot);
    let mut seeds_0 = [[(0 as u8); IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    let mut seeds_1 = [[(0 as u8); IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    for b in 0.. IKNP_KAPPA_BYTES{
    seeds_0[i][b] = ((rng_r.next_u32() & 255) as u8);
    seeds_1[i][b] = ((rng_r.next_u32() & 255) as u8);
};
};
    let mut chosen_seeds = [[(0 as u8); IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    let (s_state, setup) = B::sender_setup(rng_r);
    let (r_state, recv_msg) = B::recv_start(rng_s, &setup, delta_ot[i]);
    let payload = B::sender_payload(rng_r, &s_state, &recv_msg, &seeds_0[i], &seeds_1[i]);
    chosen_seeds[i] = B::recv_finish(&r_state, &payload);
};
    let (t_cols, u_msg) = iknp_receiver_u_cols::<D>(m, receiver_bits, &seeds_0, &seeds_1);
    let (sender_r0, corrections) = iknp_sender_from_u::<D, L>(l, m, delta_msg, &delta_ot, &delta_ot_bytes, &chosen_seeds, &u_msg);
    let receiver_v = iknp_receiver_finish::<D, L>(l, receiver_bits, &t_cols, &corrections);
    (sender_r0, receiver_v)
}

pub fn iknp_receiver_u_cols<D: Digest>(mut m: usize, mut receiver_bits: &[bool], mut seeds_0: &[[u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA], mut seeds_1: &[[u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA]) -> (Vec<Vec<bool>>, IknpUMsg)
{
    let mut t_cols = Vec::with_capacity(IKNP_KAPPA);
    let mut u_cols = Vec::with_capacity(IKNP_KAPPA);
    for i in 0.. IKNP_KAPPA{
    let mut t_col = vec![false; m];
    prg_to_bools::<D>(&seeds_0[i], &mut t_col);
    let mut prg1 = vec![false; m];
    prg_to_bools::<D>(&seeds_1[i], &mut prg1);
    let mut u_col = vec![false; m];
    for j in 0.. m{
    u_col[j] = ((t_col[j] ^ prg1[j]) ^ receiver_bits[j]);
};
    t_cols.push(t_col);
    u_cols.push(u_col);
};
    (t_cols, IknpUMsg { u_cols: u_cols })
}

pub fn iknp_sender_from_u<D: Digest>(mut l: usize, mut m: usize, mut delta_msg: &[u8; L], mut delta_ot: &[bool; IKNP_KAPPA], mut delta_ot_bytes: &[u8; IKNP_KAPPA_BYTES], mut chosen_seeds: &[[u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA], mut u_msg: &IknpUMsg) -> (Vec<[u8; L]>, Vec<[u8; L]>)
{
    let mut q_cols = Vec::with_capacity(IKNP_KAPPA);
    for i in 0.. IKNP_KAPPA{
    let mut prg_chosen = vec![false; m];
    prg_to_bools::<D>(&chosen_seeds[i], &mut prg_chosen);
    let mut q_col = vec![false; m];
    for j in 0.. m{
    if delta_ot[i]{
    q_col[j] = (prg_chosen[j] ^ u_msg.u_cols[i][j]);
} else {
    q_col[j] = prg_chosen[j];
};
};
    q_cols.push(q_col);
};
    let mut sender_r0 = Vec::with_capacity(m);
    let mut corrections = Vec::with_capacity(m);
    let mut q_row = [false; IKNP_KAPPA];
    for j in 0.. m{
    for i in 0.. IKNP_KAPPA{
    q_row[i] = q_cols[i][j];
};
    let q_bytes = pack_kappa(&q_row);
    let mut r0 = [(0 as u8); l];
    prg_with_index::<D>(&q_bytes, (j as u32), &mut r0);
    let mut q_xor_delta = q_bytes;
    for b in 0.. IKNP_KAPPA_BYTES{
    q_xor_delta[b] ^= delta_ot_bytes[b];
};
    let mut r1 = [(0 as u8); l];
    prg_with_index::<D>(&q_xor_delta, (j as u32), &mut r1);
    let mut correction = [(0 as u8); l];
    for b in 0.. l{
    correction[b] = ((r0[b] ^ r1[b]) ^ delta_msg[b]);
};
    sender_r0.push(r0);
    corrections.push(correction);
};
    (sender_r0, corrections)
}

pub fn iknp_receiver_finish<D: Digest>(mut l: usize, mut receiver_bits: &[bool], mut t_cols: &[Vec<bool>], mut corrections: &[[u8; L]]) -> Vec<[u8; L]>
{
    let m = receiver_bits.len();
    let mut receiver_v = Vec::with_capacity(m);
    let mut t_row = [false; IKNP_KAPPA];
    for j in 0.. m{
    for i in 0.. IKNP_KAPPA{
    t_row[i] = t_cols[i][j];
};
    let t_bytes = pack_kappa(&t_row);
    let mut v_pre = [(0 as u8); l];
    prg_with_index::<D>(&t_bytes, (j as u32), &mut v_pre);
    let mut vj = [(0 as u8); l];
    if receiver_bits[j]{
    for b in 0.. l{
    vj[b] = (v_pre[b] ^ corrections[j][b]);
}
} else {
    vj = v_pre;
};
    receiver_v.push(vj);
};
    receiver_v
}

pub fn zq_add(mut a: Zq, mut b: Zq) -> Zq
{
    a.wrapping_add(b) & LWE_Q_MASK
}

pub fn zq_sub(mut a: Zq, mut b: Zq) -> Zq
{
    a.wrapping_sub(b) & LWE_Q_MASK
}

pub fn zq_mul(mut a: Zq, mut b: Zq) -> Zq
{
    a.wrapping_mul(b) & LWE_Q_MASK
}

pub fn zq_neg(mut a: Zq) -> Zq
{
    LWE_Q.wrapping_sub(a) & LWE_Q_MASK
}

pub fn sample_noise<R: SpecRng>(mut rng: &mut R) -> Zq
{
    let span = (2 * LWE_NOISE_BOUND) + 1;
    let raw = rng.next_u32() % span;
    if raw <= LWE_NOISE_BOUND{
    raw
} else {
    zq_neg((raw - LWE_NOISE_BOUND))
}
}

pub fn sample_zq<R: SpecRng>(mut rng: &mut R) -> Zq
{
    rng.next_u32() & LWE_Q_MASK
}

pub fn lwe_ot_recv<R: SpecRng>(mut n: usize, mut rng: &mut R, mut crs: &LweOtCrsDyn, mut c: bool) -> (LweOtReceiverDyn, LweOtRecvMsgDyn)
{
    let mut s = [(0 as u32); n];
    for i in 0.. n{
    s[i] = sample_noise(rng);
};
    let mut pk_real = [(0 as u32); n];
    for i in 0.. n{
    let mut acc: Zq = 0;
    for j in 0.. n{
    acc = zq_add(acc, zq_mul(crs.a[i][j], s[j]));
};
    acc = zq_add(acc, sample_noise(rng));
    pk_real[i] = acc;
};
    let pk0 = if c{
    let mut pk0 = [(0 as u32); n];
    for i in 0.. n{
    pk0[i] = zq_sub(crs.h[i], pk_real[i]);
};
    pk0
} else {
    pk_real
};
    (LweOtReceiverDyn { s: s, c: c, n: 0 }, LweOtRecvMsgDyn { pk0: pk0, n: 0 })
}

pub fn encrypt_branch<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R, mut crs: &LweOtCrsDyn, mut pk: &[Zq; N], mut msg: &[u8; L]) -> ([Zq; N], [Zq; L])
{
    let mut r = [(0 as u32); n];
    for i in 0.. n{
    r[i] = sample_noise(rng);
};
    let mut u = [(0 as u32); n];
    for j in 0.. n{
    let mut acc: Zq = 0;
    for i in 0.. n{
    acc = zq_add(acc, zq_mul(crs.a[i][j], r[i]));
};
    acc = zq_add(acc, sample_noise(rng));
    u[j] = acc;
};
    let mut base: Zq = 0;
    for i in 0.. n{
    base = zq_add(base, zq_mul(pk[i], r[i]));
};
    let half_q = LWE_Q / 2;
    let mut v = [(0 as u32); l];
    for k in 0.. l{
    let plain = if (msg[k] & 1) == 1{
    half_q
} else {
    0
};
    v[k] = zq_add(zq_add(base, sample_noise(rng)), plain);
};
    (u, v)
}

pub fn encrypt_branch_dyn<R: SpecRng>(mut n: usize, mut rng: &mut R, mut crs: &LweOtCrsDyn, mut pk: &[Zq; N], mut msg_bits: &[u8]) -> (Vec<Zq>, Vec<Zq>)
{
    let mut r = [(0 as u32); n];
    for i in 0.. n{
    r[i] = sample_noise(rng);
};
    let mut u = vec![(0 as u32); n];
    for j in 0.. n{
    let mut acc: Zq = 0;
    for i in 0.. n{
    acc = zq_add(acc, zq_mul(crs.a[i][j], r[i]));
};
    acc = zq_add(acc, sample_noise(rng));
    u[j] = acc;
};
    let mut base: Zq = 0;
    for i in 0.. n{
    base = zq_add(base, zq_mul(pk[i], r[i]));
};
    let half_q = LWE_Q / 2;
    let mut v = vec![(0 as u32); msg_bits.len()];
    for (k, bit) in msg_bits.iter().enumerate(){
    let plain = if (bit & 1) == 1{
    half_q
} else {
    0
};
    v[k] = zq_add(zq_add(base, sample_noise(rng)), plain);
};
    (u, v)
}

pub fn encrypt_branch_secure<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R, mut crs: &LweOtCrsDyn, mut pk: &[Zq; N], mut msg: &[u8; L]) -> ([[Zq; N]; L], [Zq; L])
{
    let half_q = LWE_Q / 2;
    let mut u = [[(0 as u32); n]; l];
    let mut v = [(0 as u32); l];
    for k in 0.. l{
    let mut r = [(0 as u32); n];
    for i in 0.. n{
    r[i] = sample_noise(rng);
};
    for j in 0.. n{
    let mut acc: Zq = 0;
    for i in 0.. n{
    acc = zq_add(acc, zq_mul(crs.a[i][j], r[i]));
};
    acc = zq_add(acc, sample_noise(rng));
    u[k][j] = acc;
};
    let mut base: Zq = 0;
    for i in 0.. n{
    base = zq_add(base, zq_mul(pk[i], r[i]));
};
    let plain = if (msg[k] & 1) == 1{
    half_q
} else {
    0
};
    v[k] = zq_add(zq_add(base, sample_noise(rng)), plain);
};
    (u, v)
}

pub fn lwe_ot_send_secure<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R, mut crs: &LweOtCrsDyn, mut recv_msg: &LweOtRecvMsgDyn, mut m0: &[u8; L], mut m1: &[u8; L]) -> LweOtSenderMsgSecureLoweredDyn
{
    let pk0 = recv_msg.pk0;
    let mut pk1 = [(0 as u32); n];
    for i in 0.. n{
    pk1[i] = zq_sub(crs.h[i], pk0[i]);
};
    let (u0, v0) = encrypt_branch_secure::<R, N, L>(n, l, rng, crs, &pk0, m0);
    let (u1, v1) = encrypt_branch_secure::<R, N, L>(n, l, rng, crs, &pk1, m1);
    LweOtSenderMsgSecureLoweredDyn { u0: u0, v0: v0, u1: u1, v1: v1, n: 0, l: 0 }
}

pub fn lwe_ot_recv_decrypt_secure(mut n: usize, mut l: usize, mut receiver: &LweOtReceiverDyn, mut msg: &LweOtSenderMsgSecureLoweredDyn) -> [u8; L]
{
    let (us, vs) = if receiver.c{
    (&msg.u1, &msg.v1)
} else {
    (&msg.u0, &msg.v0)
};
    let quarter = LWE_Q / 4;
    let three_quarter = 3 * quarter;
    let mut out = [(0 as u8); l];
    for k in 0.. l{
    let mut s_dot_u: Zq = 0;
    for i in 0.. n{
    s_dot_u = zq_add(s_dot_u, zq_mul(receiver.s[i], us[k][i]));
};
    let raw = zq_sub(vs[k], s_dot_u);
    out[k] = if (raw > quarter) && (raw <= three_quarter){
    1
} else {
    0
};
};
    out
}

pub fn lwe_ot_send<R: SpecRng>(mut n: usize, mut l: usize, mut rng: &mut R, mut crs: &LweOtCrsDyn, mut recv_msg: &LweOtRecvMsgDyn, mut m0: &[u8; L], mut m1: &[u8; L]) -> LweOtSenderMsgLoweredDyn
{
    let pk0 = recv_msg.pk0;
    let mut pk1 = [(0 as u32); n];
    for i in 0.. n{
    pk1[i] = zq_sub(crs.h[i], pk0[i]);
};
    let (u0, v0) = encrypt_branch::<R, N, L>(n, l, rng, crs, &pk0, m0);
    let (u1, v1) = encrypt_branch::<R, N, L>(n, l, rng, crs, &pk1, m1);
    LweOtSenderMsgLoweredDyn { u0: u0, v0: v0, u1: u1, v1: v1, n: 0, l: 0 }
}

pub fn lwe_ot_send_bytes<R: SpecRng>(mut n: usize, mut rng: &mut R, mut crs: &LweOtCrsDyn, mut recv_msg: &LweOtRecvMsgDyn, mut m0: &[u8], mut m1: &[u8]) -> LweOtSenderMsgDyn
{
    let bits0 = bytes_to_bits(m0);
    let bits1 = bytes_to_bits(m1);
    let pk0 = recv_msg.pk0;
    let mut pk1 = [(0 as u32); n];
    for i in 0.. n{
    pk1[i] = zq_sub(crs.h[i], pk0[i]);
};
    let (u0, v0) = encrypt_branch_dyn(n, rng, crs, &pk0, &bits0);
    let (u1, v1) = encrypt_branch_dyn(n, rng, crs, &pk1, &bits1);
    LweOtSenderMsgDyn { u0: u0, v0: v0, u1: u1, v1: v1 }
}

pub fn bytes_to_bits(mut bytes: &[u8]) -> Vec<u8>
{
    let mut bits = Vec::with_capacity((bytes.len() * 8));
    for .. in bytes{
    for bit in 0.. 8{
    bits.push(((b >> bit) & 1));
};
};
    bits
}

pub fn bits_to_bytes(mut bits: &[u8], mut nbytes: usize) -> Vec<u8>
{
    let mut out = vec![(0 as u8); nbytes];
    for i in 0.. nbytes{
    let mut acc = 0 as u8;
    for bit in 0.. 8{
    let idx = (i * 8) + bit;
    if (idx < bits.len()) && (bits[idx] != 0){
    acc |= (1 << bit);
};
};
    out[i] = acc;
};
    out
}

pub fn lwe_ot_recv_decrypt(mut n: usize, mut l: usize, mut receiver: &LweOtReceiverDyn, mut sender_msg: &LweOtSenderMsgLoweredDyn) -> [u8; L]
{
    let (u, v) = if receiver.c{
    (&sender_msg.u1[..], &sender_msg.v1[..])
} else {
    (&sender_msg.u0[..], &sender_msg.v0[..])
};
    decrypt_coords::<N, L>(n, l, receiver, u, v)
}

pub fn decrypt_coords(mut n: usize, mut l: usize, mut receiver: &LweOtReceiverDyn, mut u: &[Zq], mut v: &[Zq]) -> [u8; L]
{
    let mut s_dot_u: Zq = 0;
    for i in 0.. n{
    s_dot_u = zq_add(s_dot_u, zq_mul(receiver.s[i], u[i]));
};
    let quarter = LWE_Q / 4;
    let three_quarter = 3 * quarter;
    let mut out = [(0 as u8); l];
    for k in 0.. l{
    let raw = zq_sub(v[k], s_dot_u);
    out[k] = if (raw > quarter) && (raw <= three_quarter){
    1
} else {
    0
};
};
    out
}

pub fn lwe_ot_recv_decrypt_bytes(mut n: usize, mut receiver: &LweOtReceiverDyn, mut sender_msg: &LweOtSenderMsgDyn, mut nbytes: usize) -> Vec<u8>
{
    let (u, v) = if receiver.c{
    (&sender_msg.u1[..], &sender_msg.v1[..])
} else {
    (&sender_msg.u0[..], &sender_msg.v0[..])
};
    let mut s_dot_u: Zq = 0;
    for i in 0.. n{
    s_dot_u = zq_add(s_dot_u, zq_mul(receiver.s[i], u[i]));
};
    let quarter = LWE_Q / 4;
    let three_quarter = 3 * quarter;
    let mut bits = vec![(0 as u8); v.len()];
    for k in 0.. v.len(){
    let raw = zq_sub(v[k], s_dot_u);
    bits[k] = if (raw > quarter) && (raw <= three_quarter){
    1
} else {
    0
};
};
    bits_to_bytes(&bits, nbytes)
}

pub fn lwe_ot_send_bytes_secure<R: SpecRng>(mut n: usize, mut rng: &mut R, mut crs: &LweOtCrsDyn, mut recv_msg: &LweOtRecvMsgDyn, mut m0: &[u8], mut m1: &[u8]) -> LweOtSenderMsgSecureDyn
{
    let bits0 = bytes_to_bits(m0);
    let bits1 = bytes_to_bits(m1);
    let pk0 = recv_msg.pk0;
    let mut pk1 = [(0 as u32); n];
    for i in 0.. n{
    pk1[i] = zq_sub(crs.h[i], pk0[i]);
};
    let half_q = LWE_Q / 2;
    let enc = |rng, pk, bits| {
    let mut u = Vec::with_capacity(bits.len());
    let mut v = Vec::with_capacity(bits.len());
    for .. in bits{
    let mut r = [(0 as u32); n];
    for i in 0.. n{
    r[i] = sample_noise(rng);
};
    let mut uk = vec![(0 as u32); n];
    for j in 0.. n{
    let mut acc: Zq = 0;
    for i in 0.. n{
    acc = zq_add(acc, zq_mul(crs.a[i][j], r[i]));
};
    acc = zq_add(acc, sample_noise(rng));
    uk[j] = acc;
};
    let mut base: Zq = 0;
    for i in 0.. n{
    base = zq_add(base, zq_mul(pk[i], r[i]));
};
    let plain = if (bit & 1) == 1{
    half_q
} else {
    0
};
    v.push(zq_add(zq_add(base, sample_noise(rng)), plain));
    u.push(uk);
};
    (u, v)
};
    let (u0, v0) = enc(rng, &pk0, &bits0);
    let (u1, v1) = enc(rng, &pk1, &bits1);
    LweOtSenderMsgSecureDyn { u0: u0, v0: v0, u1: u1, v1: v1 }
}

pub fn lwe_ot_recv_decrypt_bytes_secure(mut n: usize, mut receiver: &LweOtReceiverDyn, mut sender_msg: &LweOtSenderMsgSecureDyn, mut nbytes: usize) -> Vec<u8>
{
    let (us, vs) = if receiver.c{
    (&sender_msg.u1[..], &sender_msg.v1[..])
} else {
    (&sender_msg.u0[..], &sender_msg.v0[..])
};
    let quarter = LWE_Q / 4;
    let three_quarter = 3 * quarter;
    let mut bits = vec![(0 as u8); vs.len()];
    for k in 0.. vs.len(){
    let mut s_dot_u: Zq = 0;
    for i in 0.. n{
    s_dot_u = zq_add(s_dot_u, zq_mul(receiver.s[i], us[k][i]));
};
    let raw = zq_sub(vs[k], s_dot_u);
    bits[k] = if (raw > quarter) && (raw <= three_quarter){
    1
} else {
    0
};
};
    bits_to_bytes(&bits, nbytes)
}

pub fn softspoken_cot_extend<G, D, R>(mut k: usize, mut m: usize, mut l: usize, mut rng_s: &mut R, mut rng_r: &mut R, mut receiver_bits: &[bool; M], mut delta_msg: &[u8; L]) -> SoftSpokenOutLoweredDyn<D> where D: Digest
{
    let (sender_r0, receiver_v) = iknp_cot_extend::<G, D, _, M, L>(m, l, rng_s, rng_r, receiver_bits, delta_msg);
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
    let mut r0_reconstructed = [(0 as u8); l];
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
    SoftSpokenOutLoweredDyn { sender_r0: sender_r0, receiver_v: receiver_v, sender_tag: sender_tag, receiver_tag: receiver_tag, m: 0, l: 0 }
}

pub fn softspoken_cot_extend_base<B, D, R>(mut l: usize, mut rng_s: &mut R, mut rng_r: &mut R, mut receiver_bits: &[bool], mut delta_msg: &[u8; L]) -> SoftSpokenOutDynDyn<D> where B: BaseOt, D: Digest
{
    let (sender_r0, receiver_v) = iknp_cot_extend_base::<B, D, R, L>(l, rng_s, rng_r, receiver_bits, delta_msg);
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
    for j in 0.. receiver_bits.len(){
    let mut r0_reconstructed = [(0 as u8); l];
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
    SoftSpokenOutDynDyn { sender_r0: sender_r0, receiver_v: receiver_v, sender_tag: sender_tag, receiver_tag: receiver_tag, l: 0 }
}

pub fn softspoken_cot_extend_dyn<G, D, R>(mut l: usize, mut rng_s: &mut R, mut rng_r: &mut R, mut receiver_bits: &[bool], mut delta_msg: &[u8; L]) -> SoftSpokenOutDynDyn<D> where D: Digest
{
    softspoken_cot_extend_base::<ChouOrlandiDyn<G, D>, D, R, L>(l, rng_s, rng_r, receiver_bits, delta_msg)
}

pub fn bit_to_g128(mut b: bool) -> Galois128
{
    Galois128((b as u128))
}

pub fn stack_uses_lwe_base(mut n: usize, mut l: usize) -> bool
{
    core::mem::<<LweBaseOt<N> as BaseOt<L>>::SetupMsg>::size_of() > 0
}

pub fn sample_bytes<R: SpecRng>(mut l: usize, mut rng: &mut R) -> [u8; L]
{
    let mut b = [(0 as u8); l];
    for chunk in b.chunks_mut(4){
    chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
};
    b
}

pub fn ssp_sender_tag(mut delta_msg: &[u8; 16], mut r0: &[[u8; 16]]) -> Output<DigestImpl>
{
    let mut hs = DigestImpl::new();
    hs.update(TAG_DOMAIN);
    hs.update(delta_msg);
    for row in r0{
    hs.update(row);
};
    hs.finalize().to_vec()
}

pub fn ssp_receiver_tag(mut delta_msg: &[u8; 16], mut bits: &[bool], mut v: &[[u8; 16]]) -> Output<DigestImpl>
{
    let mut hr = DigestImpl::new();
    hr.update(TAG_DOMAIN);
    hr.update(delta_msg);
    for j in 0.. bits.len(){
    let mut r0r = [(0 as u8); 16];
    if bits[j]{
    for b in 0.. 16{
    r0r[b] = (v[j][b] ^ delta_msg[b]);
}
} else {
    r0r = v[j];
};
    hr.update(&r0r);
};
    hr.finalize().to_vec()
}

pub fn stack_setup_sender<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut params: FerretParams, mut io: &mut Io) -> CotPoolSender
{
    stack_setup_sender_m(rng, params, io, false)
}

pub fn stack_setup_sender_malicious<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut params: FerretParams, mut io: &mut Io) -> CotPoolSender
{
    stack_setup_sender_m(rng, params, io, true)
}

pub fn stack_setup_sender_m<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut params: FerretParams, mut io: &mut Io, mut malicious: bool) -> CotPoolSender
{
    let m = params.seed_cot_count(malicious);
    let delta_msg = sample_bytes::<R, 16>(16, rng);
    io.send(TAG_DELTA, &delta_msg);
    let mut delta_ot = [false; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    delta_ot[i] = ((rng.next_u32() & 1) == 1);
};
    let delta_ot_bytes = pack_kappa(&delta_ot);
    let mut chosen_seeds = [[(0 as u8); IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    let setup = decode_lwe_crs::<LWE_N>(16, &io.recv(TAG_LWE_SETUP));
    let (r_state, recv_msg) = Base::<IKNP_KAPPA_BYTES>::recv_start(rng, &setup, delta_ot[i]);
    io.send(TAG_LWE_RECV, &encode_lwe_recv(&recv_msg));
    let payload = decode_lwe_payload(&io.recv(TAG_LWE_PAYLOAD));
    chosen_seeds[i] = Base::<IKNP_KAPPA_BYTES>::recv_finish(&r_state, &payload);
};
    let u_msg = decode_iknp_u(&io.recv(TAG_IKNP_U));
    let (sender_r0, corrections) = iknp_sender_from_u::<DigestImpl, 16>(16, m, &delta_msg, &delta_ot, &delta_ot_bytes, &chosen_seeds, &u_msg);
    io.send(TAG_IKNP_CORR, &encode_iknp_corr(&corrections));
    let tag_s = ssp_sender_tag(&delta_msg, &sender_r0);
    io.send(TAG_SSP_S, tag_s.as_slice());
    let tag_r = io.recv(TAG_SSP_R);
    let mut q = Vec::with_capacity(m);
    for row in sender_r0{
    q.push(row);
};
    let mut sender = CotPoolSender { params: params, seed: FerretSenderSeed { delta: delta_msg, q: q }, out: alloc::collections::VecDeque::new(), raise_n: None, malicious: malicious, refill_count: 0 };
    if malicious{
    stack_refill_sender_malicious(rng, &mut sender, io);
} else {
    stack_refill_sender(rng, &mut sender, io);
};
    sender
}

pub fn stack_setup_receiver<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut params: FerretParams, mut io: &mut Io) -> CotPoolReceiver
{
    stack_setup_receiver_m(rng, params, io, false)
}

pub fn stack_setup_receiver_malicious<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut params: FerretParams, mut io: &mut Io) -> CotPoolReceiver
{
    stack_setup_receiver_m(rng, params, io, true)
}

pub fn stack_setup_receiver_m<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut params: FerretParams, mut io: &mut Io, mut malicious: bool) -> CotPoolReceiver
{
    let m = params.seed_cot_count(malicious);
    let delta_raw = io.recv(TAG_DELTA);
    let mut delta_msg = [(0 as u8); 16];
    delta_msg.copy_from_slice(&delta_raw);
    let mut bits = vec![false; m];
    for b in &mut bits{
    *b = ((rng.next_u32() & 1) == 1);
};
    let mut seeds_0 = [[(0 as u8); IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    let mut seeds_1 = [[(0 as u8); IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0.. IKNP_KAPPA{
    seeds_0[i] = sample_bytes(rng);
    seeds_1[i] = sample_bytes(rng);
};
    for i in 0.. IKNP_KAPPA{
    let (s_state, setup) = Base::<IKNP_KAPPA_BYTES>::sender_setup(rng);
    io.send(TAG_LWE_SETUP, &encode_lwe_crs(&setup));
    let recv_msg = decode_lwe_recv::<LWE_N>(16, &io.recv(TAG_LWE_RECV));
    let payload = Base::<IKNP_KAPPA_BYTES>::sender_payload(rng, &s_state, &recv_msg, &seeds_0[i], &seeds_1[i]);
    io.send(TAG_LWE_PAYLOAD, &encode_lwe_payload(&payload));
};
    let (t_cols, u_msg) = iknp_receiver_u_cols::<DigestImpl>(m, &bits, &seeds_0, &seeds_1);
    io.send(TAG_IKNP_U, &encode_iknp_u(&u_msg));
    let corrections = decode_iknp_corr(&io.recv(TAG_IKNP_CORR));
    let receiver_v = iknp_receiver_finish::<DigestImpl, 16>(16, &bits, &t_cols, &corrections);
    let tag_s = io.recv(TAG_SSP_S);
    let tag_r = ssp_receiver_tag(&delta_msg, &bits, &receiver_v);
    io.send(TAG_SSP_R, tag_r.as_slice());
    let mut w = Vec::with_capacity(m);
    for row in receiver_v{
    w.push(row);
};
    let mut receiver = CotPoolReceiver { params: params, seed: FerretReceiverSeed { u: bits, w: w }, out_x: alloc::collections::VecDeque::new(), out_z: alloc::collections::VecDeque::new(), malicious: malicious };
    if malicious{
} else {
    stack_refill_receiver(rng, &mut receiver, io);
};
    receiver
}

pub fn stack_refill_sender<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut sender: &mut CotPoolSender, mut io: &mut Io)
{
    let raw = io.recv(TAG_FERRET_OPEN);
    let (lpn_seed, choices) = decode_ferret_open(&raw);
    let (s, mpcot) = ferret_sender_mpcot(rng, sender.params, &sender.seed, &choices);
    io.send(TAG_FERRET_MPCOT, &encode_mpcot_reg(&mpcot));
    let out_full = encode_sender_only(sender, lpn_seed, &s, sender.params.seed_cot_count(false));
    sender.seed.q = out_full.seed_q;
    sender.out.extend(out_full.emit);
}

pub fn xor_block(mut a: &[u8; 16], mut b: &[u8; 16]) -> [u8; 16]
{
    let mut o = [(0 as u8); 16];
    for i in 0.. 16{
    o[i] = (a[i] ^ b[i]);
};
    o
}

pub fn encode_sender_only(mut sender: &CotPoolSender, mut lpn_seed: [u8; 16], mut s: &[[u8; 16]], mut m: usize) -> SenderLpn
{
    let k = sender.params.k;
    let n = sender.params.n;
    let y_lpn = encode_blocks(&lpn_seed, k, n, &sender.seed.q[..k]);
    let mut y = Vec::with_capacity(n);
    for j in 0.. n{
    y.push(xor_block(&y_lpn[j], &s[j]));
};
    SenderLpn { seed_q: y[..m].to_vec(), emit: y[m..].iter().copied().collect() }
}

pub fn stack_refill_receiver<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut receiver: &mut CotPoolReceiver, mut io: &mut Io)
{
    let prep = ferret_prepare_receiver(rng, receiver.params, &receiver.seed);
    io.send(TAG_FERRET_OPEN, &encode_ferret_open(&prep.lpn_seed, &prep.choices));
    let mpcot = decode_mpcot_reg(&io.recv(TAG_FERRET_MPCOT));
    let r = ferret_receiver_mpcot(receiver.params, &prep, &receiver.seed, &mpcot);
    let rec = encode_receiver_only(receiver, &prep, &r, receiver.params.seed_cot_count(false));
    receiver.seed = rec.seed;
    receiver.out_x.extend(rec.x);
    receiver.out_z.extend(rec.z);
}

pub fn stack_refill_sender_malicious<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut sender: &mut CotPoolSender, mut io: &mut Io)
{
    let params = sender.params;
    let k = params.k;
    let t = params.t;
    let extra_off = k + (t * params.log_splen());
    let raw = io.recv(TAG_FERRET_OPEN);
    let (lpn_seed, choices) = decode_ferret_open(&raw);
    let (s_padded, mpcot) = mpcot_reg_sender_padded(rng, &sender.seed.delta, params.n, t, &sender.seed.q[k..extra_off], &choices);
    let mpcot_bytes = encode_mpcot_reg(&mpcot);
    io.send(TAG_FERRET_MPCOT, &mpcot_bytes);
    let x_star_prime = decode_bools(&io.recv(TAG_FERRET_CHECK_MASK)).0;
    let mut transcript = raw.clone();
    transcript.extend_from_slice(&mpcot_bytes);
    let lens = vec![params.splen(); t];
    let chis = spcot_batched_fs_chis(&lens, &transcript);
    let vs: Vec<&[Block]> = s_padded.iter().map(Vec::as_slice).collect();
    let extra_q = &sender.seed.q[extra_off..(extra_off + KAPPA_BITS)];
    let hv = spcot_batched_sender_hash_v(&sender.seed.delta, &vs, extra_q, &x_star_prime, &chis);
    io.send(TAG_FERRET_CHECK_HV, &hv);
    let s = mpcot_reg_flatten_outputs(params.n, t, &s_padded);
    let m = params.seed_cot_count(true);
    let out_full = encode_sender_only(sender, lpn_seed, &s, m);
    sender.seed.q = out_full.seed_q;
    sender.out.extend(out_full.emit);
    sender.refill_count += 1;
}

pub fn stack_refill_receiver_malicious<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut receiver: &mut CotPoolReceiver, mut io: &mut Io) -> bool
{
    let params = receiver.params;
    let k = params.k;
    let n = params.n;
    let t = params.t;
    let extra_off = k + (t * params.log_splen());
    let alphas = sample_regular_noise(rng, n, t);
    let mut e = vec![false; n];
    for (i, ..) in alphas.iter().enumerate(){
    let (start, end) = params.interval_bounds(i).expect("regular noise interval index must be valid");
    e[start + a] = true;
};
    let lpn_seed = sample_seed(rng);
    let choices = mpcot_reg_choice_bits(n, t, &alphas, &receiver.seed.u[k..extra_off]);
    let open_bytes = encode_ferret_open(&lpn_seed, &choices);
    io.send(TAG_FERRET_OPEN, &open_bytes);
    let mpcot_bytes = io.recv(TAG_FERRET_MPCOT);
    let mpcot = decode_mpcot_reg(&mpcot_bytes);
    let r_padded = mpcot_reg_receiver_padded(n, t, &alphas, &receiver.seed.w[k..extra_off], &mpcot);
    let mut transcript = open_bytes.clone();
    transcript.extend_from_slice(&mpcot_bytes);
    let lens = vec![params.splen(); t];
    let chis = spcot_batched_fs_chis(&lens, &transcript);
    let extra_r = &receiver.seed.u[extra_off..(extra_off + KAPPA_BITS)];
    let extra_t = &receiver.seed.w[extra_off..(extra_off + KAPPA_BITS)];
    let x_star_prime = spcot_batched_masked_choice(&alphas, extra_r, &chis);
    io.send(TAG_FERRET_CHECK_MASK, &encode_bools(&x_star_prime));
    let hv = io.recv(TAG_FERRET_CHECK_HV);
    let ws: Vec<&[Block]> = r_padded.iter().map(Vec::as_slice).collect();
    let hw = spcot_batched_receiver_hash_w(&ws, extra_t, &chis);
    let mut hv_arr = [(0 as u8); 32];
    hv_arr.copy_from_slice(&hv);
    let check = hv_arr == hw;
    if !check{
    return false;
};
    let prep = FerretPrep { alphas: alphas, e: e, lpn_seed: lpn_seed, choices: choices };
    let m = params.seed_cot_count(true);
    let r = mpcot_reg_flatten_outputs(n, t, &r_padded);
    let rec = encode_receiver_only(receiver, &prep, &r, m);
    receiver.seed = rec.seed;
    receiver.out_x.extend(rec.x);
    receiver.out_z.extend(rec.z);
    true
}

pub fn encode_receiver_only(mut receiver: &CotPoolReceiver, mut prep: &FerretPrep, mut r: &[[u8; 16]], mut m: usize) -> RecvLpn
{
    let k = receiver.params.k;
    let n = receiver.params.n;
    let x_bits = encode_bits(&prep.lpn_seed, k, n, &receiver.seed.u[..k]);
    let z_lpn = encode_blocks(&prep.lpn_seed, k, n, &receiver.seed.w[..k]);
    let mut x = Vec::with_capacity(n);
    let mut z = Vec::with_capacity(n);
    for j in 0.. n{
    x.push((x_bits[j] ^ prep.e[j]));
    z.push(xor_block(&z_lpn[j], &r[j]));
};
    RecvLpn { seed: FerretReceiverSeed { u: x[..m].to_vec(), w: z[..m].to_vec() }, x: x[m..].to_vec(), z: z[m..].to_vec() }
}

pub fn ensure_sender<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut sender: &mut CotPoolSender, mut io: &mut Io, mut need: usize)
{
    let watermark = sender.params.seed_cot_count(sender.malicious);
    while ((sender.remaining() < need) || (sender.remaining().saturating_sub(need) < watermark)){
    if sender.malicious{
    stack_refill_sender_malicious(rng, sender, io);
} else {
    stack_refill_sender(rng, sender, io);
};
}
}

pub fn ensure_receiver<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut receiver: &mut CotPoolReceiver, mut io: &mut Io, mut need: usize)
{
    let watermark = receiver.params.seed_cot_count(receiver.malicious);
    while ((receiver.remaining() < need) || (receiver.remaining().saturating_sub(need) < watermark)){
    if receiver.malicious{
} else {
    stack_refill_receiver(rng, receiver, io);
};
}
}

pub fn stack_bea95_sender<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut sender: &mut CotPoolSender, mut io: &mut Io) -> [u8; 16]
{
    ensure_sender(rng, sender, io, 1);
    let r0 = sender.out.pop_front().unwrap();
    let d = io.recv(TAG_BEA95)[0] != 0;
    if d{
    xor_block(&r0, &sender.seed.delta)
} else {
    r0
}
}

pub fn stack_bea95_receiver<R: SpecRng, Io: StackIo>(mut rng: &mut R, mut receiver: &mut CotPoolReceiver, mut io: &mut Io, mut bit: bool) -> [u8; 16]
{
    ensure_receiver(rng, receiver, io, 1);
    let x = receiver.out_x.pop_front().unwrap();
    let z = receiver.out_z.pop_front().unwrap();
    let (_r0, zc, d) = bea95_chosen_bit(&[(0 as u8); 16], [(0 as u8); 16], x, z, bit);
    let _ = _r0;
    io.send(TAG_BEA95, &[(d as u8)]);
    zc
}

pub fn push_u32(mut buf: &mut Vec<u8>, mut x: u32)
{
    buf.extend_from_slice(&x.to_le_bytes());
}

pub fn take_u32(mut bytes: &[u8], mut off: &mut usize) -> u32
{
    let x = u32::from_le_bytes(bytes[*off..(*off + 4)].try_into().unwrap());
    *off += 4;
    x
}

pub fn push_block(mut buf: &mut Vec<u8>, mut b: &Block)
{
    buf.extend_from_slice(b);
}

pub fn take_block(mut bytes: &[u8], mut off: &mut usize) -> Block
{
    let mut b = [(0 as u8); 16];
    b.copy_from_slice(&bytes[*off..(*off + 16)]);
    *off += 16;
    b
}

pub fn encode_bools(mut bits: &[bool]) -> Vec<u8>
{
    let mut buf = Vec::with_capacity((4 + bits.len()));
    push_u32(&mut buf, (bits.len() as u32));
    buf.extend(bits.iter().map(|b| (b as u8)));
    buf
}

pub fn decode_bools(mut bytes: &[u8]) -> (Vec<bool>, usize)
{
    let mut off = 0 as usize;
    let n = take_u32(bytes, &mut off) as usize;
    let bits = bytes[off..(off + n)].iter().map(|b| (b != 0)).collect::<Vec<_>>();
    off += n;
    (bits, off)
}

pub fn encode_spcot(mut msg: &SpcotSenderMsg) -> Vec<u8>
{
    let mut buf = Vec::new();
    push_u32(&mut buf, (msg.ms.len() as u32));
    for pair in &msg.ms{
    push_block(&mut buf, &pair[0]);
    push_block(&mut buf, &pair[1]);
};
    push_block(&mut buf, &msg.c);
    push_u32(&mut buf, (msg.hash_v.len() as u32));
    buf.extend_from_slice(&msg.hash_v);
    buf
}

pub fn decode_spcot(mut bytes: &[u8]) -> (SpcotSenderMsg, usize)
{
    let mut off = 0 as usize;
    let h = take_u32(bytes, &mut off) as usize;
    let mut ms = Vec::with_capacity(h);
    for _ in 0.. h{
    let a = take_block(bytes, &mut off);
    let b = take_block(bytes, &mut off);
    ms.push([a, b]);
};
    let c = take_block(bytes, &mut off);
    let hv_len = take_u32(bytes, &mut off) as usize;
    let hash_v = bytes[off..(off + hv_len)].to_vec();
    off += hv_len;
    (SpcotSenderMsg { ms: ms, c: c, hash_v: hash_v }, off)
}

pub fn encode_mpcot_reg(mut msg: &MpcotRegSenderMsg) -> Vec<u8>
{
    let mut buf = Vec::new();
    push_u32(&mut buf, (msg.blocks.len() as u32));
    for b in &msg.blocks{
    let inner = encode_spcot(b);
    push_u32(&mut buf, (inner.len() as u32));
    buf.extend_from_slice(&inner);
};
    buf
}

pub fn decode_mpcot_reg(mut bytes: &[u8]) -> MpcotRegSenderMsg
{
    let mut off = 0 as usize;
    let n = take_u32(bytes, &mut off) as usize;
    let mut blocks = Vec::with_capacity(n);
    for _ in 0.. n{
    let len = take_u32(bytes, &mut off) as usize;
    let (msg, used) = decode_spcot(&bytes[off..(off + len)]);
    off += len;
    blocks.push(msg);
};
    MpcotRegSenderMsg { blocks: blocks }
}

pub fn encode_ferret_open(mut lpn_seed: &[u8; 16], mut choices: &[bool]) -> Vec<u8>
{
    let mut buf = Vec::new();
    buf.extend_from_slice(lpn_seed);
    buf.extend(encode_bools(choices));
    buf
}

pub fn decode_ferret_open(mut bytes: &[u8]) -> ([u8; 16], Vec<bool>)
{
    let mut seed = [(0 as u8); 16];
    seed.copy_from_slice(&bytes[..16]);
    let (choices, _) = decode_bools(&bytes[16..]);
    (seed, choices)
}

pub fn encode_lwe_crs(mut n: usize, mut crs: &LweOtCrsDyn) -> Vec<u8>
{
    let mut buf = Vec::with_capacity((4 + (4 * ((n * n) + n))));
    push_u32(&mut buf, (n as u32));
    for i in 0.. n{
    for j in 0.. n{
    buf.extend_from_slice(&crs.a[i][j].to_le_bytes());
};
};
    for i in 0.. n{
    buf.extend_from_slice(&crs.h[i].to_le_bytes());
};
    buf
}

pub fn decode_lwe_crs(mut n: usize, mut bytes: &[u8]) -> LweOtCrsDyn
{
    let mut off = 0 as usize;
    let n = take_u32(bytes, &mut off) as usize;
    let mut a = [[(0 as u32); n]; n];
    for i in 0.. n{
    for j in 0.. n{
    a[i][j] = u32::from_le_bytes(bytes[off..(off + 4)].try_into().unwrap());
    off += 4;
};
};
    let mut h = [(0 as u32); n];
    for i in 0.. n{
    h[i] = u32::from_le_bytes(bytes[off..(off + 4)].try_into().unwrap());
    off += 4;
};
    LweOtCrsDyn { a: a, h: h, n: 0 }
}

pub fn encode_lwe_recv(mut n: usize, mut msg: &LweOtRecvMsgDyn) -> Vec<u8>
{
    let mut buf = Vec::with_capacity((4 * n));
    for i in 0.. n{
    buf.extend_from_slice(&msg.pk0[i].to_le_bytes());
};
    buf
}

pub fn decode_lwe_recv(mut n: usize, mut bytes: &[u8]) -> LweOtRecvMsgDyn
{
    let mut pk0 = [(0 as u32); n];
    for i in 0.. n{
    pk0[i] = u32::from_le_bytes(bytes[(i * 4)..((i * 4) + 4)].try_into().unwrap());
};
    LweOtRecvMsgDyn { pk0: pk0, n: 0 }
}

pub fn encode_zq_vec(mut v: &[Zq]) -> Vec<u8>
{
    let mut buf = Vec::new();
    push_u32(&mut buf, (v.len() as u32));
    for x in v{
    buf.extend_from_slice(&x.to_le_bytes());
};
    buf
}

pub fn decode_zq_vec(mut bytes: &[u8], mut off: &mut usize) -> Vec<Zq>
{
    let n = take_u32(bytes, off) as usize;
    let mut v = Vec::with_capacity(n);
    for _ in 0.. n{
    v.push(u32::from_le_bytes(bytes[*off..(*off + 4)].try_into().unwrap()));
    *off += 4;
};
    v
}

pub fn encode_lwe_payload(mut msg: &LweOtSenderMsgDyn) -> Vec<u8>
{
    let mut buf = Vec::new();
    buf.extend(encode_zq_vec(&msg.u0));
    buf.extend(encode_zq_vec(&msg.v0));
    buf.extend(encode_zq_vec(&msg.u1));
    buf.extend(encode_zq_vec(&msg.v1));
    buf
}

pub fn decode_lwe_payload(mut bytes: &[u8]) -> LweOtSenderMsgDyn
{
    let mut off = 0 as usize;
    let u0 = decode_zq_vec(bytes, &mut off);
    let v0 = decode_zq_vec(bytes, &mut off);
    let u1 = decode_zq_vec(bytes, &mut off);
    let v1 = decode_zq_vec(bytes, &mut off);
    LweOtSenderMsgDyn { u0: u0, v0: v0, u1: u1, v1: v1 }
}

pub fn encode_iknp_u(mut msg: &IknpUMsg) -> Vec<u8>
{
    let mut buf = Vec::new();
    push_u32(&mut buf, (msg.u_cols.len() as u32));
    for col in &msg.u_cols{
    buf.extend(encode_bools(col));
};
    buf
}

pub fn decode_iknp_u(mut bytes: &[u8]) -> IknpUMsg
{
    let mut off = 0 as usize;
    let n = take_u32(bytes, &mut off) as usize;
    let mut u_cols = Vec::with_capacity(n);
    for _ in 0.. n{
    let (col, used) = decode_bools(&bytes[off..]);
    off += used;
    u_cols.push(col);
};
    IknpUMsg { u_cols: u_cols }
}

pub fn encode_iknp_corr(mut rows: &[[u8; IKNP_KAPPA_BYTES]]) -> Vec<u8>
{
    let mut buf = Vec::new();
    push_u32(&mut buf, (rows.len() as u32));
    for r in rows{
    buf.extend_from_slice(r);
};
    buf
}

pub fn decode_iknp_corr(mut bytes: &[u8]) -> Vec<[u8; IKNP_KAPPA_BYTES]>
{
    let mut off = 0 as usize;
    let n = take_u32(bytes, &mut off) as usize;
    let mut rows = Vec::with_capacity(n);
    for _ in 0.. n{
    let mut r = [(0 as u8); IKNP_KAPPA_BYTES];
    r.copy_from_slice(&bytes[off..(off + IKNP_KAPPA_BYTES)]);
    off += IKNP_KAPPA_BYTES;
    rows.push(r);
};
    rows
}

pub fn tfhe_trivial_zero(mut n_lwe: usize) -> LweCiphertextDyn
{
    LweCiphertextDyn { a: [(0 as u32); n_lwe], b: 0, n_lwe: 0 }
}

pub fn tfhe_trivial_one(mut n_lwe: usize) -> LweCiphertextDyn
{
    LweCiphertextDyn { a: [(0 as u32); n_lwe], b: Q4, n_lwe: 0 }
}

pub fn tfhe_trivial_encrypt(mut n_lwe: usize, mut b: bool) -> LweCiphertextDyn
{
    if b{
    tfhe_trivial_one(n_lwe)
} else {
    tfhe_trivial_zero(n_lwe)
}
}

pub fn tfhe_not(mut n_lwe: usize, mut a: LweCiphertextDyn) -> LweCiphertextDyn
{
    let mut out_a = [(0 as u32); n_lwe];
    for i in 0.. n_lwe{
    out_a[i] = a.a[i].wrapping_neg();
};
    LweCiphertextDyn { a: out_a, b: Q4.wrapping_sub(a.b), n_lwe: 0 }
}

pub fn tfhe_gate_bootstrapping_and(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut bs_bg_log: usize, mut ks_bg_log: usize, mut ct_a: LweCiphertextDyn, mut ct_b: LweCiphertextDyn, mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let mut ct = lwe_add(n_lwe, ct_a, ct_b);
    ct.b = ct.b.wrapping_sub((Q4 >> 1));
    let acc = blind_rotate(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, &ct, bk);
    let lwe_big = sample_extract(big_n, &acc);
    let mut ct_out = key_switch(n_lwe, big_n, ks_ell, ks_bg_log, &lwe_big, &bk.ksk);
    ct_out.b = ct_out.b.wrapping_add((Q4 >> 1));
    ct_out
}

pub fn tfhe_gate_bootstrapping_or(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut bs_bg_log: usize, mut ks_bg_log: usize, mut ct_a: LweCiphertextDyn, mut ct_b: LweCiphertextDyn, mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let mut ct = lwe_add(n_lwe, ct_a, ct_b);
    ct.b = ct.b.wrapping_add((Q4 >> 1));
    let acc = blind_rotate(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, &ct, bk);
    let lwe_big = sample_extract(big_n, &acc);
    let mut ct_out = key_switch(n_lwe, big_n, ks_ell, ks_bg_log, &lwe_big, &bk.ksk);
    ct_out.b = ct_out.b.wrapping_add((Q4 >> 1));
    ct_out
}

pub fn tfhe_cmux(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut bs_bg_log: usize, mut ks_bg_log: usize, mut sel: LweCiphertextDyn, mut a: LweCiphertextDyn, mut b: LweCiphertextDyn, mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let not_sel = tfhe_not(n_lwe, sel);
    let sel_and_a = tfhe_gate_bootstrapping_and(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, sel, a, bk);
    let nsel_and_b = tfhe_gate_bootstrapping_and(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, not_sel, b, bk);
    tfhe_gate_bootstrapping_or(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, sel_and_a, nsel_and_b, bk)
}

pub fn tfhe_programmable_bootstrap(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut bs_bg_log: usize, mut ks_bg_log: usize, mut ct: LweCiphertextDyn, mut test_poly: [u32; BIG_N], mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    let acc = blind_rotate_with_poly(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, &ct, test_poly, bk);
    let lwe_big = sample_extract(big_n, &acc);
    key_switch(n_lwe, big_n, ks_ell, ks_bg_log, &lwe_big, &bk.ksk)
}

pub fn tfhe_lut_read(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut bs_bg_log: usize, mut ks_bg_log: usize, mut addr_bits: usize, mut table_len: usize, mut encrypted_addr_bits: &[LweCiphertextDyn; ADDR_BITS], mut table: &TfheBootstrapTableDyn, mut bk: &BootstrappingKeyDyn) -> LweCiphertextDyn
{
    if table.is_constant{
    return tfhe_trivial_encrypt(n_lwe, table.logical[0]);
};
    let delta = ((1 as u64) << 32) / (table_len as u64);
    let mut combined = LweCiphertextDyn { a: [(0 as u32); n_lwe], b: 0, n_lwe: 0 };
    for j in 0.. addr_bits{
    let addr_ct = &encrypted_addr_bits[j];
    let target = ((1 as u64) << j) * delta;
    for i in 0.. n_lwe{
    let scaled = (addr_ct.a[i] as u64).wrapping_mul(target) / (Q4 as u64);
    combined.a[i] = combined.a[i].wrapping_add((scaled as u32));
};
    let scaled_b = (addr_ct.b as u64).wrapping_mul(target) / (Q4 as u64);
    combined.b = combined.b.wrapping_add((scaled_b as u32));
};
    combined.b = combined.b.wrapping_add(((delta / 2) as u32));
    let mut ct_out = tfhe_programmable_bootstrap(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, combined, table.test_poly, bk);
    ct_out.b = ct_out.b.wrapping_add((Q4 >> 1));
    ct_out
}

pub fn lwe_encrypt<R: SpecRng>(mut n_lwe: usize, mut m: bool, mut sk: &LweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> LweCiphertextDyn
{
    let mut a = [(0 as u32); n_lwe];
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
    0 as u32
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
    let half = Q4 >> 1;
    let shifted = phase.wrapping_sub(half);
    shifted < Q4
}

pub fn gen_lwe_secret_key<R: SpecRng>(mut n_lwe: usize, mut rng: &mut R) -> LweSecretKeyDyn
{
    let mut key = [(0 as u8); n_lwe];
    for k in key.iter_mut(){
    *k = ((rng.next_u8() & 1) as u8);
};
    LweSecretKeyDyn { key: key, n_lwe: 0 }
}

pub fn gen_rlwe_secret_key<R: SpecRng>(mut big_n: usize, mut rng: &mut R) -> RlweSecretKeyDyn
{
    let mut key = [(0 as u32); big_n];
    for k in key.iter_mut(){
    *k = ((rng.next_u8() & 1) as u32);
};
    RlweSecretKeyDyn { key: key, big_n: 0 }
}

pub fn gen_bootstrapping_key<R: SpecRng>(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut bs_bg_log: usize, mut ks_bg_log: usize, mut lwe_sk: &LweSecretKeyDyn, mut rlwe_sk: &RlweSecretKeyDyn, mut bs_noise_bits: u32, mut ks_noise_bits: u32, mut rng: &mut R) -> BootstrappingKeyDyn
{
    let bsk = (0..n_lwe).map(|i| {
    let bit = lwe_sk.key[i] != 0;
    rgsw_encrypt::<BIG_N, BS_ELL, BS_BG_LOG, _>(big_n, bs_ell, bs_bg_log, bit, rlwe_sk, bs_noise_bits, rng)
}).collect::<Vec<_>>();
    let ksk_array: [[LweCiphertextDyn; KS_ELL]; BIG_N] = (0..big_n).map(|i| {
    let s_bit = rlwe_sk.key[i];
    (0..ks_ell).map(|j| {
    let shift = (32 as u32).saturating_sub(((ks_bg_log * (j + 1)) as u32));
    let msg_val = s_bit.wrapping_shl(shift);
    lwe_encrypt_raw(n_lwe, msg_val, lwe_sk, ks_noise_bits, rng)
}).collect::<Vec<_>>()
}).collect::<Vec<_>>();
    let ksk = KeySwitchingKeyDyn { ksk: ksk_array, n_lwe: 0, big_n: 0, ks_ell: 0, ks_bg_log: 0 };
    BootstrappingKeyDyn { bsk: bsk, ksk: ksk, n_lwe: 0, big_n: 0, bs_ell: 0, ks_ell: 0, bs_bg_log: 0, ks_bg_log: 0 }
}

pub fn rlwe_encrypt_scalar<R: SpecRng>(mut big_n: usize, mut m: u32, mut sk: &RlweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> RlweCiphertextDyn
{
    let a: [u32; BIG_N] = (0..big_n).map(|_| rng.next_u32()).collect::<Vec<_>>();
    let mut b = poly_mul_neg(big_n, &a, &sk.key);
    b[0] = b[0].wrapping_add(small_noise(noise_bits, rng)).wrapping_add(m);
    RlweCiphertextDyn { a: a, b: b, big_n: 0 }
}

pub fn rlwe_encrypt_poly<R: SpecRng>(mut big_n: usize, mut msg_poly: &[u32; BIG_N], mut sk: &RlweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> RlweCiphertextDyn
{
    let a: [u32; BIG_N] = (0..big_n).map(|_| rng.next_u32()).collect::<Vec<_>>();
    let mut b = poly_mul_neg(big_n, &a, &sk.key);
    for i in 0.. big_n{
    b[i] = b[i].wrapping_add(small_noise(noise_bits, rng)).wrapping_add(msg_poly[i]);
};
    RlweCiphertextDyn { a: a, b: b, big_n: 0 }
}

pub fn rgsw_encrypt<R: SpecRng>(mut big_n: usize, mut bs_ell: usize, mut bs_bg_log: usize, mut m: bool, mut sk: &RlweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> RgswCiphertextDyn
{
    let msg_bit = if m{
    1 as u32
} else {
    0 as u32
};
    let rows = (0..bs_ell).map(|j| {
    let shift = (32 as u32).saturating_sub(((bs_bg_log * (j + 1)) as u32));
    let g_factor = (1 as u32).wrapping_shl(shift);
    let contrib = msg_bit.wrapping_mul(g_factor);
    let mut rlwe0 = rlwe_encrypt_scalar(big_n, 0, sk, noise_bits, rng);
    rlwe0.a[0] = rlwe0.a[0].wrapping_add(contrib);
    let rlwe1 = rlwe_encrypt_scalar(big_n, contrib, sk, noise_bits, rng);
    RgswRowDyn { rlwe0: rlwe0, rlwe1: rlwe1, big_n: 0 }
}).collect::<Vec<_>>();
    RgswCiphertextDyn { rows: rows, big_n: 0, bs_ell: 0 }
}

pub fn blind_rotate_with_poly(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut bs_bg_log: usize, mut ks_bg_log: usize, mut ct: &LweCiphertextDyn, mut test_poly: [u32; BIG_N], mut bk: &BootstrappingKeyDyn) -> RlweCiphertextDyn
{
    let mut acc = RlweCiphertextDyn { a: [(0 as u32); big_n], b: test_poly, big_n: 0 };
    let two_n = 2 * big_n;
    let log2_two_n = two_n.trailing_zeros();
    let scale_shift = (32 as u32).saturating_sub(log2_two_n);
    let b_exp = torus_to_exp(ct.b, scale_shift, two_n);
    if b_exp != 0{
    acc = rlwe_rotate(big_n, &acc, (two_n - b_exp));
};
    for i in 0.. n_lwe{
    let a_exp = torus_to_exp(ct.a[i], scale_shift, two_n);
    if a_exp != 0{
    let acc_rotated = rlwe_rotate(big_n, &acc, a_exp);
    acc = cmux::<BIG_N, BS_ELL, BS_BG_LOG>(big_n, bs_ell, bs_bg_log, &bk.bsk[i], &acc_rotated, &acc);
};
};
    acc
}

pub fn and_test_poly(mut big_n: usize) -> [u32; BIG_N]
{
    let mut v = [(0 as u32); big_n];
    let half_q4 = Q4 >> 1;
    for k in 0.. big_n / 2{
    v[k] = half_q4.wrapping_neg();
};
    for k in big_n / 2.. big_n{
    v[k] = half_q4;
};
    v
}

pub fn blind_rotate(mut n_lwe: usize, mut big_n: usize, mut bs_ell: usize, mut ks_ell: usize, mut bs_bg_log: usize, mut ks_bg_log: usize, mut ct: &LweCiphertextDyn, mut bk: &BootstrappingKeyDyn) -> RlweCiphertextDyn
{
    blind_rotate_with_poly(n_lwe, big_n, bs_ell, ks_ell, bs_bg_log, ks_bg_log, ct, and_test_poly(big_n), bk)
}

pub fn torus_to_exp(mut x: u32, mut scale_shift: u32, mut two_n: usize) -> usize
{
    let half = if scale_shift > 0{
    (1 as u32) << (scale_shift - 1)
} else {
    0
};
    let exp = (x.wrapping_add(half) >> scale_shift) as usize;
    exp & (two_n - 1)
}

pub fn sample_extract(mut big_n: usize, mut rlwe: &RlweCiphertextDyn) -> LweCiphertextDyn
{
    let mut a_lwe = [(0 as u32); big_n];
    a_lwe[0] = rlwe.a[0];
    for i in 1.. big_n{
    a_lwe[i] = rlwe.a[big_n - i].wrapping_neg();
};
    LweCiphertextDyn { a: a_lwe, b: rlwe.b[0], n_lwe: 0 }
}

pub fn key_switch(mut n_lwe: usize, mut big_n: usize, mut ks_ell: usize, mut ks_bg_log: usize, mut ct_big: &LweCiphertextDyn, mut ksk: &KeySwitchingKeyDyn) -> LweCiphertextDyn
{
    let mut out_a = [(0 as u32); n_lwe];
    let mut out_b = ct_big.b;
    for i in 0.. big_n{
    let digits = ks_decompose::<KS_ELL, KS_BG_LOG>(ks_ell, ks_bg_log, ct_big.a[i]);
    for j in 0.. ks_ell{
    let d = digits[j] as u32;
    if d == 0{
    continue;
};
    let ksk_ct = &ksk.ksk[i][j];
    for k in 0.. n_lwe{
    out_a[k] = out_a[k].wrapping_sub(d.wrapping_mul(ksk_ct.a[k]));
};
    out_b = out_b.wrapping_sub(d.wrapping_mul(ksk_ct.b));
};
};
    LweCiphertextDyn { a: out_a, b: out_b, n_lwe: 0 }
}

pub fn ks_decompose(mut ks_ell: usize, mut ks_bg_log: usize, mut x: u32) -> [u32; KS_ELL]
{
    let bg = (1 as u64) << ks_bg_log;
    let mask = bg - 1;
    let mut rem = x as u64;
    let tail_shift = (32 as u32).saturating_sub(((ks_bg_log * ks_ell) as u32));
    if (tail_shift > 0) && (tail_shift < 32){
    let half_tail = (1 as u64) << (tail_shift - 1);
    rem = rem.wrapping_add(half_tail);
};
    let mut digits = [(0 as u32); ks_ell];
    for j in 0..ks_ell.rev(){
    let shift = (32 as u32).saturating_sub(((ks_bg_log * (j + 1)) as u32));
    if shift < 32{
    digits[j] = (((rem >> shift) & mask) as u32);
};
};
    digits
}

pub fn cmux(mut big_n: usize, mut bs_ell: usize, mut bs_bg_log: usize, mut c: &RgswCiphertextDyn, mut d1: &RlweCiphertextDyn, mut d0: &RlweCiphertextDyn) -> RlweCiphertextDyn
{
    let diff = rlwe_sub(big_n, d1, d0);
    let prod = external_product::<BIG_N, BS_ELL, BS_BG_LOG>(big_n, bs_ell, bs_bg_log, c, &diff);
    rlwe_add(big_n, d0, &prod)
}

pub fn external_product(mut big_n: usize, mut bs_ell: usize, mut bs_bg_log: usize, mut rgsw: &RgswCiphertextDyn, mut rlwe: &RlweCiphertextDyn) -> RlweCiphertextDyn
{
    let a_decomp = poly_decompose::<BIG_N, BS_ELL, BS_BG_LOG>(big_n, bs_ell, bs_bg_log, &rlwe.a);
    let b_decomp = poly_decompose::<BIG_N, BS_ELL, BS_BG_LOG>(big_n, bs_ell, bs_bg_log, &rlwe.b);
    let mut out_a = [(0 as u32); big_n];
    let mut out_b = [(0 as u32); big_n];
    for j in 0.. bs_ell{
    let row = &rgsw.rows[j];
    let prod_a0 = poly_mul_neg(big_n, &a_decomp[j], &row.rlwe0.a);
    let prod_a1 = poly_mul_neg(big_n, &a_decomp[j], &row.rlwe0.b);
    let prod_b0 = poly_mul_neg(big_n, &b_decomp[j], &row.rlwe1.a);
    let prod_b1 = poly_mul_neg(big_n, &b_decomp[j], &row.rlwe1.b);
    for k in 0.. big_n{
    out_a[k] = out_a[k].wrapping_add(prod_a0[k]).wrapping_add(prod_b0[k]);
    out_b[k] = out_b[k].wrapping_add(prod_a1[k]).wrapping_add(prod_b1[k]);
};
};
    RlweCiphertextDyn { a: out_a, b: out_b, big_n: 0 }
}

pub fn poly_decompose(mut big_n: usize, mut bs_ell: usize, mut bs_bg_log: usize, mut p: &[u32; BIG_N]) -> [[u32; BIG_N]; BS_ELL]
{
    let bg = (1 as u64) << bs_bg_log;
    let mask = (bg - 1) as u32;
    let mut result = [[(0 as u32); big_n]; bs_ell];
    for i in 0.. big_n{
    let x = p[i];
    let tail_bits = (32 as u32).saturating_sub(((bs_bg_log * bs_ell) as u32));
    let rounded = if (tail_bits > 0) && (tail_bits < 32){
    x.wrapping_add(((1 as u32) << (tail_bits - 1)))
} else {
    x
};
    for j in 0.. bs_ell{
    let shift = (32 as u32).saturating_sub(((bs_bg_log * (j + 1)) as u32));
    result[j][i] = if shift < 32{
    (rounded >> shift) & mask
} else {
    0
};
};
};
    result
}

pub fn rlwe_add(mut big_n: usize, mut a: &RlweCiphertextDyn, mut b: &RlweCiphertextDyn) -> RlweCiphertextDyn
{
    RlweCiphertextDyn { a: poly_add_neg(big_n, &a.a, &b.a), b: poly_add_neg(big_n, &a.b, &b.b), big_n: 0 }
}

pub fn rlwe_sub(mut big_n: usize, mut a: &RlweCiphertextDyn, mut b: &RlweCiphertextDyn) -> RlweCiphertextDyn
{
    RlweCiphertextDyn { a: poly_sub_neg(big_n, &a.a, &b.a), b: poly_sub_neg(big_n, &a.b, &b.b), big_n: 0 }
}

pub fn rlwe_rotate(mut big_n: usize, mut ct: &RlweCiphertextDyn, mut exp: usize) -> RlweCiphertextDyn
{
    RlweCiphertextDyn { a: poly_rotate(big_n, &ct.a, exp), b: poly_rotate(big_n, &ct.b, exp), big_n: 0 }
}

pub fn lwe_add(mut n_lwe: usize, mut a: LweCiphertextDyn, mut b: LweCiphertextDyn) -> LweCiphertextDyn
{
    let mut out_a = [(0 as u32); n_lwe];
    for i in 0.. n_lwe{
    out_a[i] = a.a[i].wrapping_add(b.a[i]);
};
    LweCiphertextDyn { a: out_a, b: a.b.wrapping_add(b.b), n_lwe: 0 }
}

pub fn lwe_encrypt_raw<R: SpecRng>(mut n_lwe: usize, mut msg: u32, mut sk: &LweSecretKeyDyn, mut noise_bits: u32, mut rng: &mut R) -> LweCiphertextDyn
{
    let mut a = [(0 as u32); n_lwe];
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
    let mut result = [(0 as u32); n];
    for i in 0.. n{
    for j in 0.. n{
    let deg = i + j;
    if deg < n{
    result[deg] = result[deg].wrapping_add(a[i].wrapping_mul(b[j]));
} else {
    result[deg - n] = result[deg - n].wrapping_sub(a[i].wrapping_mul(b[j]));
};
};
};
    result
}

pub fn poly_add_neg(mut n: usize, mut a: &[u32; N], mut b: &[u32; N]) -> [u32; N]
{
    let mut result = [(0 as u32); n];
    for i in 0.. n{
    result[i] = a[i].wrapping_add(b[i]);
};
    result
}

pub fn poly_sub_neg(mut n: usize, mut a: &[u32; N], mut b: &[u32; N]) -> [u32; N]
{
    let mut result = [(0 as u32); n];
    for i in 0.. n{
    result[i] = a[i].wrapping_sub(b[i]);
};
    result
}

pub fn poly_rotate(mut n: usize, mut p: &[u32; N], mut exp: usize) -> [u32; N]
{
    let exp = exp % (2 * n);
    if exp == 0{
    return *p;
};
    let mut result = [(0 as u32); n];
    for i in 0.. n{
    let new_pos = i + exp;
    if new_pos < n{
    result[new_pos] = result[new_pos].wrapping_add(p[i]);
} else if new_pos < (2 * n){
    result[new_pos - n] = result[new_pos - n].wrapping_sub(p[i]);
} else {
    result[new_pos - (2 * n)] = result[new_pos - (2 * n)].wrapping_add(p[i]);
};
};
    result
}

pub fn small_noise<R: SpecRng>(mut noise_bits: u32, mut rng: &mut R) -> u32
{
    if noise_bits >= 32{
    return rng.next_u32();
};
    let raw: u32 = rng.next_u32();
    let mask = ((1 as u32) << noise_bits).wrapping_sub(1);
    let small = raw & mask;
    if (noise_bits > 0) && ((small >> (noise_bits - 1)) != 0){
    small | !mask
} else {
    small
}
}

pub fn in_interval_mod(mut x: u64, mut lo: u64, mut hi: u64, mut q: u64) -> bool
{
    let x = x % q;
    let lo = lo % q;
    let hi = hi % q;
    if lo <= hi{
    (x >= lo) && (x < hi)
} else {
    (x >= lo) || (x < hi)
}
}

pub fn evaluate_certificate(mut cert: &GateCertificate, mut inputs: &[u64], mut q: u64) -> u64
{
    let prepared = cert.prepare(inputs, q) % q;
    let (lo, hi) = cert.interval_true;
    let signed_eighth: i64 = if in_interval_mod(prepared, lo, hi, q){
    (q / 8) as i64
} else {
    -((q / 8) as i64)
};
    let restored = signed_eighth + ((q / 8) as i64);
    restored.rem_euclid((q as i64)) as u64
}

pub fn true_val(mut q: u64) -> u64
{
    q / 4
}

pub fn evaluate_gate(mut cert_eighths: &GateCertificate, mut inputs: &[u64], mut q: u64) -> u64
{
    let scale = q / 8;
    let scaled = GateCertificate { name: cert_eighths.name, arity: cert_eighths.arity, prepare: cert_eighths.prepare, interval_true: (cert_eighths.interval_true.0.wrapping_mul(scale), cert_eighths.interval_true.1.wrapping_mul(scale)) };
    evaluate_certificate(&scaled, inputs, q)
}

pub fn cert_and() -> GateCertificate
{
    GateCertificate { name: "AND", arity: 2, prepare: |c, q| ((c[0] + c[1]) % q), interval_true: (3, 7) }
}

pub fn cert_nand() -> GateCertificate
{
    GateCertificate { name: "NAND", arity: 2, prepare: |c, q| ((c[0] + c[1]) % q), interval_true: (7, 3) }
}

pub fn cert_or() -> GateCertificate
{
    GateCertificate { name: "OR", arity: 2, prepare: |c, q| ((c[0] + c[1]) % q), interval_true: (1, 5) }
}

pub fn cert_nor() -> GateCertificate
{
    GateCertificate { name: "NOR", arity: 2, prepare: |c, q| ((c[0] + c[1]) % q), interval_true: (5, 1) }
}

pub fn cert_xor() -> GateCertificate
{
    GateCertificate { name: "XOR", arity: 2, prepare: |c, q| {
    let diff = ((c[0] as i64) - (c[1] as i64)).rem_euclid((q as i64)) as u64;
    (2 * diff) % q
}, interval_true: (1, 5) }
}

pub fn cert_xnor() -> GateCertificate
{
    GateCertificate { name: "XNOR", arity: 2, prepare: |c, q| {
    let diff = ((c[0] as i64) - (c[1] as i64)).rem_euclid((q as i64)) as u64;
    (2 * diff) % q
}, interval_true: (5, 1) }
}

pub fn cert_majority() -> GateCertificate
{
    GateCertificate { name: "Majority", arity: 3, prepare: |c, q| (((c[0] + c[1]) + c[2]) % q), interval_true: (3, 7) }
}

pub fn eval_not(mut c: u64, mut q: u64) -> u64
{
    let true_v = q / 4;
    ((true_v as i64) - (c as i64)).rem_euclid((q as i64)) as u64
}

pub fn to_bool(mut phase: u64, mut q: u64) -> bool
{
    phase == (q / 4)
}

pub fn from_bool(mut b: bool, mut q: u64) -> u64
{
    if b{
    true_val(q)
} else {
    FALSE
}
}

pub fn encode_label_16(mut label: [u8; 16]) -> [u64; 3]
{
    let first = u64::from_le_bytes([label[0], label[1], label[2], label[3], label[4], label[5], 0, 0]);
    let second = u64::from_le_bytes([label[6], label[7], label[8], label[9], label[10], label[11], 0, 0]);
    let third = u32::from_le_bytes([label[12], label[13], label[14], label[15]]) as u64;
    [first, second, third]
}

pub fn decode_label_16(mut elements: [u64; 3]) -> Result<[u8; 16], LabelEncodingError>
{
    let WIDTHS: [u32; 3] = [48, 48, 32];
    let modulus = ring_lwe::REFERENCE_PLAINTEXT_MODULUS;
    let mut label = [(0 as u8); 16];
    let mut offset = 0 as usize;
    for (index, (.., ..)) in elements.iter().zip(WIDTHS.iter()).enumerate(){
    if element >= modulus{
    return Err(LabelEncodingError::FieldElementOutOfRange { index: index });
};
    if element >= ((1 as u64) << width){
    return Err(LabelEncodingError::NonCanonicalElement { index: index });
};
    let bytes = element.to_le_bytes();
    let count = (width / 8) as usize;
    label[offset..(offset + count)].copy_from_slice(&bytes[..count]);
    offset += count;
};
    Ok(label)
}

pub fn inner_product(mut ring: &Ring, mut left: &[Polynomial], mut right: &[Polynomial]) -> Result<Polynomial, Error>
{
    if (left.len() != right.len()) || left.iter().chain(right.iter()).any(|value| (value.degree() != ring.parameters.degree)){
    return Err(Error::LengthMismatch);
};
    let mut output = ring.zero();
    for (left, right) in left.iter().zip(right.iter()){
    output.add_assign(ring, &left.product(ring, right));
};
    Ok(output)
}

pub fn bit_reverse(mut values: &mut [u64])
{
    let bits = values.len().ilog2();
    for index in 0.. values.len(){
    let reversed = index.reverse_bits() >> (usize::BITS - bits);
    if index < reversed{
    values.swap(index, reversed);
};
}
}

pub fn find_negacyclic_root(mut degree: usize, mut modulus: u64) -> Option<u64>
{
    let exponent = (modulus - 1) / (2 * (degree as u64));
    for candidate in 2.. modulus{
    let root = pow_mod(candidate, (exponent as usize), modulus);
    if pow_mod(root, degree, modulus) == (modulus - 1){
    return Some(root);
};
};
    None
}

pub fn sample_uniform(mut random: &mut impl RandomSource, mut modulus: u64, mut output: &mut [u64]) -> Result<(), Error>
{
    let threshold = u64::MAX - (u64::MAX % modulus);
    for value in output{
    while true{
    let mut bytes = [(0 as u8); 8];
    random.fill_bytes(&mut bytes)?;
    let candidate = u64::from_le_bytes(bytes);
    if candidate < threshold{
    *value = (candidate % modulus);
    break;
};
};
};
    Ok(())
}

pub fn signed_to_mod(mut value: i64, mut modulus: u64) -> u64
{
    if value < 0{
    let magnitude = value.unsigned_abs() % modulus;
    if magnitude == 0{
    0
} else {
    modulus - magnitude
}
} else {
    (value as u64) % modulus
}
}

pub fn add_mod(mut left: u64, mut right: u64, mut modulus: u64) -> u64
{
    let sum = left + right;
    if sum >= modulus{
    sum - modulus
} else {
    sum
}
}

pub fn sub_mod(mut left: u64, mut right: u64, mut modulus: u64) -> u64
{
    if left >= right{
    left - right
} else {
    modulus - (right - left)
}
}

pub fn mul_mod(mut left: u64, mut right: u64, mut modulus: u64) -> u64
{
    (((left as u128) * (right as u128)) % (modulus as u128)) as u64
}

pub fn pow_mod(mut value: u64, mut exponent: usize, mut modulus: u64) -> u64
{
    let mut result = 1;
    while (exponent != 0){
    if (exponent & 1) != 0{
    result = mul_mod(result, value, modulus);
};
    value = mul_mod(value, value, modulus);
    exponent >>= 1;
};
    result
}

pub fn inverse_mod(mut value: u64, mut modulus: u64) -> Option<u64>
{
    if (value == 0) || (modulus < 2){
    return None;
};
    let mut old_r = value as i128;
    let mut r = modulus as i128;
    let mut old_s = 1 as i128;
    let mut s = 0 as i128;
    while (r != 0){
    let quotient = old_r / r;
    (old_r, r) = (r, (old_r - (quotient * r)));
    (old_s, s) = (s, (old_s - (quotient * s)));
};
    if old_r != 1{
    return None;
};
    Some((old_s.rem_euclid((modulus as i128)) as u64))
}

pub fn vole_rekey_prover<N, T>(mut n: usize, mut wire: VopeDyn<T>, mut key: VopeDyn<T>) -> VopeDyn<T> where T: Clone + Add<Output = T> + Default, VopeDyn<T>: Add<Output = Vope<N, T, U1>>
{
    wire + key
}

pub fn vole_rekey_verifier_check<N, T>(mut n: usize, mut q_wire: &QDyn<T>, mut q_key: &QDyn<T>, mut q_rekeyed: &QDyn<T>) -> bool where T: Clone + Add<Output = T> + PartialEq
{
    let mut ok = true;
    for i in 0.. n{
    let expect = q_wire.q[i].clone() + q_key.q[i].clone();
    ok = (ok && (q_rekeyed.q[i].clone() == expect));
};
    ok
}

pub fn mem_acc_absorb<T>(mut acc: T, mut r0: T, mut r1: T, mut r2: T, mut r3: T, mut addr: T, mut value: T, mut ts: T) -> T where T: Clone + Add<Output = T> + Mul<Output = T>
{
    acc + r0 + (addr * r1) + (value * r2) + (ts * r3)
}

pub fn vope_scale_const<N, T>(mut n: usize, mut w: &VopeDyn<T>, mut c: &T) -> VopeDyn<T> where T: Clone + Mul<Output = T> + Default
{
    VopeDyn { u: (0..1).map(|_| {
    (0..n).map(|i| (w.u[0][i].clone() * c.clone())).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: (0..n).map(|i| (w.v[i].clone() * c.clone())).collect::<Vec<T>>(), n: 0, k: 1 }
}

pub fn mem_acc_absorb_vope<N, T>(mut n: usize, mut acc: VopeDyn<T>, mut one: &VopeDyn<T>, mut addr: &VopeDyn<T>, mut value: &VopeDyn<T>, mut ts: &VopeDyn<T>, mut r0: &T, mut r1: &T, mut r2: &T, mut r3: &T) -> VopeDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T> + Default, VopeDyn<T>: Add<Output = Vope<N, T, U1>>
{
    acc + vope_scale_const(n, one, r0) + vope_scale_const(n, addr, r1) + vope_scale_const(n, value, r2) + vope_scale_const(n, ts, r3)
}

pub fn vope_bitpack<N, T>(mut bits: usize, mut n: usize, mut bit_values: &[VopeDyn<T>; BITS], mut pow2: &[T; BITS]) -> VopeDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T> + Default, VopeDyn<T>: Add<Output = Vope<N, T, U1>>
{
    let mut acc = VopeDyn { u: (0..1).map(|_| (0..n).map(|_| T::default()).collect::<Vec<T>>()).collect::<Vec<Vec<T>>>(), v: (0..n).map(|_| T::default()).collect::<Vec<T>>(), n: 0, k: 1 };
    for index in 0.. bits{
    acc = (acc + vope_scale_const(n, &bit_values[index], &pow2[index]));
};
    acc
}

pub fn q_scale_const<N, T>(mut n: usize, mut q: &QDyn<T>, mut c: &T) -> QDyn<T> where T: Clone + Mul<Output = T>
{
    QDyn { q: (0..n).map(|i| (q.q[i].clone() * c.clone())).collect::<Vec<T>>(), n: 0 }
}

pub fn mem_acc_absorb_q<N, T>(mut n: usize, mut acc: QDyn<T>, mut one: &QDyn<T>, mut addr: &QDyn<T>, mut value: &QDyn<T>, mut ts: &QDyn<T>, mut r0: &T, mut r1: &T, mut r2: &T, mut r3: &T) -> QDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T>
{
    let c = q_scale_const(n, one, r0);
    let a = q_scale_const(n, addr, r1);
    let v = q_scale_const(n, value, r2);
    let t = q_scale_const(n, ts, r3);
    QDyn { q: (0..n).map(|i| {
    acc.q[i].clone() + c.q[i].clone() + a.q[i].clone() + v.q[i].clone() + t.q[i].clone()
}).collect::<Vec<T>>(), n: 0 }
}

pub fn q_bitpack<N, T>(mut bits: usize, mut n: usize, mut bit_values: &[QDyn<T>; BITS], mut pow2: &[T; BITS]) -> QDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T> + Default
{
    let mut acc = QDyn { q: (0..n).map(|_| T::default()).collect::<Vec<T>>(), n: 0 };
    for index in 0.. bits{
    let scaled = q_scale_const(n, &bit_values[index], &pow2[index]);
    acc = QDyn { q: (0..n).map(|i| (acc.q[i].clone() + scaled.q[i].clone())).collect::<Vec<T>>(), n: 0 };
};
    acc
}

pub fn mem_drain_open<N, T>(mut n: usize, mut prod: &VopeDyn<T>, mut cons: &VopeDyn<T>) -> Vec<T> where T: Clone + Add<Output = T>
{
    (0..n).map(|i| (prod.v[i].clone() + cons.v[i].clone())).collect::<Vec<T>>()
}

pub fn mem_drain_check<N, T>(mut n: usize, mut prod_q: &QDyn<T>, mut cons_q: &QDyn<T>, mut opening: &Vec<T>) -> bool where T: Clone + Add<Output = T> + PartialEq
{
    let mut ok = true;
    for i in 0.. n{
    let k_diff = prod_q.q[i].clone() + cons_q.q[i].clone();
    ok = (ok && (k_diff == opening[i].clone()));
};
    ok
}

pub fn vope_open_mask<N, T>(mut n: usize, mut w: &VopeDyn<T>) -> Vec<T> where T: Clone
{
    w.v.clone()
}

pub fn assert_one_check<N, T>(mut n: usize, mut q: &QDyn<T>, mut opening: &Vec<T>, mut delta: &DeltaLoweredDyn<T>) -> bool where T: Clone + Add<Output = T> + PartialEq
{
    let mut ok = true;
    for i in 0.. n{
    ok = (ok && ((q.q[i].clone() + opening[i].clone()) == delta.delta[i].clone()));
};
    ok
}

pub fn memory_check_per_lane<N, T>(mut n: usize, mut challenges: Vec<T>) -> Vec<MemoryCheckStateDyn<T, AdditiveHasher>> where T: Clone + Default + Add<Output = T> + Mul<Output = T> + PartialEq
{
    (0..n).map(|i| {
    let key = ChallengeKeyDyn::from_challenge(challenges[i].clone());
    MemoryCheckStateDyn::new(key)
}).collect::<Vec<_>>()
}

pub fn vole_and_prover_step<N, T>(mut n: usize, mut vope_a: VopeDyn<T>, mut vope_b: VopeDyn<T>) -> (VopeDyn<T>, Vec<T>) where T: Clone + Add<Output = T> + Mul<Output = T> + Default
{
    let u_c_inner = (0..n).map(|i| (vope_a.u[0][i].clone() * vope_b.u[0][i].clone())).collect::<Vec<T>>();
    let u_c = (0..1).map(|_| u_c_inner.clone()).collect::<Vec<Vec<T>>>();
    let v_c = (0..n).map(|i| {
    (vope_a.v[i].clone() * vope_b.u[0][i].clone()) + (vope_b.v[i].clone() * vope_a.u[0][i].clone())
}).collect::<Vec<T>>();
    let hat = (0..n).map(|i| (vope_a.v[i].clone() * vope_b.v[i].clone())).collect::<Vec<T>>();
    (VopeDyn { u: u_c, v: v_c, n: 0, k: 1 }, hat)
}

pub fn vole_and_verifier_check<N, T>(mut n: usize, mut delta: &DeltaLoweredDyn<T>, mut q_a: &QDyn<T>, mut q_b: &QDyn<T>, mut q_and: &QDyn<T>, mut hat: &Vec<T>) -> (QDyn<T>, bool) where T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default
{
    let mut ok = true;
    for i in 0.. n{
    let lhs = (q_a.q[i].clone() * q_b.q[i].clone()) + hat[i].clone();
    let rhs = q_and.q[i].clone() * delta.delta[i].clone();
    ok = (ok && (lhs == rhs));
};
    (QDyn { q: q_and.q.clone(), n: 0 }, ok)
}

pub fn vole_sbox_prover_step<N, T>(mut n: usize, mut vope_a: VopeDyn<T>, mut vope_b: VopeDyn<T>) -> (VopeDyn<T>, VopeDyn<T>) where T: Add<Output = T> + Mul<Output = T> + Default + Clone
{
    let k2: VopeDyn<T> = vope_a.mul_generalized(&vope_b.k, &vope_b);
    let k1 = VopeDyn { u: (0..1).map(|_| k2.u[1].clone()).collect::<Vec<Vec<T>>>(), v: k2.u[0].clone(), n: 0, k: 1 };
    (k1, k2)
}

pub fn vole_sbox_verifier_check<N, T>(mut n: usize, mut delta: &DeltaLoweredDyn<T>, mut q_a: &QDyn<T>, mut q_b: &QDyn<T>, mut vope_k2: VopeDyn<T>) -> (QDyn<T>, bool) where T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default + Into<T>
{
    let q_c = vope_k2 * delta.clone();
    let mut ok = true;
    for i in 0.. n{
    ok = (ok && ((q_a.q[i].clone() * q_b.q[i].clone()) == q_c.q[i]));
};
    (q_c, ok)
}

pub fn vole_mul3_prover_step<N, T>(mut n: usize, mut vope_a: &VopeDyn<T>, mut vope_b: &VopeDyn<T>, mut vope_d: &VopeDyn<T>) -> VopeDyn<T> where T: Add<Output = T> + Mul<Output = T> + Default + Clone
{
    let ab: VopeDyn<T> = vope_a.mul_generalized(vope_b.k, vope_b);
    ab.mul_generalized(vope_d.k, vope_d)
}

pub fn vole_mul3_verifier_check<N, T>(mut n: usize, mut delta: &DeltaLoweredDyn<T>, mut q_a: &QDyn<T>, mut q_b: &QDyn<T>, mut q_d: &QDyn<T>, mut vope_abd: VopeDyn<T>) -> (QDyn<T>, bool) where T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default + Into<T>
{
    let q_abd = vope_abd * delta.clone();
    let mut ok = true;
    for i in 0.. n{
    let lhs = q_a.q[i].clone() * q_b.q[i].clone() * q_d.q[i].clone();
    ok = (ok && (lhs == q_abd.q[i]));
};
    (q_abd, ok)
}

pub fn random_nonzero_delta<N, T, R>(mut n: usize, mut rng: &mut R, mut sample_t: impl Fn, mut is_zero: impl Fn) -> DeltaLoweredDyn<T>
{
    DeltaLoweredDyn { delta: (0..n).map(|_| {
    let mut x = sample_t(rng);
    let mut tries = 0 as usize;
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
    (vole_commit_bit_prover_share(n, v, bit_to_t, bit), vole_commit_bit_verifier_share(n, r0))
}

pub fn vole_commit_bit_prover_share<N, T>(mut n: usize, mut v: Vec<T>, mut bit_to_t: impl FnMut(bool) -> T, mut bit: bool) -> VopeDyn<T> where T: Clone + Default
{
    let u_t = bit_to_t(bit);
    let u_row: Vec<T> = lift_bit(n, u_t);
    let u: Vec<Vec<T>> = (0..1).map(|_| (0..n).map(|i| u_row[i].clone()).collect::<Vec<T>>()).collect::<Vec<Vec<T>>>();
    VopeDyn { u: u, v: v, n: 0, k: 1 }
}

pub fn vole_commit_bit_verifier_share<N, T>(mut n: usize, mut q: Vec<T>) -> QDyn<T> where T: Clone + Default
{
    QDyn { q: q, n: 0 }
}

pub fn vole_commit_bit_shares<N, T>(mut n: usize, mut r0: Vec<T>, mut v: Vec<T>, mut bit_to_t: impl FnMut(bool) -> T, mut bit: bool) -> (VopeDyn<T>, QDyn<T>) where T: Clone + Default
{
    (vole_commit_bit_prover_share(n, v, bit_to_t, bit), vole_commit_bit_verifier_share(n, r0))
}

pub fn vole_commit_bit_from<N, T, R, C>(mut n: usize, mut cot: &mut C, mut rng: &mut R, mut sample_t: impl Fn, mut bit_to_t: impl FnMut(bool) -> T, mut bit: bool) -> (VopeDyn<T>, QDyn<T>) where T: Clone + Add<Output = T> + Mul<Output = T> + Default
{
    let (r0, v) = cot.cot(rng, sample_t, bit);
    vole_commit_bit_shares(n, r0, v, bit_to_t, bit)
}

pub fn derive_and_q<N, T>(mut n: usize, mut delta: &DeltaLoweredDyn<T>, mut q_a: &QDyn<T>, mut q_b: &QDyn<T>, mut hat: &Vec<T>) -> QDyn<T> where T: Clone + Add<Output = T> + Mul<Output = T> + Invert + Default
{
    QDyn { q: (0..n).map(|i| {
    let lhs = (q_a.q[i].clone() * q_b.q[i].clone()) + hat[i].clone();
    lhs * delta.delta[i].invert()
}).collect::<Vec<T>>(), n: 0 }
}

pub fn debug_check_pool_written(mut written: bool, mut slot: usize)
{
    if !written{
}
}

pub fn is_pow2(mut n: usize) -> bool
{
    (n != 0) && n.is_power_of_two()
}

pub fn log2_pow2(mut n: usize) -> usize
{
    n.trailing_zeros() as usize
}

pub fn le16(mut v: Galois128) -> Vec<u8>
{
    let b = v.0.to_le_bytes();
    (0..16).map(|i| b[i]).collect::<Vec<u8>>()
}

pub fn warp_transcript_new() -> WarpTranscript
{
    WarpTranscript { sponge: spec_sponge_new(SHAKE128_RATE) }
}

pub fn warp_hash_leaf_field(mut v: Galois128) -> WarpDigest
{
    let mut h = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb_array(&mut h, &(0..1).map(|_| 0).collect::<Vec<u8>>());
    let v_bytes: Vec<u8> = le16(v);
    spec_sponge_absorb_array(&mut h, &v_bytes);
    let mut reader = spec_sponge_finalize_xof(&h, 6);
    let mut out: Vec<u8> = (0..32).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read_array(&mut reader, &mut out);
    out
}

pub fn warp_hash_node(mut left: &WarpDigest, mut right: &WarpDigest) -> WarpDigest
{
    let mut h = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb_array(&mut h, &(0..1).map(|_| 1).collect::<Vec<u8>>());
    spec_sponge_absorb_array(&mut h, left);
    spec_sponge_absorb_array(&mut h, right);
    let mut reader = spec_sponge_finalize_xof(&h, 6);
    let mut out: Vec<u8> = (0..32).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read_array(&mut reader, &mut out);
    out
}

pub fn warp_merkle_commit_leaves(mut nodes: usize, mut leaves: usize, mut leaves: &Vec<WarpDigest>) -> WarpMerkleTreeDyn
{
    let mut nodes = (0..nodes).map(|_| <WarpDigest>::default()).collect::<Vec<WarpDigest>>();
    for i in 0.. leaves{
    nodes[i] = leaves[i].clone();
};
    let mut level_start = 0 as usize;
    let mut level_len = leaves;
    for _level in 0.. leaves{
    if level_len > 1{
    let next_start = level_start + level_len;
    for j in 0.. level_len / 2{
    nodes[next_start + j] = warp_hash_node(&nodes[level_start + (2 * j)], &nodes[level_start + (2 * j) + 1]);
};
    level_start = next_start;
    level_len /= 2;
};
};
    WarpMerkleTreeDyn { leaves: leaves, nodes: nodes }
}

pub fn warp_merkle_root(mut nodes: usize, mut tree: &WarpMerkleTreeDyn) -> WarpDigest
{
    tree.nodes[nodes - 1].clone()
}

pub fn warp_merkle_open(mut nodes: usize, mut log2: usize, mut tree: &WarpMerkleTreeDyn, mut index: usize) -> WarpAuthPathDyn
{
    let mut siblings = (0..log2).map(|_| <WarpDigest>::default()).collect::<Vec<WarpDigest>>();
    let mut idx = index;
    let mut level_start = 0 as usize;
    let mut level_len = tree.leaves;
    for depth in 0.. log2{
    siblings[depth] = tree.nodes[level_start + (idx ^ 1)].clone();
    idx /= 2;
    level_start += level_len;
    level_len /= 2;
};
    WarpAuthPathDyn { siblings: siblings, log2_leaves: 0 }
}

pub fn warp_merkle_verify_hashed(mut log2_leaves: usize, mut root: &WarpDigest, mut leaf_hash: &WarpDigest, mut index: usize, mut path: &WarpAuthPathDyn) -> bool
{
    let mut cur = leaf_hash.clone();
    let mut idx = index;
    for i in 0.. log2_leaves{
    let sibling = &path.siblings[i];
    cur = if (idx % 2) == 0{
    warp_hash_node(&cur, sibling)
} else {
    warp_hash_node(sibling, &cur)
};
    idx /= 2;
};
    cur == *root
}

pub fn warp_commit_codeword<N, NODES>(mut n: usize, mut nodes: usize, mut codeword: &Vec<Galois128>) -> WarpMerkleTreeDyn
{
    let leaves = (0..n).map(|i| warp_hash_leaf_field(codeword[i])).collect::<Vec<WarpDigest>>();
    warp_merkle_commit_leaves::<NODES, N>(nodes, n, &leaves)
}

pub fn warp_commit_stacked<N, FRESH, NODES>(mut n: usize, mut fresh: usize, mut nodes: usize, mut codewords: &Vec<Vec<Galois128>>) -> WarpMerkleTreeDyn
{
    let leaves = (0..n).map(|i| {
    let mut h = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb_array(n, &mut h, &(0..1).map(|_| 0).collect::<Vec<u8>>());
    for j in 0.. fresh{
    let elem_bytes: Vec<u8> = le16(codewords[j][i]);
    spec_sponge_absorb_array(n, &mut h, &elem_bytes);
};
    let mut reader = spec_sponge_finalize_xof(&h, 6);
    let mut out: Vec<u8> = (0..32).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read_array(n, &mut reader, &mut out);
    out
}).collect::<Vec<WarpDigest>>();
    warp_merkle_commit_leaves::<NODES, N>(nodes, n, &leaves)
}

pub fn warp_eq_evals(mut points: usize, mut out: usize, mut point: &Vec<Galois128>) -> Vec<Galois128>
{
    let mut out = (0..out).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    out[0] = Galois128(1);
    for c in 0.. points{
    let coordinate = point[c];
    for index in 0.. (1 as usize) << c{
    let product = out[index] * coordinate;
    out[index] = (out[index] - product);
    out[index + ((1 as usize) << c)] = product;
};
};
    out
}

pub fn warp_eq_scalar(mut l: usize, mut a: &Vec<Galois128>, mut b: &Vec<Galois128>) -> Galois128
{
    let mut out = Galois128(1);
    for i in 0.. l{
    let left = a[i];
    let right = b[i];
    let same = left * right;
    let different = (Galois128(1) - left) * (Galois128(1) - right);
    out = (out * (same + different));
};
    out
}

pub fn warp_binary_point(mut log: usize, mut index: usize) -> Vec<Galois128>
{
    (0..log).map(|bit| {
    if ((index >> bit) & 1) == 1{
    Galois128(1)
} else {
    Galois128(0)
}
}).collect::<Vec<_>>()
}

pub fn warp_mle_eval<N, LOGN>(mut n: usize, mut logn: usize, mut values: &Vec<Galois128>, mut point: &Vec<Galois128>) -> Galois128
{
    let eq = warp_eq_evals::<LOGN, N>(logn, n, point);
    let mut out = Galois128(0);
    for i in 0.. n{
    out = (out + (values[i] * eq[i]));
};
    out
}

pub fn warp_term_degree(mut deg: usize, mut term: &WarpTermDyn) -> usize
{
    term.n_vars as usize
}

pub fn warp_eval_term(mut deg: usize, mut x: usize, mut k: usize, mut term: &WarpTermDyn, mut public: &Vec<Galois128>, mut witness: &Vec<Galois128>) -> Galois128
{
    let mut out = term.coeff;
    for i in 0.. deg{
    if i < (term.n_vars as usize){
    let var = term.vars[i];
    let value = if var < x{
    public[var]
} else {
    witness[var - x]
};
    out = (out * value);
};
};
    out
}

pub fn warp_constraint_max_degree(mut terms: usize, mut deg: usize, mut constraint: &WarpConstraintDyn) -> usize
{
    let mut out = 0 as usize;
    for i in 0.. terms{
    if i < (constraint.n_terms as usize){
    let d = warp_term_degree(deg, &constraint.terms[i]);
    if d > out{
    out = d;
}
};
};
    out
}

pub fn warp_eval_constraint(mut terms: usize, mut deg: usize, mut x: usize, mut k: usize, mut constraint: &WarpConstraintDyn, mut public: &Vec<Galois128>, mut witness: &Vec<Galois128>) -> Galois128
{
    let mut out = Galois128(0);
    for i in 0.. terms{
    if i < (constraint.n_terms as usize){
    out = (out + warp_eval_term::<DEG, X, K>(deg, x, k, &constraint.terms[i], public, witness));
};
};
    out
}

pub fn warp_variable_count(mut m: usize, mut terms: usize, mut deg: usize, mut pesat: &WarpIndexDyn) -> usize
{
    pesat.public_len + pesat.witness_len
}

pub fn warp_log_constraints(mut m: usize, mut terms: usize, mut deg: usize, mut pesat: &WarpIndexDyn) -> usize
{
    log2_pow2(m)
}

pub fn warp_index_is_valid(mut m: usize, mut terms: usize, mut deg: usize, mut pesat: &WarpIndexDyn) -> bool
{
    if !is_pow2(m){
    return false;
};
    for c in 0.. m{
    let constraint = &pesat.constraints[c];
    if (constraint.n_terms as usize) > terms{
    return false;
};
    if warp_constraint_max_degree(terms, deg, constraint) > pesat.degree_bound{
    return false;
};
    for i in 0.. terms{
    if i < (constraint.n_terms as usize){
    let term = &constraint.terms[i];
    if (term.n_vars as usize) > deg{
    return false;
};
    for j in 0.. deg{
    if (j < (term.n_vars as usize)) && (term.vars[j] >= warp_variable_count(m, terms, deg, pesat)){
    return false;
};
}
};
};
};
    true
}

pub fn warp_evaluate_bundled(mut m: usize, mut terms: usize, mut deg: usize, mut k: usize, mut beta: usize, mut pesat: &WarpIndexDyn, mut beta: &Vec<Galois128>, mut witness: &Vec<Galois128>) -> Galois128
{
    let log_m = warp_log_constraints(m, terms, deg, pesat);
    let mut eq = (0..m).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    eq[0] = Galois128(1);
    for j in 0.. m{
    if ((1 as usize) << j) < m{
    let coordinate = beta[pesat.public_len + j];
    for index in 0.. (1 as usize) << j{
    let product = eq[index] * coordinate;
    eq[index] = (eq[index] - product);
    eq[index + ((1 as usize) << j)] = product;
}
};
};
    let mut out = Galois128(0);
    for index in 0.. m{
    let constraint = &pesat.constraints[index];
    let mut value = Galois128(0);
    for i in 0.. constraint.n_terms as usize{
    let term = &constraint.terms[i];
    let mut product = term.coeff;
    for v in 0.. term.n_vars as usize{
    let var = term.vars[v];
    let x = if var < pesat.public_len{
    beta[var]
} else {
    witness[var - pesat.public_len]
};
    product = (product * x);
};
    value = (value + product);
};
    out = (out + (eq[index] * value));
};
    out
}

pub fn warp_is_instance_satisfied(mut m: usize, mut terms: usize, mut deg: usize, mut k: usize, mut x: usize, mut pesat: &WarpIndexDyn, mut instance: &WarpWitnessDyn) -> bool
{
    for index in 0.. m{
    let constraint = &pesat.constraints[index];
    if warp_eval_constraint(terms, deg, x, k, constraint, &instance.public, &instance.witness) != Galois128(0){
    return false;
};
};
    true
}

pub fn warp_absorb_index(mut m: usize, mut terms: usize, mut deg: usize, mut pesat: &WarpIndexDyn, mut transcript: &mut WarpTranscript)
{
    transcript.absorb_array(&(0..u5).map(|i| b"[105, 110, 100, 101, 120]"[i]).collect::<Vec<u8>>().n, &(0..u5).map(|i| b"[105, 110, 100, 101, 120]"[i]).collect::<Vec<u8>>());
    transcript.absorb_usize(m);
    transcript.absorb_usize(pesat.public_len);
    transcript.absorb_usize(pesat.witness_len);
    transcript.absorb_usize(pesat.degree_bound);
    for c in 0.. m{
    let constraint = &pesat.constraints[c];
    transcript.absorb_usize((constraint.n_terms as usize));
    for i in 0.. terms{
    if i < (constraint.n_terms as usize){
    let term = &constraint.terms[i];
    transcript.absorb_field(term.coeff);
    transcript.absorb_usize((term.n_vars as usize));
    for j in 0.. deg{
    if j < (term.n_vars as usize){
    transcript.absorb_usize(term.vars[j]);
};
}
};
};
}
}

pub fn warp_uni_at_zero(mut cap: usize, mut poly: &WarpUniDyn) -> Galois128
{
    poly.evaluations[0]
}

pub fn warp_uni_at_one(mut cap: usize, mut poly: &WarpUniDyn) -> Galois128
{
    poly.evaluations[1]
}

pub fn warp_uni_evaluate(mut cap: usize, mut poly: &WarpUniDyn, mut x: Galois128) -> Galois128
{
    let n = poly.count as usize;
    let mut out = Galois128(0);
    for i in 0.. cap{
    if i < n{
    let mut numerator = Galois128(1);
    let mut denominator = Galois128(1);
    for j in 0.. cap{
    if (j < n) && (i != j){
    numerator = (numerator * (x - Galois128((j as u128))));
    denominator = (denominator * (Galois128((i as u128)) - Galois128((j as u128))));
};
};
    out = (out + ((poly.evaluations[i] * numerator) * warp_inv(denominator)));
};
};
    out
}

pub fn warp_inv(mut a: Galois128) -> Galois128
{
    let mut result = Galois128(1);
    for _ in 0.. 127 as u32{
    result = ((result * result) * a);
};
    result * result
}

pub fn warp_absorb_instance(mut logn: usize, mut beta: usize, mut transcript: &mut WarpTranscript, mut instance: &WarpAccInstanceDyn)
{
    transcript.absorb_array(&(0..u12).map(|i| b"[97, 99, 99, 45, 105, 110, 115, 116, 97, 110, 99, 101]"[i]).collect::<Vec<u8>>().n, &(0..u12).map(|i| b"[97, 99, 99, 45, 105, 110, 115, 116, 97, 110, 99, 101]"[i]).collect::<Vec<u8>>());
    transcript.absorb_len_prefixed(&instance.root.n, &instance.root);
    transcript.absorb_fields(&instance.alpha.n, &instance.alpha);
    transcript.absorb_field(instance.mu);
    transcript.absorb_fields(&instance.beta.n, &instance.beta);
    transcript.absorb_field(instance.eta);
}

pub fn warp_transcript_for<M, TERMS, DEG, X, FRESH, A, LOGN, BETA>(mut m: usize, mut terms: usize, mut deg: usize, mut x: usize, mut fresh: usize, mut a: usize, mut logn: usize, mut beta: usize, mut index: &WarpIndexDyn, mut fresh_publics: &Vec<Vec<Galois128>>, mut accumulators: &Vec<WarpAccInstanceDyn>) -> WarpTranscript
{
    let domain = (0..u51).map(|i| WARP_TRANSCRIPT_DOMAIN[i]).collect::<Vec<u8>>();
    let mut transcript = warp_transcript_new().domain_sep(&domain.n, &domain);
    warp_absorb_index(m, terms, deg, index, &mut transcript);
    transcript.absorb_usize(fresh);
    for i in 0.. fresh{
    transcript.absorb_array(&(0..u12).map(|i| b"[102, 114, 101, 115, 104, 45, 112, 117, 98, 108, 105, 99]"[i]).collect::<Vec<u8>>().n, &(0..u12).map(|i| b"[102, 114, 101, 115, 104, 45, 112, 117, 98, 108, 105, 99]"[i]).collect::<Vec<u8>>());
    transcript.absorb_fields(&fresh_publics[i].n, &fresh_publics[i]);
};
    transcript.absorb_usize(a);
    for i in 0.. a{
    warp_absorb_instance(logn, beta, &mut transcript, &accumulators[i]);
};
    transcript
}

pub fn warp_genesis_spec<M, TERMS, DEG, K, X, N, C, NODES, BETA>(mut m: usize, mut terms: usize, mut deg: usize, mut k: usize, mut x: usize, mut n: usize, mut nodes: usize, mut beta: usize, mut index: &WarpIndexDyn, mut code: &C, mut instance: &WarpWitnessDyn) -> Option<WarpAccDyn>
{
    if !warp_index_is_valid(m, terms, deg, index) || !is_pow2(n){
    return None;
};
    if (x != index.public_len) || (k != index.witness_len) || (beta != (index.public_len + warp_log_constraints(m, terms, deg, index))){
    return None;
};
    if !warp_is_instance_satisfied(m, terms, deg, k, x, index, instance){
    return None;
};
    let codeword = code.encode(&instance.witness);
    let tree = warp_commit_codeword::<N, NODES>(n, nodes, &codeword);
    let alpha: Vec<Galois128> = (0..ilog2(n)).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    let mut beta = (0..beta).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for i in 0.. x{
    beta[i] = instance.public[i];
};
    let mu = warp_mle_eval(&codeword, &alpha);
    let eta = warp_evaluate_bundled(m, terms, deg, k, beta, index, &beta, &instance.witness);
    Some(WarpAccDyn { instance: WarpAccInstanceDyn { root: warp_merkle_root(nodes, &tree), alpha: alpha, mu: mu, beta: beta, eta: eta, logn: 0 }, witness: WarpAccWitnessDyn { tree: tree, codeword: codeword, witness: instance.witness.clone(), n: 0, k: 0, nodes: 0 }, n: 0, k: 0, logn: 0, beta: 0, nodes: 0 })
}

pub fn warp_decide_spec<M, TERMS, DEG, N, K, LOGN, BETA, NODES, C>(mut m: usize, mut terms: usize, mut deg: usize, mut n: usize, mut k: usize, mut logn: usize, mut beta: usize, mut nodes: usize, mut index: &WarpIndexDyn, mut code: &C, mut accumulator: &WarpAccDyn) -> bool
{
    if !warp_index_is_valid(m, terms, deg, index){
    return false;
};
    if (logn != log2_pow2(n)) || (beta != (index.public_len + warp_log_constraints(m, terms, deg, index))) || (k != index.witness_len) || (warp_merkle_root(nodes, &accumulator.witness.tree) != accumulator.instance.root){
    return false;
};
    let recomputed = warp_commit_codeword::<N, NODES>(n, nodes, &accumulator.witness.codeword);
    if warp_merkle_root(nodes, &recomputed) != accumulator.instance.root{
    return false;
};
    if code.encode(&accumulator.witness.witness) != accumulator.witness.codeword{
    return false;
};
    if warp_mle_eval(n, logn, &accumulator.witness.codeword, &accumulator.instance.alpha) != accumulator.instance.mu{
    return false;
};
    warp_evaluate_bundled(m, terms, deg, k, beta, index, &accumulator.instance.beta, &accumulator.witness.witness) == accumulator.instance.eta
}

pub fn warp_combine_vectors<ELL, LOGL, LEN>(mut ell: usize, mut logl: usize, mut len: usize, mut items: &Vec<Vec<Galois128>>, mut point: &Vec<Galois128>) -> Vec<Galois128>
{
    let weights = warp_eq_evals::<LOGL, ELL>(logl, ell, point);
    let mut out = (0..len).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for i in 0.. ell{
    let weight = weights[i];
    for j in 0.. len{
    out[j] = (out[j] + (items[i][j] * weight));
};
};
    out
}

pub fn warp_point_with_suffix(mut log: usize, mut prefix: &Vec<Galois128>, mut prefix_len: usize, mut current: Galois128, mut suffix: usize, mut suffix_len: usize) -> Vec<Galois128>
{
    (0..log).map(|i| {
    if i < prefix_len{
    prefix[i]
} else if i == prefix_len{
    current
} else {
    let bit = (i - prefix_len) - 1;
    if (bit < suffix_len) && (((suffix >> bit) & 1) == 1){
    Galois128(1)
} else {
    Galois128(0)
}
}
}).collect::<Vec<_>>()
}

pub fn warp_evaluate_g<M, TERMS, DEG, N, K, LOGN, BETA>(mut m: usize, mut terms: usize, mut deg: usize, mut n: usize, mut k: usize, mut logn: usize, mut beta: usize, mut index: &WarpIndexDyn, mut codeword: &Vec<Galois128>, mut witness: &Vec<Galois128>, mut alpha: &Vec<Galois128>, mut beta: &Vec<Galois128>, mut omega: Galois128) -> Galois128
{
    let evaluation = warp_mle_eval(n, logn, codeword, alpha);
    let bundled = warp_evaluate_bundled(m, terms, deg, k, beta, index, beta, witness);
    evaluation + (omega * bundled)
}

pub fn warp_eq_star_at<R, LOGR, LOGN>(mut r: usize, mut logr: usize, mut logn: usize, mut claim_points: &Vec<Vec<Galois128>>, mut xi: &Vec<Galois128>, mut point: &Vec<Galois128>) -> Galois128
{
    let mut out = Galois128(0);
    for index in 0.. r{
    let claim_index = warp_binary_point::<LOGR>(logr, index);
    let weight = warp_eq_scalar(logr, xi, &claim_index) * warp_eq_scalar(logn, &claim_points[index], point);
    out = (out + weight);
};
    out
}

pub fn warp_first_sumcheck_eval<M, TERMS, DEG, K, N, ELL, LOGL, LOGN, BETA>(mut m: usize, mut terms: usize, mut deg: usize, mut k: usize, mut n: usize, mut ell: usize, mut logl: usize, mut logn: usize, mut beta: usize, mut index: &WarpIndexDyn, mut codewords: &Vec<Vec<Galois128>>, mut witnesses: &Vec<Vec<Galois128>>, mut alphas: &Vec<Vec<Galois128>>, mut betas: &Vec<Vec<Galois128>>, mut tau: &Vec<Galois128>, mut omega: Galois128, mut prefix: &Vec<Galois128>, mut round: usize, mut x: Galois128) -> Galois128
{
    let log_ell = log2_pow2(ell);
    let suffix_len = (log_ell - round) - 1;
    let mut out = Galois128(0);
    for suffix in 0.. ell >> (round + 1){
    let point = warp_point_with_suffix::<LOGL>(logl, prefix, round, x, suffix, suffix_len);
    let codeword = warp_combine_vectors(ell, logl, n, codewords, &point);
    let witness = warp_combine_vectors(ell, logl, k, witnesses, &point);
    let alpha = warp_combine_vectors(ell, logl, logn, alphas, &point);
    let beta = warp_combine_vectors(ell, logl, beta, betas, &point);
    let g = warp_evaluate_g(m, terms, deg, n, k, logn, beta, index, &codeword, &witness, &alpha, &beta, omega);
    out = (out + (warp_eq_scalar(logl, tau, &point) * g));
};
    out
}

pub fn warp_second_sumcheck_eval<N, R, LOGR, LOGN>(mut n: usize, mut r: usize, mut logr: usize, mut logn: usize, mut codeword: &Vec<Galois128>, mut claim_points: &Vec<Vec<Galois128>>, mut xi: &Vec<Galois128>, mut prefix: &Vec<Galois128>, mut round: usize, mut x: Galois128) -> Galois128
{
    let log_n = log2_pow2(n);
    let suffix_len = (log_n - round) - 1;
    let mut out = Galois128(0);
    for suffix in 0.. n >> (round + 1){
    let point = warp_point_with_suffix::<LOGN>(logn, prefix, round, x, suffix, suffix_len);
    let eq = warp_eq_star_at(r, logr, logn, claim_points, xi, &point);
    out = (out + (eq * warp_mle_eval(n, logn, codeword, &point)));
};
    out
}

pub fn warp_check_sumcheck(mut sc: usize, mut rounds: usize, mut polys: &Vec<WarpUniDyn>, mut initial_claim: Galois128, mut challenges: &Vec<Galois128>, mut final_claim: Galois128) -> bool
{
    if rounds == 0{
    return (initial_claim == final_claim);
};
    if (warp_uni_at_zero(sc, &polys[0]) + warp_uni_at_one(sc, &polys[0])) != initial_claim{
    return false;
};
    for round in 1.. rounds{
    let expected = warp_uni_evaluate(sc, &polys[round - 1], challenges[round - 1]);
    if (warp_uni_at_zero(sc, &polys[round]) + warp_uni_at_one(sc, &polys[round])) != expected{
    return false;
};
};
    warp_uni_evaluate(sc, &polys[rounds - 1], challenges[rounds - 1]) == final_claim
}

pub fn warp_prove_spec<M, TERMS, DEG, K, X, N, FRESH, A, ELL, S, SHIFTS, R, SC, TW, FSD1, NODES, BETA, C>(mut m: usize, mut terms: usize, mut deg: usize, mut k: usize, mut x: usize, mut n: usize, mut fresh: usize, mut a: usize, mut ell: usize, mut s: usize, mut shifts: usize, mut r: usize, mut sc: usize, mut tw: usize, mut fsd1: usize, mut nodes: usize, mut beta: usize, mut index: &WarpIndexDyn, mut code: &C, mut fresh: &Vec<WarpWitnessDyn>, mut accumulators: &Vec<WarpAccDyn>) -> Option<(WarpAccDyn, WarpProofDyn)>
{
    if !warp_index_is_valid(m, terms, deg, index){
    return None;
};
    let log_n = log2_pow2(n);
    let log_m = warp_log_constraints(m, terms, deg, index);
    let log_ell = log2_pow2(ell);
    if !is_pow2(n) || !is_pow2(ell) || (ell != (fresh + a)) || (fresh == 0) || !is_pow2(r) || (r != ((1 + s) + shifts)) || (tw != (log_ell + 1)) || (beta != (index.public_len + log_m)) || (k != index.witness_len) || (x != index.public_len) || (nodes != ((2 * n) - 1)){
    return None;
};
    let first_degree = 1 + (log_n + 1).max((log_m + index.degree_bound));
    if (fsd1 != (first_degree + 1)) || (fsd1 > sc) || (3 > sc){
    return None;
};
    for i in 0.. fresh{
    if !warp_is_instance_satisfied(m, terms, deg, k, x, index, &fresh[i]){
    return None;
};
};
    for i in 0.. a{
    if warp_merkle_root(nodes, &accumulators[i].witness.tree) != accumulators[i].instance.root{
    return None;
};
};
    let fresh_publics = (0..fresh).map(|i| fresh[i].public.clone()).collect::<Vec<Vec<Galois128>>>();
    let acc_instances = (0..a).map(|i| accumulators[i].instance.clone()).collect::<Vec<WarpAccInstanceDyn>>();
    let mut transcript = warp_transcript_for(index, &fresh_publics, &acc_instances);
    let fresh_codewords = (0..fresh).map(|i| code.encode(&fresh[i].witness)).collect::<Vec<Vec<Galois128>>>();
    let initial_tree = warp_commit_stacked::<N, FRESH, NODES>(n, fresh, nodes, &fresh_codewords);
    let initial_root = warp_merkle_root(nodes, &initial_tree);
    transcript.absorb_len_prefixed(n, &initial_root);
    let taus: Vec<Vec<Galois128>> = transcript.squeeze_field_grid(r);
    let zero_point_n = (0..ilog2(n)).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    let fresh_mus = (0..fresh).map(|i| {
    warp_mle_eval(&fresh_codewords[i], &zero_point_n)
}).collect::<Vec<Galois128>>();
    let mut alphas = (0..ell).map(|_| <Vec<Galois128>>::default()).collect::<Vec<Vec<Galois128>>>();
    let mut mus = (0..ell).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    let mut betas = (0..ell).map(|_| <Vec<Galois128>>::default()).collect::<Vec<Vec<Galois128>>>();
    let mut etas = (0..ell).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    let mut codewords = (0..ell).map(|_| <Vec<Galois128>>::default()).collect::<Vec<Vec<Galois128>>>();
    let mut witnesses = (0..ell).map(|_| <Vec<Galois128>>::default()).collect::<Vec<Vec<Galois128>>>();
    for i in 0.. fresh{
    alphas[i] = (0..ilog2(n)).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    mus[i] = fresh_mus[i];
    let mut beta = (0..beta).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for j in 0.. x{
    beta[j] = fresh[i].public[j];
};
    for j in 0.. m{
    if ((1 as usize) << j) < m{
    beta[x + j] = taus[i][j];
};
};
    betas[i] = beta;
    etas[i] = Galois128(0);
    codewords[i] = fresh_codewords[i].clone();
    witnesses[i] = fresh[i].witness.clone();
};
    transcript.absorb_fields(n, &fresh_mus);
    for i in 0.. a{
    let acc = &accumulators[i];
    alphas[fresh + i] = acc.instance.alpha.clone();
    mus[fresh + i] = acc.instance.mu;
    betas[fresh + i] = acc.instance.beta.clone();
    etas[fresh + i] = acc.instance.eta;
    codewords[fresh + i] = acc.witness.codeword.clone();
    witnesses[fresh + i] = acc.witness.witness.clone();
};
    let tau_omega: Vec<Galois128> = transcript.squeeze_fields(n);
    let tau = (0..ilog2(ell)).map(|i| tau_omega[i]).collect::<Vec<Galois128>>();
    let omega = tau_omega[tw - 1];
    let sigma1 = {
    let eq_tau = warp_eq_evals::<<ELL as Logarithm2>::Output, ELL>(&tau);
    let mut out = Galois128(0);
    for i in 0.. ell{
    out = (out + (eq_tau[i] * (mus[i] + (omega * etas[i]))));
};
    out
};
    let mut first_sumcheck = (0..ilog2(ell)).map(|_| WarpUniDyn { evaluations: (0..sc).map(|_| Galois128.default()).collect::<Vec<Galois128>>(), count: 0, cap: 0 }).collect::<Vec<WarpUniDyn>>();
    let mut gamma = (0..ilog2(ell)).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for round in 0.. ell{
    if ((1 as usize) << round) < ell{
    let mut evals = (0..sc).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for x in 0.. fsd1{
    evals[x] = warp_first_sumcheck_eval(index, &codewords, &witnesses, &alphas, &betas, &tau, omega, &gamma, round, Galois128((x as u128)));
};
    let poly = WarpUniDyn { evaluations: evals, count: (fsd1 as u32), cap: 0 };
    transcript.absorb_poly(sc, &poly.cnt, &poly);
    gamma[round] = transcript.squeeze_fields(n)[0];
    first_sumcheck[round] = poly;
};
};
    let zeta0 = warp_combine_vectors(&alphas, &gamma);
    let beta = warp_combine_vectors(&betas, &gamma);
    let folded_codeword = warp_combine_vectors(&codewords, &gamma);
    let folded_witness = warp_combine_vectors(&witnesses, &gamma);
    let nu0 = warp_mle_eval(&folded_codeword, &zeta0);
    let eta = warp_evaluate_bundled(m, terms, deg, k, beta, index, &beta, &folded_witness);
    if !warp_check_sumcheck(&first_sumcheck, sigma1, &gamma, {
    warp_eq_scalar(&tau, &gamma) * (nu0 + (omega * eta))
}){
    return None;
};
    let output_tree = warp_commit_codeword::<N, NODES>(n, nodes, &folded_codeword);
    let output_root = warp_merkle_root(nodes, &output_tree);
    transcript.absorb_len_prefixed(n, &output_root);
    transcript.absorb_field(nu0);
    transcript.absorb_field(eta);
    let ood_points: Vec<Vec<Galois128>> = transcript.squeeze_field_grid(r);
    let ood_answers = (0..s).map(|i| {
    warp_mle_eval(&folded_codeword, &ood_points[i])
}).collect::<Vec<Galois128>>();
    transcript.absorb_fields(n, &ood_answers);
    let (shift_indices, xi): (Vec<u32>, Vec<Galois128>) = transcript.squeeze_shift_and_xi(shifts);
    let mut claim_points = (0..r).map(|_| <Vec<Galois128>>::default()).collect::<Vec<Vec<Galois128>>>();
    let mut claim_values = (0..r).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    claim_points[0] = zeta0;
    claim_values[0] = nu0;
    for i in 0.. s{
    claim_points[1 + i] = ood_points[i].clone();
    claim_values[1 + i] = ood_answers[i];
};
    for i in 0.. shifts{
    claim_points[1 + s + i] = warp_binary_point((shift_indices[i] as usize));
    claim_values[1 + s + i] = folded_codeword[shift_indices[i] as usize];
};
    let sigma2 = {
    let eq_xi = warp_eq_evals::<<R as Logarithm2>::Output, R>(&xi);
    let mut out = Galois128(0);
    for i in 0.. r{
    out = (out + (claim_values[i] * eq_xi[i]));
};
    out
};
    let mut second_sumcheck = (0..ilog2(n)).map(|_| WarpUniDyn { evaluations: (0..sc).map(|_| Galois128.default()).collect::<Vec<Galois128>>(), count: 0, cap: 0 }).collect::<Vec<WarpUniDyn>>();
    let mut alpha = (0..ilog2(n)).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for round in 0.. n{
    if ((1 as usize) << round) < n{
    let mut evals = (0..sc).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for x in 0.. 3 as usize{
    evals[x] = warp_second_sumcheck_eval(&folded_codeword, &claim_points, &xi, &alpha, round, Galois128((x as u128)));
};
    let poly = WarpUniDyn { evaluations: evals, count: 3, cap: 0 };
    transcript.absorb_poly(sc, &poly.cnt, &poly);
    alpha[round] = transcript.squeeze_fields(n)[0];
    second_sumcheck[round] = poly;
};
};
    let mu = warp_mle_eval(&folded_codeword, &alpha);
    if !warp_check_sumcheck(&second_sumcheck, sigma2, &alpha, {
    let eq = warp_eq_star_at(&claim_points, &xi, &alpha);
    mu * eq
}){
    return None;
};
    let initial_paths = (0..shifts).map(|i| {
    warp_merkle_open(&initial_tree, (shift_indices[i] as usize))
}).collect::<Vec<WarpAuthPathDyn>>();
    let accumulator_paths = (0..a).map(|i| {
    (0..shifts).map(|j| {
    warp_merkle_open(&accumulators[i].witness.tree, (shift_indices[j] as usize))
}).collect::<Vec<WarpAuthPathDyn>>()
}).collect::<Vec<Vec<WarpAuthPathDyn>>>();
    let openings = (0..shifts).map(|j| {
    (0..ell).map(|i| codewords[i][shift_indices[j] as usize]).collect::<Vec<Galois128>>()
}).collect::<Vec<Vec<Galois128>>>();
    let accumulator = WarpAccDyn { instance: WarpAccInstanceDyn { root: output_root, alpha: alpha, mu: mu, beta: beta, eta: eta, logn: 0 }, witness: WarpAccWitnessDyn { tree: output_tree, codeword: folded_codeword, witness: folded_witness, n: 0, k: 0, nodes: 0 }, n: 0, k: 0, logn: 0, beta: 0, nodes: 0 };
    let proof = WarpProofDyn { initial_root: initial_root, fresh_mus: fresh_mus, first_sumcheck: first_sumcheck, nu0: nu0, ood_answers: ood_answers, second_sumcheck: second_sumcheck, initial_paths: initial_paths, accumulator_paths: accumulator_paths, openings: openings, fresh: 0, ell: 0, a: 0, s: 0, shifts: 0, sc: 0, logn: 0 };
    Some((accumulator, proof))
}

pub fn warp_hash_fresh_row(mut ell: usize, mut fresh: usize, mut row: &Vec<Galois128>) -> WarpDigest
{
    let mut h = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb_array(&mut h, &(0..1).map(|_| 0).collect::<Vec<u8>>());
    for i in 0.. fresh{
    let row_bytes: Vec<u8> = le16(row[i]);
    spec_sponge_absorb_array(&mut h, &row_bytes);
};
    let mut reader = spec_sponge_finalize_xof(&h, 6);
    let mut out: Vec<u8> = (0..32).map(|_| 0).collect::<Vec<u8>>();
    spec_xof_read_array(&mut reader, &mut out);
    out
}

pub fn warp_verify_spec<M, TERMS, DEG, X, FRESH, A, ELL, S, SHIFTS, R, SC, TW, FSD1, N, BETA>(mut m: usize, mut terms: usize, mut deg: usize, mut x: usize, mut fresh: usize, mut a: usize, mut ell: usize, mut s: usize, mut shifts: usize, mut r: usize, mut sc: usize, mut tw: usize, mut fsd1: usize, mut n: usize, mut beta: usize, mut index: &WarpIndexDyn, mut fresh_publics: &Vec<Vec<Galois128>>, mut input_accumulators: &Vec<WarpAccInstanceDyn>, mut output: &WarpAccInstanceDyn, mut proof: &WarpProofDyn) -> bool
{
    if !warp_index_is_valid(m, terms, deg, index){
    return false;
};
    let log_m = warp_log_constraints(m, terms, deg, index);
    let log_ell = log2_pow2(ell);
    let log_n = log2_pow2(n);
    if (fresh == 0) || !is_pow2(ell) || (ell != (fresh + a)) || !is_pow2(r) || (r != ((1 + s) + shifts)) || (tw != (log_ell + 1)) || (beta != (index.public_len + log_m)) || (x != index.public_len) || (n != ((1 as usize) << log2_pow2(n))) || (fsd1 != (2 + (log_n + 1).max((log_m + index.degree_bound)))){
    return false;
};
    let mut transcript = warp_transcript_for(index, fresh_publics, input_accumulators);
    transcript.absorb_len_prefixed(n, &proof.initial_root);
    let taus: Vec<Vec<Galois128>> = transcript.squeeze_field_grid(r);
    transcript.absorb_fields(n, &proof.fresh_mus);
    let mut alphas = (0..ell).map(|_| <Vec<Galois128>>::default()).collect::<Vec<Vec<Galois128>>>();
    let mut mus = (0..ell).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    let mut betas = (0..ell).map(|_| <Vec<Galois128>>::default()).collect::<Vec<Vec<Galois128>>>();
    let mut etas = (0..ell).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for i in 0.. fresh{
    mus[i] = proof.fresh_mus[i];
    let mut beta_i = (0..beta).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for j in 0.. x{
    beta_i[j] = fresh_publics[i][j];
};
    for j in 0.. m{
    if ((1 as usize) << j) < m{
    beta_i[x + j] = taus[i][j];
};
};
    betas[i] = beta_i;
    etas[i] = Galois128(0);
};
    for i in 0.. a{
    let acc = &input_accumulators[i];
    alphas[fresh + i] = acc.alpha.clone();
    mus[fresh + i] = acc.mu;
    betas[fresh + i] = acc.beta.clone();
    etas[fresh + i] = acc.eta;
};
    let tau_omega: Vec<Galois128> = transcript.squeeze_fields(n);
    let tau = (0..ilog2(ell)).map(|i| tau_omega[i]).collect::<Vec<Galois128>>();
    let omega = tau_omega[tw - 1];
    let mut gamma = (0..ilog2(ell)).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for round in 0.. ell{
    if ((1 as usize) << round) < ell{
    let poly = &proof.first_sumcheck[round];
    transcript.absorb_poly(sc, poly.cnt, poly);
    gamma[round] = transcript.squeeze_fields(n)[0];
};
};
    let sigma1 = {
    let eq_tau = warp_eq_evals::<<ELL as Logarithm2>::Output, ELL>(&tau);
    let mut out = Galois128(0);
    for i in 0.. ell{
    out = (out + (eq_tau[i] * (mus[i] + (omega * etas[i]))));
};
    out
};
    let zeta0 = warp_combine_vectors(&alphas, &gamma);
    let beta = warp_combine_vectors(&betas, &gamma);
    if beta != output.beta{
    return false;
};
    if !warp_check_sumcheck(&proof.first_sumcheck, sigma1, &gamma, {
    warp_eq_scalar(&tau, &gamma) * (proof.nu0 + (omega * output.eta))
}){
    return false;
};
    transcript.absorb_len_prefixed(n, &output.root);
    transcript.absorb_field(proof.nu0);
    transcript.absorb_field(output.eta);
    let ood_points: Vec<Vec<Galois128>> = transcript.squeeze_field_grid(r);
    transcript.absorb_fields(n, &proof.ood_answers);
    let (shift_indices, xi): (Vec<u32>, Vec<Galois128>) = transcript.squeeze_shift_and_xi(shifts);
    for query in 0.. shifts{
    let path = &proof.initial_paths[query];
    let row = &proof.openings[query];
    let leaf = warp_hash_fresh_row::<ELL, FRESH>(ell, fresh, row);
    if !warp_merkle_verify_hashed(&proof.initial_root, &leaf, (shift_indices[query] as usize), path){
    return false;
};
};
    for accumulator_index in 0.. a{
    for query in 0.. shifts{
    let path = &proof.accumulator_paths[accumulator_index][query];
    let value = proof.openings[query][fresh + accumulator_index];
    let leaf = warp_hash_leaf_field(value);
    if !warp_merkle_verify_hashed(&input_accumulators[accumulator_index].root, &leaf, (shift_indices[query] as usize), path){
    return false;
};
};
};
    let mut claim_points = (0..r).map(|_| <Vec<Galois128>>::default()).collect::<Vec<Vec<Galois128>>>();
    let mut claim_values = (0..r).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    claim_points[0] = zeta0;
    claim_values[0] = proof.nu0;
    for i in 0.. s{
    claim_points[1 + i] = ood_points[i].clone();
    claim_values[1 + i] = proof.ood_answers[i];
};
    let eq_gamma = warp_eq_evals::<<ELL as Logarithm2>::Output, ELL>(&gamma);
    for i in 0.. shifts{
    claim_points[1 + s + i] = warp_binary_point((shift_indices[i] as usize));
    let row = &proof.openings[i];
    let mut value = Galois128(0);
    for j in 0.. ell{
    value = (value + (row[j] * eq_gamma[j]));
};
    claim_values[1 + s + i] = value;
};
    let sigma2 = {
    let eq_xi = warp_eq_evals::<<R as Logarithm2>::Output, R>(&xi);
    let mut out = Galois128(0);
    for i in 0.. r{
    out = (out + (claim_values[i] * eq_xi[i]));
};
    out
};
    let mut alpha = (0..ilog2(n)).map(|_| Galois128.default()).collect::<Vec<Galois128>>();
    for round in 0.. n{
    if ((1 as usize) << round) < n{
    let poly = &proof.second_sumcheck[round];
    transcript.absorb_poly(sc, poly.cnt, poly);
    alpha[round] = transcript.squeeze_fields(n)[0];
};
};
    if alpha != output.alpha{
    return false;
};
    warp_check_sumcheck(&proof.second_sumcheck, sigma2, &alpha, {
    let eq = warp_eq_star_at(&claim_points, &xi, &output.alpha);
    output.mu * eq
})
}

