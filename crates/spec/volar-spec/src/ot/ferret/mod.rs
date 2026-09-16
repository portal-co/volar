// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Ferret COT extension (Yang, Weng, Lan, Zhang, Wang, ePrint 2020/924).
//!
//! First landing is **Ferret-Reg** (regular LPN noise): Fig. 6 SPCOT, §5
//! regular-indices MPCOT, Fig. 9 ΠCOT with the §6.2 bootstrap. Ferret-Uni
//! (Fig. 7 Cuckoo) is [`mpcot_uni`].
//!
//! Binding: [`docs/reviews/ferret-paper-binding.md`]. This module does not
//! self-promote to paper-pinned.

pub mod cot;
pub mod lpn;
pub mod mpcot_reg;
pub mod mpcot_uni;
pub mod params;
pub mod pool;
pub mod spcot;

pub use cot::{
    FerretExtendOut, FerretIterMsg, FerretPrep, FerretReceiverSeed, FerretSenderSeed,
    ferret_extend, ferret_extend_malicious, ferret_extend_uni, ferret_extend_uni_malicious,
    ferret_finish, ferret_prepare_receiver, ferret_receiver_mpcot, ferret_sender_mpcot,
    sample_seed_cots,
};
pub use params::{
    FERRET_REG_MAIN, FERRET_REG_SETUP, FERRET_REG_TOY, FERRET_UNI_MAIN, FERRET_UNI_SETUP,
    FERRET_UNI_TOY, FerretParams,
};
pub use pool::{CotPoolReceiver, CotPoolSender, bea95_chosen_bit, refill, take_random};
pub use spcot::{
    Block, KAPPA_BITS, KAPPA_BYTES, spcot_batched_fs_chis, spcot_batched_masked_choice,
    spcot_batched_receiver_hash_w, spcot_batched_sender_hash_v, spcot_consistency_check,
    spcot_fs_chis, spcot_masked_choice, spcot_receiver_extend, spcot_receiver_hash_w,
    spcot_sender_extend, spcot_sender_hash_v,
};
