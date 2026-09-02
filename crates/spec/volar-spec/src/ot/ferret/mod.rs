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
    ferret_extend, ferret_extend_uni, ferret_finish, ferret_prepare_receiver, ferret_receiver_mpcot,
    ferret_sender_mpcot, sample_seed_cots, FerretExtendOut, FerretIterMsg, FerretPrep,
    FerretReceiverSeed, FerretSenderSeed,
};
pub use params::{
    FerretParams, FERRET_REG_MAIN, FERRET_REG_SETUP, FERRET_REG_TOY, FERRET_UNI_MAIN,
    FERRET_UNI_SETUP, FERRET_UNI_TOY,
};
pub use pool::{bea95_chosen_bit, refill, take_random, CotPoolReceiver, CotPoolSender};
pub use spcot::{spcot_receiver_extend, spcot_sender_extend, Block, KAPPA_BITS, KAPPA_BYTES};
