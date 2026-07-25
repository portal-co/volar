//! Generic, N-storage/N-address honest memory-boundary accounting for a
//! driven test's own generated Rust source.
//!
//! `mem_probe.rs`'s own honest driver hand-writes this bookkeeping
//! (`mem2`/`mem33`, two `MemoryCheckState` locals, each single-address)
//! because its own circuit only ever touches 2 storages at 1 address
//! each. The real interpreter touches dozens of storages across many
//! addresses each (register file, RAM, `StorageId::VAFFLE_SSA_SPILL`'s
//! own cross-block spill slots) -- this module generalizes the same
//! `MemoryCheckState` machinery (`crates/spec/volar-spec/src/vole/memory.rs`)
//! programmatically, driven by a real `crate::vole::MemoryTrace` (which
//! storage/address/read-or-write, in real per-step order) plus real plain
//! values read via `volar_fuzz::interpreter::ir::eval_ir_circuit_step_with_watch`.
//!
//! # Field choice: `Gf128`, not `Galois`
//!
//! `mem_probe.rs` uses `MemoryCheckState<Galois>` (`Galois` = `u8`, `GF(2^8)`)
//! because every address there is a compile-time-known `0`. A real
//! address here can be up to 64 bits (`VAFFLE_SSA_SPILL`'s own `SP|vid`
//! addressing -- see `crates/ir/volar-vaffle-target/src/vaffle_ssa.rs`),
//! which would alias under `Galois`'s 256-value domain. `Gf128 =
//! Ext<Gf64, Beta64>` (`crates/iop/volar-iop/src/field.rs`) implements
//! `FromBytes::from_u64`, injective for any `u64`, and is *already* the
//! field `prove_and_verify_iop`'s own boundary check consumes directly --
//! no separate `.iop_embed()` step needed (that trait, `IopLift`, is only
//! implemented for `Galois`/`Bit`, not `Gf128` itself -- confirmed by
//! reading `crates/iop/volar-verifier-iop-runtime/src/lib.rs`).
//!
//! # Timestamps
//!
//! Each `(storage_id, type_id)` gets its own monotonic timestamp counter,
//! shared across every address within that storage (matching
//! `mem_probe.rs`'s own `ts2`/`ts33` convention) -- safe because
//! `MemoryCheckState::encode` folds `addr` into the encoding, so two
//! different addresses at the same nominal timestamp never collide.
//! `MemoryTraceEntry.timestamp` (a circuit-position marker, the same
//! every real step since it's the same circuit re-executed) is *not*
//! reused here -- the driver, not the circuit, owns real cross-step
//! timestamp uniqueness, exactly as `mem_probe.rs`'s own hand-threaded
//! `ts2`/`ts33` do.

use std::collections::BTreeMap;

/// Per-`(storage_id, type_id, addr)` running state: the last known plain
/// value and the timestamp that established it (a write, or a prior
/// read re-producing it).
type AddrKey = (u32, u32, u64);

/// Generic honest multiset memory-check accounting, built incrementally
/// as real ops are observed (in true execution order) and finished once
/// at the end of a driven run.
pub struct MemCheckAccounting {
    /// `(storage_id, type_id)` -> this storage's own `MemoryCheckState`
    /// local variable name in the generated source (declared on first
    /// use).
    storages: BTreeMap<(u32, u32), String>,
    /// `(storage_id, type_id, addr)` -> `(last_value, last_timestamp)`.
    addr_state: BTreeMap<AddrKey, (u64, u64)>,
    /// `(storage_id, type_id)` -> next timestamp to assign (0 is reserved
    /// for `init`).
    next_ts: BTreeMap<(u32, u32), u64>,
}

impl MemCheckAccounting {
    pub fn new() -> Self {
        MemCheckAccounting {
            storages: BTreeMap::new(),
            addr_state: BTreeMap::new(),
            next_ts: BTreeMap::new(),
        }
    }

    /// Record one real storage op (in the exact order the real
    /// `MemoryTrace` -- and the plain interpreter's own evaluation --
    /// produced it) and append the Rust statement(s) needed to `out`.
    /// `pre_init` supplies each address's own true initial value (from
    /// `circuit.pre_init`), defaulting to `0` for any address not
    /// pre-initialized -- looked up lazily, only the first time each
    /// address is actually touched (matching `MemoryCheckState::init`'s
    /// own "call once per address before any write/read" contract).
    pub fn emit_op(
        &mut self,
        out: &mut String,
        storage_id: u32,
        type_id: u32,
        addr: u64,
        value: u64,
        is_write: bool,
        pre_init: &BTreeMap<AddrKey, u64>,
    ) {
        let key = (storage_id, type_id);
        if !self.storages.contains_key(&key) {
            let name = format!("mem_s{storage_id}_t{type_id}");
            out.push_str(&format!("let mut {name} = MemoryCheckState::<Gf128>::new(key.clone());\n"));
            self.storages.insert(key, name);
        }
        let local = self.storages[&key].clone();

        let addr_key = (storage_id, type_id, addr);
        if !self.addr_state.contains_key(&addr_key) {
            let init_val = pre_init.get(&addr_key).copied().unwrap_or(0);
            out.push_str(&format!(
                "{local}.init(Gf128::from_u64({addr}u64), Gf128::from_u64({init_val}u64));\n"
            ));
            self.addr_state.insert(addr_key, (init_val, 0));
        }

        let (old_value, old_ts) = self.addr_state[&addr_key];
        let ts_counter = self.next_ts.entry(key).or_insert(0);
        *ts_counter += 1;
        let new_ts = *ts_counter;

        if is_write {
            out.push_str(&format!(
                "{local}.write(Gf128::from_u64({addr}u64), Gf128::from_u64({value}u64), {new_ts}u64, Gf128::from_u64({old_value}u64), {old_ts}u64);\n"
            ));
        } else {
            debug_assert_eq!(
                value, old_value,
                "dishonest trace: read at storage=({storage_id},{type_id}) addr={addr} observed {value}, but this accounting's own tracked value is {old_value} -- the plain interpreter and this accounting have desynced"
            );
            out.push_str(&format!(
                "{local}.read(Gf128::from_u64({addr}u64), Gf128::from_u64({value}u64), {new_ts}u64, {old_ts}u64);\n"
            ));
        }
        // A read re-produces the value at a fresh timestamp too (so a
        // later op can reference *this* read as its own prior
        // reference); a write obviously changes both. Either way the
        // value itself only ever changes on a write.
        self.addr_state.insert(addr_key, (value, new_ts));
    }

    /// After every step has been processed: drain every address ever
    /// touched, emit a `verify()` assertion per storage, and return
    /// `(h_produce_expr, h_consume_expr)` -- Rust array-literal
    /// expressions (`[s0_produce, s1_produce, ...]`, one element per
    /// storage in stable `(storage_id, type_id)` order) ready to feed
    /// `prove_and_verify_iop`'s own `mem_acc_in`/`mem_acc_out`.
    pub fn finish(&mut self, out: &mut String) -> (String, String) {
        let mut by_storage: BTreeMap<(u32, u32), Vec<(u64, (u64, u64))>> = BTreeMap::new();
        for (&(sid, tid, addr), &(val, ts)) in &self.addr_state {
            by_storage.entry((sid, tid)).or_default().push((addr, (val, ts)));
        }

        let mut produce_names = Vec::new();
        let mut consume_names = Vec::new();
        for (key, local) in &self.storages {
            if let Some(addrs) = by_storage.get(key) {
                for &(addr, (val, ts)) in addrs {
                    out.push_str(&format!(
                        "{local}.drain(Gf128::from_u64({addr}u64), Gf128::from_u64({val}u64), {ts}u64);\n"
                    ));
                }
            }
            out.push_str(&format!(
                "assert!({local}.verify(), \"storage (sid={}, tid={}) own independent memory multiset check must balance\");\n",
                key.0, key.1
            ));
            let p_name = format!("{local}_produce");
            let c_name = format!("{local}_consume");
            out.push_str(&format!("let {p_name} = *{local}.produce();\n"));
            out.push_str(&format!("let {c_name} = *{local}.consume();\n"));
            produce_names.push(p_name);
            consume_names.push(c_name);
        }

        (
            format!("[{}]", produce_names.join(", ")),
            format!("[{}]", consume_names.join(", ")),
        )
    }

    /// Whether any storage op has been recorded at all -- a circuit that
    /// never touches real committed storage (unlikely for the real
    /// interpreter, but a real possibility for a smaller test circuit)
    /// has nothing to finalize a memory boundary over.
    pub fn is_empty(&self) -> bool {
        self.storages.is_empty()
    }
}

impl Default for MemCheckAccounting {
    fn default() -> Self {
        Self::new()
    }
}
