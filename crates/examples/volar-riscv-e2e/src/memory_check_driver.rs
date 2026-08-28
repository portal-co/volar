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
//!
//! # Real-runtime-loop calling convention
//!
//! `record` is pure host-side bookkeeping (no text emitted) -- call it
//! once per `(real step, trace-entry-position)` pair, in true execution
//! order, exactly as the old `emit_op` was called, but it now *returns*
//! a [`MemOpWitness`] instead of writing literals into the generated
//! source. The caller collects one `Vec<MemOpWitness>` per real step
//! (index-matched to `crate::vole::MemoryTrace::entries`) into a runtime
//! witness array emitted once in the generated source, then calls
//! `emit_pre_loop_decls` once (before the loop) and `emit_call_site`
//! once per trace-entry position (inside the loop body, referencing
//! that position's own witness field expression) -- mirroring
//! `mem_probe.rs`'s own `witness[step]...`-referencing conversion.
//!
//! A trace-entry position's own runtime *address* can differ from step
//! to step (e.g. a RAM access with a data-dependent address) even though
//! the position itself is static (same circuit re-executed every real
//! step) -- so whether `.init()` is needed this step (`needs_init`) is a
//! genuinely per-step fact, not something the call-site's *text* can
//! decide once at generation time. `MemOpWitness` carries it as a plain
//! `bool` field; `emit_call_site`'s own generated text gates the
//! `.init()` call on it at runtime (`if {witness}.needs_init {{ ... }}`).

use std::collections::BTreeMap;

/// Per-`(storage_id, type_id, addr)` running state: the last known plain
/// value and the timestamp that established it (a write, or a prior
/// read re-producing it).
type AddrKey = (u32, u32, u64);

/// One real memory op's fully-resolved host-side facts, captured ahead
/// of generating the driver's loop body -- mirrors
/// `MemoryCheckState::read`/`write`'s own arguments exactly, plus
/// `needs_init`/`init_val` (this address's first-ever touch, if so).
#[derive(Clone, Copy, Debug, Default)]
pub struct MemOpWitness {
    pub needs_init: bool,
    pub init_val: u64,
    pub addr: u64,
    pub value: u64,
    pub is_write: bool,
    pub old_value: u64,
    pub old_ts: u64,
    pub new_ts: u64,
}

/// Generic honest multiset memory-check accounting, built incrementally
/// as real ops are observed (in true execution order) and finished once
/// at the end of a driven run.
pub struct MemCheckAccounting {
    /// `(storage_id, type_id)` -> this storage's own `MemoryCheckState`
    /// local variable name in the generated source (declared on first
    /// use). `BTreeMap` gives a stable, deterministic declaration order
    /// regardless of discovery order, which is fine here since the *set*
    /// of storages touched is fixed by the circuit's own static
    /// structure (identical every real step).
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
    /// produced it) and return its fully-resolved witness. Pure
    /// host-side bookkeeping; emits no text. `pre_init` supplies each
    /// address's own true initial value (from `circuit.pre_init`),
    /// defaulting to `0` for any address not pre-initialized --
    /// consulted only the first time each address is actually touched
    /// (matching `MemoryCheckState::init`'s own "call once per address
    /// before any write/read" contract).
    pub fn record(
        &mut self,
        storage_id: u32,
        type_id: u32,
        addr: u64,
        value: u64,
        is_write: bool,
        pre_init: &BTreeMap<AddrKey, u64>,
    ) -> MemOpWitness {
        let key = (storage_id, type_id);
        self.storages
            .entry(key)
            .or_insert_with(|| format!("mem_s{storage_id}_t{type_id}"));

        let addr_key = (storage_id, type_id, addr);
        let needs_init = !self.addr_state.contains_key(&addr_key);
        let init_val = if needs_init {
            pre_init.get(&addr_key).copied().unwrap_or(0)
        } else {
            0
        };
        if needs_init {
            self.addr_state.insert(addr_key, (init_val, 0));
        }

        let (old_value, old_ts) = self.addr_state[&addr_key];
        let ts_counter = self.next_ts.entry(key).or_insert(0);
        *ts_counter += 1;
        let new_ts = *ts_counter;

        if !is_write {
            debug_assert_eq!(
                value, old_value,
                "dishonest trace: read at storage=({storage_id},{type_id}) addr={addr} observed {value}, but this accounting's own tracked value is {old_value} -- the plain interpreter and this accounting have desynced"
            );
        }
        // A read re-produces the value at a fresh timestamp too (so a
        // later op can reference *this* read as its own prior
        // reference); a write obviously changes both. Either way the
        // value itself only ever changes on a write.
        self.addr_state.insert(addr_key, (value, new_ts));

        MemOpWitness {
            needs_init,
            init_val,
            addr,
            value,
            is_write,
            old_value,
            old_ts,
            new_ts,
        }
    }

    /// Emit each touched storage's own `MemoryCheckState` declaration --
    /// call once, with `out` targeting text placed *before* the runtime
    /// loop (these are the running accumulators the loop mutates every
    /// iteration, so they must persist across iterations like
    /// `mem_probe.rs`'s own `mem2`/`mem33`).
    pub fn emit_pre_loop_decls(&self, out: &mut String) {
        for name in self.storages.values() {
            out.push_str(&format!(
                "let mut {name} = MemoryCheckState::<Gf128>::new(key.clone());\n"
            ));
        }
    }

    /// Emit one trace-entry position's own call-site text -- call once
    /// per position (matching `crate::vole::MemoryTrace::entries`'
    /// index), with `out` targeting the loop body. `witness_expr` is a
    /// Rust expression evaluating to that position's own `MemOpWitness`
    /// for the current iteration (e.g. `"witness[step].mem_ops[3]"`).
    pub fn emit_call_site(
        &self,
        out: &mut String,
        storage_id: u32,
        type_id: u32,
        witness_expr: &str,
    ) {
        let local = &self.storages[&(storage_id, type_id)];
        out.push_str(&format!(
            "if {witness_expr}.needs_init {{ {local}.init(Gf128::from_u64({witness_expr}.addr), Gf128::from_u64({witness_expr}.init_val)); }}\n"
        ));
        out.push_str(&format!(
            "if {witness_expr}.is_write {{ {local}.write(Gf128::from_u64({witness_expr}.addr), Gf128::from_u64({witness_expr}.value), {witness_expr}.new_ts, Gf128::from_u64({witness_expr}.old_value), {witness_expr}.old_ts); }} else {{ {local}.read(Gf128::from_u64({witness_expr}.addr), Gf128::from_u64({witness_expr}.value), {witness_expr}.new_ts, {witness_expr}.old_ts); }}\n"
        ));
    }

    /// After every step has been processed: drain every address ever
    /// touched, emit a `verify()` assertion per storage, and return
    /// `(h_produce_expr, h_consume_expr)` -- Rust array-literal
    /// expressions (`[s0_produce, s1_produce, ...]`, one element per
    /// storage in stable `(storage_id, type_id)` order) ready to feed
    /// `prove_and_verify_iop`'s own `mem_acc_in`/`mem_acc_out`. Called
    /// once, host-side, after the loop -- literal embedding here is
    /// fine, it's O(addresses touched), not O(steps).
    pub fn finish(&mut self, out: &mut String) -> (String, String) {
        let mut by_storage: BTreeMap<(u32, u32), Vec<(u64, (u64, u64))>> = BTreeMap::new();
        for (&(sid, tid, addr), &(val, ts)) in &self.addr_state {
            by_storage
                .entry((sid, tid))
                .or_default()
                .push((addr, (val, ts)));
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
