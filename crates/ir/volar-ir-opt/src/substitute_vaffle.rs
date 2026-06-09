// @reliability: experimental
// @ai: assisted
//! Substitution of oracle, action, and RNG declarations in VAFFLE modules.
//!
//! Each substitution replaces every call site for a named oracle/action/RNG
//! with a `Value::Call` to a replacement function imported from another
//! `Module`.  The replacement module's types, sigs, functions, and nested
//! declarations are merged into the host module before rewriting.

use alloc::{
    collections::BTreeMap,
    string::String,
    vec,
    vec::Vec,
};
use vaffle::{FuncDecl, FuncId, Module, SigDecl, Value};
use volar_ir_common::{Stmt, StorageAllocator, TypeRemapper};

/// One substitution entry for a VAFFLE module.
///
/// The replacement is a full [`Module`].  Its entry function is located via
/// `replacement.exports["entry"]`, or falls back to the sole `FuncDecl::Body`
/// if there is only one and no export is set.
pub enum VaffleSubstitution {
    Oracle { name: String, replacement: Module },
    Action { name: String, replacement: Module },
    Rng    { name: String, replacement: Module },
}

impl VaffleSubstitution {
    fn name(&self) -> &str {
        match self {
            VaffleSubstitution::Oracle { name, .. } => name,
            VaffleSubstitution::Action { name, .. } => name,
            VaffleSubstitution::Rng    { name, .. } => name,
        }
    }

    fn replacement(&self) -> &Module {
        match self {
            VaffleSubstitution::Oracle { replacement, .. } => replacement,
            VaffleSubstitution::Action { replacement, .. } => replacement,
            VaffleSubstitution::Rng    { replacement, .. } => replacement,
        }
    }
}

/// Apply all substitutions to `module`, replacing oracle/action/RNG call sites
/// with direct function calls to the replacement bodies.
///
/// Returns the number of call sites rewritten.
pub fn substitute_vaffle(module: &mut Module, subs: &[VaffleSubstitution]) -> usize {
    let mut total = 0;
    for sub in subs {
        total += apply_one(module, sub);
    }
    total
}

fn apply_one(module: &mut Module, sub: &VaffleSubstitution) -> usize {
    let repl = sub.replacement();

    // ── 1. Merge type tables ─────────────────────────────────────────────────
    let tr = TypeRemapper::merge(&mut module.types, &repl.types);

    // ── 2. Merge nested declarations (oracles, actions) ─────────────────────
    for mut decl in repl.oracles.iter().cloned() {
        tr.remap_oracle_decl(&mut decl);
        if !module.oracles.iter().any(|d| d.name == decl.name) {
            module.oracles.push(decl);
        }
    }
    for mut decl in repl.actions.iter().cloned() {
        tr.remap_action_decl(&mut decl);
        if !module.actions.iter().any(|d| d.name == decl.name) {
            module.actions.push(decl);
        }
    }

    // ── 3. Remap and add replacement functions ───────────────────────────────
    // Map guest SigId → host SigId.
    let mut sig_map: Vec<vaffle::SigId> = Vec::with_capacity(repl.sigs.len());
    for sig in &repl.sigs {
        let new_sig = SigDecl {
            params:  sig.params.iter().map(|&t| tr.remap(t)).collect(),
            results: sig.results.iter().map(|&t| tr.remap(t)).collect(),
        };
        let new_id = vaffle::SigId(module.sigs.len());
        module.sigs.push(new_sig);
        sig_map.push(new_id);
    }

    // Map guest FuncId → host FuncId.
    let func_base = module.funcs.len();
    let mut func_map: Vec<FuncId> = Vec::with_capacity(repl.funcs.len());
    for (gi, _) in repl.funcs.iter().enumerate() {
        func_map.push(FuncId(func_base + gi));
    }

    for func in &repl.funcs {
        let new_func = match func {
            FuncDecl::Import { module: m, name: n, sig } => FuncDecl::Import {
                module: m.clone(),
                name: n.clone(),
                sig: sig_map[sig.0],
            },
            FuncDecl::Body(body) => {
                let new_sig = sig_map[body.sig.0];
                let new_values: Vec<Value> = body.values.iter().map(|v| {
                    remap_value(v, &tr, &sig_map, &func_map)
                }).collect();
                let new_blocks: Vec<vaffle::Block> = body.blocks.iter().map(|b| {
                    vaffle::Block {
                        params: b.params.iter().map(|(vid, tid)| (*vid, tr.remap(*tid))).collect(),
                        stmts: b.stmts.clone(),
                        stmt_provs: b.stmt_provs.clone(),
                        terminator: b.terminator.clone(),
                    }
                }).collect();
                FuncDecl::Body(vaffle::FuncBody {
                    sig: new_sig,
                    blocks: new_blocks,
                    values: new_values,
                    entry: body.entry,
                })
            }
            _ => panic!("substitute_vaffle: unhandled FuncDecl variant — add handling for this variant"),
        };
        module.funcs.push(new_func);
    }

    // ── 4. Find entry FuncId in host ─────────────────────────────────────────
    let entry_func_id = find_entry(repl, &func_map);

    // ── 5. Rewrite call sites in pre-existing bodies ─────────────────────────
    let name = sub.name();
    let mut count = 0;
    for fi in 0..func_base {
        if let FuncDecl::Body(body) = &mut module.funcs[fi] {
            count += rewrite_body(body, sub, name, entry_func_id);
        }
    }
    count
}

/// Find the entry [`FuncId`] (in the host's remapped range) for a replacement module.
fn find_entry(repl: &Module, func_map: &[FuncId]) -> FuncId {
    if let Some(&guest_fid) = repl.exports.get("entry") {
        return func_map[guest_fid.0];
    }
    // Fall back: the sole Body func.
    let mut body_idx = None;
    for (i, f) in repl.funcs.iter().enumerate() {
        if matches!(f, FuncDecl::Body(_)) {
            body_idx = Some(i);
        }
    }
    func_map[body_idx.expect("replacement module has no Body function and no 'entry' export")]
}

/// Rewrite one function body, replacing matching call stmts with `Value::Call`.
fn rewrite_body(
    body: &mut vaffle::FuncBody,
    sub: &VaffleSubstitution,
    name: &str,
    entry_func_id: FuncId,
) -> usize {
    let mut count = 0;
    // First pass: find OracleCall/ActionCall/Rng sites and their ValueIds.
    // We need to track which ValueIds were OracleCall/ActionCall so we can
    // rewrite downstream OracleOutput/ActionOutput.
    let mut replaced_calls: BTreeMap<usize, ()> = BTreeMap::new();

    for (vi, value) in body.values.iter_mut().enumerate() {
        let Value::Op(ref stmt) = *value else { continue };
        let rewrite = match (sub, stmt) {
            (VaffleSubstitution::Oracle { .. }, Stmt::OracleCall { name: n, args, .. })
                if n == name =>
            {
                Some(Value::Call { func: entry_func_id, args: args.clone() })
            }
            (VaffleSubstitution::Action { .. }, Stmt::ActionCall { name: n, guard, args, .. })
                if n == name =>
            {
                let mut call_args = vec![*guard];
                call_args.extend_from_slice(args);
                Some(Value::Call { func: entry_func_id, args: call_args })
            }
            (VaffleSubstitution::Rng { .. }, Stmt::Rng { name: n, .. })
                if n == name =>
            {
                Some(Value::Call { func: entry_func_id, args: Vec::new() })
            }
            _ => None,
        };
        if let Some(new_val) = rewrite {
            *value = new_val;
            replaced_calls.insert(vi, ());
            count += 1;
        }
    }

    // Second pass: rewrite OracleOutput/ActionOutput for replaced calls.
    for value in body.values.iter_mut() {
        let Value::Op(ref stmt) = *value else { continue };
        let rewrite = match stmt {
            Stmt::OracleOutput { call, idx, .. }
                if replaced_calls.contains_key(&call.0) =>
            {
                Some(Value::Output { value: *call, idx: *idx })
            }
            Stmt::ActionOutput { call, idx, .. }
                if replaced_calls.contains_key(&call.0) =>
            {
                Some(Value::Output { value: *call, idx: *idx })
            }
            _ => None,
        };
        if let Some(new_val) = rewrite {
            *value = new_val;
        }
    }

    count
}

/// Clone and remap a single [`Value`] from the replacement module into the host.
fn remap_value(
    v: &Value,
    tr: &TypeRemapper,
    _sig_map: &[vaffle::SigId],
    func_map: &[FuncId],
) -> Value {
    match v {
        Value::Param { block, ty, idx } => Value::Param {
            block: *block,
            ty: tr.remap(*ty),
            idx: *idx,
        },
        Value::Call { func, args } => Value::Call {
            func: func_map[func.0],
            args: args.clone(),
        },
        Value::Output { value, idx } => Value::Output { value: *value, idx: *idx },
        Value::Op(stmt) => {
            let mut s = stmt.clone();
            tr.remap_stmt_types(&mut s);
            Value::Op(s)
        }
        Value::StackAlloc { elem_ty, count, base_slot } => Value::StackAlloc {
            elem_ty: tr.remap(*elem_ty),
            count: *count,
            base_slot: *base_slot,
        },
        Value::PtrLoad { ptr, pointee_ty } => Value::PtrLoad {
            ptr: *ptr,
            pointee_ty: tr.remap(*pointee_ty),
        },
        Value::PtrStore { ptr, val } => Value::PtrStore { ptr: *ptr, val: *val },
        Value::PtrOffset { ptr, idx, elem_bits } => Value::PtrOffset {
            ptr: *ptr,
            idx: *idx,
            elem_bits: *elem_bits,
        },
        _ => panic!("remap_value: unhandled Value variant — add remapping for this variant"),
    }
}

/// Build a [`StorageAllocator`] seeded above all `StorageId`s in use in `module`.
pub fn vaffle_storage_allocator(module: &Module) -> StorageAllocator {
    let mut max = 63u32; // never issue IDs in the reserved range [0..64)
    for func in &module.funcs {
        if let FuncDecl::Body(body) = func {
            for v in &body.values {
                if let Value::Op(Stmt::StorageRead { storage, .. })
                    | Value::Op(Stmt::StorageWrite { storage, .. }) = v
                {
                    if storage.0 > max { max = storage.0; }
                }
            }
        }
    }
    StorageAllocator::new(max + 1)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use std::collections::BTreeMap;
    use alloc::{string::ToString, vec};
    use vaffle::{Block, BlockId, FuncBody, FuncDecl, Module, SigDecl, Terminator, Value, ValueId};
    use volar_ir_common::{Constant, IrType, OracleDecl, Stmt, Type, TypeTable};
    use super::{substitute_vaffle, VaffleSubstitution};

    fn empty_module_with_types() -> Module {
        Module {
            types: TypeTable::new(),
            oracles: vec![],
            actions: vec![],
            funcs: vec![],
            sigs: vec![],
            exports: BTreeMap::new(),
            pre_init: vec![],
        }
    }

    /// Build a minimal host module containing one oracle call.
    ///
    /// Program structure:
    ///   block 0: params=[], stmts=[OracleCall{hash,args:[]}], terminator=Return{[v0]}
    ///            (+ OracleOutput{call:v0,idx:0} as v1, then return v1)
    fn host_with_oracle_call() -> (Module, vaffle::SigId) {
        let mut m = empty_module_with_types();
        let u64_ty = m.types.primitive(Type::_64);
        m.oracles.push(OracleDecl {
            name: "hash".to_string(),
            params: vec![],
            results: vec![u64_ty],
        });

        // sig: () -> u64
        let sig_id = vaffle::SigId(0);
        m.sigs.push(SigDecl { params: vec![], results: vec![u64_ty] });

        // values:
        //   v0 = OracleCall("hash", args=[])
        //   v1 = OracleOutput(call=v0, idx=0)
        let result_ty = m.types.intern(IrType::Tuple(vec![u64_ty]));
        let v0 = ValueId(0);
        let values = vec![
            Value::Op(Stmt::OracleCall {
                name: "hash".to_string(),
                args: vec![],
                output_tys: vec![u64_ty],
                result_ty,
            }),
            Value::Op(Stmt::OracleOutput { call: v0, idx: 0, ty: u64_ty }),
        ];
        let block = Block {
            params: vec![],
            stmts: vec![ValueId(0), ValueId(1)],
            stmt_provs: vec![(), ()],
            terminator: Terminator::Return { values: vec![ValueId(1)] },
        };
        m.funcs.push(FuncDecl::Body(FuncBody {
            sig: sig_id,
            blocks: vec![block],
            values,
            entry: BlockId(0),
        }));
        (m, sig_id)
    }

    /// Build a replacement module whose entry function returns a constant 42u64.
    fn replacement_const42() -> Module {
        let mut m = empty_module_with_types();
        let u64_ty = m.types.primitive(Type::_64);
        let sig_id = vaffle::SigId(0);
        m.sigs.push(SigDecl { params: vec![], results: vec![u64_ty] });
        let values = vec![
            Value::Op(Stmt::Const(Constant { hi: 0, lo: 42 }, u64_ty)),
        ];
        let block = Block {
            params: vec![],
            stmts: vec![ValueId(0)],
            stmt_provs: vec![()],
            terminator: Terminator::Return { values: vec![ValueId(0)] },
        };
        let entry_fid = vaffle::FuncId(0);
        m.funcs.push(FuncDecl::Body(FuncBody {
            sig: sig_id,
            blocks: vec![block],
            values,
            entry: BlockId(0),
        }));
        m.exports.insert("entry".to_string(), entry_fid);
        m
    }

    #[test]
    fn oracle_call_replaced_with_value_call() {
        let (mut host, _) = host_with_oracle_call();
        let repl = replacement_const42();
        let subs = [VaffleSubstitution::Oracle {
            name: "hash".to_string(),
            replacement: repl,
        }];
        let count = substitute_vaffle(&mut host, &subs);
        assert_eq!(count, 1);

        // The host function body should contain no more OracleCall stmts.
        if let FuncDecl::Body(body) = &host.funcs[0] {
            for v in &body.values {
                if let Value::Op(Stmt::OracleCall { name, .. }) = v {
                    panic!("OracleCall to '{name}' was not substituted");
                }
            }
            // At least one Value::Call must exist.
            let has_call = body.values.iter().any(|v| matches!(v, Value::Call { .. }));
            assert!(has_call, "expected a Value::Call after substitution");
        } else {
            panic!("expected FuncDecl::Body");
        }
    }

    #[test]
    fn oracle_output_replaced_with_value_output() {
        let (mut host, _) = host_with_oracle_call();
        let repl = replacement_const42();
        let subs = [VaffleSubstitution::Oracle {
            name: "hash".to_string(),
            replacement: repl,
        }];
        substitute_vaffle(&mut host, &subs);

        if let FuncDecl::Body(body) = &host.funcs[0] {
            for v in &body.values {
                assert!(
                    !matches!(v, Value::Op(Stmt::OracleOutput { .. })),
                    "OracleOutput should have been rewritten to Value::Output"
                );
            }
        }
    }

    #[test]
    fn rng_two_sites_both_replaced() {
        let mut m = empty_module_with_types();
        let u64_ty = m.types.primitive(Type::_64);
        m.sigs.push(SigDecl { params: vec![], results: vec![u64_ty, u64_ty] });
        let values = vec![
            Value::Op(Stmt::Rng { name: "rand".to_string(), ty: u64_ty }),
            Value::Op(Stmt::Rng { name: "rand".to_string(), ty: u64_ty }),
        ];
        let block = Block {
            params: vec![],
            stmts: vec![ValueId(0), ValueId(1)],
            stmt_provs: vec![(), ()],
            terminator: Terminator::Return { values: vec![ValueId(0), ValueId(1)] },
        };
        m.funcs.push(FuncDecl::Body(FuncBody {
            sig: vaffle::SigId(0),
            blocks: vec![block],
            values,
            entry: BlockId(0),
        }));

        let repl = replacement_const42();
        let subs = [VaffleSubstitution::Rng {
            name: "rand".to_string(),
            replacement: repl,
        }];
        let count = substitute_vaffle(&mut m, &subs);
        assert_eq!(count, 2, "both Rng sites should be substituted");

        if let FuncDecl::Body(body) = &m.funcs[0] {
            for v in &body.values {
                assert!(
                    !matches!(v, Value::Op(Stmt::Rng { .. })),
                    "Rng should have been rewritten to Value::Call"
                );
            }
            let call_count = body.values.iter().filter(|v| matches!(v, Value::Call { .. })).count();
            assert_eq!(call_count, 2, "expected two Value::Call nodes");
        }
    }

    #[test]
    fn all_typeids_valid_after_substitution() {
        let (mut host, _) = host_with_oracle_call();
        let repl = replacement_const42();
        let subs = [VaffleSubstitution::Oracle {
            name: "hash".to_string(),
            replacement: repl,
        }];
        substitute_vaffle(&mut host, &subs);

        let n_types = host.types.0.len();
        if let FuncDecl::Body(body) = &host.funcs[0] {
            for v in &body.values {
                match v {
                    Value::Param { ty, .. } => assert!((ty.0 as usize) < n_types),
                    Value::StackAlloc { elem_ty, .. } => assert!((elem_ty.0 as usize) < n_types),
                    Value::PtrLoad { pointee_ty, .. } => assert!((pointee_ty.0 as usize) < n_types),
                    _ => {}
                }
            }
        }
    }

    #[test]
    fn replacement_body_present_in_module() {
        let (mut host, _) = host_with_oracle_call();
        let orig_func_count = host.funcs.len();
        let repl = replacement_const42();
        let subs = [VaffleSubstitution::Oracle {
            name: "hash".to_string(),
            replacement: repl,
        }];
        substitute_vaffle(&mut host, &subs);
        assert!(
            host.funcs.len() > orig_func_count,
            "replacement function should have been appended"
        );
    }
}
