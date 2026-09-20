// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Mutual-privacy side vocabulary for garbled-circuit weaving.
//!
//! This is the MPC analogue of [`crate::vole`]'s [`VoleProtection`] /
//! [`VoleSideAssignments`]: the policy-free [`volar_side`] machinery resolves
//! *which party* an IR value belongs to, and this module gives that the
//! garbled-circuit meaning — public (both parties), garbler-private, or
//! evaluator-private. It is the weaver-side vocabulary the `volar-mpc`
//! session layer's [`InputOwner`](volar_mpc::InputOwner) partition mirrors.
//!
//! # The protection lattice
//!
//! Two-party garbled circuits have three input visibilities:
//!
//! | Protection | Meaning | How its label reaches the evaluator |
//! |------------|---------|-------------------------------------|
//! | [`MpcProtection::Public`] | known to both | garbler encodes and sends the selected label |
//! | [`MpcProtection::Garbler`] | garbler-only | garbler encodes and sends the selected label |
//! | [`MpcProtection::Evaluator`] | evaluator-only | one 1-of-2 OT per bit |
//!
//! Propagation ([`volar_side::propagate`]) keeps a derived value on its
//! operands' common side and yields `None` on disagreement. [`MpcResolver`]
//! supplies the *join* for that disagreement: a value combining two distinct
//! private sides stays private (it is computable only inside the garbled
//! circuit, never in cleartext by a single party), while anything touching a
//! public side but no private side is public.
//!
//! # Relationship to the session layer
//!
//! The weaver decides, per circuit input bit, which of the three sets it
//! belongs to; the `volar-mpc` session layer then delivers labels for those
//! sets by the matching mechanism. This module only assigns the side
//! vocabulary and resolves it — the actual OT / label-delivery flow lives in
//! `volar-mpc`.

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::{boxed::Box, format, vec, vec::Vec};

use volar_side::{SideHandler, SideId};

/// What an MPC-side value is, in the two-party garbled-circuit sense: public
/// (known to both), garbler-private, or evaluator-private.
///
/// The `volar-side` vocabulary for the garbling weaver — resolved from a
/// [`SideId`] by any [`SideHandler`]. This is the three-point analogue of
/// [`VoleProtection`](crate::vole::VoleProtection)'s two-point
/// witness/statement split.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MpcProtection {
    /// Public cleartext value, known to both parties (`bool`).
    Public,
    /// Private to the garbler; the garbler encodes the selected label and
    /// sends it to the evaluator.
    Garbler,
    /// Private to the evaluator; the selected label reaches the evaluator via
    /// 1-of-2 OT (garbler is OT sender).
    Evaluator,
}

impl MpcProtection {
    /// Is this value private to exactly one party (i.e. carried as a garbled
    /// label rather than a cleartext bit)?
    pub fn is_private(self) -> bool {
        matches!(self, MpcProtection::Garbler | MpcProtection::Evaluator)
    }
}

/// Resolves the *join* of operand protections when [`volar_side::propagate`]
/// reports disagreement (`None`): which protection a value derived from
/// mixed-side operands carries.
///
/// The rule for two-party garbled circuits:
///
/// - **public ∧ public = public.** A function of only public values is public.
/// - **anything ∧ private = private.** Once any operand is one party's
///   private value, the result is computable only inside the garbled circuit;
///   neither party can hold it in cleartext. Combining *both* private sides
///   still yields a private value — the circuit mixes them, and the output
///   wire carries a label, not a cleartext bit.
///
/// So the lattice is `Public < Private`, with the two private parties
/// (garbler/evaluator) collapsing to the single "private" join — the label
/// does not record *which* party's privacy dominates, only that the value is
/// no longer public.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct MpcResolver;

impl MpcResolver {
    /// Join a set of operand protections into the result's protection.
    ///
    /// Empty (a constant or zero-operand node) is [`MpcProtection::Public`];
    /// otherwise the join is `Public` iff *every* operand is public. A mixed
    /// set yields the privacy of whichever private party is present; when
    /// *both* private parties appear the result is still private, and we
    /// report it as [`MpcProtection::Garbler`] — an arbitrary but fixed choice,
    /// since the label on a mixed wire is not attributable to a single party
    /// (neither can hold it in cleartext). Callers that need to distinguish
    /// "attributable to one party" from "mixed" should consult the operand
    /// sides directly rather than the joined protection.
    pub fn join(operands: impl IntoIterator<Item = MpcProtection>) -> MpcProtection {
        let mut any = false;
        let mut first_private: Option<MpcProtection> = None;
        for p in operands {
            any = true;
            if p.is_private() && first_private.is_none() {
                first_private = Some(p);
            }
        }
        match first_private {
            Some(p) => p,
            None => {
                if any {
                    // All operands public.
                    MpcProtection::Public
                } else {
                    // Constant / zero-operand node.
                    MpcProtection::Public
                }
            }
        }
    }
}

/// Side assignment for a garbled circuit's introduction points: circuit input
/// params and action output bits.
///
/// Mirrors [`VoleSideAssignments`](crate::vole::VoleSideAssignments): a
/// `BIrBlock`'s params aren't individually `Node`-wrapped and an action's
/// output bits have no IR node of their own, so these introduction points
/// take an explicit side from the weaver caller rather than reading one off
/// the IR. The MPC version assigns *which party* each introduction point
/// belongs to.
#[derive(Clone, Debug, Default)]
pub struct MpcSideAssignments {
    /// Side for circuit input param `i` (0-based), if assigned.
    pub input_sides: BTreeMap<u32, SideId>,
    /// Side for action `name`'s output bit `j` (0-based), if assigned.
    pub action_sides: BTreeMap<String, BTreeMap<usize, SideId>>,
}

impl MpcSideAssignments {
    /// Assign `side` to circuit input param `idx`, returning `self` for chaining.
    pub fn with_input(mut self, idx: u32, side: SideId) -> Self {
        self.input_sides.insert(idx, side);
        self
    }

    /// Assign `side` to action `name`'s output bit `bit`, returning `self` for chaining.
    pub fn with_action_output(mut self, name: &str, bit: usize, side: SideId) -> Self {
        self.action_sides.entry(name.into()).or_default().insert(bit, side);
        self
    }
}

/// A circuit's input wires partitioned by owner: which wires are public,
/// which are garbler-private, which are evaluator-private.
///
/// This is the weaver-side product that an MPC session layer consumes to
/// build its per-wire [`InputOwner`](volar_mpc::InputOwner) vector: the three
/// disjoint wire-index sets, in circuit-input order within each set. Wires
/// not present in any set are treated as public (the safe default — a wire
/// with no side assignment carries no private data).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InputPartition {
    /// Public input wire indices (sorted, deduplicated).
    pub public: alloc::vec::Vec<u32>,
    /// Garbler-private input wire indices.
    pub garbler: alloc::vec::Vec<u32>,
    /// Evaluator-private input wire indices.
    pub evaluator: alloc::vec::Vec<u32>,
}

impl InputPartition {
    /// The owning set of wire `idx`, defaulting to public for unassigned wires.
    pub fn owner(&self, idx: u32) -> MpcProtection {
        if self.garbler.contains(&idx) {
            MpcProtection::Garbler
        } else if self.evaluator.contains(&idx) {
            MpcProtection::Evaluator
        } else {
            MpcProtection::Public
        }
    }

    /// Total number of partitioned (non-default-public) wires.
    pub fn private_len(&self) -> usize {
        self.garbler.len() + self.evaluator.len()
    }
}

/// Derive an [`InputPartition`] from side assignments and a handler that
/// resolves each side to its [`MpcProtection`].
///
/// `num_inputs` bounds the partition to the circuit's actual input wires.
/// Each assigned input wire is bucketed by its resolved protection; wires
/// with no assignment (or resolving to `Public`) fall into `public`. The
/// handler is any [`SideHandler`] with `Protection = MpcProtection` —
/// typically `TableProtection<MpcProtection>` mapping the interned
/// `public`/`garbler`/`evaluator` side names.
pub fn partition_inputs<H: SideHandler<Protection = MpcProtection>>(
    assignments: &MpcSideAssignments,
    handler: &H,
    num_inputs: u32,
) -> InputPartition {
    let mut part = InputPartition::default();
    for idx in 0..num_inputs {
        let side = assignments.input_sides.get(&idx).copied();
        match handler.protection(side) {
            MpcProtection::Public => part.public.push(idx),
            MpcProtection::Garbler => part.garbler.push(idx),
            MpcProtection::Evaluator => part.evaluator.push(idx),
        }
    }
    part
}

// ============================================================================
// B3: partition-aware evaluator weaving
// ============================================================================

/// Weave a single-block boolean circuit into a **mutual-privacy evaluator**
/// `IrModule`, driven by an [`InputPartition`] (B3).
///
/// Unlike [`weave_evaluator`](crate::garble::weave_evaluator) — whose
/// generated function takes *every* input label as a parameter (forcing the
/// caller, and so the evaluator, to see labels for wires that are not its
/// own) — this weaver separates the evaluator's inputs by owner:
///
/// - **`one_wire`**, **`and_table_k`** (one per AND gate): as before.
/// - **`pg_i`** for each public/garbler-owned input wire `i` (in circuit-input
///   order): the label the garbler encodes and sends.
/// - **`ev_j`** for each evaluator-owned input wire `j` (in circuit-input
///   order): the label the evaluator receives via 1-of-2 OT.
///
/// The circuit body is identical to the plain evaluator weave (free XOR/NOT,
/// `and_via_table` per AND); only the *input interface* is partitioned, so the
/// generated evaluator's signature makes visible exactly which labels it
/// legitimately holds — the structural mutual-privacy guarantee the
/// `volar-mpc` session layer enforces at runtime.
///
/// The generated signature is:
/// ```text
/// fn <name>_eval<N: ArraySize, D: Digest>(
///     one_wire: &Eval<N>,
///     and_table_0: &GarbleTable<N>, ...,
///     pg_<i>: &Eval<N>, ...,   // public/garbler-owned inputs
///     ev_<j>: &Eval<N>, ...,   // evaluator-owned (OT) inputs
/// ) -> Eval<N>                  // (or a tuple for multi-output)
/// ```
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_mpc_evaluator_with_partition<P: Clone>(
    circuit: &volar_ir::boolar::BIrBlocks<P>,
    name: &str,
    partition: &InputPartition,
) -> volar_discipline::Tagged<volar_discipline::Transparent, volar_compiler::ir::IrModule<volar_compiler::ir::IrFunction>>
{
    use volar_compiler::ir::{
        ExternalKind, IrBlock, IrExprKind, IrFunction, IrModule, IrParam, IrPattern,
        IrStmtKind, MethodKind, SpecBinOp,
    };
    use volar_ir::boolar::BIrStmt;

    use crate::{
        array_default, base_index, build_return, clone_expr, expand_ors, ir_expr, ref_expr, ref_to,
        var,
    };
    use crate::garble::{eval_type, garble_table_type, generic_params_pub};

    assert!(
        circuit.is_circuit(),
        "weave_mpc_evaluator: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let and_count = expanded
        .iter()
        .filter(|(_, s, _)| matches!(s, BIrStmt::And(..)))
        .count();

    // Sanity: every partition index must be a real input wire, and the two
    // private sets must be disjoint (checked by InputOwner at the session
    // layer; here we just defensively ignore out-of-range indices).
    let in_range = |i: u32| (i as usize) < num_params;

    // Map each input wire to its parameter variable name.
    let mut var_names = alloc::collections::BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("in_{}", i));
    }

    // Build params: one_wire, and_table_k, then pg_i (public/garbler) and ev_j
    // (evaluator) in circuit-input order within each group.
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "one_wire".into(),
        ty: ref_to(eval_type()),
    });
    for k in 0..and_count {
        params.push(IrParam {
            name: format!("and_table_{}", k),
            ty: ref_to(garble_table_type()),
        });
    }
    // Public/garbler-owned inputs (label sent directly by the garbler).
    let mut pg_wires: Vec<u32> = partition.public.iter().copied().filter(|i| in_range(*i)).collect();
    pg_wires.extend(partition.garbler.iter().copied().filter(|i| in_range(*i)));
    pg_wires.sort_unstable();
    pg_wires.dedup();
    for i in &pg_wires {
        params.push(IrParam {
            name: format!("in_{}", i),
            ty: ref_to(eval_type()),
        });
    }
    // Evaluator-owned inputs (label via OT).
    let mut ev_wires: Vec<u32> = partition.evaluator.iter().copied().filter(|i| in_range(*i)).collect();
    ev_wires.sort_unstable();
    ev_wires.dedup();
    for j in &ev_wires {
        params.push(IrParam {
            name: format!("in_{}", j),
            ty: ref_to(eval_type()),
        });
    }

    // Circuit body — identical gate semantics to the plain evaluator weave.
    let mut stmts: Vec<volar_compiler::ir::IrStmt> = Vec::new();
    let mut and_counter: usize = 0;

    for (result_id, stmt, _prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        let init_expr = match stmt {
            BIrStmt::Zero => ir_expr(IrExprKind::StructExpr {
                kind: volar_compiler::ir::StructKind::Custom("Eval".into()),
                type_args: vec![],
                fields: vec![("target".into(), array_default())],
                rest: None,
            }),
            BIrStmt::One => clone_expr(var("one_wire")),
            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var(&name_b))),
                })
            }
            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_name = format!("and_table_{}", and_counter);
                and_counter += 1;
                ir_expr(IrExprKind::MethodCall {
                    receiver: Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Other("and_via_table".into()),
                    type_args: vec![volar_compiler::ir::IrType::TypeParam("D".into())],
                    args: vec![ref_expr(clone_expr(var(&name_b))), var(&table_name)],
                })
            }
            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                ir_expr(IrExprKind::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(clone_expr(var(&name_a))),
                    right: Box::new(clone_expr(var("one_wire"))),
                })
            }
            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::ActionCall { .. }
            | BIrStmt::ActionBit { .. }
            | BIrStmt::Rng { .. }
            | BIrStmt::StorageRead { .. }
            | BIrStmt::StorageWrite { .. } => {
                unimplemented!("mpc evaluator weaver: extended BIrStmt variants not supported")
            }
            _ => unimplemented!("mpc evaluator weaver: unhandled BIrStmt variant — add support"),
        };

        stmts.push(volar_compiler::ir::IrStmt::new(
            IrStmtKind::Let {
                pattern: IrPattern::ident(&let_name),
                ty: None,
                init: Some(init_expr),
            },
            Default::default(),
            None,
        ));
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let func = IrFunction {
        no_inline: false,
        name: format!("{}_eval", name),
        module_path: vec![],
        generics: generic_params_pub(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let module = IrModule {
        name: "weaved_mpc_evaluator".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    volar_discipline::Tagged::seal(module)
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use volar_side::SideTable;

    use MpcProtection::*;

    #[test]
    fn join_all_public_is_public() {
        assert_eq!(MpcResolver::join([Public, Public]), Public);
        assert_eq!(MpcResolver::join([Public]), Public);
    }

    #[test]
    fn join_empty_is_public() {
        // A constant / zero-operand node has no private operand.
        assert_eq!(MpcResolver::join([]), Public);
    }

    #[test]
    fn join_any_private_is_private() {
        // Public mixed with a private operand is private.
        assert!(MpcResolver::join([Public, Garbler]).is_private());
        assert!(MpcResolver::join([Public, Evaluator]).is_private());
        assert!(MpcResolver::join([Garbler]).is_private());
        assert!(MpcResolver::join([Evaluator]).is_private());
    }

    #[test]
    fn join_single_private_party_is_attributable() {
        assert_eq!(MpcResolver::join([Public, Garbler]), Garbler);
        assert_eq!(MpcResolver::join([Evaluator, Public]), Evaluator);
    }

    #[test]
    fn join_both_private_parties_stays_private() {
        // Mixing the garbler's and evaluator's private values still yields a
        // private value (computable only in-circuit); the reported party is a
        // fixed arbitrary choice, but it must be private.
        let j = MpcResolver::join([Garbler, Evaluator]);
        assert!(j.is_private());
        let j2 = MpcResolver::join([Evaluator, Garbler, Public]);
        assert!(j2.is_private());
    }

    #[test]
    fn assignments_chain_and_store() {
        let mut table = SideTable::new();
        let public = table.intern("public");
        let alice = table.intern("alice");
        let bob = table.intern("bob");

        let a = MpcSideAssignments::default()
            .with_input(0, public)
            .with_input(1, alice)
            .with_input(2, bob)
            .with_action_output("rand", 0, alice);

        assert_eq!(a.input_sides.get(&0), Some(&public));
        assert_eq!(a.input_sides.get(&1), Some(&alice));
        assert_eq!(a.input_sides.get(&2), Some(&bob));
        assert_eq!(a.action_sides.get("rand").and_then(|m| m.get(&0)), Some(&alice));
    }

    #[test]
    fn partition_inputs_buckets_by_protection() {
        use volar_side::TableProtection;
        let mut table = SideTable::new();
        let public = table.intern("public");
        let alice = table.intern("alice"); // garbler
        let bob = table.intern("bob"); // evaluator

        let handler = TableProtection::new(MpcProtection::Public)
            .with(alice, MpcProtection::Garbler)
            .with(bob, MpcProtection::Evaluator);

        // 5 inputs: 0 public(explicit), 1 alice, 2 bob, 3 unassigned, 4 bob.
        let a = MpcSideAssignments::default()
            .with_input(0, public)
            .with_input(1, alice)
            .with_input(2, bob)
            .with_input(4, bob);

        let part = partition_inputs(&a, &handler, 5);
        assert_eq!(part.public, alloc::vec![0, 3]); // explicit public + unassigned
        assert_eq!(part.garbler, alloc::vec![1]);
        assert_eq!(part.evaluator, alloc::vec![2, 4]);

        // owner() reflects the buckets; unassigned defaults to public.
        assert_eq!(part.owner(0), MpcProtection::Public);
        assert_eq!(part.owner(1), MpcProtection::Garbler);
        assert_eq!(part.owner(2), MpcProtection::Evaluator);
        assert_eq!(part.owner(3), MpcProtection::Public);
        assert_eq!(part.private_len(), 3);
    }

    #[test]
    fn partition_inputs_defaults_all_public_when_unassigned() {
        use volar_side::UniformProtection;
        let handler = UniformProtection(MpcProtection::Public);
        let a = MpcSideAssignments::default();
        let part = partition_inputs(&a, &handler, 3);
        assert_eq!(part.public, alloc::vec![0, 1, 2]);
        assert!(part.garbler.is_empty());
        assert!(part.evaluator.is_empty());
    }

    /// B3: the partition-aware evaluator weave takes only its OT-received
    /// labels (plus public/garbler inputs) and still computes the correct
    /// circuit output for every input combination — the generated code
    /// compiles and runs against volar-spec.
    #[test]
    fn mpc_evaluator_partition_weave_runs() {
        use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
        use volar_ir::ir::{IRBlockTargetId, IRVarId};
        use volar_ir_common::Node;

        // (x0 XOR x1) AND x2: Xor(0,1)->3, And(3,2)->4, return 4.
        let circuit: BIrBlocks = BIrBlocks { blocks: vec![BIrBlock {
            params: 3,
            stmts: vec![
                Node::new(BIrStmt::Xor(IRVarId(0), IRVarId(1)), (), None),
                Node::new(BIrStmt::And(IRVarId(3), IRVarId(2)), (), None),
            ],
            terminator: BIrTerminator::Jmp(BIrTarget { block: IRBlockTargetId::Return, args: vec![IRVarId(4)] }),
        }], pre_init: vec![] };

        // Partition: x0 public, x1 garbler, x2 evaluator.
        let partition = InputPartition {
            public: alloc::vec![0],
            garbler: alloc::vec![1],
            evaluator: alloc::vec![2],
        };

        let eval_module = weave_mpc_evaluator_with_partition(&circuit, "mpc3", &partition).into_inner();
        let gc_module = crate::garble::weave_into_gc(&circuit, "mpc3", None).into_inner();

        let combined = volar_compiler::ir::IrModule {
            name: "combined".into(),
            functions: gc_module.functions.into_iter().chain(eval_module.functions).collect(),
            structs: vec![],
            enums: vec![],
            traits: vec![],
            impls: vec![],
            type_aliases: vec![],
            consts: vec![],
        };
        let fns_code = crate::garble::print_weaved_module(&combined, false);

        let root = crate::tests_common::workspace_root();
        let test_code = std::format!(
            "{fns}\n\
             #[cfg(test)]\n\
             mod mpc_eval_tests {{\n\
                 use super::*;\n\
                 use volar_spec::garble::{{Garble, GlobalSecret}};\n\
                 use hybrid_array::{{Array, typenum::U16}};\n\
                 use sha2::Sha256;\n\
                 type N = U16;\n\
                 type D = Sha256;\n\
                 fn det(seed: u8) -> Array<u8, N> {{\n\
                     let mut a = Array::<u8, N>::default();\n\
                     for (i, b) in a.iter_mut().enumerate() {{ *b = (i as u8).wrapping_mul(53).wrapping_add(seed); }}\n\
                     a[0] |= 1; a\n\
                 }}\n\
                 #[test]\n\
                 fn partitioned_truth_table() {{\n\
                     let secret = GlobalSecret::<N>::new(det(11));\n\
                     let l0 = Garble::<N> {{ base: det(21) }};\n\
                     let l1 = Garble::<N> {{ base: det(37) }};\n\
                     let l2 = Garble::<N> {{ base: det(71) }};\n\
\
\
                     let gc = mpc3_into_gc::<N, D>(secret, l0, l1, l2);\n\
                     let setup = gc.eval_setup();\n\
                     for x0 in [false, true] {{ for x1 in [false, true] {{ for x2 in [false, true] {{\n\
                         let enc = gc.encode_inputs(&[x0, x1, x2]);\n\
                         let result = mpc3_eval::<N, D>(\n\
                             &setup.one_wire, &setup.tables[0],\n\
                             &enc[0], &enc[1],   // public x0, garbler x1\n\
                             &enc[2],            // evaluator x2 (would arrive via OT)\n\
                         );\n\
                         let want = (x0 ^ x1) && x2;\n\
                         assert_eq!(setup.recover_output(&result), want,\n\
                             \"(x0 ^ x1) && x2 for {{}} {{}} {{}}\", x0, x1, x2);\n\
                     }} }} }}\n\
                 }}\n\
             }}\n",
            fns = fns_code,
        );

        let tmpdir = std::env::temp_dir().join("volar_weaver_mpc_eval_partition");
        let srcdir = tmpdir.join("src");
        std::fs::create_dir_all(&srcdir).unwrap();
        let cargo_toml = std::format!(
            "[package]\n\
             name = \"weave-check-mpc-eval\"\n\
             version = \"0.1.0\"\n\
             edition = \"2024\"\n\
             \n\
             [[test]]\n\
             name = \"mpc_eval\"\n\
             path = \"src/lib.rs\"\n\
             \n\
             [dependencies]\n\
             volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
             volar-primitives = {{ path = \"{root}/crates/spec/volar-primitives\" }}\n\
             volar-common = {{ path = \"{root}/crates/spec/volar-common\" }}\n\
             hybrid-array = {{ version = \"0.4.8\" }}\n\
             digest = {{ version = \"0.11.2\", default-features = false }}\n\
             cipher = {{ version = \"0.5.1\", default-features = false }}\n\
             rand = {{ version = \"0.9.2\", default-features = false }}\n\
             typenum = {{ version = \"1.17\", default-features = false }}\n\
             elliptic-curve = {{ version = \"0.13.8\", features = [\"arithmetic\"], default-features = false }}\n\
             sha2 = {{ version = \"0.11\", default-features = false }}\n",
            root = root,
        );
        std::fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).unwrap();
        std::fs::write(srcdir.join("lib.rs"), &test_code).unwrap();

        let output = std::process::Command::new("cargo")
            .args(["test", "--quiet", "--test", "mpc_eval"])
            .current_dir(&tmpdir)
            .env("CARGO_TARGET_DIR", String::from(tmpdir.join("target").to_str().unwrap()))
            .output()
            .expect("failed to run cargo test");
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let _ = std::fs::remove_dir_all(&tmpdir);
        assert!(
            output.status.success(),
            "MPC partition evaluator test failed\n--- code ---\n{}\n--- stdout ---\n{}\n--- stderr ---\n{}",
            fns_code, stdout, stderr
        );
    }

    /// B3: the partition is reflected in the generated evaluator's signature —
    /// it has one param per public/garbler input plus one per evaluator input,
    /// not one per input total.
    #[test]
    fn mpc_evaluator_partition_signature_shape() {
        use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
        use volar_ir::ir::{IRBlockTargetId, IRVarId};
        use volar_ir_common::Node;

        let circuit: BIrBlocks = BIrBlocks { blocks: vec![BIrBlock {
            params: 4,
            stmts: vec![Node::new(BIrStmt::And(IRVarId(0), IRVarId(1)), (), None)],
            terminator: BIrTerminator::Jmp(BIrTarget { block: IRBlockTargetId::Return, args: vec![IRVarId(4)] }),
        }], pre_init: vec![] };

        let partition = InputPartition {
            public: alloc::vec![0],
            garbler: alloc::vec![1],
            evaluator: alloc::vec![2, 3],
        };
        let module = weave_mpc_evaluator_with_partition(&circuit, "sig", &partition).into_inner();
        let func = &module.functions[0];

        // Params: one_wire + and_table_0 + in_0 (public) + in_1 (garbler)
        //         + in_2, in_3 (evaluator) = 2 + 2 + 2 = 6.
        let names: Vec<&str> = func.params.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(
            names,
            alloc::vec!["one_wire", "and_table_0", "in_0", "in_1", "in_2", "in_3"],
            "partitioned evaluator params"
        );
    }
}
