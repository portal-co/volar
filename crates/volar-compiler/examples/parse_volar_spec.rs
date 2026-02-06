//! Example demonstrating parsing volar-spec and printing the IR.

use std::fs;
use std::path::Path;
use volar_compiler::{
    parse_sources, print_module,
    TypeContext, OperatorAnalysis, type_to_string,
    StructKind, TraitKind, MathTrait, ArrayKind,
    IrType, IrExpr, IrStmt, IrImplItem, AssociatedType,
};

fn main() {
    let base_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("volar-spec")
        .join("src");

    let mut sources = Vec::new();

    fn collect_rs_files(dir: &Path, sources: &mut Vec<(String, String)>) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() && path.extension().is_some_and(|e| e == "rs") {
                    if let Ok(content) = fs::read_to_string(&path) {
                        let name = path.file_stem().unwrap().to_string_lossy().to_string();
                        sources.push((content, name));
                    }
                } else if path.is_dir() {
                    collect_rs_files(&path, sources);
                }
            }
        }
    }

    collect_rs_files(&base_path, &mut sources);

    println!("Found {} source files in volar-spec", sources.len());
    
    let sources_ref: Vec<(&str, &str)> = sources
        .iter()
        .map(|(content, name)| (content.as_str(), name.as_str()))
        .collect();

    match parse_sources(&sources_ref, "volar_spec") {
        Ok(module) => {
            println!("\n=== IR Module Statistics ===");
            println!("Structs: {}", module.structs.len());
            println!("Traits: {}", module.traits.len());
            println!("Impls: {}", module.impls.len());
            println!("Functions: {}", module.functions.len());
            println!("Type aliases: {}", module.type_aliases.len());

            // Analyze operators
            let op_analysis = OperatorAnalysis::from_module(&module);
            println!("\n=== Operator Implementations ===");
            for ((trait_name, self_ty, rhs_ty), output_ty) in &op_analysis.binary_ops {
                println!(
                    "  {} {} {} = {}",
                    self_ty,
                    trait_name,
                    rhs_ty,
                    type_to_string(output_ty)
                );
            }

            // Build type context
            let type_ctx = TypeContext::from_module(&module);
            println!("\n=== Type Context ===");
            println!("Known structs: {}", type_ctx.structs.len());
            println!("Trait implementations: {}", type_ctx.trait_impls.len());
            println!("Associated types: {}", type_ctx.assoc_types.len());

            println!("\n=== Specialized Struct Classifications ===");
            for s in &module.structs {
                let classification = match &s.kind {
                    StructKind::Custom(name) => match name.as_str() {
                        "Delta" | "Q" | "Vope" | "BitVole" => "VOLE",
                        "ABO" | "ABOOpening" | "CommitmentCore" => "Crypto",
                        "Poly" | "PolyInputPool" => "Polynomial",
                        _ => "Other",
                    },
                    _ => "Other",
                };
                println!("  {:?}: {} ({} fields)", s.kind, classification, s.fields.len());
                
                // Show field types
                for field in &s.fields {
                    let ty_desc = match &field.ty {
                        IrType::Primitive(p) => format!("primitive {:?}", p),
                        IrType::Array { kind: ArrayKind::GenericArray, .. } => "GenericArray".to_string(),
                        IrType::Array { kind: ArrayKind::FixedArray, .. } => "fixed array".to_string(),
                        IrType::TypeParam(name) => format!("type param {}", name),
                        IrType::Struct { kind, .. } => format!("struct {:?}", kind),
                        _ => "other".to_string(),
                    };
                    println!("    {}: {}", field.name, ty_desc);
                }
            }
            
            println!("\n=== Specialized Trait Impl Classifications ===");
            let mut math_count = 0;
            let mut crypto_count = 0;
            let mut inherent_count = 0;
            
            for imp in &module.impls {
                match &imp.trait_ {
                    Some(tr) => match &tr.kind {
                        TraitKind::Math(m) => {
                            math_count += 1;
                            if matches!(m, MathTrait::Add | MathTrait::Sub | MathTrait::Mul | MathTrait::BitXor) {
                                println!("  Math op: {:?} for {:?}", m, type_to_string(&imp.self_ty));
                            }
                        }
                        TraitKind::Crypto(c) => {
                            crypto_count += 1;
                            println!("  Crypto: {:?} for {:?}", c, type_to_string(&imp.self_ty));
                        }
                        _ => {}
                    },
                    None => {
                        inherent_count += 1;
                    }
                }
            }
            
            println!("\n  Math trait impls: {}", math_count);
            println!("  Crypto trait impls: {}", crypto_count);
            println!("  Inherent impls: {}", inherent_count);

            // Count total loop constructs
            println!("\n=== Total (Bounded) Loop Analysis ===");
            
            fn count_loops_in_expr(expr: &IrExpr, counts: &mut (usize, usize, usize, usize, usize)) {
                match expr {
                    IrExpr::ArrayGenerate { body, .. } => {
                        counts.0 += 1;
                        count_loops_in_expr(body, counts);
                    }
                    IrExpr::IterPipeline(chain) => {
                        counts.1 += 1;
                        // Walk into chain step/terminal bodies
                        match &chain.source {
                            volar_compiler::IterChainSource::Method { collection, .. } => {
                                count_loops_in_expr(collection, counts);
                            }
                            volar_compiler::IterChainSource::Range { start, end, .. } => {
                                count_loops_in_expr(start, counts);
                                count_loops_in_expr(end, counts);
                            }
                            volar_compiler::IterChainSource::Zip { .. } => {}
                        }
                        for step in &chain.steps {
                            match step {
                                volar_compiler::IterStep::Map { body, .. }
                                | volar_compiler::IterStep::Filter { body, .. }
                                | volar_compiler::IterStep::FilterMap { body, .. }
                                | volar_compiler::IterStep::FlatMap { body, .. } => {
                                    count_loops_in_expr(body, counts);
                                }
                                _ => {}
                            }
                        }
                        match &chain.terminal {
                            volar_compiler::IterTerminal::Fold { init, body, .. } => {
                                count_loops_in_expr(init, counts);
                                count_loops_in_expr(body, counts);
                            }
                            _ => {}
                        }
                    }
                    IrExpr::BoundedLoop { body, .. } => {
                        counts.2 += 1;
                        for stmt in &body.stmts {
                            if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                                count_loops_in_expr(e, counts);
                            }
                        }
                        if let Some(e) = &body.expr {
                            count_loops_in_expr(e, counts);
                        }
                    }
                    IrExpr::IterLoop { body, collection, .. } => {
                        counts.3 += 1;
                        count_loops_in_expr(collection, counts);
                        for stmt in &body.stmts {
                            if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                                count_loops_in_expr(e, counts);
                            }
                        }
                        if let Some(e) = &body.expr {
                            count_loops_in_expr(e, counts);
                        }
                    }
                    IrExpr::Block(block) => {
                        for stmt in &block.stmts {
                            if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                                count_loops_in_expr(e, counts);
                            }
                        }
                        if let Some(e) = &block.expr {
                            count_loops_in_expr(e, counts);
                        }
                    }
                    IrExpr::MethodCall { receiver, args, .. } => {
                        count_loops_in_expr(receiver, counts);
                        for arg in args {
                            count_loops_in_expr(arg, counts);
                        }
                    }
                    IrExpr::Call { func, args, .. } => {
                        count_loops_in_expr(func, counts);
                        for arg in args {
                            count_loops_in_expr(arg, counts);
                        }
                    }
                    IrExpr::Closure { body, .. } => {
                        count_loops_in_expr(body, counts);
                    }
                    IrExpr::If { cond, then_branch, else_branch } => {
                        count_loops_in_expr(cond, counts);
                        for stmt in &then_branch.stmts {
                            if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                                count_loops_in_expr(e, counts);
                            }
                        }
                        if let Some(e) = &then_branch.expr {
                            count_loops_in_expr(e, counts);
                        }
                        if let Some(eb) = else_branch {
                            count_loops_in_expr(eb, counts);
                        }
                    }
                    _ => {}
                }
            }
            
            let mut counts = (0, 0, 0, 0, 0);
            for imp in &module.impls {
                for item in &imp.items {
                    if let IrImplItem::Method(f) = item {
                        for stmt in &f.body.stmts {
                            if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                                count_loops_in_expr(e, &mut counts);
                            }
                        }
                        if let Some(e) = &f.body.expr {
                            count_loops_in_expr(e, &mut counts);
                        }
                    }
                }
            }
            for f in &module.functions {
                for stmt in &f.body.stmts {
                    if let IrStmt::Semi(e) | IrStmt::Expr(e) = stmt {
                        count_loops_in_expr(e, &mut counts);
                    }
                }
                if let Some(e) = &f.body.expr {
                    count_loops_in_expr(e, &mut counts);
                }
            }
            
            println!("  ArrayGenerate: {}", counts.0);
            println!("  IterPipeline: {}", counts.1);
            println!("  BoundedLoop: {}", counts.2);
            println!("  IterLoop: {}", counts.3);
            println!("  Total: {}", counts.4);
            println!("\n  Total bounded loops: {} (all loops are provably terminating)", 
                     counts.0 + counts.1 + counts.2 + counts.3 + counts.4);

            // Print the IR (truncated)
            println!("\n=== IR Output (truncated) ===");
            let printed = print_module(&module);
            
            if printed.len() > 2000 {
                println!("{}...\n", &printed[..2000]);
            } else {
                println!("{}", printed);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse: {}", e);
            std::process::exit(1);
        }
    }
}
