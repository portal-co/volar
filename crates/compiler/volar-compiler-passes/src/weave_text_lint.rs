// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! `weave_text_lint` — source lint for the IR-not-text rule (AGENTS.md Core
//! Design Rule 13; plan: `docs/ir-not-text-weaving-plan.md`).
//!
//! A `syn`-based source scan (the same mechanism family as
//! [`crate::vec_lint`]'s parse-time exemption scan). The rule has two halves:
//!
//! 1. **Weaver output is IR, not text.** Production code must not consume a
//!    woven program via the `print_*` text emitters destined for `rustc`
//!    (W1); a weaver function must not *return* text (W2); and the
//!    print→`Command::new("cargo"|"rustc")` pipeline shape is flagged (W3).
//! 2. **No weaving pass operates on text.** A function in the compiler
//!    workspace that takes program text (`&str`/`String`) and returns
//!    `String` is a text-level weaving pass (W5) — the
//!    `nested_block_chunk` shape. Weaving passes are pure IR→IR transforms.
//!
//! Exemptions follow the `@volar-allow-vec` precedent:
//!
//! ```text
//! /// @volar-allow-rust-text: <test-fixture | diagnostic | ts-target | migration-in-progress>: <reason>
//! ```
//!
//! on the item, an enclosing `mod`, or the file (`//!`). Unrecognized
//! categories fail closed. `#[cfg(test)]` modules and `tests/` paths are
//! skipped entirely.

#[cfg(feature = "parsing")]
use std::string::String;
#[cfg(feature = "parsing")]
use std::vec::Vec;
#[cfg(feature = "parsing")]
use std::format;
#[cfg(feature = "parsing")]
use quote::ToTokens;

/// The exemption categories (AGENTS.md rule 13).
pub const ALLOW_TEXT_CATEGORIES: [&str; 5] = [
    "test-fixture",
    "diagnostic",
    "ts-target",
    "migration-in-progress",
    // Spec-source preprocessing ahead of the parser (e.g. attribute
    // stripping); not woven-program output.
    "source-preprocessing",
];

/// Weaver text emitters whose production call sites are violations (W1).
/// Keep in sync with `volar-weaver`'s exported `print_*` surface; the
/// drift-guard test in `volar-weaver` enforces it.
pub const WEAVER_TEXT_EMITTERS: [&str; 10] = [
    "print_fhe_flat_module",
    "print_fhe_cfg_module",
    "print_fhe_cfg_module_ts",
    "print_weaved_module",
    "print_weaved_vole_module",
    "print_weaved_faest_module",
    "print_net_vole_module",
    "print_net_vole_cfg_module",
    "print_hybrid_net_cfg_module",
    "print_glue_module",
];

/// One violation.
#[cfg(feature = "parsing")]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WeaveTextError {
    /// File path.
    pub file: String,
    /// Dotted item path (e.g. `my_mod::my_fn`).
    pub item: String,
    /// Rule id (W1/W2/W3/W5).
    pub rule: &'static str,
    pub note: String,
}

/// Extract the `@volar-allow-rust-text:` category from `syn` attributes.
/// Fails closed on unrecognized categories.
#[cfg(feature = "parsing")]
pub fn parse_allow_text(attrs: &[syn::Attribute]) -> Option<&'static str> {
    for attr in attrs {
        if !attr.path().is_ident("doc") {
            continue;
        }
        let doc_text: String = match &attr.meta {
            syn::Meta::NameValue(nv) => {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = &nv.value
                {
                    s.value()
                } else {
                    continue;
                }
            }
            _ => continue,
        };
        let text = doc_text.trim();
        if let Some(rest) = text.strip_prefix("@volar-allow-rust-text:") {
            let rest = rest.trim();
            let category = rest.split(':').next().unwrap_or("").trim();
            return ALLOW_TEXT_CATEGORIES.iter().find(|c| **c == category).copied();
        }
    }
    None
}

/// Recognize an `@volar-allow-rust-text:` category, failing closed.
#[cfg(feature = "parsing")]
fn allow_text_category(category: &str) -> Option<&'static str> {
    ALLOW_TEXT_CATEGORIES
        .iter()
        .find(|c| **c == category)
        .copied()
}

/// Extract the `@volar-allow-rust-text:` category from plain doc strings
/// (accepts both `//!`-stripped and raw `///`/`//!` forms).
#[cfg(feature = "parsing")]
pub fn allow_text_category_in_docs<'a>(
    docs: impl IntoIterator<Item = &'a str>,
) -> Option<&'static str> {
    for doc in docs {
        let text = doc.trim();
        let text = text
            .strip_prefix("//!")
            .or_else(|| text.strip_prefix("///"))
            .map(str::trim_start)
            .unwrap_or(text);
        if let Some(rest) = text.strip_prefix("@volar-allow-rust-text:") {
            let rest = rest.trim();
            let category = rest.split(':').next().unwrap_or("").trim();
            return allow_text_category(category);
        }
    }
    None
}

/// File-level exemption from leading `//!` doc lines (category-validated).
#[cfg(feature = "parsing")]
pub fn source_is_exempt(source: &str) -> bool {
    allow_text_category_in_docs(
        source
            .lines()
            .take_while(|l| {
                let t = l.trim_start();
                t.starts_with("//!") || t.starts_with("//") || t.is_empty()
            })
            .filter(|l| l.trim_start().starts_with("//!"))
            .map(|l| l.trim_start()),
    )
    .is_some()
}

#[cfg(feature = "parsing")]
struct Ctx<'a> {
    file: &'a str,
    errors: Vec<WeaveTextError>,
    /// Item path stack for readable reports.
    path: Vec<String>,
}

#[cfg(feature = "parsing")]
impl Ctx<'_> {
    fn flag(&mut self, rule: &'static str, note: impl Into<String>) {
        self.errors.push(WeaveTextError {
            file: self.file.into(),
            item: self.path.join("::"),
            rule,
            note: note.into(),
        });
    }
}

/// Lint one Rust source file. `is_test_path` skips standalone test files.
#[cfg(feature = "parsing")]
pub fn lint_source(source: &str, path: &str) -> Vec<WeaveTextError> {
    if source_is_exempt(source) {
        return Vec::new();
    }
    let file = match syn::parse_file(source) {
        Ok(f) => f,
        Err(_) => return Vec::new(), // parse errors are the compiler's job
    };
    let mut ctx = Ctx {
        file: path,
        errors: Vec::new(),
        path: Vec::new(),
    };
    walk_items(&file.items, false, false, &mut ctx);
    ctx.errors
}

#[cfg(feature = "parsing")]
fn item_is_cfg_test(attrs: &[syn::Attribute]) -> bool {
    // Matches `#[cfg(test)]` and nested forms like
    // `#[cfg(all(test, feature = "linking"))]`.
    attrs.iter().any(|a| {
        if !a.path().is_ident("cfg") {
            return false;
        }
        syn::Meta::to_token_stream(&a.meta).to_string().contains("test")
    })
}

#[cfg(feature = "parsing")]
fn walk_items(items: &[syn::Item], in_test: bool, exempt: bool, ctx: &mut Ctx) {
    for item in items {
        let item_exempt = exempt || parse_allow_text(item_attrs(item)).is_some();
        let item_test = in_test || item_is_cfg_test(item_attrs(item));
        match item {
            syn::Item::Mod(m) => {
                ctx.path.push(m.ident.to_string());
                if let Some((_, items)) = &m.content {
                    walk_items(items, item_test, item_exempt, ctx);
                }
                ctx.path.pop();
            }
            syn::Item::Fn(f) => {
                ctx.path.push(f.sig.ident.to_string());
                if !item_test && !item_exempt {
                    check_fn(f, ctx);
                }
                ctx.path.pop();
            }
            _ => {}
        }
    }
}

#[cfg(feature = "parsing")]
fn item_attrs(item: &syn::Item) -> &[syn::Attribute] {
    match item {
        syn::Item::Fn(f) => &f.attrs,
        syn::Item::Mod(m) => &m.attrs,
        syn::Item::Struct(s) => &s.attrs,
        syn::Item::Enum(e) => &e.attrs,
        syn::Item::Const(c) => &c.attrs,
        syn::Item::Type(t) => &t.attrs,
        syn::Item::Impl(i) => &i.attrs,
        _ => &[],
    }
}

/// Does this type mention `String` (as an owned text type)?
#[cfg(feature = "parsing")]
fn type_is_stringish(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(tp) => tp
            .path
            .segments
            .last()
            .is_some_and(|s| s.ident == "String"),
        syn::Type::Reference(r) => type_is_stringish(&r.elem),
        _ => false,
    }
}

#[cfg(feature = "parsing")]
fn type_is_str_ref(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(r) => matches!(&*r.elem, syn::Type::Path(tp) if tp.path.segments.last().is_some_and(|s| s.ident == "str")),
        syn::Type::Path(tp) => tp
            .path
            .segments
            .last()
            .is_some_and(|s| s.ident == "String"),
        _ => false,
    }
}

#[cfg(feature = "parsing")]
fn check_fn(f: &syn::ItemFn, ctx: &mut Ctx) {
    let name = f.sig.ident.to_string();

    // W2: a weaver function returning text.
    if name.starts_with("weave_") {
        if let syn::ReturnType::Type(_, ty) = &f.sig.output {
            if type_is_stringish(ty) {
                ctx.flag(
                    "W2",
                    format!(
                        "weaver `{name}` returns String; weavers must return IR (IrModule/IrCfgModule)"
                    ),
                );
            }
        }
    }

    // W5: a text-level weaving pass: takes *program* text (param named
    // `source`/`text`/`code`/`body`/`program`), returns String. Param-name
    // narrowing keeps identifier manglers (`mangle`, `escape_ts_reserved`)
    // out of scope; those are printer internals, not program transforms.
    let takes_text = f.sig.inputs.iter().any(|arg| {
        if let syn::FnArg::Typed(pt) = arg {
            let name_is_program_text = matches!(&*pt.pat, syn::Pat::Ident(pi) if
                matches!(pi.ident.to_string().as_str(),
                    "source" | "text" | "code" | "body" | "program"));
            name_is_program_text && type_is_str_ref(&pt.ty)
        } else {
            false
        }
    });
    if takes_text {
        if let syn::ReturnType::Type(_, ty) = &f.sig.output {
            if type_is_stringish(ty) {
                ctx.flag(
                    "W5",
                    format!(
                        "`{name}` takes program text and returns String: text-level weaving passes are forbidden; operate on IR"
                    ),
                );
            }
        }
    }

    // Expression-level checks (W1, W3) walk the body.
    check_expr_block(&f.block, ctx);
}

#[cfg(feature = "parsing")]
struct ExprVisitor<'a, 'b> {
    ctx: &'a mut Ctx<'b>,
}

#[cfg(feature = "parsing")]
impl syn::visit::Visit<'_> for ExprVisitor<'_, '_> {
    fn visit_expr_call(&mut self, call: &syn::ExprCall) {
        // W1: calls to weaver text emitters (path or method-call shape).
        // Calls from within another `print_*` emitter are the sanctioned
        // test-render path (emitters may compose) and are not flagged.
        let in_emitter = self
            .ctx
            .path
            .last()
            .is_some_and(|n| n.starts_with("print_"));
        if let syn::Expr::Path(p) = &*call.func {
            if let Some(seg) = p.path.segments.last() {
                let fname = seg.ident.to_string();
                if !in_emitter && WEAVER_TEXT_EMITTERS.contains(&fname.as_str()) {
                    self.ctx.flag(
                        "W1",
                        format!(
                            "call to weaver text emitter `{fname}` in production code; consume the woven IR via lower_module/lower_cfg_module instead"
                        ),
                    );
                }
                // W3: Command::new("cargo"/"rustc") — the print→rustc shape.
                if fname == "new" && p.path.segments.len() >= 2 {
                    let qual = &p.path.segments[p.path.segments.len() - 2].ident;
                    if qual == "Command" {
                        if let Some(syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(s),
                            ..
                        })) = call.args.first()
                        {
                            let tool = s.value();
                            if tool == "cargo" || tool == "rustc" {
                                self.ctx.flag(
                                    "W3",
                                    format!(
                                        "spawns `{tool}` from production code (print→rustc pipeline shape); compile the woven IR via an LirTarget instead"
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }
        syn::visit::visit_expr_call(self, call);
    }

    fn visit_expr_method_call(&mut self, m: &syn::ExprMethodCall) {
        let mname = m.method.to_string();
        let in_emitter = self
            .ctx
            .path
            .last()
            .is_some_and(|n| n.starts_with("print_"));
        if !in_emitter && WEAVER_TEXT_EMITTERS.contains(&mname.as_str()) {
            self.ctx.flag(
                "W1",
                format!(
                    "method call to weaver text emitter `{mname}` in production code"
                ),
            );
        }
        syn::visit::visit_expr_method_call(self, m);
    }
}

#[cfg(feature = "parsing")]
fn check_expr_block(block: &syn::Block, ctx: &mut Ctx) {
    use syn::visit::Visit;
    let mut v = ExprVisitor { ctx };
    v.visit_block(block);
}

#[cfg(all(test, feature = "parsing"))]
mod tests {
    use super::*;

    #[test]
    fn flags_emitter_calls_in_production_code() {
        let src = r#"
pub fn pipeline(module: &IrModule) -> String {
    let code = print_weaved_vole_module(module);
    std::fs::write("out.rs", &code).unwrap();
    code
}
"#;
        let errors = lint_source(src, "src/x.rs");
        assert!(errors.iter().any(|e| e.rule == "W1"), "errors: {errors:?}");
    }

    #[test]
    fn flags_text_returning_weaver_and_text_pass() {
        let src = r#"
pub fn weave_bad(c: &BIrBlocks) -> String { String::new() }
pub fn chunk_function_bodies(source: &str, chunk_size: usize) -> String { source.to_string() }
"#;
        let errors = lint_source(src, "src/x.rs");
        assert!(errors.iter().any(|e| e.rule == "W2"), "errors: {errors:?}");
        assert!(errors.iter().any(|e| e.rule == "W5"), "errors: {errors:?}");
    }

    #[test]
    fn flags_cargo_rustc_spawn() {
        let src = r#"
pub fn build_it(code: &str) {
    std::fs::write("gen.rs", code).unwrap();
    let _ = std::process::Command::new("cargo").arg("check").output();
}
"#;
        let errors = lint_source(src, "src/x.rs");
        assert!(errors.iter().any(|e| e.rule == "W3"), "errors: {errors:?}");
    }

    #[test]
    fn cfg_test_modules_are_skipped() {
        let src = r#"
#[cfg(test)]
mod tests {
    pub fn helper() -> String {
        print_weaved_vole_module(&x())
    }
}
"#;
        assert!(lint_source(src, "src/x.rs").is_empty());
    }

    #[test]
    fn exemptions_apply_at_item_and_file_level() {
        let src = r#"
/// @volar-allow-rust-text: test-fixture: compile-check harness
pub fn compile_check(code: &str) -> String {
    let _ = std::process::Command::new("cargo").output();
    code.to_string()
}
"#;
        assert!(lint_source(src, "src/x.rs").is_empty());

        let file_src = "//! Module docs.\n//! @volar-allow-rust-text: migration-in-progress: wat_gen\npub fn pipeline() -> String { print_weaved_vole_module(&m()) }";
        assert!(lint_source(file_src, "src/x.rs").is_empty());
        // Unrecognized category fails closed.
        let bad = "//! @volar-allow-rust-text: whatever: nope\npub fn pipeline() -> String { print_weaved_vole_module(&m()) }";
        assert!(!lint_source(bad, "src/x.rs").is_empty());
    }

    #[test]
    fn ir_returning_weavers_are_clean() {
        let src = r#"
pub fn weave_vole_verifier(c: &BIrBlocks) -> IrCfgModule { todo!() }
pub fn print_weaved_vole_module(module: &IrModule<IrFunction>) -> String { String::new() }
"#;
        assert!(lint_source(src, "src/x.rs").is_empty());
    }
}
