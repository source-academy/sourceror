mod dep_graph;
// mod dep_extract;
mod attributes;
mod builtins;
mod error;
mod estree;
mod extensions;
mod frontendvar;
mod func;
mod import_name_resolver;
mod importer;
mod parse_state;

use async_trait::async_trait;
use error::*;
use extensions::IntoSourceLocation;
use frontendvar::*;
use ir;
use projstd::log::CompileMessage;
use projstd::log::LogErr;
use projstd::log::Logger;
use projstd::log::Severity;
use projstd::log::SourceLocationRef as plSLRef;
use serde_json;
use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::marker::Send;
use std::result::Result;

use estree::*;

pub type ProgramPreExports = VarCtx<String, VarValue<VarLocId, Box<[ir::VarType]>>>;
pub type ParseState = parse_state::ParseState;
// note: ProgramExports is kept in increasing order of VarLocId (in its natural ordering)

// Storage for stuff needed when later doing additional compilation for the repl
pub type ReplContext = (HashMap<String, PreVar>, ParseState, usize);

enum SourceItem {
    ESTree(estree::Node),
    ImportSpec(importer::ImportSpec),
}

#[derive(Copy, Clone)]
struct SourceFetcher<F> {
    raw_fetch: F,
}
//#[async_trait(?Send)]
impl<Fut: Future<Output = Option<String>>, F: 'static + Copy + FnOnce(String) -> Fut>
    dep_graph::Fetcher<SourceItem> for SourceFetcher<F>
{
    fn fetch<'a>(
        self,
        name: &'a str,
        sl: plSLRef<'a>,
    ) -> std::pin::Pin<
        Box<dyn 'a + Future<Output = Result<SourceItem, CompileMessage<FetcherError>>>>,
    > {
        Box::pin((|| async move {
            (self.raw_fetch)(name.to_owned()).await.map_or_else(
                || {
                    Err(
                        CompileMessage::new_error(sl.to_owned(), FetchError::new(name.to_owned()))
                            .into_cm(),
                    )
                },
                |estree_str: String| {
                    if importer::has_imports_header(estree_str.as_str()) {
                        // this is an imports file
                        importer::parse_imports(name, estree_str.as_str())
                            .map(|import_spec| SourceItem::ImportSpec(import_spec))
                            .map_err(|e| e.into_cm())
                    } else {
                        serde_json::from_str(estree_str.as_str())
                            .map(|estree_node| SourceItem::ESTree(estree_node))
                            .map_err(|_| {
                                CompileMessage::new_error(
                                    plSLRef::entire_file(Some(name)).to_owned(),
                                    ESTreeParseError {},
                                )
                                .into_cm()
                            })
                    }
                },
            )
        })())
    }
}

impl<'a> dep_graph::ExtractDeps<'a> for SourceItem {
    // todo! Change `dyn Iterator` to some compile-time thing when Rust gets impl Traits support for traits.
    type Iter = Box<dyn Iterator<Item = (import_name_resolver::ResolveIter, plSLRef<'a>)> + 'a>;
    fn extract_deps(&'a self, filename: Option<&'a str>) -> Self::Iter {
        match self {
            SourceItem::ESTree(es_node) => Box::new(
                (match &es_node.kind {
                    NodeKind::Program(es_program) => es_program.body.as_slice(),
                    _ => &[],
                })
                .iter()
                .filter_map(move |es_node| {
                    if let NodeKind::ImportDeclaration(ImportDeclaration {
                        specifiers: _,
                        source,
                    }) = &es_node.kind
                    {
                        if let NodeKind::Literal(Literal {
                            value: LiteralValue::String(s),
                        }) = &source.kind
                        {
                            return Some((
                                import_name_resolver::resolve(s.as_str(), filename),
                                source.loc.into_sl(filename),
                            ));
                        }
                    }
                    None
                }),
            ),
            SourceItem::ImportSpec(import_spec) => Box::new(std::iter::empty()),
        }
    }
}

/// Main entry point for the frontend.  Call this and everything will work.
pub async fn run_frontend<
    L: Logger,
    F: 'static + Copy + FnOnce(String) -> Fut,
    Fut: Future<Output = Option<String>>,
>(
    estree_str: String,
    raw_fetch: F,
    logger: L,
) -> Result<(ReplContext, ir::Program), ()> {
    // parse the given string as estree
    let es_program: estree::Node = serde_json::from_str(estree_str.as_str())
        .map_err(|_| {
            CompileMessage::new_error(plSLRef::entire_file(None).to_owned(), ESTreeParseError {})
        })
        .log_err(&logger)?;

    // fetch and parse all the import files
    let dep_graph = dep_graph::Graph::try_async_build_from_root(
        SourceItem::ESTree(es_program),
        SourceFetcher::<F> {
            raw_fetch: raw_fetch,
        },
    )
    .await
    .log_err(&logger)?;

    // find all the FFI imports first
    // (because ir imports must come before all other functions in the ir_program)
    let mut imports: Vec<ir::Import> = dep_graph
        .topological_traverse()
        .filter_map(|(source_item, _)| {
            if let SourceItem::ImportSpec(import_spec) = source_item {
                Some(import_spec)
            } else {
                None
            }
        })
        .flat_map(|import_spec| import_spec.content.iter().map(|(name, import)| import))
        .cloned()
        .collect();

    // sort and deduplicate the imports so we don't have duplicated imports
    // note: we can import the same module+entity pair under multiple signatures, this is allowed in ir and wasm
    imports.sort_unstable();
    imports.dedup();

    // keep a map from import to funcidx, so that we can use it later
    let import_funcidx_map: HashMap<ir::Import, ir::FuncIdx> = imports
        .iter()
        .enumerate()
        .map(|(i, import)| (import.clone(), i))
        .collect();

    // construct the ir_program with the given imports
    let mut ir_program = ir::Program::new_with_imports(imports.into_boxed_slice());
    let mut ir_toplevel_sequence: Vec<ir::Expr> = Vec::new();

    // parse all the source files in topological order
    //let default_state: compact_state::CompactState<compact_state::FrontendVar> =
    //    builtins::state_with_builtins();
    // contains builtins, e.g. __string_to_number(), and __undefined.
    // The builtins are encoded as string, e.g. "+", "-", etc, and are all Direct
    // the mapping is in builtins module, there is a special transformation for unary minus to avoid name clash
    // todo: also add the automatic imports
    let mut start_idx = 0;
    let (name_ctx, parse_state): (HashMap<String, PreVar>, ParseState) =
        builtins::state_with_builtins(&mut start_idx, &mut ir_program);
    // We act as if every global in the main program (i.e. the main file) is exported,
    // so when we compile additional stuff from the REPL later, it is as if we just imported the main program.
    let (main_name_ctx, main_parse_state): (HashMap<String, PreVar>, ParseState) = dep_graph
        .topological_traverse_state_into(
            |i, deps, source_item, filename, (start_idx, ir_program, ir_toplevel_sequence)| {
                match source_item {
                    SourceItem::ESTree(es_program) => func::parse_dep_program(
                        &name_ctx,
                        &parse_state,
                        es_program,
                        deps,
                        start_idx,
                        filename,
                        i,
                        ir_program,
                        ir_toplevel_sequence,
                    )
                    .map_err(|cm| {
                        logger.log(cm);
                    }),
                    SourceItem::ImportSpec(import_spec) => {
                        assert!(deps.is_empty(), "Import spec should be empty");
                        Ok(importer::make_export_state(
                            import_spec,
                            i,
                            &import_funcidx_map,
                        ))
                    }
                }
            },
            |i, deps, source_item, filename, (start_idx, ir_program, ir_toplevel_sequence)| {
                match source_item {
                    SourceItem::ESTree(es_program) => func::parse_main_program(
                        &name_ctx,
                        &parse_state,
                        es_program,
                        deps,
                        start_idx,
                        filename,
                        i,
                        ir_program,
                        ir_toplevel_sequence,
                    )
                    .map_err(|cm| {
                        logger.log(cm);
                    }),
                    SourceItem::ImportSpec(import_spec) => {
                        panic!("Main program cannot be ImportSpec")
                    }
                }
            },
            &mut (&mut start_idx, &mut ir_program, &mut ir_toplevel_sequence),
        )?;

    // put the toplevel sequence into the program
    // and set it as the entry_point function
    gen_toplevel_func(&mut ir_program, ir_toplevel_sequence);

    Ok(((main_name_ctx, main_parse_state, start_idx), ir_program))
}

/// Main entry point for appending code for REPL.  Pass in the ReplContext obtained previously.
/// Returns a ReplContext and a usize (denoting the funcidx of the ir_program from which is new)
pub fn run_frontend_repl<L: Logger>(
    estree_str: String,
    (name_ctx, parse_ctx, mut start_idx): ReplContext,
    ir_program: &mut ir::Program,
    logger: L,
) -> Result<(ReplContext, usize), ()> {
    // parse the given string as estree
    let es_program: estree::Node = serde_json::from_str(estree_str.as_str())
        .map_err(|_| {
            CompileMessage::new_error(plSLRef::entire_file(None).to_owned(), ESTreeParseError {})
        })
        .log_err(&logger)?;

    // check that there are no ImportDeclarations
    validate_is_repl_safe(&es_program).log_err(&logger)?;

    let mut ir_toplevel_sequence: Vec<ir::Expr> = Vec::new();

    let new_funcidx_start = ir_program.funcs.len();

    // see run_frontend() for more comments
    // We parse in REPL mode (the const generic IsRepl == true) to throw an error if we find an import statement in top-level.
    let (main_name_ctx, main_parse_state): (HashMap<String, PreVar>, ParseState) =
        func::parse_main_program(
            &name_ctx,
            &parse_ctx,
            es_program,
            Box::new([]),
            &mut start_idx,
            Some("<REPL>".to_string()),
            usize::MAX,
            ir_program,
            &mut ir_toplevel_sequence,
        )
        .map_err(|cm| {
            logger.log(cm);
        })?;

    // put the toplevel sequence into the program
    // and set it as the entry_point function
    gen_toplevel_func(ir_program, ir_toplevel_sequence);

    Ok((
        (main_name_ctx, main_parse_state, start_idx),
        new_funcidx_start,
    ))
}

/// Puts the toplevel sequence into the program
/// and set it as the entry_point function
fn gen_toplevel_func(ir_program: &mut ir::Program, ir_toplevel_sequence: Vec<ir::Expr>) {
    let ir_toplevel_func = ir::Func {
        params: Box::new([]),
        result: Some(ir::VarType::Any),
        expr: ir::Expr {
            vartype: ir_toplevel_sequence
                .last()
                .map_or_else(|| Some(ir::VarType::Undefined), |ir_expr| ir_expr.vartype),
            kind: ir::ExprKind::Sequence {
                content: ir_toplevel_sequence,
            },
        },
        signature_filter: Default::default(),
    };
    ir_program.entry_point = ir_program.add_func(ir_toplevel_func);
}

fn validate_is_repl_safe(
    es_node: &estree::Node,
) -> Result<(), CompileMessage<error::ReplHasImportDeclError>> {
    if (match &es_node.kind {
        NodeKind::Program(es_program) => es_program.body.as_slice(),
        _ => &[],
    })
    .iter()
    .any(|es_node| match &es_node.kind {
        NodeKind::ImportDeclaration(_) => true,
        _ => false,
    }) {
        Err(CompileMessage::new_error(
            plSLRef::entire_file(None).to_owned(),
            error::ReplHasImportDeclError {},
        ))
    } else {
        Ok(())
    }
}
