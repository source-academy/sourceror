use super::ParseState;
use super::ProgramPreExports;
use crate::attributes::NodeForEachWithAttributes;
use crate::estree::*;
use crate::extensions::IntoSourceLocation;
use ir;
use projstd::log::CompileMessage;
use projstd::log::LogErr;
use projstd::log::Logger;
use projstd::log::Severity;
use projstd::log::SourceLocationRef as plSLRef;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

mod constraint;
mod post_parse;
mod pre_parse;
mod undoable_hash_map;
mod vartype_superset;
mod varusage;

pub enum ParseProgramError {
    ESTreeError(&'static str), // ESTree semantic error (reason)
    SourceRestrictionError(&'static str),
    DuplicateDeclarationError(String), // duplicate declaration of the same name in the same scope (varname)
    DuplicateExportError(String),      // duplicate export of the same name
    DanglingAttributeError, // multiple consecutive attributes, or last statement of block is an attribute
    AttributeNotStringLiteralError, // RHS of attribute assignment is not a string literal
    AttributeParseError,    // attribute string cannot be parsed into a hashmap
    AttributeContentError(&'static str), // some issue with the key or value of an attribute
    AttributeUnrecognizedError(String), // this attribute key is not recognized
    /*ConstraintTargetError,             // constraint cannot be applied to this type of node
    ConstraintMissingError, // constraint attribute did not come with a value
    ConstraintParseError,   // constraint attribute value cannot be parsed into a hashmap*/
    DirectFunctionCaptureError, // direct function tried to capture a non-global variable // todo! store the variable name and declaration location?
    UndeclaredNameError(String), // undeclared variable name (both direct and target)
    UndeclaredExportError(String), // the imported module did not export this name (both direct and target)
    SourceRestrictionUnaryOperatorError(String), // this unary operator is not allowed
    SourceRestrictionBinaryOperatorError(String), // this binary operator is not allowed
    SourceRestrictionLogicalOperatorError(String), // this logical operator is not allowed
    SourceRestrictionAssignmentOperatorError(String), // this assignment operator is not allowed
}

/**
 * Parse a estree::Node that represents a whole source file.
 */
pub fn parse_program<L: Logger>(
    default_name_ctx: &HashMap<String, PreVar>, // pre-declared Source names
    default_parse_state: &ParseState,           // already parsed automatic imports
    es_program_node: Node,
    deps: Box<[&(ProgramPreExports, ParseState)]>,
    start_idx: &mut usize,
    filename: Option<String>,
    order: usize,
    logger: &L,
    ir_program: &mut ir::Program,
    ir_toplevel_seq: &mut Vec<ir::Expr>,
) -> Result<(ProgramPreExports, ParseState), CompileMessage<ParseProgramError>> {
    if let Node {
        loc,
        kind: NodeKind::Program(es_program),
    } = es_program_node
    {
        let program_pre_exports: ProgramPreExports = pre_parse::pre_parse_program(
            &mut es_program,
            &loc,
            &mut default_name_ctx.clone(),
            &deps
                .iter()
                .copied()
                .map(|(pre_exports, _)| pre_exports)
                .collect::<Box<[&ProgramPreExports]>>(),
            start_idx,
            filename.as_deref(),
        )?;
        let parse_state: ParseState = post_parse::post_parse_program(
            es_program,
            loc,
            &mut default_parse_state.clone(),
            &deps
                .iter()
                .copied()
                .map(|(_, parse_state)| parse_state)
                .collect::<Box<[&ParseState]>>(),
            filename.as_deref(),
            ir_program,
            ir_toplevel_seq,
        )?;
        Ok((program_pre_exports, parse_state))
    /*let current_scope_decls: compact_state::CompactState<compact_state::CurrentScopeItem> =
        compact_state::CompactState::from_unmaterialized(extract_current_decls_and_imports(
            &es_program.body,
            &*deps,
            filename.as_deref(),
            order,
        )?);
    let mut import_decl_idx = 0;
    for es_node in es_program.body {
        match es_node {
            Node {
                loc,
                kind: NodeKind::ImportDeclaration(import_decl),
            } => parse_import_declaration(
                import_decl,
                deps[import_decl_idx],
                filename.as_deref(),
                order,
            )?,
            _ => parse_statement(
                es_node,
                state,
                true,
                filename.as_deref(),
                order,
                ir_program,
                ir_toplevel_seq,
            ),
        };
    }
    Ok(())*/
    } else {
        Err(CompileMessage::new_error(
            es_program_node.loc.into_sl(filename),
            ParseProgramError::ESTreeError("Root node of ESTree must be Program"),
        ))
    }
}
