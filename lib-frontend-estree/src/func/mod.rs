use super::ParseState;
use super::ProgramPreExports;
use crate::estree::SourceLocation as esSL;
use crate::estree::*;
use crate::extensions::IntoSourceLocation;
use ir;
use projstd::log::CompileMessage;
use std::collections::HashMap;

mod constraint;
mod post_parse;
mod pre_parse;
mod undoable_hash_map;
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

impl std::fmt::Display for ParseProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseProgramError::ESTreeError(reason) => write!(f, "ESTree semantics: {}", reason),
            ParseProgramError::SourceRestrictionError(reason) => {
                write!(f, "Source restriction: {}", reason)
            }
            ParseProgramError::DuplicateDeclarationError(varname) => {
                write!(f, "Duplicate declaration of `{}'", varname)
            }
            ParseProgramError::DuplicateExportError(varname) => {
                write!(f, "Duplicate export of `{}'", varname)
            }
            ParseProgramError::DanglingAttributeError => {
                write!(f, "This attribute is not associated with any statement")
            }
            ParseProgramError::AttributeNotStringLiteralError => {
                write!(f, "RHS of attribute declaration must be a string literal")
            }
            ParseProgramError::AttributeParseError => write!(
                f,
                "Attribute declaration string is not in the correct format"
            ),
            ParseProgramError::AttributeContentError(reason) => {
                write!(f, "Attribute content: {}", reason)
            }
            ParseProgramError::AttributeUnrecognizedError(key) => {
                write!(f, "Unrecognized attribute `{}'", key)
            }
            ParseProgramError::DirectFunctionCaptureError => write!(
                f,
                "Direct function must not capture any non-global variables"
            ),
            ParseProgramError::UndeclaredNameError(varname) => {
                write!(f, "Undeclared name `{}'", varname)
            }
            ParseProgramError::UndeclaredExportError(varname) => write!(
                f,
                "Could not find an export called `{}' in the imported module",
                varname
            ),
            ParseProgramError::SourceRestrictionUnaryOperatorError(op) => write!(
                f,
                "Source restriction: Unary operator `{}' is not allowed",
                op
            ),
            ParseProgramError::SourceRestrictionBinaryOperatorError(op) => write!(
                f,
                "Source restriction: Binary operator `{}' is not allowed",
                op
            ),
            ParseProgramError::SourceRestrictionLogicalOperatorError(op) => write!(
                f,
                "Source restriction: Logical operator `{}' is not allowed",
                op
            ),
            ParseProgramError::SourceRestrictionAssignmentOperatorError(op) => write!(
                f,
                "Source restriction: Compound assignment operator `{}' is not allowed",
                op
            ),
        }
    }
}

/**
 * Parse a estree::Node that represents a whole source file of a dependency (i.e not the main program).
 */
pub fn parse_dep_program(
    default_name_ctx: &HashMap<String, PreVar>, // pre-declared Source names
    default_parse_state: &ParseState,           // already parsed automatic imports
    es_program_node: Node,
    deps: Box<[&(ProgramPreExports, ParseState)]>,
    start_idx: &mut usize,
    filename: Option<String>,
    order: usize,
    ir_program: &mut ir::Program,
    ir_toplevel_seq: &mut Vec<ir::Expr>,
) -> Result<(ProgramPreExports, ParseState), CompileMessage<ParseProgramError>> {
    parse_program(
        pre_parse::pre_parse_program::<true>,
        post_parse::post_parse_program::<true>,
        &mut default_name_ctx.clone(),
        &mut default_parse_state.clone(),
        es_program_node,
        deps,
        start_idx,
        filename,
        order,
        ir_program,
        ir_toplevel_seq,
    )
}

/**
 * Parse a estree::Node that represents a whole source file of the main program.
 */
pub fn parse_main_program(
    default_name_ctx: &HashMap<String, PreVar>, // pre-declared Source names
    default_parse_state: &ParseState,           // already parsed automatic imports
    es_program_node: Node,
    deps: Box<[&(ProgramPreExports, ParseState)]>,
    start_idx: &mut usize,
    filename: Option<String>,
    order: usize,
    ir_program: &mut ir::Program,
    ir_toplevel_seq: &mut Vec<ir::Expr>,
) -> Result<(HashMap<String, PreVar>, ParseState), CompileMessage<ParseProgramError>> {
    let mut modified_name_ctx = default_name_ctx.clone();
    let mut modified_parse_state = default_parse_state.clone();
    parse_program(
        pre_parse::pre_parse_program::<false>,
        post_parse::post_parse_program::<false>,
        &mut modified_name_ctx,
        &mut modified_parse_state,
        es_program_node,
        deps,
        start_idx,
        filename,
        order,
        ir_program,
        ir_toplevel_seq,
    )?;
    Ok((modified_name_ctx, modified_parse_state))
}

/**
 * Helper to parse a estree::Node that represents a whole source file.
 */
pub fn parse_program<R1, R2>(
    pre_parse_program: impl FnOnce(
        &mut Program,
        &Option<esSL>,
        &mut HashMap<String, PreVar>,
        &[&ProgramPreExports],
        &mut usize,
        Option<&str>,
    ) -> Result<R1, CompileMessage<ParseProgramError>>,
    post_parse_program: impl FnOnce(
        Program,
        Option<esSL>,
        &mut ParseState,
        &[&ParseState],
        Option<&str>,
        &mut ir::Program,
        &mut Vec<ir::Expr>,
    ) -> Result<R2, CompileMessage<ParseProgramError>>,
    default_name_ctx: &mut HashMap<String, PreVar>, // pre-declared Source names
    default_parse_state: &mut ParseState,           // already parsed automatic imports
    es_program_node: Node,
    deps: Box<[&(ProgramPreExports, ParseState)]>,
    start_idx: &mut usize,
    filename: Option<String>,
    _order: usize,
    ir_program: &mut ir::Program,
    ir_toplevel_seq: &mut Vec<ir::Expr>,
) -> Result<(R1, R2), CompileMessage<ParseProgramError>> {
    if let Node {
        loc,
        kind: NodeKind::Program(mut es_program),
    } = es_program_node
    {
        let program_pre_exports: R1 = pre_parse_program(
            &mut es_program,
            &loc,
            default_name_ctx,
            &deps
                .iter()
                .copied()
                .map(|(pre_exports, _)| pre_exports)
                .collect::<Box<[&ProgramPreExports]>>(),
            start_idx,
            filename.as_deref(),
        )?;
        let parse_state: R2 = post_parse_program(
            es_program,
            loc,
            default_parse_state,
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
    } else {
        Err(CompileMessage::new_error(
            es_program_node.loc.into_sl(filename),
            ParseProgramError::ESTreeError("Root node of ESTree must be Program"),
        ))
    }
}
