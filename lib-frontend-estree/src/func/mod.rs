use crate::attributes::NodeForEachWithAttributes;
use crate::compact_state;
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
}

/**
 * Parse a estree::Node that represents a whole source file.
 */
pub fn parse_program<L: Logger>(
    state: &mut compact_state::CompactState<compact_state::FrontendVar>,
    es_program_node: Node,
    deps: Box<[&compact_state::CompactState<compact_state::FrontendVar>]>,
    filename: Option<String>,
    order: usize,
    logger: &L,
    ir_program: &mut ir::Program,
    ir_toplevel_seq: &mut Vec<ir::Expr>,
) -> Result<(), CompileMessage<ParseProgramError>> {
    if let Node {
        loc,
        kind: NodeKind::Program(es_program),
    } = es_program_node
    {
        let current_scope_decls: compact_state::CompactState<compact_state::CurrentScopeItem> =
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
        Ok(())
    } else {
        Err(CompileMessage::new_error(
            es_program_node.loc.into_sl(filename),
            ParseProgramError::ESTreeError("Root node of ESTree must be Program"),
        ))
    }
}

/**
 * Extract all variables in the current scope, putting them in a hashmap (because variable names must be unique in each scope).
 * All the values (i.e. compact_state::CurrentScopeItem) are set to UnMaterialized (because they have not gotten a storage location yet)
 */
fn extract_current_decls_and_imports(
    statements: &[Node],
    deps: &[&compact_state::CompactState<compact_state::FrontendVar>],
    filename: Option<&str>,
    order: usize,
) -> Result<
    compact_state::CompactState<compact_state::UnmaterializedFrontendVar>,
    CompileMessage<ParseProgramError>,
> {
    let mut ret: compact_state::CompactState<compact_state::UnmaterializedFrontendVar> =
        CompactState::new();
    let mut import_decl_idx = 0;
    statements.each_with_attributes(filename, |es_node, attrs| {
        if let Some(constraint) = attrs.get("constrain") {
            if let Node {loc, kind: NodeKind::FunctionDeclaration(func_decl)} = es_node {
                extract_func_decl_with_constraint(
                    func_decl, constraint.map(|s| s.as_str()).ok_or_else(||CompileMessage::new_error(
                    es_node.loc.into_sl(filename).to_owned(),
                    ParseProgramError::ConstraintMissingError,
                ))?, filename, order, &mut ret
                )
			}
            else {
                Err(CompileMessage::new_error(
                    es_node.loc.into_sl(filename).to_owned(),
                    ParseProgramError::ConstraintTargetError/*("'constrain' attribute cannot be applied to this item, it is only allowed for FunctionDeclaration")*/,
                ))
			}
		}
        else {
            match es_node {
                Node {
                    loc,
                    kind: NodeKind::ImportDeclaration(import_decl),
                } => {
                    extract_import_decls(import_decl, deps[import_decl_idx], filename, &mut ret)?;
                    import_decl_idx +=1;
                    }
                Node {loc, kind: NodeKind::VariableDeclaration(var_decl)} => extract_var_decl(var_decl, filename, order, &mut ret)?,
                Node {loc, kind: NodeKind::FunctionDeclaration(func_decl)} => extract_func_decl(
                    func_decl, filename,order,  &mut ret
                )?,
            };
            Ok(())
        }
	})?;
    Ok(ret)
}

/**
 * Add the name and the overload kind of this function declaration into the out_map.
 * If there is a previous declaration of the same name, it returns a DuplicateDeclarationError.
 */
fn extract_func_decl(
    func_decl: &FunctionDeclaration,
    filename: Option<&str>,
    order: usize,
    out_map: &mut compact_state::CompactState<compact_state::UnmaterializedFrontendVar>,
) -> Result<(), CompileMessage<ParseProgramError>> {
    if let Node {
        loc,
        kind: NodeKind::Identifier(ir_identifier),
    } = *func_decl.id
    {
        if out_map.unique_insert(
            ir_identifier.name.clone(),
            // note: since the params are all any, it will definitely completely shadow any existing overload (which will raise the DuplicateDeclarationError)
            compact_state::UnmaterializedFrontendVar::new_overload(
                compact_state::UnmaterializedOverload {
                    params: func_decl.params.map(|_| ir::VarType::Any),
                    result: ir::VarType::Any,
                    order: order,
                },
            ),
        ) {
            return Err(CompileMessage::new_error(
                loc.into_sl(filename).to_owned(),
                ParseProgramError::DuplicateDeclarationError(ir_identifier.name),
            ));
        }
        return Ok(());
    }
    Err(CompileMessage::new_error(
        func_decl.id.loc.into_sl(filename).to_owned(),
        ParseProgramError::ESTreeError("FunctionDeclaration.id must be an Identifier"),
    ))
}

/**
 * Add the name and the overload kind of this constrained function declaration into the out_map.
 * If this function declaration completely shadows a previous declaration of the same name, it returns a DuplicateDeclarationError.
 * The constraint is of the form x:<type>,y:<type>,... (where x, y are param names)
 */
fn extract_func_decl_with_constraint(
    func_decl: &FunctionDeclaration,
    constraint: &str,
    filename: Option<&str>,
    order: usize,
    out_map: &mut compact_state::CompactState<compact_state::UnmaterializedFrontendVar>,
) -> Result<(), CompileMessage<ParseProgramError>> {
    if let Node {
        loc,
        kind: NodeKind::Identifier(ir_identifier),
    } = *func_decl.id
    {
        let constraint_map: HashMap<&str, ir::VarType> = parse_constraint(constraint)?;
        if out_map.unique_insert(
            ir_identifier.name.clone(),
            compact_state::UnmaterializedFrontendVar::new_overload(
                compact_state::UnmaterializedOverload {
                    params: func_decl.params.map(|id_node| {
                        if let Node {
                            id_loc,
                            kind: NodeKind::Identifier(id),
                        } = id_node
                        {
                            constraint_map.get(id.name).unwrap_or(ir::VarType::Any)
                        } else {
                            return Err(CompileMessage::new_error(
                                id_loc.into_sl(filename).to_owned(),
                                ParseProgramError::ESTreeError(
                                    "FunctionDeclaration.param must be a list of Identifier",
                                ),
                            ));
                        }
                    }),
                    result: ir::VarType::Any,
                    order: order,
                },
            ),
        ) {
            return Err(CompileMessage::new_error(
                loc.into_sl(filename).to_owned(),
                ParseProgramError::DuplicateDeclarationError(ir_identifier.name),
            ));
        }
        return Ok(());
    }
    Err(CompileMessage::new_error(
        func_decl.id.loc.into_sl(filename).to_owned(),
        ParseProgramError::ESTreeError("FunctionDeclaration.id must be an Identifier"),
    ))
}
