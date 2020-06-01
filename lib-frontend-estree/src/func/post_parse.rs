use super::ParseProgramError;
use super::ParseState;
use crate::estree::SourceLocation as esSL;
use crate::estree::*;
use crate::extensions::IntoSourceLocation;
use crate::frontendvar::*;
use ir;
use projstd::log::CompileMessage;
use std::collections::HashMap;
use std::result::Result;

/**
 * ESTree variables have now been tagged with PreVar,
 * and closures know the non-global variables that they capture,
 * and scopes (block and function bodies) know their variables that are address-taken.
 *
 * So the parse_ctx is sufficient to translate the tagged es_program to the relevant additions to the ir_program and ir_toplevel_seq.
 *
 * Note: we do not validate ESTree/SourceRestrictions here, because pre_parse() should have already done it.
 * Here, we forcefully unwrap optionals etc.
 *
 * Returns the parse state (of the globals only!)
 */
pub fn post_parse_program(
    es_program: Program,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    deps: &[&ParseState],
    filename: Option<&str>,
    ir_program: &mut ir::Program,
    ir_toplevel_seq: &mut Vec<ir::Expr>,
) -> Result<ParseState, CompileMessage<ParseProgramError>> {
    unimplemented!();
}

// Returns the ir::Expr representing the current block
fn post_parse_block_statement(
    es_block: BlockStatement,
    _loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program, // for adding new structs/functions if necessary
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    let new_depth = depth + 1;

    // synthesise the struct for the address taken vars
    let struct_idx = ir_program.struct_types.len();
    let struct_type: Box<[ir::VarType]> = es_block
        .address_taken_vars
        .iter()
        .map(|_| ir::VarType::Any)
        .collect();
    ir_program.struct_types.push(struct_type);

    // generate the TargetExprs for the address taken vars
    let target_expr_entries: Box<[(VarLocId, ir::TargetExpr)]> = es_block
        .address_taken_vars
        .iter()
        .copied()
        .enumerate()
        .map(|(ct, idx)| {
            (
                VarLocId {
                    depth: new_depth,
                    index: idx,
                },
                ir::TargetExpr::Local {
                    localidx: num_locals,
                    next: Some(Box::new(ir::StructField {
                        typeidx: struct_idx,
                        fieldidx: ct,
                        next: None,
                    })),
                },
            )
        })
        .collect();

    let new_num_locals = num_locals + 1;

    // add these vars to the parse_ctx
    let target_undo_ctx = parse_ctx.add_targets(target_expr_entries);

    // reserve ir::FuncIdx and generate the overload sets for all the directs
    let direct_entries: Box<[(String, OverloadSet<(Box<[ir::VarType]>, ir::FuncIdx)>)]> = es_block
        .direct_funcs
        .into_iter()
        .map(|(s, overload_set)| {
            let tmp = ir_program.funcs.len();
            ir_program.funcs.push(ir::Func::new());
            (s, overload_set.map(|sig| (sig, tmp)))
        })
        .collect();

    // add these direct entries to the parse_ctx
    let direct_undo_ctx = parse_ctx.add_directs(direct_entries);

    // make the actual ir:
    let mut sequence: Vec<ir::Expr> = Vec::new();
    // add the struct allocation
    sequence.push(ir::Expr {
        vartype: Some(ir::VarType::Undefined),
        kind: ir::ExprKind::Assign {
            target: ir::TargetExpr::Local {
                localidx: num_locals,
                next: None,
            },
            expr: Box::new(ir::Expr {
                vartype: Some(ir::VarType::StructT {
                    typeidx: struct_idx,
                }),
                kind: ir::ExprKind::PrimStructT {
                    typeidx: struct_idx,
                },
            }),
        },
    });
    {
        let stmt_nodes_attrs: Vec<(Node, HashMap<String, Option<String>>)> = Vec::new();
        es_block
            .body
            .each_with_attributes_into(filename, |es_node, attr| {
                stmt_nodes_attrs.push((es_node, attr));
                Ok(())
            })?;

        let mut stmt_iter = stmt_nodes_attrs.into_iter();
        while let Some((es_stmt, attr)) = stmt_iter.next() {
            let (ir_expr, new_stmt_iter) = post_parse_statement(
                es_stmt, attr, parse_ctx, stmt_iter, depth, num_locals, filename, ir_program,
            )?;
            sequence.push(ir_expr);
            stmt_iter = new_stmt_iter;
        }
    }
    // the extra undefined below ensures that the sequence returns undefined
    // todo! IR should remove this Expr if the last stmt already returns undefined, so that we can ensure tail call in certain situations.
    sequence.push(make_prim_undefined());

    let ret = ir::Expr {
        vartype: Some(ir::VarType::Undefined),
        kind: ir::ExprKind::Declaration {
            local: ir::VarType::StructT {
                typeidx: struct_idx,
            },
            expr: Box::new(ir::Expr {
                vartype: Some(ir::VarType::Undefined),
                kind: ir::ExprKind::Sequence { content: sequence },
            }),
        },
    };

    // remove the direct entries from the parse_ctx
    parse_ctx.remove_directs(direct_undo_ctx);

    // remove the vars from the parse_ctx
    parse_ctx.remove_targets(target_undo_ctx);

    Ok(ret)
}

fn add_remaining_stmts<I: Iterator<Item = (Node, HashMap<String, Option<String>>)>>(
    mut stmt_iter: I,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize,
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, I), CompileMessage<ParseProgramError>> {
    let mut sequence: Vec<ir::Expr> = Vec::new();
    while let Some((es_stmt, attr)) = stmt_iter.next() {
        let (ir_expr, new_stmt_iter) = post_parse_statement(
            es_stmt, attr, parse_ctx, stmt_iter, depth, num_locals, filename, ir_program,
        )?;
        sequence.push(ir_expr);
        stmt_iter = new_stmt_iter;
    }
    sequence.push(make_prim_undefined());
    Ok((
        ir::Expr {
            vartype: Some(ir::VarType::Undefined),
            kind: ir::ExprKind::Sequence { content: sequence },
        },
        stmt_iter,
    ))
}

fn post_parse_statement<I: Iterator<Item = (Node, HashMap<String, Option<String>>)>>(
    es_node: Node,
    attributes: HashMap<String, Option<String>>,
    parse_ctx: &mut ParseState,
    more_stmt_attr_iter: I,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, I), CompileMessage<ParseProgramError>> {
    // we do not validate constraints or anything else (pre_parse should have done it)
    match es_node.kind {
        NodeKind::ExpressionStatement(stmt) => Ok((
            post_parse_expr_statement(
                stmt,
                es_node.loc,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?,
            more_stmt_attr_iter,
        )),
        NodeKind::BlockStatement(block) => Ok((
            post_parse_block_statement(
                block,
                es_node.loc,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?,
            more_stmt_attr_iter,
        )),
        NodeKind::ReturnStatement(stmt) => Ok((
            post_parse_return_statement(
                stmt,
                es_node.loc,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?,
            more_stmt_attr_iter,
        )),
        NodeKind::IfStatement(stmt) => Ok((
            post_parse_if_statement(
                stmt,
                es_node.loc,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?,
            more_stmt_attr_iter,
        )),
        NodeKind::FunctionDeclaration(func_decl) => {
            if attributes.get("direct").is_some() {
                Ok((
                    post_parse_direct_func_decl(
                        func_decl,
                        es_node.loc,
                        parse_ctx,
                        attributes.get("constraint"),
                        depth,
                        num_locals,
                        filename,
                        ir_program,
                    )?,
                    more_stmt_attr_iter,
                ))
            } else {
                post_parse_func_decl(
                    func_decl,
                    es_node.loc,
                    parse_ctx,
                    more_stmt_attr_iter,
                    depth,
                    num_locals,
                    filename,
                    ir_program,
                )
            }
        }
        NodeKind::VariableDeclaration(var_decl) => post_parse_var_decl(
            var_decl,
            es_node.loc,
            parse_ctx,
            more_stmt_attr_iter,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::EmptyStatement(_) => Ok((
            ir::Expr {
                vartype: Some(ir::VarType::Undefined),
                kind: ir::ExprKind::Sequence {
                    content: Vec::new(),
                },
            },
            more_stmt_attr_iter,
        )), // todo! IR optimisation should prune empty statments
        _ => pppanic(),
    }
}

fn post_parse_expr_statement(
    es_expr_stmt: ExpressionStatement,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // We don't need to detect AssignmentExpression separately here...
    // We just treat it as an expression that returns undefined.
    // pre_parse() would have already ensured that there are no nested AssignmentExpressions.

    post_parse_expr(
        *es_expr_stmt.expression,
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

fn post_parse_return_statement(
    es_return: ReturnStatement,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // Emits the ExprKind::Return.

    Ok(ir::Expr {
        vartype: None, // return statements produce Void
        kind: ir::ExprKind::Return {
            expr: Box::new(post_parse_expr(
                *es_return.expression,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?),
        },
    })
}

fn post_parse_if_statement(
    es_if: IfStatement,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // Emits the ExprKind::Conditional.
    // Each branch is a BlockStatement, and hence returns Undefined.

    // todo! Must cond have type boolean?????

    let cond_loc: ir::SourceLocation = as_ir_sl(es_if.test.loc, FILE);

    // We synthesise the typecheck to ensure that the condition is a boolean
    // then add the true_expr and false_expr
    Ok(ir::Expr {
        vartype: Some(ir::VarType::Undefined),
        kind: ir::ExprKind::Conditional {
            cond: Box::new(ir::Expr {
                vartype: Some(ir::VarType::Boolean),
                kind: ir::ExprKind::TypeCast {
                    test: Box::new(post_parse_expr(
                        *es_if.test,
                        parse_ctx,
                        depth,
                        num_locals,
                        filename,
                        ir_program,
                    )?),
                    expected: ir::VarType::Boolean,
                    create_narrow_local: true,
                    true_expr: Box::new(ir::Expr {
                        vartype: Some(ir::VarType::Boolean),
                        kind: ir::ExprKind::VarName {
                            source: ir::TargetExpr::Local {
                                localidx: num_locals,
                                next: None,
                            },
                        },
                    }),
                    false_expr: Box::new(ir::Expr {
                        vartype: None,
                        kind: ir::ExprKind::Trap {
                            code: ir::error::ERROR_CODE_IF_STATEMENT_CONDITION_TYPE,
                            location: cond_loc,
                        },
                    }),
                },
            }),
            true_expr: Box::new(post_parse_expr(
                *es_if.consequent,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?),
            false_expr: Box::new(post_parse_expr(
                *es_if.alternate.unwrap(),
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?),
        },
    })
}

fn post_parse_direct_func_decl(
    es_func_decl: FunctionDeclaration,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    opt_constraint: Option<&Option<String>>,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    unimplemented!();
}

fn post_parse_func_decl<I: Iterator<Item = (Node, HashMap<String, Option<String>>)>>(
    es_func_decl: FunctionDeclaration,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    more_stmt_attr_iter: I,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, I), CompileMessage<ParseProgramError>> {
    let varlocid = as_varlocid(as_id(*es_func_decl.id).prevar.unwrap());
    let rhs_expr: ir::Expr = post_parse_function(
        es_func_decl,
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )?;
    post_parse_decl_helper(
        varlocid,
        rhs_expr,
        parse_ctx,
        |parse_ctx, depth, num_locals, filename, ir_program| {
            add_remaining_stmts(
                more_stmt_attr_iter,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )
        },
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

fn post_parse_var_decl<I: Iterator<Item = (Node, HashMap<String, Option<String>>)>>(
    es_var_decl: VariableDeclaration,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    more_stmt_attr_iter: I,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, I), CompileMessage<ParseProgramError>> {
    post_parse_var_decr_recurse(
        es_var_decl
            .declarations
            .into_iter()
            .map(|decr_node| as_var_decr(decr_node)),
        parse_ctx,
        more_stmt_attr_iter,
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

fn post_parse_var_decr_recurse<
    I: Iterator<Item = (Node, HashMap<String, Option<String>>)>,
    J: Iterator<Item = VariableDeclarator>,
>(
    es_var_decr_iter: J,
    parse_ctx: &mut ParseState,
    more_stmt_attr_iter: I,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, I), CompileMessage<ParseProgramError>> {
    if let Some(es_var_decr) = es_var_decr_iter.next() {
        let varlocid = as_varlocid(as_id(*es_var_decr.id).prevar.unwrap());
        let rhs_expr: ir::Expr = post_parse_expr(
            *es_var_decr.init.unwrap(),
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        )?;
        post_parse_decl_helper(
            varlocid,
            rhs_expr,
            parse_ctx,
            |parse_ctx, depth, num_locals, filename, ir_program| {
                post_parse_var_decr_recurse(
                    es_var_decr_iter,
                    parse_ctx,
                    more_stmt_attr_iter,
                    depth,
                    num_locals,
                    filename,
                    ir_program,
                )
            },
            depth,
            num_locals,
            filename,
            ir_program,
        )
    } else {
        add_remaining_stmts(
            more_stmt_attr_iter,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        )
    }
}

/**
 * A helper function to emit a ExprKind::Declaration.
 * Used by VariableDeclaration and FunctionDeclaration (Target kind).
 */
fn post_parse_decl_helper<
    R,
    F: FnOnce(
        &mut ParseState,
        usize,
        usize,
        Option<&str>,
        &mut ir::Program,
    ) -> Result<(ir::Expr, R), CompileMessage<ParseProgramError>>,
>(
    varlocid: VarLocId,
    es_rhs_expr: ir::Expr,
    parse_ctx: &mut ParseState,
    more: F,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, R), CompileMessage<ParseProgramError>> {
    let mut sequence: Vec<ir::Expr> = Vec::new();
    sequence.push(ir::Expr {
        vartype: Some(ir::VarType::Undefined),
        kind: ir::ExprKind::Assign {
            target: ir::TargetExpr::Local {
                localidx: num_locals,
                next: None,
            },
            expr: Box::new(es_rhs_expr),
        },
    });

    let undo_ctx = parse_ctx.add_target(
        varlocid,
        ir::TargetExpr::Local {
            localidx: num_locals,
            next: None,
        },
    );

    let new_num_locals = num_locals + 1;

    let (ir_expr, ret) = more(parse_ctx, depth, new_num_locals, filename, ir_program)?;
    sequence.push(ir_expr);

    parse_ctx.remove_target(undo_ctx);

    Ok((
        ir::Expr {
            vartype: Some(ir::VarType::Undefined),
            kind: ir::ExprKind::Declaration {
                local: ir::VarType::Any,
                expr: Box::new(ir::Expr {
                    vartype: Some(ir::VarType::Undefined),
                    kind: ir::ExprKind::Sequence { content: sequence },
                }),
            },
        },
        ret,
    ))
}

fn post_parse_expr(
    es_expr: Node,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    unimplemented!();
}

fn make_prim_undefined() -> ir::Expr {
    ir::Expr {
        vartype: Some(ir::VarType::Undefined),
        kind: ir::ExprKind::PrimUndefined,
    }
}

fn as_varlocid(prevar: PreVar) -> VarLocId {
    if let PreVar::Target(varlocid) = prevar {
        varlocid
    } else {
        pppanic();
    }
}

fn as_var_decr(es_node: Node) -> VariableDeclarator {
    if let NodeKind::VariableDeclarator(var_decr) = es_node.kind {
        var_decr
    } else {
        pppanic();
    }
}

fn as_id(es_node: Node) -> Identifier {
    if let NodeKind::Identifier(id) = es_node.kind {
        id
    } else {
        pppanic();
    }
}

#[feature(never_type)]
fn pppanic() -> ! {
    panic!("pre_parse() should have detected this error");
}
