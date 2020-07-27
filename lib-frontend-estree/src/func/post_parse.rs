use super::ParseProgramError;
use super::ParseState;
use crate::attributes::NodeForEachWithAttributes;
use crate::attributes::NodeForEachWithAttributesInto;
use crate::attributes::NodeForEachWithAttributesMut;
use crate::builtins;
use crate::estree::SourceLocation as esSL;
use crate::estree::*;
use crate::extensions::IntoSourceLocation;
use crate::frontendvar::OverloadSet;
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
 * Note: parse_ctx will be discarded after this function returns.  By convention,
 * this function should put parse_ctx to its original state before returning.
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
    let (mut body, direct_funcs) = es_program.destructure();

    // direct functions that are imported
    let mut direct_imports: Vec<(String, OverloadSet<(Box<[ir::VarType]>, ir::FuncIdx)>)> =
        Vec::new();
    {
        let mut dep_index = 0;
        body.each_with_attributes(filename, |es_node, _| {
            if let Node {
                loc: _,
                kind: NodeKind::ImportDeclaration(import_decl),
            } = es_node
            {
                let curr_dep_idx = dep_index;
                dep_index += 1;

                for import_spec_node in &import_decl.specifiers {
                    let import_spec = as_import_spec_ref(import_spec_node);
                    //let source_id = as_id_ref(&*import_spec.imported);
                    let local_id = as_id_ref(&*import_spec.local);
                    match local_id.prevar.unwrap() {
                        PreVar::Direct => direct_imports.push((
                            local_id.name.to_owned(),
                            deps[curr_dep_idx]
                                .get_direct(local_id.name.as_str())
                                .unwrap()
                                .clone(),
                        )),
                        _ => {}
                    }
                }
            }
            Ok(())
        })?;
    }

    // add the direct functions imported
    let direct_import_undo_ctx =
        parse_ctx.add_direct_overloadsets(direct_imports.into_boxed_slice());

    // generate the TargetExprs for the globals and imports
    let mut target_expr_entries: Vec<(VarLocId, ir::TargetExpr)> = Vec::new();
    {
        let mut dep_index = 0;
        body.each_with_attributes(filename, |es_node, _| {
            if let Node {
                loc: _,
                kind: NodeKind::FunctionDeclaration(func_decl),
            } = es_node
            {
                if let PreVar::Target(varlocid) = as_id_ref(&*func_decl.id).prevar.as_ref().unwrap()
                {
                    target_expr_entries.push((
                        *varlocid,
                        ir::TargetExpr::Global {
                            globalidx: {
                                let tmp = ir_program.globals.len();
                                ir_program.globals.push(ir::VarType::Any);
                                tmp
                            },
                            next: None,
                        },
                    ))
                }
            } else if let Node {
                loc: _,
                kind: NodeKind::VariableDeclaration(var_decl),
            } = es_node
            {
                for var_decr_node in &var_decl.declarations {
                    let es_var_decr: &VariableDeclarator = as_var_decr_ref(var_decr_node);
                    let varlocid = as_varlocid(as_id_ref(&*es_var_decr.id).prevar.unwrap());
                    target_expr_entries.push((
                        varlocid,
                        ir::TargetExpr::Global {
                            globalidx: {
                                let tmp = ir_program.globals.len();
                                ir_program.globals.push(ir::VarType::Any);
                                tmp
                            },
                            next: None,
                        },
                    ))
                }
            } else if let Node {
                loc: _,
                kind: NodeKind::ImportDeclaration(import_decl),
            } = es_node
            {
                let curr_dep_idx = dep_index;
                dep_index += 1;

                for import_spec_node in &import_decl.specifiers {
                    let import_spec = as_import_spec_ref(&import_spec_node);
                    //let source_id = as_id_ref(&*import_spec.imported);
                    let local_id = as_id_ref(&*import_spec.local);
                    match local_id.prevar.as_ref().unwrap() {
                        PreVar::Target(varlocid) => target_expr_entries.push((
                            *varlocid,
                            deps[curr_dep_idx].get_target(varlocid).unwrap().clone(),
                        )),
                        _ => {}
                    }
                }
            }
            Ok(())
        })?;
    }

    // add these vars to the parse_ctx
    let target_undo_ctx = parse_ctx.add_targets(target_expr_entries.into_boxed_slice());

    // same as post_parse_scope() START

    // reserve ir::FuncIdx and generate the overload sets for all the directs
    let direct_entries: Box<[(String, (Box<[ir::VarType]>, ir::FuncIdx))]> = direct_funcs
        .into_iter()
        .map(|(s, signature)| {
            let tmp = ir_program.funcs.len();
            ir_program.funcs.push(ir::Func::new());
            (s, (signature, tmp))
        })
        .collect();

    // give the ir::FuncIdx to each direct FunctionDeclaration
    {
        let mut ct = 0;
        body.each_with_attributes_mut(filename, |es_node, _| {
            if let Node {
                loc: _,
                kind: NodeKind::FunctionDeclaration(func_decl),
            } = es_node
            {
                if let PreVar::Direct = as_id_ref(&*func_decl.id).prevar.as_ref().unwrap() {
                    func_decl.direct_props = Some((direct_entries[ct].1).clone());
                    ct += 1;
                }
            }
            Ok(())
        })?;
        assert!(ct == direct_entries.len());
    }

    // add these direct entries to the parse_ctx
    let direct_undo_ctx = parse_ctx.add_directs(direct_entries);

    // same as post_parse_scope() END

    // for the exports
    let mut exports: ParseState = Default::default();

    // emit the body
    body.each_with_attributes_into(filename, |es_node, attr| {
        let ir_expr: ir::Expr = post_parse_toplevel_statement(
            es_node,
            attr,
            parse_ctx,
            deps,
            filename,
            ir_program,
            &mut exports,
        )?;
        ir_toplevel_seq.push(ir_expr);
        Ok(())
    })?;

    // remove the direct entries from the parse_ctx
    parse_ctx.remove_directs(direct_undo_ctx);

    // remove the vars from the parse_ctx
    parse_ctx.remove_targets(target_undo_ctx);

    // remove the direct import overloadsets
    parse_ctx.remove_direct_overloadsets(direct_import_undo_ctx);

    Ok(exports)
}

/**
 * This is a workaround because we don't have generic lambdas in Rust. (for more info, see: C++ generic lambdas)
 */
trait ScopePrefixEmitter {
    fn emit<I: Iterator<Item = (Node, HashMap<String, Option<String>>)>>(
        self,
        out_sequence: &mut Vec<ir::Expr>, // the output vec to append to
        parse_ctx: &mut ParseState,
        stmt_iter: I,
        new_depth: usize,      // new depth
        new_num_locals: usize, // new num locals
        filename: Option<&str>,
        ir_program: &mut ir::Program,
    ) -> Result<I, CompileMessage<ParseProgramError>>;
}

// todo! don't generate a new struct if there are no address-taken vars
fn post_parse_scope<S: Scope, PE: ScopePrefixEmitter>(
    es_scope: S,
    _loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    write_prefix_exprs: PE,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program, // for adding new structs/functions if necessary
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    let (mut body, address_taken_vars, direct_funcs) = es_scope.destructure();

    let new_depth = depth + 1;

    // synthesise the struct for the address taken vars
    let struct_idx = ir_program.struct_types.len();
    let struct_type: Box<[ir::VarType]> = address_taken_vars
        .iter()
        .map(|_| ir::VarType::Any)
        .collect();
    ir_program.struct_types.push(struct_type);

    // generate the TargetExprs for the address taken vars
    let target_expr_entries: Box<[(VarLocId, ir::TargetExpr)]> = address_taken_vars
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

    // same as post_parse_program() START

    // reserve ir::FuncIdx and generate the overload sets for all the directs
    let direct_entries: Box<[(String, (Box<[ir::VarType]>, ir::FuncIdx))]> = direct_funcs
        .into_iter()
        .map(|(s, signature)| {
            let tmp = ir_program.funcs.len();
            ir_program.funcs.push(ir::Func::new());
            (s, (signature, tmp))
        })
        .collect();

    // give the ir::FuncIdx to each direct FunctionDeclaration
    {
        let mut ct = 0;
        body.each_with_attributes_mut(filename, |es_node, attr| {
            if let Node {
                loc: _,
                kind: NodeKind::FunctionDeclaration(func_decl),
            } = es_node
            {
                if attr.contains_key("direct") {
                    func_decl.direct_props = Some((direct_entries[ct].1).clone());
                    ct += 1;
                }
            }
            Ok(())
        })?;
        assert!(ct == direct_entries.len());
    }

    // add these direct entries to the parse_ctx
    let direct_undo_ctx = parse_ctx.add_directs(direct_entries);

    // same as post_parse_program() END

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

    // emit the body
    {
        let mut stmt_nodes_attrs: Vec<(Node, HashMap<String, Option<String>>)> = Vec::new();
        body.each_with_attributes_into(filename, |es_node, attr| {
            stmt_nodes_attrs.push((es_node, attr));
            Ok(())
        })?;

        let mut stmt_iter = stmt_nodes_attrs.into_iter().fuse();

        // emit pre-bits if any (usually this is used for function params if this scope is a function scope)
        let ret_stmt_iter = write_prefix_exprs.emit(
            &mut sequence,
            parse_ctx,
            stmt_iter,
            new_depth,
            new_num_locals,
            filename,
            ir_program,
        )?;
        stmt_iter = ret_stmt_iter;

        // process all the body statements
        while let Some((es_stmt, attr)) = stmt_iter.next() {
            let (ir_expr, new_stmt_iter) = post_parse_statement(
                es_stmt,
                attr,
                parse_ctx,
                stmt_iter,
                new_depth,
                new_num_locals,
                filename,
                ir_program,
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
) -> Result<(Vec<ir::Expr>, I), CompileMessage<ParseProgramError>> {
    let mut ret: Vec<ir::Expr> = Vec::new();
    while let Some((es_stmt, attr)) = stmt_iter.next() {
        let (ir_expr, new_stmt_iter) = post_parse_statement(
            es_stmt, attr, parse_ctx, stmt_iter, depth, num_locals, filename, ir_program,
        )?;
        ret.push(ir_expr);
        stmt_iter = new_stmt_iter;
    }
    Ok((ret, stmt_iter))
}

// Returns the ir::Expr representing the current block
fn post_parse_block_statement(
    es_block: BlockStatement,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program, // for adding new structs/functions if necessary
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    struct DummyScopePrefixEmitter {}
    impl ScopePrefixEmitter for DummyScopePrefixEmitter {
        fn emit<I: Iterator<Item = (Node, HashMap<String, Option<String>>)>>(
            self,
            _out_sequence: &mut Vec<ir::Expr>, // the output vec to append to
            _parse_ctx: &mut ParseState,
            stmt_iter: I,
            _new_depth: usize,      // new depth
            _new_num_locals: usize, // new num locals
            _filename: Option<&str>,
            _ir_program: &mut ir::Program,
        ) -> Result<I, CompileMessage<ParseProgramError>> {
            Ok(stmt_iter)
        }
    }
    post_parse_scope(
        es_block,
        loc,
        parse_ctx,
        DummyScopePrefixEmitter {},
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

fn make_function_body<S: Scope>(
    es_func: S,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    ir_params_without_closure: Box<[ir::VarType]>,
    closure_count: usize, // 0 = no closure, 1 = has closure
    depth: usize,
    filename: Option<&str>,
    ir_program: &mut ir::Program, // for adding new structs/functions if necessary
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // setup the function body:
    // params are copied into position as if:
    // let local#0 = param#0;
    // let local#1 = param#1; ...
    // where each param has the correct param type specified in the signature
    // and each local has the type Any and is placed on the stack or heap as per usual address-takenness

    struct FuncScopePrefixEmitter {
        ir_params: Box<[ir::VarType]>,
        closure_count: usize,
    }
    impl FuncScopePrefixEmitter {
        fn post_parse_params_recurse<
            I: Iterator<Item = (Node, HashMap<String, Option<String>>)>,
            J: Iterator<Item = (usize, ir::VarType)>,
        >(
            j: usize,
            closure_count: usize,
            ir_vartype: ir::VarType,
            more_ir_vartype_iter: J,
            parse_ctx: &mut ParseState,
            more_stmt_attr_iter: I,
            depth: usize,
            num_locals: usize, // current number of IR locals
            filename: Option<&str>,
            ir_program: &mut ir::Program,
        ) -> Result<(ir::Expr, (J, I)), CompileMessage<ParseProgramError>> {
            let varlocid = VarLocId {
                depth: depth,
                index: j,
            };
            let rhs_expr = ir::Expr {
                vartype: Some(ir_vartype),
                kind: ir::ExprKind::VarName {
                    source: ir::TargetExpr::Local {
                        localidx: closure_count + j,
                        next: None,
                    },
                },
            };

            post_parse_decl_helper(
                varlocid,
                move |_, _, _, _, _| Ok(rhs_expr),
                (more_ir_vartype_iter, more_stmt_attr_iter),
                parse_ctx,
                |(mut more_ir_vartype_iter, mut more_stmt_attr_iter),
                 parse_ctx,
                 depth,
                 num_locals,
                 filename,
                 ir_program| {
                    let mut ret: Vec<ir::Expr> = Vec::new();
                    while let Some((j, ir_vartype)) = more_ir_vartype_iter.next() {
                        let (ir_expr2, (var_2, stmt_2)) = Self::post_parse_params_recurse(
                            j,
                            closure_count,
                            ir_vartype,
                            more_ir_vartype_iter,
                            parse_ctx,
                            more_stmt_attr_iter,
                            depth,
                            num_locals,
                            filename,
                            ir_program,
                        )?;
                        ret.push(ir_expr2);
                        more_ir_vartype_iter = var_2;
                        more_stmt_attr_iter = stmt_2;
                    }
                    let (mut ir_exprs, ret_more_stmt_attr_iter) = add_remaining_stmts(
                        more_stmt_attr_iter,
                        parse_ctx,
                        depth,
                        num_locals,
                        filename,
                        ir_program,
                    )?;
                    ret.append(&mut ir_exprs);
                    Ok((ret, (more_ir_vartype_iter, ret_more_stmt_attr_iter)))
                },
                depth,
                num_locals,
                filename,
                ir_program,
            )
        }
    }
    impl ScopePrefixEmitter for FuncScopePrefixEmitter {
        fn emit<I: Iterator<Item = (Node, HashMap<String, Option<String>>)>>(
            self,
            out_sequence: &mut Vec<ir::Expr>, // the output vec to append to
            parse_ctx: &mut ParseState,
            mut stmt_iter: I,
            new_depth: usize,      // new depth
            new_num_locals: usize, // new num locals
            filename: Option<&str>,
            ir_program: &mut ir::Program,
        ) -> Result<I, CompileMessage<ParseProgramError>> {
            // Processing assignment exprs here
            let mut ir_param_iter = self.ir_params.into_iter().copied().enumerate().fuse();

            // insert all the assignment/declarations, consuming the rest of the stmt_iters if necessary
            while let Some((i, ir_param)) = ir_param_iter.next() {
                let (ir_expr, (new_ir_param_iter, new_stmt_attr_iter)) =
                    Self::post_parse_params_recurse(
                        i,
                        self.closure_count,
                        ir_param,
                        ir_param_iter,
                        parse_ctx,
                        stmt_iter,
                        new_depth,
                        new_num_locals,
                        filename,
                        ir_program,
                    )?;
                out_sequence.push(ir_expr);
                ir_param_iter = new_ir_param_iter;
                stmt_iter = new_stmt_attr_iter;
            }

            Ok(stmt_iter)
        }
    }

    let initial_decl_count = closure_count + ir_params_without_closure.len();

    post_parse_scope(
        es_func,
        loc,
        parse_ctx,
        FuncScopePrefixEmitter {
            ir_params: ir_params_without_closure,
            closure_count: closure_count,
        },
        depth,
        initial_decl_count, // the first few params are function parameters, implicitly declared
        filename,
        ir_program,
    )
}

fn post_parse_direct_function(
    mut es_func: FunctionDeclaration,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // we don't care about the existing number of locals, because we start from zero when in a new function
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(), CompileMessage<ParseProgramError>> {
    // Note: a direct function has no capture var, so the ir param list is exactly the list in es_func.direct_props.
    assert!(es_func.captured_vars.is_empty());

    // the current function that we are emitting (the entry was already created by the enclosing block)
    // the std::mem::take is safe here because make_function_body() does not use direct_props.
    let (ir_params, ir_funcidx) = std::mem::take(&mut es_func.direct_props).unwrap();
    let num_params = es_func.params.len();
    assert!(num_params == ir_params.len());
    //let es_params = std::mem::take(&mut es_func.params);

    let undo_ctx = parse_ctx.enter_closure(Box::new([])); // new closure with no non-global Target entries in the parse_ctx

    let ir_func_body: ir::Expr = make_function_body(
        es_func,
        loc,
        parse_ctx,
        ir_params.clone(),
        0,
        depth,
        filename,
        ir_program,
    )?;

    parse_ctx.leave_closure(undo_ctx);

    let curr_func: &mut ir::Func = &mut ir_program.funcs[ir_funcidx];
    curr_func.params = ir_params;
    curr_func.result = Some(ir::VarType::Any);
    curr_func.expr = ir_func_body;
    Ok(())
}

fn post_parse_function<Func: Function>(
    mut es_func: Func,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize,
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // firstly, prep the closure

    // todo! only generate the closure if there is at least one captured variable (otherwise, the function can have closure_count==0)
    // move the vector out so we don't unnecessarily allocate memory
    let es_captured_vars: Vec<VarLocId> = std::mem::take(es_func.captured_vars_mut());
    let ir_params_without_closure: Box<[ir::VarType]> = es_func
        .params_mut()
        .iter()
        .map(|_| ir::VarType::Any)
        .collect();

    // the output sequence; this will go into a Declaration expression
    let mut sequence: Vec<ir::Expr> = Vec::new();

    // declare a new struct type
    // note: for address-taken variables, we only store the enclosing struct in the closure
    // and we dedup the set of enclosing structs
    let struct_idx = ir_program.struct_types.len();
    let ir_params_with_closure: Box<[ir::VarType]> = std::iter::once(ir::VarType::StructT {
        typeidx: struct_idx,
    })
    .chain(es_func.params_mut().iter().map(|_| ir::VarType::Any))
    .collect();

    // emit the struct allocation
    sequence.push(ir::Expr {
        vartype: Some(ir::VarType::Undefined),
        kind: ir::ExprKind::Assign {
            target: ir::TargetExpr::Local {
                // this assumes a new declaration of a local, which we will do later
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

    // Do a few things including calculating the struct def and emitting assignment statements for the captured vars
    // this will become the closure struct definition:
    let mut struct_def: Vec<ir::VarType> = Vec::new();
    // this will become a map from es_captured_vars index to struct_def index
    let mut struct_field_map: Box<[usize]> = es_captured_vars.iter().map(|_| usize::MAX).collect();
    for (i, varlocid) in es_captured_vars.iter().enumerate() {
        if let ir::TargetExpr::Local { localidx, next } = parse_ctx.get_target(varlocid).unwrap() {
            match next {
                None => {
                    // not address taken
                    let field_idx = struct_def.len();
                    struct_field_map[i] = struct_def.len();
                    struct_def.push(ir::VarType::Any);
                    // the assignment statement
                    sequence.push(ir::Expr {
                        vartype: Some(ir::VarType::Undefined),
                        kind: ir::ExprKind::Assign {
                            target: ir::TargetExpr::Local {
                                // this assumes a new declaration of a local, which we will do later
                                localidx: num_locals,
                                next: Some(Box::new(ir::StructField {
                                    typeidx: struct_idx,
                                    fieldidx: field_idx,
                                    next: None,
                                })),
                            },
                            expr: Box::new(ir::Expr {
                                vartype: Some(ir::VarType::Any),
                                kind: ir::ExprKind::VarName {
                                    source: ir::TargetExpr::Local {
                                        localidx: *localidx,
                                        next: None,
                                    },
                                },
                            }),
                        },
                    });
                }
                Some(_) => {}
            }
        } else {
            // globals should not be captured
            pppanic();
        }
    }
    {
        let mut prev_var_depth = usize::MAX;
        for (i, varlocid) in es_captured_vars.iter().enumerate() {
            if let ir::TargetExpr::Local { localidx, next } =
                parse_ctx.get_target(varlocid).unwrap()
            {
                match next {
                    None => {}
                    Some(box_structfield) => {
                        // address taken
                        if varlocid.depth != prev_var_depth {
                            // since each depth has only one scope struct, we know that it's the same struct
                            // (since es_captured_vars is already sorted by pre_parse())
                            let field_idx = struct_def.len();
                            struct_field_map[i] = struct_def.len();
                            struct_def.push(ir::VarType::StructT {
                                typeidx: box_structfield.typeidx,
                            });
                            // the assignment statement
                            sequence.push(ir::Expr {
                                vartype: Some(ir::VarType::Undefined),
                                kind: ir::ExprKind::Assign {
                                    target: ir::TargetExpr::Local {
                                        // this assumes a new declaration of a local, which we will do later
                                        localidx: num_locals,
                                        next: Some(Box::new(ir::StructField {
                                            typeidx: struct_idx,
                                            fieldidx: field_idx,
                                            next: None,
                                        })),
                                    },
                                    expr: Box::new(ir::Expr {
                                        vartype: Some(ir::VarType::StructT {
                                            typeidx: box_structfield.typeidx,
                                        }),
                                        kind: ir::ExprKind::VarName {
                                            source: ir::TargetExpr::Local {
                                                localidx: *localidx,
                                                next: None, // None - because we need to store the enclosing struct instance to do pass-by-reference
                                            },
                                        },
                                    }),
                                },
                            });
                            prev_var_depth = varlocid.depth;
                        } else {
                            assert!(!struct_def.is_empty());
                            struct_field_map[i] = struct_def.len() - 1;
                        }
                    }
                }
            } else {
                // globals should not be captured
                pppanic();
            }
        }
    }

    // declare the layout of the new struct type
    ir_program.struct_types.push(struct_def.into_boxed_slice());

    // generate the new ir::VarTypes for VarCtx
    // the TargetExprs for access from inside the new function
    let new_targets_for_parse_ctx: Box<[(VarLocId, ir::TargetExpr)]> = es_captured_vars
        .into_iter()
        .enumerate()
        .map(|(i, varlocid)| {
            if let ir::TargetExpr::Local { localidx: _, next } =
                parse_ctx.get_target(&varlocid).unwrap()
            {
                (
                    varlocid,
                    ir::TargetExpr::Local {
                        localidx: 0, // the closure struct's localidx is 0
                        next: Some(Box::new(ir::StructField {
                            typeidx: struct_idx,
                            fieldidx: struct_field_map[i], // todo: this is wrong
                            // later note: now i'm not sure why the previous line is wrong
                            next: next.clone(),
                        })),
                    },
                )
            } else {
                pppanic();
            }
        })
        .collect();

    // enter the closure context
    let undo_ctx = parse_ctx.enter_closure(new_targets_for_parse_ctx);

    // encode the function function body using the modified parse_ctx
    let ir_func_body: ir::Expr = make_function_body(
        es_func,
        loc,
        parse_ctx,
        ir_params_without_closure,
        1,
        depth,
        filename,
        ir_program,
    )?;

    // leave the closure context
    parse_ctx.leave_closure(undo_ctx);

    // add the function to the ir_program
    let ir_funcidx = ir_program.funcs.len();
    ir_program.funcs.push(ir::Func {
        params: ir_params_with_closure,
        result: Some(ir::VarType::Any),
        expr: ir_func_body,
        signature_filter: Default::default(),
    });

    // add the primfunc expr that will be returned (since it's the last item in the sequence)
    sequence.push(ir::Expr {
        vartype: Some(ir::VarType::Func),
        kind: ir::ExprKind::PrimFunc {
            funcidxs: Box::new([ir::OverloadEntry {
                funcidx: ir_funcidx,
                has_closure_param: true,
            }]),
            closure: Box::new(ir::Expr {
                vartype: Some(ir::VarType::StructT {
                    typeidx: struct_idx,
                }),
                kind: ir::ExprKind::VarName {
                    source: ir::TargetExpr::Local {
                        localidx: num_locals,
                        next: None,
                    },
                },
            }),
        },
    });

    // the returned expression (which returns the func variable)
    let ret = ir::Expr {
        vartype: Some(ir::VarType::Func),
        kind: ir::ExprKind::Declaration {
            // the declaration of the temporary closure struct that we store in the returned func
            local: ir::VarType::StructT {
                typeidx: struct_idx,
            },
            expr: Box::new(ir::Expr {
                vartype: Some(ir::VarType::Func),
                kind: ir::ExprKind::Sequence { content: sequence },
            }),
        },
    };

    Ok(ret)
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
                // direct func declarations do not generate any ir::Expr in the current context
                // the current block is only used for scoping rules.
                post_parse_direct_func_decl(
                    func_decl,
                    es_node.loc,
                    parse_ctx,
                    depth,
                    num_locals,
                    filename,
                    ir_program,
                )?;
                Ok((make_prim_undefined(), more_stmt_attr_iter))
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
        NodeKind::EmptyStatement(_) => Ok((make_prim_undefined(), more_stmt_attr_iter)), // todo! IR optimisation should prune empty statments
        _ => pppanic(),
    }
}

fn post_parse_toplevel_statement(
    es_node: Node,
    attributes: HashMap<String, Option<String>>,
    parse_ctx: &mut ParseState,
    deps: &[&ParseState],
    filename: Option<&str>,
    ir_program: &mut ir::Program,
    exports: &mut ParseState,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // We do not validate constraints or anything else (pre_parse should have done it)
    // This function should be like post_parse_statement(), but all declarations
    // become global assignments (ir_program.globals should be modified by this function to add the new globals)
    // Also import statments to add names into `parse_ctx`, while export statements to add names to `exports`.

    // we do not validate constraints or anything else (pre_parse should have done it)
    match es_node.kind {
        NodeKind::ExpressionStatement(stmt) => {
            post_parse_expr_statement(stmt, es_node.loc, parse_ctx, 0, 0, filename, ir_program)
        }
        NodeKind::BlockStatement(block) => {
            post_parse_block_statement(block, es_node.loc, parse_ctx, 0, 0, filename, ir_program)
        }
        NodeKind::ReturnStatement(stmt) => {
            post_parse_return_statement(stmt, es_node.loc, parse_ctx, 0, 0, filename, ir_program)
        }
        NodeKind::IfStatement(stmt) => {
            post_parse_if_statement(stmt, es_node.loc, parse_ctx, 0, 0, filename, ir_program)
        }
        NodeKind::FunctionDeclaration(func_decl) => {
            if attributes.get("direct").is_some() {
                // direct func declarations do not generate any ir::Expr in the current context
                // the current block is only used for scoping rules.
                post_parse_direct_func_decl(
                    func_decl,
                    es_node.loc,
                    parse_ctx,
                    0,
                    0,
                    filename,
                    ir_program,
                )?;
                Ok(make_prim_undefined())
            } else {
                let varlocid = as_varlocid(as_id_ref(&*func_decl.id).prevar.unwrap());
                let rhs_expr: ir::Expr = post_parse_function(
                    func_decl,
                    es_node.loc,
                    parse_ctx,
                    0,
                    0,
                    filename,
                    ir_program,
                )?;

                Ok(ir::Expr {
                    vartype: Some(ir::VarType::Undefined),
                    kind: ir::ExprKind::Assign {
                        target: parse_ctx.get_target(&varlocid).unwrap().clone(),
                        expr: Box::new(rhs_expr),
                    },
                })
            }
        }

        NodeKind::VariableDeclaration(var_decl) => {
            post_parse_toplevel_var_decl(var_decl, es_node.loc, parse_ctx, filename, ir_program)
        }
        NodeKind::ImportDeclaration(import_decl) => Ok(make_prim_undefined()),
        NodeKind::ExportNamedDeclaration(export_decl) => {
            post_parse_toplevel_export_decl(
                export_decl,
                es_node.loc,
                parse_ctx,
                filename,
                exports,
            )?;
            Ok(make_prim_undefined())
        }
        NodeKind::EmptyStatement(_) => Ok(make_prim_undefined()), // todo! IR optimisation should prune empty statments
        _ => pppanic(),
    }
}

/*fn post_parse_toplevel_import_decl(
    es_import_decl: ImportDeclaration,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    dep: &ParseState,
    filename: Option<&str>,
) -> Result<(), CompileMessage<ParseProgramError>> {
    for import_spec_node in es_import_decl.specifiers {
        let import_spec = as_import_spec(import_spec_node);
        let source_id = as_id(*import_spec.source);
        let local_id = as_id(*import_spec.local);

    }
}*/

fn post_parse_toplevel_export_decl(
    es_export_decl: ExportNamedDeclaration,
    loc: Option<esSL>,
    parse_ctx: &ParseState,
    filename: Option<&str>,
    exports: &mut ParseState,
) -> Result<(), CompileMessage<ParseProgramError>> {
    for export_spec_node in es_export_decl.specifiers {
        let export_spec = as_export_spec(export_spec_node);
        let exported_id = as_id(*export_spec.exported);
        let local_id = as_id(*export_spec.local);
        match local_id.prevar.unwrap() {
            PreVar::Target(varlocid) => {
                exports.add_target(varlocid, parse_ctx.get_target(&varlocid).unwrap().clone());
            }
            PreVar::Direct => {
                let os: OverloadSet<_> = parse_ctx
                    .get_direct(local_id.name.as_str())
                    .unwrap()
                    .clone();
                exports.add_direct(local_id.name, os);
            }
        }
    }
    Ok(())
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
                *es_return.argument.unwrap(),
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
    // also emits a type check to ensure that the conditional is boolean type

    let cond_loc: ir::SourceLocation = as_ir_sl(&es_if.test.loc, 0 /*FILE*/);

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
            true_expr: Box::new({
                let (block_stmt, loc) = as_block_statement_with_loc(*es_if.consequent);
                post_parse_block_statement(
                    block_stmt, loc, parse_ctx, depth, num_locals, filename, ir_program,
                )?
            }),
            false_expr: Box::new({
                let (block_stmt, loc) = as_block_statement_with_loc(*es_if.alternate.unwrap());
                post_parse_block_statement(
                    block_stmt, loc, parse_ctx, depth, num_locals, filename, ir_program,
                )?
            }),
        },
    })
}

fn post_parse_direct_func_decl(
    es_func_decl: FunctionDeclaration,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(), CompileMessage<ParseProgramError>> {
    // This emits a new ir::Func in the ir_program, with the correct signature specified in es_func_decl (without closure).
    // This doesn't return any success value, because es_func_decl already contains the ir::FuncIdx to write to,
    // and the signature to use.
    post_parse_direct_function(
        es_func_decl,
        loc,
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )

    // Also, nothing to add to parse_ctx because everything necessary have already been added in the post_parse_block_statement()
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
    let varlocid = as_varlocid(as_id_ref(&*es_func_decl.id).prevar.unwrap());

    post_parse_decl_helper(
        varlocid,
        move |parse_ctx, depth, num_locals, filename, ir_program| {
            post_parse_function(
                es_func_decl,
                loc,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )
        },
        more_stmt_attr_iter,
        parse_ctx,
        move |more_stmt_attr_iter, parse_ctx, depth, num_locals, filename, ir_program| {
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
    mut more_stmt_attr_iter: I,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, I), CompileMessage<ParseProgramError>> {
    // Since post_parse_statement only allows returning a single ir::Expr, we have to stuff these things into a sequence.
    let mut ret: Vec<ir::Expr> = Vec::new();
    let mut var_decr_iter = es_var_decl
        .declarations
        .into_iter()
        .map(|decr_node| as_var_decr(decr_node))
        .fuse();

    // insert all the var declarators, consuming the rest of the stmt_attrs if necessary
    while let Some(es_var_decr) = var_decr_iter.next() {
        let (ir_expr, (new_var_decr_iter, new_stmt_attr_iter)) = post_parse_var_decr_recurse(
            es_var_decr,
            var_decr_iter,
            parse_ctx,
            more_stmt_attr_iter,
            depth,
            num_locals,
            filename,
            ir_program,
        )?;
        ret.push(ir_expr);
        var_decr_iter = new_var_decr_iter;
        more_stmt_attr_iter = new_stmt_attr_iter;
    }

    // This is an optimisation... most of the time we won't need a sequence because there is only one declaration.
    assert!(!ret.is_empty());
    Ok((
        if ret.len() == 1 {
            // if there is only one declaration, just return it
            ret.remove(0)
        } else {
            // if there are more than one declarations, pack it into a sequence and return the sequence

            ret.push(make_prim_undefined());

            let ir_seq_expr = ir::Expr {
                vartype: Some(ir::VarType::Undefined),
                kind: ir::ExprKind::Sequence { content: ret },
            };

            ir_seq_expr
        },
        more_stmt_attr_iter,
    ))
}

fn post_parse_var_decr_recurse<
    I: Iterator<Item = (Node, HashMap<String, Option<String>>)>,
    J: Iterator<Item = VariableDeclarator>,
>(
    es_var_decr: VariableDeclarator,
    more_var_decr_iter: J,
    parse_ctx: &mut ParseState,
    more_stmt_attr_iter: I,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, (J, I)), CompileMessage<ParseProgramError>> {
    let varlocid = as_varlocid(as_id(*es_var_decr.id).prevar.unwrap());
    let init_expr = *es_var_decr.init.unwrap();

    post_parse_decl_helper(
        varlocid,
        move |parse_ctx, depth, num_locals, filename, ir_program| {
            post_parse_expr(
                init_expr, parse_ctx, depth, num_locals, filename, ir_program,
            )
        },
        (more_var_decr_iter, more_stmt_attr_iter),
        parse_ctx,
        move |(mut more_var_decr_iter, mut more_stmt_attr_iter),
              parse_ctx,
              depth,
              num_locals,
              filename,
              ir_program| {
            let mut ret: Vec<ir::Expr> = Vec::new();
            while let Some(es_var_decr) = more_var_decr_iter.next() {
                let (ir_expr2, (var_2, stmt_2)) = post_parse_var_decr_recurse(
                    es_var_decr,
                    more_var_decr_iter,
                    parse_ctx,
                    more_stmt_attr_iter,
                    depth,
                    num_locals,
                    filename,
                    ir_program,
                )?;
                ret.push(ir_expr2);
                more_var_decr_iter = var_2;
                more_stmt_attr_iter = stmt_2;
            }
            let (mut ir_exprs, ret_more_stmt_attr_iter) = add_remaining_stmts(
                more_stmt_attr_iter,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?;
            ret.append(&mut ir_exprs);
            Ok((ret, (more_var_decr_iter, ret_more_stmt_attr_iter)))
        },
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

/**
 * A helper function to emit a ExprKind::Declaration.
 * Used by VariableDeclaration and FunctionDeclaration (Target kind).
 * `more` will only be called if we actually create a declaration (i.e. it was not previously already created due to address-takenness)
 */
fn post_parse_decl_helper<
    R,
    F: FnOnce(
        R,
        &mut ParseState,
        usize,
        usize,
        Option<&str>,
        &mut ir::Program,
    ) -> Result<(Vec<ir::Expr>, R), CompileMessage<ParseProgramError>>,
    G: FnOnce(
        &mut ParseState,
        usize,
        usize,
        Option<&str>,
        &mut ir::Program,
    ) -> Result<ir::Expr, CompileMessage<ParseProgramError>>,
>(
    varlocid: VarLocId,
    es_rhs_expr_maker: G,
    forwarded: R,
    parse_ctx: &mut ParseState,
    more: F,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<(ir::Expr, R), CompileMessage<ParseProgramError>> {
    if let Some(target_expr) = parse_ctx.get_target(&varlocid) {
        // target already exists... it must be address-taken
        // so emit an assignment statement with that target_expr
        let ret_expr = ir::Expr {
            vartype: Some(ir::VarType::Undefined),
            kind: ir::ExprKind::Assign {
                target: target_expr.clone(),
                expr: Box::new(es_rhs_expr_maker(
                    parse_ctx, depth, num_locals, filename, ir_program,
                )?),
            },
        };
        Ok((ret_expr, forwarded))
    } else {
        // target does not exist... it is not address-taken
        // so we create a new local variable

        let new_num_locals = num_locals + 1;

        let mut sequence: Vec<ir::Expr> = Vec::new();
        sequence.push(ir::Expr {
            vartype: Some(ir::VarType::Undefined),
            kind: ir::ExprKind::Assign {
                target: ir::TargetExpr::Local {
                    localidx: num_locals,
                    next: None,
                },
                expr: Box::new(es_rhs_expr_maker(
                    parse_ctx,
                    depth,
                    new_num_locals,
                    filename,
                    ir_program,
                )?),
            },
        });

        let undo_ctx = parse_ctx.add_target(
            varlocid,
            ir::TargetExpr::Local {
                localidx: num_locals,
                next: None,
            },
        );

        let (mut ir_exprs, ret_fwd) = more(
            forwarded,
            parse_ctx,
            depth,
            new_num_locals,
            filename,
            ir_program,
        )?;
        sequence.append(&mut ir_exprs);

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
            ret_fwd,
        ))
    }
}

fn post_parse_toplevel_var_decl(
    es_var_decl: VariableDeclaration,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // Since post_parse_statement only allows returning a single ir::Expr, we have to stuff these things into a sequence.
    let mut ret: Vec<ir::Expr> = es_var_decl
        .declarations
        .into_iter()
        .map(|decr_node| {
            let es_var_decr: VariableDeclarator = as_var_decr(decr_node);
            let varlocid = as_varlocid(as_id(*es_var_decr.id).prevar.unwrap());
            let rhs_expr: ir::Expr = post_parse_expr(
                *es_var_decr.init.unwrap(),
                parse_ctx,
                0,
                0,
                filename,
                ir_program,
            )?;
            Ok(ir::Expr {
                vartype: Some(ir::VarType::Undefined),
                kind: ir::ExprKind::Assign {
                    target: parse_ctx.get_target(&varlocid).unwrap().clone(),
                    expr: Box::new(rhs_expr),
                },
            })
        })
        .collect::<Result<Vec<ir::Expr>, CompileMessage<ParseProgramError>>>()?;

    // This is an optimisation... most of the time we won't need a sequence because there is only one declaration.
    assert!(!ret.is_empty());
    Ok(if ret.len() == 1 {
        // if there is only one declaration, just return it
        ret.remove(0)
    } else {
        // if there are more than one declarations, pack it into a sequence and return the sequence

        ret.push(make_prim_undefined());

        let ir_seq_expr = ir::Expr {
            vartype: Some(ir::VarType::Undefined),
            kind: ir::ExprKind::Sequence { content: ret },
        };

        ir_seq_expr
    })
}

fn post_parse_expr(
    es_expr: Node,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    match es_expr.kind {
        NodeKind::Identifier(es_id) => post_parse_varname(
            es_id,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::Literal(es_literal) => post_parse_literal(
            es_literal,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::ArrowFunctionExpression(es_arrowfunc) => post_parse_function(
            es_arrowfunc,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::UnaryExpression(unary_expr) => post_parse_unary_expr(
            unary_expr,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::BinaryExpression(binary_expr) => post_parse_binary_expr(
            binary_expr,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::LogicalExpression(logical_expr) => post_parse_logical_expr(
            logical_expr,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::AssignmentExpression(assign_expr) => post_parse_assign_expr(
            assign_expr,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::ConditionalExpression(cond_expr) => post_parse_cond_expr(
            cond_expr,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        NodeKind::CallExpression(call_expr) => post_parse_call_expr(
            call_expr,
            es_expr.loc,
            parse_ctx,
            depth,
            num_locals,
            filename,
            ir_program,
        ),
        _ => pppanic(),
    }
}

fn post_parse_varname(
    es_id: Identifier,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    match es_id.prevar.unwrap() {
        PreVar::Target(varlocid) => {
            // it's a Target, so return a ExprKind::VarName containing the TargetExpr
            Ok(ir::Expr {
                vartype: Some(ir::VarType::Any),
                kind: ir::ExprKind::VarName {
                    source: parse_ctx.get_target(&varlocid).unwrap().clone(),
                },
            })
        }
        PreVar::Direct => {
            // it's a direct, so we have to synthesise a wrapper func for it
            post_parse_direct_varname(
                es_id.name.as_str(),
                loc,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )
        }
    }
}

fn post_parse_direct_varname(
    name: &str,
    _loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    _depth: usize,
    _num_locals: usize, // current number of IR locals
    _filename: Option<&str>,
    _ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    let funcidxs: Box<[ir::OverloadEntry]> = parse_ctx
        .get_direct(name)
        .as_ref()
        .unwrap()
        .signatures
        .iter()
        .map(|(_, funcidx)| ir::OverloadEntry {
            funcidx: *funcidx,
            has_closure_param: false,
        })
        .collect();
    Ok(ir::Expr {
        vartype: Some(ir::VarType::Func),
        kind: ir::ExprKind::PrimFunc {
            funcidxs: funcidxs,
            closure: Box::new(make_prim_undefined()),
        },
    })
}

fn post_parse_literal(
    es_literal: Literal,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    match es_literal.value {
        LiteralValue::String(string_val) => Ok(ir::Expr {
            vartype: Some(ir::VarType::String),
            kind: ir::ExprKind::PrimString { val: string_val },
        }),
        LiteralValue::Boolean(bool_val) => Ok(ir::Expr {
            vartype: Some(ir::VarType::Boolean),
            kind: ir::ExprKind::PrimBoolean { val: bool_val },
        }),
        LiteralValue::Number(number_val) => Ok(ir::Expr {
            vartype: Some(ir::VarType::Number),
            kind: ir::ExprKind::PrimNumber { val: number_val },
        }),
        _ => pppanic(),
    }
}

fn post_parse_unary_expr(
    es_unary_expr: UnaryExpression,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // operators are Direct functions
    let opt_func_name: Option<&str> =
        builtins::resolve_unary_operator(es_unary_expr.operator.as_str());
    let op_str = es_unary_expr.operator;
    let func_name = opt_func_name.ok_or_else(|| {
        CompileMessage::new_error(
            loc.into_sl(filename).to_owned(),
            ParseProgramError::SourceRestrictionUnaryOperatorError(op_str),
        )
    })?;
    post_parse_direct_call_helper(
        func_name,
        Box::new([*es_unary_expr.argument]),
        loc,
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

fn post_parse_binary_expr(
    es_binary_expr: BinaryExpression,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // operators are Direct functions
    let opt_func_name: Option<&str> =
        builtins::resolve_binary_operator(es_binary_expr.operator.as_str());
    let op_str = es_binary_expr.operator;
    let func_name = opt_func_name.ok_or_else(|| {
        CompileMessage::new_error(
            loc.into_sl(filename).to_owned(),
            ParseProgramError::SourceRestrictionBinaryOperatorError(op_str),
        )
    })?;
    post_parse_direct_call_helper(
        func_name,
        Box::new([*es_binary_expr.left, *es_binary_expr.right]),
        loc,
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

fn post_parse_logical_expr(
    es_logical_expr: LogicalExpression,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // operators are Direct functions
    let opt_func_name: Option<&str> =
        builtins::resolve_logical_operator(es_logical_expr.operator.as_str());
    let op_str = es_logical_expr.operator;
    let func_name = opt_func_name.ok_or_else(|| {
        CompileMessage::new_error(
            loc.into_sl(filename).to_owned(),
            ParseProgramError::SourceRestrictionLogicalOperatorError(op_str),
        )
    })?;
    post_parse_direct_call_helper(
        func_name,
        Box::new([*es_logical_expr.left, *es_logical_expr.right]),
        loc,
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

fn post_parse_assign_expr(
    es_assign_expr: AssignmentExpression,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // check that it is '='.
    if es_assign_expr.operator != "=" {
        return Err(CompileMessage::new_error(
            loc.into_sl(filename).to_owned(),
            ParseProgramError::SourceRestrictionAssignmentOperatorError(es_assign_expr.operator),
        ));
    }
    // an assignment expr, that returns undefined
    let varlocid = as_varlocid(as_id(*es_assign_expr.left).prevar.unwrap());
    Ok(ir::Expr {
        vartype: Some(ir::VarType::Undefined),
        kind: ir::ExprKind::Assign {
            target: parse_ctx.get_target(&varlocid).unwrap().clone(),
            expr: Box::new(post_parse_expr(
                *es_assign_expr.right,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?),
        },
    })
}

fn post_parse_cond_expr(
    es_cond_expr: ConditionalExpression,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // see post_parse_if_statement() for comparison
    // Emits the ExprKind::Conditional.
    // also emits a type check to ensure that the conditional is boolean type

    let cond_loc: ir::SourceLocation = as_ir_sl(&es_cond_expr.test.loc, 0 /*FILE*/);

    // We synthesise the typecheck to ensure that the condition is a boolean
    // then add the true_expr and false_expr
    Ok(ir::Expr {
        vartype: Some(ir::VarType::Any),
        kind: ir::ExprKind::Conditional {
            cond: Box::new(ir::Expr {
                vartype: Some(ir::VarType::Boolean),
                kind: ir::ExprKind::TypeCast {
                    test: Box::new(post_parse_expr(
                        *es_cond_expr.test,
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
                *es_cond_expr.consequent,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?),
            false_expr: Box::new(post_parse_expr(
                *es_cond_expr.alternate,
                parse_ctx,
                depth,
                num_locals,
                filename,
                ir_program,
            )?),
        },
    })
}

fn post_parse_call_expr(
    es_call_expr: CallExpression,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // We do not need to differentiate between direct and indirect calls,
    // the IR knows how to do the optimisation.
    // TODO: should we detect direct calls anyway, because we have the post_parse_direct_call_helper()?
    // (since we wouldn't need to generate a lot of redundant things)

    let callee_loc: ir::SourceLocation = as_ir_sl(&es_call_expr.callee.loc, 0 /*FILE*/);

    // We synthesise the typecheck to ensure that the func is a Func.

    let func_any: ir::Expr = post_parse_expr(
        *es_call_expr.callee,
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )?;
    let func = ir::Expr {
        vartype: Some(ir::VarType::Func),
        kind: ir::ExprKind::TypeCast {
            test: Box::new(func_any),
            expected: ir::VarType::Func,
            create_narrow_local: true,
            true_expr: Box::new(ir::Expr {
                vartype: Some(ir::VarType::Func),
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
                    code: ir::error::ERROR_CODE_FUNCTION_APPLICATION_NOT_CALLABLE_TYPE,
                    location: callee_loc,
                },
            }),
        },
    };
    post_parse_call_func_with_params_helper(
        func,
        es_call_expr.arguments.into_iter(),
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )
}

fn post_parse_call_func_with_params_helper(
    func_expr: ir::Expr,
    args_iter: impl ExactSizeIterator<Item = Node> + DoubleEndedIterator<Item = Node>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    let args: Box<[ir::Expr]> = args_iter
        .map(|arg| post_parse_expr(arg, parse_ctx, depth, num_locals, filename, ir_program))
        .collect::<Result<Box<[ir::Expr]>, CompileMessage<ParseProgramError>>>()?;
    Ok(ir::Expr {
        vartype: Some(ir::VarType::Any),
        kind: ir::ExprKind::Appl {
            func: Box::new(func_expr),
            args: args,
        },
    })
}

fn post_parse_direct_call_helper(
    func_name: &str,
    params: Box<[Node]>,
    loc: Option<esSL>,
    parse_ctx: &mut ParseState,
    depth: usize,
    num_locals: usize, // current number of IR locals
    filename: Option<&str>,
    ir_program: &mut ir::Program,
) -> Result<ir::Expr, CompileMessage<ParseProgramError>> {
    // This is only for operators, and maybe other builtin things.
    // IR should propage constants in order to convert this to a real direct call (perhaps by considering cases based on the param types here)
    // TODO: look at post_parse_varname() to see how names are gotten.

    let primfunc_expr: ir::Expr = post_parse_direct_varname(
        func_name, loc, parse_ctx, depth, num_locals, filename, ir_program,
    )?;

    post_parse_call_func_with_params_helper(
        primfunc_expr,
        Vec::from(params).into_iter(),
        parse_ctx,
        depth,
        num_locals,
        filename,
        ir_program,
    )
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

fn as_var_decr_ref(es_node: &Node) -> &VariableDeclarator {
    if let NodeKind::VariableDeclarator(var_decr) = &es_node.kind {
        var_decr
    } else {
        pppanic();
    }
}

fn as_import_spec(es_node: Node) -> ImportSpecifier {
    if let NodeKind::ImportSpecifier(import_spec) = es_node.kind {
        import_spec
    } else {
        pppanic();
    }
}

fn as_import_spec_ref(es_node: &Node) -> &ImportSpecifier {
    if let NodeKind::ImportSpecifier(import_spec) = &es_node.kind {
        import_spec
    } else {
        pppanic();
    }
}

fn as_export_spec(es_node: Node) -> ExportSpecifier {
    if let NodeKind::ExportSpecifier(export_spec) = es_node.kind {
        export_spec
    } else {
        pppanic();
    }
}

fn as_block_statement_with_loc(es_node: Node) -> (BlockStatement, Option<esSL>) {
    if let NodeKind::BlockStatement(block_stmt) = es_node.kind {
        (block_stmt, es_node.loc)
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

fn as_id_ref(es_node: &Node) -> &Identifier {
    if let NodeKind::Identifier(id) = &es_node.kind {
        id
    } else {
        pppanic();
    }
}

// TODO: store both line and column, and make fileidx work.
fn as_ir_sl(opt_es_sl: &Option<SourceLocation>, fileidx: u32) -> ir::SourceLocation {
    let (start, end) = match opt_es_sl {
        Some(es_sl) => (es_sl.start.line as u32, es_sl.end.line as u32),
        None => (0, 0),
    };
    ir::SourceLocation {
        file: fileidx,
        start: start,
        end: end,
    }
}

#[feature(never_type)]
fn pppanic() -> ! {
    panic!("pre_parse() should have detected this error");
}
