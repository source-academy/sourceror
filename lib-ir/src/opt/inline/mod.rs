use super::*;
use relabeller::relabel;
use relabeller::Relabeller;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::ptr::NonNull;
mod relabel_site;
use relabel_site::relabel_inline_func;
mod populate_properties;
use populate_properties::populate_properties_func;

const INLINING_MAX_ALLOWABLE_COST: usize = 40;

/**
 * Discretionary optimisation to inline function calls where beneficial.
 * The second return value is true if the program got changed, or false otherwise.
 * The algorithm is designed to be stable - repeatedly running the algorithm will eventually reach a stable state, even with future devirtualisation.
 *
 * The algorithm works as follows:
 * 1. Let F be the set of functions that do not contain indirect calls (only such functions are ever considered, in order to guarantee stability).
 * 2. Let G be the call graph built from F (add an edge from f to g iff f contains a direct call to g (only actual direct calls, not transitive)).
 * 3. If there is at least one function in G that has zero outgoing edges:
 *   3a. Take any such function f, remove it from G.
 *     3ai. If there is only one direct call and no indirect calls to f, inline the call (at this point we can also remove f from the program).
 *     3aii. Otherwise, if f has a cost that is less than C, then inline all direct calls to f.
 *     3aiii. Otherwise, do nothing.
 *   3b. Go to step 3.
 * 4. If G is non empty (i.e. there is a cycle):
 *   4a. Remove the function with largest cost from G.
 *   4b. Go to step 3.
 * 5. Done.
 *
 * Only these Expr nodes count to the cost: PrimNumber, PrimBoolean, PrimStructT, PrimString, PrimFunc, VarName, Trap
 */
pub fn optimize(mut program: Program) -> (Program, bool) {
    let mut changed = false;

    // TODO: collect the FunctionProperties (including imports, which will never be inlining candidates)
    // those with None are not currently inlining candidates (i.e. not in G), those with Some are currently candidates
    let mut func_props: Box<[Option<FunctionProperties>]> = program
        .imports
        .iter()
        .map(|_| None)
        .chain(
            program
                .funcs
                .iter()
                .map(|_| Some(FunctionProperties::new())), // TODO: wrong, because a func has to add properties for other funcs
        )
        .collect();

    for (i, func) in program.funcs.iter_mut().enumerate() {
        populate_properties_func(program.imports.len() + i, func, &mut func_props);
    }

    let mut og_calls_pq: BinaryHeap<OutgoingPQItem> = func_props
        .iter()
        .enumerate()
        .filter_map(|(i, opt_fp)| {
            opt_fp.as_ref().map(|fp| OutgoingPQItem {
                num_calls: fp.num_calls,
                funcidx: i,
            })
        })
        .collect();
    let mut cost_pq: BinaryHeap<CostPQItem> = func_props
        .iter()
        .enumerate()
        .filter_map(|(i, opt_fp)| {
            opt_fp.as_ref().map(|fp| CostPQItem {
                cost: fp.cost,
                funcidx: i,
            })
        })
        .collect();

    // step 3
    while let Some(og_item) = og_calls_pq.pop() {
        if func_props[og_item.funcidx].is_some() {
            if og_item.num_calls == 0 {
                // this is a candidate for inlining, so we do step 3a.
                let mut fp = std::mem::replace(&mut func_props[og_item.funcidx], None).unwrap();
                assert!(fp.num_calls == 0);
                // at this point, we're guaranteed that no references to Exprs in the current function are being held,
                // because fp.num_calls == 0

                fn update_caller_func(
                    og_calls_pq: &mut BinaryHeap<OutgoingPQItem>,
                    cost_pq: &mut BinaryHeap<CostPQItem>,
                    func_props: &mut [Option<FunctionProperties>],
                    funcidx: FuncIdx,
                    additional_cost: usize,
                ) {
                    if let Some(caller_fp) = &mut func_props[funcidx] {
                        caller_fp.num_calls -= 1;
                        caller_fp.cost += additional_cost; // todo: this doesn't seem very exact
                        og_calls_pq.push(OutgoingPQItem {
                            num_calls: caller_fp.num_calls,
                            funcidx: funcidx,
                        });
                        cost_pq.push(CostPQItem {
                            cost: caller_fp.cost,
                            funcidx: funcidx,
                        });
                    }
                }

                if fp.parents.len() == 1 && !fp.has_indirect_calls {
                    // 3ai succeeds, we inline by 'moving' the current function into the caller
                    let (caller_funcidx, mut direct_call_expr, site) = fp.parents.pop().unwrap();
                    inline_by_destructive_move(
                        unsafe { direct_call_expr.as_mut() },
                        site,
                        std::mem::replace(program.get_func_mut(og_item.funcidx), Func::new()),
                    );
                    changed = true;
                    update_caller_func(
                        &mut og_calls_pq,
                        &mut cost_pq,
                        &mut func_props,
                        caller_funcidx,
                        fp.cost,
                    );
                } else if fp.cost <= INLINING_MAX_ALLOWABLE_COST {
                    // 3aii succeeds, we inline by usual copy
                    for (caller_funcidx, mut direct_call_expr, site) in fp.parents {
                        inline_by_copy(
                            unsafe { direct_call_expr.as_mut() },
                            site,
                            program.get_func(og_item.funcidx),
                        );
                        changed = true;
                        update_caller_func(
                            &mut og_calls_pq,
                            &mut cost_pq,
                            &mut func_props,
                            caller_funcidx,
                            fp.cost,
                        );
                    }
                }
            } else {
                // step 4
                loop {
                    let cost_item = cost_pq.pop().unwrap(); // cost_pq can't be empty here because og_calls_pq is not empty
                    if func_props[cost_item.funcidx].is_some() {
                        assert!(
                            func_props[cost_item.funcidx].as_ref().unwrap().cost == cost_item.cost
                        );
                        func_props[cost_item.funcidx] = None;
                        break;
                    }
                }
            }
        }
        // if it's None, then we should continue to get the next og_item
    }

    (program, changed)
}

struct FunctionProperties {
    parents: Vec<(FuncIdx, NonNull<Expr>, SiteProperties)>, // (funcidx, direct appl expr), list of incoming direct calls; second thing must be a DirectAppl that calls the current function
    num_calls: usize,         // number of outgoing direct calls to functions in G
    has_indirect_calls: bool, // whether this function's index has been taken (by a PrimFunc)
    cost: usize,              // the number of Expr nodes in this function
}
impl FunctionProperties {
    pub fn new() -> Self {
        Self {
            parents: Vec::new(),
            num_calls: 0,
            has_indirect_calls: false,
            cost: 0,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct SiteProperties {
    num_locals: usize, // number of locals declared before this Expr (includes function params)
                       //num_landings: usize, // number of landings declared before this Expr
}

impl SiteProperties {
    fn new() -> Self {
        Self {
            num_locals: 0,
            //num_landings: 0,
        }
    }
    fn new_with_amount(amount: usize) -> Self {
        Self {
            num_locals: amount,
            //num_landings: 0,
        }
    }
    fn with_local(self) -> Self {
        Self {
            num_locals: self.num_locals + 1,
            //num_landings: self.num_landings,
        }
    }
    /*fn with_landing(self) -> Self {
        Self {
            num_locals: self.num_locals,
            num_landings: self.num_landings + 1,
        }
    }*/
    fn num_locals(&self) -> usize {
        self.num_locals
    }
    /*fn num_landings(&self) -> usize {
        self.num_landings
    }*/
}

#[derive(Eq)]
struct OutgoingPQItem {
    num_calls: usize,
    funcidx: FuncIdx, // remember that this indexing includes imports
}
impl PartialOrd for OutgoingPQItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for OutgoingPQItem {
    // reversed comparison by num_calls, to make the smallest at the top
    fn cmp(&self, other: &Self) -> Ordering {
        self.num_calls.cmp(&other.num_calls).reverse()
    }
}
impl PartialEq for OutgoingPQItem {
    fn eq(&self, other: &Self) -> bool {
        self.num_calls == other.num_calls
    }
}

#[derive(Eq)]
struct CostPQItem {
    cost: usize,
    funcidx: FuncIdx, // remember that this indexing includes imports
}
impl PartialOrd for CostPQItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for CostPQItem {
    // largest cost on top
    fn cmp(&self, other: &Self) -> Ordering {
        self.cost.cmp(&other.cost)
    }
}
impl PartialEq for CostPQItem {
    fn eq(&self, other: &Self) -> bool {
        self.cost == other.cost
    }
}

fn inline_by_destructive_move(direct_call_expr: &mut Expr, site: SiteProperties, func: Func) {
    let actual_args = std::mem::take(as_direct_appl_args(direct_call_expr));
    let tmp_expr = func.expr;
    *direct_call_expr = wrap_declarations(
        Vec::from(actual_args).into_iter(),
        &func.params,
        site,
        |site| relabel_inline_func(tmp_expr, site).0,
    );
}

fn inline_by_copy(direct_call_expr: &mut Expr, site: SiteProperties, func: &Func) {
    let actual_args = std::mem::take(as_direct_appl_args(direct_call_expr));
    *direct_call_expr = wrap_declarations(
        Vec::from(actual_args).into_iter(),
        &func.params,
        site,
        |site| {
            let expr = func.expr.clone();
            relabel_inline_func(expr, site).0
        },
    );
}

/**
 * Wraps declarations on the Expr returned by f.
 * Actual args must be a subtype of the params.
 */
fn wrap_declarations<F: FnOnce(SiteProperties) -> Expr>(
    actual_args: impl Iterator<Item = Expr>,
    params: &[VarType],
    site: SiteProperties,
    f: F,
) -> Expr {
    fn wrap_decl_recursive<F: FnOnce(SiteProperties) -> Expr>(
        mut args_remaining: impl Iterator<Item = (Expr, VarType)>,
        args_relabeller: &mut Relabeller,
        f_site: SiteProperties,
        f: F,
    ) -> Expr {
        if let Some((mut expr, vartype)) = args_remaining.next() {
            // emit one declaration
            let contained_expr = args_relabeller.with_skipped_new(|args_relabeller| {
                wrap_decl_recursive(args_remaining, args_relabeller, f_site, f)
            });
            relabel(&mut expr, args_relabeller);
            Expr {
                vartype: contained_expr.vartype,
                kind: ExprKind::Declaration {
                    local: vartype,
                    init: Some(Box::new(expr)),
                    contained_expr: Box::new(contained_expr),
                },
            }
        } else {
            f(f_site)
        }
    }
    wrap_decl_recursive(
        actual_args.zip(params.iter().copied()),
        &mut Relabeller::new_with_identities((0..site.num_locals()).into_iter()),
        site,
        f,
    )
}

fn as_direct_appl_args(expr: &mut Expr) -> &mut Box<[Expr]> {
    if let ExprKind::DirectAppl { funcidx: _, args } = &mut expr.kind {
        args
    } else {
        panic!("Not a DirectAppl");
    }
}
