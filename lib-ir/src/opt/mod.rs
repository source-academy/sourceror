mod inline;
mod landing_context;
mod propagate;
mod relabeller;
mod typecast;
mod unreachable;

use super::*;

/**
 * Main function to do mandatory optimizations for a program.
 * Mandatory optimizations are those that are required for the IR to function correctly.
 */
pub fn optimize_mandatory(mut program: Program, start_funcidx: usize) -> Program {
    let mut n: usize = 0;
    const TOTAL: usize = 2;
    loop {
        {
            let (new_program, changed) = unreachable::optimize(program, start_funcidx);
            program = new_program;
            if changed {
                n = 1;
            } else {
                n += 1;
            }
            if n == TOTAL {
                break;
            }
        }
        {
            let (new_program, changed) = typecast::optimize(program, start_funcidx);
            program = new_program;
            if changed {
                n = 1;
            } else {
                n += 1;
            }
            if n == TOTAL {
                break;
            }
        }
    }

    program
}

/**
 * Main function to do discretionary optimizations for a program.
 * start_funcidx: The funcidx from which to optimise (used for REPL where part of the program has already been optimised).
 */
pub fn optimize_all(mut program: Program, start_funcidx: usize) -> Program {
    let mut n: usize = 0;
    const TOTAL: usize = 2;
    loop {
        {
            let (new_program, changed) = propagate::optimize(program, start_funcidx);
            program = new_program;
            if changed {
                n = 0;
            } else {
                n += 1;
            }
            if n == TOTAL {
                break;
            }
        }
        {
            let (new_program, changed) = inline::optimize(program, start_funcidx);
            program = new_program;
            if changed {
                n = 0;
            } else {
                n += 1;
            }
            if n == TOTAL {
                break;
            }
        }
    }

    program
}

/**
 * Returns the type wide enough to contain both the given two types.
 */
fn union_type(first: Option<VarType>, second: Option<VarType>) -> Option<VarType> {
    if first.is_none() {
        return second;
    }
    if second.is_none() {
        return first;
    }
    // now both first and second are not None
    let u_first = first.unwrap();
    let u_second = second.unwrap();
    if u_first == u_second {
        return Some(u_first);
    }
    Some(VarType::Any)
}

/**
 * Helper function that returns true if dest got changed
 */
fn useful_update<T: Eq>(dest: &mut T, source: T) -> bool {
    if *dest == source {
        false
    } else {
        *dest = source;
        true
    }
}
