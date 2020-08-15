mod propagate;
mod relabeller;
mod typecast;
mod unreachable;

use super::*;

/**
 * Main function to do mandatory optimizations for a program.
 * Mandatory optimizations are those that are required for the IR to function correctly.
 */
pub fn optimize_mandatory(mut program: Program) -> Program {
    let mut n: usize = 0;
    const TOTAL: usize = 2;
    loop {
        {
            let (new_program, changed) = unreachable::optimize(program);
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
            let (new_program, changed) = typecast::optimize(program);
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
 */
pub fn optimize_all(mut program: Program) -> Program {
    loop {
        let (new_program, changed) = propagate::optimize(program);
        program = new_program;
        if !changed {
            break;
        }
    }
    program
}
