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
    const total: usize = 2;
    loop {
        {
            let (new_program, changed) = unreachable::optimize(program);
            program = new_program;
            if changed {
                n = 1;
            } else {
                n += 1;
            }
            if n == total {
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
            if n == total {
                break;
            }
        }
    }

    program
}
