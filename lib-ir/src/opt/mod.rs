mod unreachable;

use super::*;

/**
 * Main function to optimize a program.
 * Currently it does only remove unreachable statements,
 * because it is necessary for the backend to work properly.
 */
pub fn optimize(program: Program) -> Program {
    let (new_program, _) = unreachable::optimize(program);
    new_program
}
