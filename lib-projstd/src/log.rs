// This module contains stuff for platform independent compiler error printing.

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Severity {
    Hint,
    Note,
    Info,
    Warning,
    Error,
}

pub trait Logger {
    fn log(&self, severity: Severity, message: String);
}
