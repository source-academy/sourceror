// This module contains stuff for platform independent compiler error printing.

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Severity {
    Hint,
    Note,
    Info,
    Warning,
    Error,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub trait Logger {
    fn log(&self, severity: Severity, message: String);
}
