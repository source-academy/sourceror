// This module contains stuff for platform independent compiler error printing.

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Severity {
    Hint,
    Note,
    Info,
    Warning,
    Error,
}
impl Severity {
    pub fn code(&self) -> i32 {
        match self {
            Severity::Hint => 0,
            Severity::Note => 1,
            Severity::Info => 2,
            Severity::Warning => 3,
            Severity::Error => 4,
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Default)]
pub struct SourceLocation {
    pub line: i32,
    pub column: i32,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub trait Logger {
    fn log(&self, severity: Severity, message: String, loc: SourceLocation);
}
