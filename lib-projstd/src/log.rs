use crate::subslice::SubsliceOffset;
use std::fmt::Display;

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
pub struct SourceLocationRef<'a> {
    pub source: Option<&'a str>,
    pub start: Position,
    pub end: Position,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Default)]
pub struct Position {
    pub line: i32,
    pub column: i32,
}

impl<'a> SourceLocationRef<'a> {
    pub fn new(
        start_line: i32,
        start_column: i32,
        end_line: i32,
        end_column: i32,
        source: Option<&'a str>,
    ) -> Self {
        Self {
            source: source,
            start: Position {
                line: start_line,
                column: start_column,
            },
            end: Position {
                line: end_line,
                column: end_column,
            },
        }
    }
    pub fn entire_line(line: i32, source: Option<&'a str>) -> Self {
        Self {
            source: source,
            start: Position {
                line: line,
                column: 0,
            },
            end: Position {
                line: line,
                column: 0,
            },
        }
    }
    pub fn entire_file(source: Option<&'a str>) -> Self {
        Self {
            source: source,
            start: Position { line: 0, column: 0 },
            end: Position { line: 0, column: 0 },
        }
    }
    pub fn within_line(line: i32, range: &str, line_start: &str, source: Option<&'a str>) -> Self {
        let std::ops::Range { start, end } = line_start.subslice_offset_range(range);
        Self {
            source: source,
            start: Position {
                line: line,
                column: start as i32,
            },
            end: Position {
                line: line,
                column: end as i32,
            },
        }
    }
    pub fn to_owned(&self) -> SourceLocation {
        SourceLocation {
            source: self.source.map(|x| x.into()),
            start: self.start,
            end: self.end,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Default)]
pub struct SourceLocation {
    pub source: Option<String>,
    pub start: Position,
    pub end: Position,
}

impl SourceLocation {
    pub fn as_ref(&self) -> SourceLocationRef {
        SourceLocationRef {
            source: self.source.as_deref(),
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> std::fmt::Display for SourceLocationRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct CompileMessage<E> {
    location: SourceLocation,
    severity: Severity,
    message: E,
}
impl<E> CompileMessage<E> {
    pub fn new<SL: Into<SourceLocation>, SV: Into<Severity>>(
        location: SL,
        severity: SV,
        message: E,
    ) -> Self {
        Self {
            location: location.into(),
            severity: severity.into(),
            message: message,
        }
    }
    pub fn new_error<SL: Into<SourceLocation>>(location: SL, message: E) -> Self {
        Self {
            location: location.into(),
            severity: Severity::Error,
            message: message,
        }
    }
    pub fn into_cm<F: From<E>>(self) -> CompileMessage<F> {
        CompileMessage {
            location: self.location,
            severity: self.severity,
            message: self.message.into(),
        }
    }
    pub fn message(&self) -> &E {
        &self.message
    }
}
impl<E: std::fmt::Display> Loggable for CompileMessage<E> {
    fn severity(&self) -> Severity {
        self.severity
    }
    fn location<'a>(&'a self) -> SourceLocationRef<'a> {
        self.location.as_ref()
    }
    fn message(&self) -> String {
        format!("{}", self.message)
    }
}

/**
 * A structured message that can be logged into js-slang format.
 */
pub trait Loggable {
    fn severity(&self) -> Severity;
    fn location<'a>(&'a self) -> SourceLocationRef<'a>;
    fn message(&self) -> String;
}

pub trait Logger {
    fn log<L: Loggable>(&self, content: L);
}

pub trait LogErr<R> {
    fn log_err<L: Logger>(self, logger: &L) -> R;
}

impl<T, E: Loggable> LogErr<Result<T, ()>> for Result<T, E> {
    fn log_err<L: Logger>(self, logger: &L) -> Result<T, ()> {
        self.map_err(|e| logger.log(e))
    }
}
