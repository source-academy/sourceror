use crate::estree;

pub trait IntoSourceLocation<S, R> {
    fn into_sl(&self, filename: S) -> R;
}
impl<'a> IntoSourceLocation<Option<&'a str>, projstd::log::SourceLocationRef<'a>>
    for Option<estree::SourceLocation>
{
    fn into_sl(&self, filename: Option<&'a str>) -> projstd::log::SourceLocationRef<'a> {
        match self {
            Some(sl) => projstd::log::SourceLocationRef {
                source: filename,
                start: projstd::log::Position {
                    line: sl.start.line as i32,
                    column: sl.start.column as i32,
                },
                end: projstd::log::Position {
                    line: sl.end.line as i32,
                    column: sl.end.column as i32,
                },
            },
            None => projstd::log::SourceLocationRef::default(),
        }
    }
}
impl<'a> IntoSourceLocation<Option<String>, projstd::log::SourceLocation>
    for Option<estree::SourceLocation>
{
    fn into_sl(&self, filename: Option<String>) -> projstd::log::SourceLocation {
        match self {
            Some(sl) => projstd::log::SourceLocation {
                source: filename,
                start: projstd::log::Position {
                    line: sl.start.line as i32,
                    column: sl.start.column as i32,
                },
                end: projstd::log::Position {
                    line: sl.end.line as i32,
                    column: sl.end.column as i32,
                },
            },
            None => projstd::log::SourceLocationRef::default().to_owned(),
        }
    }
}
