use crate::estree;

pub trait IntoSourceLocation<R> {
    fn into_sl(self) -> R;
}
impl<'a> IntoSourceLocation<projstd::log::SourceLocationRef<'a>>
    for Option<estree::SourceLocation>
{
    fn into_sl(self) -> projstd::log::SourceLocationRef<'a> {
        match self {
            Some(sl) => projstd::log::SourceLocationRef {
                source: sl.source.as_deref(),
                start: projstd::log::Position {
                    line: sl.start.line as i32,
                    column: sl.start.column as i32,
                },
                end: projstd::log::Position {
                    line: sl.start.line as i32,
                    column: sl.start.column as i32,
                },
            },
            None => projstd::log::SourceLocationRef::default(),
        }
    }
}
