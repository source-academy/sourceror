use ir::Import;
use ir::ImportValType;
use projstd::log::Logger;
use projstd::log::Severity;
use projstd::log::SourceLocation;
use std::boxed::Box;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::result::Result;

pub fn parse_imports<L: Logger + Copy>(
    import_spec: &str,
    logger: L,
) -> Result<HashMap<String, Import>, ()> {
    let mut ret = HashMap::new();
    for (i, line) in import_spec.lines().enumerate() {
        let (name, import) = parse_import(line, (i + 1) as i32, logger)?;
        match ret.entry(name) {
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(import);
            }
            Entry::Occupied(occupied_entry) => {
                logger.log(
                    Severity::Error,
                    format!("duplicate import declaration \"{}\"", occupied_entry.key()),
                    SourceLocation {
                        line: (i + 1) as i32,
                        column: 0,
                    },
                );
                return Err(());
            }
        }
    }
    Ok(ret)
}

fn parse_import<L: Logger + Copy>(
    import_line: &str,
    line_num: i32,
    logger: L,
) -> Result<(String, Import), ()> {
    //"__ffi_display misc display undefined string";
    fn log_if_error<L: Logger>(
        x: Option<&str>,
        line_num: i32,
        logger: L,
        seen_so_far: usize,
    ) -> Result<&str, ()> {
        match x {
            Some(y) => Ok(y),
            None => {
                logger.log(
                    Severity::Error,
                    format!(
                        "expected at least 4 strings in this line, got {}",
                        seen_so_far
                    ),
                    SourceLocation {
                        line: line_num,
                        column: 0,
                    },
                );
                Err(())
            }
        }
    };

    fn log_if_not_valid_vartype<L: Logger>(
        x: Option<ImportValType>,
        line_num: i32,
        logger: L,
        bad_name: &str,
    ) -> Result<ImportValType, ()> {
        match x {
            Some(y) => Ok(y),
            None => {
                logger.log(
                    Severity::Error,
                    format!("the name \"{}\" is not a valid ImportValType", bad_name),
                    SourceLocation {
                        line: line_num,
                        column: 0,
                    },
                );
                Err(())
            }
        }
    };

    let mut it = import_line.split(' ');
    let source_name = log_if_error(it.next(), line_num, logger, 0)?;
    let host_module = log_if_error(it.next(), line_num, logger, 1)?;
    let host_entity = log_if_error(it.next(), line_num, logger, 2)?;
    let return_type_str = log_if_error(it.next(), line_num, logger, 3)?;

    let return_type = log_if_not_valid_vartype(
        make_vartype(return_type_str),
        line_num,
        logger,
        return_type_str,
    )?;

    let param_types = it
        .map(|x| log_if_not_valid_vartype(make_vartype(x), line_num, logger, x))
        .collect::<Result<Box<[ImportValType]>, ()>>()?;

    Ok((
        source_name.to_owned(),
        Import {
            module_name: host_module.to_owned(),
            entity_name: host_entity.to_owned(),
            params: param_types,
            result: return_type,
        },
    ))
}

fn make_vartype(type_name: &str) -> Option<ImportValType> {
    match type_name {
        "undefined" => Some(ImportValType::Undefined),
        "number" => Some(ImportValType::Number),
        "string" => Some(ImportValType::String),
        _ => None,
    }
}
