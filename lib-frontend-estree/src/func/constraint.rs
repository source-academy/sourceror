use std::collections::HashMap;
use std::result::Result;

// e.g. x:number,y:string
pub fn parse_constraint(
    constraint_str: &str,
) -> Result<HashMap<&str, ir::VarType>, (&str, &'static str)> {
    let mut ret: HashMap<&str, ir::VarType> = HashMap::new();

    for raw_c in constraint_str.split_terminator(',') {
        let c = raw_c.trim();
        if let Some(idx) = c.find(':') {
            let (raw_key, tmp) = c.split_at(idx);
            let key = raw_key.trim();
            let val = tmp[':'.len_utf8()..].trim();
            if key.is_empty() || val.is_empty() {
                return Err((c, "Constraint cannot be empty"));
            }
            if ret.contains_key(key) {
                return Err((key, "Duplicate key"));
            } else {
                let ir_vartype = convert_vartype(val).ok_or((val, "Unknown type name"))?;
                ret.insert(key, ir_vartype);
            }
        } else {
            return Err((
                c,
                "Constraint must have syntax: \"<param_name>:<param_type>\"",
            ));
        }
    }

    Ok(ret)
}

fn convert_vartype(s: &str) -> Option<ir::VarType> {
    match s {
        "undefined" => Some(ir::VarType::Undefined),
        "number" => Some(ir::VarType::Number),
        "boolean" => Some(ir::VarType::Boolean),
        "string" => Some(ir::VarType::String),
        "function" => Some(ir::VarType::Func),
        _ => None,
    }
}
