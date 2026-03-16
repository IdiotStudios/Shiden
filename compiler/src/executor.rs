use crate::ir_types::IrNode;
use std::collections::HashMap;

pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<IrNode>,
}

pub fn execute(entries: &[IrNode]) -> Result<(), String> {
    let mut funcs: HashMap<String, Function> = HashMap::new();
    let mut i = 0;
    while i < entries.len() {
        match &entries[i] {
            IrNode::FnStart { name, args } => {
                let mut body = Vec::new();
                i += 1;
                while i < entries.len() {
                    match &entries[i] {
                        IrNode::FnEnd => {
                            i += 1;
                            break;
                        }
                        node => {
                            body.push(node.clone());
                            i += 1;
                        }
                    }
                }
                funcs.insert(
                    name.clone(),
                    Function {
                        name: name.clone(),
                        args: args.clone(),
                        body,
                    },
                );
            }
            _ => {
                i += 1;
            }
        }
    }

    if let Some(main_fn) = funcs.get("main") {
        let mut globals = HashMap::new();
        let _ = run_function(main_fn, &funcs, &mut globals, &[])?;
        Ok(())
    } else {
        Err("no main function found".to_string())
    }
}

fn eval_operand(tok: &str, env: &HashMap<String, String>) -> String {
    if tok.starts_with('"') && tok.ends_with('"') && tok.len() >= 2 {
        return tok.trim_matches('"').to_string();
    }
    if let Some(v) = env.get(tok) {
        return v.clone();
    }

    tok.to_string()
}

fn run_function(
    f: &Function,
    funcs: &HashMap<String, Function>,
    globals: &mut HashMap<String, String>,
    args: &[String],
) -> Result<Option<String>, String> {
    let mut locals: HashMap<String, String> = HashMap::new();

    for (i, aname) in f.args.iter().enumerate() {
        if i < args.len() {
            locals.insert(aname.clone(), args[i].clone());
        }
    }

    for node in &f.body {
        match node {
            IrNode::Instr { opcode, operands } => {
                if opcode == "println" {
                    let mut parts = Vec::new();
                    for op in operands {
                        parts.push(eval_operand(op, &locals));
                    }
                    println!("{}", parts.join(" "));
                } else {
                    if opcode == "add" && operands.len() >= 3 {
                        let dst = operands[0].clone();
                        let a = eval_operand(&operands[1], &locals);
                        let b = eval_operand(&operands[2], &locals);
                        if let (Ok(ai), Ok(bi)) = (a.parse::<i64>(), b.parse::<i64>()) {
                            locals.insert(dst, (ai + bi).to_string());
                        }
                    } else if opcode == "mov" && operands.len() >= 2 {
                        let dst = operands[0].clone();
                        let src = operands[1].clone();
                        let val = eval_operand(&src, &locals);
                        locals.insert(dst, val);
                    } else if opcode == "load" && operands.len() >= 2 {
                        let dst = operands[0].clone();
                        let src = operands[1].clone();
                        let val = eval_operand(&src, &locals);
                        locals.insert(dst, val);
                    } else if opcode == "store" && operands.len() >= 2 {
                        let src = operands[0].clone();
                        let dst = operands[1].clone();
                        let val = eval_operand(&src, &locals);
                        locals.insert(dst, val);
                    } else {
                    }
                }
            }
            IrNode::Call { call } => {
                let parts: Vec<&str> = call.split_whitespace().collect();
                if parts.is_empty() {
                    continue;
                }
                let fname = parts[0];
                let mut call_args: Vec<String> = Vec::new();
                for p in parts.iter().skip(1) {
                    call_args.push(eval_operand(p, &locals));
                }
                if let Some(fnobj) = funcs.get(fname) {
                    let res = run_function(fnobj, funcs, globals, &call_args)?;
                    if let Some(v) = res {
                        locals.insert("_retval".to_string(), v);
                    }
                } else {
                    return Err(format!("unknown function called: {}", fname));
                }
            }
            IrNode::Let { name, rhs } => {
                if rhs.starts_with("call ") {
                    let parts: Vec<&str> = rhs.split_whitespace().collect();
                    if parts.len() >= 2 {
                        let fname = parts[1];
                        let mut call_args: Vec<String> = Vec::new();
                        for p in parts.iter().skip(2) {
                            call_args.push(eval_operand(p, &locals));
                        }
                        if let Some(fnobj) = funcs.get(fname) {
                            let res = run_function(fnobj, funcs, globals, &call_args)?;
                            if let Some(v) = res {
                                locals.insert(name.clone(), v);
                            } else {
                                locals.insert(name.clone(), "".to_string());
                            }
                        } else {
                            locals.insert(name.clone(), "".to_string());
                        }
                    } else {
                        locals.insert(name.clone(), "".to_string());
                    }
                } else {
                    let val = if rhs.starts_with('"') && rhs.ends_with('"') {
                        rhs.trim_matches('"').to_string()
                    } else {
                        if let Some(v) = locals.get(rhs) {
                            v.clone()
                        } else if let Some(v) = globals.get(rhs) {
                            v.clone()
                        } else {
                            rhs.clone()
                        }
                    };
                    locals.insert(name.clone(), val);
                }
            }
            IrNode::Label { name: _ } => {}
            IrNode::Ret { value } => {
                if let Some(v) = value {
                    let val = if v.starts_with('"') && v.ends_with('"') {
                        v.trim_matches('"').to_string()
                    } else if let Some(lv) = locals.get(v) {
                        lv.clone()
                    } else {
                        v.clone()
                    };
                    return Ok(Some(val));
                }
                return Ok(None);
            }
            _ => {}
        }
    }
    Ok(None)
}
