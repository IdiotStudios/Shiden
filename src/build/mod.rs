use object::SymbolScope;
use object::write::{Object, StandardSection, Symbol, SymbolSection};
use object::{Architecture, Endianness};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::libraries::runtime_helpers;

mod linux;
mod windows;
use linux::RelocEntry;

fn get_binary_format(target: &str) -> Result<object::BinaryFormat, String> {
    if target.contains("windows") {
        Ok(object::BinaryFormat::Pe)
    } else if target.contains("linux") {
        Ok(object::BinaryFormat::Elf)
    } else if target.contains("macos") {
        Ok(object::BinaryFormat::MachO)
    } else {
        Err(format!(
            "Unsupported target: {}, contact us if you want support",
            target
        ))
    }
}

fn get_executable_extension(target: &str) -> &'static str {
    if target.contains("windows") {
        ".exe"
    } else {
        ""
    }
}

fn resolve_imports(proj_dir: &Path, prog: &mut crate::syntax::Program) -> Result<(), String> {
    let mut imports = Vec::new();
    let mut remaining_items = Vec::new();

    for item in prog.items.drain(..) {
        if let crate::syntax::Item::Import { path, alias } = item {
            imports.push((path, alias));
        } else {
            remaining_items.push(item);
        }
    }

    let mut loaded_modules = HashMap::new();
    for (path, _alias) in &imports {
        if !loaded_modules.contains_key(path) {
            let module_path = proj_dir.join("src").join(format!("{}.sd", path));
            let module_src = std::fs::read_to_string(&module_path)
                .map_err(|e| format!("Failed to read module {}: {}", path, e))?;
            let module_prog = crate::frontend::parse(&module_src)
                .map_err(|e| format!("Parse error in module {}: {}", path, e))?;
            loaded_modules.insert(path.clone(), module_prog);
        }
    }

    for (path, alias) in imports {
        if let Some(module_prog) = loaded_modules.get(&path) {
            for item in &module_prog.items {
                if let crate::syntax::Item::Function {
                    name,
                    params,
                    ret,
                    body,
                } = item
                {
                    let qualified_name = format!("{}_{}", alias, name);
                    remaining_items.push(crate::syntax::Item::Function {
                        name: qualified_name,
                        params: params.clone(),
                        ret: ret.clone(),
                        body: body.clone(),
                    });
                }
            }
        }
    }

    prog.items = remaining_items;
    Ok(())
}

fn check_dead_code(prog: &crate::syntax::Program) {
    use crate::syntax::{Expr, Item, Stmt};
    use std::collections::HashSet;

    let mut defined_functions: HashMap<String, String> = HashMap::new();

    for item in &prog.items {
        if let Item::Function {
            name, params, ret, ..
        } = item
        {
            let params_str = params
                .iter()
                .map(|(pname, pty)| {
                    if let Some(t) = pty {
                        format!("{}/{}", pname, t)
                    } else {
                        pname.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");

            let ret_str = if let Some(r) = ret {
                format!("/{}", r)
            } else {
                String::new()
            };

            let signature = format!("fn new {}({}){}", name, params_str, ret_str);
            defined_functions.insert(name.clone(), signature);
        }
    }

    let mut called_functions = HashSet::new();

    fn collect_calls_from_expr(expr: &Expr, calls: &mut HashSet<String>) {
        match expr {
            Expr::Call { name, args } => {
                calls.insert(name.clone());
                for arg in args {
                    collect_calls_from_expr(arg, calls);
                }
            }
            Expr::Binary { left, right, .. } => {
                collect_calls_from_expr(left, calls);
                collect_calls_from_expr(right, calls);
            }
            Expr::Unary { expr, .. } => {
                collect_calls_from_expr(expr, calls);
            }
            Expr::Index { expr, index } => {
                collect_calls_from_expr(expr, calls);
                collect_calls_from_expr(index, calls);
            }
            Expr::ArrayLiteral(exprs) => {
                for e in exprs {
                    collect_calls_from_expr(e, calls);
                }
            }
            _ => {}
        }
    }

    fn collect_calls_from_stmt(stmt: &Stmt, calls: &mut HashSet<String>) {
        match stmt {
            Stmt::Let { value, .. } => {
                collect_calls_from_expr(value, calls);
            }
            Stmt::Assign { value, .. } => {
                collect_calls_from_expr(value, calls);
            }
            Stmt::Expr(expr) => {
                collect_calls_from_expr(expr, calls);
            }
            Stmt::Return(expr, _) => {
                collect_calls_from_expr(expr, calls);
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                collect_calls_from_expr(cond, calls);
                for s in then_block {
                    collect_calls_from_stmt(s, calls);
                }
                if let Some(else_blk) = else_block {
                    for s in else_blk {
                        collect_calls_from_stmt(s, calls);
                    }
                }
            }
            Stmt::While { cond, body } => {
                collect_calls_from_expr(cond, calls);
                for s in body {
                    collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::For { iterable, body, .. } => {
                collect_calls_from_expr(iterable, calls);
                for s in body {
                    collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::AssignIndex { index, value, .. } => {
                collect_calls_from_expr(index, calls);
                collect_calls_from_expr(value, calls);
            }
            _ => {}
        }
    }

    for item in &prog.items {
        if let Item::Function { body, .. } = item {
            for stmt in body {
                collect_calls_from_stmt(stmt, &mut called_functions);
            }
        }
    }

    for (func_name, signature) in &defined_functions {
        if func_name != "main" && !called_functions.contains(func_name) {
            eprintln!("\x1b[33mWARNING\x1b[0m: {} is not called", signature);
        }
    }
}

pub fn compile_project(
    proj_dir: &Path,
    proj_name: &str,
    target: &str,
    opt_level: Option<i32>,
) -> Result<PathBuf, String> {
    let binary_format = get_binary_format(target)?;

    let main_path = proj_dir.join("src").join("main.sd");
    let src = std::fs::read_to_string(&main_path)
        .map_err(|e| format!("Failed to read {}: {}", main_path.display(), e))?;
    let mut prog = crate::frontend::parse(&src).map_err(|e| format!("Parse error: {}", e))?;

    resolve_imports(proj_dir, &mut prog)?;

    check_dead_code(&prog);

    let main = prog
        .items
        .iter()
        .find_map(|it| {
            if let crate::syntax::Item::Function { name, body, .. } = it {
                if name == "main" {
                    Some(body.clone())
                } else {
                    None
                }
            } else {
                None
            }
        })
        .ok_or_else(|| "missing 'main' function".to_string())?;

    #[derive(Debug)]
    enum IrInstr {
        EmitString(String),
        PushConst(i64),
        BinOp(crate::syntax::BinOp),
        Compare(crate::syntax::BinOp),
        Call(String, usize),
        Ret,
        EmitInt,
        EmitChar,
        StoreLocal(usize),
        LoadLocal(usize),
        Pop,
        Label(usize),
        Jump(usize),
        JumpIfFalse(usize),
    }

    fn eval_const_integer(e: &crate::syntax::Expr) -> Option<i64> {
        use crate::syntax::*;
        match e {
            Expr::Number(s) => s.parse::<i64>().ok(),
            Expr::Unary {
                op: crate::syntax::UnaryOp::Neg,
                expr,
            } => eval_const_integer(expr).map(|v| -v),
            Expr::Unary { .. } => None,
            Expr::Binary { left, op, right } => {
                let l = eval_const_integer(left)?;
                let r = eval_const_integer(right)?;
                match op {
                    crate::syntax::BinOp::Add => Some(l + r),
                    crate::syntax::BinOp::Sub => Some(l - r),
                    crate::syntax::BinOp::Mul => Some(l * r),
                    crate::syntax::BinOp::Div => Some(l / r),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn lower_expr_to_ir(
        e: &crate::syntax::Expr,
        out: &mut Vec<IrInstr>,
        locals: &mut std::collections::HashMap<String, usize>,
        next_local: &mut usize,
        next_label: &mut usize,
    ) -> Result<(), String> {
        use crate::syntax::*;
        match e {
            Expr::Number(s) => {
                let v = s.parse::<i64>().map_err(|_| "invalid number")?;
                out.push(IrInstr::PushConst(v));
                Ok(())
            }
            Expr::Bool(b) => {
                out.push(IrInstr::PushConst(if *b { 1 } else { 0 }));
                Ok(())
            }
            Expr::Identifier(name) => {
                if let Some(idx) = locals.get(name) {
                    out.push(IrInstr::LoadLocal(*idx));
                    Ok(())
                } else {
                    Err(format!("unknown identifier '{}'", name))
                }
            }
            Expr::Call { name, args } => {
                if name == "args" && args.is_empty() {
                    out.push(IrInstr::Call("__build_args_array".to_string(), 0));
                    return Ok(());
                }

                for a in args {
                    lower_expr_to_ir(a, out, locals, next_local, next_label)?;
                }
                out.push(IrInstr::Call(name.clone(), args.len()));
                Ok(())
            }
            Expr::Unary { op, expr } => match op {
                UnaryOp::Neg => {
                    lower_expr_to_ir(expr, out, locals, next_local, next_label)?;
                    out.push(IrInstr::PushConst(-1));
                    out.push(IrInstr::BinOp(BinOp::Mul));
                    Ok(())
                }
                UnaryOp::Not => {
                    lower_expr_to_ir(expr, out, locals, next_local, next_label)?;
                    out.push(IrInstr::PushConst(0));
                    out.push(IrInstr::Compare(BinOp::Eq));
                    Ok(())
                }
            },
            Expr::Binary { left, op, right } => {
                use crate::syntax::BinOp::*;
                match op {
                    Add | Sub | Mul | Div => {
                        lower_expr_to_ir(left, out, locals, next_local, next_label)?;
                        lower_expr_to_ir(right, out, locals, next_local, next_label)?;
                        out.push(IrInstr::BinOp(op.clone()));
                        Ok(())
                    }
                    Eq | Ne | Lt | Le | Gt | Ge => {
                        let is_string_compare = matches!(op, Eq | Ne)
                            && (matches!(left.as_ref(), Expr::StringLiteral(_))
                                || matches!(right.as_ref(), Expr::StringLiteral(_)));
                        if is_string_compare {
                            let left_tmp = *next_local;
                            *next_local += 1;
                            let right_tmp = *next_local;
                            *next_local += 1;
                            let len_left = *next_local;
                            *next_local += 1;
                            let len_right = *next_local;
                            *next_local += 1;
                            let idx_tmp = *next_local;
                            *next_local += 1;

                            lower_expr_to_ir(left, out, locals, next_local, next_label)?;
                            out.push(IrInstr::StoreLocal(left_tmp));
                            lower_expr_to_ir(right, out, locals, next_local, next_label)?;
                            out.push(IrInstr::StoreLocal(right_tmp));

                            out.push(IrInstr::LoadLocal(left_tmp));
                            out.push(IrInstr::Call("len".into(), 1));
                            out.push(IrInstr::StoreLocal(len_left));
                            out.push(IrInstr::LoadLocal(right_tmp));
                            out.push(IrInstr::Call("len".into(), 1));
                            out.push(IrInstr::StoreLocal(len_right));

                            out.push(IrInstr::LoadLocal(len_left));
                            out.push(IrInstr::LoadLocal(len_right));
                            out.push(IrInstr::Compare(Eq));

                            let mismatch_lbl = *next_label;
                            *next_label += 1;
                            let loop_lbl = *next_label;
                            *next_label += 1;
                            let done_lbl = *next_label;
                            *next_label += 1;
                            let end_lbl = *next_label;
                            *next_label += 1;

                            out.push(IrInstr::JumpIfFalse(mismatch_lbl));

                            out.push(IrInstr::PushConst(0));
                            out.push(IrInstr::StoreLocal(idx_tmp));
                            out.push(IrInstr::Label(loop_lbl));

                            out.push(IrInstr::LoadLocal(idx_tmp));
                            out.push(IrInstr::LoadLocal(len_left));
                            out.push(IrInstr::Compare(Lt));
                            out.push(IrInstr::JumpIfFalse(done_lbl));

                            out.push(IrInstr::LoadLocal(left_tmp));
                            out.push(IrInstr::LoadLocal(idx_tmp));
                            out.push(IrInstr::Call("array_get".into(), 2));
                            out.push(IrInstr::LoadLocal(right_tmp));
                            out.push(IrInstr::LoadLocal(idx_tmp));
                            out.push(IrInstr::Call("array_get".into(), 2));
                            out.push(IrInstr::Compare(Eq));
                            out.push(IrInstr::JumpIfFalse(mismatch_lbl));

                            out.push(IrInstr::LoadLocal(idx_tmp));
                            out.push(IrInstr::PushConst(1));
                            out.push(IrInstr::BinOp(Add));
                            out.push(IrInstr::StoreLocal(idx_tmp));
                            out.push(IrInstr::Jump(loop_lbl));

                            out.push(IrInstr::Label(done_lbl));
                            out.push(IrInstr::PushConst(1));
                            out.push(IrInstr::Jump(end_lbl));

                            out.push(IrInstr::Label(mismatch_lbl));
                            out.push(IrInstr::PushConst(0));
                            out.push(IrInstr::Label(end_lbl));

                            if matches!(op, Ne) {
                                out.push(IrInstr::PushConst(0));
                                out.push(IrInstr::Compare(Eq));
                            }
                            return Ok(());
                        }

                        lower_expr_to_ir(left, out, locals, next_local, next_label)?;
                        lower_expr_to_ir(right, out, locals, next_local, next_label)?;
                        out.push(IrInstr::Compare(op.clone()));
                        Ok(())
                    }
                    And => {
                        let else_lbl = *next_label;
                        *next_label += 1;
                        let end_lbl = *next_label;
                        *next_label += 1;
                        lower_expr_to_ir(left, out, locals, next_local, next_label)?;
                        out.push(IrInstr::JumpIfFalse(else_lbl));
                        lower_expr_to_ir(right, out, locals, next_local, next_label)?;
                        out.push(IrInstr::Jump(end_lbl));
                        out.push(IrInstr::Label(else_lbl));
                        out.push(IrInstr::PushConst(0));
                        out.push(IrInstr::Label(end_lbl));
                        Ok(())
                    }
                    Or => {
                        let eval_lbl = *next_label;
                        *next_label += 1;
                        let end_lbl = *next_label;
                        *next_label += 1;
                        lower_expr_to_ir(left, out, locals, next_local, next_label)?;
                        out.push(IrInstr::JumpIfFalse(eval_lbl));

                        out.push(IrInstr::PushConst(1));
                        out.push(IrInstr::Jump(end_lbl));
                        out.push(IrInstr::Label(eval_lbl));
                        lower_expr_to_ir(right, out, locals, next_local, next_label)?;
                        out.push(IrInstr::Label(end_lbl));
                        Ok(())
                    }
                }
            }
            Expr::Char(c) => {
                out.push(IrInstr::PushConst(*c as i64));
                Ok(())
            }
            Expr::StringLiteral(s) => {
                out.push(IrInstr::Call("array_new".into(), 0));
                for ch in s.chars() {
                    out.push(IrInstr::PushConst(ch as i64));
                    out.push(IrInstr::Call("push".into(), 2));
                }
                Ok(())
            }
            Expr::ArrayLiteral(elems) => {
                out.push(IrInstr::Call("array_new".into(), 0));
                for el in elems {
                    lower_expr_to_ir(el, out, locals, next_local, next_label)?;
                    out.push(IrInstr::Call("push".into(), 2));
                }
                Ok(())
            }
            Expr::Index { expr: arr, index } => {
                lower_expr_to_ir(arr, out, locals, next_local, next_label)?;
                lower_expr_to_ir(index, out, locals, next_local, next_label)?;
                out.push(IrInstr::Call("array_get".into(), 2));
                Ok(())
            }
        }
    }

    fn lower_stmt_to_ir(
        stmt: &crate::syntax::Stmt,
        locals: &mut std::collections::HashMap<String, usize>,
        types: &mut std::collections::HashMap<String, String>,
        next_local: &mut usize,
        next_label: &mut usize,
        break_lbl: Option<usize>,
        continue_lbl: Option<usize>,
    ) -> Result<Vec<IrInstr>, String> {
        use crate::syntax::*;
        match stmt {
            Stmt::Let {
                name, value, ty, ..
            } => {
                let mut res = Vec::new();
                lower_expr_to_ir(value, &mut res, locals, next_local, next_label)?;
                let idx = if let Some(&i) = locals.get(name) {
                    i
                } else {
                    let i = *next_local;
                    locals.insert(name.clone(), i);
                    *next_local += 1;
                    i
                };
                if let Some(ty) = ty {
                    types.insert(name.clone(), ty.clone());
                }
                res.push(IrInstr::StoreLocal(idx));
                Ok(res)
            }
            Stmt::Break => {
                if let Some(lbl) = break_lbl {
                    Ok(vec![IrInstr::Jump(lbl)])
                } else {
                    Err("break outside loop".into())
                }
            }
            Stmt::Continue => {
                if let Some(lbl) = continue_lbl {
                    Ok(vec![IrInstr::Jump(lbl)])
                } else {
                    Err("continue outside loop".into())
                }
            }
            Stmt::AssignIndex { name, index, value } => {
                let mut res = Vec::new();

                let arr_idx = locals
                    .get(name)
                    .ok_or_else(|| format!("assign-index to unknown variable '{}'", name))?;
                res.push(IrInstr::LoadLocal(*arr_idx));

                lower_expr_to_ir(index, &mut res, locals, next_local, next_label)?;
                lower_expr_to_ir(value, &mut res, locals, next_local, next_label)?;
                res.push(IrInstr::Call("array_set".into(), 3));
                Ok(res)
            }
            Stmt::Assign { name, value } => {
                let mut res = Vec::new();
                lower_expr_to_ir(value, &mut res, locals, next_local, next_label)?;
                let idx = locals
                    .get(name)
                    .ok_or_else(|| format!("assign to unknown variable '{}'", name))?;
                res.push(IrInstr::StoreLocal(*idx));
                Ok(res)
            }
            Stmt::Expr(Expr::Call { name, args }) if name == "print_str" => {
                if args.len() != 1 {
                    return Err("print_str expects 1 argument".into());
                }
                let mut res = Vec::new();
                lower_expr_to_ir(&args[0], &mut res, locals, next_local, next_label)?;
                res.push(IrInstr::Call("__print_str".into(), 1));
                res.push(IrInstr::Pop);
                Ok(res)
            }
            Stmt::Expr(Expr::Call { name, args }) if name == "println_str" => {
                if args.len() != 1 {
                    return Err("println_str expects 1 argument".into());
                }
                let mut res = Vec::new();
                lower_expr_to_ir(&args[0], &mut res, locals, next_local, next_label)?;
                res.push(IrInstr::Call("__print_str".into(), 1));
                res.push(IrInstr::Pop);
                res.push(IrInstr::EmitString("\n".to_string()));
                Ok(res)
            }
            Stmt::Expr(Expr::Call { name, args }) if name == "println" => {
                if args.is_empty() {
                    return Ok(vec![IrInstr::EmitString("\n".to_string())]);
                }
                if let Expr::StringLiteral(fmt) = &args[0] {
                    let mut parts = Vec::new();
                    let mut last = 0usize;
                    let mut placeholders = 0usize;
                    for (i, _) in fmt.match_indices("{}") {
                        parts.push(fmt[last..i].to_string());
                        last = i + 2;
                        placeholders += 1;
                    }
                    parts.push(fmt[last..].to_string());

                    if placeholders == 0 {
                        Ok(vec![IrInstr::EmitString(format!("{}\n", fmt))])
                    } else if placeholders != args.len() - 1 {
                        Err("placeholder count does not match arguments".into())
                    } else {
                        let mut res: Vec<IrInstr> = Vec::new();
                        for (i, p) in parts.iter().enumerate() {
                            if !p.is_empty() {
                                res.push(IrInstr::EmitString(p.clone()));
                            }
                            if i < placeholders {
                                let arg_expr = &args[i + 1];

                                if let Some(v) = eval_const_integer(arg_expr) {
                                    res.push(IrInstr::EmitString(format!("{}", v)));
                                    continue;
                                }

                                if let Expr::Char(c) = arg_expr {
                                    res.push(IrInstr::EmitString(format!("{}", c)));
                                    continue;
                                }

                                if let Expr::StringLiteral(s) = arg_expr {
                                    res.push(IrInstr::EmitString(s.clone()));
                                    continue;
                                }

                                if let Expr::Identifier(id) = arg_expr {
                                    if matches!(types.get(id).map(String::as_str), Some("str")) {
                                        lower_expr_to_ir(
                                            arg_expr, &mut res, locals, next_local, next_label,
                                        )?;
                                        res.push(IrInstr::Call("__print_str".into(), 1));
                                        res.push(IrInstr::Pop);
                                        continue;
                                    }
                                    if matches!(types.get(id).map(String::as_str), Some("char")) {
                                        lower_expr_to_ir(
                                            arg_expr, &mut res, locals, next_local, next_label,
                                        )?;
                                        res.push(IrInstr::EmitChar);
                                        continue;
                                    }

                                    lower_expr_to_ir(
                                        arg_expr, &mut res, locals, next_local, next_label,
                                    )?;
                                    res.push(IrInstr::EmitInt);
                                    continue;
                                }

                                if let Expr::Index {
                                    expr: arr,
                                    index: _,
                                } = arg_expr
                                    && let Expr::Identifier(base_id) = arr.as_ref()
                                    && matches!(
                                        types.get(base_id).map(String::as_str),
                                        Some("array<char>")
                                    )
                                {
                                    lower_expr_to_ir(
                                        arg_expr, &mut res, locals, next_local, next_label,
                                    )?;
                                    res.push(IrInstr::EmitChar);
                                    continue;
                                }

                                lower_expr_to_ir(
                                    arg_expr, &mut res, locals, next_local, next_label,
                                )?;
                                res.push(IrInstr::EmitInt);
                            }
                        }
                        res.push(IrInstr::EmitString("\n".to_string()));
                        Ok(res)
                    }
                } else {
                    Err("first argument to println must be a string literal format".into())
                }
            }
            Stmt::Expr(Expr::Call { name, args }) if name == "push" => {
                if args.len() == 2
                    && let Expr::Identifier(arr_name) = &args[0]
                    && let Expr::Char(_) = &args[1]
                {
                    types.insert(arr_name.clone(), "array<char>".to_string());
                }
                let mut res = Vec::new();
                lower_expr_to_ir(
                    &Expr::Call {
                        name: name.clone(),
                        args: args.clone(),
                    },
                    &mut res,
                    locals,
                    next_local,
                    next_label,
                )?;
                res.push(IrInstr::Pop);
                Ok(res)
            }

            Stmt::Expr(expr) => {
                let mut res = Vec::new();
                lower_expr_to_ir(expr, &mut res, locals, next_local, next_label)?;

                res.push(IrInstr::Pop);
                Ok(res)
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let mut res = Vec::new();
                let else_lbl = *next_label;
                *next_label += 1;
                let end_lbl = *next_label;
                *next_label += 1;
                lower_expr_to_ir(cond, &mut res, locals, next_local, next_label)?;
                res.push(IrInstr::JumpIfFalse(else_lbl));
                for s in then_block {
                    let mut inner = lower_stmt_to_ir(
                        s,
                        locals,
                        types,
                        next_local,
                        next_label,
                        break_lbl,
                        continue_lbl,
                    )?;
                    res.append(&mut inner);
                }
                res.push(IrInstr::Jump(end_lbl));
                res.push(IrInstr::Label(else_lbl));
                if let Some(eb) = else_block {
                    for s in eb {
                        let mut inner = lower_stmt_to_ir(
                            s,
                            locals,
                            types,
                            next_local,
                            next_label,
                            break_lbl,
                            continue_lbl,
                        )?;
                        res.append(&mut inner);
                    }
                }
                res.push(IrInstr::Label(end_lbl));
                Ok(res)
            }
            Stmt::While { cond, body } => {
                let mut res = Vec::new();
                let start_lbl = *next_label;
                *next_label += 1;
                let end_lbl = *next_label;
                *next_label += 1;
                res.push(IrInstr::Label(start_lbl));
                lower_expr_to_ir(cond, &mut res, locals, next_local, next_label)?;
                res.push(IrInstr::JumpIfFalse(end_lbl));
                for s in body {
                    let mut inner = lower_stmt_to_ir(
                        s,
                        locals,
                        types,
                        next_local,
                        next_label,
                        Some(end_lbl),
                        Some(start_lbl),
                    )?;
                    res.append(&mut inner);
                }
                res.push(IrInstr::Jump(start_lbl));
                res.push(IrInstr::Label(end_lbl));
                Ok(res)
            }
            Stmt::For {
                var,
                iterable,
                body,
            } => {
                let mut res = Vec::new();

                let arr_local = if locals.get(var).is_some() {
                    let i = *next_local;
                    *next_local += 1;
                    locals.insert(format!("{}_iter", var), i);
                    i
                } else {
                    let i = *next_local;
                    locals.insert(format!("{}_iter", var), i);
                    *next_local += 1;
                    i
                };
                let idx_local = *next_local;
                *next_local += 1;
                let var_local = if let Some(&i) = locals.get(var) {
                    i
                } else {
                    let i = *next_local;
                    locals.insert(var.clone(), i);
                    *next_local += 1;
                    i
                };

                lower_expr_to_ir(iterable, &mut res, locals, next_local, next_label)?;
                res.push(IrInstr::StoreLocal(arr_local));

                res.push(IrInstr::PushConst(0));
                res.push(IrInstr::StoreLocal(idx_local));

                let start_lbl = *next_label;
                *next_label += 1;
                let end_lbl = *next_label;
                *next_label += 1;

                res.push(IrInstr::Label(start_lbl));

                res.push(IrInstr::LoadLocal(idx_local));
                res.push(IrInstr::LoadLocal(arr_local));
                res.push(IrInstr::Call("len".into(), 1));
                res.push(IrInstr::Compare(crate::syntax::BinOp::Lt));
                res.push(IrInstr::JumpIfFalse(end_lbl));

                res.push(IrInstr::LoadLocal(arr_local));
                res.push(IrInstr::LoadLocal(idx_local));
                res.push(IrInstr::Call("array_get".into(), 2));
                res.push(IrInstr::StoreLocal(var_local));

                for s in body {
                    let mut inner = lower_stmt_to_ir(
                        s,
                        locals,
                        types,
                        next_local,
                        next_label,
                        Some(end_lbl),
                        Some(start_lbl),
                    )?;
                    res.append(&mut inner);
                }

                res.push(IrInstr::LoadLocal(idx_local));
                res.push(IrInstr::PushConst(1));
                res.push(IrInstr::BinOp(crate::syntax::BinOp::Add));
                res.push(IrInstr::StoreLocal(idx_local));

                res.push(IrInstr::Jump(start_lbl));
                res.push(IrInstr::Label(end_lbl));
                Ok(res)
            }
            Stmt::Return(expr, _ty) => {
                let mut res = Vec::new();
                lower_expr_to_ir(expr, &mut res, locals, next_local, next_label)?;
                res.push(IrInstr::Ret);
                Ok(res)
            }
        }
    }

    let mut function_infos: Vec<(String, usize, Vec<IrInstr>, usize, usize)> = Vec::new();
    let mut func_label_map: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();

    let mut func_meta: std::collections::HashMap<usize, (usize, usize)> =
        std::collections::HashMap::new();
    let mut _next_label: usize = 0;
    let mut next_label_for_funcs = _next_label;
    for item in prog.items.iter() {
        if let crate::syntax::Item::Function {
            name, params, body, ..
        } = item
        {
            if name == "main" {
                continue;
            }
            let mut locals_map: std::collections::HashMap<String, usize> =
                std::collections::HashMap::new();
            let mut types_map: std::collections::HashMap<String, String> =
                std::collections::HashMap::new();
            let mut next_local_idx: usize = 0;

            for (i, (pname, pty)) in params.iter().enumerate() {
                locals_map.insert(pname.clone(), i);
                if let Some(ty) = pty {
                    types_map.insert(pname.clone(), ty.clone());
                }
                next_local_idx = i + 1;
            }
            let my_label = next_label_for_funcs;
            next_label_for_funcs += 1;
            func_label_map.insert(name.clone(), my_label);
            let mut f_ir: Vec<IrInstr> = Vec::new();
            for stmt in body.iter() {
                let mut is = lower_stmt_to_ir(
                    stmt,
                    &mut locals_map,
                    &mut types_map,
                    &mut next_local_idx,
                    &mut next_label_for_funcs,
                    None,
                    None,
                )
                .map_err(|e| format!("lowering error in func {}: {}", name, e))?;
                f_ir.append(&mut is);
            }

            if !matches!(f_ir.last(), Some(IrInstr::Ret)) {
                f_ir.push(IrInstr::PushConst(0));
                f_ir.push(IrInstr::Ret);
            }
            func_meta.insert(my_label, (params.len(), next_local_idx));
            function_infos.push((name.clone(), params.len(), f_ir, next_local_idx, my_label));
        }
    }

    let mut ir: Vec<IrInstr> = Vec::new();
    let mut locals: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    let mut types: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut next_local: usize = 0;

    let exit_label = next_label_for_funcs;
    next_label_for_funcs += 1;
    for stmt in main {
        let mut is = lower_stmt_to_ir(
            &stmt,
            &mut locals,
            &mut types,
            &mut next_local,
            &mut next_label_for_funcs,
            None,
            None,
        )
        .map_err(|e| format!("lowering error: {}", e))?;
        ir.append(&mut is);
    }

    ir.push(IrInstr::Jump(exit_label));

    for (_fname, _pcount, mut f_ir, _loc_count, lbl) in function_infos.drain(..) {
        ir.push(IrInstr::Label(lbl));

        ir.append(&mut f_ir);
    }

    ir.push(IrInstr::Label(exit_label));

    _next_label = next_label_for_funcs;

    fn optimize_ir(mut ir: Vec<IrInstr>, opt_level: Option<i32>) -> Vec<IrInstr> {
        if opt_level.unwrap_or(0) >= 1 {
            let mut out = Vec::new();
            for instr in ir {
                match instr {
                    IrInstr::EmitString(s) => {
                        if let Some(IrInstr::EmitString(prev)) = out.last_mut() {
                            prev.push_str(&s);
                        } else {
                            out.push(IrInstr::EmitString(s));
                        }
                    }
                    other => out.push(other),
                }
            }
            ir = out;
        }

        if opt_level.unwrap_or(0) >= 2 {
            use crate::syntax::BinOp;
            let mut stack: Vec<IrInstr> = Vec::new();
            for instr in ir {
                match instr {
                    IrInstr::BinOp(op) => {
                        let b_opt = stack.pop();
                        let a_opt = stack.pop();

                        if let (Some(IrInstr::PushConst(a)), Some(IrInstr::PushConst(b))) =
                            (&a_opt, &b_opt)
                        {
                            let res = match op {
                                BinOp::Add => a + b,
                                BinOp::Sub => a - b,
                                BinOp::Mul => a * b,
                                BinOp::Div => a / b,
                                _ => {
                                    if let Some(a_instr) = a_opt {
                                        stack.push(a_instr);
                                    }
                                    if let Some(b_instr) = b_opt {
                                        stack.push(b_instr);
                                    }
                                    stack.push(IrInstr::BinOp(op));
                                    continue;
                                }
                            };
                            stack.push(IrInstr::PushConst(res));
                        } else {
                            if let Some(a_instr) = a_opt {
                                stack.push(a_instr);
                            }
                            if let Some(b_instr) = b_opt {
                                stack.push(b_instr);
                            }
                            stack.push(IrInstr::BinOp(op));
                        }
                    }
                    other => stack.push(other),
                }
            }
            ir = stack;
        }

        ir
    }

    ir = optimize_ir(ir, opt_level);

    let mut strings: Vec<String> = Vec::new();
    let mut string_indices: Vec<Option<usize>> = Vec::new();
    for instr in &ir {
        match instr {
            IrInstr::EmitString(s) => {
                string_indices.push(Some(strings.len()));
                strings.push(s.clone());
            }
            _ => string_indices.push(None),
        }
    }

    let obj_format_for_writer = if target.contains("windows") {
        object::BinaryFormat::Elf
    } else {
        binary_format
    };
    let mut obj = Object::new(
        obj_format_for_writer,
        Architecture::X86_64,
        Endianness::Little,
    );

    let text_id = obj.section_id(StandardSection::Text);
    let data_id = obj.section_id(StandardSection::Data);

    let mut rodata = Vec::new();
    let mut string_positions: Vec<usize> = Vec::new();
    for s in strings.iter() {
        let offset = rodata.len();
        rodata.extend_from_slice(s.as_bytes());
        rodata.push(0);
        string_positions.push(offset);
    }

    let argc_ro_offset = rodata.len();
    rodata.extend_from_slice(&[0u8; 16]);
    let argv_ro_offset = argc_ro_offset + 8;

    let rodata_id = data_id;
    let rodata_offset = obj.append_section_data(rodata_id, &rodata, 1);

    for (i, pos) in string_positions.iter().enumerate() {
        let sym_name = format!(".str.{}", i);
        let sym = Symbol {
            name: sym_name.into_bytes(),
            value: (*pos as u64) + rodata_offset,
            size: (strings[i].len()) as u64,
            kind: object::SymbolKind::Data,
            scope: SymbolScope::Compilation,
            weak: false,
            section: SymbolSection::Section(rodata_id),
            flags: object::SymbolFlags::None,
        };
        obj.add_symbol(sym);
    }

    let argc_sym = Symbol {
        name: b"__argc".to_vec(),
        value: (argc_ro_offset as u64) + rodata_offset,
        size: 8,
        kind: object::SymbolKind::Data,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(rodata_id),
        flags: object::SymbolFlags::None,
    };
    obj.add_symbol(argc_sym);

    let argv_sym = Symbol {
        name: b"__argv".to_vec(),
        value: (argv_ro_offset as u64) + rodata_offset,
        size: 8,
        kind: object::SymbolKind::Data,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(rodata_id),
        flags: object::SymbolFlags::None,
    };
    obj.add_symbol(argv_sym);

    let print_sym = Symbol {
        name: b"_print_i64".to_vec(),
        value: 0,
        size: 0,
        kind: object::SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Undefined,
        flags: object::SymbolFlags::None,
    };
    let _print_sym_id = obj.add_symbol(print_sym);

    let locals_count = locals.len();

    let _locals_id_opt: Option<object::write::SymbolId> = None;

    let mut text = Vec::new();

    text.extend_from_slice(&[0x48, 0x8B, 0x04, 0x24]);

    text.extend_from_slice(&[0x48, 0xB9]);
    let argc_reloc_offset = text.len();
    text.write_u64::<LittleEndian>(0).unwrap();

    text.extend_from_slice(&[0x48, 0x89, 0x01]);

    text.extend_from_slice(&[0x48, 0x8D, 0x44, 0x24, 0x08]);

    text.extend_from_slice(&[0x48, 0xB9]);
    let argv_reloc_offset = text.len();
    text.write_u64::<LittleEndian>(0).unwrap();
    text.extend_from_slice(&[0x48, 0x89, 0x01]);

    text.push(0x55);
    text.extend_from_slice(&[0x48, 0x89, 0xE5]);

    let mut alloc = (locals_count as i64) * 8;
    if alloc % 16 == 0 {
        alloc += 8;
    }
    if alloc > 0 {
        use byteorder::{LittleEndian, WriteBytesExt};
        text.extend_from_slice(&[0x48, 0x81, 0xEC]);
        text.write_i32::<LittleEndian>(alloc as i32).unwrap();
    }

    let mut reloc_entries: Vec<RelocEntry> = Vec::new();

    reloc_entries.push(RelocEntry {
        offset: argc_reloc_offset,
        sym_name: Some(b"__argc".to_vec()),
        _is_call: false,
    });
    reloc_entries.push(RelocEntry {
        offset: argv_reloc_offset,
        sym_name: Some(b"__argv".to_vec()),
        _is_call: false,
    });

    use byteorder::{LittleEndian, WriteBytesExt};

    let mut string_iter_idx = 0usize;

    let mut label_positions: std::collections::HashMap<usize, usize> =
        std::collections::HashMap::new();

    let mut jump_placeholders: Vec<(usize, usize)> = Vec::new();

    for instr in ir {
        match instr {
            IrInstr::EmitString(s) => {
                let idx = string_iter_idx;
                let sym_name = format!(".str.{}", idx);
                let _sym_id = obj
                    .symbol_id(sym_name.as_bytes())
                    .ok_or_else(|| format!("symbol not found {}", sym_name))?;

                if target.contains("windows") {
                    text.extend_from_slice(&[0x48, 0x83, 0xEC, 0x30]);

                    text.extend_from_slice(&[0x48, 0xC7, 0x44, 0x24, 0x20, 0x00, 0x00, 0x00, 0x00]);

                    text.extend_from_slice(&[0x48, 0xC7, 0xC1, 0xF5, 0xFF, 0xFF, 0xFF]);

                    text.extend_from_slice(&[0x48, 0xB8]);
                    let gsh_off = text.len();
                    text.write_u64::<LittleEndian>(0).unwrap();
                    text.extend_from_slice(&[0xFF, 0xD0]);

                    text.extend_from_slice(&[0x48, 0x89, 0xC1]);

                    text.extend_from_slice(&[0x48, 0xBA]);
                    let ptr_off = text.len();
                    text.write_u64::<LittleEndian>(0).unwrap();

                    text.push(0x41);
                    text.push(0xB8);
                    let _len_off = text.len();
                    text.write_u32::<LittleEndian>(s.len() as u32).unwrap();

                    text.extend_from_slice(&[0x4C, 0x8D, 0x4C, 0x24, 0x18]);

                    text.extend_from_slice(&[0x48, 0xB8]);
                    let wf_off = text.len();
                    text.write_u64::<LittleEndian>(0).unwrap();

                    text.extend_from_slice(&[0xFF, 0xD0]);

                    text.extend_from_slice(&[0x48, 0x83, 0xC4, 0x30]);

                    reloc_entries.push(RelocEntry {
                        offset: ptr_off,
                        sym_name: Some(sym_name.into_bytes()),
                        _is_call: false,
                    });

                    reloc_entries.push(RelocEntry {
                        offset: gsh_off,
                        sym_name: Some(b"GetStdHandle".to_vec()),
                        _is_call: false,
                    });
                    reloc_entries.push(RelocEntry {
                        offset: wf_off,
                        sym_name: Some(b"WriteFile".to_vec()),
                        _is_call: false,
                    });
                    string_iter_idx += 1;
                } else {
                    text.extend_from_slice(&[0x48, 0xBE]);
                    let imm64_off = text.len();
                    text.write_u64::<LittleEndian>(0).unwrap();

                    text.push(0xBA);
                    text.write_u32::<LittleEndian>(s.len() as u32).unwrap();

                    text.extend_from_slice(&[0xB8, 0x01, 0x00, 0x00, 0x00]);

                    text.extend_from_slice(&[0xBF, 0x01, 0x00, 0x00, 0x00]);

                    text.extend_from_slice(&[0x0F, 0x05]);
                    reloc_entries.push(RelocEntry {
                        offset: imm64_off,
                        sym_name: Some(sym_name.into_bytes()),
                        _is_call: false,
                    });
                    string_iter_idx += 1;
                }
            }
            IrInstr::PushConst(v) => {
                text.extend_from_slice(&[0x48, 0xB8]);
                text.write_u64::<LittleEndian>(v as u64).unwrap();
                text.push(0x50);
            }
            IrInstr::StoreLocal(idx) => {
                text.push(0x58);

                text.extend_from_slice(&[0x48, 0x89, 0x85]);
                let offset = 8 + (idx as i32 * 8);
                let disp32 = -offset;
                text.write_i32::<LittleEndian>(disp32).unwrap();
            }
            IrInstr::LoadLocal(idx) => {
                text.extend_from_slice(&[0x48, 0x8B, 0x85]);
                let offset = 8 + (idx as i32 * 8);
                let disp32 = -offset;
                text.write_i32::<LittleEndian>(disp32).unwrap();
                text.push(0x50);
            }
            IrInstr::EmitInt => {
                text.push(0x5F);

                text.push(0x53);
                text.extend_from_slice(&[0x48, 0x81, 0xEC]);
                text.write_i32::<LittleEndian>(0x80).unwrap();

                text.extend_from_slice(&[0x48, 0x89, 0xE3]);

                text.extend_from_slice(&[0x48, 0x81, 0xE4]);
                text.write_u32::<LittleEndian>(0xFFFFFFF0u32).unwrap();

                text.extend_from_slice(&[0x48, 0x89, 0xE6]);
                text.extend_from_slice(&[0x48, 0x83, 0xEE, 0x40]);

                text.extend_from_slice(&[0x48, 0xB8]);
                let imm64_off = text.len();
                text.write_u64::<LittleEndian>(0).unwrap();

                text.extend_from_slice(&[0xFF, 0xD0]);

                text.extend_from_slice(&[0x48, 0x89, 0xDC]);

                text.extend_from_slice(&[0x48, 0x81, 0xC4]);
                text.write_i32::<LittleEndian>(0x80).unwrap();
                text.push(0x5B);

                reloc_entries.push(RelocEntry {
                    offset: imm64_off,
                    sym_name: Some(b"_print_i64".to_vec()),
                    _is_call: false,
                });
            }
            IrInstr::EmitChar => {
                text.push(0x5F);

                text.push(0x53);
                text.extend_from_slice(&[0x48, 0x81, 0xEC]);
                text.write_i32::<LittleEndian>(0x80).unwrap();

                text.extend_from_slice(&[0x48, 0x89, 0xE3]);

                text.extend_from_slice(&[0x48, 0x81, 0xE4]);
                text.write_u32::<LittleEndian>(0xFFFFFFF0u32).unwrap();

                text.extend_from_slice(&[0x48, 0x89, 0xE6]);
                text.extend_from_slice(&[0x48, 0x83, 0xEE, 0x40]);

                text.extend_from_slice(&[0x48, 0xB8]);
                let imm64_off = text.len();
                text.write_u64::<LittleEndian>(0).unwrap();

                text.extend_from_slice(&[0xFF, 0xD0]);

                text.extend_from_slice(&[0x48, 0x89, 0xDC]);

                text.extend_from_slice(&[0x48, 0x81, 0xC4]);
                text.write_i32::<LittleEndian>(0x80).unwrap();
                text.push(0x5B);

                reloc_entries.push(RelocEntry {
                    offset: imm64_off,
                    sym_name: Some(b"_print_char".to_vec()),
                    _is_call: false,
                });
            }
            IrInstr::Pop => {
                text.push(0x58);
            }
            IrInstr::BinOp(op) => {
                text.push(0x5B);
                text.push(0x58);
                match op {
                    crate::syntax::BinOp::Add => {
                        text.extend_from_slice(&[0x48, 0x01, 0xD8]);
                    }
                    crate::syntax::BinOp::Sub => {
                        text.extend_from_slice(&[0x48, 0x29, 0xD8]);
                    }
                    crate::syntax::BinOp::Mul => {
                        text.extend_from_slice(&[0x48, 0x0F, 0xAF, 0xC3]);
                    }
                    crate::syntax::BinOp::Div => {
                        text.extend_from_slice(&[0x48, 0x99]);
                        text.extend_from_slice(&[0x48, 0xF7, 0xFB]);
                    }
                    _ => return Err("unsupported binop at runtime".into()),
                }
                text.push(0x50);
            }
            IrInstr::Compare(op) => {
                text.push(0x5B);
                text.push(0x58);

                text.extend_from_slice(&[0x48, 0x39, 0xD8]);

                let set_opcode = match op {
                    crate::syntax::BinOp::Eq => 0x94u8,
                    crate::syntax::BinOp::Ne => 0x95u8,
                    crate::syntax::BinOp::Lt => 0x9Cu8,
                    crate::syntax::BinOp::Le => 0x9Eu8,
                    crate::syntax::BinOp::Gt => 0x9Fu8,
                    crate::syntax::BinOp::Ge => 0x9Du8,
                    _ => return Err("unsupported comparison op".into()),
                };
                text.extend_from_slice(&[0x0F, set_opcode, 0xC0]);

                text.extend_from_slice(&[0x48, 0x0F, 0xB6, 0xC0]);
                text.push(0x50);
            }
            IrInstr::JumpIfFalse(lbl) => {
                text.push(0x58);
                text.extend_from_slice(&[0x48, 0x83, 0xF8, 0x00]);
                text.extend_from_slice(&[0x0F, 0x84]);
                let placeholder_off = text.len();
                text.write_i32::<LittleEndian>(0).unwrap();

                jump_placeholders.push((placeholder_off, lbl));
            }
            IrInstr::Jump(lbl) => {
                text.push(0xE9);
                let placeholder_off = text.len();
                text.write_i32::<LittleEndian>(0).unwrap();
                jump_placeholders.push((placeholder_off, lbl));
            }
            IrInstr::Label(id) => {
                label_positions.insert(id, text.len());

                if let Some((param_count, local_count)) = func_meta.get(&id) {
                    text.push(0x55);
                    text.extend_from_slice(&[0x48, 0x89, 0xE5]);

                    let mut f_alloc = (*local_count as i64) * 8;
                    if f_alloc % 16 == 0 {
                        f_alloc += 8;
                    }
                    if f_alloc > 0 {
                        text.extend_from_slice(&[0x48, 0x81, 0xEC]);
                        use byteorder::WriteBytesExt;
                        text.write_i32::<LittleEndian>(f_alloc as i32).unwrap();
                    }

                    let reg_codes: [u8; 6] = [7, 6, 2, 1, 8, 9];

                    for (i, &reg) in reg_codes.iter().enumerate().take(*param_count) {
                        let local_offset = 8 + (i as i32 * 8);
                        let reg_low = reg & 0x7;
                        let modrm = 0x80 | (reg_low << 3) | 0x05;
                        let rex: u8 = 0x48 | if reg >= 8 { 0x04 } else { 0x00 };
                        text.push(rex);
                        text.push(0x89);
                        text.push(modrm);
                        text.write_i32::<LittleEndian>(-local_offset).unwrap();
                    }

                    if *param_count > 6 {
                        for i in 6..*param_count {
                            let local_offset = 8 + (i as i32 * 8);
                            text.extend_from_slice(&[0x48, 0x8B, 0x85]);
                            let src_disp = 16 + ((i - 6) as i32 * 8);
                            text.write_i32::<LittleEndian>(src_disp).unwrap();

                            text.extend_from_slice(&[0x48, 0x89, 0x85]);
                            text.write_i32::<LittleEndian>(-local_offset).unwrap();
                        }
                    }
                }
            }
            IrInstr::Call(name, nargs) => {
                let num_reg = if nargs > 6 { 6 } else { nargs };
                let stack_count = nargs.saturating_sub(6);
                let use_pop_args = nargs <= 6;
                let mut scratch_alloc: i32 = 0;

                use byteorder::WriteBytesExt as _;

                if nargs > 0 {
                    if use_pop_args {
                        for i in (0..nargs).rev() {
                            match i {
                                0 => text.push(0x5F),
                                1 => text.push(0x5E),
                                2 => text.push(0x5A),
                                3 => text.push(0x59),
                                4 => text.extend_from_slice(&[0x41, 0x58]),
                                5 => text.extend_from_slice(&[0x41, 0x59]),
                                _ => {}
                            }
                        }
                    } else {
                        let pad = if stack_count % 2 == 1 { 8 } else { 0 };

                        use byteorder::WriteBytesExt as _;

                        scratch_alloc = (nargs as i32 * 8) + pad;
                        if scratch_alloc > 0 {
                            text.extend_from_slice(&[0x48, 0x81, 0xEC]);
                            text.write_i32::<LittleEndian>(scratch_alloc).unwrap();
                        }

                        for k in 0..nargs {
                            let orig_off = ((nargs - 1 - k) as i32) * 8;
                            let from_off = scratch_alloc + orig_off;

                            text.extend_from_slice(&[0x48, 0x8B, 0x84, 0x24]);
                            text.write_i32::<LittleEndian>(from_off).unwrap();

                            let dest = pad + (k as i32 * 8);
                            text.extend_from_slice(&[0x48, 0x89, 0x84, 0x24]);
                            text.write_i32::<LittleEndian>(dest).unwrap();
                        }

                        for i in 0..num_reg {
                            let src_off = pad + (i as i32 * 8);

                            text.extend_from_slice(&[0x48, 0x8B, 0x84, 0x24]);
                            text.write_i32::<LittleEndian>(src_off).unwrap();

                            match i {
                                0 => text.extend_from_slice(&[0x48, 0x89, 0xC7]),
                                1 => text.extend_from_slice(&[0x48, 0x89, 0xC6]),
                                2 => text.extend_from_slice(&[0x48, 0x89, 0xC2]),
                                3 => text.extend_from_slice(&[0x48, 0x89, 0xC1]),
                                4 => text.extend_from_slice(&[0x49, 0x89, 0xC0]),
                                5 => text.extend_from_slice(&[0x49, 0x89, 0xC1]),
                                _ => {}
                            }
                        }

                        text.extend_from_slice(&[0x4C, 0x8B, 0xDC]);
                        for j in (num_reg..nargs).rev() {
                            let src_off = pad + (j as i32 * 8);

                            text.extend_from_slice(&[0x49, 0x8B, 0x83]);
                            text.write_i32::<LittleEndian>(src_off).unwrap();

                            text.push(0x50);
                        }
                    }
                }

                if let Some(lbl) = func_label_map.get(&name) {
                    text.push(0xE8);
                    let placeholder_off = text.len();
                    text.write_i32::<LittleEndian>(0).unwrap();
                    jump_placeholders.push((placeholder_off, *lbl));
                } else {
                    text.extend_from_slice(&[0x48, 0xB8]);
                    let imm64_off = text.len();
                    text.write_u64::<LittleEndian>(0).unwrap();

                    text.extend_from_slice(&[0xFF, 0xD0]);

                    let _sym_id = if let Some(sid) = obj.symbol_id(name.as_bytes()) {
                        sid
                    } else {
                        let sym = Symbol {
                            name: name.as_bytes().to_vec(),
                            value: 0,
                            size: 0,
                            kind: object::SymbolKind::Text,
                            scope: SymbolScope::Linkage,
                            weak: false,
                            section: SymbolSection::Undefined,
                            flags: object::SymbolFlags::None,
                        };
                        obj.add_symbol(sym)
                    };
                    reloc_entries.push(RelocEntry {
                        offset: imm64_off,
                        sym_name: Some(name.as_bytes().to_vec()),
                        _is_call: false,
                    });
                }

                if !use_pop_args {
                    if stack_count > 0 {
                        text.extend_from_slice(&[0x48, 0x81, 0xC4]);
                        text.write_i32::<LittleEndian>(stack_count as i32 * 8)
                            .unwrap();
                    }

                    if scratch_alloc > 0 {
                        text.extend_from_slice(&[0x48, 0x81, 0xC4]);
                        text.write_i32::<LittleEndian>(scratch_alloc).unwrap();
                    }

                    if nargs > 0 {
                        text.extend_from_slice(&[0x48, 0x81, 0xC4]);
                        text.write_i32::<LittleEndian>(nargs as i32 * 8).unwrap();
                    }
                }

                text.push(0x50);
            }
            IrInstr::Ret => {
                text.push(0x58);

                text.extend_from_slice(&[0x48, 0x89, 0xEC]);
                text.push(0x5D);
                text.push(0xC3);
            }
        }
    }

    text.extend_from_slice(&[0x48, 0x89, 0xEC]);
    text.push(0x5D);

    if target.contains("windows") {
        text.extend_from_slice(&[0x31, 0xC9]);

        text.extend_from_slice(&[0x48, 0xB8]);
        let exit_off = text.len();
        text.write_u64::<LittleEndian>(0).unwrap();
        text.extend_from_slice(&[0xFF, 0xD0]);

        reloc_entries.push(RelocEntry {
            offset: exit_off,
            sym_name: Some(b"ExitProcess".to_vec()),
            _is_call: false,
        });
    } else {
        text.extend_from_slice(&[0xB8, 0x3C, 0x00, 0x00, 0x00]);
        text.extend_from_slice(&[0x31, 0xFF]);
        text.extend_from_slice(&[0x0F, 0x05]);
    }

    for (placeholder_off, lbl) in jump_placeholders.iter() {
        let label_pos = label_positions
            .get(lbl)
            .ok_or_else(|| format!("undefined label {}", lbl))?;

        let rel = (*label_pos as i64) - ((*placeholder_off as i64) + 4);
        let rel32 = rel as i32;
        let b = rel32.to_le_bytes();
        let po = *placeholder_off;
        text[po..po + 4].copy_from_slice(&b);
    }

    let text_off = obj.append_section_data(text_id, &text, 16);

    for (fname, _pcount, _f_ir, _loc_count, lbl) in function_infos.iter() {
        if let Some(pos) = label_positions.get(lbl) {
            let sym = Symbol {
                name: fname.as_bytes().to_vec(),
                value: (*pos as u64) + text_off,
                size: 0,
                kind: object::SymbolKind::Text,
                scope: SymbolScope::Linkage,
                weak: false,
                section: SymbolSection::Section(text_id),
                flags: object::SymbolFlags::None,
            };
            obj.add_symbol(sym);
        }
    }

    let start_sym = Symbol {
        name: b"_start".to_vec(),
        value: text_off as u64,
        size: text.len() as u64,
        kind: object::SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(text_id),
        flags: object::SymbolFlags::None,
    };
    obj.add_symbol(start_sym);

    let out_dir = proj_dir.join("build").join(target);
    std::fs::create_dir_all(&out_dir)
        .map_err(|e| format!("Failed to create output dir {}: {}", out_dir.display(), e))?;
    let obj_path = out_dir.join(format!("{}.o", proj_name));

    if !target.contains("windows") {
        let obj_bytes = obj
            .write()
            .map_err(|e| format!("object write error: {}", e))?;
        std::fs::write(&obj_path, &obj_bytes)
            .map_err(|e| format!("Failed to write object file: {}", e))?;
    }

    let helpers: std::collections::BTreeMap<&str, Vec<u8>> = runtime_helpers::get_helpers(target);

    let mut helper_pos: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for (name, bytes) in helpers.iter() {
        let pos = text.len();
        text.extend_from_slice(bytes);
        helper_pos.insert(name.to_string(), pos);

        if *name == "__cstr_to_string" {
            reloc_entries.push(RelocEntry {
                offset: pos + 16,
                sym_name: Some(b"array_new".to_vec()),
                _is_call: false,
            });
            reloc_entries.push(RelocEntry {
                offset: pos + 46,
                sym_name: Some(b"push".to_vec()),
                _is_call: false,
            });
        }
        if *name == "__print_str" {
            reloc_entries.push(RelocEntry {
                offset: pos + 21,
                sym_name: Some(b"len".to_vec()),
                _is_call: false,
            });
            reloc_entries.push(RelocEntry {
                offset: pos + 50,
                sym_name: Some(b"array_get".to_vec()),
                _is_call: false,
            });
            if target.contains("windows") {
                reloc_entries.push(RelocEntry {
                    offset: pos + 83,
                    sym_name: Some(b"GetStdHandle".to_vec()),
                    _is_call: false,
                });

                reloc_entries.push(RelocEntry {
                    offset: pos + 116,
                    sym_name: Some(b"WriteFile".to_vec()),
                    _is_call: false,
                });
            }
        }

        if *name == "__build_args_array" {
            reloc_entries.push(RelocEntry {
                offset: pos + 2,
                sym_name: Some(b"__argc".to_vec()),
                _is_call: false,
            });
            reloc_entries.push(RelocEntry {
                offset: pos + 15,
                sym_name: Some(b"__argv".to_vec()),
                _is_call: false,
            });

            reloc_entries.push(RelocEntry {
                offset: pos + 41,
                sym_name: Some(b"array_new".to_vec()),
                _is_call: false,
            });
            reloc_entries.push(RelocEntry {
                offset: pos + 71,
                sym_name: Some(b"__cstr_to_string".to_vec()),
                _is_call: false,
            });
            reloc_entries.push(RelocEntry {
                offset: pos + 89,
                sym_name: Some(b"push".to_vec()),
                _is_call: false,
            });
            if target.contains("windows") {
                reloc_entries.push(RelocEntry {
                    offset: pos + 41,
                    sym_name: Some(b"GetCommandLineA".to_vec()),
                    _is_call: false,
                });
            }
        }

        if *name == "heap_alloc" && target.contains("windows") {
            reloc_entries.push(RelocEntry {
                offset: pos + 15,
                sym_name: Some(b"GetProcessHeap".to_vec()),
                _is_call: false,
            });

            reloc_entries.push(RelocEntry {
                offset: pos + 36,
                sym_name: Some(b"HeapAlloc".to_vec()),
                _is_call: false,
            });
        }
    }

    let exe_path = if target.contains("windows") {
        windows::write_executable_from_sections(
            &out_dir,
            proj_name,
            target,
            &mut text,
            &rodata,
            &string_positions,
            &helper_pos,
            &reloc_entries,
            &func_label_map,
            &label_positions,
            argc_ro_offset,
            argv_ro_offset,
        )?
    } else {
        linux::write_executable_from_sections(
            &out_dir,
            proj_name,
            target,
            &mut text,
            &rodata,
            &string_positions,
            &helper_pos,
            &reloc_entries,
            &func_label_map,
            &label_positions,
            argc_ro_offset,
            argv_ro_offset,
        )?
    };

    Ok(exe_path)
}
