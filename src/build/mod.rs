use object::SymbolScope;
use object::write::{Object, StandardSection, Symbol, SymbolSection};
use object::{Architecture, BinaryFormat, Endianness};
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::frontend;

pub fn compile_project(
    proj_dir: &Path,
    proj_name: &str,
    target: &str,
    opt_level: Option<i32>,
) -> Result<PathBuf, String> {
    if !target.contains("linux") {
        return Err("Only linux targets supported for this backend".into());
    }

    let main_path = proj_dir.join("src").join("main.sd");
    let src = std::fs::read_to_string(&main_path)
        .map_err(|e| format!("Failed to read {}: {}", main_path.display(), e))?;
    let prog = crate::frontend::parse(&src).map_err(|e| format!("Parse error: {}", e))?;

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
            Expr::Unary { op, expr } => match op {
                crate::syntax::UnaryOp::Neg => eval_const_integer(expr).map(|v| -v),
                _ => None,
            },
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
                for a in args {
                    lower_expr_to_ir(a, out, locals, next_label)?;
                }
                out.push(IrInstr::Call(name.clone(), args.len()));
                Ok(())
            }
            Expr::Unary { op, expr } => match op {
                UnaryOp::Neg => {
                    lower_expr_to_ir(expr, out, locals, next_label)?;
                    out.push(IrInstr::PushConst(-1));
                    out.push(IrInstr::BinOp(BinOp::Mul));
                    Ok(())
                }
                UnaryOp::Not => {
                    lower_expr_to_ir(expr, out, locals, next_label)?;
                    out.push(IrInstr::PushConst(0));
                    out.push(IrInstr::Compare(BinOp::Eq));
                    Ok(())
                }
            },
            Expr::Binary { left, op, right } => {
                use crate::syntax::BinOp::*;
                match op {
                    Add | Sub | Mul | Div => {
                        lower_expr_to_ir(left, out, locals, next_label)?;
                        lower_expr_to_ir(right, out, locals, next_label)?;
                        out.push(IrInstr::BinOp(op.clone()));
                        Ok(())
                    }
                    Eq | Ne | Lt | Le | Gt | Ge => {
                        lower_expr_to_ir(left, out, locals, next_label)?;
                        lower_expr_to_ir(right, out, locals, next_label)?;
                        out.push(IrInstr::Compare(op.clone()));
                        Ok(())
                    }
                    And => {
                        let else_lbl = *next_label;
                        *next_label += 1;
                        let end_lbl = *next_label;
                        *next_label += 1;
                        lower_expr_to_ir(left, out, locals, next_label)?;
                        out.push(IrInstr::JumpIfFalse(else_lbl));
                        lower_expr_to_ir(right, out, locals, next_label)?;
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
                        lower_expr_to_ir(left, out, locals, next_label)?;
                        out.push(IrInstr::JumpIfFalse(eval_lbl));

                        out.push(IrInstr::PushConst(1));
                        out.push(IrInstr::Jump(end_lbl));
                        out.push(IrInstr::Label(eval_lbl));
                        lower_expr_to_ir(right, out, locals, next_label)?;
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
                    lower_expr_to_ir(el, out, locals, next_label)?;
                    out.push(IrInstr::Call("push".into(), 2));
                }
                Ok(())
            }
            Expr::Index { expr: arr, index } => {
                lower_expr_to_ir(arr, out, locals, next_label)?;
                lower_expr_to_ir(index, out, locals, next_label)?;
                out.push(IrInstr::Call("array_get".into(), 2));
                Ok(())
            }
            _ => Err(format!("unsupported expression in runtime lowering: {:?}", e).into()),
        }
    }

    fn lower_stmt_to_ir(
        stmt: &crate::syntax::Stmt,
        locals: &mut std::collections::HashMap<String, usize>,
        next_local: &mut usize,
        next_label: &mut usize,
        break_lbl: Option<usize>,
        continue_lbl: Option<usize>,
    ) -> Result<Vec<IrInstr>, String> {
        use crate::syntax::*;
        match stmt {
            Stmt::Let { name, value, .. } => {
                let mut res = Vec::new();
                lower_expr_to_ir(value, &mut res, locals, next_label)?;
                let idx = if let Some(&i) = locals.get(name) {
                    i
                } else {
                    let i = *next_local;
                    locals.insert(name.clone(), i);
                    *next_local += 1;
                    i
                };
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

                lower_expr_to_ir(index, &mut res, locals, next_label)?;
                lower_expr_to_ir(value, &mut res, locals, next_label)?;
                res.push(IrInstr::Call("array_set".into(), 3));
                Ok(res)
            }
            Stmt::Assign { name, value } => {
                let mut res = Vec::new();
                lower_expr_to_ir(value, &mut res, locals, next_label)?;
                let idx = locals
                    .get(name)
                    .ok_or_else(|| format!("assign to unknown variable '{}'", name))?;
                res.push(IrInstr::StoreLocal(*idx));
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
                        return Ok(vec![IrInstr::EmitString(format!("{}\n", fmt))]);
                    }
                    if placeholders != args.len() - 1 {
                        return Err("placeholder count does not match arguments".into());
                    }
                    let mut res: Vec<IrInstr> = Vec::new();
                    for (i, p) in parts.iter().enumerate() {
                        if !p.is_empty() {
                            res.push(IrInstr::EmitString(p.clone()));
                        }
                        if i < placeholders {
                            let arg_expr = &args[i + 1];
                            if let Some(v) = eval_const_integer(arg_expr) {
                                res.push(IrInstr::EmitString(format!("{}", v)));
                            } else {
                                lower_expr_to_ir(arg_expr, &mut res, locals, next_label)?;
                                res.push(IrInstr::EmitInt);
                            }
                        }
                    }
                    res.push(IrInstr::EmitString("\n".to_string()));
                    return Ok(res);
                }
                Err("first argument to println must be a string literal format".into())
            }
            Stmt::Expr(expr) => {
                let mut res = Vec::new();
                lower_expr_to_ir(expr, &mut res, locals, next_label)?;

                res.push(IrInstr::Pop);
                return Ok(res);
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
                lower_expr_to_ir(cond, &mut res, locals, next_label)?;
                res.push(IrInstr::JumpIfFalse(else_lbl));
                for s in then_block {
                    let mut inner = lower_stmt_to_ir(
                        s,
                        locals,
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
                lower_expr_to_ir(cond, &mut res, locals, next_label)?;
                res.push(IrInstr::JumpIfFalse(end_lbl));
                for s in body {
                    let mut inner = lower_stmt_to_ir(
                        s,
                        locals,
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

                let arr_local = if let Some(&i) = locals.get(var) {
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

                lower_expr_to_ir(iterable, &mut res, locals, next_label)?;
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
                lower_expr_to_ir(expr, &mut res, locals, next_label)?;
                res.push(IrInstr::Ret);
                Ok(res)
            }
            _ => Err(format!("unsupported statement for lowering: {:?}", stmt).into()),
        }
    }

    let mut function_infos: Vec<(String, usize, Vec<IrInstr>, usize, usize)> = Vec::new();
    let mut func_label_map: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();

    let mut func_meta: std::collections::HashMap<usize, (usize, usize)> =
        std::collections::HashMap::new();
    let mut next_label: usize = 0;
    let mut next_label_for_funcs = next_label;
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
            let mut next_local_idx: usize = 0;

            for (i, (pname, _pty)) in params.iter().enumerate() {
                locals_map.insert(pname.clone(), i);
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
    let mut next_local: usize = 0;

    let exit_label = next_label_for_funcs;
    next_label_for_funcs += 1;
    for stmt in main {
        let mut is = lower_stmt_to_ir(
            &stmt,
            &mut locals,
            &mut next_local,
            &mut next_label_for_funcs,
            None,
            None,
        )
        .map_err(|e| format!("lowering error: {}", e))?;
        ir.append(&mut is);
    }

    ir.push(IrInstr::Jump(exit_label));

    for (fname, _pcount, mut f_ir, _loc_count, lbl) in function_infos.drain(..) {
        ir.push(IrInstr::Label(lbl));

        ir.append(&mut f_ir);
    }

    ir.push(IrInstr::Label(exit_label));

    next_label = next_label_for_funcs;

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

    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);

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
    let print_sym_id = obj.add_symbol(print_sym);

    let locals_count = locals.len();

    let locals_id_opt: Option<object::write::SymbolId> = None;

    let mut text = Vec::new();

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

    struct RelocEntry {
        offset: usize,
        sym_name: Option<Vec<u8>>,
        is_call: bool,
    }
    let mut reloc_entries: Vec<RelocEntry> = Vec::new();

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
                let sym_id = obj
                    .symbol_id(sym_name.as_bytes())
                    .ok_or_else(|| format!("symbol not found {}", sym_name))?;

                text.extend_from_slice(&[0x48, 0xBE]);
                let imm64_off = text.len();
                text.write_u64::<LittleEndian>(0).unwrap();

                text.push(0xBA);
                text.write_u32::<LittleEndian>(s.as_bytes().len() as u32)
                    .unwrap();

                text.extend_from_slice(&[0xB8, 0x01, 0x00, 0x00, 0x00]);

                text.extend_from_slice(&[0xBF, 0x01, 0x00, 0x00, 0x00]);

                text.extend_from_slice(&[0x0F, 0x05]);
                reloc_entries.push(RelocEntry {
                    offset: imm64_off as usize,
                    sym_name: Some(sym_name.into_bytes()),
                    is_call: false,
                });
                string_iter_idx += 1;
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
                    offset: imm64_off as usize,
                    sym_name: Some(b"_print_i64".to_vec()),
                    is_call: false,
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
                    for i in 0..(*param_count) {
                        let local_offset = 8 + (i as i32 * 8);
                        if i < 6 {
                            let reg = reg_codes[i];
                            let reg_low = reg & 0x7;
                            let modrm = 0x80 | (reg_low << 3) | 0x05;
                            let rex: u8 = 0x48 | if reg >= 8 { 0x04 } else { 0x00 };
                            text.push(rex);
                            text.push(0x89);
                            text.push(modrm);
                            text.write_i32::<LittleEndian>(-local_offset).unwrap();
                        } else {
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
                let stack_count = if nargs > 6 { nargs - 6 } else { 0 };
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

                        scratch_alloc = (nargs as i32 * 8) + pad as i32;
                        if scratch_alloc > 0 {
                            text.extend_from_slice(&[0x48, 0x81, 0xEC]);
                            text.write_i32::<LittleEndian>(scratch_alloc).unwrap();
                        }

                        for k in 0..nargs {
                            let orig_off = ((nargs - 1 - k) as i32) * 8;
                            let from_off = scratch_alloc + orig_off;

                            text.extend_from_slice(&[0x48, 0x8B, 0x84, 0x24]);
                            text.write_i32::<LittleEndian>(from_off).unwrap();

                            let dest = (pad as i32) + (k as i32 * 8);
                            text.extend_from_slice(&[0x48, 0x89, 0x84, 0x24]);
                            text.write_i32::<LittleEndian>(dest).unwrap();
                        }

                        for i in 0..num_reg {
                            let src_off = (pad as i32) + (i as i32 * 8);

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
                            let src_off = (pad as i32) + (j as i32 * 8);

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

                    let sym_id = if let Some(sid) = obj.symbol_id(name.as_bytes()) {
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
                        offset: imm64_off as usize,
                        sym_name: Some(name.as_bytes().to_vec()),
                        is_call: false,
                    });
                }

                if !use_pop_args {
                    if stack_count > 0 {
                        text.extend_from_slice(&[0x48, 0x81, 0xC4]);
                        text.write_i32::<LittleEndian>((stack_count as i32 * 8))
                            .unwrap();
                    }

                    if scratch_alloc > 0 {
                        text.extend_from_slice(&[0x48, 0x81, 0xC4]);
                        text.write_i32::<LittleEndian>(scratch_alloc).unwrap();
                    }

                    if nargs > 0 {
                        text.extend_from_slice(&[0x48, 0x81, 0xC4]);
                        text.write_i32::<LittleEndian>((nargs as i32 * 8)).unwrap();
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

    text.extend_from_slice(&[0xB8, 0x3C, 0x00, 0x00, 0x00]);
    text.extend_from_slice(&[0x31, 0xFF]);
    text.extend_from_slice(&[0x0F, 0x05]);

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
    let obj_bytes = obj
        .write()
        .map_err(|e| format!("object write error: {}", e))?;
    std::fs::write(&obj_path, &obj_bytes)
        .map_err(|e| format!("Failed to write object file: {}", e))?;

    let helpers: std::collections::BTreeMap<&str, Vec<u8>> = {
        let mut m = std::collections::BTreeMap::new();

        m.insert(
            "_print_i64",
            vec![
                0x55, 0x48, 0x89, 0xe5, 0x53, 0x41, 0x54, 0x41, 0x55, 0x48, 0x83, 0xec, 0x20, 0x48,
                0x89, 0xf8, 0x90, 0x90, 0x90, 0x90, 0x48, 0xc7, 0xc1, 0x00, 0x00, 0x00, 0x00, 0x49,
                0x89, 0xc4, 0x49, 0x83, 0xfc, 0x00, 0x7d, 0x06, 0x49, 0xf7, 0xdc, 0x41, 0xb5, 0x01,
                0x48, 0xc7, 0xc2, 0x00, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc3, 0x0a, 0x00, 0x00, 0x00,
                0x48, 0x99, 0x48, 0xf7, 0xfb, 0x80, 0xc2, 0x30, 0x88, 0x14, 0x0e, 0x48, 0xff, 0xc1,
                0x48, 0x83, 0xf8, 0x00, 0x75, 0xe5, 0x41, 0x80, 0xfd, 0x00, 0x74, 0x07, 0xc6, 0x04,
                0x0e, 0x2d, 0x48, 0xff, 0xc1, 0x48, 0xc7, 0xc2, 0x00, 0x00, 0x00, 0x00, 0x48, 0x8d,
                0x3c, 0x0e, 0x48, 0xff, 0xcf, 0x4c, 0x8d, 0x04, 0x16, 0x49, 0x39, 0xf8, 0x7d, 0x12,
                0x8a, 0x04, 0x16, 0x8a, 0x1f, 0x88, 0x1c, 0x16, 0x88, 0x07, 0x48, 0xff, 0xc2, 0x48,
                0xff, 0xcf, 0xeb, 0xe5, 0x48, 0xc7, 0xc0, 0x01, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc7,
                0x01, 0x00, 0x00, 0x00, 0x48, 0x89, 0xf6, 0x48, 0x89, 0xca, 0x0f, 0x05, 0x48, 0x83,
                0xc4, 0x20, 0x41, 0x5d, 0x41, 0x5c, 0x5b, 0x5d, 0xc3, 0x55, 0x48, 0x89, 0xe5, 0x53,
                0x41, 0x54, 0x41, 0x55, 0x48, 0x83, 0xec, 0x20, 0x48, 0xc7, 0xc7, 0x00, 0x00, 0x00,
                0x00, 0x48, 0xc7, 0xc6, 0x10, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc2, 0x03, 0x00, 0x00,
                0x00, 0x49, 0xc7, 0xc2, 0x22, 0x00, 0x00, 0x00, 0x4d, 0x31, 0xc0, 0x4d, 0x31, 0xc9,
                0x48, 0xc7, 0xc0, 0x09, 0x00, 0x00, 0x00, 0x0f, 0x05, 0x48, 0xc7, 0x00, 0xc7, 0x40,
                0x08, 0x00, 0x00, 0x00, 0x00, 0x48, 0x83, 0xc4, 0x20, 0x41, 0x5d, 0x41, 0x5c, 0x5b,
                0x5d, 0xc3, 0x55, 0x48, 0x89, 0xe5, 0x53, 0x41, 0x54, 0x41, 0x55, 0x41, 0x56, 0x41,
                0x57, 0x48, 0x83, 0xec, 0x20, 0x49, 0x89, 0xfc, 0x49, 0x89, 0xf6, 0x49, 0x8b, 0x1c,
                0x24, 0x4d, 0x8b, 0x6c, 0x24, 0x08, 0x48, 0xc7, 0xc7, 0x00, 0x00, 0x00, 0x00, 0x48,
                0xc7, 0xc6, 0x10, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc2, 0x03, 0x00, 0x00, 0x00, 0x49,
                0xc7, 0xc2, 0x22, 0x00, 0x00, 0x00, 0x4d, 0x31, 0xc0, 0x4d, 0x31, 0xc9, 0x48, 0xc7,
                0xc0, 0x09, 0x00, 0x00, 0x00, 0x0f, 0x05, 0x4c, 0x89, 0x30, 0x48, 0xc7, 0x40, 0x08,
                0x00, 0x00, 0x00, 0x00, 0x49, 0x83, 0xfd, 0x00, 0x74, 0x12, 0x4d, 0x89, 0xef, 0x4d,
                0x8b, 0x7f, 0x08, 0x4d, 0x85, 0xff, 0x75, 0xf7, 0x49, 0x89, 0x45, 0x08, 0xeb, 0x05,
                0x49, 0x89, 0x44, 0x24, 0x08, 0x49, 0xff, 0x04, 0x24, 0x4c, 0x89, 0xe0, 0x48, 0x83,
                0xc4, 0x20, 0x41, 0x5f, 0x41, 0x5e, 0x41, 0x5d, 0x41, 0x5c, 0x5b, 0x5d, 0xc3, 0x55,
                0x48, 0x89, 0xe5, 0x53, 0x48, 0x83, 0xec, 0x10, 0x48, 0x8b, 0x47, 0x08, 0x48, 0x85,
                0xc0, 0x74, 0x20, 0x48, 0x89, 0xf3, 0x48, 0x85, 0xdb, 0x74, 0x0e, 0x48, 0x8b, 0x40,
                0x08, 0x48, 0x85, 0xc0, 0x74, 0x0f, 0x48, 0xff, 0xcb, 0xeb, 0xed, 0x48, 0x8b, 0x00,
                0x48, 0x83, 0xc4, 0x10, 0x5b, 0x5d, 0xc3,
            ],
        );

        m.insert(
            "array_new",
            vec![
                0x55, 0x48, 0x89, 0xe5, 0x53, 0x41, 0x54, 0x41, 0x55, 0x48, 0x83, 0xec, 0x20, 0x48,
                0xc7, 0xc7, 0x00, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc6, 0x10, 0x00, 0x00, 0x00, 0x48,
                0xc7, 0xc2, 0x03, 0x00, 0x00, 0x00, 0x49, 0xc7, 0xc2, 0x22, 0x00, 0x00, 0x00, 0x4d,
                0x31, 0xc0, 0x4d, 0x31, 0xc9, 0x48, 0xc7, 0xc0, 0x09, 0x00, 0x00, 0x00, 0x0f, 0x05,
                0x48, 0xc7, 0x00, 0x00, 0x00, 0x00, 0x00, 0x48, 0xc7, 0x40, 0x08, 0x00, 0x00, 0x00,
                0x00, 0x48, 0x83, 0xc4, 0x20, 0x41, 0x5d, 0x41, 0x5c, 0x5b, 0x5d, 0xc3,
            ],
        );

        m.insert(
            "push",
            vec![
                0x55, 0x48, 0x89, 0xe5, 0x53, 0x41, 0x54, 0x41, 0x55, 0x41, 0x56, 0x41, 0x57, 0x49,
                0x89, 0xfc, 0x49, 0x89, 0xf5, 0x48, 0xc7, 0xc7, 0x00, 0x00, 0x00, 0x00, 0x48, 0xc7,
                0xc6, 0x20, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc2, 0x03, 0x00, 0x00, 0x00, 0x49, 0xc7,
                0xc2, 0x22, 0x00, 0x00, 0x00, 0x49, 0xc7, 0xc0, 0xff, 0xff, 0xff, 0xff, 0x4d, 0x31,
                0xc9, 0x48, 0xc7, 0xc0, 0x09, 0x00, 0x00, 0x00, 0x0f, 0x05, 0x49, 0x89, 0xc6, 0x4d,
                0x89, 0x2e, 0x49, 0xc7, 0x46, 0x08, 0x00, 0x00, 0x00, 0x00, 0x49, 0x8b, 0x5c, 0x24,
                0x08, 0x48, 0x85, 0xdb, 0x74, 0x14, 0x48, 0x8b, 0x43, 0x08, 0x48, 0x85, 0xc0, 0x74,
                0x05, 0x48, 0x89, 0xc3, 0xeb, 0xf2, 0x4c, 0x89, 0x73, 0x08, 0xeb, 0x05, 0x4d, 0x89,
                0x74, 0x24, 0x08, 0x49, 0x8b, 0x04, 0x24, 0x48, 0x83, 0xc0, 0x01, 0x49, 0x89, 0x04,
                0x24, 0x4c, 0x89, 0xe0, 0x41, 0x5f, 0x41, 0x5e, 0x41, 0x5d, 0x41, 0x5c, 0x5b, 0x5d,
                0xc3,
            ],
        );

        m.insert(
            "array_get",
            vec![
                0x55, 0x48, 0x89, 0xe5, 0x53, 0x48, 0x83, 0xec, 0x10, 0x48, 0x8b, 0x47, 0x08, 0x48,
                0x85, 0xc0, 0x74, 0x20, 0x48, 0x89, 0xf3, 0x48, 0x85, 0xdb, 0x74, 0x0e, 0x48, 0x8b,
                0x40, 0x08, 0x48, 0x85, 0xc0, 0x74, 0x0f, 0x48, 0xff, 0xcb, 0xeb, 0xed, 0x48, 0x8b,
                0x00, 0x48, 0x83, 0xc4, 0x10, 0x5b, 0x5d, 0xc3, 0x48, 0xc7, 0xc0, 0x00, 0x00, 0x00,
                0x00, 0x48, 0x83, 0xc4, 0x10, 0x5b, 0x5d, 0xc3,
            ],
        );

        m.insert(
            "array_set",
            vec![
                0x55, 0x48, 0x89, 0xe5, 0x53, 0x48, 0x83, 0xec, 0x10, 0x48, 0x8b, 0x47, 0x08, 0x48,
                0x89, 0xf3, 0x48, 0x85, 0xdb, 0x74, 0x0e, 0x48, 0x8b, 0x40, 0x08, 0x48, 0x85, 0xc0,
                0x74, 0x08, 0x48, 0xff, 0xcb, 0xeb, 0xed, 0x48, 0x89, 0x10, 0x48, 0xc7, 0xc0, 0x00,
                0x00, 0x00, 0x00, 0x48, 0x83, 0xc4, 0x10, 0x5b, 0x5d, 0xc3,
            ],
        );

        m.insert(
            "len",
            vec![0x55, 0x48, 0x89, 0xe5, 0x48, 0x8b, 0x07, 0x5d, 0xc3],
        );

        m.insert(
            "early_stop",
            vec![
                0xB8, 0x27, 0x00, 0x00, 0x00, 0x0F, 0x05, 0x48, 0x89, 0xC7, 0xBE, 0x13, 0x00, 0x00,
                0x00, 0xB8, 0x3E, 0x00, 0x00, 0x00, 0x0F, 0x05, 0xC3,
            ],
        );

        m
    };

    let mut helper_pos: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for (name, bytes) in helpers.iter() {
        let pos = text.len();
        text.extend_from_slice(bytes);
        helper_pos.insert(name.to_string(), pos);
    }

    let base_vaddr: u64 = 0x400000;
    let file_text_off: usize = 0x1000;

    for r in reloc_entries.iter() {
        let sym_name = r
            .sym_name
            .as_ref()
            .ok_or_else(|| "missing symbol name for relocation".to_string())?;
        let sym = String::from_utf8_lossy(sym_name).into_owned();
        let sym_pos_opt = if let Some(p) = helper_pos.get(sym.as_str()) {
            Some(*p)
        } else if sym.starts_with(".str.") {
            if let Ok(idx) = sym[5..].parse::<usize>() {
                if idx < string_positions.len() {
                    Some(text.len() + string_positions[idx])
                } else {
                    None
                }
            } else {
                None
            }
        } else if let Some(lbl) = func_label_map.get(&sym) {
            label_positions.get(lbl).cloned()
        } else if sym == "_start" {
            Some(0usize)
        } else {
            None
        };
        let pos = sym_pos_opt.ok_or_else(|| format!("undefined symbol in relocation: {}", sym))?;
        let addr = base_vaddr + (file_text_off as u64) + (pos as u64);
        let off = r.offset;
        let mut le = (addr).to_le_bytes();
        if off + 8 > text.len() {
            return Err(format!(
                "relocation offset out of range: {} > {}",
                off + 8,
                text.len()
            )
            .into());
        }
        text[off..off + 8].copy_from_slice(&le);
    }

    use std::io::Write as _;
    fn write_u16(buf: &mut Vec<u8>, v: u16) {
        buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_u32(buf: &mut Vec<u8>, v: u32) {
        buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_u64(buf: &mut Vec<u8>, v: u64) {
        buf.extend_from_slice(&v.to_le_bytes());
    }

    let mut elf: Vec<u8> = Vec::new();

    elf.extend_from_slice(&[0x7f, b'E', b'L', b'F']);
    elf.push(2);
    elf.push(1);
    elf.push(1);
    elf.extend_from_slice(&[0u8; 9]);
    write_u16(&mut elf, 2);
    write_u16(&mut elf, 0x3e);
    write_u32(&mut elf, 1);

    let entry_addr = base_vaddr + (file_text_off as u64);
    write_u64(&mut elf, entry_addr);

    let phoff = 64u64;
    write_u64(&mut elf, phoff);
    write_u64(&mut elf, 0);
    write_u32(&mut elf, 0);
    write_u16(&mut elf, 64);
    write_u16(&mut elf, 56);
    write_u16(&mut elf, 1);
    write_u16(&mut elf, 0);
    write_u16(&mut elf, 0);
    write_u16(&mut elf, 0);

    write_u32(&mut elf, 1);

    write_u32(&mut elf, 7);

    write_u64(&mut elf, file_text_off as u64);

    write_u64(&mut elf, base_vaddr + (file_text_off as u64));

    write_u64(&mut elf, base_vaddr + (file_text_off as u64));

    let filesz = (text.len() + rodata.len()) as u64;
    write_u64(&mut elf, filesz);
    write_u64(&mut elf, filesz);

    write_u64(&mut elf, 0x1000);

    if elf.len() > file_text_off {
        return Err("ELF headers too large".into());
    }
    let mut file_bytes: Vec<u8> = elf;
    file_bytes.resize(file_text_off, 0);

    file_bytes.extend_from_slice(&text);
    file_bytes.extend_from_slice(&rodata);

    let exe_path = out_dir.join(proj_name);
    {
        use std::io::Write;
        let mut f =
            std::fs::File::create(&exe_path).map_err(|e| format!("failed to create exe: {}", e))?;
        f.write_all(&file_bytes)
            .map_err(|e| format!("failed to write exe: {}", e))?;
        f.sync_all()
            .map_err(|e| format!("failed to sync exe: {}", e))?;
    }

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&exe_path)
            .map_err(|e| format!("stat failed: {}", e))?
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&exe_path, perms)
            .map_err(|e| format!("set_permissions failed: {}", e))?;
    }

    Ok(exe_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::process::Command;
    use tempfile::tempdir;

    #[test]
    fn compile_simple_println_and_run() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"hello\")/unit\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = Command::new(&exe).output().expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "hello\n");
    }

    #[test]
    fn compile_println_with_placeholder_and_expr() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"sum {} {}\", 2+3, 4)/unit\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = Command::new(&exe).output().expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "sum 5 4\n");
    }

    #[test]
    fn compile_locals_and_arith() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new main/\n    let x = 2/i64\n    let y = 3/i64\n    println(\"sum {}\", x + y)/unit\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = Command::new(&exe).output().expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(String::from_utf8_lossy(&out.stdout), "sum 5\n");
    }

    #[test]
    fn compile_locals_simple() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new main/\n    let x = 42/i64\n    println(\"after {}\", x)/unit\n    println(\"both {} {}\", 42, x)/unit\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");

        let _ = std::fs::copy(&exe, std::path::Path::new("/tmp/shiden_test_exe"));

        let objdump = Command::new("objdump").arg("-d").arg(&exe).output().ok();
        if let Some(od) = objdump {
            println!("objdump stdout:\n{}", String::from_utf8_lossy(&od.stdout));
            println!("objdump stderr:\n{}", String::from_utf8_lossy(&od.stderr));
        }
        let out = Command::new(&exe).output().expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "after 42\nboth 42 42\n"
        );
    }

    #[test]
    fn compile_if_else_and_run() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new main/\n    if 1 == 1/\n        println(\"yes\")/unit\n    else/\n        println(\"no\")/unit\n    fn/\n    println(\"after\")/unit\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = Command::new(&exe).output().expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "yes\nafter\n");
    }

    #[test]
    fn compile_while_and_run() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new main/\n    let i = 0/i64\n    while i < 3/\n        println(\"{}\", i)/unit\n        i = i + 1/i64\n    fn/\n    println(\"done\")/unit\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = Command::new(&exe).output().expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "0\n1\n2\ndone\n");
    }

    #[test]
    fn compile_short_circuit_and() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"res {}\", 0 == 1 && (1 / 0))/unit\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = Command::new(&exe).output().expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "res 0\n");
    }

    #[test]
    fn compile_short_circuit_or() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"res {}\", 1 == 1 || (1 / 0))/unit\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = Command::new(&exe).output().expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "res 1\n");
    }

    #[test]
    fn compile_function_call_simple() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new add(a/i64, b/i64)/\n    return a + b/\nfn/\nfn new main/\n    println(\"{}\", add(2, 3))/unit\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");

        let _ = std::fs::copy(
            &exe,
            std::path::Path::new("/tmp/shiden_test_function_call_exe"),
        );
        println!("exe path: {}", exe.display());
        let out = Command::new(&exe).output().expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(String::from_utf8_lossy(&out.stdout), "5\n");
    }

    #[test]
    fn compile_function_call_debug() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new add(a/i64, b/i64)/\n    println(\"in add\")/unit\n    return a + b/\nfn/\nfn new main/\n    println(\"before\")/unit\n    let _ = add(2, 3)/unit\n    println(\"after\")/unit\n    println(\"exit_main\")/unit\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");

        let _ = std::fs::copy(
            &exe,
            std::path::Path::new("/tmp/shiden_test_function_call_debug_exe"),
        );
        let out = Command::new(&exe).output().expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "before\nin add\nafter\nexit_main\n"
        );
    }

    #[test]
    fn compile_function_call_many_args() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");

        fs::write(pd.join("src/main.sd"), "fn new add(a/i64, b/i64, c/i64, d/i64, e/i64, f/i64, g/i64, h/i64)/\n    println(\"args {} {} {} {} {} {} {} {}\", a, b, c, d, e, f, g, h)/unit\n    return a + b + c + d + e + f + g + h/\nfn/\nfn new main/\n    println(\"{}\", add(1,2,3,4,5,6,7,8))/unit\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");

        let _ = std::fs::copy(&exe, std::path::Path::new("/tmp/shiden_many_args_exe"));
        let out = Command::new(&exe).output().expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "args 1 2 3 4 5 6 7 8\n36\n"
        );
    }

    #[test]
    fn debug_array_run() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    let mut a = []/array\n    push(a, 'H')/unit\n    push(a, 'i')/unit\n    println(\"{} {}\", a[0], a[1])/unit\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        println!("exe path: {}", exe.display());

        let _ = std::fs::copy(&exe, std::path::Path::new("/tmp/shiden_debug_exe"));
        let objdump = Command::new("objdump").arg("-d").arg(&exe).output().ok();
        if let Some(od) = objdump {
            println!("objdump stdout:\n{}", String::from_utf8_lossy(&od.stdout));
            println!("objdump stderr:\n{}", String::from_utf8_lossy(&od.stderr));
        }

        let out = Command::new(&exe).output().expect("run exe");

        println!(
            "status: {:?}\nstdout: {}\nstderr: {}",
            out.status,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );

        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "72 105\n");
    }
}
