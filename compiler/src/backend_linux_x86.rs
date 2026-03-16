use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use crate::target::Target;

fn read_ir_entries(ir_text: &str) -> Vec<String> {
    if let Some(pos) = ir_text.find("\"entries\"") {
        if let Some(lb) = ir_text[pos..].find('[') {
            let start = pos + lb + 1;
            if let Some(rb) = ir_text[start..].rfind(']') {
                let inner = &ir_text[start..start + rb];

                return inner
                    .lines()
                    .map(|l| l.trim().trim_end_matches(',').to_string())
                    .filter(|l| !l.is_empty())
                    .collect();
            }
        }
    }
    Vec::new()
}

fn extract_op(entry: &str) -> String {
    if let Some(p) = entry.find("\"op\"") {
        if let Some(col) = entry[p..].find(':') {
            let rest = &entry[p + col + 1..];

            if let Some(q1) = rest.find('"') {
                let after = &rest[q1 + 1..];
                if let Some(q2) = after.find('"') {
                    return after[..q2].to_string();
                }
            }
        }
    }
    String::new()
}

fn extract_field(entry: &str, key: &str) -> Option<String> {
    if let Some(p) = entry.find(&format!("\"{}\"", key)) {
        if let Some(col) = entry[p..].find(':') {
            let rest = &entry[p + col + 1..];
            if let Some(q1) = rest.find('"') {
                let after = &rest[q1 + 1..];
                if let Some(q2) = after.find('"') {
                    return Some(after[..q2].to_string());
                }
            }
        }
    }
    None
}

pub fn emit_backend(t: &Target, ir_path: &str, out_path: &str) -> Result<(), String> {
    let mut f = File::open(ir_path).map_err(|e| format!("failed to open ir {}: {}", ir_path, e))?;
    let mut s = String::new();
    f.read_to_string(&mut s)
        .map_err(|e| format!("failed to read ir: {}", e))?;

    let entries = read_ir_entries(&s);

    let inipath = format!("{}.target.ini", ir_path.trim_end_matches(".ir.json"));
    let mut meta_name: Option<String> = None;
    if Path::new(&inipath).exists() {
        if let Ok(mut f) = File::open(&inipath) {
            let mut s2 = String::new();
            let _ = f.read_to_string(&mut s2);
            let mut in_meta = false;
            for line in s2.lines() {
                let line = line.trim();
                if line.starts_with('[') && line.ends_with(']') {
                    in_meta = &line[1..line.len() - 1] == "meta";
                    continue;
                }
                if in_meta {
                    if let Some(eq) = line.find('=') {
                        let k = line[..eq].trim();
                        let v = line[eq + 1..].trim();
                        if k == "name" {
                            meta_name = Some(v.to_string());
                            break;
                        }
                    }
                }
            }
        }
    }

    let mut asm = String::new();
    asm.push_str(".text\n.global _start\n");

    let mut data = String::new();
    data.push_str(".section .rodata\n");

    let mut str_labels: Vec<(String, String, usize)> = Vec::new();
    let mut func_open = false;

    let mut globals: Vec<(String, String)> = Vec::new();
    let mut global_map: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut i = 0;
    while i < entries.len() {
        let e = &entries[i];
        let op = extract_op(&e);
        if op == "fn-start" {
            break;
        }
        if op == "let" {
            if let Some(name) = extract_field(&e, "name") {
                if let Some(rhs) = extract_field(&e, "rhs") {
                    globals.push((name.clone(), rhs.clone()));
                }
            }
        }
        i += 1;
    }

    fn intern_str_fn(str_labels: &mut Vec<(String, String, usize)>, lit: &str) -> String {
        for (s, l, len) in str_labels.iter() {
            if s == lit {
                return l.clone();
            }
        }
        let lab = format!(".LC{}", str_labels.len());

        let len = lit.len() + 1;
        str_labels.push((lit.to_string(), lab.clone(), len));
        lab
    }

    fn resolve_symbol_fn(
        global_map: &std::collections::HashMap<String, String>,
        str_labels: &mut Vec<(String, String, usize)>,
        ident: &str,
    ) -> String {
        if let Some(l) = global_map.get(ident) {
            return l.clone();
        }
        intern_str_fn(str_labels, ident)
    }

    let mut funcs: Vec<String> = Vec::new();

    let mut idx = 0usize;
    while idx < entries.len() {
        let e = &entries[idx];
        let op = extract_op(&e);
        match op.as_str() {
            "fn-start" => {
                if let Some(name) = extract_field(&e, "name") {
                    funcs.push(name.clone());

                    let mut locals: Vec<String> = Vec::new();
                    let mut j = idx + 1;
                    while j < entries.len() {
                        let ej = &entries[j];
                        let opj = extract_op(&ej);
                        if opj == "fn-end" {
                            break;
                        }
                        if opj == "let" {
                            if let Some(ln) = extract_field(&ej, "name") {
                                locals.push(ln);
                            }
                        }
                        j += 1;
                    }

                    asm.push_str(&format!(".global {}\n{}:\n    push rbp\n    mov rbp, rsp\n    push rbx\n    push r12\n    push r13\n    push r14\n    push r15\n", name, name));
                    let stack_size_raw = (locals.len() as i32) * 8;

                    let stack_size = if stack_size_raw == 0 {
                        0
                    } else {
                        ((stack_size_raw + 15) / 16) * 16
                    };
                    if stack_size > 0 {
                        asm.push_str(&format!("    sub rsp, {}\n", stack_size));
                    }
                    func_open = true;

                    let mut local_offsets: std::collections::HashMap<String, i32> =
                        std::collections::HashMap::new();
                    for (k, nm) in locals.iter().enumerate() {
                        local_offsets.insert(nm.clone(), ((k as i32) + 1) * 8);
                    }

                    let mut k = idx + 1;
                    while k < entries.len() {
                        let ek = &entries[k];
                        let opk = extract_op(&ek);
                        if opk == "fn-end" {
                            if stack_size > 0 {
                                asm.push_str(&format!("    add rsp, {}\n", stack_size));
                            }
                            asm.push_str("    pop r15\n    pop r14\n    pop r13\n    pop r12\n    pop rbx\n    mov rsp, rbp\n    pop rbp\n    ret\n");
                            func_open = false;
                            break;
                        }
                        match opk.as_str() {
                            "let" => {
                                if let Some(name) = extract_field(&ek, "name") {
                                    if let Some(rhs) = extract_field(&ek, "rhs") {
                                        if rhs.starts_with("call ") {
                                            let parts: Vec<&str> = rhs.split_whitespace().collect();
                                            if parts.len() >= 2 {
                                                let fname = parts[1];
                                                let regs =
                                                    vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                                                for (ai, aop) in parts.iter().skip(2).enumerate() {
                                                    if ai < regs.len() {
                                                        let reg = regs[ai];
                                                        let sop = *aop;
                                                        if sop.starts_with('"')
                                                            && sop.ends_with('"')
                                                        {
                                                            let lab = intern_str_fn(
                                                                &mut str_labels,
                                                                sop.trim_matches('"'),
                                                            );
                                                            asm.push_str(&format!(
                                                                "    lea {}, [rel {}]\n",
                                                                reg, lab
                                                            ));
                                                        } else if let Ok(num) = sop.parse::<i64>() {
                                                            asm.push_str(&format!(
                                                                "    mov {}, {}\n",
                                                                reg, num
                                                            ));
                                                        } else if let Some(off) =
                                                            local_offsets.get(sop)
                                                        {
                                                            asm.push_str(&format!("    mov {}, qword ptr [rbp - {}]\n", reg, off));
                                                        } else {
                                                            let lab = resolve_symbol_fn(
                                                                &global_map,
                                                                &mut str_labels,
                                                                sop,
                                                            );
                                                            asm.push_str(&format!(
                                                                "    lea {}, [rel {}]\n",
                                                                reg, lab
                                                            ));
                                                        }
                                                    }
                                                }
                                                asm.push_str(&format!("    call {}\n", fname));
                                                if let Some(off) = local_offsets.get(&name) {
                                                    asm.push_str(&format!(
                                                        "    mov qword ptr [rbp - {}], rax\n",
                                                        off
                                                    ));
                                                }
                                            }
                                        } else if rhs.starts_with('"') && rhs.ends_with('"') {
                                            let lit = rhs.trim_matches('"');
                                            let lab = intern_str_fn(&mut str_labels, lit);
                                            if let Some(off) = local_offsets.get(&name) {
                                                asm.push_str(&format!("    lea rax, [rel {}]\n    mov qword ptr [rbp - {}], rax\n", lab, off));
                                            }
                                        } else if let Ok(num) = rhs.parse::<i64>() {
                                            if let Some(off) = local_offsets.get(&name) {
                                                asm.push_str(&format!(
                                                    "    mov qword ptr [rbp - {}], {}\n",
                                                    off, num
                                                ));
                                            }
                                        } else {
                                            if let Some(off_src) = local_offsets.get(&rhs) {
                                                if let Some(off_dst) = local_offsets.get(&name) {
                                                    asm.push_str(&format!("    mov rax, qword ptr [rbp - {}]\n    mov qword ptr [rbp - {}], rax\n", off_src, off_dst));
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            "instr" => {
                                if let Some(opcode) = extract_field(&ek, "opcode") {
                                    let opcode = opcode;
                                    let mut ops: Vec<String> = Vec::new();
                                    if let Some(operands) = extract_field(&ek, "operands") {
                                        for p in operands.split_whitespace() {
                                            ops.push(p.to_string());
                                        }
                                    }
                                    match opcode.as_str() {
                                        "println" => {
                                            if !ops.is_empty() {
                                                let outop = &ops[0];
                                                if outop.starts_with('"') && outop.ends_with('"') {
                                                    let lit = outop.trim_matches('"');
                                                    let lab = intern_str_fn(&mut str_labels, lit);

                                                    let len = str_labels
                                                        .iter()
                                                        .find(|(_, l, _)| l == &lab)
                                                        .map(|(_, _, len)| *len)
                                                        .unwrap_or(lit.len() + 1);
                                                    asm.push_str(&format!("    lea rsi, [rel {}]\n    mov rdx, {}\n    mov rdi, 1\n    mov rax, 1\n    syscall\n", lab, len));
                                                } else if let Some(off) = local_offsets.get(outop) {
                                                    asm.push_str(&format!("    mov rsi, qword ptr [rbp - {}]\n    mov rdi, 1\n    mov rax, 1\n    syscall\n", off));
                                                } else {
                                                    let lab = resolve_symbol_fn(
                                                        &global_map,
                                                        &mut str_labels,
                                                        outop,
                                                    );
                                                    let len = str_labels
                                                        .iter()
                                                        .find(|(_, l, _)| l == &lab)
                                                        .map(|(_, _, len)| *len)
                                                        .unwrap_or(0);
                                                    asm.push_str(&format!("    lea rsi, [rel {}]\n    mov rdx, {}\n    mov rdi, 1\n    mov rax, 1\n    syscall\n", lab, len));
                                                }
                                            }
                                        }
                                        "mov" => {
                                            if ops.len() >= 2 {
                                                let dst = &ops[0];
                                                let src = &ops[1];
                                                if let Some(off_dst) = local_offsets.get(dst) {
                                                    if src.starts_with('"') && src.ends_with('"') {
                                                        let lab = intern_str_fn(
                                                            &mut str_labels,
                                                            src.trim_matches('"'),
                                                        );
                                                        asm.push_str(&format!("    lea rax, [rel {}]\n    mov qword ptr [rbp - {}], rax\n", lab, off_dst));
                                                    } else if let Ok(num) = src.parse::<i64>() {
                                                        asm.push_str(&format!(
                                                            "    mov qword ptr [rbp - {}], {}\n",
                                                            off_dst, num
                                                        ));
                                                    } else if let Some(off_src) =
                                                        local_offsets.get(src)
                                                    {
                                                        asm.push_str(&format!("    mov rax, qword ptr [rbp - {}]\n    mov qword ptr [rbp - {}], rax\n", off_src, off_dst));
                                                    } else {
                                                        let lab = resolve_symbol_fn(
                                                            &global_map,
                                                            &mut str_labels,
                                                            src,
                                                        );
                                                        asm.push_str(&format!("    lea rax, [rel {}]\n    mov qword ptr [rbp - {}], rax\n", lab, off_dst));
                                                    }
                                                }
                                            }
                                        }
                                        "add" | "sub" => {
                                            if ops.len() >= 3 {
                                                let dst = &ops[0];
                                                let a = &ops[1];
                                                let b = &ops[2];
                                                let mut load_op = |op: &str| -> String {
                                                    if let Ok(num) = op.parse::<i64>() {
                                                        format!("{}", num)
                                                    } else if let Some(off) = local_offsets.get(op)
                                                    {
                                                        format!("[rbp - {}]", off)
                                                    } else {
                                                        op.to_string()
                                                    }
                                                };
                                                if let Some(off_dst) = local_offsets.get(dst) {
                                                    if let Ok(num) = a.parse::<i64>() {
                                                        asm.push_str(&format!(
                                                            "    mov rax, {}\n",
                                                            num
                                                        ));
                                                    } else if let Some(off_a) = local_offsets.get(a)
                                                    {
                                                        asm.push_str(&format!(
                                                            "    mov rax, qword ptr [rbp - {}]\n",
                                                            off_a
                                                        ));
                                                    }
                                                    if let Ok(numb) = b.parse::<i64>() {
                                                        if opcode == "add" {
                                                            asm.push_str(&format!(
                                                                "    add rax, {}\n",
                                                                numb
                                                            ));
                                                        } else {
                                                            asm.push_str(&format!(
                                                                "    sub rax, {}\n",
                                                                numb
                                                            ));
                                                        }
                                                    } else if let Some(off_b) = local_offsets.get(b)
                                                    {
                                                        if opcode == "add" {
                                                            asm.push_str(&format!("    add rax, qword ptr [rbp - {}]\n", off_b));
                                                        } else {
                                                            asm.push_str(&format!("    sub rax, qword ptr [rbp - {}]\n", off_b));
                                                        }
                                                    }
                                                    asm.push_str(&format!(
                                                        "    mov qword ptr [rbp - {}], rax\n",
                                                        off_dst
                                                    ));
                                                }
                                            }
                                        }
                                        _ => {
                                            asm.push_str(&format!(
                                                "    # unhandled instr {}\n",
                                                opcode
                                            ));
                                        }
                                    }
                                }
                            }
                            "call" => {
                                if let Some(callv) = extract_field(&ek, "call") {
                                    let parts: Vec<&str> = callv.split_whitespace().collect();
                                    if !parts.is_empty() {
                                        let fname = parts[0];
                                        let regs = vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                                        for (ai, aop) in parts.iter().skip(1).enumerate() {
                                            if ai < regs.len() {
                                                let reg = regs[ai];
                                                let sop = *aop;
                                                if sop.starts_with('"') && sop.ends_with('"') {
                                                    let lab = intern_str_fn(
                                                        &mut str_labels,
                                                        sop.trim_matches('"'),
                                                    );
                                                    asm.push_str(&format!(
                                                        "    lea {}, [rel {}]\n",
                                                        reg, lab
                                                    ));
                                                } else if let Ok(num) = sop.parse::<i64>() {
                                                    asm.push_str(&format!(
                                                        "    mov {}, {}\n",
                                                        reg, num
                                                    ));
                                                } else if let Some(off) = local_offsets.get(sop) {
                                                    asm.push_str(&format!(
                                                        "    mov {}, qword ptr [rbp - {}]\n",
                                                        reg, off
                                                    ));
                                                } else {
                                                    let lab = resolve_symbol_fn(
                                                        &global_map,
                                                        &mut str_labels,
                                                        sop,
                                                    );
                                                    asm.push_str(&format!(
                                                        "    lea {}, [rel {}]\n",
                                                        reg, lab
                                                    ));
                                                }
                                            }
                                        }
                                        asm.push_str(&format!("    call {}\n", fname));
                                    }
                                }
                            }
                            "ret" => {
                                asm.push_str("    ret\n");
                            }
                            _ => {}
                        }
                        k += 1;
                    }

                    idx = k;
                }
            }
            "fn-end" => {
                if func_open {
                    asm.push_str("    mov rsp, rbp\n    pop rbp\n    ret\n");
                    func_open = false;
                }
            }
            "instr" => {
                if let Some(opcode) = extract_field(&e, "opcode") {
                    if opcode == "println" {
                        if let Some(operands) = extract_field(&e, "operands") {
                            let mut outop = operands.clone();
                            if outop.starts_with('"') && outop.ends_with('"') {
                                let lit = outop.trim_matches('"');
                                let lab = intern_str_fn(&mut str_labels, lit);
                                asm.push_str(&format!(
                                    "    lea rdi, [rel {}]\n    call puts\n",
                                    lab
                                ));
                            } else {
                                let lab = intern_str_fn(&mut str_labels, &outop);
                                asm.push_str(&format!(
                                    "    lea rdi, [rel {}]\n    call puts\n",
                                    lab
                                ));
                            }
                        }
                    } else {
                        asm.push_str(&format!("    # unhandled instr {}\n", opcode));
                    }
                }
            }
            "call" => {
                if let Some(callv) = extract_field(&e, "call") {
                    let parts: Vec<&str> = callv.split_whitespace().collect();
                    if !parts.is_empty() {
                        let fname = parts[0];
                        asm.push_str(&format!("    call {}\n", fname));
                    }
                }
            }
            "let" => {
                asm.push_str("    # let ignored in backend\n");
            }
            "ret" => {
                asm.push_str("    ret\n");
            }
            _ => {}
        }
    }

    asm.push_str("_start:\n");
    if funcs.contains(&"main".to_string()) {
        asm.push_str("    call main\n    mov rdi, rax\n    mov rax, 60\n    syscall\n");
    } else {
        asm.push_str("    mov rax, 60\n    xor rdi, rdi\n    syscall\n");
    }

    for (lit, lab, len) in &str_labels {
        data.push_str(&format!(
            "{}:\n    .string \"{}\\n\"\n",
            lab,
            lit.replace("\"", "\\\"")
        ));

        let llen = *len;
        data.push_str(&format!("{}__len:\n    .quad {}\n", lab, llen));
    }

    let mut prolog = String::new();
    if !globals.is_empty() {
        prolog.push_str(".data\n");
        for (name, rhs) in &globals {
            let sym = format!("global_{}", name);
            global_map.insert(name.clone(), sym.clone());
            if rhs.starts_with('"') && rhs.ends_with('"') {
                let lit = rhs.trim_matches('"');

                let lab = intern_str_fn(&mut str_labels, lit);
                prolog.push_str(&format!("{}:\n    .quad {}\n", sym, lab));
            } else if let Ok(num) = rhs.parse::<i64>() {
                prolog.push_str(&format!("{}:\n    .quad {}\n", sym, num));
            } else {
                let lab = if let Some(existing) = global_map.get(rhs) {
                    existing.clone()
                } else {
                    intern_str_fn(&mut str_labels, rhs)
                };
                prolog.push_str(&format!("{}:\n    .quad {}\n", sym, lab));
            }
        }
    }
    if !str_labels.is_empty() {
        prolog.push_str(".section .rodata\n");
        prolog.push_str(".extern puts\n");
    }

    let asm_path = format!("{}.asm.s", out_path);
    let mut af = File::create(&asm_path)
        .map_err(|e| format!("failed to create asm file {}: {}", asm_path, e))?;
    af.write_all(prolog.as_bytes())
        .map_err(|e| format!("failed to write asm: {}", e))?;
    af.write_all(asm.as_bytes())
        .map_err(|e| format!("failed to write asm: {}", e))?;
    if !str_labels.is_empty() {
        af.write_all(data.as_bytes())
            .map_err(|e| format!("failed to write asm data: {}", e))?;
    }

    let obj_path = format!("{}.o", out_path);
    let as_status = std::process::Command::new("as")
        .arg(&asm_path)
        .arg("-o")
        .arg(&obj_path)
        .status();
    match as_status {
        Ok(s) if s.success() => (),
        Ok(s) => return Err(format!("as failed with status: {}", s)),
        Err(e) => return Err(format!("failed to run as: {}", e)),
    }

    let ld_status = std::process::Command::new("ld")
        .arg(&obj_path)
        .arg("-o")
        .arg(out_path)
        .status();
    match ld_status {
        Ok(s) if s.success() => Ok(()),
        Ok(s) => Err(format!("ld failed with status: {}", s)),
        Err(e) => Err(format!("failed to run ld: {}", e)),
    }
}
