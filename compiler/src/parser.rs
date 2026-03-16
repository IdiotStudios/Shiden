use crate::ir_types::IrNode;
use crate::lexer::{Token, tokenize};
use std::fs;
use std::path::Path;

fn parse_src_with_base(src: &str, base_dir: Option<&str>) -> Vec<IrNode> {
    let toks = tokenize(src);
    let mut i = 0usize;
    let mut out: Vec<IrNode> = Vec::new();

    loop {
        let t = toks.get(i).cloned().unwrap_or(Token::Eof);
        match t {
            Token::Ident(s) if s == "import" => {
                i += 1;
                if let Token::Str(p) = toks.get(i).cloned().unwrap_or(Token::Eof) {
                    i += 1;
                    if matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash) {
                        i += 1;
                    }

                    let path_resolved = if Path::new(&p).is_absolute() {
                        p.clone()
                    } else if let Some(bd) = base_dir {
                        format!("{}/{}", bd.trim_end_matches('/'), p)
                    } else {
                        p.clone()
                    };
                    match fs::read_to_string(&path_resolved) {
                        Ok(src2) => {
                            let mut nested = parse_src_with_base(
                                &src2,
                                Path::new(&path_resolved)
                                    .parent()
                                    .map(|p| p.to_str().unwrap_or(".")),
                            );
                            out.append(&mut nested);
                        }
                        Err(e) => {
                            i += 0;
                            let _ = e;
                        }
                    }
                } else {
                    i += 1;
                }
            }
            Token::Ident(s) if s == "fn" => {
                i += 1;

                let k = toks.get(i).cloned().unwrap_or(Token::Eof);
                if let Token::Ident(kname) = k {
                    i += 1;
                    let name = if kname == "new" {
                        let n = toks.get(i).cloned().unwrap_or(Token::Eof);
                        if let Token::Ident(nname) = n {
                            i += 1;
                            nname
                        } else {
                            "<anon>".to_string()
                        }
                    } else {
                        kname
                    };

                    let mut args = Vec::new();
                    if matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::LParen) {
                        i += 1;
                        while !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::RParen)
                            && !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Eof)
                        {
                            match toks.get(i).cloned().unwrap_or(Token::Eof) {
                                Token::Ident(a) => {
                                    args.push(a);
                                    i += 1;
                                }
                                Token::Comma => {
                                    i += 1;
                                }
                                _ => {
                                    i += 1;
                                }
                            }
                        }
                        if matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::RParen) {
                            i += 1;
                        }
                    }

                    while !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash) {
                        i += 1;
                        if i >= toks.len() {
                            break;
                        }
                    }
                    if i < toks.len()
                        && matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash)
                    {
                        i += 1;
                    }
                    out.push(IrNode::FnStart {
                        name: name.clone(),
                        args: args.clone(),
                    });

                    loop {
                        if i >= toks.len() {
                            break;
                        }
                        let tt = toks.get(i).cloned().unwrap_or(Token::Eof);
                        if let Token::Ident(ref s) = tt {
                            if s == "fn" {
                                if toks.get(i + 1).cloned().unwrap_or(Token::Eof) == Token::Slash {
                                    i += 2;
                                    out.push(IrNode::FnEnd);
                                    break;
                                }
                            }
                        }

                        match tt {
                            Token::Ident(ref inner_s) if inner_s == "let" => {
                                i += 1;
                                let mut name = "".to_string();
                                if let Token::Ident(n) = toks.get(i).cloned().unwrap_or(Token::Eof)
                                {
                                    name = n;
                                    i += 1;
                                }
                                if let Token::Ident(eq) = toks.get(i).cloned().unwrap_or(Token::Eof)
                                {
                                    if eq == "=" {
                                        i += 1;
                                    }
                                }
                                let mut rhs = Vec::new();
                                while !matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Slash
                                ) && !matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Eof
                                ) {
                                    match toks.get(i).cloned().unwrap_or(Token::Eof) {
                                        Token::Ident(x) => {
                                            rhs.push(x);
                                            i += 1;
                                        }
                                        Token::Number(x) => {
                                            rhs.push(x);
                                            i += 1;
                                        }
                                        Token::Str(x) => {
                                            rhs.push(format!("\"{}\"", x));
                                            i += 1;
                                        }
                                        Token::Comma => {
                                            rhs.push(",".to_string());
                                            i += 1;
                                        }
                                        other => {
                                            let _ = other;
                                            i += 1;
                                        }
                                    }
                                }
                                if matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Slash
                                ) {
                                    i += 1;
                                }
                                let rhs_str = rhs.join(" ");
                                if rhs_str.starts_with("call ") {
                                    out.push(IrNode::Call {
                                        call: rhs_str.trim_start_matches("call ").to_string(),
                                    });
                                    out.push(IrNode::Let {
                                        name,
                                        rhs: "_retval".to_string(),
                                    });
                                } else {
                                    out.push(IrNode::Let { name, rhs: rhs_str });
                                }
                            }
                            Token::Ident(ref inner_s) if inner_s == "call" => {
                                i += 1;
                                let mut parts = Vec::new();
                                while !matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Slash
                                ) && !matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Eof
                                ) {
                                    match toks.get(i).cloned().unwrap_or(Token::Eof) {
                                        Token::Ident(x) => {
                                            parts.push(x);
                                            i += 1;
                                        }
                                        Token::Number(x) => {
                                            parts.push(x);
                                            i += 1;
                                        }
                                        Token::Str(x) => {
                                            parts.push(format!("\"{}\"", x));
                                            i += 1;
                                        }
                                        Token::Comma => {
                                            parts.push(",".to_string());
                                            i += 1;
                                        }
                                        Token::LParen => {
                                            i += 1;
                                            let mut inner = Vec::new();
                                            while !matches!(
                                                toks.get(i).cloned().unwrap_or(Token::Eof),
                                                Token::RParen
                                            ) && !matches!(
                                                toks.get(i).cloned().unwrap_or(Token::Eof),
                                                Token::Eof
                                            ) {
                                                match toks.get(i).cloned().unwrap_or(Token::Eof) {
                                                    Token::Ident(a) => {
                                                        inner.push(a);
                                                        i += 1;
                                                    }
                                                    Token::Number(a) => {
                                                        inner.push(a);
                                                        i += 1;
                                                    }
                                                    Token::Str(a) => {
                                                        inner.push(format!("\"{}\"", a));
                                                        i += 1;
                                                    }
                                                    Token::Comma => {
                                                        i += 1;
                                                    }
                                                    _ => {
                                                        i += 1;
                                                    }
                                                }
                                            }
                                            if matches!(
                                                toks.get(i).cloned().unwrap_or(Token::Eof),
                                                Token::RParen
                                            ) {
                                                i += 1;
                                            }
                                            parts.push(inner.join(" "));
                                        }
                                        Token::RParen => {
                                            i += 1;
                                        }
                                        _ => {
                                            i += 1;
                                        }
                                    }
                                }
                                if matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Slash
                                ) {
                                    i += 1;
                                }
                                out.push(IrNode::Call {
                                    call: parts.join(" "),
                                });
                            }
                            Token::Ident(ref inner_s) if inner_s == "ret" => {
                                i += 1;
                                let mut rhs = Vec::new();
                                while !matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Slash
                                ) && !matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Eof
                                ) {
                                    match toks.get(i).cloned().unwrap_or(Token::Eof) {
                                        Token::Ident(x) => {
                                            rhs.push(x);
                                            i += 1;
                                        }
                                        Token::Number(x) => {
                                            rhs.push(x);
                                            i += 1;
                                        }
                                        Token::Str(x) => {
                                            rhs.push(format!("\"{}\"", x));
                                            i += 1;
                                        }
                                        Token::Comma => {
                                            rhs.push(",".to_string());
                                            i += 1;
                                        }
                                        _ => {
                                            i += 1;
                                        }
                                    }
                                }
                                if matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Slash
                                ) {
                                    i += 1;
                                }
                                let rv = rhs.join(" ");
                                if rv.is_empty() {
                                    out.push(IrNode::Ret { value: None });
                                } else {
                                    out.push(IrNode::Ret { value: Some(rv) });
                                }
                            }
                            Token::Ident(_) => {
                                let tt2 = toks.get(i).cloned().unwrap_or(Token::Eof);
                                if let Token::Ident(s2) = tt2 {
                                    if s2.ends_with(':') {
                                        let name = s2.trim_end_matches(':').to_string();
                                        i += 1;
                                        out.push(IrNode::Label { name });
                                    } else {
                                        let opcode = s2;
                                        i += 1;
                                        let mut operands = Vec::new();
                                        while !matches!(
                                            toks.get(i).cloned().unwrap_or(Token::Eof),
                                            Token::Slash
                                        ) && !matches!(
                                            toks.get(i).cloned().unwrap_or(Token::Eof),
                                            Token::Eof
                                        ) {
                                            match toks.get(i).cloned().unwrap_or(Token::Eof) {
                                                Token::Ident(x) => {
                                                    operands.push(x);
                                                    i += 1;
                                                }
                                                Token::Number(x) => {
                                                    operands.push(x);
                                                    i += 1;
                                                }
                                                Token::Str(x) => {
                                                    operands.push(format!("\"{}\"", x));
                                                    i += 1;
                                                }
                                                Token::Comma => {
                                                    i += 1;
                                                }
                                                Token::LParen => {
                                                    i += 1;
                                                    let mut inner = Vec::new();
                                                    while !matches!(
                                                        toks.get(i).cloned().unwrap_or(Token::Eof),
                                                        Token::RParen
                                                    ) && !matches!(
                                                        toks.get(i).cloned().unwrap_or(Token::Eof),
                                                        Token::Eof
                                                    ) {
                                                        match toks
                                                            .get(i)
                                                            .cloned()
                                                            .unwrap_or(Token::Eof)
                                                        {
                                                            Token::Ident(a) => {
                                                                inner.push(a);
                                                                i += 1;
                                                            }
                                                            Token::Number(a) => {
                                                                inner.push(a);
                                                                i += 1;
                                                            }
                                                            Token::Str(a) => {
                                                                inner.push(format!("\"{}\"", a));
                                                                i += 1;
                                                            }
                                                            Token::Comma => {
                                                                i += 1;
                                                            }
                                                            _ => {
                                                                i += 1;
                                                            }
                                                        }
                                                    }
                                                    if matches!(
                                                        toks.get(i).cloned().unwrap_or(Token::Eof),
                                                        Token::RParen
                                                    ) {
                                                        i += 1;
                                                    }
                                                    operands.push(inner.join(" "));
                                                }
                                                Token::RParen => {
                                                    i += 1;
                                                }
                                                _ => {
                                                    i += 1;
                                                }
                                            }
                                        }
                                        if matches!(
                                            toks.get(i).cloned().unwrap_or(Token::Eof),
                                            Token::Slash
                                        ) {
                                            i += 1;
                                        }
                                        out.push(IrNode::Instr { opcode, operands });
                                    }
                                } else {
                                    i += 1;
                                }
                            }
                            Token::Slash => {
                                i += 1;
                            }
                            Token::Eof => {
                                break;
                            }
                            _ => {
                                i += 1;
                            }
                        }
                    }
                } else {
                    i += 1;
                }
            }
            Token::Ident(s) if s == "let" => {
                i += 1;
                let mut name = "".to_string();
                if let Token::Ident(n) = toks.get(i).cloned().unwrap_or(Token::Eof) {
                    name = n;
                    i += 1;
                }

                if let Token::Ident(eq) = toks.get(i).cloned().unwrap_or(Token::Eof) {
                    if eq == "=" {
                        i += 1;
                    }
                }
                let mut rhs = Vec::new();
                while !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash)
                    && !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Eof)
                {
                    match toks.get(i).cloned().unwrap_or(Token::Eof) {
                        Token::Ident(x) => {
                            rhs.push(x);
                            i += 1;
                        }
                        Token::Number(x) => {
                            rhs.push(x);
                            i += 1;
                        }
                        Token::Str(x) => {
                            rhs.push(format!("\"{}\"", x));
                            i += 1;
                        }
                        Token::Comma => {
                            rhs.push(",".to_string());
                            i += 1;
                        }
                        other => {
                            let _ = other;
                            i += 1;
                        }
                    }
                }
                if matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash) {
                    i += 1;
                }
                let rhs_str = rhs.join(" ");
                if rhs_str.starts_with("call ") {
                    out.push(IrNode::Call {
                        call: rhs_str.trim_start_matches("call ").to_string(),
                    });
                    out.push(IrNode::Let {
                        name,
                        rhs: "_retval".to_string(),
                    });
                } else {
                    out.push(IrNode::Let { name, rhs: rhs_str });
                }
            }
            Token::Ident(s) if s == "call" => {
                i += 1;
                let mut parts = Vec::new();
                while !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash)
                    && !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Eof)
                {
                    match toks.get(i).cloned().unwrap_or(Token::Eof) {
                        Token::Ident(x) => {
                            parts.push(x);
                            i += 1;
                        }
                        Token::Number(x) => {
                            parts.push(x);
                            i += 1;
                        }
                        Token::Str(x) => {
                            parts.push(format!("\"{}\"", x));
                            i += 1;
                        }
                        Token::Comma => {
                            parts.push(",".to_string());
                            i += 1;
                        }
                        Token::LParen => {
                            parts.push("(".to_string());
                            i += 1;
                        }
                        Token::RParen => {
                            parts.push(")".to_string());
                            i += 1;
                        }
                        _ => {
                            i += 1;
                        }
                    }
                }
                if matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash) {
                    i += 1;
                }
                out.push(IrNode::Call {
                    call: parts.join(" "),
                });
            }
            Token::Ident(s) if s == "ret" => {
                i += 1;
                let mut rhs = Vec::new();
                while !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash)
                    && !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Eof)
                {
                    match toks.get(i).cloned().unwrap_or(Token::Eof) {
                        Token::Ident(x) => {
                            rhs.push(x);
                            i += 1;
                        }
                        Token::Number(x) => {
                            rhs.push(x);
                            i += 1;
                        }
                        Token::Str(x) => {
                            rhs.push(format!("\"{}\"", x));
                            i += 1;
                        }
                        Token::Comma => {
                            rhs.push(",".to_string());
                            i += 1;
                        }
                        _ => {
                            i += 1;
                        }
                    }
                }
                if matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash) {
                    i += 1;
                }
                let rv = rhs.join(" ");
                if rv.is_empty() {
                    out.push(IrNode::Ret { value: None });
                } else {
                    out.push(IrNode::Ret { value: Some(rv) });
                }
            }
            Token::Ident(s) => {
                if s.ends_with(':') {
                    let name = s.trim_end_matches(':').to_string();
                    i += 1;
                    out.push(IrNode::Label { name });
                } else {
                    let opcode = s;
                    i += 1;
                    let mut operands = Vec::new();
                    while !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash)
                        && !matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Eof)
                    {
                        match toks.get(i).cloned().unwrap_or(Token::Eof) {
                            Token::Ident(x) => {
                                operands.push(x);
                                i += 1;
                            }
                            Token::Number(x) => {
                                operands.push(x);
                                i += 1;
                            }
                            Token::Str(x) => {
                                operands.push(format!("\"{}\"", x));
                                i += 1;
                            }
                            Token::Comma => {
                                i += 1;
                            }
                            Token::LParen => {
                                i += 1;
                                let mut inner = Vec::new();
                                while !matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::RParen
                                ) && !matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::Eof
                                ) {
                                    match toks.get(i).cloned().unwrap_or(Token::Eof) {
                                        Token::Ident(a) => {
                                            inner.push(a);
                                            i += 1;
                                        }
                                        Token::Number(a) => {
                                            inner.push(a);
                                            i += 1;
                                        }
                                        Token::Str(a) => {
                                            inner.push(format!("\"{}\"", a));
                                            i += 1;
                                        }
                                        Token::Comma => {
                                            i += 1;
                                        }
                                        _ => {
                                            i += 1;
                                        }
                                    }
                                }
                                if matches!(
                                    toks.get(i).cloned().unwrap_or(Token::Eof),
                                    Token::RParen
                                ) {
                                    i += 1;
                                }
                                operands.push(inner.join(" "));
                            }
                            Token::RParen => {
                                i += 1;
                            }
                            _ => {
                                i += 1;
                            }
                        }
                    }
                    if matches!(toks.get(i).cloned().unwrap_or(Token::Eof), Token::Slash) {
                        i += 1;
                    }
                    out.push(IrNode::Instr { opcode, operands });
                }
            }
            Token::Slash => {
                i += 1;
            }
            Token::Eof => {
                break;
            }
            _ => {
                i += 1;
            }
        }
        if i >= toks.len() {
            break;
        }
    }

    out
}

pub fn parse_file(path: &str) -> Vec<IrNode> {
    match fs::read_to_string(path) {
        Ok(src) => {
            let base = Path::new(path).parent().map(|p| p.to_str().unwrap_or("."));
            parse_src_with_base(&src, base)
        }
        Err(_) => Vec::new(),
    }
}
