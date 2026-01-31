pub fn check_format(src: &str) -> Result<(), String> {
    let mut block_level: usize = 0;
    let mut stack: Vec<String> = Vec::new();

    for (i, raw_line) in src.lines().enumerate() {
        let line_no = i + 1;

        if raw_line.contains('\t') {
            return Err(format!(
                "line {}: tab characters are not allowed; use 4 spaces",
                line_no
            ));
        }

        let line = raw_line.trim_end();
        if line.is_empty() {
            continue;
        }

        let trimmed = line.trim_start();
        if trimmed == "fn/" {
        } else if trimmed.starts_with("fn ")
            || trimmed == "fn main/"
            || trimmed.starts_with("if ")
            || trimmed == "else/"
            || trimmed.starts_with("while ")
        {
            if !line.ends_with('/') {
                return Err(format!("line {}: block header must end with '/'", line_no));
            }
        } else {
            let allowed_type_suffixes = [
                "u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64", "f32", "f64", "str",
                "char", "bool", "array", "unit",
            ];
            if trimmed.starts_with("let ") || trimmed.starts_with("let\t") {
                let mut ok_terminator = false;
                for t in allowed_type_suffixes.iter() {
                    if line.ends_with(&format!("/{}", t)) {
                        ok_terminator = true;
                        break;
                    }
                }
                if !ok_terminator {
                    return Err(format!(
                        "line {}: 'let' statements must end with '/<type>' (e.g., '/unit' or '/str')",
                        line_no
                    ));
                }
            } else {
                let mut ok_terminator = false;
                if line.ends_with('/') {
                    ok_terminator = true;
                }
                for t in allowed_type_suffixes.iter() {
                    if line.ends_with(&format!("/{}", t)) {
                        ok_terminator = true;
                        break;
                    }
                }
                if !ok_terminator {
                    return Err(format!(
                        "line {}: statements must end with '/' or '/<type>' (e.g., '/unit' or '/str')",
                        line_no
                    ));
                }
            }
        }

        let leading_spaces = raw_line.chars().take_while(|c| *c == ' ').count();
        if raw_line.chars().any(|c| c == '\t') {
            return Err(format!(
                "line {}: tabs are not allowed in indentation",
                line_no
            ));
        }
        if leading_spaces % 4 != 0 {
            return Err(format!(
                "line {}: indentation must be a multiple of 4 spaces",
                line_no
            ));
        }

        let trimmed = line.trim_start();
        let is_closing = trimmed == "fn/";
        let expected_indent = if is_closing {
            if block_level == 0 {
                return Err(format!("line {}: unexpected 'fn/' at top level", line_no));
            }
            (block_level - 1) * 4
        } else {
            block_level * 4
        };
        if leading_spaces != expected_indent {
            return Err(format!(
                "line {}: unexpected indentation, expected {} spaces",
                line_no, expected_indent
            ));
        }

        if line.contains(';') {
            return Err(format!("line {}: semicolons are not allowed", line_no));
        }

        if find_unquoted(line, "//") {
            return Err(format!(
                "line {}: inline comments '//' are not allowed",
                line_no
            ));
        }

        if is_closing {
            stack.pop();
            block_level -= 1;
        } else if trimmed == "fn main/"
            || trimmed.starts_with("fn new ")
            || trimmed.starts_with("if ")
            || trimmed == "else/"
        {
            stack.push(
                trimmed
                    .split_whitespace()
                    .next()
                    .unwrap_or("block")
                    .to_string(),
            );
            block_level += 1;
        }
    }

    if block_level != 0 {
        return Err("unclosed block: missing 'fn/'".into());
    }

    Ok(())
}

fn find_unquoted(s: &str, pat: &str) -> bool {
    let mut in_quote = false;
    let mut quote_char = '\0';
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if in_quote {
            if c == quote_char {
                in_quote = false;
                quote_char = '\0';
            }
            continue;
        }
        if c == '"' || c == '\'' {
            in_quote = true;
            quote_char = c;
            continue;
        }

        if c == pat.chars().next().unwrap_or('\0') {
            let mut rest = String::new();
            rest.push(c);
            for _ in 1..pat.len() {
                if let Some(nc) = chars.peek() {
                    rest.push(*nc);
                    chars.next();
                } else {
                    break;
                }
            }
            if rest == pat {
                return true;
            }
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_stub() {
        let src = "fn main/\n    println(\"hi\")/unit\nfn/";
        if let Err(e) = check_format(src) {
            panic!("format error: {}", e);
        }
        assert!(check_format(src).is_ok());
    }

    #[test]
    fn rejects_tabs() {
        let src = "fn main/\n\tprintln(\"hi\")/unit\nfn/";
        assert!(check_format(src).is_err());
    }

    #[test]
    fn rejects_wrong_indent() {
        let src = "fn new main/\n  println(\"hi\")/unit\nfn/";
        assert!(check_format(src).is_err());
    }

    #[test]
    fn rejects_missing_slash() {
        let src = "fn main/\n    println(\"hi\")\nfn/";
        assert!(check_format(src).is_err());
    }

    #[test]
    fn accepts_type_terminator() {
        let src = "fn main/\n    let age = 20/u8\nfn/";
        assert!(check_format(src).is_ok());
    }

    #[test]
    fn rejects_semicolon() {
        let src = "fn main/\n    let x = 1/i64\n    println(\"{}\", x)/unit\nfn/";
        let mut bad = src.replace("/i64\n", ";\n");
        bad = bad.replace("/unit\n", ";\n");
        assert!(check_format(&bad).is_err());
    }

    #[test]
    fn rejects_inline_comment() {
        let src = "fn main/\n    println(\"hi\")// comment/\nfn/";
        assert!(check_format(src).is_err());
    }

    #[test]
    fn unclosed_block() {
        let src = "fn new main/\n    println(\"hi\")/unit";
        assert!(check_format(src).is_err());
    }

    #[test]
    fn nested_blocks_ok() {
        let src = "fn new outer/\n    fn new inner/\n        println(\"ok\")/unit\n    fn/\nfn/";
        assert!(check_format(src).is_ok());
    }
}
