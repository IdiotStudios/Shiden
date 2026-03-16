#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Number(String),
    Str(String),
    Slash,
    LParen,
    RParen,
    Comma,
    Eof,
}

pub fn tokenize(src: &str) -> Vec<Token> {
    let mut out = Vec::new();
    let mut chars = src.chars().peekable();
    while let Some(&c) = chars.peek() {
        match c {
            x if x.is_whitespace() => {
                chars.next();
            }
            '"' => {
                chars.next();
                let mut s = String::new();
                while let Some(ch) = chars.next() {
                    if ch == '"' {
                        break;
                    }
                    if ch == '\\' {
                        if let Some(esc) = chars.next() {
                            s.push(esc);
                        }
                    } else {
                        s.push(ch);
                    }
                }
                out.push(Token::Str(s));
            }
            '/' => {
                chars.next();
                out.push(Token::Slash);
            }
            '(' => {
                chars.next();
                out.push(Token::LParen);
            }
            ')' => {
                chars.next();
                out.push(Token::RParen);
            }
            ',' => {
                chars.next();
                out.push(Token::Comma);
            }
            x if x.is_ascii_digit() => {
                let mut n = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_ascii_digit() {
                        n.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
                out.push(Token::Number(n));
            }
            _ => {
                let mut id = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_whitespace() || ch == '/' || ch == '(' || ch == ')' || ch == ',' {
                        break;
                    }
                    id.push(ch);
                    chars.next();
                }
                out.push(Token::Ident(id));
            }
        }
    }
    out.push(Token::Eof);
    out
}
