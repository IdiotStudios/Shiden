#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Fn,
    New,
    Let,
    Mut,
    Return,
    If,
    While,
    Else,
    Break,
    Continue,
    For,
    In,
    Identifier(String),
    StringLiteral(String),
    CharLiteral(char),
    Number(String),
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Equal,
    EqEq,
    NotEq,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    SlashOp,
    Slash,
    Type(String),
    AndAnd,
    OrOr,
    Bang,
    Eof,
}

pub struct Lexer<'a> {
    src: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { src, pos: 0 }
    }

    fn peek_char(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    fn bump(&mut self, n: usize) {
        for _ in 0..n {
            if let Some(ch) = self.peek_char() {
                self.pos += ch.len_utf8();
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.bump(1);
                continue;
            }
            break;
        }

        if self.pos >= self.src.len() {
            return Token::Eof;
        }

        let ch = self.peek_char().unwrap();

        if ch == '(' {
            self.bump(1);
            return Token::LParen;
        }
        if ch == ')' {
            self.bump(1);
            return Token::RParen;
        }
        if ch == '[' {
            self.bump(1);
            return Token::LBracket;
        }
        if ch == ']' {
            self.bump(1);
            return Token::RBracket;
        }
        if ch == '=' {
            let rest = &self.src[self.pos..];
            let mut it = rest.chars();
            it.next();
            if let Some('=') = it.next() {
                self.bump(2);
                return Token::EqEq;
            }
            self.bump(1);
            return Token::Equal;
        }
        if ch == ',' {
            self.bump(1);
            return Token::Comma;
        }
        if ch == '<' {
            let rest = &self.src[self.pos..];
            if rest.starts_with("<=") {
                self.bump(2);
                return Token::LessEqual;
            }
            self.bump(1);
            return Token::Less;
        }
        if ch == '>' {
            let rest = &self.src[self.pos..];
            if rest.starts_with(">=") {
                self.bump(2);
                return Token::GreaterEqual;
            }
            self.bump(1);
            return Token::Greater;
        }
        if ch == '+' {
            self.bump(1);
            return Token::Plus;
        }
        if ch == '-' {
            self.bump(1);
            return Token::Minus;
        }
        if ch == '*' {
            self.bump(1);
            return Token::Star;
        }
        if ch == '&' {
            let rest = &self.src[self.pos..];
            if rest.starts_with("&&") {
                self.bump(2);
                return Token::AndAnd;
            }
        }
        if ch == '|' {
            let rest = &self.src[self.pos..];
            if rest.starts_with("||") {
                self.bump(2);
                return Token::OrOr;
            }
        }
        if ch == '!' {
            let rest = &self.src[self.pos..];
            let mut it = rest.chars();
            it.next();
            if let Some('=') = it.next() {
                self.bump(2);
                return Token::NotEq;
            }
            self.bump(1);
            return Token::Bang;
        }
        if ch == '/' {
            let rest = &self.src[self.pos + 1..];

            let types = [
                "u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64", "f32", "f64", "str",
                "char", "bool", "array", "unit",
            ];
            for ty in types.iter() {
                if rest.starts_with(ty) {
                    let after = &rest[ty.len()..];
                    let next_char = after.chars().next();
                    if next_char.is_none()
                        || next_char == Some('\n')
                        || next_char == Some(' ')
                        || next_char == Some('\t')
                        || next_char == Some(',')
                        || next_char == Some(')')
                        || next_char == Some('/')
                    {
                        self.bump(1 + ty.len());
                        return Token::Type(ty.to_string());
                    }
                }
            }

            let mut i = self.pos + 1;
            let mut next_non_ws: Option<char> = None;
            while i < self.src.len() {
                let c = self.src[i..].chars().next().unwrap();
                if c == ' ' || c == '\t' {
                    i += c.len_utf8();
                    continue;
                }
                next_non_ws = Some(c);
                break;
            }
            self.bump(1);
            return match next_non_ws {
                Some('\n') | None => Token::Slash,
                _ => Token::SlashOp,
            };
        }

        if ch == '"' {
            self.bump(1);
            let mut s = String::new();
            while let Some(c) = self.peek_char() {
                if c == '"' {
                    self.bump(1);
                    break;
                }
                s.push(c);
                self.bump(1);
            }
            return Token::StringLiteral(s);
        }

        if (ch as u32) == 39 {
            self.bump(1);
            if let Some(c) = self.peek_char() {
                let chval = c;
                self.bump(1);

                if let Some('\'') = self.peek_char() {
                    self.bump(1);
                }
                return Token::CharLiteral(chval);
            }
        }

        if ch.is_ascii_alphabetic() || ch == '_' {
            let mut ident = String::new();
            while let Some(c) = self.peek_char() {
                if c.is_ascii_alphanumeric() || c == '_' {
                    ident.push(c);
                    self.bump(1);
                    continue;
                }
                break;
            }
            return match ident.as_str() {
                "fn" => Token::Fn,
                "new" => Token::New,
                "let" => Token::Let,
                "mut" => Token::Mut,
                "return" => Token::Return,
                "if" => Token::If,
                "while" => Token::While,
                "else" => Token::Else,
                "break" => Token::Break,
                "continue" => Token::Continue,
                "for" => Token::For,
                "in" => Token::In,
                _ => Token::Identifier(ident),
            };
        }

        if ch.is_ascii_digit() {
            let mut num = String::new();
            while let Some(c) = self.peek_char() {
                if c.is_ascii_digit() {
                    num.push(c);
                    self.bump(1);
                    continue;
                }
                if c == '.' && !num.contains('.') {
                    num.push(c);
                    self.bump(1);
                    continue;
                }
                break;
            }
            return Token::Number(num);
        }

        self.bump(1);
        self.next_token()
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Function {
        name: String,
        params: Vec<(String, Option<String>)>,
        ret: Option<String>,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let {
        name: String,
        value: Expr,
        mutable: bool,
        ty: Option<String>,
    },
    Assign {
        name: String,
        value: Expr,
    },
    Expr(Expr),
    Return(Expr, Option<String>),
    If {
        cond: Expr,
        then_block: Vec<Stmt>,
        else_block: Option<Vec<Stmt>>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    Break,
    Continue,
    For {
        var: String,
        iterable: Expr,
        body: Vec<Stmt>,
    },
    AssignIndex {
        name: String,
        index: Expr,
        value: Expr,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Identifier(String),
    StringLiteral(String),
    Char(char),
    Bool(bool),
    Number(String),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    ArrayLiteral(Vec<Expr>),
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
}

#[allow(dead_code)]
pub fn parse_program(_src: &str) -> Result<Program, String> {
    Ok(Program { items: vec![] })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_function_example() {
        let input = "fn new greet/\n    let name = \"Nayte\"/str\n    println(\"Hello {}\", name)/unit\nfn/";
        let mut lx = Lexer::new(input);
        let mut toks = Vec::new();
        loop {
            let t = lx.next_token();
            toks.push(t.clone());
            if t == Token::Eof {
                break;
            }
        }

        assert!(toks.contains(&Token::Fn));
        assert!(toks.contains(&Token::New));
        assert!(toks.contains(&Token::Identifier("greet".into())));
        assert!(toks.contains(&Token::StringLiteral("Nayte".into())));
        assert!(toks.contains(&Token::Slash));
    }

    #[test]
    fn ast_structs_smoke() {
        let prog = Program {
            items: vec![Item::Function {
                name: "greet".into(),
                params: vec![],
                ret: None,
                body: vec![],
            }],
        };
        match &prog.items[0] {
            Item::Function { name, params, .. } => {
                assert_eq!(name, "greet");
                assert_eq!(params.len(), 0);
            }
        }
    }

    #[test]
    fn lex_type_suffix_on_float() {
        let input = "let x = 1.5/f64\n";
        let mut lx = Lexer::new(input);
        let mut toks = Vec::new();
        loop {
            let t = lx.next_token();
            toks.push(format!("{:?}", t));
            if matches!(t, Token::Eof) {
                break;
            }
        }

        assert!(toks.iter().any(|s| s.contains("Type(\"f64\")")));
    }

    #[test]
    fn lex_type_suffix_on_char() {
        let input = "let c = 'a'/char\n";
        let mut lx = Lexer::new(input);
        let mut toks = Vec::new();
        loop {
            let t = lx.next_token();
            toks.push(format!("{:?}", t));
            if matches!(t, Token::Eof) {
                break;
            }
        }

        assert!(
            toks.iter()
                .any(|s| s.contains("CharLiteral('a')") || s.contains("CharLiteral(a)"))
        );
        assert!(toks.iter().any(|s| s.contains("Type(\"char\")")));
    }

    #[test]
    fn lex_println_unit_has_type() {
        let input = "println(\"hi\")/unit\n";
        let mut lx = Lexer::new(input);
        let mut toks = Vec::new();
        loop {
            let t = lx.next_token();
            toks.push(format!("{:?}", t));
            if matches!(t, Token::Eof) {
                break;
            }
        }

        assert!(toks.iter().any(|s| s.contains("RParen")));
        assert!(toks.iter().any(|s| s.contains("Type(\"unit\")")));
    }

    #[test]
    fn lex_brainfuck_example() {
        let src = std::fs::read_to_string("examples/brainfuck.sd").expect("read example");
        let mut l = Lexer::new(&src);
        let mut toks = Vec::new();
        loop {
            let t = l.next_token();
            println!("TOKEN: {:?}", t);
            if let Token::Eof = t {
                toks.push(t);
                break;
            }
            toks.push(t);
        }
        for i in 0..toks.len() - 1 {
            if matches!(toks[i], Token::Slash) && matches!(toks[i + 1], Token::Equal) {
                println!(
                    "Found Slash followed by Equal at token index {}: {:?} {:?}",
                    i,
                    toks[i],
                    toks[i + 1]
                );
            }
        }
    }
}
