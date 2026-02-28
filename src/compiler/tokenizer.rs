use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Pub,
    Fn,
    Struct,
    Enum,
    Trait,
    Impl,
    Let,
    Mut,
    Self_,
    If,
    Else,
    While,
    For,
    In,
    Match,
    Return,

    Ident(String),
    Int(i64),

    Arrow,
    Eq,
    Neq,
    Lte,
    Gte,
    And,
    Or,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Lt,
    Gt,
    Bang,
    Assign,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Colon,
    DoubleColon,
    Comma,
    Dot,

    Eof,
}

pub struct Tokenizer {
    chars: Peekable<Chars<'static>>,
    current: Option<char>,
}

impl Tokenizer {
    pub fn new(input: &'static str) -> Self {
        let mut chars = input.chars().peekable();
        let current = chars.next();
        Tokenizer { chars, current }
    }

    fn advance(&mut self) {
        self.current = self.chars.next();
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_line_comment(&mut self) {
        self.advance();
        self.advance();
        while let Some(c) = self.current {
            if c == '\n' {
                self.advance();
                break;
            }
            self.advance();
        }
    }

    fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        while let Some(c) = self.current {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }
        ident
    }

    fn read_int(&mut self) -> i64 {
        let mut num_str = String::new();
        while let Some(c) = self.current {
            if c.is_ascii_digit() {
                num_str.push(c);
                self.advance();
            } else {
                break;
            }
        }
        num_str.parse::<i64>().unwrap_or(0)
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            self.skip_whitespace();

            let next_char = self.peek();

            match self.current {
                None => return Token::Eof,
                Some('/') if next_char == Some('/') => {
                    self.skip_line_comment();
                }
                Some(c) if c.is_ascii_digit() => {
                    return Token::Int(self.read_int());
                }
                Some(c) if c.is_alphabetic() || c == '_' => {
                    let ident = self.read_ident();
                    return match ident.as_str() {
                        "pub" => Token::Pub,
                        "fn" => Token::Fn,
                        "struct" => Token::Struct,
                        "enum" => Token::Enum,
                        "trait" => Token::Trait,
                        "impl" => Token::Impl,
                        "let" => Token::Let,
                        "mut" => Token::Mut,
                        "self" => Token::Self_,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "for" => Token::For,
                        "in" => Token::In,
                        "match" => Token::Match,
                        "return" => Token::Return,
                        _ => Token::Ident(ident),
                    };
                }
                Some('-') if next_char == Some('>') => {
                    self.advance();
                    self.advance();
                    return Token::Arrow;
                }
                Some('=') if next_char == Some('=') => {
                    self.advance();
                    self.advance();
                    return Token::Eq;
                }
                Some('!') if next_char == Some('=') => {
                    self.advance();
                    self.advance();
                    return Token::Neq;
                }
                Some('<') if next_char == Some('=') => {
                    self.advance();
                    self.advance();
                    return Token::Lte;
                }
                Some('>') if next_char == Some('=') => {
                    self.advance();
                    self.advance();
                    return Token::Gte;
                }
                Some('&') if next_char == Some('&') => {
                    self.advance();
                    self.advance();
                    return Token::And;
                }
                Some('|') if next_char == Some('|') => {
                    self.advance();
                    self.advance();
                    return Token::Or;
                }
                Some(':') if next_char == Some(':') => {
                    self.advance();
                    self.advance();
                    return Token::DoubleColon;
                }
                Some('+') => {
                    self.advance();
                    return Token::Plus;
                }
                Some('-') => {
                    self.advance();
                    return Token::Minus;
                }
                Some('*') => {
                    self.advance();
                    return Token::Star;
                }
                Some('/') => {
                    self.advance();
                    return Token::Slash;
                }
                Some('%') => {
                    self.advance();
                    return Token::Percent;
                }
                Some('<') => {
                    self.advance();
                    return Token::Lt;
                }
                Some('>') => {
                    self.advance();
                    return Token::Gt;
                }
                Some('!') => {
                    self.advance();
                    return Token::Bang;
                }
                Some('=') => {
                    self.advance();
                    return Token::Assign;
                }
                Some('(') => {
                    self.advance();
                    return Token::LParen;
                }
                Some(')') => {
                    self.advance();
                    return Token::RParen;
                }
                Some('{') => {
                    self.advance();
                    return Token::LBrace;
                }
                Some('}') => {
                    self.advance();
                    return Token::RBrace;
                }
                Some(';') => {
                    self.advance();
                    return Token::Semicolon;
                }
                Some(':') => {
                    self.advance();
                    return Token::Colon;
                }
                Some(',') => {
                    self.advance();
                    return Token::Comma;
                }
                Some('.') => {
                    self.advance();
                    return Token::Dot;
                }
                Some(c) => {
                    eprintln!("Unexpected character: {}", c);
                    self.advance();
                }
            }
        }
    }
}

pub fn tokenize(input: &'static str) -> Result<Vec<Token>, String> {
    let mut tokenizer = Tokenizer::new(input);
    let mut tokens = Vec::new();

    loop {
        let token = tokenizer.next_token();
        if token == Token::Eof {
            tokens.push(Token::Eof);
            break;
        }
        tokens.push(token);
    }

    Ok(tokens)
}
