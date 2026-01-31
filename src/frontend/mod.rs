use crate::syntax::{Expr, Item, Lexer, Program, Stmt, Token};

struct Parser<'a> {
    lexer: Lexer<'a>,
    cur: Token,
    peek: Token,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        let mut lx = Lexer::new(src);
        let cur = lx.next_token();
        let peek = lx.next_token();
        Parser {
            lexer: lx,
            cur,
            peek,
        }
    }

    fn advance(&mut self) {
        self.cur = std::mem::replace(&mut self.peek, self.lexer.next_token());
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        let mut items = vec![];
        loop {
            match &self.cur {
                Token::Eof => break,
                Token::Fn => {
                    // could be fn new ... or fn/ (end marker)
                    if let Token::Slash = self.peek {
                        // stray top-level fn/ — consume and continue
                        self.advance(); // cur becomes Slash
                        self.advance(); // cur becomes next
                        continue;
                    }
                    let f = self.parse_function()?;
                    items.push(Item::Function {
                        name: f.0,
                        params: f.1,
                        ret: f.2,
                        body: f.3,
                    });
                }
                _ => {
                    // skip unknown top-level tokens
                    self.advance();
                }
            }
        }
        Ok(Program { items })
    }

    fn parse_function(
        &mut self,
    ) -> Result<
        (
            String,
            Vec<(String, Option<String>)>,
            Option<String>,
            Vec<Stmt>,
        ),
        String,
    > {
        // expecting: Fn New Identifier (params)? / ... Fn /
        // cur == Fn
        self.advance(); // now cur == New or other
        match &self.cur {
            Token::New => {}
            other => return Err(format!("expected 'new' after fn, got: {:?}", other)),
        }
        self.advance(); // now cur should be Identifier
        let name = match &self.cur {
            Token::Identifier(s) => s.clone(),
            other => return Err(format!("expected identifier for fn name, got: {:?}", other)),
        };

        // optional params (now typed: name/type)
        let mut params: Vec<(String, Option<String>)> = Vec::new();
        self.advance();
        if let Token::LParen = &self.cur {
            // parse param list
            self.advance();
            while let Token::Identifier(_) = &self.cur {
                if let Token::Identifier(id) = &self.cur {
                    let name = id.clone();
                    self.advance();
                    // optional type annotation like '/i64' after param
                    if let Token::Type(t) = &self.cur {
                        let ptype = t.clone();
                        self.advance();
                        params.push((name, Some(ptype)));
                    } else {
                        params.push((name, None));
                    }
                }
                if let Token::Comma = &self.cur {
                    self.advance();
                    continue;
                }
                break;
            }
            if let Token::RParen = &self.cur {
                self.advance();
            } else {
                return Err("expected ')' in parameter list".into());
            }
        }

        // now expect '/' or '/<type>' (return type may be present as '/type')
        let mut ret_type: Option<String> = None;
        match &self.cur {
            Token::Slash => {
                self.advance();
            }
            Token::Type(t) => {
                ret_type = Some(t.clone());
                self.advance();
            }
            other => {
                return Err(format!(
                    "expected '/' or '/<type>' after function header, got: {:?}",
                    other
                ));
            }
        }

        // parse body until Fn Slash sequence
        let mut body = Vec::new();
        while !(matches!(&self.cur, Token::Fn) && matches!(&self.peek, Token::Slash)) {
            if let Token::Eof = &self.cur {
                return Err("unexpected EOF while parsing function body".into());
            }
            // DEBUG: print cur/peek for tracing
            println!("PARSE_FUNCTION: cur={:?} peek={:?}", self.cur, self.peek);
            if let Some(stmt) = self.parse_stmt()? {
                body.push(stmt);
            } else {
                // no stmt parsed, advance to avoid infinite loop
                self.advance();
            }
        }

        // consume Fn Slash
        self.advance(); // cur becomes Slash
        self.advance(); // cur becomes next after end

        Ok((name, params, ret_type, body))
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>, String> {
        // DEBUG: trace stmt entry
        println!("PARSE_STMT ENTRY cur={:?} peek={:?}", self.cur, self.peek);
        match &self.cur {
            Token::Let => {
                // let (mut)? name = expr (/type)? /
                self.advance();
                let mut mutable = false;
                if let Token::Mut = &self.cur {
                    mutable = true;
                    self.advance();
                }
                let name = match &self.cur {
                    Token::Identifier(s) => s.clone(),
                    other => {
                        return Err(format!("expected identifier after let, got: {:?}", other));
                    }
                };
                self.advance(); // expect Equal
                if let Token::Equal = &self.cur {
                } else {
                    return Err("expected '=' after let name".into());
                }
                self.advance(); // expr start
                let expr = self.parse_expr()?;
                // mandatory type annotation token (acts as terminator)
                let ty = if let Token::Type(t) = &self.cur {
                    let s = t.clone();
                    self.advance();
                    s
                } else {
                    return Err("expected '/<type>' after statement".into());
                };
                Ok(Some(Stmt::Let {
                    name,
                    value: expr,
                    mutable,
                    ty: Some(ty),
                }))
            }
            Token::Return => {
                // return expr / or /<type>
                self.advance();
                let expr = self.parse_expr()?;
                let ty = if let Token::Type(t) = &self.cur {
                    let s = t.clone();
                    self.advance();
                    Some(s)
                } else if let Token::Slash = &self.cur {
                    self.advance();
                    None
                } else {
                    return Err("expected '/' or '/<type>' after return".into());
                };
                Ok(Some(Stmt::Return(expr, ty)))
            }
            Token::Break => {
                // break /
                self.advance();
                if let Token::Type(_t) = &self.cur {
                    self.advance();
                    return Ok(Some(Stmt::Break));
                } else if let Token::Slash = &self.cur {
                    self.advance();
                    return Ok(Some(Stmt::Break));
                } else {
                    return Err("expected '/' after break".into());
                }
            }
            Token::Continue => {
                // continue /
                self.advance();
                if let Token::Type(_t) = &self.cur {
                    self.advance();
                    return Ok(Some(Stmt::Continue));
                } else if let Token::Slash = &self.cur {
                    self.advance();
                    return Ok(Some(Stmt::Continue));
                } else {
                    return Err("expected '/' after continue".into());
                }
            }
            Token::For => {
                // for <ident> in <expr>/ <stmts> fn/
                self.advance();
                let var = match &self.cur {
                    Token::Identifier(s) => s.clone(),
                    other => {
                        return Err(format!("expected identifier after for, got: {:?}", other));
                    }
                };
                self.advance();
                if let Token::In = &self.cur {
                    self.advance();
                } else {
                    return Err("expected 'in' after for variable".into());
                }
                let iterable = self.parse_expr()?;
                // accept optional type annotation then '/'
                if let Token::Type(_t) = &self.cur {
                    self.advance();
                }
                if let Token::Slash = &self.cur {
                    self.advance();
                } else {
                    return Err("expected '/' after for iterable".into());
                }
                // parse body until Fn /
                let mut body = Vec::new();
                while !(matches!(&self.cur, Token::Fn) && matches!(&self.peek, Token::Slash)) {
                    if let Token::Eof = &self.cur {
                        return Err("unexpected EOF while parsing for body".into());
                    }
                    if let Some(stmt) = self.parse_stmt()? {
                        body.push(stmt);
                    } else {
                        self.advance();
                    }
                }
                // consume Fn /
                self.advance();
                self.advance();
                Ok(Some(Stmt::For {
                    var,
                    iterable,
                    body,
                }))
            }
            Token::If => {
                // if <cond>/ <stmts> (else/ <stmts>)? fn/
                self.advance(); // move to condition start
                let cond = self.parse_expr()?;
                // accept optional type annotation then '/'
                if let Token::Type(_t) = &self.cur {
                    self.advance();
                }
                if let Token::Slash = &self.cur {
                    self.advance();
                } else {
                    return Err("expected '/' after if condition".into());
                }
                // parse then block until Else/ or Fn/
                let mut then_block = Vec::new();
                while !(matches!(&self.cur, Token::Else)
                    || (matches!(&self.cur, Token::Fn) && matches!(&self.peek, Token::Slash)))
                {
                    if let Token::Eof = &self.cur {
                        return Err("unexpected EOF in if then-block".into());
                    }
                    if let Some(stmt) = self.parse_stmt()? {
                        then_block.push(stmt);
                    } else {
                        self.advance();
                    }
                }
                let mut else_block = None;
                if let Token::Else = &self.cur {
                    // consume Else and expected '/'
                    self.advance();
                    if let Token::Slash = &self.cur {
                        self.advance();
                    } else {
                        return Err("expected '/' after else".into());
                    }
                    let mut eblock = Vec::new();
                    while !(matches!(&self.cur, Token::Fn) && matches!(&self.peek, Token::Slash)) {
                        if let Token::Eof = &self.cur {
                            return Err("unexpected EOF in else block".into());
                        }
                        if let Some(stmt) = self.parse_stmt()? {
                            eblock.push(stmt);
                        } else {
                            self.advance();
                        }
                    }
                    else_block = Some(eblock);
                }
                // consume Fn /
                if matches!(&self.cur, Token::Fn) && matches!(&self.peek, Token::Slash) {
                    self.advance();
                    self.advance();
                } else {
                    return Err("expected 'fn/' to close if".into());
                }
                Ok(Some(Stmt::If {
                    cond,
                    then_block,
                    else_block,
                }))
            }
            Token::While => {
                // while <cond>/ <stmts> fn/
                self.advance(); // move to condition start
                let cond = self.parse_expr()?;
                // accept optional type annotation then '/'
                if let Token::Type(_t) = &self.cur {
                    self.advance();
                }
                if let Token::Slash = &self.cur {
                    self.advance();
                } else {
                    return Err("expected '/' after while condition".into());
                }
                // parse body until Fn/
                let mut body = Vec::new();
                while !(matches!(&self.cur, Token::Fn) && matches!(&self.peek, Token::Slash)) {
                    if let Token::Eof = &self.cur {
                        return Err("unexpected EOF in while body".into());
                    }
                    if let Some(stmt) = self.parse_stmt()? {
                        body.push(stmt);
                    } else {
                        self.advance();
                    }
                }
                // consume Fn /
                if matches!(&self.cur, Token::Fn) && matches!(&self.peek, Token::Slash) {
                    self.advance();
                    self.advance();
                } else {
                    return Err("expected 'fn/' to close while".into());
                }
                Ok(Some(Stmt::While { cond, body }))
            }
            Token::Identifier(_) => {
                // could be an assignment: ident = expr /  OR ident[expr] = expr /
                if let Token::Identifier(id) = &self.cur {
                    let name = id.clone();
                    // assignment to index a[expr] = val/
                    if let Token::LBracket = &self.peek {
                        // parse index expression
                        self.advance(); // cur becomes LBracket
                        self.advance(); // cur becomes index expr start
                        let idx = self.parse_expr()?;
                        if let Token::RBracket = &self.cur {
                            self.advance();
                        } else {
                            return Err("expected ']' in index".into());
                        }
                        if let Token::Equal = &self.cur {
                            self.advance(); // cur becomes expr start
                            let expr = self.parse_expr()?;
                            if let Token::Type(_t) = &self.cur {
                                self.advance();
                                return Ok(Some(Stmt::AssignIndex {
                                    name,
                                    index: idx,
                                    value: expr,
                                }));
                            } else if let Token::Slash = &self.cur {
                                self.advance();
                                return Ok(Some(Stmt::AssignIndex {
                                    name,
                                    index: idx,
                                    value: expr,
                                }));
                            } else {
                                return Err("expected '/' after assignment".into());
                            }
                        }
                        // if not an assignment, we should treat this as an index expr statement
                        // fallthrough to parse_expr
                    }
                    if let Token::Equal = &self.peek {
                        self.advance(); // cur becomes Equal
                        self.advance(); // cur becomes expr start
                        let expr = self.parse_expr()?;
                        if let Token::Type(_t) = &self.cur {
                            self.advance();
                            return Ok(Some(Stmt::Assign { name, value: expr }));
                        } else if let Token::Slash = &self.cur {
                            self.advance();
                            return Ok(Some(Stmt::Assign { name, value: expr }));
                        } else {
                            return Err("expected '/' after assignment".into());
                        }
                    }
                }
                // fallback to expression statement
                let expr = self.parse_expr()?;
                if let Token::Type(_t) = &self.cur {
                    self.advance();
                    Ok(Some(Stmt::Expr(expr)))
                } else if let Token::Slash = &self.cur {
                    self.advance();
                    Ok(Some(Stmt::Expr(expr)))
                } else {
                    Err("expected '/' after expression".into())
                }
            }
            Token::StringLiteral(_) => {
                // expression statement (call or literal)
                let expr = self.parse_expr()?;
                if let Token::Type(_t) = &self.cur {
                    self.advance();
                    Ok(Some(Stmt::Expr(expr)))
                } else if let Token::Slash = &self.cur {
                    self.advance();
                    Ok(Some(Stmt::Expr(expr)))
                } else {
                    Err("expected '/' after expression".into())
                }
            }
            _ => Ok(None),
        }
    }

    // Expression parsing with precedence: comparators < add/sub < mul/div
    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_or()
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_add_sub()?;
        loop {
            let op = match &self.cur {
                Token::EqEq => Some(crate::syntax::BinOp::Eq),
                Token::NotEq => Some(crate::syntax::BinOp::Ne),
                Token::Less => Some(crate::syntax::BinOp::Lt),
                Token::LessEqual => Some(crate::syntax::BinOp::Le),
                Token::Greater => Some(crate::syntax::BinOp::Gt),
                Token::GreaterEqual => Some(crate::syntax::BinOp::Ge),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let right = self.parse_add_sub()?;
                left = Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_comparison()?;
        loop {
            if let Token::AndAnd = self.cur {
                self.advance();
                let right = self.parse_comparison()?;
                left = Expr::Binary {
                    left: Box::new(left),
                    op: crate::syntax::BinOp::And,
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(left)
    }
    // Note: this function was kept in the file as a helper when refactoring; kept for backward compatibility.
    #[allow(dead_code)]
    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;
        loop {
            if let Token::OrOr = self.cur {
                self.advance();
                let right = self.parse_and()?;
                left = Expr::Binary {
                    left: Box::new(left),
                    op: crate::syntax::BinOp::Or,
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(left)
    }

    fn parse_add_sub(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_mul_div()?;
        loop {
            let op = match &self.cur {
                Token::Plus => Some(crate::syntax::BinOp::Add),
                Token::Minus => Some(crate::syntax::BinOp::Sub),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let right = self.parse_mul_div()?;
                left = Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(left)
    }

    fn parse_mul_div(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary()?;
        loop {
            let op = match &self.cur {
                Token::Star => Some(crate::syntax::BinOp::Mul),
                Token::SlashOp => Some(crate::syntax::BinOp::Div),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let right = self.parse_unary()?;
                left = Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        match &self.cur {
            Token::Minus => {
                self.advance();
                let rhs = self.parse_unary()?;
                // unary - as numeric negate (prefer explicit Unary op)
                Ok(Expr::Unary {
                    op: crate::syntax::UnaryOp::Neg,
                    expr: Box::new(rhs),
                })
            }
            Token::Bang => {
                self.advance();
                let inner = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: crate::syntax::UnaryOp::Not,
                    expr: Box::new(inner),
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match &self.cur {
            Token::Identifier(name) => {
                let name = name.clone();
                // handle boolean literals 'true' and 'false'
                if name == "true" {
                    self.advance();
                    return Ok(Expr::Bool(true));
                }
                if name == "false" {
                    self.advance();
                    return Ok(Expr::Bool(false));
                }
                self.advance();
                if let Token::LParen = &self.cur {
                    self.advance();
                    let mut args = Vec::new();
                    while let Token::StringLiteral(_)
                    | Token::Identifier(_)
                    | Token::Number(_)
                    | Token::LParen
                    | Token::Bang
                    | Token::Minus
                    | Token::CharLiteral(_) = &self.cur
                    {
                        args.push(self.parse_expr()?);
                        if let Token::Comma = &self.cur {
                            self.advance();
                            continue;
                        }
                        if let Token::RParen = &self.cur {
                            break;
                        }
                    }
                    if let Token::RParen = &self.cur {
                        self.advance();
                    } else {
                        return Err("expected ')'".into());
                    }
                    Ok(Expr::Call { name, args })
                } else {
                    let mut primary = Ok(Expr::Identifier(name));
                    // support indexing after an identifier: a[expr]
                    while let Token::LBracket = &self.cur {
                        self.advance();
                        let idx = self.parse_expr()?;
                        if let Token::RBracket = &self.cur {
                            self.advance();
                        } else {
                            return Err("expected ']'".into());
                        }
                        let cur_expr = primary?;
                        primary = Ok(Expr::Index {
                            expr: Box::new(cur_expr),
                            index: Box::new(idx),
                        });
                    }
                    primary
                }
            }
            Token::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::StringLiteral(s))
            }
            Token::CharLiteral(c) => {
                let c = *c;
                self.advance();
                Ok(Expr::Char(c))
            }
            Token::Number(n) => {
                let n = n.clone();
                self.advance();
                Ok(Expr::Number(n))
            }
            Token::LBracket => {
                // array literal
                self.advance(); // consume '['
                let mut elems = Vec::new();
                while !matches!(&self.cur, Token::RBracket) {
                    elems.push(self.parse_expr()?);
                    if let Token::Comma = &self.cur {
                        self.advance();
                        continue;
                    }
                    if matches!(&self.cur, Token::RBracket) {
                        break;
                    }
                }
                if let Token::RBracket = &self.cur {
                    self.advance();
                    Ok(Expr::ArrayLiteral(elems))
                } else {
                    Err("expected ']'".into())
                }
            }
            Token::LParen => {
                self.advance();
                let e = self.parse_expr()?;
                if let Token::RParen = &self.cur {
                    self.advance();
                    Ok(e)
                } else {
                    Err("expected ')'".into())
                }
            }
            other => Err(format!("unexpected token in expr: {:?}", other)),
        }
    }
}

pub fn parse(src: &str) -> Result<Program, String> {
    let mut p = Parser::new(src);
    p.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_function_with_let_and_call() {
        let src = "fn new greet/\n    let name = \"Nayte\"/str\n    println(\"Hello {}\", name)/unit\nfn/";
        let res = parse(src).expect("parse failed");
        assert_eq!(res.items.len(), 1);
        match &res.items[0] {
            Item::Function {
                name, params, body, ..
            } => {
                assert_eq!(name, "greet");
                assert_eq!(params.len(), 0);
                assert_eq!(body.len(), 2);
            }
        }
    }

    #[test]
    fn parse_function_with_params() {
        let src = "fn new add(a/i64, b/i64)/\n    println(\"{} {}\", a, b)/unit\nfn/";
        let res = parse(src).expect("parse failed");
        match &res.items[0] {
            Item::Function {
                name, params, body, ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(
                    params,
                    &vec![
                        ("a".to_string(), Some("i64".to_string())),
                        ("b".to_string(), Some("i64".to_string()))
                    ]
                );
                assert_eq!(body.len(), 1);
            }
        }
    }

    #[test]
    fn parse_if_else() {
        let src = "fn new main/\n    if 1 == 1/\n        println(\"yes\")/unit\n    else/\n        println(\"no\")/unit\n    fn/\nfn/";
        let res = parse(src).expect("parse failed");
        match &res.items[0] {
            Item::Function { name, body, .. } => {
                assert_eq!(name, "main");
                assert_eq!(body.len(), 1);
            }
        }
    }

    #[test]
    fn parse_while_simple() {
        let src = "fn new main/\n    let mut x = 0/i64\n    while x < 3/\n        x = x + 1/i64\n    fn/\n    println(\"{}\", x)/unit\nfn/";
        let res = parse(src).expect("parse failed");
        match &res.items[0] {
            Item::Function { name, body, .. } => {
                assert_eq!(name, "main");
                assert_eq!(body.len(), 3);
                match &body[1] {
                    Stmt::While { cond: _, body: b } => assert_eq!(b.len(), 1),
                    other => panic!("expected while, got {:?}", other),
                }
            }
        }
    }

    #[test]
    fn parse_let_with_type() {
        let src = "fn new main/\n    let age = 20/u8\nfn/";
        let res = parse(src).expect("parse failed");
        match &res.items[0] {
            Item::Function { body, .. } => {
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Stmt::Let {
                        name,
                        value,
                        mutable,
                        ty,
                    } => {
                        assert_eq!(name, "age");
                        assert_eq!(*mutable, false);
                        assert_eq!(ty.as_ref().map(|s| s.as_str()), Some("u8"));
                        match value {
                            Expr::Number(n) => assert_eq!(n, "20"),
                            _ => panic!("expected number"),
                        }
                    }
                    other => panic!("unexpected stmt: {:?}", other),
                }
            }
        }
    }

    #[test]
    fn parse_float_lets() {
        let src = "fn new main/\n    let x = 1.5/f64\n    let y = 2.0/f64\n    println(\"{}\", x + y)/unit\nfn/";
        let res = parse(src).expect("parse failed");
        match &res.items[0] {
            Item::Function { body, .. } => {
                assert_eq!(body.len(), 3);
            }
        }
    }

    #[test]
    fn parse_array_and_index() {
        let src = "fn new main/\n    let a = [1, 2, 3]/array\n    println(\"{}\", a[1])/unit\nfn/";
        let res = parse(src).expect("parse failed");
        match &res.items[0] {
            Item::Function { body, .. } => {
                assert_eq!(body.len(), 2);
                match &body[0] {
                    Stmt::Let { name, value, .. } => match value {
                        Expr::ArrayLiteral(elems) => assert_eq!(elems.len(), 3),
                        _ => panic!("expected array"),
                    },
                    other => panic!("expected let, got {:?}", other),
                }
                match &body[1] {
                    Stmt::Expr(e) => match e {
                        Expr::Call { name, args } => {
                            assert_eq!(name, "println");
                            match &args[1] {
                                Expr::Index { expr: _, index: _ } => {}
                                _ => panic!("expected index expr"),
                            }
                        }
                        _ => panic!("unexpected expr"),
                    },
                    other => panic!("expected expr, got {:?}", other),
                }
            }
        }
    }

    // DEBUG: step through parsing the brainfuck example to locate parse failure
    #[test]
    fn debug_parse_brainfuck_example() {
        let src = std::fs::read_to_string("examples/brainfuck.shiden").expect("read example");
        let mut p = Parser::new(&src);
        loop {
            println!("CUR {:?} PEEK {:?}", p.cur, p.peek);
            match p.parse_stmt() {
                Ok(Some(s)) => println!("Parsed stmt: {:?}", s),
                Ok(None) => {
                    println!("no more stmts");
                    break;
                }
                Err(e) => panic!("parse error at cur {:?} peek {:?}: {}", p.cur, p.peek, e),
            }
        }
    }

    #[test]
    fn debug_parse_brainfuck_function() {
        let src = std::fs::read_to_string("examples/brainfuck.shiden").expect("read example");
        let mut p = Parser::new(&src);
        // advance until we hit the function header for run_bf
        while !(matches!(&p.cur, Token::Fn) && matches!(&p.peek, Token::New)) {
            p.advance();
            if let Token::Eof = p.cur {
                panic!("no function found");
            }
        }
        // now call parse_function and catch error
        match p.parse_function() {
            Ok((name, _params, _ret, body)) => {
                println!("Parsed function '{}' with {} stmts", name, body.len())
            }
            Err(e) => panic!(
                "parse_function failed at cur {:?} peek {:?}: {}",
                p.cur, p.peek, e
            ),
        }
    }
}
