use crate::compiler::tokenizer::Token;
use crate::compiler::types::*;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    fn peek(&self, offset: usize) -> &Token {
        self.tokens.get(self.pos + offset).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if std::mem::discriminant(self.current()) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, found {:?} at position {}",
                expected,
                self.current(),
                self.pos
            ))
        }
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        if let Token::Ident(name) = self.current() {
            let type_name = name.clone();
            self.advance();

            if let Some(typ) = Type::from_str(&type_name) {
                Ok(typ)
            } else {
                Ok(Type::Struct(type_name))
            }
        } else {
            Err(format!("Expected type, found {:?}", self.current()))
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut structs = Vec::new();
        let mut enums = Vec::new();
        let mut functions = Vec::new();
        let mut traits = Vec::new();
        let mut trait_impls = Vec::new();
        let mut impl_blocks = Vec::new();

        while self.current() != &Token::Eof {
            match self.current() {
                Token::Struct => {
                    structs.push(self.parse_struct_def()?);
                }
                Token::Enum => {
                    enums.push(self.parse_enum_def()?);
                }
                Token::Trait => {
                    traits.push(self.parse_trait()?);
                }
                Token::Impl => {
                    let checkpoint = self.pos;
                    self.advance();

                    let _first_name = if let Token::Ident(_) = self.current() {
                        self.advance();
                        true
                    } else {
                        self.pos = checkpoint;
                        return Err("Expected identifier after 'impl'".to_string());
                    };

                    if self.current() == &Token::For {
                        self.pos = checkpoint;
                        trait_impls.push(self.parse_impl_for_trait()?);
                    } else {
                        self.pos = checkpoint;
                        impl_blocks.push(self.parse_impl_block()?);
                    }
                }
                Token::Pub => {
                    functions.push(self.parse_function()?);
                }
                _ => {
                    return Err(format!(
                        "Unexpected token at program level: {:?}",
                        self.current()
                    ));
                }
            }
        }

        for (struct_name, methods, associated_functions) in impl_blocks {
            if let Some(struct_def) = structs.iter_mut().find(|s| s.name == struct_name) {
                struct_def.methods.extend(methods);
                struct_def.associated_functions.extend(associated_functions);
            }
        }

        Ok(Program {
            structs,
            enums,
            functions,
            traits,
            trait_impls,
        })
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        self.expect(Token::Pub)?;
        self.expect(Token::Fn)?;

        let name = if let Token::Ident(n) = self.current() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected function name, found {:?}",
                self.current()
            ));
        };

        self.expect(Token::LParen)?;

        let mut params = Vec::new();
        while self.current() != &Token::RParen {
            let param_name = if let Token::Ident(n) = self.current() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(format!(
                    "Expected parameter name, found {:?}",
                    self.current()
                ));
            };

            self.expect(Token::Colon)?;
            let param_type = self.parse_type()?;

            params.push((param_name, param_type));

            if self.current() == &Token::Comma {
                self.advance();
            }
        }

        self.expect(Token::RParen)?;

        let ret_type = if self.current() == &Token::Arrow {
            self.advance();
            self.parse_type()?
        } else {
            Type::Void
        };

        self.expect(Token::LBrace)?;

        let body = self.parse_block()?;

        self.expect(Token::RBrace)?;

        Ok(Function {
            name,
            params,
            ret_type,
            body,
        })
    }

    fn parse_struct_def(&mut self) -> Result<StructDef, String> {
        self.expect(Token::Struct)?;

        let name = if let Token::Ident(n) = self.current() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(format!("Expected struct name, found {:?}", self.current()));
        };

        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();
        while self.current() != &Token::RBrace && self.current() != &Token::Eof {
            let field_name = if let Token::Ident(n) = self.current() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(format!("Expected field name, found {:?}", self.current()));
            };

            self.expect(Token::Colon)?;
            let field_type = self.parse_type()?;

            fields.push((field_name, field_type));

            if self.current() == &Token::Comma {
                self.advance();
            }
        }

        self.expect(Token::RBrace)?;

        Ok(StructDef {
            name,
            fields,
            methods: Vec::new(),
            associated_functions: Vec::new(),
        })
    }

    fn parse_enum_def(&mut self) -> Result<EnumDef, String> {
        self.expect(Token::Enum)?;

        let name = if let Token::Ident(n) = self.current() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(format!("Expected enum name, found {:?}", self.current()));
        };

        self.expect(Token::LBrace)?;

        let mut variants = Vec::new();
        while self.current() != &Token::RBrace && self.current() != &Token::Eof {
            let variant_name = if let Token::Ident(n) = self.current() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(format!("Expected variant name, found {:?}", self.current()));
            };

            let variant_type = if self.current() == &Token::LParen {
                self.advance();
                let variant_type = self.parse_type()?;
                self.expect(Token::RParen)?;
                Some(variant_type)
            } else {
                None
            };

            variants.push((variant_name, variant_type));

            if self.current() == &Token::Comma {
                self.advance();
            }
        }

        self.expect(Token::RBrace)?;

        Ok(EnumDef { name, variants })
    }

    fn parse_impl_block(&mut self) -> Result<(String, Vec<MethodDef>, Vec<Function>), String> {
        self.expect(Token::Impl)?;

        let struct_name = if let Token::Ident(n) = self.current() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected struct name after impl, found {:?}",
                self.current()
            ));
        };

        self.expect(Token::LBrace)?;

        let mut methods = Vec::new();
        let mut associated_functions = Vec::new();

        while self.current() != &Token::RBrace && self.current() != &Token::Eof {
            self.expect(Token::Fn)?;

            let func_name = if let Token::Ident(n) = self.current() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(format!(
                    "Expected function/method name, found {:?}",
                    self.current()
                ));
            };

            self.expect(Token::LParen)?;

            if self.current() == &Token::Self_ {
                self.advance();
                let mut params = vec![("self".to_string(), Type::Void)];

                if self.current() != &Token::RParen {
                    self.expect(Token::Comma)?;

                    while self.current() != &Token::RParen {
                        let param_name = if let Token::Ident(n) = self.current() {
                            let name = n.clone();
                            self.advance();
                            name
                        } else {
                            return Err(format!(
                                "Expected parameter name, found {:?}",
                                self.current()
                            ));
                        };

                        self.expect(Token::Colon)?;
                        let param_type = self.parse_type()?;

                        params.push((param_name, param_type));

                        if self.current() == &Token::Comma {
                            self.advance();
                        }
                    }
                }

                self.expect(Token::RParen)?;

                let ret_type = if self.current() == &Token::Arrow {
                    self.advance();
                    self.parse_type()?
                } else {
                    Type::Void
                };

                self.expect(Token::LBrace)?;
                let body = self.parse_block()?;
                self.expect(Token::RBrace)?;

                methods.push(MethodDef {
                    name: func_name,
                    params,
                    ret_type,
                    body,
                });
            } else {
                let mut params = Vec::new();

                while self.current() != &Token::RParen {
                    let param_name = if let Token::Ident(n) = self.current() {
                        let name = n.clone();
                        self.advance();
                        name
                    } else {
                        return Err(format!(
                            "Expected parameter name, found {:?}",
                            self.current()
                        ));
                    };

                    self.expect(Token::Colon)?;
                    let param_type = self.parse_type()?;

                    params.push((param_name, param_type));

                    if self.current() == &Token::Comma {
                        self.advance();
                    }
                }

                self.expect(Token::RParen)?;

                let ret_type = if self.current() == &Token::Arrow {
                    self.advance();
                    self.parse_type()?
                } else {
                    Type::Void
                };

                self.expect(Token::LBrace)?;
                let body = self.parse_block()?;
                self.expect(Token::RBrace)?;

                associated_functions.push(Function {
                    name: func_name,
                    params,
                    ret_type,
                    body,
                });
            }
        }

        self.expect(Token::RBrace)?;

        Ok((struct_name, methods, associated_functions))
    }

    fn parse_trait(&mut self) -> Result<TraitDef, String> {
        self.expect(Token::Trait)?;

        let name = if let Token::Ident(n) = self.current() {
            let trait_name = n.clone();
            self.advance();
            trait_name
        } else {
            return Err(format!("Expected trait name, found {:?}", self.current()));
        };

        self.expect(Token::LBrace)?;

        let mut methods = Vec::new();

        while self.current() != &Token::RBrace && self.current() != &Token::Eof {
            self.expect(Token::Fn)?;

            let method_name = if let Token::Ident(n) = self.current() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(format!(
                    "Expected method name in trait, found {:?}",
                    self.current()
                ));
            };

            self.expect(Token::LParen)?;

            let mut params = Vec::new();
            let has_self = self.current() == &Token::Self_;

            if has_self {
                self.advance();
                params.push(("self".to_string(), Type::Void));

                if self.current() != &Token::RParen {
                    self.expect(Token::Comma)?;
                }
            }

            while self.current() != &Token::RParen {
                let param_name = if let Token::Ident(n) = self.current() {
                    let name = n.clone();
                    self.advance();
                    name
                } else {
                    return Err(format!(
                        "Expected parameter name, found {:?}",
                        self.current()
                    ));
                };

                self.expect(Token::Colon)?;
                let param_type = self.parse_type()?;

                params.push((param_name, param_type));

                if self.current() == &Token::Comma {
                    self.advance();
                }
            }

            self.expect(Token::RParen)?;

            let ret_type = if self.current() == &Token::Arrow {
                self.advance();
                self.parse_type()?
            } else {
                Type::Void
            };

            self.expect(Token::Semicolon)?;

            methods.push(TraitMethod {
                name: method_name,
                params,
                ret_type,
            });
        }

        self.expect(Token::RBrace)?;

        Ok(TraitDef { name, methods })
    }

    fn parse_impl_for_trait(&mut self) -> Result<TraitImpl, String> {
        self.expect(Token::Impl)?;

        let trait_name = if let Token::Ident(n) = self.current() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected trait name after impl, found {:?}",
                self.current()
            ));
        };

        self.expect(Token::For)?;

        let type_name = if let Token::Ident(n) = self.current() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected type name after for, found {:?}",
                self.current()
            ));
        };

        self.expect(Token::LBrace)?;

        let mut methods = Vec::new();

        while self.current() != &Token::RBrace && self.current() != &Token::Eof {
            self.expect(Token::Fn)?;

            let method_name = if let Token::Ident(n) = self.current() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(format!(
                    "Expected method name in trait impl, found {:?}",
                    self.current()
                ));
            };

            self.expect(Token::LParen)?;

            self.expect(Token::Self_)?;
            let mut params = vec![("self".to_string(), Type::Void)];

            if self.current() != &Token::RParen {
                self.expect(Token::Comma)?;

                while self.current() != &Token::RParen {
                    let param_name = if let Token::Ident(n) = self.current() {
                        let name = n.clone();
                        self.advance();
                        name
                    } else {
                        return Err(format!(
                            "Expected parameter name, found {:?}",
                            self.current()
                        ));
                    };

                    self.expect(Token::Colon)?;
                    let param_type = self.parse_type()?;

                    params.push((param_name, param_type));

                    if self.current() == &Token::Comma {
                        self.advance();
                    }
                }
            }

            self.expect(Token::RParen)?;

            let ret_type = if self.current() == &Token::Arrow {
                self.advance();
                self.parse_type()?
            } else {
                Type::Void
            };

            self.expect(Token::LBrace)?;
            let body = self.parse_block()?;
            self.expect(Token::RBrace)?;

            methods.push(MethodDef {
                name: method_name,
                params,
                ret_type,
                body,
            });
        }

        self.expect(Token::RBrace)?;

        Ok(TraitImpl {
            trait_name,
            type_name,
            methods,
        })
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();

        while self.current() != &Token::RBrace && self.current() != &Token::Eof {
            stmts.push(self.parse_stmt()?);
        }

        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match self.current() {
            Token::Let => self.parse_let(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Return => self.parse_return(),
            _ => {
                let expr = self.parse_expr()?;

                if self.current() == &Token::Assign {
                    self.advance();
                    let value = self.parse_expr()?;
                    self.consume_semicolon();

                    if let Expr::FieldAccess { object, field } = expr {
                        return Ok(Stmt::FieldAssign {
                            object,
                            field,
                            value,
                        });
                    }

                    if let Expr::Var(var_name) = expr {
                        return Ok(Stmt::Assign {
                            var: var_name,
                            value,
                        });
                    }

                    return Err("Invalid assignment target".to_string());
                }

                self.consume_semicolon();
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_let(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Let)?;

        let mutable = if self.current() == &Token::Mut {
            self.advance();
            true
        } else {
            false
        };

        let var = if let Token::Ident(n) = self.current() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected variable name, found {:?}",
                self.current()
            ));
        };

        self.expect(Token::Assign)?;

        let value = self.parse_expr()?;

        self.consume_semicolon();

        Ok(Stmt::Let {
            var,
            mutable,
            value,
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, String> {
        self.expect(Token::If)?;

        let cond = self.parse_expr()?;

        self.expect(Token::LBrace)?;
        let then_block = self.parse_block()?;
        self.expect(Token::RBrace)?;

        let else_block = if self.current() == &Token::Else {
            self.advance();
            self.expect(Token::LBrace)?;
            let block = self.parse_block()?;
            self.expect(Token::RBrace)?;
            Some(block)
        } else {
            None
        };

        Ok(Stmt::If {
            cond,
            then_block,
            else_block,
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, String> {
        self.expect(Token::While)?;

        let cond = self.parse_expr()?;

        self.expect(Token::LBrace)?;
        let body = self.parse_block()?;
        self.expect(Token::RBrace)?;

        Ok(Stmt::While { cond, body })
    }

    fn parse_for(&mut self) -> Result<Stmt, String> {
        self.expect(Token::For)?;

        let var = if let Token::Ident(n) = self.current() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected variable name after 'for', found {:?}",
                self.current()
            ));
        };

        self.expect(Token::In)?;

        let iter = self.parse_expr()?;

        self.expect(Token::LBrace)?;
        let body = self.parse_block()?;
        self.expect(Token::RBrace)?;

        Ok(Stmt::For { var, iter, body })
    }

    fn parse_return(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Return)?;

        let expr = if self.current() == &Token::Semicolon {
            Expr::Literal(0)
        } else {
            self.parse_expr()?
        };

        self.consume_semicolon();

        Ok(Stmt::Return(expr))
    }

    fn consume_semicolon(&mut self) {
        if self.current() == &Token::Semicolon {
            self.advance();
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, min_prec: u8) -> Result<Expr, String> {
        let mut left = self.parse_unary_expr()?;

        loop {
            let op = match self.current() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                Token::Lt => BinOp::Lt,
                Token::Gt => BinOp::Gt,
                Token::Lte => BinOp::Lte,
                Token::Gte => BinOp::Gte,
                Token::Eq => BinOp::Eq,
                Token::Neq => BinOp::Neq,
                Token::And => BinOp::And,
                Token::Or => BinOp::Or,
                _ => break,
            };

            if op.precedence() < min_prec {
                break;
            }

            self.advance();

            let right = self.parse_binary_expr(op.precedence() + 1)?;

            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, String> {
        match self.current() {
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary {
                    op: UnOp::Neg,
                    expr: Box::new(expr),
                })
            }
            Token::Bang => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary {
                    op: UnOp::Not,
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary_expr()?;

        while self.current() == &Token::Dot {
            self.advance();

            let field = if let Token::Ident(n) = self.current() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(format!(
                    "Expected field name after dot, found {:?}",
                    self.current()
                ));
            };

            if self.current() == &Token::LParen {
                self.advance();

                let mut args = Vec::new();
                while self.current() != &Token::RParen {
                    args.push(self.parse_expr()?);

                    if self.current() == &Token::Comma {
                        self.advance();
                    }
                }

                self.expect(Token::RParen)?;

                expr = Expr::MethodCall {
                    receiver: Box::new(expr),
                    method: field,
                    args,
                };
            } else {
                expr = Expr::FieldAccess {
                    object: Box::new(expr),
                    field,
                };
            }
        }

        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, String> {
        match self.current() {
            Token::Int(n) => {
                let val = *n;
                self.advance();
                Ok(Expr::Literal(val))
            }
            Token::Ident(name) => {
                let ident = name.clone();
                self.advance();

                if self.current() == &Token::DoubleColon {
                    self.advance();

                    let func_name = if let Token::Ident(n) = self.current() {
                        let name = n.clone();
                        self.advance();
                        name
                    } else {
                        return Err(format!(
                            "Expected function name after ::, found {:?}",
                            self.current()
                        ));
                    };

                    self.expect(Token::LParen)?;

                    let mut args = Vec::new();
                    while self.current() != &Token::RParen {
                        args.push(self.parse_expr()?);

                        if self.current() == &Token::Comma {
                            self.advance();
                        }
                    }

                    self.expect(Token::RParen)?;

                    return Ok(Expr::AssociatedCall {
                        type_name: ident,
                        function: func_name,
                        args,
                    });
                }

                if self.current() == &Token::LBrace {
                    self.advance();
                    let mut fields = Vec::new();

                    while self.current() != &Token::RBrace {
                        let field_name = if let Token::Ident(n) = self.current() {
                            let name = n.clone();
                            self.advance();
                            name
                        } else {
                            return Err(format!("Expected field name, found {:?}", self.current()));
                        };

                        self.expect(Token::Colon)?;
                        let field_value = self.parse_expr()?;

                        fields.push((field_name, field_value));

                        if self.current() == &Token::Comma {
                            self.advance();
                        }
                    }

                    self.expect(Token::RBrace)?;

                    Ok(Expr::StructInit {
                        struct_name: ident,
                        fields,
                    })
                } else if self.current() == &Token::LParen {
                    self.advance();
                    let mut args = Vec::new();

                    while self.current() != &Token::RParen {
                        args.push(self.parse_expr()?);

                        if self.current() == &Token::Comma {
                            self.advance();
                        }
                    }

                    self.expect(Token::RParen)?;

                    Ok(Expr::Call { func: ident, args })
                } else {
                    Ok(Expr::Var(ident))
                }
            }
            Token::If => self.parse_if_expr(),
            Token::Match => self.parse_match_expr(),
            Token::Self_ => {
                self.advance();
                Ok(Expr::Self_)
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(format!(
                "Unexpected token in expression: {:?}",
                self.current()
            )),
        }
    }

    fn parse_match_expr(&mut self) -> Result<Expr, String> {
        self.expect(Token::Match)?;

        let expr = self.parse_expr()?;

        self.expect(Token::LBrace)?;

        let mut arms = Vec::new();
        while self.current() != &Token::RBrace && self.current() != &Token::Eof {
            let variant = if let Token::Ident(n) = self.current() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(format!(
                    "Expected variant name in match arm, found {:?}",
                    self.current()
                ));
            };

            let binding = if self.current() == &Token::LParen {
                self.advance();
                let binding = if let Token::Ident(n) = self.current() {
                    let name = n.clone();
                    self.advance();
                    Some(name)
                } else {
                    None
                };
                self.expect(Token::RParen)?;
                binding
            } else {
                None
            };

            self.expect(Token::Arrow)?;

            let body = self.parse_expr()?;

            arms.push((variant, binding, body));

            if self.current() == &Token::Comma {
                self.advance();
            }
        }

        self.expect(Token::RBrace)?;

        Ok(Expr::Match {
            expr: Box::new(expr),
            arms,
        })
    }

    fn parse_if_expr(&mut self) -> Result<Expr, String> {
        self.expect(Token::If)?;

        let cond = self.parse_expr()?;

        self.expect(Token::LBrace)?;
        let then_stmts = self.parse_block()?;
        let then_val = self.block_to_expr(then_stmts)?;
        self.expect(Token::RBrace)?;

        self.expect(Token::Else)?;
        self.expect(Token::LBrace)?;
        let else_stmts = self.parse_block()?;
        let else_val = self.block_to_expr(else_stmts)?;
        self.expect(Token::RBrace)?;

        Ok(Expr::If {
            cond: Box::new(cond),
            then_val: Box::new(then_val),
            else_val: Box::new(else_val),
        })
    }

    fn block_to_expr(&self, stmts: Vec<Stmt>) -> Result<Expr, String> {
        if let Some(Stmt::Return(expr)) = stmts.last() {
            Ok(expr.clone())
        } else if let Some(Stmt::Expr(expr)) = stmts.last() {
            Ok(expr.clone())
        } else {
            Ok(Expr::Literal(0))
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}
