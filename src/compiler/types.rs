use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I64,
    Void,
    Struct(String),
    Enum(String),
    Self_,
}

impl Type {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "i64" => Some(Type::I64),
            "void" => Some(Type::Void),
            "Self" => Some(Type::Self_),

            _ => None,
        }
    }

    pub fn from_ident(s: String, structs: &[StructDef], enums: &[EnumDef]) -> Option<Self> {
        match s.as_str() {
            "i64" => Some(Type::I64),
            "void" => Some(Type::Void),
            _ => {
                if structs.iter().any(|st| st.name == s) {
                    Some(Type::Struct(s))
                } else if enums.iter().any(|en| en.name == s) {
                    Some(Type::Enum(s))
                } else {
                    None
                }
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I64 => write!(f, "i64"),
            Type::Void => write!(f, "void"),
            Type::Struct(name) => write!(f, "struct {}", name),
            Type::Enum(name) => write!(f, "enum {}", name),
            Type::Self_ => write!(f, "Self"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
    And,
    Or,
}

impl BinOp {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "+" => Some(BinOp::Add),
            "-" => Some(BinOp::Sub),
            "*" => Some(BinOp::Mul),
            "/" => Some(BinOp::Div),
            "%" => Some(BinOp::Mod),
            "<" => Some(BinOp::Lt),
            ">" => Some(BinOp::Gt),
            "<=" => Some(BinOp::Lte),
            ">=" => Some(BinOp::Gte),
            "==" => Some(BinOp::Eq),
            "!=" => Some(BinOp::Neq),
            "&&" => Some(BinOp::And),
            "||" => Some(BinOp::Or),
            _ => None,
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            BinOp::Or => 1,
            BinOp::And => 2,
            BinOp::Eq | BinOp::Neq => 3,
            BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => 4,
            BinOp::Add | BinOp::Sub => 5,
            BinOp::Mul | BinOp::Div | BinOp::Mod => 6,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

impl UnOp {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "-" => Some(UnOp::Neg),
            "!" => Some(UnOp::Not),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(i64),
    Var(String),
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
    },
    Call {
        func: String,
        args: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_val: Box<Expr>,
        else_val: Box<Expr>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    StructInit {
        struct_name: String,
        fields: Vec<(String, Expr)>,
    },
    EnumVariant {
        enum_name: String,
        variant: String,
        value: Option<Box<Expr>>,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<(String, Option<String>, Expr)>,
    },
    MethodCall {
        receiver: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    AssociatedCall {
        type_name: String,
        function: String,
        args: Vec<Expr>,
    },
    Self_,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        var: String,
        mutable: bool,
        value: Expr,
    },
    Assign {
        var: String,
        value: Expr,
    },
    FieldAssign {
        object: Box<Expr>,
        field: String,
        value: Expr,
    },
    Expr(Expr),
    Return(Expr),
    If {
        cond: Expr,
        then_block: Vec<Stmt>,
        else_block: Option<Vec<Stmt>>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    For {
        var: String,
        iter: Expr,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret_type: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct MethodDef {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret_type: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub methods: Vec<MethodDef>,
    pub associated_functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<(String, Option<Type>)>,
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub name: String,
    pub methods: Vec<TraitMethod>,
}

#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret_type: Type,
}

#[derive(Debug, Clone)]
pub struct TraitImpl {
    pub trait_name: String,
    pub type_name: String,
    pub methods: Vec<MethodDef>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub functions: Vec<Function>,
    pub traits: Vec<TraitDef>,
    pub trait_impls: Vec<TraitImpl>,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub typ: Type,
    pub mutable: bool,
    pub stack_offset: usize,
}

pub struct CompileContext {
    pub bindings: Vec<Binding>,
    pub current_function: Option<String>,
    pub max_locals: usize,
}

impl CompileContext {
    pub fn new(max_locals: usize) -> Self {
        CompileContext {
            bindings: Vec::new(),
            current_function: None,
            max_locals,
        }
    }

    pub fn push_binding(
        &mut self,
        name: String,
        typ: Type,
        mutable: bool,
    ) -> Result<usize, String> {
        if self.bindings.len() >= self.max_locals {
            return Err(format!(
                "Too many local variables (max: {})",
                self.max_locals
            ));
        }

        let stack_offset = 8 + (self.bindings.len() * 8);
        self.bindings.push(Binding {
            name: name.clone(),
            typ,
            mutable,
            stack_offset,
        });

        Ok(stack_offset)
    }

    pub fn find_binding(&self, name: &str) -> Option<&Binding> {
        self.bindings.iter().rev().find(|b| b.name == name)
    }

    pub fn clear_bindings(&mut self) {
        self.bindings.clear();
    }

    pub fn frame_size(&self) -> usize {
        8 + (self.bindings.len() * 8)
    }
}
