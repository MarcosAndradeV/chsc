use std::collections::HashMap;

use crate::{
    chslexer::{Token, TokenKind},
};

#[derive(Debug, Default)]
pub struct Program<'src> {
    pub externs: Vec<ExternFunc<'src>>,
    pub funcs: Vec<Func<'src>>,
}

#[derive(Debug)]
pub enum Names {
    ExternFunc(usize),
    Func(usize),
    Var(VarId),
}

#[derive(Debug, Default)]
pub struct NameSpace<'src>(pub Vec<HashMap<&'src str, Names>>);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(pub usize);

#[derive(Debug, Default, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarId(pub usize);

#[derive(Debug)]
pub struct ExternFunc<'src> {
    pub name: Token<'src>,
    pub args: Vec<Type>,
    pub is_variadic: bool,
    pub ret: Type,
}

#[derive(Debug)]
pub struct Var<'src> {
    pub token: Token<'src>,
    pub ty: Type,
}

#[derive(Debug, Default)]
pub struct Func<'src> {
    pub name: Token<'src>,
    pub args: Vec<Token<'src>>,
    pub args_types: Vec<Type>,
    pub ret_type: Type,

    pub vars: Vec<Var<'src>>,
    pub block_count: usize,
    pub body: Vec<Stmt<'src>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Type {
    #[default]
    Void,

    Int,
    Bool,
    Char,

    Ptr,
    PtrTo(Box<Self>),

    Array(usize, Box<Self>),
}

#[derive(Debug)]
pub enum Stmt<'src> {
    Store {
        target: Expr<'src>,
        rhs: Expr<'src>,
    },
    AssignVar {
        var: (Token<'src>, VarId),
        rhs: Expr<'src>,
    },
    Return(Loc<'src>, Option<Expr<'src>>),
    Unop {
        result: VarId,
        operator: Token<'src>,
        operand: Expr<'src>,
    },
    Binop {
        result: VarId,
        operator: Token<'src>,
        lhs: Expr<'src>,
        rhs: Expr<'src>,
    },
    Funcall {
        result: Option<VarId>,
        caller: Token<'src>,
        args: Vec<Expr<'src>>,
    },
    Syscall {
        result: Option<VarId>,
        args: Vec<Expr<'src>>,
    },
    JZ(Expr<'src>, BlockId),
    Jmp(BlockId),
    Block(BlockId),
}

#[derive(Debug)]
pub enum Expr<'src> {
    Void(Loc<'src>),
    IntLit(Loc<'src>, u64),
    StrLit(Token<'src>),

    Var(Token<'src>, VarId),
    Global(Token<'src>),

    Deref(Token<'src>, VarId),
    Ref(Token<'src>, VarId),

    Cast(Loc<'src>, Type, Box<Self>),

    Index {
        loc: Loc<'src>,
        base: Box<Self>,
        index: Box<Self>,
    },
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'src> NameSpace<'src> {
    pub fn push_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        assert!(self.0.len() != 1);
        self.0.pop();
    }

    pub fn insert_var_index(&mut self, k: &'src str, v: Names) -> Option<Names> {
        self.0.last_mut().and_then(|scope| scope.insert(k, v))
    }

    pub fn get_var_index(&self, k: &'src str) -> Option<&Names> {
        for scope in self.0.iter().rev() {
            let v = scope.get(k);
            if v.is_some() {
                return v;
            }
        }
        None
    }
}

impl<'src> Var<'src> {
    pub fn new(token: Token<'src>, ty: Type) -> Self {
        Self { token, ty }
    }
}

impl<'src> Expr<'src> {
    pub fn loc(&self) -> Loc<'src> {
        match self {
            Expr::Void(loc) => *loc,
            Expr::IntLit(loc, _) => *loc,
            Expr::StrLit(token) => token.loc,
            Expr::Var(token, _) => token.loc,
            Expr::Global(token) => token.loc,
            Expr::Deref(token, _) => token.loc,
            Expr::Ref(token, _) => token.loc,
            Expr::Cast(loc, ..) => *loc,
            Expr::Index { loc, .. } => *loc,
        }
    }
}

impl<'src> std::fmt::Display for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Void(_) => write!(f, "void"),
            Expr::IntLit(_, lit) => write!(f, "{lit}"),
            Expr::StrLit(_) => write!(f, "@str"),
            Expr::Var(_, VarId(id)) => write!(f, "Var({id})"),
            Expr::Global(token) => write!(f, "{token}"),
            Expr::Deref(_, VarId(id)) => write!(f, "Deref({id})"),
            Expr::Ref(_, VarId(id)) => write!(f, "Ref({id})"),
            Expr::Cast(_, ty, e) => write!(f, "Cast({ty}){e}"),
            Expr::Index { loc, base, index } => write!(f, "Index({base})[{index}]"),
        }
    }
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Void => todo!("Cannot get size of void"),
            Type::Array(size, ty) => ty.size() * size,
            Type::Char => 1,
            Type::Bool => 4,
            Type::Int => 4,
            _ => 8,
        }
    }
    pub fn get_inner(&self) -> &Self {
        match self {
            Type::Array(_, ty) => ty.as_ref(),
            Type::PtrTo(ty) => ty.as_ref(),
            _ => self,
        }
    }
}

impl<'src> std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Ptr => write!(f, "ptr"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::Void => write!(f, "void"),
            Self::PtrTo(ty) => write!(f, "*{ty}"),
            Self::Array(size, ty) => write!(f, "[{size}]{ty}"),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    Assignment,
    LogicalOr,
    LogicalAnd,
    Equality,
    Comparison,
    BitWiseOr,
    BitWiseXor,
    BitWiseAnd,
    BitWiseShift,
    Sum,
    Factor,
    RefDeref,
    NegNot,
    Call,
    Highest,
}

impl Precedence {
    pub fn from_token_kind(kind: &TokenKind) -> Option<Precedence> {
        use TokenKind::*;

        let r = match kind {
            // Assign => Self::Assignment,
            DoublePipe => Self::LogicalOr,
            DoubleAmpersand => Self::LogicalAnd,

            Eq | NotEq => Self::Equality,
            Lt | Gt | LtEq | GtEq => Self::Comparison,

            Pipe => Self::BitWiseOr,
            Caret => Self::BitWiseXor,
            Ampersand => Self::BitWiseAnd,
            ShiftLeft | ShiftRight => Self::BitWiseShift,

            Plus | Minus => Self::Sum,
            Asterisk | Slash | Percent => Self::Factor,

            Bang | Tilde => Self::NegNot,

            OpenParen | OpenBracket => Self::Call,

            Caret | Ampersand => Self::RefDeref,

            _ => return None,
        };
        Some(r)
    }
}

use crate::*;

pub fn print_program(program: &Program) {
    for ext in &program.externs {
        let ExternFunc {
            name,
            args,
            is_variadic,
            ret,
        } = &ext;
        print!("extern fn {}(", name);
        for (i, arg) in args.iter().enumerate() {
            print!("{}", arg);
            if i < args.len() - 1 {
                print!(", ");
            }
        }
        if *is_variadic {
            print!(", ...");
        }
        print!(")");
        print!("-> {}", ret);
        println!(";");
    }

    for func in &program.funcs {
        print_func(func);
    }
}

fn print_func(func: &Func) {
    print!("fn {}(", func.name.source);
    for (i, _) in func.args.iter().enumerate() {
        print!("Var({i})");
        if i < func.args.len() - 1 {
            print!(", ");
        }
    }
    println!(") -> {:?} {{", func.ret_type);

    for stmt in &func.body {
        print_stmt(&func.vars, stmt);
    }

    println!("}}");
}

fn print_stmt(vars: &[Var<'_>], stmt: &Stmt) {
    print!("    ");
    match stmt {
        Stmt::Store { target, rhs } => {
            println!("Store({}) = {};", target, rhs);
        }
        Stmt::AssignVar { var, rhs } => {
            println!("Var({}) = {};", var.1.0, rhs);
        }
        Stmt::Return(_, expr) => match expr {
            Some(e) => println!("return {};", e),
            None => println!("return;"),
        },
        Stmt::Unop {
            result,
            operator,
            operand,
        } => {
            println!("Var({}) = {}{};", result.0, operator, operand);
        }
        Stmt::Binop {
            result,
            operator,
            lhs,
            rhs,
        } => {
            println!("Var({}) = {} {} {};", result.0, lhs, operator, rhs);
        }
        Stmt::Funcall {
            result,
            caller,
            args,
        } => {
            if let Some(res) = result {
                print!("Var({}) = ", res.0);
            }
            print!("{}(", caller);
            for (i, arg) in args.iter().enumerate() {
                match arg {
                    Expr::Var(_, var_id) => {
                        print!("Var({})", var_id.0);
                    }
                    Expr::Deref(_, var_id) => {
                        print!("Var({})^", var_id.0);
                    }
                    _ => {
                        print!("{}", arg);
                    }
                }
                if i < args.len() - 1 {
                    print!(", ");
                }
            }
            println!(");");
        }
        Stmt::Syscall { result, args } => {
            if let Some(res) = result {
                print!("Var({}) = ", res.0);
            }
            print!("syscall(");
            for (i, arg) in args.iter().enumerate() {
                print!("{}", arg);
                if i < args.len() - 1 {
                    print!(", ");
                }
            }
            println!(");");
        }
        Stmt::JZ(cond, block) => {
            println!("if !{} goto block_{};", cond, block);
        }
        Stmt::Jmp(block) => {
            println!("goto block_{};", block);
        }
        Stmt::Block(block) => {
            println!("block_{}:", block);
        }
    }
}
