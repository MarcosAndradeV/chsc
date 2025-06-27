use std::collections::HashMap;

use crate::{
    chslexer::{Token, TokenKind},
    parser::ParseResult,
};

#[derive(Debug, Default)]
pub struct Program<'src> {
    pub externs: Vec<Extern<'src>>,
    pub funcs: Vec<Func<'src>>,
}

#[derive(Debug)]
pub enum Names {
    Var(VarId),
    Extern,
}

#[derive(Debug, Default)]
pub struct NameSpace<'src>(pub Vec<HashMap<&'src str, Names>>);
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(pub usize);
impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Default, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarId(pub usize);

#[derive(Debug)]
pub enum Extern<'src> {
    Symbol(Token<'src>),
    Func(Func<'src>),
    Var(Var<'src>),
}
impl<'src> Extern<'src> {
    pub fn name(&self) -> &'src str {
        match self {
            Extern::Symbol(token) => token.source,
            Extern::Func(func) => func.name.source,
            Extern::Var(var) => var.token.source,
        }
    }
}

#[derive(Debug)]
pub struct Var<'src> {
    pub token: Token<'src>,
    pub ty: Option<Type<'src>>,
    pub used: bool,
}

impl<'src> Var<'src> {
    pub fn new(token: Token<'src>) -> Self {
        Self {
            token,
            ty: None,
            used: false,
        }
    }
}

#[derive(Debug)]
pub struct Func<'src> {
    pub name: Token<'src>,
    pub args: Vec<(Token<'src>, Type<'src>)>,
    pub ret_type: Option<Type<'src>>,

    pub vars: Vec<Var<'src>>,
    pub block_count: usize,
    pub body: Vec<Stmt<'src>>,
}

#[derive(Debug, Clone)]
pub enum Type<'src> {
    Name(Token<'src>),
}

#[derive(Debug)]
pub enum Stmt<'src> {
    Assign {
        lhs: Expr<'src>,
        rhs: Expr<'src>,
    },
    Return(Option<Expr<'src>>),
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
        caller: Expr<'src>,
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
    IntLit(Loc<'src>, u64),
    StrLit(Token<'src>),
    Var(Token<'src>, VarId),

    Global(Token<'src>),

    Deref(Token<'src>, VarId),
    Ref(Token<'src>, VarId),
}

impl<'src> PartialEq for Expr<'src> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::IntLit(_, l1), Self::IntLit(_, r1)) => l1 == r1,
            (Self::StrLit(l0), Self::StrLit(r0)) => l0.source == r0.source,
            (Self::Var(_, l1), Self::Var(_, r1)) => l1 == r1,
            (Self::Global(l0), Self::Global(r0)) => l0.source == r0.source,
            (Self::Deref(_, l1), Self::Deref(_, r1)) => l1 == r1,
            (Self::Ref(_, l1), Self::Ref(_, r1)) => l1 == r1,
            _ => false,
        }
    }
}

impl<'src> std::fmt::Display for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::IntLit(_, lit) => write!(f, "{lit}"),
            Expr::StrLit(_) => write!(f, "@str"),
            Expr::Var(token, VarId(id)) => write!(f, "Var({})", id),
            Expr::Global(token) => write!(f, "{token}"),
            Expr::Deref(token, var_id) => write!(f, "{token}^"),
            Expr::Ref(token, var_id) => write!(f, "&{token}"),
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
    pub fn from_token_kind(kind: &TokenKind) -> Precedence {
        use TokenKind::*;

        match kind {
            Assign => Self::Assignment,

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

            _ => Self::Lowest,
        }
    }
}

use crate::*;

pub fn print_program(program: &Program) {
    for ext in &program.externs {
        match ext {
            Extern::Symbol(tok) => {
                println!("extern symbol: {}", tok);
            }
            Extern::Func(func) => {
                println!("extern func:");
                print_func(func);
            }
            Extern::Var(var) => {
                println!("extern var: {}{:?}", var.token, var.ty);
            }
        }
    }

    for func in &program.funcs {
        print_func(func);
    }
}

fn print_func(func: &Func) {
    print!("fn {}(", func.name.source);
    for (i, (arg_name, arg_ty)) in func.args.iter().enumerate() {
        print!("    {}: {:?}", arg_name, arg_ty);
        if i < func.args.len() - 1 {
            println!(",");
        } else {
            println!();
        }
    }
    println!(") -> {:?} {{", func.ret_type);

    for stmt in &func.body {
        print_stmt(&func.vars, stmt);
    }

    println!("}}");
}

fn print_stmt(vars: &[Var<'_>], stmt: &Stmt) {
    match stmt {
        Stmt::Assign { lhs, rhs } => {
            if let Expr::Var(_, var_id) = lhs {
                if vars[var_id.0].used {
                    println!("   [x] {} = {};", lhs, rhs);
                } else {
                    println!("   [ ] {} = {};", lhs, rhs);
                }
            } else {
                println!("    {} = {};", lhs, rhs);
            }
        }
        Stmt::Return(expr) => match expr {
            Some(e) => println!("    return {};", e),
            None => println!("    return;"),
        },
        Stmt::Unop {
            result,
            operator,
            operand,
        } => {
            println!(
                "   [{}] Var({}) = {}{};",
                if vars[result.0].used { 'x' } else { ' ' },
                result.0,
                operator,
                operand
            );
        }
        Stmt::Binop {
            result,
            operator,
            lhs,
            rhs,
        } => {
            println!(
                "   [{}] Var({}) = {} {} {};",
                if vars[result.0].used { 'x' } else { ' ' },
                result.0,
                lhs,
                operator,
                rhs
            );
        }
        Stmt::Funcall {
            result,
            caller,
            args,
        } => {
            if let Some(res) = result {
                print!(
                    "   [{}] Var({}) = ",
                    if vars[res.0].used { 'x' } else { ' ' },
                    res.0
                );
            } else {
                print!("    ");
            }
            print!("{}(", caller);
            for (i, arg) in args.iter().enumerate() {
                print!("{}", arg);
                if i < args.len() - 1 {
                    print!(", ");
                }
            }
            println!(");");
        }
        Stmt::Syscall { result, args } => {
            if let Some(res) = result {
                print!(
                    "   [{}] Var({}) = ",
                    if vars[res.0].used { 'x' } else { ' ' },
                    res.0
                );
            } else {
                print!("    ");
            }
            for (i, arg) in args.iter().enumerate() {
                print!("{}", arg);
                if i < args.len() - 1 {
                    print!(", ");
                }
            }
            println!(");");
        }
        Stmt::JZ(cond, block) => {
            println!("    if !{} goto block_{};", cond, block);
        }
        Stmt::Jmp(block) => {
            println!("    goto block_{};", block);
        }
        Stmt::Block(block) => {
            println!("block_{}:", block);
        }
    }
}
