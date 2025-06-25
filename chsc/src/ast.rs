use std::collections::HashMap;

use crate::{chslexer::{Token, TokenKind}, parser::ParseResult};

#[derive(Debug, Default)]
pub struct Program<'src> {
    pub externs: Vec<Extern<'src>>,
    pub funcs: Vec<Func<'src>>,
}

#[derive(Debug, Default)]
pub struct VarsIndex<'src>(pub Vec<HashMap<&'src str, VarId>>);
impl<'src> VarsIndex<'src> {
    pub fn push_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        assert!(self.0.len() != 1);
        self.0.pop();
    }

    pub fn insert_var_index(&mut self, k: &'src str, v: VarId) -> Option<VarId> {
        self.0.last_mut().and_then(|scope| scope.insert(k, v))
    }

    pub fn get_var_index(&self, k: &'src str) -> Option<&VarId> {
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarId(pub usize, pub bool);
impl VarId {
    pub fn fetch<'src>(&self, exprs: &'src [Var<'src>]) -> Option<&Var<'src>> {
        exprs.get(self.0)
    }

    pub fn unwrap_fetch<'f, 'src>(&self, exprs: &'f [Var<'src>]) -> &'f Var<'src> {
        exprs.get(self.0).unwrap()
    }
}

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
    IntLit(Token<'src>),
    StrLit(Token<'src>),
    Var(Token<'src>, VarId),
    Deref(Token<'src>, VarId),
}

#[derive(Debug, Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    Assignment,
    LogicalOr,
    LogicalAnd,
    Equality,
    Comparison,
    Sum,
    Factor,
    Deref,
    Call
}

impl Precedence {
    pub fn from_token_kind(kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::Plus | TokenKind::Minus => Self::Sum,
            TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => Self::Factor,
            TokenKind::Lt | TokenKind::Gt => Self::Comparison,
            TokenKind::Eq => Self::Equality,
            _ => Self::Lowest
        }
    }
}
