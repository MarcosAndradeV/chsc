use std::collections::HashMap;

use crate::chslexer::{Loc, Token, TokenKind};

#[derive(Default)]
pub struct Module<'src> {
    pub name: Token<'src>,
    // pub name_space: HashMap<&'src str, Name>,
    pub funcs: Vec<Func<'src>>,
    pub externs: Vec<ExternFunc<'src>>,
    pub global_vars: Vec<GlobalVar<'src>>,
    // imports
}

impl<'src> Module<'src> {
    pub fn add_fn(&mut self, r#fn: Func<'src>) {
        // self.name_space
        //     .insert(r#fn.name.source, Name::Func(self.funcs.len()));
        self.funcs.push(r#fn);
    }

    pub fn add_extern_fn(&mut self, r#extern: ExternFunc<'src>) {
        // self.name_space
        //     .insert(r#extern.name.source, Name::ExternFn(self.externs.len()));
        self.externs.push(r#extern);
    }
    pub fn add_global_vars(&mut self, var: GlobalVar<'src>) {
        // self.name_space
        //     .insert(var.name.source, Name::GlobalVar(self.global_vars.len()));
        self.global_vars.push(var);
    }
}

pub enum Name {
    GlobalVar(usize),
    Func(usize),
    ExternFn(usize),
}

pub struct GlobalVar<'src> {
    pub name: Token<'src>,
    pub is_vec: Option<usize>,
    pub r#type: Type<'src>,
    pub expr: Option<Expr<'src>>,
}

pub struct ExternFunc<'src> {
    pub name: Token<'src>,
    pub args: Vec<Type<'src>>,
    pub ret_type: Option<Type<'src>>,
    pub is_variadic: bool,
}

pub struct Func<'src> {
    pub name: Token<'src>,
    pub args: Vec<(Token<'src>, Type<'src>)>,
    pub ret_type: Option<Type<'src>>,

    pub body: Vec<Stmt<'src>>,
}

#[derive(Clone)]
pub enum Type<'src> {
    Name(Token<'src>),
    PtrTo(Box<Self>),
}

pub enum Stmt<'src> {
    Return {
        loc: Loc<'src>,
        expr: Option<Expr<'src>>,
    },
    Expr {
        loc: Loc<'src>,
        expr: Expr<'src>,
    },
    VarDecl {
        name: Token<'src>,
        is_vec: Option<usize>,
        r#type: Type<'src>,
        expr: Option<Expr<'src>>,
    },
    Assign {
        loc: Loc<'src>,
        lhs: Expr<'src>,
        rhs: Expr<'src>,
    },
    While {
        loc: Loc<'src>,
        cond: Expr<'src>,
        body: Box<Self>,
    },
    If {
        loc: Loc<'src>,
        cond: Expr<'src>,
        true_branch: Box<Self>,
        else_branch: Option<Box<Self>>,
    },
    Block(Loc<'src>, Vec<Self>),
}

pub enum Expr<'src> {
    IntLit(Token<'src>),
    StrLit(Token<'src>),
    Ident(Token<'src>),
    Deref(Loc<'src>, Box<Self>),
    Ref(Loc<'src>, Box<Self>),
    Call {
        loc: Loc<'src>,
        caller: Box<Self>,
        args: Vec<Self>,
    },
    Syscall {
        loc: Loc<'src>,
        args: Vec<Self>,
    },
    Binop {
        operator: Token<'src>,
        lhs: Box<Expr<'src>>,
        rhs: Box<Expr<'src>>,
    },
    Index {
        loc: Loc<'src>,
        base: Box<Expr<'src>>,
        index: Box<Expr<'src>>,
    }
}


#[derive(Debug, Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,

    // Assignment,       // =, +=, -=, etc.
    LogicalOr,    // ||
    LogicalAnd,   // &&
    BitWiseOr,    // |
    BitWiseXor,   // ^
    BitWiseAnd,   // &
    Equality,     // ==, !=
    Comparison,   // <, >, <=, >=
    BitWiseShift, // <<, >>
    Sum,          // +, -
    Factor,       // *, /, %
    RefDeref,     // &, *
    NegNot,       // !, - (unary)
    Call,         // function calls, array indexing, field access

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
