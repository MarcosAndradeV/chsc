#![allow(dead_code)]
#![allow(unused)]

use std::{
    collections::HashMap, env::args, ffi::OsStr, fmt::Display, fs, path::PathBuf, process::Command,
};

use crate::chslexer::*;

use crate::fasm_backend::{DataDef, DataDirective, DataExpr, Function, Module, Register};

mod fasm_backend;
mod chslexer;

fn main() {
    let mut args = args();
    let program = args.next().expect("program");

    let Some(file_path) = args.next() else {
        eprintln!("Usage: {program} <input>");
        return;
    };

    let Ok(source) = fs::read_to_string(&file_path) else {
        eprintln!("Cannot read file `{file_path}`");
        return;
    };

    let mut l = PeekableLexer::new(&file_path, &source);
    l.set_is_keyword_fn(|k| matches!(k, "fn" | "var" | "return" | "extern"));
    let mut ctx = CompilerContext::new(&mut l);
    let Some(p) = compile(&mut ctx) else {
        return;
    };

    let Some(m) = generate(p) else {
        return;
    };

    let asm_path = PathBuf::from(&file_path).with_extension("asm");
    let o_path = PathBuf::from(&file_path).with_extension("o");
    let exe_path = PathBuf::from(&file_path).with_extension("");
    let _ = fs::write(&asm_path, m.to_string());

    let Ok(_) = run_fasm(&asm_path, &o_path) else {
        return;
    };

    let Ok(_) = run_cc(&o_path, &exe_path, ["-no-pie"]) else {
        return;
    };
}

fn run_cc<I, O, A, F>(input_path: I, output_path: O, compiler_flags: F) -> Result<(), ()>
where
    I: AsRef<OsStr>,
    O: AsRef<OsStr>,
    A: AsRef<OsStr>,
    F: IntoIterator<Item = A>,
{
    let mut cc_command = Command::new("cc");
    cc_command.arg("-o").arg(output_path).arg(input_path);

    cc_command.args(compiler_flags);

    let output = cc_command
        .output()
        .map_err(|e| eprintln!("Failed to run cc: {}", e))?;
    if !output.status.success() {
        eprintln!(
            "cc failed to generate\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        return Err(());
    }
    Ok(())
}

fn run_fasm<I: AsRef<OsStr>, O: AsRef<OsStr>>(input_path: I, output_path: O) -> Result<(), ()> {
    let mut command = Command::new("fasm");
    command.arg(input_path).arg(output_path);

    let output = command
        .output()
        .map_err(|e| eprintln!("Failed to run fasm: {}", e))?;
    if !output.status.success() {
        eprintln!(
            "fasm failed to generate\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        return Err(());
    }
    Ok(())
}

struct CompilerContext<'src> {
    lexer: &'src mut PeekableLexer<'src>,
}

impl<'src> CompilerContext<'src> {
    fn new(lexer: &'src mut PeekableLexer<'src>) -> Self {
        Self { lexer }
    }
}

fn compile<'src>(ctx: &mut CompilerContext<'src>) -> Option<Program<'src>> {
    let mut p = Program::default();

    loop {
        let token = ctx.lexer.next_token();

        if token.is_eof() {
            return Some(p);
        }

        match token.kind {
            TokenKind::Keyword if token.source == "fn" => compile_func(ctx, &mut p)?,
            TokenKind::Keyword if token.source == "extern" => {
                let fname = expect_token(ctx, TokenKind::Identifier)?;
                p.extern_funcs.push(fname);
            }
            _ => {
                eprintln!("{}: Unexpected token {}", token.loc, token);
                return None;
            }
        }
    }
    Some(p)
}

fn compile_func<'src>(ctx: &mut CompilerContext<'src>, p: &mut Program<'src>) -> Option<()> {
    let fname = expect_token(ctx, TokenKind::Identifier)?;
    let mut f = Func::new(fname);
    expect_token(ctx, TokenKind::OpenParen)?;
    expect_token(ctx, TokenKind::CloseParen)?;
    expect_token(ctx, TokenKind::OpenBrace)?;

    loop {
        let token = ctx.lexer.next_token();
        if token.is_eof() {
            eprintln!("{}: Unexpected EOF", token.loc);
            return None;
        }
        match token.kind {
            TokenKind::CloseBrace => break,
            TokenKind::Keyword if token.source == "var" => {
                let name = expect_token(ctx, TokenKind::Identifier)?;
                let offset = f.vars_table.len();
                if f.vars_table.insert(name.source, offset).is_some() {
                    eprintln!("{}: Redefinition of var {}", name.loc, name);
                    return None;
                }
                expect_token(ctx, TokenKind::SemiColon)?;
            }
            TokenKind::Keyword if token.source == "return" => {
                if ctx.lexer.peek_token().kind != TokenKind::SemiColon {
                    let expr = compile_expr(ctx, &mut f, Precedence::Lowest)?;
                    f.body.push(Stmt::Return(Some(expr)));
                } else {
                    f.body.push(Stmt::Return(None));
                }
                expect_token(ctx, TokenKind::SemiColon)?;
            }
            TokenKind::Identifier if ctx.lexer.peek_token().kind == TokenKind::Assign => {
                ctx.lexer.next_token();

                let lhs = compile_expr(ctx, &mut f, Precedence::Lowest)?;

                if let Some(offset) = f.vars_table.get(token.source) {
                    f.body.push(Stmt::Assign {
                        rhs: LValue::Var(token, *offset),
                        lhs,
                    });
                } else {
                    eprintln!("{}: Undefined var {}", token.loc, token);
                    return None;
                }
                expect_token(ctx, TokenKind::SemiColon)?;
            }
            TokenKind::Identifier if ctx.lexer.peek_token().kind == TokenKind::OpenParen => {
                ctx.lexer.next_token();
                let mut args = vec![];
                loop {
                    let val_token = ctx.lexer.peek_token();
                    match val_token.kind {
                        TokenKind::CloseParen => break,
                        TokenKind::Comma => {
                            ctx.lexer.next_token();
                        }
                        _ => args.push(compile_expr(ctx, &mut f, Precedence::Lowest)?),
                    }
                }
                ctx.lexer.next_token();
                expect_token(ctx, TokenKind::SemiColon)?;
                f.body.push(Stmt::Funcall(token, args));
            }
            _ => {
                eprintln!("{}: Expected Stmt but got {}", token.loc, token);
                return None;
            }
        }
    }
    p.funcs.push(f);
    Some(())
}

fn compile_expr<'src>(
    ctx: &mut CompilerContext<'src>,
    f: &mut Func<'src>,
    precedence: Precedence,
) -> Option<Expr<'src>> {
    let token = ctx.lexer.next_token();
    let mut left = match token.kind {
        TokenKind::IntegerNumber => Expr::IntLit(token),
        TokenKind::StringLiteral => Expr::StrLit(token),
        TokenKind::Identifier => {
            if let Some(offset) = f.vars_table.get(token.source) {
                Expr::Var(token, *offset)
            } else {
                eprintln!("{}: Undefined var {}", token.loc, token);
                return None;
            }
        }
        _ => todo!(),
    };
    while !next_token_is(ctx, &[TokenKind::SemiColon])? && precedence < peek_infix_precedence(ctx) {
        let token = ctx.lexer.next_token();
        let infix = {
            let precedence = Precedence::from_token_kind(&token.kind);
            let right = compile_expr(ctx, f, precedence)?;
            let id = f.temp_count;
            f.temp_count += 1;
            f.body.push(Stmt::Binop {
                loc: token.loc,
                result: id,
                operator: BinaryOperator::from_token(&token)?,
                rhs: right,
                lhs: left,
            });
            Expr::Temp(id)
        };
        left = infix
    }
    Some(left)
}

fn generate(p: Program) -> Option<Module> {
    let mut m = Module::new(true);

    for name in p.extern_funcs {
        m.push_extrn(name.source);
    }

    for func in p.funcs {
        generate_func(func, &mut m)?
    }

    Some(m)
}

fn generate_func(func: Func, m: &mut Module) -> Option<()> {
    let mut f = Function::new(true, func.name.source);
    f.allocate_stack((func.vars_table.len() + func.temp_count) * 8);
    let id = f.blocks.len();
    f.push_block(format!("b{id}"));
    for stmt in func.body {
        match stmt {
            Stmt::Assign {
                rhs: LValue::Var(_, id),
                lhs,
            } => {
                generate_expr(m, &mut f, lhs, Register::Rax)?;
                let offset = id * 8;
                f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
            }

            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    generate_expr(m, &mut f, expr, Register::Rax)?;
                }
                f.push_raw_instr("mov rsp, rbp");
                f.push_raw_instr("pop rbp");
                f.push_raw_instr("ret");
            }
            Stmt::Funcall(token, exprs) => {
                let call_conv_len = Register::get_syscall_call_convention().len();
                let mut regs = Register::get_syscall_call_convention().into_iter();
                let mut i = 0;
                assert!(exprs.len() < call_conv_len, "Implement args via stack");
                for expr in exprs {
                    let reg = regs.next().unwrap();
                    generate_expr(m, &mut f, expr, reg)?;
                }
                f.push_raw_instr(format!("call _{token}"));
            }
            Stmt::Binop {
                loc,
                result,
                operator,
                rhs,
                lhs,
            } => {
                generate_expr(m, &mut f, lhs, Register::Rbx)?;
                generate_expr(m, &mut f, rhs, Register::Rax)?;
                match operator {
                    BinaryOperator::Add => f.push_raw_instr(format!("add rax, rbx")),
                    BinaryOperator::Mul => f.push_raw_instr(format!("imul rax, rbx")),
                }
                let offset = result * 8;
                f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
            }
        }
    }

    m.push_function(f);
    Some(())
}

fn generate_expr<'src>(m: &mut Module, f: &mut Function, expr: Expr, reg: Register) -> Option<()> {
    match expr {
        Expr::Temp(id) => {
            let offset = id * 8;
            f.push_raw_instr(format!("mov {reg}, [rbp-{offset}]"));
        }
        Expr::Var(_, id) => {
            let offset = id * 8;
            f.push_raw_instr(format!("mov {reg}, [rbp-{offset}]"));
        }
        Expr::IntLit(token) => {
            f.push_raw_instr(format!("mov {reg}, {token}"));
        }
        Expr::StrLit(token) => {
            let id = m.data.len();
            let name = format!("str{id}");
            m.push_data(DataDef::new(
                &name,
                DataDirective::Db,
                vec![DataExpr::Str(token.unescape()), DataExpr::Const(0)],
            ));
            f.push_raw_instr(format!("mov {reg}, {name}"));
        }
    }

    Some(())
}

fn peek_infix_precedence(ctx: &mut CompilerContext<'_>) -> Precedence {
    Precedence::from_token_kind(&ctx.lexer.peek_token().kind)
}

fn next_token_is<'src>(ctx: &mut CompilerContext<'src>, kind: &[TokenKind]) -> Option<bool> {
    let token = ctx.lexer.peek_token();
    if token.is_eof() {
        eprintln!("{}: Expected {:?} EOF", token.loc, kind);
        return None;
    }
    Some(kind.contains(&token.kind))
}

fn expect_token<'src>(ctx: &mut CompilerContext<'src>, kind: TokenKind) -> Option<Token<'src>> {
    let token = ctx.lexer.next_token();
    if token.kind == kind {
        return Some(token);
    } else {
        eprintln!("{}: Expected {:?} got token {}", token.loc, kind, token);
        return None;
    }
}

#[derive(Debug, Default)]
struct Program<'src> {
    funcs: Vec<Func<'src>>,
    extern_funcs: Vec<Token<'src>>,
}

#[derive(Debug, Default)]
struct Func<'src> {
    name: Token<'src>,
    vars_table: HashMap<&'src str, VarId>,
    temp_count: usize,
    body: Vec<Stmt<'src>>,
}

impl<'src> Func<'src> {
    fn new(name: Token<'src>) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }
}

#[derive(Debug)]
enum Stmt<'src> {
    Assign {
        rhs: LValue<'src>,
        lhs: Expr<'src>,
    },
    Return(Option<Expr<'src>>),
    Funcall(Token<'src>, Vec<Expr<'src>>),
    Binop {
        loc: Loc<'src>,
        result: VarId,
        operator: BinaryOperator,
        rhs: Expr<'src>,
        lhs: Expr<'src>,
    },
}

#[derive(Debug)]
enum BinaryOperator {
    Add,
    Mul,
}

impl BinaryOperator {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token.kind {
            TokenKind::Plus => Some(Self::Add),
            TokenKind::Asterisk => Some(Self::Mul),
            _ => {
                eprintln!("{}: {} is not a binary operator", token.loc, token);
                None
            }
        }
    }
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 1,
    Sum,
    Factor,
}

impl Precedence {
    pub fn from_token_kind(kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::Plus => Self::Sum,
            TokenKind::Asterisk => Self::Factor,
            _ => Self::Lowest,
        }
    }
}

type VarId = usize;

#[derive(Debug)]
enum Expr<'src> {
    Temp(usize),
    Var(Token<'src>, VarId),
    IntLit(Token<'src>),
    StrLit(Token<'src>),
}

impl<'src> Expr<'src> {
    pub fn is_rvalue(&self) -> bool {
        matches!(self, Self::Var(..))
    }
}

#[derive(Debug)]
enum LValue<'src> {
    Var(Token<'src>, VarId),
}

#[derive(Debug)]
enum Type {
    Word,
}
