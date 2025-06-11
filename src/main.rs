#![allow(dead_code)]
#![allow(unused)]

use std::{collections::HashMap, env::args, ffi::OsStr, fs, path::PathBuf, process::Command};

use crate::chslexer::*;

use crate::fasm_backend::{DataDef, DataDirective, DataExpr, Function, Module, Register};

mod chslexer;
mod fasm_backend;

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
    l.set_is_keyword_fn(|k| matches!(k, "fn" | "var" | "return" | "extern" | "if" | "while"));
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
                expect_token(ctx, TokenKind::SemiColon)?;
            }
            TokenKind::Keyword if token.source == "var" => {
                let name = expect_token(ctx, TokenKind::Identifier)?;
                let ty = compile_type(ctx)?;
                if p.global_vars_table.insert(name.source, ty).is_some() {
                    eprintln!("{}: Redefinition of global var {}", name.loc, name);
                    return None;
                }
                expect_token(ctx, TokenKind::SemiColon)?;
            }
            _ => {
                eprintln!("{}: Unexpected token {}", token.loc, token);
                return None;
            }
        }
    }
}

fn compile_func<'src>(ctx: &mut CompilerContext<'src>, p: &mut Program<'src>) -> Option<()> {
    let fname = expect_token(ctx, TokenKind::Identifier)?;
    let mut f = Func::new(fname);
    expect_token(ctx, TokenKind::OpenParen)?;
    expect_token(ctx, TokenKind::CloseParen)?;
    expect_token(ctx, TokenKind::OpenBrace)?;
    loop {
        let token = ctx.lexer.peek_token();
        if token.is_eof() {
            eprintln!("{}: Unexpected EOF", token.loc);
            return None;
        }
        if token.kind == TokenKind::CloseBrace {
            ctx.lexer.next_token();
            break;
        }
        compile_stmt(ctx, p, &mut f)?;
    }
    p.funcs.push(f);
    Some(())
}

fn compile_stmt<'src>(
    ctx: &mut CompilerContext<'src>,
    p: &Program<'src>,
    f: &mut Func<'src>,
) -> Option<()> {
    let token = ctx.lexer.next_token();
    match token.kind {
        TokenKind::Keyword if token.source == "var" => {
            let name = expect_token(ctx, TokenKind::Identifier)?;
            let ty = compile_type(ctx)?;
            let offset = f.temp_count;
            f.temp_count += 1;
            if f.vars_table.insert(name.source, (ty, offset)).is_some() {
                eprintln!("{}: Redefinition of var {}", name.loc, name);
                return None;
            }
            expect_token(ctx, TokenKind::SemiColon)?;
        }
        TokenKind::Keyword if token.source == "return" => {
            if ctx.lexer.peek_token().kind != TokenKind::SemiColon {
                let expr = compile_expr(ctx, p, f, Precedence::Lowest, &[TokenKind::SemiColon])?;
                f.body.push(Stmt::Return(Some(expr)));
            } else {
                f.body.push(Stmt::Return(None));
            }
            expect_token(ctx, TokenKind::SemiColon)?;
        }
        TokenKind::Keyword if token.source == "if" => {
            expect_token(ctx, TokenKind::OpenParen)?;
            let cond = compile_expr(ctx, p, f, Precedence::Lowest, &[TokenKind::CloseParen])?;
            expect_token(ctx, TokenKind::CloseParen)?;
            expect_token(ctx, TokenKind::OpenBrace)?;
            let if_index = f.body.len();
            f.body.push(Stmt::Jz { cond, label: 0 });
            loop {
                let token = ctx.lexer.peek_token();
                if token.is_eof() {
                    eprintln!("{}: Unexpected EOF", token.loc);
                    return None;
                }
                if token.kind == TokenKind::CloseBrace {
                    ctx.lexer.next_token();
                    break;
                }
                compile_stmt(ctx, p, f)?;
            }
            let false_block_patch = f.body.len();
            if let Some(Stmt::Jz { cond: _, label }) = f.body.get_mut(if_index) {
                *label = false_block_patch;
            }
            f.body.push(Stmt::Block(false_block_patch));
        }
        TokenKind::Keyword if token.source == "while" => {
            expect_token(ctx, TokenKind::OpenParen)?;
            let cond_label = f.body.len();
            f.body.push(Stmt::Block(cond_label));
            let cond = compile_expr(ctx, p, f, Precedence::Lowest, &[TokenKind::CloseParen])?;
            expect_token(ctx, TokenKind::CloseParen)?;
            expect_token(ctx, TokenKind::OpenBrace)?;
            let patch = f.body.len();
            f.body.push(Stmt::Jz { cond, label: 0 });
            loop {
                let token = ctx.lexer.peek_token();
                if token.is_eof() {
                    eprintln!("{}: Unexpected EOF", token.loc);
                    return None;
                }
                if token.kind == TokenKind::CloseBrace {
                    ctx.lexer.next_token();
                    break;
                }
                compile_stmt(ctx, p, f)?;
            }
            f.body.push(Stmt::Jmp { label: cond_label });
            let len = f.body.len();
            if let Some(Stmt::Jz { cond, label }) = f.body.get_mut(patch) {
                *label = len;
            }
            f.body.push(Stmt::Block(len));
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
                    _ => args.push(compile_expr(
                        ctx,
                        p,
                        f,
                        Precedence::Lowest,
                        &[TokenKind::CloseParen, TokenKind::Comma],
                    )?),
                }
            }
            ctx.lexer.next_token();
            expect_token(ctx, TokenKind::SemiColon)?;
            f.body.push(Stmt::Funcall(token, args));
        }
        _ => {
            let lhs = match token.kind {
                TokenKind::Identifier => {
                    if let Some((_, offset)) = f.vars_table.get(token.source) {
                        Expr::Var(token, *offset)
                    } else if let Some(t) = p.global_vars_table.get(token.source) {
                        Expr::GlobalVar(token, t.clone())
                    } else {
                        eprintln!("{}: Undefined var {}", token.loc, token);
                        return None;
                    }
                }
                _ => {
                    eprintln!(
                        "{}: Expected assginable expression but got {}",
                        token.loc, token
                    );
                    return None;
                }
            };

            expect_token(ctx, TokenKind::Assign)?;

            let rhs = compile_expr(ctx, p, f, Precedence::Lowest, &[TokenKind::SemiColon])?;

            f.body.push(Stmt::Assign { lhs, rhs });
            expect_token(ctx, TokenKind::SemiColon)?;
        }
    }

    Some(())
}

fn compile_type(ctx: &mut CompilerContext<'_>) -> Option<Type> {
    let token = ctx.lexer.next_token();
    if token.is_eof() {
        eprintln!("{}: Unexpected EOF", token.loc);
        return None;
    }
    match token.kind {
        TokenKind::Identifier if token.source == "word" => Some(Type::Word),
        TokenKind::Identifier if token.source == "i32" => Some(Type::I32),
        TokenKind::Identifier if token.source == "ptr" => Some(Type::Ptr),
        _ => {
            eprintln!("{}: Expected type but got {}", token.loc, token);
            return None;
        }
    }
}

fn compile_expr<'src>(
    ctx: &mut CompilerContext<'src>,
    p: &Program<'src>,
    f: &mut Func<'src>,
    precedence: Precedence,
    stop: &[TokenKind],
) -> Option<Expr<'src>> {
    let token = ctx.lexer.next_token();
    let mut left = match token.kind {
        TokenKind::IntegerNumber => Expr::IntLit(token),
        TokenKind::StringLiteral => Expr::StrLit(token),
        TokenKind::Identifier => {
            if let Some((_, offset)) = f.vars_table.get(token.source) {
                Expr::Var(token, *offset)
            } else if let Some(t) = p.global_vars_table.get(token.source) {
                Expr::GlobalVar(token, t.clone())
            } else {
                eprintln!("{}: Undefined var {}", token.loc, token);
                return None;
            }
        }
        _ => {
            eprintln!("{}: Expected expression but got {}", token.loc, token);
            return None;
        }
    };
    while !next_token_is(ctx, stop)? && precedence < peek_infix_precedence(ctx) {
        let token = ctx.lexer.next_token();
        let infix = {
            let precedence = Precedence::from_token_kind(&token.kind);
            let right = compile_expr(ctx, p, f, precedence, stop)?;
            let id = f.temp_count;
            f.temp_count += 1;
            f.body.push(Stmt::Binop {
                loc: token.loc,
                result: id,
                operator: BinaryOperator::from_token(&token)?,
                lhs: left,
                rhs: right,
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

    for (name, t) in p.global_vars_table {
        m.push_data(DataDef::new(
            format!("_{name}"),
            DataDirective::Rq,
            vec![DataExpr::Const(t.size() as u64)],
        ));
    }

    Some(m)
}

fn generate_func(func: Func, m: &mut Module) -> Option<()> {
    let mut f = Function::new(true, func.name.source);
    f.allocate_stack((func.temp_count) * 8);
    f.push_block("start");
    for stmt in func.body {
        match stmt {
            Stmt::Assign {
                lhs: Expr::Var(_, id),
                rhs,
            } => {
                generate_expr(m, &mut f, rhs, Register::Rax)?;
                let offset = id * 8;
                f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
            }
            Stmt::Assign {
                lhs: Expr::GlobalVar(token, s),
                rhs,
            } => {
                generate_expr(m, &mut f, rhs, Register::Rax)?;
                f.push_raw_instr(format!("mov QWORD [_{token}], rax"));
            }
            Stmt::Assign { .. } => unreachable!(),
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    generate_expr(m, &mut f, expr, Register::Rax)?;
                }
                f.push_raw_instr("mov rsp, rbp");
                f.push_raw_instr("pop rbp");
                f.push_raw_instr("ret");
            }
            Stmt::Funcall(token, exprs) => {
                assert!(
                    exprs.len() < Register::get_syscall_call_convention().len(),
                    "Implement args via stack"
                );
                let mut regs = Register::get_syscall_call_convention().into_iter();
                for (expr, reg) in exprs.into_iter().zip(regs).rev() {
                    generate_expr(m, &mut f, expr, reg)?;
                }

                // x86_64 Linux ABI passes the amount of floating point args via al.
                f.push_raw_instr("mov al, 0");
                f.push_raw_instr(format!("call _{token}"));
            }
            Stmt::Binop {
                loc: _,
                result,
                operator,
                lhs,
                rhs,
            } => {
                generate_expr(m, &mut f, lhs, Register::Rax)?;
                generate_expr(m, &mut f, rhs, Register::Rbx)?;
                let offset = result * 8;
                let mut out_reg = Register::Rax;
                match operator {
                    BinaryOperator::Add => f.push_raw_instr("add rax, rbx"),
                    BinaryOperator::Mul => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr("imul rbx")
                    }
                    BinaryOperator::Sub => f.push_raw_instr("sub rax, rbx"),
                    BinaryOperator::Mod => {
                        f.push_raw_instr("cqo");
                        f.push_raw_instr("idiv rbx");
                        out_reg = Register::Rdx;
                    }
                    BinaryOperator::Div => {
                        f.push_raw_instr("cqo");
                        f.push_raw_instr("idiv rbx");
                    }
                    BinaryOperator::Less => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr("cmp rax, rbx");
                        f.push_raw_instr("setl dl");
                        out_reg = Register::Rdx;
                    }
                    BinaryOperator::Eq => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr("cmp rax, rbx");
                        f.push_raw_instr("sete dl");
                        out_reg = Register::Rdx;
                    }
                }
                f.push_raw_instr(format!("mov QWORD [rbp-{offset}], {out_reg}"));
            }
            Stmt::Jnz { cond, label } => {
                let reg = Register::Rax;
                generate_expr(m, &mut f, cond, reg)?;
                f.push_raw_instr(format!("test {reg}, {reg}"));
                f.push_raw_instr(format!("jnz .b{label}"));
            }
            Stmt::Jz { cond, label } => {
                let reg = Register::Rax;
                generate_expr(m, &mut f, cond, reg)?;
                f.push_raw_instr(format!("test {reg}, {reg}"));
                f.push_raw_instr(format!("jz .b{label}"));
            }
            Stmt::Jmp { label } => {
                f.push_raw_instr(format!("jmp .b{label}"));
            }
            Stmt::Block(id) => {
                f.push_block(format!("b{id}"));
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
        Expr::GlobalVar(token, _) => {
            f.push_raw_instr(format!("mov {reg}, [_{token}]"));
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
    global_vars_table: HashMap<&'src str, Type>,
}

#[derive(Debug, Default)]
struct Func<'src> {
    name: Token<'src>,
    vars_table: HashMap<&'src str, (Type, VarId)>,
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
        lhs: Expr<'src>,
        rhs: Expr<'src>,
    },
    Return(Option<Expr<'src>>),
    Funcall(Token<'src>, Vec<Expr<'src>>),
    Binop {
        loc: Loc<'src>,
        result: VarId,
        operator: BinaryOperator,
        lhs: Expr<'src>,
        rhs: Expr<'src>,
    },
    Jnz {
        cond: Expr<'src>,
        label: usize,
    },
    Jz {
        cond: Expr<'src>,
        label: usize,
    },
    Block(usize),
    Jmp {
        label: usize,
    },
}

#[derive(Debug)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Less,
    Eq,
}

impl BinaryOperator {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token.kind {
            TokenKind::Plus => Some(Self::Add),
            TokenKind::Minus => Some(Self::Sub),
            TokenKind::Asterisk => Some(Self::Mul),
            TokenKind::Mod => Some(Self::Mod),
            TokenKind::Slash => Some(Self::Div),
            TokenKind::Lt => Some(Self::Less),
            TokenKind::Eq => Some(Self::Eq),
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
    Assignment,
    LogicalOr,
    LogicalAnd,
    Equality,
    Comparison,
    Sum,
    Factor,
}

impl Precedence {
    pub fn from_token_kind(kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::Plus | TokenKind::Minus => Self::Sum,
            TokenKind::Asterisk | TokenKind::Slash | TokenKind::Mod => Self::Factor,
            TokenKind::Lt | TokenKind::Gt => Self::Comparison,
            TokenKind::Eq => Self::Equality,
            _ => Self::Lowest,
        }
    }
}

type VarId = usize;

#[derive(Debug)]
enum Expr<'src> {
    Temp(usize),
    Var(Token<'src>, VarId),
    GlobalVar(Token<'src>, Type),
    IntLit(Token<'src>),
    StrLit(Token<'src>),
}

#[derive(Debug, Clone)]
enum Type {
    Word,
    Ptr,
    I32,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Word => 8,
            Type::Ptr => 8,
            Type::I32 => 4,
        }
    }
}
