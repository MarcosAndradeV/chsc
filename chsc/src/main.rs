#![allow(dead_code)]
#![allow(unused)]

use std::collections::HashSet;
use std::env::{self, current_dir};
use std::fmt::Display;
use std::{collections::HashMap, env::args, ffi::OsStr, fs, path::PathBuf, process::Command};

use crate::chslexer::*;

use crate::fasm_backend::{DataDef, DataDirective, DataExpr, Function, Module, Register, Value};

mod chslexer;
mod fasm_backend;

const STDLIB_PATH: &str = "stdlib";

fn main() {
    let mut args = args();
    let program = args.next().expect("Executable name missing");

    let file_path = match args.next() {
        Some(path) => path,
        None => {
            eprintln!("Usage: {program} <input>");
            return;
        }
    };

    let stdlib_path = env::var("CHS_STDLIB_PATH").unwrap_or_else(|_| "./stdlib".to_string());

    let chsi_path = PathBuf::from(&file_path).with_extension("chsi");
    if run_cpp(&file_path, &chsi_path, &[stdlib_path]).is_err() {
        eprintln!("C preprocessing failed.");
        return;
    }

    let source = match fs::read_to_string(&chsi_path) {
        Ok(content) => content,
        Err(_) => {
            eprintln!("Cannot read file `{}`", file_path);
            return;
        }
    };

    let mut lexer = PeekableLexer::new(&file_path, &source);
    lexer.set_is_keyword_fn(|k| matches!(k, "fn" | "var" | "return" | "extern" | "if" | "while"));

    let mut ctx = CompilerContext::new(&mut lexer);

    let program_ast = match compile(&mut ctx) {
        Some(p) => p,
        None => {
            eprintln!("Compilation failed.");
            return;
        }
    };

    let machine_code = match generate(program_ast) {
        Some(m) => m,
        None => {
            eprintln!("Code generation failed.");
            return;
        }
    };

    let asm_path = PathBuf::from(&file_path).with_extension("asm");
    let o_path = PathBuf::from(&file_path).with_extension("o");
    let exe_path = PathBuf::from(&file_path).with_extension("");

    if let Err(e) = fs::write(&asm_path, machine_code.to_string()) {
        eprintln!("Failed to write assembly file: {e}");
        return;
    }

    if run_fasm(&asm_path, &o_path).is_err() {
        eprintln!("Assembly with FASM failed.");
        return;
    }

    if run_cc(&o_path, &exe_path, ["-no-pie"]).is_err() {
        eprintln!("Linking with cc failed.");
        return;
    }
}

fn run_cpp<I, O, Inc, Ps>(input_path: I, output_path: O, include_paths: Ps) -> Result<(), ()>
where
    I: AsRef<OsStr>,
    O: AsRef<OsStr>,
    Inc: AsRef<OsStr> + Display,
    Ps: IntoIterator<Item = Inc>,
{
    let mut cpp_command = Command::new("cpp");
    cpp_command
        .arg("-o")
        .arg(output_path)
        .arg(input_path);

    cpp_command.args(include_paths.into_iter().map(|i| format!("-I{i}")));

    let output = cpp_command
        .output()
        .map_err(|e| eprintln!("Failed to run cc: {}", e))?;
    if !output.status.success() {
        eprintln!(
            "cpp failed to generate\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        return Err(());
    }
    Ok(())
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
    vars_index: VarsIndex<'src>,
    funcs_index: FuncIndex<'src>,
    attr_index: AttrIndex<'src>,
}

impl<'src> CompilerContext<'src> {
    fn new(lexer: &'src mut PeekableLexer<'src>) -> Self {
        Self {
            lexer,
            vars_index: VarsIndex::default(),
            funcs_index: FuncIndex::default(),
            attr_index: AttrIndex::default(),
        }
    }
}

fn compile<'src>(ctx: &mut CompilerContext<'src>) -> Option<Program<'src>> {
    let mut p = Program::default();
    ctx.vars_index.push_scope();

    loop {
        let token = ctx.lexer.next_token();

        if token.is_eof() {
            return Some(p);
        }

        match token.kind {
            TokenKind::MacroCall => compile_attr1(ctx, token),
            TokenKind::MacroCallWithArgs => compile_attr2(ctx, token),
            TokenKind::Keyword if token.source == "fn" => {
                compile_func(ctx, &mut p)?;
            }
            TokenKind::Keyword if token.source == "extern" => {
                expect_token_source(ctx, TokenKind::Keyword, "fn")?;
                let fname = expect_token(ctx, TokenKind::Identifier)?;
                expect_token(ctx, TokenKind::OpenParen)?;
                let mut args_tys = vec![];
                let mut arg_count = 0;
                let mut ret_ty = None;
                let mut is_variadic = false;
                loop {
                    let token = ctx.lexer.peek_token();
                    match token.kind {
                        TokenKind::CloseParen => {
                            ctx.lexer.next_token();
                            break;
                        }
                        TokenKind::Comma => {
                            ctx.lexer.next_token();
                        }
                        TokenKind::Splat => {
                            ctx.lexer.next_token();
                            expect_token(ctx, TokenKind::CloseParen)?;
                            is_variadic = true;
                            break;
                        }
                        _ => {
                            args_tys.push(compile_type(ctx)?);
                            arg_count += 1;
                        }
                    }
                }
                if ctx.lexer.peek_token().kind == TokenKind::Arrow {
                    ctx.lexer.next_token();
                    ret_ty = Some(compile_type(ctx)?);
                }
                expect_token(ctx, TokenKind::SemiColon)?;
                let func_id = p.funcs.len();
                if ctx.funcs_index.0.insert(fname.source, func_id).is_some() {
                    eprintln!("{}: Redefinition of function {}", fname.loc, fname);
                    return None;
                }
                p.externs.insert(fname.source);
                p.funcs.push(Func {
                    name: fname,
                    args_tys,
                    ret_ty,
                    is_variadic,
                    is_extern: true,
                    arg_count,
                    local_var_count: 0,
                    body: vec![],
                });
            }
            TokenKind::Keyword if token.source == "var" => {
                let token = expect_token(ctx, TokenKind::Identifier)?;
                let ty = compile_type(ctx)?;
                let v = Var {
                    storage: Storage::Global,
                    token,
                    ty,
                    id: 0,
                };
                let id = p.push_var(v);
                if ctx.vars_index.insert_var_index(token.source, id).is_some() {
                    eprintln!("{}: Redefinition of global var {}", token.loc, token);
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

fn compile_attr1<'src>(ctx: &mut CompilerContext<'src>, token: Token<'src>) {
    let name = &token.source[1..token.source.len()];
    ctx.attr_index.0.insert(name, None);
}

fn compile_attr2<'src>(ctx: &mut CompilerContext<'src>, token: Token<'src>) {
    let idx = token.source.find("(").unwrap();
    let last_idx = token.source.find(")").unwrap();
    let name = &token.source[1..idx];
    let val = &token.source[idx + 1..last_idx - 1];
    ctx.attr_index.0.insert(name, Some(val));
}

fn compile_func<'src>(ctx: &mut CompilerContext<'src>, p: &mut Program<'src>) -> Option<()> {
    let fname = expect_token(ctx, TokenKind::Identifier)?;

    let func_id = p.funcs.len();
    if ctx.funcs_index.0.insert(fname.source, func_id).is_some() {
        eprintln!("{}: Redefinition of function {}", fname.loc, fname);
        return None;
    }

    let mut f = Func::new(fname);
    expect_token(ctx, TokenKind::OpenParen)?;

    loop {
        let token = ctx.lexer.next_token();
        match token.kind {
            TokenKind::CloseParen => break,
            TokenKind::Comma => {}
            TokenKind::Splat => {
                expect_token(ctx, TokenKind::CloseParen)?;
                f.is_variadic = true;
                f.args_tys.push(Type::Vaargs);
                break;
            }
            TokenKind::Identifier => {
                let ty = compile_type(ctx)?;
                f.args_tys.push(ty.clone());
                let id = f.local_var_count;
                f.local_var_count += 1;
                let v = Var {
                    storage: Storage::Local,
                    token,
                    ty,
                    id,
                };
                let id = p.push_var(v);
                if ctx.vars_index.insert_var_index(token.source, id).is_some() {
                    eprintln!("{}: Redefinition of arg {}", token.loc, token);
                    return None;
                }
                f.arg_count += 1;
            }
            _ => {
                eprintln!("{}: Expected arg but got {}", token.loc, token);
                return None;
            }
        }
    }

    if ctx.lexer.peek_token().kind == TokenKind::Arrow {
        ctx.lexer.next_token();
        f.ret_ty = Some(compile_type(ctx)?);
    }

    expect_token(ctx, TokenKind::OpenBrace)?;
    ctx.vars_index.push_scope();
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
    ctx.vars_index.pop_scope();
    p.funcs.push(f);
    Some(())
}

fn compile_stmt<'src>(
    ctx: &mut CompilerContext<'src>,
    p: &mut Program<'src>,
    f: &mut Func<'src>,
) -> Option<()> {
    let token = ctx.lexer.next_token();
    match token.kind {
        TokenKind::Keyword if token.source == "var" => {
            let name = expect_token(ctx, TokenKind::Identifier)?;
            let ty = compile_type(ctx)?;
            let id = f.local_var_count;
            f.local_var_count += 1;
            let v = Var {
                storage: Storage::Local,
                token: name,
                ty,
                id,
            };
            let index = p.push_var(v);
            if ctx
                .vars_index
                .insert_var_index(name.source, index)
                .is_some()
            {
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
            ctx.vars_index.push_scope();
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
            ctx.vars_index.pop_scope();
        }
        TokenKind::Keyword if token.source == "while" => {
            ctx.vars_index.push_scope();
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
            if let Some(Stmt::Jz { cond: _, label }) = f.body.get_mut(patch) {
                *label = len;
            }
            f.body.push(Stmt::Block(len));
            ctx.vars_index.pop_scope();
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
            f.body.push(Stmt::Funcall {
                result: None,
                caller: token,
                args,
            });
        }
        _ => {
            let lhs = match token.kind {
                TokenKind::Identifier => {
                    if let Some(&index) = ctx.vars_index.get_var_index(token.source) {
                        let var = p.get_var(index)?;
                        match var.storage {
                            Storage::Global => Expr::GlobalVar(token),
                            Storage::Local => Expr::Var(token, var.id),
                        }
                    } else {
                        eprintln!("{}: Undefined var {}", token.loc, token);
                        return None;
                    }
                }
                TokenKind::Asterisk => {
                    let lhs = compile_expr(ctx, p, f, Precedence::Lowest, &[TokenKind::Assign])?;

                    let lhs = match lhs {
                        Expr::Var(_, id) => id,
                        Expr::Temp(id) => id,
                        _ => todo!(),
                    };

                    expect_token(ctx, TokenKind::Assign)?;

                    let rhs = compile_expr(ctx, p, f, Precedence::Lowest, &[TokenKind::SemiColon])?;

                    f.body.push(Stmt::Store { lhs, rhs });
                    expect_token(ctx, TokenKind::SemiColon)?;
                    return Some(());
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
    p: &mut Program<'src>,
    f: &mut Func<'src>,
    precedence: Precedence,
    stop: &[TokenKind],
) -> Option<Expr<'src>> {
    let token = ctx.lexer.next_token();
    let mut left = match token.kind {
        TokenKind::IntegerNumber => Expr::IntLit(token),
        TokenKind::StringLiteral => Expr::StrLit(token),
        TokenKind::OpenParen => {
            let expr = compile_expr(ctx, p, f, precedence, stop)?;
            expect_token(ctx, TokenKind::CloseParen)?;
            expr
        }
        TokenKind::Ampersand => {
            let expr = compile_expr(ctx, p, f, precedence, stop)?;
            match expr {
                Expr::Var(token, id) => Expr::Ref(token, id),
                _ => todo!(),
            }
        }
        TokenKind::Asterisk => {
            let expr = compile_expr(ctx, p, f, precedence, stop)?;
            let id = f.local_var_count;
            f.local_var_count += 1;
            p.vars.push(Var {
                storage: Storage::Local,
                token,
                ty: Type::Word,
                id,
            });
            f.body.push(Stmt::Assign {
                lhs: Expr::Temp(id),
                rhs: expr,
            });
            Expr::Deref(id)
        }
        TokenKind::Identifier if next_token_is(ctx, &[TokenKind::OpenParen])? => {
            ctx.lexer.next_token();
            let id = f.local_var_count;
            f.local_var_count += 1;
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
            f.body.push(Stmt::Funcall {
                result: Some(id),
                caller: token,
                args,
            });
            Expr::Temp(id)
        }
        TokenKind::Identifier => {
            if let Some(&index) = ctx.vars_index.get_var_index(token.source) {
                let var = p.get_var(index)?;
                match var.storage {
                    Storage::Global => Expr::GlobalVar(token),
                    Storage::Local => Expr::Var(token, var.id),
                }
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
            let id = f.local_var_count;
            f.local_var_count += 1;
            p.vars.push(Var {
                storage: Storage::Local,
                token,
                ty: Type::Word,
                id,
            });
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

    for name in p.externs {
        m.push_extrn(name);
    }

    for func in p.funcs.into_iter().filter(|f| !f.is_extern) {
        generate_func(func, &mut m)?
    }

    for v in p.vars.iter().filter(|v| v.storage.is_global()) {
        m.push_data(DataDef::new(
            format!("_{}", v.token),
            DataDirective::Rq,
            vec![DataExpr::Const(v.ty.size() as u64)],
        ));
    }

    Some(m)
}

fn generate_func(func: Func, m: &mut Module) -> Option<()> {
    let mut f = Function::new(true, func.name.source);
    f.allocate_stack(func.local_var_count * 8);
    f.push_block("start");
    assert!(
        func.arg_count < Register::get_syscall_call_convention().len(),
        "Implement args via stack"
    );
    assert!(!func.is_variadic, "Implement variadic call");
    let regs = Register::get_syscall_call_convention().into_iter();
    for (id, reg) in (0..func.arg_count).into_iter().zip(regs) {
        let offset = id * 8;
        f.push_raw_instr(format!("mov QWORD [rbp-{offset}], {reg}"));
    }
    for stmt in func.body {
        match stmt {
            Stmt::Store { lhs, rhs } => {
                let offset = lhs * 8;
                f.push_raw_instr(format!("mov QWORD rax, [rbp-{offset}]"));
                generate_expr(m, &mut f, rhs, Value::Register(Register::Rbx))?;
                f.push_raw_instr(format!("mov QWORD [rax], rbx"));
            }
            Stmt::Assign {
                lhs: Expr::Var(_, id),
                rhs,
            } => {
                let offset = id * 8;
                generate_expr(m, &mut f, rhs, Value::Register(Register::Rax))?;
                f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
            }
            Stmt::Assign {
                lhs: Expr::GlobalVar(token),
                rhs,
            } => {
                generate_expr(m, &mut f, rhs, Value::Register(Register::Rax))?;
                f.push_raw_instr(format!("mov QWORD [_{token}], rax"));
            }
            Stmt::Assign {
                lhs: Expr::Temp(id),
                rhs,
            } => {
                let offset = id * 8;
                generate_expr(m, &mut f, rhs, Value::Register(Register::Rax))?;
                f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
            }
            Stmt::Assign { .. } => unreachable!(),
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    generate_expr(m, &mut f, expr, Value::Register(Register::Rax))?;
                }
                f.push_raw_instr("mov rsp, rbp");
                f.push_raw_instr("pop rbp");
                f.push_raw_instr("ret");
            }
            Stmt::Funcall {
                result,
                caller,
                args,
            } => {
                assert!(
                    args.len() < Register::get_syscall_call_convention().len(),
                    "Implement args via stack"
                );
                let regs = Register::get_syscall_call_convention().into_iter();
                for (expr, reg) in args.into_iter().zip(regs).rev() {
                    generate_expr(m, &mut f, expr, Value::Register(reg))?;
                }

                // x86_64 Linux ABI passes the amount of floating point args via al.
                f.push_raw_instr("mov al, 0");
                f.push_raw_instr(format!("call _{caller}"));
                if let Some(result) = result {
                    let offset = result * 8;
                    f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
                }
            }
            Stmt::Binop {
                loc: _,
                result,
                operator,
                lhs,
                rhs,
            } => {
                generate_expr(m, &mut f, lhs, Value::Register(Register::Rax))?;
                generate_expr(m, &mut f, rhs, Value::Register(Register::Rbx))?;
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
                generate_expr(m, &mut f, cond, Value::Register(reg))?;
                f.push_raw_instr(format!("test {reg}, {reg}"));
                f.push_raw_instr(format!("jnz .b{label}"));
            }
            Stmt::Jz { cond, label } => {
                let reg = Register::Rax;
                generate_expr(m, &mut f, cond, Value::Register(reg))?;
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

fn generate_expr<'src>(m: &mut Module, f: &mut Function, expr: Expr, val: Value) -> Option<()> {
    match expr {
        Expr::Deref(id) => {
            let offset = id * 8;
            f.push_raw_instr(format!("mov {val}, [rbp-{offset}]"));
            f.push_raw_instr(format!("mov {val}, [{val}]"));
        }
        Expr::Ref(_, id) => {
            let offset = id * 8;
            f.push_raw_instr(format!("lea {val}, [rbp-{offset}]"));
        }
        Expr::Temp(id) => {
            let offset = id * 8;
            f.push_raw_instr(format!("mov {val}, [rbp-{offset}]"));
        }
        Expr::Var(_, id) => {
            let offset = id * 8;
            f.push_raw_instr(format!("mov {val}, [rbp-{offset}]"));
        }
        Expr::GlobalVar(token) => {
            f.push_raw_instr(format!("mov {val}, [_{token}]"));
        }
        Expr::IntLit(token) => {
            f.push_raw_instr(format!("mov {val}, {token}"));
        }
        Expr::StrLit(token) => {
            let id = m.data.len();
            let name = format!("str{id}");
            m.push_data(DataDef::new(
                &name,
                DataDirective::Db,
                vec![DataExpr::Str(token.unescape()), DataExpr::Const(0)],
            ));
            f.push_raw_instr(format!("mov {val}, {name}"));
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

fn expect_token_source<'src>(
    ctx: &mut CompilerContext<'src>,
    kind: TokenKind,
    source: &str,
) -> Option<Token<'src>> {
    let token = expect_token(ctx, kind)?;
    if token.source == source {
        return Some(token);
    } else {
        eprintln!(
            "{}: Expected {:?}({}) got token {}",
            token.loc, kind, source, token
        );
        return None;
    }
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
    externs: HashSet<&'src str>,
    vars: Vec<Var<'src>>,
    funcs: Vec<Func<'src>>,
}

#[derive(Debug, Default)]
struct VarsIndex<'src>(pub Vec<HashMap<&'src str, VarId>>);
#[derive(Debug, Default)]
struct FuncIndex<'src>(pub HashMap<&'src str, FuncId>);
#[derive(Debug, Default)]
struct AttrIndex<'src>(pub HashMap<&'src str, Option<&'src str>>);

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

impl<'src> Program<'src> {
    pub fn push_var(&mut self, v: Var<'src>) -> VarId {
        let id = self.vars.len();
        self.vars.push(v);
        id
    }

    pub fn get_var(&self, index: VarId) -> Option<&Var> {
        self.vars.get(index)
    }
}

#[derive(Debug, Default)]
struct Func<'src> {
    name: Token<'src>,

    args_tys: Vec<Type>,
    ret_ty: Option<Type>,

    is_variadic: bool,
    is_extern: bool,

    arg_count: usize,
    local_var_count: usize,
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
    Store {
        lhs: VarId,
        rhs: Expr<'src>,
    },
    Return(Option<Expr<'src>>),
    Funcall {
        result: Option<VarId>,
        caller: Token<'src>,
        args: Vec<Expr<'src>>,
    },
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

#[derive(Debug, Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
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
type FuncId = usize;

#[derive(Debug)]
struct Global<'src> {
    token: Token<'src>,
    ty: Type,
}

#[derive(Debug)]
struct Var<'src> {
    storage: Storage,
    token: Token<'src>,
    ty: Type,
    id: VarId,
}

#[derive(Debug, Clone)]
enum Storage {
    Global,
    Local,
}
impl Storage {
    fn is_global(&self) -> bool {
        matches!(self, Self::Global)
    }
}

#[derive(Debug)]
enum Expr<'src> {
    Ref(Token<'src>, VarId),
    Deref(VarId),
    Temp(VarId),
    Var(Token<'src>, VarId),
    GlobalVar(Token<'src>),
    IntLit(Token<'src>),
    StrLit(Token<'src>),
}

#[derive(Debug, Clone)]
enum Type {
    Vaargs,
    Word,
    Ptr,
    I32,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Vaargs => 8, // TODO What is the size of Vaargs?
            Type::Word => 8,
            Type::Ptr => 8,
            Type::I32 => 4,
        }
    }
}
