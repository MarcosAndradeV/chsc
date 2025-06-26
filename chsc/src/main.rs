#![allow(unused)]

use std::fs;

use fasm_backend::{
    Cond, DataDef, DataDirective, DataExpr, Function, Instr, Module, Register, Value,
};

use crate::ast::*;
use crate::chslexer::*;

use crate::opt::*;
use crate::utils::*;

mod ast;
mod chslexer;
mod fasm_backend;
mod opt;
mod parser;
mod utils;

fn main() {
    match app() {
        Err(err) => {
            handle_app_error(&err);
        }
        _ => (),
    }
}

/// I HATE RUST
fn app() -> Result<(), AppError> {
    let (file_path, compiler_flags, run, use_c, debug_ast) = parse_args()?;

    validate_input_file(&file_path)?;

    let source = fs::read_to_string(&file_path).map_err(|e| AppError::FileError {
        path: file_path.clone(),
        error: e,
    })?;

    let mut program_ast =
        parser::parse(&file_path, &source).map_err(|e| AppError::ParseError(format!("{}", e)))?;

    if debug_ast {
        // const_fold(&mut program_ast);
        print_program(&program_ast);
        let useds = find_used_vars(&program_ast.funcs[0]);
        for (i, used) in useds.iter().enumerate() {
            println!("Var({i}) [{}]", if *used { 'x' } else { ' ' })
        }
        return Ok(());
    }

    let asm_code = generate(program_ast, use_c)?;

    let (asm_path, o_path, exe_path) = generate_output_paths(&file_path)?;

    fs::write(&asm_path, asm_code.to_string()).map_err(|e| AppError::FileError {
        path: asm_path.to_string_lossy().to_string(),
        error: e,
    })?;

    println!("Generated assembly: {}", asm_path.display());

    let target_path = if use_c { &o_path } else { &exe_path };
    run_fasm(&asm_path, target_path)?;
    println!("Assembly successful: {}", target_path.display());

    if use_c {
        run_cc(&o_path, &exe_path, &compiler_flags)?;
        println!("Linking successful: {}", exe_path.display());
    }

    if run {
        println!("Running executable...");
        let output = run_exe(&exe_path.to_string_lossy().to_string())?;
        print!("{}", output);
    }

    Ok(())
}

fn generate(p: Program, use_c: bool) -> Result<Module, AppError> {
    let mut m = Module::new(use_c);

    for extern_ in p.externs {
        match extern_ {
            Extern::Symbol(token) => {
                m.push_extrn(token.source);
            }
            Extern::Func(_) => todo!(),
            Extern::Var(_) => todo!(),
        }
    }

    for func in p.funcs {
        generate_func(func, &mut m)?
    }

    Ok(m)
}

fn generate_func(func: Func, m: &mut Module) -> Result<(), AppError> {
    let Func {
        name,
        args,
        vars,
        body,
        ..
    } = func;
    let mut f = Function::new(m.link_with_c, func.name.source);
    f.allocate_stack(vars.len() * 8);
    f.push_block("start");
    assert!(args.is_empty());
    for stmt in body {
        match stmt {
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    generate_expr(m, &mut f, &vars, expr, Value::Register(Register::Rax));
                }
                f.push_raw_instr("mov rsp, rbp");
                f.push_raw_instr("pop rbp");
                f.push_raw_instr("ret");
            }
            Stmt::Syscall { result, args } => {
                assert!(
                    args.len() < Register::get_syscall_call_convention().len(),
                    "Implement args via stack"
                );
                let regs = Register::get_syscall_call_convention().into_iter();
                for (expr, reg) in args.into_iter().zip(regs).rev() {
                    generate_expr(m, &mut f, &vars, expr, Value::Register(reg));
                }

                f.push_raw_instr(format!("syscall"));

                if let Some(VarId(var_id, _)) = result {
                    let offset = var_id * 8;
                    f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
                }
            }
            Stmt::Funcall {
                result,
                caller,
                args,
            } => {
                if args.len() >= Register::get_call_convention().len() {
                    return Err(AppError::GenerationError(
                        Some(
                            "Functions with more than 6 arguments are not supported yet."
                                .to_string(),
                        ),
                        "Implement args via stack".to_string(),
                    ));
                }

                let regs = Register::get_call_convention().into_iter();
                for (expr, reg) in args.into_iter().zip(regs).rev() {
                    generate_expr(m, &mut f, &vars, expr, Value::Register(reg));
                }

                // x86_64 Linux ABI passes the amount of floating point args via al.
                f.push_raw_instr("mov al, 0");
                match caller {
                    Expr::Var(token, _) => {
                        let name = token.source;
                        f.push_raw_instr(format!("call _{name}"));
                    }
                    Expr::IntLit(loc, ..)
                    | Expr::StrLit(Token { loc, .. })
                    | Expr::Deref(Token { loc, .. }, _)
                    | Expr::Ref(Token { loc, .. }, _) => {
                        return Err(AppError::GenerationError(
                            Some(format!(
                                "{}: call arbitrary expressions is not supported yet",
                                loc
                            )),
                            format!("{}: Cannot call arbitrary expressions", loc),
                        ));
                    }
                }

                if let Some(VarId(var_id, _)) = result {
                    let offset = var_id * 8;
                    f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
                }
            }
            Stmt::Unop {
                result,
                operator,
                operand,
            } => {
                generate_expr(m, &mut f, &vars, operand, Value::Register(Register::Rax));
                let offset = result.0 * 8;
                let mut out_reg = Register::Rax;
                match operator.kind {
                    TokenKind::Bang => {
                        f.push_raw_instr("xor rbx, rbx");
                        f.push_raw_instr("test rax, rax");
                        f.push_instr(Instr::Set(Cond::Z, Value::Register(Register::Bl)));
                        out_reg = Register::Rbx;
                    }
                    _ => todo!(),
                }
                f.push_raw_instr(format!("mov QWORD [rbp-{offset}], {out_reg}"));
            }
            Stmt::Binop {
                result,
                operator,
                lhs,
                rhs,
            } => {
                generate_expr(m, &mut f, &vars, lhs, Value::Register(Register::Rax));
                generate_expr(m, &mut f, &vars, rhs, Value::Register(Register::Rbx));
                let offset = result.0 * 8;
                let mut out_reg = Register::Rax;
                match operator.kind {
                    TokenKind::Plus => f.push_raw_instr("add rax, rbx"),
                    TokenKind::Minus => f.push_raw_instr("sub rax, rbx"),
                    TokenKind::Asterisk => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr("imul rbx")
                    }
                    TokenKind::Percent => {
                        f.push_raw_instr("cqo");
                        f.push_raw_instr("idiv rbx");
                        out_reg = Register::Rdx;
                    }
                    TokenKind::Slash => {
                        f.push_raw_instr("cqo");
                        f.push_raw_instr("idiv rbx");
                    }
                    TokenKind::Lt => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr("cmp rax, rbx");
                        f.push_instr(Instr::Set(Cond::L, Value::Register(Register::Dl)));
                        out_reg = Register::Rdx;
                    }
                    TokenKind::Eq => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr("cmp rax, rbx");
                        f.push_instr(Instr::Set(Cond::E, Value::Register(Register::Dl)));
                        out_reg = Register::Rdx;
                    }
                    TokenKind::NotEq => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr("cmp rax, rbx");
                        f.push_instr(Instr::Set(Cond::NE, Value::Register(Register::Dl)));
                        out_reg = Register::Rdx;
                    }
                    _ => todo!(),
                }
                f.push_raw_instr(format!("mov QWORD [rbp-{offset}], {out_reg}"));
            }
            Stmt::Assign { lhs, rhs } => match lhs {
                Expr::Var(token, VarId(var_id, is_extern)) => {
                    generate_expr(m, &mut f, &vars, rhs, Value::Register(Register::Rax));
                    if is_extern {
                        let name = token.source;
                        f.push_raw_instr(format!("mov QWORD [_{name}], rax"));
                    } else {
                        let offset = var_id * 8;
                        f.push_raw_instr(format!("mov QWORD [rbp-{offset}], rax"));
                    }
                }
                Expr::Deref(token, VarId(var_id, is_extern)) => {
                    generate_expr(m, &mut f, &vars, rhs, Value::Register(Register::Rbx));
                    if is_extern {
                        let name = token.source;
                        f.push_raw_instr(format!("mov QWORD rax, [_{name}]"));
                        f.push_raw_instr(format!("mov QWORD [rax], rbx"));
                    } else {
                        let offset = var_id * 8;
                        f.push_raw_instr(format!("mov QWORD rax, [rbp-{offset}]"));
                        f.push_raw_instr(format!("mov QWORD [rax], rbx"));
                    }
                }
                _ => todo!("{lhs:?}"),
            },
            Stmt::Block(id) => {
                f.push_block(format!("b{id}"));
            }
            Stmt::JZ(cond, id) => {
                generate_expr(m, &mut f, &vars, cond, Value::Register(Register::Rax));
                f.push_raw_instr("test rax, rax");
                f.push_raw_instr(format!("jz .b{id}"));
            }
            Stmt::Jmp(id) => {
                f.push_raw_instr(format!("jmp .b{id}"));
            }
        }
    }

    m.push_function(f);
    Ok(())
}

fn generate_expr<'src>(
    m: &mut Module,
    f: &mut Function,
    vars: &Vec<Var<'src>>,
    expr: Expr,
    val: Value,
) {
    match expr {
        Expr::StrLit(token) => {
            let i = m.data.len();
            let name = format!("str{i}");
            m.push_data(DataDef::new(
                &name,
                DataDirective::Db,
                vec![DataExpr::Str(token.unescape()), DataExpr::Const(0)],
            ));
            f.push_raw_instr(format!("mov {val}, {name}"));
        }
        Expr::IntLit(_, lit) => {
            f.push_raw_instr(format!("mov {val}, {lit}"));
        }
        Expr::Var(token, VarId(var_id, is_extern)) => {
            if is_extern {
                let name = token.source;
                f.push_raw_instr(format!("mov {val}, QWORD _{name}"));
            } else {
                let offset = var_id * 8;
                f.push_raw_instr(format!("mov {val}, QWORD [rbp-{offset}]"));
            }
        }
        Expr::Deref(token, VarId(var_id, is_extern)) => {
            if is_extern {
                let name = token.source;
                f.push_raw_instr(format!("mov {val}, QWORD _{name}"));
                f.push_raw_instr(format!("mov {val}, [{val}]"));
            } else {
                let offset = var_id * 8;
                f.push_raw_instr(format!("mov {val}, [rbp-{offset}]"));
                f.push_raw_instr(format!("mov {val}, [{val}]"));
            }
        }
        Expr::Ref(token, VarId(var_id, is_extern)) => {
            if is_extern {
                let name = token.source;
                f.push_raw_instr(format!("lea {val}, QWORD _{name}"));
            } else {
                let offset = var_id * 8;
                f.push_raw_instr(format!("lea {val}, [rbp-{offset}]"));
            }
        }
    }
}
