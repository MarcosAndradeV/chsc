use crate::ir::Program;
use crate::utils::AppError;

use std::fmt::Write;

pub fn generate(ast: Program) -> Result<String, AppError> {
    let mut out = String::new();
    // writeln!(&mut out, "#include<stdio.h>");
    for r#extern in ast.externs {
        write!(&mut out, "extern ");
        chs_to_c_type(&mut out, &r#extern.ret);
        write!(&mut out, " {}(", r#extern.name);
        for (i, arg) in r#extern.args.iter().enumerate() {
            if i > 0 {
                write!(&mut out, ", ");
            }
            chs_to_c_type(&mut out, arg);
            // write!(&mut out, " _{}", i);
        }
        if r#extern.is_variadic {
            write!(&mut out, ", ...");
        }
        write!(&mut out, ");\n");
    }

    for global_var in &ast.global_vars {
        todo!()
    }

    for func in ast.funcs {
        chs_to_c_type(&mut out, &func.ret_type);
        write!(&mut out, " {}(", func.name);
        for (i, arg) in func.args.iter().enumerate() {
            if i > 0 {
                write!(&mut out, ", ");
            }
            chs_to_c_type(&mut out, &func.args_types[i]);
            write!(&mut out, " {}", arg);
        }
        write!(&mut out, "){{\n");
        for stmt in func.body {
            match stmt {
                crate::ir::Stmt::Store { target, rhs } => todo!(),
                crate::ir::Stmt::AssignGlobalVar { var, rhs } => todo!(),
                crate::ir::Stmt::AssignVar { loc, var, rhs } => todo!(),
                crate::ir::Stmt::Return(loc, expr) => todo!(),
                crate::ir::Stmt::Unop {
                    result,
                    operator,
                    operand,
                } => todo!(),
                crate::ir::Stmt::Binop {
                    result,
                    operator,
                    lhs,
                    rhs,
                } => todo!(),
                crate::ir::Stmt::Funcall {
                    result,
                    caller,
                    args,
                } => {
                    write!(&mut out, "{caller}(");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(&mut out, ", ");
                        }
                        chs_to_c_expr(&mut out, arg);
                    }
                    write!(&mut out, ");\n");
                }
                crate::ir::Stmt::Syscall { result, args } => todo!(),
                crate::ir::Stmt::JZ(expr, _) => todo!(),
                crate::ir::Stmt::JNZ(expr, _) => todo!(),
                crate::ir::Stmt::Jmp(_) => todo!(),
                crate::ir::Stmt::Block(_) => todo!(),
            }
        }
        write!(&mut out, "}}\n");
    }
    Ok(out)
}

fn chs_to_c_expr(out: &mut String, epxr: &crate::ir::Expr<'_>) {
    match epxr {
        crate::ir::Expr::Void(loc) => todo!(),
        crate::ir::Expr::IntLit(loc, _) => todo!(),
        crate::ir::Expr::CharLit(loc, _) => todo!(),
        crate::ir::Expr::StrLit(token) => {
            write!(out, "\"{}\"", token.unescape().escape_default());
        }
        crate::ir::Expr::Var(loc, var_id) => todo!(),
        crate::ir::Expr::Global(token, _) => todo!(),
        crate::ir::Expr::Deref(loc, var_id) => todo!(),
        crate::ir::Expr::Ref(loc, var_id) => todo!(),
        crate::ir::Expr::GlobalDeref(token, _) => todo!(),
    }
}

fn chs_to_c_type(out: &mut String, r#type: &crate::ir::Type) {
    match r#type {
        crate::ir::Type::Void => write!(out, "void"),
        crate::ir::Type::Int => write!(out, "int"),
        crate::ir::Type::Bool => write!(out, "bool"),
        crate::ir::Type::Char => write!(out, "char"),
        crate::ir::Type::Ptr => write!(out, "void*"),
        crate::ir::Type::PtrTo(t) => {
            chs_to_c_type(out, t.as_ref());
            write!(out, "*")
        }
    };
}
