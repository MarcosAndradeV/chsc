use crate::ir::{Program, VarId};
use crate::utils::AppError;

use std::fmt::Write;

pub fn generate(ast: Program) -> Result<String, AppError> {
    let mut out = String::new();
    for r#extern in ast.externs {
        write!(&mut out, "extern ");
        chs_to_c_type(&mut out, &r#extern.ret);
        write!(&mut out, " {}(", r#extern.name);
        for (i, arg) in r#extern.args.iter().enumerate() {
            if i > 0 {
                write!(&mut out, ", ");
            }
            chs_to_c_type(&mut out, arg);
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
        write!(&mut out, " _{}(", func.name);
        for (i, arg) in func.args.iter().enumerate() {
            if i > 0 {
                write!(&mut out, ", ");
            }
            chs_to_c_type(&mut out, &func.args_types[i]);
            write!(&mut out, " {}", arg);
        }
        write!(&mut out, "){{\n");
        for (id, var) in func.vars.iter().enumerate() {
            chs_to_c_type(&mut out, &var.ty);
            write!(&mut out, " _{}", id);
            write!(&mut out, ";\n");
        }
        for stmt in func.body {
            match stmt {
                crate::ir::Stmt::Store { target, rhs } => {
                    chs_to_c_expr(&mut out, &target);
                    write!(&mut out, " = ");
                    chs_to_c_expr(&mut out, &rhs);
                    write!(&mut out, ";\n");
                }
                crate::ir::Stmt::AssignGlobalVar { var, rhs } => todo!(),
                crate::ir::Stmt::AssignVar {
                    loc,
                    var: VarId(id),
                    rhs,
                } => {
                    write!(&mut out, "_{} = ", id);
                    chs_to_c_expr(&mut out, &rhs);
                    write!(&mut out, ";\n");
                }
                crate::ir::Stmt::Return(loc, Some(expr)) => {
                    write!(&mut out, "return(");
                    chs_to_c_expr(&mut out, &expr);
                    write!(&mut out, ");\n");
                }
                crate::ir::Stmt::Return(loc, None) => {
                    write!(&mut out, "return;\n");
                }
                crate::ir::Stmt::Unop {
                    result: VarId(id),
                    operator,
                    operand,
                } => {
                    write!(&mut out, "_{} = ", id);
                    write!(&mut out, "{operator}(");
                    chs_to_c_expr(&mut out, &operand);
                    write!(&mut out, ");\n");
                }
                crate::ir::Stmt::Binop {
                    result: VarId(id),
                    operator,
                    lhs,
                    rhs,
                } => {
                    write!(&mut out, "_{} = ", id);
                    chs_to_c_expr(&mut out, &lhs);
                    write!(&mut out, " {operator} ");
                    chs_to_c_expr(&mut out, &rhs);
                    write!(&mut out, ";\n");
                }
                crate::ir::Stmt::Funcall {
                    result,
                    caller,
                    args,
                } => {
                    if let Some(VarId(id)) = result {
                        write!(&mut out, "_{} = ", id);
                    }
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
                crate::ir::Stmt::JZ(expr, id) => {
                    write!(&mut out, "if(");
                    chs_to_c_expr(&mut out, &expr);
                    write!(&mut out, "){{\n");
                    write!(&mut out, "goto b{id};\n");
                    write!(&mut out, "}}\n");
                }
                crate::ir::Stmt::JNZ(expr, id) => {
                    write!(&mut out, "if(!(");
                    chs_to_c_expr(&mut out, &expr);
                    write!(&mut out, ")){{\n");
                    write!(&mut out, "goto b{id};\n");
                    write!(&mut out, "}}\n");
                }
                crate::ir::Stmt::Jmp(id) => {
                    write!(&mut out, "goto b{id};\n");
                }
                crate::ir::Stmt::Block(id) => {
                    write!(&mut out, "b{id}:\n");
                }
            }
        }
        write!(&mut out, "}}\n");
    }

    write!(&mut out, "int main() {{\n");
    write!(&mut out, "_main();\n");
    write!(&mut out, "return 0;\n");
    write!(&mut out, "}}\n");
    Ok(out)
}

fn chs_to_c_expr(out: &mut String, epxr: &crate::ir::Expr<'_>) {
    match epxr {
        crate::ir::Expr::Void(loc) => todo!(),
        crate::ir::Expr::IntLit(_, lit) => {
            write!(out, "{lit}");
        }
        crate::ir::Expr::CharLit(_, lit) => {
            write!(out, "'{}'", lit.escape_default());
        }
        crate::ir::Expr::StrLit(token) => {
            write!(out, "\"{}\"", token.unescape().escape_default());
        }
        crate::ir::Expr::Var(loc, VarId(id)) => {
            write!(out, "_{}", id);
        }
        crate::ir::Expr::Global(token, _) => todo!(),
        crate::ir::Expr::Deref(loc, VarId(id)) => {
            write!(out, "*_{}", id);
        }
        crate::ir::Expr::Ref(loc, VarId(id)) => {
            write!(out, "&_{}", id);
        }
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
