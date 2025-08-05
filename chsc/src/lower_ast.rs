use std::cell::RefCell;

use crate::chslexer::TokenKind;
use crate::interpreter::{Interpreter, Value};
use crate::ir::Body;
use crate::ir::{self, type_of_expr};
use crate::utils::AppError;
use crate::{Compiler, ast};

pub fn lower_ast_to_ir<'src>(c: &Compiler<'src>) -> Result<ir::Program<'src>, AppError> {
    let mut p = ir::Program::default();
    let mut names_index = ir::NameSpace::default();

    names_index.push_scope();
    let mut interpreter = Interpreter::new();

    for module in c.modules.borrow().values() {
        for c in &module.consts {
            let mut body = Body::default();
            let v = match &c.expr {
                ast::Expr::IntLit(loc, i) => *i,
                _ => todo!(),
            };
            names_index.insert_var_index(c.name.source, ir::Names::Const(v));
        }

        for f in &module.externs {
            let uid = p.externs.len();
            names_index.insert_var_index(f.name.source, ir::Names::ExternFunc(uid));
            p.externs.push(ir::ExternFunc {
                name: f.name,
                args: f.args.iter().map(convert_types).collect(),
                is_variadic: f.is_variadic,
                ret: f.ret_type.as_ref().map(convert_types).unwrap_or_default(),
            });
        }

        for f in &module.funcs {
            let uid = p.funcs.len();
            names_index.insert_var_index(f.name.source, ir::Names::Func(uid));
            let mut func = ir::Func {
                name: f.name,
                ..Default::default()
            };

            func.ret_type = f.ret_type.as_ref().map(convert_types).unwrap_or_default();

            p.funcs.push(func);
        }

        for v in &module.global_vars {
            let uid = p.global_vars.len();
            names_index.insert_var_index(v.name.source, ir::Names::GlobalVar(uid));
            p.global_vars.push(ir::GlobalVar {
                token: v.name,
                ty: convert_types(&v.r#type),
                is_vec: v.is_vec,
            });
        }

        if !module.execs.is_empty() {
            todo!()
        }
    }
    for module in c.modules.borrow().values() {
        for f in &module.funcs {
            let ir::Names::Func(uid) = *names_index.get(f.name.source).unwrap() else {
                unreachable!()
            };
            names_index.push_scope();

            let func = &mut p.funcs[uid];
            let mut body = Body::default();
            for (arg, typ) in &f.args {
                let id = func.args.len();
                func.args.push(arg.source);
                let ty = convert_types(typ);
                func.args_types.push(ty.clone());
                let id = ir::VarId(body.vars.len());
                names_index.insert_var_index(arg.source, ir::Names::Var(id));
                body.vars.push(ir::Var {
                    loc: arg.loc,
                    ty,
                    is_vec: None,
                });
            }

            compile_stmt(&mut p, &mut names_index, &mut body, &f.body);

            let func = &mut p.funcs[uid];
            func.body = body;

            names_index.pop_scope();
        }
    }

    Ok(p)
}

fn compile_stmt<'src>(
    p: &mut ir::Program<'src>,
    names_index: &mut ir::NameSpace<'src>,
    body: &mut Body<'src>,
    stmt: &ast::Stmt<'src>,
) {
    match stmt {
        ast::Stmt::Return { loc, expr } => {
            let expr = expr.as_ref().map(|e| compile_expr(p, names_index, body, e));
            body.push(ir::Stmt::Return(*loc, expr));
        }
        ast::Stmt::VarDecl {
            name,
            is_vec,
            r#type,
            expr,
        } => {
            let loc = name.loc;
            let id = ir::VarId(body.vars.len());
            names_index.insert_var_index(name.source, ir::Names::Var(id));
            body.vars.push(ir::Var {
                loc: name.loc,
                ty: convert_types(r#type),
                is_vec: *is_vec,
            });
            if let Some(expr) = expr {
                let rhs = compile_expr(p, names_index, body, expr);
                body.push(ir::Stmt::AssignVar { loc, var: id, rhs });
            }
        }
        ast::Stmt::Expr { loc, expr } => {
            match expr {
                ast::Expr::Call { loc, caller, args } => {
                    let args = args
                        .into_iter()
                        .map(|arg| compile_expr(p, names_index, body, arg))
                        .collect();
                    let caller = match caller.as_ref() {
                        ast::Expr::Ident(ident) => *ident,
                        _ => todo!(),
                    };
                    body.push(ir::Stmt::Funcall {
                        result: None,
                        caller,
                        args,
                    });
                }
                ast::Expr::Syscall { loc, args } => {
                    let args = args
                        .into_iter()
                        .map(|arg| compile_expr(p, names_index, body, arg))
                        .collect();
                    body.push(ir::Stmt::Syscall { result: None, args });
                }
                _ => {}
            };
        }
        ast::Stmt::Assign { loc, lhs, rhs } => match compile_expr(p, names_index, body, lhs) {
            ir::Expr::Var(_, id) => {
                let rhs = compile_expr(p, names_index, body, rhs);
                body.push(ir::Stmt::AssignVar {
                    loc: *loc,
                    var: id,
                    rhs,
                });
            }
            ir::Expr::Global(token, id) => {
                let rhs = compile_expr(p, names_index, body, rhs);
                body.push(ir::Stmt::AssignGlobalVar {
                    var: (token, id),
                    rhs,
                });
            }
            target @ ir::Expr::Deref(loc, id) => {
                let rhs = compile_expr(p, names_index, body, rhs);
                body.push(ir::Stmt::Store { target, rhs });
            }
            target @ ir::Expr::GlobalDeref(loc, id) => {
                let rhs = compile_expr(p, names_index, body, rhs);
                body.push(ir::Stmt::Store { target, rhs });
            }
            _ => unreachable!(),
        },
        ast::Stmt::While {
            loc,
            cond,
            body: while_body,
        } => {
            let body_id = body.next_block();
            let cond_id = body.next_block();
            body.push(ir::Stmt::Jmp(cond_id));
            body.push_block(body_id);
            compile_stmt(p, names_index, body, while_body);
            body.push_block(cond_id);
            let jnz = ir::Stmt::JNZ(compile_expr(p, names_index, body, cond), body_id);
            body.push(jnz);
        }
        ast::Stmt::If {
            loc,
            cond,
            true_branch,
            else_branch,
        } => {
            let true_branch_id = body.next_block();
            let else_branch_id = body.next_block();
            let after_branch_id = body.next_block();

            let jz = ir::Stmt::JZ(compile_expr(p, names_index, body, cond), else_branch_id);
            body.push(jz);
            compile_stmt(p, names_index, body, true_branch);
            if let Some(else_branch) = else_branch {
                body.push(ir::Stmt::Jmp(after_branch_id));
                body.push_block(else_branch_id);
                compile_stmt(p, names_index, body, else_branch);
                body.push_block(after_branch_id);
            } else {
                body.push_block(else_branch_id);
            }
        }
        ast::Stmt::Block(loc, stmts) => {
            names_index.push_scope();
            for stmt in stmts {
                compile_stmt(p, names_index, body, stmt);
            }
            names_index.pop_scope();
        }
    }
}

fn compile_expr<'src>(
    p: &ir::Program<'src>,
    names_index: &ir::NameSpace<'src>,
    body: &mut Body<'src>,

    expr: &ast::Expr<'src>,
) -> ir::Expr<'src> {
    match expr {
        ast::Expr::IntLit(loc, lit) => ir::Expr::IntLit(*loc, *lit),
        ast::Expr::BoolLit(loc, lit) => ir::Expr::IntLit(*loc, if *lit { 1 } else { 0 }),
        ast::Expr::StrLit(token) => ir::Expr::StrLit(*token),
        ast::Expr::Ident(token) => match names_index.get(token.source) {
            Some(ir::Names::Var(id)) => ir::Expr::Var(token.loc, *id),
            Some(ir::Names::Arg(id)) => ir::Expr::Arg(token.loc, *id),
            Some(ir::Names::GlobalVar(id)) => ir::Expr::Global(*token, *id),
            Some(ir::Names::Const(lit)) => ir::Expr::IntLit(token.loc, *lit),
            None => {
                todo!(
                    "{}",
                    AppError::ParseError {
                        path: token.loc.to_string(),
                        error: format!("Undefined name `{token}`")
                    }
                    .to_string()
                );
            }
            _ => todo!(),
        },
        ast::Expr::Deref(loc, expr) => match compile_expr(p, names_index, body, expr) {
            ir::Expr::Var(loc, var_id) => ir::Expr::Deref(loc, var_id),
            ir::Expr::StrLit(token) => {
                let unescape = token.unescape();
                ir::Expr::CharLit(token.loc, unescape.chars().nth(0).unwrap_or('\0'))
            }
            ir::Expr::Global(token, uid) => ir::Expr::GlobalDeref(token, uid),
            _ => todo!(),
        },
        ast::Expr::Ref(loc, expr) => match compile_expr(p, names_index, body, expr) {
            ir::Expr::Var(loc, var_id) => ir::Expr::Ref(loc, var_id),
            _ => todo!(),
        },
        ast::Expr::Call { loc, caller, args } => {
            let id = ir::VarId(body.vars.len());
            let args = args
                .into_iter()
                .map(|arg| compile_expr(p, names_index, body, arg))
                .collect();
            let (caller, ty) = match caller.as_ref() {
                ast::Expr::Ident(ident) => {
                    let ty = match names_index.get(ident.source) {
                        Some(ir::Names::Func(uid)) => p.funcs[*uid].ret_type.clone(),
                        _ => todo!(),
                    };
                    (*ident, ty)
                }
                _ => todo!(),
            };
            body.push(ir::Stmt::Funcall {
                result: Some(id),
                caller,
                args,
            });
            let loc = *loc;
            body.vars.push(ir::Var {
                loc,
                ty,
                is_vec: None,
            });

            ir::Expr::Var(loc, id)
        }
        ast::Expr::Syscall { loc, args } => {
            let args = args
                .into_iter()
                .map(|arg| compile_expr(p, names_index, body, arg))
                .collect();

            let id = ir::VarId(body.vars.len());
            body.push(ir::Stmt::Syscall {
                result: Some(id),
                args,
            });
            let loc = *loc;
            body.vars.push(ir::Var {
                loc,
                ty: ir::Type::Int,
                is_vec: None,
            });

            ir::Expr::Var(loc, id)
        }
        ast::Expr::Binop { operator, lhs, rhs } => {
            let loc = operator.loc;

            let lhs = compile_expr(p, names_index, body, lhs);
            let rhs = compile_expr(p, names_index, body, rhs);

            let id = ir::VarId(body.vars.len());
            let ty = match operator.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    let lhs_ty = type_of_expr(&lhs, &p.global_vars, &body.vars).unwrap();
                    let rhs_ty = type_of_expr(&rhs, &p.global_vars, &body.vars).unwrap();
                    if matches!(lhs_ty, ir::Type::PtrTo(_)) {
                        lhs_ty
                    } else if matches!(rhs_ty, ir::Type::PtrTo(_)) {
                        rhs_ty
                    } else {
                        ir::Type::Int
                    }
                }
                TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => {
                    // func.body.push(ir::Stmt::ArithmeticBinop {
                    //     result: id,
                    //     operator,
                    //     lhs,
                    //     rhs,
                    // });
                    ir::Type::Int
                }

                TokenKind::Ampersand => todo!(),
                TokenKind::Pipe => todo!(),
                TokenKind::Caret => todo!(),
                TokenKind::ShiftLeft => todo!(),
                TokenKind::ShiftRight => todo!(),

                TokenKind::Eq
                | TokenKind::NotEq
                | TokenKind::Gt
                | TokenKind::GtEq
                | TokenKind::Lt
                | TokenKind::LtEq => {
                    // func.body.push(ir::Stmt::ComparisonBinop {
                    //     result: id,
                    //     operator,
                    //     lhs,
                    //     rhs,
                    // });
                    ir::Type::Bool
                }

                TokenKind::DoubleAmpersand | TokenKind::DoublePipe => {
                    // func.body.push(ir::Stmt::LogicalBinop {
                    //     result: id,
                    //     operator,
                    //     lhs,
                    //     rhs,
                    // });
                    ir::Type::Bool
                }
                _ => todo!(),
            };
            let operator = *operator;
            body.push(ir::Stmt::Binop {
                result: id,
                operator,
                lhs,
                rhs,
            });

            body.vars.push(ir::Var {
                loc,
                ty,
                is_vec: None,
            });
            ir::Expr::Var(loc, id)
        }
        ast::Expr::Index { loc, base, index } => {
            todo!()
        }
    }
}

fn convert_types(ast_type: &ast::Type) -> ir::Type {
    match ast_type {
        ast::Type::Name(token) if token.source == "int" => ir::Type::Int,
        ast::Type::Name(token) if token.source == "char" => ir::Type::Char,
        ast::Type::Name(token) if token.source == "bool" => ir::Type::Bool,
        ast::Type::Name(token) if token.source == "ptr" => ir::Type::Ptr,
        ast::Type::Name(token) if token.source == "void" => ir::Type::Void,
        ast::Type::PtrTo(t) => ir::Type::PtrTo(Box::new(convert_types(t))),
        _ => todo!("{ast_type:?}"),
    }
}
