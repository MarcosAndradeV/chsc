use crate::ast;
use crate::ir;
use crate::utils::AppError;

pub fn compile_ast_to_ir<'src>(module: ast::Module<'src>) -> Result<ir::Program<'src>, AppError> {
    let mut p = ir::Program::default();
    let mut names_index = ir::NameSpace::default();
    names_index.push_scope();

    for f in &module.funcs {
        let uid = p.funcs.len();
        names_index.insert_var_index(f.name.source, ir::Names::Func(uid));
        let mut func = ir::Func {
            name: f.name,
            ..Default::default()
        };

        func.ret_type = f.ret_type.clone().map(convert_types).unwrap_or_default();

        p.funcs.push(func);
    }

    for f in module.externs {
        let uid = p.externs.len();
        names_index.insert_var_index(f.name.source, ir::Names::ExternFunc(uid));
        p.externs.push(ir::ExternFunc {
            name: f.name,
            args: f.args.into_iter().map(convert_types).collect(),
            is_variadic: f.is_variadic,
            ret: f.ret_type.map(convert_types).unwrap_or_default(),
        });
    }

    for f in module.funcs {
        let ir::Names::Func(uid) = *names_index.get(f.name.source).unwrap() else {
            unreachable!()
        };
        names_index.push_scope();
        let func = &mut p.funcs[uid];
        for (arg, typ) in f.args {
            func.args.push(arg);
            let ty = convert_types(typ);
            func.args_types.push(ty.clone());
            let id = ir::VarId(func.vars.len());
            names_index.insert_var_index(arg.source, ir::Names::Var(id));
            func.vars.push(ir::Var {
                loc: arg.loc,
                ty,
                is_vec: (false, 0),
            });
        }

        for stmt in f.body {
            compile_stmt(&mut names_index, stmt, &mut p, uid);
        }
        names_index.pop_scope();
    }

    Ok(p)
}

fn compile_stmt<'src>(
    names_index: &mut ir::NameSpace<'src>,
    stmt: ast::Stmt<'src>,
    p: &mut ir::Program<'src>,
    func_uid: usize,
) {
    let func = unsafe {
        ((&mut p.funcs[func_uid]) as *mut ir::Func)
            .as_mut()
            .unwrap()
    };
    match stmt {
        ast::Stmt::Return { loc, expr } => {
            let expr = expr.map(|e| compile_expr(e, func, names_index, p));
            func.body.push(ir::Stmt::Return(loc, expr));
        }
        ast::Stmt::VarDecl { name, r#type, expr } => {
            let loc = name.loc;
            let id = ir::VarId(func.vars.len());
            names_index.insert_var_index(name.source, ir::Names::Var(id));
            func.vars.push(ir::Var {
                loc: name.loc,
                ty: convert_types(r#type),
                is_vec: (false, 0),
            });
            if let Some(expr) = expr {
                let rhs = compile_expr(expr, func, names_index, p);
                func.body.push(ir::Stmt::AssignVar { loc, var: id, rhs });
            }
        }
        ast::Stmt::Expr { loc, expr } => {
            match expr {
                ast::Expr::Call { loc, caller, args } => {
                    let args = args
                        .into_iter()
                        .map(|arg| compile_expr(arg, func, names_index, p))
                        .collect();
                    let caller = match caller.as_ref() {
                        ast::Expr::Ident(ident) => *ident,
                        _ => todo!(),
                    };
                    func.body.push(ir::Stmt::Funcall {
                        result: None,
                        caller,
                        args,
                    });
                }
                ast::Expr::Syscall { loc, args } => {
                    let args = args
                        .into_iter()
                        .map(|arg| compile_expr(arg, func, names_index, p))
                        .collect();

                    func.body.push(ir::Stmt::Syscall { result: None, args });
                }
                _ => {}
            };
        }
        ast::Stmt::Assign { loc, lhs, rhs } => match compile_expr(lhs, func, names_index, p) {
            ir::Expr::Var(_, id) => {
                let rhs = compile_expr(rhs, func, names_index, p);
                func.body.push(ir::Stmt::AssignVar { loc, var: id, rhs });
            }
            _ => {}
        },
        ast::Stmt::While { loc, cond, body } => {
            let body_id = func.next_block();
            let cond_id = func.next_block();
            func.body.push(ir::Stmt::Jmp(cond_id));
            func.push_block(body_id);
            compile_stmt(names_index, *body, p, func_uid);
            func.push_block(cond_id);
            let jnz = ir::Stmt::JNZ(compile_expr(cond, func, names_index, p), body_id);
            func.body.push(jnz);
        }
        ast::Stmt::If {
            loc,
            cond,
            true_branch,
            else_branch,
        } => todo!(),
        ast::Stmt::Block(loc, stmts) => {
            names_index.push_scope();
            for stmt in stmts {
                compile_stmt(names_index, stmt, p, func_uid);
            }
            names_index.pop_scope();
        }
    }
}

fn compile_expr<'src>(
    expr: ast::Expr<'src>,
    func: &mut ir::Func<'src>,
    names_index: &ir::NameSpace<'src>,
    p: &ir::Program<'src>,
) -> ir::Expr<'src> {
    match expr {
        ast::Expr::IntLit(token) => ir::Expr::IntLit(token.loc, token.source.parse().unwrap()),
        ast::Expr::StrLit(token) => ir::Expr::StrLit(token),
        ast::Expr::Ident(token) => match names_index.get(token.source) {
            Some(ir::Names::Var(id)) => ir::Expr::Var(token.loc, *id),
            _ => todo!(),
        },
        ast::Expr::Deref(loc, expr) => match compile_expr(*expr, func, &names_index, p) {
            ir::Expr::Var(loc, var_id) => ir::Expr::Deref(loc, var_id),
            _ => todo!(),
        },
        ast::Expr::Call { loc, caller, args } => {
            let id = ir::VarId(func.vars.len());
            let args = args
                .into_iter()
                .map(|arg| compile_expr(arg, func, &names_index, p))
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
            func.body.push(ir::Stmt::Funcall {
                result: Some(id),
                caller,
                args,
            });

            func.vars.push(ir::Var {
                loc: loc,
                ty,
                is_vec: (false, 0),
            });

            ir::Expr::Var(loc, id)
        }
        ast::Expr::Syscall { loc, args } => {
            let args = args
                .into_iter()
                .map(|arg| compile_expr(arg, func, &names_index, p))
                .collect();

            let id = ir::VarId(func.vars.len());
            func.body.push(ir::Stmt::Syscall {
                result: Some(id),
                args,
            });

            func.vars.push(ir::Var {
                loc: loc,
                ty: ir::Type::Int,
                is_vec: (false, 0),
            });

            ir::Expr::Var(loc, id)
        }
        ast::Expr::Binop { operator, lhs, rhs } => {
            let id = ir::VarId(func.vars.len());
            let loc = operator.loc;

            let lhs = compile_expr(*lhs, func, names_index, p);
            let rhs = compile_expr(*rhs, func, names_index, p);

            func.body.push(ir::Stmt::Binop {
                result: id,
                operator,
                lhs,
                rhs,
            });

            func.vars.push(ir::Var {
                loc,
                ty: ir::Type::Ptr,
                is_vec: (false, 0),
            });
            ir::Expr::Var(loc, id)
        }
    }
}

fn convert_types(ast_type: ast::Type) -> ir::Type {
    match ast_type {
        ast::Type::Name(token) if token.source == "int" => ir::Type::Int,
        ast::Type::Name(token) if token.source == "char" => ir::Type::Char,
        ast::Type::PtrTo(t) => ir::Type::PtrTo(Box::new(convert_types(*t))),
        _ => todo!(),
    }
}
