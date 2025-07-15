use crate::{
    ir::*,
    chslexer::TokenKind,
    fasm_backend::{
        Cond, DataDef, DataDirective, DataExpr, Function, Instr, Module, Register, SizeOperator,
        Value,
    },
    utils::AppError,
};

macro_rules! anno_asm {
    ($anno:expr, $f:expr, $($msg: expr),*) => {
        if $anno {
            $f.push_raw_instr(
                format!(";; {}",
                format_args!($($msg),*)
            ));
        }
    };
}

macro_rules! raw_instr {
    ($f:expr, $inst:expr) => {
        $f.push_raw_instr(format!("{}", format_args!($inst)));
    };
}

pub fn generate(ast: Program, use_c: bool) -> Result<Module, AppError> {
    let mut m = Module::new(use_c);

    for r#extern in ast.externs {
        m.push_extrn(r#extern.name.source);
    }

    for global_var in &ast.global_vars {
        let size = if let Some(sz) = global_var.is_vec {
            global_var.ty.size() * sz
        } else {
            global_var.ty.size()
        };
        m.bss.push(DataDef::new(
            format!("_{}", global_var.token.source),
            DataDirective::Rb,
            vec![DataExpr::Const(size as u64)],
        ));
    }

    for func in ast.funcs {
        let mut f = Function::new(m.link_with_c, func.name.source);

        let (size, offsets) = calculate_stack_offsets(&func.vars);
        f.allocate_stack(size);
        f.push_block("b");
        if func.ret_type != Type::Void {
            f.epiloge = false;
        }

        let call_convention = Register::get_call_convention();
        if func.args.len() >= call_convention.len() {
            return Err(AppError::GenerationError(
                Some("Functions with more than 6 arguments are not supported yet.".to_string()),
                "Args via stack are not implemented yet".to_string(),
            ));
        }

        for (id, reg) in func
            .args
            .iter()
            .enumerate()
            .map(|arg| arg.0)
            .zip(call_convention)
        {
            let offset = offsets[id];
            f.push_raw_instr(format!("mov [rbp-{offset}], {reg}"));
        }

        let anno = true;

        let mut ctx = GenCtx {
            f: &mut f,
            m: &mut m,
            global_vars: &ast.global_vars,
            vars: &func.vars,
            offets: &offsets,
        };

        for stmt in func.body {
            match stmt {
                Stmt::Return(loc, expr) => {
                    anno_asm!(anno, ctx.f, "Return@{loc}");
                    if let Some(expr) = expr {
                        mov_to_reg(&mut ctx, expr, Register::Rax);
                    }
                    ctx.f.push_raw_instr("mov rsp, rbp");
                    ctx.f.push_raw_instr("pop rbp");
                    ctx.f.push_raw_instr("ret");
                }
                Stmt::AssignVar {
                    var: VarId(id),
                    rhs,
                    ..
                } => {
                    let r = Register::Rax;
                    mov_to_reg(&mut ctx, rhs, r);
                    let offset = ctx.get_offset(id);
                    raw_instr!(ctx.f, "mov [rbp-{offset}], {r}");
                }
                Stmt::Store { target, rhs } => {
                    let rhs_ty = ctx.type_of_expr(&rhs)?;
                    let reg = Register::Rax;
                    mov_to_reg(&mut ctx, rhs, reg);
                    match target {
                        Expr::Deref(token, VarId(id)) => {
                            let offset = ctx.get_offset(id);
                            let s = size_of_type(ctx.get_type_of_var(id).get_inner());
                            let reg = s.register_for_size(reg);
                            raw_instr!(ctx.f, "mov rbx, [rbp-{offset}]");
                            raw_instr!(ctx.f, "mov {s} [rbx], {reg}");
                        }
                        Expr::GlobalDeref(token, uid) => {
                            todo!()
                        }
                        _ => unreachable!(),
                    }
                }
                Stmt::AssignGlobalVar { var, rhs } => todo!(),
                Stmt::Unop {
                    result,
                    operator,
                    operand,
                } => todo!(),
                Stmt::Binop {
                    result: VarId(id),
                    operator,
                    lhs,
                    rhs,
                } => {
                    let offset = ctx.get_offset(id);
                    let sty = size_of_type(ctx.get_type_of_var(id));
                    let l = Register::Rax;
                    let r = Register::Rbx;
                    let mut out = l;
                    match operator.kind {
                        TokenKind::Plus => {
                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);
                            raw_instr!(ctx.f, "add {l}, {r}");
                            // out = l;
                        }
                        TokenKind::Minus => {
                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);
                            raw_instr!(ctx.f, "sub {l}, {r}");
                            // out = l;
                        }
                        TokenKind::Asterisk => {
                            raw_instr!(ctx.f, "xor rdx, rdx");
                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);
                            raw_instr!(ctx.f, "imul {r}");
                            // out = l;
                        }
                        TokenKind::Slash => {
                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);
                            raw_instr!(ctx.f, "cqo");
                            raw_instr!(ctx.f, "idiv {r}");
                            // out = l;
                        }
                        TokenKind::Percent => {
                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);
                            raw_instr!(ctx.f, "cqo");
                            raw_instr!(ctx.f, "idiv {r}");
                            out = Register::Rdx;
                        }
                        TokenKind::Eq => {
                            raw_instr!(ctx.f, "xor rdx, rdx");

                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);

                            raw_instr!(ctx.f, "cmp {l}, {r}");
                            raw_instr!(ctx.f, "sete dl");
                            out = Register::Rdx;
                        }
                        TokenKind::NotEq => {
                            raw_instr!(ctx.f, "xor rdx, rdx");

                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);

                            raw_instr!(ctx.f, "cmp {l}, {r}");
                            raw_instr!(ctx.f, "setne dl");
                            out = Register::Rdx;
                        }
                        TokenKind::Lt => {
                            raw_instr!(ctx.f, "xor rdx, rdx");

                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);

                            raw_instr!(ctx.f, "cmp {l}, {r}");
                            raw_instr!(ctx.f, "setl dl");
                            out = Register::Rdx;
                        }
                        TokenKind::Gt => {
                            raw_instr!(ctx.f, "xor rdx, rdx");

                            mov_to_reg(&mut ctx, lhs, l);
                            mov_to_reg(&mut ctx, rhs, r);

                            raw_instr!(ctx.f, "cmp {l}, {r}");
                            raw_instr!(ctx.f, "setg dl");
                            out = Register::Rdx;
                        }
                        _ => todo!("{operator}"),
                    }
                    raw_instr!(ctx.f, "mov [rbp-{offset}], {out}");
                }
                Stmt::Funcall {
                    result,
                    caller,
                    args,
                } => {
                    let call_convention = Register::get_call_convention();
                    if args.len() >= call_convention.len() {
                        return Err(AppError::GenerationError(
                            Some(
                                "Functions with more than 6 arguments are not supported yet."
                                    .to_string(),
                            ),
                            "Args via stack are not implemented yet".to_string(),
                        ));
                    }

                    let regs = call_convention.into_iter();
                    for (expr, reg) in args.into_iter().zip(regs).rev() {
                        mov_to_reg(&mut ctx, expr, reg);
                    }

                    // x86_64 Linux ABI passes the amount of floating point args via al.
                    raw_instr!(ctx.f, "mov al, 0");

                    raw_instr!(ctx.f, "call _{caller}");
                    if let Some(result) = result {
                        let offset = offsets[result.0];
                        raw_instr!(ctx.f, "mov [rbp-{offset}], rax");
                    }
                }
                Stmt::Syscall { result, args } => {
                    let call_convention = Register::get_syscall_call_convention();

                    if args.len() < 1 {
                        return Err(AppError::GenerationError(
                            None,
                            "Syscall".to_string(),
                        ));
                    }

                    let args_len = args.len() - 1;

                    let regs = call_convention.into_iter();
                    for (expr, reg) in args.into_iter().zip(regs).rev() {
                        mov_to_reg(&mut ctx, expr, reg);
                    }

                    raw_instr!(ctx.f, "syscall");
                    if let Some(result) = result {
                        let offset = offsets[result.0];
                        raw_instr!(ctx.f, "mov [rbp-{offset}], rax");
                    }
                }
                Stmt::Block(id) => {
                    ctx.f.push_block(format!("b{id}"));
                }
                Stmt::JZ(cond, id) => {
                    let l = Register::Rax;
                    raw_instr!(ctx.f, "xor rax, rax");
                    mov_to_reg(&mut ctx, cond, l);
                    raw_instr!(ctx.f, "test {l}, {l}");
                    raw_instr!(ctx.f, "jz .b{id}");
                }
                Stmt::JNZ(cond, id) => {
                    let l = Register::Rax;
                    raw_instr!(ctx.f, "xor rax, rax");
                    mov_to_reg(&mut ctx, cond, l);
                    raw_instr!(ctx.f, "test {l}, {l}");
                    raw_instr!(ctx.f, "jnz .b{id}");
                }
                Stmt::Jmp(id) => {
                    raw_instr!(ctx.f, "jmp .b{id}");
                }
            }
        }

        m.push_function(f);
    }
    Ok(m)
}

struct GenCtx<'ctx, 'src> {
    f: &'ctx mut Function,
    m: &'ctx mut Module,
    global_vars: &'ctx [GlobalVar<'src>],
    vars: &'ctx [Var<'src>],
    offets: &'ctx [usize],
}

impl GenCtx<'_, '_> {
    fn type_of_expr(&self, expr: &Expr<'_>) -> Result<Type, AppError> {
        type_of_expr(&expr, self.global_vars, self.vars)
    }

    fn get_offset(&self, id: usize) -> usize {
        self.offets[id]
    }

    fn get_type_of_var(&self, id: usize) -> &Type {
        &self.vars[id].ty
    }
}

fn mov_to_reg(ctx: &mut GenCtx, expr: Expr<'_>, reg: Register) -> Result<(), AppError> {
    // let ty = ctx.type_of_expr(&expr)?;
    // let sty = size_of_type(&ty);
    match expr {
        Expr::IntLit(loc, lit) => {
            raw_instr!(ctx.f, "mov {reg}, {lit}");
            Ok(())
        }
        Expr::CharLit(loc, lit) => {
            let lit = lit as i32;
            raw_instr!(ctx.f, "mov {reg}, {lit}");
            Ok(())
        }
        Expr::Var(token, VarId(id)) => {
            let offset = ctx.get_offset(id);
            raw_instr!(ctx.f, "mov {reg}, [rbp-{offset}]");
            Ok(())
        }
        Expr::Void(loc) => todo!(),
        Expr::StrLit(token) => {
            let n = ctx.m.rodata.len();
            let name = format!("str{n}");
            ctx.m.push_rodata(DataDef::new(
                &name,
                DataDirective::Db,
                vec![DataExpr::Str(token.unescape()), DataExpr::Const(0)],
            ));
            raw_instr!(ctx.f, "mov {reg}, {name}");
            Ok(())
        }
        Expr::Global(token, uid) => {
            if ctx.global_vars[uid].is_vec.is_some() {
                raw_instr!(ctx.f, "mov {reg}, _{token}");
            } else {
                raw_instr!(ctx.f, "mov {reg}, [_{token}]");
            }
            Ok(())
        }
        Expr::Deref(token, VarId(id)) => {
            let offset = ctx.get_offset(id);
            let s = size_of_type(ctx.get_type_of_var(id).get_inner());
            raw_instr!(ctx.f, "mov {reg}, [rbp-{offset}]");
            match s {
                SizeOperator::Byte => {
                    raw_instr!(ctx.f, "movzx {reg}, {s} [{reg}]");
                }
                SizeOperator::Word => todo!(),
                SizeOperator::Dword => {
                    raw_instr!(ctx.f, "mov {reg}, [{reg}]");
                }
                SizeOperator::Qword => {
                    raw_instr!(ctx.f, "mov {reg}, [{reg}]");
                }
                _ => todo!(),
            }

            Ok(())
        }
        Expr::Ref(token, VarId(id)) => {
            let offset = ctx.get_offset(id);
            raw_instr!(ctx.f, "lea {reg}, [rbp-{offset}]");
            Ok(())
        }
        Expr::GlobalDeref(token, uid) => {
            let global_var = &ctx.global_vars[uid];
            if global_var.is_vec.is_some() {
                raw_instr!(ctx.f, "mov {reg}, _{token}");
            } else {
                raw_instr!(ctx.f, "mov {reg}, [_{token}]");
            }
            let s = size_of_type(&global_var.ty);
            match s {
                SizeOperator::Byte => {
                    raw_instr!(ctx.f, "movzx {reg}, {s} [{reg}]");
                }
                SizeOperator::Word => todo!(),
                SizeOperator::Dword => {
                    raw_instr!(ctx.f, "mov {reg}, [{reg}]");
                }
                SizeOperator::Qword => {
                    raw_instr!(ctx.f, "mov {reg}, [{reg}]");
                }
                _ => todo!(),
            }

            Ok(())
        }
    }
}

// fn generate_func(
//     func: Func<'_>,
//     f: &mut Function,
//     m: &mut Module,
//     global_vars: &Vec<GlobalVar<'_>>,
// ) -> Result<(), AppError> {
//     let (size, offsets) = calculate_stack_offsets(&func.vars);
//     f.allocate_stack(size);
//     f.push_block("b");

//     let call_convention = Register::get_call_convention();
//     if func.args.len() >= call_convention.len() {
//         return Err(AppError::GenerationError(
//             Some("Functions with more than 6 arguments are not supported yet.".to_string()),
//             "Args via stack are not implemented yet".to_string(),
//         ));
//     }

//     for (id, reg) in func
//         .args
//         .iter()
//         .enumerate()
//         .map(|arg| arg.0)
//         .zip(call_convention)
//     {
//         let offset = offsets[id];
//         f.push_raw_instr(format!("mov [rbp-{offset}], {reg}"));
//     }

//     for stmt in func.body {
//         match stmt {
//             Stmt::Return(loc, expr) => {
//                 if let Some(expr) = expr {
//                     mov_expr_to_reg(
//                         m,
//                         f,
//                         &global_vars,
//                         &func.vars,
//                         &offsets,
//                         expr,
//                         Register::Rax,
//                     )?;
//                 }
//                 f.push_raw_instr(format!(";; Return@{loc}"));
//                 f.push_raw_instr("mov rsp, rbp");
//                 f.push_raw_instr("pop rbp");
//                 f.push_raw_instr("ret");
//             }
//             Stmt::Store { target, rhs } => {
//                 mov_expr_to_reg(m, f, &global_vars, &func.vars, &offsets, rhs, Register::Rax)?;
//                 f.push_raw_instr(format!(";; Store@{loc}", loc = target.loc()));
//                 match target {
//                     Expr::Deref(token, VarId(id)) => {
//                         let offset = offsets[id];
//                         let s = size_of_type(func.vars[id].ty.get_inner());
//                         let out = s.register_for_size(Register::Rax);
//                         f.push_raw_instr(format!("mov rbx, [rbp-{offset}]"));
//                         f.push_raw_instr(format!("mov {s} [rbx], {out}"));
//                     }
//                     Expr::GlobalDeref(token, uid) => {
//                         let ty = &global_vars[uid].ty;
//                         let s = size_of_type(ty);
//                         let out = s.register_for_size(Register::Rax);
//                         f.push_raw_instr(format!("mov {s} [_{token}], {out}"));
//                     }
//                     _ => unreachable!(),
//                 }
//             }
//             Stmt::AssignGlobalVar {
//                 var: (token, uid),
//                 rhs,
//             } => {
//                 mov_expr_to_reg(m, f, &global_vars, &func.vars,&offsets,  rhs, Register::Rax)?;
//                 f.push_raw_instr(format!(";; AssignVar@{loc}", loc = token.loc));
//                 let s = size_of_type(&global_vars[uid].ty);
//                 let r = s.register_for_size(Register::Rax);
//                 f.push_raw_instr(format!("mov {s} [_{token}], {r}"));
//             }
//             Stmt::AssignVar {
//                 var: (token, VarId(id)),
//                 rhs,
//             } => {
//                 let mut r = UinitValue::new(Value::from(Register::Rax));
//                 generate_expr(m, f, &global_vars, &offsets, &func.vars, rhs, &mut r)?;
//                 f.push_raw_instr(format!(";; AssignVar@{loc}", loc = token.loc));
//                 let offset = offsets[id];
//                 f.push_raw_instr(format!("mov [rbp-{offset}], {r}"));
//             }
//             Stmt::Unop {
//                 result,
//                 operator,
//                 operand,
//             } => {
//                 let mut l = UinitValue::new(Value::from(Register::Rax));
//                 generate_expr(m, f, &global_vars, &offsets, &func.vars, operand, &mut l)?;

//                 match operator.kind {
//                     TokenKind::Bang => {
//                         f.push_raw_instr("xor rdx, rdx");
//                         f.push_raw_instr(format!("test {l}, {l}"));
//                         f.push_instr(Instr::Set(Cond::Z, Value::Register(Register::Bl)));
//                         l.init(Value::from(Register::Rbx));
//                     }
//                     _ => todo!(),
//                 }

//                 let offset = offsets[result.0];
//                 f.push_raw_instr(format!("mov [rbp-{offset}], {l}"));
//             }
//             Stmt::Binop {
//                 result,
//                 operator,
//                 lhs,
//                 rhs,
//             } => {
//                 let mut l = UinitValue::new(Value::from(Register::Rax));
//                 generate_expr(m, f, &global_vars, &offsets, &func.vars, lhs, &mut l)?;
//                 let mut r = UinitValue::new(Value::from(Register::Rbx));
//                 generate_expr(m, f, &global_vars, &offsets, &func.vars, rhs, &mut r)?;

//                 match operator.kind {
//                     TokenKind::Plus => {
//                         f.push_raw_instr(format!("add {l}, {r}"));
//                     }
//                     TokenKind::Minus => f.push_raw_instr(format!("sub {l}, {r}")),
//                     TokenKind::Pipe | TokenKind::DoublePipe => {
//                         f.push_raw_instr(format!("or {l}, {r}"))
//                     }
//                     TokenKind::Asterisk => {
//                         f.push_raw_instr("xor rdx, rdx");
//                         f.push_raw_instr(format!("imul {r}"))
//                     }
//                     TokenKind::Percent => {
//                         f.push_raw_instr("cqo");
//                         f.push_raw_instr(format!("idiv {r}"));
//                         l.init(Value::from(Register::Rdx));
//                     }
//                     TokenKind::Slash => {
//                         f.push_raw_instr("cqo");
//                         f.push_raw_instr(format!("idiv {r}"));
//                     }
//                     TokenKind::Lt => {
//                         f.push_raw_instr("xor rdx, rdx");
//                         f.push_raw_instr(format!("cmp {l}, {r}"));
//                         f.push_instr(Instr::Set(Cond::L, Value::Register(Register::Dl)));
//                         l.init(Value::from(Register::Rdx));
//                     }
//                     TokenKind::LtEq => {
//                         f.push_raw_instr("xor rdx, rdx");
//                         f.push_raw_instr(format!("cmp {l}, {r}"));
//                         f.push_instr(Instr::Set(Cond::LE, Value::Register(Register::Dl)));
//                         l.init(Value::from(Register::Rdx));
//                     }
//                     TokenKind::Gt => {
//                         f.push_raw_instr("xor rdx, rdx");
//                         f.push_raw_instr(format!("cmp {l}, {r}"));
//                         f.push_instr(Instr::Set(Cond::G, Value::Register(Register::Dl)));
//                         l.init(Value::from(Register::Rdx));
//                     }
//                     TokenKind::GtEq => {
//                         f.push_raw_instr("xor rdx, rdx");
//                         f.push_raw_instr(format!("cmp {l}, {r}"));
//                         f.push_instr(Instr::Set(Cond::GE, Value::Register(Register::Dl)));
//                         l.init(Value::from(Register::Rdx));
//                     }
//                     TokenKind::Eq => {
//                         f.push_raw_instr("xor rdx, rdx");
//                         f.push_raw_instr(format!("cmp {l}, {r}"));
//                         f.push_instr(Instr::Set(Cond::E, Value::Register(Register::Dl)));
//                         l.init(Value::from(Register::Rdx));
//                     }
//                     TokenKind::NotEq => {
//                         f.push_raw_instr("xor rdx, rdx");
//                         f.push_raw_instr(format!("cmp {l}, {r}"));
//                         f.push_instr(Instr::Set(Cond::NE, Value::Register(Register::Dl)));
//                         l.init(Value::from(Register::Rdx));
//                     }
//                     _ => todo!("Operator {:?}", operator.kind),
//                 }

//                 let offset = offsets[result.0];
//                 f.push_raw_instr(format!("mov [rbp-{offset}], {l}"));
//             }
//             Stmt::Funcall {
//                 result,
//                 caller,
//                 args,
//             } => {
//                 let call_convention = Register::get_call_convention();
//                 if args.len() >= call_convention.len() {
//                     return Err(AppError::GenerationError(
//                         Some(
//                             "Functions with more than 6 arguments are not supported yet."
//                                 .to_string(),
//                         ),
//                         "Args via stack are not implemented yet".to_string(),
//                     ));
//                 }

//                 let regs = call_convention.into_iter();
//                 for (expr, reg) in args.into_iter().zip(regs).rev() {
//                     let mut dst = UinitValue::new(Value::from(reg));
//                     generate_expr(m, f, &global_vars, &offsets, &func.vars, expr, &mut dst)?;
//                 }

//                 // x86_64 Linux ABI passes the amount of floating point args via al.
//                 f.push_raw_instr("mov al, 0");

//                 f.push_raw_instr(format!("call _{caller}"));
//                 if let Some(result) = result {
//                     let offset = offsets[result.0];
//                     f.push_raw_instr(format!("mov [rbp-{offset}], rax"));
//                 }
//             }
//             Stmt::Syscall { .. } => todo!(),
//             Stmt::Block(id) => {
//                 f.push_block(format!("b{id}"));
//             }
//             Stmt::JZ(cond, id) => {
//                 let mut val = UinitValue::new(Value::from(Register::Rax));
//                 generate_expr(m, f, &global_vars, &offsets, &func.vars, cond, &mut val)?;
//                 f.push_raw_instr("test rax, rax");
//                 f.push_raw_instr(format!("jz .b{id}"));
//             }
//             Stmt::Jmp(id) => {
//                 f.push_raw_instr(format!("jmp .b{id}"));
//             }
//             _ => todo!(),
//         }
//     }
//     Ok(())
// }

// fn mov_expr_to_reg(
//     m: &mut Module,
//     f: &mut Function,
//     global_vars: &Vec<GlobalVar<'_>>,
//     vars: &[Var<'_>],
//     offsets: &Vec<usize>,

//     expr: Expr<'_>,
//     reg: Register,
// ) -> Result<Register, AppError> {
//     match expr {
//         Expr::IntLit(loc, lit) => {
//             f.push_raw_instr(format!(";; IntLit@{loc}"));
//             f.push_raw_instr(format!("mov {reg}, {lit}"));
//             reg

//         }
//         Expr::StrLit(lit) => {
//             f.push_raw_instr(format!(";; StrLit@{loc}", loc = lit.loc));
//             let n = m.rodata.len();
//             let name = format!("str{n}");
//             m.push_rodata(DataDef::new(
//                 &name,
//                 DataDirective::Db,
//                 vec![DataExpr::Str(lit.unescape()), DataExpr::Const(0)],
//             ));
//             f.push_raw_instr(format!("mov {reg}, {name}"));
//             reg
//         }
//         Expr::Var(token, VarId(id)) => {
//             f.push_raw_instr(format!(";; Var({id})@{loc}", loc = token.loc));
//             let offset = offsets[id];
//             f.push_raw_instr(format!("mov {reg}, [rbp-{offset}]"));
//         }
//         Expr::Ref(token, VarId(id)) => {
//             f.push_raw_instr(format!(";; Ref({id})@{loc}", loc = token.loc));
//             let offset = offsets[id];
//             f.push_raw_instr(format!("lea {reg}, [rbp-{offset}]"));
//         }
//         Expr::Deref(token, VarId(id)) => {
//             f.push_raw_instr(format!(";; Deref({id})@{loc}", loc = token.loc));
//             let offset = offsets[id];
//             let s = size_of_type(vars[id].ty.get_inner());
//             let out = s.register_for_size(reg);
//             f.push_raw_instr(format!("mov {reg}, [rbp-{offset}]"));
//             f.push_raw_instr(format!("mov {out}, {s} [{reg}]"));
//         }
//         Expr::GlobalDeref(token, uid) => {
//             f.push_raw_instr(format!(";; GlobalDeref({token})@{loc}", loc = token.loc));
//             let global_var = &global_vars[uid];
//             let s = size_of_type(&global_var.ty);
//             let out = s.register_for_size(reg);
//             if global_var.is_vec.0 {
//                 f.push_raw_instr(format!("mov {reg}, _{token}"));
//             } else {
//                 f.push_raw_instr(format!("mov {reg}, [_{token}]"));
//             }
//             f.push_raw_instr(format!("mov {out}, {s} [{reg}]"));
//         }
//         Expr::Global(token, uid) => {
//             f.push_raw_instr(format!(";; Global@{loc}", loc = token.loc));
//             let global_var = &global_vars[uid];
//             let s = size_of_type(&global_var.ty);
//             let out = s.register_for_size(reg);
//             if global_var.is_vec.0 {
//                 f.push_raw_instr(format!("mov {reg}, _{token}"));
//             } else {
//                 f.push_raw_instr(format!("mov {out}, {s} [_{token}]"));
//             }
//         }
//         Expr::Cast(_, _, expr) => {
//             mov_expr_to_reg(m, f, global_vars, vars, offsets, *expr, reg)
//         }
//         Expr::Void(_) => todo!(),
//         Expr::Index { loc, base, index } => todo!(),
//     }
// }

// #[derive(Debug, Default)]
// struct UinitValue(Option<Value>);
// impl UinitValue {
//     fn init(&mut self, lit: Value) {
//         self.0 = Some(lit)
//     }

//     fn new(val: Value) -> Self {
//         Self(Some(val))
//     }
// }

// impl std::fmt::Display for UinitValue {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         if let Some(val) = &self.0 {
//             write!(f, "{val}")
//         } else {
//             write!(f, ";; Uinit-value")
//         }
//     }
// }

fn calculate_stack_offsets(vars: &[Var<'_>]) -> (usize, Vec<usize>) {
    let mut offsets = vec![0; vars.len()];
    let mut offset = 0usize;
    for (i, var) in vars.iter().enumerate() {
        let size = var.ty.size();
        if let Some(sz)  = var.is_vec{
            let size = size * sz;
            offsets[i] = offset;
            offset += if size <= 8 { 8 } else { size };
        } else {
            offset += if size <= 8 { 8 } else { size };
            offsets[i] = offset;
        }
    }
    (offset, offsets)
}

fn size_of_type(ty: &Type) -> SizeOperator {
    match ty {
        Type::Char => SizeOperator::Byte,
        // Type::Int16 => SizeOperator::Word,
        Type::Int | Type::Bool => SizeOperator::Dword,
        Type::Ptr => SizeOperator::Qword,
        Type::PtrTo(inner) => size_of_type(inner.as_ref()),
        _ => todo!(),
    }
}
