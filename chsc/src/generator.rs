use crate::{
    ast::*,
    chslexer::TokenKind,
    fasm_backend::{
        Cond, DataDef, DataDirective, DataExpr, Function, Instr, Module, Register, SizeOperator,
        Value,
    },
    utils::AppError,
};

pub fn generate(ast: Program, use_c: bool) -> Result<Module, AppError> {
    let mut m = Module::new(use_c);

    for r#extern in ast.externs {
        match r#extern {
            Extern::Symbol(token) => m.push_extrn(token.source),
            Extern::Func(func) => todo!(),
            Extern::Var(var) => todo!(),
        }
    }

    for func in ast.funcs {
        let mut f = Function::new(m.link_with_c, func.name.source);
        generate_func(func, &mut f, &mut m);

        m.push_function(f);
    }
    Ok(m)
}

fn generate_func(func: Func<'_>, f: &mut Function, m: &mut Module) -> Result<(), AppError> {
    let (size, offsets) = calculate_stack_offsets(&func.vars);
    f.allocate_stack(size);
    f.push_block("b");

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

    for stmt in func.body {
        match stmt {
            Stmt::Return(loc, expr) => {
                if let Some(expr) = expr {
                    let mut val = UinitValue::new(Value::from(Register::Rax));
                    generate_expr(m, f, &offsets, expr, &mut val)?;
                }
                f.push_raw_instr(format!(";; Return@{loc}"));
                f.push_raw_instr("mov rsp, rbp");
                f.push_raw_instr("pop rbp");
                f.push_raw_instr("ret");
            }
            Stmt::Store {
                target: (token, VarId(id)),
                rhs,
            } => {
                let mut r = UinitValue::new(Value::from(Register::Rax));
                generate_expr(m, f, &offsets, rhs, &mut r)?;
                f.push_raw_instr(format!(";; Store@{loc}", loc = token.loc));
                let offset = offsets[id];
                f.push_raw_instr(format!("mov rbx, [rbp-{offset}]"));
                f.push_raw_instr(format!("mov [rbx], {r}"));
            }
            Stmt::AssignVar {
                var: (token, VarId(id)),
                rhs,
            } => {
                let mut r = UinitValue::new(Value::from(Register::Rax));
                generate_expr(m, f, &offsets, rhs, &mut r)?;
                f.push_raw_instr(format!(";; AssignVar@{loc}", loc = token.loc));
                let offset = offsets[id];
                f.push_raw_instr(format!("mov [rbp-{offset}], {r}"));
            }
            Stmt::Unop { .. } => todo!(),
            Stmt::Binop {
                result,
                operator,
                lhs,
                rhs,
            } => {
                let mut l = UinitValue::new(Value::from(Register::Rax));
                generate_expr(m, f, &offsets, lhs, &mut l)?;
                let mut r = UinitValue::new(Value::from(Register::Rbx));
                generate_expr(m, f, &offsets, rhs, &mut r)?;

                match operator.kind {
                    TokenKind::Plus => {
                        f.push_raw_instr(format!("add {l}, {r}"));
                    }
                    TokenKind::Minus => f.push_raw_instr(format!("sub {l}, {r}")),
                    TokenKind::Asterisk => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr(format!("imul {r}"))
                    }
                    TokenKind::Percent => {
                        f.push_raw_instr("cqo");
                        f.push_raw_instr(format!("idiv {r}"));
                        l.init(Value::from(Register::Rdx));
                    }
                    TokenKind::Slash => {
                        f.push_raw_instr("cqo");
                        f.push_raw_instr(format!("idiv {r}"));
                    }
                    TokenKind::Lt => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr(format!("cmp {l}, {r}"));
                        f.push_instr(Instr::Set(Cond::L, Value::Register(Register::Dl)));
                        l.init(Value::from(Register::Rdx));
                    }
                    TokenKind::Eq => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr(format!("cmp {l}, {r}"));
                        f.push_instr(Instr::Set(Cond::E, Value::Register(Register::Dl)));
                        l.init(Value::from(Register::Rdx));
                    }
                    TokenKind::NotEq => {
                        f.push_raw_instr("xor rdx, rdx");
                        f.push_raw_instr(format!("cmp {l}, {r}"));
                        f.push_instr(Instr::Set(Cond::NE, Value::Register(Register::Dl)));
                        l.init(Value::from(Register::Rdx));
                    }
                    _ => todo!(),
                }

                let offset = offsets[result.0];
                f.push_raw_instr(format!("mov [rbp-{offset}], {l}"));
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
                    let mut dst = UinitValue::new(Value::from(reg));
                    generate_expr(m, f, &offsets, expr, &mut dst)?;
                }

                // x86_64 Linux ABI passes the amount of floating point args via al.
                f.push_raw_instr("mov al, 0");

                f.push_raw_instr(format!("call _{caller}"));
                if let Some(result) = result {
                    let offset = offsets[result.0];
                    f.push_raw_instr(format!("mov [rbp-{offset}], rax"));
                }
            }
            Stmt::Syscall { .. } => todo!(),
            Stmt::Block(id) => {
                f.push_block(format!("b{id}"));
            }
            Stmt::JZ(cond, id) => {
                let mut val = UinitValue::new(Value::from(Register::Rax));
                generate_expr(m, f, &offsets, cond, &mut val)?;
                f.push_raw_instr("test rax, rax");
                f.push_raw_instr(format!("jz .b{id}"));
            }
            Stmt::Jmp(id) => {
                f.push_raw_instr(format!("jmp .b{id}"));
            }
        }
    }
    Ok(())
}

fn generate_expr(
    m: &mut Module,
    f: &mut Function,
    offsets: &Vec<usize>,
    expr: Expr<'_>,
    dst: &mut UinitValue,
) -> Result<(), AppError> {
    match expr {
        Expr::IntLit(loc, lit) => {
            f.push_raw_instr(format!(";; IntLit@{loc}"));
            match dst {
                UinitValue(Some(val)) if val.is_register() => {
                    f.push_raw_instr(format!("mov {val}, {lit}"));
                }
                UinitValue(None) => dst.init(Value::from(lit)),
                _ => todo!(),
            }
        }
        Expr::StrLit(lit) => {
            f.push_raw_instr(format!(";; StrLit@{loc}", loc = lit.loc));
            let n = m.rodata.len();
            let name = format!("str{n}");
            m.push_rodata(DataDef::new(
                &name,
                DataDirective::Db,
                vec![DataExpr::Str(lit.unescape()), DataExpr::Const(0)],
            ));
            match dst {
                UinitValue(Some(val)) if val.is_register() => {
                    f.push_raw_instr(format!("mov {val}, {name}"));
                }
                UinitValue(None) => dst.init(Value::Label(name)),
                _ => todo!(),
            }
        }
        Expr::Var(token, VarId(id)) => {
            f.push_raw_instr(format!(";; Var({id})@{loc}", loc = token.loc));
            match dst {
                UinitValue(Some(val)) if val.is_register() => {
                    let offset = offsets[id];
                    f.push_raw_instr(format!("mov {val}, [rbp-{offset}]"));
                }
                _ => todo!(),
            }
        }
        Expr::Ref(token, VarId(id)) => {
            f.push_raw_instr(format!(";; Ref({id})@{loc}", loc = token.loc));
            match dst {
                UinitValue(Some(val)) if val.is_register() => {
                    let offset = offsets[id];
                    f.push_raw_instr(format!("lea {val}, [rbp-{offset}]"));
                }
                _ => todo!(),
            }
        }
        Expr::Deref(token, VarId(id)) => {
            f.push_raw_instr(format!(";; Deref({id})@{loc}", loc = token.loc));
            match dst {
                UinitValue(Some(val)) if val.is_register() => {
                    let offset = offsets[id];
                    f.push_raw_instr(format!("mov {val}, [rbp-{offset}]"));
                    f.push_raw_instr(format!("mov {val}, [{val}]"));
                }
                _ => todo!(),
            }
        }
        Expr::Global(token) => {
            f.push_raw_instr(format!(";; Global@{loc}", loc = token.loc));
            match dst {
                UinitValue(Some(val)) if val.is_register() => {
                    f.push_raw_instr(format!("mov {val}, [_{token}]"));
                }
                _ => todo!(),
            }
        }
        _ => todo!("Expression: {expr}"),
    }
    Ok(())
}

#[derive(Debug, Default)]
struct UinitValue(Option<Value>);
impl UinitValue {
    fn init(&mut self, lit: Value) {
        self.0 = Some(lit)
    }

    fn new(val: Value) -> Self {
        Self(Some(val))
    }
}

impl std::fmt::Display for UinitValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(val) = &self.0 {
            write!(f, "{val}")
        } else {
            write!(f, ";; Uinit-value")
        }
    }
}

fn calculate_stack_offsets(vars: &[Var<'_>]) -> (usize, Vec<usize>) {
    let mut offsets = vec![0; vars.len()];
    let mut offset = 8usize;
    for (i, var) in vars.iter().enumerate() {
        offsets[i] = offset;
        offset += 8;
    }
    (vars.len() * 8, offsets)
}
