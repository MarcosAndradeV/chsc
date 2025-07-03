use std::collections::HashMap;

use crate::{
    ast::{type_of_expr, Expr, Program, Stmt, Type, VarId},
    chslexer::Token,
    utils::AppError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnFlow {
    Always,
    Never,
    Sometimes,
}

impl ReturnFlow {
    pub fn combine(self, other: ReturnFlow) -> ReturnFlow {
        match (self, other) {
            (ReturnFlow::Always, _) | (_, ReturnFlow::Always) => ReturnFlow::Always,
            (ReturnFlow::Never, _) | (_, ReturnFlow::Never) => ReturnFlow::Never,
            _ => ReturnFlow::Sometimes,
        }
    }

    pub fn combine_mut(&mut self, other: ReturnFlow) {
        *self = match (&self, other) {
            (ReturnFlow::Always, _) | (_, ReturnFlow::Always) => ReturnFlow::Always,
            (ReturnFlow::Never, _) | (_, ReturnFlow::Never) => ReturnFlow::Never,
            _ => ReturnFlow::Sometimes,
        };
    }
}

enum TypeOfDef<'src> {
    ExternFunc {
        args: &'src [Type],
        ret: &'src Type,
        is_variadic: bool,
    },
    Func {
        args: &'src [Type],
        ret: &'src Type,
    },
}

pub fn check_program(p: &Program) -> Result<(), AppError> {
    let mut types_of_defs = HashMap::new();
    for e in &p.externs {
        types_of_defs.insert(
            e.name.source,
            TypeOfDef::ExternFunc {
                args: &e.args,
                ret: &e.ret,
                is_variadic: e.is_variadic,
            },
        );
    }
    for f in &p.funcs {
        types_of_defs.insert(
            f.name.source,
            TypeOfDef::Func {
                args: &f.args_types,
                ret: &f.ret_type,
            },
        );
    }
    let mut return_flow = ReturnFlow::Never;
    for func in &p.funcs {
        if func.ret_type == Type::Void {
            return_flow.combine_mut(ReturnFlow::Always);
        }
        for stmt in func.body.iter() {
            match stmt {
                Stmt::AssignVar {
                    var: (token, VarId(id)),
                    rhs,
                } => {
                    let ty = &func.vars[*id].ty;
                    let rhs_ty = type_of_expr(rhs, &func.vars)?;
                    if *ty != rhs_ty {
                        Err(AppError::TypeError(format!(
                            "{loc}: Cannot assign a value of type `{rhs_ty}` to a variable of type `{ty}`.",
                            loc = token.loc
                        )))?;
                    }
                }
                Stmt::Return(loc, None) => {
                    if func.ret_type != Type::Void {
                        Err(AppError::TypeError(format!(
                            "{loc}: Function `{name}` must return `{ret}`.",
                            loc = loc,
                            name = func.name,
                            ret = func.ret_type,
                        )))?;
                    }
                }
                Stmt::Return(loc, Some(expr)) => {
                    let ty = type_of_expr(expr, &func.vars)?;
                    if ty != func.ret_type {
                        Err(AppError::TypeError(format!(
                            "{loc}: Function `{name}` cannot return `{ty}`. It must return `{ret}`.",
                            loc = loc,
                            name = func.name,
                            ret = func.ret_type,
                        )))?;
                    }
                }
                Stmt::Store { target, rhs } => {
                    assert!(matches!(target, Expr::Deref(..)));
                    let rhs_ty = type_of_expr(rhs, &func.vars)?;
                    let ty = type_of_expr(target, &func.vars)?;
                    if ty != rhs_ty {
                        Err(AppError::TypeError(format!(
                            "{loc}: Cannot store value of type `{rhs_ty}` in a pointer to `{ty}`.",
                            loc = target.loc()
                        )))?;
                    }
                }
                Stmt::Unop {
                    result,
                    operator,
                    operand,
                } => todo!(),
                Stmt::Binop {
                    result,
                    operator,
                    lhs,
                    rhs,
                } => {}
                Stmt::Funcall {
                    result,
                    caller,
                    args,
                } => {
                    let (ty_args, ret, is_variadic) = match types_of_defs.get(caller.source) {
                        Some(TypeOfDef::ExternFunc {
                            args,
                            ret,
                            is_variadic,
                        }) => (args, ret, *is_variadic),
                        Some(TypeOfDef::Func { args, ret }) => (args, ret, false),
                        None => unreachable!("Parser should see that first"),
                    };
                    match (args.len().cmp(&ty_args.len()), is_variadic) {
                        (std::cmp::Ordering::Less, _) | (std::cmp::Ordering::Greater, false) => {
                            Err(AppError::TypeError(format!(
                                "{loc}: Not enough arguments for {caller} call {arity} is expected.",
                                loc = caller.loc,
                                arity = ty_args.len()
                            )))?;
                        }
                        (std::cmp::Ordering::Equal, _) | (std::cmp::Ordering::Greater, true) => (),
                    }
                    for (i, (arg, expect_ty)) in args.iter().zip(ty_args.iter()).enumerate() {
                        let actual_ty = type_of_expr(arg, &func.vars)?;
                        if actual_ty != *expect_ty {
                            Err(AppError::TypeError(format!(
                                "{loc}: The argument {i} of call has type `{actual_ty}`, but `{expect_ty}` is expected.",
                                loc = caller.loc
                            )))?;
                        }
                    }
                }
                Stmt::Syscall { result, args } => todo!(),
                Stmt::JZ(cond, block_id) => {
                    let cond_ty = type_of_expr(cond, &func.vars)?;
                    if cond_ty != Type::Bool {
                        Err(AppError::TypeError(format!(
                            "{loc}: Condition expected to be boolean, but is `{cond_ty}`",
                            loc = cond.loc()
                        )))?;
                    }
                }
                Stmt::Jmp(block_id) => (),
                Stmt::Block(block_id) => (),
            }
        }
    }
    Ok(())
}
