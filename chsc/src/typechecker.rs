use std::collections::HashMap;

use crate::{
    ast::*,
    chslexer::{Loc, Token, TokenKind},
};

#[derive(Debug)]
pub enum TypeError<'src> {
    ReturnType(Loc<'src>),
    ReturnIncompatibleType(Loc<'src>, Type, Type),
    IncompatibleType(Loc<'src>, Type, Type),
    WrongArity(Token<'src>, usize, usize),
}

impl std::fmt::Display for TypeError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReturnType(loc) => {
                write!(f, "{loc}: Non-void function does not return a value")
            }
            Self::ReturnIncompatibleType(loc, actual, expect) => {
                write!(
                    f,
                    "{loc}: incompatible types when returning type ‘{actual}’ but ‘{expect}’ was expected"
                )
            }
            Self::IncompatibleType(loc, actual, expect) => {
                write!(
                    f,
                    "{loc}: incompatible types, got type ‘{actual}’ but ‘{expect}’ was expected"
                )
            }
            Self::WrongArity(token, actual, expect) => {
                write!(
                    f,
                    "{loc}: Wrong number of arguments expected {expect} but got {actual} for {name}",
                    loc = token.loc,
                    name = token.source
                )
            }
        }
    }
}

impl std::error::Error for TypeError<'_> {}

#[derive(Debug)]
enum TypeCons {
    Var(Type),
    Func(usize),
    ExternFunc(usize),
}

#[derive(Debug, Default)]
struct TypeEnv<'src> {
    pub globals: HashMap<&'src str, TypeCons>,
    pub locals: Vec<HashMap<VarId, TypeCons>>,
}
impl<'src> TypeEnv<'src> {
    pub fn push_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        assert!(self.locals.len() != 1);
        self.locals.pop();
    }

    pub fn insert_global(&mut self, k: &'src str, v: TypeCons) -> Option<TypeCons> {
        self.globals.insert(k, v)
    }

    pub fn get_global(&self, k: &'src str) -> Option<&TypeCons> {
        self.globals.get(k)
    }

    pub fn insert_local(&mut self, k: VarId, v: TypeCons) -> Option<TypeCons> {
        self.locals.last_mut().and_then(|scope| scope.insert(k, v))
    }

    pub fn get_local(&self, k: &VarId) -> Option<&TypeCons> {
        for scope in self.locals.iter().rev() {
            let v = scope.get(k);
            if v.is_some() {
                return v;
            }
        }
        None
    }
}

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
}

pub fn type_check_program<'src>(p: &mut Program<'src>) -> Result<(), TypeError<'src>> {
    let mut env = TypeEnv::default();
    env.push_scope();
    for (i, ele) in p.externs.iter().enumerate() {
        match ele {
            Extern::Func { name, .. } => {
                env.insert_global(name.source, TypeCons::ExternFunc(i));
            }
            Extern::Var(..) => todo!(),
        }
    }
    let mut ret_type = Type::Void;
    let mut ret = ReturnFlow::Never;
    for i in 0..p.funcs.len() {
        let func = unsafe { p.funcs.as_ptr().add(i).cast_mut().as_mut().unwrap() };
        env.insert_global(func.name.source, TypeCons::Func(i));
        env.push_scope();
        for (i, ty) in func.args_types.iter().enumerate() {
            env.insert_local(VarId(i), TypeCons::Var(ty.clone()));
        }
        if func.ret_type.is_none() {
            ret = ReturnFlow::Always;
        }
        ret_type = func.ret_type.clone().unwrap_or(Type::Void);
        for stmt in func.body.iter() {
            let flow = match stmt {
                Stmt::Return(loc, expr) => {
                    if let Some(expr) = expr {
                        let ty = type_of_expr(&env, expr)?;
                        if ret_type != ty {
                            return Err(TypeError::ReturnIncompatibleType(*loc, ty, ret_type));
                        }
                    } else {
                        if ret_type != Type::Void {
                            return Err(TypeError::ReturnIncompatibleType(
                                *loc,
                                Type::Void,
                                ret_type,
                            ));
                        }
                    }
                    ReturnFlow::Always
                }
                Stmt::Store {
                    target: (token, var_id),
                    rhs,
                } => {
                    if let Some(vty) = func.vars[var_id.0].ty.clone() {
                        match vty {
                            Type::Ptr => {}
                            Type::PtrTo(ref inner) => {
                                let ty = type_of_expr(&env, rhs)?;
                                if inner.as_ref() != &ty {
                                    return Err(TypeError::IncompatibleType(token.loc, ty, vty));
                                }
                            }
                            _ => {
                                return Err(TypeError::IncompatibleType(token.loc, Type::Ptr, vty));
                            }
                        }
                    } else {
                        todo!()
                    };
                    ReturnFlow::Always
                }
                Stmt::AssignVar {
                    var: (token, var_id),
                    rhs,
                } => {
                    let ty = type_of_expr(&env, rhs)?;
                    if let Some(vty) = func.vars[var_id.0].ty.clone() {
                        if vty != ty {
                            return Err(TypeError::IncompatibleType(token.loc, ty, vty));
                        }
                    } else {
                        todo!()
                    };
                    env.insert_local(*var_id, TypeCons::Var(ty));
                    ReturnFlow::Never
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
                } => {
                    let lhs = type_of_expr(&env, lhs)?;
                    let rhs = type_of_expr(&env, rhs)?;
                    let ty = binop_typecheck(lhs, operator, rhs)?;
                    env.insert_local(*result, TypeCons::Var(ty.clone()));
                    func.vars[result.0].ty = Some(ty);
                    ReturnFlow::Never
                }
                Stmt::Funcall {
                    result,
                    caller: caller_token,
                    args: call_args,
                } => {
                    let caller = env.get_global(caller_token.source);
                    if let Some(caller) = caller {
                        let (name, args, is_variadic, ret) = match caller {
                            TypeCons::ExternFunc(id) => p.externs[*id].as_func_(),
                            TypeCons::Func(id) => unsafe {
                                let f = (&p.funcs[*id]);
                                (&f.name, &f.args_types, false, &f.ret_type)
                            },
                            _ => todo!(),
                        };
                        let arity = if is_variadic {
                            call_args.len() >= args.len()
                        } else {
                            call_args.len() == args.len()
                        };
                        if !arity {
                            return Err(TypeError::WrongArity(
                                *caller_token,
                                args.len(),
                                call_args.len(),
                            ));
                        }
                        for (actual, expect_ty) in call_args.iter().zip(args) {
                            let actual_ty = type_of_expr(&env, actual)?;
                            if actual_ty != *expect_ty {
                                return Err(TypeError::IncompatibleType(
                                    actual.loc(),
                                    actual_ty,
                                    expect_ty.clone(),
                                ));
                            }
                        }
                        if let Some(res) = result {
                            env.insert_local(
                                *res,
                                TypeCons::Var(ret.clone().unwrap_or(Type::Void)),
                            );
                            func.vars[res.0].ty = ret.clone();
                        }
                    }
                    ReturnFlow::Never
                }
                Stmt::Syscall { result, args } => todo!(),
                Stmt::JZ(cond, ..) => {
                    let cond_ty = type_of_expr(&env, cond)?;
                    if cond_ty != Type::Bool {
                        return Err(TypeError::IncompatibleType(cond.loc(), cond_ty, Type::Bool));
                    }
                    ReturnFlow::Never
                }
                Stmt::Jmp(_) => ReturnFlow::Never,
                Stmt::Block(_) => ReturnFlow::Never,
            };
            ret = ret.combine(flow)
        }
        if ret != ReturnFlow::Always {
            return Err(TypeError::ReturnType(func.name.loc));
        }
        env.pop_scope();
    }
    Ok(())
}

fn binop_typecheck<'src>(lhs: Type, op: &Token<'src>, rhs: Type) -> Result<Type, TypeError<'src>> {
    use TokenKind::*;
    match op.kind {
        Eq | NotEq | Gt | GtEq | Lt | LtEq => {
            if lhs == rhs {
                return Ok(Type::Bool);
            }
        }
        op => match (&lhs, op, &rhs) {
            (Type::Int, Plus | Minus | Asterisk | Slash | Percent, Type::Int) => {
                return Ok(Type::Int);
            }
            (Type::Ptr | Type::PtrTo(..), Plus, Type::Int) => {
                return Ok(lhs);
            }
            (Type::Int, Plus, Type::Ptr | Type::PtrTo(..)) => {
                return Ok(rhs);
            }
            _ => {}
        },
        _ => {}
    }
    return Err(TypeError::IncompatibleType(op.loc, lhs, rhs));
}

fn type_of_expr<'src>(env: &TypeEnv<'src>, expr: &Expr<'src>) -> Result<Type, TypeError<'src>> {
    match expr {
        Expr::IntLit(..) => Ok(Type::Int),
        Expr::StrLit(..) => Ok(Type::PtrTo(Box::new(Type::Char))),
        Expr::Var(token, id) => {
            if let Some(v) = env.get_local(id) {
                match v {
                    TypeCons::Var(ty) => return Ok(ty.clone()),
                    TypeCons::Func(_) => todo!(),
                    TypeCons::ExternFunc(_) => todo!(),
                }
            } else {
                todo!("Undefined {token}")
            }
        }
        Expr::Global(token) => todo!(),
        Expr::Deref(token, id) => {
            if let Some(v) = env.get_local(id) {
                match v {
                    TypeCons::Var(ty) => return Ok(ty.deref_as_inner_type()),
                    TypeCons::Func(_) => todo!(),
                    TypeCons::ExternFunc(_) => todo!(),
                }
            } else {
                todo!("Undefined {token}")
            }
        }
        Expr::Ref(token, id) => {
            if let Some(v) = env.get_local(id) {
                match v {
                    TypeCons::Var(ty) => return Ok(Type::PtrTo(Box::new(ty.clone()))),
                    TypeCons::Func(_) => todo!(),
                    TypeCons::ExternFunc(_) => todo!(),
                }
            } else {
                todo!("Undefined {token}")
            }
        }
        Expr::Cast(_, ty, _) => Ok(ty.clone()),
    }
}
