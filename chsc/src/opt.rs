use crate::ast::*;
use crate::chslexer::*;
use crate::fasm_backend::Function;
use crate::fasm_backend::Value;

pub fn const_fold(program_ast: &mut Program<'_>) {
    fn try_to_comptime(comptime_vars: &[Option<u64>], expr: &Expr<'_>) -> Option<u64> {
        match expr {
            Expr::IntLit(_, lit) => Some(*lit),
            Expr::Var(_, VarId(id, false)) => comptime_vars[*id],
            _ => None,
        }
    }

    for func in program_ast.funcs.iter_mut() {
        let mut comptime_vars: Vec<Option<u64>> = vec![None; func.vars.len()];
        for stmt in func.body.iter_mut() {
            match stmt {
                Stmt::Block(_) | Stmt::Jmp(_) | Stmt::JZ(..) => {
                    comptime_vars = vec![None; func.vars.len()];
                }
                Stmt::Binop {
                    result,
                    operator,
                    lhs,
                    rhs,
                } => {
                    let lhs = try_to_comptime(&comptime_vars, lhs);
                    let rhs = try_to_comptime(&comptime_vars, rhs);
                    match (lhs, rhs) {
                        (Some(lhs), Some(rhs)) => {
                            let val = match operator.kind {
                                TokenKind::Plus => lhs + rhs,
                                TokenKind::Minus => lhs - rhs,
                                TokenKind::Asterisk => lhs * rhs,
                                _ => continue,
                            };
                            comptime_vars[result.0] = Some(val);
                            *stmt = Stmt::Assign {
                                lhs: Expr::Var(*operator, *result),
                                rhs: Expr::IntLit(operator.loc, val),
                            };
                        }
                        _ => (),
                    }
                }
                Stmt::Assign {
                    lhs: Expr::Var(token, VarId(id, false)),
                    rhs,
                } => {
                    try_to_comptime(&comptime_vars, rhs);
                    if let Some(val) = try_to_comptime(&comptime_vars, rhs) {
                        comptime_vars[*id] = Some(val);
                        *rhs = Expr::IntLit(token.loc, val);
                    }
                }
                Stmt::Funcall {
                    result: _,
                    caller: _,
                    args,
                } => {
                    for arg in args {
                        match arg {
                            Expr::Var(token, VarId(id, false)) => {
                                let loc = token.loc;
                                if let Some(lit) = try_to_comptime(&comptime_vars, arg) {
                                    *arg = Expr::IntLit(loc, lit);
                                }
                            }
                            _ => (),
                        }
                    }
                }
                _ => (),
            }
        }
    }
}

pub fn find_used_vars(af: &Func<'_>) -> Vec<bool> {
    let mut used = vec![false; af.vars.len()];
    fn mark_used(used: &mut Vec<bool>, expr: &Expr<'_>) {
        if let Expr::Var(_, VarId(id, false)) = expr {
            used[*id] = true;
        }
    }
    for stmt in &af.body {
        match stmt {
            Stmt::Return(Some(expr)) => {
                mark_used(&mut used, expr);
            }
            Stmt::Unop { operand, .. } => {
                mark_used(&mut used, operand);
            }
            Stmt::Binop { lhs, rhs, .. } => {
                mark_used(&mut used, lhs);
                mark_used(&mut used, rhs);
            }
            Stmt::JZ(expr, ..) => {
                mark_used(&mut used, expr);
            }
            Stmt::Assign { rhs, .. } => {
                mark_used(&mut used, rhs);
            }
            Stmt::Funcall { args, .. } | Stmt::Syscall { args, .. } => {
                for arg in args {
                    mark_used(&mut used, arg);
                }
            }
            _ => (),
        }
    }

    return used;
}
