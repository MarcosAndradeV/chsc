use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::ast::*;
use crate::chslexer::*;

pub fn const_fold(program_ast: &mut Program<'_>) {
    fn try_into_const(comptime_vars: &mut [Option<u64>], expr: &Expr<'_>) -> Option<u64> {
        match expr {
            Expr::IntLit(_, lit) => Some(*lit),
            Expr::Var(_, VarId(id)) => comptime_vars[*id],
            Expr::Ref(_, VarId(id)) | Expr::Deref(_, VarId(id)) => {
                comptime_vars[*id] = None;
                None
            }
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
                    let lhs = try_into_const(&mut comptime_vars, lhs);
                    let rhs = try_into_const(&mut comptime_vars, rhs);
                    match (lhs, rhs) {
                        (Some(lhs), Some(rhs)) => {
                            let val = match operator.kind {
                                TokenKind::Plus => lhs + rhs,
                                TokenKind::Minus => lhs - rhs,
                                TokenKind::Asterisk => lhs * rhs,
                                _ => continue,
                            };
                            comptime_vars[result.0] = Some(val);
                            *stmt = Stmt::AssignVar {
                                var: (*operator, *result),
                                rhs: Expr::IntLit(operator.loc, val),
                            };
                        }
                        _ => (),
                    }
                }
                Stmt::Return(_, Some(expr)) => {
                    if let Some(val) = try_into_const(&mut comptime_vars, expr) {
                        *expr = Expr::IntLit(expr.loc(), val);
                    }
                }
                _ => (),
            }
        }
    }
}

pub fn mark_used_variables(af: &mut Func<'_>) {
    fn mark_used(vars: &mut Vec<Var<'_>>, expr: &mut Expr<'_>) {
        match expr {
            // Expr::Var(_, var_id) => {
            //     vars[var_id.0].used = true;
            // }
            // Expr::Ref(_, var_id) => {
            //     vars[var_id.0].used = true;
            // }
            // Expr::Deref(_, var_id) => {
            //     vars[var_id.0].used = true;
            // }
            _ => (),
        }
    }

    for stmt in &mut af.body {
        match stmt {
            Stmt::Return(_, Some(expr)) => {
                mark_used(&mut af.vars, expr);
            }
            Stmt::Unop { operand, .. } => {
                mark_used(&mut af.vars, operand);
            }
            Stmt::Binop { lhs, rhs, .. } => {
                mark_used(&mut af.vars, lhs);
                mark_used(&mut af.vars, rhs);
            }
            Stmt::JZ(expr, ..) => {
                mark_used(&mut af.vars, expr);
            }
            Stmt::AssignVar { var: _, rhs } => {
                mark_used(&mut af.vars, rhs);
            }
            Stmt::Funcall { args, .. } | Stmt::Syscall { args, .. } => {
                for arg in args {
                    mark_used(&mut af.vars, arg);
                }
            }
            _ => (),
        }
    }
}

pub fn map_vars_to_offsets(vars: &[Var<'_>]) -> Vec<usize> {
    let mut offsets = vec![0; vars.len()];
    let mut offset = 8usize;
    for (i, var) in vars.iter().enumerate() {
        // if var.used {
            offsets[i] = offset;
            offset += 8;
        // }
    }
    offsets
}

pub fn get_used_vars_len(vars: &[Var<'_>]) -> usize {
    vars.len()
    // .iter().fold(0, |acc, elem| if elem.used { acc + 1 } else { acc })
}
