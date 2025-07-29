use crate::chslexer::*;
use crate::ir::*;

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Int(i64),
    Bool(bool),
    Char(char),
    Ptr(usize),
    Str(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void => write!(f, "()"),
            Value::Int(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::Char(v) => write!(f, "{v}"),
            Value::Ptr(v) => write!(f, "{v}"),
            Value::Str(v) => write!(f, "{v}"),
        }
    }
}

struct Frame<'src> {
    func: &'src Func<'src>,
    vars: Vec<Value>,
    ip: usize,
}

pub struct Interpreter<'src> {
    program: &'src Program<'src>,
    globals: Vec<Value>,
    stack: Vec<Frame<'src>>,
    return_slot: Option<VarId>,
}

impl<'src> Interpreter<'src> {
    pub fn new(program: &'src Program<'src>) -> Self {
        let globals = vec![Value::Void; program.global_vars.len()];
        Self {
            program,
            globals,
            stack: Vec::new(),
            return_slot: None,
        }
    }
    pub fn reset(&mut self) {
        self.stack= Vec::new();
        self.return_slot= None;
    }

    pub fn run(&mut self, entry: &str) {
        let main = self
            .program
            .funcs
            .iter()
            .find(|f| f.name.source == entry)
            .expect("no main function");

        self.stack.push(Frame {
            func: main,
            vars: vec![Value::Void; main.vars.len()],
            ip: 0,
        });

        while let Some(frame) = self.stack.last_mut() {
            if frame.ip >= frame.func.body.len() {
                self.stack.pop();
                continue;
            }

            let stmt = &frame.func.body[frame.ip];
            frame.ip += 1;
            self.execute_stmt(stmt);
        }
    }
    pub fn execute_stmt(&mut self, stmt: &Stmt<'src>) {
        match stmt {
            Stmt::AssignVar { var, rhs, .. } => {
                let val = self.eval_expr(rhs);
                self.current_frame_mut().vars[var.0] = val;
            }
            Stmt::Return(_, maybe_expr) => {
                let ret_val = maybe_expr
                    .as_ref()
                    .map(|e| self.eval_expr(e))
                    .unwrap_or(Value::Void);
                self.stack.pop();
                if let Some(dest) = self.return_slot.take() {
                    self.current_frame_mut().vars[dest.0] = ret_val;
                }
            }
            Stmt::Binop {
                result,
                operator,
                lhs,
                rhs,
            } => {
                let l = self.eval_expr(lhs);
                let r = self.eval_expr(rhs);
                let out = self.eval_binop(operator, l, r);
                self.current_frame_mut().vars[result.0] = out;
            }
            Stmt::Jmp(target) => {
                self.current_frame_mut().ip = self.find_block(*target);
            }
            Stmt::JZ(cond, target) => {
                let v = self.eval_expr(cond);
                if let Value::Int(0) | Value::Bool(false) = v {
                    self.current_frame_mut().ip = self.find_block(*target);
                }
            }
            Stmt::JNZ(cond, target) => {
                let v = self.eval_expr(cond);
                match v {
                    Value::Int(n) if n != 0 => {}
                    Value::Bool(true) => {}
                    _ => return,
                }
                self.current_frame_mut().ip = self.find_block(*target);
            }
            Stmt::Block(_) => {}
            Stmt::Funcall {
                result,
                caller,
                args,
            } => {
                let func_name = caller.source;
                let arg_values: Vec<Value> = args.iter().map(|arg| self.eval_expr(arg)).collect();
                if func_name == "print" {
                    if !arg_values.is_empty() {
                        for (i, arg) in arg_values.iter().enumerate() {
                            if i >= 1 {
                                print!(" ");
                            }
                            print!("{}", arg.to_string())
                        }
                        println!();
                    }
                    return;
                }
                let callee = self
                    .program
                    .funcs
                    .iter()
                    .find(|f| f.name.source == func_name)
                    .unwrap_or_else(|| panic!("function not found: {}", func_name));

                assert_eq!(
                    callee.args.len(),
                    arg_values.len(),
                    "argument count mismatch"
                );

                let mut frame = Frame {
                    func: callee,
                    vars: vec![Value::Void; callee.vars.len()],
                    ip: 0,
                };

                for (i, val) in arg_values.into_iter().enumerate() {
                    let var_id = self.get_func_var_id(callee, i);
                    frame.vars[var_id.0] = val;
                }

                self.stack.push(frame);

                if let Some(var_id) = result {
                    self.return_slot = Some(*var_id);
                } else {
                    self.return_slot = None;
                }
            }
            _ => todo!("stmt not yet implemented: {stmt:?}"),
        }
    }
    pub fn eval_expr(&self, expr: &Expr<'src>) -> Value {
        match expr {
            Expr::IntLit(_, n) => Value::Int(*n as i64),
            Expr::StrLit(lit) => Value::Str(lit.unescape()),
            Expr::CharLit(_, c) => Value::Char(*c),
            Expr::Var(_, v) => self.current_frame().vars[v.0].clone(),
            Expr::Global(_, idx) => self.globals[*idx].clone(),
            _ => todo!("expr not yet implemented: {expr:?}"),
        }
    }
    pub fn eval_binop(&self, op: &Token<'_>, lhs: Value, rhs: Value) -> Value {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => match op.source {
                "+" => Value::Int(l + r),
                "-" => Value::Int(l - r),
                "*" => Value::Int(l * r),
                "/" => Value::Int(l / r),
                "%" => Value::Int(l % r),
                "==" => Value::Bool(l == r),
                "!=" => Value::Bool(l != r),
                "<" => Value::Bool(l < r),
                "<=" => Value::Bool(l <= r),
                ">" => Value::Bool(l > r),
                ">=" => Value::Bool(l >= r),
                _ => panic!("Unsupported int op: {}", op.source),
            },
            _ => panic!("Unsupported binop operands"),
        }
    }
    pub fn find_block(&self, block_id: usize) -> usize {
        self.current_frame()
            .func
            .body
            .iter()
            .position(|stmt| match stmt {
                Stmt::Block(id) if *id == block_id => true,
                _ => false,
            })
            .expect("block not found")
    }
    pub fn get_func_var_id(&self, func: &Func<'src>, arg_index: usize) -> VarId {
        VarId(arg_index)
    }
    fn current_frame_mut(&mut self) -> &mut Frame<'src> {
        self.stack.last_mut().unwrap()
    }

    fn current_frame(&self) -> &Frame {
        self.stack.last().unwrap()
    }
}
