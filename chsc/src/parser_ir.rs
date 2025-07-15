use crate::{ir::*, chslexer::*, utils::AppError};

pub fn parse_program<'src>(
    file_path: &'src str,
    source: &'src str,
) -> Result<Program<'src>, AppError> {
    let mut lexer = PeekableLexer::new(file_path, source);
    lexer.set_is_keyword_fn(|s| {
        matches!(
            s,
            "extern" | "fn" | "return" | "var" | "if" | "else" | "while" | "syscall" | "cast"
        )
    });
    let mut p = Program::default();
    let mut names_index = NameSpace::default();
    names_index.push_scope();

    loop {
        let token = lexer.next_token();
        if token.is_eof() {
            break;
        }
        match token.kind {
            TokenKind::Keyword if token.source == "fn" => {
                let uid = p.funcs.len();
                let r#fn = parse_fn(&mut lexer, &mut names_index, &p)?;
                p.funcs.push(r#fn);
            }
            TokenKind::Keyword if token.source == "extern" => {
                let r#extern = parse_extern(&mut lexer, &mut names_index, &p)?;
                let uid = p.externs.len();
                names_index.insert_var_index(r#extern.name.source, Names::ExternFunc(uid));
                p.externs.push(r#extern);
            }
            TokenKind::Keyword if token.source == "var" => {
                let name = expect(
                    &mut lexer,
                    TokenKind::Identifier,
                    Some("Variable must have a name"),
                )?;
                let uid = p.global_vars.len();
                let is_vec = if inspect(&mut lexer, &[TokenKind::OpenBracket])? {
                    lexer.next_token();
                    let size = expect(
                        &mut lexer,
                        TokenKind::IntegerNumber,
                        Some("Expected a number"),
                    )?;
                    expect(
                        &mut lexer,
                        TokenKind::CloseBracket,
                        Some("Array types are like this [SIZE]TYPE"),
                    )?;
                    Some(size.source.parse().unwrap())
                } else {
                    None
                };
                let ty = {
                    let ty = parse_type(&mut lexer)?;
                    if is_vec.is_some() {
                        Type::PtrTo(Box::new(ty))
                    } else {
                        ty
                    }
                };
                expect(&mut lexer, TokenKind::SemiColon, None::<&str>)?;
                names_index.insert_var_index(name.source, Names::GlobalVar(uid));
                p.global_vars.push(GlobalVar {
                    token: name,
                    ty,
                    is_vec,
                });
            }
            _ => unexpected_token(token, Some("Expected a top level definition"))?,
        }
    }

    return Ok(p);
}

fn parse_extern<'src>(
    lexer: &mut PeekableLexer<'src>,
    vars_index: &mut NameSpace<'src>,
    p: &Program<'src>,
) -> Result<ExternFunc<'src>, AppError> {
    let token = lexer.next_token();

    match token.kind {
        TokenKind::Keyword if token.source == "fn" => {
            let name = expect(lexer, TokenKind::Identifier, None::<&str>)?;
            let mut args = vec![];
            let mut is_variadic = false;
            let mut ret = Type::Void;

            expect(lexer, TokenKind::OpenParen, None::<&str>)?;

            loop {
                let ptoken = lexer.peek_token();
                match ptoken.kind {
                    TokenKind::CloseParen => {
                        lexer.next_token();
                        if inspect(lexer, &[TokenKind::Arrow])? {
                            lexer.next_token();
                            ret = parse_type(lexer)?;
                        }
                        break;
                    }
                    TokenKind::Comma => {
                        lexer.next_token();
                    }
                    _ => {}
                }
                if inspect(lexer, &[TokenKind::Splat])? {
                    lexer.next_token();
                    is_variadic = true;
                    expect(lexer, TokenKind::CloseParen, None::<&str>)?;
                    if inspect(lexer, &[TokenKind::Arrow])? {
                        lexer.next_token();
                        ret = parse_type(lexer)?;
                    }
                    break;
                }
                let ty = parse_type(lexer)?;
                args.push(ty);
            }

            expect(lexer, TokenKind::SemiColon, None::<&str>)?;

            return Ok(ExternFunc {
                name,
                args,
                is_variadic,
                ret,
            });
        }
        _ => unexpected_token(token, Some("Expected extern declaration")),
    }
}

fn parse_fn<'src>(
    lexer: &mut PeekableLexer<'src>,
    names_index: &mut NameSpace<'src>,
    p: &Program<'src>,
) -> Result<Func<'src>, AppError> {
    let name = expect(lexer, TokenKind::Identifier, None::<&str>)?;
    names_index.insert_var_index(name.source, Names::Func(p.funcs.len()));
    names_index.push_scope();
    let mut func = Func {
        name,
        ..Default::default()
    };

    expect(lexer, TokenKind::OpenParen, None::<&str>)?;
    loop {
        let ptoken = lexer.peek_token();
        match ptoken.kind {
            TokenKind::CloseParen => {
                lexer.next_token();
                if inspect(lexer, &[TokenKind::Arrow])? {
                    lexer.next_token();
                    func.ret_type = parse_type(lexer)?;
                }
                break;
            }
            TokenKind::Comma => {
                lexer.next_token();
            }
            TokenKind::Identifier => {
                let token = lexer.next_token();

                let id = VarId(func.vars.len());
                names_index.insert_var_index(token.source, Names::Var(id));
                let mut is_vec = if inspect(lexer, &[TokenKind::OpenBracket])? {
                    lexer.next_token();
                    let size = expect(lexer, TokenKind::IntegerNumber, Some("Expected a number"))?;
                    expect(
                        lexer,
                        TokenKind::CloseBracket,
                        Some("Array types are like this [SIZE]TYPE"),
                    )?;
                    Some(size.source.parse().unwrap())
                } else {
                    None
                };
                let ty = {
                    let ty = parse_type(lexer)?;
                    if is_vec.is_some() {
                        is_vec = None;
                        Type::PtrTo(Box::new(ty))
                    } else {
                        ty
                    }
                };
                func.vars.push(Var::new(token.loc, ty.clone(), is_vec));
                func.args.push(token);
                func.args_types.push(ty);
            }
            _ => {}
        }
    }

    expect(lexer, TokenKind::OpenBrace, None::<&str>)?;

    loop {
        let ptoken = lexer.peek_token();
        if ptoken.is_eof() {
            break;
        }
        match ptoken.kind {
            TokenKind::CloseBrace => {
                lexer.next_token();
                break;
            }
            _ => {
                parse_stmt(lexer, &mut func, names_index, p)?;
            }
        }
    }
    names_index.pop_scope();

    Ok(func)
}

fn parse_type<'src>(lexer: &mut PeekableLexer<'src>) -> Result<Type, AppError> {
    let token = lexer.next_token();
    match token.kind {
        TokenKind::Identifier => match token.source {
            "int" => Ok(Type::Int),
            "ptr" => Ok(Type::Ptr),
            "bool" => Ok(Type::Bool),
            "char" => Ok(Type::Char),
            _ => unexpected_token(token, Some("Expected type")),
        },
        TokenKind::Asterisk => {
            let ty = parse_type(lexer)?;
            Ok(Type::PtrTo(Box::new(ty)))
        }
        _ => unexpected_token(token, Some("Expected type")),
    }
}

fn parse_stmt<'src>(
    lexer: &mut PeekableLexer<'src>,
    curr_fn: &mut Func<'src>,
    names_index: &mut NameSpace<'src>,
    p: &Program<'src>,
) -> Result<(), AppError> {
    let token = lexer.peek_token();
    match token.kind {
        TokenKind::Keyword if token.source == "var" => {
            lexer.next_token();
            let token = expect(lexer, TokenKind::Identifier, None::<&str>)?;
            let is_vec = if inspect(lexer, &[TokenKind::OpenBracket])? {
                lexer.next_token();
                let size = expect(lexer, TokenKind::IntegerNumber, Some("Expected a number"))?;
                expect(
                    lexer,
                    TokenKind::CloseBracket,
                    Some("Array types are like this [SIZE]TYPE"),
                )?;
                Some( size.source.parse().unwrap())
            } else {
                None
            };
            let ty = {
                let ty = parse_type(lexer)?;
                if is_vec.is_some() {
                    Type::PtrTo(Box::new(ty))
                } else {
                    ty
                }
            };
            let id = if inspect(lexer, &[TokenKind::Assign])? {
                lexer.next_token();
                let rhs = parse_expr(
                    lexer,
                    curr_fn,
                    names_index,
                    Precedence::Lowest,
                    &[TokenKind::SemiColon],
                    p,
                )?;
                let var_id = VarId(curr_fn.vars.len());
                curr_fn.body.push(Stmt::AssignVar {
                    loc: token.loc,
                    var:  var_id,
                    rhs,
                });
                var_id
            } else {
                VarId(curr_fn.vars.len())
            };

            names_index.insert_var_index(token.source, Names::Var(id));
            curr_fn.vars.push(Var::new(token.loc, ty, is_vec));

            expect(lexer, TokenKind::SemiColon, None::<&str>)?;

            Ok(())
        }
        TokenKind::Keyword if token.source == "return" => {
            let token = lexer.next_token();
            let expr = if inspect(lexer, &[TokenKind::SemiColon])? {
                None
            } else {
                let expr = parse_expr(
                    lexer,
                    curr_fn,
                    names_index,
                    Precedence::Lowest,
                    &[TokenKind::SemiColon],
                    p,
                )?;
                let ty = type_of_expr(&expr, &p.global_vars, &curr_fn.vars)?;
                Some(expr)
            };
            expect(
                lexer,
                TokenKind::SemiColon,
                Some("Expected `;` after return"),
            )?;
            curr_fn.body.push(Stmt::Return(token.loc, expr));
            return Ok(());
        }
        TokenKind::Keyword if token.source == "if" => {
            lexer.next_token();

            expect(lexer, TokenKind::OpenParen, None::<&str>)?;
            let cond = parse_expr(
                lexer,
                curr_fn,
                names_index,
                Precedence::Lowest,
                &[TokenKind::CloseParen],
                p,
            )?;
            expect(lexer, TokenKind::CloseParen, None::<&str>)?;
            let after_block = next_block(curr_fn);

            curr_fn.body.push(Stmt::JZ(cond, after_block));

            names_index.push_scope();
            parse_stmt(lexer, curr_fn, names_index, p)?;
            names_index.pop_scope();

            if lexer.peek_token().source == "else" {
                let after_else_block = next_block(curr_fn);
                curr_fn.body.push(Stmt::Jmp(after_else_block));

                curr_fn.body.push(Stmt::Block(after_block));
                lexer.next_token();

                names_index.push_scope();
                parse_stmt(lexer, curr_fn, names_index, p)?;
                names_index.pop_scope();

                curr_fn.body.push(Stmt::Block(after_else_block));
                return Ok(());
            }

            curr_fn.body.push(Stmt::Block(after_block));
            Ok(())
        }
        TokenKind::Keyword if token.source == "while" => {
            lexer.next_token();

            expect(lexer, TokenKind::OpenParen, None::<&str>)?;
            let cond_block = next_block(curr_fn);
            let after_block = next_block(curr_fn);
            curr_fn.body.push(Stmt::Block(cond_block));
            let cond = parse_expr(
                lexer,
                curr_fn,
                names_index,
                Precedence::Lowest,
                &[TokenKind::CloseParen],
                p,
            )?;
            expect(lexer, TokenKind::CloseParen, None::<&str>)?;

            curr_fn.body.push(Stmt::JZ(cond, after_block));

            names_index.push_scope();
            parse_stmt(lexer, curr_fn, names_index, p)?;
            names_index.pop_scope();

            curr_fn.body.push(Stmt::Jmp(cond_block));

            curr_fn.body.push(Stmt::Block(after_block));
            Ok(())
        }
        TokenKind::OpenBrace => {
            expect(lexer, TokenKind::OpenBrace, None::<&str>)?;
            names_index.push_scope();
            loop {
                let ptoken = lexer.peek_token();
                if ptoken.is_eof() {
                    break;
                }
                match ptoken.kind {
                    TokenKind::CloseBrace => {
                        lexer.next_token();
                        break;
                    }
                    _ => {
                        parse_stmt(lexer, curr_fn, names_index, p)?;
                    }
                }
            }
            names_index.pop_scope();
            Ok(())
        }
        _ => {
            let expr = parse_expr(
                lexer,
                curr_fn,
                names_index,
                Precedence::Lowest,
                &[
                    TokenKind::Assign,
                    TokenKind::SemiColon,
                    TokenKind::OpenParen,
                ],
                p,
            )?;

            let ptoken = lexer.peek_token();
            match (expr, ptoken.kind) {
                (Expr::Var(loc, id), TokenKind::Assign) => {
                    lexer.next_token();
                    let rhs = parse_expr(
                        lexer,
                        curr_fn,
                        names_index,
                        Precedence::Lowest,
                        &[TokenKind::SemiColon],
                        p,
                    )?;
                    expect(
                        lexer,
                        TokenKind::SemiColon,
                        Some("Expected `;` after expression."),
                    )?;
                    curr_fn.body.push(Stmt::AssignVar {
                        loc: loc,
                        var:  id,
                        rhs,
                    });
                }
                (Expr::Global(token, uid), TokenKind::Assign) => {
                    lexer.next_token();
                    let rhs = parse_expr(
                        lexer,
                        curr_fn,
                        names_index,
                        Precedence::Lowest,
                        &[TokenKind::SemiColon],
                        p,
                    )?;
                    expect(
                        lexer,
                        TokenKind::SemiColon,
                        Some("Expected `;` after expression."),
                    )?;
                    curr_fn.body.push(Stmt::AssignGlobalVar {
                        var: (token, uid),
                        rhs,
                    });
                }
                (
                    expr @ Expr::Deref(..)
                    | expr @ Expr::GlobalDeref(..),
                    TokenKind::Assign,
                ) => {
                    lexer.next_token();
                    let rhs = parse_expr(
                        lexer,
                        curr_fn,
                        names_index,
                        Precedence::Lowest,
                        &[TokenKind::SemiColon],
                        p,
                    )?;
                    expect(
                        lexer,
                        TokenKind::SemiColon,
                        Some("Expected `;` after expression."),
                    )?;
                    curr_fn.body.push(Stmt::Store { target: expr, rhs });
                }
                (Expr::Global(token, _), TokenKind::OpenParen) => {
                    lexer.next_token();
                    let mut args = collect_args(lexer, curr_fn, names_index, p)?;
                    expect(
                        lexer,
                        TokenKind::SemiColon,
                        Some("Expected `;` after expression."),
                    )?;
                    curr_fn.body.push(Stmt::Funcall {
                        result: None,
                        caller: token,
                        args,
                    });
                }
                (_, TokenKind::SemiColon) => {
                    lexer.next_token();
                }
                (expr, _) => todo!("{expr}, {ptoken}"),
            }

            Ok(())
        }
    }
}

fn parse_expr<'src>(
    lexer: &mut PeekableLexer<'src>,
    curr_fn: &mut Func<'src>,
    names_index: &NameSpace,
    precedence: Precedence,
    stop: &[TokenKind],

    p: &Program<'src>,
) -> Result<Expr<'src>, AppError> {
    let mut curr_precedence = precedence;
    let left_token = lexer.next_token();
    let mut left = match left_token.kind {
        TokenKind::IntegerNumber => {
            curr_precedence = Precedence::Highest;
            Expr::IntLit(left_token.loc, left_token.source.parse().unwrap())
        }
        TokenKind::StringLiteral => {
            curr_precedence = Precedence::Highest;
            Expr::StrLit(left_token)
        }
        TokenKind::OpenParen => {
            curr_precedence = Precedence::Highest;
            let expr = parse_expr(
                lexer,
                curr_fn,
                names_index,
                precedence,
                &[TokenKind::CloseParen],
                p,
            )?;
            expect(lexer, TokenKind::CloseParen, Some("Expected `)`."))?;
            expr
        }
        TokenKind::Bang => {
            curr_precedence = Precedence::NegNot;
            let expr = parse_expr(lexer, curr_fn, names_index, precedence, stop, p)?;
            let ty = type_of_expr(&expr, &p.global_vars, &curr_fn.vars)?;
            let id = {
                let len = curr_fn.vars.len();
                curr_fn.vars.push(Var::new(left_token.loc, ty, None));
                VarId(len)
            };

            curr_fn.body.push(Stmt::Unop {
                result: id,
                operator: left_token,
                operand: expr,
            });
            Expr::Var(left_token.loc, id)
        }
        TokenKind::Identifier => {
            curr_precedence = Precedence::Highest;
            match names_index.get(left_token.source) {
                Some(Names::Var(id)) => Expr::Var(left_token.loc, *id),
                Some(Names::ExternFunc(uid) | Names::Func(uid) | Names::GlobalVar(uid)) => {
                    Expr::Global(left_token, *uid)
                }
                _ => undefined(left_token)?,
            }
        }
        TokenKind::Ampersand => {
            curr_precedence = Precedence::RefDeref;
            let operand = parse_expr(lexer, curr_fn, names_index, curr_precedence, stop, p)?;
            match operand {
                Expr::Var(token, var_id) => Expr::Ref(token, var_id),
                _ => todo!(),
            }
        }
        TokenKind::Asterisk => {
            curr_precedence = Precedence::RefDeref;
            let operand = parse_expr(lexer, curr_fn, names_index, curr_precedence, stop, p)?;
            match operand {
                Expr::Ref(token, var_id) => Expr::Var(token, var_id),
                Expr::Var(token, var_id) => Expr::Deref(token, var_id),
                Expr::Global(token, uid) => Expr::GlobalDeref(token, uid),
                _ => todo!("{operand}"),
            }
        }
        _ => unexpected_token(left_token, Some("Expected a expression."))?,
    };

    loop {
        let ptoken = lexer.peek_token();
        match (&left, ptoken.kind) {
            (Expr::Global(token, _), TokenKind::OpenParen) => {
                lexer.next_token();
                curr_precedence = Precedence::Call;
                let mut args = collect_args(lexer, curr_fn, names_index, p)?;
                let ty = match names_index.get(left_token.source) {
                    Some(Names::ExternFunc(uid)) => &p.externs[*uid].ret,
                    Some(Names::Func(uid)) => &p.funcs[*uid].ret_type,
                    _ => undefined(left_token)?,
                };
                let token = *token;
                let id = if *ty != Type::Void {
                    let id = VarId(curr_fn.vars.len());
                    curr_fn.vars.push(Var::new(token.loc, ty.clone(), None));
                    left = Expr::Var(token.loc, id);
                    Some(id)
                } else {
                    left = Expr::Void(token.loc);
                    None
                };
                curr_fn.body.push(Stmt::Funcall {
                    result: id,
                    caller: token,
                    args,
                });
            }
            _ => break,
        }
    }

    while !next_token_is(lexer, stop)? && precedence < curr_precedence {
        let token = lexer.next_token();
        curr_precedence = if let Some(precedence) = Precedence::from_token_kind(&token.kind) {
            precedence
        } else {
            return unexpected_token(token, Some("Expected operator."));
        };
        left = {
            let right = parse_expr(lexer, curr_fn, names_index, curr_precedence, stop, p)?;
            let left_ty = type_of_expr(&left, &p.global_vars, &curr_fn.vars)?;
            let right_ty = type_of_expr(&right, &p.global_vars, &curr_fn.vars)?;
            let ty = get_type_of_binop(left_ty, &token, right_ty)?;

            let id = {
                let len = curr_fn.vars.len();
                curr_fn.vars.push(Var::new(left_token.loc, ty, None));
                VarId(len)
            };

            curr_fn.body.push(Stmt::Binop {
                result: id,
                operator: token,
                lhs: left,
                rhs: right,
            });
            Expr::Var(token.loc, id)
        };
    }
    Ok(left)
}

fn collect_args<'src>(
    lexer: &mut PeekableLexer<'src>,
    curr_fn: &mut Func<'src>,
    names_index: &NameSpace,
    p: &Program<'src>,
) -> Result<Vec<Expr<'src>>, AppError> {
    let mut args = vec![];
    loop {
        let ptoken = lexer.peek_token();
        match ptoken.kind {
            TokenKind::CloseParen => {
                lexer.next_token();
                break;
            }
            TokenKind::Comma => {
                lexer.next_token();
            }
            _ => {}
        }
        let arg = parse_expr(
            lexer,
            curr_fn,
            names_index,
            Precedence::Lowest,
            &[TokenKind::Comma, TokenKind::CloseParen],
            p,
        )?;
        args.push(arg);
    }
    Ok(args)
}

fn next_block<'src>(curr_fn: &mut Func<'src>) -> usize {
    curr_fn.block_count += 1;
    let id = curr_fn.block_count;
    id
}

fn next_token_is<'src>(
    lexer: &mut PeekableLexer<'src>,
    kind: &[TokenKind],
) -> Result<bool, AppError> {
    let token = lexer.peek_token();
    if token.is_eof() {
        unexpected_end_of_file(token)?;
    }
    Ok(kind.contains(&token.kind))
}

fn unexpected_token<T>(token: Token<'_>, msg: Option<impl ToString>) -> Result<T, AppError> {
    if let Some(msg) = msg {
        Err(AppError::ParseError(format!(
            "{}: Unexpected token {}, {}",
            token.loc,
            token,
            msg.to_string()
        )))
    } else {
        Err(AppError::ParseError(format!(
            "{}: Unexpected token {}",
            token.loc, token
        )))
    }
}

fn undefined<T>(token: Token<'_>) -> Result<T, AppError> {
    Err(AppError::ParseError(format!(
        "{}: Undefined name {}",
        token.loc, token
    )))
}

fn inspect<'src>(
    lexer: &mut PeekableLexer<'src>,
    kinds: &[TokenKind],
    // msg: Option<impl ToString>,
) -> Result<bool, AppError> {
    let token = lexer.peek_token();
    if token.is_eof() {
        unexpected_end_of_file(token)?;
    }
    Ok(kinds.contains(&token.kind))
}

fn unexpected_end_of_file(token: &Token<'_>) -> Result<bool, AppError> {
    Err(AppError::ParseError(format!(
        "{}: Unexpected end of file",
        token.loc
    )))
}

fn expect<'src>(
    lexer: &mut PeekableLexer<'src>,
    kind: TokenKind,
    msg: Option<impl ToString>,
) -> Result<Token<'src>, AppError> {
    let token = lexer.next_token();
    if token.kind == kind {
        return Ok(token);
    }
    unexpected_token(token, msg)
}

fn get_type_of_binop<'src>(lhs: Type, op: &Token<'src>, rhs: Type) -> Result<Type, AppError> {
    use TokenKind::*;
    match op.kind {
        Eq | NotEq | Gt | GtEq | Lt | LtEq | DoublePipe | DoubleAmpersand => {
            return Ok(Type::Bool);
        }
        _ => {
            return Ok(Type::Int);
        }
    }
}
