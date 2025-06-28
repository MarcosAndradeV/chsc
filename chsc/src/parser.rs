use crate::{ast::*, chslexer::*};

#[derive(Debug)]
pub enum ParserError<'src> {
    UnexpectedEOF(Loc<'src>),
    UnexpectedToken(Token<'src>, Option<String>),
    Undefined(Token<'src>),
}

impl std::fmt::Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedEOF(loc) => write!(f, "{}: Unexpected end of file", loc),
            ParserError::UnexpectedToken(token, None) => {
                write!(f, "{}: Unexpected token {}", token.loc, token)
            }
            ParserError::UnexpectedToken(token, Some(msg)) => {
                write!(f, "{}: Unexpected token {}, {}", token.loc, token, msg)
            }
            ParserError::Undefined(token) => {
                write!(f, "{}: Undefined name {}", token.loc, token)
            }
        }
    }
}

impl std::error::Error for ParserError<'_> {}

pub type ParseResult<'src, T> = Result<T, ParserError<'src>>;

pub fn parse<'src>(
    file_path: &'src str,
    source: &'src str,
) -> Result<Program<'src>, ParserError<'src>> {
    let mut lexer = PeekableLexer::new(file_path, source);
    lexer.set_is_keyword_fn(|s| {
        matches!(
            s,
            "extern" | "fn" | "return" | "var" | "if" | "else" | "while" | "syscall"
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
                let fn_ = parse_fn(&mut lexer, &mut names_index)?;
                p.funcs.push(fn_);
            }
            TokenKind::Keyword if token.source == "extern" => {
                let r#extern = parse_extern(&mut lexer, &mut names_index)?;
                names_index.insert_var_index(r#extern.name(), Names::Global);
                p.externs.push(r#extern);
            }
            _ => todo!("{token}"),
        }
    }

    return Ok(p);
}

fn parse_extern<'src>(
    lexer: &mut PeekableLexer<'src>,
    vars_index: &mut NameSpace<'src>,
) -> ParseResult<'src, Extern<'src>> {
    let token = lexer.next_token();

    match token.kind {
        TokenKind::Identifier => {
            expect(
                lexer,
                TokenKind::SemiColon,
                Some("Expected `;` after the symbol name."),
            )?;
            return Ok(Extern::Symbol(token));
        }
        _ => todo!("{token}"),
    }
}

fn parse_fn<'src>(
    lexer: &mut PeekableLexer<'src>,
    names_index: &mut NameSpace<'src>,
) -> ParseResult<'src, Func<'src>> {
    let name = expect(lexer, TokenKind::Identifier, None::<&str>)?;
    names_index.insert_var_index(name.source, Names::Global);
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
                break;
            }
            TokenKind::Comma => {
                lexer.next_token();
            }
            TokenKind::Identifier => {
                let token = lexer.next_token();

                let id = VarId(func.vars.len());
                names_index.insert_var_index(token.source, Names::Var(id));
                func.vars.push(Var::new(token));

                func.args.push((token, Type::Name(token)));
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
                parse_stmt(lexer, &mut func, names_index)?;
            }
        }
    }
    names_index.pop_scope();

    Ok(func)
}

fn parse_stmt<'src>(
    lexer: &mut PeekableLexer<'src>,
    curr_fn: &mut Func<'src>,
    names_index: &mut NameSpace<'src>,
) -> ParseResult<'src, ()> {
    let token = lexer.peek_token();
    match token.kind {
        TokenKind::Keyword if token.source == "var" => {
            lexer.next_token();
            let token = expect(lexer, TokenKind::Identifier, None::<&str>)?;
            let id = VarId(curr_fn.vars.len());
            names_index.insert_var_index(token.source, Names::Var(id));
            curr_fn.vars.push(Var::new(token));

            if inspect(lexer, &[TokenKind::Assign])? {
                lexer.next_token();
                let rhs = parse_expr(
                    lexer,
                    curr_fn,
                    names_index,
                    Precedence::Lowest,
                    &[TokenKind::SemiColon],
                )?;
                curr_fn.body.push(Stmt::AssignVar {
                    var: (token, id),
                    rhs,
                });
            }

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
                )?;
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
            )?;
            expect(lexer, TokenKind::CloseParen, None::<&str>)?;
            let after_block = next_block(curr_fn)?;

            curr_fn.body.push(Stmt::JZ(cond, after_block));

            names_index.push_scope();
            parse_stmt(lexer, curr_fn, names_index)?;
            names_index.pop_scope();

            if lexer.peek_token().source == "else" {
                let after_else_block = next_block(curr_fn)?;
                curr_fn.body.push(Stmt::Jmp(after_else_block));

                curr_fn.body.push(Stmt::Block(after_block));
                lexer.next_token();

                names_index.push_scope();
                parse_stmt(lexer, curr_fn, names_index)?;
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
            let cond_block = next_block(curr_fn)?;
            let after_block = next_block(curr_fn)?;
            curr_fn.body.push(Stmt::Block(cond_block));
            let cond = parse_expr(
                lexer,
                curr_fn,
                names_index,
                Precedence::Lowest,
                &[TokenKind::CloseParen],
            )?;
            expect(lexer, TokenKind::CloseParen, None::<&str>)?;

            curr_fn.body.push(Stmt::JZ(cond, after_block));

            names_index.push_scope();
            parse_stmt(lexer, curr_fn, names_index)?;
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
                        parse_stmt(lexer, curr_fn, names_index)?;
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
            )?;

            let ptoken = lexer.peek_token();
            match (expr, ptoken.kind) {
                (Expr::Var(token, id), TokenKind::Assign) => {
                    lexer.next_token();
                    let rhs = parse_expr(
                        lexer,
                        curr_fn,
                        names_index,
                        Precedence::Lowest,
                        &[TokenKind::SemiColon],
                    )?;
                    expect(
                        lexer,
                        TokenKind::SemiColon,
                        Some("Expected `;` after expression."),
                    )?;
                    curr_fn.body.push(Stmt::AssignVar {
                        var: (token, id),
                        rhs,
                    });
                }
                (Expr::Deref(token, id), TokenKind::Assign) => {
                    lexer.next_token();
                    let rhs = parse_expr(
                        lexer,
                        curr_fn,
                        names_index,
                        Precedence::Lowest,
                        &[TokenKind::SemiColon],
                    )?;
                    expect(
                        lexer,
                        TokenKind::SemiColon,
                        Some("Expected `;` after expression."),
                    )?;
                    curr_fn.body.push(Stmt::Store {
                        target: (token, id),
                        rhs,
                    });
                }
                (Expr::Global(token), TokenKind::OpenParen) => {
                    lexer.next_token();
                    let mut args = collect_args(lexer, curr_fn, names_index)?;
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
) -> ParseResult<'src, Expr<'src>> {
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
            )?;
            expect(lexer, TokenKind::CloseParen, Some("Expected `)`."))?;
            expr
        }
        TokenKind::Identifier => {
            curr_precedence = Precedence::Highest;
            match names_index.get_var_index(left_token.source) {
                Some(Names::Var(id)) => Expr::Var(left_token, *id),
                Some(Names::Global) => Expr::Global(left_token),
                _ => undefined(left_token)?,
            }
        }
        TokenKind::Ampersand => {
            curr_precedence = Precedence::RefDeref;
            let operand = parse_expr(lexer, curr_fn, names_index, curr_precedence, stop)?;
            match operand {
                Expr::Var(token, var_id) => Expr::Ref(token, var_id),
                _ => todo!(),
            }
        }
        TokenKind::Asterisk => {
            curr_precedence = Precedence::RefDeref;
            let operand = parse_expr(lexer, curr_fn, names_index, curr_precedence, stop)?;
            match operand {
                Expr::Ref(token, var_id) => Expr::Deref(token, var_id),
                Expr::Var(token, var_id) => Expr::Deref(token, var_id),
                _ => todo!(),
            }
        }
        _ => unexpected_token(left_token, Some("Expected a expression."))?,
    };

    loop {
        let ptoken = lexer.peek_token();
        match (&left, ptoken.kind) {
            (Expr::Global(token), TokenKind::OpenParen) => {
                lexer.next_token();
                let mut args = collect_args(lexer, curr_fn, names_index)?;
                let id = VarId(curr_fn.vars.len());
                curr_fn.vars.push(Var::new(*token));
                curr_fn.body.push(Stmt::Funcall {
                    result: Some(id),
                    caller: *token,
                    args,
                });
                left = Expr::Var(*token, id);
            }
            _ => break,
        }
    }

    while !next_token_is(lexer, stop)? && precedence < curr_precedence {
        let token = lexer.next_token();
        curr_precedence = if let Some(precedence) = Precedence::from_token_kind(&token.kind) {
            precedence
        } else {
            return unexpected_token(token, Some("Expected `;`."));
        };
        left = {
            let right = parse_expr(lexer, curr_fn, names_index, curr_precedence, stop)?;

            let id = get_new_var_id(curr_fn, left_token);

            curr_fn.body.push(Stmt::Binop {
                result: id,
                operator: token,
                lhs: left,
                rhs: right,
            });
            Expr::Var(token, id)
        };
    }
    Ok(left)
}

fn collect_args<'src>(
    lexer: &mut PeekableLexer<'src>,
    curr_fn: &mut Func<'src>,
    names_index: &NameSpace,
) -> Result<Vec<Expr<'src>>, ParserError<'src>> {
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
        )?;
        args.push(arg);
    }
    Ok(args)
}

fn get_new_var_id<'src>(curr_fn: &mut Func<'src>, left_token: Token<'src>) -> VarId {
    let len = curr_fn.vars.len();
    curr_fn.vars.push(Var::new(left_token));
    VarId(len)
}

fn next_block<'src>(curr_fn: &mut Func<'src>) -> ParseResult<'src, BlockId> {
    curr_fn.block_count += 1;
    let id = BlockId(curr_fn.block_count);
    Ok(id)
}

fn next_token_is<'src>(
    lexer: &mut PeekableLexer<'src>,
    kind: &[TokenKind],
) -> ParseResult<'src, bool> {
    let token = lexer.peek_token();
    if token.is_eof() {
        return Err(ParserError::UnexpectedEOF(token.loc));
    }
    Ok(kind.contains(&token.kind))
}

fn unexpected_token<T>(token: Token<'_>, msg: Option<impl ToString>) -> Result<T, ParserError<'_>> {
    Err(ParserError::UnexpectedToken(
        token,
        msg.map(|m| m.to_string()),
    ))
}

fn undefined<T>(token: Token<'_>) -> Result<T, ParserError<'_>> {
    Err(ParserError::Undefined(
        token,
    ))
}


fn inspect<'src>(
    lexer: &mut PeekableLexer<'src>,
    kinds: &[TokenKind],
    // msg: Option<impl ToString>,
) -> ParseResult<'src, bool> {
    let token = lexer.peek_token();
    if token.is_eof() {
        let token = lexer.peek_token();
        return Err(ParserError::UnexpectedEOF(token.loc));
    }
    Ok(kinds.contains(&token.kind))
}

fn expect<'src>(
    lexer: &mut PeekableLexer<'src>,
    kind: TokenKind,
    msg: Option<impl ToString>,
) -> ParseResult<'src, Token<'src>> {
    let token = lexer.next_token();
    if token.kind == kind {
        return Ok(token);
    }
    return Err(ParserError::UnexpectedToken(
        token,
        msg.map(|m| m.to_string()),
    ));
}
