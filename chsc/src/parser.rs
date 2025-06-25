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
    let mut vars_index = VarsIndex::default();
    vars_index.push_scope();

    loop {
        let token = lexer.next_token();
        if token.is_eof() {
            break;
        }
        match token.kind {
            TokenKind::Keyword if token.source == "fn" => {
                let fn_ = parse_fn(&mut lexer, &mut vars_index)?;
                p.funcs.push(fn_);
            }
            TokenKind::Keyword if token.source == "extern" => {
                let extern_ = parse_extern(&mut lexer, &mut vars_index)?;
                let id = VarId(p.externs.len(), true);
                vars_index.insert_var_index(extern_.name(), id);
                p.externs.push(extern_);
            }
            _ => todo!("{token}"),
        }
    }

    return Ok(p);
}

fn parse_extern<'src>(
    lexer: &mut PeekableLexer<'src>,
    vars_index: &mut VarsIndex<'src>,
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
    vars_index: &mut VarsIndex<'src>,
) -> ParseResult<'src, Func<'src>> {
    let name = expect(lexer, TokenKind::Identifier, None::<&str>)?;
    expect(lexer, TokenKind::OpenParen, None::<&str>)?;
    let args: Vec<(Token<'src>, Type<'src>)> = Default::default();
    expect(lexer, TokenKind::CloseParen, None::<&str>)?;
    let ret_type: Option<Type<'src>> = Default::default();

    let body: Vec<Stmt<'src>> = Default::default();
    let vars: Vec<Var<'src>> = Default::default();

    let mut func = Func {
        name,
        args,
        ret_type,
        vars,
        block_count: 0,
        body,
    };

    expect(lexer, TokenKind::OpenBrace, None::<&str>)?;
    vars_index.push_scope();
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
                parse_stmt(lexer, &mut func, vars_index)?;
            }
        }
    }
    vars_index.pop_scope();

    Ok(func)
}

fn parse_stmt<'src>(
    lexer: &mut PeekableLexer<'src>,
    curr_fn: &mut Func<'src>,
    vars_index: &mut VarsIndex<'src>,
) -> ParseResult<'src, ()> {
    let token = lexer.peek_token();
    match token.kind {
        TokenKind::Keyword if token.source == "var" => {
            lexer.next_token();
            let token = expect(lexer, TokenKind::Identifier, None::<&str>)?;
            let id = VarId(curr_fn.vars.len(), false);
            vars_index.insert_var_index(token.source, id);
            curr_fn.vars.push(Var { token, ty: None });
            if inspect(lexer, &[TokenKind::Assign])? {
                lexer.next_token();
                let value = parse_expr(
                    lexer,
                    curr_fn,
                    vars_index,
                    Precedence::Lowest,
                    &[TokenKind::SemiColon],
                )?;
                expect(lexer, TokenKind::SemiColon, None::<&str>)?;
                curr_fn.body.push(Stmt::Assign {
                    lhs: Expr::Var(token, id),
                    rhs: value,
                });
                return Ok(());
            } else {
                expect(lexer, TokenKind::SemiColon, None::<&str>)?;
                return Ok(());
            }
        }
        TokenKind::Keyword if token.source == "return" => {
            lexer.next_token();
            if inspect(lexer, &[TokenKind::SemiColon])? {
                curr_fn.body.push(Stmt::Return(None));
                return Ok(());
            } else {
                let expr = parse_expr(
                    lexer,
                    curr_fn,
                    vars_index,
                    Precedence::Lowest,
                    &[TokenKind::SemiColon],
                )?;
                expect(lexer, TokenKind::SemiColon, None::<&str>)?;
                curr_fn.body.push(Stmt::Return(Some(expr)));
                return Ok(());
            }
        }
        TokenKind::Keyword if token.source == "if" => {
            lexer.next_token();
            vars_index.push_scope();
            expect(lexer, TokenKind::OpenParen, None::<&str>)?;
            let cond = parse_expr(
                lexer,
                curr_fn,
                vars_index,
                Precedence::Lowest,
                &[TokenKind::CloseParen],
            )?;
            expect(lexer, TokenKind::CloseParen, None::<&str>)?;
            let after_block = next_block(curr_fn)?;

            curr_fn.body.push(Stmt::JZ(cond, after_block));

            parse_stmt(lexer, curr_fn, vars_index)?;

            if lexer.peek_token().source == "else" {
                let after_else_block = next_block(curr_fn)?;
                curr_fn.body.push(Stmt::Jmp(after_else_block));

                curr_fn.body.push(Stmt::Block(after_block));
                lexer.next_token();

                vars_index.push_scope();
                parse_stmt(lexer, curr_fn, vars_index)?;
                vars_index.pop_scope();
                curr_fn.body.push(Stmt::Block(after_else_block));
                return Ok(());
            }
            curr_fn.body.push(Stmt::Block(after_block));
            Ok(())
        }
        TokenKind::Keyword if token.source == "while" => {
            lexer.next_token();
            vars_index.push_scope();
            expect(lexer, TokenKind::OpenParen, None::<&str>)?;
            let cond_block = next_block(curr_fn)?;
            curr_fn.body.push(Stmt::Block(cond_block));
            let cond = parse_expr(
                lexer,
                curr_fn,
                vars_index,
                Precedence::Lowest,
                &[TokenKind::CloseParen],
            )?;
            expect(lexer, TokenKind::CloseParen, None::<&str>)?;
            let after_block = next_block(curr_fn)?;

            curr_fn.body.push(Stmt::JZ(cond, after_block));

            parse_stmt(lexer, curr_fn, vars_index)?;

            curr_fn.body.push(Stmt::Jmp(cond_block));
            curr_fn.body.push(Stmt::Block(after_block));
            Ok(())
        }
        TokenKind::OpenBrace => {
            expect(lexer, TokenKind::OpenBrace, None::<&str>)?;
            vars_index.push_scope();
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
                        parse_stmt(lexer, curr_fn, vars_index)?;
                    }
                }
            }
            vars_index.pop_scope();
            Ok(())
        }
        _ => {
            let lhs = parse_expr(
                lexer,
                curr_fn,
                vars_index,
                Precedence::Lowest,
                &[TokenKind::Assign, TokenKind::SemiColon],
            )?;

            if lexer.peek_token().kind == TokenKind::SemiColon {
                lexer.next_token();
                return Ok(());
            }

            expect(lexer, TokenKind::Assign, None::<&str>)?;
            let rhs = parse_expr(
                lexer,
                curr_fn,
                vars_index,
                Precedence::Lowest,
                &[TokenKind::SemiColon],
            )?;
            expect(lexer, TokenKind::SemiColon, None::<&str>)?;
            curr_fn.body.push(Stmt::Assign { lhs, rhs });

            Ok(())
        }
    }
}

fn parse_expr<'src>(
    lexer: &mut PeekableLexer<'src>,
    curr_fn: &mut Func<'src>,
    vars_index: &VarsIndex,
    precedence: Precedence,
    stop: &[TokenKind],
) -> ParseResult<'src, Expr<'src>> {
    let mut curr_precedence = Precedence::Lowest;
    let left_token = lexer.next_token();
    let mut left = match left_token.kind {
        TokenKind::IntegerNumber => Expr::IntLit(left_token),
        TokenKind::StringLiteral => Expr::StrLit(left_token),
        TokenKind::Identifier => {
            let Some(var_id) = vars_index.get_var_index(left_token.source) else {
                return Err(ParserError::Undefined(left_token));
            };
            Expr::Var(left_token, *var_id)
        }
        TokenKind::Keyword if left_token.source == "syscall" => {
            expect(lexer, TokenKind::OpenParen, None::<&str>)?;
            curr_precedence = Precedence::Call;
            let mut args = vec![];
            loop {
                let ptoken = lexer.peek_token();
                match ptoken.kind {
                    TokenKind::CloseParen => {
                        lexer.next_token();
                        let id = VarId(curr_fn.vars.len(), false);
                        curr_fn.vars.push(Var {
                            token: left_token,
                            ty: None,
                        });
                        curr_fn.body.push(Stmt::Syscall {
                            result: Some(id),
                            args,
                        });
                        break Expr::Var(left_token, id);
                    }
                    TokenKind::Comma => {
                        lexer.next_token();
                        continue;
                    }
                    _ => {
                        let arg = parse_expr(
                            lexer,
                            curr_fn,
                            vars_index,
                            Precedence::Lowest,
                            &[TokenKind::Comma, TokenKind::CloseParen],
                        )?;
                        args.push(arg);
                    }
                }
            }
        }
        TokenKind::Bang => {
            let operand = parse_expr(lexer, curr_fn, vars_index, curr_precedence, stop)?;
            let id = VarId(curr_fn.vars.len(), false);
            curr_fn.vars.push(Var {
                token: left_token,
                ty: None,
            });
            curr_fn.body.push(Stmt::Unop {
                result: id,
                operator: left_token,
                operand,
            });
            curr_precedence = Precedence::NegNot;
            Expr::Var(left_token, id)
        }
        TokenKind::Ampersand => {
            let operand = parse_expr(lexer, curr_fn, vars_index, curr_precedence, stop)?;
            curr_precedence = Precedence::RefDeref;
            match operand {
                Expr::Var(token, var_id) => Expr::Ref(token, var_id),
                Expr::Deref(token, var_id) => Expr::Var(token, var_id),
                _ => todo!(),
            }
        }
        _ => todo!("{left_token}"),
    };

    loop {
        let ptoken = lexer.peek_token();
        match ptoken.kind {
            TokenKind::OpenParen => {
                lexer.next_token();
                let mut args = vec![];
                left = loop {
                    let ptoken = lexer.peek_token();
                    match ptoken.kind {
                        TokenKind::CloseParen => {
                            lexer.next_token();
                            let id = VarId(curr_fn.vars.len(), false);
                            curr_fn.vars.push(Var {
                                token: left_token,
                                ty: None,
                            });
                            curr_fn.body.push(Stmt::Funcall {
                                result: Some(id),
                                caller: left,
                                args,
                            });
                            break Expr::Var(left_token, id);
                        }
                        TokenKind::Comma => {
                            lexer.next_token();
                            continue;
                        }
                        _ => {
                            let arg = parse_expr(
                                lexer,
                                curr_fn,
                                vars_index,
                                Precedence::Lowest,
                                &[TokenKind::Comma, TokenKind::CloseParen],
                            )?;
                            args.push(arg);
                        }
                    }
                };
                curr_precedence = Precedence::Call;
            }
            TokenKind::Caret => {
                lexer.next_token();
                let id = VarId(curr_fn.vars.len(), false);
                curr_fn.vars.push(Var {
                    token: left_token,
                    ty: None,
                });
                curr_precedence = Precedence::RefDeref;
                left = match left {
                    Expr::Var(token, var_id) => Expr::Deref(token, var_id),
                    Expr::Ref(token, var_id) => Expr::Var(token, var_id),
                    _ => {
                        curr_fn.body.push(Stmt::Assign {
                            lhs: Expr::Var(left_token, id),
                            rhs: left,
                        });
                        Expr::Deref(left_token, id)
                    }
                }
            }
            _ => {
                curr_precedence = Precedence::from_token_kind(&lexer.peek_token().kind);
                break;
            }
        }
    }

    while !next_token_is(lexer, stop)? && precedence < curr_precedence {
        let token = lexer.next_token();
        curr_precedence = Precedence::from_token_kind(&token.kind);
        left = {
            let right = parse_expr(lexer, curr_fn, vars_index, curr_precedence, stop)?;
            let id = VarId(curr_fn.vars.len(), false);
            curr_fn.vars.push(Var { token, ty: None });
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

fn new_block_with_stmt<'src>(
    lexer: &mut PeekableLexer<'src>,
    curr_fn: &mut Func<'src>,
    vars_index: &mut VarsIndex<'src>,
) -> ParseResult<'src, BlockId> {
    curr_fn.block_count += 1;
    let id = BlockId(curr_fn.block_count);
    curr_fn.body.push(Stmt::Block(id));
    parse_stmt(lexer, curr_fn, vars_index)?;
    Ok(id)
}

fn next_block<'src>(curr_fn: &mut Func<'src>) -> ParseResult<'src, BlockId> {
    curr_fn.block_count += 1;
    let id = BlockId(curr_fn.block_count);
    Ok(id)
}

fn peek_infix_precedence(lexer: &mut PeekableLexer<'_>) -> Precedence {
    Precedence::from_token_kind(&lexer.peek_token().kind)
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
