use crate::arena::Arena;
use crate::ast::{Expr, ExternFunc, Func, GlobalVar, Module, Name, Precedence, Stmt, Type};
use crate::chslexer::*;
use crate::utils::AppError;

pub fn parse_module<'src>(
    strings: &'src Arena<String>,
    file_path: &'src str,
    source: &'src String,
) -> Result<Module<'src>, AppError> {
    let mut lexer = PeekableLexer::new(file_path, source);
    lexer.set_is_keyword_fn(|s| {
        matches!(
            s,"module"
                | "fn"
                | "return"
                | "extern"
                | "var"
                | "syscall"
                | "while"
                | "if"
                | "else"
                | "import"
        )
    });

    let mut module = Module {
        ..Default::default()
    };

    loop {
        let token = lexer.next_token();
        if token.is_eof() {
            break;
        }
        match token.kind {
            TokenKind::MacroCall if token.source == "@exec" => {
                let stmt = parse_stmt(&mut lexer)?;
                module.add_exec(stmt);
            }
            TokenKind::Keyword if token.source == "fn" => {
                let r#fn = parse_fn(&mut lexer)?;
                module.add_fn(r#fn);
            }
            TokenKind::Keyword if token.source == "extern" => {
                let r#extern = parse_extern_fn(&mut lexer)?;
                module.add_extern_fn(r#extern);
            }
            TokenKind::Keyword if token.source == "import" => {
                let file_path = expect(&mut lexer, TokenKind::StringLiteral, Some(""))?;
                let file_path = strings.alloc(file_path.unescape());
                let source =
                    std::fs::read_to_string(&file_path).map_err(|e| AppError::FileError {
                        path: file_path.clone(),
                        error: e,
                    })?;
                let source = strings.alloc(source);
                let m = parse_module(strings, file_path, source)?;
                module.imported_funcs.extend(m.funcs);
                module.name_space.extend(m.name_space);
                expect(&mut lexer, TokenKind::SemiColon, Some("Expected `;`"))?;
            }
            TokenKind::Keyword if token.source == "var" => {
                let name = expect(
                    &mut lexer,
                    TokenKind::Identifier,
                    Some("Expected variable name"),
                )?;
                let is_vec = if inspect(&mut lexer, &[TokenKind::OpenBracket])? {
                    lexer.next_token();
                    let sz = expect(
                        &mut lexer,
                        TokenKind::IntegerNumber,
                        Some("Expected size of vector. Hint: `var xs[10];`"),
                    )?;
                    expect(&mut lexer, TokenKind::CloseBracket, None::<&str>)?;
                    Some(sz.source.parse().unwrap())
                } else {
                    None
                };
                let r#type = parse_type(&mut lexer)?;
                let expr = None;
                expect(&mut lexer, TokenKind::SemiColon, Some("Expected `;`"))?;
                module.add_global_vars(GlobalVar {
                    name,
                    is_vec,
                    r#type,
                    expr,
                });
            }
            _ => unexpected_token(token, Some("Expected a top level definition"))?,
        }
    }

    return Ok(module);
}

fn parse_extern_fn<'src>(lexer: &mut PeekableLexer<'src>) -> Result<ExternFunc<'src>, AppError> {
    let token = lexer.next_token();
    if token.source != "fn" {
        return unexpected_token(token, Some("Expect `fn` after `extern`"));
    }
    let name = expect(lexer, TokenKind::Identifier, Some("Expected function name"))?;
    let mut args = vec![];
    let mut ret_type = None;
    let mut is_variadic = false;

    expect(lexer, TokenKind::OpenParen, Some("Expected `(`"))?;
    loop {
        let ptoken = lexer.peek_token();
        match ptoken.kind {
            TokenKind::CloseParen => {
                break;
            }
            TokenKind::Comma => {
                lexer.next_token();
            }
            _ => {
                if inspect(lexer, &[TokenKind::Splat])? {
                    lexer.next_token();
                    is_variadic = true;
                    break;
                }
                let r#type = parse_type(lexer)?;
                args.push(r#type);
            }
        }
    }

    expect(lexer, TokenKind::CloseParen, None::<&str>)?;
    if inspect(lexer, &[TokenKind::Arrow])? {
        lexer.next_token();
        ret_type = Some(parse_type(lexer)?);
    }

    expect(lexer, TokenKind::SemiColon, Some("Expected `;`"))?;

    Ok(ExternFunc {
        name,
        args,
        ret_type,
        is_variadic,
    })
}

fn parse_fn<'src>(lexer: &mut PeekableLexer<'src>) -> Result<Func<'src>, AppError> {
    let name = expect(lexer, TokenKind::Identifier, Some("Expected function name"))?;
    let mut args = vec![];
    let mut ret_type = None;
    let mut body = vec![];

    expect(lexer, TokenKind::OpenParen, Some("Expected `(`"))?;
    loop {
        let ptoken = lexer.peek_token();
        match ptoken.kind {
            TokenKind::CloseParen => {
                lexer.next_token();
                if inspect(lexer, &[TokenKind::Arrow])? {
                    lexer.next_token();
                    ret_type = Some(parse_type(lexer)?);
                }
                break;
            }
            TokenKind::Comma => {
                lexer.next_token();
            }
            TokenKind::Identifier => {
                let name = lexer.next_token();
                let r#type = parse_type(lexer)?;
                args.push((name, r#type));
            }
            _ => {
                let token = lexer.next_token();
                unexpected_token(token, Some("Expected argument"))?
            }
        }
    }

    expect(lexer, TokenKind::OpenBrace, Some("Expected `{`"))?;
    loop {
        let ptoken = lexer.peek_token();
        match ptoken.kind {
            TokenKind::CloseBrace => {
                lexer.next_token();
                break;
            }
            _ => {
                let stmt = parse_stmt(lexer)?;
                body.push(stmt);
            }
        }
    }

    Ok(Func {
        name,
        args,
        ret_type,
        body,
    })
}

fn parse_stmt<'src>(lexer: &mut PeekableLexer<'src>) -> Result<Stmt<'src>, AppError> {
    let ptoken = lexer.peek_token();
    match ptoken.kind {
        TokenKind::Keyword if ptoken.source == "return" => {
            let loc = lexer.next_token().loc;
            let expr = if inspect(lexer, &[TokenKind::SemiColon])? {
                None
            } else {
                Some(parse_expr(
                    lexer,
                    &[TokenKind::SemiColon],
                    Precedence::Lowest,
                )?)
            };
            expect(lexer, TokenKind::SemiColon, Some("Expected `;`"))?;
            return Ok(Stmt::Return { loc, expr });
        }
        TokenKind::Keyword if ptoken.source == "var" => {
            lexer.next_token();
            let name = expect(lexer, TokenKind::Identifier, Some("Expected variable name"))?;
            let is_vec = if inspect(lexer, &[TokenKind::OpenBracket])? {
                lexer.next_token();
                let sz = expect(
                    lexer,
                    TokenKind::IntegerNumber,
                    Some("Expected size of vector. Hint: `var xs[10];`"),
                )?;
                expect(lexer, TokenKind::CloseBracket, None::<&str>)?;
                Some(sz.source.parse().unwrap())
            } else {
                None
            };
            let r#type = parse_type(lexer)?;
            let expr = if inspect(lexer, &[TokenKind::Assign])? {
                lexer.next_token();
                Some(parse_expr(
                    lexer,
                    &[TokenKind::SemiColon],
                    Precedence::Lowest,
                )?)
            } else {
                None
            };
            expect(lexer, TokenKind::SemiColon, Some("Expected `;`"))?;
            return Ok(Stmt::VarDecl {
                name,
                is_vec,
                r#type,
                expr,
            });
        }
        TokenKind::Keyword if ptoken.source == "while" => {
            let loc = lexer.next_token().loc;
            expect(lexer, TokenKind::OpenParen, Some("Expected `(`"))?;
            let cond = parse_expr(lexer, &[TokenKind::CloseParen], Precedence::Lowest)?;
            expect(lexer, TokenKind::CloseParen, Some("Expected `)`"))?;
            let body = parse_stmt(lexer)?;
            return Ok(Stmt::While {
                loc,
                cond,
                body: Box::new(body),
            });
        }
        TokenKind::Keyword if ptoken.source == "if" => {
            let loc = lexer.next_token().loc;
            expect(lexer, TokenKind::OpenParen, Some("Expected `(`"))?;
            let cond = parse_expr(lexer, &[TokenKind::CloseParen], Precedence::Lowest)?;
            expect(lexer, TokenKind::CloseParen, Some("Expected `)`"))?;
            let true_branch = parse_stmt(lexer)?;
            let else_branch = if lexer.peek_token().source == "else" {
                lexer.next_token();
                let else_branch = parse_stmt(lexer)?;
                Some(Box::new(else_branch))
            } else {
                None
            };
            return Ok(Stmt::If {
                loc,
                cond,
                true_branch: Box::new(true_branch),
                else_branch,
            });
        }
        TokenKind::OpenBrace => {
            let loc = lexer.next_token().loc;
            let mut body = vec![];
            loop {
                let ptoken = lexer.peek_token();
                match ptoken.kind {
                    TokenKind::CloseBrace => {
                        lexer.next_token();
                        break;
                    }
                    _ => {
                        let stmt = parse_stmt(lexer)?;
                        body.push(stmt);
                    }
                }
            }
            return Ok(Stmt::Block(loc, body));
        }
        _ => {
            let loc = ptoken.loc;
            let expr = parse_expr(
                lexer,
                &[TokenKind::SemiColon, TokenKind::Assign],
                Precedence::Lowest,
            )?;
            if next_token_is(lexer, &[TokenKind::Assign])? {
                let loc = lexer.next_token().loc;
                let rhs = parse_expr(lexer, &[TokenKind::SemiColon], Precedence::Lowest)?;
                expect(lexer, TokenKind::SemiColon, Some("Expected `;`"))?;
                return Ok(Stmt::Assign {
                    loc,
                    lhs: expr,
                    rhs,
                });
            }
            expect(lexer, TokenKind::SemiColon, Some("Expected `;`"))?;
            return Ok(Stmt::Expr { loc, expr });
        } // _ => {
          //     let token = lexer.next_token();
          //     unexpected_token(token, Some("This is not a valid statment!"))?
          // }
    }
}

fn parse_expr<'src>(
    lexer: &mut PeekableLexer<'src>,
    stop: &[TokenKind],
    precedence: Precedence,
) -> Result<Expr<'src>, AppError> {
    let mut curr_precedence = precedence;
    let left_token = lexer.next_token();
    let mut left = match left_token.kind {
        TokenKind::IntegerNumber => {
            curr_precedence = Precedence::Highest;
            Expr::IntLit(left_token)
        }
        TokenKind::StringLiteral => {
            curr_precedence = Precedence::Highest;
            Expr::StrLit(left_token)
        }
        TokenKind::Identifier => {
            curr_precedence = Precedence::Highest;
            Expr::Ident(left_token)
        }
        TokenKind::Asterisk => {
            curr_precedence = Precedence::RefDeref;
            let expr = parse_expr(lexer, stop, Precedence::Lowest)?;
            Expr::Deref(left_token.loc, Box::new(expr))
        }
        TokenKind::Ampersand => {
            curr_precedence = Precedence::RefDeref;
            let expr = parse_expr(lexer, stop, Precedence::Lowest)?;
            Expr::Ref(left_token.loc, Box::new(expr))
        }
        TokenKind::OpenParen => {
            curr_precedence = Precedence::Highest;
            let expr = parse_expr(lexer, &[TokenKind::CloseParen], Precedence::Lowest)?;
            expect(lexer, TokenKind::CloseParen, Some("Expected `)`"))?;
            expr
        }
        TokenKind::Keyword if left_token.source == "syscall" => {
            curr_precedence = Precedence::Call;
            expect(lexer, TokenKind::OpenParen, Some("Expected `(`"))?;
            let args = collect_args(lexer)?;
            Expr::Syscall {
                loc: left_token.loc,
                args,
            }
        }
        _ => unexpected_token(left_token, Some("This is not a valid expr!"))?,
    };
    loop {
        let ptoken = lexer.peek_token();
        match (&left, ptoken.kind) {
            (Expr::Ident(left_token), TokenKind::OpenParen) => {
                curr_precedence = Precedence::Call;
                let loc = lexer.next_token().loc;
                let args = collect_args(lexer)?;
                left = Expr::Call {
                    loc,
                    caller: Box::new(left),
                    args,
                };
            }
            (_, TokenKind::OpenBracket) => {
                curr_precedence = Precedence::Call;
                let loc = lexer.next_token().loc;
                let base = left;
                let index = parse_expr(lexer, &[TokenKind::CloseBracket], Precedence::Lowest)?;
                expect(lexer, TokenKind::CloseBracket, Some("Expected `]`"))?;
                left = Expr::Index {
                    loc,
                    base: Box::new(base),
                    index: Box::new(index),
                };
            }
            _ => break,
        }
    }
    while !next_token_is(lexer, stop)? && (precedence < curr_precedence) {
        let token = lexer.next_token();
        curr_precedence = if let Some(precedence) = Precedence::from_token_kind(&token.kind) {
            precedence
        } else {
            return unexpected_token(token, Some("Expected operator."));
        };
        let right = parse_expr(lexer, stop, curr_precedence)?;
        left = Expr::Binop {
            operator: token,
            lhs: Box::new(left),
            rhs: Box::new(right),
        }
    }
    Ok(left)
}

fn collect_args<'src>(lexer: &mut PeekableLexer<'src>) -> Result<Vec<Expr<'src>>, AppError> {
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
            &[TokenKind::CloseParen, TokenKind::Comma],
            Precedence::Lowest,
        )?;
        args.push(arg);
    }
    Ok(args)
}

fn parse_type<'src>(lexer: &mut PeekableLexer<'src>) -> Result<Type<'src>, AppError> {
    let token = lexer.next_token();
    match token.kind {
        TokenKind::Identifier => Ok(Type::Name(token)),
        TokenKind::Asterisk => Ok(Type::PtrTo(Box::new(parse_type(lexer)?))),
        _ => unexpected_token(token, Some("This is not a valid type!")),
    }
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
