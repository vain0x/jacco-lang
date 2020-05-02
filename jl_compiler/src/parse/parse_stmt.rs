use super::*;

impl TokenKind {
    fn is_block_terminator(self) -> bool {
        match self {
            TokenKind::Eof | TokenKind::RightBrace => true,
            _ => false,
        }
    }
}

fn parse_break_stmt(px: &mut Px) -> PStmt {
    let keyword = px.expect(TokenKind::Break);
    let semi_opt = px.eat(TokenKind::Semi);

    PStmt::Break { keyword, semi_opt }
}

fn parse_continue_stmt(px: &mut Px) -> PStmt {
    let keyword = px.expect(TokenKind::Continue);
    let semi_opt = px.eat(TokenKind::Semi);

    PStmt::Continue { keyword, semi_opt }
}

fn parse_if_stmt(px: &mut Px) -> PStmt {
    let keyword = px.expect(TokenKind::If);

    let cond_opt = if px.next().is_term_first() {
        Some(parse_term(px))
    } else {
        None
    };

    let body_opt = if px.next() == TokenKind::LeftBrace {
        Some(parse_block(px))
    } else {
        None
    };

    let else_opt = px.eat(TokenKind::Else);

    let alt_opt = if else_opt.is_some() {
        match px.next() {
            TokenKind::LeftBrace => Some(Box::new(PStmt::Block(parse_block(px)))),
            TokenKind::If => Some(Box::new(parse_if_stmt(px))),
            _ => {
                // error
                None
            }
        }
    } else {
        None
    };

    PStmt::If {
        keyword,
        cond_opt,
        body_opt,
        else_opt,
        alt_opt,
    }
}

fn parse_while_stmt(px: &mut Px) -> PStmt {
    let keyword = px.expect(TokenKind::While);

    let cond_opt = if px.next().is_term_first() {
        Some(parse_term(px))
    } else {
        None
    };

    let body_opt = if px.next() == TokenKind::LeftBrace {
        Some(parse_block(px))
    } else {
        None
    };

    PStmt::While {
        keyword,
        cond_opt,
        body_opt,
    }
}

fn parse_loop_stmt(px: &mut Px) -> PStmt {
    let keyword = px.expect(TokenKind::Loop);

    let body_opt = if px.next() == TokenKind::LeftBrace {
        Some(parse_block(px))
    } else {
        None
    };

    PStmt::Loop { keyword, body_opt }
}

fn parse_let_stmt(px: &mut Px) -> PStmt {
    let keyword = px.expect(TokenKind::Let);

    let name_opt = eat_name(px);

    let init_opt = if px.next() == TokenKind::Equal {
        px.bump();

        if px.next().is_term_first() {
            Some(parse_term(px))
        } else {
            // error
            None
        }
    } else {
        None
    };

    px.eat(TokenKind::Semi);

    PStmt::Let {
        keyword,
        name_opt,
        init_opt,
    }
}

fn parse_fn_stmt(px: &mut Px) -> PStmt {
    let keyword = px.expect(TokenKind::Fn);

    px.eat(TokenKind::Ident);
    px.eat(TokenKind::LeftParen);
    px.eat(TokenKind::RightParen);

    if px.eat(TokenKind::RightSlimArrow).is_some() {
        px.eat(TokenKind::Ident);
    }

    let block_opt = if px.next() == TokenKind::LeftBrace {
        Some(parse_block(px))
    } else {
        None
    };

    PStmt::Fn { keyword, block_opt }
}

fn parse_extern_fn_stmt(px: &mut Px) -> PStmt {
    let extern_keyword = px.expect(TokenKind::Extern);

    let fn_keyword = px.expect(TokenKind::Fn);

    let name_opt = eat_name(px);

    let param_list_opt = if let Some(left) = px.eat(TokenKind::LeftParen) {
        let mut params = vec![];

        loop {
            match px.next() {
                TokenKind::Eof | TokenKind::RightParen => break,
                TokenKind::Ident => {
                    let name = parse_name(px);

                    let colon_opt = px.eat(TokenKind::Colon);

                    let ty_opt = if colon_opt.is_some() {
                        eat_name(px)
                    } else {
                        None
                    };

                    let comma_opt = px.eat(TokenKind::Comma);

                    params.push(PParam {
                        name,
                        colon_opt,
                        ty_opt,
                        comma_opt,
                    })
                }
                _ => unimplemented!(),
            }
        }

        let right_opt = px.eat(TokenKind::RightParen);

        Some(PParamList {
            left,
            right_opt,
            params,
        })
    } else {
        None
    };

    let result_opt = match px.eat(TokenKind::RightSlimArrow) {
        None => None,
        Some(arrow) => {
            let ty_opt = eat_name(px);
            Some(PResult { arrow, ty_opt })
        }
    };

    let semi_opt = px.eat(TokenKind::Semi);

    PStmt::ExternFn {
        extern_keyword,
        fn_keyword,
        name_opt,
        param_list_opt,
        result_opt,
        semi_opt,
    }
}

pub(crate) fn parse_semi(placement: Placement, px: &mut Px) -> (Vec<PStmt>, Option<PTerm>) {
    let mut stmts = vec![];
    let mut last_opt = None;

    while !px.next().is_block_terminator() {
        px.eat(TokenKind::Pub);

        match px.next() {
            TokenKind::Semi => {
                // Empty statement.
                px.bump();
            }
            TokenKind::Break => {
                stmts.push(parse_break_stmt(px));
            }
            TokenKind::Continue => {
                stmts.push(parse_continue_stmt(px));
            }
            TokenKind::If => {
                stmts.push(parse_if_stmt(px));
            }
            TokenKind::While => {
                stmts.push(parse_while_stmt(px));
            }
            TokenKind::Loop => {
                stmts.push(parse_loop_stmt(px));
            }
            TokenKind::Let => {
                stmts.push(parse_let_stmt(px));
            }
            TokenKind::Fn => {
                stmts.push(parse_fn_stmt(px));
            }
            TokenKind::Extern => {
                stmts.push(parse_extern_fn_stmt(px));
            }
            kind if kind.is_term_first() && placement == Placement::Local => {
                let term = parse_term(px);

                if px.next().is_block_terminator() {
                    last_opt = Some(term);
                    break;
                }

                let semi_opt = px.eat(TokenKind::Semi);
                stmts.push(PStmt::Expr { term, semi_opt });
            }
            kind if kind.is_block_terminator() => {
                break;
            }
            _ => {
                // error
                px.bump();
            }
        }
    }

    (stmts, last_opt)
}

fn parse_root(px: &mut Px) -> Vec<PStmt> {
    let (stmts, _) = parse_semi(Placement::Global, px);
    stmts
}

pub(crate) fn parse_tokens(mut tokens: Vec<TokenData>, logger: Logger) -> PRoot {
    tokens.retain(|token| match token.kind() {
        TokenKind::Other => {
            logger.error(
                token.location().clone(),
                format!("invalid token {:?}", token.text()),
            );
            false
        }
        TokenKind::Space | TokenKind::Comment => false,
        _ => true,
    });

    let mut px = Px::new(tokens);

    let body = parse_root(&mut px);

    px.finish(body)
}
