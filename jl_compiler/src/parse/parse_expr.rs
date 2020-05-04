use super::*;

fn parse_term_expr(px: &mut Px) -> Option<PExpr> {
    let term = parse_term(px)?;
    let semi_opt = px.eat(TokenKind::Semi);

    Some(PExpr::Term { term, semi_opt })
}

pub(crate) fn parse_block(px: &mut Px) -> Option<PBlock> {
    let left = px.eat(TokenKind::LeftBrace)?;

    let (decls, last_opt) = parse_semi(Placement::Local, px);

    let right_opt = px.eat(TokenKind::RightBrace);

    Some(PBlock {
        left,
        right_opt,
        decls,
        last_opt: last_opt.map(Box::new),
    })
}

fn parse_block_expr(px: &mut Px) -> PExpr {
    assert_eq!(px.next(), TokenKind::LeftBrace);

    let block = parse_block(px).unwrap();
    PExpr::Block(block)
}

fn parse_break_expr(px: &mut Px) -> PExpr {
    let keyword = px.expect(TokenKind::Break);
    let semi_opt = px.eat(TokenKind::Semi);

    PExpr::Break { keyword, semi_opt }
}

fn parse_continue_expr(px: &mut Px) -> PExpr {
    let keyword = px.expect(TokenKind::Continue);
    let semi_opt = px.eat(TokenKind::Semi);

    PExpr::Continue { keyword, semi_opt }
}

fn parse_if_expr(px: &mut Px) -> PExpr {
    let keyword = px.expect(TokenKind::If);

    let cond_opt = parse_cond(px);

    let body_opt = parse_block(px);

    let else_opt = px.eat(TokenKind::Else);

    let alt_opt = if else_opt.is_some() {
        match px.next() {
            TokenKind::LeftBrace => Some(Box::new(parse_block_expr(px))),
            TokenKind::If => Some(Box::new(parse_if_expr(px))),
            _ => {
                // error
                None
            }
        }
    } else {
        None
    };

    PExpr::If {
        keyword,
        cond_opt,
        body_opt,
        else_opt,
        alt_opt,
    }
}

fn parse_while_expr(px: &mut Px) -> PExpr {
    let keyword = px.expect(TokenKind::While);

    let cond_opt = parse_cond(px);

    let body_opt = parse_block(px);

    PExpr::While {
        keyword,
        cond_opt,
        body_opt,
    }
}

fn parse_loop_expr(px: &mut Px) -> PExpr {
    let keyword = px.expect(TokenKind::Loop);

    let body_opt = parse_block(px);

    PExpr::Loop { keyword, body_opt }
}

pub(crate) fn parse_expr(px: &mut Px) -> Option<PExpr> {
    let expr = match px.next() {
        TokenKind::Break => parse_break_expr(px),
        TokenKind::Continue => parse_continue_expr(px),
        TokenKind::If => parse_if_expr(px),
        TokenKind::While => parse_while_expr(px),
        TokenKind::Loop => parse_loop_expr(px),
        _ => return parse_term_expr(px),
    };
    Some(expr)
}

pub(crate) fn parse_args(args: &mut Vec<PArg>, px: &mut Px) {
    loop {
        match px.next() {
            TokenKind::Eof
            | TokenKind::RightParen
            | TokenKind::RightBracket
            | TokenKind::RightBrace => break,
            _ => {
                let expr = match parse_expr(px) {
                    None => {
                        // error
                        px.bump();
                        continue;
                    }
                    Some(expr) => expr,
                };

                let comma_opt = px.eat(TokenKind::Comma);
                let can_continue = comma_opt.is_some();

                args.push(PArg { expr, comma_opt });

                if !can_continue {
                    break;
                }
            }
        }
    }
}
