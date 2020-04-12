use super::*;

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

    PStmt::Fn {
        keyword: keyword.into_location(),
        block_opt,
    }
}

pub(crate) fn parse_semi(px: &mut Px) -> (Vec<PStmt>, Option<PTerm>) {
    let mut last_opt = None;

    if px.next() != TokenKind::RightBrace {
        last_opt = Some(parse_term(px));
    }

    while !px.at_eof() && px.next() != TokenKind::RightBrace {
        px.bump();
    }

    (vec![], last_opt)
}

fn parse_stmt(px: &mut Px) -> Option<PStmt> {
    px.eat(TokenKind::Pub);

    match px.next() {
        TokenKind::Fn => Some(parse_fn_stmt(px)),
        _ => None,
    }
}

fn parse_root(px: &mut Px) -> Option<PStmt> {
    parse_stmt(px)
}

pub(crate) fn parse_tokens(mut tokens: Vec<TokenData>) -> PRoot {
    tokens.retain(|token| {
        token.kind() != TokenKind::Space
            && token.kind() != TokenKind::Comment
            && token.kind() != TokenKind::Other
    });

    let mut px = Px::new(tokens);

    let body = parse_root(&mut px);

    px.finish(body)
}
