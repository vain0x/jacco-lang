use super::*;

impl TokenKind {
    fn is_block_terminator(self) -> bool {
        match self {
            TokenKind::Eof | TokenKind::RightBrace => true,
            _ => false,
        }
    }
}

fn parse_let_stmt(px: &mut Px) -> PStmt {
    let keyword = px.expect(TokenKind::Let);

    let name_opt = px.eat(TokenKind::Ident);

    let name = match name_opt {
        Some(name) => name.into_text(),
        None => "".to_string(),
    };

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
        name,
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
            TokenKind::Let => {
                stmts.push(parse_let_stmt(px));
            }
            TokenKind::Fn => {
                stmts.push(parse_fn_stmt(px));
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
