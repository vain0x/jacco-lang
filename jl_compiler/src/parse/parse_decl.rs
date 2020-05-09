//! 宣言の構文解析ルール

use super::*;

fn parse_param_list(px: &mut Px) -> Option<PParamList> {
    let left = px.eat(TokenKind::LeftParen)?;

    let mut params = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightParen | TokenKind::RightBrace => break,
            TokenKind::Ident => {
                let name = parse_name(px).unwrap();

                let colon_opt = px.eat(TokenKind::Colon);

                let ty_opt = if colon_opt.is_some() {
                    parse_ty(px)
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
}

/// 結果型注釈 (`-> T`) のパース
fn parse_result(px: &mut Px) -> (Option<TokenData>, Option<PTy>) {
    let arrow_opt = px.eat(TokenKind::RightSlimArrow);
    let ty_opt = if arrow_opt.is_some() {
        parse_ty(px)
    } else {
        None
    };
    (arrow_opt, ty_opt)
}

fn parse_expr_decl(px: &mut Px) -> Option<PDecl> {
    let expr = parse_expr(px)?;
    let semi_opt = px.eat(TokenKind::Semi);

    Some(PDecl::Expr { expr, semi_opt })
}

fn parse_let_decl(px: &mut Px) -> PDecl {
    let keyword = px.expect(TokenKind::Let);

    let name_opt = parse_name(px);
    let colon_opt = px.eat(TokenKind::Colon);
    let ty_opt = if colon_opt.is_some() {
        parse_ty(px)
    } else {
        None
    };

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);

    PDecl::Let {
        keyword,
        name_opt,
        colon_opt,
        ty_opt,
        equal_opt,
        init_opt,
        semi_opt,
    }
}

fn parse_fn_decl(px: &mut Px) -> PDecl {
    let keyword = px.expect(TokenKind::Fn);

    let name_opt = parse_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_opt) = parse_result(px);
    let block_opt = parse_block(px);

    PDecl::Fn {
        keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_opt,
        block_opt,
    }
}

fn parse_extern_fn_decl(px: &mut Px) -> PDecl {
    let extern_keyword = px.expect(TokenKind::Extern);
    let fn_keyword = px.expect(TokenKind::Fn);

    let name_opt = parse_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_opt) = parse_result(px);
    let semi_opt = px.eat(TokenKind::Semi);

    PDecl::ExternFn {
        extern_keyword,
        fn_keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_opt,
        semi_opt,
    }
}

pub(crate) fn parse_decl(px: &mut Px) -> Option<PDecl> {
    let decl = match px.next() {
        TokenKind::Let => parse_let_decl(px),
        TokenKind::Fn => parse_fn_decl(px),
        TokenKind::Extern if px.nth(1) == TokenKind::Fn => parse_extern_fn_decl(px),
        _ => return parse_expr_decl(px),
    };
    Some(decl)
}

pub(crate) fn parse_semi(placement: Placement, px: &mut Px) -> (Vec<PDecl>, Option<PExpr>) {
    let mut decls = vec![];
    let mut last_opt = None;

    loop {
        px.eat(TokenKind::Pub);

        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace => break,
            TokenKind::Semi => {
                // Empty declaration.
                px.bump();
                continue;
            }
            _ => {}
        }

        match parse_decl(px) {
            Some(decl) => decls.push(decl),
            None => {
                eprintln!("expected decl {:?}", px.next());
                px.bump();
            }
        }
    }

    if placement == Placement::Local {
        match decls.pop() {
            Some(PDecl::Expr {
                expr: last,
                semi_opt: None,
            }) => {
                last_opt = Some(last);
            }
            Some(last) => decls.push(last),
            None => {}
        }
    }

    (decls, last_opt)
}

fn parse_root(px: &mut Px) -> Vec<PDecl> {
    let (decls, last_opt) = parse_semi(Placement::Global, px);

    assert!(last_opt.is_none());

    decls
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

    let mut px = Px::new(tokens, logger);

    let decls = parse_root(&mut px);

    px.finish(decls)
}
