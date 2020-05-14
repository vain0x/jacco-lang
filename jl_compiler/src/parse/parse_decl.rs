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
                let (colon_opt, ty_opt) = parse_ty_ascription(px);
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

fn parse_expr_decl(px: &mut Px) -> Option<PExprDecl> {
    let expr = parse_expr(px)?;
    let semi_opt = px.eat(TokenKind::Semi);

    Some(PExprDecl { expr, semi_opt })
}

fn parse_let_decl(px: &mut Px) -> PLetDecl {
    let keyword = px.expect(TokenKind::Let);

    let name_opt = parse_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);

    PLetDecl {
        keyword,
        name_opt,
        colon_opt,
        ty_opt,
        equal_opt,
        init_opt,
        semi_opt,
    }
}

fn parse_fn_decl(px: &mut Px) -> PFnDecl {
    let keyword = px.expect(TokenKind::Fn);

    let name_opt = parse_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_opt) = parse_result(px);
    let block_opt = parse_block(px);

    PFnDecl {
        keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_opt,
        block_opt,
    }
}

fn parse_extern_fn_decl(px: &mut Px) -> PExternFnDecl {
    let extern_keyword = px.expect(TokenKind::Extern);
    let fn_keyword = px.expect(TokenKind::Fn);

    let name_opt = parse_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_opt) = parse_result(px);
    let semi_opt = px.eat(TokenKind::Semi);

    PExternFnDecl {
        extern_keyword,
        fn_keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_opt,
        semi_opt,
    }
}

pub(crate) fn parse_struct_decl(px: &mut Px) -> PStructDecl {
    let keyword = px.expect(TokenKind::Struct);

    let name_opt = parse_name(px);
    let semi_opt = px.eat(TokenKind::Semi);

    PStructDecl {
        keyword,
        name_opt,
        semi_opt,
    }
}

pub(crate) fn parse_decl(px: &mut Px) -> Option<PDecl> {
    let decl = match px.next() {
        TokenKind::Let => PDecl::Let(parse_let_decl(px)),
        TokenKind::Fn => PDecl::Fn(parse_fn_decl(px)),
        TokenKind::Extern if px.nth(1) == TokenKind::Fn => {
            PDecl::ExternFn(parse_extern_fn_decl(px))
        }
        TokenKind::Struct => PDecl::Struct(parse_struct_decl(px)),
        _ => PDecl::Expr(parse_expr_decl(px)?),
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
                p_error("expected decl", px);
                px.bump();
            }
        }
    }

    if placement == Placement::Local {
        match decls.pop() {
            Some(PDecl::Expr(PExprDecl {
                expr: last,
                semi_opt: None,
            })) => {
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
    let eof = px.finish();

    PRoot { decls, eof }
}
