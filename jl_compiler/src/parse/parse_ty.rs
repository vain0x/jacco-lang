use super::*;

pub(crate) fn parse_mut(px: &mut Px) -> Option<PMut> {
    let p_mut = match px.next() {
        TokenKind::Const => (KMut::Const, px.bump()),
        TokenKind::Mut => (KMut::Mut, px.bump()),
        _ => return None,
    };
    Some(p_mut)
}

pub(crate) fn parse_ty(px: &mut Px) -> Option<AfterTy> {
    let event = px.start_element();
    let ty = match px.next() {
        TokenKind::Ident => {
            let name = parse_qualifiable_name(px).unwrap();
            match px.next() {
                TokenKind::LeftBracket => {
                    let ty_arg_list = parse_ty_arg_list(px).unwrap();
                    alloc_app_ty(event, name, ty_arg_list, px)
                }
                _ => alloc_name_ty(event, name, px),
            }
        }
        TokenKind::Underscore => {
            let token = px.bump();
            alloc_infer_ty(event, token, px)
        }
        TokenKind::Unit => {
            let token = px.bump();
            alloc_unit_ty(event, token, px)
        }
        TokenKind::LeftParen => {
            let left_paren = px.bump();
            let right_paren_opt = px.eat(TokenKind::RightParen);
            alloc_unit_ty_from_parens(event, left_paren, right_paren_opt, px)
        }
        TokenKind::Bang => {
            let bang = px.bump();
            alloc_never_ty(event, bang, px)
        }
        TokenKind::Star => {
            let star = px.bump();
            let mut_opt = parse_mut(px);
            let ty_opt = parse_ty(px);
            alloc_ptr_ty(event, star, mut_opt, ty_opt, px)
        }
        TokenKind::Fn => {
            let keyword = px.bump();
            let param_ty_list_opt = parse_param_ty_list(px);
            let (arrow_opt, result_ty_opt) = parse_result_ty(px);
            alloc_fn_ty(
                event,
                keyword,
                param_ty_list_opt,
                arrow_opt,
                result_ty_opt,
                px,
            )
        }
        _ => return None,
    };
    Some(ty)
}

/// 型注釈 (`: ty`) のパース
pub(crate) fn parse_ty_ascription(px: &mut Px) -> (Option<PToken>, Option<AfterTy>) {
    let colon_opt = px.eat(TokenKind::Colon);

    let ty_opt = if colon_opt.is_some() {
        parse_ty(px)
    } else {
        None
    };

    (colon_opt, ty_opt)
}

/// 型引数リストのパース
pub(crate) fn parse_ty_arg_list(px: &mut Px) -> Option<AfterParamTyList> {
    let left_bracket = px.eat(TokenKind::LeftBracket)?;
    let mut ty_args = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBracket | TokenKind::RightBrace => break,
            _ => {}
        }

        let ty_arg_event = px.start_element();
        let ty = match parse_ty(px) {
            Some(it) => it,
            None => {
                px.skip();
                continue;
            }
        };
        let comma_opt = px.eat(TokenKind::Comma);

        ty_args.push(alloc_ty_arg(ty_arg_event, ty, comma_opt, px));
    }

    let right_bracket_opt = px.eat(TokenKind::RightBracket);
    Some(alloc_ty_arg_list(
        left_bracket,
        ty_args,
        right_bracket_opt,
        px,
    ))
}

/// 関数ポインタ型のパラメータリストのパース
fn parse_param_ty_list(px: &mut Px) -> Option<AfterParamTyList> {
    let left_paren = px.eat(TokenKind::LeftParen)?;
    let mut param_tys = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightParen | TokenKind::RightBrace => break,
            _ => {}
        }

        let param_event = px.start_element();
        let ty = match parse_ty(px) {
            Some(it) => it,
            None => {
                px.skip();
                continue;
            }
        };
        let comma_opt = px.eat(TokenKind::Comma);

        param_tys.push(alloc_param_ty(param_event, ty, comma_opt, px));
    }

    let right_paren_opt = px.eat(TokenKind::RightParen);
    Some(alloc_param_ty_list(
        left_paren,
        param_tys,
        right_paren_opt,
        px,
    ))
}
