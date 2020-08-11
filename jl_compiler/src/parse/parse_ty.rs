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
            alloc_name_ty(event, name, px)
        }
        TokenKind::Underscore => {
            let token = px.bump();
            alloc_infer_ty(event, token, px)
        }
        TokenKind::LeftParen => {
            let left_paren = px.bump();
            let right_paren_opt = px.eat(TokenKind::RightParen);
            alloc_unit_ty(event, left_paren, right_paren_opt, px)
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
