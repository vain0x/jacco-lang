use super::*;

pub(crate) fn parse_mut(px: &mut Px) -> Option<PMut> {
    let p_mut = match px.next() {
        TokenKind::Const => (KMut::Const, px.bump()),
        TokenKind::Mut => (KMut::Mut, px.bump()),
        _ => return None,
    };
    Some(p_mut)
}

pub(crate) fn parse_ty(px: &mut Px) -> Option<PTy> {
    let ty = match px.next() {
        TokenKind::Ident => {
            let name = parse_name(px).unwrap();
            PTy::Name(name)
        }
        TokenKind::LeftParen => {
            let left_paren = px.bump();
            let right_paren_opt = px.eat(TokenKind::RightParen);
            PTy::Unit(PUnitTy {
                left_paren,
                right_paren_opt,
            })
        }
        TokenKind::Bang => {
            let bang = px.bump();
            PTy::Never(PNeverTy { bang })
        }
        TokenKind::Star => {
            let star = px.bump();
            let mut_opt = parse_mut(px);
            let ty_opt = parse_ty(px).map(Box::new);
            PTy::Ptr(PPtrTy {
                star,
                mut_opt,
                ty_opt,
            })
        }
        _ => return None,
    };
    Some(ty)
}

/// 型注釈 (`: ty`) のパース
pub(crate) fn parse_ty_ascription(px: &mut Px) -> (Option<PToken>, Option<PTy>) {
    let colon_opt = px.eat(TokenKind::Colon);

    let ty_opt = if colon_opt.is_some() {
        parse_ty(px)
    } else {
        None
    };

    (colon_opt, ty_opt)
}
