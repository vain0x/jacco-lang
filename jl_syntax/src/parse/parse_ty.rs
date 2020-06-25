use super::*;

pub(crate) fn parse_ty(px: &mut Px) -> Option<PTy> {
    match px.next() {
        TokenKind::Ident => {
            let name = parse_name(px).unwrap();
            Some(PTy::Name(PNameTy(name)))
        }
        TokenKind::LeftParen => {
            let left_paren = px.bump();
            let right_paren_opt = px.eat(TokenKind::RightParen);
            Some(PTy::Unit(PUnitTy {
                left_paren,
                right_paren_opt,
            }))
        }
        TokenKind::Bang => {
            let bang = px.bump();
            Some(PTy::Never(PNeverTy { bang }))
        }
        TokenKind::Star => {
            let star = px.bump();
            let ty_opt = parse_ty(px).map(Box::new);
            Some(PTy::Ptr(PPtrTy { star, ty_opt }))
        }
        _ => None,
    }
}

/// 型注釈 (`: ty`) のパース
pub(crate) fn parse_ty_ascription(px: &mut Px) -> (Option<TokenData>, Option<PTy>) {
    let colon_opt = px.eat(TokenKind::Colon);

    let ty_opt = if colon_opt.is_some() {
        parse_ty(px)
    } else {
        None
    };

    (colon_opt, ty_opt)
}
