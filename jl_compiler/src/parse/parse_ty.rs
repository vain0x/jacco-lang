use super::*;

pub(crate) fn parse_ty(px: &mut Px) -> Option<PTy> {
    match px.next() {
        TokenKind::Ident => {
            let name = parse_name(px).unwrap();
            Some(PTy::Name(name))
        }
        TokenKind::LeftParen => {
            let left = px.bump();
            let right_opt = px.eat(TokenKind::RightParen);
            Some(PTy::Unit { left, right_opt })
        }
        TokenKind::Bang => {
            let bang = px.bump();
            Some(PTy::Never { bang })
        }
        TokenKind::Star => {
            let star = px.bump();
            let ty_opt = parse_ty(px).map(Box::new);
            Some(PTy::Ptr { star, ty_opt })
        }
        _ => None,
    }
}
