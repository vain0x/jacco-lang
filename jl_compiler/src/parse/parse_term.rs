use super::*;

impl TokenKind {
    fn is_term_first(self) -> bool {
        match self {
            TokenKind::Int | TokenKind::Float | TokenKind::Str | TokenKind::Ident => true,
            _ => false,
        }
    }
}

pub(crate) fn parse_block(px: &mut Px) -> PBlock {
    let left = px.expect(TokenKind::LeftBrace);

    let (body, last_opt) = parse_semi(px);

    let right_opt = px.eat(TokenKind::RightBrace);

    PBlock {
        left,
        right_opt,
        body,
        last_opt,
    }
}

pub(crate) fn parse_term(px: &mut Px) -> PTerm {
    assert!(px.next().is_term_first());

    match px.next() {
        TokenKind::Int => PTerm::Int(px.expect(TokenKind::Int)),
        _ => unreachable!(),
    }
}
