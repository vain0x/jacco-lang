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

pub(crate) fn parse_atom(px: &mut Px) -> PTerm {
    match px.next() {
        TokenKind::Int => PTerm::Int(px.bump()),
        TokenKind::Str => PTerm::Str(px.bump()),
        TokenKind::Ident => PTerm::Name(px.bump()),
        _ => unimplemented!(),
    }
}

pub(crate) fn parse_add(px: &mut Px) -> PTerm {
    let mut left = parse_atom(px);

    loop {
        match px.next() {
            TokenKind::Plus => {
                let op = px.bump();

                let right = parse_atom(px);
                left = PTerm::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(left),
                    right: Box::new(right),
                    location: op.into_location(),
                };
            }
            _ => return left,
        }
    }
}

pub(crate) fn parse_term(px: &mut Px) -> PTerm {
    assert!(px.next().is_term_first());

    parse_add(px)
}
