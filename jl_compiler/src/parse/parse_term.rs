use super::*;

impl TokenKind {
    pub(crate) fn is_term_first(self) -> bool {
        match self {
            TokenKind::Int | TokenKind::Float | TokenKind::Str | TokenKind::Ident => true,
            _ => false,
        }
    }
}

pub(crate) fn parse_name(px: &mut Px) -> PName {
    let (_, text, location) = px.expect(TokenKind::Ident).decompose();

    PName {
        name_id: px.fresh_id(),
        text,
        location,
    }
}

pub(crate) fn eat_name(px: &mut Px) -> Option<PName> {
    if px.next() == TokenKind::Ident {
        Some(parse_name(px))
    } else {
        None
    }
}

pub(crate) fn parse_block(px: &mut Px) -> PBlock {
    let left = px.expect(TokenKind::LeftBrace);

    let (body, last_opt) = parse_semi(Placement::Local, px);

    let right_opt = px.eat(TokenKind::RightBrace);

    PBlock {
        left,
        right_opt,
        body,
        last_opt,
    }
}

fn parse_atom(px: &mut Px) -> PTerm {
    match px.next() {
        TokenKind::Int => PTerm::Int(px.bump()),
        TokenKind::Str => PTerm::Str(px.bump()),
        TokenKind::Ident => PTerm::Name(parse_name(px)),
        _ => unimplemented!(),
    }
}

fn parse_mul(px: &mut Px) -> PTerm {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right = parse_atom(px);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location: op_token.into_location(),
        }
    };

    let mut left = parse_atom(px);

    loop {
        match px.next() {
            TokenKind::Star => left = parse_right(BinaryOp::Mul, left, px),
            TokenKind::Slash => left = parse_right(BinaryOp::Div, left, px),
            TokenKind::Percent => left = parse_right(BinaryOp::Mod, left, px),
            _ => return left,
        }
    }
}

fn parse_add(px: &mut Px) -> PTerm {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right = parse_mul(px);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location: op_token.into_location(),
        }
    };

    let mut left = parse_mul(px);

    loop {
        match px.next() {
            TokenKind::Plus => left = parse_right(BinaryOp::Add, left, px),
            TokenKind::Minus => left = parse_right(BinaryOp::Sub, left, px),
            _ => return left,
        }
    }
}

fn parse_cmp(px: &mut Px) -> PTerm {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right = parse_add(px);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location: op_token.into_location(),
        }
    };

    let mut left = parse_add(px);

    loop {
        match px.next() {
            TokenKind::Plus => left = parse_right(BinaryOp::Add, left, px),
            TokenKind::Minus => left = parse_right(BinaryOp::Sub, left, px),
            _ => return left,
        }
    }
}

fn parse_assign(px: &mut Px) -> PTerm {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right = parse_cmp(px);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location: op_token.into_location(),
        }
    };

    let left = parse_cmp(px);

    match px.next() {
        TokenKind::EqualEqual => parse_right(BinaryOp::Eq, left, px),
        TokenKind::BangEqual => parse_right(BinaryOp::Ne, left, px),
        TokenKind::LeftAngle => parse_right(BinaryOp::Lt, left, px),
        TokenKind::LeftAngleEqual => parse_right(BinaryOp::Le, left, px),
        TokenKind::RightAngle => parse_right(BinaryOp::Gt, left, px),
        TokenKind::RightAngleEqual => parse_right(BinaryOp::Ge, left, px),
        _ => left,
    }
}

pub(crate) fn parse_term(px: &mut Px) -> PTerm {
    assert!(px.next().is_term_first());

    parse_assign(px)
}
