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

pub(crate) fn eat_block(px: &mut Px) -> Option<PBlock> {
    if px.next() == TokenKind::LeftBrace {
        Some(parse_block(px))
    } else {
        None
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

fn parse_args(args: &mut Vec<PArg>, px: &mut Px) {
    loop {
        match px.next() {
            TokenKind::Eof
            | TokenKind::RightParen
            | TokenKind::RightBracket
            | TokenKind::RightBrace => break,
            token if token.is_term_first() => {
                let term = parse_term(px);
                let comma_opt = px.eat(TokenKind::Comma);
                let can_continue = comma_opt.is_some();

                args.push(PArg { term, comma_opt });

                if !can_continue {
                    break;
                }
            }
            _ => {
                // error
                px.bump();
            }
        }
    }
}

fn parse_suffix(px: &mut Px) -> PTerm {
    let mut left = parse_atom(px);

    loop {
        match px.next() {
            TokenKind::LeftParen => {
                let left_paren = px.bump();

                let mut args = vec![];
                parse_args(&mut args, px);

                let right_opt = px.eat(TokenKind::RightParen);
                let arg_list = PArgList {
                    left: left_paren,
                    args,
                    right_opt,
                };
                left = PTerm::Call {
                    callee: Box::new(left),
                    arg_list,
                };
            }
            _ => return left,
        }
    }
}

fn parse_mul(px: &mut Px) -> PTerm {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right = parse_suffix(px);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location: op_token.into_location(),
        }
    };

    let mut left = parse_suffix(px);

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

fn parse_bit(px: &mut Px) -> PTerm {
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
            TokenKind::And => left = parse_right(BinaryOp::BitAnd, left, px),
            TokenKind::Pipe => left = parse_right(BinaryOp::BitOr, left, px),
            TokenKind::Hat => left = parse_right(BinaryOp::BitXor, left, px),
            TokenKind::LeftShift => left = parse_right(BinaryOp::LeftShift, left, px),
            TokenKind::RightShift => left = parse_right(BinaryOp::RightShift, left, px),
            _ => return left,
        }
    }
}

fn parse_cmp(px: &mut Px) -> PTerm {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right = parse_bit(px);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location: op_token.into_location(),
        }
    };

    let mut left = parse_bit(px);

    loop {
        match px.next() {
            TokenKind::EqualEqual => left = parse_right(BinaryOp::Eq, left, px),
            TokenKind::BangEqual => left = parse_right(BinaryOp::Ne, left, px),
            TokenKind::LeftAngle => left = parse_right(BinaryOp::Lt, left, px),
            TokenKind::LeftAngleEqual => left = parse_right(BinaryOp::Le, left, px),
            TokenKind::RightAngle => left = parse_right(BinaryOp::Gt, left, px),
            TokenKind::RightAngleEqual => left = parse_right(BinaryOp::Ge, left, px),
            _ => return left,
        }
    }
}

fn parse_log(px: &mut Px) -> PTerm {
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

    let mut left = parse_cmp(px);

    loop {
        match px.next() {
            TokenKind::AndAnd => left = parse_right(BinaryOp::LogAnd, left, px),
            TokenKind::PipePipe => left = parse_right(BinaryOp::LogOr, left, px),
            _ => return left,
        }
    }
}

fn parse_assign(px: &mut Px) -> PTerm {
    parse_log(px)
}

pub(crate) fn parse_term(px: &mut Px) -> PTerm {
    assert!(px.next().is_term_first());

    parse_assign(px)
}

pub(crate) fn eat_term(px: &mut Px) -> Option<PTerm> {
    if px.next().is_term_first() {
        Some(parse_term(px))
    } else {
        None
    }
}
