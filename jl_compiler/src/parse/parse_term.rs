use super::*;
use crate::NO_ID;

pub(crate) fn parse_name(px: &mut Px) -> Option<PName> {
    let (_, text, location) = px.eat(TokenKind::Ident)?.decompose();

    Some(PName {
        name_id: NO_ID,
        text,
        location,
    })
}

fn parse_atom(px: &mut Px) -> Option<PTerm> {
    let term = match px.next() {
        TokenKind::Int => PTerm::Int(px.bump()),
        TokenKind::Str => PTerm::Str(px.bump()),
        TokenKind::Ident => PTerm::Name(parse_name(px).unwrap()),
        _ => return None,
    };
    Some(term)
}

fn parse_suffix(px: &mut Px) -> Option<PTerm> {
    let mut left = parse_atom(px)?;

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
            _ => return Some(left),
        }
    }
}

fn parse_mul(px: &mut Px) -> Option<PTerm> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_suffix(px).map(Box::new);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        }
    };

    let mut left = parse_suffix(px)?;

    loop {
        match px.next() {
            TokenKind::Star => left = parse_right(BinaryOp::Mul, left, px),
            TokenKind::Slash => left = parse_right(BinaryOp::Div, left, px),
            TokenKind::Percent => left = parse_right(BinaryOp::Mod, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_add(px: &mut Px) -> Option<PTerm> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_mul(px).map(Box::new);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        }
    };

    let mut left = parse_mul(px)?;

    loop {
        match px.next() {
            TokenKind::Plus => left = parse_right(BinaryOp::Add, left, px),
            TokenKind::Minus => left = parse_right(BinaryOp::Sub, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_bit(px: &mut Px) -> Option<PTerm> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_add(px).map(Box::new);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        }
    };

    let mut left = parse_add(px)?;

    loop {
        match px.next() {
            TokenKind::And => left = parse_right(BinaryOp::BitAnd, left, px),
            TokenKind::Pipe => left = parse_right(BinaryOp::BitOr, left, px),
            TokenKind::Hat => left = parse_right(BinaryOp::BitXor, left, px),
            TokenKind::LeftShift => left = parse_right(BinaryOp::LeftShift, left, px),
            TokenKind::RightShift => left = parse_right(BinaryOp::RightShift, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_cmp(px: &mut Px) -> Option<PTerm> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_bit(px).map(Box::new);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        }
    };

    let mut left = parse_bit(px)?;

    loop {
        match px.next() {
            TokenKind::EqualEqual => left = parse_right(BinaryOp::Eq, left, px),
            TokenKind::BangEqual => left = parse_right(BinaryOp::Ne, left, px),
            TokenKind::LeftAngle => left = parse_right(BinaryOp::Lt, left, px),
            TokenKind::LeftAngleEqual => left = parse_right(BinaryOp::Le, left, px),
            TokenKind::RightAngle => left = parse_right(BinaryOp::Gt, left, px),
            TokenKind::RightAngleEqual => left = parse_right(BinaryOp::Ge, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_log(px: &mut Px) -> Option<PTerm> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_cmp(px).map(Box::new);
        PTerm::BinaryOp {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        }
    };

    let mut left = parse_cmp(px)?;

    loop {
        match px.next() {
            TokenKind::AndAnd => left = parse_right(BinaryOp::LogAnd, left, px),
            TokenKind::PipePipe => left = parse_right(BinaryOp::LogOr, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_assign(px: &mut Px) -> Option<PTerm> {
    parse_log(px)
}

pub(crate) fn parse_term(px: &mut Px) -> Option<PTerm> {
    parse_assign(px)
}

pub(crate) fn parse_cond(px: &mut Px) -> Option<PTerm> {
    parse_term(px)
}
