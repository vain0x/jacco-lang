//! 式の構文解析ルール

use super::*;
use crate::NO_ID;

pub(crate) fn parse_name(px: &mut Px) -> Option<PName> {
    let token = px.eat(TokenKind::Ident)?;

    Some(PName {
        name_id: NO_ID,
        token,
    })
}

fn parse_field_expr(px: &mut Px) -> PFieldExpr {
    let name = parse_name(px).unwrap();

    let colon_opt = px.eat(TokenKind::Colon);
    let value_opt = parse_expr(px);

    let comma_opt = px.eat(TokenKind::Comma);

    PFieldExpr {
        name,
        colon_opt,
        value_opt,
        comma_opt,
    }
}

fn parse_field_exprs(px: &mut Px) -> Vec<PFieldExpr> {
    let mut fields = vec![];

    loop {
        match px.next() {
            TokenKind::Eof
            | TokenKind::Semi
            | TokenKind::RightParen
            | TokenKind::RightBracket
            | TokenKind::RightBrace => {
                break;
            }
            TokenKind::Ident => {
                let field = parse_field_expr(px);
                let can_continue = field.comma_opt.is_some();
                fields.push(field);

                if !can_continue {
                    break;
                }
            }
            _ => {
                // FIXME: recovery
                p_error("expected a field (`name: expr,`)", px);
                px.bump();
                continue;
            }
        }
    }

    fields
}

fn parse_struct_expr(px: &mut Px) -> PExpr {
    assert_eq!(px.next(), TokenKind::Ident);

    let name = parse_name(px).unwrap();

    if let Some(left_brace) = px.eat(TokenKind::LeftBrace) {
        let fields = parse_field_exprs(px);
        let right_brace_opt = px.eat(TokenKind::RightBrace);
        return PExpr::Struct(PStructExpr {
            name: PNameTy(name),
            left_brace,
            fields,
            right_brace_opt,
        });
    }

    PExpr::Name(PNameExpr(name))
}

fn parse_atomic_expr(px: &mut Px) -> Option<PExpr> {
    let term = match px.next() {
        TokenKind::Int => PExpr::Int(PIntExpr { token: px.bump() }),
        TokenKind::Str => PExpr::Str(PStrExpr { token: px.bump() }),
        TokenKind::Ident => parse_struct_expr(px),
        TokenKind::LeftParen => PExpr::Tuple(PTupleExpr {
            arg_list: parse_arg_list(px),
        }),
        _ => return None,
    };
    Some(term)
}

fn parse_suffix_expr(px: &mut Px) -> Option<PExpr> {
    let mut left = parse_atomic_expr(px)?;

    loop {
        match px.next() {
            TokenKind::Dot => {
                let dot = px.bump();
                let name_opt = px.eat(TokenKind::Ident);

                left = PExpr::DotField(PDotFieldExpr {
                    left: Box::new(left),
                    dot,
                    name_opt,
                });
            }
            TokenKind::LeftParen => {
                let arg_list = parse_arg_list(px);

                left = PExpr::Call(PCallExpr {
                    callee: Box::new(left),
                    arg_list,
                });
            }
            _ => return Some(left),
        }
    }
}

fn parse_prefix_expr(px: &mut Px) -> Option<PExpr> {
    let parse_right = |op, px: &mut Px| {
        let op_token = px.bump();
        let arg_opt = parse_prefix_expr(px).map(Box::new);
        let location = op_token.into_location();
        Some(PExpr::UnaryOp(PUnaryOpExpr {
            op,
            arg_opt,
            location,
        }))
    };

    match px.next() {
        TokenKind::And => parse_right(PUnaryOp::Ref, px),
        TokenKind::Bang => parse_right(PUnaryOp::Negate, px),
        TokenKind::Minus => parse_right(PUnaryOp::Minus, px),
        TokenKind::Star => parse_right(PUnaryOp::Deref, px),
        _ => parse_suffix_expr(px),
    }
}

fn parse_mul(px: &mut Px) -> Option<PExpr> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_prefix_expr(px).map(Box::new);
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        })
    };

    let mut left = parse_prefix_expr(px)?;

    loop {
        match px.next() {
            TokenKind::Star => left = parse_right(PBinaryOp::Mul, left, px),
            TokenKind::Slash => left = parse_right(PBinaryOp::Div, left, px),
            TokenKind::Percent => left = parse_right(PBinaryOp::Mod, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_add(px: &mut Px) -> Option<PExpr> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_mul(px).map(Box::new);
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        })
    };

    let mut left = parse_mul(px)?;

    loop {
        match px.next() {
            TokenKind::Plus => left = parse_right(PBinaryOp::Add, left, px),
            TokenKind::Minus => left = parse_right(PBinaryOp::Sub, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_bit(px: &mut Px) -> Option<PExpr> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_add(px).map(Box::new);
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        })
    };

    let mut left = parse_add(px)?;

    loop {
        match px.next() {
            TokenKind::And => left = parse_right(PBinaryOp::BitAnd, left, px),
            TokenKind::Pipe => left = parse_right(PBinaryOp::BitOr, left, px),
            TokenKind::Hat => left = parse_right(PBinaryOp::BitXor, left, px),
            TokenKind::LeftShift => left = parse_right(PBinaryOp::LeftShift, left, px),
            TokenKind::RightShift => left = parse_right(PBinaryOp::RightShift, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_comparison(px: &mut Px) -> Option<PExpr> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_bit(px).map(Box::new);
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        })
    };

    let mut left = parse_bit(px)?;

    loop {
        match px.next() {
            TokenKind::EqualEqual => left = parse_right(PBinaryOp::Eq, left, px),
            TokenKind::BangEqual => left = parse_right(PBinaryOp::Ne, left, px),
            TokenKind::LeftAngle => left = parse_right(PBinaryOp::Lt, left, px),
            TokenKind::LeftAngleEqual => left = parse_right(PBinaryOp::Le, left, px),
            TokenKind::RightAngle => left = parse_right(PBinaryOp::Gt, left, px),
            TokenKind::RightAngleEqual => left = parse_right(PBinaryOp::Ge, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_logical(px: &mut Px) -> Option<PExpr> {
    let parse_right = |op, left, px: &mut Px| {
        let op_token = px.bump();
        let right_opt = parse_comparison(px).map(Box::new);
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        })
    };

    let mut left = parse_comparison(px)?;

    loop {
        match px.next() {
            TokenKind::AndAnd => left = parse_right(PBinaryOp::LogAnd, left, px),
            TokenKind::PipePipe => left = parse_right(PBinaryOp::LogOr, left, px),
            _ => return Some(left),
        }
    }
}

fn parse_assign(px: &mut Px) -> Option<PExpr> {
    let mut left = parse_logical(px)?;

    loop {
        let binary_op = match px.next() {
            TokenKind::Equal => PBinaryOp::Assign,
            _ => return Some(left),
        };

        let op = px.bump();
        let right_opt = parse_logical(px).map(Box::new);

        left = PExpr::BinaryOp(PBinaryOpExpr {
            op: binary_op,
            left: Box::new(left),
            right_opt,
            location: op.into_location(),
        });
    }
}

pub(crate) fn parse_cond(
    px: &mut Px,
) -> (Option<TokenData>, Option<Box<PExpr>>, Option<TokenData>) {
    let left_paren_opt = px.eat(TokenKind::LeftParen);

    let expr_opt = parse_logical(px).map(Box::new);

    let right_paren_opt = if left_paren_opt.is_some() {
        px.eat(TokenKind::RightParen)
    } else {
        None
    };

    (left_paren_opt, expr_opt, right_paren_opt)
}

pub(crate) fn parse_block(px: &mut Px) -> Option<PBlock> {
    let left = px.eat(TokenKind::LeftBrace)?;

    let (decls, last_opt) = parse_semi(Placement::Local, px);

    let right_opt = px.eat(TokenKind::RightBrace);

    Some(PBlock {
        left,
        right_opt,
        decls,
        last_opt: last_opt.map(Box::new),
    })
}

fn parse_block_expr(px: &mut Px) -> PBlockExpr {
    assert_eq!(px.next(), TokenKind::LeftBrace);

    PBlockExpr(parse_block(px).unwrap())
}

fn parse_break_expr(px: &mut Px) -> PBreakExpr {
    let keyword = px.expect(TokenKind::Break);
    let arg_opt = parse_expr(px).map(Box::new);

    PBreakExpr { keyword, arg_opt }
}

fn parse_continue_expr(px: &mut Px) -> PContinueExpr {
    let keyword = px.expect(TokenKind::Continue);

    PContinueExpr { keyword }
}

fn parse_return_expr(px: &mut Px) -> PReturnExpr {
    let keyword = px.expect(TokenKind::Return);
    let arg_opt = parse_expr(px).map(Box::new);

    PReturnExpr { keyword, arg_opt }
}

fn parse_if_expr(px: &mut Px) -> PIfExpr {
    let keyword = px.expect(TokenKind::If);

    let (left_paren_opt, cond_opt, right_paren_opt) = parse_cond(px);
    let body_opt = parse_block(px);
    let else_opt = px.eat(TokenKind::Else);

    let alt_opt = if else_opt.is_some() {
        match px.next() {
            TokenKind::LeftBrace => Some(Box::new(PExpr::Block(parse_block_expr(px)))),
            TokenKind::If => Some(Box::new(PExpr::If(parse_if_expr(px)))),
            _ => {
                p_error("expected 'if' or '{ ... }'", px);
                None
            }
        }
    } else {
        None
    };

    PIfExpr {
        keyword,
        left_paren_opt,
        cond_opt,
        right_paren_opt,
        body_opt,
        else_opt,
        alt_opt,
    }
}

fn parse_while_expr(px: &mut Px) -> PWhileExpr {
    let keyword = px.expect(TokenKind::While);

    let (left_paren_opt, cond_opt, right_paren_opt) = parse_cond(px);
    let body_opt = parse_block(px);

    PWhileExpr {
        keyword,
        left_paren_opt,
        cond_opt,
        right_paren_opt,
        body_opt,
    }
}

fn parse_loop_expr(px: &mut Px) -> PLoopExpr {
    let keyword = px.expect(TokenKind::Loop);

    let body_opt = parse_block(px);

    PLoopExpr { keyword, body_opt }
}

pub(crate) fn parse_expr(px: &mut Px) -> Option<PExpr> {
    let expr = match px.next() {
        TokenKind::Break => PExpr::Break(parse_break_expr(px)),
        TokenKind::Continue => PExpr::Continue(parse_continue_expr(px)),
        TokenKind::Return => PExpr::Return(parse_return_expr(px)),
        TokenKind::If => PExpr::If(parse_if_expr(px)),
        TokenKind::While => PExpr::While(parse_while_expr(px)),
        TokenKind::Loop => PExpr::Loop(parse_loop_expr(px)),
        _ => return parse_assign(px),
    };
    Some(expr)
}

pub(crate) fn parse_args(args: &mut Vec<PArg>, px: &mut Px) {
    loop {
        match px.next() {
            TokenKind::Eof
            | TokenKind::RightParen
            | TokenKind::RightBracket
            | TokenKind::RightBrace => break,
            _ => {
                let expr = match parse_expr(px) {
                    None => {
                        p_error("expected argument", px);
                        px.bump();
                        continue;
                    }
                    Some(expr) => expr,
                };

                let comma_opt = px.eat(TokenKind::Comma);
                let can_continue = comma_opt.is_some();

                args.push(PArg { expr, comma_opt });

                if !can_continue {
                    break;
                }
            }
        }
    }
}

fn parse_arg_list(px: &mut Px) -> PArgList {
    let left = px.expect(TokenKind::LeftParen);

    let mut args = vec![];
    parse_args(&mut args, px);

    let right_opt = px.eat(TokenKind::RightParen);

    PArgList {
        left,
        args,
        right_opt,
    }
}
