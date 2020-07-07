//! 式の構文解析ルール

use super::*;
use parse_pat::parse_pat;
use parse_ty::parse_mut;
use std::mem::replace;

/// `if cond { ... }` の `cond` に直接、構造体リテラルを書けないようにするためのもの。
/// そうでないと `if name { ... }` の `name { ... }` の部分が構造体リテラルとしてパースされてしまう。
#[derive(Copy, Clone)]
enum AllowStruct {
    True,
    False,
}

enum AllowAssign {
    True,
    False,
}

pub(crate) fn parse_name(px: &mut Px) -> Option<PName> {
    let mut left = px.eat(TokenKind::Ident)?;
    let mut quals = vec![];

    while let TokenKind::ColonColon = px.next() {
        if px.nth(1) != TokenKind::Ident {
            px.skip();
            break;
        }

        let colon_colon = px.bump();
        let right = px.bump();

        let name = replace(&mut left, right);
        quals.push(PNameQual { name, colon_colon });
    }

    Some(PName {
        quals,
        token: left,
        info_opt: None,
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
            TokenKind::Eof | TokenKind::RightBrace => {
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
            _ => px.skip(),
        }
    }

    fields
}

fn parse_record_expr(name: PName, px: &mut Px) -> PExpr {
    let left_brace = match px.eat(TokenKind::LeftBrace) {
        Some(left_brace) => left_brace,
        None => return PExpr::Name(name),
    };

    let fields = parse_field_exprs(px);
    let right_brace_opt = px.eat(TokenKind::RightBrace);
    PExpr::Record(PRecordExpr {
        name,
        left_brace,
        fields,
        right_brace_opt,
    })
}

fn parse_atomic_expr(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let term = match px.next() {
        TokenKind::Int => PExpr::Int(PIntExpr { token: px.bump() }),
        TokenKind::Float => PExpr::Float(PFloatExpr { token: px.bump() }),
        TokenKind::Char => PExpr::Char(PCharExpr { token: px.bump() }),
        TokenKind::Str => PExpr::Str(PStrExpr { token: px.bump() }),
        TokenKind::True => PExpr::True(PTrueExpr(px.bump())),
        TokenKind::False => PExpr::False(PFalseExpr(px.bump())),
        TokenKind::Ident => {
            let name = parse_name(px).unwrap();
            match allow_struct {
                AllowStruct::True => parse_record_expr(name, px),
                AllowStruct::False => PExpr::Name(name),
            }
        }
        TokenKind::LeftParen => PExpr::Tuple(PTupleExpr {
            arg_list: parse_tuple_arg_list(px),
        }),
        _ => return None,
    };
    Some(term)
}

fn parse_suffix_expr(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let mut left = parse_atomic_expr(allow_struct, px)?;

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
                let arg_list = parse_tuple_arg_list(px);

                left = PExpr::Call(PCallExpr {
                    left: Box::new(left),
                    arg_list,
                });
            }
            TokenKind::LeftBracket => {
                let arg_list = parse_array_arg_list(px);

                left = PExpr::Index(PIndexExpr {
                    left: Box::new(left),
                    arg_list,
                });
            }
            _ => return Some(left),
        }
    }
}

fn parse_as_expr(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let mut left = parse_suffix_expr(allow_struct, px)?;

    loop {
        let keyword = match px.next() {
            TokenKind::As => px.bump(),
            _ => return Some(left),
        };

        let ty_opt = parse_ty(px);
        left = PExpr::As(PAsExpr {
            left: Box::new(left),
            keyword,
            ty_opt,
        });
    }
}

fn parse_prefix_expr(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let op = match px.next() {
        TokenKind::And => PUnaryOp::Ref,
        TokenKind::Bang => PUnaryOp::Not,
        TokenKind::Minus => PUnaryOp::Minus,
        TokenKind::Star => PUnaryOp::Deref,
        TokenKind::StarStar => PUnaryOp::DerefDeref,
        _ => return parse_as_expr(allow_struct, px),
    };

    let op_token = px.bump();
    let mut_opt = if op.allow_mut() { parse_mut(px) } else { None };
    let arg_opt = parse_prefix_expr(allow_struct, px).map(Box::new);
    let location = op_token.into_location();
    Some(PExpr::UnaryOp(PUnaryOpExpr {
        op,
        mut_opt,
        arg_opt,
        location,
    }))
}

fn parse_mul(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let mut left = parse_prefix_expr(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::Star => PBinaryOp::Mul,
            TokenKind::Slash => PBinaryOp::Div,
            TokenKind::Percent => PBinaryOp::Modulo,
            _ => return Some(left),
        };

        let op_token = px.bump();
        let right_opt = parse_prefix_expr(allow_struct, px).map(Box::new);
        left = PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        });
    }
}

fn parse_add(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let mut left = parse_mul(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::Plus => PBinaryOp::Add,
            TokenKind::Minus => PBinaryOp::Sub,
            _ => return Some(left),
        };

        let op_token = px.bump();
        let right_opt = parse_mul(allow_struct, px).map(Box::new);
        left = PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        });
    }
}

fn parse_bit(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let mut left = parse_add(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::And => PBinaryOp::BitAnd,
            TokenKind::Pipe => PBinaryOp::BitOr,
            TokenKind::Hat => PBinaryOp::BitXor,
            TokenKind::LeftLeft => PBinaryOp::LeftShift,
            TokenKind::RightRight => PBinaryOp::RightShift,
            _ => return Some(left),
        };

        let op_token = px.bump();
        let right_opt = parse_add(allow_struct, px).map(Box::new);
        left = PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        });
    }
}

fn parse_comparison(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let mut left = parse_bit(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::EqualEqual => PBinaryOp::Eq,
            TokenKind::BangEqual => PBinaryOp::Ne,
            TokenKind::LeftAngle => PBinaryOp::Lt,
            TokenKind::LeftEqual => PBinaryOp::Le,
            TokenKind::RightAngle => PBinaryOp::Gt,
            TokenKind::RightEqual => PBinaryOp::Ge,
            _ => return Some(left),
        };

        let op_token = px.bump();
        let right_opt = parse_bit(allow_struct, px).map(Box::new);
        left = PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        });
    }
}

fn parse_logical(allow_struct: AllowStruct, px: &mut Px) -> Option<PExpr> {
    let mut left = parse_comparison(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::AndAnd => PBinaryOp::LogAnd,
            TokenKind::PipePipe => PBinaryOp::LogOr,
            _ => return Some(left),
        };

        let op_token = px.bump();
        let right_opt = parse_comparison(allow_struct, px).map(Box::new);
        left = PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left: Box::new(left),
            right_opt,
            location: op_token.into_location(),
        });
    }
}

fn parse_pipe_or_assign(
    allow_struct: AllowStruct,
    mut allow_assign: AllowAssign,
    px: &mut Px,
) -> Option<PExpr> {
    let mut left = parse_logical(allow_struct, px)?;

    loop {
        let assign_op = match (px.next(), allow_assign) {
            (TokenKind::Equal, AllowAssign::True) => PBinaryOp::Assign,
            (TokenKind::LeftLeftEqual, AllowAssign::True) => PBinaryOp::LeftShiftAssign,
            (TokenKind::RightRightEqual, AllowAssign::True) => PBinaryOp::RightShiftAssign,
            (TokenKind::AndEqual, AllowAssign::True) => PBinaryOp::BitAndAssign,
            (TokenKind::HatEqual, AllowAssign::True) => PBinaryOp::BitXorAssign,
            (TokenKind::MinusEqual, AllowAssign::True) => PBinaryOp::SubAssign,
            (TokenKind::PercentEqual, AllowAssign::True) => PBinaryOp::ModuloAssign,
            (TokenKind::PipeEqual, AllowAssign::True) => PBinaryOp::BitOrAssign,
            (TokenKind::PlusEqual, AllowAssign::True) => PBinaryOp::AddAssign,
            (TokenKind::SlashEqual, AllowAssign::True) => PBinaryOp::DivAssign,
            (TokenKind::StarEqual, AllowAssign::True) => PBinaryOp::MulAssign,
            (TokenKind::PipeRight, _) => {
                let pipe = px.bump();
                let right_opt = parse_suffix_expr(allow_struct, px).map(Box::new);

                left = PExpr::Pipe(PPipeExpr {
                    left: Box::new(left),
                    pipe,
                    right_opt,
                });
                allow_assign = AllowAssign::False;
                continue;
            }
            _ => return Some(left),
        };

        let op = px.bump();
        let right_opt = parse_pipe_or_assign(allow_struct, AllowAssign::False, px).map(Box::new);

        left = PExpr::BinaryOp(PBinaryOpExpr {
            op: assign_op,
            left: Box::new(left),
            right_opt,
            location: op.into_location(),
        });
        return Some(left);
    }
}

pub(crate) fn parse_cond(px: &mut Px) -> Option<PExpr> {
    // 構造体リテラルは if/match/while の本体を表す {...} と衝突するので、許可しない。
    parse_pipe_or_assign(AllowStruct::False, AllowAssign::False, px)
}

pub(crate) fn parse_block(px: &mut Px) -> Option<PBlock> {
    let left_brace = px.eat(TokenKind::LeftBrace)?;
    let (decls, last_opt) = parse_semi(px);
    let right_brace_opt = px.eat(TokenKind::RightBrace);

    Some(PBlock {
        left_brace,
        right_brace_opt,
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

    PBreakExpr {
        keyword,
        arg_opt,
        loop_id_opt: None,
    }
}

fn parse_continue_expr(px: &mut Px) -> PContinueExpr {
    let keyword = px.expect(TokenKind::Continue);

    PContinueExpr {
        keyword,
        loop_id_opt: None,
    }
}

fn parse_return_expr(px: &mut Px) -> PReturnExpr {
    let keyword = px.expect(TokenKind::Return);
    let arg_opt = parse_expr(px).map(Box::new);

    PReturnExpr {
        keyword,
        arg_opt,
        fn_id_opt: None,
    }
}

fn parse_if_expr(px: &mut Px) -> PIfExpr {
    let keyword = px.expect(TokenKind::If);

    let cond_opt = parse_cond(px).map(Box::new);
    let body_opt = parse_block(px);
    let else_opt = px.eat(TokenKind::Else);

    let alt_opt = if else_opt.is_some() {
        match px.next() {
            TokenKind::LeftBrace => Some(Box::new(PExpr::Block(parse_block_expr(px)))),
            TokenKind::If => Some(Box::new(PExpr::If(parse_if_expr(px)))),
            _ => None,
        }
    } else {
        None
    };

    PIfExpr {
        keyword,
        cond_opt,
        body_opt,
        else_opt,
        alt_opt,
    }
}

fn parse_arm(px: &mut Px) -> Option<PArm> {
    let pat = parse_pat(px)?;
    let arrow_opt = px.eat(TokenKind::RightFatArrow);
    let body_opt = parse_expr(px).map(Box::new);
    let comma_opt = px.eat(TokenKind::Comma);

    Some(PArm {
        pat,
        arrow_opt,
        body_opt,
        comma_opt,
    })
}

fn parse_match_expr(px: &mut Px) -> PMatchExpr {
    let keyword = px.expect(TokenKind::Match);

    let cond_opt = parse_cond(px).map(Box::new);

    let left_brace_opt = px.eat(TokenKind::LeftBrace);
    let mut arms = vec![];
    if left_brace_opt.is_some() {
        loop {
            match px.next() {
                TokenKind::Eof | TokenKind::RightBrace => break,
                _ => {}
            }

            match parse_arm(px) {
                Some(arm) => arms.push(arm),
                None => px.skip(),
            }
        }
    }
    let right_brace_opt = if left_brace_opt.is_some() {
        px.eat(TokenKind::RightBrace)
    } else {
        None
    };

    PMatchExpr {
        keyword,
        cond_opt,
        left_brace_opt,
        arms,
        right_brace_opt,
    }
}

fn parse_while_expr(px: &mut Px) -> PWhileExpr {
    let keyword = px.expect(TokenKind::While);

    let cond_opt = parse_cond(px).map(Box::new);
    let body_opt = parse_block(px);

    PWhileExpr {
        keyword,
        cond_opt,
        body_opt,
        loop_id_opt: None,
    }
}

fn parse_loop_expr(px: &mut Px) -> PLoopExpr {
    let keyword = px.expect(TokenKind::Loop);

    let body_opt = parse_block(px);

    PLoopExpr {
        keyword,
        body_opt,
        loop_id_opt: None,
    }
}

pub(crate) fn parse_expr(px: &mut Px) -> Option<PExpr> {
    let expr = match px.next() {
        TokenKind::Break => PExpr::Break(parse_break_expr(px)),
        TokenKind::Continue => PExpr::Continue(parse_continue_expr(px)),
        TokenKind::Return => PExpr::Return(parse_return_expr(px)),
        TokenKind::If => PExpr::If(parse_if_expr(px)),
        TokenKind::Match => PExpr::Match(parse_match_expr(px)),
        TokenKind::While => PExpr::While(parse_while_expr(px)),
        TokenKind::Loop => PExpr::Loop(parse_loop_expr(px)),
        _ => return parse_pipe_or_assign(AllowStruct::True, AllowAssign::True, px),
    };
    Some(expr)
}

pub(crate) fn parse_args(args: &mut Vec<PArg>, px: &mut Px) {
    loop {
        match px.next() {
            TokenKind::Eof
            | TokenKind::RightParen
            | TokenKind::RightBracket
            | TokenKind::RightBrace
            | TokenKind::Semi => break,
            _ => {}
        }

        let expr = match parse_expr(px) {
            Some(expr) => expr,
            None => {
                px.skip();
                continue;
            }
        };

        let comma_opt = px.eat(TokenKind::Comma);
        let can_continue = comma_opt.is_some();

        args.push(PArg { expr, comma_opt });

        if !can_continue {
            break;
        }
    }
}

fn do_parse_arg_list(left: TokenKind, right: TokenKind, px: &mut Px) -> PArgList {
    let left_paren = px.expect(left);

    let mut args = vec![];
    parse_args(&mut args, px);

    let right_paren_opt = px.eat(right);

    PArgList {
        left_paren,
        args,
        right_paren_opt,
    }
}

fn parse_tuple_arg_list(px: &mut Px) -> PArgList {
    do_parse_arg_list(TokenKind::LeftParen, TokenKind::RightParen, px)
}

fn parse_array_arg_list(px: &mut Px) -> PArgList {
    do_parse_arg_list(TokenKind::LeftBracket, TokenKind::RightBracket, px)
}
