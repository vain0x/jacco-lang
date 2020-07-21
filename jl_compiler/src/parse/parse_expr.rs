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

pub(crate) fn parse_name(px: &mut Px) -> Option<AfterName> {
    let event = px.start_element();
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

    Some(alloc_name(event, quals, left, px))
}

pub(crate) fn parse_unqualified_name(px: &mut Px) -> Option<AfterUnqualifiedName> {
    let event = px.start_element();
    let token = px.eat(TokenKind::Ident)?;
    let quals = vec![];
    let (p_name, _, event) = alloc_name(event, quals, token, px);
    Some((p_name, token, event))
}

fn parse_tuple_expr(event: ExprStart, left_paren: PToken, px: &mut Px) -> AfterExpr {
    match px.next() {
        TokenKind::RightParen => {
            let right_paren = px.bump();
            alloc_unit_expr(event, left_paren, right_paren, px)
        }
        _ => {
            let body_opt = parse_expr(px);
            let right_paren_opt = px.eat(TokenKind::RightParen);
            alloc_group_expr(event, left_paren, body_opt, right_paren_opt, px)
        }
    }
}

fn parse_field_expr(event: ParseStart, name: AfterUnqualifiedName, px: &mut Px) -> AfterFieldExpr {
    let colon_opt = px.eat(TokenKind::Colon);
    let value_opt = parse_expr(px);
    let comma_opt = px.eat(TokenKind::Comma);
    alloc_field_expr(event, name, colon_opt, value_opt, comma_opt, px)
}

fn parse_field_exprs(px: &mut Px) -> Vec<AfterFieldExpr> {
    let mut fields = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace => {
                break;
            }
            TokenKind::Ident => {
                let event = px.start_element();
                let name = parse_unqualified_name(px).unwrap();
                let field = parse_field_expr(event, name, px);
                let can_continue = field.0.comma_opt.is_some();
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

fn parse_record_expr(event: ExprStart, name: AfterName, px: &mut Px) -> AfterExpr {
    let left_brace = match px.eat(TokenKind::LeftBrace) {
        Some(left_brace) => left_brace,
        None => return alloc_name_expr(event, name, px),
    };

    let fields = parse_field_exprs(px);
    let right_brace_opt = px.eat(TokenKind::RightBrace);
    alloc_record_expr(event, name, left_brace, fields, right_brace_opt, px)
}

fn parse_atomic_expr(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
    let event = px.start_element();
    let expr = match px.next() {
        TokenKind::Number => {
            let token = px.bump();
            alloc_number(event, token, px)
        }
        TokenKind::Char => {
            let token = px.bump();
            alloc_char(event, token, px)
        }
        TokenKind::Str => {
            let token = px.bump();
            alloc_str(event, token, px)
        }
        TokenKind::True => {
            let token = px.bump();
            alloc_true(event, token, px)
        }
        TokenKind::False => {
            let token = px.bump();
            alloc_false(event, token, px)
        }
        TokenKind::Ident => {
            let name = parse_name(px).unwrap();
            match allow_struct {
                AllowStruct::True => parse_record_expr(event, name, px),
                AllowStruct::False => alloc_name_expr(event, name, px),
            }
        }
        TokenKind::LeftParen => {
            let left_paren = px.bump();
            parse_tuple_expr(event, left_paren, px)
        }
        _ => return None,
    };
    Some(expr)
}

fn parse_suffix_expr(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
    let mut left = parse_atomic_expr(allow_struct, px)?;

    loop {
        match px.next() {
            TokenKind::Dot => {
                let event = px.start_parent(&left.2);
                let dot = px.bump();
                let name_opt = px.eat(TokenKind::Ident);
                left = alloc_dot_field_expr(event, left, dot, name_opt, px);
            }
            TokenKind::LeftParen => {
                let event = px.start_parent(&left.2);
                let left_paren = px.bump();
                let arg_list = parse_tuple_arg_list(left_paren, px);
                left = alloc_call_expr(event, left, arg_list, px);
            }
            TokenKind::LeftBracket => {
                let event = px.start_parent(&left.2);
                let left_bracket = px.bump();
                let arg_list = parse_array_arg_list(left_bracket, px);
                left = alloc_index_expr(event, left, arg_list, px);
            }
            _ => return Some(left),
        }
    }
}

fn parse_as_expr(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
    let mut left = parse_suffix_expr(allow_struct, px)?;

    loop {
        let event = px.start_parent(&left.2);
        let keyword = match px.next() {
            TokenKind::As => px.bump(),
            _ => return Some(left),
        };

        let ty_opt = parse_ty(px);
        left = alloc_as_expr(event, left, keyword, ty_opt, px);
    }
}

fn parse_prefix_expr(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
    let event = px.start_element();
    let unary_op = match px.next() {
        TokenKind::And => PUnaryOp::Ref,
        TokenKind::Bang => PUnaryOp::Not,
        TokenKind::Minus => PUnaryOp::Minus,
        TokenKind::Star => PUnaryOp::Deref,
        TokenKind::StarStar => PUnaryOp::DerefDeref,
        _ => return parse_as_expr(allow_struct, px),
    };

    let token = px.bump();
    let mut_opt = if unary_op.allow_mut() {
        parse_mut(px)
    } else {
        None
    };
    let arg_opt = parse_prefix_expr(allow_struct, px);
    Some(alloc_prefix_expr(
        event, unary_op, token, mut_opt, arg_opt, px,
    ))
}

fn parse_mul(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
    let mut left = parse_prefix_expr(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::Star => PBinaryOp::Mul,
            TokenKind::Slash => PBinaryOp::Div,
            TokenKind::Percent => PBinaryOp::Modulo,
            _ => return Some(left),
        };

        let event = px.start_parent(&left.2);
        let op_token = px.bump();
        let right_opt = parse_prefix_expr(allow_struct, px);
        left = alloc_binary_op_expr(event, op, left, op_token, right_opt, px);
    }
}

fn parse_add(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
    let mut left = parse_mul(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::Plus => PBinaryOp::Add,
            TokenKind::Minus => PBinaryOp::Sub,
            _ => return Some(left),
        };

        let event = px.start_parent(&left.2);
        let op_token = px.bump();
        let right_opt = parse_mul(allow_struct, px);
        left = alloc_binary_op_expr(event, op, left, op_token, right_opt, px);
    }
}

fn parse_bit(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
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

        let event = px.start_parent(&left.2);
        let op_token = px.bump();
        let right_opt = parse_add(allow_struct, px);
        left = alloc_binary_op_expr(event, op, left, op_token, right_opt, px);
    }
}

fn parse_comparison(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
    let mut left = parse_bit(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::EqualEqual => PBinaryOp::Equal,
            TokenKind::BangEqual => PBinaryOp::NotEqual,
            TokenKind::LeftAngle => PBinaryOp::LessThan,
            TokenKind::LeftEqual => PBinaryOp::LessEqual,
            TokenKind::RightAngle => PBinaryOp::GreaterThan,
            TokenKind::RightEqual => PBinaryOp::GreaterEqual,
            _ => return Some(left),
        };

        let event = px.start_parent(&left.2);
        let op_token = px.bump();
        let right_opt = parse_bit(allow_struct, px);
        left = alloc_binary_op_expr(event, op, left, op_token, right_opt, px);
    }
}

fn parse_logical(allow_struct: AllowStruct, px: &mut Px) -> Option<AfterExpr> {
    let mut left = parse_comparison(allow_struct, px)?;

    loop {
        let op = match px.next() {
            TokenKind::AndAnd => PBinaryOp::LogAnd,
            TokenKind::PipePipe => PBinaryOp::LogOr,
            _ => return Some(left),
        };

        let event = px.start_parent(&left.2);
        let op_token = px.bump();
        let right_opt = parse_comparison(allow_struct, px);
        left = alloc_binary_op_expr(event, op, left, op_token, right_opt, px);
    }
}

fn parse_pipe_or_assign(
    allow_struct: AllowStruct,
    mut allow_assign: AllowAssign,
    px: &mut Px,
) -> Option<AfterExpr> {
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
                let event = px.start_parent(&left.2);
                let pipe = px.bump();
                let right_opt = parse_suffix_expr(allow_struct, px);

                left = alloc_pipe_expr(event, left, pipe, right_opt, px);
                allow_assign = AllowAssign::False;
                continue;
            }
            _ => return Some(left),
        };

        let event = px.start_parent(&left.2);
        let op_token = px.bump();
        let right_opt = parse_pipe_or_assign(allow_struct, AllowAssign::False, px);

        left = alloc_binary_op_expr(event, assign_op, left, op_token, right_opt, px);
        return Some(left);
    }
}

pub(crate) fn parse_cond(px: &mut Px) -> Option<AfterExpr> {
    // 構造体リテラルは match や while の本体を表す {...} と衝突するので、許可しない。
    parse_pipe_or_assign(AllowStruct::False, AllowAssign::False, px)
}

pub(crate) fn parse_block(px: &mut Px) -> Option<AfterBlock> {
    let event = px.start_element();
    let left_brace = px.eat(TokenKind::LeftBrace)?;
    let semi = parse_semi(px);
    let right_brace_opt = px.eat(TokenKind::RightBrace);

    Some(alloc_block(event, left_brace, semi, right_brace_opt, px))
}

fn parse_block_expr(event: ExprStart, left_brace: PToken, px: &mut Px) -> AfterExpr {
    let semi = parse_semi(px);
    let right_brace_opt = px.eat(TokenKind::RightBrace);
    alloc_block_expr(event, left_brace, semi, right_brace_opt, px)
}

fn parse_break_expr(event: ExprStart, keyword: PToken, px: &mut Px) -> AfterExpr {
    let arg_opt = parse_expr(px);
    alloc_break_expr(event, keyword, arg_opt, px)
}

fn parse_return_expr(event: ExprStart, keyword: PToken, px: &mut Px) -> AfterExpr {
    let arg_opt = parse_expr(px);
    alloc_return_expr(event, keyword, arg_opt, px)
}

fn parse_if_expr(event: ExprStart, keyword: PToken, px: &mut Px) -> AfterExpr {
    let cond_opt = parse_cond(px);
    let body_opt = parse_block(px);
    let else_opt = px.eat(TokenKind::Else);

    let alt_opt = if else_opt.is_some() {
        let alt_event = px.start_element();
        match px.next() {
            TokenKind::LeftBrace => {
                let left_brace = px.bump();
                Some(parse_block_expr(alt_event, left_brace, px))
            }
            TokenKind::If => {
                let keyword = px.bump();
                Some(parse_if_expr(alt_event, keyword, px))
            }
            _ => None,
        }
    } else {
        None
    };

    alloc_if_expr(event, keyword, cond_opt, body_opt, else_opt, alt_opt, px)
}

fn parse_arm(px: &mut Px) -> Option<AfterArm> {
    let event = px.start_element();
    let pat = parse_pat(px)?;
    let arrow_opt = px.eat(TokenKind::RightFatArrow);
    let body_opt = parse_expr(px);
    let comma_opt = px.eat(TokenKind::Comma);

    Some(alloc_arm(event, pat, arrow_opt, body_opt, comma_opt, px))
}

fn parse_match_expr(event: ExprStart, keyword: PToken, px: &mut Px) -> AfterExpr {
    let cond_opt = parse_cond(px);

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

    alloc_match_expr(
        event,
        keyword,
        cond_opt,
        left_brace_opt,
        arms,
        right_brace_opt,
        px,
    )
}

fn parse_while_expr(event: ExprStart, keyword: PToken, px: &mut Px) -> AfterExpr {
    let cond_opt = parse_cond(px);
    let body_opt = parse_block(px);

    alloc_while_expr(event, keyword, cond_opt, body_opt, px)
}

fn parse_loop_expr(event: ExprStart, keyword: PToken, px: &mut Px) -> AfterExpr {
    let body_opt = parse_block(px);

    alloc_loop_expr(event, keyword, body_opt, px)
}

pub(crate) fn parse_expr(px: &mut Px) -> Option<AfterExpr> {
    let event = px.start_element();
    let expr = match px.next() {
        TokenKind::LeftBrace => {
            let left_brace = px.bump();
            parse_block_expr(event, left_brace, px)
        }
        TokenKind::Break => {
            let keyword = px.bump();
            parse_break_expr(event, keyword, px)
        }
        TokenKind::Continue => {
            let keyword = px.bump();
            alloc_continue_expr(event, keyword, px)
        }
        TokenKind::Return => {
            let keyword = px.bump();
            parse_return_expr(event, keyword, px)
        }
        TokenKind::If => {
            let keyword = px.bump();
            parse_if_expr(event, keyword, px)
        }
        TokenKind::Match => {
            let keyword = px.bump();
            parse_match_expr(event, keyword, px)
        }
        TokenKind::While => {
            let keyword = px.bump();
            parse_while_expr(event, keyword, px)
        }
        TokenKind::Loop => {
            let keyword = px.bump();
            parse_loop_expr(event, keyword, px)
        }
        _ => return parse_pipe_or_assign(AllowStruct::True, AllowAssign::True, px),
    };
    Some(expr)
}

pub(crate) fn parse_args(args: &mut Vec<AfterArg>, px: &mut Px) {
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

        args.push(alloc_arg(expr, comma_opt, px));

        if !can_continue {
            break;
        }
    }
}

fn do_parse_arg_list(left_paren: PToken, right: TokenKind, px: &mut Px) -> AfterArgList {
    let mut args = vec![];
    parse_args(&mut args, px);

    let right_paren_opt = px.eat(right);
    alloc_arg_list(left_paren, args, right_paren_opt, px)
}

fn parse_tuple_arg_list(left_paren: PToken, px: &mut Px) -> AfterArgList {
    do_parse_arg_list(left_paren, TokenKind::RightParen, px)
}

fn parse_array_arg_list(left_bracket: PToken, px: &mut Px) -> AfterArgList {
    do_parse_arg_list(left_bracket, TokenKind::RightBracket, px)
}
