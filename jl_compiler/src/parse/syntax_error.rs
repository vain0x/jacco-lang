//! 構文エラーについて書く場所

use super::*;

fn validate_paren_matching(left: PToken, right_opt: Option<PToken>, px: &Px) {
    if right_opt.is_none() {
        px.logger()
            .error(PLoc::Token(left), "丸カッコが閉じられていません");
    }
}

fn validate_brace_matching(left: PToken, right_opt: Option<PToken>, px: &Px) {
    if right_opt.is_none() {
        px.logger()
            .error(PLoc::Token(left), "波カッコが閉じられていません");
    }
}

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) fn validate_ptr_ty(
    star: PToken,
    _mut_opt: Option<PMut>,
    ty_opt: Option<&AfterTy>,
    px: &Px,
) {
    if ty_opt.is_none() {
        px.logger().error(
            PLoc::TokenBehind(star),
            "ポインタ型の * の後ろに型が必要です。",
        );
    }
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

pub(crate) fn validate_record_pat(
    _name: &AfterQualifiableName,
    left_brace: PToken,
    right_brace_opt: Option<PToken>,
    px: &Px,
) {
    validate_brace_matching(left_brace, right_brace_opt, px);
}

// -----------------------------------------------
// 式
// -----------------------------------------------

pub(crate) fn validate_paren_expr(left_paren: PToken, right_paren_opt: Option<PToken>, px: &Px) {
    validate_paren_matching(left_paren, right_paren_opt, px);
}

pub(crate) fn validate_record_expr(left_brace: PToken, right_brace_opt: Option<PToken>, px: &Px) {
    validate_brace_matching(left_brace, right_brace_opt, px);
}

pub(crate) fn validate_field_expr(
    _left: &AfterExpr,
    dot: PToken,
    name_opt: Option<PToken>,
    px: &Px,
) {
    if name_opt.is_none() {
        px.logger()
            .error(PLoc::TokenBehind(dot), ". の後ろにフィールド名が必要です。");
    }
}

pub(crate) fn validate_as_expr(
    _left: &AfterExpr,
    keyword: PToken,
    ty_opt: Option<&AfterTy>,
    px: &Px,
) {
    if ty_opt.is_none() {
        px.logger()
            .error(PLoc::TokenBehind(keyword), "as の後ろに型が必要です。");
    }
}

pub(crate) fn validate_prefix_expr(
    token: PToken,
    mut_opt: Option<PMut>,
    arg_opt: Option<&AfterExpr>,
    px: &Px,
) {
    if arg_opt.is_none() {
        let last = match mut_opt {
            Some((_, token)) => token,
            None => token,
        };

        px.logger()
            .error(PLoc::TokenBehind(last), "前置演算子の後ろに式が必要です。");
    }
}

pub(crate) fn validate_binary_op_expr(token: PToken, right_opt: Option<&AfterExpr>, px: &Px) {
    if right_opt.is_none() {
        px.logger()
            .error(PLoc::TokenBehind(token), "二項演算子の後ろに式が必要です。");
    }
}

pub(crate) fn validate_block_expr(left_brace: PToken, right_brace_opt: Option<PToken>, px: &Px) {
    validate_brace_matching(left_brace, right_brace_opt, px);
}

pub(crate) fn validate_if_expr(
    keyword: PToken,
    cond_opt: Option<&AfterExpr>,
    body_opt: Option<&AfterBlock>,
    else_opt: Option<PToken>,
    alt_opt: Option<&AfterExpr>,
    px: &mut Px,
) {
    match (cond_opt, body_opt, else_opt, alt_opt) {
        (Some(_), Some(_), None, _) | (Some(_), Some(_), Some(_), Some(_)) => {}
        (None, ..) => {
            px.logger().error(
                PLoc::TokenBehind(keyword),
                "if キーワードの後ろに条件式が必要です。",
            );
        }
        (Some(cond), None, ..) => {
            px.builder.error_behind(
                cond.1.id(),
                "if 式の本体が見つかりません。(if 式の本体は波カッコ {} で囲む必要があります。)",
            );
        }
        (Some(_), Some(_), Some(else_keyword), None) => {
            px.logger().error(PLoc::TokenBehind(else_keyword), "else 節の本体が見つかりません。(else 節の本体は if 式かブロック { ... } に限られます。)");
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::parse_tokens;
    use crate::token::tokenize;
    use crate::{logs::DocLogs, source::cursor_text::parse_cursor_text};

    fn assert_syntax_error(text: &str) {
        let parse_text = parse_cursor_text(text).unwrap();
        let text = parse_text.as_str().to_string();
        let expected = parse_text.to_range_vec();

        let logs = DocLogs::new();
        let tokens = tokenize(text.into());
        let tree = &parse_tokens(tokens, logs.logger());

        let mut actual = logs
            .finish()
            .into_iter()
            .map(|loc| loc.range(tree).unwrap())
            .collect::<Vec<_>>();
        actual.sort();

        assert_eq!(actual, expected, "{:#?}", tree);
    }

    #[test]
    fn test_ptr_ty_syntax_error() {
        assert_syntax_error("fn f() -> *<[]> {}");
    }

    #[test]
    fn test_record_pat_syntax_error() {
        assert_syntax_error(
            r#"
                match a {
                    A <[{]> => {}
                }
            "#,
        );
    }

    #[test]
    fn test_paren_expr_syntax_error() {
        assert_syntax_error("const A: i32 = <[(]> 1;");
    }

    #[test]
    fn test_record_expr_syntax_error() {
        assert_syntax_error("Point <[{]> x: 0.0, y: 1.0");
    }

    #[test]
    fn test_field_expr_syntax_error() {
        assert_syntax_error("point.<[]> + 1;");
    }

    #[test]
    fn test_as_expr_syntax_error() {
        assert_syntax_error("0 as<[]> ;");
    }

    #[test]
    fn test_prefix_expr_syntax_error() {
        assert_syntax_error("*<[]> ;");
    }

    #[test]
    fn test_prefix_expr_syntax_error_after_mut() {
        assert_syntax_error("&mut<[]> ;");
    }

    #[test]
    fn test_binary_op_expr_syntax_error() {
        assert_syntax_error("a %<[]> ;")
    }

    #[test]
    fn test_block_expr_syntax_error() {
        assert_syntax_error("<[{]> ;");
    }

    #[test]
    fn test_if_expr_syntax_error_no_cond() {
        assert_syntax_error("if<[]> {}");
    }

    #[test]
    fn test_if_expr_syntax_error_no_body() {
        assert_syntax_error("if true<[]> ;");
    }

    #[test]
    fn test_if_expr_syntax_error_no_alt() {
        assert_syntax_error("if true {} else<[]> ;");
    }
}
