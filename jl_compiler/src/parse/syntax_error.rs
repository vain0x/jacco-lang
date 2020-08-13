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
}
