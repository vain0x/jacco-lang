//! 構文エラーについて書く場所

use super::*;

fn validate_paren_matching(left_paren: PToken, right_paren_opt: Option<PToken>, px: &Px) {
    if right_paren_opt.is_none() {
        px.logger()
            .error(PLoc::Token(left_paren), "丸カッコ () が閉じられていません");
    }
}

fn validate_brace_matching(left_brace: PToken, right_brace_opt: Option<PToken>, px: &Px) {
    if right_brace_opt.is_none() {
        px.logger()
            .error(PLoc::Token(left_brace), "波カッコ {} が閉じられていません");
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
        px.logger().error(
            PLoc::TokenBehind(dot),
            "ドット . の後ろにフィールド名が必要です。",
        );
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
                "条件式の後ろに本体が必要です。(if 式の本体は波カッコ {} で囲む必要があります。)",
            );
        }
        (Some(_), Some(_), Some(else_keyword), None) => {
            px.logger().error(PLoc::TokenBehind(else_keyword), "else 節の本体が見つかりません。(else 節の本体は if 式であるか、波カッコ {} で囲む必要があります。)");
        }
    }
}

pub(crate) fn validate_arm(
    pat: &AfterPat,
    arrow_opt: Option<PToken>,
    body_opt: Option<&AfterExpr>,
    _comma_opt: Option<PToken>,
    px: &mut Px,
) {
    // FIXME: 次のアームが存在し、本体が '{}' で終止していなければ、カンマの抜けを指摘する？ (match 式の方で調査するほうがよいかもしれない)

    match (arrow_opt, body_opt) {
        (Some(_), Some(_)) => {}
        (None, ..) => {
            px.builder
                .error_behind(pat.1.id(), "パターンの後に => が必要です。");
        }
        (Some(arrow), _) => {
            px.logger().error(
                PLoc::TokenBehind(arrow),
                "=> の後にアームの本体となる式が必要です。",
            );
        }
    }
}

pub(crate) fn validate_match_expr(
    keyword: PToken,
    cond_opt: Option<&AfterExpr>,
    left_brace_opt: Option<PToken>,
    _arms: &[AfterArm],
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) {
    match (cond_opt, left_brace_opt, right_brace_opt) {
        (None, ..) => {
            px.logger().error(
                PLoc::TokenBehind(keyword),
                "match キーワードの後ろに条件式が必要です。",
            );
        }
        (Some(cond), None, ..) => {
            px.builder.error_behind(
                cond.1.id(),
                "条件式の後にアームの本体が必要です。(本体は波カッコ {} で囲む必要があります。)",
            );
        }
        (Some(_), Some(left_brace), right_brace_opt) => {
            validate_brace_matching(left_brace, right_brace_opt, px);
        }
    }
}

pub(crate) fn validate_while_expr(
    keyword: PToken,
    cond_opt: Option<&AfterExpr>,
    body_opt: Option<&AfterBlock>,
    px: &mut Px,
) {
    match (cond_opt, body_opt) {
        (Some(_), Some(_)) => {}
        (None, ..) => px.logger().error(
            PLoc::TokenBehind(keyword),
            "while キーワードの後ろに条件式が必要です。",
        ),
        (Some(cond), None) => {
            px.builder.error_behind(
                cond.1.id(),
                "条件式の後にループの本体が必要です。(本体は波カッコ {} で囲む必要があります。)",
            );
        }
    }
}

pub(crate) fn validate_loop_expr(keyword: PToken, body_opt: Option<&AfterBlock>, px: &mut Px) {
    if body_opt.is_none() {
        px.logger().error(
            PLoc::TokenBehind(keyword),
            "loop キーワードの後にループの本体が必要です。(本体は波カッコ {} で囲む必要があります。)",
        );
    }
}

pub(crate) fn validate_arg_list(
    left_paren: PToken,
    _args: &Vec<AfterArg>,
    right_paren_opt: Option<PToken>,
    px: &Px,
) {
    // FIXME: カンマの抜けを報告する

    validate_paren_matching(left_paren, right_paren_opt, px);
}

// -----------------------------------------------
// 宣言
// -----------------------------------------------

fn error_behind_token(token: PToken, message: impl Into<String>, px: &Px) {
    px.logger().error(PLoc::TokenBehind(token), message);
}

fn error_behind_element(event_id: EventId, message: impl Into<String>, px: &mut Px) {
    px.builder.error_behind(event_id, message);
}

enum IsRequired {
    True,
    False,
}

/// `: ty` (必須)
fn validate_ty_ascription(
    name_opt: Option<&AfterUnqualifiableName>,
    colon_opt: Option<PToken>,
    ty_opt: Option<&AfterTy>,
    is_required: IsRequired,
    px: &mut Px,
) {
    match (name_opt, colon_opt, ty_opt, is_required) {
        (None, ..) | (Some(_), Some(_), Some(_), _) | (Some(_), None, _, IsRequired::False) => {
            // OK.
        }
        (Some(name), None, _, IsRequired::True) => error_behind_element(
            name.1.id(),
            "型注釈が必須です。(型注釈は `名前 : 型` と書きます。)",
            px,
        ),
        (Some(_), Some(colon), None, _) => {
            error_behind_token(colon, "コロン : の後に型が必要です。", px)
        }
    }
}

/// `= init` (省略可能)
fn validate_initializer(equal_opt: Option<PToken>, init_opt: Option<&AfterExpr>, px: &Px) {
    if let Some(equal) = equal_opt {
        if init_opt.is_none() {
            error_behind_token(equal, "等号 = の後に式が必要です。", px);
        }
    }
}

fn validate_decl_semi(event: &DeclStart, semi_opt: Option<PToken>, px: &mut Px) {
    if semi_opt.is_none() {
        // FIXME: 末尾がブロックなら省略可能。
        px.builder
            .error_behind(event.id(), "セミコロンが必要です。");
    }
}

pub(crate) fn validate_param(
    name: &AfterUnqualifiableName,
    colon_opt: Option<PToken>,
    ty_opt: Option<&AfterTy>,
    px: &mut Px,
) {
    validate_ty_ascription(Some(name), colon_opt, ty_opt, IsRequired::True, px);
}

pub(crate) fn validate_param_list(
    left_paren: PToken,
    _params: &[AfterParam],
    right_paren_opt: Option<PToken>,
    px: &Px,
) {
    // FIXME: カンマの抜けを報告する

    validate_paren_matching(left_paren, right_paren_opt, px);
}

pub(crate) fn validate_let_decl(
    event: &DeclStart,
    _modifiers: &AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<&AfterUnqualifiableName>,
    colon_opt: Option<PToken>,
    ty_opt: Option<&AfterTy>,
    equal_opt: Option<PToken>,
    init_opt: Option<&AfterExpr>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) {
    if name_opt.is_none() {
        error_behind_token(keyword, "let キーワードの後にパターンが必要です。", px);
        return;
    }

    validate_ty_ascription(name_opt, colon_opt, ty_opt, IsRequired::False, px);
    validate_initializer(equal_opt, init_opt, px);
    validate_decl_semi(event, semi_opt, px);
}

pub(crate) fn validate_const_decl(
    event: &DeclStart,
    _modifiers: &AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<&AfterUnqualifiableName>,
    colon_opt: Option<PToken>,
    ty_opt: Option<&AfterTy>,
    equal_opt: Option<PToken>,
    init_opt: Option<&AfterExpr>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) {
    if name_opt.is_none() {
        error_behind_token(keyword, "const キーワードの後に定数名が必要です。", px);
        return;
    }

    validate_ty_ascription(name_opt, colon_opt, ty_opt, IsRequired::True, px);
    validate_initializer(equal_opt, init_opt, px);
    validate_decl_semi(event, semi_opt, px);
}

pub(crate) fn validate_static_decl(
    event: &DeclStart,
    _modifiers: &AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<&AfterUnqualifiableName>,
    colon_opt: Option<PToken>,
    ty_opt: Option<&AfterTy>,
    equal_opt: Option<PToken>,
    init_opt: Option<&AfterExpr>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) {
    if name_opt.is_none() {
        error_behind_token(keyword, "static キーワードの後に定数名が必要です。", px);
        return;
    }

    validate_ty_ascription(name_opt, colon_opt, ty_opt, IsRequired::True, px);
    validate_initializer(equal_opt, init_opt, px);
    validate_decl_semi(event, semi_opt, px);
}

fn do_validate_fn_decl_sig(
    keyword: PToken,
    name_opt: Option<&AfterUnqualifiableName>,
    param_list_opt: Option<&AfterParamList>,
    arrow_opt: Option<PToken>,
    result_ty_opt: Option<&AfterTy>,
    px: &mut Px,
) -> bool {
    match (name_opt, param_list_opt, arrow_opt, result_ty_opt) {
        (Some(_), Some(_), None, _) | (Some(_), Some(_), Some(_), Some(_)) => return true,
        (None, ..) => error_behind_token(keyword, "関数名が必要です。", px),
        (Some(name), None, ..) => error_behind_element(
            name.1.id(),
            "パラメータリストの丸カッコ () が必要です。",
            px,
        ),
        (Some(_), Some(_), Some(arrow), None) => {
            error_behind_token(arrow, "矢印 -> の後に型が必要です。", px)
        }
    }

    false
}

pub(crate) fn validate_fn_decl(
    event: &DeclStart,
    _modifiers: &AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<&AfterUnqualifiableName>,
    param_list_opt: Option<&AfterParamList>,
    arrow_opt: Option<PToken>,
    result_ty_opt: Option<&AfterTy>,
    block_opt: Option<&AfterBlock>,
    px: &mut Px,
) {
    if !do_validate_fn_decl_sig(
        keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_ty_opt,
        px,
    ) {
        return;
    }

    if block_opt.is_none() {
        error_behind_element(
            event.id(),
            "関数の本体が必要です。(本体は波カッコ { } で囲む必要があります。)",
            px,
        );
    }
}

pub(crate) fn validate_extern_fn_decl(
    event: &DeclStart,
    _modifiers: &AfterDeclModifiers,
    _extern_keyword: PToken,
    fn_keyword: PToken,
    name_opt: Option<&AfterUnqualifiableName>,
    param_list_opt: Option<&AfterParamList>,
    arrow_opt: Option<PToken>,
    result_ty_opt: Option<&AfterTy>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) {
    if !do_validate_fn_decl_sig(
        fn_keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_ty_opt,
        px,
    ) {
        return;
    }

    validate_decl_semi(event, semi_opt, px);
}

pub(crate) fn validate_const_variant_decl(
    _name: &AfterUnqualifiableName,
    equal_opt: Option<PToken>,
    init_opt: Option<&AfterExpr>,
    px: &mut Px,
) {
    validate_initializer(equal_opt, init_opt, px);
}

pub(crate) fn validate_field_decl(
    name: &AfterUnqualifiableName,
    colon_opt: Option<PToken>,
    ty_opt: Option<&AfterTy>,
    px: &mut Px,
) {
    validate_ty_ascription(Some(&name), colon_opt, ty_opt, IsRequired::True, px);
}

pub(crate) fn validate_record_variant_decl(
    _name: &AfterUnqualifiableName,
    left_brace: PToken,
    _fields: &AfterFieldDecls,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) {
    // FIXME: カンマの抜けを報告する

    validate_brace_matching(left_brace, right_brace_opt, px);
}

pub(crate) fn validate_enum_decl(
    _modifiers: &AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<&AfterUnqualifiableName>,
    left_brace_opt: Option<PToken>,
    _variants: &AfterVariantDecls,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) {
    // FIXME: カンマの抜けを報告する

    match (name_opt, left_brace_opt, right_brace_opt) {
        (None, ..) => error_behind_token(keyword, "enum キーワードの後に型名が必要です。", px),
        (Some(name), None, ..) => {
            error_behind_element(name.1.id(), "enum 宣言の本体が必要です。", px);
        }
        (Some(_), Some(left_brace), _) => validate_brace_matching(left_brace, right_brace_opt, px),
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

        let errors = logs.finish();
        let mut actual = errors
            .iter()
            .map(|loc| loc.range(tree).unwrap())
            .collect::<Vec<_>>();
        actual.sort();

        let messages = errors
            .into_iter()
            .map(|item| item.into_message())
            .collect::<Vec<_>>();

        assert_eq!(
            actual, expected,
            "messages={:#?}, tree={:#?}",
            messages, tree
        );
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

    #[test]
    fn test_arm_syntax_error_no_arrow() {
        assert_syntax_error("match a { _<[]> }");
    }

    #[test]
    fn test_arm_syntax_error_no_body() {
        assert_syntax_error("match a { _ =><[]> }");
    }

    #[test]
    fn test_match_expr_syntax_error_no_cond() {
        assert_syntax_error("match<[]> {}");
    }

    #[test]
    fn test_match_expr_syntax_error_no_block() {
        assert_syntax_error("match a<[]> ;");
    }

    #[test]
    fn test_while_expr_syntax_error_no_cond() {
        assert_syntax_error("while<[]> {}");
    }

    #[test]
    fn test_while_expr_syntax_error_no_block() {
        assert_syntax_error("while true<[]> ;");
    }

    #[test]
    fn test_loop_expr_syntax_error_no_block() {
        assert_syntax_error("loop<[]> ;");
    }

    #[test]
    fn test_arg_list_syntax_error() {
        assert_syntax_error("f<[(]> ;");
    }

    #[test]
    fn test_param_decl_syntax_error_no_colon() {
        assert_syntax_error("fn f( a<[]> ) {}");
        assert_syntax_error("fn f( a<[]> , b<[]> , c : i32 ) {}");
    }

    #[test]
    fn test_param_list_syntax_error_no_right_paren() {
        // FIXME: 関数の本体がない旨のエラーが変な位置に出る。適切にエラー回復すれば出ないようにできる？
        assert_syntax_error("fn f<[(]> a: i32 {<[]>}");
    }

    #[test]
    fn test_let_decl_no_pat() {
        assert_syntax_error("let<[]> ;");
    }

    #[test]
    fn test_let_decl_no_ty() {
        assert_syntax_error("let _ :<[]> ;");
    }

    #[test]
    fn test_let_decl_no_init() {
        assert_syntax_error("let _ =<[]> ;");
    }

    #[test]
    fn test_let_decl_no_semi_after_pat() {
        assert_syntax_error("let _<[]>");
    }

    #[test]
    fn test_let_decl_no_semi_after_init() {
        assert_syntax_error("let n: i32 = 1<[]>");
    }

    #[test]
    fn test_const_decl_no_name() {
        assert_syntax_error("const<[]> ;");
    }

    #[test]
    fn test_const_decl_no_ty_ascription() {
        assert_syntax_error("const A<[]> = 1;");
    }

    #[test]
    fn test_const_decl_no_semi_after_init() {
        assert_syntax_error("{ const A: i32 = 1<[]> }");
    }

    // static は const と同じだから省略。

    #[test]
    fn test_fn_decl_no_name() {
        assert_syntax_error("fn<[]> () {}");
    }

    #[test]
    fn test_fn_decl_no_param_list() {
        assert_syntax_error("fn f<[]> -> i32 {}");
    }

    #[test]
    fn test_fn_decl_no_result_ty() {
        assert_syntax_error("fn f() -><[]> {}");
    }

    #[test]
    fn test_fn_decl_no_body_after_param_list() {
        assert_syntax_error("fn f()<[]> ;");
    }

    #[test]
    fn test_fn_decl_no_body_after_ty() {
        assert_syntax_error("fn f() -> i32<[]> ;");
    }

    // extern fn はおおよそ fn と同じだから省略。

    #[test]
    fn test_extern_fn_decl_no_semi() {
        assert_syntax_error("extern fn f()<[]> ");
    }

    #[test]
    fn test_const_variant_decl_no_init() {
        assert_syntax_error("enum A { A =<[]> , }")
    }

    #[test]
    fn test_field_decl_no_colon() {
        assert_syntax_error("struct Point { x<[]> , }");
    }

    #[test]
    fn test_field_decl_no_ty() {
        assert_syntax_error("struct Point { x :<[]> , }");
    }

    #[test]
    fn test_record_variant_syntax_error() {
        assert_syntax_error("enum A <[{]> B <[{]> ");
    }

    #[test]
    fn test_enum_decl_syntax_error_no_name() {
        assert_syntax_error("enum<[]> {}");
    }

    #[test]
    fn test_enum_decl_syntax_error_no_block() {
        assert_syntax_error("enum Option<[]> ");
    }
}
