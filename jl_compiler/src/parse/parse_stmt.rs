//! 文の構文解析ルール

use super::*;
use crate::{cps::KVis, logs::DocLogger};
use parse_expr::parse_unqualifiable_name;

enum VariantParentKind {
    Enum,
    Struct,
}

fn parse_ty_param_list(px: &mut Px) -> Option<AfterTyParamList> {
    let left_paren = match px.next() {
        TokenKind::LeftBracket => px.bump(),
        TokenKind::ColonColon if px.nth(1) == TokenKind::LeftBracket => {
            px.bump();
            px.bump()
        }
        _ => return None,
    };

    let mut ty_params = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBracket | TokenKind::RightBrace => break,
            TokenKind::Ident => {
                let param_event = px.start_element();
                let name = parse_unqualifiable_name(px).unwrap();
                let comma_opt = px.eat(TokenKind::Comma);

                ty_params.push(alloc_ty_param(param_event, name, comma_opt, px));
            }
            _ => px.skip(),
        }
    }

    let right_paren_opt = px.eat(TokenKind::RightBracket);
    Some(alloc_ty_param_list(
        left_paren,
        ty_params,
        right_paren_opt,
        px,
    ))
}

fn parse_param_list(px: &mut Px) -> Option<AfterParamList> {
    let left_paren = px.eat(TokenKind::LeftParen)?;
    let mut params = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightParen | TokenKind::RightBrace => break,
            TokenKind::Ident => {
                let param_event = px.start_element();
                let name = parse_unqualifiable_name(px).unwrap();
                let (colon_opt, ty_opt) = parse_ty_ascription(px);
                let comma_opt = px.eat(TokenKind::Comma);

                params.push(alloc_param(
                    param_event,
                    name,
                    colon_opt,
                    ty_opt,
                    comma_opt,
                    px,
                ))
            }
            _ => px.skip(),
        }
    }

    let right_paren_opt = px.eat(TokenKind::RightParen);
    Some(alloc_param_list(left_paren, params, right_paren_opt, px))
}

/// 結果型注釈 (`-> T`) のパース
pub(crate) fn parse_result_ty(px: &mut Px) -> (Option<PToken>, Option<AfterTy>) {
    let arrow_opt = px.eat(TokenKind::RightSlimArrow);
    let ty_opt = if arrow_opt.is_some() {
        parse_ty(px)
    } else {
        None
    };
    (arrow_opt, ty_opt)
}

fn parse_expr_stmt(modifiers: AfterStmtModifiers, px: &mut Px) -> Option<AfterStmt> {
    let expr = parse_expr(px)?;
    let semi_opt = px.eat(TokenKind::Semi);

    Some(alloc_expr_stmt(modifiers.0, expr, semi_opt, px))
}

fn parse_let_stmt(modifiers: AfterStmtModifiers, keyword: PToken, px: &mut Px) -> AfterStmt {
    let name_opt = parse_unqualifiable_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);
    alloc_let_stmt(
        modifiers, keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt, px,
    )
}

fn parse_const_stmt(modifiers: AfterStmtModifiers, keyword: PToken, px: &mut Px) -> AfterStmt {
    let name_opt = parse_unqualifiable_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);
    alloc_const_stmt(
        modifiers, keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt, px,
    )
}

fn parse_static_stmt(modifiers: AfterStmtModifiers, keyword: PToken, px: &mut Px) -> AfterStmt {
    let name_opt = parse_unqualifiable_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);
    alloc_static_stmt(
        modifiers, keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt, px,
    )
}

fn parse_fn_stmt(modifiers: AfterStmtModifiers, keyword: PToken, px: &mut Px) -> AfterStmt {
    before_fn_stmt(px);

    let name_opt = parse_unqualifiable_name(px);
    let ty_param_list_opt = parse_ty_param_list(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_ty_opt) = parse_result_ty(px);
    let block_opt = parse_block(px);

    alloc_fn_stmt(
        modifiers,
        keyword,
        name_opt,
        ty_param_list_opt,
        param_list_opt,
        arrow_opt,
        result_ty_opt,
        block_opt,
        px,
    )
}

fn parse_extern_fn_stmt(
    modifiers: AfterStmtModifiers,
    extern_keyword: PToken,
    fn_keyword: PToken,
    px: &mut Px,
) -> AfterStmt {
    before_extern_fn_stmt(px);

    let name_opt = parse_unqualifiable_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_ty_opt) = parse_result_ty(px);
    let semi_opt = px.eat(TokenKind::Semi);

    alloc_extern_fn_stmt(
        modifiers,
        extern_keyword,
        fn_keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_ty_opt,
        semi_opt,
        px,
    )
}

fn parse_const_variant_decl(
    event: ParseStart,
    name: AfterUnqualifiableName,
    px: &mut Px,
) -> AfterVariantDecl {
    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let comma_opt = px.eat(TokenKind::Comma);
    alloc_const_variant_decl(event, name, equal_opt, init_opt, comma_opt, px)
}

fn parse_field_decl(
    event: ParseStart,
    name: AfterUnqualifiableName,
    px: &mut Px,
) -> AfterFieldDecl {
    let colon_opt = px.eat(TokenKind::Colon);
    let ty_opt = parse_ty(px);
    let comma_opt = px.eat(TokenKind::Comma);
    alloc_field_decl(event, name, colon_opt, ty_opt, comma_opt, px)
}

fn parse_field_decls(px: &mut Px) -> AfterFieldDecls {
    let mut fields = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace => break,
            TokenKind::Ident => {
                let event = px.start_element();
                let name = parse_unqualifiable_name(px).unwrap();
                let field = parse_field_decl(event, name, px);
                // let can_continue = field.0.comma_opt.is_some();
                fields.push(field);

                // if !can_continue {
                //     break;
                // }
            }
            _ => px.skip(),
        }
    }

    alloc_field_decls(fields, px)
}

fn parse_record_variant_decl(
    event: ParseStart,
    name: AfterUnqualifiableName,
    ty_param_list_opt: Option<AfterTyParamList>,
    left_brace: PToken,
    px: &mut Px,
) -> AfterVariantDecl {
    let fields = parse_field_decls(px);
    let right_brace_opt = px.eat(TokenKind::RightBrace);
    let comma_opt = px.eat(TokenKind::Comma);
    alloc_record_variant_decl(
        event,
        name,
        ty_param_list_opt,
        left_brace,
        fields,
        right_brace_opt,
        comma_opt,
        px,
    )
}

fn parse_variant_decl(parent: VariantParentKind, px: &mut Px) -> Option<AfterVariantDecl> {
    let event = px.start_element();
    let name = parse_unqualifiable_name(px)?;

    let ty_param_list_opt = match parent {
        VariantParentKind::Enum => None,
        VariantParentKind::Struct => parse_ty_param_list(px),
    };

    let variant_decl = match px.next() {
        TokenKind::LeftBrace => {
            let left_brace = px.bump();
            parse_record_variant_decl(event, name, ty_param_list_opt, left_brace, px)
        }
        _ => {
            // FIXME: このとき型パラメータリストは空でなければいけない
            parse_const_variant_decl(event, name, px)
        }
    };
    Some(variant_decl)
}

fn parse_variants(px: &mut Px) -> AfterVariantDecls {
    let mut variants = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace => break,
            _ => {}
        }

        match parse_variant_decl(VariantParentKind::Enum, px) {
            Some(variant_decl) => variants.push(variant_decl),
            None => px.skip(),
        }
    }

    alloc_variants(variants, px)
}

fn parse_enum_stmt(modifiers: AfterStmtModifiers, keyword: PToken, px: &mut Px) -> AfterStmt {
    before_enum_stmt(px);
    let name_opt = parse_unqualifiable_name(px);

    let left_brace_opt = px.eat(TokenKind::LeftBrace);
    let variants = if left_brace_opt.is_some() {
        parse_variants(px)
    } else {
        vec![]
    };
    let right_brace_opt = if left_brace_opt.is_some() {
        px.eat(TokenKind::RightBrace)
    } else {
        None
    };

    alloc_enum_stmt(
        modifiers,
        keyword,
        name_opt,
        left_brace_opt,
        variants,
        right_brace_opt,
        px,
    )
}

fn parse_struct_stmt(modifiers: AfterStmtModifiers, keyword: PToken, px: &mut Px) -> AfterStmt {
    before_struct_stmt(px);
    let variant_opt = parse_variant_decl(VariantParentKind::Struct, px);
    let semi_opt = px.eat(TokenKind::Semi);

    alloc_struct_stmt(modifiers, keyword, variant_opt, semi_opt, px)
}

fn parse_use_stmt(modifiers: AfterStmtModifiers, keyword: PToken, px: &mut Px) -> AfterStmt {
    let name_opt = parse_qualifiable_name(px);
    let semi_opt = px.eat(TokenKind::Semi);
    alloc_use_stmt(modifiers, keyword, name_opt, semi_opt, px)
}

fn parse_attr_stmt(event: StmtStart, hash_bang: PToken, px: &mut Px) -> AfterStmt {
    let left_bracket_opt = px.eat(TokenKind::LeftBracket);

    if left_bracket_opt.is_some() {
        loop {
            match px.next() {
                TokenKind::Eof
                | TokenKind::RightBracket
                | TokenKind::Hash
                | TokenKind::HashBang => break,
                _ => {
                    let _ = px.bump();
                }
            }
        }
    }

    if left_bracket_opt.is_some() {
        px.eat(TokenKind::RightBracket);
    }

    alloc_attr_stmt(event, hash_bang, px)
}

pub(crate) fn parse_stmt(px: &mut Px) -> Option<AfterStmt> {
    let event = px.start_element();

    if px.next() == TokenKind::HashBang {
        let hash_bang = px.bump();
        return Some(parse_attr_stmt(event, hash_bang, px));
    }

    // modifiers
    let vis_opt = match px.next() {
        // TokenKind::Priv => Some((KVis::Priv, px.bump()),
        TokenKind::Pub => Some((KVis::Pub, px.bump())),
        _ => None,
    };
    let modifiers: AfterStmtModifiers = (event, vis_opt);

    let stmt = match px.next() {
        TokenKind::Let => {
            let keyword = px.bump();
            parse_let_stmt(modifiers, keyword, px)
        }
        TokenKind::Const => {
            let keyword = px.bump();
            parse_const_stmt(modifiers, keyword, px)
        }
        TokenKind::Static => {
            let keyword = px.bump();
            parse_static_stmt(modifiers, keyword, px)
        }
        TokenKind::Fn => {
            let keyword = px.bump();
            parse_fn_stmt(modifiers, keyword, px)
        }
        TokenKind::Extern if px.nth(1) == TokenKind::Fn => {
            let extern_keyword = px.bump();
            let fn_keyword = px.bump();
            parse_extern_fn_stmt(modifiers, extern_keyword, fn_keyword, px)
        }
        TokenKind::Enum => {
            let keyword = px.bump();
            parse_enum_stmt(modifiers, keyword, px)
        }
        TokenKind::Struct => {
            let keyword = px.bump();
            parse_struct_stmt(modifiers, keyword, px)
        }
        TokenKind::Use => {
            let keyword = px.bump();
            parse_use_stmt(modifiers, keyword, px)
        }
        _ => {
            // FIXME: pub expr; をエラーにする
            return parse_expr_stmt(modifiers, px);
        }
    };
    Some(stmt)
}

fn do_parse_stmts(stmts: &mut Vec<AfterStmt>, px: &mut Px) {
    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace => break,
            TokenKind::Semi => {
                // 空文
                let _ = px.bump();
                continue;
            }
            _ => {}
        }

        match parse_stmt(px) {
            Some(stmt) => stmts.push(stmt),
            None => px.skip(),
        }
    }
}

pub(crate) fn parse_semi(px: &mut Px) -> AfterSemi {
    let mut stmts = vec![];
    do_parse_stmts(&mut stmts, px);
    stmts
}

fn parse_root(px: &mut Px) -> AfterRoot {
    let mut stmts = vec![];

    loop {
        do_parse_stmts(&mut stmts, px);

        match px.next() {
            TokenKind::Eof => break,
            _ => px.skip(),
        }
    }

    stmts
}

pub(crate) fn parse_tokens(mut tokens: Vec<TokenData>, logger: DocLogger) -> PTree {
    tokens.retain(|token| match token.kind() {
        TokenKind::Other => {
            logger.error(
                PLoc::Range(token.range()),
                format!("invalid token {:?}", token.text()),
            );
            false
        }
        TokenKind::Space | TokenKind::Comment => false,
        _ => true,
    });
    let tokens = PTokens::from_vec(tokens);

    let mut px = Px::new(tokens, logger.clone());

    let root = parse_root(&mut px);
    let stmts = px.alloc_stmts(root);
    let name_referents = px.syntax_scopes.finish(&px.ast);
    let (eof, skipped, tokens, mut elements, mut ast, builder) = px.finish();

    let (root, events) = builder.finish(&mut elements, &logger);
    ast.root = ARoot { stmts: stmts };
    ast.events = events;

    for token in &skipped {
        logger.error(
            PLoc::Token(*token),
            "このトークンを有効な構文として解釈できません。".into(),
        );
    }

    PTree {
        eof,
        elements,
        skipped,
        tokens,
        ast,
        name_referents,
        root,
    }
}

#[cfg(test)]
mod tests {
    use crate::{logs::DocLogs, parse, token};

    #[test]
    fn test_parse_does_not_stop() {
        let source_code = "}}}}}";

        let logs = DocLogs::new();
        let tokens = token::tokenize(source_code.to_string().into());
        let p_root = parse::parse_tokens(tokens, logs.logger());

        assert!(p_root.ast.stmts().is_empty());
    }
}
