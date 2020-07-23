//! 宣言の構文解析ルール

use super::*;
use crate::cps::KVis;
use parse_expr::parse_unqualifiable_name;

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
fn parse_result_ty(px: &mut Px) -> (Option<PToken>, Option<AfterTy>) {
    let arrow_opt = px.eat(TokenKind::RightSlimArrow);
    let ty_opt = if arrow_opt.is_some() {
        parse_ty(px)
    } else {
        None
    };
    (arrow_opt, ty_opt)
}

fn parse_expr_decl(modifiers: AfterDeclModifiers, px: &mut Px) -> Option<AfterDecl> {
    let expr = parse_expr(px)?;
    let semi_opt = px.eat(TokenKind::Semi);

    Some(alloc_expr_decl(modifiers.0, expr, semi_opt, px))
}

fn parse_let_decl(modifiers: AfterDeclModifiers, keyword: PToken, px: &mut Px) -> AfterDecl {
    let name_opt = parse_unqualifiable_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);
    alloc_let_decl(
        modifiers, keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt, px,
    )
}

fn parse_const_decl(modifiers: AfterDeclModifiers, keyword: PToken, px: &mut Px) -> AfterDecl {
    let name_opt = parse_unqualifiable_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);
    alloc_const_decl(
        modifiers, keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt, px,
    )
}

fn parse_static_decl(modifiers: AfterDeclModifiers, keyword: PToken, px: &mut Px) -> AfterDecl {
    let name_opt = parse_unqualifiable_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);
    alloc_static_decl(
        modifiers, keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt, px,
    )
}

fn parse_fn_decl(modifiers: AfterDeclModifiers, keyword: PToken, px: &mut Px) -> AfterDecl {
    let name_opt = parse_unqualifiable_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_ty_opt) = parse_result_ty(px);
    let block_opt = parse_block(px);
    alloc_fn_decl(
        modifiers,
        keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_ty_opt,
        block_opt,
        px,
    )
}

fn parse_extern_fn_decl(
    modifiers: AfterDeclModifiers,
    extern_keyword: PToken,
    fn_keyword: PToken,
    px: &mut Px,
) -> AfterDecl {
    let name_opt = parse_unqualifiable_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_ty_opt) = parse_result_ty(px);
    let semi_opt = px.eat(TokenKind::Semi);

    alloc_extern_fn_decl(
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
                let can_continue = field.0.comma_opt.is_some();
                fields.push(field);

                if !can_continue {
                    break;
                }
            }
            _ => px.skip(),
        }
    }

    alloc_field_decls(fields, px)
}

fn parse_record_variant_decl(
    event: ParseStart,
    name: AfterUnqualifiableName,
    left_brace: PToken,
    px: &mut Px,
) -> AfterVariantDecl {
    let fields = parse_field_decls(px);
    let right_brace_opt = px.eat(TokenKind::RightBrace);
    let comma_opt = px.eat(TokenKind::Comma);
    alloc_record_variant_decl(
        event,
        name,
        left_brace,
        fields,
        right_brace_opt,
        comma_opt,
        px,
    )
}

fn parse_variant_decl(px: &mut Px) -> Option<AfterVariantDecl> {
    let event = px.start_element();
    let name = parse_unqualifiable_name(px)?;

    let variant_decl = match px.next() {
        TokenKind::LeftBrace => {
            let left_brace = px.bump();
            parse_record_variant_decl(event, name, left_brace, px)
        }
        _ => parse_const_variant_decl(event, name, px),
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

        match parse_variant_decl(px) {
            Some(variant_decl) => variants.push(variant_decl),
            None => px.skip(),
        }
    }

    alloc_variants(variants, px)
}

fn parse_enum_decl(modifiers: AfterDeclModifiers, keyword: PToken, px: &mut Px) -> AfterDecl {
    let name_opt = parse_unqualifiable_name(px);

    let left_brace_opt = px.eat(TokenKind::LeftBrace);
    let variants = if left_brace_opt.is_some() {
        parse_variants(px)
    } else {
        (vec![], vec![])
    };
    let right_brace_opt = if left_brace_opt.is_some() {
        px.eat(TokenKind::RightBrace)
    } else {
        None
    };

    alloc_enum_decl(
        modifiers,
        keyword,
        name_opt,
        left_brace_opt,
        variants,
        right_brace_opt,
        px,
    )
}

fn parse_struct_decl(modifiers: AfterDeclModifiers, keyword: PToken, px: &mut Px) -> AfterDecl {
    let variant_opt = parse_variant_decl(px);
    let semi_opt = px.eat(TokenKind::Semi);

    alloc_struct_decl(modifiers, keyword, variant_opt, semi_opt, px)
}

fn parse_use_decl(modifiers: AfterDeclModifiers, keyword: PToken, px: &mut Px) -> AfterDecl {
    let name_opt = parse_qualifiable_name(px);
    let semi_opt = px.eat(TokenKind::Semi);
    alloc_use_decl(modifiers, keyword, name_opt, semi_opt, px)
}

pub(crate) fn parse_decl(px: &mut Px) -> Option<AfterDecl> {
    let event = px.start_element();

    // modifiers
    let vis_opt = match px.next() {
        // TokenKind::Priv => Some((KVis::Priv, px.bump()),
        TokenKind::Pub => Some((KVis::Pub, px.bump())),
        _ => None,
    };
    let modifiers: AfterDeclModifiers = (event, vis_opt);

    let decl = match px.next() {
        TokenKind::Let => {
            let keyword = px.bump();
            parse_let_decl(modifiers, keyword, px)
        }
        TokenKind::Const => {
            let keyword = px.bump();
            parse_const_decl(modifiers, keyword, px)
        }
        TokenKind::Static => {
            let keyword = px.bump();
            parse_static_decl(modifiers, keyword, px)
        }
        TokenKind::Fn => {
            let keyword = px.bump();
            parse_fn_decl(modifiers, keyword, px)
        }
        TokenKind::Extern if px.nth(1) == TokenKind::Fn => {
            let extern_keyword = px.bump();
            let fn_keyword = px.bump();
            parse_extern_fn_decl(modifiers, extern_keyword, fn_keyword, px)
        }
        TokenKind::Enum => {
            let keyword = px.bump();
            parse_enum_decl(modifiers, keyword, px)
        }
        TokenKind::Struct => {
            let keyword = px.bump();
            parse_struct_decl(modifiers, keyword, px)
        }
        TokenKind::Use => {
            let keyword = px.bump();
            parse_use_decl(modifiers, keyword, px)
        }
        _ => {
            // FIXME: pub expr; をエラーにする
            return parse_expr_decl(modifiers, px);
        }
    };
    Some(decl)
}

fn do_parse_decls(decls: &mut Vec<AfterDecl>, px: &mut Px) {
    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace => break,
            TokenKind::Semi => {
                // 空宣言
                let _ = px.bump();
                continue;
            }
            _ => {}
        }

        match parse_decl(px) {
            Some(decl) => decls.push(decl),
            None => px.skip(),
        }
    }
}

pub(crate) fn parse_semi(px: &mut Px) -> AfterSemi {
    let mut decls = vec![];
    do_parse_decls(&mut decls, px);

    let (mut decls, a_decls): (Vec<_>, Vec<_>) = decls.into_iter().map(|(a, b, _)| (a, b)).unzip();

    // 末尾が式文で、セミコロンで終止していなければ、それは最後の式とみなす。
    let last_opt = match decls.pop() {
        Some(PDecl::Expr(PExprDecl {
            expr: last,
            semi_opt: None,
        })) => Some(last),
        Some(last) => {
            decls.push(last);
            None
        }
        None => None,
    };

    (decls, last_opt, a_decls)
}

fn parse_root(px: &mut Px) -> AfterRoot {
    let mut decls = vec![];

    loop {
        do_parse_decls(&mut decls, px);

        match px.next() {
            TokenKind::Eof => break,
            _ => px.skip(),
        }
    }

    decls
}

pub(crate) fn parse_tokens(mut tokens: Vec<TokenData>, logger: Logger) -> PRoot {
    tokens.retain(|token| match token.kind() {
        TokenKind::Other => {
            logger.error(token, format!("invalid token {:?}", token.text()));
            false
        }
        TokenKind::Space | TokenKind::Comment => false,
        _ => true,
    });
    let tokens = PTokens::from_vec(tokens);

    let mut px = Px::new(tokens, logger);

    let decls = parse_root(&mut px);
    let (decls, a_decls): (Vec<_>, Vec<_>) = decls.into_iter().map(|(a, b, _)| (a, b)).unzip();
    let (eof, names, skipped, tokens, mut elements, mut ast, builder) = px.finish();

    let root = builder.finish(&mut elements);
    let a_decls = ast.decls.alloc_slice(a_decls);
    ast.root = ARoot { decls: a_decls };

    PRoot {
        decls,
        eof,
        names,
        elements,
        skipped,
        tokens,
        ast,
        root,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logs::Logs,
        parse,
        token::{self, TokenSource},
    };

    #[test]
    fn test_parse_does_not_stop() {
        let token_source = TokenSource::Special("test");
        let source_code = "}}}}}";

        let logs = Logs::new();
        let tokens = token::tokenize(token_source, source_code.to_string().into());
        let p_root = parse::parse_tokens(tokens, logs.logger());

        assert!(p_root.decls.is_empty());
    }
}
