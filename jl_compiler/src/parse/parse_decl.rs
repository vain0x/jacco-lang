//! 宣言の構文解析ルール

use super::*;
use crate::cps::KVis;

fn parse_param_list(px: &mut Px) -> Option<PParamList> {
    let left_paren = px.eat(TokenKind::LeftParen)?;

    let mut params = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightParen | TokenKind::RightBrace => break,
            TokenKind::Ident => {
                let name = parse_name(px).unwrap();
                let (colon_opt, ty_opt) = parse_ty_ascription(px);
                let comma_opt = px.eat(TokenKind::Comma);

                params.push(PParam {
                    name,
                    colon_opt,
                    ty_opt,
                    comma_opt,
                })
            }
            _ => px.skip(),
        }
    }

    let right_paren_opt = px.eat(TokenKind::RightParen);

    Some(PParamList {
        left_paren,
        right_paren_opt,
        params,
    })
}

/// 結果型注釈 (`-> T`) のパース
fn parse_result_ty(px: &mut Px) -> (Option<PToken>, Option<PTy>) {
    let arrow_opt = px.eat(TokenKind::RightSlimArrow);
    let ty_opt = if arrow_opt.is_some() {
        parse_ty(px)
    } else {
        None
    };
    (arrow_opt, ty_opt)
}

fn parse_expr_decl(px: &mut Px) -> Option<PExprDecl> {
    let expr = parse_expr(px)?;
    let semi_opt = px.eat(TokenKind::Semi);

    Some(PExprDecl { expr, semi_opt })
}

fn parse_let_decl(px: &mut Px) -> PLetDecl {
    let keyword = px.expect(TokenKind::Let);

    let name_opt = parse_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);

    PLetDecl {
        keyword,
        name_opt,
        colon_opt,
        ty_opt,
        equal_opt,
        init_opt,
        semi_opt,
    }
}

fn parse_const_decl(px: &mut Px) -> PConstDecl {
    let keyword = px.expect(TokenKind::Const);

    let name_opt = parse_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);

    PConstDecl {
        keyword,
        name_opt,
        colon_opt,
        ty_opt,
        equal_opt,
        init_opt,
        semi_opt,
    }
}

fn parse_static_decl(px: &mut Px) -> PStaticDecl {
    let keyword = px.expect(TokenKind::Static);

    let name_opt = parse_name(px);
    let (colon_opt, ty_opt) = parse_ty_ascription(px);

    let equal_opt = px.eat(TokenKind::Equal);
    let init_opt = if equal_opt.is_some() {
        parse_expr(px)
    } else {
        None
    };

    let semi_opt = px.eat(TokenKind::Semi);

    PStaticDecl {
        keyword,
        name_opt,
        colon_opt,
        ty_opt,
        equal_opt,
        init_opt,
        semi_opt,
    }
}

fn parse_fn_decl(vis_opt: Option<PVis>, px: &mut Px) -> PFnDecl {
    let keyword = px.expect(TokenKind::Fn);

    let name_opt = parse_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_ty_opt) = parse_result_ty(px);
    let block_opt = parse_block(px);

    PFnDecl {
        vis_opt,
        keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_ty_opt,
        block_opt,
        fn_id_opt: None,
    }
}

fn parse_extern_fn_decl(px: &mut Px) -> PExternFnDecl {
    let extern_keyword = px.expect(TokenKind::Extern);
    let fn_keyword = px.expect(TokenKind::Fn);

    let name_opt = parse_name(px);
    let param_list_opt = parse_param_list(px);
    let (arrow_opt, result_ty_opt) = parse_result_ty(px);
    let semi_opt = px.eat(TokenKind::Semi);

    PExternFnDecl {
        extern_keyword,
        fn_keyword,
        name_opt,
        param_list_opt,
        arrow_opt,
        result_ty_opt,
        semi_opt,
        extern_fn_id_opt: None,
    }
}

fn parse_const_variant_decl(name: PName, px: &mut Px) -> PConstVariantDecl {
    let equal_opt = px.eat(TokenKind::Equal);
    let value_opt = if equal_opt.is_some() {
        parse_expr(px).map(Box::new)
    } else {
        None
    };

    let comma_opt = px.eat(TokenKind::Comma);
    PConstVariantDecl {
        name,
        equal_opt,
        value_opt,
        comma_opt,
        const_variant_id_opt: None,
    }
}

fn parse_field_decl(px: &mut Px) -> PFieldDecl {
    let name = parse_name(px).unwrap();

    let colon_opt = px.eat(TokenKind::Colon);
    let ty_opt = parse_ty(px);

    let comma_opt = px.eat(TokenKind::Comma);

    PFieldDecl {
        name,
        colon_opt,
        ty_opt,
        comma_opt,
        field_id_opt: None,
    }
}

fn parse_field_decls(px: &mut Px) -> Vec<PFieldDecl> {
    let mut fields = vec![];

    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace => break,
            TokenKind::Ident => {
                let field = parse_field_decl(px);
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

fn parse_record_variant_decl(name: PName, left_brace: PToken, px: &mut Px) -> PRecordVariantDecl {
    let fields = parse_field_decls(px);
    let right_brace_opt = px.eat(TokenKind::RightBrace);
    let comma_opt = px.eat(TokenKind::Comma);

    PRecordVariantDecl {
        name,
        left_brace,
        fields,
        right_brace_opt,
        comma_opt,
    }
}

fn parse_variant_decl(px: &mut Px) -> Option<PVariantDecl> {
    let name = parse_name(px)?;

    let variant_decl = match px.next() {
        TokenKind::LeftBrace => {
            let left_brace = px.bump();
            PVariantDecl::Record(parse_record_variant_decl(name, left_brace, px))
        }
        _ => PVariantDecl::Const(parse_const_variant_decl(name, px)),
    };
    Some(variant_decl)
}

fn parse_variants(px: &mut Px) -> Vec<PVariantDecl> {
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

    variants
}

fn parse_enum_decl(vis_opt: Option<PVis>, px: &mut Px) -> PEnumDecl {
    let keyword = px.expect(TokenKind::Enum);

    let name_opt = parse_name(px);

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

    PEnumDecl {
        vis_opt,
        keyword,
        name_opt,
        left_brace_opt,
        variants,
        right_brace_opt,
    }
}

fn parse_struct_decl(px: &mut Px) -> PStructDecl {
    let keyword = px.expect(TokenKind::Struct);

    let variant_opt = parse_variant_decl(px);
    let semi_opt = px.eat(TokenKind::Semi);

    PStructDecl {
        keyword,
        variant_opt,
        semi_opt,
    }
}

fn parse_use_decl(px: &mut Px) -> PUseDecl {
    let keyword = px.expect(TokenKind::Use);

    let name_opt = parse_name(px);
    let semi_opt = px.eat(TokenKind::Semi);

    PUseDecl {
        keyword,
        name_opt,
        semi_opt,
    }
}

fn parse_decl_with_vis(vis: PVis, px: &mut Px) -> Option<PDecl> {
    // FIXME: const, struct
    let decl = match px.next() {
        TokenKind::Fn => PDecl::Fn(parse_fn_decl(Some(vis), px)),
        TokenKind::Enum => PDecl::Enum(parse_enum_decl(Some(vis), px)),
        _ => {
            px.logger()
                .error(&vis.1.loc(px.tokens()), "unexpected visibility");
            return None;
        }
    };
    Some(decl)
}

pub(crate) fn parse_decl(px: &mut Px) -> Option<PDecl> {
    let decl = match px.next() {
        TokenKind::Pub => {
            let vis = (KVis::Pub, px.bump());
            return parse_decl_with_vis(vis, px);
        }
        TokenKind::Let => PDecl::Let(parse_let_decl(px)),
        TokenKind::Const => PDecl::Const(parse_const_decl(px)),
        TokenKind::Static => PDecl::Static(parse_static_decl(px)),
        TokenKind::Fn => PDecl::Fn(parse_fn_decl(None, px)),
        TokenKind::Extern if px.nth(1) == TokenKind::Fn => {
            PDecl::ExternFn(parse_extern_fn_decl(px))
        }
        TokenKind::Enum => PDecl::Enum(parse_enum_decl(None, px)),
        TokenKind::Struct => PDecl::Struct(parse_struct_decl(px)),
        TokenKind::Use => PDecl::Use(parse_use_decl(px)),
        _ => PDecl::Expr(parse_expr_decl(px)?),
    };
    Some(decl)
}

fn do_parse_decls(decls: &mut Vec<PDecl>, px: &mut Px) {
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

pub(crate) fn parse_semi(px: &mut Px) -> (Vec<PDecl>, Option<PExpr>) {
    let mut decls = vec![];

    do_parse_decls(&mut decls, px);

    // 末尾が式文で、セミコロンで終始していなければ、それは最後の式とみなす。
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

    (decls, last_opt)
}

fn parse_root(px: &mut Px) -> Vec<PDecl> {
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
    let (eof, names, skipped, tokens, elements) = px.finish();

    PRoot {
        decls,
        eof,
        names,
        elements,
        skipped,
        tokens,
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
