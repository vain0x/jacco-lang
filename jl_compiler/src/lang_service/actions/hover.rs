use super::{Doc, LangService, TPos16};
use crate::{
    cps::*,
    lang_service::{
        doc_analysis::DocSymbolAnalysisMut,
        lang_service::{collect_def_sites, hit_test, Content},
    },
    source::TPos,
    token::{tokenize, TokenKind},
};
use std::{
    io::{self, Write},
    rc::Rc,
};

fn do_collect_doc_comments(source_code: Rc<String>, pos: TPos, comments: &mut Vec<String>) {
    let tokens = tokenize(source_code);
    for token in tokens
        .into_iter()
        .rev()
        .skip_while(|token| token.range().start().row >= pos.row)
        .take_while(|token| token.kind().is_leading_trivia())
        .filter(|token| token.kind() == TokenKind::Comment && token.text().starts_with("///"))
    {
        comments.push(token.text()[3..].trim().to_string());
    }
}

fn collect_doc_comments(
    doc: Doc,
    symbol: KModLocalSymbol,
    ls: &mut LangService,
) -> Option<Vec<String>> {
    let DocSymbolAnalysisMut { syntax, symbols } = ls.request_symbols(doc)?;

    let mut locations = vec![];
    collect_def_sites(doc, symbol, syntax, symbols, &mut locations);

    let mut comments = vec![];
    for location in locations {
        let doc = location.doc();
        let pos = location.range().start();

        let source_code = match ls.docs.get(&doc).map(|doc| doc.text.clone()) {
            Some(it) => it,
            None => continue,
        };

        do_collect_doc_comments(source_code, pos, &mut comments);
    }
    comments.reverse();

    if comments.is_empty() {
        None
    } else {
        Some(comments)
    }
}

fn write_param_sig(
    out: &mut impl Write,
    params: &[KSymbol],
    locals: &KLocalArena,
    mod_outlines: &KModOutlines,
) -> io::Result<()> {
    for (i, symbol) in params.iter().enumerate() {
        if i != 0 {
            write!(out, ", ")?;
        }

        let local_var = symbol.local.of(&locals);
        write!(
            out,
            "{}: {}",
            &local_var.name,
            local_var.ty.display(KTyEnv::EMPTY, mod_outlines)
        )?;
    }
    write!(out, ")")
}

fn write_result_ty(
    out: &mut impl Write,
    k_mod: KMod,
    result_ty: &KTy,
    mod_outlines: &KModOutlines,
) -> io::Result<()> {
    if !result_ty.is_unit() {
        let ty = result_ty.to_ty2(k_mod).display(KTyEnv::EMPTY, mod_outlines);
        write!(out, " -> {}", ty)?;
    }
    Ok(())
}

pub(crate) fn hover(doc: Doc, pos: TPos16, ls: &mut LangService) -> Option<Content> {
    ls.request_types();

    let DocSymbolAnalysisMut { syntax, symbols } = ls.request_symbols(doc)?;
    let (symbol, _) = hit_test(doc, pos, syntax, symbols)?;

    let comments = collect_doc_comments(doc, symbol, ls).unwrap_or_default();

    let mut contents_opt = None;
    match symbol {
        KModLocalSymbol::LocalVar { parent, local_var } => {
            ls.do_with_mods(|ls, mod_outlines, mods| {
                let k_mod = ls.docs.get(&doc).unwrap().mod_opt.unwrap();
                let mod_data = k_mod.of_mut(mods);

                let ty_env = match parent {
                    KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).ty_env,
                    KLocalVarParent::ExternFn(_) => KTyEnv::EMPTY,
                };

                let locals = match parent {
                    KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).locals,
                    KLocalVarParent::ExternFn(extern_fn) => {
                        &extern_fn.of(&mod_data.extern_fns).locals
                    }
                };

                contents_opt = Some(Content::JaccoCode(
                    local_var.ty(locals).display(ty_env, mod_outlines),
                ));
            });
        }
        KModLocalSymbol::Const(_) => {}
        KModLocalSymbol::StaticVar(_) => {}
        KModLocalSymbol::Fn(k_fn) => {
            let mut out = Vec::new();

            ls.do_with_mods(|ls, mod_outlines, mods| {
                let k_mod = ls.docs.get(&doc).unwrap().mod_opt.unwrap();
                let mod_outline = k_mod.of(mod_outlines);
                let mod_data = k_mod.of(mods);
                let fn_outline = k_fn.of(&mod_outline.fns);
                let fn_data = k_fn.of(&mod_data.fns);

                let text = {
                    write!(out, "fn {}(", &fn_outline.name).unwrap();
                    write_param_sig(&mut out, &fn_data.params, &fn_data.locals, mod_outlines)
                        .unwrap();
                    write_result_ty(&mut out, k_mod, &fn_outline.result_ty, mod_outlines).unwrap();
                    write!(out, ";").unwrap();
                    unsafe { String::from_utf8_unchecked(out) }
                };

                contents_opt = Some(Content::JaccoCode(text));
            });
        }
        KModLocalSymbol::ExternFn(extern_fn) => {
            let mut out = Vec::new();

            ls.do_with_mods(|ls, mod_outlines, mods| {
                let k_mod = ls.docs.get(&doc).unwrap().mod_opt.unwrap();
                let mod_outline = k_mod.of(mod_outlines);
                let mod_data = k_mod.of(mods);
                let fn_outline = extern_fn.of(&mod_outline.extern_fns);
                let fn_data = extern_fn.of(&mod_data.extern_fns);

                let text = {
                    write!(out, "extern fn {}(", &fn_outline.name).unwrap();
                    write_param_sig(&mut out, &fn_data.params, &fn_data.locals, mod_outlines)
                        .unwrap();
                    write_result_ty(&mut out, k_mod, &fn_outline.result_ty, mod_outlines).unwrap();
                    write!(out, ";").unwrap();
                    unsafe { String::from_utf8_unchecked(out) }
                };

                contents_opt = Some(Content::JaccoCode(text));
            });
        }
        KModLocalSymbol::Enum(_) => {}
        KModLocalSymbol::Struct(_) => {}
        KModLocalSymbol::Field(_) => {}
        KModLocalSymbol::Alias(_) => {}
    }

    let mut contents = vec![];
    if !comments.is_empty() {
        contents.push(Content::String(comments.join("\n")));
    }
    contents.extend(contents_opt);

    if contents.is_empty() {
        None
    } else {
        Some(Content::Concat(contents))
    }
}
