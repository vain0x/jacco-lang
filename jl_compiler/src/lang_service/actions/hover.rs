use super::{Doc, LangService, TPos16};
use crate::{
    cps::*,
    lang_service::{
        doc_analysis::DocContentAnalysisMut,
        lang_service::{collect_def_sites, hit_test, Content, SymbolOccurrence},
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
    symbol: SymbolOccurrence,
    ls: &mut LangService,
) -> Option<Vec<String>> {
    let DocContentAnalysisMut {
        syntax,
        symbols,
        cps,
        mod_outline,
        mod_data,
    } = ls.request_cps(doc)?;

    let mut locations = vec![];
    collect_def_sites(
        doc,
        symbol,
        syntax,
        symbols,
        cps,
        mod_outline,
        mod_data,
        &mut locations,
    );

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
    params: &[KVarTerm],
    local_vars: &KLocalVarArena,
    mod_outline: &KModOutline,
) -> io::Result<()> {
    for (i, symbol) in params.iter().enumerate() {
        if i != 0 {
            write!(out, ", ")?;
        }

        let local_var = symbol.local_var.of(&local_vars);
        write!(
            out,
            "{}: {}",
            &local_var.name,
            local_var.ty.display(KTyEnv::EMPTY, mod_outline)
        )?;
    }
    write!(out, ")")
}

fn write_result_ty(
    out: &mut impl Write,
    result_ty: &KTy,
    mod_outline: &KModOutline,
) -> io::Result<()> {
    if !result_ty.is_unit() {
        let ty = result_ty
            .erasure(mod_outline)
            .display(KTyEnv::EMPTY, mod_outline);
        write!(out, " -> {}", ty)?;
    }
    Ok(())
}

pub(crate) fn hover(doc: Doc, pos: TPos16, ls: &mut LangService) -> Option<Content> {
    ls.request_types_for(doc);

    let (symbol, comments) = {
        let DocContentAnalysisMut {
            syntax,
            symbols,
            cps,
            mod_outline,
            mod_data,
        } = ls.request_cps(doc)?;
        let (symbol, _) = hit_test(doc, pos, syntax, symbols, cps, mod_outline, mod_data)?;

        let comments = collect_doc_comments(doc, symbol, ls).unwrap_or_default();
        (symbol, comments)
    };

    let mod_outline = &ls.mod_outline;
    let mod_data = &ls.mod_data;

    let contents_opt = match symbol {
        SymbolOccurrence::LocalVar(local_var, parent) => {
            let ty_env = match parent {
                KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).ty_env,
                KLocalVarParent::ExternFn(_) => KTyEnv::EMPTY,
            };

            let local_vars = match parent {
                KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).local_vars,
                KLocalVarParent::ExternFn(extern_fn) => {
                    &extern_fn.of(&mod_data.extern_fns).local_vars
                }
            };

            Some(Content::JaccoCode(
                local_var.ty(local_vars).display(ty_env, mod_outline),
            ))
        }
        SymbolOccurrence::ModLocal(KModSymbol::Const(_))
        | SymbolOccurrence::ModLocal(KModSymbol::StaticVar(_)) => None,
        SymbolOccurrence::ModLocal(KModSymbol::Fn(k_fn)) => {
            let mut out = Vec::new();
            let fn_outline = k_fn.of(&mod_outline.fns);
            let fn_data = k_fn.of(&mod_data.fns);

            let text = {
                write!(out, "fn {}(", &fn_outline.name).unwrap();
                write_param_sig(&mut out, &fn_data.params, &fn_data.local_vars, mod_outline)
                    .unwrap();
                write_result_ty(&mut out, &fn_outline.result_ty, mod_outline).unwrap();
                write!(out, ";").unwrap();
                unsafe { String::from_utf8_unchecked(out) }
            };

            Some(Content::JaccoCode(text))
        }
        SymbolOccurrence::ModLocal(KModSymbol::ExternFn(extern_fn)) => {
            let mut out = Vec::new();
            let fn_outline = extern_fn.of(&mod_outline.extern_fns);
            let fn_data = extern_fn.of(&mod_data.extern_fns);

            let text = {
                write!(out, "extern fn {}(", &fn_outline.name).unwrap();
                write_param_sig(&mut out, &fn_data.params, &fn_data.local_vars, mod_outline)
                    .unwrap();
                write_result_ty(&mut out, &fn_outline.result_ty, mod_outline).unwrap();
                write!(out, ";").unwrap();
                unsafe { String::from_utf8_unchecked(out) }
            };

            Some(Content::JaccoCode(text))
        }
        _ => None,
    };

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
